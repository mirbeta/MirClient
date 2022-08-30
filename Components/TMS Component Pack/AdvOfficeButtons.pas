{*************************************************************************}
{ TAdvOfficeButtons components                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{    TMS Software                                                         }
{    copyright © 2007 - 2015                                              }
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

unit AdvOfficeButtons;

{$I TMSDEFS.INC}
{$R AdvOfficeButtons.res}
{$DEFINE REMOVESTRIP}
{$DEFINE REMOVEDRAW}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, StdCtrls,
  PictureContainer, AdvGroupBox, Types, AdvStyleIF
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1;  // Major version nr.
  MIN_VER = 4;  // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0;  // Build nr.

  // version history
  // 1.0.0.1 : Fixed compatibility issue with TRadioGroup of TAdvOfficeRadioGroup
  // 1.0.1.0 : Improved : exposed Visible property in TAdvOfficeRadioButton
  // 1.0.2.0 : New : Added OnEnter, OnExit events in TAdvOfficeRadioButton, TAdvOfficeCheckBox
  // 1.0.3.0 : Improved : painting hot state of controls
  // 1.1.0.0 : New property Value added in AdvOfficeCheckGroup
  //         : New component TDBAdvOfficeCheckGroup added
  // 1.1.0.1 : Improved : painting of focus rectangle
  // 1.1.0.2 : Fixed : issue with ImageIndex for caption
  // 1.1.0.3 : Fixed : issue with arrow keys & TAdvOfficeRadioGroup
  // 1.1.0.4 : Fixed : issue with dbl click & mouseup handling
  // 1.1.0.5 : Fixed : small painting issue with ClearType fonts
  // 1.1.0.6 : Fixed : issue with runtime creating controls
  // 1.1.0.7 : Fixed : issue with setting separate radiobuttons in group as disabled
  // 1.1.0.8 : Fixed : issue with OnClick event for TAdvOfficeRadioGroup
  // 1.1.0.9 : Fixed : issue with vertical alignment of radiobutton label text
  // 1.1.1.0 : Improved : BidiMode RightToLeft support
  // 1.1.1.1 : Fixed : painting issue with BiDiMode bdRightToLeft for radiobutton
  // 1.1.1.2 : Fixed : issue with transparency on Windows Vista
  // 1.1.1.3 : Improved : tab key handling for TAdvOfficeCheckGroup
  // 1.1.1.4 : Fixed : background painting issue with Delphi 2009
  // 1.1.2.0 : Improved : shows native Windows Vista checkbox & radiobutton on themed Windows Vista
  // 1.2.0.0 : New : DisabledFontColor added for all controls
  // 1.2.1.0 : New : property Themed added to control style of checkbox/radiobutton
  // 1.2.2.0 : Improved : painting of cbGrayed state
  // 1.2.2.1 : Fixed : issue with setting focus on disabled checkboxes in AdvOfficeCheckGroup
  // 1.2.3.0 : New : ShowFocus property added for controls
  // 1.2.3.1 : Fixed : issue with empty caption
  // 1.2.3.2 : Improved : vertical top text alignment
  // 1.2.3.3 : Improved : focus painting
  // 1.2.4.0 : Improved : handling of actions with TAdvOfficeCheckBox
  // 1.2.4.1 : Changed : OnClick event triggered in TAdvOfficeCheckBox for programmatic changes
  // 1.2.4.2 : Improved : bdRightToLeft text drawing for non HTML formatted text
  // 1.2.5.0 : New : Support for cbGrayed state in not themed mode
  // 1.3.0.0 : New : AllowGrayed property added
  // 1.3.1.0 : New : Event OnCheckBoxClick added in TAdvOfficeCheckGroup
  // 1.3.1.1 : Improved : Themed drawing of checkboxes in TAdvOfficeCheckBox
  // 1.3.1.2 : Fixed : Paint issue with Alignment = taRightJustify in TAdvOfficeRadioButton
  // 1.3.1.3 : Fixed : Issue with right-click handling of TAdvOfficeCheckBox
  // 1.3.1.4 : Fixed : Painting issue with taCenter aligned TAdvOfficeRadioGroup
  // 1.3.2.0 : New : Function XYToItem() added in TAdvOfficeCheckGroup, TAdvOfficeRadioGroup
  // 1.3.2.1 : Improved : BiDiMode support for TAdvOfficeRadioButton
  // 1.3.3.0 : New : Event OnGroupCheckClick added in TAdvOfficeCheckGroup
  // 1.3.4.0 : New : Property AdvOfficeRadioGroup.RadioButtons[index]: TAdvOfficeRadioButton;
  // 1.3.4.1 : Fixed : Issue with transparency when BidiMode = bdRightToLeft
  // 1.3.4.2 : Fixed : Issue with OnCheckBoxClick in TAdvOfficeRadioGroup
  // 1.3.4.3 : Fixed : Regression in TAdvOfficeRadioButton
  // 1.3.5.0 : New : Support for use of Actions with TAdvOfficeRadioButton
  // 1.3.5.1 : Fixed : Focus indication in TAdvOfficeCheckGroup
  // 1.3.6.0 : New: ControlIndent property added for single column TAdvOfficeRadioGroup
  //         : New: ControlIndent property added for single column TAdvOfficeCheckGroup
  // 1.3.6.1 : Fixed : Issue with setting Visible/Enabled before control is shown
  // 1.3.6.2 : Fixed : Issue with ItemIndex in TAdvOfficeRadioGroup
  // 1.3.6.3 : Fixed : Issue with setting ItemIndex from OnClick event
  // 1.3.6.4 : Fixed : Issue with TAdvOfficeRadioGroup, TAdvOfficeCheckGroup enabling when CheckBox is unchecked
  // 1.3.6.5 : Fixed : Issue with OnClick event handler and use of actions
  // 1.3.7.0 : New: OnRadioButtonClick event added in TAdvOfficeRadioGroup
  // 1.3.8.0 : Improved : Changed AdvOfficeCheckGroup.Value to int64 type to support more than 32 checkboxes
  // 1.3.8.1 : Fixed : Issue with AdvOfficeRadioGroup focus handling
  // 1.3.8.2 : Fixed : Issue with programmatically setting focus to TAdvOfficeRadioGroup
  // 1.3.8.3 : Fixed : Issue with triggering OnRadioButtonClick
  // 1.3.8.4 : Fixed : Issue with arrow key handling in TAdvOfficeRadioGroup
  // 1.3.8.5 : Fixed : Regression issue with arrow key handling in TAdvOfficeRadioGroup
  // 1.3.8.6 : Fixed : Issue with TAdvOfficeRadioGroup getting focus & checking radiobutton when TabStop = false
  // 1.3.8.7 : Fixed : Issue with OnRadioButtonClick in TAdvOfficeRadioGroup
  // 1.3.8.8 : Fixed : Issue with TabStop in TAdvOfficeRadioGroup
  // 1.3.8.9 : Fixed : Issue with ButtonAlign for TAdvOfficeCheckGroup
  // 1.3.9.0 : Improved : Made ArrangeButtons virtual protected methods
  // 1.3.9.1 : Fixed : Issue with TabStop for TAdvOfficeRadioGroup
  // 1.4.0.0 : New : TAdvFormStyler awareness added for TAdvOfficeRadioGroup, TAdvOfficeCheckGroup

const
  {$IFDEF DELPHI9_LVL}
  DefDisabledColor = clDkGray;
  {$ENDIF}
  {$IFNDEF DELPHI9_LVL}
  DefDisabledColor = clGray;
  {$ENDIF}

type
  TAnchorClick = procedure(Sender:TObject; Anchor:string) of object;

  TCheckBoxClick = procedure(Sender: TObject; CheckBoxIndex: integer; Value: boolean) of object;

  TCustomAdvOfficeCheckBox = class;

  TAdvOfficeCheckBoxActionLink = class(TControlActionLink)
  protected
    FImageIndex: Integer;
    FClient: TCustomAdvOfficeCheckBox;
    function IsCaptionLinked: Boolean; override;
    function IsCheckedLinked: Boolean; override;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
  end;

  TCustomAdvOfficeCheckBox = class(TCustomControl)
  private
    FAllowGrayed: Boolean;
    FDown:Boolean;
    FState:TCheckBoxState;
    FFocused:Boolean;
    FShowFocus:Boolean;
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
    FBkgBmp: TBitmap;
    FBkgCache: boolean;
    FTransparentCaching: boolean;
    FDrawBkg: boolean;
    FGotClick: boolean;
    FDisabledFontColor: TColor;
    FInternalClick: boolean;
    FThemed: boolean;
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure SetState(Value:TCheckBoxState);
    procedure SetCaption(Value: string);
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
    procedure SetThemed(const Value: boolean);
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
  protected
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawCheck;
    procedure Paint; override;
    procedure SetChecked(Value:Boolean); virtual;
    function  GetChecked:Boolean; virtual;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key:Word;Shift:TShiftSTate); override;
    procedure KeyUp(var Key:Word;Shift:TShiftSTate); override;
    procedure SetDown(Value:Boolean);
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure WndProc(var Message: TMessage); override;
    property Checked: Boolean read GetChecked write SetChecked default False;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Toggle; virtual;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TransparentChaching: boolean read FTransparentCaching write FTransparentCaching;
    property DrawBkg: Boolean read FDrawBkg write FDrawBkg;
    property Alignment: TLeftRight read FAlignment write SetAlignment;
    property ButtonVertAlign: TTextLayout read FBtnVAlign write setButtonVertAlign default tlTop;
    property Caption: string read FCaption write SetCaption;
    property DisabledFontColor: TColor read FDisabledFontColor write FDisabledFontColor default DefDisabledColor;
    property AllowGrayed: boolean read FAllowGrayed write FAllowGrayed default False;
    property Down: Boolean read FDown write SetDown default False;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property ReturnIsTab: Boolean read FReturnIsTab write FReturnIsTab;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default true;
    property State: TCheckBoxState read FState write SetState default cbUnchecked;
    property Themed: boolean read FThemed write SetThemed default false;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property OnAnchorClick: TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter: TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit: TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeCheckBox = class(TCustomAdvOfficeCheckBox)
  published
    property Action;
    property Align;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Color;
    property Checked;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;

    property Alignment;
    property ButtonVertAlign;
    property Caption;
    property DisabledFontColor;
    property Down;
    property Ellipsis;
    property Images;
    property PictureContainer;
    property ReadOnly;
    property ReturnIsTab;
    property ShadowColor;
    property ShadowOffset;
    property ShowFocus;
    property State;
    property Themed;
    property URLColor;
    property OnAnchorClick;
    property OnAnchorEnter;
    property OnAnchorExit;
    property Version;
  end;

  TCustomAdvOfficeRadioButton = class;


  TAdvOfficeRadioButtonActionLink = class(TControlActionLink)
  protected
    FImageIndex: Integer;
    FClient: TCustomAdvOfficeRadioButton;
    procedure AssignClient(AClient: TObject); override;
    procedure SetChecked(Value: Boolean); override;
    procedure SetCaption(const Value: string); override;
  end;

  TCustomAdvOfficeRadioButton = class(TCustomControl)
  private
    FDown: Boolean;
    FChecked: Boolean;
    FFocused: Boolean;
    FShowFocus: Boolean;
    FGroupIndex: Byte;
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
    FBkgBmp: TBitmap;
    FBkgCache: boolean;
    FTransparentCaching: boolean;
    FDrawBkg: Boolean;
    FGotClick: boolean;
    FDisabledFontColor: TColor;
    FThemed: boolean;
    FCheckDown: boolean;
    procedure TurnSiblingsOff;
    procedure SetDown(Value:Boolean);
    procedure SetChecked(Value:Boolean);
    procedure SetImages(const Value: TImageList);
    procedure SetURLColor(const Value:TColor);
    function IsAnchor(x,y:integer):string;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMLButtonDown(var Message:TWMLButtonDown); message WM_LBUTTONDOWN;
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
    procedure SetThemed(const Value: boolean);
    function GetVersionNr: Integer;
    procedure DrawParentImage (Control: TControl; Dest: TCanvas);
  protected
    procedure DrawRadio;
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
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
    property ClicksDisabled: Boolean read FClicksDisabled write FClicksDisabled;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property TransparentChaching: boolean read FTransparentCaching write FTransparentCaching;
    property DrawBkg: Boolean read FDrawBkg write FDrawBkg;
    property Alignment: TLeftRight read fAlignment write SetAlignment;
    property URLColor:TColor read FURLColor write SetURLColor default clBlue;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property Caption: string read FCaption write SetCaption;
    property Checked:Boolean read FChecked write SetChecked default False;
    property DisabledFontColor: TColor read FDisabledFontColor write FDisabledFontColor default DefDisabledColor;
    property Down:Boolean read FDown write SetDown default False;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis default False;
    property GroupIndex:Byte read FGroupIndex write FGroupIndex default 0;
    property Images:TImageList read fImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ReturnIsTab:Boolean read FReturnIsTab write FReturnIsTab;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowFocus: Boolean read FShowFocus write FShowFocus default true;
    property Themed: boolean read FThemed write SetThemed default false;
    property OnAnchorClick:TAnchorClick read fAnchorClick write fAnchorClick;
    property OnAnchorEnter:TAnchorClick read fAnchorEnter write fAnchorEnter;
    property OnAnchorExit:TAnchorClick read fAnchorExit write fAnchorExit;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeRadioButton = class(TCustomAdvOfficeRadioButton)
  published
    property Align;
    property Action;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Color;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    property ParentFont;
    property ParentColor;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property OnClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property Visible;

    property Alignment;
    property URLColor;
    property ButtonVertAlign;
    property Caption;
    property Checked;
    property DisabledFontColor;
    property Down;
    property Ellipsis;
    property GroupIndex;
    property Images;
    property PictureContainer;
    property ReturnIsTab;
    property ShadowColor;
    property ShadowOffset;
    property ShowFocus;
    property Themed;
    property OnAnchorClick;
    property OnAnchorEnter;
    property OnAnchorExit;
    property Version;
  end;

  TEnabledEvent = procedure (Sender:TObject; ItemIndex: Integer; var Enabled: Boolean) of object;

  TCustomAdvOfficeRadioGroup = class(TAdvGroupbox, ITMSStyle)
  private
    FButtons: TList;
    FItems: TStrings;
    FItemIndex: Integer;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FAlignment: TAlignment;
    FBtnVAlign: TTextLayout;
    FImages: TImageList;
    FContainer: TPictureContainer;
    FEllipsis: Boolean;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FShowFocus: Boolean;
    FOnIsEnabled: TEnabledEvent;
    FIsReadOnly: boolean;
    FDisabledFontColor: TColor;
    FThemed: boolean;
    FClicksDisabled: boolean;
    FControlIndent: integer;
    FOnRadioButtonClick: TNotifyEvent;
    FFocusButtonIdx: integer;
    procedure ButtonClick(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItemIndex(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure CheckFocus(Sender: TObject);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetImages(const Value: TImageList);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    procedure SetThemed(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetClicksDisabled(const Value: boolean);
    function GetRadioButton(Index: integer): TAdvOfficeRadioButton;
    procedure SetControlIndent(const Value: integer);
  protected
    procedure Loaded; override;
    procedure ReadState(Reader: TReader); override;
    procedure ArrangeButtons; virtual;
    function CanModify: Boolean; virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property Columns: Integer read FColumns write SetColumns default 1;
    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;
    property Items: TStrings read FItems write SetItems;
    property IsReadOnly: boolean read FIsReadOnly write FIsReadOnly;
    property Themed: boolean read FThemed write SetThemed default false;
    procedure UpdateButtons;
    property ClicksDisabled: boolean read FClicksDisabled write SetClicksDisabled;
    procedure DoRadioButtonClick; virtual;
    procedure DoEnter; override;
    procedure DoExit; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure PushKey(var Key: Char);
    procedure PushKeyDown(var Key: Word; Shift: TShiftState);
    function XYToItem(X,Y: integer): integer;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property ControlIndent: integer read FControlIndent write SetControlIndent default 0;
    property RadioButtons[Index: integer]: TAdvOfficeRadioButton read GetRadioButton;
    property DisabledFontColor: TColor read FDisabledFontColor write FDisabledFontColor default DefDisabledColor;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clSilver;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property OnIsEnabled: TEnabledEvent read FOnIsEnabled write FOnIsEnabled;
    property OnRadioButtonClick: TNotifyEvent read FOnRadioButtonClick write FOnRadioButtonClick;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeRadioGroup = class(TCustomAdvOfficeRadioGroup)
  private
  protected
  public
  published
    property Align;
    property Anchors;
    property Constraints;
    property ControlIndent;
    property DragKind;
    property ParentBiDiMode;
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
    property Themed;
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

    property Alignment;
    property ButtonVertAlign;
    property DisabledFontColor;
    property Ellipsis;
    property Images;
    property PictureContainer;
    property ShadowColor;
    property ShadowOffset;
    property ShowFocus;
    property OnRadioButtonClick;
    property OnIsEnabled;
    property Version;
  end;

  TCustomAdvOfficeCheckGroup = class(TAdvGroupBox, ITMSStyle)
  private
    FButtons: TList;
    FItems: TStrings;
    FColumns: Integer;
    FReading: Boolean;
    FUpdating: Boolean;
    FAlignment: TAlignment;
    FBtnVAlign: TTextLayout;
    FImages: TImageList;
    FContainer: TPictureContainer;
    FEllipsis: Boolean;
    FShadowOffset: Integer;
    FShadowColor: TColor;
    FShowFocus: Boolean;
    FOnIsEnabled: TEnabledEvent;
    FValue: int64;
    FFocusButtonIdx: integer;
    FDisabledFontColor: TColor;
    FThemed: Boolean;
    FOnCheckBoxClick: TCheckBoxClick;
    FOnGroupCheckClick: TNotifyEvent;
    FControlIndent: integer;
    procedure ButtonClick(Sender: TObject);
    procedure CheckFocus(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetButtonCount(Value: Integer);
    procedure SetColumns(Value: Integer);
    procedure SetItems(Value: TStrings);
    procedure UpdateButtons;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetAlignment(const Value: TAlignment);
    procedure SetButtonVertAlign(const Value: TTextLayout);
    procedure SetContainer(const Value: TPictureContainer);
    procedure SetImages(const Value: TImageList);
    procedure SetEllipsis(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
    procedure SetShadowOffset(const Value: Integer);
    function GetReadOnly(Index: Integer): Boolean;
    procedure SetReadOnly(Index: Integer; const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetValue(const Value: int64);
    function GetValue: int64;
    procedure SetThemed(const Value: boolean);
    procedure SetShowFocus(const Value: boolean);
    procedure SetControlIndent(const Value: integer);
  protected
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ArrangeButtons; virtual;
    procedure PerformCheckBoxAction; override;
    procedure ReadState(Reader: TReader); override;
    function CanModify: Boolean; virtual;
    function GetChecked(Index: Integer): Boolean; virtual;
    procedure SetChecked(Index: Integer; const Value: Boolean); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TStrings read FItems write SetItems;
    property Value: int64 read GetValue write SetValue;
    property Themed: boolean read FThemed write SetThemed default false;
    procedure DoCheckBoxClick(Index: integer; Value: boolean); virtual;
    procedure DoGroupCheckClick; override;
  public
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetVersionNr: Integer; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure UpdateValue;
    procedure PushKey(var Key: Char);
    procedure PushKeyDown(var Key: Word; Shift: TShiftState);
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property ReadOnly[Index: Integer]: Boolean read GetReadOnly write SetReadOnly;
    function XYToItem(X,Y: Integer): Integer;

    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ControlIndent: integer read FControlIndent write SetControlIndent default 0;
    property ButtonVertAlign: TTextLayout read fBtnVAlign write SetButtonVertAlign default tlTop;
    property Ellipsis: Boolean read FEllipsis write SetEllipsis;
    property DisabledFontColor: TColor read FDisabledFontColor write FDisabledFontColor default DefDisabledColor;
    property Images: TImageList read FImages write SetImages;
    property PictureContainer: TPictureContainer read FContainer write SetContainer;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clSilver;
    property ShadowOffset: Integer read FShadowOffset write SetShadowOffset default 1;
    property ShowFocus: boolean read FShowFocus write SetShowFocus default true;
    property OnIsEnabled: TEnabledEvent read FOnIsEnabled write FOnIsEnabled;
    property OnCheckBoxClick: TCheckBoxClick read FOnCheckBoxClick write FOnCheckBoxClick;
    property OnGroupCheckClick: TNotifyEvent read FOnGroupCheckClick write FOnGroupCheckClick;
    property Version: string read GetVersion write SetVersion;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeCheckGroup = class(TCustomAdvOfficeCheckGroup)
  private
  protected
  public
    property Value;
  published
    property Align;
    property Anchors;
    property Constraints;
    property DragKind;
    property ParentBiDiMode;
    property Caption;
    property Color;
    property Columns;
    property ControlIndent;
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
    property Themed;
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

    property Alignment;
    property ButtonVertAlign;
    property Ellipsis;
    property DisabledFontColor;
    property Images;
    property PictureContainer;
    property ShadowColor;
    property ShadowOffset;
    property ShowFocus;
    property Version;
    property OnCheckBoxClick;
    property OnIsEnabled;
    property OnGroupCheckClick;
  end;

var
  TMS_NoClickForProgrammaticCheck: boolean = true;


implementation

uses
  ShellApi, CommCtrl, Math, Imglist, ActnList, Forms, AOBXPVS;

{$I HTMLENGO.PAS}


const
  BW = 12;

function IsVista: boolean;
var
//  hKernel32: HMODULE;
  dwVersion:Dword;
  dwWindowsMajorVersion,dwWindowsMinorVersion:Dword;

begin
  dwVersion := GetVersion;
  dwWindowsMajorVersion :=  DWORD(LOBYTE(LOWORD(dwVersion)));
  dwWindowsMinorVersion :=  DWORD(HIBYTE(LOWORD(dwVersion)));

  Result := (dwWindowsMajorVersion > 5) OR
    ((dwWindowsMajorVersion = 5) AND (dwWindowsMinorVersion >= 1));

  (*
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
  *)
end;

procedure PaintFocusRect(ACanvas: TCanvas; R: TRect; Clr: TColor);
var
  LB: TLogBrush;
  HPen, HOldPen: THandle;
begin
  ACanvas.Pen.Color := Clr;

  lb.lbColor := ColorToRGB(Clr);
  lb.lbStyle := bs_Solid;

  HPen := ExtCreatePen(PS_COSMETIC or PS_ALTERNATE,1, lb, 0, nil);
  HOldPen := SelectObject(ACanvas.Handle, HPen);

  MoveToEx(ACanvas.Handle, R.Left, R.Top, nil);
  LineTo(ACanvas.Handle, R.Right, R.Top);

  MoveToEx(ACanvas.Handle, R.Right, R.Top, nil);
  LineTo(ACanvas.Handle, R.Right, R.Bottom);

  MoveToEx(ACanvas.Handle, R.Right, R.Bottom, nil);
  LineTo(ACanvas.Handle, R.Left, R.Bottom);

  MoveToEx(ACanvas.Handle, R.Left, R.Top, nil);
  LineTo(ACanvas.Handle, R.Left, R.Bottom);

  DeleteObject(SelectObject(ACanvas.Handle,HOldPen));
end;

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

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);

  Result := FIsComCtl6 and FIsWinXP;
end;

{ TCustomHTMLCheckBox }

constructor TCustomAdvOfficeCheckBox.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
  Width := 120;
  Height := 20;
  FUrlColor := clBlue;
  FBtnVAlign := tlTop;
  FImageCache := THTMLPictureCache.Create;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    FCaption := self.ClassName;

  FShadowOffset := 1;
  FShadowColor := clGray;
  FDisabledFontColor := DefDisabledColor;
  FShowFocus := true;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);

  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

  ControlStyle := ControlStyle - [csClickEvents];
  FReadOnly := False;

  FBkgBmp := TBitmap.Create;
  FBkgCache := false;
  FTransparentCaching := false;
  FDrawBkg := true;
end;

function TCustomAdvOfficeCheckBox.IsAnchor(x,y:integer):string;
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

  Result := '';

  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0) then
    Result := Anchor;
end;


procedure TCustomAdvOfficeCheckBox.DrawParentImage(Control: TControl; Dest: TCanvas);
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

procedure TCustomAdvOfficeCheckBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  FBkgCache := false;
  Repaint;
end;



procedure TCustomAdvOfficeCheckBox.DrawCheck;
var
  bmp: TBitmap;
  BL,BT:Integer;
  HTheme: THandle;
  r: TRect;
  FIsComCtl6: boolean;
  i: integer;
  DrawThemed: boolean;
begin
  BT := 4;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);


  DrawThemed := IsVista and Themed and IsThemeActive and FIsComCtl6;

  if DrawThemed then
  begin
    case FBtnVAlign of
    tlTop: BT := 4 + (Canvas.TextHeight('gh') - 16) div 2;
    tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - (16 div 2);
    tlBottom: BT := ClientRect.Bottom - 16;
    end;

    if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
      BL := ClientRect.Right - 16 - 1
    else
      BL := 0;

    HTheme := OpenThemeData(Self.Handle,'button');
    r := Rect(BL, BT, BL + 16, BT + 16);

    if HTheme <> 0 then
    begin
      case State of
      cbChecked:
        begin
          if not Enabled then
             DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDDISABLED,@r,nil)
          else
            if Down then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL or CBS_PUSHED,@r,nil)
            else
              if FHot then
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDHOT or CBS_HOT,@r,nil)
              else
                DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_CHECKEDNORMAL,@r,nil);
        end;
      cbUnChecked:
        begin
          if not Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDDISABLED,@r,nil)
          else
          if Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL or CBS_PUSHED,@r,nil)
          else
            if FHot then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_HOT,@r,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_UNCHECKEDNORMAL,@r,nil);
        end;
      cbGrayed:
        begin

          if not Enabled then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDDISABLED,@r,nil)
          else
          if Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDPRESSED,@r,nil)
          else
            if FHot then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDHOT or CBS_HOT,@r,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_CHECKBOX,CBS_MIXEDNORMAL,@r,nil);
         end;
      end;
      CloseThemeData(HTheme);
    end
    else
      DrawThemed := false;
  end;

  if not DrawThemed then
  begin
    //ExtraBW := 4;
    bmp := TBitmap.Create;
    try
      if state = cbGrayed then
      begin
        if Down then
          bmp.LoadFromResourceName(hinstance,'TMSOFCGD')
        else
          if FHot then
            bmp.LoadFromResourceName(hinstance,'TMSOFCGH')
          else
            bmp.LoadFromResourceName(hinstance,'TMSOFCG');

      end
      else
      if state = cbChecked then
      begin
        if Down then
          bmp.LoadFromResourceName(hinstance,'TMSOFCCD')
        else
          if FHot then
            bmp.LoadFromResourceName(hinstance,'TMSOFCCH')
          else
            bmp.LoadFromResourceName(hinstance,'TMSOFCC');

      end
      else
      begin
        if Down then
          bmp.LoadFromResourceName(hinstance,'TMSOFCUD')
        else
          if FHot then
            bmp.LoadFromResourceName(hinstance,'TMSOFCUH')
          else
            bmp.LoadFromResourceName(hinstance,'TMSOFCU');
      end;

      bmp.Transparent := true;
      bmp.TransparentMode := tmAuto;

      case FBtnVAlign of
      tlTop: BT :=  4 + (Canvas.TextHeight('gh') - bmp.Height) div 2;
      tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - (bmp.Height div 2);
      tlBottom: BT := ClientRect.Bottom - bmp.Height;
      end;

      if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
        BL := ClientRect.Right - bmp.Width - 1
      else
        BL := 0;

      Canvas.Draw(BL,BT,bmp);
    finally
      bmp.free;
    end;
  end;
end;

procedure TCustomAdvOfficeCheckBox.Paint;
var
  R, hr: TRect;
  a,s,fa,text: string;
  xsize,ysize: Integer;
  ExtraBW,HyperLinks,MouseLink: Integer;

begin
  Canvas.Font := Font;

  if FTransparentCaching then
  begin
    if FBkgCache then
    begin
      Canvas.Draw(0,0,FBkgBmp)
    end
    else
    begin
      FBkgBmp.Width := self.Width;
      FBkgBmp.Height := self.Height;
      DrawParentImage(Self, FBkgBmp.Canvas);
      Canvas.Draw(0,0,FBkgBmp);
      FBkgCache := true;
    end;
  end
  else
  begin
    {$IFNDEF DELPHI_UNICODE}
    if FDrawBkg or IsVista then
    {$ENDIF}
      DrawParentImage(Self, Canvas);
  end;

  with Canvas do
  begin
    Text := Caption;

    DrawCheck;

    ExtraBW := 4;

    R := GetClientRect;

    if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
    begin
      r.Left := 0;
      r.Right := r.Right - BW - ExtraBW;
    end
    else
      r.Left := r.Left + BW + ExtraBW;

    if Ellipsis then
    begin
      case FBtnVAlign of
        tlTop: r.top := 4 + (Canvas.TextHeight('gh') - 16) div 2;
        tlCenter: r.top := (ClientRect.Bottom - ClientRect.Top) div 2 - (16 div 2);
        tlBottom: r.top := ClientRect.Bottom - 16;
      end;
    end
    else
      r.Top := r.Top + 4;

    if (BidiMode = bdRightToLeft) and (pos('</',Text) = 0) then
    begin
      Canvas.Brush.Style := bsClear;

      if not Enabled then
      begin
        OffsetRect(r,1,1);
        Canvas.Font.Color := clWhite;

        Canvas.Brush.Style := bsClear;
        DrawText(Canvas.Handle, PChar(Text), Length(Text), r, DT_RIGHT);

        Canvas.Font.Color := DisabledFontColor;
        Offsetrect(r,-1,-1);
      end;

      DrawText(Canvas.Handle, PChar(Text), Length(Text), r, DT_RIGHT);
    end
    else
    begin

      HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
        clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

      if UseRightToLeftAlignment then
        r.Left := r.Right - Xsize - 3;

      if ButtonVertAlign in [tlCenter, tlBottom] then
      begin
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,false,true,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);
        case ButtonVertAlign of
        tlCenter: r.Top := r.Top - 3 + (r.Bottom - r.Top - YSize) div 2;
        tlBottom: r.Top := r.Bottom - YSize - 3;
        end;
      end;


      if not Enabled then
      begin
        OffsetRect(r,1,1);
        Canvas.Font.Color := clWhite;
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clWhite,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);

        Canvas.Font.Color := DisabledFontColor;
        Offsetrect(r,-1,-1);

        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clGray,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
      end
      else
      begin
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0);
      end;

    end;
    if FFocused and (TabStop or (Parent is TAdvOfficeCheckGroup))and FShowFocus then
    begin
      r.right := r.left + xsize + 3;
      r.bottom := r.top + ysize ;
      PaintFocusRect(Canvas,R,Font.Color);
    end;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetDown(Value:Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetState(Value:TCheckBoxState);
var
  r: TRect;
begin
  if FState <> Value then
  begin
    FState := Value;

    if HandleAllocated and HasParent then
    begin
      r := GetClientRect;
      case Alignment of
      taLeftJustify: r.Right := 20;
      taRightJustify: r.Left := r.Right - 20;
      end;
      InvalidateRect(self.Handle,@r,True);
    end;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetThemed(const Value: boolean);
begin
  if (Value <> FThemed) then
  begin
    FThemed := Value;
    Invalidate;
  end;
end;

function TCustomAdvOfficeCheckBox.GetChecked: Boolean;
begin
  Result := (State = cbChecked);
end;

procedure TCustomAdvOfficeCheckBox.SetChecked(Value:Boolean);
begin
  if (Value <> (State = cbChecked)) then
  begin

    if Value then
      State := cbChecked
    else
      State := cbUnchecked;

    Invalidate;

    if not FInternalClick and Assigned(OnClick) and not TMS_NoClickForProgrammaticCheck then
      OnClick(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeCheckBox.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    Caption := TCustomAction(Sender).Caption;
    Checked := TCustomAction(Sender).Checked;
    Enabled := TCustomAction(Sender).Enabled;
  end;
end;

//------------------------------------------------------------------------------

function TCustomAdvOfficeCheckBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvOfficeCheckBoxActionLink;
end;


procedure TCustomAdvOfficeCheckBox.DoEnter;
{$IFNDEF DELPHI9_LVL}
var
  R: TRect;
{$ENDIF}
begin
  inherited DoEnter;
  FFocused := True;
  {$IFDEF DELPHI9_LVL}
  Repaint;
  {$ELSE}
  if ShowFocus then
    Repaint
  else
  begin
    R := ClientRect;
    R.Right := 16;
    InvalidateRect(self.Handle, @R, true);
  end;
  {$ENDIF}
end;


procedure TCustomAdvOfficeCheckBox.DoExit;
var
  db: boolean;
begin
  inherited DoExit;
  FFocused := False;
  db := FDrawBkg;
  FDrawBkg := true;
  Repaint;
  FDrawBkg := db;
end;

procedure TCustomAdvOfficeCheckBox.MouseDown(Button: TMouseButton; Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
  R: TRect;
begin
  if Button <> mbLeft then
  begin
    inherited;
    Exit;
  end;

  Anchor := '';
  FGotClick := true;

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
    if (self.CanFocus and not (csDesigning in ComponentState)) then
    begin
      SetFocus;
      FFocused := True;
    end;
  end;

  if (Anchor = '') then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    MouseCapture := True;
    Down := True;
  end;

  R := ClientRect;
  R.Right := 16;
  InvalidateRect(Self.Handle,@R, true);
end;

procedure TCustomAdvOfficeCheckBox.MouseUp(Button: TMouseButton; Shift: TShiftState;
                                  X, Y: Integer);
var
  R: TRect;
begin
  MouseCapture := False;

  Down := False;

  if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and FFocused and FGotClick then
  begin
    ClicksDisabled := True;
    Toggle;
    ClicksDisabled := False;
    Click;
  end;

  inherited MouseUp(Button, Shift, X, Y);

  if HandleAllocated and not (csDestroying in ComponentState) and Visible then
  begin
    R := ClientRect;
    R.Right := 16;
    InvalidateRect(Handle,@R, true);
  end;

  FGotClick := false;
end;

procedure TCustomAdvOfficeCheckBox.MouseMove(Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
begin

  if MouseCapture then
     Down := (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height);

  if fFocused then
    Anchor := IsAnchor(x,y)
  else
    Anchor := '';

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

procedure TCustomAdvOfficeCheckBox.KeyDown(var Key:Word;Shift:TShiftSTate);
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

procedure TCustomAdvOfficeCheckBox.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = vk_Space then
  begin
    Down := False;
    Toggle;
    Click;
  end;
end;


procedure TCustomAdvOfficeCheckBox.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TCustomAdvOfficeCheckBox.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeCheckBox.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages:=nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomAdvOfficeCheckBox.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomAdvOfficeCheckBox.SetButtonVertAlign(const Value: TTextLayout);
begin
  if Value <> FBtnVAlign then
  begin
    FBtnVAlign := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

destructor TCustomAdvOfficeCheckBox.Destroy;
begin
  FBkgBmp.Free;
  FImageCache.Free;
  inherited;
end;

procedure TCustomAdvOfficeCheckBox.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetCaption(Value: string);
begin
  SetWindowText(Handle,pchar(Value));
  FCaption := Value;
  Invalidate;
end;


procedure TCustomAdvOfficeCheckBox.Toggle;
begin
  if not FReadOnly then
  begin
    FInternalClick := true;

    if AllowGrayed then
    begin
      case State of
      cbUnchecked: State := cbGrayed;
      cbChecked: State := cbUnchecked;
      cbGrayed: State := cbChecked;
      end;
    end
    else
      Checked := not Checked;

    FInternalClick := false;
  end;
end;

procedure TCustomAdvOfficeCheckBox.WMEraseBkGnd(var Message: TMessage);
begin
  {$IFDEF DELPHI_UNICODE}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Message.Result := 1
  {$ENDIF}  
end;

procedure TCustomAdvOfficeCheckBox.WndProc(var Message: TMessage);
begin
  if (Message.Msg = BM_SETCHECK) and (Message.WParam in [0..2]) then
  begin
    if TCheckBoxState(Message.WParam) = cbGrayed then
      State := TCheckBoxState(Message.WParam)
    else
    begin
      Checked := TCheckBoxState(Message.WParam) = cbChecked;
      Click;
    end;
  end;

  inherited WndProc(Message);
end;

procedure TCustomAdvOfficeCheckBox.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
  begin
    if IsAccel(CharCode, FCaption) and CanFocus then
    begin
      Toggle;
      if Assigned(OnClick) then
        OnClick(Self);
      if TabStop then
        if (self.CanFocus and not (csDesigning in ComponentState)) then 
          SetFocus;
      Result := 1;
    end 
    else
      inherited;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure TCustomAdvOfficeCheckBox.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeCheckBox.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeCheckBox.CMMouseEnter(var Message: TMessage);
begin
  FHot := True;
  DrawCheck;
  inherited;
end;

procedure TCustomAdvOfficeCheckBox.CMMouseLeave(var Message: TMessage);
begin
  FHot := False;
  DrawCheck;
  inherited;
end;

procedure TCustomAdvOfficeCheckBox.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

function TCustomAdvOfficeCheckBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomAdvOfficeCheckBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomAdvOfficeCheckBox.SetVersion(const Value: string);
begin

end;

{ TCustomAdvOfficeRadioButton }

constructor TCustomAdvOfficeRadioButton.Create(AOwner: TComponent);
var
  VerInfo: TOSVersionInfo;
  FDesignTime: boolean;
begin
  inherited Create(AOwner);
  Width := 135;
  Height := 20;
  FURLColor := clBlue;
  FBtnVAlign := tlTop;
  FImageCache := THTMLPictureCache.Create;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    FCaption := self.ClassName;

  FShadowOffset := 1;
  FShadowColor := clGray;
  FDisabledFontColor := DefDisabledColor;
  FShowFocus := True;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) OR
    ((verinfo.dwMajorVersion = 5) AND (verinfo.dwMinorVersion >= 1));

//  TabStop := true;
  FBkgBmp := TBitmap.Create;
  FBkgCache := false;
  FTransparentCaching := False;
  FDrawBkg := true;
  FClicksDisabled := False;
end;

function TCustomAdvOfficeRadioButton.IsAnchor(x,y:integer):string;
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

  Result := '';

  if HTMLDrawEx(Canvas,s,r,FImages,x,y,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,Anchor,Stripped,FocusAnchor,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0) then
    Result := Anchor;
end;

procedure TCustomAdvOfficeRadioButton.DrawParentImage(Control: TControl; Dest: TCanvas);
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


procedure TCustomAdvOfficeRadioButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  begin
    FBkgCache := false;
    Repaint;
  end;
end;

procedure TCustomAdvOfficeRadioButton.DrawRadio;
var
  bmp: TBitmap;
  BT, BL: integer;
  i: integer;
  HTheme: THandle;
  FIsComCtl6: boolean;
  r: TRect;
  DrawThemed: boolean;
  btnres: string;
begin
  BT := 4;

  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsComCtl6 := (i > 5);

  DrawThemed := IsVista and Themed and IsThemeActive and FIsComCtl6;

  if DrawThemed then
  begin
    case FBtnVAlign of
    tlTop: BT := 4;
    tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - (16 div 2);
    tlBottom: BT := ClientRect.Bottom - 16;
    end;

    if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
      BL := ClientRect.Right - 16 - 1
    else
      BL := 0;

    HTheme := OpenThemeData(Self.Handle,'button');
    r := Rect(BL, BT, BL + 16, BT + 16);

    if HTheme <> 0 then
    begin
      if Checked then
      begin
        if not Enabled then
           DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDDISABLED,@r,nil)
        else
          if Down then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL or RBS_PUSHED,@r,nil)
          else
            if FHot then
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDHOT or RBS_HOT,@r,nil)
            else
              DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_CHECKEDNORMAL,@r,nil);
      end
      else
      begin
        if not Enabled then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDDISABLED,@r,nil)
        else
        if Down then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL or RBS_PUSHED,@r,nil)
        else
          if FHot then
            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDHOT or RBS_HOT,@r,nil)
          else
            DrawThemeBackground(HTheme,Canvas.Handle, BP_RADIOBUTTON,RBS_UNCHECKEDNORMAL,@r,nil);
      end;

      CloseThemeData(HTheme);
    end
    else
      DrawThemed := false;
  end;

  if not DrawThemed then
  begin
    btnres := 'TMSOFRU';
    bmp := TBitmap.Create;
    if (Checked) then
    begin
      if Down then
        btnres := 'TMSOFRCD'
      else
        if FHot then
          btnres := 'TMSOFRCH'
        else
          btnres := 'TMSOFRC';
    end
    else
    begin
      if Down then
        btnres := 'TMSOFRUD'
      else
        if FHot then
          btnres := 'TMSOFRUH'
        else
          btnres := 'TMSOFRU';
    end;

    bmp.LoadFromResourceName(hinstance,btnres);
    bmp.Transparent := true;
    bmp.TransparentMode := tmAuto;

    case FBtnVAlign of
    tlTop: BT := 4;
    tlCenter: BT := (ClientRect.Bottom - ClientRect.Top) div 2 - (bmp.Height div 2);
    tlBottom: BT := ClientRect.Bottom - bmp.Height - 2;
    end;

    if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
      BL := ClientRect.Right - bmp.Width - 1
    else
      BL := 0;

    Canvas.Draw(BL,BT,bmp);
    bmp.Free;
  end;
end;

procedure TCustomAdvOfficeRadioButton.Paint;
var
  BR:Integer;
  R,hr: TRect;
  a,s,fa,text: string;
  XSize,YSize,HyperLinks,MouseLink: Integer;

begin
  Canvas.Font := Font;
  Text := Caption;

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
      //FBkgBmp.PixelFormat := pf32bit;
      DrawParentImage(Self, FBkgBmp.Canvas);
      Self.Canvas.Draw(0,0,FBkgBmp);
      FBkgCache := true;
    end;
  end
  else
  begin
    {$IFNDEF DELPHI_UNICODE}
    if DrawBkg or IsVista then
    {$ENDIF}
      DrawParentImage(Self, self.Canvas);
  end;

  with Canvas do
  begin
    BR := 13;
    DrawRadio;

    r := GetClientRect;
    if (FAlignment = taRightJustify) or UseRightToLeftAlignment then
    begin
      r.Left := 0;
      r.Right := r.Right - BR - 5;
    end
    else
      r.Left := r.Left + BR + 5;

    r.Top := r.Top + 4;

    if (BidiMode = bdRightToLeft) and (pos('</',Text) = 0) then
    begin
      Canvas.Brush.Style := bsClear;
      if not Enabled then
      begin
        OffsetRect(R,1,1);
        Canvas.Font.Color := clWhite;
        DrawText(Canvas.Handle, PChar(Text), Length(Text), r, DT_RIGHT);
        Canvas.Font.Color := DisabledFontColor;
        Offsetrect(R,-1,-1);
      end;

      DrawText(Canvas.Handle, PChar(Text), Length(Text), r, DT_RIGHT);
    end
    else
    begin
      HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,True,False,False,False,False,False,not FEllipsis,1.0,clGray,
        clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);

      if UseRightToLeftAlignment then
        r.Left := r.Right - Xsize - 3;

      if ButtonVertAlign in [tlCenter, tlBottom] then
      begin
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,false,true,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);
        case ButtonVertAlign of
        tlCenter: r.Top := r.Top - 3 + (r.Bottom - r.Top - YSize) div 2;
        tlBottom: r.Top := r.Bottom - YSize - 3;
        end;
      end;

      if not Enabled then
      begin
        OffsetRect(R,1,1);
        Canvas.Font.Color := clWhite;
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clGray,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);
        //Canvas.Font.Color := clGray;
        Canvas.Font.Color := DisabledFontColor;
        Offsetrect(R,-1,-1);
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,clWhite,
          clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);
      end
      else
      begin
        Canvas.Font.Color := Font.Color;
        HTMLDrawEx(Canvas,Text,R,FImages,0,0,-1,-1,FShadowOffset,False,False,False,False,False,False,not FEllipsis,1.0,FURLColor,
                clNone,clNone,FShadowColor,a,s,fa,XSize,YSize,HyperLinks,MouseLink,hr,FImageCache,FContainer,0,BidiMode);
      end;
    end;


    (*
    if FFocused then
    begin
      r.Right := r.Left + xsize + 3;
      r.Bottom := r.Top + ysize {+ 1};
      PaintFocusRect(Canvas,R,clBlack);
    end;
    *)
    R.Right := R.Left + XSize + 1;
    R.Left := ClientRect.Left;
    R.Bottom := ClientRect.Bottom - 1;
    R.Top := R.Top - 1;

    if FFocused {and FChecked and TabStop} and FShowFocus then
    begin
      PaintFocusRect(Canvas,R,Font.Color);
    end
    else if (Parent <> nil) then
    begin
      //PaintFocusRect(Canvas,R,TCustomAdvOfficeRadioGroup(Parent).Color);
    end;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetURLColor(const Value: TColor);
begin
  FURLColor := Value;
  Invalidate;
end;


procedure TCustomAdvOfficeRadioButton.SetDown(Value:Boolean);
begin
  if FDown <> Value then
  begin
    FDown := Value;
  end;
end;


procedure TCustomAdvOfficeRadioButton.TurnSiblingsOff;
var
  i:Integer;
  Sibling: TAdvOfficeRadioButton;

begin
  if (Parent <> nil) then
  for i:=0 to Parent.ControlCount-1 do
    if Parent.Controls[i] is TAdvOfficeRadioButton then
    begin
      Sibling := TAdvOfficeRadioButton(Parent.Controls[i]);
      if (Sibling <> Self) and
         (Sibling.GroupIndex = GroupIndex) then
          Sibling.SetChecked(False);
    end;
end;

procedure TCustomAdvOfficeRadioButton.SetChecked(Value: Boolean);
{$IFDEF DELPHI2006_LVL}
var
  r: TRect;
{$ENDIF}
begin
  if FChecked <> Value then
  begin
//    TabStop := true;
    FChecked := Value;

    if FClicksDisabled then
      FGotClick := false;

    if Value then
    begin
      TurnSiblingsOff;
      if not FClicksDisabled then
        DoClick;
(*
      if Assigned(Parent) and (Parent is TAdvOfficeRadioGroup) then
      begin
        if Assigned((Parent as TAdvOfficeRadioGroup).OnCheckBoxClick) then
          (Parent as TAdvOfficeRadioGroup).OnCheckBoxClick(Parent);
      end;
*)
    end;

    {$IFDEF DELPHI2006_LVL}
    if HandleAllocated and HasParent then
    begin
      R := ClientRect;
      if (BiDiMode = bdLeftToRight) and (Alignment = taLeftJustify) then
      begin
       // R.Right := 16;
        InvalidateRect(self.Handle, @r, true);
      end
      else
        Invalidate;
    end;
    {$ENDIF}

    {$IFNDEF DELPHI2006_LVL}
    Invalidate;
    {$ENDIF}
  end;
end;


procedure TCustomAdvOfficeRadioButton.DoClick;
begin
  if not (csLoading in ComponentState) then
  begin
    {$IFDEF DELPHI_UNICODE}
    if Assigned(OnClick) and (Action <> nil) and not DelegatesEqual(@OnClick, @Action.OnExecute) then
      OnClick(Self)
    else
    {$ENDIF}

    if not (csDesigning in ComponentState) and (ActionLink <> nil) then
      ActionLink.Execute(Self)
    else

    if Assigned(OnClick) then
      OnClick(Self);
  end;
end;

procedure TCustomAdvOfficeRadioButton.DoEnter;
{$IFNDEF DELPHI9_LVL}
var
  R: TRect;
{$ENDIF}
begin
  FCheckDown := Checked;

  inherited DoEnter;
  FFocused := True;
  if (Parent is TAdvOfficeRadioGroup) and (Parent.TabStop) then
    Checked := true;

  {$IFDEF DELPHI9_LVL}
  Repaint;
  {$ELSE}
  R := ClientRect;
  R.Right := 16;
  InvalidateRect(Handle, @R, true);
  {$ENDIF}
end;

procedure TCustomAdvOfficeRadioButton.DoExit;
var
  db: boolean;
begin
  inherited DoExit;
  FFocused := False;
  db := FDrawBkg;
  FDrawBkg := true;
  Repaint;
  FDrawBkg := db;
end;

procedure TCustomAdvOfficeRadioButton.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  Anchor:string;
  {$IFDEF DELPHI2006_LVL}
  R: TRect;
  {$ENDIF}
begin
  Anchor := '';
  FGotClick := true;

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
    if (CanFocus and not (csDesigning in ComponentState)) then
    begin
      SetFocus;
      FFocused := True;
    end;
  end;

  if Anchor = '' then
  begin
    inherited MouseDown(Button, Shift, X, Y);
    MouseCapture := True;
    Down := True;
  end;

  {$IFNDEF DELPHI2006_LVL}
  Invalidate;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}

  if Alignment = taLeftJustify then
  begin
    R := ClientRect;
    R.Right := 16;
    InvalidateRect(self.Handle, @r, true);
  end
  else
    Invalidate;
  {$ENDIF}
end;

procedure TCustomAdvOfficeRadioButton.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
{$IFDEF DELPHI2006_LVL}
  R: TRect;
{$ENDIF}
  state: boolean;
begin
  MouseCapture := False;
  Down := False;

  if (X >= 0) and (X <= Width) and (Y >= 0) and (Y <= Height) and not Checked and FGotClick then
  begin
    FClicksDisabled := true;
    Checked := true;
    FGotClick := true;
  end;

  state := Checked;

  inherited MouseUp(Button, Shift, X, Y);

  if FGotClick then
    DoClick;

  {$IFNDEF DELPHI2006_LVL}
  Invalidate;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  if Alignment = taLeftJustify then
  begin
    R := ClientRect;
    R.Right := 16;
    InvalidateRect(self.Handle, @r, true);
  end
  else
    Invalidate;
  {$ENDIF}
  FGotClick := false;

  if (state <> FCheckDown) and Assigned(Parent) and (Parent is TAdvOfficeRadioGroup) then
  begin
    if Assigned((Parent as TAdvOfficeRadioGroup).OnRadioButtonClick) then
    begin
      (Parent as TAdvOfficeRadioGroup).FItemIndex := (Parent as TAdvOfficeRadioGroup).FButtons.IndexOf(Self);
      (Parent as TAdvOfficeRadioGroup).OnRadioButtonClick(Parent);
    end;
  end;
end;

procedure TCustomAdvOfficeRadioButton.MouseMove(Shift: TShiftState;X, Y: Integer);
var
  Anchor:string;
begin
  if MouseCapture then
    Down := (X>=0) and (X<=Width) and (Y>=0) and (Y<=Height);

  if FFocused then
    Anchor := IsAnchor(x,y)
  else
    Anchor := '';

  if Anchor <> '' then
  begin
    if (self.Cursor = crDefault) or (fAnchor <> Anchor) then
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

procedure TCustomAdvOfficeRadioButton.KeyDown(var Key:Word;Shift:TShiftSTate);
begin
  if (Key = vk_return) and (FReturnIsTab) then
  begin
    Key := vk_tab;
    PostMessage(self.Handle,wm_keydown,VK_TAB,0);
  end;

  if Key = VK_SPACE then
    Down := True;

  inherited KeyDown(Key,Shift);
end;

procedure TCustomAdvOfficeRadioButton.KeyUp(var Key:Word;Shift:TShiftSTate);
begin
  if Key = VK_SPACE then
  begin
    Down := False;
    if not Checked then Checked := True;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetImages(const Value: TImageList);
begin
  FImages := Value;
  Invalidate;
end;

procedure TCustomAdvOfficeRadioButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomAdvOfficeRadioButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomAdvOfficeRadioButton.SetButtonVertAlign(const Value: TTextLayout);
begin
  if Value <> FBtnVAlign then
  begin
    FBtnVAlign := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetAlignment(const Value: TLeftRight);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    Invalidate;
  end;
end;

destructor TCustomAdvOfficeRadioButton.Destroy;
begin
  FBkgBmp.Free;
  FImageCache.Free;
  inherited;
end;

procedure TCustomAdvOfficeRadioButton.SetEllipsis(const Value: Boolean);
begin
  if FEllipsis <> Value then
  begin
    FEllipsis := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetCaption(const Value: string);
begin
  inherited Caption := Value;
  FCaption := Value;
  Invalidate;
end;

procedure TCustomAdvOfficeRadioButton.Click;
begin
//  inherited;
end;

procedure TCustomAdvOfficeRadioButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, FCaption) and CanFocus then
    begin
      Checked := True;
      if TabStop then
        if (CanFocus and not (csDesigning in ComponentState)) then
          SetFocus;
      Result := 1;
    end
    else
      inherited;
end;

procedure TCustomAdvOfficeRadioButton.SetContainer(const Value: TPictureContainer);
begin
  FContainer := Value;
  Invalidate;
end;

procedure TCustomAdvOfficeRadioButton.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetShadowOffset(const Value: Integer);
begin
  if FShadowOffset <> Value then
  begin
    FShadowOffset := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeRadioButton.SetThemed(const Value: boolean);
begin
  if (Value <> FThemed) then
  begin
    FThemed := Value;
    Invalidate;
  end;
end;

procedure TCustomAdvOfficeRadioButton.CMMouseEnter(var Message: TMessage);
begin
  FHot := True;
  DrawRadio;
  inherited;
end;

procedure TCustomAdvOfficeRadioButton.CMMouseLeave(var Message: TMessage);
begin
  FHot := False;
  DrawRadio;
  inherited;
end;


procedure TCustomAdvOfficeRadioButton.WMEraseBkGnd(var Message: TMessage);
begin
  {$IFDEF DELPHI_UNICODE}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  Message.Result := 1
  {$ENDIF}
end;

procedure TCustomAdvOfficeRadioButton.WMLButtonDblClk(
  var Message: TWMLButtonDblClk);
begin
  inherited;
  if Assigned(Parent) and (Parent is TCustomAdvOfficeRadioGroup) then
  begin
    if Assigned((Parent as TCustomAdvOfficeRadioGroup).OnDblClick) then
      (Parent as TCustomAdvOfficeRadioGroup).OnDblClick(Parent);
  end;
end;

procedure TCustomAdvOfficeRadioButton.WMLButtonDown(var Message:TWMLButtonDown);
begin
  FClicksDisabled := True;

  if (CanFocus and not (csDesigning in ComponentState)) then
  begin
    if Assigned(Parent) and (Parent is TCustomAdvOfficeRadioGroup) then
    begin
      (Parent as TCustomAdvOfficeRadioGroup).FFocusButtonIdx :=
        (Parent as TCustomAdvOfficeRadioGroup).FButtons.IndexOf(Self);
    end;
    SetFocus;
  end;

  FClicksDisabled := False;

  inherited;
end;

procedure TCustomAdvOfficeRadioButton.Loaded;
begin
  inherited;
  FOldCursor := Cursor;
end;

function TCustomAdvOfficeRadioButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomAdvOfficeRadioButton.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomAdvOfficeRadioButton.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TCustomAdvOfficeRadioButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
  begin
    Caption := TCustomAction(Sender).Caption;
    Checked := TCustomAction(Sender).Checked;
    Enabled := TCustomAction(Sender).Enabled;
  end;
end;

//------------------------------------------------------------------------------

function TCustomAdvOfficeRadioButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvOfficeRadioButtonActionLink;
end;

//------------------------------------------------------------------------------
{ TAdvOfficeRadioButtonActionLink }

procedure TAdvOfficeRadioButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomAdvOfficeRadioButton;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeRadioButtonActionLink.SetChecked(Value: Boolean);
begin
  FClient.FClicksDisabled := true;
  FClient.Checked := Value;
  FClient.FClicksDisabled := false;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeRadioButtonActionLink.SetCaption(const Value: string);
begin
  FClient.Caption := Value;
end;

//------------------------------------------------------------------------------

{ TAdvGroupButton }

type
  TAdvGroupButton = class(TAdvOfficeRadioButton)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(RadioGroup: TCustomAdvOfficeRadioGroup);
    destructor Destroy; override;
  end;

constructor TAdvGroupButton.InternalCreate(RadioGroup: TCustomAdvOfficeRadioGroup);
begin
  inherited Create(RadioGroup);
  RadioGroup.FButtons.Add(Self);
  Visible := true;
  Enabled := RadioGroup.Enabled;
  ParentShowHint := False;
  OnClick := RadioGroup.ButtonClick;
  OnEnter := RadioGroup.CheckFocus;
  Parent := RadioGroup;
end;

destructor TAdvGroupButton.Destroy;
begin
  TCustomAdvOfficeRadioGroup(Owner).FButtons.Remove(Self);
  inherited Destroy;
end;

procedure TAdvGroupButton.CNCommand(var Message: TWMCommand);
begin
  if not FInClick then
  begin
    FInClick := True;
    try
      if ((Message.NotifyCode = BN_CLICKED) or
        (Message.NotifyCode = BN_DOUBLECLICKED)) and
        TCustomAdvOfficeRadioGroup(Parent).CanModify then
        inherited;
    except
      Application.HandleException(Self);
    end;
    FInClick := False;
  end;
end;

procedure TAdvGroupButton.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);

  TCustomAdvOfficeRadioGroup(Parent).PushKey(Key);

  if (Key = #8) or (Key = ' ') then
  begin
    if not TCustomAdvOfficeRadioGroup(Parent).CanModify then
      Key := #0;
  end;
end;

procedure TAdvGroupButton.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if Assigned((Parent as TCustomAdvOfficeRadioGroup).OnMouseMove) then
  begin
    (Parent as TCustomAdvOfficeRadioGroup).OnMouseMove(Parent, Shift, X + Left, Y + Top);
  end;

end;

procedure TAdvGroupButton.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TCustomAdvOfficeRadioGroup(Parent).PushKeyDown(Key, Shift);
end;

{ TCustomAdvOfficeRadioGroup }

constructor TCustomAdvOfficeRadioGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FItemIndex := -1;
  FColumns := 1;
  FAlignment := taLeftJustify;
  FBtnVAlign := tlTop;
  ShadowOffset := 1;
  ShadowColor := clSilver;
  ShowFocus := true;
  FIsReadOnly := false;
  FDisabledFontColor := DefDisabledColor;
  FControlIndent := 0;
end;

destructor TCustomAdvOfficeRadioGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TCustomAdvOfficeRadioGroup.DoEnter;
begin
  inherited;

  if FButtons.Count >  FFocusButtonIdx then
  begin
    if TAdvGroupButton(FButtons[FFocusButtonIdx]).HandleAllocated then
    begin
      TAdvGroupButton(FButtons[FFocusButtonIdx]).SetFocus;
      Invalidate;
    end;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.DoExit;
begin
  inherited;
end;

procedure TCustomAdvOfficeRadioGroup.DoRadioButtonClick;
begin
  if Assigned(OnRadioButtonClick) then
    OnRadioButtonClick(Self);
end;

procedure TCustomAdvOfficeRadioGroup.PushKey(var Key: Char);
begin
  KeyPress(Key);
end;

procedure TCustomAdvOfficeRadioGroup.PushKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key,Shift);
end;


procedure TCustomAdvOfficeRadioGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft, ci: Integer;
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

   	if Length(Caption) <= 0 then
 	   	TopMargin := TopMargin - Metrics.tmHeight div 2;

    if Columns = 1 then
      ci := ControlIndent
    else
      ci := 0;

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TAdvGroupButton(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;
          DrawBkg := false;
          Alignment := Self.Alignment;
          ButtonVertAlign := Self.ButtonVertAlign;
          Images := Self.Images;
          PictureContainer := Self.PictureContainer;
          Ellipsis := Self.Ellipsis;
          ShadowOffset := Self.ShadowOffset;
          ShadowColor := Self.ShadowColor;
          ShowFocus := self.ShowFocus;
          DisabledFontColor := Self.DisabledFontColor;
          Themed := self.Themed;
//          TabStop := self.TabStop;

          RadioEnable := Self.Enabled {and Enabled} and not FIsReadOnly;

          if Assigned(FOnIsEnabled) then
            FOnIsEnabled(Self,I,RadioEnable);

          // Make sure Enabled setting before ArrangeButtons is preserved
          //Enabled := RadioEnable;

          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8 + ci;
          if UseRightToLeftAlignment then
            ALeft := Self.ClientWidth - ALeft - ButtonWidth;
          DeferHandle := DeferWindowPos(DeferHandle, Handle, 0,
            ALeft,
            (I mod ButtonsPerCol) * ButtonHeight + TopMargin,
            ButtonWidth, ButtonHeight,
            SWP_NOZORDER or SWP_NOACTIVATE);

        //  Left := ALeft;
        //  Top := (I mod ButtonsPerCol) * ButtonHeight + TopMargin;

        //  Make sure Visible setting before ArrangeButtons is preserved
        //  Visible := True;

        end;
    finally
      EndDeferWindowPos(DeferHandle);
    end;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    if CanModify then
    begin
      FItemIndex := FButtons.IndexOf(Sender);
      Changed;
      Click;
    end;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    if FItemIndex >= FItems.Count then
      FItemIndex := FItems.Count - 1;
    UpdateButtons;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do TAdvGroupButton.InternalCreate(Self);
  while FButtons.Count > Value do TAdvGroupButton(FButtons.Last).Free;
end;

procedure TCustomAdvOfficeRadioGroup.SetClicksDisabled(const Value: boolean);
var
  i: integer;
begin
  FClicksDisabled := Value;

  for i := 0 to FButtons.Count - 1 do
  begin
     TAdvGroupButton(FButtons[i]).ClicksDisabled := Value;
  end;

end;

procedure TCustomAdvOfficeRadioGroup.SetColumns(Value: Integer);
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

procedure TCustomAdvOfficeRadioGroup.SetComponentStyle(AStyle: TTMSStyle);
begin
  if AStyle in [tsOffice2016Black] then
    Font.Color := clWhite
  else
    Font.Color := clWindowText;
end;

procedure TCustomAdvOfficeRadioGroup.SetItemIndex(Value: Integer);
begin
  if FReading then
    FItemIndex := Value
  else
  begin
    if Value < -1 then Value := -1;
    if Value >= FButtons.Count then Value := FButtons.Count - 1;
    if FItemIndex <> Value then
    begin
      if FItemIndex >= 0 then
      begin
        TAdvGroupButton(FButtons[FItemIndex]).ClicksDisabled := true;
        TAdvGroupButton(FButtons[FItemIndex]).Checked := False;
      end;
      FItemIndex := Value;
      if FItemIndex >= 0 then
      begin
        TAdvGroupButton(FButtons[FItemIndex]).ClicksDisabled := true;
        TAdvGroupButton(FButtons[FItemIndex]).Checked := True;
      end;

      //Click;
    end;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomAdvOfficeRadioGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
    TAdvGroupButton(FButtons[I]).Caption := FItems[I];

  if FItemIndex >= 0 then
  begin
    FUpdating := True;
    TAdvGroupButton(FButtons[FItemIndex]).Checked := True;
    FUpdating := False;
  end;

  ArrangeButtons;
  Invalidate;
end;

procedure TCustomAdvOfficeRadioGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;

  if CheckBox.Visible and not CheckBox.Checked then
    Exit;


  for I := 0 to FButtons.Count - 1 do
    TAdvGroupButton(FButtons[I]).Enabled := Enabled;
end;

procedure TCustomAdvOfficeRadioGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TCustomAdvOfficeRadioGroup.XYToItem(X, Y: integer): integer;
var
  i: integer;
  r: TRect;
  pt: TPoint;
begin
  Result := -1;
  pt := Point(X,Y);
  for i := 0 to FButtons.Count - 1 do
  begin
    r :=  TAdvGroupButton(FButtons[i]).ClientRect;
    OffsetRect(r,   TAdvGroupButton(FButtons[i]).Left, TAdvGroupButton(FButtons[i]).Top);
    if PtInRect(r,pt) then
    begin
      Result := i;
      break;
    end;
  end;

end;

function TCustomAdvOfficeRadioGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomAdvOfficeRadioGroup.CheckFocus(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FButtons.Count - 1 do
  begin
    if TAdvGroupButton(FButtons[i]).Focused then
      FFocusButtonIdx := i;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

function TCustomAdvOfficeRadioGroup.GetRadioButton(
  Index: integer): TAdvOfficeRadioButton;
begin
  Result := TAdvGroupButton(FButtons[Index]);
end;

procedure TCustomAdvOfficeRadioGroup.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetButtonVertAlign(
  const Value: TTextLayout);
begin
  fBtnVAlign := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetContainer(
  const Value: TPictureContainer);
begin
  FContainer := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetControlIndent(const Value: integer);
begin
  if (FControlIndent <> Value) then
  begin
    FControlIndent := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.SetImages(const Value: TImageList);
begin
  inherited Images := Value;
  FImages := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages:=nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomAdvOfficeRadioGroup.SetEllipsis(const Value: Boolean);
begin
  FEllipsis := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetShadowColor(const Value: TColor);
begin
  FShadowColor := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetShadowOffset(const Value: Integer);
begin
  FShadowOffset := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeRadioGroup.SetShowFocus(const Value: Boolean);
begin
  if (FShowFocus <> Value) then
  begin
    FShowFocus := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeRadioGroup.SetThemed(const Value: boolean);
begin
  if (FThemed <> Value) then
  begin
    FThemed := Value;
    ArrangeButtons;
  end;
end;

function TCustomAdvOfficeRadioGroup.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomAdvOfficeRadioGroup.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomAdvOfficeRadioGroup.SetVersion(const Value: string);
begin

end;


{ TGroupCheck }

type
  TGroupCheck = class(TAdvOfficeCheckBox)
  private
    FInClick: Boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure MouseMove(Shift: TShiftState;X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
  public
    constructor InternalCreate(CheckGroup: TCustomAdvOfficeCheckGroup);
    destructor Destroy; override;
  end;

constructor TGroupCheck.InternalCreate(CheckGroup: TCustomAdvOfficeCheckGroup);
begin
  inherited Create(CheckGroup);
  CheckGroup.FButtons.Add(Self);
  Visible := False;
  Enabled := CheckGroup.Enabled;
  ParentShowHint := False;
  OnClick := CheckGroup.ButtonClick;
  OnEnter := CheckGroup.CheckFocus;
  Parent := CheckGroup;
end;

destructor TGroupCheck.Destroy;
begin
  TCustomAdvOfficeCheckGroup(Owner).FButtons.Remove(Self);
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
        TCustomAdvOfficeCheckGroup(Parent).CanModify then
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
  TCustomAdvOfficeCheckGroup(Parent).PushKey(Key);
  if (Key = #8) or (Key = ' ') then
  begin
    if not TCustomAdvOfficeCheckGroup(Parent).CanModify then Key := #0;
  end;
end;

procedure TGroupCheck.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if Assigned((Parent as TCustomAdvOfficeCheckGroup).OnMouseMove) then
  begin
    (Parent as TCustomAdvOfficeCheckGroup).OnMouseMove(Parent, Shift, X + Left, Y + Top);
  end;
end;

procedure TGroupCheck.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  TCustomAdvOfficeCheckGroup(Parent).PushKeyDown(Key, Shift);
end;


{ TCustomAdvOfficeCheckGroup }

constructor TCustomAdvOfficeCheckGroup.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := [csSetCaption, csDoubleClicks];
  FButtons := TList.Create;
  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;
  FColumns := 1;
  FAlignment := taLeftJustify;
  FBtnVAlign := tlTop;
  ShadowOffset := 1;
  ShadowColor := clSilver;
  ShowFocus := true;
  FDisabledFontColor := DefDisabledColor;
  FValue := 0;
  FControlIndent := 0;
end;

destructor TCustomAdvOfficeCheckGroup.Destroy;
begin
  SetButtonCount(0);
  TStringList(FItems).OnChange := nil;
  FItems.Free;
  FButtons.Free;
  inherited Destroy;
end;

procedure TCustomAdvOfficeCheckGroup.PerformCheckBoxAction;
var
  i: integer;
begin
  if not (csDesigning in ComponentState) and Enabled and CheckBox.Visible and
    (CheckBox.State in [cbChecked, cbUnChecked]) and (CheckBox.Action <> AdvGroupBox.caNone) then
  begin
    if (CheckBox.Action = caCheckAll) then
    begin
      for i := 0 to Fbuttons.Count - 1 do
        Checked[I] := CheckBox.State = cbChecked;
    end;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.PushKey(var Key: Char);
begin
  KeyPress(Key);
end;

procedure TCustomAdvOfficeCheckGroup.PushKeyDown(var Key: Word; Shift: TShiftState);
begin
  KeyDown(Key,Shift);
end;


procedure TCustomAdvOfficeCheckGroup.ArrangeButtons;
var
  ButtonsPerCol, ButtonWidth, ButtonHeight, TopMargin, I: Integer;
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
  DeferHandle: THandle;
  ALeft,ci: Integer;
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

    if Columns = 1 then
      ci := ControlIndent
    else
      ci := 0;

   	if Length(Caption) <= 0 then
	   	TopMargin := TopMargin - Metrics.tmHeight div 2;

    DeferHandle := BeginDeferWindowPos(FButtons.Count);
    try
      for I := 0 to FButtons.Count - 1 do
        with TGroupCheck(FButtons[I]) do
        begin
          BiDiMode := Self.BiDiMode;

          DrawBkg := false;
          Alignment := self.Alignment;
          ButtonVertAlign := self.ButtonVertAlign;
          Images := self.Images;
          PictureContainer := self.PictureContainer;
          Ellipsis := self.Ellipsis;
          ShadowOffset := self.ShadowOffset;
          ShadowColor := self.ShadowColor;
          ShowFocus := self.ShowFocus;
          DisabledFontColor := Self.DisabledFontColor;
          Themed := Self.Themed;

          RadioEnable := self.Enabled;
          if Assigned(FOnIsEnabled) then
            FOnIsEnabled(Self,I,RadioEnable);

          Enabled := RadioEnable;

          ALeft := (I div ButtonsPerCol) * ButtonWidth + 8 + ci;
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

procedure TCustomAdvOfficeCheckGroup.CheckFocus(Sender: TObject);
var
  i: integer;
begin
  for i := 0 to FButtons.Count - 1 do
  begin
    if TGroupCheck(FButtons[i]).Focused then
      FFocusButtonIdx := i;
  end;
end;


procedure TCustomAdvOfficeCheckGroup.ButtonClick(Sender: TObject);
begin
  if not FUpdating then
  begin
    Changed;
    Click;
    DoCheckBoxClick((sender as TGroupCheck).Tag, (sender as TGroupCheck).Checked);
  end;
  UpdateValue;
end;

procedure TCustomAdvOfficeCheckGroup.ItemsChange(Sender: TObject);
begin
  if not FReading then
  begin
    UpdateButtons;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.DoExit;
begin
  inherited;
end;

procedure TCustomAdvOfficeCheckGroup.DoGroupCheckClick;
begin
  inherited;
  if Assigned(OnGroupCheckClick) then
    OnGroupCheckClick(Self);
end;

procedure TCustomAdvOfficeCheckGroup.DoCheckBoxClick(Index: integer;
  Value: boolean);
begin
  if Assigned(OnCheckBoxClick) then
    OnCheckBoxClick(Self, Index, Value);
end;

procedure TCustomAdvOfficeCheckGroup.DoEnter;
var
  i: integer;
  found: boolean;
begin
  inherited;

  if FButtons.Count >  FFocusButtonIdx then
  begin
    if TGroupCheck(FButtons[FFocusButtonIdx]).HandleAllocated then
    begin
      if ReadOnly[FFocusButtonIdx] then
      begin
        found := false;
        for i := 0 to Items.Count - 1 do
        begin
          if not ReadOnly[i] then
          begin
            FFocusButtonIdx := i;
            found := true;
            break;
          end;
        end;
        if not found then
          Exit;
      end;

      TGroupCheck(FButtons[FFocusButtonIdx]).SetFocus;
      Invalidate;
    end;
  end;
end;


procedure TCustomAdvOfficeCheckGroup.Loaded;
begin
  inherited Loaded;
  ArrangeButtons;
  Value := Value;
end;

procedure TCustomAdvOfficeCheckGroup.ReadState(Reader: TReader);
begin
  FReading := True;
  inherited ReadState(Reader);
  FReading := False;
  UpdateButtons;
end;

procedure TCustomAdvOfficeCheckGroup.SetButtonCount(Value: Integer);
begin
  while FButtons.Count < Value do
    TGroupCheck.InternalCreate(Self);
  while FButtons.Count > Value do
    TGroupCheck(FButtons.Last).Free;
end;

procedure TCustomAdvOfficeCheckGroup.SetColumns(Value: Integer);
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

procedure TCustomAdvOfficeCheckGroup.SetComponentStyle(AStyle: TTMSStyle);
begin
  if AStyle in [tsOffice2016Black] then
    Font.Color := clWhite
  else
    Font.Color := clWindowText;
end;

procedure TCustomAdvOfficeCheckGroup.SetItems(Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCustomAdvOfficeCheckGroup.UpdateButtons;
var
  I: Integer;
begin
  SetButtonCount(FItems.Count);
  for I := 0 to FButtons.Count - 1 do
  begin
    TGroupCheck(FButtons[I]).Caption := FItems[I];
    TGroupCheck(FButtons[I]).Tag := I;
  end;
  ArrangeButtons;
  Invalidate;
end;

procedure TCustomAdvOfficeCheckGroup.CMEnabledChanged(var Message: TMessage);
var
  I: Integer;
begin
  inherited;

  if CheckBox.Visible and not CheckBox.Checked then
    Exit;

  for I := 0 to FButtons.Count - 1 do
    TGroupCheck(FButtons[I]).Enabled := Enabled;
end;

procedure TCustomAdvOfficeCheckGroup.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeCheckGroup.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

function TCustomAdvOfficeCheckGroup.XYToItem(X, Y: Integer): Integer;
var
  i: integer;
  r: TRect;
  pt: TPoint;
begin
  Result := -1;
  pt := Point(X,Y);
  for i := 0 to FButtons.Count - 1 do
  begin
    r :=  TGroupCheck(FButtons[i]).ClientRect;
    OffsetRect(r,   TGroupCheck(FButtons[i]).Left, TGroupCheck(FButtons[i]).Top);
    if PtInRect(r,pt) then
    begin
      Result := i;
      break;
    end;
  end;
end;

function TCustomAdvOfficeCheckGroup.CanModify: Boolean;
begin
  Result := True;
end;

procedure TCustomAdvOfficeCheckGroup.GetChildren(Proc: TGetChildProc; Root: TComponent);
begin
end;

procedure TCustomAdvOfficeCheckGroup.SetAlignment(const Value: TAlignment);
begin
  FAlignment := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeCheckGroup.SetButtonVertAlign(
  const Value: TTextLayout);
begin
  fBtnVAlign := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeCheckGroup.SetContainer(
  const Value: TPictureContainer);
begin
  FContainer := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeCheckGroup.SetControlIndent(const Value: integer);
begin
  if (FControlIndent <> Value) then
  begin
    FControlIndent := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.SetImages(const Value: TImageList);
begin
  inherited Images := Value;
  FImages := Value;
  ArrangeButtons;
end;

procedure TCustomAdvOfficeCheckGroup.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if (AOperation = opRemove) and (AComponent = FImages) then
    FImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;
end;

procedure TCustomAdvOfficeCheckGroup.SetEllipsis(const Value: Boolean);
begin
  if (FEllipsis <> Value) then
  begin
    FEllipsis := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.SetShadowColor(const Value: TColor);
begin
  if (FShadowColor <> Value) then
  begin
    FShadowColor := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.SetShadowOffset(const Value: Integer);
begin
  if (FShadowOffset <> Value) then
  begin
    FShadowOffset := Value;
    ArrangeButtons;
  end;
end;


procedure TCustomAdvOfficeCheckGroup.SetShowFocus(const Value: boolean);
begin
  if (FShowFocus <> Value) then
  begin
    FShowFocus := Value;
    ArrangeButtons;
  end;
end;

procedure TCustomAdvOfficeCheckGroup.SetThemed(const Value: boolean);
begin
  if (FThemed <> Value) then
  begin
    FThemed := Value;
    ArrangeButtons;
  end;
end;

function TCustomAdvOfficeCheckGroup.GetChecked(Index: Integer): Boolean;
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    Result := TGroupCheck(FButtons[Index]).Checked
  else
    raise Exception.Create('Invalid checkbox index');
end;

procedure TCustomAdvOfficeCheckGroup.SetChecked(Index: Integer;
  const Value: Boolean);
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
  begin
    TGroupCheck(FButtons[Index]).FInternalClick := true;
    TGroupCheck(FButtons[Index]).Checked := Value;
    TGroupCheck(FButtons[Index]).FInternalClick := false;
  end;
end;

function TCustomAdvOfficeCheckGroup.GetReadOnly(Index: Integer): Boolean;
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    Result := not TGroupCheck(FButtons[Index]).Enabled
  else
    raise Exception.Create('Invalid checkbox index');
end;

procedure TCustomAdvOfficeCheckGroup.SetReadOnly(Index: Integer;
  const Value: Boolean);
begin
  if (Index < FButtons.Count)  and (Index >= 0) then
    TGroupCheck(FButtons[Index]).Enabled := not Value;
end;

procedure TCustomAdvOfficeCheckGroup.UpdateValue;
var
  i, j: Integer;
  BitMask: DWord;
begin
  FValue := Value;
  j := Min(FButtons.Count, sizeof(DWord) * 8);
  BitMask := 1;
  FValue := 0;
  for i := 0 to j - 1 do
  begin
    if TGroupCheck(FButtons[i]).Checked then
    begin
      FValue := FValue or BitMask;
    end;
    BitMask := BitMask * 2;
  end;
end;

function TCustomAdvOfficeCheckGroup.GetValue: int64;
begin
  Result := FValue;
end;

procedure TCustomAdvOfficeCheckGroup.SetValue(const Value: int64);
var
  i, j: Integer;
  BitMask: int64;
begin
  //if (FValue <> Value) then
  begin
    FValue := Value;
    j := Min(FButtons.Count, SizeOf(int64) * 8);
    BitMask := 1;
    for i := 0 to j - 1 do
    begin
      TGroupCheck(FButtons[i]).Checked := ((FValue And BitMask) > 0);
      BitMask := BitMask * 2;
    end;
  end;
end;

function TCustomAdvOfficeCheckGroup.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TCustomAdvOfficeCheckGroup.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TCustomAdvOfficeCheckGroup.SetVersion(const Value: string);
begin

end;

procedure TAdvOfficeCheckBoxActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TCustomAdvOfficeCheckBox;
end;

procedure TAdvOfficeCheckBoxActionLink.SetChecked(Value: Boolean);
begin
  FClient.FInternalClick := true;
  FClient.Checked := Value;
  FClient.FInternalClick := false;
end;

procedure TAdvOfficeCheckBoxActionLink.SetCaption(const Value: string);
begin
  FClient.Caption := Value;
end;

function TAdvOfficeCheckBoxActionLink.IsCaptionLinked: Boolean;
begin
  Result := inherited IsCaptionLinked;
end;

function TAdvOfficeCheckBoxActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}

end.

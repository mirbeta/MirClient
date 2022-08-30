{*************************************************************************}
{ Curvy Controls :  panel, combobox, edit, memo control component         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 2010 - 2013                                      }
{            Email : info@tmssoftware.com                                 }
{            Website : http://www.tmssoftware.com/                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit CurvyControls;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, StdCtrls, forms,
  Math, GDIPFill, AdvGDIP, uxTheme, Menus, ImgList, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 9; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : Issue with ItemIndex in TCurvyComboBox
  // v1.0.0.2 : Fixed : Issue with triggering OnChange from selection in TCurvyComboBox
  // v1.0.0.3 : Fixed : Issue with TabOrder property
  // v1.0.0.4 : Fixed : Issue with SetFocus call
  // v1.0.1.0 : New : public property ShadowColor
  // v1.0.2.0 : New : SelStart/SelLength/SelectAll methods exposed in TCurvyMemo
  // v1.0.3.0 : Exposed TabStop/TabOrder in TCurvyPanel
  // v1.0.3.1 : Fixed : Issue with TabStop
  // v1.0.3.2 : Fixed : Issue with Shift tab handling
  // v1.0.3.3 : Fixed : Issue to work in XE2 styled apps
  // v1.0.4.0 : Improved : Removed limitation with Height setting on TCurvyEdit
  // v1.0.5.0 : New : Exposed SelStart/SelLength/SelectAll/SelText in TCurvyEdit, TCurvyCombo
  //          : New : Exposed Sorted in TCurvyComboBox
  //          : Fixed : Issue with Form.ActiveControl & focus handling
  //          : New : Exposed Sorted in TCurvyComboBox
  // v1.1.0.0 : New : Added property HideEmptyText on TCurvyEdit
  // v1.1.0.1 : Fixed : Issue with CanFocus
  // v1.1.1.0 : New : DropDownCount property added in TCurvyCombo
  // v1.1.1.1 : Improved : Painting of TCurvyComboBox
  // v1.1.1.2 : Fixed : Issue with OnChange triggering for TCurvyMemo
  // v1.1.1.3 : Fixed : Issue with EmptyText font
  // v1.1.1.4 : Fixed : Repaint issue on resize
  // v1.1.1.5 : Fixed : Issue with CurvyCombo and large fonts
  // v1.1.1.6 : Improved : Removed flickering in TCurvyComboBox
  // v1.1.1.7 : Fixed : Issue with painting in non Themed apps of TCurvyComboBox
  // v1.1.1.8 : Fixed : Issue with TabStop for runtime created controls
  // v1.1.1.9 : Fixed : Issue with borders with large rounding for panel


type
  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurvyPanel = class(TCustomControl)
  private
    FRounding: integer;
    FColor: TColor;
    FShadowColor: TColor;
    FBorderColor: TColor;
    FIndentRight: integer;
    FIndentLeft: integer;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetRounding(const Value: integer);
    procedure SetColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
  protected
    function IsPanel: boolean; virtual;
    procedure ColorChanged; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    property IndentLeft: integer read FIndentLeft write FIndentLeft;
    property IndentRight: integer read FIndentRight write FIndentRight;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property ShadowColor: TColor read FShadowColor write FShadowColor;
  published
    property Align;
    property Anchors;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clGray;
    property Color: TColor read FColor write SetColor default clWindow;
    property Constraints;
    property Rounding: integer read FRounding write SetRounding default 8;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnMouseDown;
    property OnMouseUp;
    {$IFDEF DELPHI2006_LVL}
    property OnAlignPosition;
    property OnMouseActivate;
    property OnMouseLeave;
    property OnMouseEnter;
    {$ENDIF}
    property OnMouseMove;
    property OnDockDrop;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnUnDock;
    property OnStartDock;
    property OnStartDrag;
    property OnCanResize;
    property OnContextPopup;
    property OnResize;
  end;

  TControlPosition = (cpLeftInControl,cpRightInControl,cpLeft,cpRight);
  TControlState = (csNormal, csHover, csDown);

  TCurvyControl = class(TCollectionItem)
  private
    FImageIndex: integer;
    FImageIndexHover: integer;
    FImageIndexDown: integer;
    FPicture: TAdvGDIPPicture;
    FPictureHover: TAdvGDIPPicture;
    FPictureDown: TAdvGDIPPicture;
    FHint: string;
    FDropDown: boolean;
    FPopupMenu: TPopupMenu;
    //FCaption: string;
    FPosition: TControlPosition;
    FState: TControlState;
    function GetImageIndex: integer;
    function GetImageIndexDown: integer;
    function GetImageIndexHover: integer;
    function GetPicture: TAdvGDIPPicture;
    function GetPictureHover: TAdvGDIPPicture;
    function GetPicureDown: TAdvGDIPPicture;
    //procedure SetCaption(const Value: string);
    procedure SetDropDown(const Value: boolean);
    procedure SetImageIndex(const Value: integer);
    procedure SetImageIndexDown(const Value: integer);
    procedure SetImageIndexHover(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetPictureDown(const Value: TAdvGDIPPicture);
    procedure SetPictureHover(const Value: TAdvGDIPPicture);
    procedure SetPosition(const Value: TControlPosition);
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
    procedure SetState(const Value: TControlState);
  protected
    property State: TControlState read FState write SetState;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    destructor Destroy; override;
  published
    //property Caption: string read FCaption write SetCaption;
    property DropDown: boolean read FDropDown write SetDropDown default false;
    property Hint: string read FHint write FHint;
    property ImageIndex: integer read GetImageIndex write SetImageIndex default -1;
    property ImageIndexHover: integer read GetImageIndexHover write SetImageIndexHover default -1;
    property ImageIndexDown: integer read GetImageIndexDown write SetImageIndexDown default -1;
    property Picture: TAdvGDIPPicture read GetPicture write SetPicture;
    property PictureHover: TAdvGDIPPicture read GetPictureHover write SetPictureHover;
    property PictureDown: TAdvGDIPPicture read GetPicureDown write SetPictureDown;
    property Position: TControlPosition read FPosition write SetPosition default cpRightInControl;
    property PopupMenu: TPopupMenu read FPopupMenu write FPopupMenu;
  end;

  TCurvyControls = class(TOwnedCollection)
  private
    FLeftSize: integer;
    FLeftControlSize: integer;
    FRightSize: integer;
    FRightControlSize: integer;
    FImageListImageWidth: integer;
    FImageListImageHeight: integer;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TCurvyControl;
    procedure SetItem(Index: Integer; const Value: TCurvyControl);
  protected
    procedure GetSizes;
    function GetControlSize(index: integer): TPoint;
    property LeftSize: integer read FLeftSize;
    property LeftControlSize: integer read FLeftControlSize;
    property RightSize: integer read FRightSize;
    property RightControlSize: integer read FRightControlSize;
    property ImageListImageWidth: integer read FImageListImageWidth write FImageListImageWidth;
    property ImageListImageHeight: integer read FImageListImageHeight write FImageListImageHeight;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    function Add: TCurvyControl;
    function Insert(Index: Integer): TCurvyControl;
    property Items[Index: Integer]: TCurvyControl read GetItem write SetItem; default;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TControlClickEvent = procedure(Sender: TObject; Index: integer) of object;

  TCurvyControlPanel = class(TCurvyPanel)
  private
    FCurvyControls: TCurvyControls;
    FImages: TCustomImageList;
    FOnControlClick: TControlClickEvent;
    procedure SetCurvyControls(const Value: TCurvyControls);
    procedure SetImages(const Value: TCustomImageList);
  protected
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure CMMouseLeave(Var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(Var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMHintShow(var Msg: TCMHintShow); message CM_HINTSHOW;
    procedure ControlsChanged(Sender: TObject);
    procedure PaintControl(x,y,index: integer);
    function HandleControls(x,y: integer; var ctrlx: integer; dopaint: boolean): integer;
    function ControlAtXY(x,y: integer): integer;
    procedure DoControlClick(index: integer); virtual;
    procedure UpdateControl;
    procedure UpdateEditControl; virtual;

    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation);  override;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Images: TCustomImageList read FImages write SetImages;
    property Controls: TCurvyControls read FCurvyControls write SetCurvyControls;
    property OnControlClick: TControlClickEvent read FOnControlClick write FOnControlClick;
  published
    property Version: string read GetVersion write SetVersion;
  end;

  THideEmptyText = (heOnText, heOnFocus);

  TEmptyEdit = class(TEdit)
  private
    FEmptyText: string;
    FHideEmptyText: THideEmptyText;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure SetEmptyText(const Value: string);
  public
    property EmptyText: string read FEmptyText write SetEmptyText;
    property HideEmptyText: THideEmptyText read FHideEmptyText write FHideEmptyText default heOnText;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurvyEdit = class(TCurvyControlPanel)
  private
    FEdit: TEmptyEdit;
    FOnClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    {$IFDEF DELPHI_UNICODE}
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    {$ENDIF}
    function GetAutoSelect: Boolean;
    procedure SetAutoSelect(const Value: Boolean);
    function GetEnabledEx: boolean;
    procedure SetEnabledEx(const Value: boolean);
    function GetHideSelection: boolean;
    function GetMaxLength: integer;
    procedure SetHideSelection(const Value: boolean);
    procedure SetMaxLength(const Value: integer);
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    procedure SetImeMode(const Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    {$IFDEF DELPHI_UNICODE}
    function GetNumbersOnly: boolean;
    procedure SetNumbersOnly(const Value: boolean);
    {$ENDIF}
    function GetOEMConvert: boolean;
    procedure SetOEMConvert(const Value: boolean);
    function GetPasswordChar: char;
    procedure SetPasswordChar(const Value: char);
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    {$IFDEF DELPHI_UNICODE}
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF}
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$IFDEF DELPHI_UNICODE}
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    {$ENDIF}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragModeI(const Value: TDragMode);
    function GetEmptyText: string;
    procedure SetEmptyText(const Value: string);
    function GetSelLength: integer;
    function GetSelStart: integer;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelText: string;
    procedure SetSelText(const Value: string);
    procedure SetHideEmptyText(const Value: THideEmptyText);
    function GetHideEmptyText: THideEmptyText;
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    procedure Loaded; override;
    function IsPanel: boolean; override;
    procedure ColorChanged; override;

    procedure EditClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditDblClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseLeave(Sender: TObject);
    procedure EditMouseEnter(Sender: TObject);
    procedure UpdateEditControl; override;
    procedure SetTabStop(const Value: boolean);
    function GetTabStop: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetFocus; override;
    function CanFocus: Boolean; override;
    procedure SelectAll;
    property Edit: TEmptyEdit read FEdit;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
  published
    property Align;
    {$IFDEF DELPHI_UNICODE}
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    {$ENDIF}
    property AutoSelect: Boolean read GetAutoSelect write SetAutoSelect default true;
    property BiDiMode;
    {$IFDEF DELPHI_UNICODE}
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    {$ENDIF}
    property Controls;
    property DragCursor: TCursor read GetDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    property DragMode: TDragMode read GetDragMode write SetDragModeI default dmManual;
    property EmptyText: string read GetEmptyText write SetEmptyText;
    property Enabled: boolean read GetEnabledEx write SetEnabledEx default true;
    property Font;
    property HideEmptyText: THideEmptyText read GetHideEmptyText write SetHideEmptyText default heOnText;
    property HideSelection: boolean read GetHideSelection write SetHideSelection default true;
    property Images;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default imDontCare;
    property ImeName: TImeName read GetImeName write SetImeName;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    {$IFDEF DELPHI_UNICODE}
    property NumbersOnly: boolean read GetNumbersOnly write SetNumbersOnly default false;
    {$ENDIF}
    property OEMConvert: boolean read GetOEMConvert write SetOEMConvert default false;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    {$IFDEF DELPHI_UNICODE}
    property ParentCustomHint;
    {$ENDIF}
    property PasswordChar: char read GetPasswordChar write SetPasswordChar default #0;
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenu;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property ShowHint;
    property TabOrder;
    property TabStop: boolean read GetTabStop write SetTabStop;
    property Text: TCaption read GetText write SetText;
    {$IFDEF DELPHI_UNICODE}
    property TextHint: string read GetTextHint write SetTextHint;
    {$ENDIF}
    {$IFDEF DELPHI2010_LVL}
    property Touch;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnControlClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurvyMemo = class(TCurvyPanel)
  private
    FEdit: TMemo;
    FLines: TStringList;
    FOnClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnDblClick: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnExit: TNotifyEvent;
    FOnKeyDown: TKeyEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyUp: TKeyEvent;
    FOnMouseDown: TMouseEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FStopChange: boolean;
    {$IFDEF DELPHI_UNICODE}
    function GetAlignment: TAlignment;
    procedure SetAlignment(const Value: TAlignment);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    {$ENDIF}
    function GetEnabledEx: boolean;
    procedure SetEnabledEx(const Value: boolean);
    function GetHideSelection: boolean;
    function GetMaxLength: integer;
    procedure SetHideSelection(const Value: boolean);
    procedure SetMaxLength(const Value: integer);
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    procedure SetImeMode(const Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    function GetOEMConvert: boolean;
    procedure SetOEMConvert(const Value: boolean);
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    function GetReadOnly: boolean;
    procedure SetReadOnly(const Value: boolean);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$IFDEF DELPHI_UNICODE}
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    {$ENDIF}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    function GetScrollStyle: TScrollStyle;
    procedure SetScrollStyle(const Value: TScrollStyle);
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragModeI(const Value: TDragMode);
    function GetLines: TStrings;
    procedure SetLines(const Value: TStrings);
    function GetSelLength: integer;
    function GetSelStart: integer;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    function GetSelText: string;
    procedure SetSelText(const Value: string);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    function IsPanel: boolean; override;
    procedure ColorChanged; override;
    procedure LinesChanged(Sender: TObject);
    procedure EditClick(Sender: TObject);
    procedure EditChange(Sender: TObject);
    procedure EditDblClick(Sender: TObject);
    procedure EditEnter(Sender: TObject);
    procedure EditExit(Sender: TObject);
    procedure EditKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure EditKeyPress(Sender: TObject; var Key: Char);
    procedure EditMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure EditMouseLeave(Sender: TObject);
    procedure EditMouseEnter(Sender: TObject);
    procedure SetTabStop(const Value: boolean);
    function GetTabStop: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetFocus; override;
    function CanFocus: Boolean; override;
    property Memo: TMemo read FEdit;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
    procedure SelectAll;
  published
    property Align;
    {$IFDEF DELPHI_UNICODE}
    property Alignment: TAlignment read GetAlignment write SetAlignment default taLeftJustify;
    {$ENDIF}
    property BiDiMode;
    {$IFDEF DELPHI_UNICODE}
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    {$ENDIF}
    property DragCursor: TCursor read GetDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    property DragMode: TDragMode read GetDragMode write SetDragModeI default dmManual;
    property Enabled: boolean read GetEnabledEx write SetEnabledEx default true;
    property Font;
    property HideSelection: boolean read GetHideSelection write SetHideSelection default true;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default imDontCare;
    property ImeName: TImeName read GetImeName write SetImeName;
    property Lines: TStrings read GetLines write SetLines;
    property MaxLength: integer read GetMaxLength write SetMaxLength default 0;
    property OEMConvert: boolean read GetOEMConvert write SetOEMConvert default false;
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    {$IFDEF DELPHI_UNICODE}
    property ParentCustomHint;
    {$ENDIF}
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenu;
    property ReadOnly: boolean read GetReadOnly write SetReadOnly default false;
    property ScrollBars: TScrollStyle read GetScrollStyle write SetScrollStyle default ssNone;
    property ShowHint;
    property TabOrder;
    property TabStop: boolean read GetTabStop write SetTabStop;
    {$IFDEF DELPHI2010_LVL}
    property Touch;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
  end;


  TBorderlessCustomComboBox = class(TCustomComboBox)
  private
    FButtonWidth: integer;
    FButtonHover: boolean;
    FForcePaint: boolean;
    FHasMouse: boolean;
    FOldButtonHover: boolean;
    procedure DrawControlBorder(DC: HDC);
    procedure DrawBorders;
    //procedure DrawButton(DC: HDC);
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint (var Message: TMessage); message WM_NCPAINT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CNCommand (var Message: TWMCommand); message CN_COMMAND;
  protected
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    function UsesCOM6: boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Text;
  end;

  TBorderlessComboBox = class(TBorderlessCustomComboBox)
  published
    property Items;
    property Text;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TCurvyCombo = class(TCurvyControlPanel)
  private
    FItems: TStringList;
    FItemIndex: integer;
    FCombo: TBorderlessComboBox;
    FOnMouseDown: TMouseEvent;
    FOnExit: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseMove: TMouseMoveEvent;
    FOnMouseUp: TMouseEvent;
    FOnKeyPress: TKeyPressEvent;
    FOnKeyDown: TKeyEvent;
    FOnDblClick: TNotifyEvent;
    FOnChange: TNotifyEvent;
    FOnEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnKeyUp: TKeyEvent;
    FOnClick: TNotifyEvent;
    FOnCloseUp: TNotifyEvent;
    FOnDropDown: TNotifyEvent;
    FOnSelect: TNotifyEvent;
    FDropDownCount: integer;
    function GetText: TCaption;
    procedure SetText(const Value: TCaption);
    function GetItems: TStrings;
    procedure SetItems(const Value: TStrings);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    {$IFDEF DELPHI_UNICODE}
    procedure CMParentFontChanged(var Message: TCMParentFontChanged); message CM_PARENTFONTCHANGED;
    {$ENDIF}
    procedure CMBiDiModeChanged(var Message: TMessage); message CM_BIDIMODECHANGED;
    procedure CMParentBiDiModeChanged(var Message: TMessage); message CM_PARENTBIDIMODECHANGED;
    procedure WMSetFocus(var Message: TMessage); message WM_SETFOCUS;
    function GetAutoCloseUp: boolean;
    function GetAutoComplete: boolean;
    {$IFDEF DELPHI_UNICODE}
    function GetAutoCompleteDelay: integer;
    procedure SetAutoCompleteDelay(const Value: integer);
    {$ENDIF}
    function GetAutoDropDown: boolean;
    procedure SetAutoCloseUp(const Value: boolean);
    procedure SetAutoComplete(const Value: boolean);
    procedure SetAutoDropDown(const Value: boolean);
    function GetCharCase: TEditCharCase;
    procedure SetCharCase(const Value: TEditCharCase);
    function GetEnabledEx: boolean;
    procedure SetEnabledEx(const Value: boolean);
    function GetImeMode: TImeMode;
    function GetImeName: TImeName;
    procedure SetImeMode(const Value: TImeMode);
    procedure SetImeName(const Value: TImeName);
    function GetPopupMenuEx: TPopupMenu;
    procedure SetPopupMenu(const Value: TPopupMenu);
    {$IFDEF DELPHI_UNICODE}
    function GetTextHint: string;
    procedure SetTextHint(const Value: string);
    {$ENDIF}
    function GetItemIndex: integer;
    procedure SetItemIndex(const Value: integer);
    function GetStyle: TComboBoxStyle;
    procedure SetStyle(const Value: TComboBoxStyle);
    function GetDragCursor: TCursor;
    function GetDragKind: TDragKind;
    function GetDragMode: TDragMode;
    procedure SetDragCursor(const Value: TCursor);
    procedure SetDragKind(const Value: TDragKind);
    procedure SetDragModeI(const Value: TDragMode);
    function GetSelLength: integer;
    function GetSelStart: integer;
    function GetSelText: string;
    procedure SetSelLength(const Value: integer);
    procedure SetSelStart(const Value: integer);
    procedure SetSelText(const Value: string);
    function GetSorted: boolean;
    procedure SetSorted(const Value: boolean);
    procedure SetDropDownCount(const Value: integer);
    function GetTabStop: boolean;
    procedure SetTabStop(const Value: boolean);
  protected
    procedure CreateWnd; override;
    procedure Resize; override;
    function IsPanel: boolean; override;
    procedure ColorChanged; override;
    procedure ItemsChanged(Sender: TObject); virtual;
    procedure Loaded; override;
    procedure ComboClick(Sender: TObject);
    procedure ComboChange(Sender: TObject);
    procedure ComboDblClick(Sender: TObject);
    procedure ComboEnter(Sender: TObject);
    procedure ComboExit(Sender: TObject);
    procedure ComboKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ComboKeyPress(Sender: TObject; var Key: Char);
    procedure ComboMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ComboMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ComboMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ComboMouseLeave(Sender: TObject);
    procedure ComboMouseEnter(Sender: TObject);
    procedure ComboDropDown(Sender: TObject);
    procedure ComboCloseUp(Sender: TObject);
    procedure ComboSelect(Sender: TObject);
    procedure UpdateCombo;
    procedure UpdateControl;
    procedure UpdateEditControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property Combo: TBorderlessComboBox read FCombo;
    procedure SetFocus; override;
    function CanFocus: Boolean; override;
    property SelStart: integer read GetSelStart write SetSelStart;
    property SelLength: integer read GetSelLength write SetSelLength;
    property SelText: string read GetSelText write SetSelText;
  published
    property Align;
    property AutoCloseUp: boolean read GetAutoCloseUp write SetAutoCloseUp default false;
    property AutoComplete: boolean read GetAutoComplete write SetAutoComplete default true;
    {$IFDEF DELPHI_UNICODE}
    property AutoCompleteDelay: integer read GetAutoCompleteDelay write SetAutoCompleteDelay default 500;
    {$ENDIF}
    property AutoDropDown: boolean read GetAutoDropDown write SetAutoDropDown default false;
    property BiDiMode;
    property CharCase: TEditCharCase read GetCharCase write SetCharCase default ecNormal;
    property Controls;
    property DragCursor: TCursor read GetDragCursor write SetDragCursor default crDrag;
    property DragKind: TDragKind read GetDragKind write SetDragKind default dkDrag;
    property DragMode: TDragMode read GetDragMode write SetDragModeI default dmManual;
    property DropDownCount: integer read FDropDownCount write SetDropDownCount default 8;
    property Enabled: boolean read GetEnabledEx write SetEnabledEx default true;
    property Images;
    property ImeMode: TImeMode read GetImeMode write SetImeMode default imDontCare;
    property ImeName: TImeName read GetImeName write SetImeName;

    property ItemIndex: integer read GetItemIndex write SetItemIndex default -1;
    property Items: TStrings read GetItems write SetItems;
    property Font;

    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    {$IFDEF DELPHI_UNICODE}
    property ParentCustomHint;
    {$ENDIF}
    property PopupMenu: TPopupMenu read GetPopupMenuEx write SetPopupMenu;
    property ShowHint;
    property Sorted: boolean read GetSorted write SetSorted;
    property Style: TComboBoxStyle read GetStyle write SetStyle default csDropDown;
    property TabOrder;
    property TabStop: boolean read GetTabStop write SetTabStop;
    property Text: TCaption read GetText write SetText;
    {$IFDEF DELPHI_UNICODE}
    property TextHint: string read GetTextHint write SetTextHint;
    {$ENDIF}
    {$IFDEF DELPHI2010_LVL}
    property Touch;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnControlClick;
    property OnDblClick: TNotifyEvent read FOnDblClick write FOnDblClick;
    property OnDropDown: TNotifyEvent read FOnDropDown write FOnDropDown;
    property OnEnter: TNotifyEvent read FOnEnter write FOnEnter;
    property OnExit: TNotifyEvent read FOnExit write FOnExit;
    property OnKeyDown: TKeyEvent read FOnKeyDown write FOnKeyDown;
    property OnKeyPress: TKeyPressEvent read FOnKeyPress write FOnKeyPress;
    property OnKeyUp: TKeyEvent read FOnKeyUp write FOnKeyUp;
    property OnMouseDown: TMouseEvent read FOnMouseDown write FOnMouseDown;
    property OnMouseMove: TMouseMoveEvent read FOnMouseMove write FOnMouseMove;
    property OnMouseUp: TMouseEvent read FOnMouseUp write FOnMouseUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    {$ENDIF}
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;


implementation

const
  CONTROL_SPACING = 2;

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

{ TCurvyPanel }

procedure TCurvyPanel.ColorChanged;
begin
  Repaint;
end;

constructor TCurvyPanel.Create(AOwner: TComponent);
begin
  inherited;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FRounding := 8;
  FColor := clWindow;
  FBorderColor := clGray;
  Width := 200;
  Height := 100;
  DoubleBuffered := true;
  FShadowColor := $D9D9D9;
end;

procedure TCurvyPanel.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    Style := Style and not WS_BORDER;
  end;
end;

destructor TCurvyPanel.Destroy;
begin
  inherited;
end;

function TCurvyPanel.IsPanel: boolean;
begin
  Result := true;
end;

procedure TCurvyPanel.Paint;
var
  g: TGPGraphics;
  fill: TGDIPFill;
  p: TGPPen;
begin
  fill := TGDIPFill.Create;

  try
    fill.BorderColor := ColorToRGB(FBorderColor);
    fill.Rounding := FRounding;
    fill.RoundingType := rtBoth;

    fill.Color := ColorToRGB(Color);
    fill.ColorTo := ColorToRGB(Color);
    fill.ColorMirror := ColorToRGB(Color);
    fill.ColorMirrorTo := ColorToRGB(Color);

    g := TGPGraphics.Create(Canvas.Handle);

    try
      g.SetSmoothingMode(SmoothingModeAntiAlias);

      fill.Fill(g,MakeRect(FIndentLeft, 0, Width - 1 - FIndentRight - FIndentLeft, Height - 1));

      if (FShadowColor <> clNone) and not IsPanel then
      begin
        p := TGPPen.Create(ColorToARGB(FShadowColor));

        g.DrawLine(p, FRounding + FIndentLeft, 1 , Width - FRounding - FIndentRight, 1);

        g.DrawArc(p, Width - Rounding - 3 - FIndentRight, 1, FRounding, FRounding , -90, 90);

        p.Free;
      end;

    finally
      g.Free;
    end;
  finally
    fill.Free;
  end;
end;

procedure TCurvyPanel.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

procedure TCurvyPanel.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    ColorChanged;
  end;
end;

procedure TCurvyPanel.SetRounding(const Value: integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    Invalidate;
  end;
end;

procedure TCurvyPanel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  {$IFDEF DELPHI2006_LVL}
  inherited;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  message.Result := 1;
  {$ENDIF}
end;

procedure TCurvyPanel.WMPaint(var Message: TWMPaint);
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
  if Assigned(Parent) then
  begin
    DC := Message.DC;
    if (DC <> 0) then
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


{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

{ TCurvyEdit }

function TCurvyEdit.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FEdit.Enabled;
end;

procedure TCurvyEdit.CMBiDiModeChanged(var Message: TMessage);
begin
  FEdit.BiDiMode := BiDiMode;
end;

procedure TCurvyEdit.CMFontChanged(var Message: TMessage);
begin
  FEdit.Font.Assign(Font);
end;

procedure TCurvyEdit.CMParentBiDiModeChanged(var Message: TMessage);
begin
  FEdit.BiDiMode := BiDiMode;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyEdit.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  FEdit.Font.Assign(Font);
end;
{$ENDIF}

procedure TCurvyEdit.ColorChanged;
begin
  inherited;
  FEdit.Color := Color;
end;

constructor TCurvyEdit.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := TEmptyEdit.Create(Self);
  FEdit.BorderStyle := bsNone;
  FEdit.Height := 18;
  FEdit.TabStop := false;

  Height := 24;
  Width := 100;
  FEdit.OnChange := EditChange;
  FEdit.OnClick := EditClick;
  FEdit.OnDblClick := EditDblClick;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyUp := EditKeyUp;
  FEdit.OnKeyPress := EditKeypress;
  FEdit.OnEnter := EditEnter;
  FEdit.OnExit := EditExit;
  {$IFDEF DELPHI2007_LVL}
  FEdit.OnMouseEnter := EditMouseEnter;
  FEdit.OnMouseLeave := EditMouseLeave;
  {$ENDIF}
  FEdit.OnMouseDown := EditMouseDown;
  FEdit.OnMouseUp := EditMouseUp;
  FEdit.OnMouseMove := EditMouseMove;
end;

procedure TCurvyEdit.CreateWnd;
begin
  inherited;
  FEdit.Parent := Self;
  FEdit.Top := 5;
  UpdateEditControl;
  //Height := 24;
end;

destructor TCurvyEdit.Destroy;
begin
  FEdit.Free;
  inherited;
end;

procedure TCurvyEdit.EditChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCurvyEdit.EditClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCurvyEdit.EditDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TCurvyEdit.EditEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TCurvyEdit.EditExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TCurvyEdit.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCurvyEdit.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TCurvyEdit.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure TCurvyEdit.EditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCurvyEdit.EditMouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCurvyEdit.EditMouseLeave(Sender: TObject);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCurvyEdit.EditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCurvyEdit.EditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyEdit.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;
{$ENDIF}

function TCurvyEdit.GetAutoSelect: Boolean;
begin
  Result := FEdit.AutoSelect;
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyEdit.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;
{$ENDIF}

function TCurvyEdit.GetDragCursor: TCursor;
begin
  Result := FEdit.DragCursor;
end;

function TCurvyEdit.GetDragKind: TDragKind;
begin
  Result := FEdit.DragKind;
end;

function TCurvyEdit.GetDragMode: TDragMode;
begin
  Result := FEdit.DragMode;
end;

function TCurvyEdit.GetEmptyText: string;
begin
  Result := FEdit.EmptyText;
end;

function TCurvyEdit.GetEnabledEx: boolean;
begin
  Result := FEdit.Enabled;
end;

function TCurvyEdit.GetHideEmptyText: THideEmptyText;
begin
  Result := FEdit.HideEmptyText;
end;

function TCurvyEdit.GetHideSelection: boolean;
begin
  Result := FEdit.HideSelection;
end;

function TCurvyEdit.GetImeMode: TImeMode;
begin
  Result := FEdit.ImeMode;
end;

function TCurvyEdit.GetImeName: TImeName;
begin
  Result := FEdit.ImeName;
end;

function TCurvyEdit.GetMaxLength: integer;
begin
  Result := FEdit.MaxLength;
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyEdit.GetNumbersOnly: boolean;
begin
  Result := FEdit.NumbersOnly;
end;
{$ENDIF}

function TCurvyEdit.GetOEMConvert: boolean;
begin
  Result := FEdit.OEMConvert;
end;

function TCurvyEdit.GetPasswordChar: char;
begin
  Result := FEdit.PasswordChar;
end;

function TCurvyEdit.GetPopupMenuEx: TPopupMenu;
begin
  Result := FEdit.PopupMenu;
end;

function TCurvyEdit.GetReadOnly: boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TCurvyEdit.GetSelLength: integer;
begin
  Result := FEdit.SelLength;
end;

function TCurvyEdit.GetSelStart: integer;
begin
  Result := FEdit.SelStart;
end;

function TCurvyEdit.GetSelText: string;
begin
  Result := FEdit.SelText;
end;

function TCurvyEdit.GetTabStop: boolean;
begin
  Result := inherited TabStop;
end;

function TCurvyEdit.GetText: TCaption;
begin
  Result := FEdit.Text;
end;

function TCurvyEdit.IsPanel: boolean;
begin
  Result := false;
end;

procedure TCurvyEdit.Loaded;
begin
  inherited;

  if not (csDesigning in ComponentState) then
  begin
    FEdit.TabStop := TabStop;
    inherited TabStop := false;
  end;
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyEdit.GetTextHint: string;
begin
  Result := FEdit.TextHint;
end;
{$ENDIF}

procedure TCurvyEdit.Resize;
begin
  inherited;

  if Assigned(FEdit) and FEdit.HandleAllocated then
    UpdateEditControl;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyEdit.SetAlignment(const Value: TAlignment);
begin
  FEdit.Alignment := Value;
end;
{$ENDIF}

procedure TCurvyEdit.SelectAll;
begin

end;

procedure TCurvyEdit.SetAutoSelect(const Value: Boolean);
begin
  FEdit.AutoSelect := Value;
end;

procedure TCurvyEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
//  AHeight := 24;
  inherited;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyEdit.SetCharCase(const Value: TEditCharCase);
begin
  FEdit.CharCase := Value;
end;
{$ENDIF}

procedure TCurvyEdit.SetDragCursor(const Value: TCursor);
begin
  FEdit.DragCursor := Value;
end;

procedure TCurvyEdit.SetDragKind(const Value: TDragKind);
begin
  FEdit.DragKind := Value;
end;

procedure TCurvyEdit.SetDragModeI(const Value: TDragMode);
begin
  FEdit.DragMode := Value;
end;

procedure TCurvyEdit.SetEmptyText(const Value: string);
begin
  FEdit.EmptyText := Value;
  FEdit.Repaint;
end;

procedure TCurvyEdit.SetEnabledEx(const Value: boolean);
begin
  FEdit.Enabled := Value;
end;

procedure TCurvyEdit.SetFocus;
begin
  FEdit.SetFocus;
end;

procedure TCurvyEdit.SetHideEmptyText(const Value: THideEmptyText);
begin
  Fedit.FHideEmptyText := Value;
end;

procedure TCurvyEdit.SetHideSelection(const Value: boolean);
begin
  FEdit.HideSelection := Value;
end;

procedure TCurvyEdit.SetImeMode(const Value: TImeMode);
begin
  FEdit.ImeMode := Value;
end;

procedure TCurvyEdit.SetImeName(const Value: TImeName);
begin
  FEdit.ImeName := Value;
end;

procedure TCurvyEdit.SetMaxLength(const Value: integer);
begin
  FEdit.MaxLength := Value;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyEdit.SetNumbersOnly(const Value: boolean);
begin
  FEdit.NumbersOnly := Value;
end;
{$ENDIF}

procedure TCurvyEdit.SetOEMConvert(const Value: boolean);
begin
  FEdit.OEMConvert := Value;
end;

procedure TCurvyEdit.SetPasswordChar(const Value: char);
begin
  FEdit.PasswordChar := Value;
end;

procedure TCurvyEdit.SetPopupMenu(const Value: TPopupMenu);
begin
  FEdit.PopupMenu := Value;
end;

procedure TCurvyEdit.SetReadOnly(const Value: boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TCurvyEdit.SetSelLength(const Value: integer);
begin
  FEdit.SelLength := Value;
end;

procedure TCurvyEdit.SetSelStart(const Value: integer);
begin
  FEdit.SelStart := Value;
end;

procedure TCurvyEdit.SetSelText(const Value: string);
begin
  FEdit.SelText := Value;
end;

procedure TCurvyEdit.SetTabStop(const Value: boolean);
begin
  inherited TabStop := Value;

  // setting TabStop at runtime
  if not (csReading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    FEdit.TabStop := Value;
    inherited TabStop := false;
  end;
end;

procedure TCurvyEdit.SetText(const Value: TCaption);
begin
  FEdit.Text := Value;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyEdit.SetTextHint(const Value: string);
begin
  FEdit.TextHint := Value;
end;
{$ENDIF}

procedure TCurvyEdit.UpdateEditControl;
begin
  FEdit.Left := FRounding + FCurvyControls.LeftSize + FCurvyControls.LeftControlSize;
  FEdit.Width := Width - 2 * FRounding - (FCurvyControls.RightSize + FCurvyControls.RightControlSize +
     FCurvyControls.LeftSize + FCurvyControls.LeftControlSize);
  FEdit.Height := Height - 6;
end;

procedure TCurvyEdit.WMSetFocus(var Message: TMessage);
begin
  FEdit.SetFocus;
  Message.Result := 1;
end;

{ TCurvyMemo }

function TCurvyMemo.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FEdit.Enabled;
end;

procedure TCurvyMemo.CMBiDiModeChanged(var Message: TMessage);
begin
  FEdit.BiDiMode := BiDiMode;
end;

procedure TCurvyMemo.CMFontChanged(var Message: TMessage);
begin
  FEdit.Font.Assign(Font);
end;

procedure TCurvyMemo.CMParentBiDiModeChanged(var Message: TMessage);
begin
  FEdit.BiDiMode := BiDiMode;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyMemo.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  FEdit.Font.Assign(Font);
end;
{$ENDIF}

procedure TCurvyMemo.ColorChanged;
begin
  inherited;
  FEdit.Color := Color;
end;

constructor TCurvyMemo.Create(AOwner: TComponent);
begin
  inherited;
  FEdit := TMemo.Create(Self);
  FLines := TStringList.Create;
  FLines.OnChange := LinesChanged;
  FEdit.BorderStyle := bsNone;
  FEdit.Height := 18;
  Height := 24;
  Width := 100;
  FStopChange := false;
  FEdit.TabStop := false;
  FEdit.OnChange := EditChange;
  FEdit.OnClick := EditClick;
  FEdit.OnDblClick := EditDblClick;
  FEdit.OnKeyDown := EditKeyDown;
  FEdit.OnKeyUp := EditKeyUp;
  FEdit.OnKeyPress := EditKeypress;
  FEdit.OnEnter := EditEnter;
  FEdit.OnExit := EditExit;
  {$IFDEF DELPHI2007_LVL}
  FEdit.OnMouseEnter := EditMouseEnter;
  FEdit.OnMouseLeave := EditMouseLeave;
  {$ENDIF}
  FEdit.OnMouseDown := EditMouseDown;
  FEdit.OnMouseUp := EditMouseUp;
  FEdit.OnMouseMove := EditMouseMove;
end;

procedure TCurvyMemo.CreateWnd;
begin
  inherited;
  FEdit.Parent := Self;
  FEdit.Top := FRounding;
  FEdit.Left := FRounding;
  FEdit.Lines.Assign(FLines);
end;

destructor TCurvyMemo.Destroy;
begin
  FEdit.Free;
  FLines.Free;
  inherited;
end;

procedure TCurvyMemo.EditChange(Sender: TObject);
begin
  if FStopChange then
    Exit;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCurvyMemo.EditClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCurvyMemo.EditDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TCurvyMemo.EditEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TCurvyMemo.EditExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TCurvyMemo.EditKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCurvyMemo.EditKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TCurvyMemo.EditKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure TCurvyMemo.EditMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCurvyMemo.EditMouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCurvyMemo.EditMouseLeave(Sender: TObject);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCurvyMemo.EditMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCurvyMemo.EditMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyMemo.GetAlignment: TAlignment;
begin
  Result := FEdit.Alignment;
end;
{$ENDIF}

{$IFDEF DELPHI_UNICODE}
function TCurvyMemo.GetCharCase: TEditCharCase;
begin
  Result := FEdit.CharCase;
end;
{$ENDIF}

function TCurvyMemo.GetDragCursor: TCursor;
begin
  Result := FEdit.DragCursor;
end;

function TCurvyMemo.GetDragKind: TDragKind;
begin
  Result := FEdit.DragKind;
end;

function TCurvyMemo.GetDragMode: TDragMode;
begin
  Result := FEdit.DragMode;
end;

function TCurvyMemo.GetEnabledEx: boolean;
begin
  Result := FEdit.Enabled;
end;

function TCurvyMemo.GetHideSelection: boolean;
begin
  Result := FEdit.HideSelection;
end;

function TCurvyMemo.GetImeMode: TImeMode;
begin
  Result := FEdit.ImeMode;
end;

function TCurvyMemo.GetImeName: TImeName;
begin
  Result := FEdit.ImeName;
end;

function TCurvyMemo.GetLines: TStrings;
begin
  FStopChange := true;
  FLines.Assign(FEdit.Lines);
  Result := FLines;
  FStopChange := false;
end;

function TCurvyMemo.GetMaxLength: integer;
begin
  Result := FEdit.MaxLength;
end;

function TCurvyMemo.GetOEMConvert: boolean;
begin
  Result := FEdit.OEMConvert;
end;

function TCurvyMemo.GetPopupMenuEx: TPopupMenu;
begin
  Result := FEdit.PopupMenu;
end;

function TCurvyMemo.GetReadOnly: boolean;
begin
  Result := FEdit.ReadOnly;
end;

function TCurvyMemo.GetScrollStyle: TScrollStyle;
begin
  Result := FEdit.ScrollBars;
end;

function TCurvyMemo.GetSelLength: integer;
begin
  Result := FEdit.SelLength;
end;

function TCurvyMemo.GetSelStart: integer;
begin
  Result := FEdit.SelStart;
end;

function TCurvyMemo.GetSelText: string;
begin
  Result := FEdit.SelText;
end;

function TCurvyMemo.GetTabStop: boolean;
begin
  Result := inherited TabStop;
end;

function TCurvyMemo.IsPanel: boolean;
begin
  Result := false;
end;

procedure TCurvyMemo.LinesChanged(Sender: TObject);
begin
  if FEdit.HandleAllocated then
    FEdit.Lines.Assign(FLines);
end;

procedure TCurvyMemo.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FEdit.TabStop := TabStop;
    inherited TabStop := false;
  end;
end;

procedure TCurvyMemo.Resize;
begin
  inherited;
  FEdit.Width := Width - FRounding * 2;
  FEdit.Height := Height - FRounding * 2;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyMemo.SetAlignment(const Value: TAlignment);
begin
  FEdit.Alignment := Value;
end;
{$ENDIF}

procedure TCurvyMemo.SelectAll;
begin
  FEdit.SelectAll;
end;

procedure TCurvyMemo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyMemo.SetCharCase(const Value: TEditCharCase);
begin
  FEdit.CharCase := Value;
end;
{$ENDIF}

procedure TCurvyMemo.SetDragCursor(const Value: TCursor);
begin
  FEdit.DragCursor := Value;
end;

procedure TCurvyMemo.SetDragKind(const Value: TDragKind);
begin
  FEdit.DragKind := Value;
end;

procedure TCurvyMemo.SetDragModeI(const Value: TDragMode);
begin
  FEdit.DragMode := Value;
end;

procedure TCurvyMemo.SetEnabledEx(const Value: boolean);
begin
  FEdit.Enabled := Value;
end;

procedure TCurvyMemo.SetFocus;
begin
  FEdit.SetFocus;
end;

procedure TCurvyMemo.SetHideSelection(const Value: boolean);
begin
  FEdit.HideSelection := Value;
end;

procedure TCurvyMemo.SetImeMode(const Value: TImeMode);
begin
  FEdit.ImeMode := Value;
end;

procedure TCurvyMemo.SetImeName(const Value: TImeName);
begin
  FEdit.ImeName := Value;
end;

procedure TCurvyMemo.SetLines(const Value: TStrings);
begin
  FLines.Assign(Value);
end;

procedure TCurvyMemo.SetMaxLength(const Value: integer);
begin
  FEdit.MaxLength := Value;
end;

procedure TCurvyMemo.SetOEMConvert(const Value: boolean);
begin
  FEdit.OEMConvert := Value;
end;

procedure TCurvyMemo.SetPopupMenu(const Value: TPopupMenu);
begin
  FEdit.PopupMenu := Value;
end;

procedure TCurvyMemo.SetReadOnly(const Value: boolean);
begin
  FEdit.ReadOnly := Value;
end;

procedure TCurvyMemo.SetScrollStyle(const Value: TScrollStyle);
begin
  FEdit.ScrollBars := Value;
end;

procedure TCurvyMemo.SetSelLength(const Value: integer);
begin
  FEdit.SelLength := Value;
end;

procedure TCurvyMemo.SetSelStart(const Value: integer);
begin
  FEdit.SelStart := Value;
end;

procedure TCurvyMemo.SetSelText(const Value: string);
begin
  FEdit.SelText := Value;
end;

procedure TCurvyMemo.SetTabStop(const Value: boolean);
begin
  inherited TabStop := Value;

  // setting TabStop at runtime
  if not (csReading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    FEdit.TabStop := Value;
    inherited TabStop := false;
  end;
end;

procedure TCurvyMemo.WMSetFocus(var Message: TMessage);
begin
  FEdit.SetFocus;
  Message.Result := 1;
end;

{ TBorderlessCustomComboBox }

procedure TBorderlessCustomComboBox.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHasMouse := true;
end;

procedure TBorderlessCustomComboBox.CMMouseLeave(var Message: TMessage);
var
  pt: TPoint;
begin
  inherited;

  GetCursorPos(pt);

  pt := ScreenToClient(pt);

  if not ptInRect(clientrect,pt) then
  begin
    FHasMouse := false;
  end;

  if FButtonHover then
  begin
    FButtonHover := false;
    Invalidate;
  end;

end;

procedure TBorderlessCustomComboBox.CNCommand(var Message: TWMCommand);
begin
  inherited;

  if (Message.NotifyCode in [CBN_CLOSEUP]) then
  begin
    FForcePaint := true;
    Invalidate;
  end;
end;

constructor TBorderlessCustomComboBox.Create(AOwner: TComponent);
begin
  inherited;
  Ctl3D := false;
  ParentCtl3D := false;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL) + 2;
  FButtonHover := false;
  FHasMouse := false;
end;

destructor TBorderlessCustomComboBox.Destroy;
begin
  inherited;
end;

function TBorderlessCustomComboBox.UsesCOM6: boolean;
var
  i: Integer;
begin
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  Result := (i > 5)
end;


(*
procedure TBorderlessCustomComboBox.DrawButton(DC: HDC);
var
  ARect: TRect;
  htheme: THandle;
  voffset: integer;

begin
  GetWindowRect(Handle, ARect);
  OffsetRect(ARect, -ARect.Left, -ARect.Top);
  Inc(ARect.Left, ClientWidth - FButtonWidth + 2);
  InflateRect(ARect, -1, -1);

  if not (UsesCom6 and IsThemeActive) then
  begin
    ExcludeClipRect(DC, ClientWidth - FButtonWidth - 4 , 0, ClientWidth + 2, ClientHeight);
    Exit;
  end;

  if IsThemeActive then
  begin
    htheme := OpenThemeData(Handle,'combobox');

    if not Enabled then
    begin
      DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_DISABLED,ARect,nil)
    end
    else
    begin
      if DroppedDown then
      begin
        DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_PRESSED or CBXS_HOT,ARect,nil);
      end
      else
      begin
        if not FButtonHover then
        begin
          voffset := 8;
          ARect.Left := ARect.Left + 4;

          Canvas.Pen.Color := clBlack;
          Canvas.MoveTo(ARect.Left, ARect.Top + voffset);
          Canvas.LineTo(ARect.Left + 7, ARect.Top + voffset);

          inc(voffset);

          Canvas.MoveTo(ARect.Left  + 1, ARect.Top + voffset);
          Canvas.LineTo(ARect.Left + 6, ARect.Top + voffset);

          inc(voffset);

          Canvas.MoveTo(ARect.Left  + 2, ARect.Top + voffset);
          Canvas.LineTo(ARect.Left + 5, ARect.Top + voffset);

          inc(voffset);

          Canvas.MoveTo(ARect.Left  + 3, ARect.Top + voffset);
          Canvas.LineTo(ARect.Left + 4, ARect.Top + voffset);
        end
        else
          DrawThemeBackground(htheme,DC,CP_DROPDOWNBUTTON,CBXS_HOT,ARect,nil);
      end;
    end;
    CloseThemeData(htheme);
  end
  else
  begin
    if Enabled then
      DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_FLAT )
    else
      DrawFrameControl(DC, ARect, DFC_SCROLL, DFCS_SCROLLCOMBOBOX or DFCS_INACTIVE )
  end;

  ExcludeClipRect(DC, ClientWidth - FButtonWidth - 4 , 0, ClientWidth + 2, ClientHeight);
end;
*)

procedure TBorderlessCustomComboBox.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  WindowBrush: HBRUSH;
  bw: Integer;

begin
  WindowBrush := CreateSolidBrush(ColorToRGB(Color));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);

    FrameRect(DC, ARect, WindowBrush);

    if Enabled then
    begin
      if (UsesCOM6 and IsThemeActive) then
        bw := FButtonWidth
      else
        bw := 0;

      InflateRect(ARect, -1, -1);
      ARect.Right := ARect.Right - bw;
      FrameRect(DC, ARect, WindowBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, WindowBrush);
      ARect.Right := ARect.Right + bw;
    end;

  finally
    DeleteObject(WindowBrush);
  end;
end;

procedure TBorderlessCustomComboBox.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if (X > Width - FButtonWidth) and (X < Width) then
  begin
    if not FButtonHover then
    begin
      FButtonHover := True;
      Invalidate;
    end;
  end
  else
  begin
    if FButtonHover then
    begin
      FButtonHover := False;
      Invalidate;
    end;
  end;

end;

procedure TBorderlessCustomComboBox.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TBorderlessCustomComboBox.DrawBorders;
var
  DC: HDC;
begin
  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    {
    if (Style <> csSimple) and not
      (FIsWinXP and DoVisualStyles) then
        DrawButtonBorder(DC);
    }
  finally
    ReleaseDC(Handle,DC);
  end;
end;


procedure TBorderlessCustomComboBox.WMNCPaint(var Message: TMessage);
begin
  inherited;
  DrawBorders;
end;

procedure TBorderlessCustomComboBox.WMPaint(var Message: TWMPaint);
var
  DC: HDC;
  PS: TPaintStruct;

begin
  inherited;

  if (Message.DC = 0) then
    DC := BeginPaint(Handle, PS)
  else
    DC := Message.DC;
  try
    if (Style <> csSimple) or not FHasMouse then
    begin
      if (FOldButtonHover <> FButtonHover) or not FHasMouse then
      begin
        FillRect(DC, ClientRect, Brush.Handle);
      end;
      FOldButtonHover := FButtonHover;
      FForcePaint := false;
      //DrawButton(DC);
    end;

    if (Style = csDropDownList) then
      PaintWindow(DC);
  finally
    if Message.DC = 0 then
      EndPaint(Handle, PS);
  end;

  DrawBorders;

  Message.Result := 1;
end;

{ TCurvyCombo }

function TCurvyCombo.CanFocus: Boolean;
begin
  Result := inherited CanFocus and FCombo.Enabled;
end;

procedure TCurvyCombo.CMBiDiModeChanged(var Message: TMessage);
begin
  FCombo.BiDiMode := BiDiMode;
end;

procedure TCurvyCombo.CMFontChanged(var Message: TMessage);
begin
  FCombo.Font.Assign(Font);
  SetBounds(Left,Top,Width,Height);
end;

procedure TCurvyCombo.CMParentBiDiModeChanged(var Message: TMessage);
begin
  FCombo.BiDiMode := BiDiMode;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyCombo.CMParentFontChanged(var Message: TCMParentFontChanged);
begin
  FCombo.Font.Assign(Font);
end;
{$ENDIF}

procedure TCurvyCombo.ColorChanged;
begin
  inherited;
  FCombo.Color := FColor;
end;

procedure TCurvyCombo.ComboChange(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCurvyCombo.ComboClick(Sender: TObject);
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TCurvyCombo.ComboCloseUp(Sender: TObject);
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self);
end;

procedure TCurvyCombo.ComboDblClick(Sender: TObject);
begin
  if Assigned(FOnDblClick) then
    FOnDblClick(Self);
end;

procedure TCurvyCombo.ComboDropDown(Sender: TObject);
begin
  if Assigned(FOnDropDown) then
    FOnDropDown(Self);
end;

procedure TCurvyCombo.ComboEnter(Sender: TObject);
begin
  if Assigned(FOnEnter) then
    FOnEnter(Self);
end;

procedure TCurvyCombo.ComboExit(Sender: TObject);
begin
  if Assigned(FOnExit) then
    FOnExit(Self);
end;

procedure TCurvyCombo.ComboKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyDown) then
    FOnKeyDown(Self, Key, Shift);
end;

procedure TCurvyCombo.ComboKeyPress(Sender: TObject; var Key: Char);
begin
  if Assigned(FOnKeyPress) then
    FOnKeyPress(Self, Key);
end;

procedure TCurvyCombo.ComboKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if Assigned(FOnKeyUp) then
    FOnKeyUp(Self, Key, Shift);
end;

procedure TCurvyCombo.ComboMouseDown(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseDown) then
    FOnMouseDown(Self, Button, Shift, X, Y);
end;

procedure TCurvyCombo.ComboMouseEnter(Sender: TObject);
begin
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TCurvyCombo.ComboMouseLeave(Sender: TObject);
begin
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

procedure TCurvyCombo.ComboMouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self, Shift, X, Y);
end;

procedure TCurvyCombo.ComboMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnMouseUp) then
    FOnMouseUp(Self, Button, Shift, X, Y);
end;

procedure TCurvyCombo.ComboSelect(Sender: TObject);
begin
  if Assigned(FOnSelect) then
    FOnSelect(Self);
  if Assigned(FOnChange) then
    FOnChange(Self);
end;


constructor TCurvyCombo.Create(AOwner: TComponent);
begin
  inherited;
  FCombo := TBorderlessComboBox.Create(Self);
  FItems := TStringList.Create;

  Height := 24;
  Width := 100;
  FItems.OnChange := ItemsChanged;
  FItemIndex := -1;

  FDropDownCount := 8;

  FCombo.TabStop := false;

  FCombo.OnChange := ComboChange;
  FCombo.OnClick := ComboClick;
  FCombo.OnKeyDown := ComboKeyDown;
  FCombo.OnKeyUp := ComboKeyUp;
  FCombo.OnKeyPress := ComboKeyPress;
  FCombo.OnMouseDown := ComboMouseDown;
  FCombo.OnMouseUp := ComboMouseUp;
  FCombo.OnMouseMove := ComboMouseMove;
  {$IFDEF DELPHI2007_LVL}
  FCombo.OnMouseLeave := ComboMouseLeave;
  FCombo.OnMouseEnter := ComboMouseEnter;
  {$ENDIF}
  FCombo.OnEnter := ComboEnter;
  FCombo.OnExit := ComboExit;
  FCombo.OnDropDown := ComboDropDown;
  FCombo.OnCloseUp := ComboCloseUp;
  FCombo.OnSelect := ComboSelect;
end;

procedure TCurvyCombo.CreateWnd;
begin
  inherited;
  FCombo.Parent := Self;
  FCombo.Top := 2;
  FCombo.Left := FRounding;

  UpdateEditControl;

//  Height := 24;
  FCombo.Items.Assign(FItems);
  FCombo.ItemIndex := FItemIndex;

  Height := FCombo.Height;
end;

destructor TCurvyCombo.Destroy;
begin
  FCombo.Free;
  FItems.Free;
  inherited;
end;

function TCurvyCombo.GetAutoCloseUp: boolean;
begin
  Result := FCombo.AutoCloseUp;
end;

function TCurvyCombo.GetAutoComplete: boolean;
begin
  Result := FCombo.AutoComplete;
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyCombo.GetAutoCompleteDelay: integer;
begin
  Result := FCombo.AutoCompleteDelay;
end;
{$ENDIF}

function TCurvyCombo.GetAutoDropDown: boolean;
begin
  Result := FCombo.AutoDropDown;
end;

function TCurvyCombo.GetCharCase: TEditCharCase;
begin
  Result := FCombo.CharCase;
end;

function TCurvyCombo.GetDragCursor: TCursor;
begin
  Result := FCombo.DragCursor;
end;

function TCurvyCombo.GetDragKind: TDragKind;
begin
  Result := FCombo.DragKind;
end;

function TCurvyCombo.GetDragMode: TDragMode;
begin
  Result := FCombo.DragMode;
end;

function TCurvyCombo.GetEnabledEx: boolean;
begin
  Result := FCombo.Enabled;
end;

function TCurvyCombo.GetImeMode: TImeMode;
begin
  Result := FCombo.ImeMode;
end;

function TCurvyCombo.GetImeName: TImeName;
begin
  Result := FCombo.ImeName;
end;

function TCurvyCombo.GetItemIndex: integer;
begin
  if FCombo.HandleAllocated then
    Result := FCombo.ItemIndex
  else
    Result := FItemIndex;
end;

function TCurvyCombo.GetItems: TStrings;
begin
  Result := FItems;
end;

function TCurvyCombo.GetPopupMenuEx: TPopupMenu;
begin
  Result := FCombo.PopupMenu;
end;

function TCurvyCombo.GetSelLength: integer;
begin
  Result := FCombo.SelLength;
end;

function TCurvyCombo.GetSelStart: integer;
begin
  Result := FCombo.SelStart;
end;

function TCurvyCombo.GetSelText: string;
begin
  Result := FCombo.SelText;
end;

function TCurvyCombo.GetSorted: boolean;
begin
  Result := FCombo.Sorted;
end;

function TCurvyCombo.GetStyle: TComboBoxStyle;
begin
  Result := FCombo.Style;
end;

function TCurvyCombo.GetTabStop: boolean;
begin
  Result := inherited TabStop;
end;

function TCurvyCombo.GetText: TCaption;
begin
  Result := FCombo.Text;
end;

{$IFDEF DELPHI_UNICODE}
function TCurvyCombo.GetTextHint: string;
begin
  Result := FCombo.TextHint;
end;
{$ENDIF}

function TCurvyCombo.IsPanel: boolean;
begin
  Result := false;
end;

procedure TCurvyCombo.ItemsChanged(Sender: TObject);
begin
  if FCombo.HandleAllocated then
    FCombo.Items.Assign(FItems);
end;

procedure TCurvyCombo.Loaded;
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    FCombo.TabStop := TabStop;
    TabStop := false;
  end;
end;

procedure TCurvyCombo.Resize;
begin
  inherited;
  if Assigned(FCombo) and FCombo.HandleAllocated then
  begin
    UpdateEditControl;
  end;
end;

procedure TCurvyCombo.SetAutoCloseUp(const Value: boolean);
begin
  FCombo.AutoCloseUp := Value;
end;

procedure TCurvyCombo.SetAutoComplete(const Value: boolean);
begin
  FCombo.AutoComplete := Value;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyCombo.SetAutoCompleteDelay(const Value: integer);
begin
  FCombo.AutoCompleteDelay := Value;
end;
{$ENDIF}

procedure TCurvyCombo.SetAutoDropDown(const Value: boolean);
begin
  FCombo.AutoDropDown := Value;
end;

procedure TCurvyCombo.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  if Assigned(FCombo) then
    AHeight := FCombo.Height + 3
  else
    AHeight := 24;

  inherited;
end;

procedure TCurvyCombo.SetCharCase(const Value: TEditCharCase);
begin
  FCombo.CharCase := Value;
end;

procedure TCurvyCombo.SetDragCursor(const Value: TCursor);
begin
  FCombo.DragCursor := Value;
end;

procedure TCurvyCombo.SetDragKind(const Value: TDragKind);
begin
  FCombo.DragKind := Value;
end;

procedure TCurvyCombo.SetDragModeI(const Value: TDragMode);
begin
  FCombo.DragMode := Value;
end;

procedure TCurvyCombo.SetEnabledEx(const Value: boolean);
begin
  FCombo.Enabled := Value;
end;

procedure TCurvyCombo.SetFocus;
begin
  FCombo.SetFocus;
end;

procedure TCurvyCombo.SetImeMode(const Value: TImeMode);
begin
  FCombo.ImeMode := Value;
end;

procedure TCurvyCombo.SetImeName(const Value: TImeName);
begin
  FCombo.ImeName := Value;
end;

procedure TCurvyCombo.SetItemIndex(const Value: integer);
begin
  FItemIndex := Value;
  if FCombo.HandleAllocated then
    FCombo.ItemIndex := Value;
end;

procedure TCurvyCombo.SetItems(const Value: TStrings);
begin
  FItems.Assign(Value);
end;

procedure TCurvyCombo.SetPopupMenu(const Value: TPopupMenu);
begin
  FCombo.PopupMenu := Value;
end;

procedure TCurvyCombo.SetSelLength(const Value: integer);
begin
  FCombo.SelLength := Value;
end;

procedure TCurvyCombo.SetSelStart(const Value: integer);
begin
  FCombo.SelStart := Value;
end;

procedure TCurvyCombo.SetSelText(const Value: string);
begin
  FCombo.SelText := Value;
end;

procedure TCurvyCombo.SetSorted(const Value: boolean);
begin
  FCombo.Sorted := Value;
end;

procedure TCurvyCombo.SetDropDownCount(const Value: integer);
begin
  FDropDownCount := Value;
  if Assigned(FCombo) then
    FCombo.DropDownCount := Value;
end;


procedure TCurvyCombo.SetStyle(const Value: TComboBoxStyle);
begin
  FCombo.Style := Value;
end;

procedure TCurvyCombo.SetTabStop(const Value: boolean);
begin
  inherited TabStop := Value;

  // setting TabStop at runtime
  if not (csReading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    FCombo.TabStop := Value;
    inherited TabStop := false;
  end;
end;

procedure TCurvyCombo.SetText(const Value: TCaption);
begin
  FCombo.Text := Value;
end;

{$IFDEF DELPHI_UNICODE}
procedure TCurvyCombo.SetTextHint(const Value: string);
begin
  FCombo.TextHint := Value;
end;
{$ENDIF}

procedure TCurvyCombo.UpdateCombo;
begin
  FCombo.Left := FRounding + FCurvyControls.LeftSize + FCurvyControls.LeftControlSize;
  FCombo.Width := Width - 2 * FRounding - (FCurvyControls.RightSize + FCurvyControls.RightControlSize +
     FCurvyControls.LeftSize + FCurvyControls.LeftControlSize);
end;

procedure TCurvyCombo.UpdateControl;
begin
  FCurvyControls.GetSizes;
  IndentLeft := FCurvyControls.LeftSize;
  IndentRight := FCurvyControls.RightSize;
  UpdateCombo;
  Invalidate;
end;

procedure TCurvyCombo.UpdateEditControl;
begin
  FCombo.Left := FRounding + FCurvyControls.LeftSize + FCurvyControls.LeftControlSize;
  FCombo.Width := Width - 2 * FRounding - (FCurvyControls.RightSize + FCurvyControls.RightControlSize +
     FCurvyControls.LeftSize + FCurvyControls.LeftControlSize);
end;

procedure TCurvyCombo.WMSetFocus(var Message: TMessage);
begin
  FCombo.SetFocus;
  Message.Result := 1;
end;

{ TCurvyControls }

function TCurvyControls.Add: TCurvyControl;
begin
  Result := TCurvyControl(inherited Add);
end;

constructor TCurvyControls.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TCurvyControl);
end;

destructor TCurvyControls.Destroy;
begin
  inherited Destroy;
end;

function TCurvyControls.GetControlSize(Index: Integer): TPoint;
var
  dx: Integer;
begin
  Result := Point(0,0);

  dx := 0;
  if Items[Index].DropDown then
    dx := 5;

  if Assigned(Items[index].Picture) and (not Items[index].Picture.Empty) then
  begin
    Items[index].Picture.GetImageSizes;
    Result := Point(Items[index].Picture.Width + CONTROL_SPACING + dx, Items[index].Picture.Height)
  end
  else
    //if Items[index].ImageIndex >= 0 then
      Result := Point(ImageListImageWidth + CONTROL_SPACING + dx, ImageListImageHeight);
end;

function TCurvyControls.GetItem(Index: Integer): TCurvyControl;
begin
  Result := TCurvyControl(inherited Items[Index]);
end;

procedure TCurvyControls.GetSizes;
var
  i: integer;
  pt: TPoint;
begin
  FLeftSize := 0;
  FRightSize := 0;
  FLeftControlSize := 0;
  FRightControlSize := 2;

  for i := 0 to Count - 1 do
  begin
    pt := GetControlSize(i);
    case Items[i].Position of
    cpLeftInControl: FLeftControlSize := FLeftControlSize + pt.X;
    cpLeft: FLeftSize := FLeftSize + pt.X;
    cpRightInControl: FRightControlSize := FRightControlSize + pt.X;
    cpRight: FRightSize := FRightSize + pt.X;
    end;
  end;
end;

function TCurvyControls.Insert(Index: Integer): TCurvyControl;
begin
  Result := TCurvyControl(inherited Insert(Index));
end;

procedure TCurvyControls.SetItem(Index: Integer; const Value: TCurvyControl);
begin
  inherited Items[Index] := Value;
end;

procedure TCurvyControls.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

{ TCurvyControl }

procedure TCurvyControl.Assign(Source: TPersistent);
begin
  if (Source is TCurvyControl) then
  begin
    //FCaption := (Source as TCurvyControl).Caption;
    FHint := (Source as TCurvyControl).Hint;
    FImageIndex := (Source as TCurvyControl).ImageIndex;
    FImageIndexDown := (Source as TCurvyControl).ImageIndexDown;
    FImageIndexHover := (Source as TCurvyControl).ImageIndexHover;
    FPosition := (Source as TCurvyControl).Position;
    FDropDown := (Source as TCurvyControl).DropDown;
  end;
end;

procedure TCurvyControl.Changed;
begin
  Collection.BeginUpdate;
  Collection.EndUpdate;
end;

constructor TCurvyControl.Create(Collection: TCollection);
begin
  inherited;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FPictureDown := TAdvGDIPPicture.Create;
  FPictureDown.OnChange := PictureChanged;
  FPictureHover := TAdvGDIPPicture.Create;
  FPictureHover.OnChange := PictureChanged;

  FImageIndex := -1;
  FImageIndexDown := -1;
  FImageIndexHover := -1;
  FPosition := cpRightInControl;
  FDropDown := false;
end;

destructor TCurvyControl.Destroy;
begin
  FPicture.Free;
  FPictureDown.Free;
  FPictureHover.Free;
  inherited;
end;

function TCurvyControl.GetImageIndex: integer;
begin
  Result := FImageIndex;
end;

function TCurvyControl.GetImageIndexDown: integer;
begin
  Result := FImageIndexDown;
end;

function TCurvyControl.GetImageIndexHover: integer;
begin
  Result := FImageIndexHover;
end;

function TCurvyControl.GetPicture: TAdvGDIPPicture;
begin
  Result := FPicture;
end;

function TCurvyControl.GetPictureHover: TAdvGDIPPicture;
begin
  Result := FPictureHover;
end;

function TCurvyControl.GetPicureDown: TAdvGDIPPicture;
begin
  Result := FPictureDown;
end;

procedure TCurvyControl.PictureChanged(Sender: TObject);
begin
  Changed;
end;

//procedure TCurvyControl.SetCaption(const Value: string);
//begin
//  FCaption := Value;
//end;

procedure TCurvyControl.SetDropDown(const Value: boolean);
begin
  if (FDropDown <> Value) then
  begin
    FDropDown := Value;
    Changed;
  end;
end;

procedure TCurvyControl.SetImageIndex(const Value: integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TCurvyControl.SetImageIndexDown(const Value: integer);
begin
  if (FImageIndexDown <> Value) then
  begin
    FImageIndexDown := Value;
  end;
end;

procedure TCurvyControl.SetImageIndexHover(const Value: integer);
begin
  if (FImageIndexHover <> Value) then
  begin
    FImageIndexHover := Value;
  end;
end;

procedure TCurvyControl.SetPicture(const Value: TAdvGDIPPicture);
begin
  FPicture.Assign(Value);
end;

procedure TCurvyControl.SetPictureDown(const Value: TAdvGDIPPicture);
begin
  FPictureDown.Assign(Value);
end;

procedure TCurvyControl.SetPictureHover(const Value: TAdvGDIPPicture);
begin
  FPictureHover.Assign(Value);
end;

procedure TCurvyControl.SetPosition(const Value: TControlPosition);
begin
  if (FPosition <> Value) then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TCurvyControl.SetState(const Value: TControlState);
begin
  FState := Value;
end;



{ TCurvyControlPanel }

procedure TCurvyControlPanel.CMHintShow(var Msg: TCMHintShow);
var
  i: integer;
begin
  inherited;

  i := ControlAtXY(Msg.HintInfo^.CursorPos.X,Msg.HintInfo^.CursorPos.Y);

  if (i <> -1) then
    if (FCurvyControls.Items[i].Hint <> '') then
      Msg.HintInfo^.HintStr := FCurvyControls.Items[i].Hint;
end;

procedure TCurvyControlPanel.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

procedure TCurvyControlPanel.CMMouseLeave(var Msg: TMessage);
var
  i: integer;
  flg: boolean;

begin
  inherited;

  flg := false;
  for i := 0 to FCurvyControls.Count - 1 do
  begin
    if FCurvyControls.Items[i].State = csHover then
    begin
      FCurvyControls.Items[i].State := csNormal;
      flg := true;
    end;
  end;

  if flg then
    Invalidate;
end;

function TCurvyControlPanel.ControlAtXY(x, y: integer): integer;
var
  cx: integer;
begin
  Result := HandleControls(x,y,cx,false);
end;

procedure TCurvyControlPanel.ControlsChanged(Sender: TObject);
begin
  UpdateControl;
end;

constructor TCurvyControlPanel.Create(AOwner: TComponent);
begin
  inherited;
  FCurvyControls := TCurvyControls.Create(Self);
  FCurvyControls.OnChange := ControlsChanged;
end;

destructor TCurvyControlPanel.Destroy;
begin
  FCurvyControls.Free;
  inherited;
end;

procedure TCurvyControlPanel.DoControlClick(index: integer);
begin
  if Assigned(OnControlClick) then
    OnControlClick(Self, index);
end;

function TCurvyControlPanel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' +
    IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TCurvyControlPanel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TCurvyControlPanel.HandleControls(x, y: integer; var ctrlx: integer;
  dopaint: boolean): integer;
var
  i: integer;
  dl,dlc,dr,drc,t: integer;
  pt: TPoint;
begin
  Result := -1;
  dl := 0;
  dlc := FCurvyControls.LeftSize + FRounding div 2;
  dr := Width - FCurvyControls.RightSize + 2;
  drc := Width - FCurvyControls.RightSize - FCurvyControls.RightControlSize  - (FRounding div 2);

  for i := 0 to FCurvyControls.Count - 1 do
  begin
    pt := FCurvyControls.GetControlSize(i);

    t := (Height - pt.Y) div 2;

    case FCurvyControls[i].Position of
    cpLeftInControl:
      begin
        if dopaint then
          PaintControl(dlc,t,i);

        if (x > dlc) and (x < dlc + pt.X) then
        begin
          ctrlx := dlc;
          Result := i;
        end;

        dlc := dlc + pt.X;
      end;
    cpLeft:
      begin
        if dopaint then
          PaintControl(dl,t,i);

        if (x > dl) and (x < dl + pt.X) then
        begin
          ctrlx := dl;
          Result := i;
        end;

        dl := dl + pt.X;
      end;
    cpRightInControl:
      begin
        if dopaint then
          PaintControl(drc,t,i);

        if (x > drc) and (x < drc + pt.X) then
        begin
          ctrlx := drc;
          Result := i;
        end;

        drc := drc + pt.X;
      end;
    cpRight:
      begin
        if dopaint then
          PaintControl(dr,t,i);

        if (x > dr) and (x < dr + pt.X) then
        begin
          ctrlx := dr;
          Result := i;
        end;

        dr := dr + pt.X;
      end;
    end;
  end;
end;

procedure TCurvyControlPanel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i,cx: integer;
  pt : TPoint;
begin
  inherited;

  i := Handlecontrols(X,Y,cx, false);

  if (i <> -1) then
  begin
    FCurvyControls.Items[i].State := csDown;
    Invalidate;

    if FCurvyControls.Items[i].DropDown and Assigned(FCurvyControls.Items[i].PopupMenu) then
    begin
      pt := Point(cx, Height - 3);
      pt := ClientToScreen(pt);
      FCurvyControls.Items[i].PopupMenu.Popup(pt.X,pt.Y);
    end;
  end;
end;

procedure TCurvyControlPanel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i,j: integer;
  flg: boolean;
begin
  inherited;
  //
  i := ControlAtXY(X,Y);

  if (i <> -1) then
  begin
    flg := false;

    for j := 0 to FCurvyControls.Count - 1 do
    begin
      if (FCurvyControls.Items[j].State = csHover) and (j <> i) then
      begin
        FCurvyControls.Items[j].State := csNormal;
        flg := true;
      end;

      if (FCurvyControls.Items[j].State = csNormal) and (j = i) then
      begin
        FCurvyControls.Items[j].State := csHover;
        flg := true;
      end;
    end;

    if flg then
      Invalidate;
  end;
end;

procedure TCurvyControlPanel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  i: integer;
begin
  inherited;

  i := ControlAtXY(X,Y);

  if (i <> -1) then
  begin
    DoControlClick(i);

    FCurvyControls.Items[i].State := csNormal;
    Invalidate;
  end;
end;

procedure TCurvyControlPanel.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: integer;
begin
  if (AOperation = opRemove) and (AComponent = FImages) then
  begin
    Images := nil;
  end;

  if not (csDestroying in ComponentState) and Assigned(FCurvyControls) then
  begin
    for i := 0 to FCurvyControls.Count - 1 do
    begin
      if FCurvyControls.Items[i].PopupMenu = AComponent then
        FCurvyControls.Items[i].PopupMenu := nil;
    end;
  end;

  inherited;
end;

procedure TCurvyControlPanel.Paint;
var
  cx: integer;
begin
  inherited;
  HandleControls(-1,-1,cx,true);
end;

procedure TCurvyControlPanel.PaintControl(x, y, index: integer);
var
  g: TGPGraphics;
  p: TGPGraphicsPath;
  b: TGPSolidBrush;
  pts: array[0..2] of TGPPointF;
  idx,w: integer;
  pic: TAdvGDIPPicture;
begin
  w := 0;

  if not FCurvyControls[index].Picture.Empty then
  begin
    pic := FCurvyControls[index].Picture;

    case FCurvyControls[index].State of
    csDown: if not FCurvyControls[index].PictureDown.Empty then
      pic := FCurvyControls[index].PictureDown;
    csHover: if not FCurvyControls[index].PictureHover.Empty then
      pic := FCurvyControls[index].PictureHover;
    end;

    g := TGPGraphics.Create(Canvas.Handle);
    pic.GetImageSizes;
    pic.GDIPDraw(g, MakeRect(x,y,pic.Width, pic.Height));
    w := pic.Width;
    g.Free;
  end
  else
  begin
    idx := FCurvyControls[index].ImageIndex;

    case FCurvyControls[index].State of
    csDown: if (FCurvyControls[index].ImageIndexDown >= 0) then
      idx := FCurvyControls[index].ImageIndexDown;
    csHover: if (FCurvyControls[index].ImageIndexHover >= 0) then
      idx := FCurvyControls[index].ImageIndexHover;
    end;

    if Assigned(Images) then
    begin
      w := Images.Width;
      if (idx >= 0) then
        Images.Draw(Canvas, x, y, idx, true);
    end;
  end;

  if FCurvyControls[index].DropDown then
  begin
    g := TGPGraphics.Create(Canvas.Handle);

    g.SetSmoothingMode(SmoothingModeHighQuality);


    pts[0].X := x + w;
    pts[0].Y := y + 5;

    pts[1].X := x + w + 6;
    pts[1].Y := y + 5;

    pts[2].X := x + w + 3;
    pts[2].Y := y + 9;

    p := TGPGraphicsPath.Create;

    p.AddPolygon(PGPPointF(@pts), 3);

    b := TGPSolidBrush.Create(MakeColor(255,0,0,0));

    g.FillPath(b, p);

    b.Free;
    p.Free;

    g.Free;
  end;
end;

procedure TCurvyControlPanel.SetCurvyControls(const Value: TCurvyControls);
begin
  FCurvyControls.Assign(Value);
end;

procedure TCurvyControlPanel.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
  if Assigned(FImages) then
  begin
    FCurvyControls.ImageListImageWidth := Images.Width;
    FCurvyControls.ImageListImageHeight := Images.Height;
  end
  else
  begin
    FCurvyControls.ImageListImageWidth := 0;
    FCurvyControls.ImageListImageHeight := 0;
  end;

  UpdateControl;
end;

procedure TCurvyControlPanel.SetVersion(const Value: string);
begin

end;

procedure TCurvyControlPanel.UpdateControl;
begin
  FCurvyControls.GetSizes;
  IndentLeft := FCurvyControls.LeftSize;
  IndentRight := FCurvyControls.RightSize;
  UpdateEditControl;
  Invalidate;
end;

procedure TCurvyControlPanel.UpdateEditControl;
begin
  //
end;

{ TEmptyEdit }

procedure TEmptyEdit.SetEmptyText(const Value: string);
begin
  FEmptyText := Value;
end;

procedure TEmptyEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;
  if (EmptyText <> '') and (Text = '') then
    Invalidate;
end;

procedure TEmptyEdit.WMPaint(var Msg: TWMPaint);
var
  DC: HDC;
  Canvas: TCanvas;
begin
  inherited;

  if (Text = '') and (FEmptyText <> '') and ((GetFocus <> Handle) or (HideEmptyText = heOnText)) then
  begin
    DC := GetDC(Handle);
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    Canvas.Font.Assign(Font);
    Canvas.Font.Color := clGray;
    Canvas.TextOut(3, 0, FEmptyText);
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;
end;

procedure TEmptyEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  inherited;
  if (EmptyText <> '') and (Text = '') then
    Invalidate;
end;

{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.

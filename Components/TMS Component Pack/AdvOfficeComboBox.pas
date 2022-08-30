{***************************************************************************}
{ TAdvFontSelector components                                               }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2006 - 2015                                        }
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

unit AdvOfficeComboBox;

{$I TMSDEFS.INC}

interface

uses
  Windows, Messages, Types, StdCtrls, Classes, Graphics, Controls, SysUtils, Forms, Math,
  ComCtrls, AdvGDIP, GDIPicture, AdvGlowButton, AdvStyleIF, AdvHintInfo
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  LISTITEMHEIGHT = 15;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  //
  // v1.0.0.0 : first release
  // v1.0.0.1 : fixed issue with bold-only fonts in AdvOfficeFontSelector
  // v1.0.0.2 : exposed TabOrder in AdvOfficeSelector
  // v1.0.1.0 : New support for Office 2007 silver style added
  // v1.0.2.0 : Added property AutoResetFocus to TAdvCustomOfficeComboBox
  // v1.0.2.1 : Fixed issue with & display in items
  // v1.0.2.2 : Fixed issue with ItemIndex during OnChange event
  // v1.0.2.3 : Fixed issue with drawing
  // v1.0.2.4 : Fixed issue with bold-italic only fonts in AdvOfficeFontSelector
  // v1.0.3.0 : New : events OnEnter, OnExit exposed
  // v1.0.3.1 : Fixed : issue with csDropDown mode and text entry
  // v1.0.3.2 : Fixed issue with keyboard handling for csDropDownlist style
  // v1.0.4.0 : Improved : exposed Anchors property
  // v1.0.4.1 : Fixed issue with ItemIndex update from OnChange event
  // v1.0.4.2 : Improved : protection for GDI+ font creation issue
  // v1.0.5.0 : New : mousewheel support in TAdvOfficeComboBox & descendent components
  // v1.0.6.0 : New : exposed methods ShowDropDown, HideDropDown
  // v1.0.7.0 : New : combobox text vertically centered
  // v1.0.8.0 : Improved : optimization to reduce use of LoadFontNames in TAdvOfficeFontSelector
  // v1.1.0.0 : New : Terminal, Vista & Windows 7 styles
  // v1.2.0.0 : New : Built in support for Office 2010 colors
  // v1.2.1.0 : Improved : Display of dropdown on right side of screen
  // v1.2.1.1 : Improved : Behaviour with changing selected item with keyboard when the mouse hovers over the dropdown
  // v1.2.2.0 : New : Property DropDownColor added
  //          : Fixed : Rare issue when control is used on a TPageControl that up/down refuses to work
  // v1.2.3.0 : New : Exposed Enabled property in TAdvOfficeSelector
  // v1.2.3.1 : Improved : Dropdown alignment
  // v1.2.4.0 : New : OfficeHint support added
  // v1.2.4.1 : Fixed : Issue with border painting, painting under border
  //          : Fixed : Issue with handling Enter key to select on modal forms
  // v1.2.5.0 : New : DroppedDown property added
  // v1.2.5.1 : Fixed : Issue with up/down on form open
  // v1.2.5.2 : Fixed : Issue with lookup and special chars like ä,ü,...
  // v1.2.6.0 : New : Property EnableMouseWheel added
  // v1.2.7.0 : New : Windows 8, Office 2013 styles added
  // v1.2.7.1 : Fixed : Issue when changing Font.Size in TAdvOfficeSizeSelector
  // v1.2.7.2 : Fixed : Issue with accepting non numeric chars in TAdvOfficeSizeSelector
  // v1.2.8.0 : New : Added property FontSizeDigits to TAdvOfficeFontSizeSelector
  //          : Fixed : Issue with positioning on multimonitor config
  //          : Improved : Speed of loading fonts
  // v1.2.9.0 : New : OnCloseUp event added
  // v1.3.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvFontType = (aftBitmap, aftTrueType, aftPostScript, aftPrinter, aftFixedPitch, aftProportional);
  TAdvFontTypes = set of TAdvFontType;

  TWinCtrl = class(TWinControl);
  TAdvCustomOfficeComboBox = class;
  TAdvOfficeFontSizeSelector = class;

  TDropDownWindow = class(THintWindow)
  private
    FListControl: TListBox;
    FHideOnDeActivate: Boolean;
    FOnHide: TNotifyEvent;
    procedure WMNCButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    property HideOnDeActivate: Boolean read FHideOnDeActivate write FHideOnDeActivate;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property BorderWidth;
    property ListControl: TListBox read FListControl write FListControl;
    property OnHide: TNotifyEvent read FOnHide write FOnHide;
  end;

  TAdvComboBtn = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FWidth: integer;
    FGlyph: TBitMap;
    FGlyphDown: TBitMap;
    FGlyphHot: TBitMap;
    FColorHot: TColor;
    FColorTo: TColor;
    FColorDownTo: TColor;
    FColorDown: TColor;
    FColorHotTo: TColor;
    FColor: TColor;
    FDownArrowColor: TColor;
    FHotArrowColor: TColor;
    FArrowColor: TColor;
    FCombo: TAdvCustomOfficeComboBox;
    FDownArrow: Boolean;
    FColorMirror: TColor;
    FColorMirrorDownTo: TColor;
    FColorMirrorHot: TColor;
    FColorMirrorTo: TColor;
    FColorMirrorDown: TColor;
    FColorMirrorHotTo: TColor;
    FGradientMirrorDown: TGDIPGradient;
    FGradient: TGDIPGradient;
    FGradientMirrorHot: TGDIPGradient;
    FGradientHot: TGDIPGradient;
    FGradientDown: TGDIPGradient;
    FGradientMirror: TGDIPGradient;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FDisabledArrowColor: TColor;
    FColorMirrorDisabled: TColor;
    FColorMirrorDisabledTo: TColor;
    procedure SetWidth(const Value: integer);
    procedure Change;
    procedure SetColor(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetGlyphDown(const Value: TBitMap);
    procedure SetGlyph(const Value: TBitMap);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetGlyphHot(const Value: TBitMap);
    procedure SetArrowColor(const Value: TColor);
    procedure SetDownArrowColor(const Value: TColor);
    procedure SetHotArrowColor(const Value: TColor);
    procedure SetColorMirror(const Value: TColor);
    procedure SetColorMirrorTo(const Value: TColor);
    procedure SetGradient(const Value: TGDIPGradient);
    procedure SetGradientMirror(const Value: TGDIPGradient);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetDisabledArrowColor(const Value: TColor);
    procedure SetColorMirrorDisabled(const Value: TColor);
    procedure SetColorMirrorDisabledTo(const Value: TColor);
  protected
    property ArrowColor: TColor read FArrowColor write SetArrowColor;
    property HotArrowColor: TColor read FHotArrowColor write SetHotArrowColor;
    property DownArrowColor: TColor read FDownArrowColor write SetDownArrowColor;
    property DisabledArrowColor: TColor read FDisabledArrowColor write SetDisabledArrowColor;
    property DownArrow: Boolean read FDownArrow write FDownArrow;

    property Combo: TAdvCustomOfficeComboBox read FCombo write FCombo;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor;
    property ColorTo: TColor read FColorTo write SetColorTo;
    property ColorHot: TColor read FColorHot write SetColorHot;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo;
    property ColorDown: TColor read FColorDown write SetColorDown;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo;

    property ColorMirror: TColor read FColorMirror write SetColorMirror default clSilver;
    property ColorMirrorTo: TColor read FColorMirrorTo write SetColorMirrorTo default clWhite;
    property ColorMirrorHot: TColor read FColorMirrorHot write FColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write FColorMirrorHotTo;
    property ColorMirrorDown: TColor read FColorMirrorDown write FColorMirrorDown;
    property ColorMirrorDownTo: TColor read FColorMirrorDownTo write FColorMirrorDownTo;
    property ColorMirrorDisabled: TColor read FColorMirrorDisabled write SetColorMirrorDisabled default clSilver;
    property ColorMirrorDisabledTo: TColor read FColorMirrorDisabledTo write SetColorMirrorDisabledTo default clWhite;

    property Gradient: TGDIPGradient read FGradient write SetGradient default ggRadial;
    property GradientMirror: TGDIPGradient read FGradientMirror write SetGradientMirror default ggRadial;
    property GradientHot: TGDIPGradient read FGradientHot write FGradientHot default ggRadial;
    property GradientMirrorHot: TGDIPGradient read FGradientMirrorHot write FGradientMirrorHot default ggRadial;
    property GradientDown: TGDIPGradient read FGradientDown write FGradientDown default ggRadial;
    property GradientMirrorDown: TGDIPGradient read FGradientMirrorDown write FGradientMirrorDown default ggRadial;

    property Glyph: TBitMap read FGlyph write SetGlyph;
    property GlyphHot: TBitMap read FGlyphHot write SetGlyphHot;
    property GlyphDown: TBitMap read FGlyphDown write SetGlyphDown;
    property Width: integer read FWidth write SetWidth;
  end;

  TComboStyle = (csDropDown, csDropDownList);
  TSelectionGradient = (sgVertical, sgHorizontal, sgVerticalInOut, sgHorizontalInOut);

  TSelectFontNameEvent = procedure(Sender: TObject; AName: string) of object;
  TSelectFontSizeEvent = procedure(Sender: TObject; ASize: Integer) of object;

  TSelectionAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorMirror: TColor;
    FColorMirrorTo: TColor;
    FGradientMirror: TGDIPGradient;
    FGradient: TGDIPGradient;
    FTextColor: TColor;
    FRounded: Boolean;
    procedure SetTextColor(const Value: TColor);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write FBorderColor default clSilver;
    property Color: TColor read FColor write FColor default clWhite;
    property ColorTo: TColor read FColorTo write FColorTo default clWhite;
    property ColorMirror: TColor read FColorMirror write FColorMirror default clSilver;
    property ColorMirrorTo: TColor read FColorMirrorTo write FColorMirrorTo default clWhite;
    property Gradient: TGDIPGradient read FGradient write FGradient default ggVertical;
    property GradientMirror: TGDIPGradient read FGradientMirror write FGradientMirror default ggVertical;
    property TextColor: TColor read FTextColor write SetTextColor;
    property Rounded: Boolean read FRounded write FRounded;
  end;

  TOfficeListBox = class(TListBox)
  private
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
  end;


  TAdvOfficeComboBoxActionLink = class(TControlActionLink)
  protected
    FClient: TAdvCustomOfficeComboBox;
    procedure AssignClient(AClient: TObject); override;
  public
    procedure UpdateText(AValue: string); virtual;
  end;


  TAdvCustomOfficeComboBox = class(TCustomEdit, ITMSStyleEx)
  private
    FOldX,FOldY: integer;
    FAutoFocus: boolean;
    FAutoResetFocus: boolean;
    FMouseInControl: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FButtonDown: Boolean;
    FEnableMouseWheel: Boolean;
    //FFocusFontColor: TColor;
    FOnMouseLeave: TNotifyEvent;
    FButton: TAdvComboBtn;
    FFlat: Boolean;
    FBorderHotColor: TColor;
    FBorderColor: TColor;
    FOldCursor: TCursor;
    FDropDownList: TDropDownWindow;
    FDropDownListBox: TListBox;
    FItems: TStringList;
    FDropDownCount: integer;
    FDropDownWidth: integer;
    FOnBeforeDropDown: TNotifyEvent;
    FItemIndex: integer;
    FFontSize: integer;
    FItemHeight: integer;
    FLookUp: Boolean;
    FLookUpText: string;
    FLookupTime: DWORD;
    FChanged: Boolean;
    FMatchCase: Boolean;
    FWorkMode: Boolean;
    FDisplayRecentSelection: Boolean;
    FRecentSelection: TStringList;
    FStyle: TComboStyle;
    FAutoItemSize: boolean;
    FOnSelect: TNotifyEvent;
    FSelectionAppearance: TSelectionAppearance;
    FAntiAlias: TAntiAlias;
    FDropDownColor: TColor;
    FBorderDisabledColor: TColor;
    FOfficeHint: TAdvHintInfo;
    FOnCloseUp: TNotifyEvent;
//    FDisplayRecentSelection: Boolean;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CMCancelMode(var Message: TMessage); message CM_CANCELMODE;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;

    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit); message CM_EXIT;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure OnSelectionAppearanceChanged(Sender: TObject);
    procedure DrawButton(DC: HDC); overload;
    procedure DrawButton; overload;
    procedure DrawBorders(DC: HDC); overload;
    procedure DrawBorders; overload;
    function GetButtonRect: TRect;
    function GetMinHeight: Integer;
    procedure ResizeControl;
    procedure ButtonOnChange(Sender: TObject);
    procedure ButtonClick;
    procedure ListBoxKeyPress(Sender: TObject; var Key: Char);
    procedure ListBoxMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure ListBoxKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxOnEnter(Sender: TObject);
    procedure LookUpText;
    function GetVersionNr: Integer; virtual;
    procedure SetButton(const Value: TAdvComboBtn);
    procedure SetFlat(const Value: Boolean);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderHotColor(const Value: TColor);
    //procedure SetAppereanceStyle(const Value: TAdvComboStyle);
    procedure SetItems(const Value: TStringList);
    procedure SetItemIndex(const Value: integer);
    procedure SetFontSize(const Value: integer);
    procedure SetLookUp(const Value: Boolean);
    function GetItemIndex: integer;
    procedure SetDisplayRecentSelection(const Value: Boolean);
    function GetItemHeight: integer;
    procedure SetItemHeight(const Value: integer);
    procedure SetStyle(const Value: TComboStyle);
    procedure SetAutoItemSize(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetSelectionAppearance(const Value: TSelectionAppearance);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetDropDownColor(const Value: TColor);
    procedure SetBorderDisabledColor(const Value: TColor);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetDroppedDown(const Value: boolean);
  protected
    FUpdatingIndex: boolean;
    FDroppedDown: Boolean;
    FOldText: string;
    function GetListItemHeight: integer;
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ShowDropDownInt; virtual;
    procedure SetEditRect;
    procedure Loaded; override;
    procedure DoEnter; override;
    procedure CreateWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Message: TMessage); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): boolean; override;
    procedure DropWindowHide(Sender: TObject);

    procedure DoSelect(Index: Integer); virtual;

    procedure First;
    procedure Previous;
    procedure Next;
    procedure Last;
    procedure PageJump(GoForward: Boolean);

    procedure BeforeDropDown; virtual;
    procedure DropDownOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState); virtual;
    procedure ValueChanged; virtual;
    procedure PopulateListBox; virtual;
    property DropDownListBox: TListBox read FDropDownListBox;
    property Flat: Boolean read FFlat write SetFlat;
    property AutoItemSize: boolean read FAutoItemSize write SetAutoItemSize default true;
    property ItemHeight: integer read GetItemHeight write SetItemHeight;
    property FontSize: integer read FFontSize write SetFontSize;
    property Style: TComboStyle read FStyle write SetStyle default csDropDown;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    function CharFromPos(pt: TPoint): Integer;
    procedure Change; override;
    property ItemIndex: integer read GetItemIndex write SetItemIndex;
    property Items: TStringList read FItems write SetItems;
    property RecentSelection: TStringList read FRecentSelection;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetComponentStyleAndAppColor(AStyle: TTMSStyle; AppColor: TColor);
    property DroppedDown: boolean read FDroppedDown write SetDroppedDown;
    procedure ShowDropDown;
    procedure HideDropDown;
  published
    property Action;
    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property AutoFocus: boolean read FAutoFocus write fAutoFocus default false;
    property AutoResetFocus: boolean read FAutoResetFocus write fAutoResetFocus default false;
    property Button: TAdvComboBtn read FButton write SetButton;
    property DisplayRecentSelection: Boolean read FDisplayRecentSelection write SetDisplayRecentSelection default true;
    property DropDownCount: integer read FDropDownCount write FDropDownCount default 8;
    property DropDownWidth: integer read FDropDownWidth write FDropDownWidth default 0;
    property DropDownColor: TColor read FDropDownColor write SetDropDownColor default clWindow;
    property EnableMouseWheel: boolean read FEnableMouseWheel write FEnableMouseWheel default true;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property BorderHotColor: TColor read FBorderHotColor write SetBorderHotColor;
    property BorderDisabledColor: TColor read FBorderDisabledColor write SetBorderDisabledColor default clSilver;
    property LookUp: Boolean read FLookUp write SetLookUp default true;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property SelectionAppearance: TSelectionAppearance read FSelectionAppearance write SetSelectionAppearance;
    property Text;
    {$IFDEF DELPHI2010_LVL}
    property TextHint;
    {$ENDIF}
    property Version: string read GetVersion write SetVersion;
    property OnChange;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnCloseUp: TNotifyEvent read FOnCloseUp write FOnCloseUp;
    property OnSelect: TNotifyEvent read FOnSelect write FOnSelect;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeSelector = class(TAdvCustomOfficeComboBox)
  private
  protected
  public
    constructor Create(aOwner: TComponent); override;
  published
    property Anchors;
    property Color;
    property Enabled;
    property Items;
    property ItemIndex;
    property ItemHeight;
    property Text;
    property Font;
    property TabOrder;
    property TabStop;
    property ShowHint;
    property Style;
    property OnEnter;
    property OnExit;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeFontSelector = class(TAdvCustomOfficeComboBox)
  private
    FFontGlyphTT: TBitMap;
    FFontGlyphPS: TBitMap;
    FFontGlyphPRN: TBitMap;
    FFontGlyphBMP: TBitMap;
    FShowFontStyle: Boolean;
    FShowFontGlyph: Boolean;
    FAllowedFontTypes: TAdvFontTypes;
    FFontSizeSelector: TAdvOfficeFontSizeSelector;
    FOnSelectFontName: TSelectFontNameEvent;
    procedure SetFontGlyphTT(const Value: TBitMap);
    procedure SetFontGlyphPS(const Value: TBitMap);
    procedure SetFontGlyphPRN(const Value: TBitMap);
    procedure SetFontGlyphBMP(const Value: TBitMap);
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    function GetFontType(const Fontname: string): TAdvFontTypes;
    function GetSelectedFontName: string;
    procedure SetSelectedFontName(const Value: string);
    procedure SetAllowedFontTypes(const Value: TAdvFontTypes);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Loaded; override;
    procedure BeforeDropDown; override;
    procedure DropDownOnDrawItem(Control: TWinControl; Index: Integer; Rect: TRect; State: TOwnerDrawState); override;
  public
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure LoadFontNames;
    procedure Change; override;
    procedure DoSelect(Index: Integer); override;
    property SelectedFontName: string read GetSelectedFontName write SetSelectedFontName;
  published
    property AllowedFontTypes: TAdvFontTypes read FAllowedFontTypes write SetAllowedFontTypes;
    property FontGlyphTT: TBitMap read FFontGlyphTT write SetFontGlyphTT;
    property FontGlyphPS: TBitMap read FFontGlyphPS write SetFontGlyphPS;
    property FontGlyphPRN: TBitMap read FFontGlyphPRN write SetFontGlyphPRN;
    property FontGlyphBMP: TBitMap read FFontGlyphBMP write SetFontGlyphBMP;
    property FontSizeSelector: TAdvOfficeFontSizeSelector read FFontSizeSelector write FFontSizeSelector;
    property ShowFontStyle : Boolean read FShowFontStyle write FShowFontStyle default  True;
    property ShowFontGlyph: Boolean read FShowFontGlyph write FShowFontGlyph default True;
    property FontSize;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property Color;
    property Enabled;
    property Font;
    property ParentBiDiMode;
    property ImeMode;
    property ImeName;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    property OnSelectFontName: TSelectFontNameEvent read FOnSelectFontName write FOnSelectFontName;
  end;

  TFontSizeDigits = (fs2, fs3);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvOfficeFontSizeSelector = class(TAdvCustomOfficeComboBox)
  private
    PixelsPerInch: Integer;
    FFontName: TFontName;
    FOnSelectFontSize: TSelectFontSizeEvent;
    FFontSizeDigits: TFontSizeDigits;
    procedure SetFontName(const Value: TFontName);
    procedure Build;
    function GetSelectedFontSize: integer;
    procedure SetSelectedFontSize(const Value: integer);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
  public
    constructor Create(aOwner: TComponent); override;
    property FontName: TFontName read FFontName write SetFontName;
    procedure DoSelect(Index: Integer); override;
    property SelectedFontSize: integer read GetSelectedFontSize write SetSelectedFontSize;
    procedure KeyPress(var Ch: char); override;
  published
    property Color;
    property Enabled;
    property Font;
    property Anchors;
    property BiDiMode;
    property Constraints;
    property DisplayRecentSelection default false;
    property FontSizeDigits: TFontSizeDigits read FFontSizeDigits write FFontSizeDigits default fs2;

    property ImeMode;
    property ImeName;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnStartDrag;
    property OnEndDock;
    property OnStartDock;
    property OnSelectFontSize: TSelectFontSizeEvent read FOnSelectFontSize write FOnSelectFontSize;
  end;


procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
  GradientU,GradientB: TGDIPGradient; const Caption:string; AFont: TFont;
  Images: TImageList; ImageIndex: integer; EnabledImage: Boolean; Layout: TButtonLayout;
  DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
  Picture: TGDIPPicture; AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition); overload;

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
  GradientU,GradientB: TGDIPGradient; const Caption:string; AFont: TFont; Enabled: Boolean; Focus: Boolean;
  AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition = tpTop); overload;

function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; const Caption:string; AFont: TFont; Enabled:
  Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition): TRect;

implementation

{$R AdvOfficeComboBox.RES}

uses
  ComObj;

const
  // theme changed notifier
  WM_THEMECHANGED = $031A;

type
  XPColorScheme = (xpNone, xpBlue, xpGreen, xpGray);

var
  GetCurrentThemeName: function(pszThemeFileName: PWideChar;
    cchMaxNameChars: Integer;
    pszColorBuff: PWideChar;
    cchMaxColorChars: Integer;
    pszSizeBuff: PWideChar;
    cchMaxSizeChars: Integer): THandle cdecl stdcall;

  IsThemeActive: function: BOOL cdecl stdcall;


function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;
  hThemeLib: THandle;
begin
  hThemeLib := 0;
  Result := xpNone;

  try
    hThemeLib := LoadLibrary('uxtheme.dll');

    if hThemeLib > 0 then
    begin
      IsThemeActive := GetProcAddress(hThemeLib, 'IsThemeActive');

      if Assigned(IsThemeActive) then
        if IsThemeActive then
        begin
          GetCurrentThemeName := GetProcAddress(hThemeLib, 'GetCurrentThemeName');
          if Assigned(GetCurrentThemeName) then
          begin
            SetLength(FileName, 255);
            SetLength(ColorScheme, 255);
            SetLength(SizeName, 255);
            OleCheck(GetCurrentThemeName(PWideChar(FileName), 255,
              PWideChar(ColorScheme), 255, PWideChar(SizeName), 255));
            if (PWideChar(ColorScheme) = 'NormalColor') then
              Result := xpBlue
            else if (PWideChar(ColorScheme) = 'HomeStead') then
              Result := xpGreen
            else if (PWideChar(ColorScheme) = 'Metallic') then
              Result := xpGray
            else
              Result := xpNone;
          end;
        end;
    end;
  finally
    if hThemeLib <> 0 then
      FreeLibrary(hThemeLib);
  end;
end;


//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Steps = 0 then
    Steps := 1;

  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
        Rectangle(R.Left + stepw, R.Top, R.Left + stepw + Round(rstepw) + 1, R.Bottom)
      else
        Rectangle(R.Left, R.Top + stepw, R.Right, R.Top + stepw + Round(rstepw) + 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

function upstr(s: string; docase: boolean): string;
begin
  if docase then
    Result := s
  else
    Result := AnsiUpperCase(s);
end;

//------------------------------------------------------------------------------

function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := ColorToRGB(Col);
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,Brightness)
  else
    r1 := Round( Min(100,(100 + Brightness))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,Brightness)
  else
    g1 := Round( Min(100,(100 + Brightness))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,Brightness)
  else
    b1 := Round( Min(100,(100 + Brightness))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
var
  r1,g1,b1: Integer;
begin
  Col := Longint(ColorToRGB(Col));
  r1 := GetRValue(Col);
  g1 := GetGValue(Col);
  b1 := GetBValue(Col);

  if r1 = 0 then
    r1 := Max(0,BR)
  else
    r1 := Round( Min(100,(100 + BR))/100 * r1 );

  if g1 = 0 then
    g1 := Max(0,BG)
  else
    g1 := Round( Min(100,(100 + BG))/100 * g1 );

  if b1 = 0 then
    b1 := Max(0,BB)
  else
    b1 := Round( Min(100,(100 + BB))/100 * b1 );

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
var
  r1,g1,b1: Integer;
  r2,g2,b2: Integer;

begin
  if BlendFactor >= 100 then
  begin
    Result := Col1;
    Exit;
  end;
  if BlendFactor <= 0 then
  begin
    Result := Col2;
    Exit;
  end;

  Col1 := Longint(ColorToRGB(Col1));
  r1 := GetRValue(Col1);
  g1 := GetGValue(Col1);
  b1 := GetBValue(Col1);

  Col2 := Longint(ColorToRGB(Col2));
  r2 := GetRValue(Col2);
  g2 := GetGValue(Col2);
  b2 := GetBValue(Col2);

  r1 := Round( BlendFactor/100 * r1 + (1 - BlendFactor/100) * r2);
  g1 := Round( BlendFactor/100 * g1 + (1 - BlendFactor/100) * g2);
  b1 := Round( BlendFactor/100 * b1 + (1 - BlendFactor/100) * b2);

  Result := RGB(r1,g1,b1);
end;

//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

procedure DrawRect(graphics: TGPGraphics; Pen: TGPPen; X,Y,Width,Height: integer);
var
  path:TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  path.AddLine(X, Y, X + width, Y);
  path.AddLine(X + width, Y, X + width, Y + height);
  path.AddLine(X + width, Y + height, X, Y + height);
  path.AddLine(X, Y + height, X, Y);
  path.CloseFigure;
  graphics.DrawPath(pen, path);
  path.Free;
end;

//------------------------------------------------------------------------------

function DrawVistaText(Canvas: TCanvas; Alignment: TAlignment; r: TRect; const Caption:string; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias; Direction: TTabPosition): TRect;
var
  graphics : TGPGraphics;
  w,h: Integer;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  szRect: TRect;
  DTFLAG: DWORD;
  tm: TTextMetric;
  ttf: boolean;

begin
  if (Caption <> '') then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);

    if (Pos('@', AFont.Name) = 1) then
      fontFamily := TGPFontFamily.Create(Copy(AFont.Name,2,Length(AFont.Name) - 1))
    else
      fontFamily := TGPFontFamily.Create(AFont.Name);

    fs := 0;

    if (fsBold in AFont.Style) then
      fs := fs + 1;

    if (fsItalic in AFont.Style) then
      fs := fs + 2;

    if (fsUnderline in AFont.Style) then
      fs := fs + 4;

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    graphics.SetSmoothingMode(SmoothingModeAntiAlias);

    w := R.Right - R.Left;
    h := R.Bottom - R.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    Canvas.Font.Name := AFont.Name;

    ttf := false;

    GetTextMetrics(Canvas.Handle, tm);

    if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
    begin
      if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
      begin
        ttf := true;
      end
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := nil;
    if RealDraw then
    begin
      case (Direction) of
        tpTop, tpBottom: stringFormat := TGPStringFormat.Create;
        tpLeft:
        begin
          stringFormat := TGPStringFormat.Create; //($00000002);
        end;
        tpRight: stringFormat := TGPStringFormat.Create($00000002);
      end;
    end
    else
      stringFormat := TGPStringFormat.Create;


    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    case Alignment of
      taLeftJustify: stringFormat.SetAlignment(StringAlignmentNear);
      taCenter:
      begin
        // Center-justify each line of text.
        stringFormat.SetAlignment(StringAlignmentCenter);
      end;
      taRightJustify: stringFormat.SetAlignment(StringAlignmentFar);
    end;

    // Center the block of text (top to bottom) in the rectangle.
    stringFormat.SetLineAlignment(StringAlignmentCenter);
    stringFormat.SetHotkeyPrefix(HotkeyPrefixNone);

    //graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    //graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    if (AntiAlias = aaNone) or not ttf or (font.Status <> Ok) then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;
      szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_SINGLELINE or DT_NOPREFIX);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);

    Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
      if (AntiAlias = aaNone) or not ttf or (font.Status <> Ok) then
      begin
        szRect.Left := round(rectf.X);
        szRect.Top := round(rectf.Y);
        szRect.Right := szRect.Left + round(rectf.Width);
        szRect.Bottom := szRect.Top + round(rectf.Height);
        Canvas.Brush.Style := bsClear;

        DTFLAG := DT_LEFT;
        case Alignment of
        taRightJustify: DTFLAG := DT_RIGHT;
        taCenter: DTFLAG := DT_CENTER;
        end;
        DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX)
      end
      else
        graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
    end;

    stringformat.Free;
    solidBrush.Free;
    font.Free;
    fontfamily.Free;
    graphics.Free;
  end;
end;

//------------------------------------------------------------------------------

procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; const Caption:string; AFont: TFont;
   Images: TImageList; ImageIndex: integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; DrawDwLine: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition); overload;
var
  graphics : TGPGraphics;
  path: TGPGraphicsPath;
  pthGrBrush: TGPPathGradientBrush;
  linGrBrush: TGPLinearGradientBrush;
  gppen : tgppen;
  count: Integer;
  w,h,h2,w2: Integer;
  colors : array[0..0] of TGPColor;
  fontFamily: TGPFontFamily;
  font: TGPFont;
  rectf: TGPRectF;
  stringFormat: TGPStringFormat;
  solidBrush: TGPSolidBrush;
  x1,y1,x2,y2: single;
  fs: integer;
  sizerect: TGPRectF;
  ImgX, ImgY, ImgW, ImgH: Integer;
  BtnR, DwR: TRect;
  AP: TPoint;
  szRect: TRect;
  ttf: boolean;
  tm: TTextMetric;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;

begin
  BtnR := R;
  if DropDownPos = dpRight then
  begin
    DwR := Rect(BtnR.Right - DropDownSectWidth, BtnR.Top, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Right := DwR.Left;
  end
  else // DropDownPos = doBottom
  begin
    DwR := Rect(BtnR.Left, BtnR.Bottom - DropDownSectWidth, BtnR.Right, BtnR.Bottom);
    if DropDownButton then
      BtnR.Bottom := DwR.Top;
  end;

  w := r.Right - r.Left;
  h := r.Bottom - r.Top;

  h2 := h div 2;
  w2 := w div 2;

  graphics := TGPGraphics.Create(Canvas.Handle);

  case (Direction) of
    tpTop:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom ));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left, r.Top +  h2, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h2),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2, w - 1, h2+1);
        pthGrBrush.Free;
      end
      else
      begin
        if not RotateLeftRight then
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1)
        else
          graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 + 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      //Canvas.FillRect(rect(r.Left + 1, r.Top + 2, r.Right - 1, r.top +  h2));
      Canvas.FillRect(rect(r.Left , r.Top , r.Right , r.top +  h2));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left, r.Top - h2 , w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2+1),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.top));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w - 1, h - h2 - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2);
        linGrBrush.Free;
      end;

      path.Free;

    end;
    tpBottom:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top, r.Right , r.top +  h2));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left, r.Top, w , h2);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h2),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Top));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top, w - 1, h2+1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w - 1, h2 + 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      Canvas.FillRect(rect(r.Left , r.top +  h2, r.Right , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left, r.Bottom - h2 , w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2-1,w,h2),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeVertical);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top + h2,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left + w2, r.Bottom));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + h2 + 1, w - 1, h2 - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + h2, w - 1, h2 - 1);
        linGrBrush.Free;
      end;

      path.Free;
    end;
    tpLeft:
    begin
      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left + w2, r.top, r.Right , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left + w2, r.Top, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left + w2,r.Top,w2,h),ColorToARGB(CFB),ColorToARGB(CTB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.Top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left + w2, r.Top, w2 + 1, h-1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + w2 + 1,r.Top, w2 + 1, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

      Canvas.Brush.Color := cfu;
      //Canvas.FillRect(rect(r.Left + 1, r.Top + 2, r.Right - 1, r.top +  h2));
      Canvas.FillRect(rect(r.Left , r.Top , r.Left + w2 , r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Left - w2, r.Top, w , h);

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(CFU),ColorToARGB(CTU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left + 1,r.Top + 1, w2 - 1, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

    end;
    tpRight:
    begin
      Canvas.Brush.Color := cfu;
      Canvas.FillRect(rect(r.Right - w2 , r.Top , r.Right ,r.Bottom));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      path.AddEllipse(r.Right - w2, r.Top, w, h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientU of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);                      // FF: Gradient fix here replace h by h2
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w2,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Right-w2,r.Top,w,h),ColorToARGB(CTU),ColorToARGB(CFU), LinearGradientModeBackwardDiagonal);
      end;

      if GradientU = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Right, r.top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTU));

        colors[0] := ColorToARGB(CFU);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);

        graphics.FillRectangle(pthGrBrush, r.Right - w2 + 1,r.Top + 1, w2 - 1, h - 1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Right - w2, r.Top + 1, w2, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

      // down ellips brush
      Canvas.Brush.Color := cfb;
      Canvas.FillRect(rect(r.Left , r.top, r.Left + w2, r.Bottom ));

      // Create a path that consists of a single ellipse.
      path := TGPGraphicsPath.Create;
      //  path.AddRectangle(MakeRect(r.Left, r.Top +  (h div 2), w , h));
      path.AddEllipse(r.Left - w2, r.Top, w , h);

      pthGrBrush := nil;
      linGrBrush := nil;

      case GradientB of
      ggRadial: pthGrBrush := TGPPathGradientBrush.Create(path);
      ggVertical: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2+2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeHorizontal);
      ggDiagonalForward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeForwardDiagonal);
      ggDiagonalBackward: linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w2,h),ColorToARGB(CTB),ColorToARGB(CFB), LinearGradientModeBackwardDiagonal);
      end;

      if GradientB = ggRadial then
      begin
        pthGrBrush.SetCenterPoint(MakePoint(r.Left, r.Top + h2));

        // Set the color at the center point to blue.
        pthGrBrush.SetCenterColor(ColorToARGB(CTB));

        colors[0] := ColorToARGB(CFB);
        count := 1;
        pthGrBrush.SetSurroundColors(@colors, count);
        graphics.FillRectangle(pthGrBrush, r.Left,r.Top, w2 + 1, h-1);
        pthGrBrush.Free;
      end
      else
      begin
        graphics.FillRectangle(linGrBrush, r.Left,r.Top, w2 + 2, h - 1);
        linGrBrush.Free;
      end;

      path.Free;

    end;
  end;


  gppen := tgppen.Create(ColorToARGB(PC),1);

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if (PC <> clNone) then
  begin
    if not RoundEdges then
      DrawRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1)
    else
      DrawRoundRect(graphics, gppen,r.Left,r.Top, w - 1, h - 1, 3);
  end;

  gppen.Free;

  if Focus then
  begin
    gppen := tgppen.Create(ColorToARGB($E4AD89),1);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 1,r.Top + 1, r.Right - 3, r.Bottom - 3, 3);
    gppen.Free;
    gppen := tgppen.Create(ColorToARGB(clgray),1);
    gppen.SetDashStyle(DashStyleDot);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawRoundRect(graphics, gppen,r.Left + 2,r.Top + 2, r.Right - 5, r.Bottom - 5, 3);
    gppen.Free;
  end;

  Canvas.Font.Name := AFont.Name;

  ttf := false;

  GetTextMetrics(Canvas.Handle, tm);

  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      ttf := true;
    end
  end;

  if Screen.Fonts.IndexOf(AFont.Name) = -1 then
    ttf := false;

  fontFamily:= TGPFontFamily.Create(AFont.Name);

  fs := 0;

  ImgH := 0;
  ImgW := 0;

  if (fsBold in AFont.Style) then
    fs := fs + 1;
  if (fsItalic in AFont.Style) then
    fs := fs + 2;
  if (fsUnderline in AFont.Style) then
    fs := fs + 4;

  if Assigned(Picture) and not Picture.Empty then
  begin
    Picture.GetImageSizes;
    ImgW := Picture.Width;
    ImgH := Picture.Height;
  end
  else
  begin
    if (ImageIndex > -1) and Assigned(Images) then
    begin
      ImgW := Images.Width;
      ImgH := Images.Height;
    end;
  end;

  if (Caption <> '') then
  begin
    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    w := BtnR.Right - BtnR.Left;
    h := BtnR.Bottom - BtnR.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    rectf := MakeRect(x1,y1,x2,y2);

    stringFormat := TGPStringFormat.Create;

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    // Center-justify each line of text.
    stringFormat.SetAlignment(StringAlignmentCenter);

    // Center the block of text (top to bottom) in the rectangle.
    stringFormat.SetLineAlignment(StringAlignmentCenter);

    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);

    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    //graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);
    if (AntiAlias = aaNone) or not ttf then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;
      szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK or DT_NOPREFIX);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
      graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect);

    if (ImgW > 0) then
    begin
      case Layout of
        blGlyphLeft:
        begin
          x1 := r.Left + 2 + ImgW;
          x2 := w - 2 - ImgW;

          ImgX := round(sizerect.X - ImgW div 2);
          if ImgX < 2 then ImgX := 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphTop:
        begin
          y1 := r.Top{ + 2} + ImgH;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(y1) - ImgH + ImgY; //round(sizerect.Height) - ImgY - 4;
          if ImgY < 2 then ImgY := 2;
        end;
        blGlyphRight:
        begin
          x1 := 2;
          x2 := w - 4 - ImgW;

          ImgX := round(X2 - sizerect.width);
          ImgX := Max(0, ImgX div 2);
          ImgX := ImgX + round(sizerect.width) + 4;
          if ImgX > (w - ImgW) then
            ImgX := w - ImgW - 2;
          ImgY := r.Top + Max(0, (h - ImgH) div 2);
        end;
        blGlyphBottom:
        begin
          y1 := 2;
          y2 := h - 2 - ImgH;

          ImgX := r.Left + Max(0, (w - ImgW) div 2);
          ImgY := round(y2 - sizerect.Height);
          ImgY := Max(0, ImgY div 2);
          ImgY := round(sizerect.Height + 2) + ImgY;
          if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
        end;
      end;
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    //graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);
    if (AntiAlias = aaNone) or not ttf then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);
      szRect.Right := szRect.Left + round(rectf.Width);
      szRect.Bottom := szRect.Top + round(rectf.Height);
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CENTER or DT_VCENTER or DT_SINGLELINE or DT_NOPREFIX)
    end
    else
      graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush);

    stringformat.Free;
    font.Free;
  end;

  fontfamily.Free;

  if DropDownButton then
  begin

    if DropDownPos = dpRight then
      w := w - 8
    else
      h := h - 8;
  end;

  if Assigned(Picture) and not Picture.Empty then
  begin
     if Caption = '' then
       Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture)
     else
       Canvas.Draw(ImgX, ImgY, Picture);
  end
  else
    if (ImageIndex <> -1) and Assigned(Images) then
    begin
      if Caption = '' then
        Images.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, EnabledImage)
      else
      begin
        Images.Draw(Canvas, ImgX, ImgY, ImageIndex, EnabledImage);
      end;
    end;


  Canvas.Brush.Style := bsClear;
  if DropDownButton then
  begin
    if DrawDwLine then
    begin
      Canvas.Pen.Color := PC;
      //Canvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 6, 6);
      if (DropDownPos = dpRight) then
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Left, DwR.Bottom);
      end
      else
      begin
        Canvas.MoveTo(DwR.Left, DwR.Top);
        Canvas.LineTo(DwR.Right, DwR.Top);
      end;
    end;
    AP.X := DwR.Left + ((DwR.Right - DwR.Left - 5) div 2);
    AP.Y := DwR.Top + ((DwR.Bottom - DwR.Top - 3) div 2) + 1;
    if not Enabled then
      DrawArrow(AP, clGray)
    else
      DrawArrow(AP, clBlack);
  end;

  graphics.Free;
end;


procedure DrawVistaGradient(Canvas: TCanvas; r: TRect; CFU, CTU, CFB, CTB, PC: TColor;
   GradientU,GradientB: TGDIPGradient; const Caption:string; AFont: TFont; Enabled: Boolean; Focus: Boolean;
   AntiAlias: TAntiAlias; RoundEdges: Boolean; RotateLeftRight: Boolean; Direction: TTabPosition = tpTop); overload;
begin
  DrawVistaGradient(Canvas, r, CFU, CTU, CFB, CTB, PC, GradientU,GradientB, Caption, AFont,
   nil, -1, True, blGlyphLeft, False, False, Enabled, Focus, dpRight, nil, AntiAlias, RoundEdges, RotateLeftRight, Direction);
end;

//------------------------------------------------------------------------------

function IsTrueTypeFont(FontName : string):boolean;
const
  PITCH_MASK: byte = $0F;
var
  TxMet: TTextMetric;
  TempCanvas : TCanvas;
  PitchTest : byte;
begin
  TempCanvas := TCanvas.Create;
  TempCanvas.Handle := CreateCompatibleDC(0) ;
  TempCanvas.Font.Name := FontName;
  GetTextMetrics(TempCanvas.Handle, TxMet) ;
  PitchTest := TxMet.tmPitchAndFamily and PITCH_MASK;
  Result := (PitchTest and TMPF_TRUETYPE) <> 0;
  DeleteDC(TempCanvas.Handle);
  TempCanvas.Free;
end;

//------------------------------------------------------------------------------

{ TAdvCustomOfficeComboBox }

procedure TAdvCustomOfficeComboBox.ButtonOnChange(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.CharFromPos(pt: TPoint): Integer;
begin
  Result := Loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelparam(pt.x, pt.y)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMFontChanged(var Message: TMessage);
begin
  inherited;
  ResizeControl;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMMouseEnter(var Msg: TMessage);
var
  DC: HDC;
  pf: TCustomForm;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  pf := GetParentForm(self);

  if FAutoFocus then
  begin
    if Assigned(pf) then
    begin
      if (GetActiveWindow = pf.Handle) then
        SetFocus;
    end
    else
      SetFocus;
  end;
    
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    DC := GetDC(Handle);

    DrawButton(DC);
    DrawBorders(DC);

    ReleaseDC(Handle, DC);
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    Invalidate;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvCustomOfficeComboBox.Create(aOwner: TComponent);
begin
  inherited;
  FAutoFocus := false;
  FMouseInControl := false;
  //FFocusFontColor:= clWindowText;
  FBorderColor := clNone;
  FBorderHotColor := clNone;
  FDropDownColor := clWindow;
  FBorderDisabledColor := clSilver;
  FEnableMouseWheel := true;

  FOfficeHint := TAdvHintInfo.Create;

  FButtonDown := false;
  FButton := TAdvComboBtn.Create;
  FButton.OnChange := ButtonOnChange;

  BevelInner := bvNone;
  BevelOuter := bvNone;
  BorderStyle := bsNone;

  ControlStyle := ControlStyle - [csSetCaption];
  Text := '';
  FOldText := '';

  FItems := TStringList.Create;
  FItemIndex := -1;

  FAutoItemSize := true;

  FDroppedDown := false;
  FFlat := true;

  FFontSize := 8;

  FRecentSelection := TStringList.Create;
  FDisplayRecentSelection := true;

  FLookUp := true;
  FWorkMode := true;

  FStyle := csDropDown;

  FOldCursor := Cursor;

  FItemHeight := LISTITEMHEIGHT;

  if not (csDesigning in ComponentState) then
  begin
    FDropDownList := TDropDownWindow.Create(Self);
    FDropDownList.Visible := False;
    FDropDownList.Width := Self.Width;
    FDropDownList.Height := 20;
    FDropDownList.Parent := Self;
    FDropDownList.OnHide := DropWindowHide;
    if IsWinXP then
      FDropDownList.BorderWidth := 0
    else
      FDropDownList.BorderWidth := 1;
    FDropDownList.TabStop := false;
    FDropDownListBox := TOfficeListBox.Create(FDropDownList);

    with FDropDownListBox do
    begin
      DoubleBuffered := true;
      Parent := FDropDownList;
      Align := alClient;
      Style := lbOwnerDrawFixed;
      ItemHeight := LISTITEMHEIGHT;
      Ctl3D := false;
      TabStop := false;
      BorderStyle := bsNone;
      TabOrder := 0;
      OnKeyPress := ListBoxKeyPress;
      OnKeyDown := ListBoxKeyDown;
      OnMouseUp := ListBoxMouseUp;
      OnMouseMove := ListBoxMouseMove;
      OnEnter := ListBoxOnEnter;
      OnDrawItem := DropDownOnDrawItem;
      Color := FDropDownColor;
    end;
    FDropDownList.ListControl := FDropDownListBox;
  end;

  FDropDownCount := 8;

  FDropDownWidth := 0;

  FSelectionAppearance := TSelectionAppearance.Create;
  FSelectionAppearance.OnChange := OnSelectionAppearanceChanged;
  FAntiAlias := aaClearType;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomOfficeComboBox.Destroy;
begin
  FOfficeHint.Free;
  FButton.Free;
  FRecentSelection.Free;
  if not (csDesigning in ComponentState) then
  begin
    FDropDownListBox.Free;
    FDropDownList.Free;
  end;
  FItems.Free;
  FSelectionAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DoEnter;
begin
  inherited;
  SetEditRect;
end;

function TAdvCustomOfficeComboBox.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if EnableMouseWheel then
    Next;
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

function TAdvCustomOfficeComboBox.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): boolean;
begin
  if EnableMouseWheel then
    Previous;
  Result := inherited DoMouseWheelUp(Shift, MousePos);  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.Loaded;
begin
  inherited Loaded;
  SetEditRect;
  PopulateListBox;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetBorderDisabledColor(const Value: TColor);
begin
  if (FBorderDisabledColor <> Value) then
  begin
    FBorderDisabledColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetButton(const Value: TAdvComboBtn);
begin
  if Assigned(Value) then
    FButton.Assign(Value);
end;

procedure TAdvCustomOfficeComboBox.SetComponentStyle(AStyle: TTMSStyle);
begin
  SelectionAppearance.Rounded := False;

  if (Astyle in [tsOffice2003Blue, tsOffice2003Silver, tsOffice2003Olive, tsWhidbey]) then
  begin
      Button.ColorHot := $EBFDFF;
      Button.ColorHotTo := $ACECFF;
      Button.ColorMirrorHot := $ACECFF; //$59DAFF;
      Button.ColorMirrorHotTo := $A4E9FF;
      Button.GradientHot := ggVertical;
      Button.GradientMirrorHot := ggVertical;

      Button.ColorDown := $76AFF1;
      Button.ColorDownTo := $4190F3;
      Button.ColorMirrorDown := $4190F3; //$0E72F1;
      Button.ColorMirrorDownTo := $4C9FFD;
      Button.GradientDown := ggVertical;
      Button.GradientMirrorDown := ggVertical;

      SelectionAppearance.Color := $ACECFF;
      SelectionAppearance.ColorTo := $ACECFF;
      SelectionAppearance.ColorMirror := $ACECFF;
      SelectionAppearance.ColorMirrorTo := $ACECFF;
      SelectionAppearance.TextColor := clBlack;
      SelectionAppearance.BorderColor := $76AFF1;

      BorderColor := $00E0B99B;
      BorderHotColor := $0099CEDB;
  end;

  case AStyle of
    tsOffice2003Blue:
      begin
        Button.Color := $EEDBC8;
        Button.ColorTo := $F6DDC9;
        Button.ColorMirror := $F6DDC9; //$EDD4C0;
        Button.ColorMirrorTo := $F7E1D0;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;
      end;
    tsOffice2003Silver:
      begin
        Button.Color := $E6D8D8;
        Button.ColorTo := $EDD4C0;
        Button.ColorMirror := $EDD4C0;
        Button.ColorMirrorTo := $C8B2B3;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;
      end;
    tsOffice2003Olive:
      begin
        Button.Color := $CFF0EA;
        Button.ColorTo := $CFF0EA;
        Button.ColorMirror := $CFF0EA;
        Button.ColorMirrorTo := $8CC0B1;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;
      end;
    tsOffice2003Classic:
      begin
        Button.Color := clBtnFace;
        Button.ColorTo := clBtnFace;
        Button.ColorMirror := clBtnFace;
        Button.ColorMirrorTo := clBtnFace;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        BorderColor := clNone;
        BorderHotColor := $0099CEDB;

      end;
    tsOffice2007Luna:
      begin
        Button.Color := $EEDBC8;
        Button.ColorTo := $F6DDC9;
        Button.ColorMirror := $EDD4C0;
        Button.ColorMirrorTo := $F7E1D0;
        BorderColor := $E0B99B;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $EBFDFF;
        Button.ColorHotTo := $ACECFF;
        Button.ColorMirrorHot := $59DAFF;
        Button.ColorMirrorHotTo := $A4E9FF;
        BorderHotColor := $99CEDB;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $76AFF1;
        Button.ColorDownTo := $4190F3;
        Button.ColorMirrorDown := $0E72F1;
        Button.ColorMirrorDownTo := $4C9FFD;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $EBFDFF;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.Rounded := True;
      end;
    tsOffice2007Obsidian:
      begin
        SelectionAppearance.Color := $EBFDFF;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.Rounded := True;

        Button.Color := $DFDED6;
        Button.ColorTo := $E4E2DB;
        Button.ColorMirror := $D7D5CE;
        Button.ColorMirrorTo := $E7E5E0;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $EBFDFF;
        Button.ColorHotTo := $ACECFF;
        Button.ColorMirrorHot := $59DAFF;
        Button.ColorMirrorHotTo := $A4E9FF;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $76AFF1;
        Button.ColorDownTo := $4190F3;
        Button.ColorMirrorDown := $0E72F1;
        Button.ColorMirrorDownTo := $4C9FFD;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        BorderColor := $00C0BCB2;
      end;
    tsOffice2007Silver:
      begin
        Button.Color := $F3F3F1;
        Button.ColorTo := $F5F5F3;
        Button.ColorMirror := $EEEAE7;
        Button.ColorMirrorTo := $F8F7F6;
        BorderColor := $CCCAC9;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $EBFDFF;
        Button.ColorHotTo := $ACECFF;
        Button.ColorMirrorHot := $59DAFF;
        Button.ColorMirrorHotTo := $A4E9FF;
        BorderHotColor := $99CEDB;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $76AFF1;
        Button.ColorDownTo := $4190F3;
        Button.ColorMirrorDown := $0E72F1;
        Button.ColorMirrorDownTo := $4C9FFD;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $EBFDFF;
        SelectionAppearance.ColorTo := $ABEBFF;
        SelectionAppearance.ColorMirror := $69D6FF;
        SelectionAppearance.ColorMirrorTo := $96E4FF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.Rounded := True;
      end;
    tsWindowsXP:
      begin
        Button.Color := clWhite;
        Button.ColorTo := $B9D8DC;
        Button.ColorMirror := $B9D8DC;
        Button.ColorMirrorTo := $B9D8DC;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $EFD3C6;
        Button.ColorHotTo := $EFD3C6;
        Button.ColorMirrorHot := $EFD3C6;
        Button.ColorMirrorHotTo := $EFD3C6;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $B59284;
        Button.ColorDownTo := $B59284;
        Button.ColorMirrorDown := $B59284;
        Button.ColorMirrorDownTo := $B59284;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := clHighlight;
        SelectionAppearance.ColorTo := clHighlight;
        SelectionAppearance.ColorMirror := clHighlight;
        SelectionAppearance.ColorMirrorTo := clHighlight;
        SelectionAppearance.TextColor := clHighlightText;

        BorderColor := clNone;
        BorderHotColor := $00E0B99B;
      end;
      tsWindowsVista:
      begin
        Button.Color := RGB(255, 255, 255);
        Button.ColorTo := RGB(255, 255, 255);
        Button.ColorMirror := RGB(255, 255, 255);
        Button.ColorMirrorTo := RGB(255, 255, 255);
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $FFFDF9;
        Button.ColorHotTo := $FFFDF9;
        Button.ColorMirrorHot := $FFFAF0;
        Button.ColorMirrorHotTo := $FFFAF0;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $FEF9F0;
        Button.ColorDownTo := $FEF9F0;
        Button.ColorMirrorDown := $FDF0D7;
        Button.ColorMirrorDownTo := $FDF0D7;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $FDF7E0;
        SelectionAppearance.ColorTo := $FCF0D7;
        SelectionAppearance.ColorMirror := $FCF0D7;
        SelectionAppearance.ColorMirrorTo := $FCF0D7;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $FDDE99;

        BorderColor := RGB(151, 151, 151);
        BorderHotColor := $F1E2C7;
      end;
      tsWindows7:
      begin
        Button.Color := RGB(255, 255, 255);
        Button.ColorTo := RGB(255, 255, 255);
        Button.ColorMirror := RGB(255, 255, 255);
        Button.ColorMirrorTo := RGB(255, 255, 255);
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $FDFBFA;
        Button.ColorHotTo := $FDFBFA;
        Button.ColorMirrorHot := $FDF3EB;
        Button.ColorMirrorHotTo := $FDF3EB;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $FCEBDC;
        Button.ColorDownTo := $FCEBDC;
        Button.ColorMirrorDown := $FCDBC1;
        Button.ColorMirrorDownTo := $FCDBC1;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $FCEADA;
        SelectionAppearance.ColorTo := $FCDCC3;
        SelectionAppearance.ColorMirror := $FCDCC3;
        SelectionAppearance.ColorMirrorTo := $FCDCC3;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor:= $CEA27D;

        BorderColor := RGB(151, 151, 151);
        BorderHotColor := $F1E2C7;
      end;
      tsTerminal:
      begin
        Button.Color := clBtnFace;
        Button.ColorTo := clBtnFace;
        Button.ColorMirror := clBtnFace;
        Button.ColorMirrorTo := clBtnFace;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := clSilver;
        Button.ColorHotTo := clSilver;
        Button.ColorMirrorHot := clSilver;
        Button.ColorMirrorHotTo := clSilver;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := clHighLight;
        Button.ColorDownTo := clHighLight;
        Button.ColorMirrorDown := clHighLight;
        Button.ColorMirrorDownTo := clHighLight;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := clHighlight;
        SelectionAppearance.ColorTo := clHighlight;
        SelectionAppearance.ColorMirror := clHighlight;
        SelectionAppearance.ColorMirrorTo := clHighlight;
        SelectionAppearance.TextColor := clWhite;
        SelectionAppearance.BorderColor := clGray;

        BorderColor := clGray;
        BorderHotColor := clSilver;
      end;
      tsOffice2010Blue:
      begin
        Button.Color := $FDF6EF;
        Button.ColorTo := $F0DAC7;
        Button.ColorMirror := $F0DAC7;
        Button.ColorMirrorTo := $F0DAC7;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $D9F9FD;
        Button.ColorHotTo := $8AE3FD;
        Button.ColorMirrorHot := $8AE3FD;
        Button.ColorMirrorHotTo := $8AE3FD;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $7BEEFF;
        Button.ColorDownTo := $6CD0FF;
        Button.ColorMirrorDown := $6CD0FF;
        Button.ColorMirrorDownTo := $7BEEFF;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $7BEEFF;
        SelectionAppearance.ColorTo := $6CD0FF;
        SelectionAppearance.ColorMirror := $6CD0FF;
        SelectionAppearance.ColorMirrorTo := $7BEEFF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $308AC2;

        BorderColor := $C7B29F;
        BorderHotColor := $308AC2;
      end;
      tsOffice2010Silver:
      begin
        Button.Color := $FFFFFF;
        Button.ColorTo := $EDE5E0;
        Button.ColorMirror := $EDE5E0;
        Button.ColorMirrorTo := $EDE5E0;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $D9F9FD;
        Button.ColorHotTo := $8AE3FD;
        Button.ColorMirrorHot := $8AE3FD;
        Button.ColorMirrorHotTo := $8AE3FD;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $7BEEFF;
        Button.ColorDownTo := $6CD0FF;
        Button.ColorMirrorDown := $6CD0FF;
        Button.ColorMirrorDownTo := $7BEEFF;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $7BEEFF;
        SelectionAppearance.ColorTo := $6CD0FF;
        SelectionAppearance.ColorMirror := $6CD0FF;
        SelectionAppearance.ColorMirrorTo := $7BEEFF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $308AC2;

        BorderColor := $D2CDC8;
        BorderHotColor := $308AC2;
      end;
      tsOffice2010Black:
      begin
        Button.Color := $BFBFBF;
        Button.ColorTo := $919191;
        Button.ColorMirror := $919191;
        Button.ColorMirrorTo := $919191;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $D9F9FD;
        Button.ColorHotTo := $8AE3FD;
        Button.ColorMirrorHot := $8AE3FD;
        Button.ColorMirrorHotTo := $8AE3FD;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $7BEEFF;
        Button.ColorDownTo := $6CD0FF;
        Button.ColorMirrorDown := $6CD0FF;
        Button.ColorMirrorDownTo := $7BEEFF;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $7BEEFF;
        SelectionAppearance.ColorTo := $6CD0FF;
        SelectionAppearance.ColorMirror := $6CD0FF;
        SelectionAppearance.ColorMirrorTo := $7BEEFF;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $308AC2;

        BorderColor := $6D6D6D;
        BorderHotColor := $308AC2;
      end;
    tsWindows8, tsWindows10:
      begin
        Button.Color := $F7F6F5;
        Button.ColorTo := $F7F6F5;
        Button.ColorMirror := $F7F6F5;
        Button.ColorMirrorTo := $F7F6F5;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $F7EFE8;
        Button.ColorHotTo := $F7EFE8;
        Button.ColorMirrorHot := $F7EFE8;
        Button.ColorMirrorHotTo := $F7EFE8;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $F7E0C9;
        Button.ColorDownTo := $F7E0C9;
        Button.ColorMirrorDown := $F7E0C9;
        Button.ColorMirrorDownTo := $F7E0C9;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $F7E0C9;
        SelectionAppearance.ColorTo := $F7E0C9;
        SelectionAppearance.ColorMirror := $F7E0C9;
        SelectionAppearance.ColorMirrorTo := $F7E0C9;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $E4A262;

        BorderColor := $E4E3E2;
        BorderHotColor := $F9CEA4;
      end;
    tsOffice2013White:
      begin
        Button.Color := clWhite;
        Button.ColorTo := clNone;
        Button.ColorMirror := clWhite;
        Button.ColorMirrorTo := clNone;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $FCF0E4;
        Button.ColorHotTo := $FCF0E4;
        Button.ColorMirrorHot := $FCF0E4;
        Button.ColorMirrorHotTo := $FCF0E4;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $FCE2C8;
        Button.ColorDownTo := $FCE2C8;
        Button.ColorMirrorDown := $FCE2C8;
        Button.ColorMirrorDownTo := $FCE2C8;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $FCE2C8;
        SelectionAppearance.ColorTo := $FCE2C8;
        SelectionAppearance.ColorMirror := $FCE2C8;
        SelectionAppearance.ColorMirrorTo := $FCE2C8;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $E59D56;

        BorderColor := $D4D4D4;
        BorderHotColor := $EAB47E;
      end;

    tsOffice2013LightGray:
      begin
        Button.Color := $F6F6F6;
        Button.ColorTo := $F6F6F6;
        Button.ColorMirror := $F6F6F6;
        Button.ColorMirrorTo := $F6F6F6;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $FCF0E4;
        Button.ColorHotTo := $FCF0E4;
        Button.ColorMirrorHot := $FCF0E4;
        Button.ColorMirrorHotTo := $FCF0E4;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $FCE2C8;
        Button.ColorDownTo := $FCE2C8;
        Button.ColorMirrorDown := $FCE2C8;
        Button.ColorMirrorDownTo := $FCE2C8;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $FCE2C8;
        SelectionAppearance.ColorTo := $FCE2C8;
        SelectionAppearance.ColorMirror := $FCE2C8;
        SelectionAppearance.ColorMirrorTo := $FCE2C8;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $E59D56;

        BorderColor := $C6C6C6;
        BorderHotColor := $EAB47E;
      end;

    tsOffice2013Gray:
      begin
        Button.Color := $E5E5E5;
        Button.ColorTo := $E5E5E5;
        Button.ColorMirror := $E5E5E5;
        Button.ColorMirrorTo := $E5E5E5;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $FCF0E4;
        Button.ColorHotTo := $FCF0E4;
        Button.ColorMirrorHot := $FCF0E4;
        Button.ColorMirrorHotTo := $FCF0E4;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $FCE2C8;
        Button.ColorDownTo := $FCE2C8;
        Button.ColorMirrorDown := $FCE2C8;
        Button.ColorMirrorDownTo := $FCE2C8;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $FCE2C8;
        SelectionAppearance.ColorTo := $FCE2C8;
        SelectionAppearance.ColorMirror := $FCE2C8;
        SelectionAppearance.ColorMirrorTo := $FCE2C8;
        SelectionAppearance.TextColor := clBlack;
        SelectionAppearance.BorderColor := $E59D56;

        BorderColor := $ABABAB;
        BorderHotColor := $EAB47E;
      end;

    tsOffice2016White:
      begin

        Button.Color := clWhite;
        Button.ColorTo := clNone;
        Button.ColorMirror := clWhite;
        Button.ColorMirrorTo := clNone;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $F2E1D5;
        Button.ColorHotTo := $F2E1D5;
        Button.ColorMirrorHot := $F2E1D5;;
        Button.ColorMirrorHotTo := $F2E1D5;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $E3BDA3;
        Button.ColorDownTo := $E3BDA3;
        Button.ColorMirrorDown := $E3BDA3;
        Button.ColorMirrorDownTo := $E3BDA3;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $E3BDA3;
        SelectionAppearance.ColorTo := $E3BDA3;
        SelectionAppearance.ColorMirror := $E3BDA3;
        SelectionAppearance.ColorMirrorTo := $E3BDA3;
        SelectionAppearance.TextColor := $505050;
        SelectionAppearance.BorderColor := $E3BDA3;

        BorderColor := $D4D4D4;
        BorderHotColor := $F2E1D5;
      end;

    tsOffice2016Gray:
      begin
        Button.Color := $B2B2B2;
        Button.ColorTo := $B2B2B2;
        Button.ColorMirror := $B2B2B2;
        Button.ColorMirrorTo := $B2B2B2;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $F2E1D5;
        Button.ColorHotTo := $F2E1D5;
        Button.ColorMirrorHot := $F2E1D5;
        Button.ColorMirrorHotTo := $F2E1D5;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $E3BDA3;
        Button.ColorDownTo := $E3BDA3;
        Button.ColorMirrorDown := $E3BDA3;
        Button.ColorMirrorDownTo := $E3BDA3;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;


        SelectionAppearance.Color := $E3BDA3;
        SelectionAppearance.ColorTo := $E3BDA3;
        SelectionAppearance.ColorMirror := $E3BDA3;
        SelectionAppearance.ColorMirrorTo := $E3BDA3;
        SelectionAppearance.TextColor := $424242;
        SelectionAppearance.BorderColor := $E3BDA3;

        BorderColor := $444444;
        BorderHotColor := $F2E1D5;
      end;

    tsOffice2016Black:
      begin
        Button.Color := $363636;
        Button.ColorTo := $363636;
        Button.ColorMirror := $363636;
        Button.ColorMirrorTo := $363636;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        Button.ColorHot := $6A6A6A;
        Button.ColorHotTo := $6A6A6A;
        Button.ColorMirrorHot := $6A6A6A;
        Button.ColorMirrorHotTo := $6A6A6A;
        Button.GradientHot := ggVertical;
        Button.GradientMirrorHot := ggVertical;

        Button.ColorDown := $444444;
        Button.ColorDownTo := $444444;
        Button.ColorMirrorDown := $444444;
        Button.ColorMirrorDownTo := $444444;
        Button.GradientDown := ggVertical;
        Button.GradientMirrorDown := ggVertical;

        SelectionAppearance.Color := $444444;
        SelectionAppearance.ColorTo := $444444;
        SelectionAppearance.ColorMirror := $444444;
        SelectionAppearance.ColorMirrorTo := $444444;
        SelectionAppearance.TextColor := $A6A6A6;
        SelectionAppearance.BorderColor := $444444;

        BorderColor := $444444;
        BorderHotColor := $A6A6A6;
      end;

    tsWhidbey:
      begin
        Button.Color := clWhite;
        Button.ColorTo := $DFEDF0;
        Button.ColorMirror := $DFEDF0;
        Button.ColorMirrorTo := $DFEDF0;
        Button.Gradient := ggVertical;
        Button.GradientMirror := ggVertical;

        BorderColor := clNone;
        BorderHotColor := $0099CEDB;
      end;
    tsCustom:
      begin
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetComponentStyleAndAppColor(
  AStyle: TTMSStyle; AppColor: TColor);
begin
  SetComponentStyle(AStyle);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetEditRect;
var
  Loc: TRect;
  eh,th: integer;
  Canvas: TCanvas;
begin
  if csDesigning in ComponentState then
    Exit;

  Canvas := TCanvas.Create;
  try
    Canvas.Handle := GetDC(Handle);
    Canvas.Font.Assign(Font);
    th := Canvas.TextHeight('gh');
    eh := ClientRect.Bottom - ClientRect.Top;
    if (th < eh) then
      th := (eh - th) div 2
    else
      th := 0;

    ReleaseDC(Handle, Canvas.Handle);
  finally
    Canvas.Free;
  end;

  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 5;
  if (BorderStyle = bsNone) then
  begin
    Loc.Top := th; // +2;
    Loc.Left := 4; // 2;
  end
  else
  begin
    Loc.Top := th; // +1;
    Loc.Left := 4; // 2;
  end;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetFlat(const Value: Boolean);
begin
  if (csLoading in ComponentState) then
  begin
    FFlat := Value;
    Exit;
  end;

  if FFlat <> Value then
  begin
    FFlat := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetBorderHotColor(const Value: TColor);
begin
  if FBorderHotColor <> Value then
  begin
    FBorderHotColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  //inherited;
  Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMLButtonDown(var Msg: TWMMouse);
var
  //uchar: Integer;
  SecondDown: Boolean;
begin
  SecondDown:= false;
  if FDroppedDown and Assigned(FDropDownList) and (FDropDownList.Visible) then   // CancelMode wihe DropDown on second click
    SecondDown:= true;

  inherited;
  if csDesigning in ComponentState then
    Exit;

  if Style = csDropDownList then
  begin
    if not SecondDown then
      ButtonClick;
    exit;
  end;

  if PtInRect(GetButtonRect, point(msg.xpos, msg.ypos)) then
  begin
    if not SecondDown then
      ButtonClick;
  end;

  {click outside selection}
 { uchar := CharFromPos(point(msg.xpos,msg.ypos));

  if (SelLength <= 0) or (uchar < SelStart) or (uChar > SelStart + SelLength) or
     (GetFocus <> self.Handle) then
    inherited
  else
    if (uChar >= SelStart) and (uChar <= SelStart + SelLength) and (SelLength > 0) then
      FButtonDown := True;  }
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMLButtonUp(var Msg: TWMMouse);
{var
 uchar: Integer;
}
begin
{ if fButtonDown then
  begin
   uchar:=CharFromPos(point(msg.xpos,msg.ypos));
   SelStart:=uChar;
   SelLength:=0;
  end;
 }
  fButtonDown := false;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMMouseMove(var Msg: TWMMouse);
begin
  inherited;

  if Style = csDropDownList then
  begin
    if (Cursor <> crArrow) then
    begin
      FOldCursor := Cursor;
      Cursor := crArrow;
    end;
    Exit;
  end;

  if PtInRect(GetButtonRect, point(msg.xpos, msg.ypos)) then
  begin
    if (Cursor <> crArrow) then
    begin
      FOldCursor := Cursor;
      Cursor := crArrow;
    end;
  end
  else if (Cursor = crArrow) then
    Cursor := FOldCursor;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvOfficeComboBoxActionLink;
end;

function TAdvCustomOfficeComboBox.GetButtonRect: TRect;
var
  R: TRect;
begin
  R := ClientRect;
  Result := Rect(R.Right - FButton.Width - 1, R.Top + 1, R.Right - 1, R.Bottom - 2);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DrawBorders(DC: HDC);
var
  Canvas: TCanvas;
  R, BtnR: TRect;
begin
  Canvas := TCanvas.Create;
  Canvas.Handle := DC;

  R := ClientRect;
  BtnR := GetButtonRect;

  if Flat then
  begin
    //if (FBorderColor <> clNone) or ((FMouseInControl or (Assigned(fDropDownList) and FDropDownList.Visible)) and (FBorderHotColor <> clNone)) then
    begin
      if (FMouseInControl or (GetFocus = self.Handle) or (Assigned(fDropDownList) and FDropDownList.Visible)) and (FBorderHotColor <> clNone) then
      begin
        Canvas.Pen.Color := FBorderHotColor;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end
      else
      begin
        if not Enabled and not (csDesigning in ComponentState) and (BorderDisabledColor <> clNone) then
          Canvas.Pen.Color := BorderDisabledColor
        else if (FBorderColor <> clNone) then
          Canvas.Pen.Color := FBorderColor
        else
          Canvas.Pen.Color := self.Color;
        Canvas.Brush.Style := bsClear;
        Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      end;
      Canvas.MoveTo(BtnR.Left - 1, BtnR.Top);
      Canvas.LineTo(BtnR.Left - 1, BtnR.Bottom + 2);
    end;
  end
  else
  begin
  end;
  Canvas.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DrawButton;
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  DrawButton(DC);
  ReleaseDC(Handle, DC);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DrawButton(DC: HDC);
var
  Canvas: TCanvas;
  BtnR, Loc, R: TRect;
  P: TPoint;
  l, t: integer;

  procedure DrawArrow(ArP: TPoint; ArClr: TColor);
  begin
    Canvas.Pen.Color := ArClr;
    Canvas.MoveTo(ArP.X, ArP.Y);
    Canvas.LineTo(ArP.X + 5, ArP.Y);
    Canvas.MoveTo(ArP.X + 1, ArP.Y + 1);
    Canvas.LineTo(ArP.X + 4, ArP.Y + 1);
    Canvas.Pixels[ArP.X + 2, ArP.Y + 2] := ArClr;
  end;

begin
  Canvas := TCanvas.Create;
  Canvas.Handle := DC;

  BtnR := GetButtonRect;

  //--------- Clean unused Edit borders
  Canvas.Brush.Color := Color;
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

  R := Rect(0, 0, Loc.Left, Height);
  Canvas.FillRect(R);

  R := Rect(Loc.Right, 0, BtnR.Left, Height);
  Canvas.FillRect(R);

  R := Rect(0, 0, Width, Loc.Top);
  Canvas.FillRect(R);

  R := Rect(0, Loc.Bottom, Width, Height);
  Canvas.FillRect(R);
  //---------------------------

  P.X := BtnR.Left + ((BtnR.Right - BtnR.Left - 5) div 2) + 1;
  P.Y := BtnR.Top + ((BtnR.Bottom - BtnR.Top - 3) div 2) + 1;

  BtnR.Bottom := BtnR.Bottom + 1;

  if Flat then
  begin
    if Assigned(FDropDownList) and FDropDownList.Visible {FDroppedDown} then
    begin
      //P.X:= P.X + 1;
      //P.Y := P.Y + 1;
      if (FButton.ColorDown <> clNone) then
      begin
        DrawVistaGradient(Canvas, BtnR, FButton.ColorDown, FButton.ColorDownTo, FButton.ColorMirrorDown, FButton.ColorMirrorDownTo, clNone,
          FButton.GradientDown, FButton.GradientMirrorDown, '', Canvas.Font, Enabled, False, aaClearType, True, False, tpTop);
        //DrawGradient(Canvas, FButton.ColorDown, FButton.ColorDownTo, 16, BtnR, false);
      end;

      if not FButton.GlyphDown.Empty then
      begin
        t := ((BtnR.Bottom - BtnR.Top) - FButton.GlyphDown.Height) div 2 + 1;
        l := ((BtnR.Right - BtnR.Left) - FButton.GlyphDown.Width) div 2;
        FButton.GlyphDown.Transparent := true;
        Canvas.Draw(BtnR.Left + l, BtnR.Top + t, FButton.GlyphDown);
      end
      else
        DrawArrow(P, FButton.DownArrowColor);
    end
    else if (FMouseInControl or self.Focused) and not (csDesigning in ComponentState) then
    begin
      if FButton.ColorHot <> clNone then
      begin
        DrawVistaGradient(Canvas, BtnR, FButton.ColorHot, FButton.ColorHotTo, FButton.ColorMirrorHot, FButton.ColorMirrorHotTo, clNone,
          FButton.GradientHot, FButton.GradientMirrorHot, '', Canvas.Font, Enabled, False, aaClearType, True, False, tpTop);
        //DrawGradient(Canvas, FButton.ColorHot, FButton.ColorHotTo, 16, BtnR, false);
      end;

      if not FButton.GlyphHot.Empty then
      begin
        t := ((BtnR.Bottom - BtnR.Top) - FButton.GlyphHot.Height) div 2 + 1;
        l := ((BtnR.Right - BtnR.Left) - FButton.GlyphHot.Width) div 2;
        FButton.GlyphHot.Transparent := true;
        Canvas.Draw(BtnR.Left + l, BtnR.Top + t, FButton.GlyphHot);
      end
      else
        DrawArrow(P, FButton.HotArrowColor);
    end
    else
    begin
      if not Enabled and not (csDesigning in ComponentState) then
      begin
        DrawVistaGradient(Canvas, BtnR, FButton.ColorDisabled, FButton.ColorDisabledTo, FButton.ColorMirrorDisabled, FButton.ColorMirrorDisabledTo, clNone,
          FButton.Gradient, FButton.GradientMirror, '', Canvas.Font, Enabled, False, aaClearType, True, False, tpTop);
      end
      else
      if (FButton.Color <> clNone) then
      begin
        DrawVistaGradient(Canvas, BtnR, FButton.Color, FButton.ColorTo, FButton.ColorMirror, FButton.ColorMirrorTo, clNone,
          FButton.Gradient, FButton.GradientMirror, '', Canvas.Font, Enabled, False, aaClearType, True, False, tpTop);
        //DrawGradient(Canvas, FButton.Color, FButton.ColorTo, 16, BtnR, false);
      end;

      if not FButton.Glyph.Empty then
      begin
        t := ((BtnR.Bottom - BtnR.Top) - FButton.Glyph.Height) div 2 + 1;
        l := ((BtnR.Right - BtnR.Left) - FButton.Glyph.Width) div 2;
        FButton.Glyph.Transparent := true;
        Canvas.Draw(BtnR.Left + l, BtnR.Top + t, FButton.Glyph);
      end
      else
        if not Enabled and not (csDesigning in ComponentState) then
          DrawArrow(P, FButton.DisabledArrowColor)
        else
          DrawArrow(P, FButton.ArrowColor);
    end;
  end
  else
  begin
  end;
  Canvas.Free;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMPaint(var Msg: TWMPaint);
var
  R, BtnR: TRect;
begin
  inherited;

  R := ClientRect;
  BtnR := GetButtonRect;

  if Flat then
  begin
    // Button Painting
    DrawButton;

    // Control and Button Border
    DrawBorders();
  end
  else
  begin
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ResizeControl;
var
  MinHeight: Integer;
begin
  MinHeight := GetMinHeight;

  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }

  if (Height < MinHeight) then
    Height := MinHeight
  else
    if (FButton <> nil) then
    begin
      SetEditRect;
    end;

  Invalidate;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.GetMinHeight: Integer;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  I := SysMetrics.tmHeight;
  if I > Metrics.tmHeight then I := Metrics.tmHeight;
  {Result := Metrics.tmHeight + I div 4 + GetSystemMetrics(SM_CYBORDER) * 4 +2;}
  Result := Metrics.tmHeight + I div 4 {+ GetSystemMetrics(SM_CYBORDER) * 4};
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMSize(var Message: TWMSize);
begin
  inherited;
  ResizeControl;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CreateWnd;
begin
  inherited CreateWnd;
  Width := Width - 1;
  Width := Width + 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or (ES_MULTILINE);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WndProc(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMEnter(var Message: TCMGotFocus);
begin
  inherited;
  DrawButton;
  DrawBorders;
  PopulateListbox;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMExit(var Message: TCMExit);
begin
  inherited;
  //Text := FOldText;
  DrawButton;
  DrawBorders;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DrawBorders;
var
  DC: HDC;
begin
  DC := GetDC(Handle);
  DrawBorders(DC);
  ReleaseDC(Handle, DC);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ButtonClick;
begin
  ShowDropDownInt;
end;

procedure TAdvCustomOfficeComboBox.PopulateListBox;
var
  i: integer;
begin
  if Assigned(FDropDownListBox) then
  begin
    FDropDownListBox.Items.Clear;
    if FDisplayRecentSelection then
      for i := min(FRecentSelection.Count - 1, DropDownCount - 1) downto 0 do
        FDropDownListBox.Items.Add(FRecentSelection[i]);

    for i := 0 to Items.Count - 1 do
      FDropDownListBox.Items.Add(Items[i]);

    //FDropDownListBox.Items.Assign(FItems);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ShowDropDownInt;
begin
  if not Assigned(FDropDownList) then
    Exit;

  if FDropDownList.Visible then
  begin
    HideDropDown;
    Exit;
  end;
  ShowDropDown;
end;
//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ShowDropDown;
var
  P: TPoint;
  i, MinTW: integer;
  R: TRect;
  mon: TMonitor;

begin
  if not Assigned(FDropDownList) then
    Exit;

  if FDropDownList.Visible then
    Exit;

  if not FDropDownList.Visible then
    BeforeDropDown;

  mon := Screen.MonitorFromWindow(Handle);

  r := mon.WorkareaRect;

  FDropDownListBox.ItemHeight := GetListItemHeight;

  if FItems.Count < FDropDownCount then
  begin
    FDropDownList.Height := Max(20, (FItems.Count * GetListItemHeight {LISTITEMHEIGHT}) + 4);
  end
  else
  begin
    FDropDownList.Height := Max(20, (FDropDownCount * GetListItemHeight {LISTITEMHEIGHT}) + 4);
  end;

  P := Point(0, Height);
  P := ClientToScreen(P);
  FDropDownList.Left := P.X;

  if R.Bottom > (P.Y + FDropDownList.Height + 4) then
    FDropDownList.Top := P.Y
  else
    FDropDownList.Top := P.Y - Height - FDropDownList.Height;

  if R.Right < (P.X + FDropDownList.Width + 4) then
    FDropDownList.Left := P.X + Width - FDropDownList.Width;

  FDropDownListBox.Font.Size := FontSize;

  PopulateListBox;

  FUpdatingIndex := true;

  i := FDropDownListBox.Items.IndexOf(Text);
  if (i >= 0) then
    FDropDownListBox.ItemIndex := i
  else
    FDropDownListBox.ItemIndex := 0;

  FUpdatingIndex := false;

  MinTW := Width;

  for i := 0 to FDropDownListBox.Items.Count - 1 do
  begin
    FDropDownListBox.Canvas.Font.Name := FDropDownListBox.Items[i];
    FDropDownListBox.Canvas.Font.Size := FontSize;
    MinTW := Max(MinTW, Round(1.4 * FDropDownList.Canvas.TextWidth(FDropDownListBox.Items[i])));
  end;

  if (FDropDownWidth > 0) then
    FDropDownList.Width := FDropDownWidth
  else
  begin
    if MinTW > self.Width + 1 then
      FDropDownList.Width := MinTW + 24  + 24 //GetSystemMetrics(SM_CXHSCROLL)
    else
      FDropDownList.Width := MinTW;
  end;

  FDropDownList.Visible := true;
  FDroppedDown := true;

  SendMessage(Handle, EM_SETSEL, 0, Length(Text));
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.HideDropDown;
begin
  if Assigned(OnCloseUp) then
    OnCloseUp(Self);
  FDropDownList.Visible := false;
  FDroppedDown := false;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.CMCancelMode(var Message: TMessage);
begin
  inherited;
  if FDropDownList.Visible then
    HideDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMKillFocus(var Msg: TWMKillFocus);
begin
  if (csLoading in ComponentState) then
    Exit;

  if FDropDownList.Visible and not (msg.FocusedWnd = FDropDownList.Handle) then
    HideDropDown;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMSetFocus(var Msg: TWMSetFocus);
begin
  if csLoading in ComponentState then
    Exit;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ListBoxKeyPress(Sender: TObject;
  var Key: Char);
begin
{
  case Key of
    #27:
    begin
      Text:= FOldText;
      if FDropDownList.Visible then
      begin
        HideDropDown;
      end;
    end;
    #13:
    begin
      if (Items.IndexOf(Text)<>-1) then
      begin
        text:= Items.Strings[Items.IndexOf(Text)];
        self.Change;
        ValueChanged;
        if FDropDownList.Visible then
          HideDropDown;
      end;
    end;
  end;
}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DoSelect(Index: Integer);
begin
  if Assigned(OnSelect) then
    OnSelect(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ListBoxMouseUp(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  i: integer;

begin
  // selection End
  i := TListBox(Sender).ItemAtPos(Point(X, Y), true);
  if i >= 0 then
  begin
    FUpdatingIndex := true;
    i := FItems.IndexOf(TListBox(Sender).Items[i]);
    self.ItemIndex := i;
    DoSelect(i);
    FUpdatingIndex := false;
  end;

  HideDropDown;

  i := FDropdownListBox.ItemIndex;

  if Assigned(GetParentForm(self)) then
    SetActiveWindow(GetParentForm(self).Handle);

  if FAutoResetFocus then
    Self.SetFocus;

  // restore as Windows sometimes resets it to -1!
  FDropdownListBox.ItemIndex := i;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetItems(const Value: TStringList);
begin
  if Assigned(Value) then
    FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.DropDownOnDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  TrueTypeFont: Boolean;
  tm: TTextMetric;
begin
  TListBox(Control).Canvas.Font.Assign(font);

  //TrueTypeFont := IsTrueTypeFont(TListBox(Control).Canvas.Font.Name);

  TrueTypeFont := false;

  GetTextMetrics(TListBox(Control).Canvas.Handle, tm);
  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      TrueTypeFont := true;
    end
  end;

  if (State = [odSelected]) or (State = [odFocused]) or (State = [odSelected, odFocused]) then
  begin
    TListBox(Control).Canvas.Brush.Color := FSelectionAppearance.Color; // SelectionColor;
    TListBox(Control).Canvas.Font.Color := FSelectionAppearance.TextColor; // FSelectionTextColor;
  end;

  if (FSelectionAppearance.ColorTo <> clNone) and ((State = [odSelected]) or (State = [odFocused]) or (State = [odSelected, odFocused])) then
  begin
    with FSelectionAppearance do
    begin
      r := Rect;
      DrawVistaGradient(TListBox(Control).Canvas, r, Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
        Gradient, GradientMirror, '', TListBox(Control).Canvas.Font, Enabled, False, FAntiAlias, FSelectionAppearance.Rounded, False, tpTop);

      // Trnasparent Corners
      TListBox(Control).Canvas.Pixels[R.Left, R.Bottom-1] := clWhite;

      TListBox(Control).Canvas.Pixels[R.Right-1, R.Bottom-1] := clWhite;
    end;

  end
  else
    TListBox(Control).Canvas.FillRect(Rect);

  TListBox(Control).Canvas.Brush.Style := bsClear;

  //TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FFontGlyph);
  Rect.Left := Rect.Left + 4;

  if FDisplayRecentSelection and (Index = FRecentSelection.Count - 1) then
  begin
    Rect.Bottom := Rect.Bottom - 3;
  end;

  if (TrueTypeFont) then
  begin
    DrawVistaText(TListBox(Control).Canvas, taLeftJustify, Rect, TListBox(Control).Items[Index], TListBox(Control).Canvas.Font, Enabled, True, FAntiAlias, tpTop)
  end
  else
  begin
    DrawText(TListBox(Control).Canvas.Handle, PChar(TListBox(Control).Items[Index]), -1, Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  end;

  if FDisplayRecentSelection and (Index = FRecentSelection.Count - 1) then
  begin
    Rect.Bottom := Rect.Bottom + 3;
    TListBox(Control).Canvas.Pen.Color := clGray;
    TListBox(Control).Canvas.MoveTo(1, Rect.Bottom - 3);
    TListBox(Control).Canvas.LineTo(TListBox(Control).Width - 1, Rect.bottom - 3);

    TListBox(Control).Canvas.MoveTo(1, Rect.Bottom - 1);
    TListBox(Control).Canvas.LineTo(TListBox(Control).Width - 1, Rect.bottom - 1);
  end;

 // TListBox(Control).Canvas.Font.Assign(Self.Font);
{  TListBox(Control).Canvas.FillRect(Rect);
  TListBox(Control).Canvas.Brush.Style:= bsClear;
  TListBox(Control).Canvas.TextOut(Rect.Left + 2, Rect.Top, Items[Index]); }
end;

procedure TAdvCustomOfficeComboBox.DropWindowHide(Sender: TObject);
begin
  if Assigned(OnCloseUp) then
    OnCloseUp(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.BeforeDropDown;
begin
  if Assigned(FOnBeforeDropDown) then
    FOnBeforeDropDown(self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ListBoxMouseMove(Sender: TObject;
  Shift: TShiftState; X, Y: Integer);
var
  i: integer;
begin
  // Hot selection
  if FUpdatingIndex then
    Exit;

  if (X = FOldX) and (Y = FOldY) then
    Exit;

  FOldX := X;
  FOldY := Y;

  i := TListBox(Sender).ItemAtPos(Point(X, Y), true);
  if (i >= 0) and (i <> TListBox(Sender).ItemIndex) then
  begin
    FUpdatingIndex := true;
    TListBox(Sender).ItemIndex := i;
    FUpdatingIndex := false;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetItemIndex(const Value: integer);
begin
  if FItems.Count <= 0 then
  begin
    FItemIndex := -1;
    Exit;
  end;

  if (Value < FItems.Count) then
  begin
    FItemIndex := Value;
    if (Value >= 0) then
    begin
      Self.Text := FItems[FItemIndex];
      if Assigned(FDropDownList) and FDropDownList.Visible then
        if Assigned(FDropDownListBox) then
          FDropDownListBox.ItemIndex := FDropDownListBox.Items.IndexOf(FItems[FItemIndex]);
    end
    else
      if (Value = -1) then
      begin
        Self.Text := '';
      end;

   ValueChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetFontSize(const Value: integer);
begin
  FFontSize := Value;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.GetListItemHeight: integer;
var
  OlfFontName: string;
begin
  if Assigned(FDropDownListBox) and FAutoItemSize then
  begin
    OlfFontName := FDropDownListBox.Canvas.Font.Name;
    FDropDownListBox.Canvas.Font.Name := 'Tahoma';
    FDropDownListBox.Canvas.Font.Size := FFontSize;
    Result := Max(LISTITEMHEIGHT, FDropDownListBox.Canvas.TextHeight('gh') + 2);
    FDropDownListBox.Canvas.Font.Name := OlfFontName;
  end
  else
    Result := FItemHeight;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetLookUp(const Value: Boolean);
begin
  FLookUp := Value;
end;

procedure TAdvCustomOfficeComboBox.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.LookUpText;
var
  c: string;
  i: integer;
  UsrStr, AutoAdd: string;
begin
  if csDesigning in ComponentState then
    Exit;
    
  if not FLookUp then
    Exit;

  if not FWorkMode then
    Exit;

  c := UpStr(Text, FMatchCase);
  c := Copy(c, 1, selstart);

  if (Items.Count > 0) then
    for i := 0 to Items.count - 1 do
    begin
      if pos(c, upstr(Items.Strings[i], fMatchCase)) = 1 then
      begin
        UsrStr := copy(text, 1, length(c));
        AutoAdd := copy(Items.Strings[i], length(c) + 1, 255);

        if Assigned(FDropDownListBox) and FDropDownList.Visible then
        begin
          FDropDownListBox.ItemIndex := FDropDownListBox.Items.IndexOf(Items.Strings[i]);
        end;
        Text := UsrStr + AutoAdd;
       //Modified := True;
        SendMessage(Handle, EM_SETSEL, length(c), length(text));
        Exit;
      end;
    end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if FChanged and LookUp and not (key in [VK_BACK, VK_DELETE, VK_LEFT, VK_RIGHT]) then
    LookupText;
  FChanged := False;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.Change;
begin
  inherited;
  FChanged := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  case Key of
    vk_back, vk_delete: FWorkMode := false;
    vk_return:
      begin
        if (Items.IndexOf(Text) <> -1) then
        begin
          text := Items.Strings[Items.IndexOf(Text)];
          self.Change;
          ValueChanged;
          if FDropDownList.Visible then
            HideDropDown;
        end;
      end;
    VK_F4:
      begin
        if (GetKeyState(VK_MENU) and $8000 = 0) then
          ShowDropDownInt;
      end;
    VK_UP: Previous;
    VK_DOWN:
      begin
        if (ssAlt in Shift) then
          ShowDropDownInt
        else
          Next;
      end;
    VK_PRIOR: PageJump(false);
    VK_NEXT: PageJump(true);
    VK_ESCAPE:
      begin
        Text := FOldText;
        if FDropDownList.Visible then
        begin
          HideDropDown;
          Self.SetFocus;
        end;
      end;
  else
    FWorkMode := true;
  end;

  inherited KeyDown(key, shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.WMChar(var Msg: TWMKey);
var
  key: Char;
  i: Integer;
  nt: DWORD;
  str: string;
begin
  if Msg.CharCode = VK_RETURN then
  begin
    key := #13;
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, key);

    Msg.CharCode := 0;
    i := Items.IndexOf(Text);
    if (i <> -1) then
    begin
      text := Items.Strings[Items.IndexOf(Text)];
      SendMessage(Handle, EM_SETSEL, 0, 0);
      self.Change;
      ValueChanged;

      if FDropDownList.Visible then
        HideDropDown;

      DoSelect(i);
    end;
    Exit;
  end;

  if (Style = csDropDownlist) then
  begin
    if (Msg.CharCode <> 8) then
    begin
      nt := GetTickCount;
      if nt - FLookupTime < 1000 then
      begin
        str := FLookupText + AnsiUppercase(Chr(Msg.CharCode));
      end
      else
        str := Uppercase(Chr(Msg.CharCode));

      FLookupTime := nt;
      FLookUpText := str;

      if (Items.Count > 0) then
      begin
        for i := 0 to Items.count - 1 do
        begin
          if pos(str, upstr(Items.Strings[i], FMatchCase)) = 1 then
          begin
            if Assigned(FDropDownListBox) and FDropDownList.Visible then
            begin
              FDropDownListBox.ItemIndex := FDropDownListBox.Items.IndexOf(Items.Strings[i]);
            end;

            FItemIndex := i;
            inherited Text := Items.Strings[i];
            SelStart := 0;
            SelLength := Length(Text);
            break;
          end;
        end;
        ValueChanged;
      end;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ValueChanged;
begin
  FOldText := Text;

  if Items.IndexOf(Text) >= 0 then
  begin
    FItemIndex := FItems.IndexOf(Text);

    if csLoading in ComponentState then
      Exit;

    if FRecentSelection.IndexOf(Text) >= 0 then
      FRecentSelection.Delete(FRecentSelection.IndexOf(Text));

    if (FRecentSelection.Count > 0) and (FRecentSelection.Count > DropDownCount - 2) then
      while (FRecentSelection.Count > DropDownCount - 2) do
        FRecentSelection.Delete(0);

    if FRecentSelection.IndexOf(Text) < 0 then
      FRecentSelection.Add(Text);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.First;
begin
  if Assigned(FDropDownListBox) and (FDropDownListBox.Items.Count > 0) then
  begin
    FDropDownListBox.ItemIndex := 0;
    Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.Last;
begin
  if Assigned(FDropDownListBox) and (FDropDownListBox.Items.Count > 0) then
  begin
    FDropDownListBox.ItemIndex := FDropDownListBox.Items.Count - 1;
    Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.Next;
begin
  if Assigned(FDropDownListBox) and (FDropDownListBox.Items.Count > 0) then
  begin
    if (FDropDownListBox.ItemIndex < FDropDownListBox.Items.Count - 1) and (FDropDownListBox.ItemIndex >= 0) then // added by dh
    begin
      FDropDownListBox.ItemIndex := FDropDownListBox.ItemIndex + 1;
      FItemIndex := Items.IndexOf(FDropDownListBox.Items[FDropDownListBox.ItemIndex]);
      Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
    end;
  end
  else
  begin
    if FItemIndex < Items.Count - 1 then
    begin
      inc(FItemIndex);
      Text := Items[FItemIndex];
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.Previous;
begin
  if Assigned(FDropDownListBox) and (FDropDownListBox.Items.Count > 0) then
  begin
    if (FDropDownListBox.ItemIndex > 0) then    // added by dh
    begin
       FDropDownListBox.ItemIndex := FDropDownListBox.ItemIndex - 1;
       FItemIndex := Items.IndexOf(FDropDownListBox.Items[FDropDownListBox.ItemIndex]);
       Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
    end;
  end
  else
  begin
    if FItemIndex > 0 then
    begin
      dec(FItemIndex);
      Text := Items[FItemIndex];
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.PageJump(GoForward: Boolean);
var
  ItemsToBeJumpped: integer;
begin
  if Assigned(FDropDownListBox) and (FDropDownListBox.ItemIndex >= 0) then
  begin
    ItemsToBeJumpped := DropDownCount - 1;
    if GoForward then
    begin
      if (FDropDownListBox.ItemIndex < FDropDownListBox.Items.Count - 1) then
      begin
        FDropDownListBox.ItemIndex := FDropDownListBox.ItemIndex + min(FDropDownListBox.Items.Count - 1 - FDropDownListBox.ItemIndex, ItemsToBeJumpped);
        Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
      end;
    end
    else
    begin
      if (FDropDownListBox.Items.Count > 0) and (FDropDownListBox.ItemIndex > 0) then
      begin
        FDropDownListBox.ItemIndex := FDropDownListBox.ItemIndex - min(FDropDownListBox.ItemIndex, ItemsToBeJumpped);
        Text := FDropDownListBox.Items[FDropDownListBox.ItemIndex];
      end;
    end;
  end
  else
  begin
    if GoForward then
      FItemIndex := Min(Items.Count - 1, FItemIndex + 4)
    else
      FItemIndex := Max(0, FItemIndex - 4);
    Text := Items[FItemIndex];
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.GetItemIndex: integer;
begin
  Result := FItemIndex; // FItems.IndexOf(Text);
  //FItemIndex:= Result;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ListBoxOnEnter(Sender: TObject);
begin
  //Self.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.ListBoxKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  FDropDownList.HideOnDeActivate := false;
  SendMessage(self.Handle, WM_KEYDOWN, Key, 0);
  SendMessage(self.Handle, WM_KEYUP, Key, 0);
  FDropDownList.HideOnDeActivate := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetDisplayRecentSelection(
  const Value: Boolean);
begin
  FDisplayRecentSelection := Value;
end;

procedure TAdvCustomOfficeComboBox.SetDropDownColor(const Value: TColor);
begin
  FDropDownColor := Value;

  if not (csDesigning in ComponentState) and Assigned(FDropDownListBox) then
    FDropDownListBox.Color := Value;
end;

procedure TAdvCustomOfficeComboBox.SetDroppedDown(const Value: boolean);
begin
  FDroppedDown := Value;
  if FDroppedDown then
  begin
    SetFocus;
    ShowDropDown;
  end
  else
    HideDropDown;
end;

//------------------------------------------------------------------------------

function TAdvCustomOfficeComboBox.GetItemHeight: integer;
begin
  Result := FItemHeight;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetItemHeight(const Value: integer);
begin
  FItemHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomOfficeComboBox.SetStyle(const Value: TComboStyle);
begin
  if Value <> FStyle then
  begin
    FStyle := Value;
    self.ReadOnly := FStyle = csDropDownList;
  end;
end;

procedure TAdvCustomOfficeComboBox.SetAutoItemSize(const Value: boolean);
begin
  FAutoItemSize := Value;
end;

function TAdvCustomOfficeComboBox.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvCustomOfficeComboBox.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvCustomOfficeComboBox.SetVersion(const Value: string);
begin

end;

procedure TAdvCustomOfficeComboBox.SetSelectionAppearance(
  const Value: TSelectionAppearance);
begin
  FSelectionAppearance.Assign(Value);
end;

procedure TAdvCustomOfficeComboBox.OnSelectionAppearanceChanged(
  Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvCustomOfficeComboBox.SetAntiAlias(const Value: TAntiAlias);
begin
  FAntiAlias := Value;
  Invalidate;
end;

{ TAdvComboBtn }

procedure TAdvComboBtn.Assign(Source: TPersistent);
begin
  if Source is TAdvComboBtn then
  begin
    FColor := TAdvComboBtn(Source).Color;
    FColorTo := TAdvComboBtn(Source).ColorTo;
    FColorHot := TAdvComboBtn(Source).ColorHot;
    FColorHotTo := TAdvComboBtn(Source).ColorHotTo;
    FColorDown := TAdvComboBtn(Source).ColorDown;
    FColorDownTo := TAdvComboBtn(Source).ColorDownTo;
    FColorDisabled := TAdvComboBtn(Source).ColorDisabled;
    FColorDisabledTo := TAdvComboBtn(Source).ColorDisabledTo;

    FColorMirror := TAdvComboBtn(Source).ColorMirror;
    FColorMirrorTo := TAdvComboBtn(Source).ColorMirrorTo;
    FColorMirrorHot := TAdvComboBtn(Source).ColorMirrorHot;
    FColorMirrorHotTo := TAdvComboBtn(Source).ColorMirrorHotTo;
    FColorMirrorDown := TAdvComboBtn(Source).ColorMirrorDown;
    FColorMirrorDownTo := TAdvComboBtn(Source).ColorMirrorDownTo;
    FColorMirrorDisabled := TAdvComboBtn(Source).ColorMirrorDisabled;
    FColorMirrorDisabledTo := TAdvComboBtn(Source).ColorMirrorDisabledTo;

    FGlyph.Assign(TAdvComboBtn(Source).Glyph);
    FGlyphHot.Assign(TAdvComboBtn(Source).GlyphHot);
    FGlyphDown.Assign(TAdvComboBtn(Source).GlyphDown);
    FWidth := TAdvComboBtn(Source).Width;

    inherited Assign(Source);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

constructor TAdvComboBtn.Create;
begin
  inherited;
  FColor := clBtnFace;
  FColorTo := clBtnFace;
  FColorHot := clBtnFace;
  FColorHotTo := clBtnFace;
  FColorDown := clBtnFace;
  FColorDownTo := clBtnFace;

  FColorDisabled := clWhite;
  FColorDisabledTo := clWhite;

  FColorMirror := clSilver;
  FColorMirrorTo := clWhite;
  FColorMirrorHot := $F5C8AD;
  FColorMirrorHotTo := $FFF8F4;
  FColorMirrorDown := BrightnessColor($F5C8AD, -10,-10,0);
  FColorMirrorDownTo := BrightnessColor($FFF8F4, -10,-10,0);
  FColorMirrorDisabled := clSilver;
  FColorMirrorDisabledTo := clWhite;

  FArrowColor := clBlack;
  FHotArrowColor := clBlack;
  FDownArrowColor := clWhite;
  FDisabledArrowColor := clGray;

  FGlyph := TBitMap.Create;
  FGlyphHot := TBitMap.Create;
  FGlyphDown := TBitMap.Create;
  FWidth := 12;
end;

//------------------------------------------------------------------------------

destructor TAdvComboBtn.Destroy;
begin
  FGlyph.Free;
  FGlyphHot.Free;
  FGlyphDown.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetArrowColor(const Value: TColor);
begin
  if FArrowColor <> Value then
  begin
    FArrowColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorTo(const Value: TColor);
begin
  if FColorTo <> Value then
  begin
    FColorTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetDisabledArrowColor(const Value: TColor);
begin
  if (FDisabledArrowColor <> Value) then
  begin
    FDisabledArrowColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetDownArrowColor(const Value: TColor);
begin
  if Value <> FDownArrowColor then
  begin
    FDownArrowColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorDown(const Value: TColor);
begin
  if FColorDown <> Value then
  begin
    FColorDown := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorDownTo(const Value: TColor);
begin
  if FColorDownTo <> Value then
  begin
    FColorDownTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetGlyphDown(const Value: TBitMap);
begin
  FGlyphDown.Assign(Value);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetGlyph(const Value: TBitMap);
begin
  FGlyph.Assign(Value);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetHotArrowColor(const Value: TColor);
begin
  if Value <> FHotArrowColor then
  begin
    FHotArrowColor := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorHot(const Value: TColor);
begin
  if FColorHot <> Value then
  begin
    FColorHot := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorHotTo(const Value: TColor);
begin
  if FColorHotTo <> Value then
  begin
    FColorHotTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetGlyphHot(const Value: TBitMap);
begin
  FGlyphHot.Assign(Value);
  Change;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetWidth(const Value: integer);
begin
  if (FWidth <> Value) and (Value >= 0) then
  begin
    FWidth := Value;
    if Assigned(Combo) then
      Combo.SetEditRect;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorMirror(const Value: TColor);
begin
  if (FColorMirror <> Value) then
  begin
    FColorMirror := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorMirrorDisabled(const Value: TColor);
begin
  if (FColorMirrorDisabled <> Value) then
  begin
    FColorMirrorDisabled := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorMirrorDisabledTo(const Value: TColor);
begin
  if (FColorMirrorDisabledTo <> Value) then
  begin
    FColorMirrorDisabledTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetColorMirrorTo(const Value: TColor);
begin
  if (FColorMirrorTo <> Value) then
  begin
    FColorMirrorTo := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetGradient(const Value: TGDIPGradient);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvComboBtn.SetGradientMirror(const Value: TGDIPGradient);
begin
  if (FGradientMirror <> Value) then
  begin
    FGradientMirror := Value;
    Change;
  end;
end;

//------------------------------------------------------------------------------

{ TDropDownWindow }

constructor TDropDownWindow.Create(AOwner: TComponent);
begin
  inherited;
  FHideOnDeActivate := true;
  Color := clWhite;
end;

//------------------------------------------------------------------------------

procedure TDropDownWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  //Params.Style := Params.Style + WS_BORDER;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then // not for Win9x
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;

//------------------------------------------------------------------------------

destructor TDropDownWindow.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownWindow.WMActivate(var Message: TMessage);
begin
  inherited;
  if integer(Message.WParam) = integer(False) then
  begin
    if HideOnDeActivate then
    begin
      if Assigned(OnHide) then
        OnHide(Self);
      Hide;
    end;
  end
  else if Assigned(FListControl) then
    if Visible then
      FListControl.SetFocus
    else
      self.Parent.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TDropDownWindow.WMNCButtonDown(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
  // Make the hint sizable
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));

  if (pt.X > Width - 10) and (pt.Y > Height - 10) then
    message.Result := HTBOTTOMRIGHT
end;

//------------------------------------------------------------------------------

{ TAdvFontSelector }

procedure TAdvOfficeFontSelector.BeforeDropDown;
begin
  inherited;

  if FFontGlyphTT.Empty and not (csDesigning in ComponentState) then
  begin
    FFontGlyphTT.LoadFromResourceName(Hinstance, 'TMSOFONTTT');
    FFontGlyphTT.Transparent := true;
  end;
  if FFontGlyphPS.Empty and not (csDesigning in ComponentState) then
  begin
    FFontGlyphPS.LoadFromResourceName(Hinstance, 'TMSOFONTPS');
    FFontGlyphPS.Transparent := true;
  end;
  if FFontGlyphPRN.Empty and not (csDesigning in ComponentState) then
  begin
    FFontGlyphPRN.LoadFromResourceName(Hinstance, 'TMSOFONTPRN');
    FFontGlyphPRN.Transparent := true;
  end;
  if FFontGlyphBMP.Empty and not (csDesigning in ComponentState) then
  begin
    FFontGlyphBMP.LoadFromResourceName(Hinstance, 'TMSOFONTBMP');
    FFontGlyphBMP.Transparent := true;
  end;

  //LoadFontNames;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.Change;
begin
  inherited;
  if Assigned(FFontSizeSelector) then
    FFontSizeSelector.FontName := self.Text;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

constructor TAdvOfficeFontSelector.Create(aOwner: TComponent);
begin
  inherited;
  FFontGlyphTT := TBitMap.Create;
  FFontGlyphTT.LoadFromResourceName(Hinstance, 'TMSOFONTTT');
  FFontGlyphTT.Transparent := true;

  FFontGlyphPS := TBitMap.Create;
  FFontGlyphPS.LoadFromResourceName(Hinstance, 'TMSOFONTPS');
  FFontGlyphPS.Transparent := true;

  FFontGlyphPRN := TBitMap.Create;
  FFontGlyphPRN.LoadFromResourceName(Hinstance, 'TMSOFONTPRN');
  FFontGlyphPRN.Transparent := true;

  FFontGlyphBMP := TBitMap.Create;
  FFontGlyphBMP.LoadFromResourceName(Hinstance, 'TMSOFONTBMP');
  FFontGlyphBMP.Transparent := true;

  FShowFontStyle := True;
  FShowFontGlyph := True;

  FFontSize := 12;
  
  FAllowedFontTypes := [aftBitmap, aftTrueType, aftPostScript, aftPrinter, aftFixedPitch, aftProportional];
  
  //FRecentSelection:= TStringList.Create;
  //FDisplayRecentSelection:= true;
end;

procedure TAdvOfficeFontSelector.CreateWnd;
begin
  inherited;
  if not (csDesigning in ComponentState) then
    LoadFontNames;
end;

//------------------------------------------------------------------------------

destructor TAdvOfficeFontSelector.Destroy;
begin
  FFontGlyphBMP.Free;
  FFontGlyphPRN.Free;
  FFontGlyphPS.Free;
  FFontGlyphTT.Free;
  //FRecentSelection.Free;
  inherited;
end;

procedure TAdvOfficeFontSelector.DoSelect(Index: Integer);
begin
  inherited;

  if Assigned(Action) then
    Action.Execute;

  if Assigned(OnSelectFontName) then
  begin
    if (ItemIndex >= 0) then
      OnSelectFontName(Self, self.Items[ItemIndex])
    else
      OnSelectFontName(Self, '');
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.SetSelectedFontName(const Value: string);
var
  idx: integer;
begin
  idx := Items.IndexOf(value);

  if idx >= 0 then
    ItemIndex := idx
  else
  begin
    ItemIndex := Items.Add(value);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeFontSelector.GetSelectedFontName: string;
begin
  if ItemIndex >= 0 then
    Result := Items[ItemIndex]
  else
    Result := '';
end;

//------------------------------------------------------------------------------

function TAdvOfficeFontSelector.GetFontType(const Fontname: string): TAdvFontTypes;
var
  tm : TTextMetric;
begin
  Result := [aftTrueType, aftProportional];

  if (csLoading in ComponentState) then
    Exit;
  if not Assigned(FDropDownListBox) then
    Exit;

  FDropDownListBox.Canvas.Font.Name := FontName;
  FDropDownListBox.Canvas.Font.Style := [];

  GetTextMetrics(FDropDownListBox.Canvas.Handle, tm);

  Result := [aftBitmap];
  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      Result := [aftPostScript];
    end
    else
    begin
      Result := [aftTrueType];
    end;
  end
  else
  begin
    if ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      Result := [aftPrinter];
    end;
  end;
  if ((tm.tmPitchAndFamily AND TMPF_FIXED_PITCH) = TMPF_FIXED_PITCH) then
    Result := Result + [aftProportional]
  else
    Result := Result + [aftFixedPitch];
end;

procedure TAdvOfficeFontSelector.DropDownOnDrawItem(Control: TWinControl;
  Index: Integer; Rect: TRect; State: TOwnerDrawState);
var
  r: TRect;
  aft : TAdvFontType;
  tm : TTextMetric;
  TrueTypeFont: Boolean;
  isItal: boolean;
  isBold: boolean;
  s: string;

begin
//   TAdvFontType = (aftBitmap, aftTrueType, aftPostScript, aftPrinter);

  if not Assigned(Control) then
    Exit;

  if not (Control is TListBox) then
    Exit;

  if (Index <  0) or (Index >= TListBox(Control).Items.Count) then
    Exit;

  TListBox(Control).Canvas.Font.Assign(Font);
  s := TListBox(Control).Items[Index];

  if s = '' then
    Exit;

  if ShowFontStyle and (s <> '') then
    TListBox(Control).Canvas.Font.Name := s;

  TListBox(Control).Canvas.Font.Style := [];

  //TrueTypeFont := IsTrueTypeFont(TListBox(Control).Canvas.Font.Name);

  TrueTypeFont := false;

  GetTextMetrics(TListBox(Control).Canvas.Handle, tm);
  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      TrueTypeFont := true;
    end
  end;

  isItal := (tm.tmItalic > 0);
  isBold := (tm.tmWeight >= 700);

  aft := aftBitmap;
  if ((tm.tmPitchAndFamily AND TMPF_VECTOR) = TMPF_VECTOR) then
  begin
    if ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      aft := aftPostScript;
    end
    else if TrueTypeFont then
    begin
      if not ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
        aft := aftTrueType
      else
        aft := aftBitmap;
    end;
  end
  else
  begin
    if ((tm.tmPitchAndFamily AND TMPF_DEVICE) = TMPF_DEVICE) then
    begin
      aft := aftPrinter;
    end;
  end;

  if (State = [odSelected]) or (State = [odFocused]) or (State = [odSelected, odFocused]) then
  begin
    TListBox(Control).Canvas.Brush.Color := FSelectionAppearance.Color; // SelectionColor;
    TListBox(Control).Canvas.Font.Color := FSelectionAppearance.TextColor; // FSelectionTextColor;
  end;

  if (FSelectionAppearance.ColorTo <> clNone) and ((State = [odSelected]) or (State = [odFocused]) or (State = [odSelected, odFocused])) then
  begin
    with FSelectionAppearance do
    begin
      r := Rect;
      DrawVistaGradient(TListBox(Control).Canvas, r, Color, ColorTo, ColorMirror, ColorMirrorTo, BorderColor,
        Gradient, GradientMirror, '', TListBox(Control).Canvas.Font, Enabled, False, FAntiAlias, FSelectionAppearance.Rounded, False, tpTop);

      // Trnasparent Corners
      //TListBox(Control).Canvas.Pixels[R.Left, R.Bottom-2] := clWhite;
      TListBox(Control).Canvas.Pixels[R.Left, R.Bottom-1] := clWhite;
      //TListBox(Control).Canvas.Pixels[R.Left+1, R.Bottom-1] := clWhite;

      //TListBox(Control).Canvas.Pixels[R.Right-2, R.Bottom-1] := clWhite;
      TListBox(Control).Canvas.Pixels[R.Right-1, R.Bottom-1] := clWhite;
      //TListBox(Control).Canvas.Pixels[R.Right-1, R.Bottom-2] := clWhite;
    end;

  end
  else
    TListBox(Control).Canvas.FillRect(Rect);

  TListBox(Control).Canvas.Brush.Style := bsClear;

  if FShowFontGlyph then
  begin
    case aft of
       aftTrueType   : TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FFontGlyphTT);
       aftPostScript : TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FFontGlyphPS);
       aftPrinter    : TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FFontGlyphPRN);
       aftBitmap     : TListBox(Control).Canvas.Draw(Rect.Left + 2, Rect.Top + 2, FFontGlyphBMP);
    end;
    Rect.Left := Rect.Left + 16;
  end;

  Rect.Left := Rect.Left + 4;

  if FDisplayRecentSelection and (Index = FRecentSelection.Count - 1) then
  begin
    Rect.Bottom := Rect.Bottom - 3;
  end;

  TListBox(Control).Canvas.Font.Name := s;
  //TListBox(Control).Canvas.Font.Style := [];

  if not FShowFontStyle or (tm.tmCharSet <> ANSI_CHARSET) then
  begin
    TListBox(Control).Canvas.Font.Name := Font.Name;
    TListBox(Control).Canvas.Font.Size := Font.Size;
  end;

  TListBox(Control).Canvas.Font.Size := FontSize;

  if (TrueTypeFont) then
  begin
    if isItal then
      TListBox(Control).Canvas.Font.Style := [fsItalic];

    if isBold then
      TListBox(Control).Canvas.Font.Style := TListBox(Control).Canvas.Font.Style + [fsBold];

    DrawVistaText(TListBox(Control).Canvas, taLeftJustify, Rect, s, TListBox(Control).Canvas.Font, Enabled, True, FAntiAlias, tpBottom)
  end
  else
  begin
    DrawText(TListBox(Control).Canvas.Handle, PChar(s), -1, Rect, DT_SINGLELINE or DT_VCENTER or DT_NOPREFIX);
  end;

  if FDisplayRecentSelection and (Index = FRecentSelection.Count - 1) then
  begin
    Rect.Bottom := Rect.Bottom + 3;
    TListBox(Control).Canvas.Pen.Color := clGray;
    TListBox(Control).Canvas.MoveTo(1, Rect.Bottom - 3);
    TListBox(Control).Canvas.LineTo(TListBox(Control).Width - 1, Rect.bottom - 3);

    TListBox(Control).Canvas.MoveTo(1, Rect.Bottom - 1);
    TListBox(Control).Canvas.LineTo(TListBox(Control).Width - 1, Rect.bottom - 1);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.Loaded;
begin
  inherited;
  LoadFontNames;
  PopulateListBox;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.LoadFontNames;
var
  i: integer;
begin
  self.Items.Clear;

  for i := 0 to Screen.Fonts.Count - 1 do
  begin
    if (Uppercase(Screen.Fonts.Strings[i]) <> 'DEFAULT') and (Pos('@',Screen.Fonts.Strings[i]) <> 1) then
    begin
      if GetFontType(Screen.Fonts.Strings[i]) <= FAllowedFontTypes then
        self.Items.Add(Screen.Fonts.Strings[i]);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FFontSizeSelector) then
    FFontSizeSelector := nil;
end;

//------------------------------------------------------------------------------
{
procedure TAdvFontSelector.SetDisplayRecentSelection(const Value: Boolean);
begin
  FDisplayRecentSelection := Value;
end;
}
//------------------------------------------------------------------------------

procedure TAdvOfficeFontSelector.SetFontGlyphTT(const Value: TBitMap);
begin
  FFontGlyphTT.Assign(Value);
end;

procedure TAdvOfficeFontSelector.SetFontGlyphPS(const Value: TBitMap);
begin
  FFontGlyphPS.Assign(Value);
end;

procedure TAdvOfficeFontSelector.SetFontGlyphPRN(const Value: TBitMap);
begin
  FFontGlyphPRN.Assign(Value);
end;

procedure TAdvOfficeFontSelector.SetAllowedFontTypes(
  const Value: TAdvFontTypes);
begin
  FAllowedFontTypes := Value;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
  begin
    LoadFontNames;
  end;
end;

procedure TAdvOfficeFontSelector.SetFontGlyphBMP(const Value: TBitMap);
begin
  FFontGlyphBMP.Assign(Value);
end;

//------------------------------------------------------------------------------
(*
procedure TAdvFontSelector.ValueChanged;
begin
  inherited;
{  if Items.IndexOf(Text) >= 0 then
  begin
    if FRecentSelection.IndexOf(Text) >= 0 then
      FRecentSelection.Delete(FRecentSelection.IndexOf(Text));

    if (FRecentSelection.Count > 0) and (FRecentSelection.Count > DropDownCount - 2) then
      while (FRecentSelection.Count > DropDownCount - 2) do
        FRecentSelection.Delete(0);

    if FRecentSelection.IndexOf(Text) < 0 then
      FRecentSelection.Add(Text);
  end;  }
end;
*)
//------------------------------------------------------------------------------

{ TAdvOfficeSelector }

constructor TAdvOfficeSelector.Create(aOwner: TComponent);
begin
  inherited;
  AutoItemSize := false;
end;

//------------------------------------------------------------------------------

function EnumFontSizes(var EnumLogFont: TEnumLogFont;
  PTextMetric: PNewTextMetric; FontType: Integer; Data: LPARAM): Integer;  export; stdcall;
var
  s: String;
  i,v,v2: Integer;
begin
  if (FontType and TRUETYPE_FONTTYPE) <> 0 then
  begin
    TAdvOfficeFontSizeSelector(Data).Items.Add('8');
    TAdvOfficeFontSizeSelector(Data).Items.Add('9');
    TAdvOfficeFontSizeSelector(Data).Items.Add('10');
    TAdvOfficeFontSizeSelector(Data).Items.Add('11');
    TAdvOfficeFontSizeSelector(Data).Items.Add('12');
    TAdvOfficeFontSizeSelector(Data).Items.Add('14');
    TAdvOfficeFontSizeSelector(Data).Items.Add('16');
    TAdvOfficeFontSizeSelector(Data).Items.Add('18');
    TAdvOfficeFontSizeSelector(Data).Items.Add('20');
    TAdvOfficeFontSizeSelector(Data).Items.Add('22');
    TAdvOfficeFontSizeSelector(Data).Items.Add('24');
    TAdvOfficeFontSizeSelector(Data).Items.Add('26');
    TAdvOfficeFontSizeSelector(Data).Items.Add('28');
    TAdvOfficeFontSizeSelector(Data).Items.Add('36');
    TAdvOfficeFontSizeSelector(Data).Items.Add('48');
    TAdvOfficeFontSizeSelector(Data).Items.Add('72');
    Result := 0;
    end
  else
  begin
    v := Round((EnumLogFont.elfLogFont.lfHeight - PTextMetric.tmInternalLeading)*72 /
           TAdvOfficeFontSizeSelector(Data).PixelsPerInch);
    s := IntToStr(v);
    Result := 1;
    for i := 0 to TAdvOfficeFontSizeSelector(Data).Items.Count - 1 do
    begin
      v2 := StrToInt(TAdvOfficeFontSizeSelector(Data).Items[i]);
      if v2 = v then
        Exit;
      if v2 > v then
      begin
        TAdvOfficeFontSizeSelector(Data).Items.Insert(i,s);
        Exit;
      end;
    end;
    TAdvOfficeFontSizeSelector(Data).Items.Add(s);
  end;
end;

{------------------------------------------------------------------------------}

procedure TAdvOfficeFontSizeSelector.Build;
var
  DC: HDC;
  OC: TNotifyEvent;
begin
  DC := GetDC(0);
  Items.BeginUpdate;
  try
    Items.Clear;
    if FontName <> '' then
    begin
      PixelsPerInch := GetDeviceCaps(DC, LOGPIXELSY);
      EnumFontFamilies(DC, PChar(FontName), @EnumFontSizes, LParam(Self));
      OC := OnClick;
      OnClick := nil;
      ItemIndex := Items.IndexOf(Text);
      OnClick := OC;
      if Assigned(OnClick) then
        OnClick(Self);
    end;
  finally
    Items.EndUpdate;
    ReleaseDC(0, DC);
  end;
end;

{------------------------------------------------------------------------------}

procedure TAdvOfficeFontSizeSelector.CMFontChanged(var Message: TMessage);
begin
  inherited;
  FFontSize := Font.Size;
end;

constructor TAdvOfficeFontSizeSelector.Create(aOwner: TComponent);
begin
  inherited;
  Width := 42;
  DisplayRecentSelection := false;
  FontName := 'Tahoma';
  AutoItemSize := true;
  FFontSizeDigits := fs2;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSizeSelector.DoSelect(Index: Integer);
begin
  inherited;

  if Assigned(Action) then
    Action.Execute;

  if Assigned(OnSelectFontSize) then
  begin
    if (ItemIndex >= 0) then
      OnSelectFontSize(Self, StrToInt(self.Items[ItemIndex]))
    else
      OnSelectFontSize(Self, -1);
  end;
end;

//------------------------------------------------------------------------------

function TAdvOfficeFontSizeSelector.GetSelectedFontSize: integer;
begin
  if ItemIndex >= 0 then
    Result := StrToInt(Items[ItemIndex])
  else
    Result := -1;
end;

procedure TAdvOfficeFontSizeSelector.KeyPress(var Ch: char);
var
  fsl: integer;
begin
  inherited;

  fsl := 2;
  if FontSizeDigits = fs3 then
    fsl := 3;


  if (Length(Text) = fsl) and (ch <> #8) and (SelLength = 0) then
  begin
    ch := #0;
    Exit;
  end;

  if not ((ch =#8) or ((ord(ch) >= ord('0')) and (ord(ch) <= ord('9')))) then
    ch := #0;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSizeSelector.SetFontName(const Value: TFontName);
begin
  FFontName := Value;
  Build;
end;

//------------------------------------------------------------------------------

function NumCompare(List: TStringList; Index1, Index2: Integer): Integer;
var
  n1,n2,e: integer;
begin
  val(List.Strings[Index1],n1,e);
  val(List.Strings[Index2],n2,e);

  if n1 > n2 then
    Result := 1
  else
    if n1 = n2 then
      Result := 0
    else
      Result := -1;
end;

//------------------------------------------------------------------------------

procedure TAdvOfficeFontSizeSelector.SetSelectedFontSize(const Value: integer);
var
  idx: integer;
begin
  idx := Items.IndexOf(IntToStr(Value));

  if idx >= 0 then
    ItemIndex := idx
  else
  begin
    Items.Add(IntToStr(Value));
    items.CustomSort(@NumCompare);
    ItemIndex := Items.IndexOf(IntToStr(Value));
  end;
end;

//------------------------------------------------------------------------------

{ TSelectionAppearance }

procedure TSelectionAppearance.Assign(Source: TPersistent);
begin
  inherited;
  if (Source is TSelectionAppearance) then
  begin
    FColor := TSelectionAppearance(Source).Color;
    FColorTo := TSelectionAppearance(Source).ColorTo;
    FColorMirror := TSelectionAppearance(Source).ColorMirror;
    FColorMirrorTo := TSelectionAppearance(Source).ColorMirrorTo;

    FBorderColor := TSelectionAppearance(Source).BorderColor;

    FGradient := TSelectionAppearance(Source).Gradient;
    FGradientMirror := TSelectionAppearance(Source).GradientMirror;

    TextColor := TSelectionAppearance(Source).TextColor;
    Rounded := TSelectionAppearance(Source).Rounded;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TSelectionAppearance.Create;
begin
  inherited;
  Color := clWhite;
  ColorTo := clWhite;
  ColorMirror := clSilver;
  ColorMirrorTo := clWhite;

  BorderColor := clSilver;

  Gradient := ggVertical;
  GradientMirror := ggVertical;

  FTextColor := clHighLightText;
  FRounded := False;
end;

//------------------------------------------------------------------------------

procedure TSelectionAppearance.SetTextColor(const Value: TColor);
begin
  if (FTextColor <> Value) then
  begin
    FTextColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TOfficeListBox }

procedure TOfficeListBox.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS;
end;

//------------------------------------------------------------------------------

{ TAdvOfficeComboBoxActionLink }

procedure TAdvOfficeComboBoxActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvCustomOfficeComboBox;
end;

procedure TAdvOfficeComboBoxActionLink.UpdateText(AValue: string);
begin
  if not FClient.Focused then
    FClient.Text := AValue;
end;

end.

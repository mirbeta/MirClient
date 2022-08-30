{***************************************************************************}
{ TAdvDropdown components                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2010 - 2015                                        }
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

{$I TMSDEFS.INC}

unit AdvDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, ExtCtrls, SysUtils, ImgList,
  Forms, Math, Mask, Dialogs, Buttons, StdCtrls, AsgHTMLE, AdvUtil, CommCtrl,
  ComCtrls, Clipbrd, AdvStyleIF, ATXPVS
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes, System.Types
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : issue with TAdvMemoDropDown and 'A' key handling
  //          : Fixed : issue with Time separators in TAdvTimePickerDropDown
  // v1.0.1.0 : New : Exposed AdvMultiColumnDropDown.DropDownRowHeight: integer

  // v1.0.2.0 : New : Exposed method AddDefaultColors in TAdvColorPickerDropDown
  //          : New : Public property ImageIndex added to set selected image of TAdvImagePickerDropDown directly as imageindex instead of itemindex
  //          : Improved : drawing of selected item when BorderStyle = bsNone
  //          : Improved : color styles
  // v1.0.2.1 : Fixed : selection drawing issue when DropDownButtonWidth was non default value
  // v1.0.2.2 : Fixed : issue with use on forms with WS_EX_APPWINDOW style
  // v1.0.2.3 : Fixed : issue with TDBAdvColorPickerDropDown
  //          : Improved : painting of selected color value in color picker
  //          : Improved : painting of control on Windows 7
  // v1.0.2.4 : Fixed : painting issue with Ctl3D = false
  // v1.0.2.5 : Fixed : issue with affecting parent form z-order
  // v1.0.3.0 : New : ShowThousandSeparator added in TAdvCalculatorDropDown
  // v1.0.3.1 : Fixed : issue with changing ColorSelectionStyle at design time
  // v1.0.4.0 : New : cvtHTML text display in color picker
  //          : Improved : painting of color picker
  // v1.0.5.0 : Improved : ensuring dropdown is always entirely on screen when displayed
  // v1.0.5.1 : Fixed : Issue with editing time directly in inplace editor
  // v1.0.6.0 : Fixed : Issue with default key handling
  //          : New : shortcut key a/p for toggling AM/PM in TAdvTimePickerDropDown
  //          : Fixed : Setting ItemIndex at design time issue with TAdvDetailDropDown
  // v1.0.6.1 : Fixed : Paint issue with dropdown button when setting control readonly
  // v1.0.7.0 : New : OnBeforeDropDown event added
  //          : Fixed : Issue with setting DropDownHeight at runtime
  // v1.0.7.1 : Fixed : Issue with clear & refill TAdvMultiColumnDropDown
  // v1.0.8.0 : New : Added mousewheel support for TAdvDetailDropDown
  //          : New : OnBeforeDropUp event added
  // v1.1.0.0 : New : Built in support for Office 2010 colors
  // v1.1.1.0 : Improved : support for DB blob fields in TDBAdvMemoDropDown
  // v1.1.1.1 : Fixed : Issue with showing dropdown control in older Delphi versions
  // v1.1.1.2 : Fixed : Issue with using dropdown controls on dropdown controls
  // v1.1.1.3 : Fixed : Issue with AdvMultiColumnDropDown when dropdown without selection
  // v1.1.1.4 : Fixed : Issue with setting Enabled = false and updating button
  // v1.1.2.0 : New : Items.IndexInColumn function added in TAdvMultiColumnDropDown
  //          : Fixed : Issue with close & reopen TAdvMultiColumnDropDown without selecting item
  // v1.1.2.1 : Fixed : Issue with setting ItemIndex = -1 in TAdvDetailDropDown
  // v1.2.0.0 : New : Attached label
  //          : New : FocusBorderColor, BorderColor properties added
  // v1.2.1.0 : New : ItemAppearance.BorderColorHot, ItemAppearance.BorderColorSelected added
  // v1.2.1.1 : Fixed : Issue with runtime Visible change & attached label
  // v1.2.1.2 : Improved : Handling of memo fields in TDBAdvDropDownMemo
  // v1.2.1.3 : Improved : Handling of selection in TAdvMultiColumnDropDown
  // v1.2.1.4 : Improved : Handling time updates from dropdown footer buttons in TAdvTimePickerDropDown
  // v1.2.2.0 : New: Property DropDownEnabled added
  // v1.3.0.0 : New : Event OnGetDropDownPos added
  // v1.3.0.1 : Fixed : Issue with initial key lookup in TAdvColorPickerDropDown
  // v1.3.1.0 : New : Exposed CharCase property
  // v1.3.1.1 : Fixed : Issue with TAdvMemoDropDown with EditorEnabled = false
  // v1.3.1.2 : Improved : DropDownButtonGlyph centering in the dropdown button
  // v1.3.2.0 : New : Exposed Alignment property
  // v1.3.2.1 : Fixed : Issue with ReadOnly in TAdvMultiColumnDropDown
  // v1.3.2.2 : Improved : Handling of signed numeric handling in TAdvCalculatorDropDown
  // v1.3.2.3 : Fixed : Issue with handling ESC in TAdvMemoDropDown
  // v1.3.2.4 : Fixed : Issue with toggling color selector style at runtime
  // v1.3.2.5 : Improved : Dealing with programmatically showing the dropdown from focus related events
  // v1.3.3.0 : New : Handling of empty value entry for numeric fields in TDBAdvMultiColumnDropDown
  // v1.3.4.0 : New : Public property DropDownSize: TSize added to retrieve size of last opened dropdown
  // v1.3.4.1 : Fixed : Issue with click on partial visible row in TAdvMultiColumnDropDown
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added
  // v1.4.0.1 : Fixed : Issue with ESC key handling in TAdvMultiColumnDropDown
  // v1.4.0.2 : Fixed : Visual issue when BevelKind = bkFlat and BorderStyle = bsNone
  //          : Fixed : Issue when showing color picker dropdown via pressing F2 or return
  // v1.4.0.3 : Improved : Behavior in TAdvMultiColumnDropdown lookup
  // v1.4.1.0 : New : Vertical image alignment control for TAdvDetailDropDown
  // v1.4.1.1 : Fixed : Regression with lookup in TAdvMultiColumnDropDown


  DD_HEADERHEIGHT = 22;
  DD_FOOTERHEIGHT = 22;
  DD_DROPDOWNBUTTONWIDTH = 17;
  DD_DROPDOWNBUTTONHEIGHT = 17;
  DD_IMAGEXOFFSET = 4;
  DD_IMAGEGAP = 4;
  GRIP_SIZE = 15;
  DD_COLORBOXHEIGHT = 14;
  DD_COLORBOXWIDTH = 14;
  DD_BUTTONOFFSET_X = 4;
  DD_BUTTONOFFSET_Y = 2;
  DD_MINHEIGHT = 50;
  DD_MINWIDTH = 80;

  ErrText = 'Error';

type
  TAdvCustomDropDown = class;
  TAdvCustomItemSelector = class;
  TItemAppearance = class;

  TDropPosition = (dpAuto, dpDown, dpUp);
  TNumGlyphs = Buttons.TNumGlyphs;
  TButtonStyle = (bsButton, bsDropDown);
  TGradientDirection = (gdHorizontal, gdVertical);
  TAdvButtonStyle = (tasButton, tasCheck);
  TAdvButtonState = (absUp, absDisabled, absDown, absDropDown, absExclusive);
  TSelectionColorStyle = (scCustom, scOffice2007, scWindowsVista, scWindows7);
  TSelectorType = (stImage, stColor);
  TItemLayout = (ilCaptionLeft, ilCaptionRight, ilCaptionTop, ilCaptionBottom);
  TDropDownEditType = (etString, etNumeric, etFloat, etUppercase, etMixedCase, etLowerCase,
    etPassword, etMoney, etRange, etHex, etAlphaNumeric, etValidChars);
  TAutoType = (atNumeric, atFloat, atString, atDate, atTime, atHex);

  TDrawBackGroundEvent = procedure (Sender: TObject; ACanvas: TCanvas ; ARect: TRect) of object; //OnDrawHeader, OnDrawFooter
  TGetTextEvent = procedure (Sender: TObject; var Text: string) of object; //OnGetHeaderText, OnGetFooterText
  TDropDown = procedure(Sender: TObject; var acceptdrop: boolean) of object;
  //cancelled = true ignores SelecteItem and stores Old Edit caption
  //cancelled = false on selection and true when Cancel (key=Esc, click outside of control)
  TDropUp = procedure(Sender: TObject; Cancelled: boolean) of object;
  TClipboardEvent = procedure(Sender: TObject; value: string; var allow: Boolean) of object;
  TDropDownButtonItemClick = procedure(Sender: TObject; ButtonIndex: Integer) of object;
  TOnDrawSelectedItem = procedure(Sender: TObject; Canvas: TCanvas; R: TRect) of object;
  TDrawItemEvent = procedure(Sender: TObject; Canvas: TCanvas; R: TRect; Index: Integer) of object;
  TItemSizeEvent = procedure(Sender: TObject; var ASize: TSize) of object;
  TGetDropDownPosEvent = procedure(Sender: TObject; var Pos: TPoint) of object;

  TDbgList = class(TList)
  private
    function GetItemsEx(Index: Integer): Pointer;
    procedure SetItemsEx(Index: Integer; const Value: Pointer);
  public
    procedure AssignList(ListA: TList);
    property Items[Index: Integer]: Pointer read GetItemsEx write SetItemsEx; default;
  end;

  TAdvButtonAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBorderColorHot: TColor;
    FBorderColorDown: TColor;
    FBorderColorDisabled: TColor;
    FBorderColorChecked: TColor;
    FColor: TColor;
    FColorTo: TColor;
    FColorDown: TColor;
    FColorDownTo: TColor;
    FColorHot: TColor;
    FColorHotTo: TColor;
    FColorCheckedTo: TColor;
    FColorDisabled: TColor;
    FColorDisabledTo: TColor;
    FColorChecked: TColor;
    FGradient: TGradientDirection;
    FFont: TFont;
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderColorChecked(const Value: TColor);
    procedure SetBorderColorDisabled(const Value: TColor);
    procedure SetBorderColorDown(const Value: TColor);
    procedure SetBorderColorHot(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetColorChecked(const Value: TColor);
    procedure SetColorCheckedTo(const Value: TColor);
    procedure SetColorDisabled(const Value: TColor);
    procedure SetColorDisabledTo(const Value: TColor);
    procedure SetColorDown(const Value: TColor);
    procedure SetColorDownTo(const Value: TColor);
    procedure SetColorHot(const Value: TColor);
    procedure SetColorHotTo(const Value: TColor);
    procedure SetColorTo(const Value: TColor);
    procedure SetGradient(const Value: TGradientDirection);
    procedure SetFont(const Value: TFont);
  protected
    procedure Changed;
    //--- make these properties published when button exposes Down/GroupIndex properties
    property BorderColorChecked: TColor read FBorderColorChecked write SetBorderColorChecked default clBlue;
    property ColorChecked: TColor read FColorChecked write SetColorChecked;
    property ColorCheckedTo: TColor read FColorCheckedTo write SetColorCheckedTo;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property BorderColorHot: TColor read FBorderColorHot write SetBorderColorHot default clGray;
    property BorderColorDown: TColor read FBorderColorDown write SetBorderColorDown default clNavy;
    property BorderColorDisabled: TColor read FBorderColorDisabled write SetBorderColorDisabled default clGray;
    property Color: TColor read FColor write SetColor default clNone;
    property ColorTo: TColor read FColorTo write SetColorTo default clNone;
    property ColorDisabled: TColor read FColorDisabled write SetColorDisabled default $00F2F2F2;
    property ColorDisabledTo: TColor read FColorDisabledTo write SetColorDisabledTo default clNone;
    property ColorDown: TColor read FColorDown write SetColorDown default $00F5D8CA;
    property ColorDownTo: TColor read FColorDownTo write SetColorDownTo default $00F9BDA0;
    property ColorHot: TColor read FColorHot write SetColorHot default $F5F0E1;
    property ColorHotTo: TColor read FColorHotTo write SetColorHotTo default $F9D2B2;
    property Font: TFont read FFont write SetFont;
    property Gradient: TGradientDirection read FGradient write SetGradient default gdVertical;
  end;

  TAdvButton = class(TCustomControl)
  private
    FGroupIndex: Integer;
    FDown: Boolean;
    FAllowAllUp: Boolean;
    FOffSet: integer;
    FMouseInControl: Boolean;
    FHot: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FStyle: TAdvButtonStyle;
    FState: TAdvButtonState;
    FMouseDownInControl: Boolean;
    FGrouped: Boolean;
    FDragging: Boolean;
    FPropHot: Boolean;
    FUnHotTimer: TTimer;
    FInitialDown: Boolean;
    FInternalClick: Boolean;
    FAppearance: TAdvButtonAppearance;
    FImages: TCustomImageList;
    FImageIndex: Integer;
    FModalResult: TModalResult;
    FAdvDropDown: TAdvCustomDropDown;
    procedure UnHotTimerOnTime(Sender: TObject);
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure ButtonDown;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetStyle(const Value: TAdvButtonStyle);
    procedure SetState(const Value: TAdvButtonState);
    procedure SetGrouped(const Value: Boolean);
    function GetHot: Boolean;
    procedure SetHot(const Value: Boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetAdvDropDown(const Value: TAdvCustomDropDown);
  protected
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawButton(ACanvas: TCanvas); virtual;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure SetAutoSizeEx;

    procedure InvalidateMe;
    property MouseInControl: Boolean read FMouseInControl;
    property State: TAdvButtonState read FState write SetState;

    // published
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Appearance: TAdvButtonAppearance read FAppearance write FAppearance;   // internal property, never publish it
    property Anchors;
    property BiDiMode;

    property Constraints;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Enabled;
    property Font;
    property Hot: Boolean read GetHot write SetHot default false;
    property Images: TCustomImageList read FImages write SetImages;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property AdvDropDown: TAdvCustomDropDown read FAdvDropDown write SetAdvDropDown;

    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Style: TAdvButtonStyle read FStyle write SetStyle default tasButton;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
  end;

  THeaderButton = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetCaption(const Value: TCaption);
    procedure SerHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure PropChanged;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property Width: Integer read FWidth write SetWidth default 0;  // 0 = AutoWidth
    property Height: Integer read FHeight write SerHeight default 0; // 0 = AutoHeight
  end;

  THeaderButtons = class(TOwnedCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): THeaderButton;
    procedure SetItem(Index: Integer; const Value: THeaderButton);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: THeaderButton read GetItem write SetItem; default;
    function Add: THeaderButton;
    function Insert(Index: Integer): THeaderButton;
    //function GetOwner: TPersistent; override;
    //{$IFNDEF DELPHI6_LVL}
    //property Owner: TPersistent read FMyOwner;
    //{$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TButtonAlignment = (baLeft, baRight);


  THeaderAppearance = class(TPersistent)
  private
    FOwner: TPersistent;
    FVisible: Boolean;
    FHeight: Integer;
    FBorderWidth: integer;
    FImageIndex: integer;
    FCaption: string;
    FColorTo: TColor;
    FColor: TColor;
    FBorderColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FButtons: THeaderButtons;
    FGradientDirection: TGradientDirection;
    FButtonAlignment: TButtonAlignment;
    FOnButtonsChanged: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetButtons(const Value: THeaderButtons);
    procedure OnButtonChanged(Sender: TObject);
    procedure SetVisible(const Value: Boolean);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnButtonsChanged: TNotifyEvent read FOnButtonsChanged write FOnButtonsChanged;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignColors(Source: TPersistent);
  published
    property Color: TColor read FColor write FColor default clSilver;
    property ColorTo: TColor read FColorTo write FColorTo default clWhite;
    property Caption: string read FCaption write FCaption; //(supporting HTML formatted text)
    property Font: TFont read FFont write SetFont;
    property GradientDirection: TGradientDirection read FGradientDirection write FGradientDirection default gdVertical;
    property Visible: Boolean read FVisible write SetVisible;
    property Height: Integer read FHeight write FHeight default DD_HEADERHEIGHT;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBlack; // color of line under header
    property BorderWidth: integer read FBorderWidth write FBorderWidth default 1; //width of line under header
    property ImageIndex: integer read FImageIndex write FImageIndex default -1; //property to get index of image to show optionally before text
    property Buttons: THeaderButtons read FButtons write SetButtons;
    property ButtonAlignment: TButtonAlignment read FButtonAlignment write FButtonAlignment default baRight;
  end;


  TAdvHeader = class(TCustomControl)
  private
    FAppearance: THeaderAppearance;
    FImages: TCustomImageList;
    FAdvDropDown: TAdvCustomDropDown;
    FButtonList: TDbgList;
    FOnDrawBackGround: TDrawBackGroundEvent;
    FOnGetText: TGetTextEvent;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetAppearance(const Value: THeaderAppearance);
    procedure AppearanceChanged(Sender: TObject);
    procedure OnButtonsChanged(Sender: TObject);
    procedure OnButtonClick(Sender: TObject);
    function GetButtons: THeaderButtons;
    procedure SetAdvDropDown(const Value: TAdvCustomDropDown);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure CreateButtons; virtual;
    procedure DestroyButtons; virtual;
    procedure ArrangeButtons; virtual;
    procedure UpdateButtons; virtual;
    function GetButtonsRect: TRect;
    function GetMaxButtonHeight: Integer;
    procedure Initialize;
    function GetButtonsCoveredRect: TRect;

    property AdvDropDown: TAdvCustomDropDown read FAdvDropDown write SetAdvDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Update; override;
  published
    property Appearance: THeaderAppearance read FAppearance write SetAppearance;
    property Images: TCustomImageList read FImages write SetImages;
    property Buttons: THeaderButtons read GetButtons;

    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    {$IFDEF DELPHI2006_LVL}
    property ParentBackground;
    property ParentCtl3D;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnDrawBackGround: TDrawBackGroundEvent read FOnDrawBackGround write FOnDrawBackGround;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
  end;

  TFooterButton = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FModalResult: TModalResult;
    FEnabled: Boolean;
    FHeight: Integer;
    FWidth: Integer;
    procedure SetCaption(const Value: TCaption);
    procedure SerHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetEnabled(const Value: Boolean);
  protected
    procedure PropChanged;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;
    property Enabled: Boolean read FEnabled write SetEnabled default True;
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1;
    property ModalResult: TModalResult read FModalResult write FModalResult default mrNone;
    property Width: Integer read FWidth write SetWidth default 0;  // 0 = AutoWidth
    property Height: Integer read FHeight write SerHeight default 0; // 0 = AutoHeight
  end;

  TFooterButtons = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TFooterButton;
    procedure SetItem(Index: Integer; const Value: TFooterButton);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TFooterButton read GetItem write SetItem; default;
    function Add: TFooterButton;
    function Insert(Index: Integer): TFooterButton;
    function GetOwner: TPersistent; override;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TFooterAppearance = class(TPersistent)
  private
    FOwner: TPersistent;
    FVisible: Boolean;
    FHeight: Integer;
    FBorderWidth: integer;
    FSizeGrip: Boolean;
    FCaption: string;
    FColorTo: TColor;
    FColor: TColor;
    FBorderColor: TColor;
    FFont: TFont;
    FOnChange: TNotifyEvent;
    FImageIndex: Integer;
    FButtons: TFooterButtons;
    FGradientDirection: TGradientDirection;
    FButtonAlignment: TButtonAlignment;
    FOnButtonsChanged: TNotifyEvent;
    procedure SetFont(const Value: TFont);
    procedure SetButtons(const Value: TFooterButtons);
    procedure SetVisible(const Value: Boolean);
    procedure OnButtonsChange(Sender: TObject);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnButtonsChanged: TNotifyEvent read FOnButtonsChanged write FOnButtonsChanged;
  public
    constructor Create(AOwner: TPersistent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignColors(Source: TPersistent);
  published
    property Color: TColor read FColor write FColor default clSilver;
    property ColorTo: TColor read FColorTo write FColorTo default clWhite;
    property Caption: string read FCaption write FCaption; //(supporting HTML formatted text)
    property Font: TFont read FFont write SetFont;
    property GradientDirection: TGradientDirection read FGradientDirection write FGradientDirection default gdVertical;
    property Visible: Boolean read FVisible write SetVisible;
    property Height: Integer read FHeight write FHeight default DD_FOOTERHEIGHT;
    property BorderColor: TColor read FBorderColor write FBorderColor default clBlack; // color of line above footer
    property BorderWidth: Integer read FBorderWidth write FBorderWidth default 1; //width of line above footer
    property ImageIndex: Integer read FImageIndex write FImageIndex default -1; //property to get index of image to show optionally before text
    property SizeGrip: Boolean read FSizeGrip write FSizeGrip default True;
    property Buttons: TFooterButtons read FButtons write SetButtons;
    property ButtonAlignment: TButtonAlignment read FButtonAlignment write FButtonAlignment default baRight;
  end;

  TAdvFooter = class(TCustomControl)
  private
    FAppearance: TFooterAppearance;
    FImages: TCustomImageList;
    FAdvDropDown: TAdvCustomDropDown;
    FButtonList: TDbgList;
    FOnDrawBackGround: TDrawBackGroundEvent;
    FOnGetText: TGetTextEvent;
    FOldCursor: TCursor;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMNCHitTest(var Msg: TWMNCHitTest); message WM_NCHITTEST;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetImages(const Value: TCustomImageList);
    procedure SetAppearance(const Value: TFooterAppearance);
    procedure AppearanceChanged(Sender: TObject);
    procedure OnButtonClick(Sender: TObject);
    function GetButtons: TFooterButtons;
    procedure SetAdvDropDown(const Value: TAdvCustomDropDown);
    function HasSizeGrip: Boolean;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure DrawSizeGrip(R: TRect);
    procedure CreateButtons; virtual;
    procedure DestroyButtons; virtual;
    procedure ArrangeButtons; virtual;
    procedure UpdateButtons; virtual;
    function GetButtonsRect: TRect;
    function GetMaxButtonHeight: Integer;
    procedure Initialize;
    function GetButtonsCoveredRect: TRect;

    property AdvDropDown: TAdvCustomDropDown read FAdvDropDown write SetAdvDropDown;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Appearance: TFooterAppearance read FAppearance write SetAppearance;
    property Images: TCustomImageList read FImages write SetImages;
    property Buttons: TFooterButtons read GetButtons;

    property Align;
    property Anchors;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderWidth;
    property Constraints;
    property Ctl3D;
    property UseDockManager default True;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ParentBiDiMode;
    {$IFDEF DELPHI2006_LVL}
    property ParentBackground;
    property ParentCtl3D;
    {$ENDIF}
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnCanResize;
    property OnClick;
    property OnConstrainedResize;
    property OnContextPopup;
    property OnDockDrop;
    property OnDockOver;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnDrawBackGround: TDrawBackGroundEvent read FOnDrawBackGround write FOnDrawBackGround;
    property OnGetText: TGetTextEvent read FOnGetText write FOnGetText;
  end;

  TDropDownForm = class(TForm)
  private
    FDeActivate: DWORD;
    FShadow: Boolean;
    FFooter: TAdvFooter;
    FHeader: TAdvHeader;
    FAdvDropDown: TAdvCustomDropDown;
    FScrollBox: TScrollBox;
    FSizeable: Boolean;
    FCancelOnDeActivate: Boolean;
    FOnSizing: TNotifyEvent;
    FBlockActivate: boolean;
    FIsDeactivating: boolean;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMSizing(var Message: TWMSize); message WM_SIZING;
    procedure WMClose(var Msg: TMessage); message WM_CLOSE;
    procedure WMActivate(var Message: TWMActivate); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    function GetParentWnd: HWnd;
    procedure SetAdvDropDown(const Value: TAdvCustomDropDown);
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure Paint; override;
    procedure DrawBackGround(aCanvas: TCanvas);
    function GetClientRect: TRect; override;
    procedure AdjustClientRect(var Rect: TRect); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    constructor CreateNew(AOwner: TComponent; Dummy: Integer = 0); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure UpdateSize; virtual;
    procedure InitializeAndUpdate;

    property IsDeactivating: boolean read FIsDeactivating write FIsDeactivating;
    property BlockActivate: boolean read FBlockActivate write FBlockActivate;
    property DeActivateTime: DWORD read FDeActivate;
    property ScrollBox: TScrollBox read FScrollBox;
    property Sizeable: Boolean read FSizeable write FSizeable;

    property Shadow: Boolean read FShadow write FShadow;
    property Header: TAdvHeader read FHeader;
    property Footer: TAdvFooter read FFooter;
    property CancelOnDeActivate: Boolean read FCancelOnDeActivate write FCancelOnDeActivate default True;
    property AdvDropDown: TAdvCustomDropDown read FAdvDropDown write SetAdvDropDown;
    property OnSizing: TNotifyEvent read FOnSizing write FOnSizing;
  end;

  { TAdvDropDownSpeedButton }
  TAdvDropDownSpeedButton = class(TSpeedButton)
  private
    FEtched: Boolean;
    FFocused: Boolean;
    FHot: Boolean;
    FUp: Boolean;
    FIsWinXP: Boolean;
    FButtonStyle: TButtonStyle;
    procedure SetEtched(const Value: Boolean);
    procedure SetFocused(const Value: Boolean);
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure PaintDropDown;
    procedure PaintButton;
    procedure SetButtonStyle(const Value: TButtonStyle);
  protected
    procedure Paint; override;
    function DoVisualStyles: Boolean;
  public
    procedure SetUp;
    constructor Create(AOwner: TComponent); override;
  published
    property ButtonStyle: TButtonStyle read FButtonStyle write SetButtonStyle;
    property Etched: boolean read FEtched write SetEtched;
    property Focused: boolean read FFocused write SetFocused;
  end;

  { TDropDownEditButton }
  TDropDownEditButton = class(TWinControl)
  private
    FButton: TAdvDropDownSpeedButton;
    FFocusControl: TWinControl;
    FOnClick: TNotifyEvent;
    FBWidth: Integer;
    FButtonColorDown: TColor;
    FButtonBorderColor: TColor;
    FButtonTextColor: TColor;
    FButtonTextColorHot: TColor;
    FButtonColor: TColor;
    FButtonColorHot: TColor;
    FButtonTextColorDown: TColor;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    function CreateButton: TAdvDropDownSpeedButton;
    function GetGlyph: TBitmap;
    procedure SetGlyph(Value: TBitmap);
    function GetNumGlyphs: TNumGlyphs;
    procedure SetNumGlyphs(Value: TNumGlyphs);
    procedure SetCaption(value:string);
    function GetCaption:string;
    procedure BtnClick(Sender: TObject);
    procedure BtnMouseDown (Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure AdjustWinSize (var W: Integer; var H: Integer);
    procedure WMSize(var Message: TWMSize);  message WM_SIZE;
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    property BWidth: Integer read fBWidth write fBWidth;
    procedure Setup;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    property ButtonColor: TColor read FButtonColor write FButtonColor default clNone;
    property ButtonColorHot: TColor read FButtonColorHot write FButtonColorHot default clNone;
    property ButtonColorDown: TColor read FButtonColorDown write FButtonColorDown default clNone;
    property ButtonTextColor: TColor read FButtonTextColor write FButtonTextColor default clNone;
    property ButtonTextColorHot: TColor read FButtonTextColorHot write FButtonTextColorHot default clNone;
    property ButtonTextColorDown: TColor read FButtonTextColorDown write FButtonTextColorDown default clNone;
    property ButtonBorderColor: TColor read FButtonBorderColor write FButtonBorderColor default clNone;
  published
    property Align;
    property Ctl3D;
    property Glyph: TBitmap read GetGlyph write SetGlyph;
    property ButtonCaption:string read GetCaption write SetCaption;
    property NumGlyphs: TNumGlyphs read GetNumGlyphs write SetNumGlyphs default 1;
    property DragCursor;
    property DragMode;
    property Enabled;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property ParentCtl3D;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    {$IFDEF WIN32}
    property OnStartDrag;
    {$ENDIF}
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter, lpBottomCenter,
    lpRightTop, lpRightCenter, lpRighBottom, lpTopRight, lpBottomRight);

  TAdvCustomDropDown = class(TCustomMaskEdit, ITMSStyle, ITMSTones)
  private
    { Private declarations }
    FLabel: TLabel;
    FParentFnt: boolean;
    FDropDownWidth: integer;
    FDropDownHeight: integer;
    FUserDropDownWidth: integer;
    FUserDropDownHeight: integer;
    FEditorEnabled: boolean;
    FDropPosition: TDropPosition;
    FOnDropDown: TDropDown;
    FOnDropUP: TDropUp;
    FFlat: Boolean;
    FImages: TCustomImageList;
    FDropDownShadow: Boolean;
    FDropDownBorderWidth: Integer;
    FButtonHint: string;
    FDropDownColorTo: TColor;
    FDropDownColor: TColor;
    FDropDownBorderColor: TColor;
    FButtonWidth: Integer;
    FMouseInControl: Boolean;
    FEtched: Boolean;
    FFocusControl: TWinControl;
    FDropDownHeader: THeaderAppearance;
    FButtonAppearance: TAdvButtonAppearance;
    FDropDownFooter: TFooterAppearance;
    FControl: TControl;
    FDropDownGradient: TGradientDirection;
    FDropDownSizeable: Boolean;
    FSelectionColor: TColor;
    FSelectionColorTo: TColor;
    FSelectionColorStyle: TSelectionColorStyle;
    FEditType: TDropDownEditType;
    FLengthLimit: SmallInt;
    FPrecision: SmallInt;
    FPrefix: string;
    FSuffix: string;
    FOldString: string;
    FSigned: Boolean;
    FReturnIsTab: Boolean;
    FIsWinXP: Boolean;
    FIsThemed: Boolean;
    FBlockChange: Boolean;
    FAllowNumericNullValue: Boolean;
    FDefaultHandling: Boolean;
    FCanUndo: boolean;
    FExcelStyleDecimalSeparator: boolean;
    FValidChars: string;
    FBlockCopy: Boolean;
    FDoShowBusy: Boolean;
    FOnClipboardCopy: TClipboardEvent;
    FOnClipboardPaste: TClipboardEvent;
    FOnClipboardCut: TClipboardEvent;
    FButtonDown: Boolean;
    FAutoThousandSeparator: Boolean;
    FIsModified: Boolean;
    FEditRect: TRect;
    FDropDownEnabled: Boolean;
    FOnDropDownFooterButtonClick: TDropDownButtonItemClick;
    FOnDropDownHeaderButtonClick: TDropDownButtonItemClick;
    FOnDrawHeader: TDrawBackGroundEvent;
    FOnDrawFooter: TDrawBackGroundEvent;
    FOnGetFooterText: TGetTextEvent;
    FOnGetHeaderText: TGetTextEvent;
    FDroppedDown: Boolean;
    FFocusDraw: Boolean;
    FForceShadow: Boolean;
    FTMSStyle: TTMSStyle;
    FOnBeforeDropDown: TNotifyEvent;
    FOnBeforeDropUp: TNotifyEvent;
    FLabelMargin: Integer;
    FLabelPosition: TLabelPosition;
    FLabelAlwaysEnabled: Boolean;
    FLabelTransparent: Boolean;
    FLabelFont: TFont;
    FOnLabelDblClick: TNotifyEvent;
    FOnLabelClick: TNotifyEvent;
    FBorderColor: TColor;
    FFocusBorderColor: TColor;
    FDisabledBorder: boolean;
    FOnGetDropDownPos: TGetDropDownPosEvent;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMVisibleChanged(var Message: TMessage); message CM_VISIBLECHANGED;
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
    procedure CMEnter(var Message: TCMGotFocus); message CM_ENTER;
    procedure CMExit(var Message: TCMExit);   message CM_EXIT;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMKeyDown(var Msg: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message: TWMPaste); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure OnFooterAppearanceChanged(Sender: TObject);
    procedure OnHeaderAppearanceChanged(Sender: TObject);
    //procedure OnButtonGlyphChanged(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure OnFormHide(Sender: TObject);
    procedure OnFormDestroy(Sender: TObject);
    procedure OnFormKeyPress(Sender: TObject; var Key: Char);
    procedure OnFormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnFormKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnFormMouseWheelDown(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnFormMouseWheelUp(Sender: TObject; Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
    procedure OnControlKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnControlKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnControlKeyPress(Sender: TObject; var Key: Char);
    procedure SetEditorEnabled(const Value: boolean);
    procedure SetImages(const Value: TCustomImageList);
    procedure SetFlat(const Value: Boolean);
    procedure SetVersion(const Value: string);
    function GetButtonGlyph: TBitmap;
    function GetButtonWidth: Integer;
    procedure SetButtonGlyph(const Value: TBitmap);
    procedure SetButtonHint(const Value: string);
    procedure SetButtonWidth(const Value: Integer);
    function GetButtonCaption: string;
    procedure SetButtonCaption(const Value: string);
    procedure SetEtched(const Value : boolean);
    procedure DrawControlBorder(DC:HDC);
    procedure DrawButtonBorder;
    procedure DrawBorders;
    function Is3DBorderControl: Boolean;
    function Is3DBorderButton: Boolean;
    procedure SetDropDownHeader(const Value: THeaderAppearance);
    procedure SetButtonAppearance(const Value: TAdvButtonAppearance);
    procedure SetDropDownFooter(const Value: TFooterAppearance);
    procedure SetControl(const Value: TControl);
    procedure SetSelectionColor(const Value: TColor);
    procedure SetSelectionColorTo(const Value: TColor);
    procedure SetEditType(const Value: TDropDownEditType);
    function GetText: string;
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
    procedure SetPrecision(const Value: smallint);
    function FixedLength(s: string): Integer;
    function AllowMin(ch: char): boolean;
    function DecimalPos: Integer;
    procedure SetFloat(const Value: double);
    procedure SetInt(const Value: integer);
    procedure AutoSeparators;
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    procedure SetCanUndo(const Value: boolean);
    function GetFloat: double;
    function GetInt: Integer;
    function EStrToFloat(s: string): extended;
    function CharFromPos(pt: TPoint): Integer;
    procedure UpdateLookup;
    procedure SetAutoThousandSeparator(const Value: Boolean);
    procedure SetDropDownEnabled(const Value: Boolean);
    procedure SetDropDownColor(const Value: TColor);
    procedure DropDownSizing(Sender: TObject);
    procedure SetDropDownHeight(const Value: Integer);
    procedure SetDropDownWidth(const Value: Integer);
    function GetLabelCaption: string;
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelFont(const Value: TFont);
    procedure SetLabelMargin(const Value: Integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure SetLabelTransparent(const Value: Boolean);
    procedure LabelFontChange(Sender: TObject);
    procedure SetBorderColor(const Value: TColor);
    procedure SetFocusBorderColor(const Value: TColor);
    procedure SetDisabledBorder(const Value: boolean);
  protected
    FDropDownForm: TDropDownForm;
    FButton: TDropDownEditButton;
    FOldText: string;
    procedure CreateWnd; override;
    procedure DestroyWnd; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure SetParent(AParent: TWinControl); override;
    procedure Loaded; override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoEnter; override;
    procedure WndProc(var Message: TMessage); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function PosFromChar(uChar: word): TPoint;
    function CreateLabel: TLabel;
    procedure UpdateLabel;
    procedure LabelClick(Sender: TObject); virtual;
    procedure LabelDblClick(Sender: TObject); virtual;

    function GetVersionEx: string;
    function GetVersionNr: Integer; virtual;

    procedure CreateDropDownForm; virtual;
    procedure CreateDropDownButton; virtual;
    procedure UpdateDropDownButton; virtual;
    procedure BeforeDropDown; virtual;
    procedure ResetDropSize;
    procedure AdaptDropDownSize(var AHeight: integer); virtual;
    procedure AfterDropDown; virtual;
    procedure DoShowDropDown; virtual;
    procedure SetCenterControl; virtual;  // Set control below Header and above footer
    procedure UpdateDropDownSize; virtual;
    procedure DoHideDropDown(Cancelled: Boolean); virtual;
    procedure OnHideDropDown; virtual;
    procedure OnDestroyDropDownForm; virtual;
    procedure OnDropDownFormMouseWheelDown(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure OnDropDownFormMouseWheelUp(Shift: TShiftState; MousePos: TPoint; var Handled: Boolean); virtual;
    procedure OnDropDownFormKeyPress(var Key: Char); virtual;
    procedure OnDropDownFormKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure OnDropDownFormKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); virtual;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); virtual;
    procedure OnDropDownControlKeyPress(var Key: Char); virtual;
    procedure OnDropDownSizing; virtual;
    procedure HandleMouseWheelDown; virtual;
    procedure HandleMouseWheelUp; virtual;
    procedure DrawBackGround; virtual;

    function GetMinHeight: Integer;
    function GetEditRect: TRect;
    procedure SetEditRect; virtual;
    procedure ResizeControl; virtual;
    procedure SetTextDirect(s:string);
    procedure SetText(Value: string); virtual;
    procedure SetSelectionColorStyle(const Value: TSelectionColorStyle); virtual;

    procedure ReadSelectionColorStyle(Reader: TReader);
    procedure WriteSelectionColorStyle(Writer: TWriter);
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);

    procedure MouseButtonDown(Sender: TObject);
    procedure DropDownButtonClick(Sender: TObject);

    procedure DrawSelectionBackground(Canvas: TCanvas; R: TRect; bkgcolor: TColor; part: TGradientPart); virtual;
    procedure DrawGradientBackground(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; GradientDirection: TGradientDirection); virtual;

    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property DisabledBorder: boolean read FDisabledBorder write SetDisabledBorder default True;
    property FocusBorderColor: TColor read FFocusBorderColor write SetFocusBorderColor default clNone;
    property FocusControl: TWinControl read FFocusControl write FFocusControl;
    property FocusDraw: Boolean read FFocusDraw write FFocusDraw default True;
    property ForceShadow: Boolean read FForceShadow write FForceShadow default False;

    property DropDownEnabled: Boolean read FDropDownEnabled write SetDropDownEnabled default true;   // Enable dropdown when EditorEnable = false or Readonly = true;

    property DropDownColor: TColor read FDropDownColor write SetDropDownColor default clWhite;
    property DropDownColorTo: TColor read FDropDownColorTo write FDropDownColorTo default clNone; //when clNone, solid color is used
    property DropDownGradient: TGradientDirection read FDropDownGradient write FDropDownGradient default gdHorizontal;
    property DropDownBorderColor: TColor read FDropDownBorderColor write FDropDownBorderColor default clBlack; //border color
    property DropDownBorderWidth: Integer read FDropDownBorderWidth write FDropDownBorderWidth default 1;
    property DropDownShadow: Boolean read FDropDownShadow write FDropDownShadow default True; //shadow on dropdown
    property DropDownWidth: Integer read FDropDownWidth write SetDropDownWidth default 0; //when 0, same size as control
    property DropDownHeight: Integer read FDropDownHeight write SetDropDownHeight default 0; //when 0, autosize
    property DropDownSizeable: Boolean read FDropDownSizeable write FDropDownSizeable default True;

    property EditType: TDropDownEditType read FEditType write SetEditType default etString;
    property ReturnIsTab: Boolean read fReturnIsTab write FReturnIsTab default False;
    property LengthLimit: smallint read fLengthLimit write FLengthLimit default 0;
    property Precision: smallint read FPrecision write SetPrecision;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property DefaultHandling: Boolean read FDefaultHandling write FDefaultHandling;
    property CanUndo: boolean read FCanUndo write SetCanUndo default True;
    property ExcelStyleDecimalSeparator: boolean read FExcelStyleDecimalSeparator write FExcelStyleDecimalSeparator default False;
    property ValidChars: string read FValidChars write FValidChars;
    property FloatValue: double read GetFloat write SetFloat;
    property IntValue: Integer read GetInt write SetInt;
    property Modified: Boolean read GetModified write SetModified;
    property Signed: Boolean read FSigned write FSigned default False;
    property AutoThousandSeparator: Boolean read FAutoThousandSeparator write SetAutoThousandSeparator default True;

    property Control: TControl read FControl write SetControl;  // DropDown Control

    property ButtonAppearance: TAdvButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property DropDownHeader: THeaderAppearance read FDropDownHeader write SetDropDownHeader;
    property DropDownFooter: TFooterAppearance read FDropDownFooter write SetDropDownFooter;

    property DropPosition: TDropPosition read FDropPosition write FDropPosition default dpAuto;
    property Flat: Boolean read FFlat write SetFlat default False;
    property DropDownButtonWidth: Integer read GetButtonWidth write SetButtonWidth default 17;
    property DropDownButtonHint: string read FButtonHint write SetButtonHint;
    property EditorEnabled: Boolean read FEditorEnabled write SetEditorEnabled default True;
    property DropDownButtonGlyph: TBitmap read GetButtonGlyph write SetButtonGlyph;
    property DropDownButtonCaption:string read GetButtonCaption write SetButtonCaption;
    property Etched: Boolean read FEtched write SetEtched;

    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;

    property Images: TCustomImageList read FImages write SetImages;
    property Version: string read GetVersionEx write SetVersion;

    property SelectionColorStyle: TSelectionColorStyle read FSelectionColorStyle write SetSelectionColorStyle;
    property SelectionColor: TColor read FSelectionColor write SetSelectionColor; //(color used for csCustom style, border color is same as brush color)
    property SelectionColorTo: TColor read FSelectionColorTo write SetSelectionColorTo; //(when clNone, selection color is solid color)

    procedure DefineProperties(Filer: TFiler); override;

    property Text: string read GetText write SetText;

    property OnDropDownHeaderButtonClick: TDropDownButtonItemClick read FOnDropDownHeaderButtonClick write FOnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick: TDropDownButtonItemClick read FOnDropDownFooterButtonClick write FOnDropDownFooterButtonClick;
    property OnDrawHeader: TDrawBackGroundEvent read FOnDrawHeader write FOnDrawHeader;
    property OnDrawFooter: TDrawBackGroundEvent read FOnDrawFooter write FOnDrawFooter;
    property OnGetHeaderText: TGetTextEvent read FOnGetHeaderText write FOnGetHeaderText;
    property OnGetFooterText: TGetTextEvent read FOnGetFooterText write FOnGetFooterText;

    property OnBeforeDropDown: TNotifyEvent read FOnBeforeDropDown write FOnBeforeDropDown;
    property OnDropDown: TDropDown read FOnDropDown write FOnDropDown;
    property OnBeforeDropUp: TNotifyEvent read FOnBeforeDropUp write FOnBeforeDropUp;
    property OnDropUp: TDropUP read FOnDropUP write FOnDropUp;
    property OnClipboardCopy: TClipboardEvent read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardCut: TClipboardEvent read FOnClipboardCut write FOnClipboardCut;
    property OnClipboardPaste: TClipboardEvent read FOnClipboardPaste write FOnClipboardPaste;
    property OnLabelClick: TNotifyEvent read FOnLabelClick write FOnLabelClick;
    property OnLabelDblClick: TNotifyEvent read FOnLabelDblClick write FOnLabelDblClick;
    property OnGetDropDownPos: TGetDropDownPosEvent read FOnGetDropDownPos write FOnGetDropDownPos;

    procedure SetAppearanceStyle(ItemAppearance: TItemAppearance; AStyle: TTMSStyle);
    function GetDropDownSize: TSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetComponentStyle(AStyle: TTMSStyle); virtual;
    procedure SetColorTones(ATones: TColorTones); virtual;
    function GetComponentStyle: TTMSStyle;
    procedure SelectAll;
    procedure SelectBeforeDecimal;
    procedure SelectAfterDecimal;
    procedure ShowDropDown;
    procedure HideDropDown(CancelChanges: Boolean = false);
    property Button: TDropDownEditButton read FButton;
    property DroppedDown: Boolean read FDroppedDown;
    property DropDownSize: TSize read GetDropDownSize;
  end;

  
  TAdvDropDown = class(TAdvCustomDropDown)
  published
    property Align;
    {$IFDEF DELPHIXE_LVL}
    property Alignment;
    {$ENDIF}
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property FocusBorderColor;

    property ImeMode;
    property ImeName;
    property MaxLength;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Visible;

    property DropDownColor;
    property DropDownColorTo;
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropDownEnabled;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownSizeable;
    property Enabled;
    property EditorEnabled;
    property Font;
    property DropDownButtonGlyph;
    property Images;
    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property Version;
    property ReadOnly;
    property Text;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;

    property DragCursor;
    property DragKind;
    property DragMode;
    property TabStop;
    property TabOrder;

    property OnEnter;
    property OnExit;
    property OnChange;
    property OnClick;
    property OnDblClick;

    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseEnter;
    property OnMouseLeave;
    {$ENDIF}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    
    property OnBeforeDropDown;    
    property OnDropDown;
    property OnBeforeDropUp;
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnLabelClick;
    property OnLabelDblClick;
    property OnGetDropDownPos;
  end;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TSelectorItem = class(TCollectionItem)
  private
    FCaption: TCaption;
    FImageIndex: Integer;
    FEnabled: Boolean;
    FColor: TColor;
    FImage: TPicture;
    FRect: TRect;
    FHint: string;
    procedure SetCaption(const Value: TCaption);
    procedure SetColor(const Value: TColor);
    procedure SetImage(const Value: TPicture);
    procedure SetImageIndex(const Value: Integer);
  protected
    procedure Changed;
    property Rect: TRect read FRect write FRect;
    property Enabled: Boolean read FEnabled write FEnabled default True;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Caption: TCaption read FCaption write SetCaption;    // Supporting HTML
    property Color: TColor read FColor write SetColor default clNone;
    property ImageIndex: Integer read FImageIndex write SetImageIndex default -1;
    property Image: TPicture read FImage write SetImage;
    property Hint: string read FHint write FHint;
  end;

  TSelectorItems = class(TCollection)
  private
    FMyOwner: TPersistent;
    FOnChange: TNotifyEvent;
    function GetItem(Index: Integer): TSelectorItem;
    procedure SetItem(Index: Integer; const Value: TSelectorItem);
  protected
    procedure Change;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer]: TSelectorItem read GetItem write SetItem; default;
    function Add: TSelectorItem;
    function Insert(Index: Integer): TSelectorItem;
    function GetOwner: TPersistent; override;
    {$IFNDEF DELPHI6_LVL}
    property Owner: TPersistent read FMyOwner;
    {$ENDIF}
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TItemAppearance = class(TPersistent)
  private
    FOwner: TPersistent;
    FOnChange: TNotifyEvent;
    FColorMirrorHotTo: TColor;
    FBorderColor: TColor;
    FColorMirrorHot: TColor;
    FColorSelectedTo: TColor;
    FColorSelected: TColor;
    FEdgeColor: TColor;
    FColorHotTo: TColor;
    FBorderColorBottom: TColor;
    FColorHot: TColor;
    FBorderColorTop: TColor;
    FColorMirrorSelectedTo: TColor;
    FColorMirrorSelected: TColor;
    FColorSelectedText: TColor;
    FColorHotText: TColor;
    FColorStyle: TSelectionColorStyle;
    FFont: TFont;
    FBorderColorHot: TColor;
    FBorderColorSelected: TColor;
    procedure SetColorStyle(const Value: TSelectionColorStyle);
    procedure SetFont(const Value: TFont);
  protected
    function GetOwner: TPersistent; override;
    procedure Changed;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property ColorStyle: TSelectionColorStyle read FColorStyle write SetColorStyle;
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property BorderColor: TColor read FBorderColor write FBorderColor;
    property BorderColorTop: TColor read FBorderColorTop write FBorderColorTop;
    property BorderColorBottom: TColor read FBorderColorBottom write FBorderColorBottom;
    property BorderColorHot: TColor read FBorderColorHot write FBorderColorHot default clNone;
    property BorderColorSelected: TColor read FBorderColorSelected write FBorderColorSelected default clNone;
    property ColorHot: TColor read FColorHot write FColorHot;
    property ColorHotTo: TColor read FColorHotTo write FColorHotTo;
    property ColorHotText: TColor read FColorHotText write FColorHotText;
    property ColorMirrorHot: TColor read FColorMirrorHot write FColorMirrorHot;
    property ColorMirrorHotTo: TColor read FColorMirrorHotTo write FColorMirrorHotTo;
    property ColorSelected: TColor read FColorSelected write FColorSelected;
    property ColorSelectedTo: TColor read FColorSelectedTo write FColorSelectedTo;
    property ColorMirrorSelected: TColor read FColorMirrorSelected write FColorMirrorSelected;
    property ColorMirrorSelectedTo: TColor read FColorMirrorSelectedTo write FColorMirrorSelectedTo;
    property ColorSelectedText: TColor read FColorSelectedText write FColorSelectedText;
    property EdgeColor: TColor read FEdgeColor write FEdgeColor;
    property Font: TFont read FFont write SetFont;
  end;
  
  TCustomItemSelector = class(TCustomControl)
  private
    FItemIndex: Integer;
    FItemHot: Integer;
    FOffSetX: Integer;  // Control's top/bottom offset
    FOffSetY: Integer;  // Control's Left/Right offset
    FItemOffSetX: Integer;  // Item's top/bottom offset
    FItemOffSetY: Integer;  // Item's Left/Right offset
    FCaptionGap: Integer;
    FAdvItemSelector: TAdvCustomItemSelector;
    FOnItemSelect: TNotifyEvent;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemOffSetX(const Value: Integer);
    procedure SetItemOffSetY(const Value: Integer);
    procedure SetCaptionGap(const Value: Integer);
    procedure SetItemHot(const Value: Integer);

    function GetColumns: Integer;
    function GetItems: TSelectorItems;
    function GetSelectorType: TSelectorType;
    function GetItemLayout: TItemLayout;
    function GetImages: TCustomImageList;
    function GetItemColorStyle: TSelectionColorStyle;
    function GetItemColorHot: TColor;
    function GetItemColorHotTo: TColor;
    function GetItemColorSelected: TColor;
    function GetItemColorSelectedTo: TColor;
    function GetColorBoxHeight: Integer;
    function GetColorBoxWidth: Integer;
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;

    procedure DrawItems(ACanvas: TCanvas);
    procedure DrawItem(Index: Integer; ACanvas: TCanvas); virtual;
    function GetInnerRect: TRect; virtual;
    function GetItemSize: TSize; virtual;
    procedure InvalidateItem(Index: Integer); virtual;
    function ItemAtPos(X, Y: Integer): Integer;
    function GetItemRect(Index: Integer): TRect;
    procedure UpdateRectAndSize; virtual;

    property ItemOffSetX: Integer read FItemOffSetX write SetItemOffSetX default 4;
    property ItemOffSetY: Integer read FItemOffSetY write SetItemOffSetY default 4;
    property CaptionGap: Integer read FCaptionGap write SetCaptionGap default 4;
    property ItemHot: Integer read FItemHot write SetItemHot default -1;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    function LookupItem(s: string): Boolean;
    procedure HotNext;
    procedure HotPrevious;

    property ItemIndex: Integer read FItemIndex write SetItemIndex default -1;

    property Columns: Integer read GetColumns;
    property Items: TSelectorItems read GetItems;
    property SelectorType: TSelectorType read GetSelectorType;
    property ItemLayout: TItemLayout read GetItemLayout;
    property Images: TCustomImageList read GetImages;
    property ItemColorStyle: TSelectionColorStyle read GetItemColorStyle;
    property ItemColorHot: TColor read GetItemColorHot;
    property ItemColorHotTo: TColor read GetItemColorHotTo;
    property ItemColorSelected: TColor read GetItemColorSelected;
    property ItemColorSelectedTo: TColor read GetItemColorSelectedTo;
    property ColorBoxHeight: Integer read GetColorBoxHeight;
    property ColorBoxWidth: Integer read GetColorBoxWidth;

    property AdvItemSelector: TAdvCustomItemSelector read FAdvItemSelector;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
  end;

  TAdvCustomItemSelector = class(TScrollingWinControl)
  private
    FItems: TSelectorItems;
    FSelectorType: TSelectorType;
    FItemLayout: TItemLayout;
    FColumns: Integer;
    FColorBoxWidth: Integer;
    FColorBoxHeight: Integer;
    FImages: TCustomImageList;
    FItemSelectorPanel: TCustomItemSelector;
    FOnItemSelect: TNotifyEvent;
    FItemAppearance: TItemAppearance;
    FOnDrawItem: TDrawItemEvent;
    FOnItemSize: TItemSizeEvent;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure OnItemChanged(Sender: TObject);
    procedure OnPanelItemSelect(SEnder: TObject);
    procedure SetItems(const Value: TSelectorItems);
    procedure SetSelectorType(const Value: TSelectorType);
    procedure SetItemLayout(const Value: TItemLayout);
    procedure SetColumns(const Value: Integer);
    procedure SetColorBoxHeight(const Value: Integer);
    procedure SetColorBoxWidth(const Value: Integer);
    procedure SetImages(const Value: TCustomImageList);
    procedure OnItemAppearanceChanged(Sender: TObject);
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    procedure SetItemAppearance(const Value: TItemAppearance);
    function GetItemHot: Integer;
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure WndProc(var Message: TMessage); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure CreateSelectorPanel;
    function GetItemPanelSize: TSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SelectFirst;
    procedure SelectLast;
    procedure SelectNext;
    procedure SelectPrevious;
    procedure HotNext;
    procedure HotPrevious;
    procedure ScrollItemInView(Index: Integer);
    function GetVisibleItemCount: Integer;
    function LookupItem(s: string): Boolean;  // search and select first occurance of s, Result true if found
    procedure UpdateSelectorPanel;
    property ItemHot: Integer read GetItemHot;
  published
    property Color;
    property Columns: Integer read FColumns write SetColumns default 1;
    property Items: TSelectorItems read FItems write SetItems;
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    property SelectorType: TSelectorType read FSelectorType write SetSelectorType default stImage;
    property ItemLayout: TItemLayout read FItemLayout write SetItemLayout default ilCaptionRight;
    property Images: TCustomImageList read FImages write SetImages;

    property ItemAppearance: TItemAppearance read FItemAppearance write SetItemAppearance;
    property ColorBoxHeight: Integer read FColorBoxHeight write SetColorBoxHeight;
    property ColorBoxWidth: Integer read FColorBoxWidth write SetColorBoxWidth;
    property OnItemSelect: TNotifyEvent read FOnItemSelect write FOnItemSelect;
    property OnDrawItem: TDrawItemEvent read FOnDrawItem write FOnDrawItem;
    property OnItemSize: TItemSizeEvent read FOnItemSize write FOnItemSize;
    property OnResize;
  end;


procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
//function BlendColor(Col1,Col2:TColor; BlendFactor:Integer): TColor;
function BrightnessColor(Col: TColor; Brightness: integer): TColor; overload;
function BrightnessColor(Col: TColor; BR,BG,BB: integer): TColor; overload;
//procedure DrawGradientBackGround(Canvas: TCanvas; R: TRect; Clr, ClrTo, BrClr: TColor; BrWidth: Integer; Grad: TGradientDirection);
procedure DrawButtonContent(Canvas: TCanvas; R: TRect; Caption: string; Images: TCustomImageList; ImageIndex: Integer);
function DrawHTMLEX(Canvas: TCanvas; s: string; R: TRect; Images: TCustomImageList; RealDraw: Boolean; WinHandle: THandle): TSize;
//procedure DrawBitmapTransp(Canvas: TCanvas;bmp:TBitmap;bkcolor:TColor;r:TRect);
function GetTextSize(Canvas: TCanvas; Text: string): TSize;
function GetTextOfSize(Canvas: TCanvas; Text: string; cx: Integer; AddEllipsis: Boolean = True): string;

implementation

//{$R AdvDropDown.res}

const
  Ctrl_Codes = [vk_back, vk_tab, vk_return];
  Numeric_Codes = [ord('0')..ord('9'), ord('-')];
  Money_Codes = Numeric_Codes;
  Float_Codes = Numeric_Codes + [ord(','), ord('.')];
  Range_Codes = Numeric_Codes + [ord(','), ord(';')];
  Hex_Codes = Numeric_Codes + [ord('A')..ord('F'), ord('a')..ord('f')];
  Bin_Codes = ['0', '1'];
  AlphaNum_Codes = [ord('0')..ord('9')] + [ord('a')..ord('z'), ord('A')..ord('Z')];

{$I DELPHIXE.INC}

//----------------------------------------------------------------- DrawGradient

procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; R: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if Direction then
    R.Right := R.Right - 1
  else
    R.Bottom := R.Bottom - 1;

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

procedure DrawGradientBackGround(Canvas: TCanvas; R: TRect; Clr, ClrTo, BrClr: TColor; BrWidth: Integer; Grad: TGradientDirection);
begin
  if not Assigned(Canvas) then
    Exit;
    
  if (ClrTo <> clNone) then
    DrawGradient(Canvas, Clr, ClrTo, 80, R, Grad = gdHorizontal)
  else if (Clr <> clNone) then
  begin
    Canvas.Brush.Color := Clr;
    Canvas.FillRect(R);
  end;

  //--- Draw Border
  if (BrClr <> clNone) and (BrWidth > 0) then
  begin
    Canvas.Brush.Style := bsClear;
    //--- Border Shade
    Canvas.Pen.Width := 1;
    {R1 := R;
    InflateRect(R1, -BrWidth, -BrWidth);
    Canvas.Pen.Color := BrightnessColor(BrClr, 20);
    Canvas.Rectangle(R1);}
    //---
    Canvas.Pen.Width := BrWidth;
    Canvas.Pen.Color := BrClr;
    Canvas.Rectangle(R);
  end;

end;

//------------------------------------------------------------------------------

procedure DrawButtonContent(Canvas: TCanvas; R: TRect; Caption: string; Images: TCustomImageList; ImageIndex: Integer);
var
  XPos, YPos: Integer;
  DFLAG: word;
begin
  if not Assigned(Canvas) then
    Exit;

  DFLAG := DT_CENTER;
  //--- Draw Image
  if (ImageIndex >= 0) and Assigned(Images) then
  begin
    XPos := R.Left + DD_IMAGEXOFFSET;
    YPos := R.Top + (R.Bottom - R.Top - Images.Height) div 2;
    Images.Draw(Canvas, XPos, YPos, ImageIndex);
    R.Left := XPos + Images.Width + DD_IMAGEGAP;
    DFLAG := DT_LEFT;
  end;
  //---

  //--- Draw Caption
  if (Caption <> '') then
  begin
    DrawText(Canvas.Handle, PChar(Caption), -1, R, DT_SINGLELINE or DT_VCENTER or DFLAG or DT_END_ELLIPSIS);
  end;
end;

//------------------------------------------------------------------------------

procedure DrawBitmapTransp(Canvas: TCanvas;bmp:TBitmap;bkcolor:TColor;r:TRect);
var
  tmpbmp: TBitmap;
  srcColor: TColor;
  tgtrect: TRect;
begin
  TmpBmp := TBitmap.Create;
  try
    TmpBmp.Height := bmp.Height;
    TmpBmp.Width := bmp.Width;

    tgtrect.left := 0;
    tgtrect.top := 0;
    tgtrect.right := r.right - r.left;
    tgtrect.bottom := r.bottom - r.Top;

    TmpBmp.Canvas.Brush.Color := bkcolor;
    srcColor := bmp.Canvas.Pixels[0,0];
    TmpBmp.Canvas.BrushCopy(tgtrect,bmp,tgtrect,srcColor);
    Canvas.CopyRect(r, TmpBmp.Canvas, tgtrect);
  finally
    TmpBmp.Free;
  end;
end;

//------------------------------------------------------------------------------

function GetTextSize(Canvas: TCanvas; Text: string): TSize;
var
  R: TRect;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (Text <> '') then
  begin
    R := Rect(0, 0, 1000, 500);
    DrawText(Canvas.Handle,PChar(Text),Length(Text), R, DT_CALCRECT or DT_LEFT or DT_SINGLELINE);
    Result.cx := R.Right;
    Result.cy := R.Bottom;
  end;
end;

//------------------------------------------------------------------------------

function GetTextOfSize(Canvas: TCanvas; Text: string; cx: Integer; AddEllipsis: Boolean = True): string;
var
  i: Integer;
  s: string;
  ts: TSize;
begin
  Result := Text;
  ts := GetTextSize(Canvas, Text);
  if (ts.cx > cx) then
  begin
    for I := 1 to Length(Text) do
    begin
      s := Copy(Text, 1, Length(Text) - i);
      if AddEllipsis then
        s := s + '...';
      ts := GetTextSize(Canvas, s);
      if (ts.cx < cx) then
      begin
        Result := s;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function DrawHTMLEX(Canvas: TCanvas; s: string; R: TRect; Images: TCustomImageList; RealDraw: Boolean; WinHandle: THandle): TSize;
var
  ml,hl: Integer;
  hr,CR: TRect;
  CV,CT, CID: string;
  Anchor,Stripped,FocusAnchor,AnchorHint: string;
begin
  Result.cx := 0;
  Result.cy := 0;
  if (s = '') then
    Exit;

  HTMLDrawEx(Canvas, s, r, Images, 0, 0, -1, 0, 1, False, not RealDraw,False,True,True,False, False{Wordwrap},false,'',
             0.0, clBlue, clNone, clNone, clGray, Anchor, Stripped, FocusAnchor, AnchorHint,
             Integer(Result.cx), Integer(Result.cy), hl, ml, hr, cr, CID, CT, CV, nil, nil, WinHandle);
end;

//------------------------------------------------------------------------------

function CheckSeparator(ch: char): boolean;
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in ['-', ',', ';'];
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  Result := (ch = '-') or (ch = ',') or (ch = ';');
  {$ENDIF}
end;

//------------------------------------------------------------------------------

function IsType(s: string): TAutoType;
var
  i: Integer;
  isI, isF, isH: Boolean;
  th, de, mi: Integer;

begin
  Result := atString;

  isI := true;
  isF := true;
  isH := true;

  if s = '' then
  begin
    isI := false;
    isF := false;
    isH := false;
  end;

  th := -1; de := 0; mi := 0;

  for i := 1 to Length(s) do
  begin
    if not (ord(s[i]) in Numeric_Codes) then isI := false;
    if not (ord(s[i]) in Float_Codes) then isF := false;
    if not (ord(s[i]) in Hex_Codes) then isH := false;

    if (s[i] = thousandseparator) and (i - th < 3) then isF := false;

    if s[i] = thousandseparator then th := i;
    if s[i] = decimalseparator then inc(de);
    if s[i] = '-' then inc(mi);
  end;

  if isH and not isI then
    Result := atHex;

  if isI then
    Result := atNumeric
  else
  begin
    if isF then
      Result := atFloat;
  end;

  if (mi > 1) or (de > 1) then
    Result := atString;
end;

//------------------------------------------------------------------------------

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;

//------------------------------------------------------------------------------

function HexToInt(s: string): Integer;
var
  i: Integer;
  r, m: Integer;

  function CharVal(c: char): Integer;
  begin
    Result := 0;
    if ((c >= '0') and (c <= '9')) then Result := ord(c) - ord('0');
    if ((c >= 'A') and (c <= 'F')) then Result := ord(c) - ord('A') + 10;
    if ((c >= 'a') and (c <= 'f')) then Result := ord(c) - ord('a') + 10;
  end;

begin
  r := 0;
  m := 1;
  for i := Length(s) downto 1 do
  begin
    r := r + m * CharVal(s[i]);
    m := m shl 4;
  end;
  Result := r;
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

{TWinCtrl}

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

//------------------------------------------------------------------------------

{$IFNDEF DELPHI7_LVL}
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
{$ENDIF}

//------------------------------------------------------------------------------

{ TAdvCustomDropDown }

constructor TAdvCustomDropDown.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  i: integer;
begin

  inherited Create(AOwner);

  FBorderColor := clNone;
  FFocusBorderColor := clNone;
  FDisabledBorder := true;

  FDropDownBorderColor := clBlack;
  FDropDownGradient := gdHorizontal;
  FDropDownColor := clWhite; //$00F7F8F9;
  FDropDownColorTo := clNone;
  FDropDownHeight := 0;
  FDropDownWidth := 0;
  FDropDownBorderWidth := 1;
  FDropDownSizeable := True;
  FDropDownShadow := True;
  FEditorEnabled := True;
  FDropDownEnabled := True;

  FEditType := etString;
  FParentFnt := false;

  SetBounds(left, top, 200, 21);
  FButtonWidth := DD_DROPDOWNBUTTONWIDTH;

  //--- Create DropDown Form
  if not (csDesigning in ComponentState) then
    CreateDropDownForm;

  //FButtonGlyph := TBitmap.Create;
  //FButtonGlyph.OnChange := OnButtonGlyphChanged;

  FButton := nil;
  //--- Create DropDownButton
  CreateDropDownButton;

  ControlStyle := ControlStyle - [csSetCaption];
  ReadOnly := false;
  FDropPosition := dpAuto;
  Text := '';
  FOldText := '';

  FUserDropDownWidth := -1;
  FUserDropDOwnHeight := -1;

  FDropDownHeader := THeaderAppearance.Create(Self);
  FDropDownHeader.OnChange := OnHeaderAppearanceChanged;
  FDropDownHeader.OnButtonsChanged := OnHeaderAppearanceChanged;
  FDropDownFooter := TFooterAppearance.Create(Self);
  FDropDownFooter.OnChange := OnFooterAppearanceChanged;
  FDropDownFooter.OnButtonsChanged := OnFooterAppearanceChanged;
  FButtonAppearance := TAdvButtonAppearance.Create;

  FDefaultHandling := True;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  Windows.GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  FDoShowBusy := false;
  FCanUndo := True;
  FAutoThousandSeparator := True;
  SelectionColorStyle := scOffice2007;
  FFocusDraw := True;
  SetComponentStyle(tsOffice2007Luna);

  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
end;


//------------------------------------------------------------------------------

destructor TAdvCustomDropDown.Destroy;
begin
  FLabelFont.Free;
  FButton.Free;
  if not (csDesigning in ComponentState) then
    FDropDownForm.Free;
  FDropDownHeader.Free;
  FDropDownFooter.Free;
  FButtonAppearance.Free;
  //FButtonGlyph.Free;
  inherited Destroy;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style or ES_MULTILINE or WS_CLIPCHILDREN;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CreateDropDownForm;
begin
  if not Assigned(FDropDownForm) then
  begin
    FDropDownForm := TDropDownForm.CreateNew(Self, 0);
    with FDropDownForm do
    begin
      //BorderWidth := 4;
      //Color := clWhite;
      AdvDropDown := Self;
      BorderStyle := bsNone;
//      FormStyle := fsStayOnTop;
      Visible := False;
      Width := FDropDownWidth;
      Height := FDropDownHeight;
      OnClose := FormClose;
      OnHide := OnFormHide;
      OnKeyPress := OnFormKeyPress;
      OnKeyDown := OnFormKeyDown;
      OnKeyUp := OnFormKeyUp;
      OnMouseWheelDown := OnFormMouseWheelDown;
      OnMouseWheelUp := OnFormMouseWheelUp;
      FDropDownForm.OnDestroy := OnFormDestroy;
      FDropDownForm.OnSizing := DropDownSizing;
      DoubleBuffered := true;
    end;
  end;
end;

function TAdvCustomDropDown.CreateLabel: TLabel;
begin
  Result := TLabel.Create(self);
  Result.Parent := self.Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.OnClick := LabelClick;
  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := self.ParentFont;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CreateDropDownButton;
begin
  if not Assigned(FButton) then
  begin
    FButton := TDropDownEditButton.Create(Self);
    FButton.Parent := Self;
    FButton.OnClick := DropDownButtonClick;
    if (csDesigning in ComponentState) then
      FButton.Color := clBtnFace;
  end;
  UpdateDropDownButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.UpdateDropDownButton;
begin
  if not Assigned(FButton) then
    Exit;

  FButton.Width := DropDownButtonWidth;
  FButton.Height := DD_DROPDOWNBUTTONHEIGHT;
  FButton.Visible := True;
  //if not (csDesigning in ComponentState) then  
    FButton.FocusControl := Self;

  //if FButtonGlyph.Empty then
  //  FButton.Glyph.Handle := LoadBitmap(0, MakeIntResource(OBM_COMBO))
  //else
  //  FButton.Glyph.Assign(FButtonGlyph);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.MouseButtonDown(Sender: TObject);
begin
  if csDesigning in ComponentState then
    Exit;

  if not FDropDownForm.Visible and (GetTickCount - FDropDownForm.DeActivateTime > 250) then
    DoShowDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DropDownButtonClick(Sender: TObject);
begin
  MouseButtonDown(nil);
end;

procedure TAdvCustomDropDown.DropDownSizing(Sender: TObject);
begin
  OnDropDownSizing;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMSize(var Message: TWMSize);
begin
  inherited;
  ResizeControl;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetMinHeight: Integer;
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
  Result := Metrics.tmHeight + I div 4;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetEditRect: TRect;
begin
  Result := FEditRect;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetEditRect;
var
  Loc: TRect;
begin
  SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

  Loc.Bottom := ClientHeight + 1; {+1 is workaround for windows paint bug}
  Loc.Right := ClientWidth - FButton.Width - 3;

  if (BorderStyle = bsNone) and (BevelKind = bkNone) then
  begin
    Loc.Top := 3;
    Loc.Left := 2;
  end
  else
  begin
    Loc.Top := 1;
    Loc.Left := 1;
  end;
  FEditRect := Loc;

  if not Ctl3D then
    Loc.Left := Loc.Left + 1;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@Loc));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CreateWnd;
begin
  inherited CreateWnd;
  SetEditRect;
  if Assigned(FLabel) then
    UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DestroyWnd;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetEditorEnabled(const Value: boolean);
begin
  FEditorEnabled := Value;
  //ReadOnly := not (Value);
  if FEditorEnabled then
    ControlStyle := ControlStyle + [csDoubleClicks]
  else
    ControlStyle := ControlStyle - [csDoubleClicks];
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.BeforeDropDown;
begin
  if Assigned(FOnBeforeDropDown) then
    FOnBeforeDropDown(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.AdaptDropDownSize;
begin
  //
end;

procedure TAdvCustomDropDown.AfterDropDown;
begin
  if Assigned(FFocusControl) and FFocusControl.TabStop and
      FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
    FFocusControl.SetFocus;

  //else if TabStop and (GetFocus <> Handle) and CanFocus then
    //SetFocus;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetCenterControl;
begin
  if Assigned(FDropDownForm) and Assigned(FControl) and Assigned(DropDownHeader) then
  begin
    FControl.Parent := FDropDownForm;
    if DropDownHeader.Visible then
      FControl.Top := DropDownHeader.Height + 2
    else
      FControl.Top := DropDownBorderWidth;
    FControl.Align := alClient;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.UpdateDropDownSize;
var
  h: Integer;
begin
  if Assigned(FDropDownForm) then
  begin
    if (FDropDownWidth > 0) then
      FDropDownForm.Width := FDropDownWidth
    else
    begin
      if Assigned(FControl) and FControl.Visible then
      begin
        FDropDownForm.Width := FControl.Width; // + DropDownBorderWidth * 2;
      end
      else
      begin
        FDropDownForm.Width := Self.Width;
      end;
    end;

    if (FDropDownHeight > 0) then
    begin
      FDropDownForm.Height := FDropDownHeight;
    end
    else
    begin
      // AutoSize
      h := DropDownBorderWidth * 2;
      if DropDownHeader.Visible then
        h := h + DropDownHeader.Height;
      if DropDownFooter.Visible then
        h := h + DropDownFooter.Height;
      if Assigned(FControl) and FControl.Visible then
        h := h + FControl.Height;
      FDropDownForm.Height := h;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DoShowDropDown;
{$IFNDEF DELPHI2006_LVL}
const
  CS_DROPSHADOW = $00020000;
{$ENDIF}
var
  p: TPoint;
  accept: Boolean;
  R: TRect;
  {$IFDEF DELPHI6_LVL}
  mon: TMonitor;
  {$ENDIF}
  w, h: Integer;

begin
  if (csDesigning in ComponentState) or not Assigned(FDropDownForm) then
    Exit;

  if FDropDownForm.Visible or not Enabled or ReadOnly then
    Exit;

  if FDropDownForm.IsDeactivating then
    Exit;

  if FDoShowBusy then
    Exit;

  FDoShowBusy := true;

  BeforeDropDown;

  //FOldText := Text; // Caption

  FDropDownForm.Shadow := DropDownShadow;

  //FDropDownForm.BorderStyle := bsSizeable;
  (*
  FDropDownForm.Sizeable := FDropDownSizeable;
  if (FDropDownForm.Shadow <> DropDownShadow) then
  begin
    FDropDownForm.Shadow := DropDownShadow;

    if not (FDropDownForm.Shadow) then
      SetClassLong(FDropDownForm.Handle,GCL_STYLE, GetClassLong(FDropDownForm.Handle, GCL_STYLE) - CS_DROPSHADOW)
    else
      SetClassLong(FDropDownForm.Handle,GCL_STYLE, GetClassLong(FDropDownForm.Handle, GCL_STYLE) and not CS_DROPSHADOW);

    FDropDownForm.RecreateWnd;

  end;
  //FDropDownForm.BorderStyle := bsSizeable;
  *)

  FDropDownForm.InitializeAndUpdate;

  if not (FDropDownForm.Shadow) then
    SetClassLong(FDropDownForm.Handle,GCL_STYLE, GetClassLong(FDropDownForm.Handle, GCL_STYLE) and not CS_DROPSHADOW)
  else
    SetClassLong(FDropDownForm.Handle,GCL_STYLE, GetClassLong(FDropDownForm.Handle, GCL_STYLE) or CS_DROPSHADOW);


  FDropDownForm.Left := self.Left;
  FDropDownForm.Top := self.Top;

  UpdateDropDownSize;
  SetCenterControl;

  FDropDownForm.Constraints.MinWidth := DD_MINWIDTH;
  FDropDownForm.Constraints.MinHeight := DD_MINHEIGHT;

  if Assigned(FControl) and (FControl is TWinControl) then
  begin
    TWinCtrl(FControl).OnKeyDown := OnControlKeyDown;
    TWinCtrl(FControl).OnKeyUp := OnControlKeyUp;
    TWinCtrl(FControl).OnKeyPress := OnControlKeyPress;
  end;

  if (Parent is TWinControl) then
  begin
    P := Point(Left, Top);
    P := Parent.ClientToScreen(P);
  end
  else
  begin
    P := Point(0, 0);
    P := ClientToScreen(P);
  end;

  case FDropPosition of
    dpAuto:
    begin
      if P.y + FDropDownForm.Height >= GetSystemMetrics(SM_CYSCREEN) then
      begin //Up
        FdropDownForm.Left := P.x;
        FdropDownForm.Top := p.y - FDropDownForm.Height;
      end
      else
      begin //Down
        FDropDownForm.Left := P.x;
        FDropDownForm.Top := p.y + Height - 1;
      end;
    end;
    dpDown:
    begin
      FDropDownForm.Left := P.x;
      FDropDownForm.Top := p.y + Height - 1;
    end;
    dpUp:
    begin
      FDropDownForm.Left := P.x;
      FDropDownForm.Top := p.y - FDropDownForm.Height;
    end;
  end;

  Accept := true;

  if Assigned(FOnDropDown) then
    FOnDropdown(Self, Accept);

  if not Accept then
  begin
    FDoShowBusy := false;
    Exit;
  end;

  R := Rect(-1, -1, -1, -1);
  mon := Screen.MonitorFromPoint(p);
  if Assigned(mon) then
    R := mon.WorkAreaRect
  else
    SystemParametersInfo(SPI_GETWORKAREA, 0, @R, 0);

  if (R.Left >= 0) and (R.Right < FDropDownForm.Left + FDropDownForm.Width) and (R.Right > FDropDownForm.Left) then
    FDropDownForm.Left := FDropDownForm.Left - ((FDropDownForm.Left + FDropDownForm.Width) - R.Right);

  p.x := FDropDownForm.Left;
  p.y := FDropDownForm.Top;

  if FUserDropDownWidth <> -1 then
  begin
    FDropDownForm.Width := FUserDropDownWidth;
  end;

  if FUserDropDownHeight <> -1 then
  begin
    FDropDownForm.Height := FUserDropDownHeight;
  end;

  if Assigned(FOnGetDropDownPos) then
  begin
    FOnGetDropDownPos(Self, p);

    FDropDownForm.Left := P.x;
    FDropDownForm.Top := p.y;
  end;

  w := FDropDownForm.Width;
  h := FDropDownForm.Height;

  FDropDownForm.Width := 0;
  FDropDownForm.Height := 0;
  FDropDownForm.Visible := True;

  if DropDownShadow and ForceShadow then
  begin
    FDropDownForm.Visible := False;
    FDropDownForm.Width := 1;
    FDropDownForm.Height := 1;
    FDropDownForm.Visible := True;
  end;

  AdaptDropDownSize(h);

  if Accept then
  begin
    FDroppedDown := True;
    (*
    FDropDownForm.Show;
    FDroppedDown := True;
    FDropDownForm.Left := P.x;
    FDropDownForm.Top := p.y;
    {$IFDEF DELPHI9_LVL}
    FDropDownForm.Width := w;
    FDropDownForm.Height := h;
    {$ENDIF}
    *)
    SetWindowPos(FDropDOwnForm.Handle,HWND_TOPMOST,p.x,p.y,w,h,0);
    AfterDropDown;
  end;

  FDoShowBusy := false;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DoHideDropDown(Cancelled: boolean);
begin
  if (csDesigning in ComponentState) then
    Exit;

  if not FDropDownForm.Visible then
    Exit;

  if Assigned(FOnBeforeDropUp) then
    FOnBeforeDropUp(self);

  FUserDropDownWidth := FDropDownForm.Width;
  FUserDropDownHeight := FDropDownForm.Height;

  FDropDownForm.Hide;

  Application.CancelHint;

  if Cancelled then
  begin
    Text := FOldText;
  end
  else
  begin
    //--- Update new value
    {if Assigned(FControl.Selected) then
    begin
      Text := FControl.Selected.Text;
    end;
    }
  end;

  if Assigned(FOnDropUp) then
    FOnDropUp(self, Cancelled);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnHideDropDown;
begin
  FDroppedDown := False;
  if TabStop and CanFocus and (GetFocus <> Handle) {and not ReadOnly and EditorEnabled} and not (csDestroying in ComponentState) then
    SetFocus;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMKeyDown(var Msg: TWMKeyDown);
var
  selp: Integer;
  s: string;
  isCtrl, IsAlt: Boolean;
begin
  if csDesigning in ComponentState then
    Exit;

  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;

  if IsAlt then
  begin
    case Msg.CharCode of
      VK_UP, VK_DOWN:
      begin
        if DroppedDown then
          DoHideDropDown(False)
        else
          DoShowDropDown;
      end;
    end;
  end;

  if not EditorEnabled and Enabled then
  begin
    if not ReadOnly then
    begin
      case Msg.CharCode of
      //VK_UP: DoHideDropDown(False);
      //VK_DOWN: DoShowDropDown;
      VK_F4:
        begin
          if FDropDownForm.Visible then
            DoHideDropDown(False)
          else
            DoShowDropDown;
        end;
      VK_RETURN:
        begin
            //PostMessage(Handle,WM_KEYDOWN,VK_UP,0);
          DoHideDropDown(False);
        end;
      VK_ESCAPE: DoHideDropDown(True);
      end;
    end;

    if (Msg.CharCode in [VK_RETURN,VK_ESCAPE,VK_TAB]) then
      inherited;
    Exit;
  end;

  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  if (msg.CharCode in [VK_TAB, VK_RETURN]) and IsCtrl then
  begin
    inherited;
    Exit;
  end;

  if (msg.CharCode = VK_RETURN) and (FReturnIsTab) then
  begin
    msg.CharCode := VK_TAB;
    if IsWindowVisible(self.Handle) then
      PostMessage(self.Handle, WM_KEYDOWN, VK_TAB, 0);
  end;

  if (msg.CharCode = VK_RIGHT) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(self.handle, EM_GETSEL, 0, 0));
    if selp >= Length(self.text) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.CharCode = VK_DELETE) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(Handle, EM_GETSEL, 0, 0));
    if (selp >= Length(self.text)) and (SelLength = 0) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
    SetModified(true);
  end;

  if (msg.CharCode = VK_LEFT) and (FPrefix <> '') then
  begin
    selp := hiword(SendMessage(Handle, EM_GETSEL, 0, 0));

    if selp <= Length(FPrefix) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.CharCode = VK_END) and (FSuffix <> '') then
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(Text);
      SelLength := 0;
    end
    else
      SelLength := Length(Text) - SelStart;
    msg.charcode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.CharCode = VK_HOME) and (FPrefix <> '') then
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(fPrefix);
      SelLength := 0;
    end
    else
    begin
      SendMessage(Handle, EM_SETSEL, Length(fprefix) + Length(self.text), Length(fprefix));
    end;
    msg.Charcode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.CharCode = VK_BACK) and (FPrefix <> '') then
  begin
    if (SelStart <= Length(FPrefix) + 1) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;
    SetModified(true);
  end;

  if (msg.CharCode = VK_DELETE) and (SelStart >= Length(FPrefix)) then
  begin
    s := self.text;
    if SelLength = 0 then
      Delete(s, SelStart - Length(fprefix) + 1, 1)
    else
      Delete(s, SelStart - Length(fprefix) + 1, SelLength);

    if (lengthlimit > 0) and (fixedLength(s) - 1 > lengthlimit) then
    begin
      msg.CharCode := 0;
      msg.Result := 0;
      Exit;
    end;

    SetModified(true);
  end;

  inherited;

  if (msg.CharCode = VK_DELETE) and (EditType = etMoney) then
    AutoSeparators;

  if (fPrefix <> '') and (SelStart < Length(fPrefix)) then
    SelStart := Length(fPrefix);

  case Msg.CharCode of
  //VK_UP: DoHideDropDown(False);
  //VK_DOWN: DoShowDropDown;
  VK_F4:
    begin
      if FDropDownForm.Visible then
        DoHideDropDown(False)
      else
        DoShowDropDown;
    end;
  VK_RETURN:
    begin
      //if (GetKeyState(VK_CONTROL) and $8000 = $8000) then
        //PostMessage(Handle,WM_KEYDOWN,VK_UP,0);
      DoHideDropDown(False);
    end;
  VK_ESCAPE: DoHideDropDown(True);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;

  if (ssAlt in Shift) then
  begin
    case Key of
      VK_UP, VK_DOWN:
      begin
        if DroppedDown then
          DoHideDropDown(False)
        else
          DoShowDropDown;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = FImages) then
    begin
      Images := nil;
      Invalidate;
    end;

    if (AComponent = FControl) then
      Control := nil;

    if (AComponent = FFocusControl) then
      FocusControl := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetImages(const Value: TCustomImageList);
begin
  FImages := value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
//  Message.Result := 1; // Message.Result or DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.KeyPress(var Key: Char);
begin
  inherited KeyPress(key);
  if (Key = Char(VK_RETURN)) then Key := #0;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
//  FormDeactivate(self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetFlat(const Value: Boolean);
begin
  if (FFlat <> Value) then
  begin
    FFlat := Value;
    if Assigned(FButton) then
      FButton.FButton.Flat := True;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetVersionEx: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.ShowDropDown;
begin
  DoShowDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.HideDropDown(CancelChanges: Boolean = false);
begin
  DoHideDropDown(CancelChanges);
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  HandleMouseWheelDown;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  HandleMouseWheelUp;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.HandleMouseWheelDown;
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.HandleMouseWheelUp;
begin

end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetButtonGlyph: TBitmap;
begin
  //Result := FButtonGlyph;
  if Assigned(FButton) then
    Result := FButton.Glyph
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetButtonGlyph(const Value: TBitmap);
begin
  //FButtonGlyph.Assign(Value);
  if Assigned(FButton) then
  begin
  //  if FButtonGlyph.Empty then
  //    UpdateDropDownButton
  //  else
  //    FButton.Glyph.Assign(FButtonGlyph);

    FButton.Glyph.Assign(Value);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetButtonHint(const Value: string);
begin
  if FButtonHint <> Value then
  begin
    FButtonHint := Value;
    if Assigned(FButton) then
    begin
      FButton.Hint := Value;
      FButton.ShowHint := Value <> '';
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetButtonWidth: Integer;
begin
  Result := FButtonWidth;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetDropDownSize: TSize;
begin
   Result.cx := 0;
   Result.cy := 0;

  if (DropDownWidth <> 0) and (DropDownHeight <> 0) then
  begin
    Result.cx := DropDownWidth;
    Result.cy := DropDownHeight;
  end
  else
  if Assigned(FDropDownForm) then
  begin
    Result.cx := FDropDownForm.Width;
    Result.cy := FDropDownForm.Height;
  end
  else
  begin
    if Assigned(FControl) then
    begin
      Result.cx := FControl.Width;
      Result.cy := FControl.Height;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetButtonWidth(const Value: Integer);
begin
  if Assigned(FButton) and (FButtonWidth <> Value) then
  begin
    FButtonWidth := Value;
    FButton.FBWidth := Value;
    if FButton.HandleAllocated then
      ResizeControl;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetButtonCaption(const value: string);
begin
  if Assigned(FButton) then
    FButton.ButtonCaption := value;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetButtonCaption: string;
begin
  Result := '';
  if Assigned(FButton) then
    Result := FButton.ButtonCaption;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.ReadSelectionColorStyle(Reader: TReader);
begin
  FSelectionColorStyle := TSelectionColorStyle(Reader.ReadInteger);
end;

procedure TAdvCustomDropDown.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.ResetDropSize;
begin
  FUserDropDownWidth := -1;
  FUserDropDownHeight := -1;
end;

procedure TAdvCustomDropDown.ResizeControl;
var
  MinHeight: Integer;
  Dist,FlatCorr: Integer;
  Offs: Integer;
begin
  if (BorderStyle = bsNone) and (BevelKind = bkNone) then
    Dist := 2
  else
    Dist := 4;

  if FFlat then
    Dist := 3;

  if FFlat then
    FlatCorr := 1
  else
    FlatCorr := -1;

  MinHeight := GetMinHeight;

  if not Ctl3d then
    Offs := 2
  else
    Offs := 0;

  { text edit bug: if size to less than minheight, then edit ctrl does
    not display the text }

  if (Height < MinHeight) then
    Height := MinHeight
  else
  if (FButton <> nil) then
  begin
    if NewStyleControls and Ctl3D and not ( (BorderStyle = bsNone) and (BevelKind = bkNone)) then
      FButton.SetBounds(Width - FButton.FBWidth - Dist - Offs,1 + FlatCorr,FButton.FBWidth,Height - Dist)
    else
      FButton.SetBounds(Width - FButton.FBWidth - Offs,1,FButton.FBWidth,Height - 2);
    SetEditRect;
   end;

  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMChar(var Msg: TWMKey);
var
  oldSelStart, oldprec: Integer;
  s: string;
  key: char;
  isCtrl: Boolean;
  cf: TCustomForm;

  function scanprecision(s: string; inspos: Integer): Boolean;
  var
    mdist: Integer;
  begin
    Result := false;
    inspos := inspos - Length(FPrefix);
    if FPrecision <= 0 then Exit;

    if (Length(s) - inspos > FPrecision) then
    begin
      Result := false;
      exit;
    end;

    if (Pos(decimalseparator, s) > 0) then
    begin
      mdist := Length(s) - Pos(decimalseparator, s);
      if (inspos >= Pos(decimalseparator, s)) and (mdist >= FPrecision) then
        Result := true;
    end;
  end;

  function scandistance(s: string; inspos: Integer): Boolean;
  var
    mdist: Integer;
  begin
    Result := false;
    inspos := inspos - Length(fPrefix);
    mdist := Length(s);

    if (Pos(thousandseparator, s) = 0) then
    begin
      Result := false;
      Exit;
    end;

    while (Pos(thousandseparator, s) > 0) do
    begin
      if abs(Pos(thousandseparator, s) - inspos) < mdist then mdist := abs(Pos(thousandseparator, s) - inspos);
      if (abs(Pos(thousandseparator, s) - inspos) < 3) then
      begin
        Result := true;
        break;
      end;

      if inspos > Pos(thousandseparator, s) then inspos := inspos - Pos(thousandseparator, s);
      delete(s, 1, Pos(thousandseparator, s));
    end;

    if (mdist > 3) then
    begin
      Result := true;
    end;
  end;

begin
  if not EditorEnabled then
    Exit;

  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  if (SelLength = length(Text)) and (Text <> '') then
    FBlockChange := true;

  if (Msg.CharCode = VK_RETURN) and IsCtrl then
  begin
    Msg.CharCode := 0;
    Msg.Result := 1;
    Exit;
  end;

  if Msg.CharCode = VK_RETURN then
  begin
    key := #13;
    if Assigned(OnKeyPress) then
      OnKeyPress(Self, key);

    Msg.CharCode := 0;

    if not DefaultHandling then
    begin
      if (Parent is TWinControl) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
    Exit;
  end;

  if Msg.CharCode = VK_ESCAPE then
  begin
    if not DefaultHandling then
    begin
      if (Parent is TWinControl) then
      begin

        cf := GetParentForm(self);

        if Assigned(cf) then
          if cf.KeyPreview then
          begin
            inherited;
            Exit;
          end;

        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_ESCAPE, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_ESCAPE, 0);
      end;
    end;
  end;

  if (Msg.Charcode = VK_ESCAPE) and not FCanUndo then
  begin
    inherited;
    Exit;
  end;

// allow Ctrl-C, Ctrl-X, Ctrl-V
  if IsCtrl and (Msg.CharCode in [3, 22, 24]) then
  begin
    inherited;
    Exit;
  end;

  if (msg.charcode = ord('.')) and (FExcelStyleDecimalSeparator) and (msg.keydata and $400000 = $400000) then
  begin
    msg.charcode := ord(decimalseparator);
  end;

  if (msg.charcode = ord(',')) and (FExcelStyleDecimalSeparator) and (msg.keydata and $400000 = $400000) then
  begin
    msg.charcode := ord(decimalseparator);
  end;

  if (msg.CharCode = vk_back) and (FPrefix <> '') then
    if (SelStart <= Length(FPrefix)) and (SelLength = 0) then Exit;

  if (FLengthLimit > 0) and (FixedLength(self.Text) > FLengthLimit) and
    (SelLength = 0) and (SelStart < DecimalPos)
  and (msg.charcode <> vk_back) and (msg.charcode <> ord(decimalseparator)) and not AllowMin(chr(msg.CharCode)) then Exit;

  if (msg.charcode = vk_back) then
  begin
    s := self.Text;
    if SelLength = 0 then
      delete(s, SelStart - Length(fprefix), 1)
    else
      delete(s, SelStart - Length(fprefix), SelLength);

    inherited;
    if (Text = '') and (s <> '') then
      Change;

    //if (lengthlimit > 0) and (fixedLength(s) - 1 > lengthlimit) then
    Exit;
  end;

  if IsCtrl and (Msg.CharCode = 10) then
    Exit;

  if (EditType in [etMoney, etNumeric, etFloat]) and not FSigned and (msg.charcode = ord('-')) then
    Exit;

  case EditType of
    etString, etPassword:
      begin
        SetModified(true);
        inherited;
      end;
    etAlphaNumeric:
      begin
        if msg.charcode in AlphaNum_Codes + Ctrl_Codes then
        begin
          SetModified(true);
          inherited;
        end;
      end;
    etValidChars:
      begin
        if (pos(chr(msg.CharCode), ValidChars) > 0) or (msg.CharCode = 8) then
        begin
          SetModified(true);
          inherited;
        end;
      end;
    etNumeric:
      begin
        if (msg.CharCode = ord('-')) then
        begin
          if (SelLength = Length(self.Text)) then
          begin
            inherited Text := '-';
            SelStart := 1;
            Exit;
          end;

          s := self.Text;
          oldSelStart := SelStart;
      // oldSelLength := SelLength;
          if (Pos('-', s) > 0) then
          begin
            delete(s, 1, 1);
            inherited Text := s + Suffix;
            if (oldSelStart > 0) and (oldSelStart > Length(fPrefix)) then
              SelStart := oldSelStart - 1
            else
              SelStart := Length(Prefix);
            SelLength := 0;
          end
          else
          begin
            inherited Text := '-' + self.Text + Suffix;
            SelLength := 0;
            SelStart := oldSelStart + 1;
            SetModified(true);
          end;
      // SelLength := oldSelLength;
        end
        else
        begin
          if (msg.charcode in Numeric_Codes + Ctrl_Codes) then
            inherited;

          if ((GetKeyState(vk_rcontrol) and $8000 = $8000) or
            (GetKeyState(vk_lcontrol) and $8000 = $8000)) then
            inherited;
        end;
      end;
    etHex: if msg.charcode in Hex_Codes + Ctrl_Codes then
      begin
        SetModified(true);
        inherited;
      end;
    etRange:
      begin
        if msg.charcode in Range_Codes + Ctrl_Codes then
        begin
          SetModified(true);
          s := (inherited Text) + ' ';
          if (msg.charcode in [ord('-'), ord(','), ord(';')]) then
          begin
            if (SelStart <= Length(fPrefix)) then Exit;
            if (SelStart > Length(fPrefix)) and (CheckSeparator(s[SelStart])) then
              Exit;
            if (SelStart > Length(fPrefix)) and (CheckSeparator(s[SelStart + 1])) then
              Exit;
            inherited;
          end
          else
            inherited;
        end;
      end;
    etMoney:
      begin
        if (chr(msg.charcode) = decimalseparator) and ((Pos(decimalseparator, self.Text) > 0) or (FPrecision = 0)) then
        begin
          if (FPrecision > 0) then
          begin
            if SelLength = Length(Text) then
              Text := '0,0';
            SelectAfterDecimal;
          end;
          Exit;
        end;

        if (msg.charcode in Money_Codes + Ctrl_Codes) or (chr(msg.charcode) = decimalseparator) then
        begin
          if (chr(msg.charcode) = thousandseparator) or (chr(msg.charcode) = decimalseparator) then
          begin
            if scandistance(self.Text, SelStart) then
              Exit;
          end;

          if scanprecision(self.Text, SelStart) and (msg.charcode in [$30..$39, ord(decimalseparator)]) and (SelLength = 0) then
            begin
              if (FPrecision > 0) and (SelStart - Length(fprefix) >= Pos(decimalseparator, self.text))
                and (msg.charcode in [$30..$39]) and (SelStart - Length(fprefix) < Length(self.text)) then
              begin
                SelLength := 1;
              end
              else
                Exit;
            end;

          if (SelStart = 0) and (self.Text = '') and (msg.charcode = ord(decimalseparator)) then
            begin
              inherited Text := '0' + decimalseparator;
              SelStart := 2;
              SetModified(true);
              Exit;
            end;

          if (msg.charcode = ord('-')) and (SelLength = 0) then
          begin
            s := self.Text;
            oldprec := FPrecision;
            FPrecision := 0;
            oldSelStart := SelStart;

            if (Pos('-', s) <> 0) then
            begin
              delete(s, 1, 1);
              inherited Text := s + Suffix;
              SetModified(true);
              if (oldSelStart > 0) and (oldSelStart > Length(fPrefix)) then
                SelStart := oldSelStart - 1
              else
                SelStart := Length(Prefix);
            end
            else
            begin
              if (Floatvalue <> 0) or (1 > 0) then
              begin
                inherited Text := '-' + self.Text + Suffix;
                SelLength := 0;
                SelStart := oldSelStart + 1;
                SetModified(true);
              end;
            end;
            FPrecision := oldprec;
            Exit;
          end;

          inherited;

          if (self.Text <> '') and (self.Text <> '-') and
            (chr(msg.charcode) <> decimalseparator) then
          begin
            if inherited Modified then SetModified(true);

            AutoSeparators;

          end;
        end;
      end;
    etFloat:
      begin
        if (msg.charcode = ord(',')) and (DecimalSeparator <> ',') and (ThousandSeparator <> ',') then
          Exit;
        if (msg.charcode = ord('.')) and (DecimalSeparator <> '.') and (ThousandSeparator <> '.') then
          Exit;

        if (msg.charcode in Float_Codes + Ctrl_Codes) then
        begin
          if (chr(msg.charcode) = decimalseparator) and
            (Pos(decimalseparator, self.getseltext) = 0) and
            ((Pos(decimalseparator, self.Text) > 0) or (FPrecision = 0)) then
          begin
            if (FPrecision > 0) then SelectAfterDecimal;
            Exit;
          end;

          if ((msg.charcode = ord(',')) and (Pos(',', self.Text) > 0) and (Pos(',', self.getSelText) = 0)) and
            (chr(msg.charcode) <> thousandseparator) then exit;

          if (chr(msg.charcode) = thousandseparator) or (chr(msg.charcode) = decimalseparator) then
          begin
            if scandistance(self.Text, SelStart) then exit;
          end;

          if ScanPrecision(self.text, SelStart) and (msg.charcode in [$30..$39, ord(decimalseparator)]) and (SelLength = 0) then
            begin
              if (FPrecision > 0) and (SelStart - Length(fprefix) >= Pos(decimalseparator, self.Text))
                and (msg.CharCode in [$30..$39]) and (SelStart - Length(fprefix) < Length(self.Text)) then
              begin
                SelLength := 1;
              end
              else
                Exit;
            end;

          if (SelStart = 0) and (self.Text = '') and (msg.charcode = ord(decimalseparator)) then
            begin
              inherited Text := '0' + decimalseparator;
              SelStart := 2;
              SetModified(true);
              Exit;
            end;

          if (msg.charcode = ord('-')) and (SelLength = 0) then
          begin
            s := self.Text;
            oldprec := FPrecision;
            FPrecision := 0;
            oldSelStart := SelStart;

            if (Pos('-', s) <> 0) then
            begin
              delete(s, 1, 1);
              inherited Text := s + Suffix;
              if (oldSelStart > 0) and (oldSelStart > Length(fPrefix)) then
                SelStart := oldSelStart - 1
              else
                SelStart := Length(Prefix);
              SetModified(true);
            end
            else
            begin
              if (floatvalue <> 0) or (1 > 0) then
              begin
                inherited Text := '-' + self.text + Suffix;
                SelLength := 0;
                SelStart := oldSelStart + 1;
                SetModified(true);
              end;
            end;
            FPrecision := oldprec;
            Exit;
          end;
          inherited;
        end;
      end;
    etUppercase:
      begin
        s := AnsiUpperCase(chr(msg.charcode));
        msg.charcode := ord(s[1]);
        inherited;
      end;
    etLowercase:
      begin
        s := AnsiLowerCase(chr(msg.charcode));
        msg.charcode := ord(s[1]);
        inherited;
      end;
    etMixedCase:
      begin
        oldSelStart := SelStart;
        inherited;
        inherited Text := ShiftCase(self.text);
        SelStart := oldSelStart + 1;
      end;
  end;

  if inherited Modified then
    SetModified(true);

  UpdateLookup;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMCut(var Message: TWMCut);
var
  Allow: Boolean;
begin
  if not FEditorEnabled or ReadOnly then Exit;

  Allow := True;
  FBlockCopy := True;
  if Assigned(FOnClipboardCut) then
  begin
    FOnClipboardCut(self, copy(self.text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  end;
  if Allow then inherited;

  FBlockCopy := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMPaste(var Message: TWMPaste);
var
{$IFNDEF DELPHI_UNICODE}
  Data: THandle;
  content: PChar;
{$ENDIF}
  newstr: string;
  newss, newsl, i: Integer;
  allow: Boolean;

  function InsertString(s: string): string;
  var
    ss: Integer;
  begin
    Result := self.text;
    ss := SelStart - Length(fPrefix);
    if (SelLength = 0) then
    begin
      insert(s, result, ss + 1);
      newsl := 0;
      newss := ss + Length(s) + Length(fPrefix);
    end
    else
    begin
      delete(result, ss + 1, SelLength);
      insert(s, result, ss + 1);
      newsl := Length(s);
      newss := ss + Length(fPrefix);
    end;
  end;

begin
  if ReadOnly then
    Exit;

  {$IFDEF DELPHI_UNICODE}
  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    Allow := True;
    newstr := InsertString(Clipboard.AsText);
  {$ENDIF}


{$IFNDEF DELPHI_UNICODE}

  if ClipBoard.HasFormat(CF_TEXT) then
  begin
    ClipBoard.Open;
    Data := GetClipBoardData(CF_TEXT);
    try
      if Data <> 0 then
        Content := PChar(GlobalLock(Data))
      else
        Content := nil
    finally
      if Data <> 0 then
        GlobalUnlock(Data);
      ClipBoard.Close;
    end;

    if Content = nil then
      Exit;

    Allow := True;

    newstr := InsertString(StrPas(Content));

{$ENDIF}

    if Assigned(FOnClipboardPaste) then
      FOnClipboardPaste(self, newstr, Allow);

    if not Allow then Exit;

    if MaxLength > 0 then
    begin
{$IFNDEF DELPHI_UNICODE}
      if Length(StrPas(Content)) + Length(Self.Text) - SelLength > MaxLength then
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
    if Length(Clipboard.AsText) + Length(Self.Text) - SelLength > MaxLength then
{$ENDIF}
        Exit;
    end;

    case FEditType of
      etAlphaNumeric:
        begin
          Allow := True;
          for i := 1 to length(newstr) do
            if not (ord(newstr[i]) in AlphaNum_Codes) then
              Allow := False;

          if Allow then
          begin
            Self.Text := newstr;
            SetModified(True);
          end;
        end;
      etNumeric:
        begin
          if IsType(newstr) = atNumeric then
          begin
            if not (not Signed and (pos('-', newstr) > 0)) then
            begin
              self.Text := newstr;
              SetModified(True);
            end;
          end;
        end;
      etFloat, etMoney:
        begin
          if IsType(newstr) in [atFloat, atNumeric] then
          begin
{$IFNDEF DELPHI_UNICODE}
            if not ((FPrecision = 0) and (Pos(DecimalSeparator, StrPas(Content)) > 0)) then
{$ENDIF}
{$IFDEF DELPHI_UNICODE}
            if not ((FPrecision = 0) and (Pos(DecimalSeparator, Clipboard.AsText) > 0)) then
{$ENDIF}
              begin
                if not (not Signed and (pos('-', newstr) > 0)) then
                begin
                  self.Text := newstr;
                  Floatvalue := Floatvalue;
                  SetModified(True);
                end;
              end;
          end;
        end;
      etString, etPassWord: self.Text := NewStr;
      etLowerCase: self.Text := AnsiLowerCase(NewStr);
      etUpperCase: self.Text := AnsiUpperCase(NewStr);
      etMixedCase: self.Text := ShiftCase(NewStr);
      etValidChars:
        begin
          Allow := true;
          for i := 1 to length(newstr) do
          begin
            if pos(newstr[i], ValidChars) = 0 then
            begin
              Allow := false;
              break;
            end;
          end;
          if Allow then
          begin
            self.Text := newstr;
            SetModified(True);
          end;
        end;
    end;

    if (FEditType = etMoney) and (Length(self.Text) > 3) then
      SelectAll
    else
    begin
      SelStart := newss;
      SelLength := newsl;
    end;

    if FEditType in [etString, etPassWord, etLowerCase, etUpperCase, etMixedCase] then
      SetModified(true);
  end;

  UpdateLookup;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.LabelClick(Sender: TObject);
begin
  if Assigned(FOnLabelClick) then
    FOnLabelClick(Self);
end;

procedure TAdvCustomDropDown.LabelDblClick(Sender: TObject);
begin
  if Assigned(FOnLabelDblClick) then
    FOnLabelDblClick(Self);
end;

procedure TAdvCustomDropDown.LabelFontChange(Sender: TObject);
begin
  if FLabel <> nil then
  begin
    UpdateLabel;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      ParentFont := false;
  end;
end;

procedure TAdvCustomDropDown.Loaded;
begin
  inherited;
  FParentFnt := self.ParentFont;

  SetEditRect;
  ResizeControl;

  if Assigned(FButton) then
    FButton.Enabled := Enabled;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
    FLabel.Enabled := Enabled;

  if (FLabel <> nil) then
    UpdateLabel;

  if ParentFont and Assigned(FLabel) then
    FLabel.Font.Assign(Font);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FButton) then
    FButton.Enabled := self.Enabled;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMEnter(var Message: TCMGotFocus);
begin
  if AutoSelect and not (csLButtonDown in ControlState) then
    SelectAll;
  inherited;
  DrawBorders;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMExit(var Message: TCMExit);
begin
  inherited;
  DrawBorders;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    DrawBorders;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    DrawBorders;
  end;
end;

procedure TAdvCustomDropDown.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;

procedure TAdvCustomDropDown.CMVisibleChanged(var Message: TMessage);
begin
  inherited;
  if LabelCaption <> '' then
    FLabel.Visible := Self.Visible;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetEtched(const Value: Boolean);
begin
  if FEtched <> value then
  begin
    FEtched := Value;
    FButton.FButton.Etched:=value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.Is3DBorderControl: Boolean;
begin
  if csDesigning in ComponentState then
    Result := False
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DoEnter;
begin
  inherited;
  SetEditRect;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawControlBorder(DC: HDC);
var
  ARect:TRect;
  BtnFaceBrush, WindowBrush: HBRUSH;
begin
  if (csDesigning in ComponentState) then
    Exit;

  if not Enabled and FIsThemed and DisabledBorder then
  begin
    BtnFaceBrush := CreateSolidBrush(ColorToRGB(clSilver));
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    FrameRect(DC, ARect, BtnFaceBrush);
    DeleteObject(BtnFaceBrush);
    Exit;
  end;

  if (FBorderColor <> clNone) then
  begin
    if (GetFocus <> Handle) or (FFocusBorderColor = clNone) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
      Exit;
    end;
  end;

  if (FFocusBorderColor <> clNone) then
  begin
    if (GetFocus = Handle) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if Is3DBorderControl then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((parent as TWinControl).brush.color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderControl then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawButtonBorder;
begin
  FButton.FButton.Focused := Is3DBorderButton;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawBorders;
var
  DC: HDC;
begin
//  if not FFlat then Exit;
  if Enabled and not (FFlat or (FBorderColor <> clNone) {or (FFocusBorder and FMouseInControl) or Border3D} or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
    DrawButtonBorder;
  finally
    ReleaseDC(DC, Handle);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WndProc(var Message: TMessage);
begin
  if Message.Msg = EM_SETREADONLY then
  begin
    inherited;
    //FButton.Enabled := not ReadOnly or DropDownEnabled;
    if Assigned(FButton) then    
      FButton.Invalidate;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WriteSelectionColorStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FSelectionColorStyle));
end;

procedure TAdvCustomDropDown.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMFontChanged(var Message: TMessage);
begin
  inherited;
  if HandleAllocated then
    SetEditRect;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMSetFocus(var Message: TWMSetFocus);
begin
  if EditorEnabled or true then
  begin
    if csLoading in ComponentState then
      Exit;

    inherited;

    if not EditorEnabled then
    begin
      HideCaret(Handle);
      Invalidate;
      Exit;
    end;

    FOldString := Self.Text;

    {
    if FFocusColor <> clNone then
    begin
      inherited Color := FFocusColor;
      if FIsWinXP then
      begin
        Width := Width - 1;
        Width := Width + 1;
      end;
    end;
    }
    if AutoSelect then
      SelectAll
    else
      if EditType in [etFloat, etMoney] then
        SelectBeforeDecimal;

    {FOldEditAlign := FEditAlign;

    if (FEditAlign <> FFocusAlign) and (FFocusAlign <> eaDefault) then
    begin
      FAlignChanging := True;
      EditAlign := FFocusAlign;
      FAlignChanging := False;
    end;

    if ((EmptyText <> '') and (Text = '')) or (FocusBorder) or (FFocusBorderColor <> clNone) then
    begin
      if BorderStyle = bsNone then
        SetFlatRect(true);
      Invalidate;
    end;
    }
  end
  else
    FButton.SetFocus;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDropDownHeader(
  const Value: THeaderAppearance);
begin
  FDropDownHeader.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDropDownHeight(const Value: Integer);
begin
  if (FDropDownHeight <> Value) and (Value >= 0) then
  begin
    FDropDownHeight := Value;
    FUserDropDownHeight := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDropDownWidth(const Value: Integer);
begin
  if (FDropDownWidth <> Value) then
  begin
    FDropDownWidth := Value;
    FUserDropDownWidth := -1;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetButtonAppearance(
  const Value: TAdvButtonAppearance);
begin
  FButtonAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDropDownFooter(const Value: TFooterAppearance);
begin
  FDropDownFooter.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetColorTones(ATones: TColorTones);
begin
  Button.ButtonColor := ATones.Background.BrushColor;
  Button.ButtonColorHot := ATones.Hover.BrushColor;
  Button.ButtonColorDown := ATones.Selected.BrushColor;
  Button.ButtonBorderColor := ATones.Background.BorderColor;
  BorderColor := ATones.Background.BorderColor;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  case AStyle of
    tsOffice2003Blue:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := clNone;

        DropDownHeader.Color := $FCE1CB;
        DropDownHeader.ColorTo := $E0A57D;
        DropDownHeader.BorderColor := clBlack;
        DropDownHeader.Font.Color := clBlack;

        DropDownFooter.Color := $FCE1CB;
        DropDownFooter.ColorTo := $E0A57D;
        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := $962D00;

      end;
    tsOffice2003Silver:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := clNone;

        DropDownHeader.Color := $ECE2E1;
        DropDownHeader.ColorTo := $B39698;

        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := $947C7C;

        DropDownFooter.Color := $ECE2E1;
        DropDownFooter.ColorTo := $B39698;

        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := $947C7C;

      end;
    tsOffice2003Olive:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := clNone;

        DropDownBorderColor := clNone;

        DropDownHeader.Color := $CFF0EA;
        DropDownHeader.ColorTo := $8CC0B1;

        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := $588060;

        DropDownFooter.Color := $CFF0EA;
        DropDownFooter.ColorTo := $8CC0B1;

        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := $588060;

        //DropDownFooter.Font.Color := clWhite;
        //DropDownFooter.BorderColor := $588060;

      end;
    tsOffice2003Classic:
      begin
        DropDownColor := $00F2F2F2;
        DropDownColorTo := $00F2F2F2;
        DropDownBorderColor := clNone;

        DropDownHeader.Color := $808080;
        DropDownHeader.ColorTo := $808080;
        DropDownHeader.Font.Color := clWhite;
        DropDownHeader.BorderColor := $808080;

        DropDownFooter.Color := $808080;
        DropDownFooter.ColorTo := $808080;
        DropDownFooter.Font.Color := clWhite;
        DropDownFooter.BorderColor := $808080;

      end;
    tsOffice2007Luna:
      begin
        DropDownColor := $00F3E5DA;
        DropDownColorTo := $00F0DED0;
        DropDownBorderColor := clNone;

        DropDownHeader.Color := $FFEFE3;
        DropDownHeader.ColorTo := $FFD2AF;
        DropDownHeader.Font.Color := $723708;
        DropDownHeader.BorderColor := $00FFD2AF;

        DropDownFooter.Color := $FFEFE3;
        DropDownFooter.ColorTo := $FFD2AF;
        DropDownFooter.Font.Color := $723708;
        DropDownFooter.BorderColor := $00FFD2AF;

      end;
    tsOffice2007Obsidian:
      begin
        //DropDownColor := $5C534C;
        //DropDownColorTo := $5C534C;
        DropDownColor := clWhite;
        DropDownColorTo := clNone;


        DropDownBorderColor := clNone;

        DropDownHeader.Color := $F2F1F0;
        DropDownHeader.ColorTo := $C9C2BD;
        DropDownHeader.Font.Color := $433C37;
        DropDownHeader.BorderColor := $5C534C;

        DropDownFooter.Color := $F2F1F0;
        DropDownFooter.ColorTo := $C9C2BD;
        DropDownFooter.Font.Color := $433C37;
        DropDownFooter.BorderColor := $5C534C;
      end;
    tsWindowsXP:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := clNone;
        DropDownHeader.Color := clWhite;
        DropDownHeader.ColorTo := clBtnFace;
        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := clGray;
        DropDownFooter.Color := clWhite;
        DropDownFooter.ColorTo := clBtnFace;
        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := clGray;
      end;
    tsWhidbey:
      begin
        DropDownColor := $F5F9FA;
        DropDownColorTo := $F5F9FA;
        DropDownBorderColor := clNone;

        DropDownHeader.Color := $EBEEEF;
        DropDownHeader.ColorTo := $7E9898;
        DropDownHeader.Font.Color := clWhite;
        DropDownHeader.BorderColor := $962D00;

        DropDownFooter.Color := $EBEEEF;
        DropDownFooter.ColorTo := $7E9898;
        DropDownFooter.Font.Color := clWhite;
        DropDownFooter.BorderColor := $962D00;

      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        DropDownColor := RGB(241, 244, 248);
        DropDownColorTo := RGB(227, 232, 240);
        DropDownBorderColor := clNone;

        DropDownHeader.Color := $F8F7F6;
        DropDownHeader.ColorTo := $E8E0DB;
        DropDownHeader.Font.Color := $8B4215;
        DropDownHeader.BorderColor := $74706F;

        DropDownFooter.Color := $F8F7F6;
        DropDownFooter.ColorTo := $E8E0DB;
        DropDownFooter.Font.Color := $8B4215;
        DropDownFooter.BorderColor := $74706F;

      end;
      tsWindowsVista:
      begin
        DropDownColor := RGB(255, 255, 255);
        DropDownColorTo := RGB(255, 255, 255);
        DropDownBorderColor := RGB(151, 151, 151);

        DropDownHeader.Color := $FFFDF9;
        DropDownHeader.ColorTo := $FFFAF0;
        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := $FCF2DA;

        DropDownFooter.Color := $FFFAF0;
        DropDownFooter.ColorTo := $FFFDF9;
        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := $FCF2DA;

      end;
      tsWindows7:
      begin
        DropDownColor := RGB(255, 255, 255);
        DropDownColorTo := RGB(255, 255, 255);
        DropDownBorderColor := RGB(151, 151, 151);

        DropDownHeader.Color := $FDFBFA;
        DropDownHeader.ColorTo := $FDF3EB;
        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := $FBD6B8;

        DropDownFooter.Color := $FDFBFA;
        DropDownFooter.ColorTo := $FDF3EB;
        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := $FBD6B8;

      end;
      tsTerminal:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := clWhite;
        DropDownBorderColor := clGray;

        DropDownHeader.Color := clBtnFace;
        DropDownHeader.ColorTo := clBtnFace;
        DropDownHeader.Font.Color := clBlack;
        DropDownHeader.BorderColor := clNone;

        DropDownFooter.Color := clBtnFace;
        DropDownFooter.ColorTo := clBtnFace;
        DropDownFooter.Font.Color := clBlack;
        DropDownFooter.BorderColor := clNone;
      end;
      tsOffice2010Blue:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := RGB(237, 239, 241);
        DropDownBorderColor := RGB(236, 237, 237);

        DropDownHeader.Color := $FDF6EF;
        DropDownHeader.ColorTo := $F0DAC7;
        DropDownHeader.Font.Color := $5B391E;
        DropDownHeader.BorderColor := $C7B29F;

        DropDownFooter.Color := $FDF6EF;
        DropDownFooter.ColorTo := $F0DAC7;
        DropDownFooter.Font.Color := $5B391E;
        DropDownFooter.BorderColor := $C7B29F;
      end;
      tsOffice2010Silver:
      begin

        DropDownColor := clWhite;
        DropDownColorTo := RGB(237, 239, 241);
        DropDownBorderColor := RGB(236, 237, 237);

        DropDownHeader.Color := $FFFFFF;
        DropDownHeader.ColorTo := $EDE5E0;
        DropDownHeader.Font.Color := $5B391E;
        DropDownHeader.BorderColor := $D2CDC8;

        DropDownFooter.Color := $FFFFFF;
        DropDownFooter.ColorTo := $EDE5E0;
        DropDownFooter.Font.Color := $5B391E;
        DropDownFooter.BorderColor := $D2CDC8;
      end;
      tsOffice2010Black:
      begin
        DropDownColor := clWhite;
        DropDownColorTo := RGB(237, 239, 241);
        DropDownBorderColor := RGB(236, 237, 237);

        DropDownHeader.Color := $BFBFBF;
        DropDownHeader.ColorTo := $919191;
        DropDownHeader.Font.Color := $D7D7D6;
        DropDownHeader.BorderColor := $6D6D6D;

        DropDownFooter.Color := $BFBFBF;
        DropDownFooter.ColorTo := $919191;
        DropDownFooter.Font.Color := $D7D7D6;
        DropDownFooter.BorderColor := $6D6D6D;
      end;
  tsWindows8, tsWindows10:
    begin

      DropDownHeader.Color := $F7F6F5;
      DropDownHeader.ColorTo := $F7F6F5;
      DropDownHeader.Font.Color := clBlack;
      DropDownHeader.BorderColor := $E4E3E2;

      DropDownFooter.Color := $F7F6F5;
      DropDownFooter.ColorTo := $F7F6F5;
      DropDownFooter.Font.Color := clBlack;
      DropDownFooter.BorderColor := $E4E3E2;


      DropDownColor := clWhite;
      DropDownColorTo := clWhite;
      DropDownBorderColor := $E4E3E2;
    end;
        tsOffice2013White:
    begin
      DropDownHeader.Color := clWhite;
      DropDownHeader.ColorTo := clNone;
      DropDownHeader.Font.Color := clBlack;
      DropDownHeader.BorderColor := $D4D4D4;

      DropDownFooter.Color := clWhite;
      DropDownFooter.ColorTo := clNone;
      DropDownFooter.Font.Color := clBlack;
      DropDownFooter.BorderColor := $D4D4D4;

      DropDownColor := clWhite;
      DropDownColorTo := clNone;
      DropDownBorderColor := $D4D4D4;
    end;
        tsOffice2013LightGray:
    begin
      DropDownHeader.Color := $F6F6F6;
      DropDownHeader.ColorTo := clNone;
      DropDownHeader.Font.Color := clBlack;
      DropDownHeader.BorderColor := $C6C6C6;

      DropDownFooter.Color := $F6F6F6;
      DropDownFooter.ColorTo := clNone;
      DropDownFooter.Font.Color := clBlack;
      DropDownFooter.BorderColor := $C6C6C6;

      DropDownColor := clWhite;
      DropDownColorTo := clNone;
      DropDownBorderColor := $D4D4D4;
    end;
        tsOffice2013Gray:
    begin
      DropDownHeader.Color := $E5E5E5;
      DropDownHeader.ColorTo := clNone;
      DropDownHeader.Font.Color := clBlack;
      DropDownHeader.BorderColor := $ABABAB;

      DropDownFooter.Color := $E5E5E5;
      DropDownFooter.ColorTo := clNone;
      DropDownFooter.Font.Color := clBlack;
      DropDownFooter.BorderColor := $ABABAB;

      DropDownColor := clWhite;
      DropDownColorTo := clNone;
      DropDownBorderColor := $D4D4D4;
    end;
  tsOffice2016White:
    begin
        DropDownHeader.Color := clWhite;
        DropDownHeader.ColorTo := clNone;
        DropDownHeader.Font.Color := $505050;
        DropDownHeader.BorderColor := $D4D4D4;

        DropDownFooter.Color := clWhite;
        DropDownFooter.ColorTo := clNone;
        DropDownFooter.Font.Color := $505050;
        DropDownFooter.BorderColor := $D4D4D4;

        DropDownColor := clWhite;
        DropDownColorTo := clNone;
        DropDownBorderColor := $D4D4D4;
    end;
        tsOffice2016Gray:
    begin
        DropDownHeader.Color := $444444;
        DropDownHeader.ColorTo := clNone;
        DropDownHeader.Font.Color := $F0F0F0;
        DropDownHeader.BorderColor := $444444;

        DropDownFooter.Color := $444444;
        DropDownFooter.ColorTo := clNone;
        DropDownFooter.Font.Color := $F0F0F0;
        DropDownFooter.BorderColor := $444444;

        DropDownColor := $B2B2B2;
        DropDownColorTo := clNone;
        DropDownBorderColor := $444444;
    end;
        tsOffice2016Black:
    begin
        DropDownHeader.Color := $444444;
        DropDownHeader.ColorTo := clNone;
        DropDownHeader.Font.Color := $F0F0F0;
        DropDownHeader.BorderColor := $444444;

        DropDownFooter.Color := $444444;
        DropDownFooter.ColorTo := clNone;
        DropDownFooter.Font.Color := $F0F0F0;
        DropDownFooter.BorderColor := $444444;

        DropDownColor := $363636;
        DropDownColorTo := clNone;
        DropDownBorderColor := $444444;;
    end;


  end;

   case AStyle of
    tsOffice2003Blue:
      begin
        //ButtonAppearance.Color := $EEDBC8;
        //ButtonAppearance.ColorTo := $F6DDC9;

        ButtonAppearance.Color := $FCE1CB;
        ButtonAppearance.ColorTo := $E0A57D;

        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
    tsOffice2003Silver:
      begin
        ButtonAppearance.Color := $ECE2E1;
        ButtonAppearance.ColorTo := $B39698;

        //ButtonAppearance.Color := $E6E9E2;
        //ButtonAppearance.ColorTo := $00E6D8D8;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
    tsOffice2003Olive:
      begin
        ButtonAppearance.Color := $CFF0EA;
        ButtonAppearance.ColorTo := $8CC0B1; //CFF0EA
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
    end;
    tsOffice2003Classic:
      begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := $C9D1D5;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $D2BDB6;
        ButtonAppearance.ColorHotTo := $D2BDB6;
        ButtonAppearance.BorderColorHot :=  $808080;

        ButtonAppearance.ColorDown := $B59285;
        ButtonAppearance.ColorDownTo := $B59285;
        ButtonAppearance.BorderColorDown := $808080;

        ButtonAppearance.ColorDisabled := $D8D5D4;
        ButtonAppearance.ColorDisabledTo := $D8D5D4;

        ButtonAppearance.ColorChecked := $D8D5D4;
        ButtonAppearance.ColorCheckedTo := $D8D5D4;
        ButtonAppearance.BorderColorChecked := $808080;
      end;
    tsOffice2007Luna:
      begin
        ButtonAppearance.Color := $FFEFE3;
        ButtonAppearance.ColorTo := $FFDDC4;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
    tsOffice2007Obsidian:
      begin
        ButtonAppearance.Color := $F2F1F0;
        ButtonAppearance.ColorTo := $C9C2BD;

        //ButtonAppearance.Color := $F9F8F8;
        //ButtonAppearance.ColorTo := $E4E2DF;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
    tsWindowsXP:
      begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := clBtnFace;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EFD3C6;
        ButtonAppearance.ColorHotTo := $EFD3C6;
        ButtonAppearance.BorderColorHot :=  clHighlight;

        ButtonAppearance.ColorDown := clInactiveCaption;
        ButtonAppearance.ColorDownTo := clInactiveCaption;
        ButtonAppearance.BorderColorDown := clHighLight;

        ButtonAppearance.ColorDisabled := $00B6B6B6;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $B9D8DC;
        ButtonAppearance.ColorCheckedTo := $B9D8DC;
        ButtonAppearance.BorderColorChecked := clBlack;
      end;
    tsWhidbey:
      begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := $DFEDF0;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        ButtonAppearance.Color := $F9F8F8;
        ButtonAppearance.ColorTo := $E4E2DF;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := $EBFDFF;
        ButtonAppearance.ColorHotTo := $ACECFF;
        ButtonAppearance.BorderColorHot :=  $99CEDB;

        ButtonAppearance.ColorDown := $AAD9FF;
        ButtonAppearance.ColorDownTo := $6EBBFF;
        ButtonAppearance.BorderColorDown := $42AEFE;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := $76AFF1;
        ButtonAppearance.ColorCheckedTo := $4190F3;
        ButtonAppearance.BorderColorChecked := $45667B;
      end;
      tsWindowsVista:
      begin
        ButtonAppearance.Color := $FFFDF9;
        ButtonAppearance.ColorTo := $FFFAF0;
        ButtonAppearance.BorderColor := $FCF2DA;

        ButtonAppearance.ColorHot := $FEF9F0;
        ButtonAppearance.ColorHotTo := $FFFAF0;
        ButtonAppearance.BorderColorHot :=  $FCF2DA;

        ButtonAppearance.ColorDown := $FEF9F0;
        ButtonAppearance.ColorDownTo := $FDF0D7;
        ButtonAppearance.BorderColorDown := $FEDF9A;

        ButtonAppearance.ColorDisabled := $FFFFFF;
        ButtonAppearance.ColorDisabledTo := $FFFFFF;

        ButtonAppearance.ColorChecked := $FEF9F0;
        ButtonAppearance.ColorCheckedTo := $FDF0D7;
        ButtonAppearance.BorderColorChecked := $FEDF9AB;
      end;
      tsWindows7:
      begin
        ButtonAppearance.Color := $FDFBFA;
        ButtonAppearance.ColorTo := $FDF3EB;
        ButtonAppearance.BorderColor := $FBD6B8;

        ButtonAppearance.ColorHot := $FDFBFA;
        ButtonAppearance.ColorHotTo := $FDF3EB;
        ButtonAppearance.BorderColorHot :=  $FBD6B8;

        ButtonAppearance.ColorDown := $FCEBDC;
        ButtonAppearance.ColorDownTo := $FCDBC1;
        ButtonAppearance.BorderColorDown := $CEA27D;

        ButtonAppearance.ColorDisabled := $FFFFFF;
        ButtonAppearance.ColorDisabledTo := $FFFFFF;

        ButtonAppearance.ColorChecked := $FEF9F0;
        ButtonAppearance.ColorCheckedTo := $FCDBC1;
        ButtonAppearance.BorderColorChecked := $CEA27D;
      end;
      tsTerminal:
      begin
        ButtonAppearance.Color := clBtnFace;
        ButtonAppearance.ColorTo := clBtnFace;
        ButtonAppearance.BorderColor := clNone;

        ButtonAppearance.ColorHot := clSilver;
        ButtonAppearance.ColorHotTo := clSilver;
        ButtonAppearance.BorderColorHot :=  clGray;

        ButtonAppearance.ColorDown := clHighLight;
        ButtonAppearance.ColorDownTo := clHighLight;
        ButtonAppearance.BorderColorDown := clGray;

        ButtonAppearance.ColorDisabled := clBtnFace;
        ButtonAppearance.ColorDisabledTo := clBtnFace;

        ButtonAppearance.ColorChecked := clBtnFace;
        ButtonAppearance.ColorCheckedTo := clBtnFace;
        ButtonAppearance.BorderColorChecked := clHighlight;

      end;
        tsOffice2010Blue:
      begin
        ButtonAppearance.Color := $FDF6EF;
        ButtonAppearance.ColorTo := $F0DAC7;
        ButtonAppearance.BorderColor := $C7B29F;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot :=  $58CAF1;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $308AC2;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := RGB(254, 225, 69);
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := RGB(206, 160, 79);

       end;
        tsOffice2010Silver:
      begin
        ButtonAppearance.Color := $FFFFFF;
        ButtonAppearance.ColorTo := $EDE5E0;
        ButtonAppearance.BorderColor := $D2CDC8;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot :=  $58CAF1;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $308AC2;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := RGB(254, 225, 69);
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := RGB(206, 160, 79);
      end;
        tsOffice2010Black:
      begin
        ButtonAppearance.Color := $BFBFBF;
        ButtonAppearance.ColorTo := $919191;
        ButtonAppearance.BorderColor := $D7D7D6;

        ButtonAppearance.ColorHot := $8AE3FD;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot :=  $58CAF1;

        ButtonAppearance.ColorDown := $6CD0FF;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $308AC2;

        ButtonAppearance.ColorDisabled := $00F2F2F2;
        ButtonAppearance.ColorDisabledTo := $00B6B6B6;

        ButtonAppearance.ColorChecked := RGB(254, 225, 69);
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := RGB(206, 160, 79);
      end;
      tsWindows8, tsWindows10:
      begin
        ButtonAppearance.Color := $F7F6F5;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $E4E3E2;

        ButtonAppearance.ColorHot := $F7EFE8;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $F9CEA4;

        ButtonAppearance.ColorDown := $F7E0C9;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E4A262;

        ButtonAppearance.ColorChecked := $DAA026;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $DAA026;

        ButtonAppearance.ColorDisabled := $F7F7F7;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := ButtonAppearance.Color;
       end;

      tsOffice2013White:
      begin
        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $D4D4D4;

        ButtonAppearance.ColorHot := $FCF0E4;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $EAB47E;

        ButtonAppearance.ColorDown := $FCE2C8;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E59D56;

        ButtonAppearance.ColorChecked := $FF9933;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $FF9933;

        ButtonAppearance.ColorDisabled := $EEEEEE;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $ACACAC;

      end;

      tsOffice2013LightGray:
      begin
        ButtonAppearance.Color := $F6F6F6;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $C6C6C6;

        ButtonAppearance.ColorHot := $FCF0E4;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $EAB47E;

        ButtonAppearance.ColorDown := $FCE2C8;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E59D56;

        ButtonAppearance.ColorChecked := $FF9933;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $FF9933;

        ButtonAppearance.ColorDisabled := $EEEEEE;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $ACACAC;


      end;

      tsOffice2013Gray:
      begin
        ButtonAppearance.Color := $E5E5E5;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $ABABAB;

        ButtonAppearance.ColorHot := $FCF0E4;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $EAB47E;

        ButtonAppearance.ColorDown := $FCE2C8;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E59D56;

        ButtonAppearance.ColorChecked := $FF9933;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $FF9933;

        ButtonAppearance.ColorDisabled := $EEEEEE;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $ACACAC;

      end;

      tsOffice2016White:
      begin

        ButtonAppearance.Color := clWhite;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $D4D4D4;

        ButtonAppearance.ColorHot := $F2E1D5;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $F2E1D5;

        ButtonAppearance.ColorDown := $E3BDA3;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E3BDA3;

        ButtonAppearance.ColorChecked := $F2D5C2;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $F2D5C2;

        ButtonAppearance.ColorDisabled := clWhite;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $D4D4D4;

      end;

      tsOffice2016Gray:
      Begin
        ButtonAppearance.Color := $B2B2B2;
        ButtonAppearance.ColorTo := clNone;
        ButtonAppearance.BorderColor := $444444;

        ButtonAppearance.ColorHot := $F2E1D5;
        ButtonAppearance.ColorHotTo := clNone;
        ButtonAppearance.BorderColorHot := $F2E1D5;

        ButtonAppearance.ColorDown := $E3BDA3;
        ButtonAppearance.ColorDownTo := clNone;
        ButtonAppearance.BorderColorDown := $E3BDA3;

        ButtonAppearance.ColorChecked := $F2D5C2;
        ButtonAppearance.ColorCheckedTo := clNone;
        ButtonAppearance.BorderColorChecked := $F2D5C2;

        ButtonAppearance.ColorDisabled := $B2B2B2;
        ButtonAppearance.ColorDisabledTo := clNone;
        ButtonAppearance.BorderColorDisabled := $444444;
      End;

      tsOffice2016Black:
      Begin

        ButtonAppearance.Color := $363636;
        ButtonAppearance.ColorTo := $363636;
        ButtonAppearance.BorderColor := $444444;

        ButtonAppearance.ColorHot := $6A6A6A;
        ButtonAppearance.ColorHotTo := $6A6A6A;
        ButtonAppearance.BorderColorHot := $6A6A6A;

        ButtonAppearance.ColorDown := $444444;
        ButtonAppearance.ColorDownTo := $444444;
        ButtonAppearance.BorderColorDown := $444444;

        ButtonAppearance.ColorChecked := $575757;
        ButtonAppearance.ColorCheckedTo := $575757;
        ButtonAppearance.BorderColorChecked := $575757;

        ButtonAppearance.ColorDisabled := $363636;
        ButtonAppearance.ColorDisabledTo := clnone;
        ButtonAppearance.BorderColorDisabled := $444444;

        Font.Color := $A6A6A6;
      End;

  end;
end;

procedure TAdvCustomDropDown.SetControl(const Value: TControl);
begin
  if (FControl <> Value) then
  begin
    FControl := Value;
    if not (csDesigning in ComponentState) then
      SetCenterControl;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawSelectionBackground(Canvas: TCanvas; R: TRect; bkgcolor: TColor; part: TGradientPart);
begin
  case SelectionColorStyle of
    scWindowsVista: DrawSelectionGradient(Canvas, SelectionColor, SelectionColorTo,clNone, clNone, $FDFBF6,$FDF5E7,$FDDE99,$FCEDCB, bkgColor, R, part);
    scWindows7: DrawSelectionGradient(Canvas, SelectionColor, SelectionColorTo,clNone, clNone, $FDF4EB,$FDEADB,$CEA27D,$E0C5AE, bkgColor, R, part);
    scOffice2007: DrawSelectionGradient(Canvas, SelectionColor, SelectionColorTo,$58D4FC,$B3F1FC,$F4FEFF,$CAF8FF,$8DD7D3,$C3D9DF, bkgColor, R, part);
    //scCustom: DrawSelectionGradient(Canvas, SelectionColor, SelectionColorTo, SelectionColor, SelectionColorTo, $F4FEFF,$CAF8FF,$8DD7D3,$C3D9DF, bkgcolor, R, part);
    scCustom:
      begin
        Canvas.Brush.Color := SelectionColor;
        Canvas.Pen.Color := SelectionColor;
        Canvas.FillRect(R);
      end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawGradientBackground(Canvas: TCanvas;
  FromColor, ToColor: TColor; Steps: Integer; R: TRect;
  GradientDirection: TGradientDirection);
begin
  DrawGradient(Canvas, FromColor, ToColor, Steps, R, GradientDirection = gdHorizontal);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetSelectionColor(const Value: TColor);
begin
  FSelectionColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetSelectionColorStyle(
  const Value: TSelectionColorStyle);
begin
  //if (FSelectionColorStyle <> Value) then
  begin
    FSelectionColorStyle := Value;
    case FSelectionColorStyle of
      scWindowsVista:
      begin
        SelectionColor := $FDF8F1;
        SelectionColorTo := $FCEFD5;
      end;
      scWindows7:
      begin
        SelectionColor := $FCEBDC;
        SelectionColorTo := $FCDBC1;
      end;
      scOffice2007:
      begin
        SelectionColor := $D7FFFD;
        SelectionColorTo := $58D4FC;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetSelectionColorTo(const Value: TColor);
begin
  FSelectionColorTo := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetEditType(const Value: TDropDownEditType);
var
  at: TAutoType;
begin
  if FEditType <> Value then
  begin
    FEditType := Value;
    if FEditType = etPassword then
    begin
      PassWordChar := '*';
      FCanUndo := False;
    // FEditAlign := eaLeft;
      ReCreateWnd;
    end
    else
      Passwordchar := #0;

    at := IsType(self.Text);
    case FEditType of
      etHex: if not (at in [atNumeric, atHex]) then self.IntValue := 0;
      etNumeric: if (at <> atNumeric) then self.IntValue := 0;
      etFloat, etMoney: if not (at in [atFloat, atNumeric]) then self.FloatValue := 0.0;
    end;

    if (csDesigning in ComponentState) and (FEditType = etFloat) and (Precision = 0) then
      Precision := 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMKillFocus(var Msg: TWMKillFocus);
var
  OldModified: Boolean;
begin
  if (csLoading in ComponentState) then
    Exit;

  if (FPrecision > 0) and (EditType in [etFloat, etMoney]) then
  begin
    if (self.Text <> '') or not FAllowNumericNullValue then
    begin
      // update for precision
      OldModified := Modified;
      Floatvalue := self.Floatvalue;
      Modified := OldModified;
    end;
  end;

  if (EditType in [etNumeric]) and (Self.Text = '') and not FAllowNumericNullValue then
    Text := '0';

  if (EditType in [etFloat, etMoney]) and (Self.Text = '') and not FAllowNumericNullValue  then
    Floatvalue := 0.0;

  inherited;

  {
  if FFocusBorder then
    Invalidate;

  if FFocusWidthInc > 0 then
    Width := Width - FFocusWidthInc;


  if (FEditAlign <> FOldEditAlign) and (FFocusAlign <> eaDefault) then
  begin
    EditAlign := FOldEditAlign;
  end;

  if (EmptyText <> '') and (Text = '') then
    Invalidate;

  TT := Trim(Text);
  }
  if not EditorEnabled then
  begin
    Invalidate;
  end;

end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetText: string;
var
  s: string;
begin
  s := inherited Text;
  if (fPrefix <> '') and (Pos(fPrefix, s) = 1) then delete(s, 1, Length(fPrefix));
  if (fSuffix <> '') then delete(s, Length(s) - Length(fSuffix) + 1, Length(fSuffix));
  Result := s;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetText(Value: string);
var
  fmt, neg: string;
  f: extended;
begin
  if (value = '') then
  begin
    case FEditType of
      etFloat: if not (IsType(value) in [atFloat, atNumeric]) then value := '0';
      etMoney: if not (IsType(value) in [atFloat, atNumeric]) then value := '0';
      etHex: if not (IsType(value) in [atHex, atNumeric]) then value := '0';
      etNumeric: if not (IsType(value) in [atNumeric]) then value := '0';
    end;
  end;

  if (FPrecision > 0) and (Value <> '') and (Value <> ErrText) then
  begin
    if (FEditType in [etMoney]) then
    begin
      if (Pos('-', value) > 0) then neg := '-' else neg := '';
      fmt := '%.' + IntToStr(FPrecision) + 'n';
      Value := Format(fmt, [EStrToFloat(Value)]);
    end;

    if (FEditType in [etFloat]) then
    begin
      fmt := '%.' + inttostr(FPrecision) + 'f';
      f := EStrToFloat(value);
      Value := Format(fmt, [f]);
    end;
  end;

  if (FEditType in [etHex]) then
    Value := AnsiUpperCase(value);

  inherited Text := FPrefix + Value + FSuffix;

  FOldText := Value;

  SetModified(False);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetTextDirect(s: string);
begin
  inherited Text := s;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetParent(AParent: TWinControl);
begin
  inherited;
  if FLabel <> nil then
    FLabel.Parent := AParent;
end;

procedure TAdvCustomDropDown.SetPrecision(const Value: smallint);
var
  at: TAutoType;
begin
  if (FPrecision <> value) and (editType in [etFloat, etMoney, etString]) then
  begin
    FPrecision := Value;
    at := IsType(Text);
    if (at in [atFloat, atNumeric]) then
      FloatValue := FloatValue
    else
      FloatValue := 0.0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetPrefix(const Value: string);
var
  s: string;
begin
  s := self.Text;
  fPrefix := Value;
  inherited Text := s;
  Text := s;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetSuffix(const Value: string);
var
  s: string;
begin
  s := self.text;
  fSuffix := Value;
  inherited Text := s;
  Text := s;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SelectAll;
begin
  SelStart := 0;
  SelLength := Length(self.text);

  if (fPrefix <> '') then
  begin
    if (SelStart < Length(fPrefix)) then
    begin
      SelStart := Length(fPrefix);
      SelLength := Length(self.Text);
    end;
  end;

  if (fSuffix <> '') then
  begin
    SelStart := Length(fPrefix);
    SelLength := Length(self.Text);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SelectAfterDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.Text);

  if (i > 0) then
    SelStart := i + Length(fPrefix)
  else
    SelStart := Length(fPrefix);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SelectBeforeDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i > 0) then
    SelStart := i + Length(fPrefix) - 1
  else
    SelStart := Length(fPrefix);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMCopy(var Message: TWMCopy);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnClipboardCopy) and not FBlockCopy then
    FOnClipboardCopy(self, copy(self.Text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  FBlockCopy := False;  
  if Allow then inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMLButtonDblClk(var Message: TWMLButtonDblClk);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMLButtonDown(var Msg: TWMMouse);
var
  uchar: Integer;
begin
  if not EditorEnabled then
  begin
    inherited;
    MouseButtonDown(nil);
  end
  else
  begin
  {click outside selection}
    uchar := CharFromPos(point(msg.xpos, msg.ypos));

    if (SelLength <= 0) or (uchar < SelStart) or (uChar > SelStart + SelLength) or
      (GetFocus <> self.Handle) then
      inherited
    else
      if (uChar >= SelStart) and (uChar <= SelStart + SelLength) and (SelLength > 0) then
        FButtonDown := True;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMLButtonUp(var Msg: TWMMouse);
var
  uchar: Integer;

begin
  if not EditorEnabled or ReadOnly then
    DrawBackGround;
  
  if fButtonDown then
  begin
    uchar := CharFromPos(point(msg.xpos, msg.ypos));
    SelStart := uChar;
    SelLength := 0;
  end;

  fButtonDown := false;

  inherited;
  if (fPrefix <> '') then
  begin
    if (SelStart < Length(fPrefix)) then
    begin
      SelStart := Length(fPrefix);
      SelLength := Length(self.Text);
    end;
  end;
  if (fSuffix <> '') then
  begin
    if (SelStart > Length(self.text)) then
    begin
      SelStart := Length(self.Text);
      SelLength := 0;
    end;
    if (SelStart + SelLength > Length(self.text)) then
    begin
      SelLength := Length(self.Text) - SelStart;
    end;
  end;
end;

procedure TAdvCustomDropDown.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) {or (not Enabled and DisabledBorder)} then
    DrawBorders;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.AllowMin(ch: char): boolean;
begin
  Result := Signed and (EditType in [etFloat,etNumeric, etMoney]) and (ch = '-');
end;

procedure TAdvCustomDropDown.Assign(Source: TPersistent);
begin
  if (Source is TAdvCustomDropDown) then
  begin
    FFocusDraw := (Source as TAdvCustomDropDown).FocusDraw;
    FForceShadow := (Source as TAdvCustomDropDown).ForceShadow;
    FDropDownEnabled := (Source as TAdvCustomDropDown).DropDownEnabled;

    FDropDownColor := (Source as TAdvCustomDropDown).DropDownColor;
    FDropDownColorTo := (Source as TAdvCustomDropDown).DropDownColorTo;
    FDropDownGradient := (Source as TAdvCustomDropDown).DropDownGradient;
    FDropDownBorderColor := (Source as TAdvCustomDropDown).DropDownBorderColor;
    FDropDownBorderWidth := (Source as TAdvCustomDropDown).DropDownBorderWidth;
    FDropDownShadow := (Source as TAdvCustomDropDown).DropDownShadow;
    FDropDownWidth := (Source as TAdvCustomDropDown).DropDownWidth;
    FDropDownHeight := (Source as TAdvCustomDropDown).DropDownHeight;
    FDropDownSizeable := (Source as TAdvCustomDropDown).DropDownSizeable;

    FEditType := (Source as TAdvCustomDropDown).EditType;
    FReturnIsTab := (Source as TAdvCustomDropDown).ReturnIsTab;
    FLengthLimit := (Source as TAdvCustomDropDown).LengthLimit;
    FPrecision := (Source as TAdvCustomDropDown).Precision;
    FPrefix := (Source as TAdvCustomDropDown).Prefix;
    FSuffix := (Source as TAdvCustomDropDown).Suffix;
    FDefaultHandling := (Source as TAdvCustomDropDown).DefaultHandling;
    FCanUndo := (Source as TAdvCustomDropDown).CanUndo;
    FExcelStyleDecimalSeparator := (Source as TAdvCustomDropDown).ExcelStyleDecimalSeparator;
    FValidChars := (Source as TAdvCustomDropDown).ValidChars;

    FSigned := (Source as TAdvCustomDropDown).Signed;
    FAutoThousandSeparator := (Source as TAdvCustomDropDown).AutoThousandSeparator;

    FButtonAppearance.Assign((Source as TAdvCustomDropDown).ButtonAppearance);
    FDropDownHeader.Assign((Source as TAdvCustomDropDown).DropDownHeader);
    FDropDownFooter.Assign((Source as TAdvCustomDropDown).DropDownFooter);

    FDropPosition := (Source as TAdvCustomDropDown).DropPosition;
    FFlat := (Source as TAdvCustomDropDown).Flat;

    DropDownButtonWidth := (Source as TAdvCustomDropDown).DropDownButtonWidth;
    DropDownButtonHint := (Source as TAdvCustomDropDown).DropDownButtonHint;
    FEditorEnabled := (Source as TAdvCustomDropDown).EditorEnabled;

    DropDownButtonGlyph.Assign((Source as TAdvCustomDropDown).DropDownButtonGlyph);
    DropDownButtonCaption := (Source as TAdvCustomDropDown).DropDownButtonCaption;

    FEtched := (Source as TAdvCustomDropDown).Etched;
    FImages  := (Source as TAdvCustomDropDown).Images;

    FSelectionColorStyle := (Source as TAdvCustomDropDown).SelectionColorStyle;
    FSelectionColor := (Source as TAdvCustomDropDown).SelectionColor;
    FSelectionColorTo := (Source as TAdvCustomDropDown).SelectionColorTo;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.DecimalPos: Integer;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i = 0) then Result := Length(fprefix) + Length(self.text) + Length(fSuffix) + 1
  else Result := Length(fPrefix) + i;
end;

procedure TAdvCustomDropDown.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('SelectionColorStyle', ReadSelectionColorStyle, WriteSelectionColorStyle, True);
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.FixedLength(s: string): Integer;
var
  i: Integer;
begin
  s := StripThousandSep(s);
  i := Pos(decimalseparator, s);
  if (i > 0) then Result := i else Result := Length(s) + 1;

  if Signed and (EditType in [etFloat,etNumeric, etMoney]) and (pos('-',s) > 0) then
    Result := Result - 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetFloat(const Value: double);
var
  s:string;
begin
  case FEditType of
    etHex: self.Text := IntToHex(trunc(value), 0);
    etNumeric:
      if (FPrecision >= 0) then
        self.Text := Format('%.' + inttostr(FPrecision) + 'n', [value])
      else
        self.Text := Format('%g', [Value]);
    etFloat, etString:
      if (FPrecision >= 0) then
      begin
        s := Format('%.' + inttostr(FPrecision) + 'f', [value]);
        self.Text := s;
      end  
      else
        self.Text := Format('%g', [Value]);
    etMoney:
      begin
        if (FPrecision >= 0) then
          self.Text := Format('%.' + inttostr(FPrecision) + 'f', [value]) else self.Text := Format('%g', [Value]);
        AutoSeparators;
      end;
  end;
  SetModified(True);
end;

procedure TAdvCustomDropDown.SetFocusBorderColor(const Value: TColor);
begin
  FFocusBorderColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetInt(const Value: integer);
begin
  case FEditType of
    etHex: self.Text := IntToHex(value, 0);
    etNumeric: self.Text := Inttostr(value);
    etFloat: self.Text := Inttostr(value);
    etMoney:
      begin
        self.Text := IntToStr(value);
        AutoSeparators;
      end;
  end;
  SetModified(True);
end;

procedure TAdvCustomDropDown.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
end;

procedure TAdvCustomDropDown.SetLabelCaption(const Value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvCustomDropDown.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvCustomDropDown.SetLabelMargin(const Value: Integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomDropDown.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvCustomDropDown.SetLabelTransparent(const Value: Boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.AutoSeparators;
var
  s, si, neg: string;
  d: Double;
  Diffl, OldSelStart, OldPrec: Integer;

begin
  s := self.Text;
  Diffl := Length(s);
  OldSelStart := SelStart;

  if (s = '') then
    Exit;

  if (Pos('-', s) = 1) then
  begin
    Delete(s, 1, 1);
    neg := '-';
  end
  else
    neg := '';

  if (Pos(DecimalSeparator, s) > 0) then
    s := Copy(s, Pos(DecimalSeparator, s), 255)
  else
    s := '';

  d := Trunc(Abs(self.FloatValue));

  if FAutoThousandSeparator then
    si := Format('%n', [d])
  else
    si := Format('%f', [d]);

  si := Copy(si, 1, Pos(decimalseparator, si) - 1);

  OldPrec := FPrecision;
  FPrecision := 0;

  FBlockChange := (Text <> FPrefix + neg + si + s + fSuffix);

  inherited Text := FPrefix + neg + si + s + fSuffix;

  FPrecision := OldPrec;

  Diffl := Length(self.Text) - Diffl;

  SelStart := OldSelStart + Diffl;
  SelLength := 0;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetModified: boolean;
begin
  Result := FIsModified;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetModified(const Value: boolean);
begin
  if csLoading in ComponentState then
    Exit;

  if not EditorEnabled or ReadOnly then
    Exit;

  {
  if FShowModified then
  begin
    if (value = false) then
      self.Font.Color := FFontColor
    else
      self.Font.Color := FModifiedColor;
  end;
  }
  inherited Modified := value;
  FIsModified := value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) then
  begin
    if FBlockChange then
    begin
      FBlockChange := false;
      Exit;
    end;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  IsPrev: Boolean;
  p: TWinControl;
begin
  p := Parent;
  while Assigned(p) and not (p is TCustomForm) do
    p := p.Parent;

  if not Assigned(p) then
    Exit; 

  IsPrev := (p as TCustomForm).KeyPreview;

  if (Msg.CharCode = VK_ESCAPE) and FCanUndo and not IsPrev then
  begin
    //Text := FOldString;
    //Font.Color := FFocusFontColor;
    SelectAll;
    SetModified(False);
    Msg.CharCode := 0;
    Msg.Result := 0;
    // Take care of default key handling
    if (Parent is TWinControl) and FDefaultHandling then
    begin
      PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_ESCAPE, 0);
      PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_ESCAPE, 0);
    end;
  end;


  if (Msg.CharCode = VK_RETURN) and FDefaultHandling and not IsPrev then
  begin
    // Take care of default key handling
    if (Parent is TWinControl) then
    begin
      if (GetFocus = Parent.handle) then
      begin
        PostMessage((Parent as TWinControl).Handle, WM_KEYDOWN, VK_RETURN, 0);
        PostMessage((Parent as TWinControl).Handle, WM_KEYUP, VK_RETURN, 0);
      end;
    end;
  end;

  inherited;

  // wants return, escape, tab
//  if (Msg.CharCode = VK_RETURN) then
//    Msg.Result := 1;

  if (Msg.CharCode = VK_ESCAPE) and (DroppedDown) then
    Msg.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetCanUndo(const Value: boolean);
begin
  if FCanUndo <> Value then
  begin
    FCanUndo := Value;
  //CanUndo is not compatible with etPassword style
    if FCanUndo and (FEditType = etPassWord) then
      FCanUndo := False;
    ReCreateWnd;
  {force a proper re-alignment}
    self.Width := self.Width + 1;
    self.Width := self.Width - 1;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetFloat: double;
var
  s: string;
  d: double;
  e: integer;
begin
  Result := 0;
  case FEditType of
    etHex: if self.Text <> '' then Result := HexToInt(self.Text);
    etString:
      begin
        val(self.Text, d, e);
        Result := d;
      end;
    etNumeric, etFloat:
      if (Text <> '') and (Text <> ErrText) then
      begin
        s := self.Text;
        if (s = '-') then
          Result := 0
        else
          Result := EStrToFloat(s);
      end;  
    etMoney:
      if self.Text <> '' then
      begin
        s := StripThousandSep(self.Text);
        if (Pos(Decimalseparator, s) = Length(s)) then Delete(s, Pos(decimalseparator, s), 1);
        if (s = '') or (s = '-') then Result := 0 else
          Result := EStrToFloat(s);
      end;
  end;
end;

//------------------------------------------------------------------------------

function ValStr(s: string): Integer;
var
  err: Integer;
begin
  val(s, result, err);
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.GetInt: Integer;
begin
  Result := 0;
  case FEditType of
    etHex: if (self.Text <> '') then Result := HexToInt(self.Text);
    etNumeric, etFloat: Result := ValStr(self.Text);
    etMoney: Result := ValStr(StripThousandSep(self.Text));
  end;
end;

function TAdvCustomDropDown.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.UpdateLabel;
var
  tw: Integer;
  r: TRect;

begin
  FLabel.Transparent := FLabeltransparent;

  if not FParentFnt then
  begin
    FLabel.Font.Assign(FLabelFont);
  end
  else
    FLabel.Font.Assign(Font);

  tw := 0;
  if Assigned(FLabel.Parent) and FLabel.Parent.HandleAllocated  then
  begin
    r := Rect(0,0,1000,255);
    DrawText(FLabel.Canvas.Handle, PChar(FLabel.Caption), Length(FLabel.Caption), r, DT_HIDEPREFIX or DT_CALCRECT);
    tw := r.Right;
  end;

  case FLabelPosition of
    lpLeftTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left - tw - FLabelMargin;
      end;
    lpLeftCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.Top + ((self.Height - FLabel.Height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - self.Height) div 2);
        FLabel.Left := self.Left - tw - FLabelMargin;
      end;
    lpLeftBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left - tw - FLabelMargin;
      end;
    lpTopLeft:
      begin
        FLabel.Top := self.Top - FLabel.height - FLabelMargin;
        FLabel.Left := self.Left;
      end;
    lpTopRight:
      begin
        FLabel.Top := self.Top - FLabel.height - FLabelMargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpTopCenter:
      begin
        FLabel.Top := self.Top - FLabel.height - FLabelMargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.Width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.Width) div 2)
      end;
    lpBottomLeft:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        FLabel.left := self.left;
      end;
    lpBottomCenter:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        if self.Width - FLabel.Width > 0 then
          FLabeL.Left := self.Left + ((self.Width - FLabel.width) div 2)
        else
          FLabeL.Left := self.Left - ((FLabel.Width - self.width) div 2)
      end;
    lpBottomRight:
      begin
        FLabel.top := self.top + self.height + FLabelMargin;
        FLabel.Left := self.Left + self.Width - FLabel.Width;
      end;
    lpLeftTopLeft:
      begin
        FLabel.top := self.top;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftCenterLeft:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.top + ((self.height - FLabel.height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - self.Height) div 2);
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftBottomLeft:
      begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpRightTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRightCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.Top + ((self.Height - FLabel.Height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - self.Height) div 2);

        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRighBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
  end;

  FLabel.Visible := Visible;
end;

procedure TAdvCustomDropDown.UpdateLookup;
begin
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.EStrToFloat(s: string): extended;
begin
  if Pos(ThousandSeparator, s) > 0 then
    s := StripThousandSep(s);

  if (FPrecision > 0) and (Length(s) > FPrecision) then
    if s[Length(s) - FPrecision] = Thousandseparator then
      s[Length(s) - FPrecision] := Decimalseparator;
  try
    Result := StrToFloat(s);
  except
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.CharFromPos(pt: TPoint): Integer;
begin
  Result := Loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelparam(pt.x, pt.y)));
end;

//------------------------------------------------------------------------------

function TAdvCustomDropDown.PosFromChar(uChar: word): TPoint;
var
  pt: tpoint;
  l: Integer;
  DC: HDC;
  s: string;
  sz: TSize;
begin
  if uChar = 0 then
    Result := Point(0, 0);

  l := SendMessage(Handle, EM_POSFROMCHAR, uChar, 0);
  pt := Point(loword(l), hiword(l));

  Result := pt;

  if (pt.x < 0) or (pt.y < 0) or (pt.x >= 65535) or (pt.y >= 65535) then
  begin
    s := inherited Text;

    if Length(s) = 0 then
      Result := Point(0, 0)
    else
    begin
      dec(uChar);
      l := SendMessage(Handle, EM_POSFROMCHAR, uChar, 0);
      pt.x := loword(l);
      pt.y := hiword(l);

      Delete(s, 1, Length(s) - 1);
      DC := GetDC(Handle);
      GetTextExtentPoint32(DC, pchar(s + 'w'), 2, sz);
      pt.x := pt.x + sz.cx;
      GetTextExtentPoint32(DC, pchar(s), 2, sz);
      pt.x := pt.x - sz.cx;
      ReleaseDC(Handle, DC);
    end;

    Result := pt;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetAppearanceStyle(ItemAppearance: TItemAppearance;
  AStyle: TTMSStyle);
begin
  if not Assigned(ItemAppearance) then
    Exit;

  if AStyle <> tsCustom then
  begin
    ItemAppearance.BorderColorHot := clNone;
    ItemAppearance.BorderColorSelected := clNone;
    ItemAppearance.ColorSelectedText := clBlack;
    ItemAppearance.ColorHotText := clBlack;
  end;

  ItemAppearance.Font.Color := clBlack;
  case AStyle of
    tsOffice2003Blue:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;
        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;

      end;
    tsOffice2003Silver:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;
        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;

      end;
    tsOffice2003Olive:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;
        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;

      end;
    tsOffice2003Classic:
      begin
        ItemAppearance.ColorHot := $D2BDB6;
        ItemAppearance.ColorHotTo := $D2BDB6;
        ItemAppearance.ColorMirrorHot := clNone;
        ItemAppearance.ColorMirrorHotTo := clNone;
        ItemAppearance.BorderColor := $808080;

        ItemAppearance.ColorSelected := $B59285;
        ItemAppearance.ColorSelectedTo := $B59285;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColor := $808080;
        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;

      end;
    tsOffice2007Luna:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;

        ItemAppearance.BorderColorTop := $F4FEFF;
        ItemAppearance.BorderColorBottom := $CAF8FF;
        ItemAppearance.EdgeColor := $C3D9DF;

      end;
    tsOffice2007Obsidian:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;

        ItemAppearance.BorderColorTop := $F4FEFF;
        ItemAppearance.BorderColorBottom := $CAF8FF;
        ItemAppearance.EdgeColor := $C3D9DF;
      end;
    tsWindowsXP:
      begin
        ItemAppearance.ColorHot := $EFD3C6;
        ItemAppearance.ColorHotTo := $EFD3C6;
        ItemAppearance.ColorMirrorHot := clNone;
        ItemAppearance.ColorMirrorHotTo := clNone;
        ItemAppearance.BorderColor :=  clHighlight;
        ItemAppearance.EdgeColor := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.BorderColorTop := clNone;

        ItemAppearance.ColorSelected := clHighLight;
        ItemAppearance.ColorSelectedTo := clNone;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColor := clHighLight;
        ItemAppearance.ColorSelectedText := clHighLightText;
      end;
    tsWhidbey:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;

        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        ItemAppearance.ColorHot := $EBFDFF;
        ItemAppearance.ColorHotTo := $ACECFF;
        ItemAppearance.ColorMirrorHot := $59DAFF;
        ItemAppearance.ColorMirrorHotTo := $A4E9FF;
        ItemAppearance.BorderColor :=  $99CEDB;

        ItemAppearance.ColorSelected := $AAD9FF;
        ItemAppearance.ColorSelectedTo := $6EBBFF;
        ItemAppearance.ColorMirrorSelected := $42AEFE;
        ItemAppearance.ColorMirrorSelectedTo := $7AE1FE;
        ItemAppearance.BorderColor := $42AEFE;

        ItemAppearance.BorderColorTop := $F4FEFF;
        ItemAppearance.BorderColorBottom := $CAF8FF;
        ItemAppearance.EdgeColor := $C3D9DF;
      end;
      tsWindowsVista:
      begin
        ItemAppearance.ColorHot := $FFFDF9;
        ItemAppearance.ColorHotTo := $FFFAF0;
        ItemAppearance.ColorMirrorHot := clNone;
        ItemAppearance.ColorMirrorHotTo := clNone;
        ItemAppearance.BorderColor :=  $FCF2DA;

        ItemAppearance.ColorSelected := $FEF9F0;
        ItemAppearance.ColorSelectedTo := $FDF0D7;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColor := $FEDF9A;

        ItemAppearance.BorderColorTop := $FDFBF6;
        ItemAppearance.BorderColorBottom := $FDF5E7;
        ItemAppearance.EdgeColor := $FCEDCB;
      end;
      tsWindows7:
      begin
        ItemAppearance.ColorHot := $FDFBFA;
        ItemAppearance.ColorHotTo := $FDF3EB;
        ItemAppearance.ColorMirrorHot := clNone;
        ItemAppearance.ColorMirrorHotTo := clNone;
        ItemAppearance.BorderColor :=  $FBD6B8;

        ItemAppearance.ColorSelected := $FCEBDC;
        ItemAppearance.ColorSelectedTo := $FCDBC1;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColor := $CEA27D;

        ItemAppearance.BorderColorTop := $FDF4EB;
        ItemAppearance.BorderColorBottom := $FDEADB;
        ItemAppearance.EdgeColor := $E0C5AE;

      end;
      tsTerminal:
      begin
        ItemAppearance.ColorHot := clSilver;
        ItemAppearance.ColorHotTo := clSilver;
        ItemAppearance.ColorMirrorHot := clNone;
        ItemAppearance.ColorMirrorHotTo := clNone;
        ItemAppearance.BorderColor :=  clGray;

        ItemAppearance.ColorSelected := clHighLight;
        ItemAppearance.ColorSelectedTo := clNone;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColor := clGray;
        ItemAppearance.ColorSelectedText := clHighLightText;

        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;
      end;
     tsOffice2010Blue:
      begin
        ItemAppearance.ColorHot := $D9F9FD;
        ItemAppearance.ColorHotTo := $8AE3FD;
        ItemAppearance.ColorMirrorHot := $8AE3FD;
        ItemAppearance.ColorMirrorHotTo := $D9F9FD;
        ItemAppearance.BorderColor :=  $58CAF1;

        ItemAppearance.ColorSelected := $7BEEFF;
        ItemAppearance.ColorSelectedTo := $6CD0FF;
        ItemAppearance.ColorMirrorSelected := $6CD0FF;
        ItemAppearance.ColorMirrorSelectedTo := $7BEEFF;
        ItemAppearance.BorderColor := $308AC2;
        ItemAppearance.ColorSelectedText := clBlack;

        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;
      end;
     tsOffice2010Silver:
      begin
        ItemAppearance.ColorHot := $D9F9FD;
        ItemAppearance.ColorHotTo := $8AE3FD;
        ItemAppearance.ColorMirrorHot := $8AE3FD;
        ItemAppearance.ColorMirrorHotTo := $D9F9FD;
        ItemAppearance.BorderColor :=  $58CAF1;

        ItemAppearance.ColorSelected := $7BEEFF;
        ItemAppearance.ColorSelectedTo := $6CD0FF;
        ItemAppearance.ColorMirrorSelected := $6CD0FF;
        ItemAppearance.ColorMirrorSelectedTo := $7BEEFF;
        ItemAppearance.BorderColor := $308AC2;
        ItemAppearance.ColorSelectedText := clBlack;

        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;
      end;
     tsOffice2010Black:
      begin
        ItemAppearance.ColorHot := $D9F9FD;
        ItemAppearance.ColorHotTo := $8AE3FD;
        ItemAppearance.ColorMirrorHot := $8AE3FD;
        ItemAppearance.ColorMirrorHotTo := $D9F9FD;
        ItemAppearance.BorderColorHot := $58CAF1;

        ItemAppearance.ColorSelected := $7BEEFF;
        ItemAppearance.ColorSelectedTo := $6CD0FF;
        ItemAppearance.ColorMirrorSelected := $6CD0FF;
        ItemAppearance.ColorMirrorSelectedTo := $7BEEFF;
        ItemAppearance.BorderColorSelected := $308AC2;
        ItemAppearance.ColorSelectedText := clBlack;

        ItemAppearance.BorderColorTop := clNone;
        ItemAppearance.BorderColorBottom := clNone;
        ItemAppearance.EdgeColor := clNone;
      end;

  tsWindows8, tsWindows10:
    begin

      ItemAppearance.ColorSelected := $F7E0C9;
      ItemAppearance.ColorSelectedTo := clNone;
      ItemAppearance.ColorMirrorSelected := clNone;
      ItemAppearance.ColorMirrorSelectedTo := clNone;
      ItemAppearance.BorderColorSelected := $E4A262;

      ItemAppearance.ColorHot := $F7EFE8;
      ItemAppearance.ColorHotTo := $F7EFE8;
      ItemAppearance.ColorMirrorHot := $F7EFE8;
      ItemAppearance.ColorMirrorHotTo := $F7EFE8;
      ItemAppearance.BorderColorHot :=  $F9CEA4;
    end;
        tsOffice2013White:
    begin
      ItemAppearance.ColorSelected := $FCE2C8;
      ItemAppearance.ColorSelectedTo := clNone;
      ItemAppearance.ColorMirrorSelected := clNone;
      ItemAppearance.ColorMirrorSelectedTo := clNone;
      ItemAppearance.BorderColorSelected := $E59D56;

      ItemAppearance.ColorHot := $FCF0E4;
      ItemAppearance.ColorHotTo := $FCF0E4;
      ItemAppearance.ColorMirrorHot := clNone;
      ItemAppearance.ColorMirrorHotTo := clNone;
      ItemAppearance.BorderColorHot :=  $EAB47E;
    end;
        tsOffice2013LightGray:
    begin
      ItemAppearance.ColorSelected := $FCE2C8;
      ItemAppearance.ColorSelectedTo := clNone;
      ItemAppearance.ColorMirrorSelected := clNone;
      ItemAppearance.ColorMirrorSelectedTo := clNone;
      ItemAppearance.BorderColorSelected := $E59D56;

      ItemAppearance.ColorHot := $FCF0E4;
      ItemAppearance.ColorHotTo := $FCF0E4;
      ItemAppearance.ColorMirrorHot := clNone;
      ItemAppearance.ColorMirrorHotTo := clNone;
      ItemAppearance.BorderColorHot :=  $EAB47E;
    end;
        tsOffice2013Gray:
    begin

      ItemAppearance.ColorSelected := $FCE2C8;
      ItemAppearance.ColorSelectedTo := clNone;
      ItemAppearance.ColorMirrorSelected := clNone;
      ItemAppearance.ColorMirrorSelectedTo := clNone;
      ItemAppearance.BorderColorSelected := $E59D56;

      ItemAppearance.ColorHot := $FCF0E4;
      ItemAppearance.ColorHotTo := $FCF0E4;
      ItemAppearance.ColorMirrorHot := clNone;
      ItemAppearance.ColorMirrorHotTo := clNone;
      ItemAppearance.BorderColorHot :=  $EAB47E;
    end;
  tsOffice2016White:
    begin
        ItemAppearance.ColorSelected := $E3BDA3;
        ItemAppearance.ColorSelectedTo := clNone;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColorSelected := $E3BDA3;

       ItemAppearance.ColorHot := $F2E1D5;
       ItemAppearance.ColorHotTo := clNone;
       ItemAppearance.ColorMirrorHot := clNone;
       ItemAppearance.ColorMirrorHotTo := clNone;
       ItemAppearance.BorderColorHot :=  $F2E1D5;
    end;
        tsOffice2016Gray:
    begin

        ItemAppearance.ColorSelected := $E3BDA3;
        ItemAppearance.ColorSelectedTo := clNone;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColorSelected := $E3BDA3;

       ItemAppearance.ColorHot := $F2E1D5;
       ItemAppearance.ColorHotTo := $F2E1D5;
       ItemAppearance.ColorMirrorHot := clNone;
       ItemAppearance.ColorMirrorHotTo := clNone;
       ItemAppearance.BorderColorHot :=  $F2E1D5;
    end;
        tsOffice2016Black:
    begin
        ItemAppearance.ColorSelected := $444444;
        ItemAppearance.ColorSelectedTo := clNone;
        ItemAppearance.ColorMirrorSelected := clNone;
        ItemAppearance.ColorMirrorSelectedTo := clNone;
        ItemAppearance.BorderColorSelected := $444444;
       ItemAppearance.Font.Color := clWhite;

       ItemAppearance.ColorHot := $6A6A6A;
       ItemAppearance.ColorHotTo := $6A6A6A;
       ItemAppearance.ColorMirrorHot := clNone;
       ItemAppearance.ColorMirrorHotTo := clNone;
       ItemAppearance.BorderColorHot :=  $6A6A6A;

       ItemAppearance.Font.Color:= clWhite;
    end;



  end;
end;

procedure TAdvCustomDropDown.SetAutoThousandSeparator(
  const Value: Boolean);
begin
  FAutoThousandSeparator := Value;
  if FEditType in [etMoney, etFloat] then AutoSeparators;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormHide(Sender: TObject);
begin
  OnHideDropDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormDestroy(Sender: TObject);
begin
  OnDestroyDropDownForm;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormMouseWheelDown(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  OnDropDownFormMouseWheelDown(Shift, MousePos, Handled);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormMouseWheelUp(Sender: TObject;
  Shift: TShiftState; MousePos: TPoint; var Handled: Boolean);
begin
  OnDropDownFormMouseWheelUp(Shift, MousePos, Handled);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownFormMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  HandleMouseWheelDown;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownFormMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint; var Handled: Boolean);
begin
  HandleMouseWheelUp;
end;

procedure TAdvCustomDropDown.OnDropDownSizing;
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormKeyPress(Sender: TObject;
  var Key: Char);
begin
  OnDropDownFormKeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OnDropDownFormKeyUp(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OnDropDownFormKeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownFormKeyPress(var Key: Char);
begin
  //if (Integer(Key) in [VK_ESCAPE, VK_RETURN, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_Char, Integer(Key), 0);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownFormKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  //if (Key in [VK_ESCAPE, VK_RETURN, VK_F4, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_KEYUP, Key, 0);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownFormKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  //if (Key in [VK_ESCAPE, VK_RETURN, VK_F4, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_KEYDOWN, Key, 0);
  //SendMessage(Self.Handle, WM_KEYUP, Key, 0);
  {IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  case Key of
    VK_ESCAPE: DoHideDropDown(True);
    VK_RETURN: DoHideDropDown(False);
    VK_UP: if IsAlt then DoHideDropDown(True);
    VK_F4: DoHideDropDown(True);
  end;}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownControlKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  //if (Key in [VK_ESCAPE, VK_RETURN, VK_F4, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_KEYDOWN, Key, 0);
  //SendMessage(Self.Handle, WM_KEYUP, Key, 0);

  {IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  case Key of
    VK_ESCAPE: DoHideDropDown(True);
    VK_RETURN: DoHideDropDown(False);
    VK_UP: if IsAlt then DoHideDropDown(True);
    VK_F4: DoHideDropDown(True);
  end;}
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownControlKeyPress(var Key: Char);
begin
  //if (Integer(Key) in [VK_ESCAPE, VK_RETURN, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_CHAR, Integer(Key), 0);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDropDownControlKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  //if (Key in [VK_ESCAPE, VK_RETURN, VK_F4, VK_LEFT,VK_RIGHT,VK_UP,VK_DOWN,VK_PRIOR,VK_NEXT]) then
    SendMessage(Self.Handle, WM_KEYUP, Key, 0);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnControlKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OnDropDownControlKeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnControlKeyPress(Sender: TObject; var Key: Char);
begin
  OnDropDownControlKeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnControlKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  OnDropDownControlKeyUp(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.WMPaint(var Message: TWMPaint);
begin
  inherited;
  DrawBackGround;

  if {Border3D or} (FFocusBorderColor <> clNone) or (FBorderColor <> clNone) or (not Enabled and DisabledBorder) then
    DrawBorders;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.DrawBackGround;
var
  DC: HDC;
  Canvas: TCanvas;
  R: TRect;
begin
  if not EditorEnabled and Focused and not (csDesigning in ComponentState) and FocusDraw then
  begin
    DC := GetWindowDC(Handle);
    try
      Canvas := TCanvas.Create;
      Canvas.Handle := DC;

      R := Rect(2,2, Width - DropDownButtonWidth - 1, Height - 2);

      Canvas.Pen.Color := clBlack;
      Canvas.Brush.Color := clHighlight;
      Canvas.Rectangle(R);

      R.Left := R.Left + 2;
      R.Right := R.Right - 3;
      Canvas.Font.Assign(Font);
      Canvas.Font.Color := clHighlightText;
      Canvas.Brush.Style := bsClear;
      DrawText(Canvas.Handle, PChar(Text), -1, R, DT_SINGLELINE or DT_VCENTER or DT_LEFT);
      Canvas.Free;
    finally
      ReleaseDC(Handle,DC);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Invalidate;
    Button.Repaint;
  end;
end;

procedure TAdvCustomDropDown.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

  if (FLabel <> nil) then
  begin
    if FLabel.Parent <> nil then
      UpdateLabel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDropDownEnabled(const Value: Boolean);
begin
  FDropDownEnabled := Value;
  if Assigned(FButton) then
    FButton.Enabled := FDropDownEnabled;
end;

//------------------------------------------------------------------------------
(*
procedure TAdvCustomDropDown.OnButtonGlyphChanged(Sender: TObject);
begin
  if Assigned(FButton) then
  begin
    if FButtonGlyph.Empty then
      UpdateDropDownButton
    else
      FButton.Glyph.Assign(FButtonGlyph);
  end;
end;
*)
//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnDestroyDropDownForm;
begin

end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.SetDisabledBorder(const Value: boolean);
begin
  if (FDisabledBorder <> Value) then
  begin
    FDisabledBorder := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomDropDown.SetDropDownColor(const Value: TColor);
begin
  FDropDownColor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnFooterAppearanceChanged(Sender: TObject);
begin
  if Assigned(FDropDownForm) and FDroppedDown then
  begin
    FDropDownForm.InitializeAndUpdate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomDropDown.OnHeaderAppearanceChanged(Sender: TObject);
begin
  if Assigned(FDropDownForm) and FDroppedDown then
  begin
    FDropDownForm.InitializeAndUpdate;
  end;
end;

//------------------------------------------------------------------------------

{ TDropDownForm }

constructor TDropDownForm.Create(AOwner: TComponent);
begin
  CreateNew(AOwner);
end;

//------------------------------------------------------------------------------

constructor TDropDownForm.CreateNew(AOwner: TComponent; Dummy: Integer);
begin
  inherited;
  FShadow := True;
  FIsDeactivating := false;
  FCancelOnDeActivate := True;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.CreateParams(var Params: TCreateParams);
{$IFNDEF DELPHI2006_LVL}
const
  CS_DROPSHADOW = $00020000;
{$ENDIF}
var
  f: TCustomForm;
begin
  inherited CreateParams(Params);

  //Params.Style := Params.Style + WS_BORDER;

  (*
  if Shadow and (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW
  else
    Params.WindowClass.Style := Params.WindowClass.Style - CS_DROPSHADOW;
  *)

  if (Win32Platform = VER_PLATFORM_WIN32_NT) then // not for Win9x
    Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;

   //Params.Style := WS_SIZEBOX or WS_SYSMENU;
   //Params.ExStyle := WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE; Params.ExStyle: = WS_EX_DLGMODALFRAME or WS_EX_WINDOWEDGE;


  if Assigned(AdvDropDown) then
    f := GetParentForm(AdvDropDown)
  else
    f := nil;

  if Assigned(f) then
    Params.WndParent := f.Handle;

end;

//------------------------------------------------------------------------------

function TDropDownForm.GetClientRect: TRect;
begin
  Result := inherited GetClientRect;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.AdjustClientRect(var Rect: TRect);
begin
  inherited AdjustClientRect(Rect);
  if Assigned(FAdvDropDown) then
    InflateRect(Rect, -FAdvDropDown.FDropDownBorderWidth, -FAdvDropDown.FDropDownBorderWidth);
end;

//------------------------------------------------------------------------------

function TDropDownForm.GetParentWnd: HWnd;
var
  Last, P: HWnd;
begin
  Result := 0;
  if (Owner <> nil) then
  begin
    P := GetParent((Owner as TWinControl).Handle);
    Last := P;
    while P <> 0 do
    begin
      Last := P;
      P := GetParent(P);
    end;
    Result := Last;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.Paint;
begin
  inherited;
  DrawBackGround(Canvas);
end;

//------------------------------------------------------------------------------

function TDropDownForm.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

//------------------------------------------------------------------------------

function TDropDownForm.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.DrawBackGround(ACanvas: TCanvas);
var
  R: TRect;
begin
  if not Assigned(AdvDropDown) then
    Exit;

  R := ClientRect;
  InflateRect(R, -FAdvDropDown.FDropDownBorderWidth, -FAdvDropDown.FDropDownBorderWidth);
  if (AdvDropDown.DropDownColor <> clNone) and (AdvDropDown.DropDownColorTo <> clNone) then
    DrawGradient(aCanvas, AdvDropDown.DropDownColor, AdvDropDown.DropDownColorTo, 80, R, AdvDropDown.DropDownGradient = gdHorizontal)
  else if (AdvDropDown.DropDownColor <> clNone) then
  begin
    ACanvas.Brush.Color := AdvDropDown.DropDownColor;
    ACanvas.FillRect(R);
  end;

  if (AdvDropDown.DropDownBorderWidth > 0) then
  begin
    R := ClientRect;
    R.Top := R.Top + AdvDropDown.DropDownBorderWidth div 2;
    R.Left := R.Left + AdvDropDown.DropDownBorderWidth div 2;
    ACanvas.Pen.Width := AdvDropDown.DropDownBorderWidth;
    ACanvas.Pen.Color := AdvDropDown.DropDownBorderColor;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Rectangle(R);
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMSize(var Message: TWMSize);
begin
  inherited;
  DrawBackGround(Canvas);
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMSizing(var Message: TWMSize);
begin
  inherited;
  if Assigned(FOnSizing) then
    FOnSizing(Self);
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.SetAdvDropDown(const Value: TAdvCustomDropDown);
begin
  FAdvDropDown := Value;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.UpdateSize;
begin

end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMActivate(var Message: TWMActivate);
var
  ph: THandle;
  RealDeActivate: Boolean;

begin
  inherited;

  if FBlockActivate then
    Exit;

  if Message.Active = integer(False) then
  begin
    if Visible then
    begin
      FIsDeactivating := true;

      FDeActivate := GetTickCount;
      RealDeActivate := True;

      ph := Message.ActiveWindow;
      repeat
        ph := GetParent(ph);
        if ph = self.Handle then
          RealDeactivate := false;
      until (ph = 0) or (RealDeactivate = false);

      if RealDeActivate then
      begin
        if Assigned(AdvDropDown) then
          AdvDropDown.DoHideDropDown(CancelOnDeActivate)
        else
          Hide;

        FIsDeactivating := false;
      end;
    end;
  end
  else
  begin
    SendMessage(GetParentWnd, WM_NCACTIVATE, 1, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMClose(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
  //inherited;
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));

  if Sizeable and (pt.X > Width - 10) and (pt.Y > Height - 10) then
    message.Result := HTBOTTOMRIGHT;

  {if Sizeable and Visible and ((Message.XPos < Left + 5) or (Message.YPos < Top + 5)) then
  begin
    Message.Result := 0;
    Exit;
  end;}

  //inherited;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.InitializeAndUpdate;
begin
  if not Assigned(FAdvDropDown) then
    Exit;
    
  if not Assigned(FHeader) then
  begin
    if FAdvDropDown.DropDownHeader.Visible then
    begin
      FHeader := TAdvHeader.Create(Self);
      FHeader.Parent := Self;
      FHeader.Height := AdvDropDown.DropDownHeader.Height;
      FHeader.AdvDropDown := AdvDropDown;
      FHeader.Initialize;
      FHeader.Align := alTop;
    end;
  end
  else
  begin
    Fheader.AdvDropDown := AdvDropDown;
    FHeader.Initialize;
    FHeader.Visible := FAdvDropDown.DropDownHeader.Visible;
  end;

  if Assigned(FHeader) then
  begin
    FHeader.OnDrawBackGround := FAdvDropDown.OnDrawHeader;
    FHeader.OnGetText := FAdvDropDown.OnGetHeaderText;
  end;

  if not Assigned(FFooter) then
  begin
    if AdvDropDown.DropDownFooter.Visible then
    begin
      FFooter := TAdvFooter.Create(Self);
      FFooter.Parent := Self;
      FFooter.Height := AdvDropDown.DropDownFooter.Height;
      FFooter.AdvDropDown := AdvDropDown;
      FFooter.Initialize;
      FFooter.Align := alBottom;
    end;
  end
  else
  begin
    FFooter.AdvDropDown := AdvDropDown;
    FFooter.Initialize;
    FFooter.Visible := AdvDropDown.DropDownFooter.Visible;
  end;

  if Assigned(FFooter) then
  begin
    FFooter.OnDrawBackGround := FAdvDropDown.OnDrawFooter;
    FFooter.OnGetText := FAdvDropDown.OnGetFooterText;
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownForm.WMGetDlgCode(var Message: TMessage);
begin
  if TabStop or true then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

//------------------------------------------------------------------------------

{ TAdvDropDownSpeedButton }

procedure TAdvDropDownSpeedButton.SetButtonStyle(const Value: TButtonStyle);
begin
  FButtonStyle := Value;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.SetEtched(const Value: Boolean);
begin
  if Value <> FEtched then
  begin
    FEtched := value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.SetFocused(const Value: Boolean);
begin
  if Value <> FFocused then
  begin
    FFocused := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  FHot := True;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  FHot := False;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.Paint;
begin
  case ButtonStyle of
  bsButton: PaintButton;
  bsDropDown: PaintDropDown;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.PaintDropDown;
var
  htheme: THandle;
  ARect: TRect;
  pt: array of TPoint;
  FEnabled: boolean;
  t,l: integer;
begin
  {if (csDesigning in ComponentState) then
  begin
    inherited Paint;
    Exit;
  end;
  }

  FEnabled := TDropDownEditButton(Owner).Enabled;

  if (TDropDownEditButton(Owner).ButtonColor <> clNone) then
  begin
    Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonColor;

    if FHot then
      Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonColorHot;

    if (FState in [bsDown, bsExclusive]) and not FUp then
      Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonColorDown;

    Canvas.Pen.Color := Canvas.Brush.Color;
    ARect := ClientRect;
    ARect.Left := ARect.Left + 2;
    Canvas.FillRect(ClientRect);
    ARect.Left := ARect.Left - 2;

    Canvas.Pen.Color := TAdvCustomDropDown(Owner.Owner).BorderColor;
    Canvas.MoveTo(ARect.Left, ARect.Top);
    Canvas.LineTo(ARect.Left, ARect.Bottom);

    Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonTextColor;

    if FHot then
    begin
      Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonTextColorHot;
    end;

    if ((FState in [bsDown, bsExclusive]) and not FUp) then
    begin
      Canvas.Brush.Color := TDropDownEditButton(Owner).ButtonTextColorDown;
    end;

    Canvas.Pen.Color := Canvas.Brush.Color;


    SetLength(pt, 3);

    pt[0].X := 5;
    pt[0].y := 7;

    pt[1].X := 11;
    pt[1].y := 7;

    pt[2].X := 8;
    pt[2].y := 10;

    Canvas.Polygon(pt);
    Exit;
  end;


  if Assigned(Glyph) and not Glyph.Empty then
  begin
    Canvas.Brush.Color := clBtnFace;
    Canvas.Pen.Color := clgray;
    Canvas.Rectangle(ClientRect);
    Glyph.TransparentMode := tmAuto;
    Glyph.Transparent := true;
    // do center drawing
    t := 0;
    l := 0;
    if Glyph.Width < Width then
      l := (Width - Glyph.Width) div 2;
    if Glyph.Height < Height then
      t := (Height - Glyph.Height) div 2;
    Canvas.Draw(l,t,Glyph);
    Exit;
  end;

  FEnabled := FEnabled and TCustomEdit(Owner.Owner).Enabled and not TAdvCustomDropDown(Owner.Owner).ReadOnly; //and TAdvCustomDropDown(Owner.Owner).DropDownEnabled;

  if not (DoVisualStyles and IsThemeActive) {or (csDesigning in ComponentState)} then
  begin
    inherited Paint;

    Canvas.Pen.Color := clBtnFace;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Width-2,0);
    Canvas.LineTo(0,0);
    Canvas.LineTo(0,Height - 1);

    Canvas.Pen.Color := clWhite;
    Canvas.Pen.Width := 1;
    Canvas.MoveTo(Width-3,1);
    Canvas.LineTo(1,1);
    Canvas.LineTo(1,Height - 2);

    Canvas.Brush.Color := clBlack;
    Canvas.Pen.Color := Canvas.Brush.Color;
    SetLength(pt, 3);

    pt[0].X := 5;
    pt[0].y := 7;

    pt[1].X := 11;
    pt[1].y := 7;

    pt[2].X := 8;
    pt[2].y := 10;

    Canvas.Polygon(pt);
  end
  else
  begin
    htheme := OpenThemeData(Parent.Handle,'combobox');
    ARect := ClientRect;
    if not IsVista then
    begin
      InflateRect(ARect,1,1);
      ARect.Left := ARect.Left + 2;
    end
    else
      Arect.Left := Arect.Left + 1;

    if not FEnabled then
    begin
      DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_DISABLED,@ARect,nil)
    end
    else
    begin
      if (FState in [bsDown, bsExclusive]) and not FUp then
        DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_PRESSED,@ARect,nil)
      else
      begin
        if FHot then
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_HOT,@ARect,nil)
        else
          DrawThemeBackground(htheme,Canvas.Handle,CP_DROPDOWNBUTTON,CBXS_NORMAL,@ARect,nil);
      end;
    end;
    CloseThemeData(htheme);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.PaintButton;
const
  Flags: array[Boolean] of Integer = (0, BF_FLAT);
  Edge: array[Boolean] of Integer = (EDGE_RAISED,EDGE_ETCHED);

var
  r: TRect;
  BtnFaceBrush: HBRUSH;
  HTheme: THandle;
begin
  Canvas.Font.Assign(TAdvCustomDropDown(Owner.Owner).Font);

  if DoVisualStyles then
  begin
    r := BoundsRect;
    FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

    r := Rect(0, 0, Width + 1, Height + 1);

    HTheme := OpenThemeData(Parent.Handle,'button');

    if not Enabled then
      DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_DISABLED,@r,nil)
    else
    begin
      if (FState in [bsDown, bsExclusive]) and not FUp then
        DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_PRESSED,@r,nil)
      else
        if FHot then
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_HOT,@r,nil)
        else
          DrawThemeBackground(HTheme,Canvas.Handle, BP_PUSHBUTTON,PBS_NORMAL,@r,nil);

    end;

    CloseThemeData(HTheme);

    r := ClientRect;

    if Assigned(Glyph) then
    begin
      if not Glyph.Empty then
      begin
        InflateRect(r,-2,-2);

        if (Caption = '') then
        begin
          if Glyph.Width < r.Right - r.Left then
            r.Left := r.Left + (r.Right - r.Left - Glyph.Width) shr 1;
        end
        else
          r.Left := r.Left + 2;

        if Glyph.Height < r.Bottom - r.Top then
          r.Top := r.Top + (r.Bottom - r.Top - Glyph.Height) shr 1;

        if FState = bsdown then OffsetRect(r,1,1);

        Glyph.TransparentMode := tmAuto;
        Glyph.Transparent := true;
        Canvas.Draw(r.Left,r.Top, Glyph);
      end;
    end;


    if (Caption <> '') then
    begin
      Windows.setbkmode(canvas.handle,windows.TRANSPARENT);
      if not Glyph.Empty then
      begin
        r.Left := r.Left + Glyph.Width + 2;
        r.Top := r.Top -1;
        DrawText(canvas.handle,pchar(Caption),length(Caption),r,DT_LEFT);
      end
      else
      begin
        Inflaterect(r,-3,-1);
        if FState = bsdown then Offsetrect(r,1,1);
        DrawText(canvas.handle,pchar(Caption),length(Caption),r,DT_CENTER);
      end;
    end;

  end
  else
  begin
    if not Flat then
      inherited Paint else
    begin

      r := BoundsRect;
      FillRect(Canvas.Handle,r,Canvas.Brush.Handle);

      BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE));

      FillRect(Canvas.Handle, r, BtnFaceBrush);

      DeleteObject(BtnFaceBrush);

      r.Bottom := r.Bottom + 1;
      r.Right := r.Right + 1;
      DrawEdge(Canvas.Handle, r, Edge[fEtched], BF_RECT or flags[fState=bsDown]);

      r := ClientRect;

      if Assigned(Glyph) then
      begin
        if not Glyph.Empty then
        begin
          InflateRect(r,-3,-3);
          if fstate = bsdown then offsetrect(r,1,1);
          DrawBitmapTransp(canvas,glyph,ColorToRGB(clBtnFace),r);
        end;
      end;

      if (Caption <> '') then
      begin
        Inflaterect(r,-3,-1);
        if FState = bsdown then Offsetrect(r,1,1);
        Windows.SetBKMode(canvas.handle,windows.TRANSPARENT);
        DrawText(Canvas.handle,pchar(Caption),length(Caption),r,DT_CENTER);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvDropDownSpeedButton.DoVisualStyles: Boolean;
begin
  if FIsWinXP then
    Result := IsThemeActive
  else
    Result := False;
end;

//------------------------------------------------------------------------------

constructor TAdvDropDownSpeedButton.Create(AOwner: TComponent);
var
  i: Integer;
begin
  inherited;

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;

  FIsWinXP := (i > 5);

  FUp := False;
  //ButtonStyle := bsDropDown;
  {if not (DoVisualStyles and IsThemeActive) and (csDesigning in ComponentState) then
  begin
    Transparent := False;
    inherited Flat := True;
  end;}
end;

//------------------------------------------------------------------------------

procedure TAdvDropDownSpeedButton.SetUp;
begin
  FUp := true;
end;

//------------------------------------------------------------------------------

{ TDropDownEditButton }
constructor TDropDownEditButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
//  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csFramed, csOpaque];
  ControlStyle := ControlStyle - [csAcceptsControls, csSetCaption] + [csOpaque];
  FButton := CreateButton;
  FButton.ButtonStyle := bsDropDown;

  Glyph := nil;
  Width := 16;
  Height := 25;
  FBWidth := 16;
  FButtonColor := clNone;
  FButtonColorHot := clNone;
  FButtonColorDown := clNone;
  FButtonTextColor := clNone;
  FButtonTextColorHot := clNone;
  FButtonTextColorDown := clNone;
  FButtonBorderColor := clNone;
end;

//------------------------------------------------------------------------------

function TDropDownEditButton.CreateButton: TAdvDropDownSpeedButton;
begin
  Result := TAdvDropDownSpeedButton.Create(Self);
  Result.Parent := Self;
  Result.OnClick := BtnClick;
  Result.OnMouseUp := BtnMouseDown;
  Result.Visible := True;
  Result.Enabled := Enabled;
  Result.Caption := '';
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.CMEnabledChanged(var Msg: TMessage);
begin
  inherited;
  if not (csDesigning in ComponentState) and Assigned(FButton) then
    FButton.Enabled := Enabled;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FFocusControl) then
    FFocusControl := nil;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.AdjustWinSize (var W: Integer; var H: Integer);
begin
  if (FButton = nil) or ((csLoading in ComponentState) and not (csDesigning in ComponentState)) then
    Exit;
  W := FBWidth;
  FButton.SetBounds (0, 0, W, H);
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
var
  W, H: Integer;
begin
  W := AWidth;
  H := AHeight;
  AdjustWinSize (W, H);
  inherited SetBounds (ALeft, ATop, W, H);
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.WMSize(var Message: TWMSize);
var
  W, H: Integer;
begin
  inherited;
  { check for minimum size }
  W := Width;
  H := Height;
  AdjustWinSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds(Left, Top, W, H);
  Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.BtnMouseDown (Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  if Button = mbLeft then
  begin
    if Assigned(FOnClick) and (Sender = FButton) then
      FOnClick(Self);
    {
    if (FFocusControl <> nil) and FFocusControl.TabStop and
        FFocusControl.CanFocus and (GetFocus <> FFocusControl.Handle) then
      FFocusControl.SetFocus;
    else if TabStop and (GetFocus <> Handle) and CanFocus then
      SetFocus;
    }
  end;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.BtnClick(Sender: TObject);
begin
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.Loaded;
var
  W, H: Integer;
begin
  inherited Loaded;
  W := Width;
  H := Height;
  AdjustWinSize (W, H);
  if (W <> Width) or (H <> Height) then
    inherited SetBounds (Left, Top, W, H);
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.Setup;
begin
  FButton.Setup;
end;

//------------------------------------------------------------------------------

function TDropDownEditButton.GetGlyph: TBitmap;
begin
  Result := FButton.Glyph;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.SetGlyph(Value: TBitmap);
begin
  FButton.Glyph := Value;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.SetCaption(value:string);
begin
  FButton.Caption := Value;
end;

//------------------------------------------------------------------------------

function TDropDownEditButton.GetCaption:string;
begin
  Result := FButton.Caption;
end;

//------------------------------------------------------------------------------

function TDropDownEditButton.GetNumGlyphs: TNumGlyphs;
begin
  Result := FButton.NumGlyphs;
end;

//------------------------------------------------------------------------------

procedure TDropDownEditButton.SetNumGlyphs(Value: TNumGlyphs);
begin
  FButton.NumGlyphs := Value;
end;

//------------------------------------------------------------------------------

{ THeaderAppearance }

procedure THeaderAppearance.Assign(Source: TPersistent);
begin
  if (Source is THeaderAppearance) then
  begin
    FColorTo := THeaderAppearance(Source).FColorTo;
    FColor := THeaderAppearance(Source).FColor;
    FBorderColor := THeaderAppearance(Source).FBorderColor;
    FCaption := THeaderAppearance(Source).FCaption;
    FFont.Assign(THeaderAppearance(Source).FFont);
    FImageIndex := THeaderAppearance(Source).FImageIndex;
    FBorderWidth := THeaderAppearance(Source).FBorderWidth;
    FGradientDirection := THeaderAppearance(Source).FGradientDirection;
    Height := THeaderAppearance(Source).FHeight;
    Visible := THeaderAppearance(Source).FVisible;
    FButtonAlignment := THeaderAppearance(Source).FButtonAlignment;
    FButtons.Assign(THeaderAppearance(Source).FButtons);
  end;
end;

//------------------------------------------------------------------------------

constructor THeaderAppearance.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  FVisible := True;
  FHeight := DD_HEADERHEIGHT;
  FBorderWidth := 1;
  FImageIndex := -1;
  FCaption := '';
  FColorTo := clWhite;
  FColor := clSilver;
  FBorderColor := clBlack;
  FGradientDirection := gdVertical;
  FButtons := THeaderButtons.Create(FOwner);
  FButtons.OnChange := OnButtonChanged;
  FButtonAlignment := baRight;
end;

//------------------------------------------------------------------------------

destructor THeaderAppearance.Destroy;
begin
  FFont.Free;
  FButtons.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure THeaderAppearance.AssignColors(Source: TPersistent);
begin
  if (Source is THeaderAppearance) then
  begin
    FColorTo := THeaderAppearance(Source).FColorTo;
    FColor := THeaderAppearance(Source).FColor;
    FBorderColor := THeaderAppearance(Source).FBorderColor;
    FFont.Assign(THeaderAppearance(Source).FFont);
    FGradientDirection := THeaderAppearance(Source).FGradientDirection;
  end;
end;

procedure THeaderAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure THeaderAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure THeaderAppearance.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure THeaderAppearance.SetButtons(const Value: THeaderButtons);
begin
  FButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

function THeaderAppearance.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure THeaderAppearance.OnButtonChanged(Sender: TObject);
begin
  if Assigned(FOnButtonsChanged) then
    FOnButtonsChanged(Self);
end;

//------------------------------------------------------------------------------

{ TFooterAppearance }

procedure TFooterAppearance.Assign(Source: TPersistent);
begin
  if (Source is TFooterAppearance) then
  begin
    FBorderColor := TFooterAppearance(Source).FBorderColor;
    FColor := TFooterAppearance(Source).FColor;
    FColorTo := TFooterAppearance(Source).FColorTo;
    FGradientDirection := TFooterAppearance(Source).FGradientDirection;
    FFont.Assign(TFooterAppearance(Source).FFont);
    FCaption := TFooterAppearance(Source).FCaption;
    SizeGrip := TFooterAppearance(Source).FSizeGrip;
    FBorderWidth := TFooterAppearance(Source).FBorderWidth;
    Height := TFooterAppearance(Source).FHeight;
    ImageIndex := TFooterAppearance(Source).ImageIndex;
    Visible := TFooterAppearance(Source).FVisible;
    FButtonAlignment := TFooterAppearance(Source).FButtonAlignment;
    FButtons.Assign(TFooterAppearance(Source).FButtons);
  end;
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.AssignColors(Source: TPersistent);
begin
  if (Source is TFooterAppearance) then
  begin
    FBorderColor := TFooterAppearance(Source).FBorderColor;
    FColor := TFooterAppearance(Source).FColor;
    FColorTo := TFooterAppearance(Source).FColorTo;
    FGradientDirection := TFooterAppearance(Source).FGradientDirection;
    FFont.Assign(TFooterAppearance(Source).FFont);
  end;
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TFooterAppearance.Create(AOwner: TPersistent);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FVisible := True;
  FHeight := DD_FOOTERHEIGHT;
  FBorderWidth := 1;
  FSizeGrip := True;
  FCaption := '';
  FColorTo := clWhite;
  FColor := clSilver;
  FBorderColor := clBlack;
  FGradientDirection := gdVertical;
  FButtons := TFooterButtons.Create(FOwner);
  FButtons.OnChange := OnButtonsChange;
  FImageIndex := -1;
  FButtonAlignment := baRight;
end;

//------------------------------------------------------------------------------

destructor TFooterAppearance.Destroy;
begin
  FFont.Free;
  FButtons.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TFooterAppearance.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.OnButtonsChange(Sender: TObject);
begin
  if Assigned(FOnButtonsChanged) then
    FOnButtonsChanged(Self);
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.SetButtons(const Value: TFooterButtons);
begin
  FButtons.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TFooterAppearance.SetVisible(const Value: Boolean);
begin
  if (FVisible <> Value) then
  begin
    FVisible := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ THeaderButton }

procedure THeaderButton.Assign(Source: TPersistent);
begin
  if (Source is THeaderButton) then
  begin
    Caption := (Source as THeaderButton).Caption;
    ImageIndex := (Source as THeaderButton).ImageIndex;
    Enabled := (Source as THeaderButton).Enabled;
    Width := (Source as THeaderButton).Width;
    Height := (Source as THeaderButton).Height;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor THeaderButton.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FEnabled := True;
  FWidth := 0;
  FHeight := 0;
end;

//------------------------------------------------------------------------------

destructor THeaderButton.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure THeaderButton.PropChanged;
begin
  THeaderButtons(Collection).Change;
end;

//------------------------------------------------------------------------------

procedure THeaderButton.SerHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
    FHeight := Value;
end;

//------------------------------------------------------------------------------

procedure THeaderButton.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure THeaderButton.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure THeaderButton.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

{ THeaderButtons }

function THeaderButtons.Add: THeaderButton;
begin
  Result := THeaderButton(inherited Add);
end;

//------------------------------------------------------------------------------

procedure THeaderButtons.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(self);
end;

//------------------------------------------------------------------------------

constructor THeaderButtons.Create(AOwner: TPersistent);
begin
  //inherited Create(THeaderButton);
  inherited Create(AOwner, THeaderButton);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function THeaderButtons.GetItem(Index: Integer): THeaderButton;
begin
  Result := THeaderButton(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

//function THeaderButtons.GetOwner: TPersistent;
//begin
//  Result := FMyOwner;
//end;

//------------------------------------------------------------------------------

function THeaderButtons.Insert(Index: Integer): THeaderButton;
begin
  Result := THeaderButton(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure THeaderButtons.SetItem(Index: Integer; const Value: THeaderButton);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TFooterButton }

procedure TFooterButton.Assign(Source: TPersistent);
begin
  if (Source is TFooterButton) then
  begin
    Caption := (Source as TFooterButton).Caption;
    ImageIndex := (Source as TFooterButton).ImageIndex;
    ModalResult := (Source as TFooterButton).ModalResult;
    Enabled := (Source as TFooterButton).Enabled;
    Width := (Source as TFooterButton).Width;
    Height := (Source as TFooterButton).Height;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

constructor TFooterButton.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FModalResult := mrNone;
  FEnabled := True;
  FWidth := 0;
  FHeight := 0;
end;

//------------------------------------------------------------------------------

destructor TFooterButton.Destroy;
begin

  inherited;
end;

//------------------------------------------------------------------------------

procedure TFooterButton.PropChanged;
begin
  TFooterButtons(Collection).Change;
end;

//------------------------------------------------------------------------------

procedure TFooterButton.SerHeight(const Value: Integer);
begin
  if (FHeight <> Value) then
    FHeight := Value;
end;

//------------------------------------------------------------------------------

procedure TFooterButton.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TFooterButton.SetEnabled(const Value: Boolean);
begin
  if (FEnabled <> Value) then
  begin
    FEnabled := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

procedure TFooterButton.SetWidth(const Value: Integer);
begin
  if (FWidth <> Value) then
  begin
    FWidth := Value;
    PropChanged;
  end;
end;

//------------------------------------------------------------------------------

{ TFooterButtons }

function TFooterButtons.Add: TFooterButton;
begin
  Result := TFooterButton(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TFooterButtons.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TFooterButtons.Create(AOwner: TPersistent);
begin
  inherited Create(TFooterButton);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TFooterButtons.GetItem(Index: Integer): TFooterButton;
begin
  Result := TFooterButton(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TFooterButtons.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TFooterButtons.Insert(Index: Integer): TFooterButton;
begin
  Result := TFooterButton(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TFooterButtons.SetItem(Index: Integer; const Value: TFooterButton);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TAdvButtonAppearance }

constructor TAdvButtonAppearance.Create;
begin
  inherited;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  
  // Set normal transparent
  FBorderColor := clNone;
  FColor := clNone;
  FColorTo := clNone;

  FColorHot := $F5F0E1;
  FColorHotTo := $F9D2B2;

  FColorDown := $00F5D8CA;
  FColorDownTo := $00F9BDA0;

  FColorChecked := BrightnessColor($F5F0E1,-10,-10,0);
  FColorCheckedTo := BrightnessColor($F9D2B2, -10,-10,0);

  FColorDisabled := $00F2F2F2;
  FColorDisabledTo := clNone; //$00F2F2F2;

  FBorderColorHot := clGray;
  FBorderColorDown := clNavy;
  FBorderColorChecked := clBlue;
  FBorderColorDisabled := clGray;

  FGradient := gdVertical;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvButtonAppearance) then
  begin
    Color := (Source as TAdvButtonAppearance).Color;
    ColorTo := (Source as TAdvButtonAppearance).ColorTo;

    ColorHot := (Source as TAdvButtonAppearance).ColorHot;
    ColorHotTo := (Source as TAdvButtonAppearance).ColorHotTo;

    ColorDown := (Source as TAdvButtonAppearance).ColorDown;
    ColorDownTo := (Source as TAdvButtonAppearance).ColorDownTo;

    ColorChecked := (Source as TAdvButtonAppearance).ColorChecked;
    ColorCheckedTo := (Source as TAdvButtonAppearance).ColorCheckedTo;

    ColorDisabled := (Source as TAdvButtonAppearance).ColorDisabled;
    ColorDisabledTo := (Source as TAdvButtonAppearance).ColorDisabledTo;

    BorderColor := (Source as TAdvButtonAppearance).BorderColor;
    BorderColorHot := (Source as TAdvButtonAppearance).BorderColorHot;
    BorderColorDown := (Source as TAdvButtonAppearance).BorderColorDown;
    BorderColorChecked := (Source as TAdvButtonAppearance).BorderColorChecked;
    BorderColorDisabled := (Source as TAdvButtonAppearance).BorderColorDisabled;

    Gradient := (Source as TAdvButtonAppearance).Gradient;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetBorderColorChecked(const Value: TColor);
begin
  if (FBorderColorChecked <> Value) then
  begin
    FBorderColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetBorderColorDisabled(
  const Value: TColor);
begin
  if (FBorderColorDisabled <> Value) then
  begin
    FBorderColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetBorderColorDown(const Value: TColor);
begin
  if (FBorderColorDown <> Value) then
  begin
    FBorderColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetBorderColorHot(const Value: TColor);
begin
  if (FBorderColorHot <> Value) then
  begin
    FBorderColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorChecked(const Value: TColor);
begin
  if (FColorChecked <> Value) then
  begin
    FColorChecked := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorCheckedTo(const Value: TColor);
begin
  if (FColorCheckedTo <> Value) then
  begin
    FColorCheckedTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorDisabled(const Value: TColor);
begin
  if (FColorDisabled <> Value) then
  begin
    FColorDisabled := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorDisabledTo(const Value: TColor);
begin
  if (FColorDisabledTo <> Value) then
  begin
    FColorDisabledTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorDown(const Value: TColor);
begin
  if (FColorDown <> Value) then
  begin
    FColorDown := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorDownTo(const Value: TColor);
begin
  if (FColorDownTo <> Value) then
  begin
    FColorDownTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorHot(const Value: TColor);
begin
  if (FColorHot <> Value) then
  begin
    FColorHot := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorHotTo(const Value: TColor);
begin
  if (FColorHotTo <> Value) then
  begin
    FColorHotTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetColorTo(const Value: TColor);
begin
  if (FColorTo <> Value) then
  begin
    FColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetGradient(const Value: TGradientDirection);
begin
  if (FGradient <> Value) then
  begin
    FGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButtonAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------

destructor TAdvButtonAppearance.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

{ TAdvButton }

constructor TAdvButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FAppearance := nil;

  SetBounds(0, 0, 23, 22);
  ControlStyle := [csCaptureMouse, csDoubleClicks, csClickEvents];
  ParentFont := True;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';

  FOffSet := 4;
  FImageIndex := -1;

  FStyle := tasButton;
  FGroupIndex := 0;
  FGrouped := true;

  FUnHotTimer := TTimer.Create(self);
  FUnHotTimer.Interval := 1;
  FUnHotTimer.Enabled := false;
  FUnHotTimer.OnTimer := UnHotTimerOnTime;

  ShowHint := False;
  FModalResult := mrNone;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

destructor TAdvButton.Destroy;
begin
  FUnHotTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMDialogChar(var Message: TCMDialogChar);
begin
  if Enabled and IsAccel(Message.CharCode, Caption) then
  begin
    Click;
    Message.Result := 1;
    Exit;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := true;
  if Enabled then
    InvalidateMe;
  FUnHotTimer.Enabled := True;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    exit;

  FUnHotTimer.Enabled := False;
  FMouseInControl := false;
  FHot := false;

  if Enabled then
    InvalidateMe;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.Loaded;
begin
  inherited;

  if (Down <> FInitialDown) then
    Down := FInitialDown;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;

  if (Button <> mbLeft) or not Enabled or (csDesigning in ComponentState) then
    Exit;

  FMouseDownInControl := true;

  ButtonDown;

  if not FDown then
  begin
    FState := absDown;
    Invalidate;
  end;

  if Style = tasCheck then
  begin
    FState := absDown;
    Repaint;
  end;

  FDragging := True;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.MouseMove(Shift: TShiftState; X,
  Y: Integer);
var
  NewState: TAdvButtonState;
begin
  inherited;

  if (csDesigning in ComponentState) then
    Exit;

  if FDragging then
  begin
    if (not FDown) then NewState := absUp
    else NewState := absExclusive;

    if (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight) then
      if FDown then NewState := absExclusive else NewState := absDown;

    if (Style = tasCheck) and FDown then
    begin
      NewState := absDown;
    end;

    if NewState <> FState then
    begin
      FState := NewState;
      Invalidate;
    end;
  end
  else if not FMouseInControl then
    UpdateTracking;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  DoClick: Boolean;
begin
  inherited;

  if (csDesigning in ComponentState) then
    exit;

  FMouseDownInControl := false;
  InvalidateMe;

  if FDragging then
  begin
    FDragging := False;
    DoClick := (X >= 0) and (X < ClientWidth) and (Y >= 0) and (Y <= ClientHeight);
    if FGroupIndex = 0 then
    begin
      // Redraw face in-case mouse is captured
      FState := absUp;
      FMouseInControl := False;
      FHot := false;

      if Style = tasCheck then
      begin
        SetDown(not FDown);
        FState := absUp;
      end;

      if DoClick and not (FState in [absExclusive, absDown]) then
        Invalidate;
    end
    else
      if DoClick then
      begin
        SetDown(not FDown);
        if FDown then Repaint;
      end
      else
      begin
        if FDown then
          FState := absExclusive;
        Repaint;
      end;
    //if DoClick then Click;
    UpdateTracking;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.WndProc(var Message: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.Paint;
var
  Rgn1: HRGN;
  R: TRect;
  i: Integer;
  p: TPoint;
begin
  if not Enabled then
  begin
    FState := absDisabled;
    FDragging := False;
  end
  else
  begin
    if (FState = absDisabled) then
      if FDown and (GroupIndex <> 0) then
        FState := absExclusive
      else
        FState := absUp;
  end;

  if (Style = tasCheck) and (Down) then
  begin
    FState := absDown;
  end;

  if True then
  begin
    // TRANSPARENCY CODE

    R := ClientRect;
    rgn1 :=  CreateRectRgn(R.Left, R.Top, R.Right, R.Bottom);
    SelectClipRgn(Canvas.Handle, rgn1);

    i := SaveDC(Canvas.Handle);
    p := ClientOrigin;
    Windows.ScreenToClient(Parent.Handle, p);
    p.x := -p.x;
    p.y := -p.y;
    MoveWindowOrg(Canvas.Handle, p.x, p.y);

    SendMessage(Parent.Handle, WM_ERASEBKGND, Canvas.Handle, 0);
    // transparency ?
    SendMessage(Parent.Handle, WM_PAINT, Canvas.Handle, 0);

    if (Parent is TWinCtrl) then
     (Parent as TWinCtrl).PaintCtrls(Canvas.Handle, nil);

    RestoreDC(Canvas.Handle, i);

    SelectClipRgn(Canvas.Handle, 0);
    DeleteObject(rgn1);
  end;

  //inherited;

  DrawButton(Canvas);
end;

//------------------------------------------------------------------------------

procedure TAdvButton.DrawButton(ACanvas: TCanvas);
var
  R: TRect;
  Clr, ClrTo, BrClr: TColor;
begin
  if not Assigned(FAppearance) then
    Exit;

  R := ClientRect;  
  Clr := Appearance.Color;
  ClrTo := Appearance.ColorTo;
  BrClr := Appearance.BorderColor;

  if not Enabled then
  begin
    Clr := Appearance.ColorDisabled;
    ClrTo := Appearance.ColorDisabledTo;
    BrClr := Appearance.BorderColorDisabled;
  end
  else if ((FMouseDownInControl and FMouseInControl))then
  begin
    Clr := Appearance.ColorDown;
    ClrTo := Appearance.ColorDown;
    BrClr := Appearance.BorderColorDown;
  end
  else if (Down) then
  begin
    Clr := Appearance.ColorChecked;
    ClrTo := Appearance.ColorChecked;
    BrClr := Appearance.BorderColorChecked;
  end
  else if (FMouseInControl or Self.Focused) then
  begin
    Clr := Appearance.ColorHot;
    ClrTo := Appearance.ColorHot;
    BrClr := Appearance.BorderColorHot;
  end;

  DrawGradientBackGround(Canvas, R, Clr, ClrTo, BrClr, 1, Appearance.Gradient);

  Canvas.Brush.Style := bsClear;
  Canvas.Font.Assign(Appearance.Font);
  DrawButtonContent(Canvas, R, Caption, Images, ImageIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvButton.UpdateExclusive;
var
  Msg: TMessage;
begin
  if (FGroupIndex <> 0) and (Parent <> nil) then
  begin
    Msg.Msg := CM_BUTTONPRESSED;
    Msg.WParam := FGroupIndex;
    Msg.LParam := LParam(Self);
    Msg.Result := 0;
    Parent.Broadcast(Msg);
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvButton.UpdateTracking;
var
  P: TPoint;
begin
  if Enabled then
  begin
    GetCursorPos(P);
    FMouseInControl := not (FindDragTarget(P, True) = Self);
    if FMouseInControl then
      Perform(CM_MOUSELEAVE, 0, 0)
    else
      Perform(CM_MOUSEENTER, 0, 0);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetDown(Value: Boolean);
begin
  if (csLoading in ComponentState) then
    FInitialDown := Value;

  if (FGroupIndex = 0) and (Style = tasButton) then
    Value := False;

  if (Style = tasCheck) then
  begin
    FDown := Value;
    if FDown then
      FState := absDown
    else
      FState := absUp;
    Repaint;
    Exit;
  end;

  if Value <> FDown then
  begin
    if FDown and (not FAllowAllUp) then Exit;
    FDown := Value;
    if Value then
    begin
      if FState = absUp then Invalidate;
      FState := absExclusive
    end
    else
    begin
      FState := absUp;
      Repaint;
    end;
    if Value then UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetState(const Value: TAdvButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetStyle(const Value: TAdvButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.InvalidateMe;
begin
  invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetGrouped(const Value: Boolean);
begin
  FGrouped := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.ButtonDown;
begin
end;

//------------------------------------------------------------------------------


procedure TAdvButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := absUp;
        //if (Action is TCustomAction) then   // UnComment when support Actions
          //TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvButton.GetHot: Boolean;
begin
  Result := FPropHot;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetHot(const Value: Boolean);
var
  OldV: Boolean;
begin
  OldV := FPropHot;
  FPropHot := Value;
  if (State <> absUp) then
    FPropHot := false;

  FPropHot := false;
  if OldV <> FPropHot then
    InvalidateMe;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.UnHotTimerOnTime(Sender: TObject);
var
  CurP: TPoint;
begin
  GetCursorPos(CurP);
  CurP := ScreenToClient(CurP);
  if (not PtInRect(ClientRect, CurP)) then
  begin
    FUnHotTimer.Enabled := False;
    FMouseInControl := false;
    FHot := false;

    if Enabled then
      InvalidateMe;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetParent(AParent: TWinControl);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.Click;
var
  Form: TCustomForm;
  b: Boolean;
begin
  Form := GetParentForm(Self);
  if Form <> nil then
    Form.ModalResult := ModalResult;

  b := False;  
  case ModalResult of
    mrOk, mrYes, mrYesToAll, mrAll: b := False;
    mrCancel, mrAbort, mrIgnore, mrNo, mrNoToAll: b := True;
  end;

  if Assigned(AdvDropDown) and (ModalResult in [mrOk, mrYes, mrYesToAll, mrAll, mrCancel, mrAbort, mrIgnore, mrNo, mrNoToAll]) then
    AdvDropDown.DoHideDropDown(b);

  if not FInternalClick then
    inherited;    
end;

//------------------------------------------------------------------------------

procedure TAdvButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.WMKeyDown(var Message: TWMKeyDown);
begin
  case Message.CharCode of
    VK_RETURN, VK_SPACE:
    begin
      if not FDown then
      begin
        FState := absDown;
        Invalidate;
      end;

      if Style = tasCheck then
      begin
        FState := absDown;
        Repaint;
      end;

      FInternalClick := True;
      Click;
      FInternalClick := False;
    end;
  end;
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetAutoSizeEx;
var
  ts: TSize;
begin
  if not Assigned(FAppearance) then
    Exit;

  ts.cx := 0;
  ts.cy := 0;
  Canvas.Font.Assign(Appearance.Font);
  ts := GetTextSize(Canvas, Caption);

  if Assigned(Images) and (ImageIndex >= 0) then
  begin
    ts.cx := Max(ts.cx, Images.Width);
    ts.cy := Max(ts.cy, Images.Height);
  end;

  Height := ts.cy + DD_BUTTONOFFSET_Y * 2;
  Width := ts.cx + DD_BUTTONOFFSET_X * 2;
end;

//------------------------------------------------------------------------------

procedure TAdvButton.SetAdvDropDown(const Value: TAdvCustomDropDown);
begin
  if (FAdvDropDown <> Value) then
    FAdvDropDown := Value;
end;

//------------------------------------------------------------------------------

{ TDbgList }

procedure TDbgList.AssignList(ListA: TList);
var
  I: Integer;
begin
  Clear;
  for I := 0 to ListA.Count - 1 do
    Add(ListA[I]);
end;

//------------------------------------------------------------------------------

function TDbgList.GetItemsEx(Index: Integer): Pointer;
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list read access');
  end;

  if Index < Count then
    Result := inherited Items[Index]
  else
    Result := nil;
end;

//------------------------------------------------------------------------------

procedure TDbgList.SetItemsEx(Index: Integer; const Value: Pointer);
begin
  if (Index >= Count) then
  begin
    raise Exception.Create('Index out of bounds in list write access');
  end;
  if Index < Count then
    inherited Items[Index] := value;
end;

//------------------------------------------------------------------------------

{ TAdvHeader }

constructor TAdvHeader.Create(AOwner: TComponent);
begin
  inherited;
  FAppearance := THeaderAppearance.Create(AOwner);
  FAppearance.OnChange := AppearanceChanged;
  FAppearance.OnButtonsChanged := OnButtonsChanged;
  ControlStyle := [csAcceptsControls, csClickEvents, csDoubleClicks];
  BorderWidth := 0;
  FButtonList := TDbgList.Create;
  DoubleBuffered := True;
end;

//------------------------------------------------------------------------------

destructor TAdvHeader.Destroy;
begin
  FAppearance.Free;
  FButtonList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.AppearanceChanged(Sender: TObject);
begin
  Visible := Appearance.Visible;
  Height := Appearance.Height;  
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.SetAppearance(const Value: THeaderAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.Paint;
var
  R, R1: TRect;
  ml,hl,XPos,YPos,XSize, YSize: Integer;
  hr,CR: TRect;
  CV,CT, NewID, s: string;
  Anchor,Stripped,FocusAnchor,AnchorHint: string;
begin
  //inherited;
  R := ClientRect;

  if Assigned(FOnDrawBackGround) then
    FOnDrawBackGround(Self, Canvas, R)
  else
  begin
    //--- Draw Gradient
    if (Appearance.ColorTo <> clNone) then
      DrawGradient(Canvas, Appearance.Color, Appearance.ColorTo, 80, R, Appearance.GradientDirection = gdHorizontal)
    else if (Appearance.Color <> clNone) then
    begin
      Canvas.Brush.Color := Appearance.Color;
      Canvas.FillRect(R);
    end;

    //--- Draw Border
    if (Appearance.BorderColor <> clNone) and (Appearance.BorderWidth > 0) then
    begin
      // draw top border
      Canvas.Pen.Width := Appearance.BorderWidth;
      Canvas.Pen.Color := Appearance.BorderColor;
      Canvas.MoveTo(R.Left, R.Bottom - 1);
      Canvas.LineTo(R.Right, R.Bottom - 1);

      // draw left/right/bottom border is dropdown window has no border
      if not (Assigned(AdvDropDown) and (AdvDropDown.DropDownBorderWidth > 0) and (AdvDropDown.DropDownBorderColor <> clNone)) then
      begin
        Canvas.MoveTo(R.Left, R.Bottom - 1);
        Canvas.LineTo(R.Left, R.Top);

        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Right, R.Top);

        Canvas.MoveTo(R.Right, R.Top);
        Canvas.LineTo(R.Right, R.Bottom - 1);
      end;

      //--- Border 3d shade
      Canvas.Pen.Width := 1;
      R1 := R;
      //InflateRect(R1, -Appearance.BorderWidth, -Appearance.BorderWidth);
      R1.Bottom := R1.Bottom - Appearance.BorderWidth;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BlendColor(Appearance.BorderColor, clWhite, 20);
      Canvas.Rectangle(R1);
      //---
      {
      Canvas.Brush.Style := bsClear;
      //--- Border Shade
      Canvas.Pen.Width := 1;
      R1 := R;
      InflateRect(R1, -Appearance.BorderWidth, -Appearance.BorderWidth);
      Canvas.Pen.Color := BlendColor(Appearance.BorderColor, clWhite, 20);
      Canvas.Rectangle(R1);
      //---
      Canvas.Pen.Width := Appearance.BorderWidth;
      Canvas.Pen.Color := Appearance.BorderColor;
      Canvas.Rectangle(R);
      }
      //DrawSelectionGradient(Canvas,$D7FFFD,$58D4FC,$58D4FC,$B3F1FC,$F4FEFF,$CAF8FF,$8DD7D3,$C3D9DF,Color,R,gpMiddle);
    end;

    R1 := R;
    InflateRect(R1, -Appearance.BorderWidth, -Appearance.BorderWidth);

    //--- Draw Image
    if (Appearance.ImageIndex >= 0) and Assigned(Images) then
    begin
      XPos := R1.Left + DD_IMAGEXOFFSET;
      YPos := R1.Top + (R1.Bottom - R1.Top - Images.Height) div 2;
      Images.Draw(Canvas, XPos, YPos, Appearance.ImageIndex);
      R1.Left := XPos + Images.Width + DD_IMAGEGAP;
    end;
    //---

    //--- Draw Caption
    s := Appearance.Caption;
    if Assigned(FOnGetText) then
      FOnGetText(Self, s);
    if (s <> '') then
    begin
      if (FButtonList.Count > 0) then
        R1.Right := GetButtonsCoveredRect.Right;
      XPos := 0;
      YPos := 0;
      Canvas.Font.Assign(Appearance.Font);
      HTMLDrawEx(Canvas, s, R1, Images, XPos, YPos,-1,0,1,
                 False, True,False,False,False,False, True{not EnhTextSize},False,'',
                 0.0,clBlue{FURLColor},clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,AnchorHint,
                 XSize,YSize,ml,hl,hr,CR,NewID,CV,CT,nil{FImageCache},nil{FContainer},self.Handle);

      if ((R1.Bottom - R1.Top) > YSize) then
        R1.Top := R1.Top + ((R1.Bottom - R1.Top) - YSize) div 2;

      HTMLDrawEx(Canvas, s, R1, Images, XPos, YPos,-1,0,1,
                 False,False,False,False,False,False, True{not EnhTextSize},False,'',
                 0.0,clBlue{FURLColor},clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,AnchorHint,
                 XSize,YSize,ml,hl,hr,CR,NewID,CV,CT,nil{FImageCache},nil{FContainer},self.Handle);
    end;
    //---
  end;  
end;

//------------------------------------------------------------------------------

function TAdvHeader.GetButtons: THeaderButtons;
begin
  Result := nil;
  if Assigned(FAdvDropDown) then
    Result := FAdvDropDown.DropDownHeader.Buttons;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.CreateButtons;
var
  i, mh: Integer;
  Btn: TAdvButton;
begin
  if not Assigned(Buttons) or not Assigned(AdvDropDown) then
    Exit;
  DestroyButtons;
  mh := GetMaxButtonHeight;
  for i:= 0 to Buttons.Count -1 do
  begin
    //--- CreateButton
    Btn := TAdvButton.Create(Self);
    Btn.Parent := Self;
    Btn.Appearance := AdvDropDown.ButtonAppearance;
    Btn.Caption := Buttons[i].Caption;
    Btn.Images := Self.Images;
    Btn.ImageIndex := Buttons[i].ImageIndex;
    Btn.Enabled := Buttons[i].Enabled;
    Btn.SetAutoSizeEx;
    if (Buttons[i].Width > 0) then
      Btn.Width := Buttons[i].Width;
    if (Buttons[i].Height > 0) then
      Btn.Height := Buttons[i].Height;
    if (Btn.Height > mh) then
      Btn.Height := mh;

    Btn.Tag := i;   // Buttons index
    Btn.OnClick := OnButtonClick;
    //---
    FButtonList.Add(Btn);
  end;

  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.UpdateButtons;
var
  i: Integer;
  Btn: TAdvButton;
begin
  if not Assigned(Buttons) or not Assigned(AdvDropDown) then
    Exit;

  for i := 0 to FButtonList.Count - 1 do
  begin
    //--- UpdateButton
    Btn := TAdvButton(FButtonList[i]);
    Btn.Appearance := AdvDropDown.ButtonAppearance;
    Btn.Images := Self.Images;
    if (i < Buttons.Count) then
    begin
      Btn.Caption := Buttons[i].Caption;
      Btn.ImageIndex := Buttons[i].ImageIndex;
      Btn.Enabled := Buttons[i].Enabled;
    end;
    Btn.AutoSize := True;
    //---
  end;
  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.ArrangeButtons;
var
  i, X, Y: Integer;
  R: TRect;
begin
  if (FButtonList.Count <= 0) then
    Exit;
    
  R := GetButtonsRect;

  //Y := R.Top;

  if Appearance.ButtonAlignment = baRight then
  begin
    X := R.Right;
    for i := 0 to FButtonList.Count -1 do
    begin
      Y := Max(R.Top, (R.Top + (R.Bottom - R.Top) - TAdvButton(FButtonList[i]).Height) div 2);
      TAdvButton(FButtonList[i]).Top := Y;
      TAdvButton(FButtonList[i]).Left := X - TAdvButton(FButtonList[i]).Width;
      X := TAdvButton(FButtonList[i]).Left;
    end;
  end
  else
  begin
    X := R.Left;
    for i := 0 to FButtonList.Count -1 do
    begin
      Y := Max(R.Top, (R.Top + (R.Bottom - R.Top) - TAdvButton(FButtonList[i]).Height) div 2);
      TAdvButton(FButtonList[i]).Top := Y;
      TAdvButton(FButtonList[i]).Left := X;
      X := X + TAdvButton(FButtonList[i]).Width;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvHeader.GetButtonsCoveredRect: TRect;
begin
  Result := Rect(-1 , -1, -1, -1);
  if (FButtonList.Count > 0) then
  begin
    Result := GetButtonsRect;
    Result.Right := TAdvButton(FButtonList[FButtonList.Count - 1]).Left - 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.DestroyButtons;
var
  i: Integer;
begin
  for i := FButtonList.Count -1 downto 0 do
  begin
    TAdvButton(FButtonList[i]).Free;
  end;
  FButtonList.Clear;
end;

//------------------------------------------------------------------------------

function TAdvHeader.GetButtonsRect: TRect;
var
  bw: Integer;
begin
  Result := ClientRect;
  bw := 2;
  Result := Rect(Result.Left + bw, Result.Top + bw, Result.Right - bw, Result.Bottom - bw - 1);
end;

//------------------------------------------------------------------------------

function TAdvHeader.GetMaxButtonHeight: Integer;
var
  R: TRect;
begin
  R := GetButtonsRect;
  Result := R.Bottom - R.Top;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.SetAdvDropDown(const Value: TAdvCustomDropDown);
begin
  FAdvDropDown := Value;
  if Assigned(FAdvDropDown) then
  begin
    Images := AdvDropDown.Images;
    Appearance.Assign(AdvDropDown.DropDownHeader);
  end
  else
  begin
    Images := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.Initialize;
begin
  if not Assigned(FAdvDropDown) then
    Exit;
  CreateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.Update;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvHeader.OnButtonClick(Sender: TObject);
begin
  if (Sender is TAdvButton) and Assigned(AdvDropDown) and Assigned(AdvDropDown.OnDropDownHeaderButtonClick) then
    AdvDropDown.OnDropDownHeaderButtonClick(AdvDropDown, TAdvButton(Sender).Tag);
end;

//------------------------------------------------------------------------------

procedure TAdvHeader.OnButtonsChanged(Sender: TObject);
begin
  
end;

//------------------------------------------------------------------------------

{ TAdvFooter }

constructor TAdvFooter.Create(AOwner: TComponent);
begin
  inherited;
  FAppearance := TFooterAppearance.Create(AOwner);
  FAppearance.OnChange := AppearanceChanged;
  ControlStyle := [csAcceptsControls, csCaptureMouse, csClickEvents, csDoubleClicks];
  BorderWidth := 0;
  FButtonList := TDbgList.Create;
  DoubleBuffered := True;
  FOldCursor := crDefault;
end;

//------------------------------------------------------------------------------

destructor TAdvFooter.Destroy;
begin
  FAppearance.Free;
  FButtonList.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.AppearanceChanged(Sender: TObject);
begin
  Visible := Appearance.Visible;
  Height := Appearance.Height;  
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Images) then
    Images := nil;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.SetAppearance(const Value: TFooterAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.SetImages(const Value: TCustomImageList);
begin
  FImages := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.Paint;
var
  R, R1: TRect;
  ml,hl,XPos,YPos,XSize, YSize: Integer;
  hr,CR: TRect;
  CV,CT, NewID, s: string;
  Anchor,Stripped,FocusAnchor,AnchorHint: string;
begin
  //inherited;
  R := ClientRect;

  if Assigned(FOnDrawBackGround) then
    FOnDrawBackGround(Self, Canvas, R)
  else
  begin
    //--- Draw Gradient
    if (Appearance.ColorTo <> clNone) then
      DrawGradient(Canvas, Appearance.Color, Appearance.ColorTo, 80, R, Appearance.GradientDirection = gdHorizontal)
    else if (Appearance.Color <> clNone) then
    begin
      Canvas.Brush.Color := Appearance.Color;
      Canvas.FillRect(R);
    end;

    //--- Draw Border
    if (Appearance.BorderColor <> clNone) and (Appearance.BorderWidth > 0) then
    begin
      // draw top border
      Canvas.Pen.Width := Appearance.BorderWidth;
      Canvas.Pen.Color := Appearance.BorderColor;
      Canvas.MoveTo(R.Left, R.Top);
      Canvas.LineTo(R.Right, R.Top);

      // draw left/right/bottom border is dropdown window has no border
      if not (Assigned(AdvDropDown) and (AdvDropDown.DropDownBorderWidth > 0) and (AdvDropDown.DropDownBorderColor <> clNone)) then
      begin
        Canvas.MoveTo(R.Left, R.Top);
        Canvas.LineTo(R.Left, R.Bottom);

        Canvas.MoveTo(R.Left, R.Bottom);
        Canvas.LineTo(R.Right, R.Bottom);

        Canvas.MoveTo(R.Right, R.Bottom);
        Canvas.LineTo(R.Right, R.Top);
      end;

      //--- Border 3d shade
      Canvas.Pen.Width := 1;
      R1 := R;
      //InflateRect(R1, -Appearance.BorderWidth, -Appearance.BorderWidth);
      R1.Top := R1.Top + Appearance.BorderWidth;
      Canvas.Brush.Style := bsClear;
      Canvas.Pen.Color := BlendColor(Appearance.BorderColor, clWhite, 20);
      Canvas.Rectangle(R1);
      //---
      {
      Canvas.Pen.Width := Appearance.BorderWidth;
      Canvas.Pen.Color := Appearance.BorderColor;
      Canvas.Rectangle(R);
      }
      //DrawSelectionGradient(Canvas,$D7FFFD,$58D4FC,$58D4FC,$B3F1FC,$F4FEFF,$CAF8FF,$8DD7D3,$C3D9DF,Color,R,gpMiddle);
    end;

    R1 := R;
    InflateRect(R1, -Appearance.BorderWidth, -Appearance.BorderWidth);

    //--- Draw Image
    if (Appearance.ImageIndex >= 0) and Assigned(Images) then
    begin
      XPos := R1.Left + DD_IMAGEXOFFSET;
      YPos := R1.Top + (R1.Bottom - R1.Top - Images.Height) div 2;
      Images.Draw(Canvas, XPos, YPos, Appearance.ImageIndex);
      R1.Left := XPos + Images.Width + DD_IMAGEGAP;
    end;
    //---

    //--- Draw Caption
    s := Appearance.Caption;
    if Assigned(FOnGetText) then
      FOnGetText(Self, s);
    if (s <> '') then
    begin
      if (FButtonList.Count > 0) then
        R1.Right := GetButtonsCoveredRect.Right;
      XPos := 0;
      YPos := 0;
      Canvas.Font.Assign(Appearance.Font);
      HTMLDrawEx(Canvas, s, R1, Images, XPos, YPos,-1,0,1,
                 False, True,False,False,False,False, True{not EnhTextSize},False,'',
                 0.0,clBlue{FURLColor},clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,AnchorHint,
                 XSize,YSize,ml,hl,hr,CR,NewID,CV,CT,nil{FImageCache},nil{FContainer},self.Handle);


      if ((R1.Bottom - R1.Top) > YSize) then
        R1.Top := R1.Top + ((R1.Bottom - R1.Top) - YSize) div 2;

      HTMLDrawEx(Canvas, s, R1, Images, XPos, YPos,-1,0,1,
                 False,False,False,False,False,False, True{not EnhTextSize},False,'',
                 0.0,clBlue{FURLColor},clNone,clNone,clGray,Anchor,Stripped,FocusAnchor,AnchorHint,
                 XSize,YSize,ml,hl,hr,CR,NewID,CV,CT,nil{FImageCache},nil{FContainer},self.Handle);
    end;
    //---

    //--- SizeGrip
    if HasSizeGrip then
    begin
      R1 := Rect(R.Right - GRIP_SIZE - Appearance.BorderWidth - 1, 1, R.Right - Appearance.BorderWidth - 1, R.Bottom);
      DrawSizeGrip(R1);
    end;
    //---
  end;  
end;

//------------------------------------------------------------------------------

function TAdvFooter.GetButtons: TFooterButtons;
begin
  Result := nil;
  if Assigned(FAdvDropDown) then
    Result := FAdvDropDown.DropDownFooter.Buttons;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.WMSize(var Message: TWMSize);
begin
  inherited;
  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.CreateButtons;
var
  i, mh: Integer;
  Btn: TAdvButton;
begin
  if not Assigned(Buttons) or not Assigned(AdvDropDown) then
    Exit;
  DestroyButtons;
  mh := GetMaxButtonHeight;
  for i:= 0 to Buttons.Count -1 do
  begin
    //--- CreateButton
    Btn := TAdvButton.Create(Self);
    Btn.Parent := Self;
    Btn.AdvDropDown := AdvDropDown;
    Btn.Appearance := AdvDropDown.ButtonAppearance;
    Btn.Caption := Buttons[i].Caption;
    Btn.Images := Self.Images;
    Btn.ImageIndex := Buttons[i].ImageIndex;
    Btn.Enabled := Buttons[i].Enabled;
    Btn.ModalResult := Buttons[i].ModalResult;
    Btn.SetAutoSizeEx;
    if (Buttons[i].Width > 0) then
      Btn.Width := Buttons[i].Width;
    if (Buttons[i].Height > 0) then
      Btn.Height := Buttons[i].Height;
    if (Btn.Height > mh) then
      Btn.Height := mh;
    Btn.Tag := i; // Buttons index
    Btn.OnClick := OnButtonClick;
    //---
    FButtonList.Add(Btn);
  end;

  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.ArrangeButtons;
var
  i, X, Y: Integer;
  R: TRect;
begin
  R := GetButtonsRect;
  //Y := R.Top;

  if Appearance.ButtonAlignment = baRight then
  begin
    X := R.Right;
    for i := 0 to FButtonList.Count -1 do
    begin
      Y := Max(R.Top, (R.Top + (R.Bottom - R.Top) - TAdvButton(FButtonList[i]).Height) div 2);
      TAdvButton(FButtonList[i]).Top := Y;
      TAdvButton(FButtonList[i]).Left := X - TAdvButton(FButtonList[i]).Width;
      X := TAdvButton(FButtonList[i]).Left;
    end
  end
  else
  begin
    X := R.Left;
    for i := 0 to FButtonList.Count -1 do
    begin
      Y := Max(R.Top, (R.Top + (R.Bottom - R.Top) - TAdvButton(FButtonList[i]).Height) div 2);
      TAdvButton(FButtonList[i]).Top := Y;
      TAdvButton(FButtonList[i]).Left := X;
      X := X + TAdvButton(FButtonList[i]).Width;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvFooter.GetButtonsCoveredRect: TRect;
begin
  Result := Rect(-1 , -1, -1, -1);
  if (FButtonList.Count > 0) then
  begin
    Result := GetButtonsRect;
    Result.Right := TAdvButton(FButtonList[FButtonList.Count - 1]).Left - 2;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.DestroyButtons;
var
  i: Integer;
begin
  for i := FButtonList.Count -1 downto 0 do
  begin
    TAdvButton(FButtonList[i]).Free;
  end;
  FButtonList.Clear;
end;

//------------------------------------------------------------------------------

function TAdvFooter.GetButtonsRect: TRect;
var
  bw, sg: Integer;
begin
  Result := ClientRect;
  bw := 2;

  if HasSizeGrip then
    sg := 12
  else
    sg := 0;
  Result := Rect(Result.Left + bw, Result.Top + bw, Result.Right - bw - sg, Result.Bottom - bw);
end;

//------------------------------------------------------------------------------

function TAdvFooter.GetMaxButtonHeight: Integer;
var
  R: TRect;
begin
  R := GetButtonsRect;
  Result := R.Bottom - R.Top;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.Initialize;
begin
  if not Assigned(FAdvDropDown) then
    Exit;
  CreateButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.UpdateButtons;
var
  i: Integer;
  Btn: TAdvButton;
begin
  if not Assigned(Buttons) or not Assigned(AdvDropDown) then
    Exit;

  for i := 0 to FButtonList.Count - 1 do
  begin
    //--- UpdateButton
    Btn := TAdvButton(FButtonList[i]);
    Btn.Appearance := AdvDropDown.ButtonAppearance;
    Btn.Images := Self.Images;
    if (i < Buttons.Count) then
    begin
      Btn.Caption := Buttons[i].Caption;
      Btn.ImageIndex := Buttons[i].ImageIndex;
      Btn.Enabled := Buttons[i].Enabled;
    end;
    Btn.AutoSize := True;
    //---
  end;
  ArrangeButtons;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.SetAdvDropDown(const Value: TAdvCustomDropDown);
begin
  FAdvDropDown := Value;
  if Assigned(FAdvDropDown) then
  begin
    Images := AdvDropDown.Images;
    Appearance.Assign(AdvDropDown.DropDownFooter);
  end
  else
  begin
    Images := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.DrawSizeGrip(R: TRect);
var
  sp, fx, fy: Integer;
  Clr: TColor;
begin
  sp := 4;
  fx := 5;
  fy := 2;
  with Canvas do
  begin
    // Light Dots
    Clr := clWhite; // TODO: set proper color
    Brush.Color := BlendColor(Clr, clWhite, 50);
    Pen.Color := Brush.Color;
    Rectangle(R.Left + fx, R.Bottom - fy, R.Left + fx + 2, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy, R.Left + fx + 2 + sp, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2);

    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp*2, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp*2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy - sp, R.Left + fx + 2 + sp, R.Bottom - fy - 2 - sp);


    // Dark Dots
    fx := fx - 1;
    fy := fy + 1;

    Clr := Appearance.BorderColor;
    Canvas.Brush.Color := BlendColor(Clr, clWhite, 100);
    Pen.Color := Brush.Color;
    Rectangle(R.Left + fx, R.Bottom - fy, R.Left + fx + 2, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy, R.Left + fx + 2 + sp, R.Bottom - fy - 2);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2);

    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp);
    Rectangle(R.Left + fx + sp*2, R.Bottom - fy - sp*2, R.Left + fx + 2 + sp*2, R.Bottom - fy - 2 - sp*2);
    Rectangle(R.Left + fx + sp, R.Bottom - fy - sp, R.Left + fx + 2 + sp, R.Bottom - fy - 2 - sp);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.WMNCHitTest(var Msg: TWMNCHitTest);
var
  pt: TPoint;
begin
  inherited;
  //if (csDesigning in ComponentState) or not HasSizeGrip then
  Exit;

  pt := ScreenToClient(point(msg.xpos,msg.ypos));

  if (pt.y > height - GetSystemMetrics(SM_CYHSCROLL)) and
     (pt.x > width - GetSystemMetrics(SM_CXHSCROLL)) and
     (Msg.Result = htClient) and Appearance.SizeGrip then
  begin
    SetWindowPos(Parent.Handle, HWND_TOP,0,0,0,0,  SWP_NOMOVE or SWP_NOSIZE);
    Msg.Result := HTBOTTOMRIGHT;
    //SendMessage (Parent.Handle, WM_NCHITTEST, 2, MakeLParam(Msg.XPos, Msg.YPos));
    SendMessage(Parent.Handle,WM_SYSCOMMAND,SC_SIZE + 1,0);
  end
  else
  begin
    if (pt.Y > Height - 4) or (pt.X > Width - 4) then
      Msg.Result := HTTRANSPARENT;
  end;
end;

//------------------------------------------------------------------------------

function TAdvFooter.HasSizeGrip: Boolean;
begin
  Result := Appearance.SizeGrip and Assigned(AdvDropDown) and AdvDropDown.DropDownSizeable;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.CreateParams(var Params: TCreateParams);
const
  GripStyles: array[Boolean] of DWORD = (CCS_TOP, SBARS_SIZEGRIP);
begin
  inherited CreateParams(Params);

  with Params do
  begin
    //Style := Style or GripStyles[Appearance.FSizeGrip and hasSizeGrip];
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if HasSizeGrip and (Button = mbLeft) and Assigned(Parent) and (X > Width - 12) and (Y > Height - 15) then
  begin
    //SendMessage(Handle, WM_LButtonUp, 0, 0);
    ReleaseCapture;
    SendMessage(Parent.Handle, WM_NCLButtonDown, HTBOTTOMRIGHT, 0);
    //Screen.Cursor := crSizeNWSE;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if HasSizeGrip and Assigned(Parent) and (X > Width - 12) and (Y > Height - 15) then
  begin
    SendMessage(Parent.Handle, WM_NCHITTEST, 2, 0);
    //SendMessage(Handle,WM_SYSCOMMAND,SC_MOVE+1,0);

    if (Screen.Cursor <> crSizeNWSE) then
      Screen.Cursor := crSizeNWSE;
  end
  else
  begin
    if (Screen.Cursor <> FOldCursor) then
      Screen.Cursor := FOldCursor;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;

  if (Screen.Cursor <> FOldCursor) then
    Screen.Cursor := FOldCursor;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.CMMouseEnter(var Message: TMessage);
begin
  if (Screen.Cursor <> crSizeNWSE) then
    FOldCursor := Screen.Cursor;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (Screen.Cursor <> FOldCursor) then
    Screen.Cursor := FOldCursor;
end;

//------------------------------------------------------------------------------

procedure TAdvFooter.OnButtonClick(Sender: TObject);
begin
  if (Sender is TAdvButton) and Assigned(AdvDropDown) and Assigned(AdvDropDown.OnDropDownFooterButtonClick) then
    AdvDropDown.OnDropDownFooterButtonClick(AdvDropDown, TAdvButton(Sender).Tag);
end;

//------------------------------------------------------------------------------

{ TSelectorItem }

procedure TSelectorItem.Assign(Source: TPersistent);
begin
  if (Source is TSelectorItem) then
  begin
    Caption := (Source as TSelectorItem).Caption;
    Color := (Source as TSelectorItem).Color;
    ImageIndex := (Source as TSelectorItem).ImageIndex;
    Image.Assign((Source as TSelectorItem).Image);
    Enabled := (Source as TSelectorItem).Enabled;
    Hint := (Source as TSelectorItem).Hint;
  end
  else
    inherited Assign(Source);
end;

//------------------------------------------------------------------------------

procedure TSelectorItem.Changed;
begin
  TSelectorItems(Collection).Change;
end;

//------------------------------------------------------------------------------

constructor TSelectorItem.Create(Collection: TCollection);
begin
  inherited;
  FCaption := '';
  FImageIndex := -1;
  FImage := TPicture.Create;
  FEnabled := True;
  FColor := clNone;
end;

//------------------------------------------------------------------------------

destructor TSelectorItem.Destroy;
begin
  FImage.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TSelectorItem.SetCaption(const Value: TCaption);
begin
  if (FCaption <> Value) then
  begin
    FCaption := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectorItem.SetColor(const Value: TColor);
begin
  if (FColor <> Value) then
  begin
    FColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TSelectorItem.SetImage(const Value: TPicture);
begin
  FImage.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TSelectorItem.SetImageIndex(const Value: Integer);
begin
  if (FImageIndex <> Value) then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TSelectorItems }

function TSelectorItems.Add: TSelectorItem;
begin
  Result := TSelectorItem(inherited Add);
end;

//------------------------------------------------------------------------------

procedure TSelectorItems.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

constructor TSelectorItems.Create(AOwner: TPersistent);
begin
  inherited Create(TSelectorItem);
  FMyOwner := AOwner;
end;

//------------------------------------------------------------------------------

function TSelectorItems.GetItem(Index: Integer): TSelectorItem;
begin
  Result := TSelectorItem(inherited Items[Index]);
end;

//------------------------------------------------------------------------------

function TSelectorItems.GetOwner: TPersistent;
begin
  Result := FMyOwner;
end;

//------------------------------------------------------------------------------

function TSelectorItems.Insert(Index: Integer): TSelectorItem;
begin
  Result := TSelectorItem(inherited Insert(Index));
end;

//------------------------------------------------------------------------------

procedure TSelectorItems.SetItem(Index: Integer;
  const Value: TSelectorItem);
begin
  inherited Items[Index] := Value;
end;

//------------------------------------------------------------------------------

{ TCustomItemSelector }

constructor TCustomItemSelector.Create(AOwner: TComponent);
begin
  inherited;
  if not (AOwner is TAdvCustomItemSelector) then
    raise Exception.Create('Invalid parent');

  FAdvItemSelector := TAdvCustomItemSelector(AOwner);  
  FItemIndex := -1;
  FItemHot := -1;
  FOffSetX := 2;
  FOffSetY := 2;
  FItemOffSetX := 4;
  FItemOffSetY := 4;
  FCaptionGap := 4;
  ParentColor := True;
end;

//------------------------------------------------------------------------------

destructor TCustomItemSelector.Destroy;
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SetItemIndex(const Value: Integer);
var
  i: Integer;
begin
  //if (FItemIndex <> Value) then
  begin
    if (Value >= 0) and (Value < Items.Count) then
      if not Items[Value].Enabled then
        Exit;

    if (FItemIndex >= 0) and (FItemIndex <> Value) then  // refresh old hot item
    begin
      i := FItemIndex;
      FItemIndex := -1;
      InvalidateItem(i);
    end;

    FItemIndex := Value;
    if (FItemIndex >= 0) then
    begin
      DrawItem(FItemIndex, Canvas);
      if Assigned(OnItemSelect) then
        FOnItemSelect(Self);
    end;    
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SetItemHot(const Value: Integer);
var
  i: Integer;
begin
  if (FItemHot <> Value) then
  begin
    if (Value >= 0) and (Value < Items.Count) then
      if not Items[Value].Enabled then
        Exit;

    if (FItemHot >= 0) then  // refresh old hot item
    begin
      i := FItemHot;
      FItemHot := -1;
      InvalidateItem(i);
    end;

    FItemHot := Value;
    if (FItemHot >= 0) then
      DrawItem(FItemHot, Canvas);
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ItemAtPos(X, Y);
  if (i >= 0) {and (i <> FItemIndex)} then
  begin
    if Items[i].Enabled then
    begin
      ItemIndex := i;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  i: Integer;
begin
  inherited;

  i := ItemAtPos(X, Y);
  if (i >= 0) then
  begin
    if (i <> FItemHot) then
    begin
      if Items[i].Enabled then
        ItemHot := i
      else
        ItemHot := -1;

      Hint := Items[i].Hint;
      Application.CancelHint;
    end;
  end
  else
  begin
    if (FItemHot >= 0) then
    begin
      ItemHot := -1;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.Paint;
begin
  inherited;
  DrawItems(Canvas);
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.DrawItem(Index: Integer; ACanvas: TCanvas);
var
  Clr, ClrTo, MClr, MClrTo, BrClr, LTop, LBottom, EdgClr: TColor;
  R, TR, IR: TRect;
  iw, ih: Integer;
  ts: TSize;
  DrawBkg: Boolean;
begin
  if (Index < 0) or (Index >= Items.Count) or not Assigned(ACanvas) or not Assigned(AdvItemSelector) then
    Exit;

  R := GetItemRect(Index);

  if (R.Left < 0) and (R.Right < 0) then
    Exit;

  BrClr := AdvItemSelector.ItemAppearance.BorderColor;
  LTop := AdvItemSelector.ItemAppearance.BorderColorTop;
  LBottom := AdvItemSelector.ItemAppearance.BorderColorBottom;
  EdgClr := AdvItemSelector.ItemAppearance.EdgeColor;

  Clr := clNone;
  ClrTo := clNone;
  MClr := clNone;
  MClrTo := clNone;
  DrawBkg := False;

  //--- Draw Background
  if (Index = FItemHot) then
  begin
    Clr := ItemColorHot;
    ClrTo := ItemColorHotTo;
    MClr := AdvItemSelector.ItemAppearance.ColorMirrorHot;
    MClrTo := AdvItemSelector.ItemAppearance.ColorMirrorHotTo;

    if (AdvItemSelector.ItemAppearance.BorderColorHot <> clNone) then
    begin
      BrClr := AdvItemSelector.ItemAppearance.BorderColorHot;
      LTop := clNone;
      LBottom := clNone;
      EdgClr := clNone;
    end;

    DrawBkg := True;
  end;

  if (Index = ItemIndex) then
  begin
    Clr := ItemColorSelected;
    ClrTo := ItemColorSelectedTo;
    MClr := AdvItemSelector.ItemAppearance.ColorMirrorSelected;
    MClrTo := AdvItemSelector.ItemAppearance.ColorMirrorSelectedTo;

    if (AdvItemSelector.ItemAppearance.BorderColorSelected <> clNone) then
    begin
      BrClr := AdvItemSelector.ItemAppearance.BorderColorSelected;
      LTop := clNone;
      LBottom := clNone;
      EdgClr := clNone;
    end;


    DrawBkg := True;
  end;

  if DrawBkg then
  begin
    if (LTop <> clNone) then
      DrawSelectionGradient(Canvas, CLr, ClrTo, MClr, MClrTO, LTop, LBottom, BrClr, EdgClr, Color, R, gpFull)
    else
      DrawGradientBackGround(Canvas, R, Clr, ClrTo, BrClr, 1, gdVertical);
  end
  else
  begin
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
  end;
  //---

  if Assigned(AdvItemSelector.OnDrawItem) then
  begin
    AdvItemSelector.OnDrawItem(Self, Canvas, R, Index);
    Exit;
  end;

  iw := 0;
  ih := 0;
  if (SelectorType = stImage) then
  begin
    if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
    begin
      iw := Items[Index].Image.Graphic.Width;
      ih := Items[Index].Image.Graphic.Height;
    end
    else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
    begin
      iw := Images.Width;
      ih := Images.Height;
    end;
  end
  else // SelectorType = stColor
  begin
    iw := ColorBoxWidth;
    ih := ColorBoxHeight;
  end;

  TR := R;
  InflateRect(TR, -ItemOffSetX, -ItemOffSetY);
  IR := TR;

  ACanvas.Font.Assign(AdvItemSelector.ItemAppearance.Font);

  if (Index = FItemHot) then
    ACanvas.Font.Color := AdvItemSelector.ItemAppearance.ColorHotText;

  if (Index = ItemIndex) then
    ACanvas.Font.Color := AdvItemSelector.ItemAppearance.ColorSelectedText;

  case ItemLayout of
    ilCaptionLeft:
    begin
      if (Items[Index].Caption <> '') then
      begin
        TR.Right := TR.Right - iw;
        //-- setting center vertically
        ts := DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, False, Handle);
        if (ts.cy < (TR.Bottom - TR.Top)) then
          TR.Top := TR.Top + (((TR.Bottom - TR.Top) - ts.cy) div 2);
        //--
        ts := DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, True, Handle);
        IR.Left := TR.Left + ts.cx + CaptionGap;
      end;

      if (SelectorType = stImage) then
      begin
        if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
        begin
          aCanvas.Draw(IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Items[Index].Image.Graphic);
        end
        else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
        begin
          Images.Draw(aCanvas, IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Items[Index].ImageIndex, Items[Index].Enabled);
        end;
      end
      else // SelectorType = stColor
      begin
        IR.Right := IR.Left + iw;
        IR.Bottom := IR.Top + ih;

        aCanvas.Pen.Color := clBlack;   // border color
        aCanvas.Brush.Color := Items[Index].Color;
        aCanvas.Rectangle(IR);
      end;
    end;
    ilCaptionRight:
    begin
      if (SelectorType = stImage) then
      begin
        if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
        begin
          aCanvas.Draw(IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Items[Index].Image.Graphic);
          TR.Left := IR.Left + Items[Index].Image.Graphic.Width + CaptionGap;
        end
        else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
        begin
          Images.Draw(aCanvas, IR.Left, IR.Top + ((IR.Bottom - IR.Top - ih) div 2), Items[Index].ImageIndex, Items[Index].Enabled);
          TR.Left := IR.Left + Images.Width + CaptionGap;
        end;
      end
      else // SelectorType = stColor
      begin
        IR.Right := IR.Left + iw;
        IR.Bottom := IR.Top + ih;

        aCanvas.Pen.Color := clBlack;   // border color
        aCanvas.Brush.Color := Items[Index].Color;
        aCanvas.Rectangle(IR);

        TR.Left := IR.Left + iw + CaptionGap;
      end;

      if (Items[Index].Caption <> '') then
      begin
        //-- setting center vertically  
        ts := DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, False, Handle);
        if (ts.cy < (TR.Bottom - TR.Top)) then
          TR.Top := TR.Top + (((TR.Bottom - TR.Top) - ts.cy) div 2);
        //--
        DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, True, Handle);
      end;
    end;
    ilCaptionTop:
    begin
      if (Items[Index].Caption <> '') then
      begin
        TR.Bottom := TR.Bottom - ih;
        ts := DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, True, Handle);
        IR.Top := TR.Top + ts.cy + CaptionGap;
      end
      else
        IR.Top := IR.Top + ((IR.Bottom - IR.Top - ih) div 2);

      IR.Left := IR.Left + ((IR.Right - IR.Left) - iw) div 2;
      if (SelectorType = stImage) then
      begin
        if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
        begin
          aCanvas.Draw(IR.Left, IR.Top, Items[Index].Image.Graphic);
        end
        else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
        begin
          Images.Draw(aCanvas, IR.Left, IR.Top, Items[Index].ImageIndex, Items[Index].Enabled);
        end;
      end
      else // SelectorType = stColor
      begin
        IR.Right := IR.Left + iw;
        IR.Bottom := IR.Top + ih;

        aCanvas.Pen.Color := clBlack;   // border color
        aCanvas.Brush.Color := Items[Index].Color;
        aCanvas.Rectangle(IR);
      end;
    end;
    ilCaptionBottom:
    begin
      if (Items[Index].Caption = '') then
        IR.Top := IR.Top + ((IR.Bottom - IR.Top - ih) div 2);

      IR.Left := IR.Left + ((IR.Right - IR.Left) - iw) div 2;
      if (SelectorType = stImage) then
      begin
        if Assigned(Items[Index].Image.Graphic) and not Items[Index].Image.Graphic.Empty then
        begin
          aCanvas.Draw(IR.Left, IR.Top, Items[Index].Image.Graphic);
          TR.Top := IR.Top + Items[Index].Image.Graphic.Width + CaptionGap;
        end
        else if Assigned(Images) and (Items[Index].ImageIndex >= 0) then
        begin
          Images.Draw(aCanvas, IR.Left, IR.Top, Items[Index].ImageIndex, Items[Index].Enabled);
          TR.Top := IR.Top + Images.Width + CaptionGap;
        end;
      end
      else // SelectorType = stColor
      begin
        IR.Right := IR.Left + iw;
        IR.Bottom := IR.Top + ih;

        aCanvas.Pen.Color := clBlack;   // border color
        aCanvas.Brush.Color := Items[Index].Color;
        aCanvas.Rectangle(IR);
      end;

      if (Items[Index].Caption <> '') then
      begin
        DrawHTMLEX(aCanvas, Items[Index].Caption, TR, Images, True, Handle);
      end;
    end;
    
  end;  // end case

end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.UpdateRectAndSize;
var
  h, w, i, x, y, c: Integer;
  sz: TSize;
  R: TRect;
begin
  if (Items.Count = 0) then
  begin
    Height := 50;
    Width := 100;
    Exit;
  end;

  FItemHot := -1;
  
  sz := GetItemSize;

  R := GetInnerRect; // consider left/top only, Height and width will be set here
  X := R.Left;
  Y := R.Top;
  c := 0;
  w := 0;
  for i := 0 to Items.Count -1 do
  begin
    Items[i].Rect := Rect(X, Y, X + sz.cx, Y + sz.cy);
    Inc(c);
    if (i < Items.Count - 1) then // not last item
    begin
      if (c >= Columns) then
      begin
        w := max(w, X);
        X := R.Left;
        Y := Y + sz.cy;
        c := 0;
      end
      else
      begin
        X := X + sz.cx;
      end;
    end;
  end;

  w := max(w, X);
  
  h := Y + sz.cy + R.Top;
  w := w + sz.cx + R.Left;

  Height := h;
  Width := w;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemSize: TSize;
var
  i, ih, iw, th, tw: Integer;
  ts: TSize;
  R: TRect;
begin
  if Assigned(AdvItemSelector.OnItemSize) then
  begin
    AdvItemSelector.OnItemSize(Self, Result);
    Exit;
  end;

  Result.cx := 0;
  Result.cy := 0;
  if (SelectorType = stColor) then
  begin
    ih := ColorBoxHeight;
    iw := ColorBoxWidth;
  end
  else
  begin
    ih := 0;  // Image/Color box height
    iw := 0;  // Image/Color box width
  end;

  th := 0;
  tw := 0;

  Canvas.Font.Assign(AdvItemSelector.ItemAppearance.Font);

  R := Rect(0, 0, 1000, 500);
  for i := 0 to Items.Count -1 do
  begin
    if (SelectorType = stImage) then
    begin
      if Assigned(Items[i].Image.Graphic) and not Items[i].Image.Graphic.Empty then
      begin
        ih := max(ih, Items[i].Image.Graphic.Height);
        iw := max(iw, Items[i].Image.Graphic.Width);
      end
      else if Assigned(Images) and (Items[i].ImageIndex >= 0) then
      begin
        ih := max(ih, Images.Height);
        iw := max(iw, Images.Width);
      end;
    end;

    ts := DrawHTMLEX(Canvas, AdvItemSelector.Items[i].Caption, R, Images, False, Self.Handle);
    tw := max(tw, ts.cx);
    th := max(th, ts.cy);
  end; // end for

  if (tw > 0) then
    tw := tw + 4;   // some extra space

  case ItemLayout of
    ilCaptionLeft, ilCaptionRight:
    begin
      Result.cx := FItemOffSetX * 2;
      if (iw > 0) then
        Result.cx := Result.cx + iw;
      if (tw > 0) then
      begin
        if (iw > 0) then
          Result.cx := Result.cx + CaptionGap;
        Result.cx := Result.cx + tw;
      end;

      Result.cy := ItemOffSetY * 2 + Max(th, ih);
    end;
    ilCaptionTop, ilCaptionBottom:
    begin
      Result.cx := FItemOffSetX * 2 + Max(tw, iw);

      Result.cy := ItemOffSetY * 2;
      if (ih > 0) then
        Result.cy := Result.cy + ih;
      if (th > 0) then
      begin
        if (ih > 0) then
          Result.cy := Result.cy + CaptionGap;
        Result.cy := Result.cy + th;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemRect(Index: Integer): TRect;
begin
  Result := Rect(-1, -1, -1, -1);
  if (Index >= 0) and (Index < Items.Count) then
    Result := Items[Index].Rect;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.InvalidateItem(Index: Integer);
var
  R: TRect;
begin
  if (Index >= 0) and (Index < Items.Count) then
  begin
    R := GetItemRect(Index);
    InvalidateRect(Handle, @R, True);

  end;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.ItemAtPos(X, Y: Integer): Integer;
var
  i: Integer;
  P: TPoint;
begin
  Result := -1;
  for i := 0 to Items.Count - 1 do
  begin
    P := Point(X, Y);
    if PtInRect(Items[i].Rect, P) then
    begin
      Result := i;
      Break;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.DrawItems(ACanvas: TCanvas);
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    DrawItem(i, ACanvas);
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetInnerRect: TRect;
begin
  Result := ClientRect;
  InflateRect(Result, -FOffsetX, -FOffsetY);
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SetItemOffSetX(const Value: Integer);
begin
  if (FItemOffSetX <> Value) then
  begin
    FItemOffSetX := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SetItemOffSetY(const Value: Integer);
begin
  if (FItemOffSetY <> Value) then
  begin
    FItemOffSetY := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SelectFirst;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := 0;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SelectLast;
begin
  if (Items.Count <= 0) then
    Exit;

  ItemIndex := Items.Count - 1;  
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SelectNext;
begin
  if (Items.Count <= 0) then
    Exit;
    
  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex + 1) < Items.Count) then
      ItemIndex := ItemIndex + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SelectPrevious;
begin
  if (Items.Count <= 0) then
    Exit;

  if (FItemIndex < 0) then
    ItemIndex := 0
  else
  begin
    if ((ItemIndex - 1) >= 0) then
      ItemIndex := ItemIndex - 1;
  end;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.LookupItem(s: string): Boolean;
var
  I: Integer;
  Idx: integer;
begin
  Result := False;
  if (Items.Count <= 0) then
    Exit;

  idx := Max(0,ItemIndex);

  for I := idx to Items.Count - 1 do
  begin
    if (pos(s, upstr(Trim(HTMLStrip(Items.Items[i].Caption)), False)) = 1) and (I <> ItemIndex) then
    begin
      ItemIndex := I;
      Result := True;
      Break;
    end;
  end;

  // not found a 2nd instance, restart from zero
  if not Result and (idx > 0) then
  begin
    for I := 0 to Items.Count - 1 do
    begin
      if (pos(s, upstr(Trim(HTMLStrip(Items.Items[i].Caption)), False)) = 1) then
      begin
        ItemIndex := I;
        Result := True;
        Break;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.HotNext;
begin
  if (Items.Count <= 0) then
    Exit;

  if (ItemHot < 0) then
    ItemHot := 0
  else
  begin
    if ((ItemHot + 1) < Items.Count) then
      ItemHot := ItemHot + 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.HotPrevious;
begin
  if (Items.Count <= 0) then
    Exit;

  if (ItemHot < 0) then
    ItemHot := 0
  else
  begin
    if ((ItemHot - 1) >= 0) then
      ItemHot := ItemHot - 1;
  end;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.SetCaptionGap(const Value: Integer);
begin
  if (FCaptionGap <> Value) then
  begin
    FCaptionGap := Value;
    UpdateRectAndSize;
  end;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetColumns: Integer;
begin
  Result := FAdvItemSelector.Columns;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItems: TSelectorItems;
begin
  Result := FAdvItemSelector.Items;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetSelectorType: TSelectorType;
begin
  Result := FAdvItemSelector.SelectorType;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemLayout: TItemLayout;
begin
  Result := FAdvItemSelector.ItemLayout;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetImages: TCustomImageList;
begin
  Result := FAdvItemSelector.Images;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemColorStyle: TSelectionColorStyle;
begin
  Result := FAdvItemSelector.ItemAppearance.ColorStyle;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemColorHot: TColor;
begin
  Result := FAdvItemSelector.ItemAppearance.ColorHot;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemColorHotTo: TColor;
begin
  Result := FAdvItemSelector.ItemAppearance.ColorHotTo;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemColorSelected: TColor;
begin
  Result := FAdvItemSelector.ItemAppearance.ColorSelected;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetItemColorSelectedTo: TColor;
begin
  Result := FAdvItemSelector.ItemAppearance.ColorSelectedTo;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetColorBoxHeight: Integer;
begin
  Result := FAdvItemSelector.ColorBoxHeight;
end;

//------------------------------------------------------------------------------

function TCustomItemSelector.GetColorBoxWidth: Integer;
begin
  Result := FAdvItemSelector.ColorBoxWidth;
end;

//------------------------------------------------------------------------------

procedure TCustomItemSelector.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if (ItemHot >= 0) then
    ItemHot := -1;
end;

//------------------------------------------------------------------------------

{ TAdvCustomItemSelector }

constructor TAdvCustomItemSelector.Create(AOwner: TComponent);
begin

  inherited;
  FItemSelectorPanel := nil;
  FItems := TSelectorItems.Create(Self);
  FItems.OnChange := OnItemChanged;
  FItemLayout := ilCaptionRight;
  FItemAppearance := TItemAppearance.Create(Self);
  FItemAppearance.OnChange := OnItemAppearanceChanged;
  FColumns := 1;
  FColorBoxWidth := DD_COLORBOXHEIGHT;
  FColorBoxHeight := DD_COLORBOXWIDTH;
  FItemAppearance.ColorStyle := scOffice2007;

  AutoScroll := True;
  AutoSize := False;

  CreateSelectorPanel;
  UpdateSelectorPanel;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomItemSelector.Destroy;
begin
  FItems.Free;
  FItemAppearance.Free;
  inherited;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.DoMouseWheelDown(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.DoMouseWheelUp(Shift: TShiftState;
  MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.WndProc(var Message: tMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (Operation = opRemove) then
  begin
    if (AComponent = Images) then
      Images := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetItems(const Value: TSelectorItems);
begin
  FItems.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.CreateSelectorPanel;
begin
  if not Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel := TCustomItemSelector.Create(Self);
    FItemSelectorPanel.Parent := Self;
    FItemSelectorPanel.OnItemSelect := OnPanelItemSelect;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.UpdateSelectorPanel;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.UpdateRectAndSize;
  end;  
end;

procedure TAdvCustomItemSelector.WMGetDlgCode(var Message: TMessage);
begin
  Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.WMVScroll(var Message: TWMVScroll);
begin
  inherited;
  if (Message.ScrollCode = SB_THUMBTRACK) then
  begin
    VertScrollBar.Position := Message.Pos;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetSelectorType(const Value: TSelectorType);
begin
  if (FSelectorType <> Value) then
  begin
    FSelectorType := Value;
    UpdateSelectorPanel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetItemLayout(const Value: TItemLayout);
begin
  if (FItemLayout <> Value) then
  begin
    FItemLayout := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.OnItemChanged(Sender: TObject);
begin
  UpdateSelectorPanel;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetColumns(const Value: Integer);
begin
  if (FColumns <> Value) and (Value > 0) then
  begin
    FColumns := Value;
    UpdateSelectorPanel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.ScrollItemInView(Index: Integer);
var
  R: TRect;
begin
  if Assigned(FItemSelectorPanel) and (Index >= 0) and (Index < Items.Count) and VertScrollBar.IsScrollBarVisible then
  begin
    R := FItemSelectorPanel.GetItemRect(Index);
    Dec(R.Top, VertScrollBar.Margin);
    Inc(R.Bottom, VertScrollBar.Margin);
    R.TopLeft := ScreenToClient(FItemSelectorPanel.ClientToScreen(R.TopLeft));
    R.BottomRight := ScreenToClient(FItemSelectorPanel.ClientToScreen(R.BottomRight));
    if R.Top < 0 then
      with VertScrollBar do Position := Position + R.Top
    else if R.Bottom > ClientHeight then
    begin
      if R.Bottom - R.Top > ClientHeight then
        R.Bottom := R.Top + ClientHeight;
      with VertScrollBar do Position := Position + R.Bottom - ClientHeight;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SelectFirst;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.SelectFirst;
    ScrollItemInView(FItemSelectorPanel.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SelectLast;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.SelectLast;
    ScrollItemInView(FItemSelectorPanel.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SelectNext;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.SelectNext;
    ScrollItemInView(FItemSelectorPanel.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SelectPrevious;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.SelectPrevious;
    ScrollItemInView(FItemSelectorPanel.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.LookupItem(s: string): Boolean;
begin
  if Assigned(FItemSelectorPanel) then
    Result := FItemSelectorPanel.LookupItem(s)
  else
    Result := False;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.HotNext;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.HotNext;
    ScrollItemInView(FItemSelectorPanel.ItemHot);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.HotPrevious;
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.HotPrevious;
    ScrollItemInView(FItemSelectorPanel.ItemHot);
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.GetVisibleItemCount: Integer;
var
  ih: Integer;
  h: Integer;
begin
  Result := 0;
  if Assigned(FItemSelectorPanel) and (Items.Count > 0) then
  begin
    ih := Items.Items[0].Rect.Bottom - Items.Items[0].Rect.Top;
    h := Height;
    if HorzScrollBar.IsScrollBarVisible then
      h := h - 20;
    Result := (h div ih) * Columns; 
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetColorBoxHeight(const Value: Integer);
begin
  if (FColorBoxHeight <> Value) then
  begin
    FColorBoxHeight := Value;
    UpdateSelectorPanel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetColorBoxWidth(const Value: Integer);
begin
  if (FColorBoxWidth <> Value) then
  begin
    FColorBoxWidth := Value;
    UpdateSelectorPanel;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetImages(const Value: TCustomImageList);
begin
  if (FImages <> Value) then
  begin
    FImages := Value;
    UpdateSelectorPanel;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.GetItemHot: Integer;
begin
  Result := -1;
  if Assigned(FItemSelectorPanel) then
    Result := FItemSelectorPanel.ItemHot;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.GetItemIndex: Integer;
begin
  Result := -1;
  if Assigned(FItemSelectorPanel) then
    Result := FItemSelectorPanel.ItemIndex;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetItemIndex(const Value: Integer);
begin
  if Assigned(FItemSelectorPanel) then
  begin
    FItemSelectorPanel.ItemIndex := Value;
    ScrollItemInView(FItemSelectorPanel.ItemIndex);
  end;
end;

//------------------------------------------------------------------------------

function TAdvCustomItemSelector.GetItemPanelSize: TSize;
begin
  Result.cx := 100;
  Result.cy := 50;
  if Assigned(FItemSelectorPanel) then
  begin
    Result.cx := FItemSelectorPanel.Width;
    Result.cy := FItemSelectorPanel.Height;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.OnPanelItemSelect(SEnder: TObject);
begin
  if Assigned(FOnItemSelect) then
    FOnItemSelect(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.SetItemAppearance(
  const Value: TItemAppearance);
begin
  FItemAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomItemSelector.OnItemAppearanceChanged(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

{ TItemAppearance }

constructor TItemAppearance.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FFont := TFont.Create;
  FFont.Name := 'Tahoma';
  ColorStyle := scOffice2007;
  FColorSelectedText := clBlack;
  FColorHotText := clBlack;
  FBorderColorHot := clNone;
  FBorderColorSelected := clNone;
end;

//------------------------------------------------------------------------------

destructor TItemAppearance.Destroy;
begin
  FFont.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TItemAppearance.Assign(Source: TPersistent);
begin
  if (Source is TItemAppearance) then
  begin
    FColorMirrorHotTo := (Source as TItemAppearance).FColorMirrorHotTo;
    FBorderColor := (Source as TItemAppearance).FBorderColor;
    FColorMirrorHot := (Source as TItemAppearance).FColorMirrorHot;
    FColorSelectedTo := (Source as TItemAppearance).FColorSelectedTo;
    FColorSelected := (Source as TItemAppearance).FColorSelected;
    FEdgeColor := (Source as TItemAppearance).FEdgeColor;
    FColorHotTo := (Source as TItemAppearance).FColorHotTo;
    FBorderColorBottom := (Source as TItemAppearance).FBorderColorBottom;
    FColorHot := (Source as TItemAppearance).FColorHot;
    FBorderColorTop := (Source as TItemAppearance).FBorderColorTop;
    FColorMirrorSelectedTo := (Source as TItemAppearance).FColorMirrorSelectedTo;
    FColorMirrorSelected := (Source as TItemAppearance).FColorMirrorSelected;
    Font.Assign((Source as TItemAppearance).Font);
    ColorStyle := (Source as TItemAppearance).FColorStyle;
    FColorSelectedText := (Source as TItemAppearance).FColorSelectedText;
    FBorderColorHot := (Source as TItemAppearance).BorderColorHot;
    FBorderColorSelected := (Source as TItemAppearance).BorderColorSelected;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TItemAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

function TItemAppearance.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

//------------------------------------------------------------------------------

procedure TItemAppearance.SetColorStyle(
  const Value: TSelectionColorStyle);
begin
  if (FColorStyle <> Value) then
  begin
    FColorStyle := Value;
    case FColorStyle of
      scOffice2007:
      begin
        ColorHot := $D7FFFD;
        ColorHotTo := $58D4FC;
        ColorMirrorHot := $58D4FC;
        ColorMirrorHotTo := $B3F1FC;
        BorderColorTop := $F4FEFF;
        BorderColorBottom := $CAF8FF;
        BorderColor := $8DD7D3;
        EdgeColor := $C3D9DF;

        ColorSelected := $00A6FFFB;
        ColorSelectedTo := $0004B9F4;
        ColorMirrorSelected := $0015C1FB;
        ColorMirrorSelectedTo := $0068E3F9;
      end;
      scWindowsVista:
      begin
        ColorHot := $FDF8F1;
        ColorHotTo := $FCEFD5;
        ColorMirrorHot := clNone;
        ColorMirrorHotTo := clNone;
        BorderColorTop := $FDFBF6;
        BorderColorBottom := $FDF5E7;
        BorderColor := $FDDE99;
        EdgeColor := $FCEDCB;

        ColorSelected := $00FAEEDE;
        ColorSelectedTo := $00FAE3B4;
        ColorMirrorSelected := clNone;
        ColorMirrorSelectedTo := clNone;
      end;
      scWindows7:
      begin
        ColorHot := $FCEBDC;
        ColorHotTo := $FCDBC1;
        ColorMirrorHot := clNone;
        ColorMirrorHotTo := clNone;
        BorderColorTop := $FDF4EB;
        BorderColorBottom := $FDEADB;
        BorderColor := $CEA27D;
        EdgeColor := $E0C5AE;

        ColorSelected := $00F9D8B9;
        ColorSelectedTo := $00FBCAA2;
        ColorMirrorSelected := clNone;
        ColorMirrorSelectedTo := clNone;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TItemAppearance.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

//------------------------------------------------------------------------------


{$IFDEF FREEWARE}
{$I TRIAL.INC}
{$ENDIF}


end.

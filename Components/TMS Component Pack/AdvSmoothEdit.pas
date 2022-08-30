{*************************************************************************}
{ TAdvSmoothEdit & TAdvSmoothMaskEdit component                                       }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 1996-2012                                         }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvSmoothEdit;

{$I TMSDEFS.INC}

interface

uses
  Windows, Dialogs, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  StdCtrls, Mask, AdvSmoothEdDD, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  , Character
  {$ENDIF}
  ;

const
  MAJ_VER = 2; // Major version nr.
  MIN_VER = 8; // Minor version nr.
  REL_VER = 5; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // v2.6.1.0 : added event OnLookupIndexSelect
  // v2.6.2.0 : fix for WinXP themed border drawing
  // v2.6.2.1 : fix for decimalseparator entry when multiple characters are selected
  // v2.6.2.2 : fix for OnClipboardPaste event
  // v2.7.0.0 : New FocusBorderColor property
  //          : optimized property storage in DFM file
  // v2.7.0.1 : fix for issue with DefaultHandling and return key in forms with KeyPreview
  // v2.7.0.2 : fix for transparent = true in VCL.NET
  // v2.7.0.3 : fix for transparent with parent controls without WM_ERASEBKGND
  // v2.7.0.4 : fix for TabOnFullLength for pasting
  // v2.7.0.5 : fix for handling eaRight edit & focus alignment
  // v2.7.0.6 : improved handling of ExcelStyleDecimalSeparator
  // v2.7.1.0 : improved disabled border drawing with WinXP theme
  // v2.7.2.0 : New : DisabledBorder property added
  //          : Improved : focus border drawing for TAdvSmoothMaskEdit
  // v2.7.3.0 : New : AllowNumericNullValue added
  // v2.7.3.1 : Fixed : issue with using Color & FlatParentColor = false
  // v2.7.3.2 : Fixed : issue with ESC/Enter key handling on form with KeyPreview=true
  // v2.7.3.3 : Fixed : issue with design time destroy of TAdvSmoothEdit, TAdvSmoothMaskEdit with Label
  // v2.7.3.4 : Fixed : issue with lookup case insensitive compare
  // v2.7.3.5 : Improved : painting on leaving focus
  // v2.7.4.0 : Fixed : issue with OnChange event
  //          : New : label alignemnts lpRightTop, lpRightCenter, lpRighBottom added
  //          : Improved : clipboard event handling
  // v2.7.4.1 : Fixed : issue with keypreview on frames
  // v2.7.5.0 : Improved : sign not longer taken in account for LengthLimit in etNumeric, etFloat, etMoney types
  // v2.7.5.1 : Fixed : edit rect issue with Ctl3D = false
  // v2.7.5.2 : Fixed : issue with OnChange for etMoney type
  // v2.7.5.3 : Fixed : issue with label margin
  // v2.7.5.4 : Fixed : issue with OnChange & backspace
  // v2.8.0.0 : New : PrecisionDisplay property to show floating point values in shortest possible way
  // v2.8.0.1 : Fixed : issue with PrecisionDisplay when used with edit types other than etFloat, etMoney
  // v2.8.0.2 : Improved : exposed GetTextSize function in public section
  // v2.8.1.1 : Fixed : issue with SoftBorder drawing
  // v2.8.1.2 : Fixed : issue with OnChange
  // v2.8.1.3 : Fixed : issue with ShowError / OnValueValidate & resetting the error
  // v2.8.1.4 : Fixed : issue with TAdvSmoothMaskEdit.ParentFont = true and label
  // v2.8.1.5 : Fixed : issue with FocusBorderColor <> clNone and leaving focus
  // v2.8.1.6 : Improved : behaviour with empty text and setting AllowNumericNullValue = true
  // v2.8.1.7 : Fixed : issue with disabled border drawing
  // v2.8.1.8 : Fixed : issue with AutoTab = true on TAdvSmoothMaskEdit
  // v2.8.1.9 : Fixed : issue with label font and ShowURL = true
  // v2.8.1.10: Fixed : issue with edit rect when Ctl3D = false
  // v2.8.1.11: Improved : ParentFont set to false when LabelFont is used.
  // v2.8.1.12: Fixed : issue with setting text = '' when AllowNumericNullValue = true
  // v2.8.1.13: Fixed : Modified not set for backspace key
  // v2.8.1.14: Fixed : issue with lookup for non-existing entry
  // v2.8.1.15: Improved : mixed case edit handling
  // v2.8.1.16: Improved : caret positioning when both prefix & suffix are used
  // v2.8.1.17: Fixed : border issue in TAdvSmoothMaskEdit when Flat & SoftBorder enabled
  // v2.8.1.18: Fixed : issue with display of lookup list
  // v2.8.2.0 : Improved : Setting FlatLineColor = clNone removes flat line painting
  // v2.8.2.1 : Fixed : issue with handling click on URL
  // v2.8.2.2 : Fixed : border issue with Ctl3D = false and large fonts
  // v2.8.2.3 : Fixed : issue with Paste and EditType = etAlphaNumeric
  // v2.8.3.0 : Improved : lookup settings to avoid duplicates
  // v2.8.4.0 : New : property FocusLabel added to TAdvSmoothMaskEdit
  // v2.8.4.1 : Fixed : issue with toggling Enabled property
  // v2.8.4.2 : Fixed : issue with combining ShowError and FocusColor
  //          : Fixed : issue with ParentFont and using LabelCaption
  // v2.8.5.0 : Improved : perform auto focus only when parent form is active
  // v2.8.5.1 : Fixed : issue with LabelFont and Form ScaleBy
  //            Fixed : issue with EmptyText on Windows Vista / 7
  // v2.8.5.2 : Fixed : issue with auto separators in etMoney edit type and backspace key
  // v2.8.5.3 : Fixed : issue with running in different locale from design time locale
  // v2.8.5.4 : Fixed : issue with EditAlign / FocusAlign set to different values
  // v2.8.5.5 : Improved : Empty text drawing position

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TLabelPosition = (lpLeftTop, lpLeftCenter, lpLeftBottom, lpTopLeft, lpBottomLeft,
    lpLeftTopLeft, lpLeftCenterLeft, lpLeftBottomLeft, lpTopCenter, lpBottomCenter,
    lpRightTop, lpRightCenter, lpRighBottom);

  TAdvSmoothEditType = (etString, etNumeric, etFloat, etUppercase, etMixedCase, etLowerCase,
    etPassword, etMoney, etRange, etHex, etAlphaNumeric, etValidChars);

  TAutoType = (atNumeric, atFloat, atString, atDate, atTime, atHex);

  TValueValidateEvent = procedure(Sender: TObject; value: string; var IsValid: Boolean) of object;

  TClipboardEvent = procedure(Sender: TObject; value: string; var allow: Boolean) of object;

  TMaskCompleteEvent = procedure(Sender: TObject; value: string; var accept: Boolean) of object;

  TLookupSelectEvent = procedure(Sender: TObject; var Value: string) of object;

  TLookupIndexSelectEvent = procedure(Sender: TObject; Index: Integer; var Value: string) of object;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;


  TRangeList = class(TList)
  private
    procedure SetInteger(Index: Integer; Value: Integer);
    function GetInteger(Index: Integer): Integer;
  public
    constructor Create;
    property Items[index: Integer]: Integer read GetInteger write SetInteger; default;
    procedure Add(Value: integer);
    procedure AddMultiple(Value, Count: Integer);
    procedure Delete(Index: Integer);
    procedure Show;
    function InList(Value: integer): Boolean;
    function StrToList(s: string): Boolean;
  end;

  TPersistenceLocation = (plInifile, plRegistry);

  TPersistence = class(TPersistent)
  private
    FEnable: boolean;
    FKey: string;
    FSection: string;
    FLocation: TPersistenceLocation;
  published
    property Enable: Boolean read FEnable write FEnable default False;
    property Key: string read FKey write FKey;
    property Section: string read FSection write FSection;
    property Location: TPersistenceLocation read FLocation write FLocation default plIniFile;
  end;

  TItemClickEvent = procedure(Sender: TObject; Index: Integer) of object;

  { TListHintWindow }
  TListHintWindow = class(THintWindow)
  private
    FListControl: TListBox;
    procedure WMNCButtonDown(var Message: TMessage); message WM_NCLBUTTONDOWN;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property BorderWidth;
    property Text;
  published
    property ListControl: TListBox read FListControl write FListControl;
  end;

  TLookupSettings = class(TPersistent)
  private
    FDisplayList: TStringList;
    FValueList: TStringList;
    FDisplayCount: Integer;
    FColor: TColor;
    FEnabled: Boolean;
    FNumChars: Integer;
    FCaseSensitive: Boolean;
    FHistory: Boolean;
    FMulti: Boolean;
    FSeparator: char;
    procedure SetDisplayList(const Value: TStringList);
    procedure SetValueList(const Value: TStringList);
    procedure SetNumChars(const Value: Integer);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property CaseSensitive: Boolean read FCaseSensitive write FCaseSensitive default False;
    property Color: TColor read FColor write FColor default clWindow;
    property DisplayCount: Integer read FDisplayCount write FDisplayCount default 4;
    property DisplayList: TStringList read FDisplayList write SetDisplayList;
    property Enabled: Boolean read FEnabled write FEnabled default False;
    property History: Boolean read FHistory write FHistory default False;
    property NumChars: Integer read FNumChars write SetNumChars default 2;
    property ValueList: TStringList read FValueList write SetValueList;
    property Multi: Boolean read FMulti write FMulti default False;
    property Separator: char read FSeparator write FSeparator;
  end;

  TURLClickEvent = procedure(Sender: TObject; URL: string; var Show: Boolean) of object;

  TEditAlign = (eaLeft, eaRight, eaDefault, eaCenter);

  TPrecisionDisplay = (pdNormal, pdShortest);

  TAdvSmoothEdit = class(TCustomMaskEdit)
  private
    { Private declarations }
    FAlignChanging: Boolean;
    FLabel: TLabel;
    FLabelFont: TFont;
    FLabelPosition: TLabelPosition;
    FLabelMargin: Integer;
    FLabelTransparent: Boolean;
    FAutoFocus: Boolean;
    FCanUndo: Boolean;
    FEditType: TAdvSmoothEditType;
    FEditAlign: TEditAlign;
    FOldEditAlign: TEditAlign;
    FOldBorder: TBorderStyle;
    FExcelStyleDecimalSeparator: Boolean;
    FTabOnFullLength: Boolean;
    FDisabledColor: TColor;
    FDisabledBorder: boolean;
    FNormalColor: TColor;
    FFocusColor: TColor;
    FFocusFontColor: TColor;
    FErrorColor: TColor;
    FErrorFontColor: TColor;
    FError: Boolean;
    FFocusLabel: Boolean;
    FFontColor: TColor;
    FModifiedColor: TColor;
    FReturnIsTab: Boolean;
    FShowModified: Boolean;
    FIsModified: Boolean;
    FShowURL: Boolean;
    FURLColor: TColor;
    FFocusWidthInc: Integer;
    FFocusAlign: TEditAlign;
    FLengthLimit: SmallInt;
    FPrecision: SmallInt;
    FPrecisionDisplay: TPrecisionDisplay;
    FPrefix: string;
    FSuffix: string;
    FOldString: string;
    FSigned: Boolean;
    FIsUrl: Boolean;
    FFlat: Boolean;
    FMouseInControl: Boolean;
    FFlatLineColor: TColor;
    FPersistence: TPersistence;
    FOnValueValidate: TValueValidateEvent;
    FOnClipboardCut: TClipboardEvent;
    FOnClipboardPaste: TClipboardEvent;
    FOnClipboardCopy: TClipboardEvent;
    FBlockCopy: boolean;
    FFlatParentColor: Boolean;
    FTransparent: Boolean;
    FCaretPos: TPoint;
    FOleDropSource: Boolean;
    FOleDropTarget: Boolean;
    FOleDropTargetAssigned: Boolean;
    FIsDragSource: Boolean;
    FButtonDown: Boolean;
    FFocusBorder: Boolean;
    FFocusBorderColor: TColor;
    FHintShowLargeText: Boolean;
    FShowError: Boolean;
    FAutoThousandSeparator: Boolean;
    FEmptyText: string;
    FSoftBorder: Boolean;
    FDefaultHandling: Boolean;
    FBlockDefaultHandling: Boolean;
    FLabelAlwaysEnabled: Boolean;
    FBorder3D: Boolean;
    FErrorMarkerLen: Integer;
    FErrorMarkerPos: Integer;
    FIndentR: Integer;
    FIndentL: Integer;
    FLoadedHeight: Integer;
    FLookupList: TListHintWindow;
    FLookupListBox: TListbox;
    FLookup: TLookupSettings;
    FOnLookupSelect: TLookupSelectEvent;
    FOnLookupIndexSelect: TLookupIndexSelectEvent;
    FIsValidating: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FOnMouseLeave: TNotifyEvent;
    FOnURLClick: TURLClickEvent;
    FOnLabelClick: TNotifyEvent;
    FOnLabelDblClick: TNotifyEvent;
    FValidChars: string;
    FIsWinXP: Boolean;
    FIsThemed: Boolean;
    FBlockChange: Boolean;
    FAllowNumericNullValue: Boolean;
    FParentFnt: boolean;
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure WMActivate(var Message: TMessage); message WM_ACTIVATE;
    procedure CNCtlColorEdit(var Message: TWMCtlColorEdit); message CN_CTLCOLOREDIT;
    procedure CNCtlColorStatic(var Message: TWMCtlColorStatic); message CN_CTLCOLORSTATIC;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMCancelMode(var Message: TMessage); message CM_CANCELMODE;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure WMPaste(var Msg: TMessage); message WM_PASTE;
    procedure WMCut(var Message: TWMCut); message WM_CUT;
    procedure WMCopy(var Message: TWMCopy); message WM_COPY;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    procedure WMLButtonUp(var Msg: TWMMouse); message WM_LBUTTONUP;
    procedure WMLButtonDown(var Msg: TWMMouse); message WM_LBUTTONDOWN;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMDestroy(var Msg: TMessage); message wm_Destroy; //***
    procedure WMMouseMove(var Msg: TWMMouse); message WM_MOUSEMOVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure SetEditType(value: TAdvSmoothEditType);
    function GetText: string;
    procedure SetText(value: string);
    function GetFloat: double;
    function GetInt: integer;
    function FixedLength(s: string): Integer;
    function AllowMin(ch: char): boolean;
    function DecimalPos: Integer;
    procedure SetFloat(const Value: double);
    procedure SetInt(const Value: integer);
    procedure SetPrefix(const Value: string);
    procedure SetSuffix(const Value: string);
    procedure SetLabelCaption(const value: string);
    function GetLabelCaption: string;
    procedure SetLabelPosition(const value: TLabelPosition);
    procedure SetLabelMargin(const value: integer);
    procedure SetLabelTransparent(const value: boolean);
    procedure SetFlat(const value: boolean);
    procedure SetFlatRect(const Value: Boolean);
    procedure SetPrecision(const Value: SmallInt);
    procedure SetPrecisionDisplay(const Value: TPrecisionDisplay);
    function EStrToFloat(s: string): extended;
    procedure UpdateLabel;
    procedure AutoSeparators;
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    function GetVisible: boolean;
    procedure SetVisible(const Value: boolean);
    procedure PaintEdit;
    procedure DrawControlBorder(DC: HDC);
    procedure DrawBorder;
    function Is3DBorderButton: Boolean;
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledBorder(const Value: boolean);
    function GetEnabledEx: boolean;
    procedure SetEnabledEx(const Value: boolean);
    procedure SetEditAlign(const Value: TEditAlign);
    procedure SetCanUndo(const Value: boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    procedure SetTransparent(const Value: boolean);
    procedure SetFlatLineColor(const Value: TColor);
    procedure SetFlatParentColor(const Value: boolean);
    procedure LabelFontChange(Sender: TObject);
    procedure SetLabelFont(const Value: TFont);
    function GetError: Boolean;
    procedure SetError(const Value: Boolean);
    procedure ApplyURL(const Value: Boolean);
    procedure DrawErrorLines(Canvas: TCanvas; ErrPos, ErrLen: Integer);
    procedure SetOleDropSource(const Value: boolean);
    procedure SetOleDropTarget(const Value: boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure DoneLookup;
    function GetHeightEx: Integer;
    procedure ListKeyPress(Sender: TObject; var Key: Char);
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure SetAutoThousandSeparator(const Value: Boolean);
    procedure SetBorder3D(const Value: Boolean);
    procedure SetEmptyText(const Value: string);
    procedure SetErrorMarkerLen(const Value: Integer);
    procedure SetErrorMarkerPos(const Value: Integer);
    procedure SetFocusBorder(const Value: Boolean);
    procedure SetHeightEx(const Value: Integer);
    procedure SetLabelAlwaysEnabled(const Value: Boolean);
    procedure SetSoftBorder(const Value: Boolean);
    procedure UpdateLookup;
  protected
    function IsDropDownVisible: Boolean; virtual;
    function GetVersionNr: Integer; virtual;
    { Protected declarations }
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CreateLabel: TLabel;
    procedure Loaded; override;
    procedure InvalidateCaret(pt: tpoint);
    procedure EraseCaret;
    procedure DrawCaretByCursor;
    procedure SetCaretByCursor;
    property IndentR: Integer read FIndentR write FIndentR;
    property IndentL: Integer read FIndentL write FIndentL;
    function DoValidate(value: string): Boolean; virtual;
    procedure ValidateEvent(Value: string; var IsValid: Boolean); virtual;
    procedure LabelClick(Sender: TObject);
    procedure LabelDblClick(Sender: TObject);
    procedure SetParent(AParent: TWinControl); override;
    procedure SetTextDirect(s:string);
    function TestURL: Boolean; virtual;
    property BlockDefaultHandling: boolean read FBlockDefaultHandling write FBlockDefaultHandling;
  public
    { Public declarations }
    constructor Create(aOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SelectAll;
    procedure SelectBeforeDecimal;
    procedure SelectAfterDecimal;
    procedure Init;
    function GetTextSize: integer;
    function CharFromPos(pt: TPoint): Integer;
    function PosFromChar(uChar: word): TPoint;
    property FloatValue: double read GetFloat write SetFloat;
    property IntValue: Integer read GetInt write SetInt;
    property Modified: Boolean read GetModified write SetModified;
    property IsError: Boolean read GetError write SetError;
    function RangeStrToList(rangelist: TRangeList): Boolean;
    procedure ListToRangeStr(rangelist: TRangeList);
    procedure LoadPersist; virtual;
    procedure SavePersist; virtual;
    property DefaultHandling: Boolean read FDefaultHandling write FDefaultHandling;

    property EditLabel: TLabel read FLabel;
    property Border3D: Boolean read FBorder3D write SetBorder3D;
  published
    { Published declarations }
    property OnValueValidate: TValueValidateEvent read FOnValueValidate write FOnValueValidate;
    property OnClipboardCopy: TClipboardEvent read FOnClipboardCopy write FOnClipboardCopy;
    property OnClipboardCut: TClipboardEvent read FOnClipboardCut write FOnClipboardCut;
    property OnClipboardPaste: TClipboardEvent read FOnClipboardPaste write FOnClipboardPaste;
    property AllowNumericNullValue: Boolean read FAllowNumericNullValue write FAllowNumericNullValue default False;
    property AutoFocus: boolean read FAutoFocus write FAutoFocus default False;
    property AutoThousandSeparator: Boolean read FAutoThousandSeparator write SetAutoThousandSeparator default True;
    property EditAlign: TEditAlign read FEditAlign write SetEditAlign default eaLeft;
    property EditType: TAdvSmoothEditType read FEditType write SetEditType default etString;
    property EmptyText: string read FEmptyText write SetEmptyText;
    property ErrorMarkerPos: Integer read FErrorMarkerPos write SetErrorMarkerPos default 0;
    property ErrorMarkerLen: Integer read FErrorMarkerLen write SetErrorMarkerLen default 0;
    property ErrorColor: TColor read FErrorColor write FErrorColor default clRed;
    property ErrorFontColor: TColor read FErrorFontColor write FErrorFontColor default clWhite;
    property ExcelStyleDecimalSeparator: boolean read fExcelStyleDecimalSeparator write fExcelStyleDecimalSeparator default False;
    property Flat: boolean read FFlat write SetFlat default False;
    property FlatLineColor: TColor read fFlatLineColor write SetFlatLineColor default clBlack;
    property FlatParentColor: boolean read fFlatParentColor write SetFlatParentColor default true;
    property FocusAlign: TEditAlign read FFocusAlign write FFocusAlign default eaDefault;
    property FocusBorder: Boolean read FFocusBorder write SetFocusBorder default False;
    property FocusBorderColor: TColor read FFocusBorderColor write FFocusBorderColor default clNone;
    property FocusColor: TColor read FFocusColor write FFocusColor default clNone;
    property FocusFontColor: TColor read FFocusFontColor write FFocusFontColor default clWindowText;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property FocusWidthInc: Integer read FFocusWidthInc write FFocusWidthInc default 0;
    property Height: Integer read GetHeightEx write SetHeightEx;
    property ModifiedColor: TColor read FModifiedColor write FModifiedColor default clHighlight;
    property DisabledBorder: Boolean read FDisabledBorder write SetDisabledBorder default true;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property ShowError: Boolean read FShowError write FShowError default False;
    property ShowModified: Boolean read FShowModified write FShowModified default False;
    property ShowURL: Boolean read FShowURL write FShowURL default False;
    property SoftBorder: Boolean read FSoftBorder write SetSoftBorder default False;
    property URLColor: TColor read FURLColor write FURLColor default clBlue;
    property ReturnIsTab: Boolean read fReturnIsTab write FReturnIsTab default False;
    property LengthLimit: smallint read fLengthLimit write FLengthLimit default 0;
    property TabOnFullLength: boolean read fTabOnFullLength write FTabOnFullLength default False;
    property Precision: smallint read FPrecision write SetPrecision default 0;
    property PrecisionDisplay: TPrecisionDisplay read FPrecisionDisplay write SetPrecisionDisplay default pdNormal;
    property Prefix: string read FPrefix write SetPrefix;
    property Suffix: string read FSuffix write SetSuffix;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition default lpLeftTop;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin default 4;
    property LabelTransparent: Boolean read FLabelTransparent write SetLabelTransparent default False;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write SetLabelAlwaysEnabled default False;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property Lookup: TLookupSettings read FLookup write FLookup;
    property Persistence: TPersistence read FPersistence write FPersistence;

{$IFDEF DELPHI6_LVL}
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
{$ENDIF}
    property Align;
    property Anchors;
    property BiDiMode;
    property ParentBiDiMode;
    property Constraints;
    property DragKind;
    property OnEndDock;
    property OnStartDock;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CanUndo: boolean read FCanUndo write SetCanUndo default True;
    property Color: TColor read GetColorEx write SetColorEx;
    property Ctl3D;
    property DragCursor;
    property DragMode;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    property Font;
    property HideSelection;
    property Hint;
    property HintShowLargeText: boolean read FHintShowLargeText write FHintShowLargeText default False;
    property ImeMode;
    property ImeName;
    property MaxLength;
    property OEMConvert;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PasswordChar;
    property OleDropTarget: Boolean read fOleDropTarget write SetOleDropTarget default False;
    property OleDropSource: Boolean read fOleDropSource write SetOleDropSource default False;
{$IFDEF WIN32}
    property PopupMenu;
{$ENDIF}
    property ReadOnly;
    property ShowHint;
    property Signed: Boolean read FSigned write FSigned default False;
    property TabOrder;
    property TabStop;
    property Text: string read GetText write SetText;
    property Transparent: boolean read FTransparent write SetTransparent default False;
    property ValidChars: string read FValidChars write FValidChars;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnChange;
    property OnClick;
    property OnDblClick;
{$IFDEF WIN32}
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnStartDrag;
{$ENDIF}
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnMouseUp;
    property OnContextPopup;
    property OnLookupSelect: TLookupSelectEvent read FOnLookupSelect write FOnLookupSelect;
    property OnLookupIndexSelect: TLookupIndexSelectEvent read FOnLookupIndexSelect write FOnLookupIndexSelect;
    property OnLabelClick: TNotifyEvent read FOnLabelClick write FOnLabelClick;
    property OnLabelDblClick: TNotifyEvent read FOnLabelDblClick write FOnLabelDblClick;
    property OnURLClick: TURLClickEvent read FOnURLClick write FOnURLClick;
    property Version: string read GetVersion write SetVersion;
  end;

  TAdvSmoothMaskEdit = class(TMaskEdit)
  private
    { Private declarations }
    FLabel: TLabel;
    FAutoFocus: boolean;
    FAutoTab: Boolean;
    FReturnIsTab: Boolean;
    FAlignment: TAlignment;
    FFocusColor: TColor;
    FFocusFontColor: TColor;
    FNormalColor: TColor;
    FLoadedColor: TColor;
    FFontColor: TColor;
    FModifiedColor: tcolor;
    FShowModified: Boolean;
    FLabelMargin: integer;
    FLabelPosition: TLabelPosition;
    FLabelTransparent: boolean;
    FSelectFirstChar: boolean;
    FFlat: boolean;
    FOnMaskComplete: TMaskCompleteEvent;
    FDisabledColor: TColor;
    FDisabledBorder: Boolean;
    FOriginalValue: string;
    FCanUndo: Boolean;
    FLabelFont: TFont;
    FLabelAlwaysEnabled: Boolean;
    FFlatLineColor: TColor;
    FSoftBorder: Boolean;
    FFocusBorder: Boolean;
    FFocusBorderColor: TColor;
    FMouseInControl: Boolean;
    FBorder3D: Boolean;
    FFlatParentColor: Boolean;
    FOldBorder: TBorderStyle;
    FIsThemed: Boolean;
    FFocusLabel: Boolean;
    procedure SetAlignment(value: TAlignment);
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure WMSetFocus(var Msg: TWMSetFocus); message WM_SETFOCUS;
    procedure WMKillFocus(var Msg: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMChar(var Msg: TWMKey); message WM_CHAR;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure WMPaint(var Msg: TWMPaint); message WM_PAINT;
    procedure WMNCPaint(var Message: TMessage); message WM_NCPAINT;
    procedure CMParentFontChanged(var Message: TMessage); message CM_PARENTFONTCHANGED;
    procedure WMKeyDown(var Msg: TWMKeydown); message WM_KEYDOWN;
    function GetLabelCaption: string;
    procedure SetLabelCaption(const Value: string);
    procedure SetLabelMargin(const Value: integer);
    procedure SetLabelPosition(const Value: TLabelPosition);
    procedure UpdateLabel;
    procedure SetFlat(const Value: boolean);
    function GetModified: boolean;
    procedure SetModified(const Value: boolean);
    procedure SetLabelTransparent(const Value: boolean);
    procedure SetDisabledColor(const Value: TColor);
    procedure SetDisabledBorder(const Value: boolean);
    function GetEnabledEx: Boolean;
    procedure SetEnabledEx(const Value: Boolean);
    function GetColorEx: TColor;
    procedure SetColorEx(const Value: TColor);
    procedure SetLabelFont(const Value: TFont);
    procedure LabelFontChanged(Sender: TObject);
    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    procedure SetFlatLineColor(const Value: TColor);
    procedure PaintEdit;
    procedure SetSoftBorder(const Value: Boolean);
    procedure DrawBorder;
    procedure DrawControlBorder(DC: HDC);
    function Is3DBorderButton: Boolean;
    procedure SetBorder3D(const Value: Boolean);
    procedure SetFlatRect(const Value: Boolean);
    procedure SetFlatParentColor(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAutoFocus(const Value: boolean);
  protected
    { Protected declarations }
    function GetVersionNr: Integer; virtual;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    function CreateLabel: TLabel;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    destructor Destroy; override;
    property Modified: Boolean read GetModified write SetModified;
    procedure Loaded; override;

    property EditLabel: TLabel read FLabel;
    property Border3D: Boolean read FBorder3D write SetBorder3D;
  published
    { Published declarations }
    property AutoFocus: Boolean read fAutoFocus write SetAutoFocus;
    property AutoTab: Boolean read FAutoTab write FAutoTab default true;
    property CanUndo: Boolean read FCanUndo write FCanUndo default false;
    property Color: TColor read GetColorEx write SetColorEx;
    property DisabledBorder: Boolean read FDisabledBorder write SetDisabledBorder default true;
    property DisabledColor: TColor read FDisabledColor write SetDisabledColor default clSilver;
    property Enabled: Boolean read GetEnabledEx write SetEnabledEx;
    property Flat: Boolean read FFlat write SetFlat;
    property FlatLineColor: TColor read FFlatLineColor write SetFlatLineColor;
    property FlatParentColor: Boolean read FFlatParentColor write SetFlatParentColor;
    property ShowModified: boolean read FShowModified write fShowModified;
    property FocusBorderColor: TColor read  FFocusBorderColor write FFocusBorderColor default clNone;    
    property FocusColor: TColor read FFocusColor write FFocusColor;
    property FocusBorder: Boolean read FFocusBorder write FFocusBorder;
    property FocusFontColor: TColor read FFocusFontColor write fFocusFontColor;
    property FocusLabel: Boolean read FFocusLabel write FFocusLabel default false;
    property LabelCaption: string read GetLabelCaption write SetLabelCaption;
    property LabelAlwaysEnabled: Boolean read FLabelAlwaysEnabled write FLabelAlwaysEnabled;
    property LabelPosition: TLabelPosition read FLabelPosition write SetLabelPosition;
    property LabelMargin: Integer read FLabelMargin write SetLabelMargin;
    property LabelTransparent: boolean read FLabelTransparent write SetLabelTransparent;
    property LabelFont: TFont read FLabelFont write SetLabelFont;
    property ModifiedColor: tcolor read FModifiedColor write fModifiedColor;
    property ReturnIsTab: Boolean read FReturnIsTab write fReturnIsTab default True;
    property SoftBorder: Boolean read FSoftBorder write SetSoftBorder default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property SelectFirstChar: boolean read fSelectFirstChar write fSelectFirstChar;
    property Visible: Boolean read GetVisible write SetVisible;
    property OnMaskComplete: TMaskCompleteEvent read fOnMaskComplete write fOnMaskComplete;
    property Version: string read GetVersion write SetVersion;
  end;

  PQueryParams = ^TQueryParams;
  TQueryParams = record
    Precision: Integer;
    Flat: Boolean;
    Lengthlimit: Integer;
    Prefix: string;
    Suffix: string;
  end;

  TEditDropTarget = class(TAEDropTarget)
  private
    FAdvSmoothEdit: TAdvSmoothEdit;
  public
    constructor Create(aEdit: TAdvSmoothEdit);
    procedure DropText(pt: tpoint; s: string); override;
    procedure DragMouseMove(pt: tpoint; var allow: Boolean); override;
  end;


const
  BorderRec: array[TBorderStyle] of Integer = (1, -1);

var
  AdvSmoothEdit_DEFAULTHANDLING : boolean = True;

function AdvInputQuery(const QueryType: tAdvSmoothEditType; QueryParams: PQueryParams; const ACaption, APrompt: string;
  var Value: string): Boolean;

implementation

{$IFNDEF VER80}
uses

  Shellapi, ActiveX, Consts, Inifiles, Registry, Clipbrd;
{$ENDIF}

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
      if (pvs^.dwSignature=$FEEF04BD) then
      begin
        Result := pvs^.dwFileVersionMS;
      end;
    end;
    FreeMem(buf);
  end;
end;
{$ENDIF}

function IsVista: boolean;
var
  hKernel32: HMODULE;
begin
  hKernel32 := GetModuleHandle('kernel32');
  if (hKernel32 > 0) then
  begin
    Result := GetProcAddress(hKernel32, 'GetLocaleInfoEx') <> nil;
  end
  else
    Result := false;
end;

function RangeListCompare(Item1, Item2: Pointer): Integer;
begin
  if integer(Item1) > integer(Item2) then Result := 1 else
    if integer(Item1) = integer(Item2) then Result := 0 else Result := -1;
end;

function CheckTerminator(ch: char): boolean;
const
  Terminators = [' ', ',', '.', '/', '\', ':', '*', '$', '-', '(', ')', '[', ']', '"','''','&'];
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in Terminators;
  {$ENDIF}
  {$IFDEF DELPHI_UNICODE}
  Result := (ch = ' ') or (ch = ',') or (ch = '.') or (ch = '-') or (ch = '''') or
    (ch = '\') or (ch = '/') or (ch = ':') or (ch = '*') or (ch = '$') or (ch = ')') or (ch = ')') or (ch = '"');
  {$ENDIF}
end;

function ShiftCase(Name: string): string;

  {$IFNDEF DELPHI_UNICODE}
  function LowCase(C: char): char;
  begin
    if C in ['A'..'Z'] then
      LowCase := Chr(Ord(C) - Ord('A') + Ord('a'))
    else
      Lowcase := C;
  end;
  {$ENDIF}


var
  I, L: Integer;
  NewName: string;
  First: Boolean;
begin
  First := true;
  NewName := Name;
  L := Length(Name);
  for I := 1 to L do
  begin
    if CheckTerminator(NewName[I]) then
      First := true
    else
      if First then
      begin
        {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Upcase(Name[I]);
        {$ENDIF}
        {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToUpper;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := character.ToUpper(Name[I]);
        {$ENDIF}
        {$ENDIF}
        First := false;
      end
      else
      begin
        {$IFNDEF DELPHI_UNICODE}
        NewName[I] := Lowcase(Name[I]);
        {$ENDIF}
        {$IFDEF DELPHI_UNICODE}
        {$IFDEF DELPHIXE4_LVL}
        NewName[I] := Name[I].ToLower;
        {$ENDIF}
        {$IFNDEF DELPHIXE4_LVL}
        NewName[I] := character.ToLower(Name[I]);
        {$ENDIF}
        {$ENDIF}
      end;

    if (Copy(NewName, 1, I) = 'Mc') or (Copy(NewName, 1, I) = 'Mac') or
      ((Pos(' Mc', NewName) = I - 2) and (I > 2)) or
      ((I > L - 3) and ((Copy(NewName, I - 1, 2) = ' I') or
      (Copy(NewName, I - 2, 3) = ' II'))) then
      First := true;
  end;
  ShiftCase := NewName;
end;

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

function StripThousandSep(s: string): string;
begin
  while (Pos(ThousandSeparator, s) > 0) do
    Delete(s, Pos(ThousandSeparator, s), 1);
  Result := s;
end;

{$IFNDEF DELPHI6_LVL}
function TryStrToFloat(s: string; var f: extended): boolean;
begin
  Result := false;
  try
    f := StrToFloat(s);
    Result := true;
  except
    f := 0;
  end;
end;
{$ENDIF}

function TAdvSmoothEdit.EStrToFloat(s: string): extended;
begin
  Result := 0;
  if Pos(ThousandSeparator, s) > 0 then
    s := StripThousandSep(s);

  if (FPrecision > 0) and (Length(s) > FPrecision) then
    if s[Length(s) - FPrecision] = Thousandseparator then
      s[Length(s) - FPrecision] := Decimalseparator;
  try
    TryStrToFloat(s, Result);
  except
    Result := 0;
  end;
end;

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

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

function TAdvSmoothEdit.CharFromPos(pt: TPoint): Integer;
begin
  Result := Loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelparam(pt.x, pt.y)));
end;

function TAdvSmoothEdit.PosFromChar(uChar: word): TPoint;
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

function TAdvSmoothEdit.GetTextSize: Integer;
var
  DC: HDC;
  sz: TSize;
  holdFont: THandle;
begin
  DC := GetDC(Handle);
  holdFont := SelectObject(DC, Font.Handle);
  GetTextExtentPoint32(DC, pchar(Text), Length(Text), sz);
  result := sz.CX;
  SelectObject(DC, holdFont);
  ReleaseDC(Handle, DC);
end;


procedure TAdvSmoothEdit.InvalidateCaret(pt: TPoint);
var
  r: TRect;
begin
  r := rect(pt.x, pt.y, pt.x + 1, pt.y - Font.Height);
  InvalidateRect(self.Handle, @r, true);
end;

procedure TAdvSmoothEdit.EraseCaret;
var
  pt: TPoint;
begin
  pt := FCaretPos;
  FCaretPos := Point(-1, -1);
  InvalidateCaret(pt);
end;

procedure TAdvSmoothEdit.DrawCaretByCursor;
var
  nChar: Integer;
  pt, ptCursor: TPoint;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);

  nChar := CharFromPos(ptCursor);

  pt := PosFromChar(nChar);

  if (fCaretPos.x <> pt.x) or (fCaretPos.y <> pt.y) then
  begin
    InvalidateCaret(fCaretPos);
    InvalidateCaret(pt);
    FCaretPos := pt;
  end;
end;

procedure TAdvSmoothEdit.SetCaretByCursor;
var
  ptCursor: TPoint;
  nChar: integer;
begin
  GetCursorPos(ptCursor);
  ptCursor := ScreenToClient(ptCursor);
  nChar := loword(SendMessage(self.Handle, EM_CHARFROMPOS, 0, makelong(ptCursor.x, ptCursor.y)));
  SelStart := nChar;
  SelLength := 0;
end;

function CheckSeparator(ch: char): boolean;
begin
  {$IFNDEF DELPHI_UNICODE}
  Result := ch in ['-', ',', ';'];
  {$ENDIF}

  {$IFDEF DELPHI_UNICODE}
  Result := (ch = '-') or (ch = ',') or (ch = ';');
  {$ENDIF}
end;

procedure TAdvSmoothEdit.WMChar(var Msg: TWMKey);
var
  oldSelStart, oldprec: Integer;
// oldSelLength: Integer;
  s: string;
  key: char;
  isCtrl: Boolean;
  cf: TCustomForm;

  function scanprecision(s: string; inspos: Integer): Boolean;
  var
    mdist: Integer;
  begin
    Result := false;
    inspos := inspos - Length(fPrefix);
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

    if (s <> Text) then
      Modified := true;

    inherited;

    if (EditType = etMoney) then
      AutoSeparators;


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
              if (floatvalue <> 0) or (1 > 0) then
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
            if inherited Modified then
              SetModified(true);
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

        inherited Text := ShiftCase(self.Text);
        SelStart := oldSelStart + 1;
      end;
  end;

  if (FTabOnFullLength) then
  begin
    if (FLengthlimit > 0) and (FixedLength(self.text) > flengthlimit) and
      (SelLength = 0) and (SelStart = fLengthlimit) then postmessage(self.handle, wm_keydown, VK_TAB, 0);
  end;

  if inherited Modified then
    SetModified(true);

  UpdateLookup;
end;

procedure TAdvSmoothEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if not (FEditType = etPassword) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;

  case FEditAlign of
    eaRight:
      begin
        Params.Style := Params.Style and not (ES_LEFT) and not (ES_CENTER);
        Params.Style := Params.Style or (ES_RIGHT);
      end;
    eaCenter:
      begin
        Params.Style := Params.Style and not (ES_LEFT) and not (ES_RIGHT);
        Params.Style := Params.Style or (ES_CENTER);
      end;
  end;
end;

procedure TAdvSmoothEdit.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(self.font);
    end;

  inherited;
  SetFlatRect(FFlat);
end;

procedure TAdvSmoothEdit.CMCancelMode(var Message: TMessage);
begin
  inherited;
  if FLookupList.Visible then
    FLookupList.Hide;
end;

procedure TAdvSmoothEdit.SetFlatRect(const Value: Boolean);
var
  loc: TRect;
  lft: integer;
begin
  if HandleAllocated then
  begin
    lft := 0;
    if not Ctl3D then
      lft := Font.Size div 3;

    if Value then
    begin
      loc.Left := lft + 4 + IndentL;
      loc.Top := 3;
      loc.Right := Clientrect.Right - 4 - IndentR - lft;
      loc.Bottom := Clientrect.Bottom - 4;
    end
    else
    begin
      loc.Left := lft + IndentL;
      loc.Top := 0;
      loc.Right := ClientRect.Right - IndentR - lft;
      loc.Bottom := ClientRect.Bottom;
    end;

    SendMessage(Handle, EM_SETRECTNP, 0, LParam(@loc));

  end;
end;

procedure TAdvSmoothEdit.SetFlat(const value: boolean);
var
  OldColor: TColor;

begin
  if (csLoading in ComponentState) then
  begin
    FFlat := Value;
    Exit;
  end;

  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
    begin
      OldColor := Color;
      if FFlatParentColor then
      begin
        Color := (Parent as TWinControl).Brush.Color;
        FocusColor := Color;
      end
      else
      begin
        Color := FNormalColor;
        FocusColor := FFocusColor;
      end;

      FNormalColor := OldColor;
      BorderStyle := bsNone;
      SetFlatRect(True);
    end
    else
    begin
      Color := FNormalColor;
      FocusColor := FFocusColor;
      BorderStyle := FOldBorder;
      SetFlatRect(False);
    end;
    Invalidate;
  end;
end;

procedure TAdvSmoothEdit.CNCtlColorEdit(var Message: TWMCtlColorEdit);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;

procedure TAdvSmoothEdit.CNCtlColorStatic(var Message: TWMCtlColorStatic);
begin
  inherited;
  if FTransparent then
    SetBkMode(Message.ChildDC, Windows.TRANSPARENT);
end;


procedure TAdvSmoothEdit.SetTransparent(const Value: boolean);
begin
  if FTransparent <> Value then
  begin
    FTransparent := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothEdit.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.SetFlatParentColor(const Value: boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.SetEditType(value: TAdvSmoothEditType);
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

    if (Text <> '') or (not AllowNumericNullValue) then
    begin
      at := IsType(self.Text);
      case FEditType of
        etHex: if not (at in [atNumeric, atHex]) then self.IntValue := 0;
        etNumeric: if (at <> atNumeric) then self.IntValue := 0;
        etFloat, etMoney: if not (at in [atFloat, atNumeric]) then self.FloatValue := 0.0;
      end;
    end;

    if (csDesigning in ComponentState) and (FEditType = etFloat) and (Precision = 0) then
      Precision := 2;
  end;
end;

procedure TAdvSmoothEdit.SetEditAlign(const Value: TEditAlign);
begin
  if FEditAlign <> Value then
  begin
    FEditAlign := Value;
    ReCreateWnd;
  {force a proper re-alignment}
    self.Width := self.Width + 1;
    self.Width := self.Width - 1;
  end;
end;

procedure TAdvSmoothEdit.SetCanUndo(const Value: boolean);
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

procedure TAdvSmoothEdit.WMActivate(var Message: TMessage);
begin
  inherited;
end;

procedure TAdvSmoothEdit.WMKillFocus(var Msg: TWMKillFocus);
var
  IsValid: Boolean;
  OldModified: Boolean;
  TT: string;
begin
  if (csLoading in ComponentState) then
    Exit;

  if FLookupList.Visible and not (msg.FocusedWnd = FLookupList.Handle) then
    FLookupList.Hide;

  if not IsError then
  begin
    if (Color <> FNormalColor) then
    begin
      Color := FNormalColor;
      if FIsWinXP then
      begin
        Width := Width - 1;
        Width := Width + 1;
      end;
    end;
  end;

  if not ((IsError and ShowError) or (ShowModified)) then
  begin
    if not (TestURL and ShowURL) then
      Font.Color := FFontColor;
  end;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];

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

  IsValid := DoValidate(Self.Text);

  if not IsValid then
  begin
    Msg.Result := 0;
  end;

  inherited;

  if FFocusBorder then
    Invalidate;

  if FAlignChanging then
    Exit;

  if FFocusWidthInc > 0 then
    Width := Width - FFocusWidthInc;


  if (FEditAlign <> FOldEditAlign) and (FFocusAlign <> eaDefault) then
  begin
    EditAlign := FOldEditAlign;
  end;

  if (EmptyText <> '') and (Text = '') then
    Invalidate;

  TT := Trim(Text);

  if FLookup.Enabled and FLookup.History and (TT <> '') then
  begin
    if FLookup.DisplayList.IndexOf(TT) = -1 then
      FLookup.DisplayList.Add(TT);
  end;

  if FocusBorderColor <> clNone then
    SendMessage(self.Handle, WM_NCPAINT, 0,0);

  if (ErrorMarkerLen > 0) then
    Invalidate;
end;

procedure TAdvSmoothEdit.SelectAll;
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

  if (Pos('://', self.Text) > 0) and FShowURL then
  begin
    SelStart := Pos('://', self.text) + 2;
    SelLength := Length(self.text);
  end;

  if (Pos('mailto:', self.text) > 0) and FShowURL then
  begin
    SelStart := Pos('mailto:', self.text) + 6;
    SelLength := Length(self.text);
  end;
end;

procedure TAdvSmoothEdit.SelectBeforeDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i > 0) then
    SelStart := i + Length(fPrefix) - 1
  else
    SelStart := Length(fPrefix);
end;

procedure TAdvSmoothEdit.SelectAfterDecimal;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.Text);

  if (i > 0) then
    SelStart := i + Length(fPrefix)
  else
    SelStart := Length(fPrefix);
end;


procedure TAdvSmoothEdit.WMSetFocus(var Msg: TWMSetFocus);
begin
  if csLoading in ComponentState then
    Exit;

  inherited;

  FOldString := Self.Text;

  if (FFocusColor <> clNone) and DoValidate(Self.Text) then
  begin
    inherited Color := FFocusColor;
    if FIsWinXP then
    begin
      Width := Width - 1;
      Width := Width + 1;
    end;
  end;

  if not ((IsError and ShowError) or (ShowModified)) then
  begin
    if (Font.Color <> FFontColor) then
      FFontColor := Font.Color;

    if not (TestURL and ShowURL) and (FFocusFontColor <> clNone) then
      Font.Color := FFocusFontColor;
  end;

  if AutoSelect then
    SelectAll;
  //else
  //  if EditType in [etFloat, etMoney] then
  //    SelectBeforeDecimal;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];

  if (FFocusWidthInc > 0) and not FAlignChanging then
    Width := Width + FFocusWidthInc;

  if not FAlignChanging then
    FOldEditAlign := FEditAlign;

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
end;

constructor TAdvSmoothEdit.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  i: integer;

begin
  inherited Create(aOwner);
  FFocusColor := clNone;
  FFocusFontColor := clWindowText;
  FNormalColor := clWindow;
  FFocusAlign := eaDefault;
  FFontColor := self.Font.Color;
  FModifiedColor := clHighLight;
  FErrorColor := clRed;
  FErrorFontColor := clWhite;
  FError := False;
  FLabel := nil;
  FLabelMargin := 4;
  FURLColor := clBlue;
  FDisabledColor := clSilver;
  FDisabledBorder := true;
  FPersistence := TPersistence.Create;
  FFlatParentColor := True;
  FFlatLineColor := clBlack;
  FCaretPos := point(-1, -1);
  FButtonDown := false;
  FMouseInControl := false;
  FCanUndo := True;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChange;
  FAutoThousandSeparator := True;
  FDefaultHandling := ADVSMOOTHEDIT_DEFAULTHANDLING;
  FOldBorder := bsSingle;
  FFocusBorderColor := clNone;
  FIndentL := 0;
  FIndentR := 0;
  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  FIsWinXP := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));

  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);

  if not (csDesigning in ComponentState) then
  begin
    FLookupList := TListHintWindow.Create(Self);
    FLookupList.Visible := False;
    FLookupList.Width := 0;
    FLookupList.Height := 0;
    //FLookupList.Parent := Self;
    FLookupList.BorderWidth := 1;
    FLookupList.ControlStyle := FLookupList.ControlStyle + [csReplicatable];
    FLookupListbox := TListBox.Create(FLookupList);

    with FLookupListBox do
    begin
      Parent := FLookupList;
      Align := alClient;
      Style := lbOwnerDrawFixed;
      ItemHeight := 12;
      Ctl3D := false;
      TabStop := true;
      BorderStyle := bsNone;
      TabOrder := 0;
      OnKeyPress := ListKeyPress;
      OnMouseUp := ListMouseUp;
      ControlStyle := Controlstyle + [csReplicatable];
    end;
    FLookupList.ListControl := FLookupListBox;
  end;

  FLookup := TLookupSettings.Create;

  FParentFnt := false;
end;

destructor TAdvSmoothEdit.Destroy;
begin
  if (FLabel <> nil) then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  
  FLabelFont.Free;
  FPersistence.Free;
  FPersistence := nil;
  if not (csDesigning in ComponentState) then
  begin
    FLookupListBox.Free;
    FLookupList.Free;
  end;
  FLookup.Free;
  inherited Destroy;
end;

procedure TAdvSmoothEdit.SetParent(AParent: TWinControl);
begin
  inherited;
  if FLabel <> nil then
    FLabel.Parent := AParent;
end;

procedure TAdvSmoothEdit.ApplyURL(const Value: Boolean);
begin
  if Value then
  begin
    FParentFnt := false;  
    Font.Style := Font.Style + [fsUnderline];
    Font.Color := FURLColor;
    FIsUrl := True;
    Cursor := crHandPoint;
    Invalidate;
  end
  else
  begin
    Font.Style := Font.Style - [fsUnderline];
    Font.Color := FFontColor;
    FIsUrl := False;
    Cursor := crDefault;
  end;
end;

procedure TAdvSmoothEdit.CNCommand(var Message: TWMCommand);
begin
  if (Message.NotifyCode = EN_CHANGE) then
  begin
    if FBlockChange then
    begin
      FBlockChange := false;
      Exit;
    end;
  end;

  if (Message.NotifyCode = EN_CHANGE) then
    if FTransparent then
    begin
      Invalidate;
    end;

  if (Message.NotifyCode = EN_CHANGE) and (FShowURL) then
  begin
    if TestURL and not FIsUrl then
    begin
      ApplyURL(True);
    end
    else
      if FIsUrl and not TestURL then
      begin
        ApplyURL(False);
      end;
  end;

  inherited;
end;


procedure TAdvSmoothEdit.WMKeyDown(var Msg: TWMKeydown);
var
  selp: Integer;
  s: string;
  isCtrl: Boolean;

begin
  IsCtrl := GetKeyState(VK_CONTROL) and $8000 = $8000;

  if (msg.CharCode = VK_RETURN) and FLookupList.Visible then
  begin
    DoneLookup;
    Exit;
  end;

  if (msg.CharCode in [VK_TAB, VK_RETURN]) and IsCtrl then
  begin
//    if msg.CharCode = VK_RETURN then
//      Msg.CharCode := 0;
//    Msg.Result := 1;
    inherited;
    Exit;
  end;

  if (msg.CharCode = VK_HOME) and FLookupList.Visible then
  begin
    FLookupListBox.ItemIndex := 0;
    Msg.result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_END) and FLookupList.Visible then
  begin
    FLookupListBox.ItemIndex := FLookupListBox.Items.Count - 1;
    Msg.result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_NEXT) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex + FLookup.DisplayCount < FLookupListBox.Items.Count then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex + FLookup.DisplayCount
    else
      FLookupListBox.ItemIndex := FLookupListBox.Items.Count - 1;
    Msg.result := 1;
    Exit;
  end;

  if (msg.CharCode = VK_PRIOR) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex > FLookup.DisplayCount then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex - FLookup.DisplayCount
    else
      FLookupListBox.ItemIndex := 0;
    Msg.result := 1;
    Exit;
  end;

  if (msg.charcode = VK_RETURN) and (FReturnIsTab) then
  begin
    msg.charcode := VK_TAB;
    if IsWindowVisible(self.Handle) then
      PostMessage(self.Handle, WM_KEYDOWN, VK_TAB, 0);
  end;                                                       

  if (msg.charcode = VK_RIGHT) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(self.handle, EM_GETSEL, 0, 0));
    if selp >= Length(self.text) then
    begin
      msg.charcode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.charcode = VK_DELETE) and (FSuffix <> '') then
  begin
    selp := hiword(SendMessage(self.handle, EM_GETSEL, 0, 0));
    if (selp >= Length(self.text)) and (SelLength = 0) then
    begin
      msg.charcode := 0;
      msg.Result := 0;
      Exit;
    end;
    SetModified(true);
  end;

  if (msg.charcode = VK_LEFT) and (FPrefix <> '') then
  begin
    selp := hiword(SendMessage(self.handle, EM_GETSEL, 0, 0));

    if selp <= Length(FPrefix) then
    begin
      msg.charcode := 0;
      msg.Result := 0;
      Exit;
    end;
  end;

  if (msg.charcode = VK_END) and (FSuffix <> '') then
  begin
    if (GetKeyState(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(self.text);
      SelLength := 0;
    end
    else
      SelLength := Length(self.text) - SelStart;
    msg.charcode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.charcode = VK_HOME) and (FPrefix <> '') then
  begin
    if (getkeystate(VK_SHIFT) and $8000 = 0) then
    begin
      SelStart := Length(fPrefix);
      SelLength := 0;
    end
    else
    begin
      SendMessage(self.handle, EM_SETSEL, Length(fprefix) + Length(self.text), Length(fprefix));
    end;
    msg.Charcode := 0;
    msg.Result := 0;
    Exit;
  end;

  if (msg.charcode = VK_BACK) and (FPrefix <> '') then
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
      exit;
    end;
    SetModified(true);
  end;

  if (msg.CharCode = VK_UP) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex > 0 then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex - 1;
    msg.CharCode := 0;
  end;

  if (msg.CharCode = VK_DOWN) and FLookupList.Visible then
  begin
    if FLookupListBox.ItemIndex + 1 < FLookupListBox.Items.Count then
      FLookupListBox.ItemIndex := FLookupListBox.ItemIndex + 1;
    msg.CharCode := 0;
  end;

  inherited;

  if (msg.CharCode = VK_DELETE) and (EditType = etMoney) then
    AutoSeparators;

  if (fPrefix <> '') and (SelStart < Length(fPrefix)) then
    SelStart := Length(fPrefix);
end;

procedure TAdvSmoothEdit.WMCopy(var Message: TWMCopy);
var
  Allow: Boolean;
begin
  Allow := True;
  if Assigned(FOnClipboardCopy) and not FBlockCopy then
    FOnClipboardCopy(self, copy(self.Text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  FBlockCopy := False;  
  if Allow then inherited;
end;

procedure TAdvSmoothEdit.WMCut(var Message: TWMCut);
var
  Allow: Boolean;
begin
  Allow := True;
  FBlockCopy := True;
  if Assigned(FOnClipboardCut) then
  begin
    FOnClipboardCut(self, copy(self.text, SelStart + 1 - Length(fPrefix), SelLength), allow);
  end;
  if Allow then inherited;

  FBlockCopy := False;
end;


procedure TAdvSmoothEdit.WMPaste(var Msg: TMessage);
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

  if TabOnFullLength then
  begin
    if (length(self.Text) = LengthLimit) and (LengthLimit > 0) then
    begin
      Windows.SetFocus(GetNextDlgTabItem(Parent.Handle,self.Handle,true));
    end;
  end;
end;

procedure TAdvSmoothEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  PaintEdit;
  if Border3D or (FFocusBorderColor <> clNone) or (not Enabled and DisabledBorder) then
    DrawBorder;
end;

procedure TAdvSmoothEdit.PaintEdit;
var
  DC: HDC;
  Oldpen: HPen;
  Loc: TRect;
  Canvas: TCanvas;
begin
  if FFlat then
  begin
    DC := GetDC(Handle);
    if FFocusBorder then
    begin
      DrawControlBorder(DC);
    end
    else
    begin
      if FFlatLineColor <> clNone then
      begin
        if Enabled then
          OldPen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(FFlatLineColor)))
        else
          OldPen := SelectObject(DC, CreatePen(PS_SOLID, 1, ColorToRGB(clGray)));

         SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

        if FSoftBorder then
        begin
          MovetoEx(DC, Loc.Left - 4 + IndentL, Height - 1, nil);
          LineTo(DC, Width - 1, Height - 1);
          LineTo(DC, Width - 1, Loc.Top - 3);
          LineTo(DC, Loc.Left - 4 + IndentL, Loc.Top - 3);
          LineTo(DC, Loc.Left - 4 + IndentL, Height - 1);
        end
        else
        begin
          MovetoEx(DC, Loc.Left - 2 + IndentL, Height - 1, nil);
          LineTo(DC, Width - IndentR, Height - 1);
        end;

        DeleteObject(SelectObject(DC, OldPen));
      end;
    end;

    ReleaseDC(Handle, DC);
  end;

  if (FCaretPos.x <> -1) and (FCaretPos.y <> -1) then
  begin
    DC := GetDC(Handle);
    Rectangle(DC, FCaretPos.x, FCaretPos.y, FCaretPos.x + 1, FCaretPos.y - Font.Height);
    ReleaseDC(Handle, DC);
  end;

  if (Text = '') and (GetFocus <> Handle) and (FEmptyText <> '') then
  begin
    DC := GetDC(Handle);
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    if IsVista then
    begin
      Canvas.Brush.Color := Color;
    end
    else
      SetBkMode(Canvas.Handle, windows.TRANSPARENT);

    Canvas.Font.Color := clGray;
    Canvas.TextOut(3, 2, FEmptyText);
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;

  if (GetFocus <> Handle) and (ErrorMarkerLen > 0) then
  begin
    DC := GetDC(Handle);
    Canvas := TCanvas.Create;
    Canvas.Handle := DC;
    DrawErrorLines(Canvas, ErrorMarkerPos, ErrorMarkerLen);
    Canvas.Free;
    ReleaseDC(Handle, DC);
  end;
end;

procedure TAdvSmoothEdit.DrawErrorLines(Canvas: TCanvas; ErrPos, ErrLen: Integer);
var
  pt1: TPoint;
  pt2: TPoint;
  l: Integer;
  o: Integer;
  ep: Integer;
  Rect: TRect;
  h: Integer;
begin
  Rect := GetClientRect;
  if ErrPos >= Length(Text) then
  begin
    ep := Length(Text);
    l := SendMessage(Handle, EM_POSFROMCHAR, ep, 0);
    pt1 := Point(LoWord(l), HiWord(l));
    pt1.X := pt1.X + 4;
  end
  else
  begin
    l := SendMessage(Handle, EM_POSFROMCHAR, ErrPos, 0);
    pt1 := Point(LoWord(l), HiWord(l));
  end;

  if ErrPos + ErrLen >= Length(Text) then
  begin
    ep := Length(Text) - 1;
    l := SendMessage(Handle, EM_POSFROMCHAR, ep, 0);
    pt2 := Point(LoWord(l), HiWord(l));
    pt2.X := pt2.X + 4;
    pt2.Y := pt1.Y;
  end
  else
  begin
    l := SendMessage(Handle, EM_POSFROMCHAR, ErrPos + ErrLen - 1, 0);
    pt2 := Point(LoWord(l), HiWord(l));
  end;

  Canvas.Pen.Color := clRed;
  Canvas.Pen.Width := 2;

  Canvas.Font.Assign(Font);
  h := Canvas.TextHeight('gh') - 1;

  l := pt1.X;
  o := 3;

  Canvas.MoveTo(Rect.Left + l, Rect.Top + pt1.Y + h + o);

  while l <= pt2.X do
  begin
    if o = 3 then o := 0 else o := 3;
    Canvas.LineTo(Rect.Left + l + 3, pt2.Y + h + o);
    Inc(l, 3);
  end;

  if o = 3 then o := 0 else o := 3;
  Canvas.LineTo(Rect.Left + l + 3, Rect.Top + pt2.Y + h + o);
end;


procedure TAdvSmoothEdit.WMEraseBkGnd(var Message: TWMEraseBkGnd);
var
  DC: HDC;
  i: Integer;
  p: TPoint;
begin
  if FTransparent then
  begin
    if Assigned(Parent) then
    begin
      DC := Message.DC;
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
    end;
  end
  else
    inherited;
end;

procedure TAdvSmoothEdit.WMMouseMove(var Msg: TWMMouse);
var
  m: pchar;
  s: string;
  dwEffects: Integer;
  isCopy: Boolean;
  hres: HResult;

begin
  inherited;
  if (SelLength > 0) and (FButtonDown) and (FOleDropSource) then
  begin
    GetMem(m, SelLength + 1);
    GetSelTextBuf(m, SelLength + 1);
    s := StrPas(m);
    FreeMem(m);

    FIsDragSource := true;
    hres := StartTextDoDragDrop(s, '', DROPEFFECT_COPY or DROPEFFECT_MOVE, dwEffects);
    FIsDragSource := false;

    isCopy := (getkeystate(vk_control) and $8000 = $8000);

    if not isCopy and (hres = DRAGDROP_S_DROP) then
    begin
    {cut the text here}
      ClearSelection;
      EraseCaret;
      Invalidate;
    end;

    FButtonDown := False;
  end;
end;

procedure TAdvSmoothEdit.WMLButtonDown(var Msg: TWMMouse);
var
  uchar: Integer;
begin
  // click outside selection
  uchar := CharFromPos(point(msg.xpos, msg.ypos));

  if (SelLength <= 0) or (uchar < SelStart) or (uChar > SelStart + SelLength) or
    (GetFocus <> self.Handle) then
    inherited
  else
    if (uChar >= SelStart) and (uChar <= SelStart + SelLength) and (SelLength > 0) then
      FButtonDown := True;
end;

procedure TAdvSmoothEdit.WMLButtonUp(var Msg: TWMMouse);
var
  uchar: Integer;
  show: Boolean;
begin
  if fButtonDown then
  begin
    uchar := CharFromPos(point(msg.xpos, msg.ypos));
    SelStart := uChar;
    SelLength := 0;
  end;

  fButtonDown := false;

  inherited;

  if FIsUrl and (self.Handle = GetFocus) and FShowURL then
  begin
    show := True;
    if Assigned(FOnURLClick) then
      FOnURLClick(self, self.Text, show);

    if show then
      ShellExecute(0, 'open', pchar(self.Text), nil, nil, SW_NORMAL);
    inherited;
    Exit;
  end;


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
      if (fPrefix <> '') then
        SelStart := Length(fPrefix) + Length(self.Text)
      else
        SelStart := Length(self.Text);
      SelLength := 0;
    end;
    if (SelStart + SelLength > Length(self.text)) then
    begin
      if (fPrefix <> '') then
        SelLength := Length(self.Text)// - SelStart;
      else
        SelLength := Length(self.Text) - SelStart;
    end;
  end;
end;

procedure TAdvSmoothEdit.SetPrefix(const Value: string);
var
  s: string;
begin
  s := self.Text;
  fPrefix := Value;
  inherited Text := s;
//changed for v1.8
  Text := s;
end;

procedure TAdvSmoothEdit.SetSuffix(const Value: string);
var
  s: string;
begin
  s := self.text;
  fSuffix := Value;
  inherited Text := s;
//changed for v1.8
  Text := s;
end;

function TAdvSmoothEdit.DecimalPos: Integer;
var
  i: Integer;
begin
  i := Pos(decimalseparator, self.text);
  if (i = 0) then Result := Length(fprefix) + Length(self.text) + Length(fSuffix) + 1
  else Result := Length(fPrefix) + i;
end;

function TAdvSmoothEdit.AllowMin(ch: char): boolean;
begin
  Result := Signed and (EditType in [etFloat,etNumeric, etMoney]) and (ch = '-');
end;

function TAdvSmoothEdit.FixedLength(s: string): Integer;
var
  i: Integer;
begin
  s := StripThousandSep(s);
  i := Pos(decimalseparator, s);
  if (i > 0) then Result := i else Result := Length(s) + 1;

  if Signed and (EditType in [etFloat,etNumeric, etMoney]) and (pos('-',s) > 0) then
    Result := Result - 1;
end;

function TAdvSmoothEdit.GetText: string;
var
  s: string;
begin
  s := inherited Text;
  if (fPrefix <> '') and (Pos(fPrefix, s) = 1) then delete(s, 1, Length(fPrefix));
  if (fSuffix <> '') then delete(s, Length(s) - Length(fSuffix) + 1, Length(fSuffix));
  Result := s;
end;

procedure TAdvSmoothEdit.SetText(value: string);
var
  fmt, neg: string;
  f: extended;
begin
  if (value = '') then
  begin
    if not AllowNumericNullValue then
    begin
    case FEditType of
      etFloat: if not (IsType(value) in [atFloat, atNumeric]) then value := '0';
      etMoney: if not (IsType(value) in [atFloat, atNumeric]) then value := '0';
      etHex: if not (IsType(value) in [atHex, atNumeric]) then value := '0';
      etNumeric: if not (IsType(value) in [atNumeric]) then value := '0';
    end;
    end;


  end;

  if PrecisionDisplay = pdNormal then
  begin
    if (FPrecision > 0) and (value <> '') then
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
  end
  else
  begin
    if (FEditType in [etFloat, etMoney]) then
    begin
      f := EStrToFloat(Value);
      Value := Format('%g', [f]);
    end;
  end;



  if (FEditType in [etHex]) then
    Value := AnsiUpperCase(value);

  inherited Text := FPrefix + Value + FSuffix;

  SetModified(False);

  if FShowURL then ApplyURL(TestURL);
end;

procedure TAdvSmoothEdit.SetTextDirect(s: string);
begin
  inherited Text := s;
end;

function TAdvSmoothEdit.GetVisible: boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvSmoothEdit.CMShowingChanged(var Message: TMessage);
begin
  inherited;
  if (FLabel <> nil) then
    FLabel.Visible := Visible;
end;

procedure TAdvSmoothEdit.SetVisible(const Value: boolean);
begin
  inherited Visible := Value;
  if (FLabel <> nil) then
    FLabel.Visible := Value;
end;


function TAdvSmoothEdit.CreateLabel: TLabel;
begin
  Result := TLabel.Create(self);
  Result.Parent := self.Parent;
  Result.FocusControl := self;
  Result.Font.Assign(LabelFont);
  Result.OnClick := LabelClick;
  Result.OnDblClick := LabelDblClick;
  Result.ParentFont := self.ParentFont;
end;

function TAdvSmoothEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.Caption
  else
    Result := '';
end;

procedure TAdvSmoothEdit.AutoSeparators;
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

//changed 1.8
  FBlockChange := (Text <> FPrefix + neg + si + s + FSuffix);

  inherited Text := FPrefix + neg + si + s + fSuffix;

  FBlockChange := false;

  FPrecision := OldPrec;

  Diffl := Length(self.Text) - Diffl;

  SelStart := OldSelStart + Diffl;
  SelLength := 0;
end;
                  
procedure TAdvSmoothEdit.UpdateLabel;
begin
  FLabel.Transparent := FLabeltransparent;

  if not FParentFnt then
  begin
    FLabel.Font.Assign(FLabelFont);
  end
  else
    FLabel.Font.Assign(self.Font);

  case FLabelPosition of
    lpLeftTop:
      begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left - FLabel.Canvas.TextWidth(FLabel.Caption) - FLabelMargin;
      end;
    lpLeftCenter:
      begin
        if Self.Height > FLabel.Height then
          FLabel.Top := self.Top + ((self.Height - FLabel.Height) div 2)
        else
          FLabel.Top := self.Top - ((FLabel.Height - self.Height) div 2);   
        FLabel.Left := self.Left - FLabel.Canvas.TextWidth(FLabel.Caption) - FLabelMargin;
      end;
    lpLeftBottom:
      begin
        FLabel.Top := self.Top + self.Height - FLabel.Height;
        FLabel.Left := self.Left - FLabel.Canvas.TextWidth(FLabel.Caption) - FLabelMargin;
      end;
    lpTopLeft:
      begin
        FLabel.Top := self.Top - FLabel.height - FLabelMargin;
        FLabel.Left := self.Left;
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

procedure TAdvSmoothEdit.SetLabelPosition(const value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvSmoothEdit.SetLabelMargin(const value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvSmoothEdit.SetLabelFont(const Value: TFont);
begin
  if not ParentFont then
    FLabelFont.Assign(Value);

  if FLabel <> nil then
    UpdateLabel;
end;

procedure TAdvSmoothEdit.LabelFontChange(Sender: TObject);
begin
  if FLabel <> nil then
  begin
    UpdateLabel;
    if (csDesigning in ComponentState) and not (csLoading in ComponentState) then
      ParentFont := false;
  end;
end;

procedure TAdvSmoothEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (FLabel <> nil) then
  begin
    if FLabel.Parent <> nil then
      UpdateLabel;
  end;

  SetFlatRect(FFlat);

  if FFlat then
    Flat := FFlat;
end;

procedure TAdvSmoothEdit.SetLabelCaption(const value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := Value;
  UpdateLabel;
end;

procedure TAdvSmoothEdit.SetLabelTransparent(const value: boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvSmoothEdit.CMMouseEnter(var Msg: TMessage);
var
  pf: TCustomForm;
begin
  inherited;

  pf := GetParentForm(self);

  if FAutoFocus and not (csDesigning in ComponentState) then
  begin
    if Assigned(pf) then
    begin
      if (GetActiveWindow = pf.Handle) then
        self.SetFocus;
    end
    else
      self.SetFocus;
  end;

    
  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    if FFocusBorder then
      DrawBorder;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

procedure TAdvSmoothEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    if FFocusBorder then Invalidate;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

function TAdvSmoothEdit.GetFloat: double;
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
      if (self.Text <> '') then
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

function ValStr(s: string): Integer;
var
  err: Integer;
begin
  val(s, result, err);
end;

function TAdvSmoothEdit.GetInt: integer;
begin
  Result := 0;
  case FEditType of
    etHex: if (self.Text <> '') then Result := HexToInt(self.Text);
    etNumeric, etFloat: Result := ValStr(self.Text);
    etMoney: Result := ValStr(StripThousandSep(self.Text));
  end;
end;

procedure TAdvSmoothEdit.SetFloat(const Value: double);
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

procedure TAdvSmoothEdit.SetInt(const Value: integer);
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

procedure TAdvSmoothEdit.SetPrecision(const Value: smallint);
var
  at: TAutoType;
begin
  if (FPrecision <> value) and (editType in [etFloat, etMoney, etString]) then
  begin
    FPrecision := Value;
    if (Text <> '') or (not AllowNumericNullValue) then
    begin
      at := IsType(self.text);
      if (at in [atFloat, atNumeric]) then
        FloatValue := FloatValue
      else
        FloatValue := 0.0;
    end;
  end;
end;

procedure TAdvSmoothEdit.SetPrecisionDisplay(const Value: TPrecisionDisplay);
begin
  if (FPrecisionDisplay <> Value) then
  begin
    FPrecisionDisplay := Value;
    FloatValue := FloatValue;
  end;
end;

function TAdvSmoothEdit.GetModified: boolean;
begin
  Result := fIsModified;
end;

procedure TAdvSmoothEdit.SetModified(const Value: boolean);
begin
  if csLoading in ComponentState then
    Exit;

  if ReadOnly then
    Exit;

  if FShowModified then
  begin
    if (value = false) then
      self.Font.Color := FFontColor
    else
      self.Font.Color := FModifiedColor;
  end;

  inherited Modified := value;
  FIsModified := value;
end;


procedure TAdvSmoothEdit.ListToRangeStr(rangelist: TRangeList);
var
  c: Integer;
  fstart, fcurr: Integer;
  s: string;
begin
  RangeList.sort(RangeListCompare);

  c := 1;
  fstart := RangeList.Items[0];
  fcurr := fstart;
  s := '';
  while (c < RangeList.Count) do
  begin
    if RangeList.Items[c] <> fCurr + 1 then
    begin
      if (fStart = -2) then {new possible start?}
      begin
        fStart := Rangelist.Items[c];
      end
      else
      begin
        if Length(s) > 0 then s := s + ',';
        if (fStart <> fCurr) then s := s + inttostr(fStart) + '-' + inttostr(fCurr) else s := s + inttostr(fCurr);
        fStart := Rangelist.Items[c];
      end;
    end;
    fCurr := RangeList.Items[c];
    inc(c);
  end;

  if Length(s) > 0 then
    s := s + ',';
  if (FStart <> FCurr) then
    s := s + inttostr(fStart) + '-' + inttostr(fCurr)
  else
    s := s + inttostr(fCurr);

  inherited Text := s;
end;

function TAdvSmoothEdit.RangeStrToList(rangelist: TRangeList): Boolean;
begin
  Result := RangeList.StrToList(self.Text);
end;

procedure TAdvSmoothEdit.LoadPersist;
var
  Inifile: TInifile;
  RegInifile: TRegInifile;
  s: string;
  i, j: Integer;
begin
  if FPersistence.Enable and (FPersistence.Section <> '') then
  begin
    if fPersistence.Location = plInifile then
    begin
      if FPersistence.Key = '' then
        FPersistence.Key := ChangeFileExt(ParamStr(0), '.INI');
        
      if Persistence.Section = '' then
        Persistence.Section := Name;

      Inifile := TInifile.Create(FPersistence.Key);

      if Lookup.Enabled and Lookup.History then
      begin
        i := IniFile.ReadInteger(FPersistence.Section, 'LOOKUPCOUNT', 0);
        for j := 1 to i do
        begin
          s := Inifile.ReadString(FPersistence.Section, 'LOOKUPVAL' + inttostr(j), '');
          if s <> '' then
            Lookup.ValueList.Add(s);

          s := Inifile.ReadString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(j), '');
          if s <> '' then
            Lookup.DisplayList.Add(s);
        end;
      end;

      s := Inifile.ReadString(FPersistence.Section, self.Name, '@');

      Inifile.Free;
    end
    else
    begin
      RegInifile := TRegInifile.Create(fPersistence.Key);

      if Lookup.Enabled and Lookup.History then
      begin
        i := RegIniFile.ReadInteger(FPersistence.Section, 'LOOKUPCOUNT', 0);
        for j := 1 to i do
        begin
          s := RegInifile.ReadString(FPersistence.Section, 'LOOKUPVAL' + inttostr(j), '');
          if s <> '' then
            Lookup.ValueList.Add(s);

          s := RegInifile.ReadString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(j), '');
          if s <> '' then
            Lookup.DisplayList.Add(s);
        end;
      end;

      s := RegInifile.ReadString(fPersistence.Section, self.Name, '@');

      RegInifile.Free;
    end;
    if (s <> '@') then inherited Text := s;
  end;
end;

procedure TAdvSmoothEdit.SavePersist;
var
  Inifile: TInifile;
  RegInifile: TRegInifile;
  i: Integer;
begin
  if not Assigned(FPersistence) then Exit;

  if FPersistence.Enable then
  begin
    if fPersistence.Location = plInifile then
    begin
      if FPersistence.Key = '' then
        FPersistence.Key := ChangeFileExt(ParamStr(0), '.INI');

      if FPersistence.Section = '' then
        FPersistence.Section := Name;

      Inifile := TInifile.Create(fPersistence.Key);
      Inifile.WriteString(fPersistence.Section, self.Name, fPrefix + self.Text + fSuffix);

      if Lookup.Enabled and Lookup.History then
      begin
        IniFile.WriteInteger(FPersistence.Section, 'LOOKUPCOUNT', Lookup.DisplayList.Count);
        for i := 1 to Lookup.DisplayList.Count do
        begin
          Inifile.WriteString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(i), Lookup.DisplayList[i - 1]);
          if i < Lookup.ValueList.Count then
            Inifile.WriteString(FPersistence.Section, 'LOOKUPVAL' + inttostr(i), Lookup.ValueList[i - 1]);
        end;
      end;

      Inifile.Free;
    end
    else
    begin
      RegInifile := TRegInifile.Create(fPersistence.Key);
      RegInifile.WriteString(fPersistence.Section, self.Name, fPrefix + self.Text + fSuffix);

      if Lookup.Enabled and Lookup.History then
      begin
        RegIniFile.WriteInteger(FPersistence.Section, 'LOOKUPCOUNT', Lookup.DisplayList.Count);
        for i := 1 to Lookup.DisplayList.Count do
        begin
          RegInifile.WriteString(FPersistence.Section, 'LOOKUPDISPL' + inttostr(i), Lookup.DisplayList[i - 1]);
          if i < Lookup.ValueList.Count then
            RegInifile.WriteString(FPersistence.Section, 'LOOKUPVAL' + inttostr(i), Lookup.ValueList[i - 1]);
        end;
      end;

      RegInifile.Free;
    end;
  end;
end;

procedure TAdvSmoothEdit.WMDestroy(var Msg: TMessage);
begin
  if not (csDesigning in ComponentState) then
    SavePersist;
  DefaultHandler(msg);
end;

function TAdvSmoothEdit.TestURL: Boolean;
begin
  Result := (Pos('://', self.text) > 0) or (Pos('@', self.text) > 1) or (Pos('www.', lowercase(self.Text)) = 1);
end;

function TAdvSmoothEdit.GetError: Boolean;
begin
  Result := FError;
end;

procedure TAdvSmoothEdit.SetError(const Value: Boolean);
begin
  if (csDesigning in ComponentState) or
    (csLoading in ComponentState) then
    Exit;

  if (Value <> FError) then
  begin
    FError := Value;

    if not ShowError then
      Exit;

    if FError then
    begin
      inherited Color := FErrorColor;
      Font.Color := FErrorFontColor;
    end
    else
    begin
      if GetFocus = Handle then
      begin
        if FFocusColor <> clNone then
          Color := FFocusColor
        else
          Color := FNormalColor;
        if FFocusFontColor <> clNone then
          Font.Color := FFocusFontColor;
      end
      else
      begin

        Color := FNormalColor;
        Font.Color := FFontColor;
      end;
    end;
  end;
end;

procedure TAdvSmoothEdit.Change;
var
  IsValid: Boolean;
begin
  inherited Change;

  if not (csLoading in ComponentState) then
  begin
    if FShowError then
    begin
      IsValid := DoValidate(Self.Text);
      IsError := not IsValid;
    end;
  end;
end;


procedure TAdvSmoothEdit.Init;
var
  OldColor: TColor;
begin
  FNormalColor := Color;
  FFontColor := Font.Color;
  FOldBorder := BorderStyle;
  FFlat := not FFlat;
  SetFlat(not FFlat);

  if FLabel <> nil then UpdateLabel;

  if not Enabled then
  begin
    OldColor := Color;
    Color := FDisabledColor;
    FNormalColor := OldColor;
  end;
end;

procedure TAdvSmoothEdit.Loaded;
begin
  inherited Loaded;

  if not (csDesigning in ComponentState) then
    Init;

  Height := FLoadedHeight;
  SetBounds(Left, Top, Width, Height);

  if not (csDesigning in ComponentState) then
    LoadPersist;

  if not LabelAlwaysEnabled and Assigned(FLabel) then
    FLabel.Enabled := Enabled;

  if (FLabel <> nil) then
    UpdateLabel;

  if Self.ParentFont and Assigned(FLabel) then
  begin
    FLabel.Font.Assign(Font);
  end;

  FParentFnt := self.ParentFont;

  if (FLabel <> nil) then
    UpdateLabel;
end;

procedure TAdvSmoothEdit.DrawBorder;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FFocusBorder and FMouseInControl) or Border3D or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;

procedure TAdvSmoothEdit.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
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

  if (FFocusBorderColor <> clNone) then
  begin
    if (GetFocus = self.Handle) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if not Enabled then
    Exit;

  if Is3DBorderButton then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((Parent as TWinControl).Brush.Color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderButton then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

function TAdvSmoothEdit.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := (Result and FFocusBorder) or (Border3D);
end;

function TAdvSmoothEdit.IsDropDownVisible: Boolean;
begin
  Result := false;
end;

procedure TAdvSmoothEdit.SetOleDropSource(const Value: boolean);
begin
  FOleDropSource := Value;
end;

procedure TAdvSmoothEdit.SetOleDropTarget(const Value: boolean);
begin
  FOleDropTarget := Value;
  if not (csDesigning in ComponentState) then
  begin
    if FOleDropTarget then
    begin
      FOleDropTargetAssigned := RegisterDragDrop(self.Handle, TEditDropTarget.Create(self)) = s_OK;
    end
    else
      if FOleDropTargetAssigned then RevokeDragDrop(self.Handle);
  end;
end;

procedure TAdvSmoothEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if (FFocusBorderColor <> clNone) or (not Enabled and DisabledBorder) then
    DrawBorder;
end;


function TAdvSmoothEdit.GetEnabledEx: boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvSmoothEdit.SetEnabledEx(const Value: boolean);
var
  OldValue: Boolean;
  OldColor: TColor;
begin
  if Value = inherited Enabled then
    Exit;

  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
    (csDesigning in ComponentState) then
    Exit;

  if OldValue <> Value then
  begin
    if Value then
    begin
      Color := FNormalColor;
    end
    else
    begin
      OldColor := Color;
      Color := FDisabledColor;
      FNormalColor := OldColor;
    end;

    if Assigned(FLabel) then
      if not FLabelAlwaysEnabled then
        FLabel.Enabled := Value;
  end;

  Width := Width + 1;
  Width := Width - 1;
end;

procedure TAdvSmoothEdit.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.SetDisabledBorder(const Value: boolean);
begin
  FDisabledBorder := Value;
  Invalidate;
end;


function TAdvSmoothEdit.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvSmoothEdit.SetColorEx(const Value: TColor);
begin
  inherited Color := Value;
  FNormalColor := Value;
end;

procedure TAdvSmoothEdit.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;

procedure TAdvSmoothEdit.CMHintShow(var Msg: TMessage);
  var
    hi: PHintInfo;

begin
  if (GetTextSize > Width) and (FHintShowLargeText) then
  begin
    hi := PHintInfo(Msg.LParam);
    hi.HintStr := Text;
    hi.HintPos := ClientToScreen(Point(0, 0));
  end;
  inherited;
end;

procedure TAdvSmoothEdit.SetAutoThousandSeparator(const Value: Boolean);
begin
  FAutoThousandSeparator := Value;
  if FEditType in [etMoney, etFloat] then AutoSeparators;
end;

procedure TAdvSmoothEdit.SetEmptyText(const Value: string);
begin
  FEmptyText := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
var
  IsPrev: Boolean;
  p: TWinControl;

begin
  p := Parent;
  while Assigned(p) and not (p is TCustomForm) do
    p := p.Parent;

  if not Assigned(p) then
    Exit;

  if FBlockDefaultHandling then
  begin
    FBlockDefaultHandling := false;
    Exit;
  end;

  IsPrev := (p as TCustomForm).KeyPreview;

  if (Msg.CharCode = VK_ESCAPE) and FCanUndo and not IsPrev then
  begin
    Text := FOldString;
    Font.Color := FFocusFontColor;
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
end;

procedure TAdvSmoothEdit.SetSoftBorder(const Value: Boolean);
begin
  if FSoftBorder <> Value then
  begin
    FSoftBorder := Value;
    Invalidate;
  end;
end;

procedure TAdvSmoothEdit.SetLabelAlwaysEnabled(const Value: Boolean);
begin
  FLabelAlwaysEnabled := Value;
  if FLabel <> nil then
    if Value then
      FLabel.Enabled := True;
  Invalidate;
end;


procedure TAdvSmoothEdit.SetBorder3D(const Value: Boolean);
begin
  FBorder3D := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.CreateWnd;
begin
  inherited;
  SetFlatRect(FFlat);
end;


procedure TAdvSmoothEdit.SetErrorMarkerLen(const Value: Integer);
begin
  FErrorMarkerLen := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.SetErrorMarkerPos(const Value: Integer);
begin
  FErrorMarkerPos := Value;
  Invalidate;
end;

procedure TAdvSmoothEdit.SetFocusBorder(const Value: Boolean);
begin
  FFocusBorder := Value;
  Invalidate;
end;

function TAdvSmoothEdit.GetHeightEx: Integer;
begin
  Result := inherited Height;
end;

procedure TAdvSmoothEdit.SetHeightEx(const Value: Integer);
begin
  if (csLoading in ComponentState) then
    FLoadedHeight := Value;
  inherited Height := Value;
end;

function VarPos(su, s: string; var Res: Integer): Integer;
begin
  Res := Pos(su, s);
  Result := Res;
end;

procedure TAdvSmoothEdit.ListMouseUp(Sender: TObject; Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  DoneLookup;
end;

procedure TAdvSmoothEdit.DoneLookup;
var
  idx, vp: Integer;
  NewValue: string;
  LookupText, NewText: string;
begin
  SetForegroundWindow(Handle);
  SetActiveWindow(Handle);
  Self.SetFocus;
  FLookupList.Hide;

  if (FlookupListBox.ItemIndex = -1) then
    Exit;

  idx := Integer(FLookupListBox.Items.Objects[FlookupListBox.ItemIndex]);

  if (idx >= 0) and (idx < FLookup.ValueList.Count) then
    NewValue := FLookup.ValueList.Strings[idx]
  else
    NewValue := FLookupListbox.Items[FLookupListBox.ItemIndex];

  if Assigned(FOnLookupSelect) then
    FOnLookupSelect(Self, NewValue);

  if Assigned(FOnlookupIndexSelect) then
    FOnLookupIndexSelect(Self, idx, NewValue);

  if FLookup.Multi then
  begin
    NewValue := NewValue + FLookup.Separator;
    LookupText := Text; // get current text value & strip till last lookup part
    NewText := '';
    while VarPos(FLookup.Separator, LookupText, vp) > 0 do
    begin
      NewText := NewText + Copy(LookupText, 1, vp);
      Delete(LookupText, 1, vp);
    end;
    Text := NewText + NewValue;

  end
  else
    Text := NewValue;

  if FLookup.Multi then
    SelStart := length(Text)
  else
    SelectAll;
end;

procedure TAdvSmoothEdit.ListKeyPress(Sender: TObject; var Key: Char);
begin
  if Key = #13 then
    DoneLookup;
end;

procedure TAdvSmoothEdit.UpdateLookup;
var
  pt, cp: TPoint;
  i, cnt, tp: Integer;
  mw, tw, vp: Integer;
  LookupText: string;
  lx,ly: Integer;
begin
  if not FLookup.Enabled then
    Exit;

  tp := 0;

  if FLookup.Multi then
  begin
    LookupText := Text;
    while VarPos(FLookup.Separator, LookupText, vp) > 0 do
    begin
      tp := tp + vp;
      Delete(LookupText, 1, vp);
    end;
  end
  else
    LookupText := Text;

  if Length(LookupText) >= FLookup.NumChars then
  begin
    pt := ClientToScreen(Point(0, 0));
    FLookupList.Text := '  ';
    FLookupList.Color := FLookup.Color;
    FLookupListbox.Color := FLookup.Color;
    FLookupListbox.Font.Name := 'Arial';
    FlookupListbox.Font.Size := 8;
    FLookupListbox.Ctl3D := False;
    FLookupList.Height := (12 * FLookup.DisplayCount) + 4;
    ly := pt.Y + Height - 6;

    i := SendMessage(Handle, EM_POSFROMCHAR, tp, 0);
    cp := Point(loword(i), hiword(i));

    lx := pt.X + 8 + cp.X;

    FLookupList.Color := clWindow;

    cnt := 0;
    FLookupListBox.Items.Clear;

    for i := 1 to FLookup.DisplayList.Count do
    begin
      if FLookup.CaseSensitive then
      begin
        if Pos(LookupText, FLookup.FDisplayList.Strings[i - 1]) = 1 then
        begin
          FLookupListbox.Items.AddObject(FLookup.FDisplayList.Strings[i - 1], TObject(i - 1));
          inc(cnt);
        end;
      end
      else
      begin
        if Pos(AnsiUppercase(LookupText), AnsiUppercase(FLookup.FDisplayList.Strings[i - 1])) = 1 then
        begin
          FLookupListbox.Items.AddObject(FLookup.FDisplayList.Strings[i - 1], TObject(i - 1));
          inc(cnt);
        end;
      end;
    end;

    if FLookup.FDisplayList.Count > 0 then
      FLookupListBox.ItemIndex := 0;

    if cnt < FLookup.DisplayCount then
      FLookupList.Height := (cnt * 12) + 4;

    FLookupListBox.Sorted := True;

    mw := 50;
    if cnt > 0 then
    begin
      for i := 1 to cnt do
      begin
        tw := FLookupList.Canvas.TextWidth(FLookupListBox.Items[i - 1]);

        if tw > mw then
          mw := tw;
      end;

      if cnt > FLookup.DisplayCount then
        mw := mw + GetSystemMetrics(SM_CXHSCROLL);

      if not FLookupList.Visible then
        FLookupList.Width := mw + 10;
    end;

    if cnt > 0 then
    begin
      FLookupList.Parent := self;
      FLookupList.Top := ly;
      FLookupList.Left := lx;
      FLookupList.Visible := true;
      FLookupList.Top := ly;
      FLookupList.Left := lx;
    end
    else
      FLookupList.Hide;

  end
  else
  begin
    FLookupList.Visible := False;
    FLookupList.Parent := nil;
  end;
end;

procedure TAdvSmoothEdit.LabelClick(Sender: TObject);
begin
  if Assigned(FOnLabelClick) then
    FOnLabelClick(Self);
end;

procedure TAdvSmoothEdit.LabelDblClick(Sender: TObject);
begin
  if Assigned(FOnLabelDblClick) then
    FOnLabelDblClick(Self);
end;

function TAdvSmoothEdit.DoValidate(value: string): Boolean;
var
  IsValid: Boolean;
begin
  IsValid := True;

  Result := IsValid;

  if FIsValidating then
    Exit;

  FIsValidating := True;

  ValidateEvent(value, isValid);

  FIsValidating := False;

  Result := IsValid;
end;

procedure TAdvSmoothEdit.ValidateEvent(Value: string; var IsValid: Boolean);
begin
  if Assigned(FOnValueValidate) then
    FOnValueValidate(Self, value, IsValid);
end;

function TAdvSmoothEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSmoothEdit.SetVersion(const Value: string);
begin

end;

{TAdvSmoothMaskEdit}

constructor TAdvSmoothMaskEdit.Create(AOwner: TComponent);
var
  i: integer;
begin
  inherited Create(aOwner);
  FAutoTab := True;
  FLabelMargin := 4;
  FReturnIsTab := True;
  FFocusColor := clWindow;
  FNormalColor := clWindow;
  FModifiedColor := clRed;
  FFocusBorderColor := clNone;
  FDisabledColor := clSilver;
  FDisabledBorder := true;
  FLabelFont := TFont.Create;
  FLabelFont.OnChange := LabelFontChanged;
  FFlatParentColor := True;
  // app is linked with COMCTL32 v6 or higher -> xp themes enabled
  i := GetFileVersion('COMCTL32.DLL');
  i := (i shr 16) and $FF;
  FIsThemed := (i > 5);
end;


procedure TAdvSmoothMaskEdit.SetAlignment(value: tAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TAdvSmoothMaskEdit.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(params);

  if (PasswordChar = #0) then
  begin
    Params.Style := Params.Style or ES_MULTILINE;
  end;

  if (FAlignment = taRightJustify) then
  begin
    params.style := params.style and not (ES_LEFT) and not (ES_CENTER);
    params.style := params.style or (ES_RIGHT);
    params.style := params.style or (ES_MULTILINE);
  end;

  if (FAlignment = taCenter) then
  begin
    params.style := params.style and not (ES_LEFT) and not (ES_RIGHT);
    params.style := params.style or (ES_CENTER);
    params.style := params.style or (ES_MULTILINE);
  end;

end;

procedure TAdvSmoothMaskEdit.KeyUp(var Key: Word; Shift: TShiftState);
var
  accept: Boolean;

begin
  inherited keyUp(key, shift);

  if (Pos(' ', self.text) = 0) and (self.SelStart = Length(EditText)) and (self.editmask <> '') and (self.Text <> '') then
  begin
    Accept := true;
    if Assigned(FOnMaskComplete) then
      FOnMaskComplete(self, self.Text, accept);
    if FAutoTab and Accept then
      Postmessage(self.handle, wm_keydown, VK_TAB, 0);
  end;
end;

procedure TAdvSmoothMaskEdit.DoEnter;
begin
  if (self.EditMask <> '') and fSelectFirstChar then
  begin
    self.SelStart := 0;
    self.SelLength := 1;
  end;
  inherited DoEnter;
end;

procedure TAdvSmoothMaskEdit.CMMouseEnter(var Msg: TMessage);
begin
  if FAutoFocus then
    self.SetFocus;

  if not FMouseInControl and Enabled then
  begin
    FMouseInControl := True;
    if FFocusBorder then DrawBorder;
  end;
end;

procedure TAdvSmoothMaskEdit.CMMouseLeave(var Msg: TMessage);
begin
  inherited;
  if FMouseInControl and Enabled then
  begin
    FMouseInControl := False;
    if FFocusBorder then Invalidate;
  end;
end;


procedure TAdvSmoothMaskEdit.WMKillFocus(var Msg: TWMKillFocus);
begin
  inherited;

  if csLoading in ComponentState then
    Exit;

  inherited Color := FNormalColor;

  Font.Color := FFontColor;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style - [fsBold];

  if FocusBorderColor <> clNone then
    SendMessage(self.Handle, WM_NCPAINT, 0,0);
end;

procedure TAdvSmoothMaskEdit.WMSetFocus(var Msg: TWMSetFocus);
begin

  inherited;

  if csLoading in ComponentState then
    Exit;

  inherited Color := FFocusColor;

  FOriginalValue := self.Text;

  if FocusFontColor <> clNone then
    Font.Color := FocusFontColor;

  if FFocusLabel and (FLabel <> nil) then
    FLabel.Font.Style := FLabel.Font.Style + [fsBold];

  if AutoSelect then
    SelectAll;

  if (FocusBorder) or (FFocusBorderColor <> clNone) then
  begin
    if BorderStyle = bsNone then
      SetFlatRect(true);
    Invalidate;
  end;
end;

procedure TAdvSmoothMaskEdit.WMChar(var Msg: TWMKey);
begin
  if (msg.charcode = VK_RETURN) and (FReturnIsTab) then
    Exit;
  inherited;
  if FShowModified then
    self.Font.Color := FModifiedColor;
end;

function TAdvSmoothMaskEdit.GetLabelCaption: string;
begin
  if FLabel <> nil then
    Result := FLabel.caption
  else
    Result := '';
end;

procedure TAdvSmoothMaskEdit.SetLabelCaption(const value: string);
begin
  if FLabel = nil then
    FLabel := CreateLabel;
  FLabel.Caption := value;
  UpdateLabel;
end;



function TAdvSmoothMaskEdit.CreateLabel: TLabel;
begin
  Result := TLabel.Create(Self);
  Result.Parent := Self.Parent;
  Result.FocusControl := Self;
  Result.Font.Assign(LabelFont);
  Result.ParentFont := self.ParentFont;
end;


procedure TAdvSmoothMaskEdit.SetLabelMargin(const Value: integer);
begin
  FLabelMargin := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvSmoothMaskEdit.SetLabelTransparent(const Value: boolean);
begin
  FLabelTransparent := Value;
  if FLabel <> nil then UpdateLabel;
end;


procedure TAdvSmoothMaskEdit.SetLabelPosition(const Value: TLabelPosition);
begin
  FLabelPosition := Value;
  if FLabel <> nil then UpdateLabel;
end;

procedure TAdvSmoothMaskEdit.UpdateLabel;
begin
  FLabel.Transparent := FLabeltransparent;

  if not ParentFont then
    FLabel.Font.Assign(FLabelFont);

  
  case FLabelPosition of
    lpLeftTop: begin
        FLabel.top := self.top;
        FLabel.left := self.left - FLabel.canvas.textwidth(FLabel.caption) - FLabelMargin;
      end;
    lpLeftCenter: begin
        FLabel.top := self.top + ((self.height - FLabel.height) shr 1);
        FLabel.left := self.left - FLabel.canvas.textwidth(FLabel.caption) - FLabelMargin;
      end;
    lpLeftBottom: begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - FLabel.canvas.textwidth(FLabel.caption) - FLabelMargin;
      end;
    lpTopLeft: begin
        FLabel.top := self.top - FLabel.height - FLabelMargin;
        FLabel.left := self.left;
      end;
    lpBottomLeft: begin
        FLabel.top := self.top + self.height + FLabelMargin;
        FLabel.left := self.left;
      end;
    lpLeftTopLeft: begin
        FLabel.top := self.top;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftCenterLeft: begin
        FLabel.top := self.top + ((self.height - FLabel.height) shr 1);
        FLabel.left := self.left - FLabelMargin;
      end;
    lpLeftBottomLeft: begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.left := self.left - FLabelMargin;
      end;
    lpRightTop: begin
        FLabel.Top := self.Top;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRightCenter: begin
        FLabel.top := self.top + ((self.height - FLabel.height) shr 1);
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
    lpRighBottom: begin
        FLabel.top := self.top + self.height - FLabel.height;
        FLabel.Left := self.Left + Self.Width + FLabelMargin;
      end;
  end;

  FLabel.Visible := Visible;
end;


destructor TAdvSmoothMaskEdit.Destroy;
begin
  if FLabel <> nil then
  begin
    FLabel.Parent := nil;
    FLabel.Free;
    FLabel := nil;
  end;
  FLabelFont.Free;
  inherited Destroy;
end;

procedure TAdvSmoothMaskEdit.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);

  if (FLabel <> nil) then
  begin
    if FLabel.Parent <> nil then
      UpdateLabel;
  end;
  
  if FFlat then
    SetFlatRect(FFlat);
end;

procedure TAdvSmoothMaskEdit.SetFlat(const Value: boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    if FFlat then
    begin
      if not (csLoading in ComponentState) then
        if FFlatParentColor then
          Color := (Parent as TWinControl).Brush.Color;
      Borderstyle := bsNone;
      SetFlatRect(True);
    end
    else
    begin
      Color := clWindow;
      BorderStyle := FOldBorder;
      SetFlatRect(False);
    end;
    Invalidate;
  end;
end;

procedure TAdvSmoothMaskEdit.CMFontChanged(var Message: TMessage);
begin
  if (csDesigning in ComponentState) or (csLoading in ComponentState) then
    if (FLabel <> nil) and ParentFont then
    begin
      FLabel.Font.Assign(Font);
    end;
  inherited;
  SetFlatRect(FFlat);
end;


procedure TAdvSmoothMaskEdit.WMNCPaint(var Message: TMessage);
begin
  inherited;
  if (FFocusBorderColor <> clNone) or not Enabled then
    DrawBorder;
end;

procedure TAdvSmoothMaskEdit.CMParentFontChanged(var Message: TMessage);
begin
  inherited;
  if Assigned(FLabel) and ParentFont then
  begin
    FLabel.Font.Assign(Font);
    UpdateLabel;
  end;
end;


procedure TAdvSmoothMaskEdit.WMPaint(var Msg: TWMPaint);
begin
  inherited;
  PaintEdit;
  if Border3D or (FFocusBorderColor <> clNone) then
    DrawBorder;
end;

procedure TAdvSmoothMaskEdit.WMKeyDown(var Msg: TWMKeydown);
begin
  if (msg.CharCode = VK_RETURN) and (FReturnIsTab) then
  begin
    msg.CharCode := VK_TAB;
    PostMessage(self.Handle, WM_KEYDOWN, VK_TAB, 0);
  end;
  if (msg.CharCode = VK_ESCAPE) and (Alignment <> taLeftJustify) then
  begin
    if CanUndo then
      self.Text := FOriginalValue;
    PostMessage(Parent.Handle, WM_KEYDOWN, VK_ESCAPE, 0);
  end;
  inherited;
end;

function TAdvSmoothMaskEdit.GetModified: boolean;
begin
  Result := inherited Modified;
end;

procedure TAdvSmoothMaskEdit.SetModified(const Value: boolean);
begin
  if FShowModified then
  begin
    if Value = False then
      self.Font.Color := FFontColor
    else
      self.Font.Color := FModifiedColor;
  end;

  inherited Modified := Value;
end;

procedure TAdvSmoothMaskEdit.SetDisabledColor(const Value: TColor);
begin
  FDisabledColor := Value;
  Invalidate;
end;

procedure TAdvSmoothMaskEdit.SetDisabledBorder(const Value: boolean);
begin
  FDisabledBorder := Value;
  Invalidate;
end;

function TAdvSmoothMaskEdit.GetEnabledEx: Boolean;
begin
  Result := inherited Enabled;
end;

procedure TAdvSmoothMaskEdit.SetEnabledEx(const Value: Boolean);
var
  OldValue: Boolean;
  OldColor: TColor;
begin
  OldValue := inherited Enabled;

  inherited Enabled := Value;

  if (csLoading in ComponentState) or
    (csDesigning in ComponentState) then Exit;

  if (OldValue <> Value) then
  begin
    if value then
    begin
      Color := FNormalColor;
    end
    else
    begin
      OldColor := Color;
      Color := FDisabledColor;
      FNormalColor := OldColor;
    end;
  end;

  if Assigned(FLabel) then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := Value;
end;

function TAdvSmoothMaskEdit.GetColorEx: TColor;
begin
  Result := inherited Color;
end;

procedure TAdvSmoothMaskEdit.SetColorEx(const Value: TColor);
begin
  if csLoading in ComponentState then
    FLoadedColor := Value;

  inherited Color := Value;
  if not (csLoading in ComponentState) then
    FNormalColor := Value;
end;

procedure TAdvSmoothMaskEdit.Loaded;
var
  FOldColor: TColor;
begin
  inherited Loaded;
  FFontColor := Font.Color;
  FOldBorder := BorderStyle;

  FFlat := not FFlat;
  SetFlat(not FFlat);

  if Assigned(FLabel) and not Enabled then
    if not FLabelAlwaysEnabled then
      FLabel.Enabled := False;

  inherited Color := FLoadedColor;
  FNormalColor := FLoadedColor;

  if FlatParentColor and Flat then
    Color := (Parent as TWinControl).Brush.Color;

  if not Enabled then
  begin
    FOldColor := Color;
    Color := FDisabledColor;
    FNormalColor := FOldColor;
  end;


  if (FLabel <> nil) then
    UpdateLabel;

  if ParentFont and Assigned(FLabel) then
    FLabel.Font.Assign(Font);

  if (FLabel <> nil) then
    UpdateLabel;
end;

procedure TAdvSmoothMaskEdit.SetLabelFont(const Value: TFont);
begin
  FLabelFont.Assign(Value);
  if Assigned(FLabel) then
    FLabel.Font.Assign(FLabelFont);
end;

procedure TAdvSmoothMaskEdit.LabelFontChanged(Sender: TObject);
begin
  if Assigned(FLabel) then
    FLabel.Font.Assign(FLabelFont);
end;

function GetAveCharSize(Canvas: TCanvas): TPoint;
var
  I: Integer;
  Buffer: array[0..51] of Char;
begin
  for I := 0 to 25 do Buffer[I] := Chr(I + Ord('A'));
  for I := 0 to 25 do Buffer[I + 26] := Chr(I + Ord('a'));

  GetTextExtentPoint(Canvas.Handle, Buffer, 52, TSize(Result));
  Result.X := Result.X div 52;

end;

function AdvInputQuery(const QueryType: tAdvSmoothEditType; QueryParams: PQueryParams; const ACaption, APrompt: string;
  var Value: string): Boolean;

  var
    Form: TForm;
    Prompt: TLabel;
    Edit: TAdvSmoothEdit;
    DialogUnits: TPoint;
    ButtonTop, ButtonWidth, ButtonHeight: Integer;
  begin
    Result := False;
    Form := TForm.Create(Application);
    with Form do
    try
      Canvas.Font := Font;
      DialogUnits := GetAveCharSize(Canvas);
      BorderStyle := bsDialog;
      Caption := ACaption;
      ClientWidth := MulDiv(180, DialogUnits.X, 4);
      ClientHeight := MulDiv(63, DialogUnits.Y, 8);
      Position := poScreenCenter;
      Prompt := TLabel.Create(Form);
      with Prompt do
      begin
        Parent := Form;
        AutoSize := True;
        Left := MulDiv(8, DialogUnits.X, 4);
        Top := MulDiv(8, DialogUnits.Y, 8);
        Caption := APrompt;
      end;
      Edit := TAdvSmoothEdit.Create(Form);
      with Edit do
      begin
        Parent := Form;
        Left := Prompt.Left;
        Top := MulDiv(19, DialogUnits.Y, 8);
        Width := MulDiv(164, DialogUnits.X, 4);
        MaxLength := 255;
        FocusColor := clNone;
        Init;
        DefaultHandling := false;

        Text := Value;
        SelectAll;
        EditType := QueryType;
        if QueryParams <> nil then
        begin
          Prefix := QueryParams^.Prefix;
          Suffix := QueryParams^.Suffix;
          Precision := QueryParams^.Precision;
          LengthLimit := QueryParams^.LengthLimit;
          Flat := QueryParams^.Flat;
          if Flat then Height := Height - 4;
        end;

      end;
      ButtonTop := MulDiv(41, DialogUnits.Y, 8);
      ButtonWidth := MulDiv(50, DialogUnits.X, 4);
      ButtonHeight := MulDiv(14, DialogUnits.Y, 8);
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgOK;

        ModalResult := mrOk;
        Default := True;
        TabStop := true;
        SetBounds(MulDiv(38, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;
      with TButton.Create(Form) do
      begin
        Parent := Form;
        Caption := SMsgDlgCancel;
        ModalResult := mrCancel;
        Cancel := True;
        TabStop := true;
        SetBounds(MulDiv(92, DialogUnits.X, 4), ButtonTop, ButtonWidth,
          ButtonHeight);
      end;

      if ShowModal = mrOk then
      begin
        Value := Edit.Text;
        Result := True;
      end;
    finally
      Form.Free;
    end;
  end;


  constructor TRangeList.Create;
  begin
    inherited Create;
  end;

  procedure TRangeList.SetInteger(Index: Integer; Value: Integer);
  begin
    inherited Items[Index] := Pointer(Value);
  end;

  function TRangeList.GetInteger(Index: Integer): Integer;
  begin
    Result := Integer(inherited Items[Index]);
  end;

  procedure TRangeList.Add(Value: Integer);
  begin
    if IndexOf(pointer(value)) = -1 then inherited Add(Pointer(Value));
  end;

  procedure TRangeList.AddMultiple(Value, Count: Integer);
  var
    i: Integer;
  begin
    for i := 1 to Count do Add(value + i - 1);
  end;


  procedure TRangeList.Delete(Index: Integer);
  begin
    inherited Delete(Index);
  end;

  function TRangeList.InList(value: integer): Boolean;
  begin
    Result := not (IndexOf(pointer(value)) = -1);
  end;

  procedure TRangeList.Show;
  var
    c: Integer;
  begin
    for c := 1 to Count do
      OutputDebugString(pchar(inttostr(Items[c - 1])));
  end;

  function TRangeList.StrToList(s: string): Boolean;
  var
    c, code: Integer;
    res: Boolean;

    function DoRange(s: string): Boolean;
    var
      i, i1, i2: Integer;
    begin
      Result := true;
      val(copy(s, 1, Pos('-', s) - 1), i1, code);
      if (code <> 0) then Result := false;
      val(copy(s, Pos('-', s) + 1, Length(s)), i2, code);
      if (code <> 0) then Result := false;
      if result then for i := i1 to i2 do Add(i);
    end;

    function SepPos(s: string): Integer;
    var
      p1, p2: Integer;
    begin
      p1 := Pos(',', s);
      p2 := Pos(';', s);

      if ((p1 < p2) and (p1 > 0)) or (p2 = 0) then Result := p1 else Result := p2;

    end;

  begin
    self.Clear;
    res := true;

    while (Length(s) > 0) do
    begin
      if SepPos(s) > 0 then
      begin
        if (Pos('-', s) < SepPos(s)) and (Pos('-', s) > 0) then
        begin
          if not DoRange(copy(s, 1, SepPos(s) - 1)) then res := false;
        end
        else
        begin
          val(copy(s, 1, SepPos(s) - 1), c, code);
          if (code <> 0) then res := false
          else Add(c);
        end;
        system.delete(s, 1, SepPos(s));
      end
      else
      begin
        if Pos('-', s) > 0 then
        begin
          if not DoRange(s) then res := false;
        end
        else
        begin
          val(s, c, code);
          if (code <> 0) then res := false
          else Add(c);
        end;
        s := '';
      end;
    end;
    Result := res;
  end;

function TAdvSmoothMaskEdit.GetVisible: Boolean;
begin
  Result := inherited Visible;
end;

procedure TAdvSmoothMaskEdit.SetVisible(const Value: Boolean);
begin
  inherited Visible := Value;
  if (FLabel <> nil) then
    FLabel.Visible := Value;
end;

procedure TAdvSmoothMaskEdit.DrawBorder;
var
  DC: HDC;
begin
  if Enabled and not (FFlat or (FFocusBorder and FMouseInControl) or Border3D or (FFocusBorderColor <> clNone)) then
    Exit;

  DC := GetWindowDC(Handle);
  try
    DrawControlBorder(DC);
  finally
    ReleaseDC(Handle, DC);
  end;
end;


procedure TAdvSmoothMaskEdit.DrawControlBorder(DC: HDC);
var
  ARect: TRect;
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

  if (FFocusBorderColor <> clNone) then
  begin
    if (GetFocus = self.Handle) then
    begin
      BtnFaceBrush := CreateSolidBrush(ColorToRGB(FFocusBorderColor));
      GetWindowRect(Handle, ARect);
      OffsetRect(ARect, -ARect.Left, -ARect.Top);
      FrameRect(DC, ARect, BtnFaceBrush);
      DeleteObject(BtnFaceBrush);
    end;
    Exit;
  end;

  if not Enabled then
    Exit;


  if Is3DBorderButton then
    BtnFaceBrush := CreateSolidBrush(GetSysColor(COLOR_BTNFACE))
  else
    BtnFaceBrush := CreateSolidBrush(ColorToRGB((parent as TWinControl).Brush.color));

  WindowBrush := CreateSolidBrush(GetSysColor(COLOR_WINDOW));

  try
    GetWindowRect(Handle, ARect);
    OffsetRect(ARect, -ARect.Left, -ARect.Top);
    if Is3DBorderButton then
    begin
      DrawEdge(DC, ARect, BDR_SUNKENOUTER, BF_RECT or BF_ADJUST);
      FrameRect(DC, ARect, BtnFaceBrush);
    end
    else
    begin
      FrameRect(DC, ARect, BtnFaceBrush);
      InflateRect(ARect, -1, -1);
      FrameRect(DC, ARect, BtnFaceBrush);
    end;
  finally
    DeleteObject(WindowBrush);
    DeleteObject(BtnFaceBrush);
  end;
end;

function TAdvSmoothMaskEdit.Is3DBorderButton: Boolean;
begin
  if csDesigning in ComponentState then
    Result := Enabled
  else
    Result := FMouseInControl or (Screen.ActiveControl = Self);

  Result := (Result and FFocusBorder) or (Border3D);
end;




procedure TAdvSmoothMaskEdit.SetFlatLineColor(const Value: TColor);
begin
  FFlatLineColor := Value;
  Invalidate;
end;

procedure TAdvSmoothMaskEdit.PaintEdit;
var
  DC: HDC;
  Oldpen: HPen;
  Loc: TRect;

begin

  if FFlat then
  begin
    DC := GetDC(Handle);

    if FFocusBorder then
      DrawControlBorder(DC)
    else
    begin
      OldPen := SelectObject(dc, CreatePen(PS_SOLID, 1, ColorToRGB(FFlatLineColor)));
      SendMessage(Handle, EM_GETRECT, 0, LParam(@Loc));

      if FSoftBorder then
      begin
        MovetoEx(DC, Loc.Left - 4, Height - 1, nil);
        LineTo(DC, Width - 1, Height - 1);
        LineTo(DC, Width - 1, Loc.Top - 3);
        LineTo(DC, Loc.Left - 4, Loc.Top - 3);
        LineTo(DC, Loc.Left - 4, Height - 1);
      end
      else
      begin
        MovetoEx(DC, Loc.Left - 2, Height - 1, nil);
        LineTo(DC, Width, Height - 1);
      end;

      DeleteObject(SelectObject(DC, OldPen));
    end;

    ReleaseDC(Handle, DC);
  end;
end;

procedure TAdvSmoothMaskEdit.SetFlatRect(const Value: Boolean);
var
  loc: TRect;
  lft: integer;
begin
  lft := 0;
  if not Ctl3D then
    lft := Font.Size div 3;

  if Value then
  begin
    if FSoftBorder then
      loc.Left := 4
    else
      loc.Left := 2;

    loc.Top := 4 + lft;
    loc.Right := Clientrect.Right - 2;
    loc.Bottom := Clientrect.Bottom - 4;
  end
  else
  begin
    loc.Left := lft;
    loc.Top := 0;
    loc.Right := ClientRect.Right;
    loc.Bottom := ClientRect.Bottom;
  end;

  SendMessage(Handle, EM_SETRECTNP, 0, LParam(@loc));
end;

procedure TAdvSmoothMaskEdit.SetSoftBorder(const Value: Boolean);
begin
  FSoftBorder := Value;
  Invalidate;
end;

procedure TAdvSmoothMaskEdit.SetBorder3D(const Value: Boolean);
begin
  FBorder3D := Value;
end;

procedure TAdvSmoothMaskEdit.SetFlatParentColor(const Value: Boolean);
begin
  FFlatParentColor := Value;
  Invalidate;
end;

procedure TAdvSmoothMaskEdit.CreateWnd;
begin
  inherited;
  SetFlatRect(FFlat);
end;

function TAdvSmoothMaskEdit.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothMaskEdit.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

procedure TAdvSmoothMaskEdit.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothMaskEdit.SetAutoFocus(const Value: boolean);
begin
  FAutoFocus := Value;
end;

{ TEditDropTarget }

constructor TEditDropTarget.Create(AEdit: TAdvSmoothEdit);
begin
  inherited Create;
  FAdvSmoothEdit := AEdit;
end;

procedure TEditDropTarget.DragMouseMove(pt: tpoint; var allow: boolean);
begin
  inherited;
  pt := FAdvSmoothEdit.ScreenToClient(pt);
  FAdvSmoothEdit.DrawCaretByCursor;
end;

procedure TEditDropTarget.DropText(pt: tpoint; s: string);
var
  isCopy: Boolean;
  uchar: Integer;

begin
  inherited;

// do not copy multiline text
  if Pos(#13, s) > 0 then s := copy(s, 1, Pos(#13, s) - 1);
  if Pos(#10, s) > 0 then s := copy(s, 1, Pos(#10, s) - 1);

  if (FAdvSmoothEdit.FIsDragSource) then
  begin
    uchar := FAdvSmoothEdit.CharFromPos(pt);
    if (uchar >= FAdvSmoothEdit.SelStart) and
      (uchar <= FAdvSmoothEdit.SelStart + fAdvSmoothEdit.SelLength) then
      Exit;
  end;

  isCopy := (getkeystate(vk_control) and $8000 = $8000);

  if (fAdvSmoothEdit.fIsDragSource) and not isCopy then
  begin
    fAdvSmoothEdit.ClearSelection;
  end;

  FAdvSmoothEdit.EraseCaret;
  FAdvSmoothEdit.SetCaretByCursor;
  FAdvSmoothEdit.SetSelTextBuf(pchar(s));
  FAdvSmoothEdit.Invalidate;
end;

procedure Initialize;
//var
//  Result: HRESULT;
begin
  //Result :=
  OleInitialize(nil);
  //Assert(Result in [S_OK, S_FALSE], Format('OleInitialize failed ($%x)', [Result]));
end;

{ TListHintWindow }
constructor TListHintWindow.Create(AOwner: TComponent);
begin
  inherited;
end;

procedure TListHintWindow.CreateParams(var Params: TCreateParams);
const
  CS_DROPSHADOW = $00020000;
begin
  inherited CreateParams(Params);
  Params.Style := Params.Style + WS_BORDER;

  if (Win32Platform = VER_PLATFORM_WIN32_NT) and
    ((Win32MajorVersion > 5) or
    ((Win32MajorVersion = 5) and (Win32MinorVersion >= 1))) then
    Params.WindowClass.Style := Params.WindowClass.Style or CS_DROPSHADOW;

  Params.ExStyle := Params.ExStyle or WS_EX_TOPMOST;
end;





destructor TListHintWindow.Destroy;
begin
  inherited;
end;

procedure TListHintWindow.WMActivate(var Message: TMessage);
begin
  inherited;
  if integer(Message.WParam) = integer(False) then
    Hide
  else
    if FListControl.CanFocus then
      FListControl.SetFocus;
end;

procedure TListHintWindow.WMNCButtonDown(var Message: TMessage);
begin
  inherited;
end;

procedure TListHintWindow.WMNCHitTest(var Message: TWMNCHitTest);
var
  pt: TPoint;
begin
// Make the hint sizable
  pt := ScreenToClient(Point(Message.XPos, Message.YPos));

  if (pt.X > Width - 10) and (pt.Y > Height - 10) then
    message.Result := HTBOTTOMRIGHT
end;

{ TLookupSettings }

procedure TLookupSettings.Assign(Source: TPersistent);
begin
if (Source is TLookupSettings) then
begin
  FCaseSensitive := (Source as TLookupSettings).CaseSensitive;
  FColor :=  (Source as TLookupSettings).Color;
  FDisplayCount :=  (Source as TLookupSettings).DisplayCount;
  FDisplayList.Assign((Source as TLookupSettings).DisplayList);
  FEnabled := (Source as TLookupSettings).Enabled;
  FHistory := (Source as TLookupSettings).History;
  FNumChars := (Source as TLookupSettings).NumChars;
  FValueList.Assign((Source as TLookupSettings).ValueList);
  FMulti := (Source as TLookupSettings).Multi;
  FSeparator := (Source as TLookupSettings).Separator;
end;
end;

constructor TLookupSettings.Create;
begin
  inherited Create;
  FDisplayList := TStringList.Create;
  FDisplayList.Sorted := true;
  FDisplayList.Duplicates :=  dupIgnore;
  FValueList := TStringList.Create;
  FColor := clWindow;
  FDisplayCount := 4;
  FNumChars := 2;
  FEnabled := False;
  FSeparator := ';';
end;

destructor TLookupSettings.Destroy;
begin
  FValueList.Free;
  FDisplayList.Free;
  inherited;
end;

procedure TLookupSettings.SetDisplayList(const Value: TStringList);
begin
  FDisplayList.Assign(Value);
end;

procedure TLookupSettings.SetNumChars(const Value: Integer);
begin
  if Value > 0 then
    FNumChars := Value
end;

procedure TLookupSettings.SetValueList(const Value: TStringList);
begin
  FValueList.Assign(Value);
end;




initialization
{$IFNDEF TMSDISABLEOLE}
  Initialize;
{$ENDIF}

finalization
{$IFNDEF TMSDISABLEOLE}
  OleUninitialize
{$ENDIF}




end.

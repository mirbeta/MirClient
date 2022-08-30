{***************************************************************************}
{ TAdvTimePickerDropDown components                                         }
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

unit AdvTimePickerDropDown;

interface

uses
  Classes, Windows, Graphics, Controls, Messages, AdvDropDown, ExtCtrls, SysUtils, Math, Dialogs, ComCtrls,
  AdvStyleIF, Types
  {$IFDEF TMSGDIPLUS}
  , AdvGDIP, ActiveX
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 3; // Build nr.

  // version history
  // v1.0.0.0 : First release
  // v1.0.0.1 : Fixed : GDI+ Memory leak
  // v1.0.0.2 : Fixed : Issue with EditorEnabled and changing time via editor
  // v1.1.0.0 : New : Event OnGetDropDownPos added
  // v1.1.0.1 : Fixed : Issue with setting time via dropdown spin control
  // v1.1.0.2 : Fixed : Issue with TDBAdvTimePickerDropDown and ShowSeconds = false
  // v1.1.0.3 : Improved : Handling null dates in TDBAdvTimePickerDropDown
  // v1.1.0.4 : Fixed : Issue with switching between null time & normal time and edit mask
  // v1.1.0.5 : Fixed : Issue with displaying hour numbers when TickMarks = tmHours
  // v1.1.1.0 : New : ReadOnly property added for TAdvWatch
  // v1.1.1.1 : Fixed : psShortLine & psArrowLine pointer style issue fixed
  // v1.1.1.2 : Fixed : Issue with Tickmarks = tmNone & hour display
  // v1.1.1.3 : Fixed : Issue with updating time from keyboard

type
  TWatchShape = (wsCircle, wsRect, wsRoundRect{, wsPentagon});
  TWatchBorderStyle = (wbLine, wbDoubleLine, wbNone);
  //TSecondFall = (sfSmooth, sfJump);
  THourMarkStyle = (hmsLine, hmsQuartDblLine);
  TTickMarks = (tmAll, tmHours, tmQuartHours, tmNone);
  TPointerStyle = (psLine, psShortLine, psPointer, psLineArrow);
  //TMinuteStyle = (msDot, msLine, msNone);
  TAMPMFrame = (apRect, apRoundRect);
  //TWatchLocation = (wlCenter, wlCustom);
  TWatchGradient = (wgRadial, wgVertical, wgDiagonalForward, wgDiagonalBackward);

  TWatchAppearance = class(TPersistent)
  private
    FSize: Integer;
    FHourPointer: TColor;
    FBackgroundColor: TColor;
    FMinutePointerShadow: TColor;
    FBackgroundColorTo: TColor;
    FHourPointerShadow: TColor;
    FSecondPointerShadow: TColor;
    FSecondPointer: TColor;
    FBorderColor: TColor;
    FMinutePointer: TColor;
    FHourFont: TFont;
    FAMPMFont: TFont;
    FOnChange: TNotifyEvent;
    FPicture: TPicture;
    FAMPMFrame: TAMPMFrame;
    FWatchShape: TWatchShape;
    //FMinuteStyle: TMinuteStyle;
    FPointerStyle: TPointerStyle;
    FBorderStyle: TWatchBorderStyle;
    FHourPointerSize: byte;
    FSecondPointerSize: byte;
    FMinutePointerSize: byte;
    FHourColor: TColor;
    FMinuteColor: TColor;
    FCenterPointSize: byte;
    FCenterPointColor: TColor;
    FCenterPointBorderColor: TColor;
    //FLocation: TWatchLocation;
    //FPosY: Integer;
    //FPosX: Integer;
    FBackgroundGradient: TWatchGradient;
    FRounding: Integer;
    FHourMarkLength: byte;
    FHourMarkWidth: byte;
    FMinuteMarkLength: byte;
    FMinuteMarkWidth: byte;
    FBorderWidth: Integer;
    FAMPMColor: TColor;
    FAMPMBorderColor: TColor;
    FCenterPointOuterBorderColor: TColor;
    FTickMarks: TTickMarks;
    FHourMarkStyle: THourMarkStyle;
    procedure PictureChanged(Sender: TObject);
    procedure SetAMPMFont(const Value: TFont);
    procedure SetBackgroundColor(const Value: TColor);
    procedure SetBackgroundColorTo(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetHourFont(const Value: TFont);
    procedure SetHourPointer(const Value: TColor);
    procedure SetHourPointerShadow(const Value: TColor);
    procedure SetMinutePointer(const Value: TColor);
    procedure SetMinutePointerShadow(const Value: TColor);
    procedure SetPicture(const Value: TPicture);
    procedure SetSecondPointer(const Value: TColor);
    procedure SetSecondPointerShadow(const Value: TColor);
    procedure SetSize(const Value: Integer);
    procedure SetAMPMFrame(const Value: TAMPMFrame);
    procedure SetWatchShape(const Value: TWatchShape);
    procedure SetBorderStyle(const Value: TWatchBorderStyle);
    //procedure SetMinuteStyle(const Value: TMinuteStyle);
    procedure SetPointerStyle(const Value: TPointerStyle);
    procedure SetHourPointerSize(const Value: byte);
    procedure SetMinutePointerSize(const Value: byte);
    procedure SetSecondPointerSize(const Value: byte);
    procedure SetHourColor(const Value: TColor);
    procedure SetMinuteColor(const Value: TColor);
    procedure SetCenterPointColor(const Value: TColor);
    procedure SetCenterPointSize(const Value: byte);
    procedure SetCenterPointBorderColor(const Value: TColor);
    {procedure SetLocation(const Value: TWatchLocation);
    procedure SetPosX(const Value: Integer);
    procedure SetPosY(const Value: Integer);}
    procedure SetBackgroundGradient(const Value: TWatchGradient);
    procedure SetRounding(const Value: Integer);
    procedure SetHourMarkLength(const Value: byte);
    procedure SetMinuteMarkLength(const Value: byte);
    procedure SetHourMarkWidth(const Value: byte);
    procedure SetMinuteMarkWidth(const Value: byte);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetCenterPointOuterBorderColor(const Value: TColor);
    procedure SetTickMarks(const Value: TTickMarks);
    procedure SetHourMarkStyle(const Value: THourMarkStyle);
  protected
    procedure Changed;
    procedure InitializeDefault;

    property Rounding: Integer read FRounding write SetRounding default 5;
    property BorderStyle: TWatchBorderStyle read FBorderStyle write SetBorderStyle;
    property SecondPointerShadow: TColor read FSecondPointerShadow write SetSecondPointerShadow; //(clNone = no shadow)
  public
    constructor Create;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property AMPMFont: TFont read FAMPMFont write SetAMPMFont;
    property AMPMBorderColor: TColor read FAMPMBorderColor write FAMPMBorderColor default clBlack;
    property AMPMColor: TColor read FAMPMColor write FAMPMColor default clWhite;
    property AMPMFrame: TAMPMFrame read FAMPMFrame write SetAMPMFrame default apRect;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clBlack;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 4;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property BackgroundColorTo: TColor read FBackgroundColorTo write SetBackgroundColorTo default clNone;
    property BackgroundGradient: TWatchGradient read FBackgroundGradient write SetBackgroundGradient default wgVertical;
    property CenterPointBorderColor: TColor read FCenterPointBorderColor write SetCenterPointBorderColor default clNone;
    property CenterPointOuterBorderColor: TColor read FCenterPointOuterBorderColor write SetCenterPointOuterBorderColor default clNone;
    property CenterPointColor: TColor read FCenterPointColor write SetCenterPointColor default clBlack;
    property CenterPointSize: byte read FCenterPointSize write SetCenterPointSize default 4;
    property HourFont: TFont read FHourFont write SetHourFont;
    property HourColor: TColor read FHourColor write SetHourColor default clBlack;
    //property HourMarks: THourMarks read FHourMarks write SetHourMarks;
    property HourMarkLength: byte read FHourMarkLength write SetHourMarkLength default 0; // 0 = Auto size
    property HourMarkWidth: byte read FHourMarkWidth write SetHourMarkWidth default 1;
    property HourMarkStyle: THourMarkStyle read FHourMarkStyle write SetHourMarkStyle default hmsLine;
    property HourPointer: TColor read FHourPointer write SetHourPointer;
    property HourPointerShadow: TColor read FHourPointerShadow write SetHourPointerShadow; //(clNone = no shadow)
    property HourPointerSize: byte read FHourPointerSize write SetHourPointerSize default 4;
    property MinuteColor: TColor read FMinuteColor write SetMinuteColor default clGray;
    property MinutePointer: TColor read FMinutePointer write SetMinutePointer default clBlack;
    property MinutePointerShadow: TColor read FMinutePointerShadow write SetMinutePointerShadow; //(clNone = no shadow)
    property MinutePointerSize: byte read FMinutePointerSize write SetMinutePointerSize default 3;
    //property MinuteStyle: TMinuteStyle read FMinuteStyle write SetMinuteStyle;
    property MinuteMarkLength: byte read FMinuteMarkLength write SetMinuteMarkLength default 0; // 0 = Auto size
    property MinuteMarkWidth: byte read FMinuteMarkWidth write SetMinuteMarkWidth default 1;
    property Picture: TPicture read FPicture write SetPicture; // sets background picture
    property PointerStyle: TPointerStyle read FPointerStyle write SetPointerStyle default psLine;
    property SecondPointer: TColor read FSecondPointer write SetSecondPointer default clRed;
    property SecondPointerSize: byte read FSecondPointerSize write SetSecondPointerSize default 1;
    property Shape: TWatchShape read FWatchShape write SetWatchShape default wsCircle;
    property Size: Integer read FSize write SetSize default 0;  // 0 = full size
    property TickMarks: TTickMarks read FTickMarks write SetTickMarks default tmAll;
    {property Location: TWatchLocation read FLocation write SetLocation default wlCenter;
    property PosX: Integer read FPosX write SetPosX default 0;
    property PosY: Integer read FPosY write SetPosY default 0;
    }
  end;

  TWatchSettings = class(TPersistent)
  private
    FAuto: Boolean;
    FSeconds: Boolean;
    FTimeOffset: Integer;
    FOnChange: TNotifyEvent;
    //FSecondFall: TSecondFall;
    procedure SetAuto(const Value: Boolean);
    //procedure SetSecondFall(const Value: TSecondFall);
    procedure SetTimeOffset(const Value: Integer);
    procedure SetSeconds(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  published
    property Auto: Boolean read FAuto write SetAuto default False;
    property TimeOffset: Integer read FTimeOffset write SetTimeOffset default 0;  // Time offset from current time
    //property SecondFall: TSecondFall read FSecondFall write SetSecondFall;
    property Second: Boolean read FSeconds write SetSeconds default True;
  end;

  TWatchStyle = (wsClassic, wsTower, wsFluorescent, wsEmerald, wsBlueStar, wsFuchsia, wsBlack, wsBlackWhite, wsGray, wsSuperNova, wsSports, wsSmooth);
  // Independent Watch Control

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvWatch = class(TCustomControl, ITMSStyle)
  private
    FAppearance: TWatchAppearance;
    FSettings: TWatchSettings;
    FTime: TDateTime;
    FBKGCache: TBitmap;
    FOldHour: Word;
    FOldMinute: Word;
    FOldSecond: Word;
    FWatchTimer: TTimer;
    FAnimation: Boolean;
    FAnimationFactor: integer;
    FAnimationTimer: TTimer;
    FCurrentHourTo: byte;
    FCurrentMinuteTo: byte;
    FMinuteFrom: byte;
    FHourAnimating: Boolean;
    FInternalCall: Boolean;
    FAM: Boolean;
    FShowAMPM: Boolean;
    FShowNumbers: Boolean;
    FOnChange: TNotifyEvent;
    FInternalSet: Boolean;
    FStyle: TWatchStyle;
    FReadOnly: boolean;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure WMKeyDown(var Message: TWMKeyDown); message WM_KEYDOWN;
    procedure WMGetDlgCode(var Message: TMessage); message WM_GETDLGCODE;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure OnAppearanceChanged(Sender: TObject);
    procedure OnSettingsChanged(Sender: TObject);
    procedure OnWatchTimerTime(Sender: TObject);
    procedure OnAnimationTimerTime(Sender: TObject);
    procedure SetAppearance(const Value: TWatchAppearance);
    procedure SetSettings(const Value: TWatchSettings);
    procedure SetTime(const Value: TDateTime);
    procedure SetTimeDirect(Value: TDateTime);
    procedure SetAnimation(const Value: Boolean);
    procedure SetAnimationFactor(const Value: integer);
    procedure SetAM(Value: Boolean);
    procedure SetShowAMPM(Value: Boolean);
    procedure SetShowNumbers(Value: Boolean);
    procedure SetStyle(const Value: TWatchStyle);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
  protected
    procedure Paint; override;
    procedure Loaded; override;
    procedure Resize; override;
    function GetVersionNr: Integer; virtual;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;

    procedure UpdateCache;
  {$IFDEF TMSGDIPLUS}
    procedure DrawBackground(g: TGPGraphics; aCanvas: TCanvas; R: TRect); // background color or image
  {$ELSE}
    procedure DrawBackground(aCanvas: TCanvas; R: TRect); // background color or image
  {$ENDIF}

  {$IFDEF TMSGDIPLUS}
    procedure DrawTickMarks(g: TGPGraphics; aCanvas: TCanvas);
    procedure DrawNumbers(g: TGPGraphics; aCanvas: TCanvas);
    procedure DrawContent(g: TGPGraphics; aCanvas: TCanvas);    // ie: Hour and minute marks, numbers etc (alarm bell in future)
  {$ELSE}
    procedure DrawTickMarks(aCanvas: TCanvas);
    procedure DrawNumbers(aCanvas: TCanvas);
    procedure DrawContent(aCanvas: TCanvas);    // ie: Hour and minute marks, numbers etc (alarm bell in future)
  {$ENDIF}

  {$IFDEF TMSGDIPLUS}
    procedure DrawAMPM(g: TGPGraphics; aCanvas: TCanvas);       // Draw AMPM frame
  {$ELSE}
    procedure DrawAMPM(aCanvas: TCanvas);       // Draw AMPM frame
  {$ENDIF}

  {$IFDEF TMSGDIPLUS}
    procedure DrawNeedles(g: TGPGraphics; aCanvas: TCanvas);    // All needels ie: Hour, Munite, sec
  {$ELSE}
    procedure DrawNeedles(aCanvas: TCanvas);    // All needels ie: Hour, Munite, sec
  {$ENDIF}
    procedure RepaintTime;                      // Draw needles on cache image

    function GetRadius: Double;
    function GetWatchRect: TRect;        // main watch rect using size
    function GetWatchBorderWidth: Integer;
    function GetInnerRect: TRect;        // WatchRect - Border width
    function GetAMPMRect: TRect;
    function GetCenterPoint: TPoint;


    function GetDisplayTime: TDateTime;  // with offsettime
    procedure InvalidateWatch;
    procedure HandleKey(Key: Char);   // can be public for stand alone version

    property AM: Boolean read FAM write SetAM; // False = PM
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    procedure SetMinute(Value: byte);
    procedure SetHour(Value: byte);
    function XYToHour(X, Y: Integer): Integer;
    function XYToMinute(X, Y: Integer): Integer;
  published
    property Align;
    property Anchors;
    property Animation: Boolean read FAnimation write SetAnimation default true;
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 10;
    property Appearance: TWatchAppearance read FAppearance write SetAppearance;
    property DockSite;
    property DragMode;
    property DragKind;
    property DragCursor;
    property PopupMenu;
    property ReadOnly: boolean read FReadOnly write FReadOnly default False;
    property Settings: TWatchSettings read FSettings write SetSettings;
    property Time: TDateTime read FTime write SetTime;
    property ShowAMPM: Boolean read FShowAMPM write SetShowAMPM default True;
    property ShowHint;
    property ShowNumbers: Boolean read FShowNumbers write SetShowNumbers default True;
    property Style: TWatchStyle read FStyle write SetStyle default wsClassic;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    {$IFDEF DELPHIXE_LVL}
    property OnAlignInsertBefore;
    property OnAlignPosition;
    property OnConstrainedResize;
    {$ENDIF}
    property OnCanResize;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDockDrop;
    property OnDockOver;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    {$IFDEF DELPHI2006_LVL}
    property OnMouseLeave;
    property OnMouseEnter;
    {$ENDIF}
    property OnResize;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
    property Version: string read GetVersion write SetVersion;
  end;

	TTimeChangeEvent = procedure (Sender: TObject; Hour, Min, Sec: Integer) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvTimePickerDropDown = class(TAdvCustomDropDown)
  private
    FShowTimePicker: Boolean;
    FAdvWatch: TAdvWatch;
    FDateTimePicker: TDateTimePicker;
    FPanelCtrl: TPanel;
    FWatchAppearance: TWatchAppearance;
    FShowWatchAMPM: Boolean;
    FShowWatchNumbers: Boolean;
    FAnimation: Boolean;
    FAnimationFactor: integer;
    FTime: TDateTime;
    FWatchSettings: TWatchSettings;
    FOnTimeChange: TNotifyEvent;
    FInternalCall: Boolean;
    FShowSeconds: Boolean;
    procedure OnWatchAppearanceChange(Sender: TObject);
    procedure SetWatchAppearance(const Value: TWatchAppearance);
    procedure OnAdvWatchChange(Sender: TObject);
    procedure OnDateTimePickerChange(Sender: TObject);
    procedure SetTime(const Value: TDateTime);
    procedure SetWatchSettings(const Value: TWatchSettings);
    procedure OnDateTimePickerKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnDateTimePickerKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OnDateTimePickerKeyPress(Sender: TObject; var Key: Char);
    procedure SetMinute(Value: byte);
    procedure SetHour(Value: byte);
    procedure SetSecond(Value: byte);
    function GetFormattedTimeString(h, m, s: word): String;
    procedure SetShowSeconds(const Value: Boolean);
  protected
    procedure CreateDropDownForm; override;
    procedure BeforeDropDown; override;
    procedure OnHideDropDown; override;
    procedure UpdateDropDownSize; override;
    procedure OnDropDownFormKeyPress(var Key: Char); override;
    procedure OnDropDownFormKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownFormKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure DoHideDropDown(Canceled: Boolean); override;
    procedure SetText(Value: string); override;
    procedure Change; override;
    procedure DoExit; override;
    procedure OnDropDownControlKeyDown(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyUp(var Key: Word; Shift: TShiftState); override;
    procedure OnDropDownControlKeyPress(var Key: Char); override;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure InitEditMask;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle); override;
    destructor Destroy; override;
  published
    property Animation: Boolean read FAnimation write FAnimation default True;
    property AnimationFactor: integer read FAnimationFactor write FAnimationFactor default 10;
    property WatchSettings: TWatchSettings read FWatchSettings write SetWatchSettings;
    property Time: TDateTime read FTime write SetTime;
    property ShowWatchAMPM: Boolean read FShowWatchAMPM write FShowWatchAMPM default True;
    property ShowWatchNumbers: Boolean read FShowWatchNumbers write FShowWatchNumbers default True;
    property ShowTimePicker: Boolean read FShowTimePicker write FShowTimePicker default False;
    property WatchAppearance: TWatchAppearance read FWatchAppearance write SetWatchAppearance;
    property ShowSeconds: Boolean read FShowSeconds write SetShowSeconds default True;

    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind;
    property BevelOuter;
    property BevelWidth;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Constraints;
    property Ctl3D;
    property BorderColor;
    property DisabledBorder;
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
    property DropDownBorderColor;
    property DropDownBorderWidth;
    property DropDownShadow;
    property DropDownWidth;
    property DropDownHeight;
    property DropPosition;
    property DropDownButtonWidth;
    property DropDownButtonHint;
    property DropDownButtonGlyph;
    property DropDownSizeable;
    property EditorEnabled default False;
    property Enabled;
    property Font;
    property Images;
    property Version;
    property ButtonAppearance;
    property DropDownHeader;
    property DropDownFooter;
    property DragCursor;
    property DragKind;
    property DragMode;

    property LabelCaption;
    property LabelPosition;
    property LabelMargin;
    property LabelTransparent;
    property LabelAlwaysEnabled;
    property LabelFont;

    property TabStop;
    property TabOrder;

    property OnTimeChange: TNotifyEvent read FOnTimeChange write FOnTimeChange;
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
    property OnDropUp;
    property OnDropDownHeaderButtonClick;
    property OnDropDownFooterButtonClick;
    property OnDrawHeader;
    property OnDrawFooter;
    property OnGetHeaderText;
    property OnGetFooterText;
    property OnGetDropDownPos;
  end;

implementation

uses
  Forms;

{$I DELPHIXE.INC}

//------------------------------------------------------------------------------

{$IFDEF TMSGDIPLUS}
function DrawGDIPText(Canvas: TCanvas; g: TGPGraphics; Alignment: TAlignment; r: TRect; Caption:string; WideCaption: widestring; AFont: TFont; Enabled: Boolean; RealDraw: Boolean; AntiAlias: TAntiAlias): TRect;
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
begin
  Result := Rect(0, 0, 0, 0);
  if not Assigned(g) and not Assigned(Canvas) then
    Exit;
    
  if (Caption <> '') or (WideCaption <> '') then
  begin
    graphics := g;
    if not Assigned(graphics) then
      graphics := TGPGraphics.Create(Canvas.Handle);
    fontFamily:= TGPFontFamily.Create(AFont.Name);

    if (fontFamily.Status in [FontFamilyNotFound, FontStyleNotFound]) then
    begin
      fontFamily.Free;
      fontFamily := TGPFontFamily.Create('Arial');
    end;

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

    rectf := MakeRect(x1,y1,x2,y2);

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

    stringFormat.SetLineAlignment(StringAlignmentNear);
    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    stringFormat.SetTrimming(StringTrimmingNone);


    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    // graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

    if (AntiAlias = aaNone) and Assigned(Canvas) then
    begin
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + 2;

      if (Caption <> '') then
        szRect.Bottom := DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK)
      else
        szRect.Bottom := DrawTextW(Canvas.Handle,PWideChar(WideCaption),Length(WideCaption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK);

      sizeRect.X := szRect.Left;
      sizeRect.Y := szRect.Top;
      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;
    end
    else
    begin
      fillchar(sizerect,sizeof(sizerect),0);

      if (Caption <> '') then
        graphics.MeasureString(Caption, Length(Caption), font, rectf, stringFormat, sizerect)
      else
        graphics.MeasureString(WideCaption, Length(WideCaption), font, rectf, stringFormat, sizerect)
    end;

    Result := Rect(round(sizerect.X), Round(sizerect.Y), Round(sizerect.X + sizerect.Width), Round(sizerect.Y + sizerect.Height));
    rectf := MakeRect(x1,y1,x2,y2);

    if RealDraw then
    begin
      if (AntiAlias = aaNone) and Assigned(Canvas) then
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
        if Caption <> '' then
          DrawText(Canvas.Handle,PChar(Caption),Length(Caption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE)
        else
          DrawTextW(Canvas.Handle,PWideChar(WideCaption),Length(WideCaption), szrect, DTFLAG or DT_VCENTER or DT_SINGLELINE)
      end
      else
      begin
        if (Caption <> '') then
          graphics.DrawString(Caption, Length(Caption), font, rectf, stringFormat, solidBrush)
        else
          graphics.DrawString(WideCaption, Length(WideCaption), font, rectf, stringFormat, solidBrush)
      end;
    end;
    stringformat.Free;
    solidBrush.Free;
    font.Free;
    fontfamily.Free;
    if not Assigned(g) then
      graphics.Free;
  end;
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

procedure FillRoundRect(graphics: TGPGraphics; Brsh: TGPBrush; X,Y,Width,Height,Radius: integer);
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
  graphics.FillPath(brsh, path);
  path.Free;
end;

//------------------------------------------------------------------------------

function CreateRoundRectangle(R: TRect; Radius: Integer): TGPGraphicsPath;
var
  l, t, w, h, d: Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;
  d := Radius shl 1;
  Result.AddArc(l, t, d, d, 180, 90); // topleft
  Result.AddLine(l + radius, t, l + w - radius, t); // top
  Result.AddArc(l + w - d, t, d, d, 270, 90); // topright
  Result.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  Result.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  Result.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
  Result.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  Result.AddLine(l, t + h - radius, l, t + radius); // left
  Result.CloseFigure();
end;

//------------------------------------------------------------------------------

function CreateGDIPRectangle(R: TRect): TGPGraphicsPath;
var
  l, t, w, h: Integer;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;
  Result.AddLine(l, t, l + w, t); // top
  Result.AddLine(l + w, t, l + w, t + h); // right
  Result.AddLine(l + w, t + h, l, t + h); // bottom
  Result.AddLine(l, t + h, l, t); // left
  Result.CloseFigure();
end;

//------------------------------------------------------------------------------

procedure DrawGDIPImage(gr: TGPGraphics; Canvas: TCanvas; R: TRect; bmp: TGraphic; Stretch: Boolean; Transparent: Boolean = False);
var
  Img: TGPImage;
  pstm: IStream;
  hGlobal: THandle;
  pcbWrite: Longint;
  ms: TMemoryStream;
  graphics: TGPGraphics;
  ImageAttributes: TGPImageAttributes;
  rc, gc, bc: byte;
  GPBmp: TGPBitmap;
  Aclr: TGPColor;
  hr: HResult;
begin
  if (not Assigned(gr) and not Assigned(Canvas)) then
    Exit;

  graphics := gr;
  if not Assigned(graphics) then
  begin
    graphics := TGPGraphics.Create(Canvas.Handle);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  end;

  ms := TMemoryStream.Create;
  bmp.SaveToStream(ms);
  hGlobal := GlobalAlloc(GMEM_MOVEABLE, ms.Size);
  if (hGlobal = 0) then
  begin
    ms.Free;
    raise Exception.Create('Could not allocate memory for image');
  end;

  pstm := nil;
  pcbWrite := 0;

  // Create IStream* from global memory
  hr := CreateStreamOnHGlobal(hGlobal, TRUE, pstm);

  if hr = S_OK then
  begin
    pstm.Write(ms.Memory, ms.Size,@pcbWrite);

    if (ms.Size = pcbWrite) then
    begin
      Img := TGPImage.Create(pstm);

      if Transparent and (Img.GetFormat <> ifPNG) then
      begin
        GPBmp := TGPBitmap.Create(pstm);
        GPBmp.GetPixel(0, 0, AClr);
        GPBmp.Free;

        rc := ADVGDIP.GetRed(AClr);
        gc := ADVGDIP.GetGreen(AClr);
        bc := ADVGDIP.GetBlue(AClr);

        ImageAttributes := TGPImageAttributes.Create;
        ImageAttributes.SetColorKey(MakeColor(rc, gc, bc), MakeColor(rc, gc, bc), ColorAdjustTypeDefault);
        if Stretch then
        begin
          graphics.DrawImage(Img, MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top),  // destination rectangle
          0, 0,        // upper-left corner of source rectangle
          Img.GetWidth,       // width of source rectangle
          Img.GetHeight,      // height of source rectangle
          UnitPixel,
          ImageAttributes);
        end
        else
          graphics.DrawImage(Img, MakeRect(R.Left, R.Top, Img.GetWidth, Img.Getheight),  // destination rectangle
            0, 0,        // upper-left corner of source rectangle
            Img.GetWidth,       // width of source rectangle
            Img.GetHeight,      // height of source rectangle
            UnitPixel,
            ImageAttributes);

        ImageAttributes.Free;
      end
      else
        graphics.DrawImage(Img, R.Left, R.Top);

      Img.Free;
    end;
    pstm := nil;
  end
  else
    GlobalFree(hGlobal);

  ms.Free;

  if not Assigned(gr) then
    graphics.Free;
end;

{$ENDIF}

//------------------------------------------------------------------------------

function PtInEllipse(P: TPoint; x1, y1, x2, y2: Integer): Boolean;
var
  Rgn: HRGN;
begin
  Rgn := CreateEllipticRgn(x1, y1, x2, y2);
  Result := PtInRegion(Rgn, P.X, P.Y);
  DeleteObject(Rgn);
end;

//------------------------------------------------------------------------------

procedure DrawPic(Canvas: TCanvas; Pic: TPicture; R: TRect; Transparent: Boolean);
var
  bmp: TBitmap;
begin
  if Assigned(Pic) and Assigned(Pic.Graphic) and not Pic.Graphic.Empty and Assigned(Canvas) then
  begin
    bmp := TBitmap.Create;
    try
      bmp.Height := R.Bottom - R.Top;
      bmp.Width := R.Right - R.Left;
      bmp.TransparentMode := tmAuto;
      bmp.Transparent := True;

      bmp.Canvas.Draw(0, 0, Pic.Graphic);

      Canvas.StretchDraw(R, bmp);
    finally
      bmp.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvTimePickerDropDown }

constructor TAdvTimePickerDropDown.Create(AOwner: TComponent);
begin
  FWatchAppearance := TWatchAppearance.Create;
  FWatchAppearance.OnChange := OnWatchAppearanceChange;
  inherited;
  FShowTimePicker := False;
  //EditType := etString;
  EditorEnabled := False;
  DropDownEnabled := True;
  FShowSeconds := True;
  FInternalCall := true;
  InitEditMask;
  SetTextDirect('00' + TimeSeparator + '00' + TimeSeparator + '00');
  FInternalCall := false;
  FWatchSettings := TWatchSettings.Create;
  FShowWatchAMPM := True;
  FShowWatchNumbers := True;
  FAnimation := True;
  FAnimationFactor := 10;
  DropDownHeader.Visible := False;
  DropDownFooter.Visible := False;
end;

//------------------------------------------------------------------------------

destructor TAdvTimePickerDropDown.Destroy;
begin
  FWatchAppearance.Free;
  FWatchSettings.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.BeforeDropDown;
begin
  inherited;
  FPanelCtrl.Height := 170;
  FPanelCtrl.Width := 170;
  FDropDownForm.Width := FDropDownForm.Width + 1;
  FDropDownForm.Width := FDropDownForm.Width - 1;
  FDropDownForm.CancelOnDeActivate := False;

  FPanelCtrl.Color := DropDownColor;
  FAdvWatch.Appearance.Assign(FWatchAppearance);
  FAdvWatch.Settings.Assign(FWatchSettings);
  FAdvWatch.ShowAMPM := FShowWatchAMPM;
  FAdvWatch.ShowNumbers := FShowWatchNumbers;
  FAdvWatch.Animation := FAnimation;
  FAdvWatch.AnimationFactor := FAnimationFactor;
  FAdvWatch.OnChange := nil;
  if not WatchSettings.Auto then
    FAdvWatch.Time := Time;

  //if EditorEnabled then
  if (Text <> '') then
    FAdvWatch.Time := StrToTime(Text)
  else
    FAdvWatch.Time := 0;

  FAdvWatch.OnChange := OnAdvWatchChange;

  if ShowTimePicker then
  begin
    if not Assigned(FDateTimePicker) then
    begin
      FDateTimePicker := TDateTimePicker.Create(Self);
      FDateTimePicker.Parent := FPanelCtrl;
      FDateTimePicker.Kind := dtkTime;
    end;
    FDateTimePicker.OnChange := nil;
    FDateTimePicker.Align := alBottom;
    FDateTimePicker.Visible := True;
    FDateTimePicker.Time := FAdvWatch.Time;
    FDateTimePicker.BringToFront;
    FDateTimePicker.OnChange := OnDateTimePickerChange;
    FDateTimePicker.OnKeyDown := OnDateTimePickerKeyDown;
    FDateTimePicker.OnKeyUp := OnDateTimePickerKeyUp;
    FDateTimePicker.OnKeyPress := OnDateTimePickerKeyPress;

    if ShowSeconds then
      FDateTimePicker.Format := 'HH:mm:ss'
    else
      FDateTimePicker.Format := 'HH:mm';

    FPanelCtrl.Height := 170 + FDateTimePicker.Height;
  end
  else if Assigned(FDateTimePicker) then
  begin
    FDateTimePicker.Visible := False;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.CreateDropDownForm;
begin
  inherited;

  if not Assigned(FPanelCtrl) then
  begin
    FPanelCtrl := TPanel.Create(Self);
    FPanelCtrl.Parent := FDropDownForm;
    FPanelCtrl.Ctl3D := False;
    FPanelCtrl.BorderWidth := 0;
    //FPanelCtrl.BevelOuter := bvNone;  // uncomment to make it flat
    FPanelCtrl.BevelInner := bvNone;
  end;

  FPanelCtrl.Left := 0;
  FPanelCtrl.Top := 0;
  FPanelCtrl.Height := 180;

  if not Assigned(FAdvWatch) then
  begin
    FAdvWatch := TAdvWatch.Create(Self);
    FAdvWatch.Parent := FPanelCtrl;
  end;
  FAdvWatch.Align := alClient;

  Control := FPanelCtrl;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.DoExit;
begin
  inherited;
  FInternalCall := false;
  Change;
  FInternalCall := true;
end;

procedure TAdvTimePickerDropDown.DoHideDropDown(Canceled: Boolean);
begin
  inherited;
  if Canceled then
    FAdvWatch.Time := Time
  else
    Time := FAdvWatch.Time;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetComponentStyle(AStyle: TTMSStyle);
begin
  inherited;
  WatchAppearance.CenterPointOuterBorderColor := clBlack;
  WatchAppearance.HourFont.Color := clBlack;
  WatchAppearance.AMPMFont.Color := clBlack;
  case AStyle of
    tsOffice2003Blue:
    begin
      WatchAppearance.AMPMBorderColor := $962D00;
      WatchAppearance.AMPMColor := $D68759;
      WatchAppearance.BackgroundColor := $00FFD2AF;
      WatchAppearance.BackgroundColorTo := $00FFD2AF;
      WatchAppearance.BorderColor := $D68759;
      WatchAppearance.CenterPointBorderColor := $D68759;
      WatchAppearance.CenterPointColor := $D68759;
      WatchAppearance.HourColor := $D68759;
      WatchAppearance.MinuteColor := $D68759;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $D68759;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $D68759;
      WatchAppearance.SecondPointer := clWhite;
    end;
    tsOffice2003Silver:
    begin
      WatchAppearance.AMPMBorderColor := $962D00;
      WatchAppearance.AMPMColor := $BDA4A5;
      WatchAppearance.BackgroundColor := $00E6D8D8;
      WatchAppearance.BackgroundColorTo := $00E6D8D8;
      WatchAppearance.BorderColor := $BDA4A5;
      WatchAppearance.CenterPointBorderColor := $BDA4A5;
      WatchAppearance.CenterPointColor := $BDA4A5;
      WatchAppearance.HourColor := $BDA4A5;
      WatchAppearance.MinuteColor := $BDA4A5;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $BDA4A5;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $BDA4A5;
      WatchAppearance.SecondPointer := clWhite;
    end;
    tsOffice2003Olive:
    begin
      WatchAppearance.AMPMBorderColor := $962D00;
      WatchAppearance.AMPMColor := $82C0AF;
      WatchAppearance.BackgroundColor := RGB(225, 234, 185);
      WatchAppearance.BackgroundColorTo := RGB(225, 234, 185);
      WatchAppearance.BorderColor := $82C0AF;
      WatchAppearance.CenterPointBorderColor := $82C0AF;
      WatchAppearance.CenterPointColor := $82C0AF;
      WatchAppearance.HourColor := $82C0AF;
      WatchAppearance.MinuteColor := $82C0AF;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $82C0AF;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $82C0AF;
      WatchAppearance.SecondPointer := clWhite;
    end;
    tsOffice2003Classic:
    begin
      WatchAppearance.AMPMBorderColor := $962D00;
      WatchAppearance.AMPMColor := $808080;
      WatchAppearance.BackgroundColor := $00F2F2F2;
      WatchAppearance.BackgroundColorTo := $00F2F2F2;
      WatchAppearance.BorderColor := $808080;
      WatchAppearance.CenterPointBorderColor := $808080;
      WatchAppearance.CenterPointColor := $808080;
      WatchAppearance.HourColor := $808080;
      WatchAppearance.MinuteColor := $808080;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $808080;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $808080;
      WatchAppearance.SecondPointer := $808080;
    end;
    tsOffice2007Luna:
    begin
      WatchAppearance.AMPMBorderColor := $00F0DED0;
      WatchAppearance.AMPMColor := $00F0DED0;
      WatchAppearance.BackgroundColor := $FFEFE3;
      WatchAppearance.BackgroundColorTo := $FFEFE3;
      WatchAppearance.BorderColor := $00F0DED0;
      WatchAppearance.CenterPointBorderColor := $FFEFE3;
      WatchAppearance.CenterPointColor := $FFEFE3;
      WatchAppearance.HourColor := $723708;
      WatchAppearance.MinuteColor := $723708;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $723708;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $723708;
      WatchAppearance.SecondPointer := $723708;
    end;
    tsOffice2007Obsidian:
    begin
      WatchAppearance.AMPMBorderColor := $F2F1F0;
      WatchAppearance.AMPMColor := $5C534C;
      WatchAppearance.AMPMFont.Color := $F2F1F0;
      WatchAppearance.BackgroundColor := $5C534C;
      WatchAppearance.BackgroundColorTo := $5C534C;
      WatchAppearance.BorderColor := $F2F1F0;
      WatchAppearance.CenterPointBorderColor := $FFEFE3;
      WatchAppearance.CenterPointColor := $FFEFE3;
      WatchAppearance.HourColor := $F2F1F0;
      WatchAppearance.MinuteColor := $F2F1F0;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $F2F1F0;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $F2F1F0;
      WatchAppearance.SecondPointer := $F2F1F0;
      WatchAppearance.HourFont.Color := $F2F1F0;
    end;
    tsWindowsXP:
    begin
      WatchAppearance.AMPMBorderColor := clBlack;
      WatchAppearance.AMPMColor := clBtnFace;
      WatchAppearance.BackgroundColor := clBtnFace;
      WatchAppearance.BackgroundColorTo := clBtnFace;
      WatchAppearance.BorderColor := clBlack;
      WatchAppearance.CenterPointBorderColor := clBlack;
      WatchAppearance.CenterPointColor := clBlack;
      WatchAppearance.HourColor := clBlack;
      WatchAppearance.MinuteColor := clBlack;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := clBlack;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := clBlack;
      WatchAppearance.SecondPointer := clBlack;
    end;
    tsWhidbey:
    begin
      WatchAppearance.AMPMBorderColor := $7E9898;
      WatchAppearance.AMPMColor := $F5F9FA;
      WatchAppearance.BackgroundColor := $F5F9FA;
      WatchAppearance.BackgroundColorTo := $F5F9FA;
      WatchAppearance.BorderColor := $7E9898;
      WatchAppearance.CenterPointBorderColor := $7E9898;
      WatchAppearance.CenterPointColor := $7E9898;
      WatchAppearance.HourColor := $7E9898;
      WatchAppearance.MinuteColor := $7E9898;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $7E9898;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $7E9898;
      WatchAppearance.SecondPointer := $7E9898;
    end;
    tsOffice2007Silver:
    begin
      WatchAppearance.AMPMBorderColor := $E8E0DB;
      WatchAppearance.AMPMColor := RGB(241, 244, 248);
      WatchAppearance.BackgroundColor := RGB(241, 244, 248);
      WatchAppearance.BackgroundColorTo := RGB(241, 244, 248);
      WatchAppearance.BorderColor := $E8E0DB;
      WatchAppearance.CenterPointBorderColor := $E8E0DB;
      WatchAppearance.CenterPointColor := $74706F;
      WatchAppearance.HourColor := $74706F;
      WatchAppearance.MinuteColor := $74706F;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $74706F;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $74706F;
      WatchAppearance.SecondPointer := $74706F;
    end;
    tsWindowsVista:
    begin
      WatchAppearance.AMPMBorderColor := $FEDF9A;
      WatchAppearance.AMPMColor := $FFFDF9;
      WatchAppearance.BackgroundColor := $FFFDF9;
      WatchAppearance.BackgroundColorTo := $FFFAF0;
      WatchAppearance.BorderColor := $FEDF9A;
      WatchAppearance.CenterPointBorderColor := $FEDF9A;
      WatchAppearance.CenterPointColor := $FEDF9A;
      WatchAppearance.HourColor := $FEDF9A;
      WatchAppearance.MinuteColor := $FEDF9A;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $FEDF9A;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $FEDF9A;
      WatchAppearance.SecondPointer := $FEDF9A;
    end;
    tsWindows7:
    begin
      WatchAppearance.AMPMBorderColor := $FBD6B8;
      WatchAppearance.AMPMColor := $FDFBFA;
      WatchAppearance.BackgroundColor := $FDFBFA;
      WatchAppearance.BackgroundColorTo := $FDF3EB;
      WatchAppearance.BorderColor := $FBD6B8;
      WatchAppearance.CenterPointBorderColor := $FBD6B8;
      WatchAppearance.CenterPointColor := $FBD6B8;
      WatchAppearance.HourColor := $FBD6B8;
      WatchAppearance.MinuteColor := $FBD6B8;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $FBD6B8;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $FBD6B8;
      WatchAppearance.SecondPointer := $FBD6B8;
    end;
    tsTerminal:
    begin
      WatchAppearance.AMPMBorderColor := clGray;
      WatchAppearance.AMPMColor := clbtnface;
      WatchAppearance.BackgroundColor := clBtnFace;
      WatchAppearance.BackgroundColorTo := clBtnFace;
      WatchAppearance.BorderColor := clGray;
      WatchAppearance.CenterPointBorderColor := clGray;
      WatchAppearance.CenterPointColor := clGray;
      WatchAppearance.HourColor := clGray;
      WatchAppearance.MinuteColor := clGray;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := clGray;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := clGray;
      WatchAppearance.SecondPointer := clGray;
    end;
    tsOffice2010Blue:
    begin
      WatchAppearance.AMPMBorderColor := $C7B29F;
      WatchAppearance.AMPMColor := $F0DAC7;
      WatchAppearance.BackgroundColor := $FDF6EF;
      WatchAppearance.BackgroundColorTo := $FDF6EF;
      WatchAppearance.BorderColor := $C7B29F;
      WatchAppearance.CenterPointBorderColor := $C7B29F;
      WatchAppearance.CenterPointColor := $C7B29F;
      WatchAppearance.HourColor := $C7B29F;
      WatchAppearance.MinuteColor := $C7B29F;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $C7B29F;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $C7B29F;
      WatchAppearance.SecondPointer := $C7B29F;
    end;
    tsOffice2010Silver:
    begin
      WatchAppearance.AMPMBorderColor := $D2CDC8;
      WatchAppearance.AMPMColor := $EDE5E0;
      WatchAppearance.BackgroundColor := $FFFFFF;
      WatchAppearance.BackgroundColorTo := $FFFFFF;
      WatchAppearance.BorderColor := $D2CDC8;
      WatchAppearance.CenterPointBorderColor := $D2CDC8;
      WatchAppearance.CenterPointColor := $D2CDC8;
      WatchAppearance.HourColor := $D2CDC8;
      WatchAppearance.MinuteColor := $D2CDC8;
      WatchAppearance.HourPointerShadow := clSilver;
      WatchAppearance.HourPointer := $D2CDC8;
      WatchAppearance.MinutePointerShadow := clSilver;
      WatchAppearance.MinutePointer := $D2CDC8;
      WatchAppearance.SecondPointer := $D2CDC8;
    end;
    tsOffice2010Black:
    begin
      WatchAppearance.AMPMBorderColor := $6D6D6D;
      WatchAppearance.AMPMColor := $919191;
      WatchAppearance.BackgroundColor := $BFBFBF;
      WatchAppearance.BackgroundColorTo := $BFBFBF;
      WatchAppearance.BorderColor := $6D6D6D;
      WatchAppearance.CenterPointBorderColor := $6D6D6D;
      WatchAppearance.CenterPointColor := $6D6D6D;
      WatchAppearance.HourColor := $6D6D6D;
      WatchAppearance.MinuteColor := $6D6D6D;
      WatchAppearance.HourPointerShadow := clWhite;
      WatchAppearance.HourPointer := $6D6D6D;
      WatchAppearance.MinutePointerShadow := clWhite;
      WatchAppearance.MinutePointer := $6D6D6D;
      WatchAppearance.SecondPointer := $6D6D6D;
    end;
  end;
end;

procedure TAdvTimePickerDropDown.SetHour(Value: byte);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  dt := Time;
  DecodeTime(dT, h, m, s, ms);
  if (h <> Value) and (Value < 24) then
  begin
    h := Value;
    Time := EncodeTime(h, m, s, ms);
    if Assigned(FOnTimeChange) then
      FOnTimeChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetMinute(Value: byte);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  dt := Time;
  DecodeTime(dT, h, m, s, ms);

  if (m <> Value) and (Value < 60) then
  begin
    m := Value;
    Time := EncodeTime(h, m, s, ms);

    if Assigned(FOnTimeChange) then
      FOnTimeChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetSecond(Value: byte);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  dt := Time;
  DecodeTime(dT, h, m, s, ms);

  if (s <> Value) and (Value < 60) then
  begin
    s := Value;
    Time := EncodeTime(h, m, s, ms);

    if Assigned(FOnTimeChange) then
      FOnTimeChange(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.KeyDown(var Key: Word; Shift: TShiftState);
var
  s1:string;
  ss: integer;
  ssl: integer;
  OrgKey: Word;
  i: Integer;
  h, m, s, ms: word;
  IsAlt: Boolean;
begin
  OrgKey := Key;
  inherited;
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  if IsAlt or (ssAlt in Shift) or (ssCtrl in Shift) then
    Exit;

  case OrgKey of
  VK_DOWN:
    begin
      s1 := Text;
      ss := SelStart;
      ssl := SelStart;
      DecodeTime(Time, h, m, s, ms);
      
      if (ss < pos(TimeSeparator, s1)) then
      begin
        i := h + 1;
        if (i >= 24) then
          i := 0;
        SetHour(i);
      end
      else
      begin
        ss := ss - pos(TimeSeparator, s1);
        Delete(s1, 1, pos(TimeSeparator, s1));
        if (ss < pos(TimeSeparator,s1)) or not ShowSeconds then
        begin
          i := m + 1;
          if (i >= 60) then
            i := 0;
          SetMinute(i);
        end
        else
        begin
          i := s + 1;
          if (i >= 60) then
            i := 0;
          SetSecond(i);
        end;
      end;

      SelStart := ssl;
    end;
  VK_UP:
    begin
      s1 := Text;
      ss := SelStart;
      ssl := SelStart;
      DecodeTime(Time, h, m, s, ms);
      
      if (ss < pos(TimeSeparator, s1)) then
      begin
        i := h - 1;
        if (i < 0) then
          i := 23;
        SetHour(i);
      end
      else
      begin
        ss := ss - pos(TimeSeparator, s1);
        Delete(s1, 1, pos(TimeSeparator, s1));
        if (ss < pos(TimeSeparator,s1)) or not ShowSeconds then
        begin
          i := m - 1;
          if (i < 0) then
            i := 59;
          SetMinute(i);
        end
        else
        begin
          i := s - 1;
          if (i < 0) then
            i := 59;
          SetSecond(i);
        end;
      end;

      SelStart := ssl;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnHideDropDown;
begin
  inherited;
  
  Time := FAdvWatch.Time;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.UpdateDropDownSize;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDateTimePickerChange(Sender: TObject);
var
  h, m, s, ms: word;
  st: string;
begin
  if not Assigned(FDateTimePicker) then
    Exit;

  if Assigned(FAdvWatch) and not WatchSettings.Auto then
  begin
    FAdvWatch.Time := FDateTimePicker.Time;

    DecodeTime(FDateTimePicker.Time, h, m, s, ms);

    st := GetFormattedTimeString(h, m, s);
    if (st <> Text) then
    begin
      FInternalCall := True;
      Text := st;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDateTimePickerKeyDown(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  OnDropDownControlKeyDown(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDateTimePickerKeyPress(Sender: TObject;
  var Key: Char);
begin
  OnDropDownControlKeyPress(Key);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDateTimePickerKeyUp(Sender: TObject;
  var Key: Word; Shift: TShiftState);
begin
  OnDropDownControlKeyUp(Key, Shift);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetWatchAppearance(
  const Value: TWatchAppearance);
begin
  FWatchAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnWatchAppearanceChange(Sender: TObject);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.InitEditMask;
begin
  if FShowSeconds then
    EditMask := '!90' + TimeSeparator + '00' + TimeSeparator + '00;1; '
  else
    EditMask := '!90' + TimeSeparator + '00;1; ';
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetText(Value: string);
begin
  if Value = '' then
  begin
    FInternalCall := true;
    EditMask := '';
    FInternalCall := false;
  end
  else
  begin
    FInternalCall := true;
    InitEditMask;
    FInternalCall := false;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetShowSeconds(const Value: Boolean);
var
  t: TDateTime;
begin
  if (FShowSeconds <> Value) then
  begin
    FShowSeconds := Value;
    t := Self.Time;
    FInternalCall := True;
    EditMask := '';
    Text := '';
    FInternalCall := False;

    InitEditMask;

    FTime := FTime + 1;
    Time := t;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.Change;

  function IsValidNum(s: string): Boolean;
  begin
    Result := True;
    if (Length(s) >= 1) then
      Result := (Integer(s[1]) >= 48) and (Integer(s[1]) <= 57);
    if Result and (Length(s) > 1) then
      Result := (Integer(s[2]) >= 48) and (Integer(s[2]) <= 57);
  end;

  function IsValidText(s: string): Boolean;
  var
    i: Integer;
  begin
    Result := False;
    i := Pos(TimeSeparator, s);
    if (i > 0) then
      s := trim(copy(s, i + 1, Length(s)))
    else
      Exit;
    i := Pos(TimeSeparator, s);
    if (i > 0) then
  begin
    if not ShowSeconds then
      Exit;
      s := trim(copy(s, i + 1, Length(s)))
  end
    else
  begin
    if ShowSeconds then
      Exit;
  end;
    i := Pos(TimeSeparator, s);
    if (i > 0) then
      Exit;
    
    Result := True;
  end;

  function FillEmptySpace(s: string): string;
  var
    i: Integer;
  begin
    Result := '';
    for i := 1 to Length(s) do
    begin
      if (s[i] <> ' ') then
        Result := Result + s[i]
      else
        Result := Result + '0';
    end;
  end;

var
  h, m, s, ms: word;
  st, Txt: string;
begin
  inherited;

  if (Text <> '') {and (length(Trim(Text)) = 5)} and not FInternalCall then
  begin
    {st := trim(copy(Text, 1, 2));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then    
      h := StrToInt(st)
    else
      h := 0;
    st := Trim(copy(Text, 4, 2));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then
      m := StrToInt(st)
    else
      m := 0;
    st := Trim(copy(Text, 7, 2));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then
      s := StrToInt(st)
    else
      s := 0;}

    if not IsValidText(Text) then
      Exit;

    Txt := FillEmptySpace(Text);

    st := Trim(Copy(Txt, 1, Pos(TimeSeparator, Txt)-1));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then
      h := StrToInt(st)
    else
      h := 0;

    st := trim(copy(Txt, Pos(TimeSeparator, Txt)+1, Length(Txt)));
    if ShowSeconds then    
      st := trim(copy(st, 1, Pos(TimeSeparator, st)-1));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then
      m := StrToInt(st)
    else
      m := 0;

    s := 0;
    if ShowSeconds then
    begin
      st := trim(copy(Txt, Pos(TimeSeparator, Txt) + 1, Length(Txt)));
      st := trim(copy(st, Pos(TimeSeparator, st) + 1, Length(st)));
      st := trim(copy(st, 1, Length(st)));
    if not IsValidNum(st) then
      Exit;
    if (st <> '') then
        s := StrToInt(st);
    end;

    ms := 0;
    FTime := EncodeTime(h, m, s, ms);
    st := GetFormattedTimeString(h, m, s);
    if (st <> Text) and (m = 0) and (s = 0) then
    begin
      FInternalCall := True;
      Text := st;
      FInternalCall := False;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvTimePickerDropDown.GetFormattedTimeString(h, m, s: word): String;

  function GetTwoDigitString(w: word): string;
  begin
    Result := InttoStr(w);
    if (Length(Result) = 0) then
      Result := '00'
    else if (Length(Result) = 1) then
      Result := '0' + Result;
  end;

begin
  if ShowSeconds then
    Result := GetTwoDigitString(h) + TimeSeparator + GetTwoDigitString(m) + TimeSeparator + GetTwoDigitString(s)
  else
    Result := GetTwoDigitString(h) + TimeSeparator + GetTwoDigitString(m);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetTime(const Value: TDateTime);
var
  h, m, s, ms: word;
  st: string;
begin
  if (FTime <> Value) then
  begin
    if EditorEnabled and (Value <> 0) and (EditMask = '') then
    begin
      FInternalCall := true;
      InitEditMask;
      FInternalCall := false;
    end;

    FTime := Value;
    DecodeTime(FTime, h, m, s, ms);
    //SetTextDirect(IntToStr(h) + TimeSeparator + IntToStr(m) + TimeSeparator + IntToStr(s));
    //SetTextDirect(GetTwoDigitString(h) + TimeSeparator + GetTwoDigitString(m) + TimeSeparator + GetTwoDigitString(s));
    st := GetFormattedTimeString(h, m, s);
    SetTextDirect(st);

    if DroppedDown then
    begin
      if Assigned(FAdvWatch) then
        FAdvWatch.Time := Value;

      if ShowTimePicker and Assigned(FDateTimePicker) then
        FDateTimePicker.Time := Value;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.SetWatchSettings(
  const Value: TWatchSettings);
begin
  FWatchSettings.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnAdvWatchChange(Sender: TObject);
begin
  if Assigned(FDateTimePicker) and ShowTimePicker then
  begin
    if (FDateTimePicker.Time <> FAdvWatch.Time) then
    begin
      FDateTimePicker.Time := FAdvWatch.Time;

      if Assigned(FOnTimeChange) then
        FOnTimeChange(Self);
    end;
  end;
  
  Time := FAdvWatch.Time;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownFormKeyPress(var Key: Char);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownFormKeyUp(var Key: Word; Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownFormKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;

  if Assigned(FDropDownForm) and FDropDownForm.Visible and Assigned(FAdvWatch) then
    FAdvWatch.HandleKey(Char(key));
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownControlKeyDown(var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownControlKeyPress(var Key: Char);
var
  IsAlt: Boolean;
begin
  IsAlt := GetKeyState(VK_MENU) and $8000 = $8000;
  if not (Integer(Key) in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or IsAlt then
    inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvTimePickerDropDown.OnDropDownControlKeyUp(var Key: Word;
  Shift: TShiftState);
begin
  if not (Key in [VK_DOWN, VK_UP, VK_PRIOR, VK_NEXT]) or (ssAlt in Shift) then
    inherited;
end;

//------------------------------------------------------------------------------

{ TWatchAppearance }

constructor TWatchAppearance.Create;
begin
  inherited;
  {FLocation := wlCenter;
  FPosX := 0;
  FPosY := 0;
  }
  FHourFont := nil;
  FAMPMFont := nil;
  FPicture := nil;
  InitializeDefault;
end;

//------------------------------------------------------------------------------

destructor TWatchAppearance.Destroy;
begin
  FHourFont.Free;
  FAMPMFont.Free;
  FPicture.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.InitializeDefault;
begin
  FRounding := 5;
  FSize := 0;
  FHourPointer := clBlack;
  FBackgroundColor := clWhite;
  FBackgroundColorTo := clNone;
  FMinutePointerShadow := clBtnShadow;
  FHourPointerShadow := FMinutePointerShadow;
  FSecondPointerShadow := FMinutePointerShadow;
  FSecondPointer := clRed;
  FBorderColor := clBlack;
  FBorderWidth := 4;
  FMinutePointer := clBlack;
  if not Assigned(FHourFont) then
    FHourFont := TFont.Create;

  FHourFont.Name := 'Tahoma';
  FHourFont.Size := 8;
  if not Assigned(FAMPMFont) then
    FAMPMFont := TFont.Create;

  FAMPMFont.Size := 10;
  FAMPMFont.Name := 'Tahoma';
  FAMPMColor := clWhite;
  FAMPMBorderColor := clBlack;
  if not Assigned(FPicture) then
    FPicture := TPicture.Create;

  FPicture.OnChange := PictureChanged;
  FAMPMFrame := apRect;
  FWatchShape := wsCircle;
  //FHourMarks := hmQuart;
  //FHourStyle := hsNumLine;
  //FMinuteStyle := msLine;
  FPointerStyle := psLine;
  FBorderStyle := wbLine;
  FHourPointerSize := 4;
  FSecondPointerSize := 1;
  FMinutePointerSize := 3;
  FHourColor := clBlack;
  FMinuteColor := clGray;
  FCenterPointSize := 4;
  FCenterPointColor := clBlack;
  FCenterPointBorderColor := clNone;
  FCenterPointOuterBorderColor := clNone;
  FBackgroundGradient := wgVertical;
  FHourMarkLength := 0;
  FHourMarkWidth := 1;
  FMinuteMarkLength := 0;
  FMinuteMarkWidth := 1;
  FTickMarks := tmAll;
  FHourMarkStyle := hmsLine;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.Assign(Source: TPersistent);
begin
  if (Source is TWatchAppearance) then
  begin
    FSize := (Source as TWatchAppearance).FSize;
    FHourPointer := (Source as TWatchAppearance).FHourPointer;
    FBackgroundColor := (Source as TWatchAppearance).FBackgroundColor;
    FMinutePointerShadow := (Source as TWatchAppearance).FMinutePointerShadow;
    FBackgroundColorTo := (Source as TWatchAppearance).FBackgroundColorTo;
    FHourPointerShadow := (Source as TWatchAppearance).FHourPointerShadow;
    FSecondPointerShadow := (Source as TWatchAppearance).FSecondPointerShadow;
    FSecondPointer := (Source as TWatchAppearance).FSecondPointer;
    FBorderColor := (Source as TWatchAppearance).FBorderColor;
    FMinutePointer := (Source as TWatchAppearance).FMinutePointer;
    FHourFont.Assign((Source as TWatchAppearance).FHourFont);
    FAMPMFont.Assign((Source as TWatchAppearance).FAMPMFont);
    FPicture.Assign((Source as TWatchAppearance).FPicture);
    FAMPMFrame := (Source as TWatchAppearance).FAMPMFrame;
    FWatchShape := (Source as TWatchAppearance).FWatchShape;
    FPointerStyle := (Source as TWatchAppearance).FPointerStyle;
    FBorderStyle := (Source as TWatchAppearance).FBorderStyle;
    FHourPointerSize := (Source as TWatchAppearance).FHourPointerSize;
    FSecondPointerSize := (Source as TWatchAppearance).FSecondPointerSize;
    FMinutePointerSize := (Source as TWatchAppearance).FMinutePointerSize;
    FHourColor := (Source as TWatchAppearance).FHourColor;
    FMinuteColor := (Source as TWatchAppearance).FMinuteColor;
    FCenterPointSize := (Source as TWatchAppearance).FCenterPointSize;
    FCenterPointColor := (Source as TWatchAppearance).FCenterPointColor;
    FCenterPointBorderColor := (Source as TWatchAppearance).FCenterPointBorderColor;
    FBackgroundGradient := (Source as TWatchAppearance).FBackgroundGradient;
    FRounding := (Source as TWatchAppearance).FRounding;
    FHourMarkLength := (Source as TWatchAppearance).FHourMarkLength;
    FHourMarkWidth := (Source as TWatchAppearance).FHourMarkWidth;
    FMinuteMarkLength := (Source as TWatchAppearance).FMinuteMarkLength;
    FMinuteMarkWidth := (Source as TWatchAppearance).FMinuteMarkWidth;
    FBorderWidth := (Source as TWatchAppearance).FBorderWidth;
    FAMPMColor := (Source as TWatchAppearance).FAMPMColor;
    FAMPMBorderColor := (Source as TWatchAppearance).FAMPMBorderColor;
    CenterPointOuterBorderColor := (Source as TWatchAppearance).FCenterPointOuterBorderColor;
    TickMarks := (Source as TWatchAppearance).FTickMarks;
    HourMarkStyle := (Source as TWatchAppearance).FHourMarkStyle;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetAMPMFont(const Value: TFont);
begin
  FAMPMFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBackgroundColor(const Value: TColor);
begin
  if (FBackgroundColor <> Value) then
  begin
    FBackgroundColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBackgroundColorTo(const Value: TColor);
begin
  if (FBackgroundColorTo <> Value) then
  begin
    FBackgroundColorTo := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourFont(const Value: TFont);
begin
  FHourFont.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourPointer(const Value: TColor);
begin
  if (FHourPointer <> Value) then
  begin
    FHourPointer := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourPointerShadow(const Value: TColor);
begin
  if (FHourPointerShadow <> Value) then
  begin
    FHourPointerShadow := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinutePointer(const Value: TColor);
begin
  if (FMinutePointer <> Value) then
  begin
    FMinutePointer := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinutePointerShadow(const Value: TColor);
begin
  if (FMinutePointerShadow <> Value) then
  begin
    FMinutePointerShadow := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetSecondPointer(const Value: TColor);
begin
  if (FSecondPointer <> Value) then
  begin
    FSecondPointer := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetSecondPointerShadow(const Value: TColor);
begin
  if (FSecondPointerShadow <> Value) then
  begin
    FSecondPointerShadow := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetSize(const Value: Integer);
begin
  if (FSize <> Value) then
  begin
    FSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetAMPMFrame(const Value: TAMPMFrame);
begin
  if (FAMPMFrame <> Value) then
  begin
    FAMPMFrame := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetWatchShape(const Value: TWatchShape);
begin
  if (FWatchShape <> Value) then
  begin
    FWatchShape := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBorderStyle(const Value: TWatchBorderStyle);
begin
  if (FBorderStyle <> Value) then
  begin
    FBorderStyle := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{
procedure TWatchAppearance.SetMinuteStyle(const Value: TMinuteStyle);
begin
  FMinuteStyle := Value;
end;
}

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetPointerStyle(const Value: TPointerStyle);
begin
  if (FPointerStyle <> Value) then
  begin
    FPointerStyle := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourPointerSize(const Value: byte);
begin
  if (FHourPointerSize <> Value) then
  begin
    FHourPointerSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinutePointerSize(const Value: byte);
begin
  if (FMinutePointerSize <> Value) then
  begin
    FMinutePointerSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetSecondPointerSize(const Value: byte);
begin
  if (FSecondPointerSize <> Value) then
  begin
    FSecondPointerSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourColor(const Value: TColor);
begin
  if (FHourColor <> Value) then
  begin
    FHourColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinuteColor(const Value: TColor);
begin
  if (FMinuteColor <> Value) then
  begin
    FMinuteColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetCenterPointColor(const Value: TColor);
begin
  if (FCenterPointColor <> Value) then
  begin
    FCenterPointColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetCenterPointSize(const Value: byte);
begin
  if (FCenterPointSize <> Value) then
  begin
    FCenterPointSize := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetCenterPointBorderColor(const Value: TColor);
begin
  if (FCenterPointBorderColor <> Value) then
  begin
    FCenterPointBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{
procedure TWatchAppearance.SetLocation(const Value: TWatchLocation);
begin
  FLocation := Value;
end;

procedure TWatchAppearance.SetPosX(const Value: Integer);
begin
  FPosX := Value;
end;

procedure TWatchAppearance.SetPosY(const Value: Integer);
begin
  FPosY := Value;
end;
}

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBackgroundGradient(
  const Value: TWatchGradient);
begin
  if (FBackgroundGradient <> Value) then
  begin
    FBackgroundGradient := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetRounding(const Value: Integer);
begin
  if (FRounding <> Value) then
  begin
    FRounding := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourMarkLength(const Value: byte);
begin
  if (FHourMarkLength <> Value) then
  begin
    FHourMarkLength := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinuteMarkLength(const Value: byte);
begin
  if (FMinuteMarkLength <> Value) then
  begin
    FMinuteMarkLength := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.PictureChanged(Sender: TObject);
begin
  Changed;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourMarkWidth(const Value: byte);
begin
  if (FHourMarkWidth <> Value) then
  begin
    FHourMarkWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetMinuteMarkWidth(const Value: byte);
begin
  if (FMinuteMarkWidth <> Value) then
  begin
    FMinuteMarkWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetBorderWidth(const Value: Integer);
begin
  if (FBorderWidth <> Value) then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetCenterPointOuterBorderColor(
  const Value: TColor);
begin
  if (FCenterPointOuterBorderColor <> Value) then
  begin
    FCenterPointOuterBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetTickMarks(const Value: TTickMarks);
begin
  if (FTickMarks <> Value) then
  begin
    FTickMarks := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchAppearance.SetHourMarkStyle(const Value: THourMarkStyle);
begin
  if (FHourMarkStyle <> Value) then
  begin
    FHourMarkStyle := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TWatchSettings }

constructor TWatchSettings.Create;
begin
  inherited;
  FSeconds := True;
  //FSecondFall := sfSmooth;
  FTimeOffset := 0;
end;

//------------------------------------------------------------------------------

procedure TWatchSettings.Assign(Source: TPersistent);
begin
  if (Source is TWatchSettings) then
  begin
    FSeconds := (Source as TWatchSettings).FSeconds;
    FTimeOffset := (Source as TWatchSettings).FTimeOffset;
    Auto := (Source as TWatchSettings).FAuto;
  end
  else
    inherited;
end;

//------------------------------------------------------------------------------

procedure TWatchSettings.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TWatchSettings.SetAuto(const Value: Boolean);
begin
  if (FAuto <> Value) then
  begin
    FAuto := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{
procedure TWatchSettings.SetSecondFall(const Value: TSecondFall);
begin
  if (FSecondFall <> Value) then
  begin
    FSecondFall := Value;
    Changed;
  end;
end;
}

//------------------------------------------------------------------------------

procedure TWatchSettings.SetTimeOffset(const Value: Integer);
begin
  if (FTimeOffset <> Value) then
  begin
    FTimeOffset := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TWatchSettings.SetSeconds(const Value: Boolean);
begin
  if (FSeconds <> Value) then
  begin
    FSeconds := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvWatch }

constructor TAdvWatch.Create(AOwner: TComponent);
begin
  inherited;
  FBKGCache := TBitmap.Create;
  FBKGCache.Width := Width;
  FBKGCache.Height := Height;

  FAppearance := TWatchAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;

  FSettings := TWatchSettings.Create;
  FSettings.OnChange := OnSettingsChanged;

  FWatchTimer := TTimer.Create(Self);
  FWatchTimer.Enabled:= Settings.Auto;
  FWatchTimer.Interval := 100;
  FWatchTimer.OnTimer := OnWatchTimerTime;

  FInternalCall := False;
  FAnimation := True;
  FAnimationFactor := 10;
  FanimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 1;
  FAnimationTimer.Enabled := False;
  FAnimationTimer.OnTimer := OnAnimationTimerTime;
  FAM := True;
  FShowAMPM := True;
  FShowNumbers := True;
  Width := 180;
  Height := 180;
  FBKGCache.Width := Width;
  FBKGCache.Height := Height;

  FStyle := wsClassic;
end;

//------------------------------------------------------------------------------

destructor TAdvWatch.Destroy;
begin
  FAnimationTimer.Free;
  FAppearance.Free;
  FSettings.Free;

  FBKGCache.Free;
  FWatchTimer.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.CMMouseEnter(var Msg: TMessage);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.CMMouseLeave(var Message: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TAdvWatch.GetCenterPoint: TPoint;
var
  R: TRect;
begin
  R := GetWatchRect;
  Result := Point(R.Left + (R.Right - R.Left) div 2, R.Top + (R.Bottom - R.Top) div 2);
end;

//------------------------------------------------------------------------------

function TAdvWatch.GetDisplayTime: TDateTime;
begin
  if Settings.Auto then
  begin
    Result := Now;
    Result := Result + (Settings.TimeOffset / (60 * 24));
  end
  else
    Result := Self.Time;
end;

//------------------------------------------------------------------------------

function TAdvWatch.GetInnerRect: TRect;
var
  i: Integer;
begin
  Result := GetWatchRect;
  i := GetWatchBorderWidth;
  InflateRect(Result, -i, -i);
end;

//------------------------------------------------------------------------------

function TAdvWatch.GetWatchBorderWidth: Integer;
begin
  Result := Appearance.BorderWidth;
end;

//------------------------------------------------------------------------------

function TAdvWatch.GetWatchRect: TRect;
var
  R: TRect;
  h, w, s, offset: Integer;
begin
  R := ClientRect;
  h := R.Bottom - R.Top;
  w := R.Right - R.Left;
  offset := 20;
  if (Appearance.Size > 0) then
  begin
    s := Appearance.Size;
    if (s > h) then
      s := h;
    if (s > w) then
      s := w;
  end
  else // full/auto size
  begin
    s := h;
    if (s > h) then
      s := h;
    if (s > w) then
      s := w;
    s := s - offset;
  end;

  Result := Rect(R.Left + ((w - s) div 2), R.Top + ((h - s) div 2), R.Left + ((w - s) div 2) + s, R.Top + ((h - s) div 2) + s);
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.InvalidateWatch;
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.Loaded;
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvWatch.MouseDown(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  R: TRect;
  i: Integer;
  h, m, s, ms: word;
begin
  inherited;
  if ReadOnly then
    Exit;
  if not Settings.Auto then
  begin
    R := GetWatchRect;
    if PtInEllipse(Point(X, Y), R.Left, R.Top, R.Right, R.Bottom) then
    begin
      R := GetAMPMRect;
      if PtInRect(R, Point(X, Y)) then
      begin
        if Animation then        
          AM := not AM;
        DecodeTime(Time, h, m, s, ms);
        FInternalCall := True;
        if (h < 12) then
          FCurrentHourTo := h + 12
        else
          FCurrentHourTo := h - 12;
        SetHour(FCurrentHourTo);
        FInternalCall := False;
      end
      else
      begin
        i := XYToHour(X, Y);
        SetHour(i);
      end;
    end
    else
    begin
      i := XYToMinute(X, Y);
      SetMinute(i);
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvWatch.XYToHour(X, Y: Integer): Integer;
begin
  Result := Round(XYToMinute(X, Y) / 5);

  if (Result <= 0) then
  begin
    if AM then
      Result := 0
    else
      Result := 12;
  end
  else
  begin
    if not AM then
      Result := Result + 12;
    if (Result = 24) then
      Result := 0;
  end;
end;

//------------------------------------------------------------------------------

function TAdvWatch.XYToMinute(X, Y: Integer): Integer;
var
  //R: TRect;
  CP: TPoint;
  a, b, m: Double;
begin
  //R := GetWatchRect;
  CP := GetCenterPoint;
  //BrW := GetWatchBorderWidth;

  a := abs(CP.Y - Y);
  b := abs(CP.X - X);
  //a := (Y - CP.Y);
  if (a = 0) then
    a := 1;
  //b := (X - CP.X);
  if (b = 0) then
    b := 1;

  m := ArcTan(a / b) * 180/pi;
  m := m / 6;
  Result := Round(m);

  if (X >= CP.X) and (Y >= CP.Y) then
    Result := Result + 15
  else if (X <= CP.X) and (Y >= CP.Y) then
  begin
    Result := 15 - Result;
    Result := Result + 30;
  end
  else if (X <= CP.X) and (Y <= CP.Y) then
    Result := Result + 45
  else if (X >= CP.X) and (Y <= CP.Y) then
    Result := 15 - Result;

  if (Result < 0) or (Result >= 60) then
    Result := 0;    
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.HandleKey(Key: Char);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  if Settings.Auto then
    Exit;

  dt := GetDisplayTime;
  DecodeTime(dT, h, m, s, ms);

  if (Key = 'A') or (key = 'a') then
  begin
    if h >= 12 then
      SetHour(h - 12);
  end;

  if (Key = 'P') or (key = 'p') then
  begin
    if h < 12 then
      SetHour(h + 12);
  end;

  case Integer(key) of
    VK_RIGHT:
    begin
      if (m + 1 >= 60) then
        SetMinute(0)
      else
        SetMinute(m + 1);
    end;
    VK_LEFT:
    begin
      if (m - 1 < 0) then
      begin
        //--- reduce hour
        if (h - 1 < 0) then
        begin
          //SetHour(11);
          h := 23;
        end
        else
        begin
          //SetHour(h - 1);
          h := h - 1;
        end;

        //FInternalCall := True;
        //SetMinute(59);
        //FInternalCall := False;
        m := 59;
        dt := EncodeTime(h, m, s, ms);
        Self.Time := dt;
      end
      else
        SetMinute(m - 1);
    end;
    VK_END: SetMinute(0);
    VK_HOME:
    begin
      FInternalCall := True;
      SetHour(12);
      FInternalCall := False;
    end;
    VK_PRIOR:
    begin
      if (h + 1 > 23) then
      begin
        FInternalCall := True;
        SetHour(0);
        FInternalCall := False;
      end
      else
        SetHour(h + 1);
    end;
    VK_NEXT:
    begin
      if (h - 1 < 0) then
      begin
        FInternalCall := True;
        SetHour(23);
        FInternalCall := False;
      end
      else
        SetHour(h - 1);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvWatch.OnAppearanceChanged(Sender: TObject);
begin
  InvalidateWatch;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.OnSettingsChanged(Sender: TObject);
var
  h, m, s, ms: word;
begin
  FWatchTimer.Enabled := Settings.Auto;
  if Settings.Auto then
  begin
    Time := Now;
    DecodeTime(Now, h, m, s, ms);
    FInternalSet := True;
    AM := (h < 12);
    FInternalSet := False;
  end
  else
  begin
  end;
  InvalidateWatch;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.OnWatchTimerTime(Sender: TObject);
begin
  RepaintTime;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.UpdateCache;
var
  R: TRect;
  {$IFDEF TMSGDIPLUS}
  g: TGPGraphics;
  {$ENDIF}
begin
  R := ClientRect; // GetWatchRect;

  {$IFDEF TMSGDIPLUS}
  //--- copying canvas
  FBKGCache.Width := R.Right - R.Left;
  FBKGCache.Height := R.Bottom - R.Top;
  FBKGCache.Canvas.CopyMode := cmSrcCopy;
  FBKGCache.Canvas.CopyRect(FBKGCache.Canvas.ClipRect, Canvas, R);

  g := TGPGraphics.Create(FBKGCache.Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  //--- Draw Background, Content and AMPM on cache image
  DrawBackground(g, FBKGCache.Canvas, R);
  DrawContent(g, FBKGCache.Canvas);
  DrawAMPM(g, FBKGCache.Canvas);

  //--- copy cache image
  Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyRect(R, FBKGCache.Canvas, FBKGCache.Canvas.ClipRect);
  g.Free;

  //--- draw needles
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawNeedles(g, nil);
  g.Free;
  {$ELSE}
  //--- copying canvas
  FBKGCache.Width := R.Right - R.Left;
  FBKGCache.Height := R.Bottom - R.Top;
  FBKGCache.Canvas.CopyMode := cmSrcCopy;
  FBKGCache.Canvas.CopyRect(FBKGCache.Canvas.ClipRect, Canvas, R);

  //--- Draw Background, Content and AMPM on cache image
  DrawBackground(FBKGCache.Canvas, R);
  DrawContent(FBKGCache.Canvas);
  DrawAMPM(FBKGCache.Canvas);

  //--- copy cache image
  Canvas.CopyMode := cmSrcCopy;
  Canvas.CopyRect(R, FBKGCache.Canvas, FBKGCache.Canvas.ClipRect);

  //--- draw needles
  DrawNeedles(Canvas);
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.Paint;
begin
  inherited;
  UpdateCache;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.RepaintTime;
var
  R: TRect;
  bmp: TBitmap;
  {$IFDEF TMSGDIPLUS}
  g: TGPGraphics;
  {$ENDIF}
begin

  R := ClientRect; //GetWatchRect;

  {$IFDEF TMSGDIPLUS}
  bmp := TBitmap.Create;
  try
    bmp.Width := R.Right - R.Left;
    bmp.Height := R.Bottom - R.Top;

    //--- copy cache image
    bmp.Canvas.CopyMode := cmSrcCopy;
    bmp.Canvas.CopyRect(bmp.Canvas.ClipRect, FBKGCache.Canvas, FBKGCache.Canvas.ClipRect);

    g := TGPGraphics.Create(bmp.Canvas.Handle);
    try
      g.SetSmoothingMode(SmoothingModeAntiAlias);
      //--- draw needles or pointers
      DrawNeedles(g, bmp.Canvas);

      //--- copy image
      Canvas.CopyMode := cmSrcCopy;
      Canvas.CopyRect(R, bmp.Canvas, bmp.Canvas.ClipRect);
    finally
      g.Free;
    end;
  finally
    bmp.Free;
  end;
  {$ELSE}
  bmp := TBitmap.Create;
  try
    bmp.Width := R.Right - R.Left;
    bmp.Height := R.Bottom - R.Top;

    //--- copy cache image
    bmp.Canvas.CopyMode := cmSrcCopy;
    bmp.Canvas.CopyRect(bmp.Canvas.ClipRect, FBKGCache.Canvas, FBKGCache.Canvas.ClipRect);

    //--- draw needles or pointers
    DrawNeedles(bmp.Canvas);

    //--- copy image
    Canvas.CopyMode := cmSrcCopy;
    Canvas.CopyRect(R, bmp.Canvas, bmp.Canvas.ClipRect);
  finally
    bmp.Free;
  end;
  {$ENDIF}
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.Resize;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetAnimation(const Value: Boolean);
begin
  FAnimation := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetAnimationFactor(const Value: integer);
begin
  FAnimationFactor := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetAppearance(const Value: TWatchAppearance);
begin
  FAppearance.Assign(Value);
end;

procedure TAdvWatch.SetComponentStyle(AStyle: TTMSStyle);
begin
  Appearance.CenterPointOuterBorderColor := clBlack;
  Appearance.HourFont.Color := clBlack;
  Appearance.AMPMFont.Color := clBlack;
  case AStyle of
    tsOffice2003Blue:
    begin
      Appearance.AMPMBorderColor := $962D00;
      Appearance.AMPMColor := $D68759;
      Appearance.BackgroundColor := $00FFD2AF;
      Appearance.BackgroundColorTo := $00FFD2AF;
      Appearance.BorderColor := $D68759;
      Appearance.CenterPointBorderColor := $D68759;
      Appearance.CenterPointColor := $D68759;
      Appearance.HourColor := $D68759;
      Appearance.MinuteColor := $D68759;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $D68759;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $D68759;
      Appearance.SecondPointer := clWhite;
    end;
    tsOffice2003Silver:
    begin
      Appearance.AMPMBorderColor := $962D00;
      Appearance.AMPMColor := $BDA4A5;
      Appearance.BackgroundColor := $00E6D8D8;
      Appearance.BackgroundColorTo := $00E6D8D8;
      Appearance.BorderColor := $BDA4A5;
      Appearance.CenterPointBorderColor := $BDA4A5;
      Appearance.CenterPointColor := $BDA4A5;
      Appearance.HourColor := $BDA4A5;
      Appearance.MinuteColor := $BDA4A5;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $BDA4A5;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $BDA4A5;
      Appearance.SecondPointer := clWhite;
    end;
    tsOffice2003Olive:
    begin
      Appearance.AMPMBorderColor := $962D00;
      Appearance.AMPMColor := $82C0AF;
      Appearance.BackgroundColor := RGB(225, 234, 185);
      Appearance.BackgroundColorTo := RGB(225, 234, 185);
      Appearance.BorderColor := $82C0AF;
      Appearance.CenterPointBorderColor := $82C0AF;
      Appearance.CenterPointColor := $82C0AF;
      Appearance.HourColor := $82C0AF;
      Appearance.MinuteColor := $82C0AF;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $82C0AF;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $82C0AF;
      Appearance.SecondPointer := clWhite;
    end;
    tsOffice2003Classic:
    begin
      Appearance.AMPMBorderColor := $962D00;
      Appearance.AMPMColor := $808080;
      Appearance.BackgroundColor := $00F2F2F2;
      Appearance.BackgroundColorTo := $00F2F2F2;
      Appearance.BorderColor := $808080;
      Appearance.CenterPointBorderColor := $808080;
      Appearance.CenterPointColor := $808080;
      Appearance.HourColor := $808080;
      Appearance.MinuteColor := $808080;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $808080;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $808080;
      Appearance.SecondPointer := $808080;
    end;
    tsOffice2007Luna:
    begin
      Appearance.AMPMBorderColor := $00F0DED0;
      Appearance.AMPMColor := $00F0DED0;
      Appearance.BackgroundColor := $FFEFE3;
      Appearance.BackgroundColorTo := $FFEFE3;
      Appearance.BorderColor := $00F0DED0;
      Appearance.CenterPointBorderColor := $FFEFE3;
      Appearance.CenterPointColor := $FFEFE3;
      Appearance.HourColor := $723708;
      Appearance.MinuteColor := $723708;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $723708;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $723708;
      Appearance.SecondPointer := $723708;
    end;
    tsOffice2007Obsidian:
    begin
      Appearance.AMPMBorderColor := $F2F1F0;
      Appearance.AMPMColor := $5C534C;
      Appearance.AMPMFont.Color := $F2F1F0;
      Appearance.BackgroundColor := $5C534C;
      Appearance.BackgroundColorTo := $5C534C;
      Appearance.BorderColor := $F2F1F0;
      Appearance.CenterPointBorderColor := $FFEFE3;
      Appearance.CenterPointColor := $FFEFE3;
      Appearance.HourColor := $F2F1F0;
      Appearance.MinuteColor := $F2F1F0;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $F2F1F0;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $F2F1F0;
      Appearance.SecondPointer := $F2F1F0;
      Appearance.HourFont.Color := $F2F1F0;
    end;
    tsWindowsXP:
    begin
      Appearance.AMPMBorderColor := clBlack;
      Appearance.AMPMColor := clBtnFace;
      Appearance.BackgroundColor := clBtnFace;
      Appearance.BackgroundColorTo := clBtnFace;
      Appearance.BorderColor := clBlack;
      Appearance.CenterPointBorderColor := clBlack;
      Appearance.CenterPointColor := clBlack;
      Appearance.HourColor := clBlack;
      Appearance.MinuteColor := clBlack;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := clBlack;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := clBlack;
      Appearance.SecondPointer := clBlack;
    end;
    tsWhidbey:
    begin
      Appearance.AMPMBorderColor := $7E9898;
      Appearance.AMPMColor := $F5F9FA;
      Appearance.BackgroundColor := $F5F9FA;
      Appearance.BackgroundColorTo := $F5F9FA;
      Appearance.BorderColor := $7E9898;
      Appearance.CenterPointBorderColor := $7E9898;
      Appearance.CenterPointColor := $7E9898;
      Appearance.HourColor := $7E9898;
      Appearance.MinuteColor := $7E9898;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $7E9898;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $7E9898;
      Appearance.SecondPointer := $7E9898;
    end;
    tsOffice2007Silver:
    begin
      Appearance.AMPMBorderColor := $E8E0DB;
      Appearance.AMPMColor := RGB(241, 244, 248);
      Appearance.BackgroundColor := RGB(241, 244, 248);
      Appearance.BackgroundColorTo := RGB(241, 244, 248);
      Appearance.BorderColor := $E8E0DB;
      Appearance.CenterPointBorderColor := $E8E0DB;
      Appearance.CenterPointColor := $74706F;
      Appearance.HourColor := $74706F;
      Appearance.MinuteColor := $74706F;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $74706F;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $74706F;
      Appearance.SecondPointer := $74706F;
    end;
    tsWindowsVista:
    begin
      Appearance.AMPMBorderColor := $FEDF9A;
      Appearance.AMPMColor := $FFFDF9;
      Appearance.BackgroundColor := $FFFDF9;
      Appearance.BackgroundColorTo := $FFFAF0;
      Appearance.BorderColor := $FEDF9A;
      Appearance.CenterPointBorderColor := $FEDF9A;
      Appearance.CenterPointColor := $FEDF9A;
      Appearance.HourColor := $FEDF9A;
      Appearance.MinuteColor := $FEDF9A;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $FEDF9A;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $FEDF9A;
      Appearance.SecondPointer := $FEDF9A;
    end;
    tsWindows7:
    begin
      Appearance.AMPMBorderColor := $FBD6B8;
      Appearance.AMPMColor := $FDFBFA;
      Appearance.BackgroundColor := $FDFBFA;
      Appearance.BackgroundColorTo := $FDF3EB;
      Appearance.BorderColor := $FBD6B8;
      Appearance.CenterPointBorderColor := $FBD6B8;
      Appearance.CenterPointColor := $FBD6B8;
      Appearance.HourColor := $FBD6B8;
      Appearance.MinuteColor := $FBD6B8;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $FBD6B8;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $FBD6B8;
      Appearance.SecondPointer := $FBD6B8;
    end;
    tsTerminal:
    begin
      Appearance.AMPMBorderColor := clGray;
      Appearance.AMPMColor := clbtnface;
      Appearance.BackgroundColor := clBtnFace;
      Appearance.BackgroundColorTo := clBtnFace;
      Appearance.BorderColor := clGray;
      Appearance.CenterPointBorderColor := clGray;
      Appearance.CenterPointColor := clGray;
      Appearance.HourColor := clGray;
      Appearance.MinuteColor := clGray;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := clGray;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := clGray;
      Appearance.SecondPointer := clGray;
    end;
    tsOffice2010Blue:
    begin
      Appearance.AMPMBorderColor := $C7B29F;
      Appearance.AMPMColor := $F0DAC7;
      Appearance.BackgroundColor := $FDF6EF;
      Appearance.BackgroundColorTo := $FDF6EF;
      Appearance.BorderColor := $C7B29F;
      Appearance.CenterPointBorderColor := $C7B29F;
      Appearance.CenterPointColor := $C7B29F;
      Appearance.HourColor := $C7B29F;
      Appearance.MinuteColor := $C7B29F;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $C7B29F;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $C7B29F;
      Appearance.SecondPointer := $C7B29F;

    end;
    tsOffice2010Silver:
    begin
      Appearance.AMPMBorderColor := $D2CDC8;
      Appearance.AMPMColor := $EDE5E0;
      Appearance.BackgroundColor := $FFFFFF;
      Appearance.BackgroundColorTo := $FFFFFF;
      Appearance.BorderColor := $D2CDC8;
      Appearance.CenterPointBorderColor := $D2CDC8;
      Appearance.CenterPointColor := $D2CDC8;
      Appearance.HourColor := $D2CDC8;
      Appearance.MinuteColor := $D2CDC8;
      Appearance.HourPointerShadow := clSilver;
      Appearance.HourPointer := $D2CDC8;
      Appearance.MinutePointerShadow := clSilver;
      Appearance.MinutePointer := $D2CDC8;
      Appearance.SecondPointer := $D2CDC8;

    end;
    tsOffice2010Black:
    begin
      Appearance.AMPMBorderColor := $6D6D6D;
      Appearance.AMPMColor := $919191;
      Appearance.BackgroundColor := $BFBFBF;
      Appearance.BackgroundColorTo := $BFBFBF;
      Appearance.BorderColor := $6D6D6D;
      Appearance.CenterPointBorderColor := $6D6D6D;
      Appearance.CenterPointColor := $6D6D6D;
      Appearance.HourColor := $6D6D6D;
      Appearance.MinuteColor := $6D6D6D;
      Appearance.HourPointerShadow := clWhite;
      Appearance.HourPointer := $6D6D6D;
      Appearance.MinutePointerShadow := clWhite;
      Appearance.MinutePointer := $6D6D6D;
      Appearance.SecondPointer := $6D6D6D;
    end;
  end;
end;

//------------------------------------------------------------------------------

function AnimateDouble(var Start, Stop: byte; Delta: byte; Margin: byte): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
      Start := Start - Delta;
  end;
end;

//------------------------------------------------------------------------------

function AnimateMinute(var Start, Stop: byte; Delta: byte; Margin: byte): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(Margin, Delta);
    if Start < Stop then
      Start := Start + Delta
    else
    begin
      if (Stop = 0) and (Start > 30) then
        Start := Start + Delta
      else
        Start := Start - Delta;
    end;
    if (Start >= 60) then
      Start := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.OnAnimationTimerTime(Sender: TObject);
var
  d, pos: byte;
  res: Boolean;
  dt: TDateTime;
  h, m, s, ms: word;  
begin
  if Animation and not (csLoading in ComponentState) and not (csDesigning in ComponentState) then
  begin
    dt := GetDisplayTime;
    DecodeTime(dT, h, m, s, ms);

    if FHourAnimating then
    begin
      d := Round(Abs(FCurrentHourTo - h) / AnimationFactor);
      pos := h;
      res := AnimateDouble(pos, FCurrentHourTo, d, 1);
      if res then
      begin
        h := pos;
        FInternalCall := True;
        SetHour(h);
        FInternalCall := False;
      end
      else
      begin
        h := FCurrentHourTo;
        FInternalCall := True;
        SetHour(h);
        FInternalCall := False;
        FAnimationTimer.Enabled := False;
      end;
    end
    else // Animate Minute
    begin
      if (FCurrentMinuteTo = 0) and (m > 30) then
        d := Round(Abs(60 - m) / AnimationFactor)
      else
        d := Round(Abs(FCurrentMinuteTo - m) / AnimationFactor);
      pos := m;
      res := AnimateMinute(pos, FCurrentMinuteTo, d, 1);
      if res then
      begin
        m := pos;
        FInternalCall := True;
        SetMinute(m);
        FInternalCall := False;
      end
      else
      begin
        m := FCurrentMinuteTo;
        FInternalCall := True;
        {if (FCurrentMinuteTo = 0) and (FMinuteFrom > 30) then
        begin
          SetHour((h + 1) mod 12);
          FMinuteFrom := FCurrentMinuteTo;
        end;
        }
        SetMinute(m);
        FInternalCall := False;
        FAnimationTimer.Enabled := False;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetHour(Value: byte);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  dt := GetDisplayTime;
  DecodeTime(dT, h, m, s, ms);
  if (h <> Value) then
  begin
    if Animation and not FInternalCall and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      FCurrentHourTo := Value;
      FHourAnimating := True;
      FAnimationTimer.Enabled := True;
    end
    else
    begin
      h := Value;
      SetTimeDirect(EncodeTime(h, m, s, ms));
      //InvalidateWatch;
      RepaintTime;
      if Assigned(FOnChange) and (FCurrentHourTo = Value) then
        FOnChange(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetMinute(Value: byte);
var
  dt: TDateTime;
  h, m, s, ms: word;
begin
  dt := GetDisplayTime;
  DecodeTime(dT, h, m, s, ms);
  if (Value >= 60) then
  begin
    raise Exception.Create('Invalid time');
    Exit;
  end;

  if (m <> Value) then
  begin
    if Animation and not FInternalCall and not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    begin
      FCurrentMinuteTo := Value;
      FHourAnimating := False;
      FMinuteFrom := m;
      FAnimationTimer.Enabled := True;
    end
    else
    begin
      if not Animation then
        FMinuteFrom := m;
        
      m := Value;
      if (m = 0) and (FMinuteFrom > 30) then
      begin
        h := (h + 1); //((h + 1) mod 12);
        if (h > 23) then
          h := 0;
        FMinuteFrom := m;
      end;
      SetTimeDirect(EncodeTime(h, m, s, ms));
      //InvalidateWatch;
      RepaintTime;

      if Assigned(FOnChange) and (FCurrentMinuteTo = Value) then
        FOnChange(Self);
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetSettings(const Value: TWatchSettings);
begin
  FSettings.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetTime(const Value: TDateTime);
begin
  if (FTime <> Value) then
  begin
    SetTimeDirect(Value);
    RepaintTime;
    //InvalidateWatch;    // this add flickering
  end;
end;

procedure TAdvWatch.SetTimeDirect(Value: TDateTime);
var
  h, m, s, ms: Word;
begin
  if (FTime <> Value) then
  begin
    FTime := Value;
    //if Settings.Auto then
    begin
      DecodeTime(FTime, h, m, s, ms);
      FInternalSet := True;
      AM := h < 12;
      FInternalSet := False;
    end;
  end;
end;

procedure TAdvWatch.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

procedure TAdvWatch.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  inherited;
  //Message.Result := 1;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.WMGetDlgCode(var Message: TMessage);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.WMKeyDown(var Message: TWMKeyDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.WMPaint(var Message: TWMPaint);
begin
  inherited;
end;

procedure TAdvWatch.WMSetCursor(var Message: TWMSetCursor);
var
  P: TPoint;
  R: TRect;
begin
  if (csDesigning in ComponentState) then
    Exit;


  inherited;

  GetCursorPos(P);
  P := ScreenToClient(P);

  R := GetAMPMRect;
  if PtInRect(R, P) then
  begin
    Windows.SetCursor(Screen.Cursors[crHandpoint])
  end
  else
    Windows.SetCursor(Screen.Cursors[crDefault]);


end;

//------------------------------------------------------------------------------

procedure TAdvWatch.WMTimer(var Message: TWMTimer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

function TAdvWatch.GetRadius: Double;
var
  R: TRect;
begin
  {i := Width;
  if Height < Width then
    i := Height;
  Result := (i * 0.9) / 2;
  }
  R := GetWatchRect;
  Result := (R.Right - R.Left) div 2;
end;

function TAdvWatch.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvWatch.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetShowAMPM(Value: Boolean);
begin
  if (FShowAMPM <> Value) then
  begin
    FShowAMPM := Value;
    InvalidateWatch;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetShowNumbers(Value: Boolean);
begin
  if (FShowNumbers <> Value) then
  begin
    FShowNumbers := Value;
    InvalidateWatch;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetStyle(const Value: TWatchStyle);
begin
  if (FStyle <> Value) then
  begin
    FStyle := Value;
    case (FStyle) of
      wsClassic:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          HourFont.Name := 'Comic Sans MS';;
          HourFont.Size := 10;
          HourFont.Style := [fsBold];
          HourMarkLength := 8;
          HourPointer := clBlack;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinutePointerShadow := clBtnShadow;
          MinuteMarkLength := 4;
        end;
        Animation := true;
        AnimationFactor := 10;
        ShowAMPM := True;
        ShowNumbers := True;
      end;
      wsTower:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          AMPMFrame := apRect;
          BorderWidth := 1;
          BackgroundColor := $00B1F0FE;
          BackgroundColorTo := $0054C9E7;
          BackgroundGradient := wgDiagonalForward;
          CenterPointColor := $00002BD5;
          CenterPointSize := 6;
          HourFont.Color := clWindowText;
          HourFont.Name := 'Century';
          HourFont.Style := [fsBold];
          HourPointer := clBlack;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinutePointerShadow := clBtnShadow;
          SecondPointer := $00002BD5;
          Shape := wsRect;
          PointerStyle := psPointer;
          BackgroundColorTo := $005BCCE8;
          AMPMColor := Appearance.BackgroundColor;
        end;

        ShowNumbers := False;
        Appearance.TickMarks := tmHours;
      end;
      wsFluorescent:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Color := clWindowText;
          AMPMFont.Size := 10;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          AMPMFrame := apRoundRect;
          BorderColor := $00FF3E3E;
          BackgroundColor := clBlack;
          BackgroundColorTo := clBlack;
          CenterPointColor := $00FF3E3E;
          CenterPointBorderColor := clBlack;
          CenterPointOuterBorderColor := clBlack;
          CenterPointSize := 20;
          HourFont.Color := clWindowText;
          HourFont.Size := 8;
          HourFont.Name := 'MS Sans Serif';
          HourFont.Style := [];
          HourColor := $00FF3E3E;
          HourMarkLength := 10;
          HourPointer := $00FF3E3E;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 2;
          MinuteColor := $00FF3E3E;
          MinutePointer := $00FF3E3E;
          MinutePointerShadow := clBtnShadow;
          MinutePointerSize := 2;
          Shape := wsRoundRect;
          CenterPointOuterBorderColor := clBlack;
          HourMarkWidth := 2;
          HourMarkStyle := hmsQuartDblLine;
          TickMarks := tmHours;
        end;
        ShowAMPM := False;
        ShowNumbers := False;
      end;
      wsEmerald:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Color := $00A00000;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          AMPMFrame := apRoundRect;
          BorderColor := $003AE800;
          BackgroundColor := $003AE800;
          BackgroundColorTo := clBlack;
          BackgroundGradient := wgDiagonalForward;
          CenterPointBorderColor := $0040FF00;
          CenterPointColor := $0038E100;
          CenterPointSize := 16;
          HourFont.Color := clWindowText;
          HourFont.Name := 'MS Sans Serif';
          HourFont.Style := [];
          HourPointer := $0040FF00;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinutePointer := $0040FF00;
          MinutePointerShadow := clBtnShadow;
          Shape := wsRect;
          PointerStyle := psPointer;
          AMPMColor := clNone;
          HourFont.Color := Appearance.BackgroundColor;
          CenterPointOuterBorderColor := clBlack;
          HourMarkStyle := hmsQuartDblLine;
          HourColor := Appearance.BackgroundColor;
          SecondPointer := clYellow;
          TickMarks := tmHours;
        end;
        ShowNumbers := False;
        ShowAMPM := False;
      end;
      wsBlueStar:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Color := $00A00000;
          AMPMFont.Size := 10;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          AMPMBorderColor := clNone;
          AMPMFrame := apRoundRect;
          BorderColor := $00FFD5D5;
          BackgroundColor := $00FFD5D5;
          BackgroundColorTo := $00A00000;
          BackgroundGradient := wgRadial;
          HourFont.Color := clWindowText;
          HourFont.Size := 8;
          HourFont.Name := 'MS Sans Serif';
          HourFont.Style := [];
          HourPointer := $00A00000;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinutePointer := $00A00000;
          MinutePointerShadow := clBtnShadow;
          Shape := wsRoundRect;
          PointerStyle := psPointer;
          AMPMColor := clNone;
          HourFont.Color := Appearance.BackgroundColor;
        end;
        ShowAMPM := True;
        ShowNumbers := True;
      end;
      wsFuchsia:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Color := clWindowText;
          AMPMFont.Size := 10;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          AMPMFrame := apRoundRect;
          BorderColor := $00FFD0FF;
          BackgroundColor := clBlack;
          BackgroundColorTo := clBlack;
          CenterPointBorderColor := $00DD68DD;
          CenterPointSize := 20;
          HourFont.Color := clWindowText;
          HourFont.Size := 8;
          HourFont.Name := 'MS Sans Serif';
          HourFont.Style := [];
          HourColor := $00FFD0FF;
          HourMarkLength := 10;
          HourPointer := $00FFD0FF;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 2;
          MinuteColor := $00FFD0FF;
          MinutePointer := $00FFD0FF;
          MinutePointerShadow := clBtnShadow;
          MinutePointerSize := 2;
          Shape := wsRect;
          CenterPointOuterBorderColor := clBlack;
          HourMarkWidth := 2;
          HourMarkStyle := hmsQuartDblLine;
          SecondPointer := $00DD68DD;
          TickMarks := tmQuartHours;
        end;
        ShowAMPM := False;
        ShowNumbers := False;
      end;
      wsBlack:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Style := [];
          BackgroundColor := $00202020;
          BackgroundColorTo := clNone;
          BorderColor := clBlack;
          CenterPointBorderColor := clWhite;
          CenterPointOuterBorderColor := clWhite;
          CenterPointColor := clWhite;
          HourColor := $00585858;
          HourFont.Color := clWhite;
          HourFont.Name := 'Century Gothic';
          HourFont.Size := 11;
          HourPointer := clWhite;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinuteColor := $00585858;
          MinutePointer := clWhite;
          MinutePointerShadow := clBtnShadow;
          MinuteMarkLength := 4;
        end;

        ShowAMPM := False;
        ShowNumbers := True;
      end;
      wsBlackWhite:
      begin
        with Appearance do
        begin
          InitializeDefault;
          BackgroundColor := clWhite;
          BackgroundColorTo := clNone;
          BorderColor := clBlack;
          BorderWidth := 8;
          CenterPointBorderColor := clBlack;
          CenterPointOuterBorderColor := clBlack;
          CenterPointColor := clBlack;
          CenterPointSize := 8;
          HourColor := $00585858;
          HourFont.Color := clBlack;
          HourFont.Name := 'Century Gothic';
          HourFont.Size := 14;
          HourFont.Style := [fsBold];
          HourPointer := clBlack;
          HourPointerShadow := clBtnShadow;
          HourPointerSize := 5;
          MinutePointer := clBlack;
          MinutePointerShadow := clBtnShadow;
          MinuteMarkLength := 4;
          SecondPointer := clMaroon;
          PointerStyle := psPointer;
          TickMarks := tmQuartHours;
        end;

        ShowAMPM := False;
        ShowNumbers := True;
      end;
      wsGray:
      begin
        with Appearance do
        begin
          InitializeDefault;
          AMPMFont.Name := 'MS Sans Serif';
          AMPMFont.Size := 10;
          BackgroundColor := $00D1D1D1;
          BackgroundColorTo := $006C6C6C;
          BackgroundGradient := wgRadial;
          BorderColor := $00454545;
          CenterPointBorderColor := $00454545;
          CenterPointColor := clBlack;
          CenterPointOuterBorderColor := clNone;
          HourColor := clBlack;
          HourFont.Name := 'Comic Sans MS';
          HourFont.Size := 8;
          HourFont.Style := [];
          HourPointer := $004E4E4E;
          HourPointerShadow := clNone;
          HourPointerSize := 1;
          MinuteColor := clGray;
          MinutePointer := $004E4E4E;
          MinutePointerShadow := clNone;
          MinutePointerSize := 1;
          SecondPointer := clRed;
        end;
        ShowAMPM := True;
        ShowNumbers := True;
      end;
      wsSuperNova:
      begin
        with Appearance do
        begin
          InitializeDefault;
          BackgroundColorTo := $00D1D3D3;
          BorderColor := clNone;
          CenterPointOuterBorderColor := clBlack;
          CenterPointSize := 10;
          HourMarkLength := Round(Min(Self.Height, Self.Width) / 4.5);
          HourPointer := clBlack;
          HourPointerShadow := clBtnShadow;
          MinuteColor := clGray;
          MinuteMarkLength := Min(Self.Height, Self.Width) div 3;
          MinutePointer := clBlack;
        end;
        ShowAMPM := False;
        ShowNumbers := False;
      end;
      wsSports:
      begin
        with Appearance do
        begin
          InitializeDefault;
          BackgroundColor := clBlack;
          BackgroundColorTo := clNone;
          BorderColor := clBlack;
          BorderWidth := 1;
          CenterPointBorderColor := clBlue;
          CenterPointOuterBorderColor := clBlack;
          CenterPointColor := $00191919;
          CenterPointSize := Round(Min(Self.Height, Self.Width) / 3.6);
          HourColor := $00ABB0B1;
          HourFont.Color := $00797979;
          HourFont.Name := 'Times New Roman';
          HourFont.Size := 9;
          HourPointer := clRed;
          HourPointerShadow := clNone;
          HourPointerSize := 2;
          MinuteColor := clGray;
          MinutePointer := clYellow;
          MinutePointerSize := 2;
          MinutePointerShadow := clNone;
          MinuteMarkLength := 1;
          SecondPointer := $00188BFE;
          TickMarks := tmQuartHours;
        end;

        ShowAMPM := False;
        ShowNumbers := True;
      end;
      wsSmooth:
      begin
        with Appearance do
        begin
          InitializeDefault;
          BackgroundColor := clWhite;
          BackgroundColorTo := $00ABB0B1;
          BorderColor := $00585858;
          BorderWidth := 1;
          CenterPointBorderColor := clBlack;
          CenterPointOuterBorderColor := $004A4A4A;
          CenterPointColor := clWhite;
          CenterPointSize := 6;
          HourColor := $00ABB0B1;
          HourFont.Color := $00797979;
          HourFont.Name := 'Times New Roman';
          HourFont.Size := 9;
          HourPointer := $004A4A4A;
          HourPointerShadow := $00B2B2B2;
          HourPointerSize := 2;
          MinuteColor := clGray;
          MinutePointer := $004A4A4A;
          MinutePointerSize := 2;
          MinutePointerShadow := $00B2B2B2;
          MinuteMarkLength := 1;
          SecondPointer := $004A4A4A;
          TickMarks := tmQuartHours;
        end;

        ShowAMPM := False;
        ShowNumbers := True;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvWatch.GetAMPMRect: TRect;
var
  R: TRect;
  CP: TPoint;
  rd: Double;
begin
  Result := Rect(-1, -1, -1, -1);
  if not FShowAMPM then
    Exit;
    
  R := Rect(0, 0, 200, 200);
  Canvas.Font.Assign(Appearance.AMPMFont);
  DrawText(Canvas.Handle,PChar('AM'),Length('AM'), R, DT_CALCRECT or DT_LEFT);
  CP := GetCenterPoint;
  rd := GetRadius;
  R.Right := R.Right + 4 * 2;
  R.Bottom := R.Bottom + 0 * 2;
  Result.Left := CP.X - (R.Right div 2);
  Result.Top := CP.Y + Round((rd - R.Bottom) / 3);
  Result.Right := Result.Left + R.Right;
  Result.Bottom := Result.Top + R.Bottom;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.SetAM(Value: Boolean);
var
  bmp, bmp2: TBitmap;
  {$IFDEF TMSGDIPLUS}
  g: TGPGraphics;
  {$ENDIF}
  R, R1, RAM, R2, R3: TRect;
  i, j, h, spd: Integer;
begin
  if (FAM <> Value) then
  begin
    FAM := Value;
    if Animation and FShowAMPM and (Appearance.AMPMColor <> clNone) and not FInternalSet then
    begin
      R := ClientRect;
      //--- Draw AMPM
      R1 := GetAMPMRect;
      R3 := R1;
      R3 := Rect(R3.Left + 1, R3.Top + 1, R3.Right - 1, R3.Bottom - 1);
      RAM := R3;
      h := RAM.Bottom - RAM.Top;
      RAM := Rect(0, 0, RAM.Right - RAM.Left, RAM.Bottom - RAM.Top);
      bmp2 := TBitmap.Create;
      bmp2.Width := RAM.Right - RAM.Left;
      bmp2.Height := h * 2;
      bmp2.Canvas.Brush.Color := Appearance.AMPMColor;
      bmp2.Canvas.Pen.Color := Appearance.AMPMColor;
      bmp2.Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
      bmp2.Canvas.Brush.Style := bsClear;

      {$IFDEF TMSGDIPLUS}
      DrawGDIPText(bmp2.Canvas, nil, taCenter, RAM, 'AM', '', Appearance.AMPMFont, True, True, aaAntiAlias);
      DrawGDIPText(bmp2.Canvas, nil, taCenter, Rect(RAM.Left, RAM.Bottom, RAM.Right, RAM.Bottom + h), 'PM', '', Appearance.AMPMFont, True, True, aaAntiAlias);
      {$ELSE}
      bmp2.Canvas.Font.Assign(Appearance.AMPMFont);
      DrawText(bmp2.Canvas.Handle,PChar('AM'),Length('AM'), RAM, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      R2 := Rect(RAM.Left, RAM.Bottom, RAM.Right, RAM.Bottom + h);
      DrawText(bmp2.Canvas.Handle,PChar('PM'),Length('PM'), R2, DT_CENTER or DT_VCENTER or DT_SINGLELINE);
      {$ENDIF}

      R2 := RAM;

      spd := 1;
      j := h div spd;
      if Value then
        R2.Top := RAM.Bottom
      else
        R2.Top := RAM.Top;

      for i:= 1 to j do
      begin
        R2.Bottom := R2.Top + h;
        bmp := TBitmap.Create;
        bmp.Width := R.Right - R.Left;
        bmp.Height := R.Bottom - R.Top;

        //--- copy cache image
        bmp.Canvas.CopyMode := cmSrcCopy;
        bmp.Canvas.CopyRect(bmp.Canvas.ClipRect, FBKGCache.Canvas, FBKGCache.Canvas.ClipRect);

        //--- Copy AMPM
        bmp.Canvas.CopyRect(R3, bmp2.Canvas, R2);
        bmp.Canvas.Pen.Color := Appearance.AMPMBorderColor;
        bmp.Canvas.Brush.Style := bsClear;
        if (Appearance.AMPMFrame = apRect) then
          bmp.Canvas.Rectangle(R1.Left, R1.Top, R1.Right, R1.Bottom)
        else if (Appearance.AMPMFrame = apRoundRect) then
          bmp.Canvas.RoundRect(R1.Left, R1.Top, R1.Right, R1.Bottom, 5, 5);

       {$IFDEF TMSGDIPLUS}
        g := TGPGraphics.Create(bmp.Canvas.Handle);
        g.SetSmoothingMode(SmoothingModeAntiAlias);

        //--- draw needles or pointers
        DrawNeedles(g, bmp.Canvas);
        g.Free;
        {$ELSE}
        DrawNeedles(bmp.Canvas);
        {$ENDIF}
        //--- copy image
        Canvas.CopyMode := cmSrcCopy;
        Canvas.CopyRect(R, bmp.Canvas, bmp.Canvas.ClipRect);
        bmp.Free;

        if Value then
          R2.Top := R2.Top - spd
        else
          R2.Top := R2.Top + spd;

        Sleep(10);
      end;
      bmp2.Free;
      UpdateCache;
    end
    else
    begin
      UpdateCache;
    end;
  end;
end;

//------------------------------------------------------------------------------

{$IFDEF TMSGDIPLUS}
procedure TAdvWatch.DrawAMPM(g: TGPGraphics; aCanvas: TCanvas);
{$ELSE}
procedure TAdvWatch.DrawAMPM(aCanvas: TCanvas);
{$ENDIF}
var
  R: TRect;
  s: string;
begin
  if not Assigned(aCanvas) or not FShowAMPM then
    Exit;
    
  R := GetAMPMRect;
  if AM then
    s := 'AM'
  else
    s := 'PM';

  if (Appearance.AMPMColor <> clNone) then
    aCanvas.Brush.Color := Appearance.AMPMColor
  else
    aCanvas.Brush.Style := bsClear;
  aCanvas.Pen.Color := Appearance.AMPMBorderColor;
  if (Appearance.AMPMColor <> clNone) or (Appearance.AMPMBorderColor <> clNone) then
  begin
    if (Appearance.AMPMFrame = apRect) then
      aCanvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom)
    else if (Appearance.AMPMFrame = apRoundRect) then
      aCanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, 5, 5);
  end;

  aCanvas.Brush.Style := bsClear;
{$IFDEF TMSGDIPLUS}
  DrawGDIPText(aCanvas, g, taCenter, R, s, '', Appearance.AMPMFont, True, True, aaAntiAlias);
{$ELSE}
  aCanvas.Font.Assign(Appearance.AMPMFont);
  DrawText(aCanvas.Handle,PChar(s),Length(s), R, DT_CENTER or DT_VCENTER or DT_SINGLELINE)
{$ENDIF}  
end;

//------------------------------------------------------------------------------

{$IFDEF TMSGDIPLUS}
procedure TAdvWatch.DrawBackground(g: TGPGraphics; aCanvas: TCanvas; R: TRect);
var
  gp: TGPGraphicsPath;
  pgb: TGPPathGradientBrush;
  linGrBrush: TGPLinearGradientBrush;
  gpPen: TGPPen;
  colors : array[0..0] of TGPColor;
  count, w, h, round: Integer;
  rc, gc, bc: Byte;
  cp: TPoint;
  rd: Double;
  Clr, ClrTo, brClr: TColor;
begin
  if not Assigned(g) then
    Exit;

  w := R.Right - R.Left;
  h := R.Bottom - R.Top;
  round := 8;
  cp := GetCenterPoint;
  rd := GetRadius;

  Clr := Appearance.BackgroundColor;
  ClrTo := Appearance.BackgroundColorTo;
  if (ClrTo = clNone) then
    ClrTo := Clr;
  brClr := Appearance.BorderColor;

  //--- Draw Background picture
  if Assigned(Appearance.Picture.Graphic) and not Appearance.Picture.Graphic.Empty then
  begin
    //aCanvas.StretchDraw(R, Appearance.Picture.Graphic);
    DrawGDIPImage(g, nil, R, Appearance.Picture.Graphic, True, True);
  end
  else
  begin
    linGrBrush := nil;
    case Appearance.BackgroundGradient of
      wgRadial:
      begin
        gp := TGPGraphicsPath.Create;
        case Appearance.Shape of
          wsCircle: gp.AddEllipse(cp.X - rd, cp.Y - rd, rd * 2, rd * 2);
          wsRect:
          begin
            gp.Free;
            gp := CreateGDIPRectangle(Rect(R.Left, R.Top, w, h));
          end; 
          wsRoundRect:
          begin
            gp.Free;
            gp := CreateRoundRectangle(Rect(R.Left, R.Top, w, h), round);
          end;
          {wsPentagon:
          begin
          end;}
        end;

        pgb := TGPPathGradientBrush.Create(gp);
        pgb.SetCenterColor(ColorToARGB(Clr));
        pgb.SetCenterPoint(MakePoint(CP.X, CP.Y));
        rc := GetRValue(ClrTo);
        gc := GetGValue(ClrTo);
        bc := GetBValue(ClrTo);
        colors[0] := MakeColor(255, rc, gc, bc); //ColorToARGB(HotClr);
        count := 1;
        pgb.SetSurroundColors(@colors, count);
        //pgp.SetFocusScales(0.7, 0.7);
        case Appearance.Shape of
          wsCircle: g.FillEllipse(pgb, cp.X - rd, cp.Y - rd, rd * 2, rd * 2);
          wsRect, wsRoundRect:
          begin
            g.FillPath(pgb, gp);
          end;
          {wsPentagon:
          begin
          end;}
        end;
        pgb.Free;
        gp.Free;
      end;
      wgVertical:
      begin
        linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(Clr),ColorToARGB(ClrTo), LinearGradientModeVertical);
      end;
      wgDiagonalForward:
      begin
        linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(Clr),ColorToARGB(ClrTo), LinearGradientModeForwardDiagonal);
      end;
      wgDiagonalBackward:
      begin
        linGrBrush := TGPLinearGradientBrush.Create(MakeRect(r.Left,r.Top,w,h),ColorToARGB(Clr),ColorToARGB(ClrTo), LinearGradientModeBackwardDiagonal);
      end;
    end;

    if (Appearance.BackgroundGradient <> wgRadial) and Assigned(linGrBrush) then
    begin
      case Appearance.Shape of
        wsCircle: g.FillEllipse(linGrBrush, cp.X - rd, cp.Y - rd, rd * 2, rd * 2);
        wsRect: g.FillRectangle(linGrBrush, R.Left, R.Top, w, h);
        wsRoundRect: FillRoundRect(g, linGrBrush, R.Left, R.Top, w, h, round);
        {wsPentagon:
        begin
        end;}
      end;
      linGrBrush.Free;
    end;
  end;  

  if (Appearance.BorderStyle <> wbNone) and (Appearance.BorderWidth > 0) then
  begin
    gpPen := TGPPen.Create(ColorToARGB(BrClr), Appearance.BorderWidth);
    case Appearance.Shape of
      wsCircle: g.DrawEllipse(gpPen, cp.X - rd, cp.Y - rd, rd * 2, rd * 2);
      wsRect: g.DrawRectangle(gpPen, R.Left, R.Top, w - 1, h - 1);
      wsRoundRect: DrawRoundRect(g, gpPen, R.Left, R.Top, w - 1, h - 1, round);
      {wsPentagon:
      begin
      end;}
    end;
    gpPen.free;
  end;
end;

{$ELSE}

procedure TAdvWatch.DrawBackground(aCanvas: TCanvas; R: TRect);
var
  round: Integer;
  R1: TRect;
  Rgn: HRGN;
begin
  if not Assigned(aCanvas) then
    Exit;

  round := 20;
  if (Appearance.Shape = wsCircle) then
    R1 := GetWatchRect
  else
    R1 := R;

  with Appearance do
  begin
    //--- clip region
    Rgn := 0;
    case Shape of
      wsCircle:
      begin
        Rgn := CreateEllipticRgn(R1.Left - 1, R1.Top - 1, R1.Right + 2, R1.Bottom + 2);
        SelectClipRgn(aCanvas.Handle, Rgn);
      end;
      wsRoundRect:
      begin
        Rgn := CreateRoundRectRgn(R1.Left - 1, R1.Top - 1, R1.Right + 2, R1.Bottom + 2, round, round);
        SelectClipRgn(aCanvas.Handle, Rgn);
      end;
    end;

    //--- draw background gradient and color
    if (BackgroundColor <> clNone) and (BackgroundColorTo <> clNone) then
      DrawGradient(aCanvas, BackgroundColor, BackgroundColorTo, 80, R1, BackgroundGradient = wgRadial)
    else if (BackgroundColor <> clNone) then
    begin
      aCanvas.Brush.Color := BackgroundColor;
      aCanvas.FillRect(R1);
    end;

    //--- Draw Border
    if (BorderColor <> clNone) and (Appearance.BorderWidth > 0) then
    begin
      aCanvas.Pen.Color := BorderColor;
      aCanvas.Pen.Width := Appearance.BorderWidth;
      aCanvas.Brush.Style := bsClear;
      case Appearance.Shape of
        wsCircle: aCanvas.Ellipse(R1.Left, R1.Top, R1.Right, R1.Bottom);
        wsRect: aCanvas.Rectangle(R);
        wsRoundRect: aCanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, round,round);
      end;
    end;

    if (Rgn <> 0) then
    begin
      SelectClipRgn(aCanvas.Handle, 0);
      DeleteObject(Rgn);
    end;
    
    //--- Draw Background picture
    if Assigned(Picture.Graphic) and not Picture.Graphic.Empty then
      DrawPic(aCanvas, Picture, R, True);
  end;
end;  
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF TMSGDIPLUS}
procedure TAdvWatch.DrawTickMarks(g: TGPGraphics; aCanvas: TCanvas);
var
  R: TRect;
  CP: TPoint;
  BrW, i: Integer;
  rd, ml: Double;
  path: TGPGraphicsPath;
  gpPen: TGPPen;
begin
  if not Assigned(g) or (Appearance.TickMarks = tmNone) then
    Exit;

  //R := GetInnerRect;
  //rd := (R.Right - R.Left) div 2;
  R := GetWatchRect;
  CP := GetCenterPoint;
  BrW := GetWatchBorderWidth;
  rd := GetRadius - (BrW div 2);
  with Appearance do
  begin
    ml := rd * 0.9;
    if (HourMarkLength > 0) then
      ml := rd - HourMarkLength;
    //--- draw Four hours
    for i := 0 to 3 do
    begin
      if (HourMarkStyle = hmsQuartDblLine) then
      begin
        case i of
          0, 2:
          begin
            path := TGPGraphicsPath.Create;
            gppen := TGPPen.Create(ColorToARGB(HourColor), HourMarkWidth);
            path.AddLine( Cos(i * 90 * PI / 180) * ml + CP.X,
                   Sin(i * 90 * PI / 180) * ml + CP.Y - 2,
                   Cos(i * 90 * PI / 180) * rd + CP.X,
                   Sin(i * 90 * PI / 180) * rd + CP.Y - 2);
            g.DrawPath(gppen, path);
            path.Free;
            path := TGPGraphicsPath.Create;
            path.AddLine( Cos(i * 90 * PI / 180) * ml + CP.X,
                   Sin(i * 90 * PI / 180) * ml + CP.Y + 2,
                   Cos(i * 90 * PI / 180) * rd + CP.X,
                   Sin(i * 90 * PI / 180) * rd + CP.Y + 2);
            g.DrawPath(gppen, path);
            path.Free;
            gpPen.Free;
          end;
          1, 3:
          begin
            path := TGPGraphicsPath.Create;
            gppen := TGPPen.Create(ColorToARGB(HourColor), HourMarkWidth);
            path.AddLine( Cos(i * 90 * PI / 180) * ml + CP.X - 2,
                   Sin(i * 90 * PI / 180) * ml + CP.Y,
                   Cos(i * 90 * PI / 180) * rd + CP.X - 2,
                   Sin(i * 90 * PI / 180) * rd + CP.Y);
            g.DrawPath(gppen, path);
            path.Free;
            path := TGPGraphicsPath.Create;
            path.AddLine( Cos(i * 90 * PI / 180) * ml + CP.X + 2,
                   Sin(i * 90 * PI / 180) * ml + CP.Y,
                   Cos(i * 90 * PI / 180) * rd + CP.X + 2,
                   Sin(i * 90 * PI / 180) * rd + CP.Y);
            g.DrawPath(gppen, path);
            path.Free;
            gpPen.Free;
          end;
        end;  
      end
      else  // hmsLine
      begin
        path := TGPGraphicsPath.Create;
        gppen := TGPPen.Create(ColorToARGB(HourColor), HourMarkWidth);
        path.AddLine( Cos(i * 90 * PI / 180) * ml + CP.X,
               Sin(i * 90 * PI / 180) * ml + CP.Y,
               Cos(i * 90 * PI / 180) * rd + CP.X,
               Sin(i * 90 * PI / 180) * rd + CP.Y);
        g.DrawPath(gppen, path);
        path.Free;
        gpPen.Free;
      end;
    end;

    if (TickMarks in [tmAll, tmHours]) then
    begin  // draw 12 hours
      for i := 0 to 11 do
      begin
        if ((i mod 3) <> 0) then
        begin
          path := TGPGraphicsPath.Create;
          gppen := TGPPen.Create(ColorToARGB(HourColor), HourMarkWidth);
          path.AddLine(Cos(i * 30 * PI / 180) * ml + CP.X,
                     Sin(i * 30 * PI / 180) * ml + CP.Y,
                     Cos(i * 30 * PI / 180) * rd + CP.X,
                     Sin(i * 30 * PI / 180) * rd + CP.Y);
          g.DrawPath(gppen, path);
          path.Free;
          gpPen.Free;
        end;
      end;
    end;

    ml := rd * 0.95;
    if (MinuteMarkLength > 0) then
      ml := rd - MinuteMarkLength;
      
    if (TickMarks = tmAll) then
    begin
      for i := 0 to 59 do
      begin
        if ((i mod 5) <> 0) then
        begin
          path := TGPGraphicsPath.Create;
          gppen := TGPPen.Create(ColorToARGB(MinuteColor), MinuteMarkWidth);
          path.AddLine(Cos(i * 6 * PI / 180) * ml + CP.X,
                     Sin(i * 6 * PI / 180) * ml + CP.Y,
                     Cos(i * 6 * PI / 180) * rd + CP.X,
                     Sin(i * 6 * PI / 180) * rd + CP.Y);
          g.DrawPath(gppen, path);
          path.Free;
          gpPen.Free;
        end;
      end;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.DrawNumbers(g: TGPGraphics; aCanvas: TCanvas);
var
  R: TRect;
  CP: TPoint;
  BrW, i, h, X, Y, rd2: Integer;
  rd, ang, rn, offX, offY: Double;
  R1: TRect;
  ts: TSize;
  s: string;
begin
  if not Assigned(aCanvas) or not ShowNumbers {or (Appearance.TickMarks = tmNone)} then
    Exit;

  R := GetWatchRect;
  CP := GetCenterPoint;
  BrW := GetWatchBorderWidth;
  rd := GetRadius - (BrW div 2);
  if Assigned(aCanvas) then
    aCanvas.Font.Assign(Appearance.HourFont);

  if (Appearance.HourMarkLength > 0) then
    rd2 := Appearance.HourMarkLength
  else
    rd2 := Round(rd - (rd * 0.9)) + 2;

  Canvas.Font.Assign(Appearance.HourFont);

  with Appearance do
  begin
    offX := rd - (rd2 + 2);
    offY := rd - (rd2 + 2);
    rn := min(offY, offX) - rd2;
    aCanvas.Brush.Style := bsClear;

    for i := 1 to 12 do
    begin
      if (TickMarks in [tmAll, tmHours, tmNone]) or ((TickMarks = tmQuartHours) and ((i mod 3) = 0)) then
      begin
        ang := (pi / 3000) * ((i * 100) * 30 div 6);
        s := Inttostr(i);
        ts := GetTextSize(Canvas, s);
        //w := 0;
        h := 0;
        if (i = 10) then
        begin
          //w := w + Round(ts.cx * 0.3);
          h := h + Round(ts.cy * 0.2);
        end
        else if (i = 11) then
        begin
          //w := w + Round(ts.cx * 0.3);
          h := h + Round(ts.cy * 0.15);
        end;
        X := CP.X + 1 + Round((rn - h + (rd2 div 2)) * Sin(ang));
        Y := CP.Y + 1 - Round((rn - h + (rd2 div 2)) * Cos(ang));
        R1 := Rect(X - (rd2 div 2), y - (rd2 div 2), X + (rd2 div 2), y + (rd2 div 2));
        X := R1.Right - ((R1.Right - R1.Left) div 2) - (ts.cx div 2) - 1;
        y := R1.Bottom - ((R1.Bottom - R1.Top) div 2) - (ts.cy div 2) - 1;
        aCanvas.TextOut(X, Y, s);
        //DrawGDIPText(nil, g, taLeftJustify, R1, s, '', HourFont, True, True, aaAntiAlias);
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.DrawContent(g: TGPGraphics; aCanvas: TCanvas);
begin
  DrawTickMarks(g, aCanvas);
  DrawNumbers(g, aCanvas);
end;
{$ELSE}

procedure TAdvWatch.DrawTickMarks(aCanvas: TCanvas);
var
  R: TRect;
  CP: TPoint;
  BrW, i: Integer;
  rd, ml: Double;
begin
  if not Assigned(aCanvas) or (Appearance.TickMarks = tmNone) then
    Exit;

  R := GetWatchRect;
  CP := GetCenterPoint;
  BrW := GetWatchBorderWidth;
  rd := GetRadius - (BrW div 2);
  with Appearance do
  begin
    ml := rd * 0.9;
    if (HourMarkLength > 0) then
      ml := rd - HourMarkLength;
    //--- draw Four hours
    aCanvas.Pen.Color := HourColor;
    aCanvas.Pen.Width := HourMarkWidth;
    for i := 0 to 3 do
    begin
      if (HourMarkStyle = hmsQuartDblLine) then
      begin
        case i of
          0, 2:
          begin
            aCanvas.MoveTo(Round(Cos(i * 90 * PI / 180) * ml + CP.X), Round(Sin(i * 90 * PI / 180) * ml + CP.Y - 2));
            aCanvas.LineTo(Round(Cos(i * 90 * PI / 180) * rd + CP.X), Round(Sin(i * 90 * PI / 180) * rd + CP.Y - 2));

            aCanvas.MoveTo(Round(Cos(i * 90 * PI / 180) * ml + CP.X), Round(Sin(i * 90 * PI / 180) * ml + CP.Y + 2));
            aCanvas.LineTo(Round(Cos(i * 90 * PI / 180) * rd + CP.X), Round(Sin(i * 90 * PI / 180) * rd + CP.Y + 2));
          end;
          1, 3:
          begin
            aCanvas.MoveTo(Round(Cos(i * 90 * PI / 180) * ml + CP.X - 2), Round(Sin(i * 90 * PI / 180) * ml + CP.Y));
            aCanvas.LineTo(Round(Cos(i * 90 * PI / 180) * rd + CP.X - 2), Round(Sin(i * 90 * PI / 180) * rd + CP.Y));

            aCanvas.MoveTo(Round(Cos(i * 90 * PI / 180) * ml + CP.X + 2), Round(Sin(i * 90 * PI / 180) * ml + CP.Y));
            aCanvas.LineTo(Round(Cos(i * 90 * PI / 180) * rd + CP.X + 2), Round(Sin(i * 90 * PI / 180) * rd + CP.Y));
          end;
        end;
      end
      else  // hmsLine
      begin
        aCanvas.MoveTo(Round(Cos(i * 90 * PI / 180) * ml + CP.X), Round(Sin(i * 90 * PI / 180) * ml + CP.Y));
        aCanvas.LineTo(Round(Cos(i * 90 * PI / 180) * rd + CP.X), Round(Sin(i * 90 * PI / 180) * rd + CP.Y));
      end;
    end;

    if (TickMarks in [tmAll, tmHours]) then
    begin  // draw 12 hours
      aCanvas.Pen.Color := HourColor;
      aCanvas.Pen.Width := HourMarkWidth;
      for i := 0 to 11 do
      begin
        if ((i mod 3) <> 0) then
        begin
          aCanvas.MoveTo(Round(Cos(i * 30 * PI / 180) * ml + CP.X), Round(Sin(i * 30 * PI / 180) * ml + CP.Y));
          aCanvas.LineTo(Round(Cos(i * 30 * PI / 180) * rd + CP.X), Round(Sin(i * 30 * PI / 180) * rd + CP.Y));
        end;
      end;
    end;

    ml := rd * 0.95;
    if (MinuteMarkLength > 0) then
      ml := rd - MinuteMarkLength;
      
    if (TickMarks = tmAll) then
    begin
      for i := 0 to 59 do
      begin
        if ((i mod 5) <> 0) then
        begin
          aCanvas.Pen.Color := MinuteColor;
          aCanvas.Pen.Width := MinuteMarkWidth;

          aCanvas.MoveTo(Round(Cos(i * 6 * PI / 180) * ml + CP.X), Round(Sin(i * 6 * PI / 180) * ml + CP.Y));
          aCanvas.LineTo(Round(Cos(i * 6 * PI / 180) * rd + CP.X), Round(Sin(i * 6 * PI / 180) * rd + CP.Y));
        end;
      end;
    end;

  end;
end;

//------------------------------------------------------------------------------

procedure TAdvWatch.DrawNumbers(aCanvas: TCanvas);
var
  R: TRect;
  CP: TPoint;
  BrW, i, h, X, Y, rd2: Integer;
  rd, ang, rn, offX, offY: Double;
  R1: TRect;
  ts: TSize;
  s: string;
begin
  if not Assigned(aCanvas) or not ShowNumbers or (Appearance.TickMarks = tmNone) then
    Exit;

  R := GetWatchRect;
  CP := GetCenterPoint;
  BrW := GetWatchBorderWidth;
  rd := GetRadius - (BrW div 2);
  if Assigned(aCanvas) then
    aCanvas.Font.Assign(Appearance.HourFont);

  if (Appearance.HourMarkLength > 0) then
    rd2 := Appearance.HourMarkLength
  else
    rd2 := Round(rd - (rd * 0.9)) + 2;

  Canvas.Font.Assign(Appearance.HourFont);

  with Appearance do
  begin
    offX := rd - (rd2 + 2);
    offY := rd - (rd2 + 2);
    rn := min(offY, offX) - rd2;
    aCanvas.Brush.Style := bsClear;

    for i := 1 to 12 do
    begin
      if (TickMarks = tmAll) or ((TickMarks = tmQuartHours) and ((i mod 3) = 0)) then
      begin
        ang := (pi / 3000) * ((i * 100) * 30 div 6);
        s := Inttostr(i);
        ts := GetTextSize(Canvas, s);
        //w := 0;
        h := 0;
        if (i = 10) then
        begin
          //w := w + Round(ts.cx * 0.3);
          h := h + Round(ts.cy * 0.2);
        end
        else if (i = 11) then
        begin
          //w := w + Round(ts.cx * 0.3);
          h := h + Round(ts.cy * 0.15);
        end;
        X := CP.X + 1 + Round((rn - h + (rd2 div 2)) * Sin(ang));
        Y := CP.Y + 1 - Round((rn - h + (rd2 div 2)) * Cos(ang));
        R1 := Rect(X - (rd2 div 2), y - (rd2 div 2), X + (rd2 div 2), y + (rd2 div 2));
        X := R1.Right - ((R1.Right - R1.Left) div 2) - (ts.cx div 2) - 1;
        y := R1.Bottom - ((R1.Bottom - R1.Top) div 2) - (ts.cy div 2) - 1;
        aCanvas.TextOut(X, Y, s);
        //DrawGDIPText(nil, g, taLeftJustify, R1, s, '', HourFont, True, True, aaAntiAlias);
      end;
    end;
  end;
end;

procedure TAdvWatch.DrawContent(aCanvas: TCanvas);
  procedure DrawWatchCircle(R: TRect);
  begin
    with Appearance do
    begin
      case BorderStyle of
        wbLine:
        begin
          aCanvas.Ellipse(R);
        end;
        wbDoubleLine:
        begin
          aCanvas.Ellipse(R);
          InflateRect(R, -2, -2);
          aCanvas.Ellipse(R);
        end;
      end;
    end;
  end;

  procedure DrawWatchRect(R: TRect);
  begin
    with Appearance do
    begin
      case BorderStyle of
        wbLine:
        begin
          aCanvas.Rectangle(R);
        end;
        wbDoubleLine:
        begin
          aCanvas.Rectangle(R);
          InflateRect(R, -2, -2);
          aCanvas.Rectangle(R);
        end;
      end;
    end;
  end;

  procedure DrawWatchRoundRect(R: TRect);
  begin
    with Appearance do
    begin
      case BorderStyle of
        wbLine:
        begin
          aCanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Rounding, Rounding);
        end;
        wbDoubleLine:
        begin
          aCanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Rounding, Rounding);
          InflateRect(R, -2, -2);
          aCanvas.RoundRect(R.Left, R.Top, R.Right, R.Bottom, Rounding, Rounding);
        end;
      end;
    end;
  end;

  procedure DrawWatchPentagon(R: TRect);
  begin
    with Appearance do
    begin
      case BorderStyle of
        wbLine:
        begin
        end;
        wbDoubleLine:
        begin
        end;
      end;
    end;  
  end;
        
begin
  if not Assigned(aCanvas) then
    Exit;

  DrawTickMarks(aCanvas);
  DrawNumbers(aCanvas);
end;
{$ENDIF}

//------------------------------------------------------------------------------

{$IFDEF TMSGDIPLUS}
procedure TAdvWatch.DrawNeedles(g: TGPGraphics; ACanvas: TCanvas);
var
  h, m, s, ms: word;
  dT :TDateTime;
  CP: TPoint;
  path,arr: TGPGraphicsPath;
  gppen: TGPPen;
  SldBrush: TGPSolidBrush;
  rd: Double;
  pts: array[0..3] of TGPPointF;
  i: Integer;
  rc, gc, bc: Byte;
  overshoot: double;
begin
  if not Assigned(g) then
    Exit;

  CP := GetCenterPoint;
  rd := GetRadius;
  if Settings.Auto then
  begin
    dT := Now;
    dT := dT + (Settings.TimeOffset / (60 * 24));
  end
  else
    dT := Self.Time;

  DecodeTime(dT, h, m, s, ms);

  if Settings.Auto then
  begin
    FInternalSet := True;
    AM := h < 12;
    FInternalSet := False;
  end;

  h := h mod 12;
  if False {(Settings.SecondFall = sfJump)} then
	begin
    if (s = FOldSecond) then
      if Settings.Auto then
        Exit;
    //FOldS := s;
    ms := 0;
  end;

  if Settings.Auto and (s <> FOldSecond) then //every seconds
	begin
    FOldSecond := s;
    if (m <> FOldMinute) then
  	begin
      FOldMinute := m;
      //DoChangeMin(h,m,s);
      if (h <> FOldHour) then
    	begin
        FOldHour := h;
        //DoChangeHour(h,m,s);
	    end;
    end;
  end;

  with Appearance do
  begin
    case PointerStyle of
      psLine, psLineArrow,psShortline:
      begin
        if PointerStyle = psShortLine then
          overshoot := 0.0
        else
          overshoot := 0.1;

        //--- Draw Hour needle
          //--- shadow
        if (HourPointerShadow <> clNone) then
        begin
          path := TGPGraphicsPath.Create;
          try
            rc := GetRValue(HourPointerShadow);
            gc := GetGValue(HourPointerShadow);
            bc := GetBValue(HourPointerShadow);


            gppen := TGPPen.Create(MakeColor(80, rc, gc, bc), HourPointerSize);
            try
              path.AddLine(CP.X - 1 - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * overshoot,
                          CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * overshoot,
                          (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.46 + CP.X -1,
                          (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.46 + CP.Y + 3);
              g.DrawPath(gppen, path);
            finally
              gppen.Free;
            end;
          finally
            path.Free;
          end;
        end;

        path := TGPGraphicsPath.Create;
        try
          gppen := TGPPen.Create(ColorToARGB(HourPointer), HourPointerSize);

          try
            path.AddLine(CP.X - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * overshoot,
                        CP.Y - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * overshoot,
                        (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X,
                        (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y);
            g.DrawPath(gppen, path);

            if PointerStyle = psLineArrow then
            begin
              arr := TGPGraphicsPath.Create;

              arr.AddLine((Sin(((h * 30 + m / 12 * 6) + 10) * PI / 180)) * rd * 0.40 + CP.X,
                        (-Cos(((h * 30 + m / 12 * 6) + 10) * PI / 180)) * rd * 0.40 + CP.Y,
                        (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X,
                        (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y);

              arr.AddLine((Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X,
                        (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y,
                        (Sin(((h * 30 + m / 12 * 6) - 10) * PI / 180)) * rd * 0.40 + CP.X,
                        (-Cos(((h * 30 + m / 12 * 6) - 10) * PI / 180)) * rd * 0.40 + CP.Y);

              g.DrawPath(gppen, arr);

              arr.Free;

            end;

          finally
            gppen.Free;
          end;
        finally
          path.Free;
        end;

        //--- Draw Minute needle
          //--- Shadow
        if (MinutePointerShadow <> clNone) then
        begin
          path := TGPGraphicsPath.Create;
          try
            rc := GetRValue(MinutePointerShadow);
            gc := GetGValue(MinutePointerShadow);
            bc := GetBValue(MinutePointerShadow);
            gppen := TGPPen.Create(MakeColor(80, rc, gc, bc), MinutePointerSize);

            try
              path.AddLine(CP.X - (Sin(m * 6 * PI / 180)) * rd * overshoot,
                          CP.Y + 3- (-Cos(m * 6 * PI / 180)) * rd * overshoot,
                          (Sin(m * 6 * PI / 180)) * rd * 0.65 + CP.X,
                          (-Cos(m * 6 * PI / 180)) * rd * 0.65 + CP.Y + 3);
              g.DrawPath(gppen, path);

            finally
              gppen.Free;
            end;
          finally
            path.Free;
          end;
        end;

        path := TGPGraphicsPath.Create;
        gppen := TGPPen.Create(ColorToARGB(MinutePointer), MinutePointerSize);

        try
          path.AddLine(CP.X - (Sin(m * 6 * PI / 180)) * rd * overshoot,
                      CP.Y - (-Cos(m * 6 * PI / 180)) * rd * overshoot,
                      (Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X,
                      (-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y);

          if PointerStyle = psLineArrow then
          begin
            arr := TGPGraphicsPath.Create;

            arr.AddLine((Sin((m * 6 + 7) * PI / 180)) * rd * 0.6 + CP.X,
                      (-Cos((m * 6 + 7) * PI / 180)) * rd * 0.6 + CP.Y,
                      (Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X,
                      (-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y);

            arr.AddLine((Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X,
                      (-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y,
                      (Sin((m * 6 - 7) * PI / 180)) * rd * 0.6 + CP.X,
                      (-Cos((m * 6 - 7) * PI / 180)) * rd * 0.6 + CP.Y);

            g.DrawPath(gppen, arr);

            arr.Free;

          end;

          g.DrawPath(gppen, path);
        finally
          path.Free;
          gppen.Free;
        end;

        //--- Draw Second needle
        if (Settings.Auto) and Settings.Second then
        begin
          path := TGPGraphicsPath.Create;
          gppen := TGPPen.Create(ColorToARGB(SecondPointer), SecondPointerSize);
          try
            path.AddLine(CP.X - (Sin(s * 6 * PI / 180)) * rd * 0.2,
                        CP.Y - (-Cos(s * 6 * PI / 180)) * rd * 0.2,
                        (Sin(s * 6 * PI / 180)) * rd * 0.9 + CP.X,
                        (-Cos(s * 6 * PI / 180)) * rd * 0.9 + CP.Y);
            g.DrawPath(gppen, path);
          finally
            path.Free;
            gppen.Free;
          end;
        end;

        //--- Draw Center  Point
        {SldBrush := TGPSolidBrush.Create(ColorToARGB(FCenterPointColor));
        //g.FillEllipse(SldBrush, CP.X - rd * 0.05, CP.Y - rd * 0.05, rd * 0.1, rd * 0.1);
        g.FillEllipse(SldBrush, CP.X - (rd * 0.05), CP.Y - rd * 0.05, rd * 0.1, rd * 0.1);
        SldBrush.Free;
        }
      end;
      psPointer:
      begin
        //--- Draw Hour needle
          //-- Shadow
        if (HourPointerShadow <> clNone) then
        begin
          path := TGPGraphicsPath.Create;
          Pts[0].X := CP.X + 1 - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1;
          Pts[0].Y := CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1;
          Pts[1].X := CP.X + 1 - (Sin((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05;
          Pts[1].Y := CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05;
          Pts[2].X := (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X + 1;
          Pts[2].Y := (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y + 3;
          Pts[3].X := CP.X + 1 - (Sin((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05;
          Pts[3].Y := CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05;
          Path.AddPolygon(PGPPointF(@pts), 4);
          Path.CloseFigure;
          rc := GetRValue(HourPointerShadow);
          gc := GetGValue(HourPointerShadow);
          bc := GetBValue(HourPointerShadow);
          Sldbrush := TGPSolidBrush.Create(MakeColor(80, rc, gc, bc));
          g.FillPath(Sldbrush, Path);
          Sldbrush.Free;
          Path.Free;
        end;

        path := TGPGraphicsPath.Create;
        Pts[0].X := CP.X - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1;
        Pts[0].Y := CP.Y - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1;
        Pts[1].X := CP.X - (Sin((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05;
        Pts[1].Y := CP.Y - (-Cos((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05;
        Pts[2].X := (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X;
        Pts[2].Y := (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y;
        Pts[3].X := CP.X - (Sin((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05;
        Pts[3].Y := CP.Y - (-Cos((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05;
        Path.AddPolygon(PGPPointF(@pts), 4);
        Path.CloseFigure;
        Sldbrush := TGPSolidBrush.Create(ColorToARGB(HourPointer));
        g.FillPath(Sldbrush, Path);
        Sldbrush.Free;
        Path.Free;

        //--- Draw Minute needle
          //-- Shadow
        if (MinutePointerShadow <> clNone) then
        begin
          path := TGPGraphicsPath.Create;
          Pts[0].X := CP.X + 1 - (Sin(m * 6 * PI / 180)) * rd * 0.1;
          Pts[0].Y := CP.Y + 3 - (-Cos(m * 6 * PI / 180)) * rd * 0.1;
          Pts[1].X := CP.X + 1 - (Sin((m * 6 + 90) * PI / 180)) * rd * 0.05;
          Pts[1].Y := CP.Y + 3 - (-Cos((m * 6 + 90) * PI / 180)) * rd * 0.05;
          Pts[2].X := (Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X + 1;
          Pts[2].Y := (-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y + 3;
          Pts[3].X := CP.X + 1 - (Sin((m * 6 - 90) * PI / 180)) * rd * 0.05;
          Pts[3].Y := CP.Y + 3 - (-Cos((m * 6 - 90) * PI / 180)) * rd * 0.05;
          Path.AddPolygon(PGPPointF(@pts), 4);
          Path.CloseFigure;
          rc := GetRValue(MinutePointerShadow);
          gc := GetGValue(MinutePointerShadow);
          bc := GetBValue(MinutePointerShadow);
          Sldbrush := TGPSolidBrush.Create(MakeColor(80, rc, gc, bc));
          g.FillPath(Sldbrush, Path);
          Sldbrush.Free;
          Path.Free;
        end;

        path := TGPGraphicsPath.Create;
        Pts[0].X := CP.X - (Sin(m * 6 * PI / 180)) * rd * 0.1;
        Pts[0].Y := CP.Y - (-Cos(m * 6 * PI / 180)) * rd * 0.1;
        Pts[1].X := CP.X - (Sin((m * 6 + 90) * PI / 180)) * rd * 0.05;
        Pts[1].Y := CP.Y - (-Cos((m * 6 + 90) * PI / 180)) * rd * 0.05;
        Pts[2].X := (Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X;
        Pts[2].Y := (-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y;
        Pts[3].X := CP.X - (Sin((m * 6 - 90) * PI / 180)) * rd * 0.05;
        Pts[3].Y := CP.Y - (-Cos((m * 6 - 90) * PI / 180)) * rd * 0.05;
        Path.AddPolygon(PGPPointF(@pts), 4);
        Path.CloseFigure;
        Sldbrush := TGPSolidBrush.Create(ColorToARGB(MinutePointer));
        g.FillPath(Sldbrush, Path);
        Sldbrush.Free;
        Path.Free;

        //--- Draw Second needle
        if (Settings.Auto) and Settings.Second then
        begin
          path := TGPGraphicsPath.Create;
          gppen := TGPPen.Create(ColorToARGB(SecondPointer), SecondPointerSize);
          try
            path.AddLine(CP.X - (Sin(s * 6 * PI / 180)) * rd * 0.2,
                        CP.Y - (-Cos(s * 6 * PI / 180)) * rd * 0.2,
                        (Sin(s * 6 * PI / 180)) * rd * 0.9 + CP.X,
                        (-Cos(s * 6 * PI / 180)) * rd * 0.9 + CP.Y);
            g.DrawPath(gppen, path);
          finally
            path.Free;
            gppen.Free;
          end;
        end;
      end;
    end;
    
    //--- Draw Center Point
    if (Appearance.CenterPointSize > 0) then
    begin
      if (FCenterPointColor <> clNone) then
      begin
        SldBrush := TGPSolidBrush.Create(ColorToARGB(FCenterPointColor));
        //g.FillEllipse(SldBrush, CP.X - rd * 0.03, CP.Y - rd * 0.03, rd * 0.06, rd * 0.06);
        g.FillEllipse(SldBrush, CP.X - (CenterPointSize / 2), CP.Y - (CenterPointSize / 2), CenterPointSize, CenterPointSize);
        SldBrush.Free;
      end;
      i := 0;
      if (CenterPointOuterBorderColor <> clNone) then
      begin
        i := Round(CenterPointSize * 0.2);
        gpPen := TGPPen.Create(ColorToARGB(CenterPointOuterBorderColor), i);
        g.DrawEllipse(gpPen, CP.X - (CenterPointSize / 2), CP.Y - (CenterPointSize / 2), CenterPointSize, CenterPointSize);
        gppen.Free;
      end;
      if (CenterPointBorderColor <> clNone) then
      begin
        gpPen := TGPPen.Create(ColorToARGB(CenterPointBorderColor), Round(CenterPointSize * 0.1));
        g.DrawEllipse(gpPen, CP.X - ((CenterPointSize - i) / 2), CP.Y - ((CenterPointSize - i) / 2), CenterPointSize - i, CenterPointSize - i);
        gppen.Free;
      end;
    end;
  end;
end;

{$ELSE}
procedure TAdvWatch.DrawNeedles(aCanvas: TCanvas);
var
  h, m, s, ms: word;
  dT :TDateTime;
  CP: TPoint;
  rd: Double;
  pts: array[0..3] of TPoint;
  i, X, Y: Integer;
begin
  if not Assigned(aCanvas) then
    Exit;

  CP := GetCenterPoint;
  rd := GetRadius;
  if Settings.Auto then
  begin
    dT := Now;
    dT := dT + (Settings.TimeOffset / (60 * 24));
  end
  else
    dT := Self.Time;

  DecodeTime(dT, h, m, s, ms);

  if Settings.Auto then
  begin
    FInternalSet := True;
    AM := h < 12;
    FInternalSet := False;
  end;  

  h := h mod 12;
  if False {(Settings.SecondFall = sfJump)} then
	begin
    if (s = FOldSecond) then
      if Settings.Auto then
        Exit;
    //FOldS := s;
    ms := 0;
  end;
  if Settings.Auto and (s <> FOldSecond) then //every seconds
	begin
    FOldSecond := s;
    if (m <> FOldMinute) then
  	begin
      FOldMinute := m;
      //DoChangeMin(h,m,s);
      if (h <> FOldHour) then
    	begin
        FOldHour := h;
        //DoChangeHour(h,m,s);
	    end;
    end;
  end;

  with Appearance do
  begin
    case PointerStyle of
      psLine:
      begin
        //--- Draw Hour needle
          //--- shadow
        if (HourPointerShadow <> clNone) then
        begin
          aCanvas.Pen.Width := HourPointerSize;
          aCanvas.Pen.Color := HourPointerShadow;
          aCanvas.MoveTo(Round(CP.X - 1 - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1), Round(CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1));
          aCanvas.LineTo(Round((Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.46 + CP.X -1), Round((-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.46 + CP.Y + 3));
        end;

          //--- Minute Shadow
        if (MinutePointerShadow <> clNone) then
        begin
          aCanvas.Pen.Width := MinutePointerSize;
          aCanvas.Pen.Color := MinutePointerShadow;
          aCanvas.MoveTo(Round(CP.X - (Sin(m * 6 * PI / 180)) * rd * 0.1), Round(CP.Y + 3- (-Cos(m * 6 * PI / 180)) * rd * 0.1));
          aCanvas.LineTo(Round((Sin(m * 6 * PI / 180)) * rd * 0.65 + CP.X), Round((-Cos(m * 6 * PI / 180)) * rd * 0.65 + CP.Y + 3));
        end;

        aCanvas.Pen.Color := HourPointer;
        aCanvas.Pen.Width := HourPointerSize;
        aCanvas.MoveTo(Round(CP.X - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1), Round(CP.Y - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1));
        aCanvas.LineTo(Round((Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X), Round((-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y));

        //--- Draw Minute needle
        aCanvas.Pen.Width := MinutePointerSize;
        aCanvas.Pen.Color := MinutePointer;
        aCanvas.MoveTo(Round(CP.X - (Sin(m * 6 * PI / 180)) * rd * 0.1), Round(CP.Y - (-Cos(m * 6 * PI / 180)) * rd * 0.1));
        aCanvas.LineTo(Round((Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X), Round((-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y));

        //--- Draw Second needle
        if (Settings.Auto) and Settings.Second then
        begin
          aCanvas.Pen.Color := SecondPointer;
          aCanvas.Pen.Width := SecondPointerSize;
          aCanvas.MoveTo(Round(CP.X - (Sin(s * 6 * PI / 180)) * rd * 0.2), Round(CP.Y - (-Cos(s * 6 * PI / 180)) * rd * 0.2));
          aCanvas.LineTo(Round((Sin(s * 6 * PI / 180)) * rd * 0.9 + CP.X), Round((-Cos(s * 6 * PI / 180)) * rd * 0.9 + CP.Y));
        end;

      end;
      psPointer:
      begin
        //--- Draw Hour needle
          //-- Shadow
        if (HourPointerShadow <> clNone) then
        begin
          Pts[0].X := Round(CP.X + 1 - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1);
          Pts[0].Y := Round(CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1);
          Pts[1].X := Round(CP.X + 1 - (Sin((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05);
          Pts[1].Y := Round(CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05);
          Pts[2].X := Round((Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X + 1);
          Pts[2].Y := Round((-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y + 3);
          Pts[3].X := Round(CP.X + 1 - (Sin((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05);
          Pts[3].Y := Round(CP.Y + 3 - (-Cos((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05);
          aCanvas.Brush.Color := HourPointerShadow;
          aCanvas.Pen.Color := HourPointerShadow;
          aCanvas.Pen.Width := 1;
          aCanvas.Polygon(pts);
        end;

        Pts[0].X := Round(CP.X - (Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1);
        Pts[0].Y := Round(CP.Y - (-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.1);
        Pts[1].X := Round(CP.X - (Sin((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05);
        Pts[1].Y := Round(CP.Y - (-Cos((h * 30 + m / 12 * 6 + 90) * PI / 180)) * rd * 0.05);
        Pts[2].X := Round((Sin((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.X);
        Pts[2].Y := Round((-Cos((h * 30 + m / 12 * 6) * PI / 180)) * rd * 0.5 + CP.Y);
        Pts[3].X := Round(CP.X - (Sin((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05);
        Pts[3].Y := Round(CP.Y - (-Cos((h * 30 + m / 12 * 6 - 90) * PI / 180)) * rd * 0.05);
        aCanvas.Brush.Color := HourPointer;
        aCanvas.Pen.Color := HourPointer;
        aCanvas.Pen.Width := 1;
        aCanvas.Polygon(pts);
 
        //--- Draw Minute needle
          //-- Shadow
        if (MinutePointerShadow <> clNone) then
        begin
          Pts[0].X := Round(CP.X + 1 - (Sin(m * 6 * PI / 180)) * rd * 0.1);
          Pts[0].Y := Round(CP.Y + 3 - (-Cos(m * 6 * PI / 180)) * rd * 0.1);
          Pts[1].X := Round(CP.X + 1 - (Sin((m * 6 + 90) * PI / 180)) * rd * 0.05);
          Pts[1].Y := Round(CP.Y + 3 - (-Cos((m * 6 + 90) * PI / 180)) * rd * 0.05);
          Pts[2].X := Round((Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X + 1);
          Pts[2].Y := Round((-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y + 3);
          Pts[3].X := Round(CP.X + 1 - (Sin((m * 6 - 90) * PI / 180)) * rd * 0.05);
          Pts[3].Y := Round(CP.Y + 3 - (-Cos((m * 6 - 90) * PI / 180)) * rd * 0.05);

          aCanvas.Brush.Color := MinutePointerShadow;
          aCanvas.Pen.Color := MinutePointerShadow;
          aCanvas.Pen.Width := 1;
          aCanvas.Polygon(pts);
        end;

        Pts[0].X := Round(CP.X - (Sin(m * 6 * PI / 180)) * rd * 0.1);
        Pts[0].Y := Round(CP.Y - (-Cos(m * 6 * PI / 180)) * rd * 0.1);
        Pts[1].X := Round(CP.X - (Sin((m * 6 + 90) * PI / 180)) * rd * 0.05);
        Pts[1].Y := Round(CP.Y - (-Cos((m * 6 + 90) * PI / 180)) * rd * 0.05);
        Pts[2].X := Round((Sin(m * 6 * PI / 180)) * rd * 0.7 + CP.X);
        Pts[2].Y := Round((-Cos(m * 6 * PI / 180)) * rd * 0.7 + CP.Y);
        Pts[3].X := Round(CP.X - (Sin((m * 6 - 90) * PI / 180)) * rd * 0.05);
        Pts[3].Y := Round(CP.Y - (-Cos((m * 6 - 90) * PI / 180)) * rd * 0.05);

        aCanvas.Brush.Color := MinutePointer;
        aCanvas.Pen.Color := MinutePointer;
        aCanvas.Pen.Width := 1;
        aCanvas.Polygon(pts);
        
        //--- Draw Second needle
        if (Settings.Auto) and Settings.Second then
        begin
          aCanvas.Pen.Color := SecondPointer;
          aCanvas.Pen.Width := SecondPointerSize;
          aCanvas.MoveTo(Round(CP.X - (Sin(s * 6 * PI / 180)) * rd * 0.2), Round(CP.Y - (-Cos(s * 6 * PI / 180)) * rd * 0.2));
          aCanvas.LineTo(Round((Sin(s * 6 * PI / 180)) * rd * 0.9 + CP.X), Round((-Cos(s * 6 * PI / 180)) * rd * 0.9 + CP.Y));
        end;
      end;
    end;

    //--- Draw Center Point
    if (Appearance.CenterPointSize > 0) then
    begin
      if (FCenterPointColor <> clNone) then
      begin
        aCanvas.Brush.Color := CenterPointColor;
        aCanvas.Pen.Color := CenterPointColor;
        aCanvas.Pen.Width := 1;
        X := Round(CP.X - (CenterPointSize / 2));
        Y := Round(CP.Y - (CenterPointSize / 2));
        aCanvas.Ellipse(X, Y, X + CenterPointSize, Y + CenterPointSize);
      end;
      i := 0;
      if (CenterPointOuterBorderColor <> clNone) then
      begin
        i := Round(CenterPointSize * 0.2);
        aCanvas.Brush.Style := bsClear;
        aCanvas.Pen.Color := CenterPointOuterBorderColor;
        aCanvas.Pen.Width := i;
        X := Round(CP.X - (CenterPointSize / 2));
        Y := Round(CP.Y - (CenterPointSize / 2));
        aCanvas.Ellipse(X, Y, X + CenterPointSize, Y + CenterPointSize);
      end;
      if (CenterPointBorderColor <> clNone) then
      begin
        aCanvas.Brush.Style := bsClear;
        aCanvas.Pen.Color := CenterPointBorderColor;
        aCanvas.Pen.Width := Round(CenterPointSize * 0.1);
        X := Round(CP.X - ((CenterPointSize - i) / 2));
        Y := Round(CP.Y - ((CenterPointSize - i) / 2));
        aCanvas.Ellipse(X, Y, X + CenterPointSize - i, Y + CenterPointSize - i);
      end;
    end;

  end;
end;
{$ENDIF}

//------------------------------------------------------------------------------


end.

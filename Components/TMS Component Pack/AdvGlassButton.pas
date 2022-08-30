{***************************************************************************}
{ TAdvGlassButton component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2007 - 2015                                       }
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

unit AdvGlassButton;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, GDIPicture, ActnList, AdvHintInfo, AdvGlowButton, AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 3; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Improved : glyph positioning when ShowCaption = false
  // v1.0.0.2 : Fixed : issue with doublebuffered painting in Delphi 2007
  // v1.1.0.0 : New : Anchors property exposed
  // v1.2.0.0 : New : bsRecord background symbol type
  // v1.2.1.0 : New : key events exposed
  // v1.2.1.1 : Fixed : issue with left aligned text for btsCurvedRectangle shape
  // v1.2.1.2 : Fixed : PictureDown used when button is down in grouped buttons
  // v1.2.2.0 : New : exposed TabOrder property
  // v1.2.3.0 : Improved : TabStop property set default to true
  // v1.2.3.1 : Fixed : issue with wordwrap and aaNone antialias
  // v1.3.0.0 : New : OnCustomBackgroundSymbol event added
  // v1.3.0.1 : Fixed : Issue when used in COM plugins with focus
  // v1.3.0.2 : Fixed : Issue when using in combination with GetFormImage

type
  TAdvCustomGlassButton = class;

  TAdvToolButtonStyle = (tasButton, tasCheck);
  TBackGroundSymbol = (bsArrowLeft, bsArrowRight, bsArrowUp, bsArrowDown, bsPlay, bsPause, bsStop, bsFastForward, bsForward,
                      bsBackward, bsFastBackward, bsSpeaker, bsNoSpeaker, bsNone, bsRecord, bsCustom);

  TButtonShape = (btsRectangle, btsCurvedRectangle);
  TButtonDirection = (bdLeft, bdRight);

  TCustomBackgroundSymbolEvent = procedure(Sender: TObject; ARect: TRect; AGraphics: TGPGraphics) of object;

{$IFDEF DELPHI6_LVL}
  TAdvGlassButtonActionLink = class(TControlActionLink)
  protected
    FClient: TAdvCustomGlassButton;
    procedure AssignClient(AClient: TObject); override;
    function IsCheckedLinked: Boolean; override;
    function IsGroupIndexLinked: Boolean; override;
    procedure SetGroupIndex(Value: Integer); override;
    procedure SetChecked(Value: Boolean); override;
  end;                                    
{$ENDIF}

  TAdvCustomGlassButton = class(TCustomControl)
  private
    FGroupIndex: Integer;
    FDown: Boolean;
    FAllowAllUp: Boolean;
    FOffSet: integer;
    FMouseInControl: Boolean;
    FHot: Boolean;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FStyle: TAdvToolButtonStyle;
    FState: TAdvButtonState;
    FMouseDownInControl: Boolean;
    FGrouped: Boolean;
    FDragging: Boolean;
    FPropHot: Boolean;
    FUnHotTimer: TTimer;
    FInitialDown: Boolean;
    FOfficeHint: TAdvHintInfo;
    FIPictureDown: TGDIPPicture;
    FIPictureDisabled: TGDIPPicture;
    FIPicture: TGDIPPicture;
    FIPictureHot: TGDIPPicture;
    FShortCutHint: TShortCutHintWindow;
    FShortCutHintPos: TShortCutHintPos;
    FShortCutHintText: string;
    FBackColor: TColor;
    FForeColor: TColor;
    FInnerBorderColor: TColor;
    FOuterBorderColor: TColor;
    FShineColor: TColor;
    FGlowColor: TColor;
    FLayout: TButtonLayout;
    FAntiAlias: TAntiAlias;
    FShowCaption: Boolean;
    FShowFocusRect: Boolean;
    FActive: Boolean;
    FCornerRadius: Integer;
    FBackGroundSymbol: TBackGroundSymbol;
    FBackGroundSymbolColor: TColor;
    FButtonShape: TButtonShape;
    FButtonDirection: TButtonDirection;
    FForceTransparent: Boolean;
    FBackgroundColor: TColor;
    FOnCustomBackgroundSymbol: TCustomBackgroundSymbolEvent;
    procedure UnHotTimerOnTime(Sender: TObject);
    procedure UpdateExclusive;
    procedure UpdateTracking;
    procedure ButtonDown;
    procedure OnPictureChanged(Sender: TObject);
    procedure CNCommand(var Message: TWMCommand); message CN_COMMAND;
    procedure CMFocusChanged(var Message: TCMFocusChanged); message CM_FOCUSCHANGED;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure WMLButtonDblClk(var Message: TWMLButtonDown); message WM_LBUTTONDBLCLK;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMDialogKey(var Message: TCMDialogKey); message CM_DIALOGKEY;
    procedure CMDialogChar(var Message: TCMDialogChar); message CM_DIALOGCHAR;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMSetText(var Message: TWMSetText); message WM_SETTEXT;
    procedure CMSysColorChange(var Message: TMessage); message CM_SYSCOLORCHANGE;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMLButtonUp(var Msg:TWMLButtonDown); message WM_LBUTTONUP;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;    
    procedure CMButtonPressed(var Message: TMessage); message CM_BUTTONPRESSED;
    procedure SetDown(Value: Boolean);
    procedure SetAllowAllUp(Value: Boolean);
    procedure SetGroupIndex(Value: Integer);
    procedure SetStyle(const Value: TAdvToolButtonStyle);
    procedure SetState(const Value: TAdvButtonState);
    procedure SetGrouped(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetHot: Boolean;
    procedure SetHot(const Value: Boolean);
    procedure SetOfficeHint(const Value: TAdvHintInfo);
    procedure SetIPicture(const Value: TGDIPPicture);
    procedure SetIPictureDisabled(const Value: TGDIPPicture);
    procedure SetIPictureDown(const Value: TGDIPPicture);
    procedure SetIPictureHot(const Value: TGDIPPicture);
    procedure SetBackColor(const Value: TColor);
    procedure SetForeColor(const Value: TColor);
    procedure SetInnerBorderColor(const Value: TColor);
    procedure SetOuterBorderColor(const Value: TColor);
    procedure SetShineColor(const Value: TColor);
    procedure SetGlowColor(const Value: TColor);
    procedure SetLayout(const Value: TButtonLayout);
    procedure SetAntiAlias(const Value: TAntiAlias);
    procedure SetShowCaption(const Value: Boolean);
    procedure SetShowFocusRect(const Value: Boolean);
    procedure SetCornerRadius(Value: Integer);
    procedure SetBackGroundSymbol(const Value: TBackGroundSymbol);
    procedure SetBackGroundSymbolColor(const Value: TColor);
    procedure SetButtonShape(const Value: TButtonShape);
    procedure SetButtonDirection(const Value: TButtonDirection);
    procedure SetForceTransparent(const Value: Boolean);
    procedure SetBackgroundColor(const Value: TColor);
  protected
    procedure SetParent(AParent: TWinControl); override;
{$IFDEF DELPHI6_LVL}
    function GetActionLinkClass: TControlActionLinkClass; override;
    procedure ActionChange(Sender: TObject; CheckDefaults: Boolean); override;
{$ENDIF}
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure DrawButton(ACanvas: TCanvas); virtual;
    procedure DrawButtonBackground(graphic: TGPGraphics);
    procedure DrawTransparentBackGround(Canvas: TCanvas);
    procedure KeyPress(var Key: Char); override;
    procedure Paint; override;
    procedure WndProc(var Message: TMessage); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure DoCustomBackgroundSymbol(Graphics: TGPGraphics; ARect: TRect); virtual;

    procedure InvalidateMe;
    property MouseInControl: Boolean read FMouseInControl;
    property State: TAdvButtonState read FState write SetState;

    // published
    property Action;
    property AllowAllUp: Boolean read FAllowAllUp write SetAllowAllUp default False;
    property Anchors;
    property BiDiMode;

    property AntiAlias: TAntiAlias read FAntiAlias write SetAntiAlias default aaClearType;
    property BackgroundColor: TColor read FBackgroundColor write SetBackgroundColor default clNone;
    property BackColor: TColor read FBackColor write SetBackColor;
    property ForeColor: TColor read FForeColor write SetForeColor;
    property InnerBorderColor: TColor read FInnerBorderColor write SetInnerBorderColor;
    property OuterBorderColor: TColor read FOuterBorderColor write SetOuterBorderColor;
    property ShineColor: TColor read FShineColor write SetShineColor;
    property GlowColor: TColor read FGlowColor write SetGlowColor;
    property Layout: TButtonLayout read FLayout write SetLayout default blGlyphLeft;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default false;
    property CornerRadius: Integer read FCornerRadius write SetCornerRadius default 4;
    property BackGroundSymbol: TBackGroundSymbol read FBackGroundSymbol write SetBackGroundSymbol default bsNone;
    property BackGroundSymbolColor: TColor read FBackGroundSymbolColor write SetBackGroundSymbolColor default clWhite;
    property ButtonShape: TButtonShape read FButtonShape write SetButtonShape default btsRectangle;
    property ButtonDirection: TButtonDirection read FButtonDirection write SetButtonDirection default bdRight;
    property ForceTransparent: Boolean read FForceTransparent write SetForceTransparent default False;

    property Constraints;
    property Grouped: Boolean read FGrouped write SetGrouped default False;
    property GroupIndex: Integer read FGroupIndex write SetGroupIndex default 0;
    property Down: Boolean read FDown write SetDown default False;
    property Enabled;
    property Font;
    property Hot: Boolean read GetHot write SetHot default false;

    property Picture: TGDIPPicture read FIPicture write SetIPicture;
    property PictureHot: TGDIPPicture read FIPictureHot write SetIPictureHot;
    property PictureDown: TGDIPPicture read FIPictureDown write SetIPictureDown;
    property PictureDisabled: TGDIPPicture read FIPictureDisabled write SetIPictureDisabled;

    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property OfficeHint: TAdvHintInfo read FOfficeHint write SetOfficeHint;
    property Style: TAdvToolButtonStyle read FStyle write SetStyle default tasButton;
    property ShortCutHint: string read FShortCutHintText write FShortCutHintText;
    property ShortCutHintPos: TShortCutHintPos read FShortCutHintPos write FShortCutHintPos default shpTop;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnClick;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCustomBackgroundSymbol: TCustomBackgroundSymbolEvent read FOnCustomBackgroundSymbol write FOnCustomBackgroundSymbol;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Click; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetVersionNr: Integer; virtual;

    procedure ShowShortCutHint;
    procedure HideShortCutHint;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvGlassButton = class(TAdvCustomGlassButton)
  public
    property BackgroundColor;
  published
    property Action;
    property AllowAllUp;
    property Anchors;
    property AntiAlias;
    property BackColor;
    property BackGroundSymbol;
    property BackGroundSymbolColor;
    property ButtonDirection;
    property ButtonShape;
    property Caption;
    property Constraints;
    property CornerRadius;
    property Down;
    property Enabled;
    property Font;
    property ForeColor;
    property ForceTransparent;
    property GlowColor;
    property GroupIndex;
    property InnerBorderColor;
    property Layout;
    property OfficeHint;
    property OuterBorderColor;
    property Picture;
    property PictureDown;
    property PictureDisabled;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShineColor;
    property ShortCutHint;
    property ShortCutHintPos;
    property ShowCaption;
    property ShowFocusRect;
    property ShowHint;
    property Style;
    property TabOrder;
    property TabStop default true;
    property Version;
    property Visible;
    property OnClick;
    property OnCustomBackgroundSymbol;
    property OnDblClick;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter;
    property OnMouseLeave;
  end;

  TProWinCtrl = class(TWinControl);

implementation

const
  GDIP_NOWRAP = 4096;

//------------------------------------------------------------------------------

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

procedure DrawRoundRectangle(graphic: TGPGraphics; R: TRect; Radius: Integer; Clr: TColor);
var
  path: TGPGraphicsPath;
  l, t, w, h, d: Integer;
  gppen: TGPPen;
begin
  if not Assigned(graphic) then
    Exit;
  path := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;
  d := Radius shl 1;
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddLine(l + radius, t, l + w - radius, t); // top
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.AddLine(l, t + h - radius, l, t + radius); // left
  path.CloseFigure();
  gppen := TGPPen.Create(ColorToARGB(Clr));
  graphic.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
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

function CreateConcaveRectangle(R: TRect; Direction: TButtonDirection): TGPGraphicsPath;
var
  l, t, w, h: Integer;
  points : array[0..5] of TGPPoint;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;

  if (Direction = bdLeft) then
  begin
    Result.AddArc(l, t, h, h, 90, 180);
    Result.AddLine(l + h, t, l + w, t);
    points[0].X := (l + w); points[0].Y := t;
    points[1].X := (l + w - h div 6); points[1].Y := (t + h div 4);
    points[2].X := (round(l + w - (h / 4.7))); points[2].Y := (t + h div 2);
    points[3].X := (l + w - h div 6); points[3].Y := (t + 3 * h div 4);
    points[4].X := (l + w); points[4].Y := (t + h);
    Result.AddCurve(PGPPoint(@points), 5);
    Result.AddLine(l + h, t + h, l + w, t + h);
  end
  else
  begin
    points[0].X := l; points[0].Y := t;
    points[1].X := (l + h div 6); points[1].Y := (t + h div 4);
    points[2].X := (round(l + (h / 4.85))); points[2].Y := (t + h div 2);
    points[3].X := (l + h div 6); points[3].Y := (t + 3 * h div 4);
    points[4].X := l; points[4].Y := (t + h);
    Result.AddCurve(PGPPoint(@points), 5);
    Result.AddLine(l, t + h, l + w - h, t + h);
    Result.AddArc(l + w - h, t, h, h, 90, -180);
    Result.AddLine(l + w - h, t, l, t);
  end;
  Result.CloseFigure();
end;

//------------------------------------------------------------------------------

procedure DrawConcaveRectangle(graphic: TGPGraphics; R: TRect; Direction: TButtonDirection; Clr: TColor);
var
  l, t, w, h: Integer;
  points : array[0..5] of TGPPoint;
  gppen: TgpPen;
  path: TGPGraphicsPath;
begin
  path := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;

  if (Direction = bdLeft) then
  begin
    path.AddArc(l, t, h, h, 90, 180);
    path.AddLine(l + h, t, l + w, t);
    points[0].X := (l + w); points[0].Y := t;
    points[1].X := (l + w - h div 6); points[1].Y := (t + h div 4);
    points[2].X := (round(l + w - (h / 4.7))); points[2].Y := (t + h div 2);
    points[3].X := (l + w - h div 6); points[3].Y := (t + 3 * h div 4);
    points[4].X := (l + w); points[4].Y := (t + h);
    path.AddCurve(PGPPoint(@points), 5);
    path.AddLine(l + h, t + h, l + w, t + h);
  end
  else
  begin
    points[0].X := l; points[0].Y := t;
    points[1].X := (l + h div 6); points[1].Y := (t + h div 4);
    points[2].X := (round(l + (h / 4.85))); points[2].Y := (t + h div 2);
    points[3].X := (l + h div 6); points[3].Y := (t + 3 * h div 4);
    points[4].X := l; points[4].Y := (t + h);
    path.AddCurve(PGPPoint(@points), 5);
    path.AddLine(l, t + h, l + w - h, t + h);
    path.AddArc(l + w - h, t, h, h, 90, -180);
    path.AddLine(l + w - h, t, l, t);
  end;
  path.CloseFigure();
  gppen := TGPPen.Create(ColorToARGB(Clr));
  graphic.DrawPath(gppen, path);
  gppen.Free;
end;

//------------------------------------------------------------------------------

function CreateTopConcaveRectangle(R: TRect; Direction: TButtonDirection): TGPGraphicsPath;
var
  l, t, w, h: Integer;
  points : array[0..2] of TGPPoint;
begin
  Result := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;

  if (Direction = bdLeft) then
  begin
    Result.AddArc(l, t, h * 2, h * 2, 180, 90);
    Result.AddLine(l + h, t, l + w, t);
    points[0].X := (l + w); points[0].Y := t;
    points[1].X := (l + w - h div 3); points[1].Y := (t + h div 2);
    points[2].X := (round(l + w - (h / 2.35))); points[2].Y := (t + h);
    Result.AddCurve(PGPPoint(@points), 3);
  end
  else
  begin
    points[0].X := l; points[0].Y := t;
    points[1].X := (l + h div 3); points[1].Y := (t + h div 2);
    points[2].X := (round(l + (h / 2.35))); points[2].Y := (t + h);
    Result.AddCurve(PGPPoint(@points), 3);
    Result.AddLine(round(l + (h / 2.35)), t + h, l + w - h, t + h);
    Result.AddArc(l + w - h * 2, t, h * 2, h * 2, 0, -90);
  end;
  Result.CloseFigure();
end;

//------------------------------------------------------------------------------

function CreateTopRoundRectangle(R: TRect; Radius: Integer): TGPGraphicsPath;
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
  Result.AddLine(l + w, t + radius, l + w, t + h); // right
  Result.AddLine(l + w, t + h, l, t + h); // bottom
  Result.AddLine(l, t + h, l, t + radius); // left
  Result.CloseFigure();
end;

//------------------------------------------------------------------------------

function CreateBottomRadialPath(R: TRect): TGPGraphicsPath;
var
  Rf: TGPRectf;
begin
  Result := TGPGraphicsPath.Create;
  Rf := MakeRect(R.Left, R.Top, R.Right - R.Left, R.Bottom - R.Top);
  Rf.X := Rf.X - Rf.Width * 0.35;
  Rf.Y := Rf.Y - Rf.Height * 0.15;
  Rf.Width := Rf.Width * 1.7;
  Rf.Height := Rf.Height * 2.3;
  Result.AddEllipse(Rf);
  Result.CloseFigure;
end;

//------------------------------------------------------------------------------

procedure DrawRoundRect(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
begin
  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(PC),1);
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
end;

//------------------------------------------------------------------------------
{
procedure DrawDottedRoundRect(graphics: TGPGraphics; PC: TColor; X,Y,Width,Height,Radius: integer);
var
  path:TGPGraphicsPath;
  gppen:TGPPen;
begin
  path := TGPGraphicsPath.Create;
  gppen := tgppen.Create(ColorToARGB(PC),1);
  gppen.SetDashStyle(DashStyleDot);
  path.AddLine(X + radius, Y, X + width - (radius*2), Y);
  path.AddArc(X + width - (radius*2), Y, radius*2, radius*2, 270, 90);
  path.AddLine(X + width, Y + radius, X + width, Y + height - (radius*2));
  path.AddArc(X + width - (radius*2), Y + height - (radius*2), radius*2, radius*2,0,90);
  path.AddLine(X + width - (radius*2), Y + height, X + radius, Y + height);
  path.AddArc(X, Y + height - (radius*2), radius*2, radius*2, 90, 90);
  path.AddLine(X, Y + height - (radius*2), X, Y + radius);
  path.AddArc(X, Y, radius*2, radius*2, 180, 90);
  path.CloseFigure;
  graphics.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
end;
}
procedure DrawDottedRoundRect(graphic: TGPGraphics; R: TRect; Radius: Integer; Clr: TColor);
var
  path: TGPGraphicsPath;
  l, t, w, h, d: Integer;
  gppen: TGPPen;
begin
  if not Assigned(graphic) then
    Exit;
  path := TGPGraphicsPath.Create;
  l := R.Left;
  t := R.Top;
  w := R.Right;
  h := R.Bottom;
  d := Radius shl 1;
  path.AddArc(l, t, d, d, 180, 90); // topleft
  path.AddLine(l + radius, t, l + w - radius, t); // top
  path.AddArc(l + w - d, t, d, d, 270, 90); // topright
  path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
  path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
  path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
  path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
  path.AddLine(l, t + h - radius, l, t + radius); // left
  path.CloseFigure();
  gppen := TGPPen.Create(ColorToARGB(Clr));
  graphic.DrawPath(gppen, path);
  gppen.Free;
  path.Free;
end;

//------------------------------------------------------------------------------

function DrawButtonContent(Canvas: TCanvas; r: TRect; Caption:string; WideCaption: widestring; DrawCaption: Boolean; AFont: TFont;
   Images: TImageList; ImageIndex: Integer; EnabledImage: Boolean; Layout: TButtonLayout;
   DropDownButton: Boolean; Enabled: Boolean; Focus: Boolean; DropDownPos: TDropDownPosition;
   Picture: TGDIPPicture; AntiAlias: TAntiAlias; DrawPic: Boolean; Glyph: TBitmap; Hot: boolean;
   OverlapText, WordWrap, AutoSize, Rounded: Boolean; RoundRadius: Integer; Spacing: integer;
   Trimming: TStringTrimming; ButtonShape: TButtonShape; ButtonDir: TButtonDirection): TSize;
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
  ImgX, ImgY, ImgW, ImgH: Integer;
  BtnR, DwR: TRect;
  szRect: TRect;
  tm: TTextMetric;
  ttf: boolean;
  Radius: integer;
  uformat,wwformat: Cardinal;
  tdrect: TRect;
  th: integer;
  DCaption: string;
  DWCaption: widestring;
begin
  BtnR := R;

  if Rounded then
    Radius := RoundRadius
  else
    Radius := 0;

  if not DrawCaption then
  begin
    DCaption := '';
    DWCaption := '';
  end
  else
  begin
    DCaption := Caption;
    DWCaption := WideCaption;
  end;


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

  //h2 := h div 2;

  // Create GDI+ canvas
  graphics := TGPGraphics.Create(Canvas.Handle);

  {if not Transparent then
  begin

    if DropDownButton and (DrawDwLine) and DropDownSplit then
    begin
      if DropDownPos = dpRight then
      begin
        DR1 := Rect(r.Right - 12, r.Top + h2 - 1, r.Right, r.Bottom);
        DR2 := Rect(r.Right - 12, r.Top, r.Right, r.Bottom - h2);
        BR1 := Rect(r.Left, r.Top + h2 - 1, r.Right - 12, r.Bottom);
        BR2 := Rect(r.Left, r.Top, r.Right - 12, r.Bottom - h2);
      end
      else
      begin
        DR1 := Rect(r.Left, r.Bottom - 6, r.Right, r.Bottom);
        DR2 := Rect(r.Left, r.Bottom - 12, r.Right, r.Bottom - 6);

        DR2 := Rect(r.Left, r.Bottom - 12, r.Right, r.Bottom);

        h2d := (r.Bottom - r.Top - 12) div 2;
        BR1 := Rect(r.Left, r.Top + h2d - 1, r.Right, r.Bottom - 12);
        BR2 := Rect(r.Left, r.Top, r.Right, r.Bottom - 12 - h2d);
      end;

    end
    else
    begin
    end;

  end;}

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);

  if Focus then // Draw focus line
  begin
    //graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    //DrawRoundRect(graphics, $E4AD89,r.Left + 4,r.Top + 4, r.Right - 4, r.Bottom - 4, Radius);
    graphics.SetSmoothingMode(SmoothingModeAntiAlias);
    //DrawDottedRoundRect(graphics, clGray,r.Left + 4,r.Top + 4, r.Right - 8, r.Bottom - 8, Radius);
    case (ButtonShape) of
      btsCurvedRectangle: DrawConcaveRectangle(graphics, Rect(R.Left + 6, r.Top + 5, r.Right - 12, r.Bottom - 10), ButtonDir, clGray);
      btsRectangle: DrawDottedRoundRect(graphics, Rect(R.Left + 4, r.Top + 4, r.Right - 8, r.Bottom - 8), Radius-2, clGray);
    end;
  end;

  fontFamily := TGPFontFamily.Create(AFont.Name);

  fs := 0;

  ImgX := 0;
  ImgY := 0;
  ImgH := 0;
  ImgW := 0;

  if (fsBold in AFont.Style) then
    fs := fs + 1;
  if (fsItalic in AFont.Style) then
    fs := fs + 2;
  if (fsUnderline in AFont.Style) then
    fs := fs + 4;

  if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
  begin
    ImgW := Glyph.Width;
    ImgH := Glyph.Height;

  end
  else if Assigned(Picture) and not Picture.Empty then
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

  if DrawCaption and (DCaption <> '') then
    if (ImgW > 0) then
      ImgW := ImgW + Spacing;

  Result.cx := ImgW;
  Result.cy := ImgH;

  if (DCaption <> '') or (DWCaption <> '') then
  begin
    if pos('\n',DCaption) > 0 then
    begin
      DCaption := StringReplace(caption, '\n', #10#13, [rfReplaceAll, rfIgnoreCase]);
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

    font := TGPFont.Create(fontFamily, AFont.Size , fs, UnitPoint);

    w := BtnR.Right - BtnR.Left;
    h := BtnR.Bottom - BtnR.Top;

    x1 := r.Left;
    y1 := r.Top;
    x2 := w;
    y2 := h;

    if AutoSize then
    begin
      x2 := 4096;
      y2 := 4096;
    end;

    rectf := MakeRect(x1,y1,x2,y2);

    if WordWrap then
      stringFormat := TGPStringFormat.Create(0)
    else
      stringFormat := TGPStringFormat.Create(GDIP_NOWRAP);

    if Enabled then
      solidBrush := TGPSolidBrush.Create(ColorToARGB(AFont.Color))
    else
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clGray));

    // Center-justify each line of text.
   // stringFormat.SetAlignment(StringAlignmentCenter);
    case Layout of
      blGlyphLeftAdjusted: stringFormat.SetAlignment(StringAlignmentNear);
      blGlyphRightAdjusted: stringFormat.SetAlignment(StringAlignmentFar);
      else stringFormat.SetAlignment(StringAlignmentCenter);
    end;

    // Center the block of text (top to bottom) in the rectangle.

    case Layout of
      blGlyphTopAdjusted: stringFormat.SetLineAlignment(StringAlignmentNear);
      blGlyphBottomAdjusted: stringFormat.SetLineAlignment(StringAlignmentFar);
      else stringFormat.SetLineAlignment(StringAlignmentCenter);
    end;

    stringFormat.SetHotkeyPrefix(HotkeyPrefixShow);
    stringFormat.SetTrimming(Trimming);

    case AntiAlias of
    aaClearType:graphics.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
    aaAntiAlias:graphics.SetTextRenderingHint(TextRenderingHintAntiAlias);
    end;

    if (AntiAlias = aaNone) or not ttf then
    begin
      Canvas.Font.Assign(AFont);
      szRect.Left := round(rectf.X);
      szRect.Top := round(rectf.Y);

      szRect.Right := szRect.Left + (BtnR.Right - BtnR.Left - 4);
      szRect.Bottom := DrawText(Canvas.Handle,PChar(DCaption),Length(DCaption), szrect, DT_CALCRECT or DT_LEFT or DT_WORDBREAK{ or DT_VCENTER});

      sizeRect.Width := szRect.Right - szRect.Left;
      sizeRect.Height := szRect.Bottom - szRect.Top;

      case Layout of
        blGlyphLeft:
        begin
          sizeRect.X := (w - (szRect.Right - szRect.Left) - ImgW) div 2;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + Round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphLeftAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphTop:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := (h - (szRect.Bottom - szRect.Top) - ImgH - 2) div 2;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
        blGlyphTopAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
        blGlyphRight:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphRightAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphBottom:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
        blGlyphBottomAdjusted:
        begin
          sizeRect.X := szRect.Left;
          sizeRect.Y := szRect.Top;
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
      end;
      //Result.cx := ImgW + Spacing + round(sizerect.Width);
      //Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
    end
    else
    begin
      if DCaption <> '' then
        graphics.MeasureString(DCaption, Length(DCaption), font, rectf, stringFormat, sizerect)
      else
        graphics.MeasureString(DWCaption, Length(DWCaption), font, rectf, stringFormat, sizerect);

      case Layout of
        blGlyphLeft, blGlyphLeftAdjusted, blGlyphRight, blGlyphRightAdjusted:
        begin
          Result.cx := ImgW + Spacing + round(sizerect.Width);
          Result.cy := Max(ImgH + Spacing, Spacing + round(sizerect.Height));
        end;
        blGlyphTop, blGlyphTopAdjusted, blGlyphBottom, blGlyphBottomAdjusted:
        begin
          Result.cx := Max(ImgW + Spacing, Spacing + round(sizerect.Width));
          Result.cy := ImgH + Spacing + round(sizerect.Height);
        end;
      end;
    end;

    if not AutoSize then
    begin
      if not WordWrap then
      begin
        x2 := w;
        y2 := h;
        rectf := MakeRect(x1,y1,x2,y2);
      end;

//      if (ImgW > 0) then
      begin
        case Layout of
          blGlyphLeft:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              x1 := r.Left + 2 + ImgW;
              x2 := w - 2 - ImgW;
              ImgX := round(sizeRect.X);
            end
            else
            begin
              x1 := r.Left + 2 + ImgW;
              x2 := w - 2 - ImgW;
              ImgX := round(sizerect.X - ImgW div 2);
            end;
            if ImgX < 2 then ImgX := 2;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
          end;
          blGlyphLeftAdjusted:
          begin
            x1 := r.Left + 2 + ImgW;
            if (ButtonShape = btsCurvedRectangle) then
              x1 := x1 + (h div 4);

            x2 := w - 2 - ImgW;

            //ImgX := round(sizerect.X - ImgW div 2);
            ImgX := round(x1 - ImgW);
            if ImgX < 2 then ImgX := 2;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
          end;
          blGlyphTop:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              y1 := r.Top + ImgH;
 //             y1 := sizeRect.Y + ImgH;
              y2 := h - 2 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
//              ImgY := round(sizeRect.Y);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(y1) - ImgH + ImgY - 4;
            end
            else
            begin
              y1 := r.Top + ImgH;
              y2 := h - 2 - ImgH;
              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(y1) - ImgH + ImgY;
            end;
            if ImgY < 2 then ImgY := 2;
          end;
          blGlyphTopAdjusted:
          begin
            y1 := r.Top{ + 2} + ImgH;
            y2 := h - 2 - ImgH;

            ImgX := r.Left + Max(0, (w - ImgW) div 2);
            if Layout = blGlyphTopAdjusted then
              ImgY := 0 //force to top margin
            else
              ImgY := round(y2 - sizerect.Height);
            ImgY := Max(0, ImgY div 2);
            ImgY := round(y1) - ImgH + ImgY; //round(sizerect.Height) - ImgY - 4;
            if ImgY < 2 then ImgY := 2;
          end;
          blGlyphRight, blGlyphRightAdjusted:
          begin
            x1 := 2;
            x2 := w - 4 - ImgW;
            if Layout = blGlyphRightAdjusted then
            begin
              if (ButtonShape = btsCurvedRectangle) then
              begin
                x2 := x2 - (h div 4);
                ImgX := w - ImgW - 2 - (h div 4);
              end
              else
                ImgX := w - ImgW - 2;
            end
            else
            begin

               ImgX := round(X2 - sizerect.width);
               ImgX := Max(0, ImgX div 2);
               ImgX := ImgX + round(sizerect.width) + 4;
               if ImgX > (w - ImgW) then
                 ImgX := w - ImgW - 2;
            end;
            ImgY := r.Top + Max(0, (h - ImgH) div 2);
          end;
          blGlyphBottom:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              y1 := 2;
              y2 := h - 2 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := round(y2 - sizerect.Height);
              ImgY := Max(0, ImgY div 2);
              ImgY := round(sizerect.Height + 5) + ImgY;
              if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
            end
            else
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
          blGlyphBottomAdjusted:
          begin
            if (AntiAlias = aaNone) or not ttf then
            begin
              y1 := 2;
              y2 := h - 4 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              ImgY := (h - ImgH - 2);
            end
            else
            begin
              y1 := 2;
              y2 := h - 2 - ImgH;

              ImgX := r.Left + Max(0, (w - ImgW) div 2);
              if Layout = blGlyphBottomAdjusted then
                ImgY := h; //force to bottom margin

              ImgY := Max(0, ImgY div 2);
              ImgY := round(sizerect.Height + 2) + ImgY;
              if ImgY > (h - ImgH) then ImgY := h - ImgH - 2;
            end;
          end;
        end;
      end;

      if OverlapText then
        rectf := MakeRect(r.Left, r.Top, r.Right, r.Bottom)
      else
        rectf := MakeRect(x1, y1, x2, y2);

      if DrawPic and OverlapText then
      begin
        if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
          Canvas.Draw(ImgX, ImgY, Glyph);
      end;

      if DrawCaption then
      begin
        if (AntiAlias = aaNone) or not ttf then
        begin
          szRect.Left := round(rectf.X);
          szRect.Top := round(rectf.Y);
          szRect.Right := szRect.Left + round(rectf.Width);
          szRect.Bottom := szRect.Top + round(rectf.Height);
          Canvas.Brush.Style := bsClear;

          if WordWrap then
            wwformat := DT_WORDBREAK
          else
            wwformat := DT_SINGLELINE;

          uformat := DT_VCENTER or wwformat;
          case Layout of
            blGlyphLeft:
            begin
              uformat := DT_VCENTER or wwformat or DT_CENTER;
              szrect.Left := szrect.Left;
            end;
            blGlyphLeftAdjusted:
            begin
              uformat := DT_VCENTER or wwformat or DT_LEFT;
              szrect.Left := szrect.Left + 2;
            end;
            blGlyphTop:
            begin
              uformat := DT_TOP or wwformat or DT_CENTER or DT_VCENTER;
            end;
            blGlyphTopAdjusted: uformat := DT_TOP or wwformat or DT_CENTER;
            blGlyphRight: uformat := DT_VCENTER or wwformat or DT_CENTER;
            blGlyphRightAdjusted: uformat := DT_VCENTER or wwformat or DT_RIGHT;
            blGlyphBottom: uformat := DT_VCENTER or wwformat or DT_CENTER;
            blGlyphBottomAdjusted: uformat := DT_BOTTOM or wwformat or DT_CENTER;
          end;

          tdrect := szrect;

          if WordWrap then
          begin
            th := DrawText(Canvas.Handle,PChar(DCaption),Length(DCaption), szrect, uformat or DT_CALCRECT);

            case Layout of
            blGlyphTopAdjusted:
              begin
                // do nothing
              end;
            blGlyphTop:
              begin
                tdrect.Top := ImgY + ImgH;

                tdrect.Top := tdrect.Top + (tdrect.Bottom - tdrect.Top - th) div 2;

              end;
            blGlyphBottomAdjusted:
              begin
                tdrect.Top := tdrect.Bottom - th;
              end;
            else
              begin
                tdrect.Top := (tdrect.Bottom - tdrect.Top - th) div 2;
              end;
            end;
          end;

          DrawText(Canvas.Handle,PChar(DCaption),Length(DCaption), tdrect, uformat);
        end
        else
        begin
          if (DCaption <> '') then
            graphics.DrawString(DCaption, Length(DCaption), font, rectf, stringFormat, solidBrush)
          else
            graphics.DrawString(DWCaption, Length(DWCaption), font, rectf, stringFormat, solidBrush);
        end;
      end;
    end;

    stringformat.Free;
    solidBrush.Free;
    font.Free;
  end;

  fontFamily.Free;

  if not AutoSize then
  begin
    if DropDownButton then
    begin
      if DropDownPos = dpRight then
        w := w - 8
      else
        h := h - 8;
    end;

    if DrawPic and not OverlapText then
    begin
      if Assigned(Glyph) and not Glyph.Empty and (Glyph.Width > 1) and (Glyph.Height > 1) then
      begin
        if (DCaption = '') and (DWCaption = '') then
          Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Glyph)
        else
          Canvas.Draw(ImgX, ImgY, Glyph);
      end
      else
        if Assigned(Picture) and not Picture.Empty then
        begin
          if (DCaption = '') and (DWCaption = '') then
            Canvas.Draw(r.Left + Max(0, (w - ImgW) div 2), r.Top + Max(0, (h - ImgH) div 2), Picture)
          else
            Canvas.Draw(ImgX, ImgY, Picture);
        end
        else
          if (ImageIndex <> -1) and Assigned(Images) then
          begin
            if (DCaption = '') and (DWCaption = '') then
              Images.Draw(Canvas, r.Left + Max(0, (w - Images.Width) div 2), r.Top + Max(0, (h - Images.Height) div 2), ImageIndex, EnabledImage)
            else
            begin
              Images.Draw(Canvas, ImgX, ImgY, ImageIndex, EnabledImage);
            end;
          end;
    end;


    Canvas.Brush.Style := bsClear;

    if DropDownButton then
    begin
      {if DrawDwLine and DropDownSplit then
      begin
        Canvas.Pen.Color := ColorToRGB(PC);
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
        DrawArrow(Canvas, AP, clGray, clWhite, DropDir)
      else
        DrawArrow(Canvas, AP, clBlack, clWhite, DropDir); }
    end;
  end;

  graphics.Free;
end;

//------------------------------------------------------------------------------

{$IFDEF DELPHI6_LVL}

{ TAdvGlassButtonActionLink }

procedure TAdvGlassButtonActionLink.AssignClient(AClient: TObject);
begin
  inherited AssignClient(AClient);
  FClient := AClient as TAdvCustomGlassButton;
end;

//------------------------------------------------------------------------------

function TAdvGlassButtonActionLink.IsCheckedLinked: Boolean;
begin
  Result := inherited IsCheckedLinked {and (FClient.GroupIndex <> 0) and
    FClient.AllowAllUp} and (FClient.Down = (Action as TCustomAction).Checked);
end;

//------------------------------------------------------------------------------

function TAdvGlassButtonActionLink.IsGroupIndexLinked: Boolean;
begin
  Result := (FClient is TAdvCustomGlassButton) and
    (TAdvCustomGlassButton(FClient).GroupIndex = (Action as TCustomAction).GroupIndex);
end;

//------------------------------------------------------------------------------

procedure TAdvGlassButtonActionLink.SetChecked(Value: Boolean);
begin
  if IsCheckedLinked then TAdvCustomGlassButton(FClient).Down := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvGlassButtonActionLink.SetGroupIndex(Value: Integer);
begin
  if IsGroupIndexLinked then TAdvCustomGlassButton(FClient).GroupIndex := Value;
end;

{$ENDIF}

//------------------------------------------------------------------------------

{ TAdvCustomGlassButton }

constructor TAdvCustomGlassButton.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIPicture := TGDIPPicture.Create;
  FIPicture.OnChange := OnPictureChanged;

  FIPictureHot := TGDIPPicture.Create;
  FIPictureDown := TGDIPPicture.Create;

  FIPictureDisabled := TGDIPPicture.Create;
  FIPictureDisabled.OnChange := OnPictureChanged;

  SetBounds(0, 0, 100, 32);
  //ControlStyle := [csCaptureMouse, csDoubleClicks];
  ParentFont := True;

  // make sure to use a Truetype font
  Font.Name := 'Tahoma';

  FOffSet := 4;

  FStyle := tasButton;
  FGroupIndex := 0;
  FGrouped := true;

  FUnHotTimer := TTimer.Create(self);
  FUnHotTimer.Interval := 1;
  FUnHotTimer.Enabled := false;
  FUnHotTimer.OnTimer := UnHotTimerOnTime;

  FOfficeHint := TAdvHintInfo.Create;
  FShortCutHint := nil;
  FShortCutHintPos := shpTop;
  FShortCutHintText := '';

  ShowHint := False;

  FBackgroundColor := clNone;
  FBackColor := clBlack;
  FForeColor := clWhite;
  FOuterBorderColor := clWhite;
  FInnerBorderColor := clBlack;
  FShineColor := clWhite;
  FGlowColor := $00FFBD8D;  //ColorToARGB($0FF8DBDF)(0xFF8DBDFF);
  FLayout := blGlyphLeft;
  FAntiAlias := aaClearType;
  FShowCaption := True;
  FShowFocusRect := False;
  FCornerRadius := 4;
  FBackGroundSymbol := bsNone;
  FBackGroundSymbolColor := clWhite;
  FButtonShape := btsRectangle;
  FButtonDirection := bdRight;
  FForceTransparent := False;
  TabStop := true;
end;

//------------------------------------------------------------------------------

destructor TAdvCustomGlassButton.Destroy;
begin
  if Assigned(FShortCutHint) then
    FShortCutHint.Free;
  FIPicture.Free;
  FIPictureHot.Free;
  FIPictureDown.Free;
  FIPictureDisabled.Free;
  FUnHotTimer.Free;
  FOfficeHint.Free;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMDialogChar(var Message: TCMDialogChar);
begin
  with Message do
    if IsAccel(CharCode, Caption) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMFontChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;  
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if (csDesigning in ComponentState) then
    Exit;

  FMouseInControl := true;
  FUnHotTimer.Enabled := True;

  if Assigned(FOnMouseEnter) then
     FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMMouseLeave(var Message: TMessage);
begin
  inherited;

  if (csDesigning in ComponentState) then
    exit;

  FUnHotTimer.Enabled := False;
  FMouseInControl := false;
  FHot := false;

  if Assigned(FOnMouseLeave) then
     FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMSysColorChange(var Message: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMTextChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.Loaded;
begin
  inherited;

  if (Down <> FInitialDown) then
    Down := FInitialDown;

  if not assigned(OnDblClick) then
    ControlStyle := ControlStyle - [csDoubleClicks];
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.MouseDown(Button: TMouseButton;
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

  if TabStop and CanFocus then
    SetFocus;
      
  FDragging := True;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.MouseMove(Shift: TShiftState; X,
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

procedure TAdvCustomGlassButton.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;

  if not (csDestroying in ComponentState) and (AOperation = opRemove) then
  begin
  end;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.WMLButtonDblClk(var Message: TWMLButtonDown);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.WndProc(var Message: TMessage);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.DrawTransparentBackGround(Canvas: TCanvas);
var
  Rgn1: HRGN;
  R: TRect;
  i: Integer;
  p: TPoint;
begin
  if Assigned(Canvas) and Assigned(Parent) then
  begin
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
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.Paint;
var
  Rgn1: HRGN;
  R, R1, R2, R3: TRect;
  i, j: Integer;
  p: TPoint;
  bmp: TBitmap;
begin
  R := ClientRect;
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

  //inherited;

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

    if FForceTransparent and Assigned(Parent) then
    begin
      R := Rect(Left, Top, Left + Width, Top + Height);
      for j:= 0 to Parent.ControlCount-1 do
      begin
        if (Parent.Controls[j] <> Self) and (Parent.Controls[j] is TAdvCustomGlassButton) then
        begin
          R2 := Rect(Parent.Controls[j].Left, Parent.Controls[j].Top, Parent.Controls[j].Left + Parent.Controls[j].Width, Parent.Controls[j].Top + Parent.Controls[j].Height);
          IntersectRect(R3, R, R2);
          if (R3.Left > 0) or (R3.Right > 0) or (R3.Top > 0) or (R3.Bottom > 0) then
          begin
            bmp := TBitmap.Create;
            try
              bmp.Height := TWinControl(Parent.Controls[j]).Height;
              bmp.Width := TWinControl(Parent.Controls[j]).Width;

              R1 := Rect(R3.Left - Left, R3.Top - Top, R3.Right - Left, R3.Bottom - Top);
              R2 := Rect(R3.Left - TAdvCustomGlassButton(Parent.Controls[j]).Left, R3.Top - TAdvCustomGlassButton(Parent.Controls[j]).Top, R3.Right - TAdvCustomGlassButton(Parent.Controls[j]).Left, R3.Bottom - TAdvCustomGlassButton(Parent.Controls[j]).Top);
              bmp.Canvas.CopyMode := cmSrcCopy;
              bmp.Canvas.CopyRect(R2, Canvas, R1);
              TAdvCustomGlassButton(Parent.Controls[j]).DrawButton(bmp.Canvas);

              R1 := Rect(R3.Left - Left, R3.Top - Top, R3.Right - Left, R3.Bottom - Top);
              R2 := Rect(R3.Left - TAdvCustomGlassButton(Parent.Controls[j]).Left, R3.Top - TAdvCustomGlassButton(Parent.Controls[j]).Top, R3.Right - TAdvCustomGlassButton(Parent.Controls[j]).Left, R3.Bottom - TAdvCustomGlassButton(Parent.Controls[j]).Top);
              Canvas.CopyMode := cmSrcCopy;
              Canvas.CopyRect(R1, bmp.Canvas, R2);
            finally
              bmp.free;
            end;
          end;
        end;
      end;
    end;
  end;

  DrawButton(Canvas);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.DrawButton(ACanvas: TCanvas);
var
  Pic: TGDIPPicture;
  graphic: TGPGraphics;
  R: TRect;
  DrawFocused: Boolean;
begin
  R := ClientRect;
  Pic := Picture;
  if not Enabled and not PictureDisabled.Empty then
    Pic := PictureDisabled
  else if ((FMouseDownInControl and FMouseInControl) or fDown) and not PictureDown.Empty then
    Pic := PictureDown
  else if FMouseInControl and not PictureHot.Empty then
    Pic := PictureHot;

  graphic := TGPGraphics.Create(ACanvas.Handle);
  try
    graphic.SetSmoothingMode(SmoothingModeAntiAlias);
    DrawButtonBackground(graphic);
  finally
    graphic.Free;
  end;

  DrawFocused := (GetFocus = self.Handle) and ShowFocusRect;
  DrawButtonContent(ACanvas, R, Caption, '', ShowCaption, Font, nil, -1, True, Layout, False, Enabled, DrawFocused,
    dpRight, Pic, AntiAlias, True, nil, False, False, True, False{AutoSize}, True{Rounded}, FCornerRadius, 2, StringTrimmingNone, ButtonShape, ButtonDirection);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.DrawButtonBackground(graphic: TGPGraphics);
var
  R, R2, R3: TRect;
  path: TGPGraphicsPath;
  solidBrush: TGPSolidBrush;
  LinearGradBrush: TGPLinearGradientBrush;
  Rgn: TGPRegion;
  pathGradBrush: TGPPathGradientBrush;
  colors : array[0..0] of TGPColor;
  points : array[0..5] of TGPPoint;
  count: Integer;
  opacity, h, w, offset: Integer;
  r1, g, b: byte;
  gppen: TGPPen;
  d: double;
begin
  offset := 15;
  R := ClientRect;
  Dec(R.Right);
  Dec(R.Bottom);
  R3 := R;
  h := R3.Bottom - R3.Top;
  w := R3.Right - R3.Left;

  case (ButtonShape) of
    btsRectangle: DrawRoundRectangle(graphic, R, FCornerRadius, OuterBorderColor);
    btsCurvedRectangle:
    begin
      path := CreateConcaveRectangle(R, ButtonDirection);
      gppen := TGPPen.Create(ColorToARGB(OuterBorderColor));
      graphic.DrawPath(gppen, path);
      gppen.Free;
      path.Free;
    end;
  end;

  Inc(R.Left);
  Inc(R.Top);
  R.Right := R.Right - 2;
  R.Bottom := R.Bottom - 2;
  R2 := R;
  R2.Bottom := R2.Bottom shr 1;

  // semi transparent body
  case (ButtonShape) of
    btsCurvedRectangle:
    begin
      path := CreateConcaveRectangle(R, ButtonDirection);
    end;
    else path := CreateRoundRectangle(R, FCornerRadius);
  end;


  if FBackgroundColor <> clNone then
  begin
    opacity := 255;
    r1 := GetRValue(FBackgroundColor);
    g := GetGValue(FBackgroundColor);
    b := GetBValue(FBackgroundColor);
    solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
    graphic.FillPath(solidBrush, path);
    solidBrush.Free;
  end;

  opacity := 127;

  //graphic.SetCompositingQuality(CompositingQualityGammaCorrected);

  r1 := GetRValue(BackColor);
  g := GetGValue(BackColor);
  b := GetBValue(BackColor);
  solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
  graphic.FillPath(solidBrush, path);
  solidBrush.Free;
  path.Free;

  // Glow
  if ((FMouseDownInControl or Down or (State in [absDown, absExclusive]))) then
  begin
    case (ButtonShape) of
      btsCurvedRectangle:
      begin
        path := CreateConcaveRectangle(R, ButtonDirection);
        Rgn := TGPRegion.Create(path);
        graphic.SetClip(Rgn);
        path.Free;
      end;
      else  // btsRectangle
      begin
        path := CreateRoundRectangle(R, FCornerRadius);
        Rgn := TGPRegion.Create(path);
        graphic.SetClip(Rgn);
        path.Free;
      end;
    end;

    path := CreateBottomRadialPath(R);
    pathGradBrush := TGPPathGradientBrush.Create(path);
    opacity := 173;
    r1 := GetRValue(GlowColor);
    g := GetGValue(GlowColor);
    b := GetBValue(GlowColor);

    pathGradBrush.SetCenterColor(MakeColor(opacity, r1, g, b));
    colors[0] := MakeColor(0, r1, g, b);
    count := 1;

    pathGradBrush.SetSurroundColors(@colors, count);
    graphic.FillPath(pathGradBrush, path);

    if Assigned(Rgn) then
    begin
      graphic.ResetClip();
      Rgn.Free;
    end;

    path.Free;
    pathGradBrush.Free;
  end;

  // shine
  if ((R2.Right > 0) and (R2.Bottom > 0)) then
  begin
    Inc(R2.Bottom);
    case (ButtonShape) of
      btsCurvedRectangle:
      begin
        path := CreateTopConcaveRectangle(R2, ButtonDirection);
      end;
      else path := CreateTopRoundRectangle(R2, FCornerRadius);
    end;

    Inc(R2.Bottom);
    opacity := 153;
    r1 := GetRValue(ShineColor);
    g := GetGValue(ShineColor);
    b := GetBValue(ShineColor);
    LinearGradBrush := TGPLinearGradientBrush.Create(MakeRect(R2.Left, R2.Top, R2.Right - R2.Left, R2.Bottom - R2.Top), MakeColor(Opacity, r1, g, b), MakeColor(Opacity div 3, r1, g, b), LinearGradientModeVertical);
    graphic.FillPath(LinearGradBrush, path);
    R2.Bottom := R2.Bottom - 2;
    LinearGradBrush.Free;
    path.Free;
  end;

  // BackGround Image/Symbol
  if (FBackGroundSymbolColor <> clNone) and (BackGroundSymbol <> bsNone) then
  begin
    opacity := 160;
    r1 := GetRValue(FBackGroundSymbolColor);
    g := GetGValue(FBackGroundSymbolColor);
    b := GetBValue(FBackGroundSymbolColor);
    case (FBackGroundSymbol) of
      bsArrowLeft:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), h / 4);
        gppen.SetEndCap(LineCapArrowAnchor);
        path := TGPGraphicsPath.Create;
        path.AddLine(w - w / 5, h / 2, w / 8, h / 2);
        graphic.DrawPath(gppen, path);
        gppen.Free;
        path.Free;
      end;
      bsArrowRight:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), h / 4);
        gppen.SetEndCap(LineCapArrowAnchor);
        path := TGPGraphicsPath.Create;
        path.AddLine(w / 6, h / 2, w - w / 8, h / 2);
        graphic.DrawPath(gppen, path);
        gppen.Free;
        path.Free;
      end;
      bsArrowUp:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), h / 4);
        gppen.SetEndCap(LineCapArrowAnchor);
        path := TGPGraphicsPath.Create;
        path.AddLine(w / 2, h - h / 5, w / 2, h / 8);
        graphic.DrawPath(gppen, path);
        gppen.Free;
        path.Free;
      end;
      bsArrowDown:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), h / 4);
        gppen.SetEndCap(LineCapArrowAnchor);
        path := TGPGraphicsPath.Create;
        path.AddLine(w / 2, h / 5, w / 2, h - h / 8);
        graphic.DrawPath(gppen, path);
        gppen.Free;
        path.Free;
      end;
      bsPlay:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        points[0].X := (w div 4 + w div 20); points[0].Y := (h div 4);
        points[1].X := (W - W div 4 + W div 20); points[1].Y := (h div 2);
        points[2].X := (W div 4 + W div 20); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        solidBrush.Free;
      end;
      bsPause:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        graphic.FillRectangle(solidBrush, W / 4, H / 4, (W / 2 - W / 10) / 2, H / 2);
        graphic.FillRectangle(solidBrush, W / 2 + W / 20, H / 4, (W / 2 - W / 10) / 2, H / 2);
        solidBrush.Free;
      end;
      bsStop:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        graphic.FillRectangle(solidBrush, W / 4 + W / 20, H / 4 + H / 20, W / 2 - W / 10, H / 2 - W / 10);
        solidBrush.Free;
      end;
      bsFastForward:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 4); points[0].Y := (H div 4);
        points[1].X := (W div 2); points[1].Y := (h div 2);
        points[2].X := (W div 4); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2); points[0].Y := (H div 4);
        points[1].X := (3 * W div 4); points[1].Y := (h div 2);
        points[2].X := (W div 2); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        graphic.FillRectangle(solidBrush, 3 * W / 4, H / 4, W / 12, H / 2);
        solidBrush.Free;
      end;
      bsForward:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 4 + W div 12); points[0].Y := (H div 4);
        points[1].X := (W div 2 + W div 12); points[1].Y := (h div 2);
        points[2].X := (W div 4 + W div 12); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2 + W div 12); points[0].Y := (H div 4);
        points[1].X := (3 * W div 4 + W div 12); points[1].Y := (h div 2);
        points[2].X := (W div 2 + W div 12); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        solidBrush.Free;
      end;
      bsBackward:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 4 - W div 12); points[0].Y := (H div 2);
        points[1].X := (W div 2 - W div 12); points[1].Y := (h div 4);
        points[2].X := (W div 2 - W div 12); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2 - W div 12); points[0].Y := (H div 2);
        points[1].X := (3 * W div 4 - W div 12); points[1].Y := (h div 4);
        points[2].X := (3 * W div 4 - W div 12); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        solidBrush.Free;
      end;
      bsFastBackward:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 4); points[0].Y := (H div 2);
        points[1].X := (W div 2); points[1].Y := (h div 4);
        points[2].X := (W div 2); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2); points[0].Y := (H div 2);
        points[1].X := (3 * W div 4); points[1].Y := (h div 4);
        points[2].X := (3 * W div 4); points[2].Y := (H - H div 4);
        path.AddPolygon(PGPPoint(@points), 3);
        graphic.FillPath(solidBrush, path);
        path.Free;
        graphic.FillRectangle(solidBrush, W / 4 - W / 12, H / 4, W / 12, H / 2);
        solidBrush.Free;
      end;
      bsSpeaker:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), w / 20);
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2 - W div 6 - W div offset); points[0].Y := (H div 4 + H div 10);
        points[1].X := (W div 2 - W div offset); points[1].Y := (H div 4 + H div 10);
        points[2].X := (W div 2 + W div 5 - W div offset); points[2].Y := (H div 4);
        points[3].X := (W div 2 + W div 5 - W div offset); points[3].Y := (3 * H div 4);
        points[4].X := (W div 2 - W div offset); points[4].Y := (3 * H div 4 - H div 10);
        points[5].X := (W div 2 - W div 6 - W div offset); points[5].Y := (3 * H div 4 - H div 10);
        path.AddPolygon(PGPPoint(@points), 6);
        graphic.DrawPath(gppen, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        path.AddLine(W / 2 - W / offset, H / 4 + H / 10 + W / 40, W / 2 - W / offset, H - (H / 4 + H / 10 + W / 40));
        graphic.DrawPath(gppen, path);
        gppen.Free;
        path.Free;
      end;
      bsNoSpeaker:
      begin
        gppen := TGPPen.Create(MakeColor(opacity, r1, g, b), w / 20);
        path := TGPGraphicsPath.Create;
        points[0].X := (W div 2 - W div 6 - W div offset); points[0].Y := (H div 4 + H div 10);
        points[1].X := (W div 2 - W div offset); points[1].Y := (H div 4 + H div 10);
        points[2].X := (W div 2 + W div 5 - W div offset); points[2].Y := (H div 4);
        points[3].X := (W div 2 + W div 5 - W div offset); points[3].Y := (3 * H div 4);
        points[4].X := (W div 2 - W div offset); points[4].Y := (3 * H div 4 - H div 10);
        points[5].X := (W div 2 - W div 6 - W div offset); points[5].Y := (3 * H div 4 - H div 10);
        path.AddPolygon(PGPPoint(@points), 6);
        graphic.DrawPath(gppen, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        path.AddLine(W / 2 - W / offset, H / 4 + H / 10 + W / 40, W / 2 - W / offset, H - (H / 4 + H / 10 + W / 40));
        graphic.DrawPath(gppen, path);
        path.Free;
        path := TGPGraphicsPath.Create;
        path.AddLine(round(W / 2 - W / 3.5 - W / offset), 3 * H / 4 - H / 10, W / 2 + W / 3 - W / offset, H / 4 + H / 12 + W / 40);
        graphic.DrawPath(gppen, path);
        path.Free;
        gppen.Free;
      end;
      bsRecord:
      begin
        solidBrush := TGPSolidBrush.Create(MakeColor(opacity, r1, g, b));
        path := TGPGraphicsPath.Create;
        if (h < w) then
          d := h / 2
        else
          d := w / 2;
        path.AddEllipse((w - d) / 2, (h - d) / 2, d, d);
        graphic.FillPath(solidBrush, path);
        path.Free;
        solidBrush.Free;
      end;
      bsCustom:
      begin
        DoCustomBackgroundSymbol(Graphic,Rect(0,0,W,H));
      end;
    end;
  end;

  case (ButtonShape) of
    btsRectangle: DrawRoundRectangle(graphic, R, FCornerRadius, InnerBorderColor);
    btsCurvedRectangle:
    begin
      path := CreateConcaveRectangle(R, ButtonDirection);
      gppen := TGPPen.Create(ColorToARGB(InnerBorderColor));
      graphic.DrawPath(gppen, path);
      gppen.Free;
      path.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------


procedure TAdvCustomGlassButton.UpdateExclusive;
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

    {if Assigned(FAdvToolBar) and not (Parent is TAdvCustomToolBar) then
      FAdvToolBar.Broadcast(Msg)
    else if Assigned(AdvToolBar) and (Parent is TAdvCustomToolBar) and Assigned(AdvToolBar.FOptionWindowPanel) then
      FAdvToolBar.FOptionWindowPanel.Broadcast(Msg);}
  end;
end;

//------------------------------------------------------------------------------

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.UpdateTracking;
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

procedure TAdvCustomGlassButton.SetAllowAllUp(Value: Boolean);
begin
  if FAllowAllUp <> Value then
  begin
    FAllowAllUp := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetDown(Value: Boolean);
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

procedure TAdvCustomGlassButton.SetGroupIndex(Value: Integer);
begin
  if FGroupIndex <> Value then
  begin
    FGroupIndex := Value;
    UpdateExclusive;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetState(const Value: TAdvButtonState);
begin
  if FState <> Value then
  begin
    FState := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetStyle(const Value: TAdvToolButtonStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.InvalidateMe;
begin
  invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;

end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetGrouped(const Value: Boolean);
begin
  FGrouped := Value;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.ButtonDown;
begin
end;

//------------------------------------------------------------------------------


procedure TAdvCustomGlassButton.CMButtonPressed(var Message: TMessage);
var
  Sender: TAdvCustomGlassButton;
begin
  if integer(Message.WParam) = FGroupIndex then
  begin
    Sender := TAdvCustomGlassButton(Message.LParam);
    if Sender <> Self then
    begin
      if Sender.Down and FDown then
      begin
        FDown := False;
        FState := absUp;
        if (Action is TCustomAction) then
          TCustomAction(Action).Checked := False;
        Invalidate;
      end;
      FAllowAllUp := Sender.AllowAllUp;
    end;
  end;
end;


//------------------------------------------------------------------------------

{$IFDEF DELPHI6_LVL}

procedure TAdvCustomGlassButton.ActionChange(Sender: TObject; CheckDefaults: Boolean);
begin
  inherited ActionChange(Sender, CheckDefaults);
  if Sender is TCustomAction then
    with TCustomAction(Sender) do
    begin
      if CheckDefaults or (Self.GroupIndex = 0) then
        Self.GroupIndex := GroupIndex;
      //Self.ImageIndex := ImageIndex;
    end;
end;

//------------------------------------------------------------------------------

function TAdvCustomGlassButton.GetActionLinkClass: TControlActionLinkClass;
begin
  Result := TAdvGlassButtonActionLink;
end;
{$ENDIF}

//------------------------------------------------------------------------------

function TAdvCustomGlassButton.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

function TAdvCustomGlassButton.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetVersion(const Value: string);
begin

end;

//------------------------------------------------------------------------------

function TAdvCustomGlassButton.GetHot: Boolean;
begin
  Result := FPropHot;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetHot(const Value: Boolean);
var
  OldV: Boolean;
begin
  OldV := FPropHot;
  FPropHot := Value;
  if (State <> absUp) then
    FPropHot := false;

  {if Assigned(FAdvToolBar) then
    FAdvToolBar.UpdateButtonHot(self)
  else }
    FPropHot := false;
  if OldV <> FPropHot then
    InvalidateMe;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.UnHotTimerOnTime(Sender: TObject);
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

    {if Assigned(FAdvToolBar) then
      if not (FAdvToolBar.FInMenuLoop and FAdvToolBar.FMenuFocused) then
        Hot := False; }

    if Enabled then
      InvalidateMe;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetParent(AParent: TWinControl);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetOfficeHint(const Value: TAdvHintInfo);
begin
  FOfficeHint.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetIPicture(const Value: TGDIPPicture);
begin
  FIPicture.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetIPictureDisabled(const Value: TGDIPPicture);
begin
  FIPictureDisabled.Assign(Value);
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetIPictureDown(const Value: TGDIPPicture);
begin
  FIPictureDown.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetIPictureHot(const Value: TGDIPPicture);
begin
  FIPictureHot.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.OnPictureChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.Click;
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.HideShortCutHint;
begin
  if Assigned(FShortCutHint) then
  begin
    FShortCutHint.Visible := false;
    FShortCutHint.Free;
    FShortCutHint := nil;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.ShowShortCutHint;
var
  pt: TPoint;
begin
  if not Assigned(Parent) then
    Exit;

  if not Assigned(FShortCutHint) then
  begin
    FShortCutHint := TShortCutHintWindow.Create(Self);
    FShortCutHint.Visible := False;
    FShortCutHint.Parent := nil;
    FShortCutHint.ParentWindow := Parent.Handle;
  end;

  FShortCutHint.Caption := FShortCutHintText;

  pt := ClientToScreen(Point(0,0));

  case ShortCutHintPos of
  shpLeft:
    begin
      FShortCutHint.Left := pt.X - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  shpTop:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y - (FShortCutHint.Height div 2);
    end;
  shpRight:
    begin
      FShortCutHint.Left := pt.X + self.Width - (FShortCutHint.Width div 2);
      FShortCutHint.Top := pt.Y + (self.Height - FShortCutHint.Height) div 2;
    end;
  shpBottom:
    begin
      FShortCutHint.Left := pt.X + (self.Width - FShortCutHint.Width) div 2;
      FShortCutHint.Top := pt.Y + self.Height - (FShortCutHint.Height div 2);
    end;
  end;

  FShortCutHint.Visible := true;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMHintShow(var Message: TMessage);
begin
  if (Message.WParam = 1) then
  begin
    if (Message.LParam = 0) then
    begin
      HideShortCutHint;
    end
    else if (Message.LParam = 1) then
    begin
      ShowShortCutHint;
    end;
    Message.Result := 1;
  end;

  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetBackColor(const Value: TColor);
begin
  if (FBackColor <> Value) then
  begin
    FBackColor := Value;
    Invalidate;
  end;
end;

procedure TAdvCustomGlassButton.SetBackgroundColor(const Value: TColor);
begin
  if (FBackgroundColor <> Value) then
  begin
    FBackgroundColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetForeColor(const Value: TColor);
begin
  if (FForeColor <> Value) then
  begin
    FForeColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetInnerBorderColor(const Value: TColor);
begin
  if (FInnerBorderColor <> Value) then
  begin
    FInnerBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetOuterBorderColor(const Value: TColor);
begin
  if (FOuterBorderColor <> Value) then
  begin
    FOuterBorderColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetShineColor(const Value: TColor);
begin
  if (FShineColor <> Value) then
  begin
    FShineColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetGlowColor(const Value: TColor);
begin
  if (FGlowColor <> Value) then
  begin
    FGlowColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.WMSetText(var Message: TWMSetText);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetLayout(const Value: TButtonLayout);
begin
  if (FLayout <> Value) then
  begin
    FLayout := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetAntiAlias(const Value: TAntiAlias);
begin
  if (FAntiAlias <> Value) then
  begin
    FAntiAlias := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetShowCaption(const Value: Boolean);
begin
  if (FShowCaption <> Value) then
  begin
    FShowCaption := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetShowFocusRect(const Value: Boolean);
begin
  if (FShowFocusRect <> Value) then
  begin
    FShowFocusRect := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMFocusChanged(
  var Message: TCMFocusChanged);
begin
  inherited;
  FActive := Message.Sender = Self;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CMDialogKey(var Message: TCMDialogKey);
begin
  with Message do
    if
      (((CharCode = VK_RETURN) and FActive) {or
      ((CharCode = VK_ESCAPE) and FCancel)}) and
      (KeyDataToShiftState(Message.KeyData) = []) and CanFocus then
    begin
      Click;
      Result := 1;
    end
    else
      inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.KeyPress(var Key: Char);
begin
  inherited;

  if (Key = #32) then
  begin
    Click;
    //if Assigned(OnClick) then
      //OnClick(Self);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.CNCommand(var Message: TWMCommand);
begin
  if Message.NotifyCode = BN_CLICKED then
    Click;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.DoCustomBackgroundSymbol(Graphics: TGPGraphics;
  ARect: TRect);
begin
  if Assigned(FOnCustomBackgroundSymbol) then
    FOnCustomBackgroundSymbol(Self, ARect, Graphics);
end;

procedure TAdvCustomGlassButton.DoEnter;
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.DoExit;
begin
  inherited;
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.WMLButtonUp(var Msg: TWMLButtonDown);
var
  DoClick: Boolean;
begin
  if (csDesigning in ComponentState) then
  begin
    inherited;
    Exit;
  end;

  FMouseDownInControl := false;
  InvalidateMe;

  if FDragging then
  begin
    FDragging := False;
    DoClick := (Msg.XPos >= 0) and (Msg.XPos < ClientWidth) and (Msg.YPos >= 0) and (Msg.YPos <= ClientHeight);
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

  inherited;
  Invalidate;
end;

procedure TAdvCustomGlassButton.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
begin
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

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetCornerRadius(Value: Integer);
begin
  if (FCornerRadius <> Value) and (Value >= 0) then
  begin
    if (csDesigning in ComponentState) then
      Value := Min(Value, Height div 2);
    FCornerRadius := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetBackGroundSymbol(
  const Value: TBackGroundSymbol);
begin
  if (FBackGroundSymbol <> Value) then
  begin
    FBackGroundSymbol := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetBackGroundSymbolColor(
  const Value: TColor);
begin
  if (FBackGroundSymbolColor <> Value) then
  begin
    FBackGroundSymbolColor := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetButtonShape(
  const Value: TButtonShape);
begin
  if (FButtonShape <> Value) then
  begin
    FButtonShape := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetButtonDirection(
  const Value: TButtonDirection);
begin
  if (FButtonDirection <> Value) then
  begin
    FButtonDirection := Value;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCustomGlassButton.SetForceTransparent(const Value: Boolean);
begin
  if (FForceTransparent <> Value) then
  begin
    FForceTransparent := Value;
    Invalidate;
  end;
end;

end.

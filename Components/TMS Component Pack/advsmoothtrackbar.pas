{**************************************************************************}
{ TAdvSmoothTrackBar Component                                             }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2010 - 2015                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}
unit AdvSmoothTrackBar;

{$I TMSDEFS.inc}

interface

uses
  Classes, SysUtils, Controls, GDIPFill, Graphics, Windows,
  AdvStyleIF, Math, ExtCtrls, Messages, CommCtrl,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;


const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 5; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  //version history
  // v1.0.0.0 : First Release
  // v1.0.0.1 : Improved: Event OnPositionChanged called when user programmatically changes position
  //            or click above or under thumb
  // v1.1.0.0 : New : Keyboard support to increase or decrease value
  //          : New : Focus indication on thumb fill
  // v1.1.0.1 : Fixed: issue with saving width and height
  // v1.1.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.1.2.0 : New : Built-in support for reduced color set for use with terminal servers
  //          : Fixed : Issue with cursor not showing
  // v1.1.3.0 : New : Delphi 2010 Touch support
  // v1.1.4.0 : New : Event to customize drawing of numbers and tickmarks
  //          : New : Exposed event OnPositionChanging
  // v1.2.0.0 : New : PositionToolTip / PositionToolTipFormat added
  //          : Fixed : Issue with interaction when glowanimation is false
  //          : Improved : OnPositionChanging and OnPositionChanged with keyboard interaction
  // v1.2.1.0 : New : Added Event OnGetToolTipText
  // v1.2.1.1 : Improved : Transparency
  // v1.2.1.2 : Fixed : Issue with setting Step to 0
  // v1.2.2.0 : New : Built-in support for Office 2010 colors
  // v1.2.3.0 : New : OnDrawNumber event expanded with extra customization possibilities
  // v1.2.4.0 : New : ReadOnly property
  // v1.3.0.0 : New : Metro style support
  // v1.3.0.1 : Fixed : Issue with divide by zero when Max = Min
  // v1.3.0.2 : Improved : Performance when Max is a very large value
  // v1.4.0.0 : New : Windows 8, Office 2013 styles added
  // v1.4.1.0 : New : ShowThumb and ShowInnerBorder properties
  // v1.5.0.0 : New : Windows 10, Office 2016 styles added

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothTrackBarDirection = (dVertical, dHorizontal);

  TAdvSmoothTrackBarValueChanged = procedure(Sender: TObject; Position: Double) of object;

  TAdvSmoothTrackBarDrawNumber = procedure(Sender: TObject; value: Double; var s: String; var AllowDrawText: Boolean; var TickMarkColorLeft: TColor; var TickMarkColorRight: TColor;
    var AllowDrawTickMarkLeft: Boolean; var AllowDrawTickMarkRight: Boolean) of object;

  TAdvSmoothTrackBarGetToolTipText = procedure(Sender: TObject; Position: Double; var ToolTipText: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
   TAdvSmoothTrackBar = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FPrevPosSet: Boolean;
    FPrevPos: Double;
    FDoGlow, FFocused: Boolean;
    FGlowCount: integer;
    FGlowPos: Double;  
    FAnimationTimer: TTimer;
    FLoaded: Boolean;
    FMouseDownOnSlider: Boolean;
    FSliderPos: integer;
    FDesignTime: Boolean;  
    FResize: Boolean;
    FOldW, FOldH: integer;
    FDirection: TAdvSmoothTrackBarDirection;
    FFill: TGDIPFill;
    FProgressFill: TGDIPFill;
    FFont: TFont;
    FThumbFill: TGDIPFill;
    FMaximum: Double;
    FFormat: String;
    FMinimum: Double;
    FStep: Double;
    FThumbSize: integer;
    FPosition: Double;
    FTickMarkSize: integer;
    FShowValues: Boolean;
    FShowTickMarks: Boolean;
    FSnapToTickMarks: Boolean;
    FSnapMargin: Double;
    FOnPositionChanged: TAdvSmoothTrackBarValueChanged;
    FGlowAnimation: Boolean;
    FShowProgress: Boolean;
    FTickMarkColor: TColor;
    FProgressFont: TFont;
    FHToolTip: THandle;
    FToolTipText: string;
    FOnDrawNumber: TAdvSmoothTrackBarDrawNumber;
    FOnPositionChanging: TAdvSmoothTrackBarValueChanged;
    FPositionToolTipFormat: string;
    FPositionToolTip: boolean;
    FOnGetToolTipText: TAdvSmoothTrackBarGetToolTipText;
    FReadOnly: Boolean;
    FShowThumb: Boolean;
    FShowInnerBorder: Boolean;
    procedure SetDirection(const Value: TAdvSmoothTrackBarDirection);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetProgressFill(const Value: TGDIPFill);
    procedure SetFont(const Value: TFont);
    procedure SetThumbFill(const Value: TGDIPFill);
    procedure SetFormat(const Value: String);
    procedure SetMaximum(const Value: Double);
    procedure SetMinimum(const Value: Double);
    procedure SetStep(const Value: Double);
    procedure setThumbSize(const Value: integer);
    procedure SetPosition(const Value: Double);
    procedure SetShowTickMarks(const Value: Boolean);
    procedure SetShowValues(const Value: Boolean);
    procedure SetTickMarkSize(const Value: integer);
    procedure SetSnapToTickMarks(const Value: Boolean);
    procedure SetSnapMargin(const Value: Double);
    procedure SetGlowAnimation(const Value: Boolean);
    procedure SetShowProgress(const Value: Boolean);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetTickMarkColor(const Value: TColor);
    procedure SetProgressFont(const Value: TFont);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetReadOnly(const Value: Boolean);
    procedure SetShowInnerBorder(const Value: Boolean);
    procedure SetShowThumb(const Value: Boolean);
  protected
    procedure CreateToolTip;
    procedure AddToolTip(IconType: Integer; Text, Title: string);
    procedure DestroyToolTip;
    procedure UpdateToolTip;
    procedure ClearToolTip;

    procedure WndProc(var Message: TMessage); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;  
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawOverlay(g: TGPGraphics);
    procedure DrawProgress(g: TGPGraphics);
    procedure DrawSlider(g: TGPGraphics);
    procedure DrawNumbers(g: TGPGraphics);
    procedure AnimateGlow(Sender: TObject);
    procedure DoExit; override;
    procedure DoEnter; override;
    function IsGlowAnimation: Boolean;
    function InsideRect: TRect;
    function GetNumRect: TGPRectF;
    function GetSliderRect: TGPRectF;
    function GetPosition: Double; overload;
    function GetPosition(XYPos: Double): Double; overload;
    function GetVersionNr: integer;
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property Direction: TAdvSmoothTrackBarDirection read FDirection write SetDirection default dVertical;
    property Fill: TGDIPFill read FFill write SetFill;
    property ProgressFill: TGDIPFill read FProgressFill write SetProgressFill;
    property ThumbFill: TGDIPFill read FThumbFill write SetThumbFill;
    property ThumbSize: integer read FThumbSize write setThumbSize default 40;
    property Font: TFont read FFont write SetFont;
    property ProgressFont: TFont read FProgressFont write SetProgressFont;
    property Minimum: Double read FMinimum write SetMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Step: Double read FStep write SetStep;
    property Position: Double read FPosition write SetPosition;
    property PositionToolTip: boolean read FPositionToolTip write FPositionToolTip default false;
    property PositionToolTipFormat: string read FPositionToolTipFormat write FPositionToolTipFormat;
    property ValueFormat: String read FFormat write SetFormat;
    property ShowValues: Boolean read FShowValues write SetShowValues default true;
    property ShowTickMarks: Boolean read FShowTickMarks write SetShowTickMarks default true;
    property TickMarkSize: integer read FTickMarkSize write SetTickMarkSize default 15;
    property TickMarkColor: TColor read FTickMarkColor write SetTickMarkColor default clBlack;
    property SnapToTickMarks: Boolean read FSnapToTickMarks write SetSnapToTickMarks default true;
    property SnapMargin: Double read FSnapMargin write SetSnapMargin;
    property GlowAnimation: Boolean read FGlowAnimation write SetGlowAnimation default true;
    property ShowProgress: Boolean read FShowProgress write SetShowProgress default true;
    property Version: string read GetVersion write SetVersion;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default false;
    property ShowThumb: Boolean read FShowThumb write SetShowThumb default True;
    property ShowInnerBorder: Boolean read FShowInnerBorder write SetShowInnerBorder default True;

    property OnPositionChanged: TAdvSmoothTrackBarValueChanged read FOnPositionChanged write FOnPositionChanged;
    property OnPositionChanging: TAdvSmoothTrackBarValueChanged read FOnPositionChanging write FOnPositionChanging;
    property OnDrawNumber: TAdvSmoothTrackBarDrawNumber read FOnDrawNumber write FOnDrawNumber;
    property OnGetToolTipText: TAdvSmoothTrackBarGetToolTipText read FOnGetToolTipText write FOnGetToolTipText;
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
    property TabStop default true;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

{$IFNDEF DELPHI2007_LVL}
const TTM_POPUP = WM_USER + 34;
{$ENDIF}

implementation

procedure DrawFocus(g: TGPGraphics; r: TGPRectF; rn: Integer; rt: TFillRoundingType);
var
  pathfocus: TGPGraphicsPath;
  pfocus: TGPPen;
begin
  pathfocus := GDIPFill.CreateRoundRectangle(r, rn, rt, false);
  g.SetSmoothingMode(SmoothingModeDefault);
  pfocus := TGPPen.Create(MakeColor(255, clBlack), 1);
  pfocus.SetDashStyle(DashStyleDot);
  g.DrawPath(pfocus, pathfocus);
  pfocus.Free;
  pathfocus.Free;
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
  case RoundingType of
    rtNone:
    begin
      Result.AddLine(l, t, l + w, t); // top
      Result.AddLine(l + w, t, l + w, t + h); // right
      Result.AddLine(l + w, t + h, l, t + h); // bottom
      Result.AddLine(l, t + h, l, t); // left
    end;
    rtTop:
    begin
      Result.AddArc(l, t, d, d, 180, 90); // topleft
      Result.AddLine(l + radius, t, l + w - radius, t); // top
      Result.AddArc(l + w - d, t, d, d, 270, 90); // topright
      Result.AddLine(l + w, t + radius, l + w, t + h); // right
      Result.AddLine(l + w, t + h, l, t + h); // bottom
      Result.AddLine(l, t + h, l, t + Radius); // left
    end;
    rtBottom:
    begin
      Result.AddLine(l, t, l + w, t); // top
      Result.AddLine(l + w, t, l + w, t + h - radius); // right
      Result.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      Result.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      Result.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      Result.AddLine(l, t + h - Radius, l, t ); // left
    end;
    rtBoth:
    begin
      Result.AddArc(l, t, d, d, 180, 90); // topleft
      Result.AddLine(l + radius, t, l + w - radius, t); // top
      Result.AddArc(l + w - d, t, d, d, 270, 90); // topright
      Result.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      Result.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      Result.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      Result.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      Result.AddLine(l, t + h - radius, l, t + radius); // left
    end;
  end;
  Result.CloseFigure();
end;

{ TAdvSmoothTrackBar }

procedure TAdvSmoothTrackBar.AnimateGlow(Sender: TObject);
var
  endpos: Double;
begin
  if IsGlowAnimation then
  begin
    FGlowPos := FGlowPos + 3;
    endpos := 0;
    case Direction of
      dVertical: endpos := GetPosition;
      dHorizontal: endpos := GetPosition;
    end;

    if FGlowPos > endpos then
    begin
      FGlowCount := 0;
      FGlowPos := -60;
      FDoGlow := false;
    end
    else
      FGlowCount := FGlowCount + 10;

    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothTrackBar) then
  begin
    FDirection := (Source as TAdvSmoothTrackBar).Direction;
    FFill.Assign((Source as TAdvSmoothTrackBar).Fill);
    FProgressFill.Assign((Source as TAdvSmoothTrackBar).ProgressFill);
    FThumbFill.Assign((Source as TAdvSmoothTrackBar).ThumbFill);
    FThumbSize := (Source as TAdvSmoothTrackBar).ThumbSize;
    FFont.Assign((Source as TAdvSmoothTrackBar).Font);
    FMinimum := (Source as TAdvSmoothTrackBar).Minimum;
    FMaximum := (Source as TAdvSmoothTrackBar).Maximum;
    FStep := (Source as TAdvSmoothTrackBar).Step;
    FPosition := (Source as TAdvSmoothTrackBar).Position;
    FFormat := (Source as TAdvSmoothTrackBar).ValueFormat;
    FShowValues := (Source as TAdvSmoothTrackBar).ShowValues;
    FShowTickMarks := (Source as TAdvSmoothTrackBar).ShowTickMarks;
    FTickMarkSize := (Source as TAdvSmoothTrackBar).TickMarkSize;
    FSnapToTickMarks := (Source as TAdvSmoothTrackBar).SnapToTickMarks;
    FSnapMargin := (Source as TAdvSmoothTrackBar).SnapMargin;
    FGlowAnimation := (Source as TAdvSmoothTrackBar).GlowAnimation;
    FTickMarkColor := (Source as TAdvSmoothTrackBar).TickMarkColor;
    FShowThumb := (Source as TAdvSmoothTrackBar).ShowThumb;
    FShowInnerBorder := (Source as TAdvSmoothTrackBar).ShowInnerBorder;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.Changed;
begin
  Invalidate;
end;

constructor TAdvSmoothTrackBar.Create(AOwner: TComponent);
begin
  inherited;
  FReadOnly := false;
  FShowThumb := True;
  FShowInnerBorder := True;
  FAnimationTimer := TTimer.Create(self);
  FAnimationTimer.Interval := 10;
  FAnimationTimer.Enabled := true;  
  FAnimationTimer.OnTimer := AnimateGlow;
  FSliderPos := 0;
  DoubleBuffered := true;
  FResize := false;
  Width := 90;
  Height := 250;
  FOldW := Height;
  FOldH := Width;
  FDirection := dVertical;
  FResize := true;  
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FProgressFill := TGDIPFill.Create;
  FProgressFill.OnChange := FillChanged;
  FFont := TFont.Create;
  FFont.OnChange := FontChanged;
  FProgressFont := TFont.Create;
  FProgressFont.OnChange := FontChanged;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  FProgressFont.Name := 'Tahoma';  
  {$ENDIF}
  FThumbFill := TGDIPFill.Create;
  FThumbFill.OnChange := FillChanged;
  FMaximum := 100;
  FMinimum := 0;
  FStep := 20;
  FFormat := '%g db';
  FThumbSize := 40;
  FTickMarkSize := 15;
  FShowValues := true;
  FShowTickMarks := true;
  FSnapToTickMarks := true;
  FSnapMargin := 10;
  FGlowAnimation := true;
  FShowProgress := true;
  FGlowCount := 3000; //3sec
  FGlowPos := -40;
  FTickMarkColor := clBlack;
  FGlowPos := -60;
  FGlowCount := 0;
  FDoGlow := false;
  FPositionToolTip := false;
  FPositionToolTipFormat := '%.2f';

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
  begin
    SetComponentStyle(tsOffice2007Luna);
//    ThumbFill.Rounding := 10;
//    ThumbFill.BorderColor := clGray;
//    ThumbFill.Color := clwhite;
//    ThumbFill.ColorTo := clWhite;
//    ThumbFill.GradientType := gtForwardDiagonal;
//    ThumbFill.OpacityTo := 0;
//
//    Fill.GradientType := gtSolid;
//    Fill.Color := $ABC6C9;
//
//    ProgressFill.GradientType := gtVertical;
//    ProgressFill.Color := $A4F8FB;
//    ProgressFill.ColorTo := $3FF210;
  end;
end;

procedure TAdvSmoothTrackBar.CreateWnd;
begin
  inherited;
  FLoaded := true;
  SetDirection(Direction);

  CreateToolTip;
  AddToolTip(3,'TAdvSmoothTrackBar','ToolTip');
end;

procedure TAdvSmoothTrackBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TAdvSmoothTrackBar.Destroy;
begin
  FFill.Free;
  FProgressFill.Free;
  FThumbFill.Free;
  FFont.Free;
  FProgressFont.Free;
  inherited;
end;

procedure TAdvSmoothTrackBar.CreateToolTip;
begin
  fhToolTip := CreateWindowEx(0, 'Tooltips_Class32', nil,  TTS_NOPREFIX,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),Integer(CW_USEDEFAULT),
    Integer(CW_USEDEFAULT), Handle, 0, hInstance, nil);

  if fhToolTip <> 0 then
    SetWindowPos(fhToolTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TAdvSmoothTrackBar.DestroyToolTip;
begin
  DestroyWindow(fhToolTip);
end;

procedure TAdvSmoothTrackBar.AddToolTip(IconType: Integer; Text, Title: string);
var
  Item: THandle;
  Rect: TRect;
  ti: TToolInfo;
begin
  Item := self.Handle;

  if (Item <> 0) AND (Windows.GetClientRect(Item, Rect)) then
  begin
    ti.cbSize := SizeOf(TToolInfo);
    ti.uFlags := TTF_SUBCLASS or TTF_IDISHWND;
    ti.hInst := hInstance;

    ti.hwnd := Item;
    ti.Rect := Rect;
    ti.uId := Handle;
    ti.lpszText :=  '';
    SendMessage(fhToolTip, TTM_ADDTOOL, 0, LParam(@ti));
  end;
end;

procedure TAdvSmoothTrackbar.UpdateToolTip;
var
  ti: TToolInfo;
  NewText: string;
  Rect: TRect;
begin
  NewText := Format(FPositionToolTipFormat,[FPosition]);

  if NewText <> FToolTipText then
  begin
    if Assigned(OnGetToolTipText) then
      OnGetToolTipText(Self, FPosition, NewText);
    Windows.GetClientRect(handle, Rect);
    ti.cbSize := sizeof(ti);
    ti.lpszText := PChar(NewText);
    ti.uFlags := TTF_SUBCLASS or TTF_IDISHWND;
    ti.hInst := hInstance;
    ti.uId := Handle;
    ti.Rect := Rect;
    ti.hwnd := Handle;
    SendMessage(fhtooltip, TTM_UPDATETIPTEXT, 0, LParam(@ti));
    FToolTipText := NewText;
  end;
end;

procedure TAdvSmoothTrackbar.ClearToolTip;
var
  ti: TToolInfo;
  Rect: TRect;
begin
  SendMessage(FHTooltip, TTM_POP, 0,0);
  Windows.GetClientRect(Handle, Rect);
  ti.cbSize := SizeOf(ti);
  ti.lpszText := '';
  ti.uFlags := TTF_SUBCLASS or TTF_IDISHWND;
  ti.hInst := hInstance;
  ti.uId := Handle;
  ti.Rect := Rect;
  ti.hwnd := Handle;
  SendMessage(FHTooltip, TTM_UPDATETIPTEXT, 0, LParam(@ti));
  FToolTipText := '';
end;

procedure TAdvSmoothTrackBar.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothTrackBar.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothTrackBar.DrawBackGround(g: TGPGraphics);
begin
  FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
end;

procedure TAdvSmoothTrackBar.DrawNumbers;
var
  numr, sizer: TGPRectF;
  pos, st: Double;
  ff: TGPFontFamily;
  fs: integer;
  f: TGPFont;
  sf: TGPStringFormat;
  I: Integer;
  s: String;
  b: TGPSolidBrush;
  pt: TGPPointF;
  pLeft, pRight: TGPPen;
  numCount: integer;
  y, pty: single;
  altickleft, altickright, alnum: Boolean;
  tickcleft, tickcright: TColor;
begin
  if (not ShowValues) and (not ShowTickMarks) then
    Exit;

  g.SetTextRenderingHint(TextRenderingHintAntiAlias);

  if Step > 0 then
    numCount := Round((Maximum - Minimum) / Step)
  else
    numCount := 1;

  if numCount = 0 then
    numCount := 1;

  pos := 0;
  st := 0;
  case Direction of
    dVertical:
    begin
     numr := GetNumRect;
     st := numr.Height / numcount;
     pos := numr.Height + numr.Y;
    end;
    dHorizontal:
    begin
     numr := GetNumRect;
     st := numr.Width / numcount;
     pos := numr.Width + numr.X;
    end;
  end;

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

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, Font.Size, fs, UnitPoint);

  for I := 0 to numcount do
  begin
    try
      s := Format(FFormat, [I * step + Minimum]);
    except
      s := '';
    end;

    altickright := true;
    altickleft := True;
    tickcleft := TickMarkColor;
    tickcright := TickMarkColor;

    alnum := true;
    if Assigned(OnDrawNumber) then
      OnDrawNumber(Self, I * step + Minimum, s, alnum, tickcleft, tickcright, altickleft, altickright);

    pLeft := TGPPen.Create(MakeColor(125, tickcleft), 2);
    pRight := TGPPen.Create(MakeColor(125, tickcright), 2);

    g.MeasureString(s, Length(s), f, numr, sf, sizer);
    case Direction of
      dVertical:
      begin
        pt := MakePoint((numr.Width - sizer.Width) / 2, pos - (sizer.Height / 2));
        if ShowTickMarks and altickleft then
          g.DrawLine(pleft, numr.X, pos, numr.X + TickMarkSize, pos);

        if ShowTickMarks and altickright then
          g.DrawLine(pright, numr.X + numr.Width - TickMarkSize, pos, numr.X + numr.Width, pos);
      end;
      dHorizontal:
      begin
        pt := MakePoint(numr.X + numr.Width - pos + (ThumbSize / 2) - (sizer.Width / 2), (numr.Height - sizer.Height) / 2);
        //draw tickmarks
        if ShowTickMarks and altickleft then
          g.DrawLine(pleft, pos, numr.Y , pos, numr.Y + TickMarkSize);
        if ShowTickMarks and altickright then
          g.DrawLine(pright, pos, numr.Y + numr.Height - TickMarkSize, pos, numr.Y + numr.Height);
      end;
    end;

    pleft.Free;
    pRight.Free;

    if ShowValues and alnum then
    begin
      //draw numbers
      b := nil;
      case Direction of
        dVertical:
        begin
          pty := pt.Y;
          y := InsideRect.Bottom - GetPosition - (ThumbSize / 2) - (sizer.Height / 2);
          if (pty < y) then
            b := TGPSolidBrush.Create(MakeColor(255, FFont.Color))
          else
            b := TGPSolidBrush.Create(MakeColor(255, FProgressFont.Color));
        end;
        dHorizontal:
        begin
          pty := pt.X;
          y := GetPosition + (ThumbSize / 2) - (sizer.Width / 2);
          if (pty > y) then
            b := TGPSolidBrush.Create(MakeColor(255, FFont.Color))
          else
            b := TGPSolidBrush.Create(MakeColor(255, FProgressFont.Color));
        end;
      end;

      g.DrawString(s, Length(s), f, pt,  sf, b);
      b.Free;
    end;
    pos := pos - st;
  end;
  f.Free;
  sf.Free;
  ff.Free;
end;

procedure TAdvSmoothTrackBar.DrawOverlay(g: TGPGraphics);
var
  olr, toplr: TGPRectF;
  path: TGPGraphicsPath;
  rgn: TGPRegion;
  b: TGPBrush;
  p: TGPPen;
begin
  if not ShowInnerBorder then
    Exit;

  toplr := MakeRect(InsideRect.Left, InsideRect.Top, InsideRect.Right, InsideRect.Bottom);
  olr := MakeRect(InsideRect.Left + 7, InsideRect.Top + 7, InsideRect.Right - 14, InsideRect.Bottom - 14);
  path := CreateRoundRectangle(olr, 8, rtBoth);

  rgn := TGPRegion.Create(toplr);
  rgn.Exclude(path);
  g.SetClip(rgn);
  b := TGPSolidBrush.Create(MakeColor(75, clWhite));
  g.FillRectangle(b, toplr);
  g.ResetClip;
  rgn.Free;
  b.free;

  path.Free;

  path := CreateRoundRectangle(Makerect(olr.X, olr.Y, olr.Width, olr.Height), 10, rtBoth);
  p := TGPPen.Create(MakeColor(225, clWhite), 2);
  g.DrawPath(p, path);
  p.Free;
  path.Free;
end;

procedure TAdvSmoothTrackBar.DrawProgress(g: TGPGraphics);
var
  b: TGPLinearGradientBrush;
  rr, lr, o: TGPRectF;
  rgn: TGPRegion;
begin
  case Direction of
    dVertical:
    begin
      o := MakeRect(InsideRect.Left, InsideRect.Bottom - GetPosition - (ThumbSize / 2), InsideRect.Right, GetPosition + (ThumbSize / 2));
      FProgressFill.Fill(g, o);
      if IsGlowAnimation then
      begin
        rgn := TGPRegion.Create(o);
        g.SetClip(rgn);
        lr := MakeRect(o.X, InsideRect.Bottom - FGlowPos - 30, o.Width, 30);
        rr := MakeRect(o.X, InsideRect.Bottom - FGlowPos, o.Width, 30);
        b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeVertical);
        g.FillRectangle(b, lr);
        b.free;
        b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, clWhite), MakeColor(0, clWhite), LinearGradientModeVertical);
        g.FillRectangle(b, rr);
        b.free;
        g.ResetClip;
        rgn.free;
      end;
    end;
    dHorizontal:
    begin
      o := MakeRect(InsideRect.Left, InsideRect.Top, GetPosition + (ThumbSize / 2), InsideRect.Bottom);
      FProgressFill.Fill(g, o);
      if IsGlowAnimation then
      begin
        rgn := TGPRegion.Create(o);
        g.SetClip(rgn);
        lr := MakeRect(FGlowPos - 30, o.Y, 30, o.Height);
        rr := MakeRect(FGlowPos, o.Y, 30, o.Height);
        b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, clWhite), MakeColor(120, clWhite), LinearGradientModeHorizontal);
        g.FillRectangle(b, lr);
        b.free;
        b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, clWhite), MakeColor(0, clWhite), LinearGradientModeHorizontal);
        g.FillRectangle(b, rr);
        b.free;
        g.ResetClip;
        rgn.free;
      end;
    end;
  end;
end;

procedure TAdvSmoothTrackBar.DrawSlider(g: TGPGraphics);
var
  p, p2: TGPPen;
  slr: TGPRectF;
begin
  if not ShowThumb then
    Exit;

  p := TGPPen.Create(MakeColor(120, ThumbFill.BorderColor));
  p2:= TGPPen.Create(MakeColor(150, clWhite));
  case Direction of
    dVertical:
    begin
      slr := GetSliderRect;
      FThumbFill.Fill(g, slr);
      g.DrawLine(p, slr.X, slr.Y + (slr.Height / 2), slr.X + slr.Width, slr.Y + (slr.Height / 2));
      g.DrawLine(p2, slr.X, slr.Y + (slr.Height / 2)+1, slr.X + slr.Width, slr.Y + (slr.Height / 2)+1);
    end;
    dHorizontal:
    begin
      slr := GetSliderRect;
      FThumbFill.Fill(g, slr);
      g.DrawLine(p, slr.X + (slr.Width / 2), slr.Y , slr.X + (slr.Width / 2), slr.Y  + slr.Height);
      g.DrawLine(p2, slr.X + (slr.Width / 2)+1, slr.Y, slr.X  + (slr.Width / 2)+1, slr.Y + slr.Height);
    end;
  end;

  if TabStop and FFocused then
    DrawFocus(g, slr, FThumbFill.Rounding, FThumbFill.RoundingType);

  p.Free;
  p2.Free;
end;

procedure TAdvSmoothTrackBar.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothTrackBar.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothTrackBar.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothTrackBar.GetNumRect: TGPRectF;
begin
  case Direction of
    dVertical: Result := MakeRect(InsideRect.Left, InsideRect.Top + (ThumbSize / 2), InsideRect.Right, InsideRect.Bottom - ThumbSize);
    dHorizontal: Result := MakeRect(InsideRect.Left + (ThumbSize / 2), InsideRect.Top, InsideRect.Right - ThumbSize, InsideRect.Bottom);
  end;
end;

function TAdvSmoothTrackBar.GetPosition(XYPos: Double): Double;
var
  v, totals, qr: Double;
begin
  totals := 0;
  case Direction of
    dVertical: totals := GetNumRect.Height;
    dHorizontal: totals := GetNumRect.Width;
  end;

  if (Maximum - Minimum) > 0 then
  begin
    v := (XYPos / Totals) * (Maximum - Minimum) + Minimum;
    if FSnapToTickMarks then
    begin
      qr := Round(v / Step) * Step;
      if (v >= qr - FSnapMargin) and (v <= qr + FSnapMargin) then
        v := qr
      else
    end;

    Result := Max(Min(v, Maximum), Minimum);
  end
  else
    Result := 0;
end;

function TAdvSmoothTrackBar.GetPosition: Double;
var
  totals: Double;
begin
  totals := 0;
  case Direction of
    dVertical: totals := GetNumRect.Height;
    dHorizontal: totals := GetNumRect.Width;
  end;

  if (Maximum - Minimum) > 0 then
    Result := Min(((Position - Minimum) / (Maximum - Minimum)) * totals, totals)
  else
    Result := 0;
end;

function TAdvSmoothTrackBar.GetSliderRect: TGPRectF;
begin
  case Direction of
    dVertical: Result := MakeRect(InsideRect.Left + 7, GetNumRect.Y + GetNumRect.Height - GetPosition - (ThumbSize / 2), InsideRect.Right - 14, ThumbSize);
    dHorizontal: Result := MakeRect(GetNumRect.X + GetPosition - (ThumbSize / 2), InsideRect.Top + 7, ThumbSize, InsideRect.Bottom - 14);
  end;
end;

function TAdvSmoothTrackBar.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothTrackBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothTrackBar.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothTrackBar.InsideRect: TRect;
var
  sh, bw: integer;
begin
  sh := 0;
  if (Fill.ShadowColor <> clNone) {and not Transparent} then
    sh := Fill.ShadowOffset;

  Result := Rect(0, 0, Width, Height);
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

function TAdvSmoothTrackBar.IsGlowAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and FDoGlow then
    Result := GlowAnimation;
end;

procedure TAdvSmoothTrackBar.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not TabStop or ReadOnly then
    Exit;

  if not FPrevPosSet then
    FPrevPos := Position;

  case Key of
    VK_LEFT, VK_DOWN: FPrevPos := Position - Step;
    VK_RIGHT, VK_UP: FPrevPos := Position + Step;
    VK_HOME: FPrevPos := Minimum;
    VK_END: FPrevPos := Maximum;
  end;

  if (FPosition <> FPrevPos) and (FPrevPos >= Minimum) and (FPrevPos <= Maximum) then
  begin
    if Assigned(OnPositionChanging) then
      OnPositionChanging(Self, FPrevPos);
    FPosition := FPrevPos;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.KeyUp(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not TabStop or ReadOnly then
    Exit;

  case Key of
    VK_END, VK_HOME, VK_RIGHT, VK_UP, VK_LEFT, VK_DOWN:
    begin
      Position := FPrevPos;
      FPrevPosSet := false;
      if Assigned(OnPositionChanged) then
        OnPositionChanged(Self, Position);
    end;
  end;
end;

procedure TAdvSmoothTrackBar.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothTrackBar.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r: TGPRectF;
begin
  inherited;

  if ReadOnly then
    Exit;

  SetFocus;
  r := GetSliderRect;
  FMouseDownOnSlider := PtInRect(Bounds(Round(r.X), Round(r.Y), Round(r.Width), Round(r.Height)), Point(X, Y));

  if (FMouseDownOnSlider) then
  begin
    if PositionToolTip then
    begin
      UpdateToolTip;
      SendMessage(fhtooltip, TTM_POPUP, 0,0);
    end;
  end;
end;

procedure TAdvSmoothTrackBar.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  r: TGPRectF;
  pos, v: Double;
begin
  inherited;

  if ReadOnly then
    Exit;

  r := GetSliderRect;

  if FMouseDownOnSlider then
  begin
    case Direction of
      dVertical:
      begin
        pos := Height - (ThumbSize / 2) - Y;
        v := GetPosition(pos);
        if (v >= Minimum) and (v <= Maximum) and (v <> FPosition) then
        begin
          FPosition := v;
          if FGlowPos = -60 then
            FDoGlow := true;
          if Assigned(OnPositionChanging) then
            OnPositionChanging(Self, Position);
        end;
      end;
      dHorizontal:
      begin
        pos := X - (ThumbSize / 2);
        v := GetPosition(pos);
        if (v >= Minimum) and (v <= Maximum) and (v <> FPosition) then
        begin
          FPosition := v;
          if FGlowPos = -60 then
            FDoGlow := true;
          if Assigned(OnPositionChanging) then
            OnPositionChanging(Self, Position);
        end;
      end;
    end;

    if PositionToolTip then
      UpdateToolTip;
    Repaint;
  end;
end;

procedure TAdvSmoothTrackBar.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  r: TGPRectF;
  v: Double;
begin
  inherited;

  if ReadOnly then
    Exit;

  v := Maximum + 1;
  if not FMouseDownOnSlider then
  begin
    r := GetSliderRect;
    if FDirection = dVertical then
    begin
      if Y < r.Y  then
        v := FPosition + Step
      else if Y > r.Y + r.Height then
        v := FPosition - Step;
    end
    else
    begin
      if X < r.X  then
        v := FPosition - Step
      else if X > r.X + r.Width then
        v := FPosition + Step;
    end;
    if (v >= Minimum) and (v <= Maximum) and (v <> FPosition) then
    begin
      FPosition := v;
      if FGlowPos = -60 then
        FDoGlow := true;
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self, FPosition);
    end;
  end
  else
  begin
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self, FPosition);

    if PositionToolTip then
      ClearToolTip;
  end;

  FMouseDownOnSlider := false;
  Cursor := crArrow;
  Changed;
end;

procedure TAdvSmoothTrackBar.Paint;
var
  g: TGPGraphics;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawBackGround(g);
  if ShowProgress then
    DrawProgress(g);

  DrawNumbers(g);
  DrawOverlay(g);
  DrawSlider(g);

  g.Free;
end;

procedure TAdvSmoothTrackBar.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothTrackBar.Resize;
begin
  if FResize then
  begin
    case FDirection of
      dHorizontal:
      begin
        FOldW := Width;
        FOldH := Height;
      end;
      dVertical:
      begin
        FOldW := Height;
        FOldH := Width;
      end;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothTrackBar.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothTrackBar.SetColorTones(ATones: TColorTones);
begin
  Fill.Color := ATones.Background.BrushColor;
  Fill.ColorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;
  Font.Color := ATones.Background.TextColor;
  Font.Name := GetMetroFont;
  ProgressFont.Color := ATones.Foreground.TextColor;

  ProgressFill.Color := ATones.Foreground.BrushColor;
  ProgressFill.ColorTo := ATones.Foreground.BrushColor;
  ProgressFill.ColorMirror := ATones.Foreground.BrushColor;
  ProgressFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  ProgressFill.BorderColor := ATones.Foreground.BorderColor;

  TickMarkColor := ATones.Selected.BrushColor;
  ThumbFill.Color := ATones.Selected.BrushColor;
  ThumbFill.GradientType := gtSolid;
  ThumbFill.Opacity := 100;
  ThumbFill.BorderColor := ATones.Selected.BorderColor;
  GlowAnimation := false;
end;

procedure TAdvSmoothTrackBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  ThumbFill.Rounding := 10;
  ThumbFill.BorderColor := clGray;
  ThumbFill.Color := clwhite;
  ThumbFill.ColorTo := clWhite;
  ThumbFill.GradientType := gtForwardDiagonal;
  ThumbFill.OpacityTo := 0;
  Font.Color:= clBlack;
  
  case AStyle of
    tsOffice2003Blue:
    begin
      Fill.Color := $00FFD2AF;
      Fill.ColorTo := $00FFD2AF;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $FCE1CB;
      ProgressFill.ColorTo := $E0A57D;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $962D00;
      ProgressFill.GradientMirrorType := gtVertical;
//      ProgressFill.Color := $94E6FB;
//      ProgressFill.ColorTo := $1595EE;
//      ProgressFill.ColorMirror := clNone;
//      ProgressFill.ColorMirrorTo := clNone;
//      ProgressFill.BorderColor := $962D00;
//      ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsOffice2003Silver:
    begin
      Fill.Color := $00E6D8D8;
      Fill.ColorTo := $00E6D8D8;
      Fill.BorderColor := $00C0C0C0;
      ProgressFill.Color := $ECE2E1;
      ProgressFill.ColorTo := $B39698;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $947C7C;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $94E6FB;
//        ProgressFill.ColorTo := $1595EE;
//        ProgressFill.ColorMirror := clNone;
//        ProgressFill.ColorMirrorTo := clNone;
//        ProgressFill.BorderColor := $947C7C;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsOffice2003Olive:
    begin
      Fill.Color := $CFF0EA;
      Fill.ColorTo := $CFF0EA;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $CFF0EA;
      ProgressFill.ColorTo := $8CC0B1;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $588060;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $94E6FB;
//        ProgressFill.ColorTo := $1595EE;
//        ProgressFill.ColorMirror := clNone;
//        ProgressFill.ColorMirrorTo := clNone;
//        ProgressFill.BorderColor := $588060;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsOffice2003Classic:
    begin
      Fill.Color := $00F2F2F2;
      Fill.ColorTo := $00F2F2F2;
      Fill.BorderColor := $00C0C0C0;
      ProgressFill.Color := clWhite;
      ProgressFill.ColorTo := $C9D1D5;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $808080;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $B59285;
//        ProgressFill.ColorTo := $B59285;
//        ProgressFill.ColorMirror := clNone;
//        ProgressFill.ColorMirrorTo := clNone;
//        ProgressFill.BorderColor := $962D00;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsOffice2007Luna:
    begin
      Fill.Color := $00FFD2AF;
      Fill.ColorTo := $00FFD2AF;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $FFEFE3;
      ProgressFill.ColorTo := $FFDDC4;
      ProgressFill.ColorMirror := $FFD1AD;
      ProgressFill.ColorMirrorTo := $FFDBC0;
      ProgressFill.BorderColor := $FFD1AD;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $AAD9FF;
//        ProgressFill.ColorTo := $6EBBFF;
//        ProgressFill.ColorMirror := $42AEFE;
//        ProgressFill.ColorMirrorTo := $7AE1FE;
//        ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsOffice2007Obsidian:
    begin
      Fill.Color := $5C534C;
      Fill.ColorTo := $5C534C;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $F9F8F8;
      ProgressFill.ColorTo := $E4E2DF;
      ProgressFill.ColorMirror := $D1CBC7;
      ProgressFill.ColorMirrorTo := $E2DEDB;
      ProgressFill.BorderColor := clBlack;//$D1CBC7;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $AAD9FF;
//        ProgressFill.ColorTo := $6EBBFF;
//        ProgressFill.ColorMirror := $42AEFE;
//        ProgressFill.ColorMirrorTo := $7AE1FE;
//        ProgressFill.BorderColor := clBlack;//$42AEFE;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsWindowsXP:
    begin
      Fill.Color := $00B6B6B6;
      Fill.ColorTo := $00B6B6B6;
      Fill.BorderColor := $00C0C0C0;
      ProgressFill.Color := clWhite;
      ProgressFill.ColorTo := clBtnFace;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := clBlack;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := clInActiveCaption;
//        ProgressFill.ColorTo := clInActiveCaption;
//        ProgressFill.ColorMirror := clNone;
//        ProgressFill.ColorMirrorTo := clNone;
//        ProgressFill.BorderColor := clBlack;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsWhidbey:
    begin
      Fill.Color := $F5F9FA;
      Fill.ColorTo := $F5F9FA;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $F5F9FA;
      ProgressFill.ColorTo := $A8C0C0;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $962D00;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $94E6FB;
//        ProgressFill.ColorTo := $1595EE;
//        ProgressFill.ColorMirror := clNone;
//        ProgressFill.ColorMirrorTo := clNone;
//        ProgressFill.BorderColor := $962D00;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsCustom: ;
    tsOffice2007Silver:
    begin
      Fill.Color := $00CAC1BA;
      Fill.ColorTo := $00CAC1BA;
      Fill.BorderColor := $00C0C0C0;

      ProgressFill.Color := $FAEEEB;
      ProgressFill.ColorTo := $E5DBD7;
      ProgressFill.ColorMirror := $E2D8D4;
      ProgressFill.ColorMirrorTo := $D1C7C5;
      ProgressFill.BorderColor := clBlack;//$E2D8D4;
      ProgressFill.GradientMirrorType := gtVertical;
//        ProgressFill.Color := $AAD9FF;
//        ProgressFill.ColorTo := $6EBBFF;
//        ProgressFill.ColorMirror := $42AEFE;
//        ProgressFill.ColorMirrorTo := $7AE1FE;
//        ProgressFill.BorderColor := clBlack;//$42AEFE;
//        ProgressFill.GradientMirrorType := gtVertical;
    end;
    tsWindowsVista:
    begin
      Fill.Color := $FFFDF9;
      Fill.ColorTo := $FFFAF0;
      Fill.BorderColor := $FCF2DA;

      ProgressFill.Color := $FEF9F0;
      ProgressFill.ColorTo := $FDF0D7;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $FEDF9A;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
    tsWindows7:
    begin
      Fill.Color := $FCEBDC;
      Fill.ColorTo := $FCDBC1;
      Fill.BorderColor := $CEA27D;

      ProgressFill.Color := $FDFBFA;
      ProgressFill.ColorTo := $FDF3EB;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $FBD6B8;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
    tsTerminal:
    begin
      Fill.Color := clBtnFace;
      Fill.ColorTo := clBtnFace;
      Fill.BorderColor := clGray;
      ProgressFill.Color := clSilver;
      ProgressFill.ColorTo := clSilver;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := clGray;

    end;
    tsOffice2010Blue:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := RGB(237, 239, 241);
      Fill.BorderColor := RGB(236, 237, 237);

      ProgressFill.Color := $FDF6EF;
      ProgressFill.ColorTo := $F0DAC7;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $C7B29F;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
    tsOffice2010Silver:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := RGB(237, 239, 241);
      Fill.BorderColor := RGB(236, 237, 237);

      ProgressFill.Color := $D4CFCB; //$7C6D66;  //$FFFFFF;
      ProgressFill.ColorTo := $EDE5E0;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $D2CDC8;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
    tsOffice2010Black:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := RGB(237, 239, 241);
      Fill.BorderColor := RGB(236, 237, 237);

      ProgressFill.Color := $BFBFBF;
      ProgressFill.ColorTo := $919191;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $6D6D6D;
      ProgressFill.GradientMirrorType := gtVertical;

    end;

  tsWindows8, tsWindows10:
    begin
      Fill.Color := $F7F6F5;
      Fill.ColorTo := $F7F6F5;;
      Fill.BorderColor := $E4E3E2;

      ProgressFill.Color := $F7E0C9;
      ProgressFill.ColorTo := $F7E0C9;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E4A262;
      ProgressFill.GradientMirrorType := gtVertical;
    end;
  tsOffice2013White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clWhite;
      Fill.BorderColor := $D4D4D4;

      ProgressFill.Color := $FCE2C8;
      ProgressFill.ColorTo := $FCE2C8;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E59D56;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  tsOffice2013LightGray:
    begin
      Fill.Color := $F6F6F6;
      Fill.ColorTo := $F6F6F6;
      Fill.BorderColor := $C6C6C6;

      ProgressFill.Color := $FCE2C8;
      ProgressFill.ColorTo := $FCE2C8;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E59D56;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  tsOffice2013Gray:
    begin
      Fill.Color := $E5E5E5;;
      Fill.ColorTo := $E5E5E5;
      Fill.BorderColor := $ABABAB;

      ProgressFill.Color := $FCE2C8;
      ProgressFill.ColorTo := $FCE2C8;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E59D56;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  tsOffice2016White:
    begin
      Fill.Color := clWhite;
      Fill.ColorTo := clWhite;
      Fill.BorderColor := $D4D4D4;

      ProgressFill.Color := $E3BDA3;
      ProgressFill.ColorTo := $E3BDA3;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E3BDA3;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  tsOffice2016Gray:
    begin
      Fill.Color := $B2B2B2;
      Fill.ColorTo := $B2B2B2;
      Fill.BorderColor := $444444;

      ProgressFill.Color := $E3BDA3;
      ProgressFill.ColorTo := $E3BDA3;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $E3BDA3;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  tsOffice2016Black:
    begin
      Fill.Color := $363636;
      Fill.ColorTo := $363636;
      Fill.BorderColor := $444444;

      Font.Color:= $FFFFFF;

      ProgressFill.Color := $6A6A6A;
      ProgressFill.ColorTo := $6A6A6A;
      ProgressFill.ColorMirror := clNone;
      ProgressFill.ColorMirrorTo := clNone;
      ProgressFill.BorderColor := $6A6A6A;
      ProgressFill.GradientMirrorType := gtVertical;

    end;
  end;
end;

procedure TAdvSmoothTrackBar.SetDirection(
  const Value: TAdvSmoothTrackBarDirection);
begin
  FDirection := Value;
  if not FLoaded or (csLoading in ComponentState) then
    Exit;

  FResize := false;
  if FDirection = dHorizontal then
  begin
    Width := FOldW;
    Height := FOldH;
  end
  else
  begin
    Width := FOldH;
    Height := FOldW;
  end;
  FResize := true;
  Changed;
end;

procedure TAdvSmoothTrackBar.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTrackBar.SetFont(const Value: TFont);
begin
  if FFont <> value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetFormat(const Value: String);
begin
  if FFormat <> value then
  begin
    FFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetGlowAnimation(const Value: Boolean);
begin
  if FGlowAnimation <> value then
  begin
    FGlowAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetMaximum(const Value: Double);
begin
  if FMaximum <> value then
  begin
    FMaximum := Value;
    if FMaximum < FPosition then
      FPosition := FMaximum;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetMinimum(const Value: Double);
begin
  if FMinimum <> Value then
  begin
    FMinimum := Value;
    if FMinimum > FPosition then
      FPosition := FMinimum;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetPosition(const Value: Double);
begin
  if (FPosition <> Value) and (Value >= Minimum) and (Value <= Maximum) then
  begin
    FPosition := Value;
    if FGlowPos = -60 then
      FDoGlow := true;
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self, FPosition);
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetProgressFill(const Value: TGDIPFill);
begin
  if FProgressFill <> value then
  begin
    FProgressFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTrackBar.SetProgressFont(const Value: TFont);
begin
  if FProgressFont <> Value then
  begin
    FProgressFont.Assign(Value);
    Fontchanged(Self);
  end;
end;

procedure TAdvSmoothTrackBar.SetReadOnly(const Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetShowInnerBorder(const Value: Boolean);
begin
  if FShowInnerBorder <> value then
  begin
    FShowInnerBorder := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetShowProgress(const Value: Boolean);
begin
  if FShowProgress <> value then
  begin
    FShowProgress := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetShowThumb(const Value: Boolean);
begin
  if FShowThumb <> Value then
  begin
    FShowThumb := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetShowTickMarks(const Value: Boolean);
begin
  if FShowTickMarks <> value then
  begin
    FShowTickMarks := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetShowValues(const Value: Boolean);
begin
  if FShowValues <> Value then
  begin
    FShowValues := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetSnapMargin(const Value: Double);
begin
  if FSnapMargin <> Value then
  begin
    FSnapMargin := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetSnapToTickMarks(const Value: Boolean);
begin
  if FSnapToTickMarks <> Value then
  begin
    FSnapToTickMarks := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetStep(const Value: Double);
begin
  if (FStep <> Value) and (Value > 0) then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetThumbFill(const Value: TGDIPFill);
begin
  if FThumbFill <> value then
  begin
    FThumbFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothTrackBar.setThumbSize(const Value: integer);
begin
  if FThumbSize <> Value then
  begin
    FThumbSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetTickMarkColor(const Value: TColor);
begin
  if FTickMarkColor <> value then
  begin
    FTickMarkColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetTickMarkSize(const Value: integer);
begin
  if FTickMarkSize <> Value then
  begin
    FTickMarkSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothTrackBar.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothTrackBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TAdvSmoothTrackBar.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothTrackBar.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
  {$IFNDEF DELPHI_UNICODE}
  dbl: boolean;
  {$ENDIF}
  p: TPoint;
  i: integer;
  chk: Boolean;
begin
  chk := (Fill.Opacity = 255) and (Fill.OpacityTo = 255)
    and (Fill.OpacityMirror = 255) and (Fill.OpacityMirrorTo = 255);

  if not chk then
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
  end
  else
    inherited;
end;

procedure TAdvSmoothTrackBar.WndProc(var Message: TMessage);
begin
  if (Message.Msg = WM_DESTROY) then
  begin
    DestroyToolTip;
  end;
  inherited;

end;

procedure TAdvSmoothTrackBar.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.

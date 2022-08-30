{**************************************************************************}
{ TAdvSmoothSlider component                                               }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2010 - 2015                                                }
{   Email : info@tmssoftware.com                                           }
{   Web : http://www.tmssoftware.com                                       }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit AdvSmoothSlider;

interface

{$I TMSDEFS.INC}

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls,
  Math, AdvStyleIF, GDIPFill, Forms, ExtCtrls,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 2; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with transparency with rounded corners
  // v1.0.1.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.2.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.2.1 : Fixed : Issue with rightclick and popupmenu
  // v1.0.2.2 : Fixed : Issue with focusing
  // v1.0.3.0 : New : Built-in support for Office 2010 colors
  // v1.0.3.1 : Fixed : Issue with on off label
  // v1.0.3.2 : Fixed : Issue with right-clicking slider
  // v1.0.3.3 : Fixed : Issue with double-clicking on slider.
  // v1.1.0.0 : New : Disabled Fill on all slider elements
  // v1.2.0.0 : New : Metro Style Support
  // v1.2.0.1 : Fixed : Issue with mouse leave functionality in older delphi versions
  // v1.2.0.2 : Fixed : Issue with form scaling & button display
  // v1.3.0.0 : New : Windows 8, Office 2013 styles added
  // v1.3.1.0 : New : InverseSlider property
  // v1.3.1.1 : Fixed : Issue with update on toggling Enabled
  // v1.3.1.2 : Fixed : Issue with setting Enabled at runtime
  // v1.4.0.0 : New : Windows 10, Office 2016 styles added
  // v1.4.0.1 : Improved : Disabled fill style
  // v1.4.0.2 : Fixed : Issue with ALT + F4 to close application triggering a state switch

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothSlider = class;

  TAdvSmoothSliderButtonAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothSlider;
    FOnChange: TNotifyEvent;
    FSize: integer;
    FFill: TGDIPFill;
    FFillDisabled: TGDIPFill;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSize(const Value: integer);
    procedure SetFillDisabled(const Value: TGDIPFill);
  public
    constructor Create(AOwner: TAdvSmoothSlider);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property FillDisabled: TGDIPFill read FFillDisabled write SetFillDisabled;
    property Size: integer read FSize write SetSize default 40;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothSliderCaptionPosition = (cpTopLeft, cpTopCenter, cpTopRight, cpCenterLeft, cpCenterCenter, cpCenterRight, cpBottomLeft, cpBottomCenter, cpBottomRight, cpCustom);

  TAdvSmoothSliderAppearance = class(TPersistent)
  private
    FOwner: TAdvSmoothSlider;
    FFont: TFont;
    FCaption: String;
    FOnChange: TNotifyEvent;
    FFill: TGDIPFill;
    FCaptionPosition: TAdvSmoothSliderCaptionPosition;
    FCaptionTop: integer;
    FCaptionLeft: integer;
    FFillDisabled: TGDIPFill;
    procedure SetCaption(const Value: String);
    procedure SetFill(const Value: TGDIPFill);
    procedure SetFont(const Value: TFont);
    procedure SetCaptionPosition(const Value: TAdvSmoothSliderCaptionPosition);
    procedure SetCaptionLeft(const Value: integer);
    procedure SetCaptionTop(const Value: integer);
    procedure SetFillDisabled(const Value: TGDIPFill);
  public
    constructor Create(AOwner: TAdvSmoothSlider);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  published
    property Fill: TGDIPFill read FFill write SetFill;
    property FillDisabled: TGDIPFill read FFillDisabled write SetFillDisabled;
    property Caption: String read FCaption write SetCaption;
    property Font: TFont read FFont write SetFont;
    property CaptionPosition: TAdvSmoothSliderCaptionPosition read FCaptionPosition write SetCaptionPosition default cpCenterCenter;
    property CaptionLeft: integer read FCaptionLeft write SetCaptionLeft default 0;
    property CaptionTop: integer read FCaptionTop write SetCaptionTop default 0;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothSliderState = (ssOn, ssOff);

  TAdvSmoothStateChanged = procedure(Sender: TObject; State: TAdvSmoothSliderState; Value: Double) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothSlider = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSSTyle;
    FFocused: Boolean;
    FDoAnimation, FDesignTime: Boolean;
    FSlide: TTimer;
    FCx: integer;
    FMouseDown, FMouseDownButton: Boolean;
    FPosTo, FCurrentPos: Double;
    FAppearanceOff: TAdvSmoothSliderAppearance;
    FState: TAdvSmoothSliderState;
    FValueOff: Double;
    FButtonAppearance: TAdvSmoothSliderButtonAppearance;
    FAppearanceOn: TAdvSmoothSliderAppearance;
    FValueOn: Double;
    FFill: TGDIPFill;
    FOnStateChanged: TAdvSmoothStateChanged;
    FAnimationFactor: integer;
    FShowFocus: Boolean;
    FFocusColor: TColor;
    FFillDisabled: TGDIPFill;
    FInverseSlider: Boolean;
    procedure SetAppearanceOff(const Value: TAdvSmoothSliderAppearance);
    procedure SetAppearanceOn(const Value: TAdvSmoothSliderAppearance);
    procedure SetButtonAppearance(
      const Value: TAdvSmoothSliderButtonAppearance);
    procedure SetValueOff(const Value: Double);
    procedure SetValueOn(const Value: Double);
    procedure SetFill(const Value: TGDIPFill);
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMEnable(var Message: TWMEnable); message WM_ENABLE;
    procedure SetAnimationFactor(const Value: integer);
    procedure SetFocusColor(const Value: TColor);
    procedure SetShowFocus(const Value: Boolean);
    procedure SetFillDisabled(const Value: TGDIPFill);
    procedure SetInverseSlider(const Value: Boolean);
    procedure SetState(const Value: TAdvSmoothSliderState);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure DoStateChanged(State: TAdvSmoothSliderState; Value: Double); virtual;
    procedure Changed;
    procedure AppearanceChanged(Sender: TObject);
    procedure FillChanged(Sender: TObject);
    function InsideRect: TRect;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawButton(g: TGPGraphics);
    procedure DrawCaption(g: TGPGraphics; app: TAdvSmoothSliderAppearance; r: TGPRectF);
    procedure DrawOn(g: TGPGraphics);
    procedure DrawOff(g: TGPGraphics);
    procedure DoSlide(Sender: TObject);
    function GetButtonRect: TGPRectF;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property InverseSlider: Boolean read FInverseSlider write SetInverseSlider default False;
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 15;
    property AppearanceOn: TAdvSmoothSliderAppearance read FAppearanceOn write SetAppearanceOn;
    property AppearanceOff: TAdvSmoothSliderAppearance read FAppearanceOff write SetAppearanceOff;
    property ButtonAppearance: TAdvSmoothSliderButtonAppearance read FButtonAppearance write SetButtonAppearance;
    property ValueOn: Double read FValueOn write SetValueOn;
    property ValueOff: Double read FValueOff write SetValueOff;
    property State: TAdvSmoothSliderState read FState write SetState default ssOn;
    property ShowFocus: Boolean read FShowFocus write SetShowFocus default true;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clBlack;
    property Fill: TGDIPFill read FFill write SetFill;
    property FillDisabled: TGDIPFill read FFillDisabled write SetFillDisabled;

    property OnStateChanged: TAdvSmoothStateChanged read FOnStateChanged write FOnStateChanged;

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
  end;

implementation

function Darker(Color:TColor; Percent:Byte):TColor;
var
  r, g, b:Byte;
begin
  Color := ColorToRGB(Color);
  r := GetRValue(Color);
  g := GetGValue(Color);
  b := GetBValue(Color);
  r := r - muldiv(r, Percent, 100);  //Percent% closer to black
  g := g - muldiv(g, Percent, 100);
  b := b - muldiv(b, Percent, 100);
  result := RGB(r, g, b);
end;

procedure GetPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: integer; location: TAdvSmoothSliderCaptionPosition);
var
  w, h, tw, th: integer;
begin
  tw := objectwidth;
  th := objectheight;
  w := Round(rectangle.Width);
  h := Round(rectangle.Height);
  case location of
    cpTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    cpTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    cpBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    cpBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    cpTopCenter:
    begin
      x := (w - tw) div 2;
      y := 0;
    end;
    cpBottomCenter:
    begin
      x := (w - tw) div 2;
      y := h - th;
    end;
    cpCenterCenter:
    begin
      x := (w - tw) div 2;
      y := (h - th) div 2;
    end;
    cpCenterLeft:
    begin
      x := 0;
      y := (h - th) div 2;
    end;
    cpCenterRight:
    begin
      x := w - tw;
      y := (h - th) div 2;
    end;
  end;
end;


function AnimateDouble(var Start: Single; Stop, Delta, Margin: Single): Boolean;
begin
  Result := true;
  if (Start > Stop - Margin) and (Start < Stop + Margin) then
  begin
    Start := Stop;
    Result := false;
  end
  else
  begin
    Delta := Max(1, Delta);
    if Start < Stop then
      Start := Round(Start + Delta)
    else
      Start := Round(Start - Delta);
  end;
end;

function PtInGPRect(r: TGPRectF; pt: TPoint): Boolean;
begin
  result := ((pt.X >= r.X) and (pt.X <= r.X + r.Width)) and
     ((pt.Y >= r.Y) and (pt.Y <= r.Y + r.Height));
end;

{ TAdvSmoothSlider }

procedure TAdvSmoothSlider.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSlider.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSlider) then
  begin
    FAppearanceOff.Assign((Source as TAdvSmoothSlider).AppearanceOff);
    FAppearanceOn.Assign((source as TAdvSmoothSlider).AppearanceOn);
    FState := (Source as TAdvSmoothSlider).State;
    FValueOff := (Source as TAdvSmoothSlider).ValueOff;
    FValueOn := (Source as TAdvSmoothSlider).ValueOn;
    FAnimationFactor := (Source as TAdvSmoothSlider).AnimationFactor;
    FFill.Assign((Source as TAdvSmoothSlider).Fill);
    FFillDisabled.Assign((Source as TAdvSmoothSlider).FillDisabled);
    FButtonAppearance.Assign((Source as TAdvSmoothSlider).ButtonAppearance);
    FFocusColor := (Source as TAdvSmoothSlider).FocusColor;
    FShowFocus := (Source as TAdvSmoothSlider).ShowFocus;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.Changed;
begin
  Invalidate;
end;

procedure TAdvSmoothSlider.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TAdvSmoothSlider.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if not FMouseDown then
    FMouseDownButton := false;
end;

constructor TAdvSmoothSlider.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FInverseSlider := False;
  FSlide := TTimer.Create(Self);
  FSlide.Interval := 10;
  FSlide.Enabled := true;
  FSlide.OnTimer := DoSlide;
  FAppearanceOff := TAdvSmoothSliderAppearance.Create(Self);
  FAppearanceOff.OnChange := AppearanceChanged;
  FAppearanceOn := TAdvSmoothSliderAppearance.Create(Self);
  FAppearanceOn.OnChange := AppearanceChanged;
  FState := ssOn;
  FButtonAppearance := TAdvSmoothSliderButtonAppearance.Create(Self);
  FButtonAppearance.OnChange := AppearanceChanged;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFillDisabled := TGDIPFill.Create;
  FFillDisabled.OnChange := FillChanged;
  Width := 80;
  Height := 30;
  FDoAnimation := false;
  FAnimationFactor := 15;
  FShowFocus := true;
  FFocusColor := clBlack;
  TabStop := true;

  FAppearanceOn.Caption := 'On';
  FAppearanceOff.Caption := 'Off';


  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);
end;

destructor TAdvSmoothSlider.Destroy;
begin
  FAppearanceOff.Free;
  FAppearanceOn.Free;
  FButtonAppearance.Free;
  FFill.Free;
  FFillDisabled.Free;
  inherited;
end;

procedure TAdvSmoothSlider.DoEnter;
begin
  inherited;
  Ffocused := true;
  Changed;
end;

procedure TAdvSmoothSlider.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothSlider.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothSlider.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothSlider.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothSlider.DoSlide(Sender: TObject);
var
  d, pos: Single;
  doanim: Boolean;
begin
  if FDoAnimation then
  begin
    d := Abs(FCurrentPos - FPosTo) / AnimationFactor;
    pos := FCurrentPos;
    doanim := AnimateDouble(pos, FposTo, d, 0.01);
    if doanim then
    begin
      FCurrentPos := pos;
      Changed;
    end
    else
      FDoAnimation := false;
  end;
end;

procedure TAdvSmoothSlider.DoStateChanged(State: TAdvSmoothSliderState; Value: Double);
begin
  if Assigned(OnStateChanged) then
     OnStateChanged(Self, State, Value);
end;

procedure TAdvSmoothSlider.DrawBackGround(g: TGPGraphics);
begin
  if Enabled then
    FFill.Fill(g, MakeRect(0, 0, Width - 1, Height - 1))
  else
    FFillDisabled.Fill(g, MakeRect(0, 0, Width - 1, Height - 1));
end;

procedure TAdvSmoothSlider.DrawButton(g: TGPGraphics);
var
  f: TGDIPFill;
begin
  if Enabled then
    f := FButtonAppearance.Fill
  else
    f := FButtonAppearance.FillDisabled;

  with FButtonAppearance do
  begin
    f.BeginUpdate;
    f.Focus := ShowFocus and TabStop and Ffocused;
    f.FocusColor := FocusColor;
    f.Fill(g, GetButtonRect);
    f.EndUpdate;
  end;
end;

procedure TAdvSmoothSlider.DrawCaption(g: TGPGraphics;
  app: TAdvSmoothSliderAppearance; r: TGPRectF);
var
  ff: TGPFontFamily;
  fs: integer;
  sf: TGPStringFormat;
  f: TGPFont;
  sizerect: TGPRectF;
  tw, th: integer;
  x, y: Double;
  b: TGPSolidBrush;
begin
  with app do
  begin
    if Caption <> '' then
    begin
      ff := TGPFontFamily.Create(FFont.Name);
      if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
      begin
        ff.Free;
        ff := TGPFontFamily.Create('Arial');
      end;

      fs := 0;
      if (fsBold in FFont.Style) then
        fs := fs + 1;
      if (fsItalic in FFont.Style) then
        fs := fs + 2;
      if (fsUnderline in FFont.Style) then
        fs := fs + 4;

      sf := TGPStringFormat.Create;
      f := TGPFont.Create(ff, FFont.Size , fs, UnitPoint);
      g.MeasureString(Caption, Length(Caption), f, r, sf, sizerect);

      tw := Round(sizerect.Width);
      th := Round(sizerect.Height);

      if CaptionPosition <> cpCustom then
        GetPosition(x, y, r, tw, th, CaptionPosition)
      else
      begin
        x := CaptionLeft;
        y := CaptionLeft;
      end;

      b := TGPSolidBrush.Create(MakeColor(255, FFont.Color));
      g.DrawString(Caption, Length(Caption), f, MakePoint(r.X + x, r.Y + y), sf, b);
      b.Free;

      f.Free;
      sf.Free;
      ff.Free;
    end;
  end;
end;

procedure TAdvSmoothSlider.DrawOff(g: TGPGraphics);
var
  r: TGPRectF;
  a: TAdvSmoothSliderAppearance;
begin
  if InverseSlider then
    a := AppearanceOn
  else
    a := AppearanceOff;

  with a do
  begin
    r := MakeRect(GetButtonRect.X + ButtonAppearance.Size / 2, InsideRect.Top, InsideRect.Right - GetButtonRect.X - ButtonAppearance.Size / 2, InsideRect.Bottom - InsideRect.Top);
    if Enabled then
      FFill.Fill(g, r)
    else
      FFillDisabled.Fill(g, r);
    DrawCaption(g, a, MakeRect(r.X, r.Y, r.Width + r.X, r.Height));
  end;
end;

procedure TAdvSmoothSlider.DrawOn(g: TGPGraphics);
var
  r: TGPRectF;
  a: TAdvSmoothSliderAppearance;
begin
  if InverseSlider then
    a := AppearanceOff
  else
    a := AppearanceOn;

  with a do
  begin
    r := MakeRect(InsideRect.Left, InsideRect.Top, GetButtonRect.X + ButtonAppearance.Size / 2, InsideRect.Bottom - InsideRect.Top);
    if Enabled then
      FFill.Fill(g, r)
    else
      FFillDisabled.Fill(g, r);
    DrawCaption(g, a, MakeRect(FCurrentPos, r.Y, r.Width - FCurrentPos - ButtonAppearance.Size / 2, r.Height));
  end;
end;

procedure TAdvSmoothSlider.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothSlider.GetButtonRect: TGPRectF;
begin
  with ButtonAppearance do
  begin
    if (FFill.BorderColor <> clNone) and (FFill.BorderWidth <> 0) then
      result := MakeRect(InsideRect.Right - Size, InsideRect.Top, Size, InsideRect.Bottom - InsideRect.Top)
    else
      result := MakeRect(InsideRect.Right - Size, InsideRect.Top, Size, InsideRect.Bottom - InsideRect.Top);

    result.X := Result.X + FCurrentPos;
    Result.X := Max(result.X, InsideRect.Left);
    result.X := Min(result.X, InsideRect.Right - ButtonAppearance.Size);
  end;
end;

function TAdvSmoothSlider.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothSlider.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothSlider.InsideRect: TRect;
var
  bw: integer;
begin
  Result := ClientRect;
  // adapt width & height for GDI+ drawing rect
  Result.Right := Result.Right - 1;
  Result.Bottom := Result.Bottom - 1;

  if (Fill.BorderColor <> clNone) then
  begin
    if Fill.BorderWidth = 1 then
      bw := 1
    else
      bw := (Fill.BorderWidth + 1) div 2;

    InflateRect(Result, -bw, -bw);
  end;

end;

procedure TAdvSmoothSlider.KeyDown(var Key: Word; Shift: TShiftState);
var
  s: TAdvSmoothSliderState;
  v: Double;
begin
  inherited;
  s := FState;
  case Key of
    VK_UP, VK_LEFT: FState := ssOff;
    VK_DOWN, VK_RIGHT: FState := ssOn;
    VK_F4:
    begin
      if not (ssAlt in Shift) then
      begin
        if FState = ssOn then
          FState := ssOff
        else
          FState := ssOn;
      end;
    end;
  end;

  if s <> FState then
  begin
    case FState of
      ssOn: FPosTo := 0;
      ssOff: FPosTo := -Width + ButtonAppearance.Size;
    end;
    FDoAnimation := true;

    v := 0;
    case FState of
      ssOn: v := ValueOn;
      ssOff: v := ValueOff;
    end;
    DoStateChanged(FState, v);
  end;
end;

procedure TAdvSmoothSlider.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothSlider.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if (Button <> mbLeft) then
    Exit;

  FFocused := true;
  SetFocus;
  FMouseDown := True;
  FMouseDownButton := PtInGPRect(GetButtonRect, Point(X, Y));
  FCx := X;
end;

procedure TAdvSmoothSlider.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if FMouseDown and not FDoAnimation then
  begin
    case FState of
      ssOn: FCurrentPos := Max(Min(X - FCx, 0), - Width + ButtonAppearance.Size);
      ssOff: FCurrentPos := Max(Min(X - FCx - Width + ButtonAppearance.Size, 0), - Width + ButtonAppearance.Size);
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
var
  s: TAdvSmoothSliderState;
  v: Double;
begin
  inherited;
  if (Button <> mbLeft) or (FMouseDown = False) then
    Exit;

  s := FState;

  if FMouseDownButton then
  begin
    if GetButtonRect.X + (GetButtonRect.Width / 2) > Width / 2 then
    begin
      FPosTo := 0;
      FState := ssOn;
    end
    else
    begin
      FPosTo := -Width + ButtonAppearance.Size;
      FState := ssOff;
    end;
  end
  else
  begin
    if X < GetButtonRect.X then
    begin
      FPosTo := -Width + ButtonAppearance.Size;
      FState := ssOff;
    end
    else if x > GetbuttonRect.X then
    begin
      FPosTo := 0;
      FState := ssOn;
    end;
  end;

  FDoAnimation := true;
  FMouseDown := false;
  FMouseDownButton := False;

  if s <> FState then
  begin
    v := 0;
    case FState of
      ssOn: v := ValueOn;
      ssOff: v := ValueOff;
    end;

    DoStateChanged(FState, v);
  end;
end;

procedure TAdvSmoothSlider.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);
  DrawBackGround(g);
  DrawOn(g);
  DrawOff(g);
  DrawButton(g);
  g.Free;
end;

procedure TAdvSmoothSlider.Resize;
begin
  inherited;
  case State of
  ssOn: FPosTo := 0;
  ssOff: FPosTo := -Width + ButtonAppearance.Size;
  end;
  FCurrentPos := FPosTo;
end;

procedure TAdvSmoothSlider.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothSlider.SetAnimationFactor(const Value: integer);
begin
  if (FAnimationFactor <> value) and (Value > 0) then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetAppearanceOff(
  const Value: TAdvSmoothSliderAppearance);
begin
  if FAppearanceOff <> value then
  begin
    FAppearanceOff.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothSlider.SetAppearanceOn(
  const Value: TAdvSmoothSliderAppearance);
begin
  if FAppearanceOn <> Value then
  begin
    FAppearanceOn.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothSlider.SetButtonAppearance(
  const Value: TAdvSmoothSliderButtonAppearance);
begin
  if FButtonAppearance <> Value then
  begin
    FButtonAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothSlider.SetColorTones(ATones: TColorTones);
begin
  ButtonAppearance.Fill.Color := ATones.Background.BrushColor;
  ButtonAppearance.Fill.ColorTo := ATones.Background.BrushColor;
  ButtonAppearance.Fill.BorderColor := ATones.Background.BorderColor;

  AppearanceOff.Fill.Color := ATones.Foreground.BrushColor;
  AppearanceOff.Fill.ColorTo := ATones.Foreground.BrushColor;
  AppearanceOff.Fill.ColorMirror := ATones.Foreground.BrushColor;
  AppearanceOff.Fill.ColorMirrorTo := ATones.Foreground.BrushColor;
  AppearanceOff.Fill.BorderColor := ATones.Foreground.BorderColor;
  AppearanceOff.Font.Color := ATones.Foreground.TextColor;


  AppearanceOn.Fill.Color := ATones.Selected.BrushColor;
  AppearanceOn.Fill.ColorTo := ATones.Selected.BrushColor;
  AppearanceOn.Fill.ColorMirror := ATones.Selected.BrushColor;
  AppearanceOn.Fill.ColorMirrorTo := ATones.Selected.BrushColor;
  AppearanceOn.Fill.BorderColor := ATones.Selected.BorderColor;
  AppearanceOn.Font.Color := ATones.Selected.TextColor;
end;

procedure TAdvSmoothSlider.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  // TODO : do color settings here
  AppearanceOn.Fill.Glow := gmNone;
  AppearanceOff.Fill.Glow := gmNone;
  AppearanceOn.Fill.GlowGradientColor:= clWhite;
  AppearanceOff.Fill.GlowGradientColor:= clWhite;

  AppearanceOn.FillDisabled.Glow := gmNone;
  AppearanceOff.FillDisabled.Glow := gmNone;
  AppearanceOn.FillDisabled.GlowGradientColor:= clWhite;
  AppearanceOff.FillDisabled.GlowGradientColor:= clWhite;

  FillDisabled.Color := $00F2F2F2;
  FillDisabled.ColorTo := $00B6B6B6;
  FillDisabled.ColorMirror := clNone;
  FillDisabled.ColorMirrorTo := clNone;
  FillDisabled.BorderColor := $962D00;
  FillDisabled.GradientMirrorType := gtVertical;

  Fill.Rounding := 4;
  Fill.RoundingType := rtBoth;
  ButtonAppearance.Fill.Rounding := 4;
  AppearanceOn.Fill.Rounding := 4;
  AppearanceOn.Fill.RoundingType := rtLeft;
  AppearanceOff.Fill.Rounding := 4;
  AppearanceOff.Fill.RoundingType := rtRight;

  FillDisabled.Rounding := 4;
  FillDisabled.RoundingType := rtBoth;

  AppearanceOn.Font.Color := clBlack;
  AppearanceOff.Font.Color := clBlack;

  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        ButtonAppearance.Fill.Color := $D68759;
        ButtonAppearance.Fill.ColorTo := $933803;
        ButtonAppearance.Fill.BorderColor := $962D00;

        AppearanceOff.Fill.Color := $EEDBC8;
        AppearanceOff.Fill.ColorTo := $F6DDC9;
        AppearanceOff.Fill.ColorMirror := $EDD4C0;
        AppearanceOff.Fill.ColorMirrorTo := $F7E1D0;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;

      end;
    tsOffice2003Silver:
      begin
        ButtonAppearance.Fill.Color := $BDA4A5;
        ButtonAppearance.Fill.ColorTo := $957475;
        ButtonAppearance.Fill.BorderColor := $947C7C;

        AppearanceOff.Fill.Color := $E6E9E2;
        AppearanceOff.Fill.ColorTo := $00E6D8D8;
        AppearanceOff.Fill.ColorMirror := $C8B2B3;
        AppearanceOff.Fill.ColorMirrorTo := $E6E9E2;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Olive:
      begin
        ButtonAppearance.Fill.Color := $82C0AF;
        ButtonAppearance.Fill.ColorTo := $447A63;
        ButtonAppearance.Fill.BorderColor := $588060;

        AppearanceOff.Fill.Color := $CFF0EA;
        AppearanceOff.Fill.ColorTo := $CFF0EA;
        AppearanceOff.Fill.ColorMirror := $8CC0B1;
        AppearanceOff.Fill.ColorMirrorTo := $CFF0EA;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2003Classic:
      begin
        ButtonAppearance.Fill.Color := $808080;
        ButtonAppearance.Fill.ColorTo := $808080;
        ButtonAppearance.Fill.BorderColor := $808080;

        AppearanceOff.Fill.Color := clWhite;
        AppearanceOff.Fill.ColorTo := $C9D1D5;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $B59285;
        AppearanceOn.Fill.ColorTo := $B59285;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $808080;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Luna:
      begin
        ButtonAppearance.Fill.Color := $FFEFE3;
        ButtonAppearance.Fill.ColorTo := $FFD2AF;
        ButtonAppearance.Fill.BorderColor := $00FFD2AF;

        AppearanceOff.Fill.Color := $FFEFE3;
        AppearanceOff.Fill.ColorTo := $FFDDC4;
        AppearanceOff.Fill.ColorMirror := $FFD1AD;
        AppearanceOff.Fill.ColorMirrorTo := $FFDBC0;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsOffice2007Obsidian:
      begin
        ButtonAppearance.Fill.Color := $F2F1F0;
        ButtonAppearance.Fill.ColorTo := $C9C2BD;
        ButtonAppearance.Fill.BorderColor := $5C534C;

        AppearanceOff.Fill.Color := $F9F8F8;
        AppearanceOff.Fill.ColorTo := $E4E2DF;
        AppearanceOff.Fill.ColorMirror := $D1CBC7;
        AppearanceOff.Fill.ColorMirrorTo := $E2DEDB;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsWindowsXP:
      begin
        ButtonAppearance.Fill.Color := clBtnFace;
        ButtonAppearance.Fill.ColorTo := clBtnFace;
        ButtonAppearance.Fill.BorderColor := clBlack;

        AppearanceOff.Fill.Color := clBtnFace;//clWhite;
        AppearanceOff.Fill.ColorTo := clBtnFace;//$B9D8DC;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := clInactiveCaption;
        AppearanceOn.Fill.ColorTo := clInactiveCaption;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := clHighLight;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsWhidbey:
      begin
        ButtonAppearance.Fill.Color := $EBEEEF;
        ButtonAppearance.Fill.ColorTo := $7E9898;
        ButtonAppearance.Fill.BorderColor := $962D00;

        AppearanceOff.Fill.Color := clWhite;
        AppearanceOff.Fill.ColorTo := $DFEDF0;
        AppearanceOff.Fill.ColorMirror := $DFEDF0;
        AppearanceOff.Fill.ColorMirrorTo := $DFEDF0;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        ButtonAppearance.Fill.Color := $F8F7F6;
        ButtonAppearance.Fill.ColorTo := $E8E0DB;
        ButtonAppearance.Fill.BorderColor := $74706F;

        AppearanceOff.Fill.Color := $F9F8F8;
        AppearanceOff.Fill.ColorTo := $E4E2DF;
        AppearanceOff.Fill.ColorMirror := $D1CBC7;
        AppearanceOff.Fill.ColorMirrorTo := $E2DEDB;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $AAD9FF;
        AppearanceOn.Fill.ColorTo := $6EBBFF;
        AppearanceOn.Fill.ColorMirror := $42AEFE;
        AppearanceOn.Fill.ColorMirrorTo := $7AE1FE;
        AppearanceOn.Fill.BorderColor := $42AEFE;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
      tsWindowsVista:
      begin
        ButtonAppearance.Fill.Color := $EFEFEF;
        ButtonAppearance.Fill.ColorTo := $EFEFEF;
        ButtonAppearance.Fill.BorderColor := $A4988C;

        AppearanceOff.Fill.Color := $FCF9F2;
        AppearanceOff.Fill.ColorTo := $FCF9F2;
        AppearanceOff.Fill.ColorMirror := $F7EED9;
        AppearanceOff.Fill.ColorMirrorTo := $F7EED9;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $FBEDD3;
        AppearanceOn.Fill.ColorTo := $FAE9C6;
        AppearanceOn.Fill.ColorMirror := $F7DAA2;
        AppearanceOn.Fill.ColorMirrorTo := $F5D089;
        AppearanceOn.Fill.BorderColor := $FEDF9A;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsWindows7:
      begin
        ButtonAppearance.Fill.Color := $EFEFEF;
        ButtonAppearance.Fill.ColorTo := $EFEFEF;
        ButtonAppearance.Fill.BorderColor := $A4988C;

        AppearanceOff.Fill.Color := $FDFBFA;
        AppearanceOff.Fill.ColorTo := $FDF3EB;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $FBD6B8;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $FCEBDC;
        AppearanceOn.Fill.ColorTo := $FCDBC1;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $CEA27D;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
    tsTerminal:
      begin
        ButtonAppearance.Fill.Color := clBtnFace;
        ButtonAppearance.Fill.ColorTo := clBtnFace;
        ButtonAppearance.Fill.BorderColor := clGray;

        AppearanceOff.Fill.Color := clSilver;
        AppearanceOff.Fill.ColorTo := clSilver;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := clNone;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := clHighLight;
        AppearanceOn.Fill.ColorTo := clHighLight;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := clGray;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
      tsOffice2010Blue:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        ButtonAppearance.Fill.Color := $EAD3BF;
        ButtonAppearance.Fill.ColorTo := clNone;
        ButtonAppearance.Fill.BorderColor := $5B391E;

        AppearanceOff.Fill.Color := $FDF6EF;
        AppearanceOff.Fill.ColorTo := $F0DAC7;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $C7B29F;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;
        AppearanceOff.Fill.Glow := gmGradient;

        AppearanceOn.Fill.Color := $75E5FF;
        AppearanceOn.Fill.ColorTo := $7DF0FF;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $308AC2;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
        AppearanceOn.Fill.Glow := gmGradient;
        AppearanceOn.Fill.GlowGradientColor:= $67BCF6;

      end;
      tsOffice2010Silver:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        ButtonAppearance.Fill.Color := $E0DDDA;
        ButtonAppearance.Fill.ColorTo := clNone;
        ButtonAppearance.Fill.BorderColor := $5B391E;

        AppearanceOff.Fill.Color := $FFFFFF;
        AppearanceOff.Fill.ColorTo := $EDE5E0;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $D2CDC8;;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;
        AppearanceOff.Fill.Glow := gmGradient;

        AppearanceOn.Fill.Color := $75E5FF;
        AppearanceOn.Fill.ColorTo := $7DF0FF;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $308AC2;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
        AppearanceOn.Fill.Glow := gmGradient;
        AppearanceOn.Fill.GlowGradientColor:= $67BCF6;

      end;
      tsOffice2010Black:
      begin
        Fill.Color := $00FFD2AF;
        Fill.ColorTo := $00FFD2AF;
        Fill.BorderColor := clNone;

        ButtonAppearance.Fill.Color := $D7D7D6;
        ButtonAppearance.Fill.ColorTo := clNone;
        ButtonAppearance.Fill.BorderColor := $656565;

        AppearanceOff.Fill.Color := $BFBFBF;
        AppearanceOff.Fill.ColorTo := $919191;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $6D6D6D;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;
        AppearanceOff.Fill.Glow := gmGradient;
        AppearanceOff.Fill.GlowGradientColor:= $BFBFBF;

        AppearanceOn.Fill.Color := $75E5FF;
        AppearanceOn.Fill.ColorTo := $7DF0FF;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $308AC2;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
        AppearanceOn.Fill.Glow := gmGradient;
        AppearanceOn.Fill.GlowGradientColor:= $67BCF6;

      end;

       tsWindows8, tsWindows10:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := $F7F6F5;
        ButtonAppearance.Fill.ColorTo := $F7F6F5;
        ButtonAppearance.Fill.BorderColor := $E4E3E2;
       // ButtonAppearance.Fill.Rounding

        AppearanceOff.Fill.Color := $F7EFE8;
        AppearanceOff.Fill.ColorTo := $F7EFE8;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $F9CEA4;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $F7E0C9;
        AppearanceOn.Fill.ColorTo := $F7E0C9;;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E4A262;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2013White:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := clWhite;
        ButtonAppearance.Fill.ColorTo := clWhite;
        ButtonAppearance.Fill.BorderColor := $D4D4D4;

        AppearanceOff.Fill.Color := $FCF0E4;
        AppearanceOff.Fill.ColorTo := $FCF0E4;;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $EAB47E;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $FCE2C8;
        AppearanceOn.Fill.ColorTo := $FCE2C8;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E59D56;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2013LightGray:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := $F6F6F6;
        ButtonAppearance.Fill.ColorTo := $F6F6F6;
        ButtonAppearance.Fill.BorderColor := $C6C6C6;

        AppearanceOff.Fill.Color := $FCF0E4;;
        AppearanceOff.Fill.ColorTo := $FCF0E4;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $EAB47E;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $FCE2C8;
        AppearanceOn.Fill.ColorTo := $FCE2C8;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E59D56;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2013Gray:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := $E5E5E5;
        ButtonAppearance.Fill.ColorTo := $E5E5E5;
        ButtonAppearance.Fill.BorderColor := $ABABAB;

        AppearanceOff.Fill.Color := $FCF0E4;
        AppearanceOff.Fill.ColorTo := $FCF0E4;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $EAB47E;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $FCE2C8;
        AppearanceOn.Fill.ColorTo := $FCE2C8;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E59D56;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2016White:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := clWhite;
        ButtonAppearance.Fill.ColorTo := clWhite;
        ButtonAppearance.Fill.BorderColor := $D4D4D4;

        AppearanceOff.Fill.Color := $F2E1D5;
        AppearanceOff.Fill.ColorTo := $F2E1D5;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $F2E1D5;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $E3BDA3;
        AppearanceOn.Fill.ColorTo := $E3BDA3;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E3BDA3;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2016Gray:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := $B2B2B2;
        ButtonAppearance.Fill.ColorTo := $B2B2B2;
        ButtonAppearance.Fill.BorderColor := $444444;

        AppearanceOff.Fill.Color := $F2E1D5;
        AppearanceOff.Fill.ColorTo := $F2E1D5;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $F2E1D5;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $E3BDA3;
        AppearanceOn.Fill.ColorTo := $E3BDA3;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $E3BDA3;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;
      end;
       tsOffice2016Black:
      begin
        Fill.Rounding := 0;
        ButtonAppearance.Fill.Rounding := 0;
        AppearanceOff.Fill.Rounding := 0;
        AppearanceOn.Fill.Rounding := 0;
        FillDisabled.Rounding := 0;

        ButtonAppearance.Fill.Color := $363636;
        ButtonAppearance.Fill.ColorTo := $363636;
        ButtonAppearance.Fill.BorderColor := $444444;

        AppearanceOff.Fill.Color := $6A6A6A;
        AppearanceOff.Fill.ColorTo := $6A6A6A;
        AppearanceOff.Fill.ColorMirror := clNone;
        AppearanceOff.Fill.ColorMirrorTo := clNone;
        AppearanceOff.Fill.BorderColor := $6A6A6A;
        AppearanceOff.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Fill.Color := $444444;
        AppearanceOn.Fill.ColorTo := $444444;
        AppearanceOn.Fill.ColorMirror := clNone;
        AppearanceOn.Fill.ColorMirrorTo := clNone;
        AppearanceOn.Fill.BorderColor := $444444;
        AppearanceOn.Fill.GradientMirrorType := gtVertical;

        AppearanceOn.Font.Color := $A6A6A6;
        AppearanceOff.Font.Color := $A6A6A6;

      end;

  end;

  ButtonAppearance.FillDisabled.Rounding := ButtonAppearance.Fill.Rounding;
  AppearanceOn.FillDisabled.Rounding := AppearanceOn.Fill.Rounding;
  AppearanceOff.FillDisabled.Rounding := AppearanceOff.Fill.Rounding;
  AppearanceOn.FillDisabled.GradientType := gtSolid;
  AppearanceOn.FillDisabled.Color := RGB(220, 220, 220);
  AppearanceOff.FillDisabled.GradientType := gtSolid;
  AppearanceOff.FillDisabled.Color := RGB(220, 220, 220);
  FillDisabled.GradientType := gtSolid;
  FillDisabled.Color := RGB(220, 220, 220);
  ButtonAppearance.FillDisabled.GradientType := gtSolid;
  ButtonAppearance.FillDisabled.Color := RGB(220, 220, 220);
  FillDisabled.BorderColor := clGray;
  ButtonAppearance.FillDisabled.BorderColor := clDkGray;
end;

procedure TAdvSmoothSlider.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSlider.SetFillDisabled(const Value: TGDIPFill);
begin
  if FFillDisabled <> Value then
  begin
    FFillDisabled.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSlider.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetInverseSlider(const Value: Boolean);
begin
  if FInverseSlider <> Value then
  begin
    FInverseSlider := Value;
    if FInverseSlider then
    begin
      AppearanceOn.Fill.RoundingType := rtRight;
      AppearanceOff.Fill.RoundingType := rtLeft;
    end
    else
    begin
      AppearanceOn.Fill.RoundingType := rtLeft;
      AppearanceOff.Fill.RoundingType := rtRight;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetShowFocus(const Value: Boolean);
begin
  if (FShowFocus <> Value) then
  begin
    FShowFocus := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetState(const Value: TAdvSmoothSliderState);
begin
  if (FState <> Value) then
  begin
    FState := Value;

    case Value of
      ssOn: FPosTo := 0;
      ssOff: FPosTo := -Width + ButtonAppearance.Size;
    end;
    FCurrentPos := FPosTo;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetValueOff(const Value: Double);
begin
  if FValueOff <> value then
  begin
    FValueOff := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.SetValueOn(const Value: Double);
begin
  if FValueOn <> value then
  begin
    FValueOn := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSlider.WMEnable(var Message: TWMEnable);
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothSlider.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothSlider.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothSlider.WMPaint(var Message: TWMPaint);
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
end;

{ TAdvSmoothSliderAppearance }

procedure TAdvSmoothSliderAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSliderAppearance) then
  begin
    FFont.Assign((Source as TAdvSmoothSliderAppearance).Font);
    FCaption := (Source as TAdvSmoothSliderAppearance).Caption;
    FFill.Assign((Source as TAdvSmoothSliderAppearance).Fill);
    FFillDisabled.Assign((Source as TAdvSmoothSliderAppearance).FillDisabled);
    FCaptionPosition := (Source as TAdvSmoothSliderAppearance).CaptionPosition;
    FCaptionLeft := (Source as TAdvSmoothSliderAppearance).CaptionLeft;
    FCaptionTop := (Source as TAdvSmoothSliderAppearance).CaptionTop;
    Changed;
  end;
end;

procedure TAdvSmoothSliderAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothSliderAppearance.Create(AOwner: TAdvSmoothSlider);
begin
  FOwner := AOwner;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFillDisabled := TGDIPFill.Create;
  FFillDisabled.OnChange := FillChanged;
  FFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FFont.Name := 'Tahoma';
  {$ENDIF}
  FFont.OnChange := FontChanged;
  FCaptionPosition := cpCenterCenter;
  FCaptionLeft := 0;
  FCaptionTop := 0;
end;

destructor TAdvSmoothSliderAppearance.Destroy;
begin
  FFillDisabled.Free;
  FFill.Free;
  FFont.Free;
  inherited;
end;

procedure TAdvSmoothSliderAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSliderAppearance.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSliderAppearance.SetCaption(const Value: String);
begin
  if FCaption <> value then
  begin
    FCaption := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSliderAppearance.SetCaptionLeft(const Value: integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSliderAppearance.SetCaptionPosition(
  const Value: TAdvSmoothSliderCaptionPosition);
begin
  if FCaptionPosition <> value then
  begin
    FCaptionPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSliderAppearance.SetCaptionTop(const Value: integer);
begin
  if FCaptionTop <> value then
  begin
    FCaptionTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothSliderAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSliderAppearance.SetFillDisabled(const Value: TGDIPFill);
begin
  if FFillDisabled <> Value then
  begin
    FFillDisabled.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSliderAppearance.SetFont(const Value: TFont);
begin
  if FFont <> value  then
  begin
    FFont.Assign(Value);
    FontChanged(Self);
  end;
end;

{ TAdvSmoothSliderButtonAppearance }

procedure TAdvSmoothSliderButtonAppearance.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothSliderButtonAppearance) then
  begin
    FSize := (Source as TAdvSmoothSliderButtonAppearance).Size;
    FFill.Assign((Source as TAdvSmoothSliderButtonAppearance).Fill);
    FFillDisabled.Assign((Source as TAdvSmoothSliderButtonAppearance).FillDisabled);
    Changed;
  end;
end;

procedure TAdvSmoothSliderButtonAppearance.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TAdvSmoothSliderButtonAppearance.Create(AOwner: TAdvSmoothSlider);
begin
  FOwner := AOwner;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FFillDisabled := TGDIPFill.Create;
  FFillDisabled.OnChange := FillChanged;
  FSize := 40;
end;

destructor TAdvSmoothSliderButtonAppearance.Destroy;
begin
  FFill.Free;
  FFillDisabled.Free;
  inherited;
end;

procedure TAdvSmoothSliderButtonAppearance.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothSliderButtonAppearance.SetFill(const Value: TGDIPFill);
begin
  if FFill <> value then
  begin
    FFill.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSliderButtonAppearance.SetFillDisabled(
  const Value: TGDIPFill);
begin
  if FFillDisabled <> Value then
  begin
    FFillDisabled.Assign(Value);
    FillChanged(Self);
  end;
end;

procedure TAdvSmoothSliderButtonAppearance.SetSize(const Value: integer);
begin
  if FSize <> value then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.

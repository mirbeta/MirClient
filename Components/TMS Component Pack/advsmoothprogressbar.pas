{*************************************************************************}
{ TAdvSmoothProgressBar component                                         }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2015                                             }
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
unit AdvSmoothProgressBar;

interface

{$I TMSDEFS.INC}

uses
  Classes, Controls, GDIPFill, Windows, Graphics,
  ExtCtrls, Math, SysUtils, AdvStyleIF, Messages,
  AdvGDIP, ComCtrls
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 9; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : issue with Maximum = Minimum
  // v1.0.1.0 : New: function AnimationInProgress returns true when progress bar is still animating
  // v1.0.1.1 : Fixed : Flicker when animating
  //          : Fixed : Issue with setting ProgressAnimation := false when Animation is busy
  // v1.0.2.0 : New : Property Transparent to hide background
  // v1.0.2.1 : Improved: Position changing with animation
  // v1.0.3.0 : New : Support for Windows Vista and Windows Seven Style
  // v1.0.4.0 : New : Built-in support for reduced color set for use with terminal servers
  // v1.0.4.1 : Improved : WMPaint only executed with Transparent settings
  // v1.0.4.2 : Fixed : Issue with animation in NextPosition
  // v1.0.4.3 : Improved : Transparency
  // v1.0.5.0 : New : Built-in support for Office 2010 colors
  // v1.0.5.1 : Fixed : Issue with border and black background
  // v1.5.0.0 : New : Marquee support with adjustable size, color and interval
  // v1.5.0.1 : Improved : Optional Shadows with Appearance.Shadows
  //          : Improved : Optional Overlays with Appearance.Overlays
  // v1.5.1.0 : New : OnDrawValue event
  // v1.6.0.0 : New : Vertical Direction
  // v1.6.0.1 : Fixed : Issue with direction of vertical direction
  // v1.7.0.0 : New : Metro Style support
  // v1.8.0.0 : New : Windows 8, Office 2013 styles added
  // v1.9.0.0 : New : Windows 10, Office 2016 styles added

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothProgressBarPositionChanged = procedure(Sender: TObject; Value: Double) of object;

  {$IFNDEF DELPHI2009_LVL}
  TProgressBarStyle = (pbstNormal, pbstMarquee);
  {$ENDIF}

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothProgressBar = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FPositionTo, FPositionTemp: double;
    FRect: TRect;
    FDesignTime, FDoAnimation, FAnimate: Boolean;
    FGlowCount: integer;
    FGlowPos: Double;
    FAnimationTimer, FMarqueeTimer: TTimer;
    FMaximum: Double;
    FAppearance: TGDIPProgress;
    FGlowAnimation: Boolean;
    FOnChange: TNotifyEvent;
    FMinimum: Double;
    FProgressAnimation: Boolean;
    FStep: Double;
    FPosition: Double;
    FOnPositionChanged: TAdvSmoothProgressBarPositionChanged;
    FMarqueeInterval: Integer;
    FStyle: TProgressBarStyle;
    FMarqueeColor: TColor;
    FMarqueeSize: Integer;
    FOnDrawValue: TGDIPProgressDrawValueEvent;
    FDirection: TGDIPProgressDirection;
    procedure SetAppearance(const Value: TGDIPProgress);
    procedure SetGlowAnimation(const Value: Boolean);
    procedure SetMaximum(const Value: Double);
    procedure SetMinimum(const Value: Double);
    procedure SetProgressAnimation(const Value: Boolean);
    procedure SetStep(const Value: Double);
    procedure SetPosition(const Value: Double);
    function GetVersion: String;
    procedure SetVersion(const Value: String);
    function GetPosition: Double;
    function GetNextPosition: Double;
    procedure SetMarqueeInterval(const Value: Integer);
    procedure SetPBStyle(const Value: TProgressBarStyle);
    procedure SetMarqueeColor(const Value: TColor);
    procedure SetMarqueeSize(const Value: Integer);
    procedure SetDirection(const Value: TGDIPProgressDirection);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure AppearanceChanged(Sender: TObject);
    procedure AnimateProgress(Sender: TObject);
    procedure MarqueeProgress(Sender: TObject);
    procedure DrawGlow(g: TGPGRaphics; r: TRect);
    procedure SetStyle(AStyle: TTMSStyle; Selected: Boolean);
    function IsProgressAnimation: Boolean;
    function IsGlowAnimation: Boolean;
    function GetVersionNr: integer;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure DoDrawValue(Sender: TObject; ValueFormat: string; var ValueText: String);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure Next; overload;
    procedure Previous; overload;
    procedure Next(AStep: Double); overload;
    procedure Previous(AStep: Double); overload;
    procedure GoToValue(AValue: Double);
    procedure GoToEnd;
    procedure GoToStart;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    function AnimationInProgress: Boolean;
    property NextPosition: Double read GetNextPosition;
    procedure SaveToTheme(FileName: String);
    procedure LoadFromTheme(FileName: String);
    function GetThemeID: String;
  published
    property Step: Double read FStep write SetStep;
    property Minimum: Double read FMinimum write SetMinimum;
    property Maximum: Double read FMaximum write SetMaximum;
    property Position: Double read GetPosition write SetPosition;
    property GlowAnimation: Boolean read FGlowAnimation write SetGlowAnimation default true;
    property ProgressAnimation: Boolean read FProgressAnimation write SetProgressAnimation default true;
    property Appearance: TGDIPProgress read FAppearance write SetAppearance;
    property Version: String read GetVersion write SetVersion;
    property Style: TProgressBarStyle read FStyle write SetPBStyle default pbstNormal;
    property MarqueeInterval: Integer read FMarqueeInterval write SetMarqueeInterval default 10;
    property MarqueeColor: TColor read FMarqueeColor write SetMarqueeColor default clLime;
    property MarqueeSize: Integer read FMarqueeSize write SetMarqueeSize default 60;
    property OnDrawValue: TGDIPProgressDrawValueEvent read FOnDrawValue write FOnDrawValue;
    property Direction: TGDIPProgressDirection read FDirection write SetDirection default pbdHorizontal;

    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnPositionChanged: TAdvSmoothProgressBarPositionChanged read FOnPositionChanged write FOnPositionChanged;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property ParentShowHint;
    property ShowHint;
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
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;   
    property Visible;
    property Hint;
    property TabStop default true;
  end;

implementation

{ TAdvSmoothProgressBar }

function AnimateDouble(var Start, Stop: Double; Delta: Double; Margin: Double): Boolean;
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

procedure TAdvSmoothProgressBar.AnimateProgress(Sender: TObject);
var
  d: Double;
  pos: Double;
begin
  if (IsProgressAnimation and FAnimate) or (Abs(NextPosition - FPositionTemp) > 0) then
  begin
    d := Abs(NextPosition - FPositionTemp) / 15;
    pos := FPositionTemp;
    FDoAnimation := AnimateDouble(pos, FPositionTo, d, 0.000001);
    if FDoAnimation then
    begin
      FPosition := Pos;    
      FPositionTemp := pos;
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self, FPosition);
      Changed;
    end
    else
    begin
      FAnimate := false;
      FPosition := NextPosition;
      FPositionTemp := NextPosition;
      if Assigned(FOnPositionChanged) then
        FOnPositionChanged(Self, FPosition);
      Changed;
    end;
  end;

  if (Position > Minimum) and IsGlowAnimation then
  begin
    if FGlowCount >= 1500 then
      FGlowPos := FGlowPos + 3;


    case Direction of
      pbdHorizontal:
      begin
        if FGlowPos > Appearance.CalculateProgressRectangle(FRect, Minimum, Maximum, Position, Direction).Width then
        begin
          FGlowCount := 0;
          FGlowPos := -60;
        end
        else
          FGlowCount := FGlowCount + 10;

      end ;
      pbdVertical:
      begin
        if FGlowPos > Appearance.CalculateProgressRectangle(FRect, Minimum, Maximum, Position, Direction).Height then
        begin
          FGlowCount := 0;
          FGlowPos := -60;
        end
        else
          FGlowCount := FGlowCount + 10;

      end;
    end;

    Changed;
  end;
end;

function TAdvSmoothProgressBar.AnimationInProgress: Boolean;
begin
 Result := IsProgressAnimation and not (Abs(FPositionTemp - NextPosition) < 1e-3);
end;

procedure TAdvSmoothProgressBar.AppearanceChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothProgressBar.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothProgressBar) then
  begin
    FStep := (Source as TAdvSmoothProgressBar).Step;
    FMinimum := (Source as TAdvSmoothProgressBar).Minimum;
    FMaximum := (Source as TAdvSmoothProgressBar).Maximum;
    FPosition := (Source as TAdvSmoothProgressBar).Position;
    FGlowAnimation := (Source as TAdvSmoothProgressBar).GlowAnimation;
    FProgressAnimation := (Source as TAdvSmoothProgressBar).ProgressAnimation;
    FAppearance.Assign((Source as TAdvSmoothProgressBar).Appearance);
    FStyle := (Source as TAdvSmoothProgressBar).Style;
    FMarqueeInterval := (Source as TAdvSmoothProgressBar).MarqueeInterval;
    FMarqueeColor := (Source as TAdvSmoothProgressBar).MarqueeColor;
    FMarqueeSize := (Source as TAdvSmoothProgressBar).MarqueeSize;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.Changed;
begin
  Invalidate;
end;

constructor TAdvSmoothProgressBar.Create(AOwner: TComponent);
begin
  inherited;
  DoubleBuffered := true;
  FStep := 10;
  FMinimum := 0;
  FMaximum := 100;
  FPositionTo := 0;
  FPositionTemp := 0;
  FPosition := 0;
  FGlowAnimation := true;
  FProgressAnimation := true;
  FGlowCount := 3000; //3sec
  FGlowPos := -40;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 10;
  FAnimationTimer.Enabled := true;
  FAnimationTimer.OnTimer := AnimateProgress;
  FMarqueeTimer := TTimer.Create(Self);
  FMarqueeTimer.Interval := 10;
  FMarqueeTimer.Enabled := False;
  FMarqueeTimer.OnTimer := MarqueeProgress;
  FMarqueeColor := clLime;
  FMarqueeSize := 60;

  FAppearance := TGDIPProgress.Create;
  FAppearance.OnDrawValue := DoDrawValue;
  FAppearance.OnChange := AppearanceChanged;
  TabStop := true;
  FStyle := pbstNormal;
  FMarqueeInterval := 10;
  FDirection := pbdHorizontal;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    SetComponentStyle(tsOffice2007Luna);

  Height := 17;
  Width := 150;
end;

destructor TAdvSmoothProgressBar.Destroy;
begin
  FAppearance.Free;
  FAnimationTimer.Free;
  FMarqueeTimer.Free;
  inherited;
end;

procedure TAdvSmoothProgressBar.DoDrawValue(Sender: TObject; ValueFormat: String;
  var ValueText: String);
begin
  if Assigned(OnDrawValue) then
    OnDrawValue(Sender, ValueFormat, ValueText);
end;

procedure TAdvSmoothProgressBar.DrawGlow(g: TGPGRaphics; r: TRect);
var
  b: TGPLinearGradientBrush;
  rr, lr, o: TGPRectF;
  rgn: TGPRegion;
  c: TColor;
  s: Integer;
begin
  c := clWhite;
  if Style = pbstMarquee then
    c := MarqueeColor;

  if Style = pbstNormal then
  begin
    rgn := TGPRegion.Create(Appearance.CalculateProgressRectangle(r, Minimum, Maximum, Position, Direction));
    g.SetClip(rgn);
  end
  else
    rgn := nil;

  s := 30;
  if Style = pbstMarquee then
    s := MarqueeSize;


  o := Appearance.GetInsideRectF(r);
  case Direction of
    pbdHorizontal:
    begin
      lr := MakeRect(FGlowPos - s, o.Y, s, o.Height);
      rr := MakeRect(FGlowPos, o.Y, s, o.Height);
      b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, c), MakeColor(120, c), LinearGradientModeHorizontal);
      g.FillRectangle(b, lr);
      b.free;
      b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, c), MakeColor(0, c), LinearGradientModeHorizontal);
      g.FillRectangle(b, rr);
      b.free;
    end;
    pbdVertical:
    begin
      lr := MakeRect(o.X, o.Height -  FGlowPos - s, o.Width, s);
      rr := MakeRect(o.X, o.Height - FGlowPos, o.Width, s);
      b := TGPLinearGradientBrush.Create(MakeRect(lr.X - 1, lr.Y - 1, lr.Width + 2, lr.Height + 2), MakeColor(0, c), MakeColor(120, c), LinearGradientModeVertical);
      g.FillRectangle(b, lr);
      b.free;
      b := TGPLinearGradientBrush.Create(MakeRect(rr.X - 1, rr.Y - 1, rr.Width + 2, rr.Height + 2), MakeColor(120, c), MakeColor(0, c), LinearGradientModeVertical);
      g.FillRectangle(b, rr);
      b.free;
    end;
  end;

  if Style = pbstNormal then
  begin
    g.ResetClip;
    rgn.free;
  end;
end;

procedure TAdvSmoothProgressBar.FillChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothProgressBar.IsGlowAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Result := GlowAnimation;
end;

function TAdvSmoothProgressBar.IsProgressAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Result := ProgressAnimation;
end;

procedure TAdvSmoothProgressBar.LoadFromTheme(FileName: String);
begin

end;

procedure TAdvSmoothProgressBar.MarqueeProgress(Sender: TObject);
begin
  if not (csDesigning in ComponentState) and not (csDestroying in ComponentState) then
  begin
    FGlowPos := FGlowPos + 8;

    case Direction of
      pbdHorizontal:
      begin
        if FGlowPos > Width + (MarqueeSize * 2) then
        begin
          FGlowCount := 0;
          FGlowPos := -(MarqueeSize * 2);
        end;
      end;
      pbdVertical:
      begin
        if FGlowPos > Height + (MarqueeSize * 2) then
        begin
          FGlowCount := 0;
          FGlowPos := -(MarqueeSize * 2);
        end;
      end;
    end;

    Changed;
  end;
end;

function TAdvSmoothProgressBar.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothProgressBar.GetNextPosition: Double;
begin
  Result := FPositionTo;
end;

function TAdvSmoothProgressBar.GetPosition: Double;
begin
  Result := FPosition;
end;

function TAdvSmoothProgressBar.GetThemeID: String;
begin
  Result := ClassName;
end;

function TAdvSmoothProgressBar.GetVersion: String;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

procedure TAdvSmoothProgressBar.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothProgressBar.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothProgressBar.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

function TAdvSmoothProgressBar.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvSmoothProgressBar.GoToEnd;
begin
  Position := Maximum;
end;

procedure TAdvSmoothProgressBar.GoToValue(AValue: Double);
begin
  Position := AValue;
end;

procedure TAdvSmoothProgressBar.GoToStart;
begin
  Position := Minimum;
end;

procedure TAdvSmoothProgressBar.Next(AStep: Double);
begin
  Position := NextPosition + AStep;
end;

procedure TAdvSmoothProgressBar.Next;
begin
  Position := NextPosition + Step;
end;

procedure TAdvSmoothProgressBar.Paint;
var
  g: TGPGraphics;
  r: TRect;
  pos: Double;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintClearTypeGridFit);

  r := Rect(0, 0, Width, Height);
  Frect := r;
  pos := Position;
  if Style = pbstMarquee then
    pos := 0;

  Appearance.Draw(g, r, Minimum, Maximum, pos, Direction);

  if (IsGlowAnimation and (Position > 0)) or (Style = pbstMarquee) then
    DrawGlow(g, r);

  g.Free;
end;

procedure TAdvSmoothProgressBar.Previous(AStep: Double);
begin
  Position := NextPosition - AStep;
end;

procedure TAdvSmoothProgressBar.Previous;
begin
  Position := NextPosition - Step;
end;

procedure TAdvSmoothProgressBar.Resize;
begin
  inherited;
  Changed;
end;

procedure TAdvSmoothProgressBar.SaveToTheme(FileName: String);
begin

end;

procedure TAdvSmoothProgressBar.SetAppearance(const Value: TGDIPProgress);
begin
  if FAppearance <> value then
  begin
    FAppearance.Assign(Value);
    AppearanceChanged(Self);
  end;
end;

procedure TAdvSmoothProgressBar.SetColorTones(ATones: TColorTones);
begin
  Appearance.Overlays := False;
  Appearance.Shadows := False;
  GlowAnimation := False;
  Appearance.BackGroundFill.Color := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirror := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.ColorMirrorTo := ATones.Foreground.BrushColor;
  Appearance.BackGroundFill.BorderColor := ATones.Foreground.BorderColor;

  Appearance.ProgressFill.Color := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirror := ATones.Selected.BrushColor;
  Appearance.ProgressFill.ColorMirrorTo := ATones.Selected.BrushColor;
  Appearance.ProgressFill.BorderColor := ATones.Selected.BorderColor;
  Appearance.Font.Color := ATones.Selected.TextColor;
end;

procedure TAdvSmoothProgressBar.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  Appearance.Overlays := True;
  Appearance.Shadows := True;
  GlowAnimation := True;
  SetStyle(AStyle, false);
end;

procedure TAdvSmoothProgressBar.SetDirection(
  const Value: TGDIPProgressDirection);
begin
  if FDirection <> Value then
  begin
    FDirection := Value;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetGlowAnimation(const Value: Boolean);
begin
  if FGlowAnimation <> value then
  begin
    FGlowAnimation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetMarqueeColor(const Value: TColor);
begin
  if FMarqueeColor <> Value then
  begin
    FMarqueeColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetMarqueeInterval(const Value: Integer);
begin
  if FMarqueeInterval <> Value then
  begin
    FMarqueeInterval := Value;
    if Assigned(FMarqueeTimer) then
      FMarqueeTimer.Interval := FMarqueeInterval;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetMarqueeSize(const Value: Integer);
begin
  if FMarqueeSize <> Value then
  begin
    FMarqueeSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetMaximum(const Value: Double);
begin
  if FMaximum <> value then
  begin
    FMaximum := Value;
    if FMaximum < FPosition then
    begin
      if IsProgressAnimation then
        FPositionTo := FMaximum
      else
      begin
        FPosition := FMaximum;
        FPositionTemp := FMaximum;
        FPositionTo := FMaximum;
      end;
    end;

    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetMinimum(const Value: Double);
begin
  if FMinimum <> value then
  begin
    FMinimum := Value;
    if FMinimum > FPosition then
    begin
      if IsProgressAnimation then
        FPositionTo := FMinimum
      else
      begin
        FPosition := FMinimum;
        FPositionTemp := FMinimum;
        FPositionTo := FMinimum;
      end;
    end;

    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetPBStyle(const Value: TProgressBarStyle);
begin
  if FStyle <> Value then
  begin
    FStyle := Value;
    case FStyle of
      pbstNormal:
      begin
        if Assigned(FAnimationTimer) then
          FAnimationTimer.Enabled := True;

        if Assigned(FMarqueeTimer) then
          FMarqueeTimer.Enabled := False;
      end;
      pbstMarquee:
      begin
        if Assigned(FAnimationTimer) then
          FAnimationTimer.Enabled := False;

        if Assigned(FMarqueeTimer) then
          FMarqueeTimer.Enabled := True;
      end;
    end;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetPosition(const Value: Double);
var
  anim: Boolean;
begin
  anim := AnimationInProgress and IsProgressAnimation;
  FPositionTo := Min(Max(Minimum, Value), Maximum);
  if anim then
  begin
    FPosition := NextPosition;
    FPositionTemp := FPosition;
    Changed;
  end;
  if not IsProgressAnimation then
  begin
    FPosition := NextPosition;
    FPositionTemp := FPosition;
    if Assigned(FOnPositionChanged) then
      FOnPositionChanged(Self, FPosition);

    Changed;
  end
  else
    FAnimate := true;
end;

procedure TAdvSmoothProgressBar.SetProgressAnimation(const Value: Boolean);
begin
  if FProgressAnimation <> value then
  begin
    FProgressAnimation := Value;
    Changed;
  end;
  if FProgressAnimation = false then
    SetPosition(NextPosition);
end;

procedure TAdvSmoothProgressBar.SetStep(const Value: Double);
begin
  if FStep <> value then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothProgressBar.SetStyle(AStyle: TTMSStyle; Selected: Boolean);
begin
  with Appearance do
  begin
    case AStyle of
      tsOffice2003Blue:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FCE1CB;
          ProgressFill.ColorTo := $E0A57D;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Silver:
      begin
        BackGroundFill.Color := $00E6D8D8;
        BackGroundFill.ColorTo := $00E6D8D8;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $ECE2E1;
          ProgressFill.ColorTo := $B39698;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $947C7C;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Olive:
      begin
        BackGroundFill.Color := $CFF0EA;
        BackGroundFill.ColorTo := $CFF0EA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $CFF0EA;
          ProgressFill.ColorTo := $8CC0B1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $588060;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2003Classic:
      begin
        BackGroundFill.Color := $00F2F2F2;
        BackGroundFill.ColorTo := $00F2F2F2;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := $C9D1D5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $808080;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $B59285;
          ProgressFill.ColorTo := $B59285;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Luna:
      begin
        BackGroundFill.Color := $00FFD2AF;
        BackGroundFill.ColorTo := $00FFD2AF;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FFEFE3;
          ProgressFill.ColorTo := $FFDDC4;
          ProgressFill.ColorMirror := $FFD1AD;
          ProgressFill.ColorMirrorTo := $FFDBC0;
          ProgressFill.BorderColor := $FFD1AD;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := $FFD1AD;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsOffice2007Obsidian:
      begin
        BackGroundFill.Color := $5C534C;
        BackGroundFill.ColorTo := $5C534C;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F9F8F8;
          ProgressFill.ColorTo := $E4E2DF;
          ProgressFill.ColorMirror := $D1CBC7;
          ProgressFill.ColorMirrorTo := $E2DEDB;
          ProgressFill.BorderColor := clBlack;//$D1CBC7;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsXP:
      begin
        BackGroundFill.Color := $00B6B6B6;
        BackGroundFill.ColorTo := $00B6B6B6;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clBtnFace;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := clInActiveCaption;
          ProgressFill.ColorTo := clInActiveCaption;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clBlack;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWhidbey:
      begin
        BackGroundFill.Color := $F5F9FA;
        BackGroundFill.ColorTo := $F5F9FA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $F5F9FA;
          ProgressFill.ColorTo := $A8C0C0;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $94E6FB;
          ProgressFill.ColorTo := $1595EE;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $962D00;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsCustom: ;
      tsOffice2007Silver:
      begin
        BackGroundFill.Color := $00CAC1BA;
        BackGroundFill.ColorTo := $00CAC1BA;
        BackGroundFill.BorderColor := $00C0C0C0;

        if not Selected then
        begin
          ProgressFill.Color := $FAEEEB;
          ProgressFill.ColorTo := $E5DBD7;
          ProgressFill.ColorMirror := $E2D8D4;
          ProgressFill.ColorMirrorTo := $D1C7C5;
          ProgressFill.BorderColor := clBlack;//$E2D8D4;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $AAD9FF;
          ProgressFill.ColorTo := $6EBBFF;
          ProgressFill.ColorMirror := $42AEFE;
          ProgressFill.ColorMirrorTo := $7AE1FE;
          ProgressFill.BorderColor := clBlack;//$42AEFE;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindowsVista:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not Selected then
        begin
          ProgressFill.Color := $FDF8F1;
          ProgressFill.ColorTo := $FCEFD5;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FDDE99;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FEF9F0;
          ProgressFill.ColorTo := $FDF0D7;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FEDF9A;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsWindows7:
      begin
        BackGroundFill.Color := $FDF8F1;
        BackGroundFill.ColorTo := $FDF8F1;
        BackGroundFill.BorderColor := $FDDE99;

        if not Selected then
        begin
          ProgressFill.Color := $FDFBFA;
          ProgressFill.ColorTo := $FDF3EB;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $FBD6B8;
          ProgressFill.GradientMirrorType := gtVertical;
        end
        else
        begin
          ProgressFill.Color := $FCEBDC;
          ProgressFill.ColorTo := $FCDBC1;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := $CEA27D;
          ProgressFill.GradientMirrorType := gtVertical;
        end;
      end;
      tsTerminal:
      begin
        BackGroundFill.Color := clBtnFace;
        BackGroundFill.ColorTo := clBtnFace;
        BackGroundFill.BorderColor := clGray;

        if not Selected then
        begin
          ProgressFill.Color := clSilver;
          ProgressFill.ColorTo := clSilver;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end
        else
        begin
          ProgressFill.Color := clWhite;
          ProgressFill.ColorTo := clWhite;
          ProgressFill.ColorMirror := clNone;
          ProgressFill.ColorMirrorTo := clNone;
          ProgressFill.BorderColor := clGray;

        end;
      end;
      tsOffice2010Blue:
      begin
        BackGroundFill.Color := $FDF6EF;
        BackGroundFill.ColorTo := $F0DAC7;
        BackGroundFill.BorderColor := $C7B29F;

        ProgressFill.Color := $EDDBCD;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $5B391E;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Silver:
      begin
        BackGroundFill.Color := $FFFFFF;
        BackGroundFill.ColorTo := $EDE5E0;
        BackGroundFill.BorderColor := $D2CDC8;

        ProgressFill.Color := $EDE9E5;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $7C6D66;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
      tsOffice2010Black:
      begin
        BackGroundFill.Color := $BFBFBF;
        BackGroundFill.ColorTo := $919191;
        BackGroundFill.BorderColor := $D7D7D6;

        ProgressFill.Color := $828282;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6D6D6D;
        ProgressFill.GradientMirrorType := gtVertical;

      end;
    tsWindows8, tsWindows10:
      begin

        Appearance.Overlays := False;
        Appearance.Shadows := False;
        BackGroundFill.Color := $F7F6F5;
        BackGroundFill.ColorTo := $F7F6F5;
        BackGroundFill.BorderColor := $E4E3E2;

        ProgressFill.Color := $F7E0C9;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E4A262;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013White:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2013LightGray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $F6F6F6;
        BackGroundFill.ColorTo := $F6F6F6;
        BackGroundFill.BorderColor := $C6C6C6;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2013Gray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $E5E5E5;
        BackGroundFill.ColorTo := $E5E5E5;
        BackGroundFill.BorderColor := $ABABAB;

        ProgressFill.Color := $FCE2C8;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := clNone; //$E59D56;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016White:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := clWhite;
        BackGroundFill.ColorTo := clWhite;
        BackGroundFill.BorderColor := $D4D4D4;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;
    tsOffice2016Gray:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $B2B2B2;
        BackGroundFill.ColorTo := $B2B2B2;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $E3BDA3;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $E3BDA3;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    tsOffice2016Black:
      begin
        Appearance.Overlays := False;
        Appearance.Shadows := False;

        BackGroundFill.Color := $363636;
        BackGroundFill.ColorTo := $363636;
        BackGroundFill.BorderColor := $444444;

        ProgressFill.Color := $6A6A6A;
        ProgressFill.ColorTo := clNone;
        ProgressFill.ColorMirror := clNone;
        ProgressFill.ColorMirrorTo := clNone;
        ProgressFill.BorderColor := $6A6A6A;
        ProgressFill.GradientMirrorType := gtVertical;
      end;

    end;
  end;
end;

procedure TAdvSmoothProgressBar.SetVersion(const Value: String);
begin

end;

procedure TAdvSmoothProgressBar.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  message.Result := 1;
end;

procedure TAdvSmoothProgressBar.WMPaint(var Message: TWMPaint);
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
  chk := (Appearance.Transparent = false) and (Appearance.BackGroundFill.Opacity = 255) and (Appearance.BackGroundFill.OpacityTo = 255)
    and (Appearance.BackGroundFill.OpacityMirror = 255) and (Appearance.BackGroundFill.OpacityMirrorTo = 255);

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

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.

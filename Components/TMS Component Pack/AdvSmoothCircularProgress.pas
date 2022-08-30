{**************************************************************************}
{ TAdvSmoothCircularProgress component                                     }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2013 - 2015                                                }
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

unit AdvSmoothCircularProgress;

{$I TMSDEFS.INC}

interface

uses
  Windows, Classes, Graphics, Controls, Messages, GDIPFill, AdvGDIP, 
  Math, ExtCtrls, SysUtils, AdvStyleIF
  {$IFDEF DELPHIXE3_LVL}
  ,UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.0.0.1 : Fixed : Issue with inherited not called in Resize override;
  // v1.0.0.2 : Fixed : Memory leak
  // v1.1.0.0 : New : Windows 10, Office 2016 styles added

type
  {$IFNDEF DELPHI2009_LVL}
  TProgressBarStyle = (pbstNormal, pbstMarquee);
  {$ENDIF}
  
  TAdvSmoothCircularProgress = class;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothCircularProgressDigits = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FShadow: Boolean;
    FFormat: String;
    FBorderColor: TColor;
    FBorderWidth: Integer;
    FFont: TFont;
    FShadowColor: TColor;
    FVisible: Boolean;
    FOverlay: Boolean;
    procedure SetShadow(const Value: Boolean);
    procedure SetFormat(const Value: String);
    procedure SetBorderColor(const Value: TColor);
    procedure SetBorderWidth(const Value: Integer);
    procedure SetFont(const Value: TFont);
    procedure SetShadowColor(const Value: TColor);
    procedure SetVisible(const Value: Boolean);
    procedure SetOverlay(const Value: Boolean);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothCircularProgress);
    destructor Destroy; override;
  published
    property Format: String read FFormat write SetFormat;
    property Overlay: Boolean read FOverlay write SetOverlay default True;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clDkGray;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property BorderWidth: Integer read FBorderWidth write SetBorderWidth default 1;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Font: TFont read FFont write SetFont;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

  TAdvSmoothCircularProgressOuterCircle = class(TPersistent)
  private
    FFill: TGDIPFill;
    FSize: Integer;
    FOnChange: TNotifyEvent;
    FShadow: Boolean;
    FShadowColor: TColor;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSize(const Value: Integer);
    procedure SetShadow(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothCircularProgress);
    destructor Destroy; override;
  published
    property Size: Integer read FSize write SetSize default 15;
    property Fill: TGDIPFill read FFill write SetFill;
    property Shadow: Boolean read FShadow write SetShadow default True;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothCircularProgressInnerCircle = class(TPersistent)
  private
    FFill: TGDIPFill;
    FSize: Integer;
    FOnChange: TNotifyEvent;
    FShadow: Boolean;
    FShadowColor: TColor;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetSize(const Value: Integer);
    procedure SetShadow(const Value: Boolean);
    procedure SetShadowColor(const Value: TColor);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothCircularProgress);
    destructor Destroy; override;
  published
    property Shadow: Boolean read FShadow write SetShadow default True;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clBlack;
    property Size: Integer read FSize write SetSize default 65;
    property Fill: TGDIPFill read FFill write SetFill;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothCircularProgressCircle = class(TPersistent)
  private
    FFill: TGDIPFill;
    FOnChange: TNotifyEvent;
    FBackGroundFill: TGDIPFill;
    FStepLineColor: TColor;
    FStepLineWidth: Integer;
    FStepFont: TFont;
    FStepFormat: string;
    FStepsVisible: Boolean;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetBackGroundFill(const Value: TGDIPFill);
    procedure SetStepLineColor(const Value: TColor);
    procedure SetStepLineWidth(const Value: Integer);
    procedure SetStepFont(const Value: TFont);
    procedure SetStepFormat(const Value: string);
    procedure SetStepsVisible(const Value: Boolean);
  protected
    procedure Changed;
    procedure FillChanged(Sender: TObject);
    procedure FontChanged(Sender: TObject);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AOwner: TAdvSmoothCircularProgress);
    destructor Destroy; override;
  published
    property StepFont: TFont read FStepFont write SetStepFont;
    property StepLineColor: TColor read FStepLineColor write SetStepLineColor default clGray;
    property StepLineWidth: Integer read FStepLineWidth write SetStepLineWidth default 1;
    property BackGroundFill: TGDIPFill read FBackGroundFill write SetBackGroundFill;
    property Fill: TGDIPFill read FFill write SetFill;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property StepFormat: string read FStepFormat write SetStepFormat;
    property StepsVisible: Boolean read FStepsVisible write SetStepsVisible default True;
  end;

  TAdvSmoothCircularProgressPositionChanged = procedure(Sender: TObject; Value: Single) of object;
  TAdvSmoothCircularProgressGetValue = procedure(Sender: TObject; AValue: Single; var AValueString: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothCircularProgress = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FMouseDown: Boolean;
    FMarqueePos: Single;
    FAnimationTimer, FMarqueeTimer: TTimer;
    FPositionTo, FPositionTemp: Single;
    FDesignTime, FDoAnimation, FAnimate: Boolean;
    FOuterRect, FDigitsRect, FInnerRect, FProgressRect: TGPRectF;
    FUpdateCount: Integer;
    FOuterCircle: TAdvSmoothCircularProgressOuterCircle;
    FAutoRounding: Boolean;
    FAspectRatio: Boolean;
    FInnerCircle: TAdvSmoothCircularProgressInnerCircle;
    FMaximum: Single;
    FPosition: Single;
    FMinimum: Single;
    FProgressCircle: TAdvSmoothCircularProgressCircle;
    FStep: Single;
    FProgressAnimation: Boolean;
    FOnPositionChanged: TAdvSmoothCircularProgressPositionChanged;
    FMarqueeInterval: Integer;
    FStyle: TProgressBarStyle;
    FAnimationFactor: Single;
    FDigits: TAdvSmoothCircularProgressDigits;
    FOnGetStepValue: TAdvSmoothCircularProgressGetValue;
    FOnGetPositionValue: TAdvSmoothCircularProgressGetValue;
    FInteraction: Boolean;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetOuterCircle(const Value: TAdvSmoothCircularProgressOuterCircle);
    procedure SetAutoRounding(const Value: Boolean);
    procedure SetAspectRatio(const Value: Boolean);
    procedure SetInnerCircle(
      const Value: TAdvSmoothCircularProgressInnerCircle);
    procedure SetMaximum(const Value: Single);
    procedure SetMinimum(const Value: Single);
    procedure SetPosition(const Value: Single);
    procedure SetProgressCircle(const Value: TAdvSmoothCircularProgressCircle);
    procedure SetStep(const Value: Single);
    function GetPosition: Single;
    function GetNextPosition: Single;
    procedure SetMarqueeInterval(const Value: Integer);
    procedure SetPBStyle(const Value: TProgressBarStyle);
    procedure SetAnimationFactor(const Value: Single);
    procedure SetDigits(const Value: TAdvSmoothCircularProgressDigits);
    procedure SetInteraction(const Value: Boolean);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure MouseMove(Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState;
      X, Y: Integer); override;
    procedure AnimateProgress(Sender: TObject);
    procedure MarqueeProgress(Sender: TObject);  
    procedure Changed;
    procedure ProgressCircleChanged(Sender: TObject);
    procedure OuterCircleChanged(Sender: TObject);
    procedure DigitsChanged(Sender: TObject);
    procedure InnerCircleChanged(Sender: TObject);
    procedure UpdateCircularProgress;
    procedure DrawOuterCircle(g: TGPGraphics);
    procedure DrawProgress(g: TGPGraphics);
    procedure DrawProgressBackGround(g: TGPGraphics);
    procedure DrawSteps(g: TGPGraphics);
    procedure DrawDigits(g: TGPGraphics);
    procedure DrawDigit(g: TGPGraphics; ARect: TGPRectF; ADigit: String);
    function GetFontStyle(AFontStyles: TFontStyles): Integer;
    procedure DrawOuterCircleShadow(g: TGPGraphics);
    procedure DrawInnerCircle(g: TGPGraphics);
    procedure DrawInnerCircleShadow(g: TGPGraphics);
    function GetPositionStringSize: TGPRectF;
    function GetDigitSize(AGraphics: TGPGraphics; ADigit: String): TGPRectF;
  public
    function XYToValue(X, Y: Integer): Single;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure ApplyDefaultStyle;
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure Resize; override;
    procedure Next; overload;
    procedure Previous; overload;
    procedure Next(AStep: Single); overload;
    procedure Previous(AStep: Single); overload;
    procedure GoToValue(AValue: Single);
    procedure GoToEnd;
    procedure GoToStart;
    property NextPosition: Single read GetNextPosition;  
    function AnimationInProgress: Boolean;      
    function IsProgressAnimation: Boolean;
    function GetPositionString: String;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
  published
    property Interaction: Boolean read FInteraction write SetInteraction default True;
    property AnimationFactor: Single read FAnimationFactor write SetAnimationFactor;
    property Style: TProgressBarStyle read FStyle write SetPBStyle default pbstNormal;  
    property AspectRatio: Boolean read FAspectRatio write SetAspectRatio default True;
    property AutoRounding: Boolean read FAutoRounding write SetAutoRounding default True;
    property OuterCircle: TAdvSmoothCircularProgressOuterCircle read FOuterCircle write SetOuterCircle;
    property InnerCircle: TAdvSmoothCircularProgressInnerCircle read FInnerCircle write SetInnerCircle;
    property ProgressCircle: TAdvSmoothCircularProgressCircle read FProgressCircle write SetProgressCircle;
    property Minimum: Single read FMinimum write SetMinimum;
    property Maximum: Single read FMaximum write SetMaximum;
    property Position: Single read GetPosition write SetPosition;
    property Digits: TAdvSmoothCircularProgressDigits read FDigits write SetDigits;
    property Step: Single read FStep write SetStep;
    property OnPositionChanged: TAdvSmoothCircularProgressPositionChanged read FOnPositionChanged write FOnPositionChanged;
    property OnGetStepValue: TAdvSmoothCircularProgressGetValue read FOnGetStepValue write FOnGetStepValue;
    property OnGetPositionValue: TAdvSmoothCircularProgressGetValue read FOnGetPositionValue write FOnGetPositionValue;
    property MarqueeInterval: Integer read FMarqueeInterval write SetMarqueeInterval default 10;

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

procedure GetPath(l, t, w, h, d, radius: Double; RoundingType: TFillRoundingType; var path: TGPGraphicsPath);
begin
  case RoundingType of
    rtNone:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t); // left
    end;
    rtRight:
    begin
      path.AddLine(l, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l, t + h); // bottom
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtLeft:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddLine(l + w, t, l + w, t + h); // right
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
    rtTop:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h); // right
      path.AddLine(l + w, t + h, l, t + h); // bottom
      path.AddLine(l, t + h, l, t + Radius); // left
    end;
    rtBottom:
    begin
      path.AddLine(l, t, l + w, t); // top
      path.AddLine(l + w, t, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - Radius, l, t ); // left
    end;
    rtBoth:
    begin
      path.AddArc(l, t, d, d, 180, 90); // topleft
      path.AddLine(l + radius, t, l + w - radius, t); // top
      path.AddArc(l + w - d, t, d, d, 270, 90); // topright
      path.AddLine(l + w, t + radius, l + w, t + h - radius); // right
      path.AddArc(l + w - d, t + h - d, d, d, 0, 90); // bottomright
      path.AddLine(l + w - radius, t + h, l + radius, t + h); // bottom
      path.AddArc(l, t + h - d, d, d, 90, 90); // bottomleft
      path.AddLine(l, t + h - radius, l, t + radius); // left
    end;
  end;
end;

function CreateRoundRectangle(R: TGPRectF; Radius: Integer; RoundingType: TFillRoundingType; Mirror: Boolean): TGPGraphicsPath; overload;
var
  l, t, w, h, d: Double;
begin
  Result := TGPGraphicsPath.Create;
  l := R.X;
  t := R.Y;
  w := R.Width;
  h := R.Height;
  d := Radius shl 1;
  GetPath(l, t, w, h, d, radius, RoundingType, Result);
  Result.CloseFigure();
end;

function AnimateDouble(var Start, Stop: Single; Delta: Single; Margin: Single): Boolean;
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

procedure TAdvSmoothCircularProgress.AnimateProgress(Sender: TObject);
var
  d: Single;
  pos: Single;
begin
  if (IsProgressAnimation and FAnimate) or (Abs(NextPosition - FPositionTemp) > 0) then
  begin
    d := Abs(NextPosition - FPositionTemp) / Max(1, AnimationFactor);
    pos := FPositionTemp;
    FDoAnimation := AnimateDouble(pos, FPositionTo, d, 0.00001);
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
end;

{ TAdvSmoothCircularProgress }

function TAdvSmoothCircularProgress.AnimationInProgress: Boolean;
begin
  Result := IsProgressAnimation and not (Abs(FPositionTemp - NextPosition) < 1e-3);
end;

procedure TAdvSmoothCircularProgress.ApplyDefaultStyle;
begin
  BeginUpdate;
  FOuterCircle.FFill.Color := RGB(209, 210, 213);
  FOuterCircle.FFill.ColorTo := clWhite;
  FOuterCircle.FFill.BorderColor := RGB(201, 202, 205);

  FInnerCircle.FFill.Color := RGB(235, 235, 235);
  FInnerCircle.FFill.ColorTo := clNone;
  FInnerCircle.FFill.BorderColor := RGB(201, 202, 205);

  FProgressCircle.FFill.Color := RGB(28, 117, 208);
  FProgressCircle.FFill.ColorTo := RGB(75, 205, 238);
  FProgressCircle.FFill.GradientType := gtHorizontal;
  FProgressCircle.FFill.BorderColor := $C9CACD;

  FProgressCircle.FBackGroundFill.Color := RGB(210, 212, 216);
  FProgressCircle.FBackGroundFill.ColorTo := clNone;
  FProgressCircle.FBackGroundFill.GradientType := gtHorizontal;
  FProgressCircle.FBackGroundFill.BorderColor := $C9CACD;

  FProgressCircle.FStepLineColor := RGB(28, 117, 208);
  
  EndUpdate;
end;

procedure TAdvSmoothCircularProgress.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothCircularProgress then
  begin
    FInteraction := (Source as TAdvSmoothCircularProgress).Interaction;
    FOuterCircle.Assign((Source as TAdvSmoothCircularProgress).OuterCircle);
    FAspectRatio := (Source as TAdvSmoothCircularProgress).AspectRatio;
    FAutoRounding := (Source as TAdvSmoothCircularProgress).AutoRounding;
    FInnerCircle.Assign((Source as TAdvSmoothCircularProgress).InnerCircle);
    FProgressCircle.Assign((Source as TAdvSmoothCircularProgress).ProgressCircle);
    FMinimum := (Source as TAdvSmoothCircularProgress).Minimum;
    FMaximum := (Source as TAdvSmoothCircularProgress).Maximum;
    FPosition := (Source as TAdvSmoothCircularProgress).Position;
    FStep := (Source as TAdvSmoothCircularProgress).Step;
    FAnimationFactor := (Source as TAdvSmoothCircularProgress).AnimationFactor;
    FStyle := (Source as TAdvSmoothCircularProgress).Style;
    FMarqueeInterval := (Source as TAdvSmoothCircularProgress).MarqueeInterval;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TAdvSmoothCircularProgress.UpdateCircularProgress;
var
  o, oi, op: TNotifyEvent;
  w, h, pw, ph: Single;
  sz: TGPRectF;
begin
  o := OuterCircle.Fill.OnChange;
  oi := InnerCircle.Fill.OnChange;
  op := ProgressCircle.Fill.OnChange;

  OuterCircle.Fill.OnChange := nil;
  InnerCircle.Fill.OnChange := nil;
  ProgressCircle.Fill.OnChange := nil;

  if AspectRatio then
  begin
    if Width < Height then
      FOuterRect := MakeRect(0, (Height - Width) / 2, Width, Width)
    else
      FOuterRect := MakeRect((Width - Height) / 2, 0, Height, Height);
  end
  else
    FOuterRect := MakeRect(0, 0, Width, Height);

  w := FOuterRect.Width * FInnerCircle.Size / 100;
  h := FOuterRect.Height * FInnerCircle.Size / 100;
  pw := FOuterRect.Width * (100 - FOuterCircle.Size) / 100;
  ph := FOuterRect.Height * (100 - FOuterCircle.Size) / 100;
  FInnerRect := MakeRect(FOuterRect.X + (FOuterRect.Width - w) / 2, FOuterRect.Y + (FOuterRect.Height - h) / 2, w, h);
  FProgressRect := MakeRect(FOuterRect.X + (FOuterRect.Width - pw) / 2, FOuterRect.Y + (FOuterRect.Height - ph) / 2, pw, ph);
  sz := GetPositionStringSize;
  FDigitsRect := MakeRect(FOuterRect.X + (FOuterRect.Width - sz.Width) / 2, FInnerRect.Y + (FInnerRect.Height - sz.Height) / 2, sz.Width, sz.Height);

  if AutoRounding then
  begin
    if FOuterRect.Width > FOuterRect.Height then
      OuterCircle.Fill.Rounding := Round(FOuterRect.Height / 2)
    else
      OuterCircle.Fill.Rounding := Round(FOuterRect.Width / 2);

    if FInnerRect.Width > FInnerRect.Height then
      InnerCircle.Fill.Rounding := Round(FInnerRect.Height / 2)
    else
      InnerCircle.Fill.Rounding := Round(FInnerRect.Width / 2);

    if FProgressRect.Width > FProgressRect.Height then
    begin
      ProgressCircle.Fill.Rounding := Round(FProgressRect.Height / 2);
      ProgressCircle.BackGroundFill.Rounding := Round(FProgressRect.Height / 2);
    end
    else
    begin
      ProgressCircle.Fill.Rounding := Round(FProgressRect.Width / 2);
      ProgressCircle.BackGroundFill.Rounding := Round(FProgressRect.Width / 2);      
    end;
  end;

  OuterCircle.Fill.OnChange := o;
  InnerCircle.Fill.OnChange := oi;
  ProgressCircle.Fill.OnChange := op;
end;

procedure TAdvSmoothCircularProgress.Changed;
begin
  //recalculate
  UpdateCircularProgress;
  Invalidate;
end;

procedure TAdvSmoothCircularProgress.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothCircularProgress.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothCircularProgress.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

constructor TAdvSmoothCircularProgress.Create(AOwner: TComponent);
begin
  inherited;
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  DoubleBuffered := True;
  FInteraction := True;
  FAnimationFactor := 5;
  Width := 200;
  Height := 200;
  FAspectRatio := True;
  FAutoRounding := True;
  FMinimum := 0;
  FMaximum := 100;
  FPosition := 0;
  FStep := 100;
  FOuterCircle := TAdvSmoothCircularProgressOuterCircle.Create(Self);
  FOuterCircle.OnChange := OuterCircleChanged;
  FInnerCircle := TAdvSmoothCircularProgressInnerCircle.Create(Self);
  FInnerCircle.OnChange := InnerCircleChanged;
  FProgressCircle := TAdvSmoothCircularProgressCircle.Create(Self);
  FProgressCircle.OnChange := ProgressCircleChanged;
  FDigits := TAdvSmoothCircularProgressDigits.Create(Self);
  FDigits.OnChange := DigitsChanged;

  FStep := 10;
  FPositionTo := 0;
  FPositionTemp := 0;
  FProgressAnimation := true;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 10;
  FAnimationTimer.Enabled := true;
  FAnimationTimer.OnTimer := AnimateProgress;
  FMarqueeTimer := TTimer.Create(Self);
  FMarqueeTimer.Interval := 10;
  FMarqueeTimer.Enabled := False;
  FMarqueeTimer.OnTimer := MarqueeProgress;

  TabStop := true;
  FStyle := pbstNormal;
  FMarqueeInterval := 10;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    ApplyDefaultStyle;
end;

destructor TAdvSmoothCircularProgress.Destroy;
begin
  FMarqueeTimer.Free;
  FAnimationTimer.Free;
  FDigits.Free;
  FOuterCircle.Free;
  FInnerCircle.Free;
  FProgressCircle.Free;
  inherited;
end;

procedure TAdvSmoothCircularProgress.DigitsChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgress.DrawDigit(g: TGPGraphics; ARect: TGPRectF; ADigit: String);
var
  pth: TGPGraphicsPath;
  r: TGPRectF;
  b: TGPSolidBrush;
  p: TGPPen;
  sf: TGPStringFormat;
  f: TGPFontFamily;
  lb: TGPLinearGradientBrush;
  rs: TGPRectF;
  rgn: TGPRegion;
begin
  r := ARect;
  pth := TGPGraphicsPath.Create;

  f := TGPFontFamily.Create(Digits.Font.Name);

  if (f.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    f.Free;
    f := TGPFontFamily.Create('Arial');
  end;

  sf := TGPStringFormat.Create;
  sf.SetAlignment(StringAlignmentCenter);
  sf.SetLineAlignment(StringAlignmentCenter);

  if Digits.Shadow then
  begin
    b := TGPSolidBrush.Create(MakeColor(75, Digits.ShadowColor));
    rs := MakeRect(r.X + 2, r.Y + 2, r.Width, r.Height);
    pth.AddString(ADigit, Length(ADigit), f, GetFontStyle(Digits.Font.Style), Digits.Font.Size, rs, sf);
    g.FillPath(b, pth);
    b.Free;
  end;

  pth.Reset;
  pth.AddString(ADigit, Length(ADigit), f, GetFontStyle(Digits.Font.Style), Digits.Font.Size, r, sf);
  b := TGPSolidBrush.Create(MakeColor(255, Digits.Font.Color));
  p := TGPPen.Create(MakeColor(255, Digits.BorderColor), Digits.BorderWidth);
  g.FillPath(b, pth);
  g.DrawPath(p, pth);
  b.Free;
  p.Free;

  rgn := TGPRegion.Create(pth);
  g.SetClip(rgn);

  if Digits.Overlay then
  begin
    lb := TGPLinearGradientBrush.Create(r, MakeColor(0, clSilver), MakeColor(100, clWhite), LinearGradientModeForwardDiagonal);
    g.FillPath(lb, pth);
    lb.Free;
  end;

  g.ResetClip;
  rgn.Free;

  f.Free;
  sf.Free;
  pth.Free;
end;

procedure TAdvSmoothCircularProgress.DrawDigits(g: TGPGraphics);
var
  str: String;
  I: Integer;
  r: TGPRectF;
  pt: TGPRectF;
  cnt: Integer;
  x: Single;
begin
  if (not Digits.Visible) or (FDigitsRect.Width = 0) or (FDigitsRect.Height = 0) then
    Exit;

  str := GetPositionString;
  if str <> '' then
  begin
    cnt := Length(str);
    x := FDigitsRect.X;
    for I := 1 to cnt do
    begin
      pt := GetDigitSize(g, str[I]);
      r := MakeRect(x, FDigitsRect.Y, pt.Width, FDigitsRect.Height);
      DrawDigit(g, r, str[I]);
      x := x + pt.Width;
    end;
  end;
end;

procedure TAdvSmoothCircularProgress.DrawInnerCircle(g: TGPGraphics);
var
  r: TGPRectF;
begin
  if (FInnerRect.Width = 0) or (FInnerRect.Height = 0) then
    Exit;

  r := MakeRect(FInnerRect.X, FInnerRect.Y, FInnerRect.Width - 1, FInnerRect.Height - 1);
  
  InnerCircle.Fill.Fill(g, r);
end;

procedure TAdvSmoothCircularProgress.DrawInnerCircleShadow(g: TGPGraphics);
var
  r, rShad: TGPRectF;
  pth: TGPGraphicsPath;
  rd: TGPPathGradientBrush;
  cs : array[0..2] of TGPColor;
  pb: array[0..2] of single;
  cCount: integer;
  w, h: Single;
const
  offset: Integer = 5;
begin
  if (FInnerRect.Width = 0) or (FInnerRect.Height = 0) then
    Exit;

  r := MakeRect(FInnerRect.X, FInnerRect.Y, FInnerRect.Width - 1, FInnerRect.Height - 1);
  
  if InnerCircle.Shadow then
  begin
    w := r.Width / 100 * offset;
    h := r.Height / 100 * offset;
    rShad := MakeRect(r.X - w, r.Y - h, r.Width + w * 2, r.Height + h * 2);
    pth := CreateRoundRectangle(rShad, InnerCircle.Fill.Rounding, InnerCircle.Fill.RoundingType, False);
    
    rd := TGPPathGradientBrush.Create(pth);
    rd.SetWrapMode(WrapModeClamp);
    cs[0] := MakeColor(0, InnerCircle.ShadowColor);
    cs[1] := MakeColor(50, InnerCircle.ShadowColor);
    cs[2] := MakeColor(0, InnerCircle.ShadowColor);
    
    pb[0] := 0;
    pb[1] := 0.1;    
    pb[2] := 1;
    cCount := 3;        
    rd.SetInterpolationColors(@cs, @pb, cCount);
    g.FillPath(rd, pth);
    rd.Free;
    pth.Free;  
  end;
end;

procedure TAdvSmoothCircularProgress.DrawOuterCircle(g: TGPGraphics);
var
  r: TGPRectF;
begin
  if (FOuterRect.Width = 0) or (FOuterRect.Height = 0) then
    Exit;

  r := MakeRect(FOuterRect.X, FOuterRect.Y, FOuterRect.Width - 1, FOuterRect.Height - 1);
  OuterCircle.Fill.Fill(g, r); 
end;

procedure TAdvSmoothCircularProgress.DrawOuterCircleShadow(g: TGPGraphics);
var
  pth: TGPGraphicsPath;
  rShad: TGPRectF;
  rd: TGPPathGradientBrush;
  cs: array[0..2] of TGPColor;
  pb: array[0..2] of Single;
  cCount: Integer;  
  r: TGPRectF;
begin  
  if (FProgressRect.Width = 0) or (FProgressRect.Height = 0) or (FOuterRect.Width = 0) or (FOuterRect.Height = 0) then
    Exit;
      
  if OuterCircle.Shadow then
  begin
    r := FProgressRect;
    rShad := MakeRect(r.X, r.Y, r.Width, r.Height);
    pth := CreateRoundRectangle(rShad, ProgressCircle.Fill.Rounding, ProgressCircle.Fill.RoundingType, False);
    
    rd := TGPPathGradientBrush.Create(pth);
    rd.SetWrapMode(WrapModeClamp);
    cs[0] := MakeColor(50, OuterCircle.ShadowColor);
    cs[1] := MakeColor(0, OuterCircle.ShadowColor);
    cs[2] := MakeColor(0, OuterCircle.ShadowColor);
    
    pb[0] := 0;    
    pb[1] := 0.08;    
    pb[2] := 1;
    cCount := 3;        
    rd.SetInterpolationColors(@cs, @pb, cCount);
    g.FillPath(rd, pth);
    rd.Free;
    pth.Free;  
  end; 
end;

procedure TAdvSmoothCircularProgress.DrawProgress(g: TGPGraphics);
var
  r: TGPRectF;
begin
  if (FProgressRect.Width = 0) or (FProgressRect.Height = 0) then
    Exit;

  r := MakeRect(FProgressRect.X, FProgressRect.Y, FProgressRect.Width - 1, FProgressRect.Height - 1);
  ProgressCircle.Fill.Fill(g, r);  
end;

procedure TAdvSmoothCircularProgress.DrawProgressBackGround(g: TGPGraphics);
var
  r: TGPRectF;
begin
  if (FProgressRect.Width = 0) or (FProgressRect.Height = 0) then
    Exit;

  r := MakeRect(FProgressRect.X, FProgressRect.Y, FProgressRect.Width - 1, FProgressRect.Height - 1);
  ProgressCircle.BackGroundFill.Fill(g, r);
end;

procedure TAdvSmoothCircularProgress.DrawSteps(g: TGPGraphics);
var
  st, p, a, ac: Single;
  ex, ey, rdinx, rdpx, rdiny, rdpy: Single;
  c, cto: TGPPointF;
  pen: TGPPen;
  str: String;
  b: TGPSolidBrush;
  m: TGPMatrix;
  ft: TGPFont;
  sizer: TGPRectF;
  min, max, f: Single;
  i, k: Integer;
begin
  if (not ProgressCircle.StepsVisible) or (Step = 0) or (FProgressRect.Width = 0) or (FProgressRect.Height = 0) or (FInnerRect.Width = 0) or (FInnerRect.Height = 0) then
    Exit;

  pen := TGPPEn.Create(MakeColor(200, ProgressCircle.StepLineColor), ProgressCircle.StepLineWidth);
  ft := g.MakeFont(ProgressCircle.StepFont);
  b := TGPSolidBrush.Create(MakeColor(255, ProgressCircle.StepFont.Color));

  min := Minimum;
  max := Maximum;

  st := (max - min) / Step;

  i := 0;
  while Frac(st) > 0 do
  begin
    st := st * 10;
    Inc(i);
  end;

  k := i;
  while k > 0 do
  begin
    min := min * 10;
    max := max * 10;
    Dec(k);
  end;

  p := min;

  rdinx := (FInnerRect.Width / 2) + 1;
  rdiny := (FInnerRect.Height / 2) + 1;
  rdpx := (FProgressRect.Width / 2) - 1;
  rdpy := (FProgressRect.Height / 2) - 1;

  
  while p <= max do
  begin
    a := -90 + (360 / (max - min) * (p + min));
    ac := DegToRad(a);
    ex := rdinx * Cos(ac);
    ey := rdiny * Sin(ac);
    c := MakePoint(FInnerRect.X + (FInnerRect.Width / 2) + ex, FInnerRect.Y + (FInnerRect.Height / 2) + ey);

    ex := rdpx * Cos(ac);
    ey := rdpy * Sin(ac);
    cto := MakePoint(FProgressRect.X + (FProgressRect.Width / 2) + ex, FProgressRect.Y + (FProgressRect.Height / 2) + ey);

    k := i;
    f := p;
    while k > 0 do
    begin
      f := f / 10;
      Dec(k);
    end;
    str := FormatFloat(ProgressCircle.StepFormat, f);

    if Assigned(OnGetStepValue) then
      OnGetStepValue(Self, p, str);

    g.MeasureString(str, Length(str), ft, MakeRect(0, 0, 10000, 10000), sizer);

    if p = (max + min) / 2 then
    begin
      g.DrawLine(pen, c.X, c.Y, cto.X, c.Y+ (cto.Y - c.Y) /2 - sizer.Width /2);
      g.DrawLine(pen, c.X, cto.Y, cto.X, cto.Y - (cto.Y - c.Y) / 2 + sizer.Width /2)
    end
    else
      g.DrawLine(pen, c.X, c.Y, cto.X, cto.Y);

    m := TGPMatrix.Create;
    m.Translate((c.X + (cto.X - c.X) / 2), (c.Y + (cto.Y - c.Y) / 2));
    m.Rotate(a);
    if p >= (max + min) / 2 then
      m.Scale(-1, -1);
    g.SetTransform(m);
    if p = (max + min) / 2 then
      g.DrawString(str, Length(str), ft, MakePoint(-sizer.Width / 2, -sizer.Height / 2), b)
    else
      g.DrawString(str, Length(str), ft, MakePoint(-sizer.Width / 2, 0.0), b);

    g.ResetTransform;
    m.Free;
    p := p + st;
  end;

  ft.Free;
  b.Free;
  pen.Free;
end;

procedure TAdvSmoothCircularProgress.EndUpdate;
begin
  Dec(FUpdateCount);
  if FUpdateCount = 0 then
    Changed;
end;

function TAdvSmoothCircularProgress.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothCircularProgress.GetDigitSize(AGraphics: TGPGraphics; ADigit: String): TGPRectF;
var
  g: TGPGraphics;
  bmp: TBitmap;
  ft: TGPFont;
  sf: TGPStringFormat;
  sizer: TGPRectF;
begin
  bmp := nil;
  if AGraphics = nil then
  begin
    bmp := TBitmap.Create;
    g := TGPGraphics.Create(bmp.Canvas.Handle);
  end
  else
    g := AGraphics;

  ft := g.MakeFont(Digits.Font);
  sf := TGPStringFormat.Create;
  sf.SetAlignment(StringAlignmentCenter);
  sf.SetLineAlignment(StringAlignmentCenter);
  g.MeasureString(ADigit, Length(ADigit), ft, MakeRect(0, 0, 10000, 10000), sf, sizer);
  sizer.Width := sizer.Width * 0.55;
  sizer.Height := sizer.Height * 0.55;
  Result := sizer;
  sf.Free;
  ft.Free;

  if AGraphics = nil then
  begin
    g.Free;
    bmp.Free;
  end;
end;

function TAdvSmoothCircularProgress.GetFontStyle(
  AFontStyles: TFontStyles): Integer;
begin
  Result := 0;
  if (fsBold in AFontStyles) then
    Result := Result + 1;
  if (fsItalic in AFontStyles) then
    Result := Result + 2;
  if (fsUnderline in AFontStyles) then
    Result := Result + 4;
  if (fsStrikeOut in AFontStyles) then
    Result := Result + 8;
end;

function TAdvSmoothCircularProgress.GetNextPosition: Single;
begin
  Result := FPositionTo;
end;

function TAdvSmoothCircularProgress.GetPosition: Single;
begin
  Result := FPosition;
end;

function TAdvSmoothCircularProgress.GetPositionString: String;
begin
  Result := FormatFloat(Digits.Format, Position);
  if Assigned(OnGetPositionValue) then
    OnGetPositionValue(Self, Position, Result);
end;

function TAdvSmoothCircularProgress.GetPositionStringSize: TGPRectF;
var
  str: String;
  I: Integer;
  sz: TGPRectF;
begin
  Result := MakeRect(0, 0, 0, 0);
  str := GetPositionString;
  for I := 1 to Length(str) do
  begin
    sz := GetDigitSize(nil, str[I]);
    Result.Width := Result.Width + sz.Width;
    if sz.Height > Result.Height then
      Result.Height := sz.Height;
  end;
end;

procedure TAdvSmoothCircularProgress.GoToEnd;
begin
  Position := Maximum;
end;

procedure TAdvSmoothCircularProgress.GoToValue(AValue: Single);
begin
  Position := AValue;
end;

procedure TAdvSmoothCircularProgress.InnerCircleChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothCircularProgress.IsProgressAnimation: Boolean;
begin
  Result := false;
  if not (csDesigning in ComponentState) and not (csLoading in ComponentState) then
    Result := AnimationFactor > 0;
end;

procedure TAdvSmoothCircularProgress.MarqueeProgress(Sender: TObject);
var
  st: Single;
  s: Single;
begin
  if (Step > 0) and not (csDesigning in ComponentState) and not (csDestroying in ComponentState) then
  begin

    st := (Maximum - Minimum) / Step;

    s := 1;
    while Frac(st) > 0 do
    begin
      s := s / 10;
      st := st * 10;
    end;

    FMarqueePos := FMarqueePos + s;
    if FMarqueePos > Maximum then
       FMarqueePos := Minimum;
       
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Interaction then
    Exit;
  FMouseDown := True;
end;

procedure TAdvSmoothCircularProgress.MouseMove(Shift: TShiftState; X,
  Y: Integer);
begin
  inherited;
  if not Interaction then
    Exit;

  if FMouseDown then
    Position := XYToValue(X, Y);
end;

procedure TAdvSmoothCircularProgress.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
  if not Interaction then
    Exit;

  FMouseDown := False;
  Position := XYToValue(X, Y);
end;

procedure TAdvSmoothCircularProgress.GoToStart;
begin
  Position := Minimum;
end;

procedure TAdvSmoothCircularProgress.Next(AStep: Single);
begin
  Position := NextPosition + AStep;
end;

procedure TAdvSmoothCircularProgress.Next;
begin
  Position := NextPosition + Step;
end;

procedure TAdvSmoothCircularProgress.OuterCircleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgress.Paint;
var
  g: TGPGraphics;
  rgn: TGPRegion;
  pth, pthex: TGPGraphicsPath;
  p: Single;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  g.SetTextRenderingHint(TextRenderingHintAntiAlias);
  DrawOuterCircle(g);

  DrawProgressBackGround(g);

  if ((Position > Minimum) or (Style = pbstMarquee)) and (FProgressRect.Width > 0) and (FProgressRect.Height > 0) and (FInnerRect.Width > 0)
    and (FInnerRect.Height > 0) then
  begin
    pthex := TGPGraphicsPath.Create;
    pth := CreateRoundRectangle(FProgressRect, ProgressCircle.Fill.Rounding, ProgressCircle.Fill.RoundingType, False);
    case Style of
      pbstNormal:
      begin
        p := 360 - (360 / Abs(Maximum - Minimum) * (Position - Minimum));
        pthex.AddPie(FOuterRect, -90, -p);
      end;
      pbstMarquee:
      begin
        p := 360 - (360 / Abs(Maximum - Minimum) * FMarqueePos);
        pthex.AddPie(FOuterRect, -90-p, 360 -(360 / Step));
      end;
    end;
    rgn := TGPRegion.Create(pth);
    rgn.Exclude(pthex);
    g.SetClip(rgn);
    DrawProgress(g);
    g.ResetClip;
    rgn.Free;
    pth.Free;
    pthex.Free;
  end;
  DrawSteps(g);
  DrawOuterCircleShadow(g);
  DrawInnerCircleShadow(g);
  DrawInnerCircle(g);
  DrawDigits(g);
  g.Free;
end;

procedure TAdvSmoothCircularProgress.Previous(AStep: Single);
begin
  Position := NextPosition - AStep;
end;

procedure TAdvSmoothCircularProgress.Previous;
begin
  Position := NextPosition - Step;
end;

procedure TAdvSmoothCircularProgress.ProgressCircleChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgress.Resize;
begin
  inherited;
  UpdateCircularProgress;
end;

procedure TAdvSmoothCircularProgress.SetAnimationFactor(const Value: Single);
begin
  if FAnimationFactor <> Value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetAspectRatio(const Value: Boolean);
begin
  if FAspectRatio <> Value then
  begin
    FAspectRatio := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetAutoRounding(const Value: Boolean);
begin
  if FAutoRounding <> Value then
  begin
    FAutoRounding := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetColorTones(ATones: TColorTones);
begin
  OuterCircle.Shadow := False;
  InnerCircle.Shadow := False;
  Digits.Shadow := False;
  Digits.Overlay := False;

  OuterCircle.Fill.Color := ATones.Background.BrushColor;
  OuterCircle.Fill.ColorTo := ATones.Background.BrushColor;
  OuterCircle.Fill.ColorMirror := ATones.Background.BrushColor;
  OuterCircle.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  OuterCircle.Fill.BorderColor := ATones.Background.BorderColor;

  ProgressCircle.BackGroundFill.Color := ATones.Hover.BrushColor;
  ProgressCircle.BackGroundFill.ColorTo := ATones.Hover.BrushColor;
  ProgressCircle.BackGroundFill.ColorMirror := ATones.Hover.BrushColor;
  ProgressCircle.BackGroundFill.ColorMirrorTo := ATones.Hover.BrushColor;
  ProgressCircle.BackGroundFill.BorderColor := ATones.Hover.BorderColor;

  ProgressCircle.Fill.Color := ATones.Selected.BrushColor;
  ProgressCircle.Fill.ColorTo := ATones.Selected.BrushColor;
  ProgressCircle.Fill.ColorMirror := ATones.Selected.BrushColor;
  ProgressCircle.Fill.ColorMirrorTo := ATones.Selected.BrushColor;
  ProgressCircle.Fill.BorderColor := ATones.Selected.BorderColor;

  ProgressCircle.StepFont.Color := ATones.Background.BrushColor;

  InnerCircle.Fill.Color := ATones.Background.BrushColor;
  InnerCircle.Fill.ColorTo := ATones.Background.BrushColor;
  InnerCircle.Fill.ColorMirror := ATones.Background.BrushColor;
  InnerCircle.Fill.ColorMirrorTo := ATones.Background.BrushColor;
  InnerCircle.Fill.BorderColor := ATones.Background.BorderColor;

  ProgressCircle.StepLineColor := ATones.Hover.TextColor;

  Digits.BorderColor := ATones.Hover.BrushColor;
  Digits.Font.Color := ATones.Selected.BrushColor;

end;

procedure TAdvSmoothCircularProgress.SetComponentStyle(AStyle: TTMSStyle);
var
  Selected: Boolean;
begin
  FTMSStyle := AStyle;
  Selected := False;
  OuterCircle.Shadow := True;
  InnerCircle.Shadow := True;
  Digits.Shadow := True;
  Digits.Overlay := True;

  case AStyle of
    tsOffice2003Blue:
    begin
      OuterCircle.Fill.Color := $00FFD2AF;
      OuterCircle.Fill.ColorTo := $00FFD2AF;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $FCE1CB;
        ProgressCircle.Fill.ColorTo := $E0A57D;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $962D00;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $94E6FB;
        ProgressCircle.Fill.ColorTo := $1595EE;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $962D00;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Silver:
    begin
      OuterCircle.Fill.Color := $00E6D8D8;
      OuterCircle.Fill.ColorTo := $00E6D8D8;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $ECE2E1;
        ProgressCircle.Fill.ColorTo := $B39698;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $947C7C;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $94E6FB;
        ProgressCircle.Fill.ColorTo := $1595EE;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $947C7C;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Olive:
    begin
      OuterCircle.Fill.Color := $CFF0EA;
      OuterCircle.Fill.ColorTo := $CFF0EA;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $CFF0EA;
        ProgressCircle.Fill.ColorTo := $8CC0B1;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $588060;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $94E6FB;
        ProgressCircle.Fill.ColorTo := $1595EE;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $588060;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2003Classic:
    begin
      OuterCircle.Fill.Color := $00F2F2F2;
      OuterCircle.Fill.ColorTo := $00F2F2F2;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := clWhite;
        ProgressCircle.Fill.ColorTo := $C9D1D5;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $808080;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $B59285;
        ProgressCircle.Fill.ColorTo := $B59285;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $962D00;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2007Luna:
    begin
      OuterCircle.Fill.Color := $00FFD2AF;
      OuterCircle.Fill.ColorTo := $00FFD2AF;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $FFEFE3;
        ProgressCircle.Fill.ColorTo := $FFDDC4;
        ProgressCircle.Fill.ColorMirror := $FFD1AD;
        ProgressCircle.Fill.ColorMirrorTo := $FFDBC0;
        ProgressCircle.Fill.BorderColor := $FFD1AD;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $AAD9FF;
        ProgressCircle.Fill.ColorTo := $6EBBFF;
        ProgressCircle.Fill.ColorMirror := $42AEFE;
        ProgressCircle.Fill.ColorMirrorTo := $7AE1FE;
        ProgressCircle.Fill.BorderColor := $FFD1AD;//$42AEFE;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsOffice2007Obsidian:
    begin
      OuterCircle.Fill.Color := $5C534C;
      OuterCircle.Fill.ColorTo := $5C534C;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $F9F8F8;
        ProgressCircle.Fill.ColorTo := $E4E2DF;
        ProgressCircle.Fill.ColorMirror := $D1CBC7;
        ProgressCircle.Fill.ColorMirrorTo := $E2DEDB;
        ProgressCircle.Fill.BorderColor := clBlack;//$D1CBC7;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $AAD9FF;
        ProgressCircle.Fill.ColorTo := $6EBBFF;
        ProgressCircle.Fill.ColorMirror := $42AEFE;
        ProgressCircle.Fill.ColorMirrorTo := $7AE1FE;
        ProgressCircle.Fill.BorderColor := clBlack;//$42AEFE;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWindowsXP:
    begin
      OuterCircle.Fill.Color := $00B6B6B6;
      OuterCircle.Fill.ColorTo := $00B6B6B6;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := clWhite;
        ProgressCircle.Fill.ColorTo := clBtnFace;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := clBlack;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := clInActiveCaption;
        ProgressCircle.Fill.ColorTo := clInActiveCaption;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := clBlack;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWhidbey:
    begin
      OuterCircle.Fill.Color := $F5F9FA;
      OuterCircle.Fill.ColorTo := $F5F9FA;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $F5F9FA;
        ProgressCircle.Fill.ColorTo := $A8C0C0;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $962D00;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $94E6FB;
        ProgressCircle.Fill.ColorTo := $1595EE;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $962D00;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsCustom: ;
    tsOffice2007Silver:
    begin
      OuterCircle.Fill.Color := $00CAC1BA;
      OuterCircle.Fill.ColorTo := $00CAC1BA;
      OuterCircle.Fill.BorderColor := $00C0C0C0;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $FAEEEB;
        ProgressCircle.Fill.ColorTo := $E5DBD7;
        ProgressCircle.Fill.ColorMirror := $E2D8D4;
        ProgressCircle.Fill.ColorMirrorTo := $D1C7C5;
        ProgressCircle.Fill.BorderColor := clBlack;//$E2D8D4;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $AAD9FF;
        ProgressCircle.Fill.ColorTo := $6EBBFF;
        ProgressCircle.Fill.ColorMirror := $42AEFE;
        ProgressCircle.Fill.ColorMirrorTo := $7AE1FE;
        ProgressCircle.Fill.BorderColor := clBlack;//$42AEFE;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWindowsVista:
    begin
      OuterCircle.Fill.Color := $FDF8F1;
      OuterCircle.Fill.ColorTo := $FDF8F1;
      OuterCircle.Fill.BorderColor := $FDDE99;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $FDF8F1;
        ProgressCircle.Fill.ColorTo := $FCEFD5;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $FDDE99;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $FEF9F0;
        ProgressCircle.Fill.ColorTo := $FDF0D7;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $FEDF9A;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsWindows7:
    begin
      OuterCircle.Fill.Color := $FDF8F1;
      OuterCircle.Fill.ColorTo := $FDF8F1;
      OuterCircle.Fill.BorderColor := $FDDE99;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := $FDFBFA;
        ProgressCircle.Fill.ColorTo := $FDF3EB;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $FBD6B8;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end
      else
      begin
        ProgressCircle.Fill.Color := $FCEBDC;
        ProgressCircle.Fill.ColorTo := $FCDBC1;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := $CEA27D;
        ProgressCircle.Fill.GradientMirrorType := gtVertical;
      end;
    end;
    tsTerminal:
    begin
      OuterCircle.Fill.Color := clBtnFace;
      OuterCircle.Fill.ColorTo := clBtnFace;
      OuterCircle.Fill.BorderColor := clGray;

      if not Selected then
      begin
        ProgressCircle.Fill.Color := clSilver;
        ProgressCircle.Fill.ColorTo := clSilver;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := clGray;

      end
      else
      begin
        ProgressCircle.Fill.Color := clWhite;
        ProgressCircle.Fill.ColorTo := clWhite;
        ProgressCircle.Fill.ColorMirror := clNone;
        ProgressCircle.Fill.ColorMirrorTo := clNone;
        ProgressCircle.Fill.BorderColor := clGray;

      end;
    end;
    tsOffice2010Blue:
    begin
      OuterCircle.Fill.Color := $FDF6EF;
      OuterCircle.Fill.ColorTo := $F0DAC7;
      OuterCircle.Fill.BorderColor := $C7B29F;

      ProgressCircle.Fill.Color := $EDDBCD;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := $5B391E;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;

    end;
    tsOffice2010Silver:
    begin
      OuterCircle.Fill.Color := $FFFFFF;
      OuterCircle.Fill.ColorTo := $EDE5E0;
      OuterCircle.Fill.BorderColor := $D2CDC8;

      ProgressCircle.Fill.Color := $EDE9E5;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := $7C6D66;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;

    end;
    tsOffice2010Black:
    begin
      OuterCircle.Fill.Color := $BFBFBF;
      OuterCircle.Fill.ColorTo := $919191;
      OuterCircle.Fill.BorderColor := $D7D7D6;

      ProgressCircle.Fill.Color := $828282;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := $6D6D6D;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;

    end;
  tsWindows8, tsWindows10:
    begin

      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := $F7F6F5;
      OuterCircle.Fill.ColorTo := $F7F6F5;
      OuterCircle.Fill.BorderColor := $E4E3E2;

      ProgressCircle.Fill.Color := $F7E0C9;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E4A262;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;
  tsOffice2013White:
    begin
      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := clWhite;
      OuterCircle.Fill.ColorTo := clWhite;
      OuterCircle.Fill.BorderColor := $D4D4D4;

      ProgressCircle.Fill.Color := $FCE2C8;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E59D56;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;
  tsOffice2013LightGray:
    begin
      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := $F6F6F6;
      OuterCircle.Fill.ColorTo := $F6F6F6;
      OuterCircle.Fill.BorderColor := $C6C6C6;

      ProgressCircle.Fill.Color := $FCE2C8;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E59D56;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;

  tsOffice2013Gray:
    begin
      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := $E5E5E5;
      OuterCircle.Fill.ColorTo := $E5E5E5;
      OuterCircle.Fill.BorderColor := $ABABAB;

      ProgressCircle.Fill.Color := $FCE2C8;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E59D56;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;
 tsOffice2016White:
    begin
      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := clWhite;
      OuterCircle.Fill.ColorTo := clWhite;
      OuterCircle.Fill.BorderColor := $D4D4D4;

      ProgressCircle.Fill.Color := $E3BDA3;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E3BDA3;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;
  tsOffice2016Gray:
    begin
      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := $B2B2B2;
      OuterCircle.Fill.ColorTo := $B2B2B2;
      OuterCircle.Fill.BorderColor := $444444;

      ProgressCircle.Fill.Color := $E3BDA3;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$E3BDA3;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;

  tsOffice2016Black:
    begin

     //ProgressCircle.StepFont.Color := clwhite;
      //Digits.Font.Color := clwhite;

      OuterCircle.Shadow := False;
      InnerCircle.Shadow := False;
      Digits.Shadow := False;

      OuterCircle.Fill.Color := $363636;
      OuterCircle.Fill.ColorTo := $363636;
      OuterCircle.Fill.BorderColor := $444444;

      ProgressCircle.Fill.Color := $6A6A6A;
      ProgressCircle.Fill.ColorTo := clNone;
      ProgressCircle.Fill.ColorMirror := clNone;
      ProgressCircle.Fill.ColorMirrorTo := clNone;
      ProgressCircle.Fill.BorderColor := clNone; //$444444;
      ProgressCircle.Fill.GradientMirrorType := gtVertical;
    end;
  end;

  ProgressCircle.Fill.BorderColor := clNone;
end;

procedure TAdvSmoothCircularProgress.SetDigits(
  const Value: TAdvSmoothCircularProgressDigits);
begin
  if FDigits <> Value then
  begin
    FDigits.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetStep(const Value: Single);
begin
  if FStep <> Value then
  begin
    FStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetInnerCircle(
  const Value: TAdvSmoothCircularProgressInnerCircle);
begin
  if FInnerCircle <> Value then
  begin
    FInnerCircle.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetInteraction(const Value: Boolean);
begin
  if FInteraction <> Value then
  begin
    FInteraction := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetMarqueeInterval(const Value: Integer);
begin
  if FMarqueeInterval <> Value then
  begin
    FMarqueeInterval := Value;
    if Assigned(FMarqueeTimer) then
      FMarqueeTimer.Interval := FMarqueeInterval;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetMaximum(const Value: Single);
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

procedure TAdvSmoothCircularProgress.SetMinimum(const Value: Single);
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

procedure TAdvSmoothCircularProgress.SetOuterCircle(
  const Value: TAdvSmoothCircularProgressOuterCircle);
begin
  if FOuterCircle <> Value then
  begin
    FOuterCircle.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.SetPBStyle(const Value: TProgressBarStyle);
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

procedure TAdvSmoothCircularProgress.SetPosition(const Value: Single);
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

procedure TAdvSmoothCircularProgress.SetProgressCircle(
  const Value: TAdvSmoothCircularProgressCircle);
begin
  if FProgressCircle <> Value then
  begin
    FProgressCircle.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgress.WMPaint(var Message: TWMPaint);
var
  DC, MemDC: HDC;
  MemBitmap, OldBitmap: HBITMAP;
  PS: TPaintStruct;
{$IFNDEF DELPHI_UNICODE}
  dbl: Boolean;
{$ENDIF}
  p: TPoint;
  I: Integer;
begin
  if Assigned(Parent) { and (Fill.ShadowOffset > 0) ? } then
  begin
    DC := Message.DC;
    if DC <> 0 then
    begin
  {$IFNDEF DELPHI_UNICODE}
      dbl := Parent.DoubleBuffered;
      Parent.DoubleBuffered := false;
  {$ENDIF}
      I := SaveDC(DC);
      p := ClientOrigin;
      Windows.ScreenToClient(Parent.Handle, p);
      p.X := -p.X;
      p.Y := -p.Y;
      MoveWindowOrg(DC, p.X, p.Y);
      SendMessage(Parent.Handle, WM_ERASEBKGND, DC, 0);
      SendMessage(Parent.Handle, WM_PAINT, DC, 0);
      if (Parent is TWinCtrl) then (Parent as TWinCtrl)
        .PaintCtrls(DC, nil);
      RestoreDC(DC, I);
  {$IFNDEF DELPHI_UNICODE}
      Parent.DoubleBuffered := dbl;
  {$ENDIF}
    end;
  end;

  if not FDoubleBuffered or (Message.DC <> 0) then
  begin
    if not(csCustomPaint in ControlState) and (ControlCount = 0) then
      inherited
    else
      PaintHandler(Message);
  end
  else
  begin
    DC := GetDC(0);
    MemBitmap := CreateCompatibleBitmap
      (DC, ClientRect.Right, ClientRect.Bottom);
    ReleaseDC(0, DC);
    MemDC := CreateCompatibleDC(0);
    OldBitmap := SelectObject(MemDC, MemBitmap);
    try
      DC := BeginPaint(Handle, PS);
      Perform(WM_ERASEBKGND, MemDC, MemDC);
      Message.DC := MemDC;
      WMPaint(Message);
      Message.DC := 0;
      BitBlt(DC, 0, 0, ClientRect.Right, ClientRect.Bottom, MemDC, 0, 0,
        SRCCOPY);
      EndPaint(Handle, PS);
    finally
      SelectObject(MemDC, OldBitmap);
      DeleteDC(MemDC);
      DeleteObject(MemBitmap);
    end;
  end;
end;

function TAdvSmoothCircularProgress.XYToValue(X, Y: Integer): Single;
var
  cx, cy, res: Single;
begin
  Result := 0;
  if (Step = 0) or (FProgressRect.Width = 0) or (FProgressRect.Height = 0) or (FInnerRect.Width = 0) or (FInnerRect.Height = 0) then
    Exit;
  cx := FInnerRect.X + FInnerRect.Width / 2;
  cy := FInnerRect.Y + FInnerRect.Height / 2;
  res := 90 + RadToDeg(ArcTan2(y - cy, x - cx));
  if res < 0 then
    res := res + 360;
  Result := Minimum +  ((Maximum - Minimum) / 360 * res)
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  inherited PaintControls(DC, First);
end;

{ TAdvSmoothCircularProgressOuterCircle }

procedure TAdvSmoothCircularProgressOuterCircle.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothCircularProgressOuterCircle then
  begin
    FFill.Assign((Source as TAdvSmoothCircularProgressOuterCircle).Fill);
    FSize := (Source as TAdvSmoothCircularProgressOuterCircle).Size;
    FShadow := (Source as TAdvSmoothCircularProgressOuterCircle).Shadow;
  end;
end;

procedure TAdvSmoothCircularProgressOuterCircle.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSmoothCircularProgressOuterCircle.Create(AOwner: TAdvSmoothCircularProgress);
begin
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FSize := 15;
  FShadow := True;
  FShadowColor := clBlack;
end;

destructor TAdvSmoothCircularProgressOuterCircle.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothCircularProgressOuterCircle.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressOuterCircle.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressOuterCircle.SetShadow(const Value: Boolean);
begin
  if FShadow <> Value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressOuterCircle.SetShadowColor(
  const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressOuterCircle.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TAdvSmoothCircularProgressDigits }

procedure TAdvSmoothCircularProgressDigits.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothCircularProgressDigits then
  begin
    FBorderColor := (Source as TAdvSmoothCircularProgressDigits).BorderColor;
    FShadow := (Source as TAdvSmoothCircularProgressDigits).Shadow;
    FFormat := (Source as TAdvSmoothCircularProgressDigits).Format;
    FBorderWidth := (Source as TAdvSmoothCircularProgressDigits).BorderWidth;
    FFont.Assign((Source as TAdvSmoothCircularProgressDigits).Font);
    FShadowColor := (Source as TAdvSmoothCircularProgressDigits).ShadowColor;
    FVisible := (Source as TAdvSmoothCircularProgressDigits).Visible;
    FOverlay := (Source as TAdvSmoothCircularProgressDigits).Overlay;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSmoothCircularProgressDigits.Create(AOwner: TAdvSmoothCircularProgress);
begin
  FOverlay := True;
  FVisible := True;
  FFormat := '0"%"';
  FShadow := True;
  FShadowColor := clBlack;
  FBorderColor := clDkGray;
  FBorderWidth := 1;
  FFont := TFont.Create;
  FFont.Size := 44;
  FFont.Style := [fsBold];
  FFont.Color := clSilver;
  FFont.Name := 'Segoe UI';
  FFont.OnChange := FontChanged;
end;

destructor TAdvSmoothCircularProgressDigits.Destroy;
begin
  FFont.Free;
  inherited;
end;

procedure TAdvSmoothCircularProgressDigits.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressDigits.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressDigits.SetBorderColor(const Value: TColor);
begin
  if FBorderColor <> Value then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetBorderWidth(const Value: Integer);
begin
  if FBorderWidth <> Value then
  begin
    FBorderWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetFont(const Value: TFont);
begin
  if FFont <> Value then
  begin
    FFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetFormat(const Value: String);
begin
  if FFormat <> Value then
  begin
    FFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetOverlay(const Value: Boolean);
begin
  if FOverlay <> Value then
  begin
    FOverlay := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetShadow(const Value: Boolean);
begin
  if FShadow <> Value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressDigits.SetVisible(const Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

{ TAdvSmoothCircularProgressInnerCircle }

procedure TAdvSmoothCircularProgressInnerCircle.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothCircularProgressInnerCircle then
  begin
    FFill.Assign((Source as TAdvSmoothCircularProgressInnerCircle).Fill);
    FSize := (Source as TAdvSmoothCircularProgressInnerCircle).Size;
    FShadow := (Source as TAdvSmoothCircularProgressOuterCircle).Shadow;
    FShadowColor := (Source as TAdvSmoothCircularProgressOuterCircle).ShadowColor;
  end;
end;

procedure TAdvSmoothCircularProgressInnerCircle.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSmoothCircularProgressInnerCircle.Create(AOwner: TAdvSmoothCircularProgress);
begin
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FSize := 60;
  FShadow := True;
  FShadowColor := clBlack;
end;

destructor TAdvSmoothCircularProgressInnerCircle.Destroy;
begin
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothCircularProgressInnerCircle.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressInnerCircle.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressInnerCircle.SetShadow(const Value: Boolean);
begin
  if FShadow <> Value then
  begin
    FShadow := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressInnerCircle.SetShadowColor(
  const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressInnerCircle.SetSize(const Value: Integer);
begin
  if FSize <> Value then
  begin
    FSize := Value;
    Changed;
  end;
end;

{ TAdvSmoothCircularProgressCircle }

procedure TAdvSmoothCircularProgressCircle.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothCircularProgressCircle then
  begin
    FFill.Assign((Source as TAdvSmoothCircularProgressCircle).Fill);
    FStepFont.Assign((Source as TAdvSmoothCircularProgressCircle).StepFont);
    FStepLineColor := (Source as TAdvSmoothCircularProgressCircle).StepLineColor;
    FStepLineWidth := (Source as TAdvSmoothCircularProgressCircle).StepLineWidth;
    FBackGroundFill.Assign((Source as TAdvSmoothCircularProgressCircle).BackGroundFill);
    FStepFormat := (Source as TAdvSmoothCircularProgressCircle).StepFormat;
    FStepsVisible := (Source as TAdvSmoothCircularProgressCircle).StepsVisible;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

constructor TAdvSmoothCircularProgressCircle.Create(AOwner: TAdvSmoothCircularProgress);
begin
  FStepsVisible := True;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FBackGroundFill := TGDIPFill.Create;
  FBackGroundFill.OnChange := FillChanged;
  FStepLineColor := clGray;
  FStepLineWidth := 1;
  FStepFormat := '0';
  FStepFont := TFont.Create;
  FStepFont.Name := 'Segoe UI';
  FStepFont.OnChange := FontChanged;
end;

destructor TAdvSmoothCircularProgressCircle.Destroy;
begin
  FStepFont.Free;
  FBackGroundFill.Free;
  FFill.Free;
  inherited;
end;

procedure TAdvSmoothCircularProgressCircle.FillChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressCircle.FontChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothCircularProgressCircle.SetBackGroundFill(
  const Value: TGDIPFill);
begin
  if FBackGroundFill <> Value then
  begin
    FBackGroundFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetFill(const Value: TGDIPFill);
begin
  if FFill <> Value then
  begin
    FFill.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetStepFont(const Value: TFont);
begin
  if FStepFont <> Value then
  begin
    FStepFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetStepFormat(const Value: string);
begin
  if FStepFormat <> Value then
  begin
    FStepFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetStepLineColor(
  const Value: TColor);
begin
  if FStepLineColor <> Value then
  begin
    FStepLineColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetStepLineWidth(
  const Value: Integer);
begin
  if FStepLineWidth <> Value then
  begin
    FStepLineWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothCircularProgressCircle.SetStepsVisible(
  const Value: Boolean);
begin
  if FStepsVisible <> Value then
  begin
    FStepsVisible := Value;
    Changed;
  end;
end;

end.

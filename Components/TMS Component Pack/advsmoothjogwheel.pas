{*************************************************************************}
{ TAdvSmoothJogWheel component                                            }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright ©  2012                                             }
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

unit AdvSmoothJogWheel;

interface

{$I TMSDEFS.INC}

uses
  Messages, Controls, Graphics, ExtCtrls, Windows, Classes,
  Math, SysUtils, ImgList,
  AdvGDIP, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 2; // Release nr.
  BLD_VER = 5; // Build nr.

  // version history
  // v1.0.0.0 : first release
  // v1.1.0.0 : New: Added properties MinimumValue and MaximumValue to limit Continuous mode
  //          : New: Added IndicatorValue property to change the indicator start value
  // v1.1.1.0 : New : SingleClickStep MouseWheelStep
  //          : New : Display value on Jogwheel
  //          : Fixed : issue with saving width and height
  // v1.1.1.1 : Fixed : Issue with transparency with rounded corners
  // v1.1.1.2 : Fixed : Issue with calculation of minimum and maximum values
  // v1.1.1.3 : Fixed : Issue with limit of mouse wheel scrolling
  // v1.1.2.0 : New : Delphi 2010 Touch Support
  // v1.1.2.1 : Fixed : Issue with direction of jogwheel
  // v1.1.2.2 : Fixed : Issue with Minimum and maximum of jogwheel
  // v1.1.2.3 : Fixed : Issue with vertical direction
  // v1.1.2.4 : Fixed : Issue with calculation of aperture minimum and maximum
  // v1.1.2.5 : Fixed : Issue with keyboard scrolling direction of jogwheel

type
  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothJogWheel = class;

  TAdvSmoothJogWheelDirection = (wdHorizontal, wdVertical);

  TAdvSmoothJogWheelModeType = (wmContinuous, wmCue, wmAutoCue);

  TAdvSmoothJogWheelModes = set of TAdvSmoothJogWheelModeType;

  TAdvSmoothJogWheelValueChanged = procedure(Sender: TObject; Value: Double; CurrentMode: TAdvSmoothJogWheelModeType) of object;

  TAdvSmoothJogWheelModeChanged = procedure(Sender: TObject; Mode: TAdvSmoothJogWheelModeType) of object;

  TAdvSmoothJogWheelIndicatorShape = (wsLine, wsCircle, wsTriangle, wsSquare, wsDiamond, wsNone);

  TAdvSmoothJogWheelIndicatorPosition = (wpCenter, wpTop, wpBottom);

  TAdvSmoothJogWheelLocation = (wlTopLeft, wlTopCenter, wlTopRight, wlCenterLeft, wlCenterCenter, wlCenterRight, wlBottomLeft, wlBottomCenter, wlBottomRight, wlCustom);

  TAdvSmoothJogWheelIndicator = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FOwner: TAdvSmoothJogWheel;
    FPicture: TAdvGDIPPicture;
    FWidth: integer;
    FShape: TAdvSmoothJogWheelIndicatorShape;
    FColor: TColor;
    FVisible: Boolean;
    FSize: integer;
    FPosition: TAdvSmoothJogWheelIndicatorPosition;
    FImageIndex: integer;
    FPictureWidth: integer;
    FPictureHeight: integer;
    FValue: Double;
    procedure SetColor(const Value: TColor);
    procedure SetImageIndex(const Value: integer);
    procedure SetPicture(const Value: TAdvGDIPPicture);
    procedure SetPosition(const Value: TAdvSmoothJogWheelIndicatorPosition);
    procedure SetShape(const Value: TAdvSmoothJogWheelIndicatorShape);
    procedure SetSize(const Value: integer);
    procedure SetVisible(const Value: Boolean);
    procedure SetWidth(const Value: integer);
    procedure SetPictureHeight(const Value: integer);
    procedure SetPictureWidth(const Value: integer);
    procedure SetValue(const Value: Double);
  protected
    procedure Changed;
    procedure PictureChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothJogWheel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Visible: Boolean read FVisible write SetVisible default true;
    property Color: TColor read FColor write SetColor default clRed;
    property ImageIndex: integer read FImageIndex write SetImageIndex default -1;
    property Picture: TAdvGDIPPicture read FPicture write SetPicture;
    property PictureHeight: integer read FPictureHeight write SetPictureHeight default 15;
    property PictureWidth: integer read FPictureWidth write SetPictureWidth default 15;
    property Shape: TAdvSmoothJogWheelIndicatorShape read FShape write SetShape default wsLine;
    property Size: integer read FSize write SetSize default 15;
    property Width: integer read FWidth write SetWidth default 2;
    property Position: TAdvSmoothJogWheelIndicatorPosition read FPosition write SetPosition default wpCenter;
    property Value: Double read FValue write SetValue;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TAdvSmoothJogWheelMode = class(TPersistent)
  private
    FOwner: TAdvSmoothJogWheel;
    FColor: TColor;
    FIndicator: TAdvSmoothJogWheelIndicator;
    FOnChange: TNotifyEvent;
    FBrightness: Double;
    procedure SetColor(const Value: TColor);
    procedure SetIndicator(const Value: TAdvSmoothJogWheelIndicator);
    procedure SetBrightness(const Value: Double);
  protected
    procedure Changed;
    procedure IndicatorChanged(Sender: TObject);
  public
    constructor Create(AOwner: TAdvSmoothJogWheel);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Brightness: Double read FBrightness write SetBrightness;
    property Color: TColor read FColor write SetColor default clBlack;
    property Indicator: TAdvSmoothJogWheelIndicator read FIndicator write SetIndicator;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothJogWheel = class(TCustomControl)
  private
    FLoaded: Boolean;
    FFocused: Boolean;
    FModeCount: integer;
    StartModeTimer: Boolean;
    FCurrentMode: TAdvSmoothJogWheelModeType;
    FAnimationTimer: TTimer;
    FAnimating, FAnimate, FMouseDown, FMouseUp: Boolean;
    FSp: Double;
    FTimeStop, FCurrentScPos, FScPosTo, FTimeStart, FDragXY, FScrollXY,
      FClickX, FClickY: integer;
    FResize: boolean;
    FOldW, FOldH: integer;
    FGripSpacing: Integer;
    FGripSize: integer;
    FDirection: TAdvSmoothJogWheelDirection;
    FValue: Double;
    FStep: Double;
    FModeAutoCue: TAdvSmoothJogWheelMode;
    FModeContinuous: TAdvSmoothJogWheelMode;
    FModeCue: TAdvSmoothJogWheelMode;
    FModes: TAdvSmoothJogWheelModes;
    FOnValueChanged: TAdvSmoothJogWheelValueChanged;
    FApperture: integer;
    FAnimationFactor: integer;
    FImages: TCustomImageList;
    FModeChangeDelay: Double;
    FOnModeChanged: TAdvSmoothJogWheelModeChanged;
    FFocusColor: TColor;
    FEnabled: Boolean;
    FMaximumValue: Double;
    FMinimumValue: Double;
    FSingleClickStep: Double;
    FMouseWheelStep: Double;
    FValueFont: TFont;
    FValueFormat: String;
    FShowValue: Boolean;
    FValueTop: integer;
    FValueLeft: integer;
    FValueLocation: TAdvSmoothJogWheelLocation;
    procedure WMEraseBkgnd(var Message: TWmEraseBkgnd); message WM_ERASEBKGND;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetGripSpacing(const Value: Integer);
    procedure SetGripSize(const Value: integer);
    procedure SetDirection(const Value: TAdvSmoothJogWheelDirection);
    procedure SetStep(const Value: Double);
    procedure SetValue(const Value: Double);
    procedure SetModeAutoCue(const Value: TAdvSmoothJogWheelMode);
    procedure SetModeContinuous(const Value: TAdvSmoothJogWheelMode);
    procedure SetModeCue(const Value: TAdvSmoothJogWheelMode);
    procedure SetModes(const Value: TAdvSmoothJogWheelModes);
    procedure SetApperture(const Value: integer);
    procedure SetCurrentMode(const Value: TAdvSmoothJogWheelModeType);
    procedure SetAnimationFactor(const Value: integer);
    procedure SetModeChangeDelay(const Value: Double);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetFocusColor(const Value: TColor);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure SetMaximumValue(const Value: Double);
    procedure SetMinimumValue(const Value: Double);
    procedure SetSingleClickStep(const Value: Double);
    procedure SetMouseWheelStep(const Value: Double);
    procedure SetShowValue(const Value: Boolean);
    procedure SetValueFont(const Value: TFont);
    procedure SetValueFormat(const Value: String);
    procedure SetValueLeft(const Value: integer);
    procedure SetValueLocation(const Value: TAdvSmoothJogWheelLocation);
    procedure SetValueTop(const Value: integer);
  protected
    procedure SetEnabled(Value: Boolean); override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure Changed;
    procedure FontChanged(Sender: TObject);
    procedure ModeChanged(Sender: TObject);
    procedure DrawBackGround(g: TGPGraphics);
    procedure DrawGloss(g: TGPGraphics);
    procedure DrawOverlay(g: TGPGraphics);
    procedure DrawValue(g: TGPGraphics);
    procedure DrawGrips(g: TGPGraphics);
    procedure DrawIndicator(g: TGPGraphics);
    procedure DrawGrip(g: TGPGraphics; Position: Double);
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoExit; override;
    procedure DoEnter; override;
    procedure Animate(Sender: TObject);
    function IsMode: Boolean;
    function GetColor(Delta: integer): TColor;
    function GetPosition: integer;
    function GetPositionTo: integer;
    function GetVersionNr: integer;
    procedure GetLabelPosition(var x, y: Double; rectangle: TGPRectF; objectwidth, objectheight: Double; location: TAdvSmoothJogWheelLocation);
  public
    constructor Create(AComponent: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure MouseWheelHandler(var Message: TMessage); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure CreateWnd; override;
  published
    property AnimationFactor: integer read FAnimationFactor write SetAnimationFactor default 5;
    property ModeChangeDelay: Double read FModeChangeDelay write SetModeChangeDelay;
    property ShowValue: Boolean read FShowValue write SetShowValue default false;
    property ValueFont: TFont read FValueFont write SetValueFont;
    property ValueFormat: String read FValueFormat write SetValueFormat;
    property ValueLocation: TAdvSmoothJogWheelLocation read FValueLocation write SetValueLocation default wlCenterLeft;
    property ValueLeft: integer read FValueLeft write SetValueLeft default 0;
    property ValueTop: integer read FValueTop write SetValueTop default 0;
    property GripSpacing: Integer read FGripSpacing write SetGripSpacing default 20;
    property GripSize: integer read FGripSize write SetGripSize default 15;
    property Step: Double read FStep write SetStep;
    property SingleClickStep: Double read FSingleClickStep write SetSingleClickStep;
    property MouseWheelStep: Double read FMouseWheelStep write SetMouseWheelStep;
    property Value: Double read FValue write SetValue;
    property Direction: TAdvSmoothJogWheelDirection read FDirection write SetDirection default wdHorizontal;
    property ModeContinuous: TAdvSmoothJogWheelMode read FModeContinuous write SetModeContinuous;
    property ModeCue: TAdvSmoothJogWheelMode read FModeCue write SetModeCue;
    property ModeAutoCue: TAdvSmoothJogWheelMode read FModeAutoCue write SetModeAutoCue;
    property Modes: TAdvSmoothJogWheelModes read FModes write SetModes;
    property MaximumValue: Double read FMaximumValue write SetMaximumValue;
    property MinimumValue: Double read FMinimumValue write SetMinimumValue;
    property Apperture: integer read FApperture write SetApperture default 80;
    property Mode: TAdvSmoothJogWheelModeType read FCurrentMode write SetCurrentMode default wmContinuous;
    property Images: TCustomImageList read FImages write FImages;
    property Version: string read GetVersion write SetVersion;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clWhite;
    property Enabled: Boolean read FEnabled write SetEnabled default true;

    property OnValueChanged: TAdvSmoothJogWheelValueChanged read FOnValueChanged write FOnValueChanged;
    property OnModeChanged: TAdvSmoothJogWheelModeChanged read FOnModeChanged write FOnModeChanged;

    property Align;
    property Anchors;
    property Constraints;
    property PopupMenu;
    property TabOrder;
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
    property TabStop;
    {$IFDEF DELPHI_TOUCH}
    property OnGesture;
    property Touch;
    {$ENDIF}
  end;

implementation

type
  TRoundingType = (rtNone, rtTop, rtBottom, rtBoth);

function AnimateDouble(var Start: integer; Stop: integer; Delta: Double; Margin: integer): Boolean;
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

function TAdvSmoothJogWheel.GetColor(Delta: integer): TColor;
var
  r,g,b: longint;
  l: longint;
  c: TColor;
  br: Double;
begin
  c := clBlack;
  br := 1;
  case FCurrentMode of
    wmContinuous:
    begin
      c := ModeContinuous.Color;
      br := ModeContinuous.Brightness;
    end;
    wmCue:
    begin
      c := ModeCue.Color;
      br := ModeCue.Brightness;
    end;
    wmAutoCue:
    begin
      c := ModeAutoCue.Color;
      br := ModeAutoCue.Brightness;
    end;
  end;

  l := ColorToRGB(c);
  b := ((l AND $FF0000) shr 16);
  g := ((l AND $FF00) shr 8);
  r := (l AND $FF);

  Delta := Round(Delta * br);

  if FMouseDown and ((FModeCount / 100) >= FModeChangeDelay) then
  begin
    r := Max(0,Min(255,r + Delta - 30));
    g := Max(0,Min(255,g + Delta - 30));
    b := Max(0,Min(255,b + Delta - 30));
  end
  else
  begin
    r := Max(0,Min(255,r + Delta));
    g := Max(0,Min(255,g + Delta));
    b := Max(0,Min(255,b + Delta));  
  end;

//  r := round(Min(255,r * delta /100));
//  g := round(Min(255,g * delta /100));
//  b := round(Min(255,b * delta /100));

  Result := RGB(r,g,b);
end;

function CreateRoundRectangle(R: TGPRectF; Radius: Integer; RoundingType: TRoundingType; Mirror: Boolean): TGPGraphicsPath;
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

{ TAdvSmoothJogWheel }

procedure TAdvSmoothJogWheel.Animate(Sender: TObject);
var
  d: Double;
  posTo: integer;
  temp: TAdvSmoothJogWheelModeType;
begin
  if StartModeTimer then
  begin
    Inc(FModeCount);
    if ((FModeCount / 100) >= FModeChangeDelay) then
    begin
      StartModeTimer := false;
      temp := FCurrentMode;
      if temp = wmAutoCue then
        temp := wmContinuous
      else
        Inc(temp);

      SetCurrentMode(temp);
      Changed;
    end;
  end;
  
  if FAnimate and IsMode then
  begin
    posto := GetPositionTo;
    d := Abs(posto - GetPosition) / Max(1, Abs(FSp) * AnimationFactor);
    FAnimating := AnimateDouble(FCurrentScPos, posto, d, 1);
    if FAnimating then
    begin
      case Direction of
        wdHorizontal: FValue := Max(Min(Step * -GetPosition, MaxDouble), -MaxDouble);
        wdVertical: FValue := Max(Min(Step * GetPosition, MaxDouble), -MaxDouble);
      end;
      Changed;
    end
    else
    begin
      FCurrentScPos := GetPositionTo;
      if IsMode and (FCurrentMode = wmAutoCue) and (FValue <> 0) then
      begin
        FScPosTo := 0;
        FAnimate := true;
      end
      else
        FAnimate := false;

      case Direction of
        wdHorizontal: FValue := Max(Min(Step * -GetPosition, MaxDouble), -MaxDouble);
        wdVertical: FValue := Max(Min(Step * GetPosition, MaxDouble), -MaxDouble);
      end;
    end;
    if FAnimate then
      if Assigned(FOnValueChanged) then
        FOnValueChanged(Self, FValue, FCurrentMode);
  end;
end;

procedure TAdvSmoothJogWheel.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothJogWheel) then
  begin
    FGripSpacing := (Source as TAdvSmoothJogWheel).GripSpacing;
    FGripSize := (Source as TAdvSmoothJogWheel).GripSize;
    FDirection := (Source as TAdvSmoothJogWheel).Direction;
    FValue := (Source as TAdvSmoothJogWheel).Value;
    FStep := (Source as TAdvSmoothJogWheel).Step;
    FSingleClickStep := (Source as TAdvSmoothJogWheel).SingleClickStep;
    FModeAutoCue.Assign((Source as TAdvSmoothJogWheel).ModeAutoCue);
    FModeCue.Assign((Source as TAdvSmoothJogWheel).ModeCue);
    FModeContinuous.Assign((Source as TAdvSmoothJogWheel).ModeContinuous);
    FModes := (Source as TAdvSmoothJogWheel).Modes;
    FApperture := (Source as TAdvSmoothJogWheel).Apperture;
    FAnimationFactor := (Source as TAdvSmoothJogWheel).AnimationFactor;
    FMinimumValue := (Source as TAdvSmoothJogWheel).MinimumValue;
    FMaximumValue := (Source as TAdvSmoothJogWheel).MaximumValue;
    FMouseWheelStep := (Source as TAdvSmoothJogWheel).MouseWheelStep;
    //Needs check
    Mode := (Source as TAdvSmoothJogWheel).Mode;
    FFocusColor := (Source as TAdvSmoothJogWheel).FocusColor;
    FShowValue := (Source as TAdvSmoothJogWheel).ShowValue;
    FValueFont.Assign((Source as TAdvSmoothJogWheel).FValueFont);
    FValueFormat := (Source as TAdvSmoothJogWheel).FValueFormat;
    FValueTop := (Source as TAdvSmoothJogWheel).ValueTop;
    FValueLeft := (Source as TAdvSmoothJogWheel).FValueLeft;
    FValueLocation := (Source as TAdvSmoothJogWheel).ValueLocation;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.Changed;
begin
  Invalidate;
end;

constructor TAdvSmoothJogWheel.Create(AComponent: TComponent);
begin
  inherited;
  FAnimationFactor := 5;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.OnTimer := Animate;
  FAnimationTimer.Interval := 1;
  FAnimationTimer.Enabled := true;
  DoubleBuffered := true;
  Height := 35;
  Width := 250;
  FGripSpacing := 20;
  FGripSize := 15;
  FDirection := wdHorizontal;
  FOldW := Width;
  FOldH := Height;
  FResize := true;
  FValue := 0;
  FStep := 10;
  FSingleClickStep := 10;
  FMinimumValue := 0;
  FMaximumValue := 0;
  FModeAutoCue := TAdvSmoothJogWheelMode.Create(Self);
  FModeAutoCue.OnChange := ModeChanged;
  FModeContinuous := TAdvSmoothJogWheelMode.Create(Self);
  FModeContinuous.OnChange := ModeChanged;
  FModeCue := TAdvSmoothJogWheelMode.Create(Self);
  FModeCue.OnChange := ModeChanged;
  FModes := [wmContinuous, wmCue, wmAutoCue];
  FCurrentMode := wmContinuous;
  FApperture := 80;
  FModeChangeDelay := 2;
  FFocusColor := clWhite;
  FEnabled := true;
  FMouseWheelStep := 10;
  FShowValue := false;
  FValueFormat := '%g';
  FValueFont := TFont.Create;
  {$IFNDEF DELPHI9_LVL}
  FValueFont.Name := 'Tahoma';
  {$ENDIF}
  FValueFont.Color := clWhite;
  FValueFont.OnChange := FontChanged;
  FValueLocation := wlCenterLeft;
  FValueLeft := 0;
  FValueTop := 0;
end;

procedure TAdvSmoothJogWheel.CreateWnd;
begin
  inherited;
  FLoaded := true;
  SetDirection(Direction);
end;

destructor TAdvSmoothJogWheel.Destroy;
begin
  FAnimationTimer.Free;
  FModeAutoCue.Free;
  FModeCue.Free;
  FModeContinuous.Free;
  FValueFont.Free;
  inherited;
end;

procedure TAdvSmoothJogWheel.DoEnter;
begin
  inherited;
  FFocused := true;
  Changed;
end;

procedure TAdvSmoothJogWheel.DoExit;
begin
  inherited;
  FFocused := false;
  Changed;
end;

procedure TAdvSmoothJogWheel.DrawBackGround(g: TGPGraphics);
var
  b: TGPSolidBrush;
  r: TGPRectF;
  path: TGPGraphicsPath;
begin
  r := MakeRect(0, 0, Width - 1, Height - 1);
  path := CreateRoundRectangle(r, 5, rtBoth, false);
  b := TGPSolidBrush.Create(MakeColor(255, GetColor(0)));
  g.FillPath(b, path);
  b.free;
  path.free;

  r := MakeRect(2, 2, Width - 5, Height - 5);
  path := CreateRoundRectangle(r, 5, rtBoth, false);
  b := TGPSolidBrush.Create(MakeColor(255, GetColor(120)));
  g.FillPath(b, path);
  b.free;

  path.free;
end;

procedure TAdvSmoothJogWheel.DrawGloss(g: TGPGraphics);
var
  b: TGPSolidBrush;
  rTop, rBottom: TGPRectF;
  p: TGPGraphicsPath;
begin
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);

  case FDirection of
    wdHorizontal: rTop := MakeRect(2, 2, Width - 5, 5);
    wdVertical: rTop := MakeRect(2, 2, 5, Height - 5);
  end;

  p := CreateRoundRectangle(rtop, 5, rtBoth, false);
  b := TGPSolidBrush.Create(MakeColor(100, clwhite));
  g.FillPath(b, p);
  p.free;
  b.free;

  case FDirection of
    wdHorizontal: rBottom := MakeRect(2, Height - 6, Width - 5, 4);
    wdVertical: rBottom := MakeRect(Width - 6, 2, 4, Height - 5);
  end;

  p := CreateRoundRectangle(rBottom, 5, rtBoth, false);
  b := TGPSolidBrush.Create(MakeColor(255, GetColor(50)));
  g.FillPath(b, p);
  p.free;
  b.free;

  g.free;
end;

procedure TAdvSmoothJogWheel.DrawGrip(g: TGPGraphics; Position: Double);
var
  r: TGPRectF;
  m: TLinearGradientMode;
  b: TGPBrush;
  path: TGPGraphicsPath;
begin
  m := LinearGradientModeHorizontal;
  case FDirection of
    wdHorizontal:
    begin
      r := MakeRect(Position, 4, GripSize, Height - 8);
      m := LinearGradientModeVertical;
    end;
    wdVertical:
    begin
      r := MakeRect(4, Position, Width - 8, GripSize);
      m := LinearGradientModeHorizontal;
    end;
  end;

  b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1 , r.Width + 2 , r.Height + 2),
    MakeColor(255, GetColor(50)), MakeColor(255, clWhite), m);
  path := CreateRoundRectangle(r, GripSize div 3, rtBoth, false);
  g.FillPath(b, path);
  b.free;
  path.free;

  case FDirection of
    wdHorizontal:
    begin
      r := MakeRect(Position + 0.5, 4 + 0.5, GripSize - 1, Height - 9);
      m := LinearGradientModeVertical;
    end;
    wdVertical:
    begin
      r := MakeRect(4 + 0.5, Position + 0.5, Width - 9, GripSize - 1);
      m := LinearGradientModeHorizontal;
    end;
  end;

  b := TGPLinearGradientBrush.Create(MakeRect(r.X - 1, r.Y - 1 , r.Width + 2 , r.Height + 2),
    MakeColor(255, GetColor(50)), MakeColor(255, GetColor(120)), m);
  path := CreateRoundRectangle(r, (GripSize - 1) div 3, rtBoth, false);
  g.FillPath(b, path);
  b.free;
  path.free;
end;

procedure TAdvSmoothJogWheel.DrawGrips;
var
  pos: Double;
  cnt, i, j: integer;
  rgn: TGPRegion;
begin
  rgn := TGPRegion.Create(MakeRect(2, 2, Width - 5, Height - 5));
  g.SetClip(rgn);

  pos := 0;
  cnt := 0;
  case FDirection of
    wdHorizontal: cnt := Round(Width / ((GripSpacing / 2) + GripSize)) + 5;
    wdVertical: cnt := Round(Height / ((GripSpacing / 2) + GripSize)) + 5;
  end;

  i := (GetPosition div GripSpacing) - (cnt div 2);
  j := 0;
  while j <= cnt do
  begin
    case FDirection of
      wdHorizontal: pos := -GetPosition + (i * GripSpacing) + (Width - GripSize) / 2;
      wdVertical: pos := -GetPosition + (i * GripSpacing) + (Height - GripSize) / 2;
    end;

    DrawGrip(g, pos);
    Inc(j);
    Inc(i);
  end;

  g.ResetClip;
  rgn.Free;
end;

procedure TAdvSmoothJogWheel.DrawIndicator(g: TGPGraphics);
var
  m: TAdvSmoothJogWheelMode;
  r: TGPRectF;
  b: TGPSolidBrush;
  p: TGPPen;
  path: TGPGraphicsPath;
  xm, ym, msx, msy: Double;
  rgn: TGPRegion;
  w, h, x, y: integer;
  pos: Double;
begin 
  if IsMode then
  begin
    m := nil;
    case FCurrentMode of
      wmContinuous: m := ModeContinuous;
      wmCue: m := ModeCue;
      wmAutoCue: m := ModeAutoCue;
    end;

    if m <> nil then
    begin
      with m.Indicator do
      begin
        if Visible then
        begin
          rgn := TGPRegion.Create(MakeRect(2, 2, Self.Width - 5, Self.Height - 5));
          g.SetClip(rgn);         
          
          b := TGPSolidBrush.create(MakeColor(255, Color));
          p := TGPPen.Create(Makecolor(255, Color), Width);
          path := TGPGraphicsPath.Create;

          pos := -GetPosition + (Value / Step);
          
          case FDirection of
            wdHorizontal:
            begin
              case Position of
                wpCenter: r := MakeRect(pos - (Width / 2) + (Self.Width / 2), (Height - Size) / 2, Width, Size);
                wpTop: r := MakeRect(pos - (Width / 2) + (Self.Width / 2), 0, Width, Size);
                wpBottom: r := MakeRect(pos - (Width / 2) + (Self.Width / 2), Height - Size, Width, Size);
              end;            
            end;
            wdVertical:
            begin
              case Position of
                wpCenter: r := MakeRect((Self.Width - Size) / 2, pos - (Width / 2) + (Self.Height / 2), Size, Width);
                wpTop: r := MakeRect(0, pos - (Width / 2) + (Self.Height / 2), Size, Width);
                wpBottom: r := MakeRect(Self.Width - Size, pos - (Width / 2) + (Self.Height / 2), Size, Width);
              end;                        
            end;
          end;

          xm := r.X;
          ym := r.Y;
          msx := r.Width;
          msy := r.Height;
          
          case Shape of
            wsLine: 
            begin
              case FDirection of
                wdHorizontal: g.DrawLine(p, xm + (Width / 2), ym, xm + (Width / 2), ym + msy);
                wdVertical: g.DrawLine(p, xm, ym + (Width / 2), xm + msx, ym + (Width / 2));
              end;              
            end;
            wsCircle: g.FillEllipse(b, r);
            wsDiamond:
            begin
               //DIAMOND 4 POINTS
              path.AddLine(MakePoint(xm + msx / 2, ym), MakePoint(xm + msx, ym + msy / 2));
              path.AddLine(MakePoint(xm + msx, ym + msy / 2), MakePoint(xm + msx / 2 , ym + msy));              
              path.AddLine(MakePoint(xm + msx / 2 , ym + msy), MakePoint(xm, ym + msy / 2));              
              path.CloseFigure;
              g.FillPath(b, path);
            end;
            wsTriangle:
            begin
              //TRIANGLE 3 POINTS
              path.AddLine(Makepoint(xm + msx / 2, ym), MakePoint(xm + msx , ym + msy));
              path.AddLine(MakePoint(xm + msx, ym + msy), MakePoint(xm , ym + msy));
              path.CloseFigure;
              g.FillPath(b, path);
            end;
            wsSquare:
            begin
              g.FillRectangle(b, r);
            end;
          end;         

          if not Picture.Empty then
          begin
            w := PictureHeight;
            h := PictureWidth;
            x := Round(xm + (msx / 2) - (w / 2));
            y := Round(ym + (msy / 2) - (h / 2));            
            picture.GDIPDraw(g, Bounds(x, y, w, h));            
          end;          
          
          path.free;
          b.Free;
          p.Free;
          g.ResetClip;
          rgn.Free;

          if Assigned(FImages) then
          begin
            if (ImageIndex > -1) and (ImageIndex < FImages.Count) then
            begin
              w := FImages.Width;
              h := FImages.Height;
              x := Round(xm + (msx / 2) - (w / 2));
              y := Round(ym + (msy / 2) - (h / 2));
              FImages.Draw(Canvas, x, y, ImageIndex);
            end;                      
          end;          
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothJogWheel.DrawOverlay(g: TGPGraphics);
var
  b: TGPLinearGradientBrush;
  rLeft, rRight: TGPRectF;
  m: TLinearGradientMode;
begin
  m := LinearGradientModeHorizontal;
  case FDirection of
    wdHorizontal:
    begin
      rLeft := MakeRect(2, 2, Width / 3, Height - 5);
      m := LinearGradientModeHorizontal;
    end;
    wdVertical:
    begin
      rLeft := MakeRect(2, 2, Width - 5, Height / 3);
      m := LinearGradientModeVertical;
    end;
  end;

  b := TGPLinearGradientBrush.Create(MakeRect(rLeft.X - 1, rLeft.Y - 1, rLeft.Width + 2, rLeft.Height + 2),
    MakeColor(255, GetColor(0)), MakeColor(0, GetColor(0)), m);
  g.FillRectangle(b, rLeft);
  b.free;

  case FDirection of
    wdHorizontal:
    begin
      rRight := MakeRect(width - (Width / 3) - 2, 2, Width / 3, Height - 5);
      m := LinearGradientModeHorizontal;
    end;
    wdVertical:
    begin
      rRight := MakeRect(2, Height - (Height / 3) - 2, Width - 5, Height / 3);
      m := LinearGradientModeVertical;
    end;
  end;

  b := TGPLinearGradientBrush.Create(MakeRect(rRight.X - 1, rRight.Y - 1, rRight.Width + 2, rRight.Height + 2),
    MakeColor(0, GetColor(0)), MakeColor(255, GetColor(0)), m);
  g.FillRectangle(b, rRight);
  b.free;
end;

procedure TAdvSmoothJogWheel.DrawValue;
var
  f: TGPFont;
  ff: TGPFontFamily;
  sf: TGPStringFormat;
  fs: integer;
  s: string;
  xs, ys, tw, th: Double;
  sizerect: TGPRectF;
  b: TGPSolidBrush;
begin
  ff := TGPFontFamily.Create(ValueFont.Name);
  if (ff.Status in [FontFamilyNotFound, FontStyleNotFound]) then
  begin
    ff.Free;
    ff := TGPFontFamily.Create('Arial');
  end;

  fs := 0;
  if (fsBold in ValueFont.Style) then
    fs := fs + 1;
  if (fsItalic in ValueFont.Style) then
    fs := fs + 2;
  if (fsUnderline in ValueFont.Style) then
    fs := fs + 4;


  s := Format(ValueFormat, [Value]);

  sf := TGPStringFormat.Create;
  f := TGPFont.Create(ff, ValueFont.Size , fs, UnitPoint);
  g.MeasureString(s, Length(s), f, MakeRect(0, 0, 10000, 10000), sf, sizerect);

  tw := Round(sizerect.Width);
  th := Round(sizerect.Height);

  GetLabelPosition(xs, ys, MakeRect(0, 0, Width - 1, Height - 1), tw, th, valueLocation);

  b := TGPSolidBrush.Create(MakeColor(255, ValueFont.Color));
  g.DrawString(s, Length(s), f, MakePoint(xs, ys), sf, b);
  b.free;

  f.Free;
  sf.Free;
  ff.Free;
end;

procedure TAdvSmoothJogWheel.FontChanged(Sender: TObject);
begin
  Changed;
end;

function TAdvSmoothJogWheel.GetPosition: integer;
var
  s: integer;
  mx, mn: integer;
begin
  Result := 0;
  if IsMode then
  begin
    s := 0;
    case Direction of
      wdHorizontal: s := Width;
      wdVertical: s := Height;
    end;

    mx := 0;
    mn := 0;
    case FCurrentMode of
      wmContinuous:
      begin
        if Step > 0 then
        begin
          case Direction of
            wdHorizontal:
            begin
              mn := Round(MinimumValue / Step);
              mx := Round(MaximumValue / Step);
            end;
            wdVertical:
            begin
              mn := -Round(MinimumValue / Step);
              mx := -Round(MaximumValue / Step);
            end;
          end;
        end;
        if (mn = 0) and (mx = 0) then
        begin
          mn := -FCurrentScPos;
          mx := -FCurrentScPos;
        end;
      end;
      wmAutoCue, wmCue:
      begin
        case Direction of
          wdHorizontal:
          begin
           mn := -Round((s / 2) * (Apperture / 100));
           mx := Round((s / 2) * (Apperture / 100));
          end;
          wdVertical:
          begin
            mn := Round((s / 2) * (Apperture / 100));
            mx := -Round((s / 2) * (Apperture / 100));
          end;
        end;
      end;
    end;

    case Direction of
      wdHorizontal: Result := -Max(Min(-FCurrentScPos, mx), mn);
      wdVertical: Result := Max(Min(FCurrentScPos, -mx), -mn);
    end;
  end
  else
    Result := FCurrentScPos;
end;

procedure TAdvSmoothJogWheel.GetLabelPosition(var x, y: Double; rectangle: TGPRectF;
  objectwidth, objectheight: Double; location: TAdvSmoothJogWheelLocation);
var
  w, h, tw, th: Double;
begin
  tw := objectwidth;
  th := objectheight;
  w := rectangle.Width;
  h := rectangle.Height;
  case location of
    wlTopLeft:
    begin
      x := 0;
      y := 0;
    end;
    wlTopRight:
    begin
      x := w - tw;
      y := 0;
    end;
    wlBottomLeft:
    begin
      x := 0;
      y := h - th;
    end;
    wlBottomRight:
    begin
      x := w - tw;
      y := h - th;
    end;
    wlTopCenter:
    begin
      x := (w - tw) / 2;
      y := 0;
    end;
    wlBottomCenter:
    begin
      x := (w - tw) / 2;
      y := h - th;
    end;
    wlCenterCenter:
    begin
      x := (w - tw) / 2;
      y := (h - th) / 2;
    end;
    wlCenterLeft:
    begin
      x := 0;
      y := (h - th) / 2;
    end;
    wlCenterRight:
    begin
      x := w - tw;
      y := (h - th) / 2;
    end;
    wlCustom:
    begin
      x := ValueLeft;
      y := ValueTop;
    end;
  end;
end;

function TAdvSmoothJogWheel.GetPositionTo: integer;
var
  s, mn, mx: integer;
begin
  Result := 0;
  if IsMode then
  begin
    s := 0;
    case Direction of
      wdHorizontal: s := Width;
      wdVertical: s := Height;
    end;
    
    mx := 0;
    mn := 0;
    case FCurrentMode of
      wmContinuous:
      begin
        if Step > 0 then
        begin
          mn := Round(MinimumValue / Step);
          mx := Round(MaximumValue / Step);
        end;
        if (mn = 0) and (mx = 0) then
        begin
          mn := -FScPosTo;
          mx := -FScPosTo;
        end;
      end;
      wmAutoCue, wmCue:
      begin
        mn := -(s div 2 * Apperture div 100);
        mx := (s div 2 * Apperture div 100);
      end;
    end;

    case Direction of
      wdHorizontal:
      begin
        if FScPosTo < 0 then
          Result := -Min(-FScPosTo, mx)
        else
          Result := -Max(-FScPosTo, mn);
      end;
      wdVertical:
      begin
        if FScPosTo < 0 then
          Result := Min(FScPosTo, mx)
        else
          Result := Max(FScPosTo, mn);
      end;
    end;
  end
  else
    Result := FScPosTo;
end;

function TAdvSmoothJogWheel.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvSmoothJogWheel.GetVersionNr: integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TAdvSmoothJogWheel.IsMode: Boolean;
begin
  result := (wmContinuous in Modes) or (wmAutoCue in Modes) or (wmCue in Modes)
end;

procedure TAdvSmoothJogWheel.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if not TabStop and not FFocused then
    Exit;
    
  case Key of
    VK_DOWN, VK_LEFT: Value := Value - Step;
    VK_UP, VK_RIGHT: Value := Value + Step;
    VK_PRIOR: Value := Value + (10 * Step);
    VK_NEXT: Value := Value - (10 * Step);
  end;
end;

procedure TAdvSmoothJogWheel.ModeChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothJogWheel.MouseDown(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
begin
  inherited;
  if not Enabled then
    Exit;
      
  SetFocus;
  if IsMode then
  begin
    StartModeTimer := true;
    FModeCount := 100;
    FMouseDown := true;
    Changed;

    if FDirection = wdVertical then
    begin
      FDragXY := Y;
      FScrollXY := Y;
    end
    else
    begin
      FDragXY := X;
      FScrollXY := X;
    end;

    FTimeStart := GetTickCount;
    FClickY := Y;
    FClickX := X;
  end;
end;

procedure TAdvSmoothJogWheel.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  xy: integer;
begin
  inherited;
  if not Enabled then
    Exit;
      
  if IsMode then
  begin
    if (csDesigning in ComponentState) then
    begin
      FMouseDown := false;
      FMouseUp := false;
      exit;
    end;

    if FDirection = wdVertical then
      XY := Y
    else
      XY := X;

    if FMouseDown then
    begin
      FSp := 4;
      FAnimate := false;

      if (Abs(X - FClickX) > 4) or (Abs(Y - FClickY) > 4) then
      begin
        FClickX := X;
        FClickY := Y;
        StartModeTimer := false;
        FModeCount := 0;
      end;

      if (XY - FDragXY) > 0 then
        FCurrentScPos := GetPosition - Abs(XY - FDragXY)
      else
        FCurrentScPos := GetPosition + Abs(XY - FDragXY);

      FDragXY := XY;
      FScPosTo := GetPosition;
      case Direction of
        wdHorizontal: Value := Max(Min(Step * -GetPosition, MaxDouble), -MaxDouble);
        wdVertical: Value := Min(Max(Step * GetPosition, -MaxDouble), MaxDouble);
      end;
    end
    else
    begin
      if FMouseUp then
      begin
        FMouseUp := false;

        if ((FTimeStop - FTimeStart) > 500) or ((FTimeStop - FTimeStart) = 0) then
          exit;

        FSp := Abs(XY - FScrollXY) / (FTimeStop - FTimeStart);
        if FSp > 0 then
        begin
          if (XY - FScrollXY) > 0 then
            FScPosTo := GetPositionTo - Round(Abs(XY - FScrollXY) * FSp)
          else
            FScPosTo := GetPositionTo + Round(Abs(XY - FScrollXY) * FSp);
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothJogWheel.MouseUp(Button: TMouseButton; Shift: TShiftState;
  X, Y: Integer);
var
  xy, xl, xr, yl, yr, wl, wr, hl, hr: integer;
begin
  inherited;
  if not Enabled then
    Exit;

  StartModeTimer := false;

  if (Abs(X - FClickX) > 4) or (Abs(Y - FClickY) > 4) and not ((FModeCount / 100) >= FModeChangeDelay) then
  begin
    FClickX := X;
    FClickY := Y;
    FModeCount := 100;
  end;

  if IsMode then
  begin
    if FAnimating and FAnimate then
    begin
      FAnimate := false;
      FScrollXY := GetPosition;
      FScPosTo := GetPosition;
      FCurrentScPos := GetPosition;
      FTimeStart := 0;
      FTimeStop := 0;
    end;

    if FDirection = wdVertical then
      XY := Y
    else
      XY := X;

    if ((FScrollXY > XY - 4) and (FScrollXY < XY + 4)) and not ((FModeCount / 100) >= FModeChangeDelay) then
    begin
      xl := 0;
      yl := 0;
      wl := Width div 2;
      hl := Height;

      xr := Width div 2;
      yr := 0;
      wr := Width div 2;
      hr := Height;
        
      if FDirection = wdVertical then
      begin
        xl := 0;
        yl := 0;
        wl := Width;
        hl := Height div 2;

        xr := 0;
        yr := Height div 2;
        wr := Width;
        hr := Height div 2;
      end;

      //Single click left
      if PtInRect(Bounds(xl, yl, wl, hl), Point(X, Y)) then
      begin
        case Direction of
          wdHorizontal: Value := Value - SingleClickStep;
          wdVertical: Value := Value + SingleClickStep;
        end;
      end
      //Single click right
      else if PtInRect(Bounds(xr, yr, wr, hr), Point(X, Y)) then
      begin
        case Direction of
          wdHorizontal: Value := Value + SingleClickStep;
          wdVertical: Value := Value - SingleClickStep;
        end;
      end;
    end
    else
    begin
      FSp := 4;
      if IsMode and (FCurrentMode = wmAutoCue) then
      begin
        FScPosTo := 0;
        FAnimate := true;
        FSp := 4;
      end;
    end;

    FMouseDown := false;
    FMouseUp := true;
    Changed;
    FTimeStop := GetTickCount;
    FAnimate := (FTimeStop - FTimeStart > 0);
  end;
end;

procedure TAdvSmoothJogWheel.MouseWheelHandler(var Message: TMessage);
var
  s: integer;
  m: Double;
begin
  inherited;
  if IsMode then
  begin
    s := 0;
    m := MouseWheelStep;
    case Direction of
      wdHorizontal: s := Width;
      wdVertical: s := Height;
    end;

    case Message.Msg of
      WM_MOUSEWHEEL:
      begin
        if FCurrentMode = wmContinuous then
        begin
          if (MaximumValue <> 0) and (MinimumValue <> 0) and (MinimumValue < MaximumValue) then
          begin
            if integer(Message.WParam) < 0 then
              Value := Max(Min(Value - m, MaximumValue), MinimumValue)
            else
              Value := Max(Min(Value + m, MaximumValue), MinimumValue);
          end
          else
          begin
            if integer(Message.WParam) < 0 then
              Value := Value - m
            else
              Value := Value + m;
          end;
        end
        else
        begin
          if integer(Message.WParam) < 0 then
            Value := Max(Min(Value - m, (s div 2 * Apperture div 100) * FStep), -(s div 2 * Apperture div 100) * FStep)
          else
            Value := Max(Min(Value + m, (s div 2 * Apperture div 100) * FStep), -(s div 2 * Apperture div 100) * FStep)
        end;
      end;
    end;
  end;
end;

procedure TAdvSmoothJogWheel.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  if not (csDestroying in ComponentState) then
  begin
    if (AOperation = opRemove) and (AComponent = FImages) then
      FImages := nil;
  end;
  inherited;    
end;

procedure TAdvSmoothJogWheel.Paint;
var
  g: TGPGraphics;
  path: TGPGraphicsPath;
  p: TGPPen;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawBackGround(g);
  DrawGloss(g);
  DrawGrips(g);
  DrawIndicator(g);
  DrawOverlay(g);
  if ShowValue then
    DrawValue(g);
  if FFocused and TabStop then
  begin
    g.SetSmoothingMode(SmoothingModeHighQuality);
    path := CreateRoundRectangle(MakeRect(0, 0, Width - 1, Height - 1), 5, rtBoth, false);
    p := TGPPen.Create(Makecolor(255, FocusColor));
    p.SetDashStyle(DashStyleDot);
    g.DrawPath(p, path);
    p.Free;
    path.free;
  end;
  g.free;
end;

procedure TAdvSmoothJogWheel.Resize;
begin
  if FResize then
  begin
    case FDirection of
      wdHorizontal:
      begin
        FOldW := Width;
        FOldH := Height;
      end;
      wdVertical:
      begin
        FOldW := Height;
        FOldH := Width;
      end;
    end;
  end;
  inherited;
end;

procedure TAdvSmoothJogWheel.SetAnimationFactor(const Value: integer);
begin
  if FAnimationFactor <> value then
  begin
    FAnimationFactor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetApperture(const Value: integer);
begin
  if FApperture <> value then
  begin
    FApperture := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetCurrentMode(
  const Value: TAdvSmoothJogWheelModeType);
var
  temp: TAdvSmoothJogWheelModeType;
begin
  temp := Value;
  if IsMode then
  begin
    while not (temp in Modes) do
      Inc(temp);

    FCurrentMode := temp;
    if FcurrentMode = wmAutoCue then
      Self.Value := 0;

    if Assigned(FOnModeChanged) then
      FOnModeChanged(Self, FCurrentMode);
  end
  else
  begin
    FCurrentMode := temp;
  end;
  Changed;
end;

procedure TAdvSmoothJogWheel.SetGripSpacing(const Value: Integer);
begin
  if FGripSpacing <> value then
  begin
    FGripSpacing := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetMaximumValue(const Value: Double);
begin
  if FMaximumValue <> value then
  begin
    FMaximumValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetMinimumValue(const Value: Double);
begin
  if FMinimumValue <> value then
  begin
    FMinimumValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetModeAutoCue(
  const Value: TAdvSmoothJogWheelMode);
begin
  if FModeAutoCue <> value then
  begin
    FModeAutoCue := Value;
    ModeChanged(Self);
  end;
end;

procedure TAdvSmoothJogWheel.SetModeChangeDelay(const Value: Double);
begin
  if (FModeChangeDelay <> value) and (Value > 0) then
  begin
    FModeChangeDelay := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetModeContinuous(
  const Value: TAdvSmoothJogWheelMode);
begin
  if FModeContinuous <> value then
  begin
    FModeContinuous := Value;
    ModeChanged(Self);
  end;
end;

procedure TAdvSmoothJogWheel.SetModeCue(const Value: TAdvSmoothJogWheelMode);
begin
  if FModeCue <> value then
  begin
    FModeCue := Value;
    ModeChanged(Self);
  end;
end;

procedure TAdvSmoothJogWheel.SetModes(const Value: TAdvSmoothJogWheelModes);
begin
  if FModes <> value then
  begin
    FModes := Value;
    SetCurrentMode(FCurrentMode);
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetMouseWheelStep(const Value: Double);
begin
  if FMouseWheelStep <> value then
  begin
    FMouseWheelStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetGripSize(const Value: integer);
begin
  if FGripSize <> value then
  begin
    FGripSize := Max(1, Value);
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetDirection(const Value: TAdvSmoothJogWheelDirection);
begin
  FDirection := Value;
  
  if not FLoaded or (csLoading in ComponentState) then
    Exit;

  FResize := false;
  if FDirection = wdHorizontal then
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

procedure TAdvSmoothJogWheel.SetEnabled(Value: Boolean);
begin
  inherited;
  if FEnabled <> value then
  begin
    FEnabled := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetFocusColor(const Value: TColor);
begin
  if FFocusColor <> value then
  begin
    FFocusColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetShowValue(const Value: Boolean);
begin
  if FShowValue <> value then
  begin
    FShowValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetSingleClickStep(const Value: Double);
begin
  if FSingleClickStep <> value then
  begin
    FSingleClickStep := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetStep(const Value: Double);
begin
  if (FStep <> value) and (Value > 0) then
  begin
    FStep := Value;
    case Direction of
      wdHorizontal: FScPosTo := -Round(FValue / FStep);
      wdVertical: FScPosTo := Round(FValue / FStep);
    end;
    FCurrentScPos := FScPosTo;    
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValue(const Value: Double);
var
  v: Double;
begin
  v := 0;
  if IsMode then
  begin
    if (Mode = wmContinuous) and not ((MaximumValue = 0) and (MinimumValue = 0)) then
      v := Max(Min(Value, MaximumValue), MinimumValue)
    else
      v := Max(Min(Value, MaxDouble), -MaxDouble);
  end;

  if FValue <> v then
  begin
    FValue := v;
    case Direction of
      wdHorizontal: FScPosTo := -Round(Fvalue / FStep);
      wdVertical: FScPosTo := Round(FValue / FStep);
    end;

    FCurrentScPos := FScPosTo;

    if Assigned(FOnValueChanged) then
      FOnValueChanged(Self, FValue, FCurrentMode);

    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValueFont(const Value: TFont);
begin
  if FValueFont <> value then
  begin
    FValueFont.Assign(Value);
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValueFormat(const Value: String);
begin
  if FValueFormat <> value then
  begin
    FValueFormat := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValueLeft(const Value: integer);
begin
  if FValueLeft <> Value then
  begin
    FValueLeft := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValueLocation(
  const Value: TAdvSmoothJogWheelLocation);
begin
  if FValueLocation <> value then
  begin
    FValueLocation := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetValueTop(const Value: integer);
begin
  if FValueTop <> value then
  begin
    FValueTop := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheel.SetVersion(const Value: string);
begin

end;

procedure TAdvSmoothJogWheel.WMEraseBkgnd(var Message: TWmEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TAdvSmoothJogWheel.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  if TabStop then
    Message.Result := DLGC_WANTALLKEYS or DLGC_WANTARROWS
  else
    Message.Result := 0;
end;

procedure TAdvSmoothJogWheel.WMPaint(var Message: TWMPaint);
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

{ TAdvSmoothJogWheelIndicator }

procedure TAdvSmoothJogWheelIndicator.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothJogWheelIndicator then
  begin
    FVisible := (Source as TAdvSmoothJogWheelIndicator).Visible;
    FPicture.Assign((Source as TAdvSmoothJogWheelIndicator).Picture);
    FColor := (Source as TAdvSmoothJogWheelIndicator).Color;
    FImageIndex := (Source as TAdvSmoothJogWheelIndicator).ImageIndex;
    FShape := (Source as TAdvSmoothJogWheelIndicator).Shape;
    FSize := (Source as TAdvSmoothJogWheelIndicator).Size;
    FWidth := (Source as TAdvSmoothJogWheelIndicator).Width;
    FPosition := (Source as TAdvSmoothJogWheelIndicator).Position;
    FPictureWidth := (Source as TAdvSmoothJogWheelIndicator).PictureWidth;
    FPictureHeight := (Source as TAdvSmoothJogWheelIndicator).PictureHeight;
    FValue := (Source as TAdvSmoothJogWheelIndicator).Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothJogWheelIndicator.Create(AOwner: TAdvSmoothJogWheel);
begin
  FOwner := AOwner;
  FPicture := TAdvGDIPPicture.Create;
  FPicture.OnChange := PictureChanged;
  FVisible := true;
  FColor := clRed;
  FValue := 0;
  FImageIndex := -1;
  FShape := wsLine;
  FWidth := 2;
  FSize := 15;
  FPosition := wpCenter;
  FPictureHeight := 15;
  FPictureWidth := 15;
end;

destructor TAdvSmoothJogWheelIndicator.Destroy;
begin
  FPicture.Free;
  inherited;
end;

procedure TAdvSmoothJogWheelIndicator.PictureChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothJogWheelIndicator.SetColor(const Value: TColor);
begin
  if FColor <> value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetImageIndex(const Value: integer);
begin
  if FImageIndex <> value then
  begin
    FImageIndex := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetPicture(const Value: TAdvGDIPPicture);
begin
  if FPicture <> value then
  begin
    FPicture.Assign(Value);
    PictureChanged(Self);
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetPictureHeight(const Value: integer);
begin
  if FPictureHeight <> value then
  begin
    FPictureHeight := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetPictureWidth(const Value: integer);
begin  
  if FPictureWidth <> value then
  begin
    FPictureWidth := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetPosition(
  const Value: TAdvSmoothJogWheelIndicatorPosition);
begin
  if FPosition <> value then
  begin
    FPosition := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetShape(
  const Value: TAdvSmoothJogWheelIndicatorShape);
begin
  if FShape <> value then
  begin
    FShape := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetSize(const Value: integer);
begin
  if FSize <> value then
  begin
    FSize := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetValue(const Value: Double);
begin
  if FValue <> value then
  begin
    FValue := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetVisible(const Value: Boolean);
begin
  if FVisible <> value then
  begin
    FVisible := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelIndicator.SetWidth(const Value: integer);
begin
  if FWidth <> value then
  begin
    FWidth := Value;
    Changed;
  end;
end;

{ TAdvSmoothJogWheelMode }

procedure TAdvSmoothJogWheelMode.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothJogWheelMode) then
  begin
    FColor := (Source as TAdvSmoothJogWheelMode).Color;
    FIndicator.Assign((Source as TAdvSmoothJogWheelMode).Indicator);
    FBrightness := (Source as TAdvSmoothJogWheelMode).Brightness;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelMode.Changed;
begin
  FOwner.Changed;
end;

constructor TAdvSmoothJogWheelMode.Create(AOwner: TAdvSmoothJogWheel);
begin
  FOwner := AOwner;
  FColor := clBlack;
  FIndicator := TAdvSmoothJogWheelIndicator.Create(FOwner);
  FIndicator.OnChange := IndicatorChanged;
  FBrightness := 1;
end;

destructor TAdvSmoothJogWheelMode.Destroy;
begin
  FIndicator.Free;
  inherited;
end;

procedure TAdvSmoothJogWheelMode.IndicatorChanged(Sender: TObject);
begin
  Changed;
end;

procedure TAdvSmoothJogWheelMode.SetBrightness(const Value: Double);
begin
  if FBrightness <> value then
  begin
    FBrightness := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelMode.SetColor(const Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    Changed;
  end;
end;

procedure TAdvSmoothJogWheelMode.SetIndicator(
  const Value: TAdvSmoothJogWheelIndicator);
begin
  if FIndicator <> value then
  begin
    FIndicator.Assign(Value);
    IndicatorChanged(Self);
  end;
end;

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  PaintControls(DC, First);
end;

end.


{*************************************************************************}
{ TAdvCircularProgress component                                          }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2007 - 2015                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage caused by the use of this code.                 }
{ The licensed user can use the source code royalty free for building any }
{ compiled application. The complete source code remains property of the  }
{ author and may not be distributed, published, given or sold in any form }
{ as such. No parts of the source code can be included in any other       }
{ component or application without                                        }
{ written authorization of the author.                                    }
{*************************************************************************}

unit AdvCircularProgress;

{$I TMSDEFS.INC}

interface

uses
  Classes, Windows, Forms, Dialogs, Controls, Graphics, Messages, ExtCtrls,
  SysUtils, Math, AdvGDIP
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  SEGMENT_COUNT = 12;

  MAJ_VER = 1; // Major version nr.
  MIN_VER = 0; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.

  // version history
  // 1.0.1.0 : New: StepIt, StepBy procedures added , Step property added
  // 1.0.1.1 : Fixed: Issue with setting Position at runtime fixed
  // 1.0.1.2 : Improved : Drawing for small circular progress bars
  // 1.0.2.0 : Delphi XE5 & C++Builder support
  // 1.0.3.0 : New : Method WaitForComplete added

type
  TGradientDirection = (gdHorizontal, gdVertical);

  TProgressAppearance = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    FBorderColor: TColor;
    FBackGroundColor: TColor;
    FActiveSegmentColor: TColor;
    FTransitionSegmentColor: TColor;
    FInActiveSegmentColor: TColor;
    FProgressSegmentColor: TColor;
    procedure Changed;
    procedure SetBackGroundColor(const Value: TColor);
    procedure SetBorderColor(const Value: TColor);
    procedure SetActiveSegmentColor(const Value: TColor);
    procedure SetInActiveSegmentColor(const Value: TColor);
    procedure SetProgressSegmentColor(const Value: TColor);
    procedure SetTransitionSegmentColor(const Value: TColor);
  protected
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  published
    property BackGroundColor: TColor read FBackGroundColor write SetBackGroundColor;
    property BorderColor: TColor read FBorderColor write SetBorderColor;
    property ActiveSegmentColor: TColor read FActiveSegmentColor write SetActiveSegmentColor;
    property InActiveSegmentColor: TColor read FInActiveSegmentColor write SetInActiveSegmentColor;
    property TransitionSegmentColor: TColor read FTransitionSegmentColor write SetTransitionSegmentColor;
    property ProgressSegmentColor: TColor read FProgressSegmentColor write SetProgressSegmentColor;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvCircularProgress = class(TGraphicControl)
  private
    FSegPath: array[1..SEGMENT_COUNT] of TGPGraphicsPath;
    FInnerCircleRgn: TGPRegion;
    FOnMouseLeave: TNotifyEvent;
    FOnMouseEnter: TNotifyEvent;
    FAppearance: TProgressAppearance;
    FTimer: TTimer;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FTransitionSegment: Integer;
    FActiveBehind: Boolean;
    FOnProgressUpdate: TNotifyEvent;
    FClipDraw: Boolean;
    FStep: integer;
    procedure WMSize(var Message: TWMSize); message WM_SIZE;
    procedure WMEraseBkGnd(var Msg: TMessage); message WM_ERASEBKGND;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure OnAppearanceChanged(Sender: TObject);
    procedure OnTimer(Sender: TObject);
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    procedure SetAppearance(const Value: TProgressAppearance);
    function GetInterval: Cardinal;
    procedure SetInterval(const Value: Cardinal);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetPosition(const Value: Integer);
  protected
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure DrawSegments;
    procedure DrawSegment(Seg: Integer; graphics: TGPGraphics);
    procedure IncreaseByOne;
    function GetProgressSegment: Integer;
    function GetMyClientRect: TRect;
    function GetInnerCircleRect: TRect;
    procedure CalculateSegmentSize;
    procedure ClearSegmentSize;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    function GetVersionNr: Integer;
    procedure Stepit;
    procedure StepBy(delta: integer);
    procedure WaitForComplete;
  published
    property Align;
    property Anchors;
    property Constraints;
    property Enabled;
    property Font;
    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Position: Integer read FPosition write SetPosition default 0;
    property ParentFont;
    property ParentShowHint;
    property ParentBiDiMode;
    property PopupMenu;
    property ShowHint;
    property Step: integer read FStep write FStep default 10;
    property Visible;
    property Version: string read GetVersion write SetVersion stored false;
    property Appearance: TProgressAppearance read FAppearance write SetAppearance;
    property Interval: Cardinal read GetInterval write SetInterval;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnProgressUpdate: TNotifyEvent read FOnProgressUpdate write FOnProgressUpdate;
  end;


implementation

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

function ColorToARGB(Color: TColor): ARGB;
var
  c: TColor;
begin
  c := ColorToRGB(Color);
  Result := ARGB( $FF000000 or ((DWORD(c) and $FF) shl 16) or ((DWORD(c) and $FF00) or ((DWORD(c) and $ff0000) shr 16)));
end;

//------------------------------------------------------------------------------

{ TProgressAppearance }

constructor TProgressAppearance.Create;
begin
  inherited;
  FBorderColor := clNone;
  FBackGroundColor := clNone; // Transparent
  FActiveSegmentColor := $00FF9F9F;
  FTransitionSegmentColor := $00A00000;
  FInActiveSegmentColor := clSilver;
  FProgressSegmentColor := $00400080;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.Assign(Source: TPersistent);
begin
  if (Source is TProgressAppearance) then
  begin
    FBorderColor := (Source as TProgressAppearance).BorderColor;
    FBackGroundColor := (Source as TProgressAppearance).BackGroundColor;
    FActiveSegmentColor := (Source as TProgressAppearance).ActiveSegmentColor;
    FTransitionSegmentColor := (Source as TProgressAppearance).TransitionSegmentColor;
    FInActiveSegmentColor := (Source as TProgressAppearance).InActiveSegmentColor;
    FProgressSegmentColor := (Source as TProgressAppearance).ProgressSegmentColor;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.Changed;
begin
  if Assigned(OnChange) then
    OnChange(Self);
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetBorderColor(const Value: TColor);
begin
  if (FBorderColor <> Value) then
  begin
    FBorderColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetBackGroundColor(const Value: TColor);
begin
  if (FBackGroundColor <> Value) then
  begin
    FBackGroundColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetActiveSegmentColor(const Value: TColor);
begin
  if (FActiveSegmentColor <> Value) then
  begin
    FActiveSegmentColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetInActiveSegmentColor(const Value: TColor);
begin
  if (FInActiveSegmentColor <> Value) then
  begin
    FInActiveSegmentColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetProgressSegmentColor(const Value: TColor);
begin
  if (FProgressSegmentColor <> Value) then
  begin
    FProgressSegmentColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

procedure TProgressAppearance.SetTransitionSegmentColor(
  const Value: TColor);
begin
  if (FTransitionSegmentColor <> Value) then
  begin
    FTransitionSegmentColor := Value;
    Changed;
  end;
end;

//------------------------------------------------------------------------------

{ TAdvCircularProgress }

constructor TAdvCircularProgress.Create(AOwner: TComponent);
begin
  inherited;
  FAppearance := TProgressAppearance.Create;
  FAppearance.OnChange := OnAppearanceChanged;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 100;
  FTimer.OnTimer := OnTimer;
  FPosition := 0;
  FMin := 0;
  FMax := 100;
  FStep := 10;
  FTransitionSegment := 1;
  FActiveBehind := True;
  ClearSegmentSize;
  Height := 60;
  Width := 60;
  FClipDraw := False;
end;

//------------------------------------------------------------------------------

destructor TAdvCircularProgress.Destroy;
begin
  FAppearance.Free;
  if Assigned(FTimer) then
    FTimer.Free;
  ClearSegmentSize;  
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  FTimer.Enabled := Enabled;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.Loaded;
begin
  inherited;
  CalculateSegmentSize;
  if not (csDesigning in ComponentState) then
  begin
    FTimer.Enabled := Enabled;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.DrawSegment(Seg: Integer;
  graphics: TGPGraphics);
var
  solidBrush: TGPSolidBrush;
  Clr: TColor;
  ProgSeg: Integer;
begin
  if Assigned(graphics) then
  begin
    ProgSeg := GetProgressSegment;

    if (Seg = FTransitionSegment) then  // Transition Segment
    begin
      clr := Appearance.TransitionSegmentColor;
      solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
    end
    else if (Seg < FTransitionSegment) then // Inactive/Progress Segment
    begin
      if (Seg <= ProgSeg) and not FActiveBehind then   // ProgressSegment
      begin
        clr := Appearance.ProgressSegmentColor;
        solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
      end
      else
      begin
        if FActiveBehind then
        begin
          clr := Appearance.ActiveSegmentColor;
          solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
        end
        else
        begin
          clr := Appearance.InActiveSegmentColor;
          solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
        end;
      end;
    end
    else
    begin
      if FActiveBehind then
      begin
        if (Seg <= ProgSeg) then   // ProgressSegment
        begin
          clr := Appearance.ProgressSegmentColor;
          solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
        end
        else
        begin
          clr := Appearance.InActiveSegmentColor;
          solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
        end;
      end
      else
      begin
        clr := Appearance.ActiveSegmentColor;
        solidBrush := TGPSolidBrush.Create(ColorToARGB(clr));
      end;
    end;
    if (clr <> clNone) then
      graphics.FillPath(solidbrush, FSegPath[Seg]);
    solidbrush.Free;
  end;  
end;

procedure TAdvCircularProgress.DrawSegments;
var
  i: Integer;
  graphics: TGPGraphics;
begin
  graphics := TGPGraphics.Create(Canvas.Handle);
  
  if Assigned(FInnerCircleRgn) then
  begin
    graphics.ExcludeClip(FInnerCircleRgn);
  end;

  graphics.SetSmoothingMode(SmoothingModeAntiAlias);
  
  // Create segment pieces
  //j := Math.Max(1, FTransitionSegment-1);
  for i := 1 to SEGMENT_COUNT do
  begin
    {if FClipDraw then
    begin
      if not (i in [FTransitionSegment, j, 12]) then
        Continue;
    end;
    }
    DrawSegment(i, graphics);
  end;
  graphics.free;

  if FClipDraw then
    FClipDraw := False;
end;

procedure TAdvCircularProgress.Paint;
begin
  //inherited;
  {bmp := TBitmap.Create;
  bmp.Width := Width;
  bmp.Height := Height;
  bmp.Canvas.CopyRect(Rect(0, 0, bmp.Width, bmp.Height), Canvas, Rect(0, 0, bmp.Width, bmp.Height));
  }
  if (Appearance.BackGroundColor <> clNone) then
  begin
    Canvas.Brush.Color := Appearance.BackGroundColor;
    Canvas.Pen.Color := Appearance.BackGroundColor;
    Canvas.Rectangle(ClientRect);
  end;

  if (Appearance.BorderColor <> clNone) then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Color := Appearance.BorderColor;
    Canvas.Rectangle(ClientRect);
  end;

  {if FClipDraw and (FTransitionSegment >= 1) then
  begin
    R := GetMyClientRect;
    rectf := MakeRect(R.Left,R.Top,R.Right,R.Bottom);
    GPRgn := TGPRegion.Create(rectf);
    GPRgn.Exclude(FSegPath[FTransitionSegment]);
    i := Math.Max(1, FTransitionSegment-1);
    GPRgn.Exclude(FSegPath[i]);
    graphics.ExcludeClip(GPRgn);
    GPRgn.Free;
    FClipDraw := False;
  end;}
  
  DrawSegments;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
  CalculateSegmentSize;
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetVersion(const Value: string);
begin

end;

procedure TAdvCircularProgress.StepBy(delta: integer);
begin
  Position := Math.Min(Max, Position + delta);
end;

procedure TAdvCircularProgress.Stepit;
begin
  Position := Math.Min(Max, Position + FStep);
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetAppearance(const Value: TProgressAppearance);
begin
  FAppearance.Assign(Value);
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.OnAppearanceChanged(Sender: TObject);
begin
  Invalidate;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.WMSize(var Message: TWMSize);
begin
  inherited;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.OnTimer(Sender: TObject);
begin
  if Enabled then
    IncreaseByOne;
  if Assigned(FOnProgressUpdate) then
  begin
    FOnProgressUpdate(Self);
  end;
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetInterval: Cardinal;
begin
  if Assigned(FTimer) then
  begin
    Result := FTimer.Interval;
  end
  else
  begin
    Result := 0;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetInterval(const Value: Cardinal);
begin
  if Assigned(FTimer) and (FTimer.Interval <> Value) then
  begin
    FTimer.Interval := Value;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.IncreaseByOne;
var
  r: TRect;
begin
  if (FTransitionSegment >= SEGMENT_COUNT) then
  begin
    FActiveBehind := not FActiveBehind;
    FTransitionSegment := 1;
  end
  else
  begin
    Inc(FTransitionSegment);
  end;

  FClipDraw := True;

  R := ClientRect;

  OffsetRect(R,Left,Top);

  case FTransitionSegment of
  1:
    begin
      R.Bottom := R.Top + (R.Bottom - R.Top) div 2;
    end;
  2..3:
    begin
      R.Left := R.Left + (R.Right - R.Left) div 2;
      R.Bottom := R.Top + (R.Bottom - R.Top) div 2;
    end;
  4:
    begin
      R.Left := R.Left + (R.Right - R.Left) div 2;
    end;
  5..6:
    begin
      R.Left := R.Left + (R.Right - R.Left) div 2;
      R.Top := R.Top + (R.Bottom - R.Top) div 2;
    end;
  7:
    begin
      R.Top := R.Top + (R.Bottom - R.Top) div 2;
    end;
  8..9:
    begin
      R.Right := R.Right - (R.Right - R.Left) div 2 + 4;
      R.Top := R.Top + (R.Bottom - R.Top) div 2;
    end;
  10:
    begin
      R.Right := R.Right - (R.Right - R.Left) div 2;
    end;
  11..12:
    begin
      R.Right := R.Right - (R.Right - R.Left) div 2;
      R.Bottom := R.Top + (R.Bottom - R.Top) div 2 + 4;
    end;
  end;

  InvalidateRect(Parent.Handle,@r, true);
end;

//------------------------------------------------------------------------------


procedure TAdvCircularProgress.CalculateSegmentSize;
var
  i, j, k: Integer;
  R: TRect;
  rectf: TGPRectF;
  Path: TGPGraphicsPath;
begin
  ClearSegmentSize;

  k := Math.Max(Width, Height);
  if (K < 40) then
  begin
    j := 22;
  end
  else if (k < 60) then
  begin
    j := 23;
  end
  else if (k < 100) then
  begin
    j := 24;
  end
  else if (k < 200) then
  begin
    j := 25;
  end
  else
  begin
    j := 26;
  end;

  //--- Segments
  R := GetMyClientRect;
  rectf := MakeRect(R.Left,R.Top,R.Right,R.Bottom);

  for i := 1 to SEGMENT_COUNT do
  begin
    if (FSegPath[i] = nil) then
    begin
      FSegPath[i] := TGPGraphicsPath.Create;
    end;
    FSegPath[i].AddPie(rectf, ((i - 1) * 30) - 90, j);
  end;

  //--- Inner Circle
  R := GetInnerCircleRect;
  rectf := MakeRect(R.Left,R.Top,R.Right,R.Bottom);
  Path := TGPGraphicsPath.Create;
  Path.AddPie(rectf, 0, 360);
  FInnerCircleRgn := TGPRegion.Create(Path);
  Path.Free;
end;


//------------------------------------------------------------------------------

procedure TAdvCircularProgress.ClearSegmentSize;
var
  i: Integer;
begin
  for i := 1 to SEGMENT_COUNT do
  begin
    if (FSegPath[i] <> nil) then
    begin
      FSegPath[i].Free;
    end;
    FSegPath[i] := nil;
  end;

  if (FInnerCircleRgn <> nil) then
  begin
    FInnerCircleRgn.Free;
    FInnerCircleRgn := nil;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetInnerCircleRect: TRect;
var
  w, h: Integer;
  R: TRect;
begin
  R := GetMyClientRect;
  w := (R.Right - R.Left);
  h := (R.Bottom - R.Top);
  Result := Rect(Round((W *  7) / 30),Round((H *  7) / 30), Trunc(W -  ((W *  14 ) / 30 )), Trunc(H - ((H * 14) / 30 )));
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetMyClientRect: TRect;
begin
  if (Appearance.BorderColor <> clNone) then
  begin
    Result := Rect(0, 0, Width-1, Height-1);
  end
  else
  begin
    Result := Rect(0, 0, Width, Height);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetMax(const Value: Integer);
begin
  if (FMax <> Value) and (Value >= 0) then
  begin
    FMax := Value;
    if (FMax < Position) then
    begin
      Position := FMax;
    end;
    Invalidate;    
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetMin(const Value: Integer);
begin
  if (FMin <> Value) and (Value >= 0) and (Value <= FMax) then
  begin
    FMin := Value;
    if (FMin > Position) then
    begin
      Position := FMin;
    end;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.SetPosition(const Value: Integer);
begin
  if (Value >= Min) and (Value <= Max) then
  begin
    FPosition := Value;
    FTransitionSegment := Value;
    FActiveBehind := true;
    Invalidate;
  end;
end;

//------------------------------------------------------------------------------

function TAdvCircularProgress.GetProgressSegment: Integer;
var
  f: single;
begin
  Result := -1;
  if (Position = Max) then
  begin
    Result := 12;
  end
  else if (Position > Min) then
  begin
    f := (Max - Min) / SEGMENT_COUNT;
    f := Position / f;
    Result := Trunc(f);
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.WaitForComplete;
begin
  FTimer.Enabled := false;
  while FTransitionSegment < SEGMENT_COUNT do
  begin
    Sleep(FTimer.Interval);
    IncreaseByOne;
    Application.ProcessMessages;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvCircularProgress.WMEraseBkGnd(var Msg: TMessage);
begin
  Msg.Result := 1;
end;


end.

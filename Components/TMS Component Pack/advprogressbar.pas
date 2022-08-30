{***************************************************************************}
{ TADVPROGRESSBAR component                                                 }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by                                                                }
{       TMS Software                                                        }
{       copyright © 2003 - 2015                                             }
{       Email : info@tmssoftware.com                                        }
{       Web : http://www.tmssoftware.com                                    }
{                                                                           }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit AdvProgressBar;

interface
{$I TMSDEFS.INC}

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls,
  Forms, Dialogs, ExtCtrls, Math
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 1; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.1.1.0 : exposed standard events OnMouseDown, OnMouseUp, OnMouseMove
  // 1.1.2.0 : added property InfiniteInterval
  // 1.1.2.1 : Fixed issue with display with Format
  // 1.1.2.2 : Fixed issue with stacked level colors
  // 1.1.2.3 : Improved : painting performance
  // 1.1.2.4 : Fixed : issue with stacked gradient drawing
  // 1.1.2.5 : Fixed : possible div by zero in calculation
  // 1.2.0.0 : New : Level0,Level1,Level2 background colors
  // 1.2.0.1 : Fixed : update issue when position changes for certain values
  // 1.2.0.2 : Improved : Painting of last segment
  // 1.2.1.0 : New : Delphi XE5 & C++Builder XE5 support
  // 1.2.1.1 : Fixed : Painting in stacked mode

type
  TGaugeOrientation = (goHorizontal, goVertical);

  TGaugeSettings = record
    Level0Color: TColor;
    Level0ColorTo : TColor;
    Level1Color : TColor;
    Level1ColorTo : TColor;
    Level2Color : TColor;
    Level2ColorTo : TColor;
    Level3Color : TColor;
    Level3ColorTo : TColor;
    Level1Perc : Integer;
    Level2Perc : Integer;
    BorderColor : TColor;
    Level0BkColor : TColor;
    Level1BkColor : TColor;
    Level2BkColor : TColor;
    ShowBorder : Boolean;
    Stacked : Boolean;
    ShowPercentage : Boolean;
    ShowPosition: Boolean;
    Font : TFont;
    CompletionSmooth : Boolean;
    ShowGradient : Boolean;
    Steps : Integer;
    Position : Integer;
    Value: Integer;
    Max: Integer;
    BackgroundColor : TColor;
    Orientation : TGaugeOrientation;
    Rounded: Boolean;
    ParentColor: TColor;
    Format: string;
    Infinite: boolean;
    InfinitePos: Integer;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvProgressBar = class(TGraphicControl)
  private
    { Private declarations }
    FLevel0Color: TColor;
    FLevel0ColorTo : TColor;
    FLevel1Color : TColor;
    FLevel1ColorTo : TColor;
    FLevel2Color : TColor;
    FLevel2ColorTo : TColor;
    FLevel3Color : TColor;
    FLevel3ColorTo : TColor;
    FLevel1Perc : Integer;
    FLevel2Perc : Integer;
    FBorderColor : TColor;
    FShowBorder : Boolean;
    FStacked : Boolean;
    FShowPercentage : Boolean;
    FFontPercentage : TFont;
    FCompletionSmooth : Boolean;
    FShowGradient : Boolean;
    FSteps : Integer;
    FPosition : Integer;
    FBackgroundColor : TColor;
    FOrientation : TGaugeOrientation;
    FShowPosition: Boolean;
    FMin: Integer;
    FMax: Integer;
    FMaxIncrement: Integer;
    FRounded: Boolean;
    FFormat: string;
    FAnimated: Boolean;
    FInfinite: Boolean;
    FInfinitePos: Integer;
    FInfiniteInc: Boolean;
    FInfiniteInterval: integer;
    FOldBlockPos: integer;
    FOldPos: integer;
    FTimer: TTimer;
    FLevel0BkColor: TColor;
    FLevel1BkColor: TColor;
    FLevel2BkColor: TColor;
    procedure SetOrientation(Value : TGaugeOrientation);
    procedure SetBackgroundColor(Color : TColor);
    procedure SetBorderColor(Color : TColor);
    procedure SetCompletionSmooth(Value : Boolean);
    procedure SetFont(Value : TFont);
    procedure SetLevel0Color(Color : TColor);
    procedure SetLevel0ColorTo(Color : TColor);
    procedure SetLevel1Color(Color : TColor);
    procedure SetLevel1ColorTo(Color : TColor);
    procedure SetLevel2Color(Color : TColor);
    procedure SetLevel2ColorTo(Color : TColor);
    procedure SetLevel3Color(Color : TColor);
    procedure SetLevel3ColorTo(Color : TColor);
    procedure SetLevel1Perc(Percentage : Integer);
    procedure SetLevel2Perc(Percentage : Integer);
    procedure SetPosition(Value : Integer);
    procedure SetShowBorder(Value : Boolean);
    procedure SetShowGradient(Value : Boolean);
    procedure SetShowPercentage(Value : Boolean);
    procedure SetStacked(Value : Boolean);
    procedure SetSteps(Value : Integer);
    procedure FontChanged(Sender: TObject);
    procedure WMEraseBkGnd(var Message:TMessage); message WM_ERASEBKGND;
    function GetVersion: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    procedure SetShowPosition(const Value: Boolean);
    procedure SetMax(const Value: Integer);
    procedure SetMin(const Value: Integer);
    procedure SetRounded(const Value: Boolean);
    procedure SetFormat(const Value: string);
    procedure SetAnimated(const Value: Boolean);
    procedure SetInfinite(const Value: Boolean);
    procedure SetInfiniteInterval(const Value: Integer);
    procedure SetLevel0BkColor(const Value: TColor);
    procedure SetLevel1BkColor(const Value: TColor);
    procedure SetLevel2BkColor(const Value: TColor);
  protected
    { Protected declarations }
    procedure Paint; override;
    procedure TimerProc(Sender: TObject);
  public
    { Public declarations }
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
  published
    { Published declarations }
    property Align;
    property Anchors;
    property Animated: Boolean read FAnimated write SetAnimated default False;
    property Constraints;
    property BackgroundColor : TColor read FBackgroundColor write SetBackgroundColor default clWhite;
    property BorderColor : TColor read FBorderColor write SetBorderColor default clGray;
    property CompletionSmooth : Boolean read FCompletionSmooth write SetCompletionSmooth default False;
    property Font : TFont read FFontPercentage write SetFont;
    property Format: string read FFormat write SetFormat;
    property Infinite: Boolean read FInfinite write SetInfinite default False;
    property InfiniteInterval: Integer read FInfiniteInterval write SetInfiniteInterval default 50;
    property Level0Color : TColor read FLevel0Color write SetLevel0Color default clLime;
    property Level0ColorTo : TColor read FLevel0ColorTo write SetLevel0ColorTo;
    property Level1Color : TColor read FLevel1Color write SetLevel1Color default clYellow;
    property Level1ColorTo : TColor read FLevel1ColorTo write SetLevel1ColorTo;
    property Level2Color : TColor read FLevel2Color write SetLevel2Color;
    property Level2ColorTo : TColor read FLevel2ColorTo write SetLevel2ColorTo;
    property Level3Color : TColor read FLevel3Color write SetLevel3Color default clRed;
    property Level3ColorTo : TColor read FLevel3ColorTo write SetLevel3ColorTo;
    property Level1Perc : Integer read FLevel1Perc write SetLevel1Perc;
    property Level2Perc : Integer read FLevel2Perc write SetLevel2Perc;

    property Level0BkColor : TColor read FLevel0BkColor write SetLevel0BkColor default clNone;
    property Level1BkColor : TColor read FLevel1BkColor write SetLevel1BkColor default clNone;
    property Level2BkColor : TColor read FLevel2BkColor write SetLevel2BkColor default clNone;

    property Min: Integer read FMin write SetMin default 0;
    property Max: Integer read FMax write SetMax default 100;
    property Orientation : TGaugeOrientation read FOrientation write SetOrientation default goHorizontal;
    property Position : Integer read FPosition write SetPosition;
    property Rounded: Boolean read FRounded write SetRounded default true;
    property ShowBorder : Boolean read FShowBorder write SetShowBorder;
    property ShowGradient : Boolean read FShowGradient write SetShowGradient default true;
    property ShowPercentage : Boolean read FShowPercentage write SetShowPercentage default true;
    property ShowPosition: Boolean read FShowPosition write SetShowPosition default true;
    property Stacked : Boolean read FStacked write SetStacked default false;
    property Steps : Integer read FSteps write SetSteps default 8;
    property ShowHint;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnMouseDown;
    property OnMouseUp;
    property OnMouseMove;
  end;

implementation

constructor TAdvProgressBar.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLevel0Color := clLime;
  FLevel0ColorTo := $00E1FFE1;
  FLevel1Color := clYellow;
  FLevel1ColorTo := $00CAFFFF;
  FLevel2Color := $0053A9FF;
  FLevel2ColorTo := $00A8D3FF;
  FLevel3Color := clRed;
  FLevel3ColorTo := $00CACAFF;
  FLevel1Perc := 70;
  FLevel2Perc := 90;
  FBorderColor := clGray;
  FLevel0BkColor := clNone;
  FLevel1BkColor := clNone;
  FLevel2BkColor := clNone;
  FShowBorder := true;
  FStacked := false;
  FShowPercentage := true;
  FShowPosition := true;
  FFontPercentage := TFont.Create;
  FFontPercentage.Name := 'Verdana';
  FFontPercentage.Size := 10;
  FFontPercentage.OnChange := FontChanged;
  FCompletionSmooth := false;
  FShowGradient := true;
  FSteps := 8;
  FBackgroundColor := clWhite;
  FOrientation := goHorizontal;
  FPosition := 50;
  FInfinitePos := 0;
  FRounded := true;
  FMin := 0;
  FMax := 100;
  FMaxIncrement := 1;
  Width := 128;
  Height := 18;
  FInfiniteInterval := 50;
  FTimer := TTimer.Create(self);
  FTimer.Enabled := false;
  FTimer.Interval := FInfiniteInterval;
  FTimer.OnTimer := TimerProc;
end;

destructor TAdvProgressBar.Destroy;
begin
  FTimer.Free;
  FFontPercentage.Free;
  inherited;
end;

procedure DrawRectangle(Canvas: TCanvas; R : TRect; BrushColor : TColor);
begin
  Canvas.Brush.Color := BrushColor;
  Canvas.Pen.Color := BrushColor;
  Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
end;

procedure DrawGradient(Canvas: TCanvas; FromColor,ToColor: TColor; Steps: Integer;R:TRect; Direction: Boolean);
var
  diffr,startr,endr: Integer;
  diffg,startg,endg: Integer;
  diffb,startb,endb: Integer;
  iend: Integer;
  rstepr,rstepg,rstepb,rstepw: Real;
  i,stepw: Word;
begin
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
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i*rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw,R.Top,iend,R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw)+1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left,R.Top + stepw,R.Right,iend);
      end;
    end;
  end;
end;

procedure DrawGauge(Canvas: TCanvas; R : TRect; Settings : TGaugeSettings);

  procedure DrawBackGround(WidthBar: integer);
  var
    WidthPart : Integer;
    RectL, RectM, RectR: TRect;
    BarFilled : Integer;
  begin
    if (Settings.Level0BKColor = clNone) and (Settings.Level1BKColor = clNone) and (Settings.Level2BKColor = clNone) then
      Exit;
      
    WidthPart := Round((Settings.Level1Perc / 100) * WidthBar);

    //Draw first part
    if (Settings.Orientation = goHorizontal) then
    begin
      RectL.Left := R.Left + 2;
      RectL.Top := R.Top + 1;
      RectL.Right := RectL.Left + WidthPart - 2;
      RectL.Bottom := r.Bottom - 1;
    end
    else
    begin
      RectL.Left := r.Left + 1;
      RectL.Right := R.Right - 1;
      RectL.Top := R.Bottom - WidthPart + 1;
      RectL.Bottom := R.Bottom - 2;
    end;

    if (Settings.Level0BKColor <> clNone) then
      DrawRectangle(Canvas, RectL, Settings.Level0BKColor);

    BarFilled := WidthPart;

    //Draw second part
    if (Settings.Orientation = goHorizontal) then
    begin
      RectM.Left := RectL.Right;
      RectM.Top := r.Top + 1;
      RectM.Bottom := r.Bottom - 1;
    end
    else
    begin
      RectM.Left := R.Left + 1;
      RectM.Right := R.Right - 1;
      RectM.Bottom := RectL.Top;
    end;

    WidthPart := Round(WidthBar * ((Settings.Level2Perc - Settings.Level1Perc) /100));

    if (Settings.Orientation = goHorizontal) then
      RectM.Right := WidthPart + RectM.Left
    else
      RectM.Top := RectM.Bottom - WidthPart;

    if (Settings.Level1BKColor <> clNone) then
      DrawRectangle(Canvas, RectM, Settings.Level1BKColor);

    BarFilled := BarFilled + WidthPart;

    //Draw third part
    WidthPart := Round(WidthBar - BarFilled);

    if (Settings.Orientation = goHorizontal) then
    begin
      RectR.Left := RectM.Right;
      RectR.Top := R.Top + 1;
      RectR.Bottom := r.Bottom - 1;
      RectR.Right := RectR.Left + WidthPart;
    end
    else
    begin
      RectR.Left := R.Left + 1;
      RectR.Right := R.Right - 1;
      RectR.Bottom := RectM.Top - 1;
      RectR.Top := RectR.Bottom - WidthPart;
    end;

    if (Settings.Level2BKColor <> clNone) then
      DrawRectangle(Canvas,RectR, Settings.Level2BKColor);
  end;

var
  RectL : TRect;
  RectM : TRect;
  RectR : TRect;
  WidthBar : integer;
  WidthPart : Integer;
  Continue : Boolean;
  GradDir : Boolean;
  BrushColor : TColor;
  BrushColorTo : TColor;
  BarFilled : Integer;
  NumberOfBlock : Integer;
  i : Integer;
  EmptyWidth : integer;
  lf : TLogFont;
  tf : TFont;
  R1 : TRect;
  R2 : TRect;
  txt: string;
  IPW: integer;
  
begin
  if (Settings.Orientation = goHorizontal) then
    WidthBar := R.Right - R.Left
  else
    WidthBar := R.Bottom - R.Top;

  Continue := true;

  GradDir := not (Settings.Orientation = goHorizontal);

  if Settings.Rounded then
  begin
    Canvas.Brush.Color := clFuchsia;
    Canvas.Pen.Color := clFuchsia;
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
  end;

  if Settings.Infinite then
  begin
    Settings.Position := 100;
    Settings.Level1Perc := 101;
    Settings.Level2Perc := 101;
  end;

  Canvas.Brush.Color := Settings.BackgroundColor;
  //Draw Border
  if (Settings.ShowBorder) then
    Canvas.Pen.Color := Settings.BorderColor
  else
    Canvas.Pen.Color := Settings.BackgroundColor;

  if Settings.Rounded then
    Canvas.RoundRect(R.Left,R.Top,R.Right,R.Bottom,6,6)
  else
    Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);

  WidthBar := WidthBar - 2;

  DrawBackGround(WidthBar);
  
  if (Settings.Position > 0) then
  begin
    if (Settings.Stacked) then
    begin
      if (Settings.Position >= Settings.Level1Perc) then
        WidthPart := Round((Settings.Level1Perc / 100) * WidthBar)
      else
      begin
        WidthPart := Round((Settings.Position / 100) * WidthBar);
        Continue := false;
      end;

      //Draw first part
      if (Settings.Orientation = goHorizontal) then
      begin
        RectL.Left := R.Left + 1;
        RectL.Top := R.Top + 1;
        RectL.Right := RectL.Left + WidthPart;
        RectL.Bottom := r.Bottom - 1;
      end
      else
      begin
        RectL.Left := r.Left + 1;
        RectL.Right := R.Right - 1;
        RectL.Top := R.Bottom - WidthPart + 1;
        RectL.Bottom := R.Bottom - 2;
      end;

      if (Settings.ShowGradient) then
      begin
        if not (Settings.Orientation = goHorizontal) then
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
          R1.Bottom := RectL.Bottom;
          R1.Top := RectL.Top;
          R2.Left := R1.Right;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
          R2.Top := RectL.Top;
        end
        else
        begin
          R1.Left := RectL.Left;
          R1.Right := RectL.Right;
          R1.Top := RectL.Top;
          R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
          R2.Top := R1.Bottom;
          R2.Left := RectL.Left;
          R2.Right := RectL.Right;
          R2.Bottom := RectL.Bottom;
        end;

        DrawGradient(Canvas, Settings.Level0ColorTo, Settings.Level0Color, Settings.Steps,R1,GradDir);
        DrawGradient(Canvas, Settings.Level0Color, Settings.Level0ColorTo, Settings.Steps,R2,GradDir);
      end
      else
      begin
        DrawRectangle(Canvas, RectL, Settings.Level0Color);
      end;

      BarFilled := WidthPart;

      if (Continue) then
      begin
        //Draw second part
        if (Settings.Orientation = goHorizontal) then
        begin
          RectM.Left := RectL.Right;
          RectM.Top := r.Top + 1;
          RectM.Bottom := r.Bottom - 1;
        end
        else
        begin
          RectM.Left := R.Left + 1;
          RectM.Right := R.Right - 1;
          RectM.Bottom := RectL.Top;
        end;

        if (Settings.Position >= Settings.Level2Perc) then
          WidthPart := Round(WidthBar * ((Settings.Level2Perc - Settings.Level1Perc) /100))
        else
        begin
          WidthPart := Round(WidthBar * ((Settings.Position - Settings.Level1Perc) /100));
          Continue := false;
        end;

        if (Settings.Orientation = goHorizontal) then
          RectM.Right := WidthPart + RectM.Left
        else
           RectM.Top := RectM.Bottom - WidthPart;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Left + (RectM.Right - RectM.Left) div 2;
            R1.Bottom := RectM.Bottom;
            R1.Top := RectM.Top;
            R2.Left := R1.Right;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
            R2.Top := RectM.Top;
          end
          else
          begin
            R1.Left := RectM.Left;
            R1.Right := RectM.Right;
            R1.Top := RectM.Top;
            R1.Bottom := RectM.Top + (RectM.Bottom - RectM.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectM.Left;
            R2.Right := RectM.Right;
            R2.Bottom := RectM.Bottom;
          end;

          DrawGradient(Canvas, Settings.Level1ColorTo, Settings.Level1Color, Settings.Steps,R1,GradDir);
          DrawGradient(Canvas, Settings.Level1Color,Settings.Level1ColorTo, Settings.Steps,R2,GradDir);
        end
        else
        begin
          DrawRectangle(Canvas, RectM,Settings.Level1Color);
        end;

        BarFilled := BarFilled + WidthPart;

        if (Continue) then
        begin
          //Draw third part
          if (Settings.Position = 100) then
            WidthPart := Round(WidthBar - BarFilled)
          else
            WidthPart := Round(WidthBar * ((Settings.Position - Settings.Level2Perc)/ 100));

          if (Settings.Orientation = goHorizontal) then
          begin
            RectR.Left := RectM.Right;
            RectR.Top := R.Top + 1;
            RectR.Bottom := r.Bottom - 1;
            RectR.Right := RectR.Left + WidthPart;
          end
          else
          begin
            RectR.Left := R.Left + 1;
            RectR.Right := R.Right - 1;
            RectR.Bottom := RectM.Top - 1;
            RectR.Top := RectR.Bottom - WidthPart;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Left + (RectR.Right - RectR.Left) div 2;
              R1.Bottom := RectR.Bottom;
              R1.Top := RectR.Top;
              R2.Left := R1.Right;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
              R2.Top := RectR.Top;
            end
            else
            begin
              R1.Left := RectR.Left;
              R1.Right := RectR.Right;
              R1.Top := RectR.Top;
              R1.Bottom := RectR.Top + (RectR.Bottom - RectR.Top) div 2;
              R2.Top := R1.Bottom;
              R2.Left := RectR.Left;
              R2.Right := RectR.Right;
              R2.Bottom := RectR.Bottom;
            end;
            DrawGradient(Canvas, Settings.Level2ColorTo, Settings.Level2Color, Settings.Steps,R1,GradDir);
            DrawGradient(Canvas, Settings.Level2Color, Settings.Level2ColorTo, Settings.Steps,R2,GradDir);
          end
          else
          begin
            DrawRectangle(Canvas,RectR, Settings.Level2Color);
          end;
        end;
      end;
    end
    else
    begin
      if (Settings.Position < Settings.Level1Perc) then
      begin
        BrushColor := Settings.Level0Color;
        BrushColorTo := Settings.Level0ColorTo;
      end
      else
      begin
        if (Settings.Position < Settings.Level2Perc) then
        begin
          BrushColor := Settings.Level1Color;
          BrushColorTo := Settings.Level1ColorTo;
        end
        else
        begin
          if (Settings.Position < 100) then
          begin
            BrushColor := Settings.Level2Color;
            BrushColorTo := Settings.Level2ColorTo;
          end
          else
          begin
            BrushColor := Settings.Level3Color;
            BrushColorTo := Settings.Level3ColorTo;
          end;
        end;
      end;

      if not (Settings.CompletionSmooth) then
      begin
        Canvas.Brush.Color := Settings.BackgroundColor;

        if (Round((Settings.Position * WidthBar)/100) > 9) then
        begin
          if (Settings.Orientation = goHorizontal) then
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := RectL.Left + 7;
            RectL.Top := R.Top + 2;
            RectL.Bottom := R.Bottom - 2;
          end
          else
          begin
            RectL.Left := R.Left + 2;
            RectL.Right := R.Right - 2;
            RectL.Bottom := R.Bottom - 2;
            RectL.Top := RectL.Bottom - 7;
          end;

          if (Settings.ShowGradient) then
          begin
            if not (Settings.Orientation = goHorizontal) then
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
              R1.Bottom := RectL.Bottom;
              R1.Top := RectL.Top;

              R2.Left := R1.Right;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
              R2.Top := RectL.Top;
            end
            else
            begin
              R1.Left := RectL.Left;
              R1.Right := RectL.Right;
              R1.Top := RectL.Top;
              R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
              R2.Top := R1.Bottom;
              R2.Left := RectL.Left;
              R2.Right := RectL.Right;
              R2.Bottom := RectL.Bottom;
            end;
            DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
            DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
          end
          else
          begin
            DrawRectangle(Canvas, RectL, BrushColor);
          end;

          NumberOfBlock := (Round((Settings.Position * WidthBar) / 100) div 9) - 1;
          EmptyWidth := Round((Settings.Position * WidthBar) / 100) mod 9;

          for i := 0 to NumberOfBlock - 1 do
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + 7;
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := RectL.Bottom - 7;
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;

          if (EmptyWidth >= 2) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := RectL.Right + 2;
              RectL.Right := RectL.Left + (EmptyWidth - 2);
            end
            else
            begin
              RectL.Bottom := RectL.Top - 2;
              RectL.Top := Max( R.Top + 2 , RectL.Bottom - (EmptyWidth - 2));
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
          Canvas.Brush.style := bsClear;
        end
        else
        begin
          if (Round((Settings.Position * WidthBar)/100) > 1) then
          begin
            if (Settings.Orientation = goHorizontal) then
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := RectL.Left + (Round((Settings.Position * WidthBar)/100) - 1);
              RectL.Top := R.Top + 2;
              RectL.Bottom := R.Bottom - 2;
            end
            else
            begin
              RectL.Left := R.Left + 2;
              RectL.Right := R.Right - 2;
              RectL.Bottom := R.Bottom - 2;
              RectL.Top := RectL.Bottom - (Round((Settings.Position * WidthBar)/100) - 1);
            end;

            if (Settings.ShowGradient) then
            begin
              if not (Settings.Orientation = goHorizontal) then
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
                R1.Bottom := RectL.Bottom;
                R1.Top := RectL.Top;
                R2.Left := R1.Right;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
                R2.Top := RectL.Top;
              end
              else
              begin
                R1.Left := RectL.Left;
                R1.Right := RectL.Right;
                R1.Top := RectL.Top;
                R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
                R2.Top := R1.Bottom;
                R2.Left := RectL.Left;
                R2.Right := RectL.Right;
                R2.Bottom := RectL.Bottom;
              end;
              DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
              DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
            end
            else
              DrawRectangle(Canvas, RectL, BrushColor);
          end;
        end;
      end
      else
      begin
        WidthPart := Round((Settings.Position / 100) * WidthBar);

        if (Settings.Orientation = goHorizontal) then
        begin
          RectL.Left := R.Left + 2;
          RectL.Top := R.Top + 2;
          RectL.Right := RectL.Left + WidthPart - 2;
          RectL.Bottom := R.Bottom - 2;
        end
        else
        begin
          RectL.Left := r.Left + 2;
          RectL.Bottom := R.Bottom - 2;
          RectL.Top := RectL.Bottom - WidthPart + 2;
          RectL.Right := r.Right - 2;
        end;

        if (Settings.ShowGradient) then
        begin
          if not (Settings.Orientation = goHorizontal) then
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Left + (RectL.Right - RectL.Left) div 2;
            R1.Bottom := RectL.Bottom;
            R1.Top := RectL.Top;
            R2.Left := R1.Right;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
            R2.Top := RectL.Top;
          end
          else
          begin
            R1.Left := RectL.Left;
            R1.Right := RectL.Right;
            R1.Top := RectL.Top;
            R1.Bottom := RectL.Top + (RectL.Bottom - RectL.Top) div 2;
            R2.Top := R1.Bottom;
            R2.Left := RectL.Left;
            R2.Right := RectL.Right;
            R2.Bottom := RectL.Bottom;
          end;
          DrawGradient(Canvas, BrushColorTo, BrushColor, Settings.Steps,R1,GradDir);
          DrawGradient(Canvas, BrushColor, BrushColorTo, Settings.Steps,R2,GradDir);
        end
        else
          DrawRectangle(Canvas, RectL, BrushColor);
      end;
    end;
  end;

  if Settings.Infinite then
  begin
    if Settings.Orientation = goHorizontal then
    begin
      if R.Right - R.Left > 4 then
        IPW := Round(Settings.InfinitePos / 100 * (R.Right - R.Left - 4))
      else
        IPW := 0;

      Canvas.Brush.Color := Settings.BackgroundColor;
      Canvas.Pen.Color := Settings.BackgroundColor;
      RectL := R;
      RectL.Left := RectL.Left + 2;
      RectL.Top := RectL.Top + 1;
      RectL.Bottom := RectL.Bottom - 1;
      RectL.Right := Max(RectL.Left, RectL.Left + IPW - 12);
      Canvas.Rectangle(RectL.Left, RectL.Top, RectL.Right, RectL.Bottom);

      RectL := R;
      RectL.Right := RectL.Right - 2;
      RectL.Top := RectL.Top + 2;
      RectL.Bottom := RectL.Bottom - 2;
      RectL.Left := Min(RectL.Right, RectL.Right - (R.Right - R.Left - 4 - IPW) + 10);
      Canvas.Rectangle(RectL.Left, RectL.Top, RectL.Right, RectL.Bottom);
    end
    else
    begin
      if R.Bottom - R.Top > 4 then
        IPW := Round(Settings.InfinitePos / 100 * (R.Bottom - R.Top - 4))
      else
        IPW := 0;

      Canvas.Brush.Color := Settings.BackgroundColor;
      Canvas.Pen.Color := Settings.BackgroundColor;
      RectL := R;
      RectL.Left := RectL.Left + 2;
      RectL.Top := RectL.Top + 2;
      RectL.Right := RectL.Right - 2;
      RectL.Bottom := Max(RectL.Left, RectL.Top + IPW - 12);
      Canvas.Rectangle(RectL.Left, RectL.Top, RectL.Right, RectL.Bottom);

      RectL := R;
      RectL.Right := RectL.Right - 2;
      RectL.Left := RectL.Left + 2;
      RectL.Bottom := RectL.Bottom - 2;
      RectL.Top := Min(RectL.Bottom, RectL.Bottom - (R.Bottom - R.Top - 4 - IPW) + 10);
      Canvas.Rectangle(RectL.Left, RectL.Top, RectL.Right, RectL.Bottom);
    end;
  end;

  //Draw text with PositionPercentage
  if Settings.ShowPosition then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Assign(Settings.Font);

    if Settings.ShowPercentage then
      txt := IntToStr(Settings.Position) + '%'
    else
      txt := IntToStr(Settings.Value);

    if Settings.Format <> '' then
      txt := Format(Settings.Format,[Settings.Value, Settings.Max]);

    if not (Settings.Orientation = goHorizontal) then
    begin
      tf := TFont.Create;
      try
        tf.Assign(Settings.Font);
        GetObject(tf.Handle, sizeof(lf),@lf);
        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
        Canvas.Font.Assign(tf);
        Canvas.TextOut(R.Left + ((R.Right - R.Left) div 2 - (Canvas.TextHeight(txt) div 2)),R.Top + ((R.Bottom - R.Top) div 2) + Canvas.TextWidth(txt) div 2 ,txt);
      finally
        tf.Free;
      end;
    end
    else
    begin
      Canvas.TextOut(((R.Right - R.Left) div 2) - (Canvas.TextWidth(txt) div 2 ) + r.Left, r.Top + ((R.Bottom - R.Top) div 2) - Canvas.TextHeight(txt) div 2,txt);
    end;
  end;
end;

procedure TAdvProgressBar.FontChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TAdvProgressBar.Paint;
var
  R : TRect;
  Settings : TGaugeSettings;
  bmp: TBitmap;
begin
  if not ((Visible or (csDesigning in ComponentState)) and
          Assigned(parent)) then
    Exit;

  r := ClientRect;

  Settings.Level0Color := Level0Color;
  Settings.Level0ColorTo := Level0ColorTo;
  Settings.Level1Color := Level1Color;
  Settings.Level1ColorTo := Level1ColorTo;
  Settings.Level2Color := Level2Color;
  Settings.Level2ColorTo := Level2ColorTo;
  Settings.Level3Color := Level3Color;
  Settings.Level3ColorTo := Level3ColorTo;
  Settings.Level1Perc := Level1Perc;
  Settings.Level2Perc := Level2Perc;
  Settings.BorderColor := BorderColor;
  Settings.Level0BkColor := Level0BkColor;
  Settings.Level1BkColor := Level1BkColor;
  Settings.Level2BkColor := Level2BkColor;
  Settings.ShowBorder := ShowBorder;
  Settings.Stacked := Stacked;
  Settings.ShowPercentage := ShowPercentage;
  Settings.ShowPosition := ShowPosition;
  Settings.Font := TFont.Create;
  Settings.Font.Assign(FFontPercentage);
  Settings.CompletionSmooth := CompletionSmooth;
  Settings.ShowGradient := ShowGradient;
  Settings.Steps := Steps;
  Settings.Format := Format;

  if (Min <> Max) then
    Settings.Position := round(Position / (Max - Min) * 100)
  else
    Settings.Position := 0;

  Settings.Value := Position;  

  Settings.BackgroundColor := BackgroundColor;
  Settings.Orientation := Orientation;
  Settings.Rounded := Rounded;
  Settings.Max := Max;

  Settings.Infinite := Infinite;
  Settings.InfinitePos := FInfinitePos;

  bmp := TBitmap.Create;

  try
    bmp.Width := r.Right - r.Left;
    bmp.Height := r.Bottom - r.Top;

    DrawGauge(bmp.Canvas,R,Settings);

    if Settings.Rounded then
    begin
      bmp.TransparentColor := clFuchsia;
      bmp.TransparentMode := tmAuto;
      bmp.Transparent := true;
    end;

    Canvas.Draw(0,0,bmp);
  finally
    Settings.Font.Free;
    bmp.Free;
  end;
end;

procedure TAdvProgressBar.SetBackgroundColor(Color : TColor);
begin
  FBackgroundColor := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetBorderColor(Color: TColor);
begin
  FBorderColor := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetCompletionSmooth(Value: Boolean);
begin
  FCompletionSmooth := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetFont(Value: TFont);
begin
  FFontPercentage.Assign(Value);
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel0Color(Color: TColor);
begin
  FLevel0Color := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel0ColorTo(Color: TColor);
begin
  FLevel0ColorTo := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel1Color(Color: TColor);
begin
  FLevel1Color := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel1ColorTo(Color: TColor);
begin
  FLevel1ColorTo := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel1Perc(Percentage: Integer);
begin
  FLevel1Perc := Percentage;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel2Color(Color: TColor);
begin
  FLevel2Color := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel2ColorTo(Color: TColor);
begin
  FLevel2ColorTo := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel2Perc(Percentage: Integer);
begin
  FLevel2Perc := Percentage;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel3Color(Color: TColor);
begin
  FLevel3Color := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetLevel3ColorTo(Color: TColor);
begin
  FLevel3ColorTo := Color;
  Invalidate;
end;

procedure TAdvProgressBar.SetOrientation(Value : TGaugeOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetPosition(Value : Integer);
var
  NewPos: integer;
begin
  if (Value >= Min) and (Value <= Max) then
  begin
    FPosition := Value;

    if (Max <> Min) then
      NewPos := round(Position / (Max - Min) * 100)
    else
      NewPos := 0;

    if (FMaxIncrement = 0) or (FPosition mod FMaxIncrement = 0) or
       (FOldBlockPos <> FPosition div FMaxIncrement) or (FOldPos <> NewPos) then
      Paint;

    FOldPos := NewPos;
    if (FMaxIncrement <> 0) then
      FOldBlockPos := FPosition div FMaxIncrement;
  end
  else
  begin
    if Value < Min then
      FPosition := Min;
    if Value > Min then
      FPosition := Max;
  end;
end;

procedure TAdvProgressBar.SetShowBorder(Value: Boolean);
begin
  FShowBorder := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetShowGradient(Value: Boolean);
begin
  FShowGradient := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetShowPercentage(Value: Boolean);
begin
  FShowPercentage := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetStacked(Value: Boolean);
begin
  FStacked := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetSteps(Value: Integer);
begin
  FSteps := Value;
  Invalidate;
end;

procedure TAdvProgressBar.WMEraseBkGnd(var Message: TMessage);
begin
  Message.Result := 0;
end;

function TAdvProgressBar.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;

function TAdvProgressBar.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TAdvProgressBar.SetVersion(const Value: string);
begin

end;

procedure TAdvProgressBar.SetShowPosition(const Value: Boolean);
begin
  FShowPosition := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetMax(const Value: Integer);
begin
  FMax := Value;
  FMaxIncrement := (FMax - FMin) div 100;
  Invalidate;
end;

procedure TAdvProgressBar.SetMin(const Value: Integer);
begin
  FMin := Value;
  FMaxIncrement := (FMax - FMin) div 100;
  Invalidate;
end;

procedure TAdvProgressBar.SetRounded(const Value: Boolean);
begin
  FRounded := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetFormat(const Value: string);
begin
  FFormat := Value;
  Invalidate;
end;

procedure TAdvProgressBar.SetAnimated(const Value: Boolean);
begin
  FAnimated := Value;
  FTimer.Enabled := Infinite and Animated;
  Invalidate;
end;

procedure TAdvProgressBar.SetInfinite(const Value: Boolean);
begin
  FInfinite := Value;
  FTimer.Enabled := Infinite and Animated;
  if Value then
    FInfinitePos := 0;
  Invalidate;
end;

procedure TAdvProgressBar.SetInfiniteInterval(const Value: Integer);
begin
  FInfiniteInterval := Value;
  FTimer.Interval := Value;
end;

procedure TAdvProgressBar.TimerProc(Sender: TObject);
begin
  if FInfiniteInc then
  begin
    FInfinitePos := FInfinitePos + 4;
    if FInfinitePos >= 100 then
    begin
      FInfinitePos := 100;
      FInfiniteInc := false;
    end;
  end
  else
  begin
    FInfinitePos := FInfinitePos - 4;
    if FInfinitePos <= 0 then
    begin
      FInfinitePos := 0;
      FInfiniteInc := true;
    end;
  end;
  Paint;
end;

procedure TAdvProgressBar.SetLevel0BkColor(const Value: TColor);
begin
  if (FLevel0BkColor <> Value) then
  begin
    FLevel0BkColor := Value;
    Invalidate;
  end;
end;

procedure TAdvProgressBar.SetLevel1BkColor(const Value: TColor);
begin
  if (FLevel1BkColor <> Value) then
  begin
    FLevel1BkColor := Value;
    Invalidate;
  end;
end;

procedure TAdvProgressBar.SetLevel2BkColor(const Value: TColor);
begin
  if (FLevel2BkColor <> Value) then
  begin
    FLevel2BkColor := Value;
    Invalidate;
  end;
end;

end.

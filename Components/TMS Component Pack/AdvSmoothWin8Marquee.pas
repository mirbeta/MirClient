{**************************************************************************}
{ TAdvSmoothWin8Marquee component                                          }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written                                                                  }
{   TMS Software                                                           }
{   copyright © 2013                                                       }
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

unit AdvSmoothWin8Marquee;

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
  // v1.1.0.0 : New : Windows 10, Office 2016 styles added

type
  TAdvSmoothWin8Marquee = class;

  TWinCtrl = class(TWinControl)
  public
    procedure PaintCtrls(DC: HDC; First: TControl);
  end;

  TAdvSmoothWin8MarqueeBulletShape = (bsEllipse, bsRectangle, bsTriangle, bsCustom);

  TAdvSmoothWin8MarqueeBullets = class(TPersistent)
  private
    FOwner: TAdvSmoothWin8Marquee;
    FCount: Integer;
    FWidth: Integer;
    FHeight: Integer;
    FSpacing: Integer;
    FBorderColor: TColor;
    FColor: TColor;
    FOpacity: Byte;
    FBorderOpacity: Byte;
    FShape: TAdvSmoothWin8MarqueeBulletShape;
    procedure SetCount(const Value: Integer);
    procedure SetHeight(const Value: Integer);
    procedure SetWidth(const Value: Integer);
    procedure SetSpacing(const Value: Integer);
    procedure SetBorderColor(const Value: TColor);
    procedure SetColor(const Value: TColor);
    procedure SetBorderOpacity(const Value: Byte);
    procedure SetOpacity(const Value: Byte);
    procedure SetShape(const Value: TAdvSmoothWin8MarqueeBulletShape);
  public
    constructor Create(AOwner: TAdvSmoothWin8Marquee);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property Color: TColor read FColor write SetColor default $E16941;
    property BorderColor: TColor read FBorderColor write SetBorderColor default clNone;
    property Opacity: Byte read FOpacity write SetOpacity default 255;
    property BorderOpacity: Byte read FBorderOpacity write SetBorderOpacity default 255;
    property Count: Integer read FCount write SetCount default 6;
    property Width: Integer read FWidth write SetWidth default 10;
    property Height: Integer read FHeight write SetHeight default 10;
    property Spacing: Integer read FSpacing write SetSpacing default 3;
    property Shape: TAdvSmoothWin8MarqueeBulletShape read FShape write SetShape default bsEllipse;
  end;

  TAdvSmoothWin8MarqueeCustomBulletShape = procedure(Sender: TObject; AGraphics: TGPGraphics; ABulletIndex: Integer; ARect: TGPRectF) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TAdvSmoothWin8Marquee = class(TCustomControl, ITMSStyle, ITMSTones)
  private
    FTMSStyle: TTMSStyle;
    FDesignTime: Boolean;
    FAnimationTimer: TTimer;
    FFill: TGDIPFill;
    FBullets: TAdvSmoothWin8MarqueeBullets;
    FPosition: Single;
    FAnimationFactor: Single;
    FAnimationStep: Single;
    FAnimate: Boolean;
    FTransparent: Boolean;
    FOnCustomBulletShape: TAdvSmoothWin8MarqueeCustomBulletShape;
    FAnimationStretchFactor: Single;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure SetFill(const Value: TGDIPFill);
    procedure SetBullets(const Value: TAdvSmoothWin8MarqueeBullets);
    procedure SetPosition(const Value: Single);
    procedure SetAnimate(const Value: Boolean);
    procedure SetTransparent(const Value: Boolean);
    procedure SetAnimationStretchFactor(const Value: Single);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure FillChanged(Sender: TObject);
    procedure Changed;
    procedure DrawBullets(g: TGPGraphics);
    procedure AnimateProgress(Sender: TObject);
    procedure DrawBackGround(g: TGPGraphics);
    function GetTotalBulletWidth: Single;
  public
    property Position: Single read FPosition write SetPosition;
    procedure Step(AValue: Single);
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure Paint; override;
    procedure Resize; override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;
    procedure SetColorTones(ATones: TColorTones);
    procedure ApplyDefaultStyle;
  published
    property AnimationStretchFactor: Single read FAnimationStretchFactor write SetAnimationStretchFactor;
    property Transparent: Boolean read FTransparent write SetTransparent default False;
    property Animate: Boolean read FAnimate write SetAnimate default True;
    property AnimationFactor: Single read FAnimationFactor write FAnimationFactor;
    property AnimationStep: Single read FAnimationStep write FAnimationStep;
    property Fill: TGDIPFill read FFill write SetFill;
    property Bullets: TAdvSmoothWin8MarqueeBullets read FBullets write SetBullets;
    property OnCustomBulletShape: TAdvSmoothWin8MarqueeCustomBulletShape read FOnCustomBulletShape write FOnCustomBulletShape;

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

{ TAdvSmoothWin8Marquee }

procedure TAdvSmoothWin8Marquee.AnimateProgress(Sender: TObject);
var
  d, p, pto, pos: Single;
  doanim: Boolean;
begin
  if (csDesigning in ComponentState) then
    Exit;

  p := Position;
  pto := p + AnimationStep;
  d := Abs(p - pto) / AnimationFactor;
  pos := p;
  doanim := AnimateDouble(pos, pto, d, 0.01);
  if doanim then
  begin
    Position := pos;
    Changed;
  end;
end;

procedure TAdvSmoothWin8Marquee.ApplyDefaultStyle;
begin
  Fill.Color := clWhite;
  Fill.GradientType := gtSolid;
  Fill.BorderColor := 9470064;
  Bullets.Color := 9470064;
end;

procedure TAdvSmoothWin8Marquee.Assign(Source: TPersistent);
begin
  if Source is TAdvSmoothWin8Marquee then
  begin
    FBullets.Assign((Source as TAdvSmoothWin8Marquee).Bullets);
    FFill.Assign((Source as TAdvSmoothWin8Marquee).Fill);
    Changed;
  end;
end;

procedure TAdvSmoothWin8Marquee.Changed;
begin
  Invalidate;
end;

constructor TAdvSmoothWin8Marquee.Create(AOwner: TComponent);
begin
  inherited;
  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));


  FAnimationStretchFactor := 1.7;
  FTransparent := False;
  FAnimationFactor := 3;
  FAnimationStep := 10;
  FAnimate := True;
  FFill := TGDIPFill.Create;
  FFill.OnChange := FillChanged;
  FBullets := TAdvSmoothWin8MarqueeBullets.Create(Self);

  DoubleBuffered := True;
  Width := 400;
  Height := 20;
  FAnimationTimer := TTimer.Create(Self);
  FAnimationTimer.Interval := 10;
  FAnimationTimer.OnTimer := AnimateProgress;

  TabStop := true;

  FDesignTime := (csDesigning in ComponentState) and not
    ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  FPosition := 0;
  FAnimationTimer.Enabled := True;

  if FDesignTime then
    ApplyDefaultStyle;
end;

destructor TAdvSmoothWin8Marquee.Destroy;
begin
  FAnimationTimer.Free;
  FFill.Free;
  FBullets.Free;
  inherited;
end;

procedure TAdvSmoothWin8Marquee.DrawBackGround(g: TGPGraphics);
var
  r: TGPRectF;
begin
  if Transparent then
    Exit;

  r := MakeRect(0, 0, Width - 1, Height - 1);
  Fill.Fill(g, r);
end;

procedure TAdvSmoothWin8Marquee.DrawBullets(g: TGPGraphics);
var
  I: Integer;
  w, h: Single;
  r: TGPRectF;
  totw: Single;
  sx, mx, x: Single;
  y: Single;
  b: TGPSolidBrush;
  p: TGPPen;
  spc: Single;
  pl: TGPGraphicsPath;
begin
  spc := Bullets.Spacing;
  w := Bullets.Width;
  h := Bullets.Height;

  totw := GetTotalBulletWidth;
  totw := totw * AnimationStretchFactor;
  mx := (Width - 1 - totw) / 2;
  sx := mx;
  mx := mx + Position;
  y := (Height - 1 - h) / 2;

  b := nil;
  if Bullets.Color <> clNone then
    b := TGPSolidBrush.Create(MakeColor(Bullets.Opacity, Bullets.Color));

  p := nil;
  if Bullets.BorderColor <> clNone then
    p := TGPPen.Create(MakeColor(Bullets.BorderOpacity, Bullets.BorderColor));

  if (b = nil) and (p = nil) then
    Exit;

  for I := 0 to Bullets.Count - 1 do
  begin
    x := mx + (I * w) + (I * spc);

    if (x < sx) then
    begin
      x := x + (Abs(x - sx) * (Position / Width) * (Bullets.Count - I));
    end
    else if (x > sx + totw) then
    begin
      x := x + (Abs(x - sx - totw) * (Position / Width) * (I + 1));
    end;

    r := MakeRect(x, y, w, h);

    if (Bullets.Shape = bsCustom) then
    begin
      if Assigned(OnCustomBulletShape) then
        OnCustomBulletShape(Self, g, I, r);
    end
    else
    begin
      if (Bullets.Color <> clNone) and Assigned(b) then
      begin
        case Bullets.Shape of
          bsEllipse: g.FillEllipse(b, r);
          bsRectangle: g.FillRectangle(b, r);
          bsTriangle:
          begin
            pl := TGPGraphicsPath.Create;
            pl.AddLine(Makepoint(r.X + r.Width / 2, r.Y), MakePoint(r.X + r.Width , r.Y + r.Height));
            pl.AddLine(MakePoint(r.X + r.Width , r.Y + r.Height), MakePoint(r.X , r.Y + r.Height));
            pl.CloseFigure;
            g.FillPath(b, pl);
            pl.Free;
          end;
        end;
      end;

      if (Bullets.BorderColor <> clNone) and Assigned(p) then
      begin
        case Bullets.Shape of
          bsEllipse: g.DrawEllipse(p, r);
          bsRectangle: g.DrawRectangle(p, r);
          bsTriangle:
          begin
            pl := TGPGraphicsPath.Create;
            pl.AddLine(Makepoint(r.X + r.Width / 2, r.Y), MakePoint(r.X + r.Width , r.Y + r.Height));
            pl.AddLine(MakePoint(r.X + r.Width , r.Y + r.Height), MakePoint(r.X , r.Y + r.Height));
            pl.CloseFigure;
            g.DrawPath(p, pl);
            pl.Free;
          end;
        end;
      end;
    end;
  end;
  if Assigned(p) then
    p.Free;

  if Assigned(b) then
    b.free;
end;

procedure TAdvSmoothWin8Marquee.FillChanged(Sender: TObject);
begin
  Invalidate;
end;

function TAdvSmoothWin8Marquee.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

function TAdvSmoothWin8Marquee.GetTotalBulletWidth: Single;
var
  w, spc: Single;
begin
  w := Bullets.Width;
  spc := Bullets.Spacing;
  Result := w * Bullets.Count + (spc * Bullets.Count - 1);
end;

procedure TAdvSmoothWin8Marquee.Paint;
var
  g: TGPGraphics;
begin
  inherited;
  g := TGPGraphics.Create(Canvas.Handle);
  g.SetSmoothingMode(SmoothingModeAntiAlias);
  DrawBackGround(g);
  DrawBullets(g);
  g.Free;
end;

procedure TAdvSmoothWin8Marquee.Resize;
begin
  inherited;

end;

procedure TAdvSmoothWin8Marquee.SetAnimate(const Value: Boolean);
begin
  FAnimate := Value;
  FAnimationTimer.Enabled := FAnimate;
  Invalidate;
end;

procedure TAdvSmoothWin8Marquee.SetAnimationStretchFactor(const Value: Single);
begin
  FAnimationStretchFactor := Value;
  Changed;
end;

procedure TAdvSmoothWin8Marquee.SetBullets(
  const Value: TAdvSmoothWin8MarqueeBullets);
begin
  FBullets.Assign(Value);
  Changed;
end;

procedure TAdvSmoothWin8Marquee.SetColorTones(ATones: TColorTones);
begin
  Fill.Color :=  ATones.Background.BrushColor;
  Fill.ColorTo :=  ATones.Background.BrushColor;
  Fill.ColorMirror :=  ATones.Background.BrushColor;
  Fill.ColorMirrorTo := ATones.Background.BrushColor;
  Fill.BorderColor := ATones.Background.BorderColor;
  Bullets.BorderColor := ATones.Background.BorderColor;
  Bullets.Color := ATones.Selected.BrushColor;
end;

procedure TAdvSmoothWin8Marquee.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

procedure TAdvSmoothWin8Marquee.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TAdvSmoothWin8Marquee.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TAdvSmoothWin8Marquee.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  case AStyle of
    tsOffice2003Blue:
      begin
        Fill.Color := $00FDEADA;
        Fill.ColorTo := $00E4AE88;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $DFD2C5;
        Bullets.BorderColor := $DFD2C5;
        Bullets.Color := $94E6FB;
      end;
    tsOffice2003Silver:
      begin
        Fill.Color := $00F7F3F3;
        Fill.ColorTo := $00E6D8D8;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00927476;
        Bullets.BorderColor := $00927476;
        Bullets.Color := $94E6FB;
      end;
    tsOffice2003Olive:
      begin
        Fill.Color := $00CFF0EA;
        Fill.ColorTo := $008CC0B1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $006B7760;
        Bullets.BorderColor := $006B7760;
        Bullets.Color := $94E6FB;
      end;
    tsOffice2003Classic:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := $00C9D1D5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clGray;
        Bullets.BorderColor := clGray;
        Bullets.Color := $B59285;
      end;
    tsOffice2007Luna:
      begin
        Fill.BorderColor := $E3B28D;
        Fill.Color := $FAF1E9;
        Fill.ColorTo := $EDD8C7;
        Fill.ColorMirror := $EDD8C7;
        Fill.ColorMirrorTo := $FFF2E7;
        Fill.GradientMirrorType := gtVertical;
        Fill.GradientType := gtVertical;
        Bullets.BorderColor := $E3B28D;
        Bullets.Color := $AAD9FF;
      end;
    tsOffice2007Obsidian:
      begin
        Fill.Color := $CFC6C1;
        Fill.ColorTo := $C5BBB4;
        Fill.ColorMirror := $C5BBB4;
        Fill.ColorMirrorTo := $ECECE5;
        Fill.BorderColor := clBlack;
        Bullets.BorderColor := clBlack;
        Fill.GradientMirrorType := gtVertical;
        Bullets.Color := $AAD9FF;
      end;
    tsWindowsXP:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
        Bullets.BorderColor := clBlack;
        Bullets.Color := clInactiveCaption;
      end;
    tsWhidbey:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := $00D9E9EC;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $00828F92;
        Bullets.BorderColor := $00828F92;
        Bullets.Color := $94E6FB;
      end;
    tsCustom: ;
    tsOffice2007Silver:
      begin
        Fill.BorderColor := $74706F;
        Bullets.BorderColor := $74706F;
        Fill.Color := $F6F1EE;
        Fill.ColorTo := $E7DCD5;
        Fill.ColorMirror := $E7DCD5;
        Fill.ColorMirrorTo := $F4F4EE;
        Fill.GradientMirrorType := gtVertical;
        Bullets.Color := $AAD9FF;
      end;
    tsWindowsVista:
      begin
        Fill.Color := $FDF8F1;
        Fill.ColorTo := $FCEFD5;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $FDDE99;
        Bullets.BorderColor := $FDDE99;
        Bullets.Color := $FEF9F0;
      end;
    tsWindows7:
      begin
        Fill.Color := $FCEBDC;
        Fill.ColorTo := $FCDBC1;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $CEA27D;
        Bullets.BorderColor := $CEA27D;
        Bullets.Color := $FCEBDC;
      end;
    tsTerminal:
      begin
        Fill.Color := clBtnFace;
        Fill.ColorTo := clBtnFace;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := clBlack;
        Bullets.BorderColor := clBlack;
        Bullets.Color := clHighLight;
      end;
      tsOffice2010Blue:
      begin
        Fill.Color := $FDF6EF;
        Fill.ColorTo := $F0DAC7;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C7B29F;
        Bullets.BorderColor := $C7B29F;
        Bullets.Color := $6CD0FF;
      end;
      tsOffice2010Silver:
      begin
        Fill.Color := $FFFFFF;
        Fill.ColorTo := $EDE5E0;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D2CDC8;
        Bullets.BorderColor := $D2CDC8;
        Bullets.Color := $6CD0FF;
      end;
      tsOffice2010Black:
      begin
        Fill.Color := $BFBFBF;
        Fill.ColorTo := $919191;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $6D6D6D;
        Bullets.BorderColor := $6D6D6D;
        Bullets.Color := $6CD0FF;
      end;
      tsWindows8, tsWindows10:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $E4E3E2;
        Bullets.BorderColor := $E4E3E2;
        Bullets.Color := $F7E0C9;
      end;
    tsOffice2013White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
        Bullets.BorderColor := $D4D4D4;
        Bullets.Color := $FCE2C8;
      end;
    tsOffice2013LightGray:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $C6C6C6;
        Bullets.BorderColor := $C6C6C6;
        Bullets.Color := $FCE2C8;
      end;
    tsOffice2013Gray:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $ABABAB;
        Bullets.BorderColor := $ABABAB;
        Bullets.Color := $FCE2C8;
      end;
    tsOffice2016White:
      begin
        Fill.Color := clWhite;
        Fill.ColorTo := clWhite;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $D4D4D4;
        Bullets.BorderColor := $F2E1D5;
        Bullets.Color := $F2E1D5;
      end;
    tsOffice2016Gray:
      begin
        Fill.Color := $B2B2B2;
        Fill.ColorTo := $B2B2B2;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
        Bullets.BorderColor := $F2E1D5;
        Bullets.Color := $F2E1D5;
      end;
    tsOffice2016Black:
      begin
        Fill.Color := $363636;
        Fill.ColorTo := $363636;
        Fill.ColorMirror := clNone;
        Fill.ColorMirrorTo := clNone;
        Fill.BorderColor := $444444;
        Bullets.BorderColor := $6A6A6A;
        Bullets.Color := $6A6A6A;
      end;
  end;

end;

procedure TAdvSmoothWin8Marquee.SetFill(const Value: TGDIPFill);
begin
  FFill := Value;
end;

procedure TAdvSmoothWin8Marquee.SetPosition(const Value: Single);
var
  totw: Single;
begin
  FPosition := Value;
  totw := GetTotalBulletWidth;
  if FPosition > (Width / 2) + totw / 2 then
    FPosition := -Width / 2 - totw / 2;
  Invalidate;
end;

procedure TAdvSmoothWin8Marquee.SetTransparent(const Value: Boolean);
begin
  FTransparent := Value;
  Changed;
end;

procedure TAdvSmoothWin8Marquee.Step(AValue: Single);
begin
  Position := Position + AValue;
end;

procedure TAdvSmoothWin8Marquee.WMPaint(var Message: TWMPaint);
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

{ TWinCtrl }

procedure TWinCtrl.PaintCtrls(DC: HDC; First: TControl);
begin
  inherited PaintControls(DC, First);
end;

{ TAdvSmoothWin8MarqueeBullets }

procedure TAdvSmoothWin8MarqueeBullets.Assign(Source: TPersistent);
begin
  if (Source is TAdvSmoothWin8MarqueeBullets) then
  begin
    FCount := (Source as TAdvSmoothWin8MarqueeBullets).Count;
    FWidth := (Source as TAdvSmoothWin8MarqueeBullets).Width;
    FHeight := (Source as TAdvSmoothWin8MarqueeBullets).Height;
    FSpacing := (Source as TAdvSmoothWin8MarqueeBullets).Spacing;
    FColor := (Source as TAdvSmoothWin8MarqueeBullets).Color;
    FBorderColor := (Source as TAdvSmoothWin8MarqueeBullets).BorderColor;
    FOpacity := (Source as TAdvSmoothWin8MarqueeBullets).Opacity;
    FBorderOpacity := (Source as TAdvSmoothWin8MarqueeBullets).BorderOpacity;
    FShape := (Source as TAdvSmoothWin8MarqueeBullets).Shape;
  end;
end;

constructor TAdvSmoothWin8MarqueeBullets.Create(AOwner: TAdvSmoothWin8Marquee);
begin
  FOwner := AOwner;
  FColor := $E16941;
  FOpacity := 255;
  FBorderColor := clNone;
  FBorderOpacity := 255;
  FCount := 6;
  FShape := bsEllipse;
  FSpacing := 3;
  FWidth := 10;
  FHeight := 10;
end;

destructor TAdvSmoothWin8MarqueeBullets.Destroy;
begin
  inherited;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetBorderColor(const Value: TColor);
begin
  FBorderColor := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetBorderOpacity(const Value: Byte);
begin
  FBorderOpacity := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetColor(const Value: TColor);
begin
  FColor := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetCount(const Value: Integer);
begin
  FCount := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetHeight(const Value: Integer);
begin
  FHeight := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetOpacity(const Value: Byte);
begin
  FOpacity := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetShape(
  const Value: TAdvSmoothWin8MarqueeBulletShape);
begin
  FShape := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetSpacing(const Value: Integer);
begin
  FSpacing := Value;
  FOwner.Changed;
end;

procedure TAdvSmoothWin8MarqueeBullets.SetWidth(const Value: Integer);
begin
  FWidth := Value;
  FOwner.Changed;
end;

end.

{*********************************************************************}
{ TGradientLabel component                                            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2001 - 2015                                            }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{*********************************************************************}

unit GradientLabel;

{$I TMSDEFS.INC}

interface

uses
  Windows, SysUtils, Classes, Graphics, StdCtrls
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}
  ;

const
  MAJ_VER = 1; // Major version nr.
  MIN_VER = 2; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 1; // Build nr.

  // version history
  // 1.1.1.0 : New : GradientDirection property added
  // 1.2.0.0 : New : gtCenterLineNoOverlap gradienttype
  // 1.2.0.1 : Fixed : Issue with AutoSize

type
  TVAlignment = (vaTop,vaCenter,vaBottom);

  TEllipsType = (etNone, etEndEllips, etPathEllips);

  TGradientType = (gtFullHorizontal, gtFullVertical, gtBottomLine, gtCenterLine, gtTopLine);

  TGradientOrientation = (goHorizontal, goVertical);

  TGradientDirection = (gdLeftToRight, gdRightToLeft);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TGradientLabel = class(TLabel)
  private
    { Private declarations }
    FColorTo: TColor;
    FEllipsType: TEllipsType;
    FValignment: TVAlignment;
    FIndent: Integer;
    FGradientType: TGradientType;
    FTransparentText: Boolean;
    FLineWidth: Integer;
    FOrientation : TGradientOrientation;
    FGradientDirection: TGradientDirection;
    procedure SetOrientation(const Value: TGradientOrientation);
    procedure SetColor(const Value: TColor);
    procedure SetEllipsType(const Value: TEllipsType);
    procedure SetVAlignment(const Value: TVAlignment);
    procedure SetIndent(const Value: Integer);
    procedure SetGradientType(const Value: TGradientType);
    procedure SetTransparentText(const Value: Boolean);
    procedure SetLineWidth(const Value: Integer);
    procedure SetGradientDirection(const Value: TGradientDirection);
  protected
    { Protected declarations }
    procedure Paint; override;
    function GetVersionComp: string;
    procedure SetVersion(const Value: string);
    function GetVersionNr: Integer;
    function GetColor: TColor;
    function GetColorTo: TColor;
  public
    { Public declarations }
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
  published
    { Published declarations }
    property ColorTo: TColor read FColorTo write SetColor default clWhite;
    property EllipsType: TEllipsType read FEllipsType write SetEllipsType;
    property GradientType: TGradientType read FGradientType write SetGradientType;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection;
    property Indent: Integer read FIndent write SetIndent;
    property LineWidth: Integer read FLineWidth write SetLineWidth default 2;
    property Orientation : TGradientOrientation read FOrientation write SetOrientation;
    property TransparentText: Boolean read FTransparentText write SetTransparentText;
    property VAlignment: TVAlignment read FValignment write SetVAlignment;
    property Version: string read GetVersionComp write SetVersion;
    property Transparent default False;
  end;

implementation

uses
  Controls;

const
  ALIGNSTYLE : array[TAlignment] of DWORD = (DT_LEFT, DT_RIGHT, DT_CENTER);
  WORDWRAPSTYLE : array[Boolean] of DWORD = (DT_SINGLELINE, DT_WORDBREAK);
  LAYOUTSTYLE : array[TTextLayout] of DWORD = (0,DT_VCENTER,DT_BOTTOM);
  ELLIPSSTYLE : array[TEllipsType] of DWORD = (0,DT_END_ELLIPSIS,DT_PATH_ELLIPSIS);
  ACCELSTYLE : array[Boolean] of DWORD = (DT_NOPREFIX,0);
  VALIGNSTYlE : array[TVAlignment] of DWORD = (DT_TOP,DT_VCENTER,DT_BOTTOM);

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
      endr := startr + Round(rstepr*i);
      endg := startg + Round(rstepg*i);
      endb := startb + Round(rstepb*i);
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

{ TGradientLabel }

constructor TGradientLabel.Create(AOwner: TComponent);
begin
  inherited;
  FLineWidth := 2;
  FColorTo := clWhite;
  Transparent := False;
end;

{$WARNINGS OFF}
procedure TGradientLabel.Loaded;
begin
  inherited;
end;

procedure TGradientLabel.Paint;
var
  R,LR: TRect;
  DrawStyle: DWORD;
  tw: Integer;
  gsteps: Integer;
  rgn1,rgn2,rgn3: THandle;

  lf : TLogFont;
  tf : TFont;

  LeftCoord : Integer;
  TopCoord : Integer;
begin
  R := GetClientRect;

  Canvas.Font := Font;

  DrawStyle := DT_LEFT or DT_EXPANDTABS;
  DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle or DT_CALCRECT, nil);

  tw := R.Right - R.Left;

  R := GetClientRect;

  if GradientType = gtFullVertical  then
    gsteps := (R.Bottom - R.Top) div 4
  else
    gsteps := (R.Right - R.Left) div 4;

  if gsteps < 32 then
    gsteps := 32;

  if not Transparent and (GradientType in [gtFullHorizontal,gtFullVertical]) then
  begin
    if (ColorTo <> clNone) then
    begin
      DrawGradient(Canvas,GetColor,GetColorTo,gsteps,R,GradientType = gtFullHorizontal);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.Pen.Color := Color;
      Canvas.Rectangle(R.Left,R.Top,R.Right,R.Bottom);
    end;
  end;

  if (Orientation = goHorizontal) then
  begin
    case GradientType of
    gtBottomLine: R.Top := R.Bottom - LineWidth;
    gtTopLine: R.Bottom := R.Top + LineWidth;
    gtCenterLine:
      begin
        R.Top := R.Top + (R.Bottom - R.Top - LineWidth) div 2;
        R.Bottom := R.Top + LineWidth;
      end;
    end;
  end
  else
  begin
    case GradientType of
    gtBottomLine: R.Left := R.Right - LineWidth;
    gtTopLine: R.Right := R.Left + LineWidth;
    gtCenterLine:
      begin
        R.Left := R.Left + (R.Right - R.Left - LineWidth) div 2;
        R.Right := R.Left + LineWidth;
      end;
    end;
  end;

  if GradientType in [gtBottomLine, gtTopLine, gtCenterLine] then
  begin
    {clip out region}

    if TransparentText then
    begin
      LR := R;
      OffsetRect(LR,Left,Top);

      rgn3 := CreateRectRgn(LR.Left,LR.Top,LR.Right,LR.Bottom);
      case Alignment of
      taLeftJustify:
        begin
          rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Left + Indent,LR.Bottom);
          rgn2 := CreateRectRgn(LR.Left + Indent + tw,LR.Top, LR.Right,LR.Bottom);
          CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
        end;
      taRightJustify:
        begin
          rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Right - tw - Indent,LR.Bottom);
          rgn2 := CreateRectRgn(LR.Right - Indent,LR.Top, LR.Right,LR.Bottom);
          CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
        end;
      taCenter:
        begin
          rgn1 := CreateRectRgn(LR.Left,LR.Top,LR.Left + ((LR.Right - LR.Left - tw) div 2) - 2,LR.Bottom);
          rgn2 := CreateRectRgn(LR.Left + tw + ((LR.Right - LR.Left -tw) div 2),LR.Top, LR.Right,LR.Bottom);
          CombineRgn( rgn3,rgn1,rgn2, RGN_OR);
        end;
      end;

      SelectClipRgn(Canvas.Handle,rgn3);
    end;

    if (Orientation = goHorizontal) then
      DrawGradient(Canvas,GetColor,GetColorTo,gsteps,R,true)
    else
      DrawGradient(Canvas, GetColorTo, GetColor, gsteps, R, false);

    if TransparentText then
    begin
      SelectClipRgn(Canvas.Handle,0);
      DeleteObject(rgn1);
      DeleteObject(rgn2);
      DeleteObject(rgn3);
    end;
  end;

  R := GetClientRect;

  Canvas.Brush.Style := bsClear;

  DrawStyle := ALIGNSTYLE[Alignment] or WORDWRAPSTYLE[WordWrap] or DT_NOCLIP or
    LAYOUTSTYLE[Layout] or ELLIPSSTYLE[FEllipsType] or ACCELSTYLE[ShowAccelChar] or
    VALIGNSTYLE[VAlignment];

  DrawStyle := DrawTextBiDiModeFlags(DrawStyle);
  DrawStyle := DrawStyle or DT_EXPANDTABS;

  if (Orientation = goHorizontal) then
  begin
    if Alignment = taLeftJustify then
      R.Left := R.Left + Indent;

    if Alignment = taRightJustify then
      R.Right := R.Right - Indent;
  end
  else
  begin
//    if Layout = tlBottom then
      if Alignment = taLeftJustify then
        R.Bottom := R.Bottom - Indent;

  //  if Layout = tlTop then
     if (Alignment = taRightJustify) then
      R.Top := R.Top + Indent;
  end;

  if not Enabled then
  begin
    OffsetRect(R, 1, 1);
    Canvas.Font.Color := clBtnHighlight;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);
    OffsetRect(R, -1, -1);
    Canvas.Font.Color := clBtnShadow;
    DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);

  end
  else
  begin
    if (Orientation = goVertical) then
    begin
      case Layout of
        tlTop:LeftCoord := R.Left;
        tlCenter: LeftCoord := ((R.Right - R.Left) div 2) - Canvas.TextHeight(PChar(Caption)) div 2;
        tlBottom: LeftCoord := R.Right - Canvas.TextHeight(Pchar(Caption));
      end;
      case Alignment of
        taRightJustify : TopCoord := R.Top + Canvas.TextWidth(PChar(Caption));
        taLeftJustify : TopCoord := R.Bottom;
        taCenter: TopCoord := (R.Bottom - R.Top) div 2 + Canvas.TextWidth(PChar(Caption)) div 2;
      end;
{
      case Alignment of
        taLeftJustify:LeftCoord := R.Left;
        taCenter: LeftCoord := ((R.Right - R.Left) div 2) - Canvas.TextHeight(PChar(Caption)) div 2;
        taRightJustify: LeftCoord := R.Right - Canvas.TextHeight(Pchar(Caption));
      end;

      case Layout of
        tlTop : TopCoord := R.Top + Canvas.TextWidth(PChar(Caption));
        tlBottom : TopCoord := R.Bottom;
        tlCenter: TopCoord := (R.Bottom - R.Top) div 2 + Canvas.TextWidth(PChar(Caption)) div 2;
      end;
}
      tf := TFont.Create;
      try
        tf.Assign(Font);
        GetObject(tf.Handle, sizeof(lf),@lf);
        lf.lfEscapement := 900;
        lf.lfOrientation := 900;
        tf.Handle := CreateFontIndirect(lf);
        Canvas.Font.Assign(tf);
        Canvas.TextOut(LeftCoord,TopCoord,PChar(Caption));
//        OffsetRect(R,Canvas.TextHeight(PChar(Caption)), Canvas.TextWidth(PChar(Caption)));
//        DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DT_CALCRECT, nil);
      finally
        tf.Free;
      end;
    end
    else
      DrawTextEx(Canvas.Handle,PChar(Caption),Length(Caption),R, DrawStyle, nil);

  end;
end;
{$WARNINGS ON}

procedure TGradientLabel.SetColor(const Value: TColor);
begin
  FColorTo := Value;
  Invalidate;
end;

procedure TGradientLabel.SetEllipsType(const Value: TEllipsType);
begin
  if FEllipsType <> Value then
  begin
    FEllipsType := Value;
    Invalidate;
  end;
end;

procedure TGradientLabel.SetGradientDirection(const Value: TGradientDirection);
begin
  FGradientDirection := Value;
  invalidate;
end;

procedure TGradientLabel.SetGradientType(const Value: TGradientType);
begin
  FGradientType := Value;
  Invalidate;
end;

procedure TGradientLabel.SetIndent(const Value: Integer);
begin
  FIndent := Value;
  Invalidate;
end;

procedure TGradientLabel.SetLineWidth(const Value: Integer);
begin
  FLineWidth := Value;
  Invalidate;
end;

procedure TGradientLabel.SetOrientation(const Value: TGradientOrientation);
begin
  FOrientation := Value;
  Invalidate;
end;

procedure TGradientLabel.SetTransparentText(const Value: Boolean);
begin
  FTransparentText := Value;
  Invalidate;
end;

procedure TGradientLabel.SetVAlignment(const Value: TVAlignment);
begin
  FValignment := Value;
  Invalidate;
end;

function TGradientLabel.GetColor: TColor;
begin
  Result := Color;
  case GradientDirection of
    gdRightToLeft: Result := ColorTo;
  end;
end;

function TGradientLabel.GetColorTo: TColor;
begin
  Result := ColorTo;
  case GradientDirection of
    gdRightToLeft: Result := Color;
  end;
end;

function TGradientLabel.GetVersionComp: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)));
end;


function TGradientLabel.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

procedure TGradientLabel.SetVersion(const Value: string);
begin

end;


end.

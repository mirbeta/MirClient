unit uGameEngine;

interface

{$J+}
uses
  Windows, Classes, SysUtils, PXL.Textures, Graphics, PXL.Canvas, PXL.Types,
  PngImage, Math, uCommon;

const
  TransparentBlendingEff: array [Boolean] of TBlendingEffect = (TBlendingEffect.None, TBlendingEffect.Normal);
  FontBorderColor = $00080808;

type
  PBGR = ^TBGR;
  TBGR = packed record
    B, G, R: Byte;
  end;
  TRGBA = packed record
    R: Byte;
    G: Byte;
    B: Byte;
    A: Byte;
  end;

  TARGB = packed record
    A: Byte;
    R: Byte;
    G: Byte;
    B: Byte;
  end;

  TReversalKind = (rkNone, rkHor, rkVer);
  TCustomCanvasHelper = class helper for TCustomCanvas
  public
    procedure Draw(SrcRect, DstRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload;
    procedure Draw(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure Draw(X, Y: Integer; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;

    procedure Draw(X, Y: Integer; Texture: TCustomBaseTexture; Effect: TBlendingEffect; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure Draw(X, Y: Integer; Texture: TCustomBaseTexture; Transparent: Boolean = True; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure Draw(X, Y: Integer; SourceRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean = True; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawColor(X, Y: Integer; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean = True; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawColor(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean = True; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawBlend(X, Y: Integer; Texture: TCustomBaseTexture; Blendmode: Integer; ReversalKind: TReversalKind = rkNone); inline;
    procedure DrawBlendEffect(X, Y: Integer; Texture: TCustomBaseTexture; Blendmode: Integer; ReversalKind: TReversalKind = rkNone); inline;
    procedure DrawBlendAdd(X, Y: Integer; Texture: TCustomBaseTexture; Blendmode, tNum: Integer; ReversalKind: TReversalKind = rkNone); inline;
    procedure DrawAlpha(X, Y: Integer; Texture: TCustomBaseTexture; Alpha: Byte; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawAlpha(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Alpha: Byte; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure FillRectAlpha(const DestRect: TIntRect; Color: TColor; Alpha: Integer); inline;
    procedure DrawColorAlpha(const X, Y: Integer; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; Alpha: Integer; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawColorAlpha(const X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; Alpha: Integer; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawInRect(const X, Y: Integer; DstRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload;
    procedure HorFillDraw(const X1, X2, Y: Integer; Source: TCustomBaseTexture; const Transparent: Boolean = True);
    procedure VerFillDraw(const X, Y1, Y2: Integer; Source: TCustomBaseTexture; const Transparent: Boolean = True);

    procedure StretchDraw(const DestRect, SrcRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean = True); overload;
    procedure StretchDraw(const DestRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean = True); overload;
    procedure DrawTiled(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawTiled(SrcRect, DstRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect = TBlendingEffect.Normal; ReversalKind: TReversalKind = rkNone); overload; inline;
    procedure DrawTiled(X, Y: Integer; SourceRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean = True; ReversalKind: TReversalKind = rkNone); overload; inline;

    procedure ShadowTextOut(X, Y: Integer; const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles; FontSize: Integer); overload;
    procedure ShadowTextOut(X, Y: Integer; const Text: String; FontColor, BgColor: TColor); overload;
    procedure BoldText(X, Y: Integer; const Text: String; FontColor, BgColor: TColor); overload;
    procedure BoldText(const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles; X, Y: Integer); overload;
    procedure BoldText(const Text: String; FontColor, BgColor: TColor; X, Y: Integer); overload;
    procedure BoldTextOut(X, Y: Integer; const Text: String; FontColor, BgColor: TColor); overload;
    procedure BoldTextOut(X, Y: Integer;  FontColor, BgColor: TColor;Text: String); overload;
    procedure BoldTextOut(const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles; X, Y: Integer); overload;
    procedure BoldTextOut(const Text: String; FontColor, BgColor: TColor; X, Y: Integer); overload;
    procedure BoldTextOut(X, Y: Integer;const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles;FontSize: Integer); overload;
    procedure BoldTextOut(X, Y: Integer;const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles;FontSize: Integer;FontName: String); overload;
    procedure BoldTextOutZ( X, Y, Z, fcolor, bcolor: Integer; Str: string);
    procedure RectText(X, Y: Integer; const Text: String; FColor, BColor: TColor; DstRect: TIntRect; FontStyles: TFontStyles; FontSize: Integer; Transparent: Boolean = True);
    procedure TextOut(X, Y: Integer; const Text: String; FontColor: TColor = clWhite; Effect: TBlendingEffect = TBlendingEffect.Normal); overload;
    procedure TextOut(X, Y: Integer; const Text: string; FontColor: TColor; FontStyles: TFontStyles; FontSize: Integer; Effect: TBlendingEffect = TBlendingEffect.Normal); overload;
    procedure DrawShadowText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Transparent: Boolean = True); overload; inline;
    procedure DrawBoldText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Transparent: Boolean = True); overload; inline;
    procedure DrawOutLineTextInRect(X, Y: Integer; DstRect:TIntRect; Text: TCustomBaseTexture; FontColor, BgColor: TColor; OutLinePixel : Integer = 1;  Transparent: Boolean = true);{$IFNDEF DEBUG}inline; {$ENDIF}
    procedure DrawOutLineText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; OutLinePixel : Integer = 1; Transparent: Boolean = True); {$IFNDEF DEBUG}inline; {$ENDIF}
    procedure DrawBoldTextInRect(X, Y: Integer; DstRect: TIntRect; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Effect: TBlendingEffect = TBlendingEffect.Normal); inline;
    procedure DrawText(X, Y: Integer; Text: TCustomBaseTexture; FontColor: TColor; Transparent: Boolean = True); inline;
    procedure DrawTextInRect(X, Y: Integer; DstRect: TIntRect; Text: TCustomBaseTexture; FontColor: TColor; Effect: TBlendingEffect = TBlendingEffect.Normal); inline;
    procedure PixelsOut(X, Y: Integer; Color: TColor; Size: Integer);
    procedure FillFrameRect(ARect: TIntRect; BorderColor, FillColor: TColor); overload;
    procedure FrameRect(ARect: TIntRect; FrameColor: TColor); overload;
    procedure FillRect(ARect: TIntRect; FillColor: TColor); overload;
    procedure Line(X1, Y1, X2, Y2: Integer; LineColor: TColor); overload;
  end;

  TCustomBaseTextureHelper = class helper for TCustomBaseTexture
  public
    function ClientRect: TIntRect; inline;
    procedure SetSize(W, H: Integer; AutoInitialize: Boolean = False); inline;
  end;

  TCustomLockableTextureHelper = class helper for TCustomLockableTexture
  public
    function ClientRect: TIntRect; inline;
    procedure SetSize(W, H: Integer; AutoInitialize: Boolean = False); inline;
    function LoadFromFontData(AData: Pointer; ADataSize: LongWord; AWidth, AHeight: Integer): Boolean;
    function LoadFromDataEx(ABuffer: Pointer; ADataSize: LongWord; ABitCount: Byte; AWidth, AHeight: Integer; MirrorX, MirrorY: Boolean): Boolean;
    function LoadAlphaFromDataEx(ABuffer: Pointer; ADataSize: LongWord; AWidth, AHeight: Integer): Boolean;
    function LoadFromPng32Data(ABuffer: Pointer; ADataSize: LongWord; ABitCount: Byte; AWidth, AHeight: Integer): Boolean;
    function LoadFromPng(PNG:TPngImage):Boolean;
    procedure ResetAlpha;
    procedure Fill(Color: TColor);
    procedure FillRect(ARect: TIntRect; Color: TColor);
    procedure Draw(X, Y: Integer; Source: TCustomLockableTexture; Transparent: Boolean = True); overload;
    procedure Draw(X, Y: Integer; SrcRect: TIntRect; Source: TCustomLockableTexture; Transparent: Boolean = True); overload;
    procedure Draw(SrcRect, DstRect: TIntRect; Source: TCustomLockableTexture; Transparent: Boolean = True); overload;
    procedure HorFillDraw(const X1, X2, Y: Integer; Source: TCustomLockableTexture; const Transparent: Boolean = True);
    procedure VerFillDraw(const X, Y1, Y2: Integer; Source: TCustomLockableTexture; const Transparent: Boolean = True);
    procedure CopyFrom(Source : TCustomLockableTexture);
    function Pixels(x, y: Integer):Cardinal;
  end;

function dxColorToAlphaColor(AColor: TColor; Alpha: Byte): Cardinal;
function NotColor(Source: TColor): TColor; inline;
function cColor1(Color: TColor): Cardinal; inline;
procedure WidthBytes(AWidth, ABitCount: Integer; var APicth: Integer); inline;

implementation

uses
  AsphyreTextureFonts, WIL;

function NotColor(Source: TColor): TColor;
begin
  Result := $FFFFFF - ColorToRGB(Source);
end;

function DisplaceRB(Color: Cardinal): Cardinal;
asm
  mov ecx, eax
  mov edx, eax
  and eax, 0FF00FF00h
  and edx, 0000000FFh
  shl edx, 16
  or eax, edx
  mov edx, ecx
  shr edx, 16
  and edx, 0000000FFh
  or eax, edx
end;

function dxColorToAlphaColor(AColor: TColor; Alpha: Byte): Cardinal;

  function _ColorToRGBQuad(AColor: TColor; AReserved: Byte): TRGBQuad;
  var
    ATemp: TRGBA;
  begin
    DWORD(ATemp) := ColorToRGB(AColor);
    Result.rgbBlue := ATemp.B;
    Result.rgbRed := ATemp.R;
    Result.rgbGreen := ATemp.G;
    Result.rgbReserved := AReserved;
  end;

begin
  if AColor = Graphics.clNone then
    Result := $00000000
  else
    if AColor = Graphics.clDefault then
      Result := $00010203
    else
      Result := Cardinal(_ColorToRGBQuad(AColor, Alpha));
end;

function cColor1(Color: TColor): Cardinal;
begin
  Result := dxColorToAlphaColor(Color, 255);
  //Result := DisplaceRB(Color) or $FF000000;
end;

procedure WidthBytes(AWidth, ABitCount: Integer; var APicth: Integer);
begin
  APicth := (((AWidth * ABitCount) + 31) and not 31) div 8;
  //APicth := (((AWidth * ABitCount) + 31) shr 5) * 4;
end;

{ TCustomCanvasHelper }

procedure TCustomCanvasHelper.FillFrameRect(ARect: TIntRect; BorderColor, FillColor: TColor);
var
  SrcRect: TFloatRect;
begin
  SrcRect := FloatRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  FillRect(SrcRect, ColorRect(cColor1(FillColor)));
  FrameRect(Quad(SrcRect), ColorRect(cColor1(BorderColor)));
end;

procedure TCustomCanvasHelper.FrameRect(ARect: TIntRect; FrameColor: TColor);
var
  SrcRect: TFloatRect;
begin
  SrcRect := FloatRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  FrameRect(Quad(SrcRect), ColorRect(cColor1(FrameColor)));
end;

procedure TCustomCanvasHelper.FillRect(ARect: TIntRect; FillColor: TColor);
var
  SrcRect: TFloatRect;
begin
  SrcRect := FloatRect(ARect.Left, ARect.Top, ARect.Right, ARect.Bottom);
  FillRect(SrcRect, ColorRect(cColor1(FillColor)));
end;

procedure TCustomCanvasHelper.FillRectAlpha(const DestRect: TIntRect; Color: TColor; Alpha: Integer);
var
  SrcRect: TFloatRect;
begin
  SrcRect := FloatRect(DestRect.Left, DestRect.Top, DestRect.Right-DestRect.Left, DestRect.Bottom-DestRect.Top);
  FillQuad(Quad(SrcRect), IntColor(cColor1(Color), Alpha), TBlendingEffect.Normal);
end;

procedure TCustomCanvasHelper.Draw(X, Y: Integer; Texture: TCustomBaseTexture; Transparent: Boolean; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColorWhite, TransparentBlendingEff[Transparent], ReversalKind);
end;

function ClipRect2(var DestRect, SrcRect: TIntRect; const DestRect2, SrcRect2: TIntRect): Boolean;
begin
  if DestRect.Left < DestRect2.Left then
  begin
    SrcRect.Left := SrcRect.Left + (DestRect2.Left - DestRect.Left);
    DestRect.Left := DestRect2.Left;
  end;

  if DestRect.Top < DestRect2.Top then
  begin
    SrcRect.Top := SrcRect.Top + (DestRect2.Top - DestRect.Top);
    DestRect.Top := DestRect2.Top;
  end;

  if SrcRect.Left < SrcRect2.Left then
  begin
    DestRect.Left := DestRect.Left + (SrcRect2.Left - SrcRect.Left);
    SrcRect.Left := SrcRect2.Left;
  end;

  if SrcRect.Top < SrcRect2.Top then
  begin
    DestRect.Top := DestRect.Top + (SrcRect2.Top - SrcRect.Top);
    SrcRect.Top := SrcRect2.Top;
  end;

  if DestRect.Right > DestRect2.Right then
  begin
    SrcRect.Right := SrcRect.Right - (DestRect.Right - DestRect2.Right);
    DestRect.Right := DestRect2.Right;
  end;

  if DestRect.Bottom > DestRect2.Bottom then
  begin
    SrcRect.Bottom := SrcRect.Bottom - (DestRect.Bottom - DestRect2.Bottom);
    DestRect.Bottom := DestRect2.Bottom;
  end;

  if SrcRect.Right > SrcRect2.Right then
  begin
    DestRect.Right := DestRect.Right - (SrcRect.Right - SrcRect2.Right);
    SrcRect.Right := SrcRect2.Right;
  end;

  if SrcRect.Bottom > SrcRect2.Bottom then
  begin
    DestRect.Bottom := DestRect.Bottom - (SrcRect.Bottom - SrcRect2.Bottom);
    SrcRect.Bottom := SrcRect2.Bottom;
  end;

  Result := (DestRect.Left < DestRect.Right) and (DestRect.Top < DestRect.Bottom) and (SrcRect.Left < SrcRect.Right) and (SrcRect.Top < SrcRect.Bottom);
end;

procedure TCustomCanvasHelper.Draw(X, Y: Integer; SourceRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, SourceRect, Texture, IntColorWhite, TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawAlpha(X, Y: Integer; Texture: TCustomBaseTexture; Alpha: Byte; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColorAlpha(Alpha), Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.Draw(X, Y: Integer; Texture: TCustomBaseTexture; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColorWhite, Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.Draw(X, Y: Integer; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, Color, Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.Draw(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(SrcRect, IntRect(X, Y, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top), Texture, Color, Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.Draw(SrcRect, DsTRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
var
  Mapping: TQuad;
  Dest: TPoint2f;
  VisibleSize: TPoint2f;
  ViewPos, ViewSize: TPoint2f;
begin
  if Texture = nil then Exit;
  if (DsTRect.Left > ClipRect.Right) or (DsTRect.Top > ClipRect.Bottom) then Exit;

  if ClipRect2(DsTRect, SrcRect, ClipRect, Texture.ClienTRect) then
  begin
    ViewSize.X := SrcRect.Right - SrcRect.Left;
    ViewSize.Y := SrcRect.Bottom - SrcRect.Top;

    ViewPos.X := SrcRect.Left;
    ViewPos.Y := SrcRect.Top;

    VisibleSize.X := Texture.Width;
    VisibleSize.Y := Texture.Height;

    Dest.X := ViewPos.X + Min(ViewSize.X, VisibleSize.X);
    Dest.Y := ViewPos.Y + Min(ViewSize.Y, VisibleSize.Y);

    case ReversalKind of
      rkNone:
      begin
        Mapping.Values[0].X := ViewPos.X / Texture.Width;
        Mapping.Values[0].X := ViewPos.X / Texture.Width;
        Mapping.Values[0].Y := ViewPos.Y / Texture.Height;

        Mapping.Values[1].X := Dest.X / Texture.Width;
        Mapping.Values[1].Y := Mapping.Values[0].Y;

        Mapping.Values[2].X := Mapping.Values[1].X;
        Mapping.Values[2].Y := Dest.Y / Texture.Height;

        Mapping.Values[3].X := Mapping.Values[0].X;
        Mapping.Values[3].Y := Mapping.Values[2].Y;
      end;
      rkHor:
      begin
        Mapping.Values[1].X := ViewPos.X / Texture.Width;
        Mapping.Values[1].Y := ViewPos.Y / Texture.Height;

        Mapping.Values[0].X := Dest.X / Texture.Width;
        Mapping.Values[0].Y := Mapping.Values[1].Y;

        Mapping.Values[3].X := Mapping.Values[0].X;
        Mapping.Values[3].Y := Dest.Y / Texture.Height;

        Mapping.Values[2].X := Mapping.Values[1].X;
        Mapping.Values[2].Y := Mapping.Values[3].Y;
      end;
      rkVer:
      begin
        Mapping.Values[2].X := ViewPos.X / Texture.Width;
        Mapping.Values[2].Y := ViewPos.Y / Texture.Height;

        Mapping.Values[3].X := Dest.X / Texture.Width;
        Mapping.Values[3].Y := Mapping.Values[2].Y;

        Mapping.Values[0].X := Mapping.Values[3].X;
        Mapping.Values[0].Y := Dest.Y / Texture.Height;

        Mapping.Values[1].X := Mapping.Values[2].X;
        Mapping.Values[1].Y := Mapping.Values[0].Y;
      end;
    end;
    if Texture.ISPNGTexture and (Effect = TBlendingEffect.beBlend)  then
      Effect := TBlendingEffect.Normal;

    UseTexture(Texture, Mapping);
    TexQuad(Quad(DsTRect), Color, Effect);
  end;
end;

procedure TCustomCanvasHelper.DrawAlpha(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Alpha: Byte; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, SrcRect, Texture, IntColorAlpha(Alpha), Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.DrawBlend(X, Y: Integer; Texture: TCustomBaseTexture; Blendmode: Integer; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColorWhite, TBlendingEffect.beBlend, ReversalKind);
end;

procedure TCustomCanvasHelper.DrawBlendAdd(X, Y: Integer; Texture: TCustomBaseTexture; Blendmode, tNum: Integer; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColorWhite, TBlendingEffect.beSrcColorAdd, ReversalKind);
end;

procedure TCustomCanvasHelper.DrawBlendEffect(X, Y: Integer;
  Texture: TCustomBaseTexture; Blendmode: Integer;
  ReversalKind: TReversalKind);
var
 E : TBlendingEffect;
begin
  if Texture = nil then Exit;
  if Blendmode in [Ord(TBlendingEffect.None) .. Ord(TBlendingEffect.beBright)] then
    E:= TBlendingEffect(Blendmode)
  else
    E:= TBlendingEffect.beBlend;

  Draw(X, Y, Texture.ClientRect, Texture, IntColorWhite, E, ReversalKind);
end;

procedure TCustomCanvasHelper.DrawColor(X, Y: Integer; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColor(cColor1(Color),255), TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawColor(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, SrcRect, Texture, IntColor(cColor1(Color),255), TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawColorAlpha(const X, Y: Integer; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; Alpha: Integer; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, Texture.ClientRect, Texture, IntColor(cColor1(Color), Alpha), TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawColorAlpha(const X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Transparent: Boolean; Alpha: Integer; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  Draw(X, Y, SrcRect, Texture, IntColor(cColor1(Color), Alpha), TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawInRect(const X, Y: Integer; DstRect: TIntRect; Texture: TCustomBaseTexture; Color: TColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
var
  SrcRect: TIntRect;
begin
  SrcRect := IntRect(X, Y, Texture.Width, Texture.Height);
  DstRect := ShortRect(SrcRect, DstRect);
  SrcRect := IntRect(DstRect.Left - X, DstRect.Top - Y, DstRect.Right - DstRect.Left, DstRect.Bottom - DstRect.Top);
  Draw(SrcRect, DstRect, Texture, IntColor(cColor1(Color), 255), Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.HorFillDraw(const X1, X2, Y: Integer; Source: TCustomBaseTexture; const Transparent: Boolean);
var
  W, I: Integer;
begin
  if Source = nil then Exit;

  if (ABS(X2-X1) < Source.Width) then
  begin
    Draw(Source.ClientRect, IntRectBDS(X1, Y, X2, Y), Source, IntColorWhite);
    Exit;
  end;

  W := Source.Width;
  I := 0;
  while True do
  begin
    Draw(X1 + I * W, Y, Source, Transparent);
    Inc(I);
    if X1 + I * W + W> X2 then
    begin
      if X1 < X2 then
        Draw(X2 - W, Y, Source, Transparent);;
      Break;
    end;
  end;
end;

procedure TCustomCanvasHelper.Line(X1, Y1, X2, Y2: Integer; LineColor: TColor);
begin
  Line(X1, Y1, X2, Y2, cColor1(LineColor));
end;

procedure TCustomCanvasHelper.PixelsOut(X, Y: Integer; Color: TColor;
  Size: Integer);
var
  SrcRect: TFloatRect;
begin
  SrcRect := FloatRect(X, Y, Size, Size);
//  FillRect(SrcRect, ColorRect(cColor1(Color)));
  FillRect(SrcRect, IntColor(cColor1(Color), 255));
end;

procedure TCustomCanvasHelper.VerFillDraw(const X, Y1, Y2: Integer; Source: TCustomBaseTexture; const Transparent: Boolean);
var
  H, I: Integer;
begin
  if Source = nil then Exit;

  if (ABS(Y2 - Y1) < Source.Height) then
  begin
    Draw(Source.ClientRect, IntRectBDS(X, Y1, X, Y2), Source, IntColorWhite);
    Exit;
  end;

  H := Source.Height;
  I := 0;
  while True do
  begin
    Draw(X, Y1 + I * H, Source, Transparent);
    Inc(I);
    if Y1 + I * H + H > Y2 then
    begin
      if Y1 < Y2 then
        Draw(X, Y2 - H, Source, Transparent);
      Break;
    end;
  end;
end;

procedure TCustomCanvasHelper.StretchDraw(const DestRect, SrcRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean);
var
  Mapping: TQuad;
  Source: TPoint2f;
  Dest: TPoint2f;
  VisibleSize: TPoint2f;
  ViewPos, ViewSize: TPoint2f;
  Effect: TBlendingEffect;
begin
  if Transparent then
    Effect := TBlendingEffect.Normal
  else
    Effect := TBlendingEffect.Unknown;

  ViewSize.x := DestRect.Right - DestRect.Left;
  ViewSize.y := DestRect.Bottom - DestRect.Top;
  ViewPos.x := SrcRect.Left;
  ViewPos.y := SrcRect.Top;

  VisibleSize.x := Texture.Width;
  VisibleSize.y := Texture.Height;

  Source.x := ViewPos.x;
  Source.y := ViewPos.y;
  Dest.x := Source.x + VisibleSize.x;
  Dest.y := Source.y + VisibleSize.y;

  Mapping.Values[0].x := Source.x / Texture.Width;
  Mapping.Values[0].y := Source.y / Texture.Height;

  Mapping.Values[1].x := Dest.x / Texture.Width;
  Mapping.Values[1].y := Mapping.Values[0].y;

  Mapping.Values[2].x := Mapping.Values[1].x;
  Mapping.Values[2].y := Dest.y / Texture.Height;

  Mapping.Values[3].x := Mapping.Values[0].x;
  Mapping.Values[3].y := Mapping.Values[2].y;

  UseTexture(Texture, Mapping);
  TexQuad(Quad(DestRect.Left, DestRect.Top, ViewSize.x, ViewSize.y), IntColorWhite, Effect);
end;

procedure TCustomCanvasHelper.StretchDraw(const DestRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean);
begin
  StretchDraw(DestRect, Texture.ClientRect, Texture, Transparent);
end;

procedure TCustomCanvasHelper.DrawTiled(X, Y: Integer; SourceRect: TIntRect; Texture: TCustomBaseTexture; Transparent: Boolean; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  DrawTiled(X, Y, SourceRect, Texture, IntColorWhite, TransparentBlendingEff[Transparent], ReversalKind);
end;

procedure TCustomCanvasHelper.DrawTiled(X, Y: Integer; SrcRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
begin
  if Texture = nil then Exit;
  DrawTiled(SrcRect, IntRectBDS(X, Y, SrcRect.Right - SrcRect.Left, SrcRect.Bottom - SrcRect.Top), Texture, Color, Effect, ReversalKind);
end;

procedure TCustomCanvasHelper.DrawTiled(SrcRect, DstRect: TIntRect; Texture: TCustomBaseTexture; Color: TIntColor; Effect: TBlendingEffect; ReversalKind: TReversalKind);
var
  Mapping: TQuad;
  Dest: TPoint2f;
  VisibleSize: TPoint2f;
  ViewPos, ViewSize: TPoint2f;
begin
  if Texture = nil then Exit;
  if (DstRect.Left > ClipRect.Right) or (DstRect.Top > ClipRect.Bottom) then Exit;

  ViewSize.X := SrcRect.Right - SrcRect.Left;
  ViewSize.Y := SrcRect.Bottom - SrcRect.Top;

  ViewPos.X := SrcRect.Left;
  ViewPos.Y := SrcRect.Top;

  VisibleSize.X := Texture.Width;
  VisibleSize.Y := Texture.Height;

  Dest.X := ViewPos.X + Min(ViewSize.X, VisibleSize.X);
  Dest.Y := ViewPos.Y + Min(ViewSize.Y, VisibleSize.Y);

  case ReversalKind of
    rkNone:
    begin
      Mapping.Values[0].X := ViewPos.X / Texture.Width;
      Mapping.Values[0].Y := ViewPos.Y / Texture.Height;

      Mapping.Values[1].X := Dest.X / Texture.Width;
      Mapping.Values[1].Y := Mapping.Values[0].Y;

      Mapping.Values[2].X := Mapping.Values[1].X;
      Mapping.Values[2].Y := Dest.Y / Texture.Height;

      Mapping.Values[3].X := Mapping.Values[0].X;
      Mapping.Values[3].Y := Mapping.Values[2].Y;
    end;
    rkHor:
    begin
      Mapping.Values[1].X := ViewPos.X / Texture.Width;
      Mapping.Values[1].Y := ViewPos.Y / Texture.Height;

      Mapping.Values[0].X := Dest.X / Texture.Width;
      Mapping.Values[0].Y := Mapping.Values[1].Y;

      Mapping.Values[3].X := Mapping.Values[0].X;
      Mapping.Values[3].Y := Dest.Y / Texture.Height;

      Mapping.Values[2].X := Mapping.Values[1].X;
      Mapping.Values[2].Y := Mapping.Values[3].Y;
    end;
    rkVer:
    begin
      Mapping.Values[2].X := ViewPos.X / Texture.Width;
      Mapping.Values[2].Y := ViewPos.Y / Texture.Height;

      Mapping.Values[3].X := Dest.X / Texture.Width;
      Mapping.Values[3].Y := Mapping.Values[2].Y;

      Mapping.Values[0].X := Mapping.Values[3].X;
      Mapping.Values[0].Y := Dest.Y / Texture.Height;

      Mapping.Values[1].X := Mapping.Values[2].X;
      Mapping.Values[1].Y := Mapping.Values[0].Y;
    end;
  end;
  if Texture.ISPNGTexture and (Effect = TBlendingEffect.beBlend)  then
    Effect := TBlendingEffect.Normal;

  UseTexture(Texture, Mapping);
  TexQuad(Quad(DsTRect), Color, Effect);
end;

procedure TCustomCanvasHelper.TextOut(X, Y: Integer; const Text: String; FontColor: TColor; Effect: TBlendingEffect);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    Draw(X, Y, AText.ClientRect, AText, IntColor(cColor1(FontColor), 255), Effect);
end;

procedure TCustomCanvasHelper.TextOut(X, Y: Integer; const Text: String; FontColor: TColor; FontStyles: TFontStyles;FontSize: Integer; Effect: TBlendingEffect);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, DefaultFontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    Draw(X, Y, AText.ClientRect, AText, IntColor(cColor1(FontColor), 255), Effect);
end;

procedure TCustomCanvasHelper.ShadowTextOut(X, Y: Integer; const Text: string; FontColor, BgColor: TColor; FontStyles: TFontStyles; FontSize: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, FontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    DrawShadowText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.ShadowTextOut(X, Y: Integer; const Text: string; FontColor, BgColor: TColor);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawShadowText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldText(X, Y: Integer; const Text: String; FontColor, BgColor: TColor);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldText(const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles; X, Y: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, DefaultFontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldText(const Text: String; FontColor, BgColor: TColor; X, Y: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldTextOut(X, Y: Integer; const Text: String;
  FontColor, BgColor: TColor);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldTextOut(X, Y: Integer;  FontColor, BgColor: TColor;Text: String);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldTextOut(const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles; X, Y: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, DefaultFontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldTextOut(const Text: String; FontColor, BgColor: TColor; X, Y: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.Default.TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

procedure TCustomCanvasHelper.BoldTextOut(X, Y: Integer;const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles;FontSize: Integer);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, FontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;
procedure TCustomCanvasHelper.BoldTextOut(X, Y: Integer;const Text: String; FontColor, BgColor: TColor; FontStyles: TFontStyles;FontSize: Integer;FontName: String);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(FontName, FontSize, FontStyles).TextOut(Text);
  if AText <> nil then
    DrawBoldText(X, Y, AText, FontColor, BgColor);
end;

//增加鉴宝文字绘制
procedure TCustomCanvasHelper.BoldTextOutZ(X, Y, Z, fcolor, bcolor: Integer;Str: string);
var
  tStr, temp        : string;
  i, Len, n, aline  : Integer;
label
  Loop1;
begin
  tStr := Str;
  n := 0;
  if tStr <> '' then begin
    Loop1:
    Len := Length(tStr);
    temp := '';
    i := 1;
    while True do begin
      if i > Len then Break;

      if Byte(tStr[i]) >= 128 then begin
        temp := temp + tStr[i];
        Inc(i);
        if i <= Len then
          temp := temp + tStr[i]
        else
          Break;
      end else
        temp := temp + tStr[i];
      //clGray

      if (temp <> '') and (tStr[i] = #$0D) and ((i + 1 <= Len) and (tStr[i + 1] = #$0A)) then begin
        BoldTextOut( X, Y + n * 14, fcolor, bcolor, temp);
        tStr := Copy(tStr, i + 1 + 1, Len - i - 1);
        if tStr <> '' then Inc(n);
        temp := '';
        Break;
      end;

      aline := FontManager.Default.TextWidth(temp);
      if (aline > Z) and (temp <> '') then begin
        BoldTextOut( X, Y + n * 14, fcolor,bcolor,  temp);
        tStr := Copy(tStr, i + 1, Len - i);
        if tStr <> '' then Inc(n);
        temp := '';
        Break;
      end;
      Inc(i);
    end;
    if temp <> '' then begin
      BoldTextOut( X, Y + n * 14, fcolor,bcolor, temp);
      tStr := '';
    end;
    if tStr <> '' then
      goto Loop1;
  end;
end;

procedure TCustomCanvasHelper.RectText(X, Y: Integer; const Text: String; FColor, BColor: TColor; DstRect: TIntRect; FontStyles: TFontStyles; FontSize: Integer; Transparent: Boolean = True);
var
  AText: TCustomBaseTexture;
begin
  AText := FontManager.GetFont(DefaultFontName, FontSize, FontStyles).TextOut(Text);
  if AText <> nil then begin
    DrawColor(X - 1, Y, DstRect, AText, BColor, Transparent);
    DrawColor(X + 1, Y, DstRect, AText, BColor, Transparent);
    DrawColor(X, Y - 1, DstRect, AText, BColor, Transparent);
    DrawColor(X, Y + 1, DstRect, AText, BColor, Transparent);
    DrawColor(X, Y, DstRect, AText, FColor, Transparent);
  end;
end;

procedure TCustomCanvasHelper.DrawShadowText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Transparent: Boolean);
begin
  DrawColor(X + 1, Y + 1, Text, BgColor, Transparent);
  DrawColor(X, Y, Text, FontColor, Transparent);
end;

procedure TCustomCanvasHelper.DrawBoldText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Transparent: Boolean);
begin
//  DrawColor(X - 1, Y - 1, Text, BgColor, Transparent);
  //DrawColor(X - 1, Y + 1, Text, BgColor, Transparent);
  //DrawColor(X + 1, Y - 1, Text, BgColor, Transparent);
  //DrawColor(X + 1, Y + 1, Text, BgColor, Transparent);
  DrawColor(X - 1, Y, Text, BgColor, Transparent);
  DrawColor(X + 1, Y, Text, BgColor, Transparent);
  DrawColor(X, Y - 1, Text, BgColor, Transparent);
  DrawColor(X, Y + 1, Text, BgColor, Transparent);
  DrawColor(X, Y, Text, FontColor, Transparent);
end;

procedure TCustomCanvasHelper.DrawOutLineText(X, Y: Integer; Text: TCustomBaseTexture; FontColor, BgColor: TColor; OutLinePixel : Integer;  Transparent: Boolean);
var
  I : Integer;
begin
  for i := 1 to OutLinePixel do
  begin
    DrawColor(X - i, Y, Text, BgColor, Transparent);
    DrawColor(X + i, Y, Text, BgColor, Transparent);
    DrawColor(X, Y - i, Text, BgColor, Transparent);
    DrawColor(X, Y + i, Text, BgColor, Transparent);
  end;
  DrawColor(X, Y, Text, FontColor, Transparent);
end;

procedure TCustomCanvasHelper.DrawOutLineTextInRect(X, Y: Integer; DstRect:TIntRect; Text: TCustomBaseTexture; FontColor, BgColor: TColor; OutLinePixel : Integer;  Transparent: Boolean);
var
  I : Integer;
begin
  for i := 1 to OutLinePixel do
  begin
    DrawInRect(X - i, Y, DstRect , Text ,  IntColor(cColor1(BgColor),255), TBlendingEffect.Normal);
    DrawInRect(X + i, Y, DstRect , Text ,  IntColor(cColor1(BgColor),255), TBlendingEffect.Normal);
    DrawInRect(X, Y - i, DstRect , Text ,  IntColor(cColor1(BgColor),255), TBlendingEffect.Normal);
    DrawInRect(X, Y + i, DstRect , Text ,  IntColor(cColor1(BgColor),255), TBlendingEffect.Normal);
  end;
  DrawInRect(X, Y , DstRect , Text , IntColor(cColor1(FontColor),255), TBlendingEffect.Normal);
end;

procedure TCustomCanvasHelper.DrawBoldTextInRect(X, Y: Integer; DstRect: TIntRect; Text: TCustomBaseTexture; FontColor, BgColor: TColor; Effect: TBlendingEffect);
begin
  DrawInRect(X - 1, Y - 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X - 1, Y + 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X + 1, Y - 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X + 1, Y + 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X - 1, Y, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X + 1, Y, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X, Y - 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X, Y + 1, DstRect, Text, IntColor(cColor1(BgColor),255), Effect);
  DrawInRect(X, Y, DstRect, Text, IntColor(cColor1(FontColor),255), Effect);
end;

procedure TCustomCanvasHelper.DrawText(X, Y: Integer; Text: TCustomBaseTexture; FontColor: TColor; Transparent: Boolean);
begin
  DrawColor(X, Y, Text, FontColor, Transparent);
end;

procedure TCustomCanvasHelper.DrawTextInRect(X, Y: Integer; DstRect: TIntRect; Text: TCustomBaseTexture; FontColor: TColor; Effect: TBlendingEffect);
begin
  DrawInRect(X, Y, DstRect, Text, IntColor(cColor1(FontColor),255), Effect);
end;

{ TCustomLockableTextureHelper }

function TCustomLockableTextureHelper.ClientRect: TIntRect;
begin
  Result := IntRect(0, 0, Width, Height);
end;

procedure TCustomLockableTextureHelper.Draw(X, Y: Integer; Source: TCustomLockableTexture; Transparent: Boolean);
begin
  Draw(Source.ClientRect, IntRect(X, Y, Width - X, Height - Y), Source, Transparent);
end;

procedure TCustomLockableTextureHelper.Draw(X, Y: Integer; SrcRect: TIntRect; Source: TCustomLockableTexture; Transparent: Boolean);
begin
  Draw(SrcRect, IntRect(X, Y, Width, Height), Source, Transparent);
end;

procedure TCustomLockableTextureHelper.CopyFrom(
  Source: TCustomLockableTexture);
var
  Pixels: TLockedPixels;
  Pixels2: TLockedPixels;
begin
  Self.Mipmapping := Source.Mipmapping;
  Self.FPixelFormat := Source.PixelFormat;
  Self.Width := Source.Width;
  Self.Height := Source.Height;
  Source.Lock(IntRectBDS(0, 0, Source.Width, Source.Height), Pixels);
  try
    Pixels2.Pitch := Pixels.Pitch;
    if Initialize then begin
      try
        Lock(IntRectBDS(0, 0, Width, Height), Pixels2);
        Move(Pixels.Bits^, Pixels2.Bits^, Pixels2.Pitch * Height);
      finally
        Unlock;
      end;
    end;
  finally
    Source.Unlock;
  end;
end;

function TCustomLockableTextureHelper.Pixels(x, y: Integer):Cardinal;
var
  ADPixels: TLockedPixels;
begin
  Result := 0;
  if (x < 0) or (y < 0) or (x >= FWidth) or (y >= FHeight) then Exit;

  Lock(IntRectBDS(0, y, FWidth, 1), ADPixels);
  if (not Assigned(ADPixels.Bits)) then Exit;
//  Lock(ClientRect, ADPixels);
  try
    Result:= ADPixels.Pixels[x, y];
  finally
    Unlock;
  end;
end;


procedure TCustomLockableTextureHelper.Draw(SrcRect, DstRect: TintRect; Source: TCustomLockableTexture; Transparent: Boolean);
var
  ASLine, ADLine: Pointer;
  I, J, W, H: Integer;
  ASPixels: TLockedPixels;
  ADPixels: TLockedPixels;
begin
  if Source.PixelFormat = PixelFormat then
  begin
    Source.Lock(Source.ClientRect, ASPixels);
     Lock(ClientRect, ADPixels);
    try
      W := Min(SrcRect.Right - SrcRect.Left, DstRect.Right - DstRect.Left);
      H := Min(SrcRect.Bottom - SrcRect.Top, DstRect.Right - DstRect.Left);
      Inc(Cardinal(ADPixels.Bits), DstRect.Top * ADPixels.Pitch + DstRect.Left * BytesPerPixel);
      Inc(Cardinal(ASPixels.Bits), SrcRect.Top * ASPixels.Pitch + SrcRect.Left * Source.BytesPerPixel);
      if Transparent then
      begin
        for I := 0 to H - 1 do
        begin
          ADLine := ADPixels.Bits;
          ASLine := ASPixels.Bits;
          for J := 0 to W - 1 do
          begin
            if PCardinal(ASLine)^ > 0 then
              PCardinal(ADLine)^ := PCardinal(ASLine)^;
            Inc(PCardinal(ASLine));
            Inc(PCardinal(ADLine));
          end;
          Inc(Cardinal(ASPixels.Bits), ASPixels.Pitch);
          Inc(Cardinal(ADPixels.Bits), ADPixels.Pitch);
        end;
      end
      else
      begin
        for I := 0 to H - 1 do
        begin
          Move(ASPixels.Bits^, ADPixels.Bits^, W * BytesPerPixel);
          Inc(Cardinal(ASPixels.Bits), ASPixels.Pitch);
          Inc(Cardinal(ADPixels.Bits), ADPixels.Pitch);
        end;
      end;
    finally
      Source.Unlock;
      Unlock;
    end;
  end;
end;

procedure TCustomLockableTextureHelper.Fill(Color: TColor);
var
  ACColor: Cardinal;
  X: Integer;
  APixels:TLockedPixels;
begin
  ACColor := cColor1(Color);
  Lock(ClientRect, APixels);
  try
    for X := 0 to Height - 1 do
    begin
      FillChar(APixels.Bits^, APixels.Pitch, ACColor);
      APixels.Bits := Pointer(Cardinal(APixels.Bits) + APixels.Pitch);
    end;
  finally
    Unlock;
  end;
end;

procedure TCustomLockableTextureHelper.FillRect(ARect: TIntRect; Color: TColor);
var
  ACColor: Cardinal;
  I: Integer;
  APixels:TLockedPixels;
begin
  ACColor := cColor1(Color);
  Lock(ClientRect, APixels);
  try
    Inc(Cardinal(APixels.Bits), APixels.Pitch * ARect.Top + ARect.Left * BytesPerPixel);
    for I := ARect.Top to ARect.Bottom do
    begin
      FillChar(APixels.Bits^, (ARect.Right - ARect.Left) * BytesPerPixel, ACColor);
      Inc(Cardinal(APixels.Bits), APixels.Pitch);
    end;
  finally
    Unlock;
  end;
end;

procedure TCustomLockableTextureHelper.HorFillDraw(const X1, X2, Y: Integer; Source: TCustomLockableTexture; const Transparent: Boolean);
var
  W, I: Integer;
begin
  W := Source.Width;
  I := 0;
  while True do
  begin
    Self.Draw(X1 + I * W, Y, Source, Transparent);
    Inc(I);
    if X1 + I * W > X2 then
    begin
      if X1 < X2 then
        Draw(X2 - W, Y, Source, Transparent);;
      Break;
    end;
  end;
end;

function TCustomLockableTextureHelper.LoadFromFontData(AData: Pointer; ADataSize: LongWord; AWidth, AHeight: Integer): Boolean;
var
  ADLine: Pointer;
  ASourceLine: PWord;
  AWord: Word;
  APitch1, X, Y: Integer;
  APixels: TLockedPixels;
begin
  Result := False;
  try
    if Initialize then
    begin
      WidthBytes(AWidth, 16, APitch1);
      Lock(IntRectBDS(0, 0, Width, Height),APixels);
      try
        for Y := 0 to Height - 1 do
        begin
          ADLine := Pointer(Cardinal(APixels.Bits) + APixels.Pitch * Y); //图片
          ASourceLine := PWord(Cardinal(AData) + APitch1 * Y);
          for X := 0 to Width - 1 do
          begin
            AWord := 0;
            if PWord(ASourceLine)^ > 0 then
              AWord := ColorTable_A1R5G5B5[$FFFF];
            PWord(ADLine)^ := AWord;
            ADLine := Pointer(Cardinal(ADLine) + 2{BytesPerPixel});
            Inc(ASourceLine);
          end;
        end;
        Result := True;
      finally
        Unlock;
      end;
    end;
  except
  end;
end;

var
  LogicThreadID: Cardinal = 0;

function TCustomLockableTextureHelper.LoadFromDataEx(ABuffer: Pointer; ADataSize: LongWord; ABitCount: Byte; AWidth, AHeight: Integer; MirrorX, MirrorY: Boolean): Boolean;
var
  ADLine, ASourceLine: Pointer;
  AWord: Word;
  V: Cardinal;
  X, Y: Integer;
  LockedPixels: TLockedPixels;
begin
  Result := False;
  PixelFormat := TPixelFormat.A8R8G8B8;
  Width := AWidth;
  Height := AHeight;
  if LogicThreadID = 0 then
  begin
    LogicThreadID := GetCurrentThreadId();
  end;

  if LogicThreadID  <> GetCurrentThreadId then
  begin
    DebugBreak;
  end;

  if Initialize then
  begin
    Lock(IntRectBDS(0, 0, Width, Height), LockedPixels);
    try
      for Y := 0 to AHeight - 1 do
      begin
        if MirrorY then
          ADLine := Pointer(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * (Height - Y - 1))
        else
          ADLine := Pointer(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * Y);
        if MirrorX then
          ADLine := Pointer(Cardinal(ADLine) + LockedPixels.Pitch - BytesPerPixel);
        ASourceLine := Pointer(Cardinal(ABuffer) + (ADataSize div AHeight) * Y);
        case ABitCount of
          8:
          begin
            for X := 0 to AWidth - 1 do
            begin
              if PByte(ASourceLine)^ = 0 then
                V := $00000000
              else
               // V := ColorTable_R5G6B5_32[ColorTable_565[PByte(ASourceLine)^]];
                V :=  ColorPalette[PByte(ASourceLine)^];
              PCardinal(ADLine)^ := V;
              if MirrorX then
                Dec(PCardinal(ADLine))
              else
                Inc(PCardinal(ADLine));
              Inc(PByte(ASourceLine));
            end;
          end;
          16:
          begin
            for X := 0 to AWidth - 1 do
            begin
              if PWord(ASourceLine)^ = 0 then
                V := $00000000
              else
                V := ColorTable_R5G6B5_32[PWord(ASourceLine)^];
              PCardinal(ADLine)^ := V;
              if MirrorX then
                Dec(PCardinal(ADLine))
              else
                Inc(PCardinal(ADLine));
              Inc(PWord(ASourceLine));
            end;
          end;
          24:
          begin
            for X := 0 to AWidth - 1 do
            begin
              V := 0;
              Move(ASourceLine^, V, 3);
              if V <> 0 then
                V := V or $FF000000;
              PCardinal(ADLine)^ := V;
              if MirrorX then
                Dec(PCardinal(ADLine))
              else
                Inc(PCardinal(ADLine));
              Inc(PBGR(ASourceLine));
            end;
          end;
          32:
          begin
            for X := 0 to AWidth - 1 do
            begin
              if PLongWord(ASourceLine)^ > 0 then
                V := PLongWord(ASourceLine)^ or $FF000000
              else
                V := 0;
              PCardinal(ADLine)^ := V;
              if MirrorX then
                ADLine := Pointer(Cardinal(ADLine) - BytesPerPixel)
              else
                ADLine := Pointer(Cardinal(ADLine) + BytesPerPixel);
              Inc(PLongWord(ASourceLine));
            end;
          end;
        end;
      end;
    finally
      Unlock;
    end;
    Result := True;
  end else begin
    {$IFDEF CONSOLE}
       Writeln('[异常] 纹理初始化失败');
    {$ENDIF}
  end;
end;

function TCustomLockableTextureHelper.LoadFromPng(PNG: TPngImage): Boolean;
var
  P: pRGBLine;
  PAlpha : pByteArray;
  RGB:TRGBTriple;
  I,J : Integer;
  A : Byte; //透明值
  ADLine : pCardinal;
  DIB:TBitmap;
  LockedPixels: TLockedPixels;
begin
  PixelFormat := TPixelFormat.A8R8G8B8;
  SetSize(PNG.Width,PNG.Height);
  ISPNGTexture := True;
  if Initialize then
  begin
    Lock(IntRectBDS(0, 0, Width, Height),LockedPixels);
    Try
      if (PNG.Header.ColorType = COLOR_RGBALPHA) then
      begin
        for I := 0  to PNG.Height - 1 do
        begin
          P := pRGBLine(png.Scanline[I]);
          PAlpha := PNG.AlphaScanline[i];
          ADLine := pCardinal(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * I);
          for J := 0 to PNG.Width - 1 do
          begin
            RGB := P^[J];
            A := PAlpha^[J];
            ADLine^ := (A shl 24) or (RGB.rgbtRed shl 16) or (RGB.rgbtGreen shl 8) or (RGB.rgbtBlue);
            Inc(ADLine);
          end;
        end;
      end else
      begin
        DIB := TBitmap.Create;
        Try
          DIB.PixelFormat := pf24bit;
          DIB.SetSize(PNG.Width,PNG.Height);
          DIB.Canvas.Draw(0,0,PNG);
          for I := 0  to PNG.Height - 1 do
          begin
            P := pRGBLine(DIB.Scanline[I]);
            ADLine := pCardinal(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * I);
            for J := 0 to PNG.Width - 1 do
            begin
              RGB := P^[J];
              ADLine^ := ($FF000000) or (RGB.rgbtRed shl 16) or (RGB.rgbtGreen shl 8) or (RGB.rgbtBlue);
              Inc(ADLine);
            end;
          end;
        Finally
          DIB.Free;
        End;
      end;
    Finally
      Unlock;
    End;
  end;
end;

function TCustomLockableTextureHelper.LoadFromPng32Data(ABuffer: Pointer; ADataSize: LongWord; ABitCount: Byte; AWidth, AHeight: Integer): Boolean;
var
  ADLine, ASourceLine: Pointer;
  APitch2, Y: Integer;
  LockedPixels: TLockedPixels;
begin
  Result := False;
  PixelFormat := TPixelFormat.A8R8G8B8;
  Width := AWidth;
  Height := AHeight;
  if Initialize then begin
    WidthBytes(AWidth, 32, APitch2);
    Lock(IntRectBDS(0, 0, Width, Height), LockedPixels);
    try
      for Y := 0 to AHeight - 1 do begin
        ADLine := Pointer(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * Y);
        ASourceLine := Pointer(Cardinal(ABuffer) + APitch2 * Y);
        Move(ASourceLine^, ADLine^, APitch2);
      end;
      Result := True;
    finally
      Unlock;
    end;
  end;
end;

function TCustomLockableTextureHelper.LoadAlphaFromDataEx(ABuffer: Pointer; ADataSize: LongWord; AWidth, AHeight: Integer): Boolean;
  function LoByteToByte(Value:Byte):Byte;inline;
  begin
    Result := (Value and $F) * 16;
  end;

  function HiByteToByte(Value:Byte):Byte;inline;
  begin
     Result := ((Value and $F0) shr 4 ) * 16;
  end;
var
  AAlphaData: Pointer;
  ASLine: PWord;
  ADLine: PCardinal;
  AALine: PByte;
  X, Y, AAPitch: Integer;
  V32: Cardinal;
  ARGBQuad: TRGBQuad absolute V32;
  Lo:boolean;
  LockedPixels: TLockedPixels;
begin
  Result := False;
  Width := AWidth;
  Height := AHeight;
  PixelFormat := TPixelFormat.A8R8G8B8;
  if Initialize then
  begin
    Lock(IntRectBDS(0, 0, Width, Height),LockedPixels);
    try
      AAlphaData := Pointer(Cardinal(ABuffer) + (LockedPixels.Pitch DIV 2) * Height);
      AAPitch := Width div 2;

      for Y := 0 to Height - 1 do
      begin
        ASLine := PWord(Cardinal(ABuffer) + (LockedPixels.Pitch DIV 2) * Y);
        ADLine := PCardinal(Cardinal(LockedPixels.Bits) + LockedPixels.Pitch * (Height - 1 - Y));
        AALine := PByte(Cardinal(AAlphaData) + AAPitch * (Height - 1 - Y));
        Lo := True;
        for X := 0 to Width - 1 do
        begin
          V32 := ColorTable_R5G6B5_32[ASLine^];

          if Lo then
          begin
            ARGBQuad.rgbReserved := LoByteToByte( AALine^);
          end
          else
          begin
            ARGBQuad.rgbReserved := HiByteToByte( AALine^);
          end;


          ADLine^ := V32;
          Inc(ASLine);
          Inc(ADLine);

          if Lo then
          begin
            Inc(AALine);
            Lo := False;
          end else
          begin
            Lo := not Lo;
          end;
        end;
      end;
      Result := True;
    finally
      Unlock;
    end;
  end;
end;

procedure TCustomLockableTextureHelper.ResetAlpha;
var
  SrcPx: PLongWord;
  I: Integer;
  LockedPixels: TLockedPixels;
begin
  Lock(ClientRect, LockedPixels);
  try
    for i:= 0 to (Width * Height) - 1 do
    begin
      SrcPx :=LockedPixels.Bits;
      SrcPx^ := SrcPx^ or $FF000000;
      Inc(SrcPx);
    end;
  finally
    UnLock;
  end;
end;

procedure TCustomLockableTextureHelper.SetSize(W, H: Integer; AutoInitialize: Boolean);
begin
  Width := W;
  Height := H;
  if AutoInitialize then
    Initialize;
end;

procedure TCustomLockableTextureHelper.VerFillDraw(const X, Y1, Y2: Integer; Source: TCustomLockableTexture; const Transparent: Boolean);
var
  H, I: Integer;
begin
  H := Source.Height;
  I := 0;
  while True do
  begin
    Draw(X, Y1 + I * H, Source, Transparent);
    Inc(I);
    if Y1 + I * H > Y2 then
    begin
      if Y1 < Y2 then
        Draw(X, Y2 - H, Source, Transparent);;
      Break;
    end;
  end;
end;

{ TCustomBaseTextureHelper }

function TCustomBaseTextureHelper.ClientRect: TIntRect;
begin
  Result := IntRectBDS(0, 0, Width, Height);
end;

procedure TCustomBaseTextureHelper.SetSize(W, H: Integer; AutoInitialize: Boolean);
begin
  Width := W;
  Height := H;
  if AutoInitialize then
    Initialize;
end;

end.








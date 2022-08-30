{===============================================================================
  RzGrafx Unit

  Raize Components - Source Unit

  Copyright © 1995-2015 by Raize Software, Inc.  All Rights Reserved.


  Description
  ------------------------------------------------------------------------------
  This unit implements several graphics related procedures and functions.


  Modification History
  ------------------------------------------------------------------------------
  6.0    (09 Oct 2011)
    * Made necessary modifications to various display functions to support
      64-bit development.
  ------------------------------------------------------------------------------
  5.2    (05 Sep 2009)
    * Updated the PaintGradient function to utilize the GDI GradientFill API
      function when drawing an horizontal or vertical gradient to substantially
      improve the performance.
  ------------------------------------------------------------------------------
  5.0    (30 Sep 2008)
    * Updated DrawParentImage procedure to use the wm_PrintClient message
      instead of wm_Paint, which more closely matches how XP/Vista Themes
      simulate transparency in controls.
  ------------------------------------------------------------------------------
  4.0.3  (05 Apr 2006)
    * Fixed problem in PaintGradient that would result in an endless loop when
      drawing a gradient in the BigSquareBox style and the bounding rectangle
      becomes very small.
    * Fixed problem in PaintGradient where the outer edge of the box-style
      gradients (e.g. gdSquareBox) would not get painted.
  ------------------------------------------------------------------------------
  3.0.11 (12 Dec 2004)
    * Fixed problem where PaintGradient would display a white rectangle if the
      starting and stopping colors were the same and the gradient direction was
      one of the "box" styles.
  ------------------------------------------------------------------------------
  3.0.6  (11 Apr 2003)
    * Modified solution introduced in 3.0.5 to prevent repainting by checking
      that the control is operating at runtime, and not design-time.
    * Fixed problem where under certain circumstances, cascading access
      violations could occur within the IDE.
  ------------------------------------------------------------------------------
  3.0.5  (24 Mar 2003)
    * Fixed problem where DrawParentImage did not check for Control.Parent being
      a TCustomFrame. In previous builds, if a control that needed to be
      transparent was placed on a frame, and then an inline instance of the
      frame was placed on a panel, which was on a form, the frame would get
      repeatedly redrawn.
  ------------------------------------------------------------------------------
  3.0.4  (04 Mar 2003)
    * Fixed problem with FillGradDiag procedure not displaying gdDiagonalDown
      style correctly.
  ------------------------------------------------------------------------------
  3.0.3  (21 Jan 2003)
    * Fixed problem with FillGradDiag procedure not filling remaining part of
      upper triangle.
  ------------------------------------------------------------------------------
  3.0    (20 Dec 2002)
    * Added FlipBitmap procedure.
    * Added PaintGradient procedure (and supporting procedures).
    * Added overloaded version of DrawParentImage that takes an HDC.
===============================================================================}

{$I RzComps.inc}

{$R-} { Turn off range checking }

unit RzGrafx;

interface

uses
  {$IFDEF USE_CS}
  CodeSiteLogging,
  {$ENDIF}
  Types,
  Windows,
  Messages,
  Classes,
  Graphics,
  Controls,
  Forms,
  StdCtrls,
  SysUtils,
  RzCommon;


procedure FlipBitmap( Bitmap: TBitmap );
procedure TileBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
procedure TileRealizedBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
procedure StretchBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
procedure CenterBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );

procedure DrawFullTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect; Src: TRect;
                                     TransparentColor: TColor );
procedure DrawTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect; Src: TRect;
                                 TransparentColor: TColor );
procedure StretchTransparentBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor );
procedure TileTransparentBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor );


{===========================}
{== Gradient Declarations ==}
{===========================}

type
  TSmoothFactor = 1..8;

  TDWordColor = record
    ColorFract: Word;
    Color: Byte;
    Pad: Byte;
  end;

  TDWordWord = record
    FractPart: Word;
    WordPart: Word;
  end;


procedure BitFillBlit( DC: HDC; X0, Y0, W0, H0, W, H: Integer; ROP: DWord );
procedure BoxBitMirrorBlit( DC: HDC; X0, Y0, W0, H0: Integer; ROP: DWord );

procedure PaintGradient( Canvas: TCanvas; Bounds: TRect; GradDir: TGradientDirection; ColorStart, ColorStop: TColor;
                         SmoothFactor: TSmoothFactor = 1 );

procedure FillGrad( DC: HDC; AX, AY, L: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor = 1 );
procedure FillGradRect( DC: HDC; AX, AY, AX0, AY0: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor = 1 );
procedure FillGradDiag( DC: HDC; AW, AH: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor = 1 );


{==============================}
{== Trapezoidal Declarations ==}
{==============================}

type
  TTrapBevelStyle = ( bsNone, bsLowered, bsRaised );
  TTrapDirect = ( tdLargeToSmall, tdSmallToLarge );
  TTrapShape = ( tsLeft, tsCenter, tsRight );

  TTrap = record
    UpperLeft: TPoint;
    UpperRight: TPoint;
    LowerRight: TPoint;
    LowerLeft: TPoint;
  end;


procedure DrawTrapezoidBorder( Canvas: TCanvas; Trap: TTrap; Style: TTrapBevelStyle; BevelWidth: Integer );

procedure SetTrapezoid( var Trap: TTrap; Rect: TRect; TrapMin: Integer; TrapShape: TTrapShape;
                        Direct: TDirection; TrapDirect: TTrapDirect );
procedure InflateTrapezoid( var Trap: TTrap; Amount: Integer );

function AdjustTrapezoid( var Trap: TTrap; Rect: TRect; Direct: TDirection ): TTrap;
function Intersect( Pt1, Pt2, Pt3: TPoint; Direct: TDirection ): TPoint;



procedure DrawParentImage( Control: TControl; Dest: TCanvas; InvalidateParent: Boolean = False ); overload;
procedure DrawParentImage( Control: TControl; DC: HDC; InvalidateParent: Boolean = False ); overload;


implementation


uses
  GraphUtil,
  Themes;


procedure DrawFullTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect; Src: TRect;
                                     TransparentColor: TColor );
var
  MaskBmp, Bitmap: TBitmap;
  DestW, DestH, SrcW, SrcH: Integer;
begin
  DestW := Rect.Right - Rect.Left;
  DestH := Rect.Bottom - Rect.Top;

  SrcW := Src.Right - Src.Left;
  SrcH := Src.Bottom - Src.Top;

  MaskBmp := TBitmap.Create;
  try
    Bitmap := TBitmap.Create;
    try
      { Make memory Bitmap same size as client rect }

      Bitmap.Width  := DestW;
      Bitmap.Height := DestH;

      StretchBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH,
                  OrigBitmap.Canvas.Handle, Src.Left, Src.Top, SrcW, SrcH, SRCCOPY );

      MaskBmp.Monochrome := True;
      MaskBmp.Height := DestH;
      MaskBmp.Width := DestW;

      { Build mask based on transparent color. }

      SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );

      BitBlt( MaskBmp.Canvas.Handle, 0, 0, DestW, DestH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY );

      TransparentStretchBlt( Canvas.Handle, 0, 0, DestW, DestH,
                             Bitmap.Canvas.Handle, 0, 0, SrcW, SrcH,
                             MaskBmp.Canvas.Handle, 0, 0 );

    finally
      Bitmap.free;
    end;
  finally
    MaskBmp.free;
  end;
end; {= DrawFullTransparentBitmap =}


procedure DrawTransparentBitmap( Canvas: TCanvas; OrigBitmap: TBitmap; Rect: TRect;
                                 Src: TRect; TransparentColor: TColor );
var
  MaskBmp, Bitmap: TBitmap;
  DestW, DestH, SrcW, SrcH: Integer;
begin
  DestW := Rect.Right - Rect.Left;
  DestH := Rect.Bottom - Rect.Top;

  SrcW := Src.Right - Src.Left;
  SrcH := Src.Bottom - Src.Top;

  MaskBmp := TBitmap.Create;
  try
    Bitmap := TBitmap.Create;
    try
      { Make memory Bitmap same size as client rect }

      Bitmap.Width  := DestW;
      Bitmap.Height := DestH;

      StretchBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH,
                  OrigBitmap.Canvas.Handle, Src.Left, Src.Top, SrcW, SrcH, SRCCOPY );

      MaskBmp.Monochrome := True;
      MaskBmp.Height := DestH;
      MaskBmp.Width := DestW;

      { Build mask based on transparent color. }

      SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );

      BitBlt( MaskBmp.Canvas.Handle, 0, 0, DestW, DestH, Bitmap.Canvas.Handle, 0, 0, SRCCOPY );

      { If transparent color is black, the Bitmap is ready for use.
        Otherwise, put black in the Right place for masking. }
      if TransparentColor <> clBlack then
      begin
        SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( clBlack ) );
        SetTextColor( Bitmap.Canvas.Handle, ColorToRGB( clWhite ) );
        BitBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH, MaskBmp.Canvas.Handle, 0, 0, SRCAND );
      end;

      Src := Bounds( 0, 0, DestW, DestH );

      Canvas.CopyMode := cmSrcAnd;
      Canvas.CopyRect( Rect, MaskBmp.Canvas, Src );

      Canvas.CopyMode := cmSrcPaint;
      Canvas.CopyRect( Rect, Bitmap.Canvas, Src );

      { restore orig. Bitmap. }
      if TransparentColor <> clBlack then
      begin
        SetBkColor( Bitmap.Canvas.Handle, ColorToRGB( TransparentColor ) );
        SetTextColor( Bitmap.Canvas.Handle, ColorToRGB( clBlack ) );
        BitBlt( Bitmap.Canvas.Handle, 0, 0, DestW, DestH, MaskBmp.Canvas.Handle, 0, 0, SRCPAINT );
      end;
    finally
      Bitmap.free;
    end;
  finally
    MaskBmp.free;
  end;
end; {= DrawTransparentBitmap =}


procedure TileTransparentBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor  );
var
  X, Y: Integer;
  Dest, Src: TRect;
begin
  Src.Left := 0;
  Src.Top := 0;

  Y := Rect.Top;
  while Y < Rect.Bottom do
  begin
    X := Rect.Left;
    while X < Rect.Right do
    begin
      Dest.Left := X;
      Dest.Top := Y;
      Dest.Right := X + Min( Bitmap.Width, Rect.Right - X );
      Dest.Bottom := Y + Min( Bitmap.Height, Rect.Bottom - Y );

      Src.Right := Dest.Right - Dest.Left;
      Src.Bottom := Dest.Bottom - Dest.Top;

      DrawTransparentBitmap( Canvas, Bitmap, Dest, Src, TransparentColor );
      X := X + Bitmap.Width;
    end;
    Y := Y + Bitmap.Height;
  end;
end;


procedure StretchTransparentBitmap( Canvas:TCanvas; Bitmap: TBitmap; Rect: TRect; TransparentColor: TColor );
var
  Src: TRect;
begin
  { This function requires drawing the entire Bitmap each time... Can't draw just
    subset of the image. }

  Src.Top    := 0;
  Src.Left   := 0;
  Src.Right  := Bitmap.Width;
  Src.Bottom := Bitmap.Height;

  DrawTransparentBitmap( Canvas, Bitmap, Rect, Src, TransparentColor );
end;



procedure FlipBitmap( Bitmap: TBitmap );
var
  X, Y, W, H: Integer;
  NewBmp: TBitmap;
begin
  NewBmp := TBitmap.Create;
  W := Bitmap.Width;
  H := Bitmap.Height;

  NewBmp.Width := W;
  NewBmp.Height := H;

  try
    for Y := 0 to H - 1 do
    begin
      for X := 0 to W - 1 do
        NewBmp.Canvas.Pixels[ X, H - Y ] := Bitmap.Canvas.Pixels[ X, Y ];
    end;
    Bitmap.Assign( NewBmp );

  finally
    NewBmp.Free;
  end;
end;


procedure TileBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
var
  X, Y: Integer;
  Dest, Src: TRect;
begin
  Src.Left := 0;
  Src.Top := 0;

  Y := Rect.Top;
  while Y < Rect.Bottom do
  begin
    X := Rect.Left;
    while X < Rect.Right do
    begin
      Dest.Left := X;
      Dest.Top := Y;
      Dest.Right := X + Min( Bitmap.Width, Rect.Right - X );
      Dest.Bottom := Y + Min( Bitmap.Height, Rect.Bottom - Y );

      Src.Right := Dest.Right - Dest.Left;
      Src.Bottom := Dest.Bottom - Dest.Top;

      Canvas.CopyRect( Dest, Bitmap.Canvas, Src );
      X := X + Bitmap.Width;
    end;
    Y := Y + Bitmap.Height;
  end;
end;

procedure TileRealizedBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
var
  X, Y: Integer;
  Dest, Src: TRect;
begin
  Src.Left := Rect.Left mod Bitmap.Width;
  Src.Top := Rect.Top mod Bitmap.Height;

  Y := Rect.Top;
  while Y < Rect.Bottom do
  begin
    X := Rect.Left;
    while X < Rect.Right do
    begin
      Dest.Left := X;
      Dest.Top := Y;
      Dest.Right := X + Min( Bitmap.Width - Src.Left, Rect.Right - X );
      Dest.Bottom := Y + Min( Bitmap.Height - Src.Top, Rect.Bottom - Y );

      Src.Right := Dest.Right - Dest.Left + Src.Left;
      Src.Bottom := Dest.Bottom - Dest.Top + Src.Top;

      Canvas.CopyRect( Dest, Bitmap.Canvas, Src );

      X := X + Bitmap.Width-Src.Left;
      Src.Left := 0;
    end;
    Y := Y + Bitmap.Height - Src.Top;
    Src.Top := 0;
    Src.Left := Rect.Left mod Bitmap.Width;
  end;
end; {= TileRealizedBitmap =}


procedure CenterBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
var
  CenterX, CenterY, BCenterX, BCenterY, W, H: Integer;
  Dest, Src: TRect;
begin
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;

  CenterX := W div 2;
  CenterY := H div 2;

  BCenterX := Bitmap.Width  div 2;
  BCenterY := Bitmap.Height div 2;

  if CenterY > BCenterY then
  begin
    Src.Top := 0;
    Src.Bottom := Bitmap.Height;
  end
  else
  begin
    Src.Top := BCenterY - CenterY;
    Src.Bottom := Src.Top + H;
  end;


  if CenterX > BCenterX then
  begin
    Src.Left := 0;
    Src.Right := Bitmap.Width;
  end
  else
  begin
    Src.Left := BCenterX - CenterX;
    Src.Right := Src.Left + W;
  end;

  Dest.Left := Max( Rect.Left, CenterX - BCenterX  );
  Dest.Top := Max( Rect.Top, CenterY - BCenterY  );
  Dest.Right := Min( Rect.Right, CenterX + BCenterX  );
  Dest.Bottom := Min( Rect.Bottom, CenterY + BCenterY  );

  Canvas.CopyRect( Dest, Bitmap.Canvas, Src );
end; {= CenterBitmap =}


procedure StretchBitmap( Canvas: TCanvas; Bitmap: TBitmap; Rect: TRect );
var
  Src: TRect;
begin
  { This function requires drawing the entire Bitmap each time...
    Can't draw just subset of the image. }

  Src.Top := 0;
  Src.Left := 0;
  Src.Right := Bitmap.Width;
  Src.Bottom := Bitmap.Height;
  Canvas.CopyRect( Rect, Bitmap.Canvas, Src );
end;


{=======================================================================================================================
  BitFillBlit

  Make copies of the w x h bitmap in aDC located at X0,Y0. Fill the W0 x H0 rectange located at X0,Y0 with the copies.
  Using a binary fill pattern, each succeeding BitBlt copies twice as many pixels.
    BitBlt #1 copies one bitmap in Y direction.
    BitBlt #2 copies two bitmaps X direction.
    BitBlt #3 copies four bitmaps Y direction.
    BitBlt #4 copies eight bitmaps X direction.
    and so on ...
  The last BitBlt in Y or X copies just enough pixels to fill the remaining space.
=======================================================================================================================}

procedure BitFillBlit( DC: HDC; X0, Y0, W0, H0, W, H: Integer; ROP: DWord );
var
  HH, WW, XD, YD: Integer;
  YDir: Boolean;
begin
  YDir := H < H0;                             { True means copY in Y direction }
  while ( H < H0 ) or ( W < W0 ) do
  begin
    HH := H;
    WW := W;
    if YDir then
    begin                                                { Copy to space below }
      XD := X0;
      YD := Y0 + HH;
      Inc( H, HH );
      if ( H > H0 ) then
      begin
        HH := H0 - YD;
        H := H0;
      end;
      YDir := not ( W < W0 );
    end
    else
    begin                                             { Copy to space at right }
      XD := X0 + WW;
      YD := Y0;
      Inc( W, WW );
      if ( W > W0 ) then
      begin
        WW := W0 - XD;
        W := W0;
      end;
      YDir := ( H < H0 );
    end;
    BitBlt( DC, XD, YD, WW, HH, DC, X0, Y0, ROP );
  end;
end; {= BitFillBlit =}


{=======================================================================================================================
  BoxBitMirrorBlit

  Create a copy of the bitmap in the upper left quadrant. First make a copy below the original, mirroring in Y.
  Then copy both images to the right, mirroring in X.  For 256 color palettes, gradient fills copy better if the
  W0 and H0 dimensions are odd. The left edge and bottom edge are NOT copied if W0 and H0 are odd. Try a square box
  gradient in rectangles with odd and even dimensions to see the difference.
=======================================================================================================================}

procedure BoxBitMirrorBlit( DC: HDC; X0, Y0, W0, H0: Integer; ROP: DWord );
var
  AdjW, H, W: Integer;
begin
  W := W0 shr 1;
  H := H0 shr 1;
  if ( W mod 2 ) = 0 then
    AdjW := 0
  else
    AdjW := 1; { Special if W0 an odd # }

  StretchBlt( DC, X0, Y0 + H0 - 1, W + 1, -H, DC, X0, Y0, W, H, ROP );
  StretchBlt( DC, X0 + W0 - 1, Y0, -W - AdjW, H0, DC, X0, Y0, W - AdjW, H0, ROP );
end;



procedure PaintGradientEx( Canvas: TCanvas; Bounds: TRect; GradDir: RzCommon.TGradientDirection; ColorStart, ColorStop: TColor;
                           SmoothFactor: TSmoothFactor = 1 );
var
  FBitmap: TBitmap;
  Width, Height: Integer;
begin
  ColorStart := ColorToRGB( ColorStart );
  ColorStop := ColorToRGB( ColorStop );

  FBitmap := TBitmap.Create;
  try
    Width := Abs( Bounds.Right - Bounds.Left );
    Height := Abs( Bounds.Bottom - Bounds.Top );
    if ( Width = 0 ) or ( Height = 0 ) then
      Exit;

    FBitmap.Width := Width;
    FBitmap.Height := Height;

    FBitmap.Canvas.Pen.Width := 1;
    FBitmap.Canvas.Pen.Style := psClear;
    FBitmap.Canvas.Pen.Mode := pmCopy;
    FBitmap.Canvas.Brush.Style := bsSolid;

    case GradDir of
      gdHorizontalEnd:
      begin
        FillGrad( FBitmap.Canvas.Handle, 0, -1, Height, ColorStart, ColorStop, SmoothFactor );
        BitFillBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, 8, Height, SRCCOPY );
      end;

      gdHorizontalCenter:
      begin
        FillGrad( FBitmap.Canvas.Handle, 0, Height div 2, Height, ColorStart, ColorStop, SmoothFactor );
        BitFillBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, 8, Height, SRCCOPY );
      end;

      gdHorizontalBox:
      begin
        FBitmap.Canvas.Brush.Color := ColorStart;
        FBitmap.Canvas.FillRect( Rect( 0, 0, Width, Height ) );
        FillGradRect( FBitmap.Canvas.Handle, Width, Height, Width shr 2, 0, ColorStart, ColorStop, SmoothFactor );
        BoxBitMirrorBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY );
      end;

      gdVerticalEnd:
      begin
        FillGrad( FBitmap.Canvas.Handle, -1, 0, Width, ColorStart, ColorStop, SmoothFactor );
        BitFillBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, Width, 8, SRCCOPY );
      end;

      gdVerticalCenter:
      begin
        FillGrad( FBitmap.Canvas.Handle, Width div 2, 0, Width, ColorStart, ColorStop, SmoothFactor );
        BitFillBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, Width, 8, SRCCOPY );
      end;

      gdVerticalBox:
      begin
        FillGradRect( FBitmap.Canvas.Handle, Width, Height, 0, Height shr 2, ColorStart, ColorStop, SmoothFactor );
        BoxBitMirrorBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY );
      end;

      gdSquareBox:
      begin
        FBitmap.Canvas.Brush.Color := ColorStart;
        FBitmap.Canvas.FillRect( Rect( 0, 0, Width, Height ) );
        FillGradRect( FBitmap.Canvas.Handle, Width, Height, 0, 0, ColorStart, ColorStop, SmoothFactor );
        BoxBitMirrorBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY );
      end;

      gdBigSquareBox:
      begin
        FillGradRect( FBitmap.Canvas.Handle, Width, Height, Width shr 2, Height shr 2, ColorStart, ColorStop, SmoothFactor );
        BoxBitMirrorBlit( FBitmap.Canvas.Handle, 0, 0, Width, Height, SRCCOPY );
      end;

      gdDiagonalUp:
      begin
        if ColorStart = ColorStop then
        begin
          FBitmap.Canvas.Brush.Color := ColorStart;
          FBitmap.Canvas.Rectangle( 0, 0, Width, Height );
        end
        else
          FillGradDiag( FBitmap.Canvas.Handle, Width, Height, ColorStart, ColorStop, SmoothFactor );
      end;

      gdDiagonalDown:
      begin
        if ColorStart = ColorStop then
        begin
          FBitmap.Canvas.Brush.Color := ColorStart;
          FBitmap.Canvas.Rectangle( 0, 0, Width, Height );
        end
        else
          FillGradDiag( FBitmap.Canvas.Handle, -Width, Height, ColorStart, ColorStop, SmoothFactor );
      end;
    end;

    Canvas.Draw( Bounds.Left, Bounds.Top, FBitmap );
  finally
    FBitmap.Free;
  end;
end;


procedure PaintGradient( Canvas: TCanvas; Bounds: TRect; GradDir: RzCommon.TGradientDirection; ColorStart, ColorStop: TColor;
                         SmoothFactor: TSmoothFactor = 1 );
begin
  if ( GradDir = gdHorizontalEnd ) or ( GradDir = gdVerticalEnd ) then
  begin
    if GradDir = gdHorizontalEnd then
      GradientFillCanvas( Canvas, ColorStart, ColorStop, Bounds, gdVertical )
    else
      GradientFillCanvas( Canvas, ColorStart, ColorStop, Bounds, gdHorizontal );
  end
  else
    PaintGradientEx( Canvas, Bounds, GradDir, ColorStart, ColorStop, SmoothFactor );
end;




{===============================================================================
  FillGrad

  Make a gradient the Width or Height of a Windows Brush
  If AX = -1, build a bottom to top gradient, ColorFinal @ bottom
  If AX > 0, build right-edge to AX to left-edge gradient, ColorFinal @ AX
  If AY = -1, built a right to left gradient, ColorFinal @ right
  If AY > 0, build bottom-edge to AY to top-edge gradient, ColorFinal @ AY
  Either AX or AY must be zero, FillGrad constructs Horizontal OR Vertical
  gradients.
  The Windows brush is used for gradients because it will dither a 64 bit
  brush for 8 bit or 4 bit color systems that approximates a 24 bit Palatte.
===============================================================================}

procedure FillGrad( DC: HDC; AX, AY, L: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor );
label
  fgLoop;
var
  C, CI, CF, CPrev: TColor;
  BlueInc, GreenInc, RedInc: TColor;

  BlueDWord: TDWordColor;
  GreenDWord: TDWordColor;
  RedDWord: TDWordColor;
  BlueInt: Integer absolute BlueDWord;
  GreenInt: Integer absolute GreenDWord;
  RedInt: Integer absolute RedDWord;
  EndX, EndY, N, X, Y: Integer;
  R: TRect;
  Brush: HBrush;
  BGradX: Boolean;
  Q, Q0: Integer; { Quality (ie speed) }
begin
  X := 0;
  Y := 0;
  EndX := 0;
  EndY := 0;

  if ( AX <> 0 ) then
  begin                                                            { Do X grad }
    AY := 0;                             { Just in case; do EITHER X or Y grad }
    BGradX := True;
    if ( AX < 0 ) then
      X := L
    else
      X := AX + 1;
    EndX := 1;
  end
  else
  begin
    BGradX := False;
    if ( AY < 0 ) then
      Y := L
    else
      Y := AY + 1;
    EndY := 1;
  end;
  CI := ColorStart;
  CF := ColorStop;         { Start color is at top, or left; or outside edges }
  Q0 := SmoothFactor;
  if ( Q0 > 8 ) then
    Q0 := 1 + 255 div Q0;
  Dec( Q0 );
  if ( Q0 < 0 ) then
    Q0 := 0;

fgLoop:
  if BGradX then
    N := X - EndX
  else
    N := Y - EndY;
  Inc( N );
  RedInt := Integer( GetRValue( CF ) ) shl 16;                  { Setup color differences }
  GreenInt := Integer( GetGValue( CF ) ) shl 16;
  BlueInt := Integer( GetBValue( CF ) ) shl 16;

  if ( N > 0 ) then
  begin
    RedInc := ( ( Integer( GetRValue( CI ) ) shl 16 ) - RedInt ) div N;
    GreenInc := ( ( Integer( GetGValue( CI ) ) shl 16 ) - GreenInt ) div N;
    BlueInc := ( ( Integer( GetBValue( CI ) ) shl 16 ) - BlueInt ) div N;
  end
  else
  begin
    RedInc := 0;
    GreenInc := 0;
    BlueInc := 0;
  end;
  C := CF;
  if BGradX then
  begin
    { Fill an 8 pixel high row }
    R.Top := 0;
    R.Bottom := 8;
    repeat
      R.Right := X;
      CPrev := C;
      Q := Q0;
      repeat
        Inc( BlueInt, BlueInc );
        Inc( GreenInt, GreenInc );
        Inc( RedInt, RedInc );

        {$IFDEF WIN64}
        C := RGB( RedDWord.Color, GreenDWord.Color, BlueDWord.Color );
        {$ELSE}
        C := BlueInt;
        asm                          { Lot quicker than RGBColor }
          MOV EAX,C
          MOV AH,GreenDWord.Color
          MOV AL,RedDWord.Color
          MOV C,EAX
        end;
        {$ENDIF}

        Dec( X );
        Dec( Q );
      until ( ( C <> CPrev ) and ( Q < 0 ) ) or ( X < EndX );

      R.Left := X;
      Brush := CreateSolidBrush( CPrev ); { Dithered brush for 16|256 clr systems }
      FillRect( DC, R, Brush );
      DeleteObject( Brush );
    until ( X < EndX );
  end
  else
  begin
    { Fill an 8 pixel wide column }
    R.Left := 0;
    R.Right := 8;
    repeat
      R.Bottom := Y;
      CPrev := C;
      Q := Q0;
      repeat
        Inc( BlueInt, BlueInc );
        Inc( GreenInt, GreenInc );
        Inc( RedInt, RedInc );

        {$IFDEF WIN64}
        C := RGB( RedDWord.Color, GreenDWord.Color, BlueDWord.Color );
        {$ELSE}
        C := BlueInt;
        asm
          MOV EAX,C
          MOV AH,GreenDWord.Color
          MOV AL,RedDWord.Color
          MOV C,EAX
        end;
        {$ENDIF}

        Dec( Y );
        Dec( Q );
      until ( ( C <> CPrev ) and ( Q < 0 ) ) or ( Y < EndY );
      R.Top := Y;
      Brush := CreateSolidBrush( CPrev ); { Dithered brush for 16|256 clr systems }
      FillRect( DC, R, Brush );
      DeleteObject( Brush );
    until ( Y < EndY );
  end;
  CI := ColorStop;
  CF := ColorStart;

  if AX > 0 then
  begin
    { Do other GradX portion of inside-to-edges gradient }
    EndX := AX + 2;
    X := L;
    AX := 0;
    goto fgLoop;
  end;

  if ( AY > 0 ) then
  begin
    { Do other GradY portion }
    EndY := AY + 2;
    Y := L;
    AY := 0;
    goto fgLoop;
  end;
end; {= FillGrad =}


{===============================================================================
  FillGradRect

  ONLY the upper left quadrant is generated.  A call to BitMirrorBlit must be
  made to complete the other three quadrants.

    Square fill if AX0 and AY0 are 0
    Horz rect fill if AX0 > 0 and AY0 = 0
    Vert rect fill if AX0 = 0 and AY0 > 0
    BigSquare fill if AX0 and AY0 both > 0
===============================================================================}

procedure FillGradRect( DC: HDC; AX, AY, AX0, AY0: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor );
var
  C, CI, CF, CPrev: TColor;

  BlueDWord: TDWordColor;
  GreenDWord: TDWordColor;
  RedDWord: TDWordColor;
  BlueInt: Integer absolute BlueDWord;
  GreenInt: Integer absolute GreenDWord;
  RedInt: Integer absolute RedDWord;
  BlueInc, GreenInc, RedInc: Integer;

  XInc, YInc: Integer;
  XDWord: TDWordWord;
  X: Integer absolute XDWord;
  YDWord: TDWordWord;
  Y: Integer absolute YDWord;
  N: Integer;
  R, H, V: TRect;
  Brush: HBrush;
  Q, Q0: Integer; { Quality (i.e. speed) }
begin
  if ( ColorStart = ColorStop ) or ( AX <= 4 ) or ( AY <= 4 ) then
  begin
    Brush := CreateSolidBrush( ColorStart );
    R := Rect( 0, 0, AX + 1, AY + 1 );
    FillRect( DC, R, Brush );
    DeleteObject( Brush );
    Exit;
  end;



  CI := ColorStart;
  CF := ColorStop; { Start color is at top, or left; or outside edges }
  Q0 := SmoothFactor;

  if ( Q0 > 8 ) then
    Q0 := 1 + 256 div Q0;
  Dec( Q0 );
  if ( Q0 < 0 ) then
    Q0 := 0;
  AX := ( AX - 1 ) shr 1;
  H.Right := AX + 1;
  V.Left := AX + 1;
  AY := ( AY - 1 ) shr 1;
  V.Bottom := AY + 1;
  H.Top := AY + 1;
  Dec( AX, AX0 );
  Dec( AY, AY0 );
  N := AX;
  if ( AY > N ) then
    N := AY;
  Inc( N );

  RedInt := Integer( GetRValue( CF ) ) shl 16;                  { Setup color differences }
  GreenInt := Integer( GetGValue( CF ) ) shl 16;
  BlueInt := Integer( GetBValue( CF ) ) shl 16;

  RedInc := ( ( Integer( GetRValue( CI ) ) shl 16 ) - RedInt ) div N;
  GreenInc := ( ( Integer( GetGValue( CI ) ) shl 16 ) - GreenInt ) div N;
  BlueInc := ( ( Integer( GetBValue( CI ) ) shl 16 ) - BlueInt ) div N;

  X := AX shl 16;
  Y := AY shl 16;
  XInc := X div N;
  YInc := Y div N;

  C := CF;
  repeat
    H.Bottom := H.Top;
    V.Top := H.Top;
    V.Right := V.Left;
    CPrev := C;
    Q := Q0;
    repeat
      Inc( BlueInt, BlueInc );
      Inc( GreenInt, GreenInc );
      Inc( RedInt, RedInc );
      Dec( X, XInc );
      Dec( Y, YInc );

      {$IFDEF WIN64}
      C := RGB( RedDWord.Color, GreenDWord.Color, BlueDWord.Color );
      {$ELSE}
      C := BlueInt;
      asm
        MOV EAX,C
        MOV AH,GreenDWord.Color
        MOV AL,RedDWord.Color
        MOV C,EAX
      end;
      {$ENDIF}

      Dec( Q );
    until ( ( C <> CPrev ) and ( Q < 0 ) ) or
          ( ( XDWord.WordPart > $7FFF ) and ( YDWord.WordPart > $7FFF ) );

    if ( XDWord.WordPart <= $7FFF ) or ( YDWord.WordPart <= $7FFF ) then
    begin
      Brush := CreateSolidBrush( CPrev );
      H.Left := XDWord.WordPart;
      V.Left := XDWord.WordPart;
      H.Top := YDWord.WordPart;

      if H.Top <> H.Bottom then
        FillRect( DC, H, Brush );

      if V.Left <> V.Right then
        FillRect( DC, V, Brush );

      DeleteObject( Brush );
    end;
  until ( XDWord.WordPart > $7FFF ) and ( YDWord.WordPart > $7FFF );

end; {= FillGradRect =}


{===============================================================================
  FillGradDiag

  Make a diagonal gradient
  If AW is positive, gradient goes up from lower left to upper right.
  If AW is negative, gradient goes down from upper left to lower right.
===============================================================================}

procedure FillGradDiag( DC: HDC; AW, AH: Integer; ColorStart, ColorStop: TColor; SmoothFactor: TSmoothFactor );
var
  C, CI, CF, CPrev: TColor;
  BlueDWord: TDWordColor;
  GreenDWord: TDWordColor;
  RedDWord: TDWordColor;

  BlueInt: Integer absolute BlueDWord;
  GreenInt: Integer absolute GreenDWord;
  RedInt: Integer absolute RedDWord;
  BlueInc, GreenInc, RedInc: Integer;

  XInc, YInc: Integer;
  XDWord: TDWordWord;
  X: Integer absolute XDWord;
  YDWord: TDWordWord;
  Y: Integer absolute YDWord;
  N: Integer;
  P: array[ 1..4 ] of TPoint;
  Brush, BrushOld: HBrush;
  Q, Q0: Integer; { Quality (i.e. speed) }
begin
  if AW > 0 then
  begin
    CF := ColorStart;
    CI := ColorStop;
    X := 0;
  end
  else
  begin
    X := ( -AW ) shl 16;
    CI := ColorStart;
    CF := ColorStop;
  end;
  Q0 := SmoothFactor;
  if ( Q0 > 8 ) then
    Q0 := 1 + 255 div Q0;
  Dec( Q0 );
  if ( Q0 < 0 ) then
    Q0 := 0;
  N := Trunc( Sqrt( Sqr( AW ) + Sqr( AH ) ) );

  Inc( N );

  RedInt := Integer( GetRValue( CF ) ) shl 16;                  { Setup color differences }
  GreenInt := Integer( GetGValue( CF ) ) shl 16;
  BlueInt := Integer( GetBValue( CF ) ) shl 16;

  RedInc := ( ( Integer( GetRValue( CI ) ) shl 16 ) - RedInt ) div N;
  GreenInc := ( ( Integer( GetGValue( CI ) ) shl 16 ) - GreenInt ) div N;
  BlueInc := ( ( Integer( GetBValue( CI ) ) shl 16 ) - BlueInt ) div N;

  { Draw the bottom triangle first (either Lower-Right or Lower-Left) }

  N := N shr 1;
  if AW > 0 then
    XInc := ( AW shl 16 ) div N
  else
    XInc := ( -X ) div N;
  Y := AH shl 16;
  YInc := Y div N;

  C := CF;
  P[ 1 ].X := XDWord.WordPart;
  P[ 2 ].X := XDWord.WordPart;
  P[ 2 ].Y := AH;
  P[ 3 ].X := XDWord.WordPart;
  P[ 3 ].Y := AH;
  P[ 4 ].Y := AH;

  repeat
    CPrev := C;
    Q := Q0;
    repeat
      Inc( BlueInt, BlueInc );
      Inc( GreenInt, GreenInc );
      Inc( RedInt, RedInc );
      Inc( X, XInc );
      Dec( Y, YInc );

      {$IFDEF WIN64}
      C := RGB( RedDWord.Color, GreenDWord.Color, BlueDWord.Color );
      {$ELSE}
      C := BlueInt;
      asm
        MOV EAX,C
        MOV AH,GreenDWord.Color
        MOV AL,RedDWord.Color
        MOV C,EAX
      end;
      {$ENDIF}

      Dec( Q );
    until ( ( C <> CPrev ) and ( Q < 0 ) ) or ( YDWord.WordPart > $7FFF );

    if YDWord.WordPart <= $7FFF then
    begin
      P[ 1 ].Y := YDWord.WordPart;
      P[ 4 ].X := XDWord.WordPart;
      if ( P[ 1 ].Y < P[ 2 ].Y ) or ( P[ 4 ].X <> P[ 3 ].X ) then
      begin
        Brush := CreateSolidBrush( CPrev );
        BrushOld := SelectObject( DC, Brush );
        Windows.Polygon( DC, P, 4 );
        SelectObject( DC, BrushOld );
        DeleteObject( Brush );
        P[ 2 ].Y := P[ 1 ].Y;
        P[ 3 ].X := P[ 4 ].X;
      end;
    end;
  until ( YDWord.WordPart > $7FFF );

  { Fill remaining portion of lower triangle }
  { This is necessary so that upper triangle portion starts and correct spot }
  P[ 1 ].Y := 0;
  if AW > 0 then
    P[ 4 ].X := AW
  else
    P[ 4 ].X := 0;
  if ( P[ 1 ].Y < P[ 2 ].Y ) or ( P[ 4 ].X <> P[ 3 ].X ) then
  begin
    Brush := CreateSolidBrush( CPrev );
    BrushOld := SelectObject( DC, Brush );
    Windows.Polygon( DC, P, 4 );
    SelectObject( DC, BrushOld );
    DeleteObject( Brush );
    P[ 2 ].Y := P[ 1 ].Y;
    P[ 3 ].X := P[ 4 ].X;
  end;

  { Draw the top triangle next (either the Upper-Left or Upper-Right) }

  if ( AW > 0 ) then
    X := 0
  else
    X := ( -AW ) shl 16;
  Y := AH shl 16;

  repeat
    CPrev := C;
    Q := Q0;
    repeat
      Inc( BlueInt, BlueInc );
      Inc( GreenInt, GreenInc );
      Inc( RedInt, RedInc );
      Inc( X, XInc );
      Dec( Y, YInc );

      {$IFDEF WIN64}
      C := RGB( RedDWord.Color, GreenDWord.Color, BlueDWord.Color );
      {$ELSE}
      C := BlueInt;
      asm
        MOV EAX,C
        MOV AH,GreenDWord.Color
        MOV AL,RedDWord.Color
        MOV C,EAX
      end;
      {$ENDIF}

      Dec( Q );
    until ( ( C <> CPrev ) and ( Q < 0 ) ) or ( YDWord.WordPart > $7FFF );

    if YDWord.WordPart <= $7FFF then
    begin
      P[ 1 ].X := XDWord.WordPart;
      P[ 4 ].Y := YDWord.WordPart;
      if ( P[ 1 ].X <> P[ 2 ].X ) or ( P[ 4 ].Y < P[ 3 ].Y ) then
      begin
        Brush := CreateSolidBrush( CPrev );
        BrushOld := SelectObject( DC, Brush );
        Windows.Polygon( DC, P, 4 );
        SelectObject( DC, BrushOld );
        DeleteObject( Brush );
        P[ 2 ].X := P[ 1 ].X;
        P[ 3 ].Y := P[ 4 ].Y;
      end;
    end;
  until ( YDWord.WordPart > $7FFF );

  // Fill in remaining part of upper triangle
  P[ 1 ].X := XDWord.WordPart;
  if AW > 0 then
    P[ 4 ].Y := AW
  else
    P[ 4 ].Y := 0;
  if ( P[ 1 ].X <> P[ 2 ].X ) or ( P[ 4 ].Y < P[ 3 ].Y ) then
  begin
    Brush := CreateSolidBrush( CPrev );
    BrushOld := SelectObject( DC, Brush );
    Windows.Polygon( DC, P, 4 );
    SelectObject( DC, BrushOld );
    DeleteObject( Brush );
    P[ 2 ].X := P[ 1 ].X;
    P[ 3 ].Y := P[ 4 ].Y;
  end;

end; {= FillGradDiag =}



{==========================================}
{== Trapezoidal Procedures and Functions ==}
{==========================================}

procedure DrawTrapezoidBorder( Canvas: TCanvas; Trap: TTrap; Style: TTrapBevelStyle; BevelWidth: Integer );
var
  I: Integer;
  OldPen: TPen;
begin
  OldPen := Canvas.Pen;
  Canvas.Pen.Width := 1;

  for I := 0 to (bevelWidth-1) do
  begin
    if Style = bsLowered then
      Canvas.Pen.Color := clBtnHighlight
    else
      Canvas.Pen.Color := clBtnShadow;

    { Right Line }
    Canvas.MoveTo( Trap.UpperRight.X - I - 1, Trap.UpperRight.Y + I );
    Canvas.LineTo( Trap.LowerRight.X - I - 1, Trap.LowerRight.Y - I - 1 );
    { Bottom Line }
    Canvas.LineTo( Trap.LowerLeft.X + I, Trap.LowerLeft.Y - I - 1 );

    if Style = bsLowered then
      Canvas.Pen.Color := clBtnShadow
    else
      Canvas.Pen.Color := clBtnHighlight;

    { Left Line }
    Canvas.LineTo( Trap.UpperLeft.X + I, Trap.UpperLeft.Y + I );
    { Top Line }
    Canvas.LineTo( Trap.UpperRight.X - I - 1, Trap.UpperRight.Y + I );
  end;

  Canvas.Pen := OldPen;
end;


procedure SetTrapezoid( var Trap: TTrap; Rect: TRect; TrapMin: Integer; TrapShape: TTrapShape; Direct: TDirection;
                        TrapDirect: TTrapDirect );
var
  W: Integer;
  H: Integer;
  MinTrap: Integer;
  Offset: Integer;
begin
  W := Rect.Right - Rect.Left;
  H := Rect.Bottom - Rect.Top;

  case Direct of
    dirUp, dirDown:
    begin
      MinTrap := Min( W, TrapMin );

      Trap.UpperLeft.Y := Rect.Top;
      Trap.UpperRight.Y := Rect.Top;
      Trap.LowerLeft.Y := Rect.Bottom;
      Trap.LowerRight.Y := Rect.Bottom;

      case TrapShape of
        tsLeft:
          Offset := 0;
        tsCenter:
          Offset := ( W - MinTrap ) div 2;
        tsRight:
          Offset := W - MinTrap;
      else
        Offset := 0;
      end;

      if ( ( TrapDirect = tdLargeToSmall ) and ( Direct = dirUp ) ) or
         ( ( TrapDirect = tdSmallToLarge ) and ( Direct = dirDown ) ) then
      begin
        Trap.UpperLeft.X := Rect.Left + Offset;
        Trap.UpperRight.X := Trap.UpperLeft.X + MinTrap;
        Trap.LowerLeft.X := Rect.Left;
        Trap.LowerRight.X := Rect.Right;
      end
      else
      begin
        Trap.UpperLeft.X := Rect.Left;
        Trap.UpperRight.X := Rect.Right;
        Trap.LowerLeft.X := Rect.Left + Offset;
        Trap.LowerRight.X := Trap.LowerLeft.X + MinTrap;
      end;
    end;

    dirLeft, dirRight:
    begin
      MinTrap := Min( H, TrapMin );

      Trap.UpperLeft.X := Rect.Left;
      Trap.UpperRight.X := Rect.Right;
      Trap.LowerLeft.X := Rect.Left;
      Trap.LowerRight.X := Rect.Right;

      case TrapShape of
        tsLeft:
          Offset := 0;
        tsCenter:
          Offset := ( H - MinTrap ) div 2;
        tsRight:
          Offset := H - MinTrap;
      else
        Offset := 0;
      end;

      if ( ( TrapDirect = tdLargeToSmall ) and ( Direct = dirRight ) ) or
         ( ( TrapDirect = tdSmallToLarge ) and ( Direct = dirLeft ) ) then
      begin
        Trap.UpperRight.Y := Rect.Top + Offset;
        Trap.LowerRight.Y := Trap.UpperRight.Y + MinTrap;
        Trap.UpperLeft.Y := Rect.Top;
        Trap.LowerLeft.Y := Rect.Bottom;
      end
      else
      begin
        Trap.UpperLeft.Y := Rect.Top + Offset;
        Trap.LowerLeft.Y := Trap.UpperLeft.Y + MinTrap;
        Trap.UpperRight.Y := Rect.Top;
        Trap.LowerRight.Y := Rect.Bottom;
      end;
    end;
  end;
end; {= SetTrapezoid =}


function AdjustTrapezoid( var Trap: TTrap; Rect: TRect; Direct: TDirection ): TTrap;
begin
  if ( Direct = dirLeft ) or ( Direct = dirRight ) then
  begin
    Result.UpperLeft := Intersect( Trap.UpperLeft, Trap.UpperRight, Point( Rect.Left, 0 ), Direct );
    Result.LowerLeft := Intersect( Trap.LowerLeft, Trap.LowerRight, Point( Rect.Left, 0 ), Direct );

    Result.UpperRight := Intersect( Trap.UpperLeft, Trap.UpperRight, Point( Rect.Right, 0 ), Direct );
    Result.LowerRight := Intersect( Trap.LowerLeft, Trap.LowerRight, Point( Rect.Right, 0 ), Direct );
  end
  else
  begin
    Result.UpperLeft := Intersect( Trap.UpperLeft, Trap.LowerLeft, Point( 0, Rect.Top ), Direct );
    Result.LowerLeft := Intersect( Trap.UpperLeft, Trap.LowerLeft, Point( 0, Rect.Bottom ), Direct );

    Result.UpperRight := Intersect( Trap.UpperRight, Trap.LowerRight, Point( 0, Rect.Top ), Direct );
    Result.LowerRight := Intersect( Trap.UpperRight, Trap.LowerRight, Point( 0, Rect.Bottom ), Direct );
  end;
end;


{===============================================================================
  Calculates the intersecting Point of two line segments
===============================================================================}

function Intersect( Pt1: TPoint; Pt2: TPoint; Pt3: TPoint; Direct: TDirection ): TPoint;
begin
  if ( Direct = dirLeft ) or ( Direct = dirRight ) then
  begin
    Intersect.X := Pt3.X;
    Intersect.Y := Trunc( Pt2.Y - ( ( Pt2.Y - Pt1.Y ) / ( Pt2.X - Pt1.X ) ) * ( Pt2.X - Pt3.X ) );
  end
  else
  begin
    Intersect.Y := Pt3.Y;
    Intersect.X := Trunc( Pt2.X - ( ( Pt2.X - Pt1.X ) / ( Pt2.Y - Pt1.Y ) ) * ( Pt2.Y - Pt3.Y ) );
  end
end;


procedure InflateTrapezoid( var Trap: TTrap; Amount: Integer );
begin
  Trap.UpperLeft.X := Trap.UpperLeft.X - Amount;
  Trap.UpperLeft.Y := Trap.UpperLeft.Y - Amount;

  Trap.UpperRight.X := Trap.UpperRight.X + Amount;
  Trap.UpperRight.Y := Trap.UpperRight.Y - Amount;

  Trap.LowerLeft.X := Trap.LowerLeft.X - Amount;
  Trap.LowerLeft.Y := Trap.LowerLeft.Y + Amount;

  Trap.LowerRight.X := Trap.LowerRight.X + Amount;
  Trap.LowerRight.Y := Trap.LowerRight.Y + Amount;
end;



procedure DrawParentImage( Control: TControl; Dest: TCanvas; InvalidateParent: Boolean = False );
begin
  DrawParentImage( Control, Dest.Handle, InvalidateParent );
end;


procedure DrawParentImage( Control: TControl; DC: HDC; InvalidateParent: Boolean = False );
var
  SaveIndex: Integer;
  P: TPoint;
begin
  if Control.Parent = nil then
    Exit;
  SaveIndex := SaveDC( DC );
  GetViewportOrgEx( DC, P );

  SetViewportOrgEx( DC, P.X - Control.Left, P.Y - Control.Top, nil );
  IntersectClipRect( DC, 0, 0, Control.Parent.ClientWidth, Control.Parent.ClientHeight );

  if not ( csDesigning in Control.ComponentState ) then
  begin
    Control.Parent.Perform( wm_EraseBkgnd, DC, 0 );
    Control.Parent.Perform( wm_PrintClient, DC, prf_Client );
  end
  else
  begin
    // Wrapping the following calls in a try..except is necessary to prevent
    // cascading access violations in the Form Designer (and ultimately the
    // shutting down of the IDE) in the Form Designer under the following
    // specific condition:
    //   1. Control on Form Designer supports Transparency (thus this procedure
    //      is called).
    //   2. Control is selected in the Form Designer such that grab handles are
    //      visible.
    //   3. User selects File|Close All Files, or Creates a New Application
    //      (i.e. Anything that closes the current project).
    //   4. Cascading access violations are created inside the IDE Form Designer
    //
    // The same problem also occurs in Delphi 7 under Windows XP if you add a
    // Delphi32.exe.manifest to the Delphi\Bin folder. This will cause controls
    // such as TPanel to appear transparent when on the Form Designer. Repeating
    // the steps above, will result in the cascading access violations as
    // described above.

    try
      Control.Parent.Perform( wm_EraseBkgnd, DC, 0 );
      Control.Parent.Perform( wm_PrintClient, DC, prf_Client );
    except
    end;
  end;

  RestoreDC( DC, SaveIndex );

  if InvalidateParent then
  begin
    if not ( Control.Parent is TCustomControl ) and
       not ( Control.Parent is TCustomForm ) and
       not ( csDesigning in Control.ComponentState ) then
    begin
      Control.Parent.Invalidate;
    end;
  end;
end;


end.


unit PXL.Canvas.SRT;
(*
 * This file is part of Asphyre Framework, also known as Platform eXtended Library (PXL).
 * Copyright (c) 2015 - 2017 Yuriy Kotsarenko. All rights reserved.
 *
 * Licensed under the Apache License, Version 2.0 (the "License"); you may not use this file except in
 * compliance with the License. You may obtain a copy of the License at
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software distributed under the License is
 * distributed on an "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and limitations under the License.
 *)
interface

{$INCLUDE PXL.Config.inc}

uses
  PXL.Types, PXL.Surfaces, PXL.Devices, PXL.Textures, PXL.Canvas;

type
  TSRTCanvas = class(TCustomCanvas)
  protected
    {$IFDEF AUTOREFCOUNT}[weak]{$ENDIF} FSurface: TConceptualPixelSurface;
    FClipRect: TIntRect;

    function NeedsInitialization: Boolean; override;

    function BeginDraw: Boolean; override;
    procedure EndDraw; override;

    function GetClipRect: TIntRect; override;
    procedure SetClipRect(const Value: TIntRect); override;
  public
    procedure PutPixel(const Point: TPoint2f; const Color: TIntColor); override;
    procedure Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair); override;

    procedure DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
      const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
      const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
      const BlendingEffect: TBlendingEffect = TBlendingEffect.Normal); override;

    procedure Flush; override;
  end;

implementation

uses
  PXL.Types.SRT, PXL.Rasterizer.SRT, PXL.Textures.SRT;

function TSRTCanvas.NeedsInitialization: Boolean;
begin
  Result := False;
end;

function TSRTCanvas.BeginDraw: Boolean;
var
  Context: TSRTDeviceContext;
begin
  if (Device = nil) or (not (Device.Context is TSRTDeviceContext)) then
    Exit(False);

  Context := TSRTDeviceContext(Device.Context);

  FSurface := Context.Surface;
  if FSurface = nil then
    Exit(False);

  FClipRect := IntRect(ZeroPoint2i, Context.SurfaceSize);

  Result := True;
end;

procedure TSRTCanvas.EndDraw;
begin
  FSurface := nil;
end;

function TSRTCanvas.GetClipRect: TIntRect;
begin
  Result := FClipRect;
end;

procedure TSRTCanvas.SetClipRect(const Value: TIntRect);
begin
  FClipRect := Value;
end;

procedure TSRTCanvas.Flush;
begin
end;

procedure TSRTCanvas.PutPixel(const Point: TPoint2f; const Color: TIntColor);
var
  IntPoint: TPoint2i;
begin
  if FSurface <> nil then
  begin
    IntPoint := Point.ToInt;
    if FClipRect.Contains(IntPoint) then
      FSurface.DrawPixelUnsafe(IntPoint, Color);
  end;
end;

procedure TSRTCanvas.Line(const SrcPoint, DestPoint: TPoint2f; const Color: TColorPair);
var
  SrcPt, DestPt, Delta, DrawPos: TPoint2i;
  FixedPos, FixedDelta, InitialPos, I, AlphaPos, AlphaDelta: Integer;
begin
  SrcPt := SrcPoint.ToInt;
  DestPt := DestPoint.ToInt;
  Delta.X := Abs(DestPt.X - SrcPt.X);
  Delta.Y := Abs(DestPt.Y - SrcPt.Y);

  if (Delta.X < 1) and (Delta.Y < 1) then
  begin
    if FClipRect.Contains(((SrcPoint + DestPoint) * 0.5).ToInt) then
      FSurface.DrawPixelUnsafe(SrcPt, AveragePixels(Color.First, Color.Second));

    Exit;
  end;

  if Delta.Y > Delta.X then
  begin
    InitialPos := SrcPt.Y;
    FixedDelta := Round((DestPoint.X - SrcPoint.X) * 65536.0) div Delta.Y;
    AlphaDelta := $FFFF div Delta.Y;

    if DestPt.Y < InitialPos then
    begin
      InitialPos := DestPt.Y;

      FixedPos :=  Round(DestPoint.X * 65536.0);
      FixedDelta := -FixedDelta;

      AlphaPos := $FFFF;
      AlphaDelta := -AlphaDelta;
    end
    else
    begin
      FixedPos := Round(SrcPoint.X * 65536.0);
      AlphaPos := 0;
    end;

    for I := 0 to Delta.Y - 1 do
    begin
      DrawPos := Point2i(FixedPos div 65536, InitialPos + I);

      if FClipRect.Contains(DrawPos) then
        FSurface.DrawPixelUnsafe(DrawPos, BlendPixels(Color.First, Color.Second, AlphaPos div 256));

      Inc(FixedPos, FixedDelta);
      Inc(AlphaPos, AlphaDelta);
    end;
  end
  else
  begin
    InitialPos := SrcPt.X;
    FixedDelta := Round((DestPoint.Y - SrcPoint.Y) * 65536.0) div Delta.X;
    AlphaDelta := $FFFF div Delta.X;

    if DestPt.X < InitialPos then
    begin
      InitialPos := DestPt.X;

      FixedPos :=  Round(DestPoint.Y * 65536.0);
      FixedDelta := -FixedDelta;

      AlphaPos := $FFFF;
      AlphaDelta := -AlphaDelta;
    end
    else
    begin
      FixedPos := Round(SrcPoint.Y * 65536.0);
      AlphaPos := 0;
    end;

    for I := 0 to Delta.X - 1 do
    begin
      DrawPos := Point2i(InitialPos + I, FixedPos div 65536);

      if FClipRect.Contains(DrawPos) then
        FSurface.DrawPixelUnsafe(DrawPos, BlendPixels(Color.First, Color.Second, AlphaPos div 256));

      Inc(FixedPos, FixedDelta);
      Inc(AlphaPos, AlphaDelta);
    end;
  end;
end;

procedure TSRTCanvas.DrawIndexedTriangles(const Vertices: PPoint2f; const Colors: PIntColor; const Indices: PLongInt;
  const VertexCount, TriangleCount: Integer; const BlendingEffect: TBlendingEffect);
var
  I: Integer;
  Index1, Index2, Index3: PLongInt;
  Vertex1, Vertex2, Vertex3: PPoint2f;
  Color1, Color2, Color3: PIntColor;
  Det: Single;
begin
  if (TriangleCount < 1) or (VertexCount < 3) or (FSurface = nil) then
    Exit;

  Index1 := Indices;
  Index2 := Pointer(PtrInt(Indices) + SizeOf(LongInt));
  Index3 := Pointer(PtrInt(Indices) + 2 * SizeOf(LongInt));

  for I := 0 to TriangleCount - 1 do
  begin
    Vertex1 := Pointer(PtrInt(Vertices) + Index1^ * SizeOf(TPoint2f));
    Vertex2 := Pointer(PtrInt(Vertices) + Index2^ * SizeOf(TPoint2f));
    Vertex3 := Pointer(PtrInt(Vertices) + Index3^ * SizeOf(TPoint2f));

    Color1 := Pointer(PtrInt(Colors) + Index1^ * SizeOf(TIntColor));
    Color2 := Pointer(PtrInt(Colors) + Index2^ * SizeOf(TIntColor));
    Color3 := Pointer(PtrInt(Colors) + Index3^ * SizeOf(TIntColor));

    Det := (Vertex1.X - Vertex3.X) * (Vertex2.Y - Vertex3.Y) - (Vertex2.X - Vertex3.X) * (Vertex1.Y - Vertex3.Y);
    if Det > 0 then
      DrawTriangle(FSurface, nil, Vertex3^, Vertex2^, Vertex1^, ZeroPoint2f, ZeroPoint2f, ZeroPoint2f, Color3^, Color2^,
        Color1^, FClipRect, BlendingEffect = TBlendingEffect.Add)
    else
      DrawTriangle(FSurface, nil, Vertex1^, Vertex2^, Vertex3^, ZeroPoint2f, ZeroPoint2f, ZeroPoint2f, Color1^, Color2^,
        Color3^, FClipRect, BlendingEffect = TBlendingEffect.Add);

    Index1 := Pointer(PtrInt(Index1) + 3 * SizeOf(LongInt));
    Index2 := Pointer(PtrInt(Index2) + 3 * SizeOf(LongInt));
    Index3 := Pointer(PtrInt(Index3) + 3 * SizeOf(LongInt));
  end;
end;

procedure TSRTCanvas.DrawTexturedTriangles(const Texture: TCustomBaseTexture; const Vertices, TexCoords: PPoint2f;
  const Colors: PIntColor; const Indices: PLongInt; const VertexCount, TriangleCount: Integer;
  const BlendingEffect: TBlendingEffect);
var
  I: Integer;
  Index1, Index2, Index3: PLongInt;
  Vertex1, Vertex2, Vertex3, TexCoord1, TexCoord2, TexCoord3: PPoint2f;
  Color1, Color2, Color3: PIntColor;
begin
  if (TriangleCount < 1) or (VertexCount < 3) or (not (Texture is TSRTLockableTexture)) or (FSurface = nil) then
    Exit;

  Index1 := Indices;
  Index2 := Pointer(PtrInt(Indices) + SizeOf(LongInt));
  Index3 := Pointer(PtrInt(Indices) + 2 * SizeOf(LongInt));

  for I := 0 to TriangleCount - 1 do
  begin
    Vertex1 := Pointer(PtrInt(Vertices) + Index1^ * SizeOf(TPoint2f));
    Vertex2 := Pointer(PtrInt(Vertices) + Index2^ * SizeOf(TPoint2f));
    Vertex3 := Pointer(PtrInt(Vertices) + Index3^ * SizeOf(TPoint2f));

    TexCoord1 := Pointer(PtrInt(TexCoords) + Index1^ * SizeOf(TPoint2f));
    TexCoord2 := Pointer(PtrInt(TexCoords) + Index2^ * SizeOf(TPoint2f));
    TexCoord3 := Pointer(PtrInt(TexCoords) + Index3^ * SizeOf(TPoint2f));

    Color1 := Pointer(PtrInt(Colors) + Index1^ * SizeOf(TIntColor));
    Color2 := Pointer(PtrInt(Colors) + Index2^ * SizeOf(TIntColor));
    Color3 := Pointer(PtrInt(Colors) + Index3^ * SizeOf(TIntColor));

    DrawTriangle(FSurface, TSRTLockableTexture(Texture).Surface, Vertex3^, Vertex2^, Vertex1^, TexCoord3^, TexCoord2^,
      TexCoord1^, Color3^, Color2^, Color1^, FClipRect, BlendingEffect = TBlendingEffect.Add);

    Index1 := Pointer(PtrInt(Index1) + 3 * SizeOf(LongInt));
    Index2 := Pointer(PtrInt(Index2) + 3 * SizeOf(LongInt));
    Index3 := Pointer(PtrInt(Index3) + 3 * SizeOf(LongInt));
  end;
end;

end.

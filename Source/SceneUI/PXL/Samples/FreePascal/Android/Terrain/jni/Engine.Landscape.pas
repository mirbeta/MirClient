unit Engine.Landscape;
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

uses
  PXL.TypeDef, PXL.Types;

type
  TLandscape = class
  public const
    MaxCorners = 4;

    MapWidth = 64;
    MapHeight = 128;

  public type
    TMapCorner = record
      Height: VectorInt;
      Light: VectorInt;
    end;

    PMapEntry = ^TMapEntry;
    TMapEntry = record
      Corners: array[0..MaxCorners - 1] of TMapCorner;
    end;
  private
    FHeightMap: array[0..(MapHeight div 2) - 1, 0..MapWidth - 1] of VectorFloat;
    FMapEntries: array[0..MapHeight - 1, 0..MapWidth - 1] of TMapEntry;
    FWaveAlpha: VectorFloat;
    FWaveBeta: VectorFloat;
    FWaveGamma: VectorFloat;

    function GetEntry(const X, Y: Integer): PMapEntry;
    procedure UpdateHeightMap;
    procedure UpdateEntryHeights;
  public
    constructor Create;

    procedure AnimateHeights;

    function GetTileHeightSafe(const IsoPos: TPoint2i; const Corner: VectorInt): VectorInt; inline;
    function GetTileLightSafe(const IsoPos: TPoint2i; const Corner: VectorInt): VectorInt; inline;

    property Entries[const X, Y: Integer]: PMapEntry read GetEntry;
  end;

{ Converts native isometric coordinates into 45-degree rotated linear 2D coordinates. }
function IsoToLinear(const IsoPos: TPoint2i): TPoint2i;

{ Converts 45-degree rotated linear 2D coordinates into true isometric coordinates. }
function LinearToIso(const LinePos: TPoint2i): TPoint2i;

{ Takes natural 2D pixel position and calculates native isometric coordinates of the tile underneath. }
function PositionToIso(const Position, TileSize: TPoint2i): TPoint2i;

{ Returns top/left 2D pixel position of isometric tile at the given native isometric coordinates. }
function IsoToPosition(const IsoPos, TileSize: TPoint2i): TPoint2i;

implementation

uses
  Math;

function IsoToLinear(const IsoPos: TPoint2i): TPoint2i;
begin
 Result.Y := (IsoPos.Y div 2) - IsoPos.X;
 Result.X := IsoPos.X + (IsoPos.Y mod 2) + (IsoPos.Y div 2);
end;

function LinearToIso(const LinePos: TPoint2i): TPoint2i;
begin
 Result.X := (LinePos.X  - LinePos.Y) div 2;
 Result.Y := LinePos.X + LinePos.Y;
end;

function PositionToIso(const Position, TileSize: TPoint2i): TPoint2i;
begin
 Result.Y := Position.Y div (TileSize.Y div 2);
 Result.X := (Position.X - ((Result.Y mod 2) * (TileSize.X div 2))) div TileSize.X;
end;

function IsoToPosition(const IsoPos, TileSize: TPoint2i): TPoint2i;
begin
 Result.Y := IsoPos.Y * (TileSize.Y div 2);
 Result.X := (IsoPos.X * TileSize.X) + ((IsoPos.Y mod 2) * (TileSize.X div 2));
end;

constructor TLandscape.Create;
begin
  inherited;

{  XView := 1280;
  YView := 768;
  XViewFloat := 1280;
  YViewFloat := 768;}

  UpdateHeightMap;
  UpdateEntryHeights;
end;

function TLandscape.GetEntry(const X, Y: Integer): PMapEntry;
begin
  if (X >= 0) and (Y >= 0) and (X < MapWidth) and (Y < MapHeight) then
    Result := @FMapEntries[Y, X]
  else
    Result := nil;
end;

procedure TLandscape.UpdateHeightMap;
var
  I, J: VectorInt;
begin
  for J := 0 to (MapHeight div 2) - 1 do
    for I := 0 to MapWidth - 1 do
      FHeightMap[J, I] := SineTransform((I / 8.0) + FWaveAlpha) + SineTransform((I / 4.0) + FWaveBeta) * 0.5 +
        SineTransform((J / 16.0) + FWaveGamma) * 0.5;
end;

procedure TLandscape.UpdateEntryHeights;
var
  I, J, DeltaX, CurHeight, CurLight: VectorInt;
  LinePos: TPoint2i;
begin
  for J := 1 to MapHeight - 2 do
    for I := 1 to MapWidth - 2 do
    begin
      DeltaX := 1 - (J mod 2);

      LinePos := IsoToLinear(Point2i(I, J + 64));
      Dec(LinePos.X, 64);

      if (LinePos.X >= 0) and (LinePos.Y >= 0) and (LinePos.X < MapWidth) and (LinePos.Y < MapHeight div 2) then
        CurHeight := Round(FHeightMap[LinePos.Y, LinePos.X] * 96.0) + 32
      else
        CurHeight := 0;

      CurLight := Min(CurHeight, 255);

      FMapEntries[J, I].Corners[0].Height := CurHeight;
      FMapEntries[J, I].Corners[0].Light := CurLight;
      FMapEntries[J + 1, I - DeltaX].Corners[1].Height := CurHeight;
      FMapEntries[J + 1, I - DeltaX].Corners[1].Light := CurLight;
      FMapEntries[J - 1, I - DeltaX].Corners[2].Height := CurHeight;
      FMapEntries[J - 1, I - DeltaX].Corners[2].Light := CurLight;
      FMapEntries[J, I - 1].Corners[3].Height := CurHeight;
      FMapEntries[J, I - 1].Corners[3].Light := CurLight;
    end;
end;

function TLandscape.GetTileHeightSafe(const IsoPos: TPoint2i; const Corner: VectorInt): VectorInt;
begin
  Result := FMapEntries[Saturate(IsoPos.Y, 0, MapHeight - 1),
    Saturate(IsoPos.X, 0, MapWidth - 1)].Corners[Corner].Height;
end;

function TLandscape.GetTileLightSafe(const IsoPos: TPoint2i; const Corner: VectorInt): VectorInt;
begin
  Result := FMapEntries[Saturate(IsoPos.Y, 0, MapHeight - 1),
    Saturate(IsoPos.X, 0, MapWidth - 1)].Corners[Corner].Light;
end;

procedure TLandscape.AnimateHeights;
begin
  FWaveAlpha := FWaveAlpha - 0.02;
  FWaveBeta := FWaveBeta - 0.0257;
  FWaveGamma := FWaveGamma - 0.033;

  UpdateHeightMap;
  UpdateEntryHeights;
end;

end.

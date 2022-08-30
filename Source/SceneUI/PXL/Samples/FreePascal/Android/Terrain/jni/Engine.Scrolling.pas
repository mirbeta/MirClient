unit Engine.Scrolling;
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
  PXL.TypeDef, PXL.Types;

type
  TDragScroll = class
  private const
    MaxAveragePositions = 5;
    MaxScrollingSpeed = 32.0;
    MaxScrollingThrust = 1.0;
  private
    FDeviceScale: VectorFloat;

    FPosition: TPoint2f;
    FVelocity: TPoint2f;

    FReference: TPoint2i;
    FDragging: Boolean;

    FPositions: array[0..MaxAveragePositions - 1] of TPoint2i;
    FLastPosition: TPoint2i;
    FLastAveragePosition: TPoint2i;
    FLastAverageVelocity: TPoint2i;

    FViewPos: TPoint2i;

    function GetAveragePosition: TPoint2i;

    procedure StartDragging;
    procedure ContinueDragging;
  public
    constructor Create;

    procedure TouchDown(const TouchPos: TPoint2i);
    procedure TouchMove(const TouchPos: TPoint2i);
    procedure TouchUp(const TouchPos: TPoint2i);

    procedure Update;
    procedure SetPosition(const NewPosition: TPoint2f);

    property DeviceScale: VectorFloat read FDeviceScale write FDeviceScale;
    property Dragging: Boolean read FDragging;
    property ViewPos: TPoint2i read FViewPos;
  end;

implementation

uses
  Math;

constructor TDragScroll.Create;
begin
  inherited;

  FDeviceScale := 1.0;

  FPosition := ZeroPoint2f;
  FVelocity := ZeroPoint2f;

  FDragging := False;

  FLastPosition := Undefined2i;
  FPositions[0] := Undefined2i;

  FLastAverageVelocity := ZeroPoint2i;
end;

procedure TDragScroll.Update;
var
  CurrentAveragePosition: TPoint2i;
  Amp, InitAmp, Thrust: Single;
  I: Integer;
begin
  for I := MaxAveragePositions - 1 downto 1 do
    FPositions[I] := FPositions[I + 1];

  FPositions[0] := FLastPosition;

  CurrentAveragePosition := GetAveragePosition;
  FLastAverageVelocity := FLastAveragePosition - CurrentAveragePosition;
  FLastAveragePosition := CurrentAveragePosition;

  if not FDragging then
  begin
    Amp := FVelocity.Length;

    if Amp > 0.0 then
    begin
      Thrust := MaxScrollingThrust * FDeviceScale;

      InitAmp := Amp;
      Amp := Max(Amp - Thrust, 0.0);

      FVelocity.X := (FVelocity.X / InitAmp) * Amp;
      FVelocity.Y := (FVelocity.Y / InitAmp) * Amp;
    end;

    if Amp > VectorEpsilon then
      FPosition := FPosition + FVelocity;
  end;

  FViewPos := FPosition.ToInt;
end;

function TDragScroll.GetAveragePosition: TPoint2i;
var
  I, HistoryCount: Integer;
  Weight, WeightSum, AccX, AccY: Single;
begin
  HistoryCount := 0;
  WeightSum := 0.0;
  AccX := 0.0;
  AccY := 0.0;
  Weight := 1.0;

  for I := 0 to MaxAveragePositions - 1 do
  begin
    if FPositions[I] = Undefined2i then
      Break;

    AccX := AccX + FPositions[I].X * Weight;
    AccY := AccY + FPositions[I].Y * Weight;
    WeightSum := WeightSum + Weight;
    Weight := Weight * 0.5;
    Inc(HistoryCount);
  end;

  if HistoryCount < 1 then
    Exit(Undefined2i);

  Result.X := Round(AccX / WeightSum);
  Result.Y := Round(AccY / WeightSum);
end;

procedure TDragScroll.StartDragging;
begin
  FReference := FLastPosition;
  FVelocity := ZeroPoint2f;
end;

procedure TDragScroll.ContinueDragging;
var
  Shift, CurrentPos: TPoint2i;
begin
  CurrentPos := GetAveragePosition;
  if CurrentPos = Undefined2i then
    Exit;

  Shift := CurrentPos - FReference;
  FPosition := FPosition - Shift;

  FReference := CurrentPos;
end;

procedure TDragScroll.TouchDown(const TouchPos: TPoint2i);
begin
  FLastPosition := TouchPos;
  StartDragging;
  FDragging := True;
end;

procedure TDragScroll.TouchMove(const TouchPos: TPoint2i);
begin
  FLastPosition := TouchPos;

  if FDragging then
    ContinueDragging;
end;

procedure TDragScroll.TouchUp(const TouchPos: TPoint2i);
var
  MaxSpeed: Single;
begin
  FLastPosition := Undefined2i;

  if FDragging then
  begin
    FDragging := False;

    MaxSpeed := MaxScrollingSpeed * FDeviceScale;

    FVelocity := FLastAverageVelocity;

    if FVelocity.Length > MaxSpeed then
      FVelocity := FVelocity.Normalize * MaxSpeed;
  end;
end;

procedure TDragScroll.SetPosition(const NewPosition: TPoint2f);
begin
  FPosition := NewPosition;
  FVelocity := ZeroPoint2f;
end;

end.

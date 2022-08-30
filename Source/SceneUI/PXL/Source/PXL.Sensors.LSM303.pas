unit PXL.Sensors.LSM303;
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
  PXL.Types, PXL.Boards.Types, PXL.Sensors.Types;

type
  TSensorLSM303 = class(TCustomSensor)
  public const
    DefaultAccelerometerAddress = $19;
    DefaultMagnetometerAddress = $1E;
  private
    FDataPort: TCustomPortI2C;
    FAccelerometerAddress: Integer;
    FMagnetometerAddress: Integer;

    function GetAccelerometer: TVector3f;
    function GetMagnetometer: TVector3f;
    function GetThermometer: Single;
  public
    constructor Create(const ADataPort: TCustomPortI2C;
      const AAccelerometerAddress: Integer = DefaultAccelerometerAddress;
      const AMagnetometerAddress: Integer = DefaultMagnetometerAddress);

    { Returns raw values of accelerometer registers. }
    function GetAccelerometerRaw: TVector3i;

    { Returns raw values of magnetometer registers. }
    function GetMagnetometerRaw: TVector3i;

    { Returns raw values of thermometer registers. If there is a communication error, -1 is returned. On occasions
      there is an issue with thermometer on this chip, where it would fail each time when reading. This seems to be
      determined on startup, so powering down and then powering up sensor breakout may fix the problem. Therefore,
      if -1 is returned by this method, likely it will keep this way until next power cycle of sensor breakout. }
    function GetThermometerRaw: Integer;

    { Retrieves "compact" values of accelerometer, with spherical coordinates indicating direction (Latitude and
      Longitude have range of [0..255]) and magnitude multiplied by 8192. Returns True if successful and False if there
      were communication errors. }
    function GetAccelerometerCompact(out Latitude, Longitude: Byte; out Magnitude: Word): Boolean;

    { Retrieves "compact" values of magnetometer, with spherical coordinates indicating direction (Latitude and
      Longitude have range of [0..255]) and magnitude multiplied by 8192. Returns True if successful and False if there
      were communication errors. }
    function GetMagnetometerCompact(out Latitude, Longitude: Byte; out Magnitude: Word): Boolean;

    { Converts "compact" values obtained by either @link(GetAccelerometerCompact) or @link(GetMagnetometerCompact) into
      the actual 3D vector. }
    class function CompactToVector(const Latitude, Longitude: Byte; const Magnitude: Word): TVector3f;

    property DataPort: TCustomPortI2C read FDataPort;
    property AccelerometerAddress: Integer read FAccelerometerAddress;
    property MagnetometerAddress: Integer read FMagnetometerAddress;

    { Current value of accelerometer in "g" units. }
    property Accelerometer: TVector3f read GetAccelerometer;

    { Current value of magnetometer in "Gauss" units.}
    property Magnetometer: TVector3f read GetMagnetometer;

    { Current value of thermometer in "Celsius" units. Note that this value is not calibrated and can only be used to
      calculate changes in temperature. Also, on occasions (this is determined at startup) it may not work at all, in
      which case a value of zero will be returned. }
    property Thermometer: Single read GetThermometer;
  end;

implementation

uses
  SysUtils, Math;

const
  PiHalf = Pi * 0.5;
  PiTo256 = 256.0 / Pi;
  TwoPiTo256 = 256.0 / (2.0 * Pi);

constructor TSensorLSM303.Create(const ADataPort: TCustomPortI2C; const AAccelerometerAddress,
  AMagnetometerAddress: Integer);
begin
  inherited Create;

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise ESensorNoDataPort.Create(SSensorNoDataPort);

  FAccelerometerAddress := AAccelerometerAddress;
  if (FAccelerometerAddress < 0) or (FAccelerometerAddress > $7F) then
    raise ESensorInvalidAddress.Create(Format(SSensorInvalidAddress, [FAccelerometerAddress]));

  FMagnetometerAddress := AMagnetometerAddress;
  if (FMagnetometerAddress < 0) or (FAccelerometerAddress > $7F) then
    raise ESensorInvalidAddress.Create(Format(SSensorInvalidAddress, [FMagnetometerAddress]));

  FDataPort.SetAddress(FAccelerometerAddress);
  if not FDataPort.WriteByteData($20, $27) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  FDataPort.SetAddress(FMagnetometerAddress);
  if not FDataPort.WriteByteData($00, $90) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));
  if not FDataPort.WriteByteData($02, $00) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));
end;

function TSensorLSM303.GetAccelerometerRaw: TVector3i;
var
  Values: array[0..5] of Byte;
begin
  FDataPort.SetAddress(FAccelerometerAddress);

  if not FDataPort.WriteByte($28 or $80) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [1]));

  if FDataPort.Read(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(Values)]));

  Result.X := SmallInt(Word(Values[0]) or (Word(Values[1]) shl 8)) div 16;
  Result.Y := SmallInt(Word(Values[2]) or (Word(Values[3]) shl 8)) div 16;
  Result.Z := SmallInt(Word(Values[4]) or (Word(Values[5]) shl 8)) div 16;
end;

function TSensorLSM303.GetMagnetometerRaw: TVector3i;
var
  Values: array[0..5] of Byte;
begin
  FDataPort.SetAddress(FMagnetometerAddress);

  if not FDataPort.WriteByte($03) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [1]));

  if FDataPort.Read(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(Values)]));

  Result.X := SmallInt(Word(Values[1]) or (Word(Values[0]) shl 8));
  Result.Z := SmallInt(Word(Values[3]) or (Word(Values[2]) shl 8));
  Result.Y := SmallInt(Word(Values[5]) or (Word(Values[4]) shl 8));
end;

function TSensorLSM303.GetThermometerRaw: Integer;
var
  TempValue: Word;
begin
  FDataPort.SetAddress(FMagnetometerAddress);

  if not FDataPort.ReadWordData($31, TempValue) then
    Exit(-1);

  Result := (TempValue shr 8) or ((TempValue and $FF) shl 8);
end;

function TSensorLSM303.GetAccelerometer: TVector3f;
begin
  Result := TVector3f(GetAccelerometerRaw) * 0.001;
end;

function TSensorLSM303.GetMagnetometer: TVector3f;
const
  NormalizeCoefXY = 1.0 / 950.0;
  NormalizeCoefZ = 1.0 / 1055.0;
  NormalizeCoefs: TVector3f = (X: NormalizeCoefXY; Y: NormalizeCoefXY; Z: NormalizeCoefZ);
begin
  Result := TVector3f(GetMagnetometerRaw) * NormalizeCoefs;
end;

function TSensorLSM303.GetThermometer: Single;
var
  RawValue: Integer;
begin
  RawValue := GetThermometerRaw;
  if RawValue > 0 then
    Result := RawValue / 8.0
  else
    Result := 0.0;
end;

function TSensorLSM303.GetAccelerometerCompact(out Latitude, Longitude: Byte; out Magnitude: Word): Boolean;
const
  MagToWord = 8192.0 * 0.001;
var
  ValueRaw: TVector3i;
  ValueNorm: TVector3f;
begin
  try
    ValueRaw := GetAccelerometerRaw;
  except
    Exit(False);
  end;

  ValueNorm := Norm3(ValueRaw);

  Latitude := Round((ArcSin(ValueNorm.Z) + PiHalf) * PiTo256) and $FF;
  Longitude := Round((ArcTan2(ValueNorm.Y, ValueNorm.X) + Pi) * TwoPiTo256) and $FF;
  Magnitude := Min(Round(Length3px(ValueRaw) * MagToWord), 65535);

  Result := True;
end;

function TSensorLSM303.GetMagnetometerCompact(out Latitude, Longitude: Byte; out Magnitude: Word): Boolean;
var
  ValueRaw, ValueNorm: TVector3f;
begin
  try
    ValueRaw := GetMagnetometer;
  except
    Exit(False);
  end;

  ValueNorm := Norm3(ValueRaw);

  Latitude := Round((ArcSin(ValueNorm.Z) + PiHalf) * PiTo256) and $FF;
  Longitude := Round((ArcTan2(ValueNorm.Y, ValueNorm.X) + Pi) * TwoPiTo256) and $FF;
  Magnitude := Min(Round(Length3(ValueRaw) * 8192.0), 65535);

  Result := True;
end;

class function TSensorLSM303.CompactToVector(const Latitude, Longitude: Byte; const Magnitude: Word): TVector3f;
var
  LatF, LongF, MagF: Single;
begin
  LatF := ((Latitude * Pi) / 256.0) - PiHalf;
  LongF := ((Longitude * 2.0 * Pi) / 256.0) - Pi;
  MagF := Magnitude / 8192.0;

  Result.X := MagF * Cos(LatF) * Cos(LongF);
  Result.Y := MagF * Cos(LatF) * Sin(LongF);
  Result.Z := MagF * Sin(LatF);
end;

end.

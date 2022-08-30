unit PXL.Sensors.L3GD20;
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
  TSensorL3GD20 = class(TCustomSensor)
  public type
    TSensitivity = (Scale245, Scale500, Scale2000);
  public const
    DefaultAddress = $6B;
  private
    FDataPort: TCustomPortI2C;
    FAddress: Integer;
    FSensitivity: TSensitivity;
    FGyroCoefficient: Single;

    procedure TestChipID;
    procedure Configure;

    function GetGyroscope: TVector3f;
  public
    constructor Create(const ADataPort: TCustomPortI2C;  const AAddress: Integer = DefaultAddress;
      const ASensitivity: TSensitivity = TSensitivity.Scale245);

    function GetGyroscopeRaw: TVector3i;
    function GetTemperatureRaw: Integer;

    property DataPort: TCustomPortI2C read FDataPort;
    property Address: Integer read FAddress;
    property Sensitivity: TSensitivity read FSensitivity;

    property Gyroscope: TVector3f read GetGyroscope;
    property Temperature: Integer read GetTemperatureRaw;
  end;

implementation

uses
  SysUtils, Math;

constructor TSensorL3GD20.Create(const ADataPort: TCustomPortI2C; const AAddress: Integer;
  const ASensitivity: TSensitivity);
begin
  inherited Create;

  FDataPort := ADataPort;
  if FDataPort = nil then
    raise ESensorNoDataPort.Create(SSensorNoDataPort);

  FAddress := AAddress;
  if (FAddress < 0) or (FAddress > $7F) then
    raise ESensorInvalidAddress.Create(Format(SSensorInvalidAddress, [FAddress]));

  FSensitivity := ASensitivity;

  TestChipID;
  Configure;
end;

procedure TSensorL3GD20.TestChipID;
const
  ExpectedID1 = $D4;
  ExpectedID2 = $D7;
var
  ChipID: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.ReadByteData($0F, ChipID) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(ChipID)]));

  if not (ChipID in [ExpectedID1, ExpectedID2]) then
    raise ESensorInvalidChipID.Create(Format(SSensorInvalidChipID, [ExpectedID2, ChipID]));
end;

procedure TSensorL3GD20.Configure;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByteData($20, $0F) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  if not FDataPort.WriteByteData($23, Ord(FSensitivity) shl 4) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [2]));

  case FSensitivity of
    TSensitivity.Scale500:
      FGyroCoefficient := 1.0 / 500.0;

    TSensitivity.Scale2000:
      FGyroCoefficient := 1.0 / 2000.0;
  else
    FGyroCoefficient := 1.0 / 250.0;
  end;
end;

function TSensorL3GD20.GetGyroscopeRaw: TVector3i;
var
  Values: array[0..5] of Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.WriteByte($28 or $80) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [SizeOf(Byte)]));

  if FDataPort.Read(@Values[0], SizeOf(Values)) <> SizeOf(Values) then
    raise ESensorDataRead.Create(Format(SSensorDataRead, [SizeOf(Values)]));

  Result.X := SmallInt(Word(Values[0]) or (Word(Values[1]) shl 8));
  Result.Y := SmallInt(Word(Values[2]) or (Word(Values[3]) shl 8));
  Result.Z := SmallInt(Word(Values[4]) or (Word(Values[5]) shl 8));
end;

function TSensorL3GD20.GetTemperatureRaw: Integer;
var
  Value: Byte;
begin
  FDataPort.SetAddress(FAddress);

  if not FDataPort.ReadByteData($26, Value) then
    raise ESensorDataWrite.Create(Format(SSensorDataWrite, [SizeOf(Byte)]));

  Result := Value;
end;

function TSensorL3GD20.GetGyroscope: TVector3f;
var
  ValueRaw: TVector3i;
begin
  ValueRaw := GetGyroscopeRaw;

  Result.X := DegToRad(ValueRaw.X * FGyroCoefficient);
  Result.Y := DegToRad(ValueRaw.Y * FGyroCoefficient);
  Result.Z := DegToRad(ValueRaw.Z * FGyroCoefficient);
end;

end.

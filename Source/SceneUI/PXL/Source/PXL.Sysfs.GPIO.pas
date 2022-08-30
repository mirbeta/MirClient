unit PXL.Sysfs.GPIO;
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
  PXL.TypeDef, PXL.Sysfs.Types, PXL.Boards.Types;

type
  { Drive mode that is used in GPIO pins. }
  TPinDriveEx = (
    { Strong low and high. }
    Strong,

    { Resistive high, strong low. }
    PullUp,

    { Resistive low, strong high. }
    PullDown,

    { High Z state }
    HighZ);

  TSysfsGPIO = class(TCustomGPIO)
  public const
    DefaultSystemPath = '/sys/class/gpio';
  protected const
    MaximumSupportedPins = 256;

    ExportedBitmask = $80;
    DirectionDefinedBitmask = $40;
    DirectionBitmask = $20;
    DriveBitmask = $18;
    ValueDefinedBitmask = $02;
    ValueBitmask = $01;
  private
    FSystemPath: StdString;
    FExportFileName: StdString;
    FUnexportFileName: StdString;
    FAccessFileName: StdString;

    FPins: packed array[0..MaximumSupportedPins - 1] of Byte;

    procedure SetPinBit(const Pin: TPinIdentifier; const Mask: Cardinal); inline;
    procedure ClearPinBit(const Pin: TPinIdentifier; const Mask: Cardinal); inline;
    function IsPinBitSet(const Pin: TPinIdentifier; const Mask: Cardinal): Boolean; inline;

    function IsPinExported(const Pin: TPinIdentifier): Boolean; inline;
    function HasPinDirection(const Pin: TPinIdentifier): Boolean; inline;
    function HasPinValue(const Pin: TPinIdentifier): Boolean; inline;

    procedure ExportPin(const Pin: TPinIdentifier);
    procedure UnexportPin(const Pin: TPinIdentifier);
    procedure UnexportAllPins;

    function GetPinDriveEx(const Pin: TPinIdentifier): TPinDriveEx;
    procedure SetPinDriveEx(const Pin: TPinIdentifier; const Value: TPinDriveEx);
  protected
    function GetPinMode(const Pin: TPinIdentifier): TPinMode; override;
    procedure SetPinMode(const Pin: TPinIdentifier; const Mode: TPinMode); override;

    function GetPinValue(const Pin: TPinIdentifier): TPinValue; override;
    procedure SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue); override;

    function GetPinDrive(const Pin: TPinIdentifier): TPinDrive; override;
    procedure SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive); override;
  public
    constructor Create(const ASystemPath: StdString = DefaultSystemPath);
    destructor Destroy; override;

    function TrySetPinMode(const Pin: TPinIdentifier; const Mode: TPinMode): Boolean;

    property PinDrive[const Pin: TPinIdentifier]: TPinDriveEx read GetPinDriveEx write SetPinDriveEx;
  end;

  EGPIOGeneric = class(ESysfsGeneric);
  EGPIOInvalidPin = class(EGPIOGeneric);
  EGPIOUndefinedPin = class(EGPIOGeneric);
  EGPIOIncorrectPinDirection = class(EGPIOGeneric);

resourcestring
  SGPIOSpecifiedPinInvalid = 'The specified GPIO pin <%d> is invalid.';
  SGPIOSpecifiedPinUndefined = 'The specified GPIO pin <%d> is undefined.';
  SGPIOPinHasIncorrectDirection = 'The specified GPIO pin <%d> has incorrect direction.';

implementation

uses
  SysUtils;

constructor TSysfsGPIO.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
  FExportFileName := FSystemPath + '/export';
  FUnexportFileName := FSystemPath + '/unexport';
  FAccessFileName := FSystemPath + '/gpio';
end;

destructor TSysfsGPIO.Destroy;
begin
  UnexportAllPins;

  inherited;
end;

procedure TSysfsGPIO.SetPinBit(const Pin: TPinIdentifier; const Mask: Cardinal);
begin
  FPins[Pin] := FPins[Pin] or Mask;
end;

procedure TSysfsGPIO.ClearPinBit(const Pin: TPinIdentifier; const Mask: Cardinal);
begin
  FPins[Pin] := FPins[Pin] and ($FF xor Mask);
end;

function TSysfsGPIO.IsPinBitSet(const Pin: TPinIdentifier; const Mask: Cardinal): Boolean;
begin
  Result := FPins[Pin] and Mask > 0;
end;

function TSysfsGPIO.IsPinExported(const Pin: TPinIdentifier): Boolean;
begin
  Result := IsPinBitSet(Pin, ExportedBitmask);
end;

function TSysfsGPIO.HasPinDirection(const Pin: TPinIdentifier): Boolean;
begin
  Result := IsPinBitSet(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.HasPinValue(const Pin: TPinIdentifier): Boolean;
begin
  Result := IsPinBitSet(Pin, ValueDefinedBitmask);
end;

procedure TSysfsGPIO.ExportPin(const Pin: TPinIdentifier);
begin
  TryWriteTextToFile(FExportFileName, IntToStr(Pin));
  SetPinBit(Pin, ExportedBitmask);
end;

procedure TSysfsGPIO.UnexportPin(const Pin: TPinIdentifier);
begin
  TryWriteTextToFile(FUnexportFileName, IntToStr(Pin));
  ClearPinBit(Pin, ExportedBitmask);
end;

procedure TSysfsGPIO.UnexportAllPins;
var
  I: Integer;
begin
  for I := Low(FPins) to High(FPins) do
    if IsPinExported(I) then
      UnexportPin(I);
end;

function TSysfsGPIO.GetPinMode(const Pin: TPinIdentifier): TPinMode;
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) then
    Result := TPinMode.Output
  else
    Result := TPinMode.Input;
end;

procedure TSysfsGPIO.SetPinMode(const Pin: TPinIdentifier; const Mode: TPinMode);
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if not IsPinExported(Pin) then
    ExportPin(Pin);

  if Mode = TPinMode.Input then
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'in');
    ClearPinBit(Pin, DirectionBitmask);
  end
  else
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'out');
    SetPinBit(Pin, DirectionBitmask);
  end;

  SetPinBit(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.TrySetPinMode(const Pin: TPinIdentifier; const Mode: TPinMode): Boolean;
begin
  if Pin > MaximumSupportedPins then
    Exit(False);

  if not IsPinExported(Pin) then
    ExportPin(Pin);

  if Mode = TPinMode.Input then
  begin
    Result := TryWriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'in');
    ClearPinBit(Pin, DirectionBitmask);
  end
  else
  begin
    Result := TryWriteTextToFile(FAccessFileName + IntToStr(Pin) + '/direction', 'out');
    SetPinBit(Pin, DirectionBitmask);
  end;

  SetPinBit(Pin, DirectionDefinedBitmask);
end;

function TSysfsGPIO.GetPinValue(const Pin: TPinIdentifier): TPinValue;
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) and HasPinValue(Pin) then
  begin // Pin with direction set to OUTPUT and VALUE defined can be retrieved directly.
    if IsPinBitSet(Pin, ValueBitmask) then
      Result := TPinValue.High
    else
      Result := TPinValue.Low;
  end
  else
  begin // Pin needs to be read from GPIO.
    if ReadCharFromFile(FAccessFileName + IntToStr(Pin) + '/value') = '1' then
      Result := TPinValue.High
    else
      Result := TPinValue.Low;
  end;
end;

procedure TSysfsGPIO.SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue);
var
  CurValue: TPinValue;
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if not IsPinBitSet(Pin, DirectionBitmask) then
    raise EGPIOIncorrectPinDirection.Create(Format(SGPIOPinHasIncorrectDirection, [Pin]));

  if HasPinValue(Pin) then
  begin
    if IsPinBitSet(Pin, ValueBitmask) then
      CurValue := TPinValue.High
    else
      CurValue := TPinValue.Low;

    // Do not write value to the pin if it is already set.
    if CurValue = Value then
      Exit;
  end;

  if Value = TPinValue.Low then
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/value', '0');
    ClearPinBit(Pin, ValueBitmask);
  end
  else
  begin
    WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/value', '1');
    SetPinBit(Pin, ValueBitmask);
  end;
end;

function TSysfsGPIO.GetPinDriveEx(const Pin: TPinIdentifier): TPinDriveEx;
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  Result := TPinDriveEx((FPins[Pin] and DriveBitmask) shr 3);
end;

procedure TSysfsGPIO.SetPinDriveEx(const Pin: TPinIdentifier; const Value: TPinDriveEx);
var
  DriveText: StdString;
begin
  if Pin > MaximumSupportedPins then
    raise EGPIOInvalidPin.Create(Format(SGPIOSpecifiedPinInvalid, [Pin]));

  if (not IsPinExported(Pin)) or (not HasPinDirection(Pin)) then
    raise EGPIOUndefinedPin.Create(Format(SGPIOSpecifiedPinUndefined, [Pin]));

  if IsPinBitSet(Pin, DirectionBitmask) then
    raise EGPIOIncorrectPinDirection.Create(Format(SGPIOPinHasIncorrectDirection, [Pin]));

  case Value of
    TPinDriveEx.PullUp:
      DriveText := 'pullup';

    TPinDriveEx.PullDown:
      DriveText := 'pulldown';

    TPinDriveEx.HighZ:
      DriveText := 'hiz';
  else
    DriveText := 'strong';
  end;

  WriteTextToFile(FAccessFileName + IntToStr(Pin) + '/drive', DriveText);

  ClearPinBit(Pin, DriveBitmask);
  SetPinBit(Pin, (Ord(Value) and $03) shl 3);
end;

function TSysfsGPIO.GetPinDrive(const Pin: TPinIdentifier): TPinDrive;
begin
  Result := TPinDrive(GetPinDriveEx(Pin));
end;

procedure TSysfsGPIO.SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive);
begin
  SetPinDriveEx(Pin, TPinDriveEx(Value));
end;

end.


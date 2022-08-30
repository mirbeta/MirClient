unit PXL.Sysfs.PWM;
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
  TSysfsPWM = class(TCustomPWM)
  private const
    MaximumSupportedChannels = 64;

    ExportedBitmask = $80;
    EnabledDefinedBitmask = $40;
    EnabledBitmask = $20;
    PeriodDefinedBitmask = $10;
    DutyCycleDefinedBitmask = $08;
  private
    FSystemPath: StdString;
    FExportFileName: StdString;
    FUnexportFileName: StdString;
    FAccessFileName: StdString;

    FChannels: array[0..MaximumSupportedChannels - 1] of Byte;
    FPeriods: array[0..MaximumSupportedChannels - 1] of Integer;
    FDutyCycles: array[0..MaximumSupportedChannels - 1] of Integer;

    procedure SetChannelBit(const Channel: TPinChannel; const Mask: Cardinal); inline;
    procedure ClearChannelBit(const Channel: TPinChannel; const Mask: Cardinal); inline;
    function IsChannelBitSet(const Channel: TPinChannel; const Mask: Cardinal): Boolean; inline;
    function IsChannelExported(const Channel: TPinChannel): Boolean; inline;
    function HasChannelAbility(const Channel: TPinChannel): Boolean; inline;

    procedure ExportChannel(const Channel: TPinChannel);
    procedure UnexportChannel(const Channel: TPinChannel);
    procedure UnexportAllChannels;
  protected
    function GetEnabled(const Channel: TPinChannel): Boolean; override;
    procedure SetEnabled(const Channel: TPinChannel; const Value: Boolean); override;
    function GetPeriod(const Channel: TPinChannel): Cardinal; override;
    procedure SetPeriod(const Channel: TPinChannel; const Value: Cardinal); override;
    function GetDutyCycle(const Channel: TPinChannel): Cardinal; override;
    procedure SetDutyCycle(const Channel: TPinChannel; const Value: Cardinal); override;
  public
    constructor Create(const ASystemPath: StdString);
    destructor Destroy; override;
  end;

  EPWMGeneric = class(ESysfsGeneric);
  EPWMInvalidChannel = class(EPWMGeneric);
  EPWMUndefinedChannel = class(EPWMGeneric);

resourcestring
  SPWMSpecifiedChannelInvalid = 'The specified PWM channel <%d> is invalid.';
  SPWMSpecifiedChannelUndefined = 'The specified PWM channel <%d> is undefined.';

implementation

uses
  SysUtils;

constructor TSysfsPWM.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
  FExportFileName := FSystemPath + '/export';
  FUnexportFileName := FSystemPath + '/unexport';
  FAccessFileName := FSystemPath + '/pwm';
end;

destructor TSysfsPWM.Destroy;
begin
  UnexportAllChannels;

  inherited;
end;

procedure TSysfsPWM.SetChannelBit(const Channel: TPinChannel; const Mask: Cardinal);
begin
  FChannels[Channel] := FChannels[Channel] or Mask;
end;

procedure TSysfsPWM.ClearChannelBit(const Channel: TPinChannel; const Mask: Cardinal);
begin
  FChannels[Channel] := FChannels[Channel] and ($FF xor Mask);
end;

function TSysfsPWM.IsChannelBitSet(const Channel: TPinChannel; const Mask: Cardinal): Boolean;
begin
  Result := FChannels[Channel] and Mask > 0;
end;

function TSysfsPWM.IsChannelExported(const Channel: TPinChannel): Boolean;
begin
  Result := IsChannelBitSet(Channel, ExportedBitmask);
end;

function TSysfsPWM.HasChannelAbility(const Channel: TPinChannel): Boolean;
begin
  Result := IsChannelBitSet(Channel, EnabledDefinedBitmask);
end;

procedure TSysfsPWM.ExportChannel(const Channel: TPinChannel);
begin
  TryWriteTextToFile(FExportFileName, IntToStr(Channel));
  SetChannelBit(Channel, ExportedBitmask);
end;

procedure TSysfsPWM.UnexportChannel(const Channel: TPinChannel);
begin
  TryWriteTextToFile(FUnexportFileName, IntToStr(Channel));
  ClearChannelBit(Channel, ExportedBitmask);
end;

procedure TSysfsPWM.UnexportAllChannels;
var
  I: Integer;
begin
  for I := Low(FChannels) to High(FChannels) do
    if IsChannelExported(I) then
      UnexportChannel(I);
end;

function TSysfsPWM.GetEnabled(const Channel: TPinChannel): Boolean;
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := IsChannelBitSet(Channel, EnabledBitmask);
end;

procedure TSysfsPWM.SetEnabled(const Channel: TPinChannel; const Value: Boolean);
var
  NeedModify: Boolean;
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if not IsChannelExported(Channel) then
    ExportChannel(Channel);

  if IsChannelBitSet(Channel, EnabledDefinedBitmask) then
    if IsChannelBitSet(Channel, EnabledBitmask) then
      NeedModify := not Value
    else
      NeedModify := Value
  else
    NeedModify := True;

  if NeedModify then
  begin
    if Value then
    begin
      WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/enable', '1');
      SetChannelBit(Channel, EnabledBitmask);
    end
    else
    begin
      WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/enable', '0');
      ClearChannelBit(Channel, EnabledBitmask);
    end;

    SetChannelBit(Channel, EnabledDefinedBitmask);
  end;
end;

function TSysfsPWM.GetPeriod(const Channel: TPinChannel): Cardinal;
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) or
    (not IsChannelBitSet(Channel, PeriodDefinedBitmask)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := FPeriods[Channel];
end;

procedure TSysfsPWM.SetPeriod(const Channel: TPinChannel; const Value: Cardinal);
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/period', IntToStr(Value));
  FPeriods[Channel] := Value;

  SetChannelBit(Channel, PeriodDefinedBitmask);
end;

function TSysfsPWM.GetDutyCycle(const Channel: TPinChannel): Cardinal;
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) or
    (not IsChannelBitSet(Channel, DutyCycleDefinedBitmask)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  Result := FDutyCycles[Channel];
end;

procedure TSysfsPWM.SetDutyCycle(const Channel: TPinChannel; const Value: Cardinal);
begin
  if Channel > MaximumSupportedChannels then
    raise EPWMInvalidChannel.Create(Format(SPWMSpecifiedChannelInvalid, [Channel]));

  if (not IsChannelExported(Channel)) or (not HasChannelAbility(Channel)) then
    raise EPWMUndefinedChannel.Create(Format(SPWMSpecifiedChannelUndefined, [Channel]));

  WriteTextToFile(FAccessFileName + IntToStr(Channel) + '/duty_cycle', IntToStr(Value));
  FDutyCycles[Channel] := Value;

  SetChannelBit(Channel, DutyCycleDefinedBitmask);
end;

end.


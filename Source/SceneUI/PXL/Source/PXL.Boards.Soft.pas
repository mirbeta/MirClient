unit PXL.Boards.Soft;
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

{$INCLUDE PXL.MicroConfig.inc}

uses
  PXL.Boards.Types;

type
  TSoftSPI = class(TCustomPortSPI)
  private
    FSystemCore: TCustomSystemCore;
    FGPIO: TCustomGPIO;

    FPinSCLK: TPinIdentifier;
    FPinMOSI: TPinIdentifier;
    FPinMISO: TPinIdentifier;
    FPinCS: TPinIdentifier;

    FFrequency: Cardinal;
    FMode: TSPIMode;
    FDelayInterval: Cardinal;

    function GetInitialValueSCLK: Cardinal;
    function GetInitialValueCS: Cardinal;
    procedure SetChipSelect(const ValueCS: Cardinal);
    procedure FlipClock(var ValueSCLK: Cardinal);
  protected
    function GetFrequency: Cardinal; override;
    procedure SetFrequency(const Value: Cardinal); override;
    function GetBitsPerWord: TBitsPerWord; override;
    procedure SetBitsPerWord(const Value: TBitsPerWord); override;
    function GetMode: TSPIMode; override;
    procedure SetMode(const Value: TSPIMode); override;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinSCLK, APinMOSI,
      APinMISO, APinCS: TPinIdentifier; const AChipSelectMode: TChipSelectMode = TChipSelectMode.ActiveLow);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    property SystemCore: TCustomSystemCore read FSystemCore;
    property GPIO: TCustomGPIO read FGPIO;

    property PinSCLK: TPinIdentifier read FPinSCLK;
    property PinMOSI: TPinIdentifier read FPinMOSI;
    property PinMISO: TPinIdentifier read FPinMISO;
    property PinCS: TPinIdentifier read FPinCS;
  end;

  TSoftUART = class(TCustomPortUART)
  private const
    DefaultReadTimeout = 100; // ms
  private
    FGPIO: TCustomGPIO;

    FPinTX: TPinIdentifier;
    FPinRX: TPinIdentifier;

    FBaudRate: Cardinal;

    function CalculatePeriod(const ElapsedTime: TTickCounter): Cardinal;
    procedure WaitForPeriod(const StartTime: TTickCounter; const Period: Cardinal);
  protected
    function GetBaudRate: Cardinal; override;
    procedure SetBaudRate(const Value: Cardinal); override;
    function GetBitsPerWord: TBitsPerWord; override;
    procedure SetBitsPerWord(const Value: TBitsPerWord); override;
    function GetParity: TParity; override;
    procedure SetParity(const Value: TParity); override;
    function GetStopBits: TStopBits; override;
    procedure SetStopBits(const Value: TStopBits); override;
  public
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO;
      const APinTX, APinRX: TPinIdentifier);
    destructor Destroy; override;

    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    function ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal; override;
    function WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal; override;

    procedure Flush; override;

    property PinTX: TPinIdentifier read FPinTX;
    property PinRX: TPinIdentifier read FPinRX;
  end;

implementation

{$REGION 'TSoftSPI'}

constructor TSoftSPI.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinSCLK,
  APinMOSI, APinMISO, APinCS: TPinIdentifier; const AChipSelectMode: TChipSelectMode);
begin
  inherited Create(AChipSelectMode);

  FSystemCore := ASystemCore;
  FGPIO := AGPIO;

  FPinSCLK := APinSCLK;
  FPinMOSI := APinMOSI;
  FPinMISO := APinMISO;
  FPinCS := APinCS;

  FGPIO.PinMode[FPinSCLK] := TPinMode.Output;

  if FPinMOSI <> PinDisabled then
    FGPIO.PinMode[FPinMOSI] := TPinMode.Output;

  if FPinMISO <> PinDisabled then
    FGPIO.PinMode[FPinMISO] := TPinMode.Output;

  if FPinCS <> PinDisabled then
  begin
    FGPIO.PinMode[FPinCS] := TPinMode.Output;
    FGPIO.PinValue[FPinCS] := TPinValue.High;
  end;
end;

destructor TSoftSPI.Destroy;
begin
  if FPinCS <> PinDisabled then
    FGPIO.PinMode[FPinCS] := TPinMode.Input;

  if FPinMISO <> PinDisabled then
    FGPIO.PinMode[FPinMISO] := TPinMode.Input;

  if FPinMOSI <> PinDisabled then
    FGPIO.PinMode[FPinMOSI] := TPinMode.Input;

  FGPIO.PinMode[FPinSCLK] := TPinMode.Input;

  inherited;
end;

function TSoftSPI.GetFrequency: Cardinal;
begin
  Result := FFrequency;
end;

procedure TSoftSPI.SetFrequency(const Value: Cardinal);
begin
  if (Value <> 0) and (Value <= 1000000) and (FSystemCore <> nil) then
  begin
    FFrequency := Value;
    FDelayInterval := 1000000 div FFrequency;
  end
  else
  begin
    FFrequency := 0;
    FDelayInterval := 0;
  end;
end;

function TSoftSPI.GetBitsPerWord: TBitsPerWord;
begin
  Result := 8;
end;

procedure TSoftSPI.SetBitsPerWord(const Value: TBitsPerWord);
begin
end;

function TSoftSPI.GetMode: TSPIMode;
begin
  Result := FMode;
end;

procedure TSoftSPI.SetMode(const Value: TSPIMode);
begin
  FMode := Value;
end;

function TSoftSPI.GetInitialValueSCLK: Cardinal;
begin
  if FMode and 2 <> 0 then
    Result := 0
  else
    Result := 1;
end;

function TSoftSPI.GetInitialValueCS: Cardinal;
begin
  if FChipSelectMode = TChipSelectMode.ActiveHigh then
    Result := 1
  else
    Result := 0;
end;

procedure TSoftSPI.SetChipSelect(const ValueCS: Cardinal);
begin
  if (FPinCS <> PinDisabled) and (FChipSelectMode <> TChipSelectMode.Disabled) then
    FGPIO.PinValue[FPinCS] := TPinValue(ValueCS);
end;

procedure TSoftSPI.FlipClock(var ValueSCLK: Cardinal);
begin
  ValueSCLK := ValueSCLK xor 1;
  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
end;

function TSoftSPI.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  ValueSCLK, ValueCS, ReadValue: Cardinal;
  CycleStartTime: TTickCounter;
  I, BitIndex: Integer;
begin
  if (Buffer = nil) or (BufferSize = 0) then
    Exit(0);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    for I := 0 to BufferSize - 1 do
    begin
      ReadValue := 0;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if FGPIO.PinValue[FPinMISO] = TPinValue.High then
          ReadValue := ReadValue or 1;

        ReadValue := ReadValue shl 1;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;

      PByte(PtrUInt(Buffer) + Cardinal(I))^ := ReadValue;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

function TSoftSPI.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  ValueSCLK, ValueCS, WriteValue: Cardinal;
  CycleStartTime: TTickCounter;
  I, BitIndex: Integer;
begin
  if (Buffer = nil) or (BufferSize = 0) then
    Exit(0);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    for I := 0 to BufferSize - 1 do
    begin
      WriteValue := PByte(PtrUInt(Buffer) + Cardinal(I))^;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if WriteValue and $80 > 0 then
          FGPIO.PinValue[FPinMOSI] := TPinValue.High
        else
          FGPIO.PinValue[FPinMOSI] := TPinValue.Low;

        WriteValue := WriteValue shl 1;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

function TSoftSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  ValueSCLK, ValueCS, WriteValue, ReadValue: Cardinal;
  CycleStartTime: TTickCounter;
  I, BitIndex: Integer;
begin
  if ((ReadBuffer = nil) and (WriteBuffer = nil)) or (BufferSize = 0) then
    Exit(0);

  ValueSCLK := GetInitialValueSCLK;
  ValueCS := GetInitialValueCS;

  FGPIO.PinValue[FPinSCLK] := TPinValue(ValueSCLK);
  SetChipSelect(ValueCS);
  try
    WriteValue := 0;

    for I := 0 to BufferSize - 1 do
    begin
      ReadValue := 0;

      if WriteBuffer <> nil then
        WriteValue := PByte(PtrUInt(WriteBuffer) + Cardinal(I))^;

      for BitIndex := 0 to 7 do
      begin
        if FDelayInterval <> 0 then
          CycleStartTime := FSystemCore.GetTickCount;

        if FPinMOSI <> PinDisabled then
        begin
          if WriteValue and $80 > 0 then
            FGPIO.PinValue[FPinMOSI] := TPinValue.High
          else
            FGPIO.PinValue[FPinMOSI] := TPinValue.Low;

          WriteValue := WriteValue shl 1;
        end;

        if FPinMISO <> PinDisabled then
        begin
          if FGPIO.PinValue[FPinMISO] = TPinValue.High then
            ReadValue := ReadValue or 1;

          ReadValue := ReadValue shl 1;
        end;

        FlipClock(ValueSCLK);

        if FDelayInterval <> 0 then
          while FSystemCore.TicksInBetween(CycleStartTime, FSystemCore.GetTickCount) < FDelayInterval do ;

        FlipClock(ValueSCLK);
      end;

      if ReadBuffer <> nil then
        PByte(PtrUInt(ReadBuffer) + Cardinal(I))^ := ReadValue;
    end;
  finally
    SetChipSelect(ValueCS xor 1);
  end;

  Result := BufferSize;
end;

{$ENDREGION}
{$REGION 'TSoftUART'}

constructor TSoftUART.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO; const APinTX,
  APinRX: TPinIdentifier);
begin
  inherited Create(ASystemCore);

  FGPIO := AGPIO;

  FPinTX := APinTX;
  FPinRX := APinRX;

  if FPinTX <> PinDisabled then
  begin
    FGPIO.PinMode[FPinTX] := TPinMode.Output;
    FGPIO.PinValue[FPinTX] := TPinValue.High;
  end;

  if FPinRX <> PinDisabled then
    FGPIO.PinMode[FPinRX] := TPinMode.Input;
end;

destructor TSoftUART.Destroy;
begin
  if FPinTX <> PinDisabled then
    FGPIO.PinMode[FPinTX] := TPinMode.Input;

  inherited;
end;

function TSoftUART.GetBaudRate: Cardinal;
begin
  Result := FBaudRate;
end;

procedure TSoftUART.SetBaudRate(const Value: Cardinal);
begin
  FBaudRate := Value;
end;

function TSoftUART.GetBitsPerWord: TBitsPerWord;
begin
  Result := 8;
end;

procedure TSoftUART.SetBitsPerWord(const Value: TBitsPerWord);
begin
end;

function TSoftUART.GetParity: TParity;
begin
  Result := TParity.None;
end;

procedure TSoftUART.SetParity(const Value: TParity);
begin
end;

function TSoftUART.GetStopBits: TStopBits;
begin
  Result := TStopBits.One;
end;

procedure TSoftUART.SetStopBits(const Value: TStopBits);
begin
end;

function TSoftUART.CalculatePeriod(const ElapsedTime: TTickCounter): Cardinal;
begin
  Result := (UInt64(FBaudRate) * ElapsedTime) div 1000000;
end;

procedure TSoftUART.WaitForPeriod(const StartTime: TTickCounter; const Period: Cardinal);
var
  Current: Cardinal;
begin
  repeat
    Current := CalculatePeriod(FSystemCore.TicksInBetween(StartTime, FSystemCore.GetTickCount));
  until Current >= Period;
end;

function TSoftUART.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  StartTime: TTickCounter;
  Period, BitMask, Value: Cardinal;
  I: Integer;
begin
  for I := 0 to BufferSize - 1 do
  begin
    FGPIO.PinValue[FPinTX] := TPinValue.Low;

    // Start sending data a bit earlier to ensure that receiver will sample data correctly.
    FSystemCore.MicroDelay(TTickCounter(1000000 * 3) div (TTickCounter(FBaudRate) * 5));

    Period := 0;
    StartTime := FSystemCore.GetTickCount;

    Value := PByte(PtrUInt(Buffer) + Cardinal(I))^;
    BitMask := 1;

    while BitMask <> 256 do
    begin
      if Value and BitMask > 0 then
        FGPIO.PinValue[FPinTX] := TPinValue.High
      else
        FGPIO.PinValue[FPinTX] := TPinValue.Low;

      BitMask := BitMask shl 1;

      Inc(Period);
      WaitForPeriod(StartTime, Period);
    end;

    FGPIO.PinValue[FPinTX] := TPinValue.High;

    Inc(Period);
    WaitForPeriod(StartTime, Period);
  end;

  Result := BufferSize;
end;

function TSoftUART.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := ReadBuffer(Buffer, BufferSize, DefaultReadTimeout);
end;

function TSoftUART.ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal;
var
  StartTime, TimeoutStart, TimeoutMicroSec: TTickCounter;
  Period, BitMask, Value, BytesReceived: Cardinal;
begin
  BytesReceived := 0;

  if Timeout <> 0 then
    TimeoutMicrosec := TTickCounter(Timeout) * 1000
  else
    TimeoutMicrosec := DefaultReadTimeout * 1000;

  TimeoutStart := FSystemCore.GetTickCount;

  // Wait for RX line to settle on high value.
  repeat
    if FSystemCore.TicksInBetween(TimeoutStart, FSystemCore.GetTickCount) > TimeoutMicrosec then
      Exit(0);
  until FGPIO.PinValue[FPinRX] = TPinValue.High;

  while BytesReceived < BufferSize do
  begin
    // Wait until RX line goes low for the "Start" bit.
    repeat
      if FSystemCore.TicksInBetween(TimeoutStart, FSystemCore.GetTickCount) > TimeoutMicrosec then
        Exit(BytesReceived);
    until FGPIO.PinValue[FPinRX] = TPinValue.Low;

    // Once start bit is received, wait for another 1/3rd of baud to sort of center next samples.
    FSystemCore.MicroDelay(TTickCounter(1000000) div (TTickCounter(FBaudRate) * TTickCounter(4)));

    // Start receiving next byte.
    BitMask := 1;
    Value := 0;

    Period := 0;
    StartTime := FSystemCore.GetTickCount;

    // Skip the remaining of "Start" bit.
    Inc(Period);
    WaitForPeriod(StartTime, Period);

    while BitMask <> 256 do
    begin
      if FGPIO.PinValue[FPinRX] = TPinValue.High then
        Value := Value or BitMask;

      BitMask := BitMask shl 1;

      Inc(Period);
      WaitForPeriod(StartTime, Period);
    end;

    PByte(PtrUInt(Buffer) + Cardinal(BytesReceived))^ := Value;
    Inc(BytesReceived);
  end;

  Result := BytesReceived;
end;

function TSoftUART.WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal;
begin
  Result := Write(Buffer, BufferSize);
end;

procedure TSoftUART.Flush;
begin
end;

{$ENDREGION}

end.


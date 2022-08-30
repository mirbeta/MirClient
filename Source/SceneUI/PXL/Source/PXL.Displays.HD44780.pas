unit PXL.Displays.HD44780;
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

{ Comment this out to prevent the component from releasing the pins in destructor. }
{$DEFINE LCD_RESET_PINS_AFTER_DONE}

uses
  PXL.TypeDef, PXL.Types, PXL.Boards.Types;

type
  TCharacterMask = array[0..7] of Byte;

  TDisplayHD44780 = class
  private type
    TDataPins = record
      case Integer of
        0: (D0, D1, D2, D3, D4, D5, D6, D7: TPinIdentifier);
        1: (Pins: array[0..7] of TPinIdentifier);
    end;
  private
    FSystemCore: TCustomSystemCore;
    FGPIO: TCustomGPIO;

    FPinRS: TPinIdentifier;
    FPinRW: TPinIdentifier;
    FPinEN: TPinIdentifier;

    FDataPins: TDataPins;

    FScreenSize: TPoint2i;
    FCursorPosition: TPoint2i;

    FDisplayFunction: Cardinal;
    FDisplayControl: Cardinal;
    FDisplayMode: Cardinal;

    procedure PrepareDataPins;
    procedure PrepareDisplay(const AScreenSize: TPoint2i);

    procedure PulsePinEnable;
    procedure WriteRawBits4(const Value: Integer);
    procedure WriteRawBits8(const Value: Integer);
    procedure WriteCustomValue(const Value: Integer; const PinValueRS: TPinValue);

    procedure WriteCommand(const Value: Integer); inline;
    procedure WriteData(const Value: Integer); inline;

    function GetVisible: Boolean;
    procedure SetVisible(const Value: Boolean);
    function GetCursorUnderscore: Boolean;
    procedure SetCursorUnderscore(const Value: Boolean);
    function GetCursorBlinking: Boolean;
    procedure SetCursorBlinking(const Value: Boolean);
    procedure UpdateCursorPosition;
    procedure SetCursorPosition(const Value: TPoint2i);
  protected
    procedure Delay(const Microseconds: Cardinal);
  public
    { Creates display instance working in 8-bit data mode. Note that "RW" pin is not really necessary and can be
      connected to Ground on the board, while specifying "PinDisabled" for APinRW here. }
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO;
      const AScreenSize: TPoint2i; const APinRS, APinEN, APinD0, APinD1, APinD2, APinD3, APinD4, APinD5, APinD6,
      APinD7: TPinIdentifier; const APinRW: TPinIdentifier = PinDisabled); overload;

    { Creates display instance working in 4-bit data mode using as few cables as possible. Note that required data
      pins are actually last (and not the first ones!) four pins from D5 to D7. }
    constructor Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO;
      const AScreenSize: TPoint2i; const APinRS, APinEN, APinD4, APinD5, APinD6, APinD7: TPinIdentifier); overload;
    destructor Destroy; override;

    procedure Print(const Text: StdString);

    procedure Clear;
    procedure ResetCursor;

    procedure SetCharacterMask(const Location: Integer; const Mask: TCharacterMask);

    property SystemCore: TCustomSystemCore read FSystemCore;
    property GPIO: TCustomGPIO read FGPIO;

    property PinRS: TPinIdentifier read FPinRS;
    property PinRW: TPinIdentifier read FPinRW;
    property PinEN: TPinIdentifier read FPinEN;

    property PinD0: TPinIdentifier read FDataPins.D0;
    property PinD1: TPinIdentifier read FDataPins.D1;
    property PinD2: TPinIdentifier read FDataPins.D2;
    property PinD3: TPinIdentifier read FDataPins.D3;
    property PinD4: TPinIdentifier read FDataPins.D4;
    property PinD5: TPinIdentifier read FDataPins.D5;
    property PinD6: TPinIdentifier read FDataPins.D6;
    property PinD7: TPinIdentifier read FDataPins.D7;

    property ScreenSize: TPoint2i read FScreenSize;
    property Visible: Boolean read GetVisible write SetVisible;

    property CursorUnderscore: Boolean read GetCursorUnderscore write SetCursorUnderscore;
    property CursorBlinking: Boolean read GetCursorBlinking write SetCursorBlinking;
    property CursorPos: TPoint2i read FCursorPosition write SetCursorPosition;
  end;

function CharacterMask(const Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8: Byte): TCharacterMask; inline;

implementation

type
  TDisplayCommand = (
    DisplayClear = $01,
    CursorReset = $02,
    DisplayEntryMode = $04,
    DisplayControl = $08,
    DisplayFunction = $20,
    CursorMask = $40,
    CursorSet = $80);

  TEntryModeFlags = (
    LeftToRight = $02);

  TDisplayControlFlags = (
    CursorBlinking = $01,
    CursorUnderscore = $02,
    DisplayVisible = $04);

  TDisplayFunctionFlags = (
    DisplayBigFont = $04,
    DisplayMultiLine = $08,
    Interface8bits = $10);

function CharacterMask(const Row1, Row2, Row3, Row4, Row5, Row6, Row7, Row8: Byte): TCharacterMask; inline;
begin
  Result[0] := Row1;
  Result[1] := Row2;
  Result[2] := Row3;
  Result[3] := Row4;
  Result[4] := Row5;
  Result[5] := Row6;
  Result[6] := Row7;
  Result[7] := Row8;
end;

constructor TDisplayHD44780.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO;
  const AScreenSize: TPoint2i; const APinRS, APinEN, APinD0, APinD1, APinD2, APinD3, APinD4, APinD5, APinD6,
  APinD7: TPinIdentifier; const APinRW: TPinIdentifier);
begin
  FGPIO := AGPIO;
  FSystemCore := ASystemCore;

  FPinRS := APinRS;
  FPinRW := APinRW;
  FPinEN := APinEN;

  FDataPins.D0 := APinD0;
  FDataPins.D1 := APinD1;
  FDataPins.D2 := APinD2;
  FDataPins.D3 := APinD3;
  FDataPins.D4 := APinD4;
  FDataPins.D5 := APinD5;
  FDataPins.D6 := APinD6;
  FDataPins.D7 := APinD7;

  FGPIO.PinMode[FPinRS] := TPinMode.Output;

  if FPinRW <> PinDisabled then
    FGPIO.PinMode[FPinRW] := TPinMode.Output;

  FGPIO.PinMode[FPinEN] := TPinMode.Output;

  PrepareDataPins;

  if (FDataPins.D0 <> PinDisabled) and (FDataPins.D1 <> PinDisabled) and (FDataPins.D2 <> PinDisabled) and
    (FDataPins.D3 <> PinDisabled) then
    FDisplayFunction := FDisplayFunction or Ord(TDisplayFunctionFlags.Interface8bits);

  PrepareDisplay(AScreenSize);
end;

constructor TDisplayHD44780.Create(const ASystemCore: TCustomSystemCore; const AGPIO: TCustomGPIO;
  const AScreenSize: TPoint2i; const APinRS, APinEN, APinD4, APinD5, APinD6, APinD7: TPinIdentifier);
begin
  Create(ASystemCore, AGPIO, AScreenSize, APinRS, APinEN, PinDisabled, PinDisabled, PinDisabled, PinDisabled, APinD4,
    APinD5, APinD6, APinD7);
end;

destructor TDisplayHD44780.Destroy;
{$IFDEF LCD_RESET_PINS_AFTER_DONE}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF LCD_RESET_PINS_AFTER_DONE}
  for I := 0 to 7 do
    if FDataPins.Pins[I] <> PinDisabled then
      FGPIO.PinMode[FDataPins.Pins[I]] := TPinMode.Input;

  FGPIO.PinMode[FPinEN] := TPinMode.Input;

  if FPinRW <> PinDisabled then
    FGPIO.PinMode[FPinRW] := TPinMode.Input;

  FGPIO.PinMode[FPinRS] := TPinMode.Input;
{$ENDIF}

  inherited;
end;

procedure TDisplayHD44780.Delay(const Microseconds: Cardinal);
var
  BusyCounter: Cardinal;
begin
  if FSystemCore <> nil then
    FSystemCore.MicroDelay(Microseconds)
  else
  begin
    // Simulate some kind of delay using busy wait counter.
    BusyCounter := Microseconds;
    while BusyCounter <> 0 do
      Dec(BusyCounter);
  end;
end;

procedure TDisplayHD44780.PrepareDataPins;
var
  I: Integer;
begin
  for I := 0 to 7 do
    if FDataPins.Pins[I] <> PinDisabled then
      FGPIO.PinMode[FDataPins.Pins[I]] := TPinMode.Output;
end;

procedure TDisplayHD44780.PrepareDisplay(const AScreenSize: TPoint2i);
begin
  // Processor may start earlier than display while the voltage is raising, so wait until the voltage stabilizes.
  Delay(50000);

  FScreenSize := AScreenSize;
  FCursorPosition := ZeroPoint2i;

  if FScreenSize.Y > 1 then
    FDisplayFunction := FDisplayFunction or Ord(TDisplayFunctionFlags.DisplayMultiLine);

  FGPIO.PinValue[FPinRS] := TPinValue.Low;
  FGPIO.PinValue[FPinEN] := TPinValue.Low;

  if FDisplayFunction and Ord(TDisplayFunctionFlags.Interface8bits) <> 0 then
  begin
    WriteCommand(Ord(TDisplayCommand.DisplayFunction) or FDisplayFunction);
    Delay(4500);

    WriteCommand(Ord(TDisplayCommand.DisplayFunction) or FDisplayFunction);
    Delay(150);

    WriteCommand(Ord(TDisplayCommand.DisplayFunction) or FDisplayFunction);
  end
  else
  begin
    WriteRawBits4($03);
    Delay(4500);

    WriteRawBits4($03);
    Delay(4500);

    WriteRawBits4($03);
    Delay(150);

    WriteRawBits4($02);
  end;

  WriteCommand(Ord(TDisplayCommand.DisplayFunction) or FDisplayFunction);

  FDisplayControl := Ord(TDisplayControlFlags.DisplayVisible) or Ord(TDisplayControlFlags.CursorUnderscore) or
    Ord(TDisplayControlFlags.CursorBlinking);
  WriteCommand(Ord(TDisplayCommand.DisplayControl) or FDisplayControl);

  Clear;

  FDisplayMode := Ord(TEntryModeFlags.LeftToRight);
  WriteCommand(Ord(TDisplayCommand.DisplayEntryMode) or FDisplayMode);
end;

procedure TDisplayHD44780.PulsePinEnable;
begin
  FGPIO.PinValue[FPinEN] := TPinValue.Low;
  Delay(1);

  FGPIO.PinValue[FPinEN] := TPinValue.High;
  Delay(1);

  FGPIO.PinValue[FPinEN] := TPinValue.Low;
  Delay(100);
end;

procedure TDisplayHD44780.WriteRawBits4(const Value: Integer);
var
  I: Integer;
begin
  for I := 0 to 3 do
    FGPIO.PinValue[FDataPins.Pins[4 + I]] := TPinValue((Value shr I) and $01);

  PulsePinEnable;
end;

procedure TDisplayHD44780.WriteRawBits8(const Value: Integer);
var
  I: Integer;
begin
  for I := 0 to 7 do
    FGPIO.PinValue[FDataPins.Pins[I]] := TPinValue((Value shr I) and $01);

  PulsePinEnable;
end;

procedure TDisplayHD44780.WriteCustomValue(const Value: Integer; const PinValueRS: TPinValue);
begin
  FGPIO.PinValue[FPinRS] := PinValueRS;

  if FPinRW <> PinDisabled then
    FGPIO.PinValue[FPinRW] := TPinValue.Low;

  if FDisplayFunction and Ord(TDisplayFunctionFlags.Interface8bits) <> 0 then
    WriteRawBits8(Value)
  else
  begin
    WriteRawBits4(Value shr 4);
    WriteRawBits4(Value);
  end;
end;

procedure TDisplayHD44780.WriteCommand(const Value: Integer);
begin
  WriteCustomValue(Value, TPinValue.Low);
end;

procedure TDisplayHD44780.WriteData(const Value: Integer);
begin
  WriteCustomValue(Value, TPinValue.High);
end;

function TDisplayHD44780.GetVisible: Boolean;
begin
  Result := FDisplayControl and Ord(TDisplayControlFlags.DisplayVisible) <> 0;
end;

procedure TDisplayHD44780.SetVisible(const Value: Boolean);
begin
  if GetVisible <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or Ord(TDisplayControlFlags.DisplayVisible)
    else
      FDisplayControl := FDisplayControl and (not Ord(TDisplayControlFlags.DisplayVisible));

    WriteCommand(Ord(TDisplayCommand.DisplayControl) or FDisplayControl);
  end;
end;

function TDisplayHD44780.GetCursorUnderscore: Boolean;
begin
  Result := FDisplayControl and Ord(TDisplayControlFlags.CursorUnderscore) <> 0;
end;

procedure TDisplayHD44780.SetCursorUnderscore(const Value: Boolean);
begin
  if GetCursorUnderscore <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or Ord(TDisplayControlFlags.CursorUnderscore)
    else
      FDisplayControl := FDisplayControl and (not Ord(TDisplayControlFlags.CursorUnderscore));

    WriteCommand(Ord(TDisplayCommand.DisplayControl) or FDisplayControl);
  end;
end;

function TDisplayHD44780.GetCursorBlinking: Boolean;
begin
  Result := FDisplayControl and Ord(TDisplayControlFlags.CursorBlinking) <> 0;
end;

procedure TDisplayHD44780.SetCursorBlinking(const Value: Boolean);
begin
  if GetCursorBlinking <> Value then
  begin
    if Value then
      FDisplayControl := FDisplayControl or Ord(TDisplayControlFlags.CursorBlinking)
    else
      FDisplayControl := FDisplayControl and (not Ord(TDisplayControlFlags.CursorBlinking));

    WriteCommand(Ord(TDisplayCommand.DisplayControl) or FDisplayControl);
  end;
end;

procedure TDisplayHD44780.UpdateCursorPosition;
const
  RowOffsets: array[0..3] of Integer = ($00, $40, $14, $54);
begin
  WriteCommand(Ord(TDisplayCommand.CursorSet) or (FCursorPosition.X + RowOffsets[FCursorPosition.Y]));
end;

procedure TDisplayHD44780.SetCursorPosition(const Value: TPoint2i);
begin
  if FCursorPosition <> Value then
  begin
    FCursorPosition.X := Saturate(Value.X, 0, FScreenSize.X - 1);
    FCursorPosition.Y := Saturate(Value.Y, 0, FScreenSize.Y - 1);

    UpdateCursorPosition;
  end;
end;

procedure TDisplayHD44780.Clear;
begin
  WriteCommand(Ord(TDisplayCommand.DisplayClear));
  Delay(2000);
end;

procedure TDisplayHD44780.ResetCursor;
begin
  FCursorPosition := ZeroPoint2i;
  WriteCommand(Ord(TDisplayCommand.CursorReset));
  Delay(2000);
end;

procedure TDisplayHD44780.SetCharacterMask(const Location: Integer; const Mask: TCharacterMask);
var
  I: Integer;
begin
  WriteCommand(Ord(TDisplayCommand.CursorMask) or ((Location and $07) shl 3));

  for I := 0 to 7 do
    WriteData(Mask[I]);
end;

procedure TDisplayHD44780.Print(const Text: StdString);
var
  I: Integer;
begin
  for I := 1 to Length(Text) do
  begin
    WriteData(Ord(Text[I]));

    Inc(FCursorPosition.X);

    if FCursorPosition.X >= FScreenSize.X then
    begin
      FCursorPosition.X := 0;
      Inc(FCursorPosition.Y);

      if FCursorPosition.Y >= FScreenSize.Y then
        FCursorPosition.Y := 0;

      UpdateCursorPosition;
    end;
  end;
end;

end.

unit PXL.Boards.LPC11xx;
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
{< NXP LPC11xx chip series functions. }
interface

{$INCLUDE PXL.MicroConfig.inc}

uses
  PXL.Boards.Types;

type
  TMicroSystemCore = class(TCustomSystemCore)
  private const
    SystemFrequency = 12000000;
  protected const
    TimerIntResolution = 1000;
    TimerNormalizeMax = 65536;
  private
    FCPUFrequency: Cardinal;

    function GetCPUFrequency: Cardinal;
    procedure SetCPUFrequency(const Value: Cardinal);
    procedure ConfigureTimer;
  protected
    FTimerNormalize: TTickCounter;

    function IsPrimaryCore: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTickCount: TTickCounter; override;

    property CPUFrequency: Cardinal read GetCPUFrequency write SetCPUFrequency;
  end;

  { Drive mode that is used in GPIO pins. }
  TPinDriveEx = (
    { Strong low and high or high-impedance (no pull-up or pull-down resistor). }
    None,

    { Resistive high, strong low (pull-up resistor). }
    PullUp,

    { Resistive low, strong high (pull-down resistor). }
    PullDown,

    { Repeater mode }
    Repeater);

  TMicroGPIO = class(TCustomGPIO)
  private
    function GetPinDriveEx(const Pin: TPinIdentifier): TPinDriveEx; inline;
    procedure SetPinDriveEx(const Pin: TPinIdentifier; const Value: TPinDriveEx); inline;
    function GetPinHysteresis(const Pin: TPinIdentifier): Boolean; inline;
    procedure SetPinHysteresis(const Pin: TPinIdentifier; const Value: Boolean); inline;
    function GetPinOpenDrain(const Pin: TPinIdentifier): Boolean; inline;
    procedure SetPinOpenDrain(const Pin: TPinIdentifier; const Value: Boolean); inline;
    procedure ConfigureInterrupt(const Pin: TPinIdentifier; const EdgeSensing: Boolean; const SignalEdge: TSignalEdge;
      const SignalLevel: TPinValue);
  protected
    function GetPinMode(const Pin: TPinIdentifier): TPinMode; override;
    procedure SetPinMode(const Pin: TPinIdentifier; const Value: TPinMode); override;
    function GetPinValue(const Pin: TPinIdentifier): TPinValue; override;
    procedure SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue); override;
    function GetPinDrive(const Pin: TPinIdentifier): TPinDrive; override;
    procedure SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive); override;

    { Returns current function of the specified pin. These typically vary between 0 and 3, see the datasheet. }
    function GetPinFunction(const Pin: TPinIdentifier): Cardinal; inline;

    { Sets new function for the specified pin. These typically vary between 0 and 3, see the datasheet. }
    procedure SetPinFunction(const Pin: TPinIdentifier; const Value: Cardinal); inline;
  public
    constructor Create;
    destructor Destroy; override;

    { Configures the specified pin to invoke an appropriate interrupt on specific signal edge. If "none" is specified
      as signal edge, then the interrupt will occur on both edges. }
    procedure SetupInterrupt(const Pin: TPinIdentifier; const SignalEdge: TSignalEdge); overload; inline;

    { Configures the specified pin to invoke an appropriate interrupt on specific pin value (level). }
    procedure SetupInterrupt(const Pin: TPinIdentifier; const SignalLevel: TPinValue); overload; inline;

    { Clears interrupt edge status for pins that are configured as edge sensitive. }
    procedure ClearInterruptEdgeStatus(const Pin: TPinIdentifier);

    { Function for the specified pin. These typically vary between 0 and 3, see the datasheet. }
    property PinFunction[const Pin: TPinIdentifier]: Cardinal read GetPinFunction write SetPinFunction;

    { Currently set drive mode for the specified pin number. }
    property PinDriveEx[const Pin: TPinIdentifier]: TPinDriveEx read GetPinDriveEx write SetPinDriveEx;

    { Currently set pin hysteresis mode (value ranges for determining pin input value, see datasheet). }
    property PinHysteresis[const Pin: TPinIdentifier]: Boolean read GetPinHysteresis write SetPinHysteresis;

    { Whether the pin has currently set open-drain mode for output or not. }
    property PinOpenDrain[const Pin: TPinIdentifier]: Boolean read GetPinOpenDrain write SetPinOpenDrain;
  end;

  TMicroPortSPI = class(TCustomPortSPI)
  private type
    TFrequencyConfig = record
      ClockDiv: Byte;
      ClockRate: Byte;
      Prescale: Byte;
    end;
  private
    FSystemCore: TMicroSystemCore;
    FGPIO: TMicroGPIO;
    FSCKPin: TPinIdentifier;

    FFrequencyConfig: TFrequencyConfig;
    FFrequency: Cardinal;
    FBitsPerWord: TBitsPerWord;
    FMode: TSPIMode;

    function FindApproxFrequencyConfig(const ReqFrequency: Cardinal; out Config: TFrequencyConfig): Cardinal;
    procedure ResetPort;
  protected
    function GetFrequency: Cardinal; override;
    procedure SetFrequency(const Value: Cardinal); override;
    function GetBitsPerWord: TBitsPerWord; override;
    procedure SetBitsPerWord(const Value: TBitsPerWord); override;
    function GetMode: TSPIMode; override;
    procedure SetMode(const Value: TSPIMode); override;
  public
    constructor Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
      const ASCKPin: TPinIdentifier = 3; const AChipSelectMode: TChipSelectMode = TChipSelectMode.ActiveLow);
    destructor Destroy; override;

    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    property SystemCore: TMicroSystemCore read FSystemCore;
    property GPIO: TMicroGPIO read FGPIO;

    property SCKPin: TPinIdentifier read FSCKPin;
  end;

  TMicroUART = class(TCustomPortUART)
  private type
    TBaudRateConfig = record
      ClockDiv: Byte;
      DivAddVal: Byte;
      MulVal: Byte;
      DivLev: Word;
    end;
  private
    FBaudRateConfig: TBaudRateConfig;
    FBaudRate: Cardinal;
    FBitsPerWord: TBitsPerWord;
    FParity: TParity;
    FStopBits: TStopBits;

    function FindApproxBaudRate(const ReqBaudRate: Cardinal; out Config: TBaudRateConfig): Cardinal;
    procedure ResetPort;
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
    constructor Create(const ASystemCore: TMicroSystemCore);
    destructor Destroy; override;

    procedure Flush; override;
    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
  end;

implementation

uses
  cortexm0;

{$REGION 'Global Types and Constants'}

type
  PGPIO_Registers = ^TGPIO_Registers;

const
  // Indexed mapping to GPIO registers.
  GPIOMem: array[0..3] of PGPIO_Registers = (@LPC_GPIO0, @LPC_GPIO1, @LPC_GPIO2, @LPC_GPIO3);

  // IOMapping format: [7:4] - Port Number, [3:0] - I/O Number.
  // PinFuncMem defines pointers for each pin to its specific IOCON register.

{$IFDEF LPC1100L_24}
  IOMapping: array[1..28] of Byte = ($08, $09, $0A, $0B, $05, $06, $FF, $FF, $10, $11, $12, $13, $14, $15, $16, $17,
    $18, $19, $FF, $FF, $FF, $FF, $00, $01, $02, $03, $04, $07);

  PinFuncMem: array[1..28] of PLongWord = (@LPC_IOCON.PIO0_8, @LPC_IOCON.PIO0_9, @LPC_IOCON.SWCLK_PIO0_10,
    @LPC_IOCON.R_PIO0_11, @LPC_IOCON.PIO0_5, @LPC_IOCON.PIO0_6, nil, nil, @LPC_IOCON.R_PIO1_0, @LPC_IOCON.R_PIO1_1,
    @LPC_IOCON.R_PIO1_2, @LPC_IOCON.SWDIO_PIO1_3, @LPC_IOCON.PIO1_4, @LPC_IOCON.PIO1_5, @LPC_IOCON.PIO1_6,
    @LPC_IOCON.PIO1_7, @LPC_IOCON.PIO1_8, @LPC_IOCON.PIO1_9, nil, nil, nil, nil, @LPC_IOCON.RESET_PIO0_0,
    @LPC_IOCON.PIO0_1, @LPC_IOCON.PIO0_2, @LPC_IOCON.PIO0_3, @LPC_IOCON.PIO0_4, @LPC_IOCON.PIO0_7);

  {$DEFINE LPC_IOMAPPING}
{$ENDIF}

{$IFDEF LPC1100XL_48}
  IOMapping: array[1..48] of Byte = ($26, $20, $00, $01, $FF, $FF, $FF, $FF, $18, $02, $27, $28, $21, $03, $04, $05,
    $19, $34, $24, $25, $35, $06, $07, $29, $2A, $22, $08, $09, $0A, $1A, $2B, $0B, $10, $11, $12, $30, $31, $23, $13,
    $14, $FF, $1B, $32, $FF, $15, $16, $17, $33);

  PinFuncMem: array[1..48] of PLongWord = (@LPC_IOCON.PIO2_6, @LPC_IOCON.PIO2_0, @LPC_IOCON.RESET_PIO0_0,
    @LPC_IOCON.PIO0_1, nil, nil, nil, nil, @LPC_IOCON.PIO1_8, @LPC_IOCON.PIO0_2, @LPC_IOCON.PIO2_7, @LPC_IOCON.PIO2_8,
    @LPC_IOCON.PIO2_1, @LPC_IOCON.PIO0_3, @LPC_IOCON.PIO0_4, @LPC_IOCON.PIO0_5, @LPC_IOCON.PIO1_9, @LPC_IOCON.PIO3_4,
    @LPC_IOCON.PIO2_4, @LPC_IOCON.PIO2_5, @LPC_IOCON.PIO3_5, @LPC_IOCON.PIO0_6, @LPC_IOCON.PIO0_7, @LPC_IOCON.PIO2_9,
    @LPC_IOCON.PIO2_10, @LPC_IOCON.PIO2_2, @LPC_IOCON.PIO0_8, @LPC_IOCON.PIO0_9, @LPC_IOCON.SWCLK_PIO0_10,
    @LPC_IOCON.PIO1_10, @LPC_IOCON.PIO2_11, @LPC_IOCON.R_PIO0_11, @LPC_IOCON.R_PIO1_0, @LPC_IOCON.R_PIO1_1,
    @LPC_IOCON.R_PIO1_2, @LPC_IOCON.PIO3_0, @LPC_IOCON.PIO3_1, @LPC_IOCON.PIO2_3, @LPC_IOCON.SWDIO_PIO1_3,
    @LPC_IOCON.PIO1_4, nil, @LPC_IOCON.PIO1_11, @LPC_IOCON.PIO3_2, nil, @LPC_IOCON.PIO1_5, @LPC_IOCON.PIO1_6,
    @LPC_IOCON.PIO1_7, @LPC_IOCON.PIO3_3);

  {$DEFINE LPC_IOMAPPING}
{$ENDIF}

{$IFNDEF LPC_IOMAPPING}
  { If you receive the following message, then one of supported LPCxx targets has not been defined (see declarations
    above). If your particular chip is not described above, then you can try defining your own IOMapping and PinFuncMem
    constants in similar fashion. }
  {$FATAL LPCxx target not defined or unsupported.}
{$ENDIF}

{$ENDREGION}
{$REGION 'Global Variables and Functions'}

var
  SysTickCounter: TTickCounter = 0;

  { Reference to the first system core created that actually manages SysTick timer. }
  PrimaryCore: TMicroSystemCore = nil;

procedure SysTick_interrupt; [public, alias: 'SysTick_interrupt'];
begin
  Inc(SysTickCounter, TMicroSystemCore.TimerIntResolution);
end;

function GetCPUFrequency: Cardinal;
begin
  if PrimaryCore <> nil then
    Result := PrimaryCore.CPUFrequency
  else
    Result := TMicroSystemCore.SystemFrequency;
end;

{$ENDREGION}
{$REGION 'TMicroSystemCore'}

constructor TMicroSystemCore.Create;
begin
  inherited;

  FCPUFrequency := SystemFrequency;
  FTimerNormalize := 1;

  if PrimaryCore = nil then
  begin
    PrimaryCore := Self;
    ConfigureTimer;
  end;
end;

destructor TMicroSystemCore.Destroy;
begin
  if IsPrimaryCore then
  begin
    SysTick.Ctrl := $00;
    PrimaryCore := nil;
  end;

  inherited;
end;

function TMicroSystemCore.IsPrimaryCore: Boolean;
begin
  Result := (PrimaryCore = Self) or (PrimaryCore = nil);
end;

procedure TMicroSystemCore.ConfigureTimer;
var
  MaxCounter: Cardinal;
begin
  SysTick.Ctrl := $00;

  MaxCounter := CPUFrequency div (2 * TimerIntResolution);
  FTimerNormalize := (TimerNormalizeMax * TimerIntResolution) div MaxCounter;

  SysTick.Load := MaxCounter;
  SysTick.Ctrl := $03;
end;

function TMicroSystemCore.GetCPUFrequency: Cardinal;
begin
  if IsPrimaryCore then
    Result := FCPUFrequency
  else
    Result := PrimaryCore.FCPUFrequency;
end;

procedure TMicroSystemCore.SetCPUFrequency(const Value: Cardinal);
var
  Frequency, Multiplier, EnhFrequency, Phase: Cardinal;
begin
  if IsPrimaryCore then
  begin
    if FCPUFrequency <> Value then
    begin
      Frequency := Value;

      LPC_SYSCON.SYSOSCCTRL := 0;
      LPC_SYSCON.PDRUNCFG := LPC_SYSCON.PDRUNCFG and (not (1 shl 5));
    	LPC_SYSCON.SYSPLLCLKSEL := 0;
    	LPC_SYSCON.SYSPLLCLKUEN := 0;
    	LPC_SYSCON.SYSPLLCLKUEN := 1;

    	Multiplier := Frequency div SystemFrequency;
    	EnhFrequency := Multiplier * SystemFrequency * 2;
    	Frequency := SystemFrequency * Multiplier;

      Phase := 0;
    	while EnhFrequency < 156000000 do
    	begin
    		EnhFrequency := EnhFrequency * 2;
        Inc(Phase);
      end;

    	LPC_SYSCON.SYSPLLCTRL := (Multiplier - 1) or (Phase shl 5);
    	LPC_SYSCON.PDRUNCFG := LPC_SYSCON.PDRUNCFG and (not (1 shl 7));

      while (LPC_SYSCON.SYSPLLSTAT and 1) = 0 do ;

    	LPC_SYSCON.MAINCLKSEL := 3;
    	LPC_SYSCON.MAINCLKUEN := 0;
    	LPC_SYSCON.MAINCLKUEN := 1;
    	LPC_SYSCON.SYSAHBCLKDIV := 1;

      FCPUFrequency := Frequency;

      ConfigureTimer;
    end;
  end
  else
    PrimaryCore.SetCPUFrequency(Value);
end;

function TMicroSystemCore.GetTickCount: TTickCounter;
var
  FractCounter: Cardinal;
begin
  Result := SysTickCounter;

  FractCounter := SysTick.Val and $00FFFFFF;
  Inc(Result, TimerIntResolution - ((FractCounter * FTimerNormalize) div 65536));
end;

{$ENDREGION}
{$REGION 'TMicroGPIO'}

constructor TMicroGPIO.Create;
begin
  inherited;

  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL or ((1 shl 6) or (1 shl 16));
end;

destructor TMicroGPIO.Destroy;
begin
  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL and (not ((1 shl 6) or (1 shl 16)));

  inherited;
end;

function TMicroGPIO.GetPinMode(const Pin: TPinIdentifier): TPinMode;
var
  Mapping: Byte;
begin
  Mapping := IOMapping[Pin];

  if GPIOMem[Mapping shr 4].DIR and (Cardinal(1) shl (Mapping and $0F)) > 0 then
    Result := TPinMode.Output
  else
    Result := TPinMode.Input;
end;

procedure TMicroGPIO.SetPinMode(const Pin: TPinIdentifier; const Value: TPinMode);
var
  Mapping, GPIONum: Byte;
begin
  Mapping := IOMapping[Pin];
  GPIONum := Mapping shr 4;

  if Value = TPinMode.Output then
    GPIOMem[GPIONum].DIR := GPIOMem[GPIONum].DIR or (Cardinal(1) shl (Mapping and $0F))
  else
    GPIOMem[GPIONum].DIR := GPIOMem[GPIONum].DIR and (not (Cardinal(1) shl (Mapping and $0F)));
end;

function TMicroGPIO.GetPinValue(const Pin: TPinIdentifier): TPinValue;
var
  Mapping, BitNum: Byte;
begin
  Mapping := IOMapping[Pin];
  BitNum := Mapping and $0F;

  if GPIOMem[Mapping shr 4].MASKED_ACCESS[1 shl BitNum] and (Cardinal(1) shl BitNum) > 0 then
    Result := TPinValue.High
  else
    Result := TPinValue.Low;
end;

procedure TMicroGPIO.SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue);
var
  Mapping, BitNum: Byte;
begin
  Mapping := IOMapping[Pin];
  BitNum := Mapping and $0F;

  GPIOMem[Mapping shr 4].MASKED_ACCESS[1 shl BitNum] := Cardinal(Ord(Value)) shl BitNum;
end;

function TMicroGPIO.GetPinDriveEx(const Pin: TPinIdentifier): TPinDriveEx;
begin
  Result := TPinDriveEx((PinFuncMem[Pin]^ shr 3) and $03);
end;

procedure TMicroGPIO.SetPinDriveEx(const Pin: TPinIdentifier; const Value: TPinDriveEx);
begin
  PinFuncMem[Pin]^ := (PinFuncMem[Pin]^ and (not ($03 shl 3))) or (Cardinal(Ord(Value)) shl 3);
end;

function TMicroGPIO.GetPinDrive(const Pin: TPinIdentifier): TPinDrive;
begin
  Result := TPinDrive(GetPinDriveEx(Pin));
end;

procedure TMicroGPIO.SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive);
begin
  SetPinDriveEx(Pin, TPinDriveEx(Value));
end;

function TMicroGPIO.GetPinFunction(const Pin: TPinIdentifier): Cardinal;
begin
  Result := PinFuncMem[Pin]^ and $07;
end;

procedure TMicroGPIO.SetPinFunction(const Pin: TPinIdentifier; const Value: Cardinal);
begin
  PinFuncMem[Pin]^ := PinFuncMem[Pin]^ or (Value and $07);
end;

function TMicroGPIO.GetPinHysteresis(const Pin: TPinIdentifier): Boolean;
begin
  Result := (PinFuncMem[Pin]^ shr 5) <> 0;
end;

procedure TMicroGPIO.SetPinHysteresis(const Pin: TPinIdentifier; const Value: Boolean);
begin
  if Value then
    PinFuncMem[Pin]^ := PinFuncMem[Pin]^ or (1 shl 5)
  else
    PinFuncMem[Pin]^ := PinFuncMem[Pin]^ and (not (1 shl 5));
end;

function TMicroGPIO.GetPinOpenDrain(const Pin: TPinIdentifier): Boolean;
begin
  Result := (PinFuncMem[Pin]^ shr 10) <> 0;
end;

procedure TMicroGPIO.SetPinOpenDrain(const Pin: TPinIdentifier; const Value: Boolean);
begin
  if Value then
    PinFuncMem[Pin]^ := PinFuncMem[Pin]^ or (1 shl 10)
  else
    PinFuncMem[Pin]^ := PinFuncMem[Pin]^ and (not (1 shl 10));
end;

procedure TMicroGPIO.ConfigureInterrupt(const Pin: TPinIdentifier; const EdgeSensing: Boolean;
  const SignalEdge: TSignalEdge; const SignalLevel: TPinValue);
var
  Mapping, GPIONum, IRQNum: Byte;
  LGMem: PGPIO_Registers;
  BitMask: Cardinal;
begin
  Mapping := IOMapping[Pin];

  GPIONum := Mapping shr 4;
  LGMem := GPIOMem[GPIONum];
  BitMask := Cardinal(1) shl (Mapping and $0F);
  IRQNum := 31 - GPIONum;

  // Set the pin to input (pin drive should be set by the user).
  LGMem.DIR := LGMem.DIR and (not BitMask);

  // Set Interrupt Sense
  if EdgeSensing then
    LGMem.&IS := LGMem.&IS and (not BitMask)
  else
    LGMem.&IS := LGMem.&IS or BitMask;

  // Set Interrupt Both Edges and Interrupt Event
  if EdgeSensing then
  begin
    if SignalEdge <> TSignalEdge.None then
    begin
      LGMem.IBE := LGMem.IBE and (not BitMask);

      if SignalEdge = TSignalEdge.Falling then
        LGMem.IEV := LGMem.IEV and (not BitMask)
      else
        LGMem.IEV := LGMem.IEV or BitMask;
    end
    else
      LGMem.IBE := LGMem.IBE or BitMask;
  end
  else
  begin
    if SignalLevel = TPinValue.Low then
      LGMem.IEV := LGMem.IEV and (not BitMask)
    else
      LGMem.IEV := LGMem.IEV or BitMask;
  end;

  // Enable Interrupt Mask
  LGMem.IE := LGMem.IE or BitMask;

  // Enable Interrupt Source
  NVIC.ISER := 1 shl IRQNum;
end;

procedure TMicroGPIO.SetupInterrupt(const Pin: TPinIdentifier; const SignalEdge: TSignalEdge);
begin
  ConfigureInterrupt(Pin, True, SignalEdge, TPinValue.Low);
end;

procedure TMicroGPIO.SetupInterrupt(const Pin: TPinIdentifier; const SignalLevel: TPinValue);
begin
  ConfigureInterrupt(Pin, False, TSignalEdge.None, SignalLevel);
end;

procedure TMicroGPIO.ClearInterruptEdgeStatus(const Pin: TPinIdentifier);
var
  Mapping: Byte;
begin
  Mapping := IOMapping[Pin];
  GPIOMem[Mapping shr 4].IC := (Cardinal(1) shl (Mapping and $0F));
end;

{$ENDREGION}
{$REGION 'TMicroPortSPI'}

constructor TMicroPortSPI.Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
  const ASCKPin: TPinIdentifier; const AChipSelectMode: TChipSelectMode);
begin
  inherited Create(AChipSelectMode);

  FSystemCore := ASystemCore;
  FGPIO := AGPIO;

  FSCKPin := ASCKPin;
  if FSCKPin <> 3 then
    FSCKPin := 6;

  if FGPIO <> nil then
  begin
    FGPIO.PinFunction[1] := (FGPIO.PinFunction[1] and (not $07)) or $01;
    FGPIO.PinFunction[2] := (FGPIO.PinFunction[2] and (not $07)) or $01;

    if FChipSelectMode <> TChipSelectMode.Disabled then
      FGPIO.PinFunction[25] := (FGPIO.PinFunction[25] and (not $07)) or $01;

    if FSCKPin = 3 then
      FGPIO.PinFunction[3] := (FGPIO.PinFunction[3] and (not $07)) or $02
    else
      FGPIO.PinFunction[6] := (FGPIO.PinFunction[6] and (not $07)) or $02;
  end;

  if FSCKPin = 3 then
    LPC_IOCON.SCK_LOC := (LPC_IOCON.SCK_LOC and (not $03)) or $00
  else
    LPC_IOCON.SCK_LOC := (LPC_IOCON.SCK_LOC and (not $03)) or $02;

  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL or (1 shl 11);
  LPC_SYSCON.PRESETCTRL := LPC_SYSCON.PRESETCTRL or $01;

  FFrequency := FindApproxFrequencyConfig(DefaultSPIFrequency, FFrequencyConfig);
  FBitsPerWord := 8;

  ResetPort;
end;

destructor TMicroPortSPI.Destroy;
begin
  LPC_SSP0.CR1 := $00;
  LPC_SYSCON.PRESETCTRL := LPC_SYSCON.PRESETCTRL and (not $01);
  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL and (not (1 shl 11));

  inherited;
end;

function TMicroPortSPI.FindApproxFrequencyConfig(const ReqFrequency: Cardinal; out Config: TFrequencyConfig): Cardinal;
var
  SSPClock, Diff, BestDiff, ConfigFreq: Cardinal;
  ClockDiv, Attempts: Integer;
  TempConfig: TFrequencyConfig;
begin
  BestDiff := High(Cardinal);
  Result := 0;

  Config.ClockDiv := 16;
  Config.ClockRate := 0;
  Config.Prescale := 2;

  for ClockDiv := 16 downto 1 do
  begin
    TempConfig.ClockDiv := Cardinal(ClockDiv);
    TempConfig.ClockRate := 0;
    TempConfig.Prescale := 2;

    SSPClock := GetCPUFrequency div TempConfig.ClockDiv;
    Attempts := 0;

    while True do
    begin
      ConfigFreq := SSPClock div ((Cardinal(TempConfig.ClockRate) + 1) * TempConfig.Prescale);
      if ConfigFreq < ReqFrequency then
      begin
        Diff := ReqFrequency - ConfigFreq;
        if Diff < BestDiff then
        begin
          Config := TempConfig;
          BestDiff := Diff;
          Result := ConfigFreq;
        end;

        Inc(Attempts);
        if Attempts > 1 then
          Break;
      end;

      if TempConfig.ClockRate >= 255 then
      begin
        if TempConfig.Prescale >= 254 then
          Break;

        TempConfig.ClockRate := 0;
        Inc(TempConfig.Prescale, 2);
      end
      else
        Inc(TempConfig.ClockRate);
    end;
  end;
end;

procedure TMicroPortSPI.ResetPort;
begin
  LPC_SSP0.CR1 := $00;

  LPC_SYSCON.PRESETCTRL := LPC_SYSCON.PRESETCTRL or $01;
  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL or (1 shl 11);
  LPC_SYSCON.SSP0CLKDIV := FFrequencyConfig.ClockDiv;

  LPC_SSP0.CPSR := FFrequencyConfig.Prescale;

  LPC_SSP0.CR0 := (Cardinal(FBitsPerWord) - 1) or ((Cardinal(FMode) and $03) shl 6) or
    (Cardinal(FFrequencyConfig.ClockRate) shl 8);

  LPC_SSP0.CR1 := $00;
  LPC_SSP0.CR1 := $02;
end;

function TMicroPortSPI.GetFrequency: Cardinal;
begin
  Result := FFrequency;
end;

procedure TMicroPortSPI.SetFrequency(const Value: Cardinal);
begin
  if FFrequency <> Value then
  begin
    FFrequency := FindApproxFrequencyConfig(Value, FFrequencyConfig);
    ResetPort;
  end;
end;

function TMicroPortSPI.GetBitsPerWord: TBitsPerWord;
begin
  Result := FBitsPerWord;
end;

procedure TMicroPortSPI.SetBitsPerWord(const Value: TBitsPerWord);
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    ResetPort;
  end;
end;

function TMicroPortSPI.GetMode: TSPIMode;
begin
  Result := FMode;
end;

procedure TMicroPortSPI.SetMode(const Value: TSPIMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    ResetPort;
  end;
end;

function TMicroPortSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  ReadBytes, WriteBytes, Dummy: Cardinal;
begin
  ReadBytes := 0;
  WriteBytes := 0;

  while ((ReadBuffer <> nil) and (ReadBytes < BufferSize)) or ((WriteBuffer <> nil) and (WriteBytes < BufferSize)) do
  begin
    if WriteBuffer <> nil then
    begin // Put data to Write FIFOs.
      if (WriteBytes < BufferSize) and (LPC_SSP0.SR and $02 <> 0) then
      begin
        LPC_SSP0.DR := PByte(PtrUInt(WriteBuffer) + WriteBytes)^;
        Inc(WriteBytes);
      end;
    end
    else if LPC_SSP0.SR and $15 = $01 then
      // Put dummy data to Write FIFOs to allow read operation.
      LPC_SSP0.DR := $FF;

    if ReadBuffer <> nil then
    begin // Get data from Read FIFOs.
      if (ReadBytes < BufferSize) and (LPC_SSP0.SR and $04 <> 0) and
        ((ReadBuffer <> WriteBuffer) or (ReadBytes < WriteBytes)) then
      begin
        PByte(PtrUInt(ReadBuffer) + ReadBytes)^ := LPC_SSP0.DR;
        Inc(ReadBytes);
      end;
    end
    else
    begin // Flush Read FIFOs.
      while LPC_SSP0.SR and $04 <> 0 do
        Dummy := LPC_SSP0.DR;
    end;
  end;

  // Wait until data has been fully transmitted.
  while (LPC_SSP0.SR and $01 = 0) or (LPC_SSP0.SR and $10 <> 0) do ;

  if WriteBytes > ReadBytes then
    Result := WriteBytes
  else
    Result := ReadBytes;
end;

{$ENDREGION}
{$REGION 'TMicroUART'}

constructor TMicroUART.Create(const ASystemCore: TMicroSystemCore);
begin
  inherited Create(ASystemCore);

  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL or (1 shl 12);

  FBaudRate := FindApproxBaudRate(DefaultUARTBaudRate, FBaudRateConfig);
  FBitsPerWord := 8;
  FParity := TParity.None;
  FStopBits := TStopBits.One;

  ResetPort;
end;

destructor TMicroUART.Destroy;
begin
  LPC_UART.IIR_FCR := $00;
  LPC_SYSCON.SYSAHBCLKCTRL := LPC_SYSCON.SYSAHBCLKCTRL and (not (1 shl 12));

  inherited;
end;

function TMicroUART.FindApproxBaudRate(const ReqBaudRate: Cardinal; out Config: TBaudRateConfig): Cardinal;
const
  DefaultClockDiv = 4;
var
  PCLK, FRest, DLest, ConfigBaud: Cardinal;
  I, J, Diff, BestDiff: Integer;
  TempConfig: TBaudRateConfig;
begin
  TempConfig.ClockDiv := DefaultClockDiv;
  PCLK := TMicroSystemCore(FSystemCore).CPUFrequency div TempConfig.ClockDiv;

  if PCLK mod (16 * ReqBaudRate) <> 0 then
  begin
    BestDiff := High(Integer);
    Result := 0;

    for J := 0 to 14 do
      for I := J + 1 to 15 do
      begin
        TempConfig.DivAddVal := J;
        TempConfig.MulVal := I;

        FRest := Cardinal(16384) + (Cardinal(TempConfig.DivAddVal) * Cardinal(16384)) div TempConfig.MulVal;

        DLest := (UInt64(16384) * PCLK) div (UInt64(16) * ReqBaudRate * FRest);
        if (DLest = 0) or (DLest > High(Word)) then
          Continue;

        TempConfig.DivLev := DLest;

        ConfigBaud := (UInt64(16384) * PCLK) div (UInt64(16) * FRest * TempConfig.DivLev);

        Diff := Abs(Int64(ReqBaudRate) - ConfigBaud);
        if Diff < BestDiff then
        begin
          Config := TempConfig;
          BestDiff := Diff;
          Result := ConfigBaud;
        end;
      end;

    if BestDiff = High(Integer) then
    begin
      Config.ClockDiv := TempConfig.ClockDiv;
      Config.DivAddVal := 0;
      Config.MulVal := 1;
      Config.DivLev := PCLK div (16 * ReqBaudRate);
      Result := ReqBaudRate;
    end;
  end
  else
  begin
    Config.ClockDiv := TempConfig.ClockDiv;
    Config.DivAddVal := 0;
    Config.MulVal := 1;
    Config.DivLev := PCLK div (16 * ReqBaudRate);
    Result := ReqBaudRate;
  end;
end;

procedure TMicroUART.ResetPort;
var
  LValue: Byte;
begin
  LPC_UART.IIR_FCR := $00;

  LPC_SYSCON.UARTCLKDIV := Cardinal(FBaudRateConfig.ClockDiv);

  if FBitsPerWord = 5 then
    LValue := $00
  else if FBitsPerWord = 6 then
    LValue := $01
  else if FBitsPerWord = 7 then
    LValue := $02
  else
    LValue := $03;

  if FStopBits <> TStopBits.One then
    LValue := LValue or $04;

  if FParity <> TParity.None then
  begin
    LValue := LValue or $08;

    if FParity = TParity.Even then
      LValue := LValue or $10;
  end;

	LPC_UART.LCR := $80 or LValue;
	LPC_UART.RBR_THR_DLL := FBaudRateConfig.DivLev and $FF;
	LPC_UART.DLM_IER := FBaudRateConfig.DivLev shr 8;
	LPC_UART.FDR := (FBaudRateConfig.MulVal shl 4) or FBaudRateConfig.DivAddVal;
	LPC_UART.LCR := LValue;

  LPC_UART.IIR_FCR := $07;
end;

function TMicroUART.GetBaudRate: Cardinal;
begin
  Result := FBaudRate;
end;

procedure TMicroUART.SetBaudRate(const Value: Cardinal);
begin
  if FBaudRate <> Value then
  begin
    FBaudRate := FindApproxBaudRate(Value, FBaudRateConfig);
    ResetPort;
  end;
end;

function TMicroUART.GetBitsPerWord: TBitsPerWord;
begin
  Result := FBitsPerWord;
end;

procedure TMicroUART.SetBitsPerWord(const Value: TBitsPerWord);
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    ResetPort;
  end;
end;

function TMicroUART.GetParity: TParity;
begin
  Result := FParity;
end;

procedure TMicroUART.SetParity(const Value: TParity);
begin
  if FParity <> Value then
  begin
    FParity := Value;
    ResetPort;
  end;
end;

function TMicroUART.GetStopBits: TStopBits;
begin
  Result := FStopBits;
end;

procedure TMicroUART.SetStopBits(const Value: TStopBits);
begin
  if FStopBits <> Value then
  begin
    FStopBits := Value;
    ResetPort;
  end;
end;

procedure TMicroUART.Flush;
begin
  LPC_UART.IIR_FCR := $07;
end;

function TMicroUART.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := 0;

  while (Result < BufferSize) and (LPC_UART.LSR and $01 <> 0) do
  begin
    PByte(PtrUInt(Buffer) + Result)^ := LPC_UART.RBR_THR_DLL;
    Inc(Result);
  end;
end;

function TMicroUART.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := 0;

  while Result < BufferSize do
  begin
    while LPC_UART.LSR and (1 shl 5) = 0 do ;

    LPC_UART.RBR_THR_DLL := PByte(PtrUInt(Buffer) + Result)^;
    Inc(Result);
  end;
end;

{$ENDREGION}

end.


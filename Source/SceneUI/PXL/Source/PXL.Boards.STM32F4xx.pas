unit PXL.Boards.STM32F4xx;
(*
 * This file is a contribution to Pascal eXtended Library, copyright (c) 2015  Michael Ring.
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
{< ST Micro Nucleo F4xx board series functions. }
interface

{$INCLUDE PXL.MicroConfig.inc}

uses
  PXL.Boards.Types;

{$INCLUDE PXL.STM32F4Config.inc}

type
  pTSPI_Registers = ^TSPI_Registers;
  pTUSART_Registers = ^TUSART_Registers;
  pTI2C_Registers = ^TI2C_Registers;
  pTADC_Registers = ^TADC_Registers;

  TMicroSystemCore = class(TCustomSystemCore)
  type
    TOSCParameters = record
      FREQUENCY : longWord;
      PLLM : byte;
      PLLN : word;
      PLLP : byte;
      PLLQ : byte;
      HPRE : byte;
    end;
  private const
    SystemFrequency = 16000000;

  protected const
    TimerIntResolution = 1000;
    TimerNormalizeMax = 65536;

  private
    FCPUFrequency: Cardinal;
    procedure ConfigureSystem;
    function GetCPUFrequency: Cardinal;
    function getFrequencyParameters(aFrequency,aOSCCLOCK,aSYSCLOCK_MAX,aPLLN_MIN,aPLLN_MAX : longWord):TOSCParameters;
    procedure SetCPUFrequency(const Value: Cardinal);
    procedure ConfigureTimer;
    function GetSysTickClockFrequency : Cardinal;
    function GetHCLKFrequency : Cardinal;
    function GetFCLKFrequency : Cardinal;
    function GetAPB1PeripheralClockFrequency : Cardinal;
    function GetAPB1TimerClockFrequency : Cardinal;
    function GetAPB2PeripheralClockFrequency : Cardinal;
    function GetAPB2TimerClockFrequency : Cardinal;
  protected
    FTimerNormalize: TTickCounter;
    function IsPrimaryCore: Boolean;
  public
    constructor Create;
    destructor Destroy; override;

    function GetTickCount: TTickCounter; override;

    property CPUFrequency: Cardinal read GetCPUFrequency write SetCPUFrequency;
  end;

  TPinModeEx = (
    { Pin set for input / high impedance }
    Input = 0,

    { Pin set for output }
    Output = 1,

    { Pin set for alternate Pinfunction }
    Alternate = 2,

    { Pin set for analog Pinfunction }
    Analog = 3
  );

  TMicroGPIO = class(TCustomGPIO)
  private type
    pTGPIO_Registers = ^TGPIO_Registers;
  private const
    // Indexed mapping to GPIO registers.
    GPIOMem: array[0..8] of pTGPIO_Registers = (@GPIOA,
                                               @GPIOB,
                                               @GPIOC,
                                               {$ifdef has_gpiod}@GPIOD{$else}nil{$endif},
                                               {$ifdef has_gpioe}@GPIOE{$else}nil{$endif},
                                               {$ifdef has_gpiof}@GPIOF{$else}nil{$endif},
                                               {$ifdef has_gpiog}@GPIOG{$else}nil{$endif},
                                               @GPIOH,
                                               {$ifdef has_gpioi}@GPIOI{$else}nil{$endif});

    { Returns current function of the specified pin. These typically vary between 0 and 15, see the datasheet. }
    function GetPinFunction(const Pin: TPinIdentifier): Cardinal; inline;

    { Sets new function for the specified pin. These typically vary between 0 and 15, see the datasheet. }
    procedure SetPinFunction(const Pin: TPinIdentifier; const Value: Cardinal); inline;
  protected
    function GetPinMode(const Pin: TPinIdentifier): TPinMode; override;
    function GetPinModeEx(const Pin: TPinIdentifier): TPinModeEx; virtual;
    procedure SetPinMode(const Pin: TPinIdentifier; const Value: TPinMode); override;
    procedure SetPinModeEx(const Pin: TPinIdentifier; const Value: TPinModeEx); virtual;
    function GetPinValue(const Pin: TPinIdentifier): TPinValue; override;
    procedure SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue); override;
    function GetPinDrive(const Pin: TPinIdentifier): TPinDrive; override;
    procedure SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive); override;
    function GetPinOpenDrain(const Pin: TPinIdentifier): Boolean; inline;
    procedure SetPinOpenDrain(const Pin: TPinIdentifier; const Value: Boolean); inline;

  public
    constructor Create;
    destructor Destroy; override;

    { Function for the specified pin. These typically vary between 0 and 15, see the datasheet. }
    property PinFunction[const Pin: TPinIdentifier]: Cardinal read GetPinFunction write SetPinFunction;

    { Function for the specified pin. These typically vary between 0 and 15, see the datasheet. }
    property PinModeEx[const Pin: TPinIdentifier]: TPinModeEx read GetPinModeEx write SetPinModeEx;

    { Whether the pin has currently set open-drain mode for output or not. }
    property PinOpenDrain[const Pin: TPinIdentifier]: Boolean read GetPinOpenDrain write SetPinOpenDrain;

  end;


  TSPIModeEx = (
    Mode0=%00,
    Mode1=%01,
    Mode2=%10,
    Mode3=%11
  );

  TSPIBitsPerWordEx = (
    Eight=0,
    Sixteen=1
  );

  TSPINssMode = (
    Hardware,
    Software
  );

  TMicroPortSPI = class(TCustomPortSPI)
  private
    FSystemCore: TMicroSystemCore;
    FGPIO: TMicroGPIO;
    pSPI : pTSPI_Registers;

    FDivider: Cardinal;
    FBitsPerWord: TSPIBitsPerWordEx;
    FMode: TSPIModeEx;
    FNssMode : TSPINSSMode;
    FNssPin : Cardinal;

    function FindDividerValue(const ReqFrequency: Cardinal) : Cardinal;
    procedure ResetPort;
  protected
    function GetFrequency: Cardinal; override;
    procedure SetFrequency(const Value: Cardinal); override;
    function GetBitsPerWord: TBitsPerWord; override;
    procedure SetBitsPerWordEx(const Value: TSPIBitsPerWordEx); virtual;
    function GetBitsPerWordEx: TSPIBitsPerWordEx; virtual;
    procedure SetBitsPerWord(const Value: TBitsPerWord); override;
    function GetMode: TSPIMode; override;
    procedure SetMode(const Value: TSPIMode); override;
    function GetModeEx: TSPIModeEx; virtual;
    procedure SetModeEx(const Value: TSPIModeEx); virtual;
    function GetNssMode: TSPINssMode; virtual;
    procedure SetNssMode(const Value: TSPINssMode); virtual;
  public
    constructor Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
                       const ApSpiDevice : pTSPI_Registers = nil;
                       const AMosiPin : TPinIdentifier = TNativePin.None;
                       const AMisoPin : TPinIdentifier = TNativePin.None;
                       const ASCKPin : TPinIdentifier = TNativePin.None;
                       const ACSPin : TPinIdentifier = TNativePin.None);
    destructor Destroy; override;

    function TransferWord(const Value : word): word; virtual;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    property SystemCore: TMicroSystemCore read FSystemCore;
    property GPIO: TMicroGPIO read FGPIO;
    property BitsPerWordEx : TSPIBitsPerWordEx read GetBitsPerWordEx write SetBitsPerWordEx;
    property NssMode : TSPINssMode read GetNssMode write SetNssMode;

    //property SCKPin: TPinIdentifier read FSCKPin;
  end;

  TMicroUART = class(TCustomPortUART)
  private
    FGPIO: TMicroGPIO;
    FSystemCoreEx : TMicroSystemCore;
    pUSART : ^TUSART_Registers;
    FBaudRate: Cardinal;
    FBitsPerWord: TBitsPerWord;
    FParity: TParity;
    FStopBits: TStopBits;

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
    constructor Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
                       const ApUSART: pTUSART_Registers=nil;
                       const ARxPin : TPinIdentifier = TNativePin.None;
                       const ATxPin : TPinIdentifier = TNativePin.None);
    destructor Destroy; override;

    procedure Flush; override;
    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    property GPIO: TMicroGPIO read FGPIO;
  end;

implementation
uses
  cortexm4;

{$REGION 'Global Types, Constants and Variables'}

var
  SysTickCounter: TTickCounter = 0;

  { Reference to the first system core created that actually manages SysTick timer. }
  PrimaryCore: TMicroSystemCore = nil;

(*
procedure SET_BIT(var AReg: longword; ABits: longword); inline;
begin
  AReg := AReg or ABits;
end;

procedure CLEAR_BIT(var AReg: longword; ABits: longword); inline;
begin
  AReg := AReg and (not ABits);
end;

procedure CLEAR_REG(var AReg: longword); inline;
begin
  AReg := 0;
end;

procedure MODIFY_REG(var AReg: longword; AMask, ABits: longword); inline;
begin
  AReg := (AReg and (not AMask)) or ABits;
end;
*)

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
    ConfigureSystem;
    ConfigureTimer;
  end;
end;

destructor TMicroSystemCore.Destroy;
begin
  if IsPrimaryCore then
  begin
    SysTick.CTRL := $00;
    PrimaryCore := nil;
  end;

  inherited;
end;

function TMicroSystemCore.IsPrimaryCore: Boolean;
begin
  Result := (PrimaryCore = Self) or (PrimaryCore = nil);
end;


function TMicroSystemCore.GetSysTickClockFrequency : Cardinal;
begin
  //if (SysTick.Ctrl and (1 shl 2)) = 0 then
    // External Clock is always CPUFrequency / 8
  //  Result := FCPUFrequency div 8
  //else
    Result := FCPUFrequency;
end;

function TMicroSystemCore.GetAPB1PeripheralClockFrequency : Cardinal;
var
  divider : byte;
begin
  divider := (RCC.CFGR shr 10) and $07;
  if Divider < 4 then
    Result := FCPUFrequency
  else
    Result := FCPUFrequency div longWord(2 shl (divider and $03));
end;

function TMicroSystemCore.GetAPB1TimerClockFrequency : Cardinal;
begin
  if RCC.DCKCFGR and (1 shl 24)  = 0 then
  begin
    if ((RCC.CFGR shr 13) and $07) < 4 then
      Result := GetAPB2PeripheralClockFrequency
    else
      Result := GetAPB2PeripheralClockFrequency*2;
  end
  else
    if ((RCC.CFGR shr 10) and $07) < 5 then
      Result := GetAPB2PeripheralClockFrequency
    else
      Result := GetAPB2PeripheralClockFrequency*4;
end;

function TMicroSystemCore.GetAPB2PeripheralClockFrequency : Cardinal;
var
  Divider : byte;
begin
  divider := (RCC.CFGR shr 13) and $07;
  if Divider < 4 then
    Result := FCPUFrequency
  else
    Result := FCPUFrequency div longWord(2 shl (divider and $03));
end;

function TMicroSystemCore.GetAPB2TimerClockFrequency : Cardinal;
begin
  if (RCC.DCKCFGR and (1 shl 24)) = 0 then
  begin
    if (RCC.CFGR shr 13) and $07 < 4 then
      Result := GetAPB2PeripheralClockFrequency
    else
      Result := GetAPB2PeripheralClockFrequency*2;
  end
  else
    if (RCC.CFGR shr 13) and $07 < 5 then
      Result := GetAPB2PeripheralClockFrequency
    else
      Result := GetAPB2PeripheralClockFrequency*4;
end;

function TMicroSystemCore.GetHCLKFrequency : Cardinal;
begin
  Result := FCPUFrequency;
end;

function TMicroSystemCore.GetFCLKFrequency : Cardinal;
begin
  Result := FCPUFrequency;
end;

procedure TMicroSystemCore.ConfigureSystem;
var
  temp : longWord;
begin
  //PWR Enable PWR Subsystem Clock
  RCC.APB1ENR := RCC.APB1ENR or (1 shl 28);
  //Read back value for a short delay
  temp := RCC.APB1ENR and (1 shl 28);
  //Set Regulator to full power
  PWR.CR := PWR.CR or  (%11 shl 14);
  //Read back value for a short delay
  temp := PWR.CR and (%11 shl 14);
end;

procedure TMicroSystemCore.ConfigureTimer;
begin
  SysTick.CTRL := 0;
  SysTick.LOAD := (GetSysTickClockFrequency div TimerIntResolution) - 1;
  SysTick.VAL:= 0;
  SysTick.CTRL := %0111;
  asm
    cpsie if // enable faults and interrupts
  end;
end;

function TMicroSystemCore.GetCPUFrequency: Cardinal;
begin
  if IsPrimaryCore then
    Result := FCPUFrequency
  else
    Result := PrimaryCore.FCPUFrequency;
end;

function TMicroSystemCore.getFrequencyParameters(aFrequency,aOSCCLOCK,aSYSCLOCK_MAX,aPLLN_MIN,aPLLN_MAX : longWord):TOSCParameters;
const
  HPRE_VALS : array[7..15] of word = (1,2,4,8,16,64,128,256,512);
var
  PLLQ,PLLP,HPRE,VCOFREQ,SYSCLOCK : longWord;
  //PLLM 2..63
  //PLLN 192..432
  //PLLP 2 4 6 8
  //PLLQ 2..15
  //HPRE 2 4 8 16 64 128 256 512
  //SYSCLOCK MAX 84

begin
  result.FREQUENCY := 0;
  result.PLLM := aOSCCLOCK div 1000000;
  for PLLQ := 2 to 15 do
  begin
    VCOFREQ := PLLQ*48000000;
    if (VCOFREQ >= aPLLN_MIN*1000000) and  (VCOFREQ <= aPLLN_MAX*1000000) then
    begin
      // We are now in the Valid Frequency Range of the VCO
      result.PLLN := VCOFREQ div 1000000;
      for PLLP  := 0 to 3 do
      begin
        SYSCLOCK := VCOFREQ div ((PLLP+1) shl 1);
        if SYSCLOCK <= aSYSCLOCK_MAX then
        begin
          for HPRE := 7 to 15 do
            if SYSCLOCK div HPRE_VALS[HPRE] = aFrequency then
            begin
              result.FREQUENCY := SYSCLOCK div HPRE_VALS[HPRE];
              result.PLLQ := PLLQ;
              result.PLLP := PLLP;
              result.HPRE := HPRE;
              exit;
            end;
        end;
      end;
    end;
  end;
end;


procedure TMicroSystemCore.SetCPUFrequency(const Value: Cardinal);
{$if defined(stm32f401xx)}
const
  HSICLOCK=16000000;
  SYSCLOCK_MAX=84000000;
  PLLN_MIN=192;
  PLLN_MAX=432;
{$elseif defined(stm32f411xx)}
const
  HSICLOCK=16000000;
  SYSCLOCK_MAX=100000000;
  PLLN_MIN=100;
  PLLN_MAX=432;

{$elseif defined(stm32f446xx)}
const
  HSICLOCK=16000000;
  SYSCLOCK_MAX=180000000;
  PLLN_MIN=100;
  PLLN_MAX=432;
{$else}
  {$error System Clock definitions are missing for chip family, please edit source file}
{$endif}
var
  value2,i : longWord;
  Params : TOscParameters;
begin
  Value2 := (Value div 1000000) * 1000000;
  if IsPrimaryCore then
  begin
    if FCPUFrequency <> Value2 then
    begin
      //Set Flash Waitstates
      if Value2 >= 60000000 then
        FLASH.ACR := 2
      else if Value2 >= 30000000 then
        FLASH.ACR := 1
      else
        FLASH.ACR := 0;
      //Read Register to activate
      i := FLASH.ACR;
      //Make sure that HSI Clock is enabled
      if RCC.CR and (1 shl 0) = 0 then
        RCC.CR := RCC.CR or (1 shl 0);

      //Wait for HSI Clock to be stable
      while (RCC.CR and (1 shl 1)) = 0 do
        ;

      if (RCC.CFGR and (%11 shl 2)) <> 0 then
         RCC.CFGR := RCC.CFGR and not (%11 shl 0);

      //Wait until HSI Clock is activated
      while (RCC.CFGR  and (%11 shl 2)) <> 0 do
        ;

      //PLLON Disable PLL if active
      if RCC.CR and (1 shl 24) = 1 then
        RCC.CR := RCC.CR and (not (1 shl 24));

      //PLLRDY Wait for PLL to shut down
      while (RCC.CR and (1 shl 25)) <> 0 do
        ;

      Params := getFrequencyParameters(Value2,HSICLOCK,SYSCLOCK_MAX,PLLN_MIN,PLLN_MAX);
      while Params.Frequency = 0 do
      begin
        Value2 := Value2 - 1000000;
        Params := getFrequencyParameters(Value2,HSICLOCK,SYSCLOCK_MAX,PLLN_MIN,PLLN_MAX);
      end;

      RCC.PLLCFGR := (%111 shl 28) or (Params.PLLQ shl 24) or (0 shl 22) or (Params.PLLP shl 16) or (Params.PLLN shl 6)
                     or Params.PLLM;

      RCC.CFGR := (0 shl 13) or (%100 shl 10) or (Params.HPRE shl 4);

      //PLLON Enable PLL
      RCC.CR := RCC.CR or (1 shl 24);

      //PLLRDY Wait for PLL to lock
      while (RCC.CR and (1 shl 25)) = 0 do
        ;

      //Enable PLL
      RCC.CFGR := RCC.CFGR and (not (%11)) or %10;

      //Wait for PLL Switch
      while RCC.CFGR and (%11 shl 2) <> (%10 shl 2) do
        ;

      FCPUFrequency := Params.Frequency;

      if FCPUFrequency > 100000000 then
      begin
        //For fast Chips we must divide both PCLK1 and PCLK2
        RCC.CFGR := RCC.CFGR and not (%111 shl 13) or (%100 shl 13);
        RCC.CFGR := RCC.CFGR and not (%111 shl 10) or (%101 shl 10);
      end
      else if FCPUFrequency > SYSCLOCK_MAX div 2 then
      begin
        //For <100 MHZ it is good enough to change PCLK1
        RCC.CFGR := RCC.CFGR and not (%111 shl 13);
        RCC.CFGR := RCC.CFGR and not (%111 shl 10) or (%100 shl 10);
      end
      else
      begin
        // For < 50MHz we can run both busses with full speed
        RCC.CFGR := RCC.CFGR and not (%111 shl 13);
        RCC.CFGR := RCC.CFGR and not (%111 shl 10);
      end;

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

  FractCounter := SysTick.Load and $00FFFFFF;
  Inc(Result, TimerIntResolution - ((FractCounter * FTimerNormalize) div 65536));
end;

{$ENDREGION}
{$REGION 'TMicroGPIO'}

constructor TMicroGPIO.Create;
begin
  inherited;
end;

destructor TMicroGPIO.Destroy;
begin
  inherited;
end;

function TMicroGPIO.GetPinMode(const Pin: TPinIdentifier): TPinMode;
begin
  case (GPIOMem[Pin shr 4]^.MODER shr ((Pin and $0f) shl 1)) and $03 of
    00:     Result := TPinMode.Input;
    01:     Result := TPinMode.Output;
  else
    Result := TPinMode.Unknown;
  end;
end;

function TMicroGPIO.GetPinModeEx(const Pin: TPinIdentifier): TPinModeEx;
begin
  case (GPIOMem[Pin shr 4]^.MODER shr ((Pin and $0f) shl 1)) and $03 of
    00:     Result := TPinModeEx.Input;
    01:     Result := TPinModeEx.Output;
    02:     Result := TPinModeEx.Alternate;
    03:     Result := TPinModeEx.Analog;
  end;
end;

procedure TMicroGPIO.SetPinMode(const Pin: TPinIdentifier; const Value: TPinMode);
begin
  SetPinModeEx(Pin,TPinModeEx(Value));
end;

procedure TMicroGPIO.SetPinModeEx(const Pin: TPinIdentifier; const Value: TPinModeEx);
var
  BitMask,Bit2xMask : longWord;
  Bit,Bit2x,GPIO : byte;
begin
  GPIO := Pin shr 4;
  Bit := Pin and $0f;
  Bit2x := Bit shl 1;
  BitMask := not(1 shl Bit);
  Bit2xMask := not(3 shl Bit2x);

  //First make sure that the GPIO Clock is enabled

  RCC.AHB1ENR := RCC.AHB1ENR or longWord(1 shl GPIO);

  //Now set default Mode with some sane settings

  case Value of
    TPinModeEx.Input     : begin
                             //Enable Input Mode
                             GPIOMem[GPIO]^.MODER := GPIOMem[GPIO]^.MODER and Bit2xMask;
                             //Disable Pullup/Pulldown
                             GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and Bit2xMask;
    end;
    TPinModeEx.Output    : begin
                             //Enable Output Mode
                             GPIOMem[GPIO]^.MODER := GPIOMem[GPIO]^.MODER and Bit2xMask or longWord(%01 shl Bit2x);
                             //Disable Pullup/Pulldown
                             GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and Bit2xMask;
                             //Enable Push/Pull Mode
                             GPIOMem[GPIO]^.OTYPER := GPIOMem[GPIO]^.OTYPER and BitMask;
                             //Enable Fast Speed of GPIO
                             GPIOMem[GPIO]^.OSPEEDR := GPIOMem[GPIO]^.OSPEEDR and Bit2xMask or longWord(%10 shl Bit2x);
    end;

    TPinModeEx.Alternate : begin
                             //Enable Alternate Node
                             GPIOMem[GPIO]^.MODER := GPIOMem[GPIO]^.MODER and Bit2xMask or longWord(%10 shl Bit2x);
                             //Disable Pullup/Pulldown
                             GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and Bit2xMask;
                             //Enable Push/Pull Mode
                             GPIOMem[GPIO]^.OTYPER := GPIOMem[GPIO]^.OTYPER and BitMask;
                             //Enable Fast Speed of GPIO
                             GPIOMem[GPIO]^.OSPEEDR := GPIOMem[GPIO]^.OSPEEDR and Bit2xMask or longWord(%10 shl Bit2x);
    end;
    TPinModeEx.Analog    : begin
                             //Enable Analog Mode
                             GPIOMem[GPIO]^.MODER := GPIOMem[GPIO]^.MODER or longWord(%11 shl Bit2x);
                             //Disable Pullup/Pulldown
                             GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and Bit2xMask;
   end;
  end;
end;

function TMicroGPIO.GetPinValue(const Pin: TPinIdentifier): TPinValue;
begin
  if GPIOMem[Pin shr 4]^.IDR and (1 shl (Pin and $0f)) <> 0 then
    Result := TPinValue.High
  else
    Result := TPinValue.Low;
end;

procedure TMicroGPIO.SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue);
begin
  if Value = TPinValue.High then
    GPIOMem[Pin shr 4]^.BSRR := 1 shl (Pin and $0f)
  else
    GPIOMem[Pin shr 4]^.BSRR := $10000 shl (Pin and $0f);
end;

function TMicroGPIO.GetPinDrive(const Pin: TPinIdentifier): TPinDrive;
begin
  case (GPIOMem[Pin shr 4]^.PUPDR shr ((Pin and $0f) shl 1)) and $03 of
    00: Result := TPinDrive.None;
    01: Result := TPinDrive.PullUp;
    02: Result := TPinDrive.PullDown;
  end;
end;

procedure TMicroGPIO.SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive);
var
  Bit2x,GPIO : byte;
begin
  Bit2x := ((Pin and $0f) shl 1);
  GPIO := Pin shr 4;
  case Value of
    TPinDrive.None : GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and (not (3 shl Bit2x));
    TPinDrive.PullUp : GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and (not (3 shl Bit2x)) or longWord(%01 shl Bit2x);
    TPinDrive.PullDown : GPIOMem[GPIO]^.PUPDR := GPIOMem[GPIO]^.PUPDR and (not (3 shl Bit2x)) or longWord(%10 shl Bit2x);
  end;
end;

function TMicroGPIO.GetPinOpenDrain(const Pin: TPinIdentifier): Boolean; inline;
var
  GPIO : byte;
begin
  GPIO := Pin shr 4;
  Result := (GPIOMem[GPIO]^.OTYPER and (%1 shl (Pin and $0f))) <> 0;
end;

procedure TMicroGPIO.SetPinOpenDrain(const Pin: TPinIdentifier; const Value: Boolean); inline;
var
  GPIO : byte;
begin
  GPIO := Pin shr 4;
  if value = true then
    GPIOMem[GPIO]^.OTYPER := GPIOMem[GPIO]^.OTYPER or longWord(%1 shl (Pin and $0f))
  else
    GPIOMem[GPIO]^.OTYPER := GPIOMem[GPIO]^.OTYPER and (not longWord(%1 shl (Pin and $0f)));
end;

function TMicroGPIO.GetPinFunction(const Pin: TPinIdentifier): Cardinal;
var
  GPIO,Bit : byte;
begin
  GPIO := Pin shr 4;
  Bit := Pin and $0f;
  Result := (GPIOMem[GPIO]^.AFR[Bit shr 3] shr longWord((Bit and $07) shl 2)) and $0f;
end;

procedure TMicroGPIO.SetPinFunction(const Pin: TPinIdentifier; const Value: Cardinal);
var
  Bit4x,GPIO : byte;
begin
  Bit4x := (Pin and $0f) shl 2;
  GPIO := Pin shr 4;
  GPIOMem[GPIO]^.AFR[Bit4x shr 5] := GPIOMem[GPIO]^.AFR[Bit4x shr 5] and (not (%1111 shl (Bit4x and $1f)))
                                     or (Value shl (Bit4x and $1f));
end;

{$ENDREGION}
{$REGION 'TMicroPortSPI'}

constructor TMicroPortSPI.Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
                                 const ApSpiDevice : pTSPI_Registers = nil;
                                 const AMosiPin : TPinIdentifier = TNativePin.None;
                                 const AMisoPin : TPinIdentifier = TNativePin.None;
                                 const ASCKPin : TPinIdentifier = TNativePin.None;
                                 const ACSPin : TPinIdentifier = TNativePin.None);
begin
  inherited Create;

  FSystemCore := ASystemCore;
  FGPIO := AGPIO;
  if ApSPIDevice = nil then
    pSPI := @SPI1
  else
    pSPI := @ApSPIDevice;

  case longWord(pSPI) of
    SPI1_BASE : RCC.APB2ENR := RCC.APB2ENR or (1 shl 12);
    SPI2_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 14);
    SPI3_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 15);
    {$ifdef has_spi4}SPI4_BASE : RCC.APB2ENR := RCC.APB2ENR or (1 shl 13);{$endif}
    {$ifdef has_spi5}SPI5_BASE : RCC.APB2ENR := RCC.APB2ENR or (1 shl 20);{$endif}//TODO set Bits
    {$ifdef has_spi6}SPI6_BASE : RCC.APB2ENR := xRCC.APB2ENR or (1 shl 13);{$endif}//TODO set Bits
  end;

  if FGPIO <> nil then
  begin
    if (ApSpiDevice = nil) and (AMosiPin = TNativePin.None) and (AMisoPin = TNativePin.None) and (ASCkPin = TNativePin.None)
                           and (ACSPin = TNativePin.None) then
    begin
      //Default configuration
      //CS
      FGPIO.PinModeEx[TArduinoPin.D10] := TPinModeEx.Output;
      FNssPin := TArduinoPin.D10;
      FNssMode := TSPINssMode.Software;
      //MOSI
      FGPIO.PinModeEx[TArduinoPin.D11] := TPinModeEx.Alternate;
      FGPIO.PinFunction[TArduinoPin.D11] := 5;
      //MISO
      FGPIO.PinModeEx[TArduinoPin.D12] := TPinModeEx.Alternate;
      FGPIO.PinFunction[TArduinoPin.D12] := 5;
      //SCLK
      FGPIO.PinModeEx[TArduinoPin.D13] := TPinModeEx.Alternate;
      FGPIO.PinFunction[TArduinoPin.D13] := 5;
    end
    else
    begin
      if (ApSpiDevice = nil) or (AMosiPin = TNativePin.None) or (AMisoPin = TNativePin.None) or (ASCkPin = TNativePin.None) then
        writeln('Big error')
      else
      begin
        //Set configuration as defined by user
        //CS
        // For now, only use Software SPI, the Arduino default Buttons (D10,D9) do not Map to Hardware NSS anyway
        FGPIO.PinModeEx[ACSPin] := TPinModeEx.Output;
        FNssPin := ACSPin;
        FNssMode := TSPINssMode.Software;
        //MOSI
        FGPIO.PinModeEx[AMosiPin] := TPinModeEx.Alternate;
        FGPIO.PinFunction[AMosiPin] := 5;
        //MISO
        FGPIO.PinModeEx[AMisoPin] := TPinModeEx.Alternate;
        FGPIO.PinFunction[AMisoPin] := 5;
        //SCLK
        FGPIO.PinModeEx[ASCKPin] := TPinModeEx.Alternate;
        FGPIO.PinFunction[ASCKPin] := 5;
      end;
    end;
  end;

  FDivider := FindDividerValue(DefaultSPIFrequency);
  FBitsPerWord := TSPIBitsPerWordEx.Eight;
  FMode := TSPIModeEx.Mode0;

  ResetPort;
end;

destructor TMicroPortSPI.Destroy;
begin
  inherited;
end;

function TMicroPortSPI.FindDividerValue(const ReqFrequency: Cardinal): Cardinal;
var
  BaseFrequency : Cardinal;
begin
    case longWord(pSPI) of
    SPI1_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;
    SPI2_BASE : BaseFrequency := FSystemCore.GetAPB1PeripheralClockFrequency;
    SPI3_BASE : BaseFrequency := FSystemCore.GetAPB1PeripheralClockFrequency;
    {$ifdef has_spi4}SPI4_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;{$endif}
    {$ifdef has_spi5}SPI5_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;{$endif}
    {$ifdef has_spi6}SPI6_BASE : BaseFrequency := xFSystemCore.GetAPB2PeripheralClockFrequency;{$endif}//TODO set correct values
  end;

  for result := 0 to 7 do
    if ReqFrequency >= (BaseFrequency div word(2 shl result)) then
      break;
end;

procedure TMicroPortSPI.ResetPort;
var
  CR1 : longWord;
begin
  // Set Defaults, all crazy Modes turned off, SPI disabled
  pSpi^.CR1:= 0;

  // Set Defaults, Auto Bitrate off, 1 Stopbit
  pSpi^.CR2:= 0;

  CR1 := 0;

  //BIDIMODE = 0
  //BIDIOE = 0
  //CRCEN = 0
  //CRCNEXT = 0

  //DFF Switch between 8/16 Bits Mode
  if FBitsPerWord = TSPIBitsPerWordEx.Sixteen then
    CR1 := CR1 or (1 shl 11);

  //RXONLY = 0

  //SSM
  if FNssMode = TSPINssMode.Software then
    CR1 := CR1 or (1 shl 9);

  //SSI
  if FNssMode = TSPINssMode.Software then
    CR1 := CR1 or (1 shl 8);

  //LSBFIRST = 0
  //SPE = 0 (Enable SPI later)

  //BR Set SPI Frequency
  CR1 := CR1 or (FDivider shl 3);

  // MSTR Always set Master Mode
  CR1 := CR1 or (1 shl 2);

  // Set correct Polarity and Phase aka as Mode 0-3
  CR1 := CR1 or longWord(FMode);

  pSpi^.CR1 := CR1;

  if FNssMode = TSPINssMode.Software then
    pSpi^.CR2 := (1 shl 2);

  //Disable I2S Mode
  pSpi^.I2SCFGR := 0;

  if FNssMode = TSPINssMode.Software then
    FGPIO.PinValue[FNSSPin] := TPinValue.High;
end;

function TMicroPortSPI.GetFrequency: Cardinal;
var
  BaseFrequency : Cardinal;
begin
  case longWord(pSPI) of
      SPI1_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;
      SPI2_BASE : BaseFrequency := FSystemCore.GetAPB1PeripheralClockFrequency;
      SPI3_BASE : BaseFrequency := FSystemCore.GetAPB1PeripheralClockFrequency;
      {$ifdef has_spi4}SPI4_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;{$endif}
      {$ifdef has_spi5}SPI5_BASE : BaseFrequency := FSystemCore.GetAPB2PeripheralClockFrequency;{$endif}
      {$ifdef has_spi6}SPI6_BASE : BaseFrequency := xFSystemCore.GetAPB2PeripheralClockFrequency;{$endif}//TODO set correct values
  end;
  Result := BaseFrequency shr (FDivider+1);
end;

procedure TMicroPortSPI.SetFrequency(const Value: Cardinal);
begin
  if GetFrequency <> Value then
  begin
    FDivider := FindDividerValue(Value);
    ResetPort;
  end;
end;

function TMicroPortSPI.GetBitsPerWord: TBitsPerWord;
begin
  Result := longWord(FBitsPerWord);
end;

procedure TMicroPortSPI.SetBitsPerWord(const Value: TBitsPerWord);
begin
  if longWord(FBitsPerWord) <> Value then
  begin
    if Value = 8 then
    begin
      FBitsPerWord := TSPIBitsPerWordEx.Eight;
      ResetPort;
    end;
    if Value = 16 then
    begin
      FBitsPerWord := TSPIBitsPerWordEx.Sixteen;
      ResetPort;
    end;
  end;
end;

function TMicroPortSPI.GetBitsPerWordEx: TSPIBitsPerWordEx;
begin
  Result := FBitsPerWord;
end;

procedure TMicroPortSPI.SetBitsPerWordEx(const Value: TSPIBitsPerWordEx);
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    ResetPort;
  end;
end;

function TMicroPortSPI.GetMode: TSPIMode;
begin
  Result := longWord(FMode);
end;

procedure TMicroPortSPI.SetMode(const Value: TSPIMode);
begin
  if longword(FMode) <> Value then
  begin
    case value of
        0: FMode := TSPIModeEx.Mode0;
        1: FMode := TSPIModeEx.Mode1;
        2: FMode := TSPIModeEx.Mode2;
        3: FMode := TSPIModeEx.Mode3;
    else
      FMode := TSPIModeEx.Mode0;
    end;
    ResetPort;
  end;
end;

function TMicroPortSPI.GetModeEx: TSPIModeEx;
begin
  Result := FMode;
end;

procedure TMicroPortSPI.SetModeEx(const Value: TSPIModeEx);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    ResetPort;
  end;
end;

function TMicroPortSPI.GetNssMode: TSPINssMode;
begin
  Result := FNssMode;
end;

procedure TMicroPortSPI.SetNssMode(const Value: TSPINssMode);
begin
  if FNssMode <> Value then
  begin
    FNssMode := Value;
    ResetPort;
  end;
end;

function TMicroPortSPI.TransferWord(const Value : word): word;
begin
  // Enable SPI, this also sets NSS Pin Low in Hardware Mode
  pSPI^.CR1 := pSPI^.CR1 or (1 shl 6);

  //wait for TXE to go high (no more data to shift out)
  while pSPI^.SR and (1 shl 1) = 0 do
    ;

  //read data from rx buffer if available and discard it
  if pSPI^.SR and (1 shl 0) = 1 then
    Result := pSPI^.DR;

  //Take the NSS Pin Low in software Mode (start transfer)
  if FNssMode = TSPINssMode.Software then
    FGPIO.PinValue[FNSSPin] := TPinValue.Low;

  //Put Data into Send Register
  pSPI^.DR := Value;

  // RXNE Wait until data is completely shifted in
  while pSPI^.SR and (1 shl 0) = 0 do
    ;

  // Now read the result back from Data Register
  Result := pSPI^.DR;

  // Take NSS High again in Software Mode (end of Transfer)
  if FNssMode = TSPINssMode.Software then
    FGPIO.PinValue[FNSSPin] := TPinValue.High;

  // Disable SPI, this also sets NSS Pin High in Hardware Mode
  pSPI^.CR1 := pSPI^.CR1 and (not (1 shl 6));
end;

function TMicroPortSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  ReadBytes,WriteBytes : cardinal;
begin
  ReadBytes := 0;
  WriteBytes := 0;

  // Enable SPI, this also sets NSS Pin Low in Hardware Mode
  pSPI^.CR1 := pSPI^.CR1 or (1 shl 6);

  //wait for TXE to go high (no more data to shift out)
  while pSPI^.SR and (1 shl 1) = 0 do
    ;

  //read data from rx buffer if available and discard it
  if pSPI^.SR and (1 shl 0) = 1 then
    Result := pSPI^.DR;

  while ((ReadBuffer <> nil) and (ReadBytes < BufferSize)) or ((WriteBuffer <> nil) and (WriteBytes < BufferSize)) do
  begin
    if FNssMode = TSPINssMode.Software then
      FGPIO.PinValue[FNSSPin] := TPinValue.low;
    if WriteBuffer <> nil then
    begin
      if FBitsPerWord = TSPIBitsPerWordEx.Sixteen then
      begin
        pSPI^.DR := PWord(PByte(WriteBuffer) + WriteBytes)^;
        Inc(WriteBytes);
        Inc(WriteBytes);
      end
      else
      begin
        pSPI^.DR := PByte(PByte(WriteBuffer) + WriteBytes)^;
        Inc(WriteBytes);
      end;
    end
    else
      pSPI^.DR := $ff; //We need to send dummy data to be able to receive

    // RXNE Wait until data is completely shifted in
    while pSPI^.SR and (1 shl 0) = 0 do
      ;

    Result := pSPI^.DR;

    if ReadBuffer <> nil then
    begin // Get data from Read FIFOs.
      if (ReadBytes < BufferSize) and ((ReadBuffer <> WriteBuffer) or (ReadBytes < WriteBytes)) then
      begin
        if FBitsPerWord = TSPIBitsPerWordEx.Sixteen then
        begin
          PWord(PByte(ReadBuffer) + ReadBytes)^ := result;
          Inc(WriteBytes);
          Inc(WriteBytes);
        end
        else
        begin
          PByte(PByte(ReadBuffer) + ReadBytes)^ := result;
          Inc(ReadBytes);
        end;
      end;
    end;
    if FNssMode = TSPINssMode.Software then
      FGPIO.PinValue[FNSSPin] := TPinValue.High;
  end;

  // Disable SPI, this also sets NSS Pin High in Hardware Mode
  pSPI^.CR1 := pSPI^.CR1 and (not (1 shl 6));
  if WriteBytes > ReadBytes then
    Result := WriteBytes
  else
    Result := ReadBytes;
end;

  (*
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
end;   *)

{$ENDREGION}

{$REGION 'TMicroUART'}

constructor TMicroUART.Create(const ASystemCore: TMicroSystemCore; const AGPIO: TMicroGPIO;
                              const ApUSART: pTUSART_Registers=nil;
                              const ARxPin : TPinIdentifier = TNativePin.None;
                              const ATxPin : TPinIdentifier = TNativePin.None);
begin
  inherited Create(ASystemCore);

  FGPIO := AGPIO;
  FSystemCore := ASystemCore;

  if ApUSART = nil then
    pUSART := @USART2;

  case longWord(pUSART) of
    USART1_BASE : RCC.APB2ENR := RCC.APB2ENR or (1 shl 4);
    USART2_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 17);
    {$ifdef has_usart3}USART3_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 18);{$endif}
    {$ifdef has_usart4}UART4_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 19);{$endif}
    {$ifdef has_usart5}UART5_BASE : RCC.APB1ENR := RCC.APB1ENR or (1 shl 20);{$endif}
    {$ifdef has_usart6}USART6_BASE : RCC.APB2ENR := RCC.APB2ENR or (1 shl 5);{$endif}
    {$ifdef has_usart7}USART7_BASE : xRCC.APB2ENR := xRCC.APB2ENR or %0000000000000000;{$endif}//TODO set Bits
    {$ifdef has_usart8}USART8_BASE : xRCC.APB2ENR := xRCC.APB2ENR or %0000000000000000;{$endif}//TODO set Bits
  end;

  if (ApUSART = nil) and (ARxPin = TNativePin.None) and (ATxPin = TNativePin.None) then
  begin
    FGPIO.PinModeEx[TArduinoPin.D0] := TPinModeEx.Alternate;
    FGPIO.PinFunction[TArduinoPin.D0] := 7;
    FGPIO.PinModeEx[TArduinoPin.D1] := TPinModeEx.Alternate;
    FGPIO.PinFunction[TArduinoPin.D1] := 7;
  end;

  if (pUSART <> nil) and ((ARxPin = TNativePin.None) or (ATxPin = TNativePin.None)) then
    writeln('Grande Catastrophe')
  else
  begin
    FGPIO.PinModeEx[ARxPin] := TPinModeEx.Alternate;
    FGPIO.PinFunction[ARxPin] := 7;

    FGPIO.PinModeEx[ATxPin] := TPinModeEx.Alternate;
    FGPIO.PinFunction[ATxPin] := 7;
  end;

  FBaudRate := DefaultUARTBaudRate;
  FBitsPerWord := 8;
  FParity := TParity.None;
  FStopBits := TStopBits.One;

  ResetPort;
end;

destructor TMicroUART.Destroy;
begin
//  LPC_UART.IIR_FCR := $00;

  case longWord(pUSART) of
    USART1_BASE : RCC.APB2ENR := RCC.APB2ENR and not (1 shl 4);
    USART2_BASE : RCC.APB1ENR := RCC.APB1ENR and not (1 shl 17);
    {$ifdef has_usart3}USART3_BASE : RCC.APB1ENR := RCC.APB1ENR and not (1 shl 18);{$endif}
    {$ifdef has_usart4}UART4_BASE : RCC.APB1ENR := RCC.APB1ENR and not (1 shl 19);{$endif}
    {$ifdef has_usart5}UART5_BASE : RCC.APB1ENR := RCC.APB1ENR and not (1 shl 20);{$endif}
    {$ifdef has_usart6}USART6_BASE : RCC.APB2ENR := RCC.APB2ENR and not (1 shl 5);{$endif}
    {$ifdef has_usart7}USART7_BASE : xRCC.APB1ENR := RCC.APB1ENR and not (1 shl xx);{$endif}//TODO set Bits
    {$ifdef has_usart8}USART8_BASE : xRCC.APB1ENR := RCC.APB1ENR and not (1 shl xx);{$endif}//TODO set Bits
  end;

  inherited;
end;

procedure TMicroUART.ResetPort;
begin
  // First, load Reset Value, this also turns off the USART
  // Create the basic config for all n81 use cases
  pUSART^.CR1:= 0;

  // Set Defaults, Auto Bitrate off, 1 Stopbit
  pUSART^.CR2:= 0;

  // Set Defaults not RTS/CTS
  pUSART^.CR3:= 0;

  setBaudRate(BaudRate);
  // UE Enable USART
  pUSART^.CR1 := pUSART^.CR1 or (1 shl 13);
  // RE TE Enable both receiver and sender
  pUSART^.CR1 := pUSART^.CR1 or (1 shl 2) or (1 shl 3);
end;

function TMicroUART.GetBaudRate: Cardinal;
begin
  Result := FBaudRate;
end;

procedure TMicroUART.SetBaudRate(const Value: Cardinal);
var
  ClockFreq,Mantissa,Fraction : longWord;
  reactivate : boolean = false;
begin
  if FBaudRate <> Value then
  begin
    // set Baudrate
    // UE disable Serial interface
    if pUSART^.CR1 and (1 shl 13) = 1 then
    begin
      pUSART^.CR1 := pUSART^.CR1 and not(1 shl 13);
      reactivate := true;
    end;
    case longWord(pUSART) of
      USART1_BASE : ClockFreq := FSystemCoreEx.GetAPB2PeripheralClockFrequency;
      USART2_BASE : ClockFreq := FSystemCoreEx.GetAPB1PeripheralClockFrequency;
      {$ifdef has_usart3}USART3_BASE : ClockFreq := FSystemCoreEx.GetAPB1PeripheralClockFrequency;{$endif}
      {$ifdef has_usart4}UART4_BASE :  ClockFreq := FSystemCoreEx.GetAPB1PeripheralClockFrequency;{$endif}
      {$ifdef has_usart5}UART5_BASE :  ClockFreq := FSystemCoreEx.GetAPB1PeripheralClockFrequency;{$endif}
      {$ifdef has_usart6}USART6_BASE : ClockFreq := FSystemCoreEx.GetAPB1PeripheralClockFrequency;{$endif}
      {$ifdef has_usart7}USART7_BASE : xClockFreq := GetAPB1PeripheralClockFrequency;{$endif}//TODO set Bits
      {$ifdef has_usart8}USART8_BASE : xClockFreq := GetAPB1PeripheralClockFrequency;{$endif}//TODO set Bits
    end;
    //OVER8
    if pUSART^.CR1 and not(1 shl 15) = 0 then
    begin
      Mantissa := ClockFreq div (Baudrate shl 4);
      Fraction := longWord(longWord(longWord(ClockFreq - Mantissa*16*BaudRate)*100 div longWord(Baudrate shl 4)) shl 4+50) div 100;
    end
    else
    begin
      Mantissa := ((ClockFreq*25) div (2*BaudRate)) div 100;
      Fraction := (longWord(((((ClockFreq*25) div (2*Baudrate)) - Mantissa*100)*16+50) div 100) and $0f) div 2;
    end;
    pUSART^.BRR := Mantissa shl 4 or Fraction;
    if reactivate = true then
      pUSART^.CR1 := pUSART^.CR1 or (1 shl 13);
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
  //LPC_UART.IIR_FCR := $07;
end;

function TMicroUART.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := 0;

  while (Result < BufferSize) do
  begin
    while pUSART^.SR and (1 shl 5) = 0 do
      ;
    if GetBitsPerWord <= 8 then
      PByte(PByte(Buffer) + Result)^ := pUSART^.DR
    else
    begin
      PWord(PByte(Buffer) + Result)^ := pUSART^.DR;
      inc(Result);
    end;
    Inc(Result);
  end;
end;

function TMicroUART.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := 0;

  while Result < BufferSize do
  begin
    //TXE
    while pUSART^.SR and (1 shl 7) = 0 do
      ;
    if GetBitsPerWord <= 8 then
      pUSART^.DR := PByte(pByte(Buffer) + Result)^
    else
    begin
      inc(Result);
      pUSART^.DR := pword(pword(Buffer) + Result)^
    end;
    Inc(Result);
  end;
  //TXE
  while pUSART^.SR and (1 shl 7) = 0 do
    ;
end;

{$ENDREGION}

end.


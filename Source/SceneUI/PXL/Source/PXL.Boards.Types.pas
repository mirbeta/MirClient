unit PXL.Boards.Types;
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
{< Basic types and components commonly used on microcontrollers and compact singleboard devices. }
interface

{$INCLUDE PXL.MicroConfig.inc}

uses
  PXL.TypeDef;

type
  { Special unsigned integer that is used to represent ticks in microseconds. }
  TTickCounter = {$IFDEF EMBEDDED} Cardinal {$ELSE} UInt64 {$ENDIF};
  TMicroseconds = TTickCounter;
  TMilliseconds = TTickCounter;

  { Unique number that identifies each individual pin on the chip. The actual interpretation and meaning of this value
    may vary on different implementations. }
  TPinIdentifier = Cardinal;

  { I/O mode typically used in GPIO pins. }
  TPinMode = (
    { Pin set for input / high impedance }
    Input = 0,

    { Pin set for output }
    Output = 1,

    { Pin set for alternate Pinfunction }
    Unknown = 99);

  { Digital value of the pin. }
  TPinValue = (
    { Low (0) or zero voltage. }
    Low = 0,

    { High (1) or full voltage. }
    High = 1);

  { Drive mode that is used in GPIO pins. }
  TPinDrive = (
    { Strong low and high or high-impedance (no pull-up or pull-down resistor). }
    None,

    { Resistive high, strong low (pull-up resistor). }
    PullUp,

    { Resistive low, strong high (pull-down resistor). }
    PullDown);

  { Transition type of digital signal on a certain pin. }
  TSignalEdge = (
    { Depending on implementation this value can mean either that transition is unknown, disabled or using both falling
      and raising edges. }
    None,

    { Falling edge, where signal goes from high (1) to low (0). }
    Falling,

    { Raising edge, where signal goes from low (0) to high (1). }
    Raising);

  { System core of the board, which provides high-performance utility functions for accurate timing and delays. }
  TCustomSystemCore = class abstract
  public
    { Returns the current value of system timer, in microseconds. }
    function GetTickCount: TMicroseconds; virtual; abstract;

    { Calculates the difference between two system timer values with proper handling of overflows. }
    function TicksInBetween(const InitTicks, EndTicks: TMicroseconds): TMicroseconds;

    { Waits the specified amount of microseconds accurately by continuously polling the timer.
      This is useful for accurate timing but may result in high CPU usage. }
    procedure BusyWait(const Microseconds: TMicroseconds);

    { Delays the execution for the specified amount of microseconds. }
    procedure MicroDelay(const Microseconds: TMicroseconds); virtual;

    { Delays the execution for the specified amount of milliseconds. CPU is put to sleep when milliseconds > 10}
    procedure Delay(const Milliseconds: TMilliseconds); virtual;
  end;

  { Abstract GPIO (General Purpose Input / Output) manager. }
  TCustomGPIO = class abstract
  protected
    { Returns current mode for the specified pin number. }
    function GetPinMode(const Pin: TPinIdentifier): TPinMode; virtual; abstract;

    { Changes mode for the specified pin number, as long as new mode is different than the current one. }
    procedure SetPinMode(const Pin: TPinIdentifier; const Value: TPinMode); virtual; abstract;

    { Returns current value for the specified pin number, both for input and output modes. }
    function GetPinValue(const Pin: TPinIdentifier): TPinValue; virtual; abstract;

    { Changes value for the specified pin number, as long as new value is different than the current one. }
    procedure SetPinValue(const Pin: TPinIdentifier; const Value: TPinValue); virtual; abstract;

    { Returns current drive mode (pull-up/pull-down) for the specified pin number. }
    function GetPinDrive(const Pin: TPinIdentifier): TPinDrive; virtual; abstract;

    { Specifies new drive mode (pull-up/pull-down) for the specified pin number. }
    procedure SetPinDrive(const Pin: TPinIdentifier; const Value: TPinDrive); virtual; abstract;
  public
    { Configures the specified pin for output and with the specified value, typically used for configuring
      multiplexers. }
    procedure SetMux(const Pin: TPinIdentifier; const Value: TPinValue);

    { Currently set mode for the specified pin number. }
    property PinMode[const Pin: TPinIdentifier]: TPinMode read GetPinMode write SetPinMode;

    { Currently set signal value for the specified pin number. }
    property PinValue[const Pin: TPinIdentifier]: TPinValue read GetPinValue write SetPinValue;

    { Currently set drive mode (pull-up/pull-down) for the specified pin number. }
    property PinDrive[const Pin: TPinIdentifier]: TPinDrive read GetPinDrive write SetPinDrive;
  end;

  { Generic channel number, which can actually represent a physical pin. }
  TPinChannel = TPinIdentifier;

  { Abstract PWM (Pulse-Width Modulation) manager. }
  TCustomPWM = class abstract
  protected
    { Returns @True when the specified channel is configured for PWM output and @False otherwise. }
    function GetEnabled(const Channnel: TPinChannel): Boolean; virtual; abstract;

    { Changes status of PWM output on the specified channel. }
    procedure SetEnabled(const Channel: TPinChannel; const Value: Boolean); virtual; abstract;

    { Returns current period (nanoseconds) set for the specified channel. }
    function GetPeriod(const Channel: TPinChannel): Cardinal; virtual; abstract;

    { Changes period (nanoseconds) for the specified channel. }
    procedure SetPeriod(const Channel: TPinChannel; const Value: Cardinal); virtual; abstract;

    { Returns current duty cycle (nanoseconds, in relation to period) set for the specified channel. }
    function GetDutyCycle(const Channel: TPinChannel): Cardinal; virtual; abstract;

    { Changes duty cycle (nanoseconds, in relation to period) for the specified channel. }
    procedure SetDutyCycle(const Channel: TPinChannel; const Value: Cardinal); virtual; abstract;
  public
    { Starts Pulse-Width Modulation on specified channel with the desired frequency (in Hz) and duty cycle.
        @param(Pin Physical channel number on which to enable PWM.)
        @param(Frequency The desired frequency (in Hz).)
        @param(DutyCycle The desired duty cycle as fixed-point between 0 and 65535, each corresponding to 0% and
          100% respectively.) }
    procedure Start(const Channel: TPinChannel; const Frequency, DutyCycle: Cardinal); virtual;

    { Stops PWM on the specified Pin. }
    procedure Stop(const Channel: TPinChannel); virtual;

    { Determines whether the specified Pin is configured for PWM output. }
    property Enabled[const Channel: TPinChannel]: Boolean read GetEnabled write SetEnabled;

    { Determines PWM period in nanoseconds (e.g. 1000000 ns period would be 1 ms or 100 hz). }
    property Period[const Channel: TPinChannel]: Cardinal read GetPeriod write SetPeriod;

    { Determines PWM duty cycle in nanoseconds in respect to period (e.g. 500000 ns for period of 1000000 ns would
      define a 50% of duty cycle). }
    property DutyCycle[const Channel: TPinChannel]: Cardinal read GetDutyCycle write SetDutyCycle;
  end;

  { Abstract ADC (Analog-to-Digital Converter) manager. }
  TCustomADC = class abstract
  protected
    { Returns raw digital value for the given channel number. }
    function GetRawValue(const Channel: TPinChannel): Cardinal; virtual; abstract;
  public
    { Raw value on the specified analog input channel that depends on particular device's resolution. }
    property RawValue[const Channel: TPinChannel]: Cardinal read GetRawValue;
  end;

  { Abstract communication manager can be used for reading and writing data. }
  TCustomDataPort = class abstract
  public
    { Reads specified number of bytes to buffer and returns actual number of bytes read. }
    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; virtual; abstract;

    { Writes specified number of bytes from buffer and returns actual number of bytes written. }
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; virtual; abstract;
  end;

  { Abstract I2C (Inter-Integrated Circuit) communication manager. }
  TCustomPortI2C = class abstract(TCustomDataPort)
  public
    { Specifies new device address to which the communication will be made. }
    procedure SetAddress(const Address: Cardinal); virtual; abstract;

    { Reads a single byte from current address. Returns @True when the operation was successful and @False otherwise. }
    function ReadByte(out Value: Byte): Boolean; virtual;

    { Write a single byte to current address. Returns @True when the operation was successful and @False otherwise. }
    function WriteByte(const Value: Byte): Boolean; virtual;

    { Write one or more bytes to current address. Returns @True when the operation was successful and @False otherwise. }
    function WriteBytes(const Values: array of Byte): Boolean;

    { Writes command to current address and reads a single byte from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadByteData(const Command: Byte; out Value: Byte): Boolean; virtual;

    { Writes command and a single byte of data to current address. Returns @True when the operation was successful and
      @False otherwise. }
    function WriteByteData(const Command, Value: Byte): Boolean; virtual;

    { Writes command to current address and reads a word (16-bit unsigned) from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadWordData(const Command: Byte; out Value: Word): Boolean; virtual;

    { Writes command and a word (16-bit unsigned) of data to current address. Returns @True when the operation was
      successful and @False otherwise. }
    function WriteWordData(const Command: Byte; const Value: Word): Boolean; virtual;

    { Writes command to current address and reads specified block of data from it. Although this varies depending on
      implementation, but typically stop bit is given at the end of the whole transmission (so there is no stop bit
      between command and read operation). Returns @True when the operation was successful and @False otherwise. }
    function ReadBlockData(const Command: Byte; const Buffer: Pointer;
      const BufferSize: Cardinal): Cardinal; virtual; abstract;

    { Writes command and specified block of data to current address. Returns @True when the operation was
      successful and @False otherwise. }
    function WriteBlockData(const Command: Byte; const Buffer: Pointer;
      const BufferSize: Cardinal): Cardinal; virtual; abstract;
  end;

  { Chip Select operation mode. }
  TChipSelectMode = (
    { Chip Select pin shuld not be managed. }
    Disabled,

    { Chip Select should be helf active low during operations. This is the default mode. }
    ActiveLow,

    { Chip Select should be helf active high during operations. }
    ActiveHigh);

  { Number of bits each data packet (or "word) occupies. }
  TBitsPerWord = Cardinal;

  { SPI operation mode. The actual interpretation of this value depends on each platform and implementation. }
  TSPIMode = Cardinal;

  { Abstract SPI (Serial Peripheral Interface) communication manager. }
  TCustomPortSPI = class abstract(TCustomDataPort)
  protected
    { Current Chip Select operation mode. }
    FChipSelectMode: TChipSelectMode;

    { Returns current operating frequency. }
    function GetFrequency: Cardinal; virtual; abstract;

    { Changes current operating frequency. }
    procedure SetFrequency(const Value: Cardinal); virtual; abstract;

    { Returns current number of bits that each word occupies. }
    function GetBitsPerWord: TBitsPerWord; virtual; abstract;

    { Changes current number of bits each word occupies. }
    procedure SetBitsPerWord(const Value: TBitsPerWord); virtual; abstract;

    { Returns currently active SPI mode. }
    function GetMode: TSPIMode; virtual; abstract;

    { Changes current SPI mode to the specified value. }
    procedure SetMode(const Value: TSPIMode); virtual; abstract;
  public
    { Creates instance of SPI port with the specified Chip Select (CS) mode. }
    constructor Create(const AChipSelectMode: TChipSelectMode = TChipSelectMode.ActiveLow);

    { Reads specified number of bytes to buffer and returns actual number of bytes read. }
    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    { Writes specified number of bytes from buffer and returns actual number of bytes written. }
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    { Transfers data through SPI port asynchronously - that is, reading and writing at the same time.
        @param(ReadBuffer Pointer to data buffer where the data will be read from. If this parameter is set to @nil,
          then no reading will be done.)
        @param(WriteBuffer Pointer to data buffer where the data will be written to. If this parameter is set to @nil,
          then no writing will be done.)
        @param(BufferSize The size of read and write buffers in bytes.)
        @returns(Number of bytes that were actually transferred.) }
    function Transfer(const ReadBuffer, WriteBuffer: Pointer;
      const BufferSize: Cardinal): Cardinal; overload; virtual; abstract;

    { Transfers data through SPI port asynchronously - that is, reading and writing at the same time.
        @param(Buffer Pointer to data buffer where the data will be read from and at the same time written to,
          overwriting its contents.)
        @param(BufferSize The size of buffer in bytes.)
        @returns(Number of bytes that were actually transferred.) }
    function Transfer(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; overload; inline;

    { SPI operating frequency in Hz. }
    property Frequency: Cardinal read GetFrequency write SetFrequency;

    { Number of bits each word occupies, typically either 8 or 16 depending on hardware and support. }
    property BitsPerWord: TBitsPerWord read GetBitsPerWord write SetBitsPerWord;

    { Mode of SPI operation, including Clock Polarity (CPOL) and Clock Edge (CPCHA). The actual meaning of this
      parameter depends on implementation and should be consulted from corresponding documentation. }
    property Mode: TSPIMode read GetMode write SetMode;

    { Chip Select operation mode. }
    property ChipSelectMode: TChipSelectMode read FChipSelectMode;
  end;

  { Parity bit type used for transceiving binary strings. }
  TParity = (
    { No parity bit. }
    None = 0,

    { Odd parity bit. }
    Odd = 1,

    { Even parity bit. }
    Even = 2);

  { Number of stop bits used for transceiving binary strings. }
  TStopBits = (
    { One stop bit. }
    One = 0,

    { One and "half" stop bits. }
    OneDotFive = 1,

    { Two stop bits. }
    Two = 2);

  { Abstract UART (Universal Asynchronous Receiver / Transmitter) communication manager. }
  TCustomPortUART = class abstract(TCustomDataPort)
  protected const
    { Default buffer size used when reading and writing strings. }
    StringBufferSize = {$IFDEF EMBEDDED} 8 {$ELSE} 32 {$ENDIF}; // characters

    { Default sleep time while waiting during transmission between multiple attempts. }
    InterimSleepTime = {$IFDEF EMBEDDED} 2000 {$ELSE} 10000 {$ENDIF}; // us
  protected
    { Reference to @link(TCustomSystemCore) currently being used by UART port. }
    FSystemCore: TCustomSystemCore;

    { Returns currently set baud rate. }
    function GetBaudRate: Cardinal; virtual; abstract;

    { Sets new baud rate. }
    procedure SetBaudRate(const Value: Cardinal); virtual; abstract;

    { Returns current number of bits per word. }
    function GetBitsPerWord: TBitsPerWord; virtual; abstract;

    { Sets new number of bits per word. }
    procedure SetBitsPerWord(const Value: TBitsPerWord); virtual; abstract;

    { Returns current parity check type. }
    function GetParity: TParity; virtual; abstract;

    { Sets new parity check type. }
    procedure SetParity(const Value: TParity); virtual; abstract;

    { Returns current number of stop bits. }
    function GetStopBits: TStopBits; virtual; abstract;

    { Sets new number of stop bits. }
    procedure SetStopBits(const Value: TStopBits); virtual; abstract;
  public
    { Creates UART (serial) port instance associated with a specific instace of  @italic(TCustomSystemCore). Note that
      @italic(ASystemCore) parameter is required as it is used for timeout calculations. }
    constructor Create(const ASystemCore: TCustomSystemCore);

    { Flushes UART (serial) port read and write FIFO buffers. }
    procedure Flush; virtual; abstract;

    { Reads data buffer from UART (serial) port.
        @param(Buffer Pointer to data buffer where the data will be written to.)
        @param(BufferSize Number of bytes to read.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to read the buffer. If this parameter is
          set to zero, then the function will block indefinitely, attempting to read until the specified number of
          bytes have been read.)
        @returns(Number of bytes that were actually read.) }
    function ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal; virtual;

    { Writes data buffer to UART (serial) port.
        @param(Buffer Pointer to data buffer where the data will be read from.)
        @param(BufferSize Number of bytes to write.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to write the buffer. If this parameter
          is set to zero, then the function will block indefinitely, attempting to write until the specified number of
          bytes have been written.)
        @returns(Number of bytes that were actually written.) }
    function WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal; virtual;

    { Attempts to read a byte from UART (serial) port. @code(Timeout) defines maximum time (in milliseconds) to wait
      while attempting to do so; if this parameter is set to zero, then the function will block indefinitely until the
      byte has been read. @True is returned when the operation was successful and @False when the byte could not be
      read. }
    function ReadByte(out Value: Byte; const Timeout: Cardinal = 0): Boolean; inline;

    { Attempts to write a byte to UART (serial) port. @code(Timeout) defines maximum time (in milliseconds) to wait
      while attempting to do so; if this parameter is set to zero, then the function will block indefinitely until the
      byte has been written. @True is returned when the operation was successful and @False when the byte could not be
      written. }
    function WriteByte(const Value: Byte; const Timeout: Cardinal = 0): Boolean; inline;

    { Attempts to write multiple bytes to UART (serial) port. @code(Timeout) defines maximum time (in milliseconds) to
      wait while attempting to do so; if this parameter is set to zero, then the function will block indefinitely,
      attempting to write until the specified bytes have been written. @True is returned when the operation was
      successful and @False when not all bytes could be written. }
    function WriteBytes(const Values: array of Byte; const Timeout: Cardinal = 0): Boolean;

    { Reads string from UART (serial) port.
        @param(Text String that will hold the incoming data.)
        @param(MaxCharacters Maximum number of characters to read. Once this number of characters has been read, the
          function immediately returns, even if there is more data to read. When this parameter is set to zero, then
          the function will continue to read the data, depending on value of @code(Timeout).)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to read the buffer. If this parameter
          is set to zero, then the function will read only as much data as fits in readable FIFO buffers (or fail when
          such buffers are not supported).)
        @returns(Number of bytes that were actually read.) }
    function ReadString(out Text: StdString; const MaxCharacters: Cardinal = 0;
      const Timeout: Cardinal = 0): Boolean;

    { Writes string to UART (serial) port.
        @param(Text String that should be sent.)
        @param(Timeout Maximum time (in milliseconds) to wait while attempting to write the buffer. If this parameter
          is set to zero, then the function will write only what fits in writable FIFO buffers (or fail when such
          buffers are not supported).)
        @returns(Number of bytes that were actually read.) }
    function WriteString(const Text: StdString; const Timeout: Cardinal = 0): Boolean;

    { Reference to @link(TCustomSystemCore) associated with this UART port instance. }
    property SystemCore: TCustomSystemCore read FSystemCore;

    { Currently used baud rate in terms of bits per second. Note that to calculate the actual speed of transmission,
      it is necessary to take into account start and stop bits among other things; for typical situations, the actual
      transmission speed may be something like BaudRate / 10 bytes per second or less. }
    property BaudRate: Cardinal read GetBaudRate write SetBaudRate;

    { Number of bits per message (or "word") used in the transmission. Typically this equals to 8 bits, which means one
      byte is sent at a time. Accepted values usually range between 5 and 8 bits. Lower values, especially when
      combined with parity checks may result in less and/or easier to detect errors. }
    property BitsPerWord: TBitsPerWord read GetBitsPerWord write SetBitsPerWord;

    { Number of parity (error detection) bits used in the transmission. This means that additional bits will be added
      to the tail of transmission that indicate whether the number of ones in the message is odd or even; if the
      received number of bits does not match this hint, the message will be discarded. }
    property Parity: TParity read GetParity write SetParity;

    { Number of stop bits used in the transmission. Although normally only one stop bit is necessary, it may be
      difficult to detect in hardware when transmission channel is noisy; therefore, additional stop bits may help
      alleviate the problem. }
    property StopBits: TStopBits read GetStopBits write SetStopBits;
  end;

const
  { Default baud rate at which UART controller operates. }
  DefaultUARTBaudRate = 115200;

  { Default frequency at which SPI controller operates. }
  DefaultSPIFrequency = 8000000;

  { Maximum number of bytes that can be reliably sent through SPI protocol in each read/write call. }
  MaxSPITransferSize = {$IFDEF EMBEDDED} 256 {$ELSE} 4096 {$ENDIF};

  { Maximum number of bytes that can be reliably sent through I2C protocol in each read/write call. }
  MaxI2CTransferSize = {$IFDEF EMBEDDED} 8 {$ELSE} 32 {$ENDIF};

  { Constant value indicating that the specified pin is not connected and should not be used. }
  PinDisabled = TPinIdentifier(-1);

  { @exclude } ExceptionClassNameSeparator = ': ';

implementation

{$REGION 'TCustomSystemCore'}

function TCustomSystemCore.TicksInBetween(const InitTicks, EndTicks: TMicroseconds): TMicroseconds;
begin
  Result := EndTicks - InitTicks;
  if not Result < Result then
    Result := not Result;
end;

procedure TCustomSystemCore.BusyWait(const Microseconds: TMicroseconds);
var
  StartTicks: TMicroseconds;
begin
  StartTicks := GetTickCount;
  while TicksInBetween(StartTicks, GetTickCount) < Microseconds do ;
end;

procedure TCustomSystemCore.MicroDelay(const Microseconds: TMicroseconds);
begin
  BusyWait(Microseconds);
end;

procedure TCustomSystemCore.Delay(const MilliSeconds: TMilliseconds);
var
  StartTicks : TMicroseconds;
begin
  {$ifdef EMBEDDED}
  if MilliSeconds < 10 then
    BusyWait(MilliSeconds*1000)
  else
  begin
    StartTicks := GetTickCount;
    while TicksInBetween(StartTicks, GetTickCount) < Milliseconds*1000 do
    asm
      wfi // wait for interrupt
    end;
  end;
  {$else}
  BusyWait(TMicroSeconds(MilliSeconds*1000));
  {$endif}
end;


{$ENDREGION}
{$REGION 'TCustomGPIO'}

procedure TCustomGPIO.SetMux(const Pin: TPinIdentifier; const Value: TPinValue);
begin
  SetPinMode(Pin, TPinMode.Output);
  SetPinValue(Pin, Value);
end;

{$ENDREGION}
{$REGION 'TCustomPWM'}

procedure TCustomPWM.Start(const Channel: TPinChannel; const Frequency, DutyCycle: Cardinal);
var
  DesiredPeriod: Cardinal;
begin
  SetEnabled(Channel, False);

  DesiredPeriod := UInt64(1000000000) div Frequency;

  SetPeriod(Channel, DesiredPeriod);
  SetDutyCycle(Channel, (UInt64(DesiredPeriod) * DutyCycle) div 65536);

  SetEnabled(Channel, True);
end;

procedure TCustomPWM.Stop(const Channel: TPinChannel);
begin
  SetEnabled(Channel, False);
end;

{$ENDREGION}
{$REGION 'TCustomPortI2C'}

function TCustomPortI2C.ReadByte(out Value: Byte): Boolean;
begin
  Result := Read(@Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteByte(const Value: Byte): Boolean;
begin
  Result := Write(@Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteBytes(const Values: array of Byte): Boolean;
begin
  if Length(Values) > 0 then
    Result := Write(@Values[0], Length(Values)) = Cardinal(Length(Values))
  else
    Result := False;
end;

function TCustomPortI2C.ReadByteData(const Command: Byte; out Value: Byte): Boolean;
begin
  Result := ReadBlockData(Command, @Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.WriteByteData(const Command, Value: Byte): Boolean;
begin
  Result := WriteBlockData(Command, @Value, SizeOf(Byte)) = SizeOf(Byte);
end;

function TCustomPortI2C.ReadWordData(const Command: Byte; out Value: Word): Boolean;
begin
  Result := ReadBlockData(Command, @Value, SizeOf(Word)) = SizeOf(Word);
end;

function TCustomPortI2C.WriteWordData(const Command: Byte; const Value: Word): Boolean;
begin
  Result := WriteBlockData(Command, @Value, SizeOf(Word)) = SizeOf(Word);
end;

{$ENDREGION}
{$REGION 'TCustomPortSPI'}

constructor TCustomPortSPI.Create(const AChipSelectMode: TChipSelectMode);
begin
  inherited Create;

  FChipSelectMode := AChipSelectMode;
end;

function TCustomPortSPI.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := Transfer(Buffer, nil, BufferSize);
end;

function TCustomPortSPI.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := Transfer(nil, Buffer, BufferSize);
end;

function TCustomPortSPI.Transfer(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := Transfer(Buffer, Buffer, BufferSize);
end;

{$ENDREGION}
{$REGION 'TCustomPortUART'}

constructor TCustomPortUART.Create(const ASystemCore: TCustomSystemCore);
begin
  inherited Create;

  FSystemCore := ASystemCore;
end;

function TCustomPortUART.ReadBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal;
var
  StartupTicks: TTickCounter;
  BytesRead, BytesProcessed, TimeoutUS: Cardinal;
begin
  BytesProcessed := 0;

  if (Timeout <> 0) and (FSystemCore <> nil) then
  begin
    TimeoutUS := Timeout * 1000;
    StartupTicks := FSystemCore.GetTickCount;
  end
  else
    TimeoutUS := 0;

  while BytesProcessed < BufferSize do
  begin
    BytesRead := Read(Pointer(PByte(Buffer) + BytesProcessed), BufferSize - BytesProcessed);
    if BytesRead <= 0 then
    begin
      if (TimeoutUS <> 0) and (FSystemCore.TicksInBetween(StartupTicks, FSystemCore.GetTickCount) >= TimeoutUS) then
        Break;

      if FSystemCore <> nil then
        FSystemCore.MicroDelay(InterimSleepTime);

      Continue;
    end;

    Inc(BytesProcessed, BytesRead);
  end;

  Result := BytesProcessed;
end;

function TCustomPortUART.WriteBuffer(const Buffer: Pointer; const BufferSize, Timeout: Cardinal): Cardinal;
var
  StartupTicks: TTickCounter;
  BytesWritten, BytesProcessed, TimeoutUS: Cardinal;
begin
  BytesProcessed := 0;

  if (Timeout <> 0) and (FSystemCore <> nil) then
  begin
    TimeoutUS := Timeout * 1000;
    StartupTicks := FSystemCore.GetTickCount;
  end
  else
    TimeoutUS := 0;

  while BytesProcessed < BufferSize do
  begin
    BytesWritten := Write(Pointer(PByte(Buffer) + Cardinal(BytesProcessed)), BufferSize - BytesProcessed);
    if BytesWritten = 0 then
    begin
      if (TimeoutUS <> 0) and (FSystemCore.TicksInBetween(StartupTicks, FSystemCore.GetTickCount) >= TimeoutUS) then
        Break;

      if FSystemCore <> nil then
        FSystemCore.MicroDelay(InterimSleepTime);

      Continue;
    end;

    Inc(BytesProcessed, BytesWritten);
  end;

  Result := BytesProcessed;
end;

function TCustomPortUART.ReadByte(out Value: Byte; const Timeout: Cardinal): Boolean;
begin
  Result := ReadBuffer(@Value, SizeOf(Byte), Timeout) = SizeOf(Byte);
end;

function TCustomPortUART.WriteByte(const Value: Byte; const Timeout: Cardinal): Boolean;
begin
  Result := WriteBuffer(@Value, SizeOf(Byte), Timeout) = SizeOf(Byte);
end;

function TCustomPortUART.WriteBytes(const Values: array of Byte; const Timeout: Cardinal): Boolean;
begin
  if Length(Values) > 0 then
    Result := WriteBuffer(@Values[0], Length(Values), Timeout) = Cardinal(Length(Values))
  else
    Result := False;
end;

function TCustomPortUART.ReadString(out Text: StdString; const MaxCharacters, Timeout: Cardinal): Boolean;
const
  PercentualLengthDiv = 4;
var
  StartupTicks: TTickCounter;
  BytesRead, BytesToRead, I, TextLength, NewTextLength: Integer;
  Buffer: array[0..StringBufferSize - 1] of Byte;
  TimeoutUS: Cardinal;
begin
  // Define initial string length.
  NewTextLength := StringBufferSize;

  if (MaxCharacters <> 0) and (NewTextLength > Integer(MaxCharacters)) then
    NewTextLength := Integer(MaxCharacters);

  SetLength(Text, NewTextLength);

  // Start time measurement.
  TextLength := 0;

  if (Timeout <> 0) and (FSystemCore <> nil) then
  begin
    TimeoutUS := Timeout * 1000;
    StartupTicks := FSystemCore.GetTickCount;
  end
  else
    TimeoutUS := 0;

  while ((MaxCharacters = 0) or (Cardinal(TextLength) < MaxCharacters)) and ((TimeoutUS = 0) or
    (FSystemCore.TicksInBetween(StartupTicks, FSystemCore.GetTickCount) < TimeoutUS)) do
  begin
    // Determine number of bytes that still need to be read.
    BytesToRead := StringBufferSize;

    if (MaxCharacters <> 0) and (BytesToRead > Integer(MaxCharacters) - TextLength) then
      BytesToRead := Integer(MaxCharacters) - TextLength;

    // Read bytes from serial port.
    BytesRead := Integer(Read(@Buffer[0], BytesToRead));
    if BytesRead <= 0 then
    begin
      if TimeoutUS = 0 then
        Break;

      if FSystemCore <> nil then
        FSystemCore.MicroDelay(InterimSleepTime);

      Continue;
    end;

    // Increase the length of string as necessary.
    if Length(Text) < TextLength + BytesRead then
    begin
      NewTextLength := Length(Text) + StringBufferSize + (Length(Text) div PercentualLengthDiv);

      if (MaxCharacters <> 0) and (NewTextLength > Integer(MaxCharacters)) then
        NewTextLength := Integer(MaxCharacters);

      SetLength(Text, NewTextLength);
    end;

    // Copy read bytes into the string.
    for I := 0 to BytesRead - 1 do
      Text[1 + TextLength + I] := Chr(Buffer[I]);

    Inc(TextLength, BytesRead);
  end;

  // Adjust the length of string to match what was actually read.
  SetLength(Text, TextLength);
  Result := Length(Text) > 0;
end;

function TCustomPortUART.WriteString(const Text: StdString; const Timeout: Cardinal): Boolean;
{$IF SIZEOF(StdChar) <> 1}
var
  StartupTicks: TTickCounter;
  I, WrittenLength, BytesToWrite, BytesWritten: Integer;
  Buffer: array[0..StringBufferSize - 1] of Byte;
  TimeoutUS: Cardinal;
{$ENDIF}
begin
  if Length(Text) <> 0 then
  begin
{$IF SIZEOF(StdChar) = 1}
    Result := WriteBuffer(@Text[1], Length(Text), Timeout) = Cardinal(Length(Text));
{$ELSE}
    WrittenLength := 0;

    if (Timeout <> 0) and (FSystemCore <> nil) then
    begin
      TimeoutUS := Timeout * 1000;
      StartupTicks := FSystemCore.GetTickCount;
    end
    else
      TimeoutUS := 0;

    while (WrittenLength < Length(Text)) and ((TimeoutUS = 0) or
      (FSystemCore.TicksInBetween(StartupTicks, FSystemCore.GetTickCount) < TimeoutUS)) do
    begin
      BytesToWrite := Length(Text) - WrittenLength;

      if BytesToWrite > StringBufferSize then
        BytesToWrite := StringBufferSize;

      for I := 0 to BytesToWrite - 1 do
        Buffer[I] := Ord(Text[1 + WrittenLength + I]);

      BytesWritten := Write(@Buffer[0], BytesToWrite);
      if BytesWritten <= 0 then
      begin
        if TimeoutUS = 0 then
          Break;

        if FSystemCore <> nil then
          FSystemCore.DelayUS(InterimSleepTime);

        Continue;
      end;

      Inc(WrittenLength, BytesWritten);
    end;

    Result := WrittenLength = Length(Text);
{$ENDIF}
  end
  else
    Result := False;
end;

{$ENDREGION}

end.


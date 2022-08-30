unit PXL.Sysfs.SPI;
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
  PXL.TypeDef, PXL.Boards.Types, PXL.Sysfs.Buses, PXL.Sysfs.Types;

type
  TSysfsSPI = class(TCustomPortSPI)
  public const
    DefaultFrequency = 8000000;
    DefaultMode = SPI_MODE_0;
    DefaultBitsPerWord = 8;
  private
    FSystemPath: StdString;
    FHandle: TUntypedHandle;

    FFrequency: Cardinal;
    FBitsPerWord: TBitsPerWord;
    FMode: TSPIMode;

    procedure UpdateRWMode;
    procedure UpdateBitsPerWord;
    procedure UpdateFrequency;
  protected
    function GetFrequency: Cardinal; override;
    procedure SetFrequency(const Value: Cardinal); override;
    function GetBitsPerWord: TBitsPerWord; override;
    procedure SetBitsPerWord(const Value: TBitsPerWord); override;
    function GetMode: TSPIMode; override;
    procedure SetMode(const Value: TSPIMode); override;
  public
    constructor Create(const ASystemPath: StdString; const AFrequency: Cardinal = DefaultFrequency;
      const ABitsPerWord: TBitsPerWord = DefaultBitsPerWord; const AMode: TSPIMode = DefaultMode);
    destructor Destroy; override;

    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    property SystemPath: StdString read FSystemPath;
    property Handle: TUntypedHandle read FHandle;

    property Frequency: Cardinal read FFrequency write SetFrequency;
    property BitsPerWord: TBitsPerWord read FBitsPerWord write SetBitsPerWord;
    property Mode: TSPIMode read FMode write SetMode;
  end;

  ESysfsSPIOpen = class(ESysfsFileOpen);

  ESysfsSPIMode = class(ESysfsGeneric);
  ESysfsSPIWriteMode = class(ESysfsSPIMode);
  ESysfsSPIReadMode = class(ESysfsSPIMode);

  ESysfsSPIBitsPerWord = class(ESysfsGeneric);
  ESysfsSPIWriteBitsPerWord = class(ESysfsSPIBitsPerWord);
  ESysfsSPIReadBitsPerWord = class(ESysfsSPIBitsPerWord);

  ESysfsSPIFrequency = class(ESysfsGeneric);
  ESysfsSPIWriteFrequency = class(ESysfsSPIBitsPerWord);
  ESysfsSPIReadFrequency = class(ESysfsSPIBitsPerWord);

  ESysfsSPITransfer = class(ESysfsGeneric);

resourcestring
  SCannotOpenFileForSPI = 'Cannot open SPI file <%s> for reading and writing.';
  SCannotSetSPIWriteMode = 'Cannot set SPI write mode to <%d>.';
  SCannotSetSPIReadMode = 'Cannot set SPI read mode to <%d>.';
  SCannotSetSPIWriteBitsPerWord = 'Cannot set SPI write bits per word to <%d>.';
  SCannotSetSPIReadBitsPerWord = 'Cannot set SPI read bits per word to <%d>.';
  SCannotSetSPIWriteFrequency = 'Cannot set SPI write frequency to <%d>.';
  SCannotSetSPIReadFrequency = 'Cannot set SPI read frequency to <%d>.';
  SCannotSPITransferBytes = 'Cannot transfer <%d> data byte(s) through SPI bus.';

implementation

uses
  SysUtils, BaseUnix;

constructor TSysfsSPI.Create(const ASystemPath: StdString; const AFrequency: Cardinal;
  const ABitsPerWord: TBitsPerWord; const AMode: TSPIMode);
begin
  inherited Create;

  FSystemPath := ASystemPath;

  FHandle := fpopen(FSystemPath, O_RDWR);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise ESysfsSPIOpen.Create(Format(SCannotOpenFileForSPI, [FSystemPath]));
  end;

  FFrequency := AFrequency;
  UpdateFrequency;

  FBitsPerWord := ABitsPerWord;
  UpdateBitsPerWord;

  FMode := AMode;
  UpdateRWMode;
end;

destructor TSysfsSPI.Destroy;
begin
  if FHandle <> 0 then
  begin
    fpclose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

procedure TSysfsSPI.UpdateRWMode;
var
  ModeValue, Param: Byte;
begin
  ModeValue := FMode;
  if FChipSelectMode = TChipSelectMode.ActiveHigh then
    ModeValue := ModeValue or SPI_CS_HIGH
  else if FChipSelectMode = TChipSelectMode.Disabled then
    ModeValue := ModeValue or SPI_NO_CS;

  Param := ModeValue;

  if fpioctl(FHandle, SPI_IOC_WR_MODE, @Param) < 0 then
    raise ESysfsSPIWriteMode.Create(Format(SCannotSetSPIWriteMode, [FMode]));

  Param := ModeValue;

  if fpioctl(FHandle, SPI_IOC_RD_MODE, @Param) < 0 then
    raise ESysfsSPIReadMode.Create(Format(SCannotSetSPIReadMode, [FMode]));
end;

procedure TSysfsSPI.UpdateBitsPerWord;
var
  BitsValue: Byte;
begin
  BitsValue := FBitsPerWord;

  if fpioctl(FHandle, SPI_IOC_WR_BITS_PER_WORD, @BitsValue) < 0 then
    raise ESysfsSPIWriteBitsPerWord.Create(Format(SCannotSetSPIWriteBitsPerWord, [FBitsPerWord]));

  BitsValue := FBitsPerWord;

  if fpioctl(FHandle, SPI_IOC_RD_BITS_PER_WORD, @BitsValue) < 0 then
    raise ESysfsSPIReadBitsPerWord.Create(Format(SCannotSetSPIReadBitsPerWord, [FBitsPerWord]));
end;

procedure TSysfsSPI.UpdateFrequency;
var
  FrequencyValue: LongWord;
begin
  FrequencyValue := FFrequency;

  if fpioctl(FHandle, SPI_IOC_WR_MAX_SPEED_HZ, @FrequencyValue) < 0 then
    raise ESysfsSPIWriteFrequency.Create(Format(SCannotSetSPIWriteFrequency, [FFrequency]));

  FrequencyValue := FFrequency;

  if fpioctl(FHandle, SPI_IOC_RD_MAX_SPEED_HZ, @FrequencyValue) < 0 then
    raise ESysfsSPIReadFrequency.Create(Format(SCannotSetSPIReadFrequency, [FFrequency]));
end;

function TSysfsSPI.GetFrequency: Cardinal;
begin
  Result := FFrequency;
end;

procedure TSysfsSPI.SetFrequency(const Value: Cardinal);
begin
  if FFrequency <> Value then
  begin
    FFrequency := Value;
    UpdateFrequency;
  end;
end;

function TSysfsSPI.GetBitsPerWord: TBitsPerWord;
begin
  Result := FBitsPerWord;
end;

procedure TSysfsSPI.SetBitsPerWord(const Value: TBitsPerWord);
begin
  if FBitsPerWord <> Value then
  begin
    FBitsPerWord := Value;
    UpdateBitsPerWord;
  end;
end;

function TSysfsSPI.GetMode: TSPIMode;
begin
  Result := FMode;
end;

procedure TSysfsSPI.SetMode(const Value: TSPIMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    UpdateRWMode;
  end;
end;

function TSysfsSPI.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := Transfer(Buffer, nil, BufferSize);
end;

function TSysfsSPI.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
begin
  Result := Transfer(nil, Buffer, BufferSize);
end;

function TSysfsSPI.Transfer(const ReadBuffer, WriteBuffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  Data: spi_ioc_transfer;
  Res: Integer;
begin
  if ((ReadBuffer = nil) and (WriteBuffer = nil)) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  Data.tx_buf := PtrUInt(WriteBuffer);
  Data.rx_buf := PtrUInt(ReadBuffer);
  Data.len := BufferSize;
  Data.delay_usecs := 0;
  Data.speed_hz := FFrequency;
  Data.bits_per_word := FBitsPerWord;

  Res := fpioctl(FHandle, SPI_IOC_MESSAGE(1), @Data);
  if Res < 0 then
    raise ESysfsSPITransfer.Create(Format(SCannotSPITransferBytes, [BufferSize]));

  Result := Cardinal(Res);
end;

end.


unit PXL.Sysfs.I2C;
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
  PXL.TypeDef, PXL.Boards.Types, PXL.Sysfs.Types;

type
  TSysfsI2C = class(TCustomPortI2C)
  private
    FSystemPath: StdString;
    FHandle: TUntypedHandle;
    FCurrentAddress: Cardinal;
  public
    constructor Create(const ASystemPath: StdString);
    destructor Destroy; override;

    procedure SetAddress(const Address: Cardinal); override;

    function Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    function ReadByte(out Value: Byte): Boolean; override;
    function WriteByte(const Value: Byte): Boolean; override;

    function WriteQuick(const Value: Byte): Boolean;

    function ReadByteData(const Command: Byte; out Value: Byte): Boolean; override;
    function WriteByteData(const Command, Value: Byte): Boolean; override;

    function ReadWordData(const Command: Byte; out Value: Word): Boolean; override;
    function WriteWordData(const Command: Byte; const Value: Word): Boolean; override;

    function ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;
    function WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Cardinal): Cardinal; override;

    function ProcessCall(const Command: Byte; var Value: Word): Boolean;
    function ProcessBlockCall(const Command: Byte; const Buffer: Pointer;
      const BufferSize: Cardinal): Cardinal;

    property SystemPath: StdString read FSystemPath;
    property Handle: TUntypedHandle read FHandle;
  end;

  ESysfsI2COpen = class(ESysfsFileOpen);
  ESysfsI2CAddress = class(ESysfsGeneric);
  ESysfsI2CBusWrite = class(ESysfsFileWrite);
  ESysfsI2CBusRead = class(ESysfsFileRead);
  ESysfsI2CBusProcess = class(ESysfsFileRead);

resourcestring
  SCannotOpenFileForI2C = 'Cannot open I2C file <%s> for reading and writing.';
  SCannotSetI2CSlaveAddress = 'Cannot set <0x%x> slave address for I2C bus.';
  SErrorReadI2CRawBytes = 'Error reading <%d> raw byte(s) from I2C bus.';
  SErrorWriteI2CRawBytes = 'Error writing <%d> raw byte(s) to I2C bus.';
  SErrorReadI2CDataBytes = 'Error reading <%d> data byte(s) from I2C bus.';
  SErrorWriteI2CDataBytes = 'Error writing <%d> data byte(s) to I2C bus.';
  SErrorReadI2CDataBlock = 'Error reading data block from I2C bus.';
  SErrorProcessI2CDataBytes = 'Error processing <%d> data byte(s) with I2C bus.';

implementation

uses
  SysUtils, BaseUnix, PXL.Sysfs.Buses;

constructor TSysfsI2C.Create(const ASystemPath: StdString);
begin
  inherited Create;

  FSystemPath := ASystemPath;
  FCurrentAddress := High(Cardinal);

  FHandle := fpopen(FSystemPath, O_RDWR);
  if FHandle < 0 then
  begin
    FHandle := 0;
    raise ESysfsI2COpen.Create(Format(SCannotOpenFileForI2C, [FSystemPath]));
  end;
end;

destructor TSysfsI2C.Destroy;
begin
  if FHandle <> 0 then
  begin
    fpclose(FHandle);
    FHandle := 0;
  end;

  inherited;
end;

procedure TSysfsI2C.SetAddress(const Address: Cardinal);
begin
  if FCurrentAddress <> Address then
  begin
    FCurrentAddress := Address;

    if FCurrentAddress <> High(Cardinal) then
      if fpioctl(FHandle, I2C_SLAVE, Pointer(FCurrentAddress)) < 0 then
        raise ESysfsI2CAddress.Create(Format(SCannotSetI2CSlaveAddress, [FCurrentAddress]));
  end;
end;

function TSysfsI2C.Read(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  BytesRead: Integer;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  BytesRead := fpread(FHandle, Buffer^, BufferSize);
  if BytesRead < 0 then
    raise ESysfsI2CBusRead.Create(Format(SErrorReadI2CRawBytes, [BufferSize]));

  Result := Cardinal(BytesRead);
end;

function TSysfsI2C.Write(const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  BytesWritten: Integer;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  BytesWritten := fpwrite(FHandle, Buffer^, BufferSize);
  if BytesWritten < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CRawBytes, [BufferSize]));

  Result := Cardinal(BytesWritten);
end;

function TSysfsI2C.ReadByte(out Value: Byte): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_read_byte(FHandle);
  if Res < 0 then
    raise ESysfsI2CBusRead.Create(Format(SErrorReadI2CRawBytes, [SizeOf(Byte)]));

  Result := Res >= 0;
  if Result then
    Value := Res;
end;

function TSysfsI2C.WriteByte(const Value: Byte): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_write_byte(FHandle, Value);
  if Res < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CRawBytes, [SizeOf(Byte)]));

  Result := Res >= 0;
end;

function TSysfsI2C.WriteQuick(const Value: Byte): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_write_quick(FHandle, Value);
  if Res < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CRawBytes, [SizeOf(Byte)]));

  Result := Res >= 0;
end;

function TSysfsI2C.ReadByteData(const Command: Byte; out Value: Byte): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_read_byte_data(FHandle, Command);
  if Res < 0 then
    raise ESysfsI2CBusRead.Create(Format(SErrorReadI2CDataBytes, [SizeOf(Byte)]));

  Result := Res >= 0;
  if Result then
    Value := Res;
end;

function TSysfsI2C.WriteByteData(const Command, Value: Byte): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_write_byte_data(FHandle, Command, Value);
  if Res < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CDataBytes, [SizeOf(Byte)]));

  Result := Res >= 0;
end;

function TSysfsI2C.ReadWordData(const Command: Byte; out Value: Word): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_read_word_data(FHandle, Command);
  if Res < 0 then
    raise ESysfsI2CBusRead.Create(Format(SErrorReadI2CDataBytes, [SizeOf(Word)]));

  Result := Res >= SizeOf(Word);
  if Result then
    Value := Res;
end;

function TSysfsI2C.WriteWordData(const Command: Byte; const Value: Word): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_write_word_data(FHandle, Command, Value);
  if Res < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CDataBytes, [SizeOf(Word)]));

  Result := Res >= SizeOf(Word);
end;

function TSysfsI2C.ReadBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  TempBuf: Pointer;
  Res: Integer;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  if BufferSize < I2C_SMBUS_BLOCK_MAX then
  begin
    GetMem(TempBuf, I2C_SMBUS_BLOCK_MAX);
    try
      Res := i2c_smbus_read_block_data(FHandle, Command, TempBuf);
      if Res < 0 then
        raise ESysfsI2CBusRead.Create(SErrorReadI2CDataBlock);

      if Cardinal(Res) > BufferSize then
        Cardinal(Res) := BufferSize;

      Move(TempBuf^, Buffer^, Res);
    finally
      FreeMem(TempBuf);
    end;
  end
  else
  begin
    Res := i2c_smbus_read_block_data(FHandle, Command, Buffer);
    if Res < 0 then
      raise ESysfsI2CBusRead.Create(SErrorReadI2CDataBlock);
  end;

  Result := Cardinal(Res);
end;

function TSysfsI2C.WriteBlockData(const Command: Byte; const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  Res: Integer;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  Res := i2c_smbus_write_i2c_block_data(FHandle, Command, BufferSize, Buffer);
  if Res < 0 then
    raise ESysfsI2CBusWrite.Create(Format(SErrorWriteI2CDataBytes, [BufferSize]));

  Result := Cardinal(Res);
end;

function TSysfsI2C.ProcessCall(const Command: Byte; var Value: Word): Boolean;
var
  Res: Integer;
begin
  Res := i2c_smbus_process_call(FHandle, Command, Value);
  if Res < 0 then
    raise ESysfsI2CBusProcess.Create(Format(SErrorProcessI2CDataBytes, [SizeOf(Word)]));

  Result := Res > 0;
  if Result then
    Value := Res;
end;

function TSysfsI2C.ProcessBlockCall(const Command: Byte; const Buffer: Pointer; const BufferSize: Cardinal): Cardinal;
var
  TempBuf: Pointer;
  Res: Integer;
begin
  if (Buffer = nil) or (BufferSize <= 0) then
    raise ESysfsInvalidParams.Create(SInvalidParameters);

  if BufferSize < I2C_SMBUS_BLOCK_MAX then
  begin
    GetMem(TempBuf, I2C_SMBUS_BLOCK_MAX);
    try
      Move(Buffer^, TempBuf, BufferSize);

      Res := i2c_smbus_block_process_call(FHandle, Command, BufferSize, TempBuf);
      if Res < 0 then
        raise ESysfsI2CBusProcess.Create(Format(SErrorProcessI2CDataBytes, [BufferSize]));

      if Cardinal(Res) > BufferSize then
        Cardinal(Res) := BufferSize;

      Move(TempBuf^, Buffer^, Res);
    finally
      FreeMem(TempBuf);
    end;
  end
  else
  begin
    Res := i2c_smbus_block_process_call(FHandle, Command, BufferSize, Buffer);
    if Res < 0 then
      raise ESysfsI2CBusProcess.Create(Format(SErrorProcessI2CDataBytes, [BufferSize]));
  end;

  Result := Cardinal(Res);
end;

end.


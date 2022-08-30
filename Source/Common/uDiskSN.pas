unit uDiskSN;

interface

uses
  SysUtils, Windows, Classes;

{--- Disk Serial Nomber ---}
const
  IDENTIFY_BUFFER_SIZE      = 512;

type
  TIDERegs = packed record
    bFeaturesReg: Byte;                 // Used for specifying SMART "commands".
    bSectorCountReg: Byte;              // IDE sector count register
    bSectorNumberReg: Byte;             // IDE sector number register
    bCylLowReg: Byte;                   // IDE low order cylinder value
    bCylHighReg: Byte;                  // IDE high order cylinder value
    bDriveHeadReg: Byte;                // IDE drive/head register
    bCommandReg: Byte;                  // Actual IDE command.
    bReserved: Byte;                    // reserved for future use. Must be zero.
  end;

  TSendCmdInParams = packed record
    cBufferSize: DWORD;                 // Buffer size in bytes
    irDriveRegs: TIDERegs;              // Structure with drive register values.
    bDriveNumber: Byte;                 // Physical drive number to send command to (0,1,2,3).
    bReserved: array[0..2] of Byte;
    dwReserved: array[0..3] of DWORD;
    bBuffer: array[0..0] of Byte;       // Input buffer.
  end;

  TIdSector = packed record
    wGenConfig: Word;
    wNumCyls: Word;
    wReserved: Word;
    wNumHeads: Word;
    wBytesPerTrack: Word;
    wBytesPerSector: Word;
    wSectorsPerTrack: Word;
    wVendorUnique: array[0..2] of Word;
    sSerialNumber: array[0..19] of CHAR;
    wBufferType: Word;
    wBufferSize: Word;
    wECCSize: Word;
    sFirmwareRev: array[0..7] of CHAR;
    sModelNumber: array[0..39] of CHAR;
    wMoreVendorUnique: Word;
    wDoubleWordIO: Word;
    wCapabilities: Word;
    wReserved1: Word;
    wPIOTiming: Word;
    wDMATiming: Word;
    wBS: Word;
    wNumCurrentCyls: Word;
    wNumCurrentHeads: Word;
    wNumCurrentSectorsPerTrack: Word;
    ulCurrentSectorCapacity: DWORD;
    wMultSectorStuff: Word;
    ulTotalAddressableSectors: DWORD;
    wSingleWordDMA: Word;
    wMultiWordDMA: Word;
    bReserved: array[0..127] of Byte;
  end;
  PIdSector = ^TIdSector;

  TDriverStatus = packed record
    bDriverError: Byte;                 // 驱动器返回的错误代码，无错则返回0
    bIDEStatus: Byte;                   // IDE出错寄存器的内容，只有当bDriverError 为 SMART_IDE_ERROR 时有效
    bReserved: array[0..1] of Byte;
    dwReserved: array[0..1] of DWORD;
  end;

  TSendCmdOutParams = packed record
    cBufferSize: DWORD;                 // bBuffer的大小
    DriverStatus: TDriverStatus;        // 驱动器状态
    bBuffer: array[0..0] of Byte;       // 用于保存从驱动器读出的数据的缓冲区，实际长度由cBufferSize决定
  end;

  //DMA
  PMemoryBuffer = ^TMemoryBuffer;
  TMemoryBuffer = array[0..65535] of ANSICHAR;
  TArrayBuffer = array[0..254] of ANSICHAR;

function GetIdeSerialNumber(DeviceNumber: Byte): PChar;

implementation

procedure ChangeByteOrder(var Data; Size: Integer);
var
  ptr                       : PChar;
  i                         : Integer;
  c                         : CHAR;
begin
  ptr := @Data;
  for i := 0 to (Size shr 1) - 1 do begin
    c := ptr^;
    ptr^ := (ptr + 1)^;
    (ptr + 1)^ := c;
    Inc(ptr, 2);
  end;
end;

function GetIdeSerialNumber(DeviceNumber: Byte): PChar;
var
  hDevice                   : Thandle;
  cbBytesReturned           : DWORD;
  SCIP                      : TSendCmdInParams;
  aIdOutCmd                 : array[0..(SizeOf(TSendCmdOutParams) + IDENTIFY_BUFFER_SIZE - 1) - 1] of Byte;
  IdOutCmd                  : TSendCmdOutParams absolute aIdOutCmd;
begin
  Result := '';
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then begin
    hDevice := CreateFile('\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
      FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
    if hDevice = INVALID_HANDLE_VALUE then
      hDevice := CreateFile('\\.\PhysicalDrive0', GENERIC_READ or GENERIC_WRITE,
        FILE_SHARE_READ or FILE_SHARE_WRITE, nil, OPEN_EXISTING, FILE_ATTRIBUTE_NORMAL, 0);
  end else                              // Version Windows 95 OSR2, Windows 98
    hDevice := CreateFile('\\.\SMARTVSD', 0, 0, nil, CREATE_NEW, 0, 0);

  if hDevice = INVALID_HANDLE_VALUE then
    Exit;

  try
    FillChar(SCIP, SizeOf(TSendCmdInParams) - 1, #0);
    FillChar(aIdOutCmd, SizeOf(aIdOutCmd), #0);
    cbBytesReturned := 0;
    // Set up data structures for IDENTIFY command.
    with SCIP do begin
      cBufferSize := IDENTIFY_BUFFER_SIZE;
      // bDriveNumber := 0;
      with irDriveRegs do begin
        bSectorCountReg := 1;
        bSectorNumberReg := 1;
        // if Win32Platform = VER_PLATFORM_WIN32_NT then bDriveHeadReg := $A0
        // else bDriveHeadReg := $A0 or ((bDriveNum and 1) shl 4);
        bDriveHeadReg := $A0;
        bCommandReg := $EC;
      end;
    end;
    if not DeviceIoControl(hDevice, $0007C088, @SCIP, SizeOf(TSendCmdInParams) - 1, @aIdOutCmd, SizeOf(aIdOutCmd), cbBytesReturned, nil) then
      Exit;
  finally
    CloseHandle(hDevice);
  end;

  with PIdSector(@IdOutCmd.bBuffer)^ do begin
    ChangeByteOrder(sSerialNumber, SizeOf(sSerialNumber));
    (PChar(@sSerialNumber) + SizeOf(sSerialNumber))^ := #0;
    Result := PChar(Trim(sSerialNumber));
  end;
end;

end.


{*******************************************************}
{               MiTeC Common Routines                   }
{          Storage Device Detection routines            }
{                                                       }
{         Copyright (c) by 1997-2019 Michal Mutl        }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

{$IFDEF RAD7PLUS}
{$WARN IMPLICIT_STRING_CAST OFF}
{$WARN IMPLICIT_STRING_CAST_LOSS OFF}
{$WARN WIDECHAR_REDUCED OFF}
{$ENDIF}

unit MiTeC_Storage;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_WinIOCTL, MiTeC_Routines;

type
  TDiskLayout = array of TPartitionInformation;

  TDiskExtents = array of TDiskExtent;

  TSMARTAttributes = array of TDriveAttribute;

  TSMARTThresholds = array of TAttrThreshold;

  TDeviceInfoKind = (dikUnknown,dikPhysicalDrive,dikLogicalDrive,dikCDROM,dikTape,dikSymbolicLink);

  TRawData = array[0..sizeof(TIdentifyDeviceData)-1] of byte;

  TDeviceInfo = record
    Kind: TDeviceInfoKind;

    Device: Cardinal;
    Number: Cardinal;
    Instance: Cardinal;
    Drive: Char;

    PhysicalIndex: Integer;
    LayoutIndex: Integer;

    SerialNumber,
    Model,
    Revision,
    VolumeLabel,
    LogicalSerial: string;

    HaId,
    Target,
    Lun,
    PathId: integer;

    DeviceType, MediaType, BusType: Cardinal;
    Removable,RAM: Boolean;
    Size: UInt64;
    LogicalSize: UInt64;

    SMARTSupport,
    SMARTActive: Boolean;

    CtlBufSize,
    ECCCode: Cardinal;

    Layout: TDiskLayout;
    Geometry: TDiskGeometry;
    LengthInBytes: Int64;

    FileSystem: string;
    ClusterSize: Cardinal;

    SMART: TSmartAttributes;
    SMARTThresholds: TSMARTThresholds;
    Temperature: Byte;

    SSD,
    Read_CDR,
    Read_CDRW,
    Write_CDR,
    Write_CDRW,
    Read_DVDROM,
    Read_DVDR,
    Read_DVDRAM,
    Write_DVDR,
    Write_DVDRAM: Boolean;

    DiskExtents: TDiskExtents;

    HasNoSeekPenalty: Boolean;

    IdentifyDeviceData: TIdentifyDeviceData;

    {$IFDEF DEBUG}
    _RawData: TRawData;
    {$ENDIF}
  end;

procedure ClearStorageRecord(var ARecord: TDeviceInfo);

function GetHandle_PhysicalDrive(DeviceNumber: Byte): THandle;
function GetHandle_CdRom(DeviceNumber: Byte): THandle;
function GetHandle_Tape(DeviceNumber: Byte): THandle;
function GetHandle_LogicalDrive(DeviceNumber: Byte): THandle;
function GetHandle_LogicalDisk(Disk: string): THandle;
function GetHandle_SCSI(DeviceNumber: byte): THandle;
function GetHandle_Symbolic(ALink: string): THandle;

function GetData_ATA_PassThrough(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetData_SMART(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetData_SCSI_Miniport(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetData_SCSI_PassThrough(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetData_DeviceDescriptor(hDevice: THandle; var ADI: TDeviceInfo): Boolean;

function GetUSBSerialNumber(hDevice: THandle): string;
function GetDeviceNumber(hDevice: THandle; var ADeviceNumber: Cardinal): Boolean;
function GetDeviceLength(hDevice: THandle; var ALength: int64): Boolean;
function GetDeviceLocation(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetDeviceMediaType(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetDeviceGeometry(hDevice: THandle; var DG: TDiskGeometry): Boolean;
function GetDiskExtents(hDevice: THandle; var DE: TDiskExtents): Boolean;
function GetDeviceLayout(hDevice: THandle; var DL: TDiskLayout): Boolean;
function GetDeviceCapabilities(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetDeviceSMARTAttrs(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetDeviceSMARTAttrs2(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
function GetDiskLetterFromDeviceNumber(AValue: Cardinal): string;
function GetSymbolicLinkFromDeviceNumber(AValue: Cardinal): string;
procedure GetSymbolicLinkTable(ATable: TStringlist);

function GetDiskTemp(hDevice: THandle): integer;

procedure GetDeviceData(hdevice: THandle; var ADI: TDeviceInfo; DetectSMART: Boolean);
procedure IDDToDI(AData: TIdentifyDeviceData; var ADI: TDeviceInfo);
procedure AddInfoToDI(Source: TDeviceInfo; var Dest: TDeviceInfo);
function DecodeSerial(AValue: string): string;

function GetATAMajorVersion(AValue: word): string;
function GetATAMinorVersion(AValue: word): string;
function GetATATransportVersion(AValue: word): string;

function IsPortable: Boolean;

var
  LastIOCTLError: integer;

implementation

uses MiTeC_StrUtils, MiTeC_CfgMgrSetupAPI;

procedure ClearStorageRecord(var ARecord: TDeviceInfo);
begin
  Finalize(ARecord);
  ResetMemory(ARecord,SizeOf(TDeviceInfo));
  ARecord.PhysicalIndex:=-1;
  ARecord.LayoutIndex:=-1;
  ARecord.HaId:=-1;
  ARecord.Target:=-1;
  ARecord.Lun:=-1;
  ARecord.PathId:=-1;
end;

procedure CorrectDataBuffer(Buffer: PAnsiChar);
var
  i,idx: integer;
  x: word;
begin
  for i:=0 to 9 do begin
    idx:=$14+i+i;
    x:=pword(Buffer+idx)^;
    pword(Buffer+idx)^:=Swap(x);
  end;

  for i:=0 to $17 do begin
    idx:=$14+26+i+i;
    x:=pword(Buffer+idx)^;
    pword(Buffer+idx)^:=Swap(x);
  end;
end;


function CorrectSerial(AData: PByteArray; ASize: Cardinal): AnsiString;
var
  i,b: integer;
  c: ansichar;
  s: AnsiString;
  hex: Boolean;
begin
  Result:='';
  s:='';
  b:=0;
  for i:=0 to ASize-1 do begin
    if AData^[i] in [32..127] then begin
      s:=s+Chr(AData^[i]);
      b:=0;
    end else if AData^[i]=0 then
      Inc(b);
    if b>1 then
      Break;
  end;
  {b:=0;
  if s='' then begin
    for i:=0 to ASize-1 do begin
      if AData[i]=0 then
        Inc(b)
      else begin
        Result:=Result+IntToHex(AData[i],2);
        b:=0;
      end;
      if b>1 then
        Break;
    end;
  end else} begin
    i:=1;
    hex:=True;
    while i<Length(s) do begin
      if TryStrtoInt('$'+Copy(s,i,2),b) and (b in [32..127]) then begin
        Result:=Result+Chr(b);
        inc(i,2);
      end else begin
        hex:=False;
        Result:=s;
        break;
      end;
    end;

    if hex then begin
      if OS<osWin8 then begin
        i:=1;
        while i<Length(Result) do begin
          c:=Result[i];
          Result[i]:=Result[i+1];
          Result[i+1]:=c;
          inc(i,2);
        end;
      end;
    end;
  end;
  Result:=Trim(Result);
end;

function GetDeviceGeometry(hDevice: THandle; var DG: TDiskGeometry): Boolean;
var
  cbBytesReturned: Cardinal;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  ResetMemory(DG,SizeOf(TDiskGeometry));
  Result:=DeviceIoControl(hDevice,
                          IOCTL_DISK_GET_DRIVE_GEOMETRY,
                          nil,
                          0,
                          @DG,sizeof(TDISKGEOMETRY),
                          cbBytesReturned,
                          nil);
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDiskExtents(hDevice: THandle; var DE: TDiskExtents): Boolean;
var
  n: Cardinal;
  i: Integer;
  VDE: PVolumeDiskExtents;
  E: PDiskExtent;
begin
  Finalize(DE);
  Result:=False;
  if (hDevice<>INVALID_HANDLE_VALUE) then begin
    VDE:=AllocMem($400);
    try
      if DeviceIoControl(hDevice,IOCTL_VOLUME_GET_VOLUME_DISK_EXTENTS,nil,0,VDE,$400,n,nil) then begin
        E:=@VDE^.Extents[0];
        for i:=0 to (VDE^.NumberOfDiskExtents)-1 do begin
          if E^.ExtentLength.QuadPart>0 then begin
            SetLength(DE,Length(DE)+1);
            DE[High(DE)]:=E^;
          end;
          E:=PDiskExtent(PAnsiChar(E)+SizeOf(TDiskExtent));
        end;
        Result:=True;
      end;
    finally
      FreeMem(VDE);
      LastIOCTLError:=GetLastError;
    end
  end;
end;

function GetDeviceLayout(hDevice: THandle; var DL: TDiskLayout): Boolean;
var
  Layout: PDriveLayoutInformation;
  Partitions: PPartitionInformation;
  BytesReturned, BufferSize: Cardinal;
  i: Integer;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  Finalize(DL);
  BufferSize:=SizeOf(TDriveLayoutInformation)*64;
  Layout:=AllocMem(BufferSize);
  try
    if DeviceIoControl(hDevice,
                       IOCTL_DISK_GET_DRIVE_LAYOUT,
                       nil,
                       0,
                       Layout,
                       BufferSize,
                       BytesReturned,
                       nil) then begin
        Partitions:=@Layout^.PartitionEntry[0];
        for i:=0 to (Layout^.PartitionCount)-1 do begin
          if Partitions^.Number >= 1 then begin
            //if Partitions^.Typ<>PARTITION_ENTRY_UNUSED then begin
              SetLength(DL,Length(DL)+1);
              DL[High(DL)]:=Partitions^;
            end;
          Partitions:=PPartitionInformation(PAnsiChar(Partitions)+SizeOf(TPartitionInformation));
        end;
        Result:=True;
      end;
  finally
    FreeMem(Layout);
    LastIOCTLError:=GetLastError;
  end;
end;

function GetHandle_PhysicalDrive(DeviceNumber: Byte): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\PhysicalDrive%d',[DeviceNumber]));
end;

function GetHandle_CdRom(DeviceNumber: Byte): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\Cdrom%d',[DeviceNumber]));
end;

function GetHandle_Tape(DeviceNumber: Byte): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\Tape%d',[DeviceNumber]));
end;

function GetHandle_LogicalDrive(DeviceNumber: Byte): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\%s:',[Chr(DeviceNumber)]));
end;

function GetHandle_LogicalDisk(Disk: string): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\%s:',[Copy(Disk,1,1)]));
end;

function GetHandle_SCSI(DeviceNumber: byte): THandle;
begin
  Result:=GetDeviceHandle(Format('\\.\Scsi%d:',[DeviceNumber div 2]));
end;

function GetHandle_Symbolic(ALink: string): THandle;
begin
  Result:=GetDeviceHandle(ALink);
end;

function GetData_ATA_PassThrough(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  query: TATAIdentifyDeviceQuery;
  n: Cardinal;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  FillChar(query,SizeOf(query),0);
  with query do begin
    header.Length:=SizeOf(header);
    header.AtaFlags:=ATA_FLAGS_DATA_IN or ATA_FLAGS_DRDY_REQUIRED;
    header.DataTransferLength:=SizeOf(data);
    header.TimeOutValue:=3;  // sec
    header.DataBufferOffset:=SizeOf(header);
    header.CurrentTaskFile[6]:=$EC;  // ATA IDENTIFY DEVICE command
  end;
  n:=0;
  if DeviceIoControl(hDevice,IOCTL_ATA_PASS_THROUGH,@query,SizeOf(query),@query,SizeOf(query),n,nil) then begin
    Move(query.Data,ADI.IdentifyDeviceData,SizeOf(TIdentifyDeviceData));
    {$IFDEF DEBUG}
    Move(query.Data,ADI._RawData,SizeOf(TRawData));
    {$ENDIF}
    IDDToDI(ADI.IdentifyDeviceData,ADI);
    Result:=True;
  end;
end;

function GetData_SMART(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  gvop: GETVERSIONOUTPARAMS;
  scip :SENDCMDINPARAMS;
  scop :SENDCMDOUTPARAMS;
  cmd: BYTE;
  n: Cardinal;
  buffer: array [0..sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS)] of ansichar;
  scsiAddr: TSCSIAddress absolute Buffer;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;

  try
    ResetMemory(gvop,sizeof(GETVERSIONOUTPARAMS));
    ResetMemory(scip,sizeof(SENDCMDINPARAMS));
    ResetMemory(scop,sizeof(SENDCMDOUTPARAMS));

    if not DeviceIoControl(hDevice,
                           SMART_GET_VERSION,
                           nil,
                           0,
                           @gvop,
                           sizeof(GETVERSIONOUTPARAMS),
                           n,
                           nil) then
      cmd:=IDE_ATA_IDENTIFY
    else
      if ((gvop.bIDEDeviceMap shr ADI.Device) and 1)=1 then
        cmd:=IDE_ATA_IDENTIFY
      else
        cmd:=IDE_ATAPI_IDENTIFY;
    scip.cBufferSize:=512;
    scip.bDriveNumber:=0;//ADI.Device;
    scip.irDriveRegs.bSectorCountReg:=1;
    scip.irDriveRegs.bSectorNumberReg:=1;
    scip.irDriveRegs.bDriveHeadReg:=$A0 or ((ADI.Device and 1) shl 4);
    if Win32Platform<>VER_PLATFORM_WIN32_NT then
      scip.irDriveRegs.bDriveHeadReg:=scip.irDriveRegs.bDriveHeadReg or (ADI.Device and 1)*16;
    scip.irDriveRegs.bCommandReg:=cmd;
    if not DeviceIoControl(hDevice,
                           SMART_RCV_DRIVE_DATA,
                           @scip,
                           sizeof(SENDCMDINPARAMS),
                           @scop,
                           sizeof(SENDCMDOUTPARAMS),
                           n,
                           nil) then begin
      if cmd=IDE_ATA_IDENTIFY then
        cmd:=IDE_ATAPI_IDENTIFY
      else
        cmd:=IDE_ATA_IDENTIFY;
      scip.irDriveRegs.bCommandReg:=cmd;
      if not DeviceIoControl(hDevice,
                             SMART_RCV_DRIVE_DATA,
                             @scip,
                             sizeof(SENDCMDINPARAMS),
                             @scop,
                             sizeof(SENDCMDOUTPARAMS),
                             n,
                             nil) then
        Exit;
    end;

    Move(scop.bBuffer,ADI.IdentifyDeviceData,SizeOf(TIdentifyDeviceData));
    {$IFDEF DEBUG}
    Move(scop.bBuffer,ADI._RawData,SizeOf(TRawData));
    {$ENDIF}

    if scop.DriverStatus.bDriverError<>0 then
      Exit;

    IDDToDI(ADI.IdentifyDeviceData,ADI);

    Result:=True;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetData_SCSI_Miniport(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  buffer: array [0..sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS)] of ansichar;
  scsiAddr: TSCSIAddress absolute Buffer;
  p: SRB_IO_CONTROL absolute buffer;
  pin: PSENDCMDINPARAMS;
  d: Cardinal;
  pOut: PSENDCMDOUTPARAMS;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(pin):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(@buffer)+sizeof(SRB_IO_CONTROL);

    ResetMemory(buffer,sizeof(buffer));
    p.HeaderLength:=sizeof(SRB_IO_CONTROL);
    p.Timeout:=10000;
    p.Length:=sizeof(SENDCMDOUTPARAMS);
    p.ControlCode:=IOCTL_SCSI_MINIPORT_IDENTIFY;
    p.Signature:='SCSIDISK';
    pin^.irDriveRegs.bCommandReg:=IDE_ATA_IDENTIFY;
    pin^.bDriveNumber:=ADI.Device mod 2;
    if not DeviceIoControl(hDevice,
                       IOCTL_SCSI_MINIPORT,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDINPARAMS)-1,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS),
                       d,
                       nil) then
      Exit;

    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(pOut):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(@buffer)+sizeof(SRB_IO_CONTROL);

    Move(pOut^.bBuffer,ADI.IdentifyDeviceData,SizeOf(TIdentifyDeviceData));
    {$IFDEF DEBUG}
    Move(pOut.bBuffer,ADI._RawData,SizeOf(TRawData));
    {$ENDIF}
    IDDToDI(ADI.IdentifyDeviceData,ADI);
    Result:=True;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetData_SCSI_PassThrough(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  dwReturned: Cardinal;
  len: Cardinal;
  Buffer: array[0..SizeOf(TScsiPassThroughWithBuffers)+SizeOf(TScsiPassThrough)-1] of Byte;
  sptwb: TScsiPassThroughWithBuffers absolute Buffer;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  ResetMemory(Buffer,SizeOf(Buffer));
  ResetMemory(sptwb,SizeOf(TScsiPassThroughWithBuffers));
  with sptwb.spt do begin
    Length:=SizeOf(TScsiPassThrough);
    CdbLength:=CDB6GENERIC_LENGTH;
    SenseInfoLength:=24;
    DataIn:=SCSI_IOCTL_DATA_IN;
    DataTransferLength:=192;
    TimeOutValue:=2;
    DataBufferOffset:=PAnsiChar(@sptwb.bDataBuf)-PAnsiChar(@sptwb);
    SenseInfoOffset:=PAnsiChar(@sptwb.bSenseBuf)-PAnsiChar(@sptwb);
    Cdb[0]:=SCSIOP_INQUIRY;
    Cdb[4]:=192; // AllocationLength
  end;
  len:=sptwb.spt.DataBufferOffset+sptwb.spt.DataTransferLength;
  if DeviceIoControl(hDevice,
                     IOCTL_SCSI_PASS_THROUGH,
                     @sptwb,
                     SizeOf(TScsiPassThrough),
                     @sptwb,
                     len,
                     dwReturned,
                     nil) {and (sptwb.spt.ScsiStatus=0)} then begin

    {$IFDEF DEBUG}
    Move(sptwb.bDataBuf,ADI._RawData,SizeOf(TRawData));
    {$ENDIF}

    SetString(ADI.Model,PAnsiChar(@sptwb.bDataBuf)+8,24);
    ADI.Model:=StripUnprintable(ADI.Model,' ');
    SetString(ADI.Revision,PAnsiChar(@sptwb.bDataBuf)+32,4);
    //SetString(ADI.SerialNumber,PAnsiChar(@sptwb.bDataBuf)+36,20);
    ADI.SerialNumber:=CorrectSerial(PByteArray(@sptwb.bDataBuf[36]),20);

    ADI.Model:=StripSpaces(Trim(ADI.Model));
    ADI.Revision:=Trim(ADI.Revision);
    ADI.SerialNumber:=StripUnprintable(Trim(ADI.SerialNumber));

    Result:=True;
  end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetData_DeviceDescriptor(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  dwReturned: Cardinal;
  inbuf: array[0..1023] of Byte;
  query: STORAGE_PROPERTY_QUERY absolute inbuf;
  outbuf: array[0..10230] of Byte;
  sdd: STORAGE_DEVICE_DESCRIPTOR absolute outbuf;
  dspd: TDeviceSeekPenaltyDescriptor;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
    Zeromemory(@outbuf,SizeOf(outbuf));
    Zeromemory(@inbuf,SizeOf(inbuf));
    query.PropertyId:=Ord(StorageDeviceProperty);
    query.QueryType:=Ord(PropertyStandardQuery);
    if DeviceIoControl(hDevice,IOCTL_STORAGE_QUERY_PROPERTY,@inbuf,SizeOf(inbuf),@outbuf,SizeOf(outbuf),dwReturned,nil) then begin

    {$IFDEF DEBUG}
    Move(outbuf,ADI._RawData,SizeOf(TRawData));
    {$ENDIF}

      if (ADI.Model='') then begin
        if (sdd.VendorIdOffset>0) then
          ADI.Model:=Trim(PAnsiChar(@outbuf[sdd.VendorIdOffset]));
        if (sdd.ProductIdOffset>0) then
          ADI.Model:=ADI.Model+' '+Trim(PAnsiChar(@outbuf[sdd.ProductIdOffset]));
      end;
      ADI.Model:=StripSpaces(Trim(ADI.Model));

      if (ADI.Revision='') and (sdd.ProductRevisionOffset>0) then
        ADI.Revision:=Trim(PAnsiChar(@outbuf[sdd.ProductRevisionOffset]));
      if (ADI.SerialNumber='') and (sdd.SerialNumberOffset<SizeOf(outbuf)) and (sdd.SerialNumberOffset>0) then begin
        //CorrectDataBuffer(PAnsiChar(@outbuf[sdd.SerialNumberOffset]));
        //ADI.SerialNumber:=StripUnprintable(Trim({CorrectSerial(}PAnsiChar(@outbuf[sdd.SerialNumberOffset]{)})));
        ADI.SerialNumber:=CorrectSerial(PByteArray(@outbuf[sdd.SerialNumberOffset]),SizeOf(outbuf)-sdd.SerialNumberOffset);
      end;

      ADI.BusType:=sdd.BusType;
      if ADI.DeviceType=0 then
        ADI.DeviceType:=sdd.DeviceType;
      ADI.Removable:=sdd.RemovableMedia;
      Result:=True;
    end;

    Zeromemory(@dspd,SizeOf(dspd));
    Zeromemory(@inbuf,SizeOf(inbuf));
    query.PropertyId:=Ord(StorageDeviceSeekPenaltyProperty);
    query.QueryType:=Ord(PropertyStandardQuery);
    if DeviceIoControl(hDevice,IOCTL_STORAGE_QUERY_PROPERTY,@inbuf,SizeOf(inbuf),@dspd,SizeOf(dspd),dwReturned,nil) then
      ADI.HasNoSeekPenalty:=not dspd.IncursSeekPenalty;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;


function GetUSBSerialNumber(hDevice: THandle): string;
var
  dwReturned: Cardinal;
  len: Cardinal;
  Buf: array[0..SizeOf(TScsiPassThroughWithBuffers)+SizeOf(TScsiPassThrough)-1] of byte;
  msn: MEDIA_SERIAL_NUMBER_DATA absolute Buf;
begin
  Result:='';
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  len:=SizeOf(Buf);
  if DeviceIoControl(hDevice,
                     IOCTL_STORAGE_GET_MEDIA_SERIAL_NUMBER,
                     nil,
                     0,
                     @msn,
                     len,
                     dwReturned,nil) then begin
    Result:=Trim(string(PAnsiChar(@msn.SerialNumberData)));
  end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceNumber(hDevice: THandle; var ADeviceNumber: Cardinal): Boolean;
var
  dwReturned: Cardinal;
  sdn: STORAGE_DEVICE_NUMBER;
begin
  ADeviceNumber:=0;
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
    if DeviceIoControl(hDevice,IOCTL_STORAGE_GET_DEVICE_NUMBER,nil,0,@sdn,SizeOf(sdn),dwReturned,nil) then begin
      ADeviceNumber:=LongInt(sdn.DeviceNumber);
      Result:=True;
    end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceLength(hDevice: THandle; var ALength: int64): Boolean;
var
  n: Cardinal;
  gli: TGetLengthInformation;
begin
  ALength:=0;
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
    if DeviceIoControl(hDevice,IOCTL_DISK_GET_LENGTH_INFO,nil,0,@gli,SizeOf(gli),n,nil) then begin
      ALength:=gli.Length.QuadPart;
      Result:=True;
    end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceLocation(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  dwReturned: Cardinal;
  Buffer: array[0..SizeOf(TScsiPassThroughWithBuffers)+SizeOf(TScsiPassThrough)-1] of Byte;
  scsiAddr: TSCSIAddress absolute Buffer;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  ResetMemory(Buffer,SizeOf(Buffer));
  scsiAddr.Length:=SizeOf(TSCSIAddress);
  if DeviceIoControl(hDevice,IOCTL_SCSI_GET_ADDRESS,nil,0,@scsiAddr,SizeOf(TSCSIAddress),dwReturned,nil) then begin
    ADI.HaId:=scsiAddr.PortNumber;
    ADI.Target:=scsiAddr.TargetId;
    ADI.Lun:=scsiAddr.Lun;
    ADI.PathId:=scsiAddr.PathId;
    Result:=True;
  end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceMediaType(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  dwReturned: Cardinal;
  len: Cardinal;
  Buf: array[0..SizeOf(TScsiPassThroughWithBuffers)+SizeOf(TScsiPassThrough)-1] of Byte;
  mt: TGetMediaTypes absolute Buf;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  len:=SizeOf(Buf);
  if DeviceIoControl(hDevice,IOCTL_STORAGE_GET_MEDIA_TYPES_EX,nil,0,@mt,len,dwReturned,nil) then begin
    //if ADI.DeviceType=0 then
      ADI.DeviceType:=mt.DeviceType;
    case mt.DeviceType of
      FILE_DEVICE_CD_ROM,
      FILE_DEVICE_DVD: ADI.MediaType:=mt.MediaInfo[0].RemovableDiskInfo.MediaType;
      FILE_DEVICE_TAPE: begin
        ADI.MediaType:=mt.MediaInfo[0].TapeInfo.MediaType;
        ADI.BusType:=mt.MediaInfo[0].TapeInfo.BusType;
      end;
      else
        ADI.MediaType:=mt.MediaInfo[0].DiskInfo.MediaType;
    end;
    Result:=True;
  end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceCapabilities(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  dwReturned: Cardinal;
  len: Cardinal;
  Buffer: array[0..SizeOf(TScsiPassThroughWithBuffers)+SizeOf(TScsiPassThrough)-1] of Byte;
  sptwb: TScsiPassThroughWithBuffers absolute Buffer;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
  ResetMemory(Buffer,SizeOf(Buffer));
  with sptwb.spt do begin
    Length:=SizeOf(TScsiPassThrough);
    CdbLength:=CDB6GENERIC_LENGTH;
    SenseInfoLength:=24;
    DataIn:=SCSI_IOCTL_DATA_IN;
    DataTransferLength:=192;
    TimeOutValue:=2;
    DataBufferOffset:=PAnsiChar(@sptwb.bDataBuf)-PAnsiChar(@sptwb);
    SenseInfoOffset:=PAnsiChar(@sptwb.bSenseBuf)-PAnsiChar(@sptwb);
    Cdb[0]:=SCSIOP_MODE_SENSE;
    Cdb[1]:=$08;                    // target shall not return any block descriptors
    Cdb[2]:=MODE_PAGE_CAPABILITIES;
    Cdb[4]:=192; // AllocationLength
  end;
  len:=sptwb.spt.DataBufferOffset+sptwb.spt.DataTransferLength;
  if DeviceIoControl(hDevice,IOCTL_SCSI_PASS_THROUGH,@sptwb,SizeOf(TScsiPassThrough),@sptwb,len,dwReturned,nil) then begin
    ADI.Read_CDR:=sptwb.bDataBuf[6] and $01 = $01;
    ADI.Read_CDRW:=sptwb.bDataBuf[6] and $02 = $02;
    ADI.Write_CDR:=sptwb.bDataBuf[7] and $01 = $01;
    ADI.Write_CDRW:=sptwb.bDataBuf[7] and $02 = $02;
    ADI.Read_DVDROM:=sptwb.bDataBuf[6] and $08 = $08;
    ADI.Read_DVDR:=sptwb.bDataBuf[6] and $10 = $10;
    ADI.Read_DVDRAM:=sptwb.bDataBuf[6] and $20 = $20;
    ADI.Write_DVDR:=sptwb.bDataBuf[7] and $10 = $10;
    ADI.Write_DVDRAM:=sptwb.bDataBuf[7] and $20 = $20;
    Result:=True;
  end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceSMARTAttrs(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  n,i: Cardinal;
  pInData: PSendCmdInParams;
  pOutData: Pointer; // PDriveAttributes
  Buffer: array[0..BufferSize-1] of Byte;
  srbControl: TSrbIoControl absolute Buffer;
begin
  Finalize(ADI.SMART);
  ADI.Temperature:=0;
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  ResetMemory(Buffer,BufferSize);
  try
    pInData:=PSendCmdInParams(@Buffer);
    pOutData:=@pInData^.bBuffer;
    with pInData^ do begin
      cBufferSize:=READ_ATTRIBUTE_BUFFER_SIZE;
      bDriveNumber:=ADI.Number;
      with irDriveRegs do begin
        bFeaturesReg:=SMART_ENABLE_SMART_OPERATIONS;
        bSectorCountReg:=1;
        bSectorNumberReg:=1;
        bCylLowReg:=SMART_CYL_LOW;
        bCylHighReg:=SMART_CYL_HI;
        bDriveHeadReg:=$A0 or ((bDriveNumber And 1)*(2*2*2*2));
        bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
      end;
    end;

    if not DeviceIoControl(hDevice,SMART_SEND_DRIVE_COMMAND,pInData,SizeOf(TSendCmdInParams)-1,pOutData,W9xBufferSize,n,nil) then
      n:=0;
  finally
    LastIOCTLError:=GetLastError;
  end;

  if n>0 then
  try
    ADI.SMARTSupport:=True;
    ADI.SMARTActive:=True;
    pInData:=PSendCmdInParams(@Buffer);
    pOutData:=@pInData^.bBuffer;
    with pInData^ do begin
      cBufferSize:=READ_ATTRIBUTE_BUFFER_SIZE;
      bDriveNumber:=ADI.Number;
      with irDriveRegs do begin
        bFeaturesReg:=SMART_READ_ATTRIBUTE_VALUES;
        bSectorCountReg:=1;
        bSectorNumberReg:=1;
        bCylLowReg:=SMART_CYL_LOW;
        bCylHighReg:=SMART_CYL_HI;
        bDriveHeadReg:=$A0 or ((bDriveNumber And 1)*(2*2*2*2));
        bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
      end;
    end;
    if DeviceIoControl(hDevice,SMART_RCV_DRIVE_DATA,pInData,SizeOf(TSendCmdInParams)-1,pOutData,W9xBufferSize,n,nil) then begin
      for i:=0 to NUM_ATTRIBUTE_STRUCTS-1 do begin
        //if PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bAttrID>0 then begin
          SetLength(ADI.SMART,Length(ADI.SMART)+1);
          ADI.SMART[High(ADI.SMART)]:=PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^;
          if PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bAttrID in [ATTR_TEMPERATURE,ATTR_AIR_FLOW_TEMPERATURE,ATTR_TEMPERATUREII] then
            ADI.Temperature:=PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bRawValue[0];
        end;
      Result:=True;
    end;

    with pInData^ do begin
      cBufferSize:=READ_ATTRIBUTE_BUFFER_SIZE;
      bDriveNumber:=ADI.Number;
      with irDriveRegs do begin
        bFeaturesReg:=SMART_READ_ATTRIBUTE_THRESHOLDS;
        bSectorCountReg:=1;
        bSectorNumberReg:=1;
        bCylLowReg:=SMART_CYL_LOW;
        bCylHighReg:=SMART_CYL_HI;
        bDriveHeadReg:=$A0 or ((bDriveNumber And 1)*(2*2*2*2));
        bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
      end;
    end;
    if DeviceIoControl(hDevice,SMART_RCV_DRIVE_DATA,pInData,SizeOf(TSendCmdInParams)-1,pOutData,W9xBufferSize,n,nil) then begin
      for i:=0 to NUM_ATTRIBUTE_STRUCTS-1 do begin
        SetLength(ADI.SMARTThresholds,Length(ADI.SMARTThresholds)+1);
        ADI.SMARTThresholds[High(ADI.SMARTThresholds)]:=PAttrThreshold(PAnsiChar(pOutData)+18+12*i)^;
      end;
      Result:=True;
    end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDeviceSMARTAttrs2(hDevice: THandle; var ADI: TDeviceInfo): Boolean;
var
  buffer: array [0..sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS)] of ansichar;
  scsiAddr: TSCSIAddress absolute Buffer;
  p: SRB_IO_CONTROL absolute buffer;
  pin: PSENDCMDINPARAMS;
  d,i: Cardinal;
  pOut: PSENDCMDOUTPARAMS;
begin
  Result:=False;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  try
    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(pin):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(@buffer)+sizeof(SRB_IO_CONTROL);

    ResetMemory(buffer,sizeof(buffer));
    p.HeaderLength:=sizeof(SRB_IO_CONTROL);
    p.Timeout:=10000;
    p.Length:=sizeof(SENDCMDOUTPARAMS);
    p.ControlCode:=IOCTL_SCSI_MINIPORT_READ_SMART_ATTRIBS; //IOCTL_SCSI_MINIPORT_ENABLE_SMART
    p.Signature:='SCSIDISK';
    with pin^.irDriveRegs do begin
      bFeaturesReg:=SMART_ENABLE_SMART_OPERATIONS;
      bSectorCountReg:=1;
      bSectorNumberReg:=1;
      bCylLowReg:=SMART_CYL_LOW;
      bCylHighReg:=SMART_CYL_HI;
      if ADI.Number and 1 = 1 then
        bDriveHeadReg:=$B0
      else
        bDriveHeadReg:=$A0;
      if Win32Platform<>VER_PLATFORM_WIN32_NT then
        bDriveHeadReg:=bDriveHeadReg or (ADI.Device and 1)*16;
      bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
    end;
    pin^.bDriveNumber:=0;
    pin^.cBufferSize:=512;
    if not DeviceIoControl(hDevice,
                       IOCTL_SCSI_MINIPORT,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDINPARAMS)-1,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS),
                       d,
                       nil) then
      Exit;

    {$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(pOut):={$IFDEF WIN64}NativeUInt{$ELSE}Cardinal{$ENDIF}(@buffer)+sizeof(SRB_IO_CONTROL);

    ADI.SMARTSupport:=True;
    ADI.SMARTActive:=True;

    ResetMemory(buffer,sizeof(buffer));
    p.HeaderLength:=sizeof(SRB_IO_CONTROL);
    p.Timeout:=10000;
    p.Length:=sizeof(SENDCMDOUTPARAMS);
    p.ControlCode:=IOCTL_SCSI_MINIPORT_READ_SMART_ATTRIBS; //IOCTL_SCSI_MINIPORT_ENABLE_SMART
    p.Signature:='SCSIDISK';
    with pin^.irDriveRegs do begin
      bFeaturesReg:=SMART_READ_ATTRIBUTE_VALUES; //SMART_ENABLE_SMART_OPERATIONS;
      bSectorCountReg:=1;
      bSectorNumberReg:=1;
      bCylLowReg:=SMART_CYL_LOW;
      bCylHighReg:=SMART_CYL_HI;
      if ADI.Number and 1 = 1 then
        bDriveHeadReg:=$B0
      else
        bDriveHeadReg:=$A0;
      if Win32Platform<>VER_PLATFORM_WIN32_NT then
        bDriveHeadReg:=bDriveHeadReg or (ADI.Device and 1)*16;
      bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
    end;
    pin^.bDriveNumber:=0;
    pin^.cBufferSize:=512;
    if DeviceIoControl(hDevice,
                       IOCTL_SCSI_MINIPORT,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDINPARAMS)-1,
                       @buffer,
                       sizeof(SRB_IO_CONTROL)+sizeof(SENDCMDOUTPARAMS),
                       d,
                       nil) then begin
      for i:=0 to NUM_ATTRIBUTE_STRUCTS-1 do
        if PDriveAttribute(PAnsiChar(pOut)+18+12*i)^.bAttrID>0 then begin
          SetLength(ADI.SMART,Length(ADI.SMART)+1);
          ADI.SMART[High(ADI.SMART)]:=PDriveAttribute(PAnsiChar(pOut)+18+12*i)^;
          if PDriveAttribute(PAnsiChar(pOut)+18+12*i)^.bAttrID in [ATTR_TEMPERATURE,ATTR_AIR_FLOW_TEMPERATURE,ATTR_TEMPERATUREII] then begin
            ADI.Temperature:=PDriveAttribute(PAnsiChar(pOut)+18+12*i)^.bRawValue[0];
          end;
        end;
      Result:=True;
    end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

function GetDiskLetterFromDeviceNumber(AValue: Cardinal): string;
var
  i: Integer;
  s: string;
  h: THandle;
  dn: Cardinal;
begin
  Result:='';
  s:=GetLogicalDisks;
  for i:=1 to Length(s) do begin
    h:=GetHandle_LogicalDrive(Ord(s[i]));
    try
      GetDeviceNumber(h,dn);
      if dn=AValue then begin
        Result:=s[i];
        Break;
      end;
    finally
      CloseHandle(h);
    end;
  end;
end;

function GetSymbolicLinkFromDeviceNumber(AValue: Cardinal): string;
var
  sl: TStringList;
begin
  sl:=TStringlist.Create;
  try
    GetSymbolicLinkTable(sl);
    Result:=sl.Values[IntToStr(AValue)];
  finally
    sl.Free;
  end;
end;

procedure GetSymbolicLinkTable(ATable: TStringlist);
var
  guid: TGUID;
  hdev: HDEVINFO;
  did: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  s: string;
  h: THandle;
  dn,n: Cardinal;
  i: integer;
begin
  ATable.Clear;
  guid:=GUID_DEVINTERFACE_DISK;
  hdev:=SetupDiGetClassDevs(@guid,nil,0,DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then begin
    try
      for i:=0 to 99 do begin
        FillChar(did,SizeOf(did),0);
        did.cbSize:=SizeOf(did);
        if (SetupDiEnumDeviceInterfaces(hdev,nil,guid,i,did)) then begin
          n:=0;
          SetupDiGetDeviceInterfaceDetail(hdev,@did,nil,0,n,nil);
          if (ERROR_INSUFFICIENT_BUFFER=GetLastError) then begin
            pdidd:=AllocMem(n);
            try
              pdidd^.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
              if (SetupDiGetDeviceInterfaceDetail(hdev,@did,pdidd,n,n,nil)) then begin
                s:=PChar(@(pdidd^.DevicePath));
                if (Trim(s)<>'') and (Trim(s)<>'\') then begin
                  h:=GetHandle_Symbolic(s);
                  try
                    GetDeviceNumber(h,dn);
                    ATable.Add(Format('%d=%s',[dn,s]));
                  finally
                    CloseHandle(h);
                  end;
                end;
              end;
            finally
              FreeMem(pdidd);
            end;
          end;
        end else if GetLastError=ERROR_NO_MORE_ITEMS then
          Break;
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
  end;
end;

function GetDiskTemp(hDevice: THandle): integer;
var
  n,i: Cardinal;
  pInData: PSendCmdInParams;
  pOutData: Pointer; // PDriveAttributes
  Buffer: array[0..BufferSize-1] of Byte;
  srbControl: TSrbIoControl absolute Buffer;
begin
  Result:=0;
  if hDevice=INVALID_HANDLE_VALUE then
    Exit;
  ResetMemory(Buffer,BufferSize);
  try
    pInData:=PSendCmdInParams(@Buffer);
    pOutData:=@pInData^.bBuffer;
    with pInData^ do begin
      cBufferSize:=READ_ATTRIBUTE_BUFFER_SIZE;
      bDriveNumber:=0;
      with irDriveRegs do begin
        bFeaturesReg:=SMART_ENABLE_SMART_OPERATIONS;
        bSectorCountReg:=1;
        bSectorNumberReg:=1;
        bCylLowReg:=SMART_CYL_LOW;
        bCylHighReg:=SMART_CYL_HI;
        bDriveHeadReg:=$A0;
        bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
      end;
    end;

    if not DeviceIoControl(hDevice,SMART_SEND_DRIVE_COMMAND,pInData,SizeOf(TSendCmdInParams)-1,pOutData,W9xBufferSize,n,nil) then
      n:=0;
  finally
    LastIOCTLError:=GetLastError;
  end;

  if n>0 then
  try
    pInData:=PSendCmdInParams(@Buffer);
    pOutData:=@pInData^.bBuffer;
    with pInData^ do begin
      cBufferSize:=READ_ATTRIBUTE_BUFFER_SIZE;
      bDriveNumber:=0;
      with irDriveRegs do begin
        bFeaturesReg:=SMART_READ_ATTRIBUTE_VALUES;
        bSectorCountReg:=1;
        bSectorNumberReg:=1;
        bCylLowReg:=SMART_CYL_LOW;
        bCylHighReg:=SMART_CYL_HI;
        bDriveHeadReg:=$A0;
        bCommandReg:=IDE_EXECUTE_SMART_FUNCTION;
      end;
    end;
    if DeviceIoControl(hDevice,SMART_RCV_DRIVE_DATA,pInData,SizeOf(TSendCmdInParams)-1,pOutData,W9xBufferSize,n,nil) then begin
      for i:=0 to NUM_ATTRIBUTE_STRUCTS-1 do
        if PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bAttrID>0 then begin
          if PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bAttrID in [ATTR_TEMPERATURE,ATTR_TEMPERATUREII,ATTR_AIR_FLOW_TEMPERATURE] then begin
            Result:=PDriveAttribute(PAnsiChar(pOutData)+18+12*i)^.bRawValue[0];
          end;
        end;
    end;
  finally
    LastIOCTLError:=GetLastError;
  end;
end;

procedure GetDeviceData(hdevice: THandle; var ADI: TDeviceInfo; DetectSMART: Boolean);
begin
  try GetDeviceLocation(hDevice,ADI) except end;
  try GetDeviceMediaType(hDevice,ADI) except end;
  try GetDeviceGeometry(hDevice,ADI.Geometry) except end;
  try GetDeviceLayout(hDevice,ADI.Layout) except end;
  try GetDeviceCapabilities(hDevice,ADI) except end;
  if DetectSMART then begin
    try GetDeviceSMARTAttrs(hDevice,ADI) except end;
    if not ADI.SMARTSupport then
      try GetDeviceSMARTAttrs2(hDevice,ADI) except end;
  end;
  try GetDiskExtents(hDevice,ADI.DiskExtents) except end;
  ADI.Size:=ADI.Geometry.Cylinders.QuadPart*
            ADI.Geometry.TracksPerCylinder*
            ADI.Geometry.SectorsPerTrack*
            ADI.Geometry.BytesPerSector;
  if ADI.Size=0 then
    ADI.Size:=ADI.IdentifyDeviceData.Max48BitLBA[0]*512;
end;

procedure IDDToDI(AData: TIdentifyDeviceData; var ADI: TDeviceInfo);
var
  i: Integer;
  c: Char;
  s: string;
begin
  CorrectDataBuffer(@AData);

  ADI.Model:=Trim(string(AData.ModelNumber));
  ADI.Model:=StripSpaces(ADI.Model);

  i:=1;
  s:=ADI.Model;
  while i<=Length(s) do begin
    c:=s[i];
    if (c in [#0..#31]) then begin
      Delete(s,i,1);
      Insert('_',s,i);
    end;
    Inc(i);
  end;

  if not SameText(ADI.Model,s) then begin
    ADI.Model:='';
    Exit;
  end;

  ADI.Revision:=Trim(string(AData.FirmwareRevision));
  ADI.SerialNumber:=StripUnprintable(Trim(string(AData.SerialNumber)));

  ADI.SMARTSupport:=False;
  if ((PWordArray(@AData)^[83] shr 14)=1) then
    ADI.SMARTSupport:=PWordArray(@AData)^[82] and 1>0;

  ADI.SMARTActive:=False;
  if ADI.SMARTSupport then
    if ((PWordArray(@AData)^[87] shr 14)=1) then
      ADI.SMARTActive:=PWordArray(@AData)^[85] and 1>0;

  ADI.CtlBufSize:=AData.Retired2[1] shl 9;
  ADI.ECCCode:=AData.Obsolete1;
end;

function GetATAMajorVersion(AValue: word): string;
begin
  Result:='';
  if AValue and 32 > 0 then
    Result:=Result+'ATA/ATAPI-5, ';
  if AValue and 64 > 0 then
    Result:=Result+'ATA/ATAPI-6, ';
  if AValue and 128 > 0 then
    Result:=Result+'ATA/ATAPI-7, ';
  if AValue and 256 > 0 then
    Result:=Result+'ATA8-ACS, ';
  if AValue and 512 > 0 then
    Result:=Result+'ACS-2, ';
  if AValue and 1024 > 0 then
    Result:=Result+'ACS-3, ';
  SetLength(Result,Length(Result)-2);
end;

function GetATAMinorVersion(AValue: word): string;
begin
  if AValue=$13 then
    Result:=' ATA/ATAPI-5 T13 1321D version 3'
  else if AValue=$15 then
    Result:='ATA/ATAPI-5 T13 1321D version 1'
  else if AValue=$16 then
    Result:='ATA/ATAPI-5 published, ANSI INCITS 340-2000'
  else if AValue=$18 then
    Result:='ATA/ATAPI-6 T13 1410D version 0'
  else if AValue=$19 then
    Result:='ATA/ATAPI-6 T13 1410D version 3a'
  else if AValue=$1A then
    Result:=' ATA/ATAPI-7 T13 1532D version 1'
  else if AValue=$1B then
    Result:='ATA/ATAPI-6 T13 1410D version 2'
  else if AValue=$1C then
    Result:='ATA/ATAPI-6 T13 1410D version 1'
  else if AValue=$1D then
    Result:='ATA/ATAPI-7 published ANSI INCITS 397-2005'
  else if AValue=$1E then
    Result:='ATA/ATAPI-7 T13 1532D version 0'
  else if AValue=$1F then
    Result:='ACS-3 Revision 3b'
  else if AValue=$21 then
    Result:='ATA/ATAPI-7 T13 1532D version 4a'
  else if AValue=$22 then
    Result:='ATA/ATAPI-6 published, ANSI INCITS 361-2002'
  else if AValue=$27 then
    Result:='ATA8-ACS version 3c'
  else if AValue=$28 then
    Result:='ATA8-ACS version 6'
  else if AValue=$29 then
    Result:='ATA8-ACS version 4'
  else if AValue=$31 then
    Result:='ACS-2 Revision 2'
  else if AValue=$33 then
    Result:='ATA8-ACS version 3e'
  else if AValue=$39 then
    Result:='ATA8-ACS version 4c'
  else if AValue=$42 then
    Result:='ATA8-ACS version 3f'
  else if AValue=$52 then
    Result:='ATA8-ACS version 3b'
  else if AValue=$6D then
    Result:='ACS-3 Revision 5'
  else if AValue=$82 then
    Result:='ACS-2 published, ANSI INCITS 482-2012'
  else if AValue=$107 then
    Result:='ATA8-ACS version 2d'
  else if AValue=$110 then
    Result:='ACS-2 Revision 3'
  else if AValue=$11B then
    Result:='ACS-3 Revision 4'
  else
    Result:='?';
end;

function GetATATransportVersion(AValue: word): string;
begin
  Result:='';
  case (AValue shr 12) and 15 of
    0: if AValue and 2 > 0 then
         Result:='ATA8-APT'
       else if AValue and 1 > 0 then
         Result:='ATA/ATAPI-7';
    1: if AValue and 64 > 0 then
         Result:='SATA 3.1'
       else if AValue and 32 > 0 then
         Result:='SATA 3.0'
       else if AValue and 16 > 0 then
         Result:='SATA 2.6'
       else if AValue and 8 > 0 then
         Result:='SATA 2.5'
       else if AValue and 4 > 0 then
         Result:='SATA II: Extensions'
       else if AValue and 2 > 0 then
         Result:='SATA 1.0a'
       else if AValue and 1 > 0 then
         Result:='ATA8-AST';
  end;
end;

procedure AddInfoToDI(Source: TDeviceInfo; var Dest: TDeviceInfo);
{var
  i: Integer;}
begin
  if not Dest.Removable then
    Dest.Removable:=Source.Removable;
  if Dest.DeviceType=FILE_DEVICE_UNKNOWN then
    Dest.DeviceType:=Source.DeviceType;
  if Dest.MediaType=0 then
    Dest.MediaType:=Source.MediaType;
  if (Length(Trim(Dest.SerialNumber))<5) and (Length(Trim(Source.SerialNumber))>Length(Trim(Dest.SerialNumber))) then
    Dest.SerialNumber:=Source.SerialNumber;
  if (Trim(Dest.VolumeLabel)='') then
    Dest.VolumeLabel:=Source.VolumeLabel;
  if (Trim(Dest.Model)='') then
    Dest.Model:=Source.Model;
  if (Trim(Dest.LogicalSerial)='') then
    Dest.LogicalSerial:=Source.LogicalSerial;
  if Trim(Dest.Revision)='' then
    Dest.Revision:=Source.Revision;
  if Dest.Number=0 then
    Dest.Number:=Source.Number;
  if Dest.BusType=0 then
    Dest.BusType:=Source.BusType;
  if Dest.HaId=-1 then
    Dest.HaId:=Source.HaId;
  if Dest.PathId=-1 then
    Dest.PathId:=Source.PathId;
  if Dest.Target=-1 then
    Dest.Target:=Source.Target;
  if Dest.Lun=-1 then
    Dest.Lun:=Source.Lun;
  if Dest.LayoutIndex=-1 then
    Dest.LayoutIndex:=Source.LayoutIndex;
  if Dest.CtlBufSize=0 then
    Dest.CtlBufSize:=Source.CtlBufSize;
  if Dest.ECCCode=0 then
    Dest.ECCCode:=Source.ECCCode;
  if not Dest.SMARTSupport then
    Dest.SMARTSupport:=Source.SMARTSupport;
  if not Dest.SMARTActive then
    Dest.SMARTActive:=Source.SMARTActive;
  if not Dest.HasNoSeekPenalty then
    Dest.HasNoSeekPenalty:=Source.HasNoSeekPenalty;
  if not Dest.SSD then
    Dest.SSD:=Source.SSD;
  if not Dest.Write_CDR then
    Dest.Write_CDR:=Source.Write_CDR;
  if not Dest.Write_CDRW then
    Dest.Write_CDRW:=Source.Write_CDRW;
  if not Dest.Write_DVDR then
    Dest.Write_DVDR:=Source.Write_DVDR;
  if not Dest.Write_DVDRAM then
    Dest.Write_DVDRAM:=Source.Write_DVDRAM;
  if not Dest.Read_CDR then
    Dest.Read_CDR:=Source.Read_CDR;
  if not Dest.Read_CDRW then
    Dest.Read_CDRW:=Source.Read_CDRW;
  if not Dest.Read_DVDROM then
    Dest.Read_DVDROM:=Source.Read_DVDROM;
  if not Dest.Read_DVDR then
    Dest.Read_DVDR:=Source.Read_DVDR;
  if not Dest.Read_DVDRAM then
    Dest.Read_DVDRAM:=Source.Read_DVDRAM;
  if Length(Dest.SMART)=0 then
    Dest.SMART:=Source.SMART;
  if Dest.Temperature=0 then
    Dest.Temperature:=Source.Temperature;
  if Dest.FileSystem='' then
    Dest.FileSystem:=Source.FileSystem;
  if Dest.ClusterSize=0 then
    Dest.ClusterSize:=Source.ClusterSize;
  if Dest.LengthInBytes=0 then
    Dest.LengthInBytes:=Source.LengthInBytes;
  if Dest.LogicalSize=0 then
    Dest.LogicalSize:=Source.LogicalSize;

  {if Length(Dest.Layout)=0 then begin
    SetLength(Dest.Layout,Length(Source.Layout));
    for i:=0 to High(Source.Layout) do
      Dest.Layout[i]:=Source.Layout[i];
  end;}
end;

function DecodeSerial(AValue: string): string;
var
  i,p,b: Integer;
  s: AnsiString;
begin
  Result:='';
  AValue:=ExtractFilename(ExcludeTrailingPathDelimiter(ExtractFilePath(FastStringReplace(AValue,'#','\'))));
  p:=Pos('&',AValue);
  if p>0 then
    AValue:=Copy(AValue,1,p-1);
  s:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(AValue);

  i:=1;
  while i<Length(s) do begin
    if TryStrtoInt('$'+Copy(s,i,2),b) and (b in [32..127]) then begin
      Result:=Result+Chr(b);
      inc(i,2);
    end else begin
      Result:=s;
      break;
    end;
  end;
end;

function IsPortable: Boolean;
var
  di: TDiskInfo;
  dv: TDeviceInfo;
  h: THandle;
  s: string;
begin
  s:=ExtractFileDrive(ParamStr(0));
  di:=GetDiskInfo(s);
  Result:=(di.MediaType=dtRemovable);
  if not Result and (di.MediaType<>dtCDROM) then begin
    h:=GetHandle_LogicalDisk(s);
    if h<>0 then
      try
        Result:=GetData_DeviceDescriptor(h,dv) and (dv.BusType=bustypeUSB);
      finally
        CloseHandle(h);
      end;
  end;
end;

end.


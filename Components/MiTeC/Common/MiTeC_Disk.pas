{*******************************************************}
{                 MiTeC Common Routines                 }
{                 Low-Level Disk Access                 }
{                                                       }
{         Copyright (c) 1997-2016 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_Disk;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     {$ELSE}
     Windows, SysUtils, Classes,
     {$ENDIF}
     MiTeC_WinIOCTL;

const
  PartitionTableOffset = $1BE;

type
  TPartitionDescriptor = record
    BootIndicator: Byte;
    BeginCHS: array [1..3] of Byte;
    PartitionType: Byte;
    EndCHS: array [1..3] of Byte;
    StartSector: Cardinal;
    SectorCount: Cardinal;
  end;

  TPartitionTable = array [1..4] of TPartitionDescriptor;

  TDiskAccess = class(TStream)
  private
    FDH: Cardinal;
    FDN: Byte;
    FSize,FTS: Int64;
    FPT: TPartitionTable;
  public
    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64; override;
    function Read(var Buffer; Count: Longint): Longint; override;
    function Write(const Buffer; Count: Longint): Longint; override;
    constructor Create(ADiskNumber: Byte);
    destructor Destroy; override;

    property Handle: Cardinal read FDH;
    property DiskNumber: Byte read FDN;
    property TotalSize: Int64 read FSize;
    property TotalSectors: Int64 read FTS;
    property Partitions: TPartitionTable read FPT;
  end;

  TFAT32BootRecord = packed record
    BootCode: array [0..2] of Byte;
    OemName: array [0..7] of Char;
    BytesPerSec: Word;
    SecPerClust: Byte;
    ReservedSectors: Word;
    NumOfFatCopies: Byte;
    MaxRootEntries: Word;
    SectorsOld: Word;
    MediaDescriptor: Byte;
    SectorsPerFatOld: Word;
    SectorsPerTrack: Word;
    NumOfHeads: Word;
    HiddenSectors: Cardinal;
    SectorsSize: Cardinal;
    SectorsPerFat: Cardinal;
    Flags: Word;
    Version: Word;
    RootDirStartClust: Cardinal;
    FileSystemInfoSec: Word;
    BackupSec: Word;
    Reserved: array [0..11] of Byte;
    DriveNumber: Word;
    ExtraSignature: Byte;
    SerialNumber: Cardinal;
    VolumeName: array [0..10] of Char;
    FATName: array [0..7] of Char;
    Code: array [0..419] of Byte;
    BootSignature: Word;
  end;

  TFAT = (fat16, fat32);

  TFAT32 = class
  private
    FDA: TDiskAccess;
    FBS: TFAT32BootRecord;
    FFAT: TFAT;
    FSS,FDSS: Cardinal;
  public
    constructor Create(ADiskNumber: Byte);
    destructor Destroy; override;
    procedure ReadCluster(ANumber: Cardinal; AStream: TMemoryStream);
    function NextCluster(ACurrent: Cardinal): Cardinal;

    property FAT: TFAT read FFAT;
  end;

  TNTFSBootRecord = packed record
    BootCode: array [0..2] of Byte;
    OemName: array [0..7] of Char;
    BytesPerSec: Word;
    SecPerClust: Byte;
    ReservedSectors: Word;
    Unused0: array [0..4] of Byte;
    MediaDescriptor: Byte;
    Unused1: Word;
    SectorsPerTrack: Word;
    NumOfHeads: Word;
    HiddenSectors: Cardinal;
    Unused2: array [0..7] of Byte;
    SectorSize: Int64;
    MTFClust: Int64;
    MTFMirrotClust: Int64;
    ClustersPerFileRec: Cardinal;
    ClustersPerIndexBlock: Cardinal;
    SerialNumber: Int64;
    Checksum: Cardinal;
    Code: array [0..425] of Byte;
    BootSignature: Word;
  end;

  TNTFS = class
  private
    FDA: TDiskAccess;
    FBS: TNTFSBootRecord;
  public
    constructor Create(ADiskNumber: Byte);
    destructor Destroy; override;
    procedure ReadClusters(AStart,ACount: Cardinal; AStream: TMemoryStream);
  end;

implementation

var
  SectorBuffer: array [0..bBytesPerSector-1] of Byte;

constructor TDiskAccess.Create(ADiskNumber: Byte);
var
  DG: TDiskGeometry;
  n: Cardinal;
begin
  FDH:=CreateFile(PChar(Format('\\.\PhysicalDrive%d',[ADiskNumber])),GENERIC_READ,FILE_SHARE_READ,nil,OPEN_EXISTING,0,0);
  if FDH<>INVALID_HANDLE_VALUE then begin
    Position:=0;
    Read(SectorBuffer,SizeOf(SectorBuffer));
    Move(SectorBuffer[PartitionTableOffset],FPT,SizeOf(TPartitionTable));
    if DeviceIoControl(FDH,IOCTL_DISK_GET_DRIVE_GEOMETRY,nil,0,@DG,SizeOf(DG),n,nil) then begin
      FTS:=DG.Cylinders.LowPart*DG.TracksPerCylinder*DG.SectorsPerTrack;
      FSize:=FTS*bBytesPerSector;
    end else begin
      CloseHandle(FDH);
      raise EInOutError.Create('Cannot get disk geometry.');
    end;
  end else begin
    CloseHandle(FDH);
    raise EInOutError.Create('Cannot get disk access.');
  end;
end;

destructor TDiskAccess.Destroy;
begin
  CloseHandle(FDH);
end;

function TDiskAccess.Read(var Buffer; Count: Integer): Longint;
var
  BytesRead: Cardinal;
begin
  if Count mod bBytesPerSector<>0 then
     raise EInOutError.Create('Number of bytes to read must be aligned to 512 bytes.');
  if not ReadFile(FDH,Buffer,Count,BytesRead,nil) then
    RaiseLastOSError;
  Result:=BytesRead;
end;

function TDiskAccess.Seek(const Offset: Int64; Origin: TSeekOrigin): int64;
var
  HighOffset: Integer;
begin
  if Offset mod bBytesPerSector<>0 then
    raise EInOutError.Create('Number of bytes to read must be aligned to 512 bytes.');
  HighOffset:=(Offset shr 32) and $00000000FFFFFFFF;
  case Origin of
    soBeginning: Result:=SetFilePointer(FDH,Integer(Offset and $00000000FFFFFFFF),@HighOffset,FILE_BEGIN);
    soCurrent: Result:=SetFilePointer(FDH,Integer(Offset and $00000000FFFFFFFF),@HighOffset,FILE_CURRENT);
    soEnd: Result:=SetFilePointer(FDH,Integer(Offset and $00000000FFFFFFFF),@HighOffset,FILE_END);
    else Result:=0;
  end;
end;

function TDiskAccess.Write(const Buffer; Count: Integer): Longint;
begin
  raise EInOutError.Create('Read-only access granted.');
end;

{ TFAT32 }

constructor TFAT32.Create(ADiskNumber: Byte);
begin
  FDA:=TDiskAccess.Create(ADiskNumber);
  FSS:=FBS.ReservedSectors;
  if Pos('FAT32',FBS.FATName)=1 then begin
    FFAT:=fat32;
    FDSS:=FBS.ReservedSectors+(FBS.NumOfFatCopies*FBS.SectorsPerFat);
  end else begin
    FFAT:=fat16;
    FDSS:=FBS.ReservedSectors+(FBS.NumOfFatCopies*FBS.SectorsPerFatOld)+(FBS.MaxRootEntries*32 div bBytesPerSector);
  end;
end;

destructor TFAT32.Destroy;
begin
  FDA.Free;
  inherited;
end;

function TFAT32.NextCluster(ACurrent: Cardinal): Cardinal;
var
  Sector,
  Offset: Cardinal;
begin
  if FFAT=fat32 then begin
    Sector:=ACurrent div 128;
    Offset:=(ACurrent mod 128)*4;
    FDA.Position:=Int64(FSS+Sector)*Int64(bBytesPerSector);
    FDA.Read(SectorBuffer,SizeOf(SectorBuffer));
    Move(SectorBuffer[Offset],Result,4);
  end else begin
    Sector:=ACurrent div 256;
    Offset:=(ACurrent mod 256)*2;
    FDA.Position:=Int64(FSS+Sector)*Int64(bBytesPerSector);
    FDA.Read(SectorBuffer,SizeOf(SectorBuffer));
    Move(SectorBuffer[Offset],Result,2);
  end;
end;

procedure TFAT32.ReadCluster(ANumber: Cardinal; AStream: TMemoryStream);
begin
  AStream.SetSize(Int64(FBS.SecPerClust)*bBytesPerSector);
  FDA.Position:=Int64(FDSS+(ANumber*FBS.SecPerClust)-(2*FBS.SecPerClust))*bBytesPerSector;
  AStream.CopyFrom(FDA,Int64(FBS.SecPerClust)*bBytesPerSector);
  AStream.Position:=0;
end;

{ TNTFS }

constructor TNTFS.Create(ADiskNumber: Byte);
begin
  FDA:=TDiskAccess.Create(ADiskNumber);
end;

destructor TNTFS.Destroy;
begin
  FDA.Free;
  inherited;
end;

procedure TNTFS.ReadClusters(AStart, ACount: Cardinal; AStream: TMemoryStream);
begin
  if AStart>FBS.SectorSize div FBS.SecPerClust then
    raise EInOutError.Create('Tried to read cluster out of range.');
  AStream.SetSize(Int64(FBS.SecPerClust*ACount)*bBytesPerSector);
  FDA.Position:=Int64(FBS.SecPerClust*AStart)*bBytesPerSector;
  AStream.CopyFrom(FDA,Int64(FBS.SecPerClust*ACount)*bBytesPerSector);
  AStream.Position:=0;
end;

end.

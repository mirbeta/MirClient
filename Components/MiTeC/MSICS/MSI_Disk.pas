{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Disk Detection Part                    }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Disk;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_Routines;

const
  StorageFolderName = 'Disk';

type
  TMiTeC_Disk = class(TMiTeC_Component)
  private
    FDisk: string;
    FDriveType: TMediaType;
    FSectorsPerCluster: Cardinal;
    FBytesPerSector: Cardinal;
    FFreeClusters: Cardinal;
    FTotalClusters: Cardinal;
    FFileFlags: TFileFlags;
    FVolumeLabel: string;
    FSerialNumber: string;
    FFileSystem: string;
    FFreeSpace: int64;
    FCapacity: int64;
    FAvailDisks: string;
    FSerial: Cardinal;
    FUNC: string;
    function GetMediaPresent: Boolean;
  protected
    procedure SetDisk(const Value: string);
  public
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure GetFileFlagsStr(var AFileFlags :TStringList);
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    function GetCD: byte;
    function GetMediaTypeStr(MT: TMediaType): string;
    property Serial: Cardinal read FSerial stored false;
  published
    property Drive: string read FDisk Write SetDisk stored false;
    property AvailableDisks: string read FAvailDisks stored false;
    property MediaPresent: Boolean read GetMediaPresent stored false;
    property MediaType: TMediaType read FDriveType stored false;
    property FileFlags: TFileFlags read FFileFlags stored false;
    property FileSystem: string read FFileSystem stored false;
    property FreeClusters: Cardinal read FFreeClusters stored false;
    property TotalClusters: Cardinal read FTotalClusters stored false;
    property FreeSpace: int64 read FFreeSpace stored false;
    property Capacity: int64 read FCapacity stored false;
    property SerialNumber: string read FSerialNumber stored false;
    property VolumeLabel: string read FVolumeLabel stored false;
    property SectorsPerCluster: Cardinal read FSectorsPerCluster stored false;
    property BytesPerSector: Cardinal read FBytesPerSector stored false;
    property UNCPath: string read FUNC stored False;
  end;

implementation

uses {$IFDEF RAD9PLUS}
     System.Math,
     {$ELSE}
     Math,
     {$ENDIF}
     MiTeC_StrUtils;

{ TMiTeC_Disk }

procedure TMiTeC_Disk.Clear;
begin
  FDriveType:=dtUnknown;
  FFileFlags:=[];
  FCapacity:=0;
  FFreeSpace:=0;
  FBytesPerSector:=0;
  FTotalClusters:=0;
  FFreeClusters:=0;
  FSectorsPerCluster:=0;
  FVolumeLabel:='';
  FFileSystem:='';
  FSerialNumber:='';
  FSerial:=0;
  FUNC:='';
end;

function TMiTeC_Disk.GetCD: byte;
var
  i :integer;
  root :string;
begin
  result:=0;
  for i:=1 to length(FAvailDisks) do begin
    root:=copy(FAvailDisks,i,1)+':\';
    if getdrivetype(PChar(root))=drive_cdrom then begin
      result:=i;
      break;
    end;
  end;
end;

procedure TMiTeC_Disk.GetFileFlagsStr;
begin
  with AFileFlags do begin
    Add(Format('Case Is Preserved=%d',[integer(fsCaseIsPreserved in FileFlags)]));
    Add(Format('Case Sensitive=%d',[integer(fsCaseSensitive in FileFlags)]));
    Add(Format('Unicode stored On Disk=%d',[integer(fsUnicodeStoredOnDisk in FileFlags)]));
    Add(Format('Persistent Acls=%d',[integer(fsPersistentAcls in FileFlags)]));
    Add(Format('File Compression=%d',[integer(fsFileCompression in FileFlags)]));
    Add(Format('Volume Is Compressed=%d',[integer(fsVolumeIsCompressed in FileFlags)]));
    Add(Format('Long Filenames=%d',[integer(fsLongFileNames in FileFlags)]));
    Add(Format('Encrypted File System Support=%d',[integer(fsEncryptedFileSystemSupport in FileFlags)]));
    Add(Format('Object IDs Support=%d',[integer(fsObjectIDsSupport in FileFlags)]));
    Add(Format('Reparse Points Support=%d',[integer(fsReparsePointsSupport in FileFlags)]));
    Add(Format('Sparse Files Support=%d',[integer(fsSparseFilesSupport in FileFlags)]));
    Add(Format('Disk Quotas Support=%d',[integer(fsDiskQuotasSupport in FileFlags)]));
  end;
end;

procedure TMiTeC_Disk.RefreshData;
var
  i,n :integer;
  buf :PChar;
begin
  inherited;
  buf:=stralloc(255);
  try
    n:=GetLogicalDriveStrings(255,buf);
    FAvailDisks:='';
    for i:=0 to n do
      if buf[i]<>#0 then begin
        if (ord(buf[i]) in [$41..$5a]) or (ord(buf[i]) in [$61..$7a]) then
          FAvailDisks:=FAvailDisks+upcase(buf[i])
      end else
        if buf[i+1]=#0 then
          break;
  finally
    strdispose(buf);
  end;
  Drive:=Copy(GetSysDir,1,2);
  SetDataAvail(AvailableDisks<>'');
end;


function TMiTeC_Disk.GetMediaPresent :Boolean;
begin
  Result:=MiTeC_Routines.GetMediaPresent(FDisk);
end;


procedure TMiTeC_Disk.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AName: string);
var
  strm: TStorageStream;
  sl: TStringList;
  ff: TFileFlag;
  n: Integer;
begin
  sl:=TStringList.Create;
  try
    WriteStrProperty(sl,'Disk',Self.Drive);
    WriteStrProperty(sl,'UNCPath',Self.UNCPath);
    WriteStrProperty(sl,'FileSystem',Self.FileSystem);
    WriteStrProperty(sl,'SerialNumber',Self.SerialNumber);
    WriteStrProperty(sl,'VolumeLabel',Self.VolumeLabel);
    WriteIntProperty(sl,'MediaType',Integer(Self.MediaType));
    WriteIntProperty(sl,'Serial',Self.Serial);
    WriteIntProperty(sl,'FreeClusters',Self.FreeClusters);
    WriteIntProperty(sl,'TotalClusters',Self.TotalClusters);
    WriteIntProperty(sl,'FreeSpace',Self.FreeSpace);
    WriteIntProperty(sl,'Capacity',Self.Capacity);
    WriteIntProperty(sl,'SectorsPerCluster',Self.SectorsPerCluster);
    WriteIntProperty(sl,'BytesPerSector',Self.BytesPerSector);
    n:=0;
    for ff:=Low(TFileFlag) to High(TFileFlag) do
      if ff in Self.FileFlags then
        n:=n+Round(Power(2,Integer(ff)));
    WriteIntProperty(sl,'FileFlags',n);
    strm:=Sub.OpenStream(AName,STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

var
  i: Integer;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      for i:=1 to Length(Self.AvailableDisks) do begin
        Self.Drive:=Copy(Self.AvailableDisks,i,1)+':';
        WriteToStream(Copy(Self.AvailableDisks,i,1));
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Disk.SetDisk(const Value: string);
var
  DI: TDiskInfo;
  rh: Boolean;
begin
  Clear;
  FDisk:=Value;
  if LiveData then begin
    DI:=GetDiskInfo(Value);
    FDriveType:=DI.MediaType;
    FFileFlags:=DI.FileFlags;
    FCapacity:=DI.Capacity;
    FFreeSpace:=DI.FreeSpace;
    FBytesPerSector:=DI.BytesPerSector;
    FTotalClusters:=DI.TotalClusters;
    FFreeClusters:=DI.FreeClusters;
    FSectorsPerCluster:=DI.SectorsPerCluster;
    FVolumeLabel:=DI.VolumeLabel;
    FFileSystem:=DI.FileSystem;
    FSerialNumber:=DI.SerialNumber;
    FSerial:=DI.Serial;
    FUNC:=ExpandUNCFilename(Drive);
  end else begin
    rh:=True;
    LoadFromStorage(StorageFileName,rh,StreamCodeProc);
  end;
end;

function TMiTeC_Disk.GetMediaTypeStr(MT: TMediaType): string;
begin
  case MT of
    dtUnknown     :result:='<unknown>';
    dtNotExists   :result:='<not exists>';
    dtRemovable   :result:='Removable';
    dtFixed       :result:='Fixed';
    dtRemote      :result:='Remote';
    dtCDROM       :result:='CDROM';
    dtRAMDisk     :result:='RAM';
  end;
end;


function TMiTeC_Disk.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AName: string): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  ff: TFileFlag;
  n: Integer;
begin
  Result:=False;
  try strm:=Sub.OpenStream(AName,STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        Self.FDisk:=ReadStrProperty(sl,'Disk');
        Self.FDriveType:=TMediaType(ReadIntProperty(sl,'MediaType'));
        Self.FSectorsPerCluster:=ReadIntProperty(sl,'SectorsPerCluster');
        Self.FBytesPerSector:=ReadIntProperty(sl,'BytesPerSector');
        Self.FFreeClusters:=ReadIntProperty(sl,'FreeClusters');
        Self.FTotalClusters:=ReadIntProperty(sl,'TotalClusters');
        n:=ReadIntProperty(sl,'FileFlags');
        Self.FFileFlags:=[];
        for ff:=Low(TFileFlag) to High(TFileFlag) do
          if n and Round(Power(2,Integer(ff)))<>0 then
            Self.FFileFlags:=Self.FFileFlags+[ff];
        Self.FVolumeLabel:=ReadStrProperty(sl,'VolumeLabel');
        Self.FSerialNumber:=ReadStrProperty(sl,'SerialNumber');
        Self.FFileSystem:=ReadStrProperty(sl,'FileSystem');
        Self.FFreeSpace:=ReadIntProperty(sl,'FreeSpace');
        Self.FCapacity:=ReadIntProperty(sl,'Capacity');
        Self.FSerial:=ReadIntProperty(sl,'Serial');
        Self.FUNC:=ReadStrProperty(sl,'UNCPath');
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

var
  i: Integer;
  s: string;
begin
  FDisk:=Copy(GetSysDir,1,2);
  Self.FAvailDisks:='';
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then
    try
      for i:=0 to Sub.ElementCount-1 do
        if (Length(Sub.Elements[i].Name)=1) then
          Self.FAvailDisks:=Self.FAvailDisks+Sub.Elements[i].Name;
      s:=Copy(Self.Drive,1,1);
      if PosText(s,Self.FAvailDisks)>0 then
        Result:=ReadFromStream(s) or Result;
    finally
      if Sub<>nil then
       Sub.Free;
    end;
  finally
    SS.Free;
  end;
  SetDataAvail(AvailableDisks<>'');
end;

end.

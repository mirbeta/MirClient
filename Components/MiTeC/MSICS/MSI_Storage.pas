{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Storage Detection Part                   }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Storage;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_WinIOCTL, MiTeC_Storage;

const
  StorageFolderName = 'Storage';
  Logical_StorageFolderName = 'LogicalDevices';
  Physical_StorageFolderName = 'PhysicalDevices';
  Layout_StorageFolderName = 'Layout';
  Extents_StorageFolderName = 'Extents';

  strm_Layout = 'Layout_%d';
  strm_Extents = 'Extents_%d';
  strm_IDD = 'IdentifyDeviceData';
type
  TDeviceInfoArray = array of TDeviceInfo;

  TExtentsArray = array of record
                             Drive: Char;
                             MPUID,
                             UniqueID: string;
                             Extent: TDiskExtent;
                           end;

type
  TMiTeC_Storage = class(TMiTeC_Component)
  private
    FPHYS,FLOG: TDeviceInfoArray;
    FSMARTDetect: Boolean;
    function GetLogCount: Integer;
    function GetLogInfo(Index: integer): TDeviceInfo;
    procedure FreeDeviceInfoArray(var AList: TDeviceInfoArray);
    function GetPhysCount: Integer;
    function GetPhysInfo(Index: Integer): TDeviceInfo;
    function GetLogInfoBySign(Sign: Char): TDeviceInfo;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property Logical[Index: Integer]: TDeviceInfo read GetLogInfo;
    property LogicalDisk[Sign: Char]: TDeviceInfo read GetLogInfoBySign;
    property Physical[Index: Integer]: TDeviceInfo read GetPhysInfo;

    function FindPhysical(ALogical: char): Integer;
  published
    property SMARTDetect: Boolean read FSMARTDetect write FSMARTDetect default True;
    property LogicalCount: Integer read GetLogCount stored False;
    property PhysicalCount: Integer read GetPhysCount stored False;
  end;

implementation

uses MiTeC_Routines, MiTeC_StrUtils, MiTeC_CfgMgrSetupAPI, MiTeC_RegUtils;


{ TMiTeC_Storage }

procedure TMiTeC_Storage.Clear;
begin
  FreeDeviceInfoArray(FPHYS);
  FreeDeviceInfoArray(FLOG);
end;

constructor TMiTeC_Storage.Create;
begin
  inherited Create(AOwner);
  FSMARTDetect:=True;
end;

destructor TMiTeC_Storage.Destroy;
begin
  inherited;
  FreeDeviceInfoArray(FPHYS);
  FreeDeviceInfoArray(FLOG);
end;

procedure TMiTeC_Storage.RefreshData;
var
  i,j,k: Integer;
  DI,DI1: TDeviceInfo;
  DL: TDiskLayout;
  VDE: TVolumeDiskExtents;
  ok,exists: Boolean;
  h: THandle;
  s,v,m1,m2: string;
  sll,mdl: TStringList;
  vi: TDiskInfo;
begin

{$B+}

  inherited;

  sll:=TStringList.Create;
  mdl:=TStringList.Create;

  Clear;

  GetSymbolicLinkTable(sll);

// Logical devices
  CreateMountedDevicesTable(mdl,True,True);
  s:=GetLogicalDisks;
  for i:=1 to Length(s) do begin
    vi:=GetDiskInfo(s[i]+':');
    h:=GetHandle_LogicalDrive(Ord(s[i]));
    try
      ClearStorageRecord(DI);
      ClearStorageRecord(DI1);
      DI.LogicalSize:=vi.Capacity;
      DI.VolumeLabel:=vi.VolumeLabel;
      DI.LogicalSerial:=Format('%8.8x',[vi.Serial]);
      DI.FileSystem:=vi.FileSystem;
      DI.ClusterSize:=vi.SectorsPerCluster*vi.BytesPerSector;
      GetDeviceLength(h,DI.LengthInBytes);
      ok:=GetDeviceNumber(h,DI.Number);
      if (PosText('USBSTOR',sll.Values[IntToStr(DI.Number)])>0) then
        DI.SerialNumber:=DecodeSerial(sll.Values[IntToStr(DI.Number)]);
      DI1.Number:=DI.Number;
      DI1.SerialNumber:=DI.SerialNumber;
      ok:=ok or GetData_SMART(h,DI);
      ok:=ok or GetData_DeviceDescriptor(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_SCSI_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_SCSI_Miniport(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_ATA_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      GetDeviceData(h,DI,FSMARTDetect);
      DI.Drive:=s[i];
      DI.Device:=Ord(s[i]);
      DI.Removable:=(GetDriveType(PChar(string(DI.Drive+':')))=DRIVE_REMOVABLE);
      DI.RAM:=(GetDriveType(PChar(string(DI.Drive+':')))=DRIVE_RAMDISK);
      DI.MediaType:=Cardinal(vi.MediaType);
      if DI.DeviceType=0 then
        case GetDriveType(PChar(Format('%s:\',[s[i]]))) of
          DRIVE_REMOVABLE: begin
            DI.DeviceType:=FILE_DEVICE_DISK;
            DI.Removable:=True;
            ok:=True;
          end;
          DRIVE_FIXED: begin
            DI.DeviceType:=FILE_DEVICE_DISK;
            ok:=True;
          end;
          DRIVE_CDROM: begin
            DI.DeviceType:=FILE_DEVICE_CD_ROM;
            ok:=True;
          end
          else DI.DeviceType:=FILE_DEVICE_UNKNOWN;
        end;
      for j:=0 to High(DL) do
        if (VDE.Extents[0].StartingOffset.QuadPart=DL[j].StartingOffset.QuadPart) then begin
          SetLength(DI.Layout,Length(DI.Layout)+1);
          DI.Layout[High(DI.Layout)]:=DL[j];
          Break;
        end;
      if DI.Model='' then begin
        for j:=0 to mdl.Count-1 do
          if SameText(mdl.Names[j],s[i]) then begin
            v:=mdl.Values[mdl.Names[j]];
            if (Length(v)>0) and (Ord(v[1])<256) then begin
              if Pos('\',v)>0 then
                DI.Model:=GetDeviceFriendlyName(ExtractFilePath(v))
              else
                DI.Model:=v;
            end;
            Break;
          end;
      end;
      if (DI.Model='') and DI.RAM  then
        DI.Model:='RAMDisk';
      if ok then begin
        DI.Kind:=dikLogicalDrive;
        SetLength(FLOG,Length(FLOG)+1);
        FLOG[High(FLOG)]:=DI;
      end;
    finally
      CloseHandle(h);
    end;
  end;

  for i:=0 to 255 do begin
// PhysicalDrive devices
    ClearStorageRecord(DI);
    ClearStorageRecord(DI1);
    h:=GetHandle_PhysicalDrive(i);
    ok:=False;
    if h<>Cardinal(INVALID_HANDLE_VALUE) then
    try
      DI.Device:=i;
      GetDeviceLength(h,DI.LengthInBytes);
      ok:=GetDeviceNumber(h,DI.Number);
      if (PosText('USBSTOR',sll.Values[IntToStr(DI.Number)])>0) then
        DI.SerialNumber:=DecodeSerial(sll.Values[IntToStr(DI.Number)]);
      DI1.SerialNumber:=DI.SerialNumber;
      DI1.Number:=DI.Number;
      ok:=ok or GetData_SMART(h,DI);
      ok:=ok or GetData_DeviceDescriptor(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      DI1.Device:=i;
      ok:=ok or GetData_SCSI_Miniport(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      DI1.Device:=i;
      ok:=ok or GetData_SCSI_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      ok:=ok or GetData_ATA_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      GetDeviceData(h,DI,FSMARTDetect);
    finally
      CloseHandle(h);
    end;
    if ok and (DI.Model<>'') then begin
      DI.Kind:=dikPhysicalDrive;
      SetLength(FPHYS,Length(FPHYS)+1);
      FPHYS[High(FPHYS)]:=DI;
    end;

// CDROM devices
    ClearStorageRecord(DI);
    ClearStorageRecord(DI1);
    h:=GetHandle_CDROM(i);
    if h<>Cardinal(INVALID_HANDLE_VALUE) then
    try
      DI.Device:=i;
      GetDeviceLength(h,DI.LengthInBytes);
      ok:=GetDeviceNumber(h,DI.Number);
      DI1.Number:=DI.Number;
      DI1.Device:=i;
      ok:=ok or GetData_SCSI_Miniport(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_SCSI_PassThrough(h,DI);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_DeviceDescriptor(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      ok:=ok or GetData_ATA_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      GetDeviceData(h,DI,FSMARTDetect);
    finally
      CloseHandle(h);
    end;
    if ok and (DI.Model<>'') then begin
      if DI.DeviceType=0 then
        DI.DeviceType:=FILE_DEVICE_CD_ROM;
      DI.Kind:=dikCDROM;
      SetLength(FPHYS,Length(FPHYS)+1);
      FPHYS[High(FPHYS)]:=DI;
    end;

// Tape devices
    ClearStorageRecord(DI);
    ClearStorageRecord(DI1);
    h:=GetHandle_Tape(i);
    if h<>Cardinal(INVALID_HANDLE_VALUE) then
    try
      DI.Device:=i;
      DI1.Device:=i;
      GetDeviceLength(h,DI.LengthInBytes);
      ok:=GetDeviceNumber(h,DI.Number);
      DI1.Number:=DI.Number;
      ok:=ok or GetData_DeviceDescriptor(h,DI);
      ok:=ok or GetData_SCSI_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      ok:=ok or GetData_ATA_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      GetDeviceData(h,DI,FSMARTDetect);
    finally
      CloseHandle(h);
    end;
    if ok and (DI.Model<>'') then begin
      if DI.DeviceType=0 then
        DI.DeviceType:=FILE_DEVICE_TAPE;
      DI.Kind:=dikTape;
      SetLength(FPHYS,Length(FPHYS)+1);
      FPHYS[High(FPHYS)]:=DI;
    end;

  end;

// Symbolic links

  for i:=0 to sll.Count-1 do begin
    ClearStorageRecord(DI);
    ClearStorageRecord(DI1);
    h:=GetHandle_Symbolic(sll.ValueFromIndex[i]);
    if h<>Cardinal(INVALID_HANDLE_VALUE) then
    try
      DI.Device:=i;
      GetDeviceLength(h,DI.LengthInBytes);
      ok:=GetDeviceNumber(h,DI.Number);
      if (PosText('USBSTOR',sll.ValueFromIndex[i])>0) then
        DI.SerialNumber:=DecodeSerial(sll.ValueFromIndex[i]);
      DI1.SerialNumber:=DI.SerialNumber;
      DI1.SerialNumber:=DI.SerialNumber;
      DI1.Number:=DI.Number;
      ok:=ok or GetData_SMART(h,DI);
      ok:=ok or GetData_DeviceDescriptor(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      DI1.Device:=i;
      ok:=ok or GetData_SCSI_Miniport(h,DI1);
      AddInfoToDI(DI1,DI);
      ClearStorageRecord(DI1);
      DI1.Device:=i;
      ok:=ok or GetData_SCSI_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      DI1.Device:=i;
      ok:=ok or GetData_ATA_PassThrough(h,DI1);
      AddInfoToDI(DI1,DI);
      GetDeviceData(h,DI,FSMARTDetect);
      DI.SSD:=(DI.IdentifyDeviceData.NominalMediaRotationRate=1) and DI.HasNoSeekPenalty;
    finally
      CloseHandle(h);
    end;

    exists:=False;
    if ok then begin
      for j:=0 to High(FPHYS) do
        if (SameText(TrimAll(FPHYS[j].Model),TrimAll(DI.Model)) and (FPHYS[j].HaId=DI.HaId)) then begin
          exists:=True;
          AddInfoToDI(DI,FPHYS[j]);
          Break;
        end;
      if not exists and (DI.Model<>'') then begin
        DI.Kind:=dikSymbolicLink;
        SetLength(FPHYS,Length(FPHYS)+1);
        FPHYS[High(FPHYS)]:=DI;
      end;
    end;
  end;

//temporarily disabled due to BSOD on some RAID devices
(*
// SCSI devices
//  if Length(FPHYS)=0 then
    for i:=0 to 255 do begin
      ClearStorageRecord(DI);
      ClearStorageRecord(DI1);
      h:=GetHandle_SCSI(i);
      try
        DI.Device:=i;
        ok:=GetData_SCSI_PassThrough(h,DI);
        DI1.Device:=i;
        ok:=ok or GetData_SCSI_Miniport(h,DI1);
        AddInfoToDI(DI1,DI);
        GetDeviceData(h,DI,FSMARTDetect);
        DI.HaId:=i div 2;
        DI.Target:=i mod 2;
      finally
        Closehandle(h);
      end;

      exists:=False;
      if ok then begin
        for j:=0 to High(FPHYS) do
          if (SameText(TrimAll(FPHYS[j].Model),TrimAll(DI.Model)) and (FPHYS[j].HaId=DI.HaId) {and (FPHYS[j].Target=DI.Target)}) then begin
            exists:=True;
            AddInfoToDI(DI,FPHYS[j]);
            Break;
          end;
        if not exists and (DI.Model<>'') then begin
          SetLength(FPHYS,Length(FPHYS)+1);
          FPHYS[High(FPHYS)]:=DI;
        end;
      end;
    end;
*)
  for j:=0 to High(FLOG) do
    if FLOG[j].PhysicalIndex=-1 then begin
      exists:=False;
      for i:=0 to High(FPHYS) do begin
        m1:=TrimAll(FPHYS[i].Model);
        m2:=TrimAll(FLOG[j].Model);
        if SameText(m1,m2) and
           (((FPHYS[i].DeviceType in [FILE_DEVICE_DVD,FILE_DEVICE_CD_ROM]) and (FPHYS[i].PathId=FLOG[j].PathId)) or SameText(FPHYS[i].SerialNumber,FLOG[j].SerialNumber))
        then begin
          exists:=True;
          FLOG[j].PhysicalIndex:=i;
          Break;
        end;
      end;
      if not exists then begin
        SetLength(FPHYS,Length(FPHYS)+1);
        FPHYS[High(FPHYS)]:=FLOG[j];
        FLOG[j].PhysicalIndex:=High(FPHYS);
      end;
    end;

  for i:=0 to High(FPHYS) do
    for j:=0 to High(FLOG) do
      if (FLOG[j].PhysicalIndex=-1) and
         ((FPHYS[i].HaId+FLOG[j].HaId>=0) and (FPHYS[i].Target+FLOG[j].Target>=0) and (FPHYS[i].Lun+FLOG[j].Lun>=0)) and
         ((FPHYS[i].HaId=FLOG[j].HaId) and (FPHYS[i].Target=FLOG[j].Target) and (FPHYS[i].Lun=FLOG[j].Lun))
      then begin
        FLOG[j].PhysicalIndex:=i;
        AddInfoToDI(FLOG[j],FPHYS[i]);
        //Break;
      end;

  for j:=0 to High(FLOG) do
    for i:=0 to High(FPHYS) do
      if (Length(FPHYS[i].Layout)>0) and (Length(FLOG[j].DiskExtents)>0) then
        for k:=0 to High(FPHYS[i].Layout) do
          if (FPHYS[i].Layout[k].StartingOffset.QuadPart=FLOG[j].DiskExtents[0].StartingOffset.QuadPart) and
             (FPHYS[i].Layout[k].Length.QuadPart=FLOG[j].DiskExtents[0].ExtentLength.QuadPart)
          then begin
            if (FLOG[j].PhysicalIndex=-1) then
              FLOG[j].PhysicalIndex:=i;
            if (FLOG[j].PhysicalIndex=i) then begin
              FLOG[j].LayoutIndex:=k;
              AddInfoToDI(FLOG[j],FPHYS[i]);
            end;
            Break;
          end;


  SetDataAvail(True);

{$B-}
  sll.Free;
  mdl.Free;
end;

function TMiTeC_Storage.GetPhysCount: Integer;
begin
  Result:=Length(FPHYS);
end;

function TMiTeC_Storage.GetPhysInfo(Index: integer): TDeviceInfo;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(TDeviceInfo),0);
  if (Index>=0) and (Index<Length(FPHYS)) then
    Result:=FPHYS[Index];
end;

function TMiTeC_Storage.FindPhysical(ALogical: char): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FLOG) do
    if SameText(FLOG[i].Drive,ALogical) then begin
      Result:=FLOG[i].PhysicalIndex;
      Break;
    end;
end;

procedure TMiTeC_Storage.FreeDeviceInfoArray;
var
  i: Integer;
begin
  for i:=0 to High(AList) do begin
    Finalize(AList[i].Layout);
    Finalize(AList[i].SMART);
  end;
  Finalize(Alist);
end;

function TMiTeC_Storage.GetLogCount: Integer;
begin
  Result:=Length(FLOG);
end;

function TMiTeC_Storage.GetLogInfo(Index: Integer): TDeviceInfo;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(TDeviceInfo),0);
  if (Index>=0) and (Index<Length(FLOG)) then
    Result:=FLOG[Index];
end;

function TMiTeC_Storage.GetLogInfoBySign(Sign: Char): TDeviceInfo;
var
  i: Integer;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(TDeviceInfo),0);
  for i:=0 to High(FLOG) do
    if SameText(FLOG[i].Drive,Sign) then begin
      Result:=FLOG[i];
      Break;
    end;
end;

function TMiTeC_Storage.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub, Sub1: TStructuredStorage;

function ReadLayoutFromStream(ASub: TStructuredStorage; var AList: TDiskLayout; AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try
    strm:=ASub.OpenStream(Format(strm_Layout,[AIndex]),STG_READ_INSTORAGE,False)
  except
    strm:=nil
  end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(AList,Length(AList)+1);
        with AList[High(AList)] do begin
          StartingOffset.QuadPart:=ReadIntProperty(sl,'StartingOffset');
          Length.QuadPart:=ReadIntProperty(sl,'Length');
          HiddenSectors:=ReadIntProperty(sl,'HiddenSectors');
          Number:=ReadIntProperty(sl,'Number');
          Typ:=ReadIntProperty(sl,'Typ');
          BootIndicator:=ReadIntProperty(sl,'BootIndicator')=1;
          Recognized:=ReadIntProperty(sl,'Recognized')=1;
          Rewrite:=ReadIntProperty(sl,'Rewrite')=1;
        end;
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadExtentsFromStream(ASub: TStructuredStorage; var AList: TDiskExtents; AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=ASub.OpenStream(Format(strm_Extents,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        SetLength(AList,Length(AList)+1);
        with AList[High(AList)] do begin
          StartingOffset.QuadPart:=ReadIntProperty(sl,'StartingOffset');
          ExtentLength.QuadPart:=ReadIntProperty(sl,'ExtentLength');
          DiskNumber:=ReadIntProperty(sl,'DiskNumber');
        end;
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadIDDFromStream(ASub: TStructuredStorage; var AData: TIdentifyDeviceData): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  i: integer;
begin
  Result:=False;
  try strm:=ASub.OpenStream(strm_IDD,STG_READ_INSTORAGE,False) except strm:=nil end;
  if strm<>nil then
    try
      sl:=TStringList.Create;
      try
        LoadFromEncodedStream(strm,sl,ACodeStream);
        AData.GeneralConfiguration:=ReadIntProperty(sl,'GeneralConfiguration');
        AData.NumCylinders:=ReadIntProperty(sl,'NumCylinders');
        AData.ReservedWord2:=ReadIntProperty(sl,'ReservedWord2');
        AData.NumHeads:=ReadIntProperty(sl,'NumHeads');
        AData.NumSectorsPerTrack:=ReadIntProperty(sl,'NumSectorsPerTrack');
        for i:=0 to High(AData.VendorUnique1) do
          AData.VendorUnique1[i]:=ReadIntProperty(sl,Format('VendorUnique1_%d',[i]));
        for i:=0 to High(AData.Retired2) do
          AData.Retired2[i]:=ReadIntProperty(sl,Format('Retired2_%d',[i]));
        AData.Obsolete1:=ReadIntProperty(sl,'Obsolete1');
        AData.MaximumBlockTransfer:=ReadIntProperty(sl,'MaximumBlockTransfer');
        AData.VendorUnique2:=ReadIntProperty(sl,'VendorUnique2');
        AData.Capabilities:=ReadIntProperty(sl,'Capabilities');
        AData.ReservedWord50:=ReadIntProperty(sl,'ReservedWord50');
        AData.TranslationFieldsValid:=ReadIntProperty(sl,'TranslationFieldsValid');
        AData.NumberOfCurrentCylinders:=ReadIntProperty(sl,'NumberOfCurrentCylinders');
        AData.NumberOfCurrentHeads:=ReadIntProperty(sl,'NumberOfCurrentHeads');
        AData.CurrentSectorsPerTrack:=ReadIntProperty(sl,'CurrentSectorsPerTrack');
        AData.CurrentSectorCapacity:=ReadIntProperty(sl,'CurrentSectorCapacity');
        AData.CurrentMultiSectorSetting:=ReadIntProperty(sl,'CurrentMultiSectorSetting');
        AData.MultiSectorSettingValid:=ReadIntProperty(sl,'MultiSectorSettingValid');
        AData.UserAddressableSectors:=ReadIntProperty(sl,'UserAddressableSectors');
        AData.MultiWordDMAActive:=ReadIntProperty(sl,'MultiWordDMAActive');
        AData.MultiWordDMASupport:=ReadIntProperty(sl,'MultiWordDMASupport');
        AData.AdvancedPIOModes:=ReadIntProperty(sl,'AdvancedPIOModes');
        AData.MinimumMWXferCycleTime:=ReadIntProperty(sl,'MinimumMWXferCycleTime');
        AData.RecommendedMWXferCycleTime:=ReadIntProperty(sl,'RecommendedMWXferCycleTime');
        AData.MinimumPIOCycleTime:=ReadIntProperty(sl,'MinimumPIOCycleTime');
        AData.MinimumPIOCycleTimeIORDY:=ReadIntProperty(sl,'MinimumPIOCycleTimeIORDY');
        AData.QueueDepth:=ReadIntProperty(sl,'QueueDepth');
        AData.MajorRevision:=ReadIntProperty(sl,'MajorRevision');
        AData.MinorRevision:=ReadIntProperty(sl,'MinorRevision');
        for i:=0 to High(AData.CommandSetSupport) do
          AData.CommandSetSupport[i]:=ReadIntProperty(sl,Format('CommandSetSupport_%d',[i]));
        for i:=0 to High(AData.CommandSetActive) do
          AData.CommandSetActive[i]:=ReadIntProperty(sl,Format('CommandSetActive_%d',[i]));
        AData.UltraDMAActive:=ReadIntProperty(sl,'UltraDMAActive');
        AData.UltraDMASupport:=ReadIntProperty(sl,'UltraDMASupport');
        AData.HardwareResetResult:=ReadIntProperty(sl,'HardwareResetResult');
        AData.RecommendedAcousticValue:=ReadIntProperty(sl,'RecommendedAcousticValue');
        AData.CurrentAcousticValue:=ReadIntProperty(sl,'CurrentAcousticValue');
        for i:=0 to High(AData.Max48BitLBA) do
          AData.Max48BitLBA[i]:=ReadIntProperty(sl,Format('Max48BitLBA_%d',[i]));
        AData.StreamingTransferTime:=ReadIntProperty(sl,'StreamingTransferTime');
        AData.PhysicalLogicalSectorSize:=ReadIntProperty(sl,'PhysicalLogicalSectorSize');
        AData.InterSeekDelay:=ReadIntProperty(sl,'InterSeekDelay');
        for i:=0 to High(AData.WorldWideName) do
          AData.WorldWideName[i]:=ReadIntProperty(sl,Format('WorldWideName_%d',[i]));
        for i:=0 to High(AData.ReservedForWorldWideName128) do
          AData.ReservedForWorldWideName128[i]:=ReadIntProperty(sl,Format('ReservedForWorldWideName128_%d',[i]));
        AData.ReservedForTlcTechnicalReport:=ReadIntProperty(sl,'ReservedForTlcTechnicalReport');
        for i:=0 to High(AData.WordsPerLogicalSector) do
          AData.WordsPerLogicalSector[i]:=ReadIntProperty(sl,Format('WordsPerLogicalSector_%d',[i]));
        AData.CommandSetSupportExt:=ReadIntProperty(sl,'CommandSetSupportExt');
        AData.CommandSetActiveExt:=ReadIntProperty(sl,'CommandSetActiveExt');
        for i:=0 to High(AData.ReservedForExpandedSupportandActive) do
          AData.ReservedForExpandedSupportandActive[i]:=ReadIntProperty(sl,Format('ReservedForExpandedSupportandActive_%d',[i]));
        AData.MsnSupport:=ReadIntProperty(sl,'MsnSupport');
        AData.SecurityStatus:=ReadIntProperty(sl,'SecurityStatus');
        AData.CfaPowerModel:=ReadIntProperty(sl,'CfaPowerModel');
        for i:=0 to High(AData.ReservedForCfaWord161) do
          AData.ReservedForCfaWord161[i]:=ReadIntProperty(sl,Format('ReservedForCfaWord161_%d',[i]));
        AData.DataSetManagementFeature:=ReadIntProperty(sl,'DataSetManagementFeature');
        for i:=0 to High(AData.ReservedForCfaWord170) do
          AData.ReservedForCfaWord170[i]:=ReadIntProperty(sl,Format('ReservedForCfaWord170_%d',[i]));
        AData.BlockAlignment:=ReadIntProperty(sl,'BlockAlignment');
        for i:=0 to High(AData.WriteReadVerifySectorCountMode3Only) do
          AData.WriteReadVerifySectorCountMode3Only[i]:=ReadIntProperty(sl,Format('WriteReadVerifySectorCountMode3Only_%d',[i]));
        for i:=0 to High(AData.WriteReadVerifySectorCountMode2Only) do
          AData.WriteReadVerifySectorCountMode2Only[i]:=ReadIntProperty(sl,Format('WriteReadVerifySectorCountMode2Only_%d',[i]));
        AData.NVCacheCapabilities:=ReadIntProperty(sl,'NVCacheCapabilities');
        AData.NVCacheSizeLSW:=ReadIntProperty(sl,'NVCacheSizeLSW');
        AData.NVCacheSizeMSW:=ReadIntProperty(sl,'NVCacheSizeMSW');
        AData.NominalMediaRotationRate:=ReadIntProperty(sl,'NominalMediaRotationRate');
        AData.ReservedWord218:=ReadIntProperty(sl,'ReservedWord218');
        for i:=0 to High(AData.ReservedWord220) do
          AData.ReservedWord220[i]:=ReadIntProperty(sl,Format('ReservedWord220_%d',[i]));
        AData.NVCacheEstimatedTimeToSpinUpInSeconds:=ReadIntProperty(sl,'NVCacheEstimatedTimeToSpinUpInSeconds');
        AData.CheckSum:=ReadIntProperty(sl,'CheckSum');
        AData.Signature:=ReadIntProperty(sl,'Signature');
        Result:=True;
        SetDataAvail(True);
      finally
        sl.Free;
      end;
    finally
      strm.Free;
    end;
end;

function ReadFromStream(var AList: TDeviceInfoArray; AName: string): boolean;
var
  S: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
begin
  Result:=False;
  try
    S:=Sub1.OpenSubStorage(AName,STG_READ_INSTORAGE,False);
  except
    S:=nil;
  end;
  if S<>nil then
    try
      try strm:=S.OpenStream(strm_Props,STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            SetLength(AList,Length(AList)+1);
            with AList[High(AList)] do begin
              try
                Drive:=ReadStrProperty(sl,'Drive')[1];
                if not ({$IFDEF UNICODE}CharInSet(Drive,['a'..'z','A'..'Z']){$ELSE}Drive in ['a'..'z','A'..'Z']{$ENDIF}) then
                  Drive:=Chr(StrToIntDef(Drive,0));
              except
              end;
              Model:=Trim(ReadStrProperty(sl,'Vendor')+' '+ReadStrProperty(sl,'Model'));
              Revision:=ReadStrProperty(sl,'Revision');
              SerialNumber:=ReadStrProperty(sl,'SerialNumber');
              if SerialNumber='' then
                SerialNumber:=ReadStrProperty(sl,'VendorStr');
              Device:=ReadIntProperty(sl,'Device');
              PhysicalIndex:=ReadIntProperty(sl,'PHYSIndex');
              LayoutIndex:=ReadIntProperty(sl,'LayoutIndex');
              HaId:=ReadIntProperty(sl,'HaId');
              Target:=ReadIntProperty(sl,'Target');
              Lun:=ReadIntProperty(sl,'Lun');
              PathId:=ReadIntProperty(sl,'PathID');
              Size:=ReadIntProperty(sl,'Size');
              CtlBufSize:=ReadIntProperty(sl,'CtlBufSize');
              ECCCode:=ReadIntProperty(sl,'ECCCode');
              Temperature:=ReadIntProperty(sl,'Temperature');
              DeviceType:=ReadIntProperty(sl,'DeviceType');
              MediaType:=ReadIntProperty(sl,'MediaType');
              BusType:=ReadIntProperty(sl,'BusType');
              Removable:=ReadIntProperty(sl,'Removable')=1;
              SMARTSupport:=ReadIntProperty(sl,'SMARTSupport')=1;
              SMARTActive:=ReadIntProperty(sl,'SMARTActive')=1;
              Read_CDR:=ReadIntProperty(sl,'Read_CDR')=1;
              Read_CDRW:=ReadIntProperty(sl,'Read_CDRW')=1;
              Read_DVDROM:=ReadIntProperty(sl,'Read_DVDROM')=1;
              Read_DVDR:=ReadIntProperty(sl,'Read_DVDR')=1;
              Read_DVDRAM:=ReadIntProperty(sl,'Read_DVDRAM')=1;
              Write_CDR:=ReadIntProperty(sl,'Write_CDR')=1;
              Write_CDRW:=ReadIntProperty(sl,'Write_CDRW')=1;
              Write_DVDR:=ReadIntProperty(sl,'Write_DVDR')=1;
              Write_DVDRAM:=ReadIntProperty(sl,'Write_DVDRAM')=1;
              Geometry.Cylinders.QuadPart:=ReadIntProperty(sl,'Geometry.Cylinders');
              Geometry.TracksPerCylinder:=ReadIntProperty(sl,'Geometry.TracksPerCylinder');
              Geometry.SectorsPerTrack:=ReadIntProperty(sl,'Geometry.SectorsPerTrack');
              Geometry.BytesPerSector:=ReadIntProperty(sl,'Geometry.BytesPerSectors');
              Geometry.MediaType:=ReadIntProperty(sl,'MediaType');
              HasNoSeekPenalty:=ReadIntProperty(sl,'HasNoSeekPenalty')=1;
              SSD:=ReadIntProperty(sl,'SSD')=1;
            end;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      i:=0;
      while ReadLayoutFromStream(S,AList[High(AList)].Layout,i) do
        Inc(i);
      i:=0;
      while ReadExtentsFromStream(S,AList[High(AList)].DiskExtents,i) do
        Inc(i);

      ReadIDDFromStream(S,AList[High(AList)].IdentifyDeviceData);

      Result:=True;
    finally
      S.Free;
    end;
end;

var
  i: Integer;
begin
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
      Exit;
    end;
    try
      try
        Sub1:=Sub.OpenSubStorage(Physical_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadFromStream(Self.FPHYS,IntToStr(i)) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;

      try
        Sub1:=Sub.OpenSubStorage(Logical_StorageFolderName,STG_READ_INSTORAGE,False);
      except
        Sub1:=nil;
      end;
      try
        i:=0;
        while ReadFromStream(Self.FLOG,IntToStr(i)) do
          Inc(i);
        Result:=Result or (i>0);
        SetDataAvail(Result);
      finally
        if Sub1<>nil then
         Sub1.Free;
      end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Storage.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub,Sub1: TStructuredStorage;

procedure WriteLayoutToStream(ASub: TStructuredStorage; ALayout: TDiskLayout; AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    with ALayout[AIndex] do begin
      WriteIntProperty(sl,'StartingOffset',StartingOffset.QuadPart);
      WriteIntProperty(sl,'Length',Length.QuadPart);
      WriteIntProperty(sl,'HiddenSectors',HiddenSectors);
      WriteIntProperty(sl,'Number',Number);
      WriteIntProperty(sl,'Typ',Typ);
      WriteIntProperty(sl,'BootIndicator',Integer(BootIndicator));
      WriteIntProperty(sl,'Recognized',Integer(Recognized));
      WriteIntProperty(sl,'Rewrite',Integer(Rewrite));
    end;
    strm:=ASub.OpenStream(Format(strm_Layout,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteExtentsToStream(ASub: TStructuredStorage; AExtents: TDiskExtents; AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
  sl:=TStringList.Create;
  try
    with AExtents[AIndex] do begin
      WriteIntProperty(sl,'DiskNumber',DiskNumber);
      WriteIntProperty(sl,'StartingOffset',StartingOffset.QuadPart);
      WriteIntProperty(sl,'ExtentLength',ExtentLength.QuadPart);
    end;
    strm:=ASub.OpenStream(Format(strm_Extents,[AIndex]),STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteIDDToStream(ASub: TStructuredStorage; AData: TIdentifyDeviceData);
var
  strm: TStorageStream;
  sl: TStringList;
  i: integer;
begin
  sl:=TStringList.Create;
  try
    WriteIntProperty(sl,'GeneralConfiguration',AData.GeneralConfiguration);
    WriteIntProperty(sl,'NumCylinders',AData.NumCylinders);
    WriteIntProperty(sl,'NumHeads',AData.NumHeads);
    WriteIntProperty(sl,'NumSectorsPerTrack',AData.NumSectorsPerTrack);
    WriteIntProperty(sl,'NumSectorsPerTrack',AData.NumSectorsPerTrack);
    WriteIntProperty(sl,'VendorUnique1_0',AData.VendorUnique1[0]);
    WriteIntProperty(sl,'VendorUnique1_1',AData.VendorUnique1[1]);
    WriteIntProperty(sl,'VendorUnique1_2',AData.VendorUnique1[2]);
    WriteIntProperty(sl,'Retired2_0',AData.Retired2[0]);
    WriteIntProperty(sl,'Retired2_1',AData.Retired2[1]);
    WriteIntProperty(sl,'Obsolete1',AData.Obsolete1);
    WriteIntProperty(sl,'MaximumBlockTransfer',AData.MaximumBlockTransfer);
    WriteIntProperty(sl,'VendorUnique2',AData.VendorUnique2);
    WriteIntProperty(sl,'Capabilities',AData.Capabilities);
    WriteIntProperty(sl,'TranslationFieldsValid',AData.TranslationFieldsValid);
    WriteIntProperty(sl,'NumberOfCurrentCylinders',AData.NumberOfCurrentCylinders);
    WriteIntProperty(sl,'NumberOfCurrentHeads',AData.NumberOfCurrentHeads);
    WriteIntProperty(sl,'CurrentSectorsPerTrack',AData.CurrentSectorsPerTrack);
    WriteIntProperty(sl,'CurrentSectorCapacity',AData.CurrentSectorCapacity);
    WriteIntProperty(sl,'CurrentMultiSectorSetting',AData.CurrentMultiSectorSetting);
    WriteIntProperty(sl,'MultiSectorSettingValid',AData.MultiSectorSettingValid);
    WriteIntProperty(sl,'UserAddressableSectors',AData.UserAddressableSectors);
    WriteIntProperty(sl,'MultiWordDMAActive',AData.MultiWordDMAActive);
    WriteIntProperty(sl,'MultiWordDMASupport',AData.MultiWordDMASupport);
    WriteIntProperty(sl,'AdvancedPIOModes',AData.AdvancedPIOModes);
    WriteIntProperty(sl,'MinimumMWXferCycleTime',AData.MinimumMWXferCycleTime);
    WriteIntProperty(sl,'RecommendedMWXferCycleTime',AData.RecommendedMWXferCycleTime);
    WriteIntProperty(sl,'MinimumPIOCycleTime',AData.MinimumPIOCycleTime);
    WriteIntProperty(sl,'MinimumPIOCycleTimeIORDY',AData.MinimumPIOCycleTimeIORDY);
    WriteIntProperty(sl,'QueueDepth',AData.QueueDepth);
    WriteIntProperty(sl,'MajorRevision',AData.MajorRevision);
    WriteIntProperty(sl,'MinorRevision',AData.MinorRevision);
    for i:=0 to High(AData.CommandSetSupport) do
      WriteIntProperty(sl,Format('CommandSetSupport_%d',[i]),AData.CommandSetSupport[i]);
    for i:=0 to High(AData.CommandSetActive) do
      WriteIntProperty(sl,Format('CommandSetActive_%d',[i]),AData.CommandSetActive[i]);
    WriteIntProperty(sl,'UltraDMAActive',AData.UltraDMAActive);
    WriteIntProperty(sl,'UltraDMASupport',AData.UltraDMASupport);
    WriteIntProperty(sl,'HardwareResetResult',AData.HardwareResetResult);
    WriteIntProperty(sl,'RecommendedAcousticValue',AData.RecommendedAcousticValue);
    WriteIntProperty(sl,'CurrentAcousticValue',AData.CurrentAcousticValue);
    WriteIntProperty(sl,'Max48BitLBA_0',AData.Max48BitLBA[0]);
    WriteIntProperty(sl,'Max48BitLBA_1',AData.Max48BitLBA[1]);
    WriteIntProperty(sl,'StreamingTransferTime',AData.StreamingTransferTime);
    WriteIntProperty(sl,'PhysicalLogicalSectorSize',AData.PhysicalLogicalSectorSize);
    WriteIntProperty(sl,'InterSeekDelay',AData.InterSeekDelay);
    for i:=0 to High(AData.WorldWideName) do
      WriteIntProperty(sl,Format('WorldWideName_%d',[i]),AData.WorldWideName[i]);
    for i:=0 to High(AData.ReservedForWorldWideName128) do
      WriteIntProperty(sl,Format('ReservedForWorldWideName128_%d',[i]),AData.ReservedForWorldWideName128[i]);
    WriteIntProperty(sl,'ReservedForTlcTechnicalReport',AData.ReservedForTlcTechnicalReport);
    WriteIntProperty(sl,'WordsPerLogicalSector_0',AData.WordsPerLogicalSector[0]);
    WriteIntProperty(sl,'WordsPerLogicalSector_1',AData.WordsPerLogicalSector[1]);
    WriteIntProperty(sl,'CommandSetSupportExt',AData.CommandSetSupportExt);
    WriteIntProperty(sl,'CommandSetActiveExt',AData.CommandSetActiveExt);
    for i:=0 to High(AData.ReservedForExpandedSupportandActive) do
      WriteIntProperty(sl,Format('ReservedForExpandedSupportandActive_%d',[i]),AData.ReservedForExpandedSupportandActive[i]);
    WriteIntProperty(sl,'MsnSupport',AData.MsnSupport);
    WriteIntProperty(sl,'SecurityStatus',AData.SecurityStatus);
    WriteIntProperty(sl,'CfaPowerModel',AData.CfaPowerModel);
    for i:=0 to High(AData.ReservedForCfaWord161) do
      WriteIntProperty(sl,Format('ReservedForCfaWord161_%d',[i]),AData.ReservedForCfaWord161[i]);
    WriteIntProperty(sl,'DataSetManagementFeature',AData.DataSetManagementFeature);
    for i:=0 to High(AData.ReservedForCfaWord170) do
      WriteIntProperty(sl,Format('ReservedForCfaWord170_%d',[i]),AData.ReservedForCfaWord170[i]);
    WriteIntProperty(sl,'BlockAlignment',AData.BlockAlignment);
    WriteIntProperty(sl,'WriteReadVerifySectorCountMode3Only_0',AData.WriteReadVerifySectorCountMode3Only[0]);
    WriteIntProperty(sl,'WriteReadVerifySectorCountMode3Only_1',AData.WriteReadVerifySectorCountMode3Only[1]);
    WriteIntProperty(sl,'WriteReadVerifySectorCountMode2Only_0',AData.WriteReadVerifySectorCountMode2Only[0]);
    WriteIntProperty(sl,'WriteReadVerifySectorCountMode2Only_1',AData.WriteReadVerifySectorCountMode2Only[1]);
    WriteIntProperty(sl,'NVCacheCapabilities',AData.NVCacheCapabilities);
    WriteIntProperty(sl,'NVCacheSizeLSW',AData.NVCacheSizeLSW);
    WriteIntProperty(sl,'NVCacheSizeMSW',AData.NVCacheSizeMSW);
    WriteIntProperty(sl,'NominalMediaRotationRate',AData.NominalMediaRotationRate);
    for i:=0 to High(AData.ReservedWord220) do
      WriteIntProperty(sl,Format('ReservedWord220_%d',[i]),AData.ReservedWord220[i]);
    WriteIntProperty(sl,'NVCacheEstimatedTimeToSpinUpInSeconds',AData.NVCacheEstimatedTimeToSpinUpInSeconds);
    WriteIntProperty(sl,'CheckSum',AData.CheckSum);
    WriteIntProperty(sl,'Signature',AData.Signature);
    strm:=ASub.OpenStream(strm_IDD,STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
  finally
    sl.Free;
  end;
end;

procedure WriteToStream(AList: TDeviceInfoArray; AName: string; AIndex: Integer);
var
  S: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
begin
  S:=Sub1.OpenSubStorage(AName,STG_OPEN,True);
  sl:=TStringList.Create;
  try
    if {$IFDEF UNICODE}CharInSet(AList[AIndex].Drive,['a'..'z','A'..'Z']){$ELSE}AList[AIndex].Drive in ['a'..'z','A'..'Z']{$ENDIF} then
      WriteStrProperty(sl,'Drive',AList[AIndex].Drive)
    else
      WriteStrProperty(sl,'Drive',IntToStr(Ord(AList[AIndex].Drive)));
//    WriteStrProperty(sl,'Vendor',AList[AIndex].Vendor);
    WriteStrProperty(sl,'Model',AList[AIndex].Model);
    WriteStrProperty(sl,'Revision',AList[AIndex].Revision);
//    WriteStrProperty(sl,'VendorStr',AList[AIndex].VendorStr);
    WriteStrProperty(sl,'SerialNumber',AList[AIndex].SerialNumber);
    WriteIntProperty(sl,'DeviceType',AList[AIndex].DeviceType);
    WriteIntProperty(sl,'MediaType',AList[AIndex].MediaType);
    WriteIntProperty(sl,'BusType',AList[AIndex].BusType);
    WriteIntProperty(sl,'Removable',Integer(AList[AIndex].Removable));
    WriteIntProperty(sl,'Device',AList[AIndex].Device);
    WriteIntProperty(sl,'PHYSIndex',AList[AIndex].PhysicalIndex);
    WriteIntProperty(sl,'LayoutIndex',AList[AIndex].LayoutIndex);
    WriteIntProperty(sl,'HaId',AList[AIndex].HaId);
    WriteIntProperty(sl,'Target',AList[AIndex].Target);
    WriteIntProperty(sl,'Lun',AList[AIndex].Lun);
    WriteIntProperty(sl,'PathID',AList[AIndex].PathId);
    WriteIntProperty(sl,'Size',AList[AIndex].Size);
    WriteIntProperty(sl,'CtlBufSize',AList[AIndex].CtlBufSize);
    WriteIntProperty(sl,'ECCCode',AList[AIndex].ECCCode);
    WriteIntProperty(sl,'Temperature',AList[AIndex].Temperature);
    WriteIntProperty(sl,'SMARTSupport',Integer(AList[AIndex].SMARTSupport));
    WriteIntProperty(sl,'SMARTActive',Integer(AList[AIndex].SMARTActive));
    WriteIntProperty(sl,'Read_CDR',Integer(AList[AIndex].Read_CDR));
    WriteIntProperty(sl,'Read_CDRW',Integer(AList[AIndex].Read_CDRW));
    WriteIntProperty(sl,'Write_CDR',Integer(AList[AIndex].Write_CDR));
    WriteIntProperty(sl,'Write_CDRW',Integer(AList[AIndex].Write_CDRW));
    WriteIntProperty(sl,'Read_DVDROM',Integer(AList[AIndex].Read_DVDROM));
    WriteIntProperty(sl,'Read_DVDR',Integer(AList[AIndex].Read_DVDR));
    WriteIntProperty(sl,'Read_DVDRAM',Integer(AList[AIndex].Read_DVDRAM));
    WriteIntProperty(sl,'Write_DVDR',Integer(AList[AIndex].Write_DVDR));
    WriteIntProperty(sl,'Write_DVDRAM',Integer(AList[AIndex].Write_DVDRAM));
    WriteIntProperty(sl,'Geometry.Cylinders',AList[AIndex].Geometry.Cylinders.QuadPart);
    WriteIntProperty(sl,'Geometry.TracksPerCylinder',AList[AIndex].Geometry.TracksPerCylinder);
    WriteIntProperty(sl,'Geometry.SectorsPerTrack',AList[AIndex].Geometry.SectorsPerTrack);
    WriteIntProperty(sl,'Geometry.BytesPerSectors',AList[AIndex].Geometry.BytesPerSector);
    WriteIntProperty(sl,'HasNoSeekPenalty',Integer(AList[AIndex].HasNoSeekPenalty));
    WriteIntProperty(sl,'SSD',Integer(AList[AIndex].SSD));
    WriteIntProperty(sl,'Kind',Integer(AList[AIndex].Kind));
    strm:=S.OpenStream(strm_Props,STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;

    if Length(AList[AIndex].Layout)>0 then
      for i:=0 to High(AList[AIndex].Layout) do
        WriteLayoutToStream(S,AList[AIndex].Layout,i);

    if Length(AList[AIndex].DiskExtents)>0 then
      for i:=0 to High(AList[AIndex].DiskExtents) do
        WriteExtentsToStream(S,AList[AIndex].DiskExtents,i);

    WriteIDDToStream(S,AList[AIndex].IdentifyDeviceData);

  finally
    sl.Free;
    S.Free;
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
      Sub1:=Sub.OpenSubStorage(Physical_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to High(Self.FPHYS) do
          WriteToStream(Self.FPHYS,IntToStr(i),i);
      finally
        Sub1.Free;
      end;
      Sub1:=Sub.OpenSubStorage(Logical_StorageFolderName,STG_OPEN,True);
      try
        for i:=0 to High(Self.FLOG) do
          WriteToStream(Self.FLOG,IntToStr(i),i);
      finally
        Sub1.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.





{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Devices Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_Devices;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_NTDDK;

const
  StorageFolderName = 'Devices';

type
  TDevice = record
    ClassGUID: TGUID;
    Name,
    ClassName,
    ClassDesc,
    ClassIcon,
    FriendlyName,
    Description,
    GUID,
    Manufacturer,
    Location: String;
    PCINumber,
    DeviceNumber,
    FunctionNumber: Integer;
    HardwareID,
    SymbolicLink,
    Subkey,
    DeviceParam,
    Driver,
    DriverDate,
    DriverVersion,
    DriverProvider,
    DriverKey: string;
    InfPath,
    Service,
    ServiceName,
    ServiceGroup: string;
    ServiceType: integer;
    ImagePath: string;
    RegKey: string;
    ResourceListKey,
    ResourceListValue: string;
    VendorID,
    DeviceID,
    SubSysID,
    Revision: Cardinal;
    DriverDatetime,
    InstallDate,
    FirstInstallDate,
    LastArrivalDate,
    LastRemovalDate: TDateTime;
    InstallID,
    IconPath: string;
  end;

  TDeviceList = array of TDevice;

  TResourceItem = record
    Resource: string;
    Share: CM_SHARE_DISPOSITION;
    Device: string;
    DeviceClassGUID :TGUID;
  end;

  TResourceList = array of TResourceItem;

  TMiTeC_Devices = class(TMiTeC_Component)
  private
    FCount: integer;
    FDeviceList: TDeviceList;
    FCheckControl: Boolean;
    FComputer: string;
    FSC: string;
    function GetCount: integer;
    function GetDevice(Index: integer): TDevice;
    function GetDeviceCount: integer;
    //procedure ScanDevices_Registry(var ADeviceList: TDeviceList);
    procedure ScanDevices_CfgMgr(var ADeviceList: TDeviceList);
    procedure ClearList;
    function Add(ARecord: TDevice): Integer;
    function GetDeviceByName(AName: string): TDevice;
    procedure Sort;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    procedure GetResourceList(var RL: TResourceList);
    function GetDeviceByHardwareID(AHWID: string): Integer;
    function GetDeviceByHardwareIDAndDriver(AHWID,ADriver: string): Integer;
    property Devices[Index: integer]: TDevice read GetDevice;
    property DeviceByName[AName: string]: TDevice read GetDeviceByName;
  published
    property CheckControl: Boolean read FCheckControl write FCheckControl;
    property DeviceCount: integer read GetCount stored False;

    property Computer: string read FComputer stored False;
    property SoundCard: string read FSC stored False;
  end;

procedure GetDeviceResources(ADevice: TDevice; var DR: TDeviceResources);

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}
     Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_StrUtils, MiTeC_Datetime, MiTeC_RegUtils, MiTeC_CfgMgrSetupAPI;

procedure GetDeviceResources(ADevice: TDevice; var DR: TDeviceResources);
begin
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(Adevice.ResourceListKey,False) then begin
        ReadDeviceResourcesFromRegistry(CurrentKey,ADevice.ResourceListValue,DR);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure GetDeviceService(AGUID :string; var AName, AGroup, AImage: string; var AType: integer);
var
  s: string;
  rc: Integer;
const
  rvName = 'DisplayName';
  rvGroup = 'Group';
  rvType = 'Type';
  rvImage = 'ImagePath';
  rkClass = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Services';
begin
  if AGUID='' then
    Exit;
  AName:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkClass+'\'+AGUID,rvName,False);
  if Pos(';',AName)>0 then
    AName:=Copy(Aname,Pos(';',Aname)+1,1024);
  AGroup:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkClass+'\'+AGUID,rvGroup,False);
  AImage:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkClass+'\'+AGUID,rvImage,False);
  s:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkClass+'\'+AGUID,rvType,False);
  Val(s,AType,rc);
end;

function GetDeviceLastArrival(AKey: string): TDatetime;
var
  rki: TRegKeyInfo;
begin
  Result:=0;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(IncludeTrailingPathDelimiter(AKey)+'Control',False) then begin
        GetKeyInfo(rki);
        Result:={$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime);
        REsult:=UTCToSystemTime(Result);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

procedure ParseHardwareID(HID: string; var VEN,DEV,SUBSYS,REV: Cardinal);
var
  p: Cardinal;
begin
  VEN:=0;
  DEV:=0;
  SUBSYS:=0;
  REV:=0;
  if Pos('PCI\VEN',HID)=0 then
    Exit;
  p:=Pos('VEN_',HID);
  if p>0 then
    VEN:=Cardinal(StrToIntDef('$'+Copy(HID,p+4,4),0));
  p:=Pos('DEV_',HID);
  if p>0 then
    DEV:=Cardinal(StrToIntDef('$'+Copy(HID,p+4,4),0));
  p:=Pos('SUBSYS_',HID);
  if p>0 then
    SUBSYS:=Cardinal(StrToIntDef('$'+Copy(HID,p+7,8),0));
  p:=Pos('REV_',HID);
  if p>0 then
    REV:=Cardinal(StrToIntDef('$'+Copy(HID,p+4,2),0));
end;

procedure GetDeviceDriver(AGUID :string; var ARecord: TDevice);
var
  rkClass: string;
const
  rkClassNT = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\Class';
  rvDate = 'DriverDate';
  rvVersion = 'DriverVersion';
  rvProvider = 'ProviderName';
  rvINFPath = 'InfPath';
begin
  rkClass:=rkClassNT;
  AGUID:=StringReplace(AGUID,'\\','\',[rfReplaceAll,rfIgnoreCase]);
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkClass+'\'+AGUID,False) then begin
        ARecord.DriverDate:=ReadString(rvDate);
        ARecord.DriverVersion:=ReadString(rvVersion);
        ARecord.DriverProvider:=ReadString(rvProvider);
        ARecord.InfPath:=ReadString(rvInfPath);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

{ TMiTeC_Devices }

constructor TMiTeC_Devices.Create;
begin
  inherited Create(AOwner);
  FCheckControl:=OS<osWin8;
  FComputer:='';
  FSC:='';
end;

destructor TMiTeC_Devices.Destroy;
begin
  ClearList;
  inherited;
end;

function TMiTeC_Devices.GetCount: integer;
begin
  Result:=Length(FDeviceList);
end;

function TMiTeC_Devices.GetDevice(Index: integer): TDevice;
begin
  Result:=FDeviceList[Index];
end;

function TMiTeC_Devices.GetDeviceByHardwareID(AHWID: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FDeviceList) do
    if SameText(FDeviceList[i].HardwareID,AHWID) then begin
      Result:=i;
      Break;
    end;
end;

function TMiTeC_Devices.GetDeviceByHardwareIDAndDriver(AHWID,
  ADriver: string): Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FDeviceList) do
    if SameText(FDeviceList[i].HardwareID,AHWID) and SameText(FDeviceList[i].Driver,ADriver) then begin
      Result:=i;
      Break;
    end;
end;

function TMiTeC_Devices.GetDeviceByName(AName: string): TDevice;
var
  i: Integer;
begin
  ResetMemory(Result,SizeOf(Result));
  for i:=0 to High(FDeviceList) do
    if SameText(FDeviceList[i].Name,AName) then begin
      Result:=FDeviceList[i];
      Break;
    end;
end;

function TMiTeC_Devices.GetDeviceCount: integer;
begin
  Result:=Length(FDeviceList);
end;

procedure TMiTeC_Devices.RefreshData;
var
  i,k,p: Integer;
begin
  inherited;
  ScanDevices_CfgMgr(FDeviceList);
  Sort;
  FCount:=GetDeviceCount;
  FComputer:='';
  FSC:='';
  k:=-1;
  for i:=0 to DeviceCount-1 do begin
    if SameText(Devices[i].ClassName,'Computer') and (FComputer='') then
      FComputer:=Devices[i].Name;

    if SameText(Devices[i].ClassName,'Media') then begin
      p:=Pos('AUDIO',Devices[i].HardwareID);
      if (Devices[i].PCINumber>-1) or (p>0) then
        if (Devices[i].FunctionNumber<k) or (p>0) then begin
          k:=Devices[i].FunctionNumber;
          FSC:=Devices[i].Name;
        end;
    end;
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_Devices.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      WriteStrProperty(sl,'Name',Self.Devices[AIndex].Name);
      WriteStrProperty(sl,'ClassName',Self.Devices[AIndex].ClassName);
      WriteStrProperty(sl,'ClassDesc',Self.Devices[AIndex].ClassDesc);
      WriteStrProperty(sl,'ClassIcon',Self.Devices[AIndex].ClassIcon);
      WriteStrProperty(sl,'FriendlyName',Self.Devices[AIndex].FriendlyName);
      WriteStrProperty(sl,'Description',Self.Devices[AIndex].Description);
      WriteStrProperty(sl,'GUID',Self.Devices[AIndex].GUID);
      WriteStrProperty(sl,'Manufacturer',Self.Devices[AIndex].Manufacturer);
      WriteStrProperty(sl,'Location',Self.Devices[AIndex].Location);
      WriteStrProperty(sl,'HardwareID',Self.Devices[AIndex].HardwareID);
      WriteStrProperty(sl,'SymbolicLink',Self.Devices[AIndex].SymbolicLink);
      WriteStrProperty(sl,'DeviceParam',Self.Devices[AIndex].DeviceParam);
      WriteStrProperty(sl,'Driver',Self.Devices[AIndex].Driver);
      WriteStrProperty(sl,'DriverDate',Self.Devices[AIndex].DriverDate);
      WriteStrProperty(sl,'DriverVersion',Self.Devices[AIndex].DriverVersion);
      WriteStrProperty(sl,'DriverProvider',Self.Devices[AIndex].DriverProvider);
      WriteStrProperty(sl,'InfPath',Self.Devices[AIndex].InfPath);
      WriteStrProperty(sl,'Service',Self.Devices[AIndex].Service);
      WriteStrProperty(sl,'ServiceName',Self.Devices[AIndex].ServiceName);
      WriteStrProperty(sl,'ServiceGroup',Self.Devices[AIndex].ServiceGroup);
      WriteIntProperty(sl,'ServiceType',Self.Devices[AIndex].ServiceType);
      WriteStrProperty(sl,'RegKey',Self.Devices[AIndex].RegKey);
      WriteStrProperty(sl,'ResourceListKey',Self.Devices[AIndex].ResourceListKey);
      WriteStrProperty(sl,'ResourceListValue',Self.Devices[AIndex].ResourceListValue);
      WriteIntProperty(sl,'PCINumber',Self.Devices[AIndex].PCINumber);
      WriteIntProperty(sl,'DeviceNumber',Self.Devices[AIndex].DeviceNumber);
      WriteIntProperty(sl,'FunctionNumber',Self.Devices[AIndex].FunctionNumber);
      WriteIntProperty(sl,'VendorID',Self.Devices[AIndex].VendorID);
      WriteIntProperty(sl,'DeviceID',Self.Devices[AIndex].DeviceID);
      WriteIntProperty(sl,'SubSysID',Self.Devices[AIndex].SubSysID);
      WriteIntProperty(sl,'Revision',Self.Devices[AIndex].Revision);
      WriteDtProperty(sl,'LastArrivalDate',Self.Devices[AIndex].LastRemovalDate);
      strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_OPEN,True);
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
      for i:=0 to Self.DeviceCount-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Devices.ScanDevices_CfgMgr(var ADeviceList: TDeviceList);
var
  dinfo: TSPDevInfoData;
  intf: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  n,pt: Cardinal;
  i: Integer;
  hdev: HDEVINFO;
  g: TGUID;
  ft: TFiletime;
  buf: array[0..{$IFDEF UNICODE}512{$ELSE}255{$ENDIF}] of byte;
  dr: TDevice;
  hid: string;
begin
  Clear;
  hdev:=SetupDiGetClassDevs(nil,nil,0,DIGCF_ALLCLASSES or DIGCF_PRESENT or DIGCF_PROFILE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then
    try
      i:=0;
      pt:=0;
      dinfo.cbSize:=sizeof(TSPDevInfoData);
      while SetupDiEnumDeviceInfo(hDev,i,dinfo) do begin
        ResetMemory(dr,sizeof(dr));
        Zeromemory(@buf,sizeof(buf));
        hid:='';
        intf.cbSize:=sizeof(TSPDeviceInterfaceData);
        if SetupDiCreateDeviceInterface(hDev,dinfo,dinfo.ClassGuid,nil,0,@intf) then begin
          n:=0;
          SetupDiGetDeviceInterfaceDetail(hdev,@intf,nil,0,n,nil);
          if (GetLastError=ERROR_INSUFFICIENT_BUFFER) then begin
            pdidd:=AllocMem(n);
            try
              pdidd.cbSize:=SizeOf(TSPDeviceInterfaceDetailData);
              dinfo.cbSize:=sizeof(TSPDevInfoData);
              if (SetupDiGetDeviceInterfaceDetail(hdev,@intf,pdidd,n,n,@dinfo)) then begin
                dr.SymbolicLink:=PChar(@(pdidd.DevicePath));
                hid:=UpperCase(FastStringReplace(Copy(dr.SymbolicLink,5,Pos('{',dr.SymbolicLink)-5),'#','\'));
                dr.RegKey:='\SYSTEM\CurrentControlSet\Enum\'+hid;

                dr.ClassName:=GetString(hdev,dinfo,SPDRP_CLASS);
                dr.ClassGUID:=GetGUID(hdev,dinfo,SPDRP_CLASSGUID);
                dr.GUID:=GUIDToString(dr.ClassGUID);
                dr.Description:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
                dr.HardwareID:=ExtractFilename(GetString(hdev,dinfo,SPDRP_HARDWAREID));
                dr.Manufacturer:=GetString(hdev,dinfo,SPDRP_MFG);
                dr.FriendlyName:=GetString(hdev,dinfo,SPDRP_FRIENDLYNAME);
                dr.Location:=GetString(hdev,dinfo,SPDRP_LOCATION_INFORMATION);
                dr.Service:=GetString(hdev,dinfo,SPDRP_SERVICE);
                dr.DriverKey:=GetString(hdev,dinfo,SPDRP_DRIVER);
              end;
              finally
                FreeMem(pdidd);
              end;
            end;
        end else
          dr.ClassGUID:=dinfo.ClassGuid;

        Zeromemory(@buf,sizeof(buf));
        SetupDiGetClassDescription(dr.ClassGUID,@buf,sizeof(buf),@n);
        dr.ClassDesc:=string(PChar(@buf));
        if Assigned(SetupDiGetDeviceProperty) then begin
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Class,pt,@buf,sizeof(buf),nil,0);
          dr.ClassName:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_ClassGuid,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],g,sizeof(g));
          dr.ClassGUID:=g;
          dr.GUID:=GUIDToString(g);
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_FriendlyName,pt,@buf,sizeof(buf),nil,0);
          dr.FriendlyName:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DeviceDesc,pt,@buf,sizeof(buf),nil,0);
          dr.Description:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Manufacturer,pt,@buf,sizeof(buf),nil,0);
          dr.Manufacturer:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_HardwareIds,pt,@buf,sizeof(buf),nil,0);
          dr.HardwareID:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Service,pt,@buf,sizeof(buf),nil,0);
          dr.Service:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_LocationInfo,pt,@buf,sizeof(buf),nil,0);
          dr.Location:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));

          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Driver,pt,@buf,sizeof(buf),nil,0);
          dr.DriverKey:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DriverDesc,pt,@buf,sizeof(buf),nil,0);
          dr.Driver:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DriverVersion,pt,@buf,sizeof(buf),nil,0);
          dr.DriverVersion:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DriverProvider,pt,@buf,sizeof(buf),nil,0);
          dr.DriverProvider:=string(PChar(@buf));
          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DriverDate,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],ft,sizeof(ft));
          dr.DriverDatetime:=FileTimeTodatetime(ft);
          dr.DriverDate:=DateTimeToStr(dr.DriverDatetime);

          Zeromemory(@buf,sizeof(buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_InstanceId,pt,@buf,sizeof(buf),nil,0);
          dr.InstallID:=string(PChar(@buf));
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_InstallDate,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],ft,sizeof(ft));
          dr.InstallDate:=FileTimeTodatetime(ft,True);
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_FirstInstallDate,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],ft,sizeof(ft));
          dr.FirstInstallDate:=FileTimeTodatetime(ft,True);
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_LastArrivalDate,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],ft,sizeof(ft));
          dr.LastArrivalDate:=FileTimeTodatetime(ft,True);
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_LastRemovalDate,pt,@buf,sizeof(buf),nil,0);
          Move(buf[0],ft,sizeof(ft));
          dr.LastRemovalDate:=FileTimeTodatetime(ft,True);

          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_DeviceClass_Icon,pt,@buf,sizeof(buf),nil,0);
          dr.IconPath:=string(PChar(@buf));

          if (dr.SymbolicLink='') and (dr.InstallID<>'') then begin
            dr.SymbolicLink:='\\?\'+FastStringReplace(dr.InstallID,'\','#')+'#'+dr.GUID;
            dr.RegKey:='\SYSTEM\CurrentControlSet\Enum\'+dr.InstallID;
          end;
        end;

        if Trim(dr.FriendlyName)='' then
          dr.Name:=dr.Description
        else
          dr.Name:=dr.FriendlyName;
        GetDeviceService(dr.Service,dr.ServiceName,dr.ServiceGroup,dr.ImagePath,dr.ServiceType);
        if Assigned(SetupDiGetDeviceProperty) then
          ParseHardwareID(dr.HardwareID,dr.VendorID,dr.DeviceID,dr.SubSysID,dr.Revision)
        else begin
          ParseHardwareID(hid,dr.VendorID,dr.DeviceID,dr.SubSysID,dr.Revision);
          GetDeviceDriver(dr.DriverKey,dr);
          dr.LastArrivalDate:=GetDeviceLastArrival(dr.RegKey);
        end;
        GetLocation(dr.Location,dr.PCINumber,dr.DeviceNumber,dr.FunctionNumber);
        GetResourceListLocation(dr.RegKey,dr.ResourceListKey,dr.ResourceListValue);

        if dr.ClassDesc='' then
          dr.ClassDesc:='Other devices';
        if (dr.Name<>'') then
          Add(dr);
        inc(i);
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
end;

(*
procedure TMiTeC_Devices.ScanDevices_Registry(var ADeviceList: TDeviceList);

const
  rkClassNT = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\Class';

function GetSymbolicLink(AHwId,ASubkey: string): string;
const
  rk = {HKEY_LOCAL_MACHINE\}'SYSTEM\CurrentControlSet\Control\DeviceClasses';
  rvSN = 'SymbolicLink';
var
  i: Integer;
  sl1: TStringList;
  s,d: string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rk,False) then begin
        sl1:=TStringList.Create;
        try
          d:='#';
          s:='?';
          sl1.Text:=AHwId;
          if sl1.Count>0 then
            AHwId:=StringReplace(sl1[0],'\',d,[rfReplaceAll,rfIgnoreCase])
          else
            Exit;
          s:=Format('##%s#%s#%s',[s,AHwId,ASubkey]);
          sl1.Clear;
          GetKeynames(sl1);
          CloseKey;
          for i:=0 to sl1.Count-1 do begin
            d:=Format('%s\%s\%s%',[rk,sl1[i],s])+'#'+sl1[i]+'\#';
            if OpenKey(d,False) then begin
              if ValueExists(rvSN) then begin
                Result:=ReadString(rvSN);

              end;
              CloseKey;
              Break;
           end;
         end;
        finally
          sl1.Free;
        end;
      end;
    finally
      Free;
    end;
end;

procedure GetDeviceClassName(const AGUID :string; var ARecord: TDevice);
var
  i,p :integer;
  sl :TStringList;
  s,rkClass: string;
const
  rvClass = 'Class';
  rvIcon = 'Icon';
  rvLink = 'Link';
  rvClassDesc = 'ClassDesc';
begin
  rkClass:=rkClassNT;
  with OpenRegistryReadOnly do begin
    RootKey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkClass,False) then begin
      sl:=TStringList.Create;
      GetKeyNames(sl);
      CloseKey;
      i:=sl.IndexOf(AGUID);
      if i>-1 then
        if OpenKey(rkClass+'\'+sl[i],False) then begin
          ARecord.ClassName:=ReadString(rvClass);
          case GetDataType(rvIcon) of
            rdString, rdExpandString: ARecord.ClassIcon:=ReadString(rvIcon);
            rdInteger: ARecord.ClassIcon:=IntToStr(ReadInteger(rvIcon));
            else  ARecord.ClassIcon:='';
          end;
          ARecord.ClassDesc:=ReadString('');
          if ARecord.ClassDesc='' then
            ARecord.ClassDesc:=ReadString(rvClassDesc);
          if (Pos('@',ARecord.ClassDesc)=1) then begin
            s:=Copy(ARecord.ClassDesc,2,Length(ARecord.ClassDesc));
            p:=Pos(',',s);
            if p>0 then begin
              ARecord.ClassDesc:=Copy(s,1,p-1);
              ARecord.ClassDesc:=ExpandEnvVars(ARecord.ClassDesc);
              s:=Copy(s,p+1,255);
              try
                ARecord.ClassDesc:=LoadResourceString(ARecord.ClassDesc,Cardinal(StrToInt(s)));
              except
                ARecord.ClassDesc:=s;
                if Pos(';',ARecord.ClassDesc)>0 then
                  ARecord.ClassDesc:=Copy(ARecord.ClassDesc,Pos(';',ARecord.ClassDesc)+1,1024);
              end;
            end;
          end;
          CloseKey;
        end;
      sl.Free;
    end;
    Free;
  end;
end;

var
  i,j,k,l :integer;
  sl1,sl2,sl3,sl4 :TStringList;
  dr: TDevice;
  rkEnum: string;
  Data: PChar;
  rki: TRegKeyInfo;
const
  rvClass = 'Class';
  rvGUID = 'ClassGUID';
  rvDesc = 'DeviceDesc';
  rvFriend = 'FriendlyName';
  rvMfg = 'Mfg';
  rvService = 'Service';
  rvLoc = 'LocationInformation';
  rvDriver = 'Driver';
  rvHID = 'HardwareID';
  rvHWKey = 'HardwareKey';

  rkEnumNT = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Enum';
  rkConfigManager = {HKEY_DYN_DATA}'\Config Manager\Enum';

  rkControl = 'Control';
  rkDeviceParams = 'Device Parameters';

begin
  Clear;
  sl1:=TStringList.Create;
  sl2:=TStringList.Create;
  sl3:=TStringList.Create;
  sl4:=TStringList.Create;
  Data:=Allocmem(255);

  try

  rkEnum:=rkEnumNT;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkEnumNT,False) then begin
        GetKeyNames(sl1);
        CloseKey;
        for i:=0 to sl1.Count-1 do
          if OpenKey(rkEnum+'\'+sl1[i],False) then begin
            GetKeyNames(sl2);
            CloseKey;
            for j:=0 to sl2.count-1 do
              if OpenKey(rkEnum+'\'+sl1[i]+'\'+sl2[j],False) then begin
                GetKeyNames(sl3);
                CloseKey;
                for k:=0 to sl3.count-1 do
                  if OpenKey(rkEnum+'\'+sl1[i]+'\'+sl2[j]+'\'+sl3[k],False) then begin
                    if not FCheckControl or KeyExists(rkControl) then begin
                      ResetMemory(dr,SizeOf(dr));
                      with dr do begin
                        GUID:=UpperCase(ReadString(rvGUID));
                        FriendlyName:=ReadString(rvFriend);
                        if Pos(';',FriendlyName)>0 then
                          FriendlyName:=Copy(FriendlyName,Pos(';',FriendlyName)+1,1024);
                        Description:=ReadString(rvDesc);
                        if Pos(';',Description)>0 then
                          Description:=Copy(Description,Pos(';',Description)+1,1024);
                        if Trim(FriendlyName)='' then
                          Name:=Description
                        else
                          Name:=FriendlyName;
                        Manufacturer:=ReadString(rvMfg);
                        if Pos(';',Manufacturer)>0 then
                          Manufacturer:=Copy(Manufacturer,Pos(';',Manufacturer)+1,1024);
                        Service:=ReadString(rvService);
                        Location:=ReadString(rvLoc);
                        GetLocation(Location,PCINumber,DeviceNumber,FunctionNumber);
                        if Location='' then
                          GetDeviceService(sl1[i],Location,ServiceGroup,ImagePath,ServiceType);
                        GetDeviceClassName(GUID,dr);
                        if dr.ClassName='' then
                          dr.ClassName:=ReadString(rvClass);
                        Driver:=ReadString(rvDriver);
                        GetDeviceDriver(Driver,dr);
                        GetDeviceService(Service,ServiceName,ServiceGroup,ImagePath,ServiceType);
                        RegKey:=rkEnum+'\'+sl1[i]+'\'+sl2[j]+'\'+sl3[k];
                        Subkey:=sl3[k];
                        HardwareID:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkEnum+'\'+sl1[i]+'\'+sl2[j]+'\'+sl3[k],rvHID,False);
                        ParseHardwareID(HardwareID,VendorID,DeviceID,SubSysID,Revision);
                        GetResourceListLocation(RegKey,ResourceListKey,ResourceListValue);
                        SymbolicLink:=GetSymbolicLink(HardwareId,Subkey);
                        CloseKey;
                        if OpenKey(rkEnum+'\'+sl1[i]+'\'+sl2[j]+'\'+sl3[k]+'\'+rkControl,False) then begin
                          GetKeyInfo(rki);
                          TimeStamp:={$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime);
                          if ConvertTimeToLocal then
                            TimeStamp:=UTCToSystemTime(TimeStamp);
                          CloseKey;
                        end;
                        if OpenKey(rkEnum+'\'+sl1[i]+'\'+sl2[j]+'\'+sl3[k]+'\'+rkDeviceParams,False) then begin
                          GetValueNames(sl4);
                          for l:=0 to sl4.Count-1 do
                            if (sl4[l]<>'') and (GetDataType(sl4[l])=rdString) then begin
                              Deviceparam:=ReadString(sl4[l]);
                              Break;
                            end;
                          CloseKey;
                        end;
                      end;
                      if (Trim(dr.ClassName)<>'') and (GetDeviceByHardwareIDAndDriver(dr.HardwareID,dr.Driver)=-1) then begin
                        dr.DeviceClass:=GetDeviceClass(dr.ClassName);
                        Add(dr);
                      end;
                    end;
                  end;
              end;
          end;
      end;
  finally
    Free;
  end;

  finally
  Freemem(Data);
  sl1.free;
  sl2.Free;
  sl3.Free;
  sl4.Free;
  end;
end;
*)

procedure TMiTeC_Devices.Sort;
var
 i,j: Integer;
 r: TDevice;
begin
  for i:=0 to High(FDeviceList)-1 do
    for j:=High(FDeviceList) downto i+1 do
      if CompareStr(Format('%s_%s',[FDeviceList[i].ClassName,FDeviceList[i].Name]),
                    Format('%s_%s',[FDeviceList[j].ClassName,FDeviceList[j].Name]))>0 then begin
        r:=FDeviceList[i];
        FDeviceList[i]:=FDeviceList[j];
        FDeviceList[j]:=r;
      end;
end;

function TMiTeC_Devices.Add;
begin
  SetLength(FDeviceList,Length(FDeviceList)+1);
  Result:=High(FDeviceList);
  FDeviceList[Result]:=ARecord;
end;

procedure TMiTeC_Devices.Clear;
begin
  ClearList;
end;

procedure TMiTeC_Devices.ClearList;
var
  i: Integer;
begin
  for i:=0 to High(FDeviceList) do
    Finalize(FDeviceList[i]);
  Finalize(FDeviceList);
end;

procedure TMiTeC_Devices.GetResourceList(var RL: TResourceList);
var
  i,j: Integer;
  d: TDevice;
  dr: TDeviceResources;
  ri: TResourceItem;
begin
  SetLength(RL,0);
  for i:=0 to DeviceCount-1 do begin
    d:=Devices[i];
    if LiveData and not Empty(d.ResourceListKey) then begin
      GetDeviceResources(d,dr);
      for j:=0 to High(dr.Resources) do begin
        ri.Share:=dr.Resources[j].ShareDisposition;
        ri.Device:=d.Name;
        ri.DeviceClassGUID:=d.ClassGUID;
        case dr.Resources[j].Typ of
          CmResourceTypePort:
            ri.Resource:=Format('%s %4.4x - %4.4x',[DeviceResourceTypeStr(dr.Resources[j].Typ),
                                           dr.Resources[j].Port.Start.QuadPart,
                                           dr.Resources[j].Port.Start.QuadPart+dr.Resources[j].Port.Length-1]);
          CmResourceTypeInterrupt:
            ri.Resource:=Format('%s %2.2d',[DeviceResourceTypeStr(dr.Resources[j].Typ),
                                         dr.Resources[j].Interrupt.Vector]);
          CmResourceTypeMemory:
            ri.Resource:=Format('%s %8.8x - %8.8x',[DeviceResourceTypeStr(dr.Resources[j].Typ),
                                           dr.Resources[j].Memory.Start.QuadPart,
                                           dr.Resources[j].Memory.Start.QuadPart+dr.Resources[j].Memory.Length-1]);
          CmResourceTypeDma:
            ri.Resource:=Format('%s %2.2d',[DeviceResourceTypeStr(dr.Resources[j].Typ),
                                         dr.Resources[j].DMA.Channel]);
        end;
        if not Empty(ri.Resource) then begin
          SetLength(RL,Length(RL)+1);
          RL[High(RL)]:=ri;
        end;
      end;
    end;
  end;
end;

function TMiTeC_Devices.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
  dr: TDevice;
begin
  Result:=False;
  try
    strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
    if strm<>nil then
      try
        sl:=TStringList.Create;
        try
          LoadFromEncodedStream(strm,sl,ACodeStream);
          dr.Name:=ReadStrProperty(sl,'Name');
          dr.ClassName:=ReadStrProperty(sl,'ClassName');
          dr.ClassDesc:=ReadStrProperty(sl,'ClassDesc');
          dr.ClassIcon:=ReadStrProperty(sl,'ClassIcon');
          dr.FriendlyName:=ReadStrProperty(sl,'FriendlyName');
          dr.Description:=ReadStrProperty(sl,'Description');
          dr.GUID:=ReadStrProperty(sl,'GUID');
          dr.ClassGUID:=StringToGUID(dr.GUID);
          dr.Manufacturer:=ReadStrProperty(sl,'Manufacturer');
          dr.Location:=ReadStrProperty(sl,'Location');
          dr.PCINumber:=ReadIntProperty(sl,'PCINumber');
          dr.DeviceNumber:=ReadIntProperty(sl,'DeviceNumber');
          dr.FunctionNumber:=ReadIntProperty(sl,'FunctionNumber');
          dr.HardwareID:=ReadStrProperty(sl,'HardwareID');
          dr.SymbolicLink:=ReadStrProperty(sl,'SymbolicLink');
          dr.DeviceParam:=ReadStrProperty(sl,'DeviceParam');
          dr.Driver:=ReadStrProperty(sl,'Driver');
          dr.DriverDate:=ReadStrProperty(sl,'DriverDate');
          dr.DriverVersion:=ReadStrProperty(sl,'DriverVersion');
          dr.DriverProvider:=ReadStrProperty(sl,'DriverProvider');
          dr.InfPath:=ReadStrProperty(sl,'InfPath');
          dr.Service:=ReadStrProperty(sl,'Service');
          dr.ServiceName:=ReadStrProperty(sl,'ServiceName');
          dr.ServiceGroup:=ReadStrProperty(sl,'ServiceGroup');
          dr.ServiceType:=ReadIntProperty(sl,'ServiceType');
          dr.RegKey:=ReadStrProperty(sl,'RegKey');
          dr.ResourceListKey:=ReadStrProperty(sl,'ResourceListKey');
          dr.ResourceListValue:=ReadStrProperty(sl,'ResourceListValue');
          dr.VendorID:=ReadIntProperty(sl,'VendorID');
          dr.DeviceID:=ReadIntProperty(sl,'DeviceID');
          dr.SubSysID:=ReadIntProperty(sl,'SubSysID');
          dr.Revision:=ReadIntProperty(sl,'Revision');
          dr.LastArrivalDate:=ReadDtProperty(sl,'LastArrivalDate');
          if dr.LastArrivalDate=0 then
            dr.LastArrivalDate:=ReadDtProperty(sl,'TimeStamp');
          if Trim(dr.ClassName)<>'' then
            Add(dr);
          Result:=True;
        finally
          sl.Free;
        end;
      finally
        strm.Free;
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
      i:=0;
      while ReadFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
      SetDataAvail(True);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.


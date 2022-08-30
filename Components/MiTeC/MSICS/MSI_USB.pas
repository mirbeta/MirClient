{*******************************************************}
{       MiTeC System Information Component Suite        }
{              USB Detection Part                       }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_USB;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MSI_Common, MSI_Defs, MiTeC_USB, MiTeC_CfgMgrSetupAPI;

const
  StorageFolderName = 'USB';

type
  TMiTeC_USB= class(TMiTeC_Component)
  private
    FUSBNodes: TUSBNodes;
    function GetConnectedDevices: Byte;
    function GetUSBNode(Index: integer): TUSBNode;
    function GetUSBNodeCount: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property USBNodeCount: Integer read GetUSBNodeCount;
    property USBNodes[Index: integer]: TUSBNode read GetUSBNode;
    function FindDisk(ADisk: char): TUSBNode;
    function IsEjectable(ANode: TUSBNode): DEVINST;
    function EjectDevice(AInst: DEVINST): Boolean;
    function GetDevicePowerState(ADeviceInstanceID, ADriverKey: string): TDevicePowerState;
  published
    property ConnectedDevices: Byte read GetConnectedDevices stored False;
  end;

implementation

uses MiTeC_Storage, MiTeC_WinIOCTL, MiTeC_StrUtils, MiTeC_Routines;

{ TMiTeC_USB }

procedure TMiTeC_USB.Clear;
begin
  Finalize(FUSBNodes);
end;

destructor TMiTeC_USB.Destroy;
begin
  Finalize(FUSBNodes);
  inherited;
end;

function TMiTeC_USB.FindDisk(ADisk: char): TUSBNode;
var
  i,j: Integer;
begin
  Finalize(Result);
  for i:=0 to High(FUSBNodes) do
    for j:=0 to High(FUSBNodes[i].USBDevice.Registry) do
      if SameText(ADisk,FUSBNodes[i].USBDevice.Registry[j].Drive) then begin
        Result:=FUSBNodes[i];
        Break;
      end;
end;

function TMiTeC_USB.GetConnectedDevices: Byte;
var
  i: Integer;
begin
  Result:=0;
  for i:=0 to High(FUSBNodes) do
    if {(FUSBNodes[i].USBDevice.ConnectionStatus>0) and
       (FUSBNodes[i].USBClass in [usbReserved..usbStorage,usbVendorSpec])}
       (FUSBNodes[i].USBDevice.USBClassname<>'') then
      Inc(Result);
end;

function TMiTeC_USB.GetDevicePowerState(ADeviceInstanceID, ADriverKey: string): TDevicePowerState;
var
  dinfo: TSPDevInfoData;
  intf: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  n,pt: Cardinal;
  i: Integer;
  hdev: HDEVINFO;
  buf: array[0..{$IFDEF UNICODE}512{$ELSE}255{$ENDIF}] of byte;
  s: string;
  pd: TCMPowerData;
begin
  ADeviceInstanceID:=FastStringReplace(ADeviceInstanceID,'\\','\');
  Result:=PowerDeviceUnspecified;
  hdev:=SetupDiGetClassDevs(nil,'USB',0,DIGCF_ALLCLASSES or DIGCF_PRESENT or DIGCF_PROFILE);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then
    try
      i:=0;
      pt:=0;
      dinfo.cbSize:=sizeof(TSPDevInfoData);
      while SetupDiEnumDeviceInfo(hDev,i,dinfo) do begin
        if Assigned(SetupDiGetDeviceProperty) then begin
          ZeroMemory(@buf,sizeof(buf));
          if ADeviceInstanceId<>'' then begin
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_InstanceId,pt,@buf,sizeof(buf),nil,0);
            s:=string(PChar(@buf));
            if SameText(s,ADeviceInstanceId) then begin
              SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_PowerData,pt,@buf,sizeof(buf),nil,0);
              Move(buf[0],pd,sizeof(pd));
              Result:=pd.PD_MostRecentPowerState;
              Break;
            end;
          end else if ADriverKey<>'' then begin
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Driver,pt,@buf,sizeof(buf),nil,0);
            s:=string(PChar(@buf));
            if SameText(s,ADriverKey) then begin
              SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_PowerData,pt,@buf,sizeof(buf),nil,0);
              Move(buf[0],pd,sizeof(pd));
              Result:=pd.PD_MostRecentPowerState;
              Break;
            end;
          end;
        end else begin
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
                  s:=GetString(hdev,dinfo,SPDRP_DRIVER);
                  if SameText(s,ADriverKey) then begin
                    GetBinary(hDev,dinfo,SPDRP_DEVICE_POWER_DATA,@buf,sizeof(buf));
                    Move(buf[0],pd,sizeof(pd));
                    Result:=pd.PD_MostRecentPowerState;
                    Break;
                  end;
                end;
                finally
                  FreeMem(pdidd);
                end;
              end;
           end;
        end;
        inc(i);
      end;
    finally
      SetupDiDestroyDeviceInfoList(hdev);
    end;
end;

procedure TMiTeC_USB.RefreshData;
begin
  inherited;
  EnumUSBDevices(FUSBNodes);
  SetDataAvail(True);
end;

function TMiTeC_USB.EjectDevice(AInst: Cardinal): Boolean;
var
  VetoType: PNP_VETO_TYPE;
  VetoBuffer: array [0..MAX_PATH-1] of CHAR;
  i,Status: Cardinal;
begin
  i:=0;
  repeat
    VetoType:=0;
    FillChar(VetoBuffer[0],SizeOf(VetoBuffer),0);
    Status:=CM_Request_Device_Eject(AInst,@VetoType,@VetoBuffer[0],Length(VetoBuffer),0);
    Result:=(Status=CR_SUCCESS) and (VetoType=0);
    Inc(i);
  until Result or (i>4);
  //Status:=CM_Request_Device_Eject(AInst,nil,nil,0,0); //Windows notification
end;

function TMiTeC_USB.GetUSBNode;
begin
  Finalize(Result);
  FillChar(Result,SizeOf(TUSBNode),0);
  Result.USBClass:=usbUnknown;
  if (Index>=0) and (Index<Length(FUSBNodes)) then
    Result:=FUSBNodes[Index];
end;

function TMiTeC_USB.GetUSBNodeCount: Integer;
begin
  Result:=Length(FUSBNodes);
end;

function GetDriveDevInstByDeviceNumber(DriveType,DeviceNumber: Cardinal): Cardinal;
var
  DevInfo: HDEVINFO;
  dwIndex, dwSize, dwBytesReturned: DWORD;
  pspdidd: PSPDeviceInterfaceDetailData;
	spdid: SP_DEVICE_INTERFACE_DATA;
	spdd: SP_DEVINFO_DATA;
  hDrive: THandle;
  guid: TGUID;
  sdn: STORAGE_DEVICE_NUMBER;
begin
  Result:=0;
  case DriveType of
    FILE_DEVICE_TAPE: guid:=GUID_DEVINTERFACE_TAPE;
    FILE_DEVICE_DISK: guid:=GUID_DEVINTERFACE_DISK;
    FILE_DEVICE_CD_ROM,
    FILE_DEVICE_DVD: guid:=GUID_DEVINTERFACE_CDROM;
    else
      Exit;
  end;
	DevInfo:=SetupDiGetClassDevs(@guid,nil,0,DIGCF_PRESENT or DIGCF_DEVICEINTERFACE);
	if (Cardinal(DevInfo)=INVALID_HANDLE_VALUE) then
    Exit;
	dwIndex:=0;
  ZeroMemory(@spdd, SizeOf(spdd));
	spdid.cbSize:=SizeOf(spdid);
	while True do begin
		if not SetupDiEnumDeviceInterfaces(DevInfo,nil,guid,dwIndex,spdid) then
			break;
		dwSize:=0;
		SetupDiGetDeviceInterfaceDetail(DevInfo,@spdid,nil,0,dwSize,nil);
		if (dwSize<>0) and (dwSize<=1024) then begin
      GetMem(pspdidd,dwSize);
      try
			  pspdidd.cbSize:=SizeOf(pspdidd^);
			  ZeroMemory(@spdd,SizeOf(spdd));
			  spdd.cbSize:=SizeOf(spdd);
			  if SetupDiGetDeviceInterfaceDetail(DevInfo,@spdid,pspdidd,dwSize,dwSize,@spdd) then begin
          hDrive:=INVALID_HANDLE_VALUE;
          try
            hDrive:=CreateFile(pspdidd.DevicePath,0,FILE_SHARE_READ or FILE_SHARE_WRITE,nil,OPEN_EXISTING,0,0);
				    if (hDrive<>INVALID_HANDLE_VALUE) then begin
					    dwBytesReturned:=0;
					    if DeviceIoControl(hDrive,IOCTL_STORAGE_GET_DEVICE_NUMBER,nil,0,@sdn,SizeOf(sdn),dwBytesReturned,nil) then begin
						    if DeviceNumber=sdn.DeviceNumber then begin
							    Result:=spdd.DevInst;
                  Break;
						    end;
				      end;
            end;
          finally
            CloseHandle(hDrive);
          end;
			  end;
      finally
        FreeMem(pspdidd);
      end;
	  end;
    Inc(dwIndex);
  end;
	SetupDiDestroyDeviceInfoList(DevInfo);
end;

function TMiTeC_USB.IsEjectable(ANode: TUSBNode): DEVINST;
var
  i: Integer;
  dn,dt: Cardinal;
  h: THandle;
  inst: DEVINST;
begin
  Result:=0;
  for i:=0 to High(ANode.USBDevice.Registry) do
    if ANode.USBDevice.Registry[i].Drive<>'' then begin
      h:=GetHandle_LogicalDisk(ANode.USBDevice.Registry[i].Drive);
      try
        if GetDeviceNumber(h,dn) then begin
          case GetDriveType(PChar(Format('%s:\',[ANode.USBDevice.Registry[i].Drive]))) of
            DRIVE_REMOVABLE,
            DRIVE_FIXED: dt:=FILE_DEVICE_DISK;
            DRIVE_CDROM: dt:=FILE_DEVICE_CD_ROM;
            else dt:=FILE_DEVICE_UNKNOWN;
          end;
          inst:=GetDriveDevInstByDeviceNumber(dt,dn);
          CM_Get_Parent(Result,Inst,0);
        end;
      finally
        CloseHandle(h);
      end;
    end;
end;

function TMiTeC_USB.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AName: string): boolean;
var
  S: TStructuredStorage;

function ReadFromSubStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
      try strm:=S.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            with FUSBNodes[High(FUSBNodes)].USBDevice do begin
              SetLength(Registry,Length(Registry)+1);
              with Registry[High(Registry)] do begin
                Name:=ReadStrProperty(sl,'Name');
                Timestamp:=ReadDTProperty(sl,'Timestamp');
                USBClass:=ReadStrProperty(sl,'USBClass');
                DeviceClass:=ReadStrProperty(sl,'DeviceClass');
                try DeviceClassGUID:=StringToGUID(ReadStrProperty(sl,'DeviceClassGUID')); except end;
                Drive:=ReadStrProperty(sl,'Drive');
                IntfGUID:=ReadStrProperty(sl,'IntfGUID');
              end;
            end;
          Result:=True;
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;
end;

var
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
begin
  Result:=False;
  try
    S:=Sub.OpenSubStorage(AName,STG_READ_INSTORAGE,False);
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
            SetLength(FUSBNodes,Length(FUSBNodes)+1);
            with FUSBNodes[High(FUSBNodes)] do begin
              ConnectionName:=ReadStrProperty(sl,'ConnectionName');
              Timestamp:=ReadDTProperty(sl,'Timestamp');
              Keyname:=ReadStrProperty(sl,'KeyName');
              USBClass:=TUSBClass(ReadIntProperty(sl,'USBClass'));
              USBDevice.USBClassname:=ReadStrProperty(sl,'USBClassname');
              try ClassGUID:=StringToGUID(ReadStrProperty(sl,'ClassGUID'));except end;
              USBDevice.Manufacturer:=ReadStrProperty(sl,'Manufacturer');
              USBDevice.Product:=ReadStrProperty(sl,'Product');
              USBDevice.Serial:=ReadStrProperty(sl,'Serial');
              ParentIndex:=ReadIntProperty(sl,'ParentIndex');
              Level:=ReadIntProperty(sl,'Level');
              USBDevice.Port:=ReadIntProperty(sl,'Port');
              USBDevice.DeviceAddress:=ReadIntProperty(sl,'DeviceAddress');
              USBDevice.ConnectionStatus:=ReadIntProperty(sl,'ConnectionStatus');
              USBDevice.MaxPower:=ReadIntProperty(sl,'MaxPower');
              USBDevice.MajorVersion:=ReadIntProperty(sl,'MajorVersion');
              USBDevice.MinorVersion:=ReadIntProperty(sl,'MinorVersion');
              USBDevice.ProductID:=ReadIntProperty(sl,'ProductID');
              USBDevice.VendorID:=ReadIntProperty(sl,'VendorID');
            end;
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      i:=0;
      while ReadFromSubStream(i) do
        Inc(i);
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
      i:=0;
      while ReadFromStream(IntToStr(i)) do
        Inc(i);
      Result:=Result or (i>0);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_USB.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

procedure WriteToStream(AName: string; AIndex: Integer);
var
  S: TStructuredStorage;

procedure WriteToSubStream(AIndex: integer; ASubIndex: Integer);
var
  strm: TStorageStream;
  sl: TStringList;
begin
    sl:=TStringList.Create;
    try
      with Self.USBNodes[AIndex].USBDevice.Registry[ASubIndex] do begin
        WriteStrProperty(sl,'Name',Name);
        WriteDTProperty(sl,'Timestamp',Timestamp);
        WriteStrProperty(sl,'USBClass',USBClass);
        WriteStrProperty(sl,'DeviceClassGUID',GUIDToString(DeviceClassGUID));
        WriteStrProperty(sl,'DeviceClass',DeviceClass);
        WriteStrProperty(sl,'IntfGUID',IntfGUID);
        WriteStrProperty(sl,'Drive',Drive);
      end;
      strm:=S.OpenStream(Format(strm_Item,[ASubIndex]),STG_OPEN,True);
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
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
begin
  S:=Sub.OpenSubStorage(AName,STG_OPEN,True);
  try
    sl:=TStringList.Create;
    try
    WriteStrProperty(sl,'ConnectionName',Self.USBNodes[AIndex].ConnectionName);
    WriteDTProperty(sl,'Timestamp',Self.USBNodes[AIndex].TimeStamp);
    WriteStrProperty(sl,'KeyName',Self.USBNodes[AIndex].Keyname);
    WriteStrProperty(sl,'USBClassname',Self.USBNodes[AIndex].USBDevice.USBClassname);
    WriteStrProperty(sl,'USBClassGUID',GUIDToString(Self.USBNodes[AIndex].ClassGUID));
    WriteIntProperty(sl,'USBClass',Integer(Self.USBNodes[AIndex].USBClass));
    WriteStrProperty(sl,'Manufacturer',Self.USBNodes[AIndex].USBDevice.Manufacturer);
    WriteStrProperty(sl,'Product',Self.USBNodes[AIndex].USBDevice.Product);
    WriteStrProperty(sl,'Serial',Self.USBNodes[AIndex].USBDevice.Serial);
    WriteIntProperty(sl,'ParentIndex',Self.USBNodes[AIndex].ParentIndex);
    WriteIntProperty(sl,'Level',Self.USBNodes[AIndex].Level);
    WriteIntProperty(sl,'Port',Self.USBNodes[AIndex].USBDevice.Port);
    WriteIntProperty(sl,'DeviceAddress',Self.USBNodes[AIndex].USBDevice.DeviceAddress);
    WriteIntProperty(sl,'ConnectionStatus',Self.USBNodes[AIndex].USBDevice.ConnectionStatus);
    WriteIntProperty(sl,'MaxPower',Self.USBNodes[AIndex].USBDevice.MaxPower);
    WriteIntProperty(sl,'MajorVersion',Self.USBNodes[AIndex].USBDevice.MajorVersion);
    WriteIntProperty(sl,'MinorVersion',Self.USBNodes[AIndex].USBDevice.MinorVersion);
    WriteIntProperty(sl,'ProductID',Self.USBNodes[AIndex].USBDevice.ProductID);
    WriteIntProperty(sl,'VendorID',Self.USBNodes[AIndex].USBDevice.VendorID);
    strm:=S.OpenStream(strm_Props,STG_OPEN,True);
    try
      SaveToEncodedStream(sl,strm,ACodeStream);
    finally
      strm.Free;
    end;
    finally
      sl.Free;
    end;

    for i:=0 to High(Self.USBNodes[AIndex].USBDevice.Registry) do
      WriteToSubStream(AIndex,i);
  finally
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
      for i:=0 to Self.USBNodeCount-1 do
        WriteToStream(IntToStr(i),i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.


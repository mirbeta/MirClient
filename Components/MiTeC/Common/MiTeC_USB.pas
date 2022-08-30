{*******************************************************}
{                MiTeC Common Routines                  }
{                   USB Interface                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}


{$INCLUDE Compilers.inc}

unit MiTeC_USB;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils,
     {$else}
     Windows, SysUtils,
     {$ENDIF}
     MiTeC_WinIOCTL;

const
  FILE_DEVICE_USB = FILE_DEVICE_UNKNOWN;
  USB_IOCTL_INTERNAL_INDEX = $0000;
  USB_IOCTL_INDEX = $00ff;

  IOCTL_USB_GET_NODE_INFORMATION   = ((FILE_DEVICE_USB shl 16) or
                                               ((USB_IOCTL_INDEX+3) shl 2) or
                                               METHOD_BUFFERED or
                                               (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_GET_NODE_CONNECTION_INFORMATION       = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+4) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION   = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+5) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_GET_NODE_CONNECTION_NAME     = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+6) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_DIAG_IGNORE_HUBS_ON   = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+7) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_DIAG_IGNORE_HUBS_OFF  = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+8) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_GET_NODE_CONNECTION_DRIVERKEY_NAME  = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+9) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_HCD_GET_STATS_1          = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_HCD_GET_STATS_2          = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+11) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_DIAGNOSTIC_MODE_ON   = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+1) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_DIAGNOSTIC_MODE_OFF  = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+2) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_USB_GET_ROOT_HUB_NAME  = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+3) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  IOCTL_GET_HCD_DRIVERKEY_NAME = ((FILE_DEVICE_USB shl 16) or
                                                ((USB_IOCTL_INDEX+10) shl 2) or
                                                METHOD_BUFFERED or
                                                (FILE_ANY_ACCESS shl 14));

  USB_DEVICE_DESCRIPTOR_TYPE                = $01;
  USB_CONFIGURATION_DESCRIPTOR_TYPE         = $02;
  USB_STRING_DESCRIPTOR_TYPE                = $03;
  USB_INTERFACE_DESCRIPTOR_TYPE             = $04;
  USB_ENDPOINT_DESCRIPTOR_TYPE              = $05;

  USB_REQUEST_GET_STATUS                    = $00;
  USB_REQUEST_CLEAR_FEATURE                 = $01;

  USB_REQUEST_SET_FEATURE                   = $03;

  USB_REQUEST_SET_ADDRESS                   = $05;
  USB_REQUEST_GET_DESCRIPTOR                = $06;
  USB_REQUEST_SET_DESCRIPTOR                = $07;
  USB_REQUEST_GET_CONFIGURATION             = $08;
  USB_REQUEST_SET_CONFIGURATION             = $09;
  USB_REQUEST_GET_INTERFACE                 = $0A;
  USB_REQUEST_SET_INTERFACE                 = $0B;
  USB_REQUEST_SYNC_FRAME                    = $0C;



type
  TUSBHubNode = (usbhnHub,usbhnMIParent);

  TUnicodeName = record
    Length: Cardinal;
    UnicodeName: array[0..255] of Byte;
  end;

  TNodeType = record
    ConnectionIndex: Integer;
    Length: Cardinal;
    UnicodeName: array[0..255] of byte
  end;

  TSetupPacket = record
    bmRequest: Byte;
    bRequest: Byte;
    wValue: array[0..1] of Byte;
    wIndex: array[0..1] of Byte;
    wLength: array[0..1] of Byte;
  end;

  TDescriptorRequest = record
    ConnectionIndex: Integer;
    SetupPacket: TSetupPacket;
    ConfigurationDescriptor: array[0..2047] of Byte;
  end;

  TDeviceDescriptor = record
    Length: Byte;
    DescriptorType: Byte;
    USBSpec: array[0..1] of Byte;
    DeviceClass: Byte;
    DeviceSubClass: Byte;
    DeviceProtocol: Byte;
    MaxPacketSize0: Byte;
    VendorID: Word;//array[0..1] of Byte;
    ProductID: Word; //array[0..1] of Byte;
    DeviceRevision: array[0..1] of byte;
    ManufacturerStringIndex: byte;
    ProductStringIndex: Byte;
    SerialNumberStringIndex: Byte;
    ConfigurationCount: Byte;
  end;

  THubDescriptor = record
    Length: Byte;
    HubType: Byte;
    PortCount: Byte;
    Characteristics: array[0..1] of Byte;
    PowerOnToPowerGood: Byte;
    HubControlCurrent: Byte;
    RemoveAndPowerMask: array[0..63] of Byte;
  end;

  TEndPointDescriptor = record
    Length: Byte;
    DescriptorType: Byte;
    EndpointAddress: Byte;
    Attributes: Byte;
    MaxPacketSize: WORD;
    PollingInterval: Byte;
  end;

  TNodeInformation = record
    NodeType: Cardinal;
    NodeDescriptor: THubDescriptor;
    HubIsBusPowered: Byte;
  end;

  TUSBPipeInfo = record
    EndPointDescriptor: TEndpointDescriptor;
    ScheduleOffset: Cardinal;
  end;

  TNodeConnectionInformation = record
    ConnectionIndex: Integer;
    ThisDevice: TDeviceDescriptor;
    CurrentConfiguration: Byte;
    LowSpeed: Byte;
    DeviceIsHub: Byte;
    DeviceAddress: array[0..1] of Byte;
    NumberOfOpenEndPoints: array[0..3] of Byte;
    ThisConnectionStatus: array[0..3] of Byte;
    PipeList: array[0..31] of TUSBPipeInfo;
  end;

  TCollectedDeviceData = record
    DeviceType: Cardinal;
    DeviceHandle: Cardinal;
    ConnectionData: TNodeConnectionInformation;
    NodeData: TNodeInformation;
  end;

  TConfigurationDescriptor = record
    bLength: byte;
    bDescriptorType: byte;
    wTotalLength: word;
    bNumInterfaces: byte;
    bConfigurationValue: byte;
    iConfiguration: byte;
    bmAttributes: byte;
    MaxPower: byte;
  end;

  TUSBClass = 	(usbReserved, usbAudio, usbCommunications, usbHID, usbMonitor,
                 usbPhysicalIntf, usbPower, usbPrinter, usbStorage, usbHub, usbVendorSpec,
                 // illegal values
                 usbExternalHub, usbHostController, usbUnknown, usbError);

  TRegistryRecord = record
    Name,
    USBClass,
    DeviceClass: string;
    DeviceClassGUID: TGUID;
    IntfGUID: string;
    Drive: string;
    DriveConnected: boolean;
    Timestamp: TDateTime;
    _PIP: string;
    _Key: string;
  end;

  TRegistrySet = array of TRegistryRecord;

  TUSBDevice = record
    Port: Cardinal;
    DeviceAddress: Cardinal;
    Manufacturer,
    Product,
    Serial: string;
    ConnectionStatus: Byte;
    MaxPower: WORD;
    MajorVersion,
    MinorVersion: Byte;
    ProductID,
    VendorID: Word;
    USBClassname: string;
    Registry: TRegistrySet;
  end;

  TUSBNode = record
    ConnectionName: string;
    DeviceInstanceId: string;
    Keyname: string;
    USBClass: TUSBClass;
    ClassGUID: TGUID;
    ParentIndex: Integer;
    Level: Integer;
    TimeStamp: TDateTime;
    USBDevice: TUSBDevice;
  end;

  TUSBNodes = array of TUSBNode;

const
  ClassNames: array[0..14] of string = (
        'Reserved',
        'Audio',
        'Communications',
        'Human interface',
        'Monitor',
        'Physical interface',
        'Power',
        'Printer',
        'Storage',
        'Root Hub',
        'Vendor Specific',
        'External Hub',
        'Host Controller',
        '<error>',
        '<unknown>'
        );

  ConnectionStates: array[0..5] of string = (
        'No device connected',
        'Device connected',
        'Device FAILED enumeration',
        'Device general FAILURE',
        'Device caused overcurrent',
        'Not enough power for device');

function GetDeviceName1(hDevice: THandle; IOCTL_Code: Cardinal): string;
function GetDeviceName2(hDevice: THandle; PortIndex: Integer; IOCTL_Code: Cardinal): string;
function GetNodeInformation(hDevice: THandle; var Information: TNodeInformation): Integer;
function GetNodeConnection(hDevice: THandle; PortIndex: Integer; var Connection: TNodeConnectionInformation): Integer;
function GetStringDescriptor(HubHandle: THandle; PortIndex: Integer; var LanguageID: Word; Index: Byte; var Str: string): integer;
function GetConfigurationDescriptor(HubHandle: THandle; PortIndex: Integer; var MaxPower: WORD; var ClassID: integer): Integer;
function GetPortData(hRootHub: THandle; SA: TSecurityAttributes; PortCount: Byte; ParentIndex: integer; Level: Integer; var USBNodes: TUSBNodes): Integer;
procedure EnumUSBDevices(var USBNodes: TUSBNodes);

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry, System.Classes,
     {$ELSE}
     Registry, Classes,
     {$ENDIF}
     MiTeC_Routines, MiTeC_Datetime, MiTeC_Storage, MiTeC_StrUtils, MiTeC_RegUtils, MiTeC_CfgMgrSetupApi;

function ParseDeviceName(S: string): string;
var
  i: Cardinal;
  v,p,r: string;
begin
  Result:='';
  v:='';
  p:='';
  r:='';
  i:=Pos('&VEN_',Uppercase(S));
  if i=0 then
    Exit;
  Delete(S,1,i+4);
  i:=Pos('&PROD_',Uppercase(S));
  if i>0 then begin
    v:=Copy(S,1,i-1);
    Delete(S,1,i+5);
    i:=Pos('&REV_',Uppercase(S));
    if i>0 then begin
      p:=Copy(S,1,i-1);
      Delete(S,1,i+4);
      i:=Pos('\',S);
      if i>0 then
        Delete(S,i,Length(S));
      r:=S;
    end;
  end;
  Result:=Trim(StringReplace(Format('%s %s %s',[v,p,r]),'_',' ',[rfReplaceAll,rfIgnoreCase]));
end;

function GetUSBDeviceDesc(ADriverKey: string; var HardwareID: string; var Timestamp: TDateTime; var DeviceClassGUID: TGUID; var ASet: TRegistrySet): string;
const
  rkNT = 'SYSTEM\CurrentControlSet\Enum';
    rvPIP = 'ParentIdPrefix';
    rvSrv = 'Service';
    rvD = 'Driver';
    rvFN = 'FriendlyName';
    rvC = 'Class';
    rvDD = 'DeviceDesc';
    rvCG = 'ClassGUID';
    rvHID = 'HardwareID';
    rvSN = 'SymbolicName';
    rkControl = 'Control';
    rvCID = 'ContainerID';
var
  sl,kl,dl,slt: TStringList;
  i,j,k: Integer;
  s,sn,pip,rk,srv,dn,hid,cid,usbk: string;
  b: Boolean;
  rki: TRegKeyInfo;
  ts: TDateTime;
begin
  ts:=0;
  Result:='';
  Finalize(ASet);
  s:=Uppercase(ADriverKey);
  with OpenRegistryReadOnly do begin
    sl:=TStringList.Create;
    kl:=TStringList.Create;
    dl:=TStringList.Create;
    slt:=TStringList.Create;
    pip:='';
    srv:='';
    sn:='';
    usbk:='';
    try
      GetSymbolicLinkTable(slt);
      Rootkey:=HKEY_LOCAL_MACHINE;

      rk:=rkNT;
      if OpenKey(rk+'\USB',False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if OpenKey(Format('%s\USB\%s',[rk,sl[i]]),False) then begin
            usbk:=sl[i];
            GetKeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do begin
              pip:='';
              srv:='';
              sn:='';
              cid:='';
              if OpenKey(Format('%s\USB\%s\%s',[rk,sl[i],kl[j]]),False) then begin
                if ValueExists(rvD) then begin
                  sn:=Uppercase(ReadString(rvD));
                  if SameText(sn,s) then begin
                    if ValueExists(rvPIP) then
                      pip:=ReadString(rvPIP)
                    else
                      pip:=kl[j];

                    if ValueExists(rvCID) then
                      cid:=ReadString(rvCID);

                    if ValueExists(rvSrv) then
                      srv:=ReadString(rvSrv);

                    if ValueExists(rvDD) then begin
                      Result:=Trim(ReadString(rvDD));
                      if Pos(';',Result)>0 then
                        Result:=Copy(Result,Pos(';',Result)+1,1024);
                    end;

                    if ValueExists(rvCG) then
                      DeviceClassGuid:=StringToGUID(ReadString(rvCG));

                    {if ValueExists(rvHID) then begin
                      ZeroMemory(@buf,SizeOf(buf));
                      ReadBinaryData(rvHID,buf,GetDataSize(rvHID));
                      hid:=PAnsiChar(@buf);
                    end;}
                  end;
                end;
                CloseKey;
                if OpenKey(Format('%s\USB\%s\%s\Device Parameters',[rk,sl[i],kl[j]]),False) then begin
                  GetKeyInfo(rki);
                  try
                    ts:=UTCToLocalDateTime({$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime));
                  except
                    //MessageBox(0,PChar(Format('%s\USB\%s\%s\Device Parameters',[rk,sl[i],kl[j]])+#13#10+IntToStr(rki.FileTime.dwLowDateTime)+#13#10+IntToStr(rki.FileTime.dwHighDateTime)),nil,MB_OK)
                  end;
                  if ValueExists(rvSN) then
                    hid:=Trim(ReadString(rvSN));
                  CloseKey;
                end;
                if OpenKey(Format('%s\USB\%s\%s\Control',[rk,sl[i],kl[j]]),False) then begin
                  GetKeyInfo(rki);
                  try
                    ts:=UTCToLocalDateTime({$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime));
                  except
                    //MessageBox(0,PChar(Format('%s\USB\%s\%s\Control',[rk,sl[i],kl[j]])+#13#10+IntToStr(rki.FileTime.dwLowDateTime)+#13#10+IntToStr(rki.FileTime.dwHighDateTime)),nil,MB_OK)
                  end;
                  CloseKey;
                end;
                if pip<>'' then
                  Break;
              end;
            end;
          end;
          if pip<>'' then
            Break;
        end;
      end;
      TimeStamp:=ts;

      if pip='' then
        Exit;
      if (srv='') or SameText(srv,'USB') then
        srv:='USBSTOR';
      if Pos('HID',uppercase(srv))>0 then
        srv:='HID';
      if Pos('UASPSTOR',uppercase(srv))>0 then
        srv:='SCSI';
      if Pos('BTH',uppercase(srv))>0 then
        srv:='BTH';
      if SameText(srv,'USBCCGP') then
        srv:='USB';

      CreateMountedDevicesTable(dl);

      rk:=Format('%s\%s',[rk,srv]);
      if OpenKey(rk,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if not SameText(sl[i],usbk) and OpenKey(Format('%s\%s',[rk,sl[i]]),False) then begin
            GetkeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do
              if OpenKey(Format('%s\%s\%s',[rk,sl[i],kl[j]]),False) then begin
                s:='';
                if ValueExists(rvCID) then
                  s:=ReadString(rvCID);
                b:=(Pos(uppercase(pip),uppercase(kl[j]))=1) or (SameText(srv,'SCSI') and SameText(s,cid));
                if b then begin
                  SetLength(ASet,Length(Aset)+1);
                  with ASet[High(ASet)] do begin
                    _Key:=CurrentPath;
                    USBClass:=Result;
                    if ValueExists(rvPIP) then
                      _pip:=ReadString(rvPIP)
                    else
                      _pip:=kl[j];
                    dn:=ParseDeviceName(sl[i]);
                    if dn<>'' then
                      Name:=dn;
                    if Name='' then
                      if ValueExists(rvFN) then
                        Name:=Trim(ReadString(rvFN));
                    if ValueExists(rvCG) then
                      DeviceClassGuid:=StringToGUID(ReadString(rvCG));
                    if ValueExists(rvDD) then begin
                      DeviceClass:=Trim(ReadString(rvDD));
                      if Pos(';',DeviceClass)>0 then
                        DeviceClass:=Copy(DeviceClass,Pos(';',DeviceClass)+1,1024);
                    end;
                    if Name='' then
                      Name:=DeviceClass;
                    HardwareID:=hid;
                    if IsEqualGUID(DeviceClassGuid,GUID_DEVCLASS_DISKDRIVE){ (_pip<>'')} then begin
                      for k:=0 to dl.Count-1 do
                        if Pos(_pip,dl[k])>0 then begin
                          Drive:=dl.Names[k];
                          DriveConnected:=GetMediaPresent(Drive+':');
                          Break;
                        end;
                      if Drive='' then begin
                        s:=Uppercase(FastStringReplace(Name,' ','_'));
                        for k:=0 to slt.Count-1 do begin
                          if (not SameText(srv,'SCSI') or (Pos(s,UpperCase(slt.ValueFromIndex[k]))>0)) and (Pos(_pip,UpperCase(slt.ValueFromIndex[k]))>0) then begin
                            Drive:=GetDiskLetterFromDeviceNumber(StrToInt(slt.Names[k]));
                            DriveConnected:=Drive<>'';
                            Break;
                          end;
                        end;
                      end;
                    end;
                  end;
                  GetKeyInfo(rki);
                  try
                    ASet[High(ASet)].TimeStamp:=UTCToLocalDateTime({$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime));
                  except
                    //MessageBox(0,PChar(Format('%s\%s\%s',[rk,sl[i],kl[j]])+#13#10+IntToStr(rki.FileTime.dwLowDateTime)+#13#10+IntToStr(rki.FileTime.dwHighDateTime)),nil,MB_OK)
                  end;
                  CloseKey;
                  if OpenKey(Format('%s\%s\%s\Control',[rk,sl[i],kl[j]]),False) then begin
                    GetKeyInfo(rki);
                    try
                      ASet[High(ASet)].TimeStamp:=UTCToLocalDateTime({$IFNDEF FPC}FileTimeTodateTime{$ENDIF}(rki.FileTime));
                    except
                      //MessageBox(0,PChar(Format('%s\%s\%s\Control',[rk,sl[i],kl[j]])+#13#10+IntToStr(rki.FileTime.dwLowDateTime)+#13#10+IntToStr(rki.FileTime.dwHighDateTime)),nil,MB_OK)
                    end;
                  end;
                end;
                CloseKey;
              end;
          end;
        end;
      end;
    finally
      sl.Free;
      kl.Free;
      dl.Free;
      slt.Free;
      Free;
    end;
  end;
end;

function GetHubDesc(Akey: string): string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('\SYSTEM\CurrentControlSet\Enum\'+AKey,False) then begin
        if ValueExists('DeviceDesc') then begin
          Result:=Trim(ReadString('DeviceDesc'));
          if Pos(';',Result)>0 then
            Result:=Copy(Result,Pos(';',Result)+1);
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetDeviceName1(hDevice: THandle; IOCTL_Code: Cardinal): string;
var
  i: Integer;
  NameBuffer: TUnicodeName;
  BytesReturned: Cardinal;
begin
  Result:='';
  if DeviceIoControl(hDevice,IOCTL_Code,@NameBuffer,SizeOf(NameBuffer),@NameBuffer,SizeOf(NameBuffer),BytesReturned,nil) then begin
//    WideCharToMultiByte(CP_ACP,0,&ConnectedHub.Name[0], (ConnectedHub.ActualLength)/2, &ConnectedHubName[4], 252, NULL, NULL);
    i:=0;
    while NameBuffer.UnicodeName[i]<>0 do begin
      Result:=Result+Chr(NameBuffer.UnicodeName[i]);
      Inc(i,2);
    end;
  end else
    Result:=Format('Error: %s (%d)',[SysErrorMessage(GetLastError),GetLastError]);
end;

function GetDeviceName2(hDevice: THandle; PortIndex: Integer; IOCTL_Code: Cardinal): string;
var
  i: Integer;
  NameBuffer: TNodeType;
  BytesReturned: Cardinal;
begin
  Result:='';
  NameBuffer.ConnectionIndex:=PortIndex;
  if DeviceIoControl(hDevice,IOCTL_Code,@NameBuffer,SizeOf(NameBuffer),@NameBuffer,SizeOf(NameBuffer),BytesReturned,nil) then begin
//    WideCharToMultiByte(CP_ACP,0,&ConnectedHub.Name[0], (ConnectedHub.ActualLength)/2, &ConnectedHubName[4], 252, NULL, NULL);
    i:=0;
    while NameBuffer.UnicodeName[i]<>0 do begin
      Result:=Result+Chr(NameBuffer.UnicodeName[i]);
      Inc(i,2);
    end;
  end else
    Result:=Format('Error: %s (%d)',[SysErrorMessage(GetLastError),GetLastError]);
end;

function GetNodeInformation;
var
  BytesReturned: Cardinal;
begin
  if DeviceIoControl(hDevice,IOCTL_USB_GET_NODE_INFORMATION,nil,0,@Information,SizeOf(Information),BytesReturned,nil) and (BytesReturned<=256) then
    Result:=0
  else
    Result:=GetLastError;
end;

function GetNodeConnection;
var
  BytesReturned: Cardinal;
begin
  FillChar(Connection,SizeOf(Connection),0);
  Connection.ConnectionIndex:=PortIndex;
  if not DeviceIoControl(hDevice,IOCTL_USB_GET_NODE_CONNECTION_INFORMATION,@Connection,SizeOf(Connection),@Connection,SizeOf(Connection),BytesReturned,nil) and (BytesReturned<=256) then
    Result:=GetLastError
  else
    Result:=0;
end;

function GetStringDescriptor;
var
  Packet: TDescriptorRequest;
  BytesReturned: Cardinal;
  Success: boolean;
  i: Integer;
begin
  Str:='';
  Result:=0;
  for i:=0 to SizeOf(Packet.ConfigurationDescriptor)-1 do
    Packet.ConfigurationDescriptor[i]:=0;
  if (LanguageID=0) then begin
    Packet.ConnectionIndex:=PortIndex;
    Packet.SetupPacket.bmRequest:=$80;
    Packet.SetupPacket.bRequest:=USB_REQUEST_GET_DESCRIPTOR;
    Packet.SetupPacket.wValue[0]:=0;
    Packet.SetupPacket.wValue[1]:=USB_STRING_DESCRIPTOR_TYPE;
    Packet.SetupPacket.wIndex[0]:=0;
    Packet.SetupPacket.wIndex[1]:=0;
    Packet.SetupPacket.wLength[0]:=4;
    Packet.SetupPacket.wLength[1]:=0;
    Success:=DeviceIoControl(HubHandle,IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION,@Packet,
             sizeof(Packet),@Packet,sizeof(Packet),BytesReturned,nil);
    if not Success then
      Result:=GetLastError
    else
      LanguageID:=Packet.ConfigurationDescriptor[2]+(Packet.ConfigurationDescriptor[3] shl 8);
  end;
  for i:=0 to SizeOf(Packet.ConfigurationDescriptor)-1 do
    Packet.ConfigurationDescriptor[i]:=0;
  Packet.ConnectionIndex:=PortIndex;
  Packet.SetupPacket.bmRequest:=$80;
  Packet.SetupPacket.bRequest:=USB_REQUEST_GET_DESCRIPTOR;
  Packet.SetupPacket.wValue[0]:=Index;
  Packet.SetupPacket.wValue[1]:=USB_STRING_DESCRIPTOR_TYPE;
  Packet.SetupPacket.wIndex[0]:=LanguageID and $FF;
  Packet.SetupPacket.wIndex[1]:=(LanguageID shr 8) and $FF;
  Packet.SetupPacket.wLength[0]:=255;
  Packet.SetupPacket.wLength[1]:=0;
  Success:=DeviceIoControl(HubHandle,IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION,@Packet,
           sizeof(Packet),@Packet,sizeof(Packet),BytesReturned,nil);
  if not Success then
    Result:=GetLastError
  else begin
    for i:=2 to Length(Packet.ConfigurationDescriptor)-1 do
      if Packet.ConfigurationDescriptor[i]<32 then
        Packet.ConfigurationDescriptor[i]:=0;
    Str:=WideCharToString(PWideChar(@Packet.ConfigurationDescriptor[2]));
  end;
end;

function GetConfigurationDescriptor;
var
  Packet: TDescriptorRequest;
  BytesReturned: Cardinal;
  Success: boolean;
  LowByte: Byte;
  BufferPtr: Byte;
  cd: TConfigurationDescriptor;
begin
  Result:=0;
  ClassID:=-1;
  MaxPower:=0;
  with Packet do begin
    ConnectionIndex:=PortIndex;
    SetupPacket.bmRequest:=$80;
    SetupPacket.bRequest:=USB_REQUEST_GET_DESCRIPTOR;
    SetupPacket.wValue[0]:=0;
    SetupPacket.wValue[1]:=USB_CONFIGURATION_DESCRIPTOR_TYPE;
    SetupPacket.wIndex[0]:=0;
    SetupPacket.wIndex[1]:=0;
    SetupPacket.wLength[0]:=0;
    SetupPacket.wLength[1]:=1;
  end;
  Success:=DeviceIoControl(HubHandle,IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION,
                          @Packet,SizeOf(TDescriptorRequest),@Packet,SizeOf(TDescriptorRequest),BytesReturned,nil);
  if not Success then begin
    Result:=GetLastError;
    Exit;
  end;
  {with Packet do begin
    ConnectionIndex:=PortIndex;
    SetupPacket.bmRequest:=$80;
    SetupPacket.bRequest:=USB_REQUEST_GET_DESCRIPTOR;
    SetupPacket.wValue[0]:=0;
    SetupPacket.wValue[1]:=USB_CONFIGURATION_DESCRIPTOR_TYPE;
    SetupPacket.wIndex[0]:=0;
    SetupPacket.wIndex[1]:=0;
    SetupPacket.wLength[0]:=Packet.ConfigurationDescriptor[2];
    SetupPacket.wLength[1]:=Packet.ConfigurationDescriptor[3];
  end;
  Success:=DeviceIoControl(HubHandle,IOCTL_USB_GET_DESCRIPTOR_FROM_NODE_CONNECTION,
                          @Packet,SizeOf(TDescriptorRequest),@Packet,SizeOf(TDescriptorRequest),BytesReturned,nil);
  if not Success then begin
    Result:=GetLastError;
    Exit;
  end;}

  Move(Packet.ConfigurationDescriptor,cd,sizeof(cd));
  MaxPower:=cd.MaxPower shl 1;
  BufferPtr:=9;
  while (Packet.ConfigurationDescriptor[BufferPtr]<>0) do begin
    if Packet.ConfigurationDescriptor[BufferPtr+1]=4 then begin
      LowByte:=Packet.ConfigurationDescriptor[BufferPtr+5];
      if ((LowByte>9) and (LowByte<255)) then
        LowByte:=11;
      if (LowByte=255) then
        LowByte:=10;
      ClassID:=LowByte;
      Break;
    end;
    Inc(BufferPtr,9);
  end;
end;

function CorrectSerial(const Serial: ansistring): AnsiString;
var
  i,b: integer;
  s,r: string;
begin
  r:='';
  s:=Trim(string(Serial));
  i:=1;
  while i<Length(s) do begin
    if TryStrtoInt('$'+Copy(s,i,2),b) and (b in [32..127]) then begin
      r:=r+Chr(b);
      inc(i,2);
    end else begin
      r:=s;
      break;
    end;
  end;

  for i:=1 to Length(r) do
    if (r[i]<#32) or (r[i]>#127) then begin
      r:='';
      Exit;
    end;

  Result:={$IFDEF UNICODE}WideToAnsi{$ENDIF}(Trim(r));
end;

function GetPortData(hRootHub: THandle; SA: TSecurityAttributes; PortCount: Byte; ParentIndex: integer; Level: Integer; var USBNodes: TUSBNodes): Integer;
var
  i,j: Integer;
  NodeConnection: TNodeConnectionInformation;
  hERH: THandle;
  Node: TNodeInformation;
  RootHubName: string;
  LanguageID: Word;
  usb: TUSBNode;
const
  AMachine = '.';
begin
  LanguageID:=0;
  for i:=1 to PortCount do begin
    if GetNodeConnection(hRootHub,i,NodeConnection)=ERROR_SUCCESS then begin
      //IOCTL_USB_GET_PORT_CONNECTOR_PROPERTIES
      if (NodeConnection.ThisConnectionStatus[0]=1) then begin
        if Boolean(NodeConnection.DeviceIsHub) then begin
          RootHubName:=GetDeviceName2(hRootHub,i,IOCTL_USB_GET_NODE_CONNECTION_NAME);
          hERH:=CreateFile(PChar(Format('\\%s\%s',[AMachine,RootHubName])),
                      GENERIC_WRITE,
                      FILE_SHARE_WRITE,
                      @SA,
                      OPEN_EXISTING,
                      0,
                      0);
          if (hERH<>INVALID_HANDLE_VALUE) then begin
            if GetNodeInformation(hERH,Node)=ERROR_SUCCESS then begin
              Finalize(usb);
              FillChar(usb,SizeOf(usb),0);
              usb.ConnectionName:=RootHubName;
              usb.KeyName:='';
              usb.USBClass:=usbExternalHub;
              usb.ParentIndex:=ParentIndex;
              usb.Level:=Level;
              usb.USBDevice.Port:=i;
              usb.USBDevice.DeviceAddress:=NodeConnection.DeviceAddress[0];
              usb.USBDevice.ConnectionStatus:=NodeConnection.ThisConnectionStatus[0];
              usb.USBDevice.USBClassname:=GetHubDesc(ExtractFilePath(FastStringReplace(usb.ConnectionName,'#','\')));
              GetConfigurationDescriptor(hRootHub,i,usb.USBDevice.MaxPower,j);
              if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.ProductStringIndex,usb.USBDevice.Product)=0 then
                if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.ManufacturerStringIndex,usb.USBDevice.Manufacturer)=0 then begin
                  if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.SerialNumberStringIndex,usb.USBDevice.Serial)=0 then
                    usb.USBDevice.Serial:=string(CorrectSerial({$IFDEF UNICODE}WideToAnsi{$ENDIF}(usb.USBDevice.Serial)));
              end;
              SetLength(USBNodes,Length(USBNodes)+1);
              USBNodes[High(USBNodes)]:=usb;
              GetPortData(hERH,SA,Node.NodeDescriptor.PortCount,High(USBNodes),Level+1,USBNodes);
            end;
            CloseHandle(hERH);
          end;
          usb.USBDevice.USBClassname:=GetUSBDeviceDesc(usb.KeyName,usb.ConnectionName,usb.TimeStamp,usb.ClassGUID,usb.USBDevice.Registry);
        end else begin
          j:=Integer(usbError);
          Finalize(usb);
          FillChar(usb,SizeOf(usb),0);
          usb.ConnectionName:='';
          usb.KeyName:=GetDeviceName2(hRootHub,i,IOCTL_USB_GET_NODE_CONNECTION_DRIVERKEY_NAME);
          usb.ParentIndex:=ParentIndex;
          usb.Level:=Level;
          usb.USBDevice.Port:=i;
          usb.USBDevice.DeviceAddress:=NodeConnection.DeviceAddress[0];
          usb.USBDevice.ConnectionStatus:=NodeConnection.ThisConnectionStatus[0];
          usb.USBDevice.MajorVersion:=NodeConnection.ThisDevice.USBSpec[1];
          usb.USBDevice.MinorVersion:=NodeConnection.ThisDevice.USBSpec[0];
          usb.USBDevice.ProductID:=NodeConnection.ThisDevice.ProductID;
          usb.USBDevice.VendorID:=NodeConnection.ThisDevice.VendorID;
          GetConfigurationDescriptor(hRootHub,i,usb.USBDevice.MaxPower,j);
          if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.ProductStringIndex,usb.USBDevice.Product)=0 then
            if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.ManufacturerStringIndex,usb.USBDevice.Manufacturer)=0 then begin
              if GetStringDescriptor(hRootHub,i,LanguageID,NodeConnection.ThisDevice.SerialNumberStringIndex,usb.USBDevice.Serial)=0 then
                usb.USBDevice.Serial:=string(CorrectSerial({$IFDEF UNICODE}WideToAnsi{$ENDIF}(usb.USBDevice.Serial)));
            end;
          if j in [Integer(usbReserved)..Integer(usbVendorSpec)] then
            usb.USBClass:=TUSBClass(j)
          else
            usb.USBClass:=usbError;
          usb.USBDevice.USBClassname:=GetUSBDeviceDesc(usb.Keyname,usb.ConnectionName,usb.TimeStamp,usb.ClassGUID,usb.USBDevice.Registry);
          if Length(usb.USBDevice.Registry)=0 then
            GetUSBDeviceDesc(Copy(usb.Keyname,1,Length(usb.Keyname)-1),usb.ConnectionName,usb.TimeStamp,usb.ClassGUID,usb.USBDevice.Registry);
          if usb.ConnectionName<>'' then begin
            usb.DeviceInstanceId:=StringReplace(usb.ConnectionName,'\??\','',[rfReplaceAll,rfIgnoreCase]);
            usb.DeviceInstanceId:=StringReplace(usb.DeviceInstanceId,'#','\\',[rfReplaceAll,rfIgnoreCase]);
            usb.DeviceInstanceId:=Copy(usb.DeviceInstanceId,1,Pos('{',usb.DeviceInstanceId)-3);
          end;
          SetLength(USBNodes,Length(USBNodes)+1);
          USBNodes[High(USBNodes)]:=usb;
        end;
      end else begin
        Finalize(usb);
        FillChar(usb,SizeOf(usb),0);
        usb.ConnectionName:='';
        usb.Keyname:='';
        usb.ParentIndex:=ParentIndex;
        usb.Level:=Level;
        usb.USBDevice.Port:=i;
        usb.USBDevice.DeviceAddress:=NodeConnection.DeviceAddress[0];
        usb.USBDevice.ConnectionStatus:=NodeConnection.ThisConnectionStatus[0];
        SetLength(USBNodes,Length(USBNodes)+1);
        USBNodes[High(USBNodes)]:=usb;
      end;
    end;
  end;
  Result:=Level-1;
end;

procedure EnumUSBDevices;
var
  i: Integer;
  hHCD, hRH: THandle;
  Node: TNodeInformation;
  RootHubName, HCDName: string;
  SA: TSecurityAttributes;
  usb: TUSBNode;
const
  AMachine = '.';
begin
  Finalize(USBNodes);
  SA.nLength:=sizeof(SECURITY_ATTRIBUTES);
  SA.lpSecurityDescriptor:=nil;
  SA.bInheritHandle:=false;
  for i:=0 to 9 do begin
    HCDName:=Format('HCD%d',[i]);
    hHCD:=CreateFile(PChar(Format('\\%s\%s',[AMachine,HCDName])),
                      GENERIC_WRITE,
                      FILE_SHARE_WRITE,
                      @SA,
                      OPEN_EXISTING,
                      0,
                      0);
    if (hHCD<>INVALID_HANDLE_VALUE) then begin
      Finalize(usb);
      FillChar(usb,SizeOf(usb),0);
      usb.ConnectionName:=HCDName;
      usb.Keyname:=GetDeviceName1(hHCD,IOCTL_GET_HCD_DRIVERKEY_NAME);
      usb.USBClass:=usbHostController;
      usb.ParentIndex:=-1;
      usb.Level:=0;
      usb.USBDevice.Port:=i;
      usb.USBDevice.ConnectionStatus:=1;
      SetLength(USBNodes,Length(USBNodes)+1);
      USBNodes[High(USBNodes)]:=usb;
      RootHubName:=GetDeviceName1(hHCD,IOCTL_USB_GET_NODE_INFORMATION);
      hRH:=CreateFile(PChar(Format('\\%s\%s',[AMachine,RootHubName])),
                      GENERIC_WRITE,
                      FILE_SHARE_WRITE,
                      @SA,
                      OPEN_EXISTING,
                      0,
                      0);
      if (hRH<>INVALID_HANDLE_VALUE) then begin
        if GetNodeInformation(hRH,Node)=ERROR_SUCCESS then begin
          Finalize(usb);
          FillChar(usb,SizeOf(usb),0);
          usb.ConnectionName:=RootHubName;
          usb.Keyname:='';
          usb.USBClass:=usbHub;
          usb.ParentIndex:=High(USBNodes);
          usb.Level:=1;
          usb.USBDevice.ConnectionStatus:=1;
          usb.USBDevice.Port:=i;
          usb.USBDevice.USBClassname:=GetHubDesc(ExtractFilePath(FastStringReplace(usb.ConnectionName,'#','\')));
          SetLength(USBNodes,Length(USBNodes)+1);
          USBNodes[High(USBNodes)]:=usb;
          GetPortData(hRH,SA,Node.NodeDescriptor.PortCount,High(USBNodes),2,USBNodes);
        end;
        CloseHandle(hRH);
      end;
      CloseHandle(hHCD);
    end;
  end;
end;

end.

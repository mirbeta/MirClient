{*******************************************************}
{                                                       }
{          System Information Component                 }
{         Device Change Notify Component                }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_DeviceMonitor;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Forms,{$IFDEF TRIAL}VCL.Dialogs,{$ENDIF}
     {$ELSE}
     Windows, SysUtils, Classes, Messages, Forms,{$IFDEF TRIAL}Dialogs,{$ENDIF}
     {$ENDIF}
     {$IFDEF FPC}MiTeC_FPC,{$ENDIF}
     MiTeC_BTAPI, MSI_Defs;

{$IFDEF FPC}
const
  DEVICE_NOTIFY_WINDOW_HANDLE = 0;
{$ENDIF}

type
  TDeviceDesc = record
    SymbolicName,
    ClassName,
    Name,
    Location,
    Driver,
    Description,
    GUID: string;
  end;

  TDeviceChangeEvent = procedure(Sender: TObject; DeviceDesc: TDeviceDesc) of object;
  TVolumeChangeEvent = procedure(Sender: TObject; Drives: string; Remote: boolean) of object;
  TDeviceChangeMessage = procedure(Sender: TObject; Msg: TMessage) of object;

  PDevBroadcastHdr  = ^DEV_BROADCAST_HDR;
  DEV_BROADCAST_HDR = packed record
    dbch_size: cardinal;
    dbch_devicetype: cardinal;
    dbch_reserved: cardinal;
  end;

  PDevBroadcastDeviceInterface  = ^DEV_BROADCAST_DEVICEINTERFACE;
  DEV_BROADCAST_DEVICEINTERFACE = record
    dbcc_size: cardinal;
    dbcc_devicetype: cardinal;
    dbcc_reserved: cardinal;
    dbcc_classguid: TGUID;
    dbcc_name: short;
  end;

  PDevBroadcastHandle = ^DEV_BROADCAST_HANDLE;
  DEV_BROADCAST_HANDLE = record
    dbch_size: Cardinal;
    dbch_devicetype: Cardinal;
    dbch_reserved: Cardinal;
    dbch_handle: THandle;
    dbch_hdevnotify: THandle;
    dbch_eventguid: TGUID;
    dbch_nameoffset: cardinal;
    dbch_data: Byte;
  end;


  PDevBroadcastVolume = ^DEV_BROADCAST_VOLUME;
  DEV_BROADCAST_VOLUME = record
    dbcv_size: cardinal;
    dbcv_devicetype: cardinal;
    dbcv_reserved: cardinal;
    dbcv_unitmask: cardinal;
    dbcv_flags: WORD;
  end;

  PDevBroadcastNet = ^DEV_BROADCAST_NET;
  DEV_BROADCAST_NET = record
    dbcn_size: cardinal;
    dbcn_devicetype: cardinal;
    dbcn_reserved: cardinal;
    dbcn_resource: cardinal;
    dbcn_flags: cardinal;
  end;

const
  GUID_DEVINTERFACE_USB_DEVICE: TGUID =      '{A5DCBF10-6530-11D2-901F-00C04FB951ED}';
  GUID_BTHPORT_DEVICE_INTERFACE: TGUID =     '{0850302A-B344-4FDA-9BE9-90576B8D46F0}';
  GUID_DEVINTERFACE_WPD_EVENT: TGUID =      '{6AC27878-A6FA-4155-BA85-F98F491D4F33}';
  GUID_BLUETOOTH_RADIO_IN_RANGE: TGUID =     '{EA3B5B82-26EE-450E-B0D8-D26FE30A3869}';
  GUID_BLUETOOTH_RADIO_OUT_OF_RANGE: TGUID = '{E28867C9-C2AA-4CED-B969-4570866037C4}';
  GUID_BLUETOOTH_PIN_REQUEST: TGUID =        '{BD198B7C-24AB-4B9A-8C0D-A8EA8349AA16}';
  GUID_BLUETOOTH_L2CAP_EVENT: TGUID =        '{7EAE4030-B709-4AA8-AC55-E953829C9DAA}';
  GUID_BLUETOOTH_HCI_EVENT: TGUID =          '{FC240062-1541-49BE-B463-84C4DCD7BF7F}';

  DBT_DEVNODES_CHANGED        = $0007;
  DBT_DEVICEARRIVAL           = $8000;  // system detected a new device
  DBT_DEVICEQUERYREMOVE       = $8001;  // wants to remove, may fail
  DBT_DEVICEQUERYREMOVEFAILED = $8002;  // removal aborted
  DBT_DEVICEREMOVEPENDING     = $8003;  // about to remove,
  DBT_DEVICEREMOVECOMPLETE    = $8004;  // device is gone
  DBT_DEVICETYPESPECIFIC      = $8005;
  DBT_CUSTOMEVENT             = $8006; // user-defined event

  DBT_DEVTYP_OEM              = $00000000; // oem-defined device type
  DBT_DEVTYP_DEVNODE          = $00000001; // devnode number
  DBT_DEVTYP_VOLUME           = $00000002; // logical volume
  DBT_DEVTYP_PORT             = $00000003; // serial, parallel
  DBT_DEVTYP_NET              = $00000004; // network resource
  DBT_DEVTYP_DEVICEINTERFACE  = $00000005; // device interface class
  DBT_DEVTYP_HANDLE           = $00000006; // file system handle

  DEVICE_NOTIFY_ALL_INTERFACE_CLASSES = $00000004;

  DBTF_MEDIA = $0001;
  DBTF_NET   = $0002;

  DBTF_RESOURCE = $00000001; // network resource
  DBTF_XPORT    = $00000002; // new transport coming or going
  DBTF_SLOWNET  = $00000004; // new incoming transport is slow

type

  TMiTeC_DeviceMonitor = class(TComponent)
  private
    FWindowHandle: HWND;
    FHDN_BT, FHDN_USB: HDEVNOTIFY;
    FOnDevConnect: TDeviceChangeEvent;
    FOnDevDisconnect: TDeviceChangeEvent;
    FOnMsg: TDeviceChangeMessage;
    FOnVolDisconnect: TVolumeChangeEvent;
    FOnVolConnect: TVolumeChangeEvent;
    FActive: Boolean;
    hRadio: THandle;
    FBTH: Boolean;
    procedure SetActive(const Value: Boolean);
    procedure WndProc(var Msg: TMessage);
    function USBRegister: Boolean;
    function BTRegister: Boolean;
    procedure GetBTDeviceDescNT(AData: PBthHciEventInfo; var DeviceDesc: TDeviceDesc);
    procedure SetBTH(const Value: Boolean);
  protected
    procedure WMDEVICECHANGE(var Msg: TMessage); dynamic;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Active: Boolean read FActive write SetActive;
    property CatchBluetooth: Boolean read FBTH write SetBTH;
    property OnMessage: TDeviceChangeMessage read FOnMsg write FOnMsg;
    property OnDeviceConnect: TDeviceChangeEvent read FOnDevConnect write FOnDevConnect;
    property OnDeviceDisconect: TDeviceChangeEvent read FOnDevDisconnect write FOnDevDisconnect;
    property OnVolumeConnect: TVolumeChangeEvent read FOnVolConnect write FOnVolConnect;
    property OnVolumeDisconnect: TVolumeChangeEvent read FOnVolDisconnect write FOnVolDisconnect;
  end;

  TRegisterDeviceNotification = function (hRecipient: THandle; NotificationFilter: Pointer; Flags: DWORD): HDEVNOTIFY; stdcall;
  TUnregisterDeviceNotification = function (Handle: HDEVNOTIFY): BOOL; stdcall;

procedure GetDeviceDesc(var DeviceDesc: TDeviceDesc);
procedure GetUSBDeviceDesc(var DeviceDesc: TDeviceDesc);

function GetMessageType(AType: Cardinal): string;
function GetDeviceType(AType: Cardinal): string;

var
  _RegisterDeviceNotification: TRegisterDeviceNotification;
  _UnregisterDeviceNotification: TUnregisterDeviceNotification;

implementation

uses {$IFDEF RAD9PLUS}
     System.Math, System.Win.Registry, System.Win.ComObj,
     {$ELSE}
     Math, Registry,
     {$ENDIF}
     MiTeC_Routines, MiTeC_RegUtils;

function AddressToStr(const Adr: BLUETOOTH_ADDRESS): string;
var
  i: byte;
begin
  Result:=Format('%2.2x',[Adr.rgBytes[0]]);
  for i:=1 to 5 do
    Result:=Format('%2.2x:%s',[Adr.rgBytes[i],Result]);
end;

function GetMessageType(AType: Cardinal): string;
begin
  case AType of
    DBT_DEVNODES_CHANGED: Result:='DBT_DEVNODES_CHANGED';
    DBT_DEVICEARRIVAL: Result:='DBT_DEVICEARRIVAL';
    DBT_DEVICEQUERYREMOVE: Result:='DBT_DEVICEQUERYREMOVE';
    DBT_DEVICEQUERYREMOVEFAILED: Result:='DBT_DEVICEQUERYREMOVEFAILED';
    DBT_DEVICEREMOVEPENDING: Result:='DBT_DEVICEREMOVEPENDING';
    DBT_DEVICEREMOVECOMPLETE: Result:='DBT_DEVICEREMOVECOMPLETE';
    DBT_DEVICETYPESPECIFIC: Result:='DBT_DEVICETYPESPECIFIC';
    DBT_CUSTOMEVENT: Result:='DBT_CUSTOMEVENT';
    else Result:=Format('UNKNOWN(0x%x)',[AType]);
  end;
end;

function GetDeviceType(AType: Cardinal): string;
begin
  case AType of
    DBT_DEVTYP_OEM: Result:='DBT_DEVTYP_OEM';
    DBT_DEVTYP_DEVNODE: Result:='DBT_DEVTYP_DEVNODE';
    DBT_DEVTYP_VOLUME: Result:='DBT_DEVTYP_VOLUME';
    DBT_DEVTYP_PORT: Result:='DBT_DEVTYP_PORT';
    DBT_DEVTYP_NET: Result:='DBT_DEVTYP_NET';
    DBT_DEVTYP_DEVICEINTERFACE: Result:='DBT_DEVTYP_DEVICEINTERFACE';
    DBT_DEVTYP_HANDLE: Result:='DBT_DEVTYP_HANDLE';
    else Result:=Format('UNKNOWN(0x%x)',[AType]);
  end;
end;

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

procedure GetDeviceDesc;
const
  rkNT = 'SYSTEM\CurrentControlSet\Enum\';
    rvLI = 'LocationInformation';
    rvDD = 'DeviceDesc';
    rvD = 'Driver';
    rvFN = 'FriendlyName';
    rvC = 'Class';
var
  s,rk,dn: string;
  sl: TStringList;
  kl: TStringList;
begin
  s:=DeviceDesc.SymbolicName;
  s:=Copy(s,5,Length(s));
  (*
  p:=Pos('{',s);
  while p>0 do begin
    p1:=Pos('}',s);
    Delete(s,p,p1-p+1);
    p:=Pos('{',s);
  end;
  *)
  s:=ExtractFilePath(StringReplace(s,'#','\',[rfReplaceAll,rfIgnorecase]));
  rk:=rkNT+s;
  with OpenRegistryReadOnly do begin
    sl:=TStringList.Create;
    kl:=TStringList.Create;
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rk,False) then begin
        if ValueExists(rvLI) then
          DeviceDesc.Location:=Trim(ReadString(rvLI));
        if ValueExists(rvDD) then begin
          DeviceDesc.Description:=Trim(ReadString(rvDD));
          DeviceDesc.Description:=Copy(DeviceDesc.Description,Pos(';',DeviceDesc.Description)+1,1024);
        end;
        if ValueExists(rvD) then
          DeviceDesc.Driver:=ReadString(rvD);
        if ValueExists(rvC) then
          DeviceDesc.ClassName:=ReadString(rvC);
        if ValueExists(rvFN) then begin
          DeviceDesc.Name:=ReadString(rvFN);
          DeviceDesc.Name:=Copy(DeviceDesc.Name,Pos(';',DeviceDesc.Name)+1,1024);
        end;
        dn:=ParseDeviceName(s);
        if dn<>'' then
          DeviceDesc.Name:=dn;
        if (DeviceDesc.Name='') and not SameText(DeviceDesc.Classname,'USB') then
          DeviceDesc.Name:=ExtractFilename(ExcludeTrailingPathDelimiter(ExtractFilePath(ExcludeTrailingPathDelimiter(s))));
        if DeviceDesc.Location='' then
          DeviceDesc.Location:=DeviceDesc.Description;
        CloseKey;
      end;
    finally
      kl.Free;
      sl.Free;
      Free;
    end;
  end;
end;

procedure GetUSBDeviceDesc;
const
  rkUSB = 'SYSTEM\CurrentControlSet\Enum\USB';
    rvSN = 'SymbolicName';
    rvPIP = 'ParentIdPrefix';
    rvLI = 'LocationInformation';
    rvDD = 'DeviceDesc';
    rvD = 'Driver';
    rvSrv = 'Service';
  rkUSBSRV = 'SYSTEM\CurrentControlSet\Enum\%s';
    rvFN = 'FriendlyName';
    rvC = 'Class';
var
  sl,kl: TStringList;
  i,j: Integer;
  s,sn,pip,rk,srv: string;
begin
  s:=DeviceDesc.SymbolicName;
  s:=Uppercase(s);
  s:=Copy(s,Pos('USB',s),Length(s));
  with OpenRegistryReadOnly do begin
    sl:=TStringList.Create;
    kl:=TStringList.Create;
    sn:='';
    pip:='';
    srv:='';
    try
      Rootkey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkUSB,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if OpenKey(Format('%s\%s',[rkUSB,sl[i]]),False) then begin
            GetKeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do begin
              sn:='';
              pip:='';
              srv:='';
              if OpenKey(Format('%s\%s\%s\Device Parameters',[rkUSB,sl[i],kl[j]]),False) then begin
                if ValueExists(rvSN) then begin
                  sn:=ReadString(rvSN);
                  sn:=Uppercase(sn);
                  sn:=Copy(sn,Pos('USB',sn),Length(sn));
                end;
                CloseKey;
                if SameText(sn,s) then begin
                  if OpenKey(Format('%s\%s\%s',[rkUSB,sl[i],kl[j]]),False) then begin
                    if ValueExists(rvPIP) then
                      pip:=ReadString(rvPIP)
                    else
                      pip:=kl[j];
                    if ValueExists(rvLI) then
                      DeviceDesc.Location:=Trim(ReadString(rvLI));
                    if ValueExists(rvDD) then begin
                      DeviceDesc.Description:=Trim(ReadString(rvDD));
                      DeviceDesc.Description:=Copy(DeviceDesc.Description,Pos(';',DeviceDesc.Description)+1,1024);
                    end;
                    if ValueExists(rvD) then
                      DeviceDesc.Driver:=ReadString(rvD);
                    if ValueExists(rvSrv) then
                      srv:=ReadString(rvSrv);
                    CloseKey;
                    Break;
                  end;
                end;
              end;
            end;
          end;
          if pip<>'' then
            Break;
        end;
      end;

      if pip='' then
        Exit;
      if srv='' then
        srv:='USBSTOR';

      rk:=Format(rkUSBSRV,[srv]);
      if OpenKey(rk,False) then begin
        GetKeyNames(sl);
        CloseKey;
        for i:=0 to sl.Count-1 do begin
          if OpenKey(Format('%s\%s',[rk,sl[i]]),False) then begin
            GetkeyNames(kl);
            CloseKey;
            for j:=0 to kl.Count-1 do begin
              if Pos(pip,kl[j])=1 then begin
                if OpenKey(Format('%s\%s\%s',[rk,sl[i],kl[j]]),False) then begin
                    DeviceDesc.Name:=ParseDeviceName(sl[i]);
                  if DeviceDesc.Name='' then
                    if ValueExists(rvFN) then begin
                      DeviceDesc.Name:=Trim(ReadString(rvFN));
                      DeviceDesc.Name:=Copy(DeviceDesc.Name,Pos(';',DeviceDesc.Name)+1,1024);
                    end;
                  if ValueExists(rvC) then
                    DeviceDesc.ClassName:=Trim(ReadString(rvC));
                  CloseKey;
                end;
                Break;
              end;
            end;
          end;
        end;
      end;
    finally
      sl.Free;
      kl.Free;
      Free;
    end;
  end;
end;

function GetVolumeDrive(AUnitMask: Cardinal): string;
var
  i,n: Cardinal;
begin
  Result:='';
  for i:=0 to 25 do begin
    n:=Round(Power(2,i));
    if n and AUnitMask>0 then
      Result:=Result+Chr(i+65);
  end;
end;

function TMiTeC_DeviceMonitor.BTRegister: Boolean;
var
  dbi: DEV_BROADCAST_HANDLE;
  Size: Integer;
  hFindRadio: HBLUETOOTH_RADIO_FIND;
  btrp: BLUETOOTH_FIND_RADIO_PARAMS;
begin
  Result:=False;
  if not Assigned(BluetoothFindFirstRadio) then
    Exit;
  btrp.dwSize:=SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  hFindRadio:=BluetoothFindFirstRadio(@btrp,hRadio);
  if hFindRadio=0 then
    Exit;
  try
    while BluetoothFindNextRadio(hFindRadio,hRadio) do begin

    end;
  finally
    BluetoothFindRadioClose(hFindRadio);
  end;
  Size:=SizeOf(DEV_BROADCAST_HANDLE);
  ZeroMemory(@dbi,Size);
  dbi.dbch_size:=Size;
  dbi.dbch_devicetype:=DBT_DEVTYP_HANDLE;
  dbi.dbch_reserved:=0;
  dbi.dbch_eventguid:=GUID_BLUETOOTH_HCI_EVENT;
  dbi.dbch_nameoffset:=0;
  dbi.dbch_handle:=hRadio;
  dbi.dbch_hdevnotify:=0;
  if Assigned(_RegisterDeviceNotification) then begin
    FHDN_BT:=_RegisterDeviceNotification(FWindowHandle,@dbi,DEVICE_NOTIFY_WINDOW_HANDLE);
    Result:={$IFDEF FPC}FHDN_BT<>0{$ELSE}Assigned(FHDN_BT){$ENDIF};
  end;
end;

constructor TMiTeC_DeviceMonitor.Create(AOwner: TComponent);
begin
  FActive:=False;
  FBTH:=False;
  inherited Create(AOwner);
  FHDN_USB:={$IFDEF FPC}0{$ELSE}nil{$ENDIF};
  FHDN_BT:={$IFDEF FPC}0{$ELSE}nil{$ENDIF};
end;

destructor TMiTeC_DeviceMonitor.Destroy;
begin
  Active:=False;
  inherited Destroy;
end;

procedure TMiTeC_DeviceMonitor.GetBTDeviceDescNT(AData: PBthHciEventInfo;
  var DeviceDesc: TDeviceDesc);
var
  btdi: BLUETOOTH_DEVICE_INFO;
begin
  btdi.dwSize:=SizeOf(btdi);
  btdi.Address.ullLong:=AData^.bthAddress;
  Devicedesc.Name:=AddressToStr(btdi.Address);
  if not Assigned(BluetoothGetDeviceInfo) or
    (BluetoothGetDeviceInfo(hRadio,btdi)<>ERROR_SUCCESS) then
    Exit;
  DeviceDesc.Location:=btdi.szName;
end;

procedure TMiTeC_DeviceMonitor.SetActive(const Value: Boolean);
begin
  if (FActive=Value) then
    Exit;
  FActive:=Value;
  if (csDesigning in ComponentState) then
    Exit;
  if FActive then begin
    {$IFDEF TRIAL}
    if not RunFromIDE then
      MessageDlg(Self.ClassName+sLineBreak+cCompName+sLineBreak+cCopyright,mtInformation,[mbOK],0);
    {$ENDIF}
    FWindowHandle:=AllocateHWnd(WndProc);
    FActive:=USBRegister and (not FBTH or BTRegister);
  end else begin
    if Assigned(_UnRegisterDeviceNotification) then begin
      if {$IFDEF FPC}FHDN_USB<>0{$ELSE}Assigned(FHDN_USB){$ENDIF} then
        _UnregisterDeviceNotification(FHDN_USB);
      if {$IFDEF FPC}(FHDN_BT<>0){$ELSE}Assigned(FHDN_BT){$ENDIF} and FBTH then begin
        _UnregisterDeviceNotification(FHDN_BT);
        CloseHandle(hRadio);
      end;
    end;
    DeallocateHWnd(FWindowHandle);
  end;
end;

procedure TMiTeC_DeviceMonitor.SetBTH(const Value: Boolean);
begin
  if FActive then
    Exit;
  FBTH:=Value;
end;

procedure TMiTeC_DeviceMonitor.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg=WM_DEVICECHANGE) then begin
    try
      WMDeviceChange(Msg);
    except
      Application.HandleException(Self);
    end;
  end else
    Msg.Result:=DefWindowProc(FWindowHandle,Msg.Msg,Msg.wParam,Msg.lParam);
end;

procedure TMiTeC_DeviceMonitor.WMDEVICECHANGE(var Msg: TMessage);
var
  devType: Integer;
  dbh: PDevBroadcastHdr;
  DeviceDesc: TDeviceDesc;
begin
  if Assigned(FOnMsg) then
    FOnMsg(Self,Msg);
  if (Msg.wParam=DBT_DEVICEARRIVAL) or (Msg.wParam=DBT_DEVICEREMOVECOMPLETE) or (Msg.wParam=DBT_CUSTOMEVENT) then begin
    dbh:=PDevBroadcastHdr(Msg.lParam);
    devType:=dbh^.dbch_devicetype;
    if (devType=DBT_DEVTYP_DEVICEINTERFACE) then begin
      if Assigned(FOnDevConnect) or Assigned(FOnDevDisConnect) then begin
        ZeroMemory(@DeviceDesc,SizeOf(DeviceDesc));
        DeviceDesc.SymbolicName:=string(PAnsiChar(@PDevBroadcastDeviceInterface(msg.lParam)^.dbcc_name));
        if Length(DeviceDesc.SymbolicName)=1 then
          DeviceDesc.SymbolicName:=string(PWideChar(@PDevBroadcastDeviceInterface(msg.lParam)^.dbcc_name));
        DeviceDesc.GUID:=GUIDToString(PDevBroadcastDeviceInterface(msg.lParam)^.dbcc_classguid);
        if SameText(GUIDToString(PDevBroadcastDeviceInterface(msg.lParam).dbcc_classguid),GUIDToString(GUID_DEVINTERFACE_USB_DEVICE)) then
          GetUSBDeviceDesc(DeviceDesc)
        else
          GetDeviceDesc(DeviceDesc);
      end;
      if (Msg.wParam=DBT_DEVICEARRIVAL) and Assigned(FOnDevConnect) then
        FOnDevConnect(Self,DeviceDesc);
      if (Msg.wParam=DBT_DEVICEREMOVECOMPLETE) and Assigned(FOnDevDisConnect) then
        FOnDevDisconnect(Self,DeviceDesc);
    end;
    if (devType=DBT_DEVTYP_VOLUME) then begin
      if Msg.wParam=DBT_DEVICEARRIVAL then begin
        if Assigned(FOnVolConnect) then begin
          FOnVolConnect(Self,GetVolumeDrive(PDevBroadcastVolume(Msg.lParam).dbcv_unitmask),PDevBroadcastVolume(Msg.lParam).dbcv_flags=DBTF_NET);
        end;
      end else begin
        if Assigned(FOnVolDisconnect) then begin
          FOnVolDisConnect(Self,GetVolumeDrive(PDevBroadcastVolume(Msg.lParam).dbcv_unitmask),PDevBroadcastVolume(Msg.lParam).dbcv_flags=DBTF_NET);
        end;
      end;
    end;
    if (devType=DBT_DEVTYP_HANDLE) and
       SameText(GUIDToString(PDevBroadcastHandle(msg.lParam).dbch_eventguid),GUIDToString(GUID_BLUETOOTH_HCI_EVENT)) then begin
      if Assigned(FOnDevConnect) or Assigned(FOnDevDisConnect) then begin
        ZeroMemory(@DeviceDesc,SizeOf(DeviceDesc));
        DeviceDesc.GUID:=GUIDToString(PDevBroadcastHandle(msg.lParam).dbch_eventguid);
        GetBTDeviceDescNT(PBthHciEventInfo(@PDevBroadcastHandle(msg.lParam).dbch_data),DeviceDesc);
      end;
      if (PBthHciEventInfo(@PDevBroadcastHandle(msg.lParam).dbch_data).connected=1) and Assigned(FOnDevConnect) then
        FOnDevConnect(Self,DeviceDesc);
      if (PBthHciEventInfo(@PDevBroadcastHandle(msg.lParam).dbch_data).connected=0) and Assigned(FOnDevDisConnect) then
        FOnDevDisconnect(Self,DeviceDesc);
    end;
  end;
end;

function TMiTeC_DeviceMonitor.USBRegister: Boolean;
var
  dbi: DEV_BROADCAST_DEVICEINTERFACE;
  Size: Integer;
begin
  Size:=SizeOf(DEV_BROADCAST_DEVICEINTERFACE);
  ZeroMemory(@dbi,Size);
  dbi.dbcc_size:=Size;
  dbi.dbcc_devicetype:=DBT_DEVTYP_DEVICEINTERFACE;
  dbi.dbcc_reserved:=0;
  dbi.dbcc_classguid:=GUID_DEVINTERFACE_USB_DEVICE;
  dbi.dbcc_name:=0;
  if Assigned(_RegisterDeviceNotification) then begin
    if Is2K then
      FHDN_USB:=_RegisterDeviceNotification(FWindowHandle,@dbi,DEVICE_NOTIFY_WINDOW_HANDLE)
    else
      FHDN_USB:=_RegisterDeviceNotification(FWindowHandle,@dbi,DEVICE_NOTIFY_WINDOW_HANDLE or DEVICE_NOTIFY_ALL_INTERFACE_CLASSES);
    Result:={$IFDEF FPC}(FHDN_USB<>0){$ELSE}Assigned(FHDN_USB){$ENDIF};
  end else
    Result:=False;
end;

var
  User32Handle: THandle;
initialization
  User32Handle:=GetModuleHandle(user32);
  if User32Handle=0 then
    User32Handle:=LoadLibrary(user32);
  if User32Handle<>0 then begin
    @_RegisterDeviceNotification:=GetProcAddress(User32Handle,'RegisterDeviceNotificationA');
    @_UnRegisterDeviceNotification:=GetProcAddress(User32Handle,'UnRegisterDeviceNotification');
  end;
end.



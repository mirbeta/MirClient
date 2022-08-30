{*******************************************************}
{       MiTeC System Information Component Suite        }
{              Bluetooth Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_BT;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj,
     {$ENDIF}
     MiTeC_SS, MSI_Common, MSI_Defs, MiTeC_BTAPI;

const
  StorageFolderName = 'Bluetooth';

type
  TBTDevice = record
    Name: string;
    LastUsed,
    LastSeen: TDateTime;
    Authenticated,
    Remembered,
    Connected: Boolean;
    DeviceClass: Cardinal;
    Address: String;
  end;

  TBTDevices = array of TBTDevice;

  TMiTeC_BT= class(TMiTeC_Component)
  private
    FBTD: TBTDevices;
    function GetDevice(Index: integer): TBTDevice;
    function GetDeviceCount: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property Devices[Index: integer]: TBTDevice read GetDevice;
  published
    property DeviceCount: Integer read GetDeviceCount;
  end;

implementation

function AddressToStr(const Adr: BLUETOOTH_ADDRESS): string;
var
  i: byte;
begin
  Result:=Format('%2.2x',[Adr.rgBytes[0]]);
  for i:=1 to 5 do
    Result:=Format('%2.2x:%s',[Adr.rgBytes[i],Result]);
end;

{ TMiTeC_BT }

procedure TMiTeC_BT.Clear;
begin
  Finalize(FBTD);
end;

destructor TMiTeC_BT.Destroy;
begin
  Finalize(FBTD);
  inherited;
end;

procedure TMiTeC_BT.RefreshData;

procedure AddDevice(ADevice: BLUETOOTH_DEVICE_INFO);
begin
  if ADevice.szName='' then
    Exit;
  SetLength(FBTD,Length(FBTD)+1);
  with FBTD[High(FBTD)] do begin
    Name:=ADevice.szName;
    Address:=AddressToStr(Adevice.Address);
    Authenticated:=Adevice.fAuthenticated;
    Connected:=Adevice.fConnected;
    Remembered:=Adevice.fRemembered;
    DeviceClass:=Adevice.ulClassofDevice;
    try LastSeen:=SystemTimeToDatetime(Adevice.stLastSeen); except LastSeen:=0; end;
    try LastUsed:=SystemTimeToDatetime(Adevice.stLastUsed); except LastUsed:=0; end;
  end;
end;

var
  {hFindRadio: HBLUETOOTH_RADIO_FIND;
  btrp: BLUETOOTH_FIND_RADIO_PARAMS;
  hRadio: THandle;}
  hFindDevice: HBLUETOOTH_DEVICE_FIND;
  btsp: BLUETOOTH_DEVICE_SEARCH_PARAMS;
  btdi: BLUETOOTH_DEVICE_INFO;

begin
  inherited;

  Clear;

  if not Assigned(BluetoothFindFirstRadio) then
    Exit;

  {btrp.dwSize:=SizeOf(BLUETOOTH_FIND_RADIO_PARAMS);
  hFindRadio:=BluetoothFindFirstRadio(@btrp,hRadio);
  if hFindRadio=0 then
    Exit;}
  try
    ZeroMemory(@btdi,SizeOf(btdi));
    btdi.dwSize:=SizeOf(btdi);
    ZeroMemory(@btsp,SizeOf(btsp));
    btsp.hRadio:=0;//hRadio;
    btsp.fReturnAuthenticated:=True;
    btsp.fReturnRemembered:=True;
    btsp.fReturnUnknown:=True;
    btsp.fReturnConnected:=True;
    btsp.dwSize:=SizeOf(btsp);
    hFindDevice:=BluetoothFindFirstDevice(@btsp,btdi);
    if (hFindDevice<>0) then
      try
        AddDevice(btdi);
        while BluetoothFindNextDevice(hFindDevice,btdi) do
          AddDevice(btdi);
      finally
        BluetoothFindDeviceClose(hFindDevice);
      end;
    {CloseHandle(hRadio);
    while BluetoothFindNextRadio(hFindRadio,hRadio) do begin
      btsp.hRadio:=hRadio;
      hFindDevice:=BluetoothFindFirstDevice(@btsp,btdi);
      if (hFindDevice<>0) then
        try
          AddDevice(btdi);
          while BluetoothFindNextDevice(hFindDevice,btdi) do
            AddDevice(btdi);
        finally
          BluetoothFindDeviceClose(hFindDevice);
        end;
      CloseHandle(hRadio);
    end;}
  finally
    //BluetoothFindRadioClose(hFindRadio);
  end;

  SetDataAvail(True);
end;

function TMiTeC_BT.GetDevice(Index: integer): TBTDevice;
begin
  try Result:=FBTD[Index] except end;
end;

function TMiTeC_BT.GetDeviceCount: Integer;
begin
  Result:=Length(FBTD);
end;

function TMiTeC_BT.LoadFromStorage;
var
  stg: IStorage;
  SS, Sub: TStructuredStorage;

function ReadFromStream(AIndex: integer): boolean;
var
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=False;
  try strm:=Sub.OpenStream(Format(strm_Item,[AIndex]),STG_READ_INSTORAGE,False) except strm:=nil end;
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            SetLength(FBTD,Length(FBTD)+1);
            with FBTD[High(FBTD)] do begin
              Name:=ReadStrProperty(sl,'Name');
              Address:=ReadStrProperty(sl,'Address');
              DeviceClass:=ReadIntProperty(sl,'Class');
              Authenticated:=ReadIntProperty(sl,'Authenticated')=1;
              Remembered:=ReadIntProperty(sl,'Remembered')=1;
              Connected:=ReadIntProperty(sl,'Connected')=1;
              LastUsed:=ReadDTProperty(sl,'LastUsed');
              LastSeen:=ReadDTProperty(sl,'LastSeen');
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
    if Assigned(Sub) then begin
      try
        i:=0;
        Result:=i>0;
        while ReadFromStream(i) do
          Inc(i);
        Result:=Result or (i>0);
      finally
        Sub.Free;
      end;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_BT.SaveToStorage;
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
    WriteStrProperty(sl,'Address',Self.Devices[AIndex].Address);
    WriteIntProperty(sl,'Class',Self.Devices[AIndex].DeviceClass);
    WriteIntProperty(sl,'Authenticated',Integer(Self.Devices[AIndex].Authenticated));
    WriteIntProperty(sl,'Remembered',Integer(Self.Devices[AIndex].Remembered));
    WriteIntProperty(sl,'Connected',Integer(Self.Devices[AIndex].Connected));
    WriteDTProperty(sl,'LastSeen',Self.Devices[AIndex].LastSeen);
    WriteDTProperty(sl,'LastUsed',Self.Devices[AIndex].LastUsed);
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

initialization
  InitBTHAPI;
end.

{*******************************************************}
{       MiTeC System Information Component Suite        }
{                Machine Detection Part                 }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MSI_Machine;

interface

uses{$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_Windows, MSI_Common, MSI_Defs, MSI_SMBIOS, MiTeC_SysUtils, MiTeC_Routines{, MSI_Devices};

const
  StorageFolderName = 'Machine';
  BIOS_StorageFolderName = 'BIOS';
  BIOS_DataStream = 'BIOSData';

type
  TBIOSValue = record
    Name,
    Value: string;
  end;

  TMiTeC_BIOS = class(TMiTeC_Component)
  private
    FBIOSExtendedInfo: string;
    FBIOSCopyright: string;
    FBIOSName: string;
    FBIOSDate: string;
    FBIOSData: TStringList;
    function GetCount: Integer;
    function GetValue(AIndex: Integer): TBIOSValue;
    function GetNameValue(AName: string): TBIOSValue;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    property BIOSDataCount: Integer read GetCount;
    property BIOSData[AIndex: Integer]: TBIOSValue read GetValue;
    property BIOSValue[AName: string]: TBIOSValue read GetNameValue;
  published
    property Copyright: string read FBIOSCopyright stored false;
    property Date: string read FBIOSDate stored false;
    property ExtendedInfo: string read FBIOSExtendedInfo stored false;
    property NameString: string read FBIOSName stored false;
  end;

  TMiTeC_Machine = class(TMiTeC_Component)
  private
    FName: string;
    FLastBoot: TDatetime;
    FUser: string;
    FSystemUpTime: int64;
    FScrollLock: Boolean;
    FNumLock: Boolean;
    FCapsLock: Boolean;
    FComp: string;
    FSMBIOS: TMiTeC_SMBIOS;
    FBIOS: TMiTeC_BIOS;
    FJoinName: string;
    FST: TSessionTypes;
    FSP: TSessionProtocol;
    FIsInDomain: Boolean;
    FLastShut: TDatetime;
    FIsAdmin: Boolean;
    //FDevices: TMiTeC_Devices;
    function GetSystemUpTime: int64;
  protected
    procedure SetHeaderReader(const Value: THeaderReader); override;
    procedure SetHeaderWriter(const Value: THeaderWriter); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
  published
    //property MiTeC_Devices: TMiTeC_Devices read FDevices write FDevices;
    property MachineName: string read FName stored false;
    property User: string read FUser stored false;
    property IsInDomain: Boolean read FIsInDomain stored False;
    property JoinedTo: string read FJoinName stored False;
    property SystemUpTime: int64 read FSystemUpTime stored false;
    property LastBoot: TDatetime read FLastBoot stored false;
    property LastShutdown: TDatetime read FLastShut stored False;
    property CapsLock: Boolean read FCapsLock stored false;
    property NumLock: Boolean read FNumLock stored false;
    property ScrollLock: Boolean read FScrollLock stored false;
    property Computer: string read FComp stored False;
    property SMBIOS: TMiTeC_SMBIOS read FSMBIOS stored False;
    property BIOS: TMiTeC_BIOS read FBIOS stored False;
    property Session: TSessionTypes read FST stored False;
    property SessionProtocol: TSessionProtocol read FSP stored False;
    property AdminRights: Boolean read FIsAdmin stored False;
  end;


implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry,
     {$ELSE}
     Registry,
     {$ENDIF}
     MiTeC_StrUtils, MiTeC_Datetime, MiTeC_RegUtils, MiTeC_CfgMgrSetupAPI;

{ TMiTeC_Machine }

function TMiTeC_Machine.GetSystemUpTime;
begin
  try
    FSystemUpTime:=Round(GetTickCount64/1000);
  except
    FSystemUpTime:=0;
  end;
  result:=FSystemUpTime;
end;

function TMiTeC_Machine.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  Clear;
  inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B+}
  Result:=BIOS.LoadFromStorage(AFilename,AReadHeader,ACodeStream)
          and SMBIOS.LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  {$B-}
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;
    if Sub<>nil then

    try
      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FName:=ReadStrProperty(sl,'MachineName');
            Self.FIsInDomain:=ReadIntProperty(sl,'IsInDomain')=1;
            Self.FIsAdmin:=ReadIntProperty(sl,'IsAdmin')=1;
            Self.FLastBoot:=ReadDtProperty(sl,'LastBoot');
            Self.FLastShut:=ReadDtProperty(sl,'LastShutdown');
            Self.FUser:=ReadStrProperty(sl,'User');
            Self.FJoinName:=ReadStrProperty(sl,'JoinName');
            if Self.FJoinName='' then
              Self.FJoinName:=ReadStrProperty(sl,'Domain');
            Self.FComp:=ReadStrProperty(sl,'Computer');
            Self.FScrollLock:=ReadIntProperty(sl,'ScrollLock')=1;
            Self.FNumLock:=ReadIntProperty(sl,'NumLock')=1;
            Self.FCapsLock:=ReadIntProperty(sl,'CapsLock')=1;
            Self.FSystemUpTime:=ReadIntProperty(sl,'SystemUpTime');
            Self.FST:=IntAsSessionTypes(ReadIntProperty(sl,'Session'));
            Self.FSP:=TSessionProtocol(ReadIntProperty(sl,'SessionProto'));
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

    finally
      if Sub<>nil then
        Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Machine.RefreshData;
var
  keyState: TKeyboardState;
  rki: TRegKeyInfo;
  dinfo: TSPDevInfoData;
  intf: TSPDeviceInterfaceData;
  pdidd: PSPDeviceInterfaceDetailData;
  i: Integer;
  hdev: HDEVINFO;
  n,pt: Cardinal;
  c: string;
  buf: array[0..255] of byte;
begin
  inherited;
  Clear;
  FIsAdmin:=IsAdmin;
  FJoinName:=GetDomain;
  FIsInDomain:=IsInDomain;
  {if Assigned(FDevices) then
    with FDevices do begin
      if not (Owner is TMiTeC_Component) or not FDevices.DataAvailable then
        RefreshData;
      for i:=0 to DeviceCount-1 do
        if Devices[i].DeviceClass=dcComputer then begin
          FComp:=Devices[i].Name;
          Break;
        end;
    end;}

  hdev:=SetupDiGetClassDevs(nil,nil,0,DIGCF_ALLCLASSES);
  if (INVALID_HANDLE_VALUE<>THandle(hdev)) then
    try
      i:=0;
      pt:=0;
      dinfo.cbSize:=sizeof(TSPDevInfoData);
      while SetupDiEnumDeviceInfo(hDev,i,dinfo) do begin
        if Assigned(SetupDiGetDeviceProperty) then begin
          SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_Class,pt,@buf,sizeof(buf),nil,0);
          c:=string(PChar(@buf));
          if SameText(c,'computer') then begin
            SetupDiGetDeviceProperty(hDev,@dinfo,@DEVPKEY_Device_DeviceDesc,pt,@buf,sizeof(buf),nil,0);
            FComp:=string(PChar(@buf));
            Break;
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
                  c:=GetString(hdev,dinfo,SPDRP_CLASS);
                  if SameText(c,'computer') then begin
                    FComp:=GetString(hdev,dinfo,SPDRP_DEVICEDESC);
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

  BIOS.RefreshData;
  SMBIOS.RefreshData;
  try
    FLastBoot:=Now-(GetTickCount64/1000)/(24*3600);
  except
    FLastBoot:=0;
  end;
  FSystemUpTime:=GetSystemUpTime;
  FName:=GetMachine;
  FUser:=GetLoggedUser(SessionID);
  if FUser='' then
    FUser:=GetUser;
  GetKeyboardState(KeyState);
  FCapsLock:=KeyState[VK_CAPITAL]=1;
  FNumLock:=KeyState[VK_NUMLOCK]=1;
  FScrollLock:=KeyState[VK_SCROLL]=1;

  FSP:=TSessionProtocol(GetTSSessionProto(SessionID));
  FST:=GetSession;

  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey('SYSTEM\CurrentControlSet\Control\Windows',False) then begin
        GetKeyInfo(rki);
        FLastShut:=UTCToLocalDatetime({$IFNDEF FPC}FileTimeToDateTime{$ENDIF}(rki.FileTime));
      end;
    finally
      Free;
    end;
  SetDataAvail(True);
end;

procedure TMiTeC_Machine.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  BIOS.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  SMBIOS.SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'MachineName',Self.MachineName);
        WriteStrProperty(sl,'User',Self.User);
        WriteIntProperty(sl,'IsAdmin',Integer(Self.AdminRights));
        WriteIntProperty(sl,'IsInDomain',Integer(Self.IsInDomain));
        WriteStrProperty(sl,'JoinName',Self.JoinedTo);
        if IsInDomain then
          WriteStrProperty(sl,'Domain',Self.JoinedTo);
        WriteStrProperty(sl,'Computer',Self.Computer);
        WriteIntProperty(sl,'SystemUpTime',Self.SystemUpTime);
        WriteDtProperty(sl,'LastBoot',Self.LastBoot);
        WriteDtProperty(sl,'LastShutdown',Self.LastShutdown);
        WriteIntProperty(sl,'ScrollLock',Integer(Self.ScrollLock));
        WriteIntProperty(sl,'NumlLock',Integer(Self.NumLock));
        WriteIntProperty(sl,'CapsLock',Integer(Self.CapsLock));
        WriteIntProperty(sl,'Session',SessionTypesAsInt(Self.Session));
        WriteIntProperty(sl,'SessionProto',Integer(Self.SessionProtocol));

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_Machine.SetHeaderReader(const Value: THeaderReader);
begin
  inherited;
  FBIOS.OnReadHeader:=Value;
  FSMBIOS.OnReadHeader:=Value;
end;

procedure TMiTeC_Machine.SetHeaderWriter(const Value: THeaderWriter);
begin
  inherited;
  FBIOS.OnWriteHeader:=Value;
  FSMBIOS.OnWriteHeader:=Value;
end;

procedure TMiTeC_Machine.Clear;
begin
  inherited;
  FBIOS.Clear;
  FSMBIOS.Clear;
end;

constructor TMiTeC_Machine.Create;
begin
  inherited Create(AOwner);
  FSMBIOS:=TMiTeC_SMBIOS.Create(Self);
  FSMBIOS.Name:='SMBIOS';
  FBIOS:=TMiTeC_BIOS.Create(Self);
  FBIOS.Name:='BIOS';
end;

destructor TMiTeC_Machine.Destroy;
begin
  FBIOS.Free;
  FSMBIOS.Free;
  inherited;
end;

{ TMiTeC_BIOS }

procedure TMiTeC_BIOS.Clear;
begin
  FBIOSData.Clear;
end;

constructor TMiTeC_BIOS.Create(AOwner: TComponent);
begin
  inherited;
  FBIOSData:=TStringList.Create;
end;

destructor TMiTeC_BIOS.Destroy;
begin
  FBIOSData.Free;
  inherited;
end;

function TMiTeC_BIOS.GetCount: Integer;
begin
  Result:=FBIOSData.Count;
end;

function TMiTeC_BIOS.GetNameValue(AName: string): TBIOSValue;
begin
  Result.Name:=AName;
  Result.Value:=ListValue(FBIOSData,AName);
end;

function TMiTeC_BIOS.GetValue;
begin
  Result.Name:=ListName(FBIOSData,AIndex);
  Result.Value:=ListValueFromIndex(FBIOSData,AIndex);
end;

function TMiTeC_BIOS.LoadFromStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
  Clear;
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    Exit;
  OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_READ_INSTORAGE,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    Result:=False;
    try
      Sub:=SS.OpenSubStorage(BIOS_StorageFolderName,STG_READ_INSTORAGE,False);
    except
      Sub:=nil;
    end;

    if Sub<>nil then
    try

      strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          sl:=TStringList.Create;
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            Self.FBIOSName:=ReadStrProperty(sl,'NameString');
            Self.FBIOSExtendedInfo:=ReadStrProperty(sl,'ExtendedInfo');
            Self.FBIOSCopyright:=ReadStrProperty(sl,'Copyright');
            Self.FBIOSDate:=ReadStrProperty(sl,'Date');
            Result:=True;
            SetDataAvail(True);
          finally
            sl.Free;
          end;
        finally
          strm.Free;
        end;

      strm:=Sub.OpenStream(BIOS_DataStream,STG_READ_INSTORAGE,False);
      if strm<>nil then
        try
          LoadFromEncodedStream(strm,FBIOSData,ACodeStream);
        finally
          strm.Free;
        end;
    finally
      if Sub<>nil then
        Sub.Free;
    end;

  finally
    SS.Free;
  end;
end;

procedure TMiTeC_BIOS.RefreshData;
var
  sl: TStringList;
  i: Integer;
  StrData :array[0..255] of char;
const
  cBIOSName = $FE061;
  cBIOSDate = $FFFF5;
  cBIOSExtInfo = $FEC71;
  cBIOSCopyright = $FE091;

  rkBIOS = {HKEY_LOCAL_MACHINE}'\HARDWARE\DESCRIPTION\System';
    rvBiosDate = 'SystemBiosDate';
    rvBiosID = 'Identifier';
    rvBiosVersion = 'SystemBiosVersion';
  rkBIOSEx = {HKEY_LOCAL_MACHINE}'\HARDWARE\DESCRIPTION\System\BIOS';

begin
  inherited;

  FBIOSCopyright:=StringReplace(ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkBIOS,rvBiosVersion,False),#13#10,' ',[rfIgnoreCase,rfReplaceAll]);
  FBIOSName:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkBIOS,rvBiosID,False);
  FBIOSDate:=ReadRegistryValueAsString(HKEY_LOCAL_MACHINE,rkBIOS,rvBiosDate,False);
  FBIOSExtendedInfo:='';

  FBIOSData.Clear;
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_LOCAL_MACHINE;
      if OpenKey(rkBIOSEx,False) then begin
        sl:=TStringList.Create;
        try
          GetValueNames(sl);
          for i:=0 to sl.Count-1 do
            try
              case GetDataType(sl[i]) of
                rdInteger: FBIOSData.Add(Format('%s=%d',[sl[i],ReadInteger(sl[i])]));
                rdString, rdExpandString: FBIOSData.Add(Format('%s=%s',[sl[i],ReadString(sl[i])]));
                else begin
                  ReadBinaryData(sl[i],StrData,255);
                  FBIOSData.Add(Format('%s=%s',[sl[i],StrData]));
                end;
              end;
            except
            end;
        finally
          sl.Free;
        end;
        CloseKey;
      end;
    finally
      Free;
    end;
  SetDataAvail(True);
end;

procedure TMiTeC_BIOS.SaveToStorage;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
begin
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(BIOS_StorageFolderName);
    Sub:=SS.OpenSubStorage(BIOS_StorageFolderName,STG_OPEN,True);

    try
      sl:=TStringList.Create;
      try
        WriteStrProperty(sl,'NameString',Self.NameString);
        WriteStrProperty(sl,'ExtendedInfo',Self.ExtendedInfo);
        WriteStrProperty(sl,'Copyright',Self.Copyright);
        WriteStrProperty(sl,'Date',Self.Date);

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;

        strm:=Sub.OpenStream(BIOS_DataStream,STG_OPEN,True);
        try
          SaveToEncodedStream(FBIOSData,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.



{*******************************************************}
{       MiTeC System Information Component Suite        }
{             WI-FI Detection Part                      }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_WIFI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, MiTeC_SS,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, MiTeC_SS,
     {$ENDIF}
     MiTeC_WLANAPI,
     MSI_Common, MSI_Defs, MiTeC_Routines;

const
  StorageFolderName = 'WiFi';

type
  TWiFiInterface = record
    Name: string;
    GUID: string;
  end;

  TWiFiNetwork = record
    ID: Int64;
    Intf: TWiFiInterface;
    Profile,
    SSID: string;
    SignalQuality: Cardinal;
    AuthAlgorithm: TDOT11_AUTH_ALGORITHM;
    CipherAlgorithm: TDOT11_CIPHER_ALGORITHM;
    PHYType: TDOT11_PHY_TYPE;
    BSSType: TDOT11_BSS_TYPE;
    SecurityEnabled,
    Connectable,
    Connected: Boolean;
    MACAddress: string;
    MaxSpeed,              //Mbps
    ChannelFreq: Cardinal; //kHz
    RSSI: Integer; //dBm
  end;
  TWiFi = array of TWiFiNetwork;

  TMiTeC_WiFi = class(TMiTeC_Component)
  private
    FData: TWiFi;
    FCached: Boolean;
    function GetCount: Integer;
    function GetRecord(AIndex: Integer): TWifiNetwork;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;
    function GetConnectedNetwork: Integer;
    function GetConnectedNetworkName(AIncludeFreq: Boolean = False): string;
    property Networks[AIndex: Integer]: TWifiNetwork read GetRecord;
  published
    property NetworkCount: Integer read GetCount;
    property Cached: Boolean read FCached write FCached default False;
  end;

function AuthToStr(Value :TDOT11_AUTH_ALGORITHM): string;
function CipherToStr(Value :TDOT11_CIPHER_ALGORITHM): string;
function PHYToStr(Value :TDOT11_PHY_TYPE): string;
function BSSToStr(Value :TDOT11_BSS_TYPE): string;
function GetChannelNumber(AChannelFreq: Cardinal): Cardinal;

implementation

uses MiTeC_CRC;

function AuthToStr(Value :TDOT11_AUTH_ALGORITHM): string;
begin
  Result:='';
  case Value of
    DOT11_AUTH_ALGO_80211_OPEN          : Result:='80211 Open';
    DOT11_AUTH_ALGO_80211_SHARED_KEY    : Result:='80211 Shared Key';
    DOT11_AUTH_ALGO_WPA                 : Result:='WPA';
    DOT11_AUTH_ALGO_WPA_PSK             : Result:='WPA-PSK';
    DOT11_AUTH_ALGO_WPA_NONE            : Result:='WPA-NONE';
    DOT11_AUTH_ALGO_RSNA                : Result:='RSNA';
    DOT11_AUTH_ALGO_RSNA_PSK            : Result:='RSNA-PSK';
    DOT11_AUTH_ALGO_IHV_START           : Result:='IHV-START';
    DOT11_AUTH_ALGO_IHV_END             : Result:='IHV-END';
  end;
end;

function CipherToStr(Value :TDOT11_CIPHER_ALGORITHM): string;
begin
  Result:='';
  case Value of
    DOT11_CIPHER_ALGO_NONE          : Result:='None';
    DOT11_CIPHER_ALGO_WEP40         : Result:='WEP40';
    DOT11_CIPHER_ALGO_TKIP          : Result:='TKIP';
    DOT11_CIPHER_ALGO_CCMP          : Result:='CCMP';
    DOT11_CIPHER_ALGO_WEP104        : Result:='WEP104';
    DOT11_CIPHER_ALGO_WPA_USE_GROUP : Result:='WPA Use Group Key / RSN Use Group Key';
    DOT11_CIPHER_ALGO_WEP           : Result:='WEP';
    DOT11_CIPHER_ALGO_IHV_START     : Result:='IHV-START';
    DOT11_CIPHER_ALGO_IHV_END       : Result:='IHV-END';
  end;
end;

function PHYToStr(Value :TDOT11_PHY_TYPE): string;
begin
  Result:='';
  case Value of
    dot11_phy_type_unknown     : Result:='Unknown';
    dot11_phy_type_fhss        : Result:='FHSS';
    dot11_phy_type_dsss        : Result:='DSSS';
    dot11_phy_type_irbaseband  : Result:='IR';
    dot11_phy_type_ofdm        : Result:='802.11a';
    dot11_phy_type_hrdsss      : Result:='High-rate DSSS';
    dot11_phy_type_erp         : Result:='IEEE 802.11g';
    dot11_phy_type_ht          : Result:='IEEE 802.11n';
    dot11_phy_type_vht         : Result:='IEEE 802.11ac';
    dot11_phy_type_IHV_start   : Result:='IHV-START';
    dot11_phy_type_IHV_end     : Result:='IHV-END';
  end;
end;

function BSSToStr(Value :TDOT11_BSS_TYPE): string;
begin
  Result:='';
  case Value of
    dot11_BSS_type_infrastructure  : Result:='Infrastructure';
    dot11_BSS_type_independent     : Result:='Independent';
    dot11_BSS_type_any             : Result:='Any';
  end;
end;

function GetChannelNumber;
const
  cf: array[1..14] of Cardinal = (2412,2417,2422,2427,2432,2437,2442,2447,2452,2457,2462,2467,2472,2484);
var
  i: Integer;
begin
  Result:=0;
  AChannelFreq:=Round(AChannelFreq/1000);
  for i:=14 downto 1 do
    if AChannelFreq>=cf[i] then begin
      Result:=i;
      Break;
    end;
end;

{ TMiTeC_WiFi }

procedure TMiTeC_WiFi.Clear;
begin
  Finalize(FData);
end;

constructor TMiTeC_WiFi.Create(AOwner: TComponent);
begin
  FCached:=False;
  inherited;
end;

destructor TMiTeC_WiFi.Destroy;
begin
  inherited;
end;

function TMiTeC_WiFi.GetConnectedNetwork: Integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(FData) do
    if FData[i].Connected then begin
      Result:=i;
      Break;
    end;
end;

function TMiTeC_WiFi.GetConnectedNetworkName(AIncludeFreq: Boolean = False): string;
var
  idx: integer;
begin
  Result:='';
  idx:=GetConnectedNetwork;
  if idx>-1 then begin
    Result:=FData[idx].SSID;
    if AIncludeFreq then
      Result:=Result+Format(' - %1.2f GHz',[FData[idx].ChannelFreq/1000000]);
  end;
end;

function TMiTeC_WiFi.GetCount: Integer;
begin
  Result:=Length(FData);
end;

function TMiTeC_WiFi.GetRecord(AIndex: Integer): TWifiNetwork;
begin
  Result:=FData[AIndex];
end;

function TMiTeC_WiFi.LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure): boolean;
var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;

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
        SetLength(FData,Length(FData)+1);
        with FData[High(FData)] do begin
          Intf.Name:=ReadStrProperty(sl,'Interface');
          Intf.GUID:=ReadStrProperty(sl,'GUID');
          Profile:=ReadStrProperty(sl,'Profile');
          SSID:=ReadStrProperty(sl,'SSID');
          SignalQuality:=ReadIntProperty(sl,'SignalQuality');
          AuthAlgorithm:=TDOT11_AUTH_ALGORITHM(ReadIntProperty(sl,'Authentication'));
          CipherAlgorithm:=TDOT11_CIPHER_ALGORITHM(ReadIntProperty(sl,'Security'));
          PHYType:=TDOT11_PHY_TYPE(ReadIntProperty(sl,'PHYType'));
          BSSType:=TDOT11_BSS_TYPE(ReadIntProperty(sl,'BSSType'));
          SecurityEnabled:=ReadIntProperty(sl,'SecurityEnabled')=1;
          Connectable:=ReadIntProperty(sl,'Connectable')=1;
          Connected:=ReadIntProperty(sl,'Connected')=1;
          MACAddress:=ReadStrProperty(sl,'MACAddress');
          RSSI:=ReadIntProperty(sl,'RSSI');
          MaxSpeed:=ReadIntProperty(sl,'MaxSpeed');
          ChannelFreq:=ReadIntProperty(sl,'ChannelFreq');
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
  i: integer;
begin
  Clear;
  Result:=inherited LoadFromStorage(AFilename,AReadHeader,ACodeStream);
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

    if Assigned(Sub) then begin
      i:=0;
      while ReadFromStream(i) do
        Inc(i);
      Result:=Result or (i>0);
      SetDataAvail(Result);
    end;
  finally
    if Sub<>nil then
      Sub.Free;
    SS.Free;
  end;
end;

procedure TMiTeC_WiFi.RefreshData(AScanObjects: TScanObjects);
var
  h: THandle;
  v,res,ms: Cardinal;
  r: TWiFiNetwork;
  iil: PWLAN_INTERFACE_INFO_LIST;
  i,j,k: Integer;
  anl: PWLAN_AVAILABLE_NETWORK_LIST;
  IntfGuid: PGUID;
  bssl: PWLAN_BSS_LIST;
  s: string;
  idx,n: Integer;
begin
  inherited;
  if not FCached then
    Clear;

  {$R-}
  res:=WlanOpenHandle(1,nil,@v,@h);
  try
    if res<>ERROR_SUCCESS then
      Exit;
    res:=WlanEnumInterfaces(h,nil,@iil);
    if (res<>ERROR_SUCCESS) or (iil^.dwNumberOfItems=0) then
      Exit;
    try
      for i:=0 to iil^.dwNumberOfItems-1 do begin
        IntfGuid:=@iil^.InterfaceInfo[iil^.dwIndex].InterfaceGuid;
        WlanScan(h,IntfGUID,nil,nil,nil);
        res:=WlanGetAvailableNetworkList(h,IntfGUID,WLAN_AVAILABLE_NETWORK_INCLUDE_ALL_MANUAL_HIDDEN_PROFILES or WLAN_AVAILABLE_NETWORK_INCLUDE_ALL_ADHOC_PROFILES,nil,anl);
        if res=ERROR_SUCCESS then begin
          try
            for j:=0 to anl^.dwNumberOfItems-1 do begin
              ResetMemory(r,SizeOf(r));
              r.Intf.Name:=iil^.InterfaceInfo[i].strInterfaceDescription;
              r.Intf.GUID:=GUIDToString(iil^.InterfaceInfo[i].InterfaceGuid);
              r.Profile:={$IFNDEF UNICODE}WideCharToString{$ENDIF}(anl^.Network[j].strProfileName);
              r.SSID:=string(PAnsiChar(@anl^.Network[j].dot11Ssid.ucSSID));
              r.SignalQuality:=anl^.Network[j].wlanSignalQuality;
              r.AuthAlgorithm:=anl^.Network[j].dot11DefaultAuthAlgorithm;
              r.CipherAlgorithm:=anl^.Network[j].dot11DefaultCipherAlgorithm;
              r.PHYType:=anl^.Network[j].dot11PhyTypes[0];
              r.BSSType:=anl^.Network[j].dot11BssType;
              r.SecurityEnabled:=anl^.Network[j].bSecurityEnabled;
              r.Connectable:=anl^.Network[j].bNetworkConnectable;
              r.Connected:=anl^.Network[j].dwFlags and WLAN_AVAILABLE_NETWORK_CONNECTED = WLAN_AVAILABLE_NETWORK_CONNECTED;

              res:=WlanGetNetworkBssList(h,IntfGUID,
                                         @anl^.Network[j].dot11Ssid,
                                         anl^.Network[j].dot11BssType,
                                         anl^.Network[j].bSecurityEnabled,
                                         nil,bssl);
              if res=ERROR_SUCCESS then
                try
                  r.MACAddress:='';
                  for k:=0 to SizeOf(bssl^.wlanBssEntries[0].dot11Bssid)-1 do
                    r.MACAddress:=r.MACAddress+Format('%2.2x-',[bssl^.wlanBssEntries[0].dot11Bssid[k]]);
                  SetLength(r.MACAddress,Length(r.MACAddress)-1);
                  r.MaxSpeed:=0;
                  n:=bssl^.wlanBssEntries[0].wlanRateSet.uRateSetLength;
                  if n>DOT11_RATE_SET_MAX_LENGTH then
                    n:=DOT11_RATE_SET_MAX_LENGTH;
                  for k:=0 to n-1 do begin
                    ms:=Round((bssl^.wlanBssEntries[0].wlanRateSet.usRateSet[k] and $7FFF)*0.5);
                    if ms>r.MaxSpeed then
                      r.MaxSpeed:=ms;
                  end;
                  r.ChannelFreq:=bssl^.wlanBssEntries[0].ulChCenterFrequency;
                  r.RSSI:=bssl^.wlanBssEntries[0].lRssi;
                  //r.ID:=bssl^.wlanBssEntries[0].uPhyId;
                finally
                  WlanFreeMemory(bssl);
                end;

              s:=r.SSID+'_'+r.MACAddress;
              r.ID:=CRC64(s);

              //if r.ChannelFreq<3000000 then begin
                idx:=-1;
                if FCached then begin
                  for k:=0 to High(FData) do begin
                    if (r.ID=FData[i].ID) and (idx=-1) then
                      idx:=i;
                    FData[i].SignalQuality:=0;
                  end;
                end;

                if idx=-1 then begin
                  SetLength(FData,Length(FData)+1);
                  idx:=High(FData);
                end;
                FData[idx]:=r;
              //end;
            end;
          finally
            WlanFreeMemory(anl);
          end;
        end;
      end;
    finally
      WlanFreeMemory(iil);
    end;
    {$R+}
  finally
    WlanCloseHandle(h,nil);
  end;

  SetDataAvail(True);
end;

procedure TMiTeC_WiFi.SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil);
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

      WriteStrProperty(sl,'Interface',Self.Networks[AIndex].Intf.Name);
      WriteStrProperty(sl,'GUID',Self.Networks[AIndex].Intf.GUID);
      WriteStrProperty(sl,'Profile',Self.Networks[AIndex].Profile);
      WriteStrProperty(sl,'SSID',Self.Networks[AIndex].SSID);
      WriteIntProperty(sl,'SignalQuality',Self.Networks[AIndex].SignalQuality);
      WriteIntProperty(sl,'Authentication',Integer(Self.Networks[AIndex].AuthAlgorithm));
      WriteIntProperty(sl,'Security',Integer(Self.Networks[AIndex].CipherAlgorithm));
      WriteIntProperty(sl,'PHYType',Integer(Self.Networks[AIndex].PHYType));
      WriteIntProperty(sl,'BSSType',Integer(Self.Networks[AIndex].BSSType));
      WriteIntProperty(sl,'SecurityEnabled',Integer(Self.Networks[AIndex].SecurityEnabled));
      WriteIntProperty(sl,'Connectable',Integer(Self.Networks[AIndex].Connectable));
      WriteIntProperty(sl,'Connected',Integer(Self.Networks[AIndex].Connected));
      WriteStrProperty(sl,'MACAddress',Self.Networks[AIndex].MACAddress);
      WriteIntProperty(sl,'MaxSpeed',Self.Networks[AIndex].MaxSpeed);
      WriteIntProperty(sl,'ChannelFreq',Self.Networks[AIndex].ChannelFreq);
      WriteIntProperty(sl,'RSSI',Self.Networks[AIndex].RSSI);
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
      for i:=0 to Self.NetworkCount-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.


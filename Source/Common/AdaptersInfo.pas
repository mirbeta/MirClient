unit AdaptersInfo;

interface

uses
  Windows, SysUtils, M2Share;

const
  MAX_HOSTNAME_LEN          = 128;
  MAX_DOMAIN_NAME_LEN       = 128;
  MAX_SCOPE_ID_LEN          = 256;
  MAX_ADAPTER_NAME_LENGTH   = 256;
  MAX_ADAPTER_DESCRIPTION_LENGTH = 128;
  MAX_ADAPTER_ADDRESS_LENGTH = 8;

type
  TIPAddressString = array[0..4 * 4 - 1] of Char;

  pIPAddrString = ^TIPAddrString;
  TIPAddrString = record
    Next: pIPAddrString;
    IPAddress: TIPAddressString;
    IPMask: TIPAddressString;
    Context: Integer;
  end;

  pFixedInfo = ^TFixedInfo;
  TFixedInfo = record                   { FIXED_INFO }
    HostName: array[0..MAX_HOSTNAME_LEN + 3] of Char;
    DomainName: array[0..MAX_DOMAIN_NAME_LEN + 3] of Char;
    CurrentDNSServer: pIPAddrString;
    DNSServerList: TIPAddrString;
    NodeType: Integer;
    ScopeId: array[0..MAX_SCOPE_ID_LEN + 3] of Char;
    EnableRouting: Integer;
    EnableProxy: Integer;
    EnableDNS: Integer;
  end;

  pIPAdapterInfo = ^TIPAdapterInfo;
  TIPAdapterInfo = record               { IP_ADAPTER_INFO }
    Next: pIPAdapterInfo;
    ComboIndex: Integer;
    AdapterName: array[0..MAX_ADAPTER_NAME_LENGTH + 3] of Char;
    Description: array[0..MAX_ADAPTER_DESCRIPTION_LENGTH + 3] of Char;
    AddressLength: Integer;
    Address: array[1..MAX_ADAPTER_ADDRESS_LENGTH] of byte;
    Index: Integer;
    _Type: Integer;
    DHCPEnabled: Integer;
    CurrentIPAddress: pIPAddrString;
    IPAddressList: TIPAddrString;
    GatewayList: TIPAddrString;
    DHCPServer: TIPAddrString;
    HaveWINS: BOOL;
    PrimaryWINSServer: TIPAddrString;
    SecondaryWINSServer: TIPAddrString;
    LeaseObtained: Integer;
    LeaseExpires: Integer;
  end;

function GetAdaptersInfo(AI: pIPAdapterInfo; var BufLen: Integer): Integer; stdcall; external 'iphlpapi.dll' Name 'GetAdaptersInfo';

function GetWanIP: string;              //获取外网IP

implementation

function GetWanIP: string;              //获取外网IP
var
  pAI, pWork                : pIPAdapterInfo;
  nSize                     : Integer;
  nRes                      : Integer;
  pIPAddr                   : pIPAddrString;

  i, p                      : Integer;
  N1, n2, N3, n4            : byte;
  s1, s2, s3, s4, IP        : string;
  int                       : UINT;
  ints1, inte1              : UINT;
  ints2, inte2              : UINT;
  ints3, inte3              : UINT;
  ints4, inte4              : UINT;
begin
  Result := '';
  nSize := 5120;
  GetMem(pAI, nSize);
  nRes := GetAdaptersInfo(pAI, nSize);
  if (nRes <> ERROR_SUCCESS) then Exit;
  pWork := pAI;
  //Result := GetAddrString(@pWork^.IPAddressList);
  pIPAddr := @pWork^.IPAddressList;
  while (pIPAddr <> nil) do begin
    Result := pIPAddr^.IPAddress;
    pIPAddr := pIPAddr^.Next;
  end;
  FreeMem(pAI);
end;

end.


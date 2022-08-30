unit ipinfo_dll;

interface

uses
  Windows, SysUtils;

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

implementation

end.


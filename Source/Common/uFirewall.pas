unit uFirewall;

interface

procedure AddApplicationToFirewall(const AEntryName, AFileName: string);
procedure AddPortToFirewall(const AEntryName: string; APortNumber: Cardinal);

implementation

uses SysUtils, Variants, ComObj;

const
  NET_FW_PROFILE_DOMAIN = 0;
  NET_FW_PROFILE_STANDARD = 1;
  NET_FW_IP_VERSION_ANY = 2;
  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_SCOPE_ALL = 0;
  NET_FW_SCOPE_LOCAL_SUBNET = 1;

procedure AddApplicationToFirewall(const AEntryName, AFileName: string);
var
  AMgr, AApplication: OleVariant;
begin
  try
    AMgr := CreateOleObject('HNetCfg.FwMgr');
    AApplication := CreateOleObject('HNetCfg.FwAuthorizedApplication');
    try
      AApplication.ProcessImageFileName := AFileName;
      AApplication.Name := AEntryName;
      AApplication.Scope := NET_FW_SCOPE_ALL;
      AApplication.IpVersion := NET_FW_IP_VERSION_ANY;
      AApplication.Enabled := True;
      AMgr.LocalPolicy.CurrentProfile.AuthorizedApplications.Remove(AFileName);
      AMgr.LocalPolicy.CurrentProfile.AuthorizedApplications.Add(AApplication);
    finally
      AMgr := UnAssigned;
      AApplication := UnAssigned;
    end;
  except
  end;
end;

procedure AddPortToFirewall(const AEntryName: string; APortNumber: Cardinal);
var
  AMgr, APort: OleVariant;
begin
  try
    AMgr := CreateOLEObject('HNetCfg.FwMgr');
    APort := CreateOLEObject('HNetCfg.FWOpenPort');
    try
      APort.Name := AEntryName;
      APort.Protocol := NET_FW_IP_PROTOCOL_TCP;
      APort.Port := APortNumber;
      APort.Scope := NET_FW_SCOPE_ALL;
      APort.Enabled := True;
      AMgr.LocalPolicy.CurrentProfile.GloballyOpenPorts.Add(APort);
    finally
      AMgr := UnAssigned;
      APort := UnAssigned;
    end;
  except
  end;
end;

end.

{*******************************************************}
{       MiTeC System Information Component Suite        }
{         Windows Firewall Detection Part               }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_FW;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes,
     WinAPI.ActiveX, System.Win.ComObj, System.Variants,
     {$ELSE}
     Windows, SysUtils, Classes, ActiveX, ComObj, Variants,
     {$ENDIF}
     MiTeC_SS, MSI_Common, MSI_Defs;

const
  StorageFolderName = 'Firewall';

  NET_FW_PROFILE2_DOMAIN  = 1;
  NET_FW_PROFILE2_PRIVATE = 2;
  NET_FW_PROFILE2_PUBLIC  = 4;

  NET_FW_IP_PROTOCOL_TCP = 6;
  NET_FW_IP_PROTOCOL_UDP = 17;
  NET_FW_IP_PROTOCOL_ICMPv4 = 1;
  NET_FW_IP_PROTOCOL_ICMPv6 = 58;

  NET_FW_RULE_DIR_IN = 1;
  NET_FW_RULE_DIR_OUT = 2;

  NET_FW_ACTION_BLOCK = 0;
  NET_FW_ACTION_ALLOW = 1;

type
  TRule = record
    Name: string;
    Description: string;
    AppName: string;
    ServiceName: string;
    Protocol: Cardinal;
    LocalPorts: string;
    RemotePorts: string;
    LocalAddresses: string;
    RemoteAddresses: string;
    ICMP: string;
    Direction: Cardinal;
    Enabled: boolean;
    Edge: boolean;
    Action: Cardinal;
    Grouping: string;
    IntfTypes: string;
  end;

  TRules = array of TRule;

  TMiTeC_Firewall= class(TMiTeC_Component)
  private
    FRules: TRules;
    FPubProf: boolean;
    FPrivProf: boolean;
    FDomProf: boolean;
    function GetRule(Index: integer): TRule;
    function GetRuleCount: Integer;
  public
    destructor Destroy; override;
    procedure Clear; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    function IsRuleEnabled(const AName: string): Boolean;
    procedure EnableRule(const AName: string; AValue: Boolean = True);
    procedure AddRule(ARecord: TRule);
    procedure RemoveRule(const AName: string);

    property Rules[Index: integer]: TRule read GetRule;
  published
    property RuleCount: Integer read GetRuleCount;
    property PrivateProfile: boolean read FPrivProf;
    property PublicProfile: boolean read FPubProf;
    property DomainProfile: boolean read FDomProf;
  end;

implementation

uses MiTeC_Routines;

{ TMiTeC_Firewall }

procedure TMiTeC_Firewall.AddRule(ARecord: TRule);
var
  fwPolicy2: OleVariant;
  Rules: OleVariant;
  rule: OleVariant;
begin
  CoInitialize(nil);
  try
    fwPolicy2:=CreateOleObject('HNetCfg.FwPolicy2');
    Rules:=fwPolicy2.Rules;
    rule:=CreateOleObject('HNetCfg.FWRule');
    rule.Name:=ARecord.Name;
    rule.Description:=rule.Description;
    rule.ApplicationName:=ARecord.AppName;
    rule.ServiceName:=ARecord.ServiceName;
    rule.Protocol:=ARecord.Protocol;
    if (ARecord.Protocol=NET_FW_IP_PROTOCOL_TCP) or (ARecord.Protocol=NET_FW_IP_PROTOCOL_UDP) then begin
      rule.LocalPorts:=ARecord.LocalPorts;
      rule.RemotePorts:=ARecord.RemotePorts;
      rule.LocalAddresses:=ARecord.LocalAddresses;
      rule.RemoteAddresses:=ARecord.RemoteAddresses;
    end;
    if (ARecord.Protocol=NET_FW_IP_PROTOCOL_ICMPv4) or (ARecord.Protocol=NET_FW_IP_PROTOCOL_ICMPv6) then
      rule.IcmpTypesAndCodes:=ARecord.ICMP;
    rule.Direction:=ARecord.Direction;
    rule.Enabled:=ARecord.Enabled;
    rule.EdgeTraversal:=ARecord.Edge;
    rule.Action:=ARecord.Action;
    rule.Grouping:=ARecord.Grouping;
    rule.InterfaceTypes:=ARecord.IntfTypes;
    Rules.Add(rule);
  finally
    CoUninitialize;
  end;
end;

procedure TMiTeC_Firewall.Clear;
begin
  FPrivProf:=False;
  FPubProf:=False;
  FDomProf:=False;
  Finalize(FRules);
end;

destructor TMiTeC_Firewall.Destroy;
begin
  Finalize(FRules);
  inherited;
end;

procedure TMiTeC_Firewall.EnableRule(const AName: string; AValue: Boolean);
var
 cp: Integer;
 fwPolicy2: OleVariant;
 Rules: OleVariant;
begin
  CoInitialize(nil);
  try
    fwPolicy2:=CreateOleObject('HNetCfg.FwPolicy2');
    Rules:=fwPolicy2.Rules;
    cp:=fwPolicy2.CurrentProfileTypes;
    fwPolicy2.EnableRuleGroup(cp,AName,AValue);
  finally
    CoUninitialize;
  end;
end;

procedure TMiTeC_Firewall.RefreshData;
var
 cp: Integer;
 fwPolicy2: OleVariant;
 Rules: OleVariant;
 rule: OleVariant;
 enum: IEnumvariant;
 v: Cardinal;
 r: TRule;
begin
  inherited;

  Clear;

  CoInitialize(nil);
  try
    try
      fwPolicy2:=CreateOleObject('HNetCfg.FwPolicy2');
    except
      Exit;
    end;
    Rules:=fwPolicy2.Rules;

    try
      cp:=fwPolicy2.CurrentProfileTypes;
      enum:=IUnknown(Rules._NewEnum) as IEnumVariant;
    except
      Exit;
    end;

    FPrivProf:=(cp and NET_FW_PROFILE2_PRIVATE)<>0;
      FPubProf:=(cp and NET_FW_PROFILE2_PUBLIC)<>0;
      FDomProf:=(cp and NET_FW_PROFILE2_DOMAIN)<>0;

    while enum.Next(1,rule,v)=0 do begin
      if (rule.Profiles and cp)<>0 then begin
        ResetMemory(r,sizeof(r));
        r.Name:=rule.Name;
        r.Description:=rule.Description;
        r.AppName:=rule.ApplicationName;
        r.ServiceName:=rule.ServiceName;
        r.Protocol:=rule.Protocol;
        if (rule.Protocol=NET_FW_IP_PROTOCOL_TCP) or (rule.Protocol=NET_FW_IP_PROTOCOL_UDP) then begin
          r.LocalPorts:=rule.LocalPorts;
          r.RemotePorts:=rule.RemotePorts;
          r.LocalAddresses:=rule.LocalAddresses;
          r.RemoteAddresses:=rule.RemoteAddresses;
        end;
        if (rule.Protocol=NET_FW_IP_PROTOCOL_ICMPv4) or (rule.Protocol=NET_FW_IP_PROTOCOL_ICMPv6) then
          r.ICMP:=rule.IcmpTypesAndCodes;
        r.Direction:=rule.Direction;
        r.Enabled:=rule.Enabled;
        r.Edge:=rule.EdgeTraversal;
        r.Action:=rule.Action;
        r.Grouping:=rule.Grouping;
        r.IntfTypes:=rule.InterfaceTypes;
        SetLength(Frules,Length(FRules)+1);
        FRules[High(FRules)]:=r;
      end;
      rule:=Unassigned;
    end;
  finally
    CoUninitialize;
  end;
  SetDataAvail(True);
end;

procedure TMiTeC_Firewall.RemoveRule(const AName: string);
var
  fwPolicy2: OleVariant;
  Rules: OleVariant;
begin
  CoInitialize(nil);
  try
    fwPolicy2:=CreateOleObject('HNetCfg.FwPolicy2');
    Rules:=fwPolicy2.Rules;
    Rules.Remove(AName);
  finally
    CoUninitialize;
  end;
end;

function TMiTeC_Firewall.GetRule(Index: integer): TRule;
begin
  Result:=FRules[Index];
end;

function TMiTeC_Firewall.GetRuleCount: Integer;
begin
  Result:=Length(FRules);
end;

function TMiTeC_Firewall.IsRuleEnabled(const AName: string): Boolean;
var
 fwPolicy2: OleVariant;
begin
  CoInitialize(nil);
  try
    fwPolicy2:=CreateOleObject('HNetCfg.FwPolicy2');
    Result:=fwPolicy2.IsRuleGroupCurrentlyEnabled(AName);
  finally
    CoUninitialize;
  end;
end;

function TMiTeC_Firewall.LoadFromStorage;
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
            SetLength(FRules,Length(FRules)+1);
            with FRules[High(FRules)] do begin
              Name:=ReadStrProperty(sl,'Name');
              Description:=ReadStrProperty(sl,'Description');
              AppName:=ReadStrProperty(sl,'AppName');
              ServiceName:=ReadStrProperty(sl,'ServiceName');
              Protocol:=ReadIntProperty(sl,'Protocol');
              LocalPorts:=ReadStrProperty(sl,'LocalPorts');
              RemotePorts:=ReadStrProperty(sl,'RemotePorts');
              LocalAddresses:=ReadStrProperty(sl,'LocalAddresses');
              RemoteAddresses:=ReadStrProperty(sl,'RemoteAddresses');
              ICMP:=ReadStrProperty(sl,'ICMP');
              Direction:=ReadIntProperty(sl,'Direction');
              Enabled:=ReadIntProperty(sl,'Enabled')=1;
              Edge:=ReadIntProperty(sl,'Edge')=1;
              Action:=ReadIntProperty(sl,'Action');
              Grouping:=ReadStrProperty(sl,'Grouping');
              IntfTypes:=ReadStrProperty(sl,'IntfTypes');
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
  strm: TStorageStream;
  sl: TStringList;
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
        strm:=Sub.OpenStream(strm_Props,STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            sl:=TStringList.Create;
            try
              LoadFromEncodedStream(strm,sl,ACodeStream);
              Self.FPrivProf:=ReadIntProperty(sl,'PrivateProfile')=1;
              Self.FPubProf:=ReadIntProperty(sl,'PublicProfile')=1;
              Self.FDomProf:=ReadIntProperty(sl,'DomainProfile')=1;
              SetDataAvail(True);
            finally
              sl.Free;
            end;
          finally
            strm.Free;
          end;

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

procedure TMiTeC_Firewall.SaveToStorage;
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
    WriteStrProperty(sl,'Name',Self.Rules[AIndex].Name);
    WriteStrProperty(sl,'Description',Self.Rules[AIndex].Description);
    WriteStrProperty(sl,'AppName',Self.Rules[AIndex].AppName);
    WriteStrProperty(sl,'ServiceName',Self.Rules[AIndex].ServiceName);
    WriteIntProperty(sl,'Protocol',Self.Rules[AIndex].Protocol);
    WriteStrProperty(sl,'LocalPorts',Self.Rules[AIndex].LocalPorts);
    WriteStrProperty(sl,'RemotePorts',Self.Rules[AIndex].RemotePorts);
    WriteStrProperty(sl,'LocalAddresses',Self.Rules[AIndex].LocalAddresses);
    WriteStrProperty(sl,'RemoteAddresses',Self.Rules[AIndex].RemoteAddresses);
    WriteStrProperty(sl,'ICMP',Self.Rules[AIndex].ICMP);
    WriteIntProperty(sl,'Direction',Self.Rules[AIndex].Direction);
    WriteIntProperty(sl,'Enabled',integer(Self.Rules[AIndex].Enabled));
    WriteIntProperty(sl,'Edge',integer(Self.Rules[AIndex].Edge));
    WriteIntProperty(sl,'Action',Self.Rules[AIndex].Action);
    WriteStrProperty(sl,'Grouping',Self.Rules[AIndex].Grouping);
    WriteStrProperty(sl,'IntfTypes',Self.Rules[AIndex].IntfTypes);
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
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    try
      sl:=TStringList.Create;
      try
        WriteIntProperty(sl,'PrivateProfile',integer(Self.FPrivProf));
        WriteIntProperty(sl,'PublicProfile',integer(Self.FPubProf));
        WriteIntProperty(sl,'DomainProfile',integer(Self.FDomProf));

        strm:=Sub.OpenStream(strm_Props,STG_OPEN,True);
        try
          SaveToEncodedStream(sl,strm,ACodeStream);
        finally
          strm.Free;
        end;
      finally
        sl.Free;
      end;

      for i:=0 to Self.RuleCount-1 do
        WriteToStream(i);
    finally
      Sub.Free;
    end;
  finally
    SS.Free;
  end;
end;

end.

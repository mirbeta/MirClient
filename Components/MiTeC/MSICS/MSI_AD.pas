{*******************************************************}
{       MiTeC System Information Component Suite        }
{          Active Directory Detection Part              }
{                version 13.4.0                         }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MSI_AD;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, System.Variants,
     WinAPI.ActiveX, System.Win.ComObj,
     {$ELSE}
     Windows, SysUtils, Classes, Variants, ActiveX, ComObj,
     {$ENDIF}
     MiTeC_SS,
     {$IFNDEF UNICODE} MiTeC_WideStrings,{$ENDIF}
     MSI_Common, MSI_Defs, {$IFDEF FPC}MiTeC_FPC_ActiveDs_TLB{$ELSE}MiTeC_ActiveDs_TLB{$ENDIF};

const
  StorageFolderName = 'ActiveDirectory';

  PASSWORD_ATTR_NONE = 0;
  PASSWORD_ATTR_MIXED_CASE = 1;
  PASSWORD_ATTR_COMPLEX = 2;

type
  TADProvider = (adLDAP, adWinNT, adNDS);

  TADObjectType = (adCommon,adDomain,adUser,adComputer,adGroup);

const
   ADClassNames: array[TADObjectType] of string = ('*','domain','user','computer','group');

type
  TADProperty = record
    Provider: TADProvider;
    Name: string;
    Typ: Integer;
    Value: Variant;
  end;

  TADObject = record
    ObjectType: TADObjectType;
    Props: array of TADProperty;
    {$IFDEF BDS3PLUS}
    function GetProp(AName: string): TADProperty;
    function GetPropIdx(AName: string): integer;
    function GetName: WideString;
    function GetDN: WideString;
    function GetIP: WideString;
    function GetPropAsDelimitedText(AName: string): string;
    procedure ClearProps;
    procedure AddProp(AName: string; ATyp: Integer; AValue: Variant);
    {$ENDIF}
  end;

  TADObjects = array of TADObject;

  TMiTeC_AD = class(TMiTeC_Component)
  private
    FADDB: string;
    FPwd: string;
    FUser: string;
    FUsers,
    FGroups,
    FComps: TADObjects;
    FDomain: TADObject;
    FSchema: IDirectorySearch;

    procedure ClearObjects(var AObjects: TADObjects);
    //procedure EnumObjects(AADSPath,AUser,APwd: WideString; AObject: variant; AObjectType: TADObjectType; var AObjects: TADObjects);
    //procedure EnumProps(AObj: IADs; var AResult: TADObject);

    function GetDomainProps(AADSPath,AUser,APwd: WideString): boolean;

    function GetComputer(Index: Integer): TADObject;
    function GetComputerCount: Cardinal;
    function GetUser(Index: Integer): TADObject;
    function GetUserCount: Cardinal;
    function GetGroup(Index: Integer): TADObject;
    function GetGroupCount: Cardinal;
    function GetUserByName(AName: string): TADObject;
    function GetGroupByName(AName: string): TADObject;
    function GetComputerByName(AName: string): TADObject;
    function GetComputerByIP(AIP: string): TADObject;

    procedure GetComputerExtraInfo(AIndex: Integer); overload;
    procedure GetUserExtraInfo(AIndex: Integer); overload;
    procedure GetGroupExtraInfo(AIndex: Integer); overload;

    procedure GetComputerExtraInfo(AName: WideString; var Result: TADObject); overload;
    function GetUserExtraInfo(AName: WideString; var AUser: TADObject): boolean; overload;
    procedure GetGroupExtraInfo(AName: WideString; var Result: TADObject); overload;
  public
    procedure Clear; override;

    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure RefreshData(AScanObjects: TScanObjects = soAll); override;
    procedure RefreshGroups;
    procedure RefreshComputers;
    procedure RefreshUsers;
    procedure RefreshDomain;
    procedure SaveToStorage(const AFilename: string; var AWriteHeader: Boolean; AFormat: integer = 0; const AComment: string=''; ACodeStream: TCodeStreamProcedure = nil); override;
    function LoadFromStorage(const AFilename: string; var AReadHeader: boolean; ACodeStream: TCodeStreamProcedure = nil): boolean; override;

    procedure GetObjectAttributeNames(AADSPath,AUser,APwd,AClassname: WideString; AList: {$IFDEF UNICODE}TStringlist{$ELSE}TWideStrings{$ENDIF});
    procedure SearchDirectory(AADSPath,AUser,APwd: Widestring; AType: TADObjectType; var AObjects: TADObjects);

    property UserCount: Cardinal read GetUserCount;
    property Users[Index: Integer]: TADObject read GetUser;
    property UsersByName[AName: string]: TADObject read GetUserByName;

    property GroupCount: Cardinal read GetGroupCount;
    property Groups[Index: Integer]: TADObject read GetGroup;
    property GroupsByName[AName: string]: TADObject read GetGroupByName;

    property ComputerCount: Cardinal read GetComputerCount;
    property Computers[Index: Integer]: TADObject read GetComputer;
    property ComputersByName[AName: string]: TADObject read GetComputerByName;
    property ComputersByIP[AIP: string]: TADObject read GetComputerByIP;

    property Domain: TADObject read FDomain;
  published
    property ADDatabase: string read FADDB write FADDB;
    property User: string read FUser write FUser;
    property Password: string read FPwd write FPwd;
  end;

  function GetDNSDomain: string;
  function GetShortDomain: string;
  function GetPDC: string;

  function GetComputerInfo(AADSPath,AUser,APwd,AName: string; var Desc,OS: string): Boolean;

  function GetDN(AObj: TADObject): WideString;
  function GetName(AObj: TADObject): WideString;
  function GetProp(AObj: TADObject; AName: string): TADProperty;
  function GetPropAsDelimitedText(AObj: TADObject; AName: string): string;

  function PwdAttrToStr(APA: Cardinal): string;
  function GetDNPart(DN,PartName: string): string;
  function CreatePathFromDN(ADN: string): string;
  function TranslateDNSName(ADNS: string): string;

var
  AD_LastError: string;

procedure SetADLastError(AText: string = '');

implementation

uses
  MiTeC_ADSI, MiTeC_Routines, MiTeC_Datetime, MiTeC_NetAPI32, MiTeC_StrUtils;

procedure SetADLastError;
begin
  if AText='' then
    AD_LastError:=''
  else begin
    if AD_LastError<>'' then
      AD_LastError:=AD_LastError+#13#10;
    AD_LastError:=AD_LastError+AText;
  end;
end;

procedure AddProp(var AObj: TADObject; AName: string; ATyp: Integer; AValue: Variant);
var
  idx: Integer;
begin
  with AObj do begin
    SetLength(Props,Length(Props)+1);
    idx:=High(Props);
    with Props[idx] do begin
      Name:=Aname;
      Typ:=ATyp;
      Value:=AValue;
    end;
  end;
end;

procedure ClearProps(var AObj: TADObject);
begin
  Finalize(AObj.Props);
end;

function GetProp(AObj: TADObject; AName: string): TADProperty;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  with AObj do
    for i:=0 to High(Props) do
      if SameText(Props[i].Name,AName) then begin
        Result:=Props[i];
        Break;
      end;
end;

function GetDN(AObj: TADObject): WideString;
begin
  Result:=GetProp(AObj,'distinguishedName').Value;
end;

function GetName(AObj: TADObject): WideString;
begin
  Result:=GetProp(AObj,'cn').Value;
  if Result='' then
    Result:=GetProp(AObj,'name').Value;
end;

function GetIP(AObj: TADObject): WideString;
const
  ip: WideString = 'IP:';
var
  s: widestring;
  v: Variant;
  i: Integer;
begin
  v:=GetProp(AObj,'NetAddresses').Value;
  if VarIsArray(v) then begin
    for i:=0 to VarArrayHighBound(v,0) do begin
      s:=VarToStr(v[i]);
      if s<>'' then begin
        if Pos(ip,s)=1 then begin
          s:=Copy(s,4,255);
          Break;
        end else
          s:='';
      end;
    end;
  end else begin
    s:=VarToStr(v);
    if s<>'' then begin
      if Pos(ip,s)=1 then
        s:=Copy(s,4,255)
      else
        s:='';
    end;
  end;
  Result:=s;
end;

function GetPropAsDelimitedText(AObj: TADObject; AName: string): string;
var
  i: Integer;
begin
  Result:='';
  with AObj do
    for i:=0 to High(Props) do
      if SameText(Props[i].Name,AName) then
        Result:=Result+Props[i].Value+';';
  SetLength(Result,Length(Result)-1);
end;

function GetPropIdx(AObj: TADObject; AName: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  with AObj do
    for i:=0 to High(Props) do
      if SameText(Props[i].Name,AName) then begin
        Result:=i;
        Break;
      end;
end;

function PwdAttrToStr;
begin
  Result:='';
  case APA of
    PASSWORD_ATTR_NONE: Result:='None';
    PASSWORD_ATTR_MIXED_CASE: Result:='Mixed case';
    PASSWORD_ATTR_COMPLEX: Result:='Complex';
  end;
end;

function GetDNPart;
var
  sl: TStringList;
  i: Integer;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    SetDelimitedText(DN,',',sl);
    if sl.Count=0 then
      Exit;
    if SameText(PartName,'cn') then begin
      if SameText(sl.Names[0],'CN') then
        Result:=sl.Values[sl.Names[0]];
    end else begin
      for i:=0 to sl.Count-1 do
        if SameText(PartName,sl.Names[i]) then begin
          Result:=Result+ListValueFromIndex(sl,i);
          if SameText(PartName,'DC') then
            Result:=Result+'.'
          else
            Result:=Result+'\';
        end;
      SetLength(Result,Length(Result)-1);
    end;
  finally
    sl.Free;
  end;
end;

function CreatePathFromDN(ADN: string): string;
var
  sl: TStringList;
  s,v: string;
  i: Integer;
begin
  Result:='';
  s:='';
  sl:=TStringList.Create;
  {$IFDEF BDS3PLUS}
  sl.Delimiter:=',';
  sl.StrictDelimiter:=True;
  {$ENDIF}
  try
    {$IFDEF D7PLUS}
    sl.DelimitedText:=ADN;
    {$ELSE}
    SetDelimitedText(ADN,',',sl);
    {$ENDIF}
    for i:=sl.Count-1 downto 0 do
      if SameText(sl.Names[i],'DC') then
        s:=ListValueFromIndex(sl,i)+'.'+s
      else begin
        v:=ListValueFromIndex(sl,i);
        Result:=Result+IncludeTrailingPathDelimiter(StringReplace(v,'\','/',[rfReplaceAll,rfIgnoreCase]));
      end;
    SetLength(s,Length(s)-1);
    Result:=IncludeTrailingPathDelimiter(s)+Result;
  finally
    sl.Free;
  end;
end;

function TranslateDNSName;
var
  i: Integer;
  sl: TStringList;
begin
  Result:='';
  sl:=TStringlist.Create;
  with sl do
    try
      {$IFDEF BDS3PLUS}
      Delimiter:='.';
      StrictDelimiter:=True;
      DelimitedText:=ADNS;
      {$ELSE}
      SetDelimitedText(ADNS,',',sl);
      {$ENDIF}
      for i:=0 to Count-1 do
        Result:=Result+'DC='+Strings[i]+',';
      SetLength(Result,Length(Result)-1);
    finally
      Free;
    end;
end;

function GetGUIDString(AValue: IADsPropertyValue): WideString;
var
  i: Integer;
  v: OleVariant;
  p: Pointer;
  GUIDArray : array[1..16] of byte;
begin
  Result:='';
  v:=AValue.Get_OctetString;
  p:=VarArrayLock(v);
  Move(p^,GUIDArray,SizeOf(GUIDArray));
  VarArrayUnlock(v);
  for i:=1 to 16 do
    Result:=Result+IntToHex(GUIDArray[i],2);
end;

function GetLargeInteger(AValue: IADsPropertyValue): Int64;
var
  ILargeInt: IADsLargeInteger;
begin
  ILargeInt:=(AValue.Get_LargeInteger as IADsLargeInteger);
  Result:=ILargeInt.HighPart;
  Result:=(Result shl 32)+ILargeInt.LowPart;
  ILargeInt:=nil;
end;

function GetSIDString(AValue: IADsPropertyValue): WideString; overload;
var
  v: OleVariant;
  SID: PSID;
begin
  v:=AValue.OctetString;
  SID:=VarArrayLock(v);
  Result:=ConvertSIDToString(SID);
  VarArrayUnlock(v);
end;

function GetSIDString(AValue: PADSVALUE): WideString; overload;
var
  SID: PSID;
  a: TByteArray;
  g: ADS_OCTET_STRING;
begin
  g:=Avalue.__MIDL_0010.OctetString;
  Zeromemory(@a,SizeOf(a));
  CopyMemory(@a,@g,Sizeof(TGUID));
  SID:=PSID(@a);
  Result:=ConvertSIDToString(SID);
end;

function DN2Path(ASource: string): string;
var
  sl: TStringList;
  i: Integer;
begin
  Result:='';
  sl:=TStringList.Create;
  try
    sl.CommaText:=ASource;
    for i:=0 to sl.Count-1 do
      if SameText(sl.Names[i],'DC') then
        Result:=Result+ListValueFromIndex(sl,i)+'.';
    SetLength(Result,Length(Result)-1);
    for i:=sl.Count-1 downto 0 do
      if SameText(sl.Names[i],'OU') then
        Result:=Result+'\'+ListValueFromIndex(sl,i)
      else if SameText(sl.Names[i],'CN') then
        Result:=Result+'\'+ListValueFromIndex(sl,i);
    if Result[1]='\' then
      Result:=Copy(Result,2,Length(Result));
  finally
    sl.Free;
  end;
end;

function GetDNSDomain: string;
begin
  Result:='';
  {$IFDEF FPC}
  {$ELSE}
  with TADSystemInfo.Create(nil) do
    try
      Connect;
      try
        Result:=DomainDNSName;
        if Result='' then
          Result:=DomainDNSName;
      except on e:Exception do SetADLastError('GetDNSDomain: '+e.Message) end;
      Disconnect;
    finally
      Free;
    end;
  {$ENDIF}
end;

function GetPDC: string;
var
  p: Pointer;
  r: Integer;
begin
  Result:='';
  r:=NetGetDCName(nil,nil,p);
  try
    if r=0 then
      Result:=WideCharToString(PWideChar(p));
  finally
    NetApiBufferFree(p);
  end;
  if Result='' then
    try
      {$IFDEF FPC}

      {$ELSE}
      with TWinNTSystemInfo.Create(nil) do
        try
          try
            Connect;
            Result:=PDC;
          except on e:Exception do SetADLastError('GetPDC: '+e.Message) end;
          Disconnect;
        finally
          Free;
        end;
        {$ENDIF}
    except
    end;
end;

function GetShortDomain: string;
begin
  Result:='';
  {$IFDEF FPC}
  {$ELSE}
  with TADSystemInfo.Create(nil) do
    try
      Connect;
      try
        Result:=DomainShortName;
        if Result='' then
          Result:=DomainShortName;
      except on e:Exception do SetADLastError('GetShortDomain: '+e.Message) end;
      Disconnect;
    finally
      Free;
    end;
  {$ENDIF}
end;

function GetComputerInfo;
var
  ads: IDirectorySearch;
  rh: {$IFDEF FPC}Pointer{$ELSE}THandle{$ENDIF};
  adc: ads_search_column;
  hr: HResult;
  p: array[0..2] of PWideChar;
  adp: array[0..0] of ads_searchpref_info;
  wc: WideString;
  {$IFDEF FPC}pp: PWideChar;{$ENDIF}
begin
  Result:=False;
  Desc:='';
  oS:='';
  if (AUser='') or (APwd='') then
    ADsGetObject(Format('LDAP://%s',[AADSPath]),IDirectorySearch,ads)
  else
    ADsOpenObject(Format('LDAP://%s',[AADSPath]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IDirectorySearch,ads);
  try
    adp[0].dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    adp[0].vValue.dwType:=ADSTYPE_INTEGER;
    adp[0].vValue.__MIDL_0010.Integer_:=ADS_SCOPE_SUBTREE;
    hr:=ads.SetSearchPreference({$IFNDEF FPC}@{$ENDIF}adp[0],1);
    if (hr<>0) then
      Exit;
    wc:=Format('(&(objectClass=computer)(objectCategory=computer)(cn=%s))',[AName]);
    p[0]:=StringToOleStr('description');
    p[1]:=StringToOleStr('operatingSystem');
    p[2]:=StringToOleStr('operatingSystemServicePack');
    {$IFDEF FPC}pp:=@p;{$ENDIF}
    hr:=ads.ExecuteSearch(PWideChar(wc),{$IFDEF FPC}pp{$ELSE}@p{$ENDIF},Length(p),rh);
    try
      if Succeeded(hr) then begin
        hr:=ads.GetFirstRow(rh);
        if Succeeded(hr) then begin

          hr:=ads.GetColumn(rh,p[0],adc);
          if Succeeded(hr) and (adc.pADsValues<>nil) then begin
             Desc:=adc.pADsValues^.__MIDL_0010.CaseIgnoreString;
             ads.FreeColumn(adc);
          end;

          hr:=ads.GetColumn(rh,p[1],adc);
          if Succeeded(hr) and (adc.pADsValues<>nil) then begin
             OS:=adc.pADsValues^.__MIDL_0010.CaseIgnoreString;
             ads.FreeColumn(adc);
          end;

          hr:=ads.GetColumn(rh,p[2],adc);
          if Succeeded(hr) and (adc.pADsValues<>nil) then begin
             OS:=OS+' '+adc.pADsValues^.__MIDL_0010.CaseIgnoreString;
             ads.FreeColumn(adc);
          end;
        end;
      end;
    finally
      ads.CloseSearchHandle(rh);
    end;
  finally
    ads:=nil;
  end;
end;

{ TMiTeC_AD }

procedure TMiTeC_AD.Clear;
begin
  ClearObjects(FUsers);
  ClearObjects(FGroups);
  ClearObjects(FComps);
  ClearProps(FDomain);
end;

procedure TMiTeC_AD.ClearObjects(var AObjects: TADObjects);
var
  i: Integer;
begin
  for i:=0 to High(AObjects) do
    ClearProps(AObjects[i]);
  Finalize(AObjects);
end;

constructor TMiTeC_AD.Create(AOwner: TComponent);
begin
  inherited;
  FPwd:='';
  FUser:='';
  FADDB:='';
  FSchema:=nil;
  AD_LastError:='';
end;

destructor TMiTeC_AD.Destroy;
begin
  Clear;
  inherited;
end;

procedure TMiTeC_AD.SearchDirectory;
var
  ads: IDirectorySearch;
  p: PWideChar;
  rh: {$IFDEF FPC}Pointer{$ELSE}THandle{$ENDIF};
  adc: ads_search_column;
  hr: HResult;
  adp: array[0..0] of ads_searchpref_info;
  wc: WideString;
  i,j: Integer;
  cn: {$IFDEF UNICODE}TStringlist{$ELSE}TWideStrings{$ENDIF};
  pp: PWideChar;
begin
  pp:=nil;
  if (AUser='') or (APwd='') then
    ADsGetObject(Format('LDAP://%s',[AADSPath]),IDirectorySearch,ads)
  else
    ADsOpenObject(Format('LDAP://%s',[AADSPath]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IDirectorySearch,ads);
  cn:={$IFDEF UNICODE}TStringlist{$ELSE}TWideStrings{$ENDIF}.Create;
  try
    try
      GetObjectAttributeNames(AADSPath,AUser,APwd,ADClassNames[AType],cn);
    except
    end;
    adp[0].dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    adp[0].vValue.dwType:=ADSTYPE_INTEGER;
    adp[0].vValue.__MIDL_0010.Integer_:=ADS_SCOPE_SUBTREE;
    hr:=ads.SetSearchPreference({$IFNDEF FPC}@{$ENDIF}adp[0],1);
    if (hr<>0) then
      Exit;
    wc:=Format('(&(objectClass=%s)(objectCategory=%s))',[ADClassnames[AType],ADClassnames[AType]]);
    ads.ExecuteSearch(PWideChar(wc),pp,uint(-1),rh);
    hr:=ads.GetNextRow(rh);
    while(ads.GetNextColumnName(rh,p)=S_OK) do begin
      if Assigned(p) and (cn.IndexOf(p)=-1) then
        cn.Add(p);
      FreeADsMem(p);
    end;
    while (hr=S_OK) do begin
      SetLength(AObjects,Length(AObjects)+1);
      AObjects[High(AObjects)].ObjectType:=AType;
      for i:=0 to cn.Count-1 do begin
        hr:=ads.GetColumn(rh,PWideChar(cn[i]),adc);
        if Succeeded(hr) then begin
          with AObjects[High(AObjects)] do
            if (adc.pADsValues<>nil) then
              for j:=0 to adc.dwNumValues-1 do begin
                SetLength(Props,Length(Props)+1);
                with Props[High(Props)] do begin
                  Provider:=adLDAP;
                  Name:=cn[i];
                  Typ:=adc.dwADsType;
                  case adc.dwADsType of
                    ADSTYPE_DN_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.DNString);
                    ADSTYPE_CASE_EXACT_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.CaseExactString);
                    ADSTYPE_CASE_IGNORE_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.CaseIgnoreString);
                    ADSTYPE_PRINTABLE_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.PrintableString);
                    ADSTYPE_NUMERIC_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.NumericString);
                    ADSTYPE_BOOLEAN: Value:=adc.pADsValues.__MIDL_0010.Boolean_;
                    ADSTYPE_INTEGER: Value:=adc.pADsValues.__MIDL_0010.Integer_;
                    ADSTYPE_OCTET_STRING: Value:=GUIDToString(TGUID(adc.pADsValues.__MIDL_0010.OctetString));
                    ADSTYPE_UTC_TIME: Value:=SystemTimeToDatetime(adc.pADsValues.__MIDL_0010.UTCTime);
                    ADSTYPE_LARGE_INTEGER: Value:=adc.pADsValues.__MIDL_0010.LargeInteger;
                    ADSTYPE_PROV_SPECIFIC: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.ProviderSpecific);
                    ADSTYPE_PATH: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pPath^);
                    ADSTYPE_POSTALADDRESS: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pPostalAddress^);
                    ADSTYPE_TIMESTAMP: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Timestamp);
                    ADSTYPE_BACKLINK: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.BackLink);
                    ADSTYPE_TYPEDNAME: Value:=WideString(adc.pADsValues.__MIDL_0010.pTypedName.ObjectName);
                    ADSTYPE_HOLD: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Hold);
                    ADSTYPE_NETADDRESS: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pNetAddress^);
                    ADSTYPE_REPLICAPOINTER: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pReplicaPointer^);
                    ADSTYPE_FAXNUMBER: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pFaxNumber^);
                    ADSTYPE_EMAIL: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Email);
                    ADSTYPE_NT_SECURITY_DESCRIPTOR: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.SecurityDescriptor);
                    ADSTYPE_DN_WITH_BINARY: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pDNWithBinary^);
                    ADSTYPE_DN_WITH_STRING: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pDNWithString^);
                  end;
                end;
                Inc(adc.pADsValues);
              end;
          Dec(adc.pADsValues,adc.dwNumValues);
          ads.FreeColumn(adc);
        end;
      end;
      hr:=ads.GetNextRow(rh);
    end;
  finally
    ads.CloseSearchHandle(rh);
    ads:=nil;
    cn.Free;
  end;
end;

{
      hr:=ads.GetColumn(rh,UserAccountControl,adc);
      if Succeeded(hr) then begin
         if adc.pADsValues<>nil then begin
           uac:=adc.pAdsvalues^.__MIDL_0010.Integer;
           FUsers[High(FUsers)].AccountDisabled:=uac and ADS_UF_ACCOUNTDISABLE>0;
           FUsers[High(FUsers)].PasswordRequired:=uac and ADS_UF_PASSWD_NOTREQD=0;
           FUsers[High(FUsers)].IsAccountLocked:=uac and ADS_UF_LOCKOUT>0;
         end;
       ads.FreeColumn(adc);
      end;
      hr:=ads.GetColumn(rh,Membership,adc);
      if Succeeded(hr) then begin
        if adc.pADsValues<>nil then begin
          s:='';
          for i:=0 to adc.dwNumValues-1 do begin
            s:=s+GetDNPart(string(adc.pADsValues^.__MIDL_0010.CaseIgnoreString),'CN')+',';
            Inc(adc.pADsValues);
          end;
          SetLength(s,Length(s)-1);
          FUsers[High(FUsers)].Membership:=s;
        end;
        Dec(adc.pADsValues,adc.dwNumValues);
        ads.FreeColumn(adc);
      end;
}

{function TMiTeC_AD.EnumMembers(AGroup: IADsGroup): string;
var
  enum: IEnumVariant;
  users: IAdsMembers;
  user: IAdsUser;
  v: OleVariant;
  n: Cardinal;
begin
  Result:='';
  users:=AGroup.Members;
  enum:=users._NewEnum as IEnumVariant;
  if enum<>nil then begin
    while (enum.Next(1,v,n)=S_OK) do begin
      user:=IDispatch(v) as IAdsUser;
      Result:=Result+user.Name+',';
    end;
  end;
  if Result<>'' then
    SetLength(Result,Length(Result)-1);
end;}


(*
procedure TMiTeC_AD.EnumObjects;
var
  winntc: IADsContainer;
  enum: IEnumVARIANT;
  va: OleVariant;
  n: Cardinal;
  hr: integer;
  obj: IAds;
begin
  if (AUser='') or (APwd='') then
    ADsGetObject(Format('WinNT://%s',[AADSPath]),IADsContainer,winntc)
  else
    ADsOpenObject(Format('WinNT://%s',[AADSPath]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IADsContainer,winntc);
  obj:=nil;
  winntc.Filter:=VarArrayOf([AObject]);
  hr:=ADsBuildEnumerator(winntc,enum);
  while Succeeded(hr) do begin
    hr:=ADsEnumerateNext(enum,1,va,n);
    if n=0 then
      Break;
    IDispatch(va).QueryInterface(IADs,obj);
    if Assigned(obj) then begin
      SetLength(FUsers,Length(FUsers)+1);
      FUsers[High(FUsers)].ObjectType:=AObjectType;
      EnumProps(obj,FUsers[High(FUsers)]);
    end;
    Finalize(va);
    va:=null;
  end;
  obj:=nil;
  winntc:=nil;
end;

procedure TMiTeC_AD.EnumProps;
var
  i,j: Integer;
  pl: IADsPropertyList;
  pe: IADsPropertyEntry;
  pv: IADsPropertyValue;
  v: OLEVariant;
begin
  ClearProps(AResult);
  try
    AObj.GetInfo;
  except
    Exit;
  end;

  pl:=(AObj as IADsPropertyList);
  for i:=0 to pl.PropertyCount-1 do begin
    v:=pl.Item(i);
    if (VarType(v)=varDispatch) then begin
      pe:=IDispatch(v) as IADsPropertyEntry;
      if (pe<> nil) then begin
        if VarIsArray(pe.Values) then begin
          for j:=0 to VarArrayHighBound(pe.Values,1) do begin
            pv:=IDispatch(pe.Values[j]) as IADsPropertyValue;
            SetLength(AResult.Props,Length(AResult.Props)+1);
            with AResult.Props[High(AResult.Props)] do begin
              Provider:=adWinNT;
              Name:=pe.Name;
              Typ:=pv.Get_ADsType();
              case Typ of
                ADSTYPE_DN_STRING: Value:=pv.DNString;
                ADSTYPE_CASE_EXACT_STRING: Value:=pv.CaseExactString;
                ADSTYPE_CASE_IGNORE_STRING: Value:=pv.CaseIgnoreString;
                ADSTYPE_PRINTABLE_STRING: Value:=pv.PrintableString;
                ADSTYPE_NUMERIC_STRING: Value:=pv.NumericString;
                ADSTYPE_BOOLEAN: Value:=pv.Boolean;
                ADSTYPE_INTEGER: Value:=pv.Integer;
                ADSTYPE_UTC_TIME: Value:=pv.UTCTime;
                ADSTYPE_OCTET_STRING: if Pos('sid',lowercase(pe.Name))>0 then
                                        Value:=GetSIDString(pv)
                                      else
                                        Value:=GetGUIDString(pv);
                ADSTYPE_LARGE_INTEGER: Value:=GetLargeInteger(pv);
              end;
            end;
            pv:=nil;
          end;
        end;
      end;
    end;
  end;
end;
*)

function TMiTeC_AD.GetComputer(Index: Integer): TADObject;
begin
  Result:=FComps[Index];
end;

function TMiTeC_AD.GetComputerByIP(AIP: string): TADObject;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  for i:=0 to High(FComps) do
    if SameText(AIP,GetIP(FComps[i])) then begin
      Result:=FComps[i];
      Break;
    end;
end;

function TMiTeC_AD.GetComputerByName(AName: string): TADObject;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  for i:=0 to High(FComps) do
    if SameText(AName,GetName(FComps[i])) then begin
      Result:=FComps[i];
      Break;
    end;
end;

function TMiTeC_AD.GetComputerCount: Cardinal;
begin
  Result:=Length(FComps);
end;

procedure TMiTeC_AD.GetComputerExtraInfo(AName: WideString; var Result: TADObject);
var
  comp: IADsComputer;
begin
  try
    if (FUser='') or (FPwd='') then
      ADsGetObject(Format('WinNT://%s,computer',[AName]),IADsComputer,comp)
    else
      ADsOpenObject(Format('WinNT://%s,computer',[AName]),FUser,FPwd,ADS_SECURE_AUTHENTICATION,IADsComputer,comp);
    {try Result.Processor:=comp.Processor except end;
    try Result.ID:=comp.ComputerID except end;
    try Result.NetAddresses:=comp.NetAddresses except end;
    try Result.ProcessorCount:=comp.ProcessorCount except end;
    try Result.Model:=comp.Model except end;
    try Result.MemorySize:=comp.MemorySize except end;
    try Result.StorageCapacity:=comp.StorageCapacity except end;}
  except on e:Exception do SetADLastError('GetComputerExtraInfo: '+e.Message) end;
  comp:=nil;
end;

procedure TMiTeC_AD.GetComputerExtraInfo(AIndex: Integer);
begin
  GetComputerExtraInfo(GetName(FComps[AIndex]),FComps[AIndex]);
end;

function TMiTeC_AD.GetDomainProps;
{var
  winntd: IADsDomain;
  r: HRESULT;
  s: widestring;
begin
  try
    if (AUser='') or (APwd='') then
      r:=ADsGetObject(Format('WinNT://%s',[AADSPath]),IADsDomain,winntd)
    else
      r:=ADsOpenObject(Format('WinNT://%s',[AADSPath]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IADsDomain,winntd);
    EnumProps(winntd,FDomain);
    s:=TranslateDNSName(FADDB);
    FDomain.AddProp('distinguishedName',ADSTYPE_DN_STRING,s);
  finally
    Result:=(r=S_OK) and (winntd<>nil);
    winntd:=nil;
  end;
end;}
var
  ads: IDirectorySearch;
  p: PWideChar;
  rh: {$IFDEF FPC}Pointer{$ELSE}THandle{$ENDIF};
  adc: ads_search_column;
  hr: HResult;
  adp: array[0..0] of ads_searchpref_info;
  wc: WideString;
  i,j: Integer;
  cn: {$IFDEF UNICODE}TStringList{$ELSE}TWideStrings{$ENDIF};
  pp: PWideChar;
begin
  pp:=nil;
  Result:=False;
  if (AUser='') or (APwd='') then
    ADsGetObject(Format('LDAP://%s',[AADSPath]),IDirectorySearch,ads)
  else
    ADsOpenObject(Format('LDAP://%s',[AADSPath]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IDirectorySearch,ads);
  cn:={$IFDEF UNICODE}TStringList{$ELSE}TWideStrings{$ENDIF}.Create;
  try
    {try
      GetObjectAttributeNames(AADSPath,AUser,APwd,'domain',cn);
    except
    end;}
    adp[0].dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    adp[0].vValue.dwType:=ADSTYPE_INTEGER;
    adp[0].vValue.__MIDL_0010.Integer_:=ADS_SCOPE_SUBTREE;
    hr:=ads.SetSearchPreference({$IFNDEF FPC}@{$ENDIF}adp[0],1);
    if (hr<>0) then
      Exit;
    wc:=Format('(&(objectClass=%s)(objectCategory=%s))',['domain','domain']);
    ads.ExecuteSearch(PWideChar(wc),pp,$FFFFFFFF,rh);
    hr:=ads.GetFirstRow(rh);
    while(ads.GetNextColumnName(rh,p)=S_OK) do begin
      if Assigned(p) and (cn.IndexOf(wc)=-1) then
        cn.Add(p);
      FreeADsMem(p);
    end;
    if (hr=S_OK) then begin
      FDomain.ObjectType:=adDomain;
      for i:=0 to cn.Count-1 do begin
        hr:=ads.GetColumn(rh,PWideChar(cn[i]),adc);
        if Succeeded(hr) then begin
          with FDomain do
            if (adc.pADsValues<>nil) then
              for j:=0 to adc.dwNumValues-1 do begin
                SetLength(Props,Length(Props)+1);
                with Props[High(Props)] do begin
                  Provider:=adLDAP;
                  Name:=cn[i];
                  Typ:=adc.dwADsType;
                  case adc.dwADsType of
                    ADSTYPE_DN_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.DNString);
                    ADSTYPE_CASE_EXACT_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.CaseExactString);
                    ADSTYPE_CASE_IGNORE_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.CaseIgnoreString);
                    ADSTYPE_PRINTABLE_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.PrintableString);
                    ADSTYPE_NUMERIC_STRING: Value:=WideString(adc.pADsValues.__MIDL_0010.NumericString);
                    ADSTYPE_BOOLEAN: Value:=adc.pADsValues.__MIDL_0010.Boolean_;
                    ADSTYPE_INTEGER: Value:=adc.pADsValues.__MIDL_0010.Integer_;
                    ADSTYPE_OCTET_STRING: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.OctetString);
                    ADSTYPE_UTC_TIME: Value:=SystemTimeToDatetime(adc.pADsValues.__MIDL_0010.UTCTime);
                    ADSTYPE_LARGE_INTEGER: Value:=adc.pADsValues.__MIDL_0010.LargeInteger;
                    ADSTYPE_PROV_SPECIFIC: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.ProviderSpecific);
                    ADSTYPE_PATH: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pPath^);
                    ADSTYPE_POSTALADDRESS: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pPostalAddress^);
                    ADSTYPE_TIMESTAMP: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Timestamp);
                    ADSTYPE_BACKLINK: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.BackLink);
                    ADSTYPE_TYPEDNAME: Value:=WideString(adc.pADsValues.__MIDL_0010.pTypedName.ObjectName);
                    ADSTYPE_HOLD: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Hold);
                    ADSTYPE_NETADDRESS: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pNetAddress^);
                    ADSTYPE_REPLICAPOINTER: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pReplicaPointer^);
                    ADSTYPE_FAXNUMBER: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pFaxNumber^);
                    ADSTYPE_EMAIL: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.Email);
                    ADSTYPE_NT_SECURITY_DESCRIPTOR: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.SecurityDescriptor);
                    ADSTYPE_DN_WITH_BINARY: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pDNWithBinary^);
                    ADSTYPE_DN_WITH_STRING: Value:=GUIDToString(adc.pADsValues.__MIDL_0010.pDNWithString^);
                  end;
                end;
                Inc(adc.pADsValues);
              end;
          Dec(adc.pADsValues,adc.dwNumValues);
          ads.FreeColumn(adc);
        end;
      end;
    end;
  finally
    ads.CloseSearchHandle(rh);
    ads:=nil;
    cn.Free;
  end;
end;

function TMiTeC_AD.GetGroup(Index: Integer): TADObject;
begin
  Result:=FGroups[Index];
end;

function TMiTeC_AD.GetGroupByName(AName: string): TADObject;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  for i:=0 to High(FGroups) do
    if SameText(AName,GetName(FGroups[i])) then begin
      Result:=FGroups[i];
      Break;
    end;
end;

function TMiTeC_AD.GetGroupCount: Cardinal;
begin
  Result:=Length(FGroups);
end;

procedure TMiTeC_AD.GetGroupExtraInfo(AName: WideString; var Result: TADObject);
var
  Group: IADsGroup;
begin
  try
    if (FUser='') or (FPwd='') then
      ADsGetObject(Format('WinNT://%s/%s,group',[FADDB,AName]),IADsGroup,Group)
    else
      ADsOpenObject(Format('WinNT://%s/%s,group',[FADDB,AName]),FUser,FPwd,ADS_SECURE_AUTHENTICATION,IADsGroup,Group);
  except on e:Exception do SetADLastError('GetGroupExtraInfo: '+e.Message) end;
  group:=nil;
end;

procedure TMiTeC_AD.GetObjectAttributeNames;
var
  idx: array[0..0] of Integer;
  i,j: Integer;
  rh: {$IFDEF FPC}Pointer{$ELSE}THandle{$ENDIF};
  adc: ads_search_column;
  hr: HResult;
  p: array[0..4] of PWideChar;
  adp: array[0..0] of ads_searchpref_info;
  wc: WideString;
  sc: {$IFDEF UNICODE}TStringlist{$ELSE}TWideStrings{$ENDIF};
  {$IFDEF FPC}pp: PWideChar;{$ENDIF}
begin
  {$IFDEF FPC}pp:=nil;{$ENDIF}
  if not Assigned(FSchema) then begin
    if (AUser='') or (APwd='') then
      ADsGetObject(Format('LDAP://%s/CN=Schema,CN=Configuration,%s',[AADSPath,GetDN(FDomain)]),IDirectorySearch,FSchema)
    else
      ADsOpenObject(Format('LDAP://%s/CN=Schema,CN=Configuration,%s',[AADSPath,GetDN(FDomain)]),AUser,APwd,ADS_SECURE_AUTHENTICATION,IDirectorySearch,FSchema);
  end;
  sc:={$IFDEF UNICODE}TStringlist{$ELSE}TWideStrings{$ENDIF}.Create;
  try
    adp[0].dwSearchPref:=ADS_SEARCHPREF_SEARCH_SCOPE;
    adp[0].vValue.dwType:=ADSTYPE_INTEGER;
    adp[0].vValue.__MIDL_0010.Integer_:=ADS_SCOPE_SUBTREE;
    hr:=FSchema.SetSearchPreference({$IFNDEF FPC}@{$ENDIF}adp[0],1);
    if (hr<>0) then
      Exit;
    wc:=Format('(ldapdisplayname=%s)',[AClassname]);
    p[0]:=StringToOleStr('subclassof');
    p[1]:=StringToOleStr('systemAuxiliaryClass');
    p[2]:=StringToOleStr('auxiliaryClass');
    p[3]:=StringToOleStr('systemMayContain');
    p[4]:=StringToOleStr('mayContain');
    {$IFDEF FPC}pp:=@p;{$ENDIF}
    hr:=FSchema.ExecuteSearch(PWideChar(wc),{$IFDEF FPC}pp{$ELSE}@p{$ENDIF},Length(p),rh);
    try
      if Succeeded(hr) then begin
        hr:=FSchema.GetFirstRow(rh);
        if Succeeded(hr) then begin
          i:=0;
          repeat
            hr:=FSchema.GetColumn(rh,p[i],adc);
            if Succeeded(hr) and (adc.pADsValues<>nil) then begin
              for j:=0 to adc.dwNumValues-1 do begin
                if adc.dwADsType=ADSTYPE_CASE_IGNORE_STRING then begin
                  if i<3 then begin
                    if (adc.pADsValues^.__MIDL_0010.CaseIgnoreString<>'') and (adc.pADsValues^.__MIDL_0010.CaseIgnoreString<>AClassname) then
                      sc.Add(adc.pADsValues^.__MIDL_0010.CaseIgnoreString);
                  end else
                    if (AList.IndexOf(adc.pADsValues^.__MIDL_0010.CaseIgnoreString)=-1) then
                      AList.Add(adc.pADsValues^.__MIDL_0010.CaseIgnoreString);
                end;
                Inc(adc.pADsValues);
              end;
              Dec(adc.pADsValues,adc.dwNumValues);
              FSchema.FreeColumn(adc);
            end;
            Inc(i);
          until (i=Length(p))
        end;
      end;
    finally
      FSchema.CloseSearchHandle(rh);
    end;
    idx[0]:=0;
    while idx[0]<sc.Count do begin
      GetObjectAttributeNames(AADSPath,AUser,APwd,sc[idx[0]],AList);
      Inc(idx[0]);
    end;
  finally
    sc.Free;
  end;
end;

procedure TMiTeC_AD.GetGroupExtraInfo(AIndex: Integer);
begin
  GetGroupExtraInfo(GetName(FGroups[AIndex]),FGroups[AIndex]);
end;

function TMiTeC_AD.GetUser(Index: Integer): TADObject;
begin
  Result:=FUsers[Index];
end;

function TMiTeC_AD.GetUserByName(AName: string): TADObject;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  for i:=0 to High(FUsers) do
    if SameText(AName,GetName(FUsers[i])) then begin
      Result:=FUsers[i];
      Break;
    end;
end;

function TMiTeC_AD.GetUserCount: Cardinal;
begin
  Result:=Length(FUsers);
end;


procedure TMiTeC_AD.GetUserExtraInfo(AIndex: Integer);
begin
  GetUserExtraInfo(GetName(FUsers[AIndex]),FUsers[AIndex]);
end;

function TMiTeC_AD.GetUserExtraInfo(AName: WideString; var AUser: TADObject): boolean;
var
  user: IADsUser;
  r: HRESULT;
begin
  r:=S_FALSE;
  try
    if (FUser='') or (FPwd='') then
      r:=ADsGetObject(Format('WinNT://%s/%s,user',[FADDB,AName]),IADsUser,User)
    else
      r:=ADsOpenObject(Format('WinNT://%s/%s,user',[FADDB,AName]),FUser,FPwd,ADS_SECURE_AUTHENTICATION,IADsUser,User);
    {try AUser.LoginScript:=User.LoginScript except end;
    try AUser.MaxLogins:=User.MaxLogins except end;
    try AUser.MaxStorage:=User.MaxStorage except end;
    try AUser.PasswordExpirationDate:=User.PasswordExpirationDate except end;
    try AUser.PasswordMinimumLength:=User.PasswordMinimumLength except end;
    try AUser.Profile:=User.Profile except end;
    try AUser.RequireUniquePassword:=User.RequireUniquePassword except end;}
  except on e:Exception do SetADLastError('GetUserExtraInfo: '+e.Message) end;
  user:=nil;
  Result:=r=S_OK;
end;

function TMiTeC_AD.LoadFromStorage;

function CorrectString(S: string): string;
begin
  Result:=s;
  Result:=StringReplace(Result,'<CR>',#13,[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'<LF>',#10,[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,'<SEMI>',';',[rfReplaceAll,rfIgnoreCase]);
end;

procedure ParseObjectRecord(ASource: TStrings; var AObject: TADObject);
var
  i,t: Integer;
  sl1,sl2,sl3: TStringList;
  dt: TDateTime;
  v: Variant;
begin
  sl1:=TStringList.Create;
  sl2:=TStringList.Create;
  sl3:=TStringList.Create;
  try
    {$IFDEF BDS3PLUS}
    sl1.Delimiter:=';';
    sl1.StrictDelimiter:=True;
    sl1.DelimitedText:=ASource[0];
    sl2.Delimiter:=';';
    sl2.StrictDelimiter:=True;
    sl2.DelimitedText:=ASource[1];
    sl3.Delimiter:=';';
    sl3.StrictDelimiter:=True;
    sl3.DelimitedText:=ASource[2];
    {$ELSE}
    SetDelimitedText(ASource[0],';',sl1);
    SetDelimitedText(ASource[1],';',sl2);
    SetDelimitedText(ASource[2],';',sl3);
    {$ENDIF}
    for i:=0 to sl1.Count-1 do begin
      t:=StrToIntDef(sl2[i],ADSTYPE_CASE_IGNORE_STRING);
      if t=ADSTYPE_UTC_TIME then begin
        dt:=StrToFloatDef(sl3[i],0);
        v:=dt;
      end else
        v:=CorrectString(sl3[i]);
      AddProp(AObject,sl1[i],t,v);
    end;
  finally
    sl1.Free;
    sl2.Free;
    sl3.Free;
  end;
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  i: Integer;
  sl,ol: TStringList;
  ds: char;
begin
  Clear;
  FADDB:='';

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

    if Sub<>nil then begin
      sl:=TStringList.Create;
      ol:=TStringList.Create;
      ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
      try
        strm:=Sub.OpenStream('Domain',STG_READ_INSTORAGE,False);
        if strm<>nil then
          try
            LoadFromEncodedStream(strm,sl,ACodeStream);
            ParseObjectRecord(sl,FDomain);
            FDomain.ObjectType:=adDomain;
            Result:=True;
          finally
            strm.Free;
          end
        else
          Exit;

        if not Sub.ElementExists('Users') then
          Exit;
        strm:=Sub.OpenStream('Users',STG_READ_INSTORAGE,False);
        if strm=nil then
          Exit;
        try
          LoadFromEncodedStream(strm,sl,ACodeStream);
          i:=0;
          while i<sl.Count-3 do begin
            ol.Text:=sl[i];
            ol.Add(sl[i+1]);
            ol.Add(sl[i+2]);
            SetLength(FUsers,Length(FUsers)+1);
            FUsers[High(FUsers)].ObjectType:=adUser;
            ParseObjectRecord(ol,FUsers[High(FUsers)]);
            Inc(i,3);
          end;
        finally
          strm.Free;
        end;

        if not Sub.ElementExists('Groups') then
          Exit;
        strm:=Sub.OpenStream('Groups',STG_READ_INSTORAGE,False);
        if strm=nil then
          Exit;
        try
          LoadFromEncodedStream(strm,sl,ACodeStream);
          i:=0;
          while i<sl.Count-3 do begin
            ol.Text:=sl[i];
            ol.Add(sl[i+1]);
            ol.Add(sl[i+2]);
            SetLength(FGroups,Length(FGroups)+1);
            FGroups[High(FGroups)].ObjectType:=adGroup;
            ParseObjectRecord(ol,FGroups[High(FGroups)]);
            Inc(i,3);
          end;
        finally
          strm.Free;
        end;

        if not Sub.ElementExists('Computers') then
          Exit;
        strm:=Sub.OpenStream('Computers',STG_READ_INSTORAGE,False);
        if strm=nil then
          Exit;
        try
          LoadFromEncodedStream(strm,sl,ACodeStream);
          i:=0;
          while i<sl.Count-3 do begin
            ol.Text:=sl[i];
            ol.Add(sl[i+1]);
            ol.Add(sl[i+2]);
            SetLength(FComps,Length(FComps)+1);
            FComps[High(FComps)].ObjectType:=adComputer;
            ParseObjectRecord(ol,FComps[High(FComps)]);
            Inc(i,3);
          end;
        finally
          strm.Free;
        end;
      finally
        {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
        sl.Free;
        ol.Free;
        if Sub<>nil then
          Sub.Free;
      end;
    end;
    SetDataAvail(Length(FDomain.Props)>0);
  finally
    SS.Free;
  end;
end;

procedure TMiTeC_AD.RefreshComputers;
var
  adsp,user,pwd: WideString;
  p: Integer;
begin
  inherited;
  Clear;

  if FADDB='' then
    FADDB:=GetDNSDomain;
  if FADDB='' then begin
    p:=Pos('\',FUser);
    if p>0 then begin
      FADDB:=Copy(FUser,1,p-1);
      FUser:=Copy(FUser,p+1,255);
    end;
  end;
  adsp:=FADDB;
  user:=FUser;
  pwd:=FPwd;
  try GetDomainProps(adsp,user,pwd) except on e:Exception do SetADLastError('GetDomainProps: '+e.Message) end;
  try SearchDirectory(adsp,user,pwd,adComputer,FComps) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
  FSchema:=nil;
end;

procedure TMiTeC_AD.RefreshData;
var
  adsp,user,pwd: WideString;
  p: Integer;
begin
  inherited;
  Clear;

  if FADDB='' then
    FADDB:=GetDNSDomain;
  if FADDB='' then begin
    p:=Pos('\',FUser);
    if p>0 then begin
      FADDB:=Copy(FUser,1,p-1);
      FUser:=Copy(FUser,p+1,255);
    end;
  end;
  adsp:=FADDB;
  user:=FUser;
  pwd:=FPwd;

  try GetDomainProps(adsp,user,pwd) except on e: Exception do SetADLastError('GetDomainProps: '+e.Message) end;
  if Length(FDomain.Props)>0 then begin
    try SearchDirectory(adsp,user,pwd,adComputer,FComps) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
    try SearchDirectory(adsp,user,pwd,adUser,FUsers) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
    try SearchDirectory(adsp,user,pwd,adGroup,FGroups) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
  end;
  FSchema:=nil;
  SetDataAvail(Length(FDomain.Props)>0);
end;

procedure TMiTeC_AD.RefreshDomain;
var
  adsp,user,pwd: WideString;
  p: Integer;
begin
  inherited;
  Clear;

  if FADDB='' then
    FADDB:=GetDNSDomain;
  if FADDB='' then begin
    p:=Pos('\',FUser);
    if p>0 then begin
      FADDB:=Copy(FUser,1,p-1);
      FUser:=Copy(FUser,p+1,255);
    end;
  end;
  adsp:=FADDB;
  user:=FUser;
  pwd:=FPwd;

  try GetDomainProps(adsp,user,pwd) except on e:Exception do SetADLastError('GetDomainProps: '+e.Message) end;
end;

procedure TMiTeC_AD.RefreshGroups;
var
  adsp,user,pwd: WideString;
  p: Integer;
begin
  inherited;
  Clear;

  if FADDB='' then
    FADDB:=GetDNSDomain;
  if FADDB='' then begin
    p:=Pos('\',FUser);
    if p>0 then begin
      FADDB:=Copy(FUser,1,p-1);
      FUser:=Copy(FUser,p+1,255);
    end;
  end;
  adsp:=FADDB;
  user:=FUser;
  pwd:=FPwd;
  try GetDomainProps(adsp,user,pwd) except on e:Exception do SetADLastError('GetDomainProps: '+e.Message) end;
  try SearchDirectory(adsp,user,pwd,adGroup,FGroups) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
  FSchema:=nil;
end;

procedure TMiTeC_AD.RefreshUsers;
var
  adsp,user,pwd: WideString;
  p: Integer;
begin
  inherited;
  Clear;

  if FADDB='' then
    FADDB:=GetDNSDomain;
  if FADDB='' then begin
    p:=Pos('\',FUser);
    if p>0 then begin
      FADDB:=Copy(FUser,1,p-1);
      FUser:=Copy(FUser,p+1,255);
    end;
  end;
  adsp:=FADDB;
  user:=FUser;
  pwd:=FPwd;
  try GetDomainProps(adsp,user,pwd) except on e:Exception do SetADLastError('GetDomainProps: '+e.Message) end;
  try SearchDirectory(adsp,user,pwd,adUser,FUsers) except on e:Exception do SetADLastError('SearchDirectory: '+e.Message) end;
  FSchema:=nil;
end;

procedure TMiTeC_AD.SaveToStorage;

function CorrectString(S: string): string;
begin
  Result:=s;
  Result:=StringReplace(Result,#13,'<CR>',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,#10,'<LF>',[rfReplaceAll,rfIgnoreCase]);
  Result:=StringReplace(Result,';','<SEMI>',[rfReplaceAll,rfIgnoreCase]);
end;

procedure CreateObjectRecord(AObject: TADObject; AList: TStrings);
var
  i: Integer;
  s1,s2,s3: string;
begin
  s1:='';
  s2:='';
  s3:='';
  for i:=0 to High(AObject.Props) do begin
    s1:=s1+AObject.Props[i].Name+';';
    s2:=s2+IntToStr(AObject.Props[i].Typ)+';';
    if AObject.Props[i].Typ=ADSTYPE_UTC_TIME then
      s3:=s3+Format('%1.10f;',[VarToFloat(AObject.Props[i].Value)])
    else
      s3:=s3+CorrectString(AObject.Props[i].Value)+';';
  end;
  SetLength(s1,Length(s1)-1);
  SetLength(s2,Length(s2)-1);
  SetLength(s3,Length(s3)-1);
  AList.Add(s1);
  AList.Add(s2);
  AList.Add(s3);
end;

var
  stg: IStorage;
  SS: TStructuredStorage;
  Sub: TStructuredStorage;
  strm: TStorageStream;
  sl: TStringList;
  i: Integer;
  ds: char;
begin
  Sub:=nil;
  inherited SaveToStorage(AFilename,AWriteHeader,AFormat,AComment,ACodeStream);
  if StgIsStorageFile(PWideChar(WideString(AFileName)))<>S_OK then
    OleCheck(StgCreateDocFile(PWideChar(WideString(AFileName)),STG_CREATE_OPEN,0,stg))
  else
    OleCheck(StgOpenStorage(PWideChar(WideString(AFileName)),nil,STG_OPEN,nil,LongInt(nil),stg));
  SS:=TStructuredStorage.Create(nil,stg);
  try
    SS.DeleteElement(StorageFolderName);
    Sub:=SS.OpenSubStorage(StorageFolderName,STG_OPEN,True);
    sl:=TStringList.Create;
    ds:={$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator;
    {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:='.';
    try
      strm:=Sub.OpenStream('Domain',STG_OPEN,True);
      try
        CreateObjectRecord(FDomain,sl);
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;

      sl.Clear;
      strm:=Sub.OpenStream('Users',STG_OPEN,True);
      try
        for i:=0 to High(FUsers) do
          CreateObjectRecord(FUsers[i],sl);
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;

      strm:=Sub.OpenStream('Groups',STG_OPEN,True);
      try
        for i:=0 to High(FGroups) do
          CreateObjectRecord(FGroups[i],sl);
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;

      strm:=Sub.OpenStream('Computers',STG_OPEN,True);
      try
        for i:=0 to High(FComps) do
          CreateObjectRecord(FComps[i],sl);
        SaveToEncodedStream(sl,strm,ACodeStream);
      finally
        strm.Free;
      end;
    finally
      {$IFDEF RAD8PLUS}FormatSettings.{$ENDIF}DecimalSeparator:=ds;
      sl.Free;
    end;
  finally
    Sub.Free;
    try
      SS.Free;
    except
    end;
  end;
end;

{$IFDEF BDS3PLUS}
{ TADObject }

procedure TADObject.AddProp(AName: string; ATyp: Integer; AValue: Variant);
var
  idx: Integer;
begin
  {idx:=GetPropIdx(Aname);
  if idx=-1 then begin}
    SetLength(Props,Length(Props)+1);
    idx:=High(Props);
  //end;
  with Props[idx] do begin
    Name:=Aname;
    Typ:=ATyp;
    Value:=AValue;
  end;
end;

procedure TADObject.ClearProps;
begin
  Finalize(Props);
end;

function TADObject.GetDN: WideString;
begin
  Result:=GetProp('distinguishedName').Value;
end;

function TADObject.GetName: WideString;
begin
  Result:=GetProp('cn').Value;
  if Result='' then
    Result:=GetProp('name').Value;
end;

function TADObject.GetIP: WideString;
const
  ip: WideString = 'IP:';
var
  s: widestring;
  v: Variant;
  i: Integer;
begin
  v:=GetProp('NetAddresses').Value;
  if VarIsArray(v) then begin
    for i:=0 to VarArrayHighBound(v,0) do begin
      s:=VarToStr(v[i]);
      if s<>'' then begin
        if Pos(ip,s)=1 then begin
          s:=Copy(s,4,255);
          Break;
        end else
          s:='';
      end;
    end;
  end else begin
    s:=VarToStr(v);
    if s<>'' then begin
      if Pos(ip,s)=1 then
        s:=Copy(s,4,255)
      else
        s:='';
    end;
  end;
  Result:=s;
end;

function TADObject.GetProp(AName: string): TADProperty;
var
  i: Integer;
begin
  ZeroMemory(@Result,SizeOf(Result));
  for i:=0 to High(Props) do
    if SameText(Props[i].Name,AName) then begin
      Result:=Props[i];
      Break;
    end;
end;

function TADObject.GetPropAsDelimitedText(AName: string): string;
var
  i: Integer;
begin
  Result:='';
  for i:=0 to High(Props) do
    if SameText(Props[i].Name,AName) then
      Result:=Result+Props[i].Value+';';
  SetLength(Result,Length(Result)-1);
end;

function TADObject.GetPropIdx(AName: string): integer;
var
  i: Integer;
begin
  Result:=-1;
  for i:=0 to High(Props) do
    if SameText(Props[i].Name,AName) then begin
      Result:=i;
      Break;
    end;
end;
{$ENDIF}

end.

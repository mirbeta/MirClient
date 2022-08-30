{*******************************************************}
{               MiTeC Common Routines                   }
{          Windows NT Account Enumeration               }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_AccountsNT;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

type
  PUserInfo = ^TUserInfo;
  TUserInfo = record
    Name: string;
    Comment: string;
    Sid: string;
    NumberOfSubAuths: Cardinal;
    SidLength: Cardinal;
    Domain: string;
    SidType: SID_NAME_USE;
    FullName: string;
    LastLogon: TDateTime;
    LastLogoff: TDateTime;
    LogonCount: Cardinal;
    Expiration: TDateTime;
    Disabled: Boolean;
    Flags: Cardinal;
    _Membership: string;
  end;

  PGroupInfo = ^TGroupInfo;
  TGroupInfo = record
    Name: string;
    Comment: string;

    Sid: string;
    NumberOfSubAuths: Cardinal;
    SidLength: Cardinal;
    Domain: string;
    SidType: SID_NAME_USE;
  end;

  TAccounts = class(TPersistent)
  private
    FUsers, FLocalGroups, FGlobalGroups: TStringList;
    FMachine: string;
    FWMachine: WideString;

    procedure FreeUserList(var AList: TStringList);
    procedure FreeGroupList(var AList: TStringList);

    function GetLocalGroup(Index: Integer): PGroupInfo;
    function GetUser(Index: Integer): PUserInfo;
    function FindUser(AName: string): PUserInfo;
    function GetUserCount: Cardinal;
    function GetLocalGroupCount: Cardinal;
    function GetGlobalGroup(Index: Integer): PGroupInfo;
    function GetGlobalGroupCount: Cardinal;

    function RetrieveUsers(AMachine: string): Cardinal;
    procedure RetrieveLocalGroups(AMachine: string);
    procedure RetrieveGlobalGroups(AMachine: string);

    function GetSIDFromAccount(AMachine, AName: string; var Domain: string; var SidLen,SubAuthCount: Cardinal; var NameUse: SID_NAME_USE): string;
    procedure SetMachine(const Value: string);

  public
    constructor Create;
    destructor Destroy; override;

    procedure Clear;

    function RefreshUsers: Cardinal;
    procedure RefreshLocalGroups;
    procedure RefreshGlobalGroups;

    function DeleteAccount(AName: string): cardinal;
    function CreateAccount(AName,APwd: string): cardinal;
    function EnableAccount(AName: string): Cardinal;
    function DisableAccount(AName: string): Cardinal;
    function SetMembership(AAccount,AGroup: string; Adelete: boolean): Cardinal;

    procedure GetUserLocalGroups(AUsername: string; var AList: TStringList);
    procedure GetLocalGroupUsers(AGroupname: string; var AList: TStringList);
    procedure GetUserGlobalGroups(AUsername: string; var AList: TStringList);
    procedure GetGlobalGroupUsers(AGroupname: string; var AList: TStringList);
    function IsStandardUser(AUsername: string): Boolean;

    property Machine: string read FMachine write SetMachine;

    property UserCount: Cardinal read GetUserCount;
    property Users[Index: Integer]: PUserInfo read GetUser;
    property UserByName[AName: string]: PUserInfo read FindUser;

    property LocalGroupCount: Cardinal read GetLocalGroupCount;
    property LocalGroups[Index: Integer]: PGroupInfo read GetLocalGroup;

    property GlobalGroupCount: Cardinal read GetGlobalGroupCount;
    property GlobalGroups[Index: Integer]: PGroupInfo read GetGlobalGroup;
  end;

function GetNameUseStr(Value: Cardinal): string;
function ConvertSIDToStringSID(ASID: PSID): string;

implementation

uses MiTeC_Windows, MiTeC_NetAPI32, MiTeC_Routines, MiTeC_Datetime;

function ConvertSIDToStringSID(ASID: PSID): string;
var
  i: integer;
  SIDAuth: PSIDIdentifierAuthority;
  SIDSubAuth: Cardinal;
  SIDSubAuthCount: Byte;
begin
  Result:='S-1-';
  SIDAuth:=GetSidIdentifierAuthority(ASID);
  for i:=0 to 5 do
    if SIDAuth.Value[i]<>0 then
      Result:=Result+IntToStr(SIDAuth.Value[i]);
  SIDSubAuthCount:=GetSidSubAuthorityCount(ASID)^;
  for i:=0 to SIDSubAuthCount-1 do begin
    SIDSubAuth:=GetSidSubAuthority(ASID,i)^;
    Result:=Result+'-'+IntToStr(SIDSubAuth);
  end;
end;

function GetNameUseStr(Value: Cardinal): string;
begin
  case Value of
    Cardinal(SidTypeUser): Result:='SidTypeUser';
    Cardinal(SidTypeGroup): Result:='SidTypeGroup';
    Cardinal(SidTypeDomain): Result:='SidTypeDomain';
    Cardinal(SidTypeAlias): Result:='SidTypeAlias';
    Cardinal(SidTypeWellKnownGroup): Result:='SidTypeWellKnownGroup';
    Cardinal(SidTypeDeletedAccount): Result:='SidTypeDeletedAccount';
    Cardinal(SidTypeInvalid): Result:='SidTypeInvalid';
    Cardinal(SidTypeUnknown): Result:='SidTypeUnknown';
    9: Result:='SidTypeComputer';
    10: Result:='SidTypeLabel';
  end;
end;

{ TAccounts }

procedure TAccounts.Clear;
begin
  FreeUserList(FUsers);
  FreeGroupList(FLocalGroups);
  FreeGroupList(FGlobalGroups);
end;

constructor TAccounts.Create;
begin
  FUsers:=TStringList.Create;
  FLocalGroups:=TStringList.Create;
  FGlobalGroups:=TStringList.Create;
  InitNETAPI;
end;

function TAccounts.CreateAccount;
var
 WUser, WPwd: WideString;
 usri1: USER_INFO_1;
 parm_err: Cardinal;
begin
  WUser:=Aname;
  WPwd:=APwd;
  usri1.usri1_name:=PWideChar(WUser);
  usri1.usri1_password:=PWideChar(WPwd);
  usri1.usri1_password_age:=0;
  usri1.usri1_priv:=USER_PRIV_USER;
  usri1.usri1_home_dir:='';
  usri1.usri1_comment:='';
  usri1.usri1_flags:=UF_SCRIPT or UF_DONT_EXPIRE_PASSWD or UF_NORMAL_ACCOUNT;//$10201;
  usri1.usri1_script_path:='';
  Result:=NetUserAdd(nil,1,@usri1,parm_err);
end;

function TAccounts.DeleteAccount;
var
  i: Integer;
  WName: WideString;
begin
  i:=Pos('\',AName);
  if i>0 then
    AName:=Copy(AName,i+1,255);
  WName:=AName;
  Result:=NetUserDel(PWideChar(FWMachine),PWideChar(WName));
end;

destructor TAccounts.Destroy;
begin
  FreeUserList(FUsers);
  FUsers.Free;
  FreeGroupList(FLocalGroups);
  FLocalGroups.Free;
  FreeGroupList(FGlobalGroups);
  FGlobalGroups.Free;
  inherited;
end;

function TAccounts.EnableAccount(AName: string): Cardinal;
var
 WUser: WideString;
 usri: USER_INFO_1008;
begin
  Result:=0;
  WUser:=Aname;
  try
    usri.usri1008_flags:=FindUser(Aname).Flags;
    if usri.usri1008_flags and UF_ACCOUNTDISABLE=0 then
      Exit;
    usri.usri1008_flags:=usri.usri1008_flags - UF_ACCOUNTDISABLE;
    Result:=NetUserSetInfo(nil,PWideChar(WUser),1008,@usri,nil);
  finally

  end;
end;

function TAccounts.DisableAccount(AName: string): Cardinal;
var
 WUser: WideString;
 usri: USER_INFO_1008;
begin
  Result:=0;
  WUser:=Aname;
  try
    usri.usri1008_flags:=FindUser(Aname).Flags;
    if usri.usri1008_flags and UF_ACCOUNTDISABLE>0 then
      Exit;
    usri.usri1008_flags:=usri.usri1008_flags or UF_ACCOUNTDISABLE;
    Result:=NetUserSetInfo(nil,PWideChar(WUser),1008,@usri,nil);
  finally

  end;
end;

function TAccounts.FindUser(AName: string): PUserInfo;
var
  i: Integer;
begin
  Result:=nil;
  for i:=0 to FUsers.Count-1 do
    if SameText(PUserInfo(FUsers.Objects[i])^.Name,AName) then begin
      Result:=PUserInfo(FUsers.Objects[i]);
      Break;
    end;
end;

procedure TAccounts.FreeGroupList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(PGroupInfo(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

procedure TAccounts.FreeUserList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(PUserInfo(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

function TAccounts.GetGlobalGroup(Index: Integer): PGroupInfo;
begin
  if Index<FGlobalGroups.Count then
    Result:=PGroupInfo(FGlobalGroups.Objects[Index])
  else
    Result:=nil;
end;

function TAccounts.GetGlobalGroupCount: Cardinal;
begin
  Result:=FGlobalGroups.Count;
end;

procedure TAccounts.GetGlobalGroupUsers;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pTmpBuf,pBuf: PGROUP_USERS_INFO_0;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  wg: WideString;
begin
  wg:=AGroupname;
  AList.Clear;
  pBuf:=nil;
  Loop:=True;
  dwLevel:=0;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetGroupGetUsers(PWideChar(FWMachine),PWideChar(wg),
                              dwLevel, Pointer(pBuf),
                              dwPrefMaxLen, dwEntriesRead, dwTotalEntries,
                              dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        AList.Add(WideCharToString(pTmpBuf.grui0_name));
        pTmpBuf:=PGROUP_USERS_INFO_0(PAnsiChar(pTmpBuf)+SizeOf(GROUP_USERS_INFO_0));
      end;
      if Assigned(pBuf) then begin
        NetApiBufferFree(pBuf);
        pBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

function TAccounts.GetLocalGroup(Index: Integer): PGroupInfo;
begin
  if Index<FLocalGroups.Count then
    Result:=PGroupInfo(FLocalGroups.Objects[Index])
  else
    Result:=nil;
end;

function TAccounts.GetLocalGroupCount: Cardinal;
begin
  Result:=FLocalGroups.Count;
end;

procedure TAccounts.GetLocalGroupUsers;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pTmpBuf,pBuf: PLOCALGROUP_MEMBERS_INFO_3;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  wg: WideString;
begin
  wg:=AGroupname;
  AList.Clear;
  pBuf:=nil;
  Loop:=True;
  dwLevel:=3;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetLocalGroupGetMembers(PWideChar(FWMachine),PWideChar(wg),
                              dwLevel, Pointer(pBuf),
                              dwPrefMaxLen, dwEntriesRead, dwTotalEntries,
                              dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        AList.Add(WideCharToString(pTmpBuf.lgrmi3_domainandname));
        pTmpBuf:=PLOCALGROUP_MEMBERS_INFO_3(PAnsiChar(pTmpBuf)+SizeOf(LOCALGROUP_MEMBERS_INFO_3));
      end;
      if Assigned(pBuf) then begin
        NetApiBufferFree(pBuf);
        pBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

function TAccounts.GetSIDFromAccount(AMachine, AName: string; var Domain: string; var SidLen,SubAuthCount: Cardinal; var NameUse: SID_NAME_USE): string;
var
  SID: PSID;
  szDomain: PChar;
  cbDomain, cbSID: Cardinal;
begin
  SID:=nil;
  cbDomain:=0;
  cbSID:=0;
  szDomain:=nil;
  LookupAccountName(PChar(AMachine),PChar(AName),SID,cbSID,szDomain,cbDomain,NameUse);
  szDomain:=StrAlloc(cbDomain);
  SID:=AllocMem(cbSID);
  if LookupAccountName(PChar(AMachine),PChar(AName),SID,cbSID,szDomain,cbDomain,NameUse) then begin
    Result:=ConvertSIDToStringSID(SID);
    SubAuthCount:=GetSidSubAuthorityCount(PSID(SID))^;
    SidLen:=GetLengthSid(PSID(SID));
    Domain:=szDomain;
  end else
    Result:=GetErrorMessage(GetLastError);
  StrDispose(szDomain);
  FreeMem(SID);
end;

function TAccounts.GetUser(Index: Integer): PUserInfo;
begin
  if Index<FUsers.Count then
    Result:=PUserInfo(FUsers.Objects[Index])
  else
    Result:=nil;
end;

function TAccounts.GetUserCount: Cardinal;
begin
  Result:=FUsers.Count;
end;

procedure TAccounts.GetUserGlobalGroups;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pTmpBuf,pBuf: PGROUP_USERS_INFO_0;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  wu: WideString;
begin
  wu:=AUsername;
  AList.Clear;
  pBuf:=nil;
  dwLevel:=0;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  nStatus:=NetUserGetGroups(PWideChar(FWMachine),PWideChar(wu),
                         dwLevel, Pointer(pBuf),
                         dwPrefMaxLen, dwEntriesRead, dwTotalEntries);
  if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
    pTmpBuf:=pBuf;
    for i:=0 to dwEntriesRead-1 do begin
      AList.Add(WideCharToString(pTmpBuf.grui0_name));
      pTmpBuf:=PGROUP_USERS_INFO_0(PAnsiChar(pTmpBuf)+SizeOf(GROUP_USERS_INFO_0));
    end;
  end;
  if Assigned(pBuf) then
      NetApiBufferFree(pBuf);
end;

procedure TAccounts.GetUserLocalGroups;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pTmpBuf,pBuf: PLOCALGROUP_USERS_INFO_0;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  wu: WideString;
begin
  wu:=AUsername;
  AList.Clear;
  pBuf:=nil;
  dwLevel:=0;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  nStatus:=NetUserGetLocalGroups(PWideChar(FWMachine),PWideChar(wu),
                         dwLevel, LG_INCLUDE_INDIRECT, Pointer(pBuf),
                         dwPrefMaxLen, dwEntriesRead, dwTotalEntries);
  if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
    pTmpBuf:=pBuf;
    for i:=0 to dwEntriesRead-1 do begin
      AList.Add(WideCharToString(pTmpBuf.lgrui0_name));
      pTmpBuf:=PLOCALGROUP_USERS_INFO_0(PAnsiChar(pTmpBuf)+SizeOf(LOCALGROUP_USERS_INFO_0));
    end;
  end;
  if Assigned(pBuf) then
      NetApiBufferFree(pBuf);
end;

function TAccounts.IsStandardUser(AUsername: string): Boolean;
var
  i: Integer;
  sl: TStringList;
begin
  Result:=False;
  sl:=TStringList.Create;
  try
    GetUserLocalGroups(AUsername,sl);
    for i:=0 to LocalGroupCount-1 do
      if SameText(LocalGroups[i].Domain,'BUILTIN') and not SameText(LocalGroups[i].Name,'Guests') then
        if sl.IndexOf(LocalGroups[i].Name)>-1 then begin
          Result:=True;
          Break;
        end;
  finally
    sl.Free;
  end;
end;

procedure TAccounts.RefreshGlobalGroups;
begin
  RetrieveGlobalGroups(Machine);
end;

procedure TAccounts.RefreshLocalGroups;
begin
  RetrieveLocalGroups(Machine);
end;

function TAccounts.RefreshUsers: Cardinal;
begin
  Result:=RetrieveUsers(Machine);
end;

procedure TAccounts.RetrieveGlobalGroups(AMachine: string);
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pgi: PGroupInfo;
  pTmpBuf,pBuf: PGROUP_INFO_2;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
begin
  FreeGroupList(FGlobalGroups);
  pBuf:=nil;
  Loop:=True;
  dwLevel:=2;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetGroupEnum(PWideChar(FWMachine),
                         dwLevel, Pointer(pBuf),
                         dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(pgi);
        with pTmpBuf^ do begin
          pgi^.Name:=WideCharToString(grpi2_name);
          pgi^.Comment:=WideCharToString(grpi2_comment);
          pgi^.Sid:=GetSIDFromAccount(AMachine,pgi^.Name,pgi^.Domain,pgi^.SidLength,pgi^.NumberOfSubAuths,pgi^.SidType);
          FGlobalGroups.AddObject(pgi^.Name,TObject(pgi));
          pTmpBuf:=PGROUP_INFO_2(PAnsiChar(pTmpBuf)+SizeOf(GROUP_INFO_2));
        end;
      end;
      if Assigned(pBuf) then begin
        NetApiBufferFree(pBuf);
        pBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

procedure TAccounts.RetrieveLocalGroups;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pgi: PGroupInfo;
  pTmpBuf,pBuf: PLOCALGROUP_INFO_1;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
begin
  FreeGroupList(FLocalGroups);
  pBuf:=nil;
  Loop:=True;
  dwLevel:=1;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetLocalGroupEnum(PWideChar(FWMachine),
                         dwLevel, Pointer(pBuf),
                         dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(pgi);
        with pTmpBuf^ do begin
          pgi^.Name:=WideCharToString(lgrpi1_name);
          pgi^.Comment:=WideCharToString(lgrpi1_comment);
          pgi^.Sid:=GetSIDFromAccount(AMachine,pgi^.Name,pgi^.Domain,pgi^.SidLength,pgi^.NumberOfSubAuths,pgi^.SidType);
          FLocalGroups.AddObject(pgi^.Name,TObject(pgi));
          pTmpBuf:=PLOCALGROUP_INFO_1(PAnsiChar(pTmpBuf)+SizeOf(LOCALGROUP_INFO_1));
        end;
      end;
      if Assigned(pBuf) then begin
        NetApiBufferFree(pBuf);
        pBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

function TAccounts.RetrieveUsers(AMachine: string): Cardinal;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  pui: PUserInfo;
  pTmpBuf,pBuf: PUSER_INFO_3;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
begin
  Result:=0;
  FreeUserList(FUsers);
  pBuf:=nil;
  Loop:=True;
  dwLevel:=3;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetUserEnum(PWideChar(FWMachine),
                         dwLevel, FILTER_NORMAL_ACCOUNT, Pointer(pBuf),
                         dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(pui);
        with pTmpBuf^ do begin
          pui^.Name:=WideCharToString(usri3_name);
          pui^.FullName:=WideCharToString(usri3_full_name);
          pui^.Comment:=WideCharToString(usri3_comment);
          pui^.LastLogon:=UTCToDateTime(pTmpBuf^.usri3_last_logon);
          pui^.LastLogoff:=UTCToDateTime(pTmpBuf^.usri3_last_logoff);
          if pTmpBuf^.usri3_acct_expires=TIMEQ_FOREVER then
            pui^.Expiration:=-1
          else
            pui^.Expiration:=UTCToDateTime(pTmpBuf^.usri3_acct_expires);
          pui^.LogonCount:=pTmpBuf^.usri3_num_logons;
          pui^.Sid:=GetSIDFromAccount(AMachine,pui^.Name,pui^.Domain,pui^.SidLength,pui^.NumberOfSubAuths,pui^.SidType);
          pui^.Disabled:=pTmpBuf^.usri3_flags and UF_ACCOUNTDISABLE = UF_ACCOUNTDISABLE;
          pui^.Flags:=pTmpBuf^.usri3_flags;
          FUsers.AddObject(pui^.Name,TObject(pui));
          pTmpBuf:=PUSER_INFO_3(PAnsiChar(pTmpBuf)+SizeOf(USER_INFO_3));
        end;
      end;
      if Assigned(pBuf) then begin
        NetApiBufferFree(pBuf);
        pBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else begin
      Result:=GetLastError;
      Loop:=False;
    end;
  end;
  if Assigned(pBuf) then
    NetApiBufferFree(pBuf);
end;

procedure TAccounts.SetMachine(const Value: string);
begin
  FMachine := Value;
  if FMachine='' then
    FWMachine:='\\.'
  else
    FWMachine:='\\'+FMachine;
end;

function TAccounts.SetMembership;
var
  lgmi3: LOCALGROUP_MEMBERS_INFO_3;
  wuser,wgroup: WideString;
begin
  wuser:=AAccount;
  wgroup:=AGroup;
  lgmi3.lgrmi3_domainandname:=PWideChar(WUser);
  if Adelete then
    Result:=NetLocalGroupDelMembers(PWideChar(FWMachine),PWideChar(wgroup),3,lgmi3,1)
  else
    Result:=NetLocalGroupAddMembers(PWideChar(FWMachine),PWideChar(wgroup),3,lgmi3,1);
end;

end.

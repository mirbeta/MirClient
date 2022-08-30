{*******************************************************}
{               MiTeC Common Routines                   }
{                  NT Security API                      }
{                                                       }
{        Copyright (c) 1997-2019 Michal Mutl            }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_NTSecAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes, WinAPI.Messages, VCL.Controls,
     {$ELSE}
     Windows, Messages, SysUtils, Classes, Controls,
     {$ENDIF}
     MiTeC_LSAAPI;


type
  ENTException = class(Exception);
  
const
  SE_CREATE_TOKEN_NAME        = 'SeCreateTokenPrivilege';
  SE_ASSIGNPRIMARYTOKEN_NAME  = 'SeAssignPrimaryTokenPrivilege';
  SE_LOCK_MEMORY_NAME         = 'SeLockMemoryPrivilege';
  SE_INCREASE_QUOTA_NAME      = 'SeIncreaseQuotaPrivilege';
  SE_UNSOLICITED_INPUT_NAME   = 'SeUnsolicitedInputPrivilege';
  SE_MACHINE_ACCOUNT_NAME     = 'SeMachineAccountPrivilege';
  SE_TCB_NAME                 = 'SeTcbPrivilege';
  SE_SECURITY_NAME            = 'SeSecurityPrivilege';
  SE_TAKE_OWNERSHIP_NAME      = 'SeTakeOwnershipPrivilege';
  SE_LOAD_DRIVER_NAME         = 'SeLoadDriverPrivilege';
  SE_SYSTEM_PROFILE_NAME      = 'SeSystemProfilePrivilege';
  SE_SYSTEMTIME_NAME          = 'SeSystemtimePrivilege';
  SE_PROF_SINGLE_PROCESS_NAME = 'SeProfileSingleProcessPrivilege';
  SE_INC_BASE_PRIORITY_NAME   = 'SeIncreaseBasePriorityPrivilege';
  SE_CREATE_PAGEFILE_NAME     = 'SeCreatePagefilePrivilege';
  SE_CREATE_PERMANENT_NAME    = 'SeCreatePermanentPrivilege';
  SE_BACKUP_NAME              = 'SeBackupPrivilege';
  SE_RESTORE_NAME             = 'SeRestorePrivilege';
  SE_SHUTDOWN_NAME            = 'SeShutdownPrivilege';
  SE_DEBUG_NAME               = 'SeDebugPrivilege';
  SE_AUDIT_NAME               = 'SeAuditPrivilege';
  SE_SYSTEM_ENVIRONMENT_NAME  = 'SeSystemEnvironmentPrivilege';
  SE_CHANGE_NOTIFY_NAME       = 'SeChangeNotifyPrivilege';
  SE_REMOTE_SHUTDOWN_NAME     = 'SeRemoteShutdownPrivilege';
  SE_UNDOCK_NAME              = 'SeUndockPrivilege';
  SE_SYNC_AGENT_NAME          = 'SeSyncAgentPrivilege';
  SE_ENABLE_DELEGATION_NAME   = 'SeEnableDelegationPrivilege';
  SE_MANAGE_VOLUME_NAME       = 'SeManageVolumePrivilege';

  SE_INTERACTIVE_LOGON_NAME      = 'SeInteractiveLogonRight';
  SE_NETWORK_LOGON_NAME          = 'SeNetworkLogonRight';
  SE_BATCH_LOGON_NAME            = 'SeBatchLogonRight';
  SE_SERVICE_LOGON_NAME          = 'SeServiceLogonRight';
  SE_DENY_INTERACTIVE_LOGON_NAME = 'SeDenyInteractiveLogonRight';
  SE_DENY_NETWORK_LOGON_NAME     = 'SeDenyNetworkLogonRight';
  SE_DENY_BATCH_LOGON_NAME       = 'SeDenyBatchLogonRight';
  SE_DENY_SERVICE_LOGON_NAME     = 'SeDenyServiceLogonRight';
  SE_REMOTE_INTERACTIVE_LOGON_NAME  = 'SeRemoteInteractiveLogonRight';
  SE_DENY_REMOTE_INTERACTIVE_LOGON_NAME = 'SeDenyRemoteInteractiveLogonRight';

 PrivilegeNames : array [0..27] of string = (
   SE_INTERACTIVE_LOGON_NAME,
   SE_NETWORK_LOGON_NAME,
   SE_BATCH_LOGON_NAME,
   SE_SERVICE_LOGON_NAME,
   SE_CREATE_TOKEN_NAME,
   SE_ASSIGNPRIMARYTOKEN_NAME,
   SE_LOCK_MEMORY_NAME,
   SE_INCREASE_QUOTA_NAME,
   SE_UNSOLICITED_INPUT_NAME,
   SE_MACHINE_ACCOUNT_NAME,
   SE_TCB_NAME,
   SE_SECURITY_NAME,
   SE_TAKE_OWNERSHIP_NAME,
   SE_LOAD_DRIVER_NAME,
   SE_SYSTEM_PROFILE_NAME,
   SE_SYSTEMTIME_NAME,
   SE_PROF_SINGLE_PROCESS_NAME,
   SE_INC_BASE_PRIORITY_NAME,
   SE_CREATE_PAGEFILE_NAME,
   SE_CREATE_PERMANENT_NAME,
   SE_BACKUP_NAME,
   SE_RESTORE_NAME,
   SE_SHUTDOWN_NAME,
   SE_DEBUG_NAME,
   SE_AUDIT_NAME,
   SE_SYSTEM_ENVIRONMENT_NAME,
   SE_CHANGE_NOTIFY_NAME,
   SE_REMOTE_SHUTDOWN_NAME);

  _DELETE                = $00010000;
  READ_CONTROL           = $00020000;
  FILE_READ_DATA         = $0001; // file & pipe
  FILE_LIST_DIRECTORY    = $0001; // directory
  FILE_WRITE_DATA        = $0002; // file & pipe
  FILE_ADD_FILE          = $0002; // directory
  FILE_APPEND_DATA       = $0004; // file
  FILE_ADD_SUBDIRECTORY  = $0004; // directory
  FILE_CREATE_PIPE_INSTANCE = $0004; // named pipe
  FILE_READ_EA           = $0008; // file & directory
  FILE_WRITE_EA          = $0010; // file & directory
  FILE_EXECUTE           = $0020; // file
  FILE_TRAVERSE          = $0020; // directory
  FILE_DELETE_CHILD      = $0040; // directory
  FILE_READ_ATTRIBUTES   = $0080; // all
  FILE_WRITE_ATTRIBUTES  = $0100; // all
  FILE_ALL_ACCESS        = STANDARD_RIGHTS_REQUIRED or SYNCHRONIZE or $1FF;
  FILE_GENERIC_READ      = STANDARD_RIGHTS_READ or FILE_READ_DATA or FILE_READ_ATTRIBUTES or FILE_READ_EA or SYNCHRONIZE;
  FILE_GENERIC_WRITE     = STANDARD_RIGHTS_WRITE or FILE_WRITE_DATA or FILE_WRITE_ATTRIBUTES or FILE_WRITE_EA or FILE_APPEND_DATA or SYNCHRONIZE;
  FILE_GENERIC_EXECUTE   = STANDARD_RIGHTS_EXECUTE or FILE_READ_ATTRIBUTES or FILE_EXECUTE or SYNCHRONIZE;

  PRIV_READ = FILE_READ_DATA or FILE_READ_EA or FILE_READ_ATTRIBUTES or READ_CONTROL or SYNCHRONIZE;
  PRIV_READ_EXECUTE = PRIV_READ or FILE_EXECUTE;
  PRIV_READ_WRITE = PRIV_READ or FILE_WRITE_DATA or FILE_APPEND_DATA or FILE_WRITE_EA or FILE_WRITE_ATTRIBUTES;
  PRIV_READ_WRITE_EXECUTE = PRIV_READ_WRITE or FILE_EXECUTE;
  PRIV_CHANGE = PRIV_READ_WRITE_EXECUTE or _DELETE;
  PRIV_FULL_CONTROL = PRIV_CHANGE or FILE_DELETE_CHILD or WRITE_DAC or WRITE_OWNER;

  SECURITY_NT_AUTHORITY: TSIDIdentifierAuthority =
    (Value: (0, 0, 0, 0, 0, 5));
  SECURITY_BUILTIN_DOMAIN_RID = $00000020;
  DOMAIN_ALIAS_RID_ADMINS     = $00000220;

type
  TGrantPermissionSid = record  { must keep in synch with Helper.c }
    Authority: TSIDIdentifierAuthority;
    SubAuthCount: Byte;
    SubAuth: array[0..1] of Cardinal;
  end;
  TGrantPermissionEntry = record  { must keep in synch with Helper.c }
    Sid: TGrantPermissionSid;
    AccessMask: Cardinal;
  end;

function IsAdminAccount(const ADomain,AName,APassword: string): Boolean;
procedure GetObjectAccessRights (const AComputername,objectName : string; list : TStrings);
function GetPrivilegeDisplayName (const Acomputername, privilege : string) : string;
procedure GrantPrivilege (const acomputername,accountServer, user, privilege : string; bRemove : boolean);
function GrantPermissionOnFile(Filename: string; const Entries: TGrantPermissionEntry; const EntryCount: Integer): Boolean;
function GetMachine :string;

var
  MachineName: string;

implementation

uses {$IFDEF RAD9PLUS}
     System.Win.Registry;
     {$ELSE}
     Registry;
     {$ENDIF}

const
  KEY_WOW64_64KEY        = $0100;

function WideToAnsi(const ws: WideString; codePage: Word = CP_ACP): AnsiString;
var
  l: integer;
  f: Cardinal;
begin
  f:=WC_COMPOSITECHECK or WC_DISCARDNS or WC_SEPCHARS or WC_DEFAULTCHAR;
  if codepage=CP_UTF8 then
    f:=0;
  if ws = '' then
    Result := ''
  else begin
    l := WideCharToMultiByte(codePage,f,@ws[1],-1,nil,0,nil,nil);
    SetLength(Result,l-1);
    if l>1 then
      WideCharToMultiByte(codePage,f,@ws[1],-1,@Result[1],l-1,nil,nil);
  end;
end;

function GetMachine :string;
var
  n :Cardinal;
  buf :PChar;
const
  rkMachine = {HKEY_LOCAL_MACHINE}'\SYSTEM\CurrentControlSet\Control\ComputerName\ComputerName';
    rvMachine = 'ComputerName';
begin
  n:=255;
  buf:=stralloc(n);
  GetComputerName(buf,n);
  result:=buf;
  strdispose(buf);
  with TRegistry.Create(KEY_READ or KEY_WOW64_64KEY) do begin
    rootkey:=HKEY_LOCAL_MACHINE;
    if OpenKey(rkMachine,False) then begin
      if ValueExists(rvMachine) then
        result:=ReadString(rvMachine);
      closekey;
    end;
    free;
  end;
end;

function IsAdminAccount;
var
  hAccessToken: THandle;
  ptgGroups: PTokenGroups;
  dwInfoBufferSize: Cardinal;
  psidAdministrators: PSID;
  x: Integer;
  bSuccess: BOOL;
  s: string;
begin
  if SameText(ADomain,Machinename) then
    s:=AName
  else
    s:=Format('%s@%s',[AName,ADomain]);
  GrantPrivilege('','',s,SE_SERVICE_LOGON_NAME,False);
  try
  Result:=False;
  bSuccess:=LogonUser(PChar(AName),PChar(ADomain),PChar(APassword),LOGON32_LOGON_SERVICE,LOGON32_PROVIDER_DEFAULT,hAccessToken);
  if bSuccess then begin
    GetTokenInformation(hAccessToken,TokenGroups,nil,0,dwInfoBufferSize);
    GetMem(ptgGroups, dwInfoBufferSize);
    bSuccess:=GetTokenInformation(hAccessToken,TokenGroups,ptgGroups,dwInfoBufferSize,dwInfoBufferSize);

    CloseHandle(hAccessToken);
    if bSuccess then begin
      AllocateAndInitializeSid(SECURITY_NT_AUTHORITY, 2,
        SECURITY_BUILTIN_DOMAIN_RID, DOMAIN_ALIAS_RID_ADMINS,
        0, 0, 0, 0, 0, 0, psidAdministrators);
      {$R-}
      for x:=0 to ptgGroups.GroupCount - 1 do
        if EqualSid(psidAdministrators, ptgGroups.Groups[x].Sid) then begin
          Result:=True;
          Break;
        end;
      {$R+}
      FreeSid(psidAdministrators);
    end;
    FreeMem(ptgGroups);
  end;
  finally
    GrantPrivilege('','',s,SE_SERVICE_LOGON_NAME,True);
  end;
end;

function GetPrivilegeDisplayName;
var
 languageID : cardinal;
 buffer : array [0..256] of char;
 bufSize : cardinal;
begin
 bufSize := sizeof (buffer);
 if LookupPrivilegeDisplayName(PChar(aComputername), PChar (privilege), buffer, bufSize, languageID) then
   result := buffer
 else
   result := privilege;
//    raise ENTException.CreateLastError;
end;

procedure GetObjectAccessRights;
var
 sid : PSID;
 sidSize : Cardinal;
 sidNameUse : SID_NAME_USE;
 domainName : array [0..256] of char;
 domainNameSize : Cardinal;
 lsaComputerName : TLsaUnicodeStr;
 lsaRights, p : PLsaUnicodeString;
 objectAttributes : TLsaObjectAttributes;
 policyHandle : LSA_HANDLE;
 status : NTSTATUS;
 rightsCount, i, err : Cardinal;

begin
 list.Clear;
 sidSize := 65536;
 GetMem (sid, sidSize);
 try
   domainNameSize:=256;
   if LookupAccountName (PChar (aComputername), PChar (objectName), sid, sidSize, domainName, domainNameSize, sidNameUse) then
   begin
     ReallocMem (sid, sidSize);
     lsaComputerName := TLsaUnicodeStr.CreateFromStr (aComputername);
     try
       FillChar (objectAttributes, sizeof (objectAttributes), 0);
       status := LsaOpenPolicy (lsaComputername.Value, ObjectAttributes, POLICY_LOOKUP_NAMES, PolicyHandle);
       if status = STATUS_SUCCESS then
       try
         status := LsaEnumerateAccountRights (PolicyHandle, sid, lsaRights, rightsCount);
         if status = STATUS_SUCCESS then
         try
           p := lsaRights;
           for i := 0 to rightsCount - 1 do
           begin
             list.Add (LsaUnicodeStringToStr (p^));
             Inc (p)
           end
         finally
           LsaFreeMemory (lsaRights)
         end
         else
         begin
           err := LsaNTStatusToWinError (status);
           if err <> 2 then
             raise ENTException.Create(IntToStr(err))
         end
       finally
         LsaClose (PolicyHandle)
       end
       else raise ENTException.Create(IntToStr(LsaNTStatusToWinError (status)));
     finally
        lsaComputerName.Free
     end
   end
   else raise ENTException.Create(IntToStr(GetLastError));
 finally
   FreeMem (sid)
 end
end;

procedure GrantPrivilege;
var
 sid : PSID;
 sidSize: Cardinal;
 sidNameUse: SID_NAME_USE;
 domainNameSize : Cardinal;
 domainName : array [0..256] of char;
 attributes : TLsaObjectAttributes;
 policyHandle : LSA_HANDLE;
 lsaComputerName, rightsLsaUnicodeString : TLSAUnicodeStr;
 status : NTStatus;
begin
 sidSize := 65536;
 GetMem (sid, sidSize);
 domainNameSize := 256;
 try
   if LookupAccountName (PChar (accountServer), PChar (user), sid, sidSize, domainName, domainNameSize, sidNameUse) then begin
     lsaComputerName := TLsaUnicodeStr.CreateFromStr (aComputername);
     try
       FillChar (attributes, SizeOf (attributes), 0);
       status := LsaOpenPolicy (lsaComputerName.value, attributes, POLICY_CREATE_ACCOUNT or POLICY_LOOKUP_NAMES, policyHandle);
       if status = STATUS_SUCCESS then
       try
         rightsLsaUnicodeString := TLsaUnicodeStr.CreateFromStr (privilege);
         try
           if bRemove then
             status := LsaRemoveAccountRights (PolicyHandle, sid, False, @rightsLsaUnicodeString.value, 1)
           else
             status := LsaAddAccountRights (PolicyHandle, sid, @rightsLsaUnicodeString.value, 1);

           if status <> STATUS_SUCCESS then
             raise ENTException.Create (IntToStr(LsaNtStatusToWinError (status)));
         finally
           rightsLsaUnicodeString.Free
         end
       finally
         LsaClose (PolicyHandle)
       end
     finally
       lsaComputerName.Free
     end
   end
 finally
   FreeMem (sid)
 end
end;

function InternalGrantPermission(const ObjectType: DWORD; const ObjectName: String;
  const Entries: TGrantPermissionEntry; const EntryCount: Integer;
  const Inheritance: DWORD): DWORD;
type
  PPSID = ^PSID;
  PPACL = ^PACL;
  PTrusteeW = ^TTrusteeW;
  TTrusteeW = record
    pMultipleTrustee: PTrusteeW;
    MultipleTrusteeOperation: DWORD;  { MULTIPLE_TRUSTEE_OPERATION }
    TrusteeForm: DWORD;  { TRUSTEE_FORM }
    TrusteeType: DWORD;  { TRUSTEE_TYPE }
    ptstrName: PWideChar;
  end;
  TExplicitAccessW = record
    grfAccessPermissions: DWORD;
    grfAccessMode: DWORD;  { ACCESS_MODE }
    grfInheritance: DWORD;
    Trustee: TTrusteeW;
  end;
  PArrayOfExplicitAccessW = ^TArrayOfExplicitAccessW;
  TArrayOfExplicitAccessW = array[0..999999] of TExplicitAccessW;
const
  GRANT_ACCESS = 1;
  TRUSTEE_IS_SID = 0;
  TRUSTEE_IS_UNKNOWN = 0;
var
  AdvApiHandle: THandle;
  GetNamedSecurityInfoA: function(pObjectName: PAnsiChar; ObjectType: DWORD;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PPSID;
    ppDacl, ppSacl: PPACL; var ppSecurityDescriptor: PSECURITY_DESCRIPTOR): DWORD;
    stdcall;
  SetNamedSecurityInfoA: function(pObjectName: PAnsiChar; ObjectType: DWORD;
    SecurityInfo: SECURITY_INFORMATION; ppsidOwner, ppsidGroup: PSID;
    ppDacl, ppSacl: PACL): DWORD; stdcall;
  SetEntriesInAclW: function(cCountOfExplicitEntries: ULONG;
    const pListOfExplicitEntries: TExplicitAccessW; OldAcl: PACL;
    var NewAcl: PACL): DWORD; stdcall;
  SD: PSECURITY_DESCRIPTOR;
  Dacl, NewDacl: PACL;
  ExplicitAccess: PArrayOfExplicitAccessW;
  E: ^TGrantPermissionEntry;
  I: Integer;
  Sid: PSID;
begin
  if (Lo(GetVersion) < 5) then begin
    Result := ERROR_INVALID_FUNCTION;
    Exit;
  end;

  AdvApiHandle := GetModuleHandle(advapi32);
  GetNamedSecurityInfoA := GetProcAddress(AdvApiHandle, 'GetNamedSecurityInfoA');
  SetNamedSecurityInfoA := GetProcAddress(AdvApiHandle, 'SetNamedSecurityInfoA');
  SetEntriesInAclW := GetProcAddress(AdvApiHandle, 'SetEntriesInAclW');
  if (@GetNamedSecurityInfoA = nil) or (@SetNamedSecurityInfoA = nil) or
     (@SetEntriesInAclW = nil) then begin
    Result := ERROR_PROC_NOT_FOUND;
    Exit;
  end;

  ExplicitAccess := nil;
  Result := GetNamedSecurityInfoA(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(ObjectName)), ObjectType, DACL_SECURITY_INFORMATION,
    nil, nil, @Dacl, nil, SD);
  if Result <> ERROR_SUCCESS then
    Exit;
  try
    ExplicitAccess := AllocMem(EntryCount * SizeOf(ExplicitAccess[0]));
    E := @Entries;
    for I := 0 to EntryCount-1 do begin
      if not AllocateAndInitializeSid(E.Sid.Authority, E.Sid.SubAuthCount,
         E.Sid.SubAuth[0], E.Sid.SubAuth[1], 0, 0, 0, 0, 0, 0, Sid) then begin
        Result := GetLastError;
        if Result = ERROR_SUCCESS then  { just in case... }
          Result := ERROR_INVALID_PARAMETER;
        Exit;
      end;
      ExplicitAccess[I].grfAccessPermissions := E.AccessMask;
      ExplicitAccess[I].grfAccessMode := GRANT_ACCESS;
      ExplicitAccess[I].grfInheritance := Inheritance;
      ExplicitAccess[I].Trustee.TrusteeForm := TRUSTEE_IS_SID;
      ExplicitAccess[I].Trustee.TrusteeType := TRUSTEE_IS_UNKNOWN;
      PSID(ExplicitAccess[I].Trustee.ptstrName) := Sid;
      Inc(E);
    end;
    Result := SetEntriesInAclW(EntryCount, ExplicitAccess[0], Dacl, NewDacl);
    if Result <> ERROR_SUCCESS then
      Exit;
    try
      Result := SetNamedSecurityInfoA(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(ObjectName)), ObjectType,
        DACL_SECURITY_INFORMATION, nil, nil, NewDacl, nil);
    finally
      LocalFree(HLOCAL(NewDacl));
    end;
  finally
    if Assigned(ExplicitAccess) then begin
      for I := EntryCount-1 downto 0 do begin
        Sid := PSID(ExplicitAccess[I].Trustee.ptstrName);
        if Assigned(Sid) then
          FreeSid(Sid);
      end;
      FreeMem(ExplicitAccess);
    end;
    LocalFree(HLOCAL(SD));
  end;
end;

const
  OBJECT_INHERIT_ACE    = 1;
  CONTAINER_INHERIT_ACE = 2;

function PathExpand(const Filename: String): String;
var
  Res: Integer;
  FilePart: PChar;
  Buf: array[0..4095] of Char;
begin
  Cardinal(Res) := GetFullPathName(PChar(Filename), SizeOf(Buf), Buf, FilePart);
  if (Res > 0) and (Res < SizeOf(Buf)) then
    SetString(Result, Buf, Res)
  else
    Result := Filename;
end;

function GrantPermissionOnFile;
const
  SE_FILE_OBJECT = 1;
var
  Attr, Inheritance, ErrorCode: DWORD;
begin
  Filename := PathExpand(Filename);
  Attr := GetFileAttributes(PChar(FileName));
  if Attr = $FFFFFFFF then begin
    Result := False;
    Exit;
  end;
  if Attr and FILE_ATTRIBUTE_DIRECTORY <> 0 then
    Inheritance := OBJECT_INHERIT_ACE or CONTAINER_INHERIT_ACE
  else
    Inheritance := 0;
  ErrorCode := InternalGrantPermission(SE_FILE_OBJECT, Filename, Entries,
    EntryCount, Inheritance);
  SetLastError(ErrorCode);
  Result := (ErrorCode = ERROR_SUCCESS);
end;

initialization
  MachineName:=GetMachine;

end.





{*******************************************************}
{               MiTeC Common Routines                   }
{                  System routines                      }
{                                                       }
{                                                       }
{       Copyright (c) 1997-2019 Michal Mutl             }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_SysUtils;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.Classes, System.SysUtils, WinAPI.ShlObj, System.Win.Registry,
     System.Variants, Vcl.Graphics,
     {$ELSE}
     Windows, Classes, SysUtils, ShlObj, Variants, Registry,
     Graphics,
     {$ENDIF}
     MiTeC_Windows, MiTeC_NetAPI32, MiTeC_WtsApi32;

type
  TConnectionType = (ctNone, ctLAN, ctDialup);

  TSessionProtocol = (spNone = -1, spConsole, spICA, spRDP);

const
  cERROR_BUFFER_TOO_SMALL =  603;
  cRAS_MaxEntryName       =  256;
  cRAS_MaxDeviceName      =  128;
  cRAS_MaxDeviceType      =  16;

  LOGON32_LOGON_NEW_CREDENTIALS   = 9;

  PROCESS_QUERY_LIMITED_INFORMATION = $1000;
  SeDebugPrivilege = 20;


type
  HRASConn = Cardinal;
  PRASConn = ^TRASConn;
  TRASConn = record
    dwSize: Cardinal;
    rasConn: HRASConn;
    szEntryName: Array[0..cRAS_MaxEntryName] Of Char;
    szDeviceType : Array[0..cRAS_MaxDeviceType] Of Char;
    szDeviceName : Array [0..cRAS_MaxDeviceName] of char;
  end;

  TRasEnumConnections =  function (RASConn: PrasConn;     { buffer to receive Connections data }
                                   var BufSize: Cardinal;    { size in bytes of buffer }
                                   var Connections: Cardinal { number of Connections written to buffer }
                                   ): LongInt; stdcall;

const
  cSessionProto: array[TSessionProtocol] of string = ('None','Console','ICA','RDP');

function GetProcessID(const AName: string; var ASession: Cardinal): Cardinal;
function GetAnyUserProcess(const AUserName: string; var ASession: Cardinal): Cardinal;
function GetProcessName(APID: cardinal): string;
function GetProcessFileName(APID: cardinal): string;
function GetParentProcessID(APID: Cardinal): Cardinal;
function GetProcessCommandLine(APID: cardinal): string;
function GetLoggedUser(ASessionID: Cardinal = WTS_CURRENT_SESSION): string;
function GetSessionProcessID(const AName: string; ASession: Cardinal): Cardinal;
function GetProcessSessionID(APID: Cardinal): Cardinal;
function GetDomain(AHost: string = ''): string;
function GetJoinInfo(var AName: string): TNetSetupJoinStatus;
function IsInDomain: Boolean;
function GetSpecialFolderRegEx(const AName: string; ASession: Cardinal = WTS_CURRENT_SESSION): string;
function GetProfilePathEx(ASession: Cardinal = WTS_CURRENT_SESSION): string;
procedure GetClient(const AKey: string; var AName,APath: string; ASession: Cardinal = WTS_CURRENT_SESSION);
procedure GetURLAssoc(const AKey: string; var AName,APath: string; ASession: Cardinal = WTS_CURRENT_SESSION);
function GetConnectionType(ASession: Cardinal = WTS_CURRENT_SESSION): TConnectionType;
function GetProxyServer(ASession: Cardinal = WTS_CURRENT_SESSION): string;
function GetLocalIP: string;
//function IsTSRemoteSession(ASessionID: Cardinal = WTS_CURRENT_SESSION): Boolean;
function GetTSSessionCount(AServerHandle: Cardinal = WTS_CURRENT_SERVER_HANDLE): Cardinal;
function GetTSSessionProto(ASessionID: Cardinal = WTS_CURRENT_SESSION; AServerHandle: Cardinal = WTS_CURRENT_SERVER_HANDLE): Integer;
function GetSessionID: Cardinal;
function GetLoggedUserSessionID: Cardinal;
function GetActiveConsoleSessionID: Cardinal;
function IsTerminalServicesEnabled: Boolean;
function RasConnectionCount: Integer;
function ImpersonateAsLoggedUser(SessionID: Cardinal): Boolean;
function ImpersonateAsUser(const User,Domain,Pwd: string): Boolean;
function ImpersonateAsUserEx(const User,Domain,Pwd: string): Boolean;
procedure RevertImpersonation;
function CreateProcessAsLoggedUser(const AFilename: string; SessionID: Cardinal): Cardinal;
function CreateProcessAsLoggedUserEx(const AFilename: string; APID: Cardinal): Cardinal;
procedure GetAccountList(AList: TStrings);
function SetDebugPriv: Boolean;
function RetrieveUserAccountPicture(APicture: TBitmap): boolean;

{var
  ProfilePath, JoinName: string;}

implementation

uses {$IFDEF RAD9PLUS}
     System.StrUtils, System.DateUtils, WinAPI.TlHelp32, WinAPI.ActiveX, System.Math,
     WinAPI.Winsock, WinAPI.PSAPI, XML.XMLDoc, XML.XMLIntf, XML.xmldom, Soap.EncdDecd,
     Vcl.Imaging.jpeg, Vcl.Imaging.pngimage,
     {$IFDEF RAD17PLUS}System.NetEncoding,{$ENDIF}
     {$ELSE}
     StrUtils, DateUtils,
     {$IFDEF FPC}Base64, JwaTlHelp32, jwaPSAPI, xmlread, dom{$ELSE}EncdDecd, TlHelp32, PSAPI, XMLDoc, XMLIntf, XMLDom{$ENDIF},
     ActiveX, Math, Winsock, {$IFNDEF FPC}JPEG,{$ENDIF} {$IFDEF RAD7PLUS}PngImage,{$ENDIF}
     {$ENDIF}
     MiTeC_Routines, MiTeC_NativeAPI, MiTeC_NativeDefs, MiTeC_StrUtils, MiTeC_IPTypes, MiTeC_RegUtils;

var
  imp_ph, imp_th, imp_th_dup: THandle;

function GetProcessID(const AName: string; var ASession: Cardinal): Cardinal;
var
  Buffer: Pointer;
  status,sid: Cardinal;
  pspi: PSystemProcessInformation;
  n,br :Cardinal;
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
begin
  Result:=Cardinal(-1);
  if ASession=Cardinal(-1) then begin
    ProcessIdToSessionId(GetCurrentProcessID,sid);
    ASession:=sid;
  end;
  if OS>os2K then begin
    ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
    if (ps<>INVALID_HANDLE_VALUE) then
      try
        pe32.dwSize:=sizeof(TPROCESSENTRY32);
        ok:=Process32First(ps,pe32);
        while ok do begin
          if SameText(AName,ExtractFilename(string(pe32.szExeFile))) or SameText(GetProcessFileName(pe32.th32ProcessID),AName) then begin
            ProcessIdToSessionId(pe32.th32ProcessID,sid);
            if sid=ASession then begin
              Result:=pe32.th32ProcessID;
              Break;
            end;
          end;
          ok:=Process32Next(ps,pe32);
        end;
      finally
        CloseHandle(ps);
      end;
  end;
  if (OS<osXP) or (Result=INVALID_HANDLE_VALUE) then begin
    InitNativeAPI;
    Buffer:=nil;
    n:=$100;
    Buffer:=AllocMem(n*SizeOf(TSystemProcessInformation));
    status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,n*SizeOf(TSystemProcessInformation),@br);
    while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
      n:=Max(br,n*2);
      ReallocMem(Buffer,n*SizeOf(TSystemProcessInformation));
      status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,n*SizeOf(TSystemProcessInformation),@br);
    end;
    try
      if status=STATUS_SUCCESS then begin
        pspi:=PSystemProcessInformation(Buffer);
        repeat
          if (SameText(GetProcessName(pspi.ProcessId),AName) or SameText(GetProcessFileName(pspi.ProcessId),AName)) and (pspi.SessionId=ASession) then begin
            Result:=pspi.ProcessId;
            Break;
          end;
          if pspi^.NextEntryDelta=0 then
            Break;
          pspi:=PSystemProcessInformation(PAnsiChar(pspi)+pspi^.NextEntryDelta);
        until False;
      end;
    finally
      Reallocmem(Buffer,0);
      Buffer:=nil;
      FreeNativeAPI;
    end;
  end;
end;

function GetAnyUserProcess;
var
  Buffer: Pointer;
  sid,status: Cardinal;
  pspi: PSystemProcessInformation;
  n,br :Cardinal;
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
  u,d: string;
begin
  ps:=INVALID_HANDLE_VALUE;
  if ASession=Cardinal(-1) then begin
    ProcessIdToSessionId(GetCurrentProcessID,sid);
    ASession:=sid;
  end;
  Result:=Cardinal(-1);
  if OS>os2K then begin
    ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
    if (ps<>INVALID_HANDLE_VALUE) then
      try
        pe32.dwSize:=sizeof(TPROCESSENTRY32);
        ok:=Process32First(ps,pe32);
        while ok do begin
          if GetProcessUserNameFromPID(pe32.th32ProcessID,u,d) and SameText(u,AUsername) then begin
            ProcessIdToSessionId(pe32.th32ProcessID,sid);
            if sid=ASession then begin
              Result:=pe32.th32ProcessID;
              Break;
            end;
          end;
          ok:=Process32Next(ps,pe32);
        end;
      finally
        CloseHandle(ps);
      end;
  end;
  if (OS<osXP) or (ps<>INVALID_HANDLE_VALUE) then begin
    InitNativeAPI;
    Buffer:=nil;
    n:=$100;
    Buffer:=AllocMem(n*SizeOf(TSystemProcessInformation));
    status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,n*SizeOf(TSystemProcessInformation),@br);
    while (status=STATUS_BUFFER_OVERFLOW) or (status=STATUS_INFO_LENGTH_MISMATCH) do begin
      n:=Max(br,n*2);
      ReallocMem(Buffer,n*SizeOf(TSystemProcessInformation));
      status:=NtQuerySystemInformation(SystemProcessInformation,Buffer,n*SizeOf(TSystemProcessInformation),@br);
    end;
    try
      if status=STATUS_SUCCESS then begin
        pspi:=PSystemProcessInformation(Buffer);
        repeat
          if GetProcessUserNameFromPID(pspi.ProcessId,u,d) and SameText(u,AUsername) and (pspi.SessionId=ASession) then begin
            Result:=pspi.ProcessId;
            Break;
          end;
          if pspi^.NextEntryDelta=0 then
            Break;
          pspi:=PSystemProcessInformation(PAnsiChar(pspi)+pspi^.NextEntryDelta);
        until False;
      end;
    finally
      Reallocmem(Buffer,0);
      Buffer:=nil;
      FreeNativeAPI;
    end;
  end;
end;

function GetProcessCommandLine;
var
  pbi: PROCESS_BASIC_INFORMATION;
  pb: TPEB;
  ib: TProcessParameters;
  pwc: PWideChar;
  n: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  ph: THandle;
begin
  Result:='';
  ph:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,APID);
  if (ph<>INVALID_HANDLE_VALUE) and (ph<>0) then
    try
      if NtQueryInformationProcess(ph,ProcessBasicInformation,@pbi,SizeOf(pbi),@n)=0 then
        if ReadProcessMemory(ph,pbi.PebBaseAddress,@pb,SizeOf(pb),n) then
           if ReadProcessMemory(ph,Pointer(pb.ProcessParameters),@ib,SizeOf(ib),n) then begin
             pwc:=AllocMem(MAX_PATH+1);
             try
               if ReadProcessMemory(ph,Pointer(ib.CommandLine.Buffer),pwc,MAX_PATH,n) then
                 Result:=WideCharToString(pwc);
             finally
               FreeMem(pwc);
             end;
           end;

    finally
      CloseHandle(ph);
    end;
end;

function GetLoggedUser;
var
  pid,n: Cardinal;
  hProc: THandle;
  dm,user: Pointer;
  dom: string;
begin
  dm:=nil;
  user:=nil;
  Result:='';
  if WTSAvailable then begin
    if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,ASessionID,WTSUserName,user,n) then begin
      Result:=PChar(user);
      WTSFreeMemory(user);
    end;
    if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,ASessionID,WTSDomainName,dm,n) then begin
      dom:=PChar(dm);
      WTSFreeMemory(dm);
    end;
  end;
  if Result='' then begin
    if ASessionID=Cardinal(-1) then begin
      if IsRemoteSession then
        ASessionID:=GetSessionID
      else
        ASessionID:=GetActiveConsoleSessionID;
      if ASessionID=Cardinal(-1) then begin
        if Win32majorversion<6 then
          ASessionID:=0
        else
          ASessionID:=1;
      end;
    end;
    pid:=GetProcessID('explorer.exe',ASessionID);

    hProc:=OpenProcess(PROCESS_ALL_ACCESS,False,pid);
    try
      GetProcessUsername(hProc,Result,dom);
    finally
      CloseHandle(hProc);
    end;
  end;
  if Result='' then
    Result:=GetUser;
  if dom='' then
    dom:=GetDomain;
  if (dom<>'') and not SameText(dom,GetMachine) and (Result<>'') then begin
    if Pos('S-',GetSIDFromAccount('',Result+'@'+dom))=1 then
      Result:=Result+'@'+dom;
  end;
end;

function GetProcessName(APID: cardinal): string;
var
  h: THandle;
begin
  Result:='';
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      SetLength(Result,MAX_PATH);
      if GetModuleBaseName(h,0,PChar(Result),MAX_PATH)>0 then
        SetLength(Result,StrLen(PChar(Result)))
      else
        Result:='';
    finally
      CloseHandle(h);
    end;
end;

function GetProcessFileName(APID: cardinal): string;
var
  h: THandle;
begin
  Result:='';
  h:=OpenProcess(PROCESS_QUERY_INFORMATION or PROCESS_VM_READ,False,APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      SetLength(Result,MAX_PATH);
      if GetModuleFileNameEx(h,0,PChar(Result),MAX_PATH)>0 then
        SetLength(Result,StrLen(PChar(Result)))
      else
        Result:='';
    finally
      CloseHandle(h);
    end;
end;

function GetParentProcessID;
var
  pbi: TProcessBasicInformation;
  h: THandle;
  n,r: Cardinal;
begin
  Result:=0;
  h:=OpenProcess(PROCESS_QUERY_INFORMATION,False,APID);
  if (h<>0) and (h<>INVALID_HANDLE_VALUE) then
    try
      r:=NtQueryInformationProcess(h,ProcessBasicInformation,@pbi,SizeOf(pbi),@n);
      if r=0 then
        Result:=pbi.InheritedFromUniqueProcessId;
    finally
      CloseHandle(h);
    end;
end;

function GetSpecialFolderRegEx;
const
  rkSF = '\Software\Microsoft\Windows\CurrentVersion\Explorer\Shell Folders';
var
  rk: string;
begin
  Result:='';
  with OpenRegistryReadOnly do
    try
      RootKey:=HKEY_USERS;
      rk:=GetSIDFromAccount('',GetLoggedUser(ASession))+rkSF;
      if OpenKey(rk,False) then begin
        if ValueExists(Aname) then
          Result:=ReadString(AName);
        CloseKey;
      end;
      if (Result<>'') then
        Exit;
      RootKey:=HKEY_LOCAL_MACHINE;
      rk:=rkSF;
      if OpenKey(rk,False) then begin
        if ValueExists(Aname) then
          Result:=ReadString(AName);
        CloseKey;
      end;
    finally
      Free;
    end;
end;

function GetProfilePathEx;
const
  rkPF = {HKEY_LOCAL_MACHINE}'\SOFTWARE\Microsoft\Windows NT\CurrentVersion\ProfileList\%s';
  rvPIP = 'ProfileImagePath';
var
  s: string;
begin
  Result:='';
  if Win32Platform=VER_PLATFORM_WIN32_NT then begin
    with OpenRegistryReadOnly do
      try
        RootKey:=HKEY_LOCAL_MACHINE;
        s:=Format(rkPF,[GetSIDFromAccount('',GetLoggedUser(ASession))]);
        if OpenKey(s,False)then begin
          if ValueExists(rvPIP) then
            Result:=ExpandEnvVars(ReadString(rvPIP),False);
          CloseKey;
        end;
      finally
        Free;
      end;
  end;
  if Result='' then begin
    s:=GetSpecialFolder(GetDesktopWindow,CSIDL_DESKTOP);
    s:=ReverseString(s);
    Result:=ReverseString(Copy(s,Pos('\',s)+1,255));
  end;
end;

procedure GetClient;
const
  rkClients = '\SOFTWARE\Clients\';
var
  u,a,s,rk: string;
begin
  AName:='';
  APath:='';
  with TRegistry.Create do
    try
      RootKey:=HKEY_USERS;
      u:=GetLoggedUser(ASession);
      a:=GetSIDFromAccount('',u);
      rk:=IncludeTrailingPathdelimiter(a+rkClients+AKey);
      if (u='') or (a='') then begin
        RootKey:=HKEY_CURRENT_USER;
        rk:=IncludeTrailingPathdelimiter(rkClients+AKey);
      end;
      if OpenkeyReadOnly(rk) then begin
        s:='';
        try s:=ReadString('') except end;
        CloseKey;
        if s<>'' then begin
          if OpenkeyReadOnly(rk+s) then begin
            try
              s:=ReadString('');
              if s<>'' then
                AName:=s;
            except end;
            CloseKey;
          end;
          if OpenkeyReadOnly(rk+s+'\shell\open\command') then begin
            try
              s:=ReadString('');
              if s<>'' then
                APath:=ExtractFilenameFromStr(s);
            except end;
            CloseKey;
          end;
        end;
      end;

      RootKey:=HKEY_LOCAL_MACHINE;
      rk:=IncludeTrailingPathdelimiter(rkClients+AKey);
      if Openkey(rk,False) then begin
        if s='' then
          try s:=ReadString('') except end;
        CloseKey;
        if s<>'' then begin
          if OpenkeyReadOnly(rk+s) then begin
            try AName:=ReadString('') except end;
            CloseKey;
          end;
          if OpenkeyReadOnly(rk+s+'\shell\open\command') then begin
            try APath:=ExtractFilenameFromStr(ReadString('')) except end;
            CloseKey;
          end;
        end;
      end;

    finally
      Free;
    end;
end;

procedure GetURLAssoc(const AKey: string; var AName,APath: string; ASession: Cardinal = WTS_CURRENT_SESSION);
const
  rkUA = '\Software\Microsoft\Windows\Shell\Associations\UrlAssociations\%s\UserChoice';
  rkApp = '\%s\Application';
  rkCmd = '\%s\shell\open\command';
var
  u,a,s,rk: string;
  p: Integer;
begin
  AName:='';
  APath:='';
  with TRegistry.Create do
    try
      RootKey:=HKEY_USERS;
      u:=GetLoggedUser(ASession);
      a:=GetSIDFromAccount('',u);
      rk:=IncludeTrailingPathdelimiter(a);
      if (u='') or (a='') then begin
        RootKey:=HKEY_CURRENT_USER;
        rk:='';
      end;
      s:='';
      rk:=rk+Format(rkUA,[AKey]);
      if OpenkeyReadOnly(rk) then begin
        try s:=ReadString('ProgId') except end;
        CloseKey;
      end;
      if s<>'' then begin
        RootKey:=HKEY_CLASSES_ROOT;
        rk:=Format(rkApp,[s]);
        if Openkey(rk,False) then begin
          try AName:=ReadString('AppUserModelID') except end;
          p:=Pos('!',AName);
          if p>0 then
            AName:=Copy(AName,p+1,255);
          CloseKey;
        end;

        rk:=Format(rkCmd,[s]);
        if Openkey(rk,False) then begin
          try APath:=ExtractFilenameFromStr(ReadString('')) except end;
          CloseKey;
        end;
      end;
      if (AName='') and FileExistsEx(APath) then
        AName:=GetFileDesc(APath);
    finally
      Free;
    end;
end;

function GetConnectionType;
const
  rkIS = {HKEY_CURRENT_USER}'\Software\Microsoft\Windows\CurrentVersion\Internet settings';
  rvProxy = 'ProxyEnable';
  rvProxyServer = 'ProxyServer';
var
  i: Cardinal;
  rk: string;
begin
  Result:=ctNone;
  with OpenRegistryReadOnly do
  try
    try
      RootKey:=HKEY_USERS;
      rk:=GetSIDFromAccount('',GetLoggedUser(ASession))+rkIS;
      if OpenKey(rk,False) then begin
        if GetDataType(rvProxy) = rdBinary then
          ReadBinaryData(rvProxy,i,SizeOf(Cardinal))
        else
          i:=Integer(ReadBool(rvProxy));
        if (i<>0) and (ReadString(rvProxyServer)<>'') then
          Result:=ctLAN;
        CloseKey;
      end;
    except

    end;
  finally
    Free;
  end;

  if Result=ctNone then begin
    if RasConnectionCount>0 then
      Result:=ctDialup;
  end;
end;

function GetProxyServer;
const
  rkIS = {HKEY_CURRENT_USER}'\Software\Microsoft\Windows\CurrentVersion\Internet settings';
  rvProxyServer = 'ProxyServer';
var
  rk: string;
begin
  Result:='';
  with OpenRegistryReadOnly do begin
    RootKey:=HKEY_USERS;
    rk:=GetSIDFromAccount('',GetLoggedUser(ASession))+rkIS;
    if OpenKey(rk,False) then BEGIN
      Result:=ReadString(rvProxyserver);
      CloseKey;
    end;
    Free;
  end;
end;

function GetSessionProcessID;
var
  P: PWtsProcessInfo;
  n: Cardinal;
  i: Integer;
begin
  Result:=0;
  try
  if WTSEnumerateProcesses(WTS_CURRENT_SERVER_HANDLE,0,1,P,n) then begin
    {$R-}
    for i:=0 to n-1 do
      with PProcessInfoArray(P)^[i] do
        if (SessionID=ASession) and SameText(string(pProcessName),AName) then begin
          Result:=ProcessID;
          Break;
        end;
    {$R+}
    WTSFreeMemory(P);
  end;
  except

  end;
end;

function GetProcessSessionID;
var
  P: PWtsProcessInfo;
  n: Cardinal;
  i: Integer;
begin
  Result:=0;
  try
  if WTSEnumerateProcesses(WTS_CURRENT_SERVER_HANDLE,0,1,P,n) then begin
    {$R-}
    for i:=0 to n-1 do
      with PProcessInfoArray(P)^[i] do
        if (APID=ProcessID) then begin
          Result:=SessionID;
          Break;
        end;
    {$R+}
    WTSFreeMemory(P);
  end;
  except

  end;
end;

function GetLocalIP: string;
type
  TaPInAddr = array [0..255] of PInAddr;
  PaPInAddr = ^TaPInAddr;
var
  phe  :PHostEnt;
  pptr :PaPInAddr;
  Buffer :array [0..63] of ansichar;
  i :integer;
  GInitData :TWSADATA;
begin
  wsastartup($101,GInitData);
  result:='';
  GetHostName(Buffer,SizeOf(Buffer));
  phe:=GetHostByName(buffer);
  if not assigned(phe) then
    exit;
  pptr:=PaPInAddr(Phe^.h_addr_list);
  i:=0;
  while pptr^[I]<>nil do begin
    result:=Result+IPv4ToStr(InAddrToIPv4(pptr^[I]^))+',';
    inc(i);
  end;
  Delete(Result,Length(Result),1);
  wsacleanup;
end;

function GetDomain;
var
 wksta: Pointer;
 ec: cardinal;
 p: PWideChar;
begin
  Result:='';
  if not InitNetAPI then
    Exit;
  try
    if (Pos('\\',AHost)=0) then
      if Trim(AHost)='' then
        AHost:='\\.'
      else
        AHost:='\\'+AHost;

    if Trim(AHost)='' then
      ec:=NetWkstaGetInfo(nil,100,wksta)
    else begin
      p:=AllocMem(2*(Length(AHost)+1));
      StringToWideChar(AHost,p,2*(Length(AHost)+1));
      ec:=NetWkstaGetInfo(p,100,wksta);
      FreeMem(p);
    end;
    if (ec<>NERR_Success) then
      Exit;
    try
      with PWKSTA_INFO_100(wksta)^ do begin
        Result:=UpperCase(WideCharToString(wksi100_langroup));
      end;
    except

    end;
  finally
    NetApiBufferFree(wksta);
  end;
end;

function GetJoinInfo;
var
  lpNameBuffer: PWideChar;
begin
  Result:=NetSetupUnknownStatus;
  try
    if not Assigned(NetGetJoinInformation) then
      Exit;
    Result:=NetSetupUnknownStatus;
    if NetGetJoinInformation(nil,lpNameBuffer,Result)=ERROR_SUCCESS then begin
      Aname:=lpnameBuffer;
      NetApiBufferFree(lpNameBuffer);
    end;
  finally
  end;
end;

function IsInDomain;
var
  d: string;
begin
  d:=GetDomain;
  Result:=GetJoinInfo(d)=NetSetupDomainName;
end;

{function IsTSRemoteSession;
begin
  Result:=GetSystemMetrics(SM_REMOTESESSION)>0;
  if not Result and ISNT and WTSAvailable then begin
    if ASessionID=WTS_CURRENT_SESSION then
      Result:=GetActiveConsoleSessionId<>GetSessionID
    else
      Result:=GetActiveConsoleSessionId<>ASessionID;
  end;
end;}

function GetTSSessionCount;
var
  S: PWtsSessionInfo;
  n: Cardinal;
begin
  Result:=0;
  try
  if WTSEnumerateSessions(AServerHandle,0,1,S,n) then begin
    Result:=n;
    WTSFreeMemory(S);
  end;
  except

  end;
end;

function GetTSSessionProto;
var
  Buf: Pointer;
  n: Cardinal;
begin
  Result:=-1;
  try
  if WTSQuerySessionInformation(AServerHandle,ASessionID,WTSClientProtocolType,buf,n) then begin
    Result:=PByte(Buf)^;
    WTSFreeMemory(Buf);
  end;
  except

  end;
end;

function GetSessionID;
var
  Buf: Pointer;
  n,id: Cardinal;
begin
  id:=0;
  Result:=WTS_CURRENT_SESSION;
  try
    if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,WTS_CURRENT_SESSION,WTSSessionID,buf,n) then begin
      Result:=PDWORD(Buf)^;
      WTSFreeMemory(Buf);
    end else begin
      ProcessIDToSessionID(GetCurrentProcessId,id);
      Result:=UINT(id);
    end;
  except

  end;
end;

function GetActiveConsoleSessionID;
var
  S: PWtsSessionInfo;
  d,n: Cardinal;
  i: Integer;
  Buf: Pointer;
begin
  try
    Result:=WTSGetActiveConsoleSessionId;
  except
    Result:=0;
    if WTSAvailable and WTSEnumerateSessions(WTS_CURRENT_SERVER_HANDLE,0,1,S,n) then begin
      {$R-}
      for i:=0 to n-1 do
        with PSessionInfoArray(S)^[i] do
          if WTSQuerySessionInformation(WTS_CURRENT_SERVER_HANDLE,SessionId,WTSClientAddress,Buf,d) and (Buf=nil) then begin
            Result:=SessionId;
            Break;
          end;
      {$R+}
      WTSFreeMemory(S);
    end;
  end;
end;

function GetLoggedUserSessionID;
begin
  if GetSystemMetrics(SM_REMOTESESSION)>0 then
    Result:=GetSessionID
  else
    Result:=GetActiveConsoleSessionID;
end;

function IsTerminalServicesEnabled;
var
  osVersionInfo: TOSVERSIONINFOEX;
  dwlConditionMask: {$IFDEF RAD9PLUS}Uint64{$ELSE}Int64{$ENDIF};
begin
  {$IFNDEF RAD9PLUS}
  Result:=False;
  if not Assigned(VerSetConditionMask) or not Assigned(VerifyVersionInfo) then
    Exit;
  {$ENDIF}
  FillChar(osVersionInfo,SizeOf(osVersionInfo),0);
  osVersionInfo.dwOSVersionInfoSize:=sizeof(osVersionInfo);
  osVersionInfo.wSuiteMask:=VER_SUITE_TERMINAL;
  dwlConditionMask:=0;
  dwlConditionMask:=VerSetConditionMask(dwlConditionMask,VER_SUITENAME,VER_AND);
  Result:=VerifyVersionInfo(osVersionInfo,VER_SUITENAME,dwlConditionMask);
end;

function RasConnectionCount: Integer;
var
  RasDLL: HInst;
  Conns: Array[1..4] of TRasConn;
  RasEnums: TRasEnumConnections;
  BufSize: Cardinal;
  NumConns: Cardinal;
  RasResult: Longint;
begin
  Result:=0;
  RasDLL:=LoadLibrary('rasapi32.dll');
  if RasDLL<>0 then
    try
      RasEnums:=GetProcAddress(RasDLL,'RasEnumConnectionsA');
      if @RasEnums<>nil then begin
        Conns[1].dwSize:=Sizeof(Conns[1]);
        BufSize:=SizeOf(Conns);
        RasResult:=RasEnums(PRASConn(@Conns),BufSize,NumConns);
        if (RasResult=0) or (Result=cERROR_BUFFER_TOO_SMALL) then
          Result:=NumConns;
      end;
  finally
    FreeLibrary(RasDLL);
  end;
end;

function ImpersonateAsLoggedUser;
var
  pid: Cardinal;
begin
  Result:=False;
  if imp_th>0 then
    Exit;
  Result:=False;
  if IsRemoteSession then
    pid:=GetSessionProcessID('explorer.exe',SessionID)
  else
    pid:=GetProcessID('explorer.exe',SessionID);
  imp_ph:=OpenProcess(PROCESS_ALL_ACCESS,True,PID);
  if OpenProcessToken(imp_ph,TOKEN_ALL_ACCESS,imp_th) then
    Result:=ImpersonateLoggedOnUser(imp_th);
end;

function ImpersonateAsUser;
begin
  Result:=False;
  if LogonUser(PChar(User),PChar(Domain),PChar(Pwd),LOGON32_LOGON_INTERACTIVE,LOGON32_PROVIDER_DEFAULT,imp_th) then
    Result:=DuplicateToken(imp_th,SecurityImpersonation,@imp_th_dup) and ImpersonateLoggedOnUser(imp_th_dup);
end;

function ImpersonateAsUserEx;
begin
  Result:=False;
  if LogonUser(PChar(User),PChar(Domain),PChar(Pwd),LOGON32_LOGON_NEW_CREDENTIALS,LOGON32_PROVIDER_DEFAULT,imp_th) then
    Result:=DuplicateToken(imp_th,SecurityImpersonation,@imp_th_dup) and ImpersonateLoggedOnUser(imp_th_dup);
end;

function CreateProcessAsLoggedUser;
var
  th,ph,pid: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
begin
  Result:=0;
  pid:=GetProcessID('explorer.exe',SessionID);
  ph:=OpenProcess(PROCESS_ALL_ACCESS,True,PID);
  if OpenProcessToken(ph,TOKEN_ALL_ACCESS,th) then begin
    if ImpersonateLoggedOnUser(th) then
      try
        ZeroMemory(@si,sizeof(STARTUPINFO));
        si.cb:=sizeof(STARTUPINFO);
        si.lpDesktop:=PChar('winsta0\default');
        if CreateProcessAsUser(th,PChar(AFilename),nil,nil,nil,False,
                            NORMAL_PRIORITY_CLASS or CREATE_NEW_CONSOLE,nil,nil,{$IFDEF FPC}@{$ENDIF}si,{$IFDEF FPC}@{$ENDIF}pi)
           and (pi.hProcess<>INVALID_HANDLE_VALUE) then begin
          Result:=pi.dwProcessId;
          Closehandle(pi.hProcess);
          Closehandle(pi.hThread);
        end;
      finally
        RevertToSelf;
        Closehandle(th);
      end;
    Closehandle(ph);
  end;
end;

function CreateProcessAsLoggedUserEx;
var
  th,ph: {$IFDEF NATIVEINT}NativeUInt{$ELSE}Cardinal{$ENDIF};
  si: STARTUPINFO;
  pi: PROCESS_INFORMATION;
begin
  Result:=0;
  ph:=OpenProcess(PROCESS_ALL_ACCESS,True,APID);
  if OpenProcessToken(ph,TOKEN_ALL_ACCESS,th) then begin
    if ImpersonateLoggedOnUser(th) then
      try
        ZeroMemory(@si,sizeof(STARTUPINFO));
        si.cb:=sizeof(STARTUPINFO);
        si.lpDesktop:=PChar('winsta0\default');
        if CreateProcessAsUser(th,PChar(AFilename),nil,nil,nil,False,
                            NORMAL_PRIORITY_CLASS or CREATE_NEW_CONSOLE,nil,nil,{$IFDEF FPC}@{$ENDIF}si,{$IFDEF FPC}@{$ENDIF}pi)
           and (pi.hProcess<>INVALID_HANDLE_VALUE) then begin
          Result:=pi.dwProcessId;
          Closehandle(pi.hProcess);
          Closehandle(pi.hThread);
        end;
      finally
        RevertToSelf;
        Closehandle(th);
      end;
    Closehandle(ph);
  end;
end;

procedure RevertImpersonation;
begin
  RevertToSelf;
  Closehandle(imp_ph);
  Closehandle(imp_th_dup);
  Closehandle(imp_th);
  imp_ph:=0;
  imp_th:=0;
  imp_th_dup:=0;
end;

procedure GetAccountList;
var
  i: Cardinal;
  nStatus: NET_API_STATUS;
  uTmpBuf,uBuf: PUSER_INFO_3;
  gTmpBuf,gBuf: PLOCALGROUP_INFO_1;
  xTmpBuf,xBuf: PGROUP_INFO_2;
  Loop: Boolean;
  dwLevel: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Name: string;
  Sid: string;
begin
  AList.Clear;
  uBuf:=nil;
  Loop:=True;
  dwLevel:=3;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetUserEnum(nil,dwLevel,FILTER_NORMAL_ACCOUNT,Pointer(uBuf),dwPrefMaxLen,dwEntriesRead,dwTotalEntries,dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      uTmpBuf:=uBuf;
      for i:=0 to dwEntriesRead-1 do begin
        with uTmpBuf^ do begin
          Name:=WideCharToString(usri3_name);
          Sid:=GetSIDFromAccount('',Name);
          AList.Add(Format('%s=%s',[Sid,Name]));
          uTmpBuf:=PUSER_INFO_3(PAnsiChar(uTmpBuf)+SizeOf(USER_INFO_3));
        end;
      end;
      if Assigned(uBuf) then begin
        NetApiBufferFree(uBuf);
        uBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(uBuf) then
    NetApiBufferFree(uBuf);

  gBuf:=nil;
  Loop:=True;
  dwLevel:=1;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetLocalGroupEnum(nil,dwLevel,Pointer(gBuf),dwPrefMaxLen,dwEntriesRead,dwTotalEntries,dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      gTmpBuf:=gBuf;
      for i:=0 to dwEntriesRead-1 do begin
        with gTmpBuf^ do begin
          Name:=WideCharToString(lgrpi1_name);
          Sid:=GetSIDFromAccount('',Name);
          AList.Add(Format('%s=%s',[Sid,Name]));
          gTmpBuf:=PLOCALGROUP_INFO_1(PAnsiChar(gTmpBuf)+SizeOf(LOCALGROUP_INFO_1));
        end;
      end;
      if Assigned(gBuf) then begin
        NetApiBufferFree(gBuf);
        gBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(gBuf) then
    NetApiBufferFree(gBuf);

  xBuf:=nil;
  Loop:=True;
  dwLevel:=2;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=NetGroupEnum(nil,dwLevel,Pointer(xBuf),dwPrefMaxLen,dwEntriesRead,dwTotalEntries,dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      xTmpBuf:=xBuf;
      for i:=0 to dwEntriesRead-1 do begin
        with xTmpBuf^ do begin
          Name:=WideCharToString(grpi2_name);
          Sid:=GetSIDFromAccount('',Name);
          AList.Add(Format('%s=%s',[Sid,Name]));
          xTmpBuf:=PGROUP_INFO_2(PAnsiChar(xTmpBuf)+SizeOf(GROUP_INFO_2));
        end;
      end;
      if Assigned(xBuf) then begin
        NetApiBufferFree(xBuf);
        xBuf:=nil;
      end;
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
  if Assigned(xBuf) then
    NetApiBufferFree(xBuf);
end;

function SetDebugPriv: Boolean;
var
  Token: THandle;
  tkp: TTokenPrivileges;
  {$IFDEF RAD11PLUS}l: Cardinal;{$ENDIF}
begin
  Result:=False;
  if OpenProcessToken(GetCurrentProcess,TOKEN_ADJUST_PRIVILEGES or TOKEN_QUERY, Token) then begin
    if LookupPrivilegeValue(nil, PChar('SeDebugPrivilege'),tkp.Privileges[0].Luid) then begin
      tkp.PrivilegeCount := 1;
      tkp.Privileges[0].Attributes := SE_PRIVILEGE_ENABLED;
      {$IFDEF RAD11PLUS}
      Result:=AdjustTokenPrivileges(Token,False,tkp,0,nil,l);
      {$ELSE}
      Result := AdjustTokenPrivileges(Token, False, tkp, 0, PTokenPrivileges(nil)^, PDWord(nil)^);
      {$ENDIF}
    end;
  end;
end;

function RetrieveUserAccountPicture(APicture: TBitmap): boolean;
var
  ms: TMemoryStream;
  fn: string;
  {$IFDEF FPC}
  xml: TXMLDocument;
  r: TDOMNode;
  Decoder: TBase64DecodingStream;
  {$ELSE}
  xml: IXMLDocument;
  r: IXMLNode;
  {$ENDIF}
  {$IFNDEF RAD7PLUS}
  ss: TStringStream;
  {$ELSE}
  b: TBytes;
  {$ENDIF}
  jpg: TJPEGImage;
  {$IFDEF RAD7PLUS}
  png: TPNGIMage;
  {$ENDIF}
begin
  r:=nil;
  Result:=False;
  fn:=Format('%sContacts\%s.contact',[IncludeTrailingPathDelimiter(GetProfilePath),WindowsUser]);
  if FileExists(fn) then begin
    ms:=TMemoryStream.Create;
    try
      {$IFDEF FPC}
      ReadXMLFile(xml,fn);
      try
        r:=xml.DocumentElement.FindNode('c:PhotoCollection');
        if Assigned(r) then begin
          r:=r.FindNode('c:Photo');
          if Assigned(r) then begin
            r:=r.FindNode('c:Value');
            if Assigned(r) then begin
              fn:=r.TextContent;
              ss:=TStringStream.Create(fn);
              try
                ss.Position:=0;
                Decoder:=TBase64DecodingStream.Create(ss,bdmMIME);
                try
                  ms.CopyFrom(Decoder,Decoder.Size);
                finally
                  Decoder.Free;
                end;
              finally
                ss.Free;
              end;
              ms.Position:=0;
              if ms.Size>0 then begin
                APicture.LoadFromStream(ms);
                Result:=True;
              end;
            end;
          end;
        end;
      finally
        xml.Free;
      end;
      {$ELSE}
      CoInitialize(nil);
      xml:=TXMLDocument.Create(nil);
      try
        xml.LoadFromFile(fn);
        r:=xml.DocumentElement.ChildNodes.FindNode('c:PhotoCollection');
        if Assigned(r) then begin
          r:=r.ChildNodes.FindNode('c:Photo');
          if Assigned(r) then begin
            r:=r.ChildNodes.FindNode('c:Value');
            if Assigned(r) then begin
              {$IFDEF RAD7PLUS}
              b:={$IFDEF RAD9PLUS}Soap.{$ENDIF}EncdDecd.DecodeBase64({$IFDEF UNICODE}WideToAnsi{$ENDIF}(r.NodeValue));
              SaveBytesToStream(b,ms);
              {$ELSE}
              ss:=TStringStream.Create(r.NodeValue);
              try
                ss.Position:=0;
                EncdDecd.EncodeStream(ss,ms);
              finally
                ss.Free;
              end;
              {$ENDIF}
              ms.Position:=0;
              if ms.Size>0 then begin
                try
                  APicture.LoadFromStream(ms);
                  Result:=True;
                except
                  try
                    jpg:=TJPEGImage.Create;
                    try
                      ms.Position:=0;
                      jpg.LoadFromStream(ms);
                      APicture.Assign(jpg);
                      Result:=True;
                    finally
                      jpg.Free;
                    end;
                  except
                    {$IFDEF RAD7PLUS}
                    try
                      png:=TPNGImage.Create;
                      try
                        ms.Position:=0;
                        png.LoadFromStream(ms);
                        APicture.Assign(png);
                        Result:=True;
                      finally
                        png.Free;
                      end;
                    except
                    end;
                    {$ENDIF}
                  end;
                end;
              end;
            end;
          end;
        end;
      finally
        xml:=nil;
        CoUninitialize;
      end;
    {$ENDIF}
    finally
      ms.Free;
    end;
  end else begin
    fn:=IncludeTrailingPathDelimiter(GetSpecialFolder(0,CSIDL_COMMON_APPDATA))+'Microsoft\User Account Pictures\'+WindowsUser+'.bmp';
    if FileExists(fn) then begin
      APicture.LoadFromFile(fn);
      Result:=True;
    end;
  end;
end;

initialization
end.


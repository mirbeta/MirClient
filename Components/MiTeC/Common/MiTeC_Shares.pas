{*******************************************************}
{               MiTeC Common Routines                   }
{              Sharepoint enumeration                   }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}


unit MiTeC_Shares;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, System.Classes;
     {$ELSE}
     Windows, SysUtils, Classes;
     {$ENDIF}

const
  MAX_ENTRIES = 20;

  {$IFDEF FPC}
  RESOURCEDISPLAYTYPE_GENERIC            = $00000000;
  RESOURCEDISPLAYTYPE_DOMAIN             = $00000001;
  RESOURCEDISPLAYTYPE_SERVER             = $00000002;
  RESOURCEDISPLAYTYPE_SHARE              = $00000003;
  RESOURCEDISPLAYTYPE_FILE               = $00000004;
  RESOURCEDISPLAYTYPE_GROUP              = $00000005;
  RESOURCEDISPLAYTYPE_NETWORK            = $00000006;
  RESOURCEDISPLAYTYPE_ROOT               = $00000007;
  RESOURCEDISPLAYTYPE_SHAREADMIN         = $00000008;
  RESOURCEDISPLAYTYPE_DIRECTORY          = $00000009;
  RESOURCEDISPLAYTYPE_TREE               = $0000000A;
  RESOURCEDISPLAYTYPE_NDSCONTAINER       = $0000000B;
  {$ENDIF}

type
  TShareType = (stDisk, stPrnQue, stCommDev, stIPC, stSpecial);

  POpenFileRecord = ^TOpenFileRecord;
  TOpenFileRecord = record
    Name,
    UserName,
    Sharename: string;
    Locks: Cardinal;
    Mode: Cardinal;
    ID: Cardinal;
  end;

  PConnectionRecord = ^TConnectionRecord;
  TConnectionRecord = record
    Name: string;
    UserName: string;
    ID: Cardinal;
    ConnType: TShareType;
    Time: Cardinal;
    OpenFiles: Cardinal;
    Users: Cardinal;
  end;

  TConnections = class(TPersistent)
  private
    FConns: TStringList;
    FMachine: string;
    FQualifier: string;
    procedure RetrieveNT(AMachine, AQualifier: string; AClearPrevious: Boolean = True);
    function GetConn(Index: Integer): PConnectionRecord;
    function GetConnCount: integer;
    procedure FreeList(var AList: TStringList);
    procedure SetMachine(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure Refresh(AClearPrevious: Boolean = True);

    property Machine: string read FMachine Write SetMachine;
    property Qualifier: string read FQualifier write FQualifier;
    property ConnectionCount: integer read GetConnCount;
    property Connections[Index: Integer]: PConnectionRecord read GetConn;
  end;

  PNTShareRecord = ^TNTShareRecord;
  TNTShareRecord = record
    Name: string;
    ShareType: TShareType;
    Comment: string;
    Permissions: Cardinal;
    MaxUserCount: Cardinal;
    CurUserCount: Cardinal;
    Path: string;
    Password: string;
    SecurityDesc: Boolean;
  end;

  PNTSessionRecord = ^TNTSessionRecord;         
  TNTSessionRecord = record
    Name: string;
    UserName: string;
    SesiType: string;
    OpenFiles: Cardinal;
    ConnectedTime: Cardinal;
    IdleTime: Cardinal;
    Guest: Boolean;
    Transport: string;
  end;

  TNTShares = class(TPersistent)
  private
    FShares, FSessions, FOpenFiles: TStringList;
    FMachine: string;
    function GetShareCount: integer;
    function GetShare(Index: Integer): PNTShareRecord;
    function GetOpenFile(Index: Integer): POpenFileRecord;
    function GetOpenFileCount: integer;
    function GetSession(Index: Integer): PNTSessionRecord;
    function GetSessionCount: integer;

    procedure RetrieveShares(AMachine: string);
    procedure RetrieveSessions(AMachine: string);
    procedure RetrieveOpenFiles(AMachine: string);

    procedure FreeShareList(var AList: TStringList);
    procedure FreeSessionList(var AList: TStringList);
    procedure FreeOpenFileList(var AList: TStringList);
    procedure SetMachine(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RefreshShares;
    procedure RefreshSessions;
    procedure RefreshOpenFiles;

    property Machine: string read FMachine Write SetMachine;
    property ShareCount: integer read GetShareCount;
    property Shares[Index: Integer]: PNTShareRecord read GetShare;

    property SessionCount: integer read GetSessionCount;
    property Sessions[Index: Integer]: PNTSessionRecord read GetSession;

    property OpenFileCount: integer read GetOpenFileCount;
    property OpenFiles[Index: Integer]: POpenFileRecord read GetOpenFile;
  end;

type
  P9xShareRecord = ^T9xShareRecord;
  T9xShareRecord = record
    Name: string;
    ShareType: TShareType;
    Path: string;
    Comment: string;
  end;

  T9xShares = class(TPersistent)
  private
    FShares, FSessions, FOpenFiles: TStringList;
    FMachine: string;
    function GetShareCount: Cardinal;
    function GetShare(Index: Integer): P9xShareRecord;
    procedure RetrieveShares(AScope: Cardinal; ANetResource: PNetResource; AMachine: string; var AList: TStringList);
    procedure RetrieveOpenFiles(AMachine: string);
    procedure FreeShareList(var AList: TStringList);
    procedure FreeOpenFileList(var AList: TStringList);
    function GetSession(Index: Integer): P9xShareRecord;
    function GetSessionCount: Cardinal;
    function GetOpenFile(Index: Integer): POpenFileRecord;
    function GetOpenFileCount: Cardinal;
    procedure SetMachine(const Value: string);
  public
    constructor Create;
    destructor Destroy; override;

    procedure RefreshShares;
    procedure RefreshSessions;
    procedure RefreshOpenFiles;

    property Machine: string read FMachine Write SetMachine;
    property ShareCount: Cardinal read GetShareCount;
    property Shares[Index: Integer]: P9xShareRecord read GetShare;

    property SessionCount: Cardinal read GetSessionCount;
    property Sessions[Index: Integer]: P9xShareRecord read GetSession;

    property OpenFileCount: Cardinal read GetOpenFileCount;
    property OpenFiles[Index: Integer]: POpenFileRecord read GetOpenFile;
  end;

function GetPermissionStr(AValue: Cardinal): string;

const
  ShareTypes : array[TShareType] of string =
    ('Disk','Print','Device','IPC','Special');

implementation

uses MiTeC_Routines, MiTeC_NetAPI32, MiTeC_SvrAPI, MiTeC_StrUtils;

function GetPermissionStr(AValue: Cardinal): string;
begin
  Result:='-------';
  if AValue and ACCESS_READ<>0 then
    Result[1]:='R';
  if AValue and ACCESS_WRITE<>0 then
    Result[2]:='W';
  if AValue and ACCESS_CREATE<>0 then
    Result[3]:='C';
  if AValue and ACCESS_EXEC<>0 then
    Result[4]:='X';
  if AValue and ACCESS_DELETE<>0 then
    Result[5]:='D';
  if AValue and ACCESS_ATRIB<>0 then
    Result[6]:='A';
  if AValue and ACCESS_PERM<>0 then
    Result[7]:='P';
end;

{ TNTShares }

constructor TNTShares.Create;
begin
  FShares:=TStringList.Create;
  FSessions:=TStringList.Create;
  FOpenFiles:=TStringList.Create;
  Machine:=Machinename;
end;

destructor TNTShares.Destroy;
begin
  FreeShareList(FShares);
  FShares.Free;
  FreeSessionList(FSessions);
  FSessions.Free;
  FreeOpenFileList(FOpenFiles);
  FOpenFiles.Free;
  inherited;
end;

procedure TNTShares.FreeShareList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(PNTShareRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

function TNTShares.GetShareCount: integer;
begin
  Result:=FShares.Count;
end;

function TNTShares.GetShare(Index: Integer): PNTShareRecord;
begin
  if Index<FShares.Count then
    Result:=PNTShareRecord(FShares.Objects[Index])
  else
    Result:=nil;
end;

procedure TNTShares.RefreshShares;
begin
  RetrieveShares(Machine)
end;

procedure TNTShares.RetrieveShares(AMachine: string);
var
  pTmpBuf, pBuf: PSHARE_INFO_2;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  psr: PNTShareRecord;
  Loop: Boolean;
  Buffer: array[0..255] of WideChar;
begin
  FreeShareList(FShares);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=MiTeC_NetAPI32.NetShareEnum(StringToWideChar(AMachine,Buffer,256),2,
                          Pointer(pBuf),
                          dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      i:=0;
      repeat
        new(psr);
        with pTmpBuf^ do begin
          psr^.Name:=WideCharToString(shi2_netname);
          psr^.Path:=WideCharToString(shi2_path);
          psr^.Comment:=WideCharToString(shi2_remark);
          psr^.CurUserCount:=shi2_current_uses;
          psr^.MaxUserCount:=shi2_max_uses;
          //psr^.SecurityDesc:=(IsValidSecurityDescriptor(shi2_security_descriptor));
          //if Assigned(shi2_passwd) then
            psr^.Password:=WideCharToString(shi2_passwd);
          case shi2_type and $0000000f of
            STYPE_DISKTREE: psr^.ShareType:=stDisk;
            STYPE_PRINTQ: psr^.ShareType:=stPrnQue;
            STYPE_DEVICE: psr^.ShareType:=stCommDev;
            STYPE_IPC: psr^.ShareType:=stIPC;
            STYPE_SPECIAL: psr^.ShareType:=stSpecial;
          end;
          psr^.Permissions:=shi2_permissions;
        end;
        FShares.AddObject(psr^.Name,TObject(psr));
        pTmpBuf:=PSHARE_INFO_2(PAnsiChar(pTmpBuf)+SizeOf(SHARE_INFO_2));
        Inc(i);
      until i>=dwEntriesRead;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
end;

procedure TNTShares.FreeOpenFileList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(POpenFileRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

procedure TNTShares.FreeSessionList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(PNTSessionRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

function TNTShares.GetOpenFile(Index: Integer): POpenFileRecord;
begin
  if Index<FOpenFiles.Count then
    Result:=POpenFileRecord(FOpenFiles.Objects[Index])
  else
    Result:=nil;
end;

function TNTShares.GetOpenFileCount: integer;
begin
  Result:=FOpenFiles.Count;
end;

function TNTShares.GetSession(Index: Integer): PNTSessionRecord;
begin
  if Index<FSessions.Count then
    Result:=PNTSessionRecord(FSessions.Objects[Index])
  else
    Result:=nil;
end;

function TNTShares.GetSessionCount: integer;
begin
  Result:=FSessions.Count;
end;

procedure TNTShares.RefreshOpenFiles;
begin
  RetrieveOpenFiles(Machine)
end;

procedure TNTShares.RefreshSessions;
begin
  RetrieveSessions(Machine)
end;

procedure TNTShares.RetrieveOpenFiles(AMachine: string);
var
  pTmpBuf, pBuf: PFILE_INFO_3;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buffer: array[0..256] of WideChar;
  pofr: POpenFileRecord;
begin
  FreeOpenFileList(FOpenFiles);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=MiTeC_NetAPI32.NetFileEnum(StringToWideChar(AMachine,Buffer,256),nil,nil,3,
                          Pointer(pBuf),
                          dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(pofr);
        pofr^.Name:=WideCharToString(pTmpBuf^.fi3_pathname);
        pofr^.UserName:=WideCharToString(pTmpBuf^.fi3_username);
        pofr^.Locks:=pTmpBuf^.fi3_num_locks;
        pofr^.Mode:=pTmpBuf^.fi3_permissions;
        pofr^.ID:=pTmpBuf^.fi3_id;
        FOpenFiles.AddObject(IntToStr(pofr^.ID),TObject(pofr));
        pTmpBuf:=PFILE_INFO_3(PAnsiChar(pTmpBuf)+SizeOf(FILE_INFO_3));
      end;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
end;

procedure TNTShares.RetrieveSessions(AMachine: string);
var
  pTmpBuf, pBuf: PSESSION_INFO_502;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buffer: array[0..256] of WideChar;
  psr: PNTSessionRecord;
begin
  FreeSessionList(FSessions);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=MiTeC_NetAPI32.NetSessionEnum(StringToWideChar(AMachine,Buffer,256),nil,nil,502,
                          Pointer(pBuf),
                          dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(psr);
        psr^.Name:=WideCharToString(pTmpBuf^.sesi502_cname);
        psr^.UserName:=WideCharToString(pTmpBuf^.sesi502_username);
        psr^.OpenFiles:=pTmpBuf^.sesi502_num_opens;
        psr^.SesiType:=WideCharToString(pTmpBuf^.sesi502_cltype_name);
        psr^.ConnectedTime:=pTmpBuf^.sesi502_time;
        psr^.IdleTime:=pTmpBuf^.sesi502_idle_time;
        psr^.Guest:=pTmpBuf^.sesi502_user_flags=SESS_GUEST;
        psr^.Transport:=WideCharToString(pTmpBuf^.sesi502_transport);
        FSessions.AddObject(psr^.Name,TObject(psr));
        pTmpBuf:=PSESSION_INFO_502(PAnsiChar(pTmpBuf)+SizeOf(SESSION_INFO_502));
      end;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
end;

procedure TNTShares.SetMachine(const Value: string);
begin
  FMachine:=Value;
  if FMachine='' then
    FMachine:='.';
  if FMachine[1]<>'\' then
    FMachine:='\\'+FMachine;
end;

{ TConnections }

constructor TConnections.Create;
begin
  FConns:=TStringList.Create;
  Machine:=Machinename;
  InitNetAPI;
end;

destructor TConnections.Destroy;
begin
  FreeList(FConns);
  FConns.Destroy;
  inherited;
end;

procedure TConnections.FreeList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    FreeMem(PConnectionRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

function TConnections.GetConn(Index: Integer): PConnectionRecord;
begin
  if Index<FConns.Count then
    Result:=PConnectionRecord(FConns.Objects[Index])
  else
    Result:=nil;
end;

function TConnections.GetConnCount: integer;
begin
  Result:=FConns.Count;
end;

procedure TConnections.Refresh;
begin
  RetrieveNT(Machine,Qualifier,AClearPrevious)
end;

procedure TConnections.RetrieveNT;
var
  pBuf: Pointer;
  pTmpBuf: MiTeC_NetAPI32.PCONNECTION_INFO_1;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen: Cardinal;
  dwEntriesRead: Cardinal;
  dwTotalEntries: Cardinal;
  dwResumeHandle: Cardinal;
  Loop: Boolean;
  Buf1: array[0..256] of WideChar;
  pcr: PConnectionRecord;
begin
  if AClearPrevious then
    FreeList(FConns);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFFFFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  while Loop do begin
    nStatus:=MiTeC_NetAPI32.NetConnectionEnum(StringToWideChar(AMachine,Buf1,256),
                               StringToWideChar(AQualifier,Buf1,256),
                               1,
                               pBuf,
                               dwPrefMaxLen, dwEntriesRead, dwTotalEntries, dwResumeHandle);
    if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (dwEntriesRead>0) then begin
      pTmpBuf:=MiTeC_NetAPI32.PCONNECTION_INFO_1(pBuf);
      for i:=0 to dwEntriesRead-1 do begin
        new(pcr);
        pcr^.Name:=WideCharToString(pTmpBuf^.coni1_netname);
        pcr^.UserName:=WideCharToString(pTmpBuf^.coni1_username);
        pcr^.ID:=pTmpBuf^.coni1_id;
        case pTmpBuf^.coni1_type of
          STYPE_DISKTREE: pcr^.ConnType:=stDisk;
          STYPE_PRINTQ: pcr^.ConnType:=stPrnQue;
          STYPE_DEVICE: pcr^.ConnType:=stCommDev;
          STYPE_IPC: pcr^.ConnType:=stIPC;
          STYPE_SPECIAL: pcr^.ConnType:=stSpecial;
        end;
        pcr^.Time:=pTmpBuf^.coni1_time;
        pcr^.OpenFiles:=pTmpBuf^.coni1_num_opens;
        pcr^.Users:=pTmpBuf^.coni1_num_users;
        FConns.AddObject(IntToStr(pcr^.ID),TObject(pcr));
        pTmpBuf:=MiTeC_NetAPI32.PCONNECTION_INFO_1(PAnsiChar(pTmpBuf)+SizeOf(MiTeC_NetAPI32.CONNECTION_INFO_1));
      end;
      if Assigned(pBuf) then
        NetApiBufferFree(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
end;

procedure TConnections.SetMachine(const Value: string);
begin
  FMachine:=Value;
  if FMachine='' then
    FMachine:='.';
  if FMachine[1]<>'\' then
    FMachine:='\\'+FMachine;
end;

{ T9xShares }

constructor T9xShares.Create;
begin
  InitSvrAPI;
  FShares:=TStringList.Create;
  FSessions:=TStringList.Create;
  FOpenFiles:=TStringList.Create;
  Machine:=Machinename;
end;

destructor T9xShares.Destroy;
begin
  FreeShareList(FShares);
  FShares.Free;
  FreeShareList(FSessions);
  FSessions.Free;
  FreeOpenFileList(FOpenFiles);
  FOpenFiles.Free;
  inherited;
end;

procedure T9xShares.FreeShareList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(P9xShareRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

procedure T9xShares.FreeOpenFileList(var AList: TStringList);
begin
  while AList.Count>0 do begin
    dispose(POpenFileRecord(AList.Objects[AList.Count-1]));
    AList.Delete(AList.Count-1);
  end;
end;

function T9xShares.GetShareCount: Cardinal;
begin
  Result:=FShares.Count;
end;

function T9xShares.GetSession(Index: Integer): P9xShareRecord;
begin
  if Index<FSessions.Count then
    Result:=P9xShareRecord(FSessions.Objects[Index])
  else
    Result:=nil;
end;

function T9xShares.GetShare(Index: Integer): P9xShareRecord;
begin
  if Index<FShares.Count then
    Result:=P9xShareRecord(FShares.Objects[Index])
  else
    Result:=nil;
end;

procedure T9xShares.RefreshSessions;
begin
  RetrieveShares(RESOURCE_CONNECTED,nil,Machine,FSessions);
end;

procedure T9xShares.RefreshShares;
begin
  RetrieveShares(RESOURCE_GLOBALNET,nil,Machine,FShares);
end;

procedure T9xShares.RetrieveShares;
var
  phEnum :THandle;
  pcCount, Size: Cardinal;
  i, NetError :integer;
  NR :array[0..100] of TNetResource;
  nri: TNetResource;
  psr: P9xShareRecord;
  rn,n: string;
  cont: Boolean;
begin
  if wnetopenenum(AScope,RESOURCETYPE_ANY,0,ANetResource,phEnum)=NO_ERROR then begin
    repeat
      pccount:=Cardinal(-1);
      size:=sizeof(nr);
      neterror:=wnetenumresource(phEnum,pccount,@nr[0],size);
      if neterror=0 then begin
        for i:=0 to pccount-1 do  begin
          nri:=nr[i];
          rn:=UpperCase(nri.lpRemoteName);
          n:=nri.lpLocalName;
          if n='' then
            n:=rn;
          if (AList.IndexOf(n)=-1) and
             ((AScope<>RESOURCE_GLOBALNET) or ((AScope=RESOURCE_GLOBALNET) and (Pos(AMachine,rn)>0))) then begin
            new(psr);
            psr^.Path:=nri.lpRemoteName;
            psr^.Name:=n;
            case nri.dwType of
              RESOURCETYPE_ANY: psr^.ShareType:=stSpecial;
              RESOURCETYPE_DISK: psr^.ShareType:=stDisk;
              RESOURCETYPE_PRINT: psr^.ShareType:=stPrnQue;
              else
                psr^.ShareType:=stSpecial;
            end;
            psr^.Comment:=nri.lpComment;
            AList.AddObject(psr^.Name,TObject(psr));
            cont:=(nri.dwDisplayType>=RESOURCEDISPLAYTYPE_DOMAIN);
          end else
            cont:=(AScope=RESOURCE_GLOBALNET) and
                  (nri.dwDisplayType in [RESOURCEDISPLAYTYPE_DOMAIN,RESOURCEDISPLAYTYPE_GROUP,RESOURCEDISPLAYTYPE_NETWORK,RESOURCEDISPLAYTYPE_ROOT]);
          if cont then
            RetrieveShares(AScope,@nri,AMachine,AList);
        end;
      end;
    until neterror=ERROR_NO_MORE_ITEMS;
    wnetcloseenum(phEnum);
  end;
end;
{var
  pTmpBuf, pBuf: PSHARE_INFO_50;
  nStatus: NET_API_STATUS;
  i: Cardinal;
  dwPrefMaxLen,
  dwEntriesRead,
  dwTotalEntries,
  dwResumeHandle: Word;
  psr: PNTShareRecord;
  Loop: Boolean;
begin
  FreeShareList(FShares);
  pBuf:=nil;
  Loop:=True;
  dwPrefMaxLen:=$FFFF;
  dwEntriesRead:=0;
  dwTotalEntries:=0;
  dwResumeHandle:=0;
  pBuf:=AllocMem(dwPrefMaxLen);
  while Loop do begin
    nStatus:=MiTeC_SvrAPI.NetShareEnum(PAnsiChar(AMachine),50,
                          PAnsiChar(pBuf),dwPrefMaxLen, dwEntriesRead, dwTotalEntries);
    if (nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA) then begin
      pTmpBuf:=pBuf;
      for i:=0 to dwEntriesRead-1 do begin
        new(psr);
        with pTmpBuf^ do begin
          psr^.Name:=shi50_netname;
          psr^.Path:=shi50_path;
          psr^.Comment:=shi50_remark;
          case shi50_type of
            STYPE_DISKTREE: psr^.ShareType:=stDisk;
            STYPE_PRINTQ: psr^.ShareType:=stPrnQue;
            STYPE_DEVICE: psr^.ShareType:=stCommDev;
            STYPE_IPC: psr^.ShareType:=stIPC;
            STYPE_SPECIAL: psr^.ShareType:=stSpecial;
          end;
        end;
        FShares.AddObject(psr^.Name,TObject(psr));
        pTmpBuf:=PSHARE_INFO_50(PAnsiChar(pTmpBuf)+SizeOf(SHARE_INFO_50));
      end;
      if Assigned(pBuf) then
        FreeMem(pBuf);
      if nStatus=ERROR_SUCCESS then
        Loop:=False;
      dwResumeHandle:=dwEntriesRead+1;
    end else
      Loop:=False;
  end;
end;}

function T9xShares.GetSessionCount: Cardinal;
begin
  Result:=FSessions.Count;
end;

procedure T9xShares.RetrieveOpenFiles(AMachine: string);
var
  pTmpBuf, pBuf: pfile_info_50;
  nStatus: Cardinal;
  i: Cardinal;
  cbBuffer: WORD;
  nEntriesRead: WORD;
  nTotalEntries: WORD;
  pofr: POpenFileRecord;
begin
  FreeOpenFileList(FOpenFiles);
  pBuf:=nil;
  cbBuffer:=MAX_ENTRIES*SizeOf(file_info_50);
  nEntriesRead:=0;
  nTotalEntries:=0;
  pBuf:=AllocMem(cbBuffer);
  nStatus:=MiTeC_SvrAPI.NetFileEnum(PAnsiChar({$IFDEF UNICODE}WideToAnsi{$ENDIF}(AMachine)),nil,50,
                       PAnsiChar(pBuf),
                       cbBuffer, nEntriesRead, nTotalEntries);
  if ((nStatus=ERROR_SUCCESS) or (nStatus=ERROR_MORE_DATA)) and (nEntriesRead>0) then begin
    pTmpBuf:=pBuf;
    for i:=0 to nEntriesRead-1 do begin
      new(pofr);
      pofr^.Name:=string(pTmpBuf^.fi50_pathname);
      FOpenFiles.AddObject(pofr^.Name,TObject(pofr));
      pTmpBuf:=pfile_info_50(PAnsiChar(pTmpBuf)+SizeOf(file_info_50));
    end;
  end;
  if Assigned(pBuf) then
    FreeMem(pBuf);
end;

function T9xShares.GetOpenFile(Index: Integer): POpenFileRecord;
begin
  if Index<FOpenFiles.Count then
    Result:=POpenFileRecord(FOpenFiles.Objects[Index])
  else
    Result:=nil;
end;

function T9xShares.GetOpenFileCount: Cardinal;
begin
  Result:=FOpenFiles.Count;
end;

procedure T9xShares.RefreshOpenFiles;
begin
  RetrieveOpenFiles(Machine);
end;

procedure T9xShares.SetMachine(const Value: string);
begin
  FMachine:=Value;
end;

end.

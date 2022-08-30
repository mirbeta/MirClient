{*******************************************************}
{                MiTeC Common Routines                  }
{                  Windows 9x SVR API                   }
{                                                       }
{         Copyright (c) 1997-2014 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}

unit MiTeC_SvrAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils;
     {$ELSE}
     Windows, SysUtils;
     {$ENDIF}

const
  LM20_NNLEN = 12;
  SHPWLEN = 8;

  STYPE_DISKTREE = 0;
  STYPE_PRINTQ   = 1;
  STYPE_DEVICE   = 2;
  STYPE_IPC      = 3;

  STYPE_SPECIAL  = $80000000;

type
  _share_info_50 = record
    shi50_netname: array [0..LM20_NNLEN] of ansichar;
    shi50_type: BYTE;
    shi50_flags: WORD;
    shi50_remark: PAnsiChar;
    shi50_path: PAnsiChar;
    shi50_rw_password: array[0..SHPWLEN] of ansichar;
    shi50_ro_password: array[0..SHPWLEN] of ansichar;
  end;

  pshare_info_50 = ^share_info_50;
  share_info_50 = _share_info_50;

  _session_info_50 = record
    sesi50_cname: PAnsiChar;
    sesi50_username: PAnsiChar;
    sesi50_key: Cardinal;
    sesi50_num_conns: WORD;
    sesi50_num_opens: WORD;
    sesi50_time: Cardinal;
    sesi50_idle_time: Cardinal;
    sesi50_protocol: PAnsiChar;
    pad1: PAnsiChar;
  end;

  psession_info_50 = ^session_info_50;
  session_info_50 = _session_info_50;

  _file_info_50 = record
    fi50_id: Cardinal;
    fi50_permissions: WORD;
    fi50_num_locks: WORD;
    fi50_pathname: PAnsiChar;
    fi50_username: PAnsiChar;
    fi50_sharename: PAnsiChar;
  end;

  pfile_info_50 = ^file_info_50;
  file_info_50 = _file_info_50;

  _connection_info_1 = record
    coni1_id: word;
    coni1_type: word;
    coni1_num_opens: word;
    coni1_num_users: word;
    coni1_time: Cardinal;
    coni1_username: PAnsiChar;
    coni1_netname: PAnsiChar;
  end;

  pconnection_info_1 = ^connection_info_1;
  connection_info_1 = _connection_info_1;


  _connection_info_50 = record
    coni50_type: Word;
    coni50_num_opens: Word;
    coni50_time: Cardinal;
    coni50_netname: PAnsiChar;
    coni50_username: PAnsiChar;
  end;

  pconnection_info_50 = ^connection_info_50;
  connection_info_50 = _connection_info_50;


function InitSvrAPI: Boolean;
procedure FreeSvrAPI;

type
  TNetFileEnum = function(const pszServer: PAnsiChar;
                     const pszBasePath: PAnsiChar;
                     sLevel: short;
                     var pbBuffer: PAnsiChar;
                     cbBuffer: Word;
                     var pcEntriesRead: Word;
                     var pcTotalAvail: WORD): Cardinal; stdcall;

  TNetSessionEnum = function(const pszServer: PAnsiChar;
                        sLevel: short;
                        var pbBuffer: PAnsiChar;
                        cbBuffer: Word;
                        var pcEntriesRead: Word;
                        var pcTotalAvail: WORD): Cardinal; stdcall;

  TNetShareEnum = function(const pszServer: PAnsiChar;
                      sLevel: short;
                      var pbBuffer: PAnsiChar;
                      cbBuffer: Word;
                      var pcEntriesRead: Word;
                      var pcTotalAvail: WORD): Cardinal; stdcall;

  TNetConnectionEnum = function(const pszServer: PAnsiChar;
                           const pszQualifier: PAnsiChar;
                           sLevel: short;
                           var pbBuffer: PAnsiChar;
                           cbBuffer: Word;
                           var pcEntriesRead: Word;
                           var pcTotalAvail: Word): Cardinal; stdcall;

var
  NetShareEnum: TNetShareEnum;
  NetSessionEnum: TNetSessionEnum;
  NetFileEnum: TNetFileEnum;
  NetConnectionEnum: TNetConnectionEnum;

implementation

const
  SVRAPI_DLL = 'svrapi.dll';

var
  SvrAPIHandle: THandle;
  UnloadSvrAPI: Boolean;

function InitSvrAPI: Boolean;
begin
  SvrAPIHandle:=GetModuleHandle(SvrAPI_DLL);
  UnloadSvrAPI:=SvrAPIHandle=0;
  if SvrAPIHandle=0 then
    SvrAPIHandle:=loadlibrary(SvrAPI_DLL);
  if SvrAPIHandle<>0 then begin
    @NetShareEnum:=GetProcAddress(SvrAPIHandle,PAnsiChar('NetShareEnum'));
    @NetSessionEnum:=GetProcAddress(SvrAPIHandle,PAnsiChar('NetSessionEnum'));
    @NetFileEnum:=GetProcAddress(SvrAPIHandle,PAnsiChar('NetFileEnum'));
    @NetConnectionEnum:=GetProcAddress(SvrAPIHandle,PAnsiChar('NetConnectionEnum'));
  end;
  result:=(SvrAPIHandle<>0) and Assigned(NetShareEnum);
end;

procedure FreeSvrAPI;
begin
  if (SvrAPIHandle<>0) and UnloadSvrAPI then begin
    if not FreeLibrary(SvrAPIHandle) then
      raise Exception.Create(Format('Unload Error: %s - 0x%x',[SvrAPI_DLL,GetModuleHandle(SvrAPI_DLL)]))
    else
      SvrAPIHandle:=0;
  end;
end;

initialization
finalization
  FreeSvrAPI;
end.

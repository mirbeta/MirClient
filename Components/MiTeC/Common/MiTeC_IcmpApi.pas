{*******************************************************}
{               MiTeC Common Routines                   }
{              ICMP Echo API interface                  }
{                                                       }
{                                                       }
{         Copyright (c) 1997-2019 Michal Mutl           }
{                                                       }
{*******************************************************}

{$INCLUDE Compilers.inc}
{$DEFINE DYNALINK}

unit MiTeC_IcmpAPI;

interface

uses {$IFDEF RAD9PLUS}
     WinAPI.Windows, System.SysUtils, WinAPI.WinSock,
     {$ELSE}
     Windows, SysUtils, WinSock,
     {$ENDIF}
     MiTeC_Ws2_32, MiTeC_IpHlpApi, MiTeC_NativeDefs, MiTeC_Windows;


function IcmpCreateFile: THandle; stdcall;
function Icmp6CreateFile: THandle; stdcall;
function IcmpCloseHandle(IcmpHandle: THandle): BOOL; stdcall;
function IcmpSendEcho(IcmpHandle: THandle; DestinationAddress: TInAddr; RequestData: LPVOID; RequestSize: WORD;
                      RequestOptions: PIP_OPTION_INFORMATION; ReplyBuffer: LPVOID;
                      ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
function IcmpSendEcho2(IcmpHandle: THandle; Event: THandle; ApcRoutine: PIO_APC_ROUTINE;
                       ApcContext: PVOID; DestinationAddress: TInAddr; RequestData: LPVOID;
                       RequestSize: WORD; RequestOptions: PIP_OPTION_INFORMATION;
                       ReplyBuffer: LPVOID; ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
function Icmp6SendEcho2(IcmpHandle: THandle; Event: THandle; ApcRoutine: PIO_APC_ROUTINE;
                        ApcContext: PVOID; SourceAddress: Psockaddr_in6; DestinationAddress: Psockaddr_in6;
                        RequestData, RequestSize: WORD; RequestOptions: PIP_OPTION_INFORMATION;
                        ReplyBuffer: LPVOID; ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
function IcmpParseReplies(ReplyBuffer: LPVOID; ReplySize: DWORD): DWORD; stdcall;
function Icmp6ParseReplies(ReplyBuffer: LPVOID; ReplySize: DWORD): DWORD; stdcall;

implementation

const
  icmplib = 'iphlpapi.dll';

{$IFDEF DYNALINK}

type
  TIcmpCreateFile = function: THandle; stdcall;
  TIcmp6CreateFile = function: THandle; stdcall;
  TIcmpCloseHandle = function(IcmpHandle: THandle): BOOL; stdcall;
  TIcmpSendEcho = function(IcmpHandle: THandle; DestinationAddress: TInAddr; RequestData: LPVOID; RequestSize: WORD;
                      RequestOptions: PIP_OPTION_INFORMATION; ReplyBuffer: LPVOID;
                      ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
  TIcmpSendEcho2 = function(IcmpHandle: THandle; Event: THandle; ApcRoutine: PIO_APC_ROUTINE;
                       ApcContext: PVOID; DestinationAddress: TInAddr; RequestData: LPVOID;
                       RequestSize: WORD; RequestOptions: PIP_OPTION_INFORMATION;
                       ReplyBuffer: LPVOID; ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
  TIcmp6SendEcho2 = function(IcmpHandle: THandle; Event: THandle; ApcRoutine: PIO_APC_ROUTINE;
                        ApcContext: PVOID; SourceAddress: Psockaddr_in6; DestinationAddress: Psockaddr_in6;
                        RequestData, RequestSize: WORD; RequestOptions: PIP_OPTION_INFORMATION;
                        ReplyBuffer: LPVOID; ReplySize: DWORD; Timeout: DWORD): DWORD; stdcall;
  TIcmpParseReplies = function(ReplyBuffer: LPVOID; ReplySize: DWORD): DWORD; stdcall;
  TIcmp6ParseReplies = function(ReplyBuffer: LPVOID; ReplySize: DWORD): DWORD; stdcall;
var
  _IcmpCreateFile: TIcmpCreateFile = nil;
  _Icmp6CreateFile: TIcmp6CreateFile = nil;
  _IcmpCloseHandle: TIcmpCloseHandle = nil;
  _IcmpSendEcho: TIcmpSendEcho = nil;
  _IcmpSendEcho2: TIcmpSendEcho2 = nil;
  _Icmp6SendEcho2: TIcmp6SendEcho2 = nil;
  _IcmpParseReplies: TIcmpParseReplies = nil;
  _Icmp6ParseReplies: TIcmp6ParseReplies = nil;


procedure GetProcedureAddress(var P: Pointer; const ModuleName, ProcName: string);
var
  ModuleHandle: HMODULE;
begin
  if not Assigned(P) then begin
    ModuleHandle:=GetModuleHandle(PChar(ModuleName));
    if ModuleHandle=0 then begin
      ModuleHandle:=LoadLibrary(PChar(ModuleName));
      if ModuleHandle=0 then
        Exit;
    end;
    P:=GetProcAddress(ModuleHandle,PChar(ProcName));
    if not Assigned(P) then
      Exit;
  end;
end;

function IcmpCreateFile;
begin
  if not Assigned(_IcmpCreateFile) then
    GetProcedureAddress(Pointer(@_IcmpCreateFile),icmplib,'IcmpCreateFile');
  if Assigned(_IcmpCreateFile) then
    Result:=_IcmpCreateFile
  else
    Result:=INVALID_HANDLE_VALUE;
end;

function Icmp6CreateFile;
begin
  if not Assigned(_Icmp6CreateFile) then
    GetProcedureAddress(Pointer(@_Icmp6CreateFile),icmplib,'Icmp6CreateFile');
  if Assigned(_Icmp6CreateFile) then
    Result:=_Icmp6CreateFile
  else
    Result:=INVALID_HANDLE_VALUE;
end;

function IcmpCloseHandle;
begin
  if not Assigned(_IcmpCloseHandle) then
    GetProcedureAddress(Pointer(@_IcmpCloseHandle),icmplib,'IcmpCloseHandle');
  if Assigned(_IcmpCloseHandle) then
    Result:=_IcmpCloseHandle(IcmpHandle)
  else
    Result:=False;
end;

function IcmpSendEcho;
begin
  if not Assigned(_IcmpSendEcho) then
    GetProcedureAddress(Pointer(@_IcmpSendEcho),icmplib,'IcmpSendEcho');
  if Assigned(_IcmpSendEcho) then
    Result:=_IcmpSendEcho(IcmpHandle,DestinationAddress,RequestData,RequestSize,
                          RequestOptions,ReplyBuffer,ReplySize,Timeout)
  else
    Result:=0;
end;

function IcmpSendEcho2;
begin
  if not Assigned(_IcmpSendEcho2) then
    GetProcedureAddress(Pointer(@_IcmpSendEcho2),icmplib,'IcmpSendEcho2');
  if Assigned(_IcmpSendEcho2) then
    Result:=_IcmpSendEcho2(IcmpHandle,Event,ApcRoutine,ApcContext,DestinationAddress,
                           RequestData,RequestSize,RequestOptions,ReplyBuffer,ReplySize,Timeout)
  else
    Result:=0;
end;

function Icmp6SendEcho2;
begin
  if not Assigned(_Icmp6SendEcho2) then
    GetProcedureAddress(Pointer(@_Icmp6SendEcho2),icmplib,'Icmp6SendEcho2');
  if Assigned(_Icmp6SendEcho2) then
    Result:=_Icmp6SendEcho2(IcmpHandle,Event,ApcRoutine,ApcContext,SourceAddress,
                            DestinationAddress,RequestData,RequestSize,RequestOptions,
                            ReplyBuffer,ReplySize,Timeout)
  else
    Result:=0;
end;

function IcmpParseReplies;
begin
  if not Assigned(_IcmpParseReplies) then
    GetProcedureAddress(Pointer(@_IcmpParseReplies),icmplib,'IcmpParseReplies');
  if Assigned(_IcmpParseReplies) then
    Result:=_IcmpParseReplies(ReplyBuffer,ReplySize)
  else
    Result:=0;
end;

function Icmp6ParseReplies;
begin
  if not Assigned(_Icmp6ParseReplies) then
    GetProcedureAddress(Pointer(@_Icmp6ParseReplies),icmplib,'Icmp6ParseReplies');
  if Assigned(_Icmp6ParseReplies) then
    Result:=_Icmp6ParseReplies(ReplyBuffer,ReplySize)
  else
    Result:=0;
end;

{$ELSE}

function IcmpCreateFile; external icmplib name 'IcmpCreateFile';
function Icmp6CreateFile; external icmplib name 'Icmp6CreateFile';
function IcmpCloseHandle; external icmplib name 'IcmpCloseHandle';
function IcmpSendEcho; external icmplib name 'IcmpSendEcho';
function IcmpSendEcho2; external icmplib name 'IcmpSendEcho2';
function Icmp6SendEcho2; external icmplib name 'Icmp6SendEcho2';
function IcmpParseReplies; external icmplib name 'IcmpParseReplies';
function Icmp6ParseReplies; external icmplib name 'Icmp6ParseReplies';

{$ENDIF}

end.



library mPlugOfScript;

{ Important note about DLL memory management: ShareMem must be the
  first unit in your library's USES clause AND your project's (select
  Project-View Source) USES clause if your DLL exports any procedures or
  functions that pass strings as parameters or function results. This
  applies to all strings passed to and from your DLL--even those that
  are nested in records and classes. ShareMem is the interface unit to
  the BORLNDMM.DLL shared memory manager, which must be deployed along
  with your DLL. To avoid using BORLNDMM.DLL, pass string information
  using PChar or ShortString parameters. }

uses
  SysUtils,
  Windows,
  Classes,
  EDScript in 'EDScript.pas' {Form1},
  EngineAPI in '..\PlugInCommon\EngineAPI.pas',
  EngineType in '..\PlugInCommon\EngineType.pas',
  HUtil32 in '..\PlugInCommon\HUtil32.pas',
  DES in '..\..\Common\DES.pas';

{$R *.res}

type
  TMsgProc = procedure(Msg: PChar; nMsgLen: Integer; nMode: Integer); stdcall;

  TFindProc = function(sProcName: PChar; nNameLen: Integer): Pointer; stdcall;

  TFindObj = function(sObjName: PChar; nNameLen: Integer): TObject; stdcall;

  TSetProc = function(ProcAddr: Pointer; ProcName: PChar; nNameLen: Integer): Boolean; stdcall;

function Init(AppHandle: HWnd; MsgProc: TMsgProc; FindProc: TFindProc; SetProc: TSetProc; FindObj: TFindObj): PChar; stdcall;
begin
  MsgProc(LoadPlus, length(LoadPlus), 0);
  SetProc(@GetScriptText, ProcName, length(ProcName));
  Result := PlugName;
end;

procedure UnInit();
begin
  //UnInitPlug();
end;

function GetFunAddr(nIndex: Integer): Pointer; stdcall;
begin
  Result := nil;
end;

exports
  Init,
  UnInit,
  GetFunAddr;

begin
end.


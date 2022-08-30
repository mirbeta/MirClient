library mSystemModule;

uses
  Windows,
  SysUtils,
  Classes,
  tlhelp32,
  PsAPI,
  Module in 'Module.pas',
  ClassModule in 'ClassModule.pas',
  PlugManage in 'PlugManage.pas',
  PlugPin in 'PlugPin.pas',
  Share in 'Share.pas',
  MudUtil in '..\..\M2Engine\Globals\MudUtil.pas';

{$R *.res}

function StartPlug(Config: pTConfig): Boolean;
begin
  PlugEngine := TPlugManage.Create(True);
  PlugEngine.FreeOnTerminate := True;
  g_Config := Config;
  PlugEngine.Resume;
end;

function M2ServerVersion(): Single; stdcall;
begin
  Result := DLLVersion;
end;

function Init(AppHandle: HWnd; MsgProc: TMsgProc; FindProc: TFindProc; SetProc: TSetProc; FindObj: TFindObj): PChar; stdcall;
const
  sChangeLabelVerColor      = 'ChangeLabelVerColor';
  sGetResourceString        = 'GetResourceString';
  sGetExVersionNO           = 'GetExVersionNO';
  sGetGoldShape             = 'GetGoldShape';
begin
  if Assigned(MsgProc) then
    MainOutMessage := MsgProc;
  try
    FindProcTable := FindProc;
    FindObjTable := FindObj;
    SetProcTable := SetProc;
    MainOutMessage(PChar(g_DConfig.s_LoadPlugOk), Length(g_DConfig.s_LoadPlugOk), 0);
    Result := PChar(g_DConfig.s_PluginName);
    SocketRun := GetProcAddr('TRunSocket.Run'); {TRunSocket.Run'}
    PlugRunOver := GetProcAddr('PlugRunOver'); {PlugRunOver}
    SetRemoteXORKey := GetProcAddr('SetRemoteXORKey'); {SetRemoteXORKey}
    //GetVersion := GetProcAddr('GetVersion'); {GetVersion}
    EngineRun := GetProcAddr('TUserEngine.Run'); {TUserEngine.Run}
    ChangeCaptionText := GetProcAddr('ChangeCaptionText'); {ChangeCaptionText}
    ChangeLabelVerColor := GetProcAddr(sChangeLabelVerColor);
    SetProcAddr(@GetResourceString, (sGetResourceString));
    SetProcAddr(@GetExVersionNO, (sGetExVersionNO));
    SetProcAddr(@GetGoldShape, (sGetGoldShape));
    SetProcAddr(@TRunSocket.Run, ('TRunSocket.Run')); {TRunSocket.Run}
    SetProcAddr(@StartPlug, ('StartPlug') {StartPlug});
    SetProcAddr(@M2ServerVersion, ('M2ServerVersion') {'M2ServerVersion'});
    SetProcAddr(@TUserEngine.Run, ('TUserEngine.Run')); {TUserEngine.Run}
    SetProcAddr(@GetNextDirection, ('PulgProc01'));
    SetProcAddr(@GetValNameNo, ('PulgProc02'));
    SetProcAddr(@CheckUserItems, ('PulgProc03'));
    SetProcAddr(@GetItemNumber, ('PulgProc04'));
    SetProcAddr(@GetItemNumberEx, ('PulgProc05'));
    SetProcAddr(@FilterShowName, ('PulgProc06'));
    SetProcAddr(@CheckGuildName, ('PulgProc07'));
    SetProcAddr(@GetLocalIP,('GetLocalIP'));
    //if Assigned(GetVersion) then
    //g_DConfig.fVersion := GetVersion();
    //GetXORKey := GetProcAddr('GetXORKey');
  except
  end;
end;

procedure UnInit();
begin
  //MainOutMessage(PChar('卸载插件管理器成功...'), Length('卸载插件管理器成功...'), 0);
  if PlugEngine.ClientSocket <> nil then
    PlugEngine.ClientSocket.Free;
  PlugEngine.Terminate;
  FreeMem(ProcBlock);
end;

function GetFunAddr(nIndex: Integer): Pointer; stdcall;
begin
  Result := nil;
  if (nIndex >= 0) and (nIndex < High(ProcArray)) then
    Result := ProcArray[nIndex];
end;

exports
  Init, UnInit, GetFunAddr;
begin
  MainOutMessage := OutMessage;
  IsExpired();
end.


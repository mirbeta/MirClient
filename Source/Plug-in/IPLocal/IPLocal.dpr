library IPLocal;

uses
  Winapi.Windows,
  PlugMain in 'PlugMain.pas',
  Share in 'Share.pas',
  SDK in 'SDK.pas',
  Module in 'Module.pas',
  QQWry in 'QQWry.pas';

{$R *.res}
exports
  Init,
  UnInit;

procedure IPDLLHandler(Reason: integer);
begin
  case Reason of
    DLL_PROCESS_DETACH:
      begin
        //ShowMessage('整个DLL的善後程序');
        g_TQQWry.Free;
      end;
    DLL_Process_Attach:
      begin
        //ShowMessage('整个DLL的初始化代码 ');
        g_TQQWry := TQQWry.Create(sIPFileName);
      end;
    DLL_Thread_Attach:
      begin
        //ShowMessage('当主叫端开始一个Thread时');
      end;
    DLL_Thread_Detach:
      begin
        //ShowMessage('当主叫端终止一个Thread时');
      end;
  end;
end;

begin
  //初始化代码
  DLLProc := @IPDLLHandler;
  IPDLLHandler(DLL_Process_Attach);
end.


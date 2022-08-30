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
        //ShowMessage('����DLL���������');
        g_TQQWry.Free;
      end;
    DLL_Process_Attach:
      begin
        //ShowMessage('����DLL�ĳ�ʼ������ ');
        g_TQQWry := TQQWry.Create(sIPFileName);
      end;
    DLL_Thread_Attach:
      begin
        //ShowMessage('�����ж˿�ʼһ��Threadʱ');
      end;
    DLL_Thread_Detach:
      begin
        //ShowMessage('�����ж���ֹһ��Threadʱ');
      end;
  end;
end;

begin
  //��ʼ������
  DLLProc := @IPDLLHandler;
  IPDLLHandler(DLL_Process_Attach);
end.


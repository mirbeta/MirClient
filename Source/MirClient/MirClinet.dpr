program MirClinet;

{$IFDEF CONSOLE}
{$APPTYPE CONSOLE}
{$ENDIF}

{$R *.dres}

uses
  Forms,
  Windows,
  SysUtils,
  ClMain in 'ClMain.pas' {frmMain},
  DrawScrn in 'DrawScrn.pas',
  IntroScn in 'IntroScn.pas',
  PlayScn in 'PlayScn.pas',
  MapUnit in 'MapUnit.pas',
  FState in 'FState.pas' {FrmDlg},
  ClFunc in 'ClFunc.pas',
  cliUtil in 'cliUtil.pas',
  magiceff in 'magiceff.pas',
  SoundUtil in 'SoundUtil.pas',
  Actor in 'Actor.pas',
  HerbActor in 'HerbActor.pas',
  AxeMon in 'AxeMon.pas',
  clEvent in 'clEvent.pas',
  MShare in 'MShare.pas',
  HUtil32 in '..\Common\HUtil32.pas',
  EDcode in '..\Common\EDcode.pas',
  MirEffect in 'MirEffect.pas',
  MaketSystem in 'MaketSystem.pas',
  Grobal2 in '..\Common\Grobal2.pas',
  frmWebBroser in 'frmWebBroser.pas' {frmWebBrowser},
  HashList in '..\Common\HashList.pas',
  HumanActor in 'HumanActor.pas',
  HeroActor in 'HeroActor.pas',
  DxHint in 'DxHint.pas',
  StallSystem in '..\Common\StallSystem.pas',
  MD5 in '..\Common\MD5.pas',
  uSMBIOS in '..\Common\uSMBIOS.pas',
  uDiskSN in '..\Common\uDiskSN.pas',
  uThreadEx in 'uThreadEx.pas',
  CnHashTable in '..\Common\CnHashTable.pas',
  uFirewall in '..\Common\uFirewall.pas',
  uEDCode in '..\Common\uEDCode.pas',
  uLocalMessageer in 'uLocalMessageer.pas',
  uGameClientPaxTypePXL in '..\Common\uGameClientPaxTypePXL.pas',
  DWinCtl in '..\SceneUI\DWinCtl.pas',
  uCommon in '..\SceneUI\uCommon.pas',
  uGameEngine in '..\SceneUI\uGameEngine.pas',
  WIL in '..\SceneUI\WIL.pas',
  SimpleMsgPack in '..\Common\SimpleMsgPack.pas';

{$R *.RES}

begin
  //如果在发行模式下直接启动 将会连接127.0.01 服务器
//  if ParamStr(1) = '' then
//  begin
//    ClientParamStr := 'cx5gN0ZumuLh67G8oIJmWaSROj6mKANZxjKg7JAlun3H9btrP687Obj'+
//    'GUX9lZEh7+rTn6svEjK7rhPSAfc3uZCKqiNFfF4YrBIS1+D7IhUQsEz39CKhlOZ349cIW9zFO'+
//    '3ahHVejDVXlM6oOaEqyWo4FODS0fFe+6L+d20/TvaGCVPpJ1BZj1mSHvuBRA4r7LrqfahWM9i'+
//    'G/Azl9fNAlNBy6W77/7Ol9dEiqPT4PkUn5zsWLLgDLZbhH2V8KonuY87WS+aBczYFfZ4wL0y3'+
//    'yE5GdGOZXu+yL6/9VaqEIaFbOjOplx28VQeUVX8ltkbvlgPpOZmMJWbCMZip7OGw3rEd9FijJ'+
//    'w4zLklbwwZUgd0zsMdrF04qqGHBAx4S3vGnn2KDUECiau8xCVE9wlGTecqsI05WtP63Y9JxpJ'+
//    'zz/8y6PIUO1LMgiPmMAHsnhRM8jrGyX+Tp74mTmnFirexCAGdSFdS50vnYv79trTG3bLXQzwl'+
//    'nEnNHDXpiUvokSiYFfAsOvITfD4IFucH0nW2sWPHC1VdP75ISx4XwA9TpwBEhZ0ZOHQ9JZmwU'+
//    'TOFe8UTJJTobXur6yQlnByo9ERMKbIEQMp82Vjgn1Vr3cPNEDC8/Vaz/8jhvt9Cscni9oFraI'+
//    '8ffHNle3ZYjYck2ZFxAHQLq6OgGhj10D1o6ezP4OkvgCCaqBqZIbt0z3Ozust2fuK9HiZ6zyI'+
//    '2AgZzNkulPQVr+n98ayUL57on7q8bw/T2Xe9jUsjkS7T1hUyFuKHuVE3zwx8IzKvgw2I9eGh1'+
//    '1HJJzc+fjzQ81Vcqet2uhyXftbZK0V8kbw7TS5pEXUGm1FPmiB1oXsfjjvJtOcvfbNjbeJ9Ov'+
//    'MeyxM0Xu6dJ9YlQoVh04Hx93hMfZXGsmfRlHNbZlzEdpJyzmGUXZ76Z7W1hW3ho86LWvIVWlv'+
//    'VxuED67fUJLdngLbma424OfasnrHBtV0p3zhNBA/rC84Dnmk77gt74dm+9XE33QDjLVNc8Bes'+
//    'tf2hz3OMiO1oSBwr4kNQPoWNrw==';
//  end
//  else
//  begin
//{$IFNDEF DEBUG}
    ClientParamStr := ParamStr(1);
//  end;

  if ClientParamStr <> '' then
  begin
    FillChar(g_MirStartupInfo, SizeOf(TMirStartupInfo), #0);
    try
      uEDCode.DecodeSourceData(ClientParamStr, g_MirStartupInfo, SizeOf(TMirStartupInfo));   //这行是默认解密
    //uEDCode.DecodeData(ClientParamStr, g_MirStartupInfo, SizeOf(TMirStartupInfo), '密钥自己乱写'); 自定义加密20200712
    except
      Application.Terminate;
    end;
  end;
//{$ENDIF}
  Application.MainFormOnTaskBar := True;
  Application.Initialize;
  AddApplicationToFirewall('MirClient', Application.ExeName);
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TFrmDlg, FrmDlg);
  Application.CreateForm(TfrmWebBrowser, frmWebBrowser);
  FrmMain.InitializeClient;
  InitObj();
  Application.Run;
end.


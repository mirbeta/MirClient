unit Server_Form;

interface

{$include rtcDefs.inc}

uses             
  Windows, Messages, SysUtils,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, IniFiles,

  ShellApi,

  rtcTypes, rtcInfo, rtcLog, rtcConn,
  rtcDataSrv,

  Server_Module, ComCtrls;

type
  TWebServerForm = class(TForm)
    Panel8: TPanel;
    Panel9: TPanel;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    TabSheet3: TTabSheet;
    TabSheet4: TTabSheet;
    Panel5: TPanel;
    Panel2: TPanel;
    Label6: TLabel;
    Label1: TLabel;
    Label7: TLabel;
    eVirtualHosts: TMemo;
    Panel4: TPanel;
    Splitter1: TSplitter;
    Panel3: TPanel;
    Panel6: TPanel;
    Label8: TLabel;
    eIndexPages: TMemo;
    Panel10: TPanel;
    eContentTypes: TMemo;
    Panel11: TPanel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Panel1: TPanel;
    Label2: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label10: TLabel;
    ePHPDllFolder: TEdit;
    ePHPIniFolder: TEdit;
    ePHPExtensions: TEdit;
    xPHPReady: TCheckBox;
    Panel12: TPanel;
    Label3: TLabel;
    eISAPIExtensions: TEdit;
    btnUnload: TButton;
    Panel7: TPanel;
    lblCliCon: TLabel;
    Label11: TLabel;
    lblDataInOut: TLabel;
    Label12: TLabel;
    btnListen: TButton;
    btnStop: TButton;
    btnInstall: TButton;
    btnDeinstall: TButton;
    Label9: TLabel;
    Label16: TLabel;
    btnSave: TButton;
    Panel13: TPanel;
    xMsgServer: TCheckBox;
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure btnListenClick(Sender: TObject);
    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure btnStopClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnInstallClick(Sender: TObject);
    procedure btnDeinstallClick(Sender: TObject);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure btnUnloadClick(Sender: TObject);
    procedure InfoPanelClick(Sender: TObject);

    procedure ServerDataIn(Sender: TRtcConnection);
    procedure ServerDataOut(Sender: TRtcConnection);
    procedure btnSaveClick(Sender: TObject);

  private
    { Private declarations }
  public
    { Public declarations }
    CliCnt:integer;
    TotalDataIn,
    TotalDataOut:int64;
  end;

var
  WebServerForm: TWebServerForm;

implementation

{$R *.dfm}

procedure TWebServerForm.FormCreate(Sender: TObject);
  var
    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

  begin
  StartLog;

  CliCnt:=0;

  IniName := ChangeFileExt(AppFileName, '.ini');

  if File_Exists(IniName) then
    begin
    XLog('Read LOG "'+RtcString(IniName)+'"');

    SL := TStringList.Create;
    try
      Ini := TMemIniFile.Create(IniName);
      try
        SL.Clear;
        Ini.ReadSectionValues('Hosts',SL);
        if SL.Count>0 then
          eVirtualHosts.Text:=SL.Text;

        SL.Clear;
        Ini.ReadSectionValues('Index Pages',SL);
        if SL.Count>0 then
          eIndexPages.Text:=SL.Text;

        SL.Clear;
        Ini.ReadSectionValues('Content Types',SL);
        if SL.Count>0 then
          eContentTypes.Text:=SL.Text;
      finally
        Ini.Free;
        end;

      Ini := TIniFile.Create(IniName);
      try
        xPHPReady.Checked := Ini.ReadString('PHP','Enable','') = '1';

        ePHPDllFolder.Text := Ini.ReadString('PHP','DllFolder','');
        ePHPIniFolder.Text := Ini.ReadString('PHP','IniFolder','');
        ePHPExtensions.Text := Ini.ReadString('PHP','Extensions','');

        eISAPIExtensions.Text := Ini.ReadString('ISAPI','Extensions','');

        xMsgServer.Checked := Ini.ReadString('Messenger','Enable', '') = '1';
      finally
        Ini.Free;
        end;
    finally
      SL.Free;
      end;
    end;
  end;

procedure TWebServerForm.btnSaveClick(Sender: TObject);
  var
    IniName:string;
    f:TextFile;

  begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  // Write new configuration file ...
  AssignFile(f,IniName);
  {$I-}
  Rewrite(f);
  {$I+}
  if IOResult=0 then
    try
      Writeln(f, '[PHP]');
      if xPHPReady.Checked then
        Writeln(f, 'Enable = 1')
      else
        Writeln(f, 'Enable = 0');

      Writeln(f, 'DllFolder = '+ePHPDllFolder.Text);
      Writeln(f, 'IniFolder = '+ePHPIniFolder.Text);
      Writeln(f, 'Extensions = '+ePHPExtensions.Text);

      Writeln(f, '[ISAPI]');
      Writeln(f, 'Extensions = '+eISAPIExtensions.Text);

      Writeln(f, '[Messenger]');
      if xMsgServer.Checked then
        Writeln(f, 'Enable = 1')
      else
        Writeln(f, 'Enable = 0');

      Writeln(f, '[Hosts]');
      Writeln(f, eVirtualHosts.Text);

      Writeln(f, '[Index Pages]');
      Writeln(f, eIndexPages.Text);

      Writeln(f, '[Content Types]');
      Writeln(f, eContentTypes.Text);

    finally
      CloseFile(f);
    end;
  end;

procedure TWebServerForm.btnListenClick(Sender: TObject);
  begin
  TotalDataIn:=0;
  TotalDataOut:=0;
  lblDataInOut.Caption:='0';

  btnSaveClick(nil);

  Data_Server.OnStart:=ServerListenStart;
  Data_Server.OnStop:=ServerListenStop;
  Data_Server.OnError:=ServerListenError;
  Data_Server.OnConnect:=ServerConnecting;
  Data_Server.OnDisconnect:=ServerDisconnecting;

// OnDataIn and OnDataOut events are used only for display
  Data_Server.ServerHTTP.OnDataIn:=ServerDataIn;
  Data_Server.ServerHTTP.OnDataOut:=ServerDataOut;
  Data_Server.ServerHTTPS.OnDataIn:=ServerDataIn;
  Data_Server.ServerHTTPS.OnDataOut:=ServerDataOut;

  Data_Server.Start;
  end;

procedure TWebServerForm.btnStopClick(Sender: TObject);
  begin
  Data_Server.Stop;
  end;

procedure TWebServerForm.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenError,E)
  else
    ShowMessage('Error starting WebServer!'#13#10 + E.ClassName+'>'+E.Message);
  end;

procedure TWebServerForm.ServerListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStart)
  else
    begin
    lblCliCon.Caption:='Server started.';

    // Disable buttons and edit fields ...
    btnStop.Enabled:=True;
    if Visible then
      btnStop.SetFocus;
    btnUnload.Enabled:=True;

    xPHPReady.Enabled:=False;
    btnListen.Enabled:=False;
    ePHPDllFolder.Enabled:=False;
    ePHPIniFolder.Enabled:=False;
    ePHPExtensions.Enabled:=False;
    eContentTypes.Enabled:=False;
    eVirtualHosts.Enabled:=False;
    eIndexPages.Enabled:=False;
    eISAPIExtensions.Enabled:=False;
    end;
  end;

procedure TWebServerForm.ServerListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerListenStop)
  else
    begin
    lblCliCon.Caption:='Server not listening.';

    // Enable buttons and Edit fields ...
    xPHPReady.Enabled:=True;
    eISAPIExtensions.Enabled:=True;
    ePHPExtensions.Enabled:=True;
    eContentTypes.Enabled:=True;
    ePHPDllFolder.Enabled:=True;
    ePHPIniFolder.Enabled:=True;
    eVirtualHosts.Enabled:=True;
    eIndexPages.Enabled:=True;
    btnListen.Enabled:=True;
    if Visible then
      btnListen.SetFocus;

    btnUnload.Enabled:=False;
    btnStop.Enabled:=False;
    end;
  end;

procedure TWebServerForm.ServerConnecting(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerConnecting)
  else
    lblCliCon.Caption:=IntToStr(Data_Server.ClientCount)+' client(s).';
  end;

procedure TWebServerForm.ServerDisconnecting(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerDisconnecting)
  else
    lblCliCon.Caption:=IntToStr(Data_Server.ClientCount)+' client(s).';
  end;

procedure TWebServerForm.btnInstallClick(Sender: TObject);
  begin
  btnSave.Click;

  ShellExecute(0,'open',PChar(AppFileName),'/INSTALL',nil,SW_SHOW);
  end;

procedure TWebServerForm.btnDeinstallClick(Sender: TObject);
  begin
  ShellExecute(0,'open',PChar(AppFileName),'/UNINSTALL',nil,SW_SHOW);
  end;

procedure TWebServerForm.btnUnloadClick(Sender: TObject);
  begin
  Data_Server.UnloadIsapi;
  end;

procedure TWebServerForm.InfoPanelClick(Sender: TObject);
  begin
  ShellExecute(0,'open',PChar('iexplore.exe'),'http://www.realthinclient.com',nil,SW_SHOW);
  end;

procedure TWebServerForm.ServerDataIn(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerDataIn)
  else
    begin
    TotalDataIn:=TotalDataIn+Sender.DataIn;
    lblDataInOut.Caption:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TWebServerForm.ServerDataOut(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(ServerDataOut)
  else
    begin
    TotalDataOut:=TotalDataOut+Sender.DataOut;
    lblDataInOut.Caption:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

end.

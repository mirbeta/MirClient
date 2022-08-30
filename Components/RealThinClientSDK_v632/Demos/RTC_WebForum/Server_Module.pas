unit Server_Module;

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcLog, rtcSyncObjs,
  rtcInfo, rtcConn, rtcThrPool,
  rtcDataSrv, rtcHttpSrv,

{$IFDEF WIN32}
  rtcPHPProvider,
{$ENDIF}

  rtcFileProvider,
  rtcISAPIProvider,
  rtcMessengerProvider,
  rtcForumProvider;

type
  TData_Server = class(TDataModule)
    Server: TRtcHttpServer;
    procedure DataModuleDestroy(Sender: TObject);

    procedure ServerListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerListenStart(Sender: TRtcConnection);
    procedure ServerListenStop(Sender: TRtcConnection);
    procedure ServerConnecting(Sender: TRtcConnection);
    procedure ServerDisconnecting(Sender: TRtcConnection);
    procedure ServerRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerInvalidRequest(Sender: TRtcConnection);
    procedure ServerDisconnect(Sender: TRtcConnection);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

  public
    { Public declarations }

    procedure UnloadIsapi;

    procedure Start;
    procedure Stop;

    property OnStart:TRtcNotifyEvent read FOnStart write FOnStart;
    property OnStop:TRtcNotifyEvent read FOnStop write FOnStop;
    property OnError:TRtcErrorEvent read FOnError write FOnError;
    property OnConnect:TRtcNotifyEvent read FOnConnect write FOnConnect;
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
  end;

var
  Data_Server: TData_Server;

implementation

{$R *.dfm}

procedure TData_Server.DataModuleDestroy(Sender: TObject);
  begin
  if Server.isListening then
    begin
    Server.StopListen;
    while Server.TotalServerConnectionCount>0 do
      begin
      { You will need to call Application.ProcessMessages *only*
        if your Server connection component is single-threaded
        (MyRtcHttpServer.MultiThreaded=False). You don't need it otherwise.
        For Service applications, always use a MultiThreaed Server. }
      Application.ProcessMessages;
      // So we don't use up all CPU in our loop
      Sleep(10);
      end;
    end;
  end;

procedure TData_Server.Start;
  var
    a:integer;

    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

    web_usePHP,
    web_useMSG,
    web_useForum:boolean;

  begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  XLog('Read LOG "'+IniName+'"');

  SL := TStringList.Create;
  try
    Ini := TIniFile.Create(IniName);
    try
      web_UsePHP := (Ini.ReadString('PHP','Enable','') = '1');

    {$IFDEF WIN32}
      if web_UsePHP then
        begin
        with GetPHPProvider do
          begin
          DllFolder:= Ini.ReadString('PHP','DllFolder','');
          IniFolder := Ini.ReadString('PHP','IniFolder','');

          ClearExts;
          AddExts( Ini.ReadString('PHP','Extensions','') );
          end;
        end;
    {$ENDIF}

      with GetISAPIProvider do
        begin
        ClearExts;
        AddExts( Ini.ReadString('ISAPI','Extensions','') );
        end;

      web_UseMsg := (Ini.ReadString('Messenger','Enable', '') = '1');

      web_UseForum := (Ini.ReadString('Forum','Enable', '') = '1');
      if web_UseForum then
        begin
        with GetForumProvider do
          begin
          ClearContentTypes;
          Init(Ini.ReadString('Forum','Host',''),
               Ini.ReadString('Forum','URI',''),
               Ini.ReadString('Forum','Path',''));
          end;
        end;
    finally
      Ini.Free;
      end;

    Ini := TMemIniFile.Create(IniName);
    try
      with GetFileProvider do
        begin
        ClearHosts;
        SL.Clear;
        Ini.ReadSectionValues('Hosts',SL);
        for a:=0 to SL.Count-1 do
          AddHost(SL[a]);

        ClearIndexPages;
        SL.Clear;
        Ini.ReadSectionValues('Index Pages',SL);
        for a:=0 to SL.Count-1 do
          AddIndexPage(SL[a]);

        ClearContentTypes;
        SL.Clear;
        Ini.ReadSectionValues('Content Types',SL);
        for a:=0 to SL.Count-1 do
          AddContentType(SL[a]);
        end;

      if web_UseForum then
        begin
        with GetForumProvider do
          begin
          SL.Clear;
          Ini.ReadSectionValues('Content Types',SL);
          for a:=0 to SL.Count-1 do
            AddContentType(SL[a]);
          end;
        end;
    finally
      Ini.Free;
      end;
  finally
    SL.Free;
    end;

  // Assign our Server to Data Providers
  GetFileProvider.ServerLink.Server:=Server;
  GetISAPIProvider.ServerLink.Server:=Server;
{$IFDEF WIN32}
  if web_usephp then
    GetPHPProvider.ServerLink.Server:=Server;
{$ENDIF}
  if web_usemsg then
    GetMessengerProvider.ServerLink.Server:=Server;
  if web_useForum then
    GetForumProvider.ServerLink.Server:=Server;

  // Start DataServer
  Server.Listen;
  end;

procedure TData_Server.Stop;
  begin
  if Server.isListening then
    begin
    Server.StopListen;
    while Server.TotalServerConnectionCount>0 do
      begin
      { You will need to call Application.ProcessMessages *only*
        if your Server connection component is single-threaded
        (MyRtcHttpServer.MultiThreaded=False). You don't need it otherwise.
        For Service applications, always use a MultiThreaed Server. }
      Application.ProcessMessages;
      // So we don't use up all CPU in our loop
      Sleep(10);
      end;
    end;
  end;

procedure TData_Server.ServerListenError(Sender: TRtcConnection; E: Exception);
  begin
  XLog('Error starting Web Server!'#13#10 + E.ClassName+'>'+E.Message);
  if assigned(OnError) then
    OnError(Sender,E);
  end;

procedure TData_Server.ServerListenStart(Sender: TRtcConnection);
  begin
  XLog('SERVER STARTED ...');
  if assigned(OnStart) then
    OnStart(Sender);
  end;

procedure TData_Server.ServerListenStop(Sender: TRtcConnection);
  begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
  end;

procedure TData_Server.ServerConnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('++++ '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalClientConnectionCount)+' open]');

  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure TData_Server.ServerDisconnecting(Sender: TRtcConnection);
  begin
  with Sender do
    XLog('---- '+PeerAddr+':'+PeerPort+' ['+IntToStr(TotalClientConnectionCount)+' open]');

  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure TData_Server.ServerRequestNotAccepted(Sender: TRtcConnection);
  begin
  // Anything that comes this far is not acceptable by any DataProvider component.
  with TRtcDataServer(Sender) do
    begin
    XLog('BAD! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Method "'+Request.Method+'" not supported.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    
    Disconnect;
    end;
  end;

procedure TData_Server.ServerInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure TData_Server.ServerDisconnect(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.DataSize > Request.DataIn then
      begin
      // did not receive a complete request
      XLog('ERR! '+PeerAddr+' > '+Request.Host+
           ' "'+Request.Method+' '+Request.URI+'"'+
           ' 0'+
           ' REF "'+Request.Referer+'"'+
           ' AGENT "'+Request.Agent+'" '+
           '> DISCONNECTED while receiving a Request ('+IntToStr(Request.DataIn)+' of '+IntToStr(Request.DataSize)+' bytes received).');
      end;
    end;
  end;

procedure TData_Server.UnloadIsapi;
  begin
  GetISAPIProvider.UnLoad;
  end;

end.

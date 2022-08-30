unit Server_Module;

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{ To compile the project with StreamSec Tools 2.1+ components using demo SSL certificates,
  declare the StreamSecII compiler directive below, in your project or in the "rtcDeploy.inc" file.

  When StreamSecII compiler directive is declared, the Server will be listening on 2 ports.
  Port 80 will be plain HTTP and Port 443 will be using HTTP over SSL (HTTPS). }

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes, IniFiles,

  Forms,

  rtcTypes, rtcLog, rtcSyncObjs,
  rtcInfo, rtcConn, rtcThrPool,
  rtcDataSrv, rtcHttpSrv,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

{$IFDEF WIN32}
  rtcPHPProvider,
{$ENDIF}

  rtcFileProvider,
  rtcISAPIProvider,
  rtcMessengerProvider;

type
  TData_Server = class(TDataModule)
    ServerHTTP: TRtcHttpServer;
    ServerHTTPS: TRtcHttpServer;
    ServerLink: TRtcDualDataServerLink;

    procedure DataModuleCreate(Sender: TObject);

    procedure ServerHTTPListenError(Sender: TRtcConnection; E: Exception);
    procedure ServerHTTPListenStart(Sender: TRtcConnection);
    procedure ServerHTTPListenStop(Sender: TRtcConnection);
    procedure ServerHTTPConnecting(Sender: TRtcConnection);
    procedure ServerHTTPDisconnecting(Sender: TRtcConnection);
    procedure ServerHTTPRequestNotAccepted(Sender: TRtcConnection);
    procedure ServerHTTPInvalidRequest(Sender: TRtcConnection);
    procedure ServerHTTPDisconnect(Sender: TRtcConnection);
  private
    { Private declarations }
    FOnError: TRtcErrorEvent;
    FOnStart: TRtcNotifyEvent;
    FOnStop: TRtcNotifyEvent;
    FOnConnect: TRtcNotifyEvent;
    FOnDisconnect: TRtcNotifyEvent;

    CliCnt:integer;
    function GetClientCount: integer;

  public
    { Public declarations }

    procedure UnloadIsapi;

    procedure Start;
    procedure Stop;

    property ClientCount:integer read GetClientCount;

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

procedure TData_Server.DataModuleCreate(Sender: TObject);
  begin
  CliCnt:=0;
{$IFDEF StreamSecII}
  { In this demo, we will use a 2nd Server to listen
    on the HTTPS Port (443) in addition to the standard
    non-encrypted HTTP server which is listening on Port 80.
    If you only want your Server to work over SSL,
    you do NOT need 2 Server components. You can link the
    ServerCryptPlugin component to "ServerHTTP" (above).
    Also, if you only have a single Server listening on a single Port,
    you do NOT need a TRtcDualDataServerLink component. }

  AddServerRootCertFile(ExtractFilePath(AppFileName)+'root.cer'); // use our demo root certificate
  AddServerPFXFile(ExtractFilePath(AppFileName)+'server.pfx','abc'); // use our demo server PFX file

  ServerHTTPS.CryptPlugin:=GetServerCryptPlugin;
{$ENDIF}
  end;

procedure TData_Server.Start;
  var
    a:integer;

    IniName:string;
    Ini:TCustomIniFile;
    SL:TStringList;

    web_usePHP,
    web_useMSG:boolean;

  begin
  IniName := ChangeFileExt(AppFileName, '.ini');

  XLog('Read LOG "'+RtcString(IniName)+'"');

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
          IniFolder := RtcString(Ini.ReadString('PHP','IniFolder',''));

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
    finally
      Ini.Free;
      end;
  finally
    SL.Free;
    end;

{$IFDEF StreamSecII}
  // Assign our Server to Data Providers
  GetFileProvider.ServerLink.Link:=ServerLink;
  GetISAPIProvider.ServerLink.Link:=ServerLink;
  if web_usephp then
    GetPHPProvider.ServerLink.Link:=ServerLink;
  if web_usemsg then
    GetMessengerProvider.ServerLink.Link:=ServerLink;
{$ELSE}
  // Assign our Server to Data Providers
  GetFileProvider.ServerLink.Server:=ServerHTTP;
  GetISAPIProvider.ServerLink.Server:=ServerHTTP;
{$IFDEF WIN32}
  if web_usephp then
    GetPHPProvider.ServerLink.Server:=ServerHTTP;
{$ENDIF}
  if web_usemsg then
    GetMessengerProvider.ServerLink.Server:=ServerHTTP;
{$ENDIF}

  // Start DataServer
  ServerHTTP.Listen;
{$IFDEF StreamSecII}
  if assigned(ServerHTTPS.CryptPlugin) then
    ServerHTTPS.Listen;
{$ENDIF}
  end;

procedure TData_Server.Stop;
  begin
  ServerHTTP.StopListen;
{$IFDEF StreamSecII}
  ServerHTTPS.StopListen;
{$ENDIF}
  end;

procedure TData_Server.ServerHTTPListenError(Sender: TRtcConnection; E: Exception);
  begin
  XLog('Error starting Web Server!'#13#10 + RtcString(E.ClassName+'>'+E.Message) );
  if assigned(OnError) then
    OnError(Sender,E);
  end;

procedure TData_Server.ServerHTTPListenStart(Sender: TRtcConnection);
  begin
  XLog('SERVER STARTED on Port '+Sender.LocalPort+' ...');
  if assigned(OnStart) then
    OnStart(Sender);
  end;

procedure TData_Server.ServerHTTPListenStop(Sender: TRtcConnection);
  begin
  if assigned(OnStop) then
    OnStop(Sender);
  XLog('SERVER STOPPED.');
  end;

procedure TData_Server.ServerHTTPConnecting(Sender: TRtcConnection);
  begin
  with Sender do XLog('++++ '+PeerAddr+':'+PeerPort+' ['+Int2Str(Sender.TotalClientConnectionCount)+' open]');
  if assigned(OnConnect) then
    OnConnect(Sender);
  end;

procedure TData_Server.ServerHTTPDisconnecting(Sender: TRtcConnection);
  begin
  with Sender do XLog('---- '+PeerAddr+':'+PeerPort+' ['+Int2Str(Sender.TotalClientConnectionCount)+' open]');
  if assigned(OnDisconnect) then
    OnDisconnect(Sender);
  end;

procedure TData_Server.ServerHTTPRequestNotAccepted(Sender: TRtcConnection);
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

procedure TData_Server.ServerHTTPInvalidRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    XLog('ERR! '+PeerAddr+' > "'+Request.Method+' '+Request.FileName+'" > Invalid Request: Header size limit exceeded.');

    Response.Status(400,'Bad Request');
    Write('Status 400: Bad Request');
    end;
  end;

procedure TData_Server.ServerHTTPDisconnect(Sender: TRtcConnection);
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
           '> DISCONNECTED while receiving a Request ('+Int2Str(Request.DataIn)+' of '+Int2Str(Request.DataSize)+' bytes received).');
      end;
    end;
  end;

function TData_Server.GetClientCount: integer;
  begin
  Result:=CliCnt;
  end;

procedure TData_Server.UnloadIsapi;
  begin
  GetISAPIProvider.UnLoad;
  end;

end.

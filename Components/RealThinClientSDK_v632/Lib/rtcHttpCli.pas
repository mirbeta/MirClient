{
  @html(<b>)
  HTTP Client Connection
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  Introducing the @html(<b>) @Link(TRtcHttpClient) @html(</b>) component:
  @html(<br>)
  Client connection component for TCP/IP communication using HTTP requests.
}
unit rtcHttpCli;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,

  rtcTypes,
  rtcLog,
  rtcInfo,
  rtcConn,

  rtcFastStrings,
  rtcPlugins,
  rtcDataCli;

type
  // @exclude
  TRtcHttpClient=class;

  { @Abstract(Login info for authenticating the user on a proxy and/or a secure server) }
  TRtcUserLoginInfo=class(TPersistent)
  private
    FUserName: RtcString;
    FUserPassword: RtcString;
    FCertSubject: RtcString;
    FCertStoreType: TRtcCertStoreType;
    FProxyPassword: RtcString;
    FProxyAddr: RtcString;
    FProxyUserName: RtcString;
    FProxyBypass: RtcString;

    procedure SetUserName(const Value: RtcString); virtual;
    procedure SetUserPassword(const Value: RtcString); virtual;
    procedure SetCertStoreType(const Value: TRtcCertStoreType); virtual;
    procedure SetCertSubject(const Value: RtcString); virtual;
    procedure SetProxyAddr(const Value: RtcString); virtual;
    procedure SetProxyBypass(const Value: RtcString); virtual;
    procedure SetProxyPassword(const Value: RtcString); virtual;
    procedure SetProxyUserName(const Value: RtcString); virtual;

  public
    { Assign our User Login parameters to "Dest" UserLogin object.
      @exclude }
    procedure AssignTo(Dest: TPersistent); override;

    { Is our user login info equal to "dest" user login info? }
    function IsEqual(Dest: TPersistent):boolean;

    { Will be created by TRtcHttpClient component.
      @exclude }
    constructor Create;

    { Will be destroyed by TRtcHttpClient component.
      @exclude }
    destructor Destroy; override;

  published
    { Proxy Address, including http:// or https:// depending on proxy type.
      This property should ONLY be set if you want to use a specific proxy
      and do not want to use default Internet Explorer or WinHTTP settings. }
    property ProxyAddr:RtcString read FProxyAddr write SetProxyAddr;
    { List of domains which should NOT go through the proxy specified in ProxyAddr.
      This option is ONLY used if ProxyAddr is set. @html(<br>)
      When ProxyAddr is not set, default proxy settings will be used. }
    property ProxyBypass:RtcString read FProxyBypass write SetProxyBypass;

    // Proxy Username: needed for proxy servers where the user has to authenticate
    property ProxyUserName:RtcString read FProxyUserName write SetProxyUserName;
    // Proxy Password: needed for proxy servers where the user has to authenticate
    property ProxyPassword:RtcString read FProxyPassword write SetProxyPassword;

    // Username
    property UserName:RtcString read FUserName write SetUserName;
    // Password
    property UserPassword:RtcString read FUserPassword write SetUserPassword;

    // Certificate store tye
    property CertStoreType:TRtcCertStoreType read FCertStoreType write SetCertStoreType default certAny;

    { RtcString under "CN" in Certificate's "Subject" property under "Details",
      or "Issued to" in Certificate's "General" Tab. }
    property CertSubject:RtcString read FCertSubject write SetCertSubject;
    end;

  { @Abstract(Login info for authenticating the user on a proxy and/or a secure server) }
  TRtcHttpUserLogin=class(TRtcUserLoginInfo)
  protected
    // @exclude
    procedure SetUserName(const Value: RtcString); override;
    // @exclude
    procedure SetUserPassword(const Value: RtcString); override;
    // @exclude
    procedure SetCertStoreType(const Value: TRtcCertStoreType); override;
    // @exclude
    procedure SetCertSubject(const Value: RtcString); override;
    // @exclude
    procedure SetProxyAddr(const Value: RtcString); override;
    // @exclude
    procedure SetProxyBypass(const Value: RtcString); override;
    // @exclude
    procedure SetProxyPassword(const Value: RtcString); override;
    // @exclude
    procedure SetProxyUserName(const Value: RtcString); override;

  public
    Con:TRtcHttpClient;

    { Will be created by TRtcHttpClient component.
      @exclude }
    constructor Create;

    { Will be destroyed by TRtcHttpClient component.
      @exclude }
    destructor Destroy; override;
    end;

  { @Abstract(Client Connection component for TCP/IP communication using HTTP requests)

    Received data will be processed by TRtcHttpClient to gather Request
    information and make it easily accessible through the
    @Link(TRtcDataClient.Request) property.
    The same way, your response will be packed into a HTTP result header
    and sent out as a valid HTTP result, readable by any Web Browser.
    @html(<br>)
    @Link(TRtcHttpClient) also makes sure that you receive requests one by one
    and get the chance to answer them one-by-one, even if the client side
    sends all the requests at once (as one big request list), so
    you can relax and process all incomming requests, without worrying
    about overlapping your responses for different requests.
    @html(<br><br>)

    Properties to check first:
    @html(<br>)
    @Link(TRtcConnection.ServerAddr) - Local Address to bind the server to (leave empty for ALL)
    @html(<br>)
    @Link(TRtcConnection.ServerPort) - Port to listen on and wait for connections
    @html(<br><br>)

    Methods to check first:
    @html(<br>)
    @Link(TRtcClient.Connect) - Connect to Server
    @html(<br>)
    @Link(TRtcDataClient.Request), @Link(TRtcHttpClient.WriteHeader), @Link(TRtcHttpClient.Write) - Write (send) Request to Server
    @html(<br>)
    @Link(TRtcDataClient.Response), @Link(TRtcConnection.Read) - Read Server's Response
    @html(<br>)
    @Link(TRtcConnection.Disconnect) - Disconnect from Server
    @html(<br><br>)

    Events to check first:
    @html(<br>)
    @Link(TRtcConnection.OnConnect) - Connected to Server
    @html(<br>)
    @Link(TRtcConnection.OnDataSent) - Data sent to server (buffer now empty)
    @html(<br>)
    @Link(TRtcConnection.OnDataReceived) - Data available from server (check @Link(TRtcDataClient.Response))
    @html(<br>)
    @Link(TRtcHttpClient.OnInvalidResponse) - Received invalid response from Server
    @html(<br>)
    @Link(TRtcConnection.OnDisconnect) - Disconencted from Server
    @html(<br><br>)

    What are the differences between the RTC SDK on different platforms (Windows, MacOSX, iOS and Android)?
    @html(<br><br>)

    The latest RealThinClient SDK version has support for developing Win32, Win64, Mac OSX, iOS and Android Applications. 
    While the Windows platform is fully supported and all the features of the RTC SDK are fully functional 
    when used inside 32bit and 64bit Windows Applications, some features are NOT available for non-Windows platforms, 
    so you shoud take these limitations in account if you want to make your RTC Applications cross-platform.
    Here is a list of features which are ONLY available on Windows:
    @html(<br><br>)

    * Built-in Client-side SSL support is available ONLY for Windows Applications. The "useSSL" property on the 
    TRtcHttpClient component will simply be ignored on all other platforms. The reason for this limitation is that 
    WinInet and WinHTTP APIs are used for built-in SSL support, but these APIs are ONLY available on Windows and 
    it is highly unlikely that they will ever make it to any other platform. SSL support by using 3rd-party encryption 
    components is available on all platforms, but you will need to contact your encryption component vendor 
    (StreamSec or Eldos) to ask if their components can be used for the IDE and platform you are targeting.
    @html(<br><br>)

    * Built-in Client-side automatic Proxy recognition (connect through firewalls by setting the "useProxy" property to TRUE) 
    is available ONLY for Windows Applications. The reason for this limitation is the same as for built-in Client-side SSL 
    support (no WinInet and WinHTTP APIs on non-Windows platforms). This feature will remain available ONLY on Windows.
    @html(<br><br>)

    * When developing FireMonkey applications, you will need to use the 
    "rtcFMX.GUI" or the "rtcFMX_GUI" unit somewhere in your Project. 
    @html(<br><br>)

    "rtcFMX_GUI" unit is required by FireMonkey Applications for iOS compiled with Delphi XE2 or FreePascal. 
    @html(<br><br>)

    "rtcFMX.GUI" unit is required by ALL other FireMonkey Applications. In other words, 
    Firemonkey Applications compiled for Windows or MacOSX with Delphi XE2 and later, as well as 
    FireMonkey Application compiled for iOS with Delphi XE4 and later, and 
    FireMonkey Applications compiled for Android with Delphi XE5 and later 
    all need to have the "rtcFMX.GUI" unit somewhere in their "uses" clause.
    @html(<br><br>)

    That unit is responsible for synchronization between background and foreground threads, which is needed for the 
    WaitForCompletion and Execute methods on FireMonkey. It doesn't matter where you use that unit in your Project. 
    It just has to be somewhere in the "uses" clause (for example, directly in the Project "DPR" file).
    @html(<br><br>)

    Check @Link(TRtcClient) and @Link(TRtcConnection) for more info.
    }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpClient = class(TRtcDataClient)
  private
    FUseLoopLeave: boolean;

    FCryptPlugin:TRtcCryptPlugin;

    FUseProxy:boolean;
    FUseSSL:boolean;
    FUseWinHTTP:boolean;

    FNowLeaving:integer;
    FNeedToLeave:boolean;

    // User Parameters
    FMaxResponseSize:cardinal;
    FMaxHeaderSize:cardinal;
    FOnInvalidResponse:TRtcNotifyEvent;

    // Internal variables
    FWritten:boolean;
    FWriteBuffer:TRtcHugeByteArray;

    FUserLogin: TRtcHttpUserLogin;
    FBlocking: boolean;

    function GetUseProxy: boolean;
    procedure SetUseProxy(const Value: boolean);

    function GetUseSSL: boolean;
    procedure SetUseSSL(const Value: boolean);

    function GetUseWinHTTP: boolean;
    procedure SetUseWinHTTP(const Value: boolean);

    function GetCryptObject: TObject;
    procedure SetBlocking(const Value: boolean);
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    procedure SetUserLogin(const Value: TRtcHttpUserLogin);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure UserDataChange;

    // @exclude
    procedure SetTriggers; override;
    // @exclude
    procedure ClearTriggers; override;
    // @exclude
    procedure SetParams; override;

    // @exclude
    function CreateProvider:TObject; override;


    // @exclude
    procedure TriggerDataSent; override;
    // @exclude
    procedure TriggerDataReceived; override;
    // @exclude
    procedure TriggerDataOut; override;

    // @exclude
    procedure TriggerInvalidResponse; virtual;
    // @exclude
    procedure CallInvalidResponse; virtual;

    // @exclude
    procedure SetRequest(const Value: TRtcClientRequest); override;
    // @exclude
    procedure SetResponse(const Value: TRtcClientResponse); override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    class function New:TRtcHttpClient;

    // @exclude
    procedure LeaveEvent; override;

    { Flush all buffered data.
      @html(<br>)
      When using 'Write' without calling 'WriteHeader' before, all data
      prepared by calling 'Write' will be buffered until your event
      returns to its caller (automatically upon your event completion) or
      when you first call 'Flush'. Flush will check if Request.ContentLength is set
      and if not, will set the content length to the number of bytes buffered.
      @html(<br>)
      Flush does nothing if WriteHeader was called for this response.

      @exclude}
    procedure Flush; override;

    // You can call WriteHeader to send the Request header out.
    procedure WriteHeader(SendNow:boolean=True); overload; override;
    { You can call WriteHeader with empty 'HeaderText' parameter to
      tell the component that you do not want any HTTP header to be sent. }
    procedure WriteHeader(const HeaderText: RtcString; SendNow:boolean=True); overload; override;

    // Use Write to send any Content (document body) out.
    procedure WriteEx(const s:RtcByteArray); override;

    // Use Write to send any Content (document body) out.
    procedure Write(const s:RtcString); override;

    { Encryption object associated with this physical connection.
      The actual type of the object depends on the CryptPlugin component used.
      Will be NIL if Encryption components are not used,
      not initialized or did not update this property. }
    property CryptObject:TObject read GetCryptObject;

  published
    { Setting Blocking to TRUE will make sure that a blocking
      low-level connection provider will be used for all communication. @html(<br><br>)

      Because blocking connection providers do NOT require message queues,
      they are ideal candidates for console and service applications. @html(<br><br>)

      Blocking connection providers can also be faster than non-blocking
      connection providers because they do not rely on windows messages. @html(<br><br>)

      But, a down-side of blocking connection providers is that they
      will NOT inform you of a connection loss unless the connection
      is currently being used for sending or receiving data. @html(<br><br>)

      On the other hand, non-blocking providers will always keep you informed
      of your connection status, regardless of wether the connection is currently
      being used or not. Also, because non-blocking connection providers will NOT
      block the Main Thread when sending or receiving data, they are better suited for
      applications which have a graphical user interface and require users interaction. @html(<br><br>)

      Blocking connection providers can be used in non-blockig mode when
      you set their MultiThreaded property to TRUE, in which case all communication
      will be handled from background threads and you need to use "inMainThread" and
      "Sync" methods to synchronize with the main thread when accessing the GUI. }
    property Blocking:boolean read FBlocking write SetBlocking default False;

    { UserLogin data is ignored when CryptPlugin is assigned. @html(<br><br>)

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      Using this property, you can define login information for a server which
      requires user authentication and/or a client certificate (using WinInet API).
      If you want to use Third-party SSL/SSH components for encryption,
      simply assign the plug-in to the CryptPlugin property. }
    property UserLogin:TRtcHttpUserLogin read FUserLogin write SetUserLogin;

    { UseProxy is ignored (assumed to be FALSE) when CryptPlugin is assigned. @html(<br><br>)

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      When UseProxy is TRUE, connection component will use a WinInet connection provider,
      which supports transfering data over HTTP proxy servers. When UseProxy is FALSE,
      proxy servers will be ignored and component will always try to open a direct
      connection to the server, ignoring any proxy settings in the system. }
    property UseProxy:boolean read GetUseProxy write SetUseProxy default False;

    { UseSSL is ignored (assumed to be TRUE) when CryptPlugin is assigned. @html(<br><br>)

      If CryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      When UseSSL is TRUE, connection component will use a connection provider
      which supports transfering data using the Secure-Socket-Layer (SSL) over
      the HTTPS protocol and send all requests using the HTTPS protocol instead
      of the standard HTTP protocol. When UseSSL is FALSE, standard HTTP protocol
      will be used. Note that RTC Servers do NOT support SSL. }
    property UseSSL:boolean read GetUseSSL write SetUseSSL default False;

    { Set this property if you want to use the WinHTTP API. @html(<br><br>)

      WinHTTP API is blocking, it supports Proxy and SSL options,
      but it ignores any parameters set in the "UserLogin" property.
      WinHTTP can be used instead of WinINET for applications running
      as Windows Services which do not have access to Internet Explorer settings. }
    property UseWinHTTP:boolean read GetUseWinHTTP write SetUseWinHTTP default False;

    { Maximum allowed size of the first response line, without header (0 = no limit).
      This is the first line in a HTTP response and includes Response.StatusCode and Response.StatusText }
    property MaxResponseSize:cardinal read FMaxResponseSize write FMaxResponseSize default 0;
    { Maximum allowed size of each response's header size (0 = no limit).
      This are all the remaining header lines in a HTTP response,
      which come after the first line and end with an empty line,
      after which usually comes the content (document body). }
    property MaxHeaderSize:cardinal read FMaxHeaderSize write FMaxHeaderSize default 0;

    { This event will be called if the received response exceeds your defined
      maximum response or header size. If both values are 0, this event will never be called. }
    property OnInvalidResponse:TRtcNotifyEvent read FOnInvalidResponse write FOnInvalidResponse;

    { To use SSL/SSH encryption with third-party components, simply assign the encryption
      plug-in here before you start using the Client connection (before first connect). }
    property CryptPlugin:TRtcCryptPlugin read FCryptPlugin write SetCryptPlugin;

    { You can set all timeout parameters for the clients underlying API connection or
      default timeout periods for all client connections of the server connection component
      using this property. Check @Link(TRtcTimeoutsOfAPI) for more information. }
    property TimeoutsOfAPI;
    end;

implementation

{$IFNDEF RTC_noWinInet}  {$DEFINE RTC_useWinInet} {$ENDIF}
{$IFNDEF RTC_noWinHttp}  {$DEFINE RTC_useWinHttp} {$ENDIF}
{$IFNDEF RTC_noAsynSock} {$DEFINE RTC_useAsynSock} {$DEFINE RTC_useSockets} {$ENDIF}
{$IFNDEF RTC_noSynSock}  {$DEFINE RTC_useSynSock}  {$DEFINE RTC_useSockets} {$ENDIF}

uses
  SysUtils,
{$IFNDEF RTC_noAsynSock} rtcWinSocket, {$ENDIF}
{$IFNDEF RTC_noSynSock} rtcSynSocket, {$ENDIF}

{$IFDEF RTC_useSockets} {$DEFINE RTC_ProvOK} rtcSocketHttpCliProv, {$ENDIF}
{$IFDEF RTC_useWinHttp} {$DEFINE RTC_ProvOK} rtcWinHttpCliProv, {$ENDIF}
{$IFDEF RTC_useWinInet} {$DEFINE RTC_ProvOK} rtcWInetHttpCliProv, {$ENDIF}
  rtcConnProv;

{$IFDEF RTC_useSockets} type TSocketProv = TRtcSocketHttpClientProvider; {$ENDIF}
{$IFDEF RTC_useWinInet} type TWinInetProv = TRtcWInetHttpClientProvider; {$ENDIF}
{$IFDEF RTC_useWinHttp} type TWinHttpProv = TRtcWinHttpClientProvider; {$ENDIF}

{$IFNDEF RTC_ProvOK}
  {$MESSAGE WARN 'TRtcHttpClient component unusable: You have disabled all Client API support options!'}
{$ENDIF}

{ TRtcHttpClient }

constructor TRtcHttpClient.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FUseLoopLeave:=False;

  FNowLeaving:=0;
  FNeedToLeave:=False;

  FUserLogin:=TRtcHttpUserLogin.Create;
  FUserLogin.Con:=self;

  FUseProxy:=False;
  FUseSSL:=False;
  FUseWinHTTP:=False;

  FWriteBuffer:=TRtcHugeByteArray.Create;
  FWritten:=False;
  end;

destructor TRtcHttpClient.Destroy;
  begin
  try
    RtcFreeAndNil(FUserLogin);
    RtcFreeAndNil(FWriteBuffer);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHttpClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

class function TRtcHttpClient.New: TRtcHttpClient;
  begin
  Result:=Create(nil);
  end;

function TRtcHttpClient.CreateProvider:TObject;
{$IFDEF RTC_useWinInet}
  procedure Create_WinInet;
    begin
    Con:=TWinInetProv.Create;
    FUseLoopLeave:=True;
    end;
{$ENDIF}
{$IFDEF RTC_useWinHttp}
  procedure Create_WinHttp;
    begin
    Con:=TWinHttpProv.Create;
    FUseLoopLeave:=True;
    end;
{$ENDIF}
{$IFDEF RTC_useSockets}
  procedure Create_Socket;
    begin
    if Blocking then
      begin
    {$IFDEF RTC_useSynSock}
      Con:=TSocketProv.Create;
      TSocketProv(Con).SocketClass:=TRtcSynSocket;
      TSocketProv(Con).Blocking:=True;
      FUseLoopLeave:=False;
    {$ELSE}
      {$IFDEF RTC_useWinHttp} if HaveWinHttp then Create_WinHttp else {$ENDIF}
      {$IFDEF RTC_useWinInet} if HaveWinInet then Create_WinInet else {$ENDIF}
      begin
      Con:=TSocketProv.Create;
      TSocketProv(Con).SocketClass:=TRtcWinSocket;
      FUseLoopLeave:=False;
      end;
    {$ENDIF}
      end
    else
      begin
      Con:=TSocketProv.Create;
    {$IFDEF RTC_useAsynSock}
      TSocketProv(Con).SocketClass:=TRtcWinSocket;
      FUseLoopLeave:=False;
    {$ELSE}
      TSocketProv(Con).SocketClass:=TRtcSynSocket;
      TSocketProv(Con).Blocking:=False;
      FUseLoopLeave:=False;
    {$ENDIF}
      end;
    end;
{$ENDIF}
  begin
  if not assigned(Con) then
    begin
    {$IFDEF RTC_useSockets} if assigned(FCryptPlugin) then Create_Socket else {$ENDIF}
    if FUseWinHTTP or (FUserLogin.ProxyAddr<>'') then
      begin
      {$IFDEF RTC_useWinHttp} if HaveWinHTTP then Create_WinHttp else {$ENDIF}
      {$IFDEF RTC_useWinInet} if HaveWinInet then Create_WinInet else {$ENDIF}
      {$IFDEF RTC_useSockets}
        Create_Socket;
      {$ELSE}
        raise Exception.Create('TRtcHttpClient.CreateProvider: No API support. Can not initialize connection.');
      {$ENDIF}
      end
    else if FUseProxy or FUseSSL or FUseWinHTTP or
       (FUserLogin.UserName<>'') or
       (FUserLogin.UserPassword<>'') or
       (FUserLogin.ProxyUserName<>'') or
       (FUserLogin.ProxyPassword<>'') then
      begin
      {$IFDEF RTC_useWinInet} if HaveWinInet then Create_WinInet else {$ENDIF}
      {$IFDEF RTC_useWinHttp} if HaveWinHttp then Create_WinHttp else {$ENDIF}
      {$IFDEF RTC_useSockets}
        Create_Socket;
      {$ELSE}
        raise Exception.Create('TRtcHttpClient.CreateProvider: No API support. Can not initialize connection.');
      {$ENDIF}
      end
    else
    {$IFDEF RTC_useSockets} Create_Socket;
    {$ELSE}
      {$IFDEF RTC_useWinInet} if HaveWinInet then Create_WinInet else {$ENDIF}
      {$IFDEF RTC_useWinHttp} if HaveWinHttp then Create_WinHttp else {$ENDIF}
        raise Exception.Create('TRtcHttpClient.CreateProvider: No API support. Can not initialize connection.');
    {$ENDIF}
    SetTriggers;
    end;
  Result:=Con;
  end;

procedure TRtcHttpClient.SetParams;
  begin
  inherited;
  if assigned(Con) then
    begin
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      begin
      TSocketProv(Con).CryptPlugin:=CryptPlugin;
      TSocketProv(Con).Request:=Request;
      TSocketProv(Con).Response:=Response;
      TSocketProv(Con).MaxResponseSize:=MaxResponseSize;
      TSocketProv(Con).MaxHeaderSize:=MaxHeaderSize;
      TSocketProv(Con).FixupRequest:=FixupRequest;
      TSocketProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
      end
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      begin
      TWinInetProv(Con).useHttps:=FUseSSL;
      TWinInetProv(Con).CertStoreType:=FUserLogin.CertStoreType;
      TWinInetProv(Con).CertSubject:=RtcString(FUserLogin.CertSubject);

      TWinInetProv(Con).UserName:=RtcString(FUserLogin.UserName);
      TWinInetProv(Con).UserPassword:=RtcString(FUserLogin.UserPassword);

      TWinInetProv(Con).ProxyAddr:=RtcString(FUserLogin.ProxyAddr);
      TWinInetProv(Con).ProxyBypass:=RtcString(FUserLogin.ProxyBypass);
      TWinInetProv(Con).ProxyUsername:=RtcString(FUserLogin.ProxyUserName);
      TWinInetProv(Con).ProxyPassword:=RtcString(FUserLogin.ProxyPassword);

      TWinInetProv(Con).Request:=Request;
      TWinInetProv(Con).Response:=Response;
      TWinInetProv(Con).MaxResponseSize:=MaxResponseSize;
      TWinInetProv(Con).MaxHeaderSize:=MaxHeaderSize;
      TWinInetProv(Con).FixupRequest:=FixupRequest;
      TWinInetProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
      end
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      begin
      TWinHttpProv(Con).useHttps:=FUseSSL;
      TWinHttpProv(Con).CertStoreType:=FUserLogin.CertStoreType;
      TWinHttpProv(Con).CertSubject:=FUserLogin.CertSubject;

      TWinHttpProv(Con).UserName:=FUserLogin.UserName;
      TWinHttpProv(Con).UserPassword:=FUserLogin.UserPassword;

      TWinHttpProv(Con).ProxyAddr:=FUserLogin.ProxyAddr;
      TWinHttpProv(Con).ProxyBypass:=FUserLogin.ProxyBypass;
      TWinHttpProv(Con).ProxyUsername:=FUserLogin.ProxyUserName;
      TWinHttpProv(Con).ProxyPassword:=FUserLogin.ProxyPassword;

      TWinHttpProv(Con).Request:=Request;
      TWinHttpProv(Con).Response:=Response;
      TWinHttpProv(Con).MaxResponseSize:=MaxResponseSize;
      TWinHttpProv(Con).MaxHeaderSize:=MaxHeaderSize;
      TWinHttpProv(Con).FixupRequest:=FixupRequest;
      TWinHttpProv(Con).TimeoutsOfAPI:=TimeoutsOfAPI;
      end
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.SetParams: Connection Provider not recognized.');
    end;
  end;

function TRtcHttpClient.GetCryptObject: TObject;
  begin
  if assigned(Con) then
    begin
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      Result:=TSocketProv(Con).CryptObject
    else{$ENDIF}
      Result:=nil;
    end
  else
    Result:=nil;
  end;

procedure TRtcHttpClient.SetTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).SetTriggerInvalidResponse(TriggerInvalidResponse)
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).SetTriggerInvalidResponse(TriggerInvalidResponse)
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).SetTriggerInvalidResponse(TriggerInvalidResponse)
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.SetTriggers: Unsupported connection provider.');
    end;
  end;

procedure TRtcHttpClient.ClearTriggers;
  begin
  inherited;
  if assigned(Con) then
    begin
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).SetTriggerInvalidResponse(nil)
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).SetTriggerInvalidResponse(nil)
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).SetTriggerInvalidResponse(nil)
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.ClearTriggers: Unsupported connection provider.');
    end;
  end;

procedure TRtcHttpClient.WriteHeader(SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).WriteHeader(SendNow)
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).WriteHeader(SendNow)
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).WriteHeader(SendNow)
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.WriteHeader: Unsupported connection provider.');
    end;
  end;

procedure TRtcHttpClient.WriteHeader(const HeaderText: RtcString; SendNow:boolean=True);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Active then
      raise Exception.Create('Error! Sending multiple headers for one request.');

    Timeout.DataSending;
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).WriteHeader(HeaderText, SendNow)
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).WriteHeader(HeaderText, SendNow)
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).WriteHeader(HeaderText, SendNow)
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.WriteHeader: Unsupported connection provider.');
    end;
  end;

procedure TRtcHttpClient.WriteEx(const s: RtcByteArray);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Complete then
      raise Exception.Create('Error! Request already sent, can not send more request data now! Request Header wrong?');

    if Request.Active then
      begin
      { Header is out }

      if Request.ValueCS['CONTENT-LENGTH']<>'' then
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.WriteEx(s);
      end
    else
      begin
      if (Request.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');
        if assigned(Con) then Con.WriteEx(s);
        end
      else
        begin
        { Header is not out.
          Buffer all Write() operations,
          so we can determine content size and write it all out in a flush. }
        FWritten:=True;
        FWriteBuffer.AddEx(s);
        end;
      end;
    end;
  end;

procedure TRtcHttpClient.Write(const s: RtcString);
  begin
  if assigned(Con) and (State<>conInactive) then
    begin
    if Request.Complete then
      raise Exception.Create('Error! Request already sent, can not send more request data now! Request Header wrong?');

    if Request.Active then
      begin
      { Header is out }

      if Request.ValueCS['CONTENT-LENGTH']<>'' then
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');

      { Data size is known or unimportant.
        We can just write the RtcByteString out, without buffering }

      Con.Write(s);
      end
    else
      begin
      if (Request.ValueCS['CONTENT-LENGTH']<>'') and not FWritten then
        begin
        { Content length defined and no data buffered,
          send out header prior to sending first content bytes }
        WriteHeader(length(s)=0);
        if Request.ContentLength - Request.ContentOut < length(s) then
          raise Exception.Create('Error! Sending more data out than specified in header.');
        if assigned(Con) then Con.Write(s);
        end
      else
        begin
        { Header is not out.
          Buffer all Write() operations,
          so we can determine content size and write it all out in a flush. }
        FWritten:=True;
        FWriteBuffer.Add(s);
        end;
      end;
    end;
  end;

procedure TRtcHttpClient.Flush;
  var
    Temp:RtcByteArray;
  begin
  if not FWritten then
    Exit
  else
    FWritten:=False; // so we don't re-enter this method.

  if assigned(Con) and (State<>conInactive) then
    begin
    Timeout.DataSending;

    if Request.Complete then
      raise Exception.Create('Error! Request was already sent! Can not send more data now! Request Header wrong?');

    if not Request.Active then
      begin
      if Request.ValueCS['CONTENT-LENGTH']='' then // length not specified
        begin
        Request.AutoLength:=True;
        Request.ContentLength:=FWriteBuffer.Size;
        end;

    {$IFDEF RTC_useSockets}
      if Con is TSocketProv then
        TSocketProv(Con).WriteHeader(FWriteBuffer.Size=0)
      else{$ENDIF}
    {$IFDEF RTC_useWinInet}
      if Con is TWinInetProv then
        TWinInetProv(Con).WriteHeader(FWriteBuffer.Size=0)
      else{$ENDIF}
    {$IFDEF RTC_useWinHttp}
      if Con is TWinHttpProv then
        TWinHttpProv(Con).WriteHeader(FWriteBuffer.Size=0)
      else{$ENDIF}
        raise Exception.Create('TRtcHttpClient.Flush: Unsupported connection provider.');
      end;

    if FWriteBuffer.Size>0 then
      if assigned(Con) then
        begin
        Temp:=FWriteBuffer.GetEx;
        FWriteBuffer.Clear;
        Con.WriteEx(Temp);
        SetLength(Temp,0);
        end
      else
        FWriteBuffer.Clear;
    end;
  end;

procedure TRtcHttpClient.CallInvalidResponse;
  begin
  if assigned(OnInvalidResponse) then
    OnInvalidResponse(self);
  end;

procedure TRtcHttpClient.TriggerDataReceived;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpClient.TriggerDataSent;
  begin
  if FWriteCount>0 then
    Timeout.DataSent;
  EnterEvent;
  try
    if FWriteCount>0 then
      begin
      CallDataSent;
      Flush;
      end;

    if not isClosing then
      begin
      CallReadyToSend;
      Flush;
      end;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcHttpClient.TriggerDataOut;
  begin
  inherited;
  Flush;
  end;

procedure TRtcHttpClient.TriggerInvalidResponse;
  begin
  EnterEvent;
  try
    CallInvalidResponse;
    Flush;

    Disconnect;
  finally
    LeaveEvent;
    end;
  end;

procedure TRtcHttpClient.SetRequest(const Value: TRtcClientRequest);
  begin
  inherited SetRequest(Value);
  if assigned(Con) then
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).Request:=Request
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).Request:=Request
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).Request:=Request
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.SetRequest: Unsupported connection provider.');
  end;

procedure TRtcHttpClient.SetResponse(const Value: TRtcClientResponse);
  begin
  inherited SetResponse(Value);
  if assigned(Con) then
  {$IFDEF RTC_useSockets}
    if Con is TSocketProv then
      TSocketProv(Con).Response:=Response
    else{$ENDIF}
  {$IFDEF RTC_useWinInet}
    if Con is TWinInetProv then
      TWinInetProv(Con).Response:=Response
    else{$ENDIF}
  {$IFDEF RTC_useWinHttp}
    if Con is TWinHttpProv then
      TWinHttpProv(Con).Response:=Response
    else{$ENDIF}
      raise Exception.Create('TRtcHttpClient.SetResponse: Unsupported connection provider.');
  end;

function TRtcHttpClient.GetUseProxy: boolean;
  begin
  Result:=FUseProxy;
  end;

procedure TRtcHttpClient.SetUseProxy(const Value: boolean);
  begin
  if Value<>FUseProxy then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change UseProxy after Connect.')
      else
        ReleaseProvider;
    FUseProxy:=Value;
    end;
  end;

function TRtcHttpClient.GetUseSSL: boolean;
  begin
  Result:=FUseSSL;
  end;

procedure TRtcHttpClient.SetUseSSL(const Value: boolean);
  begin
  if Value<>FUseSSL then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change UseSSL after Connect.')
      else
        ReleaseProvider;
    FUseSSL:=Value;
    end;
  end;

function TRtcHttpClient.GetUseWinHTTP: boolean;
  begin
  Result:=FUseWinHTTP;
  end;

procedure TRtcHttpClient.SetUseWinHTTP(const Value: boolean);
  begin
  if Value<>FUseWinHTTP then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change UseWinHTTP after Connect.')
      else
        ReleaseProvider;
    FUseWinHTTP:=Value;
    end;
  end;

procedure TRtcHttpClient.UserDataChange;
  begin
  if assigned(Con) then
    if isConnecting then
      Error('Can not change UserLogin data after Connect.')
    else
      ReleaseProvider;
  end;

type
  http_proc=procedure of object;

procedure TRtcHttpClient.LeaveEvent;
  procedure LoopLeave(myproc:http_proc);
    begin
    FNeedToLeave:=True;
    Inc(FNowLeaving);
    try
      if FNowLeaving=1 then
        repeat
          FNeedToLeave:=False;
          myproc;
          until not FNeedToLeave;
    finally
      Dec(FNowLeaving);
      end;
    end;
  begin
  inherited;
  if FUseLoopLeave then
    if not InsideEvent then
      if not assigned(Con) then
        Exit
    {$IFDEF RTC_useWinInet}
      else if Con is TWinInetProv then
        LoopLeave(TWinInetProv(Con).LeavingEvent)
    {$ENDIF}
    {$IFDEF RTC_useWinHttp}
      else if Con is TWinHttpProv then
        LoopLeave(TWinHttpProv(Con).LeavingEvent)
    {$ENDIF};
  end;

procedure TRtcHttpClient.SetBlocking(const Value: boolean);
  begin
  if FBlocking<>Value then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change Blocking when the Client is connected.')
      else
        ReleaseProvider;
    FBlocking:=Value;
    end;
  end;

procedure TRtcHttpClient.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  if FCryptPlugin<>Value then
    begin
    if assigned(Con) then
      if isConnecting then
        Error('Can not change CryptPlugin when the Client is connected.')
      else
        ReleaseProvider;
    FCryptPlugin := Value;
    end;
  end;

procedure TRtcHttpClient.SetUserLogin(const Value: TRtcHttpUserLogin);
  begin
  if Value<>FUserLogin then
  	FUserLogin.Assign(Value);
  end;

procedure TRtcHttpClient.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FCryptPlugin then
      SetCryptPlugin(nil);
  end;

{ TRtcUserLoginInfo }

procedure TRtcUserLoginInfo.AssignTo(Dest: TPersistent);
  begin
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcUserLoginInfo) then
    if not IsEqual(Dest) then
      begin
      TRtcUserLoginInfo(Dest).ProxyAddr:=ProxyAddr;
      TRtcUserLoginInfo(Dest).ProxyBypass:=ProxyBypass;
      TRtcUserLoginInfo(Dest).ProxyUserName:=ProxyUserName;
      TRtcUserLoginInfo(Dest).ProxyPassword:=ProxyPassword;
      TRtcUserLoginInfo(Dest).UserName:=ProxyUserName;
      TRtcUserLoginInfo(Dest).UserPassword:=UserPassword;
      TRtcUserLoginInfo(Dest).CertStoreType:=CertStoreType;
      TRtcUserLoginInfo(Dest).CertSubject:=CertSubject;
      end;
  end;

function TRtcUserLoginInfo.IsEqual(Dest: TPersistent):boolean;
  begin
  Result:=True;
  if assigned(Dest) and (Dest<>self) and (Dest is TRtcUserLoginInfo) then
    begin
    Result:= (TRtcUserLoginInfo(Dest).ProxyAddr = ProxyAddr) and
             (TRtcUserLoginInfo(Dest).ProxyBypass = ProxyBypass) and
             (TRtcUserLoginInfo(Dest).ProxyUserName = ProxyUserName) and
             (TRtcUserLoginInfo(Dest).ProxyPassword = ProxyPassword) and
             (TRtcUserLoginInfo(Dest).UserName = ProxyUserName) and
             (TRtcUserLoginInfo(Dest).UserPassword = UserPassword) and
             (TRtcUserLoginInfo(Dest).CertStoreType = CertStoreType) and
             (TRtcUserLoginInfo(Dest).CertSubject = CertSubject);
    end;
  end;

constructor TRtcUserLoginInfo.Create;
  begin
  inherited;
  FCertStoreType:=certAny;
  end;

destructor TRtcUserLoginInfo.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcUserLoginInfo.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

// -----

procedure TRtcUserLoginInfo.SetCertStoreType(const Value: TRtcCertStoreType);
  begin
  FCertStoreType := Value;
  end;

procedure TRtcUserLoginInfo.SetCertSubject(const Value: RtcString);
  begin
  FCertSubject := Value;
  end;

procedure TRtcUserLoginInfo.SetProxyAddr(const Value: RtcString);
  begin
  FProxyAddr := Value;
  end;

procedure TRtcUserLoginInfo.SetProxyBypass(const Value: RtcString);
  begin
  FProxyBypass := Value;
  end;

procedure TRtcUserLoginInfo.SetProxyPassword(const Value: RtcString);
  begin
  FProxyPassword := Value;
  end;

procedure TRtcUserLoginInfo.SetProxyUserName(const Value: RtcString);
  begin
  FProxyUserName := Value;
  end;

procedure TRtcUserLoginInfo.SetUserName(const Value: RtcString);
  begin
  FUserName := Value;
  end;

procedure TRtcUserLoginInfo.SetUserPassword(const Value: RtcString);
  begin
  FUserPassword := Value;
  end;

// ------

constructor TRtcHttpUserLogin.Create;
  begin
  inherited;
  FCertStoreType:=certAny;
  end;

destructor TRtcHttpUserLogin.Destroy;
  begin
  try
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcHttpUserLogin.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcHttpUserLogin.SetCertStoreType(const Value: TRtcCertStoreType);
  begin
  if Value<>FCertStoreType then
    begin
    Con.UserDataChange;
    FCertStoreType := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetCertSubject(const Value: RtcString);
  begin
  if Value<>FCertSubject then
    begin
    Con.UserDataChange;
    FCertSubject := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetProxyAddr(const Value: RtcString);
  begin
  if Value<>FProxyAddr then
    begin
    Con.UserDataChange;
    FProxyAddr := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetProxyBypass(const Value: RtcString);
  begin
  if Value<>FProxyBypass then
    begin
    Con.UserDataChange;
    FProxyBypass := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetProxyPassword(const Value: RtcString);
  begin
  if Value<>FProxyPassword then
    begin
    Con.UserDataChange;
    FProxyPassword := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetProxyUserName(const Value: RtcString);
  begin
  if Value<>FProxyUserName then
    begin
    Con.UserDataChange;
    FProxyUserName := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetUserName(const Value: RtcString);
  begin
  if Value<>FUserName then
    begin
    Con.UserDataChange;
    FUserName := Value;
    end;
  end;

procedure TRtcHttpUserLogin.SetUserPassword(const Value: RtcString);
  begin
  if Value<>FUserPassword then
    begin
    Con.UserDataChange;
    FUserPassword := Value;
    end;
  end;

end.

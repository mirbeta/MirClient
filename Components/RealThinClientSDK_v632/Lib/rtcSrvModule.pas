{
  @html(<b>)
  Server Module for Remote Functions
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit introduces @Link(TRtcServerModule), a server-side component for ENABLING remote functions.
  Implementing RTC Remote Functions is as easy as writing local functions.
}

unit rtcSrvModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  SysUtils,

  rtcTypes,
  memObjList,

  rtcLog,
  rtcSyncObjs,
  rtcThrPool,
  rtcTimer,
  rtcInfo,
  rtcLink,
  rtcConn,
  rtcCrypt,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcFastStrings,
  rtcDataSrv,
  rtcFunction;

const
  // @exclude
  RTC_SERVERMODULE_DELAYED_CALL='$RTC_SERVERMODULE_DELAYED_CALL$';

type
  TRtcEncryptionClass = TRtcCrypt;

  { @abstract(Server Remote RTC Object Manager implementation) }
  TRtcServerObjectManager = class(TRtcBasicRemoteObjectManager);

  // @exclude
  TRtcCryptServer=class(TRtcObject)
  public
    HaveHello,HaveStart:boolean;
    ControlCounter:integer;

    ClientHello,ServerHello,
    ClientKey,ServerKey,
    ControlKey:RtcString;

    CRead,CWrite:TRtcEncryptionClass;

    constructor Create;
    destructor Destroy; override;

    procedure Init;
    procedure Kill; override;
    end;

  { @abstract(Delayed Function Call object)
    This object is returned from the PrepareDelayedCall function and
    can be used to Wake up the delayed call. The only method you are
    allowed to use on this object is "WakeUp", and this one may
    only be called once, after which you have to "forget" about that object
    (set your variable's pointer to NIL), so you don't go and call it often. }
  TRtcDelayedCall = class(TRtcObject)
  protected
    SessionID:RtcString;
    crypt:TRtcCryptServer;
    output:TRtcHugeString;
    Compress:TRtcCompressLevel;
    FMT:TRtcDataFormat;

    msDelay:integer;
    Param:TRtcFunctionInfo;
    CallEvent:TRtcFunctionCallEvent;

    ObjectLinkSupport:TRtcObjectLinkSupport;
    ObjectDataOutEvent: TRtcDataEvent;

    Called:boolean;
    Timer:TRtcTimer;
    WaitEvent:TRtcEvent;
    Conn:TRtcConnection;

    procedure Post(Multi_Threaded:boolean);

    procedure Execute;

    procedure Cancel;

  public
    // @exclude
    constructor Create;
    // @exclude
    destructor Destroy; override;

    // @exclude
    procedure Kill; override;

    // Use this method to wake the delayed call up,
    // so it will be called NOW! instead of when the timeout period has expired.
    procedure WakeUp;
    end;

  // @exclude
  EDelayedCall = class(EAbort)
  public
    call:TRtcDelayedCall;
    end;

  { @abstract(Accepts the request and uses a TRtcFunctionGroup component to
    execute received functions and prepares the result)

    ModuleProvider is the Remote Object execution point, that enables the Client(s) to
    send one or more objects to the server and get executed object(s) as a result.
    If there are any function calls found inside the objects received, those functions
    will be executed, so that the resulting object contains only data. That resulting
    object (or a set of objects) will be sent back to the client who sent the request.
    In case of an exception, execution will be aborted and the last object sent to the
    client will be an exception message: isType = rtc_Exception; asException = error message; @html(<br><br>)

    Raising an exception in any event implemented for the Module Provider,
    will result in sending an Exception object back to the client. }
  TRtcBaseServerModule=class(TRtcAbsDataServerLink)
  private
    FFunctions:TRtcFunctionGroup;
    FModuleFileName:RtcString;
    FModuleHost:RtcString;
    FCryptSesName:RtcString;
    FObjManSesName:RtcString;
    FAutoSessionsLive: integer;
    FAutoSessionsLock: TRtcSessionLockType;

    FOnListenStart:TRtcNotifyEvent;
    FOnListenStop:TRtcNotifyEvent;
    FOnRequestAccepted:TRtcNotifyEvent;
    FOnResponseDone:TRtcNotifyEvent;
    FOnDisconnect:TRtcNotifyEvent;
    FOnSessionOpen:TRtcNotifyEvent;
    FOnSessionClose:TRtcNotifyEvent;

    FOnObjectDataOut: TRtcDataEvent;
    FOnObjectDataIn: TRtcDataEvent;
    FOnObjectCreate: TRtcObjectCreateEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FSecureKey: RtcString;
    FAutoSessions: boolean;
    FCompress: TRtcCompressLevel;

    FDataFormats: TRtcDataFormatSupport;
    FObjectLinkSupport: TRtcObjectLinkSupport;

    function GetCrypt(Session:TRtcSession):TRtcCryptServer;
    procedure NewCrypt(Session:TRtcSession);
    procedure DelCrypt(Session:TRtcSession);

    function GetFunctionGroup: TRtcFunctionGroup;
    procedure SetFunctionGroup(const Value: TRtcFunctionGroup);
    function GetModuleFileName: RtcString;
    procedure SetModuleFileName(const Value: RtcString);
    function GetModuleHost: RtcString;
    procedure SetModuleHost(const Value: RtcString);
    procedure SetAutoEncrypt(const Value: integer);
    procedure SetAutoSessions(const Value: boolean);
    procedure SetForceEncrypt(const Value: boolean);
    procedure SetCompress(const Value: TRtcCompressLevel);
    procedure SetAutoSessionsLock(const Value: TRtcSessionLockType);
    procedure SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);

    procedure DoExecute(Sender:TRtcDataServer; var MyData:TRtcValueObject);
    procedure DoObjectCreate(Sender:TObject; Param:TRtcObjectCall);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_ListenStart(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ListenStop(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_CheckRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RequestAccepted(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_DataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataOut(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_DataSent(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ReadyToSend(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_Disconnect(Sender:TRtcConnection); override;

    // @exclude
    function GetObjectManager(Sender:TRtcDataServer):TRtcRemoteObjectManager; override;
    // @exclude
    procedure ActivateObjectManager(Sender:TRtcDataServer; xCreate:boolean=True); override;
    // @exclude
    function MakeObjectManager(Sender:TRtcDataServer; xCreate:boolean=True):TRtcRemoteObjectManager;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

  protected
    { Use this property to define what compression level you want to use when sending
      data from Server to client. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your server has to
      work with clients which don't support compression, you have to use "cNone". @html(<br><br>)

      If you have to work with Clients which support compression and those which don't,
      you should use two TRtcServerModule components (one with and one without compression)
      and configure Clients to use the correct TRtcServerModule (see "ModuleFileName" property). }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;

    { Use this property to define what Data Formats you want to accept for this ServerModule.
      Since this is a set, you can choose one supported format or all supported formats. }
    property DataFormats:TRtcDataFormatSupport read FDataFormats write FDataFormats default [fmt_RTC];

    { Set this property to a value other than 0 if you want to enable automatic
      Encryption for clients which have their EncryptionKey option activated.
      EncryptionKey value defines the length on the encryption key, in bytes.
      One byte represents encryption strength of 8 bits. To have 256-bit encryption,
      set EncryptionKey=32. @html(<br><br>)

      The final encryption key is combined from a key received by the client
      and a key generated by this ServerModule. When EncryptionKey is >0 by the
      ClientModule doing the call AND by this ServerModule, Encryption handshake will
      be done automatically by those two components, so that the user only has to set
      the values and use the components as if there is no encryption.

      If ServerModule's EncryptionKey property is 0 (zero), server will not allow data to be
      encrypted and all communication with all clients will flow without encryption.
      Clients which have ForceEncryption set to True, will not work if the server
      doesn't want to support encryption. If you need to work with Clients which require
      encryption and clients which don't, you should use two TRtcServerModule components. }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Server Module. When "ObjectLinks=ol_None", calling the
        "TRtcDataServer(Sender).ActivateObjectManager;" method will also raise an exception
        because any RTC Linked Objects created this way would NOT be sent to the Client. @html(<br><br>)

        Because a single Server can potentially host any number of different Applications
        and handle requests from any number of different Clients, there is no reason why
        a single Server shouldn't have more than one "TRtcServerModule" component using the
        "RTC Linked Objects" feature. But, to keep the Client-side implementation simple,
        it is best to use only *one* "TRtcServerModule" per "Client Application" and
        customize each "TRtcServerModule" to specific needs of each Client Application. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to force the Client to call a remote
        function on the Server which will execute the "TRtcDataServer(Sender).ActivateObjectManager;"
        method before any "Linked Objects" can be created (from Client or Server side). If there is no
        active Session and you use "ActiveObjectManager(True)", a new Session will also be created,
        after which a new Object Manager will be created and assigned to the Session. @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" (or "ol_AutoBoth") if you want an Object Manager to be created
        automatically by the Server if "Linked Objects" data is received from a Client, allowing Clients to
        start creating Linked Objects without having to call a remote function and use "ActivateObjectManager"
        first (see "ol_Manual"). Please note that a Session is also required before an Object Manager can
        be created, since the Object Manager is stored inside a Session. If you also want the Session to
        be created automatically for the Client, set the "AutoSessions" property to TRUE.

      - Set "ObjectLinks" to "ol_AutoServer" (or "ol_AutoBoth") if an Object Manager should be created
        and/or activated automatically before each remote function linked to this Server Module gets executed,
        so there will be no need to explicitly call "ActivateObjectManager" from these functions OnExecute events.
        If "ObjectLinks" is "ol_AutoServer" (or "ol_AutoBoth"), because an Object Manager requires an active Session,
        the AutoSessions property also has to be TRUE for a Session to be created automatically, or a Session
        has to be opened manually by using a remote function linked to another Server Module.

      - Setting "ObjectLinks" to "ol_AutoBoth" is the equivalent of combining "ol_AutoClient" and "ol_AutoServer". }
    property ObjectLinks:TRtcObjectLinkSupport read FObjectLinkSupport write SetObjectLinkSupport default ol_None;

    { If you need a 100% secure connection, define a Secure Key String
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty String (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to decrypt the first key combination received by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey:RtcString read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ServerModule to work with
      Clients ONLY if they requested to use encryption. If AutoEncryptKey is > 0 and
      Client doesn't request encryption, function calls will not be processed
      and any request coming from the client will be rejected, until
      client requests and initializes encryption. }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Using this property, you define how long a session will live (in seconds)
      when there are no requests from this client and the session was
      created by a call from ClientModule that has its AutoSessions property enabled.
      The higher this value, the longer a session will stay valid when there
      are no requests coming from the client for which the session was created.
      This value will be used only after the Client has sent a valid request
      which produces a valid response from the server. Before that, a default
      Session Live time of @Link(RTC_SESSION_TIMEOUT) seconds will be used. @html(<br><br>)

      Session Live counter is reset each time a new request is received from the same client,
      so that this parameter only removes sessions which are inactive longer than
      AutoSessionsLive seconds. To keep your server from nasty clients creating tons of
      sessions and leaving them inactive, keep this property under 600 seconds,
      even if you want your session to stay alive for a long time. You do not have to
      overexagurate this value, since every session consumes memory and client sessions which are not
      needed will ONLY THEN be removed from memory when this AutoSessionsLive timeout expires. }
    property AutoSessionsLive:integer read FAutoSessionsLive write FAutoSessionsLive default 0;

    { Set this property to TRUE if you want ClientModule's to be able to
      reqest a new session automatically by using the NEWID parameter,
      or if you want a Session to be created automatically when "Linked Objects"
      data is received and the "ObjectLinks" property is set to "ol_Auto*". @html(<br><br>)

      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie['ID'] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
      ServerModule components, all TRtcFunction and TRtcResult components used
      by this ClientModule will have direct access to the session object.
      When AutoSessions is set to TRUE, Client's can automaticaly request a new
      Session if no Session exists, or when a Session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new Session by calling
      a remote server function to generate a new Session. Opening a Session from
      a remote function will notify the Client about the new Session (no need to
      manually send the Session ID to the Client as a result of a remote function). }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { When AutoSessions are used, you can define what client data you want to use to lock the
      Sessions to this client and deny access to session data from other clients. }
    property AutoSessionsLock:TRtcSessionLockType read FAutoSessionsLock write SetAutoSessionsLock default sesFwdLock;

    { If ModuleHost is specified, then Request.Host will be compared to the ModuleHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If ModuleHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property ModuleHost:RtcString read GetModuleHost write SetModuleHost;

    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this ServerModule. Any request asking
      for this FileName will be processed by this ServerModule component.
      Since parameters are passed to the server module through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this module. }
    property ModuleFileName:RtcString read GetModuleFileName write SetModuleFileName;

    { Set this property to tell the RtcServerModule to use this TRtcFunctionGroup
      component to execute all functions received as a request from clients. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - Connection component through which data was received)
      @param(Data - Data received from remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn:TRtcDataEvent read FOnObjectDataIn write FOnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - Connection component which will be used to send the data out)
      @param(Data - Data prepared by our local "Object Manager" for sending to remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be sent over to remote "Object Manager") }
    property OnObjectDataOut:TRtcDataEvent read FOnObjectDataOut write FOnObjectDataOut;

    { This event is triggered when the remote Object Manager has requested
      our Object Manager to create a new Object (remote instance was already created)
      and allows you to create Objects which don't have a global constructor
      registered (through the global "RegisterRtcObjectConstructor" procedure).

      Objects which you do NOT want to have created automatically by the remote
      side, but where you still want to allow controlled creation should NOT have
      a global constructor registered and should be created from THIS event instead. }
    property OnObjectCreate:TRtcObjectCreateEvent read FOnObjectCreate write FOnObjectCreate;

    { Event to be triggered after a new Session was opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { Event to be triggered before an existing Session was to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;
    { Event to be triggered when the Server starts listening on the connection.
      You can use this event for component initialization. }
    property OnListenStart:TRtcNotifyEvent read FOnListenStart write FOnListenStart;
    { Event to be triggered when the Server stops listening on the connection.
      You can use this event for component deinitialization. }
    property OnListenStop:TRtcNotifyEvent read FOnListenStop write FOnListenStop;
    { Event to be triggered when a child DataProvider component accepts the Request.
      You can use this event for request initialization. For example,
      you could create a DataTunel and assign it to Tunel, to have reuqest data tunneled. }
    property OnRequestAccepted:TRtcNotifyEvent read FOnRequestAccepted write FOnRequestAccepted;
    { Event to be triggered after the response was sent out (Response.Done) }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect:TRtcNotifyEvent read FOnDisconnect write FOnDisconnect;
    end;

  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcServerModule=class(TRtcBaseServerModule)
  published
    { Use this property to define what compression level you want to use when sending
      data from Server to client. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your server has to
      work with clients which don't support compression, you have to use "cNone". }
    property Compression;

    { Use this property to define what Data Formats you want to accept for this ServerModule.
      Since this is a set, you can choose one supported format or all supported formats. }
    property DataFormats;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Server Module. When "ObjectLinks=ol_None", calling the
        "TRtcDataServer(Sender).ActivateObjectManager;" method will also raise an exception
        because any RTC Linked Objects created this way would NOT be sent to the Client. @html(<br><br>)

        Because a single Server can potentially host any number of different Applications
        and handle requests from any number of different Clients, there is no reason why
        a single Server shouldn't have more than one "TRtcServerModule" component using the
        "RTC Linked Objects" feature. But, to keep the Client-side implementation simple,
        it is best to use only *one* "TRtcServerModule" per "Client Application" and
        customize each "TRtcServerModule" to specific needs of each Client Application. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to force the Client to call a remote
        function on the Server which will execute the "TRtcDataServer(Sender).ActivateObjectManager;"
        method before any "Linked Objects" can be created (from Client or Server side). If there is no
        active Session and you use "ActiveObjectManager(True)", a new Session will also be created,
        after which a new Object Manager will be created and assigned to the Session. @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" (or "ol_AutoBoth") if you want an Object Manager to be created
        automatically by the Server if "Linked Objects" data is received from a Client, allowing Clients to
        start creating Linked Objects without having to call a remote function and use "ActivateObjectManager"
        first (see "ol_Manual"). Please note that a Session is also required before an Object Manager can
        be created, since the Object Manager is stored inside a Session. If you also want the Session to
        be created automatically for the Client, set the "AutoSessions" property to TRUE.

      - Set "ObjectLinks" to "ol_AutoServer" (or "ol_AutoBoth") if an Object Manager should be created
        and/or activated automatically before each remote function linked to this Server Module gets executed,
        so there will be no need to explicitly call "ActivateObjectManager" from these functions OnExecute events.
        If "ObjectLinks" is "ol_AutoServer" (or "ol_AutoBoth"), because an Object Manager requires an active Session,
        the AutoSessions property also has to be TRUE for a Session to be created automatically, or a Session
        has to be opened manually by using a remote function linked to another Server Module.

      - Setting "ObjectLinks" to "ol_AutoBoth" is the equivalent of combining "ol_AutoClient" and "ol_AutoServer". }
    property ObjectLinks;

    { Set this property to a value other than 0 if you want to enable automatic
      Encryption for clients which have their EncryptionKey option activated.
      EncryptionKey value defines the length on the encryption key, in bytes.
      One byte represents encryption strength of 8 bits. To have 256-bit encryption,
      set EncryptionKey=32. @html(<br><br>)

      The final encryption key is combined from a key received by the client
      and a key generated by this ServerModule. When EncryptionKey is >0 by the
      ClientModule doing the call AND by this ServerModule, Encryption handshake will
      be done automatically by those two components, so that the user only has to set
      the values and use the components as if there is no encryption.

      If ServerModule's EncryptionKey property is 0 (zero), server will not allow data to be
      encrypted and all communication with all clients will flow without encryption.
      Clients which have ForceEncryption set to True, will not work if the server
      doesn't want to support encryption. }
    property EncryptionKey;

    { If you need a 100% secure connection, define a Secure Key String
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty String (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to decrypt the first key combination received by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey;

    { Setting this property to TRUE will tell the ServerModule to work with
      Clients ONLY if they requested to use encryption. If AutoEncryptKey is > 0 and
      Client doesn't request encryption, function calls will not be processed
      and any request coming from the client will be rejected, until
      client requests and initializes encryption. }
    property ForceEncryption;

    { Using this property, you define how long a session will live (in seconds)
      when there are no requests from this client and the session was
      created by a call from ClientModule that has its AutoSessions property enabled.
      The higher this value, the longer a session will stay valid when there
      are no requests coming from the client for which the session was created.
      This value will be used only after the Client has sent a valid request
      which produces a valid response from the server. Before that, a default
      Session Live time of @Link(RTC_SESSION_TIMEOUT) seconds will be used. @html(<br><br>)

      Session Live counter is reset each time a new request is received from the same client,
      so that this parameter only removes sessions which are inactive longer than
      AutoSessionsLive seconds. To keep your server from nasty clients creating tons of
      sessions and leaving them inactive, keep this property under 600 seconds,
      even if you want your session to stay alive for a long time. You do not have to
      overexagurate this value, since every session consumes memory and client sessions which are not
      needed will ONLY THEN be removed from memory when this AutoSessionsLive timeout expires. }
    property AutoSessionsLive;

    { Set this property to TRUE if you want ClientModule's to be able to
      reqest a new session automatically by using the NEWID parameter.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie['ID'] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
      ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, Client's can automaticaly request a new
      session is no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions;

    { When AutoSessions are used, you can define what client data you want to use to lock the
      Sessions to this client and deny access to session data from other clients. }
    property AutoSessionsLock;

    { If ModuleHost is specified, then Request.Host will be compared to the ModuleHost
      property to decide if this request should be processed by this ServerModule. @html(<br>)
      If your DataServer has to serve more different hosts, while your ServerModule
      is not supposed to react to requests from all those hosts, you can assign the
      host name to which this ServerModule belongs to. If ModuleHost is left blank,
      then this ServerModule will respond to any request asking for this servermodule's
      ModuleFileName, regardless of the HOST header. @html(<br><br>)

      To process all requests for a domain and all of its sub-domains, enter domain name ONLY
      (example: "realthinclient.com" for "realthinclient.com" and any sub-domain like
      "www.realthinclient.com", "mymod.myapp.realthinclient.com", etc). @html(<br>)
      To limit the requests to a sub-domain and its sub-sub-domains, enter the name
      of the highest sub-domain (example: "myapp.realthinclient.com" for
      "myapp.realthinclient.com" and any of its sub-domains like
      "mymod.myapp.realthinclient.com"). @html(<br>)
      To process ONLY requests pointed exactly to ONE HOST, add "." in front of your
      host name (example: ".realthinclient.com" will ONLY react to requests with
      "realthinclient.com" in their HOST header). }
    property ModuleHost;
    { This property will be compared to Request.FileName to decide if the
      request we just received was pointed to this ServerModule. Any request asking
      for this FileName will be processed by this ServerModule component.
      Since parameters are passed to the server module through request's Content
      body (rather than headers), we do not need to check the request for anything
      else than it's FileName to know if the request is directed to this module. }
    property ModuleFileName;
    { Set this property to tell the RtcServerModule to use this TRtcFunctionGroup
      component to execute all functions received as a request from clients. }
    property FunctionGroup;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - NIL, or the connection component through which data was received)
      @param(Data - Data received from remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - NIL if using the default connection; "Sender" parameter for the Call method)
      @param(Data - Data prepared by our local "Object Manager" for sending to remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be sent over to remote "Object Manager") }
    property OnObjectDataOut;

    { This event is triggered when the remote Object Manager has requested
      our Object Manager to create a new Object (remote instance was already created)
      and allows you to create Objects which don't have a global constructor
      registered (through the global "RegisterRtcObjectConstructor" procedure).

      Objects which you do NOT want to have created automatically by the remote
      side, but where you still want to allow controlled creation should NOT have
      a global constructor registered and should be created from THIS event instead. }
    property OnObjectCreate;

    { Event to be triggered after a new Session was opened. }
    property OnSessionOpen;
    { Event to be triggered before an existing Session was to be closed. }
    property OnSessionClose;
    { Event to be triggered when the Server starts listening on the connection.
      You can use this event for component initialization. }
    property OnListenStart;
    { Event to be triggered when the Server stops listening on the connection.
      You can use this event for component deinitialization. }
    property OnListenStop;
    { Event to be triggered when a child DataProvider component accepts the Request.
      You can use this event for request initialization. For example,
      you could create a DataTunel and assign it to Tunel, to have reuqest data tunneled. }
    property OnRequestAccepted;
    { Event to be triggered after the response was sent out (Response.Done) }
    property OnResponseDone;
    { Event to be triggered when connection gets lost after a request was accepted.
      You can use this event for component deinitialization. }
    property OnDisconnect;
    end;

{ Post a function call to "Event" using "Param" parameters, delayed for "msDelay" miliseconds.
  If you need to make changes to parameters, do it BEFORE calling PostDelayedCall.
  This procedure ONLY works if  called from a function which was called by TRtcBaseServerModule. }
procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent); overload;

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" with the object returned from PrepareDelayedCall (TRtcDelayedCall) to post this call.
  You can also use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by calling "TRtcDelayedCall.WakeUp".
  You can NOT CANCEL THE CALL after you have Posted it with PostDelayedCall.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly.
  If you need to make any changes to the parameters passed to the delayed call,
  you have to do it BEFORE using PrepareDelayedCall, because PrepareDelayedCall creates a copy of all parameters
  and any change to the original Params will not be reflected in that copy later. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;

{ Post delayed call which was prepared using PrepareDelayedCall.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.WakeUp" method.
  You can NOT CANCEL THE CALL after you have Posted it by using PortDelayedCall, you can only Wake it up sooner.
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;

{ If you prepared a delayed call, but do not want to Post it anymore,
  use this procedure to cancel it before calling PostDelayedCall. @html(<br>)
  DO NOT CALL THIS PROCEDURE AFTER YOU HAVE POSTED THE CALL!!!!
  You can NOT CANCEL THE CALL after you have Posted it (with PostDelayedCall).
  If connection should get lost while a Delayed Call is waiting, delayed call will be canceled automaticaly. }
procedure CancelDelayedCall(cb:TRtcDelayedCall);

implementation

{ Prepare a function call to "Event" using "Param" paremeters, which should be delayed for "msDelay" miliseconds.
  Use "PostDelayedCall" to post this delayed call, after you've stored the Result returned by this function.
  You can use the Result (TRtcDelayedCall) to execute the delayed call sooner (from any thread) by using the "TRtcDelayedCall.Call" method. }
function PrepareDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent):TRtcDelayedCall;
  begin
  Result:=TRtcDelayedCall.Create;
  Result.msDelay:=msDelay;
  Result.Param:=TRtcFunctionInfo(Param.copyOf);
  Result.CallEvent:=Event;
  end;

{ Post delayed call which was prepared using PrepareDelayedCall. }
procedure PostDelayedCall(cb:TRtcDelayedCall); overload;
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=cb;
  raise e;
  end;

procedure CancelDelayedCall(cb:TRtcDelayedCall);
  begin
  cb.Cancel;
  end;

procedure PostDelayedCall(msDelay:integer; Param:TRtcFunctionInfo; Event:TRtcFunctionCallEvent);
  var
    e:EDelayedCall;
  begin
  e:=EDelayedCall.Create('');
  e.call:=PrepareDelayedCall(msDelay,Param,Event);
  raise e;
  end;

{ TRtcBaseServerModule }

constructor TRtcBaseServerModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);
  FDataFormats:=[fmt_RTC];
  FObjectLinkSupport:=ol_None;
  FAutoSessionsLock:=sesFwdLock;
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';
  FCryptSesName:='.$CRYPT$';
  FObjManSesName:=RTCL_SRVSESSION;
  end;

destructor TRtcBaseServerModule.Destroy;
  begin
  try
    FFunctions:=nil;
    FModuleFileName:='';
    FModuleHost:='';
    FCryptSesName:='';
    FObjManSesName:='';
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcBaseServerModule.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcBaseServerModule.Call_CheckRequest(Sender: TRtcConnection);
  begin
  with TRtcDataServer(Sender) do
    begin
    SetActiveLink(self);
    if Request.FileName=FModuleFileName then
      begin
      if FModuleHost<>'' then
        begin
        if copy(FModuleHost,1,1)<>'.' then // accepting domain with any sub-domain
          begin
          if (Upper_Case(Request.Host)=FModuleHost) then // host = domain name
            Accept
          else if ( (length(Request.Host)>length(FModuleHost)) and // could be sub-domain
                    (Copy(Request.Host,length(Request.Host)-length(FModuleHost),1)='.') and // has '.' in the right place
                    ( Upper_Case(Copy(Request.Host,length(Request.Host)-length(FModuleHost)+1,length(FModuleHost)))
                      = FModuleHost) ) then // is sub-domain
            Accept;
          end
        else if Upper_Case(Request.Host)=Copy(FModuleHost,2,length(FModuleHost)-1) then
          Accept; // accepting a specific sub-domain only
        end
      else // Accept the request. It has our ModuleFileName as Request.FileName, we accept all hosts
        Accept;
      end;
    end;
  end;

procedure TRtcBaseServerModule.Call_ListenStart(Sender: TRtcConnection);
  begin
  if assigned(FOnListenStart) then
    FOnListenStart(Sender);
  end;

procedure TRtcBaseServerModule.Call_ListenStop(Sender: TRtcConnection);
  begin
  if assigned(FOnListenStop) then
    FOnListenStop(Sender);
  end;

procedure TRtcBaseServerModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionClose) then
    FOnSessionClose(Sender);
  end;

procedure TRtcBaseServerModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    FOnSessionOpen(Sender);
  end;

procedure TRtcBaseServerModule.Call_RequestAccepted(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnRequestAccepted) then
      FOnRequestAccepted(Sender);

    if assigned(Link) then
      Link.Call_RequestAccepted(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcBaseServerModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnResponseDone) then
      FOnResponseDone(Sender);

    if assigned(Link) then
      Link.Call_ResponseDone(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

procedure TRtcBaseServerModule.Call_Disconnect(Sender: TRtcConnection);
  begin
  try
    if assigned(FOnDisconnect) then
      FOnDisconnect(Sender);

    if assigned(Link) then
      Link.Call_Disconnect(Sender);
  except
    on E:Exception do
      with TRtcValue.Create do
        try
          asException:=E.Message;
          if TRtcDataServer(Sender).Request.Info.asBoolean['$XML'] then
            Sender.Write(toXMLrpcResponse)
          else
            Sender.Write(toCode);
        finally
          Free;
        end;
    end;
  end;

function RandomKey(len:integer):RtcString;
  var
    a:integer;
  begin
  Result:='';
  for a:=1 to len do
    Result:=Result+RtcChar(random(256));
  end;

procedure CryptRead(Crypt:TRtcCryptServer; var Data:RtcString);
  begin
  if assigned(Crypt) and assigned(Crypt.CRead) then
    Crypt.CRead.DeCrypt(Data);
  end;

procedure CryptWrite(Crypt:TRtcCryptServer; var Data:RtcString);
  begin
  if assigned(Crypt) and assigned(Crypt.CWrite) then
    Crypt.CWrite.Crypt(Data);
  end;

procedure CryptReadEx(Crypt:TRtcCryptServer; var Data:RtcByteArray);
  begin
  if assigned(Crypt) and assigned(Crypt.CRead) then
    Crypt.CRead.DeCryptEx(Data);
  end;

procedure CryptWriteEx(Crypt:TRtcCryptServer; var Data:RtcByteArray);
  begin
  if assigned(Crypt) and assigned(Crypt.CWrite) then
    Crypt.CWrite.CryptEx(Data);
  end;

function TRtcBaseServerModule.GetObjectManager(Sender:TRtcDataServer): TRtcRemoteObjectManager;
  begin
  if FObjectLinkSupport=ol_None then
    Result:=nil
  else if not assigned(Sender.Session) then
    Result:=nil
  else if not assigned(Sender.Session._Obj[FObjManSesName]) then
    Result:=nil
  else if Sender.Session._Obj[FObjManSesName] is TRtcServerObjectManager then
    Result:=TRtcServerObjectManager(Sender.Session._Obj[FObjManSesName])
  else
    raise ERtcObjectLinks.Create('Session.Obj['+String(FObjManSesName)+'] is '+
                                  Sender.Session._Obj[FObjManSesName].ClassName+
                                  ', but should be TRtcServerObjectManager.');
  end;

procedure TRtcBaseServerModule.ActivateObjectManager(Sender:TRtcDataServer; xCreate:boolean=True);
  var
    om:TRtcServerObjectManager;
  begin
  om:=TRtcServerObjectManager(MakeObjectManager(Sender,xCreate));
  if assigned(om) then
    begin
    SetRtcObjectManager(om);
    om.ExecuteBroadcast(Sender);
    end
  else
    raise ERtcObjectLinks.Create('"ActivateObjectManager" failed: Object Manager unavailable.');
  end;

function TRtcBaseServerModule.MakeObjectManager(Sender:TRtcDataServer; xCreate:boolean=True):TRtcRemoteObjectManager;
  begin
  if FObjectLinkSupport=ol_None then
    raise ERtcObjectLinks.Create('ActiveObjectManager: ObjectLinks property = ol_None')
  else if not assigned(Sender.Session) then
    begin
    if xCreate then
      begin
      Sender.OpenSession(AutoSessionsLock);
      Sender.Session.KeepAlive:=AutoSessionsLive;
      Sender.Response.Cookie.ValueCS['ID']:=Sender.Session.ID;
      Result:=TRtcServerObjectManager.Create(True);
      Result.BroadcastGroup:=String(FObjManSesName);
      Sender.Session._Obj[FObjManSesName]:=Result;
      end
    else
      raise ERtcObjectLinks.Create('ActivateObjectManager: No active Session.');
    end
  else if not assigned(Sender.Session._Obj[FObjManSesName]) then
    begin
    if xCreate then
      begin
      Result:=TRtcServerObjectManager.Create(True);
      Result.BroadcastGroup:=String(FObjManSesName);
      Sender.Session._Obj[FObjManSesName]:=Result;
      end
    else
      raise ERtcObjectLinks.Create('ActivateObjectManager: Active Session does NOT have an Object Manager assigned.');
    end
  else if Sender.Session._Obj[FObjManSesName] is TRtcServerObjectManager then
    Result:=TRtcServerObjectManager(Sender.Session._Obj[FObjManSesName])
  else
    raise ERtcObjectLinks.Create('Session.Obj['+String(FObjManSesName)+'] is '+
                                  Sender.Session._Obj[FObjManSesName].ClassName+
                                  ', but should be TRtcServerObjectManager.');
  if not assigned(Result) then
    raise ERtcObjectLinks.Create('"ActivateObjectManager" failed: Object Manager unavailable.');
  end;

procedure TRtcBaseServerModule.DoExecute(Sender:TRtcDataServer; var MyData:TRtcValueObject);
  var
    MyResult,TmpData,TmpRes:TRtcValueObject;
    TmpMan:TRtcObjectManager;
    ObjMan:TRtcServerObjectManager;
    xData:TRtcValue;
  begin
  if isSimpleValue(MyData) then
    begin
    if FObjectLinkSupport in [ol_AutoServer, ol_AutoBoth] then
      begin
      if FAutoSessions or assigned(Sender.Session) then
        ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True))
      else
        ObjMan:=nil;
      end
    else if FObjectLinkSupport<>ol_None then
      ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager)
    else
      ObjMan:=nil;
    if assigned(ObjMan) then
      begin
      xData:=ObjMan.GetBroadcast;
      if assigned(xData) then
        try
          TmpMan:=SetRtcObjectManager(ObjMan);
          try
            ObjMan.ExecuteBroadcast(Sender,xData);
          finally
            SetRtcObjectManager(TmpMan);
            end;
        finally
          RtcFreeAndNil(xData);
          end;
      if not ObjMan.Idle then
        begin
        xData:=ObjMan.GetData;
        if assigned(FOnObjectDataOut) then
          FOnObjectDataOut(Sender,xData);
        TmpData:=TRtcFunctionInfo.Create;
        with TRtcFunctionInfo(TmpData) do
          begin
          FunctionName:=RTCL_FUNCTION;
          asObject[RTCL_RESULT]:=MyData;
          asObject[RTCL_DATA]:=xData;
          end;
        MyData:=TmpData;
        end;
      end;
    end
  else if FObjectLinkSupport=ol_None then
    begin
    if assigned(FFunctions) then
      begin
      MyResult:=FFunctions.ExecuteData(Sender, MyData);
      if MyData<>MyResult then
        begin
        RtcFreeAndNil(MyData);
        MyData:=MyResult;
        end;
      end
    else
      raise Exception.Create('FunctionGroup property undefined, can not execute call.');
    end
  else if (TRtcValue(MyData).isType=rtc_Function) and
          (CompareText(TRtcValue(MyData).asFunction.FunctionName, RTCL_FUNCTION)=0) then
    begin
    TmpData:=TRtcValue(MyData).asFunction.asObject[RTCL_DATA];
    ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager);
    if not assigned(TmpData) then
      begin
      RtcFreeAndNil(MyData);
      MyData:=TRtcExceptionValue.Create('No data received.');
      end
    else
      begin
      if not assigned(ObjMan) then
        begin
        if FObjectLinkSupport in [ol_AutoClient, ol_AutoBoth] then
          begin
          if not assigned(Sender.Session) and (AutoSessions=False) then
            begin
            RtcFreeAndNil(MyData);
            MyData:=TRtcExceptionValue.Create('Session not found, can not create Object Manager.');
            Exit;
            end
          else
            ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True));
          end
        else
          begin
          RtcFreeAndNil(MyData);
          MyData:=TRtcExceptionValue.Create('Object Manager not found.');
          Exit;
          end;
        end;
      TmpMan:=SetRtcObjectManager(ObjMan);
      try
        if assigned(FOnObjectDataIn) then
          begin
          xData:=TRtcValue.Create;
          try
            xData.asObject:=TmpData;
            TRtcValue(MyData).asFunction.asObject[RTCL_DATA]:=nil;
            FOnObjectDataIn(Sender,xData);
            if assigned(FOnObjectCreate) then
              begin
              ObjMan.OnObjectCreate:=DoObjectCreate;
              try
                TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,xData);
              finally
                ObjMan.OnObjectCreate:=nil;
                end;
              end
            else
              TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,xData);
          finally
            xData.Free;
            end;
          end
        else
          begin
          if assigned(FOnObjectCreate) then
            begin
            ObjMan.OnObjectCreate:=DoObjectCreate;
            try
              TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,TmpData);
            finally
              ObjMan.OnObjectCreate:=nil;
              end;
            end
          else
            TmpRes:=ObjMan.ExecuteWithBroadcast(Sender,TmpData);
          end;
        xData:=ObjMan.GetBroadcast;
        if assigned(xData) then
          try
            TmpMan:=SetRtcObjectManager(ObjMan);
            try
              ObjMan.ExecuteBroadcast(Sender,xData);
            finally
              SetRtcObjectManager(TmpMan);
              end;
          finally
            RtcFreeAndNil(xData);
            end;
        if not ObjMan.Idle then
          begin
          xData:=ObjMan.GetData;
          if assigned(FOnObjectDataOut) then
            FOnObjectDataOut(Sender,xData);
          TmpData:=TRtcFunctionInfo.Create;
          with TRtcFunctionInfo(TmpData) do
            begin
            FunctionName:=RTCL_FUNCTION;
            asObject[RTCL_RESULT]:=TmpRes;
            asObject[RTCL_DATA]:=xData;
            end;
          TmpRes:=TmpData;
          end;
        RtcFreeAndNil(MyData);
        if assigned(TmpRes) then
          MyData:=TmpRes
        else
          MyData:=TRtcValue.Create;
      finally
        SetRtcObjectManager(TmpMan);
        end;
      end;
    end
  else if assigned(FFunctions) then
    begin
    if (FObjectLinkSupport in [ol_AutoServer, ol_AutoBoth]) then
      begin
      if FAutoSessions or assigned(Sender.Session) then
        begin
        ObjMan:=TRtcServerObjectManager(MakeObjectManager(Sender,True));
        TmpMan:=SetRtcObjectManager(ObjMan);
        try
          if assigned(ObjMan) then
            ObjMan.ExecuteBroadcast(Sender);
          MyResult:=FFunctions.ExecuteData(Sender,MyData);
        finally
          SetRtcObjectManager(TmpMan);
          end;
        end
      else
        MyResult:=TRtcExceptionValue.Create('ObjectLinks=ol_AutoServer; No active Session and AutoSessions=False.');
      end
    else
      begin
      TmpMan:=CheckRtcObjectManager;
      try
        MyResult:=FFunctions.ExecuteData(Sender, MyData);
      finally
        SetRtcObjectManager(TmpMan); // return the original object manager
        end;
      end;
    if MyData<>MyResult then
      begin
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;
    ObjMan:=TRtcServerObjectManager(Sender.GetObjectManager);
    if assigned(ObjMan) then
      begin
      xData:=ObjMan.GetBroadcast;
      if assigned(xData) then
        try
          TmpMan:=SetRtcObjectManager(ObjMan);
          try
            ObjMan.ExecuteBroadcast(Sender,xData);
          finally
            SetRtcObjectManager(TmpMan);
            end;
        finally
          RtcFreeAndNil(xData);
          end;
      if not ObjMan.Idle then
        begin
        xData:=ObjMan.GetData;
        if assigned(FOnObjectDataOut) then
          FOnObjectDataOut(Sender,xData);
        TmpData:=TRtcFunctionInfo.Create;
        with TRtcFunctionInfo(TmpData) do
          begin
          FunctionName:=RTCL_FUNCTION;
          asObject[RTCL_RESULT]:=MyData;
          asObject[RTCL_DATA]:=xData;
          end;
        MyData:=TmpData;
        end;
      end;
    end
  else
    raise Exception.Create('FunctionGroup property undefined, can not execute call.');
  end;

procedure TRtcBaseServerModule.Call_DataReceived(Sender: TRtcConnection);
  var
    crypt:TRtcCryptServer;
    tmpcnt:integer;
    code,tmp:RtcByteArray;
    data:RtcString;
    temp:RtcString;
    output:TRtcHugeString;
    at:integer;
    MyData:TRtcValueObject;
    mycall:TRtcDelayedCall;
    FMT:TRtcDataFormat;

  function IsValidControlKey(var Counter:integer; const s:RtcString):boolean;
  {$IFDEF RtcDoNotCheckCryptControlSums}
    begin
    // Old implementation uses 3 - 8 byte control keys,
    // New implementation uses 9 - 14 byte control keys.
    Result:= length(s) in [3..14];
    end;
  {$ELSE}
    var
      i,a,b,Chk:integer;
    begin
    Inc(Counter);
    if Counter>99 then Counter:=1;

    // New implementation sends a counter and a checksum,
    // which can be used to validate the control key.
    if length(s) in [9..14] then
      begin
      b:=(14-length(s))*9+8;
      for i:=5 to length(s) do
        Inc(b, Ord(s[i])-Ord('0'));
      a:=(Ord(s[1])-Ord('0'))*10 +
         (Ord(s[2])-Ord('0'));
      Chk:=(Ord(s[3])-Ord('0'))*10 +
           (Ord(s[4])-Ord('0'));
      Result:= (a=b) and (Chk=Counter);
      end
    else
      Result:=False;
    end;
  {$ENDIF}

  function ExtractControlKey(var Counter:integer):RtcString;
    var
      i,at,len:integer;
    begin
    Result:='';
    at:=0;len:=0;
    { Data will be closed with #13+ControlCode }
    for i:=length(code)-1 downto 0 do
      case code[i] of
        Byte('0')..Byte('9'):
          begin
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=RtcChar(code[i])+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
          end;
        13:begin
          at:=i+1;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        SetLength(code,at-1);
      end
    else
      Result:='';
    end;

  function ExtractFinalControlKey(var Counter:integer):RtcString;
    var
      DecompressTo,
      i,at,len:integer;
    begin
    Result:='';len:=0;
    at:=0;
    DecompressTo:=0;
    { Data will be closed with:
      > #13+ControlCode if not compressed
      > #0+ControlCode if compressed. }
    for i:=length(code)-1 downto 0 do
      case code[i] of
        Byte('0')..Byte('9'):
          if len<14 then // we will never use control keys longer than 14 digits
            begin
            Result:=RtcChar(code[i])+Result;
            Inc(len);
            end
          else
            begin
            // Return empty result, which will tell the client to reinitialize encryption
            Result:='';
            Exit;
            end;
        13:
          begin
          at:=i+1;
          // data is NOT compressed
          Break;
          end;
        0:
          begin
          at:=i+1;
          // data is compressed
          DecompressTo:=at-1;
          Break;
          end;
        else
          begin
          Result:='';
          Exit;
          end;
        end;

    if at>0 then // number found
      begin
      if not IsValidControlKey(Counter, Result) then
        Result:=''
      else
        begin
        if DecompressTo>0 then
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Ex(code,DecompressTo);
          except
            on E:Exception do
              Result:='';
            end;
          {$ELSE}
          Result:='';
          // raise Exception.Create('Compressed data received, but compression not supported.');
          {$ENDIF}
          end
        else
          SetLength(code,at-1);
        end;
      end
    else
      Result:='';
    end;

  begin
  with TRtcDataServer(Sender) do
    begin
    if Request.Started then
      if (Request.Query.ValueCS['ID']<>'') and
         (Request.Query.ValueCS['ID']<>'NEW') then
        begin
        // Need to handle now, because there could be a lot of data coming,
        // during which our Session could time out.
        if not FindSession(Request.Query.ValueCS['ID']) then
          if HaveSession(Request.Query.ValueCS['ID']) then
            Response.Status(410,'Session locked')
          else
            Response.Status(410,'Session not found');
        end;

    if not Request.Complete then Exit
    else if Response.StatusCode=410 then
      begin
	  // Invalid Session ID, do NOT process request
      code:=ReadEx;
      Write;
      Exit;
      end;

    code:=ReadEx;

    if Request.Query.ValueCS['ACTION']='HELLO' then
      begin
      if EncryptionKey=0 then // Encryption not supported
        begin
        // Find Client Session
        if assigned(Session) then
          DelCrypt(Session)
        else if AutoSessions and (Request.Query.ValueCS['ID']='NEW') then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.ValueCS['ID']:=Session.ID;
          end
        else if Request.Query.ValueCS['ID']<>'' then
          begin
          if FindSession(Request.Query.ValueCS['ID']) then
            DelCrypt(Session)
          else if HaveSession(Request.Query.ValueCS['ID']) then
            Response.Status(410,'Session locked')
          else
            Response.Status(410,'Session not found');
          end;
        Write; // send response with empty content
        end
      else if length(code)=0 then
        Write
      else
        begin
        // Find Client Session
        if assigned(Session) then
          DelCrypt(Session)
        else if (Request.Query.ValueCS['ID']<>'') and
                (Request.Query.ValueCS['ID']<>'NEW') then
          begin
          if FindSession(Request.Query.ValueCS['ID']) then
            DelCrypt(Session)
          else
            begin
            if HaveSession(Request.Query.ValueCS['ID']) then
              Response.Status(410,'Session locked')
            else
              Response.Status(410,'Session not found');
            Write;
            Exit;
            end;
          end
        else if AutoSessions then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.ValueCS['ID']:=Session.ID;
          end
        else
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end;

        if SecureKey<>'' then
          begin
          with TRtcEncryptionClass.Create do
            begin
            Key:=SecureKey;
            DeCryptEx(code);
            Free;
            end;
          end;

        tmpcnt:=0;
        temp:=ExtractControlKey(tmpcnt);
        if (length(code)=0) or (temp='') then
          begin
          Response.Status(417,'Encryption Key error.');
          Write;
          Exit;
          end;

        NewCrypt(Session);
        crypt:=GetCrypt(Session);
        crypt.ControlCounter:=tmpcnt;

        crypt.ControlKey:=temp;
        crypt.ClientHello:= RtcBytesToString(code);

        crypt.ServerHello:=RandomKey(EncryptionKey);
        crypt.HaveHello:=True;

        // Encryption object for Write operation
        crypt.CWrite:=TRtcEncryptionClass.Create;
        crypt.CWrite.Key:=crypt.ClientHello;

        // Encryption object for Read operation
        crypt.CRead:=TRtcEncryptionClass.Create;
        crypt.CRead.Key:=crypt.ServerHello;

        // Prepare, encode and send the response
        code:= RtcStringToBytes(crypt.ServerHello+crypt.ControlKey);
        CryptWriteEx(crypt, code);
        WriteEx(code);
        end;
      end
    else if Request.Query.ValueCS['ACTION']='START' then
      begin
      if not assigned(Session) then
        if Request.Query.ValueCS['ID']='' then
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end
        else if not FindSession(Request.Query.ValueCS['ID']) then
          begin
          if HaveSession(Request.Query.ValueCS['ID']) then
            Response.Status(410,'Session locked')
          else
            Response.Status(410,'Session not found');
          Write;
          Exit;
          end;

      if EncryptionKey=0 then // Encryption not supported
        Write // send response with empty content
      else if length(code)=0 then
        Write
      else
        begin
        crypt:=GetCrypt(Session);

        if crypt=nil then
          begin
          Response.Status(410,'Session not initialized.');
          Write;
          Exit;
          end
        else if not crypt.HaveHello then
          begin
          Response.Status(410,'Session handshake error.');
          Write;
          Exit;
          end;

        // Decode client request
        CryptReadEx(crypt, code);

        crypt.ControlKey:=ExtractControlKey(crypt.ControlCounter);
        if (crypt.ControlKey='') or (length(code)=0) then
          begin
          Response.Status(417,'Encryption Key error.');
          Write;
          Exit;
          end;

        crypt.ClientKey:= RtcBytesToString(code);
        crypt.ServerKey:=RandomKey(EncryptionKey);
        crypt.HaveStart:=True;

        // Set Encryption kexs for Reading and Writing
        crypt.CWrite.Key:= crypt.ClientKey+crypt.ServerHello;
        crypt.CRead.Key := crypt.ServerKey+crypt.ClientHello;

        // Prepare, encode and send the response
        code:= RtcStringToBytes(crypt.ServerKey+crypt.ControlKey);
        CryptWriteEx(crypt, code);
        WriteEx(code);
        end;
      end
    else
      begin

      if Request.Query.ValueCS['ID']='NEW' then
        begin
        if ForceEncryption and (EncryptionKey>0) then
          begin
          // 412 = Precondition Failed (Encryption required)
          Response.Status(412,'Encryption required.');
          Write;
          Exit;
          end
        else if (Request.Query.ValueCS['ACTION']='RESET') then
          begin
          // 417 = Expectation Failed (Encryption error)
          Response.Status(417,'Encryption error.');
          Write;
          Exit;
          end
        else if AutoSessions then
          begin
          OpenSession(AutoSessionsLock);
          Session.KeepAlive:=AutoSessionsLive;
          Response.Cookie.ValueCS['ID']:=Session.ID;
          end;
        end
      else if Request.Query.ValueCS['ID']<>'' then
        begin
        if Response.StatusCode=410 then
          begin
          // Return empty String if Session expired or non-existing
          Write;
          Exit;
          end
        else
          begin
          if not assigned(Session) then
            if not FindSession(Request.Query.ValueCS['ID']) then
              begin
              if HaveSession(Request.Query.ValueCS['ID']) then
                Response.Status(410,'Session locked')
              else
                Response.Status(410,'Session not found');
              Write;
              Exit;
              end;

          if Request.Query.ValueCS['ACTION']='RESET' then
            begin
            crypt:=GetCrypt(Session);
            if assigned(crypt) then
              begin
              if not (crypt.HaveHello and crypt.HaveStart) then
                begin
                // Encryption not fully initialized
                Response.Status(417,'Encryption error.');
                Write;
                Exit;
                end
              else
                begin
                // Encryption object for Write operation
                RtcFreeAndNil(crypt.CWrite);
                crypt.CWrite:=TRtcEncryptionClass.Create;
                crypt.CWrite.Key:=crypt.ClientKey+crypt.ServerHello;

                // Encryption object for Read operation
                RtcFreeAndNil(crypt.CRead);
                crypt.CRead:=TRtcEncryptionClass.Create;
                crypt.CRead.Key:=crypt.ServerKey+crypt.ClientHello;

                crypt.ControlCounter:=0;
                end;
              end;
            end;
          end;
        end
      else if Request.Query.ValueCS['ACTION']='RESET' then
        begin
        // No Session ID? No encryption.
        // 417 = Expectation Failed (Encryption error)
        Response.Status(417,'Encryption error.');
        Write;
        Exit;
        end;

      if Upper_Case(Request.ContentType)='TEXT/XML' then
        begin
        if (fmt_XMLRPC in FDataFormats) and isXMLString(code) then // data received as XML-RPC
          begin
          FMT:=fmt_XMLRPC;
          Request.Info.asBoolean['$XML']:=True;
          crypt:=nil;
          end
        else
          begin
          Response.Status(404,'Data Format not supported.');
          Write;
          Exit;
          end;
        end
      else if (fmt_RTC in FDataFormats) then
        begin
        FMT:=fmt_RTC;

        crypt:=GetCrypt(Session);
        if assigned(crypt) and assigned(crypt.CRead) then
          begin
          CryptReadEx(crypt, code);

          try
            crypt.ControlKey:=ExtractFinalControlKey(crypt.ControlCounter);
          except
            on E:Exception do
              begin
              Response.Status(409,'Encryption Error.');
              Write;
              Exit;
              end;
            end;

          if crypt.ControlKey='' then
            begin
            Response.Status(409,'Encryption Key Error.');
            Write;
            Exit;
            end;
          end
        else if ForceEncryption and (EncryptionKey>0) then
          begin
          // 412 = Precondition Failed (Encryption required)
          Response.Status(412,'Encryption required.');
          Write;
          Exit;
          end
        else if (length(Code)>0) and (Code[length(code)-1]=0) then // compressed
          begin
          {$IFDEF COMPRESS}
          try
            code:=ZDecompress_Ex(code,length(code)-1);
          except
            on E:Exception do
              begin
              Response.Status(409,'Decompression error.');
              Write;
              Exit;
              end;
            end;
          {$ELSE}
          Response.Status(409,'Compression not supported.');
          Write;
          Exit;
          {$ENDIF}
          end;
        end
      else
        begin
        // 404 = Request not supported.
        Response.Status(404,'Data Format not supported.');
        Write;
        Exit;
        end;

      output:=nil;

      at:=0;
      try
        if length(code)=0 then
          Write
        else
          begin
          data:=RtcBytesToString(code);
          SetLength(code,0);

          while at<length(data) do
            begin
            case FMT of
              fmt_XMLRPC: MyData:=TRtcValue.FromXMLrpc(data,at);
              else MyData:=TRtcValue.FromCode(data,at);
              end;
            {$IFDEF COMPRESS}
            if (FMT=fmt_RTC) and (FCompress<>cNone) and not assigned(output) and
               (at<length(code)) then
              output:=TRtcHugeString.Create;
            {$ENDIF}

            try
              DoExecute(TRtcDataServer(Sender),MyData);

              if assigned(MyData) then
                begin
                if assigned(output) then
                  MyData.to_Code(output)
                else
                  begin
                  {$IFDEF COMPRESS}
                  if (FMT=fmt_RTC) and (FCompress<>cNone) then
                    begin
                    { We want to send data compressed, but we haven't created the
                      "output" object until now, which means that this is the last result,
                      so we don't need "code" anymore. }
                    code:= myData.toCodeEx;
                    if length(code)<RTC_MIN_COMPRESS_SIZE then
                      begin
                      CryptWriteEx(crypt,code);
                      WriteEx(code);
                      SetLength(code,0);
                      end
                    else
                      begin
                      SetLength(tmp,0);
                      case FCompress of
                        cFast: tmp := ZCompress_Ex(code, zcFastest);
                        cMax: tmp := ZCompress_Ex(code, zcMax);
                        else tmp := ZCompress_Ex(code, zcDefault);
                        end;

                      if length(tmp)>=length(code)-1 then
                        begin
                        SetLength(tmp,0);
                        CryptWriteEx(crypt,code);
                        WriteEx(code);
                        SetLength(code,0);
                        end
                      else
                        begin
                        SetLength(code,0);
                        CryptWriteEx(crypt,tmp);
                        WriteEx(tmp);

                        SetLength(tmp,1);
                        tmp[0]:=0;
                        CryptWriteEx(crypt,tmp);
                        WriteEx(tmp);
                        SetLength(tmp,0);
                        end;
                      end;
                    end
                  else
                  {$ENDIF}
                    begin
                    if FMT=fmt_XMLRPC then
                      Write(MyData.toXMLrpcResponse)
                    else
                      begin
                      if assigned(crypt) and assigned(crypt.CWrite) then
                        begin
                        tmp:=MyData.toCodeEx;
                        CryptWriteEx(crypt,tmp);
                        WriteEx(tmp);
                        end
                      else
                        Write(MyData.toCode);
                      end;
                    end;
                  end;
                end
              else
                Write; // no content.
            finally
              RtcFreeAndNil(MyData);
              end;
            end;
          end;
        if assigned(Session) then // Session active
          begin
          if Session.isClosing then // Session closing
            Response.Cookie.ValueCS['ID']:='-'+Request.Query.ValueCS['ID']
          else if Request.Query.ValueCS['ID']<>Session.ID then // Last Session ID received from Client different from current Session ID
            if Response.Cookie.ValueCS['ID']<>Session.ID then // Session ID to be sent to the Client different from current Session ID
              Response.Cookie.ValueCS['ID']:=Session.ID;
          end;
      except
        on E:EDelayedCall do
          begin
          // Create and Post the DelayedCall
          mycall:=e.call;
          mycall.Conn:=Sender;
          mycall.output:=output;
          mycall.FMT:=FMT;
          mycall.Compress:=Compression;
          mycall.ObjectLinkSupport:=FObjectLinkSupport;
          mycall.ObjectDataOutEvent:=FOnObjectDataOut;
          if assigned(Session) then
            begin
            mycall.SessionID:=Session.ID;
            mycall.crypt:=crypt;
            end;

          Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;
          if Sender.isExtension then
            mycall.Execute
          else
            mycall.Post(MultiThreaded);
          Exit; // not sending the reponse now!
          end;
        on E:Exception do
          with TRtcValue.Create do
            try
              asException:=E.Message;
              if assigned(output) then
                to_Code(output)
              else
                begin
                if FMT=fmt_XMLRPC then
                  Write(toXMLrpcResponse)
                else
                  begin
                  if assigned(crypt) and assigned(crypt.CWrite) then
                    begin
                    tmp:=toCodeEx;
                    CryptWriteEx(crypt,tmp);
                    WriteEx(tmp);
                    end
                  else
                    Write(asCode);
                  end;
                end;
            finally
              Free;
            end;
        end;

      {$IFDEF COMPRESS}
      if assigned(output) then
        begin
        { we have stored uncompressed data in "output",
          now we need to compress it all and send it out. }
        try
          code:=output.GetEx;
        finally
          RtcFreeAndNil(output);
          end;

        if length(code)<RTC_MIN_COMPRESS_SIZE then
          begin
          CryptWriteEx(crypt,code);
          WriteEx(code);
          SetLength(code,0);
          end
        else
          begin
          SetLength(tmp,0);
          case FCompress of
            cFast: tmp := ZCompress_Ex(code, zcFastest);
            cMax: tmp := ZCompress_Ex(code, zcMax);
            else tmp := ZCompress_Ex(code, zcDefault);
            end;

          if length(tmp)>=length(code)-1 then
            begin
            SetLength(tmp,0);
            CryptWriteEx(crypt,code);
            WriteEx(code);
            SetLength(code,0);
            end
          else
            begin
            SetLength(code,0);
            CryptWriteEx(crypt,tmp);
            WriteEx(tmp);

            SetLength(tmp,1);
            tmp[0]:=0;
            CryptWriteEx(crypt,tmp);
            WriteEx(tmp);
            SetLength(tmp,0);
            end;
          end;
        end;
      {$ENDIF}
      
      if assigned(crypt) and assigned(crypt.CWrite) then
        begin
        // Add control key to the end of our response
        code:=RtcStringToBytes(crypt.ControlKey);
        CryptWriteEx(crypt, code);
        WriteEx(code);
        end;
      end;
    end;
  end;

procedure TRtcBaseServerModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcBaseServerModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

function TRtcBaseServerModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcBaseServerModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcBaseServerModule.GetModuleFileName: RtcString;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcBaseServerModule.SetModuleFileName(const Value: RtcString);
  begin
  if FModuleFileName<>Value then
    begin
    FModuleFileName:=Value;
    if FModuleFileName<>'' then
      begin
      // FileName has to start with '/'
      if Copy(FModuleFileName,1,1)<>'/' then
        FModuleFileName:='/'+FModuleFileName;
      end;
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_SRVSESSION;
    end;
  end;

function TRtcBaseServerModule.GetModuleHost: RtcString;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcBaseServerModule.SetModuleHost(const Value: RtcString);
  begin
  if FModuleHost<>Value then
    // Convert to uppercase now, so we don't have to do it on every request.
    begin
    FModuleHost:=Upper_Case(Value);
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_SRVSESSION;
    end;
  end;

function TRtcBaseServerModule.GetCrypt(Session: TRtcSession): TRtcCryptServer;
  var
    obj:TObject;
  begin
  if not assigned(Session) then
    Result:=nil
  else
    begin
    obj:=Session._Obj[FCryptSesName];
    if assigned(obj) then
      Result:=TRtcCryptServer(obj)
    else
      Result:=nil;
    end;
  end;

procedure TRtcBaseServerModule.NewCrypt(Session: TRtcSession);
  var
    obj:TObject;
  begin
  obj:=Session._Obj[FCryptSesName];
  if obj<>nil then
    TRtcCryptServer(obj).Init
  else
    Session._Obj[FCryptSesName]:=TRtcCryptServer.Create;
  end;

procedure TRtcBaseServerModule.DelCrypt(Session: TRtcSession);
  var
    obj:TObject;
  begin
  if assigned(Session) then
    begin
    obj:=Session._Obj[FCryptSesName];
    if obj<>nil then
      begin
      Session._Obj[FCryptSesName]:=nil;
      obj.Free;
      end;
    end;
  end;

procedure TRtcBaseServerModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt>0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcBaseServerModule.SetAutoSessions(const Value: boolean);
  begin
  if not Value and (FAutoEncrypt>0) then
    raise Exception.Create('Set EncryptionKey to 0 before setting AutoSessions to False.');
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcBaseServerModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

procedure TRtcBaseServerModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;

procedure TRtcBaseServerModule.SetAutoSessionsLock(const Value: TRtcSessionLockType);
  begin
  FAutoSessionsLock := Value;
  end;

procedure TRtcBaseServerModule.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FFunctions then
      SetFunctionGroup(nil);
  end;

procedure TRtcBaseServerModule.SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
  begin
  FObjectLinkSupport := Value;
  end;

procedure TRtcBaseServerModule.DoObjectCreate(Sender:TObject; Param: TRtcObjectCall);
  begin
  if assigned(FOnObjectCreate) then
    FOnObjectCreate(TRtcConnection(Sender),Param);
  end;

{ TRtcCryptServer }

constructor TRtcCryptServer.Create;
  begin
  inherited;
  HaveHello:=False;
  HaveStart:=False;
  ControlKey:='';
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  ControlCounter:=0;
  CRead:=nil;
  CWrite:=nil;
  end;

destructor TRtcCryptServer.Destroy;
  begin
  try
    Init;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCryptServer.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCryptServer.Init;
  begin
  HaveHello:=False;
  HaveStart:=False;
  ControlKey:='';
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  ControlCounter:=0;
  RtcFreeAndNil(CRead);
  RtcFreeAndNil(CWrite);
  end;

procedure TRtcCryptServer.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

{ TRtcDelayedCall }

type
  TRtcDelayedCallJob=class(TRtcJob)
  public
    Conn:TRtcConnection;
    constructor Create(Con: TRtcConnection);

    function Run(Thr:TRtcThread):boolean; override;
    procedure Kill; override;
    end;

type
  TZeroObj=class(TObject)
  constructor Create;
  end;

{ TZeroObj }

constructor TZeroObj.Create;
  begin
  end;

var
  List:tObjList;
  CS:TRtcCritSec;
  zeroObj:TObject;

procedure AddObj(o:TObject);
  begin
  CS.Acquire;
  try
    List.insert(RtcIntPtr(o),o);
  finally
    CS.Release;
    end;
  end;

procedure DelObj(o:TObject);
  begin
  CS.Acquire;
  try
    List.remove(RtcIntPtr(o));
  finally
    CS.Release;
    end;
  end;

procedure SetObj(o:TObject; num:TObject);
  begin
  CS.Acquire;
  try
    if List.search(RtcIntPtr(o))<>nil then
      List.change(RtcIntPtr(o),num);
  finally
    CS.Release;
    end;
  end;

procedure KillObjs;
  var
    ob:TObject;
    i:RtcIntPtr;
  begin
  CS.Acquire;
  try
    if assigned(List) then
      begin
      i:=List.search_min(ob);
      while assigned(ob) do
        begin
        List.remove(i);
        if (ob<>nil) and (ob<>zeroObj) then
          RtcFreeAndNil(ob);
        i:=List.search_g(i, ob);
        end;
      RtcFreeAndNil(List);
      end;
  finally
    CS.Release;
    end;
  end;

function CheckObj(o:TObject):boolean;
  var
    o2:TObject;
  begin
  CS.Acquire;
  try
    o2:=List.search(RtcIntPtr(o));
    Result:=(o2<>nil) and (o2<>zeroObj);
  finally
    CS.Release;
    end;
  end;

function HaveObj(o:TObject):boolean;
  begin
  CS.Acquire;
  try
    Result:=List.search(RtcIntPtr(o))<>nil;
  finally
    CS.Release;
    end;
  end;

constructor TRtcDelayedCall.Create;
  begin
  inherited Create;
  WaitEvent:=nil;
  Timer:=nil;
  Called:=False;
  output:=nil;

  AddObj(Self);
  end;

destructor TRtcDelayedCall.Destroy;
  begin
  try
    CS.Acquire;
    try
      if not HaveObj(self) then Exit;
      DelObj(self);
      if assigned(WaitEvent) then
        WaitEvent.SetEvent;
      if assigned(Timer) then
        TRtcTimer.Stop(Timer);
    finally
      CS.Release;
      end;

    RtcFreeAndNil(output);

    RtcFreeAndNil(Param);
    SessionID:='';
    Conn:=nil;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcDelayedCall.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcDelayedCall.Kill;
  begin
  if not HaveObj(self) then Exit;
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

procedure TRtcDelayedCall.Cancel;
  begin
  if not HaveObj(self) then Exit;
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

procedure TRtcDelayedCall.Post(Multi_Threaded:boolean);
  var
    wait:boolean;
  begin
  CS.Acquire;
  try
    if not HaveObj(self) then Exit;

    wait:=not Called;
    if wait then
      begin
      Timer:=TRtcTimer.Create(Multi_Threaded);
      TRtcTimer.Enable(Timer,msDelay,Execute,True);
      end;
  finally
    CS.Release;
    end;
  if not wait then
    Execute;
  end;

procedure TRtcDelayedCall.Execute;
  var
    ob:TRtcDelayedCallJob;
    wait:boolean;
  begin
  wait:=False;

  CS.Acquire;
  try
    if not HaveObj(self) then Exit;

    // Stop Timer if timer active
    if assigned(Timer) then
      begin
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    // Start Waiting Event if no timer
    else
      begin
      wait:=not Called;
      if wait then
        WaitEvent:=TRtcEvent.Create(True,False);
      end;
  finally
    CS.Release;
    end;

  if wait then
    begin
    WaitEvent.WaitFor(msDelay);
    CS.Acquire;
    try
      if not HaveObj(self) then Exit;

      RtcFreeAndNil(WaitEvent);
    finally
      CS.Release;
      end;
    end;

  // We don't want to have a situation where Execute would be called twice (for example, when Timer triggers and another thread uses "Call")
  CS.Acquire;
  try
    if not CheckObj(self) then Exit;

    SetObj(self,zeroObj);
  finally
    CS.Release;
    end;

  if assigned(Conn) then
    begin
    ob:=TRtcDelayedCallJob.Create(Conn);
    if not Conn.PostJob(ob) then
      ob.Kill;
    end;
  end;

procedure TRtcDelayedCall.WakeUp;
  var
    needtocall:boolean;
  begin
  needtocall:=False;

  CS.Acquire;
  try
    if not CheckObj(self) then Exit;
    Called:=True;

    if assigned(Timer) then
      begin
      // Timer active. Need to stop the timer and call Execute
      needtocall:=True;
      TRtcTimer.Stop(Timer);
      Timer:=nil;
      end
    else if assigned(WaitEvent) then
      // Wait Event active. Only need to Set the Event flag
      WaitEvent.SetEvent;
  finally
    CS.Release;
    end;

  if needtocall then
    Execute;
  end;

{ TRtDelayedCallJob }

constructor TRtcDelayedCallJob.Create(Con: TRtcConnection);
  begin
  inherited Create;
  Conn:=Con;
  end;

procedure TRtcDelayedCallJob.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

function TRtcDelayedCallJob.Run(Thr:TRtcThread):boolean;
  var
    call,mycall:TRtcDelayedCall;
    MyResult:TRtcValue;
    {$IFDEF COMPRESS}
    code:RtcByteArray;
    {$ENDIF}
    temp:RtcByteArray;
    released:boolean;

    TmpMan:TRtcObjectManager;
    ObjMan:TRtcServerObjectManager;
    xData,TmpData:TRtcValue;
  begin
  Result:=False;
  released:=False;
  call:=nil;
  try
    if assigned(Conn) then with TRtcDataServer(Conn) do
      begin
      call:=TRtcDelayedCall(Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]);
      if not assigned(call) then Exit;
      Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=nil;

      if (call.SessionID<>'') then
        if not assigned(Session) or (Session.ID<>call.SessionID) then
          begin
          Disconnect;
          Exit;
          end;

      EnterEvent;
      try
        try
          MyResult:=TRtcValue.Create;
          try
            call.CallEvent(call.Conn, call.Param, myResult);

            if (call.ObjectLinkSupport<>ol_None) and assigned(call.Conn) then
              begin
              ObjMan:=TRtcServerObjectManager(TRtcDataServer(call.Conn).GetObjectManager);
              if assigned(ObjMan) then
                begin
                xData:=ObjMan.GetBroadcast;
                if assigned(xData) then
                  try
                    TmpMan:=SetRtcObjectManager(ObjMan);
                    try
                      ObjMan.ExecuteBroadcast(call.Conn,xData);
                    finally
                      SetRtcObjectManager(TmpMan);
                      end;
                  finally
                    RtcFreeAndNil(xData);
                    end;
                if not ObjMan.Idle then
                  begin
                  xData:=ObjMan.GetData;
                  if assigned(call.ObjectDataOutEvent) then
                    call.ObjectDataOutEvent(call.Conn,xData);
                  TmpData:=TRtcValue.Create;
                  with TmpData.newFunction(RTCL_FUNCTION) do
                    begin
                    asObject[RTCL_RESULT]:=MyResult;
                    asObject[RTCL_DATA]:=xData;
                    end;
                  MyResult:=TmpData;
                  end;
                end;
              end;

            if assigned(call.output) then
              myResult.to_Code(call.output)
            else
              begin
              {$IFDEF COMPRESS}
              if (call.FMT=fmt_RTC) and (call.Compress<>cNone) then
                begin
                code:=myResult.toCodeEx;
                if length(code)<RTC_MIN_COMPRESS_SIZE then
                  begin
                  CryptWriteEx(call.crypt, code);
                  WriteEx(code);
                  end
                else
                  begin
                  { we have stored uncompressed data in "output",
                    now we need to compress it all and send it out. }
                  SetLength(temp,0);
                  case call.Compress of
                    cFast: temp:=ZCompress_Ex(code,zcFastest);
                    cMax: temp:=ZCompress_Ex(code,zcMax);
                    else temp:=ZCompress_Ex(code,zcDefault);
                    end;

                  if length(temp)>=length(code)-1 then
                    begin
                    SetLength(temp,0);
                    CryptWriteEx(call.crypt,code);
                    WriteEx(code);
                    SetLength(code,0);
                    end
                  else
                    begin
                    SetLength(code,0);
                    CryptWriteEx(call.crypt,temp);
                    WriteEx(temp);

                    SetLength(temp,1);
                    temp[0]:=0;
                    CryptWriteEx(call.crypt,temp);
                    WriteEx(temp);
                    SetLength(temp,0);
                    end;
                  end;
                end
              else
              {$ENDIF}
                begin
                if call.FMT=fmt_XMLRPC then
                  Write(MyResult.toXMLrpcResponse)
                else
                  begin
                  if assigned(call.crypt) and assigned(call.crypt.CWrite) then
                    begin
                    temp:=MyResult.toCodeEx;
                    CryptWriteEx(call.crypt,temp);
                    WriteEx(temp);
                    end
                  else
                    Write(MyResult.toCode);
                  end;
                end;
              end;
          finally
            RtcFreeAndNil(MyResult);
            end;
        except
          on E:EDelayedCall do
            begin
            // Create and Post the DelayedCall
            mycall:=E.call;
            Info.Obj[RTC_SERVERMODULE_DELAYED_CALL]:=mycall;

            mycall.Conn:=Conn;
            mycall.output:=call.output;
            mycall.FMT:=call.FMT;
            mycall.Compress:=call.Compress;
            mycall.crypt:=call.crypt;
            mycall.SessionID:=call.SessionID;
            mycall.ObjectDataOutEvent:=call.ObjectDataOutEvent;

            call.crypt:=nil;
            call.output:=nil;

            RtcFreeAndNil(call);

            if Conn.isExtension then
              begin
              released:=True;
              {$IFDEF AUTOREFCOUNT} DisposeOf; 
              {$ELSE} RtcFreeAndNil(self); {$ENDIF}
              mycall.Execute;
              end
            else
              mycall.Post(MultiThreaded);

            Exit; // not sending the reponse from here!
            end;
          on E:Exception do
            with TRtcValue.Create do
              try
                asException:=E.Message;
                if assigned(call.output) then
                  to_Code(call.output)
                else
                  begin
                  if call.FMT=fmt_XMLRPC then
                    Write(toXMLrpcResponse)
                  else
                    begin
                    if assigned(call.crypt) and assigned(call.crypt.CWrite) then
                      begin
                      temp:=toCodeEx;
                      CryptWriteEx(call.crypt,temp);
                      WriteEx(temp);
                      end
                    else
                      Write(asCode);
                    end;
                  end;
              finally
                Free;
              end;
          end;

        {$IFDEF COMPRESS}
        if assigned(call.output) then
          begin
          try
            code:=call.output.GetEx;
          finally
            RtcFreeAndNil(call.output);
            end;

          if length(code)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWriteEx(call.crypt, code);
            WriteEx(code);
            end
          else
            begin
            { we have stored uncompressed data in "output",
              now we need to compress it all and send it out. }

            SetLength(temp,0);
            case call.Compress of
              cFast: temp:=ZCompress_Ex(code,zcFastest);
              cMax: temp:=ZCompress_Ex(code,zcMax);
              else temp:=ZCompress_Ex(code,zcDefault);
              end;

            if length(temp)>=length(code)-1 then
              begin
              SetLength(temp,0);
              CryptWriteEx(call.crypt,code);
              WriteEx(code);
              SetLength(code,0);
              end
            else
              begin
              SetLength(code,0);
              CryptWriteEx(call.crypt,temp);
              WriteEx(temp);

              SetLength(temp,1);
              temp[0]:=0;
              CryptWriteEx(call.crypt,temp);
              WriteEx(temp);
              SetLength(temp,0);
              end;
            end;
          end;
        {$ENDIF}
        
        if assigned(call.crypt) and assigned(call.crypt.CWrite) then
          begin
          // Add control key to the end of our response
          temp:=RtcStringToBytes(call.crypt.ControlKey);
          CryptWriteEx(call.crypt, temp);
          WriteEx(temp);
          SetLength(temp,0);
          end;

        Flush;

      finally
        LeaveEvent;
        end;
      end;
  finally
    RtcFreeAndNil(call);
    if not released then
      {$IFDEF AUTOREFCOUNT} DisposeOf;
      {$ELSE} RtcFreeAndNil(self); {$ENDIF}
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcSrvModule Initializing ...','DEBUG');{$ENDIF}

zeroObj:=TZeroObj.Create;
CS:=TRtcCritSec.Create;
List:=tObjList.Create(128);

{$IFDEF RTC_DEBUG} Log('rtcSrvModule Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSrvModule Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

KillObjs;

RtcFreeAndNil(List);
RtcFreeAndNil(CS);
RtcFreeAndNil(zeroObj);

{$IFDEF RTC_DEBUG} Log('rtcSrvModule Finalized.','DEBUG');{$ENDIF}
end.


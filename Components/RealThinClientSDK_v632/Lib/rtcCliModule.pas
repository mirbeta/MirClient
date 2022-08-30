{
  @html(<b>)
  Client Module for Remote Functions
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit introduces @Link(TRtcClientModule), the client-side component for ENABLING remote functions.
  By using @Link(TRtcClientModule), you can easily call remote server functions and get results
  in form of objects, passed to the event handlers you define. Also, by assigning a
  @Link(TRtcFunctionGroup) component to your @Link(TRtcClientModule) component,
  server can (as a result of any remote function call from the client) return functions which
  will be executed on the client side before the result object is passed to the local event handler.
  Implementing a RTC Remote Function is as easy as writing a local function.
}

unit rtcCliModule;

{$INCLUDE rtcDefs.inc}

interface

uses
  Classes,
  SysUtils,

  rtcTypes,
  rtcThrPool,
  rtcTimer,
  rtcLog,
  rtcInfo,
  rtcLink,
  rtcConn,
  rtcCrypt,

  rtcSyncObjs,

  rtcDataCli,
  rtcFunction,

{$IFDEF COMPRESS}
  rtcZLib,
{$ENDIF}

  rtcFastStrings,
  memObjList,
  memXObjList;

var
  LOG_CLIENTMODULE_ERRORS:boolean={$IFDEF RTC_DEBUG}True{$ELSE}False{$ENDIF};

type
  ERtcExecuteError = class(Exception);

  // @exclude
  EPostInteractive = class(EAbort);

  TRtcEncryptionClass = TRtcCrypt;

  TRtcClientModule = class; // forward

  { @abstract(Client Remote RTC Object Manager implementation) }
  TRtcClientObjectManager = class(TRtcBasicRemoteObjectManager);

  // @exclude
  TRtcInteractiveResult = class(TObject)
  public
    FEvent:TRtcResult;
    Data,Result:TRtcValue;

    destructor Destroy; override;
    end;

  { @abstract(Used to store all calls for a single Post from a ClientModule)

    @exclude }
  TRtcClientModuleCallsArray=class(TRtcArray)
  private
    FEvents:array of TRtcResult;

    function GetEvent(index: integer): TRtcResult;
    procedure SetEvent(index: integer; const _Value: TRtcResult);

  public
    constructor Create; override;
    destructor Destroy; override;

    property Event[index:integer]:TRtcResult read GetEvent write SetEvent;
    end;

  // @exclude
  TRtcCryptClient=class(TRtcObject)
  public
    HaveHello,HaveStart,AllReady:boolean;
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

  // @exclude
  TRtcClientModuleData=class
  public
    FRequest:TRtcClientRequest;
    FData:TRtcValue;
    FPostLevel:integer;
    FCalls:TRtcClientModuleCallsArray;

    constructor Create; virtual;
    destructor Destroy; override;
    end;

  { @abstract(Use to call remote functions and receive their results)

    ClientModule is used to prepare remote function calls, post them to
    the server, accept server's response and call local event handlers with
    the result received for each call. You can post a single call or multiple
    function calls with one request, or even use a timer-based trigger to post
    all the requests prepared until now. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcClientModule=class(TRtcAbsDataClientLink)
  private
    FMyData:TObjList;
    FMainThrData:TRtcClientModuleData;

    FExecuteResult:TRtcValue;
    FExecuteHandler:TRtcResult;

    // Used for "PostInteractiveResult"
    FIntCS:TRtcCritSec;

    // Used for "FMyData" when HyperThreading=TRUE
    FCS:TRtcCritSec;

    FIntTimer:TRtcTimer;
    FIntRes:TXObjList;

    FPingTimer:TRtcTimer;

    FFunctions:TRtcFunctionGroup;
    FRelease:boolean;

    FModuleFileName:RtcString;
    FModuleHost:RtcString;
    FCryptSesName:RtcString;
    FObjManSesName:RtcString;
    FAutoRepost:integer;
    FAutoSessions: boolean;

    FOnBeginRequest: TRtcNotifyEvent;
    FOnResponseAbort: TRtcNotifyEvent;
    FOnResponseDone: TRtcNotifyEvent;
    FOnResponseReject: TRtcNotifyEvent;
    FOnConnectLost: TRtcNotifyEvent;
    FOnSessionExpired: TRtcNotifyEvent;
    FOnRepostCheck: TRtcNotifyEvent;

    FOnSessionOpen: TRtcNotifyEvent;
    FOnSessionClose: TRtcNotifyEvent;

    FAutoEncrypt: integer;
    FForceEncrypt: boolean;
    FOnResponseError: TRtcNotifyEvent;
    FSecureKey: RtcString;

    FCompress: TRtcCompressLevel;

    FOnNoEncryption: TRtcNotifyEvent;
    FOnNeedEncryption: TRtcNotifyEvent;
    FOnWrongEncryption: TRtcNotifyEvent;

    FOnLoginResult: TRtcResultEvent;
    FOnLoginAborted: TRtcResultEvent;
    FOnResultError: TRtcResultErrorEvent;

    FOnObjectLinkAborted: TRtcResultEvent;
    FOnObjectLinkErrors: TRtcResultEvent;
    FOnObjectDataOut: TRtcDataEvent;
    FOnObjectDataIn: TRtcDataEvent;
    FOnObjectCreate: TRtcObjectCreateEvent;

    FHyperThreading: boolean;
    FDataFormat: TRtcDataFormat;
    FOnLogin: TRtcFunctionPrepareEvent;

    FResetLogin,
    FAutoLogin: boolean;
    FLoginResult: TRtcResult;
    FPingResult: TRtcResult;

    FObjectManager: TRtcClientObjectManager;

    FObjectLinkSupport: TRtcObjectLinkSupport;
    FObjectLinkResult: TRtcResult;
    FObjectLinksOut: boolean;
    FAutoSessionsPing: integer;
    FOnPing: TRtcFunctionPrepareEvent;
    FOnPingAborted: TRtcResultEvent;
    FOnPingResult: TRtcResultEvent;

    function CheckMyData:TRtcClientModuleData;
    function GetMyData:TRtcClientModuleData;
    procedure ClearMyData;

    function IsRemoteCallRequest(Sender:TRtcConnection):boolean;

    procedure NotifyResultAborted(Sender:TRtcConnection);

    procedure Response_Problem(Sender:TRtcConnection);

    procedure Call_SessionExpired(Sender:TRtcConnection);
    procedure Call_NoEncryption(Sender:TRtcConnection);
    procedure Call_NeedEncryption(Sender:TRtcConnection);
    procedure Call_WrongResponse(Sender:TRtcConnection);
    procedure Call_WrongEncryption(Sender:TRtcConnection);
    procedure Call_WrongEncryptionStart(Sender:TRtcConnection);
    procedure Call_ResultError(Sender:TRtcConnection; Data,Result:TRtcValue; E:Exception);

    function GetCrypt(Sender:TRtcConnection):TRtcCryptClient;
    procedure NewCrypt(Sender:TRtcConnection);
    procedure DelCrypt(Sender:TRtcConnection);
    procedure ResetCrypt(Sender:TRtcConnection);

    function GetFunctionGroup: TRtcFunctionGroup;
    procedure SetFunctionGroup(const Value: TRtcFunctionGroup);

    function GetModuleFileName: RtcString;
    procedure SetModuleFileName(const Value: RtcString);

    function GetModuleHost: RtcString;
    procedure SetModuleHost(const Value: RtcString);
    procedure SetAutoEncrypt(const Value: integer);
    procedure SetAutoSessions(const Value: boolean);
    procedure SetForceEncrypt(const Value: boolean);

    procedure PostInteractiveResult(Event:TRtcResult; Data,Result:TRtcValue);

    procedure DoInteractiveResult;

    procedure DoSessionPing;

    function Get_Data: TRtcValue;
    function GetPostLevel: integer;
    function GetRequest: TRtcClientRequest;

    procedure SetCompress(const Value: TRtcCompressLevel);

    procedure SetDataFormat(const Value: TRtcDataFormat);

    procedure SetAutoLogin(const Value: boolean);

    procedure LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False); virtual;
    procedure PingCall(ResultHandler: TRtcResult; Sender:TRtcConnection); virtual;

    procedure Call_LoginResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_LoginAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_PingResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_PingAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

    procedure Call_ObjectLinkResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_ObjectLinkAborted(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    procedure Call_ObjectLinkErrors(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);

    procedure Call_ExecuteResult(Sender:TRtcConnection; Data:TRtcValue; Result:TRtcValue);
    function GetLastCallParams: TRtcFunctionInfo;

    procedure SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
    procedure SetAutoSessionsPing(const Value: integer);

    procedure DoExecute(Sender:TRtcDataClient; var MyData:TRtcValueObject);
    procedure DoObjectManagerDataReady(Sender:TRtcRemoteObjectManager);
    procedure DoObjectManagerSend(Sender:TRtcRemoteObjectManager; Conn:TRtcConnection);

    procedure DoObjectCreate(Sender:TObject; Param:TRtcObjectCall);

    procedure DoReactivateObjectManager(Sender:TRtcConnection);
    procedure DoDeactivateObjectManager(Sender:TRtcConnection);

  protected
    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure Call_BeginRequest(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseDone(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseData(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseAbort(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_ResponseReject(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_SessionOpen(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_SessionClose(Sender:TRtcConnection); override;

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
    procedure Call_ConnectLost(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_RepostCheck(Sender:TRtcConnection); override;

  public
    // @exclude
    constructor Create(AOwner:TComponent); override;
    // @exclude
    destructor Destroy; override;

    { Return this TRtcClientModule's Object Manager.

      If this TRtcClientModule does NOT have an Object Manager,
      calling "GetObjectManager(True)" will create an Object Manager,
      while calling "GetObjectManager(False)" will return NIL. }
    function GetObjectManager(xCreate:boolean=False): TRtcRemoteObjectManager;

    { Activate the Object Manager associated with this TRtcClientModule. @html(<br><br>)

      If this TRtcClientModule does NOT have an Object Manager,
      calling "ActiveObjectManager(True)" will create an Object Manager,
      while calling "ActiveObjectManager(False)" will raise an exception. @html(<br><br>)

      If the "ObjectLinks" property is set to "ol_Manual", this method needs to be called
      before calling any remote functions on the Server which might return "Linked Objects" data.
      It also needs to be called before creating any Linked Objects in code on the Client-side. @html(<br><br>) }
    procedure ActivateObjectManager(xCreate:boolean=True);

    { Use this method if you want to force the removal of any
      Object Manager associated with this TRtcClientModule - now. }
    procedure RemoveObjectManager;

    { Use this method if you want to remove all Objects linked to
      the Object Manager associated with this TRtcClientModule - now. }
    procedure RemoveManagedObjects;

    // You can call this method from Interactive result function to destroy the ClientModule object.
    procedure Release;

    { Use this method when you want to force the next remote call to make a new login attempt,
      even if the component thinks the user is already logged in. }
    procedure ResetLogin;

    { If you want to post multiple calls in one request (and not each in its own request),
      you can use the "StartCalls" methods to open a new "call transaction", which is closed
      by a call to "Post". Each "StartCalls" has to be closed by a call to "Post". Using
      StartCalls/Call/Call/Post, you can combine a number of remote calls in one request.
      Another thing you don't have to worry about when using StartCalls/Post is clearing
      of the Data object in case of an exception during Data preparation. }
    procedure StartCalls; virtual;

    { After you have used the "Data" property to prepare the objects and functions
      you want to send to the server, use the "Call" method to define the event handler
      which has to receive the result after the data has been posted to the server.
      If you are not interested in the result values of your request, but just
      want to send this to the server as a "procedure call" and ignore the result,
      you can use "Call(nil)" and any result received will be simply thrown away.
      A result WILL be received in any case, which ensures that the function was executed.
      But, even in case of an exception, result will be ignored. @html(<br><br>)

      After "Call()", an object will be created to hold the prepared "Data" with a
      pointer to the TRtcResult component, after which the Data property will be cleared,
      so that you can prepare and Call new remote functions immediatelly. @html(<br><br>)

      If you are calling a remote function from inside other remote functions event,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Call() method. @html(<br><br>)

      If you didn't start a separate call transaction using "StartCalls", your call will be
      automaticaly posted to the server in a single request. To post multiple calls in
      one request, simply call "StartCalls" first, prepare "Data" before each "Call" and
      after the last call, use the "Post" method to send everything out. }
    procedure Call(ResultHandler:TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); overload;

    { "Post" will decrease the call transaction level which was increased by a "StartCalls"
      and if the last StartCalls was closed by calling this Post, an object will be created
      to hold the prepared Request info and a list of remote calls will be sent to the server
      if connection with server is established. @html(<br><br>)

      When posting from inside a RTC event or a remote function,
      "FromInsideEvent" parameter has to be TRUE to avoid memory consumption and
      you should pass the Sender:TRtcConnection parameter to the Post() method. @html(<br><br>)

      Events assigned to this TRtcClientModule will not be removed nor cleared,
      so you can define them at design-time and not worry about them at runtime. }
    procedure Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil); virtual;

    { Cancel *all* unsent calls prepared since "StartCalls", and decrement CallsLevel to zero (0). }
    procedure CancelCalls; virtual;

    { ONLY use this Request property if you need to prepare a request BEFORE posting.
      @html(<br>)
      DO NOT directly use this property when processing function calls.
      After a request has been posted, it is moved to the DataClient,
      so you can access it (from events) using Sender's Request property. }
    property Request:TRtcClientRequest read GetRequest;

    { To prepare a remote call, use this "Data" property to define and/or assign object(s)
      you want to send to the server. After you have defined the "Data" property to hold
      all information you want to send to the server, use the "Call" method to store that
      data and define the event handler to be used when a result is received. @html(<br>)
      ONLY use this Data property to prepare data for a remote call. @html(<br>)
      DO NOT directly use this property when processing the received result. }
    property Data:TRtcValue read Get_Data;

    { As an alternative to using the Data property directly for preparing data for
      the next function call, if you only want to prepare a single remote function call
      for which you would normally use "Data.newFunction()", you can also use "Prepare". @html(<br><br>)

      The "Prepare" method returns a pointer to the TRtcFunctionInfo object, which you
      can use to prepare all function parameters. Using "Prepare" is also easier than
      using the "Data" property directly, because each call to "Prepare" will first
      make a call to "Data.Clear" to make sure there are no left-overs from
      possible problems from any prior use of the "Data" or "Prepare" methods.
      That is, as long as you only use the "Prepare" method and do NOT use "Data" directly.

      Using "Prepare" is identical to using "Data.Clear" and "Data.NewFunction()"; }
    function Prepare(const FunctionName:RtcWideString):TRtcFunctionInfo;

    { After using "Prepare" or "Data.newFunction" to create a new Function call object,
      you can use the "Param" property to prepare all your function call parameters. @html(<br><br>)

      Using "Param" is identical to using "Data.asFunction" }
    property Param:TRtcFunctionInfo read GetLastCallParams;

    { Using this property, you can check at which Calls level your ClientModule currently is.
      CallsLevel will be 0 outside of StartCalls, increased by 1 after each StartCalls
      and decreased by 1 after each Post. }
    property CallsLevel:integer read GetPostLevel;

    { If you want to send a remote function call to the Server and wait for the result without
      using a separate TRtcResult component and without writing the OnResult and OnAbort events,
      you can prepare a synchronized (blocking) call by using the "Prepare" method and then
      (once prepared) execute the remote call and get a result by using the "Execute" method. @html(<br><br>)

      In case of a communications error during the remote call, the "Execute" function will
      raise an exception with the appropriate error text. If everything goes well (no exeception),
      you will get your result directly from the "Execute" method in form of the TRtcValue object. @html(<br><br>)

      There are two ways one can use "Execute": @html(<br>)
        1) The easy way is to use "Execute" or "Execute(TRUE)" (meaning that AutoFreeResult=TRUE),
           in which case the Result object you receive will be automatically destroyed the next time
           you use the "Execute" method, or when the TRtcClientModule component is being destroyed. @html(<br>)
        2) The advanced way is to use "Execute(FALSE)", in which case you will have to FREE the
           result object received from "Execute(FALSE)" manually, once you are finished using it. @html(<br><br>)

      Using the "Execute" method is easier than using the "Call" method because: @html(<br>)
        (A) You will have access to the result data immediately after the "Execute" line,
            in the same code segment where you have "executed" your remote function call. @html(<br>)
        (B) You can either keep the result object for as long as you want and free it manually
            when using "Execute(FALSE)", or ... you can let the TRtcClientModule component free
            the result object automatically for you by using "Execute" or "Execute(TRUE)". @html(<br><br>)

      You can also specify a timeout (in seconds) how long you are ready to wait for a result.
      Should you get no result in your specified time period, request will be cancelled and an exception will be raised. @html(<br><br>)

      And ... if you are using a MultiThreaded connection and you do NOT WANT paint and other non-user messages to
      be processed while waiting for the result, call the "Execute" method with "AllowMessageProcessing=FALSE".
      For example: Execute(True, 30, False); // Auto free result, wait max 30 seconds, do NOT allow message processing while waiting. @html(<br><br>)

      Also take a look at the "LastResult" property, which is available to you if you call Execute with
      its default value for the AutoFreeResult parameter (TRUE) and gives you direct access to the result
      object received from the Execute method without having to use local variables or the "with" statement. @html(<br><br>)

      WARNING: Because of its blocking implementation, "Execute" method can NOT be used from events triggered
      by the RTC SDK (for example, from the OnResult or OnAbort events triggered by a remote call issued by
      using the "Call" method) and ... the "Execute" method can ONLY be used from one thread at a time. @html(<br><br>)

      Here are three usage examples, all of which are basically
      doing the same thing, but each using a different syntax.
      You can combine them any way you want: @html(<br><br>)

      @longCode(#
      ** EXAMPLE 1 **

      with MyClientModule do
        begin
        Prepare('myfunctionname');
        Param.asString['par1']:='Hi';
        Param.asInteger['par2']:=12345;
        Execute;
        // use the "LastResult" property to access the result data
        end;

      ** EXAMPLE 2 **

      with MyClientModule do
        begin
        with Prepare('myfunctionname') do
          begin
          asString['par1']:='Hi';
          asInteger['par2']:=12345;
          end;
        with Execute do // result object will be Freed automatically
          begin
          // ... access the result data here ...
          end;
        end;

      ** EXAMPLE 3 **

      with MyClientModule do
        begin
        Data.Clear;
        with Data.newFunction('myfunctionname') do
          begin
          asString['par1']:='Hi';
          asInteger['par2']:=12345;
          end;
        myRes:=Execute(False); // you will need to Free "myRes"
        try
          .. use myRes ...
        finally
          myRes.Free;
          end;
        end;
      #)
      }
    function Execute(AutoFreeResult:boolean=True; _Timeout:cardinal=0; AllowMessageProcessing:boolean=True):TRtcValue;

    { After the last "Execute" call, if you have used the default value (TRUE) for the "AutoFreeResult" parameter
      ("Execute" or "Execute(True)") to signal the component that you do NOT want to keep the result object to yourself
      and that you want the result object to be freed automatically by the component before the next "Execute" call,
      you can use the "LastResult" property to access the last object returned from the "Execute" method. @html(<br><br>)

      Please note that the "LastResult" property will point to the result returned from the LAST "Execute" call.
      "LastResult" will return NIL if there was an error during the last Execute call (the call was not finished)
      or if you have last used Execute(FALSE) -> using "AutoFreeResult=FALSE" in your Execute call parameters. }
    property LastResult:TRtcValue read FExecuteResult;

  published
    { Use this property to define what compression level you want to use when sending
      data from Client to Server. Default Compression value is "cNone" (no compression).
      You can use different compression levels between client and server (for example,
      fast or no compression from client and max from server). If your client has to
      work with servers which don't support compression, you have to use "cNone". }
    property Compression:TRtcCompressLevel read FCompress write SetCompress default cNone;

    { Use this property to define what data format you want to use when sending data from
      Client to Server. If your client will only be talking to a Server written using RTC
      components, it is recommended to use "fmt_RTC", which upports all RTC data types, automatic
      compression, automatic encryption and automatic Sessions, as well as nested function calls
      and sending multiple function calls in a single request. On the other hand, if your client
      has to communicate with a Server which does NOT support the RTC format, you can use
      the "fmt_XMLRPC" format, which will work with Servers implementing the XML-RPC format. @html(<br><br>)
      NOTE: Since advanced functionality like automatic Compression, auto Encryption
      and automatic Sessions are native to the RTC format and other Servers do not implement it,
      those advanced properties will be ignored and assumed to be FALSE with all formats other
      then the "fmt_RTC" format. If you need those advanced options, use the "fmt_RTC" format. }
    property DataFormat:TRtcDataFormat read FDataFormat write SetDataFormat default fmt_RTC;

    { If you want to enable the possibility to use this Client Module to send remote function
      calls from multiple threads AT THE SAME TIME, where this component will acs as if
      it were X components, one for each thread, simply set HyperThreading to TRUE. @html(<br><br>)

      This is useful if you need to send remote function calls to another Server from
      within your Server running in multi-threaded mode and want to use only one set of
      rtcHttpClient/rtcClientModule components for all clients connected to your Server.
      Even in HyperThreading mode, only properties and methods needed to prepare and post remote
      function calls (Data, Request, Call, StartCalls, PostLevel and Post) will use a separate
      copy for each thread, while all other properties and methods exist only once for all threads,
      so don't try to modify them while your application is actively using the component in
      multi-threaded mode. @html(<br><br>)

      Leave HyperThreading as FALSE to use this component "the stadard way" (for example,
      when you're writing a client application where remote calls are done from the main thread
      or if you are creating a separate component for every thread that needs it). }
    property HyperThreading:boolean read FHyperThreading write FHyperThreading default False;

    { - Set "ObjectLinks" to "ol_None" (default) to completely disable the "RTC Linked Objects"
        feature for this RTC Client Module. When "ObjectLinks=ol_None", calling the
        "ActivateObjectManager" method will raise an exception, because any RTC Linked Objects
        created for that Object Manager would NOT be sent to the Server. @html(<br><br>)

        When writing Client Applications using the "RTC Linked Objects" feature, only
        *one* "TRtcClientModule" should be used for all "RTC Linked Objects" communication.
        Leave the "ObjectLinks" property on all other TRtcClientModule components at "ol_None".
        Following that rule eliminates the need for the Client to "switch" between Object Managers,
        which keeps the Client-side code a lot easier to implement and to maintain. @html(<br><br>)

      - Set "ObjectLinks" to "ol_Manual" if you want to call the "ActivateObjectManager" method
        manually before any "Linked Objects" may be created by using this TRtcClientModule.

      - Set "ObjectLinks" to "ol_AutoServer" if you want an Object Manager to be created automatically
        after "Linked Objects" data is received from the Server, which allows the Server to create
        Linked Objects without having to wait for the Client to call "ActivateObjectManager". @html(<br><br>)

        With "ObjectLinks=ol_AutoServer", the Client will still have to call "ActivateObjectManager"
        before creating Linked Objects locally on the Client-side, because the "ol_AutoServer" setting
        only has an effect on data received for Linked Objects from the Server. @html(<br><br>)

        "Linked Objects" are usually created on the Server, while Clients only re-create them.
        TRtClientModule in that setup should set its "LinkedObjects" property to "ol_AutoServer"
        and the TRtcServerModule (Server-side) should use "ol_Manual" or "ol_AutoServer". @html(<br><br>)

      - Set "ObjectLinks" to "ol_AutoClient" or "ol_AutoBoth" if you want an Object Manager to be
        created for this TRtcClientModule immediatelly (when this property is set), even if there is
        no connection to the Server. This will allow you to start creating Linked Objects on the Client
        immediatelly, without having to call the "ActivateObjectManager" method manually. It will also
        ensure that a new Object Manager is created automatically in case this one is closed or removed. @html(<br><br>)

        If Linked Objects need to be created manually on the Client, while the Server only recreates
        them, the best option is to set the "ObjectLinks" property on the Client (TRtcClientModule)
        and on the Server (TRtcServerModule) to "ol_AutoClient". This will create the Object Manager
        on the Client-side immediatelly, so that the Client can start creating Linked Objects locally
        and the Server will create and activate an Object Manager when receiving Linked Objects data. @html(<br><br>)

        NOTE: Destroying the Server-side "Linked Object" instance will also destroy the
        Client-side instance and vice-versa, so you shouldn't keep a reference to a "Linked Object"
        inside global variables - unless you have set up destruction notification events
        to clear these refferences in case the instance is being destroyed. }
    property ObjectLinks:TRtcObjectLinkSupport read FObjectLinkSupport write SetObjectLinkSupport default ol_None;

    { Set this property to a value other than 0 if you want to use automatic Encryption
      with a random generated key of "EncryptKey" bytes. One byte stands for
      encryption strength of 8 bits. For strong 256-bit encryption, use 32 bytes. @html(<br><br>)

      The final encryption key is combined from a client-side key and a key
      received from the server, where server decides about its encryption strength.
      If server doesn't support Encryption, data will not be encrypted,
      regardless of the value you use for AutoEncrypt. @html(<br><br>)

      EncryptionKey uses sessions to keep the encryption keys. When you set EncryptionKey
      to a value other than 0 (turn it ON), AutoSessions will be set to TRUE.
      Also, setting AutoSessions to FALSE will set EncryptionKey to 0 (OFF). }
    property EncryptionKey:integer read FAutoEncrypt write SetAutoEncrypt default 0;

    { If you need a 100% secure connection, define a Secure Key string
      (combination of letters, numbers and special characters) for each
      ServerModule/ClientModule pair, additionally to the EncryptionKey value.
      ClientModule will be able to communicate with the ServerModule ONLY if
      they both use the same SecureKey. Default value for the SecureKey is
      an empty string (means: no secure key). @html(<br><br>)

      SecureKey will be used in the encryption initialisation handshake,
      to encrypt the first key combination sent by the ClientModule.
      Since all other data packages are already sent using some kind of encryption,
      by defining a SecureKey, you encrypt the only key part which would have
      been sent out without special encryption. }
    property SecureKey:RtcString read FSecureKey write FSecureKey;

    { Setting this property to TRUE will tell the ClientModule to work with the
      Server ONLY if Server supports encryption. If AutoEncryptKey is > 0 and
      server doesn't support encryption, function calls will not be passed to
      the server and any response coming from the server will be rejected, until
      server enables encryption. }
    property ForceEncryption:boolean read FForceEncrypt write SetForceEncrypt default False;

    { Set this property to TRUE if you want ClientModule to request a new session
      automatically if the Session.ID is not set when posting a request.
      Session handling is built into the ClientModule and uses Request.Params to
      send the Session.ID to the server and Response.Cookie['ID'] to receive a
      new session ID from the server and initialize the session object. @html(<br><br>)

      Since session ID's are communicated automaticaly by the ClientModule and
       ServerModule components, all TRtcFunction and TRtcResult components used by
      this ClientModule will have direct access to the session object.
      When AutoSessions is set to true, a new session will be requested if
      no session exists or when a session expires. @html(<br><br>)

      When AutoSessions is FALSE, you have to request a new session by calling
      a remote server function to generate a new session and return the session ID. }
    property AutoSessions:boolean read FAutoSessions write SetAutoSessions default false;

    { Use "AutoSessionsPing" to enable automatic "PING" calls to the Server when there
      is an active Session but no Remote Calls have been sent to the Server using
      THIS TRtcClientModule for longer than "AutoSessionsPing" seconds. }
    property AutoSessionsPing:integer read FAutoSessionsPing write SetAutoSessionsPing default 0;

    { Set this property to a value other than 0 (zero) if you want the ClientModule to
      auto-repost any request up to "AutoRepost" times, in case the connection gets lost
      while sending data to server or receiving data from server.
      If value is lower than zero, requests will be reposted unlimited number of times. }
    property AutoRepost:integer read FAutoRepost write FAutoRepost default 0;

    { Set this property to TRUE if you want to enable the use of the
      OnLogin, OnLoginResult and OnLoginAborted events to implement automatic login. }
    property AutoLogin:boolean read FAutoLogin write SetAutoLogin default false;

    { "Request.Host" will be assigned this property before sending the request out. @html(<br>)
      It is not necessary to set this property if your server's ServerModule component
      left its ModuleHost property blank and there's no remote Functions used by that
      ServerModule which would explicitly use the "Request.Host" header. On the other hand,
      for servers which serve multiple hosts, mostly where ServerModule has assigned its
      ModuleHost property, it is very important to set this ClientModule's ModuleHost
      property to the appropriate host name. }
    property ModuleHost:RtcString read GetModuleHost write SetModuleHost;
    { To be able to call remote functions, this ClientModule's ModuleFileName
      property has to be identical to the "ModuleFileName" property of the ServerModule
      which you want to use. "Request.FileName" will be assigned this property before
      sending any request out, so you won't be preparing the Request headers manualy. @html(<br>)
      All data (parameters and function calls) will be passed to the server module through
      request's Content body, so that ServerModule won't need to check the request headers
      for anything else than it's FileName to know if the request is directed to it. }
    property ModuleFileName:RtcString read GetModuleFileName write SetModuleFileName;
    { Set this property to tell the RtcClientModule to use this TRtcFunctionGroup
      component to execute all functions received as a response from server, for
      any request sent from this TRtcClientModule component. }
    property FunctionGroup:TRtcFunctionGroup read GetFunctionGroup write SetFunctionGroup;

    { This event will be called if your SecretKey does not match the SecretKey
      specified by the ServerModule you're connecting to.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to update your SecretKey property to mirror the SercetKey of your ServerModule. }
    property OnEncryptWrongKey:TRtcNotifyEvent read FOnWrongEncryption write FOnWrongEncryption;
    { This event will be called if your EncryptionKey>0 and ForceEncryption=TRUE,
      but the Server says it does not support encryption for this ServerModule.
      On this event, you can decide not to work with that server (Response.Reject or Disconnect),
      or to set your ForceEncryption property to False and repost the request. }
    property OnEncryptNotSupported:TRtcNotifyEvent read FOnNoEncryption write FOnNoEncryption;
    { This event will be called if your EncryptionKey=0,
      but the Server wants to ForceEncryption for this ServerModule.
      On this event, you can decide to not work with that server (Response.Reject or Disconnect),
      or to activate encryption by setting the EncryptionKey. }
    property OnEncryptRequired:TRtcNotifyEvent read FOnNeedEncryption write FOnNeedEncryption;

    { This event will be called if we receave invalid response from the Server,
      which could mean that our Client or our Server are not up to date. }
    property OnResponseError:TRtcNotifyEvent read FOnResponseError write FOnResponseError;
    { This event will be called if you have called a remote function with a Session ID that
      has expired. You can choose to clear the local Session object and Repost the request
      with an empty session ID to receive a new session ID, or reject the Request.
      If you do not implement this event, Session ID will be cleared and the request
      will be reposted, so your client will receive a new Session ID. }
    property OnSessionExpired:TRtcNotifyEvent read FOnSessionExpired write FOnSessionExpired;
    { This event will be called after ClientModule component has prepared the request for sending,
      but before the request has been sent out (no writing has been done yet).
      You can use this event to check or update the request object before it is sent out. @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnBeginRequest:TRtcNotifyEvent read FOnBeginRequest write FOnBeginRequest;
    { This event will be called after the last DataReceived event for this request,
      read after the request has been sent out and a complete response was received (Response.Done). @html(<br>)
      This event does not have to be defined for the ClientModule to work. }
    property OnResponseDone:TRtcNotifyEvent read FOnResponseDone write FOnResponseDone;
    { This event will be called after the OnConnectLost, OnConnectFail and OnConnectError events,
      if the request was NOT marked for reposting. }
    property OnRepostCheck:TRtcNotifyEvent read FOnRepostCheck write FOnRepostCheck;
    { This event will be called after the OnRepostCheck event, if the request was not marked for reposting.
      If this event gets triggered, it means that there is a major problem with the server and
      user has to be notified about that problem and consulted about further actions. }
    property OnResponseAbort:TRtcNotifyEvent read FOnResponseAbort write FOnResponseAbort;
    { This event will be called after the response has been rejected by calling "Response.Reject" }
    property OnResponseReject:TRtcNotifyEvent read FOnResponseReject write FOnResponseReject;

    { This event will be called after a new Session has been opened. }
    property OnSessionOpen:TRtcNotifyEvent read FOnSessionOpen write FOnSessionOpen;
    { This event will be called before an existing Session is going to be closed. }
    property OnSessionClose:TRtcNotifyEvent read FOnSessionClose write FOnSessionClose;

    { This event will be mapped as @Link(TRtcClient.OnConnectLost) event
      to the assigned DataClient component and called if your connection gets
      closed while you are still processing your request. }
    property OnConnectLost:TRtcNotifyEvent read FOnConnectLost write FOnConnectLost;

    { Use this event to implement automatic user login. Set AutoLogin to TRUE and
      this event will be fired immediately after the initial connection handshake.
      To prepare the remote function, use the "Data" object passed as a parameter.
      After the function call has returned, the OnLoginResult event will be triggered.
      If there was an error and the request was aborted, OnLoginAborted will be called. }
    property OnLogin:TRtcFunctionPrepareEvent read FOnLogin write FOnLogin;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginResult:TRtcResultEvent read FOnLoginResult write FOnLoginResult;

    { Use this event to implement automatic user login. See "OnLogin" for more info. }
    property OnLoginAborted:TRtcResultEvent read FOnLoginAborted write FOnLoginAborted;

    { Use this event to implement automatic Sessin Pinging. Set AutoSessionsPing to
      the number of seconds a connection may remain inactive before we need to send a
      "PING call" to the Server to ensure that our Session will not Expire.
      This event is fired every time when a PING request has to be sent to the Server.
      To prepare the PING call, use the "Data" object passed as a parameter.
      After the PING call has returned, the OnPingResult event will be triggered.
      If there was an error and the request was aborted, OnPingAborted will be called. }
    property OnPing:TRtcFunctionPrepareEvent read FOnPing write FOnPing;

    { Use this event to implement automatic Session Pinging. See "OnPing" for more info. }
    property OnPingResult:TRtcResultEvent read FOnPingResult write FOnPingResult;

    { Use this event to implement automatic Session Pinging. See "OnPing" for more info. }
    property OnPingAborted:TRtcResultEvent read FOnPingAborted write FOnPingAborted;

    { This event will be triggered if a request for synchronizing "Linked Objects" failed. }
    property OnObjectLinkAborted:TRtcResultEvent read FOnObjectLinkAborted write FOnObjectLinkAborted;

    { This event will be triggered if there were Errors processing data to synchronize "Linked Objects".
      Any exceptions raised from THIS event will be silently handled. }
    property OnObjectLinkErrors:TRtcResultEvent read FOnObjectLinkErrors write FOnObjectLinkErrors;

    { This event is triggered when data is received from a remote "Object Manager".
      The main purpose of this event is to allow you to *monitor* all received "Linked Objects"
      packages without changing anything, but it could also be used to modify received data
      before it is forwarded to the local "Object Manager" for processing/execution.

      @param(Sender - NIL, or the connection component through which data was received)
      @param(Data - Data received from the remote "Object Manager".
             Unless you know exactly what you are doing and understand the format which
             local and remote "Object Manager" instances are using for communication,
             it is highly recommended that you do NOT make ANY changes to this instance.
             This is the instance that will be passed over to our "Object Manager" later) }
    property OnObjectDataIn:TRtcDataEvent read FOnObjectDataIn write FOnObjectDataIn;

    { This event is triggered *before* we send data prepared by our "Object Manager".
      The main purpose of this event is to allow you to *monitor* all "Linked Objects"
      packages before they are sent out, but it could also be used to modify prepared data.

      @param(Sender - NIL if using the default connection; "Sender" parameter for the Call method)
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

    { This event will be called if an Exception is raised while (A) processing the Result
      received from the Server, or (B) memory can not be cleaned up afterwards. }
    property OnResultError:TRtcResultErrorEvent read FOnResultError write FOnResultError;
    end;

{ Call this procedure if user interaction is required anywhere inside your result event.
  When this procedure is called, the event will be posted to the main thread outside of
  the client connection's context, so that the connection can continue functioning
  and receiving new data, without being blocked by a window waiting for user input.
  Without using "PostInteractive" to post the event, the connection would be blocked until
  the event returns. This could take several minutes if user doesn't notice your window,
  which would most likely result in a session timeout on the server, so the user would
  be automaticaly logged out after he responded to your questions. @html(<br><br>)

  Even though events are posted outside of the connection context, a mechanism integrated
  into TRtcClientModule will take care that all events posted interactively from the same
  ClientModule's result event, do not overlap or get called in a different order. So,
  if you need your result events to block any upcoming resuts, you can post all your
  dependent events interactively to ensure they will get called AFTER the user responded,
  while the connection continues receiving new data from the server and keeps the session alive. @html(<br><br>)

  NOTE: This procedure works only when called from inside TRtcResult event
  which was triggered by TRtcClientModule to return a result from a remote function call.
  When a function is called asynchtonously outside of the connection context,
  Sender parameter is NIL. This has two reasons: @html(<br>)
  1. You can check "Sender" to know if your event would block a connection. @html(<br>)
  2. You can not expect the connection to remain in the same state forever and you
     can not use the connection directly from an interactive event. }
procedure PostInteractive;

implementation

procedure PostInteractive;
  begin
  raise EPostInteractive.Create('');
  end;

{ TRtcClientModuleData }

constructor TRtcClientModuleData.Create;
  begin
  inherited;
  FRequest:=nil;
  FData:=nil;
  FCalls:=nil;
  FPostLevel:=0;
  end;

destructor TRtcClientModuleData.Destroy;
  begin
  try
    RtcFreeAndNil(FRequest);
    RtcFreeAndNil(FData);
    RtcFreeAndNil(FCalls);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModuleData.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcClientModule }

var
  ActiveGlobalManager:boolean=False;

constructor TRtcClientModule.Create(AOwner: TComponent);
  begin
  inherited Create(AOwner);

  FDataFormat:=fmt_RTC;
  FHyperThreading:=False;

  FIntCS:=TRtcCritSec.Create;
  FIntRes:=TXObjList.Create(32);
  FIntTimer:=nil;

  FPingTimer:=nil;

  FRelease:=False;
  FFunctions:=nil;
  FModuleFileName:='';
  FModuleHost:='';
  FCryptSesName:='.$CLI-CRYPT$';
  FObjManSesName:=RTCL_CLISESSION;
  FAutoRepost:=0;

  FCS:=TRtcCritSec.Create;
  FMyData:=tObjList.Create(32);
  FMainThrData:=TRtcClientModuleData.Create;

  FExecuteResult:=nil;
  FExecuteHandler:=TRtcResult.Create(nil);
  FExecuteHandler.OnReturn:=Call_ExecuteResult;

  FLoginResult:=TRtcResult.Create(nil);
  FLoginResult.OnReturn:=Call_LoginResult;
  FLoginResult.RequestAborted:=Call_LoginAborted;

  FPingResult:=TRtcResult.Create(nil);
  FPingResult.OnReturn:=Call_PingResult;
  FPingResult.RequestAborted:=Call_PingAborted;

  FObjectLinkResult:=TRtcResult.Create(nil);
  FObjectLinkResult.OnReturn:=Call_ObjectLinkResult;
  FObjectLinkResult.RequestAborted:=Call_ObjectLinkAborted;

  FObjectLinksOut:=False; // Linked Objects state: idle
  FObjectLinkSupport:=ol_None;
  end;

destructor TRtcClientModule.Destroy;
  begin
  try
    FRelease:=True;

    if assigned(FPingTimer) then
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;

    if ActiveGlobalManager and (FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth]) then
      ActiveGlobalManager:=False;
    RemoveObjectManager;

    FFunctions:=nil;
    FModuleFileName:='';
    FCryptSesName:='';
    FObjManSesName:='';
    FModuleHost:='';

    RtcFreeAndNil(FMainThrData);
    RtcFreeAndNil(FMyData);
    RtcFreeAndNil(FExecuteResult);

    RtcFreeAndNil(FExecuteHandler);
    RtcFreeAndNil(FLoginResult);
    RtcFreeAndNil(FPingResult);
    RtcFreeAndNil(FObjectLinkResult);

    RtcFreeAndNil(FIntRes);
    RtcFreeAndNil(FIntCS);
    RtcFreeAndNil(FCS);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModule.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcClientModule.GetCrypt(Sender:TRtcConnection): TRtcCryptClient;
  begin
  Result:=TRtcCryptClient(TRtcDataClient(Sender).Session._Obj[FCryptSesName]);
  end;

procedure TRtcClientModule.NewCrypt(Sender:TRtcConnection);
  var
    ob:TObject;
  begin
  with TRtcDataClient(Sender).Session do
    begin
    ob:=_Obj[FCryptSesName];
    if ob<>nil then
      TRtcCryptClient(ob).Init
    else
      _Obj[FCryptSesName]:=TRtcCryptClient.Create;
    end;
  end;

procedure TRtcClientModule.DelCrypt(Sender:TRtcConnection);
  var
    ob:TObject;
  begin
  with TRtcDataClient(Sender) do
    begin
    ob:=Session._Obj[FCryptSesName];
    if TRtcCryptClient(ob)<>nil then
      begin
      Session._Obj[FCryptSesName]:=nil;
      ob.Free;
      end;
    end;
  end;

procedure TRtcClientModule.ResetCrypt(Sender:TRtcConnection);
  var
    o:TRtcCryptClient;
  begin
  with TRtcDataClient(Sender) do
    begin
    o:=TRtcCryptClient(Session._Obj[FCryptSesName]);
    if o<>nil then
      begin
      if o.AllReady and Request.Reposting and (Request.Reposted<2) then
        Request.Query.ValueCS['ACTION']:='RESET'
      else
        begin
        Session._Obj[FCryptSesName]:=nil;
        RtcFreeAndNil(o);
        end;
      end;
    end;
  end;

procedure TRtcClientModule.Call_ConnectLost(Sender: TRtcConnection);
  begin
  if assigned(FOnConnectLost) then
    if AutoSyncEvents then
      Sender.Sync(FOnConnectLost)
    else
      FOnConnectLost(Sender);
  end;

function RandomKey(len:integer):RtcString;
  var
    a:integer;
  begin
  SetLength(Result,len);
  for a:=1 to len do
    Result[a]:=RtcChar(random(256));
  end;

procedure CryptRead(Crypt:TRtcCryptClient; var Data:RtcString);
  begin
  if assigned(Crypt) and assigned(Crypt.CRead) then
    Crypt.CRead.DeCrypt(Data);
  end;

procedure CryptWrite(Crypt:TRtcCryptClient; var Data:RtcString);
  begin
  if assigned(Crypt) and assigned(Crypt.CWrite) then
    Crypt.CWrite.Crypt(Data);
  end;

procedure CryptReadEx(Crypt:TRtcCryptClient; var Data:RtcByteArray);
  begin
  if assigned(Crypt) and assigned(Crypt.CRead) then
    Crypt.CRead.DeCryptEx(Data);
  end;

procedure CryptWriteEx(Crypt:TRtcCryptClient; var Data:RtcByteArray);
  begin
  if assigned(Crypt) and assigned(Crypt.CWrite) then
    Crypt.CWrite.CryptEx(Data);
  end;

function GenerateControlKey(var Counter:integer):RtcString;
  var
    len,a,b,c:integer;
  begin
  Inc(Counter);
  if Counter>99 then Counter:=1;

  len:=5+random(5);
  SetLength(Result,len+4);
  b:=(10-len)*9+8;
  for a:=5 to len+4 do
    begin
    c:=random(10); Inc(b,c);
    Result[a]:=RtcChar(c+Ord('0'));
    end;
  Result[1]:=RtcChar(b div 10 + Ord('0'));
  Result[2]:=RtcChar(b mod 10 + Ord('0'));
  Result[3]:=RtcChar(Counter div 10 + Ord('0'));
  Result[4]:=RtcChar(Counter mod 10 + Ord('0'));
  end;

procedure TRtcClientModule.Call_BeginRequest(Sender: TRtcConnection);
  var
    idx:integer;
    MyCalls:TRtcClientModuleCallsArray;
    compressed:boolean;
    code:RtcString;
    crypt:TRtcCryptClient;
    DataReq:TRtcDataRequestInfo;
    MyRequest:TRtcClientRequest;
  {$IFDEF COMPRESS}
    output:TRtcHugeString;
    codeEx:RtcByteArray;
    tempEx:RtcByteArray;
  {$ENDIF}
    obj:TRtcValueObject;
    val:TRtcValue;
    HaveCleared:integer;
  begin
  if FResetLogin then
    begin
    FResetLogin:=False;
    TRtcDataClient(Sender).Session.Close;
    end;

  if (FDataFormat=fmt_RTC) and (EncryptionKey>0) then
    begin
    with TRtcDataClient(Sender) do
      begin
      crypt:=GetCrypt(Sender);
      if (Request.Query.ValueCS['ACTION']='HELLO') then // Sending HELLO to the Server
        begin
        if Session.ID<>'' then
          Request.Query.ValueCS['ID']:=Session.ID
        else
          Request.Query.ValueCS['ID']:='';

        // Initialize encryption for this ClientModule
        NewCrypt(Sender);
        crypt:=GetCrypt(Sender);

        // Generate Client-Hello
        crypt.ClientHello:=RandomKey(EncryptionKey);

        { Generate randoml control number to add at the end of the request,
          so we can check if the response is correctly encrypted. }
        crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);
        code:=crypt.ClientHello+#13+crypt.ControlKey;

        if SecureKey<>'' then
          begin
          with TRtcEncryptionClass.Create do
            begin
            Key:=SecureKey;
            Crypt(code);
            Free;
            end;
          end;

        Request.Info.asBoolean['ClientModule$']:=True;
        // Send ClientHello + ControlKey
        Write(code);
        Exit;
        end
      else if (crypt=nil) or not crypt.HaveHello then
        begin
        if ModuleFileName='' then
          raise Exception.Create('Module FileName is undefined. Can not Post the request.');

        MyRequest:=TRtcClientRequest.Create;
        MyRequest.Method:='POST';
        MyRequest.FileName:=ModuleFileName;
        MyRequest.Query.ValueCS['ACTION']:='HELLO';
        if ModuleHost<>'' then
          MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          on E:Exception do
            if LOG_CLIENTMODULE_ERRORS then
              Log('TRtcClientModule.BeginRequest HELLO',E,'ERROR');
          end;
        Exit;
        end
      else if (Request.Query.ValueCS['ACTION']='START') then
        begin
        if Session.ID<>'' then
          Request.Query.ValueCS['ID']:=Session.ID
        else
          Request.Query.ValueCS['ID']:='';

        // Generate Client-Key
        crypt.ClientKey:=RandomKey(EncryptionKey);

        { Generate a random control number to add at the end of the request,
          so we can check if the response is correctly encrypted. }
        crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

        code:=crypt.ClientKey+#13+crypt.ControlKey;
        CryptWrite(crypt, code);

        Request.Info.asBoolean['ClientModule$']:=True;
        // Send ClientKey + ControlKey
        Write( code );
        Exit;
        end
      else if not crypt.HaveStart then
        begin
        if ModuleFileName='' then
          raise Exception.Create('Module FileName is undefined. Can not Post the request.');

        MyRequest:=TRtcClientRequest.Create;
        MyRequest.Method:='POST';
        MyRequest.FileName:=ModuleFileName;
        MyRequest.Query.ValueCS['ACTION']:='START';
        if ModuleHost<>'' then MyRequest.Host:=ModuleHost;

        DataReq:=TRtcDataRequestInfo.Create;
        DataReq.Request:=MyRequest;
        DataReq.Events:=Self;
        try
          InsertRequest(DataReq);
        except
          on E:Exception do
            if LOG_CLIENTMODULE_ERRORS then
              Log('TRtcClientModule.BeginRequest START',E,'ERROR');
          end;
        Exit;
        end;
      end;
    end;

  with TRtcDataClient(Sender) do
    if Session.ID<>'' then
      Request.Query.ValueCS['ID']:=Session.ID
    else if AutoSessions then
      Request.Query.ValueCS['ID']:='NEW'
    else
      Request.Query.ValueCS['ID']:='';

  if FAutoLogin then
    if not assigned(FOnLogin) then
      raise Exception.Create('OnLogin event missing for ClientModule "'+String(ModuleFileName)+'", but AutoLogin is TRUE.')
    else if not TRtcDataClient(Sender).Session.asBoolean['CLIMOD.LOGIN$'] and
            not TRtcDataClient(Sender).Request.Info.asBoolean['CLIMOD.LOGIN$'] then
      begin
      LoginCall(FLoginResult,Sender,True);
      Exit;
      end;

  if assigned(Link) then
    Link.Call_BeginRequest(Sender)
  else if assigned(Client) then
    Client.CallBeginRequest;

  if not TRtcDataClient(Sender).RequestInserted and
     not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    begin
    if assigned(FOnBeginRequest) then
      if AutoSyncEvents then
        Sender.Sync(FOnBeginRequest)
      else
        FOnBeginRequest(Sender);

    if not TRtcDataClient(Sender).RequestInserted and
       not TRtcDataClient(Sender).Request.Skipped and
       not TRtcDataClient(Sender).Response.Rejected then
      begin
      with TRtcDataClient(Sender) do
        begin
        MyCalls:=TRtcClientModuleCallsArray(Request.Info.Obj['CLIMOD.CALL$']);
        if not assigned(MyCalls) then
          raise Exception.Create('Internal error! ClientModule objects undefined!');

        HaveCleared:=0;
        for idx:=0 to MyCalls.Count-1 do
          if assigned(MyCalls.Event[idx]) then
            if assigned(MyCalls.Event[idx].PreparingCall) then
              if not MyCalls.isNull[idx] then
                if MyCalls.asObject[idx] is TRtcValue then
                  begin
                  val:=TRtcValue(MyCalls.asObject[idx]);
                  if AutoSyncEvents then
                    Sync(MyCalls.Event[idx].Call_Prepare,val,nil)
                  else
                    MyCalls.Event[idx].Call_Prepare(Sender,val,nil);
                  if val.isNull then
                    begin
                    MyCalls.Event[idx]:=nil;
                    Inc(HaveCleared);
                    end;
                  end;

        if HaveCleared=MyCalls.Count then
          begin
          Request.Skip;
          Exit;
          end;

        if FDataFormat=fmt_RTC then
          begin
          crypt:=GetCrypt(Sender);
          if assigned(crypt) then
            if Request.Query.ValueCS['ACTION']='RESET' then
              begin
              RtcFreeAndNil(crypt.CWrite);
              crypt.CWrite:=TRtcEncryptionClass.Create;
              crypt.CWrite.Key:= crypt.ServerKey + crypt.ClientHello;

              RtcFreeAndNil(crypt.CRead);
              crypt.CRead:=TRtcEncryptionClass.Create;
              crypt.CRead.Key:= crypt.ClientKey + crypt.ServerHello;

              crypt.ControlCounter:=0;
              end;
          Request.Info.asBoolean['ClientModule$']:=True;
          end
        else
          crypt:=nil;

        compressed:=False;

        {$IFDEF COMPRESS}
        if (FDataFormat=fmt_RTC) and (FCompress<>cNone) then
          begin
          if MyCalls.Count=1 then
            code:=MyCalls.asCode[0]
          else
            begin
            output:=TRtcHugeString.Create;
            try
              for idx:=0 to MyCalls.Count-1 do
                output.Add(MyCalls.asCode[idx]);
              code:= output.Get;
            finally
              RtcFreeAndNil(output);
              end;
            end;

          if length(code)<RTC_MIN_COMPRESS_SIZE then
            begin
            CryptWrite(crypt, code);
            Write(code);
            end
          else
            begin
            { Using compression,
              need to compress all data now. }
            codeEx:=RtcStringToBytes(code);
            SetLength(code,0);
            case FCompress of
              cFast: tempEx:=ZCompress_Ex(codeEx,zcFastest);
              cMax: tempEx:=ZCompress_Ex(codeEx,zcMax);
              else tempEx:=ZCompress_Ex(codeEx,zcDefault);
              end;
            // use compressed version ONLY if smaller than uncompressed
            if length(tempEx)<length(codeEx)-1 then
              begin
              SetLength(codeEx,0);
              CryptWriteEx(crypt, tempEx);
              WriteEx(tempEx);

              SetLength(tempEx,1);
              tempEx[0]:=0;
              CryptWriteEx(crypt, tempEx);
              WriteEx(tempEx);
              SetLength(tempEx,0);

              compressed:=True;
              end
            else
              begin
              SetLength(tempEx,0);
              CryptWriteEx(crypt, codeEx);
              WriteEx(codeEx);
              SetLength(codeEx,0);
              end;
            end;
          end
        else
        {$ENDIF}
          begin
          if FDataFormat=fmt_RTC then
            begin
            if not assigned(crypt) then
              begin
              for idx:=0 to MyCalls.Count-1 do
                begin
                code:=MyCalls.asCode[idx];
                Write(code);
                end;
              end
            else
              begin
              for idx:=0 to MyCalls.Count-1 do
                begin
                code:=MyCalls.asCode[idx];
                CryptWrite(crypt, code);
                Write(code);
                end;
              end;
            end
          else
            begin
            for idx:=0 to MyCalls.Count-1 do
              begin
              obj:=MyCalls.asObject[idx];
              if not assigned(obj) then
                raise Exception.Create('XML-RPC Error! Can not make a "NIL" call!')
              else
                code:=obj.toXMLrpcRequest;
              Write(code);
              end;
            end;
          end;

        if (FDataFormat=fmt_RTC) then
          begin
          if assigned(crypt) and assigned(crypt.CWrite) then
            begin
            { Add random control number at the end of the request,
              so we can check if the response is correctly encrypted. }
            crypt.ControlKey := GenerateControlKey(crypt.ControlCounter);

            if not compressed then
              code:=#13+crypt.ControlKey
            else // #0 was allready added
              code:=crypt.ControlKey; // we just need to add the control number

            CryptWrite(crypt, code);
            Write(code);
            end;
          end;

        SetLength(code,0);
        end;
      end;
    end;
  end;

function TRtcClientModule.GetObjectManager(xCreate:boolean=False): TRtcRemoteObjectManager;
  begin
  if not assigned(FObjectManager) and xCreate then
    begin
    FObjectManager:=TRtcClientObjectManager.Create(False);
    FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    FObjectManager.OnDataReady:=DoObjectManagerDataReady;
    end;
  Result:=FObjectManager;
  end;

procedure TRtcClientModule.ActivateObjectManager(xCreate:boolean=True);
  begin
  if FRelease then Exit;

  if ObjectLinks=ol_None then
    raise ERtcObjectLinks.Create('ActivateObjectManager: ObjectLinks = ol_None')
  else if not assigned(FObjectManager) then
    if xCreate then
      begin
      FObjectManager:=TRtcClientObjectManager.Create(False);
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
      FObjectManager.OnDataReady:=DoObjectManagerDataReady;
      end;

  if not assigned(FObjectManager) then
    raise ERtcObjectLinks.Create('ActivateObjectManager failed')
  else
    begin
    SetRtcObjectManager(FObjectManager);
    FObjectManager.ExecuteBroadcast(nil);
    end;
  end;

procedure TRtcClientModule.RemoveObjectManager;
  begin
  if assigned(FObjectManager) then
    begin
    FObjectManager.OnDataReady:=nil;
    if CheckRtcObjectManager=FObjectManager then
      SetRtcObjectManager(nil);
    RtcFreeAndNil(FObjectManager);
    end;
  end;

procedure TRtcClientModule.RemoveManagedObjects;
  begin
  if assigned(FObjectManager) then
    FObjectManager.FreeObjects;
  end;

procedure TRtcClientModule.DoExecute(Sender: TRtcDataClient; var MyData: TRtcValueObject);
  var
    MyResult,TmpData,TmpRes:TRtcValueObject;
    TmpMan:TRtcObjectManager;
    ObjMan:TRtcClientObjectManager;
    TmpV:TRtcValue;
    xData:TRtcValue;
  begin
  if FObjectLinkSupport=ol_None then
    begin
    if assigned(FFunctions) then
      begin
      MyResult:=FFunctions.ExecuteData(Sender, MyData);
      if MyData<>MyResult then
        begin
        RtcFreeAndNil(MyData);
        MyData:=MyResult;
        end;
      end;
    end
  else if (TRtcValue(MyData).isType=rtc_Function) and
          (CompareText(TRtcValue(MyData).asFunction.FunctionName, RTCL_FUNCTION)=0) then
    begin
    TmpData:=TRtcValue(MyData).asFunction.asObject[RTCL_DATA];
    MyResult:=TRtcValue(MyData).asFunction.asObject[RTCL_RESULT];
    if MyResult=nil then
      MyResult:=TRtcValue.Create;
    TRtcValue(MyData).asFunction.asObject[RTCL_RESULT]:=nil;
    try
      if assigned(TmpData) then
        begin
        ObjMan:=TRtcClientObjectManager(FObjectManager);
        if not assigned(ObjMan) then
          if FObjectLinkSupport in [ol_AutoServer,ol_AutoBoth] then
            begin
            FObjectManager:=TRtcClientObjectManager.Create(False);
            FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
            FObjectManager.OnDataReady:=DoObjectManagerDataReady;
            ObjMan:=FObjectManager;
            end
          else
            Exit;
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
          if assigned(TmpRes) then
            begin
            TmpV:=TRtcValue.Create;
            try
              TmpV.asObject:=TmpRes;
              Call_ObjectLinkErrors(Sender,TRtcValue(MyData),TmpV);
            finally
              TmpV.Free;
              end;
            end;
        finally
          SetRtcObjectManager(TmpMan);
          end;
        end;
    finally
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;

    if assigned(FFunctions) then
      if not isSimpleValue(MyData) then
        begin
        MyResult:=FFunctions.ExecuteData(Sender, MyData);
        if MyData<>MyResult then
          begin
          RtcFreeAndNil(MyData);
          MyData:=MyResult;
          end;
        end;
    end
  else if assigned(FFunctions) then
    begin
    MyResult:=FFunctions.ExecuteData(Sender, MyData);
    if MyData<>MyResult then
      begin
      RtcFreeAndNil(MyData);
      MyData:=MyResult;
      end;
    end;
  end;

procedure TRtcClientModule.Call_DataReceived(Sender: TRtcConnection);
  var
    idx:integer;
    code:RtcString;
    codeEx:RtcByteArray;
    at:integer;
    MyTemp:TRtcValue;
    crypt:TRtcCryptClient;
    c1,c2:RtcString;
    c3:integer;
    MyData:TRtcValueObject;
    MyCalls:TRtcClientModuleCallsArray;
  begin
  with TRtcDataClient(Sender) do if Response.Done then
    if AutoSyncEvents and not Sender.inMainThread then
      Sender.Sync(Call_DataReceived)
    else if Response.StatusCode=410 then // Status 410 = Gone: Session ID invalid, clear local Session info.
      begin
      Call_SessionExpired(Sender);
      end
    else if Response.StatusCode=412 then // Status 412 = Precondition Failed: Encryption required
      begin
      Call_NeedEncryption(Sender);
      end
    else if Response.StatusCode=409 then // Status 409 = Conflict: Wrong Encryption Key
      begin
      Call_WrongEncryption(Sender);
      end
    else if Response.StatusCode=417 then // Status 417 = Expectation Failed: Encryption Key initialization error
      begin
      Call_WrongEncryptionStart(Sender);
      end
    else if Response.StatusCode<>200 then // Accept only responses with status 200 OK.
      begin
      Call_WrongResponse(Sender);
      end
    else if (EncryptionKey>0) and
            (Request.Query.ValueCS['ACTION']='HELLO') then
      begin
      // Prepare Session
      if (Session.ID='') then
        begin
        if (Response.Cookie.ValueCS['ID']='') then
          begin
          if ForceEncryption then
            begin
            Call_WrongResponse(Sender);
            Exit;
            end
          else
            begin
            NewCrypt(Sender);
            crypt:=GetCrypt(Sender);
            end;
          end
        else
          begin
          crypt:=GetCrypt(Sender);
          c1:=crypt.ClientHello;
          c2:=crypt.ControlKey;
          c3:=crypt.ControlCounter;
          Session.Open(Response.Cookie.ValueCS['ID']); // Set new Session ID
          NewCrypt(Sender);
          crypt:=GetCrypt(Sender);
          crypt.ClientHello:=c1;
          crypt.ControlKey:=c2;
          crypt.ControlCounter:=c3;
          end;
        end
      else if (Response.Cookie.ValueCS['ID']<>'') and
              (Session.ID<>Response.Cookie.ValueCS['ID']) then
        begin
        crypt:=GetCrypt(Sender);
        c1:=crypt.ClientHello;
        c2:=crypt.ControlKey;
        c3:=crypt.ControlCounter;
        Session.Open(Response.Cookie.ValueCS['ID']); // Set new Session ID
        NewCrypt(Sender);
        crypt:=GetCrypt(Sender);
        crypt.ClientHello:=c1;
        crypt.ControlKey:=c2;
        crypt.ControlCounter:=c3;
        end
      else
        crypt:=GetCrypt(Sender);

      codeEx:=ReadEx;
      crypt.HaveHello:=True;

      if length(codeEx)=0 then // Server does not support encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init;
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryptionStart(Sender);
        Exit;
        end
      else
        begin
        // Prepare the Encryption object for Reading
        RtcFreeAndNil(crypt.CRead);
        crypt.CRead:=TRtcEncryptionClass.Create;
        crypt.CRead.Key:=crypt.ClientHello;

        // DeCrypt Server-Hello + Client-Hello
        CryptReadEx(crypt, codeEx);

        // Check if response ends with sent control key
        if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
            <> crypt.ControlKey then
          begin
          Call_WrongEncryptionStart(Sender);
          Exit;
          end;

        crypt.ServerHello:= RtcBytesToString(codeEx,0,length(codeEx)-length(crypt.ControlKey));

        // Prepare the Encryption object for Writing
        RtcFreeAndNil(crypt.CWrite);
        crypt.CWrite:=TRtcEncryptionClass.Create;
        crypt.CWrite.Key:=crypt.ServerHello;
        end;
      end
    else if (EncryptionKey>0) and
            (Request.Query.ValueCS['ACTION']='START') then
      begin
      crypt:=GetCrypt(Sender);

      codeEx:=ReadEx;
      crypt.HaveStart:=True;

      if length(codeEx)=0 then // Server canceled encryption
        begin
        if ForceEncryption then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else
          begin
          crypt.Init;
          crypt.HaveHello:=True;
          crypt.HaveStart:=True;
          end;
        end
      else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
        begin
        Call_WrongEncryptionStart(Sender);
        Exit;
        end
      else
        begin
        // Set new Reading Key
        crypt.CRead.Key:= crypt.ClientKey + crypt.ServerHello;

        // DeCrypt response: Server-Key + Client-Key
        CryptReadEx(crypt, codeEx);

        // Check if response ends with sent Control Key
        if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
            <> crypt.ControlKey then
          begin
          Call_WrongEncryptionStart(Sender);
          Exit;
          end;

        crypt.ServerKey:= RtcBytesToString(codeEx, 0, length(codeEx)-length(crypt.ControlKey));

        // Set new Writing Key
        crypt.CWrite.Key:= crypt.ServerKey + crypt.ClientHello;

        crypt.AllReady:=True;
        end;
      end
    else
      begin
      if Response.Cookie.ValueCS['ID']<>'' then
        if Response.Cookie.ValueCS['ID']='-'+Request.Query.ValueCS['ID'] then
          Session.Close // Received "Session Closing" info, close local session
        else if Request.Query.ValueCS['ID']='NEW' then // we have requested a new Session ID
          Session.Open(Response.Cookie.ValueCS['ID'])
        else if Session.ID<>Response.Cookie.ValueCS['ID'] then // we have received a different Session ID
          Session.Open(Response.Cookie.ValueCS['ID']);

      MyCalls:=TRtcClientModuleCallsArray(Request.Info.Obj['CLIMOD.CALL$']);
      if not assigned(MyCalls) then
        raise Exception.Create('Internal error! ClientModule objects undefined!');

      if FDataFormat=fmt_RTC then
        begin
        codeEx:=ReadEx;

        crypt:=GetCrypt(Sender);
        if assigned(crypt) and assigned(crypt.CRead) then
          begin
          if length(codeEx)=0 then
            begin
            Call_WrongEncryption(Sender);
            Exit;
            end
          else if length(codeEx)<=length(crypt.ControlKey) then // Wrong response from server
            begin
            Call_WrongEncryption(Sender);
            Exit;
            end
          else
            begin
            // Response has to END with our ControlKey
            CryptReadEx(crypt, codeEx);
            if RtcBytesToString(codeEx, length(codeEx)-length(crypt.ControlKey), length(crypt.ControlKey))
                <> crypt.ControlKey then
              begin
              Call_WrongEncryption(Sender);
              Exit;
              end
            else
              begin
              {$IFDEF COMPRESS}
              // There is #0 before the Control Key, data is compressed
              if codeEx[length(codeEx)-length(crypt.ControlKey)-1]=0 then
                begin
                try
                  codeEx:=ZDecompress_Ex(codeEx, length(codeEx)-length(crypt.ControlKey)-1);
                except
                  on E:Exception do
                    begin
                    if LOG_CLIENTMODULE_ERRORS then
                      Log('TRtcClientModule.DataReceived DECOMPRESS-1',E,'ERROR');
                    Call_WrongResponse(Sender);
                    Exit;
                    end;
                  end;
                end
              else
                SetLength(codeEx, length(codeEx)-length(crypt.ControlKey));
              {$ELSE}
              // There is #0 before the Control Key, data is compressed
              if codeEx[length(codeEx)-length(crypt.ControlKey)-1]=0 then
                begin
                Call_WrongResponse(Sender);
                Exit;
                end
              else
                SetLength(codeEx, length(codeEx)-length(crypt.ControlKey));
              {$ENDIF}
              end;
            end;
          end
        else if ForceEncryption and (EncryptionKey>0) then
          begin
          Call_NoEncryption(Sender);
          Exit;
          end
        else if codeEx[length(codeEx)-1]=0 then // compressed data
          begin
          {$IFDEF COMPRESS}
          try
            codeEx:=ZDecompress_Ex(codeEx, length(codeEx)-1);
          except
            on E:Exception do
              begin
              if LOG_CLIENTMODULE_ERRORS then
                Log('TRtcClientModule.DataReceived DECOMPRESS-2',E,'ERROR');
              Call_WrongResponse(Sender);
              Exit;
              end;
            end;
          {$ELSE}
          Call_WrongResponse(Sender);
          Exit;
          {$ENDIF}
          end;
        code:=RtcBytesToString(codeEx);
        SetLength(codeEx,0);
        end
      else
        code:=Read;

      idx:=0;
      at:=0;
      MyData:=nil;
      try
        if length(code)=0 then
          raise Exception.Create('No data received.')
        else
          begin
          // Loop through all responses
          while at<length(code) do
            begin
            // Convert result to RTC objects
            case FDataFormat of
              fmt_XMLRPC: MyData:=TRtcValue.FromXMLrpc(code,at);
              else        MyData:=TRtcValue.FromCode(code,at);
              end;
            try
              // Execute local remote functions if result contains them
              if not isSimpleValue(MyData) then
                DoExecute(TRtcDataClient(Sender),MyData);

              if assigned(MyData) then
                begin
                if idx<MyCalls.Count then
                  begin
                  if assigned(MyCalls.Event[idx]) then
                    begin
                    if not (MyData is TRtcValue) then
                      begin
                      MyTemp:=TRtcValue.Create;
                      MyTemp.asObject:=MyData;
                      MyData:=MyTemp;
                      end;
                    try
                      MyCalls.Event[idx].
                          Call_Return(Sender,
                                      TRtcValue(MyCalls.asObject[idx]),
                                      TRtcValue(MyData));
                    except
                      on E:EPostInteractive do
                        begin
                        PostInteractiveResult(MyCalls.Event[idx],
                                              TRtcValue(MyCalls.asObject[idx]),
                                              TRtcValue(MyData));
                        MyCalls.Event[idx]:=nil;
                        MyCalls.asObject[idx]:=nil;
                        MyData:=nil;
                        end;
                      end;
                    end;
                  end
                else
                  raise Exception.Create('More Results received than Calls sent.');
                end
              else
                raise Exception.Create('Response missing a result.');
            except
              on E:Exception do
                begin
                try
                  if LOG_CLIENTMODULE_ERRORS then
                    Log('TRtcClientModule.DataReceived RESULT',E,'ERROR');
                  Call_ResultError(Sender,
                                   TRtcValue(MyCalls.asObject[idx]),
                                   TRtcValue(MyData), E);
                except
                  // ignore user exceptions
                  end;
                end;
              end;

            Inc(idx);
            if assigned(MyData) then
              begin
              try
                RtcFreeAndNil(MyData);
              except
                on E:Exception do
                  if LOG_CLIENTMODULE_ERRORS then
                    Log('TRtcClientModule.DataReceived FREE ResultData',E,'ERROR');
                end;
              MyData:=nil;
              end;

            end; // while at<length(code);

          SetLength(code,0);

          // All Results processed, clear "Call Data" ...
          for idx:=0 to MyCalls.Count-1 do
            begin
            MyData:=MyCalls.asObject[idx];
            MyCalls.Event[idx]:=nil;
            MyCalls.asObject[idx]:=nil;
            if assigned(MyData) then
              begin
              try
                RtcFreeAndNil(MyData);
              except
                on E:Exception do
                  if LOG_CLIENTMODULE_ERRORS then
                    Log('TRtcClientModule.DataReceived FREE Call Data',E,'ERROR');
                end;
              MyData:=nil;
              end;
            end;

          end; // Code<>''
      except
        on E:Exception do
          begin
          if LOG_CLIENTMODULE_ERRORS then
            Log('TRtcClientModule.DataReceived EXCEPTION',E,'ERROR');
          Response.StatusCode:=0; // Internal exception
          Response.StatusText:=RtcString(E.Message);
          Call_WrongResponse(Sender);
          end;
        end;
      end;
  end;

procedure TRtcClientModule.SetCompress(const Value: TRtcCompressLevel);
  begin
  FCompress := Value;
  end;

procedure TRtcClientModule.Call_DataOut(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataIn(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_DataSent(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ReadyToSend(Sender: TRtcConnection);
  begin
  // empty
  end;

procedure TRtcClientModule.Call_ResponseData(Sender: TRtcConnection);
  begin
  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseData(Sender)
    else if assigned(Client) then
      Client.CallResponseData;
  end;

procedure TRtcClientModule.Call_ResponseDone(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseDone) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseDone)
    else
      FOnResponseDone(Sender);

  if not TRtcDataClient(Sender).Request.Skipped and
     not TRtcDataClient(Sender).Response.Rejected then
    if assigned(Link) then
      Link.Call_ResponseDone(Sender)
    else if assigned(Client) then
      Client.CallResponseDone;
  end;

procedure TRtcClientModule.Call_RepostCheck(Sender: TRtcConnection);
  begin
  if ((AutoRepost<0) or (TRtcDataClient(Sender).Request.Reposted<AutoRepost)) then
    TRtcDataClient(Sender).Request.Repost;

  if not TRtcDataClient(Sender).Request.Reposting then
    begin
    if assigned(FOnRepostCheck) then
      FOnRepostCheck(Sender);

    if not TRtcDataClient(Sender).Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_RepostCheck(Sender)
      else if assigned(Client) then
        Client.CallRepostCheck;
      end;
    end;

  with TRtcDataClient(Sender) do
    if Request.Info.asBoolean['ClientModule$'] then
      if IsRemoteCallRequest(Sender) then
        ResetCrypt(Sender) // encryption most likely broken
      else
        DelCrypt(Sender);
  end;

procedure TRtcClientModule.Call_ResponseAbort(Sender: TRtcConnection);
  begin
  if IsRemoteCallRequest(Sender) then
    begin
    if assigned(FOnResponseAbort) then
      if AutoSyncEvents then
        Sender.Sync(FOnResponseAbort)
      else
        FOnResponseAbort(Sender);

    if not TRtcDataClient(Sender).Request.Reposting then
      begin
      if assigned(Link) then
        Link.Call_ResponseAbort(Sender)
      else if assigned(Client) then
        Client.CallResponseAbort;

      if not TRtcDataClient(Sender).Request.Reposting then
        if AutoSyncEvents then
          Sender.Sync(NotifyResultAborted)
        else
          NotifyResultAborted(Sender);
      end;
    end
  else
    begin
    if assigned(Link) then
      Link.Call_ResponseAbort(Sender)
    else if assigned(Client) then
      Client.CallResponseAbort;
    end;
  end;

procedure TRtcClientModule.Call_ResponseReject(Sender: TRtcConnection);
  begin
  if assigned(FOnResponseReject) then
    if AutoSyncEvents then
      Sender.Sync(FOnResponseReject)
    else
      FOnResponseReject(Sender);

  if assigned(Link) then
    Link.Call_ResponseReject(Sender)
  else if assigned(Client) then
    Client.CallResponseReject;
  end;

procedure TRtcClientModule.Call_SessionClose(Sender: TRtcConnection);
  begin
  if assigned(FPingTimer) then
    begin
    TRtcTimer.Stop(FPingTimer);
    FPingTimer:=nil;
    end;

  if FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth] then
    begin
    if AutoSyncEvents then
      Sender.Sync(DoReactivateObjectManager)
    else
      begin
      RemoveObjectManager;
      ActivateObjectManager(True);
      end;
    end
  else if FObjectLinkSupport<>ol_None then
    begin
    if AutoSyncEvents then
      Sender.Sync(DoDeactivateObjectManager)
    else
      RemoveObjectManager;
    end;

  if assigned(FOnSessionClose) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionClose)
    else
      FOnSessionClose(Sender);

  if assigned(Link) then
    Link.Call_SessionClose(Sender)
  else if assigned(Client) then
    Client.CallSessionClose;
  end;

procedure TRtcClientModule.Call_SessionOpen(Sender: TRtcConnection);
  begin
  if assigned(FOnSessionOpen) then
    if AutoSyncEvents then
      Sender.Sync(FOnSessionOpen)
    else
      FOnSessionOpen(Sender);

  if assigned(Link) then
    Link.Call_SessionOpen(Sender)
  else if assigned(Client) then
    Client.CallSessionOpen;

  if FAutoSessionsPing>0 then
    begin
    if not assigned(FPingTimer) then
      if assigned(FOnPing) and AutoSyncEvents then
        FPingTimer:=TRtcTimer.Create(False)
      else if assigned(Sender) then
        FPingTimer:=TRtcTimer.Create(Sender.MultiThreaded)
      else if assigned(Client) then
        FPingTimer:=TRtcTimer.Create(Client.MultiThreaded)
      else if assigned(Link) then
        FPingTimer:=TRtcTimer.Create(Link.isMultiThreaded)
      else
        Exit; // No connection!??
    TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True);
    end
  else if assigned(FPingTimer) then
    begin
    TRtcTimer.Stop(FPingTimer);
    FPingTimer:=nil;
    end;
  end;

procedure TRtcClientModule.Call(ResultHandler: TRtcResult; FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    idx:integer;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('No data defined. Use the Data property before "Call".');
  if myData.FData=nil then
    raise Exception.Create('No data defined. Use the Data property before "Call".');
  if myData.FData.isNull then
    raise Exception.Create('No data defined. Use the Data property before "Call".');

  {if not assigned(ResultHandler) then
    raise Exception.Create('Can not use "Call" with NIL as parameter.');
  if not assigned(ResultHandler.OnReturn) then
    raise Exception.Create('OnReturn event undefined for given TRtcResult component.'); }

  with myData do
    begin
    if not assigned(FCalls) then
      FCalls:=TRtcClientModuleCallsArray.Create;

    // Add Data and ResultHandler to our list of calls
    idx:=FCalls.Count;
    FCalls.asObject[idx]:=FData;
    FCalls.Event[idx]:=ResultHandler;
    // set to NIL
    FData:=nil;

    if FPostLevel=0 then
      begin
      Inc(FPostLevel);
      Post(FromInsideEvent, Sender);
      end;
    end;
  end;

procedure TRtcClientModule.LoginCall(ResultHandler: TRtcResult; Sender:TRtcConnection; Insert:boolean=False);
  var
    DataReq:TRtcDataRequestInfo;
    FCalls:TRtcClientModuleCallsArray;
    FRequest:TRtcClientRequest;
    CallData:TRtcValue;
  begin
  CallData:=TRtcValue.Create;
  try
    if AutoSyncEvents then
      Sender.Sync(FOnLogin, CallData)
    else
      FOnLogin(Sender, CallData);
  except
    RtcFreeAndNil(CallData);
    raise;
    end;
  // Create the "Calls" object with our remote function call
  FCalls:=TRtcClientModuleCallsArray.Create;
  FCalls.asObject[0]:=CallData;
  FCalls.Event[0]:=ResultHandler;

  // Create a new "Request" object
  FRequest:=TRtcClientRequest.Create;
  if ModuleFileName<>'' then
    FRequest.FileName:=ModuleFileName;
  if ModuleHost<>'' then
    FRequest.Host:=ModuleHost;
  FRequest.Method:='POST';

  if FDataFormat=fmt_XMLRPC then // need to send more info in header
    begin
    if FRequest.Host='' then
      FRequest.Host:=Sender.ServerAddr;
    if FRequest.Agent='' then
      FRequest.Agent:='RTC Client';
    FRequest.ContentType:='text/xml';
    end;

  // Assign our "Calls" object to the Request object, so we can access it after we post it.
  FRequest.Info.Obj['CLIMOD.CALL$']:=FCalls;
  FRequest.Info.asBoolean['CLIMOD.LOGIN$']:=True;

  // Create a "DataRequest" object and store the "Request" object into it
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=FRequest;
  DataReq.Events:=Self;

  // Insert the Request
  if Insert then
    TRtcDataClient(Sender).InsertRequest(DataReq)
  else
    TRtcDataClient(Sender).PostRequest(DataReq,True);
  end;

procedure TRtcClientModule.PingCall(ResultHandler: TRtcResult; Sender:TRtcConnection);
  var
    DataReq:TRtcDataRequestInfo;
    FCalls:TRtcClientModuleCallsArray;
    FRequest:TRtcClientRequest;
    CallData:TRtcValue;
  begin
  CallData:=TRtcValue.Create;
  try
    if assigned(FOnPing) then
      FOnPing(Sender, CallData);
  except
    RtcFreeAndNil(CallData);
    raise;
    end;
  // Create the "Calls" object with our remote function call
  FCalls:=TRtcClientModuleCallsArray.Create;
  FCalls.asObject[0]:=CallData;
  FCalls.Event[0]:=ResultHandler;

  // Create a new "Request" object
  FRequest:=TRtcClientRequest.Create;
  if ModuleFileName<>'' then
    FRequest.FileName:=ModuleFileName;
  if ModuleHost<>'' then
    FRequest.Host:=ModuleHost;
  FRequest.Method:='POST';

  if FDataFormat=fmt_XMLRPC then // need to send more info in header
    begin
    if FRequest.Host='' then
      FRequest.Host:=Sender.ServerAddr;
    if FRequest.Agent='' then
      FRequest.Agent:='RTC Client';
    FRequest.ContentType:='text/xml';
    end;

  // Assign our "Calls" object to the Request object, so we can access it after we post it.
  FRequest.Info.Obj['CLIMOD.CALL$']:=FCalls;

  // Create a "DataRequest" object and store the "Request" object into it
  DataReq:=TRtcDataRequestInfo.Create;
  DataReq.Request:=FRequest;
  DataReq.Events:=Self;

  // Post the Request
  if assigned(Sender) and (Sender is TRtcDataClient) then
    TRtcDataClient(Sender).PostRequest(DataReq,False)
  else
    PostRequest(DataReq,False);
  end;

procedure TRtcClientModule.StartCalls;
  begin
  with GetMyData do
    begin
    Inc(FPostLevel);
    if assigned(FData) then FData.Clear;
    end;
  end;

procedure TRtcClientModule.ClearMyData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if FHyperThreading then
    begin
    if not InsideMainThread then
      begin
      id:=GetMyThreadId;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          begin
          FMyData.remove(id);
          RtcFreeAndNil(obj);
          end;
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcClientModule.CheckMyData: TRtcClientModuleData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    if InsideMainThread then
      Result:=FMainThrData
    else
      begin
      id:=GetMyThreadId;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj<>nil then
          Result:=TRtcClientModuleData(obj)
        else
          Result:=nil;
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcClientModule.GetMyData: TRtcClientModuleData;
  var
    id:RtcThrID;
    obj:TObject;
  begin
  if not FHyperThreading then
    Result:=FMainThrData
  else
    begin
    if InsideMainThread then
      Result:=FMainThrData
    else
      begin
      id:=GetMyThreadId;
      FCS.Acquire;
      try
        obj:=FMyData.search(id);
        if obj=nil then
          begin
          obj:=TRtcClientModuleData.Create;
          FMyData.insert(id, obj);
          end;
        Result:=TRtcClientModuleData(obj);
      finally
        FCS.Release;
        end;
      end;
    end;
  end;

function TRtcClientModule.Get_Data: TRtcValue;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FData) then
    myData.FData:=TRtcValue.Create;
  Result:=myData.FData;
  end;

function TRtcClientModule.GetPostLevel: integer;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  Result:=myData.FPostLevel;
  end;

function TRtcClientModule.GetRequest: TRtcClientRequest;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=GetMyData;
  if not assigned(myData.FRequest) then
    myData.FRequest:=TRtcClientRequest.Create;
  Result:=myData.FRequest;
  end;

procedure TRtcClientModule.Post(FromInsideEvent:boolean=False; Sender:TRtcConnection=nil);
  var
    DataReq:TRtcDataRequestInfo;
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('Have to use "StartCalls" before "Post"'#13#10+
                           'to post multiple calls in one request.');

  if myData.FPostLevel<=0 then
    raise Exception.Create('Have to use "StartCalls" before "Post"'#13#10+
                           'to post multiple calls in one request.');

  with myData do
    begin
    Dec(FPostLevel);
    if FPostLevel>0 then Exit;

    if assigned(FCalls) then
      begin
      if not assigned(FRequest) then
        FRequest:=TRtcClientRequest.Create;

      if ModuleFileName<>'' then
        FRequest.FileName:=ModuleFileName;
      if FRequest.FileName='' then
        raise Exception.Create('Module FileName is undefined. Can not Post the request.');
      if ModuleHost<>'' then
        FRequest.Host:=ModuleHost;

      FRequest.Method:='POST';

      if FDataFormat=fmt_XMLRPC then // need to send more info in header
        begin
        if (FRequest.Host='') and assigned(Sender) then
          FRequest.Host:=Sender.ServerAddr;
        if FRequest.Agent='' then
          FRequest.Agent:='RTC Client';
        FRequest.ContentType:='text/xml';
        end;

      // Assign our Calls to the Request object, so we can access it after we post it.
      FRequest.Info.Obj['CLIMOD.CALL$']:=FCalls;
      FCalls:=nil;

      DataReq:=TRtcDataRequestInfo.Create;
      DataReq.Request:=FRequest;
      DataReq.Events:=Self;
      FRequest:=nil;

      if assigned(Sender) and (Sender is TRtcDataClient) then
        TRtcDataClient(Sender).PostRequest(DataReq,FromInsideEvent)
      else
        PostRequest(DataReq,FromInsideEvent);
      end;
    end;

  // Free ClientModuleData and remove it from the list
  ClearMyData;
  end;

procedure TRtcClientModule.CancelCalls;
  var
    myData:TRtcClientModuleData;
  begin
  myData:=CheckMyData;

  if myData=nil then
    raise Exception.Create('"CancelCalls" can only be used in pair with "StartCalls"');


  with myData do
    begin
    if FPostLevel<=0 then
      raise Exception.Create('"CancelCalls" can only be used in pair with "StartCalls"');

    FPostLevel:=0;

    if assigned(FCalls) then
      begin
      FCalls.Free;
      FCalls:=nil;
      end;
    end;

  ClearMyData;
  end;

function TRtcClientModule.GetFunctionGroup: TRtcFunctionGroup;
  begin
  try
    Result:=FFunctions;
    if not (Result is TRtcFunctionGroup) then
      Result:=nil;
  except
    Result:=nil;
    end;
  end;

procedure TRtcClientModule.SetFunctionGroup(const Value: TRtcFunctionGroup);
  begin
  FFunctions:=Value;
  end;

function TRtcClientModule.GetModuleFileName: RtcString;
  begin
  Result:=FModuleFileName;
  end;

procedure TRtcClientModule.SetModuleFileName(const Value: RtcString);
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
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CLI-CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_CLISESSION;
    if assigned(FObjectManager) then
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    end;
  end;

function TRtcClientModule.GetModuleHost: RtcString;
  begin
  Result:=FModuleHost;
  end;

procedure TRtcClientModule.SetModuleHost(const Value: RtcString);
  begin
  if FModuleHost<>Value then
    begin
    // Convert to uppercase now, so we don't have to do it on every request.
    FModuleHost:=Upper_Case(Value);
    FCryptSesName:=FModuleHost+FModuleFileName+'.$CLI-CRYPT$';
    FObjManSesName:=FModuleHost+FModuleFileName+RTCL_CLISESSION;
    if assigned(FObjectManager) then
      FObjectManager.BroadcastGroup:=RtcWideString(FObjManSesName);
    end;
  end;

procedure TRtcClientModule.Response_Problem(Sender: TRtcConnection);
  begin
  with TRtcDataClient(Sender) do
    begin
    if not Request.Reposting and not Response.Rejected then
      begin
      Call_RepostCheck(Sender);
      if not Request.Reposting and not Response.Rejected then
        Call_ResponseAbort(Sender);
      end;
    end;
  end;

procedure TRtcClientModule.Call_SessionExpired(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnSessionExpired) then
    FOnSessionExpired(Sender);
  with TRtcDataClient(Sender) do
    begin
    Session.Init;
    Request.Query.ValueCS['ID']:='';
    if not Request.Reposting and not Response.Rejected then
      if Request.Reposted<1 then // if Session expires, we will try to repost 1 time ...
        Request.Repost
      else // ... and leave all other decisions to the user
        Response_Problem(Sender);
    end;
  end;

procedure TRtcClientModule.Call_WrongResponse(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnResponseError) then
    FOnResponseError(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_ResultError(Sender: TRtcConnection; Data,Result:TRtcValue; E:Exception);
  begin
  if assigned(FOnResultError) then
    FOnResultError(Sender,Data,Result,E);
  end;

procedure TRtcClientModule.Call_WrongEncryption(Sender: TRtcConnection);
  begin
  ResetCrypt(Sender);
  if assigned(FOnWrongEncryption) then
    FOnWrongEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_WrongEncryptionStart(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnWrongEncryption) then
    FOnWrongEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NoEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnNoEncryption) then
    FOnNoEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.Call_NeedEncryption(Sender: TRtcConnection);
  begin
  DelCrypt(Sender);
  if assigned(FOnNeedEncryption) then
    FOnNeedEncryption(Sender);
  Response_Problem(Sender);
  end;

procedure TRtcClientModule.SetAutoEncrypt(const Value: integer);
  begin
  if Value<0 then
    raise Exception.Create('Negative values not allowed for EncryptionKey.');
  FAutoEncrypt := Value;
  if FAutoEncrypt > 0 then
    FAutoSessions:=True
  else
    FForceEncrypt:=False;
  end;

procedure TRtcClientModule.SetAutoSessions(const Value: boolean);
  begin
  FAutoSessions := Value;
  if not FAutoSessions then
    begin
    FAutoEncrypt:=0;
    FAutoLogin:=False;
    FForceEncrypt:=False;
    end;
  end;

procedure TRtcClientModule.SetForceEncrypt(const Value: boolean);
  begin
  FForceEncrypt := Value;
  if FForceEncrypt then
    begin
    FAutoSessions:=True;
    if FAutoEncrypt=0 then
      FAutoEncrypt:=16;
    end;
  end;

procedure TRtcClientModule.SetObjectLinkSupport(const Value: TRtcObjectLinkSupport);
  begin
  if (Value<>FObjectLinkSupport) then
    begin
    if ActiveGlobalManager and (FObjectLinkSupport in [ol_AutoClient,ol_AutoBoth]) then
      begin
      ActiveGlobalManager:=False;
      RemoveObjectManager;
      FObjectLinkSupport:=ol_None;
      end;
    if Value in [ol_AutoClient,ol_AutoBoth] then
      begin
      if ActiveGlobalManager then
        raise ERtcObjectLinks.Create('Only one TRtcClientModule with ObjectLinks=ol_AutoClient/ol_AutoBoth allowed per Application.');
      FObjectLinkSupport := Value;
      if not (csDesigning in ComponentState) then
        begin
        ActiveGlobalManager:=True;
        ActivateObjectManager(True);
        end;
      end
    else
      FObjectLinkSupport := Value;
    end;
  end;

procedure TRtcClientModule.PostInteractiveResult(Event: TRtcResult; Data, Result: TRtcValue);
  var
    res:TRtcInteractiveResult;
  begin
  FIntCS.Acquire;
  try
    res:=TRtcInteractiveResult.Create;
    res.FEvent:=Event;
    res.Data:=Data;
    res.Result:=Result;

    FIntRes.AddLast(res);

    if not assigned(FIntTimer) then
      begin
      FIntTimer:=TRtcTimer.Create(False);
      TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True);
      end;
  finally
    FIntCS.Release;
    end;
  end;

procedure TRtcClientModule.DoInteractiveResult;
  var
    res:TRtcInteractiveResult;
  begin
  FIntCS.Acquire;
  try
    res:=TRtcInteractiveResult(FIntRes.First);
    FIntRes.RemoveFirst;
  finally
    FIntCS.Release;
    end;

  try
    res.FEvent.Call_Return(nil, res.Data, res.Result);
  finally
    RtcFreeAndNil(res);

    FIntCS.Acquire;
    try
      if FIntRes.Count>0 then
        TRtcTimer.Enable(FIntTimer,1,DoInteractiveResult,True)
      else
        begin
        TRtcTimer.Stop(FIntTimer);
        FIntTimer:=nil;
        end;
    finally
      FIntCS.Release;
      end;
    end;
  {$IFNDEF NEXTGEN} if FRelease then Free; {$ENDIF}
  end;

procedure TRtcClientModule.Release;
  begin
  FRelease:=True;
  end;

procedure TRtcClientModule.NotifyResultAborted(Sender: TRtcConnection);
  var
    MyCalls:TRtcClientModuleCallsArray;
    event:TRtcResult;
    data:TRtcValue;
    a:integer;
  begin
  MyCalls:=TRtcClientModuleCallsArray(TRtcDataClient(Sender).Request.Info.Obj['CLIMOD.CALL$']);
  if assigned(MyCalls) then
    begin
    for a:=0 to MyCalls.Count-1 do
      begin
      event:=MyCalls.Event[a];
      if assigned(event) then
        begin
        data:=TRtcValue(MyCalls.AsObject[a]);
        event.Call_Aborted(Sender,data,nil);
        end;
      end;
    end;
  end;

function TRtcClientModule.IsRemoteCallRequest(Sender:TRtcConnection): boolean;
  begin
  Result := assigned(TRtcDataClient(Sender).Request.Info.Obj['CLIMOD.CALL$']);
  end;

procedure TRtcClientModule.SetDataFormat(const Value: TRtcDataFormat);
  begin
  FDataFormat := Value;
  end;

procedure TRtcClientModule.SetAutoLogin(const Value: boolean);
  begin
  FAutoLogin := Value;
  if FAutoLogin then
    FAutoSessions:=True;
  end;

procedure TRtcClientModule.Call_LoginAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginAborted) then
    FOnLoginAborted(Sender,Data,Result);
  end;

procedure TRtcClientModule.Call_LoginResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnLoginResult) then
    FOnLoginResult(Sender,Data,Result);

  if Result.isType<>rtc_Exception then
    TRtcDataClient(Sender).Session.asBoolean['CLIMOD.LOGIN$']:=True;
  end;

procedure TRtcClientModule.Call_PingAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnPingAborted) then
    FOnPingAborted(Sender,Data,Result);
  if not TRtcDataClient(Sender).Request.Reposting then
    begin
    RemoveObjectManager;
    if assigned(TRtcDataClient(Sender).Session) then
      TRtcDataClient(Sender).Session.Close;
    end;
  end;

procedure TRtcClientModule.Call_PingResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnPingResult) then
    FOnPingResult(Sender,Data,Result);

  if assigned(FPingTimer) then
    if FAutoSessionsPing>0 then
      TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True)
    else
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;
  end;

procedure TRtcClientModule.ResetLogin;
  begin
  FResetLogin:=True;
  end;

function TRtcClientModule.Prepare(const FunctionName: RtcWideString): TRtcFunctionInfo;
  begin
  Data.Clear;
  Result:=Data.NewFunction(FunctionName);
  end;

function TRtcClientModule.GetLastCallParams: TRtcFunctionInfo;
  begin
  if Data.isType=rtc_Function then
    Result:=Data.asFunction
  else
    raise Exception.Create('Use Prepare() or Data.newFunction() to prepare a function object first.');
  end;

function TRtcClientModule.Execute(AutoFreeResult:boolean; _Timeout:cardinal; AllowMessageProcessing:boolean): TRtcValue;
  begin
  RtcFreeAndNil(FExecuteResult);
  Call(FExecuteHandler);
  case DoWaitForCompletion(False,_Timeout,AllowMessageProcessing) of
    wait_OK:
      begin
      if FExecuteResult=nil then
        begin
        CancelRequests;
        raise ERtcExecuteError.Create('Error: Connection problems, no response received.');
        end;
      Result:=FExecuteResult;
      if not AutoFreeResult then
        FExecuteResult:=nil; // set to NIL only if we do NOT want to Free the object!
      end;
    wait_Timeout:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Connection problems, response timed out.');
      end;
    wait_Quit:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Application Terminating.');
      end;
    wait_Msg:
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Unknown message received.');
      end;
    else
      begin
      CancelRequests;
      raise ERtcExecuteError.Create('Error: Connection problems.');
      end;
    end;
  end;

procedure TRtcClientModule.Call_ExecuteResult(Sender: TRtcConnection; Data,Result: TRtcValue);
  begin
  FExecuteResult:=TRtcValue.Create;
  FExecuteResult.asObject:=Result.asObject;
  Result.asObject:=nil;
  end;

procedure TRtcClientModule.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
    if AComponent=FFunctions then
      SetFunctionGroup(nil);
  end;

procedure TRtcClientModule.DoObjectManagerDataReady(Sender: TRtcRemoteObjectManager);
  begin
  if not FObjectLinksOut then
    DoObjectManagerSend(Sender,nil);
  end;

procedure TRtcClientModule.DoObjectManagerSend(Sender: TRtcRemoteObjectManager; Conn:TRtcConnection);
  var
    MyData:TRtcValue;
  begin
  if not Sender.Idle then
    begin
    Sender.ExecuteBroadcast(Conn);
    MyData:=Sender.GetData;
    if assigned(MyData) then
      begin
      FObjectLinksOut:=True;
      if assigned(FOnObjectDataOut) then
        FOnObjectDataOut(Conn,MyData);
      Data.NewFunction(RTCL_FUNCTION).asObject[RTCL_DATA]:=MyData;
      Call(FObjectLinkResult,assigned(Conn),Conn);
      end
    else
      FObjectLinksOut:=False;
    end
  else
    FObjectLinksOut:=False;
  end;

procedure TRtcClientModule.Call_ObjectLinkAborted(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  FObjectLinksOut:=False;
  if assigned(FOnObjectLinkAborted) then
    FOnObjectLinkAborted(Sender,Data,Result);
  if not TRtcDataClient(Sender).Request.Reposting then
    begin
    RemoveObjectManager;
    if assigned(TRtcDataClient(Sender).Session) then
      TRtcDataClient(Sender).Session.Close;
    end;
  end;

procedure TRtcClientModule.Call_ObjectLinkResult(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if Result.isType<>rtc_Null then
    Call_ObjectLinkErrors(Sender,Data,Result);
  DoObjectManagerSend(FObjectManager,Sender);
  end;

procedure TRtcClientModule.Call_ObjectLinkErrors(Sender: TRtcConnection; Data, Result: TRtcValue);
  begin
  if assigned(FOnObjectLinkErrors) then
    FOnObjectLinkErrors(Sender,Data,Result);
  end;

procedure TRtcClientModule.DoObjectCreate(Sender: TObject; Param: TRtcObjectCall);
  begin
  if assigned(FOnObjectCreate) then
    FOnObjectCreate(TRtcConnection(Sender),Param);
  end;

procedure TRtcClientModule.SetAutoSessionsPing(const Value: integer);
  begin
  FAutoSessionsPing := Value;
  if FAutoSessionsPing<=0 then
    begin
    if assigned(FPingTimer) then
      begin
      TRtcTimer.Stop(FPingTimer);
      FPingTimer:=nil;
      end;
    end
  else if assigned(FPingTimer) then
    TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True);
  end;

procedure TRtcClientModule.DoSessionPing;
  var
    i:boolean;
  begin
  if Assigned(Client) then
    i:=Client.isIdle
  else if assigned(Link) then
    i:=Link.isIdle
  else
    i:=False;
  if i then
    PingCall(FPingResult,nil)
  else if assigned(FPingTimer) and (FAutoSessionsPing>0) then
    TRtcTimer.Enable(FPingTimer,FAutoSessionsPing*1000,DoSessionPing,True);
  end;

procedure TRtcClientModule.DoReactivateObjectManager(Sender: TRtcConnection);
  begin
  RemoveObjectManager;
  ActivateObjectManager(True);
  end;

procedure TRtcClientModule.DoDeactivateObjectManager(Sender: TRtcConnection);
  begin
  RemoveObjectManager;
  end;

{ TRtcInteractiveResult }

destructor TRtcInteractiveResult.Destroy;
  begin
  try
    RtcFreeAndNil(Data);
    RtcFreeAndNil(Result);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcInteractiveResult.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

{ TRtcClientModuleCallsArray }

constructor TRtcClientModuleCallsArray.Create;
  begin
  inherited;
  SetLength(FEvents,0);
  end;

destructor TRtcClientModuleCallsArray.Destroy;
  begin
  try
    SetLength(FEvents,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcClientModuleCallsArray.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcClientModuleCallsArray.GetEvent(index: integer): TRtcResult;
  begin
  if (index>=0) and (index<=length(FEvents)) then
    Result:=FEvents[index]
  else
    Result:=nil;
  end;

procedure TRtcClientModuleCallsArray.SetEvent(index: integer; const _Value: TRtcResult);
  begin
  if length(FEvents)<index+1 then
    SetLength(FEvents, index+1);
  FEvents[index]:=_Value;
  end;

{ TRtcCryptClient }

constructor TRtcCryptClient.Create;
  begin
  inherited;
  HaveHello:=False;
  HaveStart:=False;
  AllReady:=False;
  ControlCounter:=0;
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  CRead:=nil;
  CWrite:=nil;
  end;
  
destructor TRtcCryptClient.Destroy;
  begin
  try
    Init;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcCryptClient.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcCryptClient.Kill;
  begin
  {$IFNDEF NEXTGEN} Free; {$ENDIF}
  end;

procedure TRtcCryptClient.Init;
  begin
  HaveHello:=False;
  HaveStart:=False;
  AllReady:=False;
  ControlCounter:=0;
  ClientHello:='';
  ServerHello:='';
  ClientKey:='';
  ServerKey:='';
  RtcFreeAndNil(CRead);
  RtcFreeAndNil(CWrite);
  end;

end.

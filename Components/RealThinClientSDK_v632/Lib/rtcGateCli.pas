{
  @html(<b>)
  RTC Gate Client
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit implements the "HTTP Gate Client" and "Gate Client Link" components.
}
unit rtcGateCli;

interface

{$include rtcDefs.inc}

uses
  SysUtils,
  Classes,

  memBinList,
  memObjList,
  rtcFastStrings,
  rtcCrypt,

  rtcTypes,
  rtcLog,
  rtcThrPool,
  rtcTimer,
  rtcSyncObjs,

  rtcInfo,
  rtcConn,
  rtcDataCli,
  rtcHttpCli,
  rtcPlugins,

  rtcGateConst;

// Disable PING when debugging, to avoid timeout-related disconnects
{.$DEFINE DISABLE_PING}

type
  ERtcGateClient = class(Exception);

  TRtcGateClientInState = (ins_Closed, ins_Prepare, ins_Start, ins_Recv,
                           ins_Idle, ins_Done, ins_Connecting);

  TRtcGateClientOutState = (outs_Closed, outs_Prepare, outs_Start, outs_Send,
                            outs_Idle, outs_Done, outs_Connecting);

  TRtcGateClientCommand = (// Error
                           gc_Error,
                           // received a complete packet
                           gc_SendAll,
                           // received first chunk of a new packet
                           gc_SendFirst,
                           // received next chunk of the new packet
                           gc_SendMore,
                           // received last chunk of the new packet
                           gc_SendLast,
                           // User "UserID" Joind our Group "GroupID"
                           gc_UserJoined,
                           // User "UserID" left our Group "GroupID"
                           gc_UserLeft,
                           // We have joined group "GroupID" owned by user "UserID"
                           gc_JoinedUser,
                           // We have left group "GroupID" owned by user "UserID"
                           gc_LeftUser,
                           // User "UserID" is now Online.
                           // Received after sending a PING if the user is connected to the Gateway.
                           gc_UserOnline,
                           // User "UserID" is now offline.
                           // Received after sending a PING, if the user is NOT connected to the Gateway.
                           gc_UserOffline);

  // @exclude
  TRtcHugeByteArrayWithCallID = class(TRtcHugeByteArray)
  public
    CallID:word;
    end;

  { @Abstract(Data received info)
    This class is used to make the information received from the Gateway
    available to the Client inside OnDataReceived and OnInfoReceived events. }
  TRtcGateClientData = class(TObject)
  protected
    // @exclude
    Bytes:RtcByteArray;
    // @exclude
    Location:integer;
    // @exclude
    Length:integer;

  public
    // Received Command
    Command:TRtcGateClientCommand;
    
    // Error Code if Command=gc_Error
    ErrCode:LongWord;

    // ID of the User who sent the package
    UserID:TGateUID;
 
   // ID of the Group to which the User has sent the package
    GroupID:TGateUID;

    // "Content" has the first bytes of the package (Header) ?
    Header:boolean;

    // "Content" has the last bytes of the package (Footer) ?
    Footer:boolean;

    // "CallID" -> available if Header=True or ToBuffer=True (otherwise, 0)
    CallID:word;

    // "Position" of the currently received chunk inside the Buffer -> available if ToBuffer=True.
    Position:integer;

    { If Header=True & Footer=True then "Content" has the complete package.
      Otherwise, "Content" has ONLY the currently received package chunk. }
    Content:RtcByteArray;

    { Set ToBuffer=True if content should be buffered, so we can access the complete 
        package from "Content" when Header=True and Footer=True (Command=gc_SendLast). }
    ToBuffer:boolean;
    end;

  TRtcHttpGateClient = class;

  { @Abstract(Abstract RTC Gate Client Link class)
    Implements basic functionality required by Gate Client Link components. }
  TRtcAbsGateClientLink = class(TRtcComponent)
  private
    FClientData: TRtcGateClientData;
    FClient: TRtcHttpGateClient;
    FUserGroups: TBinList;

  protected
    // @exclude
    FCS:TRtcCritSec;

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure RemoveClient(Value:TRtcHttpGateClient);

    // @exclude
    function GetClient: TRtcHttpGateClient;
    // @exclude
    procedure SetClient(const Value: TRtcHttpGateClient);

    // "BeforeLogin" event
    procedure Call_BeforeLogIn(Sender:TRtcConnection); virtual;
    // "AfterLogin" event
    procedure Call_AfterLogIn(Sender:TRtcConnection); virtual;
    // "AfterLoginFail" event
    procedure Call_AfterLoginFail(Sender:TRtcConnection); virtual;
    // "AfterLogOut" event
    procedure Call_AfterLogOut(Sender:TRtcConnection); virtual;

    // "OnInfoReceived" event
    procedure Call_OnInfoReceived(Sender:TRtcConnection); virtual;
    // "OnDataReceived" event
    procedure Call_OnDataReceived(Sender:TRtcConnection); virtual;
    // "OnReadyToSend" event
    procedure Call_OnReadyToSend(Sender:TRtcConnection); virtual;

    // "OnStreamReset" event
    procedure Call_OnStreamReset(Sender:TRtcConnection); virtual;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Data received from the Gateway. This property should ONLY
      be used from inside OnDataReceived and OnInfoReceived events. }
    property Data:TRtcGateClientData read FClientData;

    // Set status for User "UserID" and Group "GroupID"
    procedure SetUserGroupStatus(UserID,GroupID,Status:TGateUID);
    { Clear status for User "UserID" and Group "GroupID".
      This is the same as calling SetUserGroupStatus with Status=0. }
    procedure ClearUserGroupStatus(UserID,GroupID:TGateUID);
    { Clear status of all Users and Groups.
      Use this method from AfterLogin and OnStreamReset events. }
    procedure ClearAllUserGroupStates;

    { Get status for User "UserID" and Group "GroupID".
      Returns 0 if no status stored for UserID + GroupID }
    function GetUserGroupStatus(UserID,GroupID:TGateUID):TGateUID;
    { Get min status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID". }
    function GetMinUserGroupStatus(UserID:TGateUID):TGateUID;
    { Get max status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMaxUserGroupStatus(UserID:TGateUID):TGateUID;
    { Get min GroupID value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMinUserGroupID(UserID:TGateUID):TGateUID;
    { Get the next UserID and GroupID with the current Status.
      Returns 0 if no there are no more status records stored. }
    function GetNextUserGroupStatus(var UserID,GroupID:TGateUID):TGateUID;

  published
    { Set the Client property to a RtcHttpGateClient component
      to listen to all events from that component. }
    property Client:TRtcHttpGateClient read FClient write SetClient;
    end;

  { @Abstract(RTC Gate Client Link)
    Link this component to a TRtcHttpGateClient to handle user events. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGateClientLink = class(TRtcAbsGateClientLink)
  private
    FBeforeLogIn: TRtcNotifyEvent;
    FAfterLogIn: TRtcNotifyEvent;
    FAfterLoginFail: TRtcNotifyEvent;
    FAfterLogOut: TRtcNotifyEvent;
    FOnDataReceived: TRtcNotifyEvent;
    FOnInfoReceived: TRtcNotifyEvent;
    FOnReadyToSend: TRtcNotifyEvent;
    FOnStreamReset: TRtcNotifyEvent;

  protected
    // @exclude
    procedure Call_BeforeLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogIn(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLoginFail(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_AfterLogOut(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnDataReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnInfoReceived(Sender:TRtcConnection); override;
    // @exclude
    procedure Call_OnReadyToSend(Sender:TRtcConnection); override;

    // @exclude
    procedure Call_OnStreamReset(Sender:TRtcConnection); override;

  published
    { Event called BEFORE the user Logs in to the Gateway.
      It can be used for custom component setup. }
    property BeforeLogIn:TRtcNotifyEvent read FBeforeLogIn write FBeforeLogIn;

    { Event called after the Client logs in to the Gateway }
    property AfterLogIn:TRtcNotifyEvent read FAfterLogIn write FAfterLogIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made. }
    property AfterLoginFail:TRtcNotifyEvent read FAfterLoginFail write FAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made. }
    property AfterLogOut:TRtcNotifyEvent read FAfterLogOut write FAfterLogOut;

    { Received data from the Gateway. Use the "Data" property to check what was received. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { Received info from the Gateway. Use the "Data" property to check what was received. }
    property OnInfoReceived:TRtcNotifyEvent read FOnInfoReceived write FOnInfoReceived;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.  }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group relations and status info. }
    property OnStreamReset:TRtcNotifyEvent read FOnStreamReset write FOnStreamReset;
    end;

  // @exclude
  TRtcGateClientLinkList = class(TObject)
  private
    FList:TObjectList;

  public
    constructor Create;
    destructor Destroy; override;

    procedure Add(Value:TRtcAbsGateClientLink);
    procedure Remove(Value:TRtcAbsGateClientLink);

    procedure RemoveAll;

    function Count:integer;
    function Get(index:integer):TRtcAbsGateClientLink;

    procedure DoBeforeLogIn(Sender:TRtcConnection; CS:TRtcCritSec);
    procedure DoAfterLogIn(Sender:TRtcConnection; CS:TRtcCritSec);
    procedure DoAfterLoginFail(Sender:TRtcConnection; CS:TRtcCritSec);
    procedure DoAfterLogOut(Sender:TRtcConnection; CS:TRtcCritSec);

    procedure DoDataReceived(Sender:TRtcConnection; CS:TRtcCritSec);
    procedure DoInfoReceived(Sender:TRtcConnection; CS:TRtcCritSec);
    procedure DoReadyToSend(Sender:TRtcConnection; CS:TRtcCritSec);

    procedure DoStreamReset(Sender:TRtcConnection; CS:TRtcCritSec);
    end;

  { @Abstract(RTC Gate HTTP Client component)
    HTTP Client component for communication with a RTC Gateway.
    This component has two (2) TRtcHttpClient components internally
    for handling outout and input data streams. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcHttpGateClient = class(TRtcComponent)
  private
    ClientOUT: TRtcHttpClient;
    ClientIN: TRtcHttpClient;
    DataLogin: TRtcDataRequest;
    DataIN: TRtcDataRequest;
    DataOUT: TRtcDataRequest;

    FGateFileName,
    FGATEURI_PING,
    FGATEURI_LOGIN,
    FGATEURI_INPUT,
    FGATEURI_OUTPUT,
    FGATEURI_INPUTRESET,
    FGATEURI_OUTPUTRESET,

    FGATE_PRIMARY_KEY,
    FGATE_SECONDARY_KEY,
    FGATE_USERAUTH: RtcString;

    FAutoRetry:integer;
    FNeedLogin:boolean;
    FAutoRelogin:boolean;

    FInputResetTime: Cardinal;
    FOutputResetTime: Cardinal;
    FInputResetAt: Cardinal;
    FOutputResetAt: Cardinal;

    FOnStreamReset: TRtcNotifyEvent;
    FOnDataReceived: TRtcNotifyEvent;
    FOnInfoReceived: TRtcNotifyEvent;
    FOnReadyToSend: TRtcNotifyEvent;
    FData: TRtcGateClientData;

    FLastError: String;

    FBackground: boolean;
    FUseBlocking: boolean;
    FUseWinHTTP: boolean;
    FUseProxy: boolean;
    FUseSSL: boolean;

    FUserLogin: TRtcUserLoginInfo;
    FCryptPlugin:TRtcCryptPlugin;

    FBlocking: boolean;

    FBeforeLogIn: TRtcNotifyEvent;
    FAfterLogOut: TRtcNotifyEvent;
    FAfterLogIn: TRtcNotifyEvent;
    FAfterLogInFail: TRtcNotifyEvent;

    FInputState: TRtcGateClientInState;
    FOutputState: TRtcGateClientOutState;
    FGateAddr: RtcString;
    FGatePort: RtcString;

    FStreamBlockSizeIn,
    FStreamBlockSizeOut:longword;

    FCS:TRtcCritSec;
    FEV:TRtcEvent;
    FPingTimer:TRtcTimer;

    FBuffRcv:TObjList;

    FTotalSent: int64;
    FTotalReceived: int64;

    FMyIP:RtcString;
    FMyKey:RtcString;
    FMyUID:TGateUID;

    FCryIn,FCryOut:TRtcCrypt;

    FBuffIn,
    FBuffOut:TRtcHugeByteArray;

    FNeedInput:integer;

    FLoginStatus:byte;
    FLoggedIn,
    FNeedOutput,
    FOutputReady,
    FInputReady:boolean;

    FLastContact,
    FLastInput,
    FLastOutput:Cardinal;

    FLastDataIn,
    FLastDataOut:Cardinal;

    FDataWaiting,
    FSpaceToSend,
    FNowSending,
    FInputWasLost,
    FOutputWasLost,
    FGatewayLost,
    FLoggedOut:boolean;

    FInputReset,
    FOutputReset:integer;

    FReqToSend,
    FLeftToSend:longword;

    FPingInCnt,
    FPingOutCnt:integer;

    FSpeedIn,
    FSpeedOut:Cardinal;

    FUsedGroupIDs:array[1..CntMaxGroups] of boolean;

    FGateClientLinks:TRtcGateClientLinkList;

    FUserGroups:TBinList;

  protected

    // @exclude
    function GetAutoRelogin: boolean;
    // @exclude
    procedure SetAutoRelogin(const Value: boolean);

    // @exclude
    procedure AddGateClientLink(Value:TRtcAbsGateClientLink);
    // @exclude
    procedure RemoveGateClientLink(Value:TRtcAbsGateClientLink);
    // @exclude
    procedure RemoveAllGateClientLinks;

    // @exclude
    procedure DataLoginBeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataLoginDataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataResponseAbort(Sender: TRtcConnection);
    // @exclude
    procedure DataINBeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataINDataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTBeginRequest(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataReceived(Sender: TRtcConnection);
    // @exclude
    procedure DataOUTDataSent(Sender: TRtcConnection);

    // @exclude
    procedure SetGateUserAuth(const Value: RtcString);
    // @exclude
    procedure SetGateFileName(const Value: RtcString);
    // @exclude
    procedure SetGatePrimaryKey(const Value: RtcString);
    // @exclude
    procedure SetGateSecondaryKey(const Value: RtcString);

    // @exclude
    procedure InitInput;
    // @exclude
    procedure InitOutput;

    // @exclude
    procedure User_LogIn(Sender:TRtcConnection);
    // @exclude
    procedure User_LogOut(Sender:TRtcConnection);
    // @exclude
    procedure UserStreamLost(Sender:TRtcConnection);
    // @exclude
    procedure UserStreamReset(Data:TRtcValue);

    // @exclude
    function ReadyToSend(Sender:TRtcConnection):boolean;
    // @exclude
    procedure ReadyToSendNow(Sender:TRtcConnection);

    // @exclude
    function ProcessInput(Sender:TRtcConnection):int64;

    // @exclude
    procedure PingCheck;

    // @exclude
    procedure DoBeforeLogin(Sender:TRtcConnection);
    // @exclude
    procedure DoAfterLogin(Sender:TRtcConnection);
    // @exclude
    procedure DoAfterLogout(Sender:TRtcConnection);
    // @exclude
    procedure DoAfterLoginFail(Sender:TRtcConnection);

    // @exclude
    procedure DoReadyToSend(Sender:TRtcConnection);
    // @exclude
    procedure DoDataReceived(Sender:TRtcConnection);
    // @exclude
    procedure DoInfoReceived(Sender:TRtcConnection);

    // @exclude
    procedure DoStreamReset(Sender:TRtcConnection);

    // @exclude
    procedure SetGateAddr(const Value: RtcString);
    // @exclude
    procedure SetGatePort(const Value: RtcString);

    // @exclude
    function GetActive: boolean;
    // @exclude
    function GetReady: boolean;

    // @exclude
    procedure SetActive(const Value: boolean);

    // @exclude
    function GetStreamBlockSizeIn: longword;
    // @exclude
    function GetStreamBlockSizeOut: longword;
    // @exclude
    procedure SetStreamBlockSizeIn(const Value: longword);
    // @exclude
    procedure SetStreamBlockSizeOut(const Value: longword);

    // @exclude
    procedure SetUseProxy(const Value: boolean);
    // @exclude
    procedure SetUseSSL(const Value: boolean);
    // @exclude
    procedure SetUseWinHTTP(const Value: boolean);
    // @exclude
    procedure SetUseBlocking(const Value: boolean);

    // @exclude
    procedure SetCryptPlugin(const Value: TRtcCryptPlugin);
    // @exclude
    procedure SetUserLogin(const Value: TRtcUserLoginInfo);

    // @exclude
    procedure RetryLogin;

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    // Set LOCAL status for User "UserID" and Group "GroupID"
    procedure SetUserGroupStatus(UserID,GroupID,Status:TGateUID);

    { Clear LOCAL status for User "UserID" and Group "GroupID".
      This is the same as calling SetUserGroupStatus with Status=0. }
    procedure ClearUserGroupStatus(UserID,GroupID:TGateUID);

    // Clear LOCAL status of all Users and Groups
    procedure ClearAllUserGroupStates;

    { Get LOCAL status for User "UserID" and Group "GroupID".
      Returns 0 if no status stored for UserID + GroupID }
    function GetUserGroupStatus(UserID,GroupID:TGateUID):TGateUID;

    { Get min LOCAL status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID". }
    function GetMinUserGroupStatus(UserID:TGateUID):TGateUID;

    { Get max LOCAL status value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMaxUserGroupStatus(UserID:TGateUID):TGateUID;

    { Get min LOCAL GroupID value (>0) for User "UserID".
      Returns 0 if no status stored for user "UserID" }
    function GetMinUserGroupID(UserID:TGateUID):TGateUID;

    { Get the next LOCAL UserID and GroupID, return UserID+GroupID Status.
      Returns 0 if no there are no more status records stored. }
    function GetNextUserGroupStatus(var UserID,GroupID:TGateUID):TGateUID;

    // Reset Input and Output data streams (reset both connections)
    procedure ResetStreams;

    { Send "data" (Content) with "CallID" to "UserID" and "GroupID" (through the Gateway).
      You can use any CallID from 0 - 65000, but do NOT use CallIDs above 65000,
      they will be used in a future updates to extend the components with new features. }
    function SendBytes(UserID, GroupID:TGateUID; CallID:word; const data:RtcByteArray):boolean; overload;

    { Send "CallID" with no extra content to "UserID" and "GroupID" (through the Gateway).
      You can use any CallID from 0 - 65000, but do NOT use IDs above 65000,
      they will be used in a future updates to extend the components with new features. }
    function SendBytes(UserID, GroupID:TGateUID; CallID:word):boolean; overload;

    // Ping user "UserID" (through the Gateway)
    function PingUser(UserID:TGateUID):boolean;

    // Add User "UserID" to group "GroupID" owned by user "OwnerID" (on the Gateway)
    function AddUserToGroup(OwnerID, GroupID, UserID:TGateUID):boolean;

    // Remove User "UserID" from group "GroupID" owned by user "OwnerID"  (on the Gateway)
    function RemUserFromGroup(OwnerID, GroupID, UserID:TGateUID):boolean;

    // Remove Group "GroupID" owned by user "OwnerID" (on the Gateway)
    function RemoveGroup(OwnerID, GroupID:TGateUID):boolean;

    // LOCAL: Allocate and return the next free GroupID currently NOT in use
    function  AllocNextFreeGroupID:TGateUID;

    // LOCAL: Manually allocate GroupID "GroupID"
    procedure AllocGroupID(GroupID:TGateUID);

    // LOCAL: Release GroupID "GroupID" - it is no longer being used and may be re-used
    procedure ReleaseGroupID(GroupID:TGateUID);

    // LOCAL: Release all GroupIDs.
    // This is required after Logout, LoginFail or StreamReset if the IDs won't be used anymore.
    procedure ReleaseAllGroupIDs;

    // My IP address returned from the Gateway after login
    property MyIP:RtcString read FMyIP;

    // My encryption Key returned from the Gateway after login
    property MyKey:RtcString read FMyKey;

    // My User ID returned from the Gateway after login
    property MyUID:TGateUID read FMyUID;

    property InputState:TRtcGateClientInState read FInputState;
    property OutputState:TRtcGateClientOutState read FOutputState;

    property LoggedIn:boolean read FLoggedIn;
    property OutputReady:boolean read FOutputReady;
    property InputReady:boolean read FInputReady;

    property LastContact:cardinal read FLastContact;
    property LastInput:cardinal read FLastInput;
    property LastOutput:cardinal read FLastOutput;

    property LastDataIn:cardinal read FLastDataIn;
    property LastDataOut:cardinal read FLastDataOut;

    property PingInCnt:integer read FPingInCnt;
    property PingOutCnt:integer read FPingOutCnt;

    property LeftToSend:longword read FLeftToSend;

    property TotalSent:int64 read FTotalSent;
    property TotalReceived:int64 read FTotalReceived;

    property LastError:String read FLastError;

    property OutputResetAt:Cardinal read FOutputResetAt;
    property InputResetAt:Cardinal read FInputResetAt;
    property OutputResetTime:Cardinal read FOutputResetTime;
    property InputResetTime:Cardinal read FInputResetTime;

    // Connected and ready to send and receive?
    property Ready:boolean read GetReady;

    { Data received from the Gateway,
      available from OnDataReceived and OnInfoReceived events }
    property Data:TRtcGateClientData read FData;

  published
    { Set Active=TRUE to connect the user to the Gateway and stay connected until LogOut or LoginFail.
      Set Active=FALSE to disconnect the user from the Gateway. If AutoLogin=True,
          a new connection will be established imediately after LogOut, with a new User ID. }
    property Active:boolean read GetActive write SetActive default False;

    { Set AutoLogin=TRUE to make sure the user is connected and stays connected, even after LogOut or LoginFail.
      Set AutoLogin=FALSE to disconnect the user from the Gateway and stop reconnecting after LogOut and LoginFail. }
    property AutoLogin:boolean read GetAutoRelogin write SetAutoRelogin default False;

    { Properties to set before use }

    // Gateway Address (IP or Domain name, without the "http://" or "https://" prefix)
    property GateAddr:RtcString read FGateAddr write SetGateAddr;

    // Gateway Port number
    property GatePort:RtcString read FGatePort write SetGatePort;

    // FileName ("/" for root) where the TRtcGateway component is accessible
    property GateFileName:RtcString read FGateFileName write SetGateFileName;

    // Primary Encryption Key set up on the TRtcGateway component
    property GatePrimaryKey:RtcString read FGATE_PRIMARY_KEY write SetGatePrimaryKey;
 
   { Secondary Encryption Key, specific to this Client.
      Leave Empty for default key generation, or implement the OnUserLogin event
      on the TRtcGateway component to set the same value on the Gateway for this Client. }
    property GateSecondaryKey:RtcString read FGATE_SECONDARY_KEY write SetGateSecondaryKey;

    { User Authentication information, packed into a single String,
      which will be made available in all events on the TRtcGateway component and
      can be used to identify this Client or add the Client to specific User Groups. }
    property GateUserAuth:RtcString read FGATE_USERAUTH write SetGateUserAuth;

    // Client running from a Background process with no GUI?
    property Background:boolean read FBackground write FBackground default False;

    // Use Blocking connection provider
    property UseBlocking:boolean read FUseBlocking write SetUseBlocking default False;

    // Use Proxy-compatible connection provider
    property UseProxy:boolean read FUseProxy write SetUseProxy default False;

    // Use WinHTTP on Windows
    property UseWinHTTP:boolean read FUseWinHTTP write SetUseWinHTTP default False;

    // Use SSL-compatible connection provider
    property UseSSL:boolean read FUseSSL write SetUseSSL default False;

    { To use SSL/SSH encryption with third-party components, simply assign the encryption
      plug-in here before you start using the Client connection (before first connect). }
    property UseCryptPlugin:TRtcCryptPlugin read FCryptPlugin write SetCryptPlugin;

    { UserLogin data is ignored when UseCryptPlugin is assigned. @html(<br><br>)

      If UseCryptPlugin is NOT assigned (not using third-party components for encryption) ... @html(<br><br>)

      Using this property, you can define login information for a server which
      requires user authentication and/or a client certificate (using WinInet API).
      If you want to use Third-party SSL/SSH components for encryption,
      simply assign the plug-in to the UseCryptPlugin property. }
    property UserLogin:TRtcUserLoginInfo read FUserLogin write SetUserLogin;

    // Input Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB
    property StreamBlockSizeIn:longword read GetStreamBlockSizeIn write SetStreamBlockSizeIn default 0;

    // Output Stream Block Size. By default (0) use DEF_OUTSTREAM_SIZE = 2 GB
    { If the Client has to work on a Systems with active Anti-Virus Software,
      make sure to set the "StreamBlockSizeOut" property on to the maximum number of bytes
      you want to allow Anti-Virus Software to use when the Client is streaming data out.
      This limit is achieved by limiting the outgoing request size and thus forcing
      the Client to wait for a response from the Gateway before more data is sent. }
    property StreamBlockSizeOut:longword read GetStreamBlockSizeOut write SetStreamBlockSizeOut default 0;

    { Event called BEFORE the user Logs in to the Gateway.
      It can be used for custom component setup. }
    property BeforeLogIn:TRtcNotifyEvent read FBeforeLogIn write FBeforeLogIn;

    { Event called after the Client logs in to the Gateway }
    property AfterLogIn:TRtcNotifyEvent read FAfterLogIn write FAfterLogIn;

    { Event called if the last Login attempt has failed.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made. }
    property AfterLoginFail:TRtcNotifyEvent read FAfterLoginFail write FAfterLoginFail;

    { Event called if the Client has been logged out of the Gateway.
      This can either be because the Client has permanently lost connection
      and the UserID belonging to this Client is no longer valid,
      or because the Active property was set to FALSE to close the connection.
      If AutoLogin=True, a new Login attempt will be made.
      If AutoLogin=False, no futher login attempts will be made. }
    property AfterLogOut:TRtcNotifyEvent read FAfterLogOut write FAfterLogOut;

    { Received data from the Gateway. Use the "Data" property to check what was received. }
    property OnDataReceived:TRtcNotifyEvent read FOnDataReceived write FOnDataReceived;

    { Received info from the Gateway. Use the "Data" property to check what was received. }
    property OnInfoReceived:TRtcNotifyEvent read FOnInfoReceived write FOnInfoReceived;

    { Client is ready to send more data to the Gateway. Use this event to implement
      the process of sending larger data "streams" by breaking them up in smaller packets.
      Keep in mind that the maximum single packet size is 16 MB.  }
    property OnReadyToSend:TRtcNotifyEvent read FOnReadyToSend write FOnReadyToSend;

    { Input and Output Streams have been reset, either because the "ResetStreams"
      method was called, or because Clients connection to the Gateway was lost.
      A new connection attempt will be made after this event.
      Use this event for cleaning up Group relations and status info. }
    property OnStreamReset:TRtcNotifyEvent read FOnStreamReset write FOnStreamReset;
  end;

implementation

{ TRtcGateClientLinkList }

constructor TRtcGateClientLinkList.Create;
  begin
  inherited;
  FList:=TObjectList.Create;
  end;

destructor TRtcGateClientLinkList.Destroy;
  begin
  try
    RtcFreeAndNil(FList);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcGateClientLinkList.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcGateClientLinkList.Add(Value: TRtcAbsGateClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx<0 then
    FList.Add(Value);
  end;

function TRtcGateClientLinkList.Count: integer;
  begin
  Result:=FList.Count;
  end;

function TRtcGateClientLinkList.Get(index:integer): TRtcAbsGateClientLink;
  begin
  if (index>=0) and (index<FList.Count) then
    Result:=TRtcAbsGateClientLink(FList.Items[index])
  else
    Result:=nil;
  end;

procedure TRtcGateClientLinkList.Remove(Value: TRtcAbsGateClientLink);
  var
    idx:integer;
  begin
  idx:=FList.IndexOf(Value);
  if idx>=0 then
    FList.Delete(idx);
  end;

procedure TRtcGateClientLinkList.RemoveAll;
  begin
  if assigned(FList) then
    FList.Clear;
  end;

procedure TRtcGateClientLinkList.DoAfterLogIn(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_AfterLogIn(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoAfterLoginFail(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_AfterLoginFail(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoAfterLogOut(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_AfterLogOut(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoBeforeLogIn(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_BeforeLogIn(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoDataReceived(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_OnDataReceived(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoInfoReceived(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      try
        Link.Call_OnInfoReceived(Sender);
      except
        // ignore exceptions
        end;
    until False;
  end;

procedure TRtcGateClientLinkList.DoReadyToSend(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      Link.Call_OnReadyToSend(Sender);
    until False;
  end;

procedure TRtcGateClientLinkList.DoStreamReset(Sender: TRtcConnection; CS:TRtcCritSec);
  var
    Link:TRtcAbsGateClientLink;
    a:integer;
  begin
  a:=0;
  repeat
    CS.Acquire;
    try
      Link:=Get(a);
    finally
      CS.Release;
      end;
    Inc(a);
    if Link=nil then
      Break
    else
      Link.Call_OnStreamReset(Sender);
    until False;
  end;

{ TRtcAbsGateClientLink }

constructor TRtcAbsGateClientLink.Create(AOwner: TComponent);
  begin
  inherited;
  FClient:=nil;
  FClientData:=nil;
  FUserGroups:=tBinList.Create(16);
  FCS:=TRtcCritSec.Create;
  end;

destructor TRtcAbsGateClientLink.Destroy;
  begin
  Client:=nil;
  FClientData:=nil;
  RtcFreeAndNil(FUserGroups);
  RtcFreeAndNil(FCS);
  inherited;
  end;

procedure TRtcAbsGateClientLink.SetUserGroupStatus(UserID, GroupID, Status: TGateUID);
  begin
  FCS.Acquire;
  try
    if Status>0 then
      begin
      if FUserGroups.search((UserID shl UserIDShift) or GroupID)<=0 then
        FUserGroups.insert((UserID shl UserIDShift) or GroupID,Status)
      else
        FUserGroups.change((UserID shl UserIDShift) or GroupID,Status);
      end
    else
      ClearUserGroupStatus(UserID,GroupID);
  finally
    FCS.Release;
    end;
  end;

function TRtcAbsGateClientLink.GetMinUserGroupID(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  FCS.Acquire;
  try
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    if (UID>0) and (UserID = UID shr UserIDShift) then
      Result:=UID - (UserID shl UserIDShift)
    else
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcAbsGateClientLink.GetMinUserGroupStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    while (GID>0) and (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if (Result=0) or (GID<Result) then
        Result:=GID;
      UID:=FUserGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcAbsGateClientLink.GetMaxUserGroupStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    while (GID>0) and (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if GID>Result then
        Result:=GID;
      UID:=FUserGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcAbsGateClientLink.GetUserGroupStatus(UserID, GroupID: TGateUID): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=FUserGroups.search((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

function TRtcAbsGateClientLink.GetNextUserGroupStatus(var UserID, GroupID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FUserGroups.search_g((UserID shl UserIDShift) or GroupID, GID);
    if (UID>0) and (GID>0) then
      begin
      UserID:=UID shr UserIDShift;
      GroupID:=UID - (UserID shl UserIDShift);
      Result:=GID;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcAbsGateClientLink.ClearAllUserGroupStates;
  begin
  FCS.Acquire;
  try
    FUserGroups.removeall;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcAbsGateClientLink.ClearUserGroupStatus(UserID, GroupID: TGateUID);
  begin
  FCS.Acquire;
  try
    if FUserGroups.search((UserID shl UserIDShift) or GroupID)>0 then
      FUserGroups.remove((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcAbsGateClientLink.Call_AfterLogIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  end;

procedure TRtcAbsGateClientLink.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  end;

function TRtcAbsGateClientLink.GetClient: TRtcHttpGateClient;
  begin
  Result:=FClient;
  end;

procedure TRtcAbsGateClientLink.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent, Operation);
  if Operation=opRemove then
    if AComponent=FClient then
      SetClient(nil);
  end;

procedure TRtcAbsGateClientLink.RemoveClient(Value: TRtcHttpGateClient);
  begin
  if Value=FClient then Client:=nil;
  end;

procedure TRtcAbsGateClientLink.SetClient(const Value: TRtcHttpGateClient);
  begin
  if Value<>FClient then
    begin
    if assigned(FClient) then
      FClient.RemoveGateClientLink(self);
    if assigned(Value) then
      begin
      FClient:=Value;
      FClient.AddGateClientLink(self);
      FClientData:=FClient.Data;
      end
    else
      begin
      FClient:=nil;
      FClientData:=nil;
      end;
    end;
  end;

{ TRtcHttpGateClient }

procedure TRtcHttpGateClient.DataLoginBeginRequest(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    auth,ccode:RtcString;
  begin
  if FGatewayLost then Exit;

  if Cli.Request.FileName=FGATEURI_PING then
    Cli.Write
  else if Cli.Request.FileName=FGATEURI_LOGIN then
    begin
    ccode:=FGATE_PRIMARY_KEY;

    FCS.Acquire;
    try
      Crypt(ccode, FMyIP);
      FMyKey:=ccode;

      if FGATE_SECONDARY_KEY<>'' then
        Crypt(FMyKey, FGATE_SECONDARY_KEY);

      FCryIn.Key:=FMyKey;
      FCryIn.Init;

      FCryOut.Key:=FMyKey;
      FCryOut.Init;

    finally
      FCS.Release;
      end;

    auth:=FGATE_USERAUTH;
    if length(auth)>0 then
      Crypt(auth, ccode);

    Cli.Write(auth);
    end;
  end;

procedure TRtcHttpGateClient.DataLoginDataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    res:RtcByteArray;
  begin
  SetLength(res,0);
  if FGatewayLost then Exit;

  if (Cli.Response.StatusCode<>GATE_OK_CODE) or (Cli.Response.ContentIn>1024) then
    begin
    if Cli.Response.Done and
      ( (Cli.Response.StatusCode=GATE_ERROR_CODE) or
        (Cli.Response.StatusCode=GATE_FATALERROR_CODE) ) then
      begin
      res:=Cli.ReadEx;
      if length(res)>0 then
        Cli.Response.StatusText:=RtcBytesToString(res);
      end
    else if Cli.Response.StatusCode=GATE_OK_CODE then
      Cli.Response.StatusText:='Bad Response from Gateway';
    // Bad Response from Gateway
    User_LogOut(Sender);
    end
  else if Cli.Response.Done then
    begin
    if Cli.Request.FileName=FGATEURI_PING then
      begin
      res:=Cli.ReadEx;
      if length(res)>30 then
        begin
        Cli.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)<8 then
        begin
        UserStreamLost(Sender);
        Exit;
        end
      else
        begin
        FCS.Acquire;
        try
          FMyIP:=RtcBytesToString(res);
        finally
          FCS.Release;
          end;

        DataLogin.Request.Method:='POST';
        DataLogin.Request.FileName:=FGATEURI_LOGIN;
        DataLogin.Post(True,Sender);
        end;
      end
    else if Cli.Request.FileName=FGATEURI_LOGIN then
      begin
      res:=Cli.ReadEx;
      if length(res)>4 then
        begin
        Cli.Response.StatusText:='Bad Response from Gateway';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)=0 then
        begin
        Cli.Response.StatusText:='Gateway Access Denied';
        User_LogOut(Sender);
        Exit;
        end
      else if length(res)<4 then
        begin
        UserStreamLost(Sender);
        Exit;
        end
      else
        begin
        FCS.Acquire;
        try
          DeCryptEx(res,RtcStringToBytes(MyKey));
          FMyUID:=Bytes2Int(res) shr UserIDShift;
        finally
          FCS.Release;
          end;

        DataIN.Request.Method:='POST';
        DataIN.Request.FileName:=FGATEURI_OUTPUTRESET;
        DataIN.Post(True,Sender);
        end;
      end;
    end;
  end;

procedure TRtcHttpGateClient.DataResponseAbort(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
  begin
  Cli.Request.Skip;
  UserStreamLost(Sender);
  end;

procedure TRtcHttpGateClient.DataOUTBeginRequest(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    UID:RtcString;
    UIDB:RtcByteArray;
  begin
  if FGatewayLost then Exit;

  FCS.Acquire;
  try
    UIDB:=Int2Bytes(FMyUID shl UserIDShift);
    CryptEx(UIDB,RtcStringToBytes(MyIP));
    UID:=LWord2Str(Bytes2Int(UIDB));
    Cli.Request.Query['id']:=UID;

    if Cli.Request.FileName<>FGATEURI_INPUT then
      Cli.Request.ContentLength:=0
    else if FStreamBlockSizeOut<1 then
      Cli.Request.ContentLength:=DEF_OUTSTREAM_SIZE
    else
      Cli.Request.ContentLength:=FStreamBlockSizeOut;

    FReqToSend:=Cli.Request.ContentLength;
    FSpaceToSend:=FReqToSend>0;

    FNowSending:=False;
  finally
    FCS.Release;
    end;

  FCS.Acquire;
  try
    FOutputState:=outs_Prepare;
    FNowSending:=True;
  finally
    FCS.Release;
    end;
  Cli.Write;
  end;

procedure TRtcHttpGateClient.DataOUTDataSent(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    needAfterLogin,needReadyToSend,haveSignal:boolean;
  begin
  if FGatewayLost then Exit;

  needAfterLogin:=False;
  needReadyToSend:=False;

  FCS.Acquire;
  try
    if (FLoggedIn or (FLoginStatus=3)) and (Cli.Request.FileName=FGATEURI_INPUT) then
      begin
      FLastOutput:=GetAppRunTime;
      FOutputReady:=True;

      FOutputWasLost:=False;
      FOutputReset:=0;

      if not FLoggedOut then FGatewayLost:=False;
      FNowSending:=False;

      if FInputReady and (FLoginStatus=3) then
        begin
        FLastInput:=FLastOutput;
        FLastContact:=FLastOutput;
        needAfterLogin:=not FLoggedIn;
        needReadyToSend:=True;
        FNeedOutput:=True;
        FLoginStatus:=0;
        FLoggedIn:=True;
        end;
      end;
  finally
    FCS.Release;
    end;

  if needAfterLogin then
    DoAfterLogin(Sender);
  if needReadyToSend and not FDataWaiting then
    DoReadyToSend(Sender);

  if Cli.Request.Complete then
    begin
    FCS.Acquire;
    try
      FOutputReady:=False;
      FOutputState:=outs_Done;
    finally
      FCS.Release;
      end;
    end
  else if not FGatewayLost then
    begin
    if Cli.Request.Started then
      FOutputState:=outs_Start
    else
      FOutputState:=outs_Idle;

    if not ReadyToSend(Sender) then
      begin
      if FBlocking then 
        haveSignal:=FEV.WaitFor(WAIT_INFINITE)=wr_Signaled 
      else
        haveSignal:=FEV.WaitFor(0)=wr_Signaled;
      if haveSignal then
        ReadyToSend(Sender);
      end;
    end;
  end;

procedure TRtcHttpGateClient.DataOUTDataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    data:RtcByteArray;
    contLoop:boolean;
  begin
  SetLength(data,0);
  if FGatewayLost then Exit;

  if (Cli.Response.StatusCode<>GATE_OK_CODE) or (Cli.Response.ContentIn>1024) then
    begin
    if Cli.Response.Done and
      ( (Cli.Response.StatusCode=GATE_ERROR_CODE) or
        (Cli.Response.StatusCode=GATE_FATALERROR_CODE) ) then
      begin
      data:=Cli.ReadEx;
      if length(data)>0 then
        Cli.Response.StatusText:=RtcBytesToString(data);
      end
    else if Cli.Response.StatusCode=GATE_OK_CODE then
      Cli.Response.StatusText:='Bad Response';

    if (Cli.Response.StatusCode=GATE_OK_CODE) or
       (Cli.Response.StatusCode=GATE_ERROR_CODE) then
      UserStreamLost(Sender)
    else // Bad Response from Gateway
      User_LogOut(Sender);
    end
  else if Cli.Response.Done then
    begin
    data:=Cli.ReadEx;
    if length(data)>0 then
      begin
      Cli.Response.StatusText:='Bad Response';
      User_LogOut(Sender);
      Exit;
      end
    else
      begin
      FCS.Acquire;
      try
        if FLoginStatus=2 then
          begin
          FLoginStatus:=3;

          FLastInput:=GetAppRunTime;
          FLastOutput:=FLastInput;

          if not FLoggedOut then FGatewayLost:=False;

          contLoop:=True;
          end
        else
          contLoop:=False;
        FOutputState:=outs_Done;
      finally
        FCS.Release;
        end;

      // Continue OUTPUT loop ...
      DataOUT.Request.Method:='POST';
      DataOUT.Request.FileName:=FGATEURI_INPUT;
      DataOUT.Post(True,Sender);
      // StartOutputJob.Post(nil);

      if contLoop then
        begin
        // Continue INPUT loop ...
        DataIN.Request.Method:='POST';
        DataIN.Request.FileName:=FGATEURI_OUTPUT;
        DataIN.Post;
        // StartInputJob.Post(nil);
        end;
      end;
    end;
  end;

procedure TRtcHttpGateClient.DataINBeginRequest(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    UID:RtcString;
    UIDB:RtcByteArray;
    s:RtcString;
  begin
  if FGatewayLost then Exit;

  FCS.Acquire;
  try
    UIDB:=Int2Bytes(FMyUID shl UserIDShift);
    CryptEx(UIDB,RtcStringToBytes(MyIP));
    UID:=LWord2Str(Bytes2Int(UIDB));
    Cli.Request.Query['id']:=UID;
    if Cli.Request.FileName<>FGATEURI_OUTPUT then
      s:=''
    else
      begin
      if FStreamBlockSizeIn<1 then
        s:=IntToStr(DEF_OUTSTREAM_SIZE)
      else
        s:=IntToStr(FStreamBlockSizeIn);
      end;
    FInputState:=ins_Prepare;
  finally
    FCS.Release;
    end;

  if Cli.Request.FileName=FGATEURI_OUTPUT then
    Cli.Response.ExpectedBytes:=ProcessInput(Sender);

  Cli.Write(s);
  end;

procedure TRtcHttpGateClient.DataINDataReceived(Sender: TRtcConnection);
  var
    Cli:TRtcDataClient absolute Sender;
    data:RtcByteArray;
    contLoop:boolean;
    needAfterLogin,needReadyToSend:boolean;
  begin
  SetLength(data,0);
  if FGatewayLost then Exit;

  if Cli.Response.StatusCode<>GATE_OK_CODE then
    begin
    if Cli.Response.Done or (Cli.Response.ContentIn>1024) then
      begin
      if Cli.Response.Done and
        ( (Cli.Response.StatusCode=GATE_ERROR_CODE) or
          (Cli.Response.StatusCode=GATE_FATALERROR_CODE) ) then
        begin
        data:=Cli.ReadEx;
        if length(data)>0 then
          Cli.Response.StatusText:=RtcBytesToString(data);
        end
      else if Cli.Response.StatusCode=GATE_OK_CODE then
        Cli.Response.StatusText:='Bad Response from Gateway';
      if (Cli.Response.StatusCode=GATE_OK_CODE) or
         (Cli.Response.StatusCode=GATE_ERROR_CODE) then
        UserStreamLost(Sender)
      else // Bad Response from Gateway
        User_LogOut(Sender);
      end;
    end
  else if not FGatewayLost then
    begin
    data:=Cli.ReadEx;
    if length(data)>0 then
      begin
      if Cli.Request.FileName<>FGATEURI_OUTPUT then
        begin
        Cli.Response.StatusText:='Bad Response';
        User_LogOut(Sender);
        Exit;
        end
      else
        begin
        needAfterLogin:=False;
        needReadyToSend:=False;

        FCS.Acquire;
        try
          if (FLoggedIn or (FLoginStatus=3)) then
            begin
            FInputState:=ins_Start;
            FInputReady:=True;
            if FOutputReady and (FLoginStatus=3) then
              begin
              FLastOutput:=FLastInput;
              FLastContact:=FLastOutput;
              FLoginStatus:=0;
              needReadyToSend:=True;
              FNeedOutput:=True;
              needAfterLogin:=not FLoggedIn;
              FLoggedIn:=True;
              end;
            end
          else
            FInputState:=ins_Recv;

          FLastInput:=GetAppRunTime;
          FLastContact:=FLastInput;
          FInputWasLost:=False;
          FInputReset:=0;
          if not FLoggedOut then FGatewayLost:=False;

          FTotalReceived:=FTotalReceived+length(Data);

          FCryIn.DeCryptEx(data);
          FBuffIn.AddEx(data);
          if length(data)>FNeedInput then
            FNeedInput:=0
          else
            Dec(FNeedInput,length(data));
        finally
          FCS.Release;
          end;

        if needAfterLogin then
          DoAfterLogin(Sender);
        if needReadyToSend then
          begin
          if not FDataWaiting then
            DoReadyToSend(Sender)
          else if not FBlocking then
            ClientOUT.PostEvent(ReadyToSendNow);
          end;

        Cli.Response.ExpectedBytes:=ProcessInput(Sender);
        end;
      end;

    if Cli.Response.Done and not FGatewayLost then
      begin
      FCS.Acquire;
      try
        FLastInput:=GetAppRunTime;
        FInputWasLost:=False;
        FInputReset:=0;
        if not FLoggedOut then FGatewayLost:=False;

        FInputReady:=False;
        FInputState:=ins_Done;

        if FLoginStatus=1 then
          begin
          contLoop:=False;
          FLoginStatus:=2;
          end
        else
          contLoop:=True;
      finally
        FCS.Release;
        end;

      if not FGatewayLost then
        if contLoop then
          begin
          // Continue loop ...
          DataIN.Request.Method:='POST';
          DataIN.Request.FileName:=FGATEURI_OUTPUT;
          DataIN.Post(True,Sender);
          // StartInputJob.Post(nil);
          end
        else
          begin
          DataOUT.Request.Method:='POST';
          DataOUT.Request.FileName:=FGATEURI_INPUTRESET;
          DataOUT.Post;
          end;
      end;
    end;
  end;

procedure TRtcHttpGateClient.InitInput;
  var
    obj:TObject;
    oid:LongWord;
  begin
  FCS.Acquire;
  try
    FInputReady:=False;
    if not assigned(FCryIn) then
      begin
      FCryIn:=TRtcCrypt.Create;
      FBuffIn:=TRtcHugeByteArray.Create;
      FBuffRcv:=tObjList.Create(16);
      FNeedInput:=0;
      end
    else
      begin
      FCryIn.Init;
      FBuffIn.Clear;
      oid:=FBuffRcv.search_min(obj);
      while oid>0 do
        begin
        if assigned(obj) then
          RtcFreeAndNil(obj);
        FBuffRcv.remove(oid);
        oid:=FBuffRcv.search_min(obj);
        end;
      FBuffRcv.removeall;
      FNeedInput:=0;
      end;
    if FLoggedIn or FGatewayLost then
      FInputState:=ins_Closed
    else
      FInputState:=ins_Connecting;
    FPingInCnt:=0;
    FLastInput:=GetAppRunTime;
    FLastDataIn:=0;
    FSpeedIn:=0;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.InitOutput;
  begin
  FCS.Acquire;
  try
    FOutputReady:=False;
    if not assigned(FCryOut) then
      begin
      FCryOut:=TRtcCrypt.Create;
      FBuffOut:=TRtcHugeByteArray.Create;
      end
    else
      begin
      FCryOut.Init;
      FBuffOut.Clear;
      end;
    FLeftToSend:=0;
    FDataWaiting:=False;
    FNeedOutput:=False;
    if FLoggedIn or FGatewayLost then
      FOutputState:=outs_Closed
    else
      FOutputState:=outs_Connecting;
    FPingOutCnt:=0;
    FLastOutput:=GetAppRunTime;
    FLastDataOut:=0;
    FSpeedOut:=0;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.User_LogIn(Sender:TRtcConnection);
  begin
  FAutoRetry:=0;

  if FLoggedIn or (FLoginStatus>0) then Exit;

  FCS.Acquire;
  try
    FOutputState:=outs_Connecting;
    FInputState:=ins_Connecting;
    FPingOutCnt:=0;
    FPingInCnt:=0;

    FMyIP:='';
    FMyKey:='';
    FMyUID:=0;

    FEV.ResetEvent;

    FInputWasLost:=False;
    FLastInput:=GetAppRunTime;
    FLastContact:=FLastInput;
    FInputReset:=0;

    FOutputWasLost:=False;
    FLastOutput:=GetAppRunTime;
    FOutputReset:=0;

    FGatewayLost:=False;
    FLOggedOut:=False;

    FTotalSent:=0;
    FTotalReceived:=0;

    InitInput;
    InitOutput;
  finally
    FCS.Release;
    end;

  DoBeforeLogin(Sender);

  ClientIN.AutoConnect:=False;
  ClientOUT.AutoConnect:=False;

  FEV.SetEvent;
  ClientIN.Disconnect;
  ClientOUT.Disconnect;

  ClientIN.SkipRequests;
  ClientOUT.SkipRequests;

  SetupConnectionTimeouts(ClientIN);
  SetupConnectionTimeouts(ClientOUT);

  ClientIN.ServerAddr:=FGateAddr;
  ClientIN.ServerPort:=FGatePort;
  ClientIN.CryptPlugin:=FCryptPlugin;
  ClientIN.Blocking:=FUseBlocking;
  ClientIN.UseProxy:=FUseProxy;
  ClientIN.UseWinHTTP:=FUseWinHTTP;
  ClientIN.UseSSL:=FUseSSL;
  ClientIN.MultiThreaded:=True;
  FUserLogin.AssignTo(ClientIN.UserLogin);

  ClientOUT.ServerAddr:=FGateAddr;
  ClientOUT.ServerPort:=FGatePort;
  ClientOUT.CryptPlugin:=FCryptPlugin;
  ClientOUT.Blocking:=FUseBlocking;
  ClientOUT.UseProxy:=FUseProxy;
  ClientOUT.UseWinHTTP:=FUseWinHTTP;
  ClientOUT.UseSSL:=FUseSSL;
  ClientOUT.MultiThreaded:=True;
  FUserLogin.AssignTo(ClientOUT.UserLogin);

  FBlocking:=(FCryptPlugin=nil) and (FUseProxy or FUseSSL or FUseWinHTTP);

  ClientIN.AutoConnect:=True;
  ClientOUT.AutoConnect:=True;

  FEV.ResetEvent;

  // This will call INPUTRESET after OUTPUTRESET
  FLoginStatus:=1;

  DataLogin.Request.Method:='GET';
  DataLogin.Request.FileName:=FGATEURI_PING;
  DataLogin.Post;
  end;

procedure TRtcHttpGateClient.User_LogOut(Sender:TRtcConnection);
  var
    fail:boolean;
  begin
  FAutoRetry:=0;

  FCS.Acquire;
  try
    if FLoggedOut then Exit;
    FGatewayLost:=True;
    FLoggedOut:=True;
  finally
    FCS.Release;
    end;

  fail:=not FLoggedIn;

  if fail then
    begin
    FCS.Acquire;
    try
      FInputReady:=False;
      FOutputReady:=False;
      FLoginStatus:=0;
      FInputState:=ins_Closed;
      FOutputState:=outs_Closed;
      FPingOutCnt:=0;
      FPingInCnt:=0;
    finally
      FCS.Release;
      end;
    end;

  FInputResetAt:=0;
  FOutputResetAt:=0;
  if Sender=ClientOUT then
    FOutputResetAt:=ClientOUT.Request.ContentOut
  else if Sender=ClientIN then
    FInputResetAt:=ClientIN.Response.ContentIn;

  if FLastInput>0 then
    FInputResetTime:=GetAppRunTime-FLastInput;
  if FLastOutput>0 then
    FOutputResetTime:=GetAppRunTime-FLastOutput;

  FLastError:='';
  if assigned(Sender) then
    if TRtcDataClient(Sender).Response.StatusText<>'OK' then
      begin
      FLastError:=TRtcDataClient(Sender).Response.StatusText;
      if FLastError<>'' then
        if Sender=ClientIN then
          FLastError:='#IN '+FLastError
        else if Sender=ClientOUT then
          FLastError:='#OUT '+FLastError;
      end;

  FLoggedIn:=False;
  FLoginStatus:=0;

  ClientIN.AutoConnect:=False;
  ClientOUT.AutoConnect:=False;

  FEV.SetEvent;
  ClientIN.Disconnect;
  ClientOUT.Disconnect;

  ClientIN.SkipRequests;
  ClientOUT.SkipRequests;

  FInputWasLost:=False;
  FInputReset:=0;

  FOutputWasLost:=False;
  FOutputReset:=0;

  InitInput;
  InitOutput;

  if fail then
    DoAfterLoginFail(Sender)
  else
    DoAfterLogout(Sender);

  if FAutoRelogin then
    Active:=True;
  end;

procedure TRtcHttpGateClient.UserStreamLost(Sender:TRtcConnection);
  var
    toPause:integer;
  begin
  FAutoRetry:=0;

  FCS.Acquire;
  try
    if FGatewayLost then Exit;
    FGatewayLost:=True;

    FLoginStatus:=99;
  finally
    FCS.Release;
    end;

  try
    FNeedLogIn:=not FLoggedIn;

    FInputResetAt:=0;
    FOutputResetAt:=0;
    if Sender=ClientOUT then
      FOutputResetAt:=ClientOUT.Request.ContentOut
    else if Sender=ClientIN then
      FInputResetAt:=ClientIN.Response.ContentIn;

    if FLastInput>0 then
      FInputResetTime:=GetAppRunTime-FLastInput;
    if FLastOutput>0 then
      FOutputResetTime:=GetAppRunTime-FLastOutput;

    FLastError:='';
    if assigned(Sender) then
      if TRtcDataClient(Sender).Response.StatusText<>'OK' then
        begin
        FLastError:=TRtcDataClient(Sender).Response.StatusText;
        if FLastError<>'' then
          if Sender=ClientIN then
            FLastError:='#IN '+FLastError
          else if Sender=ClientOUT then
            FLastError:='#OUT '+FLastError;
        end;

    if GetAppRunTime>FLastContact+CLIENTCHECKLOGOUT_TIMEOUT then
      begin
      User_LogOut(Sender);
      Exit;
      end;

    if Sender=ClientIN then
      begin
      if FInputWasLost then
        Inc(FInputReset);
      FInputWasLost:=True;
      end
    else if Sender=ClientOUT then
      begin
      if FOutputWasLost then
        Inc(FOutputReset);
      FOutputWasLost:=True;
      end
    else
      begin
      if FInputWasLost then
        Inc(FInputReset);
      if FOutputWasLost then
        Inc(FOutputReset);
      FInputWasLost:=True;
      FOutputWasLost:=True;
      end;

    toPause:=FInputReset+FOutputReset;

    ClientIN.AutoConnect:=False;
    ClientOUT.AutoConnect:=False;

    FEV.SetEvent;
    ClientIN.Disconnect;
    ClientOUT.Disconnect;

    ClientIN.SkipRequests;
    ClientOUT.SkipRequests;

    InitInput;
    InitOutput;

    FEV.ResetEvent;

    DoStreamReset(Sender);

    FLoginStatus:=99;
    FAutoRetry := toPause+1;

  finally
    if not FLoggedOut then FGatewayLost:=False;
    end;
  end;

procedure TRtcHttpGateClient.RetryLogin;
  begin
  FAutoRetry:=0;
  if FGatewayLost or FLoggedOut or (FLoginStatus<>99) then Exit;

  ClientIN.AutoConnect:=True;
  ClientOUT.AutoConnect:=True;

  if (FGateAddr<>ClientIN.ServerAddr) or
     (FGatePort<>ClientIN.ServerPort) or
     (FUseBlocking<>ClientIN.Blocking) or
     (FUseProxy<>ClientIN.UseProxy) or
     (FUseWinHTTP<>ClientIN.UseWinHTTP) or
     (FUseSSL<>ClientIN.UseSSL) or
     (FCryptPlugin<>ClientIN.CryptPlugin) or
     (not ClientIN.UserLogin.IsEqual(FUserLogin)) then
    begin
    if not (ClientIN.isConnecting or ClientOUT.isConnecting) then
      begin
      ClientIN.ServerAddr:=FGateAddr;
      ClientIN.ServerPort:=FGatePort;
      ClientIN.Blocking:=FUseBlocking;
      ClientIN.UseProxy:=FUseProxy;
      ClientIN.UseWinHTTP:=FUseWinHTTP;
      ClientIN.UseSSL:=FUseSSL;
      ClientIN.CryptPlugin:=FCryptPlugin;
      FUserLogin.AssignTo(ClientIN.UserLogin);

      ClientOUT.ServerAddr:=FGateAddr;
      ClientOUT.ServerPort:=FGatePort;
      ClientOUT.Blocking:=FUseBlocking;
      ClientOUT.UseProxy:=FUseProxy;
      ClientOUT.UseWinHTTP:=FUseWinHTTP;
      ClientOUT.UseSSL:=FUseSSL;
      ClientOUT.CryptPlugin:=FCryptPlugin;
      FUserLogin.AssignTo(ClientOUT.UserLogin);

      FBlocking:=(FCryptPlugin=nil) and (FUseProxy or FUseSSL or FUseWinHTTP);
      end;
    end;

  // Tell the Gateway to reset the encryptipn and buffers on both connection streams
  FLoginStatus:=1;

  if FNeedLogIn then
    begin
    FInputState:=ins_Connecting;
    FOutputState:=outs_Connecting;
    FPingOutCnt:=0;
    FPingInCnt:=0;
    FInputReady:=False;
    FOutputReady:=False;

    DataLogin.Request.Method:='GET';
    DataLogin.Request.FileName:=FGATEURI_PING;

    DataLogin.Post;
    end
  else
    begin
    DataIN.Request.Method:='POST';
    DataIN.Request.FileName:=FGATEURI_OUTPUTRESET;

    DataIN.Post;
    end;
  end;

function TRtcHttpGateClient.GetStreamBlockSizeIn: longword;
  begin
  Result:=FStreamBlockSizeIn;
  end;

function TRtcHttpGateClient.GetStreamBlockSizeOut: longword;
  begin
  Result:=FStreamBlockSizeOut;
  end;

procedure TRtcHttpGateClient.SetStreamBlockSizeIn(const Value: longword);
  begin
  FStreamBlockSizeIn:=Value;
  end;

procedure TRtcHttpGateClient.SetStreamBlockSizeOut(const Value: longword);
  begin
  FStreamBlockSizeOut:=Value;
  end;

function TRtcHttpGateClient.ProcessInput(Sender:TRtcConnection): int64;
  var
    len,loc:integer;
    xUserID:TGateUID;
    xCommand,xLength,xLeft:LongWord;
  begin
  Result:=0; // minimum packet

  FCS.Acquire;
  try
    if FNeedInput>0 then
      begin
      Result:=FNeedInput;
      if Result>MAX_INPACKET_SIZE then
        Result:=MAX_INPACKET_SIZE;
      Exit;
      end
    else
      FData.Bytes:=FBuffIn.GetEx;
  finally
    FCS.Release;
    end;

  len:=length(FData.Bytes);
  loc:=0;

  while (len >= loc+4) do // Have Command with UserID or GroupID
    begin
    xUserID:=Bytes2Int(FData.Bytes,loc);
    xCommand:=xUserID and CommandMask;
    xUserID:=xUserID and GroupIDMask;
    case xCommand of
      Cmd_SendAll,
      Cmd_SendFirst,
      Cmd_SendMore,
      Cmd_SendLast:
        begin
        if (len >= loc+8) then // Have Length
          begin
          xLength:=Bytes2IntCRC(FData.Bytes,loc+4);
          xLeft:=len-loc-8;
          if xLeft>=xLength then // Have the expected size
            begin
            case xCommand of
              Cmd_SendAll:   FData.Command:=gc_SendAll;
              Cmd_SendFirst: FData.Command:=gc_SendFirst;
              Cmd_SendMore:  FData.Command:=gc_SendMore;
              Cmd_SendLast:  FData.Command:=gc_SendLast;
              end;
            FData.GroupID:=xUserID and GroupNrMask;
            FData.UserID:=(xUserID and UserIDMask) shr UserIDShift;
            FData.Length:=xLength;
            FData.Location:=loc+8;
            DoDataReceived(Sender);
            Inc(loc,8);
            Inc(loc,xLength);
            end
          else
            begin
            Result:=xLength-xLeft;
            Break;
            end;
          end
        else
          begin
          Result:=(loc+8)-len;
          Break;
          end;
        end;
      Cmd_UserOn:
        begin
        if xUserID=(FMyUID shl UserIDShift) then
          FPingInCnt:=(FPingInCnt+1) mod 4
        else
          begin
          FData.GroupID:=xUserID and GroupNrMask;
          FData.UserID:=(xUserID and UserIDMask) shr UserIDShift;
          if FData.GroupID>0 then
            FData.Command:=gc_UserJoined
          else
            FData.Command:=gc_UserOnline;
          DoInfoReceived(Sender);
          end;
        Inc(loc,4);
        end;
      Cmd_UserOff,
      Cmd_UserIn,
      Cmd_UserOut:
        begin
        FData.GroupID:=xUserID and GroupNrMask;
        FData.UserID:=(xUserID and UserIDMask) shr UserIDShift;
        if FData.GroupID>0 then
          begin
          case xCommand of
            Cmd_UserIn:  FData.Command:=gc_JoinedUser;
            Cmd_UserOut: FData.Command:=gc_LeftUser;
            Cmd_UserOff: FData.Command:=gc_UserLeft;
            else
              begin
              FData.ErrCode:=xCommand;
              FData.Command:=gc_Error;
              end;
            end;
          end
        else
          begin
          case xCommand of
            Cmd_UserOff: FData.Command:=gc_UserOffline;
            else
              begin
              FData.Command:=gc_Error;
              FData.ErrCode:=xCommand;
              end;
            end;
          end;
        if FData.Command=gc_Error then
          begin
          TRtcDataClient(Sender).Response.StatusText:=
            'Data Error: UID='+Int2Str(FData.UserID)+
            ', GID='+Int2Str(FData.GroupID)+
            ', Cmd='+Int2Str(xCommand);
          // Unrecoverable error
          UserStreamLost(Sender);

          DoInfoReceived(Sender);
          Exit;
          end
        else
          begin
          DoInfoReceived(Sender);
          Inc(loc,4);
          end;
        end;
      else
        begin
        FData.GroupID:=xUserID and GroupNrMask;
        FData.UserID:=(xUserID and UserIDMask) shr UserIDShift;
        FData.Command:=gc_Error;
        FData.ErrCode:=xCommand;

        // Unrecoverable error
        TRtcDataClient(Sender).Response.StatusText:=
          'Data Error: UID='+Int2Str(FData.UserID)+
          ', GID='+Int2Str(FData.GroupID)+
          ', Cmd='+Int2Str(xCommand);
        UserStreamLost(Sender);

        DoInfoReceived(Sender);
        Exit;
        end;
      end;
    end;

  if FGatewayLost then Exit;

  FCS.Acquire;
  try
    if loc>0 then
      FBuffIn.DelStart(loc);

    xLeft:=len-loc;
    if xLeft=0 then
      if FInputState=ins_Recv then
        FInputState:=ins_Idle;

    if Result=0 then
      Result:=4-xLeft;

    FNeedInput:=Result;
    if Result>MAX_INPACKET_SIZE then
      Result:=MAX_INPACKET_SIZE;
  finally
    FCS.Release;
    end;

  SetLength(FData.Bytes,0);
  SetLength(FData.Content,0);
  end;

function TRtcHttpGateClient.ReadyToSend(Sender: TRtcConnection): boolean;
  var
    lData,xLen:longword;
    data:RtcByteArray;
    Cli:TRtcDataClient absolute Sender;
  begin
  Result:=False;
  if FLoggedIn and (FLoginStatus=0) and FOutputReady and FSpaceToSend and not (FNowSending or FGatewayLost) then
    begin
    if not FDataWaiting then
      DoReadyToSend(Sender);

    if FDataWaiting then
      begin
      SetLength(data,0);

      FCS.Acquire;
      try
        if FBuffOut.Size>0 then
          begin
          xLen:=FReqToSend;
          if xLen>MAX_OUTPACKET_SIZE then
            xLen:=MAX_OUTPACKET_SIZE;
          if xLen<FBuffOut.Size then
            begin
            data:=FBuffOut.GetStartEx(xLen);
            FBuffOut.DelStart(xLen);
            end
          else
            begin
            data:=FBuffOut.GetEx;
            FBuffOut.Clear;
            end;
          FCryOut.CryptEx(data);

          FNowSending:=True;

          lData:=longword(length(data));
          FReqToSend:=FReqToSend-lData;
          FSpaceToSend:=FReqToSend>0;

          FTotalSent:=FTotalSent+lData;
          FLeftToSend:=FLeftToSend-lData;
          FDataWaiting:=FLeftToSend>0;

          FOutputState:=outs_Send;

          FLastOutput:=GetAppRunTime;

          if not (FDataWaiting or FGatewayLost) then
            FEV.ResetEvent;
          end;
      finally
        FCS.Release;
        end;

      if FGatewayLost then
        FEV.SetEvent
      else if length(data)>0 then
        begin
        Result:=True;
        Cli.WriteEx(data);
        end;
      end;
    end;
  end;

procedure TRtcHttpGateClient.ReadyToSendNow(Sender: TRtcConnection);
  begin
  ReadyToSend(Sender);
  end;

function TRtcHttpGateClient.PingUser(UserID: TGateUID):boolean;
  begin
  Result:=False;
  if Ready and (UserID<>MyUID) then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((UserID shl UserIDShift) or Cmd_UserOn));
      FLeftToSend:=FLeftToSend+4;
      FDataWaiting:=True;
      FNeedOutput:=True;
      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.AddUserToGroup(OwnerID, GroupID, UserID: TGateUID):boolean;
  begin
  Result:=False;
  if Ready then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((OwnerID shl UserIDShift) or GroupID or Cmd_AddUserToGroup));
      FBuffOut.AddEx(Int2Bytes(UserID shl UserIDShift));
      FLeftToSend:=FLeftToSend+8;
      FDataWaiting:=True;
      FNeedOutput:=True;
      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.RemoveGroup(OwnerID, GroupID: TGateUID):boolean;
  begin
  Result:=False;
  if Ready then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((OWnerID shl UserIDShift) or GroupID or Cmd_RemoveGroup));
      FLeftToSend:=FLeftToSend+4;
      FDataWaiting:=True;
      FNeedOutput:=True;
      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.RemUserFromGroup(OwnerID, GroupID, UserID: TGateUID):boolean;
  begin
  Result:=False;
  if Ready then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((OwnerID shl UserIDShift) or GroupID or Cmd_RemoveUserFromGroup));
      FBuffOut.AddEx(Int2Bytes(UserID shl UserIDShift));
      FLeftToSend:=FLeftToSend+8;
      FDataWaiting:=True;
      FNeedOutput:=True;
      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

procedure TRtcHttpGateClient.SetGateAddr(const Value: RtcString);
  begin
  FGateAddr := Value;
  end;

procedure TRtcHttpGateClient.SetGatePort(const Value: RtcString);
  begin
  FGatePort := Value;
  end;

procedure TRtcHttpGateClient.PingCheck;
  var
    MyTime:Cardinal;
    pinged:boolean;
  begin
  if FGatewayLost then Exit;

  MyTime:=GetAppRunTime;
  if FLoginStatus=99 then
    begin
    if FAutoRetry>0 then
      begin
      Dec(FAutoRetry);
      if FAutoRetry=0 then
        RetryLogin;
      end;
    end
  else if Ready and not (FNowSending or FDataWaiting or FOutputWasLost or FInputWasLost or FNeedOutput) then
    begin
    if MyTime>FLastOutput+SENDPING_INTERVAL then
      begin
      FCS.Acquire;
      try
        if not (FNowSending or FDataWaiting) then
          begin
          pinged:=True;
          FPingOutCnt:=(FPingOutCnt+1) mod 4;
          FBuffOut.AddEx(Int2Bytes((FMyUID shl UserIDShift) or Cmd_UserOn));
          Inc(FLeftToSend,4);
          FDataWaiting:=True;
          FEV.SetEvent;
          end
        else
          pinged:=False;
      finally
        FCS.Release;
        end;
      if pinged and not FBlocking then
        ClientOUT.PostEvent(ReadyToSendNow);
      end;
    end
  else if Ready then
    begin
    if (MyTime>FLastInput+CLIENTCHECKIN_TIMEOUT) and
       (MyTime>FLastOutput+CLIENTCHECKOUT_TIMEOUT) then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('INPUT and OUTPUT Timeout '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-FLastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-FLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
      ResetStreams;
      end
    else if MyTime>FLastInput+CLIENTCHECKMAXIN_TIMEOUT then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('INPUT Timeout '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-FLastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-FLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
      ResetStreams;
      end
    else if MyTime>FLastOutput+CLIENTCHECKMAXOUT_TIMEOUT then
      begin
      if LOG_GATECLIENT_TIMEOUTS then
        Log('OUTPUT Timeout '+Int2Str(MyUID)+
            ' ('+Float2Str((MyTime-FLastInput)/RUN_TIMER_PRECISION)+
            '/'+ Float2Str((MyTime-FLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
      ResetStreams;
      end;
    end
  else if MyTime>FLastInput+CLIENTCHECKLOGIN_TIMEOUT then
    begin
    if LOG_GATECLIENT_TIMEOUTS then
      Log('INPUT Timeout '+Int2Str(MyUID)+
          ' ('+Float2Str((MyTime-FLastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-FLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    ResetStreams;
    end
  else if MyTime>FLastOutput+CLIENTCHECKLOGIN_TIMEOUT then
    begin
    if LOG_GATECLIENT_TIMEOUTS then
      Log('OUTPUT Timeout '+Int2Str(MyUID)+
          ' ('+Float2Str((MyTime-FLastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-FLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    ResetStreams;
    end;
  end;

function TRtcHttpGateClient.GetActive: boolean;
  begin
  Result:=(FLoggedIn or (FLoginStatus>0)) and not FLoggedOut;
  end;

function TRtcHttpGateClient.GetReady: boolean;
  begin
  Result:=(FLoggedIn and (FLoginStatus=0)) and not (FLoggedOut or FGatewayLost);
  end;

procedure TRtcHttpGateClient.SetActive(const Value: boolean);
  begin
  if Value<>Active then
    if Value then
      User_LogIn(nil)
    else
      User_LogOut(nil);
  end;

function TRtcHttpGateClient.SendBytes(UserID, GroupID: TGateUID; CallID:word; const data: RtcByteArray):boolean;
  begin
  Result:=False;
  if Ready then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((UserID shl UserIDShift) or GroupID));
      FBuffOut.AddEx(Int2BytesCRC(2+length(data)));
      FBuffOut.AddEx(Word2Bytes(CallID));
      FBuffOut.AddPackEx(data,MAX_OUTPACKET_SIZE);

      FLeftToSend:=FLeftToSend+10+longword(length(data));
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

function TRtcHttpGateClient.SendBytes(UserID, GroupID: TGateUID; CallID:word):boolean;
  begin
  Result:=False;
  if Ready then
    begin
    FCS.Acquire;
    try
      FBuffOut.AddEx(Int2Bytes((UserID shl UserIDShift) or GroupID));
      FBuffOut.AddEx(Int2BytesCRC(2));
      FBuffOut.AddEx(Word2Bytes(CallID));

      FLeftToSend:=FLeftToSend+10;
      FDataWaiting:=True;
      FNeedOutput:=True;

      Result:=True;
      FEV.SetEvent;
    finally
      FCS.Release;
      end;

    if not FBlocking then
      ClientOUT.PostEvent(ReadyToSendNow);
    end;
  end;

constructor TRtcHttpGateClient.Create(AOwner:TComponent);
  begin
  inherited;
  ClientOUT := TRtcHttpClient.Create(nil);
  ClientIN := TRtcHttpClient.Create(nil);

  DataLogin := TRtcDataRequest.Create(nil);
  DataLogin.Client:=ClientIN;
  DataLogin.HyperThreading:=True;
  DataLogin.OnBeginRequest:=DataLoginBeginRequest;
  DataLogin.OnDataReceived:=DataLoginDataReceived;
  DataLogin.OnResponseAbort:=DataResponseAbort;

  DataIN := TRtcDataRequest.Create(nil);
  DataIN.Client:=ClientIN;
  DataIN.HyperThreading:=True;
  DataIN.OnBeginRequest:=DataINBeginRequest;
  DataIN.OnDataReceived:=DataINDataReceived;
  DataIN.OnResponseAbort:=DataResponseAbort;

  DataOUT := TRtcDataRequest.Create(nil);
  DataOUT.Client:=ClientOUT;
  DataOUT.HyperThreading:=True;
  DataOUT.OnBeginRequest:=DataOUTBeginRequest;
  DataOUT.OnDataReceived:=DataOUTDataReceived;
  DataOUT.OnDataSent:=DataOUTDataSent;
  DataOUT.OnResponseAbort:=DataResponseAbort;

  FUserGroups:=tBinList.Create(16);

  FUserLogin:=TRtcUserLoginInfo.Create;

  FGateClientLinks:=TRtcGateClientLinkList.Create;

  FData:=TRtcGateClientData.Create;

  FGateFileName:='/';

  FGATEURI_PING:='/'+GATEURI_PING;
  FGATEURI_LOGIN:='/'+GATEURI_LOGIN;
  FGATEURI_INPUT:='/'+GATEURI_INPUT;
  FGATEURI_OUTPUT:='/'+GATEURI_OUTPUT;
  FGATEURI_INPUTRESET:='/'+GATEURI_INPUTRESET;
  FGATEURI_OUTPUTRESET:='/'+GATEURI_OUTPUTRESET;

  FGATE_PRIMARY_KEY:='';
  FGATE_USERAUTH:='';
  FGATE_SECONDARY_KEY:='';

  FCS:=TRtcCritSec.Create;
  FEV:=TRtcEvent.Create(True,False);

  FBackground:=False;
  FUseBlocking:=False;
  FUseProxy:=False;
  FUseWinHTTP:=False;
  FUseSSL:=False;
  FCryptPlugin:=nil;

  FBlocking:=FUseProxy or FUseSSL or FUseWinHTTP;

  FLoggedIn:=False;
  FInputReady:=False;
  FOutputReady:=False;
  FLoggedOut:=True;
  FGatewayLost:=True;

  FInputState:=ins_Closed;
  FOutputState:=outs_Closed;
  FPingInCnt:=0;
  FPingOutCnt:=0;

  ReleaseAllGroupIDs;
  end;

destructor TRtcHttpGateClient.Destroy;
  begin
  FAutoRelogin:=False;

  User_LogOut(nil);

  Sleep(100);

  RemoveAllGateClientLinks;

  RtcFreeAndNil(FGateClientLinks);

  DataLogin.Client:=nil;
  DataIN.Client:=nil;
  DataOUT.Client:=nil;

  if assigned(FData) then
    begin
    SetLength(FData.Bytes,0);
    SetLength(FData.Content,0);
    RtcFreeAndNil(FData);
    end;

  RtcFreeAndNil(FUserLogin);

  RtcFreeAndNil(ClientOUT);
  RtcFreeAndNil(ClientIN);

  RtcFreeAndNil(DataLogin);
  RtcFreeAndNil(DataIN);
  RtcFreeAndNil(DataOUT);

  RtcFreeAndNil(FCryIn);
  RtcFreeAndNil(FCryOut);
  RtcFreeAndNil(FBuffIn);
  RtcFreeAndNil(FBuffOut);
  RtcFreeAndNil(FBuffRcv);

  RtcFreeAndNil(FUserGroups);

  RtcFreeAndNil(FCS);
  RtcFreeAndNil(FEV);

  inherited;
  end;

procedure TRtcHttpGateClient.SetUserGroupStatus(UserID, GroupID, Status: TGateUID);
  begin
  FCS.Acquire;
  try
    if Status>0 then
      begin
      if FUserGroups.search((UserID shl UserIDShift) or GroupID)<=0 then
        FUserGroups.insert((UserID shl UserIDShift) or GroupID,Status)
      else
        FUserGroups.change((UserID shl UserIDShift) or GroupID,Status);
      end
    else
      ClearUserGroupStatus(UserID,GroupID);
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.GetMinUserGroupID(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  FCS.Acquire;
  try
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    if (UID>0) and (UserID = UID shr UserIDShift) then
      Result:=UID - (UserID shl UserIDShift)
    else
      Result:=0;
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.GetMinUserGroupStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  Result:=0;
  FCS.Acquire;
  try
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    while (GID>0) and (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if (Result=0) or (GID<Result) then
        Result:=GID;
      UID:=FUserGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.GetMaxUserGroupStatus(UserID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  FCS.Acquire;
  try
    Result:=0;
    UID:=FUserGroups.search_ge(UserID shl UserIDShift, GID);
    while (UID>0) and (UserID = UID shr UserIDShift) do
      begin
      if GID>Result then
        Result:=GID;
      UID:=FUserGroups.search_g(UID, GID);
      end;
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.GetUserGroupStatus(UserID, GroupID: TGateUID): TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=FUserGroups.search((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.GetNextUserGroupStatus(var UserID, GroupID: TGateUID): TGateUID;
  var
    UID,GID:RtcIntPtr;
  begin
  FCS.Acquire;
  try
    Result:=0;
    UID:=FUserGroups.search_g((UserID shl UserIDShift) or GroupID, GID);
    if (UID>0) and (GID>0) then
      begin
      UserID:=UID shr UserIDShift;
      GroupID:=UID - (UserID shl UserIDShift);
      Result:=GID;
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.ClearAllUserGroupStates;
  begin
  FCS.Acquire;
  try
    if assigned(FUserGroups) then
      FUserGroups.removeall;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.ClearUserGroupStatus(UserID, GroupID: TGateUID);
  begin
  FCS.Acquire;
  try
    if FUserGroups.search((UserID shl UserIDShift) or GroupID)>0 then
      FUserGroups.remove((UserID shl UserIDShift) or GroupID);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.SetUseBlocking(const Value: boolean);
  begin
  FUseBlocking := Value;
  end;

procedure TRtcHttpGateClient.SetUseProxy(const Value: boolean);
  begin
  FUseProxy := Value;
  end;

procedure TRtcHttpGateClient.SetUseSSL(const Value: boolean);
  begin
  FUseSSL := Value;
  end;

procedure TRtcHttpGateClient.SetUseWinHTTP(const Value: boolean);
  begin
  FUseWinHTTP := Value;
  end;

procedure TRtcHttpGateClient.DoBeforeLogin(Sender: TRtcConnection);
  begin
  FGateClientLinks.DoBeforeLogIn(Sender,FCS);
  if assigned(FBeforeLogin) then
    FBeforeLogin(Sender);

  {$IFNDEF DISABLE_PING}
  FCS.Acquire;
  try
    if not assigned(FPingTimer) then
      FPingTimer:=TRtcTimer.Create(True);
    TRtcTimer.Enable(FPingTimer,PING_INTERVAL_inMS,PingCheck);
  finally
    FCS.Release;
    end;
  {$ENDIF}
  end;

procedure TRtcHttpGateClient.DoAfterLogin(Sender: TRtcConnection);
  begin
  FGateClientLinks.DoAfterLogIn(Sender,FCS);
  if assigned(FAfterLogin) then
    FAfterLogin(Sender);
  end;

procedure TRtcHttpGateClient.DoAfterLogout(Sender: TRtcConnection);
  begin
  {$IFNDEF DISABLE_PING}
  FCS.Acquire;
  try
    if assigned(FPingTimer) then
      TRtcTimer.Stop(FPingTimer);
  finally
    FCS.Release;
    end;
  {$ENDIF}
  FGateClientLinks.DoAfterLogOut(Sender,FCS);
  if assigned(FAfterLogout) then
    FAfterLogout(Sender);
  end;

procedure TRtcHttpGateClient.DoAfterLoginFail(Sender: TRtcConnection);
  begin
  {$IFNDEF DISABLE_PING}
  FCS.Acquire;
  try
    if assigned(FPingTimer) then
      TRtcTimer.Stop(FPingTimer);
  finally
    FCS.Release;
    end;
  {$ENDIF}
  FGateClientLinks.DoAfterLoginFail(Sender,FCS);
  if assigned(FAfterLoginFail) then
    FAfterLogInFail(Sender);
  end;

procedure TRtcHttpGateClient.DoDataReceived(Sender: TRtcConnection);
  var
    Buff:TRtcHugeByteArrayWithCallID;
    inBuff:boolean;
  begin
  case FData.Command of
    gc_SendAll:
      begin
      FData.ToBuffer:=False;
      FData.Header:=True;
      FData.Footer:=True;
      FData.Position:=0;

      FData.CallID:=Bytes2Word(FData.Bytes, FData.Location);
      if FData.Length>2 then
        FData.Content:=Copy(FData.Bytes, FData.Location+2, FData.Length-2)
      else
        SetLength(FData.Content,0);

      FGateClientLinks.DoDataReceived(Sender,FCS);
      if assigned(FOnDataReceived) then
        FOnDataReceived(Sender);

      SetLength(FData.Content,0);
      end;

    gc_SendFirst:
      begin
      FData.ToBuffer:=False;
      FData.Header:=True;
      FData.Footer:=False;
      FData.Position:=0;

      FData.CallID:=Bytes2Word(FData.Bytes, FData.Location);
      if FData.Length>2 then
        FData.Content:=Copy(FData.Bytes, FData.Location+2, FData.Length-2)
      else
        SetLength(FData.Content,0);

      FGateClientLinks.DoDataReceived(Sender,FCS);
      if assigned(FOnDataReceived) then
        FOnDataReceived(Sender);

      SetLength(FData.Content,0);

      if FData.ToBuffer then
        begin
        FCS.Acquire;
        try
          Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl GroupIDBits) or FData.GroupID));
          if assigned(Buff) then
            Buff.Clear
          else
            begin
            Buff:=TRtcHugeByteArrayWithCallID.Create;
            FBuffRcv.insert((FData.UserID shl GroupIDBits) or FData.GroupID,Buff);
            end;
          Buff.CallID:=FData.CallID;
          if FData.Length>2 then
            Buff.AddEx(FData.Bytes, FData.Length-2, FData.Location+2);
        finally
          FCS.Release;
          end;
        end;
      end;

    gc_SendMore:
      begin
      inBuff:=False;

      FCS.Acquire;
      try
        Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl GroupIDBits) or FData.GroupID));
        if assigned(Buff) then
          begin
          FData.ToBuffer:=True;
          FData.Header:=False;
          FData.Footer:=False;
          FData.Position:=Buff.Size;
          FData.CallID:=Buff.CallID;
          FData.Content:=Copy(FData.Bytes, FData.Location, FData.Length);

          Buff.AddEx(FData.Bytes, FData.Length, FData.Location);
          inBuff:=True;
          end
        else
          begin
          FData.ToBuffer:=False;
          FData.Header:=False;
          FData.Footer:=False;
          FData.Position:=0;
          FData.CallID:=0;
          FData.Content:=Copy(FData.Bytes, FData.Location, FData.Length);
          end;
      finally
        FCS.Release;
        end;

      FGateClientLinks.DoDataReceived(Sender,FCS);
      if assigned(FOnDataReceived) then
        FOnDataReceived(Sender);

      if inBuff and not FData.ToBuffer then
        begin // ToBuffer changed to FALSE? Stop buffering!
        FCS.Acquire;
        try
          Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl GroupIDBits) or FData.GroupID));
          if assigned(Buff) then
            begin
            FBuffRcv.remove((FData.UserID shl GroupIDBits) or FData.GroupID);
            RtcFreeAndNil(Buff);
            end;
        finally
          FCS.Release;
          end;
        end;

      SetLength(FData.Content,0);
      end;

    gc_SendLast:
      begin
      FCS.Acquire;
      try
        Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl GroupIDBits) or FData.GroupID));
        if assigned(Buff) then
          begin
          FData.Position:=Buff.Size;

          Buff.AddEx(FData.Bytes, FData.Length, FData.Location);
          FBuffRcv.remove((FData.UserID shl GroupIDBits) or FData.GroupID);

          FData.ToBuffer:=True;
          FData.Header:=True;
          FData.Footer:=True;
          FData.CallID:=Buff.CallID;
          FData.Content:=Buff.GetEx;

          RtcFreeAndNil(Buff);
          end
        else
          begin
          FData.ToBuffer:=False;
          FData.Header:=False;
          FData.Footer:=True;
          FData.Position:=0;
          FData.CallID:=0;
          FData.Content:=Copy(FData.Bytes, FData.Location, FData.Length);
          end;
      finally
        FCS.Release;
        end;

      FGateClientLinks.DoDataReceived(Sender,FCS);
      if assigned(FOnDataReceived) then
        FOnDataReceived(Sender);

      SetLength(FData.Content,0);
      end;
    end;
  end;

procedure TRtcHttpGateClient.DoInfoReceived(Sender: TRtcConnection);
  var
    Buff:TRtcHugeByteArrayWithCallID;
  begin
  FGateClientLinks.DoInfoReceived(Sender,FCS);
  if assigned(FOnInfoReceived) then
    FOnInfoReceived(Sender);

  if FData.Command=gc_LeftUser then
    begin
    // Clean up bufffer
    FCS.Acquire;
    try
      Buff:=TRtcHugeByteArrayWithCallID(FBuffRcv.search((FData.UserID shl GroupIDBits) or FData.GroupID));
      if assigned(Buff) then
        begin
        FBuffRcv.remove((FData.UserID shl GroupIDBits) or FData.GroupID);
        RtcFreeAndNil(Buff);
        end;
    finally
      FCS.Release;
      end;
    end;
  end;

procedure TRtcHttpGateClient.DoReadyToSend(Sender: TRtcConnection);
  var
    notify:boolean;
  begin
  FCS.Acquire;
  try
    if FNeedOutput then
      begin
      FNeedOutput:=False;
      notify:=True;
      end
    else
      notify:=False;
  finally
    FCS.Release;
    end;
  if notify then
    begin
    FGateClientLinks.DoReadyToSend(Sender,FCS);
    if assigned(FOnReadyToSend) then
      FOnReadyToSend(Sender);
    end;
  end;

procedure TRtcHttpGateClient.DoStreamReset(Sender: TRtcConnection);
  begin
  FGateClientLinks.DoStreamReset(Sender,FCS);
  if assigned(FOnStreamReset) then
    FOnStreamReset(Sender);
  end;

procedure TRtcHttpGateClient.SetGateFileName(const Value: RtcString);
  begin
  if Value<>FGateFileName then
    begin
    FGateFileName := Value;
    if Copy(FGateFileName,1,1)<>'/' then
      FGateFileName:='/'+FGateFileName;

    FGATEURI_PING:=FGateFileName+GATEURI_PING;
    FGATEURI_LOGIN:=FGateFileName+GATEURI_LOGIN;
    FGATEURI_INPUT:=FGateFileName+GATEURI_INPUT;
    FGATEURI_OUTPUT:=FGateFileName+GATEURI_OUTPUT;
    FGATEURI_INPUTRESET:=FGateFileName+GATEURI_INPUTRESET;
    FGATEURI_OUTPUTRESET:=FGateFileName+GATEURI_OUTPUTRESET;
    end;
  end;

procedure TRtcHttpGateClient.SetGatePrimaryKey(const Value: RtcString);
  begin
  FGATE_PRIMARY_KEY := Value;
  end;

procedure TRtcHttpGateClient.SetGateSecondaryKey(const Value: RtcString);
  begin
  FGATE_SECONDARY_KEY := Value;
  end;

procedure TRtcHttpGateClient.SetGateUserAuth(const Value: RtcString);
  begin
  FGATE_USERAUTH := Value;
  end;

procedure TRtcHttpGateClient.UserStreamReset(Data: TRtcValue);
  begin
  UserStreamLost(nil);
  end;

procedure TRtcHttpGateClient.ResetStreams;
  begin
  PostQuickJob(UserStreamReset,nil,not FBackground);
  end;

procedure TRtcHttpGateClient.AddGateClientLink(Value: TRtcAbsGateClientLink);
  begin
  FCS.Acquire;
  try
    FGateClientLinks.Add(Value);
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.RemoveAllGateClientLinks;
  var
    Link:TRtcAbsGateClientLink;
  begin
  FCS.Acquire;
  try
    while FGateClientLinks.Count>0 do
      begin
      Link:=TRtcAbsGateClientLink(FGateClientLinks.Get(0));
      Link.RemoveClient(self);
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.RemoveGateClientLink(Value: TRtcAbsGateClientLink);
  begin
  FCS.Acquire;
  try
    FGateClientLinks.Remove(Value);
  finally
    FCS.Release;
    end;
  end;

function TRtcHttpGateClient.AllocNextFreeGroupID: TGateUID;
  begin
  FCS.Acquire;
  try
    Result:=1;
    while FUsedGroupIDs[Result] do
      if Result<CntMaxGroups then
        Inc(Result)
      else
        raise ERtcGateClient.Create('All Group IDs are already in use.');
    FUsedGroupIDs[Result]:=True;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcHttpGateClient.ReleaseAllGroupIDs;
  begin
  FillChar(FUsedGroupIDs,SizeOf(FUsedGroupIDs),0);
  end;

procedure TRtcHttpGateClient.ReleaseGroupID(GroupID: TGateUID);
  begin
  FUsedGroupIDs[GroupID]:=False;
  end;

procedure TRtcHttpGateClient.AllocGroupID(GroupID: TGateUID);
  begin
  FUsedGroupIDs[GroupID]:=True;
  end;

procedure TRtcHttpGateClient.SetCryptPlugin(const Value: TRtcCryptPlugin);
  begin
  FCryptPlugin := Value;
  end;

procedure TRtcHttpGateClient.SetUserLogin(const Value: TRtcUserLoginInfo);
  begin
  if Value<>FUserLogin then
  	FUserLogin.Assign(Value);
  end;

function TRtcHttpGateClient.GetAutoRelogin: boolean;
  begin
  Result:=FAutoRelogin;
  end;

procedure TRtcHttpGateClient.SetAutoRelogin(const Value: boolean);
  begin
  if Value<>FAutoRelogin then
    begin
    FAutoRelogin:=Value;
    Active:=FAutoRelogin;
    end;
  end;

{ TRtcGateClientLink }

procedure TRtcGateClientLink.Call_AfterLogIn(Sender: TRtcConnection);
  begin
  if assigned(FAfterLogin) then
    FAfterLogIn(Sender);
  end;

procedure TRtcGateClientLink.Call_AfterLoginFail(Sender: TRtcConnection);
  begin
  if assigned(FAfterLoginFail) then
    FAfterLoginFail(Sender);
  end;

procedure TRtcGateClientLink.Call_AfterLogOut(Sender: TRtcConnection);
  begin
  if assigned(FAfterLogOut) then
    FAfterLogOut(Sender);
  end;

procedure TRtcGateClientLink.Call_BeforeLogIn(Sender: TRtcConnection);
  begin
  if assigned(FBeforeLogIn) then
    FBeforeLogIn(Sender);
  end;

procedure TRtcGateClientLink.Call_OnDataReceived(Sender: TRtcConnection);
  begin
  if assigned(FOnDataReceived) then
    FOnDataReceived(Sender);
  end;

procedure TRtcGateClientLink.Call_OnInfoReceived(Sender: TRtcConnection);
  begin
  if assigned(FOnInfoReceived) then
    FOnInfoReceived(Sender);
  end;

procedure TRtcGateClientLink.Call_OnReadyToSend(Sender: TRtcConnection);
  begin
  if assigned(FOnReadyToSend) then
    FOnReadyToSend(Sender);
  end;

procedure TRtcGateClientLink.Call_OnStreamReset(Sender: TRtcConnection);
  begin
  if assigned(FOnStreamReset) then
    FOnStreamReset(Sender);
  end;

end.

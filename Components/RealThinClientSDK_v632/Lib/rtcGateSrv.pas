{
  @html(<b>)
  RTC Gateway
  @html(</b>)
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)
  @html(<br><br>)

  This unit implements the RTC Gateway component.
}
unit rtcGateSrv;

interface

{$include rtcDefs.inc}

uses
  SysUtils,
  Classes,

  memObjList,

  rtcTypes,
  rtcInfo,
  rtcSyncObjs,
  rtcCrypt,

  rtcLog,

  rtcFastStrings,

  rtcConn,
  rtcDataSrv,
  rtcTimer,
  rtcThrPool,

  rtcGateConst;

type
  ERtcGateway = class(Exception);
  ERtcGateFatal = class(Exception);
  ERtcGateClosed = class(Exception);

  TRtcGatewayLoginEvent = procedure(Sender:TRtcConnection; UserID:TGateUID; const UserAuth:RtcString; var SecondaryKey:RtcString) of object;
  TRtcGatewayNotifyEvent = procedure(Sender:TRtcConnection; UserID:TGateUID; const UserAuth:RtcString) of object;

  { @Abstract(RTC Gateway component)
    Assign a TRtcHttpServer component to the "Server" property,
    configure the other published properties if necessary, and you
    have a fully functional Gateway attached to your Server. }
  {$IFDEF IDE_XE2up}
  [ComponentPlatformsAttribute(pidAll)]
  {$ENDIF}
  TRtcGateway = class(TRtcComponent)
  private
    FServer: TRtcDataServer;

    FBeforeUserLogin: TRtcGatewayLoginEvent;

    FOnUserReady,
    FOnUserNotReady,
    FBeforeUserLogout:TRtcGatewayNotifyEvent;

    FCS:TRtcCritSec;
    FStreams:TObjList;
    FUsers:TObjList;

    PingProvider: TRtcDataProvider;
    InputStream: TRtcDataProvider;
    OutputStream: TRtcDataProvider;
    LoginProvider: TRtcDataProvider;

    PingTimer:TRtcTimer;
    GateThread:TRtcThread;

    FGateFileName: RtcString;

    FGATE_MAXAUTHLEN:integer;

    FGATEURI_PING,
    FGATEURI_LOGIN,
    FGATEURI_INPUT,
    FGATEURI_OUTPUT,
    FGATEURI_INPUTRESET,
    FGATEURI_OUTPUTRESET,
    FGATEURI_STATUS,
    FGATE_PRIMARY_KEY:RtcString;

    procedure SetGateFileName(const Value: RtcString);
    procedure SetGatePrimaryKey(const Value: RtcString);
    procedure SetMaxAuthLen(const Value: integer);

    procedure SetServer(const Value: TRtcDataServer);

    procedure PingAllUsers;

    procedure PingProviderListenStart(Conn: TRtcConnection);
    procedure PingProviderListenStop(Conn: TRtcConnection);

    procedure PingProviderCheckRequest(Conn: TRtcConnection);

    procedure LoginProviderCheckRequest(Conn: TRtcConnection);
    procedure LoginProviderDataReceived(Conn: TRtcConnection);
    procedure LoginProviderResponseDone(Conn: TRtcConnection);
    procedure LoginProviderDisconnect(Conn: TRtcConnection);

    procedure InputStreamCheckRequest(Conn: TRtcConnection);
    procedure InputStreamDataReceived(Conn: TRtcConnection);
    procedure InputStreamResponseDone(Conn: TRtcConnection);
    procedure InputStreamDisconnect(Conn: TRtcConnection);

    procedure OutputStreamCheckRequest(Conn: TRtcConnection);
    procedure OutputStreamDataReceived(Conn: TRtcConnection);
    procedure OutputStreamDataSent(Conn: TRtcConnection);
    procedure OutputStreamResponseDone(Conn: TRtcConnection);
    procedure OutputStreamDisconnect(Conn: TRtcConnection);

    function GetNextUID:TGateUID;

    procedure CreateUser(Sender:TRtcDataServer; UserID:TGateUID; const UserAuth:RtcString; const CryptKey:RtcString);

    function SignalOpen(const Sender:TRtcConnection):boolean;

    function LockOpen(const Sender:TRtcConnection):boolean;
    procedure SignalClosed(const Sender:TRtcConnection);

    procedure UnLockOpen;

  protected

    // @exclude
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;

    // @exclude
    procedure DoRemoveAllUsers;
    // @exclude
    procedure DoRemoveUser(UserID:TGateUID);

    // @exclude
    procedure DoOpenInputStream(Conn:TRtcConnection);
    // @exclude
    procedure DoOpenOutputStream(Conn:TRtcConnection);

    // @exclude
    procedure DoDisconnect(Conn:TRtcConnection);

    // @exclude
    procedure DoDataReceived(Conn:TRtcConnection; CatchErrors:boolean=True);

    // @exclude
    procedure DoDataSent(Conn:TRtcConnection);

    // @exclude
    procedure DoCloseInputStream(Conn:TRtcConnection);
    // @exclude
    procedure DoResetInputStream(Conn:TRtcConnection);

    // @exclude
    procedure DoCloseOutputStream(Conn:TRtcConnection);
    // @exclude
    procedure DoResetOutputStream(Conn:TRtcConnection);

    // @exclude
    procedure DoStartSending(Conn:TRtcConnection);
    // @exclude
    procedure DoStartReceiving(Conn:TRtcConnection);

    // @exclude
    procedure DoSendPing(Data:TRtcValue);

    // @exclude
    procedure DoUserLogin(Conn: TRtcConnection);

    // @exclude
    procedure ErrorResponse(const Conn: TRtcConnection; const meth: String; const E: Exception);

    // @exclude
    procedure InternalAddUserToGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);
    // @exclude
    procedure InternalRemoveUserFromGroup(GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);
    // @exclude
    procedure InternalRemoveGroup(OwnerID, GroupID:TGateUID; NotifyUsers, NotifyOwner: boolean);

  public

    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;

    { Returns HTML-formatted information about this Gateway.
      Used in the "/check" request to generate a response for Web Browsers. }
    function CheckGatewayStatus:RtcString;

    { Use this method if you want to add User "UserID" to Group "GroupID" owned by user "OwnerID".
      If NotifyUser=True, the user will receive the info that he was added to the Group.
      If NotifyOwner=True, the Owner will receive the info that the user was added to its Group.

      This is a Gateway-side call, so Clients won't keep track of used GroupIDs opened by the Gateaway.
      To avoid GroupID clonflicts with Clients, use only high GroupIDs here. For example, from 255 to 201. }
    procedure AddUserToGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);

    { Use this method if you want to add User "UserID" to Group "GroupID" owned by user "OwnerID".
      If NotifyUser=True, user will receive the info that he was removed from the Group.
      If NotifyOwner=True, the Owner will receive the info that the user was removed from its Group. }
    procedure RemoveUserFromGroup(OwnerID, GroupID, UserID:TGateUID; NotifyUser, NotifyOwner: boolean);

    { Use this method if you want to remove Group "GroupID" owned by user "OwnerID".
      If NotifyUsers=True, all users in that group will receive the info that they were removed from the Group.
      If NotifyOwner=True, the Owner will receive the info about every user who was removed from the Group. }
    procedure RemoveGroup(OwnerID, GroupID:TGateUID; NotifyUsers, NotifyOwner: boolean);

  published

    // TRtcHttpServer component -> set before calling "Listen"
    property Server:TRtcDataServer read FServer write SetServer;

    // "Root" FileName for this Gateway component
    property GateFileName:RtcString read FGateFileName write SetGateFileName;

    // Primary Encryption Key
    property GatePrimaryKey:RtcString read FGATE_PRIMARY_KEY write SetGatePrimaryKey;

    { Maximum allowed length for UserAuth info sent from a Client.
      If MaxUserAuthLen=0, there is no limit (Gateway accepts anything from Clients) }
    property MaxUserAuthLen:integer read FGATE_MAXAUTHLEN write SetMaxAuthLen default 0;

    { Event called when a user is logging in.
      Here, you can check if the user is allowed to log in and set the "SecondaryKey" parameter
      to mach the "GateSecondaryKey" property set for the "TRtcHttpGateClient" component on the Client.

    INPUT:
      Sender = Connection object
      UserAuth = "GateUserAuth" String received from the Client
      UserID = UserID generated by the Gateway for this Client

    OUTPUT:
      SecondaryKey = Set this to the "GateSecondaryKey" parameter defined on the Client side (based on UserAuth).
                Leave empty if the Client should use the default encryption Key.
                Default encryption Key is generated from "GatePrimaryKey" and Client's IP address.

    DISALLOW ACCESS?
      In case the user may NOT log in, raise an exception here with the appropriate error message for the Client.
    }
    property BeforeUserLogin:TRtcGatewayLoginEvent read FBeforeUserLogin write FBeforeUserLogin;

    { Event called when the user is logged in and ready to receive data. }
    property OnUserReady:TRtcGatewayNotifyEvent read FOnUserReady write FOnUserReady;

    { Event called when the user is no longer ready to receive data. }
    property OnUserNotReady:TRtcGatewayNotifyEvent read FOnUserNotReady write FOnUserNotReady;

    { Event called before the User is logged out and its User ID becomes invalid (garbage collected). }
    property BeforeUserLogout:TRtcGatewayNotifyEvent read FBeforeUserLogout write FBeforeUserLogout;
    end;

implementation

const
  INFO_USERID = 'I';
  INFO_PAUSED = 'P';
  INFO_WAITING = 'W';
  INFO_SENDING = 'S';
  INFO_LEFT = 'L';
  INFO_CLOSED = 'C';
  INFO_RESET = 'R';
  INFO_DONE = 'D';

type
  // forward
  TRtcGateOneUser = class;

  TRtcGateUser = class(TObject)
  public
    function uSendFirst(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; TotalSize:LongWord;
                       const data:RtcByteArray;
                       len, loc:LongWord):integer; virtual; abstract;
    function uSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:LongWord;
                       const data:RtcByteArray;
                       len, loc:LongWord):integer; virtual; abstract;
    function uSendNote(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID):integer; virtual; abstract;
    function uPostNote(FromUID:TGateUID):integer; virtual; abstract;
    function uSendPing(FromUID:TGateUID):integer; virtual; abstract;
  end;

  TRtcGateOneUser = class(TRtcGateUser)
  protected
    (* Read-Only after LOGIN *)
    uUID:TGateUID;
    uOwnerIP:RtcString;
    MyGate:TRtcGateway;

    // UserAuth String received from the Client during Login
    uAuth:RtcString;

    (* INPUT Read/Write *)

    // Input encryption
    uCryptIn:TRtcCrypt;

    // User sending data to these Groups (Owner)
    uOwnGroups:TObjList;

    // Input Stream connection
    uInput:TRtcDataServer;
    uInputOK,
    uInputReady:boolean;

    uReceiveEvent:TRtcNotifyEvent;

    // Input Buffer
    uInBuff:TRtcHugeByteArray;

    // Senders that need to notify us
    uWaitingOnSenders:TObjList;

    // Last package received
    uLastInput:Cardinal;

    (* OUTPUT Read/Write *)

    // Last package was not sent completely?
    uSending:boolean;
    // Receiver is my Group
    uReceiverIsGroup:boolean;
    // User/Group waiting to receive more data
    uReceiver,
    uSender:TGateUID;
    // Number of bytes that need to be sent
    uLeftToSend:LongWord;

    // Packet being sent is still valid (all parts sent so far)
    uPacketValid:boolean;

    // Output encryption
    uCryptOut:TRtcCrypt;

    // User receiving data for these Groups
    uInGroups:TObjList;

    // Output Stream connection
    uOutput:TRtcDataServer;
    uOutputOK,
    uOutputReady:boolean;

    // Users that need to be notified
    // when OnDataSent event is fired
    // on the Output (ready for more data)
    uWaitingReceivers:TObjList;

    uOutBuff:TRtcHugeByteArray;
    uOutputNotified:boolean;

    // Last package sent
    uLastOutput,
    uLastPing:Cardinal;

    uSendEvent:TRtcNotifyEvent;

  public
    constructor Create(NotifyReceive:TRtcNotifyEvent;
                       NotifySend:TRtcNotifyEvent;
                       UID:TGateUID;
                       const UserAuth:RtcString;
                       const ForIP:RtcString;
                       const CryptKey:RtcString;
                       Gate:TRtcGateway);
    destructor Destroy; override;

    procedure uNotifyTheSender(UID:TGateUID);
    procedure uNotifySenders;

    function uSendFirst(oFromUser:TRtcGateOneUser;
                        SenderUID,FromUID:TGateUID; TotalSize:LongWord;
                        const data:RtcByteArray;
                        len, loc:LongWord):integer; override;
    function uSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:LongWord;
                       const data:RtcByteArray;
                       len, loc:LongWord):integer; override;
    function uSendNote(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID):integer; override;
    function uPostNote(FromUID:TGateUID):integer; override;
    function uSendPing(FromUID:TGateUID):integer; override;

    procedure uNotifyTheReceiver(UID:TGateUID);
    procedure uNotifyReceivers;

    procedure uNewWaitingReceiver(ReceiverUID:TGateUID; oReceiver:TRtcGateOneUser);
    procedure uNotifyRemovalFromGroup(gid:TGateUID; Notify:boolean);

    procedure uRemoveAllGroupsOwnedByUser(NotifyUsers, NotifyOwner: boolean);
    procedure uRemoveUserFromAllGroups(NotifyUser, NotifyOwners: boolean);
    procedure uRemoveUserFromGroup(GroupID: TGateUID; NotifyUser, NotifyOwner: boolean);
    procedure uRemoveGroup(GroupID: TGateUID; NotifyUsers, NotifyOwner:boolean);

    function uOpenInput(Conn:TRtcConnection; EVOn, EVOff:TRtcGatewayNotifyEvent):boolean;
    procedure uCloseInput(Conn:TRtcConnection);
    function uResetInput(Conn:TRtcConnection; EVOff:TRtcGatewayNotifyEvent):boolean;

    function uOpenOutput(Conn:TRtcConnection; EVOn, EVOff:TRtcGatewayNotifyEvent):boolean;
    procedure uDataSent(Conn:TRtcConnection);
    procedure uCloseOutput(Conn:TRtcConnection);
    function uResetOutput(Conn:TRtcConnection; EVOff:TRtcGatewayNotifyEvent):boolean;

    function uResetInOutStreams(EVOff:TRtcGatewayNotifyEvent):boolean;
  end;

  TRtcGateUserGroup=class(TRtcGateUser)
  protected
    gUID:TGateUID;

    gOwner:TRtcGateOneUser;
    gUsers:TObjList;

  public
    constructor Create(Owner:TRtcGateOneUser; UID:TGateUID);
    destructor Destroy; override;

    function uSendFirst(oFromUser:TRtcGateOneUser;
                        SenderUID,FromUID:TGateUID; TotalSize:LongWord;
                        const data:RtcByteArray;
                        len, loc:LongWord):integer; override;
    function uSendMore(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID; LeftToSend:LongWord;
                       const data:RtcByteArray;
                       len, loc:LongWord):integer; override;
    function uSendNote(oFromUser:TRtcGateOneUser;
                       SenderUID,FromUID:TGateUID):integer; override;
    function uPostNote(FromUID:TGateUID):integer; override;
    function uSendPing(FromUID:TGateUID):integer; override;
  end;

function GetUserAddr(const Sender: TRtcDataServer): RtcString;
  begin
  if Sender.Request.ForwardedFor<>'' then
    Result:=Sender.PeerAddr+'/'+Sender.Request.ForwardedFor+'?'
  else
    Result:=Sender.PeerAddr+'!';
  end;

function GetUserID(const Sender: TRtcDataServer): TGateUID;
  var
    UID:RtcString;
    UIDB:RtcByteArray;
  begin
  try
    UID:=Sender.Request.Query['id'];
    if UID<>'' then
      begin
      UIDB:=Int2Bytes(Str2LWord(UID));
      DeCryptEx(UIDB,RtcStringToBytes(GetUserAddr(Sender)));
      Result:=Bytes2Int(UIDB);
      end
    else
      Result:=0;
  except
    on E:Exception do
      begin
      if LOG_GATEWAY_ERRORS then
        Log('GetUserID -> '+E.ClassName+':'+E.Message,'GATE.UID');
      Result:=0;
      end;
    end;
  end;

procedure RaiseError(const Sender: TRtcDataServer; txt: RtcString);
  begin
  raise ERtcGateway.Create(txt);
  end;

procedure RaiseFatalError(const Sender: TRtcDataServer; txt: RtcString);
  begin
  raise ERtcGateFatal.Create(txt);
  end;

procedure RaiseErrorClosed(const Sender: TRtcDataServer; txt: RtcString);
  begin
  raise ERtcGateClosed.Create(txt);
  end;

function TRtcGateway.SignalOpen(const Sender:TRtcConnection):boolean;
  begin
  Result:=False;
  FCS.Acquire;
  try
    if FStreams.search(RtcIntPtr(Sender))=nil then
      begin
      FStreams.insert(RtcIntPtr(Sender),Sender);
      Result:=True;
      end;
  finally
    if not Result then FCS.Release;
    end;
  end;

procedure TRtcGateway.SignalClosed(const Sender:TRtcConnection);
  begin
  // FCS.Acquire; --> call LockOpen first!!!
  try
    if FStreams.search(RtcIntPtr(Sender))=Sender then
      FStreams.remove(RtcIntPtr(Sender));
  finally
    FCS.Release;
    end;
  end;

function TRtcGateway.LockOpen(const Sender:TRtcConnection):boolean;
  begin
  Result:=False;
  FCS.Acquire;
  try
    if Sender=nil then
      Result:=False
    else
      Result:=FStreams.search(RtcIntPtr(Sender))=Sender;
  finally
    if not Result then
      FCS.Release;
    end;
  end;

procedure TRtcGateway.UnLockOpen;
  begin
  FCS.Release;
  end;

procedure TRtcGateway.ErrorResponse(const Conn: TRtcConnection; const meth: String; const E: Exception);
  var
    Sender:TRtcDataServer absolute Conn;
    err,lname:String;
  begin
  try
    if E is ERtcGateway then
      begin
      err:=E.Message;
      lname:='ERR.GATE';
      end
    else if E is ERtcGateFatal then
      begin
      err:=E.Message;
      lname:='ERR.GATEF';
      end
    else if E is ERtcGateClosed then
      begin
      err:=E.Message;
      lname:='ERR.GATEC';
      end
    else
      begin
      err:='('+E.ClassName+') '+E.Message;
      lname:='ERR.'+E.ClassName;
      end;
    if (Sender=nil) then
      begin
      if LOG_GATEWAY_ERRORS then
        Log(meth+' '+err,lname+'.SenderNIL');
      end
    else if not LockOpen(Sender) then
      begin
      if LOG_GATEWAY_ERRORS then
        Log(meth+' '+err,lname+'.NotOpen');
      end
    else
      try
        if E is ERtcGateClosed then
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err+' ('+IntToStr(GetUserID(Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #CLOSED!',lname);
          try
            Sender.PostEvent(DoDisconnect);
          except
            on E:Exception do
              if LOG_GATEWAY_ERRORS then
                Log('ErrorResponse - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
            end;
          end
        else if Sender.Response=nil then
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err,lname+'.NoResponse');
          end
        else if GetUserID(Sender)=0 then
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err,lname+'.NoUserID');
          end
        else if Sender.Response.Done then
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err,lname+'.ResponseDone');
          end
        else if Sender.Request.Complete and
            (Sender.Response.StatusCode=GATE_OK_CODE) and
            (Sender.Response.ContentOut=0) and
            (Sender.Response.ContentLength=0) then
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err+' ('+IntToStr(GetUserID(Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #REPLY',lname);
          if E is ERtcGateFatal then
            Sender.Response.Status(GATE_FATALERROR_CODE,GATE_FATALERROR_TEXT)
          else
            Sender.Response.Status(GATE_ERROR_CODE,GATE_ERROR_TEXT);
          Sender.Response.ContentLength:=length(err);
          Sender.Write(err);
          end
        else
          begin
          if LOG_GATEWAY_ERRORS then
            Log(meth+' '+err+' ('+IntToStr(GetUserID(Sender) shr UserIDShift)+' at '+ Sender.PeerAddr+':'+Sender.PeerPort+') - #CLOSE',lname);
          try
            Sender.PostEvent(DoDisconnect);
          except
            on E:Exception do
              if LOG_GATEWAY_ERRORS then
                Log('ErrorResponse - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
            end;
          end;
      finally
        UnlockOpen;
        end;
  except
    on EX:Exception do
      try
        if LOG_GATEWAY_ERRORS then
          Log('#ERROR-RESPONSE '+meth+' '+E.ClassName+':'+E.Message+' / '+EX.ClassName+':'+EX.Message,'ERR.REPLY');
      except
        on EX2:Exception do
          try
            if LOG_GATEWAY_ERRORS then
              Log('#ERROR-RESPONSE2 '+meth+' '+EX2.ClassName+':'+EX2.Message,'ERR.ERR');
          except
            try
              if LOG_GATEWAY_ERRORS then
                Log('#ERROR-RESPONSE3 '+meth,'ERR.ERR');
            except
              if LOG_GATEWAY_ERRORS then
                Log('#ERROR-RESPONSE4','ERR.ERR');
            end;
          end;
      end;
    end;
  end;

{ TRtcGateOneUser }

constructor TRtcGateOneUser.Create( NotifyReceive:TRtcNotifyEvent;
                                    NotifySend:TRtcNotifyEvent;
                                    UID:TGateUID;
                                    const UserAuth:RtcString;
                                    const ForIP:RtcString;
                                    const CryptKey:RtcString;
                                    Gate:TRtcGateway);
  begin
  inherited Create;

  MyGate:=Gate;
  uOwnerIP:=ForIP;

  uUID:=UID;
  uAuth:=UserAuth;

  uOutput:=nil;
  uOutputOK:=False;
  uOutputReady:=False;
  uLastOutput:=GetAppRunTime;

  uInput:=nil;
  uInputOK:=False;
  uInputReady:=False;
  uLastInput:=GetAppRunTime;

  uOutBuff:=TRtcHugeByteArray.Create;
  uInBuff:=TRtcHugeByteArray.Create;

  uSendEvent:=NotifySend;
  uWaitingReceivers:=tObjList.Create(32);

  uReceiveEvent:=NotifyReceive;
  uWaitingOnSenders:=tObjList.Create(32);

  uInGroups:=tObjList.Create(32);
  uOwnGroups:=tObjList.Create(32);

  uCryptOut:=TRtcCrypt.Create;
  uCryptIn:=TRtcCrypt.Create;

  // Can't receive messages yet
  uOutputNotified:=True;
  // Not sending anything
  uSending:=False;

  uCryptIn.Key:=CryptKey;
  uCryptIn.Init;

  uCryptOut.Key:=CryptKey;
  uCryptOut.Init;
  end;

destructor TRtcGateOneUser.Destroy;
  begin
  MyGate:=nil;

  uOutput:=nil;
  uOutputOK:=False;
  uOutputReady:=False;
  uSendEvent:=nil;

  uInput:=nil;
  uInputOK:=False;
  uInputReady:=False;
  uReceiveEvent:=nil;

  uOutBuff.Free;
  uInBuff.Free;

  uInGroups.Free;
  uWaitingReceivers.Free;

  uOwnGroups.Free;
  uWaitingOnSenders.Free;

  uCryptOut.Free;
  uCryptIn.Free;
  inherited;
  end;

procedure TRtcGateOneUser.uNotifyTheSender(UID:TGateUID);
  begin
  if uWaitingReceivers.search(UID)<>nil then
    uWaitingReceivers.remove(UID);
  end;

procedure TRtcGateOneUser.uNotifySenders;
  var
    sndID:TGateUID;
    objSnd:TObject;
    oSend:TRtcGateOneUser absolute objSnd;
  begin
  sndID:=uWaitingOnSenders.search_min(objSnd);
  while sndID>0 do
    begin
    uWaitingOnSenders.remove(sndID);
    if objSnd is TRtcGateOneUser then
      oSend.uNotifyTheSender(uUID);
    sndID:=uWaitingOnSenders.search_g(sndID,objSnd);
    end;
  end;

procedure TRtcGateOneUser.uNotifyTheReceiver(UID:TGateUID);
  var
    c:TRtcConnection;
  begin
  if uWaitingOnSenders.search(UID)<>nil then
    begin
    uWaitingOnSenders.remove(UID);
    if uWaitingOnSenders.Empty then
      begin
      c:=uInput;
      if assigned(c) then
        c.PostEvent(uReceiveEvent);
        // c.PostEventTo(MyGate.GateThread,uReceiveEvent);
        // uReceiveEvent(c);
      end;
    end;
  end;

procedure TRtcGateOneUser.uNotifyReceivers;
  var
    recID:TGateUID;
    objRec:TObject;
    oRecv:TRtcGateOneUser absolute objRec;
  begin
  recID:=uWaitingReceivers.search_min(objRec);
  while recID>0 do
    begin
    uWaitingReceivers.remove(recID);
    if objRec is TRtcGateOneUser then
      oRecv.uNotifyTheReceiver(uUID);
    recID:=uWaitingReceivers.search_g(recID,objRec);
    end;
  end;

procedure TRtcGateOneUser.uNewWaitingReceiver(ReceiverUID:TGateUID; oReceiver:TRtcGateOneUser);
  begin
  if uWaitingOnSenders.search(ReceiverUID)=nil then
    uWaitingOnSenders.insert(ReceiverUID, oReceiver);
  end;

function TRtcGateOneUser.uSendFirst(oFromUser:TRtcGateOneUser;
                                  SenderUID, FromUID: TGateUID; TotalSize: LongWord;
                                  const data: RtcByteArray;
                                  len, loc: LongWord):integer;
  var
    c:TRtcConnection;
  begin
  if not (uOutputReady and uInputReady) then
    begin
    Result:=0;
    Exit;
    end;

  if TotalSize=len then
    uOutBuff.AddEx(Int2Bytes(FromUID or Cmd_SendAll))
  else
    uOutBuff.AddEx(Int2Bytes(FromUID or Cmd_SendFirst));
  uOutBuff.AddEx(Int2BytesCRC(len));
  uOutBuff.AddEx(data,len,loc);

  oFromUser.uNewWaitingReceiver(uUID,self);
  if uWaitingReceivers.search(SenderUID)=nil then
    uWaitingReceivers.insert(SenderUID,oFromUser);

  Result:=1;

  c:=uOutput;
  if not uOutputNotified and Assigned(c) then
    begin
    uOutputNotified:=True;
    // c.PostEvent(uSendEvent);
    c.PostEventTo(MyGate.GateThread,uSendEvent);
    // uSendEvent(c);
    end;
  end;

function TRtcGateOneUser.uSendMore(oFromUser:TRtcGateOneUser;
                                SenderUID,FromUID: TGateUID; LeftToSend: LongWord;
                                const data: RtcByteArray;
                                len, loc: LongWord):integer;
  var
    c:TRtcConnection;
  begin
  if not (uOutputReady and uInputReady) then
    begin
    Result:=0;
    Exit;
    end;

  if LeftToSend=len then
    uOutBuff.AddEx(Int2Bytes(FromUID or Cmd_SendLast))
  else
    uOutBuff.AddEx(Int2Bytes(FromUID or Cmd_SendMore));
  uOutBuff.AddEx(Int2BytesCRC(len));
  uOutBuff.AddEx(data,len,loc);

  oFromUser.uNewWaitingReceiver(uUID,self);
  if uWaitingReceivers.search(SenderUID)=nil then
    uWaitingReceivers.insert(SenderUID,oFromUser);

  Result:=1;

  c:=uOutput;
  if not uOutputNotified and assigned(c) then
    begin
    uOutputNotified:=True;
    // c.PostEvent(uSendEvent);
    c.PostEventTo(MyGate.GateThread,uSendEvent);
    // uSendEvent(c);
    end;
  end;

function TRtcGateOneUser.uSendNote(oFromUser:TRtcGateOneUser;
                                   SenderUID,FromUID: TGateUID):integer;
  var
    c:TRtcConnection;
  begin
  if not (uOutputReady and uInputReady) then
    begin
    Result:=0;
    Exit;
    end;

  uOutBuff.AddEx(Int2Bytes(FromUID));

  oFromUser.uNewWaitingReceiver(uUID,self);
  if uWaitingReceivers.search(SenderUID)=nil then
    uWaitingReceivers.insert(SenderUID, oFromUser);

  Result:=1;

  c:=uOutput;
  if not uOutputNotified and Assigned(c) then
    begin
    uOutputNotified:=True;
    // c.PostEvent(uSendEvent);
    c.PostEventTo(MyGate.GateThread,uSendEvent);
    // uSendEvent(c);
    end;
  end;

function TRtcGateOneUser.uPostNote(FromUID: TGateUID):integer;
  var
    c:TRtcConnection;
  begin
  if not (uOutputReady and uInputReady) then
    begin
    Result:=0;
    Exit;
    end;

  uOutBuff.AddEx(Int2Bytes(FromUID));
  Result:=1;

  c:=uOutput;
  if not uOutputNotified and Assigned(c) then
    begin
    uOutputNotified:=True;
    // c.PostEvent(uSendEvent);
    c.PostEventTo(MyGate.GateThread,uSendEvent);
    // uSendEvent(c);
    end;
  end;

function TRtcGateOneUser.uSendPing(FromUID:TGateUID):integer;
  var
    c:TRtcDataServer;
  begin
  c:=uOutput;
  if uOutputReady and uInputReady and not uOutputNotified and assigned(c) and (uOutBuff.Size=0) then
    begin
    Result:=1;
    uLastPing:=GetAppRunTime;
    uOutBuff.AddEx(Int2Bytes(FromUID));
    uOutputNotified:=True;
    // c.PostEvent(uSendEvent);
    c.PostEventTo(MyGate.GateThread,uSendEvent);
    // uSendEvent(c);
    end
  else
    Result:=0;
  end;

function TRtcGateOneUser.uOpenInput(Conn:TRtcConnection; EVOn, EVOff:TRtcGatewayNotifyEvent):boolean;
  var
    Sender:TRtcDataServer absolute Conn;
    toReset:boolean;
    MyTime:Cardinal;
    c:TRtcConnection;
  begin
  toReset:=Sender.Request.Info.asBoolean[INFO_RESET];
  MyTime:=GetAppRunTime;

  if uOwnerIP<>GetUserAddr(Sender) then
    RaiseFatalError(Sender,'Invalid User IP')
  else if (uInputOK=False) and not toReset then
    RaiseErrorClosed(Sender,'Input Stream was Lost')
  else if uOutputOK=False then
    RaiseErrorClosed(Sender,'Output Stream was Lost');

  if (uInputOK and not toReset) and Assigned(uInput) and (uInput<>Sender) and (uOutput<>Sender) then
    begin
    if LOG_GATEWAY_STATUS then
      Log('uOpenInput '+IntToStr(uUID shr UserIDShift)+' - Old Stream not closed, closing now.',GATE_LOG);
    c:=uInput;
    uInput:=nil;
    try
      c.PostEvent(MyGate.DoDisconnect);
    except
      on E:Exception do
        if LOG_GATEWAY_ERRORS then
          Log('uOpenInput - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
      end;
    end;
  Sender.Request.Info.asCardinal[INFO_USERID]:=uUID;

  Result:=uOutputReady and uInputReady;

  uInput:=Sender;
  uInputOK:=True;

  uInputReady:=not toReset;
  uLastInput:=MyTime;
  if toReset then
    begin
    uCryptIn.Init;
    uInBuff.Clear;
    uSending:=False;

    if Result then
      if assigned(EVOff) then
        EVOff(Conn,uUID shr UserIDShift,uAuth);

    Result:=uInputReady and uOutputReady;

    uRemoveAllGroupsOwnedByUser(True,False);
    uNotifySenders;
    end
  else if Result<>(uOutputReady and uInputReady) then
    begin
    Result:=not Result;
    if Result then
      begin
      if assigned(EVOn) then
        EVOn(Conn,uUID shr UserIDShift,uAuth);
      end
    else
      begin
      if assigned(EVOff) then
        EVOff(Conn,uUID shr UserIDShift,uAuth);
      end;
    end;
  end;

procedure TRtcGateOneUser.uNotifyRemovalFromGroup(gid:TGateUID; Notify:boolean);
  begin
  uInGroups.remove(gid);
  if Notify then
    uPostNote(gid or Cmd_UserOut);
  end;

procedure TRtcGateOneUser.uRemoveAllGroupsOwnedByUser(NotifyUsers, NotifyOwner: boolean);
  var
    gid,uid:TGateUID;
    objGr,objUsr:TObject;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUser:TRtcGateOneUser absolute objUsr;
  begin
  gid:=uOwnGroups.search_min(ObjGr);
  while gid>0 do
    begin
    if objGr is TRtcGateUserGroup then
      begin
      uid:=oGroup.gUsers.search_min(objUsr);
      while uid>0 do
        begin
        if objUsr is TRtcGateOneUser then
          begin
          oGroup.gUsers.remove(uid);

          oUser.uNotifyRemovalFromGroup(gid,
                (NotifyUsers and (uid<>uUID)) or (NotifyOwner and (uid=uUID)) );

          if NotifyOwner then
            uPostNote((gid and GroupNrMask) or uid or Cmd_UserOff);
          end;
        uid:=oGroup.gUsers.search_g(uid, objUsr);
        end;
      uOwnGroups.remove(gid);
      FreeAndNil(objGr);
      end;
    gid:=uOwnGroups.search_g(gid,ObjGr);
    end;
  end;

procedure TRtcGateOneUser.uRemoveUserFromAllGroups(NotifyUser, NotifyOwners:boolean);
  var
    gid,OwnerID:TGateUID;
    objGr,objOwn:TObject;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUserOwn:TRtcGateOneUser absolute objOwn;
  begin
  gid:=uInGroups.search_min(objGr);
  while gid>0 do
    begin
    if objGr is TRtcGateUserGroup then
      begin
      OwnerID:=gid and UserIDMask;
      oGroup.gUsers.remove(uUID);
      uNotifyRemovalFromGroup(gid,
              (NotifyUser and (uUID<>OwnerID)) or
              (NotifyOwners and (uUID=OwnerID)) );
      if NotifyOwners then
        begin
        objOwn:=oGroup.gOwner;
        if assigned(ObjOwn) and (objOwn is TRtcGateOneUser) then
          oUserOwn.uPostNote((gid and GroupNrMask) or uUID or Cmd_UserOff);
        end;
      end;
    gid:=uInGroups.search_g(gid,objGr);
    end;
  end;

procedure TRtcGateOneUser.uRemoveUserFromGroup(GroupID: TGateUID;
                                               NotifyUser, NotifyOwner: boolean);
  var
    objGr,objOwn:TObject;
    OwnerID:TGateUID;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUserOwn:TRtcGateOneUser absolute objOwn;
  begin
  if (GroupID and GroupIDMask)<>GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - Bad GroupID')
  else if (GroupID and UserIDMask)=GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - GroupID=OwnerID');

  OwnerID:=GroupID and UserIDMask;

  objGr:=uInGroups.search(GroupID);
  if assigned(objGr) and (objGr is TRtcGateUserGroup) then
    begin
    oGroup.gUsers.remove(uUID);
    uNotifyRemovalFromGroup(GroupID,
        (NotifyUser and (uUID<>OwnerID)) or
        (NotifyOwner and (uUID=OwnerID)) );
    if NotifyOwner then
      begin
      objOwn:=oGroup.gOwner;
      if assigned(ObjOwn) and (objOwn is TRtcGateOneUser) then
        oUserOwn.uPostNote((GroupID and GroupNrMask) or uUID or Cmd_UserOff);
      end;
    end;
  end;

procedure TRtcGateOneUser.uRemoveGroup(GroupID: TGateUID;
                                       NotifyUsers, NotifyOwner:boolean);
  var
    uid:TGateUID;
    objGr,objUsr:TObject;
    oUser:TRtcGateOneUser absolute objUsr;
    oGroup:TRtcGateUserGroup absolute objGr;
  begin
  objGr:=uOwnGroups.search(GroupID);
  if assigned(objGr) and (objGr is TRtcGateUserGroup) then
    begin
    uid:=oGroup.gUsers.search_min(objUsr);
    while uid>0 do
      begin
      if objUsr is TRtcGateOneUser then
        begin
        oGroup.gUsers.remove(uid);
        oUser.uNotifyRemovalFromGroup(GroupID,
              (NotifyUsers and (uid<>uUID)) or
              (NotifyOwner and (uid=uUID)) );
        if NotifyOwner then
          uPostNote((GroupID and GroupNrMask) or uid or Cmd_UserOff);
        end;
      uid:=oGroup.gUsers.search_g(uid, objUsr);
      end;
    uOwnGroups.remove(GroupID);
    FreeAndNil(objGr);
    end;
  end;

procedure TRtcGateOneUser.uCloseInput(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    MyTime:Cardinal;
  begin
  if uInput=Sender then
    begin
    uInput:=nil;
    MyTime:=GetAppRunTime;
    uLastInput:=MyTime;
    end;
  end;

function TRtcGateOneUser.uResetInput(Conn:TRtcConnection; EVOff:TRtcGatewayNotifyEvent):boolean;
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    MyTime:Cardinal;
    c:TRtcConnection;
  begin
  Result:=False;
  if uInput=Sender then
    begin
    uInput:=nil;
    if uOutput=Sender then uOutput:=nil;

    MyTime:=GetAppRunTime;
    uLastInput:=MyTime;

    if uInputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uInputOK:=False;
      uInputReady:=False;
      uInBuff.Clear;
      uSending:=False;

      if Result then
        if assigned(EVOff) then
          EVOff(Conn,uUID shr UserIDShift,uAuth);

      uRemoveAllGroupsOwnedByUser(True,False);
      uNotifySenders;
      end;

    if uOutputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uOutputNotified:=True;
      uLastOutput:=MyTime;
      uLastPing:=MyTime;

      uOutputOK:=False;
      uOutputReady:=False;
      uOutBuff.Clear;

      if Result then
        if assigned(EVOff) then
          EVOff(Conn,uUID shr UserIDShift,uAuth);

      uRemoveUserFromAllGroups(False,True);
      uNotifyReceivers;

      if assigned(uOutput) then
        begin
        if LOG_GATEWAY_STATUS then
          Log('uResetInput '+IntToStr(uUID shr UserIDShift)+' - OUTPUT Stream open, closing now',GATE_LOG);
        c:=uOutput;
        uOutput:=nil;
        try
          c.PostEvent(MyGate.DoDisconnect);
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              Log('uResetInput - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
          end;
        end;
      end;
    end;
  end;

type
  TRtcGateWriteJob=class(TRtcJob)
    Srv:TRtcDataServer;
    Dat:RtcByteArray;
    constructor Create(const Sender:TRtcDataServer; var Data:RtcByteArray); virtual;
    function Run(Thr:TRtcThread):boolean; override;
    procedure Kill; override;
    end;

{ TRtcGateWriteJob }

constructor TRtcGateWriteJob.Create(const Sender: TRtcDataServer; var Data: RtcByteArray);
  begin
  inherited Create;
  Srv:=Sender;
  Dat:=Data;
  SetLength(Data,0);
  end;

procedure TRtcGateWriteJob.Kill;
  begin
  inherited;
  SetLength(Dat,0);
  Srv:=nil;
  Free;
  end;

function TRtcGateWriteJob.Run(Thr: TRtcThread): boolean;
  begin
  try
    Srv.WriteEx(Dat);
  finally
    Srv:=nil;
    SetLength(Dat,0);
    Result:=True;
    end;
  end;

function TRtcGateOneUser.uOpenOutput(Conn:TRtcConnection; EVOn, EVOff:TRtcGatewayNotifyEvent):boolean;
  var
    Sender:TRtcDataServer absolute Conn;
    xLen,
    maxLen:LongWord;
    data:RtcByteArray;
    MyTime:Cardinal;
    toReset:boolean;
    c:TRtcConnection;
  begin
  toReset:=Sender.Request.Info.asBoolean[INFO_RESET];

  SetLength(data,0);
  if Sender.Request.ContentLength=0 then
    xLen:=0
  else
    xLen:=Str2LWordDef(Sender.Read, 0);
  if xLen=0 then
    if not toReset then
      xLen:=DEF_OUTSTREAM_SIZE;

  MyTime:=GetAppRunTime;

  if uOwnerIP<>GetUserAddr(Sender) then
    RaiseFatalError(Sender,'Invalid User IP')
  else if not toReset then
    begin
    if uOutputOK=False then
      RaiseErrorClosed(Sender,'Output Stream was Lost')
    else if uInputOK=False then
      RaiseErrorClosed(Sender,'Input Stream was Lost');
    end;

  if (uOutputOK and not toReset) and assigned(uOutput) and (uOutput<>Sender) and (uInput<>Sender) then
    begin
    if LOG_GATEWAY_STATUS then
      Log('uOpenOutput '+IntToStr(uUID shr UserIDShift)+' - Old Stream not closed yet, closing now.',GATE_LOG);
    c:=uOutput;
    uOutput:=nil;
    try
      c.PostEvent(MyGate.DoDisconnect);
    except
      on E:Exception do
        if LOG_GATEWAY_ERRORS then
          Log('uOpenOutput - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
      end;
    end;

  Result:=uInputReady and uOutputReady;

  uOutput:=Sender;
  uOutputOK:=True;
  uOutputReady:=not toReset;

  uOutputNotified:=True;
  uLastOutput:=MyTime;
  uLastPing:=MyTime;

  Sender.Response.ContentLength:=xLen;
  Sender.Request.Info.asCardinal[INFO_USERID]:=uUID;
  Sender.Request.Info.asBoolean[INFO_SENDING]:=True;

  if toReset then
    begin
    Sender.Request.Info.asLargeInt[INFO_LEFT]:=xLen-LongWord(length(data));

    uCryptOut.Init;
    uOutBuff.Clear;

    if Result then
      if assigned(EVOff) then
        EVOff(Conn,uUID shr UserIDShift,uAuth);

    Result:=uInputReady and uOutputReady;

    uRemoveUserFromAllGroups(False,True);
    uNotifyReceivers;
    end
  else
    begin
    if uOutBuff.Size=0 then
      uOutBuff.AddEx(Int2Bytes(uUID or Cmd_UserOn));
    if uOutBuff.Size>0 then // Data in Output Buffer
      begin
      maxLen:=xLen;
      if maxLen>MAX_OUTPACKET_SIZE then
        maxLen:=MAX_OUTPACKET_SIZE;
      if uOutBuff.Size<=maxLen then // Can send everything from Buffer
        begin
        data:=uOutBuff.GetEx;
        uOutBuff.Clear;
        uNotifyReceivers;
        end
      else // Can send part of the Buffer
        begin
        data:=uOutBuff.GetStartEx(maxLen);
        uOutBuff.DelStart(maxLen);
        end;
      end;
    Sender.Request.Info.asLargeInt[INFO_LEFT]:=xLen-LongWord(length(data));
    end;

  // Encrypt data before sending
  if length(data)>0 then
    uCryptOut.CryptEx(data);

  if length(data)>0 then
    begin
    Sender.WriteEx(data);
    SetLength(data,0);
    end
  else
    Sender.Write; // Send Header

  if Result<>(uOutputReady and uInputReady) then
    begin
    Result:=not Result;
    if Result then
      begin
      if assigned(EVOn) then
        EVOn(Conn,uUID shr UserIDShift,uAuth);
      end
    else
      begin
      if assigned(EVOff) then
        EVOff(Conn,uUID shr UserIDShift,uAuth);
      end;
    end;
  end;

procedure TRtcGateOneUser.uDataSent(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    xLen,
    maxLen:LongWord;
    data:RtcByteArray;
    MyTime:Cardinal;
    wjob:TRtcGateWriteJob;
  begin
  SetLength(data,0);

  xLen:=Sender.Request.Info.asLargeInt[INFO_LEFT];
  maxLen:=xLen;
  if maxLen=0 then
    Exit // Done with this response
  else if maxLen>MAX_OUTPACKET_SIZE then
    maxLen:=MAX_OUTPACKET_SIZE;

  MyTime:=GetAppRunTime;

  if (uOutputOK=False) then
    RaiseErrorClosed(Sender,'Output Stream was Lost')
  else if uOutput<>Sender then
    RaiseErrorClosed(Sender,'Output Stream not registered for Output')
  else if uInputOK=False then
    RaiseErrorClosed(Sender,'Input Stream was Lost');

  uLastOutput:=MyTime;

  if uOutBuff.Size>0 then // Data in Output Buffer
    begin
    uOutputNotified:=True;
    Sender.Request.Info.asBoolean[INFO_SENDING]:=True;
    if uOutBuff.Size<=maxLen then // Can send everything from Buffer
      begin
      data:=uOutBuff.GetEx;
      uOutBuff.Clear;
      uNotifyReceivers;
      end
    else // Can send part of the Buffer
      begin
      data:=uOutBuff.GetStartEx(maxLen);
      uOutBuff.DelStart(maxLen);
      end;
    uLastPing:=MyTime;
    end
  else
    begin
    uOutputNotified:=False;
    Sender.Request.Info.asBoolean[INFO_SENDING]:=False;
    end;

  // Encrypt data before sending
  if length(data)>0 then
    uCryptOut.CryptEx(data);

  Sender.Request.Info.asLargeInt[INFO_LEFT]:=xLen-LongWord(length(data));

  if length(data)>0 then
    begin
    // Sender.WriteEx(data);
    // SetLength(data,0);
    wjob:=TRtcGateWriteJob.Create(Sender,Data);
    if not Sender.PostJob(wjob) then
      wjob.Kill;
    end;
  end;

procedure TRtcGateOneUser.uCloseOutput(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    MyTime:Cardinal;
  begin
  if uOutput=Sender then
    begin
    uOutput:=nil;
    MyTime:=GetAppRunTime;
    uOutputNotified:=True;
    uLastOutput:=MyTime;
    uLastPing:=MyTime;
    end;
  end;

function TRtcGateOneUser.uResetOutput(Conn:TRtcConnection; EVOff:TRtcGatewayNotifyEvent):boolean;
  var
    Sender:TRtcDataServer absolute Conn;
    MyTime:Cardinal;
    c:TRtcConnection;
  begin
  Result:=False;
  if uOutput=Sender then
    begin
    if uInput=Sender then uInput:=nil;
    uOutput:=nil;
    MyTime:=GetAppRunTime;
    uOutputNotified:=True;
    uLastOutput:=MyTime;
    uLastPing:=MyTime;

    if uOutputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uOutputOK:=False;
      uOutputReady:=False;
      uOutBuff.Clear;

      if Result then
        if assigned(EVOff) then
          EVOff(Conn,uUID shr UserIDShift,uAuth);

      uRemoveUserFromAllGroups(False,True);
      uNotifyReceivers;
      end;

    if uInputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uInputOK:=False;
      uInputReady:=False;
      uInBuff.Clear;
      uSending:=False;

      if Result then
        if assigned(EVOff) then
          EVOff(Conn,uUID shr UserIDShift,uAuth);

      uRemoveAllGroupsOwnedByUser(True,False);
      uNotifySenders;
      if assigned(uInput) then
        begin
        if LOG_GATEWAY_STATUS then
          Log('uResetOutput '+IntToStr(uUID shr UserIDShift)+' - INPUT Stream open, closing now',GATE_LOG);
        c:=uInput;
        uInput:=nil;
        try
          c.PostEvent(MyGate.DoDisconnect);
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              Log('uResetOutput - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
          end;
        end;
      end;
    end;
  end;

function TRtcGateOneUser.uResetInOutStreams(EVOff:TRtcGatewayNotifyEvent):boolean;
  var
    c:TRtcConnection;
  begin
  Result:=False;
  try
    if uOutputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uOutputOK:=False;
      uOutputReady:=False;
      uOutBuff.Clear;

      if Result then
        if assigned(EVOff) then
          EVOff(nil,uUID shr UserIDShift,uAuth);

      uRemoveUserFromAllGroups(False,True);
      uNotifyReceivers;
      if assigned(uOutput) then
        begin
        if LOG_GATEWAY_STATUS then
          Log('uResetInOut '+IntToStr(uUID shr UserIDShift)+' - OUTPUT Stream open, closing now',GATE_LOG);
        if uInput=uOutput then uInput:=nil;
        c:=uOutput;
        uOutput:=nil;
        try
          c.PostEvent(MyGate.DoDisconnect);
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              Log('uResetInOut(OUT) - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
          end;
        end
      else
        if LOG_GATEWAY_STATUS then
          Log('uResetInOut '+IntToStr(uUID shr UserIDShift)+' - OUTPUT Stream was closed',GATE_LOG);
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('uResetInOut(OUT) '+E.ClassName+':'+E.Message+' ('+IntToStr(uUID shr UserIDShift)+')','ERROR');
    end;
  try
    if uInputOK then
      begin
      Result:=uInputReady and uOutputReady;

      uInputOK:=False;
      uInputReady:=False;
      uInBuff.Clear;
      uSending:=False;

      if Result then
        if assigned(EVOff) then
          EVOff(nil,uUID shr UserIDShift,uAuth);

      uRemoveAllGroupsOwnedByUser(True,False);
      uNotifySenders;
      if assigned(uInput) then
        begin
        if LOG_GATEWAY_STATUS then
          Log('uResetInOut '+IntToStr(uUID shr UserIDShift)+' - INPUT Stream open, closing now',GATE_LOG);
        if uOutput=uInput then uOutput:=nil;
        c:=uInput;
        uInput:=nil;
        try
          c.PostEvent(MyGate.DoDisconnect);
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              Log('uResetInOut(IN) - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
          end;
        end
      else
        if LOG_GATEWAY_STATUS then
          Log('uResetInOut '+IntToStr(uUID shr UserIDShift)+' - INPUT Stream was closed',GATE_LOG);
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('uResetInOut(IN) '+E.ClassName+':'+E.Message+' ('+IntToStr(uUID shr UserIDShift)+')','ERROR');
    end;
  end;

{ TGateUserGroup }

constructor TRtcGateUserGroup.Create(Owner:TRtcGateOneUser; UID:TGateUID);
  begin
  gUID:=UID;
  gOwner:=Owner;
  gUsers:=tObjList.Create(32);
  end;

destructor TRtcGateUserGroup.Destroy;
  begin
  gUsers.Free;
  gOwner:=nil;
  inherited;
  end;

function TRtcGateUserGroup.uSendFirst(oFromUser:TRtcGateOneUser;
                                      SenderUID,FromUID: TGateUID; TotalSize: LongWord;
                                      const data: RtcByteArray;
                                      len, loc: LongWord):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  oid:=gUsers.search_min(obj);
  while oid>0 do
    begin
    if obj is TRtcGateOneUser then
      Result:=Result+oUser.uSendFirst(oFromUser,SenderUID,FromUID,TotalSize,data,len,loc);
    oid:=gUsers.search_g(oid,obj);
    end;
  end;

function TRtcGateUserGroup.uSendMore(oFromUser:TRtcGateOneUser;
                                     SenderUID,FromUID: TGateUID; LeftToSend:LongWord;
                                     const data: RtcByteArray;
                                     len, loc: LongWord):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  oid:=gUsers.search_min(obj);
  while oid>0 do
    begin
    if obj is TRtcGateOneUser then
      Result:=Result+oUser.uSendMore(oFromUser,SenderUID,FromUID,LeftToSend,data,len,loc);
    oid:=gUsers.search_g(oid,obj);
    end;
  end;

function TRtcGateUserGroup.uSendNote(oFromUser:TRtcGateOneUser;
                                     SenderUID,FromUID: TGateUID):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  oid:=gUsers.search_min(obj);
  while oid>0 do
    begin
    if obj is TRtcGateOneUser then
      Result:=Result+oUser.uSendNote(oFromUser,SenderUID,FromUID);
    oid:=gUsers.search_g(oid,obj);
    end;
  end;

function TRtcGateUserGroup.uPostNote(FromUID: TGateUID):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  oid:=gUsers.search_min(obj);
  while oid>0 do
    begin
    if obj is TRtcGateOneUser then
      Result:=Result+oUser.uPostNote(FromUID);
    oid:=gUsers.search_g(oid,obj);
    end;
  end;

function TRtcGateUserGroup.uSendPing(FromUID:TGateUID):integer;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
  begin
  Result:=0;
  oid:=gUsers.search_min(obj);
  while oid>0 do
    begin
    if obj is TRtcGateOneUser then
      Inc(Result,oUser.uSendPing(FromUID));
    oid:=gUsers.search_g(oid,obj);
    end;
  end;

{ TRtcGateway }

constructor TRtcGateway.Create(AOwner:TComponent);
  begin
  inherited;
  Randomize;

  FGateFileName:='/';

  FGATEURI_STATUS:='/'+GATEURI_STATUS;
  FGATEURI_PING:='/'+GATEURI_PING;
  FGATEURI_LOGIN:='/'+GATEURI_LOGIN;
  FGATEURI_INPUT:='/'+GATEURI_INPUT;
  FGATEURI_OUTPUT:='/'+GATEURI_OUTPUT;
  FGATEURI_INPUTRESET:='/'+GATEURI_INPUTRESET;
  FGATEURI_OUTPUTRESET:='/'+GATEURI_OUTPUTRESET;

  FGATE_PRIMARY_KEY:='';

  FGATE_MAXAUTHLEN:=0;

  FCS:=TRtcCritSec.Create;
  FUsers:=tObjList.Create(32);
  FStreams:=TObjList.Create(32);

  FGateFileName:='/';

  PingProvider:=TRtcDataProvider.Create(nil);
  PingProvider.OnListenStart:=PingProviderListenStart;
  PingProvider.OnListenStop:=PingProviderListenStop;
  PingProvider.OnCheckRequest:=PingProviderCheckRequest;

  LoginProvider:=TRtcDataProvider.Create(nil);
  LoginProvider.OnCheckRequest:=LoginProviderCheckRequest;
  LoginProvider.OnDataReceived:=LoginProviderDataReceived;
  LoginProvider.OnResponseDone:=LoginProviderResponseDone;
  LoginProvider.OnDisconnect:=LoginProviderDisconnect;

  InputStream:=TRtcDataProvider.Create(nil);
  InputStream.OnCheckRequest:=InputStreamCheckRequest;
  InputStream.OnDataReceived:=InputStreamDataReceived;
  InputStream.OnResponseDone:=InputStreamResponseDone;
  InputStream.OnDisconnect:=InputStreamDisconnect;

  OutputStream:=TRtcDataProvider.Create(nil);
  OutputStream.OnCheckRequest:=OutputStreamCheckRequest;
  OutputStream.OnDataReceived:=OutputStreamDataReceived;
  OutputStream.OnDataSent:=OutputStreamDataSent;
  OutputStream.OnResponseDone:=OutputStreamResponseDone;
  OutputStream.OnDisconnect:=OutputStreamDisconnect;
  end;

destructor TRtcGateway.Destroy;
  begin
  DoRemoveAllUsers;

  Server:=nil;

  RtcFreeAndNil(PingProvider);
  RtcFreeAndNil(LoginProvider);
  RtcFreeAndNil(InputStream);
  RtcFreeAndNil(OutputStream);

  RtcFreeAndNil(FUsers);
  RtcFreeAndNil(FCS);
  RtcFreeAndNil(FStreams);

  inherited;
  end;

procedure TRtcGateway.PingProviderListenStart(Conn: TRtcConnection);
  begin
  GateThread:=TRtcThread.Create;
  PingTimer:=TRtcTimer.Create(Conn.MultiThreaded);
  TRtcTimer.Enable(PingTimer,PING_INTERVAL_inMS,PingAllUsers);
  end;

procedure TRtcGateway.PingProviderListenStop(Conn: TRtcConnection);
  begin
  TRtcTimer.Stop(PingTimer);
  PingTimer:=nil;

  TRtcThread.Stop(GateThread);
  GateThread:=nil;
  end;

procedure TRtcGateway.DoOpenInputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=GetUserID(Sender);
        if UserID=0 then Exit;

        obj:=FUsers.search(UserID);
        if obj=nil then
          RaiseFatalError(Sender,'User not registered')
        else if not (obj is TRtcGateOneUser) then
          RaiseFatalError(Sender,'ID does NOT belong to one User')
        else
          begin
          if Sender.Request.FileName<>FGATEURI_INPUT then
            Sender.Request.Info.asBoolean[INFO_RESET]:=True;
          oUser.uOpenInput(Sender,FOnUserReady,FOnUserNotReady);
          end;
      except
        on E:Exception do
          ErrorResponse(Sender,'OpenInput',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoStartReceiving(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if LockOpen(Sender) then
    try
      try
        if Sender.Request.Info.asBoolean[INFO_PAUSED] then
          begin
          Sender.Request.Info.asBoolean[INFO_PAUSED]:=False;
          if Sender.Request.Info.asBoolean[INFO_WAITING] then
            begin
            Sender.Request.Info.asBoolean[INFO_WAITING]:=False;
            DoDataReceived(Sender,False);
            end;
          end;
      except
        on E:Exception do
          ErrorResponse(Sender,'StartReceiving',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoDataReceived(Conn:TRtcConnection; CatchErrors:boolean=True);
  var
    Sender:TRtcDataServer absolute Conn;
    obj,objTo:TObject;
    oUser:TRtcGateOneUser absolute obj;
    oUserTo:TRtcGateUser absolute objTo;
    data:RtcByteArray;
    UserID:TGateUID;
    toPause:boolean;

  procedure ProcessData;
    var
      ldata,loc,len,xLeft,xLength:LongWord;
      xUserID,xToUserID:TGateUID;
      xCommand:LongWord;
      OK:boolean;
    begin
    if oUser.uSending then // Already sending a package
      begin
      if (oUser.uInBuff.Size>0) then
        begin
        oUser.uInBuff.AddEx(data);
        data:=oUser.uInBuff.GetEx;
        oUser.uInBuff.Clear;
        end;

      lData:=length(Data);
      if lData>=oUser.uLeftToSend then // Received the rest of the package
        begin
        if oUser.uPacketValid then
          begin
          if oUser.uReceiverIsGroup then // Receiver is my Group
            objTo:=oUser.uOwnGroups.search(oUser.uReceiver)
          else
            objTo:=FUsers.search(oUser.uReceiver);
          if assigned(objTo) and (objTo is TRtcGateUser) then
            begin
            if oUserTo.uSendMore(oUser, UserID, oUser.uSender, oUser.uLeftToSend, data, oUser.uLeftToSend, 0)>0 then
              toPause:=True
            else
              oUser.uPacketValid:=False;
            end
          else
            oUser.uPacketValid:=False;
          end;
        if oUser.uLeftToSend=lData then
          SetLength(data,0)
        else
          data:=Copy(data, oUser.uLeftToSend, lData-oUser.uLeftToSend); // data to be processed next
        oUser.uLeftToSend:=0;
        oUser.uReceiver:=0;
        oUser.uSender:=0;
        oUser.uSending:=False;
        oUser.uPacketValid:=False;
        end
      else if lData>=MIN_SPLITPACKET_SIZE then // received enough to send
        begin
        if oUser.uPacketValid then
          begin
          if oUser.uReceiverIsGroup then // Receiver is my Group
            objTo:=oUser.uOwnGroups.search(oUser.uReceiver)
          else
            objTo:=FUsers.search(oUser.uReceiver);
          if assigned(objTo) and (objTo is TRtcGateUser) then
            begin
            if oUserTo.uSendMore(oUser, UserID, oUser.uSender, oUser.uLeftToSend, data, lData, 0)>0 then
              toPause:=True
            else
              oUser.uPacketValid:=False;
            end
          else
            oUser.uPacketValid:=False;
          end;
        oUser.uLeftToSend:=oUser.uLeftToSend-lData;
        SetLength(data,0);
        end
      else
        begin
        oUser.uInBuff.AddEx(data);
        SetLength(data,0);
        end;
      end
    else if oUser.uInBuff.Size>0 then
      begin
      oUser.uInBuff.AddEx(data);
      data:=oUser.uInBuff.GetEx;
      oUser.uInBuff.Clear;
      end;

    len:=length(data);
    if len=0 then Exit;

    loc:=0;

    try
    while (len >= loc+4) do // Have Command with UserID or GroupID
      begin
      xUserID:=Bytes2Int(data,loc);
      xCommand:=xUserID and CommandMask;
      xUserID:=xUserID and GroupIDMask;
      case xCommand of
        Cmd_SendData:
          begin
          if (len >= loc+8) then // Have Length
            begin
            xLength:=Bytes2IntCRC(data,loc+4);
            xLeft:=len-loc-8;
            if (xLeft>=xLength) or // Have the complete package, or ...
               (xLeft>=MIN_SPLITPACKET_SIZE) then // Have at least the minimum split-packet size
              begin
              xToUserID:=xUserID;
              if (xUserID and UserIDMask)=xUserID then // Send to User
                begin
                objTo:=FUsers.search(xUserID);
                xUserID:=UserID;
                end
              else if (xUserID and UserIDMask)=UserID then // Send to my Group
                objTo:=oUser.uOwnGroups.search(xUserID);
              if assigned(objTo) and (objTo is TRtcGateUser) then
                begin
                if xLeft<xLength then // Do NOT have the complete package yet
                  begin
                  if oUserTo.uSendFirst(oUser, UserID, xUserID, xLength, data, xLeft, loc+8)>0 then
                    begin
                    toPause:=True;
                    oUser.uPacketValid:=True;
                    end
                  else
                    oUser.uPacketValid:=False;
                  end
                else
                  begin
                  if oUserTo.uSendFirst(oUser, UserID, xUserID, xLength, data, xLength, loc+8)>0 then
                    toPause:=True;
                  end;
                end
              else
                oUser.uPacketValid:=False;
              Inc(loc,8);
              if xLeft<xLength then // Did not get the whole package yet
                begin
                oUser.uSending:=True;
                oUser.uReceiver:=xToUserID;
                oUser.uReceiverIsGroup:=xUserID<>UserID;
                oUser.uSender:=xUserID;
                oUser.uLeftToSend:=xLength-xLeft;
                Inc(loc,xLeft);
                end
              else
                Inc(loc,xLength);
              end
            else
              Break;
            end
          else
            Break;
          end;
        Cmd_UserOn:
          begin
          if xUserID<>UserID then // my UserID = PING
            begin
            OK:=False;
            if (xUserID and UserIDMask)=xUserID then // Send to User
              begin
              objTo:=FUsers.search(xUserID);
              if assigned(objTo) and (objTo is TRtcGateUser) then
                ok:=oUserTo.uPostNote(UserID or Cmd_UserOn)>0;
              end
            else if (xUserID and UserIDMask)=UserID then // Send to my Group
              begin
              objTo:=oUser.uOwnGroups.search(xUserID);
              if assigned(objTo) and (objTo is TRtcGateUser) then
                ok:=oUserTo.uPostNote(xUserID or Cmd_UserOn)>0;
              end
            else
              ok:=True;
            if not OK then
              oUser.uPostNote((xUserID and UserIDMask) or Cmd_UserOff);
            end;
          Inc(loc,4);
          end;
        Cmd_UserOff:
          begin
          OK:=False;
          if (xUserID and UserIDMask)=xUserID then // Send to User
            begin
            objTo:=FUsers.search(xUserID);
            if assigned(objTo) and (objTo is TRtcGateUser) then
              ok:=oUserTo.uPostNote(UserID or Cmd_UserOff)>0;
            end
          else if (xUserID and UserIDMask)=UserID then // Send to my Group
            begin
            objTo:=oUser.uOwnGroups.search(xUserID);
            if assigned(objTo) and (objTo is TRtcGateUser) then
              ok:=oUserTo.uPostNote(xUserID or Cmd_UserOff)>0;
            end
          else
            ok:=True;
          if not OK then
            oUser.uPostNote((xUserID and UserIDMask) or Cmd_UserOff);
          Inc(loc,4);
          end;
        Cmd_AddUserToGroup:
          begin
          if (len >= loc+8) then
            begin
            xToUserID:=Bytes2Int(data,loc+4);
            InternalAddUserToGroup(UserID, xUserID, xToUserID, True, True);
            Inc(loc,8);
            end
          else
            Break;
          end;
        Cmd_RemoveUserFromGroup:
          begin
          if (len >= loc+8) then
            begin
            xToUserID:=Bytes2Int(data,loc+4);
            if (xToUserID=UserID) then // removing self?
              InternalRemoveUserFromGroup(xUserID, xToUserID, True, True)
            else if (xUserID and UserIDMask)=UserID then // removing from my Group?
              InternalRemoveUserFromGroup(xUserID, xToUserID, True, True);
            Inc(loc,8);
            end
          else
            Break;
          end;
        Cmd_RemoveGroup:
          begin
          if (xUserID and UserIDMask)=UserID then // removing one of MY Groups?
            InternalRemoveGroup(UserID, xUserID, True, True);
          Inc(loc,4);
          end;
        else
          RaiseError(Sender,'Input Stream error');
        end;
      end;
    except
      on E:Exception do
        begin
        if LOG_GATEWAY_ERRORS then
          Log('Len='+IntToStr(len)+', Loc='+IntToStr(loc)+', Err='+E.ClassName+':'+E.Message,'GATE.PROC');
        raise;
        end;
      end;

    xLeft:=len-loc;
    if xLeft>0 then
      oUser.uInBuff.AddEx(data,xLeft,loc);

    SetLength(data,0);
    end;

  begin
  if LockOpen(Sender) then
    try
      try
        if Sender.Request.Info.asBoolean[INFO_PAUSED] then
          begin
          Sender.Request.Info.asBoolean[INFO_WAITING]:=True;
          Exit;
          end;

        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;

        obj:=FUsers.search(UserID);
        if obj=nil then
          RaiseFatalError(Sender,'User not registered')
        else if not (obj is TRtcGateOneUser) then
          RaiseFatalError(Sender,'ID does NOT belong to one User')
        else if (oUser.uInputOK=False) then
          RaiseErrorClosed(Sender,'Connection was Lost')
        else if (oUser.uInput<>Sender) then
          RaiseErrorClosed(Sender,'Stream not registered for Input')
        else if oUser.uOutputOK=False then
          RaiseErrorClosed(Sender,'Output Stream was Lost');

        // Read data
        data:=Sender.ReadEx;
        if length(data)=0 then
          begin
          if Sender.Request.Complete then
            begin
            if Sender.Response.StatusCode=GATE_OK_CODE then
              begin
              if Sender.Response.ContentLength=0 then
                begin
                Sender.Response.ContentLength:=0;
                Sender.Write;
                end;
              DoCloseInputStream(Sender);
              end
            else
              DoResetInputStream(Sender);
            end;
          Exit; // nothing new
          end;

        Sender.Request.Info.asBoolean[INFO_PAUSED]:=True;

        toPause:=False;
        oUser.uLastInput:=GetAppRunTime;
        // Decrypt data
        oUser.uCryptIn.DeCryptEx(data);

        ProcessData;

        if not toPause then
          Sender.Request.Info.asBoolean[INFO_PAUSED]:=False;

        if Sender.Request.Complete then
          begin
          if Sender.Request.Info.asBoolean[INFO_PAUSED] then
            Sender.Request.Info.asBoolean[INFO_WAITING]:=True
          else
            begin
            if Sender.Response.StatusCode=GATE_OK_CODE then
              begin
              if Sender.Response.ContentLength=0 then
                begin
                Sender.Response.ContentLength:=0;
                Sender.Write;
                end;
              DoCloseInputStream(Sender);
              end
            else
              DoResetInputStream(Sender);
            end;
          end;
      except
        on E:Exception do
          begin
          if CatchErrors then
            ErrorResponse(Sender,'DataReceived',E)
          else
            raise;
        end;
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoCloseInputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;
        
        obj:=FUsers.search(UserID);
        if assigned(obj) and (obj is TRtcGateOneUser) then
          oUser.uCloseInput(Sender);
      except
        on E:Exception do
          ErrorResponse(Sender,'CloseInput',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoResetInputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;
        
        obj:=FUsers.search(UserID);
        if assigned(obj) and (obj is TRtcGateOneUser) then
          oUser.uResetInput(Sender,FOnUserNotReady);
      except
        on E:Exception do
          ErrorResponse(Sender,'ResetInput',E);
        end;
    finally
      UnLockOpen;
      end;
  end;

procedure TRtcGateway.DoOpenOutputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        if not Sender.Request.Complete then
          Exit
        else if Sender.Response.Done then
          begin
          if Sender.Response.StatusCode<>GATE_OK_CODE then
            begin
            if LOG_GATEWAY_STATUS then
              Log('OUTPUT OpenOutputStream '+IntToStr(GetUserID(Sender) shr UserIDShift)+' - Error Response sent, closing.',GATE_LOG);
            try
              Sender.PostEvent(DoDisconnect);
            except
              on E:Exception do
                if LOG_GATEWAY_ERRORS then
                  Log('OpenOutputStream - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
              end;
            end;
          Exit;
          end;

        UserID:=GetUserID(Sender);
        if UserID=0 then Exit;

        obj:=FUsers.search(UserID);
        if obj=nil then
          RaiseFatalError(Sender,'User not registered')
        else if not (obj is TRtcGateOneUser) then
          RaiseFatalError(Sender,'ID does NOT belong to one User')
        else
          begin
          if Sender.Request.FileName<>FGATEURI_OUTPUT then
            Sender.Request.Info.asBoolean[INFO_RESET]:=True;
          oUser.uOpenOutput(Sender,FOnUserReady,FOnUserNotReady);
          end;
      except
        on E:Exception do
          ErrorResponse(Sender,'OpenOutput',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoDataSent(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;

        obj:=FUsers.search(UserID);
        if obj=nil then
          RaiseFatalError(Sender,'User not registered')
        else if not (obj is TRtcGateOneUser) then
          RaiseFatalError(Sender,'ID does NOT belong to one User')
        else
          oUser.uDataSent(Sender);
      except
        on E:Exception do
          ErrorResponse(Sender,'DataSent',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoStartSending(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj, objRec:TObject;
    oUser:TRtcGateOneUser absolute obj;
    oRecv:TRtcGateOneUser absolute objRec;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        if not Sender.Request.Complete then
          Exit
        else if Sender.Response.Done then
          begin
          if Sender.Response.StatusCode<>GATE_OK_CODE then
            begin
            if LOG_GATEWAY_STATUS then
              Log('StartSending '+IntToStr(GetUserID(Sender) shr UserIDShift)+' - Error Response sent, closing.',GATE_LOG);
            try
              Sender.PostEvent(DoDisconnect);
            except
              on E:Exception do
                if LOG_GATEWAY_ERRORS then
                  Log('StartSending - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
              end;
            end;
          Exit;
          end;

        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;

        obj:=FUsers.search(UserID);
        if obj=nil then
          RaiseFatalError(Sender,'User not registered')
        else if not (obj is TRtcGateOneUser) then
          RaiseFatalError(Sender,'ID does NOT belong to one User')
        else if not Sender.Request.Info.asBoolean[INFO_SENDING] then
          oUser.uDataSent(Sender);
      except
        on E:Exception do
          ErrorResponse(Sender,'StartSending',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoCloseOutputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=GetUserID(Sender);
        if UserID=0 then Exit;
        
        obj:=FUsers.search(UserID);
        if (obj<>nil) and (obj is TRtcGateOneUser) then
          oUser.uCloseOutput(Sender);
      except
        on E:Exception do
          ErrorResponse(Sender,'CloseOutput',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.DoResetOutputStream(Conn:TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    UserID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        UserID:=Sender.Request.Info.asCardinal[INFO_USERID];
        if UserID=0 then Exit;
        
        obj:=FUsers.search(UserID);
        if assigned(obj) and (obj is TRtcGateOneUser) then
          oUser.uResetOutput(Sender,FOnUserNotReady);
      except
        on E:Exception do
          ErrorResponse(Sender,'ResetOutput',E);
        end;
    finally
      UnLockOpen;
      end;
  end;

function TRtcGateway.GetNextUID: TGateUID;
  var
    loop:boolean;
  begin
  loop:=false;
  repeat
    if loop then
      Result:= (MinLowID + TGateUID(Random(CntLowIDs))) shl UserIDShift
    else
      Result:= (MinHigID + TGateUID(Random(CntHigIDs))) shl UserIDShift;
    loop:=not loop;
    until FUsers.search(Result)=nil;
  end;

procedure TRtcGateway.CreateUser(Sender:TRtcDataServer;
                                 UserID:TGateUID;
                                 const UserAuth:RtcString;
                                 const CryptKey:RtcString);
  var
    obj:TRtcGateOneUser;
  begin
  obj:=TRtcGateOneUser.Create(DoStartReceiving,
                              DoStartSending,
                              UserID,
                              UserAuth,
                              GetUserAddr(Sender),
                              CryptKey,
                              self);
  if assigned(obj) then
    FUsers.insert(UserID,obj);
  end;

procedure TRtcGateway.DoRemoveUser(UserID: TGateUID);
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
  begin
  FCS.Acquire;
  try
    obj:=FUsers.search(UserID);
    if assigned(obj) and (obj is TRtcGateOneUser) then
      begin
      oUser.uRemoveUserFromAllGroups(False, True);
      oUser.uRemoveAllGroupsOwnedByUser(True, False);
      oUser.uNotifyReceivers;
      oUser.uNotifySenders;
      FUsers.remove(UserID);
      FreeAndNil(obj);
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateway.DoRemoveAllUsers;
  var
    obj:TObject;
    oUser:TRtcGateOneUser absolute obj;
    uid:TGateUID;
  begin
  FCS.Acquire;
  try
    uid:=FUsers.search_min(obj);
    while uid>0 do
      begin
      if obj is TRtcGateOneUser then
        begin
        oUser.uRemoveUserFromAllGroups(False, False);
        oUser.uRemoveAllGroupsOwnedByUser(False, False);
        oUser.uNotifyReceivers;
        oUser.uNotifySenders;
        FUsers.remove(uid);
        FreeAndNil(obj);
        end;
      uid:=FUsers.search_g(uid,obj);
      end;
  finally
    FCS.Release;
    end;
  end;

procedure TRtcGateway.InternalAddUserToGroup(OwnerID, GroupID, UserID: TGateUID;
                                     NotifyUser, NotifyOwner: boolean);
  var
    objGr,objUsr,objOwn:TObject;
    oOwner:TRtcGateOneUser absolute objOwn;
    oUser:TRtcGateOneUser absolute objUsr;
    oGroup:TRtcGateUserGroup absolute objGr;
  begin
  if (OwnerID and UserIDMask)<>OwnerID then
    raise ERtcGateway.Create('InternalAddUserToGroup - Bad OwnerID')
  else if (OwnerID=GroupID) then
    raise ERtcGateway.Create('InternalAddUserToGroup - GroupID=OwnerID')
  else if (OwnerID xor GroupID)>CntMaxGroups then
    raise ERtcGateway.Create('InternalAddUserToGroup - GroupID out of Range')
  else if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalAddUserToGroup - Bad UserID');

  objOwn:=FUsers.search(OwnerID);
  if assigned(objOwn) and (objOwn is TRtcGateOneUser) then
    begin
    objUsr:=FUsers.search(UserID);
    if assigned(objUsr) and (objUsr is TRtcGateOneUser) then
      begin
      if oUser.uInGroups.search(GroupID)=nil then
        begin
        if oUser.uOutputReady and oUser.uInputReady then
          begin
          objGr:=oOwner.uOwnGroups.search(GroupID);
          if not assigned(objGr) then
            begin
            objGr:=TRtcGateUserGroup.Create(oOwner,GroupID);
            if assigned(objGr) then
              oOwner.uOwnGroups.insert(GroupID,oGroup);
            end;
          if assigned(objGr) and (objGr is TRtcGateUserGroup) then
            begin
            if oGroup.gUsers.search(UserID)=nil then
              begin
              oGroup.gUsers.insert(UserID,objUsr);
              oUser.uInGroups.insert(GroupID,objGr);
              if (NotifyUser and (UserID<>OwnerID)) or
                 (NotifyOwner and (UserID=OwnerID)) then
                oUser.uPostNote(GroupID or Cmd_UserIn);
              if NotifyOwner then
                oOwner.uPostNote((GroupID and GroupNrMask) or UserID or Cmd_UserOn);
              end;
            end;
          end
        else if NotifyOwner then
          oOwner.uPostNote(UserID or Cmd_UserOff);
        end;
      end
    else if NotifyOwner then
      oOwner.uPostNote(UserID or Cmd_UserOff);
    end;
  end;

procedure TRtcGateway.InternalRemoveUserFromGroup(GroupID, UserID: TGateUID;
                                          NotifyUser, NotifyOwner: boolean);
  var
    objGr,objUsr,objOwn:TObject;
    oGroup:TRtcGateUserGroup absolute objGr;
    oUser:TRtcGateOneUser absolute objUsr;
    oUserOwn:TRtcGateOneUser absolute objOwn;
  begin
  if (GroupID and GroupIDMask)<>GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - Bad GroupID')
  else if (GroupID and UserIDMask)=GroupID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - GroupID=OwnerID')
  else if (UserID and UserIDMask)<>UserID then
    raise ERtcGateway.Create('InternalRemoveUserFromGroup - Bad UserID');

  objUsr:=FUsers.search(UserID);
  if assigned(objUsr) and (objUsr is TRtcGateOneUser) then
    oUser.uRemoveUserFromGroup(GroupID,NotifyUser,NotifyOwner);
  end;

procedure TRtcGateway.InternalRemoveGroup(OwnerID, GroupID: TGateUID;
                                  NotifyUsers, NotifyOwner:boolean);
  var
    objGr,objUsr,objOwn:TObject;
    oOwner:TRtcGateOneUser absolute objOwn;
    oUser:TRtcGateOneUser absolute objUsr;
    oGroup:TRtcGateUserGroup absolute objGr;
  begin
  if (OwnerID and UserIDMask)<>OwnerID then
    raise ERtcGateway.Create('InternalRemoveGroup - Bad OwnerID')
  else if (OwnerID=GroupID) then
    raise ERtcGateway.Create('InternalRemoveGroup - GroupID=OwnerID')
  else if (OwnerID xor GroupID)>CntMaxGroups then
    raise ERtcGateway.Create('InternalRemoveGroup - GroupID out of Range');

  objOwn:=FUsers.search(OwnerID);
  if assigned(objOwn) and (objOwn is TRtcGateOneUser) then
    oOwner.uRemoveGroup(GroupID,NotifyUsers,NotifyOwner);
  end;

procedure TRtcGateway.DoSendPing(Data:TRtcValue);
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
    MyTime:Cardinal;
  procedure DoResetInOut(const kind:RtcString);
    begin
    if LOG_GATEWAY_TIMEOUTS then
      Log(kind+' '+Int2Str(oUser.uUID shr UserIDShift)+
          ' ('+Float2Str((MyTime-oUser.uLastInput)/RUN_TIMER_PRECISION)+
          '/'+ Float2Str((MyTime-oUser.uLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
    oUser.uResetInOutStreams(FOnUserNotReady);
    end;
  begin
  try
    FCS.Acquire;
    try
      MyTime:=GetAppRunTime;
      oid:=FUsers.search_min(obj);
      while oid>0 do
        begin
        if obj is TRtcGateOneUser then
          begin
          if oUser.uOutputOK then
            begin
            if oUser.uInputOK then
              begin
              if oUser.uInputReady and oUser.uOutputReady then
                begin
                if (MyTime>oUser.uLastOutput+SENDPING_INTERVAL) and
                   (MyTime>oUser.uLastPing+SENDPING_INTERVAL) then
                  if oUser.uSendPing(oUser.uUID or Cmd_UserOn)=0 then
                    begin
                    if (MyTime>oUser.uLastInput+GATECHECKIN_TIMEOUT) and
                       (MyTime>oUser.uLastOutput+GATECHECKOUT_TIMEOUT) then
                      begin
                      DoResetInOut('INPUT and OUTPUT Timeout after LOGIN');
                      end
                    else if MyTime>oUser.uLastInput+GATECHECKMAXIN_TIMEOUT then
                      begin
                      DoResetInOut('INPUT Timeout after LOGIN');
                      end
                    else if MyTime>oUser.uLastOutput+GATECHECKMAXOUT_TIMEOUT then
                      begin
                      DoResetInOut('OUTPUT Timeout after LOGIN');
                      end;
                    end;
                end
              else if MyTime>oUser.uLastOutput+GATECHECKLOGIN_TIMEOUT then
                begin
                DoResetInOut('OUTPUT Timeout before LOGIN');
                end
              else if MyTime>oUser.uLastInput+GATECHECKLOGIN_TIMEOUT then
                begin
                DoResetInOut('INPUT Timeout before LOGIN');
                end;
              end
            else if MyTime>oUser.uLastOutput+GATECHECKLOGIN_TIMEOUT then
              begin
              DoResetInOut('OUTPUT Timeout before LOGIN');
              end;
            end
          else if oUser.uInputOK then
            begin
            if MyTime>oUser.uLastInput+GATECHECKLOGIN_TIMEOUT then
              begin
              DoResetInOut('INPUT Timeout before LOGIN');
              end;
            end
          else if (MyTime>oUser.uLastOutput+GATECHECKDONE_TIMEOUT) and
                  (MyTime>oUser.uLastInput+GATECHECKDONE_TIMEOUT) then
            begin
            if LOG_GATEWAY_TIMEOUTS then
              Log('USER Logged OUT and Removed '+Int2Str(oUser.uUID shr UserIDShift)+
                  ' ('+Float2Str((MyTime-oUser.uLastInput)/RUN_TIMER_PRECISION)+
                   '/'+Float2Str((MyTime-oUser.uLastOutput)/RUN_TIMER_PRECISION)+' s)',GATE_LOG);
            if assigned(FBeforeUserLogout) then
              FBeforeUserLogout(nil,oUser.uUID shr UserIDShift,oUser.uAuth);
            DoRemoveUser(oUser.uUID);
            end;
          end;
        oid:=FUsers.search_g(oid,obj);
        end;
    finally
      FCS.Release;
      end;
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('SendPing '+E.ClassName+':'+E.Message,'ERR.PING');
    end;
  end;

function TRtcGateway.CheckGatewayStatus:RtcString;
  var
    obj:TObject;
    oid:TGateUID;
    oUser:TRtcGateOneUser absolute obj;
    MyRes:RtcString;
    MyTime,MyTime2,MyTime3:Cardinal;
    a,User_Stat:integer;
    UsersTotal,
    ConnTotal:Cardinal;
    UsersCnt:array[0..16] of integer;
    UsersINB,
    UsersOUTB,
    UsersMAXIN,
    UsersMAXOUT,
    UsersMININ,
    UsersMINOUT:array[0..16] of Cardinal;
  const
    UserCaptions:array[0..16] of RtcString =
      ('OFFLINE',     'LOGIN (+/-)',  'LOGIN (-/+)',  'LOGIN (+/+)',
       'INPUT (-/-)', 'INPUT (+/-)',  'INPUT (-/+)',  'INPUT (+/+)',
       'OUTPUT (-/-)','OUTPUT (+/-)', 'OUTPUT (-/+)', 'OUTPUT (+/+)',
       'READY (-/-)', 'READY (+/-)',  'READY (-/+)',  'ACTIVE (+/+)',
       'TOTAL');
  begin
  for a:=0 to 16 do
    begin
    UsersCnt[a]:=0;
    UsersMAXIN[a]:=0;
    UsersMAXOUT[a]:=0;
    UsersMININ[a]:=$FFFFFFFF;
    UsersMINOUT[a]:=$FFFFFFFF;
    UsersINB[a]:=0;
    UsersOUTB[a]:=0;
    end;
  MyTime:=GetAppRunTime;
  FCS.Acquire;
  try
    MyTime2:=GetAppRunTime;

    UsersTotal:=FUsers.Count;
    ConnTotal:=FStreams.Count;

    try
      oid:=FUsers.search_min(obj);
      while oid>0 do
        begin
        User_Stat:=0;
        if obj is TRtcGateOneUser then
          begin
          if oUser.uInputOK then Inc(User_Stat);
          if oUser.uOutputOK then Inc(User_Stat,2);
          if oUser.uInputReady then Inc(User_Stat,4);
          if oUser.uOutputReady then Inc(User_Stat,8);
          if oUser.uLastInput>UsersMAXIN[User_Stat]   then UsersMAXIN[User_Stat]:=oUser.uLastInput;
          if oUser.uLastOutput>UsersMAXOUT[User_Stat] then UsersMAXOUT[User_Stat]:=oUser.uLastOutput;
          if oUser.uLastInput<UsersMININ[User_Stat]   then UsersMININ[User_Stat]:=oUser.uLastInput;
          if oUser.uLastOutput<UsersMINOUT[User_Stat] then UsersMINOUT[User_Stat]:=oUser.uLastOutput;
          UsersINB[User_Stat]:=UsersINB[User_Stat]+oUser.uInBuff.Size;
          UsersOUTB[User_Stat]:=UsersOUTB[User_Stat]+oUser.uOutBuff.Size;
          end;
        Inc(UsersCnt[User_Stat]);
        oid:=FUsers.search_g(oid,obj);
        end;
    except
      on E:Exception do
        ErrorResponse(nil,'CheckGatewayStatus',E);
      end;
  finally
    FCS.Release;
    end;
  for a:=0 to 15 do
    begin
    if UsersMAXIN[a]>UsersMAXIN[16]   then UsersMAXIN[16]:=UsersMAXIN[a];
    if UsersMAXOUT[a]>UsersMAXOUT[16] then UsersMAXOUT[16]:=UsersMAXOUT[a];
    if UsersMININ[a]<UsersMININ[16]   then UsersMININ[16]:=UsersMININ[a];
    if UsersMINOUT[a]<UsersMINOUT[16] then UsersMINOUT[16]:=UsersMINOUT[a];
    UsersCnt[16]:=UsersCnt[16]+UsersCnt[a];
    UsersINB[16]:=UsersINB[16]+UsersINB[a];
    UsersOUTB[16]:=UsersOUTB[16]+UsersOUTB[a];
    end;
  MyTime3:=GetAppRunTime;

  MyRes:='Clients: '+Int2Str(UsersTotal)+' <br>'#13#10+
         'Connections: '+Int2Str(ConnTotal)+' <br>'#13#10+
         'Lock Acquired: '+Float2Str((MyTime2-MyTime)/RUN_TIMER_PRECISION)+' s <br>'#13#10+
         'Scan Complete: '+Float2Str((MyTime3-MyTime)/RUN_TIMER_PRECISION)+' s <br>'#13#10+
         'Server Up-Time: '+Float2Str(MyTime3/RUN_TIMER_PRECISION)+' s <br>'#13#10;

  if UsersTotal>0 then
    begin
    MyRes:=MyRes+'<table border=1><tr><td> Client Status </td><td> Client Count </td><td> Input Buffer </td><td> Output Buffer </td><td> Last Input </td><td> Last Output </td></tr>'#13#10;

    for a:=0 to 16 do
      if UsersCnt[a]>0 then
        MyRes := MyRes + ' <tr><td> '+UserCaptions[a]+'</td><td>'+Int2Str(UsersCnt[a])+
                         ' </td><td> '+Int2Str(UsersINB[a] div 1024)+' KB'+
                         ' </td><td> '+Int2Str(UsersOUTB[a] div 1024)+' KB'+
                         ' </td><td> '+Float2Str((MyTime3-UsersMAXIN[a])/RUN_TIMER_PRECISION)+
                         ' - '+Float2Str((MyTime3-UsersMININ[a])/RUN_TIMER_PRECISION)+' s'+
                         ' </td><td> '+Float2Str((MyTime3-UsersMAXOUT[a])/RUN_TIMER_PRECISION)+
                         ' - '+Float2Str((MyTime3-UsersMINOUT[a])/RUN_TIMER_PRECISION)+
                         ' s </td></tr>'#13#10;
    MyRes:=MyRes+'</table>';
    end;

  Result:=MyRes;
  end;

procedure TRtcGateway.PingProviderCheckRequest(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    s:RtcString;
  begin
  if (Sender.Request.Method='GET') and
     (Sender.Request.ContentLength=0) then
    begin
    if (Sender.Request.FileName=FGATEURI_PING) then
      begin
      try
        Sender.Accept;
        Sender.Write(GetUserAddr(Sender));
        if LOG_GATEWAY_STATUS then
          Log(Sender.PeerAddr+' PING'{+#13#10+s},'CHECK');
      except
        on E:Exception do
          ErrorResponse(Sender,'PingProvider.Check_PING',E);
        end;
      end
    else if (Sender.Request.FileName=FGATEURI_STATUS) then
      begin
      try
        Sender.Accept;
        s:=CheckGatewayStatus;
        Sender.Write('<html><meta http-equiv="refresh" content="1"><body>'+s+'</body></html>');
        if LOG_GATEWAY_STATUS then
          Log(Sender.PeerAddr+' STATUS'{+#13#10+s},'CHECK');
      except
        on E:Exception do
          ErrorResponse(Sender,'PingProvider.Check_STATUS',E);
        end;
      end;
    end;
  end;

procedure TRtcGateway.LoginProviderCheckRequest(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if (Sender.Request.FileName=FGATEURI_LOGIN) and
     (Sender.Request.Method='POST') and
     ( (FGATE_MAXAUTHLEN<=0) or
       (Sender.Request.ContentLength<=FGATE_MAXAUTHLEN) ) then
    begin
    if SignalOpen(Conn) then
      try
        Sender.Accept;
      finally
        UnLockOpen;
      end
    else
      if LOG_GATEWAY_ERRORS then
        Log('LoginProvider.CheckRequest: '+Sender.Request.Method+' '+Sender.Request.URI,'ERR.SIGNAL');
    end
  end;

procedure TRtcGateway.LoginProviderDataReceived(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if LockOpen(Sender) then
    try
      try
        if Sender.Request.Complete then
          DoUserLogin(Sender);
      except
        on E:Exception do
          ErrorResponse(Sender,'LOGIN DataReceived',E);
        end;
    finally
      UnLockOpen;
      end;
  end;

procedure TRtcGateway.DoUserLogin(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
    auth,ccode,newKey:RtcString;
    res:RtcByteArray;
    UID:TGateUID;
  begin
  if LockOpen(Sender) then
    try
      try
        if not Sender.Request.Complete then Exit;

        ccode:=FGATE_PRIMARY_KEY;
        Crypt(ccode, GetUserAddr(Sender));

        auth:=Sender.Read;
        DeCrypt(auth, ccode);

        newKey:='';
        UID:=GetNextUID;

        try
          if assigned(FBeforeUserLogin) then
            FBeforeUserLogin(Sender,UID shr UserIDShift,auth,newKey);
        except
          on E:Exception do
            begin
            Sender.Response.Status(GATE_FATALERROR_CODE,GATE_FATALERROR_TEXT);
            Sender.Response.ContentLength:=length(E.Message);
            Sender.Write(E.Message);
            Exit;
            end;
          end;

        if newKey='' then
          newKey:=ccode
        else
          begin
          Crypt(ccode,newKey);
          newKey:=ccode;
          end;

        CreateUser(Sender,UID,auth,newKey);
        res:=Int2Bytes(UID);
        CryptEx(res,RtcStringToBytes(newKey));
        Sender.Response.ContentLength:=length(res);
        Sender.WriteEx(res);
      except
        on E:Exception do
          ErrorResponse(Sender,'UserLogin',E);
        end;
    finally
      UnlockOpen;
      end;
  end;

procedure TRtcGateway.LoginProviderResponseDone(Conn: TRtcConnection);
  begin
  if LockOpen(Conn) then
    SignalClosed(Conn);
  end;

procedure TRtcGateway.LoginProviderDisconnect(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if LockOpen(Conn) then
    try
      if LOG_GATEWAY_STATUS then
        Log('LOGIN Disconnect',GATE_LOG);
    finally
      SignalClosed(Conn);
      end;
  end;

procedure TRtcGateway.InputStreamCheckRequest(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if ( (Sender.Request.FileName=FGATEURI_INPUT) or (Sender.Request.FileName=FGATEURI_INPUTRESET) ) and
       (Sender.Request.Method='POST') then
    begin
    if SignalOpen(Conn) then
      try
        try
          Sender.Accept;
          Sender.Request.ManualRead:=True;
          DoOpenInputStream(Sender);
        except
          on E:Exception do
            ErrorResponse(Sender,'InputStream.CheckRequest',E);
          end;
      finally
        UnLockOpen;
      end
    else
      if LOG_GATEWAY_ERRORS then
        Log('InputStream.CheckRequest: '+Sender.Request.Method+' '+Sender.Request.URI,'ERR.SIGNAL');
    end;
  end;

procedure TRtcGateway.InputStreamDataReceived(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  DoDataReceived(Sender,True);
  (*if Sender.Request.Complete then
    DoDataReceived(Sender)
  else
    Sender.PostEvent(DoDataReceived);
    // Sender.PostEventTo(GateThread,DoDataReceived);
    // DoDataReceived(Sender)*)
  end;

procedure TRtcGateway.InputStreamResponseDone(Conn: TRtcConnection);
  begin
  if LockOpen(Conn) then
    SignalClosed(Conn);
  end;

procedure TRtcGateway.InputStreamDisconnect(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if LockOpen(Conn) then
    try
      try
        if LOG_GATEWAY_STATUS then
          Log('INPUT Disconnect '+IntToStr(GetUserID(Sender) shr UserIDShift)+' at '+IntToStr(Sender.Request.ContentIn)+'/'+IntToStr(Sender.Response.ContentOut)+' ('+Sender.PeerAddr+':'+Sender.PeerPort+')',GATE_LOG);
        DoResetInputStream(Sender);
      except
        on E:Exception do
          if LOG_GATEWAY_ERRORS then
            Log('INPUT Disconnect '+E.ClassName+':'+E.Message+' ('+Sender.PeerAddr+':'+Sender.PeerPort+')','ERROR');
        end;
    finally
      SignalClosed(Conn);
      end;
  end;

procedure TRtcGateway.OutputStreamCheckRequest(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if ( (Sender.Request.FileName=FGATEURI_OUTPUT) or (Sender.Request.FileName=FGATEURI_OUTPUTRESET) ) and
     (Sender.Request.Method='POST') and
     (Sender.Request.ContentLength<64) then
    begin
    if SignalOpen(Conn) then
      try
        Sender.Accept;
      finally
        UnLockOpen;
      end
    else
      if LOG_GATEWAY_ERRORS then
        Log('OutputStream.CheckRequest: '+Sender.Request.Method+' '+Sender.Request.URI,'ERR.SIGNAL');
    end;
  end;

procedure TRtcGateway.OutputStreamDataReceived(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  DoOpenOutputStream(Sender);
  end;

procedure TRtcGateway.OutputStreamDataSent(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  try
    if Sender.Response.Done then
      begin
      if Sender.Response.StatusCode=GATE_OK_CODE then
        DoCloseOutputStream(Sender)
      else
        begin
        if LOG_GATEWAY_STATUS then
          Log('OUTPUT DataSent '+IntToStr(GetUserID(Sender) shr UserIDShift)+' - Error Response sent, closing.',GATE_LOG);
        try
          Sender.PostEvent(DoDisconnect);
        except
          on E:Exception do
            if LOG_GATEWAY_ERRORS then
              Log('DataSent - Disconnect '+E.ClassName+':'+E.Message,'ERR.DISCO');
          end;
        end;
      end
    else
      // DoDataSent(Sender);
      Sender.PostEventTo(GateThread,DoDataSent);
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('OUTPUT DataSent '+E.ClassName+':'+E.Message,'ERROR');
    end;
  end;

procedure TRtcGateway.OutputStreamResponseDone(Conn: TRtcConnection);
  begin
  if LockOpen(Conn) then
    SignalClosed(Conn);
  end;

procedure TRtcGateway.OutputStreamDisconnect(Conn: TRtcConnection);
  var
    Sender:TRtcDataServer absolute Conn;
  begin
  if LockOpen(Conn) then
    try
      try
        if LOG_GATEWAY_STATUS then
          Log('OUTPUT Disconnect '+IntToStr(GetUserID(Sender) shr UserIDShift)+' at '+IntToStr(Sender.Request.ContentIn)+'/'+IntToStr(Sender.Response.ContentOut)+' ('+Sender.PeerAddr+':'+Sender.PeerPort+')',GATE_LOG);
        DoResetOutputStream(Sender);
      except
        on E:Exception do
          if LOG_GATEWAY_ERRORS then
            Log('OUTPUT Disconnect '+E.ClassName+':'+E.Message+' ('+Sender.PeerAddr+':'+Sender.PeerPort+')','ERROR');
        end;
    finally
      SignalClosed(Conn);
      end;
  end;

procedure TRtcGateway.DoDisconnect(Conn: TRtcConnection);
  begin
  if LockOpen(Conn) then
    try
      Conn.Disconnect;
    finally
      UnLockOpen;
      end;
  end;

procedure TRtcGateway.PingAllUsers;
  begin
  try
    // DoSendPing(nil);
    TRtcThread.PostEvent(GateThread,DoSendPing);
  except
    on E:Exception do
      if LOG_GATEWAY_ERRORS then
        Log('PING '+E.ClassName+':'+E.Message,'ERROR');
    end;
  end;

procedure TRtcGateway.SetServer(const Value: TRtcDataServer);
  begin
  if FServer<>Value then
    begin
    FServer := Value;

    PingProvider.Server:=FServer;
    InputStream.Server:=FServer;
    OutputStream.Server:=FServer;
    LoginProvider.Server:=FServer;

    if assigned(FServer) then
      SetupConnectionTimeouts(FServer);
    end;
  end;

procedure TRtcGateway.SetGateFileName(const Value: RtcString);
  begin
  FGateFileName := Value;
  if FGateFileName<>Value then
    begin
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

procedure TRtcGateway.SetGatePrimaryKey(const Value: RtcString);
  begin
  FGATE_PRIMARY_KEY := Value;
  end;

procedure TRtcGateway.SetMaxAuthLen(const Value: integer);
  begin
  FGATE_MAXAUTHLEN := Value;
  end;

procedure TRtcGateway.Notification(AComponent: TComponent; Operation: TOperation);
  begin
  inherited Notification(AComponent,Operation);
  if Operation=opRemove then
    if AComponent=FServer then
      SetServer(nil);
  end;

procedure TRtcGateway.AddUserToGroup(OwnerID, GroupID, UserID: TGateUID; NotifyUser, NotifyOwner: boolean);
  begin
  InternalAddUserToGroup(OwnerID shl UserIDShift, (OwnerID shl UserIDShift) or GroupID, UserID shl UserIDShift, NotifyUser, NotifyOwner);
  end;

procedure TRtcGateway.RemoveUserFromGroup(OwnerID, GroupID, UserID: TGateUID; NotifyUser, NotifyOwner: boolean);
  begin
  InternalRemoveUserFromGroup((OwnerID shl UserIDShift) or GroupID, UserID shl UserIDShift, NotifyUser, NotifyOwner);
  end;

procedure TRtcGateway.RemoveGroup(OwnerID, GroupID: TGateUID; NotifyUsers, NotifyOwner: boolean);
  begin
  InternalRemoveGroup(OwnerID shl UserIDShift, (OwnerID shl UserIDShift) or GroupID, NotifyUsers, NotifyOwner);
  end;

end.

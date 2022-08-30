{
  "Server Socket Connection Provider"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}

unit rtcSocketSrvProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  memObjList,

  rtcInfo,
  rtcFastStrings,
  rtcLog,
  rtcThrPool,

  rtcPlugins,
  rtcSockBaseSrvProv,

  rtcConnProv, // Basic connection provider wrapper

  rtcSockBase;

type
  TRtcSocketServerProvider = class;

  TRtcSocketServerProtocol = (proTCP, proUDP);

  TRtcSocketClientThread = class(TRtcThread)
  public
    Sock: TRtcSocketBase;
    Par: TRtcSocketServerProvider;
    _Silent: boolean;

    RtcConn: TRtcSocketServerProvider;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Init;
    function RunJob:boolean; override;
    end;

  TRtcSocketServerThread = class(TRtcThread)
  public
    RtcConn: TRtcSocketServerProvider;
    Releasing: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure StartListen;
    procedure ReStartListen;
    procedure StopListen;

    function RunJob:boolean; override;
    end;

  TRtcSocketServerProvider = class(TRtcBaseSockServerProvider)
  private
    Conn:TRtcSocketBase;

    FRawOut,
    FPlainOut:int64;

    FSocketClass: TRtcSocketBaseClass;
    FProtocol: TRtcSocketServerProtocol;

    FReadBuff:RtcByteArray;

    FClientList:tObjList;
    FThrList:tObjList;

    FMultiCast          : Boolean;
    FMultiCastAddrStr   : RtcString;
    FReuseAddr          : Boolean;

    FListenerUp:boolean;
    FRealNeedData:boolean;

    Client_Thread: TRtcSocketClientThread;
    Server_Thread: TRtcSocketServerThread;

    FParent:TRtcSocketServerProvider;
    isServer,isClient:boolean;

    procedure wsOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);

    procedure wsOnNewSocket(Sender: TObject; ErrCode: Word);
    procedure wsOnSocketClosed(Sender: TObject; ErrCode: Word);

    procedure wsOnDataReceived(Sender: TObject; ErrCode: Word);
    procedure wsOnDataSent(Sender: TObject; ErrCode: Word);
    procedure wsOnDataOut(Sender: TObject; Len: Cardinal);
    procedure wsOnDataIn(Sender: TObject; Len: Cardinal);

  protected


    function GetClientThread:TRtcThread; override;
    function GetServerThread:TRtcThread; override;

    function AddClient(Client:TRtcSocketServerProvider):boolean;
    procedure RemoveClient(Client:TRtcSocketServerProvider);
    procedure KillClients;

    function ClientCount:integer;

    function AddThread(Thr:TRtcThread):boolean;
    procedure RemoveThread(Thr:TRtcThread);
    procedure KillThreads;

    function _Active:boolean;
    function _Visible:boolean;

    procedure CopyFrom(Dup:TRtcConnectionProvider); override;

    procedure StartListener(Restarting:boolean);

    procedure CryptNeedMoreData;

    procedure DirectWriteEx(const s: RtcByteArray);
    procedure BufferWriteEx(const s: RtcByteArray);

    procedure DirectWrite(const s: RtcString);
    procedure BufferWrite(const s: RtcString);

    function CryptPluginError:boolean; virtual;
    function GetCryptProtocol:TRtcCryptPluginProtocol; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Release; override;

    procedure NeedMoreData;

    function GetParent:TRtcConnectionProvider; override;

    procedure Check; override;
    procedure InternalDisconnect; override;

    procedure Listen(Restarting:boolean=False); override;
    procedure Disconnect; override;

    function ReadEx: RtcByteArray; override;
    procedure WriteEx(const s: RtcByteArray; SendNow:boolean=True); override;

    function Read: RtcString; override;
    procedure Write(const s: RtcString; SendNow:boolean=True); override;

    property SocketClass:TRtcSocketBaseClass read FSocketClass write FSocketClass;

    property Proto:TRtcSocketServerProtocol read FProtocol write FProtocol;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastAddr   : RtcString            read  FMultiCastAddrStr
                                                    write FMultiCastAddrStr;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;
    end;

implementation

{ TRtcSocketClientThread }

var
  Message_WSInit,
  Message_WSReInit,
  Message_WSStop,
  Message_WSCloseConn,
  Message_WSRelease_Silent,
  Message_WSRelease_Normal,
  Message_WSRelease:TRtcBaseMessage;


{ TRtcSocketServerProvider }

constructor TRtcSocketServerProvider.Create;
  begin
  inherited;

  SocketClass:=nil;

  FRawOut:=0;
  FPlainOut:=0;

  FClientList:=nil; // TBinList.Create(128);
  FThrList:=nil; // TBinList.Create(128);

  FProtocol:=proTCP;
  FMultiCastAddrStr:='';

  SetLength(FReadBuff, SOCK_READ_BUFFER_SIZE);

  FParent:=nil;
  isServer:=False;
  isClient:=False;

  FListenerUp:=False;

  Conn:=nil;
  end;

destructor TRtcSocketServerProvider.Destroy;
  begin
  try
    { Before destroying this connection object,
      we will disconnect this and all related open connections. }

    Closing:=True;
    Silent:=True;

    if assigned(Conn) then
      InternalDisconnect;

    if isClient then
      if assigned(FParent) and not FParent.Silent then // Client and not Silent
        TriggerConnectionLost;

    SetLength(FReadBuff,0);

    FParent:=nil;
    RtcFreeAndNil(FClientList);
    RtcFreeAndNil(FThrList);

    FMultiCastAddrStr:='';

    if assigned(Client_Thread) then
      TRtcThread.PostJob(Client_Thread, Message_WSStop, True);

    if assigned(Server_Thread) then
      TRtcThread.PostJob(Server_Thread, Message_WSStop, True);

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketServerProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.CopyFrom(Dup: TRtcConnectionProvider);
  begin
  inherited CopyFrom(Dup);

  Proto:=TRtcSocketServerProvider(Dup).Proto;

  TimeoutsOfAPI:=TRtcSocketServerProvider(Dup).TimeoutsOfAPI;
  Conn.TimeoutsOfAPI:=TimeoutsOfAPI;

  Conn.OnBgException:=wsOnBgException;

  if isServer then // Server
    begin
    with Conn do
      begin
      case Proto of
        proTCP:Protocol:=spTcp;
        proUDP:Protocol:=spUdp;
        end;

      OnNewSocket:=wsOnNewSocket;
      OnChangeState:=wsOnChangeState;
      end;
    end
  else if isClient and assigned(FParent) then // Client
    begin
    with Conn do
      begin
      case Proto of
        proTCP:Protocol:=spTcp;
        proUDP:Protocol:=spUdp;
        end;

      OnDataReceived:=wsOnDataReceived;
      OnDataSent:=wsOnDataSent;
      OnDataOut:=wsOnDataOut;
      OnDataIn:=wsOnDataIn;
      OnChangeState:=wsOnChangeState;
      end;
    end;
  end;

function TRtcSocketServerProvider.CryptPluginError:boolean;
  begin
  Result:=True;
  Lost:=True;
  InternalDisconnect;
  // raise Exception.Create('Encryption error.');
  end;

procedure TRtcSocketServerProvider.Listen(Restarting:boolean=False);
  begin
  if Restarting then
    begin
    if assigned(Server_Thread) then
      TRtcThread.PostJob(Server_Thread, Message_WSReInit)
    else if GetMultiThreaded then
      begin
      Server_Thread := TRtcSocketServerThread.Create;
      Server_Thread.RtcConn:= self;
      TRtcThread.PostJob(Server_Thread,Message_WSReInit);
      end
    else
      StartListener(True);
    end
  else
    begin
    if assigned(Server_Thread) then
      TRtcThread.PostJob(Server_Thread, Message_WSInit)
    else if GetMultiThreaded then
      begin
      Server_Thread := TRtcSocketServerThread.Create;
      Server_Thread.RtcConn:= self;
      TRtcThread.PostJob(Server_Thread,Message_WSInit);
      end
    else
      StartListener(False);
    end;
  end;

procedure TRtcSocketServerProvider.Disconnect;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease)
  else if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSCloseConn)
  else
    begin
    Lost:=False;
    InternalDisconnect;
    end;
  end;

procedure TRtcSocketServerProvider.InternalDisconnect;
  var
    myCon2:TRtcSocketBase;
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if Conn=nil then
    begin
    Closing:=True;
    Exit;
    end;

  if isServer then // Server
    begin
    Conn.EventsOff:=True;

    Closing:=True;
    State:=conInactive;

    if FListenerUp then
      begin
      FListenerUp:=False;
      if not Silent then
        TriggerListenStop;
      end;

    if GetMultiThreaded then
      KillThreads
    else
      KillClients;

    myCon2:=Conn;
    Conn:=nil;  // hide connections from component

    try
      myCon2.Close;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketServerProvider.InternalDisconnect: MyCon2.Close',E,'ERROR');
      end;
    try
      myCon2.Release;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketServerProvider.InternalDisconnect: MyCon2.Release',E,'ERROR');
      end;

    if Lost then
      TriggerListenLost;

    {if assigned(Server_Thread) then
      begin
      Server_Thread.RtcConn:=nil;
      Server_Thread.Stop;
      Server_Thread:=nil;
      end;}
    TriggerReadyToRelease;
    end
  else if isClient and (State in [conActive,conActivating]) then // Client
    begin
    if State=conActive then
      State:=conClosing
    else
      State:=conInactive;

    Conn.EventsOff:=True; // deactivate all events for this client connection

    if not Closing then
      begin
      if assigned(CryptPlugin) then
        begin
        SetLength(s_out,0);
        try
          s_res:=CryptPlugin.BeforeDisconnectEx(FCryptObject, s_out);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketServerProvider CryptPlugin.BeforeDisconnect',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if not Lost then
          begin
          if length(s_out)>0 then
            begin
            DirectWriteEx(s_out);
            SetLength(s_out,0);
            end;
          if s_res=cpsWaiting then
            CryptNeedMoreData;
          end
        else
          SetLength(s_out,0);
        end;
      wsOnSocketClosed(self,0);
      end
    else
      begin
      myCon2:=Conn;
      Conn:=nil;

      try
        MyCon2.Close;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider.InternalDisconnect: MyCon.Close',E,'ERROR');
        end;
      try
        MyCon2.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider.InternalDisconnect: MyCon.Release',E,'ERROR');
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnDataOut(Sender: TObject; Len: Cardinal);
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      FDataOut:=Len;
      try
        TriggerDataOut;
      finally
        FDataOut:=0;
        end;
      TriggerReadyToRelease;
      end
    else if State=conActive then
      begin
      if assigned(CryptPlugin) then
        begin
        Dec(FRawOut,Len);
        if (FRawOut=0) {and (FPlainOut>0)} then
          begin
          FDataOut:=FPlainOut;
          FPlainOut:=0;
          try
            TriggerDataOut;
          finally
            FDataOut:=0;
            end;
          end;
        TriggerReadyToRelease;
        end
      else
        begin
        FDataOut:=Len;
        try
          TriggerDataOut;
        finally
          FDataOut:=0;
          end;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnDataIn(Sender: TObject; Len: Cardinal);
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      FDataIn:=Len;
      TriggerDataIn;
      TriggerReadyToRelease;
      end
    else if State=conActive then
      begin
      if not assigned(CryptPlugin) then
        begin
        FDataIn:=Len;
        TriggerDataIn;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

function TRtcSocketServerProvider.ReadEx: RtcByteArray;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then
    begin
    SetLength(FReadBuff,0);
    SetLength(Result,0);
    end
  else if Proto=proTCP then
    begin
    if assigned(CryptPlugin) then
      begin
      SetLength(Result,0);
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(s_out,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, Result);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketServerProvider CryptPlugin.DataReceived',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if s_res=cpsClosed then
          if CryptPluginError then Exit;
        if length(Result)>0 then
          begin
          FRealNeedData:=False;
          // Trigger the "OnDataIn" event ...
          FDataIn:=length(Result);
          TriggerDataIn;
          end;
        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if (s_res=cpsWaiting) or FRealNeedData then
          CryptNeedMoreData;
        end;
      end
    else
      begin
      SetLength(Result,0);
      if Conn.ReceiveEx(Result)>0 then
        FRealNeedData:=False;
      end;
    end
  else
    begin
    Result:=FReadBuff;
    SetLength(FReadBuff,0);
    end;
  end;

function TRtcSocketServerProvider.Read: RtcString;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
    Res:RtcByteArray;
  begin
  if not _Visible then
    begin
    SetLength(FReadBuff,0);
    SetLength(Result,0);
    SetLength(Res,0);
    end
  else if Proto=proTCP then
    begin
    if assigned(CryptPlugin) then
      begin
      SetLength(Result,0);
      SetLength(s_in,0);
      if Conn.ReceiveEx(s_in)>0 then
        begin
        // Decrypt input data ...
        SetLength(s_out,0); SetLength(Res,0);
        try
          s_res:=CryptPlugin.DataReceivedEx(FCryptObject, s_in, s_out, Res);
        except
          on E:Exception do
            begin
            if LOG_PLUGIN_ERRORS then
              Log('TRtcSocketServerProvider CryptPlugin.DataReceived',E,'PLUGIN');
            s_res:=cpsClosed;
            end;
          end;
        if s_res=cpsClosed then
          if CryptPluginError then Exit;
        if length(Res)>0 then
          begin
          FRealNeedData:=False;
          // Trigger the "OnDataIn" event ...
          FDataIn:=length(Res);
          TriggerDataIn;
          end;
        if length(s_out)>0 then
          DirectWriteEx(s_out);
        if (s_res=cpsWaiting) or FRealNeedData then
          CryptNeedMoreData;
        Result:=RtcBytesToString(Res);
        SetLength(Res,0);
        end;
      end
    else
      begin
      SetLength(Result,0);
      if Conn.ReceiveStr(Result)>0 then
        FRealNeedData:=False;
      end;
    end
  else
    begin
    Result:=RtcBytesToString(FReadBuff);
    SetLength(FReadBuff,0);
    end;
  end;

procedure TRtcSocketServerProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then
    Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject,s,s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketServerProvider CryptPlugin.DataToSend',E,'PLUGIN');
        s_res:=cpsClosed;
        end;
      end;
    if s_res=cpsClosed then
      if CryptPluginError then Exit;
    Inc(FPlainOut, length(s));
    if length(s_out)>0 then
      DirectWriteEx(s_out);
    if s_res=cpsWaiting then
      CryptNeedMoreData;
    end
  else if SendNow then
    DirectWriteEx(s)
  else
    BufferWriteEx(s);
  end;

procedure TRtcSocketServerProvider.Write(const s: RtcString; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then
    Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject,RtcStringToBytes(s),s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketServerProvider CryptPlugin.DataToSend',E,'PLUGIN');
        s_res:=cpsClosed;
        end;
      end;
    if s_res=cpsClosed then
      if CryptPluginError then Exit;
    Inc(FPlainOut, length(s));
    if length(s_out)>0 then
      DirectWriteEx(s_out);
    if s_res=cpsWaiting then
      CryptNeedMoreData;
    end
  else if SendNow then
    DirectWrite(s)
  else
    BufferWrite(s);
  end;

procedure TRtcSocketServerProvider.DirectWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if isServer then // Server will send to all connected clients
    begin
    { This implementation is for test purposes only.
      Data should be only sent to clients using the appropriate connection objects. }
    if Proto=proUDP then
      Conn.SendEx(s);
    end
  else if isClient and assigned(FParent) then // Client
    begin
    if assigned(CryptPlugin) then
      Inc(FRawOut, length(s));
    Conn.SendEx(s);
    end;
  end;

procedure TRtcSocketServerProvider.DirectWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if isServer then // Server will send to all connected clients
    begin
    { This implementation is for test purposes only.
      Data should be only sent to clients using the appropriate connection objects. }
    if Proto=proUDP then
      Conn.SendStr(s);
    end
  else if isClient and assigned(FParent) then // Client
    begin
    if assigned(CryptPlugin) then
      Inc(FRawOut, length(s));
    Conn.SendStr(s);
    end;
  end;

procedure TRtcSocketServerProvider.BufferWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffEx(s);
  end;

procedure TRtcSocketServerProvider.BufferWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffStr(s);
  end;

procedure TRtcSocketServerProvider.wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);
  var
    Ex:Exception;
  begin
  if Closing then Exit;

  if assigned(Conn) then
    begin
    if isServer then // Server
      begin
      if NewState=wsListening then
        begin
        FListenerUp:=True;
        try
          FLocalAddr:=Conn.GetLocalAddr;
          FLocalPort:=Conn.GetLocalPort;
          FPeerAddr:='';
          FPeerPort:='';
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketServerProvider ChangeState.GetXAddr',E,'ERROR');
          end;

        TriggerListenStart;
        TriggerReadyToRelease;
        end
      else if NewState=wsClosed then
        begin
        { This is important, so we catch the case
          where Listener gets cut off by the OS. }
        InternalDisconnect;
        end
      else if NewState=wsListenLost then
        begin
        Lost:=True;
        InternalDisconnect;
        end
      else if NewState=wsListenError then
        begin
        Ex:=Exception.Create(Conn.GetLastErrorText);
        try
          State:=conInactive;
          try
            if assigned(Conn) then
              begin
              Conn.EventsOff:=True;

              try
                Conn.Release;
              finally
                Conn:=nil;
                end;
              end;
          except
            on E:Exception do
              if LOG_AV_ERRORS then
                Log('TRtcSocketServerProvider Listen.except For',E,'ERROR');
            end;
        finally
          try
            TriggerListenError(Ex);
          finally
            RtcFreeAndNil(Ex);
            end;
          end;
        end;
      end
    else if isClient and assigned(FParent) then // Client
      begin
      if NewState=wsConnected then
        begin
        FRawOut:=0;
        FPlainOut:=0;
        FRealNeedData:=False;
        FLocalAddr:=Conn.GetLocalAddr;
        if FLocalAddr<>'0.0.0.0' then
          begin
          FLocalPort:=Conn.GetLocalPort;
          FPeerAddr:=Conn.GetPeerAddr;
          FPeerPort:=Conn.GetPeerPort;

          TriggerConnecting;
          end;
        end
      else if NewState=wsClosed then
        wsOnSocketClosed(Sender,0)
      else if NewState=wsConnectLost then
        begin
        Lost:=True;
        InternalDisconnect;
        end
      else if NewState=wsConnectError then
        begin
        Lost:=True;
        InternalDisconnect;
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnNewSocket(Sender: TObject; ErrCode:Word);
  var
    cl: TRtcSocketServerProvider;
    obj: TObject;
    _Client: TRtcSocketBase;
  begin
  if Closing then Exit;

  _Client:=TRtcSocketBase(Sender).GetNewSocket;
  if not assigned(_Client) then Exit; // not a socket

  try
    TriggerConnectionAccepting;
  except
    on E:Exception do
      begin
      if LOG_REFUSED_CONNECTIONS then
        Log('Connection refused with Message: '+RtcString(E.Message),'REFUSED');
      _Client.Release; // WSocket_closesocket(HSock);
      Exit; // connection refused.
      end;
    end;

  if Closing then
    begin
    _Client.Release; // WSocket_closesocket(HSock);
    if LOG_REFUSED_CONNECTIONS then
      Log('Connection refused: Server closing.','REFUSED');
    Exit; // connection refused.
    end;

  if GetMultiThreaded then
    begin
    cl:=nil;
    try
      TriggerNewProvider(obj); // create new connection provider
      if obj=nil then
        raise Exception.Create('Connection provider not created.')
      else if obj is TRtcSocketServerProvider then
        cl:=TRtcSocketServerProvider(obj)
      else
        raise Exception.Create('Wrong connection provider class created.');

      cl.isClient:=True;
      cl.FParent:=self;
      cl.Client_Thread := TRtcSocketClientThread.Create;
      with cl.Client_Thread do
        begin
        Par:=self;
        _Silent:=False;
        Sock:=_Client;
        _Client:=nil;
        RtcConn:= cl;
        end;

    except
      on E:Exception do
        begin
        if LOG_AV_ERRORS then
          Log('TRtcSocketServerProvider SesAvail(MultiThreaded)',E,'ERROR');

        if assigned(cl) then
          begin
          try
            cl.InternalDisconnect;
          except
            on E:Exception do
              if LOG_AV_ERRORS then
                Log('TRtcSocketServerProvider SesAvail cl.Disconnect',E,'ERROR');
            end;
          end;

        try
          if assigned(_Client) then
            _Client.Release;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketServerProvider SesAvail WSocket_close',E,'ERROR');
          end;

        Exit;
        end;
      end;

    // make sure we remove this thread on Disconnect.
    if AddThread(cl.Client_Thread) then
      TRtcThread.PostJob(cl.Client_Thread, Message_WSInit)
    else
      begin // if we can't add our thread to the list, disconnect now.
      try
        cl.InternalDisconnect;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider SesAvail cl.Disconnect',E,'ERROR');
        end;
      try
        if assigned(_Client) then
          _Client.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider SesAvail WSocket_close',E,'ERROR');
        end;
      end;
    end
  else // NOT MULTI-THREADED!
    begin
    cl:=nil;
    try
      // Create Provider object
      TriggerNewProvider(obj); // create new connection provider
      if obj=nil then
        raise Exception.Create('Connection provider not created.')
      else if obj is TRtcSocketServerProvider then
        cl:=TRtcSocketServerProvider(obj)
      else
        raise Exception.Create('Wrong connection provider class created.');

      cl.isClient:=True;
      cl.FParent:=self;
      cl.Conn:=_Client;
      cl.CopyFrom(self); // initialize connection object
      cl.State:=conActivating;

      cl.TriggerConnectionAccepted;

      _Client.StartNewSocket;
      _Client:=nil;
    except
      on E:Exception do
        begin
        if LOG_AV_ERRORS then
          Log('TRtcSocketServerProvider SesAvail(not MultiThreaded)',E,'ERROR');

        if assigned(cl) then
          begin
          try
            cl.InternalDisconnect;
          except
            on E:Exception do
              if LOG_AV_ERRORS then
                Log('TRtcSocketServerProvider SesAvail cl.Disconnect',E,'ERROR');
            end;
          try
            RtcFreeAndNil(cl);
          except
            on E:Exception do
              if LOG_AV_ERRORS then
                Log('TRtcSocketServerProvider SesAvail cl.Free',E,'ERROR');
            end;
          end;

        try
          if assigned(_Client) then
            _Client.Release;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketServerProvider SesAvail WSock_Close',E,'ERROR');
          end;
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnSocketClosed(Sender: TObject; ErrCode:Word);
  var
    myParent:TRtcSocketServerProvider;
    myCon:TRtcSocketBase;
  begin
  { Client connection closed.

    This method is called when one of the active connections get closed.
    It handles connections closing for all active connection types
    (incomming and outgoing connections). }

  if not Silent then
    if isServer then // Server
      TriggerDisconnecting
    else if isClient and assigned(FParent) and not FParent.Silent then // Client, not Silent
      TriggerDisconnecting;

  if assigned(Conn) and not Closing then // Connection object still here ?
    begin
    Closing:=True; // Let everyone know we are closing now ...

    myParent:=nil;
    try
      myParent:=FParent;

      if (State in [conActive,conClosing]) and assigned(myParent) then // Connection was activated.
        begin
        myParent.RemoveClient(self);
        if not MyParent.Silent then
          begin
          if assigned(CryptPlugin) then
            try
              CryptPlugin.AfterDisconnectEx(FCryptObject);
            except
              on E:Exception do
                if LOG_PLUGIN_ERRORS then
                  Log('TRtcSocketServerProvider CryptPlugin.AfterDisconnect',E,'PLUGIN');
              end;

          TriggerDisconnect; // server needs to call Disconnect AND ConnectionLost
          end;
        end;

    finally
      try
        if assigned(myParent) and not myParent.Silent then
          TriggerConnectionLost;
      except
        on E:Exception do
          if LOG_EVENT_ERRORS then
            Log('TRtcSocketServerProvider OnSocketClosed.TriggerConnectionLost',E,'EVENT');
        end;

      State:=conInactive;

      { We need to remove all events from this connection
        before we can actually destroy our own connection object. }
      Conn.EventsOff:=True;

      myCon:=Conn;
      Conn:=nil;

      try
        MyCon.Close;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider SesClosed MyCon.Close',E,'ERROR');
        end;
      try
        MyCon.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketServerProvider SesClosed MyCon.Release',E,'ERROR');
        end;
      end;

    if not Silent then
      if assigned(Client_Thread) then
        TRtcThread.PostJob(Client_Thread, Message_WSRelease)
      {$IFNDEF NEXTGEN}else Free{$ENDIF};
    end;
  end;

procedure TRtcSocketServerProvider.wsOnDataReceived(Sender: TObject; ErrCode: Word);
  var
    len:integer;
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then // UDP Server
      begin
      FPeerPort:='';
      FPeerAddr:='';

      len:=Conn.ReceiveEx(FReadBuff);
      if len>=0 then
        begin
        FPeerPort:=Conn.GetPeerPort;
        FPeerAddr:=Conn.GetPeerAddr;
        TriggerDataReceived;
        TriggerReadyToRelease;
        end
      else
        begin
        SetLength(FReadBuff,0);
        TriggerDataLost;
        TriggerReadyToRelease;
        end;
      end
    else
      begin
      if State=conActivating then
        begin
        if FLocalAddr<>'0.0.0.0' then
          begin
          State:=conActive;
          if assigned(FParent) then
            if not FParent.AddClient(self) then
              begin
              InternalDisconnect;
              Exit;
              end;

          if assigned(CryptPlugin) then
            begin
            SetLength(s_out,0);
            try
              s_res:=CryptPlugin.AfterConnectEx(FCryptObject,s_out,GetCryptProtocol,'',PeerAddr);
            except
              on E:Exception do
                begin
                if LOG_PLUGIN_ERRORS then
                  Log('TRtcSocketServerProvider CryptPlugin.AfterConnect',E,'PLUGIN');
                s_res:=cpsClosed;
                end;
              end;
            if s_res=cpsClosed then
              if CryptPluginError then
                begin
                TriggerReadyToRelease;
                Exit;
                end;
            if length(s_out)>0 then
              begin
              DirectWriteEx(s_out);
              SetLength(s_out,0);
              end;
            if s_res=cpsWaiting then
              CryptNeedMoreData;
            end;
          TriggerConnect;
          end;
        end;

      if State=conActive then
        begin
        TriggerDataReceived;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnDataSent(Sender: TObject; ErrCode: Word);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if (State=conListening) and (Proto=proUDP) then
      begin
      TriggerDataSent;
      TriggerReadyToRelease;
      end
    else
      begin
      if State=conActivating then
        begin
        if FLocalAddr<>'0.0.0.0' then
          begin
          State:=conActive;
          if assigned(FParent) then
            if not FParent.AddClient(self) then
              begin
              InternalDisconnect;
              Exit;
              end;

          if assigned(CryptPlugin) then
            begin
            SetLength(s_out,0);
            try
              s_res:=CryptPlugin.AfterConnectEx(FCryptObject,s_out,GetCryptProtocol,'',PeerAddr);
            except
              on E:Exception do
                begin
                if LOG_PLUGIN_ERRORS then
                  Log('TRtcSocketServerProviderCryptPlugin.AfterConnect',E,'PLUGIN');
                s_res:=cpsClosed;
                end;
              end;
            if s_res=cpsClosed then
              if CryptPluginError then
                begin
                TriggerReadyToRelease;
                Exit;
                end;
            if length(s_out)>0 then
              begin
              DirectWriteEx(s_out);
              SetLength(s_out,0);
              end;
            if s_res=cpsWaiting then
              CryptNeedMoreData;
            end;
          TriggerConnect;
          end;
        end;

      if State=conActive then
        begin
        TriggerDataSent;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketServerProvider.wsOnBgException(Sender: TObject; E: Exception;
    var CanClose: Boolean);
  begin
  if (E is EClientLimitReached) or
     (E is EThreadLimitReached) then // ignore those exceptions
    CanClose:=False
  else
    begin
    CanClose:=True;
    try
      TriggerException(E);
    except
      on E:Exception do
        if LOG_EVENT_ERRORS then
          Log('TRtcSocketServerProvider BgExcept Trigger',E,'EVENT');
      // ignore all exceptions here
      end;
    end;
  end;

function TRtcSocketServerProvider.GetParent: TRtcConnectionProvider;
  begin
  Result:=FParent;
  end;

function TRtcSocketServerProvider._Active: boolean;
  begin
  Result:=not Closing and assigned(Conn) and
         (FState in [conActive,conActivating,conListening]);
  end;

function TRtcSocketServerProvider._Visible: boolean;
  begin
  if isServer then
    Result:=not Closing and (FState in [conActive,conActivating,conListening]) and assigned(Conn)
  else if isClient then
    Result:=not Closing and (FState in [conActive,conActivating,conListening]) and
            assigned(Conn) and assigned(FParent) and not FParent.Silent
  else
    Result:=False;
  end;

procedure TRtcSocketServerProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease)
  else if assigned(Server_Thread) then
    TRtcThread.PostJob(Server_Thread, Message_WSRelease)
  else
    inherited;
  end;

function TRtcSocketServerProvider.AddClient(Client: TRtcSocketServerProvider):boolean;
  begin
  Enter;
  try
    Result:=assigned(FClientList);
    if Result then
      FClientList.insert(RtcIntPtr(Client),Client);
  finally
    Leave;
    end;
  end;

procedure TRtcSocketServerProvider.RemoveClient(Client: TRtcSocketServerProvider);
  begin
  Enter;
  try
    if assigned(FClientList) then
      if FClientList.search(RtcIntPtr(Client))=Client then
        FClientList.Remove(RtcIntPtr(Client));
  finally
    Leave;
    end;
  end;

function TRtcSocketServerProvider.AddThread(Thr: TRtcThread):boolean;
  begin
  Enter;
  try
    Result:=assigned(FThrList);
    if Result then
      FThrList.insert(RtcIntPtr(Thr),Thr);
  finally
    Leave;
    end;
  end;

procedure TRtcSocketServerProvider.RemoveThread(Thr: TRtcThread);
  begin
  Enter;
  try
    if assigned(FThrList) then
      if FThrList.search(RtcIntPtr(Thr))=Thr then
        FThrList.Remove(RtcIntPtr(Thr));
  finally
    Leave;
    end;
  end;

{function TRtcSocketServerProvider.Client( a: integer): TRtcSocketServerProvider;
  begin
  Enter;
  try
    if (a>=0) and (a<FClientList.Count) then
      Result:=TRtcSocketServerProvider(FClientList.Items[a])
    else
      Result:=nil;
  finally
    Leave;
    end;
  end;}

function TRtcSocketServerProvider.ClientCount: integer;
  begin
  Enter;
  try
    if assigned(FClientList) then
      Result:=FClientList.Count
    else
      Result:=0;
  finally
    Leave;
    end;
  end;

procedure TRtcSocketServerProvider.KillThreads;
  var
    o:TObject;
    Thr:TRtcSocketClientThread absolute o;
    i:RtcIntPtr;
  begin
  Enter;
  try
    if assigned(FThrList) then
      begin
      repeat
        if FThrList.Count>0 then
          begin
          i:=FThrList.search_min(o);
          FThrList.Remove(i);
          if assigned(Thr) then
            if Silent then
              TRtcThread.PostJob(Thr, Message_WSRelease_Silent, True)
            else;
              TRtcThread.PostJob(Thr, Message_WSRelease_Normal, True);
          end
        else
          Thr:=nil;
        until Thr=nil;
      RtcFreeAndNil(FThrList);
      end;
    RtcFreeAndNil(FClientList);
  finally
    Leave;
    end;
  end;

procedure TRtcSocketServerProvider.KillClients;
  var
    o:TObject;
    cl:TRtcSocketServerProvider absolute o;
    i:RtcIntPtr;
  begin
  Enter;
  try
    if assigned(FClientList) then
      begin
      repeat
        if FClientList.Count>0 then
          begin
          i:=FClientList.search_min(o);
          FClientList.Remove(i);
          if assigned(cl) then
            try
              if assigned(cl.Client_Thread) then
                begin
                if Silent then
                  TRtcThread.PostJob(cl.Client_Thread, Message_WSRelease_Silent, True)
                else;
                  TRtcThread.PostJob(cl.Client_Thread, Message_WSRelease_Normal, True);
                end
              else
                begin
                cl.Silent:=Silent;
                cl.InternalDisconnect;
                end;
            except
              on E:Exception do
                if LOG_AV_ERRORS then
                  Log('TRtcSocketServerProvider KillClients cl.Stop/Disconnect',E,'ERROR');
              end;
          end
        else
          cl:=nil;
        until cl=nil;
      RtcFreeAndNil(FClientList);
      end;
    RtcFreeAndNil(FThrList);
  finally
    Leave;
    end;
  end;

procedure TRtcSocketServerProvider.Check;
  var
    addr:RtcString;
  begin
  if assigned(Conn) then
    begin
    addr:=Conn.GetLocalAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('TRtcSocketServerProvider CLOSING from Check. Socket not connected to local address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to local address.');
      end;
    addr:=Conn.GetPeerAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('TRtcSocketServerProvider CLOSING from Check. Socket not connected to peer address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to peer address.');
      end;
    end;
  end;

function TRtcSocketServerProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

function TRtcSocketServerProvider.GetServerThread: TRtcThread;
  begin
  Result:=Server_Thread;
  end;

procedure TRtcSocketServerProvider.StartListener(Restarting:boolean);
  var
    MyCon:TRtcSocketBase;
    MyPort:RtcString;

  procedure FreeClientLists;
    begin
    RtcFreeAndNil(FClientList);
    RtcFreeAndNil(FThrList);
    end;

  begin
  if (State=conListening) or (State=conActivating) then
    Exit; // already listening !!!

  if State<>conInactive then
    raise Exception.Create('Can not start listener again. Connection in use.');

  if assigned(Conn) then
    Error('Can not start listener. Connection in use.');

  if not assigned(SocketClass) then
    raise Exception.Create('TRtcSocketServerProvider -> SocketClass not assigned!');

  try
    if Proto=proUDP then
      SetLength(FReadBuff,0);

    isServer:=True;
    FListenerUp:=False;
    Closing:=False;
    Silent:=False;
    Lost:=True;

    Enter;
    try
      FreeClientLists;
      FClientList:=tObjList.Create(128);
      FThrList:=tObjList.Create(128);
    finally
      Leave;
      end;

    MyPort:=Trim(GetPort);
    if length(MyPort)=0 then
      Error('Port undefined.');

    State:=conActivating;
    try
      Conn:=SocketClass.Create;
      Conn.Reconnecting:=Restarting;
      Conn.MessageThread:=Server_Thread;

      Conn.TimeoutsOfAPI:=TimeoutsOfAPI;

      with Conn do
        begin
        case Proto of
          proTCP:Protocol:=spTcp;
          proUDP:
            begin
            Protocol:=spUdp;
            UdpMultiCast:=Self.UdpMultiCast;
            UdpMultiCastAddr:=Self.UdpMultiCastAddr;
            UdpReuseAddr:=Self.UdpReuseAddr;

            OnDataReceived:=wsOnDataReceived;
            OnDataSent:=wsOnDataSent;
            OnDataOut:=wsOnDataOut;
            OnDataIn:=wsOnDataIn;
            end;
          end;

        if self.GetAddr='' then
          Addr:='0.0.0.0'
        else
          Addr:=self.GetAddr;

        MultiThreaded:=assigned(Server_Thread);

        Port:=MyPort;

        OnBgException:=wsOnBgException;
        OnChangeState:=wsOnChangeState;
        OnNewSocket:=wsOnNewSocket;
        end;

      State:=conListening;

      Conn.Listen;

    except
      on E:Exception do
        begin
        State:=conInactive;
        try
          if assigned(Conn) then
            begin
            MyCon:=Conn;
            Conn:=nil;

            MyCon.EventsOff:=True;
            MyCon.Release;
            end;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketServerProvider Listen.except For',E,'ERROR');
          end;
        raise;
        end;
      end;

  except
    on E:EClientLimitReached do // connection limit reached
      begin
      FreeClientLists;
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:EThreadLimitReached do // connection limit reached
      begin
      FreeClientLists;
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcSocketError do // standard (expected) socket exception
      begin
      FreeClientLists;
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcFatalSockException do // fatal socket exception
      begin
      FreeClientLists;
      TriggerListenError(E);
      TriggerReadyToRelease;
      end;
    on E:Exception do
      begin
      FreeClientLists;
      TriggerReadyToRelease;
      raise;
      end;
    end;
  end;

constructor TRtcSocketClientThread.Create;
  begin
  inherited;
  _Silent:=False;
  RtcConn:=nil;
  Par:=nil;
  end;

destructor TRtcSocketClientThread.Destroy;
  begin
  try
    try
      if assigned(Par) then
        Par.RemoveThread(self);
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketClientThread.Destroy Par.RemoveThread',E,'ERROR');
      end;

    try
      if assigned(RtcConn) then
        begin
        if _Silent then
          begin
          RtcConn.Closing:=True;
          RtcConn.Silent:=True;
          RtcConn.FParent:=nil;
          end
        else
          RtcConn.InternalDisconnect;
        RtcFreeAndNil(RtcConn);
        end;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketClientThread.Destroy RtcConn.Free',E,'ERROR');
      end;

    try
      if assigned(Sock) then
        begin
        Sock.Release;
        Sock:=nil;
        end;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketClientThread.Destroy Sock.Release',E,'ERROR');
      end;

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSocketClientThread.Init;
  begin
  with RtcConn do
    begin
    Conn := Sock;
    Sock := nil;
    Conn.TimeoutsOfAPI:=TimeoutsOfAPI;
    Conn.MultiThreaded:=True;
    Conn.MessageThread:=self;

    CopyFrom(Par); // initialize connection object
    State:=conActivating;
    Conn.StartNewSocket;

    TriggerConnectionAccepted; // if we are over connection limit, EConnectionLimitReached exception will be triggered.
    end;
  end;

function TRtcSocketClientThread.RunJob:boolean;
  begin
  Result:=True; // Release thread object if not processed or
  try
    if Job is TRtcSockMessage then
      begin
      try
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            begin
            RtcConn.Conn.DoMessage(TRtcSockMessage(Job).Msg,0);
            if assigned(RtcConn) then
              if assigned(RtcConn.Conn) then
                Result:=False;
            end;
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketClientThread.RunJob "DoMessage('+IntToStr(TRtcSockMessage(Job).Msg)+')"',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketClientThread.RunJob "DoMessage('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
            end;
        end;
      end
    else if Job is TRtcSocketMessageJob then
      begin
      try
        if assigned(RtcConn) then
          begin
          if assigned(RtcConn.Conn) then
            begin
            if inherited RunJob=False then
              if assigned(RtcConn) then
                if assigned(RtcConn.Conn) then
                  Result:=False;
            end
          else
            begin
            TRtcJob(Job).Kill;
            raise Exception.Create('RtcConn.Conn = nil');
            end;
          end
        else
          begin
          TRtcJob(Job).Kill;
          raise Exception.Create('RtcConn = nil');
          end;
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+IntToStr(TRtcSocketMessageJob(Job).Msg)+')"',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
            end;
        end;
      end
    else if Job=Message_WSInit then
      begin
      try
        Init;
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketClientThread.RunJob "Init"',E,'ERROR');
        end;
      end
    else if Job=Message_WSRelease_Silent then
      begin
      try
        Par:=nil;
        _Silent:=True;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketClientThread.RunJob "WSRelease_Silent"',E,'ERROR');
        end;
      end
    else if Job=Message_WSRelease_Normal then
      begin
      try
        Par:=nil;
        _Silent:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketClientThread.RunJob "WSRelease_Normal"',E,'ERROR');
        end;
      end
    else if Job=Message_WSRelease then
      begin
      {try
        if assigned(RtcConn) then
          RtcConn.Lost:=False;
        // Result:=True;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketClientThread.RunJob "WSRelease"',E,'ERROR');
        end;}
      end
    else if Job=Message_WSStop then
      begin
      try
        Par:=nil;
        RtcConn:=nil;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketClientThread.RunJob "WSStop"',E,'ERROR');
        end;
      end
    else
      begin
      try
        if inherited RunJob=False then
          if assigned(RtcConn) then
            if assigned(RtcConn.Conn) then
              Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketClientThread.RunJob (else: "'+Job.ClassName+'")',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketClientThread.RunJob (else: *Error* "'+E2.ClassName+':'+E2.Message+'")',E,'ERROR');
            end;
        end;
      end;
  except
    on E:Exception do if LOG_AV_ERRORS then
      Log('SRV:TRtcSocketClientThread.RunJob *AV*',E,'ERROR');
    end;
  end;

procedure TRtcSocketServerProvider.CryptNeedMoreData;
  begin
  if assigned(Conn) then Conn.NeedMoreData;
  end;

procedure TRtcSocketServerProvider.NeedMoreData;
  begin
  if assigned(Conn) then
    begin
    FRealNeedData:=True;
    Conn.NeedMoreData;
    end;
  end;

function TRtcSocketServerProvider.GetCryptProtocol: TRtcCryptPluginProtocol;
  begin
  Result:=cppTcp;
  end;

{ TRtcSocketServerThread }

constructor TRtcSocketServerThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  end;

destructor TRtcSocketServerThread.Destroy;
  begin
  try
    if assigned(RtcConn) then
      begin
      try
        RtcConn.Server_Thread:=nil;
        StopListen;
        if Releasing then
          RtcFreeAndNil(RtcConn);
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('WSockServerThread.Destroy',E,'ERROR');
          // ignore exceptions
        end;
      RtcConn:=nil;
      end;
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketServerThread.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSocketServerThread.StartListen;
  begin
  RtcConn.StartListener(False);
  end;

procedure TRtcSocketServerThread.ReStartListen;
  begin
  RtcConn.StartListener(True);
  end;

procedure TRtcSocketServerThread.StopListen;
  begin
  if assigned(RtcConn) then
    begin
    try
      RtcConn.Lost:=False;
      RtcConn.InternalDisconnect;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('WSockServerThread.StopListen : RtConn.InternalDisconnect',E,'ERROR');
        // ignore exceptions
      end;
    end;
  end;

function TRtcSocketServerThread.RunJob:boolean;
  begin
  Result:=True; // release Thread object if not processed or error
  try
    if Job is TRtcSockMessage then
      begin
      try
        if assigned(RtcConn) then
          begin
          if assigned(RtcConn.Conn) then
            begin
            RtcConn.Conn.DoMessage(TRtcSockMessage(Job).Msg,0);
            if assigned(RtcConn) then
              if assigned(RtcConn.Conn) then
                Result:=False;
            end
          else
            raise Exception.Create('RtcConn.Conn = nil');
          end
        else
          raise Exception.Create('RtcConn = nil');
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketServerThread.RunJob "DoMessage('+IntToStr(TRtcSockMessage(Job).Msg)+')"',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketServerThread.RunJob "DoMessage('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
            end;
        end;
      end
    else if Job is TRtcSocketMessageJob then
      begin
      try
        if assigned(RtcConn) then
          begin
          if assigned(RtcConn.Conn) then
            begin
            if inherited RunJob=False then
              if assigned(RtcConn) then
                if assigned(RtcConn.Conn) then
                  Result:=False;
            end
          else
            begin
            TRtcJob(Job).Kill;
            raise Exception.Create('RtcConn.Conn = nil');
            end;
          end
        else
          begin
          TRtcJob(Job).Kill;
          raise Exception.Create('RtcConn = nil');
          end;
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+IntToStr(TRtcSocketMessageJob(Job).Msg)+')"',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
            end;
        end;
      end
    else if Job=Message_WSInit then
      begin
      try
        StartListen;
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketServerThread.RunJob "WSInit"',E,'ERROR');
        end;
      end
    else if Job=Message_WSReInit then
      begin
      try
        ReStartListen;
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketServerThread.RunJob "WSReInit"',E,'ERROR');
        end;
      end
    else if Job=Message_WSCloseConn then
      begin
      try
        StopListen;
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          Log('SRV:TRtcSocketServerThread.RunJob "WSCloseConn"',E,'ERROR');
        end;
      end
    else if Job=Message_WSRelease then
      Releasing:=True
    else if Job=Message_WSStop then
      RtcConn:=nil
    else
      begin
      try
        if inherited RunJob=False then
          if assigned(RtcConn) then
            if assigned(RtcConn.Conn) then
              Result:=False;
      except
        on E:Exception do if LOG_AV_ERRORS then
          try
            Log('SRV:TRtcSocketServerThread.RunJob (else: "'+Job.ClassName+'")',E,'ERROR');
          except
            on E2:Exception do
              Log('SRV:TRtcSocketServerThread.RunJob (else: *ClassError* "'+E2.ClassName+':'+E2.Message+'")',E,'ERROR');
            end;
        end;
      end;
  except
    on E:Exception do if LOG_AV_ERRORS then
      Log('TRtcSocketServerThread.RunJob *AV*',E,'ERROR');
    // Result:=True;
    end;
  end;

type
  TRtcSocketSrvProvUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  mySock:TRtcSocketSrvProvUnit;

{ TMySocketSrvProv }

constructor TRtcSocketSrvProvUnit.Create;
  begin
  inherited;
  Message_WSInit:=TRtcBaseMessage.Create;
  Message_WSReInit:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  Message_WSRelease_Silent:=TRtcBaseMessage.Create;
  Message_WSRelease_Normal:=TRtcBaseMessage.Create;
  end;

destructor TRtcSocketSrvProvUnit.Destroy;
  begin
  try
    RtcFreeAndNil(Message_WSInit);
    RtcFreeAndNil(Message_WSReInit);
    RtcFreeAndNil(Message_WSStop);
    RtcFreeAndNil(Message_WSCloseConn);
    RtcFreeAndNil(Message_WSRelease);
    RtcFreeAndNil(Message_WSRelease_Silent);
    RtcFreeAndNil(Message_WSRelease_Normal);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketSrvProvUnit.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcSocketSrvProv Initializing ...','DEBUG');{$ENDIF}

MySock:=TRtcSocketSrvProvUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcSocketSrvProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSocketSrvProv Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

RtcFreeAndNil(MySock);

{$IFDEF RTC_DEBUG} Log('rtcSocketSrvProv Finalized.','DEBUG');{$ENDIF}
end.

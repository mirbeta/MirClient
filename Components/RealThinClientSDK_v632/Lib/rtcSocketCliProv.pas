{
  "Client Socket Connection Provider"
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}
unit rtcSocketCliProv;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcLog,
  rtcInfo,
  rtcConnProv, // Basic connection provider wrapper
  rtcSockBaseCliProv,

  rtcPlugins,
  rtcThrPool,
  rtcThrConnProv, // Threaded connection provider wrapper

  rtcSockBase;

type
  TRtcSocketClientProvider = class;

  TRtcSocketClientProtocol = (proTCP, proUDP);

  TRtcSocketClientThread = class(TRtcThread)
  public
    RtcConn: TRtcSocketClientProvider;
    Releasing: boolean;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure OpenConn;
    procedure ReOpenConn;
    procedure CloseConn;

    function RunJob:boolean; override;
    end;

  TRtcSocketClientProvider = class(TRtcBaseSockClientProvider)
  private
    Conn:TRtcSocketBase;

    FSocketClass:TRtcSocketBaseClass;

    FProtocol: TRtcSocketClientProtocol;

    FRawOut,
    FPlainOut:int64;

    FReadBuff:RtcByteArray;

    Client_Thread : TRtcSocketClientThread;

    FMultiCast          : Boolean;
    FMultiCastIpTTL     : Integer;
    FReuseAddr          : Boolean;

    FRealNeedData:boolean;

    FBlocking: boolean;

    procedure wsOnBgException(Sender: TObject; E: Exception; var CanClose: Boolean);
    procedure wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);

    procedure wsOnSessionClosed(Sender: TObject; ErrorCode: Word);

    procedure wsOnDataReceived(Sender: TObject; ErrCode: Word);
    procedure wsOnDataSent(Sender: TObject; ErrCode: Word);

    procedure wsOnDataOut(Sender: TObject; Len: cardinal);
    procedure wsOnDataIn(Sender: TObject; Len: cardinal);

    procedure OpenConnection(Force:boolean;Reconnecting:boolean);

  protected

    function _Active:boolean;
    function _Visible:boolean;

    function GetClientThread:TRtcThread; override;

    procedure CryptNeedMoreData;

    procedure DirectWriteEx(const s:RtcByteArray);
    procedure BufferWriteEx(const s:RtcByteArray);

    procedure DirectWrite(const s:RtcString);
    procedure BufferWrite(const s:RtcString);

    function CryptPluginError:boolean; virtual;
    function GetCryptProtocol:TRtcCryptPluginProtocol; virtual;

  public
    constructor Create; override;
    destructor Destroy; override;

    procedure NeedMoreData;

    procedure Release; override;

    procedure Connect(Force:boolean=False;Reconnecting:boolean=False); override;

    procedure InternalDisconnect; override;
    procedure Disconnect; override;

    procedure Check; override;

    function ReadEx: RtcByteArray; override;
    procedure WriteEx(const s: RtcByteArray; sendNow:boolean=True); override;

    function Read: RtcString; override;
    procedure Write(const s: RtcString; sendNow:boolean=True); override;

    property Blocking:boolean read FBlocking write FBlocking;

    property SocketClass:TRtcSocketBaseClass read FSocketClass write FSocketClass;

    property Proto:TRtcSocketClientProtocol read FProtocol write FProtocol;

    property UdpMultiCast       : Boolean           read  FMultiCast
                                                    write FMultiCast;
    property UdpMultiCastMaxHops: Integer           read  FMultiCastIpTTL
                                                    write FMultiCastIpTTL;
    property UdpReuseAddr       : Boolean           read  FReuseAddr
                                                    write FReuseAddr;
    end;

implementation

{ TRtcWSockClientThread }

var
  Message_WSStop,
  Message_WSOpenConn,
  Message_WSReOpenConn,
  Message_WSCloseConn,
  Message_WSRelease:TRtcBaseMessage;

{ TRtcWSockClientProvider }

constructor TRtcSocketClientProvider.Create;
  begin
  inherited;

  FBlocking:=False;
  SocketClass:=nil;

  FRawOut:=0;
  FPlainOut:=0;

  Closing:=False;

  FPeerPort:='';
  FPeerAddr:='0.0.0.0';
  FLocalPort:='';
  FLocalAddr:='0.0.0.0';

  FProtocol:=proTCP;
  UdpMultiCastMaxHops:=1;

  SetLength(FReadBuff,0);

  Conn:=nil;
  end;

destructor TRtcSocketClientProvider.Destroy;
  begin
  try
    { Before destroying this connection object,
      we will disconnect this and all related open connections. }

    Closing:=True;
    Silent:=True;

    if assigned(Conn) then
      InternalDisconnect;

    TriggerConnectionClosing;

    if assigned(Client_Thread) then
      TRtcThread.PostJob(Client_Thread, Message_WSStop, True);

    SetLength(FReadBuff,0);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientProvider.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

function TRtcSocketClientProvider.CryptPluginError:boolean;
  begin
  Result:=True;
  Lost:=True;
  InternalDisconnect;
  //raise Exception.Create('Encryption error.');
  end;

function TRtcSocketClientProvider.ReadEx: RtcByteArray;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not assigned(Conn) then
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
              Log('TRtcSocketClientProvider CryptPlugin.DataReceived',E,'PLUGIN');
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

function TRtcSocketClientProvider.Read: RtcString;
  var
    s_in, s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
    Res:RtcByteArray;
  begin
  if not assigned(Conn) then
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
              Log('TRtcSocketClientProvider CryptPlugin.DataReceived',E,'PLUGIN');
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

procedure TRtcSocketClientProvider.WriteEx(const s: RtcByteArray; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject, s, s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketClientProvider CryptPlugin.DataToSend',E,'PLUGIN');
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

procedure TRtcSocketClientProvider.Write(const s: RtcString; SendNow:boolean=True);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    begin
    SetLength(s_out,0);
    try
      s_res:=CryptPlugin.DataToSendEx(FCryptObject, RtcStringToBytes(s), s_out);
    except
      on E:Exception do
        begin
        if LOG_PLUGIN_ERRORS then
          Log('TRtcSocketClientProvider CryptPlugin.DataToSend',E,'PLUGIN');
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

procedure TRtcSocketClientProvider.DirectWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.SendEx(s);
  end;

procedure TRtcSocketClientProvider.DirectWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.SendStr(s);
  end;

procedure TRtcSocketClientProvider.BufferWriteEx(const s: RtcByteArray);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffEx(s);
  end;

procedure TRtcSocketClientProvider.BufferWrite(const s: RtcString);
  begin
  if not _Visible then Exit;

  if assigned(CryptPlugin) then
    Inc(FRawOut, length(s));
  Conn.BuffStr(s);
  end;

procedure TRtcSocketClientProvider.wsOnChangeState(Sender: TObject; NewState: TRtcSocketState);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if Closing then Exit;

  if assigned(Conn) then
    if NewState=wsConnected then
      begin
      if Proto=proTCP then
        begin
        FLocalAddr:=Conn.GetLocalAddr;
        if FLocalAddr<>'0.0.0.0' then
          begin
          FLocalPort:=Conn.GetLocalPort;
          FPeerAddr:=Conn.GetPeerAddr;
          FPeerPort:=Conn.GetPeerPort;
          TriggerConnecting;
          end;
        end
      else
        begin
        FLocalAddr:='127.0.0.1';
        FLocalPort:=Conn.GetLocalPort;
        FPeerAddr:=Conn.GetPeerAddr;
        FPeerPort:=Conn.GetPeerPort;
        TriggerConnecting;

        State:=conActive;

        if assigned(CryptPlugin) then
          begin
          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, PeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('TRtcSocketClientProvider CryptPlugin.AfterConnect',E,'PLUGIN');
              s_res:=cpsClosed;
              end;
            end;
          if s_res=cpsClosed then
            if CryptPluginError then Exit;
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
      end
    else if NewState=wsClosed then
      wsOnSessionClosed(Sender, 0)
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

procedure TRtcSocketClientProvider.wsOnSessionClosed(Sender: TObject; ErrorCode:Word);
  var
    Ex:Exception;
  begin
  { Client connection closed.

    This method is called when one of the active connections get closed.
    It handles connections closing for all active connection types
    (incomming and outgoing connections). }

  TriggerDisconnecting;

  if assigned(Conn) and not Closing then // Connection object still here ?
    begin
    Closing:=True; // Let everyone know we are closing now ...

    try
      TriggerConnectionClosing;

      if State in [conInactive,conActivating] then // Connection still not activated,
        begin
        if Lost then
          begin
          if Conn.GetLastErrorText<>'' then
            begin
            Ex:=ERtcSocketError.Create(Conn.GetLastErrorText);
            try
              TriggerConnectError(Ex);
            finally
              Ex.Free;
              end;
            end
          else
            TriggerConnectFail; // we have a "Failed connection" here, rather then a Disconnect.
          end
        else
          TriggerDisconnect;
        end
      else
        begin
        if assigned(CryptPlugin) then
          try
            CryptPlugin.AfterDisconnectEx(FCryptObject);
          except
            on E:Exception do
              if LOG_PLUGIN_ERRORS then
                Log('TRtcSocketClientProvider CryptPlugin.AfterDisconnect',E,'PLUGIN');
            end;

        if Lost then
          TriggerConnectLost // TriggerConnectLost will call TriggerDisconnect
        else
          TriggerDisconnect;
        end;
    finally

      State:=conInactive;

      if assigned(Conn) then
        begin
        { We need to remove all events from this connection
          before we can actually destroy our own connection object. }
        Conn.EventsOff:=True;

        try
          Conn.Close;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketClientProvider.OnSessionClosed: Conn.Close',E,'ERROR'); // ignore all errors here
          end;
        try
          Conn.Release;
        except
          on E:Exception do
            if LOG_AV_ERRORS then
              Log('TRtcSocketClientProvider.OnSessionClosed: Conn.Release',E,'ERROR'); // ignore all errors here
          end;
        Conn:=nil;
        end;

      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataReceived(Sender: TObject; ErrCode: Word);
  var
    len:integer;
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if (State=conActivating) then // call "Connected" only after we know that we can relly send data.
      begin
      if FLocalAddr<>'0.0.0.0' then
        begin
        State:=conActive;

        if assigned(CryptPlugin) then
          begin
          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, GetPeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('CryptPlugin.AfterConnect',E,'PLUGIN');
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
      if Proto=proUDP then
        begin
        len:=Conn.ReceiveEx(FReadBuff);
        if (len>=0) then
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
        TriggerDataReceived;
        TriggerReadyToRelease;
        end;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataSent(Sender: TObject; ErrCode: Word);
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if _Visible then
    begin
    if (State=conActivating) then // call "Connected" only after we know that we can relly send data.
      begin
      if FLocalAddr<>'0.0.0.0' then
        begin
        State:=conActive;

        if assigned(CryptPlugin) then
          begin
          SetLength(s_out,0);
          try
            s_res:=CryptPlugin.AfterConnectEx(FCryptObject, s_out, GetCryptProtocol, GetAddr, GetPeerAddr);
          except
            on E:Exception do
              begin
              if LOG_PLUGIN_ERRORS then
                Log('CryptPlugin.AfterConnect',E,'PLUGIN');
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

    if State=conActive then // do not call this when it comes for the first time, if we haven't been sending anything out yet.
      begin
      TriggerDataSent;
      TriggerReadyToRelease;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.wsOnDataOut(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State=conActive then
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

procedure TRtcSocketClientProvider.wsOnDataIn(Sender: TObject; Len: cardinal);
  begin
  if _Visible then
    begin
    if State=conActive then
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

procedure TRtcSocketClientProvider.wsOnBgException(Sender: TObject; E: Exception;
    var CanClose: Boolean);
  begin
  if (E is EClientLimitReached) then
    CanClose:=False
  else
    begin
    CanClose:=True;
    try
      TriggerException(E);
    except
      on E:Exception do
        if LOG_EVENT_ERRORS then
          Log('TRtcSocketClientProvider.OnBgException: TriggerException',E,'ERROR');
        // ignore all exceptions here
      end;
    end;
  end;

function TRtcSocketClientProvider._Active: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]) and assigned(Conn);
  end;

function TRtcSocketClientProvider._Visible: boolean;
  begin
  Result:=not Closing and (FState in [conActive,conActivating]) and assigned(Conn);
  end;

procedure TRtcSocketClientProvider.Check;
  var
    addr:RtcString;
  begin
  if assigned(Conn) then
    begin
    addr:=Conn.GetLocalAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to local address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to local address.');
      end;
    addr:=Conn.GetPeerAddr;
    if addr='0.0.0.0' then
      begin
      if LOG_SOCKET_ERRORS then
        Log('CLOSING from Check. Socket not connected to peer address.','SOCK');
      Conn.Close;
      raise ERtcSocketError.Create('Socket not connected to peer address.');
      end;
    end;
  end;

function TRtcSocketClientProvider.GetClientThread: TRtcThread;
  begin
  Result:=Client_Thread;
  end;

procedure TRtcSocketClientProvider.Connect(Force:boolean=False;Reconnecting:boolean=False);
  begin
  if Reconnecting then
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(Force,True)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcSocketClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSReOpenConn);
      end
    else
      OpenConnection(Force,True);
    end
  else
    begin
    if assigned(Client_Thread) then
      begin
      if inThread then
        OpenConnection(Force,False)
      else
        TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else if GetMultiThreaded then
      begin
      Client_Thread := TRtcSocketClientThread.Create;
      Client_Thread.RtcConn:= self;
      TRtcThread.PostJob(Client_Thread, Message_WSOpenConn);
      end
    else
      OpenConnection(Force,False);
    end;
  end;

procedure TRtcSocketClientProvider.OpenConnection(Force:boolean;Reconnecting:boolean);
  begin
  if (State=conActive) or (State=conActivating) then Exit; // already connected !!!

  if State<>conInactive then
    raise Exception.Create('Can not connect again. Connection in use.');

  if not assigned(SocketClass) then
    raise Exception.Create('TRtcSocketClientProvider -> SocketClass not assigned!');

  try
    if Proto=proUDP then
      SetLength(FReadBuff,0);

    FRealNeedData:=False;
    Lost:=True;
    Closing:=False;
    Silent:=False;
    FRawOut:=0;
    FPlainOut:=0;
    FDataOut:=0;
    FDataIn:=0;
    FLocalAddr:='0.0.0.0';
    FPeerAddr:='0.0.0.0';

    TriggerConnectionOpening(Force);

    try
      Conn:=SocketClass.Create;
      Conn.Blocking:=Blocking;
      Conn.Reconnecting:=Reconnecting;
      Conn.MessageThread:=Client_Thread;

      Conn.TimeoutsOfAPI:=TimeoutsOfAPI;
      with Conn do
        begin
        case Proto of
          proTCP:Protocol:=spTcp;
          proUDP:
            begin
            Protocol:=spUdp;
            UdpMultiCast:=Self.UdpMultiCast;
            UdpMultiCastIpTTL:=Self.UdpMultiCastMaxHops;
            UdpReuseAddr:=Self.UdpReuseAddr;
            end;
          end;

        Addr:=self.GetAddr;
        Port:=self.GetPort;

        MultiThreaded:=assigned(Client_Thread);

        OnBgException:=wsOnBgException;
        OnChangeState:=wsOnChangeState;

        OnDataReceived:=wsOnDataReceived;
        OnDataSent:=wsOnDataSent;
        OnDataOut:=wsOnDataOut;
        OnDataIn:=wsOnDataIn;
        end;

      try
        State:=conActivating;
        Conn.Connect;
      except
        on E:Exception do
          begin
          if _Active then
            begin
            State:=conInactive;
            try
              Conn.EventsOff:=True;
              Conn.Release;
            finally
              Conn:=nil;
              end;
            end;
          raise;
          end;
        end;
    except
      TriggerConnectionClosing;
      raise;
      end;
  except
    on E:EClientLimitReached do // connection limit reached
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcSocketError do // normal (expected) socket exception
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:ERtcFatalSockException do // Fatal socket exception
      begin
      TriggerConnectError(E);
      TriggerReadyToRelease;
      end;
    on E:Exception do
      begin
      TriggerReadyToRelease;
      raise;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.Disconnect;
  begin
  if assigned(Client_Thread) and not inThread then
    TRtcThread.PostJob(Client_Thread, Message_WSCloseConn)
  else
    begin
    Lost:=False;
    InternalDisconnect;
    end;
  end;

procedure TRtcSocketClientProvider.InternalDisconnect;
  var
    s_out:RtcByteArray;
    s_res:TRtcCryptPluginState;
  begin
  if not assigned(Conn) then // not connected
    begin
    Closing:=True;
    Exit; // silent exit if nothing to do.
    end;

  if State in [conActive,conActivating] then
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
        s_res:=cpsClosed;
        try
          s_res:=CryptPlugin.BeforeDisconnectEx(FCryptObject, s_out);
        except
          on E:Exception do
            if LOG_PLUGIN_ERRORS then
              begin
              Log('CryptPlugin.BeforeDisconnect',E,'PLUGIN');
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
      wsOnSessionClosed(self,0);
      end
    else
      begin
      if assigned(CryptPlugin) then
        try
          CryptPlugin.AfterDisconnectEx(FCryptObject);
        except
          on E:Exception do
            if LOG_PLUGIN_ERRORS then
              begin
              Log('CryptPlugin.AfterDisconnect',E,'PLUGIN');
              end;
          end;
      try
        Conn.Close;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketClientProvider.InternalDisconnect: Conn.Close',E,'ERROR'); // ignore all errors here
        end;
      try
        Conn.Release;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('TRtcSocketClientProvider.InternalDisconnect: Conn.Release',E,'ERROR'); // ignore all errors here
        end;
      Conn:=nil;
      end;
    end;
  end;

procedure TRtcSocketClientProvider.Release;
  begin
  if assigned(Client_Thread) then
    TRtcThread.PostJob(Client_Thread, Message_WSRelease, True)
  else
    inherited;
  end;

constructor TRtcSocketClientThread.Create;
  begin
  inherited;
  Releasing:=False;
  RtcConn:=nil;
  end;

procedure TRtcSocketClientThread.OpenConn;
  begin
  RtcConn.OpenConnection(False,False);
  end;

procedure TRtcSocketClientThread.ReOpenConn;
  begin
  RtcConn.OpenConnection(False,True);
  end;

procedure TRtcSocketClientThread.CloseConn;
  begin
  try
    if assigned(RtcConn) then
      begin
      RtcConn.Lost:=False;
      RtcConn.InternalDisconnect;
      end;
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcSocketClientThread.CloseConn : RtConn.InternalDisconnect',E,'ERROR');
    end;
  end;

destructor TRtcSocketClientThread.Destroy;
  begin
  try
    try
      if assigned(RtcConn) then
        begin
        RtcConn.Client_Thread:=nil;
        if Releasing then
          RtcFreeAndNil(RtcConn)
        else
          RtcConn.InternalDisconnect;
        end;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcSocketClientThread.Destroy : RtConn.Free/InternalDisconnect',E,'ERROR');
      end;
    RtcConn:=nil;
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

function TRtcSocketClientThread.RunJob:boolean;
  begin
  Result:=False;
  try
    if Job is TRtcSockMessage then
      begin
      try
        if assigned(RtcConn) then
          if assigned(RtcConn.Conn) then
            RtcConn.Conn.DoMessage(TRtcSockMessage(Job).Msg,0);
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob "DoMessage('+IntToStr(TRtcSockMessage(Job).Msg)+')"',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob "DoMessage('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
              end;
          end;
        end;
      end
    else if Job is TRtcSocketMessageJob then
      begin
      try
        if assigned(RtcConn) then
          begin
          if assigned(RtcConn.Conn) then
            Result:=inherited RunJob
          else
            TRtcJob(Job).Kill;
          end
        else
          TRtcJob(Job).Kill;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+IntToStr(TRtcSocketMessageJob(Job).Msg)+')"',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob "TRtcSocketMessageJob('+E2.ClassName+':'+E2.Message+')"',E,'ERROR');
              end;
          end;
        end;
      end
    else if Job=Message_WSOpenConn then
      begin
      try
        OpenConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSOpenConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSReOpenConn then
      begin
      try
        ReOpenConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSReOpenConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSCloseConn then
      begin
      try
        CloseConn;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSCloseConn"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSStop then
      begin
      try
        RtcConn:=nil;
        Result:=True;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSStop"',E,'ERROR');
          end;
        end;
      end
    else if Job=Message_WSRelease then
      begin
      try
        Releasing:=True;
        Result:=True;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            Log('CLI:TRtcSocketClientThread.RunJob "WSRelease"',E,'ERROR');
          end;
        end;
      end
    else
      begin
      try
        Result:=inherited RunJob;
      except
        on E:Exception do
          begin
          Result:=True;
          if LOG_AV_ERRORS then
            try
              Log('CLI:TRtcSocketClientThread.RunJob (else: "'+Job.ClassName+'")',E,'ERROR');
            except
              on E2:Exception do
                Log('CLI:TRtcSocketClientThread.RunJob (else: *ClassError* "'+E2.ClassName+':'+E2.Message+'")',E,'ERROR');
              end;
          end;
        end;
      end;
  except
    on E:Exception do
      begin
      Result:=True;
      if LOG_AV_ERRORS then
        Log('CLI:TRtcSocketClientThread.RunJob *AV*',E,'ERROR');
      end;
    end;
  end;

procedure TRtcSocketClientProvider.CryptNeedMoreData;
  begin
  if assigned(Conn) then Conn.NeedMoreData;
  end;

procedure TRtcSocketClientProvider.NeedMoreData;
  begin
  if assigned(Conn) then
    begin
    FRealNeedData:=True;
    Conn.NeedMoreData;
    end;
  end;

function TRtcSocketClientProvider.GetCryptProtocol: TRtcCryptPluginProtocol;
  begin
  Result:=cppTcp;
  end;

type
  TMySocketCliProvUnit=class
    public
    constructor Create;
    destructor Destroy; override;
    end;

var
  mySock:TMySocketCliProvUnit;

{ TMySocketCliProv }

constructor TMySocketCliProvUnit.Create;
  begin
  inherited;
  Message_WSOpenConn:=TRtcBaseMessage.Create;
  Message_WSReOpenConn:=TRtcBaseMessage.Create;
  Message_WSCloseConn:=TRtcBaseMessage.Create;
  Message_WSStop:=TRtcBaseMessage.Create;
  Message_WSRelease:=TRtcBaseMessage.Create;
  end;

destructor TMySocketCliProvUnit.Destroy;
  begin
  try
    RtcFreeAndNil(Message_WSOpenConn);
    RtcFreeAndNil(Message_WSReOpenConn);
    RtcFreeAndNil(Message_WSCloseConn);
    RtcFreeAndNil(Message_WSStop);
    RtcFreeAndNil(Message_WSRelease);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TMySocketCliProv.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Initializing ...','DEBUG');{$ENDIF}

mySock:=TMySocketCliProvUnit.Create;

{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Finalizing ...','DEBUG');{$ENDIF}

CloseThreadPool;

RtcFreeAndNil(mySock);

{$IFDEF RTC_DEBUG} Log('rtcSocketCliProv Finalized.','DEBUG');{$ENDIF}
end.

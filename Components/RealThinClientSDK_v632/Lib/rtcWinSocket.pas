{
  "Low-level Async WinSock API sockets access class"

  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  Even though the current implementation of this unit is my own design
  which has very little (if anything) to do with the first version
  which was based on the TCustomWSocket class from ICS by F. Piette,
  I would still like to give credit to F. Piette, who has made my
  first steps into the async WinSock API world a lot easier ...

  +--------------------------------------------------------------+
  |  Copyright (C) 1996-2001 by Fran�ois PIETTE                  |
  |  for the TCustomWSocket class from ICS v4.34 (Sep 08, 2001)  |
  +--------------------------------------------------------------+

  This unit is ONLY for MS Windows.

  @exclude
}

{$INCLUDE rtcDefs.inc}

unit rtcWinSocket;

interface

uses
  Messages,
  Windows,
  SysUtils,

  rtcTypes,
  rtcConn,

  rtcInfo,
  rtcFastStrings,
  rtcSyncObjs,
  rtcLog,
  rtcSocketPool,
  rtcHWndPool,

  rtcSockBase,
  rtcSynAPI,

  rtcWinSock;

const
  WM_ASYNCSELECT_FIRST      = WM_USER + 1024;
  WM_ASYNCSELECT_LAST       = $7FFF;

  WM_SOCKET_FIRST           = WM_ASYNCSELECT_FIRST;
  WM_SOCKET_LAST            = WM_ASYNCSELECT_LAST;

type
  TRtcWinSocket = class(TRtcSocketBase)
  private
    FProtoStr:RtcString;
    FProto:integer;
    FProtoType:integer;

    FSrc     : TSockAddr;
    FSrcLen  : Integer;

    FLevel : integer;

    FWantRelease: Boolean;

    FSending            : Boolean;
    FPostNotify         : Boolean;
    bCanSend, bAllSent  : Boolean;
    bCanRead            : Boolean;

    FLastError          : Integer;
    FWindowHandle       : HWND;
    FMessageCode        : Cardinal;

    FHSocket            : TSocket;

    FMyPort             : RtcString;
    FMyPortNum          : Integer;
    FMyAddr             : RtcString;     { IP address for local interface to use }

    FSendNowBuff        : RtcByteArray;
    FSendNowAt          : integer;
    FSendBuffer         : TRtcHugeByteArray;
    FMaxSendSize        : integer;

    FDnsLookupHandle    : THandle;

    FState              : TRtcSocketState;
    FSelectEvent        : LongInt;

    FBuffered    : RtcByteArray;

    FLastPeerAddr,
    FLastPeerPort: RtcString;

    function   UpdateAsyncSelect:boolean;
    function   EnableMsg(flags:LongInt):boolean;
    function   DisableMsg(flags:LongInt):boolean;
    function   DisableAllMsgs:boolean;
    procedure  PostMsg(flags:LongInt);

    function   StartingConnect:boolean;
    function   StartingNewSocket:boolean;
    function   StartingListen:boolean;

    procedure   SocketError(const sockfunc: String; silent:boolean=True);
    procedure   RealSocketError(const sockfunc: String; silent:boolean=True);

    procedure   RaiseExceptionFmt(const Fmt : String; const args : array of const; silent:boolean=True);
    procedure   RaiseException(const Msg : String; silent:boolean=True);

    procedure   WMASyncSelect(lParamLo,lParamHi:word);

    procedure   ChangeState(NewState : TRtcSocketState);
    procedure   TryToSend;

    procedure   AssignDefaultValue;

    procedure   SetMyAddr(const Value : RtcString);
    procedure   SetMyPort(const Value : RtcString);
    function    GetMyAddr: RtcString;
    function    GetMyPort: RtcString;
    procedure   BindSocket;

    function    RealSend(const Data; Len : Integer; triggerEvent:boolean=True) : Integer;
    function    RealRecv(var Data; Len : Integer; triggerEvent:boolean=True) : Integer;

    procedure   CancelDnsLookup;

    procedure   SetLingerOption(abortiveclose:boolean=False);

    procedure   DeleteBufferedData;

    procedure   InternalClose(abortiveclose:boolean=False);

  protected
    TempSock: TSocket;
    sin         : TSockAddr;

    procedure   Open;

    function    Accept: TSocket;

    function    GetRcvdCount: LongInt;

    function    GetState : TRtcSocketState;
    function    GetAllSent : Boolean;

    function    GetLastError : Integer;

    procedure   Abort;

    procedure   DoConnect(Err:word);
    procedure   DoClose(Err:word);
    procedure   DoRead(Err:word);
    procedure   DoWrite(Err:word);
    procedure   DoAccept(Err:word);

    procedure   DoXOpen(Err:word);
    procedure   DoXClose(Err:word);

    procedure   DoXRead(Err:word);
    procedure   DoXWrite(Err:word);

    function GetWindowHandle:HWND;

    procedure HandleBackGroundException(E: Exception);

    property Handle : HWND              read  GetWindowHandle;
    property MessageCode : Cardinal     read  FMessageCode;

  public
    constructor Create; override;
    destructor  Destroy; override;

    procedure   Release; override;

    procedure   SetAddr(const Value : RtcString); override;
    procedure   SetPort(const Value : RtcString); override;

    procedure   Connect; override;
    procedure   Listen; override;
    procedure   Close; override;

    function    GetLocalAddr: RtcString; override;
    function    GetLocalPort: RtcString; override;

    function    GetNewSocket:TRtcSocketBase; override;
    procedure   StartNewSocket; override;

    procedure   NeedMoreData; override;

    function    ReceiveEx(var Str : RtcByteArray):integer; override;
    function    SendEx(const Str : RtcByteArray):integer; override;
    function    BuffEx(const Str : RtcByteArray):integer; override;

    function    ReceiveStr(var Str : RtcString):integer; override;
    function    SendStr(const Str : RtcString):integer; override;
    function    BuffStr(const Str : RtcString):integer; override;

    function    GetPeerAddr: RtcString; override;
    function    GetPeerPort: RtcString; override;

    procedure   SetProtocol(const Value:TRtcSocketProtocol); override;

    procedure   DoMessage(Msg:Word; Data:Cardinal); override;

    function    GetLastErrorText : String; override;

    property MyPort : RtcString        read  GetMyPort write SetMyPort;
    property MyAddr : RtcString        read  GetMyAddr write SetMyAddr;

    property PeerAddr : RtcString      read  GetPeerAddr;
    property PeerPort : RtcString      read  GetPeerPort;
  end;

implementation

const
  MSG_CONNECT = 0;
  MSG_CLOSE = 1;
  MSG_READ = 2;
  MSG_WRITE = 3;
  MSG_ACCEPT = 4;

  MSG_XOPEN = 5;
  MSG_XREAD = 6;
  MSG_XWRITE = 7;
  MSG_XCLOSE = 8;

var
  CS:TRtcCritSec;
  MyMsgCode:Cardinal;

function rtcGetNextMsgCode:Cardinal;
  begin
  CS.Acquire;
  try
    Result:=MyMsgCode;
    if MyMsgCode<WM_ASYNCSELECT_LAST then
      MyMsgCode:=MyMsgCode+1
    else
      MyMsgCode:=WM_ASYNCSELECT_FIRST;
  finally
    CS.Release;
    end;
  end;

constructor TRtcWinSocket.Create;
  begin
  inherited Create;

  FLastPeerAddr:='';
  FLastPeerPort:='';

  SetLength(FBuffered,0);

  FSending := False;
  Protocol := spTcp;
  TempSock := INVALID_SOCKET;

  FLevel := 0;
  FWantRelease := False;

  FWindowHandle:=0;

  FSendBuffer := TRtcHugeByteArray.Create;
  SetLength(FSendNowBuff,0);
  FSendNowAt := 0;

  FMultiCastIpTTL := IP_DEFAULT_MULTICAST_TTL;

  AssignDefaultValue;
  end;

destructor TRtcWinSocket.Destroy;
  begin
  try
    Abort;
    if TempSock<>INVALID_SOCKET then
      begin
      _Shutdown(TempSock,SD_BOTH);
      _CloseSocket(TempSock);
      TempSock:=INVALID_SOCKET;
      end;

    RtcFreeAndNil(FSendBuffer);
    SetLength(FSendNowBuff,0);
    FSendNowAt:=0;

    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcWinSocket.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcWinSocket.AssignDefaultValue;
  begin
  FSrcLen:=0;

  FillChar(sin, Sizeof(sin), 0);

  FMyPort         := '0';
  FMyAddr         := '0.0.0.0';

  FHSocket           := INVALID_SOCKET;
  FMessageCode       := 0;
  FSelectEvent       := 0;
  FState             := wsClosed;

  FPostNotify        := False; // need to post FD_WRITE notification?

  bCanSend           := False;
  bCanRead           := False;
  bAllSent           := True;
  end;

procedure TRtcWinSocket.RaiseException(const Msg : String; silent:boolean=True);
  begin
  if LOG_SOCKET_ERRORS then
    if FLastPeerAddr<>'' then
      Log(String(FLastPeerAddr)+':'+String(FLastPeerPort)+' > '+Msg,'SOCK')
    else
      Log(Msg,'SOCK');
  if not silent then
    raise ERtcSocketError.Create(Msg);
  end;

procedure TRtcWinSocket.RaiseExceptionFmt(const Fmt : String; const args : array of const; silent:boolean=True);
  begin
  if LOG_SOCKET_ERRORS then
    if FLastPeerAddr<>'' then
      Log(String(FLastPeerAddr)+':'+String(FLastPeerPort)+' > '+Format(Fmt,args),'SOCK')
    else
      Log(Format(Fmt,args),'SOCK');
  if not silent then
    raise ERtcSocketError.CreateFmt(Fmt, args);
  end;

procedure TRtcWinSocket.HandleBackGroundException(E: Exception);
  var
    CanAbort : Boolean;
  begin
  CanAbort := TRUE;
  { First call the error event handler, if any }
  try
    On_BgException(E, CanAbort);
  except
    on E:Exception do
      if LOG_AV_ERRORS then
        Log('TRtcWinSocket.OnBgException',E,'ERROR');
    end;
  { Then abort the socket }
  if CanAbort then
    begin
    try
      Abort;
    except
      on E:Exception do
        if LOG_AV_ERRORS then
          Log('TRtcWinSocket.Abort',E,'ERROR');
      end;
    end;
  end;

function TRtcWinSocket.GetWindowHandle: HWND;
  begin
  if FWindowHandle=0 then
    begin
    FWindowHandle := rtcGetHWND(MultiThreaded);
    if FWindowHandle = 0 then
      RaiseException('Cannot create a hidden window for TWSocket',False);
    end;
  Result:=FWindowHandle;
  end;

function TRtcWinSocket.GetAllSent: Boolean;
  begin
  Result:=bAllSent;
  end;

function TRtcWinSocket.GetLastError: Integer;
  begin
  Result:=FLastError;
  end;

function TRtcWinSocket.GetState: TRtcSocketState;
  begin
  Result:=FState;
  end;

function TRtcWinSocket.GetMyAddr: RtcString;
  begin
  Result:=FMyAddr;
  end;

function TRtcWinSocket.GetMyPort: RtcString;
  begin
  Result:=FMyPort;
  end;

function TRtcWinSocket.GetLastErrorText: String;
  begin
  if GetLastError=0 then
    Result:=''
  else
    Result:='#'+IntToStr(GetLastError)+': '+WSocket_ErrorDesc(GetLastError);
  end;

procedure TRtcWinSocket.ChangeState(NewState : TRtcSocketState);
  begin
  if FState <> NewState then
    begin
    FState := NewState;
    On_ChangeState(NewState);
    end;
  end;

procedure TRtcWinSocket.SetPort(const Value : RtcString);
  begin
  if FState <> wsClosed then
    begin
    RaiseException('Cannot change Port if not closed',False);
    Exit;
    end;
  FPort := Trim(Value);
  end;

procedure TRtcWinSocket.SetMyPort(const Value : RtcString);
  begin
  if FState <> wsClosed then
    begin
    RaiseException('Cannot change MyPort if not closed',False);
    Exit;
    end;
  FMyPort := Value;
  end;

procedure TRtcWinSocket.SetMyAddr(const Value : RtcString);
  begin
  if FState <> wsClosed then
    begin
    RaiseException('Cannot change MyAddr if not closed',False);
    Exit;
    end;
  if Length(Value) = 0 then
    FMyAddr := '0.0.0.0'
  else
    FMyAddr := Value;
  end;

function TRtcWinSocket.GetLocalPort: RtcString;
  var
    saddr    : TSockAddr;
    saddrlen : integer;
  begin
  Result := 'error';
  if FState in [wsConnected, wsBound, wsListening] then
    begin
    saddrlen := SizeOf(saddr);
    if _GetSockName(FHSocket, saddr, saddrlen) = 0 then
      Result := Int2Str(WSocket_GetSinPort(saddr));
    end;
  end;

function TRtcWinSocket.GetLocalAddr: RtcString;
  var
    saddr    : TSockAddr;
    saddrlen : integer;
  begin
  Result := '0.0.0.0';
  if FState in [wsConnected, wsBound, wsListening] then
    begin
    saddrlen := SizeOf(saddr);
    if _GetSockName(FHSocket, saddr, saddrlen) = 0 then
      Result := WSocket_GetSinIP(saddr);
    end;
  end;

procedure TRtcWinSocket.SetAddr(const Value : RtcString);
  begin
  if FState <> wsClosed then
    begin
    RaiseException('Cannot change Addr if not closed',False);
    Exit;
    end;
  FAddr := Trim(Value);
  end;

function TRtcWinSocket.GetPeerAddr: RtcString;
  var
    saddr    : TSockAddr;
    saddrlen : integer;
    Res: integer;
  begin
  if Protocol=spTcp then
    begin
    Result := '0.0.0.0';
    if FState = wsConnected then
      begin
      saddrlen := sizeof(saddr);
      Res:=_GetPeerName(FHSocket, saddr, saddrlen);
      if Res = 0 then
        begin
        Result := WSocket_GetSinIP(saddr);
        FLastPeerAddr := Result;
        end
      else
        RealSocketError('GetPeerAddr');
      end;
    end
  else if (FSrcLen>0) then
    begin
    Result:=WSocket_GetSinIP(FSrc);
    FLastPeerAddr:=Result;
    end
  else
    begin
    Result:=WSocket_GetSinIP(Sin);
    FLastPeerAddr:=Result;
    end;
  end;

function TRtcWinSocket.GetPeerPort: RtcString;
  var
    saddr    : TSockAddr;
    saddrlen : integer;
    Res : integer;
  begin
  if Protocol=spTcp then
    begin
    Result := 'error';
    if FState = wsConnected then
      begin
      saddrlen := sizeof(saddr);
      Res:=_GetPeerName(FHSocket, saddr, saddrlen);
      if Res = 0 then
        begin
        Result := Int2Str(WSocket_GetSinPort(saddr));
        FLastPeerPort := Result;
        end
      else
        RealSocketError('GetPeerPort');
      end;
    end
  else if FSrcLen>0 then
    begin
    Result:=Int2Str(WSocket_GetSinPort(FSrc));
    FLastPeerPort:=Result;
    end
  else
    begin
    Result:=Int2Str(WSocket_GetSinPort(Sin));
    FLastPeerPort:=Result;
    end;
  end;

procedure TRtcWinSocket.CancelDnsLookup;
  var
    Res:integer;
  begin
  if FDnsLookupHandle = 0 then Exit;

  Res:=_WSACancelAsyncRequest(FDnsLookupHandle);
  FDnsLookupHandle := 0;
  if Res = 0 then
    On_DnsLookupDone(WSAEINTR);
  end;

procedure TRtcWinSocket.BindSocket;
  var
    SockName      : TSockAddr;
    SockNamelen   : Integer;
    LocalSockName : TSockAddr;
    lasterr       : longint;
  begin
  WSocket_SetVarSin(LocalSockName, FMyAddr, FMyPort, AF_UNSPEC, FProto, FProtoType, True);

  SockNamelen := SizeOfSockAddr(LocalSockName);
  if _bind(FHSocket, LocalSockName, SockNamelen) <> 0 then
      begin
      lasterr:=_WSAGetLastError;
      RaiseExceptionFmt('winsock.bind failed, error #%d', [lasterr], False);
      Exit;
      end;
  SockNamelen := sizeof(SockName);
  if _getsockname(FHSocket, SockName, SockNamelen) <> 0 then
      begin
      lasterr:=_WSAGetLastError;
      RaiseExceptionFmt('winsock.getsockname failed, error #%d', [lasterr], False);
      Exit;
      end;
  FMyPortNum := WSocket_GetSinPort(SockName);
  FMyPort    := Int2Str(FMyPortNum);
  end;

procedure TRtcWinSocket.SetLingerOption(abortiveclose:boolean=False);
  var
    li      : TLinger;
  begin
  if abortiveclose then
    begin
    // Linger = True, Timeout = 0, HARD close
    li.l_onoff := 1;
    li.l_linger := 0;
    end
  else
    begin
    // Linger = False, Timeout = 0, graceful close
    li.l_onoff := 0;
    li.l_linger := 0;
    end;
  _setsockopt(FHSocket, SOL_SOCKET, SO_LINGER, @li, SizeOf(li));
  end;

procedure TRtcWinSocket.Connect;
  var
    iStatus : integer;
    optval  : integer;
    optlen  : integer;
    lAddr   : TInAddr;
  begin
  LoadWinSock;

  FLastPeerAddr:='';
  FLastPeerPort:='';

  FState:=wsClosed;
  if (FPort='') then
    begin
    RaiseException('Connect: No Port Specified', False);
    Exit;
    end;
  if (FAddr='') then
    begin
    RaiseException('Connect: No IP Address Specified', False);
    Exit;
    end;
  try
    iStatus:=WSocket_SetVarSin(Sin,FAddr,FPort,AF_UNSPEC,FProto,FProtoType,True);
    { The next line will trigger an exception in case of failure }
  except
    on E:Exception do
      begin
      RaiseException('connect: ' + E.Message, False);
      Exit;
      end;
    end;

  if iStatus<>0 then
    begin
    RealSocketError('Connect (WSocket_SetVarSin)',False);
    Exit;
    end;

  { Remove any data from the internal output buffer }
  { (should already be empty !)                     }
  DeleteBufferedData;

  FHSocket := _socket(sin.sin_family, FProtoType, FProto);
  if FHSocket = INVALID_SOCKET then
    begin
    RealSocketError('Connect (socket)',False);
    Exit;
    end;
  FMessageCode := rtcGetNextMsgCode;
  rtcStoreSocket(self, FHSocket);

  ChangeState(wsOpened);

  if FState <> wsOpened then
    begin  { 07/07/02 }
    { Socket has been closed in the OnChangeState event ! }
    FLastError:=WSAEINVAL;
    SocketError('Connect (Invalid operation in OnChangeState)',False);
    Exit;
    end;

  if FProtoType = SOCK_DGRAM then
    begin
    BindSocket;
    if FMultiCast then
      begin
      if FMultiCastIpTTL <> IP_DEFAULT_MULTICAST_TTL then
        begin
        optval := FMultiCastIpTTL; { set time-to-live for multicast }
        iStatus := _setsockopt(FHSocket, IPPROTO_IP, IP_MULTICAST_TTL, @optval, SizeOf(optval));
        if iStatus <> 0 then
          begin
          RealSocketError('setsockopt(IP_MULTICAST_TTL)',False);
          Exit;
          end;
        end;
      if FMyAddr <> '0.0.0.0' then
        begin                      { RK }
        laddr.s_addr := WSocket_ResolveHost(FMyAddr).s_addr;
        iStatus      := _SetSockOpt(FHSocket, IPPROTO_IP, IP_MULTICAST_IF, @laddr, SizeOf(laddr));
        if iStatus <> 0 then
          begin
          RealSocketError('setsockopt(IP_MULTICAST_IF)',False);
          Exit;
          end;
        end;                                                       { /RK }
      end;
    if (FAddr=cBroadcast) or (FAddr=c6Broadcast) then
      begin
      OptVal  := 1;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_BROADCAST, @OptVal, SizeOf(OptVal));
      if iStatus <> 0 then
        begin
        RealSocketError('setsockopt(SO_BROADCAST)',False);
        Exit;
        end;
      end;
    end
  else { FProtoType = SOCK_STREAM }
    begin
    optval  := -1;
    iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SO_REUSEADDR)',False);
      Exit;
      end;
    optval := -1; { -1=true, 0=false }
    iStatus := _setsockopt(FHsocket, IPPROTO_TCP, TCP_NODELAY, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(IPPROTO_TCP, TCP_NODELAY)',False);
      Exit;
      end;
    optval  := -1;
    iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_KEEPALIVE, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SO_KEEPALIVE)',False);
      Exit;
      end;
  {$IFDEF RTC_USESETTIMEOUTS}
    if assigned(TimeoutsOfAPI) then
      begin
      if TimeoutsOfAPI.ReceiveTimeout > 0 then
        begin
        optval := TimeoutsOfAPI.ReceiveTimeout * 1000;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
        if iStatus <> 0 then
          begin
          RealSocketError('Error setting socket receive timeout',False);
          Exit;
          end;
        end;
      if TimeoutsOfAPI.SendTimeout > 0 then
        begin
        optval := TimeoutsOfAPI.SendTimeout * 1000;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
        if iStatus <> 0 then
          begin
          RealSocketError('Error setting socket send timeout',False);
          Exit;
          end;
        end;
      end;
  {$ELSE RTC_USESETTIMEOUTS}
    optval := SOCK_RECV_TIMEOUT;
    {iStatus :=} _setsockopt(FHsocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
    (* if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_RCVTIMEO)',False);
      Exit;
      end; *)
    optval := SOCK_SEND_TIMEOUT;
    {iStatus :=} _setsockopt(FHsocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
    (* if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_SNDTIMEO)',False);
      Exit;
      end; *)
  {$ENDIF RTC_USESETTIMEOUTS}
    optval := SOCK_READ_BUFFER_SIZE;
    iStatus := _setsockopt(FHsocket, SOL_SOCKET, SO_RCVBUF, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_RCVBUF)',False);
      Exit;
      end;
    optval := SOCK_SEND_BUFFER_SIZE;
    iStatus := _setsockopt(FHsocket, SOL_SOCKET, SO_SNDBUF, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_SNDBUF)',False);
      Exit;
      end;
    optval := 0;
    optlen := sizeof(optval);
    iStatus := _getsockopt(FHsocket, SOL_SOCKET, SO_SNDBUF, @optval, optlen);
    if iStatus <> 0 then
      begin
      RealSocketError('getsockopt(SOL_SOCKET, SO_SNDBUF)',False);
      Exit;
      end;

    if optlen=sizeof(optval) then
      begin
      FMaxSendSize:=optval;
      if (FMaxSendSize=0) or (FMaxSendSize>SOCK_MAX_SEND_SIZE) then
        FMaxSendSize:=SOCK_MAX_SEND_SIZE;
      end
    else
      FMaxSendSize:=SOCK_MAX_SEND_SIZE;

    SetLingerOption;

    if (FMyPortNum <> 0) or (FMyAddr <> '0.0.0.0') then
      BindSocket;
    end;

  PostMsg(FDX_OPEN);
  end;

procedure TRtcWinSocket.Open;
  var
    iStatus: integer;
  begin
  if StartingConnect then
    begin
    if FProtoType = SOCK_DGRAM then
      ChangeState(wsConnecting)
    else
      begin
      iStatus := _connect(FHSocket, sin, SizeOfSockAddr(sin));
      if iStatus=0 then // waiting for FD_CONNECT ...
        ChangeState(wsConnecting)
      else
        begin
        FlastError:=_WSAGetLastError;
        if FLastError=WSAEWOULDBLOCK then
          ChangeState(wsConnecting)
        else
          begin
          SocketError('Connect');
          Abort;
          end;
        end;
      end;
    end
  else
    begin
    RealSocketError('StartingConnect');
    Abort;
    end;
  end;

procedure TRtcWinSocket.Listen;
  type
    ip_mreq = record
        imr_multiaddr : in_addr;
        imr_interface : in_addr;
    end;
  var
    blog,
    optval,
    iStatus        : Integer;
    mreq    : ip_mreq;
    szAddr : RtcByteArray;
  begin
  LoadWinSock;
  FLastPeerAddr:='';
  FLastPeerPort:='';
  if (FPort='') then
    begin
    FLastError:=WSAEINVAL;
    SocketError('listen: port not assigned',False);
    Exit;
    end;
  if (FAddr='') then
    begin
    FLastError:=WSAEINVAL;
    SocketError('listen: address not assigned',False);
    Exit;
    end;
  try
    { The next line will trigger an exception in case of failure }
    WSocket_SetVarSin(Sin,FAddr,FPort,AF_UNSPEC,FProto,FProtoType,True);
  except
    on E:Exception do
      begin
      RaiseException('listen: ' + E.Message, False);
      Exit;
      end;
    end;

  DeleteBufferedData;

  FHSocket := _socket(sin.sin_family, FProtoType, FProto);
  if FHSocket = INVALID_SOCKET then
    begin
    RealSocketError('socket',False);
    exit;
    end;
  FMessageCode := rtcGetNextMsgCode;
  rtcStoreSocket(self, FHSocket);

  if FProtoType = SOCK_DGRAM then
    begin
    if FReuseAddr then
      begin
      { Enable multiple tasks to listen on duplicate address and port }
      optval  := -1;
      iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));
      if iStatus <> 0 then
        begin
        RealSocketError('setsockopt(SO_REUSEADDR)',False);
        Exit;
        end;
      end;
    end;

  iStatus := _bind(FHSocket, TSockAddr(sin), sizeof(sin));
  if iStatus = 0 then
    ChangeState(wsBound)
  else
    begin
    RealSocketError('Bind',False);
    Exit;
    end;

  if FProtoType = SOCK_DGRAM then
    begin
    if FMultiCast then
      begin
      { Use setsockopt() to join a multicast group }
      { mreq.imr_multiaddr.s_addr := Inet_addr('225.0.0.37');}
      { mreq.imr_multiaddr.s_addr :=  sin.sin_addr.s_addr;}
      { mreq.imr_multiaddr.s_addr :=  Inet_addr(FAddrStr);}
      SetLength(szAddr,0);
      szAddr:=RtcStringToBytesZero(FMultiCastAddr);
      mreq.imr_multiaddr.s_addr :=  _inet_addr(@szAddr[0]);
    { mreq.imr_interface.s_addr := htonl(INADDR_ANY);} { RK}
      mreq.imr_interface.s_addr := WSocket_ResolveHost(FAddr).s_addr;
      iStatus := _setsockopt(FHSocket, IPPROTO_IP, IP_ADD_MEMBERSHIP, @mreq, SizeOf(mreq));
      if iStatus <> 0 then
        begin
        RealSocketError('setsockopt(IP MULTICAST)',False);
        Exit;
        end;
      end;
    if StartingListen then
      begin
      ChangeState(wsListening);
      if FHSocket<>INVALID_SOCKET then
        ChangeState(wsConnected);
      end
    else
      RealSocketError('StartingListen',False);
    end
  else
    begin
    optval := -1; { -1=true, 0=false }
    iStatus := _setsockopt(FHSocket, IPPROTO_TCP, TCP_NODELAY, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('SetSockOpt(TC_NODELAY)',False);
      Exit;
      end;
    optval  := -1;
    iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_KEEPALIVE, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('SetSockOpt(SO_KEEPALIVE)',False);
      Exit;
      end;
  {$IFDEF RTC_USESETTIMEOUTS}
    if assigned(TimeoutsOfAPI) then
      begin
      if TimeoutsOfAPI.ReceiveTimeout > 0 then
        begin
        optval := TimeoutsOfAPI.ReceiveTimeout * 1000;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
        if iStatus <> 0 then
          RealSocketError('Error setting socket receive timeout',False);
        end;
      if TimeoutsOfAPI.SendTimeout > 0 then
        begin
        optval := TimeoutsOfAPI.SendTimeout * 1000;
        iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
        if iStatus <> 0 then
          RealSocketError('Error setting socket send timeout',False);
        end;
      end;
  {$ELSE RTC_USESETTIMEOUTS}
    optval := SOCK_RECV_TIMEOUT;
    {iStatus :=} _setsockopt(FHSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
    (* if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_RCVTIMEO)',False);
      Exit;
      end; *)
    optval := SOCK_SEND_TIMEOUT;
    {iStatus :=} _setsockopt(FHSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
    (* if iStatus <> 0 then
      begin
      RealSocketError('setsockopt(SOL_SOCKET, SO_SNDTIMEO)',False);
      Exit;
      end; *)
  {$ENDIF}
    optval := SOCK_READ_BUFFER_SIZE;
    iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_RCVBUF, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('SetSockOpt(SO_RECVBUF)',False);
      Exit;
      end;
    optval := SOCK_SEND_BUFFER_SIZE;
    iStatus := _setsockopt(FHSocket, SOL_SOCKET, SO_SNDBUF, @optval, SizeOf(optval));
    if iStatus <> 0 then
      begin
      RealSocketError('SetSockOpt(SO_SNDBUF)',False);
      Exit;
      end;

    SetLingerOption;

    if StartingListen then
      begin
      blog:=LISTEN_BACKLOG;
      if blog>SOMAXCONN then blog:=SOMAXCONN;

      iStatus := _listen(FHSocket, blog);
      if iStatus <> 0 then
        begin
        RealSocketError('Listen',False);
        Exit;
        end;
      ChangeState(wsListening);
      end
    else
      RealSocketError('StartingListen',False);
    end;
  end;

function TRtcWinSocket.Accept: TSocket;
  var
    len     : integer;
  begin
  len := sizeof(sin);
  Result := _accept(FHSocket, sin, len);
  end;

function TRtcWinSocket.GetNewSocket: TRtcSocketBase;
  var
    HSock:TSocket;
    cl:TRtcSocketBaseClass;
  begin
  HSock:=Accept;
  if HSock=INVALID_SOCKET then
    Result:=nil
  else
    begin
    cl:=TRtcSocketBaseClass(ClassType);
    Result:=cl.Create;
    TRtcWinSocket(Result).TempSock:=HSock;
    end;
  end;

procedure TRtcWinSocket.StartNewSocket;
  var
    iStatus : Integer;
    optval,
    optlen : integer;
  begin
  if (TempSock = 0) or (TempSock = INVALID_SOCKET) then
    begin
    FLastError:=WSAEINVAL;
    SocketError('StartNewSocket',False);
    Exit;
    end;

  FHSocket := TempSock;
  TempSock:=INVALID_SOCKET;
  FMessageCode := rtcGetNextMsgCode;
  rtcStoreSocket(self, FHSocket);

  optval := 0;
  optlen := sizeof(optval);
  iStatus := _getsockopt(FHsocket, SOL_SOCKET, SO_SNDBUF, @optval, optlen);
  if iStatus <> 0 then
    begin
    RealSocketError('getsockopt(SOL_SOCKET, SO_SNDBUF)',False);
    Exit;
    end;
  if optlen=sizeof(optval) then
    begin
    FMaxSendSize:=optval;
    if (FMaxSendSize=0) or (FMaxSendSize>SOCK_MAX_SEND_SIZE) then
      FMaxSendSize:=SOCK_MAX_SEND_SIZE;
    end
  else
    FMaxSendSize:=SOCK_MAX_SEND_SIZE;

  SetLingerOption;

  FState := wsConnecting;
  if not StartingNewSocket then
    begin
    RealSocketError('StartingNewSocket',False);
    Exit;
    end;
  end;

procedure TRtcWinSocket.DeleteBufferedData;
  begin
  FSendBuffer.Clear;
  SetLength(FSendNowBuff,0);
  FSendNowAt:=0;
  end;

procedure TRtcWinSocket.Release;
  begin
  if FLevel=0 then
    begin
    Inc(FLevel); // make sure nobody tries to release the object by calling release again ...

    if FHSocket<>INVALID_SOCKET then
      begin
      try
        Abort;
      except
        on E:Exception do
          if LOG_AV_ERRORS then
            Log('Release: Abort',E,'ERROR');
        end;
      end;

    bCanRead:=False;
    bCanSend:=False;
    FPostNotify:=False;

    Free;
    end
  else
    FWantRelease:=True;
  end;

procedure TRtcWinSocket.Abort;
  begin
  InternalClose(True);
  end;

procedure TRtcWinSocket.Close;
  begin
  InternalClose(False);
  end;

procedure TRtcWinSocket.InternalClose(abortiveclose:boolean);
  procedure ReadTheRest;
    var
      rcv:integer;
    begin
    // make sure we have read all we have received (clean shutdown)
    rcv:=GetRcvdCount;
    while rcv>0 do
      begin
      Setlength(FBuffered,length(FBuffered)+rcv);
      if RealRecv(FBuffered[length(FBuffered)-rcv],rcv,False)=rcv then
        rcv:=GetRcvdCount
      else
        begin
        SetLength(FBuffered,length(FBuffered)-rcv);
        rcv:=0;
        end;
      end;
    end;
  begin
  if not rtcRemoveSocket(self) then Exit;

  // stop receiving notifications
  DisableAllMsgs;

  CancelDnsLookup;
  DeleteBufferedData;

  bCanRead:=False;
  SetLength(FBuffered,0);
  ReadTheRest;

  if abortiveclose then
    SetLingerOption(True);

  // shut down sending side
  _shutdown(FHSocket,SD_SEND);
  ReadTheRest;

  // shut down receiving side
  _shutdown(FHSocket,SD_RECEIVE);
  ReadTheRest;

  _closesocket(FHSocket);
  FHSocket := INVALID_SOCKET;

  ChangeState(wsClosed);
  end;

procedure TRtcWinSocket.SocketError(const sockfunc: String; silent:boolean=True);
  var
    Line   : String;
  begin
  Line  := 'Error #'+ IntToStr(FLastError) + ' in "' + sockfunc +'": '+WSocket_ErrorDesc(FLastError);
  RaiseException(Line,silent);
  end;

procedure TRtcWinSocket.RealSocketError(const sockfunc: String; silent:boolean=True);
  begin
  FLastError:=_WSAGetLastError;
  SocketError(sockfunc,silent);
  end;

procedure TRtcWinSocket.SetProtocol(const Value: TRtcSocketProtocol);
  begin
  FProtocol:=Value;
  case Value of
    spTcp:begin
          FProtoStr:=TXPROTO_TCP;
          FProto:=IPPROTO_TCP;
          FProtoType:=SOCK_STREAM;
          end;
    spUdp:begin
          FProtoStr:=TXPROTO_UDP;
          FProto:=IPPROTO_UDP;
          FProtoType:=SOCK_DGRAM;
          end;
    end;
  end;

function TRtcWinSocket.GetRcvdCount : LongInt;
  var
    Temp : integer;
    Res : integer;
  begin
  Res:=_ioctlsocket(FHSocket, FIONREAD, Temp);
  if Res = SOCKET_ERROR then
    begin
    FLastError:=_WSAGetLastError;
    if (FLastError=WSAEWOULDBLOCK) or (FLastError=0) then
      Result:=0
    else
      begin
      Result:=-1;
      if LOG_SOCKET_ERRORS then
        SocketError('GetRcvdCount');
      end;
    end
  else
    Result := LongInt(Temp);
  end;

function TRtcWinSocket.RealRecv(var Data; Len:Integer; triggerEvent:boolean=True) : Integer;
  begin
  if Protocol=spTcp then
    Result := _recv(FHSocket, Data, Len, 0)
  else
    begin
    FSrcLen := SizeOf(FSrc);
    Result := _recvfrom(FHSocket, Data, Len, 0, FSrc, FSrcLen);
    end;

  if triggerEvent and (Result>0) then
    On_DataIn(Result);
  end;

function TRtcWinSocket.ReceiveEx(var Str : RtcByteArray):integer;
  var
    Res:integer;
  begin
  if bCanRead then
    begin
    bCanRead:=False;
    Res := GetRcvdCount;
    if Res>0 then
      begin
      SetLength(Str, Res);
      Result := RealRecv(Str[0], Res);
      if Result<0 then
        begin
        SetLength(Str,0);
        RealSocketError('ReceiveEx: Receive() returned '+IntToStr(Result)+' instead of '+IntToStr(Res)+' bytes');
        end
      else if Result<Res then
        SetLength(Str,Result);
      end
    else
      begin
      Result:=Res;
      SetLength(Str,0);
      if Result<0 then
        RealSocketError('ReceiveEx: GetRcvdCount<0');
      end;
    end
  else if length(FBuffered)>0 then
    begin
    Str:=FBuffered;
    Result:=length(FBuffered);
    SetLength(FBuffered,0);
    end
  else
    begin
    Result:=0;
    SetLength(Str,0);
    end;
  end;

function TRtcWinSocket.ReceiveStr(var Str : RtcString):integer;
  var
    Res:integer;
  {$IFNDEF RTC_BYTESTRING}
    Data:RtcByteArray;
  {$ENDIF}
  begin
  if bCanRead then
    begin
    bCanRead:=False;
    Res := GetRcvdCount;
    if Res>0 then
      begin
      {$IFDEF RTC_BYTESTRING}
      SetLength(Str, Res);
      Result := RealRecv(Str[1], Res);
      {$ELSE}
      SetLength(Data, Res);
      Result := RealRecv(Data[0], Res);
      {$ENDIF}
      if Result<0 then
        begin
        SetLength(Str,0);
        RealSocketError('ReceiveStr: Receive() returned '+IntToStr(Result)+' instead of '+IntToStr(Res)+' bytes');
        end
      {$IFDEF RTC_BYTESTRING}
      else if Result<Res then
        SetLength(Str,Result);
      {$ELSE}
      else
        Str:=RtcBytesToString(Data,0,Result);
      {$ENDIF}
      end
    else
      begin
      Result:=Res;
      SetLength(Str,0);
      if Result<0 then
        RealSocketError('ReceiveStr: GetRcvdCount<0');
      end;
    end
  else if length(FBuffered)>0 then
    begin
    Result:=length(FBuffered);
    {$IFDEF RTC_BYTESTRING}
    SetLength(Str,Result);
    Move(FBuffered[0],Str[1],Result);
    {$ELSE}
    Str:=RtcBytesToString(FBuffered);
    {$ENDIF}
    SetLength(FBuffered,0);
    end
  else
    begin
    Result:=0;
    SetLength(Str,0);
    end;
  end;

function TRtcWinSocket.RealSend(const Data; Len : Integer; triggerEvent:boolean=True) : Integer;
  begin
  if Protocol=spTcp then
    Result := _Send(FHSocket, Data, Len, 0)
  else
    begin
    if (FSrcLen>0) then // Reply to the last sender
      Result := _SendTo(FHSocket, Data, Len, 0, TSockAddr(FSrc), FSrcLen)
    else
      Result := _SendTo(FHSocket, Data, Len, 0, TSockAddr(sin), SizeOf(sin));
    end;
  if triggerEvent and (Result>0) then
    On_DataOut(Result);
  end;

procedure TRtcWinSocket.TryToSend;
  var
    Len       : Integer;
    Count     : Integer;
  begin
  FSending := True;
  try
    repeat
      if FSendNowAt>=length(FSendNowBuff) then
        begin
        FSendNowBuff:=FSendBuffer.GetEx;
        FSendBuffer.Clear;
        FSendNowAt:=0;
        end;
      if length(FSendNowBuff)=0 then
        begin
        bAllSent := TRUE;
        Break;
        end
      else
        begin
        Len:=length(FSendNowBuff)-FSendNowAt;
        if Len>FMaxSendSize then
          Len:=FMaxSendSize;

        FLastError:=0;
        Count := RealSend(FSendNowBuff[FSendNowAt], Len);
        if Count>0 then
          begin
          FPostNotify:=TRUE;
          Inc(FSendNowAt,Count);
          end;
        if Count<Len then
          begin
          FPostNotify:=False; // WinSock will notify us
          bCanSend:=False;
          FLastError:=_WSAGetLastError;
          if FLastError<>WSAEWOULDBLOCK then
            RealSocketError('TryToSend '+IntToStr(Len)+', sent '+IntToStr(Count));
          Break;
          end;
        end;
      until bAllSent;
  finally
    if FPostNotify and (FLevel=0) then
      begin
      FPostNotify:=False;
      PostMsg(FDX_WRITE);
      end;
    FSending:=False;
    end;
  end;

{ Return -1 if error, else return number of bytes written }
function TRtcWinSocket.SendEx(const Str:RtcByteArray):integer;
  begin
  if Protocol=spTcp then
    begin
    Result:=length(Str);
    if Result>0 then
      begin
      if bAllSent then
        begin
        FSendNowBuff:=Str;
        FSendNowAt:=0;
        end
      else
        FSendBuffer.AddEx(Str);
      bAllSent:=False;
      end;
    if bCanSend and not (bAllSent or FSending) then
      TryToSend;
    end
  else
    begin
    Result:=RealSend(Str[0],length(Str));
    if Result<>length(Str) then
      RealSocketError('SendStr');
    end;
  end;

{ Return -1 if error, else return number of bytes written }
function TRtcWinSocket.SendStr(const Str:RtcString):integer;
  begin
  if Protocol=spTcp then
    begin
    Result:=length(Str);
    if Result>0 then
      begin
      if bAllSent then
        begin
        {$IFDEF RTC_BYTESTRING}
        SetLength(FSendNowBuff,length(Str));
        Move(Str[1],FSendNowBuff[0],length(Str));
        {$ELSE}
        FSendNowBuff:=RtcStringToBytes(Str);
        {$ENDIF}
        FSendNowAt:=0;
        end
      else
        FSendBuffer.Add(Str);
      bAllSent:=False;
      end;
    if bCanSend and not (bAllSent or FSending) then
      TryToSend;
    end
  else
    begin
    {$IFDEF RTC_BYTESTRING}
    Result:=RealSend(Str[1],length(Str));
    {$ELSE}
    Result:=RealSend(RtcStringToBytes(Str)[0],length(Str));
    {$ENDIF}
    if Result<>length(Str) then
      RealSocketError('SendStr');
    end;
  end;

function TRtcWinSocket.BuffEx(const Str:RtcByteArray):integer;
  begin
  if Protocol=spTcp then
    begin
    Result:=length(Str);
    if Result>0 then
      begin
      FSendBuffer.AddEx(Str);
      bAllSent:=False;
      end;
    end
  else
    begin
    Result:=RealSend(Str[0],length(Str));
    if Result<>length(Str) then
      RealSocketError('BuffStr');
    end;
  end;

function TRtcWinSocket.BuffStr(const Str:RtcString):integer;
  begin
  if Protocol=spTcp then
    begin
    Result:=length(Str);
    if Result>0 then
      begin
      FSendBuffer.Add(Str);
      bAllSent:=False;
      end;
    end
  else
    begin
    {$IFDEF RTC_BYTESTRING}
    Result:=RealSend(Str[1],length(Str));
    {$ELSE}
    Result:=RealSend(RtcStringToBytes(Str)[0],length(Str));
    {$ENDIF}
    if Result<>length(Str) then
      RealSocketError('BuffStr');
    end;
  end;

procedure TRtcWinSocket.DoXOpen(Err:word);
  begin
  Inc(FLevel);
  try
    Open;
  finally
    Dec(FLevel);
    if FLevel=0 then
      if FWantRelease then
        Release
      else if FPostNotify then
        begin
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
    end;
  end;

procedure TRtcWinSocket.DoXClose(Err:word);
  begin
  Inc(FLevel);
  try
    Abort;
  finally
    Dec(FLevel);
    if FWantRelease and (FLevel=0) then
      Release;
    end;
  end;

procedure TRtcWinSocket.DoXRead(Err:word);
  begin
  if bCanRead then DoRead(0);
  end;

procedure TRtcWinSocket.DoXWrite(Err:word);
  begin
  if bCanSend then DoWrite(0);
  end;

procedure TRtcWinSocket.DoConnect(Err:word);
  begin
  Inc(FLevel);
  try
    if Err<>0 then
      begin
      FLastError:=Err;
      if LOG_COMMON_SOCKET_ERRORS then
        SocketError('FD_CONNECT')
      else if (Err<>WSAECONNREFUSED) and
              (Err<>WSAETIMEDOUT) then
        SocketError('FD_CONNECT');
      Abort;
      end;
  finally
    Dec(FLevel);
    if FLevel=0 then
      if FWantRelease then
        Release
      else if FPostNotify then
        begin
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
    end;
  end;

procedure TRtcWinSocket.DoRead(Err:word);
  begin
  Inc(FLevel);
  try
    if Err=0 then
      begin
      bCanRead:=True;
      if FState = wsConnected then
        begin
        On_DataReceived(Err);
        if Protocol=spUdp then
          FSrcLen:=0; // Remove association with the last sender
        if FPostNotify then
          begin
          FPostNotify:=False;
          PostMsg(FDX_WRITE);
          end;
        end
      else if FState = wsConnecting then
        begin
        ChangeState(wsConnected);
        PostMsg(FDX_READ);
        end;
      end
    else
      begin
      FLastError:=Err;
      SocketError('FD_READ');
      Abort;
      end;
  finally
    Dec(FLevel);
    if FWantRelease and (FLevel=0) then
      Release;
    end;
  end;

procedure TRtcWinSocket.DoWrite(Err:word);
  begin
  Inc(FLevel);
  try
    if Err=0 then
      begin
      bCanSend:=True;
      if FState=wsConnected then
        begin
        if bAllSent then
          On_DataSent(0)
        else
          TryToSend;
        if FPostNotify then
          begin
          FPostNotify:=False;
          PostMsg(FDX_WRITE);
          end
        else if bCanRead then
          PostMsg(FDX_READ);
        end
      else if FState = wsConnecting then
        begin
        ChangeState(wsConnected);
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
      end
    else
      begin
      FLastError:=Err;
      SocketError('FD_WRITE');
      Abort;
      end;
  finally
    Dec(FLevel);
    if FLevel=0 then
      if FWantRelease then
        Release
      else if FPostNotify then
        begin
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
    end;
  end;

procedure TRtcWinSocket.DoClose(Err:word);
  begin
  Inc(FLevel);
  try
    if Err=0 then
      Close
    else
      begin
      FLastError:=Err;
      if LOG_COMMON_SOCKET_ERRORS then
        SocketError('FD_CLOSE')
      else if Err<>WSAECONNABORTED then
        SocketError('FD_CLOSE');
      Abort;
      end;
  finally
    Dec(FLevel);
    if FLevel=0 then
      if FWantRelease then
        Release
      else if FPostNotify then
        begin
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
    end;
  end;

procedure TRtcWinSocket.DoAccept(Err:word);
  begin
  Inc(FLevel);
  try
    if Err=0 then
      On_NewSocket(Err)
    else
      begin
      FLastError:=Err;
      SocketError('FD_ACCEPT');
      end;
  finally
    Dec(FLevel);
    if FLevel=0 then
      if FWantRelease then
        Release
      else if FPostNotify then
        begin
        FPostNotify:=False;
        PostMsg(FDX_WRITE);
        end;
    end;
  end;

procedure TRtcWinSocket.DoMessage(Msg: word; Data: cardinal);
  begin
  case Msg of
    MSG_ACCEPT:  DoAccept(Data);
    MSG_CONNECT: DoConnect(Data);
    MSG_READ:    DoRead(Data);
    MSG_WRITE:   DoWrite(Data);
    MSG_CLOSE:   DoClose(Data);

    MSG_XOPEN:   DoXOpen(Data);
    MSG_XREAD:   DoXRead(Data);
    MSG_XWRITE:  DoXWrite(Data);
    MSG_XCLOSE:  DoXClose(Data);
    else
      RaiseException('TRtcWinSocket.DoMessage: Unknown message code #'+IntToStr(Msg),False);
    end;
  end;

procedure TRtcWinSocket.WMASyncSelect(lParamLo,lParamHi:word);
  begin
  case lParamLo of
    FD_CONNECT: Call_Message(MSG_CONNECT,LParamHi);
    FD_ACCEPT:  Call_Message(MSG_ACCEPT,LParamHi);
    FD_WRITE:   Call_Message(MSG_WRITE,LParamHi);
    FD_READ:    Call_Message(MSG_READ,LParamHi);
    FD_CLOSE:   Call_Message(MSG_CLOSE,LParamHi);

    FDX_OPEN:   Call_Message(MSG_XOPEN,LParamHi);
    FDX_CLOSE:  Call_Message(MSG_XCLOSE,LParamHi);
    FDX_READ:   Call_Message(MSG_XREAD,LParamHi);
    FDX_WRITE:  Call_Message(MSG_XWRITE,LParamHi);
    end;
  end;

procedure TRtcWinSocket.NeedMoreData;
  begin
  // we will be notified by WinSock when data is ready for reading
  end;

procedure TRtcWinSocket.PostMsg(flags: Integer);
  begin
  if not FWantRelease and (FHSocket<>INVALID_SOCKET) then
    PostMessage(Handle,FMessageCode,FHSocket,flags);
  end;

function TRtcWinSocket.UpdateASyncSelect:boolean;
  var
    iStatus : integer;
  begin
  if FSelectEvent>0 then
    iStatus := _WSAASyncSelect(FHSocket, Handle, FMessageCode, FSelectEvent)
  else
    iStatus := _WSAASyncSelect(FHSocket, Handle, 0, 0);
  Result:= (iStatus = 0);
  end;

function TRtcWinSocket.EnableMsg(flags: Integer):boolean;
  begin
  flags:=FSelectEvent or flags;
  if FSelectEvent<>flags then
    begin
    FSelectEvent:=flags;
    Result:=UpdateAsyncSelect;
    end
  else
    Result:=True;
  end;

function TRtcWinSocket.DisableMsg(flags: Integer):boolean;
  begin
  flags:=FSelectEvent and not flags;
  if FSelectEvent<>flags then
    begin
    FSelectEvent:=flags;
    Result:=UpdateAsyncSelect;
    end
  else
    Result:=True;
  end;

function TRtcWinSocket.DisableAllMsgs:boolean;
  begin
  Result:=DisableMsg(FSelectEvent);
  end;

function TRtcWinSocket.StartingConnect: boolean;
  begin
  FSelectEvent:=0;
  Result:=EnableMsg(FD_CLOSE or FD_CONNECT or FD_WRITE or FD_READ);
  end;

function TRtcWinSocket.StartingNewSocket: boolean;
  begin
  FSelectEvent:=0;
  Result:=EnableMsg(FD_CLOSE or FD_WRITE or FD_READ);
  end;

function TRtcWinSocket.StartingListen: boolean;
  begin
  FSelectEvent:=0;
  if Protocol=spTcp then
    Result:=EnableMsg(FD_ACCEPT)
  else
    Result:=EnableMsg(FD_ACCEPT or FD_WRITE or FD_CLOSE or FD_CONNECT or FD_READ);
  end;

{ This function is a callback function. It means that it is called by       }
{ windows. This is the very low level message handler procedure setup to    }
{ handle the message sent by windows (winsock) to handle messages.          }
{ This message handler works with X connections by using 1 Windows handle   }
function RtcSocketWindowProc(ahWnd   : HWND;
                             auMsg   : LongWord;
                             awParam : WPARAM;
                             alParam : LPARAM): Integer;
  var
    Obj, Obj2 : TObject;
    Sock   : TSocket;
  begin
  Result:=-1;
  Sock:=awParam;
  if (Sock<>0) and (Sock<>INVALID_SOCKET) then
    begin
    Result:=0;

    if rtcEnterSocket then
      begin
      try
        Obj:=rtcGetSocket(Sock);
        if assigned(Obj) then
          begin
          if (Sock<>TRtcWinSocket(Obj).FHSocket) or
             (ahWnd<>TRtcWinSocket(Obj).Handle) or
             (auMsg<>TRtcWinSocket(Obj).MessageCode) then
            Obj:=nil
          else if TRtcWinSocket(Obj).MultiThreaded then
            begin
            Obj2:=Obj;
            Obj:=nil;
            try
              TRtcWinSocket(Obj2).WMASyncSelect(alParam and $FFFF, (alParam and $FFFF0000) shr 16)
            except
              on E:Exception do
                begin
                if LOG_AV_ERRORS then
                  Log('WM_ASYNCSELECT(msg='+IntToStr(auMsg)+', wparam='+IntToStr(awParam)+', lparam='+IntToStr(alParam)+')',E,'ERROR');
                try
                  TRtcWinSocket(Obj2).HandleBackGroundException(E);
                except
                  on E:Exception do
                    if LOG_AV_ERRORS then
                      Log('HandleBgException',E,'ERROR');
                  end;
                end;
              end;
            end;
          end;
      finally
        rtcLeaveSocket;
        end;
      if assigned(Obj) then
        begin
        try
          TRtcWinSocket(Obj).WMASyncSelect(alParam and $FFFF, (alParam and $FFFF0000) shr 16);
        except
          on E:Exception do
            begin
            if LOG_AV_ERRORS then
              Log('WM_ASYNCSELECT(msg='+IntToStr(auMsg)+', wparam='+IntToStr(awParam)+', lparam='+IntToStr(alParam)+')',E,'ERROR');
            try
              TRtcWinSocket(Obj).HandleBackGroundException(E);
            except
              on E:Exception do
                if LOG_AV_ERRORS then
                  Log('HandleBgException',E,'ERROR');
              end;
            end;
          end;
        end;
      end; // rtcEnterSocket

    end;
  end;

initialization
{$IFDEF RTC_DEBUG} Log('rtcWinSocket Initializing ...','DEBUG');{$ENDIF}

CS:=TRtcCritSec.Create;
MyMsgCode:=WM_ASYNCSELECT_FIRST;

rtcRegisterHWNDProc(@RtcSocketWindowProc, WM_SOCKET_FIRST, WM_SOCKET_LAST);

{$IFDEF RTC_DEBUG} Log('rtcWinSocket Initialized.','DEBUG');{$ENDIF}
finalization
{$IFDEF RTC_DEBUG} Log('rtcWinSocket Finalizing ...','DEBUG');{$ENDIF}

RtcFreeAndNil(CS);

{$IFDEF RTC_DEBUG} Log('rtcWinSocket Finalized.','DEBUG');{$ENDIF}
end.

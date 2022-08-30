CONST
  INVALID_SOCKET		= TSocket(NOT(0));
  SOCKET_ERROR			= -1;

  INADDR_LOOPBACK  = $7F000001;
  INADDR_BROADCAST = $FFFFFFFF;
  ADDR_ANY		 = INADDR_ANY;

  cLocalHostStr = 'localhost';
  cLocalhost = '127.0.0.1';
  cAnyHost = '0.0.0.0';
  cBroadcast = '255.255.255.255';
  c6Localhost = '::1';
  c6AnyHost = '::0';
  c6Broadcast = 'ffff::1';
  cAnyPort = '0';

CONST
{$IFDEF Darwin}
  MSG_NOSIGNAL  = 0;
{$ENDIF}

  WSAEPROCLIM = -101;
  WSASYSNOTREADY = -102;
  WSAVERNOTSUPPORTED = -103;
  WSANOTINITIALISED = -104;
  WSAEDISCON = -105;
  WSAHOST_NOT_FOUND = -106;
  WSATRY_AGAIN = -107;
  WSANO_RECOVERY = -108;
  WSANO_DATA = -109;

TYPE
  PSockAddrIn = ^TSockAddrIn;
  PSockAddrIn6 = ^TSockAddrIn6;

  TAddrFamily = integer;
  PFDSet = ^TFDSet;
  TSockAddrIn = TInetSockAddr;
  TIP_mreq =  record
    imr_multiaddr: TInAddr;     // IP multicast address of group
    imr_interface: TInAddr;     // local IP address of interface
  end;
  TSockAddrIn6 = TInetSockAddr6;
  TIPv6_mreq = record
    ipv6mr_multiaddr: TIn6Addr; // IPv6 multicast address.
    ipv6mr_interface: integer;   // Interface index.
  end;

function IN6_IS_ADDR_UNSPECIFIED(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and (a^.u6_addr32[3] = 0));
end;


function IN6_IS_ADDR_LOOPBACK(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr32[0] = 0) and (a^.u6_addr32[1] = 0) and
             (a^.u6_addr32[2] = 0) and
             (a^.u6_addr8[12] = 0) and (a^.u6_addr8[13] = 0) and
             (a^.u6_addr8[14] = 0) and (a^.u6_addr8[15] = 1));
end;

function IN6_IS_ADDR_LINKLOCAL(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $80));
end;

function IN6_IS_ADDR_SITELOCAL(const a: PIn6Addr): boolean;
begin
  Result := ((a^.u6_addr8[0] = $FE) and (a^.u6_addr8[1] = $C0));
end;

function IN6_IS_ADDR_MULTICAST(const a: PIn6Addr): boolean;
begin
  Result := (a^.u6_addr8[0] = $FF);
end;

function IN6_ADDR_EQUAL(const a: PIn6Addr; const b: PIn6Addr): boolean;
begin
  Result := (CompareMem( a, b, sizeof(TIn6Addr)));
end;

procedure SET_IN6_IF_ADDR_ANY (const a: PIn6Addr);
begin
  FillChar(a^, sizeof(TIn6Addr), 0);
end;

procedure SET_LOOPBACK_ADDR6 (const a: PIn6Addr);
begin
  FillChar(a^, sizeof(TIn6Addr), 0);
  a^.u6_addr8[15] := 1;
end;

function SizeOfVarSin(sin: TSockAddr): integer;
  begin
  case sin.sin_family of
    AF_INET: Result := SizeOf(TSockAddrIn);
    AF_INET6: Result := SizeOf(TSockAddrIn6);
    else
      Result := 0;
    end;
  end;

procedure ResolveNameToIP(Name: AnsiString; Family, SockProtocol, SockType: integer; const IPList: TStrings);
var
  x, n: integer;
  a4: array [1..255] of in_addr;
  a6: array [1..255] of Tin6_addr;
begin
  IPList.Clear;
  if (family = AF_INET) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(cLocalHost)
    else
    begin
      a4[1] := StrTonetAddr(name);
      if a4[1].s_addr = INADDR_ANY then
        x := Resolvename(name, a4)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr(a4[n]));
    end;
  end;

  if (family = AF_INET6) or (family = AF_UNSPEC) then
  begin
    if lowercase(name) = cLocalHostStr then
      IpList.Add(c6LocalHost)
    else
    begin
      a6[1] := StrTonetAddr6(name);
      if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
        x := Resolvename6(name, a6)
      else
        x := 1;
      for n := 1  to x do
        IpList.Add(netaddrToStr6(a6[n]));
    end;
  end;

  if IPList.Count = 0 then
    IPList.Add(cLocalHost);
end;

function ResolvePort(Port: AnsiString; Family, SockProtocol, SockType: integer): Word;
var
  ProtoEnt: TProtocolEntry;
  ServEnt: TServiceEntry;
begin
  Result := htons(StrToIntDef(Port, 0));
  if Result = 0 then
  begin
    ProtoEnt.Name := '';
    GetProtocolByNumber(SockProtocol, ProtoEnt);
    ServEnt.port := 0;
    GetServiceByName(Port, ProtoEnt.Name, ServEnt);
    Result := ServEnt.port;
  end;
end;

function ResolveIPToName(IP: AnsiString; Family, SockProtocol, SockType: integer): AnsiString;
var
  n: integer;
  a4: array [1..1] of in_addr;
  a6: array [1..1] of Tin6_addr;
  a: array [1..1] of AnsiString;
begin
  Result := IP;
  a4[1] := StrToNetAddr(IP);
  if a4[1].s_addr <> INADDR_ANY then
  begin
//why ResolveAddress need address in HOST order? :-O
    n := ResolveAddress(nettohost(a4[1]), a);
    if n > 0 then
      Result := a[1];
  end
  else
  begin
    a6[1] := StrToNetAddr6(IP);
    n := ResolveAddress6(a6[1], a);
    if n > 0 then
      Result := a[1];
  end;
end;

function WSA_GetSockName(s: TSocket; var name: TSockAddr): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetSockName(s, @name, @Len);
  end;

function WSA_GetPeerName(s: TSocket; var name: TSockAddr): Integer;
  var
    len: integer;
  begin
  len := SizeOf(name);
  FillChar(name, len, 0);
  Result := fpGetPeerName(s, @name, @Len);
  end;

function WSA_Accept(s: TSocket; var addr: TSockAddr): TSocket;
  var
    x: integer;
  begin
  x := SizeOf(addr);
  Result := fpAccept(s, @addr, @x);
  end;

function WSA_SetVarSin(var Sin: TSockAddr; IP, Port: AnsiString; Family, SockProtocol, SockType: integer; PreferIP4: Boolean): integer;
  var
    TwoPass: boolean;
    f1, f2: integer;

  function GetAddr(f:integer): integer;
    var
      a4: array [1..1] of in_addr;
      a6: array [1..1] of Tin6_addr;
    begin
    Result := ESysEPROTONOSUPPORT;
    case f of
      AF_INET:
          begin
          if IP = cAnyHost then
            begin
            Sin.sin_family := AF_INET;
            Result := 0;
            end
          else
            begin
            if lowercase(IP) = cLocalHostStr then
              a4[1].s_addr := htonl(INADDR_LOOPBACK)
            else
              begin
              a4[1].s_addr := 0;
              Result := WSAHOST_NOT_FOUND;
              a4[1] := StrTonetAddr(IP);
              if a4[1].s_addr = INADDR_ANY then
                Resolvename(ip, a4);
              end;
            if a4[1].s_addr <> INADDR_ANY then
              begin
              Sin.sin_family := AF_INET;
              Sin.sin_addr := a4[1];
              Result := 0;
              end;
            end;
          end;
      AF_INET6:
          begin
          if IP = c6AnyHost then
            begin
            Sin.sin_family := AF_INET6;
            Result := 0;
            end
          else
            begin
            if lowercase(IP) = cLocalHostStr then
              SET_LOOPBACK_ADDR6(@a6[1])
            else
              begin
              Result := WSAHOST_NOT_FOUND;
              SET_IN6_IF_ADDR_ANY(@a6[1]);
              a6[1] := StrTonetAddr6(IP);
              if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
                Resolvename6(ip, a6);
              end;
            if IN6_IS_ADDR_UNSPECIFIED(@a6[1]) then
              begin
              Sin.sin_family := AF_INET6;
              PSockAddrIn6(@Sin)^.sin6_addr := a6[1];
              Result := 0;
              end;
            end;
          end;
      end;
    end;

  begin
  Result := 0;
  FillChar(Sin, Sizeof(Sin), 0);
  Sin.sin_port := Resolveport(port, family, SockProtocol, SockType);
  TwoPass := False;
  if Family = AF_UNSPEC then
    begin
    if PreferIP4 then
      begin
      f1 := AF_INET;
      f2 := AF_INET6;
      TwoPass := True;
      end
    else
      begin
      f2 := AF_INET;
      f1 := AF_INET6;
      TwoPass := True;
      end;
    end
  else
    f1 := Family;
  Result := GetAddr(f1);
  if (Result <> 0) and TwoPass then
    begin
    Result := GetAddr(f2);
    if Result <> 0 then
      Result := GetAddr(f1);
    end;
  end;

function WSA_GetSinIP(Sin: TSockAddr): AnsiString;
  begin
  Result := '';
  case sin.sin_family of
    AF_INET:  Result := NetAddrToStr(sin.sin_addr);
    AF_INET6: Result := NetAddrToStr6(PSockAddrIn6(@sin)^.sin6_addr);
    end;
  end;

function WSA_GetSinPort(Sin: TSockAddr): Integer;
  begin
  if (Sin.sin_family = AF_INET6) then
    Result := Sockets.ntohs(PSockAddrIn6(@Sin)^.sin6_port)
  else
    Result := Sockets.ntohs(Sin.sin_port);
  end;

(* function WSA_InitSocketInterface(stack: String): Boolean;
  begin
  Libc.Signal(Libc.SIGPIPE, TSignalHandler(Libc.SIG_IGN));
  end; *)

function WSA_ErrorDesc(ErrCode:integer):String;
  begin
  case ErrCode of
    0:Result := '';
    ESysEINTR: {10004}
      Result := 'Interrupted system call';
    ESysEBADF: {10009}
      Result := 'Bad file number';
    ESysEACCES: {10013}
      Result := 'Permission denied';
    ESysEFAULT: {10014}
      Result := 'Bad address';
    ESysEINVAL: {10022}
      Result := 'Invalid argument';
    ESysEMFILE: {10024}
      Result := 'Too many open files';
    ESysEWOULDBLOCK: {10035}
      Result := 'Operation would block';
    ESysEINPROGRESS: {10036}
      Result := 'Operation now in progress';
    ESysEALREADY: {10037}
      Result := 'Operation already in progress';
    ESysENOTSOCK: {10038}
      Result := 'Socket operation on nonsocket';
    ESysEDESTADDRREQ: {10039}
      Result := 'Destination address required';
    ESysEMSGSIZE: {10040}
      Result := 'Message too long';
    ESysEPROTOTYPE: {10041}
      Result := 'Protocol wrong type for Socket';
    ESysENOPROTOOPT: {10042}
      Result := 'Protocol not available';
    ESysEPROTONOSUPPORT: {10043}
      Result := 'Protocol not supported';
    ESysESOCKTNOSUPPORT: {10044}
      Result := 'Socket not supported';
    ESysEOPNOTSUPP: {10045}
      Result := 'Operation not supported on Socket';
    ESysEPFNOSUPPORT: {10046}
      Result := 'Protocol family not supported';
    ESysEAFNOSUPPORT: {10047}
      Result := 'Address family not supported';
    ESysEADDRINUSE: {10048}
      Result := 'Address already in use';
    ESysEADDRNOTAVAIL: {10049}
      Result := 'Can''t assign requested address';
    ESysENETDOWN: {10050}
      Result := 'Network is down';
    ESysENETUNREACH: {10051}
      Result := 'Network is unreachable';
    ESysENETRESET: {10052}
      Result := 'Network dropped connection on reset';
    ESysECONNABORTED: {10053}
      Result := 'Software caused connection abort';
    ESysECONNRESET: {10054}
      Result := 'Connection reset by peer';
    ESysENOBUFS: {10055}
      Result := 'No Buffer space available';
    ESysEISCONN: {10056}
      Result := 'Socket is already connected';
    ESysENOTCONN: {10057}
      Result := 'Socket is not connected';
    ESysESHUTDOWN: {10058}
      Result := 'Can''t send after Socket shutdown';
    ESysETOOMANYREFS: {10059}
      Result := 'Too many references:can''t splice';
    ESysETIMEDOUT: {10060}
      Result := 'Connection timed out';
    ESysECONNREFUSED: {10061}
      Result := 'Connection refused';
    ESysELOOP: {10062}
      Result := 'Too many levels of symbolic links';
    ESysENAMETOOLONG: {10063}
      Result := 'File name is too long';
    ESysEHOSTDOWN: {10064}
      Result := 'Host is down';
    ESysEHOSTUNREACH: {10065}
      Result := 'No route to host';
    ESysENOTEMPTY: {10066}
      Result := 'Directory is not empty';
    ESysEUSERS: {10068}
      Result := 'Too many users';
    ESysEDQUOT: {10069}
      Result := 'Disk quota exceeded';
    ESysESTALE: {10070}
      Result := 'Stale NFS file handle';
    ESysEREMOTE: {10071}
      Result := 'Too many levels of remote in path';
    WSAEPROCLIM: {10067}
      Result := 'Too many processes';
    WSASYSNOTREADY: {10091}
      Result := 'Network subsystem is unusable';
    WSAVERNOTSUPPORTED: {10092}
      Result := 'Winsock DLL cannot support this application';
    WSANOTINITIALISED: {10093}
      Result := 'Winsock not initialized';
    WSAEDISCON: {10101}
      Result := 'Disconnect';
    WSAHOST_NOT_FOUND: {11001}
      Result := 'Host not found';
    WSATRY_AGAIN: {11002}
      Result := 'Non authoritative - host not found';
    WSANO_RECOVERY: {11003}
      Result := 'Non recoverable error';
    WSANO_DATA: {11004}
      Result := 'Valid name, no data record of requested type'
  else
    Result := 'Unknown Socket error #' + IntToStr(ErrCode);
    end;
  end;

{=============================================================================}

function TRtcSocket.Sock_CheckError: boolean;
  begin
  Result := (FErrCode <> 0) and
            (FErrCode <> ESysEINPROGRESS) and
            (FErrCode <> ESysEWOULDBLOCK);
  end;

procedure TRtcSocket.Sock_CheckLastErrorDesc;
  begin
  FErr := WSA_ErrorDesc(FErrCode);
  end;

procedure TRtcSocket.Sock_CheckLastError;
  begin
  FErrCode:=fpgeterrno;
  Sock_CheckLastErrorDesc;
  end;

function TRtcSocket.Sock_Err(res: Integer):boolean;
  begin
  if (res=SOCKET_ERROR) or (res<0) then
    begin
    Result:=True;
    Sock_CheckLastError;
    end
  else
    Result:=False;
  end;

procedure TRtcSocket.Sock_SetSin(var Sin: TSockAddr; const vAddr,vPort:AnsiString);
  begin
  FErrCode := WSA_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, true);
  Sock_CheckLastErrorDesc;
  end;

procedure TRtcSocket.Sock_CreateSocket(Sin: TSockAddr);
  begin
  FSocket := fpSocket(integer(Sin.sin_family), SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    fpFD_ZERO(FFDSet);
    fpFD_SET(FSocket, FFDSet);
    end;
  end;

procedure TRtcSocket.Sock_SetLinger(vEnable: Boolean; vLinger: Integer);
  var
    li: TLinger;
    buf: pointer;
  begin
  if vEnable then
    li.l_onoff := 1
  else
    li.l_onoff := 0;
  li.l_linger := vLinger;
  buf := @li;
  Sock_Err(fpSetSockOpt(FSocket, integer(SOL_SOCKET), integer(SO_LINGER), buf, SizeOf(li)));
  end;

procedure TRtcSocket.Sock_SetDelay;
  var
    optval: integer;
  begin
  // NO DELAY
  optval := -1; { -1=true, 0=false }
  fpSetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, @optval, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, @optval, SizeOf(optval));
  // REUSE ADDR.
  optval  := -1;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @optval, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @optval, SizeOf(optval));
  end;

procedure TRtcSocket.Sock_SetTimeouts(const TOA:TRtcTimeoutsOfAPI);
  var
    optval: integer;
  begin
{$IFDEF RTC_USESETTIMEOUTS}
  if assigned(TOA) then
    begin
    // Set RECV_TIMEO
    if TOA.ReceiveTimeout>0 then
      begin
      optval := TOA.ReceiveTimeout*1000;
      fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
      end;
    end;
{$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  fpSetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
{$ENDIF}
  end;

function TRtcSocket.Sock_Invalid: boolean;
  begin
  Result := FSocket=INVALID_SOCKET;
  end;

function TRtcSocket.Sock_GetLocalSinIP: AnsiString;
  begin
  Result := WSA_GetSinIP(FLocalSin);
  end;

function TRtcSocket.Sock_GetLocalSinPort: AnsiString;
  begin
  Result := Int2Str(WSA_GetSinPort(FLocalSin));
  end;

function TRtcSocket.Sock_GetRemoteSinIP: AnsiString;
  begin
  Result := WSA_GetSinIP(FRemoteSin);
  end;

function TRtcSocket.Sock_GetRemoteSinPort: AnsiString;
  begin
  Result := Int2Str(WSA_GetSinPort(FRemoteSin));
  end;

procedure TRtcSocket.Sock_Bind(const vAddr, vPort: AnsiString);
  var
    Sin: TSockAddr;
  begin
  Sock_SetSin(Sin,vAddr,vPort);
  if FErrCode<>0 then Exit;

  if FSocket = INVALID_SOCKET then
    begin
    Sock_CreateSocket(Sin);
    if FErrCode<>0 then Exit;
    end;

  if Sock_Err(fpBind(FSocket, @Sin, SizeOfVarSin(Sin))) then Exit;

  Sock_Err(WSA_GetSockName(FSocket, FLocalSin));
  end;

procedure TRtcSocket.Sock_Connect(const vAddr, vPort: AnsiString; const TOA:TRtcTimeoutsOfAPI);
  var
    Sin: TSockAddr;
  begin
  Sock_SetSin(Sin, vAddr, vPort);
  if FErrCode<>0 then Exit;

  if FSocket = INVALID_SOCKET then
    begin
    Sock_CreateSocket(Sin);
    if FErrCode<>0 then Exit;
    end;

  if Sock_Err(fpConnect(FSocket, @Sin, SizeOfVarSin(Sin))) then Exit;

  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin)) then Exit;

  Sock_SetDelay;
  Sock_SetTimeouts(TOA);
  Sock_SetLinger(False,0);
  //Log('Socket '+IntToStr(FSocket)+' OPEN!');
  end;

procedure TRtcSocket.Sock_Listen(const TOA:TRtcTimeoutsOfAPI);
  var
    blog:integer;  
  begin  
  blog:=LISTEN_BACKLOG;
  if blog>SOMAXCONN then blog:=SOMAXCONN;

  if Sock_Err(fpListen(FSocket, blog)) then Exit;

  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin)) then Exit;

  Sock_SetDelay;
  Sock_SetTimeouts(TOA);
  Sock_SetLinger(False,0);
  //Log('Socket '+IntToStr(FSocket)+' LISTEN!');
  end;

function TRtcSocket.Sock_Accept: TSocket;
  begin
  Result := WSA_Accept(FSocket, FRemoteSin);
  if Result=INVALID_SOCKET then
    begin
    Sock_CheckLastError;
    if FErrCode=0 then
      begin
      FErrCode:=-1;
      FErr:='No socket waiting';
      end;
    end;
  end;

procedure TRtcSocket.Sock_SetSocket(sid: TSocket);
  begin
  FSocket := sid;
  fpFD_ZERO(FFDSet);
  fpFD_SET(FSocket, FFDSet);
  if Sock_Err(WSA_GetSockName(FSocket, FLocalSin)) then Exit;
  if Sock_Err(WSA_GetPeerName(FSocket, FRemoteSin)) then Exit;

  Sock_SetLinger(False,0);
  //Log('Socket '+IntToStr(FSocket)+' ACCEPT!');
  end;

procedure TRtcSocket.Sock_Close;
  begin
  if FSocket <> INVALID_SOCKET then
    begin
    //Log('Socket '+IntToStr(FSocket)+' CLOSE.');
    fpShutdown(FSocket,SHUT_WR);
    fpClose(FSocket);
    end;
  FSocket := INVALID_SOCKET;
  end;

function TRtcSocket.Sock_CanRead(vTimeout: integer): boolean;
  var
    TimeVal: PTimeVal;
    TimeV: TTimeVal;
    x: Integer;
    FDSet: TFDSet;
  begin
  TimeV.tv_usec := (vTimeout mod 1000) * 1000;
  TimeV.tv_sec := vTimeout div 1000;
  TimeVal := @TimeV;
  if vTimeout = -1 then TimeVal := nil;

  FDSet := FFdSet;
  x := fpSelect(FSocket + 1, @FDSet, nil, nil, TimeVal);
  if Sock_Err(x) then
    Result:=False
  else
    Result:= x > 0;
  end;

function TRtcSocket.Sock_CanWrite(vTimeout: integer): boolean;
  var
    TimeVal: PTimeVal;
    TimeV: TTimeVal;
    x: Integer;
    FDSet: TFDSet;
  begin
  TimeV.tv_usec := (vTimeout mod 1000) * 1000;
  TimeV.tv_sec := vTimeout div 1000;
  TimeVal := @TimeV;
  if vTimeout = -1 then TimeVal := nil;

  FDSet := FFdSet;
  x := fpSelect(FSocket + 1, nil, @FDSet, nil, TimeVal);
  if Sock_Err(x) then
    Result:=False
  else
    Result:= x > 0;
  end;

function TRtcSocket.Sock_WaitingData: integer;
  var
    x: Integer;
  begin
  Result := 0;
  if not Sock_Err(fpIoctl(FSocket, FIONREAD, @x)) then
    Result := x;
  end;

function TRtcSocket.Sock_RecvBuffer(var Buffer; Len: Integer): Integer;
  begin
  Result := fpRecv(FSocket, @Buffer, Len, MSG_NOSIGNAL);
  if Result = 0 then
    begin
    FErrCode := ESysECONNRESET;
    Sock_CheckLastErrorDesc;
    end
  else if Result = SOCKET_ERROR then
    Sock_CheckLastError;
  end;

function TRtcSocket.Sock_SendBuffer(var Buffer; Len: Integer): Integer;
  begin
  if Len>SOCK_MAX_SEND_SIZE then
    Len:=SOCK_MAX_SEND_SIZE;
  Result := fpSend(FSocket, @Buffer, Len, MSG_NOSIGNAL);
  if Sock_Err(Result) then
    Result:=-1;
  end;

procedure TRtcSocket.Sock_ResetError;
  begin
  FErrCode:=0; FErr:='';
  end;


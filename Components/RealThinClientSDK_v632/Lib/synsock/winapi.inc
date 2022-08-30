function TRtcSocket.Sock_CheckError: boolean;
  begin
  Result := (FErrCode <> 0) and
            (FErrCode <> WSAEINPROGRESS) and
            (FErrCode <> WSAEWOULDBLOCK);
  end;

procedure TRtcSocket.Sock_CheckLastErrorDesc;
  begin
  FErr := WSocket_ErrorDesc(FErrCode);
  end;

procedure TRtcSocket.Sock_CheckLastError;
  begin
  FErrCode:=_WSAGetLastError;
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

procedure TRtcSocket.Sock_SetSin(var Sin: TSockAddr; const vAddr,vPort:RtcString);
  begin
  FErrCode := WSocket_SetVarSin(sin, vAddr, vPort, AF_UNSPEC, IPPROTO_TCP, SOCK_STREAM, true);
  Sock_CheckLastErrorDesc;
  end;

procedure TRtcSocket.Sock_CreateSocket(Sin: TSockAddr);
  begin
  FSocket := _Socket(Sin.sin_family, SOCK_STREAM, IPPROTO_TCP);
  if FSocket = INVALID_SOCKET then
    Sock_CheckLastError
  else
    begin
    FD_ZERO(FFDSet);
    FD_SET(FSocket, FFDSet);
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
  Sock_Err(_SetSockOpt(FSocket, SOL_SOCKET, SO_LINGER, buf, SizeOf(li)));
  end;

procedure TRtcSocket.Sock_SetDelay;
  var
    optval: integer;
  begin
  // NO DELAY
  optval := -1; { -1=true, 0=false }
  _SetSockOpt(FSocket, IPPROTO_TCP, TCP_NODELAY, @optval, SizeOf(optval));
  // KEEP-ALIVE
  optval  := -1;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_KEEPALIVE, @optval, SizeOf(optval));
  // REUSE ADDR.
  optval  := -1;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_REUSEADDR, @optval, SizeOf(optval));
  // Set READ Buffer
  optval := SOCK_READ_BUFFER_SIZE;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVBUF, @optval, SizeOf(optval));
  // Set SEND Buffer
  optval := SOCK_SEND_BUFFER_SIZE;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDBUF, @optval, SizeOf(optval));
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
      _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
      end;
    // Set SND_TIMEO
    if TOA.SendTimeout>0 then
      begin
      optval := TOA.SendTimeout*1000;
      _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
      end;
    end;
{$ELSE}
  // Set RECV_TIMEO
  optval := SOCK_RECV_TIMEOUT;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_RCVTIMEO, @optval, SizeOf(optval));
  // Set SND_TIMEO
  optval := SOCK_SEND_TIMEOUT;
  _SetSockOpt(FSocket, SOL_SOCKET, SO_SNDTIMEO, @optval, SizeOf(optval));
{$ENDIF}
  end;

function TRtcSocket.Sock_Invalid: boolean;
  begin
  Result := FSocket=INVALID_SOCKET;
  end;

function TRtcSocket.Sock_GetLocalSinIP: RtcString;
  begin
  Result := WSocket_GetSinIP(FLocalSin);
  end;

function TRtcSocket.Sock_GetLocalSinPort: RtcString;
  begin
  Result := Int2Str(WSocket_GetSinPort(FLocalSin));
  end;

function TRtcSocket.Sock_GetRemoteSinIP: RtcString;
  begin
  Result := WSocket_GetSinIP(FRemoteSin);
  end;

function TRtcSocket.Sock_GetRemoteSinPort: RtcString;
  begin
  Result := Int2Str(WSocket_GetSinPort(FRemoteSin));
  end;

procedure TRtcSocket.Sock_Bind(const vAddr, vPort: RtcString);
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

  if Sock_Err(_Bind(FSocket, Sin, SizeOfSockAddr(Sin))) then Exit;

  FLocalSinLen:=SizeOf(FLocalSin);
  Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen));
  end;

procedure TRtcSocket.Sock_Connect(const vAddr, vPort: RtcString; const TOA:TRtcTimeoutsOfAPI);
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

  if Sock_Err(_Connect(FSocket, Sin, SizeOfSockAddr(Sin))) then Exit;

  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;

  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(_GetPeerName(FSocket, FRemoteSin, FRemoteSinLen)) then Exit;

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

  if Sock_Err(_Listen(FSocket, blog)) then Exit;

  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;

  Sock_SetDelay;
  Sock_SetTimeouts(TOA);
  Sock_SetLinger(False,0);
  //Log('Socket '+IntToStr(FSocket)+' LISTEN!');
  end;

function TRtcSocket.Sock_Accept: TSocket;
  begin
  FRemoteSinLen:=SizeOf(FRemoteSin);
  Result := _Accept(FSocket, FRemoteSin, FRemoteSinLen);
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
  FD_ZERO(FFDSet);
  FD_SET(FSocket, FFDSet);

  FLocalSinLen:=SizeOf(FLocalSin);
  if Sock_Err(_GetSockName(FSocket, FLocalSin, FLocalSinLen)) then Exit;

  FRemoteSinLen:=SizeOf(FRemoteSin);
  if Sock_Err(_GetPeerName(FSocket, FRemoteSin, FRemoteSinLen)) then Exit;

  Sock_SetLinger(False,0);
  //Log('Socket '+IntToStr(FSocket)+' ACCEPT!');
  end;

procedure TRtcSocket.Sock_Close;
  begin
  if FSocket <> INVALID_SOCKET then
    begin
    //Log('Socket '+IntToStr(FSocket)+' CLOSE.');
    _Shutdown(FSocket,SD_BOTH);
    _CloseSocket(FSocket);
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
  x := _Select(FSocket + 1, @FDSet, nil, nil, TimeVal);
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
  x := _Select(FSocket + 1, nil, @FDSet, nil, TimeVal);
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
  if not Sock_Err(_IoctlSocket(FSocket, FIONREAD, x)) then
    Result := x;
  end;

function TRtcSocket.Sock_RecvBuffer(var Buffer; Len: Integer): Integer;
  begin
  Result := _Recv(FSocket, Buffer, Len, 0);
  if Result = 0 then
    begin
    Result:=-1;
    FErrCode := WSAECONNRESET;
    Sock_CheckLastErrorDesc;
    end
  else if Result < 0 then
    Sock_CheckLastError;
  end;

function TRtcSocket.Sock_SendBuffer(var Buffer; Len: Integer): Integer;
  begin
  if Len>SOCK_MAX_SEND_SIZE then
    Len:=SOCK_MAX_SEND_SIZE;
  Result := _Send(FSocket, Buffer, Len, 0);
  if Sock_Err(Result) then
    Result:=-1;
  end;

procedure TRtcSocket.Sock_ResetError;
  begin
  FErrCode:=0; FErr:='';
  end;


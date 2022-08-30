{
  RealThinClient SDK: Platform-independent Synchronous Socket API class
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}
unit rtcSynAPI;

{$INCLUDE rtcDefs.inc}

interface

uses
  SysUtils,

  rtcTypes,
  rtcConn,
  rtcInfo,
  rtcFastStrings,
  rtcSockBase,

{$IFDEF WINDOWS} //Win32 and Win64
  rtcWinSock;

{$ELSE}{$IFDEF POSIX} // Mac OSX
  Classes,
  Posix.Base, Posix.SysSocket, Posix.SysSelect,
  Posix.ArpaInet, Posix.NetinetIn, Posix.NetDB,
  Posix.Unistd, Posix.SysTime; // , PosixStrOpts;

type
  TSocket = integer;
  TSockAddr = sockaddr;
  TFDSet = fd_set;

{$ELSE}{$IFDEF DARWIN} // iOS (iPhone + iPad) on FPC
  {$DEFINE RTC_NIX_SOCK}
  rtcNixSock;

{$ELSE} // Anything else
  {$DEFINE FPSOCK}
  Classes,
  BaseUnix, Unix, termio, sockets, netdb;

{$ENDIF}{$ENDIF}{$ENDIF}

var
  LISTEN_BACKLOG:integer=200;

type
  TRtcSocket=class
  private
    FSocket: TSocket;
    NewSockID: TSocket;
    FFDSet: TFDSet;
    FErr: String;
    FErrCode: Integer;
    FLocalSin, FRemoteSin: TSockAddr;

  {$IFDEF WINDOWS}
    FLocalSinLen, FRemoteSinLen: integer;
  {$ELSE}{$IFDEF POSIX}
    FLocalSinLen, FRemoteSinLen: socklen_t;
    FTempBuffer: RtcByteArray;
  {$ELSE}{$IFDEF RTC_NIX_SOCK}
    FTempBuffer: RtcByteArray;
  {$ENDIF}{$ENDIF}{$ENDIF}

    procedure Sock_SetSin(var Sin: TSockAddr; const vAddr,vPort:RtcString);
    procedure Sock_CreateSocket(Sin: TSockAddr);
    procedure Sock_SetLinger(vEnable: Boolean; vLinger: Integer);
    procedure Sock_SetDelay;
    procedure Sock_SetTimeouts(const TOA:TRtcTimeoutsOfAPI);

    procedure Sock_Bind(const vAddr,vPort: RtcString);
    procedure Sock_Listen(const TOA:TRtcTimeoutsOfAPI);
    procedure Sock_Connect(const vAddr,vPort: RtcString; const TOA:TRtcTimeoutsOfAPI);

    function Sock_Accept:TSocket;
    procedure Sock_SetSocket(sid:TSocket);

    procedure Sock_Close;

    function Sock_Invalid:boolean;
    function Sock_CheckError:boolean;
    function Sock_Err(res:Integer):boolean;

    procedure Sock_ResetError;
    procedure Sock_CheckLastError;
    procedure Sock_CheckLastErrorDesc;

    function Sock_GetLocalSinIP:RtcString;
    function Sock_GetLocalSinPort:RtcString;
    function Sock_GetRemoteSinIP:RtcString;
    function Sock_GetRemoteSinPort:RtcString;

    function Sock_WaitingData:integer;
    function Sock_RecvBuffer(var Buffer; Len: Integer): Integer;
    function Sock_SendBuffer(var Buffer; Len: Integer): Integer;

    function Sock_CanRead(vTimeout:integer):boolean;
    function Sock_CanWrite(vTimeout:integer):boolean;

  public
    { Constructor }
    constructor Create;

    { Destructor }
    destructor Destroy; override;

    { Start using this socket as Server listener,
      listening on port "FPort", bound to local network addapter "FAddr".
      Leave "FAddr" empty to listen on all network addapters.
      TOA can be passed as parameter to set Timeouts on API.
      Send NIL as TOA parameter to use default timeout values.
      Returns TRUE if success, FALSE if error. }
    function Listen(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI):boolean;

    { Connect to address "FAddr" on port "FPort".
      TOA can be passed as parameter to set Timeouts on API.
      Send NIL as TOA parameter to use default timeout values.
      Returns TRUE if success, FALSE if error. }
    function Connect(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI): boolean;

    { Close socket connection.
      If it was a Listening socket, listener is closed but NOT connected clients.
      If it was a Client socket, it will be disconnected from Server.
      Returns TRUE if success, FALSE if error. }
    function Close: boolean;

    { Check if there are new client sockets waiting to be accepted.
      Returns the number of waiting sockets if there are sockets waiting,
      0 if no sockets were waiting after "vTimeout" (ms), or -1 if error. }
    function WaitingSockets(vTimeout:integer): integer;

    { After WaitingSockets has returned a positive result,
      use GetNewSocket to accept one socket and receive
      a new TRtcSocket component for the new socket.
      Returns a new TRtcSocket object if OK, or NIL if error. }
    function GetNewSocket: TRtcSocket;

    { Has to be called on a socket received from GetNewSocket
      before the new TRtcSocket component can be used.
      Returns TRUE is the socket can be used, FALSE if error. }
    function StartNewSocket: boolean;

    { Try to receive data from the other side.
      If data is available, will read as much as can be read without blocking.
      If no data is available, will wait up to "vTimeout" ms for new data.
      If no data after "vTimeout", returns TRUE and an empty string in "Str".
      If data was read, "Str" will contain the data received, result will be TRUE.
      If there was an error, "Str" will be empty and FALSE will be returned as Result. }
    function ReceiveEx(var Str: RtcByteArray; vTimeout:integer): boolean;

    { Try to send as much data from "Str" starting at character location "at"
      as possible without blocking. If can not read (buffer full), will wait
      up to "vTimeout" ms to be able to send at least something. If something
      was sent, will return the number of characters (bytes) sent (to buffer).
      If can not send yet but connection seems OK, will return 0.
      If connection is not working anymore, will return -1. }
    function SendEx(var Str: RtcByteArray; at: integer; vTimeout:integer): Integer;

    { If any of the methods of this class returns FALSE or -1,
      signaling that there was an error, GetLastErrorText will
      return a more-or-less descriptive error message (text). }
    function GetLastErrorText: String;

    { Local Address to which this socket is connected. }
    function GetLocalAddr: RtcString;

    { Local Port to which this socket is connected. }
    function GetLocalPort: RtcString;

    { Peer (remote) Address to which this socket is connected.
      Does NOT apply to Listening sockets (server).  }
    function GetPeerAddr: RtcString;

    { Peer (remote) Port to which this socket is connected.
      Does NOT apply to Listening sockets (server).  }
    function GetPeerPort: RtcString;
    end;

implementation

        {$IFDEF WINDOWS}        {$I synsock\winapi.inc}
{$ELSE} {$IFDEF FPSOCK}         {$I synsock\fpcapi.inc}
{$ELSE} {$IFDEF POSIX}          {$I synsock\posapi.inc}
{$ELSE} {$IFDEF RTC_NIX_SOCK}   {$I synsock\nixapi.inc}
{$ENDIF}{$ENDIF}{$ENDIF}{$ENDIF}

constructor TRtcSocket.Create;
  begin
  inherited;
{$IFDEF WINDOWS}
  LoadWinSock;
{$ELSE}{$IFDEF POSIX}
  SetLength(FTempBuffer,65000);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  LoadNixSock;
  SetLength(FTempBuffer,65000);
{$ENDIF}{$ENDIF}{$ENDIF}
  FSocket:=INVALID_SOCKET;
  NewSockID:=FSocket;
  end;

destructor TRtcSocket.Destroy;
  begin
{$IFDEF POSIX}
  SetLength(FTempBuffer,0);
{$ELSE}{$IFDEF RTC_NIX_SOCK}
  SetLength(FTempBuffer,0);
{$ENDIF}{$ENDIF}
  if not Sock_Invalid then Sock_Close;
  inherited;
  end;

function TRtcSocket.Listen(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI): boolean;
  begin
  Result:=False;
  Sock_ResetError;

  if FAddr = '' then
    Sock_Bind('0.0.0.0',FPort)
  else
    Sock_Bind(FAddr,FPort);
  if Sock_CheckError then Exit;

  Sock_Listen(TOA);
  Result:= not Sock_CheckError;
  if not Result then
    Sock_Close;
  end;

function TRtcSocket.WaitingSockets(vTimeout:integer): integer;
  begin
  Result:=-1;
  Sock_ResetError;

  if Sock_CanRead(vTimeout) then
    Result:=1
  else if not Sock_CheckError then
    Result:=0;
  end;

function TRtcSocket.GetNewSocket: TRtcSocket;
  var
    Sck:TSocket;
  begin
  Result:=nil;
  Sock_ResetError;

  Sck:=Sock_Accept;
  if Sock_CheckError then Exit;

  Result:=TRtcSocket.Create;
  Result.NewSockID:=Sck;
  end;

function TRtcSocket.StartNewSocket: boolean;
  begin
  Sock_ResetError;

  Sock_SetSocket(NewSockID);
  Result:= not Sock_CheckError;
  end;

function TRtcSocket.Close: boolean;
  begin
  Result:=False;
  Sock_ResetError;

  if not Sock_Invalid then
    begin
    Sock_Close;
    Result:=True;
    end;
  end;

function TRtcSocket.Connect(const FAddr,FPort:RtcString; const TOA:TRtcTimeoutsOfAPI): boolean;
  begin
  Sock_ResetError;

  Sock_Connect(FAddr,FPort,TOA);
  Result:= not Sock_CheckError;

  if not Result then Sock_Close;
  end;

function TRtcSocket.ReceiveEx(var Str: RtcByteArray; vTimeout:integer): boolean;
  var
    r,l: integer;
  begin
  Sock_ResetError;

  l:=Sock_WaitingData;
  if l<0 then
    begin
    SetLength(Str,0);
    Result:=False;
    end
  else
    begin
    if l=0 then // nothing to read yet?
      if not Sock_CanRead(vTimeout) then // wait for new data
        begin
        SetLength(Str,0);
        Result:=not Sock_CheckError;
        Exit;
        end
      else
        l:=Sock_WaitingData;

    if l>0 then
      begin
      if l>SOCK_MAX_READ_SIZE then
        l:=SOCK_MAX_READ_SIZE;
      SetLength(Str,l);
      r:=Sock_RecvBuffer(Str[0],l);
      if r<>l then // received size has to be equal to "WaitingData"
        begin
        if r>=0 then
          begin
          FErrCode:=-1;
          FErr:='Reading error';
          end;
        SetLength(Str,0);
        Result:=False;
        end
      else
        Result:=not Sock_CheckError;
      end
    else // can read but nothing to read? error!
      begin
      SetLength(Str,0);
      FErrCode:=-1;
      FErr:='Connection error';
      Result:=False;
      end;
    end;
  end;

function TRtcSocket.SendEx(var Str: RtcByteArray; at: integer; vTimeout:integer): Integer;
  begin
  Sock_ResetError;

  Result:=Sock_SendBuffer(Str[at-1],length(Str)-at+1);
  if Result=0 then
    if Sock_CanWrite(vTimeout) then
      begin
      Result:=Sock_SendBuffer(Str[at-1],length(Str)-at+1);
      if Result=0 then
        Result:=-1; // error!
      end;

  if Result>=0 then
    if Sock_CheckError then
      Result:=-1;
  end;

function TRtcSocket.GetLastErrorText: String;
  begin
  if FErrCode=0 then
    Result:=''
  else
    Result:='#'+IntToStr(FErrCode)+': '+FErr;
  end;

function TRtcSocket.GetLocalAddr: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetLocalSinIP
  else
    Result:='';
  end;

function TRtcSocket.GetLocalPort: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetLocalSinPort
  else
    Result:='';
  end;

function TRtcSocket.GetPeerAddr: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetRemoteSinIP
  else
    Result:='';
  end;

function TRtcSocket.GetPeerPort: RtcString;
  begin
  if not Sock_Invalid then
    Result:=Sock_GetRemoteSinPort
  else
    Result:='';
  end;

end.

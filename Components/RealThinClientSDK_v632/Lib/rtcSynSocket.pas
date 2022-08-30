{
  RealThinClient Synchronous Socket implementation
  - Copyright 2004-2014 (c) RealThinClient.com (http://www.realthinclient.com)

  @exclude
}
unit rtcSynSocket;

interface

{$include rtcDefs.inc}

uses
  SysUtils,

  rtcTypes,
  rtcInfo,
  rtcLog,

  rtcSynAPI,
  rtcSockBase,
  rtcSockBaseSyn;

const
  RTC_SYNSOCK_LISTEN_WAIT:integer=100;
  RTC_SYNSOCK_READ_WAIT:integer=20;
  RTC_SYNSOCK_SEND_WAIT:integer=20;

type
  TRtcSynSocket=class(TRtcSocketBaseSyn)
    (*
    FAddr: RtcString;
    FPort: RtcString;

    FProtocol: TRtcSocketProtocol;
    FReuseAddr: Boolean;
    FMultiCastAddr: RtcString;
    FMultiThreaded: boolean;
    FMultiCast: Boolean;
    FMultiCastIpTTL: Integer;
    FTimeoutsOfAPI: TRtcTimeoutsOfAPI;
    *)
  protected

    HSock:TRtcSocket;

    procedure Need_HSock;

    { Start Listener (Server).
      Return TRUE if everyting OK and Listener started. }
    function api_Listen:boolean; override;

    { How many Sockets are waiting to connect on this listening Server?
      Return -1 if Listener is NOT working anymore and should be terminated,
      Return 0 if there were no new Sockets waiting but Listener is OK,
      Return the number of waiting Sockets if there are new Sockets waiting. }
    function api_WaitingSockets:integer; override;

    { Get New Socket from Server Listener.
      If error no Socket waiting, return NIL. }
    function api_GetNewSocket:TRtcSocketBase; override;

    { This Socket was created with api_GetNewSocket.
      Using api_StartNewSocket, the connection has to be prepared for use (Bind).
      Return TRUE if connection is now ready for use, FALSE if error. }
    function api_StartNewSocket:boolean; override;

    { Open a new Connection (client).
      Return TRUE if connection is Open, FALSE if error connecting. }
    function api_Connect:boolean; override;

    { Close Socket (Client or Server).
      Return TRUE if socket closed, FALSE if error. }
    function api_Close:boolean; override;

    { Receive as much data as you can.
      Return TRUE if everything OK and put read data into "Str".
      Return empty Str if nothing to read but connection is OK.
      Return FALSE if error while reading.  }
    function api_ReceiveEx(var Str : RtcByteArray): boolean; override;

    { Send as much data a possible from "Str", starting from position "at" (first char at=1).
      Return the number of characters sent, 0 if can not send now and -1 if error. }
    function api_SendEx(var Str : RtcByteArray; at:integer):Integer; override;

    { Address of connected Peer }
    function api_GetPeerAddr: RtcString; override;
    { Port number of connected Peer }
    function api_GetPeerPort: RtcString; override;

    { Local connection address }
    function api_GetLocalAddr: RtcString; override;
    { Local connection port }
    function api_GetLocalPort: RtcString; override;

    { Text of the Last error }
    function api_GetLastErrorText: String; override;

  public
    constructor Create; override;
    destructor Destroy; override;
    end;

implementation

{ TRtcSynSocket }

constructor TRtcSynSocket.Create;
  begin
  inherited;
  HSock:=nil;
  end;

destructor TRtcSynSocket.Destroy;
  begin
  try
    RtcFreeAndNil(HSock);
    inherited;
  except
    on E:Exception do
      begin
      if LOG_AV_ERRORS then
        Log('TRtcSynSocket.Destroy',E,'ERROR');
      raise;
      end;
    end;
  end;

procedure TRtcSynSocket.Need_HSock;
  begin
  if not assigned(HSock) then
    HSock:=TRtcSocket.Create;
  end;

function TRtcSynSocket.api_Connect: boolean;
  begin
  Need_HSock;
  Result:=HSock.Connect(FAddr,FPort,TimeoutsOfAPI);
  end;

function TRtcSynSocket.api_Listen: boolean;
  begin
  Need_HSock;
  Result:=HSock.Listen(FAddr,FPort,TimeoutsOfAPI);
  end;

function TRtcSynSocket.api_WaitingSockets: integer;
  begin
  Result:=HSock.WaitingSockets(RTC_SYNSOCK_LISTEN_WAIT);
  end;

function TRtcSynSocket.api_GetNewSocket: TRtcSocketBase;
  var
    cl:TRtcSocketBaseClass;
    Sck:TRtcSocket;
  begin
  Result:=nil;
  Sck:=HSock.GetNewSocket;
  if assigned(Sck) then
    begin
    cl:=TRtcSocketBaseClass(ClassType);
    Result:=cl.Create;
    TRtcSynSocket(Result).HSock:=Sck;
    end;
  end;

function TRtcSynSocket.api_StartNewSocket: boolean;
  begin
  Result:=HSock.StartNewSocket;
  end;

function TRtcSynSocket.api_Close: boolean;
  begin
  Result:=HSock.Close;
  end;

function TRtcSynSocket.api_ReceiveEx(var Str: RtcByteArray): boolean;
  begin
  Result:=HSock.ReceiveEx(Str,RTC_SYNSOCK_READ_WAIT);
  end;

function TRtcSynSocket.api_SendEx(var Str: RtcByteArray; at: integer): Integer;
  begin
  Result:=HSock.SendEx(Str,at,RTC_SYNSOCK_SEND_WAIT);
  end;

function TRtcSynSocket.api_GetLastErrorText: String;
  begin
  Result:=HSock.GetLastErrorText;
  end;

function TRtcSynSocket.api_GetLocalAddr: RtcString;
  begin
  Result:=HSock.GetLocalAddr;
  end;

function TRtcSynSocket.api_GetLocalPort: RtcString;
  begin
  Result:=HSock.GetLocalPort;
  end;

function TRtcSynSocket.api_GetPeerAddr: RtcString;
  begin
  Result:=HSock.GetPeerAddr;
  end;

function TRtcSynSocket.api_GetPeerPort: RtcString;
  begin
  Result:=HSock.GetPeerPort;
  end;

end.


unit uSocket;

interface
  uses Windows, Classes, SysUtils, ExtCtrls, WinSock, ScktCompSy;

type

  TuClientSocket = class(TClientSocket)
  public
    function ReceiveText: AnsiString; inline;
    procedure SendText(const S: AnsiString); inline;
    function SendBuf(var Buf; Count: Integer): Integer; inline;
  end;

  TuServerClientSocket = class(TServerClientWinSocket)
  public
    Index: Integer;
    Created: LongWord;
    TokenID: String[38];
    InData: AnsiString;
    InDataCache: AnsiString;
    SendData: AnsiString;
    SendDataCache: AnsiString;
    Validated: Boolean;
    Data: TObject;
    constructor Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
    procedure Append(const S: AnsiString);
  end;

  TuServerSocket = class(TServerSocket)
  private
    procedure DoGetSocket(Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket);
    function GetClient(Index: Integer): TuServerClientSocket;
    function GetClientCount: Integer;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property Address;
    property ClientCount: Integer read GetClientCount;
    property Client[Index: Integer]: TuServerClientSocket read GetClient;
  end;

function PortInUse(const APort:Integer):Boolean;

implementation

function PortInUse(const APort:Integer):Boolean;
var
  S:TSocket;
  WSD :TWSAData;
  SockAddrIn:TSockAddrIn;
begin
  Result:=False;
  if (WSAStartup(MAKEWORD(2,2),WSD)= 0) then
  begin
    S := Socket(AF_INET,SOCK_STREAM,IPPROTO_TCP);
    try
      if (S <> SOCKET_ERROR) then
      begin
        SockAddrIn.sin_family := AF_INET;
        SockAddrIn.sin_addr.S_addr := htonl(INADDR_ANY);
        SockAddrIn.sin_port:= htons(APort);
        if Bind(S, SockAddrIn,SizeOf(SockAddrIn))<>0 then
          if WSAGetLastError = WSAEADDRINUSE then
            Result:=True;
      end;
    finally
      CloseSocket(S);
      WSACleanup();
    end;
  end;
end;

{ TuServerClientSocket }

constructor TuServerClientSocket.Create(Socket: TSocket; ServerWinSocket: TServerWinSocket);
begin
  Index := -1;
  Created := GetTickCount;
  TokenID := '';
  InData := '';
  InDataCache := '';
  SendData := '';
  SendDataCache := '';
  Validated := False;
  Data := nil;
  inherited Create(Socket, ServerWinSocket);
end;

procedure TuServerClientSocket.Append(const S: AnsiString);
begin
  Lock;
  try
    SendData := SendData + S;
  finally
    UnLock;
  end;
end;

{ TuServerSocket }

constructor TuServerSocket.Create(AOwner: TComponent);
begin
  inherited;
  Socket.OnGetSocket := DoGetSocket;
end;

destructor TuServerSocket.Destroy;
begin
  inherited;
end;

procedure TuServerSocket.DoGetSocket(Sender: TObject; Socket: TSocket; var ClientSocket: TServerClientWinSocket);
begin
  ClientSocket := TuServerClientSocket.Create(Socket, Self.Socket);
end;

function TuServerSocket.GetClient(Index: Integer): TuServerClientSocket;
begin
  Result := TuServerClientSocket(FServerSocket.Connections[Index]);
end;

function TuServerSocket.GetClientCount: Integer;
begin
  Result := FServerSocket.ActiveConnections;
end;

{ TuClientSocket }

function TuClientSocket.ReceiveText: AnsiString;
begin
  Result := Socket.ReceiveText;
end;

procedure TuClientSocket.SendText(const S: AnsiString);
begin
  Socket.SendText(S);
end;

function TuClientSocket.SendBuf(var Buf; Count: Integer): Integer;
begin
  Result := Socket.SendBuf(Buf, Count);
end;

end.

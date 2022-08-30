unit PlugManage;

interface

uses
  Windows, Classes, SysUtils, Grobal2, EDcode, Share, IniFiles, RSA, SDK, JSocket;

type
  TPlugManage = class(TThread)
    ClientSocket: TClientSocket;
    SocketStream: TWinSocketStream;
    CheckStatus: TCheckStatus;
    ServerAddrIndex: Integer;
    CheckStep: TCheckStep;
    nRemoteAddr: Integer;
    sReviceMsg: string;
    boReadyCheck: Boolean;
  private
    FFirst: Boolean;
    boGetFromIPArray: Boolean;
    procedure ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket); dynamic;
    procedure ClientSocketDisconnect(Sender: TObject; Socket: TCustomWinSocket); dynamic;
    procedure ClientSocketError(Sender: TObject; Socket: TCustomWinSocket; ErrorEvent: TErrorEvent; var ErrorCode: Integer); dynamic;
    procedure ClientSocketRead(Sender: TObject; Socket: TCustomWinSocket); dynamic;
    procedure ProcessServerPacket(); dynamic;
    procedure SendCheckServerMsg(); dynamic;
    procedure SendSocket(DefMsg: TDefaultMessage; sMsg: string); dynamic;
  protected
    procedure Execute; override; //因为父类是abstract 所以要在这里实现. 否则异常
  public
    constructor Create(CreateSuspended: Boolean); dynamic;
    destructor Destroy; override;
  end;

function GetLicenseInfo(var nDay: Integer; var dwM2Crc: PChar): Integer; stdcall;
function IsInAddrList(sIPaddr: string): Boolean;

var
  PlugEngine                : TPlugManage;
  //UserLicense               : TUserLicense;
  ServerAddr                : array[0..15] of TIPAddr = (
    (A: 61; B: 128; C: 194; D: 169; Port: 63000),
    (A: 61; B: 128; C: 194; D: 170; Port: 63000),
    (A: 219; B: 153; C: 11; D: 47; Port: 63000),
    (A: 61; B: 128; C: 194; D: 171; Port: 63000),
    (A: 61; B: 128; C: 194; D: 172; Port: 63000),
    (A: 219; B: 153; C: 11; D: 53; Port: 63000),
    (A: 61; B: 128; C: 194; D: 173; Port: 63000),
    (A: 61; B: 128; C: 194; D: 174; Port: 63000),
    (A: 61; B: 128; C: 194; D: 175; Port: 63000),
    (A: 61; B: 128; C: 194; D: 176; Port: 63000),
    (A: 219; B: 153; C: 11; D: 60; Port: 63000),
    (A: 219; B: 153; C: 11; D: 9; Port: 63000),
    (A: 219; B: 153; C: 11; D: 57; Port: 63000),
    (A: 219; B: 153; C: 7; D: 82; Port: 63000),
    (A: 219; B: 153; C: 7; D: 90; Port: 63000),
    (A: 219; B: 153; C: 7; D: 78; Port: 63000));

implementation

uses HUtil32, ClassModule, Module;
var
  SendLocalRSAKey           : Int64 = 36865;
  SendRSA_N                 : Int64 = 1594223867;

function IsInAddrList(sIPaddr: string): Boolean;
var
  i                         : Integer;
  sIP                       : string;
begin
  Result := False;
  for i := Low(ServerAddr) to High(ServerAddr) do begin
    sIP := MakeIPToStr(ServerAddr[i]);
    if sIP = sIPaddr then begin
      Result := True;
      Break;
    end;
  end;
end;

function GetLicenseInfo(var nDay: Integer; var dwM2Crc: PChar): Integer;
begin
  nDay := g_DConfig.nRemainDays;
  dwM2Crc := PChar(g_DConfig.g_dwM2Crc);
  Result := g_DConfig.nUserCount;
end;

{ ThreadPlug }

procedure TPlugManage.ClientSocketConnect(Sender: TObject; Socket: TCustomWinSocket);
begin
  CheckStep := c_SendClinetKey;
  CheckStatus := c_Checking;
  nRemoteAddr := MakeIPToInt(Socket.RemoteAddress);
end;

procedure TPlugManage.ClientSocketDisconnect(Sender: TObject;
  Socket: TCustomWinSocket);
begin

end;

procedure TPlugManage.ClientSocketError(Sender: TObject;
  Socket: TCustomWinSocket; ErrorEvent: TErrorEvent;
  var ErrorCode: Integer);
begin
  CheckStatus := c_CheckError;
  ErrorCode := 0;
  Socket.Close;
end;

procedure TPlugManage.ClientSocketRead(Sender: TObject;
  Socket: TCustomWinSocket);
begin
  sReviceMsg := sReviceMsg + Socket.ReceiveText;
end;

constructor TPlugManage.Create(CreateSuspended: Boolean);
begin
  inherited;
  FFirst := False;
  boReadyCheck := False;
  ClientSocket := TClientSocket.Create(nil);
  ClientSocket.ClientType := ctBlocking;
  ClientSocket.OnConnect := ClientSocketConnect;
  ClientSocket.OnDisconnect := ClientSocketDisconnect;
  ClientSocket.OnError := ClientSocketError;
  ClientSocket.OnRead := ClientSocketRead;
  CheckStatus := c_Idle;
  CheckStep := c_None;
  ServerAddrIndex := -1;
  boGetFromIPArray := False;
end;

destructor TPlugManage.Destroy;
begin
  inherited;
end;


procedure TPlugManage.Execute;
begin
  inherited;

end;

procedure TPlugManage.ProcessServerPacket;
var
  sData                     : string;
  sDefMsg                   : string;
  sBlockMsg                 : string;
  DefMsg                    : TDefaultMessage;
begin
  try
    while (True) do begin
      if Pos('!', sReviceMsg) <= 0 then Break;
      sReviceMsg := ArrestStringEx(sReviceMsg, '#', '!', sData);
      if sData <> '' then begin
        sBlockMsg := GetValidStr3(sData, sDefMsg, ['/']);
        if Length(sDefMsg) = DEFBLOCKSIZE then begin
          DefMsg := DecodeMessage(sDefMsg);
//取消版本验证  Development 2019-01-11         
//          if (CheckStep = c_SendClinetKey) and (DefMsg.Ident = GM_CHECKCLIENT) then begin
//            nLocalXORKey := GetXORKey();
//            g_DConfig.dwServerTick := MakeLong(DefMsg.Tag, DefMsg.Series);
//            sReviceMsg := '';
//            Break;
//          end;
        end;
      end;
    end;
  except
  end;
end;

procedure TPlugManage.SendCheckServerMsg;
var
  DefMsg                    : TDefaultMessage;
  CodeBuff                  : PChar;
  nLen                      : Integer;
begin
  CheckStep := c_SendClinetKey;
  DefMsg.Ident := GM_CHECKSERVER;
  DefMsg.Recog := nRemoteAddr;
  DefMsg.Param := 1;
  nLen := GetCodeBlock(CodeBuff);
  DefMsg.Tag := nLen;
  SendSocket(DefMsg, EncodeString(EncodeBuffer(CodeBuff, nLen)));
end;

procedure TPlugManage.SendSocket(DefMsg: TDefaultMessage; sMsg: string);
var
  sSendMsg                  : string;
begin
  sSendMsg := '#' + EncodeMessage(DefMsg) + '/' + sMsg + '!';
  if ClientSocket.Socket.Connected then
    ClientSocket.Socket.SendText(sSendMsg);
end;

end.


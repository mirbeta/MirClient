unit uLocalMessageer;

interface
  uses Windows, Messages, Classes, SysUtils, ExtCtrls, Graphics,
  OverbyteIcsWndControl, OverbyteIcsWSocket, OverbyteIcsWSocketS,
  MShare, uGameEngine, uCommon;

const
  /// /////////////////////////////////////////////////////////////////////////////
  CL_CLIENTOPEN  = 10000;
  CL_CLIENTCLOSE = 10001;
  CL_RES_REQUST = 10002;
  CL_RES_NEWFILE = 10003;
  CL_RES_IMGDOWNLOADED = 10004;
  CL_RES_FILEDOWNLOADED = 10005;
  CL_RES_UPDATEFILE = 10006;//通知所有客户端有图片文件需要更新。


type
  TLocalMessageer = class(TInterfacedObject)
  private
    FList: TList;
    FTimer: TTimer;
    FSocket: TWSocket;
    FMini: Boolean;
    FTilesTick: LongWord;
    FProcessTick:DWORD;
    FLastConnectingTime:DWORD;
    FSocketData:AnsiString;
    FEncodeResponseSize:Integer;
    procedure AddRequest(const FileName: String; ImageIndex: Integer;Important: Boolean);
    procedure AddFileRequest(const FileName: String; Important: Boolean);
    procedure SendRequest(ARequest: pTMiniResRequest);
    procedure Clear;
    procedure DoTimer(Sender: TObject);
    procedure OnDataAvailable(Sender: TObject; ErrCode: Word);
    procedure ProcessSocketData();
    function GetPort: String;
    procedure CheckTitleFile(const AFileName: String);
    procedure CheckMapFile(const AFileName: String);
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddDownloadImage(const FileName: String; ImageIndex: Integer;Important:Boolean);
    procedure AddDownloadFile(const FileName: String; Important: Boolean);
    procedure AddDownLoadSoundFile(const FileName: String; Important: Boolean);
    procedure ReloadImageFileFinished(const FileName:String); //刷新图片文件完成
    procedure SetAddress(const Value: String);
    procedure SetPort(Value: Word);
    procedure SetMini(Value: Boolean);
    procedure Open;
    procedure Close;
  end;

var
  g_LocalMessager: TLocalMessageer;

implementation
  uses Wil, SoundUtil, MapUnit,EDcode, HUtil32;

{ TLocalMessageer }

procedure TLocalMessageer.AddDownloadFile(const FileName: String; Important: Boolean);
var
  AFileName: String;
begin
  if FMini then
  begin
    AFileName := FileName;
    if AFileName <> '' then
    begin
      if AFileName[1] = '.' then
        Delete(AFileName, 1, 1);
      if AFileName <> '' then
      begin
        if AFileName[1] = '\' then
          Delete(AFileName, 1, 1);
      end;
      if (AFileName <> '')  then
        AddFileRequest(AFileName, Important);
    end;
  end;
end;

procedure TLocalMessageer.AddDownloadImage(const FileName: String;
  ImageIndex: Integer;Important:Boolean);
begin
  if FMini then
  begin
    if (FileName <> '') then
      AddRequest(FileName, ImageIndex,Important);
  end;
end;

procedure TLocalMessageer.AddDownLoadSoundFile(const FileName: String;
  Important: Boolean);
var
  ARequest: PTMiniResRequest;
begin
  New(ARequest);
  ARequest._Type := 2;
  ARequest.FileName := FileName;
  ARequest.Index := 0;
  ARequest.Important := False;
  FList.Add(ARequest);
end;

procedure TLocalMessageer.AddRequest(const FileName: String; ImageIndex: Integer;Important: Boolean);
var
  ARequest: PTMiniResRequest;
begin
  New(ARequest);
  ARequest._Type := 0;
  ARequest.FileName := FileName;
  ARequest.Index := ImageIndex;
  ARequest.Important := Important;
  ARequest.FailCount := 0;
  FList.Add(ARequest);
end;

procedure TLocalMessageer.AddFileRequest(const FileName: String; Important: Boolean);
var
  ARequest: PTMiniResRequest;
begin
  New(ARequest);
  ARequest._Type := 1;
  ARequest.FileName := FileName;
  ARequest.Important := Important;
  FList.Add(ARequest);
end;


procedure TLocalMessageer.ReloadImageFileFinished(const FileName: String);
var
  ARequest: PTMiniResRequest;
begin
  New(ARequest);
  ARequest._Type := 3;
  ARequest.FileName := FileName;
  ARequest.Important := False;
  FList.Add(ARequest);
end;

procedure TLocalMessageer.Clear;
var
  I: Integer;
begin
  for I := 0 to FList.Count - 1 do
    FreeMem(FList.Items[I]);
end;

procedure TLocalMessageer.Close;
begin
  FSocket.Close;
end;

constructor TLocalMessageer.Create;
var
  AResponse : TMiniResResponse;
  S:AnsiString;
begin
  FList := TList.Create;
  FTimer := TTimer.Create(nil);
  FTimer.Interval := 10;
  FTimer.Enabled := False;
  FTimer.OnTimer := DoTimer;
  FSocket := TWSocket.Create(nil);
  FSocket.Addr := '127.0.0.1';//
  FSocket.Port := '7300';
  FSocket.OnDataAvailable := OnDataAvailable;
  Wil.LibManager.OnAddDownloadImage := AddDownloadImage;
  g_SndMgr.OnAddDownloadSoundFile := AddDownLoadSoundFile;
  g_SndMgr.OnAddDownloadFile := AddDownloadFile;
  MapUnit.OnAddDownloadMapFile := AddDownloadFile;
  Wil.LibManager.OnReLoadFinished := ReloadImageFileFinished;
  FProcessTick := GetTickCount;
  S := EncodeBuffer(PAnsiChar(@AResponse),SizeOf(AResponse));
  FEncodeResponseSize := Length(S);
  S:= '';
end;

destructor TLocalMessageer.Destroy;
begin
  FreeAndNilEx(FTimer);
  FreeAndNilEx(FList);                                                            
  FreeAndNilEx(FSocket);
  inherited;
end;

procedure TLocalMessageer.DoTimer(Sender: TObject);
var
  ARequest: PTMiniResRequest;
  I:Integer;
begin
  if FSocket.State = wsClosed then
  begin
    if GetTickCount - FLastConnectingTime > 3000 then
    begin
      FLastConnectingTime := GetTickCount;
      try
        //FSocket.Connect;
      except
        On E: ESocketException do
        begin
          //吃掉
        end;
      end;
    end;
    
  end;

  ProcessSocketData;
   
  if FSocket.State in [wsOpened,wsConnected] then
  begin
    FProcessTick := GetTickCount;
    if FList.Count > 0 then
    begin
      ARequest := FList.Items[0];
      try
        Try
          FList.Delete(0);
          SendRequest(ARequest);
        Except

        End;
      finally
        FreeMem(ARequest);
      end;
    end;
  end else
  begin
    //上次操作到现在距离10秒还没被发出去。 那么丢弃
    if GetTickCount - FProcessTick > 10 * 1000  then
    begin
      FProcessTick := GetTickCount;
      try
        ConsoleDebug('==============所有微端消息进行丢弃=================');
        for i := 0 to FList.Count - 1 do
        begin
          ARequest := FList.Items[i];             
          FreeMem(ARequest);
        end;      
      finally
        FList.Clear;    
      end; 
    end;
    
  end;
end;

function TLocalMessageer.GetPort: String;
begin
  Result := FSocket.Port;
end;

procedure TLocalMessageer.CheckMapFile(const AFileName: String);
begin
  if UpperCase(ExtractFileExt(AFileName)) = '.MAP' then
  begin
    g_boForceMapDraw := True;
    g_boForceMapFileLoad := True;
  end;

  if UpperCase(ExtractFileExt(AFileName)) = '.LST' then
  begin
    if Pos('91',AFileName) > 0 then
      g_SndMgr.LoadBGMusicList(AFileName)
    else
      g_SndMgr.LoadSoundList(AFileName);
  end;

end;

procedure TLocalMessageer.CheckTitleFile(const AFileName: String);
var
  AUpName: String;
begin
  AUpName := ExtractFileName(ChangeFileExt(UpperCase(AFileName),''));
  if (Pos('TILES', AUpName) > 0) or (Pos('SMTILES', AUpName) > 0) then
  begin
    if GetTickCount - FTilesTick > 50 then
    begin
      g_boForceMapDraw := True;
      FTilesTick := GetTickCount;
    end;
  end;
end;

procedure TLocalMessageer.OnDataAvailable(Sender: TObject; ErrCode: Word);
var
  AResponse: TMiniResResponse;
begin
  if ErrCode = 0 then
  begin
    FSocketData := FSocketData + FSocket.ReceiveStrA;
   end;
end;

procedure TLocalMessageer.Open;
begin
  FSocket.Connect;
  FTimer.Enabled := True;
end;

procedure TLocalMessageer.ProcessSocketData;
var
  AResponse: TMiniResResponse;
  SockData:AnsiString;
begin
  while True do
  begin
    if Pos('!',FSocketData) > 0 then
    begin
      FSocketData := AnsiArrestStringEx(FSocketData,'#','!',SockData);
      if (SockData <> '') and (Length(SockData) = FEncodeResponseSize ) then
      begin
        DecodeBuffer(SockData,PAnsiChar(@AResponse),SizeOf(AResponse));
        case AResponse.Ident of
          10016:
          begin
            g_Application.AddToChatBoardString('【警告】系统速度不正常，请关闭加速类软件', clBlue, clRed);
          end;
          10017:
          begin
            g_Application.AddToChatBoardString('【警告】请勿使用加速软件，连接断开', clBlue, clRed);
            g_Application.DisConnect;
          end;
          10018:
          begin
            g_Application.AddToChatBoardString('【警告】发现系统中运行有外挂软件', clBlue, clRed);
          end;
          10019:
          begin
            g_Application.AddToChatBoardString('【警告】请勿使用外挂软件，连接断开', clBlue, clRed);
            g_Application.DisConnect;
          end;
          10020: g_Application.Terminate;
          CL_RES_IMGDOWNLOADED:
          begin
            g_Application.LoadImage(AResponse.FileName, AResponse.Index, AResponse.Position);
            CheckTitleFile(AResponse.FileName);
          end;
          CL_RES_FILEDOWNLOADED:
          begin
            ConsoleDebug('微端文件下载成功:' + AResponse.FileName);
            CheckMapFile(AResponse.FileName);
          end;
          CL_RES_UPDATEFILE:
          begin
            ConsoleDebug('微端服务器通知重新加载文件:' + AResponse.FileName);
            Wil.LibManager.ReLoadImageFile(AResponse.FileName);
            CheckTitleFile(AResponse.FileName);
          end;
        end;

      end else
        break;
    end else
      Break;

  end;
end;

procedure TLocalMessageer.SendRequest(ARequest: pTMiniResRequest);
var
  SocketData:AnsiString;
begin
  if ARequest <> nil then
  try
    SocketData := EncodeBuffer(PAnsiChar(ARequest),SizeOf(TMiniResRequest));
    FSocket.SendStr('#'+ SocketData + '!');
  except
  end;
end;

procedure TLocalMessageer.SetAddress(const Value: String);
begin
  FSocket.Addr := Value;
end;

procedure TLocalMessageer.SetMini(Value: Boolean);
begin
  FMini := Value;
  LibManager.MiniOpend := Value;
end;

procedure TLocalMessageer.SetPort(Value: Word);
begin
  FSocket.Port := IntToStr(Value);
end;

//initialization
//  g_LocalMessager := TLocalMessageer.Create;
//
//finalization
//  FreeAndNilEx(g_LocalMessager);

end.

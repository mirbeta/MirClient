unit fmxServer_Form;

{ To compile the project with StreamSec Tools 2.1+ components, declare
  the StreamSecII compiler directive below or in the "rtcDeploy.inc" file. }

{$include rtcDeploy.inc}
{$include rtcDefs.inc}

{.$DEFINE StreamSecII}

interface

uses
  SysUtils, Classes,

  FMX.Controls, FMX.Forms,
  FMX.Dialogs, FMX.ExtCtrls,
  FMX.Types, FMX.Edit,

  rtcLog, rtcSyncObjs,
  rtcInfo, rtcConn, rtcTimer,
  rtcDataSrv, rtcHttpSrv, rtcPlugins,

  AppServer_Module,

{$IFDEF StreamSecII}
  rtcSSecTest,
{$ENDIF}

  rtcFMX.GUI, // handle Thead.Synchronize() using a FMX Timer.

  rtcThrPool;


const
  LOOP_MAX_AVG=20;

type
  TForm1 = class(TForm)
    RtcDataServer1: TRtcHttpServer;
    Label1: TLabel;
    btnListen: TButton;
    Label2: TLabel;
    ePort: TEdit;
    Label3: TLabel;
    lblCliCnt: TLabel;
    xEncrypt: TCheckBox;
    xMultiThreaded: TCheckBox;
    xCompress: TCheckBox;
    Timer1: TTimer;
    eThreads: TEdit;
    Label5: TLabel;
    Label7: TLabel;
    xMonitorDataInOut: TCheckBox;
    lblDataInOut: TLabel;
    xBlocking: TCheckBox;
    xCryptPlugin: TCheckBox;
    lblPluginState: TLabel;
    xRTCTimeouts: TCheckBox;
    Label4: TLabel;
    lblResult: TLabel;
    xAPITimeouts: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure RtcDataServer1ListenStart(Sender: TRtcConnection);
    procedure RtcDataServer1ListenError(Sender: TRtcConnection; E: Exception);
    procedure RtcDataServer1ListenStop(Sender: TRtcConnection);
    procedure btnListenClick(Sender: TObject);
    procedure RtcDataServer1ClientConnect(Sender: TRtcConnection);
    procedure RtcDataServer1ClientDisconnect(Sender: TRtcConnection);
    procedure xEncryptClick(Sender: TObject);
    procedure xMultiThreadedClick(Sender: TObject);
    procedure xCompressClick(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
    procedure RtcDataServer1ResponseDone(Sender: TRtcConnection);
    procedure RtcDataServer1DataIn(Sender: TRtcConnection);
    procedure RtcDataServer1DataOut(Sender: TRtcConnection);
    procedure FormDestroy(Sender: TObject);
    procedure xBlockingClick(Sender: TObject);
    procedure xCryptPluginClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure xRTCTimeoutsClick(Sender: TObject);
    procedure xAPITimeoutsClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
    CS:TRtcCritSec;

    CliConnect,CliDisconnect,
    ResponseDoneCount,
    ResponseDoneIgnore,
    ResponseLoopCount,
    LoopMult:longint;

    // used for averaging over the last 10 logs
    LoopPerf,
    LoopAvg:double;
    LoopMax,LoopLoc:byte;
    LoopArr:array[1..LOOP_MAX_AVG] of double;

    LastTime,StartTime:longword;
    TotalDataIn,TotalDataOut:int64;

    RtcCryptPlugin1: TRtcCryptPlugin;
  end;

var
  Form1: TForm1;

implementation

{$R *.fmx}

procedure TForm1.FormCreate(Sender: TObject);
  begin
  StartLog;

{$IFDEF StreamSecII}
  AddServerRootCertFile('root.cer');
  AddServerPFXFile('server.pfx','abc');
  RtcCryptPlugin1 := GetServerCryptPlugin;

  if assigned(RtcCryptPlugin1) then
    begin
    lblPluginState.Caption:='CryptPlugin enables SSL (StreamSec Tools II)';
    xCryptPlugin.Caption:='Use CryptPlugin (enable SSL)';
    xCryptPlugin.Font.Style:=[fsBold];
    end
  else
{$ENDIF}
    begin
    { If encryption components are not used, will be using a dummy plugin
      which only passes data through and does NOT encrypt nor decrypt data.
      In other words, the TRtcDummyCryptPlugin does NOT add SSL support! }
    RtcCryptPlugin1 := TRtcDummyCryptPlugin.Create(self);
    lblPluginState.Text:='Using CryptPlugin dummy: SSL is NOT enabled!';
    end;

  CS:=TRtcCritSec.Create;

  {$ifdef rtcTest}
  { If we wanted to test how fast compression works,
    we could force data compression even when we know there will
    be no gain in compressing the data (only CPU usage increase). }
  // RTC_MIN_COMPRESS_SIZE:=0;
  {$endif}

  CliConnect:=0;
  CliDisconnect:=0;

  GetAppSrvModule.ServerLink.Server:=RtcDataServer1;

  // Using properties set up by form
  xCompressClick(xCompress);
  xEncryptClick(xEncrypt);
  xMultiThreadedClick(xMultiThreaded);
  xCryptPluginClick(xCryptPlugin);
  xBlockingClick(xBlocking);

  ResponseDoneCount:=0;
  ResponseLoopCount:=0;
  LoopMax:=0;
  LastTime:=0;
  StartTime:=0;
  end;

procedure TForm1.RtcDataServer1ListenStart(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenStart)
  else
    begin
    Timer1.Enabled:=True;

    StartTime:=GetTickTime;
    LastTime:=0;
    LoopMult:=1;


    Log('Time (sec); Conn+ ; Conn- ; Conn= ; Responses; Avg/sec; 10Avg/sec; Curr/sec','PERF');
    btnListen.Text:='Stop';
    Label1.Text:='Listening on Port '+Sender.sLocalPort;
    end;
  end;

procedure TForm1.RtcDataServer1ListenError(Sender: TRtcConnection; E: Exception);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenError,E)
  else
    Label1.Text:='Listening Error: '+E.Message;
  end;

procedure TForm1.RtcDataServer1ListenStop(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1ListenStop)
  else
    begin
    Timer1.Enabled:=False;
    btnListen.Text:='Listen';
    Label1.Text:='Stopped listening.';
    end;
  end;

procedure TForm1.btnListenClick(Sender: TObject);
  begin
  if RtcDataServer1.isListening then
    RtcDataServer1.StopListen
  else
    begin
    ResponseDoneCount:=0;
    TotalDataIn:=0;
    TotalDataOut:=0;
    LoopMax:=0;
    LastTime:=0;
    StartTime:=0;

    // DO NOT CHANGE THIS WHEN CLIENT OR SERVER ARE RUNNING !!!
    RTC_THREAD_POOL_MAX:=StrToInt(eThreads.Text);

    lblDataInOut.Text:='???';

    if xMonitorDataInOut.isChecked then
      begin
      RtcDataServer1.OnDataIn:=RtcDataServer1DataIn;
      RtcDataServer1.OnDataOut:=RtcDataServer1DataOut;
      end
    else
      begin
      RtcDataServer1.OnDataIn:=nil;
      RtcDataServer1.OnDataOut:=nil;
      end;
    RtcDataServer1.ServerPort:=RtcString(ePort.Text);
    RtcDataServer1.Listen;
    end;
end;

procedure TForm1.RtcDataServer1ClientConnect(Sender: TRtcConnection);
begin
  CS.Acquire;
  try
    Inc(CliConnect);
  finally
    CS.Leave;
    end;
end;

procedure TForm1.RtcDataServer1ClientDisconnect(Sender: TRtcConnection);
begin
  CS.Acquire;
  try
    Inc(CliDisconnect);
  finally
    CS.Leave;
    end;
  end;

procedure TForm1.xCryptPluginClick(Sender: TObject);
  begin
  if xCryptPlugin.isChecked then
    RtcDataServer1.CryptPlugin:=RtcCryptPlugin1
  else
    RtcDataServer1.CryptPlugin:=nil;
  end;

procedure TForm1.xEncryptClick(Sender: TObject);
  begin
  if xEncrypt.isChecked then
    GetAppSrvModule.ServerModule.EncryptionKey:=16
  else
    GetAppSrvModule.ServerModule.EncryptionKey:=0;
  end;

procedure TForm1.xMultiThreadedClick(Sender: TObject);
  begin
  RtcDataServer1.MultiThreaded:=xMultiThreaded.isChecked;
  end;

procedure TForm1.xCompressClick(Sender: TObject);
  begin
  if xCompress.isChecked then
    GetAppSrvModule.ServerModule.Compression:=cFast
  else
    GetAppSrvModule.ServerModule.Compression:=cNone;
  end;

procedure TForm1.Timer1Timer(Sender: TObject);
  var
    ocon,ccon,rdone:longint;
  begin
  CS.Acquire;
  try
    ocon:=CliConnect;
    ccon:=CliDisconnect;
    rdone:=ResponseDoneCount;
  finally
    CS.Release;
    end;
  lblCliCnt.Text:=IntToStr(RtcDataServer1.TotalServerConnectionCount)+
                     ' ('+IntToStr(ocon)+' - '+IntToStr(ccon)+')';
  lblResult.Text:=Format('%.0n',[rdone*1.0]);
  end;

procedure TForm1.RtcDataServer1ResponseDone(Sender: TRtcConnection);
  var
    a:integer;
    tm,lt:longword;
    rdc:longint;
    cnt1,cnt2:int64;
  begin
  cnt1:=0; cnt2:=0;
  tm:=0; lt:=0;
  CS.Acquire;
  try
    rdc:=ResponseDoneCount+1;
    ResponseDoneCount:=rdc;
  finally
    CS.Release;
    end;
  if rdc mod 1000 = 0 then
    begin
    CS.Acquire;
    try
      ResponseLoopCount:=ResponseLoopCount+1000;
      tm:=GetTickTime;
      if LastTime=0 then // ignore 1st run
        begin
        LastTime:=tm;
        StartTime:=tm;
        LoopMult:=1;
        ResponseDoneIgnore:=rdc;
        end
      else
        begin
        lt:=tm-LastTime;
        if lt<1000 then // do not measure intervals shorter than 5 seconds
          Inc(LoopMult)
        else
          begin
          cnt1:=rdc-ResponseDoneIgnore;
          cnt2:=ResponseLoopCount*LoopMult;

          LastTime:=tm;
          LoopMult:=1;
          end;
        end;
      ResponseLoopCount:=0;
    finally
      CS.Release;
      end;
    end;

  if cnt1>0 then
    begin
    LoopPerf:=(1000*cnt2)/lt;
    // Update our performance array:
    if LoopMax<LOOP_MAX_AVG then
      begin
      Inc(LoopMax);
      LoopLoc:=LoopMax;
      LoopArr[LoopLoc]:=LoopPerf;
      end
    else
      begin
      LoopLoc:=LoopLoc+1;
      if LoopLoc>LOOP_MAX_AVG then LoopLoc:=1;
      LoopArr[LoopLoc]:=LoopPerf;
      end;
    // Calculate average requests per second over the last 10 loops:
    LoopAvg:=0;
    for a:=1 to LoopMax do
      LoopAvg:=LoopAvg+LoopArr[a];
    LoopAvg:=LoopAvg/LoopMax;

    Log(RtcString(IntToStr((tm-StartTime) div 1000)+
        '; '+Format('%.0n', [CliConnect*1.0] )+
        '; '+Format('%.0n', [CliDisconnect*1.0] )+
        '; '+Format('%.0n', [Sender.TotalServerConnectionCount*1.0] )+
        '; '+Format('%.0n', [cnt1*1.0] )+ // Responses
        '; '+Format('%.1n', [(1000*cnt1)/(tm-StartTime)] )+
        '; '+Format('%.1n', [LoopAvg] ) +
        '; '+Format('%.1n', [LoopPerf] ) ),'PERF');
    end;
  end;

procedure TForm1.RtcDataServer1DataIn(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1DataIn)
  else
    begin
    TotalDataIn:=TotalDataIn+Sender.DataIn;
    lblDataInOut.Text:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TForm1.RtcDataServer1DataOut(Sender: TRtcConnection);
  begin
  if not Sender.inMainThread then
    Sender.Sync(RtcDataServer1DataOut)
  else
    begin
    TotalDataOut:=TotalDataOut+Sender.DataOut;
    lblDataInOut.Text:=IntToStr(TotalDataIn)+' + '+IntToStr(TotalDataOut)+' bytes';
    end;
  end;

procedure TForm1.FormDestroy(Sender: TObject);
  begin
{$IFDEF StreamSecII}
  Log('Releasing CryptPlugins ...','DEBUG');
  try
    try
      RtcDataServer1.CryptPlugin:=nil;
    except
      on E:Exception do
        Log('CryptPlugin:=nil',E,'DEBUG');
      end;
    ReleaseCryptPlugins;
    Log('CryptPlugins released.','DEBUG');
  except
    on E:Exception do
      Log('ERROR releasing CryptPlugins!',E,'DEBUG');
    end;
{$ENDIF}
  CS.Destroy;
  end;

procedure TForm1.xBlockingClick(Sender: TObject);
  begin
  if xBlocking.isChecked then
    RtcDataServer1.Blocking:=True
  else
    RtcDataServer1.Blocking:=False;
  end;

procedure TForm1.xRTCTimeoutsClick(Sender: TObject);
  begin
  if xRTCTimeouts.isChecked then
    begin
    RtcDataServer1.Timeout.AfterConnecting:=300; // max 5 minutes to accept a connection
    RtcDataServer1.Timeout.AfterConnect:=300; // max 5 minutes idle time
    end
  else
    begin
    RtcDataServer1.Timeout.AfterConnecting:=0; // no timeout
    RtcDataServer1.Timeout.AfterConnect:=0; // no timeout
    end;
  end;

procedure TForm1.xAPITimeoutsClick(Sender: TObject);
  begin
  if xAPITimeouts.isChecked then
    begin
    RtcDataServer1.TimeoutsOfAPI.ReceiveTimeout:=120; // max 2 minutes to receive a packet
    RtcDataServer1.TimeoutsOfAPI.SendTimeout:=120; // max 2 minutes to send a packet
    end
  else
    begin
    RtcDataServer1.TimeoutsOfAPI.ReceiveTimeout:=0; // no timeout
    RtcDataServer1.TimeoutsOfAPI.SendTimeout:=0; // no timeout
    end;
  end;

procedure TForm1.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  Timer1.Enabled:=False;
  // Clean Server shutdown ...
  RtcDataServer1.StopListen;
  while RtcDataServer1.isListening or (RtcDataServer1.TotalServerConnectionCount>0) do
    begin
    { You will need to call Application.ProcessMessages *only*
      if your Server connection component is single-threaded
      (MyRtcHttpServer.MultiThreaded=False). You don't need it otherwise.
      For Service applications, always use a MultiThreaed Server. }
    Application.ProcessMessages;
    // So we don't use up all CPU in our loop
    Sleep(10);
    end;
  CanClose:=True;
  end;

type
  TForcedMemLeakTest=class(TObject);

procedure ForceMemLeak;
  begin
  // We will create one object of this kind to test memory leak detection.
  TForcedMemLeakTest.Create;
  // "TMemLeakTest" is the only object that should remain in memory when app terminates.
  end;

initialization
{$IFDEF RtcDeploy}
ForceMemLeak;
{$ENDIF}
finalization
Log('************************************************','DEBUG');
Log('Server_Form finalized.','DEBUG');
end.

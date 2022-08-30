unit MainGateClientForm;

{ This Client is configured to work with the "RtcSimpleGateway" running on Llcalhost and Port 80.

  Check the "GateCli:TRtcHttpGateClient" component and set at least "GateAddr" and "GatePort"
  properties to your Gateway's Address and Port number if you want to access a Gateway running
  on a different PC and/or Port number.

  If you want to test this Client with the "ChatServer" Project, make sure to
  set the following two properties on the "GateCli:TRtcHttpGateClient" component:

  GatePrimaryKey := "MyPrimaryChatKey"
  GateUserAuth := "MyChatClient"

  See description in the "ChatServer" or "ChatClient" Projects for the explanation.

  Also ... if the Client has to work on a Systems with active Anti-Virus Software,
  always set the "StreamBlockSizeOut" property on the "GateCli:TRtcHttGateClient"
  component to the maximum number buffer size you want to allow Anti-Virus Software to use
  per outging stream. This limit is achieved by limiting the outgoing request size and
  forcing the Client to wait for a response from the Gateway before more data is sent. }

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ClipBrd,

  ExtCtrls, Spin,

  rtcInfo, rtcLog, rtcConn, rtcSyncObjs,

  rtcGateConst,
  rtcGateCli, rtcUdpSrv, rtcUdpCli, Buttons;

{$include rtcDeploy.inc}

{ Automatically start a new Client and fill up the Screen with Client
  instances when the user clicks "GO" for the 1st time? }
{.$DEFINE USE_AUTOSTART}

{ Use UDP to make all running clients "synchronized" by sending a message
  when a user to all other Clients on this PC and inside LAN?
{.$DEFINE USE_UDP}

const
  // "CallID" used in this test Client for sending a file
  cid_TestFileSend = 127;

type
  TMsgType=(msg_Input,msg_Output,msg_Speed,msg_Error,msg_Status,msg_Group);

type
  TGateClientForm = class(TForm)
    MainPanel: TPanel;
    shInput: TShape;
    shOutput: TShape;
    lblSendBuffSize: TLabel;
    lblRecvBufferSize: TLabel;
    btnLogIN: TSpeedButton;
    btnSendFile: TSpeedButton;
    StatusUpdate: TTimer;
    InfoPanel: TPanel;
    l_Status1: TLabel;
    l_Status2: TLabel;
    StartAnother: TTimer;
    btnCLR: TLabel;
    udpServer: TRtcUdpServer;
    udpClient: TRtcUdpClient;
    btnClose: TLabel;
    eYourID: TSpeedButton;
    eToID: TSpeedButton;
    btnReset: TSpeedButton;
    l_Groups: TLabel;
    l_Status3: TLabel;
    GateCli: TRtcHttpGateClient;
    GCM: TRtcGateClientLink;

    procedure btnLogINClick(Sender: TObject);
    procedure btnCLRClick(Sender: TObject);
    procedure btnSendFileClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure btnAddUserClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure StatusUpdateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure InfoPanelMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure InfoPanelMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure InfoPanelMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure btnCloseClick(Sender: TObject);
    procedure eToIDExit(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure eYourIDClick(Sender: TObject);
    procedure eToIDClick(Sender: TObject);
    procedure StartAnotherTimer(Sender: TObject);
    procedure udpServerDataReceived(Sender: TRtcConnection);

    procedure BeforeLogIN(Sender:TRtcConnection);
    procedure AfterLogIN(Sender:TRtcConnection);
    procedure AfterLogOUT(Sender:TRtcConnection);
    procedure AfterLoginFail(Sender:TRtcConnection);

    procedure DataReceived(Sender:TRtcConnection);
    procedure InfoReceived(Sender:TRtcConnection);
    procedure ReadyToSend(Sender:TRtcConnection);

    procedure StreamReset(Sender:TRtcConnection);

  public
    AutoStart, Used:boolean;
    xRepeatSend:boolean;
    xUseAPI:byte;

    FCS:TRtcCritSec;
    sStatus1,
    sStatus2,
    sStatus3,
    sGroups,
    sSend:String;

    MyFileName:String;

    FUserList:String;
    FUserListNew:boolean;

    FSendingFile:boolean;
    FSendFileStart,
    FSendFileLastTime,
    FSendFileLastSize:Cardinal;
    FLoginStart:Cardinal;
    CntReset:integer;

    FSendFileName:String;
    FSendFileLoc,
    FSendFileSize:int64;

    InGroupCnt,OutGroupCnt:integer;
    InGroupMe,OutGroupMe:boolean;

    procedure PrintMsg(const s:String; t:TMsgType);

    procedure StartNext;
  end;

var
  GateClientForm: TGateClientForm;

implementation

{$R *.dfm}

function FillZero(const s:RtcString;len:integer):RtcString;
  begin
  Result:=s;
  while length(Result)<len do
    Result:='0'+Result;
  end;

function Time2Str(v:TDateTime):RtcString;
  var
    hh,mm,ss,ms:word;
  begin
  DecodeTime(v, hh,mm,ss,ms);
  Result:=FillZero(Int2Str(hh),2)+':'+FillZero(Int2Str(mm),2)+':'+FillZero(Int2Str(ss),2);
  end;

procedure TGateClientForm.StartNext;
  var
    x,y:integer;
    s:String;
  begin
{$IFNDEF USE_AUTOSTART}
  Exit;
{$ENDIF}
  if Used then Exit;
  AutoStart:=False;
  Used:=True;

  x:=Left; y:=Top;
  if x>=0 then
    begin
    if x+ClientWidth*2<=Screen.Width then
      x:=x+ClientWidth
    else
      begin
      x:=0;
      y:=y+ClientHeight;
      end;
    end
  else
    begin
    if x-ClientWidth>=Screen.DesktopLeft then
      x:=x-ClientWidth
    else
      begin
      x:=-ClientWidth;
      y:=y+ClientHeight;
      end;
    end;
  if (y>=0) and (y<=Screen.Height-ClientHeight) then
    begin
    s:=ParamStr(0)+' '+IntToStr(x)+' '+IntToStr(y);
    WinExec(@(RtcStringToBytesZero(s)[0]),SHOW_OPENWINDOW);
    end;
  end;

procedure TGateClientForm.FormCreate(Sender: TObject);
  var
    x,y,loc:integer;
  begin
  StartLog;
  AutoSize:=True;

  { If you want a minimalistic client, so more can fit on the Screen,
   then uncomment the line below: }
  // MainPanel.Height:=InfoPanel.Top;

  { Set minimum expected connection speed in KBits.
    This is a global setting for all RTC components,
    so don't change it when a connection is active! }
  SetupConnectionSpeed(64,64);

  FCS:=TRtcCritSec.Create;
  sStatus1:='';
  sStatus2:='';
  sStatus3:='';
  sGroups:='';
  sSend:='SEND';
  FUserList:='';
  FUserListNew:=False;

{$IFDEF USE_UDP}
  udpServer.Listen();
  udpClient.Connect();
{$ENDIF}

  // We will be sending the EXE file
  MyFileName:=AppFileName;
  if ParamCount>1 then
    begin
    x:=StrToInt(ParamStr(1));
    y:=StrToInt(ParamStr(2));
    Left:=x; Top:=y;

    // Set the "xUseAPI" variable based on our Form position
    // to minimize setup steps required for a test of all APIs.
    if X<0 then
      loc:=(-X div ClientWidth) mod 4
    else
      loc:=(X div ClientWidth) mod 4;
    xUseAPI:=loc;

    btnLogINClick(nil);
    StartAnother.Enabled:=True;
    end
  else
    begin
    AutoStart:=True;
    Left:=0;
    Top:=0;
    end;
  end;

procedure TGateClientForm.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(FCS);
  end;

procedure TGateClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  AutoStart:=False;
  StartAnother.Enabled:=False;

  GateCli.AutoLogin:=False;
  CanClose:=True;
  end;

procedure TGateClientForm.btnLogINClick(Sender: TObject);
  begin
  if GateCli.AutoLogin then
    begin
    Log('Clicked LOG OUT (Active)',IntToStr(GateCli.MyUID));

    StartAnother.Enabled:=False;
    AutoStart:=False;

    GateCli.AutoLogin:=False;
    try
      if Copy(Clipboard.AsText,1,1)=',' then
        Clipboard.AsText:='';
    except
      end;

    {$IFDEF USE_UDP}
    if Sender=btnLogIN then
      udpClient.Write('stop');
    {$ENDIF}
    end
  else
    begin
    Log('Clicked LOG IN (Inactive)',IntToStr(GateCli.MyUID));

    StartAnother.Enabled:=AutoStart;
    AutoStart:=False;

    GateCli.AutoLogin:=True;

    {$IFDEF USE_UDP}
    if Sender=btnLogIN then
      udpClient.Write('start');
    {$ENDIF}
    end;
  end;

procedure TGateClientForm.btnSendFileClick(Sender: TObject);
  begin
  if FSendingFile and (Sender<>nil) then
    begin
    Log('Clicked STOP (Active)',IntToStr(GCM.Client.MyUID));
    xRepeatSend:=False;
    FSendingFile:=False;
    FCS.Acquire;
    try
      sSend:='SEND';
    finally
      FCS.Release;
      end;
    PrintMsg('STOPPED.',msg_Output);

    {$IFDEF USE_UDP}
    if Sender=btnSendFile then
      udpClient.Write('nosend');
    {$ENDIF}
    end
  else if File_Exists(MyFileName) then
    begin
    Log('Clicked SEND (Active)',IntToStr(GCM.Client.MyUID));
    xRepeatSend:=True;
    FSendingFile:=True;
    FCS.Acquire;
    try
      sSend:='STOP';
    finally
      FCS.Release;
      end;

    FSendFileStart:=GetAppRunTime;
    FSendFileName:=MyFileName;
    FSendFileLoc:=0;
    FSendFileSize:=File_Size(FSendFileName);

    if GCM.Client.Ready then
      ReadyToSend(nil);

    {$IFDEF USE_UDP}
    if Sender=btnSendFile then
      udpClient.Write('send');
    {$ENDIF}
    end;
  end;

procedure TGateClientForm.btnCLRClick(Sender: TObject);
  begin
  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';

  if FUserList<>'' then
    begin
    FUserListNew:=True;
    btnAddUserClick(nil);
    end;
  end;

procedure TGateClientForm.btnAddUserClick(Sender: TObject);
  var
    aUser:TGateUID;
    p,l,cnt:integer;
    fs,ts:String;
  begin
  try
    if FUserListNew and GCM.Client.Ready then
      begin
      FUserListNew:=False;
      cnt:=0;
      fs:=FUserList+' ';
      ts:='';
      p:=1;
      l:=length(fs);
      GCM.Client.RemoveGroup(GCM.Client.MyUID, 1);
      while p<=l do
        begin
        if fs[p] in ['0'..'9'] then
          ts:=ts+fs[p]
        else if ts<>'' then
          begin
          aUser:=Str2LWord(ts);
          GCM.Client.AddUserToGroup(GCM.Client.MyUID, 1, aUser);
          Inc(cnt);
          ts:='';
          end;
        Inc(p);
        end;
      eToID.Caption:=IntToStr(cnt);
      end;
  except
    on E:Exception do
      Log('btnAddUserClick - '+E.ClassName+':'+E.Message,'ERROR');
    end;
  end;

function KSeparate(const s:String):String;
  var
    i,len:integer;
  begin
  Result:='';
  i:=0;len:=length(s);
  while i<len do
    begin
    Result:=s[len-i]+Result;
    Inc(i);
    if (i mod 3=0) and (i<len) then Result:='.'+Result;
    end;
  end;

procedure TGateClientForm.StatusUpdateTimer(Sender: TObject);
  begin
  case GateCli.InputState of
    ins_Connecting: shInput.Brush.Color:=clYellow;
    ins_Closed:     shInput.Brush.Color:=clRed;
    ins_Prepare:    shInput.Brush.Color:=clBlue;
    ins_Start:      shInput.Brush.Color:=clGreen;
    ins_Recv:       shInput.Brush.Color:=clLime;
    ins_Idle:       shInput.Brush.Color:=clGreen;
    ins_Done:       shInput.Brush.Color:=clNavy;
    end;
  if GateCli.InputState=ins_Closed then
    shInput.Pen.Color:=shInput.Brush.Color
  else
    case GateCli.PingInCnt of
      0:shInput.Pen.Color:=clWhite;
      1:shInput.Pen.Color:=clGreen;
      2:shInput.Pen.Color:=clLime;
      3:shInput.Pen.Color:=clBlack;
      end;

  case GateCli.OutputState of
    outs_Connecting:  shOutput.Brush.Color:=clYellow;
    outs_Closed:      shOutput.Brush.Color:=clRed;
    outs_Prepare:     shOutput.Brush.Color:=clBlue;
    outs_Start:       shOutput.Brush.Color:=clGreen;
    outs_Send:        shOutput.Brush.Color:=clLime;
    outs_Idle:        shOutput.Brush.Color:=clGreen;
    outs_Done:        shOutput.Brush.Color:=clNavy;
    end;
  if GateCli.OutputState=outs_Closed then
    shOutput.Pen.Color:=shOutput.Brush.Color
  else
    case GateCli.PingOutCnt of
      0:shOutput.Pen.Color:=clWhite;
      1:shOutput.Pen.Color:=clGreen;
      2:shOutput.Pen.Color:=clLime;
      3:shOutput.Pen.Color:=clBlack;
      end;
  lblSendBuffSize.Caption:=KSeparate(Int2Str(GateCli.TotalSent div 1024))+'K';
  lblRecvBufferSize.Caption:=KSeparate(Int2Str(GateCli.TotalReceived div 1024))+'K';

  FCS.Acquire;
  try
    l_Status1.Caption:=sStatus1;
    l_Status2.Caption:=sStatus2;
    l_Status3.Caption:=sStatus3;
    l_Groups.Caption:=sGroups;
    btnSendFile.Caption:=sSend;
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.BeforeLogIN(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(BeforeLogIN);
      Exit;
      end;

  FLoginStart:=GetAppRunTime;

  { You can change all component parameters here,
    including GateAddr and GatePort properties. }
  // GateCli.GateAddr:=MyGateAddr;
  // GateCli.GatePort:=MyGatePort;

  { To test all 4 supported APIs,
    set UseBlocking, UseProxy and UseWinHTTP
    based on the "xUseAPI" variable: }
  GateCli.UseBlocking:=xUseAPI=2;
  GateCli.UseProxy:=xUseAPI=0;
  GateCli.UseWinHTTP:=xUseAPI=1;

  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';
  FSendingFile:=False;

  btnLogIN.Caption:='...';
  shInput.Brush.Color:=clYellow;
  shInput.Pen.Color:=clWhite;
  shOutput.Brush.Color:=clYellow;
  shOutput.Pen.Color:=clWhite;
  PrintMsg('Logging in ...',msg_Status);

  FCS.Acquire;
  try
    sSend:='SEND';
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  StatusUpdate.Enabled:=True;
  end;

procedure TGateClientForm.AfterLoginFail(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(AfterLoginFail);
      Exit;
      end;

  if btnLogIN.Caption<>'IN' then
    btnLogIN.Caption:='IN';

  StatusUpdate.Enabled:=False;
  StatusUpdateTimer(nil);

  PrintMsg('Login attempt FAILED.',msg_Status);
  if GateCli.LastError<>'' then
    PrintMsg(GateCli.LastError, msg_Error);

  FCS.Acquire;
  try
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  btnClose.Visible:=True;
  btnReset.Visible:=False;

  btnCLR.Color:=clRed;
  btnCLR.Font.Color:=clYellow;
  end;

procedure TGateClientForm.AfterLogIN(Sender:TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(AfterLogIN);
      Exit;
      end;

  PrintMsg('Logged IN ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Status);

  eYourID.Caption:=LWord2Str(GateCli.MyUID);

  FUserList:=eYourID.Caption;
  FUserListNew:=True;
  
  btnLogIN.Caption:='OUT';

  btnClose.Visible:=False;
  btnReset.Visible:=True;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.AfterLogOUT(Sender:TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(AfterLogOUT);
      Exit;
      end;

  PrintMsg('Logged OUT.',msg_Status);
  if GateCli.LastError<>'' then
    PrintMsg(GateCli.LastError,msg_Error);

  if btnLogIN.Caption<>'IN' then
    btnLogIN.Caption:='IN';

  StatusUpdate.Enabled:=False;

  FCS.Acquire;
  try
    if FSendingFile then
      begin
      FSendingFile:=False;
      sSend:='SEND';
      end;
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  InGroupCnt:=0;
  OutGroupCnt:=0;
  InGroupMe:=False;
  OutGroupMe:=False;

  if btnCLR.Caption<>'CLR' then
    begin
    btnCLR.Color:=clRed;
    btnCLR.Font.Color:=clYellow;
    end;

  btnClose.Visible:=True;
  btnReset.Visible:=False;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.StreamReset(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(StreamReset);
      Exit;
      end;

  if FUserList<>'' then
    FUserListNew:=True;
  FLoginStart:=GetAppRunTime;

  Inc(CntReset);
  btnCLR.Color:=clYellow;
  btnCLR.Font.Color:=clRed;
  btnCLR.Caption:=IntToStr(CntReset);

  if GateCli.Active then
    PrintMsg('#LOST ('+FloatToStr(GateCli.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(GateCli.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status)
  else
    PrintMsg('#FAIL ('+FloatToStr(GateCli.InputResetTime/RUN_TIMER_PRECISION)+'s / '+FloatToStr(GateCli.OutputResetTime/RUN_TIMER_PRECISION)+'s)',msg_Status);
  if GateCli.LastError<>'' then
    PrintMsg(GateCli.LastError, msg_Error);
  FCS.Acquire;
  try
    if FSendingFile then
      begin
      FSendingFile:=False; // Stop sending file
      sSend:='#'+IntToStr((FSendFileLoc*100) div FSendFileSize)+'%';
      end;
    InGroupMe:=False; OutGroupMe:=False;
    InGroupCnt:=0; OutGroupCnt:=0;
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.DataReceived(Sender: TRtcConnection);
  begin
  if GCM.Data.CallID=cid_TestFileSend then
    begin
    // Only Handle messages with our CallID

    if GCM.Data.Header then // Start Buffering content after we receive the header
      GCM.Data.ToBuffer:=True;

    if GCM.Data.Footer then // Whent he footer is here, we have the complete package
      PrintMsg('<'+IntToStr(Length(GCM.Data.Content) div 1024)+'K id'+IntToStr(GCM.Data.UserID), msg_Input);
    end;
  end;

procedure TGateClientForm.InfoReceived(Sender: TRtcConnection);
  var
    s:String;
  begin
  // Handle info about other Users (Clients) joining and leaving our Groups
  // and about us leaving and joining Groups managed by other Users (Clients)

  // We are in a background thread here,
  // so do NOT access any GUI elements, unless you use the Sync() method.
  // Check other events for an example on using the Sync() method.

  case GCM.Data.Command of
    gc_UserOnline:  PrintMsg(IntToStr(GCM.Data.UserID)+' ON',msg_Group);
    gc_UserOffline: PrintMsg(IntToStr(GCM.Data.UserID)+' OFF',msg_Group);
    gc_UserJoined:  begin
                    if GCM.Data.UserID=GCM.Client.MyUID then
                      OutGroupMe:=True
                    else
                      Inc(OutGroupCnt);
                    PrintMsg('OUT +'+IntToStr(GCM.Data.UserID)+'/'+INtToStr(GCM.Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                    end;
    gc_UserLeft:    begin
                    if GCM.Data.UserID=GCM.Client.MyUID then
                      OutGroupMe:=False
                    else if OutGroupCnt>0 then
                      Dec(OutGroupCnt);
                    PrintMsg('OUT -'+IntToStr(GCM.Data.UserID)+'/'+INtToStr(GCM.Data.GroupID)+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                    end;
    gc_JoinedUser:  begin
                    if GCM.Data.UserID=GCM.Client.MyUID then
                      InGroupMe:=True
                    else
                      Inc(InGroupCnt);
                    PrintMsg('IN +'+INtToStr(GCM.Data.GroupID)+'/'+IntToStr(GCM.Data.UserID)+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    end;
    gc_LeftUser:    begin
                    if GCM.Data.UserID=GCM.Client.MyUID then
                      InGroupMe:=False
                    else if InGroupCnt>0 then
                      Dec(InGroupCnt);
                    PrintMsg('IN -'+IntToStr(GCM.Data.GroupID)+'/'+INtToStr(GCM.Data.UserID)+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    end;
    gc_Error:       PrintMsg('ERR #'+IntToStr(GCM.Data.ErrCode)+' from User '+IntToStr(GCM.Data.UserID),msg_Group);
    end;
  s:='';
  if InGroupMe then s:=s+'+';
  s:=s+IntToStr(InGroupCnt)+'/'+IntToStr(OutGroupCnt);
  if OutGroupMe then s:=s+'+';
  FCS.Acquire;
  try
    sGroups:=s;
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.ReadyToSend(Sender: TRtcConnection);
  procedure DoSendFile;
    var
      sendNow,sendTime:int64;
      maxSize:integer;
    begin
    if FSendFileLoc<FSendFileSize then
      begin
      sendNow:=FSendFileSize-FSendFileLoc;
      maxSize:=random(64*1024)+32*1024;
      if sendNow>maxSize then
        sendNow:=maxSize;

      PrintMsg('>'+IntToStr(sendNow div 1024)+'K',msg_Output);

      FSendFileLastTime:=GetAppRunTime;
      FSendFileLastSize:=sendNow;

      GCM.Client.SendBytes(GCM.Client.MyUID,1,cid_TestFileSend,Read_FileEx(FSendFileName,FSendFileLoc,sendNow));
      FCS.Acquire;
      try
        sSend:=IntToStr((FSendFileLoc*100) div FSendFileSize)+' %';
      finally
        FCS.Release;
        end;
      FSendFileLoc:=FSendFileLoc+sendNow;
      end
    else
      begin
      FSendingFile:=False;
      FCS.Acquire;
      try
        sSend:='DONE';
      finally
        FCS.Release;
        end;
      sendTime:=GetAppRunTime-FSendFileStart;
      if sendTime>0 then
        begin
        PrintMsg('>>'+IntToStr(FSendFileLoc div 1024)+'K in '+FloatToStr(sendTime/RUN_TIMER_PRECISION)+' s',msg_Output);
        PrintMsg(FloatToStr(round(FSendFileLoc/(sendTime/RUN_TIMER_PRECISION)/(10.24/8))/100)+' Kbit',msg_Speed);
        end
      else
        PrintMsg('>>'+IntToStr(FSendFileLoc div 1024)+'K',msg_Output);

      if xRepeatSend then
        btnSendFileClick(nil);
      end;
    end;
  begin
  // Client is ready to send data to the Gateway
  // We are in a backgrdound thread, so NO GUI ACCESS here,
  // unless we use the Sync() method to synchronize the event with the Main thread.
  // Check other events for an example on using the Sync() method.
  if FUserListNew then
    begin
    if Sender=nil then
      PrintMsg('NIL! ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output)
    else
      begin
      PrintMsg('Prepare ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
      btnAddUserClick(nil);
      end;
    end
  else if FSendingFile then
    DoSendFile
  else if xRepeatSend then
    btnSendFileClick(nil)
  else
    begin
    if Sender=nil then
      PrintMsg('PING ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Input)
    else
      begin
      PrintMsg('Ready ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
      FLoginStart:=GetAppRunTime;
      end;
    end;
  end;

procedure TGateClientForm.PrintMsg(const s: String; t:TMsgType);
  begin
  FCS.Acquire;
  try
    case t of
      msg_Input:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Output:
        sStatus2:=Time2Str(Now)+' '+s;
      msg_Group:
        sStatus1:=Time2Str(Now)+' '+s;
      msg_Speed:
        sStatus3:=s;
      msg_Status:
        begin
        sStatus1:=Time2Str(Now)+' '+s;
        sStatus2:='';
        sStatus3:='';
        end;
      msg_Error:
        sStatus2:=Time2Str(Now)+' '+s;
      end;
  finally
    FCS.Release;
    end;

  case t of
    msg_Input:
      Log(s,IntToStr(GCM.Client.MyUID)+'_DATA');
    msg_Output:
      Log(s,IntToStr(GCM.Client.MyUID)+'_DATA');
    msg_Group:
      Log(s,IntToStr(GCM.Client.MyUID)+'_DATA');
    msg_Speed:
      Log(s,IntToStr(GCM.Client.MyUID)+'_CONN');
    msg_Status:
      if GCM.Client.MyUID>0 then
        Log(s,IntToStr(GCM.Client.MyUID)+'_CONN');
    msg_Error:
      if GCM.Client.MyUID>0 then
        Log(s,IntToStr(GCM.Client.MyUID)+'_CONN');
    end;
  end;

var
  LMouseX,LMouseY:integer;
  LMouseD:boolean=False;

procedure TGateClientForm.InfoPanelMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbRight then
    begin
    if MainPanel.Height<>InfoPanel.Top then
      MainPanel.Height:=InfoPanel.Top
    else
      MainPanel.Height:=InfoPanel.Top+InfoPanel.Height;
    end
  else if Button=mbLeft then
    begin
    LMouseD:=True;
    LMouseX:=-TControl(Sender).Left; //X;
    LMouseY:=-TCOntrol(Sender).Top; // Y;
    InfoPanelMouseMove(Sender,Shift,X,Y);
    end;
  end;

procedure TGateClientForm.InfoPanelMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
  var
    nX,nY,loc:integer;
  begin
  if LMouseD then
    begin
    if (Left+X)>=0 then
      nX:=((Left+X-LMouseX) div ClientWidth) * ClientWidth
    else
      nX:=((Left+X-LMouseX-ClientWidth) div ClientWidth) * ClientWidth;
    if (Top+Y)>=0 then
      nY:=((Top+Y-LMouseY) div ClientHeight) * ClientHeight
    else
      nY:=((Top+Y-LMouseY-ClientHeight) div ClientHeight) * ClientHeight;
    if (nX<>Left) or (nY<>Top) then
      begin
      SetBounds(nX,nY,Width,Height);
      // Update the "xUseAPI" variable based on our new Form position
      // to minimize setup steps required for a test of all APIs.
      if nX<0 then
        loc:=(-nX div ClientWidth) mod 4
      else
        loc:=(nX div ClientWidth) mod 4;
      xUseAPI:=loc;
      end;
    end;
  end;

procedure TGateClientForm.InfoPanelMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbLeft then
    LMouseD:=False;
  end;

procedure TGateClientForm.btnCloseClick(Sender: TObject);
  begin
  AutoStart:=False;
  StartAnother.Enabled:=False;

  if GateCli.AutoLogin then
    begin
    Log('Clicked CLOSE (Active)',IntToStr(GateCli.MyUID));
    GateCli.AutoLogin:=False;
    try
      if Copy(Clipboard.AsText,1,1)=',' then
        Clipboard.AsText:='';
    except
      end;
    end
  else
    begin
    Log('Clicked CLOSE (Inactive)',IntToStr(GateCli.MyUID));
    {$IFDEF USE_UDP}
    if Sender=btnClose then
      udpClient.Write('close');
    {$ENDIF}
    Close;
    end;
  end;

procedure TGateClientForm.eToIDExit(Sender: TObject);
  begin
  // btnAddUserClick(nil);
  end;

procedure TGateClientForm.btnResetClick(Sender: TObject);
  begin
  if GateCli.Active then
    GateCli.ResetStreams;
  end;

procedure TGateClientForm.eYourIDClick(Sender: TObject);
  var
    s:String;
  begin
  try
    s:=Clipboard.AsText;
    if s<>'' then
      if Copy(s,1,1)<>',' then
        s:='';
    Clipboard.AsText:=s+','+eYourID.Caption;
  except
    on E:Exception do
      Log('eYourIDClick - '+E.ClassName+':'+E.Message,'ERROR');
    end;
  end;

procedure TGateClientForm.eToIDClick(Sender: TObject);
  begin
  try
    FUserList:=Clipboard.AsText;
    if FUserList<>'' then
      begin
      FUserListNew:=True;
      btnAddUserClick(nil);
      end;
  except
    on E:Exception do
      Log('eToIDClick - '+E.ClassName+':'+E.Message,'ERROR');
    end;
  end;

procedure TGateClientForm.StartAnotherTimer(Sender: TObject);
  begin
  StartAnother.Enabled:=False;
  StartNext;
  end;

procedure TGateClientForm.udpServerDataReceived(Sender: TRtcConnection);
{$IFDEF USE_UDP}
  var
    msg:String;
  begin
  msg:=Sender.Read;
  if (Sender.PeerPort=udpClient.LocalPort) then
    Exit; // ignore our messages

  if msg='start' then
    begin
    if not GateCli.AutoLogin then
      begin
      Log('Received START',IntToStr(GateCli.MyUID));
      btnLogINClick(nil);
      end;
    end
  else if msg='stop' then
    begin
    if GateCli.AutoLogin then
      begin
      Log('Received STOP',IntToStr(GateCli.MyUID));
      btnLogINClick(nil);
      end;
    end
  else if msg='send' then
    begin
    if not FSendingFile then
      begin
      Log('Received SEND',IntToStr(GateCli.MyUID));
      btnSendFileClick(nil);
      end;
    end
  else if msg='nosend' then
    begin
    if FSendingFile then
      begin
      Log('Received NOSEND',IntToStr(GateCli.MyUID));
      btnSendFileClick(Sender);
      end;
    end
  else if msg='close' then
    begin
    Log('Received CLOSE',IntToStr(GateCli.MyUID));
    Close;
    end;
  end;
{$ELSE}
  begin
  end;
{$ENDIF}

end.

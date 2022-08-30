unit MainGateClientForm;

interface

{$include rtcDeploy.inc}

{ This "ChatClient" example will work only with the "ChatServer" example Project.

  This is accomplished by doing two things:

  1. By using a special GatePrimaryKey value, which is set on the
     Gate:TRtcGateway component in this Project (Server) and on the
     GateCli:TRtcHttpGateClient component in the ChatClient Project.
     If the GatePrimaryKey values do not exactly match, Client will
     be unable to communicate with the Gateway (encryption errors).

  2. You can use the GateUserAuth property to send any information
     from the User that is required to authenticate the User,
     like a Username and a Password for example. To keep this example
     simple, all our Clients only need "MyChatClient" as GateUserAuth.
     We have implemented the GateBeforeLogin event to raise an exception
     for any Client which tries to Login without the excepted GateUserAuth value.

  For ONLINE/OFFLINE notifications about other CHAT users,
  the ChatServer will be using our GroupID 254.

  This Chat implementation allows free creation of Rooms.
  Any user inside a Chat Room can invite any other user by using
  the "INVITE" button and entering the ID of that user.

  Check the "GateAddr" and "GatePort" properties on the "GateCli:TRtcHttpGateClient"
  component to configure this "ChatClient" to work with a "ChatServer" running on a remote machine.
  Default setting are GateAddr="localhost" and GatePort="80" - used for local testing.

  Please note that ChatClients will NOT see their own ID in the "ONLINE" List after Login.
  You need at least 2 ChatClients running to see how ONLINE/OFFLINE notification works. }

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  ExtCtrls, Buttons,

  rtcInfo, rtcLog, rtcConn, rtcSyncObjs,

  rtcGateConst,
  rtcGateCli,

  memStrList,

  ChatHostForm,
  GateCIDs;

type
  TMsgType=(msg_Input,msg_Output,msg_Speed,msg_Error,msg_Status,msg_Group);

type
  TGateClientForm = class(TForm)
    MainPanel: TPanel;
    StatusUpdate: TTimer;
    InfoPanel: TPanel;
    l_Status1: TLabel;
    l_Status2: TLabel;
    Panel1: TPanel;
    shInput: TShape;
    lblRecvBufferSize: TLabel;
    lblSendBuffSize: TLabel;
    shOutput: TShape;
    eYourID: TEdit;
    GateCli: TRtcHttpGateClient;
    Label1: TLabel;
    eMyGroup: TListBox;
    Label2: TLabel;
    Label3: TLabel;
    eInGroup: TListBox;
    btnCLR: TLabel;
    l_Groups: TLabel;
    l_Status3: TLabel;
    Bevel1: TBevel;
    btnHostChat: TButton;
    Bevel2: TBevel;
    ChatLink: TRtcGateClientLink;
    eChatUsers: TListBox;
    btnReset: TSpeedButton;
    Label7: TLabel;
    lblInvites: TLabel;
    lblGroups: TLabel;
    Label4: TLabel;
    eOnlineUsers: TListBox;
    lblOnline: TLabel;
    Bevel3: TBevel;
    procedure btnCLRClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure StatusUpdateTimer(Sender: TObject);
    procedure FormDestroy(Sender: TObject);

    procedure ClientBeforeLogIN(Sender:TRtcConnection);
    procedure ClientAfterLogIN(Sender:TRtcConnection);
    procedure ClientAfterLogOUT(Sender:TRtcConnection);
    procedure ClientAfterLoginFail(Sender:TRtcConnection);

    procedure ClientDataReceived(Sender:TRtcConnection);
    procedure ClientInfoReceived(Sender:TRtcConnection);
    procedure ClientReadyToSend(Sender:TRtcConnection);

    procedure ClientStreamReset(Sender:TRtcConnection);
    procedure eMyGroupDblClick(Sender: TObject);
    procedure eInGroupDblClick(Sender: TObject);
    procedure btnHostChatClick(Sender: TObject);
    procedure ChatLinkDataReceived(Sender: TRtcConnection);
    procedure eChatUsersDblClick(Sender: TObject);
    procedure btnResetClick(Sender: TObject);
    procedure eOnlineUsersDblClick(Sender: TObject);

  public
    FCS:TRtcCritSec;
    sStatus1,
    sStatus2,
    sStatus3,
    sGroups:String;

    FLoginStart:Cardinal;
    CntReset:integer;

    InGroupCnt,OutGroupCnt:integer;
    InGroupMe,OutGroupMe:boolean;

    FChatUsers,
    FScreenUsers,
    FFileUsers:TStrList;

    NeedProviderChange:boolean;

    procedure PrintMsg(const s:String; t:TMsgType);
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

procedure TGateClientForm.FormCreate(Sender: TObject);
  begin
  StartLog;
  SetupConnectionSpeed(64,64);

  NeedProviderChange:=False;

  FCS:=TRtcCritSec.Create;
  sStatus1:='';
  sStatus2:='';
  sStatus3:='';
  sGroups:='';

  FChatUsers:=tStrList.Create(16);
  FScreenUsers:=tStrList.Create(16);
  FFileUsers:=tStrList.Create(16);

  GateCli.AutoLogin:=True;
  end;

procedure TGateClientForm.FormDestroy(Sender: TObject);
  begin
  FreeAndNil(FChatUsers);
  FreeAndNil(FScreenUsers);
  FreeAndNil(FFileUsers);

  FreeAndNil(FCS);
  end;

procedure TGateClientForm.FormCloseQuery(Sender: TObject; var CanClose: Boolean);
  begin
  GateCli.AutoLogin:=False;
  CanClose:=True;
  end;

procedure TGateClientForm.btnCLRClick(Sender: TObject);
  begin
  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';
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
  finally
    FCS.Release;
    end;
  end;

procedure TGateClientForm.ClientBeforeLogIN(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(ClientBeforeLogIN);
      Exit;
      end;

  FLoginStart:=GetAppRunTime;

  CntReset:=0;
  btnCLR.Color:=clWhite;
  btnCLR.Font.Color:=clNavy;
  btnCLR.Caption:='CLR';

  shInput.Brush.Color:=clYellow;
  shInput.Pen.Color:=clWhite;
  shOutput.Brush.Color:=clYellow;
  shOutput.Pen.Color:=clWhite;
  PrintMsg('Logging in ...',msg_Status);

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

  eMyGroup.Clear;
  eInGroup.Clear;
  eOnlineUsers.Clear;
  eChatUsers.Clear;

  lblGroups.Visible:=False;
  lblInvites.Visible:=False;
  lblOnline.Visible:=False;

  StatusUpdate.Enabled:=True;
  end;

procedure TGateClientForm.ClientAfterLoginFail(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(ClientAfterLoginFail);
      Exit;
      end;

  if GateCli.UseWinHTTP then // WinHTTP -> async WinSock
    begin
    btnReset.Caption:='AS';
    GateCli.UseBlocking:=False;
    GateCli.UseProxy:=False;
    GateCli.UseWinHTTP:=False;
    end
  else if GateCli.UseProxy then // WinInet -> WinHTTP
    begin
    btnReset.Caption:='HT';
    GateCli.UseWinHTTP:=True;
    end
  else if GateCli.UseBlocking then // blocking WinSock -> WinInet
    begin
    btnReset.Caption:='IE';
    GateCli.UseProxy:=True;
    end
  else // async WinSock -> blocking WinSock
    begin
    btnReset.Caption:='BS';
    GateCli.UseBlocking:=True;
    end;

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

  eMyGroup.Clear;
  eInGroup.Clear;
  eOnlineUsers.Clear;
  eChatUsers.Clear;

  lblGroups.Visible:=False;
  lblInvites.Visible:=False;
  lblOnline.Visible:=False;

  btnCLR.Color:=clRed;
  btnCLR.Font.Color:=clYellow;
  end;

procedure TGateClientForm.ClientAfterLogIN(Sender:TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(ClientAfterLogIN);
      Exit;
      end;

  PrintMsg('Logged IN ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Status);

  eYourID.Text:=LWord2Str(GateCli.MyUID);

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.ClientAfterLogOUT(Sender:TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(ClientAfterLogOUT);
      Exit;
      end;

  PrintMsg('Logged OUT.',msg_Status);
  if GateCli.LastError<>'' then
    PrintMsg(GateCli.LastError,msg_Error);

  StatusUpdate.Enabled:=False;

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

  eMyGroup.Clear;
  eInGroup.Clear;
  eOnlineUsers.Clear;
  eChatUsers.Clear;

  lblGroups.Visible:=False;
  lblInvites.Visible:=False;
  lblOnline.Visible:=False;

  if btnCLR.Caption<>'CLR' then
    begin
    btnCLR.Color:=clRed;
    btnCLR.Font.Color:=clYellow;
    end;

  StatusUpdateTimer(nil);
  end;

procedure TGateClientForm.ClientStreamReset(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(ClientStreamReset);
      Exit;
      end;

  if NeedProviderChange then
    begin
    NeedProviderChange:=False;
    if GateCli.UseWinHTTP then // WinHTTP -> async WinSock
      begin
      btnReset.Caption:='AS';
      GateCli.UseBlocking:=False;
      GateCli.UseProxy:=False;
      GateCli.UseWinHTTP:=False;
      end
    else if GateCli.UseProxy then // WinInet -> WinHTTP
      begin
      btnReset.Caption:='HT';
      GateCli.UseWinHTTP:=True;
      end
    else if GateCli.UseBlocking then // blocking WinSock -> WinInet
      begin
      btnReset.Caption:='IE';
      GateCli.UseProxy:=True;
      end
    else // async WinSock -> blocking WinSock
      begin
      btnReset.Caption:='BS';
      GateCli.UseBlocking:=True;
      end;
    end;

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
    InGroupMe:=False; OutGroupMe:=False;
    InGroupCnt:=0; OutGroupCnt:=0;
    sGroups:='0/0';
  finally
    FCS.Release;
    end;
  eMyGroup.Clear;
  eInGroup.Clear;
  eOnlineUsers.Clear;
  eChatUsers.Clear;

  lblGroups.Visible:=False;
  lblInvites.Visible:=False;
  lblOnline.Visible:=False;
  end;

procedure TGateClientForm.ClientDataReceived(Sender: TRtcConnection);
  begin
  if GateCli.Data.Footer or not GateCli.Data.ToBuffer then
    PrintMsg('<'+IntToStr(Length(GateCli.Data.Content) div 1024)+'K id '+IntToStr(GateCli.Data.UserID), msg_Input)
  end;

procedure TGateClientForm.ClientInfoReceived(Sender: TRtcConnection);
  var
    s:String;
    i:integer;
  begin
  case GateCli.Data.Command of
    gc_UserOnline:  PrintMsg(IntToStr(GateCli.Data.UserID)+' ON-Line',msg_Group);
    gc_UserOffline: PrintMsg(IntToStr(GateCli.Data.UserID)+' OFF-Line',msg_Group);
    gc_UserJoined:  begin
                    if GateCli.Data.GroupID=254 then
                      begin
                      S:=IntToStr(GateCli.Data.UserID);
                      PrintMsg('CHAT +'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      if eOnlineUsers.Items.IndexOf(S)<0 then
                        eOnlineUsers.Items.Add(S);
                      lblOnline.Visible:=eOnlineUsers.Count>0;
                      end
                    else
                      begin
                      if GateCli.Data.UserID=GateCli.MyUID then
                        OutGroupMe:=True
                      else
                        Inc(OutGroupCnt);
                      S:=IntToStr(GateCli.Data.UserID)+'/'+INtToStr(GateCli.Data.GroupID);
                      PrintMsg('OUT +'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      if eMyGroup.Items.IndexOf(S)<0 then
                        eMyGroup.Items.Add(S);
                      lblGroups.Visible:=eMyGroup.Count + eInGroup.Count > 0;
                      end;
                    end;
    gc_UserLeft:    begin
                    if GateCli.Data.GroupID=254 then
                      begin
                      S:=IntToStr(GateCli.Data.UserID);
                      PrintMsg('CHAT -'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      i:=eOnlineUsers.Items.IndexOf(S);
                      if i>=0 then eOnlineUsers.Items.Delete(i);
                      lblOnline.Visible:=eOnlineUsers.Count>0;
                      end
                    else
                      begin
                      if GateCli.Data.UserID=GateCli.MyUID then
                        OutGroupMe:=False
                      else if OutGroupCnt>0 then
                        Dec(OutGroupCnt);
                      S:=IntToStr(GateCli.Data.UserID)+'/'+IntToStr(GateCli.Data.GroupID);
                      PrintMsg('OUT -'+S+' ('+IntToStr(OutGroupCnt)+')',msg_Group);
                      i:=eMyGroup.Items.IndexOf(S);
                      if i>=0 then eMyGroup.Items.Delete(i);
                      lblGroups.Visible:=eMyGroup.Count + eInGroup.Count > 0;
                      end;
                    end;
    gc_JoinedUser:  begin
                    if GateCli.Data.UserID=GateCli.MyUID then
                      InGroupMe:=True
                    else
                      Inc(InGroupCnt);
                    S:=IntToStr(GateCli.Data.UserID)+'/'+IntToStr(GateCli.Data.GroupID);
                    PrintMsg('IN +'+S+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    if eInGroup.Items.IndexOf(S)<0 then
                      eInGroup.Items.Add(S);
                    end;
    gc_LeftUser:    begin
                    if GateCli.Data.UserID=GateCli.MyUID then
                      InGroupMe:=False
                    else if InGroupCnt>0 then
                      Dec(InGroupCnt);
                    S:=IntToStr(GateCli.Data.UserID)+'/'+IntToStr(GateCli.Data.GroupID);
                    PrintMsg('IN -'+S+' ('+IntToStr(InGroupCnt)+')',msg_Group);
                    i:=eInGroup.Items.IndexOf(S);
                    if i>=0 then eInGroup.Items.Delete(i);
                    end;
    gc_Error:       PrintMsg('ERR #'+IntToStr(GateCli.Data.ErrCode)+' from User '+IntToStr(GateCli.Data.UserID),msg_Group);
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

procedure TGateClientForm.ClientReadyToSend(Sender: TRtcConnection);
  begin
  if GateCli.Ready then
    if Sender=nil then
      PrintMsg('PING ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Input)
    else
      begin
      PrintMsg('Ready ('+FloatToStr((GetAppRunTime-FLoginStart)/RUN_TIMER_PRECISION)+' s).',msg_Output);
      FLoginStart:=GetAppRunTime;
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
      Log(s,IntToStr(GateCli.MyUID)+'_DATA');
    msg_Output:
      Log(s,IntToStr(GateCli.MyUID)+'_DATA');
    msg_Group:
      Log(s,IntToStr(GateCli.MyUID)+'_DATA');
    msg_Speed:
      Log(s,IntToStr(GateCli.MyUID)+'_CONN');
    msg_Status:
      if GateCli.MyUID>0 then
        Log(s,IntToStr(GateCli.MyUID)+'_CONN');
    msg_Error:
      if GateCli.MyUID>0 then
        Log(s,IntToStr(GateCli.MyUID)+'_CONN');
    end;
  end;

procedure TGateClientForm.eMyGroupDblClick(Sender: TObject);
  var
    UID,GID:String;
    i:integer;
  begin
  if GateCli.Active then
    begin
    if (eMyGroup.Items.Count>0) and (eMyGroup.ItemIndex>=0) then
      begin
      UID:=Trim(eMyGroup.Items.Strings[eMyGroup.ItemIndex]);
      i:=Pos('/',UID);
      GID:=Copy(UID,i+1,length(UID));
      UID:=Copy(UID,1,i-1);
      if MessageDlg('Remove User '+UID+' from My Group '+GID+'?',mtConfirmation,[mbYes,mbNo],0)=mrYes then
        GateCli.RemUserFromGroup(GateCli.MyUID,StrToInt(GID),StrToInt(UID));
      end;
    end;
  end;

procedure TGateClientForm.eInGroupDblClick(Sender: TObject);
  var
    UID,GID:String;
    i:integer;
  begin
  if GateCli.Active then
    begin
    if (eInGroup.Items.Count>0) and (eInGroup.ItemIndex>=0) then
      begin
      UID:=Trim(eInGroup.Items.Strings[eInGroup.ItemIndex]);
      i:=Pos('/',UID);
      GID:=Copy(UID,i+1,length(UID));
      UID:=Copy(UID,1,i-1);
      if MessageDlg('Leave Group '+GID+' Hosted by User '+UID+' ?',mtConfirmation,[mbYes,mbNo],0)=mrYes then
        GateCli.RemUserFromGroup(StrToInt(UID),StrToInt(GID),GateCli.MyUID);
      end;
    end;
  end;

procedure TGateClientForm.btnHostChatClick(Sender: TObject);
  begin
  if GateCli.Ready then
    NewChatHostForm(GateCli);
  end;

procedure TGateClientForm.ChatLinkDataReceived(Sender: TRtcConnection);
  var
    UID:RtcString;
    i:integer;
  begin
  if (ChatLink.Data.CallID=cid_ChatInvite) and
     (ChatLink.Data.GroupID=0) then
    begin
    if ChatLink.Data.Footer then
      begin
      if not Sender.inMainThread then
        Sender.Sync(ChatLinkDataReceived)
      else
        begin
        UID:=Int2Str(ChatLink.Data.UserID)+'/'+Int2Str(ChatLink.Data.Content[0]);
        if FChatUsers.search(UID)='' then
          begin
          // Add UserID+GroupID to Chat Users invitation list
          eChatUsers.Items.Add(UID);
          // Store Invitation Key for Chat with UserID+GroupID
          FChatUsers.insert(UID,RtcBytesToString(ChatLink.Data.Content));

          if GetActiveWindow<>Handle then MessageBeep(0);
          lblInvites.Visible:=True;
          end;
        end;
      end
    else if ChatLink.Data.Header then
      ChatLink.Data.ToBuffer:=True;
    end
  else if (ChatLink.Data.CallID=cid_ChatLeft) and
          (ChatLink.Data.GroupID>0) then
    begin
    if ChatLink.Data.Footer then
      begin
      if not Sender.inMainThread then
        Sender.Sync(ChatLinkDataReceived)
      else
        begin
        UID:=Int2Str(ChatLink.Data.UserID)+'/'+Int2Str(ChatLink.Data.GroupID);
        if FChatUsers.search(UID)<>'' then
          begin
          i:=eChatUsers.Items.IndexOf(UID);
          eChatUsers.Items.Delete(i);
          FChatUsers.remove(UID);
          lblInvites.Visible:=eChatUsers.Count > 0;
          end;
        end;
      end
    else if ChatLink.Data.Header then
      ChatLink.Data.ToBuffer:=True;
    end;
  end;

procedure TGateClientForm.eChatUsersDblClick(Sender: TObject);
  var
    UID,Key:String;
    UserID,GroupID:TGateUID;
    i:integer;
    Frm:TChatHostFrm;
  begin
  if GateCli.Ready then
    begin
    if (eChatUsers.Items.Count>0) and (eChatUsers.ItemIndex>=0) then
      begin
      UID:=Trim(eChatUsers.Items.Strings[eChatUsers.ItemIndex]);
      eChatUsers.Items.Delete(eChatUsers.ItemIndex);

      lblInvites.Visible:=eChatUsers.Count > 0;

      Key:=FChatUsers.search(UID);
      if Key<>'' then
        begin
        FChatUsers.remove(UID);
        i:=Pos('/',UID);
        UserID:=StrToInt(Copy(UID,1,i-1));
        GroupID:=StrToInt(Copy(UID,i+1,length(UID)-i));

        // Open a new Chat Room
        Frm:=NewChatHostForm(GateCli);

        if (GateCli.MyUID<>UserID) or (Frm.MyGroupID<>GroupID) then
          begin
          Frm.UserIsPassive(UserID,0);
          // Add user to "passive" list
          Frm.Link.SetUserGroupStatus(UserID,0,1);
          // Send invitation Key back with our Key
          GateCli.SendBytes(UserID,0,cid_ChatAccept,RtcStringToBytes(Key + RtcBytesToString(Frm.InviteKey)));
          // Send PING to get notified if User is Off-Line
          GateCli.PingUser(UserID);
          end;
        end;
      end;
    end;
  end;

procedure TGateClientForm.btnResetClick(Sender: TObject);
  begin
  NeedProviderChange:=True;
  GateCli.ResetStreams;
  end;

procedure TGateClientForm.eOnlineUsersDblClick(Sender: TObject);
  var
    UID:String;
  begin
  if GateCli.Active then
    if (eOnlineUsers.Items.Count>0) and (eOnlineUsers.ItemIndex>=0) then
      begin
      UID:=Trim(eOnlineUsers.Items.Strings[eOnlineUsers.ItemIndex]);
      if GateCli.Ready then
        NewChatHostForm(GateCli).InviteUserToChat(StrToInt(UID),0,True);
      end;
  end;

end.

unit ChatHostForm;

interface

uses
  Windows, Messages, SysUtils, Variants,
  Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls,

  rtcInfo, rtcConn,
  rtcGateConst, rtcGateCli, rtcZLib,
  rtcFastStrings, rtcTypes, rtcSyncObjs,

  GateCIDs, ComCtrls, Buttons, rtcThrPool;

type
  TChatHostFrm = class(TForm)
    Link: TRtcGateClientLink;
    Panel2: TPanel;
    eUsers: TListBox;
    Panel3: TPanel;
    Splitter1: TSplitter;
    Label1: TLabel;
    btnAddUser: TButton;
    Panel1: TPanel;
    Splitter2: TSplitter;
    Panel4: TPanel;
    eMessage: TMemo;
    runPaintJob: TRtcQuickJob;
    Panel5: TPanel;
    panChat: TPanel;
    eChat: TMemo;
    panDraw: TPanel;
    spChat: TSplitter;
    pbDrawing: TPaintBox;
    Panel9: TPanel;
    pbPenColor: TColorBox;
    pbPenWidth: TTrackBar;
    Panel10: TPanel;
    btnClearDrawing: TBitBtn;
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure btnAddUserClick(Sender: TObject);
    procedure LinkDataReceived(Sender: TRtcConnection);
    procedure LinkInfoReceived(Sender: TRtcConnection);
    procedure eUsersDblClick(Sender: TObject);
    procedure LinkStreamReset(Sender: TRtcConnection);
    procedure LinkAfterLogOut(Sender: TRtcConnection);
    procedure LinkReadyToSend(Sender: TRtcConnection);
    procedure eMessageKeyPress(Sender: TObject; var Key: Char);
    procedure pbDrawingMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure pbDrawingMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure pbDrawingMouseUp(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure pbDrawingPaint(Sender: TObject);
    procedure btnClearDrawingClick(Sender: TObject);
    procedure runPaintJobExecute(Data: TRtcValue);
  private
    { Private declarations }
  public
    { Public declarations }
    MyGroupID:TGateUID;
    InviteKey:RtcByteArray;
    StreamWasReset:boolean;
    LMDown:boolean;
    LMX1,LMY1:integer;
    FCS:TRtcCritSec;

    FDrawing:TRtcDataSet;
    FLastPainted:integer;

    procedure PaintLine;
    procedure PaintNewLines;
    procedure PaintDrawing;

    procedure UserIsActive(UserID,GroupID:TGateUID);
    procedure UserIsPassive(UserID,GroupID:TGateUID);
    procedure UserIsRemoved(UserID,GroupID:TGateUID);

    function ConnectedUsers:integer;
    function ConnectedUserList:String;

    procedure AddAllDisabledUsers;

    procedure InviteUserToChat(UserID, GroupID:TGateUID; Forced:boolean=True);

    procedure ProcessAccept(Sender:TRtcConnection);
    procedure ProcessJoin(Sender:TRtcConnection);
    procedure ProcessLeft(Sender:TRtcConnection);
    procedure ProcessReturn(Sender:TRtcConnection);
    procedure ProcessMessage(Sender:TRtcConnection);
    procedure ProcessPaintLine(Sender:TRtcConnection);
    procedure ProcessPaintClear(Sender:TRtcConnection);
  end;

function NewChatHostForm(Cli:TRtcHttpGateClient):TChatHostFrm;

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

function Now2Str:RtcString;
  begin
  Result:=' @'+Time2Str(Now);
  end;

function MakeRandomKey(GID:TGateUID; len:integer):RtcByteArray;
  var
    a:integer;
    b:byte;
  begin
  SetLength(Result,len+1);
  Result[0]:=GID;
  for a:=1 to length(Result)-1 do
    begin
    b:=random(255);
    Result[a]:=b;
    end;
  end;

function CompareKeys(var OrigKey,RecvKey:RtcByteArray):boolean;
  var
    a:integer;
  begin
  if length(OrigKey)>length(RecvKey) then
    Result:=False
  else
    begin
    Result:=True;
    for a:=0 to length(OrigKey)-1 do
      if OrigKey[a]<>RecvKey[a] then
        begin
        Result:=False;
        Break;
        end;
    end;
  end;

function NewChatHostForm(Cli:TRtcHttpGateClient):TChatHostFrm;
  var
    id:TGateUID;
  begin
  if not Cli.Ready then
    raise ERtcGateClient.Create('No connection');
  id:=Cli.AllocNextFreeGroupID;

  Result:=TChatHostFrm.Create(Application);
  Result.Link.Client:=Cli;

  Result.MyGroupID:=id;
  Result.InviteKey:=MakeRandomKey(Result.MyGroupID,16);
  Result.Caption:=IntToStr(Cli.MyUID)+': Chat Room #'+IntToStr(Result.MyGroupID);
  Result.Show;
  end;

procedure TChatHostFrm.FormClose(Sender: TObject; var Action: TCloseAction);
  begin
  Action:=caFree;
  end;

procedure TChatHostFrm.FormDestroy(Sender: TObject);
  begin
  if assigned(Link.Client) then
    begin
    if Link.Client.Ready then
      begin
      AddAllDisabledUsers;
      Link.Client.SendBytes(Link.Client.MyUID,MyGroupID,cid_ChatLeft);
      Link.Client.RemoveGroup(Link.Client.MyUID,MyGroupID);
      end;
    Link.Client.ReleaseGroupID(MyGroupID);
    Link.Client:=nil;
    end;
  RtcFreeAndNil(FDrawing);
  RtcFreeAndNil(FCS);
  end;

procedure TChatHostFrm.btnAddUserClick(Sender: TObject);
  var
    UID:String;
  begin
  if not Link.Client.Ready then Exit;
  UID:='';
  if InputQuery('Invite User to Chat','Enter remote User ID',UID) then
    InviteUserToChat(Str2IntDef(Trim(UID),0),0);
  eMessage.SetFocus;
  end;

procedure TChatHostFrm.InviteUserToChat(UserID, GroupID: TGateUID; Forced:boolean=True);
  var
    Frm:TChatHostFrm;
  begin
  if (UserID<MinUserID) or
     (UserID>MaxUserID) or
     (UserID=Link.Client.MyUID) then
    begin
    if Forced then
      ShowMessage('Invalid User ID.');
    end
  else
    begin
    if GroupID=0 then
      GroupID:=Link.GetMinUserGroupID(UserID);
    if GroupID>0 then
      begin
      if Link.GetUserGroupStatus(UserID,GroupID)=10 then
        begin
        if Forced then
          begin
          Frm:=NewChatHostForm(Link.Client);
          Frm.InviteUserToChat(UserID,0);
          end;
        end
      else
        begin
        Link.Client.SendBytes(UserID,0,cid_ChatReturn,InviteKey);
        if Forced then Link.Client.PingUser(UserID);
        end;
      end
    else
      begin
      if Forced then
        UserIsPassive(UserID,0);
      Link.SetUserGroupStatus(UserID,0,1);
      Link.Client.SendBytes(UserID,0,cid_ChatInvite,InviteKey);
      Link.Client.PingUser(UserID);
      end;
    end;
  end;

procedure TChatHostFrm.ProcessAccept(Sender: TRtcConnection);
  var
    UserKey:RtcByteArray;
    UserGroupID:TGateUID;
    OutBytes:TRtcHugeByteArray;
  begin
  UserKey:=nil;

  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if (Link.Data.GroupID=0) and
       (Link.Data.Content[0]=MyGroupID) then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if (Link.Data.GroupID=0) and
       (Link.Data.Content[0]=MyGroupID) then
      begin
      // Invitation Key is correct?
      if CompareKeys(InviteKey, Link.Data.Content) then
        begin
        if assigned(Sender) then
          if not Sender.inMainThread then
            begin
            Sender.Sync(ProcessAccept);
            Exit;
            end;

        UserKey:=Copy(Link.Data.Content,length(InviteKey),length(Link.Data.Content)-length(InviteKey));

        if length(UserKey)=0 then Exit;
        UserGroupID:=UserKey[0];

        // Do NOT add Self!
        if (Link.Data.UserID=Link.Client.MyUID) then Exit; // and (UserGroupID=MyGroupID) then Exit;

        // Do NOT add the same link twice!
        if Link.GetUserGroupStatus(Link.Data.UserID, UserGroupID)=10 then Exit;

        if Link.GetUserGroupStatus(Link.Data.UserID, UserGroupID)=0 then
          begin
          UserIsPassive(Link.Data.UserID, UserGroupID);
          Link.ClearUserGroupStatus(Link.Data.UserID, 0);
          end;
        Link.SetUserGroupStatus(Link.Data.UserID, UserGroupID, 10);

        if Link.Data.CallID=cid_ChatAccept then
          begin
          OutBytes:=TRtcHugeByteArray.Create;
          try
            OutBytes.AddEx(UserKey);
            OutBytes.AddEx(InviteKey);
            Link.Client.SendBytes(Link.Data.UserID, 0, cid_ChatConfirm, OutBytes.GetEx);

            OutBytes.Clear;
            OutBytes.AddEx(Int2Bytes(Link.Data.UserID));
            OutBytes.AddEx(UserKey);
            Link.Client.SendBytes(Link.Client.MyUID, MyGroupID, cid_ChatJoined, OutBytes.GetEx);
          finally
            OutBytes.Free;
            end;
          end;
        Link.Client.AddUserToGroup(Link.Client.MyUID,MyGroupID,Link.Data.UserID);

        // User has already invited us to join his Group?
        if Link.Client.GetUserGroupStatus(Link.Data.UserID, UserGroupID)>0 then
          begin
          UserIsActive(Link.Data.UserID, UserGroupID);
          eChat.Lines.Add('* USER '+IntToStr(Link.Data.UserID)+'/'+IntToStr(UserGroupID)+' JOINED'+Now2Str);
          if GetActiveWindow<>Handle then MessageBeep(0);
          end;
        end;
      end;
    end;
  end;

procedure TChatHostFrm.ProcessJoin(Sender: TRtcConnection);
  var
    UserKey:RtcByteArray;
    NewUserID,UserGroupID:TGateUID;
    OutBytes:TRtcHugeByteArray;
  begin
  UserKey:=nil;

  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)>0 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)>0 then
      begin
      {if assigned(Sender) then
        if not Sender.inMainThread then
          begin
          Sender.Sync(ProcessJoin);
          Exit;
          end;}

      NewUserID:=Bytes2Int(Link.Data.Content);
      UserKey:=Copy(Link.Data.Content,4,length(Link.Data.Content)-4);

      if length(UserKey)=0 then Exit;
      UserGroupID:=UserKey[0];

      // Do NOT add Self!
      if (NewUserID=Link.Client.MyUID) then Exit; // and (UserGroupID=MyGroupID) then Exit;

      // Do NOT add the same link twice!
      if Link.GetUserGroupStatus(NewUserID,UserGroupID)>0 then Exit;

      OutBytes:=TRtcHugeByteArray.Create;
      try
        OutBytes.AddEx(UserKey);
        OutBytes.AddEx(InviteKey);
        Link.Client.SendBytes(NewUserID,0,cid_ChatAccept,OutBytes.GetEx);
      finally
        OutBytes.Free;
        end;
      end;
    end;
  end;

procedure TChatHostFrm.ProcessLeft(Sender: TRtcConnection);
  begin
  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)>0 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)>0 then
      begin
      if assigned(Sender) then
        if not Sender.inMainThread then
          begin
          Sender.Sync(ProcessLeft);
          Exit;
          end;

      UserIsRemoved(Link.Data.UserID, Link.Data.GroupID);

      eChat.Lines.Add('* USER '+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+' LEFT'+Now2Str);

      // Beep if Chat window isn't currently active
      if GetActiveWindow<>Handle then MessageBeep(0);

      Link.ClearUserGroupStatus(Link.Data.UserID, Link.Data.GroupID);

      // We have no more active Groups with this User?
      if Link.GetMaxUserGroupStatus(Link.Data.UserID)<10 then
        // Remove the User from our receiver Group
        Link.Client.RemUserFromGroup(Link.Client.MyUID, MyGroupID, Link.Data.UserID);
      end;
    end;
  end;

procedure TChatHostFrm.ProcessReturn(Sender: TRtcConnection);
  var
    UserKey:RtcByteArray;
    UserGroupID:TGateUID;
    OutBytes:TRtcHugeByteArray;
  begin
  UserKey:=nil;

  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    UserGroupID:=Link.Data.Content[0];
    if Link.GetUserGroupStatus(Link.Data.UserID, UserGroupID)=5 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    UserGroupID:=Link.Data.Content[0];
    if Link.GetUserGroupStatus(Link.Data.UserID, UserGroupID)=5 then
      begin
      if assigned(Sender) then
        if not Sender.inMainThread then
          begin
          Sender.Sync(ProcessReturn);
          Exit;
          end;

      UserKey:=Link.Data.Content;

      OutBytes:=TRtcHugeByteArray.Create;
      try
        OutBytes.AddEx(UserKey);
        OutBytes.AddEx(InviteKey);
        Link.Client.SendBytes(Link.Data.UserID, 0, cid_ChatConfirm, OutBytes.GetEx);
        OutBytes.Clear;

        Link.SetUserGroupStatus(Link.Data.UserID, UserGroupID, 10);

        OutBytes.AddEx(Int2Bytes(Link.Data.UserID));
        OutBytes.AddEx(UserKey);
        Link.Client.SendBytes(Link.Client.MyUID, MyGroupID, cid_ChatJoined, OutBytes.GetEx);

        Link.Client.AddUserToGroup(Link.Client.MyUID,MyGroupID,Link.Data.UserID);
      finally
        OutBytes.Free;
        end;

      // User has already invited us to join his Group?
      if Link.Client.GetUserGroupStatus(Link.Data.UserID, UserGroupID)>0 then
        begin
        UserIsActive(Link.Data.UserID, UserGroupID);
        eChat.Lines.Add('* USER '+IntToStr(Link.Data.UserID)+'/'+IntToStr(UserGroupID)+' JOINED'+Now2Str);
        if GetActiveWindow<>Handle then MessageBeep(0);
        end;
      end;
    end;
  end;

procedure TChatHostFrm.ProcessMessage(Sender: TRtcConnection);
  var
    msg:String;
  begin
  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      begin
      if assigned(Sender) then
        if not Sender.inMainThread then
          begin
          Sender.Sync(ProcessMessage);
          Exit;
          end;

      msg:=Utf8DecodeEx(ZDecompress_Ex(Link.Data.Content));
      eChat.Lines.Add('['+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+']'+#13#10+
                      msg+#13#10+'<-----'+Now2Str);
      if GetActiveWindow<>Handle then MessageBeep(0);
      end;
    end;
  end;

procedure TChatHostFrm.ProcessPaintLine(Sender: TRtcConnection);
  var
    CR,CG,CB,CW:byte;
    MX1,MX2,MY1,MY2:longint;
  begin
  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      begin
      if length(Link.Data.Content)<>12 then Exit;

      CR:=Bytes2OneByte(Link.Data.Content,0);
      CG:=Bytes2OneByte(Link.Data.Content,1);
      CB:=Bytes2OneByte(Link.Data.Content,2);
      CW:=Bytes2OneByte(Link.Data.Content,3);
      MX1:=Bytes2Word(Link.Data.Content,4);
      MY1:=Bytes2Word(Link.Data.Content,6);
      MX2:=Bytes2Word(Link.Data.Content,8);
      MY2:=Bytes2Word(Link.Data.Content,10);

      FCS.Acquire;
      try
        FDrawing.Append;
        FDrawing.asInteger['R']:=CR;
        FDrawing.asInteger['G']:=CG;
        FDrawing.asInteger['B']:=CB;
        FDrawing.asInteger['W']:=CW;
        FDrawing.asInteger['X1']:=MX1;
        FDrawing.asInteger['Y1']:=MY1;
        FDrawing.asInteger['X2']:=MX2;
        FDrawing.asInteger['Y2']:=MY2;
      finally
        FCS.Release;
        end;

      runPaintJob.Post(nil);
      end;
    end;
  end;

procedure TChatHostFrm.ProcessPaintClear(Sender: TRtcConnection);
  begin
  if Link.Data.Header and not Link.Data.ToBuffer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      Link.Data.ToBuffer:=True;
    end;

  if Link.Data.Footer then
    begin
    if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
      begin
      if assigned(Sender) then
        if not Sender.inMainThread then
          begin
          Sender.Sync(ProcessPaintClear);
          Exit;
          end;

      eChat.Lines.Add('['+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+']'+#13#10+
                      '> Whiteboard (Drawing) CLEARED <'+#13#10+'<-----'+Now2Str);
      FCS.Acquire;
      try
        FDrawing.Clear;
        FLastPainted:=0;
      finally
        FCS.Release;
        end;

      runPaintJob.Post(nil);
      end;
    end;
  end;

procedure TChatHostFrm.LinkDataReceived(Sender: TRtcConnection);
  begin
  case Link.Data.CallID of
    cid_ChatAccept,
    cid_ChatConfirm:
      ProcessAccept(Sender);

    cid_ChatJoined:
      ProcessJoin(Sender);

    cid_ChatLeft:
      ProcessLeft(Sender);

    cid_ChatReturn:
      ProcessReturn(Sender);

    cid_ChatMessage:
      ProcessMessage(Sender);

    cid_ChatPaintLine:
      ProcessPaintLine(Sender);

    cid_ChatPaintClear:
      ProcessPaintClear(Sender);
    end;
  end;

procedure TChatHostFrm.eMessageKeyPress(Sender: TObject; var Key: Char);
  var
    msg:RtcByteArray;
    con:integer;
  begin
  msg:=nil;
  if Key=#13 then
    begin
    Key:=#0;
    if Link.Client.Ready then
      begin
      con:=ConnectedUsers;
      if con=0 then
        begin
        eChat.Lines.Add('* Nobody is listening in this Chat Room.'+#13#10+
                        '* Click "INVITE" to invite a new User.');
        end
      else if eMessage.Text<>'' then
        begin
        msg:=ZCompress_Ex(Utf8EncodeEx(eMessage.Text),zcFastest);
        Link.Client.SendBytes(Link.Client.MyUID,MyGroupID,cid_ChatMessage,msg);
        eChat.Lines.Add('[ME] to {'+ConnectedUserList+'}'+#13#10+
                        eMessage.Text+#13#10+'----->'+Now2Str);
        eMessage.Text:='';
        end;
      end;
    end;
  end;

procedure TChatHostFrm.LinkInfoReceived(Sender: TRtcConnection);
  var
    gid:TGateUID;
  begin
  case Link.Data.Command of
    gc_UserOffline:
      begin
      if Link.GetMinUserGroupStatus(Link.Data.UserID)>0 then
        begin
        if assigned(Sender) then
          if not Sender.inMainThread then
            begin
            Sender.Sync(LinkInfoReceived);
            Exit;
            end;

        eChat.Lines.Add('* User '+IntToStr(Link.Data.UserID)+' is OFF-LINE'+Now2Str);
        if GetActiveWindow<>Handle then MessageBeep(0);

        repeat
          GID:=Link.GetMinUserGroupID(Link.Data.UserID);
          UserIsRemoved(Link.Data.UserID, GID);
          Link.ClearUserGroupStatus(Link.Data.UserID,GID);
          until GID=0;
        end;
      end;

    gc_JoinedUser:
      begin
      // User came Online and added us to his group
      if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)>0 then
        begin
        if assigned(Sender) then
          if not Sender.inMainThread then
            begin
            Sender.Sync(LinkInfoReceived);
            Exit;
            end;

        if Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
          begin
          UserIsActive(Link.Data.UserID, Link.Data.GroupID);
          eChat.Lines.Add('* User '+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+' JOINED'+Now2Str);
          if GetActiveWindow<>Handle then MessageBeep(0);
          end;
        {else
          begin
          UserIsRemoved(Link.Data.UserID, Link.Data.GroupID);

          eChat.Lines.Add('* User '+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+' LEFT.');

          Link.LeftUserGroup(Link.Data.UserID,Link.Data.GroupID);

          Link.Client.RemUserFromGroup(Link.Client.MyUID,MyGroupID,Link.Data.UserID);
          end;}
        end;
      end;

    gc_LeftUser:
      begin
      // User went offline, or removed us from his group
      if  Link.GetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID)=10 then
        begin
        if assigned(Sender) then
          if not Sender.inMainThread then
            begin
            Sender.Sync(LinkInfoReceived);
            Exit;
            end;

        UserIsPassive(Link.Data.UserID, Link.Data.GroupID);

        eChat.Lines.Add('* User '+IntToStr(Link.Data.UserID)+'/'+IntToStr(Link.Data.GroupID)+' AWAY'+Now2Str);
        if GetActiveWindow<>Handle then MessageBeep(0);

        Link.SetUserGroupStatus(Link.Data.UserID, Link.Data.GroupID, 5);

        Link.Client.RemUserFromGroup(Link.Client.MyUID,MyGroupID,Link.Data.UserID);
        end;
      end;
    end;
  end;

procedure TChatHostFrm.eUsersDblClick(Sender: TObject);
  var
    UID:String;
    i:integer;
    UserID,GroupID:TGateUID;
  begin
  if (eUsers.Items.Count>0) and (eUsers.ItemIndex>=0) and Link.Client.Ready then
    begin
    UID:=Trim(eUsers.Items.Strings[eUsers.ItemIndex]);
    if Copy(UID,1,2)='* ' then Delete(UID,1,2);
    i:=Pos('/',UID);
    if i<=0 then 
      begin
      UserID:=StrToInt(UID);
      GroupID:=0;
      end
    else
      begin
      UserID:=StrToInt(Copy(UID,1,i-1));
      GroupID:=StrToInt(Copy(UID,i+1,length(UID)-i));
      end;

    if Link.GetUserGroupStatus(UserID,GroupID)<10 then
      InviteUserToChat(UserID,GroupID)
    else
      Link.Client.RemUserFromGroup(Link.Client.MyUID,MyGroupID,UserID);
    end;
    
  eMessage.SetFocus;
  end;

procedure TChatHostFrm.UserIsActive(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf(UID);
  if i<0 then eUsers.Items.Add(UID);
  end;

procedure TChatHostFrm.UserIsPassive(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf(UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i<0 then eUsers.Items.Add('* '+UID);
  end;

procedure TChatHostFrm.UserIsRemoved(UserID,GroupID: TGateUID);
  var
    UID:RtcString;
    i:integer;
  begin
  if GroupID>0 then
    begin
    UID:=IntToStr(UserID);
    i:=eUsers.Items.IndexOf('* '+UID);
    if i>=0 then eUsers.Items.Delete(i);
    UID:=IntToStr(UserID)+'/'+IntToStr(GroupID);
    end
  else
    UID:=IntToStr(UserID);
  i:=eUsers.Items.IndexOf('* '+UID);
  if i>=0 then eUsers.Items.Delete(i);
  i:=eUsers.Items.IndexOf(UID);
  if i>=0 then eUsers.Items.Delete(i);
  end;

procedure TChatHostFrm.LinkStreamReset(Sender: TRtcConnection);
  var
    UID,GID,GST:TGateUID;
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(LinkStreamReset);
      Exit;
      end;

  StreamWasReset:=True;
  eChat.Lines.Add('* Connection Lost'+Now2Str);
  UID:=0; GID:=0;
  repeat
    GST:=Link.GetNextUserGroupStatus(UID,GID);
    if GST=10 then
      begin
      StreamWasReset:=True;
      Link.SetUserGroupStatus(UID,GID,5);
      UserIsPassive(UID,GID);
      end;
    until GST=0;
  end;

procedure TChatHostFrm.LinkAfterLogOut(Sender: TRtcConnection);
  begin
  if assigned(Sender) then
    if not Sender.inMainThread then
      begin
      Sender.Sync(LinkAfterLogOut);
      Exit;
      end;

  eChat.Lines.Add('* Logged OUT'+Now2Str);
  Link.ClearAllUserGroupStates;
  eUsers.Clear;
  end;

procedure TChatHostFrm.LinkReadyToSend(Sender: TRtcConnection);
  var
    UID,GID:TGateUID;
  begin
  if StreamWasReset then
    begin
    if assigned(Sender) then
      if not Sender.inMainThread then
        begin
        Sender.Sync(LinkReadyToSend);
        Exit;
        end;

    eChat.Lines.Add('* Connected'+Now2Str);
    StreamWasReset:=False;
    UID:=0; GID:=0;
    while Link.GetNextUserGroupStatus(UID,GID)>0 do
      InviteUserToChat(UID,GID,False);
    end;
  end;

procedure TChatHostFrm.AddAllDisabledUsers;
  var
    UID,GID,GST:TGateUID;
  begin
  if MyGroupID>0 then
    begin
    UID:=0; GID:=0;
    repeat
      GST:=Link.GetNextUserGroupStatus(UID,GID);
      if (GST>0) and (GST<10) then
        Link.Client.AddUserToGroup(Link.Client.MyUID,MyGroupID,UID);
      until GST=0;
    end;
  end;

function TChatHostFrm.ConnectedUsers: integer;
  var
    UID,GID,GST:TGateUID;
  begin
  Result:=0;
  UID:=0; GID:=0;
  repeat
    GST:=Link.GetNextUserGroupStatus(UID,GID);
    if GST=10 then
      Inc(Result);
    until GST=0;
  end;

function TChatHostFrm.ConnectedUserList: String;
  var
    UID,GID,GST:TGateUID;
  begin
  Result:='';
  UID:=0; GID:=0;
  repeat
    GST:=Link.GetNextUserGroupStatus(UID,GID);
    if GST=10 then
      Result:=Result+','+IntToStr(UID)+'/'+IntToStr(GID);
    until GST=0;
  if length(Result)>0 then
    Delete(Result,1,1);
  end;

procedure TChatHostFrm.pbDrawingMouseDown(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  var
    CR,CG,CB,CW:byte;
    OutBytes:TRtcHugeByteArray;
  begin
  if (Button=mbLeft) and Link.Client.Ready and (ConnectedUsers>0) then
    begin
    if X<0 then X:=0;
    if Y<0 then Y:=0;

    LMDown:=True;
    LMX1:=X;
    LMY1:=Y;
    CR:=ColorToRGB(pbPenColor.Selected) and $FF;
    CG:=ColorToRGB(pbPenColor.Selected) shr 8 and $FF;
    CB:=ColorToRGB(pbPenColor.Selected) shr 16 and $FF;
    CW:=pbPenWidth.Position;

    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(OneByte2Bytes(CR));
      OutBytes.AddEx(OneByte2Bytes(CG));
      OutBytes.AddEx(OneByte2Bytes(CB));
      OutBytes.AddEx(OneByte2Bytes(CW));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      Link.Client.SendBytes(Link.Client.MyUID,MyGroupID,cid_ChatPaintLine,OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;

    FCS.Acquire;
    try
      FDrawing.Append;
      FDrawing.asInteger['R']:=CR;
      FDrawing.asInteger['G']:=CG;
      FDrawing.asInteger['B']:=CB;
      FDrawing.asInteger['W']:=CW;
      FDrawing.asInteger['X1']:=LMX1;
      FDrawing.asInteger['Y1']:=LMY1;
      FDrawing.asInteger['X2']:=LMX1;
      FDrawing.asInteger['Y2']:=LMY1;
    finally
      FCS.Release;
      end;

    PaintNewLines;
    end;
  end;

procedure TChatHostFrm.pbDrawingMouseMove(Sender: TObject;
    Shift: TShiftState; X, Y: Integer);
  var
    CR,CG,CB,CW:byte;
    OutBytes:TRtcHugeByteArray;
  begin
  if LMDown and Link.Client.Ready and (ConnectedUsers>0) then
    begin
    if X<0 then X:=0;
    if Y<0 then Y:=0;

    CR:=ColorToRGB(pbPenColor.Selected) and $FF;
    CG:=ColorToRGB(pbPenColor.Selected) shr 8 and $FF;
    CB:=ColorToRGB(pbPenColor.Selected) shr 16 and $FF;
    CW:=pbPenWidth.Position;

    OutBytes:=TRtcHugeByteArray.Create;
    try
      OutBytes.AddEx(OneByte2Bytes(CR));
      OutBytes.AddEx(OneByte2Bytes(CG));
      OutBytes.AddEx(OneByte2Bytes(CB));
      OutBytes.AddEx(OneByte2Bytes(CW));
      OutBytes.AddEx(Word2Bytes(LMX1));
      OutBytes.AddEx(Word2Bytes(LMY1));
      OutBytes.AddEx(Word2Bytes(X));
      OutBytes.AddEx(Word2Bytes(Y));
      Link.Client.SendBytes(Link.Client.MyUID,MyGroupID,cid_ChatPaintLine,OutBytes.GetEx);
    finally
      OutBytes.Free;
      end;

    FCS.Acquire;
    try
      FDrawing.Append;
      FDrawing.asInteger['R']:=CR;
      FDrawing.asInteger['G']:=CG;
      FDrawing.asInteger['B']:=CB;
      FDrawing.asInteger['W']:=CW;
      FDrawing.asInteger['X1']:=LMX1;
      FDrawing.asInteger['Y1']:=LMY1;
      FDrawing.asInteger['X2']:=X;
      FDrawing.asInteger['Y2']:=Y;
    finally
      FCS.Release;
      end;

    LMX1:=X; LMY1:=Y;

    PaintNewLines;
    end;
  end;

procedure TChatHostFrm.pbDrawingMouseUp(Sender: TObject;
    Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  begin
  if Button=mbLeft then
    LMDown:=False;
  end;

procedure TChatHostFrm.PaintLine;
  begin
  with pbDrawing.Canvas do
    begin
    Pen.Color:=RGB(FDrawing.asInteger['R'],FDrawing.asInteger['G'],FDrawing.asInteger['B']);
    Pen.Width:=FDrawing.asInteger['W'];
    MoveTo(FDrawing.asInteger['X1'],FDrawing.asInteger['Y1']);
    LineTo(FDrawing.asInteger['X2'],FDrawing.asInteger['Y2']);
    end;
  end;

procedure TChatHostFrm.PaintDrawing;
  begin
  with pbDrawing.Canvas do
    begin
    Brush.Color:=clWhite;
    Brush.Style:=bsSolid;
    FillRect(Rect(0,0,pbDrawing.Width,pbDrawing.Height));
    end;
  FCS.Acquire;
  try
    FDrawing.First;
    while not FDrawing.Eof do
      begin
      PaintLine;
      FDrawing.Next;
      end;
    FLastPainted:=FDrawing.Row;
  finally
    FCS.Release;
    end;
  end;

procedure TChatHostFrm.PaintNewLines;
  begin
  if FLastPainted=0 then
    PaintDrawing
  else
    begin
    FCS.Acquire;
    try
      if FLastPainted<FDrawing.RowCount then
        begin
        FDrawing.Row:=FLastPainted;
        while not FDrawing.Eof do
          begin
          PaintLine;
          FDrawing.Next;
          end;
        FLastPainted:=FDrawing.Row;
        end;
    finally
      FCS.Release;
      end;
    end;
  end;

procedure TChatHostFrm.FormCreate(Sender: TObject);
  begin
  FCS:=TRtcCritSec.Create;
  FDrawing:=TRtcDataSet.Create;
  end;

procedure TChatHostFrm.pbDrawingPaint(Sender: TObject);
  begin
  PaintDrawing;
  end;

procedure TChatHostFrm.btnClearDrawingClick(Sender: TObject);
  begin
  if MessageDlg('Clear the Whiteboard in this Room?'#13#10+
                'This operation can NOT be undone!',mtWarning,[mbYes,mbNo],0)=mrYes then
    begin
    FCS.Acquire;
    try
      FDrawing.Clear;
      FLastPainted:=0;
    finally
      FCS.Release;
      end;
    PaintNewLines;
    if Link.Client.Ready then
      Link.Client.SendBytes(Link.Client.MyUID,MyGroupID,cid_ChatPaintClear);
    end;
  end;

procedure TChatHostFrm.runPaintJobExecute(Data: TRtcValue);
  begin
  PaintNewLines;
  end;

end.

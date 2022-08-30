unit AlertWindowDemoMain;

{$I cxVer.inc}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, ImgList, cxContainer, cxEdit, cxListBox, ComCtrls,
  ToolWin, dxAlertWindow, StdCtrls, cxGeometry, cxClasses, BaseForm,
  cxListView, ExtCtrls, ShellAPI, StrUtils;

type
  TAlertWindowDemoForm = class(TfmBaseForm)
    ilToolBar: TcxImageList;
    dxAlertWindowManager1: TdxAlertWindowManager;
    ilMessages: TcxImageList;
    pmAlertWindow: TPopupMenu;
    pmUnqueueMessage: TMenuItem;
    pmDeleteMessage: TMenuItem;
    pmCopyMessage: TMenuItem;
    miOptions: TMenuItem;
    lvContactList: TcxListView;
    bvSpacerLeft: TBevel;
    bvSpacerRight: TBevel;
    bvSpacerBottom: TBevel;
    tmGenerateMessage: TTimer;
    ilContactList_XP: TcxImageList;
    ilContactList_7: TcxImageList;
    procedure FormCreate(Sender: TObject);
    procedure pmUnqueueMessageClick(Sender: TObject);
    procedure pmDeleteMessageClick(Sender: TObject);
    procedure pmCopyMessageClick(Sender: TObject);
    procedure miOptionsAlertWindowClick(Sender: TObject);
    procedure tmGenerateMessageTimer(Sender: TObject);
    procedure lvContactListDblClick(Sender: TObject);
    procedure pmAlertWindowPopup(Sender: TObject);
    procedure dxAlertWindowManager1ButtonClick(Sender: TObject;
      AAlertWindow: TdxAlertWindow; AButtonIndex: Integer);
    procedure FormShow(Sender: TObject);
    procedure dxAlertWindowManager1Initialize(Sender: TObject;
      AAlertWindow: TdxAlertWindow);
    procedure FormDestroy(Sender: TObject);
  private
    FContactMessagesToGenerate: Integer;
    FContactMessageCounters: array of Integer;

    procedure NewMessage(AContactIndex: Integer);
    function FindWindowByCaption(const ACaption: string): TdxAlertWindow;
    function GetCurrentMessage(AAlertWindow: TdxAlertWindow): TdxAlertWindowMessage;
    procedure InitializeGenerateMessagesTimer(ACount: Integer; AInterval: Cardinal);
  end;

var
  AlertWindowDemoForm: TAlertWindowDemoForm;

implementation

uses
  dxCore, Math, Clipbrd, AlertWindowDemoOptions;

{$R *.dfm}

procedure TAlertWindowDemoForm.FormCreate(Sender: TObject);
begin
  lvContactList.SmallImages := ilContactList_XP;
  Randomize;
end;

function TAlertWindowDemoForm.FindWindowByCaption(const ACaption: string): TdxAlertWindow;

  function IsWindowForMessagesGroup(AAlertWindow: TdxAlertWindow): Boolean;
  begin
    Result := (AAlertWindow <> nil) and (AAlertWindow.Tag = 1) and
      (AAlertWindow.VisibilityTransition <> awvtHiding);
  end;

var
  I: Integer;
begin
  Result := nil;
  for I := 0 to dxAlertWindowManager1.Count - 1 do
  begin
    Result := dxAlertWindowManager1.Items[I];
    if IsWindowForMessagesGroup(Result) and (Result.MessageList[0].Caption = ACaption) then
      Break
    else
      Result := nil;
  end;
end;

procedure TAlertWindowDemoForm.InitializeGenerateMessagesTimer(
  ACount: Integer; AInterval: Cardinal);
begin
  FContactMessagesToGenerate := ACount;
  tmGenerateMessage.Interval := AInterval;
  tmGenerateMessage.Enabled := True;
end;

function TAlertWindowDemoForm.GetCurrentMessage(
  AAlertWindow: TdxAlertWindow): TdxAlertWindowMessage;
begin
  Result := AAlertWindow.MessageList[AAlertWindow.CurrentMessageIndex];
end;

procedure TAlertWindowDemoForm.miOptionsAlertWindowClick(Sender: TObject);
begin
  FormOptions.LoadWindowOptions(dxAlertWindowManager1);
  FormOptions.Show;
end;

procedure TAlertWindowDemoForm.NewMessage(AContactIndex: Integer);

  function FormatMessageText(AMessageNumber, AContactIndex: Integer): string;
  const
    FormatTextMessage = 'Message #%d from %s.' + #13#10 +
      'Note: Every new message from %s will be displayed in this window.';
  var
    ACaption: string;
  begin
    ACaption := lvContactList.Items[AContactIndex].Caption;
    Result := Format(FormatTextMessage, [AMessageNumber, ACaption, ACaption]);
  end;

  procedure ShowNewMessage(var AAlertWindow: TdxAlertWindow; const AMessageText: string; AContactIndex: Integer);
  var
    ACaption: string;
  begin
    ACaption := lvContactList.Items[AContactIndex].Caption;
    if AAlertWindow = nil then
    begin
      AAlertWIndow := dxAlertWindowManager1.Show(ACaption, AMessageText, AContactIndex);
      AAlertWIndow.Tag := 1;
    end
    else
    begin
      AAlertWindow.MessageList.Add(ACaption, AMessageText, AContactIndex);
      AAlertWindow.RestartDisplayTimer;
    end;
  end;

  procedure AddNavigationInfoToMessage(AAlertWindow: TdxAlertWindow; AIndexMessage: Integer);
  begin
    AAlertWindow.MessageList[AIndexMessage].Text := AAlertWindow.MessageList[AIndexMessage].Text
      + #13#10 + #13#10 + 'Use navigation buttons below to browse the message queue.';
  end;

var
  AMessageNumber: Integer;
  AAlertWindow: TdxAlertWindow;
begin
  if lvContactList.Items.Count >= 0 then
  begin
    AAlertWindow := FindWindowByCaption(lvContactList.Items[AContactIndex].Caption);
    Inc(FContactMessageCounters[AContactIndex]);
    AMessageNumber := FContactMessageCounters[AContactIndex];
    ShowNewMessage(AAlertWindow, FormatMessageText(AMessageNumber, AContactIndex), AContactIndex);
    if AAlertWindow.MessageList.Count > 1 then
    begin
      AddNavigationInfoToMessage(AAlertWindow, AAlertWindow.MessageList.Count - 1);
      if AAlertWindow.MessageList.Count = 2 then
        AddNavigationInfoToMessage(AAlertWindow, 0);
    end;
  end;
end;

procedure TAlertWindowDemoForm.pmCopyMessageClick(Sender: TObject);
begin
  Clipboard.AsText := GetCurrentMessage(pmAlertWindow.PopupComponent as TdxAlertWindow).Text;
end;

procedure TAlertWindowDemoForm.pmDeleteMessageClick(Sender: TObject);
begin
  (pmAlertWindow.PopupComponent as TdxAlertWindow).DeleteCurrentMessage;
end;

procedure TAlertWindowDemoForm.pmUnqueueMessageClick(Sender: TObject);
var
  AAlertWindow: TdxAlertWindow;
  AMessage: TdxAlertWindowMessage;
begin
  AAlertWindow := (pmAlertWindow.PopupComponent as TdxAlertWindow);
  AMessage := GetCurrentMessage(AAlertWindow);

  dxAlertWindowManager1.Show(AMessage.Caption,
    Copy(AMessage.Text, 1, Pos(#13#10, AMessage.Text) - 1), AMessage.ImageIndex);
  AAlertWindow.DeleteCurrentMessage;
end;

procedure TAlertWindowDemoForm.tmGenerateMessageTimer(Sender: TObject);
begin
  Dec(FContactMessagesToGenerate);
  NewMessage(RandomRange(0, lvContactList.Items.Count));
  if FContactMessagesToGenerate = 0 then
  begin
    tmGenerateMessage.Enabled := False;
    InitializeGenerateMessagesTimer(1, RandomRange(5000, 10000));
  end;
end;

procedure TAlertWindowDemoForm.lvContactListDblClick(Sender: TObject);
begin
  if lvContactList.ItemIndex > -1 then
    NewMessage(lvContactList.ItemIndex);
end;

procedure TAlertWindowDemoForm.pmAlertWindowPopup(Sender: TObject);
begin
  pmUnqueueMessage.Enabled := (pmAlertWindow.PopupComponent.Tag = 1) and
    ((pmAlertWindow.PopupComponent as TdxAlertWindow).MessageList.Count > 1);
end;

procedure TAlertWindowDemoForm.dxAlertWindowManager1ButtonClick(
  Sender: TObject; AAlertWindow: TdxAlertWindow; AButtonIndex: Integer);
var
  AAddressMessage: string;
begin
  if AButtonIndex = 0 then
  begin
    AAddressMessage := AnsiReplaceStr(AAlertWindow.MessageList[
      AAlertWindow.CurrentMessageIndex].Caption, ' ', '%20');
    ShellExecute(Handle, 'Open', PChar('mailto:' + AAddressMessage + '?'),
      nil, nil, sw_restore)
  end
  else
    if AButtonIndex = 1 then
      ShowMessage('Clicked!');
end;

procedure TAlertWindowDemoForm.FormShow(Sender: TObject);
begin
  SetLength(FContactMessageCounters, lvContactList.Items.Count);
  InitializeGenerateMessagesTimer(2, 1000);
end;

procedure TAlertWindowDemoForm.dxAlertWindowManager1Initialize(
  Sender: TObject; AAlertWindow: TdxAlertWindow);
begin
  AAlertWindow.MessageList[0].Text := AAlertWindow.MessageList[0].Text + #13#13 +
    'First message sent: ' + DateTimeToStr(Now);
end;

procedure TAlertWindowDemoForm.FormDestroy(Sender: TObject);
begin
//
end;

end.

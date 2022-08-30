unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MSI_DeviceMonitor, ImgList, ComCtrls, MiTeC_CfgMgrSetupApi;

type
  Twnd_dm_Main = class(TForm)
    Box: TListBox;
    Button1: TButton;
    DeviceMonitor: TMiTeC_DeviceMonitor;
    Button2: TButton;
    sd: TSaveDialog;
    cbxAll: TCheckBox;
    Button3: TButton;
    cbxACtive: TCheckBox;
    Memo: TMemo;
    ilSystem: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure cbxACtiveClick(Sender: TObject);
    procedure DeviceMonitorDeviceDisconect(Sender: TObject;
      DeviceDesc: TDeviceDesc);
    procedure DeviceMonitorDeviceConnect(Sender: TObject;
      DeviceDesc: TDeviceDesc);
    procedure Button3Click(Sender: TObject);
    procedure BoxDrawItem(Control: TWinControl; Index: Integer; Rect: TRect;
      State: TOwnerDrawState);
    procedure Button2Click(Sender: TObject);
    procedure BoxDblClick(Sender: TObject);
    procedure DeviceMonitorVolumeDisconnect(Sender: TObject; Drives: string;
      Remote: Boolean);
    procedure DeviceMonitorVolumeConnect(Sender: TObject; Drives: string;
      Remote: Boolean);
    procedure DeviceMonitorMessage(Sender: TObject; Msg: TMessage);
    procedure Button1Click(Sender: TObject);
    procedure BoxClick(Sender: TObject);
  private
    spid: TSPClassImageListData;
  public
    procedure AddLog(AText: string; AType: Integer = -1);
  end;

var
  wnd_dm_Main: Twnd_dm_Main;

implementation

uses MiTeC_Dialogs, MiTeC_Routines, MSI_Devices;

{$R *.dfm}

procedure Twnd_dm_Main.AddLog;
begin
  Box.Items.Add(Format('%d#%s > %s',[AType,DatetimetoStr(now),AText]));
  Box.ItemIndex:=Box.Items.Count-1;
  BoxClick(nil);
end;

procedure Twnd_dm_Main.BoxClick(Sender: TObject);
begin
  try
    Memo.Lines.Text:=Copy(Box.Items[Box.ItemIndex],Pos('>',Box.Items[Box.ItemIndex])+1,1024);
  finally

  end;
end;

procedure Twnd_dm_Main.BoxDblClick(Sender: TObject);
var
  ts,t,s: string;
  p: Integer;
begin
  if Box.ItemIndex>-1 then begin
    s:=Box.Items[Box.ItemIndex];
    p:=Pos('>',s);
    ts:=Trim(Copy(s,1,p-1));
    t:=Trim(Copy(s,p+1,Length(s)));
    InputQuery(ts,'',t);
  end;
end;

procedure Twnd_dm_Main.BoxDrawItem(Control: TWinControl; Index: Integer;
  Rect: TRect; State: TOwnerDrawState);
var
  R: TRect;
  pc,fc: TColor;
  s,ts,t: string;
  b: Boolean;
  ii,p: Integer;
begin
  inherited;
  with TListBox(Control) do begin
    s:=Items[Index];
    p:=Pos('#',s);
    ii:=StrToIntDef(Copy(s,1,p-1),-1);
    System.Delete(s,1,p);
    p:=Pos('>',s);
    ts:=Trim(Copy(s,1,p-1));
    t:=Trim(Copy(s,p+1,Length(s)));
    p:=Pos('*',t);
    b:=p=0;
    if p=1 then
      t:=Copy(t,2,Length(t));
    Canvas.FillRect(Rect);
    fc:=Canvas.Font.Color;
    pc:=Canvas.Pen.Color;
    if not(odSelected in State) then
      Canvas.Font.Color:=clGray;
    CopyRect(R,Rect);
    R.Right:=110;
//    Canvas.FillRect(R);
    if not(odSelected in State) then
      Canvas.Pen.Color:=clGray
    else
      Canvas.Pen.Color:=clWhite;
    Canvas.MoveTo(R.Right,R.Top);
    Canvas.LineTo(R.Right,R.Bottom);
    Canvas.Pen.Color:=pc;
    Canvas.TextOut(Rect.Left+3,Rect.Top+(ItemHeight-Canvas.TextHeight(ts)) div 2,ts);
    if ii>-1 then
      ilSystem.Draw(Canvas,Rect.Left+115,Rect.Top+1,ii);
    Canvas.Font.Color:=fc;
    if b then
      Canvas.Font.Style:=[fsBold];
    Canvas.TextOut(Rect.Left+135,Rect.Top+(ItemHeight-Canvas.TextHeight(t)) div 2,t);
  end;
end;

procedure Twnd_dm_Main.Button1Click(Sender: TObject);
begin
  Close;
end;

procedure Twnd_dm_Main.Button2Click(Sender: TObject);
begin
  sd.FileName:=ChangeFileExt(Application.ExeName,'.log');
  if sd.Execute then
    Box.Items.SaveToFile(sd.Filename);
end;

procedure Twnd_dm_Main.Button3Click(Sender: TObject);
begin
  if YesNo('Clear log?') then
    Box.Items.Clear;
end;

procedure Twnd_dm_Main.cbxACtiveClick(Sender: TObject);
begin
  DeviceMonitor.Active:=cbxActive.Checked;
end;

procedure Twnd_dm_Main.DeviceMonitorDeviceConnect(Sender: TObject;
  DeviceDesc: TDeviceDesc);
var
  t: Integer;
  g: TGUID;
begin
  if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_USB_DEVICE)) then
    g:=GUID_DEVCLASS_USB
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_DISK)) then
    g:=GUID_DEVCLASS_DISKDrive
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_WPD)) then
    g:=GUID_DEVCLASS_WPD
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_REMOVABLE_STORAGE)) then
    g:=GUID_DEVCLASS_FDC
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_VOLUME)) then
    g:=GUID_DEVCLASS_VOLUME
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_BLUETOOTH)) then
    g:=GUID_DEVCLASS_BLUETOOTH
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_NET)) then
    g:=GUID_DEVCLASS_NET
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_MOUSE)) then
    g:=GUID_DEVCLASS_MOUSE
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_KEYBOARD)) then
    g:=GUID_DEVCLASS_KEYBOARD
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_DISPLAY_ADAPTER)) then
    g:=GUID_DEVCLASS_DISPLAY
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_MONITOR)) then
    g:=GUID_DEVCLASS_MONITOR
  else
    g:=StringToGUID(DeviceDesc.GUID);
  //AddLog('GUID: '+DeviceDesc.GUID);
  SetupDiGetClassImageIndex(spid,g,t);
  if DeviceDesc.Name<>'' then
    AddLog(Format('%s (%s) connected.',[DeviceDesc.Location,DeviceDesc.Name]),t)
  else if DeviceDesc.Description<>'' then
    AddLog(Format('%s (%s) connected.',[DeviceDesc.Location,DeviceDesc.Description]),t)
  else if DeviceDesc.SymbolicName<>'' then
    AddLog(Format('Device (%s) connected.',[ExtractFilename(Uppercase(DeviceDesc.SymbolicName))]),t)
  else
    AddLog('Device connected.',t);
end;

procedure Twnd_dm_Main.DeviceMonitorDeviceDisconect(Sender: TObject;
  DeviceDesc: TDeviceDesc);
var
  t: Integer;
  g: TGUID;
begin
  if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_USB_DEVICE)) then
    g:=GUID_DEVCLASS_USB
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_DISK)) then
    g:=GUID_DEVCLASS_DISKDRIVE
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_WPD)) then
    g:=GUID_DEVCLASS_WPD
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_REMOVABLE_STORAGE)) then
    g:=GUID_DEVCLASS_FDC
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_VOLUME)) then
    g:=GUID_DEVCLASS_VOLUME
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_BLUETOOTH)) then
    g:=GUID_DEVCLASS_BLUETOOTH
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_NET)) then
    g:=GUID_DEVCLASS_NET
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_MOUSE)) then
    g:=GUID_DEVCLASS_MOUSE
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_KEYBOARD)) then
    g:=GUID_DEVCLASS_KEYBOARD
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_DISPLAY_ADAPTER)) then
    g:=GUID_DEVCLASS_DISPLAY
  else if SameText(DeviceDesc.GUID,GUIDToString(GUID_DEVINTERFACE_MONITOR)) then
    g:=GUID_DEVCLASS_MONITOR
  else
    g:=StringToGUID(DeviceDesc.GUID);
  //AddLog('GUID: '+DeviceDesc.GUID);
  SetupDiGetClassImageIndex(spid,g,t);
  if DeviceDesc.Name<>'' then
    AddLog(Format('%s (%s) disconnected.',[DeviceDesc.Location,DeviceDesc.Name]),t)
  else if DeviceDesc.Description<>'' then
    AddLog(Format('%s (%s) disconnected.',[DeviceDesc.Location,DeviceDesc.Description]),t)
  else if DeviceDesc.SymbolicName<>'' then
    AddLog(Format('Device (%s) disconnected.',[DeviceDesc.SymbolicName]),t)
  else
    AddLog('Device disconnected.',t);
end;

procedure Twnd_dm_Main.DeviceMonitorMessage(Sender: TObject;
  Msg: TMessage);
var
  m,d: string;
begin
  if not cbxAll.Checked then
    Exit;
  m:=GetMessageType(Msg.wParam);
  case Msg.wParam of
    DBT_DEVICEARRIVAL..DBT_CUSTOMEVENT: begin
      d:=GetDeviceType(PDevBroadcastHdr(Msg.lParam).dbch_devicetype);
      case PDevBroadcastHdr(Msg.lParam).dbch_devicetype of
        DBT_DEVTYP_OEM,
        DBT_DEVTYP_DEVNODE,
        DBT_DEVTYP_PORT: AddLog(Format('*%s: %s',[m,d]));
        DBT_DEVTYP_VOLUME: AddLog(Format('*%s: %s - 0x%x - 0x%x',[m,d,
                                                              PDevBroadcastVolume(Msg.lParam).dbcv_unitmask,
                                                              PDevBroadcastVolume(Msg.lParam).dbcv_flags]));
        DBT_DEVTYP_NET: AddLog(Format('*%s: %s - 0x%x',[m,d,PDevBroadcastNet(Msg.lParam).dbcn_resource]));
        DBT_DEVTYP_DEVICEINTERFACE:
          try
            AddLog(Format('*%s: %s - %s (Class: %s)',[m,d,
                                          string(PChar(@PDevBroadcastDeviceInterface(Msg.lParam).dbcc_name)),
                                          GUIDToString(PDevBroadcastDeviceInterface(Msg.lParam).dbcc_classguid)]));
          except
            AddLog(Format('*%s: %s',[m,d]));
          end;
        DBT_DEVTYP_HANDLE: AddLog(Format('*%s - %s - %s',[m,d,GUIDToString(PDevBroadcastHandle(Msg.lParam).dbch_eventguid)]));
        else AddLog(Format('*%s: %s',[m,d]));
      end;
    end;
    else AddLog(Format('*%s: 0x%x',[m,Msg.lParam]));
  end;
end;

procedure Twnd_dm_Main.DeviceMonitorVolumeConnect(Sender: TObject; Drives: string;
  Remote: Boolean);
var
  c: char;
begin
  for c in Drives do
    if Remote then
      AddLog(Format('Network drive %s: connected.',[c]))
    else
      AddLog(Format('Drive %s: connected.',[c]));
end;

procedure Twnd_dm_Main.DeviceMonitorVolumeDisconnect(Sender: TObject;
  Drives: string; Remote: Boolean);
var
  c: char;
begin
  for c in Drives do
    if Remote then
      AddLog(Format('Network drive %s: disconnected.',[c]))
    else
      AddLog(Format('Drive %s: disconnected.',[c]));
end;

procedure Twnd_dm_Main.FormCreate(Sender: TObject);
begin
  spid.cbSize:=sizeof(spid);
  SetupDiGetClassImageList(spid);
  ilSystem.Handle:=spid.ImageList;
end;

end.

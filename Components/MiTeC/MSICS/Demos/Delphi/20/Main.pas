unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, ExtCtrls, Gauges, StdCtrls, MSI_NetConMon;

type
  TwndMain = class(TForm)
    List: TListView;
    sb: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure NetConMonThreadInterval(Sender: TNetConMonThread);
  private
    FNCM: TNetConMonThread;
    procedure RefreshList;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_Routines, MiTeC_IpHlpAPI, TlHelp32;

{$R *.dfm}

function GetProcessName(APID: Cardinal): string;
var
  ps: THandle;
  pe32: TProcessEntry32;
  ok: Boolean;
begin
  Result:='';
  if APID=0 then begin
    Result:='System Idle Process';
    Exit;
  end;
  ps:=CreateToolhelp32Snapshot(TH32CS_SNAPPROCESS,0);
  if (ps<>INVALID_HANDLE_VALUE) then
    try
      pe32.dwSize:=sizeof(TPROCESSENTRY32);
      ok:=Process32First(ps,pe32);
      while ok do begin
        if pe32.th32ProcessID=APID then begin
          Result:=pe32.szExeFile;
          Break;
        end;
        ok:=Process32Next(ps,pe32);
      end;
    finally
      CloseHandle(ps);
    end;
end;

{ TwndMain }

procedure TwndMain.FormCreate(Sender: TObject);
begin
  FNCM:=TNetConMonThread.Create;
  FNCM.OnInterval:=NetConMonThreadInterval;
  FNCM.Suspended:=False;
end;

procedure TwndMain.NetConMonThreadInterval(Sender: TNetConMonThread);
begin
  RefreshList;
end;

procedure TwndMain.RefreshList;
var
  i,c: Integer;
  vp: Integer;
  SInfo: TScrollInfo;
  r: TRect;
  p: TNetConRecord;
  s: string;
  vi: TVersionInfo;
begin
  SInfo.cbSize:=SizeOf(SInfo);
  SInfo.fMask:=SIF_ALL;
  GetScrollInfo(List.Handle,SB_VERT,SInfo);
  vp:=SInfo.nPos;
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    c:=FNCM.RecordCount;
    for i:=0 to c-1 do begin
      FNCM.GetRecord(i,p);
      p.ProcessName:=GetProcessName(p.PID);
      FNCM.SetProcess(i,p.ProcessName,vi,-1);
      with List.Items.Add do begin
        Caption:=Format('%s (%d)',[p.ProcessName,p.PID]);
        if not IsAddressNull(p.LocalAddress) then
          SubItems.Add(p.LocalAddressString)
        else
          SubItems.Add(MachineName);
        if p.LocalPort>0 then
          SubItems.Add(IntToStr(p.LocalPort))
        else
          SubItems.Add('');
        if not IsAddressNull(p.RemoteAddress) then
          SubItems.Add(p.RemoteAddressString)
        else if SameText('UDP',p.Protocol) then
          SubItems.Add('')
        else
          SubItems.Add(MachineName);
        if p.RemotePort>0 then
          SubItems.Add(IntToStr(p.RemotePort))
        else
          SubItems.Add('');
        s:=p.Protocol;
        if p.Typ=AF_INET6 then
          s:=s+'6';
        SubItems.Add(s);
        if SameText('TCP',p.Protocol) then
          case p.State of
            MIB_TCP_STATE_CLOSED: SubItems.Add('Closed');
            MIB_TCP_STATE_LISTEN: SubItems.Add('Listen');
            MIB_TCP_STATE_SYN_SENT: SubItems.Add('SYN sent');
            MIB_TCP_STATE_SYN_RCVD: SubItems.Add('SYN received');
            MIB_TCP_STATE_ESTAB: SubItems.Add('Established');
            MIB_TCP_STATE_FIN_WAIT1: SubItems.Add('FIN wait 1');
            MIB_TCP_STATE_FIN_WAIT2: SubItems.Add('FIN wait 2');
            MIB_TCP_STATE_CLOSE_WAIT: SubItems.Add('Close wait');
            MIB_TCP_STATE_CLOSING: SubItems.Add('Closing');
            MIB_TCP_STATE_LAST_ACK: SubItems.Add('Last ACK');
            MIB_TCP_STATE_TIME_WAIT: SubItems.Add('Time wait');
            MIB_TCP_STATE_DELETE_TCB: SubItems.Add('Delete TCB');
            else SubItems.Add('Unknown');
          end
        else
          SubItems.Add('');
      end;
    end;
  finally
    List.Items.EndUpdate;
  end;
  if List.Items.Count>0 then begin
    List.Items.BeginUpdate;
    try
      r:=List.Items[0].DisplayRect(drBounds);
      List.Scroll(0,vp*(r.Bottom-r.Top));
    finally
      List.Items.EndUpdate;
    end;
  end;
  sb.Panels[0].Text:=Format('%d item(s)',[List.Items.Count]);
end;

end.

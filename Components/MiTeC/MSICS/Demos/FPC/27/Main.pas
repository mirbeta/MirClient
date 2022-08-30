unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, MSI_FW;

type
  TForm2 = class(TForm)
    List: TListView;
    Button2: TButton;
    sd: TSaveDialog;
    Button3: TButton;
    cbxDomain: TCheckBox;
    cbxPublic: TCheckBox;
    cbxPrivate: TCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
    FW: TMiTeC_Firewall;
    procedure RefreshData;
  public
  end;

var
  Form2: TForm2;

implementation

uses MiTeC_Routines, MiTeC_SIF;

{$R *.lfm}

procedure TForm2.RefreshData;
var
  i: Integer;
  s: string;
begin
  Screen.Cursor:=crHourglass;
  try
    FW.RefreshData;
    cbxDomain.Checked:=FW.DomainProfile;
    cbxPublic.Checked:=FW.PublicProfile;
    cbxPrivate.Checked:=FW.PrivateProfile;
    List.Items.Clear;
    for i:=0 to FW.RuleCount-1 do
      with List.Items.Add do begin
        Caption:=FW.Rules[i].Name;
        SubItems.Add(FW.Rules[i].Description);
        SubItems.Add(FW.Rules[i].AppName);
        SubItems.Add(FW.Rules[i].ServiceName);
        case FW.Rules[i].Protocol of
          NET_FW_IP_PROTOCOL_TCP    :s:='TCP';
          NET_FW_IP_PROTOCOL_UDP    :s:='UDP';
          NET_FW_IP_PROTOCOL_ICMPv4 :s:='ICMPv4';
          NET_FW_IP_PROTOCOL_ICMPv6 :s:='ICMPv6';
          else s:=IntToStr(FW.Rules[i].Protocol);
        end;
        SubItems.Add(s);
        SubItems.Add(FW.Rules[i].LocalPorts);
        SubItems.Add(FW.Rules[i].RemotePorts);
        SubItems.Add(FW.Rules[i].LocalAddresses);
        SubItems.Add(FW.Rules[i].RemoteAddresses);
        SubItems.Add(FW.Rules[i].ICMP);
        case FW.Rules[i].Direction of
          NET_FW_RULE_DIR_IN : s:='In';
          NET_FW_RULE_DIR_OUT: s:='Out';
        end;
        SubItems.Add(s);
        SubItems.Add(BoolToStr(FW.Rules[i].Enabled,True));
        SubItems.Add(BoolToStr(FW.Rules[i].Edge,True));
        case FW.Rules[i].Action of
          NET_FW_ACTION_ALLOW: s:='Allow';
          NET_FW_ACTION_BLOCk: s:='Block';
        end;
        SubItems.Add(FW.Rules[i].Grouping);
        SubItems.Add(FW.Rules[i].IntfTypes);
      end;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm2.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm2.Button3Click(Sender: TObject);
var
  wh: Boolean;
begin
  wh:=True;
  sd.FileName:=MachineName+cSIFExt;
  if sd.Execute then
    FW.SaveToStorage(sd.FileName,wh);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  FW:=TMiTeC_Firewall.Create(Self);
  RefreshData;
end;

end.

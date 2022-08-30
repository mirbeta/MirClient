unit IntfDlg;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics,
  Controls, Forms, Dialogs, StdCtrls, ComCtrls, MSI_Network;

type
  TdlgIntf = class(TForm)
    bOK: TButton;
    List: TListView;
  private
  public
  end;

var
  dlgIntf: TdlgIntf;

procedure ShowIntfProps(Adapter: TAdapter);

implementation

uses MiTeC_IpHlpAPI;

{$R *.dfm}

procedure ShowIntfProps(Adapter: TAdapter);
begin
  dlgIntf:=TdlgIntf.Create(Application.Mainform);
  with dlgIntf do
    try
      Caption:=Adapter.Name;
      with List.Items.Add do begin
        Caption:='Description';
        SubItems.Add(Adapter.Name);
      end;
      with List.Items.Add do begin
        Caption:='Alias';
        SubItems.Add(Adapter.Alias);
      end;
      with List.Items.Add do begin
        Caption:='MAC Address';
        SubItems.Add(Adapter.Address);
      end;
      with List.Items.Add do begin
        Caption:='MTU';
        SubItems.Add(IntToStr(Adapter.MTU));
      end;
      with List.Items.Add do begin
        Caption:='Link speed';
        SubItems.Add(Format('%d Mbps',[Adapter.MaxSpeed div 1000000]));
      end;
      with List.Items.Add do begin
        Caption:='Type';
        SubItems.Add(AdapterTypes[Adapter.Typ]);
      end;
      with List.Items.Add do begin
        Caption:='DNS connection suffix';
        SubItems.Add(Adapter.DNSSuffix);
      end;
      with List.Items.Add do begin
        Caption:='DHCP Enabled';
        SubItems.Add(BoolToStr(Adapter.EnableDHCP,True));
      end;
      with List.Items.Add do begin
        Caption:='IPv4';
        SubItems.Add(Adapter.IPAddress.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv4 Mask';
        SubItems.Add(Adapter.IPAddressMask.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv4 DHCP Servers';
        SubItems.Add(Adapter.DHCP_IPAddress.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv4 Default Gateway';
        SubItems.Add(Adapter.Gateway_IPAddress.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv4 DNS Servers';
        SubItems.Add(Adapter.DNSServers.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv4 WINS Servers';
        SubItems.Add(Adapter.PrimaryWINS_IPAddress.CommaText);
      end;

      with List.Items.Add do begin
        Caption:='IPv6';
        SubItems.Add(Adapter.IPv6Address.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv6 DHCP Servers';
        SubItems.Add(Adapter.DHCP_IPv6.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv6 Default Gateway';
        SubItems.Add(Adapter.Gateway_IPv6.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv6 DNS Servers';
        SubItems.Add(Adapter.DNSServers_IPv6.CommaText);
      end;
      with List.Items.Add do begin
        Caption:='IPv6 WINS Servers';
        SubItems.Add(Adapter.PrimaryWINS_IPv6.CommaText);
      end;
      ShowModal;
    finally
      Free;
    end;
end;

end.

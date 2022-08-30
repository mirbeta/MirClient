unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Network, MiTeC_IpHlpAPI, ImgList, StdCtrls, ComCtrls, MiTeC_NetChangeNotify;

type
  TwndMain = class(TForm)
    bOK: TButton;
    List: TListView;
    ImageList: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
    procedure NetworkChangeNotifierChange(Sender: TObject; ARes: Cardinal);
    procedure ListDblClick(Sender: TObject);
  private
    FNetwork: TMiTeC_Network;
    FNetChangeNotifier: TNetworkChangeNotifier;
  public
    procedure RefreshData;
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_NetUtils, IntfDlg;

{$R *.dfm}

procedure TwndMain.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TwndMain.FormCreate(Sender: TObject);
begin
  FNetwork:=TMiTeC_Network.Create(Self);
  RefreshData;
  FNetChangeNotifier:=TNetworkChangeNotifier.Create;
  FNetChangeNotifier.OnChange:=NetworkChangeNotifierChange;
end;


procedure TwndMain.ListDblClick(Sender: TObject);
begin
  if not Assigned(List.Selected) then
    exit;
  ShowIntfProps(FNetwork.TCPIP.Adapter[List.Selected.Index]);
end;

procedure TwndMain.NetworkChangeNotifierChange(Sender: TObject; ARes: Cardinal);
begin
  RefreshData;
end;

procedure TwndMain.RefreshData;
var
  i: Integer;
begin
  FNetwork.RefreshData;
  List.Items.BeginUpdate;
  try
    List.Items.Clear;
    for i:=0 to FNetwork.TCPIP.AdapterCount-1 do
      with List.Items.Add do begin
        Caption:=FNetwork.TCPIP.Adapter[i].Name;
        SubItems.Add(FNetwork.TCPIP.Adapter[i].IPAddress.CommaText);
        SubItems.Add(FNetwork.TCPIP.Adapter[i].IPAddressMask.CommaText);
        SubItems.Add(FNetwork.TCPIP.Adapter[i].Address);
        SubItems.Add(AdapterTypes[FNetwork.TCPIP.Adapter[i].Typ]);
        SubItems.Add(GetIntfStatStr(FNetwork.TCPIP.Adapter[i].OperStatus));
        SubItems.Add(GetIntfAdminStr(FNetwork.TCPIP.Adapter[i].AdminStatus));
        SubItems.Add(IntToStr(FNetwork.TCPIP.Adapter[i].MaxSpeed div 1000000));
        SubItems.Add(IntToStr(FNetwork.TCPIP.Adapter[i].MTU));
        if FNetwork.TCPIP.BestInterfaceIdx=FNetwork.TCPIP.Adapter[i].IntfIdx then
          ImageIndex:=0
        else
          ImageIndex:=-1;
      end;
  finally
    List.Items.EndUpdate;
  end;
end;

end.

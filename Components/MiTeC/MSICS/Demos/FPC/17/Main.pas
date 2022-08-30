unit Main;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Network, MiTeC_IpHlpAPI, ImgList, StdCtrls, ComCtrls;

type
  TwndMain = class(TForm)
    bOK: TButton;
    List: TListView;
    procedure FormCreate(Sender: TObject);
    procedure bOKClick(Sender: TObject);
  private
    FNetwork: TMiTeC_Network;
  public
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_NetUtils;

{$IFnDEF FPC}
  {$R *.dfm}
{$ELSE}
  {$R *.lfm}
{$ENDIF}

procedure TwndMain.bOKClick(Sender: TObject);
begin
  Close;
end;

procedure TwndMain.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  FNetwork:=TMiTeC_Network.Create(Self);
  FNetwork.RefreshData;
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
    end;
end;


end.

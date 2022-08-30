{$INCLUDE ..\..\..\Compilers.inc}

unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_WIFI, ComCtrls, ExtCtrls, StdCtrls, ImgList, MSI_Common;

type
  TwndMain = class(TForm)
    lv: TListView;
    Timer: TTimer;
    ilWIFI: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    WIFI: TMiTeC_WIFI;
  public
    procedure RefreshData;
  end;

var
  wndMain: TwndMain;

implementation

uses MiTeC_CRC;

{$R *.lfm}

procedure TwndMain.FormCreate(Sender: TObject);
begin
  WIFI:=TMiTeC_WIFI.Create(Self);
  RefreshData;
end;

procedure TwndMain.RefreshData;
var
  i: Integer;
  n: TListItem;
  s: string;
begin
  WIFI.RefreshData;
  lv.Items.BeginUpdate;
  try
    //lv.Items.Clear;
    for i:=0 to lv.Items.Count-1 do
      lv.Items[i].SubItems[0]:='0%';
    for i:=0 to WIFI.NetworkCount-1 do begin
      s:=WIFI.Networks[i].SSID;
      if (s='') then
        s:=WIFI.Networks[i].Profile;
      n:=lv.FindData(0,Pointer(WIFI.Networks[i].ID),True,True);
      if not Assigned(n) then begin
        n:=lv.Items.Add;
        n.Caption:=s;
        n.SubItems.Add(Format('%d%%',[WIFI.Networks[i].SignalQuality]));
        n.SubItems.Add(AuthToStr(WIFI.Networks[i].AuthAlgorithm));
        n.SubItems.Add(CipherToStr(WIFI.Networks[i].CipherAlgorithm));
        n.SubItems.Add(PHYToStr(WIFI.Networks[i].PHYType));
        n.SubItems.Add(BSSToStr(WIFI.Networks[i].BSSType));
        n.SubItems.Add(WIFI.Networks[i].MACAddress);
        n.SubItems.Add(Format('%d dBm',[WIFI.Networks[i].RSSI]));
        n.SubItems.Add(Format('%1.3f GHz',[WIFI.Networks[i].ChannelFreq/1000000]));
        n.SubItems.Add(Format('%d',[GetChannelNumber(WIFI.Networks[i].ChannelFreq)]));
        n.SubItems.Add(Format('%d Mbps',[WIFI.Networks[i].MaxSpeed]));
        n.SubItems.Add(WIFI.Networks[i].Intf.Name);
        n.Data:=Pointer(WIFI.Networks[i].ID);
      end else begin
        n.SubItems[0]:=Format('%d%%',[WIFI.Networks[i].SignalQuality]);
        n.SubItems[5]:=WIFI.Networks[i].MACAddress;
        n.SubItems[6]:=Format('%d dBm',[WIFI.Networks[i].RSSI]);
        n.SubItems[7]:=Format('%1.3f GHz',[WIFI.Networks[i].ChannelFreq/1000000]);
        n.SubItems[8]:=Format('%d',[GetChannelNumber(WIFI.Networks[i].ChannelFreq)]);
        n.SubItems[9]:=Format('%d Mbps',[WIFI.Networks[i].MaxSpeed]);
      end;
      n.ImageIndex:=Integer(WIFI.Networks[i].SecurityEnabled);
      if WIFI.Networks[i].Connected then
        n.ImageIndex:=n.ImageIndex+2;
    end;
  finally
    lv.Items.EndUpdate;
  end;
end;

procedure TwndMain.TimerTimer(Sender: TObject);
begin
  RefreshData;
end;

end.

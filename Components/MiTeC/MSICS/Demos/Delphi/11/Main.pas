unit Main;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MiTeC_BTAPI, StdCtrls, ComCtrls, ExtCtrls;

type
  TForm2 = class(TForm)
    Bevel1: TBevel;
    bRefresh: TButton;
    bClose: TButton;
    lv: TListView;
    procedure FormCreate(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
    procedure bCloseClick(Sender: TObject);
  private
  public
  end;

var
  Form2: TForm2;

implementation

uses
  MSI_BT, MiTeC_Datetime, MiTeC_StrUtils;

{$R *.dfm}

procedure TForm2.bCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TForm2.cmRefresh(Sender: TObject);
var
  i: Integer;
begin
  Screen.Cursor:=crHourglass;
  with TMiTeC_BT.Create(Self) do
    try
      lv.Items.Clear;
      RefreshData;
      for i:=0 to DeviceCount-1 do
        with lv.Items.Add do begin
          Caption:=Devices[i].Name;
          SubItems.Add(Devices[i].Address);
          SubItems.Add(DatetimeToStrDef(devices[i].LastUsed,''));
          SubItems.Add(DatetimeToStrDef(devices[i].LastSeen,''));
          SubItems.Add(BooleanEn[Devices[i].Authenticated]);
          SubItems.Add(BooleanEn[Devices[i].Remembered]);
          SubItems.Add(BooleanEn[Devices[i].Connected]);
        end;
    finally
      Free;
      Screen.Cursor:=crDefault;
    end;
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  cmrefresh(nil);
end;

end.

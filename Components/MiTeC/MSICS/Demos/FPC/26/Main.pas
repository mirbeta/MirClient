unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, MSI_WLANC;

type
  TForm2 = class(TForm)
    List: TListView;
    Button2: TButton;
    sd: TSaveDialog;
    Button3: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure ListDblClick(Sender: TObject);
  private
    WLANC: TMiTeC_WLANC;
    procedure RefreshData;
  public
  end;

var
  Form2: TForm2;

implementation

uses MiTeC_Routines, MiTeC_Datetime, MiTeC_SIF, Clipbrd;

{$R *.lfm}

procedure TForm2.RefreshData;
var
  i: Integer;
begin
  Screen.Cursor:=crHourglass;
  try
    WLANC.RefreshData;
    List.Items.Clear;
    for i:=0 to WLANC.RecordCount-1 do
      with List.Items.Add do begin
        Caption:=WLANC.Records[i].SSID;
        SubItems.Add(WLANC.Records[i].Key);
        SubItems.Add(WLANC.Records[i].Authentication);
        SubItems.Add(WLANC.Records[i].Encryption);
        SubItems.Add(WLANC.Records[i].Connection);
        SubItems.Add(WLANC.Records[i].AdapterName);
        SubItems.Add(WLANC.Records[i].IPAddress);
        SubItems.Add(DateTimeToStr(UTCToLocalDatetime(WLANC.Records[i].Timestamp)));
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
    WLANC.SaveToStorage(sd.FileName,wh);
end;

procedure TForm2.FormCreate(Sender: TObject);
begin
  WLANC:=TMiTeC_WLANC.Create(Self);
  RefreshData;
end;

procedure TForm2.ListDblClick(Sender: TObject);
begin
  if not Assigned(List.Selected) then
    Exit;
  Clipboard.AsText:=List.Selected.SubItems[0];
end;

end.

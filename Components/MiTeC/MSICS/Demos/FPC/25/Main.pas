unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, MSI_USBHistory, StdCtrls;

type
  TForm3 = class(TForm)
    lv: TListView;
    Button3: TButton;
    Button2: TButton;
    sd: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure lvCompare(Sender: TObject; Item1, Item2: TListItem; Data: Integer;
      var Compare: Integer);
    procedure Button3Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
  private
  public
    USBHistory: TMiTeC_USBHistory;
  end;

var
  Form3: TForm3;

implementation

uses MiTeC_SIF, MiTeC_Routines, MiTeC_StrUtils;

{$R *.lfm}

procedure TForm3.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm3.Button3Click(Sender: TObject);
var
  wh: Boolean;
begin
  wh:=True;
  sd.FileName:=MachineName+cSIFExt;
  if sd.Execute then
    USBHistory.SaveToStorage(sd.FileName,wh);
end;

procedure TForm3.FormCreate(Sender: TObject);
var
  i: Integer;
begin
  USBHistory:=TMiTeC_USBHistory.Create(Self);
  USBHistory.RefreshData;
  for i:=0 to USBHistory.RecordCount-1 do
    with lv.Items.Add do begin
      Caption:=USBHistory.Records[i].Name;
      Subitems.Add(USBHistory.Records[i].SerialNumber);
      SubItems.Add(DateTimeToStr(USBHistory.Records[i].Timestamp));
      Subitems.Add(USBHistory.Records[i].DeviceClass);
    end;
  lv.AlphaSort;
end;

procedure TForm3.lvCompare(Sender: TObject; Item1, Item2: TListItem;
  Data: Integer; var Compare: Integer);
var
  v1,v2: Variant;
begin
  v1:=StrTodatetimeDef(Item1.SubItems[1],0);
  v2:=StrTodatetimeDef(Item2.SubItems[1],0);
  Compare:=CustomSort(v1,v2);
end;

end.

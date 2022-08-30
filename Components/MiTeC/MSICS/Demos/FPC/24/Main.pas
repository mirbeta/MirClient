unit Main;

{$MODE Delphi}

interface

uses
  MSI_Defs,
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, MSI_NetCreds, ComCtrls;

type
  TForm1 = class(TForm)
    List: TListView;
    Button1: TButton;
    sd: TSaveDialog;
    Button2: TButton;
    procedure FormCreate(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure cmRefresh(Sender: TObject);
  private
    NC: TMiTeC_NetCreds;
  public
  end;

var
  Form1: TForm1;

implementation

uses
  MiTeC_Routines, MiTeC_SIF;

{$R *.lfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  wh: Boolean;
begin
  wh:=True;
  sd.FileName:=MachineName+cSIFExt;
  if sd.Execute then
    NC.SaveToStorage(sd.FileName,wh);
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  Close;
end;

procedure TForm1.cmRefresh(Sender: TObject);
var
  i: Integer;
begin
  NC:=TMiTeC_NetCreds.Create(Self);
  NC.RefreshData;
  List.Items.Clear;
  for i:=0 to NC.RecordCount-1 do
    with List.Items.Add do begin
      case NC.Records[i].Typ of
        1: Caption:='Generic';
        2: Caption:='Domain Password';
        3: Caption:='Domain Certificate';
        4: Caption:='Domain Visible Password';
        5: Caption:='Generic Certificate';
        6: Caption:='Domain Extended';
        7: Caption:='Maximum';
        1007: Caption:='Maximum Extended';
        else Caption:=IntToStr(NC.Records[i].Typ);
      end;
      SubItems.Add(DateTimeToStr(NC.Records[i].Timestamp));
      SubItems.Add(NC.Records[i].Target);
      SubItems.Add(NC.Records[i].Username);
      SubItems.Add(NC.Records[i].Password);
    end;

  Caption:=Format('Network Credentials - %d items',[List.Items.Count]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  cmRefresh(nil);
end;

end.

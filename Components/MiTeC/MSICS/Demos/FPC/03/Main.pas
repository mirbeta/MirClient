unit Main;

{$MODE Delphi}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, MSI_Software, MSI_MSProduct, ComCtrls, StdCtrls, ImgList, ExtCtrls;

type
  TForm1 = class(TForm)
    Panel1: TPanel;
    bSave: TButton;
    bLoad: TButton;
    od: TOpenDialog;
    sd: TSaveDialog;
    Button1: TButton;
    pc: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    lv: TListView;
    lvMSP: TListView;
    procedure FormCreate(Sender: TObject);
    procedure bSaveClick(Sender: TObject);
    procedure bLoadClick(Sender: TObject);
    procedure Button1Click(Sender: TObject);
  private
    SW: TMiTeC_Software;
    MS: TMiTeC_MSProduct;
  public
    procedure DisplayData;
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

procedure TForm1.bLoadClick(Sender: TObject);
var
  rh: Boolean;
begin
  rh:=True;
  if not od.Execute then
    Exit;
  lv.Items.Clear;
  SW.LoadFromStorage(od.FileName,rh);
  DisplayData;
end;

procedure TForm1.bSaveClick(Sender: TObject);
var
  wh: Boolean;
begin
  wh:=True;
  if not sd.Execute then
    Exit;
  SW.SaveToStorage(sd.FileName,wh);
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  lv.Items.Clear;
  SW.RefreshData;
  MS.RefreshData;
  DisplayData;
end;

procedure TForm1.DisplayData;
var
  i: Integer;
begin
  with lv.Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to SW.Count-1 do
          with Add do begin
            Caption:=SW.InstallEntry[i].Name;
            SubItems.Add(SW.InstallEntry[i].Version);
            SubItems.Add(SW.InstallEntry[i].Company);
            SubItems.Add(DateToStr(SW.InstallEntry[i].InstallDate));
            SubItems.Add(SW.InstallEntry[i].Uninstall);
          end;
      finally
        EndUpdate;
      end;
    end;
  Tabsheet1.Caption:=Format('Installed Software - %d records',[SW.Count]);

  with lvMSP.Items do begin
      BeginUpdate;
      try
        Clear;
        for i:=0 to MS.ProductCount-1 do
          with Add do begin
            Caption:=MS.Products[i].Name;
            SubItems.Add(MS.Products[i].ProductID);
            SubItems.Add(MS.Products[i].ProductKey);
            SubItems.Add(MS.Products[i].RegistryPath);
          end;
      finally
        EndUpdate;
      end;
    end;
  Tabsheet2.Caption:=Format('MS Products - %d records',[MS.ProductCount]);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  {$IFDEF WIN64}
  Caption:=Caption+' (x64)';
  {$ENDIF}
  SW:=TMiTeC_Software.Create(nil);
  MS:=TMiTeC_MSProduct.Create(Self);
  pc.ActivePageIndex:=0;
  Button1Click(nil);
end;

end.

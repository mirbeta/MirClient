unit Main;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs,
  MSI_DriveContent, Vcl.StdCtrls, Vcl.ComCtrls;

type
  TwndMain = class(TForm)
    cbDrive: TComboBox;
    bScan: TButton;
    sb: TStatusBar;
    procedure FormCreate(Sender: TObject);
    procedure cbDriveChange(Sender: TObject);
    procedure DCFolder(Sender: TObject;
      var FileInfo: TFileRecord; var Accept: Boolean);
    procedure DCFile(Sender: TObject;
      var FileInfo: TFileRecord; var Accept: Boolean);
    procedure bScanClick(Sender: TObject);
  private
  public
    DC: TMiTeC_DriveContent;
  end;

var
  wndMain: TwndMain;

implementation

{$R *.dfm}

procedure TwndMain.bScanClick(Sender: TObject);
var
  b: boolean;
begin
  Screen.Cursor:=crHourglass;
  bScan.Enabled:=False;
  try
    DC.Scan(cbDrive.Text);
    DC.SaveToStorage('DriveContent.sif',b);
    DC.Content.SaveToFile('DriveContent.csv');
  finally
    bScan.Enabled:=True;
    sb.Panels[1].Text:='';
    Screen.Cursor:=crDefault;
  end;
end;

procedure TwndMain.cbDriveChange(Sender: TObject);
begin
  bScan.Enabled:=cbDrive.ItemIndex>-1;
end;

procedure TwndMain.FormCreate(Sender: TObject);
var
  c: char;
begin
  DC:=TMiTeC_DriveContent.Create(Self);
  DC.OnFile:=DCFile;
  DC.OnFolder:=DCFolder;
  DC.MD5Extensions:='exe,dll';
  DC.RefreshData;
  for c in DC.AvailableDisks do
    cbDrive.Items.Add(c+':');
end;

procedure TwndMain.DCFile(Sender: TObject;
  var FileInfo: TFileRecord; var Accept: Boolean);
begin
  sb.Panels[0].Text:=IntToStr(DC.Content.Count);
  sb.Update;
end;

procedure TwndMain.DCFolder(Sender: TObject;
  var FileInfo: TFileRecord; var Accept: Boolean);
begin
  sb.Panels[1].Text:=FileInfo.ObjectName;
  sb.Update;
end;

end.

unit Unit1;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ExtCtrls, OleCtrls, SHDocVw;

const
  WM_REFRESH = WM_USER+1000;
type
  TForm1 = class(TForm)
    Panel1: TPanel;
    Icon: TImage;
    Label1: TLabel;
    Button1: TButton;
    WebBrowser: TWebBrowser;
    procedure Button1Click(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  protected
    procedure WMREFRESH(var AMsg: TMessage); message WM_REFRESH;
  public
    procedure RefreshData;
  end;

var
  Form1: TForm1;

implementation

uses
  MSIC_Intf, MSI_Defs;

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  s: array[0..MAX_PATH+1] of char;
begin
  Screen.Cursor:=crHourGlass;
  try
    StrPCopy(@s,ChangeFileExt(Application.ExeName,cSIFExt));
    SaveToStorage(SO_All,PChar(@s));
    MessageDlg('MiTeC System Information File was created.',mtInformation,[mbOK],0);
  finally
    Screen.Cursor:=crDefault;
  end;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  Icon.Picture.Icon.Handle:=Application.Icon.Handle;
  MSIC_DLL:=InitMSICDLL('MSIC.DLL');
  if MSIC_DLL=0 then
    MSIC_DLL:=InitMSICDLL('..\MSIC.DLL');
end;

procedure TForm1.FormShow(Sender: TObject);
begin
  PostMessage(Handle,WM_REFRESH,0,0);
end;

procedure TForm1.RefreshData;
var
  s: array[0..MAX_PATH+1] of char;
begin
  if Assigned(GenerateXMLReport) then begin
    StrPCopy(@s,ChangeFileExt(Application.ExeName,'.xml'));
    GenerateXMLReport(SO_Machine+SO_OS,PChar(@s));
    WebBrowser.Navigate(string(s));
  end else
    MessageDlg('Library not found.',mtError,[mbOK],0);
end;

procedure TForm1.WMREFRESH(var AMsg: TMessage);
begin
  Screen.Cursor:=crHourglass;
  try
    RefreshData;
  finally
    Screen.Cursor:=crDefault;
  end;
end;

end.

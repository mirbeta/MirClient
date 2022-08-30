unit frmWebBroser;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, OleCtrls, Buttons, ExtCtrls, SHDocVw, StdCtrls, MShare;

type
  TfrmWebBrowser = class(TForm)
    Panel1: TPanel;
    sbtnWebBrowserClose: TSpeedButton;
    WebBrowser: TWebBrowser;
    SpeedButton1: TSpeedButton;
    SpeedButton2: TSpeedButton;
    procedure FormCreate(Sender: TObject);
    procedure WebBrowserWindowClosing(ASender: TObject; IsChildWindow: WordBool; var Cancel: WordBool);
    procedure sbtnWebBrowserCloseClick(Sender: TObject);
    procedure WebBrowserNewWindow2(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
    procedure SpeedButton1Click(Sender: TObject);
    procedure SpeedButton2Click(Sender: TObject);
  private
    { Private declarations }
  public
    procedure Open(sWeb: string);
    { Public declarations }
  end;

var
  frmWebBrowser             : TfrmWebBrowser;

implementation

uses ClMain;

{$R *.dfm}

procedure TfrmWebBrowser.Open(sWeb: string);
begin
  WebBrowser.Navigate(sWeb);
  Show;
end;

procedure TfrmWebBrowser.FormCreate(Sender: TObject);
begin
  Top := 0;
  Left := 0;
  Height := SCREENHEIGHT;
  Width := SCREENWIDTH;
  Windows.SetParent(Handle, FrmMain.Handle);
  //ParentWindow := frmMain.ClientHandle;
end;

procedure TfrmWebBrowser.WebBrowserNewWindow2(ASender: TObject; var ppDisp: IDispatch; var Cancel: WordBool);
var
  NewApp                    : TfrmWebBrowser;
begin
  NewApp := TfrmWebBrowser.Create(Self);
  NewApp.ParentWindow := frmWebBrowser.Handle;
  NewApp.Show;
  NewApp.SetFocus;
  ppDisp := NewApp.WebBrowser.Application;
end;

procedure TfrmWebBrowser.WebBrowserWindowClosing(ASender: TObject;
  IsChildWindow: WordBool; var Cancel: WordBool);
begin
  Cancel := True;
  Close;
end;

procedure TfrmWebBrowser.sbtnWebBrowserCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TfrmWebBrowser.SpeedButton1Click(Sender: TObject);
begin
  WebBrowser.GoBack;
end;

procedure TfrmWebBrowser.SpeedButton2Click(Sender: TObject);
begin
  WebBrowser.GoForward;
end;

end.


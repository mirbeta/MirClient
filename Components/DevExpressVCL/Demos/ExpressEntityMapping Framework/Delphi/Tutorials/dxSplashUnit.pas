unit dxSplashUnit;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Forms, SysUtils, Dialogs, Graphics, Classes, Controls, cxGraphics, cxControls, cxLookAndFeels,
  cxLookAndFeelPainters, cxContainer, cxEdit, cxImage, cxLabel, cxGroupBox;

type
  TfrmSplash = class(TForm)
    GroupBox1: TcxGroupBox;
    cxLabel1: TcxLabel;
    Image1: TcxImage;
  end;

  procedure dxSetSplashVisibility(AVisible: Boolean; const ACaption: string = '');

implementation

{$R *.dfm}

var
  dxSplash: TfrmSplash;

procedure dxSetSplashVisibility(AVisible: Boolean; const ACaption: string = '');
var
  I: Integer;
begin
  if AVisible then
  begin
    if dxSplash = nil then
      dxSplash := TfrmSplash.Create(Application);
    if Application.MainForm <> nil then
      for I := 0 to 1 do // because of per-monitor dpi support
      begin
        dxSplash.Left := Application.MainForm.Left + (Application.MainForm.Width - dxSplash.Width) div 2;
        dxSplash.Top := Application.MainForm.Top + (Application.MainForm.Height - dxSplash.Height) div 2;
      end;
    dxSplash.Show;
    dxSplash.Update;
  end
  else
    FreeAndNil(dxSplash);
end;


initialization
  dxSplash := nil;

end.

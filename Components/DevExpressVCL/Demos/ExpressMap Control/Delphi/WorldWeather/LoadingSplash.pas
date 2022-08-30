unit LoadingSplash;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, cxControls, cxContainer, cxEdit, cxLabel, ExtCtrls, cxGraphics,
  cxLookAndFeels, cxLookAndFeelPainters;

type
  TfrmLoading = class(TForm)
    Panel1: TPanel;
    cxLabel1: TcxLabel;
    cxLabel2: TcxLabel;
    Image1: TImage;
  private
  public
  end;

  procedure cxSetSplashVisibility(AVisible: Boolean; const ACaption: string = '');

implementation

{$R *.dfm}

var
  ASplash: TfrmLoading;

procedure cxSetSplashVisibility(AVisible: Boolean; const ACaption: string = '');
begin
  if AVisible then
  begin
    ASplash := TfrmLoading.Create(Application);
    ASplash.cxLabel2.Caption := Format('Loading: %s', [ACaption]);
    ASplash.Show;
    ASplash.Update;
  end
  else
    ASplash.Free;
end;


end.

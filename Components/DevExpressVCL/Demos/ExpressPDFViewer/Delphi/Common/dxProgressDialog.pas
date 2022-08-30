unit dxProgressDialog;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms, Dialogs, StdCtrls, cxGraphics,
  cxControls, cxClasses, cxLookAndFeels, cxLookAndFeelPainters, cxContainer, cxEdit, cxProgressBar, cxLabel, Menus,
  cxButtons;

type
  { TfrmProgress }

  TfrmProgress = class(TForm, IcxProgress)
    lbTitle: TcxLabel;
    pbProgress: TcxProgressBar;
  private
    // IcxProgress
    procedure OnProgress(Sender: TObject; Percent: Integer);
  end;

implementation

{$R *.dfm}

{ TfrmProgress }

procedure TfrmProgress.OnProgress(Sender: TObject; Percent: Integer);
begin
  pbProgress.Position := Percent;
  Application.ProcessMessages;
end;

end.

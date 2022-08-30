unit BandedDemoBands;

interface

uses
  Windows, Messages, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, cxButtons, cxListBox, cxLookAndFeelPainters,
  cxControls, cxContainer;

type
  TBandedDemoBandsForm = class(TForm)
    lbBands: TcxListBox;
    btnOK: TcxButton;
    btnCancel: TcxButton;
    procedure btnCancelClick(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TBandedDemoBandsForm.btnCancelClick(Sender: TObject);
begin
  Close;
end;

end.

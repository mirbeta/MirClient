unit UnboundModeDemoCustomField;

interface

uses
{$IFDEF CLR}
  System.ComponentModel,
{$ENDIF}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, cxButtons, cxLookAndFeelPainters;

type
  TUnboundModeDemoCustomFieldForm = class(TForm)
    lbHeight: TLabel;
    lbWidth: TLabel;
    lbMineCount: TLabel;
    edtHeight: TEdit;
    edtWidth: TEdit;
    edtMineCount: TEdit;
    btnOK: TcxButton;
    bntCancel: TcxButton;
    procedure edtKeyPress(Sender: TObject; var Key: Char);
  public
    function ShowModal: Integer; override;
  end;

implementation

uses UnboundModeDemoTypes;

{$R *.DFM}

function TUnboundModeDemoCustomFieldForm.ShowModal: Integer;
begin
  SetFormPosition(Self, 25, 25);
  Result := inherited ShowModal;
end;

procedure TUnboundModeDemoCustomFieldForm.edtKeyPress(Sender: TObject;
  var Key: Char);
begin
  if (Key < '0') or ('9' < Key) then
    Key := Char(7);
end;

end.

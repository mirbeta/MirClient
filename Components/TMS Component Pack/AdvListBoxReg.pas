unit AdvListBoxReg;

interface

uses
  Classes, AdvListBox;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Edits',[TAdvListBox]);
end;

end.

unit AdvSmoothDatePickerReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothCalendar, AdvSmoothDatePicker, AdvSmoothEditButton;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls', [TAdvSmoothDatePicker]);
  RegisterClass(TAdvSmoothEditBtn);
end;

end.

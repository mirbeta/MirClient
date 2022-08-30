unit AdvSmoothImageListBoxPickerReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothImageListBox, AdvSmoothImageListBoxPicker, AdvSmoothEditButton;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls', [TAdvSmoothImageListBoxPicker]);
  RegisterClass(TAdvSmoothEditBtn);
end;

end.

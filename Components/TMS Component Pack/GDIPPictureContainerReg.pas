unit GDIPPictureContainerReg;

interface

uses
  GDIPPictureContainer, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls',[TGDIPPictureContainer]);
end;

end.

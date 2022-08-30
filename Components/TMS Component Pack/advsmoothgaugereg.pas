unit AdvSmoothGaugeReg;

interface

uses
  Classes, AdvSmoothGauge;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls',[TAdvSmoothGauge]);
end;


end.



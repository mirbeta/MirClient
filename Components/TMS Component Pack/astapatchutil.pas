unit AstaPatchUtil;

interface

{$R ASTAPATCHUTIL.RES}

uses
  Classes, WUpdate;

type

  TAstaPatchUtility = class(TWebUpdateUtility)
  end;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web',[TAstaPatchUtility]);
end;

end.

unit PlugPin;

interface

uses
  Classes, SysUtils;

type
  TPlugPin = class(TThread)
  private
    { Private declarations }
  protected
    procedure Execute; override;
  public
    constructor Create(CreateSuspended: Boolean); dynamic;
  end;
var
  PinEngine                 : TPlugPin;
implementation

constructor TPlugPin.Create(CreateSuspended: Boolean);
begin
  inherited;
end;

procedure TPlugPin.Execute;
var
  i, j                      : Integer;
begin
  { Place thread code here }
  while (not Terminated) do begin
    i := 100;
    Inc(i);
    Sleep(100);
  end;
  j := 10;
end;

end.

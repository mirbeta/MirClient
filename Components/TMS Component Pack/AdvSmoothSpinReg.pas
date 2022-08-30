{ ************************************************************************* }
{ TAdvSpinSelector Reg }
{ for Delphi & C++Builder }
{ }
{ written by TMS Software }
{ copyright ©  2009 }
{ Email : info@tmssoftware.com }
{ Web : http://www.tmssoftware.com }
{ }
{ The source code is given as is. The author is not responsible }
{ for any possible damage done due to the use of this code. }
{ The component can be freely used in any application. The complete }
{ source code remains property of the author and may not be distributed, }
{ published, given or sold in any form as such. No parts of the source }
{ code can be included in any other component or application without }
{ written authorization of the author. }
{ ************************************************************************* }
unit AdvSmoothSpinReg;

{$I TMSDEFS.INC}

interface

uses
  Classes, AdvSmoothSpin, contnrs, SysUtils;

  procedure Register;

implementation

procedure Register;
{$IFDEF DELPHI7_LVL}
var
  l: TClassFinder;
  found: boolean;
{$ENDIF}
begin
{$IFDEF DELPHI7_LVL}
  l := TClassFinder.Create;
  found := l.GetClass('TAdvSmoothSpinEdit') <> nil;
  l.Free;
  if not found then
{$ENDIF}
    RegisterClass(TAdvSmoothSpinEdit);
end;

end.

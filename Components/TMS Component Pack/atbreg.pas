{*************************************************************************}
{ TADVTOOLBUTON component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}
{$I TMSDEFS.INC}
unit atbreg;

interface

uses
  Classes, AdvToolBtn;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS',[TAdvToolButton, TAdvRepeatButton]);
end;

end.

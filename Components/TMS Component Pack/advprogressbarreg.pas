{********************************************************************}
{ TADVPROGRESSBAR component                                          }
{ for Delphi 5.0,6.0,7.0,2005 & C++Builder 5.0,6.0                   }
{                                                                    }
{ written by                                                         }
{       TMS Software                                                 }
{       copyright © 2003 - 2012                                      }
{       Email : info@tmssoftware.com                                 }
{       Web : http://www.tmssoftware.com                             }
{********************************************************************}

unit AdvProgressBarReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, AdvProgressBar;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvProgressBar]);
end;

end.

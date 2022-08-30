{*******************************************************************}
{ TAdvSmoothCalculator component                                    }
{ for Delphi                                                        }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2009                                               }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{*******************************************************************}

unit AdvSmoothCalculatorReg;

interface

uses
  Classes, AdvSmoothCalculator, AdvSmoothCalculatorDropDown;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS SmoothControls',[TAdvSmoothCalculator]);
  RegisterComponents('TMS SmoothControls',[TAdvSmoothCalculatorDropDown]);
end;

end.


{*********************************************************************}
{ TGradientLabel component                                            }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2012                                                   }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{*********************************************************************}

unit GradientLblReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, GradientLabel;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TGradientLabel]);
end;



end.


{*********************************************************************}
{ TEllipsLabel component                                              }
{ for Delphi & C++Builder                                             }
{                                                                     }
{ written by                                                          }
{  TMS Software                                                       }
{  copyright © 2012                                                   }
{  Email : info@tmssoftware.com                                       }
{  Web : http://www.tmssoftware.com                                   }
{*********************************************************************}

unit EllipsLblReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, EllipsLabel;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TEllipsLabel]);
end;



end.


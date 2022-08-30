{**********************************************************************}
{ TADVCOLUMNGRID component                                             }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by TMS Software                                              }
{            copyright © 1996-2012                                     }
{            Email : info@tmssoftware.com                              }
{            Web : http://www.tmssoftware.com                          }
{**********************************************************************}
{$I TMSDEFS.INC}

unit asgcreg;


interface

uses
  Classes, AdvCGrid;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids', [TAdvColumnGrid]);
end;

end.


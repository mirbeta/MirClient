{**********************************************************************}
{ TADVSPREADGRID component                                             }
{ for Delphi & C++Builder                                              }
{                                                                      }
{ written by TMS Software                                              }
{            copyright © 1998-2014                                     }
{            Email : info@tmssoftware.com                              }
{            Web : http://www.tmssoftware.com                          }
{**********************************************************************}

{$I TMSDEFS.INC}

unit aspgreg;

interface

uses
  Classes, AdvPars, AdvSprd;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids', [TAdvSpreadGrid]);
  RegisterComponents('TMS Grids', [TGridBinder]);
  RegisterComponents('TMS Grids', [TLibBinder]);  
end;



end.


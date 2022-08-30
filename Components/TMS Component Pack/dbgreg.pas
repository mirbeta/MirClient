{*******************************************************************}
{ TDBADVSTRINGGRID component                                        }
{ for Delphi & C++Builder                                           }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 1999-2012                                          }
{    Email : info@tmssoftware.com                                   }
{    Web : http://www.tmssoftware.com                               }
{                                                                   }
{*******************************************************************}

unit dbgreg;

interface

{$I TMSDEFS.INC}

uses
  Classes, DBAdvGrd;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids', [TDBAdvStringGrid]);
end;


end.

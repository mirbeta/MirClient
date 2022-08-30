{********************************************************************}
{ TCALCOMP component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by TMS Software                                            }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit calcreg;

{$I TMSDEFS.INC}
interface

uses
  Calcomp, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TCalComp]);
end;


end.


{********************************************************************}
{ TCALPANEL component                                                }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit calpreg;

{$I TMSDEFS.INC}
interface

uses
  CalPanel,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TCalPanel]);
end;



end.
 


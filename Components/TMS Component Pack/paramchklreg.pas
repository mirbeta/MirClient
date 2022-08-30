{********************************************************************}
{ TPARAMCHECKLIST component                                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit paramchklreg;
{$I TMSDEFS.INC}
interface

uses
  Paramchklist, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Param', [TParamCheckList]);
end;

end.


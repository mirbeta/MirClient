{********************************************************************}
{ TPARAMLISTBOX component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit paramlstreg;
{$I TMSDEFS.INC}
interface

uses
  Paramlistbox, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Param', [TParamListBox]);
end;



end.


{********************************************************************}
{ TBUTTONLISTBOX component                                           }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2001 - 2015                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit btnlireg;

{$I TMSDEFS.INC}

interface

uses
  BtnListb, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TButtonListbox]);
end;



end.


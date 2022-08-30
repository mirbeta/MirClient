{********************************************************************}
{ TDBLUCOMBO component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2012                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit dblucmbr;

interface

{$I TMSDEFS.INC}
uses
  DBLuComb, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBLUCombo,TDBLUEdit]);
end;




end.

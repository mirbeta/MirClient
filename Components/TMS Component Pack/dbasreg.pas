{********************************************************************}
{ TDBADVSPINEDIT component                                           }
{ for Delphi  & C++Builder                                           }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit dbasreg;

{$I TMSDEFS.INC}
interface

uses
  DBAdvSp, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBAdvSpinEdit]);
end;



end.


{********************************************************************}
{ TMoneyEdit component                                               }
{ for Delphi & C++Builder                                            }
{ version 1.1                                                        }
{                                                                    }
{ Written by :                                                       }
{   TMS Software                                                     }
{   Copyright © 1999-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit MoneyEdr;

{$I TMSDEFS.INC}
interface

uses
  MoneyEdit,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TMoneyEdit]);
end;



end.



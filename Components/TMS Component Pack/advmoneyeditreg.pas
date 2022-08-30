{********************************************************************}
{ TAdvMoneyEdit component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by :                                                       }
{   TMS Software                                                     }
{   Copyright © 2012                                                 }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit AdvMoneyEditReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvMoneyEdit;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvMoneyEdit]);
end;

end.

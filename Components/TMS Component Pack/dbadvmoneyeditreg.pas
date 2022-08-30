{********************************************************************}
{ TAdvMoneyEdit component                                            }
{ for Delphi & C++Builder                                            }
{ version 1.0                                                        }
{                                                                    }
{ Written by :                                                       }
{   TMS Software                                                     }
{   Copyright © 2007                                                 }
{   Email : info@tmssoftware.com                                     }
{   Website : http://www.tmssoftware.com                             }
{********************************************************************}
unit DBAdvMoneyEditReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, DBAdvMoneyEdit;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBAdvMoneyEdit]);
end;



end.



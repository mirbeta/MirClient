{********************************************************************}
{ TLOOKUP components : TLUEdit & TLUCombo                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1996-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}
unit lureg;
{$I TMSDEFS.INC}
interface

uses
  LuCombo, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TLUCombo,TLUEdit]);
end;


end.


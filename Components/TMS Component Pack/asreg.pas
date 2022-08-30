{***********************************************************************}
{ TADVSPINEDIT component                                                }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 1998-2012                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
unit asreg;

{$I TMSDEFS.INC}
interface

uses
  AdvSpin, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvSpinEdit]);
end;

end.


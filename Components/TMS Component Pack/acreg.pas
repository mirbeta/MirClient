{***********************************************************************}
{ TADVCOMBO component                                                   }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 1998-2012                                               }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}
unit acreg;

{$I TMSDEFS.INC}
interface

uses
  AdvCombo, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvComboBox]);
end;



end.


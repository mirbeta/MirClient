{***********************************************************************}
{ TAdvDBLookupComboReg component                                        }
{ for Delphi & C++Builder                                               }
{ version 1.5                                                           }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2002 - 2006                                             }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AdvDBLookupComboBoxReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvDBLookupComboBox;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvDBLookupComboBox]);
end;

end.

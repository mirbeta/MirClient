{********************************************************************}
{ TCOLUMNCOMBOBOX component                                          }
{ for Delphi  & C+Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000-2012                                   }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit colcoreg;
{$I TMSDEFS.INC}

interface

uses
 colcombo,classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TColumnComboBox]);
end;



end.


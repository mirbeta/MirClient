{********************************************************************}
{ TPARAMETERLISTBOX component                                        }
{ for Delphi & C++Builder                                            }
{ version 1.9                                                        }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2004                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit parlreg;

interface

uses
  ParamLst, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TParameterListBox]);
end;


end.

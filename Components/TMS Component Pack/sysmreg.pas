{********************************************************************}
{ TSYSMON component                                                  }
{ for Delphi & C++Builder                                            }
{ version 1.1                                                        }
{                                                                    }
{ written by TMS Software                                            }
{           copyright © 1998-2004                                    }
{           Email : info@tmssoftware.com                             }
{           Web : http://www.tmssoftware.com                         }
{********************************************************************}

unit sysmreg;

interface
{$I TMSDEFS.INC}
uses
  SysMon,  Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS System', [TSysMon]);
end;

end.

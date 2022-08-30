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

unit sysmregde;

interface
{$I TMSDEFS.INC}
uses
  SysMon, SysmDE, Classes,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TMonObject,'SysObject',TMonObjectProperty);
  RegisterPropertyEditor(TypeInfo(string),TMonObject,'Counter',TMonCounterProperty);
end;



end.

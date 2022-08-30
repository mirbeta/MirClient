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

unit parlregde;

interface
{$I TMSDEFS.INC}
uses
  ParamLst, ParamDE, Classes,
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
  RegisterPropertyEditor(TypeInfo(string),TParameterListBox,'Delimiter',TParamDelimiterProperty);
end;


end.

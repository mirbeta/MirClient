{***************************************************************************}
{ TDBInspectorBar component                                                 }
{ for Delphi & C++Builder                                                   }
{ version 1.4                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2005                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit DBInspectorBarRegDE;

interface
{$I TMSDEFS.INC}
uses
  DBInspectorBar, Classes, DBInspDE,
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
  RegisterPropertyEditor(TypeInfo(string),TDBInspectorItem,'DataField',TInspectorItemFieldNameProperty);
end;

end.


{********************************************************************}
{ AdvSmartMessageBoxRegDE components                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2012                                        }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit AdvSmartMessageBoxRegDE;

interface
{$I TMSDEFS.INC}
uses
  Classes, AdvSmartMessageBox, htmlde,
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
  RegisterPropertyEditor(TypeInfo(String), TDefaultSmartMessage, 'Text', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), TAdvSmartMessage, 'Text', THTMLStringProperty);
end;



end.


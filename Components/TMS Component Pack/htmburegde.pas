{********************************************************************}
{ THTMLButtons components                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit htmburegde;

interface
{$I TMSDEFS.INC}
uses
  Classes, htmlbtns, htmlde,
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
  RegisterPropertyEditor(TypeInfo(String), THTMLCheckbox, 'Caption', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), THTMLRadioButton, 'Caption', THTMLStringProperty);
  RegisterPropertyEditor(TypeInfo(String), THTMLButton, 'Caption', THTMLStringProperty);
end;



end.


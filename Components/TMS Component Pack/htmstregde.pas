{**************************************************************************}
{ THTMLStaticText component                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{**************************************************************************}

unit htmstregde;

interface
{$I TMSDEFS.INC}
uses
  Classes, HTMLText, HTMLDE,
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
  RegisterPropertyEditor(TypeInfo(TStrings), THTMLStaticText, 'HTMLText', THTMLTextProperty);
  RegisterComponentEditor(THTMLStaticText, THTMLDefaultEditor);  
end;

end.


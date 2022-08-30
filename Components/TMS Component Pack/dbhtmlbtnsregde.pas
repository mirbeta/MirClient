{********************************************************************}
{ TDBHTMLCheckBox & TDBHTMLRadioGroup component                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit DBHTMLBtnsRegDE;

interface
{$I TMSDEFS.INC}
uses
  Classes, htmlbtns, htmlde, DBHTMLBtns,
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
  RegisterPropertyEditor(TypeInfo(String), TDBHTMLCheckbox, 'Caption', THTMLStringProperty);
end;



end.


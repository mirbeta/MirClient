{********************************************************************}
{ TPARAMLISTBOX component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

{$I TMSDEFS.INC}

unit paramlstregde;

interface

uses
  Paramlistbox,Classes, paramsde,
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
  RegisterPropertyEditor(TypeInfo(TStrings), TParamListBox, 'Items', TParamStringListProperty);
  RegisterComponentEditor(TParamListBox, TParamListDefaultEditor);
end;



end.


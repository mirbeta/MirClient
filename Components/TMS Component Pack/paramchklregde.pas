{********************************************************************}
{ TPARAMCHECKLIST component                                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2000 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit paramchklregde;

{$I TMSDEFS.INC}

interface

uses
  paramchklist,classes,paramsde,
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
  RegisterPropertyEditor(TypeInfo(TStrings), TParamCheckList, 'Items', TParamStringListProperty);
  RegisterComponentEditor(TParamCheckList, TParamListDefaultEditor);
end;

end.


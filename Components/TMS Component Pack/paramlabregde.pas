{********************************************************************}
{ TPARAMLABEL component                                              }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{           copyright © 2000 - 2012                                  }
{           Email : info@tmssoftware.com                             }
{           Web : http://www.tmssoftware.com                         }
{********************************************************************}

unit paramlabregde;

interface

{$I TMSDEFS.INC}

uses
  ParamLabel, Classes,
{$IFNDEF NOEDITOR}
  paramsde,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
{$ENDIF}
  ;


procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TStringList), TParamLabel, 'HTMLText', TParamStringProperty);
  RegisterComponentEditor(TParamLabel, TParamDefaultEditor);
end;



end.


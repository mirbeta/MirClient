{***********************************************************************}
{ TAdvDBLookupComboReg component                                        }
{ for Delphi & C++Builder                                               }
{ version 1.5                                                           }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2002 - 2006                                             }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AdvDBLookupComboBoxRegDe;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvDBLookupComboBox, AdvDBLookupComboBoxDE
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TDBColumnItem,'ListField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'KeyField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'FilterField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'LabelField',TAdvDBComboFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBLookupComboBox,'SortColumn',TAdvDBComboColumnNameProperty);
  RegisterComponentEditor(TAdvDBLookupComboBox,TAdvDBLookupComboBoxEditor);
end;

end.


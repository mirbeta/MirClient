{********************************************************************}
{ TAdvDBComboBox component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2010                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvDBComboBoxRegDE;

interface

{$I TMSDEFS.INC}

uses
  AdvDBComboBox, Classes, AdvDBComboBoxDE, StdCtrls,
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
  RegisterPropertyEditor(TypeInfo(TStrings), TAdvDBComboBox, 'Items', TAdvDBComboPropertyEditor);
  RegisterPropertyEditor(TypeInfo(string),TAdvDBComboBox,'LabelField',TAdvDBComboBoxFieldNameProperty);
  RegisterComponentEditor(TAdvDBComboBox, TAdvDBComboBoxEditor);
end;

end.


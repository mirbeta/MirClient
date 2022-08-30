{********************************************************************}
{ TDBAdvSmoothComboBox component                                     }
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

unit DBAdvSmoothComboBoxRegDE;

interface
{$I TMSDEFS.INC}
uses
  DBAdvSmoothComboBox, Classes, DBAdvSmoothComboBoxDE, StdCtrls,
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
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'CaptionField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'CheckedField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'NotesField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'InfoField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'HintField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'GraphicLeftField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'GraphicRightField',TDBAdvSmoothComboBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TComboBoxDataBinding,'ProgressValueField',TDBAdvSmoothComboBoxFieldNameProperty);
end;

end.


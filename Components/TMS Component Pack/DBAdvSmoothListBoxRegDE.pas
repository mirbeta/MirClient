{********************************************************************}
{ TDBAdvSmoothListBox component                                      }
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

unit DBAdvSmoothListBoxRegDE;

interface
{$I TMSDEFS.INC}
uses
  DBAdvSmoothListBox, Classes, DBAdvSmoothListBoxDE, StdCtrls,
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
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'CaptionField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'CheckedField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'NotesField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'InfoField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'HintField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'GraphicLeftField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'GraphicRightField',TDBAdvSmoothListBoxFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TListBoxDataBinding,'ProgressValueField',TDBAdvSmoothListBoxFieldNameProperty);
end;

end.


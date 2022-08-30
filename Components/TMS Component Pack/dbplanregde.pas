{$I TMSDEFS.INC}
{***********************************************************************}
{ TDBPlanner component                                                  }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{    TMS Software                                                       }
{    copyright © 2001 - 2012                                            }
{    Email : info@tmssoftware.com                                       }
{    Web : http://www.tmssoftware.com                                   }
{                                                                       }
{***********************************************************************}

unit dbplanregde;

interface
uses
  DBPlanner, Classes, DBPlanDE,

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
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'StartTimeField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'EndTimeField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'KeyField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'NotesField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'SubjectField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'ResourceField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'RecurrencyField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'MinTimeField',TPlannerFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TDBItemSource,'MaxTimeField',TPlannerFieldNameProperty);    
end;

end.

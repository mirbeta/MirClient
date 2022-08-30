{*************************************************************************}
{ TDBTodoList component                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2012                                      }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}

unit DBTodoListRegDE;

interface

{$I TMSDEFS.INC}

uses
  DBTodoList, TodoListDE, DBTodoListDE, Classes,
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
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'SubjectField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'NotesField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'CreationDateField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'DueDateField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'CompletionDateField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'CompleteField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'CompletionField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'CategoryField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'StatusField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'PriorityField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'KeyField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'ImageField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'ProjectField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'ResourceField',TTodoListFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TTodoFields,'TotalTimeField',TTodoListFieldNameProperty);
  RegisterComponentEditor(TDBToDoList, TToDoListEditor);
end;

end.


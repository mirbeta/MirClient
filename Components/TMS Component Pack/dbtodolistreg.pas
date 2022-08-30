{*************************************************************************}
{ TDBTodoList component                                                   }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}

unit DBTodoListReg;

interface
{$I TMSDEFS.INC}
uses
  DBTodoList, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner',[TDBTodoList]);
end;

end.
 
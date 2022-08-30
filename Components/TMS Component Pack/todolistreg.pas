{*************************************************************************}
{ TTodoList component                                                     }
{ for Delphi & C++Builder                                                 }
{ version 1.2 - rel. February 2005                                        }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2001 - 2005                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}

unit TodoListReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, TodoList;

{$IFDEF TMSDOTNET}
{$R TTodoList.bmp}
{$ENDIF}

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TTodoList]);
end;


end.
 
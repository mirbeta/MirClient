{*********************************************************************}
{ TPlanner & TDBPlanner interface for Addict Spell Checker component  }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0,6.0             }
{ version 1.1 : release July 2002                                     }
{                                                                     }
{ written by TMS Software                                             }
{            copyright © 1996-2002                                    }
{            Email : info@tmssoftware.com                             }
{            Web : http://www.tmssoftware.com                         }
{*********************************************************************}

unit PlanAddictCheckReg;

interface

uses
  Classes, PlanAddictCheck;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TAddictPlannerCheck]);
end;

end.


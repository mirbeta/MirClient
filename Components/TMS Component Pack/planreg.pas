{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email : info@tmssoftware.com                               }
{            Web : http://www.tmssoftware.com                           }
{***********************************************************************}

unit planreg;

{$I TMSDEFS.INC}

interface

uses
  Classes, Planner, PlanCheck
  , PlanItemEdit, PlanSimpleEdit, PlanPeriodEdit, PlannerWaitList, PlannerActions, ActnList
  , PlanRecurrEdit
  , PlanDraw
  {$IFDEF DELPHIXE3_LVL}
  , System.Actions
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TPlanner]);
  RegisterComponents('TMS Planner', [TCapitalPlannerCheck]);
  RegisterComponents('TMS Planner', [TAlarmMessage]);

  RegisterComponents('TMS Planner', [TSimpleItemEditor]);
  RegisterComponents('TMS Planner', [TDefaultItemEditor]);
  RegisterComponents('TMS Planner', [TPeriodItemEditor]);

  RegisterComponents('TMS Planner', [TPlannerRecurrencyEditor]);
  RegisterComponents('TMS Planner', [TShapeDrawTool]);
  RegisterComponents('TMS Planner', [TPlannerWaitList]);

  RegisterActions('Planner', [TPlannerCut, TPlannerCopy, TPlannerPaste, TPlannerDelete, TPlannerInsert, TPlannerEdit], nil);
end;



end.


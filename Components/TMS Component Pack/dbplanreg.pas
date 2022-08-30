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

unit DBPlanReg;


interface
uses
  DBPlanner, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TDBPlanner]);
  RegisterComponents('TMS Planner', [TDBDaySource]);
  RegisterComponents('TMS Planner', [TDBPeriodSource]);
  RegisterComponents('TMS Planner', [TDBMonthSource]);
  RegisterComponents('TMS Planner', [TDBWeekSource]);
  RegisterComponents('TMS Planner', [TDBMultiMonthSource]);
  RegisterComponents('TMS Planner', [TDBTimeLineSource]);
  RegisterComponents('TMS Planner', [TDBHalfDayPeriodSource]);
  RegisterComponents('TMS Planner', [TDBDisjunctDaySource]);
  RegisterComponents('TMS Planner', [TDBActiveDaySource]);
  RegisterComponents('TMS Planner', [TDBActiveDayPeriodSource]);
end;

end.

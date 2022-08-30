{***********************************************************************}
{ TDBPLANNERCALENDAR & TDBPLANNERDATEPICKER components                  }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{***********************************************************************}
{$I TMSDEFS.INC}

unit DBPlannerCalReg;


interface

uses
  Classes, DBPlannerCal, DBPlannerDatePicker, PlannerDBDatePicker, PlannerDBMaskDatePicker;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TDBPlannerCalendar,TDBPlannerDatePicker, TPlannerDBDatePicker, TPlannerDBMaskDatePicker]);
  RegisterClass(TDBPlannerCalendar);
end;

end.
 
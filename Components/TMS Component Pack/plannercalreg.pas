{***********************************************************************}
{ TPLANNERCALENDAR component                                            }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2012                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{***********************************************************************}
unit PlannerCalReg;

interface
{$I TMSDEFS.INC}

uses
  PlannerCal, PlannerDatePicker, PlannerMaskDatePicker, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TPlannerCalendar,
                                     TPlannerCalendarGroup,
                                     TPlannerDatePicker,
                                     TPlannerMaskDatePicker]);
end;



end.



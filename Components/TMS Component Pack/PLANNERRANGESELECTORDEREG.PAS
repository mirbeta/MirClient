{***********************************************************************}
{ TPLANNERRANGESELECTOR component                                       }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 1999-2014                                      }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{***********************************************************************}

unit PlannerRangeSelectorDEReg;

interface

uses
  Classes, PlannerRangeSelector, PlannerCal, PlannerCalRegDE, DesignIntf, DesignEditors;

procedure Register;

implementation

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(TPlannerCalendar), TPlannerRangeSelector, 'Calendar', TPlannerCalendarEditProperty);

end;

end.

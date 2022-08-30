{$I TMSDEFS.INC}
{***********************************************************************}
{ TPLANNERMONTHVIEW component                                           }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2012                                           }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{***********************************************************************}


unit PlannerMonthViewReg;

interface


uses
  PlannerMonthView, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Planner', [TPlannerMonthView]);
end;



end.



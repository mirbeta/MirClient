{***************************************************************************}
{ TvCalendar component                                                      }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2012                                       }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit vCalReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, vCal;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Planner',[TvCalendar]);
end;



end.
 
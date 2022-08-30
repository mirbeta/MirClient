{***************************************************************************}
{ TvCard component                                                          }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2013                                               }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit vCardReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, vCard;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Planner',[TvCard]);
end;



end.
 
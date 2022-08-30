{*************************************************************************}
{ TChartLink component                                                    }
{ for Delphi & C++Builder                                                 }
{ version 1.1                                                             }
{                                                                         }
{ Copyright © 2000-2005                                                   }
{   TMS Software                                                          }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{                                                                         }
{*************************************************************************}

unit ChartLinkReg;

interface

uses
 Classes, ChartLink;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids',[TChartLink]);
end;


end.
 
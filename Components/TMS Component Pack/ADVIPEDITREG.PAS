{***********************************************************************}
{ TAdvIPEdit component                                                  }
{ for Delphi & C++ Builder                                              }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2014                                           }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit AdvIPEditReg;

interface

uses
  Classes, AdvIPEdit;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Edits',[TAdvIPEdit]);
end;

end.

{********************************************************************}
{ TMSWHEEL component                                                 }
{ for Delphi & C++Builder                                            }
{ version 1.5                                                        }
{                                                                    }
{ written by                                                         }
{    TMS Software                                                    }
{    copyright © 1997-2004                                           }
{    Email : info@tmssoftware.com                                    }
{    Web : http://www.tmssoftware.com                                }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the author and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit wheelreg;

interface

uses
  Classes, MSWheel;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS System', [TMSWheel]);
end;



end.


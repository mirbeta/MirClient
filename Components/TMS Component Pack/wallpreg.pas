{*********************************************************************}
{ TWallPaper component                                                }
{ for Delphi & C++Builder                                             }
{ version 1.1                                                         }
{                                                                     }
{ Written by TMS Software                                             }
{ Copyright © 2000 - 2004                                             }
{ Email : info@tmssoftware.com                                        }
{ Web : http://www.tmssoftware.com                                    }
{                                                                     }
{ The source code is given as is. The author is not responsible       }
{ for any possible damage done due to the use of this code.           }
{ The component can be freely used in any application. The source     }
{ code remains property of the writer and may not be distributed      }
{ freely as such.                                                     }
{*********************************************************************}

unit wallpreg;

interface

uses
  Wallpaper, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TWallPaper]);
end;

end.


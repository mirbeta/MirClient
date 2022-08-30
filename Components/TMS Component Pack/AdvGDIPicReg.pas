{********************************************************************}
{ TAdvGDIPicture component                                           }
{ for Delphi & C++Builder                                            }
{ version 1.0                                                        }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2006 - 2009                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvGDIPicReg;

interface

{$I TMSDEFS.INC}

uses
  AdvGDIPicture, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvGDIPPicture]);
end;

end.


{********************************************************************}
{ TPictureContainer component                                        }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2001 - 2013                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit PictureContainerReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, PictureContainer;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [TPictureContainer]);
end;

end.


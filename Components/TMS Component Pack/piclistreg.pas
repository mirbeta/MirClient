{********************************************************************}
{ TAdvPicture component                                              }
{ for Delphi & C++Builder                                            }
{ version 1.1                                                        }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2001                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit piclistreg;

interface

uses
  PictureList,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TPictureList]);
end;

end.


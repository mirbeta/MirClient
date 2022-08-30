{****************************************************************}
{ TWebImage component                                            }
{ for Delphi & C++Builder                                        }
{ version 1.1                                                    }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 2000-2004                                        }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}

unit WebImgR;

interface

uses
  WebImage, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web', [TWebImage]);
end;

end.


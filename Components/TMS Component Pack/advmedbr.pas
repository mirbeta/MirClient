{********************************************************************}
{ TADVMASKEDITBTN and TUnitAdvMaskEditBtn component                  }
{ for Delphi & C++Builder                                            }
{ version 1.3                                                        }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2003                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit AdvMEdBr;

interface

uses
  AdvMEdBtn,Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvMaskEditBtn,TUnitAdvMaskEditBtn]);
end;



end.


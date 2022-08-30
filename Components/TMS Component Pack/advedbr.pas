{********************************************************************}
{ TADVEDITBTN and TUnitAdvEditBtn component                          }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit advedbr;

interface
{$I TMSDEFS.INC}

uses
  AdvEdBtn, AdvFileNameEdit, Classes, AdvDirectoryEdit;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvEditBtn,TUnitAdvEditBtn]);
  RegisterComponents('TMS Edits', [TAdvFileNameEdit]);
  RegisterComponents('TMS Edits', [TAdvDirectoryEdit]);
end;



end.


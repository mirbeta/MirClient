{********************************************************************}
{ TADVLUEDIT component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit advluedr;

interface

{$I TMSDEFS.INC}

uses
  AdvLuEd, Classes;

procedure Register;


implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TAdvLUEdit]);
end;


end.
 
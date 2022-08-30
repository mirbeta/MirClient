{********************************************************************}
{ TDBADVLUEDIT component                                             }
{ for Delphi  & C++Builder                                           }
{                                                                    }
{ written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit DBAdvLer;

{$I TMSDEFS.INC}
interface

uses
  DBAdvle, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBAdvLUEdit]);
end;


end.


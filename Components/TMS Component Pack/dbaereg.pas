{*************************************************************************}
{ TDBADVEDIT component                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{   TMS Software                                                          }
{   Copyright © 1996-2012                                                 }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{*************************************************************************}

unit dbaereg;
{$I TMSDEFS.INC}
interface

uses
  DBAdvEd, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBAdvEdit, TDBAdvMaskEdit]);
end;



end.


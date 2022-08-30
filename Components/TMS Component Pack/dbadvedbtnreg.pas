{*************************************************************************}
{ TDBADVEDITBTN component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{   TMS Software                                                          }
{   Copyright © 1996-2012                                                 }
{   Email : info@tmssoftware.com                                          }
{   Web : http://www.tmssoftware.com                                      }
{*************************************************************************}

unit DBAdvEdBtnReg;
{$I TMSDEFS.INC}
interface

uses
  DBAdvEdBtn, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBAdvEditBtn]);
end;



end.


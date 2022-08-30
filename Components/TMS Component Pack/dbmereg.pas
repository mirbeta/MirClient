{*******************************************************************}
{ TDBMONEYEDIT component                                            }
{ for Delphi 3.0,4.0,5.0,6.0 & C++Builder 3.0,4.0,5.0               }
{ version 1.0                                                       }
{                                                                   }
{ Written by                                                        }
{   TMS Software                                                    }
{   Copyright © 2000 -2001                                          }
{   Email : info@tmssoftware.com                                    }
{   Web : http://www.tmssoftware.com                                }
{*******************************************************************}

unit dbmereg;

interface

uses
  DBMnyEd, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TDBMoneyEdit]);
end;



end.


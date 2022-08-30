{********************************************************************}
{ THTMLTREELIST component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2000 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit htmltlreg;

{$I TMSDEFS.INC}
interface

uses
  HTMLTreeList,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLTreeList]);
end;

end.


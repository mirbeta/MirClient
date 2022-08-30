{********************************************************************}
{ THTMLTREEVIEW component                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 1998-2012                                            }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit htmltvr;

{$I TMSDEFS.INC}
interface

uses
  HTMLTV, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLTreeview]);
end;



end.


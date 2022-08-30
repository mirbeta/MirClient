{********************************************************************}
{ THTMLSTATUSBAR Component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{   TMS Software                                                     }
{   Copyright © 2002 - 2012                                          }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{********************************************************************}

unit htmstbr;

{$I TMSDEFS.INC}
interface

uses
  HTMLStatusBar,Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLStatusBar]);
end;



end.


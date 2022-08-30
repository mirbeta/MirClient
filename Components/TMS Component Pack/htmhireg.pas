{*************************************************************************}
{ THTMLHint component                                                     }
{ for Delphi and C++Builder                                               }
{                                                                         }
{ written by TMS Software                                                 }
{            copyright © 1999 - 2012                                      }
{            Email : info@tmssoftware.com                                 }
{            Web : http://www.tmssoftware.com                             }
{                                                                         }
{*************************************************************************}

unit htmhireg;

{$I TMSDEFS.INC}
interface

uses
  Classes,htmlhint;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML',[ THTMLHint ]);
end;

end.


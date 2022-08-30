{**************************************************************************}
{ THTMLStaticText component                                                }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2000 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{**************************************************************************}

unit htmstreg;

{$I TMSDEFS.INC}
interface

uses
  Classes, HTMLText;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLStaticText ]);
end;

end.
 

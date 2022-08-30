{********************************************************************}
{ THTMLButtons components                                            }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit htmbureg;
{$I TMSDEFS.INC}
interface

uses
  Classes, htmlbtns;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLRadioButton,THTMLCheckbox,THTMLButton,THTMLRadioGroup,THTMLCheckGroup]);
end;

end.


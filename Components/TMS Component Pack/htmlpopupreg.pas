{********************************************************************}
{ THTMLPopup component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 2001 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit HTMLPopupReg;

{$I TMSDEFS.INC}
interface

uses
  Classes, HTMLPopup;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLPopup]);
end;



end.


{********************************************************************}
{ TDBHTMLCheckBox & TDBHTMLRadioGroup component                      }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1999 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}

unit DBHTMLBtnsReg;

{$I TMSDEFS.INC}

interface

uses
  Classes, htmlbtns, DBHTMLBtns;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [TDBHTMLCheckbox, TDBHTMLRadioGroup]);
end;



end.


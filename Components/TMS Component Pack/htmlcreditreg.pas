{**************************************************************************}
{ THTMLCredit component                                                    }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2003 - 2012                                       }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{**************************************************************************}

unit HTMLCreditReg;

interface
{$I TMSDEFS.INC}
uses
  Classes, HTMLCredit;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML', [THTMLCredit]);
end;

end.
 

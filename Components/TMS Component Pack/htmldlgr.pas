{********************************************************************}
{ THTMDialog component                                               }
{ for Delphi & C++Builder                                            }
{                                                                    }
{  Written by                                                        }
{    TMS Software                                                    }
{    Copyright © 2001 - 2012                                         }
{    Email : info@tmssoftware.com                                    }
{    Web : http://www.tmssoftware.com                                }
{********************************************************************}

unit htmldlgr;

interface
{$I TMSDEFS.INC}
uses
  Classes, HTMLDialog;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS HTML',[ THTMLDialog ]);
end;

end.


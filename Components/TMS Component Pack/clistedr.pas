{********************************************************************}
{ TCHECKLISTEDIT component                                           }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ Written by                                                         }
{    TMS Software                                                    }
{    Copyright © 1999-2012                                           }
{    Email : info@tmssoftware.com                                    }
{    Web : http://www.tmssoftware.com                                }
{********************************************************************}

unit clistedr;

interface

{$I TMSDEFS.INC}
uses
  Classes,CListEd;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Edits', [TCheckListEdit]);
end;


end.

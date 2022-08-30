{********************************************************************}
{ TPICKDLG component                                                 }
{ for Delphi & C++Builder                                            }
{                                                                    }
{ written by TMS Software                                            }
{            copyright © 1998 - 2012                                 }
{            Email : info@tmssoftware.com                            }
{            Web : http://www.tmssoftware.com                        }
{********************************************************************}
unit pdlgreg;

interface

{$I TMSDEFS.INC}
uses
  Pickdlg, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS',[TPickDialog]);
end;

end.
 

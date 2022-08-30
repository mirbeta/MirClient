{****************************************************************}
{ TWEBCOPY component                                             }
{ for Delphi & C++Builder                                        }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 2000-2012                                        }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}
unit webcreg;

interface
                    
uses
  WebCopy, Classes;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web',[TWebCopy]);
end;

end.


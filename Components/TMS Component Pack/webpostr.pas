{****************************************************************}
{ TWEBPOST component                                             }
{ for Delphi 3.0,4.0,5.0 - C++Builder 3,4,5                      }
{ version 1.0 - February 2001                                    }
{                                                                }
{ written by                                                     }
{   TMS Software                                                 }
{   copyright © 2001                                             }
{   Email : info@tmssoftware.com                                 }
{   Web : http://www.tmssoftware.com                             }
{****************************************************************}

unit WebPostr;

interface

uses
  Classes, WebPost;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web', [TWebPost]);
end;


end.

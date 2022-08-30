{*******************************************************************}
{ TWEBCONNECT component                                             }
{ for Delphi 4.0,5.0,6.0 - C++Builder 4,5                           }
{ version 1.0 - December 2001                                       }
{                                                                   }
{ written by                                                        }
{    TMS Software                                                   }
{    copyright © 2001                                               }
{    Email : info@tmssoftware.com                                   }
{    Web   : http://www.tmssoftware.com                             }
{                                                                   }
{*******************************************************************}

unit WebConnectReg;

interface

uses
  Classes, WebConnect;

  procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Web',[TWebConnect]);
end;

end.
 
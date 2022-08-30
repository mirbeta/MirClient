{*************************************************************************}
{ TMATHLIBS components                                                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by                                                              }
{    TMS Software                                                         }
{    copyright © 2002 - 2015                                              }
{    Email:info@tmssoftware.com                                           }
{    Web:http://www.tmssoftware.com                                       }
{                                                                         }
{*************************************************************************}

unit MiscMathLibReg;

interface

uses
  Classes, MiscMathLib;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Grids', [TMiscMathLib]);
  RegisterComponents('TMS Grids', [TFinanceMathLib]);
  RegisterComponents('TMS Grids', [TConversionMathLib]);
  RegisterComponents('TMS Grids', [TStringMathLib]);
  RegisterComponents('TMS Grids', [TDBMathLib]);
end;

end.

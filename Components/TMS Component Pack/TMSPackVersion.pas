{***************************************************************************}
{ TMS Component Pack Pro                                                    }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 1998 - 2015                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit TMSPackVersion;

interface

function GetProductVersion: string;
function GetProductName: string;
function GetCompilerVersion: string;

implementation

const
  VERNUM = '8.0.4.0';
  PRODUCTNAME = 'TMS Component Pack Pro';


function GetProductName:string;
begin
  Result := PRODUCTNAME;
end;

function GetProductVersion:string;
begin
  Result := VERNUM;
end;

function GetCompilerVersion: string;
begin
  Result := '';
  {$IFDEF VER180}
  {$IFNDEF VER185}
  Result := 'Delphi 2006';
  {$ENDIF}
  {$IFDEF VER185}
  Result := 'Delphi 2007';
  {$ENDIF}
  {$ENDIF}
  {$IFDEF VER200}
  Result := 'Delphi 2009';
  {$ENDIF}
  {$IFDEF VER210}
  Result := 'Delphi 2010';
  {$ENDIF}
  {$IFDEF VER220}
  Result := 'Delphi XE';
  {$ENDIF}
  {$IFDEF VER230}
  Result := 'Delphi XE2';
  {$ENDIF}
  {$IFDEF VER240}
  Result := 'Delphi XE3';
  {$ENDIF}
  {$IFDEF VER250}
  Result := 'Delphi XE4';
  {$ENDIF}
  {$IFDEF VER260}
  Result := 'Delphi XE5';
  {$ENDIF}
  {$IFDEF VER270}
  Result := 'Delphi XE6';
  {$ENDIF}
  {$IFDEF VER280}
  Result := 'Delphi XE7';
  {$ENDIF}
  {$IFDEF VER290}
  Result := 'Delphi XE8';
  {$ENDIF}
  {$IFDEF VER300}
  Result := 'Delphi XE9';
  {$ENDIF}
end;

end.

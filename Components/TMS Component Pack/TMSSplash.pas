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

unit TMSSplash;

interface

uses
  ToolsApi, Windows, Classes, Graphics;

{$R TMSSPLASH.RES}

implementation

uses
  TMSPackVersion;

procedure AddSplash;
var
  bmp: TBitmap;
begin
  {$IFDEF ConditionalExpressions}
  {$if CompilerVersion >= 18}
  bmp := TBitmap.Create;
  try
    bmp.LoadFromResourceName(HInstance, 'TMSSPLASH');
    SplashScreenServices.AddPluginBitmap(GetProductName + ' for ' + GetCompilerVersion + ' ' + GetProductVersion,bmp.Handle,false,'Registered','');
  finally
    bmp.Free;
  end;
  {$ifend}
  {$ENDIF}
end;

begin
  AddSplash;
end.

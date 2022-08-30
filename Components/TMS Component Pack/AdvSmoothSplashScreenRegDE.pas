{********************************************************************}
{ TAdvSmoothSplashScreen                                             }
{ for Delphi & C++Builder                                            }
{ version 1.0                                                        }
{                                                                    }
{ written                                                            }
{   TMS Software                                                     }
{   copyright © 2009                                                 }
{   Email : info@tmssoftware.com                                     }
{   Web : http://www.tmssoftware.com                                 }
{                                                                    }
{ The source code is given as is. The author is not responsible      }
{ for any possible damage done due to the use of this code.          }
{ The component can be freely used in any application. The source    }
{ code remains property of the writer and may not be distributed     }
{ freely as such.                                                    }
{********************************************************************}

unit AdvSmoothSplashScreenRegDE;

interface
{$I TMSDEFS.INC}
uses
  Classes, AdvSmoothSplashScreen, AdvSmoothSplashScreenDE, AdvSmoothHTMLProp,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


procedure Register;

implementation


procedure Register;
begin
  RegisterComponentEditor(TAdvSmoothSplashScreen, TAdvSmoothSplashScreenEditor);
  RegisterPropertyEditor(TypeInfo(string), TSplashListItem, 'HTMLText', TAdvSmoothHTMLProperty);
  RegisterPropertyEditor(TypeInfo(string), TAdvSplashHTMLText, 'Text', TAdvSmoothHTMLProperty);
end;


end.


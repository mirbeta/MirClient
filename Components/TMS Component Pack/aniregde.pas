{*******************************************************************}
{ TANIICON component                                                }
{ for Delphi & C++Builder                                           }
{ version 2.0                                                       }
{                                                                   }
{ written                                                           }
{   TMS Software                                                    }
{   copyright ? 1998-2004                                           }
{   Email : info@tmssoftware.com                                    }
{   Web : http://www.tmssoftware.com                                }
{                                                                   }
{ The source code is given as is. The author is not responsible     }
{ for any possible damage done due to the use of this code.         }
{ The component can be freely used in any application. The source   }
{ code remains property of the writer and may not be distributed    }
{ freely as such.                                                   }
{*******************************************************************}

unit AniRegDE;

interface
{$I TMSDEFS.INC}
uses
  AniIcon, Classes, AniDE,
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
  RegisterPropertyEditor(TypeInfo(TAniIconFile), TAniIcon, 'AniFile', TAniProperty);
end;

end.


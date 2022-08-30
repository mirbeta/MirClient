{*************************************************************************}
{ TAdvPanel, TAdvPanelGroup, TAdvPanelStyler component                    }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written                                                                 }
{   TMS Software                                                          }
{   copyright © 2000-2012                                                 }
{ Email : info@tmssoftware.com                                            }
{ Web : http://www.tmssoftware.com                                        }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The source         }
{ code remains property of the writer and may not be distributed          }
{ freely as such.                                                         }
{*************************************************************************}

unit advpnlreg;

{$I TMSDEFS.INC}
interface

uses
  AdvPanel, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS', [TAdvPanel, TAdvPanelGroup, TAdvPanelStyler]);
end;

end.


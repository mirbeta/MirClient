{***************************************************************************}
{ TRTTIInspectorBar component                                               }
{ for Delphi & C++Builder                                                   }
{ version 1.4                                                               }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2005                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit RTTIInspectorBarReg;

interface
{$I TMSDEFS.INC}
uses
  RTTIInspectorBar, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Bars',[TRTTIInspectorBar]);
end;

end.
 
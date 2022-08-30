{***************************************************************************}
{ TINIInspectorBar component                                                }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2012                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}

unit INIInspectorBarReg;

interface
{$I TMSDEFS.INC}

uses
  INIInspectorBar, Classes;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Bars',[TINIInspectorBar]);
end;

end.
 
{***************************************************************************}
{ TInspectorBar component                                                   }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2001 - 2014                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{***************************************************************************}
unit InspectorBarReg;

interface

{$I TMSDEFS.INC}

uses
  Classes, InspectorBar;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponents('TMS Bars',[TInspectorBar]);
end;

end.

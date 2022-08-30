{*************************************************************************}
{ TADVTOOLBUTON component                                                 }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2002 - 2012                                       }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{*************************************************************************}
{$I TMSDEFS.INC}
unit atbregde;

interface

uses
  Classes, AdvToolBtn, AdvToolButtonDE
  {$IFDEF DELPHI6_LVL}
  , DesignIntf
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;


procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvToolButton, TAdvToolButtonEditor);  
end;

end.

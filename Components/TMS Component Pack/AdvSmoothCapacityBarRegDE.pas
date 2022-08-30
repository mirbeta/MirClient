{***********************************************************************}
{ TAdvSmoothCapacityBar component                                       }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2010                                                    }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AdvSmoothCapacityBarRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvSmoothCapacityBar, AdvSmoothCapacityBarDE
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;

procedure Register;

implementation

procedure Register;
begin
  RegisterComponentEditor(TAdvSmoothCapacityBar,TAdvSmoothCapacityBarEditor);
end;

end.


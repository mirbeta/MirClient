{***********************************************************************}
{ TAdvFancyLabel component                                              }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by                                                            }
{   TMS Software                                                        }
{   Copyright © 2013                                                    }
{   Email : info@tmssoftware.com                                        }
{   Web : http://www.tmssoftware.com                                    }
{***********************************************************************}

unit AdvFancyLabelRegDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvCurve, AdvFancyLabel, AdvCurveDE, AdvCurveEditor
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
  RegisterPropertyEditor(TypeInfo(TCurvePoints),TAdvFancyLabel,'Points',TCurvePointsProperty);
  RegisterComponentEditor(TAdvFancyLabel, TCurveEditor);
end;

end.


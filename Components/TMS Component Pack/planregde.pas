{***********************************************************************}
{ TPlanner component                                                    }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2015                                      }
{            Email : info@tmssoftware.com                               }
{            Web : http://www.tmssoftware.com                           }
{***********************************************************************}

unit planregde;

{$I TMSDEFS.INC}

interface

uses
  Classes, Planner, PlannerWaitList, PlanDE, DesignIntf;

procedure Register;

implementation

procedure Register;
begin
  {$IFDEF DELPHI9_LVL}
  ForceDemandLoadState(dlDisable);
  EnableDemandLoadReport(false);
  {$ENDIF}
  RegisterComponentEditor(TPlanner, TPlannerEditor);
  RegisterComponentEditor(TPlannerWaitList, TPlannerWaitListEditor);  
  RegisterPropertyEditor(TypeInfo(TPlannerSkin),TCustomPlanner,'Skin',TSkinProperty);
end;



end.


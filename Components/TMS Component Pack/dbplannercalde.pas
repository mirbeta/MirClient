{**************************************************************************}
{ TDBPLANNERCALENDAR DESIGN TIME EDITOR                                    }
{ for Delphi and C++Builder                                                }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2012                                              }
{            Email : info@tmssoftware.com                                  }
{            Web : http://www.tmssoftware.com                              }
{                                                                          }
{ The source code is given as is. The author is not responsible            }
{ for any possible damage done due to the use of this code.                }
{ The component can be freely used in any application. The complete        }
{ source code remains property of the author and may not be distributed,   }
{ published, given or sold in any form as such. No parts of the source     }
{ code can be included in any other component or application without       }
{ written authorization of the author.                                     }
{**************************************************************************}

unit DBPlannerCalDE;

interface
{$I TMSDEFS.INC}
uses
  Classes,DBPlannerCal,DB,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors

{$ELSE}
  DsgnIntf
{$ENDIF}
  ;

type
  TPlannerCalendarFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;

implementation

function TPlannerCalendarFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TPlannerCalendarFieldNameProperty.GetValues(Proc:TGetStrProc);
var
  FDBPlannerCalendar: TDBPlannerCalendar;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
begin
  FDBPlannerCalendar := (GetComponent(0) as TDBPlannerCalendar);
  FDataSource := FDBPlannerCalendar.DataSource;

  if not Assigned(FDataSource) then Exit;
  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then Exit;

  st := TStringList.Create;
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do Proc(st.Strings[i - 1]);
  st.Free;
end;

end.


{$I TMSDEFS.INC}
{***********************************************************************}
{ TDBPlannerMonthView component                                         }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2004 - 2012                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

unit DBPlannerMonthViewRegDE;


interface


uses
  Classes, DB
  , DBPlannerMonthView

{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;

type
  TPlannerMonthViewFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;


  procedure Register;

implementation

function TPlannerMonthViewFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TPlannerMonthViewFieldNameProperty.GetValues(Proc:TGetStrProc);
var
  FDBPlannerMonthView: TDBPlannerMonthView;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
begin
  FDBPlannerMonthView := (GetComponent(0) as TPlannerDataBinding).Owner;
  FDataSource := FDBPlannerMonthView.DataSource;
  if not Assigned(FDataSource) then
    Exit;

  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then Exit;

  st := TStringList.Create;
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do Proc(st.Strings[i - 1]);
  st.Free;
end;



procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'StartTimeField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'EndTimeField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'KeyField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'NotesField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'SubjectField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'ResourceField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'RecurrencyField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'MinTimeField',TPlannerMonthViewFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string),TPlannerDataBinding,'MaxTimeField',TPlannerMonthViewFieldNameProperty);    
end;

end.

{**************************************************************************}
{ TDBInspectorBar DESIGN TIME EDITOR                                       }
{ for Delphi 5.0,6.0,7.0,2005 & C++Builder 5.0,6.0                         }
{ version 1.4                                                              }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2001 - 2005                                       }
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

unit DBInspDE;

interface
{$I TMSDEFS.INC}
uses
  Classes,DBInspectorBar,DB,
{$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
{$ELSE}
  DsgnIntf
{$ENDIF}
  ;


type
  TInspectorItemFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;

implementation

function TInspectorItemFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TInspectorItemFieldNameProperty.GetValues(Proc:TGetStrProc);
var
  InspectorItem: TDBInspectorItem;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
begin
  InspectorItem := (GetComponent(0) as TDBInspectorItem);
  FDataSource := TDBInspectorPanel(InspectorItem.InspectorPanel).DataSource;

  if not Assigned(fDataSource) then
    Exit;

  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then
    Exit;

  st := TStringList.Create;
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do
    proc(st.Strings[i-1]);
  st.Free;
end;

end.


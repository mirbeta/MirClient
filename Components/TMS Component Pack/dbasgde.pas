{**************************************************************************}
{ TDBADVSTRINGGRID DESIGN TIME EDITOR                                      }
{ for Delphi & C++Builder                                                  }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 1999-2012                                         }
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

unit DBAsgDE;

interface
{$I TMSDEFS.INC}

uses
  Classes, DBAdvGrd, DB
{$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors, ContNrs, AsgPropPref
{$ELSE}
  , DsgnIntf
{$ENDIF}
  ;


type
  TSgFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;

  TDBAdvStringGridEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
      var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;


implementation

uses
  Dialogs, SysUtils;

function TSgFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TSgFieldNameProperty.GetValues(Proc:TGetStrProc);
var
  FStringGridField: TStringGridField;
  FStringGridFields: TStringGridFields;
  FDBStringGrid: TDBAdvStringGrid;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  st: TStringList;
  i: Integer;
begin
  FStringGridField := (GetComponent(0) as TStringGridField);
  FStringGridFields := (fStringGridField.Collection as TStringGridFields);
  FDBStringGrid:=fStringGridFields.GetOwner as TDBAdvStringGrid;

  FDataSource := FDBStringGrid.DataSource;
  if not Assigned(FDataSource) then
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


{$IFDEF DELPHI6_LVL}
procedure TDBAdvStringGridEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TDBAdvStringGridEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'FIELDS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;


procedure TDBAdvStringGridEditor.ExecuteVerb(Index: integer);
var
  compiler: string;
begin
  case index of
  0:begin
    {$IFDEF VER120}
    compiler := 'Delphi 4';
    {$ENDIF}
    {$IFDEF VER125}
    compiler := 'C++Builder 4';
    {$ENDIF}
    {$IFDEF VER130}
    {$IFDEF BCB}
    compiler := 'C++Builder 5';
    {$ELSE}
    compiler := 'Delphi 5';
    {$ENDIF}
    {$ENDIF}
    {$IFDEF VER140}
    {$IFDEF BCB}
    compiler := 'C++Builder 6';
    {$ELSE}
    compiler := 'Delphi 6';
    {$ENDIF}
    {$ENDIF}

    MessageDlg(Component.ClassName+' version '+(Component as TDBAdvStringGrid).VersionString+' for '+compiler+#13#10'© 1997-2002 by TMS software',
               mtinformation,[mbok],0);
    end;
  1:begin
      Edit;
    end;
  end;
end;

function TDBAdvStringGridEditor.GetVerb(index: integer): string;
begin
  case index of
  0:Result := '&Version';
  1:Result := '&Fields';
  end;
end;

function TDBAdvStringGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;



end.


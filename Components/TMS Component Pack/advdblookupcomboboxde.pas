{**************************************************************************}
{ TAdvDBLookupComboBoxDE DESIGN TIME EDITOR                                }
{ for Delphi & C++Builder                                                  }
{ version 1.5                                                              }
{                                                                          }
{ written by TMS Software                                                  }
{            copyright © 2002 - 2006                                       }
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

unit AdvDBLookupComboBoxDE;

interface

{$I TMSDEFS.INC}

uses
  Classes, AdvDBLookupComboBox,DB,
  {$IFDEF DELPHI6_LVL}
  DesignIntf, DesignEditors
  {$ELSE}
  DsgnIntf
  {$ENDIF}
  ;

type
  TAdvDBLookupComboBoxEditor = class(TDefaultEditor)
  protected
    {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor;
                           var Continue, FreeEditor: Boolean); override;
    {$ELSE}
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    {$ENDIF}
  public
  end;


  TAdvDBComboFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;

  TAdvDBComboColumnNameProperty = class(TStringProperty)
  public
    function GetAttributes:TPropertyAttributes; override;
    procedure GetValues(Proc:TGetStrProc); override;
  end;


implementation
uses
  SysUtils
{$IFDEF DELPHI2006_LVL}
  , WideStrings
{$ENDIF}
  ;

function TAdvDBComboFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TAdvDBComboFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  FDBCombo: TAdvDBLookupComboBox;
  FDataSource: TDataSource;
  FDataSet: TDataSet;
  i: Integer;
  {$IFNDEF DELPHIXE3_LVL}
  {$IFNDEF DELPHI2006_LVL}
  sl: TStringList;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl: TWideStringList;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  sl: TStringList;
  {$ENDIF}

begin
  if (GetComponent(0) is TAdvDBLookupComboBox) then
    FDBCombo := (GetComponent(0) as TAdvDBLookupComboBox)
  else
    FDBCombo := (GetComponent(0) as TDBColumnItem).Combo;

  FDataSource := FDBCombo.ListSource;
  
  if not Assigned(FDataSource) then
    Exit;

  FDataSet := FDataSource.DataSet;

  if not Assigned(FDataSet) then Exit;

  {$IFNDEF DELPHIXE3_LVL}

  {$IFNDEF DELPHI2006_LVL}
  sl := TStringList.Create;
  {$ENDIF}
  {$IFDEF DELPHI2006_LVL}
  sl := TWideStringList.Create;
  {$ENDIF}

  {$ENDIF}

  {$IFDEF DELPHIXE3_LVL}
  sl := TStringList.Create;
  {$ENDIF}

  FDataSet.GetFieldNames(sl);
  for i := 1 to sl.Count do
    Proc(sl.Strings[i - 1]);
  sl.Free;
end;

function TAdvDBComboColumnNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList,paSortList];
end;

procedure TAdvDBComboColumnNameProperty.GetValues(Proc: TGetStrProc);
var
  FDBCombo: TAdvDBLookupComboBox;
  i: Integer;
begin
  if (GetComponent(0) is TAdvDBLookupComboBox) then
    FDBCombo := (GetComponent(0) as TAdvDBLookupComboBox)
  else
    Exit;

  for i := 1 to FDBCombo.Columns.Count do
    Proc(FDBCombo.Columns[i -1].Name);
end;



{$IFDEF DELPHI6_LVL}
procedure TAdvDBLookupComboBoxEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
{$ELSE}
procedure TAdvDBLookupComboBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ENDIF}
var
  PropName: string;
begin
  PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'COLUMNS') = 0) then
    begin
      PropertyEditor.Edit;
      Continue := False;
    end;
end;



end.



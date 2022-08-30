{*************************************************************************}
{ TAdvDBComboBox component                                                }
{ for Delphi & C++Builder                                                 }
{                                                                         }
{ written by TMS Software                                                 }
{           copyright © 2010                                              }
{           Email : info@tmssoftware.com                                  }
{           Web : http://www.tmssoftware.com                              }
{                                                                         }
{ The source code is given as is. The author is not responsible           }
{ for any possible damage done due to the use of this code.               }
{ The component can be freely used in any application. The complete       }
{ source code remains property of the author and may not be distributed,  }
{ published, given or sold in any form as such. No parts of the source    }
{ code can be included in any other component or application without      }
{ written authorization of the author.                                    }
{*************************************************************************}

{$I TMSDEFS.INC}

unit AdvDBComboBoxDE;

interface

uses
  Classes, Controls, AdvDBComboBox, AdvDBComboBoxListEditor, Forms, dialogs, DB
  {$IFDEF DELPHI6_LVL}
  , DesignIntf, DesignEditors
  {$ELSE}
  , DsgnIntf
  {$ENDIF}
  ;


type

  TAdvDBComboBoxEditor = class(TDefaultEditor)
  protected
  {$IFNDEF DELPHI6_LVL}
    procedure EditProperty(PropertyEditor: TPropertyEditor; var Continue, FreeEditor: Boolean); override;
  {$ELSE}
    procedure EditProperty(const PropertyEditor:IProperty; var Continue:Boolean); override;
  {$ENDIF}
  public
    function GetVerb(index:integer):string; override;
    function GetVerbCount:integer; override;
    procedure ExecuteVerb(Index:integer); override;
  end;

  TAdvDBComboPropertyEditor = class(TClassProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure Edit; override;
    procedure SetValue(const Value: String); override;
    function GetValue: String; override;
  end;

  TAdvDBComboBoxFieldNameProperty = class(TStringProperty)
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

//------------------------------------------------------------------------------

{$IFNDEF DELPHI6_LVL}
procedure TAdvDBComboBoxEditor.EditProperty(PropertyEditor: TPropertyEditor;
  var Continue, FreeEditor: Boolean);
{$ELSE}
procedure TAdvDBComboBoxEditor.EditProperty(const PropertyEditor:IProperty; var Continue:Boolean);
{$ENDIF}
{var
  PropName: string;}
begin
  ExecuteVerb(0);
  Continue := False;
  {PropName := PropertyEditor.GetName;
  if (CompareText(PropName, 'ITEMS') = 0) then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;}
end;

//------------------------------------------------------------------------------

function GetParentForm(AComponent: TComponent): TComponent;
begin
  Result := AComponent.Owner;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxEditor.ExecuteVerb(Index: integer);
var
  ItemsEditor: TComboListEditor;
  DBCombo: TAdvDBComboBox;
begin
  case Index of
    0:
    begin
      if (Component is TAdvDBComboBox) then
      begin
        DBCombo := TAdvDBComboBox(Component);
        ItemsEditor := TComboListEditor.Create(Application);
        ItemsEditor.DBCombo := DBCombo;
        try
          if ItemsEditor.Showmodal = mrOK then
          begin
            Designer.Modified;
          end;
        finally
          ItemsEditor.Free;
        end;
      end;
    end;
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxEditor.GetVerb(index: integer): string;
begin
  Result := '';
  case Index of
    0: Result := 'Items Editor';
  end;
end;

//------------------------------------------------------------------------------

function TAdvDBComboBoxEditor.GetVerbCount: integer;
begin
 Result := 1;
end;

//------------------------------------------------------------------------------

{ TAdvDBComboPropertyEditor }

function TAdvDBComboPropertyEditor.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog];
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboPropertyEditor.Edit;
var
  ItemsEditor: TComboListEditor;
  DBCombo: TAdvDBComboBox;
begin
  if (GetComponent(0) is TAdvDBComboBox) then
  begin
    DBCombo := TAdvDBComboBox(GetComponent(0));
    ItemsEditor := TComboListEditor.Create(Application);
    ItemsEditor.DBCombo := DBCombo;
    try
      if ItemsEditor.Showmodal = mrOK then
      begin
        Modified;
      end;
    finally
      ItemsEditor.Free;
    end;
  end;
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboPropertyEditor.SetValue(const Value: String);
begin
end;

//------------------------------------------------------------------------------

function TAdvDBComboPropertyEditor.GetValue: String;
begin
  Result:='(AdvDBComboBoxItems)';
end;

//------------------------------------------------------------------------------
{ TAdvDBComboBoxFieldNameProperty }

function TAdvDBComboBoxFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList];
end;

//------------------------------------------------------------------------------

procedure TAdvDBComboBoxFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  FDBCombo: TAdvDBComboBox;
  aDataSource: TDataSource;
  FDataSet: TDataSet;
  {$IFNDEF DELPHIXE3_LVL}
  {$IFDEF DELPHI2006_LVL}
  st: TWideStringList;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  st: TStringList;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  st: TStringList;
  {$ENDIF}

  i: Integer;
begin
  if (GetComponent(0) is TAdvDBComboBox) then
    FDBCombo := (GetComponent(0) as TAdvDBComboBox)
  else
    Exit;

  aDataSource := FDBCombo.DataSource;

  if not Assigned(aDataSource) then
    Exit;

  FDataSet := aDataSource.DataSet;

  if not Assigned(FDataSet) then
    Exit;

  {$IFNDEF DELPHIXE3_LVL}
  {$IFDEF DELPHI2006_LVL}
  st := TWideStringList.Create;
  {$ENDIF}
  {$IFNDEF DELPHI2006_LVL}
  st := TStringList.Create;
  {$ENDIF}
  {$ENDIF}
  {$IFDEF DELPHIXE3_LVL}
  st := TStringList.Create;
  {$ENDIF}
  FDataSet.GetFieldNames(st);
  for i := 1 to st.Count do
    Proc(st.Strings[i - 1]);
  st.Free;
end;

//------------------------------------------------------------------------------

end.
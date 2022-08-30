{***************************************************************************}
{ TDBAdvCardList component                                                  }
{ for Delphi & C++Builder                                                   }
{                                                                           }
{ written by TMS Software                                                   }
{            copyright © 2005 - 2010                                        }
{            Email : info@tmssoftware.com                                   }
{            Web : http://www.tmssoftware.com                               }
{                                                                           }
{ The source code is given as is. The author is not responsible             }
{ for any possible damage done due to the use of this code.                 }
{ The component can be freely used in any application. The complete         }
{ source code remains property of the author and may not be distributed,    }
{ published, given or sold in any form as such. No parts of the source      }
{ code can be included in any other component or application without        }
{ written authorization of the author.                                      }
{***************************************************************************}

unit DBAdvCardListRegDE;

{$I TMSDEFS.INC}

interface

uses
  Classes, DBAdvCardList, DB,
{$IFNDEF DELPHI6_LVL}
  DsgnIntf,
{$ENDIF}
{$IFDEF DELPHI6_LVL}
  DesignIntf,
  DesignEditors,
{$ENDIF}
  TypInfo;

procedure Register;

implementation

{ TDBStringProperty }

type
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

function TDBStringProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBStringProperty.GetValueList(List: TStrings);
begin
end;

procedure TDBStringProperty.GetValues(Proc: TGetStrProc);
var
  I: Integer;
  Values: TStringList;
begin
  Values := TStringList.Create;
  try
    GetValueList(Values);
    for I := 0 to Values.Count - 1 do Proc(Values[I]);
  finally
    Values.Free;
  end;
end;

{ Utility Functions }

function GetPropertyValue(Instance: TPersistent; const PropName: string): TPersistent;
var
  PropInfo: PPropInfo;
begin
  Result := nil;
  PropInfo := TypInfo.GetPropInfo(Instance.ClassInfo, PropName);
  if (PropInfo <> nil) and (PropInfo^.PropType^.Kind = tkClass) then
    Result := TObject(GetOrdProp(Instance, PropInfo)) as TPersistent;
end;

{ TTemplateItemDataFieldEditor }

type
  TTemplateItemDataFieldProperty = class(TDBStringProperty)
    procedure GetValueList(List: TStrings); override;
  end;

procedure TTemplateItemDataFieldProperty.GetValueList(List: TStrings);
var
  CardList: TDBAdvCardList;
  DataSource: TDataSource;
  Pers: TPersistent;
begin
  Pers := GetComponent(0);
  if Pers is TDBAdvCardTemplateItem then
    CardList := TDBAdvCardTemplateItem(Pers).CardList
  else
    CardList := TDBAdvCardTemplate(Pers).CardList;

  if (CardList = nil) then Exit;
  DataSource := GetPropertyValue(CardList, 'DataSource') as TDataSource;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    DataSource.DataSet.GetFieldNames(List);
end;

procedure Register;
begin
  RegisterPropertyEditor(TypeInfo(string), TDBAdvCardTemplateItem, 'FieldName', TTemplateItemDataFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TDBAdvCardTemplate, 'CaptionFieldName', TTemplateItemDataFieldProperty);
end;

end.

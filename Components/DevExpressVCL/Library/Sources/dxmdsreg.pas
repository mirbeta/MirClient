{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressMemData                                           }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
{           ALL RIGHTS RESERVED                                      }
{                                                                    }
{   The entire contents of this file is protected by U.S. and        }
{   International Copyright Laws. Unauthorized reproduction,         }
{   reverse-engineering, and distribution of all or any portion of   }
{   the code contained in this file is strictly prohibited and may   }
{   result in severe civil and criminal penalties and will be        }
{   prosecuted to the maximum extent possible under the law.         }
{                                                                    }
{   RESTRICTIONS                                                     }
{                                                                    }
{   THIS SOURCE CODE AND ALL RESULTING INTERMEDIATE FILES            }
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSMEMDATA                     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
{                                                                    }
{   THE SOURCE CODE CONTAINED WITHIN THIS FILE AND ALL RELATED       }
{   FILES OR ANY PORTION OF ITS CONTENTS SHALL AT NO TIME BE         }
{   COPIED, TRANSFERRED, SOLD, DISTRIBUTED, OR OTHERWISE MADE        }
{   AVAILABLE TO OTHER INDIVIDUALS WITHOUT EXPRESS WRITTEN CONSENT   }
{   AND PERMISSION FROM DEVELOPER EXPRESS INC.                       }
{                                                                    }
{   CONSULT THE END USER LICENSE AGREEMENT FOR INFORMATION ON        }
{   ADDITIONAL RESTRICTIONS.                                         }
{                                                                    }
{********************************************************************}

unit dxmdsreg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignEditors, DesignIntf, Windows, Classes, DB, dxmdaset, dxmdsedt, dxmdatps, ShellAPI, SysUtils, dxCore, dxCoreReg;

const
  dxMemDataProductName = 'ExpressMemData';
  GetVerbSt0 = 'Field Editor ...';
  GetVerbSt1 = 'Persistent Editor...';

{TdxMemDataComponentEditor}
type
  TdxMemDataComponentEditor = class(TdxComponentEditor)
  private
    function GetMemData: TdxMemData;
  protected
    function IsLinkable: Boolean; override;
    procedure DoLinkTo(AObject: TObject); override;
    function GetLinkToItemCaption: string; override;
    function GetLinkToTypeClass: TClass; override;

    function GetProductName: string; override;
    function InternalGetVerb(AIndex: Integer): string; override;
    function InternalGetVerbCount: Integer; override;
    procedure InternalExecuteVerb(AIndex: Integer); override;

    property MemData: TdxMemData read GetMemData;
  end;

function TdxMemDataComponentEditor.IsLinkable: Boolean;
begin
  Result := True;
end;

procedure TdxMemDataComponentEditor.DoLinkTo(AObject: TObject);
var
  AActive: Boolean;
  AOption: TdxMemPersistentOption;
  ADataSet: TDataSet;
begin
  ADataSet := AObject as TDataSet;
  AActive := ADataSet.Active;
  ADataSet.Active := True;
  try
    AOption := MemData.Persistent.Option;
    MemData.Persistent.Option := poNone;
    try
      MemData.CreateFieldsFromDataSet(ADataSet, MemData.Owner);
      MemData.LoadFromDataSet(ADataSet);
      MemData.Persistent.SaveData;
      MemData.Open;
    finally
      MemData.Persistent.Option := AOption;
    end
  finally
    ADataSet.Active := AActive;
  end;
  Designer.Modified;
end;

function TdxMemDataComponentEditor.GetLinkToItemCaption: string;
begin
  Result := 'Assign Data From';
end;

function TdxMemDataComponentEditor.GetLinkToTypeClass: TClass;
begin
  Result := TDataSet;
end;

function TdxMemDataComponentEditor.GetProductName: string;
begin
  Result := dxMemDataProductName;
end;

function TdxMemDataComponentEditor.InternalGetVerb(AIndex: Integer): string;
begin
  case AIndex of
    0: Result := GetVerbSt0;
    1: Result := GetVerbSt1;
  end;
end;

function TdxMemDataComponentEditor.InternalGetVerbCount: Integer;
begin
  Result := 2;
end;

procedure TdxMemDataComponentEditor.InternalExecuteVerb(AIndex: Integer);
begin
  case AIndex of
    0: ShowMemDataFieldEditor(MemData, Designer);
    1: ShowMemDataPersistentEditor(MemData, Designer);
  end;
end;

function TdxMemDataComponentEditor.GetMemData: TdxMemData;
begin
  Result := Component as TdxMemData;
end;

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

type
  TDBSortedFieldProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDBSortedFieldProperty.GetValueList(List: TStrings);
var
  i : Integer;
begin
  with TDataSet(GetComponent(0)) do
    for i := 0 to FieldCount - 1 do
      if(Fields[i].Owner = Owner)
      and (Fields[i].FieldName <> '')then
        List.Add(Fields[i].FieldName);
end;

type
  TDBIndexedFieldProperty = class(TDBStringProperty)
  public
    procedure GetValueList(List: TStrings); override;
  end;

procedure TDBIndexedFieldProperty.GetValueList(List: TStrings);
var
  i : Integer;
  MemData: TdxCustomMemData;
begin
  MemData := TdxMemIndex(GetComponent(0)).MemData;
  for i := 0 to MemData.FieldCount - 1 do
    if(MemData.Fields[i].Owner = MemData.Owner)
    and (MemData.Fields[i].FieldName <> '')then
      List.Add(MemData.Fields[i].FieldName);
end;

type
  TMemDataPersistentProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

procedure TMemDataPersistentProperty.Edit;
begin
  ShowMemDataPersistentEditor(TdxMemPersistent(GetOrdValue).MemData, Designer);
end;

function TMemDataPersistentProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paMultiSelect, paSubProperties, paDialog, paReadOnly];
end;

procedure Register;
begin
  ForceDemandLoadState(dlDisable);

  RegisterComponents(dxCoreLibraryProductPage, [TdxMemData]);

  RegisterComponentEditor(TdxMemData, TdxMemDataComponentEditor);
  RegisterPropertyEditor(TypeInfo(string), TdxMemData, 'SortedField',
     TDBSortedFieldProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxMemIndex, 'FieldName',
     TDBIndexedFieldProperty);
  RegisterPropertyEditor(TypeInfo(TdxMemPersistent), nil, '',
     TMemDataPersistentProperty);
end;

end.

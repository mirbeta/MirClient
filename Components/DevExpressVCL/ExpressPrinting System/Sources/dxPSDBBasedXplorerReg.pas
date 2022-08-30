{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPrinting System                                   }
{                                                                    }
{           Copyright (C) 1998-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSPRINTING SYSTEM AND            }
{   ALL ACCOMPANYING VCL CONTROLS AS PART OF AN                      }
{   EXECUTABLE PROGRAM ONLY.                                         }
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

unit dxPSDBBasedXplorerReg;

interface

{$I cxVer.inc}

procedure Register;

implementation

uses
  DesignIntf,
{$IFDEF CBUILDER10}
  DesignEditors,
{$ENDIF}
{$IFNDEF CBUILDER10}
  DBReg,
{$ENDIF}
  Classes, DB, dxPSGlbl, dxPSDBBasedXplorer, dxPSReg;

type
{$IFDEF CBUILDER10}
  TDBStringProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValueList(List: TStrings); virtual;
    procedure GetValues(Proc: TGetStrProc); override;
  end;
{$ENDIF}

  TdxDBFieldNameProperty = class(TDBStringProperty)
  protected
    function AcceptField(AField: TField): Boolean; virtual;
    function GetDataSet: TDataSet; virtual;
    procedure GetValueList(List: TStrings); override;
  public
    property DataSet: TDataSet read GetDataSet;
  end;

  TdxDBFolderFieldNameProperty = class(TdxDBFieldNameProperty)
  protected
    function GetDataSet: TDataSet; override;
  end;

  TdxDBFolderIDFieldNameProperty = class(TdxDBFolderFieldNameProperty)
  protected
    function AcceptField(AField: TField): Boolean; override;
  end;

  TdxDBItemFieldNameProperty = class(TdxDBFieldNameProperty)
  protected
    function GetDataSet: TDataSet; override;
  end;

  TdxDBItemDataFieldNameProperty = class(TdxDBItemFieldNameProperty)
  protected
    function AcceptField(AField: TField): Boolean; override;
  end;

  TdxDBItemIDFieldNameProperty = class(TdxDBItemFieldNameProperty)
  protected
    function AcceptField(AField: TField): Boolean; override;
  end;

{$IFDEF CBUILDER10}
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
{$ENDIF}

{ TdxDBFieldNameProperty }

function TdxDBFieldNameProperty.AcceptField(AField: TField): Boolean;
begin
  Result := True;
end;

function TdxDBFieldNameProperty.GetDataSet: TDataSet;
begin
  Result := nil;
end;

procedure TdxDBFieldNameProperty.GetValueList(List: TStrings);
var
  I: Integer;
  Field: TField;
begin
  if DataSet <> nil then
  begin
  {$WARN SYMBOL_DEPRECATED OFF}
    DataSet.GetFieldNames(List);
  {$WARN SYMBOL_DEPRECATED ON}
    for I := List.Count - 1 downto 0 do
    begin
      Field := TField(List.Objects[I]);
      if not AcceptField(Field) then List.Delete(I);
    end;
  end;
end;

{ TdxDBFolderFieldNameProperty }

function TdxDBFolderFieldNameProperty.GetDataSet: TDataSet;
begin
  Result := TdxPSDBBasedExplorerFoldersFieldNamesMap(GetComponent(0)).Explorer.Folders;
end;

{ TdxDBFolderDBFieldNameProperty }

function TdxDBFolderIDFieldNameProperty.AcceptField(AField: TField): Boolean;
begin
  Result := (AField is TIntegerField) or (AField is TSmallintField) or
    (AField is TLargeintField ) or (AField is TWordField) or (AField is TAutoIncField);
end;

{ TdxDBItemFieldNameProperty }

function TdxDBItemFieldNameProperty.GetDataSet: TDataSet;
begin
  Result := TdxPSDBBasedExplorerItemsFieldNamesMap(GetComponent(0)).Explorer.Items;
end;

{ TdxDBItemDataFieldNameProperty }

function TdxDBItemDataFieldNameProperty.AcceptField(AField: TField): Boolean;
begin
  Result := AField is TBlobField;
end;

{ TdxDBItemIDFieldNameProperty }

function TdxDBItemIDFieldNameProperty.AcceptField(AField: TField): Boolean;
begin
  Result := (AField is TIntegerField) or (AField is TSmallintField) or
    (AField is TLargeintField ) or (AField is TWordField) or (AField is TAutoIncField);
end;

procedure Register;
begin
  RegisterComponents(dxPSProductPage, [TdxPSDBBasedExplorer]);

  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerFoldersFieldNamesMap, 'ID', TdxDBFolderIDFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerFoldersFieldNamesMap, 'Name', TdxDBFolderFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerFoldersFieldNamesMap, 'ParentID', TdxDBFolderIDFieldNameProperty);

  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerItemsFieldNamesMap, 'ID', TdxDBItemIDFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerItemsFieldNamesMap, 'Name', TdxDBItemFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerItemsFieldNamesMap, 'ParentID', TdxDBItemIDFieldNameProperty);
  RegisterPropertyEditor(TypeInfo(string), TdxPSDBBasedExplorerItemsFieldNamesMap, 'Data', TdxDBItemDataFieldNameProperty);
end;

end.


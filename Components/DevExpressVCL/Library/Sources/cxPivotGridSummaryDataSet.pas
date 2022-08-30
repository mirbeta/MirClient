{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxPivotGridSummaryDataSet;

{$I cxVer.inc}

interface

uses
  Classes, DB, cxClasses, dxmdaset, cxCustomPivotGrid, cxPivotGridCustomDataSet, cxDataStorage;

type
  TcxCustomPivotGridSummaryDataSet = class(TcxPivotGridCustomDataSet)
  protected
    function GetFieldType(APivotGridField: TcxPivotGridField): TFieldType; override;

    procedure DataChanged(Sender: TcxCustomPivotGrid); override;
    procedure LayoutChanged(Sender: TcxCustomPivotGrid); override;

    function DoCreateField(APivotGridField: TcxPivotGridField): TField; override;
    procedure DoCreateFields; override;
    procedure DoPopulate; override;
  end;

  TcxPivotGridSummaryDataSet = class(TcxCustomPivotGridSummaryDataSet)
  published
    property PivotGrid;
    property SynchronizeData;
    property OnCreateField;
    property OnDataChanged;
  end;

implementation

uses
  Variants;

type
  TcxPivotGridGroupItemAccess = class(TcxPivotGridGroupItem);

{ TcxCustomPivotGridSummaryDataSet }

function TcxCustomPivotGridSummaryDataSet.GetFieldType(APivotGridField: TcxPivotGridField): TFieldType;
begin
  if APivotGridField.Area = faData then
  begin
    Result := GetDefaultFieldType(APivotGridField);
    if APivotGridField.SummaryType = stSum then
    begin
      if Result in [ftSmallint, ftInteger, ftWord, ftLargeint] then
        Result := ftFloat
    end
    else
      if Result <> ftString then
        Result := ftFloat;
  end
  else
    Result := inherited GetFieldType(APivotGridField);
end;

procedure TcxCustomPivotGridSummaryDataSet.DataChanged(Sender: TcxCustomPivotGrid);
begin
  if SynchronizeData then
    CreateData;
end;

procedure TcxCustomPivotGridSummaryDataSet.LayoutChanged(Sender: TcxCustomPivotGrid);
begin
  if SynchronizeData then
    CreateData;
end;

function TcxCustomPivotGridSummaryDataSet.DoCreateField(APivotGridField: TcxPivotGridField): TField;
begin
  Result := inherited DoCreateField(APivotGridField);
  Result.Tag := Integer(APivotGridField.Area) + 1;
end;

procedure TcxCustomPivotGridSummaryDataSet.DoCreateFields;
var
  I: Integer;
begin
  for I := 0 to DataBuilder.RowFields.Count - 1 do
    DoCreateField(DataBuilder.RowFields[I]);

  for I := 0 to DataBuilder.ColumnFields.Count - 1 do
    DoCreateField(DataBuilder.ColumnFields[I]);

  for I := 0 to DataBuilder.DataFields.Count - 1 do
    DoCreateField(DataBuilder.DataFields[I]);
end;

procedure TcxCustomPivotGridSummaryDataSet.DoPopulate;

var
  ARowFieldStartIndex, AColumnFieldStartIndex, ADataFieldStartIndex: Integer;

  procedure DrillColumn(ARowItem, AColumnItem: TcxPivotGridGroupItem);

    procedure SetFieldValue(AFieldIndex: Integer; AValue: Variant);
    begin
      if (VarType(AValue) = varString) and not (Fields[AFieldIndex] is TStringField)  then
        Fields[AFieldIndex].Value := Null
      else
        Fields[AFieldIndex].Value := AValue
    end;

  var
    I: Integer;
    ACurrentGroup: TcxPivotGridGroupItem;
    ANeedPost: Boolean;
    AValue: Variant;
  begin
    if AColumnItem.ItemCount = 0 then
    begin
      Append;

      ACurrentGroup := ARowItem;
      for I := ARowItem.Level downto 0 do
      begin
        SetFieldValue(ARowFieldStartIndex + I,
          TcxPivotGridGroupItemAccess(ACurrentGroup).DisplayValue);
        ACurrentGroup := ACurrentGroup.Parent;
      end;

      ACurrentGroup := AColumnItem;
      for I := AColumnItem.Level downto 0 do
      begin
        SetFieldValue(AColumnFieldStartIndex + I,
          TcxPivotGridGroupItemAccess(ACurrentGroup).DisplayValue);
        ACurrentGroup := ACurrentGroup.Parent;
      end;

      ANeedPost := False;
      for I := 0 to DataBuilder.DataFields.Count - 1 do
      begin
        AValue := DataBuilder.CrossCells[ARowItem, AColumnItem].GetCrossCellValue(DataBuilder.DataFields[I]);
        if not VarIsNull(AValue) or ((DataBuilder.DataFields[I].DataBinding.ValueTypeClass = TcxStringValueType) or
          (DataBuilder.DataFields[I].DataBinding.ValueTypeClass = TcxWideStringValueType)) then
        begin
          SetFieldValue(ADataFieldStartIndex + I, AValue);
          ANeedPost := True;
        end;
      end;

      if ANeedPost then
        Post
      else
        Cancel;
    end
    else
      for I := 0 to AColumnItem.ItemCount - 1 do
        DrillColumn(ARowItem, AColumnItem.Items[I]);
  end;

  procedure DrillRow(ARowItem, AColumnItem: TcxPivotGridGroupItem);
  var
    I: Integer;
  begin
    if ARowItem.ItemCount = 0 then
    begin
      DrillColumn(ARowItem, AColumnItem);
    end
    else
      for I := 0 to ARowItem.ItemCount - 1 do
        DrillRow(ARowItem.Items[I], AColumnItem)
  end;

begin
  inherited;

  ARowFieldStartIndex := 1;
  AColumnFieldStartIndex := ARowFieldStartIndex + DataBuilder.RowFields.Count;
  ADataFieldStartIndex := AColumnFieldStartIndex + DataBuilder.ColumnFields.Count;

  DrillRow(DataBuilder.Rows, DataBuilder.Columns);
end;

end.

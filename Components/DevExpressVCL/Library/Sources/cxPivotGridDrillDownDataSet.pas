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

unit cxPivotGridDrillDownDataSet;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, DB, cxClasses, dxmdaset, cxCustomPivotGrid, cxPivotGridCustomDataSet;

type
  TcxCustomPivotGridDrillDownDataSet = class(TcxPivotGridCustomDataSet)
  private
    FDrillDownDataSource: TcxPivotGridCrossCellDataSource;
  protected
    procedure SelectionChanged(Sender: TcxCustomPivotGrid); override;

    procedure DoAssignFieldProperties(AField: TField; APivotGridField: TcxPivotGridField); override;
    procedure DoCreateData; override;
    procedure DoCreateFields; override;
    procedure DoPopulate; override;
  end;

  TcxPivotGridDrillDownDataSet = class(TcxCustomPivotGridDrillDownDataSet)
  published
    property PivotGrid;
    property SynchronizeData;
    property OnCreateField;
    property OnDataChanged;
  end;

implementation

uses
  cxCustomData;

{ TcxCustomPivotGridDrillDownDataSet }

procedure TcxCustomPivotGridDrillDownDataSet.DoPopulate;
var
  I, J: Integer;
begin
  inherited;

  for I := 0 to FDrillDownDataSource.RecordCount - 1 do
  begin
    Append;
    for J := 0 to FDrillDownDataSource.FieldCount - 1 do
      Fields[J+1{RecID}].Value := FDrillDownDataSource.Values[TcxDataRecordHandle(I), TcxDataItemHandle(J)];
    Post;
  end;
end;

procedure TcxCustomPivotGridDrillDownDataSet.DoAssignFieldProperties(AField: TField; APivotGridField: TcxPivotGridField);
begin
  inherited DoAssignFieldProperties(AField, APivotGridField);
  if IsDBFieldAssigned(APivotGridField) and (APivotGridField.GroupInterval = giDefault) and (APivotGridField.Properties = nil) then
    AField.Size := GetDBField(APivotGridField).Size;
end;

procedure TcxCustomPivotGridDrillDownDataSet.DoCreateData;
begin
  FDrillDownDataSource := TcxPivotGridCrossCellDataSource(PivotGrid.CreateDrillDownDataSource);
  try
    inherited DoCreateData;
  finally
    FreeAndNil(FDrillDownDataSource);
  end;
end;

procedure TcxCustomPivotGridDrillDownDataSet.DoCreateFields;
var
  I: Integer;
begin
  for I := 0 to FDrillDownDataSource.FieldCount - 1 do
    DoCreateField(FDrillDownDataSource.PivotGridFields[I]);
end;

procedure TcxCustomPivotGridDrillDownDataSet.SelectionChanged(
  Sender: TcxCustomPivotGrid);
begin
  if SynchronizeData then
    CreateData;
end;

end.


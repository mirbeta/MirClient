{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressQuantumGrid                                       }
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
{   (DCU, OBJ, DLL, ETC.) ARE CONFIDENTIAL AND PROPRIETARY TRADE     }
{   SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER IS    }
{   LICENSED TO DISTRIBUTE THE EXPRESSQUANTUMGRID AND ALL            }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxGridDBBandedTableView;

{$I cxVer.inc}

interface

uses
  Classes, cxStorage, cxCustomData, cxDBData,
  cxGridCustomTableView, cxGridDBDataDefinitions, cxGridBandedTableView;

type
  TcxGridDBBandedTableView = class;

  TcxGridDBBandedColumn = class(TcxGridBandedColumn)
  private
    function GetDataBinding: TcxGridItemDBDataBinding;
    procedure SetDataBinding(Value: TcxGridItemDBDataBinding);
  published
    property DataBinding: TcxGridItemDBDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBBandedTableView = class(TcxGridBandedTableView)
  private
    function GetColumn(Index: Integer): TcxGridDBBandedColumn;
    function GetDataController: TcxGridDBDataController;
    procedure SetColumn(Index: Integer; Value: TcxGridDBBandedColumn);
    procedure SetDataController(Value: TcxGridDBDataController);
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
  public
    function CreateColumn: TcxGridDBBandedColumn;
    function GetColumnByFieldName(const AFieldName: string): TcxGridDBBandedColumn;
    property Columns[Index: Integer]: TcxGridDBBandedColumn read GetColumn write SetColumn;
  published
    property DataController: TcxGridDBDataController read GetDataController write SetDataController;
  end;

implementation

uses
  cxGridCustomView, cxGridDBTableView;

{ TcxGridDBBandedColumn }

function TcxGridDBBandedColumn.GetDataBinding: TcxGridItemDBDataBinding;
begin
  Result := TcxGridItemDBDataBinding(inherited DataBinding);
end;

procedure TcxGridDBBandedColumn.SetDataBinding(Value: TcxGridItemDBDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBBandedTableView }

function TcxGridDBBandedTableView.GetColumn(Index: Integer): TcxGridDBBandedColumn;
begin
  Result := TcxGridDBBandedColumn(inherited Columns[Index]);
end;

function TcxGridDBBandedTableView.GetDataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(FDataController);
end;

procedure TcxGridDBBandedTableView.SetColumn(Index: Integer; Value: TcxGridDBBandedColumn);
begin
  inherited Columns[Index] := Value;
end;

procedure TcxGridDBBandedTableView.SetDataController(Value: TcxGridDBDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridDBBandedTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDBDataController;
end;

function TcxGridDBBandedTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridDBBandedColumn;
end;

function TcxGridDBBandedTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridDBTableSummaryItem;
end;

function TcxGridDBBandedTableView.CreateColumn: TcxGridDBBandedColumn;
begin
  Result := TcxGridDBBandedColumn(inherited CreateColumn);
end;

function TcxGridDBBandedTableView.GetColumnByFieldName(const AFieldName: string): TcxGridDBBandedColumn;
begin
  Result := TcxGridDBBandedColumn(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridDBBandedTableView, 'DB Banded Table');
  Classes.RegisterClass(TcxGridDBBandedColumn);

finalization
  cxGridRegisteredViews.Unregister(TcxGridDBBandedTableView);

end.

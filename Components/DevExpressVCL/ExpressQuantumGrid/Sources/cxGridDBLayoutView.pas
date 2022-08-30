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

unit cxGridDBLayoutView;

{$I cxVer.inc}

interface

uses
  Classes, cxGridCustomTableView, cxGridLayoutView, cxGridDBDataDefinitions,
  cxCustomData, cxDBData;

type
  TcxGridDBLayoutViewItem = class(TcxGridLayoutViewItem)
  private
    function GetDataBinding: TcxGridItemDBDataBinding;
    procedure SetDataBinding(Value: TcxGridItemDBDataBinding);
  published
    property DataBinding: TcxGridItemDBDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBLayoutView = class(TcxGridLayoutView)
  private
    function GetDataController: TcxGridDBDataController;
    function GetItem(Index: Integer): TcxGridDBLayoutViewItem;
    procedure SetDataController(Value: TcxGridDBDataController);
    procedure SetItem(Index: Integer; Value: TcxGridDBLayoutViewItem);
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
  public
    function CreateItem: TcxGridDBLayoutViewItem;
    function GetItemByFieldName(const AFieldName: string): TcxGridDBLayoutViewItem;
    property Items[Index: Integer]: TcxGridDBLayoutViewItem read GetItem write SetItem;
  published
    property DataController: TcxGridDBDataController read GetDataController write SetDataController;
  end;

implementation

uses
  cxGridCustomView;

{ TcxGridDBLayoutViewRow }

function TcxGridDBLayoutViewItem.GetDataBinding: TcxGridItemDBDataBinding;
begin
  Result := TcxGridItemDBDataBinding(inherited DataBinding);
end;

procedure TcxGridDBLayoutViewItem.SetDataBinding(Value: TcxGridItemDBDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBLayoutView }

function TcxGridDBLayoutView.GetDataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(FDataController);
end;

function TcxGridDBLayoutView.GetItem(Index: Integer): TcxGridDBLayoutViewItem;
begin
  Result := TcxGridDBLayoutViewItem(inherited Items[Index]);
end;

procedure TcxGridDBLayoutView.SetDataController(Value: TcxGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridDBLayoutView.SetItem(Index: Integer; Value: TcxGridDBLayoutViewItem);
begin
  inherited Items[Index] := Value;
end;

function TcxGridDBLayoutView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDBDataController;
end;

function TcxGridDBLayoutView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridDBLayoutViewItem;
end;

{function TcxGridDBLayoutView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridDBLayoutViewSummaryItem;
end;}

function TcxGridDBLayoutView.CreateItem: TcxGridDBLayoutViewItem;
begin
  Result := TcxGridDBLayoutViewItem(inherited CreateItem);
end;

function TcxGridDBLayoutView.GetItemByFieldName(const AFieldName: string): TcxGridDBLayoutViewItem;
begin
  Result := TcxGridDBLayoutViewItem(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridDBLayoutView, 'DB Layout');
  Classes.RegisterClass(TcxGridDBLayoutViewItem);

finalization
  cxGridRegisteredViews.Unregister(TcxGridDBLayoutView);

end.


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

unit cxGridDBWinExplorerView;

{$I cxVer.inc}

interface

uses
  Classes, cxGridCustomTableView, cxGridWinExplorerView, cxGridDBDataDefinitions,
  cxCustomData, cxDBData;

type
  TcxGridDBWinExplorerViewItem = class(TcxGridWinExplorerViewItem)
  private
    function GetDataBinding: TcxGridItemDBDataBinding;
    procedure SetDataBinding(Value: TcxGridItemDBDataBinding);
  published
    property DataBinding: TcxGridItemDBDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBWinExplorerView = class(TcxGridWinExplorerView)
  private
    function GetDataController: TcxGridDBDataController;
    function GetItem(Index: Integer): TcxGridDBWinExplorerViewItem;
    procedure SetDataController(Value: TcxGridDBDataController);
    procedure SetItem(Index: Integer; Value: TcxGridDBWinExplorerViewItem);
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
  public
    function CreateItem: TcxGridDBWinExplorerViewItem;
    function GetItemByFieldName(const AFieldName: string): TcxGridDBWinExplorerViewItem;
    property Items[Index: Integer]: TcxGridDBWinExplorerViewItem read GetItem write SetItem;
  published
    property DataController: TcxGridDBDataController read GetDataController write SetDataController;
  end;

implementation

uses
  cxGridCustomView;

{ TcxGridDBLayoutViewRow }

function TcxGridDBWinExplorerViewItem.GetDataBinding: TcxGridItemDBDataBinding;
begin
  Result := TcxGridItemDBDataBinding(inherited DataBinding);
end;

procedure TcxGridDBWinExplorerViewItem.SetDataBinding(Value: TcxGridItemDBDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBLayoutView }

function TcxGridDBWinExplorerView.GetDataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(FDataController);
end;

function TcxGridDBWinExplorerView.GetItem(Index: Integer): TcxGridDBWinExplorerViewItem;
begin
  Result := TcxGridDBWinExplorerViewItem(inherited Items[Index]);
end;

procedure TcxGridDBWinExplorerView.SetDataController(Value: TcxGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridDBWinExplorerView.SetItem(Index: Integer; Value: TcxGridDBWinExplorerViewItem);
begin
  inherited Items[Index] := Value;
end;

function TcxGridDBWinExplorerView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDBDataController;
end;

function TcxGridDBWinExplorerView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridDBWinExplorerViewItem;
end;

function TcxGridDBWinExplorerView.CreateItem: TcxGridDBWinExplorerViewItem;
begin
  Result := TcxGridDBWinExplorerViewItem(inherited CreateItem);
end;

function TcxGridDBWinExplorerView.GetItemByFieldName(const AFieldName: string): TcxGridDBWinExplorerViewItem;
begin
  Result := TcxGridDBWinExplorerViewItem(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridDBWinExplorerView, 'DB WinExplorer');
  Classes.RegisterClass(TcxGridDBWinExplorerViewItem);

finalization
  cxGridRegisteredViews.Unregister(TcxGridDBWinExplorerView);

end.


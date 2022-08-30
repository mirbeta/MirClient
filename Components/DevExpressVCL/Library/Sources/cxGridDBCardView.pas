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

unit cxGridDBCardView;

interface

uses
  Classes, cxGridCustomTableView, cxGridCardView, cxGridDBDataDefinitions,
  cxCustomData, cxDBData;

type
  TcxGridDBCardViewRow = class(TcxGridCardViewRow)
  private
    function GetDataBinding: TcxGridItemDBDataBinding;
    procedure SetDataBinding(Value: TcxGridItemDBDataBinding);
  protected
    // IcxStoredObject
    {function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;}
  published
    property DataBinding: TcxGridItemDBDataBinding read GetDataBinding write SetDataBinding;
  end;

  {TcxGridDBCardViewSummaryItem = class(TcxDBDataSummaryItem)
  private
    function GetRow: TcxGridDBCardViewRow;
    procedure SetRow(Value: TcxGridDBCardViewRow);
  published
    property Row: TcxGridDBCardViewRow read GetRow write SetRow;
  end;}

  TcxGridDBCardView = class(TcxGridCardView)
  private
    function GetDataController: TcxGridDBDataController;
    function GetRow(Index: Integer): TcxGridDBCardViewRow;
    procedure SetDataController(Value: TcxGridDBDataController);
    procedure SetRow(Index: Integer; Value: TcxGridDBCardViewRow);
  protected
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    //function GetSummaryItemClass: TcxDataSummaryItemClass; override;
  public
    function CreateRow: TcxGridDBCardViewRow;
    function GetRowByFieldName(const AFieldName: string): TcxGridDBCardViewRow;
    property Rows[Index: Integer]: TcxGridDBCardViewRow read GetRow write SetRow;
  published
    property DataController: TcxGridDBDataController read GetDataController write SetDataController;
  end;

implementation

uses
  cxGridCustomView;

{ TcxGridDBCardViewRow }

function TcxGridDBCardViewRow.GetDataBinding: TcxGridItemDBDataBinding;
begin
  Result := TcxGridItemDBDataBinding(inherited DataBinding);
end;

procedure TcxGridDBCardViewRow.SetDataBinding(Value: TcxGridItemDBDataBinding);
begin
  inherited DataBinding := Value;
end;

{function TcxGridDBCardViewRow.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  inherited GetStoredProperties(AProperties);
  with AProperties do
    Add('FieldName');
  Result := True;
end;

procedure TcxGridDBCardViewRow.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'FieldName' then
    AValue := DataBinding.FieldName
  else
    inherited;
end;

procedure TcxGridDBCardViewRow.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'FieldName' then
    DataBinding.FieldName := AValue
  else
    inherited;
end;}

(*{ TcxGridDBCardViewSummaryItem }

function TcxGridDBCardViewSummaryItem.GetRow: TcxGridDBCardViewRow;
begin
  Result := TcxGridDBCardViewRow(ItemLink);
end;

procedure TcxGridDBCardViewSummaryItem.SetRow(Value: TcxGridDBCardViewRow);
begin
  ItemLink := Value;
end;*)

{ TcxGridDBCardView }

function TcxGridDBCardView.GetDataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(FDataController);
end;

function TcxGridDBCardView.GetRow(Index: Integer): TcxGridDBCardViewRow;
begin
  Result := TcxGridDBCardViewRow(inherited Rows[Index]);
end;

procedure TcxGridDBCardView.SetDataController(Value: TcxGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridDBCardView.SetRow(Index: Integer; Value: TcxGridDBCardViewRow);
begin
  inherited Rows[Index] := Value;
end;

function TcxGridDBCardView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDBDataController;
end;

function TcxGridDBCardView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridDBCardViewRow;
end;

{function TcxGridDBCardView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridDBCardViewSummaryItem;
end;}

function TcxGridDBCardView.CreateRow: TcxGridDBCardViewRow;
begin
  Result := TcxGridDBCardViewRow(inherited CreateRow);
end;

function TcxGridDBCardView.GetRowByFieldName(const AFieldName: string): TcxGridDBCardViewRow;
begin
  Result := TcxGridDBCardViewRow(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridDBCardView, 'DB Cards');
  Classes.RegisterClass(TcxGridDBCardViewRow);

finalization
  cxGridRegisteredViews.Unregister(TcxGridDBCardView);

end.

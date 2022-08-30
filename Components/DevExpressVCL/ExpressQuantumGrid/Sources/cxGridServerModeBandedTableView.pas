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

unit cxGridServerModeBandedTableView;

{$I cxVer.inc}

interface

uses
  Classes, cxGridCustomTableView, cxGridTableView, cxGridServerModeTableView,
  cxGridCustomView, cxGraphics, dxCore, cxGridBandedTableView, cxGridServerModeDataDefinitions,
  cxCustomData, cxGridDBDataDefinitions, dxServerModeData, cxStorage,
  cxDataControllerConditionalFormatting;

type
  TcxGridServerModeBandedTableView = class;

  TcxGridServerModeBandedColumnOptions = class(TcxCustomGridBandedColumnOptions)
  published
    property AutoWidthSizable;
    property CellMerging;
    property EditAutoHeight;
    property GroupFooters;
    property Grouping;
    property HorzSizing;
    property Moving;
    property ShowCaption;
    property Sorting;
    property VertSizing;
  end;

  { TcxGridServerModeBandedColumn }

  TcxGridServerModeBandedColumn = class(TcxGridBandedColumn)
  private
    function GetDataBinding: TcxGridItemServerModeDataBinding;
    function GetGridView: TcxGridServerModeBandedTableView;
    procedure SetDataBinding(Value: TcxGridItemServerModeDataBinding);
    function GetOptions: TcxGridServerModeBandedColumnOptions;
    procedure SetOptions(const Value: TcxGridServerModeBandedColumnOptions);
  protected
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    procedure GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer); override;
  public
    property GridView: TcxGridServerModeBandedTableView read GetGridView;
  published
    property DataBinding: TcxGridItemServerModeDataBinding read GetDataBinding write SetDataBinding;
    property Options: TcxGridServerModeBandedColumnOptions read GetOptions write SetOptions;
  end;

  { TcxGridServerModeBandedSummaryItem }

  TcxGridServerModeBandedSummaryItem = class(TdxServerModeSummaryItem,
    IUnknown, IcxStoredObject, IcxGridSummaryItem)
  private
    FDisplayText: string;
    FVisibleForCustomization: Boolean;
    function GetColumn: TcxGridServerModeBandedColumn;
    function GetGridView: TcxGridServerModeBandedTableView;
    procedure SetColumn(Value: TcxGridServerModeBandedColumn);
    procedure SetDisplayText(const Value: string);
    procedure SetVisibleForCustomization(Value: Boolean);
  protected
    // IInterface
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IcxGridSummaryItem
    function GetDisplayText: string;
    function GetVisibleForCustomization: Boolean;

    property GridView: TcxGridServerModeBandedTableView read GetGridView;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Column: TcxGridServerModeBandedColumn read GetColumn write SetColumn;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property Sorted;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
  end;

  { TcxGridServerModeBandedTableView }

  TcxGridServerModeBandedTableView = class(TcxGridBandedTableView)
  private
    function GetColumn(Index: Integer): TcxGridServerModeBandedColumn;
    function GetDataController: TcxGridServerModeDataController;
    procedure SetColumn(Index: Integer; Value: TcxGridServerModeBandedColumn);
    procedure SetDataController(Value: TcxGridServerModeDataController);
  protected
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    function CanEditViewLayoutAndData: Boolean; override;
    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider; override;

    function CanBeUsedAsDetail: Boolean; override;
    function CanBeUsedAsMaster: Boolean; override;
    function CanGetHitTest: Boolean; override;
    function IsCheckBoxSelectionSupported: Boolean; override;
    function IsDataRowFixingSupported: Boolean; override;
    function IsMergedGroupsSupported: Boolean; override;
    function IsPersistentMultiSelectSupported: Boolean; override;

    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
  public
    destructor Destroy; override;
    function CreateColumn: TcxGridServerModeBandedColumn;
    function GetColumnByFieldName(const AFieldName: string): TcxGridServerModeBandedColumn;
    property Columns[Index: Integer]: TcxGridServerModeBandedColumn read GetColumn write SetColumn;
  published
    property DataController: TcxGridServerModeDataController read GetDataController write SetDataController;
  end;

implementation

uses
  Math;

{ TcxGridServerModeBandedColumn }

procedure TcxGridServerModeBandedColumn.GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer);
begin
  if GridView.OptionsBehavior.BestFitMaxRecordCount > 0 then
    inherited GetBestFitRecordRange(AFirstIndex, ALastIndex)
  else
  begin
    AFirstIndex := Max(0, Controller.TopRecordIndex - GridView.ViewInfo.VisibleRecordCount);
    ALastIndex := Min(ViewData.RecordCount - 1, Controller.TopRecordIndex + 2 * GridView.ViewInfo.VisibleRecordCount);
  end;
end;

function TcxGridServerModeBandedColumn.GetDataBinding: TcxGridItemServerModeDataBinding;
begin
  Result := TcxGridItemServerModeDataBinding(inherited DataBinding);
end;

function TcxGridServerModeBandedColumn.GetGridView: TcxGridServerModeBandedTableView;
begin
  Result := TcxGridServerModeBandedTableView(inherited GridView);
end;

function TcxGridServerModeBandedColumn.GetOptions: TcxGridServerModeBandedColumnOptions;
begin
  Result := TcxGridServerModeBandedColumnOptions(inherited Options);
end;

function TcxGridServerModeBandedColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridServerModeBandedColumnOptions;
end;

procedure TcxGridServerModeBandedColumn.SetDataBinding(Value: TcxGridItemServerModeDataBinding);
begin
  inherited DataBinding := Value;
end;

procedure TcxGridServerModeBandedColumn.SetOptions(const Value: TcxGridServerModeBandedColumnOptions);
begin
  Options.Assign(Value);
end;

{ TcxGridServerModeBandedSummaryItem }

constructor TcxGridServerModeBandedSummaryItem.Create(Collection: TCollection);
begin
  inherited;
  FVisibleForCustomization := True;
end;

function TcxGridServerModeBandedSummaryItem.GetColumn: TcxGridServerModeBandedColumn;
begin
  Result := TcxGridServerModeBandedColumn(ItemLink);
end;

function TcxGridServerModeBandedSummaryItem.GetGridView: TcxGridServerModeBandedTableView;
begin
  Result := TcxGridServerModeBandedTableView(TcxGridDBDataController(DataController).GridView);
end;

procedure TcxGridServerModeBandedSummaryItem.SetColumn(Value: TcxGridServerModeBandedColumn);
begin
  ItemLink := Value;
end;

procedure TcxGridServerModeBandedSummaryItem.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
  begin
    FDisplayText := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridServerModeBandedSummaryItem.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    GridView.Changed(vcProperty);
  end;
end;

function TcxGridServerModeBandedSummaryItem.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxGridServerModeBandedSummaryItem._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridServerModeBandedSummaryItem._Release: Integer;
begin
  Result := -1;
end;

function TcxGridServerModeBandedSummaryItem.GetObjectName: string;
begin
  Result := '';
end;

function TcxGridServerModeBandedSummaryItem.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Column');
  Result := False;
end;

procedure TcxGridServerModeBandedSummaryItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Column' then
    if Column <> nil then
      AValue := (Column as IcxStoredObject).GetObjectName
    else
      AValue := '';
end;

procedure TcxGridServerModeBandedSummaryItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Column' then
    Column := TcxGridServerModeBandedColumn(TcxCustomGridTableViewAccess.FindItemByObjectName(GridView, AValue));
end;

function TcxGridServerModeBandedSummaryItem.GetDisplayText: string;
begin
  Result := DisplayText;
end;

function TcxGridServerModeBandedSummaryItem.GetVisibleForCustomization: Boolean;
begin
  Result := VisibleForCustomization;
end;

procedure TcxGridServerModeBandedSummaryItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridServerModeBandedSummaryItem then
    with TcxGridServerModeBandedSummaryItem(Source) do
    begin
      Self.DisplayText := DisplayText;
      Self.VisibleForCustomization := VisibleForCustomization;
    end;
end;

{ TcxGridServerModeBandedTableView }

destructor TcxGridServerModeBandedTableView.Destroy;
begin
  inherited Destroy;
end;

function TcxGridServerModeBandedTableView.GetColumn(Index: Integer): TcxGridServerModeBandedColumn;
begin
  Result := TcxGridServerModeBandedColumn(inherited Columns[Index]);
end;

procedure TcxGridServerModeBandedTableView.SetColumn(Index: Integer; Value: TcxGridServerModeBandedColumn);
begin
  inherited Columns[Index] := Value;
end;

function TcxGridServerModeBandedTableView.GetDataController: TcxGridServerModeDataController;
begin
  Result := TcxGridServerModeDataController(FDataController);
end;

procedure TcxGridServerModeBandedTableView.SetDataController(Value: TcxGridServerModeDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridServerModeBandedTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridServerModeDataController;
end;

function TcxGridServerModeBandedTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridServerModeBandedColumn;
end;

function TcxGridServerModeBandedTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridServerModeBandedSummaryItem;
end;

function TcxGridServerModeBandedTableView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridServerModeViewData;
end;

function TcxGridServerModeBandedTableView.CanEditViewLayoutAndData: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
begin
  Result := nil;
end;

function TcxGridServerModeBandedTableView.CanBeUsedAsDetail: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.CanBeUsedAsMaster: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.CanGetHitTest: Boolean;
begin
  Result := inherited CanGetHitTest and DataController.IsRowInfoValid;
end;

function TcxGridServerModeBandedTableView.IsCheckBoxSelectionSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.IsDataRowFixingSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.IsMergedGroupsSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.IsPersistentMultiSelectSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeBandedTableView.CreateColumn: TcxGridServerModeBandedColumn;
begin
  Result := TcxGridServerModeBandedColumn(inherited CreateColumn);
end;

function TcxGridServerModeBandedTableView.GetColumnByFieldName(const AFieldName: string): TcxGridServerModeBandedColumn;
begin
  Result := TcxGridServerModeBandedColumn(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridServerModeBandedTableView, 'Server Mode Banded Table');
  Classes.RegisterClass(TcxGridServerModeBandedColumn);

finalization
  cxGridRegisteredViews.Unregister(TcxGridServerModeBandedTableView);

end.

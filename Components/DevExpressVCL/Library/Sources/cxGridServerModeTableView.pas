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

unit cxGridServerModeTableView;

{$I cxVer.inc}

interface

uses
  Classes, cxGridCustomTableView, cxGridTableView, cxGridDBDataDefinitions,
  cxStorage, cxCustomData, cxDBData, cxGridDBTableView, cxGridCustomView,
  dxServerModeData, cxGridServerModeDataDefinitions,
  cxDataControllerConditionalFormatting;

type
  TcxGridServerModeTableView = class;

  { TcxGridServerModeColumnOptions }

  TcxGridServerModeColumnOptions = class(TcxCustomGridColumnOptions)
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
  end;

  { TcxGridServerModeColumn }

  TcxGridServerModeColumn = class(TcxGridColumn)
  private
    function GetDataBinding: TcxGridItemServerModeDataBinding;
    function GetGridView: TcxGridServerModeTableView;
    procedure SetDataBinding(Value: TcxGridItemServerModeDataBinding);
    function GetOptions: TcxGridServerModeColumnOptions;
    procedure SetOptions(const Value: TcxGridServerModeColumnOptions);
  protected
    function CanCellMerging: Boolean; override;
    function CanShowGroupFooters: Boolean; override;
    procedure GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer); override;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function HasFixedWidth: Boolean; override;
  public
    property GridView: TcxGridServerModeTableView read GetGridView;
  published
    property DataBinding: TcxGridItemServerModeDataBinding read GetDataBinding write SetDataBinding;
    property Options: TcxGridServerModeColumnOptions read GetOptions write SetOptions;
  end;

  { TcxGridServerModeSummaryItem }

  TcxGridServerModeSummaryItem = class(TdxServerModeSummaryItem,
    IUnknown, IcxStoredObject, IcxGridSummaryItem)
  private
    FDisplayText: string;
    FVisibleForCustomization: Boolean;
    function GetColumn: TcxGridServerModeColumn;
    function GetGridView: TcxGridServerModeTableView;
    procedure SetColumn(Value: TcxGridServerModeColumn);
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

    function CanSetKind(Value: TcxSummaryKind): Boolean; override;

    property GridView: TcxGridServerModeTableView read GetGridView;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Column: TcxGridServerModeColumn read GetColumn write SetColumn;
    property DisplayText: string read FDisplayText write SetDisplayText;
    property Sorted;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
  end;

  TcxGridServerModeGroupRow = class(TcxGridGroupRow)
  protected
    function GetDisplayText(Index: Integer): string; override;
  end;

  TcxGridServerModeViewData = class(TcxGridViewData)
  protected
    function GetGroupRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
  end;

  TcxGridServerModeTableView = class(TcxGridTableView)
  private
    function GetColumn(Index: Integer): TcxGridServerModeColumn;
    function GetDataController: TcxGridServerModeDataController;
    procedure SetColumn(Index: Integer; Value: TcxGridServerModeColumn);
    procedure SetDataController(Value: TcxGridServerModeDataController);
  protected
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    function CanEditViewLayoutAndData: Boolean; override;

    // IcxDataControllerConditionalFormattingProviderOwner
    function GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider; override;

    procedure AfterRestoring; override;
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
    function CreateColumn: TcxGridServerModeColumn;
    function GetColumnByFieldName(const AFieldName: string): TcxGridServerModeColumn;
    property Columns[Index: Integer]: TcxGridServerModeColumn read GetColumn write SetColumn;
  published
    property DataController: TcxGridServerModeDataController read GetDataController write SetDataController;
  end;

implementation

uses
  Math;

{ TcxGridServerModeColumn }

function TcxGridServerModeColumn.CanCellMerging: Boolean;
begin
  Result := Options.CellMerging and GridView.CanCellMerging;
end;

function TcxGridServerModeColumn.CanShowGroupFooters: Boolean;
begin
  Result := Options.GroupFooters;
end;

procedure TcxGridServerModeColumn.GetBestFitRecordRange(out AFirstIndex, ALastIndex: Integer);
begin
  if GridView.OptionsBehavior.BestFitMaxRecordCount > 0 then
    inherited GetBestFitRecordRange(AFirstIndex, ALastIndex)
  else
  begin
    AFirstIndex := Max(0, Controller.TopRecordIndex - GridView.ViewInfo.VisibleRecordCount);
    ALastIndex := Min(ViewData.RecordCount - 1, Controller.TopRecordIndex + 2 * GridView.ViewInfo.VisibleRecordCount);
  end;
end;

function TcxGridServerModeColumn.GetDataBinding: TcxGridItemServerModeDataBinding;
begin
  Result := TcxGridItemServerModeDataBinding(inherited DataBinding);
end;

function TcxGridServerModeColumn.GetGridView: TcxGridServerModeTableView;
begin
  Result := TcxGridServerModeTableView(inherited GridView);
end;

function TcxGridServerModeColumn.GetOptions: TcxGridServerModeColumnOptions;
begin
  Result := TcxGridServerModeColumnOptions(inherited Options);
end;

function TcxGridServerModeColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridServerModeColumnOptions;
end;

function TcxGridServerModeColumn.HasFixedWidth: Boolean;
begin
  Result := not Options.HorzSizing;
end;

procedure TcxGridServerModeColumn.SetDataBinding(Value: TcxGridItemServerModeDataBinding);
begin
//TODO:
//  inherited DataBinding := Value;
  inherited DataBinding.Assign(Value);
end;

procedure TcxGridServerModeColumn.SetOptions(
  const Value: TcxGridServerModeColumnOptions);
begin
  Options.Assign(Value);
end;

{ TcxGridServerModeSummaryItem }

function TcxGridServerModeSummaryItem.CanSetKind(
  Value: TcxSummaryKind): Boolean;
begin
  Result := GridView.IsRestoring or inherited CanSetKind(Value);
end;

constructor TcxGridServerModeSummaryItem.Create(Collection: TCollection);
begin
  inherited;
  FVisibleForCustomization := True;
end;

function TcxGridServerModeSummaryItem.GetColumn: TcxGridServerModeColumn;
begin
  Result := TcxGridServerModeColumn(ItemLink);
end;

function TcxGridServerModeSummaryItem.GetGridView: TcxGridServerModeTableView;
begin
  Result := TcxGridServerModeTableView(TcxGridDBDataController(DataController).GridView);
end;

procedure TcxGridServerModeSummaryItem.SetColumn(Value: TcxGridServerModeColumn);
begin
  ItemLink := Value;
end;

procedure TcxGridServerModeSummaryItem.SetDisplayText(const Value: string);
begin
  if FDisplayText <> Value then
  begin
    FDisplayText := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridServerModeSummaryItem.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    GridView.Changed(vcProperty);
  end;
end;

function TcxGridServerModeSummaryItem.QueryInterface(const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxGridServerModeSummaryItem._AddRef: Integer;
begin
  Result := -1;
end;

function TcxGridServerModeSummaryItem._Release: Integer;
begin
  Result := -1;
end;

function TcxGridServerModeSummaryItem.GetObjectName: string;
begin
  Result := '';
end;

function TcxGridServerModeSummaryItem.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('Column');
  Result := False;
end;

procedure TcxGridServerModeSummaryItem.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Column' then
    if Column <> nil then
      AValue := (Column as IcxStoredObject).GetObjectName
    else
      AValue := '';
end;

procedure TcxGridServerModeSummaryItem.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Column' then
    Column := TcxGridServerModeColumn(TcxCustomGridTableViewAccess.FindItemByObjectName(GridView, AValue));
end;

function TcxGridServerModeSummaryItem.GetDisplayText: string;
begin
  Result := DisplayText;
end;

function TcxGridServerModeSummaryItem.GetVisibleForCustomization: Boolean;
begin
  Result := VisibleForCustomization;
end;

procedure TcxGridServerModeSummaryItem.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridServerModeSummaryItem then
    with TcxGridServerModeSummaryItem(Source) do
    begin
      Self.DisplayText := DisplayText;
      Self.VisibleForCustomization := VisibleForCustomization;
    end;
end;

{ TcxGridServerModeGroupRow }

function TcxGridServerModeGroupRow.GetDisplayText(Index: Integer): string;
begin
  if ViewData.HasCustomDataHandling(GroupedColumn, doGrouping) then
    Result := ViewData.GetCustomDataDisplayText(GroupedColumn.Index, Value)
  else
    Result := inherited GetDisplayText(Index);
end;

{ TcxGridServerModeViewData }

function TcxGridServerModeViewData.GetGroupRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridServerModeGroupRow;
end;

{ TcxGridServerModeTableView }

destructor TcxGridServerModeTableView.Destroy;
begin
  inherited Destroy;
end;

function TcxGridServerModeTableView.GetColumn(Index: Integer): TcxGridServerModeColumn;
begin
  Result := TcxGridServerModeColumn(inherited Columns[Index]);
end;

procedure TcxGridServerModeTableView.SetColumn(Index: Integer; Value: TcxGridServerModeColumn);
begin
  inherited Columns[Index] := Value;
end;

function TcxGridServerModeTableView.GetDataController: TcxGridServerModeDataController;
begin
  Result := TcxGridServerModeDataController(FDataController);
end;

procedure TcxGridServerModeTableView.SetDataController(Value: TcxGridServerModeDataController);
begin
  FDataController.Assign(Value);
end;

function TcxGridServerModeTableView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridServerModeDataController;
end;

function TcxGridServerModeTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridServerModeColumn;
end;

function TcxGridServerModeTableView.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxGridServerModeSummaryItem;
end;

function TcxGridServerModeTableView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridServerModeViewData;
end;

function TcxGridServerModeTableView.CanEditViewLayoutAndData: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.GetConditionalFormattingProvider: TcxDataControllerConditionalFormattingProvider;
begin
  Result := nil;
end;

procedure TcxGridServerModeTableView.AfterRestoring;
begin
  DataController.UpdateItems(True);
  inherited AfterRestoring;
end;

function TcxGridServerModeTableView.CanBeUsedAsDetail: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.CanBeUsedAsMaster: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.CanGetHitTest: Boolean;
begin
  Result := inherited CanGetHitTest and DataController.IsRowInfoValid;
end;

function TcxGridServerModeTableView.IsCheckBoxSelectionSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.IsDataRowFixingSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.IsMergedGroupsSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.IsPersistentMultiSelectSupported: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeTableView.CreateColumn: TcxGridServerModeColumn;
begin
  Result := TcxGridServerModeColumn(inherited CreateColumn);
end;

function TcxGridServerModeTableView.GetColumnByFieldName(const AFieldName: string): TcxGridServerModeColumn;
begin
  Result := TcxGridServerModeColumn(DataController.GetItemByFieldName(AFieldName));
end;

initialization
  cxGridRegisteredViews.Register(TcxGridServerModeTableView, 'Server Mode Table');
  Classes.RegisterClass(TcxGridServerModeColumn);

finalization
  cxGridRegisteredViews.Unregister(TcxGridServerModeTableView);

end.

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

unit cxGridDBDataDefinitions;

{$I cxVer.inc}

interface

uses
  Windows, Classes, DB, cxGridCustomView,
  cxCustomData, cxFilter, cxDBData, cxDataStorage, cxDataUtils,
  cxEdit, cxDBEdit, cxGridCustomTableView;

type
  TcxGridDBDataController = class;

  //Filter Criteria
  TcxGridDBDataFilterCriteriaItem = class(TcxDBDataFilterCriteriaItem)
  private
    function GetDataController: TcxGridDBDataController;
  protected
    function IsItemSupportsMultiThreading(AItem: TcxCustomGridTableItem): Boolean; virtual;
    function SupportsMultiThreading: Boolean; override;
  public
    property DataController: TcxGridDBDataController read GetDataController;
  end;

  TcxGridDBDataFilterCriteria = class(TcxDBDataFilterCriteria)
  private
    function GetDataController: TcxGridDBDataController;
  protected
    function GetItemClass: TcxFilterCriteriaItemClass; override;
  public
    property DataController: TcxGridDBDataController read GetDataController;
  end;

  TcxGridDBDefaultValuesProvider = class(TcxCustomDBEditDefaultValuesProvider)
  private
    FFieldName: string;
  public
    function DefaultCanModify: Boolean; override;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TcxGridDBDataController = class(TcxDBDataController,
    IcxCustomGridDataController,
    IcxGridDataController,
    IcxEditorFieldLink)
  private
    FPrevScrollBarPos: Integer;
    function GetController: TcxCustomGridTableController;
    function GetGridViewValue: TcxCustomGridTableView;
  protected
    // IcxCustomGridDataController
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;
    // IcxGridDataController
    procedure CheckGridModeBufferCount;
    function DoScroll(AForward: Boolean): Boolean;
    function DoScrollPage(AForward: Boolean): Boolean;
    //function GetFilterPropertyValue(const AName: string; var AValue: Variant): Boolean;
    function GetItemDataBindingClass: TcxGridItemDataBindingClass;
    function GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
    function GetNavigatorIsBof: Boolean;
    function GetNavigatorIsEof: Boolean;
    function GetScrollBarPos: Integer;
    function GetScrollBarRecordCount: Integer;
    //function SetFilterPropertyValue(const AName: string; const AValue: Variant): Boolean;
    function SetScrollBarPos(Value: Integer): Boolean;
    // IcxEditorFieldLink
    function CreateFieldControls(X, Y: Integer; ADataSource: TObject; AFieldList: TList): Boolean; virtual;

    function CanSelectRow(ARowIndex: Integer): Boolean; override;
    function CompareByField(ARecordIndex1, ARecordIndex2: Integer;
      AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer; override;
    procedure DoDataSetCurrentChanged(AIsCurrent: Boolean); override;
    procedure DoDataSourceChanged; override;
    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    procedure FilterChanged; override;
    function GetDefaultActiveRelationIndex: Integer; override;
    function GetDefaultGridModeBufferCount: Integer; override;
    function GetFilterCriteriaClass: TcxDataFilterCriteriaClass; override;
    function GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    //function GetIncrementalSearchText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    procedure PrepareFieldForSorting(AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode); override;
    function SupportsScrollBarParams: Boolean; virtual;
    function SyncDetailsFocusWithMaster: Boolean; override;
    procedure UpdateScrollBars; override;
  public
    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    function CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
      ARecordIndex: Integer): TObject; override;
    procedure FocusControl(AItemIndex: Integer; var Done: Boolean); override;
    function GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController; override;
    function GetDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    function GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant; override;
    function GetFilterItemFieldCaption(AItem: TObject): string; override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemSortByDisplayText(AItemIndex: Integer; ASortByDisplayText: Boolean): Boolean; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    procedure Post(AForcePost: Boolean = False); override;
    procedure UpdateData; override;

    procedure CreateAllItems(AMissingItemsOnly: Boolean = False);  // IcxCustomGridDataController
    function CreateItemByField(AField: TField): TcxCustomGridTableItem;
    function GetItemByFieldName(const AFieldName: string): TcxCustomGridTableItem;

    property Controller: TcxCustomGridTableController read GetController;
    property GridView: TcxCustomGridTableView read GetGridViewValue;
  published
    property DataModeController;
    property DataSource;
    property DetailKeyFieldNames;
    property Filter;
    property KeyFieldNames;
    property MasterKeyFieldNames;
    property MultiThreadedOptions;
    property Options;
    property Summary;
    property OnCompare;
    property OnDataChanged;
    property OnDetailCollapsing;
    property OnDetailCollapsed;
    property OnDetailExpanding;
    property OnDetailExpanded;
    property OnDetailHasChildren;
    property OnFilterRecord;
    property OnGroupingChanged;
    property OnSortingChanged;
  end;

  TcxGridItemDBDataBinding = class(TcxGridItemDataBinding)
  private
    function GetDataController: TcxGridDBDataController;
    function GetField: TField;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
  protected
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;
    function GetFilterFieldName: string; override;
    procedure Init; override;
    function IsValueTypeStored: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function DefaultWidth(ATakeHeaderIntoAccount: Boolean = True): Integer; override;
    property DataController: TcxGridDBDataController read GetDataController;
    property Field: TField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

implementation

uses
  SysUtils, cxClasses, cxGraphics, cxControls, cxLookAndFeelPainters,
  cxEditDBRegisteredRepositoryItems,
  cxStorage, cxGridCommon, cxGridLevel, Controls, Forms, cxDBLookupComboBox, dxDPIAwareUtils, cxGeometry;

type
  TcxComponentAccess = class(TcxComponent);
  TcxPropertiesAccess = class(TcxCustomEditProperties);

{ TcxGridDBDataFilterCriteriaItem }

function TcxGridDBDataFilterCriteriaItem.IsItemSupportsMultiThreading(AItem: TcxCustomGridTableItem): Boolean;
var
  AProperties: TcxCustomEditProperties;
begin
  AProperties := AItem.GetProperties;
  Result := TcxPropertiesAccess(AProperties).SupportsMultiThreading;
end;

function TcxGridDBDataFilterCriteriaItem.SupportsMultiThreading: Boolean;
begin
  Result := inherited SupportsMultiThreading and IsItemSupportsMultiThreading(Field.Item as TcxCustomGridTableItem);
end;

function TcxGridDBDataFilterCriteriaItem.GetDataController: TcxGridDBDataController;
begin
  Result := inherited DataController as TcxGridDBDataController;
end;

{ TcxGridDBDataFilterCriteria }

function TcxGridDBDataFilterCriteria.GetItemClass: TcxFilterCriteriaItemClass;
begin
  Result := TcxGridDBDataFilterCriteriaItem;
end;

function TcxGridDBDataFilterCriteria.GetDataController: TcxGridDBDataController;
begin
  Result := inherited DataController as TcxGridDBDataController;
end;

{ TcxGridDBDefaultValuesProvider }

function TcxGridDBDefaultValuesProvider.DefaultCanModify: Boolean;
begin
  Result := (FieldName = '') or inherited DefaultCanModify;
end;

function TcxGridDBDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := TcxGridItemDataBinding(Owner).IsDisplayFormatDefined(AIsCurrencyValueAccepted);
end;

{ TcxGridDBDataController }

function TcxGridDBDataController.GetController: TcxCustomGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxGridDBDataController.GetGridViewValue: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(GetGridView);
end;

procedure TcxGridDBDataController.AssignData(ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridDBDataController.DeleteAllItems;
begin
  GridView.ClearItems;
end;

procedure TcxGridDBDataController.GetFakeComponentLinks(AList: TList);
begin
  if (DataSource <> nil) and (DataSource.Owner <> GridView.Owner) and
    (AList.IndexOf(DataSource.Owner) = -1) then
    AList.Add(DataSource.Owner);
end;

function TcxGridDBDataController.GetGridView: TcxCustomGridView;
begin
  Result := TcxCustomGridView(GetOwner);
end;

function TcxGridDBDataController.HasAllItems: Boolean;
var
  I: Integer;
begin
  Result := True;
  with DataSet do
    for I := 0 to FieldCount - 1 do
    begin
      Result := GetItemByFieldName(Fields[I].FieldName) <> nil;
      if not Result then Break;
    end;
end;

function TcxGridDBDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridDBDataController.IsDataLinked: Boolean;
begin
  Result := DataSet <> nil;
end;

function TcxGridDBDataController.SupportsCreateAllItems: Boolean;
begin
  Result := True;
end;

procedure TcxGridDBDataController.CheckGridModeBufferCount;
begin
  UpdateGridModeBufferCount;
end;

function TcxGridDBDataController.DoScroll(AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      Controller.GoToNext(False, False)
    else
      Controller.GoToPrev(False, False);
end;
{
function TcxGridDBDataController.DoScroll(AForward: Boolean): Boolean;
var
  AScrolled: Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
  begin
    if AForward then
      AScrolled := Controller.GoToNext(False, False)
    else
      AScrolled := Controller.GoToPrev(False, False);
    if AScrolled then
      SetSelectionAnchor(FocusedRowIndex);
  end;
end;
}

function TcxGridDBDataController.DoScrollPage(AForward: Boolean): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    if AForward then
      TcxCustomGridTableControllerAccess.FocusNextPage(Controller, False)
    else
      TcxCustomGridTableControllerAccess.FocusPrevPage(Controller, False);
end;
{
function TcxGridDBDataController.DoScrollPage(AForward: Boolean): Boolean;
var
  APrevRecNo: Integer;
begin
  Result := SupportsScrollBarParams;
  if Result then
  begin
    APrevRecNo := RecNo;
    if AForward then
      TcxCustomGridTableControllerAccess.FocusNextPage(Controller, False)
    else
      TcxCustomGridTableControllerAccess.FocusPrevPage(Controller, False);
    if RecNo <> APrevRecNo then
      SetSelectionAnchor(FocusedRowIndex);
  end;
end;
}

{function TcxGridDBDataController.GetFilterPropertyValue(const AName: string;
  var AValue: Variant): Boolean;
begin
  Result := True;
  if AName = 'FilterAutoDataSetFilter' then
    AValue := Filter.AutoDataSetFilter
  else
    Result := False;
end;}

function TcxGridDBDataController.GetItemDataBindingClass: TcxGridItemDataBindingClass;
begin
  Result := TcxGridItemDBDataBinding;
end;

function TcxGridDBDataController.GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxGridDBDefaultValuesProvider;
end;

function TcxGridDBDataController.GetNavigatorIsBof: Boolean;
begin
  Result := GridView.Controller.IsStart;
end;

function TcxGridDBDataController.GetNavigatorIsEof: Boolean;
begin
  Result := GridView.Controller.IsFinish;
end;

function TcxGridDBDataController.GetScrollBarPos: Integer;
begin
  if SupportsScrollBarParams then
    if dceInsert in EditState then
      Result := FPrevScrollBarPos
    else
      Result := RecNo - 1
  else
    Result := -1;
  FPrevScrollBarPos := Result;
end;

function TcxGridDBDataController.GetScrollBarRecordCount: Integer;
begin
  if SupportsScrollBarParams then
    Result := DataSetRecordCount + GridView.ViewInfo.VisibleRecordCount - 1
  else
    Result := -1;
end;

{function TcxGridDBDataController.SetFilterPropertyValue(const AName: string;
  const AValue: Variant): Boolean;
begin
  Result := True;
  if AName = 'FilterAutoDataSetFilter' then
    Filter.AutoDataSetFilter := StringToBoolean(AValue)
  else
    Result := False;
end;}

function TcxGridDBDataController.SetScrollBarPos(Value: Integer): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result then
    RecNo := Value + 1;
end;
{
function TcxGridDBDataController.SetScrollBarPos(Value: Integer): Boolean;
begin
  Result := SupportsScrollBarParams;
  if Result and (RecNo <> Value + 1) then
  begin
    RecNo := Value + 1;
    if RecNo = Value + 1 then
      SetSelectionAnchor(FocusedRowIndex);
  end;
end;
}

function TcxGridDBDataController.CreateFieldControls(X, Y: Integer;
  ADataSource: TObject; AFieldList: TList): Boolean;
var
  I: Integer;
  AField: TField;
begin
  Result := True;
  if ADataSource is TDataSource then
  begin
    if ADataSource <> DataSource then
      GridView.ClearItems;
    DataSource := TDataSource(ADataSource);
    if DataSet = nil then Exit;
    ShowHourglassCursor;
    try
      GridView.BeginUpdate;
      BeginUpdateFields;
      try
        for I := 0 to AFieldList.Count - 1 do
        begin
          AField := TField(AFieldList[I]);
          if GetItemByFieldName(AField.FieldName) = nil then
            CreateItemByField(AField);
        end;
      finally
        EndUpdateFields;
        GridView.EndUpdate;
      end;
    finally
      HideHourglassCursor;
    end;
  end;
end;

function TcxGridDBDataController.CanSelectRow(ARowIndex: Integer): Boolean;
begin
  Result := TcxCustomGridTableViewAccess.CanSelectRecord(GridView, ARowIndex);
end;

function TcxGridDBDataController.CompareByField(ARecordIndex1, ARecordIndex2: Integer;
  AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer;
begin
  if AField.Flags <> 0 then
    Result := GridView.ViewData.CustomCompareDataValues(AField,
      GetComparedValue(ARecordIndex1, AField), GetComparedValue(ARecordIndex2, AField), AMode)
  else
    Result := inherited CompareByField(ARecordIndex1, ARecordIndex2, AField, AMode);
end;

procedure TcxGridDBDataController.DoDataSetCurrentChanged(AIsCurrent: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess.RefreshNavigators(GridView);
end;

procedure TcxGridDBDataController.DoDataSourceChanged;
begin
  TcxComponentAccess(GridView).UpdateFakeLinks;
end;

procedure TcxGridDBDataController.DoValueTypeClassChanged(AItemIndex: Integer);
begin
  inherited;
  TcxCustomGridTableViewAccess.ItemValueTypeClassChanged(GridView, AItemIndex);
end;

procedure TcxGridDBDataController.FilterChanged;
begin
  inherited;
  TcxCustomGridTableViewAccess.FilterChanged(GridView);
end;

function TcxGridDBDataController.GetDefaultActiveRelationIndex: Integer;
begin
  Result := TcxCustomGridTableViewAccess.GetDefaultActiveDetailIndex(GridView);
end;

function TcxGridDBDataController.GetDefaultGridModeBufferCount: Integer;
begin
  Result := TcxCustomGridTableViewInfoAccess.GetDefaultGridModeBufferCount(GridView.ViewInfo);
  if Result = 0 then
    Result := inherited GetDefaultGridModeBufferCount;
end;

function TcxGridDBDataController.GetFilterCriteriaClass: TcxDataFilterCriteriaClass;
begin
  Result := TcxGridDBDataFilterCriteria;
end;

function TcxGridDBDataController.GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  if GridView.ViewData.HasCustomDataHandling(Fields[AItemIndex], doFiltering) then
    Result := GridView.ViewData.GetCustomDataDisplayText(ARecordIndex, AItemIndex, doFiltering)
  else
    Result := inherited GetFilterDisplayText(ARecordIndex, AItemIndex);
end;

{function TcxGridDBDataController.GetIncrementalSearchText(ARecordIndex, AItemIndex: Integer): string;
begin
  if not TcxCustomGridTableViewAccess(GridView).GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetIncrementalSearchText(ARecordIndex, AItemIndex);
end;}

function TcxGridDBDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := TcxCustomGridTableItem(AItem).ID
  else
    Result := -1;
end;

function TcxGridDBDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

function TcxGridDBDataController.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryGroupItemLinkClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryGroupItemLinkClass;
end;

function TcxGridDBDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryItemClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryItemClass;
end;

procedure TcxGridDBDataController.Post(AForcePost: Boolean);
begin
  TcxCustomGridTableViewAccess.BeginLockedStatePaintOnPost(GridView);
  try
    inherited Post(AForcePost);
  finally
    TcxCustomGridTableViewAccess.EndLockedStatePaint(GridView);
  end;
end;

procedure TcxGridDBDataController.PrepareFieldForSorting(AField: TcxCustomDataField;
  AMode: TcxDataControllerComparisonMode);
begin
  inherited PrepareFieldForSorting(AField, AMode);
  AField.Flags := Byte(GridView.ViewData.NeedsCustomDataComparison(AField, AMode));
end;

function TcxGridDBDataController.SupportsScrollBarParams: Boolean;
begin
  Result := IsGridMode and IsSequenced and
    TcxCustomGridTableViewAccess.IsEqualHeightRecords(GridView);
end;

function TcxGridDBDataController.SyncDetailsFocusWithMaster: Boolean;
begin
  Result := GridView.IsSynchronization;
end;

procedure TcxGridDBDataController.UpdateScrollBars;
begin
  inherited;
  Controller.UpdateScrollBars;
end;

procedure TcxGridDBDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridDBDataController.EndFullUpdate;
begin
  inherited;
  GridView.EndUpdate;
end;

function TcxGridDBDataController.CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
  ARecordIndex: Integer): TObject;
begin
  Result := TcxGridLevelAccess.CreateLinkObject(TcxGridLevel(ARelation.Item), ARelation, ARecordIndex);
end;

procedure TcxGridDBDataController.FocusControl(AItemIndex: Integer; var Done: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess.FocusDataControl(GridView, AItemIndex, Done);
end;

function TcxGridDBDataController.GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController;
begin
  Result := TcxCustomGridView(ALinkObject).DataController;
end;

function TcxGridDBDataController.GetDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  if not GridView.ViewData.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetDisplayText(ARecordIndex, AItemIndex);
  TcxCustomGridTableItemAccess.DoGetDataText(GridView.Items[AItemIndex], ARecordIndex, Result);
end;

function TcxGridDBDataController.GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
begin
  Result := inherited GetFilterDataValue(ARecordIndex, AField);
  if GridView.ViewData.HasCustomDataHandling(AField, doFiltering) then
    Result := GridView.ViewData.GetCustomDataValue(AField, Result, doFiltering);
end;

function TcxGridDBDataController.GetFilterItemFieldCaption(AItem: TObject): string;
begin
  Result := TcxCustomGridTableItemAccess.GetFilterCaption(TcxCustomGridTableItem(AItem));
end;

function TcxGridDBDataController.GetItem(Index: Integer): TObject;
begin
  Result := GridView.Items[Index];
end;

function TcxGridDBDataController.GetItemSortByDisplayText(AItemIndex: Integer;
  ASortByDisplayText: Boolean): Boolean;
begin
  Result := TcxCustomGridTableViewAccess.GetItemSortByDisplayText(GridView,
    AItemIndex, ASortByDisplayText);
end;

function TcxGridDBDataController.GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := TcxCustomGridTableViewAccess.GetItemValueSource(GridView, AItemIndex);
end;

procedure TcxGridDBDataController.UpdateData;
begin
  inherited;
  TcxCustomGridTableViewAccess.UpdateRecord(GridView);
end;

procedure TcxGridDBDataController.CreateAllItems(AMissingItemsOnly: Boolean = False);
var
  I: Integer;
begin
  if DataSet = nil then Exit;
  ShowHourglassCursor;
  try
    GridView.BeginUpdate;
    BeginUpdateFields;
    try
      with DataSet do
        for I := 0 to FieldCount - 1 do
          if not AMissingItemsOnly or (GetItemByFieldName(Fields[I].FieldName) = nil) then
            CreateItemByField(Fields[I]);
    finally
      EndUpdateFields;
      GridView.EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

function TcxGridDBDataController.CreateItemByField(AField: TField): TcxCustomGridTableItem;
begin
  Result := GridView.CreateItem;
  with Result do
  begin
    (DataBinding as TcxGridItemDBDataBinding).FieldName := AField.FieldName;
    Name := CreateUniqueName(GridView.Owner, GridView, Result, ScxGridPrefixName, AField.FieldName);
    Visible := AField.Visible;
  end;
end;

function TcxGridDBDataController.GetItemByFieldName(const AFieldName: string): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(inherited GetItemByFieldName(AFieldName));
end;

{ TcxGridItemDBDataBinding }

function TcxGridItemDBDataBinding.GetDataController: TcxGridDBDataController;
begin
  Result := TcxGridDBDataController(inherited DataController);
end;

function TcxGridItemDBDataBinding.GetField: TField;
begin
  Result := DataController.GetItemField(Item.Index);
end;

function TcxGridItemDBDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Item.Index);
end;

procedure TcxGridItemDBDataBinding.SetFieldName(const Value: string);
begin
  if FieldName <> Value then
  begin
    FilterMRUValueItems.ClearItems;
    DataController.ChangeFieldName(Item.Index, Value);
  end;
end;

function TcxGridItemDBDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := nil;
end;

function TcxGridItemDBDataBinding.GetFilterFieldName: string;
begin
  if Field = nil then
    Result := ''
  else
    Result := Field.FieldName;
end;

procedure TcxGridItemDBDataBinding.Init;
begin
  inherited;
  TcxGridDBDefaultValuesProvider(DefaultValuesProvider).DataSource := DataController.DataSource;
  TcxGridDBDefaultValuesProvider(DefaultValuesProvider).Field := Field;
  TcxGridDBDefaultValuesProvider(DefaultValuesProvider).FieldName := FieldName;
end;

function TcxGridItemDBDataBinding.IsValueTypeStored: Boolean;
begin
  Result := FieldName = '';
end;

procedure TcxGridItemDBDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridItemDBDataBinding then
    FieldName := TcxGridItemDBDataBinding(Source).FieldName;
  inherited;
end;

function TcxGridItemDBDataBinding.DefaultCaption: string;
var
  AField: TField;
begin
  AField := Field;
  if AField = nil then
    Result := FieldName
  else
    Result := AField.DisplayName;
end;

function TcxGridItemDBDataBinding.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByDataBinding(Field, Self);
end;

function TcxGridItemDBDataBinding.DefaultWidth(ATakeHeaderIntoAccount: Boolean = True): Integer;
var
  ACanvas: TcxCanvas;
  AField: TField;
  AParams: TcxViewParams;
begin
  AField := Field;
  if AField <> nil then
  begin
    Item.Styles.GetContentParams(nil, AParams);
    ACanvas := GridView.ViewInfo.Canvas;
    ACanvas.Font := AParams.Font;
    Result := AField.DisplayWidth * ACanvas.TextWidth('0') + 2 * dxGetScaleFactor(GridView).Apply(cxTextOffset);
  end
  else
    Result := inherited DefaultWidth(ATakeHeaderIntoAccount);

  TcxCustomGridTableItemAccess.CheckWidthValue(Item, Result);
end;

initialization
  RegisterClasses([TcxGridItemDBDataBinding]);

end.

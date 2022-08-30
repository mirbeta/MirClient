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

unit cxGridServerModeDataDefinitions;

{$I cxVer.inc}

interface

uses
  Classes, DB, cxGridCustomView,
  cxCustomData, cxFilter, cxDBData, cxDataStorage, cxDataUtils,
  cxEdit, cxDBEdit, cxGridCustomTableView, dxServerModeData;

type
  TcxGridServerModeDefaultValuesProvider = class(TcxCustomDBEditDefaultValuesProvider)
  private
    FFieldName: string;
  public
    function CanSetEditMode: Boolean; override;
    function DefaultCanModify: Boolean; override;
    function IsDataAvailable: Boolean; override;
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
    property FieldName: string read FFieldName write FFieldName;
  end;

  TcxGridServerModeDataController = class(TdxServerModeDataController,
    IcxCustomGridDataController, IcxGridDataController)
  private
    FPrevScrollBarPos: Integer;
    function GetController: TcxCustomGridTableController;
    function GetGridViewValue: TcxCustomGridTableView;
  protected
    { IcxCustomGridDataController }
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;
    { IcxGridDataController }
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

    function CanSelectRow(ARowIndex: Integer): Boolean; override;
    function CompareByField(ARecordIndex1, ARecordIndex2: Integer;
      AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer; override;

    procedure DoDataSetCurrentChanged(AIsCurrent: Boolean);
    procedure DoDataSourceChanged;

    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    procedure FilterChanged; override;
    function GetDefaultActiveRelationIndex: Integer; override;

    function GetDefaultGridModeBufferCount: Integer;
    procedure InitializeDescriptor(AField: TcxCustomDataField; var ADescriptor: TdxServerModeDescriptor); override;

    function GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string; override;
    //function GetIncrementalSearchText(ARecordIndex, AItemIndex: Integer): string; override;
    function DoGetGroupRowDisplayText(const ARowInfo: TcxRowInfo; var AItemIndex: Integer): string; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    function GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass; override;
    function GetSummaryItemClass: TcxDataSummaryItemClass; override;
    procedure PopulateFilterValues(AList: TcxDataFilterValueList;
      AItemIndex: Integer; ACriteria: TcxFilterCriteria;
      var AUseFilteredRecords: Boolean; out ANullExists: Boolean; AUniqueOnly: Boolean); override;
    function SupportsScrollBarParams: Boolean; virtual;
    function SyncDetailsFocusWithMaster: Boolean; override;
    procedure UpdateScrollBars;
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
    procedure UpdateData; override;

    procedure CreateAllItems(AMissingItemsOnly: Boolean = False);  // IcxCustomGridDataController
    function GetItemByFieldName(const AFieldName: string): TcxCustomGridTableItem;

    property Controller: TcxCustomGridTableController read GetController;
    property GridView: TcxCustomGridTableView read GetGridViewValue;
  published
    property DataSource;
    property Filter;
    property Options;
    property Summary;
    property OnGroupingChanged;
    property OnSortingChanged;
  end;

  TcxGridItemServerModeDataBinding = class(TcxGridItemDataBinding)
  private
    function GetDataController: TcxGridServerModeDataController;
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
    property DataController: TcxGridServerModeDataController read GetDataController;
    property Field: TField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

implementation

uses
  SysUtils, cxClasses, cxGraphics, cxControls, cxLookAndFeelPainters,
  cxEditDBRegisteredRepositoryItems,
  cxStorage, cxGridCommon, cxGridLevel, Controls, Forms;

type
  TcxComponentAccess = class(TcxComponent);
  TCustomGridTableItemAccess = class(TcxCustomGridTableItem);

{ TcxGridServerModeDefaultValuesProvider }

function TcxGridServerModeDefaultValuesProvider.CanSetEditMode: Boolean;
begin
  Result := True;
end;

function TcxGridServerModeDefaultValuesProvider.DefaultCanModify: Boolean;
begin
//  Result := False; //# (FieldName = '') or inherited DefaultCanModify;
  Result := not DefaultReadOnly and IsDataAvailable;
end;

function TcxGridServerModeDefaultValuesProvider.IsDataAvailable: Boolean;
begin
  Result := (FFieldName <> '');
end;

function TcxGridServerModeDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  Result := TcxGridItemDataBinding(Owner).IsDisplayFormatDefined(AIsCurrencyValueAccepted);
end;

{ TcxGridServerModeDataController }

function TcxGridServerModeDataController.GetController: TcxCustomGridTableController;
begin
  Result := GridView.Controller;
end;

function TcxGridServerModeDataController.GetGridViewValue: TcxCustomGridTableView;
begin
  Result := TcxCustomGridTableView(GetGridView);
end;

procedure TcxGridServerModeDataController.AssignData(ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridServerModeDataController.DeleteAllItems;
begin
  GridView.ClearItems;
end;

procedure TcxGridServerModeDataController.GetFakeComponentLinks(AList: TList);
begin
  if (DataSource <> nil) and (DataSource.Owner <> GridView.Owner) and
    (AList.IndexOf(DataSource.Owner) = -1) then
    AList.Add(DataSource.Owner);
end;

function TcxGridServerModeDataController.GetGridView: TcxCustomGridView;
begin
  Result := TcxCustomGridView(GetOwner);
end;

function TcxGridServerModeDataController.HasAllItems: Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to DataSource.Fields.Count - 1 do
  begin
    Result := GetItemByFieldName(DataSource.Fields[I].FieldName) <> nil;
    if not Result then Break;
  end;
end;

function TcxGridServerModeDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeDataController.IsDataLinked: Boolean;
begin
  Result := DataSource <> nil;
end;

function TcxGridServerModeDataController.SupportsCreateAllItems: Boolean;
begin
  Result := True;
end;

procedure TcxGridServerModeDataController.CheckGridModeBufferCount;
begin
//  do nothing
end;

function TcxGridServerModeDataController.DoScroll(AForward: Boolean): Boolean;
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

function TcxGridServerModeDataController.DoScrollPage(AForward: Boolean): Boolean;
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

function TcxGridServerModeDataController.GetItemDataBindingClass: TcxGridItemDataBindingClass;
begin
  Result := TcxGridItemServerModeDataBinding;
end;

function TcxGridServerModeDataController.GetItemDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxGridServerModeDefaultValuesProvider;
end;

function TcxGridServerModeDataController.GetNavigatorIsBof: Boolean;
begin
  Result := GridView.Controller.IsStart;
end;

function TcxGridServerModeDataController.GetNavigatorIsEof: Boolean;
begin
  Result := GridView.Controller.IsFinish;
end;

function TcxGridServerModeDataController.GetScrollBarPos: Integer;
begin
  if SupportsScrollBarParams then
    if dceInsert in EditState then
      Result := FPrevScrollBarPos
    else
      Result := FocusedRowIndex
  else
    Result := -1;
  FPrevScrollBarPos := Result;
end;

function TcxGridServerModeDataController.GetScrollBarRecordCount: Integer;
begin
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

function TcxGridServerModeDataController.SetScrollBarPos(Value: Integer): Boolean;
begin
  Result := SupportsScrollBarParams;
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

function TcxGridServerModeDataController.CanSelectRow(ARowIndex: Integer): Boolean;
begin
  Result := TcxCustomGridTableViewAccess.CanSelectRecord(GridView, ARowIndex);
end;

function TcxGridServerModeDataController.CompareByField(ARecordIndex1, ARecordIndex2: Integer;
  AField: TcxCustomDataField; AMode: TcxDataControllerComparisonMode): Integer;
begin
  if GridView.ViewData.NeedsCustomDataComparison(AField, AMode) then
    Result := GridView.ViewData.CustomCompareDataValues(AField,
      GetComparedValue(ARecordIndex1, AField), GetComparedValue(ARecordIndex2, AField), AMode)
  else
    Result := inherited CompareByField(ARecordIndex1, ARecordIndex2, AField, AMode);
end;

procedure TcxGridServerModeDataController.DoDataSetCurrentChanged(AIsCurrent: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess.RefreshNavigators(GridView);
end;

procedure TcxGridServerModeDataController.DoDataSourceChanged;
begin
  TcxComponentAccess(GridView).UpdateFakeLinks;
end;

procedure TcxGridServerModeDataController.DoValueTypeClassChanged(AItemIndex: Integer);
begin
  inherited;
  TcxCustomGridTableViewAccess.ItemValueTypeClassChanged(GridView, AItemIndex);
end;

procedure TcxGridServerModeDataController.FilterChanged;
begin
  inherited;
  TcxCustomGridTableViewAccess.FilterChanged(GridView);
end;

function TcxGridServerModeDataController.GetDefaultActiveRelationIndex: Integer;
begin
  Result := TcxCustomGridTableViewAccess.GetDefaultActiveDetailIndex(GridView);
end;

function TcxGridServerModeDataController.GetDefaultGridModeBufferCount: Integer;
begin
  Result := TcxCustomGridTableViewInfoAccess.GetDefaultGridModeBufferCount(GridView.ViewInfo);
end;

procedure TcxGridServerModeDataController.InitializeDescriptor(AField: TcxCustomDataField;
  var ADescriptor: TdxServerModeDescriptor);
const
  AConvertMap: array[TcxGridDateTimeGrouping] of TdxDateTimeGrouping = (dgDefault, dgByDateAndTime, dgRelativeToToday,
    dgByHour, dgByDate, dgByMonth, dgByYear);
var
  AItem: TcxCustomGridTableItem;
begin
  AItem := GetItemByFieldName(GetItemFieldName(AField.Index));
  ADescriptor.DateTimeGrouping := AConvertMap[TCustomGridTableItemAccess(AItem).GetDateTimeHandlingGrouping];
end;

function TcxGridServerModeDataController.GetFilterDisplayText(ARecordIndex, AItemIndex: Integer): string;
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

function TcxGridServerModeDataController.DoGetGroupRowDisplayText(const ARowInfo: TcxRowInfo; var AItemIndex: Integer): string;
var
  AValue: Variant;
  ARecordIndex: Integer;
begin
  if (DataSource <> nil) and DataSource.IsConsistentCache then
  begin
    AValue := GetGroupRowValue(ARowInfo, AItemIndex);
    ARecordIndex := ARowInfo.RecordIndex;
    Result := GridView.ViewData.GetDisplayTextFromValue(ARecordIndex, AItemIndex, AValue);
    TCustomGridTableItemAccess(GridView.Items[AItemIndex]).DoGetDataText(ARecordIndex, Result);
  end
  else
    Result := '';
end;

function TcxGridServerModeDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomGridTableItem then
    Result := TcxCustomGridTableItem(AItem).ID
  else
    Result := -1;
end;

function TcxGridServerModeDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

function TcxGridServerModeDataController.GetSummaryGroupItemLinkClass: TcxDataSummaryGroupItemLinkClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryGroupItemLinkClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryGroupItemLinkClass;
end;

function TcxGridServerModeDataController.GetSummaryItemClass: TcxDataSummaryItemClass;
begin
  Result := TcxCustomGridTableViewAccess.GetSummaryItemClass(GridView);
  if Result = nil then
    Result := inherited GetSummaryItemClass;
end;

procedure TcxGridServerModeDataController.PopulateFilterValues(AList: TcxDataFilterValueList;
  AItemIndex: Integer; ACriteria: TcxFilterCriteria;
  var AUseFilteredRecords: Boolean; out ANullExists: Boolean; AUniqueOnly: Boolean);
var
  I: Integer;
  ADisplayText: string;
  AItem: TcxFilterValueItem;
  AProperties: TcxCustomEditProperties;
  AValueList: TcxDataFilterValueList;
begin
  ANullExists := False;
  if (ACriteria.MaxValueListCount < 0) or (ACriteria.MaxValueListCount > 0) and (AList.Count >= ACriteria.MaxValueListCount) then
    Exit;
  AValueList  := TcxDataFilterValueList.Create(AList.Criteria);
  try
    inherited PopulateFilterValues(AValueList, AItemIndex, ACriteria, AUseFilteredRecords, ANullExists, AUniqueOnly);
    AProperties := GridView.Items[AItemIndex].GetProperties;
    for I := 0 to AValueList.Count - 1 do
    begin
      if (ACriteria.MaxValueListCount > 0) and (AList.Count >= ACriteria.MaxValueListCount) then
        Break;
      AItem := AValueList[I];
      ADisplayText := AProperties.GetDisplayText(AValueList[I].Value, True);
      AList.Add(AItem.Kind, AItem.Value, ADisplayText, True);
    end;
  finally
    AValueList.Free;
  end;
end;

function TcxGridServerModeDataController.SupportsScrollBarParams: Boolean;
begin
  Result := False;
end;

function TcxGridServerModeDataController.SyncDetailsFocusWithMaster: Boolean;
begin
  Result := GridView.IsSynchronization;
end;

procedure TcxGridServerModeDataController.UpdateScrollBars;
begin
  inherited;
  Controller.UpdateScrollBars;
end;

procedure TcxGridServerModeDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridServerModeDataController.EndFullUpdate;
begin
  inherited;
  GridView.EndUpdate;
end;

function TcxGridServerModeDataController.CreateDetailLinkObject(ARelation: TcxCustomDataRelation;
  ARecordIndex: Integer): TObject;
begin
  Result := TcxGridLevelAccess.CreateLinkObject(TcxGridLevel(ARelation.Item), ARelation, ARecordIndex);
end;

procedure TcxGridServerModeDataController.FocusControl(AItemIndex: Integer; var Done: Boolean);
begin
  inherited;
  TcxCustomGridTableViewAccess.FocusDataControl(GridView, AItemIndex, Done);
end;

function TcxGridServerModeDataController.GetDetailDataControllerByLinkObject(ALinkObject: TObject): TcxCustomDataController;
begin
  Result := TcxCustomGridView(ALinkObject).DataController;
end;

function TcxGridServerModeDataController.GetDisplayText(ARecordIndex, AItemIndex: Integer): string;
begin
  if not GridView.ViewData.GetDisplayText(ARecordIndex, AItemIndex, Result) then
    Result := inherited GetDisplayText(ARecordIndex, AItemIndex);
  TcxCustomGridTableItemAccess.DoGetDataText(GridView.Items[AItemIndex], ARecordIndex, Result);
end;

function TcxGridServerModeDataController.GetFilterDataValue(ARecordIndex: Integer; AField: TcxCustomDataField): Variant;
begin
  Result := inherited GetFilterDataValue(ARecordIndex, AField);
  if GridView.ViewData.HasCustomDataHandling(AField, doFiltering) then
    Result := GridView.ViewData.GetCustomDataValue(AField, Result, doFiltering);
end;

function TcxGridServerModeDataController.GetFilterItemFieldCaption(AItem: TObject): string;
begin
  Result := TcxCustomGridTableItemAccess.GetFilterCaption(TcxCustomGridTableItem(AItem));
end;

function TcxGridServerModeDataController.GetItem(Index: Integer): TObject;
begin
  Result := GridView.Items[Index];
end;

function TcxGridServerModeDataController.GetItemSortByDisplayText(AItemIndex: Integer;
  ASortByDisplayText: Boolean): Boolean;
begin
  Result := TcxCustomGridTableViewAccess.GetItemSortByDisplayText(GridView,
    AItemIndex, ASortByDisplayText);
end;

function TcxGridServerModeDataController.GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := TcxCustomGridTableViewAccess.GetItemValueSource(GridView, AItemIndex);
end;

procedure TcxGridServerModeDataController.UpdateData;
begin
  inherited;
  TcxCustomGridTableViewAccess.UpdateRecord(GridView);
end;

procedure TcxGridServerModeDataController.CreateAllItems(AMissingItemsOnly: Boolean = False);
var
  I: Integer;
  AItem: TcxCustomGridTableItem;
  AFieldName: string;
begin
  if DataSource = nil then Exit;
  ShowHourglassCursor;
  try
    GridView.BeginUpdate;
    BeginUpdateFields;
    try
      for I := 0 to DataSource.Fields.Count - 1 do
      begin
        AFieldName := DataSource.Fields[I].FieldName;
        if not AMissingItemsOnly or (GetItemByFieldName(AFieldName) = nil) then
        begin
          AItem := GridView.CreateItem;
          with AItem do
          begin
            with DataBinding as TcxGridItemServerModeDataBinding do
              FieldName := AFieldName;
            Name := CreateUniqueName(GridView.Owner, GridView, AItem,
              ScxGridPrefixName, AFieldName);
            Visible := DataSource.Fields[I].Visible;
          end;
        end;
      end;
    finally
      EndUpdateFields;
      GridView.EndUpdate;
    end;
  finally
    HideHourglassCursor;
  end;
end;

function TcxGridServerModeDataController.GetItemByFieldName(const AFieldName: string): TcxCustomGridTableItem;
begin
  Result := TcxCustomGridTableItem(inherited GetItemByFieldName(AFieldName));
end;

{ TcxGridItemDBDataBinding }

function TcxGridItemServerModeDataBinding.GetDataController: TcxGridServerModeDataController;
begin
  Result := TcxGridServerModeDataController(inherited DataController);
end;

function TcxGridItemServerModeDataBinding.GetField: TField;
begin
  Result := DataController.GetItemField(Item.Index);
end;

function TcxGridItemServerModeDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Item.Index);
end;

procedure TcxGridItemServerModeDataBinding.SetFieldName(const Value: string);
begin
  if FieldName <> Value then
  begin
    FilterMRUValueItems.ClearItems;
    DataController.ChangeFieldName(Item.Index, Value);
  end;
end;

function TcxGridItemServerModeDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := nil;
end;

function TcxGridItemServerModeDataBinding.GetFilterFieldName: string;
begin
  if Field = nil then
    Result := ''
  else
    Result := Field.FieldName;
end;

procedure TcxGridItemServerModeDataBinding.Init;
begin
  inherited;
  TcxGridServerModeDefaultValuesProvider(DefaultValuesProvider).Field := Field;
  TcxGridServerModeDefaultValuesProvider(DefaultValuesProvider).FieldName := FieldName;
end;

function TcxGridItemServerModeDataBinding.IsValueTypeStored: Boolean;
begin
  Result := FieldName = '';
end;

procedure TcxGridItemServerModeDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridItemServerModeDataBinding then
    FieldName := TcxGridItemServerModeDataBinding(Source).FieldName;
  inherited;
end;

function TcxGridItemServerModeDataBinding.DefaultCaption: string;
var
  AField: TField;
begin
  AField := Field;
  if AField = nil then
    Result := FieldName
  else
    Result := AField.DisplayName;
end;

function TcxGridItemServerModeDataBinding.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByField(Field);
end;

function TcxGridItemServerModeDataBinding.DefaultWidth(ATakeHeaderIntoAccount: Boolean = True): Integer;
var
  AField: TField;
  ACanvas: TcxCanvas;
  AParams: TcxViewParams;
  //W: Integer;
begin
  AField := Field;
  if AField = nil then
    Result := inherited DefaultWidth(ATakeHeaderIntoAccount)
  else
  begin
    ACanvas := GridView.ViewInfo.Canvas;
    Item.Styles.GetContentParams(nil, AParams);
    ACanvas.Font := AParams.Font;
    Result := AField.DisplayWidth * ACanvas.TextWidth('0') + 2 * cxTextOffset;
    {if ATakeHeaderIntoAccount then
    begin
      W := ACanvas.TextWidth(TcxCustomGridTableItemAccess(Item).Caption) +
        2 * (GridView.LookAndFeelPainter.HeaderBorderSize + cxTextOffset);
      if W > Result then Result := W;
    end;}
  end;
  TcxCustomGridTableItemAccess.CheckWidthValue(Item, Result);
end;

initialization
  RegisterClasses([TcxGridItemServerModeDataBinding]);

end.

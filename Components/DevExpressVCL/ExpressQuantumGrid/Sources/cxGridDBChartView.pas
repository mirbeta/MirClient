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

unit cxGridDBChartView;

{$I cxVer.inc}

interface

uses
  Classes, DB, cxCustomData, cxDataStorage, cxDBData, cxEdit,
  cxGridCustomView, cxGridChartView;

type
  TcxGridDBChartView = class;

  TcxGridDBChartDataController = class(TcxDBDataController, IcxCustomGridDataController,
    IcxGridChartViewItemsProvider)
  private
    function GetGridViewValue: TcxGridDBChartView;
    function GetOnAfterSummary: TcxAfterSummaryEvent;
    function GetOnSummary: TcxSummaryEvent;
    procedure SetOnAfterSummary(Value: TcxAfterSummaryEvent);
    procedure SetOnSummary(Value: TcxSummaryEvent);
  protected
    { IcxCustomGridDataController }
    procedure AssignData(ADataController: TcxCustomDataController);
    procedure CreateAllItems(AMissingItemsOnly: Boolean);
    procedure DeleteAllItems;
    procedure GetFakeComponentLinks(AList: TList);
    function GetGridView: TcxCustomGridView;
    function HasAllItems: Boolean;
    function IsDataChangeable: Boolean;
    function IsDataLinked: Boolean;
    function SupportsCreateAllItems: Boolean;
    { IcxGridChartViewItemsProvider }
    function IcxGridChartViewItemsProvider.GetItem = GetChartItem;
    function GetChartItem(AItemClass: TcxGridChartItemClass; AIndex: Integer): TcxGridChartItem;
    procedure GetItemCaptions(AItemClass: TcxGridChartItemClass; ACaptions: TStringList);
    procedure InitItem(AItem: TcxGridChartItem; AIndex: Integer);

    procedure DoDataSourceChanged; override;
    procedure DoValueTypeClassChanged(AItemIndex: Integer); override;
    //function GetChartViewItemIndex: Integer; override;
    function GetItemID(AItem: TObject): Integer; override;
    function GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass; override;
    procedure GetValidValueFields(AItemClass: TcxGridChartItemClass; AFields: TList);
    procedure Unlocked; override;
  public
    procedure BeginFullUpdate; override;
    procedure EndFullUpdate; override;
    function GetItem(Index: Integer): TObject; override;

    property GridView: TcxGridDBChartView read GetGridViewValue;
  published
    property DataModeController;
    property DataSource;
    property DetailKeyFieldNames;
    property KeyFieldNames;
    property MasterKeyFieldNames;
    property Options;
    property OnAfterSummary: TcxAfterSummaryEvent read GetOnAfterSummary write SetOnAfterSummary;
    property OnCompare;
    property OnDataChanged;
    property OnFilterRecord;
    property OnSummary: TcxSummaryEvent read GetOnSummary write SetOnSummary;
  end;

  TcxGridDBChartItemDataBinding = class(TcxGridChartItemDataBinding)
  private
    function GetDataController: TcxGridDBChartDataController;
    function GetField: TField;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
  protected
    function GetDefaultDisplayText: string; override;
    function GetDefaultRepositoryItem: TcxEditRepositoryItem; override;
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    function GetDefaultValueTypeClass: TcxValueTypeClass; override;
    procedure InitDefaultValuesProvider(ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider); override;
    function IsValueTypeStored: Boolean; override;
  public
    procedure Assign(Source: TPersistent); override;
    property DataController: TcxGridDBChartDataController read GetDataController;
    property Field: TField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  TcxGridDBChartCategories = class(TcxGridChartCategories)
  private
    function GetDataBinding: TcxGridDBChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridDBChartItemDataBinding);
  published
    property DataBinding: TcxGridDBChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBChartDataGroup = class(TcxGridChartDataGroup)
  private
    function GetDataBinding: TcxGridDBChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridDBChartItemDataBinding);
  published
    property DataBinding: TcxGridDBChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBChartSeries = class(TcxGridChartSeries)
  private
    function GetDataBinding: TcxGridDBChartItemDataBinding;
    procedure SetDataBinding(Value: TcxGridDBChartItemDataBinding);
  published
    property DataBinding: TcxGridDBChartItemDataBinding read GetDataBinding write SetDataBinding;
  end;

  TcxGridDBChartView = class(TcxGridChartView)
  private
    function GetCategories: TcxGridDBChartCategories;
    function GetDataController: TcxGridDBChartDataController;
    function GetDataGroup(Index: Integer): TcxGridDBChartDataGroup;
    function GetSeries(Index: Integer): TcxGridDBChartSeries;
    procedure SetCategories(Value: TcxGridDBChartCategories);
    procedure SetDataController(Value: TcxGridDBChartDataController);
    procedure SetDataGroup(Index: Integer; Value: TcxGridDBChartDataGroup);
    procedure SetSeries(Index: Integer; Value: TcxGridDBChartSeries);
  protected
    function GetCategoriesClass: TcxGridChartCategoriesClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetItemDataBindingClass: TcxGridChartItemDataBindingClass; override;

    function FindItemByFieldName(AItemClass: TcxGridChartItemClass; const AFieldName: string): TcxGridChartItem;
  public
    function CreateDataGroup: TcxGridDBChartDataGroup;
    function FindDataGroupByFieldName(const AFieldName: string): TcxGridDBChartDataGroup;
    function GetDataGroupClass: TcxGridChartDataGroupClass; override;
    property DataGroups[Index: Integer]: TcxGridDBChartDataGroup read GetDataGroup write SetDataGroup;

    function CreateSeries: TcxGridDBChartSeries;
    function FindSeriesByFieldName(const AFieldName: string): TcxGridDBChartSeries;
    function GetSeriesClass: TcxGridChartSeriesClass; override;
    property Series[Index: Integer]: TcxGridDBChartSeries read GetSeries write SetSeries;
  published
    property Categories: TcxGridDBChartCategories read GetCategories write SetCategories;
    property DataController: TcxGridDBChartDataController read GetDataController write SetDataController;
  end;

implementation

uses
  SysUtils, cxClasses, cxDBEdit, cxEditDBRegisteredRepositoryItems;

type
  TcxComponentAccess = class(TcxComponent);

{ TcxGridDBChartDataController }

function TcxGridDBChartDataController.GetGridViewValue: TcxGridDBChartView;
begin
  Result := TcxGridDBChartView(GetOwner);
end;

function TcxGridDBChartDataController.GetOnAfterSummary: TcxAfterSummaryEvent;
begin
  Result := Summary.OnAfterSummary;
end;

function TcxGridDBChartDataController.GetOnSummary: TcxSummaryEvent;
begin
  Result := Summary.DefaultGroupSummaryItems.OnSummary;
end;

procedure TcxGridDBChartDataController.SetOnAfterSummary(Value: TcxAfterSummaryEvent);
begin
  Summary.OnAfterSummary := Value;
end;

procedure TcxGridDBChartDataController.SetOnSummary(Value: TcxSummaryEvent);
begin
  Summary.DefaultGroupSummaryItems.OnSummary := Value;
end;

procedure TcxGridDBChartDataController.AssignData(ADataController: TcxCustomDataController);
begin
end;

procedure TcxGridDBChartDataController.CreateAllItems(AMissingItemsOnly: Boolean);
begin
end;

procedure TcxGridDBChartDataController.DeleteAllItems;
begin
end;

procedure TcxGridDBChartDataController.GetFakeComponentLinks(AList: TList);
begin
  if (DataSource <> nil) and (DataSource.Owner <> GridView.Owner) and
    (AList.IndexOf(DataSource.Owner) = -1) then
    AList.Add(DataSource.Owner);
end;

function TcxGridDBChartDataController.GetGridView: TcxCustomGridView;
begin
  Result := GridView;
end;

function TcxGridDBChartDataController.HasAllItems: Boolean;
begin
  Result := True;
end;

function TcxGridDBChartDataController.IsDataChangeable: Boolean;
begin
  Result := False;
end;

function TcxGridDBChartDataController.IsDataLinked: Boolean;
begin
  Result := DataSet <> nil;
end;

function TcxGridDBChartDataController.SupportsCreateAllItems: Boolean;
begin
  Result := False;
end;

function TcxGridDBChartDataController.GetChartItem(AItemClass: TcxGridChartItemClass;
  AIndex: Integer): TcxGridChartItem;
var
  AFields: TList;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(AItemClass, AFields);
    Result := GridView.FindItemByFieldName(AItemClass, TField(AFields[AIndex]).FieldName);
  finally
    AFields.Free;
  end;
end;

procedure TcxGridDBChartDataController.GetItemCaptions(AItemClass: TcxGridChartItemClass;
  ACaptions: TStringList);
var
  AFields: TList;
  I: Integer;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(AItemClass, AFields);
    for I := 0 to AFields.Count - 1 do
      ACaptions.Add(TField(AFields[I]).DisplayName);
  finally
    AFields.Free;
  end;
end;

procedure TcxGridDBChartDataController.InitItem(AItem: TcxGridChartItem; AIndex: Integer);
var
  AFields: TList;
begin
  AFields := TList.Create;
  try
    GetValidValueFields(TcxGridChartItemClass(AItem.ClassType), AFields);
    TcxGridDBChartItemDataBinding(AItem.DataBinding).FieldName := TField(AFields[AIndex]).FieldName;
  finally
    AFields.Free;
  end;
end;

procedure TcxGridDBChartDataController.DoDataSourceChanged;
begin
  TcxComponentAccess(GridView).UpdateFakeLinks;
end;

procedure TcxGridDBChartDataController.DoValueTypeClassChanged(AItemIndex: Integer);
var
  AChartItem: IcxGridChartItem;
begin
  inherited;
  if Supports(GetItem(AItemIndex), IcxGridChartItem, AChartItem) then
    AChartItem.ValueTypeClassChanged;
end;

{function TcxGridDBChartDataController.GetChartViewItemIndex: Integer;
begin
  if GridView.SortedSeries = nil then
    Result := -1
  else
    Result := GridView.SortedSeries.DataBinding.DataIndex;
end;}

function TcxGridDBChartDataController.GetItemID(AItem: TObject): Integer;
var
  AChartItem: IcxGridChartItem;
begin
  if Supports(AItem, IcxGridChartItem, AChartItem) then
    Result := AChartItem.GetID
  else
    Result := -1;
end;

function TcxGridDBChartDataController.GetSortingBySummaryEngineClass: TcxSortingBySummaryEngineClass;
begin
  Result := GridView.ViewData.GetSortingBySummaryEngineClass;
end;

function CompareFields(Item1, Item2: Pointer): Integer;
begin
  if TField(Item1).DisplayName < TField(Item2).DisplayName then
    Result := -1
  else
    if TField(Item1).DisplayName > TField(Item2).DisplayName then
      Result := 1
    else
      Result := 0;
end;

procedure TcxGridDBChartDataController.GetValidValueFields(AItemClass: TcxGridChartItemClass;
  AFields: TList);
var
  I: Integer;
  AField: TField;
begin
  if DataSet = nil then Exit;
  for I := 0 to DataSet.FieldCount - 1 do
  begin
    AField := DataSet.Fields[I];
    if not AItemClass.IsValue or
      IsValueTypeClassValid(GetValueTypeClassByField(AField)) then
      AFields.Add(AField);
  end;
  AFields.Sort(CompareFields);
end;

procedure TcxGridDBChartDataController.Unlocked;
begin
  inherited;
  GridView.DataControllerUnlocked;
end;

procedure TcxGridDBChartDataController.BeginFullUpdate;
begin
  GridView.BeginUpdate;
  inherited;
end;

procedure TcxGridDBChartDataController.EndFullUpdate;
begin
  inherited;
  GridView.EndUpdate;
end;

function TcxGridDBChartDataController.GetItem(Index: Integer): TObject;
begin
  Result := Fields[Index].Item;
end;

{ TcxGridDBChartItemDataBinding }

function TcxGridDBChartItemDataBinding.GetDataController: TcxGridDBChartDataController;
begin
  Result := TcxGridDBChartDataController(inherited DataController);
end;

function TcxGridDBChartItemDataBinding.GetField: TField;
begin
  Result := DataController.GetItemField(DataIndex);
end;

function TcxGridDBChartItemDataBinding.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(DataIndex);
end;

procedure TcxGridDBChartItemDataBinding.SetFieldName(const Value: string);
begin
  DataController.ChangeFieldName(DataIndex, Value);
end;

function TcxGridDBChartItemDataBinding.GetDefaultDisplayText: string;
begin
  if Field = nil then
    Result := inherited GetDefaultDisplayText
  else
    Result := Field.DisplayName;
end;

function TcxGridDBChartItemDataBinding.GetDefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByField(Field);
end;

function TcxGridDBChartItemDataBinding.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxCustomDBEditDefaultValuesProvider;
end;

function TcxGridDBChartItemDataBinding.GetDefaultValueTypeClass: TcxValueTypeClass;
begin
  Result := nil;
end;

procedure TcxGridDBChartItemDataBinding.InitDefaultValuesProvider(ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider);
begin
  inherited;
  TcxCustomDBEditDefaultValuesProvider(ADefaultValuesProvider).DataSource := DataController.DataSource;
  TcxCustomDBEditDefaultValuesProvider(ADefaultValuesProvider).Field := Field;
end;

function TcxGridDBChartItemDataBinding.IsValueTypeStored: Boolean;
begin
  Result := FieldName = '';
end;

procedure TcxGridDBChartItemDataBinding.Assign(Source: TPersistent);
begin
  if Source is TcxGridDBChartItemDataBinding then
    FieldName := TcxGridDBChartItemDataBinding(Source).FieldName;
  inherited;
end;

{ TcxGridDBChartCategories }

function TcxGridDBChartCategories.GetDataBinding: TcxGridDBChartItemDataBinding;
begin
  Result := TcxGridDBChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridDBChartCategories.SetDataBinding(Value: TcxGridDBChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBChartDataGroup }

function TcxGridDBChartDataGroup.GetDataBinding: TcxGridDBChartItemDataBinding;
begin
  Result := TcxGridDBChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridDBChartDataGroup.SetDataBinding(Value: TcxGridDBChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBChartSeries }

function TcxGridDBChartSeries.GetDataBinding: TcxGridDBChartItemDataBinding;
begin
  Result := TcxGridDBChartItemDataBinding(inherited DataBinding);
end;

procedure TcxGridDBChartSeries.SetDataBinding(Value: TcxGridDBChartItemDataBinding);
begin
  inherited DataBinding := Value;
end;

{ TcxGridDBChartView }

function TcxGridDBChartView.GetCategories: TcxGridDBChartCategories;
begin
  Result := TcxGridDBChartCategories(inherited Categories);
end;

function TcxGridDBChartView.GetDataController: TcxGridDBChartDataController;
begin
  Result := TcxGridDBChartDataController(FDataController);
end;

function TcxGridDBChartView.GetDataGroup(Index: Integer): TcxGridDBChartDataGroup;
begin
  Result := TcxGridDBChartDataGroup(inherited DataGroups[Index]);
end;

function TcxGridDBChartView.GetSeries(Index: Integer): TcxGridDBChartSeries;
begin
  Result := TcxGridDBChartSeries(inherited Series[Index]);
end;

procedure TcxGridDBChartView.SetCategories(Value: TcxGridDBChartCategories);
begin
  inherited Categories := Value;
end;

procedure TcxGridDBChartView.SetDataController(Value: TcxGridDBChartDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridDBChartView.SetDataGroup(Index: Integer; Value: TcxGridDBChartDataGroup);
begin
  inherited DataGroups[Index] := Value;
end;

procedure TcxGridDBChartView.SetSeries(Index: Integer; Value: TcxGridDBChartSeries);
begin
  inherited Series[Index] := Value;
end;

function TcxGridDBChartView.GetCategoriesClass: TcxGridChartCategoriesClass;
begin
  Result := TcxGridDBChartCategories;
end;

function TcxGridDBChartView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDBChartDataController;
end;

function TcxGridDBChartView.GetItemDataBindingClass: TcxGridChartItemDataBindingClass;
begin
  Result := TcxGridDBChartItemDataBinding;
end;

function TcxGridDBChartView.FindItemByFieldName(AItemClass: TcxGridChartItemClass;
  const AFieldName: string): TcxGridChartItem;
var
  AItems: TList;
  I: Integer;
begin
  AItems := GetItemList(AItemClass);
  for I := 0 to AItems.Count - 1 do
  begin
    Result := TcxGridChartItem(AItems[I]);
    if SameText(TcxGridDBChartItemDataBinding(Result.DataBinding).FieldName, AFieldName) then Exit;
  end;
  Result := nil;
end;

function TcxGridDBChartView.CreateDataGroup: TcxGridDBChartDataGroup;
begin
  Result := TcxGridDBChartDataGroup(inherited CreateDataGroup);
end;

function TcxGridDBChartView.FindDataGroupByFieldName(const AFieldName: string): TcxGridDBChartDataGroup;
begin
  Result := TcxGridDBChartDataGroup(FindItemByFieldName(GetDataGroupClass, AFieldName));
end;

function TcxGridDBChartView.GetDataGroupClass: TcxGridChartDataGroupClass;
begin
  Result := TcxGridDBChartDataGroup;
end;

function TcxGridDBChartView.CreateSeries: TcxGridDBChartSeries;
begin
  Result := TcxGridDBChartSeries(inherited CreateSeries);
end;

function TcxGridDBChartView.FindSeriesByFieldName(const AFieldName: string): TcxGridDBChartSeries;
begin
  Result := TcxGridDBChartSeries(FindItemByFieldName(GetSeriesClass, AFieldName));
end;

function TcxGridDBChartView.GetSeriesClass: TcxGridChartSeriesClass;
begin
  Result := TcxGridDBChartSeries;
end;

initialization
  cxGridRegisteredViews.Register(TcxGridDBChartView, 'DB Chart');
  Classes.RegisterClasses([TcxGridDBChartDataGroup, TcxGridDBChartSeries]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridDBChartView);

end.

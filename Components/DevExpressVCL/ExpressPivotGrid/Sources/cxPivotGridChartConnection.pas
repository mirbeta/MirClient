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

unit cxPivotGridChartConnection;

{$I cxVer.inc}

interface

uses
  Windows, SysUtils, Messages, Classes, cxClasses, cxCustomPivotGrid, cxGridChartView, Variants, Forms, cxControls;

type
  TcxPivotGridChartConnection = class;

  { TcxPivotGridChartCellInfo }

  TcxPivotGridChartCellInfo = class(TObject)
  strict private
    FColumnIndex: Integer;
    FRowIndex: Integer;
    FValue: Variant;
  public
    constructor Create(const AValue: Variant; AColumnIndex, ARowIndex: Integer);

    property ColumnIndex: Integer read FColumnIndex;
    property RowIndex: Integer read FRowIndex;
    property Value: Variant read FValue;
  end;

  { TcxPivotGridChartCellsIndexes }

  TcxPivotGridChartCellInfosList = class(TcxObjectList)
  strict private
    function GetItem(AIndex: Integer): TcxPivotGridChartCellInfo;
  public
    property Items[Index: Integer]: TcxPivotGridChartCellInfo read GetItem; default;
  end;

  { TcxPivotGridChartConnectionStorage }

  TcxPivotGridChartConnectionStorage = class
  strict private
    FCells: TcxPivotGridChartCellInfosList;
    FColumns: TStringList;
    FRows: TStringList;

    function GetDataItemDisplayText(ADataItem: TcxPivotGridViewDataItem): string;
    function GetRowDataItem(Index: Integer): TcxPivotGridViewDataItem;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure AddCell(const AValue: Variant; ARow, AColumn: TcxPivotGridViewDataItem);

    property Cells: TcxPivotGridChartCellInfosList read FCells;
    property Columns: TStringList read FColumns;
    property RowDataItem[Index: Integer]: TcxPivotGridViewDataItem read GetRowDataItem;
    property Rows: TStringList read FRows;
  end;

  TcxPivotGridChartViewSourceForCategories = (sfcRows, sfcColumns);
  TcxPivotGridChartViewSourceData = (sdAllShown, sdSelected);

  TcxPivotGridChartGetDataCellsEvent = procedure (Sender: TcxPivotGridChartConnection;
    ACol, ARow: Integer; ACell: TcxPivotGridCrossCellSummary; var AUseInCalculations: Boolean) of object;
  TcxPivotGridChartGetSeriesDisplayTextEvent = procedure (
    Sender: TcxPivotGridChartConnection; ASeries: TcxGridChartSeries; var ADisplayText: string) of object;

  { TcxPivotGridChartConnection }

  TcxPivotGridChartConnection = class(TcxCustomComponent, IcxPivotGridListener)
  strict private
    FGridChartView: TcxGridChartView;
    FPivotGrid: TcxCustomPivotGrid;
    FSourceData: TcxPivotGridChartViewSourceData;
    FSourceForCategories: TcxPivotGridChartViewSourceForCategories;

    FOnGetDataCells: TcxPivotGridChartGetDataCellsEvent;
    FOnGetSeriesDisplayText: TcxPivotGridChartGetSeriesDisplayTextEvent;

    function GetDataFieldFromViewData(ADataBuilder: TcxPivotGridDataBuilder; AItem: TcxPivotGridViewDataItem): TcxPivotGridField;
    function GetDataFieldValue(ACol, ARow: TcxPivotGridViewDataItem;
      AViewData: TcxPivotGridViewData; ACell: TcxPivotGridCrossCellSummary; var AValue: Variant): Boolean;
    function GetSummaryType(ACol, ARow: TcxPivotGridViewDataItem; var ASummaryType: TcxPivotGridSummaryType): Boolean;
    function GetValidName(AName: string): string;
    procedure SetGridChartView(AValue: TcxGridChartView);
    procedure SetPivotGrid(AValue: TcxCustomPivotGrid);
    procedure SetSourceData(AValue: TcxPivotGridChartViewSourceData);
    procedure SetSourceForCategories(AValue: TcxPivotGridChartViewSourceForCategories);
  protected
    procedure DoChanged; virtual;
    function DoGetDataCells(ACol, ARow: Integer; AViewData: TcxPivotGridViewData): Boolean; virtual;
    function DoGetSeriesDisplayText(ASeries: TcxGridChartSeries; const ADisplayText: string): string; virtual;
    procedure InitializeChart(AStorage: TcxPivotGridChartConnectionStorage; AGridChartView: TcxGridChartView); virtual;
    function IsTotal(ACol, ARow: Integer; AViewData: TcxPivotGridViewData): Boolean;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PopulateStorage(const AViewData: TcxPivotGridViewData; const AStorage: TcxPivotGridChartConnectionStorage); virtual;
    procedure PopulateChartView(AGridChartView: TcxGridChartView; AStorage: TcxPivotGridChartConnectionStorage); virtual;
    procedure RebuildChartSeries(AChartView: TcxGridChartView; APivotGrid: TcxCustomPivotGrid);
    // IcxPivotGridListener
    procedure DataChanged(Sender: TcxCustomPivotGrid);
    procedure LayoutChanged(Sender: TcxCustomPivotGrid);
    procedure PivotRemoved(Sender: TcxCustomPivotGrid);
    procedure SelectionChanged(Sender: TcxCustomPivotGrid);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Refresh; virtual;
  published
    property GridChartView: TcxGridChartView read FGridChartView write SetGridChartView;
    property PivotGrid: TcxCustomPivotGrid read FPivotGrid write SetPivotGrid;
    property SourceData: TcxPivotGridChartViewSourceData read FSourceData write SetSourceData default sdAllShown;
    property SourceForCategories: TcxPivotGridChartViewSourceForCategories read FSourceForCategories write SetSourceForCategories default sfcRows;
    property OnGetDataCells: TcxPivotGridChartGetDataCellsEvent read FOnGetDataCells write FOnGetDataCells;
    property OnGetSeriesDisplayText: TcxPivotGridChartGetSeriesDisplayTextEvent read FOnGetSeriesDisplayText write FOnGetSeriesDisplayText;
  end;

implementation

uses
  Types, StrUtils, dxCore;

type
  TcxPivotGridOptionsViewAccess = class(TcxPivotGridOptionsView);

  { TcxPivotGridChartConnectionController }

  TcxPivotGridChartConnectionController = class(TcxMessageWindow)
  strict private class var
    FInstance: TcxPivotGridChartConnectionController;
  strict private
    FList: TList;
  protected
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    class procedure PostRefresh(AOwner: TcxPivotGridChartConnection);
    class procedure Register(AOwner: TcxPivotGridChartConnection);
    class procedure Unregister(AOwner: TcxPivotGridChartConnection);
  end;

  { TcxPivotGridChartSeriesHelper }

  TcxPivotGridChartSeriesHelper = class(TObject)
  strict private
    FComponentList: TcxHashedStringList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    function CreateUniqueName(AOwnerComponent, AComponent: TComponent; const APrefixName: string; var AIndex: Integer): string;
    procedure InitializeComponentList(AOwner: TComponent);
  end;

{ TcxPivotGridChartSeriesHelper }

constructor TcxPivotGridChartSeriesHelper.Create;
begin
  inherited Create;
  FComponentList := TcxHashedStringList.Create;
end;

destructor TcxPivotGridChartSeriesHelper.Destroy;
begin
  FreeAndNil(FComponentList);
  inherited Destroy;
end;

function TcxPivotGridChartSeriesHelper.CreateUniqueName(
  AOwnerComponent, AComponent: TComponent; const APrefixName: string; var AIndex: Integer): string;
var
  I: Integer;
begin
  for I := AIndex to MaxInt do
  begin
    Result := cxGenerateComponentName(AOwnerComponent, AComponent.ClassName, APrefixName, '', I);
    if FComponentList.IndexOf(Result) < 0 then
    begin
      AIndex := I;
      FComponentList.Add(Result);
      Break;
    end;
  end;
end;

procedure TcxPivotGridChartSeriesHelper.InitializeComponentList(AOwner: TComponent);
var
  I: Integer;
begin
  FComponentList.Clear;
  for I := 0 to AOwner.ComponentCount - 1 do
    FComponentList.Add(AOwner.Components[I].Name);
end;

{ TcxPivotGridChartCellInfo }

constructor TcxPivotGridChartCellInfo.Create(const AValue: Variant; AColumnIndex, ARowIndex: Integer);
begin
  FValue := AValue;
  FColumnIndex := AColumnIndex;
  FRowIndex := ARowIndex;
end;

{ TcxPivotGridChartCellsIndexes }

function TcxPivotGridChartCellInfosList.GetItem(AIndex: Integer): TcxPivotGridChartCellInfo;
begin
  Result := TcxPivotGridChartCellInfo(inherited Items[AIndex]);
end;

{ TcxPivotGridChartConnection }

constructor TcxPivotGridChartConnection.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  TcxPivotGridChartConnectionController.Register(Self);
end;

destructor TcxPivotGridChartConnection.Destroy;
begin
  TcxPivotGridChartConnectionController.Unregister(Self);
  GridChartView := nil;
  PivotGrid := nil;
  inherited Destroy;
end;

procedure TcxPivotGridChartConnection.Refresh;
begin
  DoChanged;
end;

procedure TcxPivotGridChartConnection.DoChanged;
begin
  if not (csDestroying in ComponentState) then
    RebuildChartSeries(GridChartView, PivotGrid);
end;

function TcxPivotGridChartConnection.DoGetDataCells(ACol, ARow: Integer; AViewData: TcxPivotGridViewData): Boolean;
begin
  Result := ((SourceData = sdAllShown) and not IsTotal(ACol, ARow, AViewData)) or
    ((SourceData = sdSelected) and AViewData.IsCellSelected(ARow, ACol));

  if Assigned(OnGetDataCells) then
    OnGetDataCells(Self, ACol, ARow, AViewData.Cells[ARow, ACol], Result);
end;

function TcxPivotGridChartConnection.DoGetSeriesDisplayText(
  ASeries: TcxGridChartSeries; const ADisplayText: string): string;
begin
  Result := ADisplayText;
  if Assigned(OnGetSeriesDisplayText) then
    OnGetSeriesDisplayText(Self, ASeries, Result);
end;

procedure TcxPivotGridChartConnection.Loaded;
begin
  inherited Loaded;
  TcxPivotGridChartConnectionController.PostRefresh(Self);
end;

procedure TcxPivotGridChartConnection.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);

  if Operation = opRemove then
  begin
    if AComponent = PivotGrid then
      PivotGrid := nil;
    if AComponent = GridChartView then
      GridChartView := nil;
  end;
end;

procedure TcxPivotGridChartConnection.PopulateStorage(
  const AViewData: TcxPivotGridViewData; const AStorage: TcxPivotGridChartConnectionStorage);
var
  AColumn: TcxPivotGridViewDataItem;
  ARow: TcxPivotGridViewDataItem;
  AValue: Variant;
  I, J: Integer;
begin
  //todo: drop stored indexes
  for I := 0 to AViewData.RowCount - 1 do
    AViewData.Rows[I].Tag := 0;
  for I := 0 to AViewData.ColumnCount - 1 do
    AViewData.Columns[I].Tag := 0;

  AStorage.Cells.Capacity := AViewData.ColumnCount * AViewData.RowCount;
  for I := 0 to AViewData.ColumnCount - 1 do
  begin
    AColumn := AViewData.Columns[I];
    for J := 0 to AViewData.RowCount - 1 do
      if DoGetDataCells(I, J, AViewData) then
      begin
        ARow := AViewData.Rows[J];
        if GetDataFieldValue(AColumn, ARow, AViewData, AViewData.Cells[J, I], AValue) then
        begin
          if SourceForCategories = sfcRows then
            AStorage.AddCell(AValue, AColumn, ARow)
          else
            AStorage.AddCell(AValue, ARow, AColumn);
        end;
      end;
  end;
end;

procedure TcxPivotGridChartConnection.PopulateChartView(
  AGridChartView: TcxGridChartView; AStorage: TcxPivotGridChartConnectionStorage);
var
  I: Integer;
  ACell: TcxPivotGridChartCellInfo;
begin
  AGridChartView.BeginUpdate;
  try
    for I := 0 to AStorage.Cells.Count - 1 do
    begin
      ACell := AStorage.Cells[I];
      AGridChartView.ViewData.Values[ACell.RowIndex, ACell.ColumnIndex] := ACell.Value;
    end;
  finally
    AGridChartView.EndUpdate;
  end;
end;

procedure TcxPivotGridChartConnection.RebuildChartSeries(AChartView: TcxGridChartView; APivotGrid: TcxCustomPivotGrid);
var
  AStorage: TcxPivotGridChartConnectionStorage;
begin
  if (APivotGrid <> nil) and (AChartView <> nil) and
    ([csDesigning, csLoading] * (ComponentState + APivotGrid.ComponentState + AChartView.ComponentState) = []) then
  begin
    AStorage := TcxPivotGridChartConnectionStorage.Create;
    try
      PopulateStorage(APivotGrid.ViewData, AStorage);
      AChartView.BeginUpdate;
      try
        InitializeChart(AStorage, AChartView);
        PopulateChartView(AChartView, AStorage);
      finally
        AChartView.EndUpdate;
      end;
    finally
      AStorage.Free;
    end;
  end;
end;

procedure TcxPivotGridChartConnection.InitializeChart(
  AStorage: TcxPivotGridChartConnectionStorage; AGridChartView: TcxGridChartView);
var
  AHelper: TcxPivotGridChartSeriesHelper;
  ARowDataItem: TcxPivotGridViewDataItem;
  ASeries: TcxGridChartSeries;
  I, AIndex: Integer;
begin
  AGridChartView.BeginUpdate;
  try
    AGridChartView.ViewData.CategoryCount := AStorage.Columns.Count;
    for I := 0 to AStorage.Columns.Count - 1 do
      AGridChartView.ViewData.Categories[I] := AStorage.Columns.Strings[I];

    AHelper := TcxPivotGridChartSeriesHelper.Create;
    try
      AIndex := 1;
      AGridChartView.ClearSeries;
      AHelper.InitializeComponentList(Owner);
      for I := 0 to AStorage.Rows.Count - 1 do
      begin
        ASeries := AGridChartView.CreateSeries;
        ASeries.Name := AHelper.CreateUniqueName(AGridChartView, ASeries, GetValidName(AStorage.Rows.Strings[I]), AIndex);
        ASeries.DisplayText := DoGetSeriesDisplayText(ASeries, AStorage.Rows.Strings[I]);
        ARowDataItem := AStorage.RowDataItem[I];
        if ARowDataItem.Field <> nil then
          ASeries.ValueCaptionFormat := ARowDataItem.Field.DisplayFormat;
      end;
    finally
      AHelper.Free;
    end;
  finally
    AGridChartView.EndUpdate;
  end;
end;

function TcxPivotGridChartConnection.IsTotal(ACol, ARow: Integer; AViewData: TcxPivotGridViewData): Boolean;

  function IsGroupTotal(AGroupItem: TcxPivotGridGroupItem): Boolean;
  begin
    Result := (AGroupItem <> nil) and not AGroupItem.IsCollapsed and (AGroupItem.ItemCount > 0);
  end;

begin
  Result := AViewData.Columns[ACol].IsTotalItem or AViewData.Rows[ARow].IsTotalItem or
    TcxPivotGridOptionsViewAccess(PivotGrid.OptionsView).IsCompactLayout and
    IsGroupTotal(AViewData.Rows[ARow].GroupItem);
end;

// IcxPivotGridListener
procedure TcxPivotGridChartConnection.DataChanged(Sender: TcxCustomPivotGrid);
begin
  DoChanged;
end;

procedure TcxPivotGridChartConnection.LayoutChanged(Sender: TcxCustomPivotGrid);
begin
  DoChanged;
end;

procedure TcxPivotGridChartConnection.PivotRemoved(Sender: TcxCustomPivotGrid);
begin
  PivotGrid := nil;
end;

procedure TcxPivotGridChartConnection.SelectionChanged(Sender: TcxCustomPivotGrid);
begin
  if SourceData = sdSelected then
    DoChanged;
end;

function TcxPivotGridChartConnection.GetDataFieldFromViewData(
  ADataBuilder: TcxPivotGridDataBuilder; AItem: TcxPivotGridViewDataItem): TcxPivotGridField;
var
  I: Integer;
begin
  Result := nil;
  if ADataBuilder.DataFields.Count = 0 then
    Exit;
  if ADataBuilder.DataFields.Count = 1 then
    Exit(ADataBuilder.DataFields[0]);

  while (AItem <> nil) and not AItem.IsDataField do
    AItem := AItem.Parent;
  if (AItem <> nil) and AItem.IsDataField then
    Result := AItem.Field
  else
  begin
    for I := 0 to ADataBuilder.DataFields.Count - 1 do
      if ADataBuilder.DataFields[I].SummaryVariation = svNone then
        Result := ADataBuilder.DataFields[I];
  end;
end;

function TcxPivotGridChartConnection.GetSummaryType(
  ACol, ARow: TcxPivotGridViewDataItem; var ASummaryType: TcxPivotGridSummaryType): Boolean;
var
  ATotal: TcxPivotGridCustomTotal;
begin
  Result := ARow.GetSummaryType(ASummaryType, ATotal) or
    ACol.GetSummaryType(ASummaryType, ATotal);
end;

function TcxPivotGridChartConnection.GetDataFieldValue(
  ACol, ARow: TcxPivotGridViewDataItem; AViewData: TcxPivotGridViewData;
  ACell: TcxPivotGridCrossCellSummary; var AValue: Variant): Boolean;
var
  ADataField: TcxPivotGridField;
  ASummaryType: TcxPivotGridSummaryType;
begin
  if PivotGrid.OptionsDataField.Area = dfaRow then
    ADataField := GetDataFieldFromViewData(AViewData.DataBuilder, ARow)
  else
    ADataField := GetDataFieldFromViewData(AViewData.DataBuilder, ACol);

  Result := ADataField <> nil;
  if Result then
  begin
    ASummaryType := ADataField.SummaryType;
    if GetSummaryType(ACol, ARow, ASummaryType) or (ADataField.SummaryVariation = svNone) then
      AValue := ACell.GetSummaryValue(ASummaryType)
    else
      AValue := ACell.SummaryVariation;
  end;
end;

function TcxPivotGridChartConnection.GetValidName(AName: string): string;
const
  Letters = ['A'..'Z', 'a'..'z'];
  CorrectSymbols = Letters + ['_', '0'..'9'];
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(AName) do
    if dxCharInSet(AName[I], CorrectSymbols) then
      Result := Result + AName[I];
  if Length(Result) = 0 then
    Result := 'Series';
  if not dxCharInSet(Result[1], Letters) then
    Result := 'A' + Result;
end;

procedure TcxPivotGridChartConnection.SetSourceForCategories(AValue: TcxPivotGridChartViewSourceForCategories);
begin
  if AValue <> SourceForCategories then
  begin
    FSourceForCategories := AValue;
    DoChanged;
  end;
end;

procedure TcxPivotGridChartConnection.SetSourceData(AValue: TcxPivotGridChartViewSourceData);
begin
  if AValue <> SourceData then
  begin
    FSourceData := AValue;
    DoChanged;
  end;
end;

procedure TcxPivotGridChartConnection.SetGridChartView(AValue: TcxGridChartView);
begin
  if AValue <> FGridChartView then
  begin
    FGridChartView := AValue;
    DoChanged;
  end;
end;

procedure TcxPivotGridChartConnection.SetPivotGrid(AValue: TcxCustomPivotGrid);
begin
  if AValue <> FPivotGrid then
  begin
    if PivotGrid <> nil then
    begin
      FPivotGrid.RemoveListener(Self);
      FPivotGrid := nil;
    end;
    if AValue <> nil then
    begin
      FPivotGrid := AValue;
      FPivotGrid.AddListener(Self);
    end;
    DoChanged;
  end;
end;

{ TcxPivotGridChartConnectionStorage }

constructor TcxPivotGridChartConnectionStorage.Create;
begin
  FCells := TcxPivotGridChartCellInfosList.Create;
  FColumns := TStringList.Create;
  FRows := TStringList.Create;
end;

destructor TcxPivotGridChartConnectionStorage.Destroy;
begin
  FCells.Clear;
  FreeAndNil(FRows);
  FreeAndNil(FColumns);
  FreeAndNil(FCells);
  inherited Destroy;
end;

procedure TcxPivotGridChartConnectionStorage.AddCell(
  const AValue: Variant; ARow, AColumn: TcxPivotGridViewDataItem);
begin
  if ARow.Tag = 0 then
    ARow.Tag := Rows.AddObject(GetDataItemDisplayText(ARow), ARow) + 1;
  if AColumn.Tag = 0 then
    AColumn.Tag := Columns.AddObject(GetDataItemDisplayText(AColumn), AColumn) + 1;
  Cells.Add(TcxPivotGridChartCellInfo.Create(AValue, AColumn.Tag - 1, ARow.Tag - 1));
end;

function TcxPivotGridChartConnectionStorage.GetDataItemDisplayText(ADataItem: TcxPivotGridViewDataItem): string;
begin
  Result := ADataItem.GetDisplayText;
  if (ADataItem.Level > 0) and (ADataItem.Parent <> nil) and (ADataItem.Parent.Value <> '') then
    Result := GetDataItemDisplayText(ADataItem.Parent) + ' - ' + Result;
end;

function TcxPivotGridChartConnectionStorage.GetRowDataItem(Index: Integer): TcxPivotGridViewDataItem;
begin
  Result := TcxPivotGridViewDataItem(Rows.Objects[Index]);
end;

{ TcxPivotGridChartConnectionController }

constructor TcxPivotGridChartConnectionController.Create;
begin
  inherited Create;
  FList := TList.Create;
end;

destructor TcxPivotGridChartConnectionController.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

class procedure TcxPivotGridChartConnectionController.PostRefresh(AOwner: TcxPivotGridChartConnection);
begin
  if FInstance <> nil then
    PostMessage(FInstance.Handle, WM_USER, 0, LPARAM(AOwner));
end;

class procedure TcxPivotGridChartConnectionController.Register(AOwner: TcxPivotGridChartConnection);
begin
  if FInstance = nil then
    FInstance := TcxPivotGridChartConnectionController.Create;
  FInstance.FList.Add(AOwner);
end;

class procedure TcxPivotGridChartConnectionController.Unregister(AOwner: TcxPivotGridChartConnection);
begin
  if FInstance <> nil then
  begin
    FInstance.FList.Remove(AOwner);
    if FInstance.FList.Count = 0 then
      FreeAndNil(FInstance);
  end;
end;

procedure TcxPivotGridChartConnectionController.WndProc(var Message: TMessage);
begin
  if Message.Msg = WM_USER then
  begin
    if FList.IndexOf(Pointer(Message.LParam)) >= 0 then
      TcxPivotGridChartConnection(Message.LParam).Refresh;
  end;
  inherited WndProc(Message);
end;

end.

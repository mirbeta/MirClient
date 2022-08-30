{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxLookupDBGrid;

{$I cxVer.inc}

interface

uses
  Windows,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls, DB,
  cxClasses, cxControls, cxGraphics, cxLookAndFeelPainters,
  cxEdit, cxDBEdit, cxCustomData, cxDB, cxDBData, cxEditRepositoryItems,
  cxLookupGrid;

const
  DefaultSyncMode = False;

type
  TcxCustomLookupDBGrid = class;

  { TcxLookupGridDBDataController }

  TcxLookupGridDBDataController = class(TcxDBDataController)
  private
    function GetGrid: TcxCustomLookupDBGrid;
  protected
    procedure UpdateScrollBars; override;
  public
    constructor Create(AOwner: TComponent); override;
    function GetItem(Index: Integer): TObject; override;
    property Grid: TcxCustomLookupDBGrid read GetGrid;
  published
    property OnCompare;
  end;

  { TcxLookupDBGridColumn }

  TcxLookupDBGridDefaultValuesProvider = class(TcxCustomDBEditDefaultValuesProvider)
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;

  TcxLookupDBGridColumn = class(TcxLookupGridColumn)
  private
    function GetDataController: TcxLookupGridDBDataController;
    function GetField: TField;
    function GetFieldName: string;
    procedure SetFieldName(const Value: string);
  protected
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; override;
    procedure InitDefaultValuesProvider;
    property DataController: TcxLookupGridDBDataController read GetDataController;
  public
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; override;
    function DefaultRepositoryItem: TcxEditRepositoryItem; override;
    function DefaultWidth: Integer; override;
    function Equals(Obj: TObject): Boolean; override;

    property Field: TField read GetField;
  published
    property FieldName: string read GetFieldName write SetFieldName;
  end;

  { TcxLookupDBGridColumns }

  TcxLookupDBGridColumns = class(TcxLookupGridColumns)
  private
    function GetColumn(Index: Integer): TcxLookupDBGridColumn;
    procedure SetColumn(Index: Integer; Value: TcxLookupDBGridColumn);
  public
    function Add: TcxLookupDBGridColumn;
    function ColumnByFieldName(const AFieldName: string): TcxLookupDBGridColumn;
    function Equals(Obj: TObject): Boolean; override;
    //
    property Items[Index: Integer]: TcxLookupDBGridColumn read GetColumn write SetColumn; default;
  end;

  { TcxLookupDBGridOptions }

  TcxLookupDBGridOptions = class(TcxLookupGridOptions)
  private
    function GetGrid: TcxCustomLookupDBGrid;
    function GetSyncMode: Boolean;
    procedure SetSyncMode(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    property Grid: TcxCustomLookupDBGrid read GetGrid;
  published
    property SyncMode: Boolean read GetSyncMode write SetSyncMode default DefaultSyncMode;
  end;

  { TcxCustomLookupDBGrid }

  TcxCustomLookupDBGrid = class(TcxCustomLookupGrid)
  private
    function GetColumns: TcxLookupDBGridColumns;
    function GetDataController: TcxLookupGridDBDataController;
    function GetDataSource: TDataSource;
    function GetKeyFieldNames: string;
    function GetOptions: TcxLookupDBGridOptions;
    procedure SetColumns(Value: TcxLookupDBGridColumns);
    procedure SetDataController(Value: TcxLookupGridDBDataController);
    procedure SetDataSource(Value: TDataSource);
    procedure SetKeyFieldNames(const Value: string);
    procedure SetOptions(Value: TcxLookupDBGridOptions);
  protected
    procedure CreateColumnsByFields(AFieldNames: TStrings); virtual;
    procedure DataChanged; override;
    function GetColumnClass: TcxLookupGridColumnClass; override;
    function GetColumnsClass: TcxLookupGridColumnsClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetOptionsClass: TcxLookupGridOptionsClass; override;
    procedure InitScrollBarsParameters; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure UpdateScrollBars; override; // for Delphi .NET
  public
    procedure CreateAllColumns;
    procedure CreateColumnsByFieldNames(const AFieldNames: string);
    property Align;
    property Anchors;
    property Color;
    property Columns: TcxLookupDBGridColumns read GetColumns write SetColumns;
    property DataController: TcxLookupGridDBDataController read GetDataController write SetDataController;
    property Font;
    property LookAndFeel;
    property Options: TcxLookupDBGridOptions read GetOptions write SetOptions;
    property ParentFont;
    property Visible;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property KeyFieldNames: string read GetKeyFieldNames write SetKeyFieldNames;
  end;

  TcxCustomLookupDBGridClass = class of TcxCustomLookupDBGrid;

implementation

uses
  Math,
  cxEditDBRegisteredRepositoryItems;

type
  TcxCustomLookupGridAccess = class(TcxCustomLookupGrid);

function TcxLookupDBGridDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  with TcxLookupDBGridColumn(Owner) do
    Result := DataController.GetItemTextStored(Index);
end;

{ TcxLookupDBGridColumn }

procedure TcxLookupDBGridColumn.Assign(Source: TPersistent);
begin
  if Source is TcxLookupDBGridColumn then
    FieldName := TcxLookupDBGridColumn(Source).FieldName;
  inherited Assign(Source);
end;

function TcxLookupDBGridColumn.DefaultCaption: string;
var
  AField: TField;
begin
  AField := Field;
  if AField = nil then
    Result := FieldName
  else
    Result := AField.DisplayName;
end;

function TcxLookupDBGridColumn.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDBRepositoryItems.GetItemByField(Field);
end;

function TcxLookupDBGridColumn.DefaultWidth: Integer;

  function GetMaxTextWidthNearCurrentField(ACanvas: TcxCanvas): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := Max(0, DataController.FocusedRecordIndex - 100) to Min(DataController.RecordCount - 1, Index + 100) do
      Result := Max(Result, ACanvas.TextWidth(DataController.GetDisplayText(I, Index)));
  end;

var
  AField: TField;
  ACanvas: TcxCanvas;
begin
  AField := Field;
  if AField = nil then
    Result := inherited DefaultWidth
  else
  begin
    ACanvas := Grid.ViewInfo.Canvas;
    ACanvas.Font := GetContentFont;
    Result := Max(GetMaxTextWidthNearCurrentField(ACanvas), AField.DisplayWidth * ACanvas.TextWidth('0')) + 2 * cxTextOffset;
    if Grid.Options.ShowHeader then
    begin
      Result := Max(Result, Grid.Painter.LFPainterClass.ScaledHeaderWidth(ACanvas, cxBordersAll,
        Caption, Grid.ViewInfo.GetHeaderFont, TcxCustomLookupGridAccess(Grid).ScaleFactor));
    end;
  end;
  CheckWidthValue(Result);
end;

function TcxLookupDBGridColumn.Equals(Obj: TObject): Boolean;
begin
  Result := inherited Equals(Obj) and (Obj is TcxLookupDBGridColumn) and (FieldName = TcxLookupDBGridColumn(Obj).FieldName);
end;

function TcxLookupDBGridColumn.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxLookupDBGridDefaultValuesProvider;
end;

procedure TcxLookupDBGridColumn.InitDefaultValuesProvider;
begin
  TcxCustomDBEditDefaultValuesProvider(DefaultValuesProvider.GetInstance).Field := Field;
end;

function TcxLookupDBGridColumn.GetDataController: TcxLookupGridDBDataController;
begin
  Result := TcxLookupGridDBDataController(inherited DataController);
end;

function TcxLookupDBGridColumn.GetField: TField;
begin
  Result := DataController.GetItemField(Index);
end;

function TcxLookupDBGridColumn.GetFieldName: string;
begin
  Result := DataController.GetItemFieldName(Index);
end;

procedure TcxLookupDBGridColumn.SetFieldName(const Value: string);
begin
  DataController.ChangeFieldName(Index, Value);
end;

{ TcxLookupDBGridColumns }

function TcxLookupDBGridColumns.Add: TcxLookupDBGridColumn;
begin
  Result := inherited Add as TcxLookupDBGridColumn;
end;

function TcxLookupDBGridColumns.ColumnByFieldName(const AFieldName: string): TcxLookupDBGridColumn;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if AnsiCompareText(Result.FieldName, AFieldName) = 0 then
      Exit;
  end;
  Result := nil;
end;

function TcxLookupDBGridColumns.Equals(Obj: TObject): Boolean;
var
  I: Integer;
begin
  Result := (Obj is TcxLookupDBGridColumns) and (TcxLookupDBGridColumns(Obj).Count = Count);
  if Result then
  begin
    for I := 0 to Count - 1 do
      Result := Result and Items[I].Equals(TcxLookupDBGridColumns(Obj).Items[I]);
  end;
end;

function TcxLookupDBGridColumns.GetColumn(Index: Integer): TcxLookupDBGridColumn;
begin
  Result := inherited Items[Index] as TcxLookupDBGridColumn;
end;

procedure TcxLookupDBGridColumns.SetColumn(Index: Integer; Value: TcxLookupDBGridColumn);
begin
  inherited Items[Index] := Value;
end;

{ TcxLookupGridDBDataController }

constructor TcxLookupGridDBDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  DataModeController.SyncMode := DefaultSyncMode;
  DataModeController.SyncInsert := False;
end;

function TcxLookupGridDBDataController.GetItem(Index: Integer): TObject;
begin
  Result := Grid.Columns[Index];
end;

procedure TcxLookupGridDBDataController.UpdateScrollBars;
begin
  Grid.UpdateScrollBars;
end;

function TcxLookupGridDBDataController.GetGrid: TcxCustomLookupDBGrid;
begin
  Result := GetOwner as TcxCustomLookupDBGrid;
end;

{ TcxLookupDBGridOptions }

procedure TcxLookupDBGridOptions.Assign(Source: TPersistent);
begin
  if Source is TcxLookupDBGridOptions then
  begin
    if Assigned(Grid) then
      Grid.BeginUpdate;
    try
      inherited Assign(Source);
      SyncMode := TcxLookupDBGridOptions(Source).SyncMode;
    finally
      if Assigned(Grid) then
        Grid.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxLookupDBGridOptions.GetGrid: TcxCustomLookupDBGrid;
begin
  Result := TcxCustomLookupDBGrid(FGrid);
end;

function TcxLookupDBGridOptions.GetSyncMode: Boolean;
begin
  if Assigned(Grid) then
    Result := Grid.DataController.DataModeController.SyncMode
  else
    Result := DefaultSyncMode;
end;

procedure TcxLookupDBGridOptions.SetSyncMode(Value: Boolean);
begin
  if Assigned(Grid) then
    Grid.DataController.DataModeController.SyncMode := Value;
end;

{ TcxCustomLookupDBGrid }

procedure TcxCustomLookupDBGrid.CreateAllColumns;
var
  ADataSet: TDataSet;
  AFieldNames: TStrings;
begin
  Columns.Clear;
  ADataSet := DataController.DataSet;
  if ADataSet <> nil then
  begin
    AFieldNames := TStringList.Create;
    try
    {$WARNINGS OFF} { for Borland Delphi 10 }
      ADataSet.GetFieldNames(AFieldNames);
    {$WARNINGS ON}
      CreateColumnsByFields(AFieldNames);
    finally
      AFieldNames.Free;
    end;
  end;
end;

procedure TcxCustomLookupDBGrid.CreateColumnsByFieldNames(const AFieldNames: string);
var
  AFieldNamesList: TStrings;
begin
  Columns.Clear;
  AFieldNamesList := TStringList.Create;
  try
    GetFieldNames(AFieldNames, AFieldNamesList);
    CreateColumnsByFields(AFieldNamesList);
  finally
    AFieldNamesList.Free;
  end;
end;

procedure TcxCustomLookupDBGrid.CreateColumnsByFields(AFieldNames: TStrings);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to AFieldNames.Count - 1 do
      Columns.Add.FieldName := AFieldNames[I];
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomLookupDBGrid.DataChanged;
var
  I: Integer;
begin
  for I := 0 to Columns.Count - 1 do
    Columns[I].InitDefaultValuesProvider;
  inherited DataChanged;
end;

function TcxCustomLookupDBGrid.GetColumnClass: TcxLookupGridColumnClass;
begin
  Result := TcxLookupDBGridColumn;
end;

function TcxCustomLookupDBGrid.GetColumnsClass: TcxLookupGridColumnsClass;
begin
  Result := TcxLookupDBGridColumns;
end;

function TcxCustomLookupDBGrid.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxLookupGridDBDataController;
end;

function TcxCustomLookupDBGrid.GetOptionsClass: TcxLookupGridOptionsClass;
begin
  Result := TcxLookupDBGridOptions;
end;

procedure TcxCustomLookupDBGrid.InitScrollBarsParameters;
begin
  if DataController.IsGridMode and DataController.IsSequenced then
  begin
    SetScrollBarInfo(sbVertical, 0,
      (DataController.DataSetRecordCount - 1) + (ViewInfo.VisibleRowCount - 1),
      1, ViewInfo.VisibleRowCount, DataController.RecNo - 1, True, True);
  end
  else
    inherited InitScrollBarsParameters;
end;

procedure TcxCustomLookupDBGrid.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if DataController.IsGridMode and DataController.IsSequenced then
  begin
    if AScrollBarKind = sbVertical then
    begin
      case AScrollCode of
        scLineUp:
          FocusNextRow(False);
        scLineDown:
          FocusNextRow(True);
        scPageUp:
          FocusPriorPage;
        scPageDown:
          FocusNextPage;
        scTrack: ;
        scPosition:
          DataController.RecNo := AScrollPos + 1;
      end;
    end
    else
      inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
    AScrollPos := DataController.RecNo - 1;
  end
  else
    inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
end;

procedure TcxCustomLookupDBGrid.UpdateScrollBars;
begin
  inherited UpdateScrollBars;
end;

function TcxCustomLookupDBGrid.GetColumns: TcxLookupDBGridColumns;
begin
  Result := inherited Columns as TcxLookupDBGridColumns;
end;

function TcxCustomLookupDBGrid.GetDataController: TcxLookupGridDBDataController;
begin
  Result := TcxLookupGridDBDataController(FDataController);
end;

function TcxCustomLookupDBGrid.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

function TcxCustomLookupDBGrid.GetKeyFieldNames: string;
begin
  Result := DataController.KeyFieldNames;
end;

function TcxCustomLookupDBGrid.GetOptions: TcxLookupDBGridOptions;
begin
  Result := TcxLookupDBGridOptions(FOptions);
end;

procedure TcxCustomLookupDBGrid.SetColumns(Value: TcxLookupDBGridColumns);
begin
  inherited Columns := Value;
end;

procedure TcxCustomLookupDBGrid.SetDataController(Value: TcxLookupGridDBDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxCustomLookupDBGrid.SetDataSource(Value: TDataSource);
begin
  DataController.DataSource := Value;
end;

procedure TcxCustomLookupDBGrid.SetKeyFieldNames(const Value: string);
begin
  DataController.KeyFieldNames := Value;
end;

procedure TcxCustomLookupDBGrid.SetOptions(Value: TcxLookupDBGridOptions);
begin
  FOptions.Assign(Value);
end;

end.

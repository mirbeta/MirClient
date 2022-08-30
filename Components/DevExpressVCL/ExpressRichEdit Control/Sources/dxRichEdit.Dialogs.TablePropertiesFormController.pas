{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressRichEditControl                                   }
{                                                                    }
{           Copyright (c) 2000-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSRICHEDITCONTROL AND ALL        }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM       }
{   ONLY.                                                            }
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

unit dxRichEdit.Dialogs.TablePropertiesFormController;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Classes, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Dialogs.Core,
  dxRichEdit.View.Core,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.Selection,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.Utils.Types,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.Platform.Font,
  dxRichEdit.DocumentModel.PieceTable;

type

  { TdxRichEditTablePropertiesHelper }

  TdxRichEditTablePropertiesHelper = class abstract
  public const
    MinTableIndentByDefault = -15 * 1440;
    MaxTableIndentByDefault = 15 * 1440;
    MinTableWidthByDefault = 0;
    MaxTableWidthInModelUnitsByDefault = 22 * 1440;
    MaxTableWidthInPercentByDefault = 600;

    MinRowHeightByDefault = 0;
    MaxRowHeightByDefault = 22 * 1440;

    MinColumnWidthByDefault = 0;
    MaxColumnWidthInModelUnitsByDefault = 22 * 1440;
    MaxColumnWidthInPercentByDefault = 100;

    MinCellWidthByDefault = 0;
    MaxCellWidthInModelUnitsByDefault = 22 * 1440;
    MaxCellWidthInPercentByDefault = 100;
  end;

  { TdxTablePropertiesFormControllerParameters }

  TdxTablePropertiesFormControllerParameters = class(TdxFormControllerParameters)
  private
    FSelectedCells: TdxSelectedCellsCollection;
  public
    constructor Create(const AControl: IdxRichEditControl; ASelectedCells: TdxSelectedCellsCollection);
    property SelectedCells: TdxSelectedCellsCollection read FSelectedCells;
  end;

  { TdxTablePropertiesFormController }

  TdxTablePropertiesFormController = class(TdxFormController)
  private
    FSourceSelectedCells: TdxSelectedCellsCollection;
    FUseDefaultTableWidth: Boolean;
    FTableWidth: Integer;
    FTableWidthUnitType: TdxWidthUnitType;
    FTableAlignment: TdxNullableValue<TdxTableRowAlignment>;
    FTableIndent: Integer;
    FUseDefaultRowHeight: TdxNullableBoolean;
    FRowHeight: Integer;
    FRowHeightType: TdxHeightUnitType;
    FRowCantSplit: TdxNullableBoolean;
    FRowHeader: TdxNullableBoolean;
    FUseDefaultColumnWidth: TdxNullableBoolean;
    FColumnWidth: Integer;
    FColumnWidthUnitType: TdxWidthUnitType;
    FUseDefaultCellWidth: TdxNullableBoolean;
    FCellWidth: Integer;
    FCellWidthUnitType: TdxWidthUnitType;
    FOldCellWidth: TdxWidthUnitInfo;
    FCellVerticalAlignment: TdxNullableValue<TdxVerticalAlignment>;
    FPageFirstColumnWidth: Integer;
    FCellsInSelectedColumns: TdxTableCellList;
    FSelectedCells: TdxTableCellList;
    function GetTable: TdxTable; inline;
    function GetRows: TdxTableRowCollection; inline;
  protected
    procedure ApplyTableProperties;
    procedure ApplyRowProperties;
    procedure ApplyColumnProperties;
    procedure ApplyCellProperties;

    procedure ApplyTableWidth;
    procedure ApplyTableAlignment;
    procedure ApplyTableIndent;

    function CalculateColumnWidth: Integer;
    function GetCellsInSelectedColumns: TdxTableCellList;
    function GetSelectedCells: TdxTableCellList;

    class function EqualsHeightUnits(const AValue1: TdxHeightUnitInfo; const AValue2: TdxHeightUnit): Boolean; overload; static;
    class function EqualsHeightUnits(const AValue1, AValue2: TdxHeightUnit): Boolean; overload; static;
    class function EqualsWidthUnit(const AValue1: TdxWidthUnitInfo; AValue2: TdxWidthUnit): Boolean; overload; static;
    class function EqualsWidthUnit(const AValue1, AValue2: TdxWidthUnit): Boolean; overload; static;

    procedure InitializeController;
    procedure InitializeTableTab;
    procedure InitializeRowTab;
    procedure InitializeColumnTab;
    procedure InitializeCellTab;

    function GetActualHeight(const AUseDefaultValue: TdxNullableBoolean; AHeight: Integer; const AHeightUnitType: TdxHeightUnitType ): TdxHeightUnitInfo;
    function GetActualWidth(const AUseDefaultValue: TdxNullableBoolean; AWidth: Integer; const AWidthUnitType: TdxWidthUnitType): TdxWidthUnitInfo;
    function GetTableAlignment: TdxNullableValue<TdxTableRowAlignment>;
    function GetTableIndent: Integer;
    function IsCellWidthValueChange(ACellWidth: TdxWidthUnitInfo): Boolean;
    function ModelUnitToPercent(AWidth: TdxWidthUnit): Integer;
    function PercentToModelUnit(AWidth: Integer; AType: TdxWidthUnitType): Integer;

    property ValueForPercent: Integer read FPageFirstColumnWidth;
  public
    constructor Create(const AControllerParameters: TdxTablePropertiesFormControllerParameters);
    destructor Destroy; override;
    procedure ApplyChanges; override;
    function CalculateValue(const AWidth: TdxNullableInteger; const AWidthUnitType: TdxNullableValue<TdxWidthUnitType>): TdxNullableInteger;
    function CalculateValueInModelUnits(const AWidth: TdxNullableInteger): TdxNullableInteger;
    function CalculateValueInPercent(const AWidth: TdxNullableInteger): TdxNullableInteger;
    function GetCellWidthMaxValueConsiderWidthUnitType: Integer;
    function GetColumnWidthMaxValueConsiderWidthUnitType: Integer;
    function GetTableWidthMaxValueConsiderWidthUnitType: Integer;
    function IsSelectedFirstRowInTable: Boolean;

    property SourceSelectedCells: TdxSelectedCellsCollection read FSourceSelectedCells;

    property UseDefaultTableWidth: Boolean read FUseDefaultTableWidth write FUseDefaultTableWidth;
    property TableWidth: Integer read FTableWidth write FTableWidth;
    property TableWidthUnitType: TdxWidthUnitType read FTableWidthUnitType write FTableWidthUnitType;
    property TableAlignment: TdxNullableValue<TdxTableRowAlignment> read FTableAlignment write FTableAlignment;
    property TableIndent: Integer read FTableIndent write FTableIndent;

    property UseDefaultRowHeight: TdxNullableBoolean read FUseDefaultRowHeight write FUseDefaultRowHeight;
    property RowHeight: Integer read FRowHeight write FRowHeight;
    property RowHeightType: TdxHeightUnitType read FRowHeightType write FRowHeightType;
    property RowCantSplit: TdxNullableBoolean read FRowCantSplit write FRowCantSplit;
    property RowHeader: TdxNullableBoolean read FRowHeader write FRowHeader;

    property UseDefaultColumnWidth: TdxNullableBoolean read FUseDefaultColumnWidth write FUseDefaultColumnWidth;
    property ColumnWidth: Integer read FColumnWidth write FColumnWidth;
    property ColumnWidthUnitType: TdxWidthUnitType read FColumnWidthUnitType write FColumnWidthUnitType;

    property UseDefaultCellWidth: TdxNullableBoolean read FUseDefaultCellWidth write FUseDefaultCellWidth;
    property CellWidth: Integer read FCellWidth write FCellWidth;
    property CellWidthUnitType: TdxWidthUnitType read FCellWidthUnitType write FCellWidthUnitType;
    property CellVerticalAlignment: TdxNullableValue<TdxVerticalAlignment> read FCellVerticalAlignment write FCellVerticalAlignment;

    property Table: TdxTable read GetTable;
    property Rows: TdxTableRowCollection read GetRows;
    property PageFirstColumnWidth: Integer read FPageFirstColumnWidth;
    property SelectedCells: TdxTableCellList read FSelectedCells;
  end;

implementation

uses
  Contnrs, Math,
  dxTypeHelpers,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.DocumentsToLayoutDocumentsConverter,
  dxRichEdit.DocumentModel.Section;

{ TdxTablePropertiesFormControllerParameters }

constructor TdxTablePropertiesFormControllerParameters.Create(const AControl: IdxRichEditControl;
  ASelectedCells: TdxSelectedCellsCollection);
begin
  inherited Create(AControl);
  FSelectedCells := ASelectedCells;
end;

{ TdxTablePropertiesFormController }

procedure TdxTablePropertiesFormController.ApplyCellProperties;
var
  ANewCellWidth: TdxWidthUnitInfo;
  ACurrentCell: TdxTableCell;
  AIsCellWidthChanged: Boolean;
  I: Integer;
begin
  ANewCellWidth := GetActualWidth(UseDefaultCellWidth, CellWidth, CellWidthUnitType);
  AIsCellWidthChanged := Assigned(ANewCellWidth) and IsCellWidthValueChange(ANewCellWidth);
  try
    for I := 0 to SelectedCells.Count - 1 do
    begin
      ACurrentCell := SelectedCells[I];
      if AIsCellWidthChanged and not EqualsWidthUnit(ANewCellWidth, ACurrentCell.PreferredWidth) then
        ACurrentCell.Properties.PreferredWidth.CopyFrom(ANewCellWidth);
      if not CellVerticalAlignment.IsNull and (ACurrentCell.VerticalAlignment <> CellVerticalAlignment) then
        ACurrentCell.Properties.VerticalAlignment := CellVerticalAlignment.Value;
    end;
  finally
    ANewCellWidth.Free;
  end;
end;

procedure TdxTablePropertiesFormController.ApplyChanges;
var
  ADocumentModel: TdxCustomDocumentModel;
begin
  ADocumentModel := Table.DocumentModel;
  ADocumentModel.BeginUpdate;
  try
    ApplyTableProperties;
    ApplyRowProperties;
    ApplyColumnProperties;
    ApplyCellProperties;
  finally
    ADocumentModel.EndUpdate;
  end;
end;

procedure TdxTablePropertiesFormController.ApplyColumnProperties;
var
  ANewCellWidth: TdxWidthUnitInfo;
  ACurrentCell: TdxTableCell;
  I: Integer;
begin
  ANewCellWidth := GetActualWidth(UseDefaultColumnWidth, ColumnWidth, ColumnWidthUnitType);
  try
    if Assigned(ANewCellWidth) then
      for I := 0 to FCellsInSelectedColumns.Count - 1 do
      begin
        ACurrentCell := FCellsInSelectedColumns[I];
        if not EqualsWidthUnit(ANewCellWidth, ACurrentCell.PreferredWidth) then
          ACurrentCell.Properties.PreferredWidth.CopyFrom(ANewCellWidth);
      end;
  finally
    ANewCellWidth.Free;
  end;
end;

procedure TdxTablePropertiesFormController.ApplyRowProperties;
var
  ANewRowHeight: TdxHeightUnitInfo;
  ASelectedRows: TdxTableRowList;
  ACurrentRow: TdxTableRow;
  ACurrentRowProperties: TdxTableRowProperties;
  I: Integer;
begin
  ANewRowHeight := GetActualHeight(UseDefaultRowHeight, RowHeight, RowHeightType);
  try
    ASelectedRows := SourceSelectedCells.GetSelectedTableRows;
    try
      for I := 0 to ASelectedRows.Count - 1 do
      begin
        ACurrentRow := ASelectedRows[I];
        ACurrentRowProperties := ACurrentRow.Properties;
        if Assigned(ANewRowHeight) and not EqualsHeightUnits(ANewRowHeight, ACurrentRow.Height) then
          ACurrentRowProperties.Height.CopyFrom(ANewRowHeight);
        if not RowCantSplit.IsNull and (ACurrentRow.CantSplit <> RowCantSplit) then
          ACurrentRowProperties.CantSplit := RowCantSplit.Value;
        if not RowHeader.IsNull and (ACurrentRow.Header <> RowHeader) then
          ACurrentRowProperties.Header := RowHeader.Value;
      end;
    finally
      ASelectedRows.Free;
    end;
  finally
    ANewRowHeight.Free
  end;
end;

procedure TdxTablePropertiesFormController.ApplyTableAlignment;
var
  I: Integer;
  ACurrentRow: TdxTableRow;
  ARowsCount: Integer;
begin
  if TableAlignment.IsNull then
    Exit;
  if Table.TableAlignment <> TableAlignment then
    Table.TableProperties.TableAlignment := TableAlignment.Value;
  ARowsCount := Rows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := Rows[I];
    if ACurrentRow.TableRowAlignment <> TableAlignment then
      ACurrentRow.Properties.TableRowAlignment := TableAlignment.Value;
  end;
end;

procedure TdxTablePropertiesFormController.ApplyTableIndent;
var
  ANewTableIndent: TdxWidthUnitInfo;
begin
  if TableAlignment.IsNull or (TableAlignment.Value <> TdxTableRowAlignment.Left) then
    ANewTableIndent := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, 0)
  else
    ANewTableIndent := TdxWidthUnitInfo.Create(TdxWidthUnitType.ModelUnits, TableIndent);
  try
    if not EqualsWidthUnit(ANewTableIndent, Table.TableIndent) then
      Table.TableProperties.TableIndent.CopyFrom(ANewTableIndent);
  finally
    ANewTableIndent.Free;
  end;
end;

procedure TdxTablePropertiesFormController.ApplyTableProperties;
begin
  ApplyTableWidth;
  ApplyTableAlignment;
  ApplyTableIndent;
end;

procedure TdxTablePropertiesFormController.ApplyTableWidth;
var
  ANewTableWidth: TdxWidthUnitInfo;
begin
  ANewTableWidth := GetActualWidth(UseDefaultTableWidth, TableWidth, TableWidthUnitType);
  try
    if not EqualsWidthUnit(ANewTableWidth, Table.PreferredWidth) then
      Table.TableProperties.PreferredWidth.CopyFrom(ANewTableWidth);
  finally
    ANewTableWidth.Free;
  end;
end;

function TdxTablePropertiesFormController.CalculateColumnWidth: Integer;
var
  AFirstSelectedCell: TdxTableCell;
  ADocumentModel: TdxDocumentModel;
  AFirstCellStartParagraphIndex: TdxParagraphIndex;
  AFirstCellLogPosition: TdxDocumentLogPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AConverter: TdxDocumentModelDocumentsToLayoutDocumentsConverter;
  APageBoundsCalculator: TdxPageBoundsCalculator;
  APageClientBounds: TRect;
  AColumnsBoundsCalculator: TdxColumnsBoundsCalculator;
  AColumnWidthsCollection: TdxRectList;
begin
  AFirstSelectedCell := FSourceSelectedCells.FirstSelectedCell;
  ADocumentModel := TdxDocumentModel(AFirstSelectedCell.DocumentModel);
  AFirstCellStartParagraphIndex := AFirstSelectedCell.StartParagraphIndex;
  AFirstCellLogPosition := ADocumentModel.ActivePieceTable.Paragraphs[AFirstCellStartParagraphIndex].LogPosition;
  ASectionIndex := ADocumentModel.FindSectionIndex(AFirstCellLogPosition);
  ASection := ADocumentModel.Sections[ASectionIndex];

  AConverter := TdxDocumentModelDocumentsToLayoutDocumentsConverter.Create;
  try
    APageBoundsCalculator := TdxPageBoundsCalculator.Create(AConverter);
    try
      APageClientBounds := APageBoundsCalculator.CalculatePageClientBounds(ASection);
      AColumnsBoundsCalculator := TdxColumnsBoundsCalculator.Create(AConverter);
      try
        AColumnWidthsCollection := AColumnsBoundsCalculator.Calculate(ASection, APageClientBounds);
        try
          Result := AColumnWidthsCollection[0].Width;
        finally
          AColumnWidthsCollection.Free;
        end;
      finally
        AColumnsBoundsCalculator.Free;
      end;
    finally
      APageBoundsCalculator.Free;
    end;
  finally
    AConverter.Free;
  end;
end;

function TdxTablePropertiesFormController.CalculateValue(const AWidth: TdxNullableInteger;
  const AWidthUnitType: TdxNullableValue<TdxWidthUnitType>): TdxNullableInteger;
begin
  if AWidthUnitType.IsNull then
    Result := TdxNullableInteger.Null
  else
    if AWidthUnitType.Value = TdxWidthUnitType.FiftiethsOfPercent then
      Result := CalculateValueInPercent(AWidth)
    else
      Result := CalculateValueInModelUnits(AWidth);
end;

function TdxTablePropertiesFormController.CalculateValueInModelUnits(
  const AWidth: TdxNullableInteger): TdxNullableInteger;
begin
  if AWidth.IsNull then
    Result := 0
  else
    Result := ValueForPercent * AWidth.Value div 100;
end;

function TdxTablePropertiesFormController.CalculateValueInPercent(const AWidth: TdxNullableInteger): TdxNullableInteger;
begin
  if AWidth.IsNull or (ValueForPercent = 0) then
    Result := 0
  else
    Result := AWidth.Value * 100 div ValueForPercent;
end;

constructor TdxTablePropertiesFormController.Create(const AControllerParameters: TdxTablePropertiesFormControllerParameters);
begin
  inherited Create;
  FSourceSelectedCells := AControllerParameters.SelectedCells;
  FSelectedCells := GetSelectedCells;
  FCellsInSelectedColumns := GetCellsInSelectedColumns;
  InitializeController;
  FPageFirstColumnWidth := CalculateColumnWidth;
end;

destructor TdxTablePropertiesFormController.Destroy;
begin
  FCellsInSelectedColumns.Free;
  FSelectedCells.Free;
  FOldCellWidth.Free;
  inherited Destroy;
end;

class function TdxTablePropertiesFormController.EqualsHeightUnits(const AValue1, AValue2: TdxHeightUnit): Boolean;
begin
  Result := (AValue1.Value = AValue2.Value) and (AValue1.&Type = AValue2.&Type);
end;

class function TdxTablePropertiesFormController.EqualsWidthUnit(const AValue1: TdxWidthUnitInfo;
  AValue2: TdxWidthUnit): Boolean;
begin
  Result := (AValue1.Value = AValue2.Value) and (AValue1.&Type = AValue2.&Type);
end;

class function TdxTablePropertiesFormController.EqualsHeightUnits(const AValue1: TdxHeightUnitInfo;
  const AValue2: TdxHeightUnit): Boolean;
begin
  Result := (AValue1.Value = AValue2.Value) and (AValue1.&Type = AValue2.&Type);
end;

class function TdxTablePropertiesFormController.EqualsWidthUnit(const AValue1, AValue2: TdxWidthUnit): Boolean;
begin
  Result := (AValue1.Value = AValue2.Value) and (AValue1.&Type = AValue2.&Type);
end;

function TdxTablePropertiesFormController.GetActualHeight(const AUseDefaultValue: TdxNullableBoolean; AHeight: Integer;
  const AHeightUnitType: TdxHeightUnitType): TdxHeightUnitInfo;
begin
  if AUseDefaultValue.IsNull then
    Exit(nil);
  if AUseDefaultValue.Value then
    Result := TdxHeightUnitInfo.Create(0, TdxHeightUnitType.Auto)
  else
    Result := TdxHeightUnitInfo.Create(AHeight, AHeightUnitType);
end;

function TdxTablePropertiesFormController.GetActualWidth(const AUseDefaultValue: TdxNullableBoolean; AWidth: Integer;
  const AWidthUnitType: TdxWidthUnitType): TdxWidthUnitInfo;
begin
  if AUseDefaultValue.IsNull then
    Exit(nil);
  if AUseDefaultValue.Value then
    Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.Auto, 0)
  else
    Result := TdxWidthUnitInfo.Create(AWidthUnitType, PercentToModelUnit(AWidth, AWidthUnitType));
end;

function TdxTablePropertiesFormController.GetCellsInSelectedColumns: TdxTableCellList;
var
  AFirstSelectedRow: TdxSelectedCellsIntervalInRow;
  AStartColumnIndex: Integer;
  AEndColumnIndex: Integer;
  ATopSelectedRowIndex: Integer;
  ARowsCount: Integer;
  ACurrentRow: TdxTableRow;
  ACells: TdxTableCellList;
  I: Integer;
begin
  Result := TdxTableCellList.Create;
  AFirstSelectedRow := FSourceSelectedCells.First;
  AStartColumnIndex := AFirstSelectedRow.NormalizedStartCell.GetStartColumnIndexConsiderRowGrid;
  AEndColumnIndex := AFirstSelectedRow.NormalizedEndCell.GetEndColumnIndexConsiderRowGrid;
  ATopSelectedRowIndex := FSourceSelectedCells.GetTopRowIndex;

  ARowsCount := Rows.Count;
  for I := ATopSelectedRowIndex to ARowsCount - 1 do
  begin
    ACurrentRow := Rows[I];
    ACells := TdxTableCellVerticalBorderCalculator.GetCellsByIntervalColumnIndex(ACurrentRow, AStartColumnIndex, AEndColumnIndex);
    try
      Result.AddRange(ACells);
    finally
      ACells.Free;
    end;
  end;
end;

function TdxTablePropertiesFormController.GetCellWidthMaxValueConsiderWidthUnitType: Integer;
begin
  if CellWidthUnitType = TdxWidthUnitType.FiftiethsOfPercent then
    Result := TdxRichEditTablePropertiesHelper.MaxCellWidthInPercentByDefault
  else
    Result := TdxRichEditTablePropertiesHelper.MaxCellWidthInModelUnitsByDefault;
end;

function TdxTablePropertiesFormController.GetColumnWidthMaxValueConsiderWidthUnitType: Integer;
begin
  if ColumnWidthUnitType = TdxWidthUnitType.FiftiethsOfPercent then
    Result := TdxRichEditTablePropertiesHelper.MaxColumnWidthInPercentByDefault
  else
    Result := TdxRichEditTablePropertiesHelper.MaxColumnWidthInModelUnitsByDefault;
end;

function TdxTablePropertiesFormController.GetRows: TdxTableRowCollection;
begin
  Result := Table.Rows;
end;

function TdxTablePropertiesFormController.GetSelectedCells: TdxTableCellList;
var
  I, J: Integer;
  ARowsCount: Integer;
  ACurrentInterval: TdxSelectedCellsIntervalInRow;
  AEndCellIndex: Integer;
  ACells: TdxTableCellCollection;
begin
  Result := TdxTableCellList.Create;
  ARowsCount := FSourceSelectedCells.RowsCount;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentInterval := FSourceSelectedCells[I];
    AEndCellIndex := ACurrentInterval.NormalizedEndCellIndex;
    ACells := ACurrentInterval.Row.Cells;
    for J := ACurrentInterval.NormalizedStartCellIndex to AEndCellIndex do
      Result.Add(ACells[J]);
  end;
end;

function TdxTablePropertiesFormController.GetTable: TdxTable;
begin
  Result := FSourceSelectedCells.FirstSelectedCell.Table;
end;

function TdxTablePropertiesFormController.GetTableAlignment: TdxNullableValue<TdxTableRowAlignment>;
var
  I: Integer;
  ARowsCount: Integer;
  ACurrentRow: TdxTableRow;
  AFirstRowAlignment: TdxTableRowAlignment;
begin
  ARowsCount := Rows.Count;
  AFirstRowAlignment := Rows.First.TableRowAlignment;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := Rows[I];
    if AFirstRowAlignment <> ACurrentRow.TableRowAlignment then
      Exit(TdxNullableValue<TdxTableRowAlignment>.Null);
  end;
  Result := AFirstRowAlignment;
end;

function TdxTablePropertiesFormController.GetTableIndent: Integer;
var
  ATableIndent: TdxWidthUnit;
begin
  ATableIndent := Table.TableIndent;
  Result := IfThen(ATableIndent.&Type = TdxWidthUnitType.ModelUnits, ATableIndent.Value);
end;

function TdxTablePropertiesFormController.GetTableWidthMaxValueConsiderWidthUnitType: Integer;
begin
  if TableWidthUnitType = TdxWidthUnitType.FiftiethsOfPercent then
    Result := TdxRichEditTablePropertiesHelper.MaxTableWidthInPercentByDefault
  else
    Result := TdxRichEditTablePropertiesHelper.MaxTableWidthInModelUnitsByDefault;
end;

procedure TdxTablePropertiesFormController.InitializeCellTab;
var
  AIdenticalCellWidth, AIdenticalVerticalAlignment: Boolean;
  ACurrentCell, AFirstSelectedCell: TdxTableCell;
  AFirstCellWidth: TdxWidthUnit;
  AFirstCellVerticalAlignment: TdxVerticalAlignment;
  I: Integer;
begin
  AIdenticalCellWidth := True;
  AIdenticalVerticalAlignment := True;

  AFirstSelectedCell := SelectedCells[0];
  AFirstCellWidth := AFirstSelectedCell.PreferredWidth;
  AFirstCellVerticalAlignment := AFirstSelectedCell.VerticalAlignment;
  for I := 0 to SelectedCells.Count - 1 do
  begin
    ACurrentCell := SelectedCells[I];
    AIdenticalCellWidth := AIdenticalCellWidth and EqualsWidthUnit(AFirstCellWidth, ACurrentCell.PreferredWidth);
    AIdenticalVerticalAlignment := AIdenticalVerticalAlignment and (AFirstCellVerticalAlignment = ACurrentCell.VerticalAlignment);
  end;

  UseDefaultCellWidth := TdxNullableBoolean.IfThen(AIdenticalCellWidth, AFirstCellWidth.&Type = TdxWidthUnitType.Auto);
  CellWidth := ModelUnitToPercent(AFirstCellWidth);
  CellWidthUnitType := AFirstCellWidth.&Type;
  FOldCellWidth := TdxWidthUnitInfo.Create(CellWidthUnitType, AFirstCellWidth.Value);

  CellVerticalAlignment := TdxNullableValue<TdxVerticalAlignment>.IfThen(AIdenticalVerticalAlignment, AFirstCellVerticalAlignment);
end;

procedure TdxTablePropertiesFormController.InitializeColumnTab;
var
  AIdenticalCellWidth: Boolean;
  AFirstCellWidth: TdxWidthUnit;
  I: Integer;
begin
  AIdenticalCellWidth := True;

  AFirstCellWidth := FCellsInSelectedColumns[0].PreferredWidth;
  for I := 1 to FCellsInSelectedColumns.Count - 1 do
  begin
    AIdenticalCellWidth := EqualsWidthUnit(AFirstCellWidth, FCellsInSelectedColumns[I].PreferredWidth);
    if not AIdenticalCellWidth then
      Break;
  end;
  UseDefaultColumnWidth := TdxNullableBoolean.IfThen(AIdenticalCellWidth, AFirstCellWidth.&Type = TdxWidthUnitType.Auto);
  ColumnWidth := ModelUnitToPercent(AFirstCellWidth);
  ColumnWidthUnitType := AFirstCellWidth.&Type;
end;

procedure TdxTablePropertiesFormController.InitializeController;
begin
  InitializeTableTab;
  InitializeRowTab;
  InitializeColumnTab;
  InitializeCellTab;
end;

procedure TdxTablePropertiesFormController.InitializeRowTab;
var
  ASelectedRows: TdxTableRowList;
  AIdenticalCantSplit, AIdenticalHeader, AIdenticalRowHeight, AFirstRowCantSplit, AFirstRowHeader: Boolean;
  AFirstRowHeight: TdxHeightUnit;
  AFirstSelectedRow, ACurrentRow: TdxTableRow;
  I: Integer;
begin
  AIdenticalCantSplit := True;
  AIdenticalHeader := True;
  AIdenticalRowHeight := True;
  ASelectedRows := FSourceSelectedCells.GetSelectedTableRows;
  try
    AFirstSelectedRow := ASelectedRows[0];
    AFirstRowCantSplit := AFirstSelectedRow.CantSplit;
    AFirstRowHeader := AFirstSelectedRow.Header;
    AFirstRowHeight := AFirstSelectedRow.Height;

    for I := 0 to ASelectedRows.Count - 1 do
    begin
      ACurrentRow := ASelectedRows[I];
      AIdenticalCantSplit := AIdenticalCantSplit and (AFirstRowCantSplit = ACurrentRow.CantSplit);
      AIdenticalHeader := AIdenticalHeader and (AFirstRowHeader = ACurrentRow.Header);
      AIdenticalRowHeight := AIdenticalRowHeight and EqualsHeightUnits(AFirstRowHeight, ACurrentRow.Height);
    end;
  finally
    ASelectedRows.Free;
  end;
  if AIdenticalRowHeight then
    UseDefaultRowHeight := AFirstRowHeight.Value = 0
  else
    UseDefaultRowHeight := TdxNullableBoolean.Null;

  RowHeight := IfThen(AIdenticalRowHeight, AFirstRowHeight.Value);
  if AFirstRowHeight.&Type = TdxHeightUnitType.Exact then
    RowHeightType := TdxHeightUnitType.Exact
  else
    RowHeightType := TdxHeightUnitType.Minimum;

  RowCantSplit := TdxNullableBoolean.IfThen(AIdenticalCantSplit, AFirstRowCantSplit);
  RowHeader := TdxNullableBoolean.IfThen(AIdenticalHeader, AFirstRowHeader);
end;

procedure TdxTablePropertiesFormController.InitializeTableTab;
var
  ATableWidth: TdxWidthUnit;
begin
  ATableWidth := Table.PreferredWidth;
  UseDefaultTableWidth := (ATableWidth.&Type = TdxWidthUnitType.Auto) or (ATableWidth.&Type = TdxWidthUnitType.&Nil);
  TableWidth := ModelUnitToPercent(ATableWidth);
  if ATableWidth.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    TableWidthUnitType := TdxWidthUnitType.FiftiethsOfPercent
  else
    TableWidthUnitType := TdxWidthUnitType.ModelUnits;
  TableAlignment := GetTableAlignment;
  TableIndent := GetTableIndent;
end;

function TdxTablePropertiesFormController.IsCellWidthValueChange(ACellWidth: TdxWidthUnitInfo): Boolean;
var
  ANewValue: TdxNullableInteger;
  AValue: Integer;
begin
  if ACellWidth.&Type = FOldCellWidth.&Type then
    Result := not ACellWidth.Equals(FOldCellWidth)
  else
  begin
    if ACellWidth.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    begin
      AValue := ACellWidth.Value div 50;
      ANewValue := CalculateValue(FOldCellWidth.Value, ACellWidth.&Type);
    end
    else
    begin
      AValue := FOldCellWidth.Value * 50;
      ANewValue := CalculateValue(ACellWidth.Value, FOldCellWidth.&Type);
    end;
    Result := ANewValue <> AValue;
  end;
end;

function TdxTablePropertiesFormController.IsSelectedFirstRowInTable: Boolean;
var
  ASelectedRows: TdxTableRowList;
  I: Integer;
begin
  ASelectedRows := FSourceSelectedCells.GetSelectedTableRows;
  try
    for I := 0 to ASelectedRows.Count - 1 do
      if ASelectedRows[I].IsFirstRowInTable then
        Exit(True);
    Result := False;
  finally
    ASelectedRows.Free;
  end;
end;

function TdxTablePropertiesFormController.ModelUnitToPercent(AWidth: TdxWidthUnit): Integer;
begin
  if AWidth.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    Result := AWidth.Value div 50
  else
    Result := AWidth.Value;
end;

function TdxTablePropertiesFormController.PercentToModelUnit(AWidth: Integer; AType: TdxWidthUnitType): Integer;
begin
  if AType = TdxWidthUnitType.FiftiethsOfPercent then
    Result := AWidth * 50
  else
    Result := AWidth;
end;

end.

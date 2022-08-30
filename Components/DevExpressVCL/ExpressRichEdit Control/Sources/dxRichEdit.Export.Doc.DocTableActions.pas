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
unit dxRichEdit.Export.Doc.DocTableActions;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  SysUtils, Classes, Generics.Defaults, Generics.Collections, dxCore, dxCoreClasses,
  dxCoreGraphics, dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.WidthsContentInfo,
  dxRichEdit.DocumentModel.UnitConverter,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.Import.Doc.DCO;

type

  { TdxDocTableWidthsCalculator }

  TdxDocTableWidthsCalculator = class(TdxTableWidthsCalculatorBase)
  public const
    DefaultPreferredWidthInTwips   = Integer(168);
    DefaultPercentBaseWidthInTwips = Integer(5 * 1440);
  protected
    function CalculateCellContentWidthsCore(ACell: TdxTableCell; APercentBaseWidth: Integer; ASmpleView: Boolean): TdxWidthsContentInfo; override;
  public
    constructor Create(AConverter: TdxDocumentModelUnitToLayoutUnitConverter);
    function CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean; override;
    function CreateTableLayoutInfo(ATableGrid: TdxTableGrid; AMaxTableWidth: TdxModelUnit;
      AAllowTablesToExtendIntoMargins: Boolean; ASimpleView: Boolean; APercentBaseWidth: TdxModelUnit): TdxTableLayoutInfo; override;
  end;

  { TdxDocTableCellActions }

  TdxDocTableCellActions = class
  strict private
    FWriter: TBinaryWriter;
    FCell: TdxTableCell;
    FTableProperties: TdxMergedTableProperties;
    function GetTableProperties: TdxMergedTableProperties;
  protected
    property Cell: TdxTableCell read FCell;
    property TableProperties: TdxMergedTableProperties read GetTableProperties;
  public
    constructor Create(AOutput: TdxMemoryStream; ACell: TdxTableCell);
    destructor Destroy; override;
    procedure CreateTableCellPropertyModifiers(ATableDepth: Integer; AFinalParagraphInCell: Boolean);
    procedure InTableAction;
    procedure TableDepthAction(ATableDepth: Integer; AFinalParagraphInCell: Boolean);
    procedure InnerTableCellAction;
    procedure TableAnchorsAction;
  end;

  { TdxDocTableRowActions }

  TdxDocTableRowActions = class
  public const
    FirstCellsGroupCount  = Integer(22);
    SecondCellsGroupCount = Integer(44);
    ThirdCellsGroupCount  = Integer(64);
  strict private
    FWriter: TBinaryWriter;
    FRow: TdxTableRow;
    FDocumentModel: TdxDocumentModel;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FModelUnitToTwipsUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FGrid: TdxTableGrid;
  protected
    property Row: TdxTableRow read FRow;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
    property ToLayoutUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter read FModelUnitToTwipsUnitConverter;
    property Grid: TdxTableGrid read FGrid;
  public
    constructor Create(AOutput: TdxMemoryStream; ARow: TdxTableRow; AGrid: TdxTableGrid);
    destructor Destroy; override;
    procedure CreateTableRowPropertyModifiers(ATableDepth: Integer; ATableStyleIndex: Integer);
    procedure TableDepthAction(ATableDepth: Integer);
    procedure InnerTableRowAction;
    procedure TableTrailerAction;
    procedure TableStyleAction(ATableStyleIndex: Integer);
    procedure InsertAction;
    procedure CreateInsertCommand(ACellIndex: Integer; ATotalWidth: SmallInt);
    procedure PreferredCellWidthsAction;
    procedure CreatePreferredCellWidthCommand(I: Integer; ACell: TdxTableCell);
    procedure CellBordersAction;
    procedure CreateCellBordersCommand(ACell: TdxTableCell);
    procedure CreateCellBorderCommandCore(AInfo: TdxBorderInfo; ACellBorderType: TdxDocTableCellBorders; AIndex: Integer);
    procedure CellMarginsAction;
    procedure CreateCellMarginsCommands(ACell: TdxTableCell);
    procedure CreateCellMarginCommandCore(AInfo: TdxWidthUnitInfo; ACellBorderType: TdxDocTableCellBorders; AIndex: Integer);
    procedure CellsBackgroundColorAction;
    procedure DefineTableShadingsActionCore(AStartGroupIndex: Integer; AEndGroupIndex: Integer; ACommandType: TClass);
    procedure CellsVerticalAlignmentAction;
    procedure CreateCellVertacalAlignmentCommand(ACell: TdxTableCell);
    procedure VerticalMergingAction;
    procedure CreateVerticalMergeCommand(ACell: TdxTableCell);
    procedure HideCellMarkAction;
    procedure CreateHideCellMarkCommand(ACell: TdxTableCell);
    procedure HeightAction;
    procedure WidthBeforeAction;
    procedure WidthAfterAction;
    procedure DxaLeftAction;
    procedure DxaGapHalfAction;
    function GetWidthBefore: Integer;
    function GetCellWidth(AFirstColumnIndex: Integer; ACell: TdxTableCell): SmallInt;
    function GetWidthAfter: Integer;
    function GetTableIndent: Integer;
    function GetCellSpacing: Integer;
    function GetGapHalf: Integer;
    function ConvertLayoutUnitsToTwips(ATotalWidth: Integer): Integer;
  end;

  { TdxDocTableActions }

  TdxDocTableActions = class
  strict private
    FWriter: TBinaryWriter;
    FProperties: TdxMergedTableProperties;
    FTableLayout: TdxTableLayoutType;
    FCellSpacing: TdxWidthUnit;
    FUnitConverter: TdxDocumentModelUnitConverter;
    FDocumentModel: TdxDocumentModel;
    function GetFloatingPosition: TdxTableFloatingPositionInfo;
  protected
    property TableProperties: TdxMergedTableProperties read FProperties;
    property FloatingPosition: TdxTableFloatingPositionInfo read GetFloatingPosition;
    property TableLayout: TdxTableLayoutType read FTableLayout;
    property CellSpacing: TdxWidthUnit read FCellSpacing;
    property UnitConverter: TdxDocumentModelUnitConverter read FUnitConverter;
  public
    constructor Create(AOutput: TdxMemoryStream); overload;
    constructor Create(AOutput: TdxMemoryStream; ATableStyle: TdxTableStyle); overload;
    constructor Create(AOutput: TdxMemoryStream; ARow: TdxTableRow); overload;
    destructor Destroy; override;
    procedure CreateTablePropertyModifiers;
    procedure PreferredTableWidthAction;
    procedure AutofitAction;
    procedure BordersAction;
    procedure MarginsAction;
    procedure BottomMarginAction;
    procedure LeftMarginAction;
    procedure RightMarginAction;
    procedure TopMarginAction;
    procedure TableIndentAction;
    procedure TableAlignmentAction;
    procedure CellSpacingAction;
    procedure BackgroundColorAction;
    procedure AllowOverlapAction;
    procedure FloatingPositionAction;
    procedure BottomFromTextAction;
    procedure LeftFromTextAction;
    procedure RightFromTextAction;
    procedure TopFromTextAction;
    procedure HorizontalAlignAction;
    function CalcTableHorizontalPositionTypeCode: Integer;
    procedure VerticalAlignAction;
    function CalcTableVerticalPositionTypeCode: Integer;
    procedure TableStyleRowBandSizeAction;
    procedure TableStyleColBandSizeAction;
    procedure TableAnchorsAction;
  end;

implementation

uses
  Math, Contnrs,
  dxEncoding,
  dxStringHelper,
  dxRichEdit.DocumentLayout.UnitConverter,
  dxRichEdit.Doc.Utils,
  dxRichEdit.Import.Doc.DocCommand;

{ TdxDocTableWidthsCalculator }

constructor TdxDocTableWidthsCalculator.Create(AConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create(AConverter, AConverter.ToLayoutUnits(DefaultPercentBaseWidthInTwips));
end;

function TdxDocTableWidthsCalculator.CalculateCellContentWidthsCore(ACell: TdxTableCell; APercentBaseWidth: Integer; ASmpleView: Boolean): TdxWidthsContentInfo;
var
  APreferredWidth: TdxLayoutUnit;
begin
  APreferredWidth := 0;
  if ACell.PreferredWidth.&Type = TdxWidthUnitType.ModelUnits then
    APreferredWidth := Converter.ToLayoutUnits(ACell.PreferredWidth.Value);
  if APreferredWidth = 0 then
    APreferredWidth := Converter.ToLayoutUnits(DefaultPreferredWidthInTwips) * ACell.ColumnSpan;
  Result := TdxWidthsContentInfo.Create(APreferredWidth, APreferredWidth);
end;

function TdxDocTableWidthsCalculator.CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean;
begin
  Result := False;
end;

function TdxDocTableWidthsCalculator.CreateTableLayoutInfo(ATableGrid: TdxTableGrid; AMaxTableWidth: TdxModelUnit;
  AAllowTablesToExtendIntoMargins: Boolean; ASimpleView: Boolean; APercentBaseWidth: TdxModelUnit): TdxTableLayoutInfo;
begin
  Result := nil;
end;

{ TdxDocTableCellActions }

constructor TdxDocTableCellActions.Create(AOutput: TdxMemoryStream; ACell: TdxTableCell);
begin
  Assert(ACell <> nil, 'cell');
  FWriter := TBinaryWriter.Create(AOutput);
  FCell := ACell;
end;

destructor TdxDocTableCellActions.Destroy;
begin
  FTableProperties.Free;
  FWriter.Free;
  inherited Destroy;
end;

function TdxDocTableCellActions.GetTableProperties: TdxMergedTableProperties;
begin
  if FTableProperties = nil then
    FTableProperties := FCell.Table.GetMergedWithStyleTableProperties;
  Result := FTableProperties;
end;

procedure TdxDocTableCellActions.CreateTableCellPropertyModifiers(ATableDepth: Integer; AFinalParagraphInCell: Boolean);
begin
  InTableAction;
  TableDepthAction(ATableDepth, AFinalParagraphInCell);
  TableAnchorsAction;
end;

procedure TdxDocTableCellActions.InTableAction;
var
  AInTableCommand: TdxDocCommandInTable;
begin
  AInTableCommand := TdxDocCommandInTable.Create;
  try
    AInTableCommand.Value := True;
    AInTableCommand.Write(FWriter);
  finally
    AInTableCommand.Free;
  end;
end;

procedure TdxDocTableCellActions.TableDepthAction(ATableDepth: Integer; AFinalParagraphInCell: Boolean);
var
  ATableDepthCommand: TdxDocCommandTableDepth;
begin
  ATableDepthCommand := TdxDocCommandTableDepth.Create;
  try
    ATableDepthCommand.Value := ATableDepth;
    ATableDepthCommand.Write(FWriter);
    if (ATableDepth > 1) and AFinalParagraphInCell then
      InnerTableCellAction;
  finally
    ATableDepthCommand.Free;
  end;
end;

procedure TdxDocTableCellActions.InnerTableCellAction;
var
  ACommand: TdxDocCommandInnerTableCell;
begin
  ACommand := TdxDocCommandInnerTableCell.Create;
  try
    ACommand.Value := True;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableCellActions.TableAnchorsAction;
var
  AInfo: TdxTableFloatingPositionInfo;
  ACommand: TdxDocCommandTablePosition;
begin
  if not TableProperties.Options.UseFloatingPosition then
    Exit;
  AInfo := TableProperties.Info.FloatingPosition;
  if AInfo.IsHorizontalAbsolutePositionUse and AInfo.IsVerticalAbsolutePositionUse then
    Exit;
  ACommand := TdxDocCommandTablePosition.Create(AInfo);
  try
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

{ TdxDocTableRowActions }

constructor TdxDocTableRowActions.Create(AOutput: TdxMemoryStream; ARow: TdxTableRow; AGrid: TdxTableGrid);
begin
  FWriter := TBinaryWriter.Create(AOutput);
  FRow := ARow;
  FDocumentModel := TdxDocumentModel(ARow.DocumentModel);
  FUnitConverter := ARow.DocumentModel.UnitConverter;
  FModelUnitToTwipsUnitConverter := FUnitConverter.CreateConverterToLayoutUnits(TdxDocumentLayoutUnit.Twip);
  FGrid := AGrid;
end;

destructor TdxDocTableRowActions.Destroy;
begin
  FModelUnitToTwipsUnitConverter.Free;
  FWriter.Free;
  inherited Destroy;
end;

procedure TdxDocTableRowActions.CreateTableRowPropertyModifiers(ATableDepth: Integer; ATableStyleIndex: Integer);
begin
  TableDepthAction(ATableDepth);
  DxaLeftAction;
  InsertAction;
  TableStyleAction(ATableStyleIndex);
  PreferredCellWidthsAction;
  CellBordersAction;
  CellMarginsAction;
  CellsBackgroundColorAction;
  CellsVerticalAlignmentAction;
  VerticalMergingAction;
  HideCellMarkAction;
  HeightAction;
  WidthBeforeAction;
  WidthAfterAction;
  DxaGapHalfAction;
end;

procedure TdxDocTableRowActions.TableDepthAction(ATableDepth: Integer);
var
  AInTableCommand: TdxDocCommandInTable;
  ATableDepthCommand: TdxDocCommandTableDepth;
begin
  AInTableCommand := TdxDocCommandInTable.Create;
  try
    AInTableCommand.Value := True;
    AInTableCommand.Write(FWriter);
  finally
    AInTableCommand.Free;
  end;

  if ATableDepth > 1 then
    InnerTableRowAction
  else
    TableTrailerAction;

  ATableDepthCommand := TdxDocCommandTableDepth.Create;
  try
    ATableDepthCommand.Value := ATableDepth;
    ATableDepthCommand.Write(FWriter);
  finally
    ATableDepthCommand.Free;
  end;
end;

procedure TdxDocTableRowActions.InnerTableRowAction;
var
  ACellCommand: TdxDocCommandInnerTableCell;
  ARowCommand: TdxDocCommandInnerTableTrailer;
begin
  ACellCommand := TdxDocCommandInnerTableCell.Create;
  try
    ACellCommand.Value := True;
    ACellCommand.Write(FWriter);
  finally
    ACellCommand.Free;
  end;
  ARowCommand := TdxDocCommandInnerTableTrailer.Create;
  try
    ARowCommand.Value := True;
    ARowCommand.Write(FWriter);
  finally
    ARowCommand.Free;
  end;
end;

procedure TdxDocTableRowActions.TableTrailerAction;
var
  ATableTrailerCommand: TdxDocCommandTableTrailer;
begin
  ATableTrailerCommand := TdxDocCommandTableTrailer.Create;
  try
    ATableTrailerCommand.Value := True;
    ATableTrailerCommand.Write(FWriter);
  finally
    ATableTrailerCommand.Free;
  end;
end;

procedure TdxDocTableRowActions.TableStyleAction(ATableStyleIndex: Integer);
var
  ACommand: TdxDocCommandChangeTableStyle;
begin
  ACommand := TdxDocCommandChangeTableStyle.Create;
  try
    ACommand.Value := ATableStyleIndex;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.InsertAction;
var
  ACount, ACurrentColumnIndex, ACellIndex: Integer;
  ACell: TdxTableCell;
  ACellWidth: SmallInt;
begin
  ACount := Row.Cells.Count;
  ACurrentColumnIndex := Row.GridBefore;

  for ACellIndex := 0 to ACount - 1 do
  begin
    ACell := Row.Cells[ACellIndex];
    ACellWidth := GetCellWidth(ACurrentColumnIndex, ACell);
    CreateInsertCommand(ACellIndex, ACellWidth);
    Inc(ACurrentColumnIndex, ACell.ColumnSpan);
  end;
end;

procedure TdxDocTableRowActions.CreateInsertCommand(ACellIndex: Integer; ATotalWidth: SmallInt);
var
  AInsertCommand: TdxDocCommandInsertTableCell;
begin
  AInsertCommand := TdxDocCommandInsertTableCell.Create;
  try
    AInsertCommand.Insert.StartIndex := Byte(ACellIndex);
    AInsertCommand.Insert.Count := 1;
    AInsertCommand.Insert.WidthInTwips := ATotalWidth;
    AInsertCommand.Write(FWriter);
  finally
    AInsertCommand.Free;
  end;
end;

procedure TdxDocTableRowActions.PreferredCellWidthsAction;
var
  ACount, I: Integer;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
    CreatePreferredCellWidthCommand(I, Row.Cells[I]);
end;

procedure TdxDocTableRowActions.CreatePreferredCellWidthCommand(I: Integer; ACell: TdxTableCell);
var
  ACommand: TdxDocCommandPreferredTableCellWidth;
begin
  ACommand := TdxDocCommandPreferredTableCellWidth.Create;
  try
    ACommand.TableCellWidth.StartIndex := Byte(I);
    ACommand.TableCellWidth.EndIndex := Byte(I);
    ACommand.TableCellWidth.WidthUnit.ConvertFromWidthUnitInfo(ACell.PreferredWidth.Info, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.CellBordersAction;
var
  ACount, I: Integer;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
    CreateCellBordersCommand(Row.Cells[I]);
end;

procedure TdxDocTableRowActions.CreateCellBordersCommand(ACell: TdxTableCell);
var
  ABorders: TdxTableCellBorders;
begin
  ABorders := ACell.Properties.Borders;
  if ABorders.UseBottomBorder then
    CreateCellBorderCommandCore(ABorders.BottomBorder.Info, [TdxDocTableCellBorder.Bottom], ACell.IndexInRow);
  if ABorders.UseLeftBorder then
    CreateCellBorderCommandCore(ABorders.LeftBorder.Info, [TdxDocTableCellBorder.Left], ACell.IndexInRow);
  if ABorders.UseRightBorder then
    CreateCellBorderCommandCore(ABorders.RightBorder.Info, [TdxDocTableCellBorder.Right], ACell.IndexInRow);
  if ABorders.UseTopBorder then
    CreateCellBorderCommandCore(ABorders.TopBorder.Info, [TdxDocTableCellBorder.Top], ACell.IndexInRow);
end;

procedure TdxDocTableRowActions.CreateCellBorderCommandCore(AInfo: TdxBorderInfo; ACellBorderType: TdxDocTableCellBorders; AIndex: Integer);
var
  ACommand: TdxDocCommandOverrideCellBorders;
begin
  ACommand := TdxDocCommandOverrideCellBorders.Create;
  try
    ACommand.OverriddenBorders.StartIndex := Byte(AIndex);
    ACommand.OverriddenBorders.EndIndex := Byte(AIndex);

    ACommand.OverriddenBorders.Border.ConvertFromBorderInfo(AInfo, UnitConverter);

    ACommand.OverriddenBorders.CellBorders := ACellBorderType;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.CellMarginsAction;
var
  ACount, I: Integer;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
    CreateCellMarginsCommands(Row.Cells[I]);
end;

procedure TdxDocTableRowActions.CreateCellMarginsCommands(ACell: TdxTableCell);
begin
  if ACell.Properties.UseBottomMargin then
    CreateCellMarginCommandCore(ACell.Properties.CellMargins.Bottom.Info, [TdxDocTableCellBorder.Bottom], ACell.IndexInRow);
  if ACell.Properties.UseLeftMargin then
    CreateCellMarginCommandCore(ACell.Properties.CellMargins.Left.Info, [TdxDocTableCellBorder.Left], ACell.IndexInRow);
  if ACell.Properties.UseRightMargin then
    CreateCellMarginCommandCore(ACell.Properties.CellMargins.Right.Info, [TdxDocTableCellBorder.Right], ACell.IndexInRow);
  if ACell.Properties.UseTopMargin then
    CreateCellMarginCommandCore(ACell.Properties.CellMargins.Top.Info, [TdxDocTableCellBorder.Top], ACell.IndexInRow);
end;

procedure TdxDocTableRowActions.CreateCellMarginCommandCore(AInfo: TdxWidthUnitInfo; ACellBorderType: TdxDocTableCellBorders; AIndex: Integer);
var
  ACommand: TdxDocCommandCellMargin;
begin
  ACommand := TdxDocCommandCellMargin.Create;
  try
    ACommand.CellSpacing.CellBorders := ACellBorderType;
    ACommand.CellSpacing.StartIndex := AIndex;
    ACommand.CellSpacing.EndIndex := AIndex;
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(AInfo, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.CellsBackgroundColorAction;
begin
  DefineTableShadingsActionCore(0, FirstCellsGroupCount, TdxDocCommandDefineTableShadings);
  DefineTableShadingsActionCore(FirstCellsGroupCount, SecondCellsGroupCount, TdxDocCommandDefineTableShadings2nd);
  DefineTableShadingsActionCore(SecondCellsGroupCount, ThirdCellsGroupCount, TdxDocCommandDefineTableShadings3rd);
end;

procedure TdxDocTableRowActions.DefineTableShadingsActionCore(AStartGroupIndex: Integer; AEndGroupIndex: Integer; ACommandType: TClass);
var
  ACommand: TdxDocCommandShadingListBase;
  ACount, I: Integer;
begin
  if Row.Cells.Count < AStartGroupIndex then
    Exit;

  ACommand := TdxDocCommandShadingListBaseClass(ACommandType).Create;
  try
    ACount := Min(Row.Cells.Count, AEndGroupIndex);
    for I := AStartGroupIndex to ACount - 1 do
    begin
      if Row.Cells[I].Properties.UseBackgroundColor then
        ACommand.CellColors.Add(Row.Cells[I].Properties.BackgroundColor)
      else
        ACommand.CellColors.Add(TdxAlphaColors.Empty);
    end;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.CellsVerticalAlignmentAction;
var
  ACount, I: Integer;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
    CreateCellVertacalAlignmentCommand(Row.Cells[I]);
end;

procedure TdxDocTableRowActions.CreateCellVertacalAlignmentCommand(ACell: TdxTableCell);
var
  ACommand: TdxDocCommandCellRangeVerticalAlignment;
begin
  if not ACell.Properties.UseVerticalAlignment then
    Exit;

  ACommand := TdxDocCommandCellRangeVerticalAlignment.Create;
  try
    ACommand.CellRangeVerticalAlignment.StartIndex := Byte(ACell.IndexInRow);
    ACommand.CellRangeVerticalAlignment.EndIndex := Byte(ACell.IndexInRow);
    ACommand.CellRangeVerticalAlignment.VerticalAlignment := ACell.VerticalAlignment;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.VerticalMergingAction;
var
  ACount, I: Integer;
  ACurrentCell: TdxTableCell;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentCell := Row.Cells[I];
    CreateVerticalMergeCommand(ACurrentCell);
  end;
end;

procedure TdxDocTableRowActions.CreateVerticalMergeCommand(ACell: TdxTableCell);
var
  ACommand: TdxDocCommandVerticalMergeTableCells;
begin
  if ACell.VerticalMerging = TdxMergingState.None then
    Exit;
  ACommand := TdxDocCommandVerticalMergeTableCells.Create;
  try
    ACommand.CellIndex := Byte(ACell.IndexInRow);
    ACommand.VerticalMerging := ACell.VerticalMerging;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.HideCellMarkAction;
var
  ACount, I: Integer;
  ACurrentCell: TdxTableCell;
begin
  ACount := Row.Cells.Count;
  for I := 0 to ACount - 1 do
  begin
    ACurrentCell := Row.Cells[I];
    CreateHideCellMarkCommand(ACurrentCell);
  end;
end;

procedure TdxDocTableRowActions.CreateHideCellMarkCommand(ACell: TdxTableCell);
var
  ACommand: TdxDocCommandHideCellMark;
begin
  if not ACell.Properties.HideCellMark then
    Exit;
  ACommand := TdxDocCommandHideCellMark.Create;
  try
    ACommand.CellHideMark.StartIndex := Byte(ACell.IndexInRow);
    ACommand.CellHideMark.EndIndex := Byte(ACell.IndexInRow);
    ACommand.CellHideMark.HideCellMark := True;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.HeightAction;
var
  ACommand: TdxDocCommandTableRowHeight;
begin
  if not FRow.Properties.UseHeight then
    Exit;
  ACommand := TdxDocCommandTableRowHeight.Create;
  try
    ACommand.&Type := Row.Properties.Height.&Type;
    ACommand.Value := Row.Properties.Height.Value;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.WidthBeforeAction;
var
  ACommand: TdxDocCommandWidthBefore;
begin
  if not Row.Properties.UseWidthBefore then
    Exit;
  ACommand := TdxDocCommandWidthBefore.Create;
  try
    ACommand.WidthUnit.&Type := TdxWidthUnitType.ModelUnits;
    ACommand.WidthUnit.Value := GetWidthBefore;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.WidthAfterAction;
var
  ACommand: TdxDocCommandWidthAfter;
begin
  if not Row.Properties.UseWidthAfter then
    Exit;
  ACommand := TdxDocCommandWidthAfter.Create;
  try
    ACommand.WidthUnit.&Type := TdxWidthUnitType.ModelUnits;
    ACommand.WidthUnit.Value := GetWidthAfter;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.DxaLeftAction;
var
  ADxaLeft: Integer;
  ACommand: TdxDocCommandTableDxaLeft;
begin
  if not Row.Properties.UseWidthBefore and not Row.Table.TableProperties.UseTableIndent and not Row.Table.TableProperties.UseCellSpacing then
    Exit;
  ADxaLeft := GetTableIndent + GetWidthBefore - GetCellSpacing;
  ACommand := TdxDocCommandTableDxaLeft.Create;
  try
    ACommand.Value := ADxaLeft;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableRowActions.DxaGapHalfAction;
var
  ACommand: TdxDocCommandTableDxaGapHalf;
begin
  if not Row.Table.TableProperties.UseLeftMargin and not Row.Table.TableProperties.UseRightMargin then
    Exit;
  ACommand := TdxDocCommandTableDxaGapHalf.Create;
  try
    ACommand.Value := GetGapHalf;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

function TdxDocTableRowActions.GetWidthBefore: Integer;
var
  ATotalWidth, ACount, I: Integer;
begin
  ATotalWidth := 0;
  ACount := Row.GridBefore;
  for I := 0 to ACount - 1 do
    Inc(ATotalWidth, Grid.Columns[I].Width);
  Result := ConvertLayoutUnitsToTwips(ATotalWidth);
end;

function TdxDocTableRowActions.GetCellWidth(AFirstColumnIndex: Integer; ACell: TdxTableCell): SmallInt;
var
  ATotalWidth, ALastColumnIndex, AColumnIndex: Integer;
begin
  ATotalWidth := 0;
  ALastColumnIndex := AFirstColumnIndex + ACell.ColumnSpan;
  for AColumnIndex := AFirstColumnIndex to ALastColumnIndex - 1 do
    Inc(ATotalWidth, Grid.Columns[AColumnIndex].Width);
  Result := SmallInt(Min(ConvertLayoutUnitsToTwips(ATotalWidth), TdxDocConstants.MaxXASValue));
end;

function TdxDocTableRowActions.GetWidthAfter: Integer;
var
  ATotalWidth, ACount, I: Integer;
begin
  ATotalWidth := 0;
  ACount := Row.GridAfter;
  for I := 0 to ACount - 1 do
    Inc(ATotalWidth, Grid.Columns[Grid.Columns.Count - I - 1].Width);
  Result := ConvertLayoutUnitsToTwips(ATotalWidth);
end;

function TdxDocTableRowActions.GetTableIndent: Integer;
begin
  if Row.Table.TableProperties.TableIndent.&Type = TdxWidthUnitType.ModelUnits then
    Result := UnitConverter.ModelUnitsToTwips(Row.Table.TableProperties.TableIndent.Value)
  else
    Result := 0;
end;

function TdxDocTableRowActions.GetCellSpacing: Integer;
begin
  if Row.Table.TableProperties.CellSpacing.&Type = TdxWidthUnitType.ModelUnits then
    Result := UnitConverter.ModelUnitsToTwips(Row.Table.TableProperties.CellSpacing.Value)
  else
    Result := 0;
end;

function TdxDocTableRowActions.GetGapHalf: Integer;
var
  AResult: Integer;
  ATable: TdxTable;
  AProperties: TdxTableProperties;
begin
  AResult := 0;
  ATable := Row.Table;
  AProperties := ATable.TableProperties;
  if AProperties.UseLeftMargin and (ATable.LeftMargin.&Type = TdxWidthUnitType.ModelUnits) then
    Inc(AResult, UnitConverter.ModelUnitsToTwips(ATable.LeftMargin.Value));
  if AProperties.UseRightMargin and (ATable.RightMargin.&Type = TdxWidthUnitType.ModelUnits) then
    Inc(AResult, UnitConverter.ModelUnitsToTwips(ATable.RightMargin.Value));
  Result := AResult div 2;
end;

function TdxDocTableRowActions.ConvertLayoutUnitsToTwips(ATotalWidth: Integer): Integer;
begin
  Result := ToLayoutUnitConverter.ToModelUnits(ATotalWidth);
  Result := UnitConverter.ModelUnitsToTwips(Result);
end;

{ TdxDocTableActions }

constructor TdxDocTableActions.Create(AOutput: TdxMemoryStream; ARow: TdxTableRow);
var
  ATable: TdxTable;
  ATablePropertiesException: TdxTableProperties;
begin
  Create(AOutput);
  ATable := ARow.Table;
  FDocumentModel := TdxDocumentModel(ATable.DocumentModel);
  FUnitConverter := ATable.DocumentModel.UnitConverter;
  ATablePropertiesException := ARow.TablePropertiesException;
  if (ATablePropertiesException = nil) or (ATablePropertiesException.Info.Value = TdxTablePropertiesOptions.MaskUseNone) then
  begin
    FProperties := ATable.GetMergedWithStyleTableProperties;
    FTableLayout := ATable.TableLayout;
    FCellSpacing := ATable.CellSpacing;
  end
  else
  begin
    FProperties := TdxMergedTableProperties.Create(ATablePropertiesException);
    if ATablePropertiesException.Info.UseTableLayout then
      FTableLayout := ATablePropertiesException.TableLayout
    else
      FTableLayout := ATable.TableLayout;
    if ATablePropertiesException.Info.UseCellSpacing then
      FCellSpacing := ATablePropertiesException.CellSpacing
    else
      FCellSpacing := ATable.CellSpacing;
  end;
end;

constructor TdxDocTableActions.Create(AOutput: TdxMemoryStream; ATableStyle: TdxTableStyle);
begin
  Create(AOutput);
  FProperties := ATableStyle.GetMergedTableProperties;
  FDocumentModel := TdxDocumentModel(ATableStyle.DocumentModel);
  FUnitConverter := ATableStyle.DocumentModel.UnitConverter;
end;

constructor TdxDocTableActions.Create(AOutput: TdxMemoryStream);
begin
  Assert(AOutput <> nil, 'output');
  FWriter := TBinaryWriter.Create(AOutput);
  FTableLayout := TdxTableLayoutType.Autofit;
end;

destructor TdxDocTableActions.Destroy;
begin
  FWriter.Free;
  FProperties.Free;
  inherited Destroy;
end;

function TdxDocTableActions.GetFloatingPosition: TdxTableFloatingPositionInfo;
begin
  Result := FProperties.Info.FloatingPosition;
end;

procedure TdxDocTableActions.CreateTablePropertyModifiers;
begin
  PreferredTableWidthAction;
  AutofitAction;
  BordersAction;
  MarginsAction;
  TableAlignmentAction;
  TableIndentAction;
  CellSpacingAction;

  BackgroundColorAction;

  AllowOverlapAction;
  FloatingPositionAction;
end;

procedure TdxDocTableActions.PreferredTableWidthAction;
var
  ACommand: TdxDocCommandPreferredTableWidth;
begin
  if not TableProperties.Options.UsePreferredWidth then
    Exit;
  ACommand := TdxDocCommandPreferredTableWidth.Create;
  try
    ACommand.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.PreferredWidth, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.AutofitAction;
var
  ACommand: TdxDocCommandTableAutoFit;
begin
  ACommand := TdxDocCommandTableAutoFit.Create;
  try
    ACommand.Value := (TableLayout = TdxTableLayoutType.Autofit);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.BordersAction;
var
  ABorders: TdxCombinedTableBordersInfo;
  ACommand: TdxDocCommandTableBorders;
begin
  if not TableProperties.Options.UseBorders then
    Exit;
  ABorders := TableProperties.Info.Borders;
  ACommand := TdxDocCommandTableBorders.Create;
  try
    ACommand.TableBorders.BottomBorder.ConvertFromBorderInfo(ABorders.BottomBorder, FUnitConverter);
    ACommand.TableBorders.InsideHorizontalBorder.ConvertFromBorderInfo(ABorders.InsideHorizontalBorder, FUnitConverter);
    ACommand.TableBorders.InsideVerticalBorder.ConvertFromBorderInfo(ABorders.InsideVerticalBorder, FUnitConverter);
    ACommand.TableBorders.LeftBorder.ConvertFromBorderInfo(ABorders.LeftBorder, FUnitConverter);
    ACommand.TableBorders.RightBorder.ConvertFromBorderInfo(ABorders.RightBorder, FUnitConverter);
    ACommand.TableBorders.TopBorder.ConvertFromBorderInfo(ABorders.TopBorder, FUnitConverter);

    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.MarginsAction;
begin
  BottomMarginAction;
  LeftMarginAction;
  RightMarginAction;
  TopMarginAction;
end;

procedure TdxDocTableActions.BottomMarginAction;
var
  ACommand: TdxDocCommandCellMarginDefault;
begin
  if not TableProperties.Options.UseBottomMargin then
    Exit;
  ACommand := TdxDocCommandCellMarginDefault.Create;
  try
    ACommand.CellSpacing.CellBorders := [TdxDocTableCellBorder.Bottom];
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.CellMargins.Bottom, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.LeftMarginAction;
var
  ACommand: TdxDocCommandCellMarginDefault;
begin
  if not TableProperties.Options.UseLeftMargin then
    Exit;
  ACommand := TdxDocCommandCellMarginDefault.Create;
  try
    ACommand.CellSpacing.CellBorders := [TdxDocTableCellBorder.Left];
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.CellMargins.Left, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.RightMarginAction;
var
  ACommand: TdxDocCommandCellMarginDefault;
begin
  if not TableProperties.Options.UseRightMargin then
    Exit;
  ACommand := TdxDocCommandCellMarginDefault.Create;
  try
    ACommand.CellSpacing.CellBorders := [TdxDocTableCellBorder.Right];
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.CellMargins.Right, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TopMarginAction;
var
  ACommand: TdxDocCommandCellMarginDefault;
begin
  if not TableProperties.Options.UseTopMargin then
    Exit;
  ACommand := TdxDocCommandCellMarginDefault.Create;
  try
    ACommand.CellSpacing.CellBorders := [TdxDocTableCellBorder.Top];
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.CellMargins.Top, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TableIndentAction;
var
  ACommand: TdxDocCommandWidthIndent;
begin
  if not TableProperties.Options.UseTableIndent then
    Exit;
  ACommand := TdxDocCommandWidthIndent.Create;
  try
    ACommand.WidthUnit.ConvertFromWidthUnitInfo(TableProperties.Info.TableIndent, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TableAlignmentAction;
var
  ACommand: TdxDocCommandTableAlignment;
begin
  if not TableProperties.Options.UseTableAlignment then
    Exit;
  ACommand := TdxDocCommandTableAlignment.Create;
  try
    ACommand.TableAlignment := TableProperties.Info.GeneralSettings.TableAlignment;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.CellSpacingAction;
var
  ACommand: TdxDocCommandCellSpacing;
begin
  if (not TableProperties.Options.UseCellSpacing) or (CellSpacing = nil) then
    Exit;
  ACommand := TdxDocCommandCellSpacing.Create;
  try
    ACommand.CellSpacing.CellBorders := [TdxDocTableCellBorder.Top, TdxDocTableCellBorder.Left, TdxDocTableCellBorder.Bottom, TdxDocTableCellBorder.Right];
    ACommand.CellSpacing.WidthUnit.ConvertFromWidthUnitInfo(CellSpacing.Info, UnitConverter);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.BackgroundColorAction;
var
  ACommand: TdxDocCommandTableBackgroundColor;
begin
  if not TableProperties.Options.UseBackgroundColor then
    Exit;
  ACommand := TdxDocCommandTableBackgroundColor.Create;
  try
    ACommand.ShadingDescriptor.BackgroundColor := TableProperties.Info.GeneralSettings.BackgroundColor;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.AllowOverlapAction;
var
  ACommand: TdxDocCommandTableOverlap;
begin
  if not TableProperties.Options.UseIsTableOverlap then
    Exit;
  ACommand := TdxDocCommandTableOverlap.Create;
  try
    ACommand.Value := not TableProperties.Info.GeneralSettings.IsTableOverlap;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.FloatingPositionAction;
begin
  if not TableProperties.Options.UseFloatingPosition then
    Exit;
  if FloatingPosition.TextWrapping = TdxTextWrapping.Never then
    Exit;
  BottomFromTextAction;
  LeftFromTextAction;
  RightFromTextAction;
  TopFromTextAction;
  HorizontalAlignAction;
  VerticalAlignAction;
  TableStyleRowBandSizeAction;
  TableStyleColBandSizeAction;
  TableAnchorsAction;
end;

procedure TdxDocTableActions.BottomFromTextAction;
var
  ACommand: TdxDocCommandBottomFromText;
begin
  ACommand := TdxDocCommandBottomFromText.Create;
  try
    ACommand.Value := UnitConverter.ModelUnitsToTwips(FloatingPosition.BottomFromText);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.LeftFromTextAction;
var
  ACommand: TdxDocCommandLeftFromText;
begin
  ACommand := TdxDocCommandLeftFromText.Create;
  try
    ACommand.Value := UnitConverter.ModelUnitsToTwips(FloatingPosition.LeftFromText);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.RightFromTextAction;
var
  ACommand: TdxDocCommandRightFromText;
begin
  ACommand := TdxDocCommandRightFromText.Create;
  try
    ACommand.Value := UnitConverter.ModelUnitsToTwips(FloatingPosition.RightFromText);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TopFromTextAction;
var
  ACommand: TdxDocCommandTopFromText;
begin
  ACommand := TdxDocCommandTopFromText.Create;
  try
    ACommand.Value := UnitConverter.ModelUnitsToTwips(FloatingPosition.TopFromText);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.HorizontalAlignAction;
var
  ACommand: TdxDocCommandTableHorizontalPosition;
begin
  ACommand := TdxDocCommandTableHorizontalPosition.Create;
  try
    ACommand.Value := CalcTableHorizontalPositionTypeCode;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

function TdxDocTableActions.CalcTableHorizontalPositionTypeCode: Integer;
begin
  case FloatingPosition.HorizontalAlign of
    TdxHorizontalAlignMode.Center:
      Result := TdxDocCommandTableHorizontalPosition.Centered;
    TdxHorizontalAlignMode.Inside:
      Result := TdxDocCommandTableHorizontalPosition.Inside;
    TdxHorizontalAlignMode.Left:
      Result := TdxDocCommandTableHorizontalPosition.LeftAligned;
    TdxHorizontalAlignMode.Outside:
      Result := TdxDocCommandTableHorizontalPosition.Outside;
    TdxHorizontalAlignMode.Right:
      Result := TdxDocCommandTableHorizontalPosition.RightAligned;
    else
      Result := UnitConverter.ModelUnitsToTwips(FloatingPosition.TableHorizontalPosition);
  end;
end;

procedure TdxDocTableActions.VerticalAlignAction;
var
  ACommand: TdxDocCommandTableVerticalPosition;
begin
  ACommand := TdxDocCommandTableVerticalPosition.Create;
  try
    ACommand.Value := CalcTableVerticalPositionTypeCode;
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

function TdxDocTableActions.CalcTableVerticalPositionTypeCode: Integer;
begin
  case FloatingPosition.VerticalAlign of
    TdxVerticalAlignMode.Bottom:
      Result := TdxDocCommandTableVerticalPosition.Bottom;
    TdxVerticalAlignMode.Center:
      Result := TdxDocCommandTableVerticalPosition.Center;
    TdxVerticalAlignMode.Inline:
      Result := TdxDocCommandTableVerticalPosition.Inline;
    TdxVerticalAlignMode.Inside:
      Result := TdxDocCommandTableVerticalPosition.Inside;
    TdxVerticalAlignMode.Outside:
      Result := TdxDocCommandTableVerticalPosition.Outside;
    TdxVerticalAlignMode.Top:
      Result := TdxDocCommandTableVerticalPosition.Top;
    else
      Result := UnitConverter.ModelUnitsToTwips(FloatingPosition.TableVerticalPosition);
  end;
end;

procedure TdxDocTableActions.TableStyleRowBandSizeAction;
var
  ACommand: TdxDocCommandTableStyleRowBandSize;
begin
  if not TableProperties.Options.UseTableStyleRowBandSize then
    Exit;
  ACommand := TdxDocCommandTableStyleRowBandSize.Create;
  try
    ACommand.Value := Byte(TableProperties.Info.GeneralSettings.TableStyleRowBandSize);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TableStyleColBandSizeAction;
var
  ACommand: TdxDocCommandTableStyleColBandSize;
begin
  if not TableProperties.Options.UseTableStyleColBandSize then
    Exit;
  ACommand := TdxDocCommandTableStyleColBandSize.Create;
  try
    ACommand.Value := Byte(TableProperties.Info.GeneralSettings.TableStyleColBandSize);
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

procedure TdxDocTableActions.TableAnchorsAction;
var
  ACommand: TdxDocCommandTablePosition;
begin
  ACommand := TdxDocCommandTablePosition.Create(TableProperties.Info.FloatingPosition);
  try
    ACommand.Write(FWriter);
  finally
    ACommand.Free;
  end;
end;

end.

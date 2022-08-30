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

unit dxRichEdit.DocumentModel.TableCalculator;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCoreClasses,
  dxGenerics,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.WidthsContentInfo,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Boxes.Simple,
  dxRichEdit.DocumentModel.UnitToLayoutUnitConverter,
  dxRichEdit.DocumentModel.DocumentsToLayoutDocumentsConverter,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.Tables;

type
  { TdxAutofitTableLayoutCalculator }

  TdxAutofitTableLayoutCalculator = class
  private
    procedure CompressProportionallyWidthCore(AGrid: TdxTableGrid; AItems: TdxIntegerList; ATotalItemsWidth,  ADeltaTableWidth: TdxLayoutUnit);
  public
    procedure CompressTableGrid(AGrid: TdxTableGrid; AStartIndex, AEndIndex: Integer; ANewTableWidth: TdxLayoutUnit);
    procedure EnlargeProportionallyAverageWidth(AGrid: TdxTableGrid; AStartIndex, AEndIndex: Integer; ANewWidth: TdxLayoutUnit);
    procedure ChangeColumnsProportionally(AGrid: TdxTableGrid; AStartIndex, AEndIndex: Integer; AInitialWidth, ANewWidth: TdxLayoutUnit);
  end;

  { TdxCellPosition }

  TdxCellPosition = record
  strict private
    FFirstColumnIndex: Integer;
    FColumnSpan: Integer;
    function GetEndColumnIndex: Integer;
  public
    constructor Create(AFirstColumnIndex, AColumnSpan: Integer);

    property FirstColumnIndex: Integer read FFirstColumnIndex;
    property ColumnSpan: Integer read FColumnSpan;
    property EndColumnIndex: Integer read GetEndColumnIndex;
  end;

  { TdxColumnWidthInfoDictionary }

  TdxColumnWidthInfoDictionary = class(TcxIUnknownObject, IComparer<TPair<TdxCellPosition, TdxWidthsContentInfo>>)
  strict private
    FContainsCellWithSpan: Boolean;
    FInnerDictionary: TDictionary<TdxCellPosition, TdxWidthsContentInfo>;
  protected
    function Compare(const APair1, APair2: TPair<TdxCellPosition, TdxWidthsContentInfo>): Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Register(AFirstColumnIndex: Integer; AColumnSpan: Integer; const ACellWidths: TdxWidthsInfo);
    function CalculateTotalContentWidth: TdxWidthsContentInfo;
    function CellPositionComparer(const APosition1, APosition2: TdxCellPosition): Integer;
  end;

  { TdxTableWidthsCalculatorBase }

  TdxTableWidthsCalculatorBase = class abstract
  private
    FConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FPercentBaseWidth: Integer;
    function GetLeftBorder(ACell: TdxTableCell): TdxBorderInfo;
    function GetRightBorder(ACell: TdxTableCell): TdxBorderInfo;
  protected
    function GetPercentBaseWidth: Integer; virtual;
    function CalculateTableWidths(ATable: TdxTable; APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsContentInfo;
    procedure CalculateRowWidths(AColumnWidths: TdxColumnWidthInfoDictionary; ARow: TdxTableRow;
      APercentBaseWidth: Integer; ASimpleView: Boolean);
    function GetActualWidth(AUnitInfo: TdxWidthUnitInfo; APercentBaseWidth: Integer): TdxLayoutUnit;
    function GetMaxWidth(const AContentWidths: TdxWidthsContentInfo;
      AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit; virtual;
    function GetMinWidth(const AContentWidths: TdxWidthsContentInfo; AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit; virtual;
    function CalculateCellContentWidthsCore(ACell: TdxTableCell; APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsContentInfo; virtual;
    function CalculateParagraphWidths(AParagraph: TdxParagraph): TdxWidthsContentInfo; virtual;

    property Converter: TdxDocumentModelUnitToLayoutUnitConverter read FConverter;
    property PercentBaseWidth: Integer read GetPercentBaseWidth;
  public
    constructor Create(AConverter: TdxDocumentModelUnitToLayoutUnitConverter; APercentBaseWidth: Integer);
    function CalculateCellWidths(ACell: TdxTableCell; APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsInfo;
    function CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean; virtual; abstract;
    function CreateTableLayoutInfo(ATableGrid: TdxTableGrid; AMaxTableWidth: TdxLayoutUnit;
      AAllowTablesToExtendIntoMargins, ASimpleView: Boolean; APercentBaseWidth: TdxLayoutUnit): TdxTableLayoutInfo; virtual; abstract;
  end;

  { TdxTableWidthsCalculator }

  TdxTableWidthsCalculator = class(TdxTableWidthsCalculatorBase)
  private
    FPieceTable: TdxPieceTable;
    FMeasurer: TdxBoxMeasurer;
    procedure EnsureParagraphBoxes(AParagraph: TdxParagraph);
  protected
    function CalculateParagraphWidths(AParagraph: TdxParagraph): TdxWidthsContentInfo; override;
  public
    constructor Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer; APercentBaseWidth: Integer); overload;
    function CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean; override;
    function CreateTableLayoutInfo(ATableGrid: TdxTableGrid; AMaxTableWidth: TdxLayoutUnit;
      AAllowTablesToExtendIntoMargins, ASimpleView: Boolean;
      APercentBaseWidth: TdxLayoutUnit): TdxTableLayoutInfo; override;
  end;

  { TdxTableGridCalculator }

  TdxTableGridCalculator = class
  public type
    TCompressHelper = class
    public type
      TMatrixItemType = (Clear, White, Black);
      TMatrixItemDynArray = array of TMatrixItemType;
    private
      FTable: TdxTable;
      FResult: TdxTableGrid;
      FColsCount: Integer;
      FRowsCount: Integer;
      FMatrix: array of TMatrixItemDynArray;
      FSpans: array of array of Integer;
      procedure InitMatrices;
      procedure MarkColumn(ACol: Integer);
    public
      constructor Create(ATable: TdxTable; AResult: TdxTableGrid);
      destructor Destroy; override;
      function Work: TIntegerDynArray;
    end;
  private
    FConverter: TdxDocumentModelUnitToLayoutUnitConverter;
    FMaxTableWidth: Integer;
    FTableWidthsCalculator: TdxTableWidthsCalculatorBase;
    FAllowTablesToExtendIntoMargins: Boolean;
    FSimpleView: Boolean;
    FFlex: Boolean;
    procedure ApplyPercentWidth(AIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer);
    function CalculateEstimatedTableWidth(ATable: TdxTable; AGridIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxLayoutUnit;
    procedure EnlargeColumnsMinWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: Integer);
    procedure EnlargeColumnsMaxWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: Integer);
    procedure ApplyCellContentWidthWithoutSpan(AGrid: TdxTableGrid; ACell: TdxTableCell; AColumnIndex, APercentBaseWidth: Integer);
    procedure EnlargeColumnsWidths(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer; AOldMinWidth, ANewMinWidth: TdxLayoutUnit);
    function GetTotalColumnsMinWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    function GetTotalGridWidth(AGrid: TdxTableGrid): TdxLayoutUnit;
    function GetTotalWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    function GetTotalMinWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    function GetTotalHorizontalMarginsCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    function GetTotalMaxWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    function HasColumnsWithoutPreferredWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): Boolean;
    function GetTotalPreferredWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
    procedure ProcessIntervals(ACurrentRowIterator, ANextRowIterator: TdxTableGridIntervalIterator; AResult: TdxTableGridIntervalList);
    procedure ProcessDependedIntervals(const AMasterRowIterator, ASlaveRowIterator: TdxTableGridIntervalIterator; AResult: TdxTableGridIntervalList);
    function CalculateNewInterval(const ACurrentRowInterval, ANextRowInterval: TdxTableGridInterval): TdxTableGridInterval;
    function CalculateNewIntervalFromMixedIntervals(const APercentWidthInterval, AValueWidthInterval: TdxTableGridInterval; AColumnSpan: Integer): TdxTableGridInterval;
    procedure CopyRestIntervals(ASource: TdxTableGridIntervalIterator; ACollection: TdxTableGridIntervalList);
    function GetActualWidth(AUnitInfo: TdxWidthUnitInfo; APercentBaseWidth: Integer): TdxLayoutUnit;
    procedure ApplyCellContentWidthWithSpan(AGrid: TdxTableGrid; ACell: TdxTableCell; AStartColumnIndex, APercentBaseWidth: Integer);
    procedure CompressTableGridToColumnWidth(AGrid: TdxTableGrid; AOldWidth, ANewWidth: TdxLayoutUnit);
    procedure CompressTableGridToPreferredWidth(AGrid: TdxTableGrid; AOldWidth, ANewWidth: TdxLayoutUnit);
    procedure EnlargeTableGridToPreferredWidth(AGrid: TdxTableGrid; AOldWidth, ANewWidth: TdxLayoutUnit);
    procedure AutofitTable(AGrid: TdxTableGrid; ATable: TdxTable; APercentBaseWidth: Integer);
    procedure ApplyCellContentWidth(AGrid: TdxTableGrid; ATable: TdxTable; APercentBaseWidth: Integer);
    procedure CompressRelativelySizedTable(AGrid: TdxTableGrid; ATable: TdxTable);
  protected
    function CalculateCellWidthsInfo(ACell: TdxTableCell; APercentBaseWidth: Integer): TdxWidthsInfo; virtual;
    procedure EnlargeColumnsHorizontalMargins(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: TdxLayoutUnit); virtual;
    function CreateTableGridInterval(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AWidth: TdxWidthUnit; AColumnSpan: Integer): TdxTableGridInterval; virtual;
    function CreateIntervals(ARow: TdxTableRow): TdxTableGridIntervalList; virtual;
    procedure ConvertToIntervals(ACells: TdxTableCellCollection; AIntervals: TdxTableGridIntervalList; AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter); virtual;
    function CalculateTableGridCore(ACurrentRow, ANextRow: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxTableGridIntervalList; virtual;
    function CalculateTableGridPartially(ACurrentRow, ANextRow: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxTableGridIntervalList; virtual;
    function CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo; APercentBaseWidth: TdxLayoutUnit): Boolean; virtual;
    procedure ReplaceCachedTableLayoutInfoIfNeeded(ATable: TdxTable; ATableGrid: TdxTableGrid; APercentBaseWidth: TdxLayoutUnit); virtual;
  public
    constructor Create(ATableWidthsCalculator: TdxTableWidthsCalculatorBase;
      AMaxTableWidth: Integer; AAllowTablesToExtendIntoMargins: Boolean = False; ASimpleView: Boolean = False);
    function CalculateTableGrid(ATable: TdxTable; APercentBaseWidth: Integer): TdxTableGrid;
    function CalculateGridIntervals(ATable: TdxTable; APercentBaseWidth: Integer): TdxTableGridIntervalList;

    property AllowTablesToExtendIntoMargins: Boolean read FAllowTablesToExtendIntoMargins;
    property SimpleView: Boolean read FSimpleView;
  end;

  { TdxTableColumnWidthCalculator }

  TdxCustomTableColumnWidthCalculator = class abstract
  public
    function GetTableGridColumns: TdxTableGridColumnCollection; virtual; abstract;
  end;

  { TdxTableWidthsContainer }

  TdxCellWidths = class(TDictionary<TdxTableCell, Integer>);
  TdxGridBeforeWidths = class(TDictionary<TdxTableRow, Integer>);

  TdxTableWidthsContainer = record
  strict private
    FCellWidths: TdxCellWidths;
    FGridBeforeWidths: TdxGridBeforeWidths;
  public
    constructor Create(ACellWidths: TdxCellWidths; AGridBeforeWidths: TdxGridBeforeWidths);
    procedure Clear;

    property CellWidths: TdxCellWidths read FCellWidths;
    property GridBeforeWidths: TdxGridBeforeWidths read FGridBeforeWidths;
  end;

  { TdxTableColumnWidthCalculator }

  TdxTableColumnWidthCalculator = class(TdxCustomTableColumnWidthCalculator)
  strict private
    FTable: TdxTable;
    FServer: IdxInnerRichEditDocumentServerOwner;
    function GetDocumentModel: TdxDocumentModel;
  public
    constructor Create(ATable: TdxTable; const AServer: IdxInnerRichEditDocumentServerOwner);
    function GetTableGridColumns: TdxTableGridColumnCollection; override;
    function CalculateColumnWidth: Integer;
    function GetTableGrid(ATable: TdxTable; APercentWidthBase: Integer; AMeasurer: TdxBoxMeasurer): TdxTableGrid;
    function CalculateWidths: TdxTableWidthsContainer;
    function GetColumnsWidth(AGridColumns: TdxTableGridColumnCollection; AFirstIndex: Integer; ALastIndex: Integer): Integer;

    property Table: TdxTable read FTable;
    property DocumentModel: TdxDocumentModel read GetDocumentModel;
  end;

  { TdxTableColumnKnownWidthCalculator }

  TdxTableColumnKnownWidthCalculator = class(TdxCustomTableColumnWidthCalculator)
  strict private
    FTable: TdxTable;
    FCellWidths: TdxCellWidths;
    FGridBeforeWidths: TdxGridBeforeWidths;
  public
    constructor Create(ATable: TdxTable; const AContainer: TdxTableWidthsContainer);
    function GetTableGridColumns: TdxTableGridColumnCollection; override;
    function CalculateWidths: TdxIntegerList;
    function CreateColumnCollection(AListWidths: TdxIntegerList): TdxTableGridColumnCollection;
  end;

  { TdxRtfTableWidthsCalculator }

  TdxRtfTableWidthsCalculator = class(TdxTableWidthsCalculatorBase)
  public const
    DefaultPreferredWidthInTwips = 120;
    DefaultPercentBaseWidthInTwips = 5 * 1440;
  protected
    function CalculateCellContentWidthsCore(ACell: TdxTableCell;
      APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsContentInfo; override;
    function CalculateParagraphWidths(AParagraph: TdxParagraph): TdxWidthsContentInfo; override;
  public
    constructor Create(AConverter: TdxDocumentModelUnitToLayoutUnitConverter); overload;
    function CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean; override;
    function CreateTableLayoutInfo(ATableGrid: TdxTableGrid;
      AMaxTableWidth: Integer; AAllowTablesToExtendIntoMargins: Boolean;
      ASimpleView: Boolean; APercentBaseWidth: Integer): TdxTableLayoutInfo; override;
  end;

  { TdxJoinTableWidthsCalculator }

  TdxJoinTableWidthsCalculator = class(TdxRtfTableWidthsCalculator)
  protected
    function GetMaxWidth(const AContentWidths: TdxWidthsContentInfo;
      AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit; override;
    function GetMinWidth(const AContentWidths: TdxWidthsContentInfo;
      AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit; override;
  end;

implementation

uses
  Classes, Contnrs, RTLConsts, Math,
  dxTypeHelpers,
  dxRichEdit.Utils.BatchUpdateHelper,
  dxRichEdit.LayoutEngine.Formatter,
  dxRichEdit.DocumentModel.VisibleTextFilter.Core,
  dxRichEdit.DocumentModel.TwipsToLayoutDocumentsConverter,
  dxRichEdit.DocumentModel.FloatingObjectRange,
  dxRichEdit.DocumentModel.Section,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.FloatingObjectFormatting,
  dxRichEdit.DocumentModel.TableFormatting;

{ TdxAutofitTableLayoutCalculator }

procedure TdxAutofitTableLayoutCalculator.CompressTableGrid(AGrid: TdxTableGrid;
  AStartIndex, AEndIndex: Integer; ANewTableWidth: TdxLayoutUnit);
var
  ADeltas: TdxIntegerList;
  ADeltasTotalWidth, AInitialTableWidth, ADelta, ADeltaTableWidth: TdxLayoutUnit;
  AColumn: TdxTableGridColumn;
  I: Integer;
begin
  ADeltas := TdxIntegerList.Create;
  try
    ADeltasTotalWidth := 0;
    AInitialTableWidth := 0;
    for I := AStartIndex to AEndIndex do
    begin
      Inc(AInitialTableWidth, AGrid[I].Width);
      ADelta := Max(AGrid[I].Width - AGrid[I].MinWidth, 0);
      ADeltas.Add(ADelta);
      Inc(ADeltasTotalWidth, ADelta);
    end;

    ADeltaTableWidth := AInitialTableWidth - ANewTableWidth;
    if ADeltasTotalWidth > ADeltaTableWidth then
      CompressProportionallyWidthCore(AGrid, ADeltas, ADeltasTotalWidth, ADeltaTableWidth)
    else
    begin
      for I := AStartIndex to AEndIndex do
      begin
        AColumn := AGrid.Columns[I];
        AColumn.Width := AColumn.Width - ADeltas[I];
      end;
      ChangeColumnsProportionally(AGrid, AStartIndex, AEndIndex,
        AInitialTableWidth - ADeltasTotalWidth, ANewTableWidth);
    end;
  finally
    ADeltas.Free;
  end;
end;

procedure TdxAutofitTableLayoutCalculator.EnlargeProportionallyAverageWidth(AGrid: TdxTableGrid;
  AStartIndex, AEndIndex: Integer; ANewWidth: TdxLayoutUnit);
var
  AAverageWidths: TdxIntegerList;
  ATotalWidth, AAverageWidth, ARest, AWidth: TdxLayoutUnit;
  I: Integer;
begin
  AAverageWidths := TdxIntegerList.Create;
  try
    ATotalWidth := 0;
    for I := AStartIndex to AEndIndex do
    begin
      AAverageWidth := AGrid[I].MaxWidth + AGrid[I].MinWidth;
      AAverageWidths.Add(AAverageWidth);
      Inc(ATotalWidth, AAverageWidth);
    end;
    ARest := ANewWidth;
    for I := AStartIndex to AEndIndex do
    begin
      if I <> AEndIndex then
        AWidth := AAverageWidths[I] * ANewWidth div ATotalWidth
      else
        AWidth := ARest;
      if AWidth > AGrid[I].MaxWidth then
      begin
        AGrid[I].Width := AWidth;
        AGrid[I].MinWidth := AWidth;
        AGrid[I].MaxWidth := AWidth;
      end
      else
        AGrid[I].MinWidth := AWidth;
      Dec(ARest, AWidth);
    end;
  finally
    AAverageWidths.Free;
  end;
end;

procedure TdxAutofitTableLayoutCalculator.ChangeColumnsProportionally(AGrid: TdxTableGrid; AStartIndex, AEndIndex: Integer;
  AInitialWidth, ANewWidth: TdxLayoutUnit);
var
  ADeltaTableWidth, ARest, ADelta: TdxLayoutUnit;
  I: Integer;
begin
  ADeltaTableWidth := Abs(ANewWidth - AInitialWidth);
  ARest := ADeltaTableWidth;
  for I := AStartIndex to AEndIndex do
  begin
    if I <> AEndIndex then
      ADelta := AGrid[I].Width * ADeltaTableWidth div AInitialWidth
    else
      ADelta := ARest;
    if AInitialWidth > ANewWidth then
      AGrid[I].Width := Max(1, AGrid[I].Width - ADelta)
    else
      AGrid[I].Width := AGrid[I].Width + ADelta;
    Dec(ARest, ADelta);
  end;
end;

procedure TdxAutofitTableLayoutCalculator.CompressProportionallyWidthCore(AGrid: TdxTableGrid; AItems: TdxIntegerList; ATotalItemsWidth,  ADeltaTableWidth: TdxLayoutUnit);
var
  I, AColCount: Integer;
  ARest, ADelta: TdxLayoutUnit;
begin
  AColCount := AGrid.Columns.Count;
  if ATotalItemsWidth = 0 then
  begin
    ARest := ADeltaTableWidth;
    ATotalItemsWidth := AColCount;
    for I := 0 to AColCount - 1 do
    begin
      if I <> AColCount - 1 then
        ADelta := ADeltaTableWidth div ATotalItemsWidth
      else
        ADelta := ARest;
      AGrid.Columns[I].Width := Max(AGrid.Columns[I].Width - ADelta, 0);
      Dec(ARest, ADelta);
    end;
    Exit;
  end;

  for I := 0 to AColCount - 1 do
  begin
    ADelta := AItems[I] * ADeltaTableWidth div ATotalItemsWidth;
    AGrid.Columns[I].Width := AGrid.Columns[I].Width - ADelta;
    Dec(ADeltaTableWidth, ADelta);
    Dec(ATotalItemsWidth, AItems[I]);
    if ATotalItemsWidth = 0 then
    begin
      Assert(ADeltaTableWidth = 0);
      Break;
    end;
  end;
end;

{ TdxCellPosition }

constructor TdxCellPosition.Create(AFirstColumnIndex, AColumnSpan: Integer);
begin
  FFirstColumnIndex := AFirstColumnIndex;
  FColumnSpan := AColumnSpan;
end;

function TdxCellPosition.GetEndColumnIndex: Integer;
begin
  Result := FFirstColumnIndex + FColumnSpan - 1;
end;

{ TdxColumnWidthInfoDictionary }

constructor TdxColumnWidthInfoDictionary.Create;
begin
  inherited Create;
  FInnerDictionary := TDictionary<TdxCellPosition, TdxWidthsContentInfo>.Create;
end;

destructor TdxColumnWidthInfoDictionary.Destroy;
begin
  FreeAndNil(FInnerDictionary);
  inherited Destroy;
end;

procedure TdxColumnWidthInfoDictionary.Register(AFirstColumnIndex: Integer; AColumnSpan: Integer; const ACellWidths: TdxWidthsInfo);
var
  APosition: TdxCellPosition;
  AExistingInfo, AContentWidths: TdxWidthsContentInfo;
begin
  APosition := TdxCellPosition.Create(AFirstColumnIndex, AColumnSpan);
  if FInnerDictionary.TryGetValue(APosition, AExistingInfo) then
  begin
    AContentWidths := TdxWidthsContentInfo.Create(Math.Max(ACellWidths.MinWidth, AExistingInfo.MinWidth), Math.Max(ACellWidths.MaxWidth, AExistingInfo.MaxWidth));
    FInnerDictionary[APosition] := AContentWidths;
  end
  else
    FInnerDictionary.Add(APosition, TdxWidthsContentInfo.Create(ACellWidths.MinWidth, ACellWidths.MaxWidth));
  FContainsCellWithSpan := FContainsCellWithSpan or (AColumnSpan > 1);
end;

function TdxColumnWidthInfoDictionary.CalculateTotalContentWidth: TdxWidthsContentInfo;
var
  ASummaryWidths: TArray<TdxWidthsContentInfo>;
  APair: TPair<TdxCellPosition, TdxWidthsContentInfo>;
  APositions: TList<TPair<TdxCellPosition, TdxWidthsContentInfo>>;
  AMaxColumnIndex, APrevColumnIndex, AEndColumnIndex, ALastIndex, I: Integer;
  AContentInfo, AExistingStartSummaryInfo, ANewSumaryInfo, AExistingEndSummaryInfo: TdxWidthsContentInfo;
begin
  APositions := TList<TPair<TdxCellPosition, TdxWidthsContentInfo>>.Create(FInnerDictionary);
  try
    APositions.Sort(Self);
    AMaxColumnIndex := APositions[APositions.Count - 1].Key.EndColumnIndex;
    SetLength(ASummaryWidths, AMaxColumnIndex + 2);
    for APair in APositions do
    begin
      APrevColumnIndex := APair.Key.FirstColumnIndex;
      AEndColumnIndex := APair.Key.EndColumnIndex + 1;
      AContentInfo := APair.Value;
      AExistingStartSummaryInfo := ASummaryWidths[APrevColumnIndex];
      ANewSumaryInfo := TdxWidthsContentInfo.Create(AExistingStartSummaryInfo.MinWidth + AContentInfo.MinWidth,
        AExistingStartSummaryInfo.MaxWidth + AContentInfo.MaxWidth);

      if FContainsCellWithSpan then
        ALastIndex := Length(ASummaryWidths) - 1
      else
        ALastIndex := AEndColumnIndex;
      for I := AEndColumnIndex to ALastIndex do
      begin
        AExistingEndSummaryInfo := ASummaryWidths[I];
        if (ANewSumaryInfo.MinWidth <= AExistingEndSummaryInfo.MinWidth) and
          (ANewSumaryInfo.MaxWidth <= AExistingEndSummaryInfo.MaxWidth) then
          Break;
        ASummaryWidths[I] := TdxWidthsContentInfo.Create(Max(AExistingEndSummaryInfo.MinWidth, ANewSumaryInfo.MinWidth),
          Max(AExistingEndSummaryInfo.MaxWidth, ANewSumaryInfo.MaxWidth));
      end;
    end;
    Result := ASummaryWidths[Length(ASummaryWidths) - 1];
  finally
    APositions.Free;
  end;
end;

function TdxColumnWidthInfoDictionary.CellPositionComparer(const APosition1, APosition2: TdxCellPosition): Integer;
begin
  Result := APosition1.EndColumnIndex - APosition2.EndColumnIndex;
  if Result = 0 then
    Result := APosition1.ColumnSpan - APosition2.ColumnSpan
end;

function TdxColumnWidthInfoDictionary.Compare(const APair1,
  APair2: TPair<TdxCellPosition, TdxWidthsContentInfo>): Integer;
begin
  Result := CellPositionComparer(APair1.Key, APair2.Key);
end;

{ TdxTableWidthsCalculatorBase }

constructor TdxTableWidthsCalculatorBase.Create(AConverter: TdxDocumentModelUnitToLayoutUnitConverter; APercentBaseWidth: Integer);
begin
  inherited Create;
  Assert(AConverter <> nil);
  FConverter := AConverter;
  FPercentBaseWidth := APercentBaseWidth;
end;

function TdxTableWidthsCalculatorBase.CalculateCellWidths(ACell: TdxTableCell; APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsInfo;
var
  ACellPreferredWidth: TdxPreferredWidth;
  APreferredWidth, AResultMinWidth, AResultMaxWidth, AOuterWidth, AWidth: TdxLayoutUnit;
  AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit;
  ALeftBorder, ARightBorder: TdxBorderInfo;
  ACalculator: TdxTableBorderCalculator;
  AContentWidths, AWidthsInfoResult: TdxWidthsContentInfo;
  AMaxWidth: Integer;
  AUnit: TdxWidthUnit;
begin
  if ACell.VerticalMerging = TdxMergingState.Continue then
    ACell := ACell.Table.GetFirstCellInVerticalMergingGroup(ACell);

  ACellPreferredWidth := ACell.PreferredWidth;
  APreferredWidth := GetActualWidth(ACellPreferredWidth.Info, APercentBaseWidth);

  AHorizontalMargins := ACell.GetActualLeftMargin.Value + ACell.GetActualRightMargin.Value;
  ALeftBorder := GetLeftBorder(ACell);
  ARightBorder := GetRightBorder(ACell);
  ACalculator := TdxTableBorderCalculator.Create;
  try
    ABordersWidth := ACalculator.GetActualWidth(ALeftBorder) + ACalculator.GetActualWidth(ARightBorder);
    ASpacing := ACell.Row.CellSpacing.Value;
    if ASpacing > 0 then
      if (ACell.Row.GridBefore = 0) and ACell.IsFirstCellInRow or
         (ACell.Row.GridAfter = 0) and ACell.IsLastCellInRow then
        ASpacing := ASpacing * 3
      else
        ASpacing := ASpacing * 2;
  finally
    ACalculator.Free;
  end;
  AContentWidths := TdxWidthsContentInfo.Create(0, 0);
  if (ACell.Table.TableLayout = TdxTableLayoutType.Autofit) or ASimpleView then
  begin
    AContentWidths := CalculateCellContentWidthsCore(ACell, APercentBaseWidth, ASimpleView);
    if ACell.NoWrap then
    begin
      AMaxWidth := Max(AContentWidths.MinWidth, AContentWidths.MaxWidth);
      AContentWidths := TdxWidthsContentInfo.Create(AMaxWidth, AMaxWidth);
    end;
    AResultMinWidth := GetMinWidth(AContentWidths, AHorizontalMargins, ABordersWidth, ASpacing);
    AResultMinWidth := Min($7FFF, AResultMinWidth);
    AUnit := ACell.PreferredWidth;
    AResultMaxWidth := GetMaxWidth(AContentWidths, AHorizontalMargins, ABordersWidth, ASpacing);
    AResultMaxWidth := Min($7FFF, AResultMaxWidth);
    ACell.LayoutProperties.ContentWidthsInfo := TdxWidthsContentInfo.Create(AResultMinWidth, AResultMaxWidth);
    if (AUnit.&Type = TdxWidthUnitType.ModelUnits) or (AUnit.&Type = TdxWidthUnitType.FiftiethsOfPercent) then
      AResultMaxWidth := Max(AResultMinWidth, APreferredWidth);
    AWidthsInfoResult := TdxWidthsContentInfo.Create(AResultMinWidth, AResultMaxWidth);
    ACell.LayoutProperties.ContainerWidthsInfo := AWidthsInfoResult;
    Result := TdxWidthsInfo.Create(AWidthsInfoResult.MinWidth, AWidthsInfoResult.MaxWidth, Converter.ToLayoutUnits(AHorizontalMargins));
  end
  else
  begin
    AOuterWidth := Converter.ToLayoutUnits(ABordersWidth + ASpacing + AHorizontalMargins);
    AWidth := Math.Max(AOuterWidth, APreferredWidth);
    ACell.LayoutProperties.ContainerWidthsInfo := TdxWidthsContentInfo.Empty;
    Result := TdxWidthsInfo.Create(AWidth, AWidth, Converter.ToLayoutUnits(AHorizontalMargins));
  end;
end;

function TdxTableWidthsCalculatorBase.GetLeftBorder(ACell: TdxTableCell): TdxBorderInfo;
var
  ACalculator: TdxTableBorderCalculator;
  ALeftCellBorder, ARightCellBorder: TdxBorderInfo;
begin
  ACalculator := TdxTableBorderCalculator.Create;
  try
    if not ACell.IsFirstCellInRow then
      ALeftCellBorder := ACell.Previous.GetActualRightCellBorder.Info
    else
      ALeftCellBorder := nil;
    ARightCellBorder := ACell.GetActualLeftCellBorder.Info;
    Result := ACalculator.GetVerticalBorderSource(ACell.Table, ALeftCellBorder, ARightCellBorder);
  finally
    ACalculator.Free;
  end;
end;

function TdxTableWidthsCalculatorBase.GetRightBorder(ACell: TdxTableCell): TdxBorderInfo;
var
  ACalculator: TdxTableBorderCalculator;
  ALeftCellBorder, ARightCellBorder: TdxBorderInfo;
begin
  ACalculator := TdxTableBorderCalculator.Create;
  try
    ALeftCellBorder := ACell.GetActualRightCellBorder.Info;
    if not ACell.IsLastCellInRow then
      ARightCellBorder := ACell.Next.GetActualLeftCellBorder.Info
    else
      ARightCellBorder := nil;
    Result := ACalculator.GetVerticalBorderSource(ACell.Table, ALeftCellBorder, ARightCellBorder);
  finally
    ACalculator.Free;
  end;
end;

function TdxTableWidthsCalculatorBase.GetPercentBaseWidth: Integer;
begin
  Result := FPercentBaseWidth;
end;

function TdxTableWidthsCalculatorBase.CalculateTableWidths(ATable: TdxTable; APercentBaseWidth: Integer;
  ASimpleView: Boolean): TdxWidthsContentInfo;
var
  I, ARowCount: Integer;
  ATableWidth: TdxLayoutUnit;
  ARows: TdxTableRowCollection;
  AColumnWidths: TdxColumnWidthInfoDictionary;
  APreferredWidth: TdxWidthUnitInfo;
begin
  AColumnWidths := TdxColumnWidthInfoDictionary.Create;
  try
    ARows := ATable.Rows;
    ARowCount := ARows.Count;
    for I := 0 to ARowCount - 1 do
    begin
      CalculateRowWidths(AColumnWidths, ARows[I], APercentBaseWidth, ASimpleView);
    end;
    Result := AColumnWidths.CalculateTotalContentWidth;
    APreferredWidth := ATable.GetActualPreferredWidth;
    if not (APreferredWidth.&Type in [TdxWidthUnitType.Nil, TdxWidthUnitType.Auto]) then
    begin
      ATableWidth := GetActualWidth(APreferredWidth, APercentBaseWidth);
      Result := TdxWidthsContentInfo.Create(Max(Result.MinWidth, ATableWidth), Max(Result.MinWidth, ATableWidth));
    end;
  finally
    AColumnWidths.Free;
  end;
end;

procedure TdxTableWidthsCalculatorBase.CalculateRowWidths(AColumnWidths: TdxColumnWidthInfoDictionary;
  ARow: TdxTableRow; APercentBaseWidth: Integer; ASimpleView: Boolean);
var
  ACell: TdxTableCell;
  ACellWidths: TdxWidthsInfo;
  ACells: TdxTableCellCollection;
  APrevValue: TdxWidthsContentInfo;
  I, ACellCount, AColumnIndex: Integer;
begin
  ACells := ARow.Cells;
  ACellCount := ACells.Count;
  AColumnIndex := ARow.GridBefore;
  APrevValue := TdxWidthsContentInfo.Create(0, 0);
  for I := 0 to ACellCount - 1 do
  begin
    ACell := ACells[I];
    ACellWidths := CalculateCellWidths(ACell, APercentBaseWidth, ASimpleView);
    AColumnWidths.Register(AColumnIndex, ACell.ColumnSpan, ACellWidths);

    Inc(AColumnIndex, ACell.ColumnSpan);
  end;
end;

function TdxTableWidthsCalculatorBase.GetActualWidth(AUnitInfo: TdxWidthUnitInfo; APercentBaseWidth: Integer): TdxLayoutUnit;
begin
  if AUnitInfo.&Type = TdxWidthUnitType.ModelUnits then
    Exit(Converter.ToLayoutUnits(AUnitInfo.Value));

  if AUnitInfo.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    Exit(AUnitInfo.Value * APercentBaseWidth div 5000);
  Result := 0;
end;

function TdxTableWidthsCalculatorBase.GetMaxWidth(const AContentWidths: TdxWidthsContentInfo;
  AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit;
begin
  Result := AContentWidths.MaxWidth + Converter.ToLayoutUnits(AHorizontalMargins + ABordersWidth + ASpacing);
end;

function TdxTableWidthsCalculatorBase.GetMinWidth(const AContentWidths: TdxWidthsContentInfo;
  AHorizontalMargins, ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit;
begin
  Result := AContentWidths.MinWidth + Converter.ToLayoutUnits(AHorizontalMargins + ABordersWidth + ASpacing);
end;

function TdxTableWidthsCalculatorBase.CalculateCellContentWidthsCore(ACell: TdxTableCell; APercentBaseWidth: Integer; ASimpleView: Boolean): TdxWidthsContentInfo;
var
  AStartParagraphIndex, AEndParagraphIndex, AParagraphIndex: TdxParagraphIndex;
  APieceTable: TdxPieceTable;
  AParagraphs: TdxParagraphCollection;
  AParagraph: TdxParagraph;
  AParagraphCell: TdxTableCell;
  AInnerTable: TdxTable;
  ATableWidths, AParagraphWidths: TdxWidthsContentInfo;
begin
  AStartParagraphIndex := ACell.StartParagraphIndex;
  AEndParagraphIndex := ACell.EndParagraphIndex;
  APieceTable := TdxPieceTable(ACell.PieceTable);
  AParagraphs := APieceTable.Paragraphs;
  Result := TdxWidthsContentInfo.Create(0, 0);
  AParagraphIndex := AStartParagraphIndex;
  while AParagraphIndex <= AEndParagraphIndex do
  begin
    AParagraph := AParagraphs[AParagraphIndex];
    AParagraphCell := AParagraph.GetCell;
    if AParagraphCell = ACell then
    begin
      AParagraphWidths := CalculateParagraphWidths(AParagraph);
      Result := TdxWidthsContentInfo.Max(AParagraphWidths, Result);
      Inc(AParagraphIndex);
    end
    else
    begin
      AInnerTable := AParagraphCell.Table;
      while AInnerTable.NestedLevel > ACell.Table.NestedLevel + 1 do
        AInnerTable := AInnerTable.ParentCell.Table;
      ATableWidths := CalculateTableWidths(AInnerTable, 0, ASimpleView);
      Result := TdxWidthsContentInfo.Max(ATableWidths, Result);
      AParagraphIndex := AInnerTable.Rows.Last.Cells.Last.EndParagraphIndex + 1;
    end;
  end;
end;

function TdxTableWidthsCalculatorBase.CalculateParagraphWidths(AParagraph: TdxParagraph): TdxWidthsContentInfo;
var
  ABoxes: TdxSimpleParagraphBoxCollection;
  AMaxWidth, AMinWidth, ALineWidth, AMaxFloatingObjectWidth: TdxLayoutUnit;
  APieceTable: TdxPieceTable;
  I, AWordWidth, AWidth: Integer;
  ABox: TdxBox;
  AAnchorBox: TdxFloatingObjectAnchorBox;
  AAnchorRun: TdxFloatingObjectAnchorRun;
  AController: TdxFloatingObjectSizeController;
begin
  ABoxes := AParagraph.BoxCollection;
  AMaxWidth := 0;
  AMinWidth := 0;
  ALineWidth := 0;

  AMaxFloatingObjectWidth := 0;
  APieceTable := AParagraph.PieceTable;
  AWordWidth := 0;
  for I := 0  to ABoxes.Count - 1 do
  begin
    ABox := ABoxes[I];
    if ABox.IsLineBreak then
    begin
      AMaxWidth := Max(ALineWidth, AMaxWidth);
      ALineWidth := 0;
    end
    else
    begin
      if ABox is TdxFloatingObjectAnchorBox then
      begin
        AAnchorBox := TdxFloatingObjectAnchorBox(ABox);
        AAnchorRun := AAnchorBox.GetFloatingObjectRun(APieceTable);
        if AAnchorRun.FloatingObjectProperties.TextWrapType <> TdxFloatingObjectTextWrapType.None then
        begin
          AController := TdxFloatingObjectSizeController.Create(APieceTable);
          try
            AController.UpdateFloatingObjectBox(AAnchorBox);
          finally
            AController.Free;
          end;
          AWidth := AAnchorBox.ShapeBounds.Width;
          AMaxFloatingObjectWidth := Max(AWidth, AMaxFloatingObjectWidth);
        end;
      end;
      Inc(ALineWidth, ABox.Bounds.Width);
    end;
    if ABox.IsLineBreak or not ABox.IsNotWhiteSpaceBox then
    begin
      AMinWidth := Max(AWordWidth, AMinWidth);
      AWordWidth := 0;
    end
    else
      Inc(AWordWidth, ABox.Bounds.Width);
  end;
  Result := TdxWidthsContentInfo.Create(Max(AMaxFloatingObjectWidth, AMinWidth), Max(AMaxFloatingObjectWidth, AMaxWidth));
end;

{ TdxTableWidthsCalculator }

constructor TdxTableWidthsCalculator.Create(APieceTable: TdxPieceTable; AMeasurer: TdxBoxMeasurer; APercentBaseWidth: Integer);
begin
  inherited Create(APieceTable.DocumentModel.ToDocumentLayoutUnitConverter, APercentBaseWidth);
  Assert(APieceTable <> nil);
  Assert(AMeasurer <> nil);
  FPieceTable := APieceTable;
  FMeasurer := AMeasurer;
end;

function TdxTableWidthsCalculator.CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo): Boolean;
begin
  Result := True;
end;

function TdxTableWidthsCalculator.CreateTableLayoutInfo(ATableGrid: TdxTableGrid; AMaxTableWidth: TdxLayoutUnit;
  AAllowTablesToExtendIntoMargins, ASimpleView: Boolean; APercentBaseWidth: TdxLayoutUnit): TdxTableLayoutInfo;
begin
  Result := TdxTableLayoutInfo.Create(ATableGrid, AMaxTableWidth, AAllowTablesToExtendIntoMargins, ASimpleView, APercentBaseWidth);
end;

function TdxTableWidthsCalculator.CalculateParagraphWidths(AParagraph: TdxParagraph): TdxWidthsContentInfo;
begin
  EnsureParagraphBoxes(AParagraph);
  Result := inherited CalculateParagraphWidths(AParagraph);
end;

procedure TdxTableWidthsCalculator.EnsureParagraphBoxes(AParagraph: TdxParagraph);
var
  AVisibleTextFilter: TdxVisibleTextFilterBase;
  ACharacterIterator: TdxParagraphCharacterIterator;
  APreFormatter: TdxParagraphCharacterFormatter;
begin
  if not AParagraph.BoxCollection.IsValid then
  begin
    AParagraph.BoxCollection.Clear;
    AVisibleTextFilter := FPieceTable.VisibleTextFilter;
    ACharacterIterator := TdxParagraphCharacterIterator.Create(AParagraph, FPieceTable, AVisibleTextFilter);
    try
      if ACharacterIterator.RunIndex <= AParagraph.LastRunIndex then
      begin
        APreFormatter := TdxParagraphCharacterFormatter.Create(FPieceTable, FMeasurer);
        try
          APreFormatter.Format(ACharacterIterator);
          ACharacterIterator := nil;
        finally
          APreFormatter.Free;
        end;
      end;
    finally
      ACharacterIterator.Free;
    end;
  end;
  AParagraph.BoxCollection.ParagraphStartRunIndex := AParagraph.FirstRunIndex;
end;

{ TdxTableGridCalculator.TCompressHelper }

constructor TdxTableGridCalculator.TCompressHelper.Create(ATable: TdxTable; AResult: TdxTableGrid);
begin
  FTable := ATable;
  FResult := AResult;
  FColsCount := AResult.Columns.Count;
  FRowsCount := ATable.Rows.Count;
  InitMatrices;
end;

destructor TdxTableGridCalculator.TCompressHelper.Destroy;
var
  I: Integer;
begin
  for I := 0 to FColsCount - 1 do
  begin
    FSpans[I] := nil;
    FMatrix[I] := nil;
  end;
  FSpans := nil;
  FMatrix := nil;
  inherited Destroy;
end;

procedure TdxTableGridCalculator.TCompressHelper.InitMatrices;
var
  I, J, P, AColSpan: Integer;
  ARow: TdxTableRow;
  ACell: TdxTableCell;
begin
  SetLength(FSpans, FColsCount);
  for I := 0 to FColsCount - 1 do
    SetLength(FSpans[I], FRowsCount);

  for I := 0 to FRowsCount - 1 do
  begin
    ARow := FTable.Rows[I];
    P := 0;
    for J := 0 to ARow.Cells.Count -1 do
    begin
      ACell := ARow.Cells[J];
      AColSpan := ACell.ColumnSpan;
      while AColSpan > 0 do
      begin
        FSpans[P][I] := AColSpan;
        Inc(P);
        Dec(AColSpan);
      end;
    end;
  end;
  SetLength(FMatrix, FColsCount);
  for I := 0 to FColsCount - 1 do
    SetLength(FMatrix[I], FRowsCount);
end;

function TdxTableGridCalculator.TCompressHelper.Work: TIntegerDynArray;
var
  I, J: Integer;
begin
  for I := 0 to FColsCount - 1 do
    for J := 0 to FRowsCount - 1 do
      if (FSpans[I][J] = 1) and ((I = 0) or (FSpans[I - 1][J] = 1)) then
      begin
        MarkColumn(I);
        Break;
      end;
  for I := 0 to FColsCount - 1 do
    for J := 0 to FRowsCount - 1 do
      if FMatrix[I][J] = TMatrixItemType.Clear then
      begin
        MarkColumn(I);
        break;
      end;

  SetLength(Result, FColsCount);
  for I := 0 to FColsCount - 1 do
    if FMatrix[I][0] = TMatrixItemType.Black then
      Result[I] := 0
    else
      Result[I] := FResult.Columns[I].TotalHorizontalMargins;
end;

procedure TdxTableGridCalculator.TCompressHelper.MarkColumn(ACol: Integer);
var
  I, J: Integer;
begin
  for I := 0 to FRowsCount - 1 do
  begin
    FMatrix[ACol][I] := TMatrixItemType.Black;
    J := 1;
    while (ACol - J >= 0) and (FSpans[ACol - J][I] = FSpans[ACol][I] + J) do
    begin
      if FMatrix[ACol][I] = TMatrixItemType.Clear then
        FMatrix[ACol][I] := TMatrixItemType.White;
      Inc(J);
    end;
    J := 1;
    while (ACol + J < FColsCount) and (FSpans[ACol + J][I] = FSpans[ACol][I] - J) do
    begin
      if FMatrix[ACol][I] = TMatrixItemType.Clear then
        FMatrix[ACol][I] := TMatrixItemType.White;
      Inc(J);
    end;
  end;
end;

{ TdxTableGridCalculator }

constructor TdxTableGridCalculator.Create(
  ATableWidthsCalculator: TdxTableWidthsCalculatorBase; AMaxTableWidth: Integer;
  AAllowTablesToExtendIntoMargins: Boolean = False; ASimpleView: Boolean = False);
begin
  inherited Create;
  FTableWidthsCalculator := ATableWidthsCalculator;
  FMaxTableWidth := AMaxTableWidth;
  FSimpleView := ASimpleView;
  if ATableWidthsCalculator <> nil then
    FConverter := ATableWidthsCalculator.Converter;
  FAllowTablesToExtendIntoMargins := AAllowTablesToExtendIntoMargins;
end;

function TdxTableGridCalculator.CalculateTableGrid(ATable: TdxTable; APercentBaseWidth: Integer): TdxTableGrid;
var
  ACachedTableLayoutInfo: TdxTableLayoutInfo;
  AGridIntervals: TdxTableGridIntervalList;
  AAutoFitCalculator: TdxAutofitTableLayoutCalculator;
  ANewWidth, ATotalDelta, AAvailableToCompress: TdxLayoutUnit;
  AMargins: TIntegerDynArray;
  AColumns: TdxTableGridColumnCollection;
  I, ACount, ADiff, ADelta: Integer;
  AColumn: TdxTableGridColumn;
  APreferredWidth: TdxWidthUnitInfo;
begin
  ACachedTableLayoutInfo := ATable.CachedTableLayoutInfo;
  if CanUseCachedTableLayoutInfo(ACachedTableLayoutInfo, APercentBaseWidth) then
    Exit(ACachedTableLayoutInfo.TableGrid);
  AGridIntervals := CalculateGridIntervals(ATable, APercentBaseWidth);
  try
    APreferredWidth := ATable.GetActualPreferredWidth;
    if APreferredWidth.&Type = TdxWidthUnitType.Auto then
      APercentBaseWidth := CalculateEstimatedTableWidth(ATable, AGridIntervals, APercentBaseWidth);
    ApplyPercentWidth(AGridIntervals, APercentBaseWidth);
    Result := TdxTableGrid.Create(AGridIntervals);
  finally
    AGridIntervals.Free;
  end;
  ApplyCellContentWidth(Result, ATable, APercentBaseWidth);

  AAutoFitCalculator := TdxAutofitTableLayoutCalculator.Create;
  try
    AutofitTable(Result, ATable, APercentBaseWidth);
    if (APreferredWidth.&Type = TdxWidthUnitType.ModelUnits) and (APreferredWidth.Value > 0) and
      (ATable.TableLayout = TdxTableLayoutType.Fixed) then
    begin
      ANewWidth := FTableWidthsCalculator.Converter.ToLayoutUnits(APreferredWidth.Value);
      AAutoFitCalculator.CompressTableGrid(Result, 0, Result.Columns.Count - 1, ANewWidth);
    end;
  finally
    AAutoFitCalculator.Free;
  end;

  with TCompressHelper.Create(ATable, Result) do
  try
    AMargins := Work;
  finally
    Free;
  end;
  AColumns := Result.Columns;
  ACount := AColumns.Count;
  ATotalDelta := 0;
  AAvailableToCompress := 0;
  for I := 0 to ACount - 1 do
  begin
    AColumn := AColumns[I];
    if AColumn.Width < AMargins[I] then
    begin
      Inc(ATotalDelta, AMargins[I] - AColumn.Width);
      AColumn.Width := AMargins[I];
    end
    else
      Inc(AAvailableToCompress, AColumn.Width - AMargins[I]);
  end;

  if ATotalDelta > 0 then
  begin
    ATotalDelta := Min(ATotalDelta, AAvailableToCompress);
    I := 0;
    while (I < ACount) and (AAvailableToCompress > 0) do
    begin
      AColumn := AColumns[I];
      if AColumn.Width > AMargins[I] then
      begin
        ADiff := AColumn.Width - AMargins[I];
        ADelta := ATotalDelta * ADiff div AAvailableToCompress;
        Dec(AAvailableToCompress, ADiff);
        Dec(ATotalDelta, ADelta);
        AColumn.Width := AColumn.Width - ADelta;
      end;
      Inc(I);
    end;
  end;
  ReplaceCachedTableLayoutInfoIfNeeded(ATable, Result, APercentBaseWidth);
end;

function TdxTableGridCalculator.CalculateGridIntervals(ATable: TdxTable; APercentBaseWidth: Integer): TdxTableGridIntervalList;
var
  I, ARowCount: Integer;
  ACurrentRow, ANextRow: TdxTableGridIntervalList;
begin
  ACurrentRow := CreateIntervals(ATable.Rows[0]);
  ARowCount := ATable.Rows.Count;
  for I := 1 to ARowCount - 1 do
  begin
    ANextRow := CreateIntervals(ATable.Rows[I]);
    ACurrentRow := CalculateTableGridCore(ACurrentRow, ANextRow, APercentBaseWidth);
  end;
  Result := ACurrentRow;
end;

procedure TdxTableGridCalculator.ApplyPercentWidth(AIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer);
var
  I, ATotalPercentWidth, ATotalUnitWidth, ACount, AMaxPercentWidth, AUnsetCount, ARestUnitWidth,
  ARestPercentWidth, APercentWidth, ANewWidth: Integer;
  AInterval: TdxTableGridInterval;
begin
  ATotalPercentWidth := 0;
  ATotalUnitWidth := 0;
  ACount := AIntervals.Count;
  AMaxPercentWidth := 5000;
  AUnsetCount := 0;
  for I := 0 to ACount - 1 do
    case AIntervals[I].IntervalType of
      TdxTableGridIntervalType.PercentBased:
        begin
          Inc(ATotalPercentWidth, AIntervals[I].Width);
          if ATotalPercentWidth > AMaxPercentWidth then
          begin
            AInterval := AIntervals[I];
            AInterval.Width := Max(0, AMaxPercentWidth - ATotalPercentWidth + AIntervals[I].Width);
            AIntervals[I] := AInterval;
          end;
        end;
      TdxTableGridIntervalType.ModelUnit:
        Inc(ATotalUnitWidth, AIntervals[I].Width);
      TdxTableGridIntervalType.NotSet:
        Inc(AUnsetCount);
    end;
  FFlex := ATotalUnitWidth = 0;
  if ATotalPercentWidth = 0 then
    Exit;
  ARestUnitWidth := Max(0, APercentBaseWidth - ATotalUnitWidth);
  if (ARestUnitWidth > 0) and (AUnsetCount > 0) then
  begin
    ARestPercentWidth := 100 * 50 - ATotalPercentWidth;
    if ARestPercentWidth > 0 then
    begin
      I := 0;
      while (I < ACount) and (AUnsetCount > 0) do
      begin
        AInterval := AIntervals[I];
        if AInterval.IntervalType <> TdxTableGridIntervalType.NotSet then
        begin
          Inc(I);
          Continue;
        end;
        AInterval.IntervalType := TdxTableGridIntervalType.PercentBased;
        APercentWidth := ARestPercentWidth div AUnsetCount;
        AInterval.Width := APercentWidth;
        AIntervals[I] := AInterval;
        Dec(AUnsetCount);
        Dec(ARestPercentWidth, APercentWidth);
        Inc(I);
      end;
      ATotalPercentWidth := 100 * 50;
    end;
  end;
  for I := 0 to ACount- 1 do
  begin
    AInterval := AIntervals[I];
    if AInterval.IntervalType <> TdxTableGridIntervalType.PercentBased then
      Continue;
    AInterval.IntervalType := TdxTableGridIntervalType.ModelUnit;
    if ATotalPercentWidth > 0 then
      ANewWidth := ARestUnitWidth * AInterval.Width div ATotalPercentWidth
    else
      ANewWidth := 0;
    Dec(ATotalPercentWidth, AIntervals[I].Width);
    AInterval.Width := Max(1, ANewWidth);
    AIntervals[I] := AInterval;
    Dec(ARestUnitWidth, ANewWidth);
  end;
end;

function TdxTableGridCalculator.CalculateEstimatedTableWidth(ATable: TdxTable; AGridIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxLayoutUnit;
var
  AColumnsInModelUnitsWidth, ATotalPercentWidth, AEstimatedTableWidth, ARestOfWidthInPercent, ARowCount, ARowIndex,
  AColumnIndex, ACellIndex, ACellCount: Integer;
  AInterval: TdxTableGridInterval;
  ARows: TdxTableRowCollection;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
  AInfo: TdxWidthsInfo;
begin
  AColumnsInModelUnitsWidth := 0;
  ATotalPercentWidth := 0;
  for AInterval in AGridIntervals do
  begin
    if AInterval.IntervalType = TdxTableGridIntervalType.PercentBased then
      Inc(ATotalPercentWidth, AInterval.Width)
    else
      Inc(AColumnsInModelUnitsWidth, AInterval.Width);
  end;

  AEstimatedTableWidth := 0;
  if (ATotalPercentWidth = 0) or (AColumnsInModelUnitsWidth > 0) then
  begin
    if ATotalPercentWidth > 0 then
    begin
      ARestOfWidthInPercent := 5000 - ATotalPercentWidth;
      if ARestOfWidthInPercent <= 0 then
        Exit(APercentBaseWidth);
      AEstimatedTableWidth := AColumnsInModelUnitsWidth * 5000 div ARestOfWidthInPercent;
    end
    else
      AEstimatedTableWidth := AColumnsInModelUnitsWidth;
    Exit(Min(AEstimatedTableWidth, APercentBaseWidth));
  end
  else
  begin
    ARows := ATable.Rows;
    ARowCount := ARows.Count;
    for ARowIndex := 0 to ARowCount - 1 do
    begin
      ARow := ARows[ARowIndex];
      ACells := ARow.Cells;
      AColumnIndex := ARow.GridBefore;
      ACellCount := ACells.Count;
      for ACellIndex := 0 to ACellCount - 1 do
      begin
        ACell := ACells[ACellIndex];
        AInterval := AGridIntervals[AColumnIndex];
        if AInterval.IntervalType = TdxTableGridIntervalType.PercentBased then
        begin
          AInfo := CalculateCellWidthsInfo(ACell, APercentBaseWidth);
          AEstimatedTableWidth := Max(AEstimatedTableWidth, AInfo.MaxWidth);
          if AInterval.Width > 0 then
            AEstimatedTableWidth := Max(AInfo.MinWidth * 5000 div AInterval.Width, AEstimatedTableWidth);
        end;
        Inc(AColumnIndex, ACell.ColumnSpan);
      end;
    end;
    Result := Min(AEstimatedTableWidth, APercentBaseWidth) * 5000 div ATotalPercentWidth;
  end;
end;

procedure TdxTableGridCalculator.EnlargeColumnsMinWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: Integer);
var
  AHasColumnsWithoutPreferredWidth: Boolean;
  I, AZeroMinWidthCount, AExistingMinWidth, AFactor, ANewMinWidth: Integer;
  ARest: TdxLayoutUnit;
  AEqualSpace: Boolean;
begin
  AHasColumnsWithoutPreferredWidth := HasColumnsWithoutPreferredWidth(AGrid, AStartColumnIndex, AEndColumnIndex);
  AZeroMinWidthCount := 0;
  AExistingMinWidth := 0;
  for I := AEndColumnIndex downto AStartColumnIndex do
    if (AGrid.Columns[I].MinWidth = 0) and (AGrid.Columns[I].MaxWidth = 0) then
      Inc(AZeroMinWidthCount)
    else
      Inc(AExistingMinWidth, AGrid.Columns[I].MinWidth);
  ARest := GetTotalMaxWidthCore(AGrid, AStartColumnIndex, AEndColumnIndex) + GetTotalMinWidthCore(AGrid, AStartColumnIndex, AEndColumnIndex);
  AEqualSpace := ARest = 0;

  if AEqualSpace or (AZeroMinWidthCount > 0) then
  begin
    ARest := AEndColumnIndex - AStartColumnIndex + 1;
    Dec(ANewWidth, AExistingMinWidth);
  end;

  for I := AEndColumnIndex downto AStartColumnIndex do
  begin
    if not AHasColumnsWithoutPreferredWidth or (AGrid.Columns[I].PreferredWidth = 0) then
    begin
      if (AZeroMinWidthCount > 0) and ((AGrid.Columns[I].MinWidth > 0) or (AGrid.Columns[I].MaxWidth > 0)) then
        Continue;
      if AEqualSpace or (AZeroMinWidthCount > 0) then
        AFactor := 1
      else
        AFactor := AGrid.Columns[I].MinWidth + AGrid.Columns[I].MaxWidth;
      ANewMinWidth := AFactor * ANewWidth div ARest;
      Dec(ARest, AFactor);
      Dec(ANewWidth, ANewMinWidth);
      AGrid.Columns[I].MinWidth := Max(ANewMinWidth, AGrid.Columns[I].MinWidth);
      AGrid.Columns[I].MaxWidth := Max(AGrid.Columns[I].MinWidth, AGrid.Columns[I].MaxWidth);
    end;
  end;
end;

procedure TdxTableGridCalculator.EnlargeColumnsMaxWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: Integer);
var
  AHasColumnsWithoutPreferredWidth: Boolean;
  ARest: TdxLayoutUnit;
  I, ANewMaxWidth: Integer;
begin
  AHasColumnsWithoutPreferredWidth := HasColumnsWithoutPreferredWidth(AGrid, AStartColumnIndex, AEndColumnIndex);
  ARest := AOldWidth;
  for I := AEndColumnIndex downto AStartColumnIndex do
  begin
    if not AHasColumnsWithoutPreferredWidth or (AGrid.Columns[I].PreferredWidth = 0) then
    begin
      if ARest <> 0 then
        ANewMaxWidth := AGrid.Columns[I].MaxWidth * ANewWidth div ARest
      else
        ANewMaxWidth := 0;
      Dec(ARest, AGrid.Columns[I].MaxWidth);
      Dec(ANewWidth, ANewMaxWidth);
      if ARest < 0 then
        ARest := 0;
      if ANewWidth < 0 then
        ANewWidth := 0;
      AGrid.Columns[I].MaxWidth := Max(1, ANewMaxWidth);
    end;
  end;
end;

procedure TdxTableGridCalculator.ApplyCellContentWidthWithoutSpan(AGrid: TdxTableGrid; ACell: TdxTableCell; AColumnIndex, APercentBaseWidth: Integer);
var
  AInfo: TdxWidthsInfo;
  ACellMinWidth, ACellMaxWidth, APreferredWidth, ATotalColumnsMinWidth: TdxLayoutUnit;
begin
  Assert(ACell.ColumnSpan = 1);
  AInfo := CalculateCellWidthsInfo(ACell, APercentBaseWidth);
  ACellMinWidth := Max(1, AInfo.MinWidth);
  ACellMaxWidth := Max(1, AInfo.MaxWidth);
  if AGrid[AColumnIndex].Width > 0 then
    ACellMaxWidth := Max(ACellMinWidth, AGrid[AColumnIndex].Width);
  APreferredWidth := GetActualWidth(ACell.PreferredWidth.Info, APercentBaseWidth);

  AGrid[AColumnIndex].MinWidth := Math.Max(ACellMinWidth, AGrid[AColumnIndex].MinWidth);
  AGrid[AColumnIndex].MaxWidth := Math.Max(ACellMaxWidth, AGrid[AColumnIndex].MaxWidth);
  AGrid[AColumnIndex].TotalHorizontalMargins := Math.Max(AGrid[AColumnIndex].TotalHorizontalMargins, AInfo.TotalHorizontalMargins);
  if (APreferredWidth <= 0) and ACell.NoWrap then
    APreferredWidth := AGrid[AColumnIndex].MaxWidth;

  if APreferredWidth > 0 then
  begin
    if not ACell.NoWrap then
      APreferredWidth := Max(APreferredWidth, ACellMinWidth);
    AGrid[AColumnIndex].PreferredWidth := Math.Max(AGrid[AColumnIndex].PreferredWidth, APreferredWidth);
    AGrid[AColumnIndex].MaxWidth := AGrid[AColumnIndex].PreferredWidth;
  end
  else
  begin
    ATotalColumnsMinWidth := GetTotalColumnsMinWidth(AGrid, AColumnIndex, AColumnIndex);
    if ACellMinWidth > ATotalColumnsMinWidth then
      EnlargeColumnsWidths(AGrid, AColumnIndex, AColumnIndex, ATotalColumnsMinWidth, ACellMinWidth);
  end;
end;

function TdxTableGridCalculator.CalculateCellWidthsInfo(ACell: TdxTableCell; APercentBaseWidth: Integer): TdxWidthsInfo;
begin
  Result := FTableWidthsCalculator.CalculateCellWidths(ACell, APercentBaseWidth, FSimpleView);
end;

procedure TdxTableGridCalculator.EnlargeColumnsWidths(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer;
  AOldMinWidth, ANewMinWidth: TdxLayoutUnit);
var
  ARest, AAvailableWidth, ATotalRestMaxWidth, ADelta, ANewMaxWidth: TdxLayoutUnit;
  AColumns: TdxTableGridColumnCollection;
  I: Integer;
begin
  ARest := ANewMinWidth - AOldMinWidth;
  AAvailableWidth := 0;
  AColumns := AGrid.Columns;
  ATotalRestMaxWidth := 0;
  for I := AStartColumnIndex to AEndColumnIndex do
  begin
    Inc(AAvailableWidth, AColumns[I].MaxWidth - AColumns[I].MinWidth);
    Inc(ATotalRestMaxWidth, AColumns[I].MaxWidth);
  end;
  if AAvailableWidth < (ANewMinWidth - AOldMinWidth) then
  begin
    for I := AStartColumnIndex to AEndColumnIndex do
    begin
      ADelta := AColumns[I].MaxWidth * ARest div ATotalRestMaxWidth;
      ANewMaxWidth := AColumns[I].MaxWidth + ADelta;
      Dec(ATotalRestMaxWidth, AColumns[I].MaxWidth);
      Dec(ARest, ADelta);
      AColumns[I].MaxWidth := ANewMaxWidth;
      AColumns[I].MinWidth := AColumns[I].MaxWidth;
    end;
  end
  else
  begin
    for I := AStartColumnIndex to AEndColumnIndex do
    begin
      ADelta := (AColumns[I].MaxWidth - AColumns[I].MinWidth) * ARest div AAvailableWidth;
      Dec(AAvailableWidth, AColumns[I].MaxWidth - AColumns[I].MinWidth);
      Dec(ARest, ADelta);
      AColumns[I].MinWidth := AColumns[I].MinWidth + ADelta;
    end;
  end;
end;

function TdxTableGridCalculator.GetTotalColumnsMinWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  I: Integer;
begin
  Result := 0;
  for I := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid.Columns[I].MinWidth);
end;

function TdxTableGridCalculator.GetTotalGridWidth(AGrid: TdxTableGrid): TdxLayoutUnit;
begin
  Result := GetTotalWidthCore(AGrid, 0, AGrid.Columns.Count - 1);
end;

function TdxTableGridCalculator.GetTotalWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  AColumnIndex: Integer;
begin
  Result := 0;
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid[AColumnIndex].Width);
end;

function TdxTableGridCalculator.GetTotalMinWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  AColumnIndex: Integer;
begin
  Result := 0;
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid[AColumnIndex].MinWidth);
end;

function TdxTableGridCalculator.GetTotalHorizontalMarginsCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  AColumnIndex: Integer;
begin
  Result := 0;
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid[AColumnIndex].TotalHorizontalMargins);
end;

function TdxTableGridCalculator.GetTotalMaxWidthCore(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  AColumnIndex: Integer;
begin
  Result := 0;
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid[AColumnIndex].MaxWidth);
end;

function TdxTableGridCalculator.HasColumnsWithoutPreferredWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): Boolean;
var
  AColumnIndex: Integer;
begin
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    if AGrid[AColumnIndex].PreferredWidth = 0 then
      Exit(True);
  Result := False;
end;

function TdxTableGridCalculator.GetTotalPreferredWidth(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex: Integer): TdxLayoutUnit;
var
  AColumnIndex: Integer;
begin
  Result := 0;
  for AColumnIndex := AStartColumnIndex to AEndColumnIndex do
    Inc(Result, AGrid[AColumnIndex].PreferredWidth);
end;

procedure TdxTableGridCalculator.EnlargeColumnsHorizontalMargins(AGrid: TdxTableGrid; AStartColumnIndex, AEndColumnIndex, AOldWidth, ANewWidth: TdxLayoutUnit);
var
  ATotalDelta: TdxLayoutUnit;
  AEqualSpace: Boolean;
  I, ATotalCount, ADelta: Integer;
  AColumn: TdxTableGridColumn;
begin
  ATotalDelta := ANewWidth - AOldWidth;
  AEqualSpace := AOldWidth = 0;
  ATotalCount := AEndColumnIndex - AStartColumnIndex + 1;
  I := AEndColumnIndex;
  while (I >= AStartColumnIndex) and (ATotalDelta > 0) do
  begin
    AColumn := AGrid.Columns[I];
    if AEqualSpace then
      ADelta := ATotalDelta div ATotalCount
    else
      ADelta := ATotalDelta * AColumn.TotalHorizontalMargins div AOldWidth;
    Dec(ATotalDelta, ADelta);
    Dec(AOldWidth, AColumn.TotalHorizontalMargins);
    AColumn.TotalHorizontalMargins := AColumn.TotalHorizontalMargins + ADelta;
    Dec(ATotalCount);
    Dec(I);
  end;
end;

function TdxTableGridCalculator.CreateTableGridInterval(AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter; AWidth: TdxWidthUnit; AColumnSpan: Integer): TdxTableGridInterval;
begin
  if AWidth.&Type = TdxWidthUnitType.ModelUnits then
    Exit(TdxTableGridInterval.Create(AUnitConverter.ToLayoutUnits(AWidth.Value), AColumnSpan, TdxTableGridIntervalType.ModelUnit));
  if AWidth.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    Exit(TdxTableGridInterval.Create(AWidth.Value, AColumnSpan, TdxTableGridIntervalType.PercentBased));
  Result := TdxTableGridInterval.Create(0, AColumnSpan, TdxTableGridIntervalType.NotSet);
end;

function TdxTableGridCalculator.CreateIntervals(ARow: TdxTableRow): TdxTableGridIntervalList;
var
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter;
begin
  Result := TdxTableGridIntervalList.Create;
  AUnitConverter := FConverter;
  if ARow.GridBefore > 0 then
    Result.Add(CreateTableGridInterval(AUnitConverter, ARow.WidthBefore, ARow.GridBefore));
  ConvertToIntervals(ARow.Cells, Result, AUnitConverter);
  if ARow.GridAfter > 0 then
    Result.Add(CreateTableGridInterval(AUnitConverter, ARow.WidthAfter, ARow.GridAfter));
end;

procedure TdxTableGridCalculator.ConvertToIntervals(ACells: TdxTableCellCollection; AIntervals: TdxTableGridIntervalList;
  AUnitConverter: TdxDocumentModelUnitToLayoutUnitConverter);
var
  I, ACellCount: Integer;
begin
  ACellCount := ACells.Count;
  for I := 0 to ACellCount - 1 do
    AIntervals.Add(CreateTableGridInterval(AUnitConverter, ACells[I].PreferredWidth, ACells[I].ColumnSpan));
end;

function TdxTableGridCalculator.CalculateTableGridCore(ACurrentRow, ANextRow: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxTableGridIntervalList;
begin
  Assert(ACurrentRow <> nil);
  Assert(ANextRow <> nil);
  Result := CalculateTableGridPartially(ACurrentRow, ANextRow, APercentBaseWidth);
end;

function TdxTableGridCalculator.CalculateTableGridPartially(ACurrentRow, ANextRow: TdxTableGridIntervalList; APercentBaseWidth: Integer): TdxTableGridIntervalList;
var
  ACurrentRowIterator, ANextRowIterator: TdxTableGridIntervalIterator;
  ACurrentRowSpan, ANextRowSpan: Integer;
begin
  Result := TdxTableGridIntervalList.Create;
  ACurrentRowIterator := TdxTableGridIntervalIterator.Create(ACurrentRow, APercentBaseWidth);
  ANextRowIterator := TdxTableGridIntervalIterator.Create(ANextRow, APercentBaseWidth);
  try
    while not ACurrentRowIterator.EndOfIntervals and not ANextRowIterator.EndOfIntervals do
    begin
      ACurrentRowSpan := ACurrentRowIterator.CurrentInterval.Value.ColumnSpan;
      ANextRowSpan := ANextRowIterator.CurrentInterval.Value.ColumnSpan;
      if ACurrentRowSpan > ANextRowSpan then
        ProcessDependedIntervals(ACurrentRowIterator, ANextRowIterator, Result)
      else
        if ACurrentRowSpan < ANextRowSpan then
          ProcessDependedIntervals(ANextRowIterator, ACurrentRowIterator, Result)
        else
          ProcessIntervals(ACurrentRowIterator, ANextRowIterator, Result);
    end;
    CopyRestIntervals(ACurrentRowIterator, Result);
    CopyRestIntervals(ANextRowIterator, Result);
  finally
    ACurrentRowIterator.Free;
    ANextRowIterator.Free;
  end;
end;

function TdxTableGridCalculator.CanUseCachedTableLayoutInfo(ATableLayoutInfo: TdxTableLayoutInfo; APercentBaseWidth: TdxLayoutUnit): Boolean;
begin
  if ATableLayoutInfo = nil then
    Result := False
  else
    Result := FTableWidthsCalculator.CanUseCachedTableLayoutInfo(ATableLayoutInfo) and ATableLayoutInfo.CanUseTableGrid(FMaxTableWidth, FAllowTablesToExtendIntoMargins, FSimpleView, APercentBaseWidth);
end;

procedure TdxTableGridCalculator.ReplaceCachedTableLayoutInfoIfNeeded(ATable: TdxTable; ATableGrid: TdxTableGrid; APercentBaseWidth: TdxLayoutUnit);
var
  ACachedInfo: TdxTableLayoutInfo;
begin
  ACachedInfo := FTableWidthsCalculator.CreateTableLayoutInfo(ATableGrid, FMaxTableWidth, FAllowTablesToExtendIntoMargins, FSimpleView, APercentBaseWidth);
  if ACachedInfo <> nil then
  begin
    ATable.CachedTableLayoutInfo.Free;
    ATable.CachedTableLayoutInfo := ACachedInfo;
  end;
end;

procedure TdxTableGridCalculator.ProcessIntervals(ACurrentRowIterator, ANextRowIterator: TdxTableGridIntervalIterator; AResult: TdxTableGridIntervalList);
var
  ACurrentRowInterval, ANextRowInterval, ANewInterval: TdxTableGridInterval;
begin
  ACurrentRowInterval := ACurrentRowIterator.CurrentInterval.Value;
  ANextRowInterval := ANextRowIterator.CurrentInterval.Value;
  ANewInterval := CalculateNewInterval(ACurrentRowInterval, ANextRowInterval);
  AResult.Add(ANewInterval);
  ANextRowIterator.Advance(ANewInterval);
  ACurrentRowIterator.Advance(ANewInterval);
end;

procedure TdxTableGridCalculator.ProcessDependedIntervals(const AMasterRowIterator,
  ASlaveRowIterator: TdxTableGridIntervalIterator; AResult: TdxTableGridIntervalList);
var
  ADeferredIntervals: TdxTableGridIntervalList;
  AAutoSizeIntervalsCount, ARestWidth, ANewWidth, I, ACount: Integer;
  ARestMasterInterval, ASlaveInterval, AInterval: TdxTableGridInterval;
  ACalculateNotSetIntervals: Boolean;
begin
  ADeferredIntervals := TdxTableGridIntervalList.Create;
  try
    AAutoSizeIntervalsCount := 0;
    ARestMasterInterval := AMasterRowIterator.CurrentInterval.Value;
    repeat
      ASlaveInterval := ASlaveRowIterator.CurrentInterval.Value;
      if ASlaveInterval.IntervalType = TdxTableGridIntervalType.NotSet then
        Inc(AAutoSizeIntervalsCount);

      ADeferredIntervals.Add(ASlaveInterval);
      ASlaveRowIterator.Advance(ASlaveInterval);
      ARestMasterInterval := TdxTableGridIntervalIterator.SubstractIntervals(ARestMasterInterval, ASlaveInterval,
        AMasterRowIterator.PercentBaseWidth, ASlaveRowIterator.PercentBaseWidth);
    until (ARestMasterInterval.ColumnSpan = 0) or
      ASlaveRowIterator.EndOfIntervals or
      (ASlaveRowIterator.CurrentInterval.Value.ColumnSpan > ARestMasterInterval.ColumnSpan);

    ACalculateNotSetIntervals := (ARestMasterInterval.IntervalType = TdxTableGridIntervalType.ModelUnit) and
      (AAutoSizeIntervalsCount > 0);
    ARestWidth := ARestMasterInterval.Width;
    if ACalculateNotSetIntervals and (ARestWidth > 0) then
      ANewWidth := ARestWidth div AAutoSizeIntervalsCount
    else
      ANewWidth := 0;

    ACount := ADeferredIntervals.Count;
    for I := 0 to ACount - 1 do
    begin
      AInterval := ADeferredIntervals[I];
      if ACalculateNotSetIntervals and (AInterval.IntervalType = TdxTableGridIntervalType.NotSet) then
      begin
        AInterval.IntervalType := TdxTableGridIntervalType.ModelUnit;
        AInterval.Width := ANewWidth;
      end;
      if AMasterRowIterator.CurrentInterval.Value.ColumnSpan > 1 then
        AResult.Add(AInterval)
      else
        AResult.Add(CalculateNewInterval(AMasterRowIterator.CurrentInterval.Value, AInterval));
      AMasterRowIterator.Advance(AInterval);
    end;
  finally
    ADeferredIntervals.Free;
  end;
end;

function TdxTableGridCalculator.CalculateNewInterval(const ACurrentRowInterval, ANextRowInterval: TdxTableGridInterval): TdxTableGridInterval;
var
  ACurrentRowSpan: Integer;
  AWidth: TdxLayoutUnit;
begin
  ACurrentRowSpan := ACurrentRowInterval.ColumnSpan;
  if ACurrentRowInterval.IntervalType = ANextRowInterval.IntervalType then
  begin
    AWidth := Max(ACurrentRowInterval.Width, ANextRowInterval.Width);
    Result := TdxTableGridInterval.Create(AWidth, ACurrentRowSpan, ANextRowInterval.IntervalType);
  end
  else
  begin
    if ACurrentRowInterval.IntervalType = TdxTableGridIntervalType.PercentBased then
      Result := CalculateNewIntervalFromMixedIntervals(ACurrentRowInterval, ANextRowInterval, ACurrentRowSpan)
    else if ANextRowInterval.IntervalType = TdxTableGridIntervalType.PercentBased then
      Result := CalculateNewIntervalFromMixedIntervals(ANextRowInterval, ACurrentRowInterval, ACurrentRowSpan)
    else if ACurrentRowInterval.IntervalType = TdxTableGridIntervalType.ModelUnit then
    begin
      Assert(ANextRowInterval.IntervalType = TdxTableGridIntervalType.NotSet);
      Result := TdxTableGridInterval.Create(ACurrentRowInterval.Width, ACurrentRowSpan, TdxTableGridIntervalType.ModelUnit);
    end
    else
    begin
      Assert(ACurrentRowInterval.IntervalType = TdxTableGridIntervalType.NotSet);
      Assert(ANextRowInterval.IntervalType = TdxTableGridIntervalType.ModelUnit);
      Result := TdxTableGridInterval.Create(ANextRowInterval.Width, ACurrentRowSpan, TdxTableGridIntervalType.ModelUnit);
    end;
  end;
end;

function TdxTableGridCalculator.CalculateNewIntervalFromMixedIntervals(const APercentWidthInterval, AValueWidthInterval: TdxTableGridInterval; AColumnSpan: Integer): TdxTableGridInterval;
begin
  if AValueWidthInterval.Width > 0 then
    Result := TdxTableGridInterval.Create(AValueWidthInterval.Width, AColumnSpan, AValueWidthInterval.IntervalType)
  else
    Result := TdxTableGridInterval.Create(APercentWidthInterval.Width, AColumnSpan, APercentWidthInterval.IntervalType);
end;

procedure TdxTableGridCalculator.CopyRestIntervals(ASource: TdxTableGridIntervalIterator; ACollection: TdxTableGridIntervalList);
var
  AInterval: TdxTableGridInterval;
begin
  while not ASource.EndOfIntervals do
  begin
    AInterval := ASource.CurrentInterval.Value;
    ACollection.Add(AInterval);
    ASource.Advance(AInterval);
  end;
end;

function TdxTableGridCalculator.GetActualWidth(AUnitInfo: TdxWidthUnitInfo; APercentBaseWidth: Integer): TdxLayoutUnit;
begin
  if AUnitInfo.&Type = TdxWidthUnitType.ModelUnits then
    Exit(FConverter.ToLayoutUnits(AUnitInfo.Value));
  if AUnitInfo.&Type = TdxWidthUnitType.FiftiethsOfPercent then
    Exit(AUnitInfo.Value * APercentBaseWidth div 5000);
  Result := 0;
end;

procedure TdxTableGridCalculator.ApplyCellContentWidthWithSpan(AGrid: TdxTableGrid; ACell: TdxTableCell; AStartColumnIndex, APercentBaseWidth: Integer);
var
  AEndColumnIndex: Integer;
  AInfo: TdxWidthsInfo;
  AGridWidth, ACellMinWidth, ACellMaxWidth, APreferredWidth, AGridMinWidth, AGridMaxWidth, AGridTotalMargins: TdxLayoutUnit;
begin
  Assert(ACell.ColumnSpan > 1);
  AEndColumnIndex := AStartColumnIndex + ACell.ColumnSpan - 1;
  AInfo := CalculateCellWidthsInfo(ACell, APercentBaseWidth);
  AGridWidth := GetTotalWidthCore(AGrid, AStartColumnIndex, AEndColumnIndex);

  ACellMinWidth := Max(1, AInfo.MinWidth);
  ACellMaxWidth := Max(1, AInfo.MaxWidth);
  if AGridWidth > 0 then
    ACellMaxWidth := Max(ACellMinWidth, AGridWidth);

  APreferredWidth := GetActualWidth(ACell.PreferredWidth.Info, APercentBaseWidth);
  if APreferredWidth > 0 then
  begin
    APreferredWidth := Max(ACellMinWidth, APreferredWidth);
    ACellMaxWidth := APreferredWidth;
  end;
  AGridMinWidth := GetTotalMinWidthCore(AGrid, AStartColumnIndex, AEndColumnIndex);
  if ACellMinWidth > AGridMinWidth then
    EnlargeColumnsMinWidth(AGrid, AStartColumnIndex, AEndColumnIndex, AGridMinWidth, ACellMinWidth);

  AGridMaxWidth := GetTotalMaxWidthCore(AGrid, AStartColumnIndex, AEndColumnIndex);
  if ACellMaxWidth > AGridMaxWidth then
    EnlargeColumnsMaxWidth(AGrid, AStartColumnIndex, AEndColumnIndex, AGridMaxWidth, ACellMaxWidth);

  AGridTotalMargins := GetTotalHorizontalMarginsCore(AGrid, AStartColumnIndex, AEndColumnIndex);
  if AInfo.TotalHorizontalMargins > AGridTotalMargins then
    EnlargeColumnsHorizontalMargins(AGrid, AStartColumnIndex, AEndColumnIndex, AGridTotalMargins, AInfo.TotalHorizontalMargins);
end;

procedure TdxTableGridCalculator.CompressTableGridToColumnWidth(AGrid: TdxTableGrid; AOldWidth, ANewWidth: TdxLayoutUnit);
var
  ATotalMinWidth, ATotalWidth, ARest: TdxLayoutUnit;
  I, ANewColumnWidth: Integer;
begin
  ATotalMinWidth := GetTotalMinWidthCore(AGrid, 0, AGrid.Columns.Count - 1);

  if ATotalMinWidth <= ANewWidth then
  begin
    CompressTableGridToPreferredWidth(AGrid, AOldWidth, ANewWidth);
    Exit;
  end;
  if AllowTablesToExtendIntoMargins then
  begin
    CompressTableGridToPreferredWidth(AGrid, AOldWidth, ATotalMinWidth);
    Exit;
  end;

  ATotalWidth := ANewWidth;
  ARest := ATotalMinWidth;
  for I := 0 to AGrid.Columns.Count - 1 do
  begin
    if ARest > 0 then
      ANewColumnWidth := Max(AGrid.Columns[I].MinWidth * ATotalWidth div ARest, 1)
    else
      ANewColumnWidth := 1;
    AGrid.Columns[I].Width := ANewColumnWidth;
    Dec(ATotalWidth, ANewColumnWidth);
    Dec(ARest, AGrid.Columns[I].MinWidth);
  end;
end;

procedure TdxTableGridCalculator.CompressTableGridToPreferredWidth(AGrid: TdxTableGrid; AOldWidth,
  ANewWidth: TdxLayoutUnit);
var
  ATotalMinWidth, ATotalMaxWidth, ATotalPreferredWidth, ATotalMinWidthForNonPreferredWidthColumn,
    ATotalMaxWidthForNonPreferredWidthColumn, ATotalDelta, AMinWidth, ARest: TdxLayoutUnit;
  AColumnIndex, I, ADelta: Integer;
  AColumn: TdxTableGridColumn;
begin
  ATotalMinWidth := 0;
  ATotalMaxWidth := 0;
  ATotalPreferredWidth := 0;
  ATotalMinWidthForNonPreferredWidthColumn := 0;

  for AColumnIndex := 0 to AGrid.Columns.Count - 1 do
  begin
    AColumn := AGrid[AColumnIndex];
    Inc(ATotalMinWidth, AColumn.MinWidth);
    Inc(ATotalMaxWidth, AColumn.MaxWidth);
    Inc(ATotalPreferredWidth, AColumn.PreferredWidth);
    if AColumn.PreferredWidth = 0 then
      Inc(ATotalMinWidthForNonPreferredWidthColumn, AColumn.MinWidth);
  end;

  ATotalMaxWidthForNonPreferredWidthColumn := ATotalMaxWidth - ATotalPreferredWidth;
  if ANewWidth <= ATotalMinWidth then
  begin
    for I := 0 to AGrid.Columns.Count - 1 do
      AGrid.Columns[I].Width := AGrid.Columns[I].MinWidth;
    Exit;
  end;
  for I := 0 to AGrid.Columns.Count - 1 do
  begin
    if AGrid.Columns[I].MaxWidth < AGrid.Columns[I].MinWidth then
    begin
      AMinWidth := AGrid.Columns[I].MinWidth;
      AGrid.Columns[I].MinWidth := AGrid.Columns[I].MaxWidth;
      AGrid.Columns[I].MaxWidth := AMinWidth;
      AGrid.Columns[I].Width := AGrid.Columns[I].MaxWidth;
    end;
  end;
  if ANewWidth - ATotalMinWidthForNonPreferredWidthColumn < ATotalPreferredWidth then
  begin
    ATotalDelta := AOldWidth - ANewWidth;
    for I := 0 to AGrid.Columns.Count - 1 do
    begin
      if AGrid.Columns[I].PreferredWidth = 0 then
      begin
        Dec(ATotalDelta, AGrid.Columns[I].Width - AGrid.Columns[I].MinWidth);
        AGrid.Columns[I].Width := AGrid.Columns[I].MinWidth;
      end;
    end;

    ARest := ATotalPreferredWidth - (ATotalMinWidth - ATotalMinWidthForNonPreferredWidthColumn);
    I := AGrid.Columns.Count - 1;
    while (I >= 0) and (ATotalDelta > 0) do
    begin
      if AGrid.Columns[I].PreferredWidth > 0 then
      begin
        Assert(ARest > 0);

        ADelta := Trunc(((AGrid.Columns[I].PreferredWidth - AGrid.Columns[I].MinWidth) / ARest) * ATotalDelta);
        AGrid.Columns[I].Width := Math.Max(AGrid.Columns[I].Width - ADelta, 1);
        Dec(ARest, AGrid.Columns[I].PreferredWidth - AGrid.Columns[I].MinWidth);
        Dec(ATotalDelta, ADelta);
      end;
      Dec(I);
    end;
  end
  else
  begin
    ATotalDelta := AOldWidth - ANewWidth;
    ARest := ATotalMaxWidthForNonPreferredWidthColumn - ATotalMinWidthForNonPreferredWidthColumn;
    I := AGrid.Columns.Count - 1;
    while (I >= 0) and (ATotalDelta > 0) do
    begin
      if AGrid.Columns[I].PreferredWidth = 0 then
      begin
        Assert(ARest > 0);

        ADelta := Trunc(((AGrid.Columns[I].MaxWidth - AGrid.Columns[I].MinWidth) / ARest) * ATotalDelta);
        AGrid.Columns[I].Width := AGrid.Columns[I].Width - ADelta;
        Dec(ARest, AGrid.Columns[I].MaxWidth - AGrid.Columns[I].MinWidth);
        Dec(ATotalDelta, ADelta);
      end;
      Dec(I);
    end;
  end;
end;

procedure TdxTableGridCalculator.EnlargeTableGridToPreferredWidth(AGrid: TdxTableGrid; AOldWidth, ANewWidth: TdxLayoutUnit);
var
  ATotalMaxWidth, ATotalPreferredWidth, ATotalDelta, ARest: TdxLayoutUnit;
  AHasColumnsWithoutPreferredWidth: Boolean;
  I, ADelta: Integer;
  AColumn: TdxTableGridColumn;
begin
  ATotalMaxWidth := GetTotalMaxWidthCore(AGrid, 0, AGrid.Columns.Count - 1);
  ATotalPreferredWidth := GetTotalPreferredWidth(AGrid, 0, AGrid.Columns.Count - 1);
  AHasColumnsWithoutPreferredWidth := HasColumnsWithoutPreferredWidth(AGrid, 0, AGrid.Columns.Count - 1);
  ATotalDelta := ANewWidth - AOldWidth;
  ARest := ATotalMaxWidth;
  if AHasColumnsWithoutPreferredWidth then
    Dec(ARest, ATotalPreferredWidth);
  for I := AGrid.Columns.Count - 1 downto 0 do
  begin
    AColumn := AGrid[I];
    if not AHasColumnsWithoutPreferredWidth or (AColumn.PreferredWidth = 0) then
    begin
      ADelta := AColumn.MaxWidth * ATotalDelta div ARest;
      AColumn.Width := AColumn.Width + ADelta;
      Dec(ARest, AColumn.MaxWidth);
      Dec(ATotalDelta, ADelta);
    end;
  end;
end;

procedure TdxTableGridCalculator.AutofitTable(AGrid: TdxTableGrid; ATable: TdxTable; APercentBaseWidth: Integer);
var
  AWidth, AMinWidth, APreferredTableWidth: TdxLayoutUnit;
  APreferredWidth: TdxWidthUnitInfo;
begin
  AWidth := GetTotalGridWidth(AGrid);
  AMinWidth := GetTotalMinWidthCore(AGrid, 0, AGrid.Columns.Count - 1);
  APreferredWidth := ATable.GetActualPreferredWidth;
  if APreferredWidth.&Type in [TdxWidthUnitType.ModelUnits, TdxWidthUnitType.FiftiethsOfPercent] then
  begin
    APreferredTableWidth := GetActualWidth(APreferredWidth, APercentBaseWidth);
    if AWidth >= APreferredTableWidth then
    begin
      if (AMinWidth < FMaxTableWidth) or (APreferredTableWidth > FMaxTableWidth) then
        CompressTableGridToPreferredWidth(AGrid, AWidth, APreferredTableWidth)
      else
        CompressTableGridToColumnWidth(AGrid, AWidth, FMaxTableWidth);
    end
    else
      if AWidth < APreferredTableWidth then
        EnlargeTableGridToPreferredWidth(AGrid, AWidth, APreferredTableWidth);
  end
  else
  begin
    if (AWidth > FMaxTableWidth) and ((ATable.TableLayout = TdxTableLayoutType.Autofit) or SimpleView) then
      CompressTableGridToColumnWidth(AGrid, AWidth, FMaxTableWidth)
    else
      CompressRelativelySizedTable(AGrid, ATable);
  end;
end;

procedure TdxTableGridCalculator.ApplyCellContentWidth(AGrid: TdxTableGrid; ATable: TdxTable; APercentBaseWidth: Integer);
var
  ARows: TdxTableRowCollection;
  ARowCount, ARowIndex, AColumnCount, AColumnIndex, ACellIndex, ACellCount, I: Integer;
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACell: TdxTableCell;
  AColumns: TdxTableGridColumnCollection;
  AColumn: TdxTableGridColumn;
begin
  ARows := ATable.Rows;
  ARowCount := ARows.Count;
  for ARowIndex := 0 to ARowCount - 1 do
  begin
    ARow := ARows[ARowIndex];
    ACells := ARow.Cells;
    AColumnIndex := ARow.GridBefore;
    ACellCount := ACells.Count;
    for ACellIndex := 0 to ACellCount - 1 do
    begin
      ACell := ACells[ACellIndex];
      if ACell.ColumnSpan = 1 then
        ApplyCellContentWidthWithoutSpan(AGrid, ACell, AColumnIndex, APercentBaseWidth);
      Inc(AColumnIndex, ACell.ColumnSpan);
    end;
  end;
  AColumns := AGrid.Columns;
  AColumnCount := AColumns.Count;

  for I := 0 to AColumnCount - 1 do
  begin
    if (AGrid[I].MinWidth = 0) and (AGrid[I].MaxWidth = 0) then
    begin
      AGrid[I].MinWidth := AGrid[I].Width;
      AGrid[I].MaxWidth := AGrid[I].Width;
    end;
  end;

  for ARowIndex := 0 to ARowCount - 1 do
  begin
    ARow := ARows[ARowIndex];
    ACells := ARow.Cells;
    AColumnIndex := ARow.GridBefore;
    ACellCount := ACells.Count;
    for ACellIndex := 0 to ACellCount - 1 do
    begin
      ACell := ACells[ACellIndex];
      if ACell.ColumnSpan > 1 then
        ApplyCellContentWidthWithSpan(AGrid, ACell, AColumnIndex, APercentBaseWidth);
      Inc(AColumnIndex, ACell.ColumnSpan);
    end;
  end;

  for I := 0 to AColumnCount - 1 do
  begin
    AColumn := AColumns[I];
    AColumn.MaxWidth := Max(AColumn.MaxWidth, 1);
    AColumn.Width := Max(AColumn.MaxWidth, 1);
  end;
end;

procedure TdxTableGridCalculator.CompressRelativelySizedTable(AGrid: TdxTableGrid; ATable: TdxTable);
var
  ARows: TdxTableRowCollection;
  I, J, ARowCount, AColumnCount, AColCount: Integer;
  AColumns: TdxTableGridColumnCollection;
  ARatio, Q: Double;
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  AColumn: TdxTableGridColumn;
  APreferredWidth: TdxWidthUnitInfo;
begin
  APreferredWidth := ATable.GetActualPreferredWidth;
  if not ((APreferredWidth.&Type = TdxWidthUnitType.Auto) and (ATable.TableLayout = TdxTableLayoutType.Autofit) and FFlex) then
    Exit;
  ARows := ATable.Rows;
  ARowCount := ARows.Count;
  AColumns := AGrid.Columns;
  AColumnCount := AColumns.Count;
  ARatio := 0.0;
  for I := 0 to ARowCount - 1 do
  begin
    ARow := ARows[I];
    AColCount := ARow.Cells.Count;
    for J := 0 to AColCount - 1 do
    begin
      ACell := ARow.Cells[J];
      if ACell.VerticalMerging = TdxMergingState.Continue then
        Continue;
      if ACell.LayoutProperties.ContentWidthsInfo.MaxWidth >= ACell.LayoutProperties.ContainerWidthsInfo.MaxWidth then
        Exit;
      Q := ACell.LayoutProperties.ContentWidthsInfo.MaxWidth / ACell.LayoutProperties.ContainerWidthsInfo.MaxWidth;
      if Q > ARatio then
        ARatio := Q;
    end;
  end;

  for I := 0 to AColumnCount - 1 do
  begin
    AColumn := AColumns[I];
    AColumn.Width := Max(Trunc(AColumn.Width * ARatio), AColumn.MinWidth);
  end;
end;

{ TdxTableWidthsContainer }

constructor TdxTableWidthsContainer.Create(ACellWidths: TdxCellWidths; AGridBeforeWidths: TdxGridBeforeWidths);
begin
  FCellWidths := ACellWidths;
  FGridBeforeWidths := AGridBeforeWidths;
end;

procedure TdxTableWidthsContainer.Clear;
begin
  FreeAndNil(FCellWidths);
  FreeAndNil(FGridBeforeWidths);
end;

{ TdxTableColumnWidthCalculator }

constructor TdxTableColumnWidthCalculator.Create(ATable: TdxTable; const AServer: IdxInnerRichEditDocumentServerOwner);
begin
  inherited Create;
  FTable := ATable;
  FServer := AServer;
end;

function TdxTableColumnWidthCalculator.GetDocumentModel: TdxDocumentModel;
begin
  Result := TdxDocumentModel(Table.DocumentModel);
end;

function TdxTableColumnWidthCalculator.GetTableGridColumns: TdxTableGridColumnCollection;
var
  AGrid: TdxTableGrid;
  AColumnWidth: Integer;
  AMeasurement: TdxMeasurementAndDrawingStrategy;
begin
  AColumnWidth := CalculateColumnWidth;
  AMeasurement := FServer.CreateMeasurementAndDrawingStrategy(DocumentModel);
  try
    AMeasurement.Initialize;
    AGrid := GetTableGrid(Table, AColumnWidth, AMeasurement.Measurer);
    Result := AGrid.Columns;
  finally
    AMeasurement.Free;
  end;
end;

function TdxTableColumnWidthCalculator.CalculateColumnWidth: Integer;
var
  APosition: TdxDocumentLogPosition;
  ASectionIndex: TdxSectionIndex;
  ASection: TdxSection;
  AConverter: TdxDocumentModelTwipsToLayoutDocumentsConverter;
  APageBoundsCalculator: TdxPageBoundsCalculator;
  AColumnsBoundsCalculator: TdxColumnsBoundsCalculator;
  AWidths: TdxRectList;
begin
  APosition := DocumentModel.ActivePieceTable.Paragraphs[Table.StartParagraphIndex].LogPosition;
  ASectionIndex := DocumentModel.FindSectionIndex(APosition);
  ASection := DocumentModel.Sections[ASectionIndex];

  AConverter := TdxDocumentModelTwipsToLayoutDocumentsConverter.Create;
  try
    APageBoundsCalculator := TdxPageBoundsCalculator.Create(AConverter);
    try
      AColumnsBoundsCalculator := TdxColumnsBoundsCalculator.Create(AConverter);
      try
        AWidths := AColumnsBoundsCalculator.Calculate(ASection, APageBoundsCalculator.CalculatePageClientBounds(ASection));
        try
          Result := AWidths[0].Width;
        finally
          AWidths.Free;
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

function TdxTableColumnWidthCalculator.GetTableGrid(ATable: TdxTable; APercentWidthBase: Integer; AMeasurer: TdxBoxMeasurer): TdxTableGrid;
var
  AOldPieceTable: TdxCustomPieceTable;
  AWidthsCalculator: TdxTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AOldPieceTable := AMeasurer.PieceTable;
  try
    AMeasurer.PieceTable := ATable.PieceTable;
    AWidthsCalculator := TdxTableWidthsCalculator.Create(TdxPieceTable(ATable.PieceTable), AMeasurer, APercentWidthBase);
    try
      ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, APercentWidthBase);
      try
        Result := ACalculator.CalculateTableGrid(ATable, APercentWidthBase);
      finally
        ACalculator.Free;
      end;
    finally
      AWidthsCalculator.Free;
    end;
  finally
    AMeasurer.PieceTable := AOldPieceTable;
  end;
end;

function TdxTableColumnWidthCalculator.CalculateWidths: TdxTableWidthsContainer;
var
  ACellWidths: TdxCellWidths;
  AGridBeforeWidths: TdxGridBeforeWidths;
  AGridColumns: TdxTableGridColumnCollection;
  ARows: TdxTableRowCollection;
  ARowsCount, I, AFirstCellIndex, ACellsCount, J, ALastCellIndex: Integer;
  ACurrentRow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACurrentCell: TdxTableCell;
begin
  ACellWidths := TdxCellWidths.Create;
  AGridBeforeWidths := TdxGridBeforeWidths.Create;
  AGridColumns := GetTableGridColumns;
  ARows := FTable.Rows;
  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := ARows[I];
    AGridBeforeWidths.Add(ACurrentRow, GetColumnsWidth(AGridColumns, 0, ACurrentRow.GridBefore - 1));
    AFirstCellIndex := ACurrentRow.GridBefore;
    ACells := ACurrentRow.Cells;
    ACellsCount := ACells.Count;
    for J := 0 to ACellsCount - 1 do
    begin
      ACurrentCell := ACells[J];
      ALastCellIndex := AFirstCellIndex + ACurrentCell.ColumnSpan - 1;
      ACellWidths.Add(ACurrentCell, GetColumnsWidth(AGridColumns, AFirstCellIndex, ALastCellIndex));
      AFirstCellIndex := ALastCellIndex + 1;
    end;
  end;
  Result := TdxTableWidthsContainer.Create(ACellWidths, AGridBeforeWidths);
end;

function TdxTableColumnWidthCalculator.GetColumnsWidth(AGridColumns: TdxTableGridColumnCollection; AFirstIndex: Integer; ALastIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := AFirstIndex to ALastIndex do
    Inc(Result, AGridColumns[I].Width);
end;

{ TdxTableColumnKnownWidthCalculator }

constructor TdxTableColumnKnownWidthCalculator.Create(ATable: TdxTable; const AContainer: TdxTableWidthsContainer);
begin
  inherited Create;
  FTable := ATable;
  FCellWidths := AContainer.CellWidths;
  FGridBeforeWidths := AContainer.GridBeforeWidths;
end;

function TdxTableColumnKnownWidthCalculator.GetTableGridColumns: TdxTableGridColumnCollection;
var
  AListWidths: TdxIntegerList;
begin
  AListWidths := CalculateWidths;
  try
    Result := CreateColumnCollection(AListWidths);
  finally
    AListWidths.Free;
  end;
end;

function TdxTableColumnKnownWidthCalculator.CalculateWidths: TdxIntegerList;
var
  AWidth, ARowsCount, I, AGridBefore, ACellsCount, J: Integer;
  ARows: TdxTableRowCollection;
  ACurrentRow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ACurrentCell: TdxTableCell;
begin
  Result := TdxIntegerList.Create;
  ARows := FTable.Rows;
  ARowsCount := ARows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    AGridBefore := FGridBeforeWidths[ARows[I]];
    ACurrentRow := ARows[I];
    if not Result.Contains(AGridBefore) then
      Result.Add(AGridBefore);
    AWidth := AGridBefore;
    ACells := ACurrentRow.Cells;
    ACellsCount := ACells.Count;
    for J := 0 to ACellsCount - 1 do
    begin
      ACurrentCell := ACells[J];
      Inc(AWidth, FCellWidths[ACurrentCell]);
      if not Result.Contains(AWidth) then
        Result.Add(AWidth);
    end;
  end;
  Result.Sort;
end;

function TdxTableColumnKnownWidthCalculator.CreateColumnCollection(AListWidths: TdxIntegerList): TdxTableGridColumnCollection;
var
  ACount, I, AWidth: Integer;
begin
  Result := TdxTableGridColumnCollection.Create;
  ACount := AListWidths.Count;
  for I := 0 to ACount - 1 - 1 do
  begin
    AWidth := AListWidths[I + 1] - AListWidths[I];
    Result.Add(TdxTableGridColumn.Create(AWidth, False));
  end;
end;

{ TdxRtfTableWidthsCalculator }

constructor TdxRtfTableWidthsCalculator.Create(
  AConverter: TdxDocumentModelUnitToLayoutUnitConverter);
begin
  inherited Create(AConverter, AConverter.ToLayoutUnits(DefaultPercentBaseWidthInTwips));
end;

function TdxRtfTableWidthsCalculator.CreateTableLayoutInfo(
  ATableGrid: TdxTableGrid; AMaxTableWidth: Integer;
  AAllowTablesToExtendIntoMargins, ASimpleView: Boolean;
  APercentBaseWidth: Integer): TdxTableLayoutInfo;
begin
  Result := nil;
end;

function TdxRtfTableWidthsCalculator.CalculateCellContentWidthsCore(
  ACell: TdxTableCell; APercentBaseWidth: Integer;
  ASimpleView: Boolean): TdxWidthsContentInfo;
var
  APreferredWidth: TdxLayoutUnit;
begin
  Result := inherited CalculateCellContentWidthsCore(ACell, APercentBaseWidth, ASimpleView);

  if Result <> TdxWidthsContentInfo.Empty then
    Exit;

  APreferredWidth := 0;
  if ACell.PreferredWidth.&Type = TdxWidthUnitType.ModelUnits then
    APreferredWidth := Converter.ToLayoutUnits(ACell.PreferredWidth.Value);
  if APreferredWidth = 0 then
    APreferredWidth := Converter.ToLayoutUnits(DefaultPreferredWidthInTwips) * ACell.ColumnSpan;
  Result := TdxWidthsContentInfo.Create(APreferredWidth, APreferredWidth);
end;

function TdxRtfTableWidthsCalculator.CalculateParagraphWidths(
  AParagraph: TdxParagraph): TdxWidthsContentInfo;
begin
  if AParagraph.BoxCollection.IsValid then
    Result := inherited CalculateParagraphWidths(AParagraph)
  else
    Result := TdxWidthsContentInfo.Empty;
end;

function TdxRtfTableWidthsCalculator.CanUseCachedTableLayoutInfo(
  ATableLayoutInfo: TdxTableLayoutInfo): Boolean;
begin
  Result := False;
end;

{ TdxJoinTableWidthsCalculator }

function TdxJoinTableWidthsCalculator.GetMaxWidth(const AContentWidths: TdxWidthsContentInfo; AHorizontalMargins,
  ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit;
begin
  Result := AContentWidths.MaxWidth;
end;

function TdxJoinTableWidthsCalculator.GetMinWidth(const AContentWidths: TdxWidthsContentInfo; AHorizontalMargins,
  ABordersWidth, ASpacing: TdxModelUnit): TdxLayoutUnit;
begin
  Result := AContentWidths.MinWidth;
end;

end.

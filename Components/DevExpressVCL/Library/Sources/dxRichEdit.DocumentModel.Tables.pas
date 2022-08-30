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

unit dxRichEdit.DocumentModel.Tables;

{$I cxVer.inc}
{$I dxRichEditControl.inc}

interface

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, dxCoreGraphics,

  dxRichEdit.Platform.Font,
  dxRichEdit.Utils.Types,
  dxRichEdit.Utils.WidthsContentInfo,
  dxGenerics,
  dxRichEdit.DocumentModel.IndexBasedObject,
  dxRichEdit.DocumentModel.Core,
  dxRichEdit.DocumentModel.Tables.Core,
  dxRichEdit.DocumentModel.Borders,
  dxRichEdit.DocumentModel.CharacterFormatting,
  dxRichEdit.DocumentModel.ParagraphFormatting,
  dxRichEdit.DocumentModel.TableFormatting,
  dxRichEdit.DocumentModel.TableStyles,
  dxRichEdit.DocumentModel.Simple,
  dxRichEdit.DocumentModel.History.IndexChangedHistoryItem;

type
  TdxTableCell = class;
  TdxTableRow = class;
  TdxTable = class;
  TdxTableCellList = class;

  { TdxTableCellLayoutProperties }

  TdxTableCellLayoutProperties = class
  strict private
    FColumnSpan: Integer;
    FContainerWidthsInfo: TdxWidthsContentInfo;
    FContentWidthsInfo: TdxWidthsContentInfo;
  public
    property ColumnSpan: Integer read FColumnSpan write FColumnSpan;
    property ContainerWidthsInfo: TdxWidthsContentInfo read FContainerWidthsInfo write FContainerWidthsInfo;
    property ContentWidthsInfo: TdxWidthsContentInfo read FContentWidthsInfo write FContentWidthsInfo;
  end;

  { TdxTableBorderAccessor }

  TdxTableBorderAccessor = class abstract
  public
    function UseMask: Integer; virtual; abstract;
    function CellUseMask: Integer; virtual; abstract;
    function UseInsideBorderMask: Integer; virtual; abstract;
    function GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase; overload; virtual; abstract;
    function GetBorder(ABorders: TdxTableBorders): TdxBorderBase; overload; virtual; abstract;
    function GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase; overload; virtual; abstract;
    function GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase; overload; virtual; abstract;

    function GetTableBorder(ABorders: TdxTableBorders; AIsInside: Boolean): TdxBorderBase;
  end;

  { TdxTableCell }

  TdxTableCell = class(TdxCustomTableCell, IdxCellPropertiesOwner)
  strict private
    FStyleIndex: Integer;
    FRow: TdxTableRow;
    FProperties: TdxTableCellProperties;
    FLayoutProperties: TdxTableCellLayoutProperties;
    FConditionalType: TdxConditionalColumnType;
    FStartParagraphIndex: TdxParagraphIndex;
    FEndParagraphIndex: TdxParagraphIndex;
    function GetAvoidDoubleBorders: Boolean;
    function GetBackgroundColor: TdxAlphaColor;
    function GetForegroundColor: TdxAlphaColor;
    function GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
    function GetCellConditionalFormattingMasks: TdxConditionalTableStyleFormattingTypes;
    function GetColumnSpan: Integer;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetFitText: Boolean;
    function GetHideCellMark: Boolean;
    function GetIndexInRow: Integer;
    function GetIsFirstCellInRow: Boolean;
    function GetIsFirstCellInTable: Boolean;
    function GetIsFirstRow: Boolean;
    function GetIsLastCellInRow: Boolean;
    function GetIsLastCellInTable: Boolean;
    function GetIsLastRow: Boolean;
    function GetNext: TdxTableCell;
    function GetNextCellInRow: TdxTableCell;
    function GetNoWrap: Boolean;
    function GetPieceTable: TdxSimplePieceTable;
    function GetPreferredWidth: TdxPreferredWidth;
    function GetPrevious: TdxTableCell;
    function GetRowIndex: Integer;
    function GetShading: TdxShadingPattern;
    function GetTable: TdxTable;
    function GetTableCellStyle: TdxTableCellStyle;
    function GetTextDirection: TdxTextDirection;
    function GetVerticalAlignment: TdxVerticalAlignment;
    function GetVerticalMerging: TdxMergingState;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetForegroundColor(const Value: TdxAlphaColor);
    procedure SetCellConditionalFormatting(const Value: TdxConditionalTableStyleFormattingTypes);
    procedure SetColumnSpan(const Value: Integer);
    procedure SetFitText(const Value: Boolean);
    procedure SetHideCellMark(const Value: Boolean);
    procedure SetNoWrap(const Value: Boolean);
    procedure SetShading(const AValue: TdxShadingPattern);
    procedure SetStyleIndex(const Value: Integer);
    procedure SetTextDirection(const Value: TdxTextDirection);
    procedure SetVerticalAlignment(const Value: TdxVerticalAlignment);
    procedure SetVerticalMerging(const Value: TdxMergingState);
  protected
    function GetTableCellPropertiesSource(AMask: Integer): TdxTableCellProperties;
    function GetActualCellMarginBase(AAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase): TdxMarginUnitBase; virtual;
    function GetActualCellBorder(ABorderAccessor: TdxTableBorderAccessor; AIsBorderCell: Boolean): TdxBorderBase;
    function ShouldReturnEmptyTopCellBorder: Boolean;
    function ShouldReturnEmptyBottomCellBorder: Boolean;
    function CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
    procedure OnCellPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs); virtual;
  public
    constructor Create(ARow: TdxTableRow);
    destructor Destroy; override;

    function IsContinueVerticalMerging: Boolean; override;
    function GetEndParagraphIndexCore: Integer; override;
    function GetIndexCore: Integer; override;
    function GetTableCore: TdxCustomTable; override;
    function GetRowCore: TdxCustomTableRow; override;

    function GetStartColumnIndexConsiderRowGrid: Integer;
    function GetEndColumnIndexConsiderRowGrid(AStartColumnIndex: Integer): Integer; overload;

    function GetParentMergedTableCellProperties: TdxMergedTableCellProperties; virtual;
    procedure SubscribeCellPropertiesEvents; virtual;
    procedure UnsubscribeCellPropertiesEvents; virtual;

    procedure ResetConditionalType;
    function GetMinCellWidth: Integer;
    function GetActualBackgroundColor: TdxAlphaColor;
    function GetActualLeftMargin: TdxMarginUnitBase;
    function GetActualRightMargin: TdxMarginUnitBase;
    function GetActualTopMargin: TdxMarginUnitBase;
    function GetActualBottomMargin: TdxMarginUnitBase;
    function GetActualLeftCellBorder: TdxBorderBase;
    function GetActualRightCellBorder: TdxBorderBase;
    function GetActualTopCellBorder: TdxBorderBase;
    function GetActualBottomCellBorder: TdxBorderBase;
    function GetEndColumnIndexConsiderRowGrid: Integer; overload;
    function GetVerticalSpanCells: TdxTableCellList;
    procedure CopyProperties(ASourceCell: TdxTableCell);

    function GetMergedCharacterProperties: TdxMergedCharacterProperties;
    function GetMergedParagraphProperties: TdxMergedParagraphProperties;
    procedure SetTableCellStyleIndexCore(ANewStyleIndex: Integer);

    property Row: TdxTableRow read FRow;
    property Properties: TdxTableCellProperties read FProperties;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
    property Table: TdxTable read GetTable;
    property StartParagraphIndex: TdxParagraphIndex read FStartParagraphIndex write FStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read FEndParagraphIndex write FEndParagraphIndex;
    property ConditionalType: TdxConditionalColumnType read FConditionalType write FConditionalType;
    property RowIndex: Integer read GetRowIndex;
    property IndexInRow: Integer read GetIndexInRow;
    property TableCellStyle: TdxTableCellStyle read GetTableCellStyle;
    property StyleIndex: Integer read FStyleIndex write SetStyleIndex;
    property PreferredWidth: TdxPreferredWidth read GetPreferredWidth;

    property HideCellMark: Boolean read GetHideCellMark write SetHideCellMark;
    property NoWrap: Boolean read GetNoWrap write SetNoWrap;
    property FitText: Boolean read GetFitText write SetFitText;
    property TextDirection: TdxTextDirection read GetTextDirection write SetTextDirection;
    property VerticalAlignment: TdxVerticalAlignment read GetVerticalAlignment write SetVerticalAlignment;
    property ColumnSpan: Integer read GetColumnSpan write SetColumnSpan;
    property VerticalMerging: TdxMergingState read GetVerticalMerging write SetVerticalMerging;
    property CellConditionalFormatting: TdxConditionalTableStyleFormattingTypes read GetCellConditionalFormatting write SetCellConditionalFormatting;
    property CellConditionalFormattingMasks: TdxConditionalTableStyleFormattingTypes read GetCellConditionalFormattingMasks;
    property ForegroundColor: TdxAlphaColor read GetForegroundColor write SetForegroundColor;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property Shading: TdxShadingPattern read GetShading write SetShading;

    property IsFirstCellInRow: Boolean read GetIsFirstCellInRow;
    property IsLastCellInRow: Boolean read GetIsLastCellInRow;
    property IsFirstRow: Boolean read GetIsFirstRow;
    property IsLastRow: Boolean read GetIsLastRow;
    property IsFirstCellInTable: Boolean read GetIsFirstCellInTable;
    property IsLastCellInTable: Boolean read GetIsLastCellInTable;
    property AvoidDoubleBorders: Boolean read GetAvoidDoubleBorders;
    property Next: TdxTableCell read GetNext;
    property NextCellInRow: TdxTableCell read GetNextCellInRow;
    property Previous: TdxTableCell read GetPrevious;
    property LayoutProperties: TdxTableCellLayoutProperties read FLayoutProperties;
  end;

  TdxTableCellList = class(TdxList<TdxTableCell>);

  { TdxTableRowLayoutProperties }

  TdxTableRowLayoutProperties = class
  strict private
    FGridBefore: Integer;
    FGridAfter: Integer;
  protected
    procedure CopyFrom(const ASourceProperties: TdxTableRowLayoutProperties); virtual;
  public
    property GridBefore: Integer read FGridBefore write FGridBefore;
    property GridAfter: Integer read FGridAfter write FGridAfter;
  end;

  { TdxTableItemCollectionCore }

  TdxTableItemCollectionCore<TItem: class; TOwner: class> = class
  strict private
    FItems: TdxObjectList<TItem>;
    FOwner: TOwner;
    function GetItem(Index: Integer): TItem;
    function GetCount: Integer;
  public
    constructor Create(AOwner: TOwner);
    destructor Destroy; override;

    procedure ForEach(const Action: TdxAction<TItem>);
    function First: TItem;
    function Last: TItem;

    property Items: TdxObjectList<TItem> read FItems;
    property Self[Index: Integer]: TItem read GetItem; default;
    property Count: Integer read GetCount;
    property Owner: TOwner read FOwner;
  end;

  { TdxTableCellCollection }

  TdxTableCellCollection = class(TdxTableItemCollectionCore<TdxTableCell, TdxTableRow>)
  strict private
    function GetOwner: TdxTableRow;
    procedure AddCellCore(AIndex: Integer); overload;
  public
    constructor Create(ARow: TdxTableRow; ACellCount: Integer);
    procedure AddInternal(ACell: TdxTableCell);
    function IndexOf(ACell: TdxTableCell): Integer;

    procedure AddCellCore(AIndex: Integer; ACell: TdxTableCell); overload;
    procedure DeleteInternal(ACell: TdxTableCell);
    function ExtractInternal(ACell: TdxTableCell): TdxTableCell;

    property Row: TdxTableRow read GetOwner;
  end;

  { TdxTableRow }

  TdxTableRow = class(TdxCustomTableRow)
  strict private
    FTable: TdxTable;
    FConditionalType: TdxConditionalRowType;
    FProperties: TdxTableRowProperties;
    FLayoutProperties: TdxTableRowLayoutProperties;
    FCells: TdxTableCellCollection;
    FTablePropertiesException: TdxTableProperties;
    function GetCantSplit: Boolean;
    function GetCellSpacing: TdxWidthUnit;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetFirstCell: TdxTableCell;
    function GetGridAfter: Integer;
    function GetGridBefore: Integer;
    function GetHeader: Boolean;
    function GetHeight: TdxHeightUnit;
    function GetHideCellMark: Boolean;
    function GetIndexInTable: Integer;
    function GetIsFirstRowInTable: Boolean;
    function GetIsLastRowInTable: Boolean;
    function GetLastCell: TdxTableCell;
    function GetNext: TdxTableRow;
    function GetPieceTable: TdxSimplePieceTable;
    function GetPrevious: TdxTableRow;
    function GetTableRowAlignment: TdxTableRowAlignment;
    function GetWidthAfter: TdxWidthUnit;
    function GetWidthBefore: TdxWidthUnit;
    procedure SetCantSplit(const Value: Boolean);
    procedure SetGridAfter(const Value: Integer);
    procedure SetGridBefore(const Value: Integer);
    procedure SetHeader(const Value: Boolean);
    procedure SetHideCellMark(const Value: Boolean);
    procedure SetTableRowAlignment(const Value: TdxTableRowAlignment);
  protected
    function GetTableRowPropertiesSource(AMask: Integer): TdxTableRowProperties;
    procedure OnRowPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs); virtual;
    procedure CopyFrom(ASourceRow: TdxTableRow); virtual;
  public
    constructor Create(ATable: TdxTable; ACellCount: Integer = 0);
    destructor Destroy; override;

    function GetIndexCore: Integer; override;

    procedure ResetConditionalType;
    procedure SetTable(ATable: TdxTable); virtual;

    function GetLeftCellMarginConsiderExceptions: TdxMarginUnitBase; virtual;
    function GetRightCellMarginConsiderExceptions: TdxMarginUnitBase; virtual;
    function GetTopCellMarginConsiderExceptions: TdxMarginUnitBase; virtual;
    function GetBottomCellMarginConsiderExceptions: TdxMarginUnitBase; virtual;
    function GetParentMergedTableRowProperties: TdxMergedTableRowProperties; virtual;
    procedure SubscribeRowPropertiesEvents; virtual;
    procedure UnsubscribeRowPropertiesEvents; virtual;

    property Table: TdxTable read FTable;
    property Properties: TdxTableRowProperties read FProperties;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property PieceTable: TdxSimplePieceTable read GetPieceTable;
    property LayoutProperties: TdxTableRowLayoutProperties read FLayoutProperties;
    property TablePropertiesException: TdxTableProperties read FTablePropertiesException;
    property ConditionalType: TdxConditionalRowType read FConditionalType write FConditionalType;
    property Cells: TdxTableCellCollection read FCells;
    property FirstCell: TdxTableCell read GetFirstCell;
    property LastCell: TdxTableCell read GetLastCell;
    property IndexInTable: Integer read GetIndexInTable;
    property Height: TdxHeightUnit read GetHeight;
    property WidthBefore: TdxWidthUnit read GetWidthBefore;
    property WidthAfter: TdxWidthUnit read GetWidthAfter;
    property CellSpacing: TdxWidthUnit read GetCellSpacing;
    property Header: Boolean read GetHeader write SetHeader;
    property HideCellMark: Boolean read GetHideCellMark write SetHideCellMark;
    property CantSplit: Boolean read GetCantSplit write SetCantSplit;
    property TableRowAlignment: TdxTableRowAlignment read GetTableRowAlignment write SetTableRowAlignment;
    property GridAfter: Integer read GetGridAfter write SetGridAfter;
    property GridBefore: Integer read GetGridBefore write SetGridBefore;
    property IsFirstRowInTable: Boolean read GetIsFirstRowInTable;
    property IsLastRowInTable: Boolean read GetIsLastRowInTable;
    property Next: TdxTableRow read GetNext;
    property Previous: TdxTableRow read GetPrevious;
  end;

  TdxTableRowList = class(TdxFastList)
  private
    function GetItem(Index: Integer): TdxTableRow;
  public
    function Contains(Value: TdxTableRow): Boolean;
    property Items[Index: Integer]: TdxTableRow read GetItem; default;
  end;

  { TdxTableRowCollection }

  TdxTableRowCollection = class(TdxTableItemCollectionCore<TdxTableRow, TdxTable>)
  strict private const
    BinarySearchTreshold = 32;
  strict private
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetOwner: TdxTable;
  protected
    procedure AddRowCore(AIndex, ACellCount: Integer); overload;
    function IndexOfBinarySearch(ARow: TdxTableRow): Integer;
  public
    constructor Create(AOwner: TdxTable; ARowCount, ACellCount: Integer);
    destructor Destroy; override;
    procedure AddInternal(ARow: TdxTableRow);
    function IndexOf(ARow: TdxTableRow): Integer;
    procedure RemoveInternal(ARow: TdxTableRow);

    procedure AddRowCore(AIndex: Integer; ARow: TdxTableRow); overload;
    procedure DeleteRowCore(AIndex: Integer);
    function ExtractRowCore(AIndex: Integer): TdxTableRow;

    property Table: TdxTable read GetOwner;
    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
  end;

  { TdxTableGridColumn }

  TdxTableGridColumn = class
  strict private
    FWidth: TdxLayoutUnit;
    FMinWidth: TdxLayoutUnit;
    FMaxWidth: TdxLayoutUnit;
    FPreferredWidth: TdxLayoutUnit;
    FTotalHorizontalMargins: TdxLayoutUnit;
    FPercentBased: Boolean;
    procedure SetWidth(const Value: TdxLayoutUnit);
    procedure SetMinWidth(const Value: TdxLayoutUnit);
    procedure SetTotalHorizontalMargins(const Value: TdxLayoutUnit);
    procedure SetMaxWidth(const Value: TdxLayoutUnit);
    procedure SetPreferredWidth(const Value: TdxLayoutUnit);
  public
    constructor Create(AWidth: TdxLayoutUnit; APercentBased: Boolean); overload;

    property Width: TdxLayoutUnit read FWidth write SetWidth;
    property MinWidth: TdxLayoutUnit read FMinWidth write SetMinWidth;
    property TotalHorizontalMargins: TdxLayoutUnit read FTotalHorizontalMargins write SetTotalHorizontalMargins;
    property MaxWidth: TdxLayoutUnit read FMaxWidth write SetMaxWidth;
    property PreferredWidth: TdxLayoutUnit read FPreferredWidth write SetPreferredWidth;
    property PercentBased: Boolean read FPercentBased write FPercentBased;
  end;

  { TdxTableGridInterval }

  TdxTableGridInterval = record
  strict private
    FColSpan: Integer;
    FIntervalType: TdxTableGridIntervalType;
    FWidth: TdxLayoutUnit;
  public
    constructor Create(AWidth: TdxLayoutUnit; AColSpan: Integer; AIntervalType: TdxTableGridIntervalType);
    class operator Equal(const A, B: TdxTableGridInterval): Boolean;

    property Width: TdxLayoutUnit read FWidth write FWidth;
    property ColumnSpan: Integer read FColSpan;
    property IntervalType: TdxTableGridIntervalType read FIntervalType write FIntervalType;
  end;
  TdxTableGridIntervalList = class(TList<TdxTableGridInterval>);

  { TdxTableGridColumnCollection }

  TdxTableGridColumnCollection = TdxObjectList<TdxTableGridColumn>;

  { TdxTableGrid }

  TdxTableGrid = class
  strict private
    FColumnsCollection: TdxTableGridColumnCollection;
    function GetColumn(Index: Integer): TdxTableGridColumn;
  public
    constructor Create; overload;
    constructor Create(AIntervals: TdxTableGridIntervalList); overload;
    destructor Destroy; override;
    property Self[Index: Integer]: TdxTableGridColumn read GetColumn; default;
    property Columns: TdxTableGridColumnCollection read FColumnsCollection;
  end;

  { TdxTableGridIntervalIterator }

  TdxTableGridIntervalIterator = class
  strict private
    FPercentBaseWidth: Integer;
    FIntervals: TdxTableGridIntervalList;
    FCurrentIntervalIndex: Integer;
    FCurrentInterval: TdxNullableValue<TdxTableGridInterval>;
    function GetEndOfIntervals: Boolean;
  protected
    function MoveNextInterval: Boolean;

    property CurrentIntervalIndex: Integer read FCurrentIntervalIndex;
  public
    constructor Create(AIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer);
    destructor Destroy; override;
    procedure Reset;
    function Advance(const AInterval: TdxTableGridInterval): Boolean;
    class function SubstractIntervals(const AInterval1, AInterval2: TdxTableGridInterval; ABaseWidth1, ABaseWidth2: Integer): TdxTableGridInterval; static;

    property PercentBaseWidth: Integer read FPercentBaseWidth;
    property Intervals: TdxTableGridIntervalList read FIntervals;
    property EndOfIntervals: Boolean read GetEndOfIntervals;
    property CurrentInterval: TdxNullableValue<TdxTableGridInterval> read FCurrentInterval;
  end;

  { TdxTableLayoutInfo }

  TdxTableLayoutInfo = class
  strict private
    FTableGrid: TdxTableGrid;
    FMaxTableWidth: Integer;
    FAllowTablesToExtendIntoMargins: Boolean;
    FSimpleView: Boolean;
    FPercentBaseWidth: Integer;
  public
    constructor Create(ATableGrid: TdxTableGrid; AMaxTableWidth: Integer; AAllowTablesToExtendIntoMargins, ASimpleView: Boolean;
      APercentBaseWidth: Integer);
    destructor Destroy; override;
    function CanUseTableGrid(AMaxTableWidth: Integer; AAllowTablesToExtendIntoMargins, ASimpleView: Boolean;
      APercentBaseWidth: Integer): Boolean;

    property TableGrid: TdxTableGrid read FTableGrid;
  end;

  { TdxTable }

  TdxTableCellProcessorDelegate = reference to procedure(ACell: TdxTableCell);
  TdxTableRowProcessorDelegate = reference to procedure(ACell: TdxTableRow);

  TdxTable = class(TdxCustomTable)
  strict private
    FIndex: Integer;
    FIsContainsFrame: Boolean;
    FStyleIndex: Integer;
    FParentCell: TdxTableCell;
    FNestedLevel: Integer;
    FProperties: TdxTableProperties;
    FRows: TdxTableRowCollection;
    FPieceTable: TdxSimplePieceTable;
    FCachedTableLayoutInfo: TdxTableLayoutInfo;
    function IsDifferentParagraphFramesInTable: Boolean;
    procedure SetEmptyFrameProperties;

    function GetBackgroundColor: TdxAlphaColor;
    function GetBottomMargin: TdxMarginUnitBase;
    function GetCellSpacing: TdxCellSpacing;
    function GetDocumentModel: TdxSimpleDocumentModel;
    function GetEndParagraphIndex: TdxParagraphIndex;
    function GetFirstRow: TdxTableRow;
    function GetIsTableOverlap: Boolean;
    function GetLastRow: TdxTableRow;
    function GetLeftMargin: TdxMarginUnitBase;
    function GetPreferredWidth: TdxPreferredWidth;
    function GetRightMargin: TdxMarginUnitBase;
    function GetStartParagraphIndex: TdxParagraphIndex;
    function GetTableAlignment: TdxTableRowAlignment;
    function GetTableIndent: TdxTableIndent;
    function GetTableLayout: TdxTableLayoutType;
    function GetTableLook: TdxTableLookTypes;
    function GetTableStyle: TdxTableStyle;
    function GetTableStyleColBandSize: Integer;
    function GetTableStyleRowBandSize: Integer;
    function GetTopMargin: TdxMarginUnitBase;
    procedure SetBackgroundColor(const Value: TdxAlphaColor);
    procedure SetIndex(const Value: Integer);
    procedure SetIsTableOverlap(const Value: Boolean);
    procedure SetStyleIndex(const Value: Integer);
    procedure SetTableAlignment(const Value: TdxTableRowAlignment);
    procedure SetTableLayout(const Value: TdxTableLayoutType);
    procedure SetTableLook(const Value: TdxTableLookTypes);
    procedure SetTableStyleColBandSize(const Value: Integer);
    procedure SetTableStyleRowBandSize(const Value: Integer);
  public
    constructor Create(APieceTable: TdxSimplePieceTable; AParentCell: TdxTableCell = nil; ARowCount: Integer = 1;
      ACellCount: Integer = 1); overload;
    destructor Destroy; override;

    function GetCellCore(ARowIndex, AColumnIndex: Integer): TdxCustomTableCell; override;
    function GetIndexCore: Integer; override;
    function GetParentCellCore: TdxCustomTableCell; override;
    function IsContainsFrame: Boolean; override;
    function UseFloatingPosition: Boolean; override;

    procedure EnsureTableHasSameFrameProperties;

    procedure ResetCachedLayoutInfo;
    procedure SetParentCell(ANewParentCell: TdxTableCell);
    procedure RecalNestedLevel;
    function GetTablePropertiesSource(AMask: Integer): TdxTableProperties; overload;
    function GetTablePropertiesSourceForCell(AMask: Integer; ATableCell: TdxTableCell): TdxTableProperties; overload;
    function GetTablePropertiesSourceForCell(AMask: Integer; ATableCell: TdxTableCell;
      AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableProperties; overload;
    function GetTablePropertiesSource(AMask: Integer; ARowIndex, AColumnIndex: Integer): TdxTableProperties; overload;
    function GetTablePropertiesSource(AMask: Integer; ARowIndex, AColumnIndex: Integer;
      AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableProperties; overload;
    function GetTablePropertiesSource(AMask: Integer; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType): TdxTableProperties; overload;
    function GetTablePropertiesSource(AMask: Integer; ARowType: TdxConditionalRowType;
      AColumnType: TdxConditionalColumnType; AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableProperties; overload; virtual;
    function GetColumnTypeByIndex(ARowIndex, AColumnIndex: Integer): TdxConditionalColumnType;
    function HasColumnStyle(AStyle: TdxTableStyle; AType: TdxConditionalColumnType): Boolean;
    function GetColumnTypeByIndexCore(ARowIndex, AColumnIndex: Integer): TdxConditionalColumnType;
    function GetRowTypeByIndex(ARowIndex: Integer): TdxConditionalRowType;
    function HasRowStyle(AStyle: TdxTableStyle; AType: TdxConditionalRowType): Boolean;
    function GetRowTypeByIndexCore(ARowIndex: Integer): TdxConditionalRowType;
    function GetActualLeftBorder: TdxLeftBorder;
    function GetActualRightBorder: TdxRightBorder;
    function GetActualTopBorder: TdxTopBorder;
    function GetActualBottomBorder: TdxBottomBorder;
    function GetActualInsideHorizontalBorder: TdxInsideHorizontalBorder;
    function GetActualInsideVerticalBorder: TdxInsideVerticalBorder;
    function GetActualPreferredWidth: TdxWidthUnitInfo;
    function GetParentMergedTableProperties: TdxMergedTableProperties; virtual;
    function GetMergedWithStyleTableProperties: TdxMergedTableProperties; virtual;
    function GetMergedCharacterProperties(ACell: TdxTableCell): TdxMergedCharacterProperties; overload;
    procedure SetTableStyleIndexCore(ANewStyleIndex: Integer);
    function GetCell(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell; overload;
    function GetCell(ARowIndex, AColumnIndex: Integer): TdxTableCell; overload;
    function GetAbsoluteCellIndexInRow(ARow: TdxTableRow; AColumnIndex: Integer; ALayoutIndex: Boolean): Integer;
    function GetCellColumnIndexConsiderRowGrid(ACell: TdxTableCell): Integer;
    function GetStartCellColumnIndexConsiderRowGrid(ARow: TdxTableRow): Integer;
    function GetLastCellColumnIndexConsiderRowGrid(ARow: TdxTableRow; ALayoutIndex: Boolean): Integer;
    function GetTotalCellsInRowConsiderGrid(ARow: TdxTableRow): Integer;
    function FindTotalColumnsCountInTable: Integer;
    procedure CopyProperties(ASourceTable: TdxTable);
    procedure NormalizeRows;
    function AllCellsHasVerticalMerge(ATableRow: TdxTableRow): Boolean;
    procedure NormalizeCellColumnSpans; overload;
    procedure NormalizeCellColumnSpans(ACanNormalizeWidthBeforeAndWidthAfter: Boolean); overload;
    procedure NormalizeRowsGridBefore(ARows: TdxTableRowCollection; ACanNormalizeWidthBeforeAndWidthAfter: Boolean);
    procedure NormalizeRowGridBefore(ARow: TdxTableRow; ACanNormalizeWidthBeforeAndWidthAfter: Boolean;
      AMinGridBefore, AMinGridAfter: Integer);
    procedure NormalizeRowGridBeforeCore(ARow: TdxTableRow; ACanNormalizeWidthBeforeAndWidthAfter: Boolean;
      AMinGridBefore, AMinGridAfter: Integer);
    procedure NormalizeTableRow(ARow: TdxTableRow; AGridIntervals: TdxTableGridIntervalList; ARowIndex: Integer);
    procedure NormalizeTableRowCore(ARow: TdxTableRow; AGridIntervals: TdxTableGridIntervalList; ARowIndex: Integer);
    function CalculateNewSpan(AOldSpan: Integer; AIntervalIterator: TdxTableGridIntervalIterator): Integer;
    procedure NormalizeTableCellVerticalMerging; virtual;
    procedure SubscribeTablePropertiesEvents; virtual;
    procedure OnPropertiesObtainAffectedRangeCore(E: TdxObtainAffectedRangeEventArgs); overload; virtual;
    procedure OnPropertiesObtainAffectedRangeCore(E: TdxObtainAffectedRangeEventArgs; AFirstRowIndex: Integer); overload; virtual;
    procedure OnTablePropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs); virtual;
    function GetGridIntervals: TdxTableGridIntervalList;
    procedure NormalizeTableGrid;
    function GetEndColumnIndex(ALastCell: TdxTableCell): Integer;
    function GetFirstCellInVerticalMergingGroup(ATableCell: TdxTableCell): TdxTableCell; virtual;
    procedure RemoveInvalidVerticalSpans(AFixLastRow: Boolean); virtual;
    procedure RemoveInvalidVarticalSpansFromTableWithOneRow(ARows: TdxTableRowCollection);
    procedure RemoveVerticalSpanFromLastRowCells; virtual;
    procedure Normalize; overload; virtual;
    procedure Normalize(AFixLastRow: Boolean); overload; virtual;
    procedure ForEachRow(const ARowProcessor: TdxTableRowProcessorDelegate);
    procedure ForEachCell(const ACellProcessor: TdxTableCellProcessorDelegate);
    procedure InitializeColumnWidths(AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths,
      AOuterColumnWidth: Integer; AMatchHorizontalTableIndentsToTextEdge: Boolean);
    function DistributeWidthsToAllColumns(AWidth, ACount: Integer): TArray<Integer>;
    function IsCellWidthDefined(AFixedColumnWidths: Integer): Boolean;
    procedure SetAllColumnsWidthsToFixedColumnWidths(AFixedColumnWidths: Integer); virtual;
    procedure DistributeHundredPercentToAllColumns; virtual;
    procedure ApplyWidthPercentBased(ACell: TdxTableCell; AColumnWidthInFiftiethsOfPercent, ALastColumnCorrection: Integer);
    procedure ApplyWidthModelUnits(ACell: TdxTableCell; AFixedColumnWidths: Integer);
    function GetTableWidth(ATable: TdxTable; AColumnWidth: Integer; AMatchHorizontalTableIndentsToTextEdge: Boolean): Integer;
    function GetLayoutMarginValue(AMargin: TdxMarginUnitBase): Integer;
    function ContainsTable(ATable: TdxTable): Boolean;
    function GetTableCellConditionalTypes(ATableCell: TdxTableCell): TdxConditionalTableStyleFormattingTypes;
    function GetTableCellProperties(AMask: Integer; ATableCell: TdxTableCell): TdxTableCellProperties;
    function GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ATableCell: TdxTableCell): TdxCharacterProperties;
    function GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ATableCell: TdxTableCell): TdxParagraphProperties;
    function GetMergedCharacterProperties(ACell: TdxTableCell; ATableStyle: TdxTableStyle): TdxMergedCharacterProperties; overload;
    function GetMergedParagraphProperties(ATableCell: TdxTableCell): TdxMergedParagraphProperties; overload;
    function GetMergedParagraphProperties(ATableStyle: TdxTableStyle; ATableCell: TdxTableCell): TdxMergedParagraphProperties; overload;
    function GetTableRowProperties(AMask: Integer; ATableRow: TdxTableRow): TdxTableRowProperties;
    function GetExistingValidColumnsPositions: TdxSortedList<Integer>;

    property DocumentModel: TdxSimpleDocumentModel read GetDocumentModel;
    property &Index: Integer read FIndex write SetIndex;
    property PieceTable: TdxSimplePieceTable read FPieceTable;
    property TableProperties: TdxTableProperties read FProperties;
    property ParentCell: TdxTableCell read FParentCell;
    property CachedTableLayoutInfo: TdxTableLayoutInfo read FCachedTableLayoutInfo write FCachedTableLayoutInfo;
    property Rows: TdxTableRowCollection read FRows;
    property FirstRow: TdxTableRow read GetFirstRow;
    property LastRow: TdxTableRow read GetLastRow;
    property TableStyle: TdxTableStyle read GetTableStyle;
    property NestedLevel: Integer read FNestedLevel;
    property StyleIndex: Integer read FStyleIndex write SetStyleIndex;
    property LeftMargin: TdxMarginUnitBase read GetLeftMargin;
    property RightMargin: TdxMarginUnitBase read GetRightMargin;
    property TopMargin: TdxMarginUnitBase read GetTopMargin;
    property BottomMargin: TdxMarginUnitBase read GetBottomMargin;
    property CellSpacing: TdxCellSpacing read GetCellSpacing;
    property TableIndent: TdxTableIndent read GetTableIndent;
    property PreferredWidth: TdxPreferredWidth read GetPreferredWidth;
    property TableLayout: TdxTableLayoutType read GetTableLayout write SetTableLayout;
    property TableAlignment: TdxTableRowAlignment read GetTableAlignment write SetTableAlignment;
    property TableLook: TdxTableLookTypes read GetTableLook write SetTableLook;
    property TableStyleColBandSize: Integer read GetTableStyleColBandSize write SetTableStyleColBandSize;
    property TableStyleRowBandSize: Integer read GetTableStyleRowBandSize write SetTableStyleRowBandSize;
    property IsTableOverlap: Boolean read GetIsTableOverlap write SetIsTableOverlap;
    property BackgroundColor: TdxAlphaColor read GetBackgroundColor write SetBackgroundColor;
    property StartParagraphIndex: TdxParagraphIndex read GetStartParagraphIndex;
    property EndParagraphIndex: TdxParagraphIndex read GetEndParagraphIndex;
  end;

  { TdxHorizontalCellBordersInfo }

  TdxHorizontalCellBordersInfo = packed record
  strict private
    FAboveCell: TdxTableCell;
    FBelowCell: TdxTableCell;
    FBorder: TdxBorderBase;
    FColumnSpan: Integer;
    FStartColumnIndex: Integer;
    function GetEndColumnIndex: Integer;
  public
    constructor Create(AAboveCell, ABelowCell: TdxTableCell; ABorder: TdxBorderBase; AStartColumnIndex, AEndColumnIndex: Integer);

    property StartColumnIndex: Integer read FStartColumnIndex;
    property ColumnSpan: Integer read FColumnSpan;
    property EndColumnIndex: Integer read GetEndColumnIndex;
    property Border: TdxBorderBase read FBorder;
    property AboveCell: TdxTableCell read FAboveCell;
    property BelowCell: TdxTableCell read FBelowCell;
  end;

  TdxNullableHorizontalCellBordersInfo = TdxNullableValue<TdxHorizontalCellBordersInfo>;

  { TdxHorizontalCellBordersInfoList }

  TdxHorizontalCellBordersInfoList = class sealed(TList<TdxHorizontalCellBordersInfo>)
  public
    function Clone: TdxHorizontalCellBordersInfoList;
  end;

  { TdxVerticalBorderPositions }

  TdxVerticalBorderPositions = class
  strict private
    FInitialPositions: TdxLayoutUnitSortedList;
    FAlignedPositions: TdxLayoutUnitSortedList;
  public
    constructor Create(AInitialPositions, AAlignedPositions: TdxLayoutUnitSortedList);
    destructor Destroy; override;

    property InitialPositions: TdxLayoutUnitSortedList read FInitialPositions;
    property AlignedPosition: TdxLayoutUnitSortedList read FAlignedPositions;
  end;

  { TdxSelectedTableStructureBase }

  TdxSelectedTableStructureBase = class(TdxReferencedObject)
  strict private
    FOriginalStartLogPosition: TdxDocumentLogPosition;
  protected
    function GetFirstSelectedCell: TdxTableCell; virtual; abstract;
    function GetIsNotEmpty: Boolean; virtual; abstract;
    function GetSelectedOnlyOneCell: Boolean; virtual; abstract;
    function GetRowsCount: Integer; virtual; abstract;
  public
    constructor Create(AOriginalStartLogPosition: TdxDocumentLogPosition);
    procedure SetFirstSelectedCell(AStartCell: TdxTableCell; APos: TdxDocumentLogPosition); virtual; abstract;

    property FirstSelectedCell: TdxTableCell read GetFirstSelectedCell;
    property IsNotEmpty: Boolean read GetIsNotEmpty;
    property SelectedOnlyOneCell: Boolean read GetSelectedOnlyOneCell;
		property OriginalStartLogPosition: TdxDocumentLogPosition read FOriginalStartLogPosition write FOriginalStartLogPosition;
    property RowsCount: Integer read GetRowsCount;
  end;

  { TdxTableCollection }

  TdxTableCollection = class
  private
    FTables: TdxNotificationCollection<TdxTable>;
    function GetCount: Integer;
    function GetTable(Index: Integer): TdxTable;
    function GetFirst: TdxTable;
    function GetLast: TdxTable;
  public
    constructor Create;
    destructor Destroy; override;

    procedure ForEach(const Action: TdxAction<TdxTable>);
    function IndexOf(AItem: TdxTable): Integer;
    procedure Remove(AIndex: Integer);
    procedure RemoveLast;
    procedure Add(AItem: TdxTable);
    function Contains(AItem: TdxTable): Boolean;
    procedure Clear;

    procedure AddCollectionChangedHandler(const AHandler: TdxNotificationCollectionChangedEvent);
    procedure RemoveCollectionChangedHandler(const AHandler: TdxNotificationCollectionChangedEvent);

    property Count: Integer read GetCount;
    property Self[Index: Integer]: TdxTable read GetTable; default;
    property First: TdxTable read GetFirst;
    property Last: TdxTable read GetLast;
  end;

  { TdxTableChangeActionCalculator }

  TdxTableChangeActionCalculator = class abstract
  public
    class function CalculateChangeActions(AChange: TdxTableChangeType): TdxDocumentModelChangeActions; static;
  end;

implementation

uses
  Classes, Contnrs, Math, RTLConsts,
  dxRichEdit.Utils.Exceptions,
  dxMeasurementUnits,
  dxRichEdit.DocumentModel.TableCalculator,
  dxRichEdit.DocumentModel.Boxes,
  dxRichEdit.DocumentModel.PieceTable,
  dxRichEdit.DocumentModel.FrameFormatting,
  dxRichEdit.DocumentModel.History.Table;

type
  { TdxTableLeftBorderAccessor }

  TdxTableLeftBorderAccessor = class(TdxTableBorderAccessor)
  public
    function UseMask: Integer; override;
    function CellUseMask: Integer; override;
    function UseInsideBorderMask: Integer; override;
    function GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
  end;

  { TdxTableRightBorderAccessor }

  TdxTableRightBorderAccessor = class(TdxTableBorderAccessor)
  public
    function UseMask: Integer; override;
    function CellUseMask: Integer; override;
    function UseInsideBorderMask: Integer; override;
    function GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
  end;

  { TdxTableTopBorderAccessor }

  TdxTableTopBorderAccessor = class(TdxTableBorderAccessor)
  public
    function UseMask: Integer; override;
    function CellUseMask: Integer; override;
    function UseInsideBorderMask: Integer; override;
    function GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
  end;

  { TdxTableBottomBorderAccessor }

  TdxTableBottomBorderAccessor = class(TdxTableBorderAccessor)
  public
    function UseMask: Integer; override;
    function CellUseMask: Integer; override;
    function UseInsideBorderMask: Integer; override;
    function GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase; override;
    function GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase; override;
  end;

  { TTableRowAndParagraphIndexComparable }

  TTableRowAndParagraphIndexComparable = class(TInterfacedObject, IdxComparable<TdxTableRow>)
  strict private
    FParagraphIndex: TdxParagraphIndex;
  public
    constructor Create(AParagraphIndex: TdxParagraphIndex);
    function CompareTo(const Value: TdxTableRow): Integer;
  end;

{ TdxTableLeftBorderAccessor }

function TdxTableLeftBorderAccessor.UseMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseLeftBorder;
end;

function TdxTableLeftBorderAccessor.CellUseMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseLeftBorder;
end;

function TdxTableLeftBorderAccessor.UseInsideBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideVerticalBorder;
end;

function TdxTableLeftBorderAccessor.GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.LeftBorder;
end;

function TdxTableLeftBorderAccessor.GetBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.LeftBorder;
end;

function TdxTableLeftBorderAccessor.GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.InsideVerticalBorder;
end;

function TdxTableLeftBorderAccessor.GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.InsideVerticalBorder;
end;

{ TdxTableRightBorderAccessor }

function TdxTableRightBorderAccessor.UseMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseRightBorder;
end;

function TdxTableRightBorderAccessor.CellUseMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseRightBorder;
end;

function TdxTableRightBorderAccessor.UseInsideBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideVerticalBorder;
end;

function TdxTableRightBorderAccessor.GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.RightBorder;
end;

function TdxTableRightBorderAccessor.GetBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.RightBorder;
end;

function TdxTableRightBorderAccessor.GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.InsideVerticalBorder;
end;

function TdxTableRightBorderAccessor.GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.InsideVerticalBorder;
end;

{ TdxTableTopBorderAccessor }

function TdxTableTopBorderAccessor.UseMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseTopBorder;
end;

function TdxTableTopBorderAccessor.CellUseMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseTopBorder;
end;

function TdxTableTopBorderAccessor.UseInsideBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder;
end;

function TdxTableTopBorderAccessor.GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.TopBorder;
end;

function TdxTableTopBorderAccessor.GetBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.TopBorder;
end;

function TdxTableTopBorderAccessor.GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.InsideHorizontalBorder;
end;

function TdxTableTopBorderAccessor.GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.InsideHorizontalBorder;
end;

{ TdxTableBottomBorderAccessor }

function TdxTableBottomBorderAccessor.UseMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseBottomBorder;
end;

function TdxTableBottomBorderAccessor.CellUseMask: Integer;
begin
  Result := TdxTableCellPropertiesOptions.MaskUseBottomBorder;
end;

function TdxTableBottomBorderAccessor.UseInsideBorderMask: Integer;
begin
  Result := TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder;
end;

function TdxTableBottomBorderAccessor.GetBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.BottomBorder;
end;

function TdxTableBottomBorderAccessor.GetBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.BottomBorder;
end;

function TdxTableBottomBorderAccessor.GetInsideBorder(ABorders: TdxTableCellBorders): TdxBorderBase;
begin
  Result := ABorders.InsideHorizontalBorder;
end;

function TdxTableBottomBorderAccessor.GetInsideBorder(ABorders: TdxTableBorders): TdxBorderBase;
begin
  Result := ABorders.InsideHorizontalBorder;
end;

{ TdxTableBorderAccessor }

function TdxTableBorderAccessor.GetTableBorder(ABorders: TdxTableBorders; AIsInside: Boolean): TdxBorderBase;
begin
  if AIsInside then
    Result := GetInsideBorder(ABorders)
  else
    Result := GetBorder(ABorders);
end;

{ TdxTableCollection }

constructor TdxTableCollection.Create;
begin
  inherited Create;
  FTables := TdxNotificationCollection<TdxTable>.Create;
end;

destructor TdxTableCollection.Destroy;
begin
  Clear;
  FTables.Free;
  inherited Destroy;
end;

procedure TdxTableCollection.ForEach(const Action: TdxAction<TdxTable>);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Action(Self[I]);
end;

function TdxTableCollection.IndexOf(AItem: TdxTable): Integer;
begin
  Result := FTables.IndexOf(AItem);
end;

procedure TdxTableCollection.Remove(AIndex: Integer);
var
  I: Integer;
begin
  for I := AIndex + 1 to Count - 1 do
    FTables[I].Index := FTables[I].Index - 1;
  FTables.Delete(AIndex);
end;

procedure TdxTableCollection.RemoveLast;
begin
  FTables.Delete(Count - 1);
end;

procedure TdxTableCollection.Add(AItem: TdxTable);
var
  I: Integer;
begin
  if AItem.Index < 0 then
  begin
    AItem.Index := Count;
    FTables.Add(AItem);
    Exit;
  end;
  FTables.Insert(AItem.Index, AItem);
  for I := AItem.Index + 1 to Count - 1 do
    FTables[I].Index := FTables[I].Index + 1;
end;

function TdxTableCollection.Contains(AItem: TdxTable): Boolean;
begin
  Result := FTables.Contains(AItem);
end;

procedure TdxTableCollection.Clear;
var
  ACount, I: Integer;
begin
  ACount := Count;
  for I := ACount - 1 downto 0 do
    Self[I].Free;
  FTables.Clear;
end;

function TdxTableCollection.GetCount: Integer;
begin
  Result := FTables.Count;
end;

procedure TdxTableCollection.AddCollectionChangedHandler(
  const AHandler: TdxNotificationCollectionChangedEvent);
begin
  FTables.AddChangedHandler(AHandler);
end;

procedure TdxTableCollection.RemoveCollectionChangedHandler(const AHandler: TdxNotificationCollectionChangedEvent);
begin
  FTables.RemoveChangedHandler(AHandler);
end;

function TdxTableCollection.GetTable(Index: Integer): TdxTable;
begin
  Result := FTables[Index];
end;

function TdxTableCollection.GetFirst: TdxTable;
begin
  if Count <= 0 then
    Result := nil
  else
    Result := FTables[0];
end;

function TdxTableCollection.GetLast: TdxTable;
begin
  if Count <= 0 then
    Result := nil
  else
    Result := FTables[Count - 1];
end;

{ TdxTableCell }

constructor TdxTableCell.Create(ARow: TdxTableRow);
begin
  inherited Create;
  FStartParagraphIndex := -1;
  FEndParagraphIndex := -1;
  Assert(ARow <> nil);
  FRow := ARow;
  FProperties := TdxTableCellProperties.Create(PieceTable, Self);
  FLayoutProperties := TdxTableCellLayoutProperties.Create;
  SubscribeCellPropertiesEvents;
end;

destructor TdxTableCell.Destroy;
begin
  FLayoutProperties.Free;
  FProperties.Free;
  inherited Destroy;
end;

function TdxTableCell.IsContinueVerticalMerging: Boolean;
begin
  Result := VerticalMerging = TdxMergingState.Continue
end;

function TdxTableCell.GetEndParagraphIndexCore: Integer;
begin
  Result := EndParagraphIndex;
end;

function TdxTableCell.GetIndexCore: Integer;
begin
  Result := Row.Cells.IndexOf(Self);
end;

function TdxTableCell.GetTableCore: TdxCustomTable;
begin
  Result := Table;
end;

function TdxTableCell.GetRowCore: TdxCustomTableRow;
begin
  Result := Row;
end;

procedure TdxTableCell.CopyProperties(ASourceCell: TdxTableCell);
begin
  Properties.CopyFrom(ASourceCell.Properties);
  StyleIndex := ASourceCell.TableCellStyle.Copy(DocumentModel);
end;

function TdxTableCell.GetActualBottomCellBorder: TdxBorderBase;
var
  ABorderAccessor: TdxTableBottomBorderAccessor;
begin
  if AvoidDoubleBorders and ShouldReturnEmptyBottomCellBorder then
    Result := TdxDocumentModel(DocumentModel).EmptyBottomBorder
  else
  begin
    ABorderAccessor := TdxTableBottomBorderAccessor.Create;
    try
      Result := GetActualCellBorder(ABorderAccessor, IsLastRow);
    finally
      ABorderAccessor.Free;
    end;
  end;
end;

function TdxTableCell.GetActualBottomMargin: TdxMarginUnitBase;
begin
  Result := GetActualCellMarginBase(TdxBottomMarginUnit.PropertyAccessor);
end;

function TdxTableCell.GetActualCellBorder(ABorderAccessor: TdxTableBorderAccessor;
  AIsBorderCell: Boolean): TdxBorderBase;
var
  ACellSpacing: Integer;
  AInsideBorder: Boolean;
  AProperties: TdxTableProperties;
  ATableStyleCellProperties, ATableCellStyleCellProperties: TdxTableCellProperties;
begin
  if Properties.GetUse(ABorderAccessor.CellUseMask) then
    Exit(ABorderAccessor.GetBorder(Properties.Borders));
  ATableCellStyleCellProperties := TableCellStyle.GetTableCellProperties(ABorderAccessor.CellUseMask);
  if ATableCellStyleCellProperties <> nil then
    Exit(ABorderAccessor.GetBorder(ATableCellStyleCellProperties.Borders));

  ATableStyleCellProperties := Table.GetTableCellProperties(ABorderAccessor.CellUseMask, Self);
  if (ATableStyleCellProperties <> nil) and AIsBorderCell then
    Exit(ABorderAccessor.GetBorder(ATableStyleCellProperties.Borders));

  ACellSpacing := Table.CellSpacing.Value;
  if AIsBorderCell and (ACellSpacing <= 0) then
  begin
    if Row.TablePropertiesException.GetUse(ABorderAccessor.UseMask) then
      Exit(ABorderAccessor.GetBorder(Row.TablePropertiesException.Borders));
    AProperties := Table.GetTablePropertiesSourceForCell(ABorderAccessor.UseMask, Self, True, AInsideBorder);
    if AProperties <> nil then
      Exit(ABorderAccessor.GetBorder(AProperties.Borders));
  end
  else
  begin
    if Row.TablePropertiesException.GetUse(ABorderAccessor.UseInsideBorderMask) then
      Exit(ABorderAccessor.GetInsideBorder(Row.TablePropertiesException.Borders));
    AProperties := Table.GetTablePropertiesSourceForCell(ABorderAccessor.UseMask, Self, False, AInsideBorder);
    if AProperties <> nil then
      Exit(ABorderAccessor.GetTableBorder(AProperties.Borders, AInsideBorder));
  end;
  Assert(False);
  Result := ABorderAccessor.GetBorder((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties.Borders);
end;

function TdxTableCell.GetActualLeftCellBorder: TdxBorderBase;
var
  ABorderAccessor: TdxTableLeftBorderAccessor;
begin
  ABorderAccessor := TdxTableLeftBorderAccessor.Create;
  try
    Result := GetActualCellBorder(ABorderAccessor, IsFirstCellInRow);
  finally
    ABorderAccessor.Free;
  end;
end;

function TdxTableCell.GetActualBackgroundColor: TdxAlphaColor;
begin
  if Shading = TdxShadingPattern.Solid then
    Result := ForegroundColor
  else
    Result := BackgroundColor;
  if TdxAlphaColors.IsEmpty(Result) then
    Result := Table.TableProperties.BackgroundColor;
end;

function TdxTableCell.GetActualLeftMargin: TdxMarginUnitBase;
begin
  Result := GetActualCellMarginBase(TdxLeftMarginUnit.PropertyAccessor);
end;

function TdxTableCell.GetActualRightCellBorder: TdxBorderBase;
var
  ABorderAccessor: TdxTableRightBorderAccessor;
begin
  ABorderAccessor := TdxTableRightBorderAccessor.Create;
  try
    Result := GetActualCellBorder(ABorderAccessor, IsLastCellInRow);
  finally
    ABorderAccessor.Free;
  end;
end;

function TdxTableCell.GetActualRightMargin: TdxMarginUnitBase;
begin
  Result := GetActualCellMarginBase(TdxRightMarginUnit.PropertyAccessor);
end;

function TdxTableCell.GetActualTopCellBorder: TdxBorderBase;
var
  ABorderAccessor: TdxTableTopBorderAccessor;
begin
  if AvoidDoubleBorders and ShouldReturnEmptyTopCellBorder then
    Result := TdxDocumentModel(DocumentModel).EmptyTopBorder
  else
  begin
    ABorderAccessor := TdxTableTopBorderAccessor.Create;
    try
      Result := GetActualCellBorder(ABorderAccessor, IsFirstRow);
    finally
      ABorderAccessor.Free;
    end;
  end;
end;

function TdxTableCell.GetActualTopMargin: TdxMarginUnitBase;
begin
  Result := GetActualCellMarginBase(TdxTopMarginUnit.PropertyAccessor);
end;

function TdxTableCell.GetAvoidDoubleBorders: Boolean;
begin
  Result := Table.TableProperties.AvoidDoubleBorders;
end;

function TdxTableCell.GetBackgroundColor: TdxAlphaColor;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseBackgroundColor).BackgroundColor;
end;

function TdxTableCell.GetForegroundColor: TdxAlphaColor;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseForegroundColor).ForegroundColor;
end;

function TdxTableCell.GetCellConditionalFormatting: TdxConditionalTableStyleFormattingTypes;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseCellConditionalFormatting).CellConditionalFormatting;
end;

function TdxTableCell.GetCellConditionalFormattingMasks: TdxConditionalTableStyleFormattingTypes;
begin
  Result := Table.GetTableCellConditionalTypes(Self);
end;

function TdxTableCell.GetColumnSpan: Integer;
begin
  Result := Properties.ColumnSpan;
end;

function TdxTableCell.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := Row.DocumentModel;
end;

function TdxTableCell.GetEndColumnIndexConsiderRowGrid(AStartColumnIndex: Integer): Integer;
begin
  Result := AStartColumnIndex + ColumnSpan - 1;
end;

function TdxTableCell.GetEndColumnIndexConsiderRowGrid: Integer;
begin
  Result := GetEndColumnIndexConsiderRowGrid(GetStartColumnIndexConsiderRowGrid);
end;

function TdxTableCell.GetFitText: Boolean;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseFitText).FitText;
end;

function TdxTableCell.GetHideCellMark: Boolean;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseHideCellMark).HideCellMark;
end;

function TdxTableCell.GetIndexInRow: Integer;
begin
  Result := Row.Cells.IndexOf(Self);
end;

function TdxTableCell.GetIsFirstCellInRow: Boolean;
begin
  Result := Row.Cells.First = Self;
end;

function TdxTableCell.GetIsFirstCellInTable: Boolean;
begin
  Result := Table.FirstRow.FirstCell = Self;
end;

function TdxTableCell.GetIsFirstRow: Boolean;
begin
  Result := Table.Rows.First = Row;
end;

function TdxTableCell.GetIsLastCellInRow: Boolean;
begin
  Result := Row.Cells.Last = Self;
end;

function TdxTableCell.GetIsLastCellInTable: Boolean;
begin
  Result := Table.LastRow.LastCell = Self;
end;

function TdxTableCell.GetIsLastRow: Boolean;
begin
  Result := Table.Rows.Last = Row;
end;

function TdxTableCell.GetMergedCharacterProperties: TdxMergedCharacterProperties;
begin
  Result := TableCellStyle.GetMergedCharacterProperties(Table.GetRowTypeByIndex(RowIndex), Table.GetColumnTypeByIndex(RowIndex, IndexInRow));
end;

function TdxTableCell.GetMergedParagraphProperties: TdxMergedParagraphProperties;
begin
  Result := TableCellStyle.GetMergedParagraphProperties(Table.GetRowTypeByIndex(RowIndex), Table.GetColumnTypeByIndex(RowIndex, IndexInRow));
end;

function TdxTableCell.GetMinCellWidth: Integer;
begin
  Result := GetActualLeftMargin.Value + GetActualRightMargin.Value + GetActualRightCellBorder.Width + GetActualLeftCellBorder.Width;
end;

function TdxTableCell.GetNext: TdxTableCell;
begin
  if IsLastCellInRow then
  begin
    if Row.IsLastRowInTable then
      Result := nil
    else
      Result := Row.Next.FirstCell;
  end
  else
    Result := Row.Cells[Row.Cells.IndexOf(Self) + 1];
end;

function TdxTableCell.GetNextCellInRow: TdxTableCell;
begin
  if IsLastCellInRow then
    Result := nil
  else
    Result := Row.Cells[Row.Cells.IndexOf(Self) + 1];
end;

function TdxTableCell.GetNoWrap: Boolean;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseNoWrap).NoWrap;
end;

function TdxTableCell.GetParentMergedTableCellProperties: TdxMergedTableCellProperties;
var
  ATableCellProperties: TdxMergedTableCellProperties;
  AMergedTableCellProperties: TdxMergedTableCellProperties;
begin
  AMergedTableCellProperties := TableCellStyle.GetMergedTableCellProperties;
  try
    Result := TdxMergedTableCellProperties.Create(AMergedTableCellProperties);
    ATableCellProperties := Table.TableStyle.GetMergedTableCellProperties(Table.GetRowTypeByIndex(RowIndex),
      Table.GetColumnTypeByIndex(RowIndex, IndexInRow));
    try
      Result.Merge(ATableCellProperties);
    finally
      ATableCellProperties.Free;
    end;
    Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties);
  finally
    AMergedTableCellProperties.Free;
  end;
end;

function TdxTableCell.GetActualCellMarginBase(
  AAccessor: TdxMarginUnitBase.TdxMarginPropertyAccessorBase): TdxMarginUnitBase;
var
  ACurrentTableStyle: TdxTableStyle;
  ACurrentCellStyle: TdxTableCellStyle;
  ATablePropertiesException: TdxTableProperties;
begin
  if Properties.GetUse(AAccessor.CellPropertiesMask) then
    Exit(AAccessor.GetValue(Properties.CellMargins));
  ACurrentCellStyle := TableCellStyle;
  while ACurrentCellStyle <> nil do
  begin
    if ACurrentCellStyle.TableCellProperties.GetUse(AAccessor.CellPropertiesMask) then
        Exit(AAccessor.GetValue(ACurrentCellStyle.TableCellProperties.CellMargins));
    ACurrentCellStyle := ACurrentCellStyle.Parent;
  end;
  ACurrentTableStyle := Table.TableStyle;
  while ACurrentTableStyle <> nil do
  begin
    if ACurrentTableStyle.TableCellProperties.GetUse(AAccessor.CellPropertiesMask) then
      Exit(AAccessor.GetValue(ACurrentTableStyle.TableCellProperties.CellMargins));
    ACurrentTableStyle := ACurrentTableStyle.Parent;
  end;
  ATablePropertiesException := Row.TablePropertiesException;
  if ATablePropertiesException.GetUse(AAccessor.TablePropertiesMask) then
    Exit(AAccessor.GetValue(ATablePropertiesException.CellMargins));
  if Table.TableProperties.GetUse(AAccessor.TablePropertiesMask) then
    Exit(AAccessor.GetValue(Table.TableProperties.CellMargins));
  ACurrentTableStyle := Table.TableStyle;
  while ACurrentTableStyle <> nil do
  begin
    if ACurrentTableStyle.TableProperties.GetUse(AAccessor.TablePropertiesMask) then
      Exit(AAccessor.GetValue(ACurrentTableStyle.TableProperties.CellMargins));
    ACurrentTableStyle := ACurrentTableStyle.Parent;
  end;
  Result := AAccessor.GetValue((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties.CellMargins);
end;

function TdxTableCell.GetPieceTable: TdxSimplePieceTable;
begin
  Result := Row.PieceTable;
end;

function TdxTableCell.GetPreferredWidth: TdxPreferredWidth;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUsePreferredWidth).PreferredWidth;
end;

function TdxTableCell.GetPrevious: TdxTableCell;
begin
  if IsFirstCellInRow then
  begin
    if Row.IsFirstRowInTable then
      Result := nil
    else
      Result := Row.Previous.LastCell;
  end
  else
    Result := Row.Cells[Row.Cells.IndexOf(Self) - 1];
end;

function TdxTableCell.GetRowIndex: Integer;
begin
  Result := Table.Rows.IndexOf(Row);
end;

function TdxTableCell.GetStartColumnIndexConsiderRowGrid: Integer;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetStartColumnIndex(Self, False);
end;

function TdxTableCell.GetShading: TdxShadingPattern;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseShading).Shading;
end;

function TdxTableCell.GetTable: TdxTable;
begin
  Result := Row.Table;
end;

function TdxTableCell.GetTableCellPropertiesSource(AMask: Integer): TdxTableCellProperties;
var
  AResultCell: TdxTableCellProperties;
  AResultTable: TdxTableCellProperties;
begin
  if Properties.GetUse(AMask) then
    Exit(Properties);

  AResultCell := TableCellStyle.GetTableCellProperties(AMask);
  if AResultCell <> nil then
    Exit(AResultCell);
  AResultTable := Table.GetTableCellProperties(AMask, Self);
  if AResultTable <> nil then
    Exit(AResultTable);

  Result := (DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableCellProperties;
end;

function TdxTableCell.GetTableCellStyle: TdxTableCellStyle;
begin
  Result := (DocumentModel as IdxTableStylesContainer).TableCellStyles[StyleIndex];
end;

function TdxTableCell.GetTextDirection: TdxTextDirection;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseTextDirection).TextDirection;
end;

function TdxTableCell.GetVerticalAlignment: TdxVerticalAlignment;
begin
  Result := GetTableCellPropertiesSource(TdxTableCellPropertiesOptions.MaskUseVerticalAlignment).VerticalAlignment;
end;

function TdxTableCell.GetVerticalMerging: TdxMergingState;
begin
  Result := Properties.VerticalMerging;
end;

function TdxTableCell.GetVerticalSpanCells: TdxTableCellList;
begin
  Result := TdxTableCellVerticalBorderCalculator.GetVerticalSpanCells(Self, False);
end;

procedure TdxTableCell.OnCellPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
var
  ACount: TdxParagraphIndex;
  AParagraphs: TdxSimpleParagraphCollection;
  AStartParagraph, AEndParagraph: TdxParagraphBase;
begin
  AParagraphs := PieceTable.Paragraphs;
  ACount := AParagraphs.Count;
  if (StartParagraphIndex < 0) or (StartParagraphIndex >= ACount) or
    (EndParagraphIndex < 0) or (EndParagraphIndex >= ACount) then
    Exit;
  AStartParagraph := AParagraphs[StartParagraphIndex];
  E.Start := AStartParagraph.FirstRunIndex;
  if EndParagraphIndex <> StartParagraphIndex then
    AEndParagraph := AParagraphs[EndParagraphIndex]
  else
    AEndParagraph := AStartParagraph;
  E.&End := AEndParagraph.LastRunIndex;
end;

procedure TdxTableCell.ResetConditionalType;
begin
  FConditionalType := TdxConditionalColumnType.Unknown;
end;

procedure TdxTableCell.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  Properties.BackgroundColor := Value;
end;

procedure TdxTableCell.SetForegroundColor(const Value: TdxAlphaColor);
begin
  Properties.ForegroundColor := Value;
end;

procedure TdxTableCell.SetCellConditionalFormatting(const Value: TdxConditionalTableStyleFormattingTypes);
begin
  Properties.CellConditionalFormatting := Value;
end;

procedure TdxTableCell.SetColumnSpan(const Value: Integer);
begin
  Properties.ColumnSpan := Value;
end;

procedure TdxTableCell.SetFitText(const Value: Boolean);
begin
  Properties.FitText := Value;
end;

procedure TdxTableCell.SetHideCellMark(const Value: Boolean);
begin
   Properties.HideCellMark := Value;
end;

procedure TdxTableCell.SetNoWrap(const Value: Boolean);
begin
  Properties.NoWrap := Value;
end;

procedure TdxTableCell.SetShading(const AValue: TdxShadingPattern);
begin
  Properties.Shading := AValue;
end;

procedure TdxTableCell.SetStyleIndex(const Value: Integer);
var
  AItem: TdxChangeTableCellStyleIndexHistoryItem;
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('TableStyleIndex', Value);

  if FStyleIndex = Value then
    Exit;

  DocumentModel.BeginUpdate;
  try
    AItem := TdxChangeTableCellStyleIndexHistoryItem.Create(PieceTable, Table.Index, RowIndex, IndexInRow, FStyleIndex, Value);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
end;

procedure TdxTableCell.SetTableCellStyleIndexCore(ANewStyleIndex: Integer);
var
  I: Integer;
  AStart, AEnd: TdxRunIndex;
  AController: TdxTableConditionalFormattingController;
begin
  FStyleIndex := ANewStyleIndex;
  if StartParagraphIndex < 0 then
    Exit;
  for I := StartParagraphIndex to EndParagraphIndex do
    PieceTable.Paragraphs[I].ResetCachedIndices(TdxResetFormattingCacheType.All);

  AController.Init(Table);
  AController.ResetCachedProperties(0);

  if StartParagraphIndex >= 0 then
  begin
    AStart := PieceTable.Paragraphs[StartParagraphIndex].FirstRunIndex;
    AEnd := PieceTable.Paragraphs[EndParagraphIndex].LastRunIndex;
    PieceTable.ApplyChangesCore(TdxTableChangeActionCalculator.CalculateChangeActions(TdxTableChangeType.TableStyle), AStart, AEnd);
  end;
end;

procedure TdxTableCell.SetTextDirection(const Value: TdxTextDirection);
begin
  Properties.TextDirection := Value;
end;

procedure TdxTableCell.SetVerticalAlignment(const Value: TdxVerticalAlignment);
begin
  Properties.VerticalAlignment := Value;
end;

procedure TdxTableCell.SetVerticalMerging(const Value: TdxMergingState);
begin
  Properties.VerticalMerging := Value;
end;

function TdxTableCell.ShouldReturnEmptyBottomCellBorder: Boolean;
var
  AParentCellEndParagraphIndex, ALastCellEndParagraphIndex: TdxParagraphIndex;
  AParentCellEndParagraph, ALastCellEndParagraph: TdxParagraphBase;
begin
  if not IsLastRow or (Table.ParentCell = nil) then
    Exit(False);
  AParentCellEndParagraphIndex := Table.ParentCell.EndParagraphIndex;
  ALastCellEndParagraphIndex := Row.LastCell.EndParagraphIndex;
  if ALastCellEndParagraphIndex + 1 <> AParentCellEndParagraphIndex then
    Exit(False);
  ALastCellEndParagraph := PieceTable.Paragraphs[ALastCellEndParagraphIndex];
  AParentCellEndParagraph := PieceTable.Paragraphs[AParentCellEndParagraphIndex];
  Result := PieceTable.VisibleTextFilter.GetNextVisibleRunIndex(ALastCellEndParagraph.LastRunIndex) >= AParentCellEndParagraph.LastRunIndex;
end;

function TdxTableCell.CreateCellPropertiesChangedHistoryItem(AProperties: TdxTableCellProperties): TdxIndexChangedHistoryItemCore;
begin
  Assert(AProperties = Properties);
  Result := TdxTableCellPropertiesChangedHistoryItem.Create(PieceTable, StartParagraphIndex, Table.NestedLevel);
end;

function TdxTableCell.ShouldReturnEmptyTopCellBorder: Boolean;
begin
  if not IsFirstRow or (Table.ParentCell = nil) then
    Result := False
  else
    Result := Row.FirstCell.StartParagraphIndex = Table.ParentCell.StartParagraphIndex;
end;

procedure TdxTableCell.SubscribeCellPropertiesEvents;
begin
  Properties.OnObtainAffectedRange.Add(OnCellPropertiesObtainAffectedRange);
end;

procedure TdxTableCell.UnsubscribeCellPropertiesEvents;
begin
  Properties.OnObtainAffectedRange.Remove(OnCellPropertiesObtainAffectedRange);
end;

{ TdxTableRowLayoutProperties }

procedure TdxTableRowLayoutProperties.CopyFrom(const ASourceProperties: TdxTableRowLayoutProperties);
begin
  GridAfter := ASourceProperties.GridAfter;
  GridBefore := ASourceProperties.GridBefore;
end;

{ TdxTableItemCollectionCore<TItem, TOwner> }

constructor TdxTableItemCollectionCore<TItem, TOwner>.Create(AOwner: TOwner);
begin
  inherited Create;
  Assert(AOwner <> nil);
  FOwner := AOwner;
  FItems := TdxObjectList<TItem>.Create;
end;

destructor TdxTableItemCollectionCore<TItem, TOwner>.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TdxTableItemCollectionCore<TItem, TOwner>.First: TItem;
begin
  if Count > 0 then
    Result := Items[0]
  else
    Result := nil;
end;

procedure TdxTableItemCollectionCore<TItem, TOwner>.ForEach(const Action: TdxAction<TItem>);
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Action(FItems[I]);
end;

function TdxTableItemCollectionCore<TItem, TOwner>.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TdxTableItemCollectionCore<TItem, TOwner>.GetItem(Index: Integer): TItem;
begin
  Result := FItems[Index];
end;

function TdxTableItemCollectionCore<TItem, TOwner>.Last: TItem;
begin
  if Count > 0 then
    Result := Items[Count - 1]
  else
    Result := nil;
end;

{ TdxTableCellCollection }

procedure TdxTableCellCollection.AddCellCore(AIndex: Integer);
begin
  AddCellCore(AIndex, TdxTableCell.Create(Row));
end;

procedure TdxTableCellCollection.AddCellCore(AIndex: Integer; ACell: TdxTableCell);
begin
  Items.Insert(AIndex, ACell);
end;

procedure TdxTableCellCollection.AddInternal(ACell: TdxTableCell);
begin
  Items.Add(ACell);
end;

constructor TdxTableCellCollection.Create(ARow: TdxTableRow; ACellCount: Integer);
var
  I: Integer;
begin
  inherited Create(ARow);
  for I := 0 to ACellCount - 1 do
    AddCellCore(I);
end;

procedure TdxTableCellCollection.DeleteInternal(ACell: TdxTableCell);
begin
  Items.Remove(ACell);
end;

function TdxTableCellCollection.ExtractInternal(ACell: TdxTableCell): TdxTableCell;
begin
  Result := Items.Extract(ACell);
end;

function TdxTableCellCollection.GetOwner: TdxTableRow;
begin
  Result := Owner;
end;

function TdxTableCellCollection.IndexOf(ACell: TdxTableCell): Integer;
begin
  Result := Items.IndexOf(ACell);
end;

{ TdxTableRow }

constructor TdxTableRow.Create(ATable: TdxTable; ACellCount: Integer);
begin
  inherited Create;
  Assert(ATable <> nil);
  FTable := ATable;
  FCells := TdxTableCellCollection.Create(Self, ACellCount);
  FProperties := TdxTableRowProperties.Create(PieceTable);
  FLayoutProperties := TdxTableRowLayoutProperties.Create;
  FTablePropertiesException := TdxTableProperties.Create(PieceTable);
  SubscribeRowPropertiesEvents;
end;

destructor TdxTableRow.Destroy;
begin
  FTablePropertiesException.Free;
  FLayoutProperties.Free;
  FProperties.Free;
  FCells.Free;
  inherited Destroy;
end;

function TdxTableRow.GetIndexCore: Integer;
begin
  Result := Table.Rows.IndexOf(Self);
end;

procedure TdxTableRow.CopyFrom(ASourceRow: TdxTableRow);
begin
  Properties.CopyFrom(ASourceRow.Properties);
  LayoutProperties.CopyFrom(ASourceRow.LayoutProperties);
  TablePropertiesException.CopyFrom(ASourceRow.TablePropertiesException);
end;

function TdxTableRow.GetBottomCellMarginConsiderExceptions: TdxMarginUnitBase;
begin
  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseBottomMargin) then
    Result := TablePropertiesException.CellMargins.Bottom
  else
    Result := Table.BottomMargin;
end;

function TdxTableRow.GetCantSplit: Boolean;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseCantSplit).CantSplit;
end;

function TdxTableRow.GetCellSpacing: TdxWidthUnit;
var
  ATableCellsSpacing: TdxWidthUnit;
  AResult: TdxTableRowProperties;
begin
  if Properties.GetUse(TdxTableRowPropertiesOptions.MaskUseCellSpacing) then
    Exit(Properties.CellSpacing);

  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseCellSpacing) then
    Exit(TablePropertiesException.CellSpacing);

  ATableCellsSpacing := Table.CellSpacing;
  if ATableCellsSpacing <> (DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableProperties.CellSpacing then
    Exit(ATableCellsSpacing);

  AResult := Table.GetTableRowProperties(TdxTableRowPropertiesOptions.MaskUseCellSpacing, Self);
  if AResult <> nil then
    Exit(AResult.CellSpacing);

  Result := (DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableRowProperties.CellSpacing;
end;

function TdxTableRow.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := Table.DocumentModel;
end;

function TdxTableRow.GetFirstCell: TdxTableCell;
begin
  Result := Cells.First;
end;

function TdxTableRow.GetGridAfter: Integer;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseGridAfter).GridAfter;
end;

function TdxTableRow.GetGridBefore: Integer;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseGridBefore).GridBefore;
end;

function TdxTableRow.GetHeader: Boolean;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseHeader).Header;
end;

function TdxTableRow.GetHeight: TdxHeightUnit;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseHeight).Height;
end;

function TdxTableRow.GetHideCellMark: Boolean;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseHideCellMark).HideCellMark;
end;

function TdxTableRow.GetIndexInTable: Integer;
begin
  Result := FTable.Rows.IndexOf(Self);
end;

function TdxTableRow.GetIsFirstRowInTable: Boolean;
begin
  Result := Table.FirstRow = Self;
end;

function TdxTableRow.GetIsLastRowInTable: Boolean;
begin
  Result := Table.LastRow = Self;
end;

function TdxTableRow.GetLastCell: TdxTableCell;
begin
  Result := Cells.Last;
end;

function TdxTableRow.GetLeftCellMarginConsiderExceptions: TdxMarginUnitBase;
begin
  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseLeftMargin) then
    Result := TablePropertiesException.CellMargins.Left
  else
    Result := Table.LeftMargin;
end;

function TdxTableRow.GetNext: TdxTableRow;
begin
  if IsLastRowInTable then
    Result := nil
  else
    Result := Table.Rows[Table.Rows.IndexOf(Self) + 1];
end;

function TdxTableRow.GetParentMergedTableRowProperties: TdxMergedTableRowProperties;
var
  ATableRowProperties: TdxMergedTableRowProperties;
begin
  ATableRowProperties := Table.TableStyle.GetMergedTableRowProperties(FTable.GetRowTypeByIndex(IndexInTable));
  try
    Result := TdxMergedTableRowProperties.Create(ATableRowProperties);
    Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableRowProperties);
  finally
    ATableRowProperties.Free;
  end;
end;

function TdxTableRow.GetPieceTable: TdxSimplePieceTable;
begin
  Result := Table.PieceTable;
end;

function TdxTableRow.GetPrevious: TdxTableRow;
begin
  if IsFirstRowInTable then
    Result := nil
  else
    Result := Table.Rows[Table.Rows.IndexOf(Self) - 1];
end;

function TdxTableRow.GetRightCellMarginConsiderExceptions: TdxMarginUnitBase;
begin
  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseRightMargin) then
    Result := TablePropertiesException.CellMargins.Right
  else
    Result := Table.RightMargin;
end;

function TdxTableRow.GetTableRowAlignment: TdxTableRowAlignment;
begin
  if Properties.GetUse(TdxTableRowPropertiesOptions.MaskUseTableRowAlignment) then
    Exit(Properties.TableRowAlignment);

  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseTableAlignment) then
    Exit(TablePropertiesException.TableAlignment);

  Result := Table.TableAlignment;
end;

function TdxTableRow.GetTableRowPropertiesSource(AMask: Integer): TdxTableRowProperties;
begin
  if Properties.GetUse(AMask) then
    Exit(Properties);
  Result := Table.GetTableRowProperties(AMask, Self);
  if Result = nil then
    Result := (DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableRowProperties;
end;

function TdxTableRow.GetTopCellMarginConsiderExceptions: TdxMarginUnitBase;
begin
  if TablePropertiesException.GetUse(TdxTablePropertiesOptions.MaskUseTopMargin) then
    Result := TablePropertiesException.CellMargins.Top
  else
    Result := Table.TopMargin;
end;

function TdxTableRow.GetWidthAfter: TdxWidthUnit;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseWidthAfter).WidthAfter;
end;

function TdxTableRow.GetWidthBefore: TdxWidthUnit;
begin
  Result := GetTableRowPropertiesSource(TdxTableRowPropertiesOptions.MaskUseWidthBefore).WidthBefore;
end;

procedure TdxTableRow.OnRowPropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  Table.OnPropertiesObtainAffectedRangeCore(E, IndexInTable);
end;

procedure TdxTableRow.ResetConditionalType;
begin
  ConditionalType := TdxConditionalRowType.Unknown;
end;

procedure TdxTableRow.SetCantSplit(const Value: Boolean);
begin
  Properties.CantSplit := Value;
end;

procedure TdxTableRow.SetGridAfter(const Value: Integer);
begin
  Properties.GridAfter := Value;
end;

procedure TdxTableRow.SetGridBefore(const Value: Integer);
begin
  Properties.GridBefore := Value;
end;

procedure TdxTableRow.SetHeader(const Value: Boolean);
begin
  Properties.Header := Value;
end;

procedure TdxTableRow.SetHideCellMark(const Value: Boolean);
begin
  Properties.HideCellMark := Value;
end;

procedure TdxTableRow.SetTable(ATable: TdxTable);
begin
  FTable := ATable;
end;

procedure TdxTableRow.SetTableRowAlignment(const Value: TdxTableRowAlignment);
begin
  Properties.TableRowAlignment := Value;
end;

procedure TdxTableRow.SubscribeRowPropertiesEvents;
begin
  Properties.OnObtainAffectedRange.Add(OnRowPropertiesObtainAffectedRange);
end;

procedure TdxTableRow.UnsubscribeRowPropertiesEvents;
begin
  Properties.OnObtainAffectedRange.Remove(OnRowPropertiesObtainAffectedRange);
end;

{ TTableRowAndParagraphIndexComparable }

constructor TTableRowAndParagraphIndexComparable.Create(AParagraphIndex: TdxParagraphIndex);
begin
  inherited Create;
  FParagraphIndex := AParagraphIndex;
end;

function TTableRowAndParagraphIndexComparable.CompareTo(const Value: TdxTableRow): Integer;
begin
  Result := Value.FirstCell.StartParagraphIndex - FParagraphIndex;
end;

{ TdxTableRowList }

function TdxTableRowList.Contains(Value: TdxTableRow): Boolean;
begin
  Result := IndexOf(Value) >= 0;
end;

function TdxTableRowList.GetItem(Index: Integer): TdxTableRow;
begin
  Result := TdxTableRow(inherited Items[Index]);
end;

{ TdxTableRowCollection }

constructor TdxTableRowCollection.Create(AOwner: TdxTable; ARowCount, ACellCount: Integer);
var
  I: Integer;
begin
  inherited Create(AOwner);
  for I := 0 to ARowCount - 1 do
    AddRowCore(I, ACellCount);
end;

destructor TdxTableRowCollection.Destroy;
begin
  inherited Destroy;
end;

procedure TdxTableRowCollection.AddInternal(ARow: TdxTableRow);
begin
  Items.Add(ARow);
end;

procedure TdxTableRowCollection.AddRowCore(AIndex, ACellCount: Integer);
var
  ARow: TdxTableRow;
begin
  ARow := TdxTableRow.Create(Table, ACellCount);
  AddRowCore(AIndex, ARow);
end;

procedure TdxTableRowCollection.AddRowCore(AIndex: Integer; ARow: TdxTableRow);
begin
  Items.Insert(AIndex, ARow);
end;

procedure TdxTableRowCollection.DeleteRowCore(AIndex: Integer);
begin
  Items.Delete(AIndex);
end;

function TdxTableRowCollection.ExtractRowCore(AIndex: Integer): TdxTableRow;
var
  AOwnsObjects: Boolean;
begin
  AOwnsObjects := Items.OwnsObjects;
  try
    Items.OwnsObjects := False;
    Result := Items[AIndex];
    Items.Delete(AIndex);
  finally
    Items.OwnsObjects := AOwnsObjects;
  end;
end;

function TdxTableRowCollection.IndexOfBinarySearch(ARow: TdxTableRow): Integer;
var
  AFirstCell: TdxTableCell;
  AStartParagraphIndex: TdxParagraphIndex;
  AComparer: TTableRowAndParagraphIndexComparable;
begin
  AFirstCell := ARow.Cells.First;
  if AFirstCell = nil then
    Exit(Items.IndexOf(ARow));
  AStartParagraphIndex := AFirstCell.StartParagraphIndex;
  AComparer := TTableRowAndParagraphIndexComparable.Create(AStartParagraphIndex);
  try
    if not TdxAlgorithms1<TdxTableRow>.BinarySearch(Items, AComparer, Result) then
      Result := -1;
  finally
    AComparer.Free;
  end;
end;

function TdxTableRowCollection.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := Table.DocumentModel;
end;

function TdxTableRowCollection.GetOwner: TdxTable;
begin
  Result := Owner;
end;

function TdxTableRowCollection.IndexOf(ARow: TdxTableRow): Integer;
begin
  if Items.Count >= BinarySearchTreshold then
    Result := IndexOfBinarySearch(ARow)
  else
    Result := Items.IndexOf(ARow);
end;

procedure TdxTableRowCollection.RemoveInternal(ARow: TdxTableRow);
var
  AIndex: Integer;
begin
  AIndex := Items.IndexOf(ARow);
  if AIndex >= 0 then
    Items.Delete(AIndex);
end;

{ TdxTableGridColumn }

constructor TdxTableGridColumn.Create(AWidth: TdxLayoutUnit; APercentBased: Boolean);
begin
  inherited Create;
  Assert(AWidth >= 0);
  FWidth := AWidth;
  FPercentBased := APercentBased;
end;

procedure TdxTableGridColumn.SetMaxWidth(const Value: TdxLayoutUnit);
begin
  Assert(Value >= 0);
  FMaxWidth := Value;
end;

procedure TdxTableGridColumn.SetMinWidth(const Value: TdxLayoutUnit);
begin
  Assert(Value >= 0);
  FMinWidth := Value;
end;

procedure TdxTableGridColumn.SetPreferredWidth(const Value: TdxLayoutUnit);
begin
  Assert(Value >= 0);
  FPreferredWidth := Value;
end;

procedure TdxTableGridColumn.SetTotalHorizontalMargins(const Value: TdxLayoutUnit);
begin
  Assert(Value >= 0);
  FTotalHorizontalMargins := Value;
end;

procedure TdxTableGridColumn.SetWidth(const Value: TdxLayoutUnit);
begin
  Assert(Value >= 0);
  FWidth := Value;
end;

{ TdxTableGridInterval }

constructor TdxTableGridInterval.Create(AWidth: TdxLayoutUnit; AColSpan: Integer;
  AIntervalType: TdxTableGridIntervalType);
begin
  FWidth := AWidth;
  FColSpan := AColSpan;
  FIntervalType := AIntervalType;
end;

class operator TdxTableGridInterval.Equal(const A, B: TdxTableGridInterval): Boolean;
begin
  Result := (A.Width = B.Width) and (A.ColumnSpan = B.ColumnSpan) and
    (A.IntervalType = B.IntervalType);
end;

{ TdxTableGrid }

constructor TdxTableGrid.Create;
begin
  inherited Create;
  FColumnsCollection := TdxTableGridColumnCollection.Create;
end;

constructor TdxTableGrid.Create(AIntervals: TdxTableGridIntervalList);
var
  I: Integer;
begin
  Create;
  for I := 0 to AIntervals.Count - 1 do
    FColumnsCollection.Add(TdxTableGridColumn.Create(AIntervals[I].Width, AIntervals[I].IntervalType = TdxTableGridIntervalType.PercentBased));
end;

destructor TdxTableGrid.Destroy;
begin
  FreeAndNil(FColumnsCollection);
  inherited Destroy;
end;

function TdxTableGrid.GetColumn(Index: Integer): TdxTableGridColumn;
begin
  Result := FColumnsCollection[Index];
end;

{ TdxTableGridIntervalIterator }

constructor TdxTableGridIntervalIterator.Create(AIntervals: TdxTableGridIntervalList; APercentBaseWidth: Integer);
begin
  inherited Create;
  Assert(AIntervals <> nil);
  FIntervals := AIntervals;
  FPercentBaseWidth := APercentBaseWidth;
  Reset;
end;

destructor TdxTableGridIntervalIterator.Destroy;
begin
  FIntervals.Free;
  inherited Destroy;
end;

function TdxTableGridIntervalIterator.Advance(const AInterval: TdxTableGridInterval): Boolean;
begin
  Assert(not EndOfIntervals);
  Assert(CurrentInterval.Value.ColumnSpan >= AInterval.ColumnSpan);
  if CurrentInterval.Value.ColumnSpan = AInterval.ColumnSpan then
    Result := MoveNextInterval
  else
  begin
    FCurrentInterval := SubstractIntervals(CurrentInterval.Value, AInterval, PercentBaseWidth, PercentBaseWidth);
    Result := True;
  end;
end;

function TdxTableGridIntervalIterator.GetEndOfIntervals: Boolean;
begin
  Result := CurrentInterval.IsNull;
end;

function TdxTableGridIntervalIterator.MoveNextInterval: Boolean;
begin
  Inc(FCurrentIntervalIndex);
  if FCurrentIntervalIndex >= FIntervals.Count then
  begin
    FCurrentInterval.Reset;
    Result := False;
  end
  else
  begin
    FCurrentInterval := Intervals[CurrentIntervalIndex];
    Result := True;
  end;
end;

procedure TdxTableGridIntervalIterator.Reset;
begin
  FCurrentIntervalIndex := -1;
  FCurrentInterval.Reset;
  MoveNextInterval;
end;

class function TdxTableGridIntervalIterator.SubstractIntervals(const AInterval1,
  AInterval2: TdxTableGridInterval; ABaseWidth1, ABaseWidth2: Integer): TdxTableGridInterval;
var
  AColumnSpan: Integer;
begin
  AColumnSpan := AInterval1.ColumnSpan - AInterval2.ColumnSpan;
  if AInterval1.IntervalType = TdxTableGridIntervalType.NotSet then
    Result := TdxTableGridInterval.Create(0, AColumnSpan, AInterval1.IntervalType)
  else
  begin
    if AInterval2.IntervalType = TdxTableGridIntervalType.NotSet then
      Result := TdxTableGridInterval.Create(AInterval1.Width, AColumnSpan, AInterval1.IntervalType)
    else
    begin
      if AInterval1.IntervalType = AInterval2.IntervalType then
        Result := TdxTableGridInterval.Create(Max(AInterval1.Width - AInterval2.Width, 0), AColumnSpan, AInterval1.IntervalType)
      else
        if AInterval2.IntervalType = TdxTableGridIntervalType.PercentBased then
          Result := TdxTableGridInterval.Create(Max(0, AInterval1.Width - MulDiv(ABaseWidth1, AInterval2.Width, 5000)), AColumnSpan, AInterval1.IntervalType)
        else
          Result := TdxTableGridInterval.Create(Max(0, AInterval2.Width - MulDiv(ABaseWidth2, AInterval1.Width, 5000)), AColumnSpan, AInterval2.IntervalType)
    end;
  end;
end;

{ TdxTableLayoutInfo }

constructor TdxTableLayoutInfo.Create(ATableGrid: TdxTableGrid; AMaxTableWidth: Integer;
  AAllowTablesToExtendIntoMargins, ASimpleView: Boolean; APercentBaseWidth: Integer);
begin
  inherited Create;
  Assert(ATableGrid <> nil);
  FTableGrid := ATableGrid;
  FMaxTableWidth := AMaxTableWidth;
  FAllowTablesToExtendIntoMargins := AAllowTablesToExtendIntoMargins;
  FSimpleView := ASimpleView;
  FPercentBaseWidth := APercentBaseWidth;
end;

destructor TdxTableLayoutInfo.Destroy;
begin
  FreeAndNil(FTableGrid);
  inherited Destroy;
end;

function TdxTableLayoutInfo.CanUseTableGrid(AMaxTableWidth: Integer; AAllowTablesToExtendIntoMargins,
  ASimpleView: Boolean; APercentBaseWidth: Integer): Boolean;
begin
  Result := (FMaxTableWidth = AMaxTableWidth) and
    (FAllowTablesToExtendIntoMargins = AAllowTablesToExtendIntoMargins) and
    (FSimpleView = ASimpleView) and (FPercentBaseWidth = APercentBaseWidth);
end;

{ TdxTable }

constructor TdxTable.Create(APieceTable: TdxSimplePieceTable; AParentCell: TdxTableCell = nil; ARowCount: Integer = 1;
  ACellCount: Integer = 1);
begin
  inherited Create;
  FIndex := -1;
  FStyleIndex := 0;
  Assert(APieceTable <> nil);
  FParentCell := AParentCell;
  FPieceTable := APieceTable;
  if AParentCell = nil then
    FNestedLevel := 0
  else
    FNestedLevel := AParentCell.Table.NestedLevel + 1;
  FProperties := TdxTableProperties.Create(PieceTable);
  FRows := TdxTableRowCollection.Create(Self, ARowCount, ACellCount);
  SubscribeTablePropertiesEvents;
end;

destructor TdxTable.Destroy;
var
  I: Integer;
begin
  for I := FRows.Count - 1 downto 0 do
    if FRows[I].Table <> Self then
      FRows.ExtractRowCore(I);
  FCachedTableLayoutInfo.Free;
  FRows.Free;
  FProperties.Free;
  inherited Destroy;
end;

function TdxTable.AllCellsHasVerticalMerge(ATableRow: TdxTableRow): Boolean;
var
  I: Integer;
  ACells: TdxTableCellCollection;
begin
  ACells := ATableRow.Cells;
  for I := 0 to ACells.Count - 1 do
    if ACells[I].VerticalMerging <> TdxMergingState.Continue then
      Exit(False);
  Result := True;
end;

procedure TdxTable.ApplyWidthModelUnits(ACell: TdxTableCell; AFixedColumnWidths: Integer);
begin
  ACell.Properties.PreferredWidth.Value := AFixedColumnWidths;
  ACell.Properties.PreferredWidth.&Type := TdxWidthUnitType.ModelUnits;
end;

procedure TdxTable.ApplyWidthPercentBased(ACell: TdxTableCell; AColumnWidthInFiftiethsOfPercent,
  ALastColumnCorrection: Integer);
begin
  ACell.Properties.PreferredWidth.&Type := TdxWidthUnitType.FiftiethsOfPercent;
  ACell.Properties.PreferredWidth.Value := AColumnWidthInFiftiethsOfPercent;
  if ACell.IsLastCellInRow then
    ACell.Properties.PreferredWidth.Value := ACell.Properties.PreferredWidth.Value + ALastColumnCorrection;
end;

function TdxTable.CalculateNewSpan(AOldSpan: Integer; AIntervalIterator: TdxTableGridIntervalIterator): Integer;
var
  ATotalSum: Integer;
begin
  Result := 0;
  ATotalSum := 0;
  while (ATotalSum < AOldSpan) do
  begin
    Inc(ATotalSum, AIntervalIterator.CurrentInterval.Value.ColumnSpan);
    Inc(Result);
    AIntervalIterator.MoveNextInterval;
  end;
  Assert(ATotalSum = AOldSpan);
end;

function TdxTable.ContainsTable(ATable: TdxTable): Boolean;
begin
  Assert(ATable <> nil);
  if ATable.NestedLevel <= NestedLevel then
    Exit(False);
  while ATable.NestedLevel > NestedLevel do
    ATable := ATable.ParentCell.Table;
  Result := Self = ATable;
end;

procedure TdxTable.CopyProperties(ASourceTable: TdxTable);
begin
  TableProperties.CopyFrom(ASourceTable.TableProperties);
  StyleIndex := ASourceTable.TableStyle.Copy(DocumentModel);
  TableLayout := ASourceTable.TableLayout;
end;

procedure TdxTable.DistributeHundredPercentToAllColumns;
var
  AApplyWidthPercentBased: TdxTableCellProcessorDelegate;
  AColumnsCount, AColumnWidthInFiftiethsOfPercent, ALastColumnCorrection: Integer;
begin
  TableProperties.PreferredWidth.Value := 5000;
  TableProperties.PreferredWidth.&Type := TdxWidthUnitType.FiftiethsOfPercent;
  AColumnsCount := FindTotalColumnsCountInTable;
  Assert(AColumnsCount > 0);
  AColumnWidthInFiftiethsOfPercent := Math.Max(5000 div AColumnsCount, 1);
  ALastColumnCorrection := 5000 mod AColumnsCount;
  AApplyWidthPercentBased := procedure(ACell: TdxTableCell)
    begin
      ApplyWidthPercentBased(ACell, AColumnWidthInFiftiethsOfPercent, ALastColumnCorrection);
    end;
  ForEachCell(AApplyWidthPercentBased);
end;

function TdxTable.DistributeWidthsToAllColumns(AWidth, ACount: Integer): TArray<Integer>;
var
  I, ARest, ACellWidth: Integer;
begin
  SetLength(Result, ACount);
  if ACount = 0 then
    Exit;
  ARest := AWidth;
  for I := 0 to ACount - 1 do
  begin
    ACellWidth := ARest div (ACount - I);
    Result[I] := Math.Max(ACellWidth, 1);
    Dec(ARest, ACellWidth);
  end;
end;

function TdxTable.FindTotalColumnsCountInTable: Integer;
var
  ARowIndex, ACells: Integer;
  ARow: TdxTableRow;
begin
  Result := 0;
  for ARowIndex := 0 to Rows.Count - 1 do
  begin
    ARow := Rows[ARowIndex];
    ACells := GetTotalCellsInRowConsiderGrid(ARow);
    Result := Math.Max(Result, ACells);
  end;
end;

procedure TdxTable.ForEachCell(const ACellProcessor: TdxTableCellProcessorDelegate);
var
  I, J: Integer;
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
begin
  ARows := Rows;
  for I := 0 to ARows.Count - 1 do
  begin
    ACells := ARows[I].Cells;
    for J := 0 to ACells.Count - 1 do
      ACellProcessor(ACells[J]);
  end;
end;

procedure TdxTable.ForEachRow(const ARowProcessor: TdxTableRowProcessorDelegate);
var
  I: Integer;
  ARows: TdxTableRowCollection;
begin
  ARows := Rows;
  for I := 0 to ARows.Count - 1 do
    ARowProcessor(ARows[I]);
end;

function TdxTable.GetActualInsideVerticalBorder: TdxInsideVerticalBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseInsideVerticalBorder).Borders.InsideVerticalBorder;
end;

function TdxTable.GetActualPreferredWidth: TdxWidthUnitInfo;
begin
  Result := PreferredWidth.Info;
  if (Result.&Type = TdxWidthUnitType.ModelUnits) and (Result.Value = 0) then
    Result := TdxWidthUnitInfo.Create(TdxWidthUnitType.Nil, 0);
end;

function TdxTable.GetAbsoluteCellIndexInRow(ARow: TdxTableRow; AColumnIndex: Integer; ALayoutIndex: Boolean): Integer;
var
  ACurrentCell: TdxTableCell;
  ACells: TdxTableCellCollection;
  ACellsCount, ACellIndex: Integer;
begin
  if ARow.Cells.Count = 0 then
    TdxRichEditExceptions.ThrowInternalException;
  ACells := ARow.Cells;
  ACellsCount := ACells.Count;
  ACellIndex := 0;
  if ALayoutIndex then
    Dec(AColumnIndex, ARow.LayoutProperties.GridBefore)
  else
    Dec(AColumnIndex, ARow.GridBefore);
  while (AColumnIndex > 0) and (ACellIndex < ACellsCount) do
  begin
    ACurrentCell := ACells[ACellIndex];
    if ALayoutIndex then
      Dec(AColumnIndex, ACurrentCell.LayoutProperties.ColumnSpan)
     else
       Dec(AColumnIndex, ACurrentCell.ColumnSpan);
    if AColumnIndex >= 0 then
      Inc(ACellIndex);
  end;
  Result := ACellIndex;
end;

function TdxTable.GetActualBottomBorder: TdxBottomBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseBottomBorder).Borders.BottomBorder;
end;

function TdxTable.GetActualInsideHorizontalBorder: TdxInsideHorizontalBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseInsideHorizontalBorder).Borders.InsideHorizontalBorder;
end;

function TdxTable.GetActualLeftBorder: TdxLeftBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseLeftBorder).Borders.LeftBorder;
end;

function TdxTable.GetActualRightBorder: TdxRightBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseRightBorder).Borders.RightBorder;
end;

function TdxTable.GetActualTopBorder: TdxTopBorder;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTopBorder).Borders.TopBorder;
end;

function TdxTable.IsDifferentParagraphFramesInTable: Boolean;
var
  AFrameProperties, ANextFrameProperties: TdxMergedFrameProperties;
  AParagraphIndex: TdxParagraphIndex;
begin
  AFrameProperties := PieceTable.Paragraphs[StartParagraphIndex].GetActualFrameProperties(False);
  try
    for AParagraphIndex := StartParagraphIndex + 1 to EndParagraphIndex - 1 do
    begin
      ANextFrameProperties := PieceTable.Paragraphs[AParagraphIndex].GetMergedFrameProperties;
      try
        if (AFrameProperties <> nil) and not AFrameProperties.Equals(ANextFrameProperties) then
          Exit(True);
        if (AFrameProperties = nil) and (ANextFrameProperties <> nil) then
          Exit(True);
      finally
        ANextFrameProperties.Free;
      end;
    end;
    Result := False;
  finally
    AFrameProperties.Free;
  end;
end;

procedure TdxTable.SetEmptyFrameProperties;
var
  AParagraphIndex: TdxParagraphIndex;
  AParagraph: TdxSimpleParagraph;
begin
  for AParagraphIndex := StartParagraphIndex to EndParagraphIndex do
  begin
    AParagraph := PieceTable.Paragraphs[AParagraphIndex];
    AParagraph.SetDefaultFrameProperties;
  end;
end;

function TdxTable.GetBackgroundColor: TdxAlphaColor;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseBackgroundColor).BackgroundColor;
end;

function TdxTable.GetBottomMargin: TdxMarginUnitBase;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseBottomMargin).CellMargins.Bottom;
end;

function TdxTable.GetCell(ARow: TdxTableRow; AColumnIndex: Integer): TdxTableCell;
var
  AIndex: Integer;
begin
  if ARow = nil then
    Exit(nil);
  AIndex := GetAbsoluteCellIndexInRow(ARow, AColumnIndex, False);
  if ARow.Cells.Count <= AIndex then
    Result := GetCell(ARow.Previous, AColumnIndex)
  else
    Result := ARow.Cells[AIndex];
end;

function TdxTable.GetCell(ARowIndex, AColumnIndex: Integer): TdxTableCell;
var
  ARow: TdxTableRow;
begin
  if Rows.Count <= ARowIndex then
    TdxRichEditExceptions.ThrowInternalException;
  ARow := Rows[ARowIndex];
  Result := GetCell(ARow, AColumnIndex);
end;

function TdxTable.GetCellCore(ARowIndex, AColumnIndex: Integer): TdxCustomTableCell;
begin
  Result := nil;
  if (ARowIndex >= 0) and (AColumnIndex >= 0) then
    Result := Rows[ARowIndex].Cells[AColumnIndex];
end;

function TdxTable.GetIndexCore: Integer;
begin
  Result := TdxPieceTable(PieceTable).Tables.IndexOf(Self);
end;

function TdxTable.GetParentCellCore: TdxCustomTableCell;
begin
  Result := ParentCell;
end;

function TdxTable.IsContainsFrame: Boolean;
begin
  Result := FIsContainsFrame;
end;

function TdxTable.UseFloatingPosition: Boolean;
begin
  Result := TableProperties.UseFloatingPosition and (TableProperties.FloatingPosition.TextWrapping = TdxTextWrapping.Around);
end;

procedure TdxTable.EnsureTableHasSameFrameProperties;
begin
  if IsDifferentParagraphFramesInTable then
  begin
    SetEmptyFrameProperties;
    FIsContainsFrame := True;
  end;
end;

function TdxTable.GetCellColumnIndexConsiderRowGrid(ACell: TdxTableCell): Integer;
begin
  Result := ACell.GetStartColumnIndexConsiderRowGrid;
end;

function TdxTable.GetCellSpacing: TdxCellSpacing;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseCellSpacing).CellSpacing;
end;

function TdxTable.GetCharacterProperties(AMask: TdxUsedCharacterFormattingOption; ATableCell: TdxTableCell): TdxCharacterProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := TableStyle.GetCharacterProperties(AMask, GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetColumnTypeByIndex(ARowIndex, AColumnIndex: Integer): TdxConditionalColumnType;
var
  ACell: TdxTableCell;
begin
  ACell := FRows[ARowIndex].Cells[AColumnIndex];
  Result := ACell.ConditionalType;
  if Result = TdxConditionalColumnType.Unknown then
  begin
    Result := GetColumnTypeByIndexCore(ARowIndex, AColumnIndex);
    ACell.ConditionalType := Result;
  end;
end;

function TdxTable.GetColumnTypeByIndexCore(ARowIndex, AColumnIndex: Integer): TdxConditionalColumnType;
var
  AHasFirstColumnStyle: Boolean;
begin
  AHasFirstColumnStyle := False;
  if TdxTableLookType.ApplyFirstColumn in TableLook then
  begin
    if HasColumnStyle(TableStyle, TdxConditionalColumnType.FirstColumn) then
    begin
      AHasFirstColumnStyle := True;
      if AColumnIndex = 0 then
        Exit(TdxConditionalColumnType.FirstColumn);
    end;
  end;
  if TdxTableLookType.ApplyLastColumn in TableLook then
    if (AColumnIndex = Rows[ARowIndex].Cells.Count - 1) and (HasColumnStyle(TableStyle, TdxConditionalColumnType.LastColumn)) then
      Exit(TdxConditionalColumnType.LastColumn);
  if not (TdxTableLookType.DoNotApplyColumnBanding in TableLook) then
  begin
    if AHasFirstColumnStyle then
      Dec(AColumnIndex);
    if ((AColumnIndex div TableStyleColBandSize) mod 2) = 0 then
      Result := TdxConditionalColumnType.OddColumnBand
    else
      Result := TdxConditionalColumnType.EvenColumnBand;
  end
  else
    Result := TdxConditionalColumnType.Normal;
end;

function TdxTable.GetDocumentModel: TdxSimpleDocumentModel;
begin
  Result := FPieceTable.DocumentModel;
end;

function TdxTable.GetEndColumnIndex(ALastCell: TdxTableCell): Integer;
begin
  Result := ALastCell.GetEndColumnIndexConsiderRowGrid;
end;

function TdxTable.GetEndParagraphIndex: TdxParagraphIndex;
begin
  Result := LastRow.LastCell.EndParagraphIndex;
end;

function TdxTable.GetExistingValidColumnsPositions: TdxSortedList<Integer>;
var
  ARow: TdxTableRow;
  ACells: TdxTableCellCollection;
  ARowIndex, AMaxColumnIndex, AColumnIndex, ACellCount, ACellIndex: Integer;
begin
  Result := TdxSortedList<Integer>.Create;
  AMaxColumnIndex := 0;
  for ARowIndex := 0 to FRows.Count - 1 do
  begin
    ARow := FRows[ARowIndex];
    AColumnIndex := 0;
    Assert((ARow.GridBefore = 0) and (ARow.GridAfter = 0));
    if not Result.Contains(AColumnIndex) then
      Result.Add(AColumnIndex);
    ACells := ARow.Cells;
    ACellCount := ACells.Count;
    for ACellIndex := 0 to ACellCount - 1 do
    begin
      Inc(AColumnIndex, ACells[ACellIndex].ColumnSpan);
      AMaxColumnIndex := Math.Max(AColumnIndex, AMaxColumnIndex);
      if (ACellIndex < ACellCount - 1) and not Result.Contains(AColumnIndex) then
        Result.Add(AColumnIndex);
    end;
  end;
  if not Result.Contains(AMaxColumnIndex) then
    Result.Add(AMaxColumnIndex);
end;

function TdxTable.GetFirstCellInVerticalMergingGroup(ATableCell: TdxTableCell): TdxTableCell;
var
  ARow: TdxTableRow;
  ACell: TdxTableCell;
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
  I, ARowIndex, ACellColumnIndex, AColumnIndex, ACellCount: Integer;
begin
  if (ATableCell.VerticalMerging = TdxMergingState.None) or (ATableCell.VerticalMerging = TdxMergingState.Restart) then
    Exit(ATableCell);

  ARows := ATableCell.Table.Rows;
  Assert(ARows.Count > 1, 'cell.VerticalMerging == MergingState.Continue & cell.Table.Rows.Count is 1');
  ARowIndex := ARows.IndexOf(ATableCell.Row);
  ACellColumnIndex := GetCellColumnIndexConsiderRowGrid(ATableCell);
  for ARowIndex := ARowIndex - 1 downto 0 do
  begin
    ARow := ARows[ARowIndex];
    ACells := ARow.Cells;
    AColumnIndex := ARow.GridBefore;
    ACellCount := ACells.Count;
    for I := 0 to ACellCount - 1 do
    begin
      ACell := ACells[I];
      if ACellColumnIndex <= AColumnIndex then
      begin
        if (ACell.VerticalMerging <> TdxMergingState.Continue) or (ARowIndex = 0) then
          Exit(ACell)
        else
          Break;
      end;
      Inc(AColumnIndex, ACell.ColumnSpan);
    end;
  end;
  Result := nil;
end;

function TdxTable.GetFirstRow: TdxTableRow;
begin
  Result := Rows.First;
end;

function TdxTable.GetGridIntervals: TdxTableGridIntervalList;
var
  AWidthsCalculator: TdxRtfTableWidthsCalculator;
  ACalculator: TdxTableGridCalculator;
begin
  AWidthsCalculator := TdxRtfTableWidthsCalculator.Create(DocumentModel.ToDocumentLayoutUnitConverter);
  try
    ACalculator := TdxTableGridCalculator.Create(AWidthsCalculator, MaxInt);
    try
      Result := ACalculator.CalculateGridIntervals(Self, MaxInt);
    finally
      ACalculator.Free;
    end;
  finally
    AWidthsCalculator.Free;
  end;
end;

function TdxTable.GetIsTableOverlap: Boolean;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseIsTableOverlap).IsTableOverlap;
end;

function TdxTable.GetLastCellColumnIndexConsiderRowGrid(ARow: TdxTableRow; ALayoutIndex: Boolean): Integer;
var
  I: Integer;
  ACell: TdxTableCell;
  ACells: TdxTableCellCollection;
begin
  ACells := ARow.Cells;
  if ALayoutIndex then
    Result := ARow.LayoutProperties.GridBefore
  else
    Result := ARow.GridBefore;
  for I := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[I];
    if ALayoutIndex then
      Inc(Result, ACell.LayoutProperties.ColumnSpan)
    else
      Inc(Result, ACell.ColumnSpan);
  end;
end;

function TdxTable.GetLastRow: TdxTableRow;
begin
  Result := Rows.Last;
end;

function TdxTable.GetLayoutMarginValue(AMargin: TdxMarginUnitBase): Integer;
begin
  if AMargin.&Type = TdxWidthUnitType.ModelUnits then
    Result := AMargin.Value
  else
    Result := 0;
end;

function TdxTable.GetLeftMargin: TdxMarginUnitBase;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseLeftMargin).CellMargins.Left;
end;

function TdxTable.GetMergedCharacterProperties(ACell: TdxTableCell): TdxMergedCharacterProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ACell.RowIndex;
  ACellIndex := ACell.IndexInRow;
  Result := TableStyle.GetMergedCharacterProperties(GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetMergedParagraphProperties(ATableCell: TdxTableCell): TdxMergedParagraphProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := TableStyle.GetMergedParagraphProperties(GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetMergedParagraphProperties(ATableStyle: TdxTableStyle; ATableCell: TdxTableCell): TdxMergedParagraphProperties;
begin
  Result := ATableStyle.GetMergedParagraphProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTable.GetMergedWithStyleTableProperties: TdxMergedTableProperties;
begin
  Result := TdxMergedTableProperties.Create(TableProperties);
  Result.Merge(TableStyle.TableProperties);
end;

function TdxTable.GetParagraphProperties(AMask: TdxUsedParagraphFormattingOption; ATableCell: TdxTableCell): TdxParagraphProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := TableStyle.GetParagraphProperties(AMask, GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetMergedCharacterProperties(ACell: TdxTableCell; ATableStyle: TdxTableStyle): TdxMergedCharacterProperties;
begin
  Result := ATableStyle.GetMergedCharacterProperties(TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTable.GetParentMergedTableProperties: TdxMergedTableProperties;
begin
  Result := TableStyle.GetMergedTableProperties;
  Result.Merge((DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableProperties);
end;

function TdxTable.GetPreferredWidth: TdxPreferredWidth;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUsePreferredWidth).PreferredWidth;
end;

function TdxTable.GetRightMargin: TdxMarginUnitBase;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseRightMargin).CellMargins.Right;
end;

function TdxTable.GetRowTypeByIndex(ARowIndex: Integer): TdxConditionalRowType;
var
  ARow: TdxTableRow;
begin
  ARow := FRows[ARowIndex];
  Result := ARow.ConditionalType;
  if Result = TdxConditionalRowType.Unknown then
  begin
    Result := GetRowTypeByIndexCore(ARowIndex);
    ARow.ConditionalType := Result;
  end;
end;

function TdxTable.GetRowTypeByIndexCore(ARowIndex: Integer): TdxConditionalRowType;
var
  AHasFirstRowStyle: Boolean;
begin
  AHasFirstRowStyle := False;
  if TdxTableLookType.ApplyFirstRow in TableLook then
  begin
    if HasRowStyle(TableStyle, TdxConditionalRowType.FirstRow) then
    begin
      AHasFirstRowStyle := True;
      if ARowIndex = 0 then
        Exit(TdxConditionalRowType.FirstRow);
    end;
  end;
  if TdxTableLookType.ApplyLastRow in TableLook then
  begin
    if (ARowIndex = Rows.Count - 1) and HasRowStyle(TableStyle, TdxConditionalRowType.LastRow) then
      Exit(TdxConditionalRowType.LastRow);
  end;
  if not (TdxTableLookType.DoNotApplyRowBanding in TableLook) then
  begin
    if AHasFirstRowStyle then
      Dec(ARowIndex);
    if (ARowIndex div TableStyleRowBandSize) mod 2 = 0 then
      Result := TdxConditionalRowType.OddRowBand
    else
      Result := TdxConditionalRowType.EvenRowBand;
  end
  else
    Result := TdxConditionalRowType.Normal;
end;

function TdxTable.GetStartCellColumnIndexConsiderRowGrid(ARow: TdxTableRow): Integer;
begin
  Result := ARow.GridBefore;
end;

function TdxTable.GetStartParagraphIndex: TdxParagraphIndex;
begin
  Result := FirstRow.FirstCell.StartParagraphIndex;
end;

function TdxTable.GetTableAlignment: TdxTableRowAlignment;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableAlignment).TableAlignment;
end;

function TdxTable.GetTableCellConditionalTypes(ATableCell: TdxTableCell): TdxConditionalTableStyleFormattingTypes;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := TableStyle.GetConditionalPropertiesMask(GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetTableCellProperties(AMask: Integer; ATableCell: TdxTableCell): TdxTableCellProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := TableStyle.GetTableCellProperties(AMask, GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, ACellIndex));
end;

function TdxTable.GetTableRowProperties(AMask: Integer; ATableRow: TdxTableRow): TdxTableRowProperties;
var
  ARowIndex: Integer;
begin
  ARowIndex := ATableRow.IndexInTable;
  Result := TableStyle.GetTableRowProperties(AMask, GetRowTypeByIndex(ARowIndex));
end;

function TdxTable.GetTableIndent: TdxTableIndent;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableIndent).TableIndent;
end;

function TdxTable.GetTableLayout: TdxTableLayoutType;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableLayout).TableLayout;
end;

function TdxTable.GetTableLook: TdxTableLookTypes;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableLook).TableLook;
end;

function TdxTable.GetTablePropertiesSource(AMask: Integer): TdxTableProperties;
begin
  Result := GetTablePropertiesSource(AMask, TdxConditionalRowType.Normal, TdxConditionalColumnType.Normal);
end;

function TdxTable.GetTablePropertiesSource(AMask, ARowIndex, AColumnIndex: Integer): TdxTableProperties;
var
  AInsideBorder: Boolean;
begin
  Result := GetTablePropertiesSource(AMask, ARowIndex, AColumnIndex, True, AInsideBorder);
end;

function TdxTable.GetTablePropertiesSourceForCell(AMask: Integer; ATableCell: TdxTableCell; AIsBorderCell: Boolean;
  out AInsideBorder: Boolean): TdxTableProperties;
var
  ARowIndex, ACellIndex: Integer;
begin
  ARowIndex := ATableCell.RowIndex;
  ACellIndex := ATableCell.IndexInRow;
  Result := GetTablePropertiesSource(AMask, ARowIndex, ACellIndex, AIsBorderCell, AInsideBorder);
end;

function TdxTable.GetTablePropertiesSourceForCell(AMask: Integer; ATableCell: TdxTableCell): TdxTableProperties;
var
  AInsideBorder: Boolean;
begin
  Result := GetTablePropertiesSourceForCell(AMask, ATableCell, True, AInsideBorder);
end;

function TdxTable.GetTableStyle: TdxTableStyle;
begin
  Result := TdxDocumentModel(DocumentModel).TableStyles[StyleIndex];
end;

function TdxTable.GetTableStyleColBandSize: Integer;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableStyleColBandSize).TableStyleColBandSize;
end;

function TdxTable.GetTableStyleRowBandSize: Integer;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTableStyleRowBandSize).TableStyleRowBandSize;
end;

function TdxTable.GetTableWidth(ATable: TdxTable; AColumnWidth: Integer;
  AMatchHorizontalTableIndentsToTextEdge: Boolean): Integer;
var
  ALeftMargin, ARightMargin: TdxMarginUnitBase;
  ALeftMarginValue, ARightMarginValue, ATableWidth: Integer;
begin
  ALeftMargin := ATable.Rows.First.Cells.First.GetActualLeftMargin;
  ARightMargin := ATable.Rows.First.Cells.Last.GetActualRightMargin;

  ALeftMarginValue := GetLayoutMarginValue(ALeftMargin);
  ARightMarginValue := GetLayoutMarginValue(ARightMargin);
  ATableWidth := AColumnWidth;
  if not AMatchHorizontalTableIndentsToTextEdge then
    Inc(ATableWidth, ALeftMarginValue + ARightMarginValue);
  Result := ATableWidth;
end;

function TdxTable.GetTopMargin: TdxMarginUnitBase;
begin
  Result := GetTablePropertiesSource(TdxTablePropertiesOptions.MaskUseTopMargin).CellMargins.Top;
end;

function TdxTable.GetTotalCellsInRowConsiderGrid(ARow: TdxTableRow): Integer;
var
  I, ACells: Integer;
begin
  ACells := ARow.GridBefore;
  for I := 0 to ARow.Cells.Count - 1 do
    Inc(ACells, ARow.Cells[I].ColumnSpan);
  Inc(ACells, ARow.GridAfter);
  Result := ACells;
end;

function TdxTable.HasColumnStyle(AStyle: TdxTableStyle; AType: TdxConditionalColumnType): Boolean;
var
  AHasColumnStyle: Boolean;
begin
  if AStyle = nil then
    Exit(False);
  AHasColumnStyle := AStyle.HasColumnStyle(AType);
  if not AHasColumnStyle then
    AHasColumnStyle := HasColumnStyle(AStyle.Parent, AType);
  Result := AHasColumnStyle;
end;

function TdxTable.HasRowStyle(AStyle: TdxTableStyle; AType: TdxConditionalRowType): Boolean;
var
  AHasRowStyle: Boolean;
begin
  if AStyle = nil then
    Exit(False);
  AHasRowStyle := AStyle.HasRowStyle(AType);
  if not AHasRowStyle then
    AHasRowStyle := HasRowStyle(AStyle.Parent, AType);
  Result := AHasRowStyle;
end;

procedure TdxTable.InitializeColumnWidths(AAutoFitBehavior: TdxTableAutoFitBehaviorType; AFixedColumnWidths,
  AOuterColumnWidth: Integer; AMatchHorizontalTableIndentsToTextEdge: Boolean);
var
  AWidths: TArray<Integer>;
  I, J, ATableWidth, ATotalColumnsInTable, ACellWidth: Integer;
begin
  if AAutoFitBehavior = TdxTableAutoFitBehaviorType.AutoFitToContents then
  begin
    ForEachCell(procedure(ACell: TdxTableCell)
      begin
        ACell.PreferredWidth.&Type := TdxWidthUnitType.Auto;
        ACell.PreferredWidth.Value := 0;
      end);
    Exit;
  end;

  if AAutoFitBehavior = TdxTableAutoFitBehaviorType.AutoFitToWindow then
  begin
    DistributeHundredPercentToAllColumns;
    Exit;
  end;

  if AAutoFitBehavior = TdxTableAutoFitBehaviorType.FixedColumnWidth then
  begin
    TableProperties.PreferredWidth.Value := 0;
    TableProperties.PreferredWidth.&Type := TdxWidthUnitType.Auto;
    if IsCellWidthDefined(AFixedColumnWidths) then
      SetAllColumnsWidthsToFixedColumnWidths(AFixedColumnWidths)
    else
    begin
      ATableWidth := GetTableWidth(Self, AOuterColumnWidth, AMatchHorizontalTableIndentsToTextEdge);
      if AMatchHorizontalTableIndentsToTextEdge then
        ATableWidth := DocumentModel.ToDocumentLayoutUnitConverter.ToLayoutUnits(ATableWidth);
      ATotalColumnsInTable := FindTotalColumnsCountInTable;
      AWidths := DistributeWidthsToAllColumns(ATableWidth, ATotalColumnsInTable);
      for I := 0 to Rows.Count - 1 do
      begin
        Assert(ATotalColumnsInTable = FRows[I].Cells.Count);
        for J := 0 to Rows[I].Cells.Count - 1 do
        begin
          ACellWidth := AWidths[J];
          if AMatchHorizontalTableIndentsToTextEdge then
            ACellWidth := DocumentModel.ToDocumentLayoutUnitConverter.ToModelUnits(ACellWidth);
          ApplyWidthModelUnits(Rows[I].Cells[J], ACellWidth);
        end;
      end;
    end;
  end;
end;

function TdxTable.IsCellWidthDefined(AFixedColumnWidths: Integer): Boolean;
begin
  Result := (AFixedColumnWidths <> MinInt) and (AFixedColumnWidths <> MaxInt) and (AFixedColumnWidths <> 0);
end;

procedure TdxTable.NormalizeCellColumnSpans;
begin
  NormalizeCellColumnSpans(True);
end;

procedure TdxTable.Normalize;
begin
  Normalize(False);
end;

procedure TdxTable.Normalize(AFixLastRow: Boolean);
begin
  RemoveInvalidVerticalSpans(AFixLastRow);
  RemoveVerticalSpanFromLastRowCells;
end;

procedure TdxTable.NormalizeCellColumnSpans(ACanNormalizeWidthBeforeAndWidthAfter: Boolean);
var
  I: Integer;
  AResult: TdxTableGridIntervalList;
begin
  if Rows.Count = 0 then
    Exit;
  NormalizeRowsGridBefore(Rows, ACanNormalizeWidthBeforeAndWidthAfter);
  AResult := GetGridIntervals;
  try
    for I := FRows.Count - 1 downto 0 do
      NormalizeTableRow(FRows[I], AResult, I);
  finally
    AResult.Free;
  end;
end;

procedure TdxTable.NormalizeRowGridBefore(ARow: TdxTableRow; ACanNormalizeWidthBeforeAndWidthAfter: Boolean;
  AMinGridBefore, AMinGridAfter: Integer);
begin
  if (DocumentModel.IsUpdateLocked) and DocumentModel.DeferredChanges.IsSetContentMode then
  begin
    ARow.UnsubscribeRowPropertiesEvents;
    try
      NormalizeRowGridBeforeCore(ARow, ACanNormalizeWidthBeforeAndWidthAfter, AMinGridBefore, AMinGridAfter);
    finally
      ARow.SubscribeRowPropertiesEvents;
    end;
  end
  else
    NormalizeRowGridBeforeCore(ARow, ACanNormalizeWidthBeforeAndWidthAfter, AMinGridBefore, AMinGridAfter);
end;

procedure TdxTable.NormalizeRowGridBeforeCore(ARow: TdxTableRow; ACanNormalizeWidthBeforeAndWidthAfter: Boolean;
  AMinGridBefore, AMinGridAfter: Integer);
begin
  if AMinGridBefore <> 0 then
    ARow.GridBefore := ARow.GridBefore - AMinGridBefore;
  if (ARow.GridBefore = 0) and ((ARow.WidthBefore.&Type <> TdxWidthUnitType.Nil) or (ARow.WidthBefore.Value <> 0)) and
    ACanNormalizeWidthBeforeAndWidthAfter then
  begin
    ARow.Properties.WidthBefore.&Type := TdxWidthUnitType.Nil;
    ARow.Properties.WidthBefore.Value := 0;
  end;
  if AMinGridAfter <> 0 then
    ARow.GridAfter := ARow.GridAfter - AMinGridAfter;
  if (ARow.GridAfter = 0) and ((ARow.WidthAfter.&Type <> TdxWidthUnitType.Nil) or (ARow.WidthAfter.Value <> 0)) and
    ACanNormalizeWidthBeforeAndWidthAfter then
  begin
    ARow.Properties.WidthAfter.&Type := TdxWidthUnitType.Nil;
    ARow.Properties.WidthAfter.Value := 0;
  end;
end;

procedure TdxTable.NormalizeRows;
var
  I: Integer;
  AOldValue: Boolean;
  APieceTable: TdxPieceTable;
begin
  AOldValue := DocumentModel.ForceNotifyStructureChanged;
  DocumentModel.ForceNotifyStructureChanged := True;
  for I := FRows.Count - 1 downto 0 do
  begin
    if AllCellsHasVerticalMerge(FRows[I]) then
    begin
      if (FRows[I].Height.&Type <> TdxHeightUnitType.Auto) and (I > 0) then
        FRows[I - 1].Height.Value := FRows[I - 1].Height.Value + FRows[I].Height.Value;
      APieceTable := TdxPieceTable(PieceTable);
      if APieceTable.ShouldForceUpdateIntervals then
        APieceTable.UpdateIntervals;
      APieceTable.DeleteTableRowWithContent(FRows[I]);
    end;
  end;
  DocumentModel.ForceNotifyStructureChanged := AOldValue;
end;

procedure TdxTable.NormalizeRowsGridBefore(ARows: TdxTableRowCollection;
  ACanNormalizeWidthBeforeAndWidthAfter: Boolean);
var
  I, ACount, AMinGridBefore, AMinGridAfter: Integer;
  ARow: TdxTableRow;
begin
  ACount := ARows.Count;
  AMinGridBefore := ARows[0].GridBefore;
  AMinGridAfter := ARows[0].GridAfter;
  for I := 1 to ACount - 1 do
  begin
    ARow := ARows[I];
    AMinGridBefore := Math.Min(AMinGridBefore, ARow.GridBefore);
    AMinGridAfter := Math.Min(AMinGridAfter, ARow.GridAfter);
  end;
  if (AMinGridBefore = 0) and (AMinGridAfter = 0) then
    Exit;
  for I := 0 to ACount - 1 do
    NormalizeRowGridBefore(ARows[i], ACanNormalizeWidthBeforeAndWidthAfter, AMinGridBefore, AMinGridAfter);
end;

procedure TdxTable.NormalizeTableCellVerticalMerging;
var
  ACurrentCell, ANextRowCell: TdxTableCell;
  ACellsInFirstRow, ACellsInLastRow: TdxTableCellCollection;
  I, ARowCount, ACellsInFirstRowCount, AColumnIndex: Integer;
begin
  ARowCount := Rows.Count;
  ACellsInFirstRow := FirstRow.Cells;
  ACellsInFirstRowCount := ACellsInFirstRow.Count;
  for I := 0 to ACellsInFirstRowCount - 1 do
  begin
    ACurrentCell := ACellsInFirstRow[I];
    if ACurrentCell.VerticalMerging = TdxMergingState.Continue then
    begin
      if ARowCount = 1 then
      begin
        ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
        Continue;
      end;
      AColumnIndex := ACurrentCell.GetStartColumnIndexConsiderRowGrid;
      ANextRowCell := GetCell(FirstRow.Next, AColumnIndex);
      if ANextRowCell.VerticalMerging = TdxMergingState.Continue then
        ACurrentCell.Properties.VerticalMerging := TdxMergingState.Restart
      else
        ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
    end
    else
      if ACurrentCell.VerticalMerging = TdxMergingState.Restart then
        if ARowCount = 1 then
          ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
  end;
  if ARowCount = 1 then
    Exit;

  ACellsInLastRow := LastRow.Cells;
  for I := 0 to ACellsInLastRow.Count - 1 do
  begin
    ACurrentCell := ACellsInLastRow[I];
    if ACurrentCell.VerticalMerging = TdxMergingState.Restart then
      ACurrentCell.Properties.VerticalMerging := TdxMergingState.None;
  end;
end;

procedure TdxTable.NormalizeTableGrid;
var
  ACurrentRow: TdxTableRow;
  I, AGridAfterDelta, ACurrentEndColumnIndex, AMaxEndColumnIndex, ARowsCount: Integer;
begin
  AMaxEndColumnIndex := -1;
  ARowsCount := Rows.Count;
  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := Rows[I];
    ACurrentEndColumnIndex := GetEndColumnIndex(ACurrentRow.LastCell) + ACurrentRow.GridAfter;
    AMaxEndColumnIndex := Max(AMaxEndColumnIndex, ACurrentEndColumnIndex);
  end;

  for I := 0 to ARowsCount - 1 do
  begin
    ACurrentRow := Rows[I];
    ACurrentEndColumnIndex := GetEndColumnIndex(ACurrentRow.LastCell) + ACurrentRow.GridAfter;
    AGridAfterDelta := AMaxEndColumnIndex - ACurrentEndColumnIndex;
    if AGridAfterDelta <> 0 then
      ACurrentRow.GridAfter := ACurrentRow.GridAfter + AGridAfterDelta;
  end;
end;

procedure TdxTable.NormalizeTableRow(ARow: TdxTableRow; AGridIntervals: TdxTableGridIntervalList;
  ARowIndex: Integer);
begin
  if DocumentModel.IsUpdateLocked and DocumentModel.DeferredChanges.IsSetContentMode then
  begin
    ARow.UnsubscribeRowPropertiesEvents;
    try
      NormalizeTableRowCore(ARow, AGridIntervals, ARowIndex);
    finally
      ARow.SubscribeRowPropertiesEvents;
    end;
  end
  else
    NormalizeTableRowCore(ARow, AGridIntervals, ARowIndex);
end;

procedure TdxTable.NormalizeTableRowCore(ARow: TdxTableRow; AGridIntervals: TdxTableGridIntervalList;
  ARowIndex: Integer);
var
  ACells: TdxTableCellCollection;
  AIntervalIterator: TdxTableGridIntervalIterator;
  I, ASpan: Integer;
  AShouldDeleteRow: Boolean;
  ACell: TdxTableCell;
begin
  ACells := ARow.Cells;
  AIntervalIterator := TdxTableGridIntervalIterator.Create(TdxTableGridIntervalList.Create(AGridIntervals), MaxInt);
  try
    ASpan := CalculateNewSpan(ARow.GridBefore, AIntervalIterator);
    if ARow.GridBefore <> ASpan then
      ARow.GridBefore := ASpan;
    AShouldDeleteRow := True;
    for I := 0 to ACells.Count - 1 do
    begin
      ACell := ACells[I];
      ASpan := CalculateNewSpan(ACell.ColumnSpan, AIntervalIterator);
      if ACell.ColumnSpan <> ASpan then
        ACell.ColumnSpan := ASpan;
      AShouldDeleteRow := AShouldDeleteRow and (ACell.VerticalMerging = TdxMergingState.Continue);
    end;
    ASpan := CalculateNewSpan(ARow.GridAfter, AIntervalIterator);
    if ARow.GridAfter <> ASpan then
      ARow.GridAfter := ASpan;
  finally
    AIntervalIterator.Free;
  end;
end;

procedure TdxTable.OnPropertiesObtainAffectedRangeCore(E: TdxObtainAffectedRangeEventArgs; AFirstRowIndex: Integer);
var
  AFirstRow, ALastRow: TdxTableRow;
  AFirstCell, ALastCell: TdxTableCell;
  AParagraphs: TdxParagraphBaseCollection;
  AFormattingController: TdxTableConditionalFormattingController;
  AStartParagraphIndex, AEndParagraphIndex, AParagraphCount: TdxParagraphIndex;
begin
  AFormattingController.Init(Self);
  AFormattingController.ResetCachedProperties(AFirstRowIndex);
  AParagraphs := PieceTable.Paragraphs;
  AFirstRow := Rows.First;
  ALastRow := Rows.Last;
  if (AFirstRow = nil) or (ALastRow = nil) then
    Exit;
  AFirstCell := AFirstRow.FirstCell;
  ALastCell := ALastRow.LastCell;
  if (AFirstCell = nil) or (ALastCell = nil) then
    Exit;
  AStartParagraphIndex := AFirstCell.StartParagraphIndex;
  AEndParagraphIndex := ALastCell.EndParagraphIndex;
  AParagraphCount := AParagraphs.Count;
  if (AStartParagraphIndex < 0) or (AStartParagraphIndex >= AParagraphCount) or
    (AEndParagraphIndex < 0) or (AEndParagraphIndex >= AParagraphCount) then
    Exit;

  E.Start := AParagraphs[AFirstCell.StartParagraphIndex].FirstRunIndex;
  E.&End := AParagraphs[ALastCell.EndParagraphIndex].LastRunIndex;
end;

procedure TdxTable.OnPropertiesObtainAffectedRangeCore(E: TdxObtainAffectedRangeEventArgs);
begin
  OnPropertiesObtainAffectedRangeCore(E, 0);
end;

procedure TdxTable.OnTablePropertiesObtainAffectedRange(ASender: TObject; E: TdxObtainAffectedRangeEventArgs);
begin
  OnPropertiesObtainAffectedRangeCore(E);
end;

procedure TdxTable.RecalNestedLevel;
begin
  if FParentCell <> nil then
    FNestedLevel := FParentCell.Table.NestedLevel + 1
  else
    FNestedLevel := 0;
end;

procedure TdxTable.RemoveInvalidVarticalSpansFromTableWithOneRow(ARows: TdxTableRowCollection);
var
  ACell: TdxTableCell;
  ACellIndex: Integer;
  ACells: TdxTableCellCollection;
  AVerticalMerging: TdxMergingState;
begin
  ACells := ARows[0].Cells;
  for ACellIndex := 0 to ACells.Count - 1 do
  begin
    ACell := ACells[ACellIndex];
    AVerticalMerging := ACell.VerticalMerging;
    if AVerticalMerging = TdxMergingState.Continue then
      ACell.VerticalMerging := TdxMergingState.None;
  end;
end;

procedure TdxTable.RemoveInvalidVerticalSpans(AFixLastRow: Boolean);
var
  ARows: TdxTableRowCollection;
  ACells: TdxTableCellCollection;
  ACell, ALowerCell, AUpperCell: TdxTableCell;
  AVerticalMerging: TdxMergingState;
  ARowCount, ACount, ARowIndex, ACellCount, ACellIndex, AColumnIndex: Integer;
begin
  ARows := Rows;
  ARowCount := ARows.Count;
  if AFixLastRow then
    ACount := ARowCount
  else
    ACount := ARowCount - 1;
  if ARowCount = 1 then
  begin
    RemoveInvalidVarticalSpansFromTableWithOneRow(ARows);
    Exit;
  end;
  for ARowIndex := 0 to ACount - 1 do
  begin
    ACells := ARows[ARowIndex].Cells;
    ACellCount := ACells.Count;
    for ACellIndex := 0 to ACellCount - 1 do
    begin
      ACell := ACells[ACellIndex];
      AVerticalMerging := ACell.VerticalMerging;
      if AVerticalMerging = TdxMergingState.None then
        Continue;

      AColumnIndex := GetCellColumnIndexConsiderRowGrid(ACell);
      if AVerticalMerging = TdxMergingState.Restart then
      begin
        if ARowIndex = ARowCount - 1 then
          ACell.VerticalMerging := TdxMergingState.None;
        ALowerCell := GetCell(ARowIndex + 1, AColumnIndex);
        if ALowerCell.VerticalMerging <> TdxMergingState.Continue then
          ACell.VerticalMerging := TdxMergingState.None;
      end
      else
        if ACell.VerticalMerging = TdxMergingState.Continue then
        begin
          if ARowIndex = 0 then
          begin
            ACell.VerticalMerging := TdxMergingState.None;
            Continue;
          end;

          AUpperCell := GetCell(ARowIndex - 1, AColumnIndex);
          if (AUpperCell = nil) or (AUpperCell.VerticalMerging = TdxMergingState.None) then
          begin
            if ARowIndex = ARowCount - 1 then
              ACell.VerticalMerging := TdxMergingState.None
            else
            begin
              ALowerCell := GetCell(ARowIndex + 1, AColumnIndex);
              if ALowerCell.VerticalMerging = TdxMergingState.Continue then
                ACell.VerticalMerging := TdxMergingState.Restart
              else
                ACell.VerticalMerging := TdxMergingState.None;
            end;
          end;
      end;
    end;
  end;
end;

procedure TdxTable.RemoveVerticalSpanFromLastRowCells;
var
  I: Integer;
  ACell: TdxTableCell;
begin
  if Rows.Count = 0 then
    Exit;
  for I := 0 to LastRow.Cells.Count - 1 do
  begin
    ACell := LastRow.Cells[I];
    if ACell.VerticalMerging = TdxMergingState.Restart then
      ACell.VerticalMerging := TdxMergingState.None;
  end;
end;

procedure TdxTable.ResetCachedLayoutInfo;
begin
  FreeAndNil(FCachedTableLayoutInfo);
end;

procedure TdxTable.SetAllColumnsWidthsToFixedColumnWidths(AFixedColumnWidths: Integer);
var
  AApplyWidthPercentBased: TdxTableCellProcessorDelegate;
begin
  AApplyWidthPercentBased := procedure(ACell: TdxTableCell)
    begin
      ApplyWidthModelUnits(ACell, AFixedColumnWidths);
    end;
  ForEachCell(AApplyWidthPercentBased);
end;

procedure TdxTable.SetBackgroundColor(const Value: TdxAlphaColor);
begin
  TableProperties.BackgroundColor := Value;
end;

procedure TdxTable.SetIndex(const Value: Integer);
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('Index', Value);
  FIndex := Value;
end;

procedure TdxTable.SetIsTableOverlap(const Value: Boolean);
begin
  TableProperties.IsTableOverlap := Value;
end;

procedure TdxTable.SetParentCell(ANewParentCell: TdxTableCell);
begin
  FParentCell := ANewParentCell;
  RecalNestedLevel;
end;

procedure TdxTable.SetStyleIndex(const Value: Integer);
var
  AItem: TdxChangeTableStyleIndexHistoryItem;
begin
  if Value < 0 then
    TdxRichEditExceptions.ThrowArgumentException('TableStyleIndex', Value);

  if FStyleIndex = Value then
    Exit;
  DocumentModel.BeginUpdate;
  try
    AItem := TdxChangeTableStyleIndexHistoryItem.Create(PieceTable, &Index, FStyleIndex, Value);
    DocumentModel.History.Add(AItem);
    AItem.Execute;
  finally
    DocumentModel.EndUpdate;
  end;
  FStyleIndex := Value;
end;

procedure TdxTable.SetTableAlignment(const Value: TdxTableRowAlignment);
begin
  TableProperties.TableAlignment := Value;
end;

procedure TdxTable.SetTableLayout(const Value: TdxTableLayoutType);
begin
  TableProperties.TableLayout := Value;
end;

procedure TdxTable.SetTableLook(const Value: TdxTableLookTypes);
begin
  TableProperties.TableLook := Value;
end;

procedure TdxTable.SetTableStyleColBandSize(const Value: Integer);
begin
  TableProperties.TableStyleColBandSize := Value;
end;

procedure TdxTable.SetTableStyleIndexCore(ANewStyleIndex: Integer);
var
  I: TdxParagraphIndex;
  AStart, AEnd: TdxRunIndex;
  AController: TdxTableConditionalFormattingController;
begin
  FStyleIndex := ANewStyleIndex;
  if (FirstRow = nil) or (FirstRow.FirstCell = nil) or (LastRow = nil) or (LastRow.LastCell = nil) or
    (FirstRow.FirstCell.StartParagraphIndex < 0) then
    Exit;
  for I := FirstRow.FirstCell.StartParagraphIndex to LastRow.LastCell.EndParagraphIndex do
    FPieceTable.Paragraphs[I].ResetCachedIndices(TdxResetFormattingCacheType.All);
  AController.Init(Self);
  AController.ResetCachedProperties(0);

  if StartParagraphIndex >= 0 then
  begin
    AStart := PieceTable.Paragraphs[FirstRow.FirstCell.StartParagraphIndex].FirstRunIndex;
    AEnd := PieceTable.Paragraphs[LastRow.LastCell.EndParagraphIndex].LastRunIndex;
    PieceTable.ApplyChangesCore(TdxTableChangeActionCalculator.CalculateChangeActions(TdxTableChangeType.TableStyle), AStart, AEnd);
  end;
end;

procedure TdxTable.SetTableStyleRowBandSize(const Value: Integer);
begin
  TableProperties.TableStyleRowBandSize := Value;
end;

procedure TdxTable.SubscribeTablePropertiesEvents;
begin
  TableProperties.OnObtainAffectedRange.Add(OnTablePropertiesObtainAffectedRange);
end;

function TdxTable.GetTablePropertiesSource(AMask, ARowIndex, AColumnIndex: Integer; AIsBorderCell: Boolean;
  out AInsideBorder: Boolean): TdxTableProperties;
begin
  Result := GetTablePropertiesSource(AMask, GetRowTypeByIndex(ARowIndex), GetColumnTypeByIndex(ARowIndex, AColumnIndex),
    AIsBorderCell, AInsideBorder);
end;

function TdxTable.GetTablePropertiesSource(AMask: Integer; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType): TdxTableProperties;
var
  AInsideBorder: Boolean;
begin
  Result := GetTablePropertiesSource(AMask, ARowType, AColumnType, True, AInsideBorder);
end;

function TdxTable.GetTablePropertiesSource(AMask: Integer; ARowType: TdxConditionalRowType;
  AColumnType: TdxConditionalColumnType; AIsBorderCell: Boolean; out AInsideBorder: Boolean): TdxTableProperties;
var
  ASource: TdxTableProperties;
begin
  if TableProperties.GetUse(TdxTablePropertiesOptions.OuterOrInside(AMask, AIsBorderCell)) then
  begin
    AInsideBorder := not AIsBorderCell;
    Exit(TableProperties);
  end;
  ASource := TableStyle.GetTableProperties(AMask, ARowType, AColumnType, AIsBorderCell, AInsideBorder);
  if ASource <> nil then
    Exit(ASource);
  AInsideBorder := not AIsBorderCell;
  Result := (DocumentModel as IdxDefaultTablePropertiesContainer).DefaultTableProperties;
end;

{ TdxHorizontalCellBordersInfo }

constructor TdxHorizontalCellBordersInfo.Create(AAboveCell, ABelowCell: TdxTableCell; ABorder: TdxBorderBase;
  AStartColumnIndex, AEndColumnIndex: Integer);
begin
  if AEndColumnIndex < AStartColumnIndex then
    TdxRichEditExceptions.ThrowArgumentException('endColumnIndex', AEndColumnIndex);
  FBorder := ABorder;
  FStartColumnIndex := AStartColumnIndex;
  FColumnSpan := AEndColumnIndex - AStartColumnIndex + 1;
  FAboveCell := AAboveCell;
  FBelowCell := ABelowCell;
end;

function TdxHorizontalCellBordersInfo.GetEndColumnIndex: Integer;
begin
  Result := StartColumnIndex + ColumnSpan - 1;
end;

{ TdxHorizontalCellBordersInfoList }

function TdxHorizontalCellBordersInfoList.Clone: TdxHorizontalCellBordersInfoList;
var
  I: Integer;
begin
  if Self = nil then
    Exit(nil);
  Result := TdxHorizontalCellBordersInfoList.Create;
  for I := 0 to Count - 1 do
    Result.Add(Self[I]);
end;

{ TdxVerticalBorderPositions }

constructor TdxVerticalBorderPositions.Create(AInitialPositions, AAlignedPositions: TdxLayoutUnitSortedList);
begin
  inherited Create;
  Assert(AInitialPositions <> nil);
  Assert(AAlignedPositions <> nil);
  FInitialPositions := AInitialPositions;
  FAlignedPositions := AAlignedPositions;
end;

destructor TdxVerticalBorderPositions.Destroy;
begin
  FAlignedPositions.Free;
  FInitialPositions.Free;
  inherited Destroy;
end;

{ TdxSelectedTableStructureBase }

constructor TdxSelectedTableStructureBase.Create(AOriginalStartLogPosition: TdxDocumentLogPosition);
begin
  inherited Create;
  FOriginalStartLogPosition := AOriginalStartLogPosition;
end;

{ TdxTableChangeActionCalculator }

class function TdxTableChangeActionCalculator.CalculateChangeActions(
  AChange: TdxTableChangeType): TdxDocumentModelChangeActions;
const
  TableChangeActionsMap: array[TdxTableChangeType] of TdxDocumentModelChangeActions = (
    [],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler],
    [TdxDocumentModelChangeAction.Redraw,
     TdxDocumentModelChangeAction.ResetPrimaryLayout,
     TdxDocumentModelChangeAction.ResetSecondaryLayout,
     TdxDocumentModelChangeAction.ResetRuler]
  );
begin
  Result := TableChangeActionsMap[AChange];
end;

end.

{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressSpreadSheet                                       }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSPREADSHEET CONTROL AND ALL    }
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

unit dxSpreadSheetCoreHelpers;

{$I cxVer.Inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections, SysUtils, Clipbrd, cxClasses, cxGraphics,
  dxCore, cxVariants, dxHashUtils, dxSpreadSheetCore, dxSpreadSheetClasses, dxSpreadSheetTypes, dxSpreadSheetStrs,
  dxSpreadSheetGraphics, dxSpreadSheetHyperlinks, dxSpreadSheetClipboardFormats, dxCoreClasses, dxSpreadSheetCoreStyles,
  dxSpreadSheetCoreFormulas, dxSpreadSheetStyles, Math, dxSpreadSheetUtils;

const
  NeighborBorderSide: array[TcxBorder] of TcxBorder = (bRight, bBottom, bLeft, bTop);

type

  { TdxSpreadSheetCustomTableViewHelper }

  TdxSpreadSheetCustomTableViewHelper = class
  strict private
    FView: TdxSpreadSheetTableView;

    function GetColumns: TdxSpreadSheetTableColumns; inline;
    function GetHistory: TdxSpreadSheetHistory;
    function GetRows: TdxSpreadSheetTableRows; inline;
    function GetSpreadSheet: TdxCustomSpreadSheet; inline;
  protected
    procedure Changed;
    procedure CheckProtection(const AArea: TRect); overload;
    procedure CheckProtection; overload;
    procedure ClearCells(const AArea, AExcludedArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions); overload;
    procedure ClearCells(const AArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions); overload;
    //
    procedure EndAction; virtual;
    procedure StartAction; virtual;
  public
    constructor Create(AView: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;
    //
    property Columns: TdxSpreadSheetTableColumns read GetColumns;
    property History: TdxSpreadSheetHistory read GetHistory;
    property Rows: TdxSpreadSheetTableRows read GetRows;
    property SpreadSheet: TdxCustomSpreadSheet read GetSpreadSheet;
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetTableViewCustomCellsModificationHelper }

  TdxSpreadSheetTableViewCustomCellsModificationHelper = class(TdxSpreadSheetCustomTableViewHelper)
  strict private
    FFixedContainerAnchorTypes: TDictionary<TdxSpreadSheetContainer, TdxSpreadSheetContainerAnchorType>;
    FFixedContainers: TDictionary<TdxSpreadSheetContainer, TRect>;

    function GetIsHistoryAction: Boolean;
  protected
    function IsAnchorPointInArea(const AArea: TRect; AAnchorPoint: TdxSpreadSheetContainerAnchorPoint): Boolean;
    function IsDeletion: Boolean; virtual;

    procedure CreateIndependentConditionalFormattingAreasForMovedArea(const AMovingArea: TRect);
    procedure MoveConditionalFormattingAreas(const AMovingArea: TRect; ADeltaX, ADeltaY: Integer);

    procedure Move(var ALeftIndex, ARightIndex: Integer; AMovingAreaStartIndex, ADelta: Integer); inline;
    function MoveArea(var AArea: TRect; const AMovingArea: TRect; ADeltaX, ADeltaY: Integer): Boolean; inline;

    function NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean; virtual;
    procedure StoreContainersPosition;
    procedure RestoreContainerPosition(AContainer: TdxSpreadSheetContainer;
      const AContainerBounds: TRect; AAnchorType: TdxSpreadSheetContainerAnchorType); virtual;
    procedure RestoreContainersPosition;
  public
    constructor Create(AView: TdxSpreadSheetTableView); override;
    destructor Destroy; override;

    property FixedContainerAnchorTypes: TDictionary<TdxSpreadSheetContainer, TdxSpreadSheetContainerAnchorType> read FFixedContainerAnchorTypes;
    property FixedContainers: TDictionary<TdxSpreadSheetContainer, TRect> read FFixedContainers;
    property IsHistoryAction: Boolean read GetIsHistoryAction;
  end;

  { TdxSpreadSheetTableViewMoveCellsModificationHelper }

  TdxSpreadSheetTableViewMoveCellsModificationHelper = class(TdxSpreadSheetTableViewCustomCellsModificationHelper)
  strict private
    FSourceArea: TRect;
    FTargetArea: TRect;
  protected
    function NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean; override;
    procedure ProcessCore; virtual;
    procedure RestoreContainerPosition(AContainer: TdxSpreadSheetContainer;
      const AContainerBounds: TRect; AAnchorType: TdxSpreadSheetContainerAnchorType); override;

    procedure StartAction; override;
    procedure EndAction; override;
  public
    procedure Process(const AArea: TRect; const ATargetPoint: TPoint);
    //
    property SourceArea: TRect read FSourceArea;
    property TargetArea: TRect read FTargetArea;
  end;

  { TdxSpreadSheetTableViewCellsModificationHelper }

  TdxSpreadSheetTableViewCellsModificationHelper = class(TdxSpreadSheetTableViewCustomCellsModificationHelper)
  strict private
    FArea: TRect;
    FIsHorizontalShifting: Boolean;
    FProcessedArea: TRect;
    FMergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution;
    FModification: TdxSpreadSheetCellsModification;
    FMovableArea: TRect;
  protected
    procedure DoCommand(const AArea: TRect; AModification: TdxSpreadSheetCellsModification; AIsHorizontal: Boolean);
    procedure DoProcess(const AArea: TRect); virtual; abstract;
    procedure Initialize(var AArea: TRect);

    procedure BeforeShiftCells; virtual;
    procedure CheckIsAreaEmpty(AItems: TdxSpreadSheetTableItems; const AArea: TRect);
    procedure CreateBlankCellsForStyledItems(AItems, AOppositeItems: TdxSpreadSheetTableItems; const AArea: TRect; AShiftDelta: Integer);
    function IsAreaAffected(const AArea: TRect; ADeltaX, ADeltaY: Integer; var ANewArea: TRect): Boolean;
    function IsAreaEmpty(AItems: TdxSpreadSheetTableItems; const AArea: TRect): Boolean;
    function NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean; override;

    procedure CheckMergedCells;
    function HasAffectedMergedCells: Boolean;
    procedure MoveGroups(AGroups: TdxSpreadSheetTableItemGroup; AStartFromIndex, ADelta: Integer);
    procedure MovePrintAreas(const AMovingArea: TRect; ADeltaX, ADeltaY: Integer);
    procedure TransformAffectedObjectArea(ADeltaX, ADeltaY: Integer);
    procedure UnmergeAllAffectedCells;

    procedure ShiftCellsHorizontally(AStartFromIndex, AStartRowIndex, AFinishRowIndex, ADelta: Integer);
    procedure ShiftCellsVertically(AStartFromIndex, AStartColumnIndex, AFinishColumnIndex, ADelta: Integer);
    procedure ShiftColumns(AStartFromIndex, ADelta: Integer);
    procedure ShiftIndexes(AList: TdxDynamicItemList; AStartFromIndex, ADelta: Integer);
    procedure ShiftRows(AStartFromIndex, ADelta: Integer);

    property ProcessedArea: TRect read FProcessedArea;
  public
    procedure Process(AArea: TRect);
    //
    property IsHorizontalShifting: Boolean read FIsHorizontalShifting;
    property MergedCellsConflictResolution: TdxSpreadSheetTableViewMergedCellsConflictResolution read FMergedCellsConflictResolution write FMergedCellsConflictResolution;
    property Modification: TdxSpreadSheetCellsModification read FModification write FModification;
    property MovableArea: TRect read FMovableArea;
  end;

  { TdxSpreadSheetTableViewDeleteCellsHelper }

  TdxSpreadSheetTableViewDeleteCellsHelper = class(TdxSpreadSheetTableViewCellsModificationHelper)
  strict private
    procedure MoveAnchor(AAnchor: TdxSpreadSheetContainerAnchorPoint; const AArea: TRect; AMoveForward: Boolean);
  protected
    procedure CheckContainer(const AArea: TRect; AContainer: TdxSpreadSheetContainer);
    procedure CheckContainers(const AArea: TRect);
    procedure DeleteCellsInArea(const AArea: TRect);
    procedure DeleteItems(AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer);
    procedure DoProcess(const AArea: TRect); override;
    function IsDeletion: Boolean; override;
    //
    procedure EndAction; override;
    procedure StartAction; override;
  end;

  { TdxSpreadSheetTableViewInsertCellsHelper }

  TdxSpreadSheetTableViewInsertCellsHelper = class(TdxSpreadSheetTableViewCellsModificationHelper)
  protected
    procedure BeforeShiftCells; override;
    procedure CalculateStyle(ANewStyle, ALeftSourceStyle, ARightSourceStyle: TdxSpreadSheetCellStyle); virtual;
    procedure CalculateStyleForNewCells(ATarget, ALeftSource, ARightSource: TdxSpreadSheetTableItem;
      AOppositeItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer);
    procedure CalculateStyleForNewItems(const AArea: TRect; AItems, AOppositeItems: TdxSpreadSheetTableItems);
    procedure DoProcess(const AArea: TRect); override;

    function GetCellStyle(ASource: TdxSpreadSheetTableItem; ACellIndex: Integer;
      AOppositeItems: TdxSpreadSheetTableItems): TdxSpreadSheetCellStyle; inline;
    function GetStyle(AItem: TdxSpreadSheetTableItem): TdxSpreadSheetCellStyle; inline;
    //
    procedure EndAction; override;
    procedure StartAction; override;
  end;

  { TdxSpreadSheetTableViewMergeCellStyleHelper }

  TdxSpreadSheetTableViewMergeCellStyleHelper = class(TdxSpreadSheetCustomTableViewHelper)
  strict private
    FBorderColors: array[TcxBorder] of TColor;
    FBorderStyles: array[TcxBorder] of TdxSpreadSheetCellBorderStyle;
    FMergeCell: TdxSpreadSheetMergedCell;
    FNonBlankCell: TdxSpreadSheetCell;

    function GetActiveCell: TdxSpreadSheetCell; inline;
    function GetDefaultStyle: TdxSpreadSheetCellStyle; inline;
    function GetStylePattern: TdxSpreadSheetCellStyle;
  protected
    procedure CalculateBorder(ABorder: TcxBorder; AItem: TdxSpreadSheetTableItem;
      AStartIndex, AFinishIndex: Integer; out ABorderColor: TColor; out ABorderStyle: TdxSpreadSheetCellBorderStyle);
    procedure CalculateCellStyles;
    procedure CalculateCellStylesInCustomArea(const AArea: TRect; AStylePattern: TdxSpreadSheetCellStyle);
    procedure CalculateCellStylesInEntireColumnOrRow(AItems: TdxSpreadSheetTableItems;
      AStartIndex, AFinishIndex: Integer; AStylePattern: TdxSpreadSheetCellStyle);
    procedure CalculateCellValues;
    procedure DoCalculate;
    procedure SetupActiveCellValue;
    //
    property ActiveCell: TdxSpreadSheetCell read GetActiveCell;
    property DefaultStyle: TdxSpreadSheetCellStyle read GetDefaultStyle;
    property MergeCell: TdxSpreadSheetMergedCell read FMergeCell;
    property NonBlankCell: TdxSpreadSheetCell read FNonBlankCell;
  public
    constructor Create(AMergeCell: TdxSpreadSheetMergedCell); reintroduce;
    class procedure Calculate(AMergeCell: TdxSpreadSheetMergedCell);
  end;

  { TdxSpreadSheetTableViewSortingHelper }

  TdxSpreadSheetTableViewSortingHelper = class(TdxSpreadSheetCustomTableViewHelper)
  protected
    Area: TRect;
    DefaultSorting: Boolean;
    SortOrder: TdxSortOrder;
    SortOrders: TList<TdxSortOrder>;
    SortIndexes: TList<Integer>;
    function Compare(AItem1, AItem2: Pointer): Integer;
    function CompareCells(AData1, AData2: TdxSpreadSheetCellData): Integer;
    function CompareValues(const AValue1, AValue2: Variant): Integer;
    procedure DoSort(AFromIndex, AToIndex, ASortIndex: Integer);
    procedure ExchangeColumnValues(AColumn1, AColumn2: Integer);
    procedure ExchangeRowValues(ARow1, ARow2: Integer);
    function GetCellData(ARow, AColumn: Integer): TdxSpreadSheetCellData;
    function Initialize(AIndex1, AIndex2: Integer; const ASortOrders: array of TdxSortOrder; const AIndexes: array of Integer): Boolean;
    function IsColumnEmpty(AIndex: Integer): Boolean;
    function IsRowEmpty(AIndex: Integer): Boolean;
    procedure SortValues(const AValues: TcxObjectList; L, H: Integer);
    procedure SwapValues(AValue1, AValue2: TdxSpreadSheetCellData);
    function ValidateArea(const AArea: TRect): Boolean;
  public
    constructor Create(AView: TdxSpreadSheetTableView); override;
    destructor Destroy; override;
    procedure SortByColumnValues(const AArea: TRect; const ASortOrders: array of TdxSortOrder; const AColumnIndexes: array of Integer);
    procedure SortByRowValues(const AArea: TRect; const ASortOrders: array of TdxSortOrder; const ARowIndexes: array of Integer);
  end;

  { TdxSpreadSheetTableViewEnumCellStylesHelper }

  TdxSpreadSheetTableViewEnumCellStylesHelper = class(TObject)
  strict private type

    TCache = class
    strict private
      FCrossStyles: TObjectDictionary<TdxSpreadSheetCellStyleHandle, TList<TdxSpreadSheetCellStyleHandle>>;
      FStyles: TList<TdxSpreadSheetCellStyleHandle>;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(ARowStyle, AColumnStyle: TdxSpreadSheetCellStyleHandle): Boolean; overload;
      function Add(AStyle: TdxSpreadSheetCellStyleHandle): Boolean; overload;
      procedure Clear;
    end;

  strict private
    FCache: TCache;
    FEnumDefaultStyles: Boolean;
    FHasEntireRowWithDefaultStyle: Boolean;
    FSheet: TdxSpreadSheetTableView;

    procedure EnumColumnStyles(AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
    procedure EnumDefaultCellsStyles(const R: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
    procedure EnumRowStyles(AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
  public
    constructor Create(ASheet: TdxSpreadSheetTableView); virtual;
    destructor Destroy; override;
    procedure PrepareToSave(AArea: TRect);
    procedure ProcessArea(const AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
    //
    property EnumDefaultStyles: Boolean read FEnumDefaultStyles write FEnumDefaultStyles;
    property Sheet: TdxSpreadSheetTableView read FSheet;
  end;

  { TdxSpreadSheetTableViewPackHelper }

  TdxSpreadSheetTableViewPackHelper = class(TdxSpreadSheetCustomTableViewHelper)
  strict private
    FHasUnusedCells: Boolean;
  protected
    procedure RemoveUnusedCells;
    procedure RemoveUnusedItems(AItems: TdxSpreadSheetTableItems);
  public
    procedure Pack;
  end;

  { TdxSpreadSheetPasteCellDataFromStreamHelper }

  TdxSpreadSheetPasteCellDataFromStreamHelper = class(TdxSpreadSheetBinaryClipboardFormatData)
  protected
    procedure RestoreCells(AView: TdxSpreadSheetTableView; AStyles: TList<TdxSpreadSheetCellStyleHandle>;
      const AArea: TRect; AOptions: TdxSpreadSheetClipboardPasteOptions); override;
  public
    constructor Create(AStream: TStream); reintroduce;
    procedure Copy(const AArea: TRect; AContainer: TdxSpreadSheetContainer;
      AMode: TdxSpreadSheetClipboardCopyMode; AView: TdxSpreadSheetTableView); override;
  end;

  { TdxSpreadSheetDefinedNameHelper }

  TdxSpreadSheetDefinedNameEnumProc = reference to procedure (ADefinedName: TdxSpreadSheetDefinedName);

  TdxSpreadSheetDefinedNameHelper = class
  public
    class procedure Enum(AView: TdxSpreadSheetCustomView; AProc: TdxSpreadSheetDefinedNameEnumProc);
    class function FindByArea(ADefinedNames: TdxSpreadSheetDefinedNames;
      AView: TdxSpreadSheetCustomView; const AArea: TRect; out ADefinedName: TdxSpreadSheetDefinedName): Boolean;
    class function GetBoundingArea(ADefinedName: TdxSpreadSheetDefinedName): TRect; overload;
    class function GetBoundingArea(ADefinedName: TdxSpreadSheetDefinedName; out AView: TdxSpreadSheetCustomView): TRect; overload;
    class function GetUsedAreas(ADefinedName: TdxSpreadSheetDefinedName; out AView: TdxSpreadSheetCustomView): TdxRectList;
    class function IsInternalDefinedName(ADefinedName: TdxSpreadSheetDefinedName): Boolean;
  end;

  { TdxSpreadSheetExpression }

  TdxSpreadSheetExpression = class(TdxSpreadSheetCustomFormula)
  strict private
    FController: TdxSpreadSheetCustomFormulaController;
    FView: TdxSpreadSheetTableView;
  protected
    function GetController: TdxSpreadSheetCustomFormulaController; override;
    function GetView: TObject; override;
  public
    constructor Create(AView: TdxSpreadSheetTableView); reintroduce;
    destructor Destroy; override;
    procedure Calculate(const AExpression: string);
  end;

const
  dxSpreadSheetTableViewClearCellsOptionsAll = [ccoValues, ccoHyperlinks, ccoComments, ccoStyles];

procedure dxSpreadSheetMergeBorderStyle(ASide: TcxBorder; AStyle1: TdxSpreadSheetCellStyleHandle;
  AStyle2: TdxSpreadSheetCellStyleHandle; var AColor: TColor; var AStyle: TdxSpreadSheetCellBorderStyle); overload; inline;
procedure dxSpreadSheetMergeBorderStyle(ASide: TcxBorder; AStyle1: TdxSpreadSheetBordersHandle;
  AStyle2: TdxSpreadSheetBordersHandle; var AColor: TColor; var AStyle: TdxSpreadSheetCellBorderStyle); overload;

procedure dxSpreadSheetStoreSelectionAndScrollPosition(
  AView: TdxSpreadSheetTableView; ASelection: TdxRectList; out AFocusedCell, AScrollPosition: TPoint);
procedure dxSpreadSheetRestoreSelectionAndScrollPosition(
  AView: TdxSpreadSheetTableView; ASelection: TdxRectList; const AFocusedCell, AScrollPosition: TPoint);
implementation

uses
  Dialogs, Controls, Variants, StrUtils, cxGeometry, cxControls, dxTypeHelpers, dxSpreadSheetFormulas, cxDateUtils,
  dxSpreadSheetFormatBinary, dxGDIPlusClasses, dxCoreGraphics, dxSpreadSheetCoreHistory, dxSpreadSheetContainers,
  dxSpreadSheetConditionalFormatting, dxSpreadSheetCoreStrs, dxSpreadSheetPrinting;

type
  TdxDynamicItemListAccess = class(TdxDynamicItemList);
  TdxSpreadSheetAccess = class(TdxCustomSpreadSheet);
  TdxSpreadSheetCellAccess = class(TdxSpreadSheetCell);
  TdxSpreadSheetContainerAccess = class(TdxSpreadSheetContainer);
  TdxSpreadSheetContainersAccess = class(TdxSpreadSheetContainers);
  TdxSpreadSheetHistoryAccess = class(TdxSpreadSheetHistory);
  TdxSpreadSheetHyperlinkAccess = class(TdxSpreadSheetHyperlink);
  TdxSpreadSheetMergedCellAccess = class(TdxSpreadSheetMergedCell);
  TdxSpreadSheetTableColumnAccess = class(TdxSpreadSheetTableColumn);
  TdxSpreadSheetTableColumnsAccess = class(TdxSpreadSheetTableColumns);
  TdxSpreadSheetTableItemGroupsAccess = class(TdxSpreadSheetTableItemGroups);
  TdxSpreadSheetTableItemsAccess = class(TdxSpreadSheetTableItems);
  TdxSpreadSheetTableRowAccess = class(TdxSpreadSheetTableRow);
  TdxSpreadSheetTableRowCellsAccess = class(TdxSpreadSheetTableRowCells);
  TdxSpreadSheetTableRowsAccess = class(TdxSpreadSheetTableRows);
  TdxSpreadSheetTableViewAccess = class(TdxSpreadSheetTableView);
  TdxSpreadSheetTableViewInfoAccess = class(TdxSpreadSheetTableViewInfo);
  TdxSpreadSheetFormulaControllerAccess = class(TdxSpreadSheetFormulaController);

  { TdxSpreadSheetFakeCellStyle }

  TdxSpreadSheetFakeCellStyle = class(TdxSpreadSheetCellStyle)
  strict private
    FView: TdxSpreadSheetTableView;
  protected
    procedure CloneHandle; override;
    procedure ReplaceHandle; override;
  public
    constructor Create(AOwner: TdxSpreadSheetTableView); reintroduce;
    procedure Calculate(ARow: TdxSpreadSheetTableRow; AColumn: TdxSpreadSheetTableColumn);
    //
    property View: TdxSpreadSheetTableView read FView;
  end;

  { TdxSpreadSheetHistoryDeleteCommand }

  TdxSpreadSheetHistoryDeleteCommand = class(TdxSpreadSheetHistoryCustomCommand)
  strict private
    FArea: TRect;
    FIsHorizontal: Boolean;
    FModification: TdxSpreadSheetCellsModification;
    FView: TdxSpreadSheetTableView;
  public
    constructor Create(AView: TdxSpreadSheetTableView; const AArea: TRect;
      AModification: TdxSpreadSheetCellsModification; AIsHorizontal: Boolean);
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetHistoryInsertCommand }

  TdxSpreadSheetHistoryInsertCommand = class(TdxSpreadSheetHistoryDeleteCommand)
  public
    procedure Redo; override;
    procedure Undo; override;
  end;

  { TdxSpreadSheetExpressionController }

  TdxSpreadSheetExpressionController = class(TdxSpreadSheetCustomFormulaController)
  strict private
    FSpreadSheet: TdxCustomSpreadSheet;
  protected
    function CreateParser: TObject; override;
    function GetFormatSettings: TdxSpreadSheetFormatSettings; override;
  public
    constructor Create(ASpreadSheet: TdxCustomSpreadSheet);
  end;

procedure dxSpreadSheetStoreSelectionAndScrollPosition(
  AView: TdxSpreadSheetTableView; ASelection: TdxRectList; out AFocusedCell, AScrollPosition: TPoint);
var
  I: Integer;
begin
  for I := 0 to AView.Selection.Count - 1 do
    ASelection.Add(AView.Selection[I].Rect);
  AFocusedCell.X := AView.Selection.FocusedColumn;
  AFocusedCell.Y := AView.Selection.FocusedRow;
  AScrollPosition.X := TdxSpreadSheetTableViewAccess(AView).ViewInfo.FirstScrollableColumn;
  AScrollPosition.Y := TdxSpreadSheetTableViewAccess(AView).ViewInfo.FirstScrollableRow;
end;

procedure dxSpreadSheetRestoreSelectionAndScrollPosition(
  AView: TdxSpreadSheetTableView; ASelection: TdxRectList; const AFocusedCell, AScrollPosition: TPoint);
var
  I: Integer;
begin
  AView.Selection.Clear;
  for I := 0 to ASelection.Count - 1 do
    AView.Selection.Add(ASelection[I], [ssShift]);
  AView.Selection.SetFocused(AFocusedCell.Y, AFocusedCell.X, [ssShift]);
  TdxSpreadSheetTableViewAccess(AView).ViewInfo.FirstScrollableColumn := AScrollPosition.X;
  TdxSpreadSheetTableViewAccess(AView).ViewInfo.FirstScrollableRow := AScrollPosition.Y;
end;

procedure dxSpreadSheetMergeBorderStyle(ASide: TcxBorder; AStyle1: TdxSpreadSheetCellStyleHandle;
  AStyle2: TdxSpreadSheetCellStyleHandle; var AColor: TColor; var AStyle: TdxSpreadSheetCellBorderStyle);
begin
  dxSpreadSheetMergeBorderStyle(ASide, AStyle1.Borders, AStyle2.Borders, AColor, AStyle);
  if AStyle = sscbsDefault then
  begin
    if cxColorIsValid(AStyle1.Brush.BackgroundColor) or cxColorIsValid(AStyle2.Brush.BackgroundColor) then
      AStyle := sscbsNone;
  end;
end;

procedure dxSpreadSheetMergeBorderStyle(ASide: TcxBorder; AStyle1: TdxSpreadSheetBordersHandle;
  AStyle2: TdxSpreadSheetBordersHandle; var AColor: TColor; var AStyle: TdxSpreadSheetCellBorderStyle); overload;
var
  ABorderStyle1: TdxSpreadSheetCellBorderStyle;
  ABorderStyle2: TdxSpreadSheetCellBorderStyle;
  ADelta: Integer;
begin
  ABorderStyle1 := AStyle1.BorderStyle[ASide];
  ABorderStyle2 := AStyle2.BorderStyle[NeighborBorderSide[ASide]];

  ADelta := dxSpreadSheetBorderStyleWeights[ABorderStyle1] - dxSpreadSheetBorderStyleWeights[ABorderStyle2];
  if (ADelta < 0) or (ADelta = 0) and (AStyle1.BorderColor[ASide] = clDefault) then
  begin
    AColor := AStyle2.BorderColor[NeighborBorderSide[ASide]];
    AStyle := ABorderStyle2;
  end
  else
  begin
    AColor := AStyle1.BorderColor[ASide];
    AStyle := ABorderStyle1;
  end;
  if AStyle = sscbsDefault then
    AColor := clDefault;
end;

{ TdxSpreadSheetFakeCellStyle }

constructor TdxSpreadSheetFakeCellStyle.Create(AOwner: TdxSpreadSheetTableView);
begin
  FView := AOwner;
  inherited Create(AOwner);
  Handle := Handle.Clone;
end;

procedure TdxSpreadSheetFakeCellStyle.Calculate(ARow: TdxSpreadSheetTableRow; AColumn: TdxSpreadSheetTableColumn);
var
  ABorderColor: TColor;
  ABorderStyle: TdxSpreadSheetCellBorderStyle;
  ANeighborCellStyle: TdxSpreadSheetCellStyleHandle;
  ASide: TcxBorder;
  AViewInfo: TdxSpreadSheetTableViewInfoAccess;
begin
  AViewInfo := TdxSpreadSheetTableViewInfoAccess(TdxSpreadSheetTableViewAccess(View).ViewInfo);
  Merge(AColumn.Style, ARow.Style);
  for ASide := Low(TcxBorder) to High(TcxBorder) do
  begin
    ANeighborCellStyle := AViewInfo.GetNeighborCellStyle(ARow.Index, AColumn.Index, ASide, False, False);
    dxSpreadSheetMergeBorderStyle(ASide, Handle, ANeighborCellStyle, ABorderColor, ABorderStyle);
    Borders[ASide].Color := ABorderColor;
    Borders[ASide].Style := ABorderStyle;
  end;
end;

procedure TdxSpreadSheetFakeCellStyle.CloneHandle;
begin
  // do nothing
end;

procedure TdxSpreadSheetFakeCellStyle.ReplaceHandle;
begin
  // do nothing
end;

{ TdxSpreadSheetHistoryDeleteCommand }

constructor TdxSpreadSheetHistoryDeleteCommand.Create(AView: TdxSpreadSheetTableView; const AArea: TRect;
  AModification: TdxSpreadSheetCellsModification; AIsHorizontal: Boolean);
begin
  inherited Create;
  FView := AView;
  FArea := AArea;
  FModification := AModification;
  FIsHorizontal := AIsHorizontal;
end;

procedure TdxSpreadSheetHistoryDeleteCommand.Redo;
begin
  with TdxSpreadSheetTableViewDeleteCellsHelper.Create(FView) do
  try
    DoCommand(FArea, FModification, FIsHorizontal);
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetHistoryDeleteCommand.Undo;
begin
  with TdxSpreadSheetTableViewInsertCellsHelper.Create(FView) do
  try
    DoCommand(FArea, FModification, FIsHorizontal);
  finally
    Free;
  end;
end;

{ TdxSpreadSheetHistoryInsertCommand }

procedure TdxSpreadSheetHistoryInsertCommand.Redo;
begin
  inherited Undo;
end;

procedure TdxSpreadSheetHistoryInsertCommand.Undo;
begin
  inherited Redo;
end;

{ TdxSpreadSheetCustomTableViewHelper }

constructor TdxSpreadSheetCustomTableViewHelper.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create;
  FView := AView;
  FView.BeginUpdate;
  ShowHourglassCursor;
  StartAction;
end;

destructor TdxSpreadSheetCustomTableViewHelper.Destroy;
begin
  try
    FView.EndUpdate;
  finally
    HideHourglassCursor;
    EndAction;
    inherited Destroy;
  end;
end;

procedure TdxSpreadSheetCustomTableViewHelper.Changed;
begin
  TdxSpreadSheetTableViewAccess(View).FormulaController.UpdateAnchorsAndBounds;
  TdxSpreadSheetTableViewAccess(View).Changed;
end;

procedure TdxSpreadSheetCustomTableViewHelper.CheckProtection;
begin
  TdxSpreadSheetTableViewAccess(View).CheckProtection(cmmClear);
end;

procedure TdxSpreadSheetCustomTableViewHelper.ClearCells(
  const AArea, AExcludedArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions);
begin
  TdxSpreadSheetTableViewAccess(View).ClearCells(AArea, AExcludedArea, AOptions);
end;

procedure TdxSpreadSheetCustomTableViewHelper.ClearCells(const AArea: TRect; AOptions: TdxSpreadSheetTableViewClearCellsOptions);
begin
  TdxSpreadSheetTableViewAccess(View).ClearCells(AArea, AOptions);
end;

procedure TdxSpreadSheetCustomTableViewHelper.CheckProtection(const AArea: TRect);
begin
  TdxSpreadSheetTableViewAccess(View).CheckProtection(cmmClear, AArea);
end;

procedure TdxSpreadSheetCustomTableViewHelper.EndAction;
begin
end;

procedure TdxSpreadSheetCustomTableViewHelper.StartAction;
begin
end;

function TdxSpreadSheetCustomTableViewHelper.GetColumns: TdxSpreadSheetTableColumns;
begin
  Result := View.Columns;
end;

function TdxSpreadSheetCustomTableViewHelper.GetHistory: TdxSpreadSheetHistory;
begin
  Result := TdxSpreadSheetTableViewAccess(View).History;
end;

function TdxSpreadSheetCustomTableViewHelper.GetRows: TdxSpreadSheetTableRows;
begin
  Result := View.Rows;
end;

function TdxSpreadSheetCustomTableViewHelper.GetSpreadSheet: TdxCustomSpreadSheet;
begin
  Result := View.SpreadSheet;
end;

{ TdxSpreadSheetTableViewCustomCellsModificationHelper }

constructor TdxSpreadSheetTableViewCustomCellsModificationHelper.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create(AView);
  FFixedContainers := TDictionary<TdxSpreadSheetContainer, TRect>.Create;
  FFixedContainerAnchorTypes := TDictionary<TdxSpreadSheetContainer, TdxSpreadSheetContainerAnchorType>.Create;
end;

destructor TdxSpreadSheetTableViewCustomCellsModificationHelper.Destroy;
begin
  TdxSpreadSheetTableViewAccess(View).DimensionChanged;
  FreeAndNil(FFixedContainerAnchorTypes);
  FreeAndNil(FFixedContainers);
  inherited Destroy;
end;

function TdxSpreadSheetTableViewCustomCellsModificationHelper.IsAnchorPointInArea(
  const AArea: TRect; AAnchorPoint: TdxSpreadSheetContainerAnchorPoint): Boolean;
begin
  Result := dxSpreadSheetContains(AArea, AAnchorPoint.Cell.RowIndex, AAnchorPoint.Cell.ColumnIndex);
end;

function TdxSpreadSheetTableViewCustomCellsModificationHelper.IsDeletion: Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.Move(
  var ALeftIndex, ARightIndex: Integer; AMovingAreaStartIndex, ADelta: Integer);
begin
  if ADelta <> 0 then
  begin
    Inc(ARightIndex, ADelta);
    if (ALeftIndex > AMovingAreaStartIndex) or (ALeftIndex = AMovingAreaStartIndex) and not IsDeletion then
      ALeftIndex := Max(ALeftIndex + ADelta, 0);
  end;
end;

function TdxSpreadSheetTableViewCustomCellsModificationHelper.MoveArea(
  var AArea: TRect; const AMovingArea: TRect; ADeltaX, ADeltaY: Integer): Boolean;
var
  AMoveHorizontally: Boolean;
  AMoveVertically: Boolean;
begin
  if dxSpreadSheetIntersects(AMovingArea, AArea) then
  begin
    AMoveHorizontally := not dxSpreadSheetIsEntireRow(AArea) and (AArea.Top >= AMovingArea.Top) and (AArea.Bottom <= AMovingArea.Bottom);
    AMoveVertically := not dxSpreadSheetIsEntireColumn(AArea) and (AArea.Left >= AMovingArea.Left) and (AArea.Right <= AMovingArea.Right);
    if AMoveHorizontally then
      Move(AArea.Left, AArea.Right, AMovingArea.Left, ADeltaX);
    if AMoveVertically then
      Move(AArea.Top, AArea.Bottom, AMovingArea.Top, ADeltaY);
    Result := dxSpreadSheetIsValidArea(AArea);
  end
  else
    Result := True;
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.CreateIndependentConditionalFormattingAreasForMovedArea(const AMovingArea: TRect);

  procedure AddArea(ARule: TdxSpreadSheetCustomConditionalFormattingRule; const AArea: TRect);
  begin
    if dxSpreadSheetIsValidArea(AArea) then
      ARule.Areas.Add(AArea);
  end;

var
  AArea: TRect;
  AAreaIndex: Integer;
  AIsEditingStarted: Boolean;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ARuleArea: TRect;
  ARuleIndex: Integer;
begin
  if IsHistoryAction then Exit;

  for ARuleIndex := View.ConditionalFormatting.RuleCount - 1 downto 0 do
  begin
    AIsEditingStarted := False;
    ARule := View.ConditionalFormatting.Rules[ARuleIndex];
    for AAreaIndex := ARule.Areas.Count - 1 downto 0 do
    begin
      ARuleArea := ARule.Areas[AAreaIndex];
      if not dxSpreadSheetContains(AMovingArea, ARuleArea) and dxSpreadSheetIntersects(ARuleArea, AMovingArea, AArea) then
      begin
        if not AIsEditingStarted then
        begin
          AIsEditingStarted := True;
          History.AddCommand(TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Create(ARule));
        end;
        AddArea(ARule, Rect(ARuleArea.Left, ARuleArea.Top, AArea.Left - 1, ARuleArea.Bottom));
        AddArea(ARule, Rect(AArea.Right + 1, ARuleArea.Top, ARuleArea.Right, ARuleArea.Bottom));
        AddArea(ARule, Rect(AArea.Left, ARuleArea.Top, AArea.Right, AArea.Top - 1));
        AddArea(ARule, Rect(AArea.Left, AArea.Bottom + 1, AArea.Right, ARuleArea.Bottom));
        ARule.Areas[AAreaIndex] := AArea;
      end;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.MoveConditionalFormattingAreas(
  const AMovingArea: TRect; ADeltaX, ADeltaY: Integer);
var
  AArea: TRect;
  AAreaIndex: Integer;
  AIsEditingStarted: Boolean;
  ARule: TdxSpreadSheetCustomConditionalFormattingRule;
  ARuleIndex: Integer;
begin
  if IsHistoryAction then Exit;

  for ARuleIndex := View.ConditionalFormatting.RuleCount - 1 downto 0 do
  begin
    AIsEditingStarted := False;
    ARule := View.ConditionalFormatting.Rules[ARuleIndex];
    for AAreaIndex := ARule.Areas.Count - 1 downto 0 do
    begin
      AArea := ARule.Areas[AAreaIndex];
      if dxSpreadSheetIntersects(AArea, AMovingArea) then
      begin
        if not AIsEditingStarted then
        begin
          AIsEditingStarted := True;
          History.AddCommand(TdxSpreadSheetHistoryChangeConditionalFormattingRuleCommand.Create(ARule));
        end;
        if MoveArea(AArea, AMovingArea, ADeltaX, ADeltaY) then
          ARule.Areas[AAreaIndex] := dxSpreadSheetGetRealArea(AArea)
        else
          ARule.Areas.Delete(AAreaIndex);
      end;
    end;
    if ARule.Areas.Count = 0 then
      ARule.Free;
  end;
end;

function TdxSpreadSheetTableViewCustomCellsModificationHelper.NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean;
begin
  Result := False;
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.StoreContainersPosition;
var
  AContainer: TdxSpreadSheetContainerAccess;
  I: Integer;
begin
  for I := 0 to View.Containers.Count - 1 do
  begin
    AContainer := TdxSpreadSheetContainerAccess(View.Containers[I]);
    if not IsHistoryAction then
      History.AddCommand(TdxSpreadSheetHistoryChangeContainerCommand.Create(AContainer));
    if NeedSaveContainersBounds(AContainer) then
    begin
      FixedContainerAnchorTypes.Add(AContainer, AContainer.AnchorType);
      FixedContainers.Add(AContainer, AContainer.Calculator.CalculateBounds);
    end;
    TdxSpreadSheetContainersAccess(View.Containers).UnregisterCommentContainer(AContainer);
  end;
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.RestoreContainerPosition(
  AContainer: TdxSpreadSheetContainer; const AContainerBounds: TRect; AAnchorType: TdxSpreadSheetContainerAnchorType);
begin
  if AContainer.AnchorType <> AAnchorType then
  begin
    AContainer.AnchorType := AAnchorType;
    TdxSpreadSheetContainerAccess(AContainer).Calculator.UpdateAnchors(AContainerBounds);
  end
  else
    TdxSpreadSheetContainerAccess(AContainer).Calculator.UpdateAnchorsAfterResize(AContainerBounds, True);
end;

procedure TdxSpreadSheetTableViewCustomCellsModificationHelper.RestoreContainersPosition;
var
  AContainer: TdxSpreadSheetContainer;
  I: Integer;
begin
  for I := 0 to View.Containers.Count - 1 do
    TdxSpreadSheetContainersAccess(View.Containers).RegisterCommentContainer(View.Containers[I]);

  if not IsHistoryAction then
    for AContainer in FixedContainers.Keys do
    begin
      AContainer.BeginUpdate;
      try
        RestoreContainerPosition(AContainer, FixedContainers.Items[AContainer], FixedContainerAnchorTypes.Items[AContainer]);
      finally
        AContainer.EndUpdate;
      end;
    end;
end;

function TdxSpreadSheetTableViewCustomCellsModificationHelper.GetIsHistoryAction: Boolean;
begin
  Result := TdxSpreadSheetHistoryAccess(History).InProcess;
end;

{ TdxSpreadSheetTableViewMoveCellsModificationHelper }

procedure TdxSpreadSheetTableViewMoveCellsModificationHelper.Process(const AArea: TRect; const ATargetPoint: TPoint);
begin
  FSourceArea := AArea;
  FTargetArea := cxRectSetOrigin(AArea, ATargetPoint);
  StoreContainersPosition;
  try
    ProcessCore;
  finally
    RestoreContainersPosition;
  end;
end;

function TdxSpreadSheetTableViewMoveCellsModificationHelper.NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean;

  function CheckAnchor(AAnchor: TdxSpreadSheetContainerAnchorPoint): Boolean;
  begin
    Result := AAnchor.FixedToCell and (IsAnchorPointInArea(SourceArea, AAnchor) or IsAnchorPointInArea(TargetArea, AAnchor));
  end;

begin
  Result := False;
  case AContainer.AnchorType of
    catOneCell:
      Result := CheckAnchor(AContainer.AnchorPoint1);
    catTwoCell:
      Result := CheckAnchor(AContainer.AnchorPoint1) or CheckAnchor(AContainer.AnchorPoint2);
  end;
end;

procedure TdxSpreadSheetTableViewMoveCellsModificationHelper.ProcessCore;
var
  AMergedCells: TdxRectList;
  I: Integer;
begin
  AMergedCells := TdxRectList.Create;
  try
    if not IsHistoryAction then
    begin
      CheckProtection(SourceArea);
      CheckProtection(TargetArea);

      AMergedCells.Capacity := View.MergedCells.Count;
      View.MergedCells.EnumCells(SourceArea,
        procedure (ACell: TdxSpreadSheetMergedCell)
        begin
          AMergedCells.Add(cxRectOffset(ACell.Area, SourceArea.TopLeft, False));
        end);
    end;

    View.SpreadSheet.FormulaController.UpdateReferences(View, SourceArea, TargetArea.TopLeft, urmMove);

    if IsHistoryAction then
      ClearCells(TargetArea, SourceArea, [ccoFreeCellInstances])
    else
    begin
      TdxSpreadSheetTableViewAccess(View).MergedCells.DeleteItemsInArea(SourceArea, False);
      TdxSpreadSheetTableViewAccess(View).MergedCells.DeleteItemsInArea(TargetArea, False);
      ClearCells(TargetArea, SourceArea, dxSpreadSheetTableViewClearCellsOptionsAll + [ccoFreeCellInstances]);
      History.AddCommand(TdxSpreadSheetHistoryMoveCellCommand.Create(SourceArea, TargetArea));
    end;

    CreateIndependentConditionalFormattingAreasForMovedArea(SourceArea);
    MoveConditionalFormattingAreas(SourceArea, TargetArea.Left - SourceArea.Left, TargetArea.Top - SourceArea.Top);

    TdxSpreadSheetTableRowsAccess(View.Rows).ForEach(
      procedure (ARow: TdxDynamicListItem)
      begin
        TdxSpreadSheetTableRowCellsAccess(TdxSpreadSheetTableRowAccess(ARow).RowCells).ForEach(
          procedure (AItem: TdxDynamicListItem)
          begin
            TdxSpreadSheetCellAccess(AItem).Move(
              ARow.Index - SourceArea.Top + TargetArea.Top,
              AItem.Index - SourceArea.Left + TargetArea.Left);
          end,
          SourceArea.Left, SourceArea.Right, TargetArea.Left <= SourceArea.Left);
      end,
      SourceArea.Top, SourceArea.Bottom, TargetArea.Top <= SourceArea.Top);

    if not IsHistoryAction then
    begin
      for I := 0 to AMergedCells.Count - 1 do
        View.MergedCells.Add(cxRectOffset(AMergedCells[I], TargetArea.TopLeft));
    end;
  finally
    AMergedCells.Free;
  end;
end;

procedure TdxSpreadSheetTableViewMoveCellsModificationHelper.RestoreContainerPosition(
  AContainer: TdxSpreadSheetContainer; const AContainerBounds: TRect; AAnchorType: TdxSpreadSheetContainerAnchorType);
begin
  AContainer.AnchorType := AAnchorType;
  TdxSpreadSheetContainerAccess(AContainer).Calculator.UpdateAnchors(AContainerBounds);
end;

procedure TdxSpreadSheetTableViewMoveCellsModificationHelper.StartAction;
begin
  History.BeginAction(TdxSpreadSheetHistoryMoveCellsAction);
end;

procedure TdxSpreadSheetTableViewMoveCellsModificationHelper.EndAction;
begin
  History.EndAction;
end;

{ TdxSpreadSheetTableViewCellsModificationHelper }

procedure TdxSpreadSheetTableViewCellsModificationHelper.Process(AArea: TRect);
begin
  Initialize(AArea);
  try
    FProcessedArea := AArea;
    DoProcess(AArea);
    Changed;
    TdxSpreadSheetTableViewAccess(View).AddChanges([sscData, sscModified]);
  except
    on E: EAbort do
      // do nothing
    else
      raise;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.DoCommand(const AArea: TRect;
  AModification: TdxSpreadSheetCellsModification; AIsHorizontal: Boolean);
begin
  FMovableArea := AArea;
  FModification := AModification;
  FIsHorizontalShifting := Modification in [cmShiftCellsHorizontally, cmShiftColumns];
  FProcessedArea := AArea;
  DoProcess(AArea);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.Initialize(var AArea: TRect);
begin
  if dxSpreadSheetIsEntireRow(AArea) and (Modification = cmShiftCellsVertically) then
    Modification := cmShiftRows;
  if dxSpreadSheetIsEntireColumn(AArea) and (Modification = cmShiftCellsHorizontally) then
    Modification := cmShiftColumns;

  case Modification of
    cmShiftColumns:
      AArea := cxRect(AArea.Left, 0, AArea.Right, MaxInt);
    cmShiftRows:
      AArea := cxRect(0, AArea.Top, MaxInt, AArea.Bottom);
  end;

  FArea := dxSpreadSheetGetRealArea(AArea);
  case Modification of
    cmShiftCellsHorizontally, cmShiftColumns:
      FMovableArea := cxRect(AArea.Left, AArea.Top, MaxInt, AArea.Bottom);
    cmShiftCellsVertically, cmShiftRows:
      FMovableArea := cxRect(AArea.Left, AArea.Top, AArea.Right, MaxInt);
  end;
  FIsHorizontalShifting := Modification in [cmShiftCellsHorizontally, cmShiftColumns];
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.BeforeShiftCells;
begin
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.CheckIsAreaEmpty(AItems: TdxSpreadSheetTableItems; const AArea: TRect);
begin
  if (AArea.Left <= AArea.Right) and (AArea.Top <= AArea.Bottom) then
  begin
    if not IsAreaEmpty(AItems, AArea) and not IsHistoryAction then
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorPossibleDataLoss));
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.CreateBlankCellsForStyledItems(
  AItems, AOppositeItems: TdxSpreadSheetTableItems; const AArea: TRect; AShiftDelta: Integer);

  procedure ApplyStyleToBlankCellsInItem(AItem: TdxSpreadSheetTableItem; AStyleHandle: TdxSpreadSheetCellStyleHandle);
  var
    AOppositeItem: TdxSpreadSheetTableItem;
    I: Integer;
  begin
    for I := AArea.Top to AArea.Bottom do
      if AItem.Cells[I] = nil then
      begin
        AOppositeItem := AOppositeItems.Items[I];
        if IsHorizontalShifting and (AOppositeItem <> nil) and not AOppositeItem.Style.IsDefault then
          Continue;

        if (AStyleHandle <> nil) and (AItem.Style.Handle <> AStyleHandle) then
          AItem.CreateCell(I).StyleHandle := AStyleHandle
        else
          if (AStyleHandle = nil) and (AOppositeItem <> nil) then
            AItem.CreateCell(I).StyleHandle := AOppositeItem.Style.Handle;
      end;
  end;

  procedure ApplyStyleToBlankCells(AIsSecondPass: Boolean);
  var
    AItem: TdxSpreadSheetTableItem;
    AMaxItemIndex: Integer;
  begin
    AItem := AItems.Last;
    while (AItem <> nil) and (AItem.Index > AArea.Right) do
      AItem := AItem.Prev;

    AMaxItemIndex := TdxSpreadSheetTableItemsAccess(AItems).GetMaxItemIndex;
    while (AItem <> nil) and (AItem.Index >= AArea.Left) do
    begin
      if not AItem.Style.IsDefault then
      begin
        if AIsSecondPass then
          ApplyStyleToBlankCellsInItem(AItem, nil)
        else
          if InRange(AItem.Index + AShiftDelta, AArea.Left, AMaxItemIndex) then
            ApplyStyleToBlankCellsInItem(AItems.CreateItem(AItem.Index + AShiftDelta), AItem.Style.Handle);
      end;
      AItem := AItem.Prev;
    end;
  end;

begin
  ApplyStyleToBlankCells(False);
  ApplyStyleToBlankCells(True);
end;

function TdxSpreadSheetTableViewCellsModificationHelper.IsAreaAffected(
  const AArea: TRect; ADeltaX, ADeltaY: Integer; var ANewArea: TRect): Boolean;
begin
  Result := dxSpreadSheetIntersects(AArea, MovableArea, ANewArea);
  if Result then
  begin
    if cxRectIsEqual(AArea, ANewArea) and not (IsDeletion and dxSpreadSheetIntersects(ANewArea, FArea)) then
      ANewArea := cxRectOffset(AArea, ADeltaX, ADeltaY)
    else
      ANewArea := cxRectInflate(AArea, 0, 0, ADeltaX, ADeltaY);
  end;
end;

function TdxSpreadSheetTableViewCellsModificationHelper.IsAreaEmpty(AItems: TdxSpreadSheetTableItems; const AArea: TRect): Boolean;
var
  AItem: TdxSpreadSheetTableItem;
  I: Integer;
begin
  Result := True;
  AItem := AItems.Last;
  while (AItem <> nil) and (AItem.Index > AArea.Right) do
    AItem := AItem.Prev;
  while (AItem <> nil) and (AItem.Index >= AArea.Left) do
  begin
    if not AItem.Style.IsDefault then
      Exit(False);
    for I := AArea.Top to Min(AItem.CellCount - 1, AArea.Bottom) do
    begin
      if AItem.Cells[I] <> nil then
        Exit(False);
    end;
    AItem := AItem.Prev;
  end;
end;

function TdxSpreadSheetTableViewCellsModificationHelper.NeedSaveContainersBounds(AContainer: TdxSpreadSheetContainer): Boolean;
var
  R: TRect;
begin
  Result := False;
  case AContainer.AnchorType of
    catOneCell:
      Result := not (AContainer.AnchorPoint1.FixedToCell and IsAnchorPointInArea(MovableArea, AContainer.AnchorPoint1));
    catTwoCell:
      begin
        R := MovableArea;
        if IsHorizontalShifting then
          R.Left := 0
        else
          R.Top := 0;

        Result := not (
          AContainer.AnchorPoint1.FixedToCell and IsAnchorPointInArea(R, AContainer.AnchorPoint1) and
          AContainer.AnchorPoint2.FixedToCell and IsAnchorPointInArea(R, AContainer.AnchorPoint2));
      end;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.CheckMergedCells;
begin
  if HasAffectedMergedCells then
    case MergedCellsConflictResolution of
      mccrShowConfirmation:
        if MessageDlg(cxGetResourceString(@sdxUnmergeCellsConfirmation), mtConfirmation, [mbOK, mbCancel], 0) = mrOk then
          UnmergeAllAffectedCells
        else
          Abort;

      mccrUnmergeAllAffectedCells:
        UnmergeAllAffectedCells;

    else
      raise EdxSpreadSheetError.Create(cxGetResourceString(@sdxErrorCannotMoveBecauseOfMergedCells));
    end;
end;

function TdxSpreadSheetTableViewCellsModificationHelper.HasAffectedMergedCells: Boolean;
var
  ACell: TdxSpreadSheetMergedCell;
  ARect: TRect;
begin
  Result := False;
  ACell := View.MergedCells.First;
  while ACell <> nil do
  begin
    if dxSpreadSheetIntersects(ACell.Area, MovableArea, ARect) then
    begin
      if not cxRectIsEqual(ACell.Area, ARect) then
        Exit(True);
    end;
    ACell := ACell.Next;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.MoveGroups(
  AGroups: TdxSpreadSheetTableItemGroup; AStartFromIndex, ADelta: Integer);

  function ProcessGroup(AGroup: TdxSpreadSheetTableItemGroup; AStartFromIndex, ADelta: Integer): Boolean;
  var
    AStartIndex, AFinishIndex: Integer;
  begin
    AStartIndex := AGroup.StartIndex;
    if AStartFromIndex <= AStartIndex then
    begin
      if ADelta < 0 then
        Inc(AStartIndex, Max(ADelta, AStartFromIndex - AStartIndex))
      else
        Inc(AStartIndex, ADelta);
    end;

    AFinishIndex := AGroup.FinishIndex;
    if AStartFromIndex <= AFinishIndex + Ord(ADelta > 0) then
    begin
      if ADelta < 0 then
        Inc(AFinishIndex, Max(ADelta, AStartFromIndex - AFinishIndex - 1))
      else
        Inc(AFinishIndex, ADelta);
    end;

    Result := AFinishIndex >= AStartIndex;
    if Result and ((AGroup.StartIndex <> AStartIndex) or (AGroup.FinishIndex <> AFinishIndex)) then
    begin
      AGroup.BeginUpdate;
      try
        MoveGroups(AGroup, AStartFromIndex, ADelta);
        AGroup.FinishIndex := AFinishIndex;
        AGroup.StartIndex := AStartIndex;
      finally
        AGroup.EndUpdate;
      end;
    end;
  end;

var
  AIndex: Integer;
begin
  AIndex := IfThen(ADelta > 0, AGroups.Count - 1);
  while InRange(AIndex, 0, AGroups.Count - 1) do
  begin
    if ProcessGroup(AGroups[AIndex], AStartFromIndex, ADelta) then
      AIndex := AIndex + ValueIncr[ADelta < 0]
    else
      AGroups[AIndex].Free;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.MovePrintAreas(const AMovingArea: TRect; ADeltaX, ADeltaY: Integer);

  function AdjustPrintRect(ARect: TdxSpreadSheetTableViewOptionsPrintRect): Boolean;
  var
    R: TRect;
  begin
    Result := True;
    if ARect.Assigned then
    begin
      R := ARect.Rect;
      ARect.Assigned := MoveArea(R, AMovingArea, ADeltaX, ADeltaY);
      if ARect.Assigned then
        ARect.Rect := R
      else
        Result := False;
    end;
  end;

  procedure AdjustPageBreaks(AList: TList<Cardinal>; const APrintArea: TRect; AIsHorizontal: Boolean);
  var
    I: Integer;
    R: TRect;
  begin
    for I := AList.Count - 1 downto 0 do
    begin
      if AIsHorizontal then
        R := cxRectBounds(AList[I], 0, 0, MaxInt)
      else
        R := cxRectBounds(0, AList[I], MaxInt, 0);

      if dxSpreadSheetIntersects(R, APrintArea, R) and MoveArea(R, AMovingArea, ADeltaX, ADeltaY) then
        AList[I] := IfThen(AIsHorizontal, R.Right, R.Bottom)
      else
        AList.Delete(I);
    end;

    for I := AList.Count - 1 downto 0 do
    begin
      if AList.IndexOf(AList[I]) <> I then
        AList.Delete(I);
    end;
  end;

begin
  if IsHistoryAction then
    Exit;

  History.AddCommand(TdxSpreadSheetHistoryChangePrintingOptionsCommand.Create);

  if ADeltaX <> 0 then
    AdjustPageBreaks(View.OptionsPrint.Pagination.ColumnPageBreaks, View.PrintArea, True);
  if ADeltaY <> 0 then
    AdjustPageBreaks(View.OptionsPrint.Pagination.RowPageBreaks, View.PrintArea, False);

  AdjustPrintRect(View.OptionsPrint.Source.Area);
  if not AdjustPrintRect(View.OptionsPrint.Source.ColumnsToRepeat) then
    View.OptionsPrint.Source.RowsToRepeat.Assigned := False;
  if not AdjustPrintRect(View.OptionsPrint.Source.RowsToRepeat) then
    View.OptionsPrint.Source.ColumnsToRepeat.Assigned := False;

  View.OptionsPrint.Pagination.ColumnPageBreaks.Remove(View.PrintArea.Left);
  View.OptionsPrint.Pagination.RowPageBreaks.Remove(View.PrintArea.Top);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.TransformAffectedObjectArea(ADeltaX, ADeltaY: Integer);
var
  ANewArea: TRect;
  AMergedCell: TdxSpreadSheetMergedCell;
  AMergedCellNext: TdxSpreadSheetMergedCell;
  AHyperlink: TdxSpreadSheetHyperLink;
begin
  if IsHistoryAction then
    Exit;

  AMergedCell := View.MergedCells.First;
  while AMergedCell <> nil do
  begin
    AMergedCellNext := AMergedCell.Next;
    if IsAreaAffected(AMergedCell.Area, ADeltaX, ADeltaY, ANewArea) then
    begin
      if dxSpreadSheetIsValidArea(ANewArea) and not dxSpreadSheetIsSingleCellArea(ANewArea) then
      begin
        History.AddCommand(TdxSpreadSheetHistoryMergeCellsCommand.Create(AMergedCell, ANewArea));
        TdxSpreadSheetMergedCellAccess(AMergedCell).Initialize(AMergedCell.Owner, ANewArea);
      end
      else
        AMergedCell.Free;
    end;
    AMergedCell := AMergedCellNext;
  end;

  AHyperLink := View.Hyperlinks.First;
  while AHyperlink <> nil do
  begin
    if IsAreaAffected(AHyperlink.Area, ADeltaX, ADeltaY, ANewArea) then
      TdxSpreadSheetHyperlinkAccess(AHyperlink).ReplaceArea(ANewArea);
    AHyperlink := AHyperlink.Next;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.UnmergeAllAffectedCells;
var
  ACell: TdxSpreadSheetMergedCell;
  ACellPrev: TdxSpreadSheetMergedCell;
  ARect: TRect;
begin
  ACell := View.MergedCells.Last;
  while ACell <> nil do
  begin
    ACellPrev := ACell.Prev;
    if dxSpreadSheetIntersects(ACell.Area, MovableArea, ARect) then
    begin
      if not cxRectIsEqual(ACell.Area, ARect) then
        ACell.Free;
    end;
    ACell := ACellPrev;
  end;
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.ShiftCellsHorizontally(
  AStartFromIndex, AStartRowIndex, AFinishRowIndex, ADelta: Integer);
begin
  if ADelta > 0 then
    CheckIsAreaEmpty(Columns, cxRect(dxSpreadSheetMaxColumnIndex - ADelta + 1,
      AStartRowIndex, dxSpreadSheetMaxColumnIndex, AFinishRowIndex));

  CheckMergedCells;
  BeforeShiftCells;

  TdxSpreadSheetTableRowsAccess(Rows).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      ShiftIndexes(TdxSpreadSheetTableRowAccess(AItem).RowCells, AStartFromIndex, ADelta);
    end,
    AStartRowIndex, AFinishRowIndex);

  View.ForEachCell(cxRect(AStartFromIndex, AStartRowIndex, MaxInt, AFinishRowIndex),
    procedure (ACell: TdxSpreadSheetCell)
    begin
      Columns.CreateItem(ACell.ColumnIndex);
    end);

  CreateIndependentConditionalFormattingAreasForMovedArea(MovableArea);
  MoveConditionalFormattingAreas(MovableArea, ADelta, 0);
  MovePrintAreas(MovableArea, ADelta, 0);
  TransformAffectedObjectArea(ADelta, 0);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.ShiftCellsVertically(
  AStartFromIndex, AStartColumnIndex, AFinishColumnIndex, ADelta: Integer);
begin
  if ADelta > 0 then
    CheckIsAreaEmpty(Rows, cxRect(dxSpreadSheetMaxRowIndex - ADelta + 1,
      AStartColumnIndex, dxSpreadSheetMaxRowIndex, AFinishColumnIndex));

  CheckMergedCells;
  BeforeShiftCells;
  TdxSpreadSheetTableRowsAccess(Rows).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      ACell: TdxSpreadSheetCellAccess;
      ANextCell: TdxSpreadSheetCellAccess;
      ARow: TdxSpreadSheetTableRowAccess;
      ATargetRowIndex: Integer;
    begin
      ARow := TdxSpreadSheetTableRowAccess(AItem);
      ATargetRowIndex := ARow.Index + ADelta;
      ACell := TdxSpreadSheetCellAccess(ARow.RowCells.First);
      while ACell <> nil do
      begin
        ANextCell := TdxSpreadSheetCellAccess(ACell.FNext);
        if InRange(ACell.ColumnIndex, AStartColumnIndex, AFinishColumnIndex) then
          ACell.RowIndex := ATargetRowIndex;
        ACell := ANextCell;
      end;
    end,
    AStartFromIndex, MaxInt, ADelta <= 0);

  CreateIndependentConditionalFormattingAreasForMovedArea(MovableArea);
  MoveConditionalFormattingAreas(MovableArea, 0, ADelta);
  MovePrintAreas(MovableArea, 0, ADelta);
  TransformAffectedObjectArea(0, ADelta);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.ShiftColumns(AStartFromIndex, ADelta: Integer);
begin
  if ADelta > 0 then
    CheckIsAreaEmpty(Columns, cxRect(dxSpreadSheetMaxColumnIndex - ADelta + 1, 0, dxSpreadSheetMaxColumnIndex, MaxInt));
  BeforeShiftCells;
  ShiftIndexes(Columns, AStartFromIndex, ADelta);
  if View.FrozenColumn >= AStartFromIndex then
    View.FrozenColumn := View.FrozenColumn + ADelta;

  MoveConditionalFormattingAreas(MovableArea, ADelta, 0);
  MovePrintAreas(MovableArea, ADelta, 0);
  TransformAffectedObjectArea(ADelta, 0);
  MoveGroups(TdxSpreadSheetTableItemGroupsAccess(Columns.Groups).Root, AStartFromIndex, ADelta);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.ShiftIndexes(AList: TdxDynamicItemList; AStartFromIndex, ADelta: Integer);
begin
  TdxDynamicItemListAccess(AList).ShiftIndexes(AStartFromIndex, ADelta);
end;

procedure TdxSpreadSheetTableViewCellsModificationHelper.ShiftRows(AStartFromIndex, ADelta: Integer);
begin
  if ADelta > 0 then
    CheckIsAreaEmpty(Rows, cxRect(dxSpreadSheetMaxRowIndex - ADelta + 1, 0, dxSpreadSheetMaxRowIndex, MaxInt));
  BeforeShiftCells;
  ShiftIndexes(Rows, AStartFromIndex, ADelta);
  if View.FrozenRow >= AStartFromIndex then
    View.FrozenRow := View.FrozenRow + ADelta;

  MoveConditionalFormattingAreas(MovableArea, 0, ADelta);
  MovePrintAreas(MovableArea, 0, ADelta);
  TransformAffectedObjectArea(0, ADelta);
  MoveGroups(TdxSpreadSheetTableItemGroupsAccess(Rows.Groups).Root, AStartFromIndex, ADelta);
end;

{ TdxSpreadSheetTableViewDeleteCellsHelper }

procedure TdxSpreadSheetTableViewDeleteCellsHelper.CheckContainer(const AArea: TRect; AContainer: TdxSpreadSheetContainer);
var
  ANeedDelete, ARemoveFromFixedContainers: Boolean;
begin
  ANeedDelete := False;
  ARemoveFromFixedContainers := True;
  case AContainer.AnchorType of
    catTwoCell:
      if AContainer.AnchorPoint1.FixedToCell and IsAnchorPointInArea(AArea, AContainer.AnchorPoint1) and
         AContainer.AnchorPoint2.FixedToCell and IsAnchorPointInArea(AArea, AContainer.AnchorPoint2)
      then
        ANeedDelete := True
      else
        if AContainer.AnchorPoint1.FixedToCell and IsAnchorPointInArea(AArea, AContainer.AnchorPoint1) then
        begin
          MoveAnchor(AContainer.AnchorPoint1, AArea, True);
          ARemoveFromFixedContainers := AContainer.AnchorPoint2.FixedToCell;
        end
        else
          if AContainer.AnchorPoint1.FixedToCell and AContainer.AnchorPoint2.FixedToCell and
            IsAnchorPointInArea(AArea, AContainer.AnchorPoint2)
          then
            MoveAnchor(AContainer.AnchorPoint2, AArea, False)
          else
            ARemoveFromFixedContainers := False;

    catOneCell:
      if AContainer.AnchorPoint1.FixedToCell and IsAnchorPointInArea(AArea, AContainer.AnchorPoint1) then
        MoveAnchor(AContainer.AnchorPoint1, AArea, True)
      else
        ARemoveFromFixedContainers := False;

  else
    ARemoveFromFixedContainers := False;
  end;

  if ARemoveFromFixedContainers then
    FixedContainers.Remove(AContainer);
  if ANeedDelete then
    AContainer.Free;
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.CheckContainers(const AArea: TRect);
var
  I: Integer;
begin
  for I := View.Containers.Count - 1 downto 0 do
    CheckContainer(AArea, View.Containers[I]);
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.DeleteCellsInArea(const AArea: TRect);
begin
  View.ForEachCell(AArea,
    procedure (ACell: TdxSpreadSheetCell)
    begin
      ACell.Free;
    end,
    False);
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.DeleteItems(
  AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer);
begin
  TdxDynamicItemListAccess(AItems).ForEach(
    procedure(AItem: TdxDynamicListItem)
    begin
      AItem.Free;
    end,
    AStartIndex, AFinishIndex);
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.DoProcess(const AArea: TRect);
begin
  if Modification in [cmShiftCellsHorizontally, cmShiftCellsVertically] then
    CheckMergedCells;

  View.DeleteComments(AArea);
  View.MergedCells.DeleteItemsInArea(AArea);
  SpreadSheet.FormulaController.UpdateReferences(View, AArea,
    cxPoint(
      IfThen(IsHorizontalShifting, -1, AArea.Left),
      IfThen(IsHorizontalShifting, AArea.Top, -1)),
    urmDelete);

  StoreContainersPosition;
  try
    CheckContainers(AArea);
    DeleteCellsInArea(AArea);

    case Modification of
      cmShiftCellsHorizontally:
        ShiftCellsHorizontally(AArea.Left, AArea.Top, AArea.Bottom, -(cxRectWidth(AArea) + 1));

      cmShiftCellsVertically:
        ShiftCellsVertically(AArea.Top, AArea.Left, AArea.Right, -(cxRectHeight(AArea) + 1));

      cmShiftColumns:
        begin
          DeleteItems(Columns, AArea.Left, AArea.Right);
          ShiftColumns(AArea.Left, -(cxRectWidth(AArea) + 1));
        end;

      cmShiftRows:
        begin
          DeleteItems(Rows, AArea.Top, AArea.Bottom);
          ShiftRows(AArea.Top, -(cxRectHeight(AArea) + 1));
        end;
    end;

    History.AddCommand(TdxSpreadSheetHistoryDeleteCommand.Create(View, AArea, Modification, IsHorizontalShifting));

    case Modification of
      cmShiftCellsHorizontally:
        CreateBlankCellsForStyledItems(Columns, Rows, MovableArea, -(cxRectWidth(AArea) + 1));
      cmShiftCellsVertically:
        CreateBlankCellsForStyledItems(Rows, Columns, cxRect(MovableArea.Top,
          MovableArea.Left, MovableArea.Bottom, MovableArea.Right), -(cxRectHeight(AArea) + 1));
    end;
  finally
    RestoreContainersPosition;
  end;
end;

function TdxSpreadSheetTableViewDeleteCellsHelper.IsDeletion: Boolean;
begin
  Result := True;
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.EndAction;
begin
  History.EndAction;
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.StartAction;
begin
  History.BeginAction(TdxSpreadSheetHistoryDeleteCellsAction);
end;

procedure TdxSpreadSheetTableViewDeleteCellsHelper.MoveAnchor(
  AAnchor: TdxSpreadSheetContainerAnchorPoint; const AArea: TRect; AMoveForward: Boolean);
var
  AIndex: Integer;
  AOffset: TPoint;
begin
  if IsHorizontalShifting then
  begin
    AIndex := IfThen(AMoveForward, AArea.Right + 1, AArea.Left - 1);
    AIndex := Min(Max(AIndex, 0), dxSpreadSheetMaxColumnIndex);
    AAnchor.Cell := View.CreateCell(AAnchor.Cell.RowIndex, AIndex);
  end
  else
  begin
    AIndex := IfThen(AMoveForward, AArea.Bottom + 1, AArea.Top - 1);
    AIndex := Min(Max(AIndex, 0), dxSpreadSheetMaxRowIndex);
    AAnchor.Cell := View.CreateCell(AIndex, AAnchor.Cell.ColumnIndex);
  end;

  if AMoveForward then
    AOffset := cxNullPoint
  else
    AOffset := cxPoint(cxSize(AAnchor.Cell.GetAbsoluteBounds));

  if IsHorizontalShifting then
    AAnchor.Offset := cxPoint(AOffset.X, AAnchor.Offset.Y)
  else
    AAnchor.Offset := cxPoint(AAnchor.Offset.X, AOffset.Y);
end;

{ TdxSpreadSheetTableViewInsertCellsHelper }

procedure TdxSpreadSheetTableViewInsertCellsHelper.BeforeShiftCells;
begin
  SpreadSheet.FormulaController.UpdateReferences(View, ProcessedArea,
    cxPointOffset(ProcessedArea.TopLeft,
      IfThen(IsHorizontalShifting, cxRectWidth(ProcessedArea) + 1),
      IfThen(not IsHorizontalShifting, cxRectHeight(ProcessedArea) + 1)), urmInsert);
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.CalculateStyle(
  ANewStyle, ALeftSourceStyle, ARightSourceStyle: TdxSpreadSheetCellStyle);

  procedure GetSides(out ALeftSide, ARightSide: TcxBorder);
  begin
    if IsHorizontalShifting then
    begin
      ARightSide := bRight;
      ALeftSide := bLeft;
    end
    else
    begin
      ARightSide := bBottom;
      ALeftSide := bTop;
    end;
  end;

  function MergeBorderStyles: Boolean;
  var
    ALeftSide, ARightSide: TcxBorder;
  begin
    Result := False;
    if (ALeftSourceStyle = nil) or (ARightSourceStyle = nil) then
      Exit;

    GetSides(ALeftSide, ARightSide);
    Result := (ALeftSourceStyle.Borders[ARightSide].Style <> sscbsDefault) and
      (ARightSourceStyle.Borders[ALeftSide].Style <> sscbsDefault);

    if Result then
    begin
      ANewStyle.BeginUpdate;
      try
        ANewStyle.Borders[ARightSide].Assign(ARightSourceStyle.Borders[ALeftSide]);
        ANewStyle.Borders[ALeftSide].Assign(ALeftSourceStyle.Borders[ARightSide]);
      finally
        ANewStyle.EndUpdate;
      end;
    end;
  end;

begin
  if ALeftSourceStyle <> nil then
    ANewStyle.Handle := ALeftSourceStyle.Handle;

  if not MergeBorderStyles then
  begin
    ANewStyle.BeginUpdate;
    try
      ANewStyle.Handle.Borders := View.CellStyles.DefaultStyle.Borders;
    finally
      ANewStyle.EndUpdate;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.CalculateStyleForNewCells(
  ATarget, ALeftSource, ARightSource: TdxSpreadSheetTableItem;
  AOppositeItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer);
var
  ALeftSourceStyle: TdxSpreadSheetCellStyle;
  ARightSourceStyle: TdxSpreadSheetCellStyle;
  I: Integer;
begin
  if IsHistoryAction then
    Exit;
  AFinishIndex := Min(AFinishIndex, AOppositeItems.LastIndex);
  for I := AStartIndex to AFinishIndex do
  begin
    ALeftSourceStyle := GetCellStyle(ALeftSource, I, AOppositeItems);
    ARightSourceStyle := GetCellStyle(ARightSource, I, AOppositeItems);
    if (ALeftSourceStyle <> nil) or (ARightSourceStyle <> nil) then
      CalculateStyle(ATarget.CreateCell(I).Style, ALeftSourceStyle, ARightSourceStyle);
  end;
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.CalculateStyleForNewItems(
  const AArea: TRect; AItems, AOppositeItems: TdxSpreadSheetTableItems);
var
  AItem: TdxSpreadSheetTableItem;
  ALeftSourceItem: TdxSpreadSheetTableItem;
  ARightSourceItem: TdxSpreadSheetTableItem;
  I: Integer;
begin
  if Modification in [cmShiftCellsHorizontally, cmShiftCellsVertically] then
  begin
    CreateBlankCellsForStyledItems(AItems, AOppositeItems,
      cxRect(AArea.Left, AArea.Top, MaxInt, AArea.Bottom), cxRectWidth(AArea) + 1);
  end;

  if AArea.Left > 0 then
  begin
    ALeftSourceItem := AItems[AArea.Left - 1];
    ARightSourceItem := AItems[AArea.Right + 1];
    for I := AArea.Left to AArea.Right do
    begin
      AItem := AItems.CreateItem(I);
      if Modification in [cmShiftColumns, cmShiftRows] then
      begin
        CalculateStyle(AItem.Style, GetStyle(ALeftSourceItem), GetStyle(ARightSourceItem));
        if (ALeftSourceItem <> nil) and not ALeftSourceItem.DefaultSize then
          AItem.Size := ALeftSourceItem.CustomSize;
      end;
      CalculateStyleForNewCells(AItem, ALeftSourceItem, ARightSourceItem, AOppositeItems, AArea.Top, AArea.Bottom);
    end;
  end;
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.DoProcess(const AArea: TRect);
begin
  StoreContainersPosition;
  try
    case Modification of
      cmShiftColumns:
        begin
          ShiftColumns(AArea.Left, cxRectWidth(AArea) + 1);
          CalculateStyleForNewItems(cxRect(AArea.Left, 0, AArea.Right, MaxInt), Columns, Rows);
        end;

      cmShiftRows:
        begin
          ShiftRows(AArea.Top, cxRectHeight(AArea) + 1);
          CalculateStyleForNewItems(cxRect(AArea.Top, 0, AArea.Bottom, MaxInt), Rows, Columns);
        end;

      cmShiftCellsHorizontally:
        begin
          ShiftCellsHorizontally(AArea.Left, AArea.Top, AArea.Bottom, cxRectWidth(AArea) + 1);
          CalculateStyleForNewItems(AArea, Columns, Rows);
        end;

      cmShiftCellsVertically:
        begin
          ShiftCellsVertically(AArea.Top, AArea.Left, AArea.Right, cxRectHeight(AArea) + 1);
          CalculateStyleForNewItems(cxRect(AArea.Top, AArea.Left, AArea.Bottom, AArea.Right), Rows, Columns);
        end;
    end;
  finally
    RestoreContainersPosition;
  end;
  History.AddCommand(TdxSpreadSheetHistoryInsertCommand.Create(View, AArea, Modification, IsHorizontalShifting));
end;

function TdxSpreadSheetTableViewInsertCellsHelper.GetCellStyle(ASource: TdxSpreadSheetTableItem;
  ACellIndex: Integer; AOppositeItems: TdxSpreadSheetTableItems): TdxSpreadSheetCellStyle;
var
  ACell: TdxSpreadSheetCell;
begin
  if ASource <> nil then
    ACell := ASource.Cells[ACellIndex]
  else
    ACell := nil;

  if ACell <> nil then
    Result := ACell.Style
  else
    if IsHorizontalShifting then
    begin
      Result := GetStyle(AOppositeItems.Items[ACellIndex]);
      if (Result = nil) or Result.IsDefault then
        Result := GetStyle(ASource);
    end
    else
    begin
      Result := GetStyle(ASource);
      if (Result = nil) or Result.IsDefault then
        Result := GetStyle(AOppositeItems.Items[ACellIndex]);
    end;
end;

function TdxSpreadSheetTableViewInsertCellsHelper.GetStyle(AItem: TdxSpreadSheetTableItem): TdxSpreadSheetCellStyle;
begin
  if AItem <> nil then
    Result := AItem.Style
  else
    Result := nil;
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.EndAction;
begin
  History.EndAction;
end;

procedure TdxSpreadSheetTableViewInsertCellsHelper.StartAction;
begin
  History.BeginAction(TdxSpreadSheetHistoryInsertCellsAction);
end;

{ TdxSpreadSheetTableViewMergeCellStyleHelper }

constructor TdxSpreadSheetTableViewMergeCellStyleHelper.Create(AMergeCell: TdxSpreadSheetMergedCell);
begin
  inherited Create(AMergeCell.Owner.View);
  FMergeCell := AMergeCell;
end;

class procedure TdxSpreadSheetTableViewMergeCellStyleHelper.Calculate(AMergeCell: TdxSpreadSheetMergedCell);
begin
  with Create(AMergeCell) do
  try
    DoCalculate;
  finally
    Free;
  end;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.CalculateCellValues;
begin
  FNonBlankCell := nil;
  View.ForEachCell(MergeCell.Area,
    procedure (ACell: TdxSpreadSheetCell)
    begin
      if not ACell.IsEmpty then
      begin
        if FNonBlankCell = nil then
          FNonBlankCell := ACell
        else
          ACell.IsEmpty := True;
      end;
    end);
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.CalculateBorder(ABorder: TcxBorder; AItem: TdxSpreadSheetTableItem;
  AStartIndex, AFinishIndex: Integer; out ABorderColor: TColor; out ABorderStyle: TdxSpreadSheetCellBorderStyle);

  procedure GetBorderParams(ACell: TdxSpreadSheetCell; out ABorderColor: TColor; out ABorderStyle: TdxSpreadSheetCellBorderStyle);
  begin
    if ACell <> nil then
    begin
      ABorderColor := ACell.Style.Borders[ABorder].Color;
      ABorderStyle := ACell.Style.Borders[ABorder].Style;
    end
    else
    begin
      ABorderColor := clDefault;
      ABorderStyle := sscbsDefault;
    end;
  end;

  procedure ResetBorderParams(out ABorderColor: TColor; out ABorderStyle: TdxSpreadSheetCellBorderStyle);
  begin
    GetBorderParams(nil, ABorderColor, ABorderStyle);
  end;

var
  ATempBorderColor: TColor;
  ATempBorderStyle: TdxSpreadSheetCellBorderStyle;
begin
  if AItem = nil then
  begin
    ResetBorderParams(ABorderColor, ABorderStyle);
    Exit;
  end;

  GetBorderParams(AItem.Cells[AStartIndex], ABorderColor, ABorderStyle);
  while AStartIndex <= AFinishIndex do
  begin
    GetBorderParams(AItem.Cells[AStartIndex], ATempBorderColor, ATempBorderStyle);
    if (ABorderColor <> ATempBorderColor) or (ABorderStyle <> ATempBorderStyle) then
    begin
      ResetBorderParams(ABorderColor, ABorderStyle);
      Break;
    end;
    Inc(AStartIndex);
  end;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.CalculateCellStyles;
var
  AArea: TRect;
  ABorder: TcxBorder;
  AHasStyle: Boolean;
  AStylePattern: TdxSpreadSheetCellStyle;
begin
  AArea := MergeCell.Area;
  AArea.Right := Min(AArea.Right, dxSpreadSheetMaxColumnIndex);
  AArea.Bottom := Min(AArea.Bottom, dxSpreadSheetMaxRowIndex);

  CalculateBorder(bBottom, Rows[AArea.Bottom], AArea.Left, AArea.Right, FBorderColors[bBottom], FBorderStyles[bBottom]);
  CalculateBorder(bLeft, Columns[AArea.Left], AArea.Top, AArea.Bottom, FBorderColors[bLeft], FBorderStyles[bLeft]);
  CalculateBorder(bRight, Columns[AArea.Right], AArea.Top, AArea.Bottom, FBorderColors[bRight], FBorderStyles[bRight]);
  CalculateBorder(bTop, Rows[AArea.Top], AArea.Left, AArea.Right, FBorderColors[bTop], FBorderStyles[bTop]);

  AStylePattern := GetStylePattern;
  AHasStyle := not AStylePattern.IsDefault;
  for ABorder := Low(ABorder) to High(ABorder) do
    AHasStyle := AHasStyle or (FBorderStyles[ABorder] <> sscbsDefault);

  View.ForEachCell(AArea,
    procedure (ACell: TdxSpreadSheetCell)
    begin
      ACell.StyleHandle := AStylePattern.Handle;
    end);

  if AHasStyle then
  begin
    if dxSpreadSheetIsEntireColumn(AArea) then
      CalculateCellStylesInEntireColumnOrRow(View.Columns, AArea.Left, AArea.Right, AStylePattern)
    else
      if dxSpreadSheetIsEntireRow(AArea) then
        CalculateCellStylesInEntireColumnOrRow(View.Rows, AArea.Top, AArea.Bottom, AStylePattern)
      else
        CalculateCellStylesInCustomArea(AArea, AStylePattern);
  end;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.CalculateCellStylesInCustomArea(
  const AArea: TRect; AStylePattern: TdxSpreadSheetCellStyle);
var
  ACell: TdxSpreadSheetCell;
  AColumnIndex: Integer;
  AIsBottomRow: Boolean;
  AIsTopRow: Boolean;
  ARowIndex: Integer;
begin
  for ARowIndex := AArea.Top to AArea.Bottom do
  begin
    AIsBottomRow := ARowIndex = AArea.Bottom;
    AIsTopRow := ARowIndex = AArea.Top;
    for AColumnIndex := AArea.Left to AArea.Right do
    begin
      ACell := View.CreateCell(ARowIndex, AColumnIndex);
      ACell.Style.Assign(AStylePattern);

      if AIsTopRow then
      begin
        ACell.Style.Borders[bTop].Color := FBorderColors[bTop];
        ACell.Style.Borders[bTop].Style := FBorderStyles[bTop];
      end
      else
        ACell.Style.Borders[bTop].Reset;

      if AIsBottomRow then
      begin
        ACell.Style.Borders[bBottom].Color := FBorderColors[bBottom];
        ACell.Style.Borders[bBottom].Style := FBorderStyles[bBottom];
      end
      else
        ACell.Style.Borders[bBottom].Reset;

      if AColumnIndex = AArea.Left then
      begin
        ACell.Style.Borders[bLeft].Color := FBorderColors[bLeft];
        ACell.Style.Borders[bLeft].Style := FBorderStyles[bLeft];
      end
      else
        ACell.Style.Borders[bLeft].Reset;

      if AColumnIndex = AArea.Right then
      begin
        ACell.Style.Borders[bRight].Color := FBorderColors[bRight];
        ACell.Style.Borders[bRight].Style := FBorderStyles[bRight];
      end
      else
        ACell.Style.Borders[bRight].Reset;
    end;
  end;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.CalculateCellStylesInEntireColumnOrRow(
  AItems: TdxSpreadSheetTableItems; AStartIndex, AFinishIndex: Integer; AStylePattern: TdxSpreadSheetCellStyle);
var
  AItem: TdxSpreadSheetTableItem;
  AItemIndex: Integer;
  ALeftSide: TcxBorder;
  ARightSide: TcxBorder;
begin
  if AItems = View.Rows then
  begin
    ALeftSide := bLeft;
    ARightSide := bRight;
  end
  else
  begin
    ALeftSide := bTop;
    ARightSide := bRight;
  end;

  for AItemIndex := AStartIndex to AFinishIndex do
  begin
    AItem := AItems.CreateItem(AItemIndex);
    AItem.Style.Assign(AStylePattern);

    if AItemIndex = AStartIndex then
    begin
      AItem.Style.Borders[ALeftSide].Color := FBorderColors[ALeftSide];
      AItem.Style.Borders[ALeftSide].Style := FBorderStyles[ALeftSide];
    end
    else
      AItem.Style.Borders[ALeftSide].Reset;

    if AItemIndex = AFinishIndex then
    begin
      AItem.Style.Borders[ARightSide].Color := FBorderColors[ARightSide];
      AItem.Style.Borders[ARightSide].Style := FBorderStyles[ARightSide];
    end
    else
      AItem.Style.Borders[ARightSide].Reset;
  end;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.DoCalculate;
begin
  CalculateCellValues;
  CalculateCellStyles;
  SetupActiveCellValue;
end;

procedure TdxSpreadSheetTableViewMergeCellStyleHelper.SetupActiveCellValue;
var
  AComment: TdxSpreadSheetCommentContainer;
begin
  if (NonBlankCell <> nil) and (NonBlankCell <> ActiveCell) then
  begin
    if ActiveCell = nil then
      View.CreateCell(MergeCell.Area.Top, MergeCell.Area.Left);

    if not TdxSpreadSheetContainersAccess(View.Containers).FindCommentContainerCore(
      ActiveCell.RowIndex, ActiveCell.ColumnIndex, AComment) then
    begin
      if TdxSpreadSheetContainersAccess(View.Containers).FindCommentContainerCore(
        NonBlankCell.RowIndex, NonBlankCell.ColumnIndex, AComment)
      then
        AComment.Cell := ActiveCell;
    end;

    TdxSpreadSheetCellAccess(ActiveCell).AssignData(NonBlankCell);
    NonBlankCell.IsEmpty := True;
  end;
  TdxSpreadSheetTableViewAccess(View).ClearCells(
    MergeCell.Area, cxRectBounds(MergeCell.Area.TopLeft, 0, 0), [ccoComments]);
end;

function TdxSpreadSheetTableViewMergeCellStyleHelper.GetActiveCell: TdxSpreadSheetCell;
begin
  Result := MergeCell.ActiveCell;
end;

function TdxSpreadSheetTableViewMergeCellStyleHelper.GetDefaultStyle: TdxSpreadSheetCellStyle;
begin
  Result := SpreadSheet.DefaultCellStyle;
end;

function TdxSpreadSheetTableViewMergeCellStyleHelper.GetStylePattern: TdxSpreadSheetCellStyle;
begin
  if NonBlankCell <> nil then
    Result := NonBlankCell.Style
  else
    if ActiveCell <> nil then
      Result := ActiveCell.Style
    else
      Result := DefaultStyle;
end;

{ TdxSpreadSheetTableViewSortingHelper }

constructor TdxSpreadSheetTableViewSortingHelper.Create(AView: TdxSpreadSheetTableView);
begin
  inherited Create(AView);
  SortOrders := TList<TdxSortOrder>.Create;
  SortIndexes := TList<Integer>.Create;
  History.BeginAction(TdxSpreadSheetHistorySortingAction);
end;

destructor TdxSpreadSheetTableViewSortingHelper.Destroy;
begin
  History.EndAction;
  FreeAndNil(SortOrders);
  FreeAndNil(SortIndexes);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewSortingHelper.SortByColumnValues(const AArea: TRect;
  const ASortOrders: array of TdxSortOrder; const AColumnIndexes: array of Integer);
var
  AIndex, ARow: Integer;
begin
  DefaultSorting := True;
  ValidateArea(AArea);
  AIndex := 0;
  for ARow := Area.Top to Area.Bottom do
    if IsRowEmpty(ARow) then
      SortIndexes.Add(ARow)
    else
    begin
      SortIndexes.Insert(AIndex, ARow);
      Inc(AIndex);
    end;
  if AIndex = 0 then Exit;
  Area.Bottom := Area.Top + AIndex - 1;
  for ARow := Area.Top to Area.Bottom do
    if SortIndexes[ARow - Area.Top] <> ARow then
      ExchangeRowValues(ARow, SortIndexes[ARow - Area.Top]);
  if Initialize(Area.Left, Area.Right, ASortOrders, AColumnIndexes) then
    DoSort(Area.Top, Area.Bottom, 0);
end;

procedure TdxSpreadSheetTableViewSortingHelper.SortByRowValues(const AArea: TRect;
  const ASortOrders: array of TdxSortOrder; const ARowIndexes: array of Integer);
var
  AIndex, AColumn: Integer;
begin
  ValidateArea(AArea);
  AIndex := 0;
  for AColumn := Area.Left to Area.Right do
    if IsColumnEmpty(AColumn) then
      SortIndexes.Add(AColumn)
    else
    begin
      SortIndexes.Insert(AIndex, AColumn);
      Inc(AIndex);
    end;
  if AIndex = 0 then Exit;
  Area.Right := Area.Left + AIndex - 1;
  for AColumn := Area.Left to Area.Right do
    if SortIndexes[AColumn - Area.Left] <> AColumn then
      ExchangeColumnValues(AColumn, SortIndexes[AColumn - Area.Left]);
  if Initialize(Area.Top, Area.Bottom, ASortOrders, ARowIndexes) then
    DoSort(Area.Left, Area.Right, 0);
end;

function TdxSpreadSheetTableViewSortingHelper.Compare(AItem1, AItem2: Pointer): Integer;
var
  AData1, AData2: TdxSpreadSheetCellData;
begin
  Result := 0;
  AData1 := TdxSpreadSheetCellData(AItem1);
  AData2 := TdxSpreadSheetCellData(AItem2);
  if AData1 = AData2 then
    Exit
  else
    Result := CompareCells(AData1, AData2);
  if Result <> 0 then
  begin
    if SortOrder = soDescending then
      Result := -Result;
  end
  else
  begin
    Result := dxCompareValues(AData1.Row, AData2.Row);
    if Result = 0 then
      Result := dxCompareValues(AData1.Column, AData2.Column);
  end;
end;

function TdxSpreadSheetTableViewSortingHelper.CompareCells(AData1, AData2: TdxSpreadSheetCellData): Integer;
begin
  if AData1 = AData2 then
    Result := 0
  else
    Result := CompareValues(AData1.Value, AData2.Value);
  TdxSpreadSheetAccess(SpreadSheet).DoCompare(View, AData1, AData2, Result);
end;

function TdxSpreadSheetTableViewSortingHelper.CompareValues(const AValue1, AValue2: Variant): Integer;
begin
  Result := VarCompare(AValue1, AValue2);
end;

procedure TdxSpreadSheetTableViewSortingHelper.SwapValues(AValue1, AValue2: TdxSpreadSheetCellData);
begin
  if DefaultSorting then
  begin
    ExchangeRowValues(AValue1.Row, AValue2.Row);
    ExchangeLongWords(AValue1.Row, AValue2.Row);
  end
  else
  begin
    ExchangeColumnValues(AValue1.Column, AValue2.Column);
    ExchangeLongWords(AValue1.Column, AValue2.Column);
  end;
end;

procedure TdxSpreadSheetTableViewSortingHelper.DoSort(AFromIndex, AToIndex, ASortIndex: Integer);
var
  AValues: TcxObjectList;
  AIndex, AIndex2: Integer;
begin
  if (AFromIndex = AToIndex) or (ASortIndex >= SortOrders.Count) then Exit;
  AValues := TcxObjectList.Create;
  try
    AValues.Capacity := AToIndex - AFromIndex + 1;
    for AIndex := AFromIndex to AToIndex do
      if DefaultSorting then
        AValues.Add(GetCellData(AIndex, SortIndexes[ASortIndex]))
      else
        AValues.Add(GetCellData(SortIndexes[ASortIndex], AIndex));
    SortOrder := SortOrders[ASortIndex];
    SortValues(AValues, 0, AValues.Count - 1);
    AIndex := AFromIndex;
    while AIndex < AToIndex do
    begin
      AIndex2 := AIndex;
      while (AIndex2 < AToIndex) and (CompareCells(TdxSpreadSheetCellData(AValues[AIndex - AFromIndex]),
        TdxSpreadSheetCellData(AValues[AIndex2 - AFromIndex + 1])) = 0) do
          Inc(AIndex2);
      if AIndex < AIndex2 then
        DoSort(AIndex, AIndex2, ASortIndex + 1);
      AIndex := AIndex2 + 1;
    end;
  finally
    AValues.Free;
  end;
end;

procedure TdxSpreadSheetTableViewSortingHelper.ExchangeColumnValues(AColumn1, AColumn2: Integer);
var
  ARow: Integer;
begin
  for ARow := Area.Top to Area.Bottom do
    TdxSpreadSheetTableViewAccess(View).ExchangeValues(ARow, AColumn1, ARow, AColumn2);
end;

procedure TdxSpreadSheetTableViewSortingHelper.ExchangeRowValues(ARow1, ARow2: Integer);
var
  AColumn: Integer;
begin
  for AColumn := Area.Left to Area.Right do
    TdxSpreadSheetTableViewAccess(View).ExchangeValues(ARow1, AColumn, ARow2, AColumn);
end;

function TdxSpreadSheetTableViewSortingHelper.GetCellData(ARow, AColumn: Integer): TdxSpreadSheetCellData;
var
  AValue: Variant;
  ACell: TdxSpreadSheetCell;
begin
  ACell := View.Cells[ARow, AColumn];
  if ACell = nil then
    AValue := Null
  else
    AValue := ACell.AsVariant;
  Result := TdxSpreadSheetCellData.Create(ARow, AColumn, AValue);
end;

function TdxSpreadSheetTableViewSortingHelper.Initialize(AIndex1, AIndex2: Integer;
  const ASortOrders: array of TdxSortOrder; const AIndexes: array of Integer): Boolean;
var
  AIndex: Integer;
begin
  SortOrders.Clear;
  SortIndexes.Clear;
  if Length(ASortOrders) = 0 then
  begin
    SortOrders.Count := AIndex2 - AIndex1 + 1;
    SortIndexes.Count := AIndex2 - AIndex1 + 1;
    for AIndex := 0 to AIndex2 - AIndex1 do
    begin
      SortIndexes[AIndex] := AIndex + AIndex1;
      SortOrders[AIndex] := soAscending;
    end;
  end
  else
  begin
    for AIndex := 0 to Length(ASortOrders) - 1 do
    begin
      if AIndex > AIndex2 - AIndex1 then Break;
      if AIndex < Length(AIndexes) then
      begin
        if InRange(AIndexes[AIndex], AIndex1, AIndex2) then
          SortIndexes.Add(AIndexes[AIndex])
        else
          Continue;
      end
      else
        if Length(AIndexes) = 0 then
          SortIndexes.Add(AIndex + AIndex1)
        else
          Break;
      SortOrders.Add(ASortOrders[AIndex]);
    end;
    if Length(AIndexes) = 0 then
      for AIndex := AIndex1 + Length(ASortOrders) to AIndex2 do
      begin
        SortOrders.Add(soAscending);
        SortIndexes.Add(AIndex);
      end;
  end;
  Result := SortOrders.Count > 0;
end;

function TdxSpreadSheetTableViewSortingHelper.IsColumnEmpty(AIndex: Integer): Boolean;
var
  ARow: Integer;
  ACell: TdxSpreadSheetCell;
begin
  Result := True;
  if AIndex > View.Dimensions.Right then
    Exit;
  ARow := Area.Top;
  while Result and (ARow <= Area.Bottom) do
  begin
    ACell := View.Cells[ARow, AIndex];
    Result := (ACell = nil) or ACell.IsEmpty or VarIsNull(ACell.AsVariant);
    Inc(ARow);
  end;
end;

function TdxSpreadSheetTableViewSortingHelper.IsRowEmpty(AIndex: Integer): Boolean;
var
  AColumn: Integer;
  ACell: TdxSpreadSheetCell;
begin
  Result := True;
  if (View.Rows[AIndex] = nil) or (View.Rows[AIndex].CellCount = 0) then
    Exit;
  AColumn := Area.Left;
  while Result and (AColumn <= Area.Right) do
  begin
    ACell := View.Cells[AIndex, AColumn];
    Result := (ACell = nil) or ACell.IsEmpty or VarIsNull(ACell.AsVariant);
    Inc(AColumn);
  end;
end;

procedure TdxSpreadSheetTableViewSortingHelper.SortValues(const AValues: TcxObjectList; L, H: Integer);
var
  I, J: Integer;
  Middle: TObject;
begin
  repeat
    I := L;
    J := H;
    Middle := AValues[(L + H) shr 1];
    repeat
      while Compare(AValues[I], Middle) < 0 do Inc(I);
      while Compare(AValues[J], Middle) > 0 do Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          AValues.Exchange(I, J);
          SwapValues(TdxSpreadSheetCellData(AValues[I]), TdxSpreadSheetCellData(AValues[J]));
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      SortValues(AValues, L, J);
    L := I;
  until I >= H;
end;

function TdxSpreadSheetTableViewSortingHelper.ValidateArea(const AArea: TRect): Boolean;
begin
  Area := cxRectAdjust(AArea);
  Area.Bottom := Min(Area.Bottom, View.Dimensions.Bottom);
  Area.Right := Min(Area.Right, View.Dimensions.Right);
  Result := (Area.Top <= Area.Bottom) and (Area.Left <= Area.Right)
end;

{ TdxSpreadSheetTableViewEnumCellStylesHelper }

constructor TdxSpreadSheetTableViewEnumCellStylesHelper.Create(ASheet: TdxSpreadSheetTableView);
begin
  inherited Create;
  FSheet := ASheet;
  FCache := TCache.Create;
end;

destructor TdxSpreadSheetTableViewEnumCellStylesHelper.Destroy;
begin
  FreeAndNil(FCache);
  inherited Destroy;
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.PrepareToSave(AArea: TRect);
var
  AColumnIndex: Integer;
  AIsEntireColumn: Boolean;
  AIsEntireRow: Boolean;
  ARow: TdxSpreadSheetTableRow;
  ARowIndex: Integer;
begin
  AArea := dxSpreadSheetGetRealArea(AArea);
  AIsEntireColumn := dxSpreadSheetIsEntireColumn(AArea);
  AIsEntireRow := dxSpreadSheetIsEntireRow(AArea);

  if AIsEntireRow and not AIsEntireColumn then
  begin
    for ARowIndex := AArea.Top to AArea.Bottom do
      Sheet.Rows.CreateItem(ARowIndex)
  end;

  if AIsEntireColumn then
  begin
    for AColumnIndex := AArea.Left to AArea.Right do
      Sheet.Columns.CreateItem(AColumnIndex);
  end;

  if not (AIsEntireColumn or AIsEntireRow) then
    for ARowIndex := AArea.Top to AArea.Bottom do
    begin
      ARow := Sheet.Rows.CreateItem(ARowIndex);
      for AColumnIndex := AArea.Left to AArea.Right do
        ARow.CreateCell(AColumnIndex);
    end;
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.ProcessArea(
  const AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
var
  ARect: TRect;
begin
  FCache.Clear;
  try
    ARect := dxSpreadSheetGetRealArea(AArea);

    if dxSpreadSheetIsEntireColumn(ARect) and dxSpreadSheetIsEntireRow(ARect) then
    begin
      Inc(TdxSpreadSheetTableViewAccess(Sheet).FMergeCellStylesLockCount);
      try
        EnumColumnStyles(ARect, AProcRef);
        EnumRowStyles(ARect, AProcRef);
      finally
        Dec(TdxSpreadSheetTableViewAccess(Sheet).FMergeCellStylesLockCount);
      end;
    end
    else
    begin
      if dxSpreadSheetIsEntireColumn(ARect) then
        EnumColumnStyles(ARect, AProcRef);
      if dxSpreadSheetIsEntireRow(ARect) then
        EnumRowStyles(ARect, AProcRef);
    end;

    Sheet.ForEachCell(ARect,
      procedure (ACell: TdxSpreadSheetCell)
      begin
        AProcRef(ACell.Style, ACell.RowIndex, ACell.ColumnIndex, ARect);
      end);

    if EnumDefaultStyles then
      EnumDefaultCellsStyles(ARect, AProcRef);
  finally
    FCache.Clear;
  end;
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.EnumColumnStyles(
  AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
begin
  TdxSpreadSheetTableColumnsAccess(Sheet.Columns).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      AColumn: TdxSpreadSheetTableColumn;
    begin
      AColumn := TdxSpreadSheetTableColumn(AItem);
      AProcRef(AColumn.Style, -1, AColumn.Index, AArea);
    end,
    AArea.Left, AArea.Right, True);
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.EnumDefaultCellsStyles(
  const R: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
var
  ACellsCount: Integer;
  AColumn: TdxSpreadSheetTableColumnAccess;
  ARow: TdxSpreadSheetTableRowAccess;
  ARowCount: Integer;
  AStyle: TdxSpreadSheetFakeCellStyle;
begin
  AStyle := TdxSpreadSheetFakeCellStyle.Create(Sheet);
  try
    ARowCount := 0;
    ARow := TdxSpreadSheetTableRowAccess(Sheet.Rows.First);
    while (ARow <> nil) and (ARow.Index < R.Top) do
      ARow := TdxSpreadSheetTableRowAccess(ARow.Next);
    while (ARow <> nil) and (ARow.Index <= R.Bottom) do
    begin
      ACellsCount := 0;
      AColumn := TdxSpreadSheetTableColumnAccess(Sheet.Columns.First);
      while (AColumn <> nil) and (AColumn.Index < R.Left) do
        AColumn := TdxSpreadSheetTableColumnAccess(AColumn.Next);
      while (AColumn <> nil) and (AColumn.Index <= R.Right) do
      begin
        if ARow.Cells[AColumn.Index] <> nil then
          Inc(ACellsCount)
        else
          if not (ARow.Style.IsDefault and AColumn.Style.IsDefault) then
          begin
            if FCache.Add(ARow.Style.Handle, AColumn.Style.Handle) then
            begin
              AStyle.Calculate(ARow, AColumn);
              AProcRef(AStyle, ARow.Index, AColumn.Index, R);
            end;
            Inc(ACellsCount);
          end;

        AColumn := TdxSpreadSheetTableColumnAccess(AColumn.Next);
      end;
      if ACellsCount <> R.Width + 1 then
      begin
        if FCache.Add(ARow.Style.Handle) then
          AProcRef(ARow.Style, ARow.Index, -1, R);
      end;
      ARow := TdxSpreadSheetTableRowAccess(ARow.Next);
      Inc(ARowCount);
    end;
  finally
    AStyle.Free;
  end;

  FHasEntireRowWithDefaultStyle := FHasEntireRowWithDefaultStyle or (ARowCount <> R.Height + 1);
  if FHasEntireRowWithDefaultStyle then
  begin
    ACellsCount := 0;
    AColumn := TdxSpreadSheetTableColumnAccess(Sheet.Columns.First);
    while (AColumn <> nil) and (AColumn.Index < R.Left) do
      AColumn := TdxSpreadSheetTableColumnAccess(AColumn.Next);
    while (AColumn <> nil) and (AColumn.Index <= R.Right) do
    begin
      if not AColumn.Style.IsDefault then
      begin
        AProcRef(AColumn.Style, -1, AColumn.Index, R);
        Inc(ACellsCount);
      end;
      AColumn := TdxSpreadSheetTableColumnAccess(AColumn.Next);
    end;
    if ACellsCount <> R.Width + 1 then
      AProcRef(Sheet.SpreadSheet.DefaultCellStyle, -1, -1, R);
  end;
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.EnumRowStyles(
  AArea: TRect; AProcRef: TdxSpreadSheetCellStyleEnumProcRef);
var
  ACount: Integer;
begin
  ACount := 0;
  TdxSpreadSheetTableRowsAccess(Sheet.Rows).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      ARow: TdxSpreadSheetTableRow;
    begin
      ARow := TdxSpreadSheetTableRow(AItem);
      AProcRef(ARow.Style, ARow.Index, -1, AArea);
      FHasEntireRowWithDefaultStyle := FHasEntireRowWithDefaultStyle or ARow.Style.IsDefault;
      Inc(ACount);
    end,
    AArea.Top, AArea.Bottom, True);

  FHasEntireRowWithDefaultStyle := FHasEntireRowWithDefaultStyle or (ACount <> AArea.Height + 1);
end;

{ TdxSpreadSheetTableViewEnumCellStylesHelper }

constructor TdxSpreadSheetTableViewEnumCellStylesHelper.TCache.Create;
begin
  inherited Create;
  FStyles := TList<TdxSpreadSheetCellStyleHandle>.Create;
  FCrossStyles := TObjectDictionary<TdxSpreadSheetCellStyleHandle, TList<TdxSpreadSheetCellStyleHandle>>.Create([doOwnsValues]);
end;

destructor TdxSpreadSheetTableViewEnumCellStylesHelper.TCache.Destroy;
begin
  FreeAndNil(FCrossStyles);
  FreeAndNil(FStyles);
  inherited Destroy;
end;

function TdxSpreadSheetTableViewEnumCellStylesHelper.TCache.Add(AStyle: TdxSpreadSheetCellStyleHandle): Boolean;
begin
  Result := FStyles.IndexOf(AStyle) < 0;
  if Result then
    FStyles.Add(AStyle);
end;

function TdxSpreadSheetTableViewEnumCellStylesHelper.TCache.Add(ARowStyle, AColumnStyle: TdxSpreadSheetCellStyleHandle): Boolean;
var
  AList: TList<TdxSpreadSheetCellStyleHandle>;
begin
  if FCrossStyles.TryGetValue(ARowStyle, AList) then
  begin
    Result := AList.IndexOf(AColumnStyle) < 0;
    if Result then
      AList.Add(AColumnStyle);
  end
  else
  begin
    AList := TList<TdxSpreadSheetCellStyleHandle>.Create;
    AList.Add(AColumnStyle);
    FCrossStyles.Add(ARowStyle, AList);
    Result := True;
  end;
end;

procedure TdxSpreadSheetTableViewEnumCellStylesHelper.TCache.Clear;
begin
  FCrossStyles.Clear;
  FStyles.Clear;
end;

{ TdxSpreadSheetTableViewPackHelper }

procedure TdxSpreadSheetTableViewPackHelper.Pack;
var
  AChanges: TdxSpreadSheetChanges;
begin
  View.Controller.CellHintController.Hide;

  FHasUnusedCells := False;
  RemoveUnusedCells;
  RemoveUnusedItems(Rows);
  RemoveUnusedItems(Columns);

  AChanges := [sscLayout];
  if FHasUnusedCells then
    Include(AChanges, sscData);
  TdxSpreadSheetTableViewAccess(View).AddChanges(AChanges);
end;

procedure TdxSpreadSheetTableViewPackHelper.RemoveUnusedCells;
begin
  View.ForEachCell(
    procedure (ACell: TdxSpreadSheetCell)
    begin
      if ACell.IsEmpty and not ACell.IsMerged then
      begin
        if (ACell.StyleHandle = ACell.Row.Style.Handle) and
           (not ACell.Row.Style.IsDefault or ACell.Column.Style.IsDefault) or
           (ACell.StyleHandle = ACell.Column.Style.Handle) and ACell.Row.Style.IsDefault
        then
          if not View.Containers.IsCellUsed(ACell) then
          begin
            FHasUnusedCells := True;
            ACell.Free;
          end;
      end;
    end);
end;

procedure TdxSpreadSheetTableViewPackHelper.RemoveUnusedItems(AItems: TdxSpreadSheetTableItems);
begin
  TdxSpreadSheetTableItemsAccess(AItems).ForEach(
    procedure(AItem: TdxDynamicListItem)
    var
      ATableItem: TdxSpreadSheetTableItem;
      I: Integer;
    begin
      ATableItem := TdxSpreadSheetTableItem(AItem);
      if ATableItem.DefaultSize and ATableItem.Visible and ATableItem.Style.IsDefault then
      begin
        for I := 0 to ATableItem.CellCount - 1 do
        begin
          if ATableItem.Cells[I] <> nil then
            Exit;
        end;
        ATableItem.Free;
      end;
    end);
end;

{ TdxSpreadSheetPasteCellDataFromStreamHelper }

constructor TdxSpreadSheetPasteCellDataFromStreamHelper.Create(AStream: TStream);
var
  AReader: TcxReader;
  ASection: Integer;
  AVersion: Integer;
  I: Integer;
begin
  inherited Create;

  AVersion := 0;
  AReader := TcxReader.Create(AStream);
  try
    FVersion := 7;
    if AReader.ReadCardinal > 0 then
    begin
      DataArea := AReader.ReadRect;
      for I := 0 to AReader.ReadInteger - 1 do
        MergedCells.Add(AReader.ReadRect);
      AReader.ReadMemoryStream(Styles);
      AReader.ReadMemoryStream(Cells);
      AReader.ReadMemoryStream(Containers);
      ArrayFormulasInfo.ReadData(AReader);
    end;

    ASection := AReader.ReadInteger;
    FOriginalArea := DataArea;
    if ASection = 2 then
    begin
      AReader.ReadInteger;
      AVersion := AReader.ReadInteger;
      FOriginalArea := AReader.ReadRect;
      if AVersion > 2 then
      begin
        FMode := TdxSpreadSheetClipboardCopyMode(AReader.ReadByte);
        FViewID := AReader.ReadWideString;
      end
      else
      begin
        FMode := ccmCopy;
        FViewID := '';
      end;
    end;

    if (AVersion > 3) and (AReader.ReadInteger = 3) then
      AReader.ReadMemoryStream(Comments);
  finally
    AReader.Free;
  end;
end;

procedure TdxSpreadSheetPasteCellDataFromStreamHelper.Copy(const AArea: TRect;
  AContainer: TdxSpreadSheetContainer; AMode: TdxSpreadSheetClipboardCopyMode; AView: TdxSpreadSheetTableView);
begin
  raise EdxSpreadSheetError.Create('Unavailable');
end;

procedure TdxSpreadSheetPasteCellDataFromStreamHelper.RestoreCells(AView: TdxSpreadSheetTableView;
  AStyles: TList<TdxSpreadSheetCellStyleHandle>; const AArea: TRect; AOptions: TdxSpreadSheetClipboardPasteOptions);
var
  ACell: TdxSpreadSheetCellAccess;
  ACellsReader: TcxReader;
  AColumn: Integer;
  AFormulas: TdxSpreadSheetFormulaAsTextInfoList;
  AHyperlink: TdxSpreadSheetHyperlinkAccess;
  ARow: Integer;
begin
  AFormulas := TdxSpreadSheetFormulaAsTextInfoList.Create(AView.SpreadSheet);
  ACellsReader := TcxReader.Create(Cells, FVersion);
  try
    for ARow := AArea.Top to AArea.Bottom do
      for AColumn := AArea.Left to AArea.Right do
      begin
        ACellsReader.ReadInteger;
        ACellsReader.ReadInteger;

        if ACellsReader.ReadBoolean then
        begin
          ACell := TdxSpreadSheetCellAccess(AView.CreateCell(ARow, AColumn));
          ACell.StyleHandle := AStyles[ACellsReader.ReadInteger];
          ACell.LoadFromStream(ACellsReader, AFormulas);
          if AView.Options.Protected then
            ACell.Style.Locked := False;
        end
        else
        begin
          ACell := TdxSpreadSheetCellAccess(AView.Cells[ARow, AColumn]);
          if ACell <> nil then
          begin
            ACell.Clear;
            ACell.StyleHandle := AView.SpreadSheet.DefaultCellStyle.Handle;
          end;
        end;

        if ACellsReader.ReadBoolean then
        begin
          AHyperlink := TdxSpreadSheetHyperlinkAccess(AView.Hyperlinks.Add(cxInvalidRect));
          AHyperlink.LoadFromStream(ACellsReader);
          AHyperlink.ReplaceArea(Rect(AColumn, ARow, AColumn, ARow));
        end;
      end;

    RestoreArrayFormulas(AView, AArea, AFormulas);
    AFormulas.ResolveReferences;
    MergeBordersForInsertedCells(AView, AArea);
  finally
    ACellsReader.Free;
    AFormulas.Free;
  end;
end;

{ TdxSpreadSheetDefinedNameHelper }

class procedure TdxSpreadSheetDefinedNameHelper.Enum(AView: TdxSpreadSheetCustomView; AProc: TdxSpreadSheetDefinedNameEnumProc);
var
  ADefinedName: TdxSpreadSheetDefinedName;
  AList: TdxSpreadSheetDefinedNames;
  I: Integer;
begin
  if AView <> nil then
  begin
    AList := AView.SpreadSheet.DefinedNames;
    for I := 0 to AList.Count - 1 do
    begin
      ADefinedName := AList[I];
      if ((ADefinedName.Scope = nil) or (ADefinedName.Scope = AView)) and ADefinedName.IsCellReference then
      begin
        if not IsInternalDefinedName(ADefinedName) then
          AProc(ADefinedName);
      end;
    end;
  end;
end;

class function TdxSpreadSheetDefinedNameHelper.FindByArea(ADefinedNames: TdxSpreadSheetDefinedNames;
  AView: TdxSpreadSheetCustomView; const AArea: TRect; out ADefinedName: TdxSpreadSheetDefinedName): Boolean;
var
  AScope: TdxSpreadSheetCustomView;
  I: Integer;
begin
  for I := 0 to ADefinedNames.Count - 1 do
  begin
    ADefinedName := ADefinedNames[I];
    if cxRectIsEqual(GetBoundingArea(ADefinedName, AScope), AArea) and (AScope = AView) then
    begin
      if not IsInternalDefinedName(ADefinedName) then
        Exit(True);
    end;
  end;
  Result := False;
end;

class function TdxSpreadSheetDefinedNameHelper.GetBoundingArea(ADefinedName: TdxSpreadSheetDefinedName): TRect;
var
  AView: TdxSpreadSheetCustomView;
begin
  Result := GetBoundingArea(ADefinedName, AView);
end;

class function TdxSpreadSheetDefinedNameHelper.GetBoundingArea(
  ADefinedName: TdxSpreadSheetDefinedName; out AView: TdxSpreadSheetCustomView): TRect;
var
  AAreBoundsValid: Boolean;
  ABounds: TRect;
  ATargetView: TdxSpreadSheetCustomView;
begin
  AAreBoundsValid := False;
  ATargetView := ADefinedName.Scope;
  ADefinedName.EnumReferences(
    procedure (const Area: TRect; View: TObject)
    begin
      if ATargetView = nil then
        ATargetView := TdxSpreadSheetCustomView(View);
      if ATargetView = View then
      begin
        if AAreBoundsValid then
          ABounds := dxSpreadSheetCellsUnion(ABounds, Area)
        else
        begin
          ABounds := Area;
          AAreBoundsValid := True;
        end;
      end;
    end);

  AView := ATargetView;
  if AAreBoundsValid then
    Result := ABounds
  else
    Result := cxInvalidRect;
end;

class function TdxSpreadSheetDefinedNameHelper.GetUsedAreas(
  ADefinedName: TdxSpreadSheetDefinedName; out AView: TdxSpreadSheetCustomView): TdxRectList;
var
  ARanges: TdxRectList;
  ATargetView: TdxSpreadSheetCustomView;
begin
  ARanges := TdxRectList.Create;
  ATargetView := ADefinedName.Scope;

  ADefinedName.EnumReferences(
    procedure (const AArea: TRect; AView: TObject)
    begin
      if ATargetView = nil then
        ATargetView := TdxSpreadSheetCustomView(AView);
      if AView = ATargetView then
        ARanges.Add(AArea);
    end);

  AView := ATargetView;
  Result := ARanges;
end;

class function TdxSpreadSheetDefinedNameHelper.IsInternalDefinedName(ADefinedName: TdxSpreadSheetDefinedName): Boolean;
begin
  Result := StartsText('_xlnm._', ADefinedName.Caption);
end;

{ TdxSpreadSheetExpression }

constructor TdxSpreadSheetExpression.Create(AView: TdxSpreadSheetTableView);
begin
  FView := AView;
  FController := TdxSpreadSheetExpressionController.Create(AView.SpreadSheet);
  inherited Create;
end;

destructor TdxSpreadSheetExpression.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FController);
end;

procedure TdxSpreadSheetExpression.Calculate(const AExpression: string);
var
  AFormulaParser: TdxSpreadSheetFormulaParser;
begin
  AFormulaParser := TdxSpreadSheetFormulaControllerAccess(Controller).CreateParser as TdxSpreadSheetFormulaParser;
  try
    if AFormulaParser.ParseFormula(AExpression, Self) then
    begin
      Controller.Add(Self);
      Controller.Calculate;
      Controller.Remove(Self);
    end;
  finally
    AFormulaParser.Free;
  end;
end;

function TdxSpreadSheetExpression.GetController: TdxSpreadSheetCustomFormulaController;
begin
  Result := FController;
end;

function TdxSpreadSheetExpression.GetView: TObject;
begin
  Result := FView;
end;

{ TdxSpreadSheetExpressionController }

constructor TdxSpreadSheetExpressionController.Create(ASpreadSheet: TdxCustomSpreadSheet);
begin
  inherited Create;
  FSpreadSheet := ASpreadSheet;
end;

function TdxSpreadSheetExpressionController.CreateParser: TObject;
begin
  Result := TdxSpreadSheetFormulaParser.Create(FSpreadSheet);
end;

function TdxSpreadSheetExpressionController.GetFormatSettings: TdxSpreadSheetFormatSettings;
begin
  Result := FSpreadSheet.FormulaController.FormatSettings;
end;

end.

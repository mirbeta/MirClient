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

unit cxLookupGrid;

{$I cxVer.inc}

interface

uses
  Types, Windows,
  SysUtils, Classes, Controls, Graphics, Forms, StdCtrls,
  dxCore, cxClasses, cxControls, cxContainer, cxGraphics, cxLookAndFeels,
  cxLookAndFeelPainters, cxDataUtils, cxDataStorage, cxCustomData, cxData, cxEdit,
  cxEditRepositoryItems;

const
  cxLookupGridColumnDefaultMinWidth = 20;
  // TODO: Common
//  cxGridCellTextOffset = 2; // ?
  cxGridEditOffset = 1;

type
  TcxCustomLookupGrid = class;
  TcxLookupGridColumn = class;

  TcxLookupGridHitTest = (htNone, htHeader, htCell);
  TcxLookupGridScrollMode = (smNone, smTop, smBottom);

  { TcxLookupGridDataController }

  TcxLookupGridDataController = class(TcxDataController)
  private
    function GetGrid: TcxCustomLookupGrid;
  public
    function GetItem(Index: Integer): TObject; override;
    property Grid: TcxCustomLookupGrid read GetGrid;
  published
    property OnCompare;
  end;

  { TcxLookupGridViewInfo }

  TcxLookupGridPartViewInfo = class
  public
    Bounds: TRect;
    ContentBounds: TRect;
  end;

  // Columns

  TcxLookupGridColumnViewInfo = class(TcxLookupGridPartViewInfo)
  private
    FEditViewData: TcxCustomEditViewData;
    FStyle: TcxCustomEditStyle;
  public
    Alignment: TAlignment;
    Borders: TcxBorders;
    Neighbors: TcxNeighbors;
    SortOrder: TcxDataSortOrder;
    Text: string;
    destructor Destroy; override;
    function CreateEditStyle(AProperties: TcxCustomEditProperties): TcxCustomEditStyle;
    function CreateEditViewData(AProperties: TcxCustomEditProperties): TcxCustomEditViewData;
    procedure DestroyEditViewData;
    property Style: TcxCustomEditStyle read FStyle;
  end;

  TcxLookupGridColumnsViewInfo = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxLookupGridColumnViewInfo;
  public
    property Items[Index: Integer]: TcxLookupGridColumnViewInfo read GetItem; default;
  end;

  // rows

  TcxLookupGridCellViewInfo = class(TcxLookupGridPartViewInfo)
  private
    FEditViewInfo: TcxCustomEditViewInfo;
  public
    Borders: TcxBorders;
    Index: Integer;
    IsFocused: Boolean;
    destructor Destroy; override;
    function CreateEditViewInfo(AProperties: TcxCustomEditProperties): TcxCustomEditViewInfo;
    property EditViewInfo: TcxCustomEditViewInfo read FEditViewInfo;
  end;

  TcxLookupGridRowViewInfo = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxLookupGridCellViewInfo;
  protected
    function AddCell(AIndex: Integer; const AInitBounds: TRect; AIsFocused: Boolean): TcxLookupGridCellViewInfo;
  public
    Borders: TcxBorders;
    Bounds: TRect;
    ContentBounds: TRect;
    IsFocused: Boolean;
    RecordIndex: Integer;
    RowIndex: Integer;
    property Items[Index: Integer]: TcxLookupGridCellViewInfo read GetItem; default;
  end;

  TcxLookupGridRowsViewInfo = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxLookupGridRowViewInfo;
  public
    function FindByRowIndex(ARowIndex: Integer): TcxLookupGridRowViewInfo;
    property Items[Index: Integer]: TcxLookupGridRowViewInfo read GetItem; default;
  end;

  TcxLookupGridTopRowIndexCalculation = (ticNone, ticForward, ticBackward);

  TcxLookupGridViewInfo = class
  private
    FColumns: TcxLookupGridColumnsViewInfo;
    FGrid: TcxCustomLookupGrid;
    FInternalTopRowIndex: Integer;
    FTopRowIndexCalculation: TcxLookupGridTopRowIndexCalculation;
    FRowMinHeight: Integer;
    FRows: TcxLookupGridRowsViewInfo;
    function GetBounds: TRect;
    function GetCanvas: TcxCanvas;
    function GetClientBounds: TRect;
    function GetEmptyAreaColor: TColor;
    function GetGridLines: TcxGridLines;
    function GetRowCount: Integer;
    function GetRowHeight: Integer;
    function GetTopRowIndex: Integer;
  protected
    function AddRow(ARowIndex: Integer; const AInitBounds: TRect): TcxLookupGridRowViewInfo;
    function CalcCellMinHeight(AIndex: Integer): Integer;
    function CalcRowMinHeight: Integer;
    procedure CalculateCells(ARowViewInfo: TcxLookupGridRowViewInfo);
    function GetCellHeight(ARowIndex, AColumnIndex: Integer): Integer;
    function GetHeaderHeight: Integer; virtual;
    property TopRowIndexCalculation: TcxLookupGridTopRowIndexCalculation read FTopRowIndexCalculation write FTopRowIndexCalculation;
  public
    BorderSize: Integer;
    HeadersRect: TRect;
    EmptyRectBottom, EmptyRectRight: TRect;
    PartialVisibleRowCount, VisibleRowCount: Integer;
    RowsRect: TRect;
    VisibleRowsRect: TRect;
    constructor Create(AGrid: TcxCustomLookupGrid); virtual;
    destructor Destroy; override;
    procedure CalcCellColors(ARowIsSelected, ACellIsSelected: Boolean; var AColor, AFontColor: TColor);
    procedure CalcColumns; virtual;
    procedure CalcEmptyAreas; virtual;
    procedure CalcHeaders; virtual;
    procedure CalcRows; virtual;
    procedure Calculate; virtual;
    function CheckTopRowIndex(ANewTopIndex: Integer): Integer; virtual;
    procedure CreateEditStyle(AColumnViewInfo: TcxLookupGridColumnViewInfo; AColumn: TcxLookupGridColumn); virtual;
    function CreateEditViewData(AColumnViewInfo: TcxLookupGridColumnViewInfo; AColumn: TcxLookupGridColumn): TcxCustomEditViewData; virtual;
    procedure DestroyEditViewData(AColumnViewInfo: TcxLookupGridColumnViewInfo; AColumn: TcxLookupGridColumn); virtual;
    procedure DoColumnsRightToLeftConversion; virtual;
    procedure DoEmptyAreasRightToLeftConversion; virtual;
    procedure DoRowRightToLeftConversion(ARowViewInfo: TcxLookupGridRowViewInfo); virtual;
    function GetContentColor: TColor; virtual;
    function GetContentFont: TFont; virtual;
    function GetContentFontColor: TColor; virtual;
    function GetGridColor: TColor; virtual;
    function GetGridLineWidth: Integer; virtual;
    function GetHeaderColor: TColor; virtual;
    function GetHeaderFont: TFont; virtual;
    function GetHeaderFontColor: TColor; virtual;
    function GetSelectedColor: TColor; virtual;
    function GetSelectedFontColor: TColor; virtual;

    property Bounds: TRect read GetBounds;
    property Canvas: TcxCanvas read GetCanvas;
    property ClientBounds: TRect read GetClientBounds;
    property Columns: TcxLookupGridColumnsViewInfo read FColumns;
    property EmptyAreaColor: TColor read GetEmptyAreaColor;
    property Grid: TcxCustomLookupGrid read FGrid;
    property GridLines: TcxGridLines read GetGridLines;
    property GridLineWidth: Integer read GetGridLineWidth;
    property RowCount: Integer read GetRowCount;
    property RowHeight: Integer read GetRowHeight;
    property Rows: TcxLookupGridRowsViewInfo read FRows;
    property TopRowIndex: Integer read GetTopRowIndex;
  end;

  TcxLookupGridViewInfoClass = class of TcxLookupGridViewInfo;

  { TcxLookupGridPainter }

  TcxLookupGridPainter = class
  private
    FCanvas: TcxCanvas;
    FGrid: TcxCustomLookupGrid;
    FLFPainterClass: TcxCustomLookAndFeelPainter;
    function GetCanvas: TcxCanvas;
    function GetViewInfo: TcxLookupGridViewInfo;
  protected
    procedure DrawBorder; virtual;
    procedure DrawCell(ACellViewInfo: TcxLookupGridCellViewInfo); virtual;
    procedure DrawContent; virtual;
    procedure DrawEmptyArea; virtual;
    procedure DrawHeaders; virtual;
    procedure DrawRow(ARowViewInfo: TcxLookupGridRowViewInfo); virtual;
    procedure DrawRows; virtual;
    property ViewInfo: TcxLookupGridViewInfo read GetViewInfo;
  public
    constructor Create(AGrid: TcxCustomLookupGrid); virtual;
    destructor Destroy; override;
    procedure Invalidate;
    procedure Paint;
    property Canvas: TcxCanvas read GetCanvas;
    property Grid: TcxCustomLookupGrid read FGrid;
    property LFPainterClass: TcxCustomLookAndFeelPainter read FLFPainterClass write FLFPainterClass;
  end;

  TcxLookupGridPainterClass = class of TcxLookupGridPainter;

  { TcxLookupGridColumn }

  TcxLookupGridDefaultValuesProvider = class(TcxCustomEditDefaultValuesProvider)
    function IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean; override;
  end;

  TcxLookupGridColumnClass = class of TcxLookupGridColumn;
  TcxLookupGridColumn = class(TcxInterfacedCollectionItem, IcxEditRepositoryItemListener)
  strict private
    FCaption: string;
    FDefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
    FFixed: Boolean;
    FHeaderAlignment: TAlignment;
    FInternalDefaultRepositoryItem: TcxEditRepositoryItem;
    FIsCaptionAssigned: Boolean;
    FIsWidthAssigned: Boolean;
    FMinWidth: Integer;
    FRepositoryItem: TcxEditRepositoryItem;
    FSorting: Boolean;
    FWidth: Integer;

    function GetCaption: string;
    function GetDataController: TcxCustomDataController;
    function GetGrid: TcxCustomLookupGrid;
    function GetMinWidth: Integer;
    function GetProperties: TcxCustomEditProperties;
    function GetSortOrder: TcxDataSortOrder;
    function GetWidth: Integer;
    function IsCaptionStored: Boolean;
    function IsWidthStored: Boolean;
    procedure SetCaption(const Value: string);
    procedure SetFixed(Value: Boolean);
    procedure SetHeaderAlignment(Value: TAlignment);
    procedure SetMinWidth(Value: Integer);
    procedure SetRepositoryItem(Value: TcxEditRepositoryItem);
    procedure SetSorting(Value: Boolean);
    procedure SetSortOrder(Value: TcxDataSortOrder);
    procedure SetWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer);
    // IcxEditRepositoryItemListener
    procedure ItemRemoved(Sender: TcxEditRepositoryItem);
    procedure PropertiesChanged(Sender: TcxEditRepositoryItem);
    // base
    procedure CheckWidthValue(var Value: Integer); virtual;
    function GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass; virtual;
    function GetValueTypeClass: TcxValueTypeClass; virtual;
    procedure SetIndex(Value: Integer); override;
    procedure SetValueTypeClass(Value: TcxValueTypeClass); virtual;
    property DataController: TcxCustomDataController read GetDataController;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function DefaultCaption: string; virtual;
    function DefaultRepositoryItem: TcxEditRepositoryItem; virtual;
    function DefaultWidth: Integer; virtual;
    function Equals(Obj: TObject): Boolean; override;
    function GetContentFont: TFont; virtual;
    function GetInternalDefaultRepositoryItem: TcxEditRepositoryItem; virtual;
    function GetRepositoryItem: TcxEditRepositoryItem; virtual;
    function IsLeft: Boolean; virtual;
    function IsRight: Boolean; virtual;
    procedure RestoreDefaults; virtual;

    property DefaultValuesProvider: TcxCustomEditDefaultValuesProvider read FDefaultValuesProvider;
    property Properties: TcxCustomEditProperties read GetProperties;
    property Grid: TcxCustomLookupGrid read GetGrid;
    property ValueTypeClass: TcxValueTypeClass read GetValueTypeClass write SetValueTypeClass;
  published
    property Caption: string read GetCaption write SetCaption stored IsCaptionStored;
    property Fixed: Boolean read FFixed write SetFixed default False;
    property HeaderAlignment: TAlignment read FHeaderAlignment write SetHeaderAlignment default taLeftJustify;
    property MinWidth: Integer read GetMinWidth write SetMinWidth default cxLookupGridColumnDefaultMinWidth;
    property Sorting: Boolean read FSorting write SetSorting default True;
    property SortOrder: TcxDataSortOrder read GetSortOrder write SetSortOrder default soNone;
    property RepositoryItem: TcxEditRepositoryItem read FRepositoryItem write SetRepositoryItem;
    property Width: Integer read GetWidth write SetWidth stored IsWidthStored;
  end;

  { TcxLookupGridColumns }

  TcxLookupGridColumns = class(TCollection)
  private
    FGrid: TcxCustomLookupGrid;
    function GetColumn(Index: Integer): TcxLookupGridColumn;
    procedure SetColumn(Index: Integer; Value: TcxLookupGridColumn);
  protected
    procedure ChangeScale(M, D: Integer);
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AGrid: TcxCustomLookupGrid; AColumnClass: TcxLookupGridColumnClass); virtual;
    function Add: TcxLookupGridColumn;
    procedure BeginUpdate; override;
    procedure EndUpdate; override;
    procedure RestoreDefaults; virtual;
    property Grid: TcxCustomLookupGrid read FGrid;
    property Items[Index: Integer]: TcxLookupGridColumn read GetColumn write SetColumn; default;
  end;

  TcxLookupGridColumnsClass = class of TcxLookupGridColumns;

  { TcxCustomLookupGrid }

  TcxLookupGridChange = (lgcLayout, lgcData, lgcFocusedRow);
  TcxLookupGridChanges = set of TcxLookupGridChange;

  TcxLookupGridHitInfo = record
    HitTest: TcxLookupGridHitTest;
    RowIndex: Integer;
    ColumnIndex: Integer;
  end;

  TcxLookupGridOptions = class(TPersistent)
  private
    FColumnSorting: Boolean;
    FFocusRowOnMouseMove: Boolean;
    FGridLines: TcxGridLines;
    FHighlightIncrementalFilteringText: Boolean;
    FRowSelect: Boolean;
    FShowHeader: Boolean;
    FOnChanged: TNotifyEvent;
    function GetAnsiSort: Boolean;
    function GetCaseInsensitive: Boolean;
    procedure SetAnsiSort(Value: Boolean);
    procedure SetCaseInsensitive(Value: Boolean);
    procedure SetGridLines(Value: TcxGridLines);
    procedure SetRowSelect(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);
  protected
    FGrid: TcxCustomLookupGrid;
    procedure Changed; virtual;
  public
    constructor Create(AGrid: TcxCustomLookupGrid); virtual;
    procedure Assign(Source: TPersistent); override;
    property HighlightIncrementalFilteringText: Boolean read FHighlightIncrementalFilteringText write FHighlightIncrementalFilteringText;
    property Grid: TcxCustomLookupGrid read FGrid;
    property OnChanged: TNotifyEvent read FOnChanged write FOnChanged;
  published
    property AnsiSort: Boolean read GetAnsiSort write SetAnsiSort default False;
    property CaseInsensitive: Boolean read GetCaseInsensitive write SetCaseInsensitive default False;
    property ColumnSorting: Boolean read FColumnSorting write FColumnSorting default True;
    property FocusRowOnMouseMove: Boolean read FFocusRowOnMouseMove
      write FFocusRowOnMouseMove default True;
    property GridLines: TcxGridLines read FGridLines write SetGridLines default glBoth;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default True;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
  end;

  TcxLookupGridOptionsClass = class of TcxLookupGridOptions;

  TcxLookupGridCloseUpEvent = procedure (Sender: TObject; AAccept: Boolean) of object;

  TcxCustomLookupGrid = class(TcxControl)
  private
    FChanges: TcxLookupGridChanges;
    FColumns: TcxLookupGridColumns;
    FFocusedColumn: TcxLookupGridColumn;
    FIsPopupControl: Boolean;
    FLockCount: Integer;
    FPainter: TcxLookupGridPainter;
    FRowPressed: Boolean;
    FScrollMode: TcxLookupGridScrollMode;
    FScrollTimer: TcxTimer;
    FTopRowIndex: Integer;
    FViewInfo: TcxLookupGridViewInfo;
    FOnClick: TNotifyEvent;
    FOnCloseUp: TcxLookupGridCloseUpEvent;
    FOnDataChanged: TNotifyEvent;
    FOnFocusedRowChanged: TNotifyEvent;
    procedure CreateScrollTimer;
    procedure DestroyScrollTimer;
    function GetDataController: TcxCustomDataController;
    function GetFocusedColumn: TcxLookupGridColumn;
    function GetFocusedColumnIndex: Integer;
    function GetFocusedRowIndex: Integer;
    function GetRowCount: Integer;
    procedure SetColumns(Value: TcxLookupGridColumns);
    procedure SetDataController(Value: TcxCustomDataController);
    procedure SetFocusedColumn(Value: TcxLookupGridColumn);
    procedure SetFocusedColumnIndex(Value: Integer);
    procedure SetFocusedRowIndex(Value: Integer);
    procedure SetIsPopupControl(Value: Boolean);
    procedure SetOptions(Value: TcxLookupGridOptions);
    procedure SetTopRowIndex(Value: Integer);
    procedure ScrollTimerHandler(Sender: TObject);
  protected
    FDataController: TcxCustomDataController;
    FOptions: TcxLookupGridOptions;

    procedure ColorChanged; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Paint; override;
    function AllowDragAndDropWithoutFocus: Boolean; override;
    procedure BoundsChanged; override;
    procedure DoCancelMode; override;
    procedure FocusChanged; override;
    procedure FontChanged; override;
    function GetBorderSize: Integer; override;
    procedure InitControl; override;
    procedure InitScrollBarsParameters; override;
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;

    procedure AddColumn(AColumn: TcxLookupGridColumn); virtual;
    // scrollbars
    function AllowTouchScrollUIMode: Boolean; override;
    procedure Change(AChanges: TcxLookupGridChanges); virtual;
    procedure ChangeScaleEx(M, D: Integer; isDpiChange: Boolean); override;
    procedure CheckChanges;
    procedure CheckSetTopRowIndex(var Value: Integer);
    procedure CheckTopRowIndex(ATopRowIndex: Integer; ANotUpdate: Boolean);
    procedure CreateHandlers; virtual;
    procedure CreateSubClasses; virtual;
    procedure DestroyHandlers; virtual;
    procedure DestroySubClasses; virtual;
    procedure DoCellClick(ARowIndex, AColumnIndex: Integer; AShift: TShiftState); virtual;
    procedure DoHeaderClick(AColumnIndex: Integer; AShift: TShiftState); virtual;
    function GetScrollContentForegroundColor: TColor; override;
    procedure FocusColumn(AColumnIndex: Integer);
    procedure FocusNextPage;
    procedure FocusNextRow(AGoForward: Boolean);
    procedure FocusPriorPage;
    function GetColumnClass: TcxLookupGridColumnClass; virtual;
    function GetColumnsClass: TcxLookupGridColumnsClass; virtual;
    function GetDataControllerClass: TcxCustomDataControllerClass; virtual;
    function GetLFPainterClass: TcxCustomLookAndFeelPainter; virtual;
    function GetOptionsClass: TcxLookupGridOptionsClass; virtual;
    function GetPainterClass: TcxLookupGridPainterClass; virtual;
    function GetScrollBarOffsetBegin: Integer; virtual;
    function GetScrollBarOffsetEnd: Integer; virtual;
    function GetViewInfoClass: TcxLookupGridViewInfoClass; virtual;
    function IsHotTrack: Boolean; virtual;
    procedure LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues); override;
    procedure RemoveColumn(AColumn: TcxLookupGridColumn); virtual;
    procedure SetScrollMode(Value: TcxLookupGridScrollMode); virtual;
    procedure ShowNextPage;
    procedure ShowPrevPage;
    procedure UpdateFocusing; virtual;
    procedure UpdateRowInfo(ARowIndex: Integer; ARecalculate: Boolean); virtual;
    procedure UpdateLayout; virtual;

    // Data Controller Notifications
    procedure DataChanged; virtual;
    procedure DataLayoutChanged; virtual;
    procedure DoClick; virtual;
    procedure DoCloseUp(AAccept: Boolean); virtual;
    procedure DoFocusedRowChanged; virtual;
    procedure FocusedRowChanged(APrevFocusedRowIndex, AFocusedRowIndex: Integer); virtual;
    procedure LayoutChanged; virtual;
    procedure SelectionChanged(AInfo: TcxSelectionChangedInfo); virtual;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); virtual;

    property Color default clWindow;
    property ParentColor default False;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function GetHitInfo(P: TPoint): TcxLookupGridHitInfo;
    function GetNearestPopupHeight(AHeight: Integer): Integer;
    function GetPopupHeight(ADropDownRowCount: Integer): Integer;
    function IsMouseOverList(const P: TPoint): Boolean;
    function IsRowVisible(ARowIndex: Integer): Boolean;
    procedure LockPopupMouseMove;
    procedure MakeFocusedRowVisible;
    procedure MakeRowVisible(ARowIndex: Integer);
    procedure SyncSelected(ASelected: Boolean); virtual;

    property Columns: TcxLookupGridColumns read FColumns write SetColumns;
    property DataController: TcxCustomDataController read GetDataController write SetDataController;
    property FocusedColumn: TcxLookupGridColumn read GetFocusedColumn write SetFocusedColumn;
    property FocusedColumnIndex: Integer read GetFocusedColumnIndex write SetFocusedColumnIndex;
    property FocusedRowIndex: Integer read GetFocusedRowIndex write SetFocusedRowIndex;
    property IsPopupControl: Boolean read FIsPopupControl write SetIsPopupControl;
    property LockCount: Integer read FLockCount;
    property LookAndFeel;
    property Options: TcxLookupGridOptions read FOptions write SetOptions;
    property Painter: TcxLookupGridPainter read FPainter;
    property RowCount: Integer read GetRowCount;
    property ScrollBarOffsetBegin: Integer read GetScrollBarOffsetBegin;
    property ScrollBarOffsetEnd: Integer read GetScrollBarOffsetEnd;
    property TopRowIndex: Integer read FTopRowIndex write SetTopRowIndex;
    property ViewInfo: TcxLookupGridViewInfo read FViewInfo;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
    property OnCloseUp: TcxLookupGridCloseUpEvent read FOnCloseUp write FOnCloseUp;
    property OnDataChanged: TNotifyEvent read FOnDataChanged write FOnDataChanged;
    property OnFocusedRowChanged: TNotifyEvent read FOnFocusedRowChanged write FOnFocusedRowChanged;
  end;

  TcxCustomLookupGridClass = class of TcxCustomLookupGrid;

  { TcxLookupGrid }

  TcxLookupGrid = class(TcxCustomLookupGrid)
  published
    property Align;
    property Anchors;
    property Color;
    property Font;
    property ParentFont;
    property Visible;

    property Columns;
    property DataController;
    property Options;
    property LookAndFeel;
  end;

implementation

uses
  Variants, cxEditRegisteredRepositoryItems, cxEditDataRegisteredRepositoryItems, cxTextEdit;

const
  ScrollTimerInterval = 50;
var
  FPrevMousePos: TPoint;

function PtInWidth(const R: TRect; P: TPoint): Boolean;
begin
  Result := (R.Left <= P.X) and (P.X < R.Right)
end;

{ TcxLookupGridColumnViewInfo }

destructor TcxLookupGridColumnViewInfo.Destroy;
begin
  FreeAndNil(FStyle);
  inherited Destroy;
end;

function TcxLookupGridColumnViewInfo.CreateEditStyle(AProperties: TcxCustomEditProperties): TcxCustomEditStyle;
begin
  FStyle := AProperties.GetStyleClass.Create(nil, True) as TcxCustomEditStyle;
  FStyle.ButtonTransparency := ebtHideInactive;
  Result := FStyle;
end;

function TcxLookupGridColumnViewInfo.CreateEditViewData(AProperties: TcxCustomEditProperties): TcxCustomEditViewData;
begin
  FEditViewData := AProperties.CreateViewData(FStyle, True);
  Result := FEditViewData;
end;

procedure TcxLookupGridColumnViewInfo.DestroyEditViewData;
begin
  FreeAndNil(FEditViewData);
end;

{ TcxLookupGridColumnsViewInfo }

function TcxLookupGridColumnsViewInfo.GetItem(Index: Integer): TcxLookupGridColumnViewInfo;
begin
  Result := TcxLookupGridColumnViewInfo(inherited Items[Index]);
end;

{ TcxLookupGridCellViewInfo }

destructor TcxLookupGridCellViewInfo.Destroy;
begin
  FreeAndNil(FEditViewInfo);
  inherited Destroy;
end;

function TcxLookupGridCellViewInfo.CreateEditViewInfo(AProperties: TcxCustomEditProperties): TcxCustomEditViewInfo;
begin
  if FEditViewInfo <> nil then FEditViewInfo.Free;
  FEditViewInfo := AProperties.GetViewInfoClass.Create as TcxCustomEditViewInfo;
  Result := FEditViewInfo;
end;

{ TcxLookupGridRowViewInfo }

function TcxLookupGridRowViewInfo.AddCell(AIndex: Integer; const AInitBounds: TRect;
  AIsFocused: Boolean): TcxLookupGridCellViewInfo;
begin
  Result := TcxLookupGridCellViewInfo.Create;
  Add(Result);
  Result.Index := AIndex;
  Result.IsFocused := AIsFocused;
  Result.Bounds := AInitBounds;
end;

function TcxLookupGridRowViewInfo.GetItem(Index: Integer): TcxLookupGridCellViewInfo;
begin
  Result := TcxLookupGridCellViewInfo(inherited Items[Index]);
end;

{ TcxLookupGridRowsViewInfo }

function TcxLookupGridRowsViewInfo.FindByRowIndex(ARowIndex: Integer): TcxLookupGridRowViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].RowIndex = ARowIndex then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxLookupGridRowsViewInfo.GetItem(Index: Integer): TcxLookupGridRowViewInfo;
begin
  Result := TcxLookupGridRowViewInfo(inherited Items[Index]);
end;

{ TcxLookupGridViewInfo }

constructor TcxLookupGridViewInfo.Create(AGrid: TcxCustomLookupGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FColumns := TcxLookupGridColumnsViewInfo.Create;
  FRows := TcxLookupGridRowsViewInfo.Create;
end;

destructor TcxLookupGridViewInfo.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FColumns);
  inherited Destroy;
end;

procedure TcxLookupGridViewInfo.CalcHeaders;

  procedure CreateItems;
  var
    I: Integer;
    AItem: TcxLookupGridColumnViewInfo;
  begin
    for I := 0 to Grid.Columns.Count - 1 do
    begin
      AItem := TcxLookupGridColumnViewInfo.Create;
      with AItem do
      begin
        Alignment := Grid.Columns[I].HeaderAlignment;
        Neighbors := [];
        if not Grid.Columns[I].IsLeft then
          Neighbors := Neighbors + [nLeft];
        if not Grid.Columns[I].IsRight then
          Neighbors := Neighbors + [nRight];
        Borders := Grid.Painter.LFPainterClass.HeaderBorders(Neighbors);
        SortOrder := Grid.Columns[I].SortOrder;
        Text := Grid.Columns[I].Caption;
      end;
      CreateEditStyle(AItem, Grid.Columns[I]);
      FColumns.Add(AItem);
    end;
  end;

  procedure CalcBounds;
  var
    I, ALeft: Integer;
    AAutoWidthObject: TcxAutoWidthObject;
    AItem: TcxLookupGridColumnViewInfo;
  begin
    AAutoWidthObject := TcxAutoWidthObject.Create(Grid.Columns.Count);
    try
      for I := 0 to Grid.Columns.Count - 1 do
      begin
        with AAutoWidthObject.AddItem do
        begin
          MinWidth := Grid.Columns[I].MinWidth;
          Width := Grid.Columns[I].Width;
          Fixed := Grid.Columns[I].Fixed;
        end;
      end;
      AAutoWidthObject.AvailableWidth := HeadersRect.Right - HeadersRect.Left;
      AAutoWidthObject.Calculate;
      ALeft := HeadersRect.Left;
      for I := 0 to Grid.Columns.Count - 1 do
      begin
        AItem := Columns[I];
        with AItem do
        begin
          Bounds := Rect(ALeft, HeadersRect.Top, ALeft + AAutoWidthObject[I].AutoWidth, HeadersRect.Bottom);
          ALeft := Bounds.Right;
          ContentBounds := Grid.Painter.LFPainterClass.ScaledHeaderContentBounds(Bounds, Borders, Grid.ScaleFactor);
        end;
      end;
      if ALeft < HeadersRect.Right then
        HeadersRect.Right := ALeft;
    finally
      AAutoWidthObject.Free;
    end;
  end;

begin
  CreateItems;
  CalcBounds;
end;

procedure TcxLookupGridViewInfo.CalcEmptyAreas;
begin
  if HeadersRect.Right < ClientBounds.Right then
    EmptyRectRight := Rect(HeadersRect.Right, ClientBounds.Top, ClientBounds.Right, ClientBounds.Bottom)
  else
    SetRectEmpty(EmptyRectRight);

  if RowsRect.Bottom < ClientBounds.Bottom then
    EmptyRectBottom := Rect(ClientBounds.Left, RowsRect.Bottom, ClientBounds.Right, ClientBounds.Bottom)
  else
    SetRectEmpty(EmptyRectBottom);
end;

procedure TcxLookupGridViewInfo.CalcCellColors(ARowIsSelected, ACellIsSelected: Boolean; var AColor, AFontColor: TColor);
begin
  if ARowIsSelected and not ACellIsSelected then
  begin
    AColor := GetSelectedColor;
    AFontColor := GetSelectedFontColor;
  end
  else
  begin
    AColor := GetContentColor;
    AFontColor := GetContentFontColor;
  end;
end;

procedure TcxLookupGridViewInfo.CalcColumns;
begin
  FColumns.Clear;
  if Grid.Columns.Count > 0 then
  begin
    HeadersRect := ClientBounds;
    if Grid.Options.ShowHeader then
      HeadersRect.Bottom := HeadersRect.Top + GetHeaderHeight
    else
      HeadersRect.Bottom := HeadersRect.Top;
    CalcHeaders;
  end
  else
    SetRectEmpty(HeadersRect);
end;

procedure TcxLookupGridViewInfo.CalcRows;

  procedure CalcCells(ARowIndex: Integer; var ATop: Integer);
  var
    I, ACellHeight, ARowHeight: Integer;
    ARect: TRect;
    ARowViewInfo: TcxLookupGridRowViewInfo;
    ACellViewInfo: TcxLookupGridCellViewInfo;

    function ExistEmptyArea: Boolean;
    begin
      Result := (TopRowIndexCalculation = ticNone) and
        (ARowViewInfo.Bounds.Bottom <> ClientBounds.Bottom);
    end;

  begin
    ARowViewInfo := AddRow(ARowIndex, Rect(RowsRect.Left, ATop,  RowsRect.Right, ATop));
    // Init Cells
    ARowHeight := 0;
    for I := 0 to Grid.Columns.Count - 1 do
    begin
      ACellHeight := GetCellHeight(ARowIndex, I);
      if ACellHeight > ARowHeight then
        ARowHeight := ACellHeight;
      with Columns[I].Bounds do
        ARect := Rect(Left, ATop, Right, ATop);
      ARowViewInfo.AddCell(I, ARect, Grid.FocusedColumnIndex = I);
    end;
    // Correct Bottom + Calc Content
    ARowViewInfo.Bounds.Bottom := ATop + ARowHeight;
    ARowViewInfo.ContentBounds := ARowViewInfo.Bounds;
    for I := 0 to ARowViewInfo.Count - 1 do
    begin
      ACellViewInfo := ARowViewInfo[I];
      ACellViewInfo.Bounds.Bottom := ARowViewInfo.Bounds.Bottom;
      ACellViewInfo.ContentBounds := ACellViewInfo.Bounds;
      if (GridLines in [glBoth, glVertical]) or
        ((GridLines = glHorizontal) and (I = (ARowViewInfo.Count - 1))) then
      begin
        Dec(ACellViewInfo.ContentBounds.Right, GetGridLineWidth);
        if I = (ARowViewInfo.Count - 1) then
        begin
          Dec(ARowViewInfo.ContentBounds.Right, GetGridLineWidth);
          Include(ARowViewInfo.Borders, bRight);
        end
        else
          Include(ACellViewInfo.Borders, bRight);
      end;
    end;
    if (GridLines in [glBoth, glHorizontal]) or
      ((GridLines = glVertical) and (ARowIndex = (RowCount - 1)) and ExistEmptyArea) then
    begin
      Inc(ARowViewInfo.Bounds.Bottom, GetGridLineWidth);
      Include(ARowViewInfo.Borders, bBottom);
    end;
    RowsRect.Bottom := ARowViewInfo.Bounds.Bottom;
    ATop := RowsRect.Bottom;
    if Grid.UseRightToLeftAlignment then
      DoRowRightToLeftConversion(ARowViewInfo);
    CalculateCells(ARowViewInfo);
  end;

var
  I, ATop: Integer;
begin
  FRows.Clear;
  SetRectEmpty(RowsRect);
  SetRectEmpty(VisibleRowsRect);
  PartialVisibleRowCount := 0;
  VisibleRowCount := 0;
  if (HeadersRect.Right - HeadersRect.Left) > 0 then
  begin
    FRowMinHeight := CalcRowMinHeight;
    RowsRect := Rect(HeadersRect.Left, HeadersRect.Bottom, HeadersRect.Right, HeadersRect.Bottom);
    VisibleRowsRect := RowsRect;
    ATop := RowsRect.Top;
    if TopRowIndexCalculation = ticBackward  then
    begin
      for I := TopRowIndex downto 0 do
      begin
        CalcCells(I, ATop);
        Inc(PartialVisibleRowCount);
        if RowsRect.Bottom <= ClientBounds.Bottom then
        begin
          Inc(VisibleRowCount);
          VisibleRowsRect.Bottom := RowsRect.Bottom;
        end
        else
          Break;
      end;
    end
    else
    begin
      for I := TopRowIndex to RowCount - 1 do
      begin
        CalcCells(I, ATop);
        Inc(PartialVisibleRowCount);
        if RowsRect.Bottom <= ClientBounds.Bottom then
        begin
          Inc(VisibleRowCount);
          VisibleRowsRect.Bottom := RowsRect.Bottom;
        end
        else
          Break;
      end;
    end;
    if (PartialVisibleRowCount > 0) and (VisibleRowCount = 0) then
      VisibleRowCount := 1;
  end;
end;

procedure TcxLookupGridViewInfo.Calculate;
begin
  BorderSize := FGrid.GetBorderSize;
  CalcColumns;
  CalcRows;
  CalcEmptyAreas;
  if Grid.UseRightToLeftAlignment then
  begin
    DoColumnsRightToLeftConversion;
    DoEmptyAreasRightToLeftConversion;
  end;
end;

function TcxLookupGridViewInfo.CheckTopRowIndex(ANewTopIndex: Integer): Integer;
begin
  TopRowIndexCalculation := ticForward;
  try
    FInternalTopRowIndex := ANewTopIndex;
    Calculate;
    if not IsRectEmpty(EmptyRectBottom) then
    begin
      TopRowIndexCalculation := ticBackward;
      try
        FInternalTopRowIndex := ANewTopIndex + VisibleRowCount - 1;
        if FInternalTopRowIndex > (RowCount - 1) then
          FInternalTopRowIndex := RowCount - 1;
        Calculate;
        ANewTopIndex := FInternalTopRowIndex - VisibleRowCount + 1;
      finally
        TopRowIndexCalculation := ticNone;
      end;
    end;
  finally
    TopRowIndexCalculation := ticNone;
  end;
  Result := ANewTopIndex;
end;

function TcxLookupGridViewInfo.AddRow(ARowIndex: Integer; const AInitBounds: TRect): TcxLookupGridRowViewInfo;
begin
  Result := TcxLookupGridRowViewInfo.Create;
  FRows.Add(Result);
  Result.RowIndex := ARowIndex;
  Result.RecordIndex := Grid.FDataController.GetRowInfo(ARowIndex).RecordIndex;
  Result.IsFocused := ARowIndex = Grid.FocusedRowIndex;
  Result.Bounds := AInitBounds;
end;

function TcxLookupGridViewInfo.CalcCellMinHeight(AIndex: Integer): Integer;
var
  AEditViewData: TcxCustomEditViewData;
begin
  AEditViewData := CreateEditViewData(Columns[AIndex], Grid.Columns[AIndex]);
  try
    Result := 2 * cxGridEditOffset +
      AEditViewData.GetEditSize(Canvas, Null, cxDefaultEditSizeProperties).cy;
  finally
    DestroyEditViewData(Columns[AIndex], Grid.Columns[AIndex]);
  end;
end;

function TcxLookupGridViewInfo.CalcRowMinHeight: Integer;
var
  I, ACellHeight: Integer;
begin
  Result := 0;
  for I := 0 to Grid.Columns.Count - 1 do
  begin
    ACellHeight := CalcCellMinHeight(I);
    if ACellHeight > Result then
      Result := ACellHeight;
  end;
  if Grid.Columns.Count > 0 then
    dxAdjustToTouchableSize(Result, Grid.ScaleFactor);
end;

procedure TcxLookupGridViewInfo.CalculateCells(ARowViewInfo: TcxLookupGridRowViewInfo);

  procedure CalcCell(ACellViewInfo: TcxLookupGridCellViewInfo);
  var
    AColor, AFontColor: TColor;
    ADisplayValue: Variant;
    AEditViewData: TcxCustomEditViewData;
    AEditViewInfo: TcxCustomEditViewInfo;
    ARect: TRect;
    ASelected: Boolean;
    ATextEditViewData: TcxCustomTextEditViewData;
    S: String;
    APos: Integer;
  begin
    // Style
    ASelected := Grid.DataController.IsRowSelected(ARowViewInfo.RowIndex);
    CalcCellColors(ASelected and ARowViewInfo.IsFocused,
      ASelected and ACellViewInfo.IsFocused, AColor, AFontColor);
    with Columns[ACellViewInfo.Index].Style do
    begin
      StyleData.Color := AColor;
      StyleData.FontColor := AFontColor;
    end;
    // Calculate
    AEditViewInfo := ACellViewInfo.CreateEditViewInfo(Grid.Columns[ACellViewInfo.Index].Properties);
    AEditViewData := CreateEditViewData(Columns[ACellViewInfo.Index], Grid.Columns[ACellViewInfo.Index]);
    try
      AEditViewData.UseRightToLeftAlignment := Grid.UseRightToLeftAlignment;
      AEditViewData.UseRightToLeftReading := Grid.UseRightToLeftReading;
      AEditViewData.UseRightToLeftScrollBar := Grid.UseRightToLeftScrollBar;
      // Value
      if Grid.Columns[ACellViewInfo.Index].Properties.GetEditValueSource(False) = evsValue then
        ADisplayValue := Grid.FDataController.Values[ARowViewInfo.RecordIndex, ACellViewInfo.Index]
      else
        ADisplayValue := Grid.FDataController.DisplayTexts[ARowViewInfo.RecordIndex, ACellViewInfo.Index];
      // Calculate
      ARect := ACellViewInfo.ContentBounds;
      InflateRect(ARect, -cxGridEditOffset, -cxGridEditOffset);
      if Grid.Options.HighlightIncrementalFilteringText and (AEditViewData is TcxCustomTextEditViewData) then
      begin
        ATextEditViewData := TcxCustomTextEditViewData(AEditViewData);
        if Grid.DataController.HasIncrementalFilter and
          (ACellViewInfo.Index = Grid.DataController.GetIncrementalFilterField.Index) then
        begin
          S := Grid.DataController.GetIncrementalFilterText;
          APos := Pos(AnsiUpperCase(S), AnsiUpperCase(ADisplayValue));
          if APos > 0  then
          begin
            ATextEditViewData.SelStart := APos - 1;
            ATextEditViewData.SelLength := Length(S);
          end;
        end;
      end;
      AEditViewData.EditValueToDrawValue(ADisplayValue, AEditViewInfo);
      AEditViewData.Calculate(Canvas, ARect, Point(-1, -1), cxmbNone, [], AEditViewInfo, False);
    finally
      DestroyEditViewData(Columns[ACellViewInfo.Index], Grid.Columns[ACellViewInfo.Index]);
    end;
  end;

var
  I: Integer;
begin
  for I := 0 to ARowViewInfo.Count - 1 do
    CalcCell(ARowViewInfo[I]);
end;

function TcxLookupGridViewInfo.GetCellHeight(ARowIndex, AColumnIndex: Integer): Integer;
begin
  Result := FRowMinHeight;
end;

function TcxLookupGridViewInfo.GetContentColor: TColor;
begin
  Result := Grid.Color;
end;

function TcxLookupGridViewInfo.GetContentFont: TFont;
begin
  Result := Grid.Font;
end;

function TcxLookupGridViewInfo.GetContentFontColor: TColor;
begin
  Result := GetContentFont.Color;
end;

procedure TcxLookupGridViewInfo.CreateEditStyle(AColumnViewInfo: TcxLookupGridColumnViewInfo;
  AColumn: TcxLookupGridColumn);
begin
  AColumnViewInfo.CreateEditStyle(AColumn.Properties);
  with AColumnViewInfo.Style do
    StyleData.Font := AColumn.GetContentFont;
end;

function TcxLookupGridViewInfo.CreateEditViewData(AColumnViewInfo: TcxLookupGridColumnViewInfo;
  AColumn: TcxLookupGridColumn): TcxCustomEditViewData;
begin
  with AColumn.Properties do
  begin
    LockUpdate(True);
    try
      IDefaultValuesProvider := AColumn.DefaultValuesProvider;
    finally
      LockUpdate(False);
    end;
  end;
  Result := AColumnViewInfo.CreateEditViewData(AColumn.Properties);
end;

procedure TcxLookupGridViewInfo.DestroyEditViewData(AColumnViewInfo: TcxLookupGridColumnViewInfo;
  AColumn: TcxLookupGridColumn);
begin
  AColumnViewInfo.DestroyEditViewData;
  with AColumn.Properties do
  begin
    LockUpdate(True);
    try
      IDefaultValuesProvider := nil;
    finally
      LockUpdate(False);
    end;
  end;
end;

procedure TcxLookupGridViewInfo.DoColumnsRightToLeftConversion;
var
  AColumnViewInfo: TcxLookupGridColumnViewInfo;
  I: Integer;
begin
  for I := 0 to FColumns.Count - 1 do
  begin
    AColumnViewInfo := FColumns[I];
    AColumnViewInfo.Borders := TdxRightToLeftLayoutConverter.ConvertBorders(AColumnViewInfo.Borders);
    AColumnViewInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(AColumnViewInfo.Bounds, ClientBounds);
    AColumnViewInfo.ContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(AColumnViewInfo.ContentBounds,
      ClientBounds);
    AColumnViewInfo.Neighbors := TdxRightToLeftLayoutConverter.ConvertNeighbors(AColumnViewInfo.Neighbors);
  end;
end;

procedure TcxLookupGridViewInfo.DoEmptyAreasRightToLeftConversion;
begin
  EmptyRectBottom := TdxRightToLeftLayoutConverter.ConvertRect(EmptyRectBottom, ClientBounds);
  EmptyRectRight := TdxRightToLeftLayoutConverter.ConvertRect(EmptyRectRight, ClientBounds);
end;

procedure TcxLookupGridViewInfo.DoRowRightToLeftConversion(ARowViewInfo: TcxLookupGridRowViewInfo);
var
  ACellViewInfo: TcxLookupGridCellViewInfo;
  I: Integer;
begin
  ARowViewInfo.Borders := TdxRightToLeftLayoutConverter.ConvertBorders(ARowViewInfo.Borders);
  ARowViewInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(ARowViewInfo.Bounds, ClientBounds);
  ARowViewInfo.ContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(ARowViewInfo.ContentBounds, ClientBounds);
  for I := 0 to ARowViewInfo.Count - 1 do
  begin
    ACellViewInfo := ARowViewInfo[I];
    ACellViewInfo.Borders := TdxRightToLeftLayoutConverter.ConvertBorders(ACellViewInfo.Borders);
    ACellViewInfo.Bounds := TdxRightToLeftLayoutConverter.ConvertRect(ACellViewInfo.Bounds, ClientBounds);
    ACellViewInfo.ContentBounds := TdxRightToLeftLayoutConverter.ConvertRect(ACellViewInfo.ContentBounds, ClientBounds);
  end;
end;

function TcxLookupGridViewInfo.GetGridColor: TColor;
begin
  Result := clBtnFace; // TODO: style
end;

function TcxLookupGridViewInfo.GetGridLineWidth: Integer;
begin
  Result := 1;
end;

function TcxLookupGridViewInfo.GetHeaderColor: TColor;
begin
  Result := Grid.Painter.LFPainterClass.DefaultHeaderColor;
end;

function TcxLookupGridViewInfo.GetHeaderFont: TFont;
begin
  Result := Grid.Font; // TODO: style
end;

function TcxLookupGridViewInfo.GetHeaderFontColor: TColor;
begin
  Result := Grid.Painter.LFPainterClass.DefaultHeaderTextColor;
end;

function TcxLookupGridViewInfo.GetSelectedColor: TColor;
begin
  Result := Grid.Painter.LFPainterClass.DefaultSelectionColor; // clHighlight;
end;

function TcxLookupGridViewInfo.GetSelectedFontColor: TColor;
begin
  Result := Grid.Painter.LFPainterClass.DefaultSelectionTextColor; // clHighlightText;
end;

function TcxLookupGridViewInfo.GetHeaderHeight: Integer;
begin
  Result := Grid.Painter.LFPainterClass.ScaledHeaderHeight(Canvas.FontHeight(GetHeaderFont), Grid.ScaleFactor);
end;

function TcxLookupGridViewInfo.GetBounds: TRect;
begin
  Result := FGrid.Bounds;
end;

function TcxLookupGridViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FGrid.Painter.Canvas;
end;

function TcxLookupGridViewInfo.GetClientBounds: TRect;
begin
  Result := FGrid.ClientBounds;
end;

function TcxLookupGridViewInfo.GetEmptyAreaColor: TColor;
begin
  Result := FGrid.Color;
end;

function TcxLookupGridViewInfo.GetGridLines: TcxGridLines;
begin
  Result := FGrid.Options.GridLines;
end;

function TcxLookupGridViewInfo.GetRowCount: Integer;
begin
  Result := Grid.RowCount;
end;

function TcxLookupGridViewInfo.GetRowHeight: Integer;
begin
  // TODO: RowAutoHeight
  Result := FRowMinHeight;
  if (Grid.Options.GridLines in [glBoth, glHorizontal]) then
    Inc(Result, GetGridLineWidth);
end;

function TcxLookupGridViewInfo.GetTopRowIndex: Integer;
begin
  if TopRowIndexCalculation <> ticNone  then
    Result := FInternalTopRowIndex
  else
    Result := Grid.TopRowIndex;
end;

{ TcxLookupGridPainter }

constructor TcxLookupGridPainter.Create(AGrid: TcxCustomLookupGrid);
begin
  inherited Create;
  FGrid := AGrid;
end;

destructor TcxLookupGridPainter.Destroy;
begin
  FreeAndNil(FCanvas);
  inherited Destroy;
end;

procedure TcxLookupGridPainter.Invalidate;
begin
  Grid.Invalidate;
end;

procedure TcxLookupGridPainter.Paint;
begin
  DrawBorder;
  DrawContent;
end;

procedure TcxLookupGridPainter.DrawBorder;
begin
  with ViewInfo do
    if BorderSize <> 0 then
    begin
      LFPainterClass.DrawBorder(Canvas, Bounds);
      Canvas.IntersectClipRect(ClientBounds);
    end;
end;

procedure TcxLookupGridPainter.DrawContent;
begin
  DrawHeaders;
  DrawEmptyArea;
  DrawRows;
end;

procedure TcxLookupGridPainter.DrawCell(ACellViewInfo: TcxLookupGridCellViewInfo);
begin
  ACellViewInfo.EditViewInfo.Paint(Canvas);
  Canvas.FrameRect(ACellViewInfo.ContentBounds, ACellViewInfo.EditViewInfo.BackgroundColor,
    cxGridEditOffset);
  Canvas.FrameRect(ACellViewInfo.Bounds, ViewInfo.GetGridColor,
    ViewInfo.GetGridLineWidth, ACellViewInfo.Borders);
end;

procedure TcxLookupGridPainter.DrawEmptyArea;
begin
  with ViewInfo do
    if not IsRectEmpty(EmptyRectBottom) or not IsRectEmpty(EmptyRectRight) then
    begin
      Canvas.Brush.Color := EmptyAreaColor;
      if not IsRectEmpty(EmptyRectBottom) then
        Canvas.FillRect(EmptyRectBottom);
      if not IsRectEmpty(EmptyRectRight) then
        Canvas.FillRect(EmptyRectRight);
    end;
end;

procedure TcxLookupGridPainter.DrawHeaders;
var
  AAlignment: TAlignment;
  R, ASortRect, ATextRect: TRect;
  ASortOrder: TcxDataSortOrder;
  I: Integer;
begin
  with ViewInfo do
    if not IsRectEmpty(HeadersRect) then
    begin
      for I := 0 to Columns.Count - 1 do
      begin
        R := Columns[I].Bounds;
        ATextRect := LFPainterClass.ScaledHeaderContentBounds(R, Columns[I].Borders, Grid.ScaleFactor);
        InflateRect(ATextRect, -cxHeaderTextOffset, -cxHeaderTextOffset);
        ASortOrder := Columns[I].SortOrder;
        if ASortOrder <> soNone then
        begin
          ASortRect := Rect(ATextRect.Right - LFPainterClass.ScaledSortingMarkAreaSize(Grid.ScaleFactor).X, ATextRect.Top, ATextRect.Right, ATextRect.Bottom);
          ATextRect.Right := ASortRect.Left;
          if ATextRect.Right < ATextRect.Left then
            ATextRect.Right := ATextRect.Left;
        end;
        AAlignment := Columns[I].Alignment;
        if Grid.UseRightToLeftAlignment then
        begin
          ATextRect := TdxRightToLeftLayoutConverter.ConvertRect(ATextRect, R);
          ASortRect := TdxRightToLeftLayoutConverter.ConvertRect(ASortRect, R);
          case AAlignment of
            taLeftJustify:
              AAlignment := taRightJustify;
            taRightJustify:
              AAlignment := taLeftJustify;
          end;
        end;
        LFPainterClass.DrawScaledHeader(Canvas, R, ATextRect, Columns[I].Neighbors, Columns[I].Borders,
          cxbsNormal, AAlignment, vaCenter, False, True, Columns[I].Text,
          GetHeaderFont, GetHeaderFontColor, GetHeaderColor, Grid.ScaleFactor);
        if ASortOrder <> soNone then
          LFPainterClass.DrawScaledSortingMark(Canvas, ASortRect, ASortOrder = soAscending, Grid.ScaleFactor);
      end;
    end;
end;

procedure TcxLookupGridPainter.DrawRow(ARowViewInfo: TcxLookupGridRowViewInfo);
var
  I: Integer;
  ACellViewInfo: TcxLookupGridCellViewInfo;
begin
  for I := 0 to ARowViewInfo.Count - 1 do
  begin
    ACellViewInfo := ARowViewInfo[I];
    Canvas.FrameRect(ACellViewInfo.Bounds, ViewInfo.GetGridColor,
      ViewInfo.GetGridLineWidth, ACellViewInfo.Borders);
    DrawCell(ACellViewInfo);
  end;
  Canvas.FrameRect(ARowViewInfo.Bounds, ViewInfo.GetGridColor,
    ViewInfo.GetGridLineWidth, ARowViewInfo.Borders);
  if ARowViewInfo.IsFocused and (Grid.Focused or Grid.IsPopupControl) then
    Canvas.DrawFocusRect(ARowViewInfo.ContentBounds);
end;

procedure TcxLookupGridPainter.DrawRows;
var
  I: Integer;
begin
  with ViewInfo do
    if not IsRectEmpty(RowsRect) then
      for I := 0 to PartialVisibleRowCount - 1 do
        DrawRow(Rows[I]);
end;

function TcxLookupGridPainter.GetCanvas: TcxCanvas;
begin
  if Grid.HandleAllocated then
  begin
    if FCanvas <> nil then
      FreeAndNil(FCanvas);
    Result := Grid.Canvas;
  end
  else
  begin
    if FCanvas = nil then
      FCanvas := TcxScreenCanvas.Create;
    Result := FCanvas;
  end;
end;

function TcxLookupGridPainter.GetViewInfo: TcxLookupGridViewInfo;
begin
  Result := Grid.ViewInfo;
end;

{ TcxLookupGridDefaultValuesProvider }

function TcxLookupGridDefaultValuesProvider.IsDisplayFormatDefined(AIsCurrencyValueAccepted: Boolean): Boolean;
begin
  with TcxLookupGridColumn(Owner) do
    Result := DataController.GetItemTextStored(Index);
end;

{ TcxLookupGridColumn }

constructor TcxLookupGridColumn.Create(Collection: TCollection);
var
  AGrid: TcxCustomLookupGrid;
begin
  if Assigned(Collection) and (Collection is TcxLookupGridColumns) then
    AGrid := TcxLookupGridColumns(Collection).Grid
  else
    AGrid := nil;
  if Assigned(AGrid) then
    AGrid.BeginUpdate;
  try
    inherited Create(Collection);
    FDefaultValuesProvider := GetDefaultValuesProviderClass.Create(Self);
    FMinWidth := cxLookupGridColumnDefaultMinWidth;
    FSorting := True;
    Changed(False);
    if AGrid <> nil then
      AGrid.AddColumn(Self);
  finally
    if Assigned(AGrid) then
      AGrid.EndUpdate;
  end;
end;

destructor TcxLookupGridColumn.Destroy;
var
  AGrid: TcxCustomLookupGrid;
  AGridNotify: Boolean;
begin
  AGrid := Grid;
  AGridNotify := False;
  if AGrid <> nil then
  begin
    AGridNotify := not (csDestroying in AGrid.ComponentState) {and
      not TcxLookupGridColumns(Collection).Locked};
    if AGridNotify then AGrid.BeginUpdate;
    AGrid.RemoveColumn(Self);
  end;
  try
    RepositoryItem := nil;
    FreeAndNil(FInternalDefaultRepositoryItem);
    FDefaultValuesProvider.Free;
    FDefaultValuesProvider := nil;
    inherited Destroy;
  finally
    if (AGrid <> nil) and AGridNotify then
      AGrid.EndUpdate;
  end;
end;

procedure TcxLookupGridColumn.Assign(Source: TPersistent);
begin
  if Source is TcxLookupGridColumn then
  begin
    if Assigned(Collection) then
      Collection.BeginUpdate;
    try
      RestoreDefaults;
      if TcxLookupGridColumn(Source).IsCaptionStored then
        Caption := TcxLookupGridColumn(Source).Caption;
      HeaderAlignment := TcxLookupGridColumn(Source).HeaderAlignment;
      MinWidth := TcxLookupGridColumn(Source).MinWidth;
      Fixed := TcxLookupGridColumn(Source).Fixed;
      Sorting := TcxLookupGridColumn(Source).Sorting;
      SortOrder := TcxLookupGridColumn(Source).SortOrder;
      RepositoryItem := TcxLookupGridColumn(Source).RepositoryItem;
      if TcxLookupGridColumn(Source).IsWidthStored then
        Width := TcxLookupGridColumn(Source).Width;
    finally
      if Assigned(Collection) then
        Collection.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxLookupGridColumn.DefaultCaption: string;
begin
  Result := '';
end;

function TcxLookupGridColumn.DefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := GetDefaultEditDataRepositoryItems.GetDefaultItem;
end;

function TcxLookupGridColumn.DefaultWidth: Integer;
begin
  Result := 64;
end;

function TcxLookupGridColumn.Equals(Obj: TObject): Boolean;
var
  ASource: TcxLookupGridColumn;
begin
  Result := Obj is TcxLookupGridColumn;
  if Result then
  begin
    ASource := TcxLookupGridColumn(Obj);
    Result := Result and (Caption = ASource.Caption) and (HeaderAlignment = ASource.HeaderAlignment) and
      (MinWidth = ASource.MinWidth) and (Fixed = ASource.Fixed) and (Sorting = ASource.Sorting) and
      (SortOrder = ASource.SortOrder) and (RepositoryItem = ASource.RepositoryItem) and (Width = ASource.Width);
  end;
end;

function TcxLookupGridColumn.GetContentFont: TFont;
begin
  if Grid <> nil then
    Result := Grid.ViewInfo.GetContentFont
  else
    Result := nil;
end;

function TcxLookupGridColumn.GetInternalDefaultRepositoryItem: TcxEditRepositoryItem;
begin
  Result := DefaultRepositoryItem;
  if Result = nil then
  begin
    if FInternalDefaultRepositoryItem = nil then
      FInternalDefaultRepositoryItem := TcxEditRepositoryTextItem.Create(nil);
    Result := FInternalDefaultRepositoryItem;
  end;
end;

function TcxLookupGridColumn.GetRepositoryItem: TcxEditRepositoryItem;
begin
  if RepositoryItem <> nil then
    Result := RepositoryItem
  else
    Result := GetInternalDefaultRepositoryItem;
end;

function TcxLookupGridColumn.IsLeft: Boolean;
begin
  Result := Index = 0;
end;

function TcxLookupGridColumn.IsRight: Boolean;
begin
  Result := Index = Collection.Count - 1;
end;

procedure TcxLookupGridColumn.RestoreDefaults;
begin
  FIsCaptionAssigned := False;
  FIsWidthAssigned := False;
  FHeaderAlignment := taLeftJustify;
  FMinWidth := cxLookupGridColumnDefaultMinWidth;
  FFixed := False;
  FSorting := True;
  Changed(False);
end;

procedure TcxLookupGridColumn.ChangeScale(M, D: Integer);
begin
  FMinWidth := MulDiv(FMinWidth, M, D);
  FWidth := MulDiv(FWidth, M, D);
end;

// IcxEditRepositoryItemListener

procedure TcxLookupGridColumn.ItemRemoved(Sender: TcxEditRepositoryItem);
begin
  RepositoryItem := nil;
end;

procedure TcxLookupGridColumn.PropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  Changed(False);
end;

procedure TcxLookupGridColumn.CheckWidthValue(var Value: Integer);
begin
  if Value < FMinWidth then
    Value := FMinWidth;
end;

function TcxLookupGridColumn.GetDefaultValuesProviderClass: TcxCustomEditDefaultValuesProviderClass;
begin
  Result := TcxLookupGridDefaultValuesProvider;
end;

function TcxLookupGridColumn.GetValueTypeClass: TcxValueTypeClass;
begin
  if Grid <> nil then
    Result := Grid.FDataController.GetItemValueTypeClass(Index)
  else
    Result := nil;
end;

procedure TcxLookupGridColumn.SetIndex(Value: Integer);
begin
  inherited SetIndex(Value);
  if Grid <> nil then
    Grid.FDataController.UpdateItemIndexes;
end;

procedure TcxLookupGridColumn.SetValueTypeClass(Value: TcxValueTypeClass);
begin
  if Grid <> nil then
    Grid.FDataController.ChangeValueTypeClass(Index, Value);
end;

function TcxLookupGridColumn.GetCaption: string;
begin
  if FIsCaptionAssigned then
    Result := FCaption
  else
    Result := DefaultCaption;
end;

function TcxLookupGridColumn.GetDataController: TcxCustomDataController;
begin
  Result := TcxCustomDataController(Grid.FDataController)
end;

function TcxLookupGridColumn.GetGrid: TcxCustomLookupGrid;
begin
  Result := TcxLookupGridColumns(Collection).Grid;
end;

function TcxLookupGridColumn.GetMinWidth: Integer;
begin
  Result := FMinWidth;
end;

function TcxLookupGridColumn.GetProperties: TcxCustomEditProperties;
begin
  Result := GetRepositoryItem.Properties;
end;

function TcxLookupGridColumn.GetSortOrder: TcxDataSortOrder;
begin
  if Grid <> nil then
    Result := Grid.FDataController.GetItemSortOrder(Index)
  else
    Result := soNone;
end;

function TcxLookupGridColumn.GetWidth: Integer;
begin
  if FIsWidthAssigned then
    Result := FWidth
  else
    Result := DefaultWidth;
end;

function TcxLookupGridColumn.IsCaptionStored: Boolean;
begin
  Result := FIsCaptionAssigned;
end;

function TcxLookupGridColumn.IsWidthStored: Boolean;
begin
  Result := FIsWidthAssigned;
end;

procedure TcxLookupGridColumn.SetCaption(const Value: string);
begin
  FCaption := Value;
  FIsCaptionAssigned := True;
  Changed(False);
end;

procedure TcxLookupGridColumn.SetFixed(Value: Boolean);
begin
  if FFixed <> Value then
  begin
    FFixed := Value;
    Changed(False);
  end;
end;

procedure TcxLookupGridColumn.SetHeaderAlignment(Value: TAlignment);
begin
  if FHeaderAlignment <> Value then
  begin
    FHeaderAlignment := Value;
    Changed(False);
  end;
end;

procedure TcxLookupGridColumn.SetMinWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FMinWidth <> Value then
  begin
    FMinWidth := Value;
    if Width < FMinWidth then
      Width := FMinWidth;
    Changed(False);
  end;
end;

procedure TcxLookupGridColumn.SetRepositoryItem(Value: TcxEditRepositoryItem);
begin
  if FRepositoryItem <> Value then
  begin
    if FRepositoryItem <> nil then
      FRepositoryItem.RemoveListener(Self);
    FRepositoryItem := Value;
    if FRepositoryItem <> nil then
      FRepositoryItem.AddListener(Self);
    PropertiesChanged(FRepositoryItem);
  end;
end;

procedure TcxLookupGridColumn.SetSorting(Value: Boolean);
begin
  if FSorting <> Value then
  begin
    FSorting := Value;
    Changed(False);
  end;
end;

procedure TcxLookupGridColumn.SetSortOrder(Value: TcxDataSortOrder);
begin
  if Grid <> nil then
    Grid.FDataController.ChangeSorting(Index, Value);
end;

procedure TcxLookupGridColumn.SetWidth(Value: Integer);
begin
  CheckWidthValue(Value);
  FWidth := Value;
  FIsWidthAssigned := True;
  Changed(False);
end;

{ TcxLookupGridColumns }

constructor TcxLookupGridColumns.Create(AGrid: TcxCustomLookupGrid;
  AColumnClass: TcxLookupGridColumnClass);
begin
  inherited Create(AColumnClass);
  FGrid := AGrid;
end;

function TcxLookupGridColumns.Add: TcxLookupGridColumn;
begin
  Result := TcxLookupGridColumn(inherited Add);
end;

procedure TcxLookupGridColumns.BeginUpdate;
begin
  if (Grid <> nil) and not (csDestroying in Grid.ComponentState) then
    Grid.BeginUpdate;
  inherited;
end;

procedure TcxLookupGridColumns.EndUpdate;
begin
  inherited;
  if (Grid <> nil) and not (csDestroying in Grid.ComponentState) then
    Grid.EndUpdate;
end;

procedure TcxLookupGridColumns.RestoreDefaults;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count-1 do
      Items[I].RestoreDefaults;
  finally
    EndUpdate;
  end;
end;

procedure TcxLookupGridColumns.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].ChangeScale(M, D);
  finally
    EndUpdate;
  end;
end;

function TcxLookupGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

procedure TcxLookupGridColumns.Update(Item: TCollectionItem);
begin
  if (FGrid = nil) or (csLoading in FGrid.ComponentState) then Exit;
  Grid.Change([lgcLayout]);
end;

function TcxLookupGridColumns.GetColumn(Index: Integer): TcxLookupGridColumn;
begin
  Result := TcxLookupGridColumn(inherited Items[Index]);
end;

procedure TcxLookupGridColumns.SetColumn(Index: Integer; Value: TcxLookupGridColumn);
begin
  Items[Index].Assign(Value);
end;

{ TcxLookupGridDataController }

function TcxLookupGridDataController.GetItem(Index: Integer): TObject;
begin
  Result := Grid.Columns[Index];
end;

function TcxLookupGridDataController.GetGrid: TcxCustomLookupGrid;
begin
  Result := GetOwner as TcxCustomLookupGrid;
end;

{ TcxLookupGridOptions }

constructor TcxLookupGridOptions.Create(AGrid: TcxCustomLookupGrid);
begin
  inherited Create;
  FGrid := AGrid;
  FColumnSorting := True;
  FFocusRowOnMouseMove := True;
  FGridLines := glBoth;
  FRowSelect := True;
  FShowHeader := True;
end;

procedure TcxLookupGridOptions.Assign(Source: TPersistent);
begin
  if Source is TcxLookupGridOptions then
  begin
    if Assigned(Grid) then
      Grid.BeginUpdate;
    try
      AnsiSort := TcxLookupGridOptions(Source).AnsiSort;
      CaseInsensitive := TcxLookupGridOptions(Source).CaseInsensitive;
      ColumnSorting := TcxLookupGridOptions(Source).ColumnSorting;
      FocusRowOnMouseMove := TcxLookupGridOptions(Source).FocusRowOnMouseMove;
      GridLines := TcxLookupGridOptions(Source).GridLines;
      RowSelect := TcxLookupGridOptions(Source).RowSelect;
      ShowHeader := TcxLookupGridOptions(Source).ShowHeader;
    finally
      if Assigned(Grid) then
        Grid.EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxLookupGridOptions.Changed;
begin
  if Assigned(Grid) then
    Grid.Change([lgcLayout]);
  if Assigned(FOnChanged) then
    FOnChanged(Self);
end;

function TcxLookupGridOptions.GetAnsiSort: Boolean;
begin
  if Assigned(Grid) then
    Result := dcoAnsiSort in Grid.DataController.Options
  else
    Result := False;
end;

function TcxLookupGridOptions.GetCaseInsensitive: Boolean;
begin
  if Assigned(Grid) then
    Result := dcoCaseInsensitive in Grid.DataController.Options
  else
    Result := False;
end;

procedure TcxLookupGridOptions.SetAnsiSort(Value: Boolean);
begin
  if Assigned(Grid) then
  begin
    if Value then
      Grid.DataController.Options := Grid.DataController.Options + [dcoAnsiSort]
    else
      Grid.DataController.Options := Grid.DataController.Options - [dcoAnsiSort];
  end;
end;

procedure TcxLookupGridOptions.SetCaseInsensitive(Value: Boolean);
begin
  if Assigned(Grid) then
  begin
    if Value then
      Grid.DataController.Options := Grid.DataController.Options + [dcoCaseInsensitive]
    else
      Grid.DataController.Options := Grid.DataController.Options - [dcoCaseInsensitive];
  end;
end;

procedure TcxLookupGridOptions.SetGridLines(Value: TcxGridLines);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    Changed;
  end;
end;

procedure TcxLookupGridOptions.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if Value and Assigned(Grid) then
      Grid.FocusedColumn := nil;
  end;
end;

procedure TcxLookupGridOptions.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    Changed;
  end;
end;

{ TcxCustomLookupGrid }

constructor TcxCustomLookupGrid.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FColumns := GetColumnsClass.Create(Self, GetColumnClass);
  FOptions := GetOptionsClass.Create(Self);
  CreateHandlers;
  CreateSubClasses;
  Color := clWindow;
  ParentColor := False;
  Width := 250;
  Height := 200;
  Keys := [kArrows];
end;

destructor TcxCustomLookupGrid.Destroy;
begin
  SetScrollMode(smNone);
  DestroySubClasses;
  DestroyHandlers;
  FOptions.Free;
  FColumns.Free;
  inherited Destroy;
end;

procedure TcxCustomLookupGrid.BeginUpdate;
begin
  Inc(FLockCount);
  FDataController.BeginUpdate;
end;

procedure TcxCustomLookupGrid.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TcxCustomLookupGrid.EndUpdate;
begin
  FDataController.EndUpdate;
  Dec(FLockCount);
  CheckChanges;
end;

function TcxCustomLookupGrid.GetHitInfo(P: TPoint): TcxLookupGridHitInfo;

  function CalcColumnIndex(out AColumnIndex: Integer): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to ViewInfo.Columns.Count - 1 do
      if PtInRect(ViewInfo.Columns[I].Bounds, P) then
      begin
        AColumnIndex := I;
        Result := True;
        Break;
      end;
  end;

  function CalcCellIndex(out ARowIndex, AColumnIndex: Integer): Boolean;
  var
    I, J: Integer;
  begin
    Result := False;
    for I := 0 to ViewInfo.Rows.Count - 1 do
      if PtInRect(ViewInfo.Rows[I].Bounds, P) then
      begin
        ARowIndex := ViewInfo.Rows[I].RowIndex;
        for J := 0 to ViewInfo.Rows[I].Count - 1 do
          if PtInWidth(ViewInfo.Rows[I][J].Bounds, P) then
          begin
            AColumnIndex := ViewInfo.Rows[I][J].Index;
            Result := True;
            Break;
          end;
        Break;
      end;
  end;

begin
  Result.HitTest := htNone;
  Result.RowIndex := -1;
  Result.ColumnIndex := -1;
  if not PtInRect(ViewInfo.ClientBounds, P) then Exit;
  if PtInRect(ViewInfo.HeadersRect, P) then
  begin
    if CalcColumnIndex(Result.ColumnIndex) then
      Result.HitTest := htHeader;
  end
  else
    if PtInRect(ViewInfo.RowsRect, P) then
    begin
      if CalcCellIndex(Result.RowIndex, Result.ColumnIndex) then
        Result.HitTest := htCell;
    end;
end;

function TcxCustomLookupGrid.GetNearestPopupHeight(AHeight: Integer): Integer;
var
  AHeaderHeight, ARowHeight, ARowCount: Integer;
begin
  AHeaderHeight := ViewInfo.HeadersRect.Bottom - ViewInfo.HeadersRect.Top;
  ARowHeight := ViewInfo.RowHeight;
  ARowCount := (AHeight - AHeaderHeight) div ARowHeight;
  if ARowCount <= 0 then
    ARowCount := 1
  else
    if ARowCount > GetRowCount then
      ARowCount := GetRowCount;
  if ARowCount < 1 then ARowCount := 1;
  Result := AHeaderHeight + ARowHeight * ARowCount;
end;

function TcxCustomLookupGrid.GetPopupHeight(ADropDownRowCount: Integer): Integer;
begin
  Result := ViewInfo.HeadersRect.Bottom - ViewInfo.HeadersRect.Top +
    ViewInfo.RowHeight * ADropDownRowCount;
end;

function TcxCustomLookupGrid.IsMouseOverList(const P: TPoint): Boolean;
begin
  Result := GetHitInfo(P).RowIndex <> -1;
end;

function TcxCustomLookupGrid.IsRowVisible(ARowIndex: Integer): Boolean;
begin
  with ViewInfo do
    Result := (VisibleRowCount > 0) and (Rows[0].RowIndex <= ARowIndex) and
      (ARowIndex <= Rows[VisibleRowCount - 1].RowIndex);
end;

procedure TcxCustomLookupGrid.LockPopupMouseMove;
begin
  FPrevMousePos := GetMouseCursorPos;
end;

procedure TcxCustomLookupGrid.MakeFocusedRowVisible;
begin
  if FocusedRowIndex <> -1 then
    MakeRowVisible(FocusedRowIndex);
end;

procedure TcxCustomLookupGrid.MakeRowVisible(ARowIndex: Integer);

  procedure SetBottomRowIndex(ARowIndex: Integer);
  begin
    TopRowIndex := ARowIndex - ViewInfo.VisibleRowCount + 1; // TODO: AutoHeight
  end;

begin
  if ViewInfo.VisibleRowCount > 0 then
  begin
    if ARowIndex < ViewInfo.Rows[0].RowIndex then
      TopRowIndex := ARowIndex
    else
      if ARowIndex > ViewInfo.Rows[ViewInfo.VisibleRowCount - 1].RowIndex then
        SetBottomRowIndex(ARowIndex);
  end;
end;

procedure TcxCustomLookupGrid.SyncSelected(ASelected: Boolean);
begin
  DataController.SyncSelected(ASelected);
end;

procedure TcxCustomLookupGrid.ColorChanged;
begin
  LayoutChanged;
  inherited ColorChanged;
end;

procedure TcxCustomLookupGrid.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  case Key of
    VK_LEFT:
      FocusColumn(FocusedColumnIndex - 1);
    VK_RIGHT:
      FocusColumn(FocusedColumnIndex + 1);
    VK_UP:
      FocusNextRow(False); // Grid mode
    VK_DOWN:
      FocusNextRow(True); // Grid mode
    VK_HOME:
      if (ssCtrl in Shift) or Options.RowSelect then
        DataController.GotoFirst
      else
        FocusColumn(0);
    VK_END:
      if (ssCtrl in Shift) or Options.RowSelect then
        DataController.GotoLast
      else
        FocusColumn(Columns.Count - 1);
    VK_PRIOR:
      FocusPriorPage;
    VK_NEXT:
      FocusNextPage;
    VK_RETURN:
      DoCloseUp(FocusedRowIndex <> -1);
  end;
end;

procedure TcxCustomLookupGrid.Loaded;
begin
  inherited Loaded;
  Change([lgcLayout]);
end;

procedure TcxCustomLookupGrid.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitInfo: TcxLookupGridHitInfo;
begin
  inherited MouseDown(Button, Shift, X, Y);
  AHitInfo := GetHitInfo(Point(X, Y));
  if AHitInfo.HitTest = htHeader then
    DoHeaderClick(AHitInfo.ColumnIndex, Shift)
  else
    if AHitInfo.HitTest = htCell then
    begin
      DoCellClick(AHitInfo.RowIndex, AHitInfo.ColumnIndex, Shift);
      FRowPressed := True;
    end;
end;

procedure TcxCustomLookupGrid.MouseMove(Shift: TShiftState; X, Y: Integer);
const
  ScrollModeA: array[Boolean] of TcxLookupGridScrollMode = (smTop, smBottom);
var
  AHitInfo: TcxLookupGridHitInfo;
  P: TPoint;
begin
  inherited MouseMove(Shift, X, Y);
  P := GetMouseCursorPos;
  if (P.X = FPrevMousePos.X) and (P.Y = FPrevMousePos.Y) then
    Exit;
  FPrevMousePos := P;
  if MouseCapture or IsHotTrack then
  begin
    AHitInfo := GetHitInfo(Point(X, Y));
    if FRowPressed and MouseCapture and ((Y < ViewInfo.VisibleRowsRect.Top) or
      (Y > ViewInfo.VisibleRowsRect.Bottom)) then
      SetScrollMode(ScrollModeA[Y > ViewInfo.VisibleRowsRect.Bottom])
    else
    begin
      SetScrollMode(smNone);
      if AHitInfo.HitTest = htCell then
      begin
        FocusedRowIndex := AHitInfo.RowIndex;
        SyncSelected(True);
      end;
    end;
  end;
end;

procedure TcxCustomLookupGrid.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  AHitInfo: TcxLookupGridHitInfo;
begin
  inherited MouseUp(Button, Shift, X, Y);
  SetScrollMode(smNone);
  AHitInfo := GetHitInfo(Point(X, Y));
  if FRowPressed then
    DoCloseUp(AHitInfo.HitTest = htCell);
  FRowPressed := False;
end;

procedure TcxCustomLookupGrid.Paint;
begin
  inherited Paint;
  FPainter.Paint;
end;

function TcxCustomLookupGrid.AllowDragAndDropWithoutFocus: Boolean;
begin
  Result := True;
end;

procedure TcxCustomLookupGrid.BoundsChanged;
begin
  LayoutChanged;
  inherited BoundsChanged;
end;

procedure TcxCustomLookupGrid.DoCancelMode;
begin
  DestroyScrollTimer;
  FRowPressed := False;
end;

procedure TcxCustomLookupGrid.FocusChanged;
begin
  UpdateFocusing;
  inherited FocusChanged;
end;

procedure TcxCustomLookupGrid.FontChanged;
begin
  inherited FontChanged;
  LayoutChanged;
end;

function TcxCustomLookupGrid.GetBorderSize: Integer;
begin
  if IsPopupControl then
    Result := 0
  else
    Result := Painter.LFPainterClass.BorderSize;
end;

procedure TcxCustomLookupGrid.InitControl;
begin
  inherited InitControl;
  LayoutChanged;
end;

procedure TcxCustomLookupGrid.InitScrollBarsParameters;
begin
  SetScrollBarInfo(sbVertical, 0, ViewInfo.RowCount - 1 + ScrollBarOffsetBegin + ScrollBarOffsetEnd,
    1, ViewInfo.VisibleRowCount, ScrollBarOffsetBegin + ViewInfo.TopRowIndex, True, True);
end;

procedure TcxCustomLookupGrid.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollBarKind = sbVertical then
  begin
    case AScrollCode of
      scLineUp:
        TopRowIndex := TopRowIndex - 1;
      scLineDown:
        TopRowIndex := TopRowIndex + 1;
      scPageUp:
        ShowPrevPage;
      scPageDown:
        ShowNextPage;
      scTrack:
        if not DataController.IsGridMode then
          TopRowIndex := AScrollPos;  // check in CLX!!!
      scPosition:
        if DataController.IsGridMode then
          TopRowIndex := AScrollPos - ScrollBarOffsetBegin;
    end;
    AScrollPos := ScrollBarOffsetBegin + TopRowIndex;
  end;
end;

procedure TcxCustomLookupGrid.AddColumn(AColumn: TcxLookupGridColumn);
begin
  if FDataController <> nil then
    FDataController.AddItem(AColumn);
end;

function TcxCustomLookupGrid.AllowTouchScrollUIMode: Boolean;
begin
  Result := not IsDesigning;
end;

procedure TcxCustomLookupGrid.Change(AChanges: TcxLookupGridChanges);
begin
  FChanges := FChanges + AChanges;
  CheckChanges;
end;

procedure TcxCustomLookupGrid.ChangeScaleEx(M, D: Integer; isDpiChange: Boolean);
begin
  inherited;
  Columns.ChangeScale(M, D);
end;

procedure TcxCustomLookupGrid.CheckChanges;
begin
  if (FChanges <> []) and (LockCount = 0) then
    try
      if FChanges * [lgcData, lgcLayout, lgcFocusedRow] <> [] then
        LayoutChanged;
    finally
      FChanges := [];
    end;
end;

procedure TcxCustomLookupGrid.CheckSetTopRowIndex(var Value: Integer);
var
  AMaxValue: Integer;
begin
  if DataController.IsGridMode then
  begin
    if Value < 0 then
    begin
      if not DataController.IsBOF then
        DataController.Scroll(Value);
      Value := 0;
    end
    else
      if Value >= RowCount then
      begin
        if not DataController.IsEOF then
          DataController.Scroll(Value - (RowCount - 1));
        Value := RowCount - 1;
      end
      else
      begin
        AMaxValue := RowCount - ViewInfo.VisibleRowCount; // TODO
        if Value > AMaxValue then
        begin
          if not DataController.IsEOF then
            DataController.Scroll(Value - AMaxValue);
//          AMaxValue := GetMaxValue;
          Value := AMaxValue;
        end;
      end;
  end;
  if Value >= RowCount then
    Value := RowCount - 1;
  if Value < 0 then
    Value := 0;
end;

procedure TcxCustomLookupGrid.CheckTopRowIndex(ATopRowIndex: Integer; ANotUpdate: Boolean);
var
  APrevTopRowIndex, ANewTopRowIndex: Integer;
begin
  APrevTopRowIndex := TopRowIndex;
  ANewTopRowIndex := ViewInfo.CheckTopRowIndex(ATopRowIndex);
  if APrevTopRowIndex <> ANewTopRowIndex then
  begin
    FTopRowIndex := ANewTopRowIndex;
    if not ANotUpdate then
      UpdateLayout;
    ShowTouchScrollUI(Self, True);
  end
  else
    ViewInfo.Calculate;
end;

procedure TcxCustomLookupGrid.CreateHandlers;
begin
  FPainter := GetPainterClass.Create(Self);
  FPainter.LFPainterClass := GetLFPainterClass;
  FViewInfo := GetViewInfoClass.Create(Self);
end;

function TcxCustomLookupGrid.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxLookupGridDataController;
end;

procedure TcxCustomLookupGrid.CreateSubClasses;
begin
  FDataController := GetDataControllerClass.Create(Self);
  FDataController.OnUpdateControl := UpdateControl;
end;

procedure TcxCustomLookupGrid.DestroyHandlers;
begin
  FreeAndNil(FViewInfo);
  FreeAndNil(FPainter);
end;

procedure TcxCustomLookupGrid.DestroySubClasses;
begin
  FreeAndNil(FDataController);
end;

procedure TcxCustomLookupGrid.DoCellClick(ARowIndex, AColumnIndex: Integer; AShift: TShiftState);
begin
  if ARowIndex <> -1 then
    FocusedRowIndex := ARowIndex;
  if AColumnIndex <> -1 then
    FocusedColumnIndex := AColumnIndex;
  SyncSelected(True);
end;

procedure TcxCustomLookupGrid.DoHeaderClick(AColumnIndex: Integer; AShift: TShiftState);
var
  ASortOrder: TcxDataSortOrder;
begin
  if not Options.ColumnSorting or (AColumnIndex = -1) or
    not Columns[AColumnIndex].Sorting then Exit;
  try
    BeginUpdate;
    try
      with Columns[AColumnIndex] do
        if ssCtrl in AShift then
          SortOrder := soNone
        else
        begin
          if SortOrder = soAscending then
            ASortOrder := soDescending
          else
            ASortOrder := soAscending;
          if not (ssShift in AShift) then
            FDataController.ClearSorting(True);
          SortOrder := ASortOrder;
        end;
    finally
      EndUpdate;
    end;
  finally
    MakeFocusedRowVisible;
  end;
end;

procedure TcxCustomLookupGrid.FocusColumn(AColumnIndex: Integer);
begin
  FocusedColumnIndex := AColumnIndex;
  MakeFocusedRowVisible;
end;

procedure TcxCustomLookupGrid.FocusNextPage;
begin
  MakeFocusedRowVisible;
  if FocusedRowIndex = TopRowIndex + ViewInfo.VisibleRowCount - 1 then
    ShowNextPage;
  FocusedRowIndex := TopRowIndex + ViewInfo.VisibleRowCount - 1;
end;

procedure TcxCustomLookupGrid.FocusNextRow(AGoForward: Boolean);
var
  AFocusedRowIndex: Integer;
begin
  AFocusedRowIndex := FocusedRowIndex;
  if DataController.IsGridMode then
  begin
    if AGoForward then
    begin
      if not DataController.IsEOF and (AFocusedRowIndex = (RowCount - 1)) then
      begin
        DataController.Scroll(1);
        if not DataController.IsEOF then
          Dec(AFocusedRowIndex);
      end;
    end
    else
    begin
      if (AFocusedRowIndex = 0) and not DataController.IsBOF then
      begin
        DataController.Scroll(-1);
        if not DataController.IsBOF then
          Inc(AFocusedRowIndex);
      end;
    end;
  end;
  if AGoForward then
    AFocusedRowIndex := AFocusedRowIndex + 1
  else
    AFocusedRowIndex := AFocusedRowIndex - 1;
  if AFocusedRowIndex < 0 then
    AFocusedRowIndex := 0;
  if AFocusedRowIndex >= RowCount then
    AFocusedRowIndex := RowCount - 1;
  FocusedRowIndex := AFocusedRowIndex;
  SyncSelected(True);
end;

procedure TcxCustomLookupGrid.FocusPriorPage;
begin
  MakeFocusedRowVisible;
  if FocusedRowIndex = TopRowIndex then
    ShowPrevPage;
  FocusedRowIndex := TopRowIndex;
end;

function TcxCustomLookupGrid.GetColumnClass: TcxLookupGridColumnClass;
begin
  Result := TcxLookupGridColumn;
end;

function TcxCustomLookupGrid.GetColumnsClass: TcxLookupGridColumnsClass;
begin
  Result := TcxLookupGridColumns;
end;

function TcxCustomLookupGrid.GetLFPainterClass: TcxCustomLookAndFeelPainter;
begin
  Result := LookAndFeel.Painter;
end;

function TcxCustomLookupGrid.GetOptionsClass: TcxLookupGridOptionsClass;
begin
  Result := TcxLookupGridOptions;
end;

function TcxCustomLookupGrid.GetPainterClass: TcxLookupGridPainterClass;
begin
  Result := TcxLookupGridPainter;
end;

function TcxCustomLookupGrid.GetScrollBarOffsetBegin: Integer;
begin
  if DataController.IsGridMode then
    Result := Ord(not DataController.IsBOF)
  else
    Result := 0;
end;

function TcxCustomLookupGrid.GetScrollBarOffsetEnd: Integer;
begin
  if DataController.IsGridMode then
    Result := Ord(not DataController.IsEOF)
  else
    Result := 0;
end;

function TcxCustomLookupGrid.GetViewInfoClass: TcxLookupGridViewInfoClass;
begin
  Result := TcxLookupGridViewInfo;
end;

function TcxCustomLookupGrid.IsHotTrack: Boolean;
begin
  Result := IsPopupControl and Options.FocusRowOnMouseMove;
end;

procedure TcxCustomLookupGrid.LookAndFeelChanged(Sender: TcxLookAndFeel; AChangedValues: TcxLookAndFeelValues);
begin
  inherited;
  Painter.LFPainterClass := GetLFPainterClass;
  Change([lgcLayout]);
end;

procedure TcxCustomLookupGrid.RemoveColumn(AColumn: TcxLookupGridColumn);
begin
  if FDataController <> nil then
    FDataController.RemoveItem(AColumn);
  if FFocusedColumn = AColumn then
    FFocusedColumn := nil; // TODO: prev/next
end;

procedure TcxCustomLookupGrid.SetScrollMode(Value: TcxLookupGridScrollMode);
begin
  if FScrollMode <> Value then
  begin
    DestroyScrollTimer;
    FScrollMode := Value;
    if FScrollMode <> smNone then
      CreateScrollTimer;
  end;
end;

procedure TcxCustomLookupGrid.ShowNextPage;
begin
  if ViewInfo.VisibleRowCount > 1 then
    TopRowIndex := TopRowIndex + ViewInfo.VisibleRowCount - 1
  else
    TopRowIndex := TopRowIndex + 1;
end;

procedure TcxCustomLookupGrid.ShowPrevPage;
begin
  if ViewInfo.VisibleRowCount > 1 then
    TopRowIndex := TopRowIndex - (ViewInfo.VisibleRowCount - 1) // TODO: AutoHeight
  else
    TopRowIndex := TopRowIndex - 1;
end;

procedure TcxCustomLookupGrid.UpdateFocusing;
begin
  UpdateRowInfo(FocusedRowIndex, False);
end;

procedure TcxCustomLookupGrid.UpdateRowInfo(ARowIndex: Integer; ARecalculate: Boolean);
var
  ARowViewInfo: TcxLookupGridRowViewInfo;
begin
  ARowViewInfo := ViewInfo.Rows.FindByRowIndex(ARowIndex);
  if ARowViewInfo <> nil then
  begin
    if ARecalculate then
    begin
      ARowViewInfo.IsFocused := ARowViewInfo.RowIndex = FocusedRowIndex;
      ViewInfo.CalculateCells(ARowViewInfo);
    end;
    InvalidateRect(ARowViewInfo.Bounds, False);
  end;
end;

procedure TcxCustomLookupGrid.UpdateLayout;
begin
  if HandleAllocated then
  begin
    ViewInfo.Calculate;
    Painter.Invalidate;
    UpdateScrollBars;
  end;
end;

procedure TcxCustomLookupGrid.DataChanged;
begin
  // TODO:
  LayoutChanged;
  if Assigned(FOnDataChanged) then
    FOnDataChanged(Self);
end;

procedure TcxCustomLookupGrid.DataLayoutChanged;
begin
  // TODO:
  LayoutChanged;
end;

procedure TcxCustomLookupGrid.DoClick;
begin
  if Assigned(FOnClick) then
    FOnClick(Self);
end;

procedure TcxCustomLookupGrid.DoCloseUp(AAccept: Boolean);
begin
  if Assigned(FOnCloseUp) then
    FOnCloseUp(Self, AAccept);
end;

procedure TcxCustomLookupGrid.DoFocusedRowChanged;
begin
  if Assigned(FOnFocusedRowChanged) then
    FOnFocusedRowChanged(Self);
end;

procedure TcxCustomLookupGrid.FocusedRowChanged(APrevFocusedRowIndex, AFocusedRowIndex: Integer);
begin
  if IsRowVisible(AFocusedRowIndex) then
  begin
    UpdateRowInfo(APrevFocusedRowIndex, True);
    UpdateRowInfo(AFocusedRowIndex, True);
  end
  else
  begin
    LayoutChanged;
    MakeFocusedRowVisible;
  end;
end;

procedure TcxCustomLookupGrid.LayoutChanged;
begin
  CheckTopRowIndex(TopRowIndex, True);
  UpdateLayout;
end;

procedure TcxCustomLookupGrid.SelectionChanged(AInfo: TcxSelectionChangedInfo);
var
  I: Integer;
begin
  if AInfo.Count = 0 then
    LayoutChanged
  else
    for I := 0 to AInfo.Count - 1 do
      UpdateRowInfo(AInfo.RowIndexes[I], True);
end;

procedure TcxCustomLookupGrid.UpdateControl(AInfo: TcxUpdateControlInfo);
begin
  if AInfo is TcxDataChangedInfo then
    DataChanged
  else
    if AInfo is TcxLayoutChangedInfo then
      DataLayoutChanged
    else
      if AInfo is TcxFocusedRowChangedInfo then
        with TcxFocusedRowChangedInfo(AInfo) do
          FocusedRowChanged(PrevFocusedRowIndex, FocusedRowIndex)
      else
        if AInfo is TcxSelectionChangedInfo then
          SelectionChanged(TcxSelectionChangedInfo(AInfo));
end;

procedure TcxCustomLookupGrid.CreateScrollTimer;
begin
  if FScrollTimer <> nil then Exit;
  FScrollTimer := cxCreateTimer(ScrollTimerHandler, ScrollTimerInterval);
end;

procedure TcxCustomLookupGrid.DestroyScrollTimer;
begin
  FreeAndNil(FScrollTimer);
end;

function TcxCustomLookupGrid.GetDataController: TcxCustomDataController;
begin
  Result := TcxCustomDataController(FDataController);
end;

function TcxCustomLookupGrid.GetFocusedColumn: TcxLookupGridColumn;
begin
//  if (FFocusedColumn = nil) and (Columns.Count > 0) then
//    FFocusedColumn := Columns[0];
  Result := FFocusedColumn;
end;

function TcxCustomLookupGrid.GetFocusedColumnIndex: Integer;
begin
  if FocusedColumn <> nil then
    Result := FocusedColumn.Index
  else
    Result := -1;
end;

function TcxCustomLookupGrid.GetFocusedRowIndex: Integer;
begin
  Result := FDataController.GetFocusedRowIndex;
end;

function TcxCustomLookupGrid.GetRowCount: Integer;
begin
  Result := FDataController.GetRowCount;
end;

function TcxCustomLookupGrid.GetScrollContentForegroundColor: TColor;
begin
  Result := Font.Color;
end;

procedure TcxCustomLookupGrid.SetColumns(Value: TcxLookupGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TcxCustomLookupGrid.SetDataController(Value: TcxCustomDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxCustomLookupGrid.SetFocusedColumn(Value: TcxLookupGridColumn);
begin
  if Options.RowSelect then Value := nil;
  if FocusedColumn <> Value then
  begin
    FFocusedColumn := Value;
    Change([lgcLayout]);
  end;
end;

procedure TcxCustomLookupGrid.SetFocusedColumnIndex(Value: Integer);
begin
  if Columns.Count = 0 then Exit;
  if Value >= Columns.Count then
    Value := Columns.Count - 1;
  if Value < 0 then
    Value := 0;
  if FocusedColumnIndex <> Value then
    FocusedColumn := Columns[Value];
end;

procedure TcxCustomLookupGrid.SetFocusedRowIndex(Value: Integer);
begin
  FDataController.ChangeFocusedRowIndex(Value);
end;

procedure TcxCustomLookupGrid.SetIsPopupControl(Value: Boolean);
begin
  if FIsPopupControl <> Value then
  begin
    FIsPopupControl := Value;
    Change([lgcLayout]);
  end;
end;

procedure TcxCustomLookupGrid.SetOptions(Value: TcxLookupGridOptions);
begin
  FOptions.Assign(Value);
end;

procedure TcxCustomLookupGrid.SetTopRowIndex(Value: Integer);
begin
  CheckSetTopRowIndex(Value);
  if TopRowIndex <> Value then
  begin
    CheckTopRowIndex(Value, False);
  end;
end;

procedure TcxCustomLookupGrid.ScrollTimerHandler(Sender: TObject);

  procedure ChangeFocusedRow(Value: Integer);
  begin
    if Value >= RowCount then
      Value := RowCount - 1;
    if Value < 0 then Value := 0;
    FocusedRowIndex := Value;
    MakeFocusedRowVisible;
    SyncSelected(False);
  end;

var
  P: TPoint;
begin
  GetCursorPos(P);
  P := ScreenToClient(P);
  if P.Y < ViewInfo.VisibleRowsRect.Top then
    ChangeFocusedRow(TopRowIndex - 1)
  else
    if P.Y > ViewInfo.VisibleRowsRect.Bottom then
      ChangeFocusedRow(TopRowIndex + ViewInfo.VisibleRowCount + 1);
end;

end.

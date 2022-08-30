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

unit cxGridCardView;

{$I cxVer.inc}

interface

uses
  Types, Windows, Variants, Classes, Graphics, Controls, Forms, StdCtrls,
  dxCore, cxClasses, cxGraphics, cxControls, cxStyles, cxLookAndFeelPainters, cxGeometry,
  cxGridCommon, cxGrid, cxGridCustomView, cxGridCustomTableView, dxFilterPopupWindow,
  cxDataStorage, cxCustomData, cxGridCustomLayoutView, dxCoreClasses;

const
  cxGridCardViewAlternateCaptionSeparator: string = '-';

  cxGridDefaultCardBorderWidth = 3;
  cxGridCardDefaultCategoryIndent = 17;
  cxGridCardDefaultCategorySeparatorWidth = 2;
  cxGridCardDefaultLayerSeparatorWidth = 0;
  cxGridCardDefaultWidth = 200;
  cxGridCardBorderMinWidth = 1;
  cxGridCardMinWidth = 40;

  htCardViewBase = htCustomGridTableBase + 50;
  htCardRowIndent = htCardViewBase + 1;
  htCardRowExpandButton = htCardViewBase + 2;
  htRowCaption = htCardViewBase + 3;
  htRowFilterButton = htCardViewBase + 4;
  htCardScrollButtonUp = htCardViewBase + 5;
  htCardScrollButtonDown = htCardViewBase + 6;
  htSeparator = htCardViewBase + 7;

  ckRows = 2;

  bbCardViewFirst = bbCustomTableLast + 1;
  bbCaptionRow = bbCardViewFirst;
  bbCardBorder = bbCardViewFirst + 1;
  bbRowCaption = bbCardViewFirst + 2;
  bbCardViewLast = bbRowCaption;

  isRowFirst = isCustomItemLast + 1;
  isCaptionRow = isRowFirst;
  isCategoryRow = isRowFirst + 1;
  isRowCaption = isRowFirst + 2;
  isRowLast = isRowCaption;

  vsCardViewFirst = vsCustomTableLast + 1;
  vsCaptionRow = vsCardViewFirst;
  vsCardBorder = vsCardViewFirst + 1;
  vsCategoryRow = vsCardViewFirst + 2;
  vsCategorySeparator = vsCardViewFirst + 3;
  vsLayerSeparator = vsCardViewFirst + 4;
  vsRowCaption = vsCardViewFirst + 5;
  vsCardViewLast = vsRowCaption;

type
  TcxGridCardViewController = class;
  TcxGridCard = class;
  TcxGridCardViewViewData = class;
  TcxGridCardRowFilterButtonViewInfo = class;
  TcxGridCardRowCaptionViewInfo = class;
  TcxGridCardRowViewInfo = class;
  TcxCustomGridCardScrollButtonViewInfo = class;
  TcxGridCardExpandButtonViewInfo = class;
  TcxGridCardRowLayer = class;
  TcxGridCardRowLayoutClass = class of TcxGridCardRowLayout;
  TcxGridCardRowLayout = class;
  TcxGridCardRowHorizontalLayout = class;
  TcxGridCardViewInfoClass = class of TcxGridCardViewInfo;
  TcxGridCardViewInfo = class;
  TcxGridCardViewColumns = class;
  TcxGridCardsViewInfo = class;
  TcxGridCardViewSeparatorsViewInfo = class;
  TcxGridCardViewViewInfo = class;
  TcxGridCardViewInfoCacheItem = class;
  TcxGridCardViewRow = class;
  TcxGridCardVisibleRowLayoutObject = class;
  TcxGridCardViewRowLayoutObject = class;
  TcxGridCardViewVisibleRowLayoutObject = class;
  TcxGridCardViewRowLayoutController = class;
  TcxGridCardView = class;

  TcxGridCardExpandButtonAlignment = (cebaLeft, cebaRight);
  TcxGridCardViewLayoutDirection = (ldHorizontal, ldVertical);

  { hit tests }

  TcxGridCardRowFilterButtonHitTest = class(TcxGridRecordCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridCardRowIndentHitTest = class(TcxGridRecordCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridCardRowExpandButtonHitTest = class(TcxGridRecordCellHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  TcxGridCardRowCellHitTest = class(TcxGridRecordCellHitTest)
  public
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridCardRowCaptionHitTest = class(TcxGridCardRowCellHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    RowContainerKind: TcxGridItemContainerKind;
  end;

  TcxGridCardScrollButtonDownHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridCardScrollButtonUpHitTest = class(TcxGridRecordHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    class function CanClick: Boolean; override;
  end;

  TcxGridCardViewSeparatorHitTest = class(TcxCustomGridHitTest)
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    Index: Integer;
    Separators: TcxGridCardViewSeparatorsViewInfo;
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  { view data }

  TcxGridCard = class(TcxGridCustomLayoutRecord)
  private
    function GetGridView: TcxGridCardView;
  protected
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
  public
    procedure GetVisibleRows(ARows: TList);
    property GridView: TcxGridCardView read GetGridView;
  end;

  TcxGridCardViewViewData = class(TcxGridCustomLayoutViewViewData)
  private
    function GetCard(Index: Integer): TcxGridCard;
    function GetCardCount: Integer;
  protected
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
  public
    property CardCount: Integer read GetCardCount;
    property Cards[Index: Integer]: TcxGridCard read GetCard;
  end;

  { controller }

  // drag & drop objects

  TcxGridCardRowInsertionPos = (ripPrevLayer, ripNewLayer, ripSameLayer);

  TcxGridCardRowContainerZone = class(TcxGridItemContainerZone)
  private
    function GetItem: TcxGridCardViewRow;
    function GetRecordViewInfo: TcxGridCardViewInfo;
  public
    GridRecord: TcxCustomGridRecord;
    InsertionPos: TcxGridCardRowInsertionPos;
    constructor Create(AGridRecord: TcxCustomGridRecord; AItemIndex: Integer;
      AInsertionPos: TcxGridCardRowInsertionPos); reintroduce;
    function IsEqual(Value: TcxGridItemContainerZone): Boolean; override;
    property RecordViewInfo: TcxGridCardViewInfo read GetRecordViewInfo;
    property Item: TcxGridCardViewRow read GetItem;
  end;

  TcxGridCardRowMovingObjectClass = class of TcxGridCardRowMovingObject;

  TcxGridCardRowMovingObject = class(TcxCustomGridTableItemMovingObject)
  private
    FExpandingRow: TcxGridCardViewRow;
    FRowExpandingTimer: TcxTimer;
    FSourceGridRecord: TcxCustomGridRecord;
    function GetDestZone: TcxGridCardRowContainerZone;
    function GetGridView: TcxGridCardView;
    function GetRowLayout: TcxGridCardViewRowLayoutObject;
    function GetSourceItem: TcxGridCardViewRow;
    function GetViewInfo: TcxGridCardViewViewInfo;
    function GetVisibleRowLayout: TcxGridCardViewVisibleRowLayoutObject;
    procedure SetDestZone(Value: TcxGridCardRowContainerZone);
    procedure SetSourceItem(Value: TcxGridCardViewRow);
    procedure RowExpandingHandler(Sender: TObject);
  protected
    function AreArrowsVertical: Boolean; override;
    procedure CalculateDestParams(AHitTest: TcxCustomGridHitTest;
      out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone); override;
    function CanRemove: Boolean; override;
    procedure ChangeSourceItemPosition; virtual;
    function GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect; override;
    function GetArrowsClientRect: TRect; override;
    function GetSourceItemViewInfo: TcxCustomGridCellViewInfo; override;
    function IsValidDestination: Boolean; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    procedure StartRowExpanding(ARow: TcxGridCardViewRow);
    procedure StopRowExpanding;
    property ExpandingRow: TcxGridCardViewRow read FExpandingRow;

    property DestZone: TcxGridCardRowContainerZone read GetDestZone write SetDestZone;
    property GridView: TcxGridCardView read GetGridView;
    property RowLayout: TcxGridCardViewRowLayoutObject read GetRowLayout;
    property SourceGridRecord: TcxCustomGridRecord read FSourceGridRecord write FSourceGridRecord;
    property SourceItem: TcxGridCardViewRow read GetSourceItem write SetSourceItem;
    property ViewInfo: TcxGridCardViewViewInfo read GetViewInfo;
    property VisibleRowLayout: TcxGridCardViewVisibleRowLayoutObject read GetVisibleRowLayout;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  TcxGridCardSizingObject = class(TcxCustomGridDragAndDropObject)
  private
    FCardColumnIndex: Integer;
    FCardOriginalWidth: Integer;
    FDestPointX: Integer;
    FSeparators: TList;
    function GetGridView: TcxGridCardView;
    function GetSeparator(Index: Integer): TRect;
    function GetSeparatorCount: Integer;
    function GetViewInfo: TcxGridCardViewViewInfo;
    procedure SetDestPointX(Value: Integer);
  protected
    procedure DirtyChanged; override;
    function GetCurrentWidth: Integer; virtual;
    function GetDeltaWidth: Integer; virtual;
    function GetDragAndDropCursor(Accepted: Boolean): TCursor; override;
    function GetImmediateStart: Boolean; override;
    function GetOriginalWidth: Integer; virtual;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    procedure AddSeparator(const R: TRect);
    procedure CalculateSeparators;
    procedure ClearSeparators;
    procedure DrawSeparators;
    property SeparatorCount: Integer read GetSeparatorCount;
    property Separators[Index: Integer]: TRect read GetSeparator;

    property CardColumnIndex: Integer read FCardColumnIndex;
    property CardOriginalWidth: Integer read FCardOriginalWidth
      write FCardOriginalWidth;
    property CurrentWidth: Integer read GetCurrentWidth;
    property DeltaWidth: Integer read GetDeltaWidth;
    property DestPointX: Integer read FDestPointX write SetDestPointX;
    property GridView: TcxGridCardView read GetGridView;
    property OriginalWidth: Integer read GetOriginalWidth;
    property ViewInfo: TcxGridCardViewViewInfo read GetViewInfo;
  public
    constructor Create(AControl: TcxControl); override;
    destructor Destroy; override;
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  // customization form

  TcxGridCardRowsListBox = class(TcxCustomGridTableItemsListBox)
  private
    function GetGridView: TcxGridCardView;
  protected
    function CalculateItemHeight: Integer; override;
    procedure DoRefreshItems; override;
    function GetDragAndDropParams: TcxCustomGridHitTest; override;
    property GridView: TcxGridCardView read GetGridView;
  public
    procedure PaintItem(ACanvas: TcxCanvas; R: TRect; AIndex: Integer; AFocused: Boolean); override;
  end;

  TcxGridCardViewCustomizationForm = class(TcxCustomGridTableCustomizationForm)
  protected
    function GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass; override;
    function GetItemsPageCaption: string; override;
  end;

  // controllers

  TcxGridCardViewController = class(TcxGridCustomLayoutViewController)
  private
    function GetCustomizationForm: TcxGridCardViewCustomizationForm;
    function GetFocusedCard: TcxGridCard;
    function GetFocusedCardViewInfo: TcxGridCardViewInfo;
    function GetFocusedRow: TcxGridCardViewRow;
    function GetGridView: TcxGridCardView;
    function GetScrollCardViewInfo: TcxGridCardViewInfo;
    function GetTopCardIndex: Integer;
    function GetViewData: TcxGridCardViewViewData;
    function GetViewInfo: TcxGridCardViewViewInfo;
    procedure SetFocusedCard(Value: TcxGridCard);
    procedure SetFocusedRow(Value: TcxGridCardViewRow);
    procedure SetScrollCardViewInfo(Value: TcxGridCardViewInfo);
    procedure SetTopCardIndex(Value: Integer);
  protected
    function GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass; override;

    function CanFocusNextItem(AFocusedItemIndex, ANextItemIndex: Integer;
      AGoForward, AGoOnCycle, AGoToNextRecordOnCycle: Boolean): Boolean; override;
    function FindNextRow(ACard: TcxGridCard; ARows: TList; ARow: TcxGridCardViewRow;
      AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function IsKeyForMultiSelect(AKey: Word; AShift: TShiftState;
      AFocusedRecordChanged: Boolean): Boolean; override;
    procedure RowExpandedChanged(ARow: TcxGridCardViewRow); virtual;
    procedure RowExpandedChanging(ARow: TcxGridCardViewRow; AValue: Boolean); virtual;
    procedure ScrollData(ADirection: TcxDirection); override;

    // internal drag and drop data scrolling
    function CanScrollData(ADirection: TcxDirection): Boolean; override;
    function GetScrollDataTimeInterval(ADirection: TcxDirection): Integer; override;

    // customization
    function GetCustomizationFormClass: TcxCustomGridCustomizationFormClass; override;
    function GetRowDragAndDropObjectClass: TcxGridCardRowMovingObjectClass; virtual;

    property FocusedCardViewInfo: TcxGridCardViewInfo read GetFocusedCardViewInfo;
    property ScrollCardViewInfo: TcxGridCardViewInfo read GetScrollCardViewInfo write SetScrollCardViewInfo;
    property ViewData: TcxGridCardViewViewData read GetViewData;
    property ViewInfo: TcxGridCardViewViewInfo read GetViewInfo;
  public
    procedure CheckScrolling(const P: TPoint); override;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); override;

    function CanScrollFocusedCard(ADown: Boolean): Boolean; virtual;
    function ScrollFocusedCard(ADown: Boolean): Boolean; virtual;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    function FocusNextItemHorizontally(AGoForward, AGoOnCycle: Boolean): Boolean; override;
    function FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean; override;

    property CustomizationForm: TcxGridCardViewCustomizationForm read GetCustomizationForm;
    property FocusedCard: TcxGridCard read GetFocusedCard write SetFocusedCard;
    property FocusedItem: TcxGridCardViewRow read GetFocusedRow write SetFocusedRow;
    property FocusedRow: TcxGridCardViewRow read GetFocusedRow write SetFocusedRow;
    property GridView: TcxGridCardView read GetGridView;
    property TopCardIndex: Integer read GetTopCardIndex write SetTopCardIndex;
  end;

  // row layout controller

  TcxGridCardViewRowLayout = (rlHorizontal, rlVertical);
  TcxGridCardViewRowLayerPosition = (rlpBeforeRow, rlpAfterRow, rlpBeforeLayer, rlpAfterLayer);

  TcxGridCardRowPosition = record
    LayerIndex: Integer;
    IndexInLayer: Integer;
  end;

  TcxGridCardRowCoordinates = record
    ColIndex: Integer;
    RowIndex: Integer;
  end;

  TcxGridCardRowCoordinatesArray  = array of TcxGridCardRowCoordinates;

  TcxCustomGridCardRowLayoutObject = class
  private
    FLayerRowCounts: TList;
    function GetLayerCount: Integer;
    function GetLayerFirstRow(ALayerIndex: Integer): TcxGridCardViewRow;
    function GetLayerFirstRowIndex(ALayerIndex: Integer): Integer;
    function GetLayerRow(ALayerIndex, AIndex: Integer): TcxGridCardViewRow;
    function GetLayerRowCount(ALayerIndex: Integer): Integer;
    procedure SetLayerRowCount(ALayerIndex, Value: Integer);
  protected
    function GetLayout: TcxGridCardViewRowLayout; virtual; abstract;
    function GetRow(AIndex: Integer): TcxGridCardViewRow; virtual; abstract;
    function GetRowCount: Integer; virtual; abstract;
    function GetRowIndex(ARow: TcxGridCardViewRow): Integer; virtual; abstract;

    function GetCoordinates(const APosition: TcxGridCardRowPosition): TcxGridCardRowCoordinates; overload;
    function GetPosition(const ACoordinates: TcxGridCardRowCoordinates): TcxGridCardRowPosition; overload;

    function GetLayerIndex(ARowIndex: Integer): Integer; overload;
    function GetLayerPosition(APosition: TcxPosition): TcxGridCardViewRowLayerPosition;
    function GetLength(ARow: TcxGridCardViewRow): Integer;
    function GetOffsetInLayer(ALayerIndex, AIndexInLayer: Integer): Integer; overload;
    function GetPosition(ARowIndex: Integer): TcxGridCardRowPosition; overload;

    property LayerRowCounts: TList read FLayerRowCounts;
    property RowCount: Integer read GetRowCount;
    property Rows[AIndex: Integer]: TcxGridCardViewRow read GetRow;
  public
    constructor Create;
    destructor Destroy; override;

    function GetCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates; overload;
    function GetIndexInLayer(ARow: TcxGridCardViewRow): Integer;
    function GetLayerIndex(ARow: TcxGridCardViewRow): Integer; overload;
    function GetOffsetInLayer(ARow: TcxGridCardViewRow): Integer; overload;
    function GetPosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition; overload;
    function GetRowAtOffset(ALayerIndex, AOffset: Integer): TcxGridCardViewRow;
    function IsFirstInLayer(ARow: TcxGridCardViewRow): Boolean;
    function IsLastInLayer(ARow: TcxGridCardViewRow): Boolean;
    function IsWholeLine(ARow: TcxGridCardViewRow): Boolean;

    property LayerCount: Integer read GetLayerCount;
    property LayerFirstRow[ALayerIndex: Integer]: TcxGridCardViewRow read GetLayerFirstRow;
    property LayerFirstRowIndex[ALayerIndex: Integer]: Integer read GetLayerFirstRowIndex;
    property LayerRowCount[ALayerIndex: Integer]: Integer read GetLayerRowCount write SetLayerRowCount;
    property LayerRows[ALayerIndex, AIndex: Integer]: TcxGridCardViewRow read GetLayerRow;
    property Layout: TcxGridCardViewRowLayout read GetLayout;
  end;

  TcxGridCardRowLayoutObject = class(TcxCustomGridCardRowLayoutObject)
  private
    FLayout: TcxGridCardViewRowLayout;
    FRows: TList;
  protected
    function GetLayout: TcxGridCardViewRowLayout; override;
    function GetRow(AIndex: Integer): TcxGridCardViewRow; override;
    function GetRowCount: Integer; override;
    function GetRowIndex(ARow: TcxGridCardViewRow): Integer; override;
    property RowsList: TList read FRows;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Fill(ARows: TList);
    property Layout: TcxGridCardViewRowLayout read GetLayout write FLayout;
  end;

  TcxGridCardVisibleRowLayoutObject = class(TcxGridCardRowLayoutObject)
  private
    FCard: TcxGridCard;
  public
    constructor Create(ACard: TcxGridCard);
    procedure GetInsertionParams(ARow: TcxGridCardViewRow; APosition: TcxPosition;
      out AInsertionIndex: Integer; out AInsertionPos: TcxGridCardRowInsertionPos);
    procedure GetLayerRows(ARowInLayer: TcxGridCardViewRow; ARows: TList);
    procedure GetRowsAtOffset(ARowAtOffset: TcxGridCardViewRow; ARows: TList);
    property Card: TcxGridCard read FCard;
  end;

  TcxGridCardViewRowLayoutObject = class(TcxCustomGridCardRowLayoutObject)
  private
    FGridView: TcxGridCardView;
    FOnLayerIndexChanged: TNotifyEvent;
    function GetBeginsLayer(ARow: TcxGridCardViewRow): Boolean;
    procedure SetBeginsLayer(ARow: TcxGridCardViewRow; Value: Boolean);
  protected
    function GetLayout: TcxGridCardViewRowLayout; override;
    function GetRow(AIndex: Integer): TcxGridCardViewRow; override;
    function GetRowCount: Integer; override;
    function GetRowIndex(ARow: TcxGridCardViewRow): Integer; override;

    procedure AddLayer;
    procedure InsertLayer(AIndex: Integer);
    procedure RemoveLayer(AIndex: Integer);

    procedure AddRowToLayer(ALayerIndex: Integer);
    procedure RemoveRowFromLayer(ALayerIndex: Integer);

    procedure AddRow(ARow: TcxGridCardViewRow);
    procedure RemoveRow(ARowIndex: Integer);
    procedure MoveRow(ARow: TcxGridCardViewRow; AOldIndex: Integer);
    procedure MoveRows(ARows: TList; AIndex: Integer); overload;
    procedure MoveRows(ARows: TList; AIndex: Integer; ARowLayout: TcxGridCardRowLayoutObject); overload;

    procedure LayerIndexChanged;
    property OnLayerIndexChanged: TNotifyEvent read FOnLayerIndexChanged write FOnLayerIndexChanged;
  public
    constructor Create(AGridView: TcxGridCardView);

    procedure BreakLayer(ALayerIndex, AIndexInLayer: Integer);
    function IsSimpleLayout: Boolean;
    procedure MergeLayer(ALayerIndex: Integer);

    procedure SetCoordinates(ARow: TcxGridCardViewRow; const ACoordinates: TcxGridCardRowCoordinates);
    procedure SetIndexInLayer(ARow: TcxGridCardViewRow; AValue: Integer;
      AMoveSubItems: Boolean = False);
    procedure SetLayerIndex(ARow: TcxGridCardViewRow; AValue: Integer;
      ANewLayer: Boolean = False; AMoveSubItems: Boolean = False);
    procedure SetPosition(ARow: TcxGridCardViewRow; const APosition: TcxGridCardRowPosition);

    property BeginsLayer[ARow: TcxGridCardViewRow]: Boolean read GetBeginsLayer write SetBeginsLayer;
    property GridView: TcxGridCardView read FGridView;
  end;

  TcxGridCardViewVisibleRowLayoutObject = class(TcxGridCardRowLayoutObject)
  private
    FGridView: TcxGridCardView;
  protected
    function GetLayout: TcxGridCardViewRowLayout; override;
  public
    constructor Create(AGridView: TcxGridCardView);
    function GetLastVisibleSubItem(ARow: TcxGridCardViewRow): TcxGridCardViewRow;
    function GetLayerCount(ARow: TcxGridCardViewRow): Integer;
    function IsWholeLayer(ARow: TcxGridCardViewRow): Boolean;
    property GridView: TcxGridCardView read FGridView;
  end;

  TcxGridCardViewRowLayoutControllerClass = class of TcxGridCardViewRowLayoutController;

  TcxGridCardViewRowLayoutController = class
  private
    FGridView: TcxGridCardView;
    FLayoutObject: TcxGridCardViewRowLayoutObject;
    FVisibleLayoutObject: TcxGridCardViewVisibleRowLayoutObject;
    function GetBeginsLayer(ARow: TcxGridCardViewRow): Boolean;
    function GetLayout: TcxGridCardViewRowLayout;
    procedure SetBeginsLayer(ARow: TcxGridCardViewRow; Value: Boolean);
    procedure LayerIndexChanged(Sender: TObject);
  protected
    procedure RefreshVisibleLayoutObject;
    procedure RowIndexChanged(ARow: TcxGridCardViewRow; AOldIndex: Integer);
    procedure VisibleRowsListChanged;

    function CreateCardRowLayout(ACardViewInfo: TcxGridCardViewInfo): TcxGridCardRowLayout;
    function GetCardRowLayoutClass: TcxGridCardRowLayoutClass; virtual;
  public
    constructor Create(AGridView: TcxGridCardView); virtual;
    destructor Destroy; override;
    function IsHorizontalLayout: Boolean;
    function IsSimpleLayout: Boolean;
    function IsWholeLine(ARow: TcxGridCardViewRow): Boolean;

    // indexes handling

    function GetCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates;
    procedure SetCoordinates(ARow: TcxGridCardViewRow; AColIndex, ARowIndex: Integer); overload;
    procedure SetCoordinates(ARow: TcxGridCardViewRow; ACoordinates: TcxGridCardRowCoordinates); overload;
    function GetVisibleCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates;

    function GetPosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition;
    procedure SetPosition(ARow: TcxGridCardViewRow; ALayerIndex, AIndexInLayer: Integer);
    function GetVisiblePosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition;

    // navigation

    function FindNextRow(ACard: TcxGridCard; ARow: TcxGridCardViewRow;
      ASameLayer, AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;

    function FindNextRowHorizontally(ACard: TcxGridCard; ARow: TcxGridCardViewRow;
      AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;
    function FindNextRowVertically(ACard: TcxGridCard; ARow: TcxGridCardViewRow;
      AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;

    property BeginsLayer[ARow: TcxGridCardViewRow]: Boolean read GetBeginsLayer write SetBeginsLayer;
    property GridView: TcxGridCardView read FGridView;
    property Layout: TcxGridCardViewRowLayout read GetLayout;
    property LayoutObject: TcxGridCardViewRowLayoutObject read FLayoutObject;
    property VisibleLayoutObject: TcxGridCardViewVisibleRowLayoutObject read FVisibleLayoutObject;
  end;

  { painters }

  TcxGridCardRowFilterButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridCardRowFilterButtonViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxGridCardRowFilterButtonViewInfo read GetViewInfo;
  end;

  TcxGridCardRowCaptionPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridCardRowCaptionViewInfo;
  protected
    procedure DrawContent; override;
    property ViewInfo: TcxGridCardRowCaptionViewInfo read GetViewInfo;
  end;

  TcxGridCardRowPainterClass = class of TcxGridCardRowPainter;

  TcxGridCardRowPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridCardRowViewInfo;
  protected
    procedure DrawExpandButton; virtual;
    procedure DrawFocusRect; virtual;
    procedure DrawIndent; virtual;
    procedure Paint; override;
    property ViewInfo: TcxGridCardRowViewInfo read GetViewInfo;
  end;

  TcxGridCardScrollButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxCustomGridCardScrollButtonViewInfo;
  protected
    procedure DrawContent; override;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxCustomGridCardScrollButtonViewInfo read GetViewInfo;
  end;

  TcxGridCardExpandButtonPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridCardExpandButtonViewInfo;
  protected
    procedure Paint; override;
    property ViewInfo: TcxGridCardExpandButtonViewInfo read GetViewInfo;
  end;

  TcxGridCardPainter = class(TcxCustomGridRecordPainter)
  private
    FClipRegion: TcxRegion;
    function GetViewInfo: TcxGridCardViewInfo;
  protected
    procedure AfterPaint; override;
    procedure BeforePaint; override;
    procedure DrawBackground; override;
    procedure DrawCardBorder; virtual;
    procedure DrawExpandButton; override;
    function DrawExpandButtonBeforePaint: Boolean; override;
    procedure DrawLayerSeparators; virtual;
    procedure DrawRows; virtual;
    procedure DrawScrollButtons; virtual;
    procedure Paint; override;
    property ViewInfo: TcxGridCardViewInfo read GetViewInfo;
  end;

  TcxGridCardViewPainter = class(TcxGridCustomLayoutViewPainter);

  { view infos }

  // column and columns

  TcxGridCardViewColumn = class(TcxGridCustomLayoutViewBand)
  private
    function GetLastRow: TcxGridCardViewInfo;
    function GetRow(Index: Integer): TcxGridCardViewInfo;
    procedure SetRow(Index: Integer; Value: TcxGridCardViewInfo);
  public
    function GetNearestRow(APos: Integer): TcxGridCardViewInfo; reintroduce;

    property LastRow: TcxGridCardViewInfo read GetLastRow;
    property Rows[Index: Integer]: TcxGridCardViewInfo read GetRow write SetRow; default;
  end;

  TcxGridCardViewColumns = class(TcxGridCustomLayoutViewBands)
  private
    function GetCardsViewInfo: TcxGridCardsViewInfo;
    function GetItem(Index: Integer): TcxGridCardViewColumn;
    function GetLast: TcxGridCardViewColumn;
  protected
    function GetBandClass: TcxGridCustomLayoutViewBandClass; override;
    property CardsViewInfo: TcxGridCardsViewInfo read GetCardsViewInfo;
  public
    property Items[Index: Integer]: TcxGridCardViewColumn read GetItem; default;
    property Last: TcxGridCardViewColumn read GetLast;
  end;

  // card row

  TcxGridCardRowCellViewInfo = class(TcxGridTableDataCellViewInfo)
  private
    FRowViewInfo: TcxGridCardRowViewInfo;
    function GetCardViewInfo: TcxGridCardViewInfo;
    function GetGridView: TcxGridCardView;
    function GetGridRecord: TcxGridCard;
    function GetRow: TcxGridCardViewRow;
  protected
    function GetAreaBounds: TRect; override;
    function GetMultiLine: Boolean; override;
    function GetMultiLinePainting: Boolean; override;
    function GetTransparent: Boolean; override;
    function HasFocusRect: Boolean; override;
  public
    constructor Create(ARowViewInfo: TcxGridCardRowViewInfo); reintroduce; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property CardViewInfo: TcxGridCardViewInfo read GetCardViewInfo;
    property GridRecord: TcxGridCard read GetGridRecord;
    property GridView: TcxGridCardView read GetGridView;
    property Row: TcxGridCardViewRow read GetRow;
    property RowViewInfo: TcxGridCardRowViewInfo read FRowViewInfo;
  end;

  TcxGridCardRowFilterButtonViewInfoClass = class of TcxGridCardRowFilterButtonViewInfo;

  TcxGridCardRowFilterButtonViewInfo = class(TcxGridCustomLayoutViewItemFilterButtonViewInfo)
  private
    FRowCaptionViewInfo: TcxGridCardRowCaptionViewInfo;
    function GetRow: TcxGridCardViewRow;
  protected
    function GetItem: TcxCustomGridTableItem; override;

    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    function GetDropDownWindowOwnerBounds: TRect; override;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; override;
  public
    constructor Create(ARowCaptionViewInfo: TcxGridCardRowCaptionViewInfo); reintroduce; virtual;
    property Row: TcxGridCardViewRow read GetRow;
    property RowCaptionViewInfo: TcxGridCardRowCaptionViewInfo read FRowCaptionViewInfo;
  end;

  TcxGridCardRowCaptionViewInfoClass = class of TcxGridCardRowCaptionViewInfo;

  TcxGridCardRowCaptionViewInfo = class(TcxGridCardRowCellViewInfo)
  private
    FCalculatingRealWidth: Boolean;
    FFilterButtonViewInfo: TcxGridCardRowFilterButtonViewInfo;
    function GetRealWidth: Integer;
  protected
    function CalculateHeight: Integer; override;
    class function CalculateSimpleHeight(ARow: TcxGridCardViewRow;
      ACanvas: TcxCanvas; AFont: TFont): Integer; virtual;
    function CalculateRealWidth: Integer; virtual;
    function CalculateWidth: Integer; override;
    function CanFilter: Boolean; virtual;
    function CanShowAutoHint: Boolean; override;
    function CanShowCustomHint: Boolean; override;
    function CanShowEdit: Boolean; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetAutoHeight: Boolean; override;
    function GetFilterButtonBounds: TRect; virtual;
    function GetFilterButtonViewInfoClass: TcxGridCardRowFilterButtonViewInfoClass; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetShowEndEllipsis: Boolean; override;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    function UseStandardNeedShowHint: Boolean; override;

    property CalculatingRealWidth: Boolean read FCalculatingRealWidth;
    property FilterButtonBounds: TRect read GetFilterButtonBounds;
  public
    constructor Create(ARowViewInfo: TcxGridCardRowViewInfo); override;
    destructor Destroy; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;

    property FilterButtonViewInfo: TcxGridCardRowFilterButtonViewInfo read FFilterButtonViewInfo;
    property RealWidth: Integer read GetRealWidth;
  end;

  TcxGridCardRowDataViewInfoClass = class of TcxGridCardRowDataViewInfo;

  TcxGridCardRowDataViewInfo = class(TcxGridCardRowCellViewInfo)
  protected
    function CalculateHeight: Integer; override;
    class function CalculateSimpleHeight(ARow: TcxGridCardViewRow;
      ACanvas: TcxCanvas; AFont: TFont): Integer; virtual;
    function CalculateWidth: Integer; override;
    function GetAutoHeight: Boolean; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetVisible: Boolean; override;
  end;

  TcxGridCardRowViewInfoClass = class of TcxGridCardRowViewInfo;

  TcxGridCardRowViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCaptionViewInfo: TcxGridCardRowCaptionViewInfo;
    FCardViewInfo: TcxGridCardViewInfo;
    FDataViewInfo: TcxGridCardRowDataViewInfo;
    FHeight: Integer;
    FIndex: Integer;
    FPartVisible: Boolean;
    function GetExpandButtonAreaSizeValue: Integer;
    function GetExpandButtonSizeValue: Integer;
    function GetExpanded: Boolean;
    function GetGridView: TcxGridCardView;
    function GetGridRecord: TcxGridCard;
    function GetGridViewInfo: TcxGridCardViewViewInfo;
    //function GetMaxHeight: Integer;
    function GetRow: TcxGridCardViewRow;
    procedure SetExpanded(Value: Boolean);
    function CalculatePartVisible: Boolean;
    function CalculateVisible: Boolean;
    procedure CreateViewInfos;
    procedure DestroyViewInfos;
  protected
    function CalculateContentBounds: TRect; override;
    function CalculateHeaderWidth: Integer; virtual;
    function CalculateHeight: Integer; override;
    class function CalculateSimpleHeight(ARow: TcxGridCardViewRow; ACanvas: TcxCanvas): Integer; virtual;
    function CalculateWidth: Integer; override;
    class procedure CheckHeight(ACardViewInfoClass: TcxGridCardViewInfoClass; ARow: TcxGridCardViewRow;
      ARowViewInfo: TcxGridCardRowViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter;
      AScaleFactor: TdxScaleFactor; var AHeight: Integer); virtual;
    class function GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass; virtual;
    function GetCaptionWidth: Integer; virtual;
    class function GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass; virtual;
    function GetDataWidth: Integer; virtual;
    function GetDesignSelectionBounds: TRect; override;
    function GetExpandButtonAreaBounds: TRect; virtual;
    class function GetExpandButtonAreaSize(APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function GetExpandButtonBounds: TRect; virtual;
    class function GetExpandButtonSize(APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function GetHeaderWidth: Integer; virtual;
    function GetHeight: Integer; override;
    function GetHidden: Boolean; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIndent: Integer; virtual;
    function GetIndentBounds: TRect; virtual;
    function GetIndentViewParams: TcxViewParams; virtual;
    function GetIsDesignSelected: Boolean; override;
    function GetIsVisibleForPainting: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRealIndentBounds: TRect; virtual;
    class function GetSimpleViewParams(ARow: TcxGridCardViewRow; AIsCaption: Boolean): TcxViewParams; virtual;
    function GetWidth: Integer; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;

    procedure GetCaptionViewParams(var AParams: TcxViewParams; AIgnoreSelection: Boolean); virtual; abstract;
    procedure GetDataViewParams(var AParams: TcxViewParams); virtual; abstract;

    property CaptionWidth: Integer read GetCaptionWidth;
    property DataWidth: Integer read GetDataWidth;
    property ExpandButtonAreaSize: Integer read GetExpandButtonAreaSizeValue;
    property ExpandButtonSize: Integer read GetExpandButtonSizeValue;
    property Indent: Integer read GetIndent;
    //!!!property MaxHeight: Integer read GetMaxHeight;
  public
    constructor Create(ACardViewInfo: TcxGridCardViewInfo; AIndex: Integer); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(const ABounds: TRect); override;
    function CalculateVisibleCounts(var AVisibleRowCount, APartVisibleRowCount: Integer): Boolean;
    procedure CalculateVisibles;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function HasExpandButton: Boolean; virtual;
    function HasIndent: Boolean; virtual;
    function HasLimitedHeaderSpace: Boolean; virtual;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;

    property CaptionViewInfo: TcxGridCardRowCaptionViewInfo read FCaptionViewInfo;
    property CardViewInfo: TcxGridCardViewInfo read FCardViewInfo;
    property DataViewInfo: TcxGridCardRowDataViewInfo read FDataViewInfo;
    property ExpandButtonAreaBounds: TRect read GetExpandButtonAreaBounds;
    property ExpandButtonBounds: TRect read GetExpandButtonBounds;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property GridRecord: TcxGridCard read GetGridRecord;
    property GridView: TcxGridCardView read GetGridView;
    property GridViewInfo: TcxGridCardViewViewInfo read GetGridViewInfo;
    property HeaderWidth: Integer read GetHeaderWidth;
    property Height: Integer read GetHeight write FHeight;
    property Hidden: Boolean read GetHidden;
    property IndentBounds: TRect read GetRealIndentBounds;
    property Index: Integer read FIndex;
    property PartVisible: Boolean read FPartVisible;
    property Row: TcxGridCardViewRow read GetRow;
  end;

  // data row

  TcxGridCardDataRowDataViewInfo = class(TcxGridCardRowDataViewInfo)
  protected
    function CalculateSelected: Boolean; override;
  end;

  TcxGridCardDataRowViewInfo = class(TcxGridCardRowViewInfo)
  protected
    procedure GetCaptionViewParams(var AParams: TcxViewParams; AIgnoreSelection: Boolean); override;
    class function GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass; override;
    procedure GetDataViewParams(var AParams: TcxViewParams); override;
    class function GetSimpleViewParams(ARow: TcxGridCardViewRow; AIsCaption: Boolean): TcxViewParams; override;
  end;

  // caption row

  TcxGridCardCaptionRowCaptionViewInfo = class(TcxGridCardRowCaptionViewInfo)
  protected
    function GetTextAreaBounds: TRect; override;
  end;

  TcxGridCardCaptionRowDataViewInfo = class(TcxGridCardRowDataViewInfo)
  protected
    procedure GetEditViewDataContentOffsets(var R: TRect); override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetTextAreaBounds: TRect; override;
  end;

  TcxGridCardCaptionRowViewInfo = class(TcxGridCardRowViewInfo)
  protected
    class function GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass; override;
    procedure GetCaptionViewParams(var AParams: TcxViewParams; AIgnoreSelection: Boolean); override;
    class function GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass; override;
    procedure GetDataViewParams(var AParams: TcxViewParams); override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
  end;

  // category row

  TcxGridCardCategoryRowViewInfo = class(TcxGridCardCaptionRowViewInfo)
  protected
    class procedure CheckHeight(ACardViewInfoClass: TcxGridCardViewInfoClass; ARow: TcxGridCardViewRow;
      ARowViewInfo: TcxGridCardRowViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter;
      AScaleFactor: TdxScaleFactor; var AHeight: Integer); override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    procedure GetDataViewParams(var AParams: TcxViewParams); override;
    function GetSeparatorColor: TColor; virtual;
    function HasSeparator: Boolean; overload;
    class function HasSeparator(ACardViewInfoClass: TcxGridCardViewInfoClass;
      ARow: TcxGridCardViewRow): Boolean; overload;
    property SeparatorColor: TColor read GetSeparatorColor;
  public
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
  end;

  // card scroll buttons

  TcxCustomGridCardScrollButtonViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCardViewInfo: TcxGridCardViewInfo;
    FScrollTimer: TcxTimer;
    procedure ScrollTimerHandler(Sender: TObject);
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    function GetHotTrack: Boolean; override;
    function GetIsDownButton: Boolean; virtual; abstract;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure Scroll; virtual; abstract;
    procedure StateChanged(APrevState: TcxGridCellState); override;

    procedure StartAutoScrolling;
    procedure StopAutoScrolling;
    property ScrollTimer: TcxTimer read FScrollTimer;
  public
    constructor Create(ACardViewInfo: TcxGridCardViewInfo); reintroduce; virtual;
    property IsDownButton: Boolean read GetIsDownButton;
    property CardViewInfo: TcxGridCardViewInfo read FCardViewInfo;
  end;

  TcxGridCardScrollButtonDownViewInfoClass = class of TcxGridCardScrollButtonDownViewInfo;

  TcxGridCardScrollButtonDownViewInfo = class(TcxCustomGridCardScrollButtonViewInfo)
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDownButton: Boolean; override;
    function GetVisible: Boolean; override;
    procedure Scroll; override;
  end;

  TcxGridCardScrollButtonUpViewInfoClass = class of TcxGridCardScrollButtonUpViewInfo;

  TcxGridCardScrollButtonUpViewInfo = class(TcxCustomGridCardScrollButtonViewInfo)
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetIsDownButton: Boolean; override;
    function GetVisible: Boolean; override;
    procedure Scroll; override;
  end;

  // card expand button

  TcxGridCardExpandButtonViewInfoClass = class of TcxGridCardExpandButtonViewInfo;

  TcxGridCardExpandButtonViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FCardViewInfo: TcxGridCardViewInfo;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CaptureMouseOnPress: Boolean; override;
    procedure Click; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
  public
    constructor Create(ACardViewInfo: TcxGridCardViewInfo); reintroduce; virtual;
    function HasPoint(const P: TPoint): Boolean; override;
    property CardViewInfo: TcxGridCardViewInfo read FCardViewInfo;
  end;

  // card row layout

  TcxGridCardRowLayerClass = class of TcxGridCardRowLayer;

  TcxGridCardRowLayer = class
  private
    FIndex: Integer;
    FLength: Integer;
    FOwner: TcxGridCardRowLayout;
    FThickness: Integer;
    function GetBounds: TRect;
    function GetLength: Integer;
    function GetRow(AIndex: Integer): TcxGridCardRowViewInfo;
    function GetRowCount: Integer;
    function GetThickness: Integer;
  protected
    procedure BeforeCalculation; virtual;
    function CalculateLength: Integer; virtual; abstract;
    function CalculateThickness: Integer; virtual; abstract;
    procedure SetThickness(Value: Integer); virtual;
  public
    constructor Create(AOwner: TcxGridCardRowLayout; AIndex: Integer); virtual;
    procedure Calculate(AOwnerWidth: Integer); virtual;
    procedure CalculateRows(const ABounds: TRect; ATopRowIndex: Integer;
      var AVisibleRowCount, APartVisibleRowCount: Integer); virtual; abstract;
    function HasPoint(const P: TPoint): Boolean; virtual; abstract;

    property Bounds: TRect read GetBounds;
    property Index: Integer read FIndex;
    property Length: Integer read GetLength;
    property Owner: TcxGridCardRowLayout read FOwner;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TcxGridCardRowViewInfo read GetRow;
    property Thickness: Integer read GetThickness write SetThickness;
  end;

  TcxGridCardRowLayout = class(TcxGridCardVisibleRowLayoutObject)
  private
    FCardViewInfo: TcxGridCardViewInfo;
    FLayers: array of TcxGridCardRowLayer;
    FSeparatorWidth: Integer;
    FSimple: Boolean;
    function GetLayer(Index: Integer): TcxGridCardRowLayer;
    function GetLayerRowViewInfo(ALayerIndex, AIndex: Integer): TcxGridCardRowViewInfo;
    function GetSeparatorCount: Integer;
    procedure CreateLayers;
    procedure DestroyLayers;
  protected
    procedure BeforeCalculation; virtual;
    procedure CalculateLayerBounds(ALayer: TcxGridCardRowLayer; AMaxRowHeight: Integer;
      var ABounds: TRect); virtual; abstract;
    procedure CalculateLayers(AWidth: Integer); virtual;
    function CalculateRowsHeaderWidth(ARows: TList; AWidth: Integer): Integer; virtual;
    function GetLayerClass: TcxGridCardRowLayerClass; virtual;
    function GetLayersHeight: Integer; virtual; abstract;
    function GetLayersLength: Integer; virtual;
    function GetLayersThickness: Integer; virtual;
    function GetNonContentThickness: Integer; virtual;
    function HasSeparator(ALayer: TcxGridCardRowLayer): Boolean;
    function IsLayerVisible(ALayer: TcxGridCardRowLayer; ATopRowIndex: Integer): Boolean; overload; virtual;
    function IsLayerVisible(const ABounds, ALayerBounds: TRect): Boolean; overload; virtual;

    property LayersLength: Integer read GetLayersLength;
    property LayersThickness: Integer read GetLayersThickness;
    property NonContentThickness: Integer read GetNonContentThickness;
  public
    constructor Create(ACardViewInfo: TcxGridCardViewInfo; ASimple: Boolean); virtual;
    destructor Destroy; override;
    procedure Calculate(AWidth: Integer); virtual;
    procedure CalculateRows(const ABounds: TRect; ATopRowIndex, AMaxRowHeight: Integer;
      var AVisibleRowCount, APartVisibleRowCount: Integer); virtual;
    function GetHeaderWidth(ARowViewInfo: TcxGridCardRowViewInfo): Integer; virtual; abstract;
    function GetIndexInLayer(ARowViewInfo: TcxGridCardRowViewInfo): Integer; overload;
    function GetLayerIndex(ARowViewInfo: TcxGridCardRowViewInfo): Integer; overload;
    function GetLayerIndex(const P: TPoint): Integer; overload;
    function GetSeparatorBounds(AIndex: Integer): TRect; virtual; abstract;
    function IsLeft(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; virtual; abstract;
    function IsTop(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; virtual; abstract;
    function IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean; virtual; abstract;

    property CardViewInfo: TcxGridCardViewInfo read FCardViewInfo;
    property LayerRowViewInfos[ALayerIndex, AIndex: Integer]: TcxGridCardRowViewInfo read GetLayerRowViewInfo;
    property LayersHeight: Integer read GetLayersHeight;
    property Layers[Index: Integer]: TcxGridCardRowLayer read GetLayer;
    property SeparatorCount: Integer read GetSeparatorCount;
    property SeparatorWidth: Integer read FSeparatorWidth write FSeparatorWidth;
    property Simple: Boolean read FSimple;
  end;

  // - horizontal

  TcxGridCardRowHorizontalLayer = class(TcxGridCardRowLayer)
  private
    function GetOwner: TcxGridCardRowHorizontalLayout;
  protected
    function CalculateLength: Integer; override;
    procedure CalculateRowWidths(AAvailableWidth: Integer); virtual;
    function CalculateThickness: Integer; override;
  public
    procedure Calculate(AOwnerWidth: Integer); override;
    procedure CalculateRows(const ABounds: TRect; ATopRowIndex: Integer;
      var AVisibleRowCount, APartVisibleRowCount: Integer); override;
    function HasPoint(const P: TPoint): Boolean; override;
    property Owner: TcxGridCardRowHorizontalLayout read GetOwner;
  end;

  TcxGridCardRowHorizontalLayout = class(TcxGridCardRowLayout)
  private
    FFirstColumnRowsHeaderWidth: Integer;
  protected
    function CalculateFirstColumnRowsHeaderWidth(AWidth: Integer): Integer; virtual;
    procedure CalculateLayerBounds(ALayer: TcxGridCardRowLayer; AMaxRowHeight: Integer;
      var ABounds: TRect); override;
    function GetLayerClass: TcxGridCardRowLayerClass; override;
    function GetLayersHeight: Integer; override;
    function IsLayerVisible(ALayer: TcxGridCardRowLayer; ATopRowIndex: Integer): Boolean; override;
    function IsLayerVisible(const ABounds, ALayerBounds: TRect): Boolean; override;
  public
    constructor Create(ACardViewInfo: TcxGridCardViewInfo; ASimple: Boolean); override;
    procedure Calculate(AWidth: Integer); override;
    function GetHeaderWidth(ARowViewInfo: TcxGridCardRowViewInfo): Integer; override;
    function GetSeparatorBounds(AIndex: Integer): TRect; override;
    function IsLeft(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; override;
    function IsTop(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; override;
    function IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean; override;
    property FirstColumnRowsHeaderWidth: Integer read FFirstColumnRowsHeaderWidth;
  end;

  // - vertical

  TcxGridCardRowVerticalLayer = class(TcxGridCardRowLayer)
  private
    FRowsHeaderWidth: Integer;
  protected
    function CalculateLength: Integer; override;
    function CalculateRowsHeaderWidth: Integer; virtual;
    function CalculateThickness: Integer; override;
    procedure SetThickness(Value: Integer); override;
  public
    constructor Create(AOwner: TcxGridCardRowLayout; AIndex: Integer); override;
    procedure Calculate(AOwnerWidth: Integer); override;
    procedure CalculateRows(const ABounds: TRect; ATopRowIndex: Integer;
      var AVisibleRowCount, APartVisibleRowCount: Integer); override;
    function HasPoint(const P: TPoint): Boolean; override;
    property RowsHeaderWidth: Integer read FRowsHeaderWidth;
  end;

  TcxGridCardRowVerticalLayout = class(TcxGridCardRowLayout)
  protected
    procedure CalculateLayerBounds(ALayer: TcxGridCardRowLayer; AMaxRowHeight: Integer;
      var ABounds: TRect); override;
    procedure CalculateLayers(AWidth: Integer); override;
    procedure CalculateLayerWidths(AAvailableWidth: Integer); virtual;
    function GetLayerClass: TcxGridCardRowLayerClass; override;
    function GetLayersHeight: Integer; override;
  public
    function GetHeaderWidth(ARowViewInfo: TcxGridCardRowViewInfo): Integer; override;
    function GetSeparatorBounds(AIndex: Integer): TRect; override;
    function IsLeft(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; override;
    function IsTop(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; override;
    function IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean; override;
  end;

  // card

  TcxGridCardViewInfo = class(TcxGridCustomLayoutRecordViewInfo)
  private
    FExpandButtonViewInfo: TcxGridCardExpandButtonViewInfo;
    FLayout: TcxGridCardRowLayout;
    FLayoutCalculated: Boolean;
    FPartVisibleRowCount: Integer;
    FRowViewInfos: TList;
    FScrollButtonDown: TcxGridCardScrollButtonDownViewInfo;
    FScrollButtonUp: TcxGridCardScrollButtonUpViewInfo;
    FVisibleRowCount: Integer;
    function GetCacheItem: TcxGridCardViewInfoCacheItem;
    function GetCardBorderWidth: Integer;
    function GetExpandButtonSizeValue: Integer;
    function GetFirstCaptionRowViewInfo: TcxGridCardRowViewInfo;
    function GetGridView: TcxGridCardView;
    function GetGridRecord: TcxGridCard;
    function GetRecordsViewInfo: TcxGridCardsViewInfo;
    function GetRowViewInfo(Index: Integer): TcxGridCardRowViewInfo;
    function GetRowViewInfoCount: Integer;
    function GetTopRowIndex: Integer;
    function GetVisibleRowViewInfo(Index: Integer): TcxGridCardRowViewInfo;
    function GetVisibleRowViewInfoCount: Integer;
    procedure SetTopRowIndex(Value: Integer);
    procedure CreateRowViewInfos;
    procedure DestroyRowViewInfos;
    procedure CreateScrollButtons;
    procedure DestroyScrollButtons;
  protected
    procedure CalculateExpandButtonBounds(var ABounds: TRect); override;
    function CalculateHeight: Integer; override;
    procedure CalculateLayout; virtual;
    procedure DoCalculateLayout;
    procedure CalculateRows; virtual;
    procedure CalculateRowVisibles; virtual;
    function CalculateWidth: Integer; override;
    function CanGenerateExpandButtonHitTest: Boolean; override;
    function GetAutoHeight: Boolean; override;
    function GetBackgroundBitmapBounds: TRect; override;
    function GetContentBounds: TRect; override;
    function GetContentHeight: Integer; override;
    function GetContentWidth: Integer; override;
    function GetExpandButtonAlignment: TcxGridCardExpandButtonAlignment; virtual;
    function GetExpandButtonAreaBounds: TRect; override;
    function GetExpandButtonAreaWidth: Integer; virtual;
    function GetHeight: Integer; override;
    function GetLayerSeparatorAreaBounds: TRect; virtual;
    function GetLayerSeparatorColor: TColor; virtual;
    function GetLayerSeparatorWidth: Integer; virtual;
    function GetMaxRowViewInfoHeight: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRestSpaceBounds: TRect; virtual;
    function GetScrollableAreaBounds: TRect; virtual;
    function GetScrollButtonDownBounds: TRect; virtual;
    function GetScrollButtonDownViewInfoClass: TcxGridCardScrollButtonDownViewInfoClass; virtual;
    function GetScrollButtonUpViewInfoClass: TcxGridCardScrollButtonUpViewInfoClass; virtual;
    function GetScrollButtonHeight: Integer; virtual;
    function GetScrollButtonUpBounds: TRect; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    function HasCategorySeparator(ARowViewInfo: TcxGridCardCategoryRowViewInfo): Boolean; overload; virtual;
    class function HasCategorySeparator(ARow: TcxGridCardViewRow): Boolean; overload; virtual;
    function HasIndent(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; virtual;
    function HasLayerSeparators: Boolean; virtual;
    function IsRowPartiallyVisible(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; virtual;
    function IsRowVisible(ARowViewInfo: TcxGridCardRowViewInfo): Boolean; virtual;
    function SupportsScrolling: Boolean; virtual;

    function GetZone(AHitTest: TcxCustomGridHitTest): TcxGridItemContainerZone; virtual;
    function GetZoneBounds(AZone: TcxGridCardRowContainerZone): TRect; virtual;
    function IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean; virtual;

    function GetExpandButtonViewInfoClass: TcxGridCardExpandButtonViewInfoClass; virtual;
    class function GetRowViewInfoClass(ARow: TcxGridCardViewRow): TcxGridCardRowViewInfoClass; virtual;

    property CacheItem: TcxGridCardViewInfoCacheItem read GetCacheItem;
    property ExpandButtonAreaWidth: Integer read GetExpandButtonAreaWidth;
    property ExpandButtonSize: Integer read GetExpandButtonSizeValue;
    property FirstCaptionRowViewInfo: TcxGridCardRowViewInfo read GetFirstCaptionRowViewInfo;
    property LayerSeparatorWidth: Integer read GetLayerSeparatorWidth;
    property MaxRowViewInfoHeight: Integer read GetMaxRowViewInfoHeight;
    property ScrollButtonDownBounds: TRect read GetScrollButtonDownBounds;
    property ScrollButtonHeight: Integer read GetScrollButtonHeight;
    property ScrollButtonUpBounds: TRect read GetScrollButtonUpBounds;
  public
    constructor Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
      ARecord: TcxCustomGridRecord); override;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect; override;
    procedure GetCardBorderViewParams(var AParams: TcxViewParams); virtual;
    function GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo; override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure MainCalculate(ALeftBound, ATopBound: Integer); override;
    procedure MakeRowVisible(ARow: TcxGridCardViewRow); virtual;
    function NeedsScrollingDown: Boolean; virtual;
    function NeedsScrollingUp: Boolean; virtual;
    function VisibleRowViewInfoIndexOf(ARow: TcxGridCardViewRow): Integer; overload;
    function VisibleRowViewInfoIndexOf(ARowViewInfo: TcxGridCardRowViewInfo): Integer; overload;

    property CardBorderWidth: Integer read GetCardBorderWidth;
    //property DataWidth: Integer read GetDataWidth;
    property ExpandButtonAlignment: TcxGridCardExpandButtonAlignment read GetExpandButtonAlignment;
    property ExpandButtonViewInfo: TcxGridCardExpandButtonViewInfo read FExpandButtonViewInfo;
    property GridView: TcxGridCardView read GetGridView;
    property GridRecord: TcxGridCard read GetGridRecord;
    property LayerSeparatorAreaBounds: TRect read GetLayerSeparatorAreaBounds;
    property LayerSeparatorColor: TColor read GetLayerSeparatorColor;
    property Layout: TcxGridCardRowLayout read FLayout;
    property PartVisibleRowCount: Integer read FPartVisibleRowCount;
    property RecordsViewInfo: TcxGridCardsViewInfo read GetRecordsViewInfo;
    property RestSpaceBounds: TRect read GetRestSpaceBounds;
    property RowViewInfoCount: Integer read GetRowViewInfoCount;
    property RowViewInfos[Index: Integer]: TcxGridCardRowViewInfo read GetRowViewInfo;
    property ScrollableAreaBounds: TRect read GetScrollableAreaBounds;
    property ScrollButtonDown: TcxGridCardScrollButtonDownViewInfo read FScrollButtonDown;
    property ScrollButtonUp: TcxGridCardScrollButtonUpViewInfo read FScrollButtonUp;
    property TopRowIndex: Integer read GetTopRowIndex write SetTopRowIndex;
    property VisibleRowCount: Integer read FVisibleRowCount;
    property VisibleRowViewInfoCount: Integer read GetVisibleRowViewInfoCount;
    property VisibleRowViewInfos[Index: Integer]: TcxGridCardRowViewInfo read GetVisibleRowViewInfo;
  end;

  // cards

  TcxGridCardsViewInfoHorizontalCalculator = class(TcxGridCustomLayoutViewInfoHorizontalCalculator)
  protected
    function GetRecordSpaceVert: Integer; override;
  end;

  TcxGridCardsViewInfoVerticalCalculator = class(TcxGridCustomLayoutViewInfoVerticalCalculator)
  protected
    function GetRecordSpaceHorz: Integer; override;
  end;

  TcxGridCardsViewInfo = class(TcxGridCustomLayoutRecordsViewInfo)
  private
    function GetCardBorderWidth: Integer;
    function GetCardContentWidth: Integer;
    function GetColumns: TcxGridCardViewColumns;
    function GetColumnWidth: Integer;
    function GetGridView: TcxGridCardView;
    function GetGridViewInfo: TcxGridCardViewViewInfo;
    function GetItem(Index: Integer): TcxGridCardViewInfo;
    function GetRowHeight: Integer;
    function GetViewData: TcxGridCardViewViewData;
  protected
    function CalculateRecordHeight: Integer; override;
    function CalculateCardRowHeight(ARow: TcxGridCardViewRow): Integer; virtual;
    function CalculateRecordWidth: Integer; override;
    function GetAutoCellHeight: Boolean; virtual;
    function GetAutoDataRecordHeight: Boolean; override;
    function GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass; override;
    function GetSeparatorWidth: Integer; override;

    function GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass; override;
    function GetBandsClass: TcxGridCustomLayoutViewBandsClass; override;

    property GridView: TcxGridCardView read GetGridView;
    property GridViewInfo: TcxGridCardViewViewInfo read GetGridViewInfo;
    property ViewData: TcxGridCardViewViewData read GetViewData;
  public
    CardRowHeights: array of Integer;
    function GetRealItem(ARecord: TcxCustomGridRecord): TcxGridCardViewInfo; reintroduce; virtual;
    function GetZone(AHitTest: TcxCustomGridHitTest): TcxGridItemContainerZone; virtual;
    function UseCardHeight: Boolean; virtual;
    function UseCardRowHeights: Boolean; virtual;

    property AutoCellHeight: Boolean read GetAutoCellHeight;
    property CardBorderWidth: Integer read GetCardBorderWidth;
    property CardContentWidth: Integer read GetCardContentWidth;
    property Columns: TcxGridCardViewColumns read GetColumns;
    property ColumnWidth: Integer read GetColumnWidth;
    property Items[Index: Integer]: TcxGridCardViewInfo read GetItem; default;
    property RowHeight: Integer read GetRowHeight;
  end;

  // separators

  TcxGridCardViewSeparatorsViewInfo = class(TcxGridCustomLayoutViewSeparatorsViewInfo)
  private
    function GetGridView: TcxGridCardView;
    function GetGridViewInfo: TcxGridCardViewViewInfo;
    function GetCardsViewInfo: TcxGridCardsViewInfo;
  protected
    function GetHitTestItem(Index: Integer): TRect; virtual; abstract;
    function GetHitTestItemCount: Integer; virtual; abstract;

    property GridView: TcxGridCardView read GetGridView;
    property GridViewInfo: TcxGridCardViewViewInfo read GetGridViewInfo;
    property HitTestItemCount: Integer read GetHitTestItemCount;
    property HitTestItems[Index: Integer]: TRect read GetHitTestItem;
    property CardsViewInfo: TcxGridCardsViewInfo read GetCardsViewInfo;
  public
    function GetHitTest(const P: TPoint): TcxGridCardViewSeparatorHitTest; virtual;

    property Color: TColor read GetColor;
  end;

  TcxGridCardViewSeparatorsHorizontalViewInfo = class(TcxGridCardViewSeparatorsViewInfo)
  protected
    function GetHitTestItem(Index: Integer): TRect; override;
    function GetHitTestItemCount: Integer; override;
  public
    procedure DoCalculate; override;
  end;

  TcxGridCardViewSeparatorsVerticalViewInfo = class(TcxGridCardViewSeparatorsViewInfo)
  protected
    function GetHitTestItem(Index: Integer): TRect; override;
    function GetHitTestItemCount: Integer; override;
  public
    procedure DoCalculate; override;
  end;

  // card view

  TcxGridCardViewViewInfo = class(TcxGridCustomLayoutViewViewInfo)
  private
    function GetController: TcxGridCardViewController;
    function GetGridView: TcxGridCardView;
    function GetRecordsViewInfo: TcxGridCardsViewInfo;
    function GetSeparatorsViewInfo: TcxGridCardViewSeparatorsViewInfo;
    function GetViewData: TcxGridCardViewViewData;
  protected
    function DoGetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetDefaultGridModeBufferCount: Integer; override;
    function GetScrollableAreaBoundsForEdit: TRect; override;

    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;
    function GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass; override;

    property Controller: TcxGridCardViewController read GetController;
    property GridView: TcxGridCardView read GetGridView;
    property ViewData: TcxGridCardViewViewData read GetViewData;
  public
    property RecordsViewInfo: TcxGridCardsViewInfo read GetRecordsViewInfo;
    property SeparatorsViewInfo: TcxGridCardViewSeparatorsViewInfo read GetSeparatorsViewInfo;
  end;

  // cache

  TcxGridCardViewInfoCacheItem = class(TcxGridCustomLayoutViewInfoCacheItem)
  private
    FTopRowIndex: Integer;
    FIsTopRowIndexAssigned: Boolean;
    procedure SetTopRowIndex(Value: Integer);
  public
    procedure UnassignValues(AKeepMaster: Boolean); override;
    property TopRowIndex: Integer read FTopRowIndex write SetTopRowIndex;
    property IsTopRowIndexAssigned: Boolean read FIsTopRowIndexAssigned write FIsTopRowIndexAssigned;
  end;

  { view }

  TcxGridCardViewRowOptions = class(TcxCustomGridTableItemOptions)
  private
    FExpanding: Boolean;
    FShowData: Boolean;
    procedure SetExpanding(Value: Boolean);
    procedure SetShowData(Value: Boolean);
  protected
    procedure BeforeShowCaptionChange; override;
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
    procedure Assign(Source: TPersistent); override;
  published
    property Expanding: Boolean read FExpanding write SetExpanding default True;
    property FilteringAddValueItems;
    property FilteringExcelPopupApplyChanges;
    property FilteringExcelPopupDateTimeValuesPageType;
    property FilteringExcelPopupDefaultPage;
    property FilteringExcelPopupNumericValuesPageType;
    property FilteringFilteredItemsList;
    property FilteringMRUItemsList;
    property FilteringPopup;
    property FilteringPopupIncrementalFiltering;
    property FilteringPopupIncrementalFilteringOptions;
    property FilteringPopupMode;
    property FilteringPopupMultiSelect;
    property Moving;
    property ShowCaption;
    property ShowData: Boolean read FShowData write SetShowData default True;
    property ShowEditButtons;
  end;

  { TcxGridCardViewRowPosition }

  TcxGridCardViewRowPositionClass = class of TcxGridCardViewRowPosition;
  TcxGridCardViewRowPosition = class(TcxCustomGridTableItemCustomOptions)
  private
    FBeginsLayer: Boolean;
    FLineCount: Integer;
    FWidth: Integer;
    function GetBeginsLayer: Boolean;
    function GetColIndex: Integer;
    function GetGridView: TcxGridCardView;
    function GetIndexInLayer: Integer;
    function GetItem: TcxGridCardViewRow;
    function GetLayerIndex: Integer;
    function GetLayerVisibleIndex: Integer;
    function GetRowIndex: Integer;
    function GetVisibleColIndex: Integer;
    function GetVisibleIndexInLayer: Integer;
    function GetVisibleRowIndex: Integer;
    procedure SetBeginsLayer(Value: Boolean);
    procedure SetColIndex(Value: Integer);
    procedure SetIndexInLayer(Value: Integer);
    procedure SetLayerIndex(Value: Integer);
    procedure SetLineCount(Value: Integer);
    procedure SetRowIndex(Value: Integer);
    procedure SetWidth(Value: Integer);
  protected
    procedure AssignParams;
    procedure ChangeScale(M, D: Integer); virtual;
    procedure SaveParams;
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
    procedure Assign(Source: TPersistent); override;
    function IsWholeLine: Boolean;

    property GridView: TcxGridCardView read GetGridView;
    property Item: TcxGridCardViewRow read GetItem;

    property ColIndex: Integer read GetColIndex write SetColIndex;
    property RowIndex: Integer read GetRowIndex write SetRowIndex;

    property VisibleColIndex: Integer read GetVisibleColIndex;
    property VisibleRowIndex: Integer read GetVisibleRowIndex;

    property LayerVisibleIndex: Integer read GetLayerVisibleIndex;
    property VisibleIndexInLayer: Integer read GetVisibleIndexInLayer;
  published
    property BeginsLayer: Boolean read GetBeginsLayer write SetBeginsLayer;
    property IndexInLayer: Integer read GetIndexInLayer write SetIndexInLayer stored False;
    property LayerIndex: Integer read GetLayerIndex write SetLayerIndex stored False;
    property LineCount: Integer read FLineCount write SetLineCount default 1;
    property Width: Integer read FWidth write SetWidth default 0;
  end;

  TcxGridCardViewRowStyles = class(TcxCustomGridTableItemStyles)
  private
    FOnGetCaptionRowStyle: TcxGridGetCellStyleEvent;
    FOnGetCaptionStyle: TcxGridGetCellStyleEvent;
    FOnGetCategoryRowStyle: TcxGridGetCellStyleEvent;
    function GetGridViewValue: TcxGridCardView;
    function GetItem: TcxGridCardViewRow;
    procedure SetOnGetCaptionRowStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetCaptionStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetCategoryRowStyle(Value: TcxGridGetCellStyleEvent);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCaptionParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetCaptionRowParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetCategoryRowParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    property GridView: TcxGridCardView read GetGridViewValue;
    property Item: TcxGridCardViewRow read GetItem;
  published
    property Caption: TcxStyle index isRowCaption read GetValue write SetValue;
    property CaptionRow: TcxStyle index isCaptionRow read GetValue write SetValue;
    property CategoryRow: TcxStyle index isCategoryRow read GetValue write SetValue;
    property OnGetCaptionRowStyle: TcxGridGetCellStyleEvent read FOnGetCaptionRowStyle
      write SetOnGetCaptionRowStyle;
    property OnGetCaptionStyle: TcxGridGetCellStyleEvent read FOnGetCaptionStyle
      write SetOnGetCaptionStyle;
    property OnGetCategoryRowStyle: TcxGridGetCellStyleEvent read FOnGetCategoryRowStyle
      write SetOnGetCategoryRowStyle;
  end;

  TcxGridCardViewRowKind = (rkData, rkCaption, rkCategory);

  TcxGridCardViewRow = class(TcxCustomGridTableItem)
  private
    FCategoryItems: TList;
    FCategoryRow: TcxGridCardViewRow;
    FExpanded: Boolean;
    FKind: TcxGridCardViewRowKind;
    FLockPositionSync: Boolean;
    FPosition: TcxGridCardViewRowPosition;
    function GetCaptionAlignmentHorz: TAlignment;
    function GetCaptionAlignmentVert: TcxAlignmentVert;
    function GetCategoryItem(AIndex: Integer): TcxGridCardViewRow;
    function GetCategoryItemCount: Integer;
    function GetExpanded: Boolean;
    function GetGridView: TcxGridCardView;
    function GetOptions: TcxGridCardViewRowOptions;
    function GetStyles: TcxGridCardViewRowStyles;
    procedure SetCaptionAlignmentHorz(Value: TAlignment);
    procedure SetCaptionAlignmentVert(Value: TcxAlignmentVert);
    procedure SetCategoryRow(Value: TcxGridCardViewRow);
    procedure SetCategoryRowValue(Value: TcxGridCardViewRow);
    procedure SetExpanded(Value: Boolean);
    procedure SetKind(Value: TcxGridCardViewRowKind);
    procedure SetOptions(Value: TcxGridCardViewRowOptions);
    procedure SetPosition(Value: TcxGridCardViewRowPosition);
    procedure SetStyles(Value: TcxGridCardViewRowStyles);
    function IsCaptionAlignmentHorzStored: Boolean;
    function IsCaptionAlignmentVertStored: Boolean;
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function GetPositionClass: TcxGridCardViewRowPositionClass; virtual;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; override;

    function CanEdit: Boolean; override;
    function CanExpand: Boolean; virtual;
    function CanFilter(AVisually: Boolean): Boolean; override;
    function CanFocus(ARecord: TcxCustomGridRecord): Boolean; override;
    procedure ChangeGroupIndex(Value: Integer); override;
    procedure ChangeScale(M, D: Integer); override;
    function DefaultAlternateCaption: string; override;
    function DefaultWidth: Integer; override;
    function GetActuallyVisible: Boolean; override;
    function GetExpandable: Boolean; virtual;
    procedure GetItems(ARows: TList; AIncludeSubItems: Boolean); virtual;
    function GetVisibleCaption: string; override;
    function HasExpandableItems: Boolean; virtual;

    property CategoryItemsList: TList read FCategoryItems;
    property GridView: TcxGridCardView read GetGridView;
    property LockPositionSync: Boolean read FLockPositionSync write FLockPositionSync;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function HasCardExpandButton: Boolean; virtual;
    function HasExpandButton: Boolean; virtual;
    function IsVisibleInCard(ACard: TcxGridCard): Boolean; virtual;
    procedure MoveTo(AIndex: Integer; AMoveSubItems: Boolean);

    property CategoryItemCount: Integer read GetCategoryItemCount;
    property CategoryItems[AIndex: Integer]: TcxGridCardViewRow read GetCategoryItem;
    property CategoryRow: TcxGridCardViewRow read FCategoryRow write SetCategoryRow;
    property Expandable: Boolean read GetExpandable;
    property Hidden;  // obsolete, use VisibleForCustomization
  published
    property CaptionAlignmentHorz: TAlignment read GetCaptionAlignmentHorz write SetCaptionAlignmentHorz stored IsCaptionAlignmentHorzStored;
    property CaptionAlignmentVert: TcxAlignmentVert read GetCaptionAlignmentVert write SetCaptionAlignmentVert stored IsCaptionAlignmentVertStored;
    property Expanded: Boolean read GetExpanded write SetExpanded default False;
    property Kind: TcxGridCardViewRowKind read FKind write SetKind default rkData;
    property Options: TcxGridCardViewRowOptions read GetOptions write SetOptions;
    property Position: TcxGridCardViewRowPosition read FPosition write SetPosition;
    property SortIndex;
    property SortOrder;
    property Styles: TcxGridCardViewRowStyles read GetStyles write SetStyles;
    property VisibleForCustomization;
  end;

  // options

  TcxGridCardViewBackgroundBitmaps = class(TcxCustomGridTableBackgroundBitmaps)
  protected
    function GetBitmapStyleIndex(Index: Integer): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
    function GetBitmap(Index: Integer): TBitmap; override;
  published
    property CaptionRow: TBitmap index bbCaptionRow read GetValue write SetValue;
    property CardBorder: TBitmap index bbCardBorder read GetValue write SetValue;
    property RowCaption: TBitmap index bbRowCaption read GetValue write SetValue;
  end;

  TcxGridCardViewDateTimeHandling = class(TcxCustomGridTableDateTimeHandling);

  TcxGridCardViewFiltering = class(TcxCustomGridTableFiltering)
  private
    function GetRowAddValueItems: Boolean;
    function GetRowExcelPopup: TcxGridItemExcelFilterPopupOptions;
    function GetRowFilteredItemsList: Boolean;
    function GetRowMRUItemsList: Boolean;
    function GetRowMRUItemsListCount: Integer;
    function GetRowPopup: TcxGridItemFilterPopupOptions;
    function GetRowPopupMode: TdxFilterPopupWindowMode;
    procedure SetRowAddValueItems(Value: Boolean);
    procedure SetRowExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
    procedure SetRowFilteredItemsList(Value: Boolean);
    procedure SetRowMRUItemsList(Value: Boolean);
    procedure SetRowMRUItemsListCount(Value: Integer);
    procedure SetRowPopup(Value: TcxGridItemFilterPopupOptions);
    procedure SetRowPopupMode(Value: TdxFilterPopupWindowMode);
  protected
    procedure DefineProperties(Filer: TFiler); override;
  public
    // obsolete - use RowPopup.DropDownWidth
    property DropDownWidth;
    property RowPopupDropDownWidth: Integer read GetItemPopupDropDownWidth write SetItemPopupDropDownWidth;
    // obsolete - use RowPopup.MaxDropDownItemCount
    property MaxDropDownCount;
    property RowPopupMaxDropDownItemCount: Integer read GetItemPopupMaxDropDownItemCount write SetItemPopupMaxDropDownItemCount;
  published
    property RowAddValueItems: Boolean read GetRowAddValueItems write SetRowAddValueItems default True;
    property RowExcelPopup: TcxGridItemExcelFilterPopupOptions read GetRowExcelPopup write SetRowExcelPopup;
    property RowFilteredItemsList: Boolean read GetRowFilteredItemsList write SetRowFilteredItemsList default False;
    property RowMRUItemsList: Boolean read GetRowMRUItemsList write SetRowMRUItemsList default True;
    property RowMRUItemsListCount: Integer read GetRowMRUItemsListCount write SetRowMRUItemsListCount default cxGridFilterDefaultItemMRUItemsListCount;
    property RowPopup: TcxGridItemFilterPopupOptions read GetRowPopup write SetRowPopup;
    property RowPopupMode: TdxFilterPopupWindowMode read GetRowPopupMode write SetRowPopupMode default fpmDefault;
  end;

  TcxGridCardViewOptionsBehavior = class(TcxCustomGridTableOptionsBehavior)
  private
    FExpandRowOnDblClick: Boolean;
    FRowCaptionHints: Boolean;
    procedure SetExpandRowOnDblClick(Value: Boolean);
    procedure SetRowCaptionHints(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ExpandRowOnDblClick: Boolean read FExpandRowOnDblClick write SetExpandRowOnDblClick default True;
    property ImmediateEditor;
    property PullFocusing;
    property RowCaptionHints: Boolean read FRowCaptionHints write SetRowCaptionHints default True;
  end;

  { TcxGridCardViewOptionsCustomize }

  TcxGridCardViewOptionsCustomize = class(TcxGridCustomLayoutViewOptionsCustomize)
  private
    FCardSizing: Boolean;
    FLayeredRows: Boolean;
    FRowExpanding: Boolean;
    function GetCardExpanding: Boolean;
    function GetGridView: TcxGridCardView;
    function GetRowFiltering: Boolean;
    function GetRowHiding: Boolean;
    function GetRowMoving: Boolean;
    procedure SetCardExpanding(Value: Boolean);
    procedure SetCardSizing(Value: Boolean);
    procedure SetLayeredRows(Value: Boolean);
    procedure SetRowExpanding(Value: Boolean);
    procedure SetRowFiltering(Value: Boolean);
    procedure SetRowHiding(Value: Boolean);
    procedure SetRowMoving(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxGridCardView read GetGridView;
  published
    property CardExpanding: Boolean read GetCardExpanding write SetCardExpanding default False;
    property CardSizing: Boolean read FCardSizing write SetCardSizing default True;
    property LayeredRows: Boolean read FLayeredRows write SetLayeredRows default False;
    property RowExpanding: Boolean read FRowExpanding write SetRowExpanding default True;
    property RowFiltering: Boolean read GetRowFiltering write SetRowFiltering default True;
    property RowHiding: Boolean read GetRowHiding write SetRowHiding default False;
    property RowMoving: Boolean read GetRowMoving write SetRowMoving default False;
  end;

  TcxGridCardViewOptionsData = class(TcxCustomGridTableOptionsData);

  TcxGridCardViewOptionsSelection = class(TcxCustomGridTableOptionsSelection)
  private
    FCardBorderSelection: Boolean;
    procedure SetCardBorderSelection(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property CardBorderSelection: Boolean read FCardBorderSelection write SetCardBorderSelection default True;
    property HideFocusRect;
    property HideFocusRectOnExit;
    property HideSelection;
    property InvertSelect;
    property UnselectFocusedRecordOnExit;
  end;

  { TcxGridCardViewOptionsView }

  TcxGridCardViewOptionsView = class(TcxGridCustomLayoutViewOptionsView)
  strict private
    FCaptionWidth: Integer;
    FCardAutoWidth: Boolean;
    FCardBorderWidth: Integer;
    FCardExpandButtonAlignment: TcxGridCardExpandButtonAlignment;
    FCardWidth: Integer;
    FCategoryIndent: Integer;
    FCategoryRowCaptionInRowAlternateCaption: Boolean;
    FCategorySeparatorWidth: Integer;
    FEmptyRows: Boolean;
    FLayerSeparatorWidth: Integer;

    function GetCardIndent: Integer;
    function GetGridView: TcxGridCardView;
    function GetRowCaptionAutoHeight: Boolean;
    function GetRowCaptionEndEllipsis: Boolean;
    function GetShowRowFilterButtons: TcxGridShowItemFilterButtons;
    procedure SetCaptionWidth(Value: Integer);
    procedure SetCardAutoWidth(Value: Boolean);
    procedure SetCardBorderWidth(Value: Integer);
    procedure SetCardExpandButtonAlignment(Value: TcxGridCardExpandButtonAlignment);
    procedure SetCardIndent(Value: Integer);
    procedure SetCardWidth(Value: Integer);
    procedure SetCategoryIndent(Value: Integer);
    procedure SetCategoryRowCaptionInRowAlternateCaption(Value: Boolean);
    procedure SetCategorySeparatorWidth(Value: Integer);
    procedure SetEmptyRows(Value: Boolean);
    procedure SetLayerSeparatorWidth(Value: Integer);
    procedure SetRowCaptionAutoHeight(Value: Boolean);
    procedure SetRowCaptionEndEllipsis(Value: Boolean);
    procedure SetShowRowFilterButtons(Value: TcxGridShowItemFilterButtons);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    //
    property GridView: TcxGridCardView read GetGridView;
  published
    property CaptionSeparator;
    property CaptionWidth: Integer read FCaptionWidth write SetCaptionWidth default 0;
    property CardAutoWidth: Boolean read FCardAutoWidth write SetCardAutoWidth default False;
    property CardBorderWidth: Integer read FCardBorderWidth write SetCardBorderWidth default cxGridDefaultCardBorderWidth;
    property CardExpandButtonAlignment: TcxGridCardExpandButtonAlignment read FCardExpandButtonAlignment write SetCardExpandButtonAlignment default cebaRight;
    property CardIndent: Integer read GetCardIndent write SetCardIndent;
    property CardWidth: Integer read FCardWidth write SetCardWidth default cxGridCardDefaultWidth;
    property CategoryIndent: Integer read FCategoryIndent write SetCategoryIndent default cxGridCardDefaultCategoryIndent;
    property CategoryRowCaptionInRowAlternateCaption: Boolean read FCategoryRowCaptionInRowAlternateCaption write SetCategoryRowCaptionInRowAlternateCaption default False;
    property CategorySeparatorWidth: Integer read FCategorySeparatorWidth write SetCategorySeparatorWidth default cxGridCardDefaultCategorySeparatorWidth;
    property CellAutoHeight;
    property CellTextMaxLineCount;
    property EmptyRows: Boolean read FEmptyRows write SetEmptyRows default True;
    property LayerSeparatorWidth: Integer read FLayerSeparatorWidth write SetLayerSeparatorWidth default cxGridCardDefaultLayerSeparatorWidth;
    property RowCaptionAutoHeight: Boolean read GetRowCaptionAutoHeight write SetRowCaptionAutoHeight default False;
    property RowCaptionEndEllipsis: Boolean read GetRowCaptionEndEllipsis write SetRowCaptionEndEllipsis default False;
    property SeparatorColor;
    property SeparatorWidth;
    property ShowRowFilterButtons: TcxGridShowItemFilterButtons read GetShowRowFilterButtons
      write SetShowRowFilterButtons default sfbDefault;
  end;

  // styles

  TcxGridCardViewStyles = class(TcxCustomGridTableViewStyles)
  private
    FOnGetCaptionRowStyle: TcxGridGetCellStyleEvent;
    FOnGetCardBorderStyle: TcxGridGetRecordStyleEvent;
    FOnGetCategoryRowStyle: TcxGridGetCellStyleEvent;
    FOnGetRowCaptionStyle: TcxGridGetCellStyleEvent;
    function GetGridViewValue: TcxGridCardView;
    procedure SetOnGetCaptionRowStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetCardBorderStyle(Value: TcxGridGetRecordStyleEvent);
    procedure SetOnGetCategoryRowStyle(Value: TcxGridGetCellStyleEvent);
    procedure SetOnGetRowCaptionStyle(Value: TcxGridGetCellStyleEvent);
  protected
    function GetBackgroundBitmapIndex(Index: Integer): Integer; virtual;
    function GetDefaultBitmap(Index: Integer): TBitmap; virtual;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    procedure GetCaptionRowParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetCardBorderParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetCardBorderVisualParams(ARecord: TcxCustomGridRecord; out AParams: TcxViewParams); virtual;
    procedure GetCategoryRowParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    procedure GetDataCellContentParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); override;
    procedure GetRowCaptionParams(ARecord: TcxCustomGridRecord; AItem: TcxCustomGridTableItem;
      out AParams: TcxViewParams); virtual;
    property GridView: TcxGridCardView read GetGridViewValue;
  published
    property CaptionRow: TcxStyle index vsCaptionRow read GetValue write SetValue;
    property CardBorder: TcxStyle index vsCardBorder read GetValue write SetValue;
    property CategoryRow: TcxStyle index vsCategoryRow read GetValue write SetValue;
    property CategorySeparator: TcxStyle index vsCategorySeparator read GetValue write SetValue;
    property Inactive;
    property LayerSeparator: TcxStyle index vsLayerSeparator read GetValue write SetValue;
    property RowCaption: TcxStyle index vsRowCaption read GetValue write SetValue;
    property Selection;
    property StyleSheet;
    property OnGetCaptionRowStyle: TcxGridGetCellStyleEvent read FOnGetCaptionRowStyle write SetOnGetCaptionRowStyle;
    property OnGetCardBorderStyle: TcxGridGetRecordStyleEvent read FOnGetCardBorderStyle write SetOnGetCardBorderStyle;
    property OnGetCategoryRowStyle: TcxGridGetCellStyleEvent read FOnGetCategoryRowStyle write SetOnGetCategoryRowStyle;
    property OnGetRowCaptionStyle: TcxGridGetCellStyleEvent read FOnGetRowCaptionStyle write SetOnGetRowCaptionStyle;
  end;

  TcxGridCardViewStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxGridCardViewStyles;
    procedure SetStylesValue(Value: TcxGridCardViewStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxGridCardViewStyles read GetStylesValue write SetStylesValue;
  end;

  // view

  TcxGridCardRowEvent = procedure(Sender: TcxGridCardView; ARow: TcxGridCardViewRow) of object;
  TcxGridCardRowChangingEvent = procedure(Sender: TcxGridCardView; ARow: TcxGridCardViewRow; var AAllow: Boolean) of object;

  TcxGridCardView = class(TcxGridCustomLayoutView)
  private
    FLayoutDirection: TcxGridCardViewLayoutDirection;
    FRowLayout: TcxGridCardViewRowLayout;
    FRowLayoutController: TcxGridCardViewRowLayoutController;

    FOnRowCollapsing: TcxGridCardRowChangingEvent;
    FOnRowCollapsed: TcxGridCardRowEvent;
    FOnRowExpanding: TcxGridCardRowChangingEvent;
    FOnRowExpanded: TcxGridCardRowEvent;
    FOnRowPosChanged: TcxGridCardRowEvent;

    function GetBackgroundBitmaps: TcxGridCardViewBackgroundBitmaps;
    function GetControl: TcxCustomGrid;
    function GetController: TcxGridCardViewController;
    function GetDataController: TcxGridDataController;
    function GetDateTimeHandling: TcxGridCardViewDateTimeHandling;
    function GetFiltering: TcxGridCardViewFiltering;
    function GetFirstCaptionRow: TcxGridCardViewRow;
    function GetFirstCategoryRow: TcxGridCardViewRow;
    function GetOptionsBehavior: TcxGridCardViewOptionsBehavior;
    function GetOptionsCustomize: TcxGridCardViewOptionsCustomize;
    function GetOptionsData: TcxGridCardViewOptionsData;
    function GetOptionsSelection: TcxGridCardViewOptionsSelection;
    function GetOptionsView: TcxGridCardViewOptionsView;
    function GetPainter: TcxGridCardViewPainter;
    function GetRow(Index: Integer): TcxGridCardViewRow;
    function GetRowCount: Integer;
    function GetStyles: TcxGridCardViewStyles;
    function GetViewData: TcxGridCardViewViewData;
    function GetViewInfo: TcxGridCardViewViewInfo;
    function GetVisibleRow(Index: Integer): TcxGridCardViewRow;
    function GetVisibleRowCount: Integer;
    procedure SetBackgroundBitmaps(Value: TcxGridCardViewBackgroundBitmaps);
    procedure SetDataController(Value: TcxGridDataController);
    procedure SetDateTimeHandling(Value: TcxGridCardViewDateTimeHandling);
    procedure SetFiltering(Value: TcxGridCardViewFiltering);
    procedure SetLayoutDirection(Value: TcxGridCardViewLayoutDirection);
    procedure SetOnRowCollapsed(Value: TcxGridCardRowEvent);
    procedure SetOnRowCollapsing(Value: TcxGridCardRowChangingEvent);
    procedure SetOnRowExpanded(Value: TcxGridCardRowEvent);
    procedure SetOnRowExpanding(Value: TcxGridCardRowChangingEvent);
    procedure SetOnRowPosChanged(Value: TcxGridCardRowEvent);
    procedure SetOptionsBehavior(Value: TcxGridCardViewOptionsBehavior);
    procedure SetOptionsCustomize(Value: TcxGridCardViewOptionsCustomize);
    procedure SetOptionsData(Value: TcxGridCardViewOptionsData);
    procedure SetOptionsSelection(Value: TcxGridCardViewOptionsSelection);
    procedure SetOptionsView(Value: TcxGridCardViewOptionsView);
    procedure SetRow(Index: Integer; Value: TcxGridCardViewRow);
    procedure SetRowLayout(Value: TcxGridCardViewRowLayout);
    procedure SetStyles(Value: TcxGridCardViewStyles);
    procedure RefreshCategoryRowLinks;
  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    function GetLayoutCustomizationFormButtonCaption: string; override;

    procedure CreateHandlers; override;
    procedure DestroyHandlers; override;

    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetRowLayoutControllerClass: TcxGridCardViewRowLayoutControllerClass; virtual;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    procedure SaveRowParams;
    procedure AssignRowParams;

    function GetRowCoordinates: TcxGridCardRowCoordinatesArray;
    procedure SetRowCoordinates(const ACoordinates: TcxGridCardRowCoordinatesArray);

    procedure Updating; override;
    procedure Updated; override;

    procedure BeforeRestoring; override;
    procedure AfterRestoring; override;

    procedure ChangeItemIndex(AItem: TcxCustomGridTableItem; Value: Integer); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    function GetResizeOnBoundsChange: Boolean; override;
    procedure ItemIndexChanged(AItem: TcxCustomGridTableItem; AOldIndex: Integer); override;
    procedure ItemVisibilityChanged(AItem: TcxCustomGridTableItem; Value: Boolean); override;
    function IsRecordPixelScrolling: Boolean; override;
    procedure RefreshVisibleItemsList; override;
    procedure RowExpandedChanged(ARow: TcxGridCardViewRow); virtual;
    function RowExpandedChanging(ARow: TcxGridCardViewRow; AValue: Boolean): Boolean; virtual;

    function CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
      AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean;
      ACellViewInfo: TcxGridTableCellViewInfo): Boolean; override;
    function DrawCardBorderSelected(ARecord: TcxCustomGridRecord): Boolean; virtual;

    function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; override;
    function GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass; override;
    function GetFilteringClass: TcxCustomGridTableFilteringClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsDataClass: TcxCustomGridOptionsDataClass; override;
    function GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;

    function SupportsCardSizing: Boolean; virtual;
    function SupportsLayeredRows: Boolean; virtual;

    procedure DoRowCollapsed(ARow: TcxGridCardViewRow); virtual;
    function DoRowCollapsing(ARow: TcxGridCardViewRow): Boolean; virtual;
    procedure DoRowExpanded(ARow: TcxGridCardViewRow); virtual;
    function DoRowExpanding(ARow: TcxGridCardViewRow): Boolean; virtual;
    procedure DoRowPositionChanged(ARow: TcxGridCardViewRow); virtual;

    property FirstCaptionRow: TcxGridCardViewRow read GetFirstCaptionRow;
    property FirstCategoryRow: TcxGridCardViewRow read GetFirstCategoryRow;
  public
    function CreateRow: TcxGridCardViewRow;

    property Control: TcxCustomGrid read GetControl;
    property Controller: TcxGridCardViewController read GetController;
    property Painter: TcxGridCardViewPainter read GetPainter;
    property RowCount: Integer read GetRowCount;
    property Rows[Index: Integer]: TcxGridCardViewRow read GetRow write SetRow;
    property RowLayoutController: TcxGridCardViewRowLayoutController read FRowLayoutController;
    property ViewData: TcxGridCardViewViewData read GetViewData;
    property ViewInfo: TcxGridCardViewViewInfo read GetViewInfo;
    property VisibleRowCount: Integer read GetVisibleRowCount;
    property VisibleRows[Index: Integer]: TcxGridCardViewRow read GetVisibleRow;
  published
    property BackgroundBitmaps: TcxGridCardViewBackgroundBitmaps read GetBackgroundBitmaps write
      SetBackgroundBitmaps;
    property DataController: TcxGridDataController read GetDataController
      write SetDataController;
    property DateTimeHandling: TcxGridCardViewDateTimeHandling read GetDateTimeHandling write SetDateTimeHandling;
    property Filtering: TcxGridCardViewFiltering read GetFiltering write SetFiltering;
    property LayoutDirection: TcxGridCardViewLayoutDirection read FLayoutDirection
      write SetLayoutDirection default ldHorizontal;
    property OptionsBehavior: TcxGridCardViewOptionsBehavior read GetOptionsBehavior
      write SetOptionsBehavior;
    property OptionsCustomize: TcxGridCardViewOptionsCustomize read GetOptionsCustomize
      write SetOptionsCustomize;
    property OptionsData: TcxGridCardViewOptionsData read GetOptionsData write SetOptionsData;
    property OptionsSelection: TcxGridCardViewOptionsSelection read GetOptionsSelection
      write SetOptionsSelection;
    property OptionsView: TcxGridCardViewOptionsView read GetOptionsView write SetOptionsView;
    property RowLayout: TcxGridCardViewRowLayout read FRowLayout write SetRowLayout default rlHorizontal;
    property Styles: TcxGridCardViewStyles read GetStyles write SetStyles;
    property OnCustomization;
    property OnRowCollapsed: TcxGridCardRowEvent read FOnRowCollapsed write SetOnRowCollapsed;
    property OnRowCollapsing: TcxGridCardRowChangingEvent read FOnRowCollapsing write SetOnRowCollapsing;
    property OnRowExpanded: TcxGridCardRowEvent read FOnRowExpanded write SetOnRowExpanded;
    property OnRowExpanding: TcxGridCardRowChangingEvent read FOnRowExpanding write SetOnRowExpanding;
    property OnRowPosChanged: TcxGridCardRowEvent read FOnRowPosChanged write SetOnRowPosChanged;
  end;

implementation

uses
  SysUtils, Math, cxScrollBar, cxGridStrs;

const
  CardDataIndent = 2;
  CardExpandButtonOffset = 5;
  CardRowDefaultWidth = 20;
  CardRowExpandButtonOffset = 3;
  SeparatorSizingZone = 7;
  SeparatorSizingAddZone = 3;
  EmptyCardHeight = 30;

  CardScrollingInterval = 300;
  RowExpandingPauseTime = 800;

procedure CalculateCardRowWidths(var AWidths: array of Integer; AAvailableWidth: Integer);
var
  AAutoWidths: TcxAutoWidthObject;
  AAllFixed: Boolean;
  I: Integer;
begin
  AAutoWidths := TcxAutoWidthObject.Create(Length(AWidths));
  try
    AAllFixed := True;
    for I := 0 to Length(AWidths) - 1 do
      with AAutoWidths.AddItem do
      begin
        Width := AWidths[I];
        Fixed := Width <> 0;
        AAllFixed := AAllFixed and Fixed;
        if Width = 0 then Width := CardRowDefaultWidth;
      end;
    if AAllFixed or (AAutoWidths.Width > AAvailableWidth) then
      for I := 0 to AAutoWidths.Count - 1 do
        AAutoWidths[I].Fixed := False;
    AAutoWidths.AvailableWidth := AAvailableWidth;
    AAutoWidths.Calculate;
    for I := 0 to Length(AWidths) - 1 do
      AWidths[I] := AAutoWidths[I].AutoWidth;
  finally
    AAutoWidths.Free;
  end;
end;

{ TcxGridCardRowFilterButtonHitTest }

class function TcxGridCardRowFilterButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htRowFilterButton;
end;

class function TcxGridCardRowFilterButtonHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridCardRowIndentHitTest }

class function TcxGridCardRowIndentHitTest.GetHitTestCode: Integer;
begin
  Result := htCardRowIndent;
end;

{ TcxGridCardRowExpandButtonHitTest }

class function TcxGridCardRowExpandButtonHitTest.GetHitTestCode: Integer;
begin
  Result := htCardRowExpandButton;
end;

{ TcxGridCardRowCellHitTest }

function TcxGridCardRowCellHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  if ((GridRecord = nil) or GridRecord.Visible) and TcxGridCardViewRow(Item).CanMove then
    Result := TcxGridCardView(GridView).Controller.GetRowDragAndDropObjectClass
  else
    Result := nil;
end;

{ TcxGridCardRowCaptionHitTest }

procedure TcxGridCardRowCaptionHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridCardRowCaptionHitTest then
    RowContainerKind := TcxGridCardRowCaptionHitTest(Source).RowContainerKind;
end;

class function TcxGridCardRowCaptionHitTest.GetHitTestCode: Integer;
begin
  Result := htRowCaption;
end;

{ TcxGridCardScrollButtonDownHitTest }

class function TcxGridCardScrollButtonDownHitTest.GetHitTestCode: Integer;
begin
  Result := htCardScrollButtonDown;
end;

class function TcxGridCardScrollButtonDownHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridCardScrollButtonUpHitTest }

class function TcxGridCardScrollButtonUpHitTest.GetHitTestCode: Integer;
begin
  Result := htCardScrollButtonUp;
end;

class function TcxGridCardScrollButtonUpHitTest.CanClick: Boolean;
begin
  Result := False;
end;

{ TcxGridCardViewSeparatorHitTest }

procedure TcxGridCardViewSeparatorHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridCardViewSeparatorHitTest then
  begin
    Index := TcxGridCardViewSeparatorHitTest(Source).Index;
    Separators := TcxGridCardViewSeparatorHitTest(Source).Separators;
  end;
end;

class function TcxGridCardViewSeparatorHitTest.GetHitTestCode: Integer;
begin
  Result := htSeparator;
end;

function TcxGridCardViewSeparatorHitTest.Cursor: TCursor;
begin
  Result := crSizeWE;
end;

function TcxGridCardViewSeparatorHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := TcxGridCardSizingObject;
end;

{ TcxGridCard }

function TcxGridCard.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCard.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridCardViewInfoCacheItem;
end;

procedure TcxGridCard.GetVisibleRows(ARows: TList);
var
  I: Integer;
  ARow: TcxGridCardViewRow;
begin
  ARows.Clear;
  for I := 0 to GridView.VisibleRowCount - 1 do
  begin
    ARow := GridView.VisibleRows[I];
    if ARow.IsVisibleInCard(Self) then ARows.Add(ARow);
  end;
end;

{ TcxGridCardViewViewData }

function TcxGridCardViewViewData.GetCard(Index: Integer): TcxGridCard;
begin
  Result := TcxGridCard(inherited Records[Index]);
end;

function TcxGridCardViewViewData.GetCardCount: Integer;
begin
  Result := RecordCount;
end;

function TcxGridCardViewViewData.GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridCard;
end;

{ TcxGridCardRowContainerZone }

constructor TcxGridCardRowContainerZone.Create(AGridRecord: TcxCustomGridRecord;
  AItemIndex: Integer; AInsertionPos: TcxGridCardRowInsertionPos);
begin
  inherited Create(AItemIndex);
  GridRecord := AGridRecord;
  InsertionPos := AInsertionPos;
end;

function TcxGridCardRowContainerZone.GetItem: TcxGridCardViewRow;
var
  AGridView: TcxGridCardView;
begin
  AGridView := TcxGridCardView(GridRecord.GridView);
  if (0 <= ItemIndex) and (ItemIndex < AGridView.VisibleRowCount) then
    Result := AGridView.VisibleRows[ItemIndex]
  else
    Result := nil;
end;

function TcxGridCardRowContainerZone.GetRecordViewInfo: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(GridRecord.ViewInfo);
end;

function TcxGridCardRowContainerZone.IsEqual(Value: TcxGridItemContainerZone): Boolean;
var
  AValue: TcxGridCardRowContainerZone;
begin
  AValue := Value as TcxGridCardRowContainerZone;
  Result := inherited IsEqual(Value) and (GridRecord = AValue.GridRecord) and
    (InsertionPos = AValue.InsertionPos);
end;

{ TcxGridCardRowMovingObject }

function TcxGridCardRowMovingObject.GetDestZone: TcxGridCardRowContainerZone;
begin
  Result := TcxGridCardRowContainerZone(inherited DestZone);
end;

function TcxGridCardRowMovingObject.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardRowMovingObject.GetRowLayout: TcxGridCardViewRowLayoutObject;
begin
  Result := GridView.RowLayoutController.LayoutObject;
end;

function TcxGridCardRowMovingObject.GetSourceItem: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(inherited SourceItem);
end;

function TcxGridCardRowMovingObject.GetViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited ViewInfo);
end;

function TcxGridCardRowMovingObject.GetVisibleRowLayout: TcxGridCardViewVisibleRowLayoutObject;
begin
  Result := GridView.RowLayoutController.VisibleLayoutObject;
end;

procedure TcxGridCardRowMovingObject.SetDestZone(Value: TcxGridCardRowContainerZone);
begin
  inherited DestZone := Value;
end;

procedure TcxGridCardRowMovingObject.SetSourceItem(Value: TcxGridCardViewRow);
begin
  inherited SourceItem := Value;
end;

procedure TcxGridCardRowMovingObject.RowExpandingHandler(Sender: TObject);
begin
  StopRowExpanding;
  ExpandingRow.Expanded := True;
  if ExpandingRow.Expanded then
  begin
    DestZone.GridRecord.MakeVisible;
    Controller.DesignerModified;
    AfterViewChange;
  end;
end;

function TcxGridCardRowMovingObject.AreArrowsVertical: Boolean;
begin
  Result := DestZone.RecordViewInfo.IsZoneVertical(DestZone);
end;

procedure TcxGridCardRowMovingObject.CalculateDestParams(AHitTest: TcxCustomGridHitTest;
  out AContainerKind: TcxGridItemContainerKind; out AZone: TcxGridItemContainerZone);
var
  ADestRow: TcxGridCardViewRow;
  ALayerIndex, ALayerCount, AFirstIndexInLayer, ALastIndexInLayer: Integer;
  ALastVisibleSubItem: TcxGridCardViewRow;
begin
  inherited;
  if AContainerKind = ckNone then
  begin
    AZone := ViewInfo.RecordsViewInfo.GetZone(AHitTest);
    if AZone <> nil then
    begin
      AContainerKind := ckRows;
      ADestRow := TcxGridCardRowContainerZone(AZone).Item;
      if (SourceItem.Kind = rkCategory) and
        (ADestRow <> nil) and (ADestRow.CategoryRow <> nil) then
      begin
        AZone.ItemIndex := ADestRow.CategoryRow.VisibleIndex;
        ALayerIndex := ADestRow.CategoryRow.Position.LayerVisibleIndex;
        ALayerCount := VisibleRowLayout.GetLayerCount(ADestRow.CategoryRow);
        if ALayerCount = 1 then
        begin
          AFirstIndexInLayer := ADestRow.CategoryRow.Position.VisibleIndexInLayer;
          ALastVisibleSubItem := VisibleRowLayout.GetLastVisibleSubItem(ADestRow.CategoryRow);
          ALastIndexInLayer := ALastVisibleSubItem.Position.VisibleIndexInLayer;
          if ADestRow.Position.VisibleIndexInLayer > (AFirstIndexInLayer + ALastIndexInLayer) div 2 then
          begin
            AZone.ItemIndex := ALastVisibleSubItem.VisibleIndex + 1;
            if (TcxGridCardRowContainerZone(AZone).Item = nil) or
              (TcxGridCardRowContainerZone(AZone).Item.Position.LayerVisibleIndex <> ALayerIndex) then
              TcxGridCardRowContainerZone(AZone).InsertionPos := ripPrevLayer
            else
              TcxGridCardRowContainerZone(AZone).InsertionPos := ripSameLayer;
          end
          else
            TcxGridCardRowContainerZone(AZone).InsertionPos := ripSameLayer;
        end
        else
        begin
          if ADestRow.Position.LayerVisibleIndex - ALayerIndex >= MulDiv(ALayerCount, 1, 2) then
            AZone.ItemIndex := VisibleRowLayout.LayerFirstRowIndex[ALayerIndex + ALayerCount];
          TcxGridCardRowContainerZone(AZone).InsertionPos := ripNewLayer;
        end;
      end;
    end;
  end;
end;

function TcxGridCardRowMovingObject.CanRemove: Boolean;
begin
  Result := SourceItem.VisibleForCustomization and
    (SourceItemContainerKind = ckRows) and SourceItem.CanHide and
    (GridView.Controller.Customization or GridView.OptionsCustomize.RowHiding);
end;

procedure TcxGridCardRowMovingObject.ChangeSourceItemPosition;

  function GetInsertionRowIndex: Integer;
  begin
    if DestZone.InsertionPos = ripPrevLayer then
      Result := DestZone.ItemIndex - 1
    else
      Result := DestZone.ItemIndex;
  end;

  function GetLayerIndex(AInsertionRow: TcxGridCardViewRow): Integer;
  begin
    if DestZone.InsertionPos = ripNewLayer then
      if SourceItem.Kind = rkCategory then
        if AInsertionRow = nil then
          Result := RowLayout.LayerCount
        else
          Result := AInsertionRow.Position.LayerIndex
      else
      begin
        if AInsertionRow = nil then
          Result := VisibleRowLayout.LayerCount
        else
          Result := AInsertionRow.Position.LayerVisibleIndex;
        if Result > 0 then
          Result := VisibleRowLayout.LayerFirstRow[Result - 1].Position.LayerIndex + 1;
      end
    else
      Result := AInsertionRow.Position.LayerIndex;
  end;

  function GetIndexInLayer(AInsertionRow: TcxGridCardViewRow): Integer;
  var
    ALayerIndex: Integer;
  begin
    case DestZone.InsertionPos of
      ripPrevLayer:
        if SourceItem.Kind = rkCategory then
          Result := RowLayout.LayerRowCount[SourceItem.Position.LayerIndex] - 1
        else
        begin
          Result := AInsertionRow.Position.IndexInLayer + 1;
          if SourceItem.Position.IndexInLayer < Result then
            Dec(Result);
        end;
      ripSameLayer:
        begin
          if SourceItem.Kind = rkCategory then
            Result := AInsertionRow.Position.IndexInLayer
          else
            if AInsertionRow.Position.VisibleIndexInLayer = 0 then
              Result := 0
            else
            begin
              ALayerIndex := SourceItem.Position.LayerVisibleIndex;
              Result := AInsertionRow.Position.VisibleIndexInLayer - 1;
              Result := VisibleRowLayout.LayerRows[ALayerIndex, Result].Position.IndexInLayer + 1;
            end;
          if SourceItem.Position.IndexInLayer < Result then Dec(Result);
        end;
    else
      Result := -1;
    end;
  end;

  function IsNewLayer: Boolean;
  begin
    Result := DestZone.InsertionPos = ripNewLayer;
  end;

var
  AInsertionRow: TcxGridCardViewRow;
begin
  if GetInsertionRowIndex = GridView.VisibleRowCount then
    AInsertionRow := nil
  else
    AInsertionRow := GridView.VisibleRows[GetInsertionRowIndex];
  RowLayout.SetLayerIndex(SourceItem, GetLayerIndex(AInsertionRow), IsNewLayer, True);
  if GetIndexInLayer(AInsertionRow) <> -1 then
    RowLayout.SetIndexInLayer(SourceItem, GetIndexInLayer(AInsertionRow), True);
end;

function TcxGridCardRowMovingObject.GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect;
begin
  Result := DestZone.RecordViewInfo.GetZoneBounds(DestZone);
end;

function TcxGridCardRowMovingObject.GetArrowsClientRect: TRect;
begin
  Result := DestZone.RecordViewInfo.ContentBounds;
end;

function TcxGridCardRowMovingObject.GetSourceItemViewInfo: TcxCustomGridCellViewInfo;
begin
  if SourceItemContainerKind = ckRows then
    Result := ViewInfo.RecordsViewInfo.GetRealItem(FSourceGridRecord).RowViewInfos[SourceItem.VisibleIndex]
  else
    Result := inherited GetSourceItemViewInfo;
end;

function TcxGridCardRowMovingObject.IsValidDestination: Boolean;
begin
  Result := DestItemContainerKind = ckRows;
  if Result then
  begin
    Result := not SourceItem.Visible;
    if not Result then
      if DestZone.InsertionPos = ripPrevLayer then
        Result := SourceItem.VisibleIndex <> DestZone.ItemIndex - 1
      else
      begin
        Result := (DestZone.ItemIndex < SourceItem.VisibleIndex) or
          (SourceItem.VisibleIndex + 1 < DestZone.ItemIndex);
        if not Result then
          case DestZone.InsertionPos of
            ripNewLayer:
              Result := not VisibleRowLayout.IsWholeLayer(SourceItem);
            ripSameLayer:
              Result := DestZone.Item.Position.LayerIndex <> SourceItem.Position.LayerIndex;
          end;
      end;
    // do not allow to insert category row just after its items
    if Result and (SourceItem.Kind = rkCategory) and SourceItem.Visible and SourceItem.Expanded and
      ((DestZone.InsertionPos <> ripNewLayer) or VisibleRowLayout.IsWholeLayer(SourceItem)) and
      ((DestZone.Item = nil) or
       (DestZone.Item.Kind = rkCategory) and (SourceItem.VisibleIndex < DestZone.ItemIndex)) and
      (GridView.VisibleRows[DestZone.ItemIndex - 1].CategoryRow = SourceItem) then
      Result := False;
  end;
end;

procedure TcxGridCardRowMovingObject.BeginDragAndDrop;
begin
  TcxGridCardViewController(Controller).IsReadyForImmediateEditing := False;
  inherited;
end;

procedure TcxGridCardRowMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AKeepRowExpanding: Boolean;
  AHitTest: TcxCustomGridHitTest;
begin
  inherited;
  AKeepRowExpanding := False;
  if DestItemContainerKind = ckRows then
  begin
    AHitTest := ViewInfo.GetHitTest(P);
    if AHitTest.HitTestCode = htCardRowExpandButton then
    begin
      StartRowExpanding(TcxGridCardViewRow(TcxGridCardRowExpandButtonHitTest(AHitTest).Item));
      AKeepRowExpanding := True;
    end
  end;
  if not AKeepRowExpanding then StopRowExpanding;
end;

procedure TcxGridCardRowMovingObject.EndDragAndDrop(Accepted: Boolean);

  function GetTopRowIndex: Integer;
  begin
    if Assigned(SourceGridRecord) and (SourceGridRecord.ViewInfo is TcxGridCardViewInfo) then
      Result := TcxGridCardViewInfo(SourceGridRecord.ViewInfo).TopRowIndex
    else
      Result := -1;
  end;

  procedure ValidateNewPosition(APrevTopRowIndex: Integer);
  begin
    GridView.BeginUpdate;
    try
      if APrevTopRowIndex >= 0 then
        TcxGridCardViewInfo(SourceGridRecord.ViewInfo).TopRowIndex := APrevTopRowIndex;
      SourceItem.MakeVisible;
    finally
      GridView.EndUpdate;
    end;
  end;

var
  ARowPositionChanged: Boolean;
  APrevTopRowIndex: Integer;
begin
  StopRowExpanding;
  inherited;
  if Accepted then
  begin
    APrevTopRowIndex := GetTopRowIndex;
    ARowPositionChanged := False;
    GridView.BeginUpdate;
    try
      if DestItemContainerKind = ckRows then
        if IsValidDestination then
        begin
          ChangeSourceItemPosition;
          SourceItem.Visible := True;
          ARowPositionChanged := True;
        end
        else
      else
        if CanRemove then
        begin
          SourceItem.Visible := False;
          ARowPositionChanged := True;
        end;
    finally
      GridView.EndUpdate;
    end;
    if ARowPositionChanged then
    begin
      if SourceItem.Visible then
        ValidateNewPosition(APrevTopRowIndex);
      GridView.DoRowPositionChanged(SourceItem);
    end;
  end;
end;

procedure TcxGridCardRowMovingObject.StartRowExpanding(ARow: TcxGridCardViewRow);
begin
  if (FRowExpandingTimer <> nil) and (FExpandingRow = ARow) then Exit;
  StopRowExpanding;
  FExpandingRow := ARow;
  FRowExpandingTimer := TcxTimer.Create(nil);
  FRowExpandingTimer.Interval := RowExpandingPauseTime;
  FRowExpandingTimer.OnTimer := RowExpandingHandler;
end;

procedure TcxGridCardRowMovingObject.StopRowExpanding;
begin
  FreeAndNil(FRowExpandingTimer);
end;

procedure TcxGridCardRowMovingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  with AParams as TcxGridCardRowCellHitTest do
  begin
    SourceGridRecord := GridRecord;
    SourceItem := TcxGridCardViewRow(Item);
  end;
  if AParams is TcxGridCardRowCaptionHitTest then
    SourceItemContainerKind := TcxGridCardRowCaptionHitTest(AParams).RowContainerKind
  else
    SourceItemContainerKind := ckRows;
end;

{ TcxGridCardSizingObject }

constructor TcxGridCardSizingObject.Create(AControl: TcxControl);
begin
  inherited;
  FSeparators := TList.Create;
end;

destructor TcxGridCardSizingObject.Destroy;
begin
  ClearSeparators;
  FSeparators.Free;
  inherited;
end;

function TcxGridCardSizingObject.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardSizingObject.GetSeparator(Index: Integer): TRect;
begin
  Result := PRect(FSeparators[Index])^;
end;

function TcxGridCardSizingObject.GetSeparatorCount: Integer;
begin
  Result := FSeparators.Count;
end;

function TcxGridCardSizingObject.GetViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardSizingObject.SetDestPointX(Value: Integer);
var
  APrevWidth: Integer;
begin
  if FDestPointX <> Value then
  begin
    APrevWidth := CurrentWidth;
    FDestPointX := Value;
    if CurrentWidth <> APrevWidth then
      Dirty := True;
  end;
end;

procedure TcxGridCardSizingObject.DirtyChanged;
begin
  if not Dirty then
  begin
    ClearSeparators;
    CalculateSeparators;
  end;
  DrawSeparators;
end;

function TcxGridCardSizingObject.GetCurrentWidth: Integer;
begin
  Result := OriginalWidth + DeltaWidth;
  if Result < cxGridCardMinWidth then Result := cxGridCardMinWidth;
end;

function TcxGridCardSizingObject.GetDeltaWidth: Integer;
var
  ADelta: Integer;
begin
  ADelta := DestPointX - SourcePoint.X;
  if ViewInfo.UseRightToLeftAlignment then
    ADelta := -ADelta;
  Result := ADelta div (FCardColumnIndex + 1);
end;

function TcxGridCardSizingObject.GetDragAndDropCursor(Accepted: Boolean): TCursor;
begin
  if Accepted then
    Result := crSizeWE
  else
    Result := inherited GetDragAndDropCursor(Accepted);
end;

function TcxGridCardSizingObject.GetImmediateStart: Boolean;
begin
  Result := True;
end;

function TcxGridCardSizingObject.GetOriginalWidth: Integer;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.RecordWidth;
end;

procedure TcxGridCardSizingObject.BeginDragAndDrop;
begin
  FDestPointX := CurMousePos.X;
  inherited;
end;

procedure TcxGridCardSizingObject.DragAndDrop(const P: TPoint;
  var Accepted: Boolean);
begin
  DestPointX := P.X;
  Accepted := True;
  inherited;
end;

procedure TcxGridCardSizingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  if Accepted then
    GridView.OptionsView.CardWidth := CurrentWidth;
end;

procedure TcxGridCardSizingObject.AddSeparator(const R: TRect);
var
  ANewSeparator: PRect;
begin
  New(ANewSeparator);
  ANewSeparator^ := R;
  FSeparators.Add(ANewSeparator);
end;

procedure TcxGridCardSizingObject.CalculateSeparators;
var
  ACardIndent, ACardWidth, APrevCardRight: Integer;
  R, R1, AClientBounds: TRect;
begin
  ACardIndent := GridView.OptionsView.CardIndent;
  ACardWidth := CurrentWidth;
  AClientBounds := ViewInfo.ClientBounds;
  R.Right := AClientBounds.Left;
  R.Top := AClientBounds.Top + ACardIndent;
  R.Bottom := AClientBounds.Bottom - ACardIndent;
  repeat
    R.Left := R.Right + ACardIndent + ACardWidth;
    APrevCardRight := R.Left;
    if GridView.LayoutDirection = ldHorizontal then
      Inc(R.Left, ACardIndent)
    else
      Inc(R.Left, (ACardIndent - ViewInfo.SeparatorsViewInfo.Width) div 2);
    if R.Left >= ViewInfo.ClientBounds.Right then Exit;
    R.Right := R.Left + ViewInfo.SeparatorsViewInfo.Width;
    if ViewInfo.UseRightToLeftAlignment then
      R1 := TdxRightToLeftLayoutConverter.ConvertRect(R, AClientBounds)
    else
      R1 := R;
    AddSeparator(R1);
    if GridView.LayoutDirection = ldVertical then
      R.Right := APrevCardRight;
  until False;
end;

procedure TcxGridCardSizingObject.ClearSeparators;
var
  I: Integer;
begin
  for I := 0 to SeparatorCount - 1 do
    Dispose(PRect(FSeparators[I]));
  FSeparators.Clear;
end;

procedure TcxGridCardSizingObject.DrawSeparators;
var
  I: Integer;
begin
  for I := 0 to SeparatorCount - 1 do
    Canvas.InvertRect(Separators[I]);
end;

procedure TcxGridCardSizingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  FCardColumnIndex := (AParams as TcxGridCardViewSeparatorHitTest).Index;
end;

{ TcxGridCardRowsListBox }

function TcxGridCardRowsListBox.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardRowsListBox.CalculateItemHeight: Integer;
begin
  Result := TcxGridCardRowCaptionViewInfo.GetCellHeight(cxTextHeight(Canvas.Handle), LookAndFeelPainter, ScaleFactor);
end;

procedure TcxGridCardRowsListBox.DoRefreshItems;
begin
  inherited;
  RefreshItemsAsTableItems;
end;

function TcxGridCardRowsListBox.GetDragAndDropParams: TcxCustomGridHitTest;
begin
  Result := TcxGridCardRowCaptionHitTest.Instance(Point(-1, -1));
  with TcxGridCardRowCaptionHitTest(Result) do
  begin
    GridView := Self.GridView;
    GridRecord := nil;
    Item := TcxCustomGridTableItem(DragAndDropItem);
    RowContainerKind := ckCustomizationForm;
  end;
end;

procedure TcxGridCardRowsListBox.PaintItem(ACanvas: TcxCanvas; R: TRect;
  AIndex: Integer; AFocused: Boolean);
var
  AHorzAlignment, ADrawTextFlags: Integer;
begin
  ACanvas.FillRect(R);
  Inc(R.Left, ScaleFactor.Apply(cxGridCellTextOffset));
  ADrawTextFlags := cxAlignVCenter or cxSingleLine or
    Ord(GridView.OptionsView.RowCaptionEndEllipsis) * cxShowEndEllipsis;
  AHorzAlignment := cxAlignLeft;
  if UseRightToLeftAlignment then
    AHorzAlignment := TdxRightToLeftLayoutConverter.ConvertcxDrawTextAlignment(AHorzAlignment);
  ADrawTextFlags := ADrawTextFlags or AHorzAlignment;
  if UseRightToLeftReading then
     ADrawTextFlags := ADrawTextFlags or cxRtlReading;
  ACanvas.DrawText(Items[AIndex], R, ADrawTextFlags);
end;

{ TcxGridCardViewCustomizationForm }

function TcxGridCardViewCustomizationForm.GetItemsListBoxClass: TcxCustomGridTableItemsListBoxClass;
begin
  Result := TcxGridCardRowsListBox;
end;

function TcxGridCardViewCustomizationForm.GetItemsPageCaption: string;
begin
  Result := cxGetResourceString(@scxGridCustomizationFormRowsPageCaption);
end;

{ TcxGridCardViewController }

function TcxGridCardViewController.GetCustomizationForm: TcxGridCardViewCustomizationForm;
begin
  Result := TcxGridCardViewCustomizationForm(inherited CustomizationForm);
end;

function TcxGridCardViewController.GetFocusedCard: TcxGridCard;
begin
  Result := TcxGridCard(FocusedRecord);
end;

function TcxGridCardViewController.GetFocusedCardViewInfo: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited FocusedRecordViewInfo);
end;

function TcxGridCardViewController.GetFocusedRow: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(inherited FocusedItem);
end;

function TcxGridCardViewController.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewController.GetScrollCardViewInfo: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited ScrollRecordViewInfo);
end;

function TcxGridCardViewController.GetTopCardIndex: Integer;
begin
  Result := TopRecordIndex;
end;

function TcxGridCardViewController.GetViewData: TcxGridCardViewViewData;
begin
  Result := TcxGridCardViewViewData(inherited ViewData);
end;

function TcxGridCardViewController.GetViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardViewController.SetFocusedCard(Value: TcxGridCard);
begin
  FocusedRecord := Value;
end;

procedure TcxGridCardViewController.SetFocusedRow(Value: TcxGridCardViewRow);
begin
  inherited FocusedItem := Value;
end;

procedure TcxGridCardViewController.SetScrollCardViewInfo(Value: TcxGridCardViewInfo);
begin
  inherited ScrollRecordViewInfo := Value;
end;

procedure TcxGridCardViewController.SetTopCardIndex(Value: Integer);
begin
  TopRecordIndex := Value;
end;

function TcxGridCardViewController.GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass;
begin
  case GridView.LayoutDirection of
    ldHorizontal:
      Result := TcxGridCustomLayoutViewControllerHorizontalHelper;
    ldVertical:
      Result := TcxGridCustomLayoutViewControllerVerticalHelper;
  else
    Result := nil;
  end;
end;

function TcxGridCardViewController.CanFocusNextItem(AFocusedItemIndex, ANextItemIndex: Integer;
  AGoForward, AGoOnCycle, AGoToNextRecordOnCycle: Boolean): Boolean;
begin
  Result := inherited CanFocusNextItem(AFocusedItemIndex, ANextItemIndex, AGoForward, AGoOnCycle, AGoToNextRecordOnCycle) and
    (not AGoToNextRecordOnCycle or
      (not AGoForward and (not CanScrollFocusedCard(False) or (ANextItemIndex < AFocusedItemIndex)) or
        AGoForward and (not CanScrollFocusedCard(True) or (ANextItemIndex > AFocusedItemIndex))));
end;

type
  TCanFocusRowData = class
    Card: TcxGridCard;
    Rows: TList;
  end;

function CanFocusRow(AOwner: TcxCustomGridTableView; AItemIndex: Integer; AData: TObject): Boolean;
begin
  Result := TcxGridCardViewRow(TCanFocusRowData(AData).Rows[AItemIndex]).CanFocus(TCanFocusRowData(AData).Card);
end;

function TcxGridCardViewController.FindNextRow(ACard: TcxGridCard; ARows: TList;
  ARow: TcxGridCardViewRow; AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;
var
  AData: TCanFocusRowData;
  ARowIndex: Integer;
begin
  AData := TCanFocusRowData.Create;
  try
    AData.Card := ACard;
    AData.Rows := ARows;
    if FindNextCustomItem(ARows.IndexOf(ARow), ARows.Count,
      AGoForward, AGoOnCycle, @CanFocusRow, AData, ARowIndex, ACycleChanged) then
      Result := TcxGridCardViewRow(ARows[ARowIndex])
    else
      Result := nil;
  finally
    AData.Free;
  end;
end;

function TcxGridCardViewController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    Result := AHitTest.HitTestCode in [htExpandButton, htCardRowExpandButton, htRowCaption, htCell];
end;

function TcxGridCardViewController.IsKeyForMultiSelect(AKey: Word;
  AShift: TShiftState; AFocusedRecordChanged: Boolean): Boolean;
begin
  Result := inherited IsKeyForMultiSelect(AKey, AShift, AFocusedRecordChanged) or
    (AKey = VK_LEFT) or (AKey = VK_RIGHT) or
    ((AKey = VK_UP) or (AKey = VK_DOWN) or (AKey = VK_HOME) or (AKey = VK_END)) and
      (not GridView.OptionsSelection.CellSelect or (FocusedItem = nil) or
        AFocusedRecordChanged);
end;

procedure TcxGridCardViewController.RowExpandedChanged(ARow: TcxGridCardViewRow);
begin
end;

procedure TcxGridCardViewController.RowExpandedChanging(ARow: TcxGridCardViewRow;
  AValue: Boolean);
begin
  if IsDragging then
    DragAndDropObject.BeforeViewChange;
end;

procedure TcxGridCardViewController.ScrollData(ADirection: TcxDirection);
begin
  if Site.DragAndDropState <> ddsNone then
  with ScrollCardViewInfo do
    case ADirection of
      dirUp:
        TopRowIndex := TopRowIndex - 1;
      dirDown:
        TopRowIndex := TopRowIndex + 1;
    end
  else
    inherited;
end;

function TcxGridCardViewController.CanScrollData(ADirection: TcxDirection): Boolean;
begin
  with ScrollCardViewInfo do
    case ADirection of
      dirUp:
        Result := NeedsScrollingUp;
      dirDown:
        Result := NeedsScrollingDown;
    else
      Result := inherited CanScrollData(ADirection);
    end;
end;

function TcxGridCardViewController.GetScrollDataTimeInterval(ADirection: TcxDirection): Integer;
begin
  if ADirection in [dirUp, dirDown] then
    Result := CardScrollingInterval
  else
    Result := inherited GetScrollDataTimeInterval(ADirection);
end;

function TcxGridCardViewController.GetCustomizationFormClass: TcxCustomGridCustomizationFormClass;
begin
  Result := TcxGridCardViewCustomizationForm;
end;

function TcxGridCardViewController.GetRowDragAndDropObjectClass: TcxGridCardRowMovingObjectClass;
begin
  Result := TcxGridCardRowMovingObject;
end;

procedure TcxGridCardViewController.CheckScrolling(const P: TPoint);
var
  AHitTest: TcxCustomGridHitTest;
begin
  AHitTest := ViewInfo.GetHitTest(P);
  case AHitTest.HitTestCode of
    htCardScrollButtonUp, htCardScrollButtonDown:
      begin
        ScrollCardViewInfo :=
          TcxCustomGridCardScrollButtonViewInfo(AHitTest.ViewInfo).CardViewInfo;
        if AHitTest.HitTestCode = htCardScrollButtonUp then
          ScrollDirection := dirUp
        else
          ScrollDirection := dirDown;
      end
  else
    ScrollDirection := dirNone;
  end;
end;

procedure TcxGridCardViewController.MakeItemVisible(AItem: TcxCustomGridTableItem);
var
  ARow: TcxGridCardViewRow;
begin
  ARow := TcxGridCardViewRow(AItem);
  if (ARow <> nil) and (ARow.CategoryRow <> nil) then
    ARow.CategoryRow.Expanded := True;
  inherited;
  if (ARow <> nil) and (FocusedCardViewInfo <> nil) and
    not TcxCustomGrid(Control).UpdateLocked then
    FocusedCardViewInfo.MakeRowVisible(ARow);
end;

function TcxGridCardViewController.CanScrollFocusedCard(ADown: Boolean): Boolean;
begin
  Result := (FocusedCardViewInfo <> nil) and
    (not ADown and FocusedCardViewInfo.NeedsScrollingUp or
     ADown and FocusedCardViewInfo.NeedsScrollingDown);
end;

function TcxGridCardViewController.ScrollFocusedCard(ADown: Boolean): Boolean;
var
  APrevTopRowIndex: Integer;
begin
  Result := (FocusedCardViewInfo <> nil) and CanScrollFocusedCard(ADown);
  if Result then
    with FocusedCardViewInfo do
    begin
      APrevTopRowIndex := TopRowIndex;
      TopRowIndex := TopRowIndex + 2 * Ord(ADown) - 1;
      Result := TopRowIndex <> APrevTopRowIndex;
    end;
end;

procedure TcxGridCardViewController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if (FocusedRecordIndex <> -1) and (FocusedRow <> nil) and FocusedRow.CanExpand then
    case Key of
      VK_ADD:
        if not FocusedRow.Expanded then
        begin
          EatKeyPress := True;
          FocusedRow.Expanded := True;
          MakeFocusedRecordVisible;
          Key := 0;
        end;
      VK_SUBTRACT:
        if FocusedRow.Expanded then
        begin
          EatKeyPress := True;
          FocusedRow.Expanded := False;
          MakeFocusedRecordVisible;
          Key := 0;
        end;
    end;
  inherited;
end;

function TcxGridCardViewController.FocusNextItemHorizontally(AGoForward, AGoOnCycle: Boolean): Boolean;
var
  ARow: TcxGridCardViewRow;
  ACycleChanged: Boolean;
begin
  Result := FocusedRecordHasCells(True);
  if Result then
  begin
    ARow := GridView.RowLayoutController.FindNextRowHorizontally(FocusedCard,
      FocusedRow, AGoForward, AGoOnCycle, ACycleChanged);
    Result := ARow <> nil;
    if Result then
      ARow.Focused := True;
  end;
end;

function TcxGridCardViewController.FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean;
var
  ARow: TcxGridCardViewRow;
  ACycleChanged: Boolean;
begin
  Result := FocusedRecordHasCells(True);
  if Result then
  begin
    ARow := GridView.RowLayoutController.FindNextRowVertically(FocusedCard,
      FocusedRow, AGoForward, AGoOnCycle, ACycleChanged);
    Result := ARow <> nil;
    if Result then
      ARow.Focused := True;
  end;
end;

{ TcxCustomGridCardRowLayoutObject }

constructor TcxCustomGridCardRowLayoutObject.Create;
begin
  inherited;
  FLayerRowCounts := TList.Create;
end;

destructor TcxCustomGridCardRowLayoutObject.Destroy;
begin
  FLayerRowCounts.Free;
  inherited;
end;

function TcxCustomGridCardRowLayoutObject.GetLayerCount: Integer;
begin
  Result := FLayerRowCounts.Count;
end;

function TcxCustomGridCardRowLayoutObject.GetLayerFirstRow(ALayerIndex: Integer): TcxGridCardViewRow;
begin
  Result := Rows[LayerFirstRowIndex[ALayerIndex]];
end;

function TcxCustomGridCardRowLayoutObject.GetLayerFirstRowIndex(ALayerIndex: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ALayerIndex - 1 do
    Inc(Result, LayerRowCount[I]);
end;

function TcxCustomGridCardRowLayoutObject.GetLayerRow(ALayerIndex, AIndex: Integer): TcxGridCardViewRow;
begin
  Result := Rows[LayerFirstRowIndex[ALayerIndex] + AIndex];
end;

function TcxCustomGridCardRowLayoutObject.GetLayerRowCount(ALayerIndex: Integer): Integer;
begin
  Result := Integer(FLayerRowCounts[ALayerIndex]);
end;

procedure TcxCustomGridCardRowLayoutObject.SetLayerRowCount(ALayerIndex, Value: Integer);
begin
  FLayerRowCounts[ALayerIndex] := TObject(Value);
end;

function TcxCustomGridCardRowLayoutObject.GetCoordinates(const APosition: TcxGridCardRowPosition): TcxGridCardRowCoordinates;
begin
  case Layout of
    rlHorizontal:
      begin
        Result.ColIndex := APosition.IndexInLayer;
        Result.RowIndex := APosition.LayerIndex;
      end;
    rlVertical:
      begin
        Result.ColIndex := APosition.LayerIndex;
        Result.RowIndex := APosition.IndexInLayer;
      end
  else
    Result.ColIndex := -1;
    Result.RowIndex := -1;
  end;
end;

function TcxCustomGridCardRowLayoutObject.GetPosition(const ACoordinates: TcxGridCardRowCoordinates): TcxGridCardRowPosition;
begin
  case Layout of
    rlHorizontal:
      begin
        Result.LayerIndex := ACoordinates.RowIndex;
        Result.IndexInLayer := ACoordinates.ColIndex;
      end;
    rlVertical:
      begin
        Result.LayerIndex := ACoordinates.ColIndex;
        Result.IndexInLayer := ACoordinates.RowIndex;
      end
  else
    Result.LayerIndex := -1;
    Result.IndexInLayer := -1;
  end;
end;

function TcxCustomGridCardRowLayoutObject.GetLayerIndex(ARowIndex: Integer): Integer;
begin
  Result := GetPosition(ARowIndex).LayerIndex;
end;

function TcxCustomGridCardRowLayoutObject.GetLayerPosition(APosition: TcxPosition): TcxGridCardViewRowLayerPosition;
begin
  Result := rlpBeforeRow;
  case Layout of
    rlHorizontal:
      case APosition of
        posLeft:
          Result := rlpBeforeRow;
        posRight:
          Result := rlpAfterRow;
        posTop:
          Result := rlpBeforeLayer;
        posBottom:
          Result := rlpAfterLayer;
      end;
    rlVertical:
      case APosition of
        posLeft:
          Result := rlpBeforeLayer;
        posRight:
          Result := rlpAfterLayer;
        posTop:
          Result := rlpBeforeRow;
        posBottom:
          Result := rlpAfterRow;
      end;
  end;
end;

function TcxCustomGridCardRowLayoutObject.GetLength(ARow: TcxGridCardViewRow): Integer;
begin
  case Layout of
    rlHorizontal:
      Result := 1;
    rlVertical:
      Result := ARow.Position.LineCount;
  else
    Result := 0;
  end;
end;

function TcxCustomGridCardRowLayoutObject.GetOffsetInLayer(ALayerIndex, AIndexInLayer: Integer): Integer;
var
  I: Integer;
begin
  case Layout of
    rlHorizontal:
      Result := AIndexInLayer;
    rlVertical:
      begin
        Result := 0;
        for I := 0 to AIndexInLayer - 1 do
          Inc(Result, GetLength(LayerRows[ALayerIndex, I]));
      end;
  else
    Result := -1;
  end;
end;

function TcxCustomGridCardRowLayoutObject.GetPosition(ARowIndex: Integer): TcxGridCardRowPosition;
var
  I: Integer;
begin
  if ARowIndex <> -1 then
    for I := 0 to LayerCount - 1 do
    begin
      if ARowIndex < LayerRowCount[I] then
      begin
        Result.LayerIndex := I;
        Result.IndexInLayer := ARowIndex;
        Exit;
      end;
      Dec(ARowIndex, LayerRowCount[I]);
    end;
  Result.LayerIndex := -1;
  Result.IndexInLayer := -1;
end;

function TcxCustomGridCardRowLayoutObject.GetCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates;
begin
  Result := GetCoordinates(GetPosition(ARow));
end;

function TcxCustomGridCardRowLayoutObject.GetIndexInLayer(ARow: TcxGridCardViewRow): Integer;
begin
  Result := GetPosition(ARow).IndexInLayer;
end;

function TcxCustomGridCardRowLayoutObject.GetLayerIndex(ARow: TcxGridCardViewRow): Integer;
begin
  Result := GetLayerIndex(GetRowIndex(ARow));
end;

function TcxCustomGridCardRowLayoutObject.GetOffsetInLayer(ARow: TcxGridCardViewRow): Integer;
begin
  with GetPosition(ARow) do
    Result := GetOffsetInLayer(LayerIndex, IndexInLayer);
end;

function TcxCustomGridCardRowLayoutObject.GetPosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition;
begin
  Result := GetPosition(GetRowIndex(ARow));
end;

function TcxCustomGridCardRowLayoutObject.GetRowAtOffset(ALayerIndex, AOffset: Integer): TcxGridCardViewRow;
var
  I, ARowOffset: Integer;
begin
  Result := nil;
  for I := 0 to LayerRowCount[ALayerIndex] - 1 do
  begin
    Result := LayerRows[ALayerIndex, I];
    ARowOffset := GetOffsetInLayer(ALayerIndex, I);
    if ARowOffset + GetLength(Result) > AOffset then
      Exit;
  end;
end;

function TcxCustomGridCardRowLayoutObject.IsFirstInLayer(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := GetIndexInLayer(ARow) = 0;
end;

function TcxCustomGridCardRowLayoutObject.IsLastInLayer(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := GetIndexInLayer(ARow) = LayerRowCount[GetLayerIndex(ARow)] - 1;
end;

function TcxCustomGridCardRowLayoutObject.IsWholeLine(ARow: TcxGridCardViewRow): Boolean;
begin
  case Layout of
    rlHorizontal:
      Result := LayerRowCount[GetLayerIndex(ARow)] = 1;
    rlVertical:
      Result := True;
  else
    Result := True;
  end;
end;

{ TcxGridCardRowLayoutObject }

constructor TcxGridCardRowLayoutObject.Create;
begin
  inherited;
  FRows := TList.Create;
end;

destructor TcxGridCardRowLayoutObject.Destroy;
begin
  FRows.Free;
  inherited;
end;

function TcxGridCardRowLayoutObject.GetLayout: TcxGridCardViewRowLayout;
begin
  Result := FLayout;
end;

function TcxGridCardRowLayoutObject.GetRow(AIndex: Integer): TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(FRows[AIndex]);
end;

function TcxGridCardRowLayoutObject.GetRowCount: Integer;
begin
  Result := FRows.Count;
end;

function TcxGridCardRowLayoutObject.GetRowIndex(ARow: TcxGridCardViewRow): Integer;
begin
  Result := FRows.IndexOf(ARow);
end;

procedure TcxGridCardRowLayoutObject.Fill(ARows: TList);
var
  ALayerIndex, I: Integer;
  ARow: TcxGridCardViewRow;
begin
  dxCopyList(ARows, FRows);
  FLayerRowCounts.Clear;
  ALayerIndex := -1;
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    if ARow.Position.LayerIndex <> ALayerIndex then
    begin
      ALayerIndex := ARow.Position.LayerIndex;
      FLayerRowCounts.Add(nil);
    end;
    LayerRowCount[LayerCount - 1] := LayerRowCount[LayerCount - 1] + 1;
  end;
end;

{ TcxGridCardVisibleRowLayoutObject }

constructor TcxGridCardVisibleRowLayoutObject.Create(ACard: TcxGridCard);
begin
  inherited Create;
  FCard := ACard;
  Layout := ACard.GridView.RowLayout;
  FCard.GetVisibleRows(RowsList);
  Fill(RowsList);
end;

procedure TcxGridCardVisibleRowLayoutObject.GetInsertionParams(ARow: TcxGridCardViewRow;
  APosition: TcxPosition; out AInsertionIndex: Integer; out AInsertionPos: TcxGridCardRowInsertionPos);

  function GetNextLayerFirstRowVisibleIndex: Integer;
  begin
    if GetLayerIndex(ARow) = LayerCount - 1 then
      Result := Card.GridView.VisibleRowCount
    else
      Result := LayerFirstRow[GetLayerIndex(ARow) + 1].VisibleIndex;
  end;

var
  ALayerPosition: TcxGridCardViewRowLayerPosition;
begin
  ALayerPosition := GetLayerPosition(APosition);
  if ARow.GridView.UseRightToLeftAlignment and (APosition in [posLeft, posRight]) then
    case ALayerPosition of
      rlpBeforeRow:
        ALayerPosition := rlpAfterRow;
      rlpAfterRow:
        ALayerPosition := rlpBeforeRow;
      rlpBeforeLayer:
        ALayerPosition := rlpAfterLayer;
      rlpAfterLayer:
        ALayerPosition := rlpBeforeLayer;
    end;
  case ALayerPosition of
    rlpBeforeRow:
      begin
        AInsertionIndex := ARow.VisibleIndex;
        AInsertionPos := ripSameLayer;
      end;
    rlpAfterRow:
      if IsLastInLayer(ARow) then
      begin
        AInsertionIndex := GetNextLayerFirstRowVisibleIndex;
        AInsertionPos := ripPrevLayer;
      end
      else
      begin
        AInsertionIndex := LayerRows[GetLayerIndex(ARow), GetIndexInLayer(ARow) + 1].VisibleIndex;
        AInsertionPos := ripSameLayer;
      end;
    rlpBeforeLayer:
      begin
        AInsertionIndex := LayerFirstRow[GetLayerIndex(ARow)].VisibleIndex;
        AInsertionPos := ripNewLayer;
      end;
    rlpAfterLayer:
      begin
        AInsertionIndex := GetNextLayerFirstRowVisibleIndex;
        AInsertionPos := ripNewLayer;
      end;
  end;
end;

procedure TcxGridCardVisibleRowLayoutObject.GetLayerRows(ARowInLayer: TcxGridCardViewRow;
  ARows: TList);
var
  ALayerIndex, I: Integer;
begin
  ALayerIndex := GetLayerIndex(ARowInLayer);
  ARows.Clear;
  if ALayerIndex <> -1 then
    for I := 0 to LayerRowCount[ALayerIndex] - 1 do
      ARows.Add(LayerRows[ALayerIndex, I]);
end;

procedure TcxGridCardVisibleRowLayoutObject.GetRowsAtOffset(ARowAtOffset: TcxGridCardViewRow;
  ARows: TList);
var
  ARowOffset, I: Integer;
begin
  ARowOffset := GetOffsetInLayer(ARowAtOffset);
  ARows.Clear;
  for I := 0 to LayerCount - 1 do
    ARows.Add(GetRowAtOffset(I, ARowOffset));
end;

{ TcxGridCardViewRowLayoutObject }

constructor TcxGridCardViewRowLayoutObject.Create(AGridView: TcxGridCardView);
begin
  inherited Create;
  FGridView := AGridView;
end;

function TcxGridCardViewRowLayoutObject.GetBeginsLayer(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := IsFirstInLayer(ARow);
end;

procedure TcxGridCardViewRowLayoutObject.SetBeginsLayer(ARow: TcxGridCardViewRow;
  Value: Boolean);
var
  APosition: TcxGridCardRowPosition;
begin
  if BeginsLayer[ARow] <> Value then
  begin
    APosition := GetPosition(ARow);
    if Value then
      BreakLayer(APosition.LayerIndex, APosition.IndexInLayer)
    else
      MergeLayer(APosition.LayerIndex);
  end;
end;

function TcxGridCardViewRowLayoutObject.GetLayout: TcxGridCardViewRowLayout;
begin
  Result := FGridView.RowLayout;
end;

function TcxGridCardViewRowLayoutObject.GetRow(AIndex: Integer): TcxGridCardViewRow;
begin
  Result := GridView.Rows[AIndex];
end;

function TcxGridCardViewRowLayoutObject.GetRowCount: Integer;
begin
  Result := GridView.RowCount;
end;

function TcxGridCardViewRowLayoutObject.GetRowIndex(ARow: TcxGridCardViewRow): Integer;
begin
  Result := ARow.Index;
end;

procedure TcxGridCardViewRowLayoutObject.AddLayer;
begin
  LayerRowCounts.Add(TObject(0));
end;

procedure TcxGridCardViewRowLayoutObject.InsertLayer(AIndex: Integer);
begin
  LayerRowCounts.Insert(AIndex, TObject(0));
end;

procedure TcxGridCardViewRowLayoutObject.RemoveLayer(AIndex: Integer);
begin
  LayerRowCounts.Delete(AIndex);
end;

procedure TcxGridCardViewRowLayoutObject.AddRowToLayer(ALayerIndex: Integer);
begin
  LayerRowCount[ALayerIndex] := LayerRowCount[ALayerIndex] + 1;
end;

procedure TcxGridCardViewRowLayoutObject.RemoveRowFromLayer(ALayerIndex: Integer);
begin
  LayerRowCount[ALayerIndex] := LayerRowCount[ALayerIndex] - 1;
  if LayerRowCount[ALayerIndex] = 0 then
    RemoveLayer(ALayerIndex);
end;

procedure TcxGridCardViewRowLayoutObject.AddRow(ARow: TcxGridCardViewRow);
begin
  case Layout of
    rlHorizontal:
      SetLayerIndex(ARow, LayerCount);
    rlVertical:
      SetLayerIndex(ARow, Max(0, LayerCount - 1));
  end;
end;

procedure TcxGridCardViewRowLayoutObject.RemoveRow(ARowIndex: Integer);
begin
  RemoveRowFromLayer(GetLayerIndex(ARowIndex));
end;

procedure TcxGridCardViewRowLayoutObject.MoveRow(ARow: TcxGridCardViewRow;
  AOldIndex: Integer);
var
  AOldLayerIndex, ANewLayerIndex: Integer;
begin
  if ARow.LockPositionSync then Exit;
  AOldLayerIndex := GetLayerIndex(AOldIndex);
  ANewLayerIndex := GetLayerIndex(ARow);
  AddRowToLayer(ANewLayerIndex);
  RemoveRowFromLayer(AOldLayerIndex);
end;

procedure TcxGridCardViewRowLayoutObject.MoveRows(ARows: TList; AIndex: Integer);
var
  ARowLayout: TcxGridCardRowLayoutObject;
begin
  ARowLayout := TcxGridCardRowLayoutObject.Create;
  try
    ARowLayout.Fill(ARows);
    MoveRows(ARows, AIndex, ARowLayout);
  finally
    ARowLayout.Free;
  end;
end;

procedure TcxGridCardViewRowLayoutObject.MoveRows(ARows: TList; AIndex: Integer;
  ARowLayout: TcxGridCardRowLayoutObject);
var
  I, ALayerIndex: Integer;
  ARow, APrevRow: TcxGridCardViewRow;
  ANewLayer: Boolean;
begin
  GridView.BeginUpdate;
  try
    APrevRow := nil;
    for I := 0 to ARows.Count - 1 do
    begin
      ARow := TcxGridCardViewRow(ARows[I]);
      ARow.Index := AIndex + I;
      if I <> 0 then
      begin
        ALayerIndex := GetLayerIndex(TcxGridCardViewRow(ARows[0])) +
          ARowLayout.GetLayerIndex(ARow);
        ANewLayer := ARowLayout.IsFirstInLayer(ARow);
        if ANewLayer and not IsLastInLayer(APrevRow) then
          BreakLayer(ALayerIndex - 1, GetIndexInLayer(ARow) + 1);
        SetLayerIndex(ARow, ALayerIndex, ANewLayer);
      end;
      APrevRow := ARow;
    end;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridCardViewRowLayoutObject.LayerIndexChanged;
begin
  if Assigned(FOnLayerIndexChanged) then FOnLayerIndexChanged(Self);
end;

procedure TcxGridCardViewRowLayoutObject.BreakLayer(ALayerIndex, AIndexInLayer: Integer);
{var
  I: Integer;}
begin
  if AIndexInLayer = 0 then Exit;
  InsertLayer(ALayerIndex + 1);
  LayerRowCount[ALayerIndex + 1] := LayerRowCount[ALayerIndex] - AIndexInLayer;
  LayerRowCount[ALayerIndex] := AIndexInLayer;
  LayerIndexChanged;
  GridView.Changed(vcSize);
{
  GridView.BeginUpdate;
  try
    for I := AIndexInLayer to LayerRowCount[ALayerIndex] - 1 do
      SetLayerIndex(LayerRows[ALayerIndex, AIndexInLayer], ALayerIndex + 1,
        I = AIndexInLayer);
  finally
    GridView.EndUpdate;
  end;}
end;

function TcxGridCardViewRowLayoutObject.IsSimpleLayout: Boolean;
var
  I: Integer;
begin
  case Layout of
    rlHorizontal:
      begin
        Result := True;
        for I := 0 to LayerCount - 1 do
        begin
          Result := LayerRowCount[I] = 1;
          if not Result then Break;
        end;
      end;
    rlVertical:
      Result := LayerCount <= 1;
  else
    Result := True;
  end;
end;

procedure TcxGridCardViewRowLayoutObject.MergeLayer(ALayerIndex: Integer);
begin
  if ALayerIndex = 0 then Exit;
  LayerRowCount[ALayerIndex - 1] := LayerRowCount[ALayerIndex - 1] + LayerRowCount[ALayerIndex];
  RemoveLayer(ALayerIndex);
  LayerIndexChanged;
  GridView.Changed(vcSize);
end;

procedure TcxGridCardViewRowLayoutObject.SetCoordinates(ARow: TcxGridCardViewRow;
  const ACoordinates: TcxGridCardRowCoordinates);
begin
  SetPosition(ARow, GetPosition(ACoordinates));
end;

procedure TcxGridCardViewRowLayoutObject.SetIndexInLayer(ARow: TcxGridCardViewRow;
  AValue: Integer; AMoveSubItems: Boolean = False);
var
  ALayerIndex: Integer;
begin
  ALayerIndex := GetLayerIndex(ARow);
  AValue := Max(AValue, 0);
  AValue := Min(AValue, LayerRowCount[ALayerIndex] - 1);
  ARow.MoveTo(LayerFirstRowIndex[ALayerIndex] + AValue, AMoveSubItems);
end;

procedure TcxGridCardViewRowLayoutObject.SetLayerIndex(ARow: TcxGridCardViewRow;
  AValue: Integer; ANewLayer: Boolean = False; AMoveSubItems: Boolean = False);

  function GetRowNewIndex: Integer;
  begin
    Result := LayerFirstRowIndex[AValue] + LayerRowCount[AValue];
    // do not allow category row to break another category row,
    // but layer index cannot be changed
    if AMoveSubItems and (ARow.Kind = rkCategory) and
      (Result < RowCount) and (Rows[Result].CategoryRow <> nil) then
      for Result := Result - 1 downto 0 do
        if (Rows[Result].Kind = rkCategory) or IsFirstInLayer(Rows[Result]) then
          Break;
    if ARow.Index < Result then Dec(Result);
  end;

var
  ALayerIndex, ARowNewIndex: Integer;
  ARows: TList;
  ARowLayout: TcxGridCardRowLayoutObject;
begin
  AValue := Max(AValue, 0);
  AValue := Min(AValue, LayerCount);
  ALayerIndex := GetLayerIndex(ARow);
  if (ALayerIndex <> AValue) or ANewLayer and (LayerRowCount[ALayerIndex] > 1) then
  begin
    GridView.BeginUpdate;
    ARow.LockPositionSync := True;
    ARows := TList.Create;
    ARowLayout := TcxGridCardRowLayoutObject.Create;
    try
      if ARow.GridView <> nil then  // not from AddItem
      begin
        ARow.GetItems(ARows, AMoveSubItems);
        ARowLayout.Fill(ARows);
      end;
      if AValue = LayerCount then
        AddLayer
      else
        if ANewLayer then
        begin
          InsertLayer(AValue);
          if (ALayerIndex <> -1) and (AValue <= ALayerIndex) then
            Inc(ALayerIndex);
        end;
      ARowNewIndex := GetRowNewIndex;
      AddRowToLayer(AValue);
      if ALayerIndex <> -1 then
        RemoveRowFromLayer(ALayerIndex);
      if ARows.Count <> 0 then
        MoveRows(ARows, ARowNewIndex, ARowLayout);
      LayerIndexChanged;
      if ARow.GridView <> nil then  // not from AddItem
        GridView.Changed(vcSize);
    finally
      ARowLayout.Free;
      ARows.Free;
      ARow.LockPositionSync := False;
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridCardViewRowLayoutObject.SetPosition(ARow: TcxGridCardViewRow;
  const APosition: TcxGridCardRowPosition);
begin
  if APosition.LayerIndex <> -1 then
    SetLayerIndex(ARow, APosition.LayerIndex);
  if APosition.IndexInLayer <> -1 then
    SetIndexInLayer(ARow, APosition.IndexInLayer);
end;

{ TcxGridCardViewVisibleRowLayoutObject }

constructor TcxGridCardViewVisibleRowLayoutObject.Create(AGridView: TcxGridCardView);
begin
  inherited Create;
  FGridView := AGridView;
end;

function TcxGridCardViewVisibleRowLayoutObject.GetLayout: TcxGridCardViewRowLayout;
begin
  Result := FGridView.RowLayout;
end;

function TcxGridCardViewVisibleRowLayoutObject.GetLastVisibleSubItem(ARow: TcxGridCardViewRow): TcxGridCardViewRow;
var
  ARows: TList;
  I: Integer;
  ACardViewRow: TcxGridCardViewRow;
begin
  Result := nil;
  ARows := TList.Create;
  try
    ARow.GetItems(ARows, True);
    for I := ARows.Count - 1 downto 0 do
    begin
      ACardViewRow := TcxGridCardViewRow(ARows[I]);
      if GetRowIndex(ACardViewRow) <> -1 then
      begin
        Result := ACardViewRow;
        Break;
      end;
    end;
  finally
    ARows.Free;
  end;
end;

function TcxGridCardViewVisibleRowLayoutObject.GetLayerCount(ARow: TcxGridCardViewRow): Integer;
var
  ALastVisibleSubItem: TcxGridCardViewRow;
begin
  ALastVisibleSubItem := GetLastVisibleSubItem(ARow);
  if ALastVisibleSubItem = nil then
    Result := 0
  else
    Result := GetLayerIndex(ALastVisibleSubItem) - GetLayerIndex(ARow) + 1;
end;

function TcxGridCardViewVisibleRowLayoutObject.IsWholeLayer(ARow: TcxGridCardViewRow): Boolean;
var
  ALastVisibleSubItem: TcxGridCardViewRow;
begin
  Result := IsFirstInLayer(ARow);
  if Result then
  begin
    ALastVisibleSubItem := GetLastVisibleSubItem(ARow);
    Result := (GetLayerIndex(ALastVisibleSubItem) > GetLayerIndex(ARow)) or
      IsLastInLayer(ALastVisibleSubItem);
  end;
end;

{ TcxGridCardViewRowLayoutController }

constructor TcxGridCardViewRowLayoutController.Create(AGridView: TcxGridCardView);
begin
  inherited Create;
  FGridView := AGridView;
  FLayoutObject := TcxGridCardViewRowLayoutObject.Create(FGridView);
  FLayoutObject.OnLayerIndexChanged := LayerIndexChanged;
  FVisibleLayoutObject := TcxGridCardViewVisibleRowLayoutObject.Create(FGridView);
end;

destructor TcxGridCardViewRowLayoutController.Destroy;
begin
  FVisibleLayoutObject.Free;
  FLayoutObject.Free;
  inherited;
end;

function TcxGridCardViewRowLayoutController.GetBeginsLayer(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := LayoutObject.BeginsLayer[ARow];
end;

function TcxGridCardViewRowLayoutController.GetLayout: TcxGridCardViewRowLayout;
begin
  Result := GridView.RowLayout;
end;

procedure TcxGridCardViewRowLayoutController.SetBeginsLayer(ARow: TcxGridCardViewRow;
  Value: Boolean);
begin
  LayoutObject.BeginsLayer[ARow] := Value;
end;

procedure TcxGridCardViewRowLayoutController.LayerIndexChanged(Sender: TObject);
begin
  RefreshVisibleLayoutObject;
end;

procedure TcxGridCardViewRowLayoutController.RefreshVisibleLayoutObject;
begin
  VisibleLayoutObject.Fill(GridView.VisibleItemsList);
end;

procedure TcxGridCardViewRowLayoutController.RowIndexChanged(ARow: TcxGridCardViewRow;
  AOldIndex: Integer);
begin
  if AOldIndex = -1 then
    LayoutObject.AddRow(ARow)
  else
    if ARow.Index = -1 then
      LayoutObject.RemoveRow(AOldIndex)
    else
      LayoutObject.MoveRow(ARow, AOldIndex);
end;

procedure TcxGridCardViewRowLayoutController.VisibleRowsListChanged;
begin
  RefreshVisibleLayoutObject;
end;

function TcxGridCardViewRowLayoutController.CreateCardRowLayout(ACardViewInfo: TcxGridCardViewInfo): TcxGridCardRowLayout;
begin
  Result := GetCardRowLayoutClass.Create(ACardViewInfo, IsSimpleLayout);
end;

function TcxGridCardViewRowLayoutController.GetCardRowLayoutClass: TcxGridCardRowLayoutClass;
begin
  case Layout of
    rlHorizontal:
      Result := TcxGridCardRowHorizontalLayout;
    rlVertical:
      Result := TcxGridCardRowVerticalLayout;
  else
    Result := nil;
  end;
end;

function TcxGridCardViewRowLayoutController.IsHorizontalLayout: Boolean;
begin
  Result := Layout = rlHorizontal;
end;

function TcxGridCardViewRowLayoutController.IsSimpleLayout: Boolean;
begin
  Result := LayoutObject.IsSimpleLayout;
end;

function TcxGridCardViewRowLayoutController.IsWholeLine(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := VisibleLayoutObject.IsWholeLine(ARow);
end;

function TcxGridCardViewRowLayoutController.GetCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates;
begin
  Result := LayoutObject.GetCoordinates(ARow);
end;

procedure TcxGridCardViewRowLayoutController.SetCoordinates(ARow: TcxGridCardViewRow;
  AColIndex, ARowIndex: Integer);
var
  ACoordinates: TcxGridCardRowCoordinates;
begin
  ACoordinates.ColIndex := AColIndex;
  ACoordinates.RowIndex := ARowIndex;
  SetCoordinates(ARow, ACoordinates);
end;

procedure TcxGridCardViewRowLayoutController.SetCoordinates(ARow: TcxGridCardViewRow;
  ACoordinates: TcxGridCardRowCoordinates);
begin
  LayoutObject.SetCoordinates(ARow, ACoordinates);
end;

function TcxGridCardViewRowLayoutController.GetVisibleCoordinates(ARow: TcxGridCardViewRow): TcxGridCardRowCoordinates;
begin
  Result := VisibleLayoutObject.GetCoordinates(ARow);
end;

function TcxGridCardViewRowLayoutController.GetPosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition;
begin
  Result := LayoutObject.GetPosition(ARow);
end;

procedure TcxGridCardViewRowLayoutController.SetPosition(ARow: TcxGridCardViewRow;
  ALayerIndex, AIndexInLayer: Integer);
var
  APosition: TcxGridCardRowPosition;
begin
  APosition.LayerIndex := ALayerIndex;
  APosition.IndexInLayer := AIndexInLayer;
  LayoutObject.SetPosition(ARow, APosition);
end;

function TcxGridCardViewRowLayoutController.GetVisiblePosition(ARow: TcxGridCardViewRow): TcxGridCardRowPosition;
begin
  Result := VisibleLayoutObject.GetPosition(ARow);
end;

function TcxGridCardViewRowLayoutController.FindNextRow(ACard: TcxGridCard;
  ARow: TcxGridCardViewRow; ASameLayer, AGoForward, AGoOnCycle: Boolean;
  out ACycleChanged: Boolean): TcxGridCardViewRow;
var
  ARows: TList;
  ALayout: TcxGridCardVisibleRowLayoutObject;
begin
  ARows := TList.Create;
  try
    ALayout := TcxGridCardVisibleRowLayoutObject.Create(ACard);
    try
      if ASameLayer then
        ALayout.GetLayerRows(ARow, ARows)
      else
        ALayout.GetRowsAtOffset(ARow, ARows);
    finally
      ALayout.Free;
    end;
    Result := GridView.Controller.FindNextRow(ACard, ARows, ARow, AGoForward,
      AGoOnCycle, ACycleChanged);
  finally
    ARows.Free;
  end;
end;

function TcxGridCardViewRowLayoutController.FindNextRowHorizontally(ACard: TcxGridCard;
  ARow: TcxGridCardViewRow; AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;
begin
  Result := FindNextRow(ACard, ARow, IsHorizontalLayout, AGoForward, AGoOnCycle, ACycleChanged);
end;

function TcxGridCardViewRowLayoutController.FindNextRowVertically(ACard: TcxGridCard;
  ARow: TcxGridCardViewRow; AGoForward, AGoOnCycle: Boolean; out ACycleChanged: Boolean): TcxGridCardViewRow;
begin
  Result := FindNextRow(ACard, ARow, not IsHorizontalLayout, AGoForward, AGoOnCycle, ACycleChanged);
end;

{ TcxGridCardRowFilterButtonPainter }

function TcxGridCardRowFilterButtonPainter.GetViewInfo: TcxGridCardRowFilterButtonViewInfo;
begin
  Result := TcxGridCardRowFilterButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardRowFilterButtonPainter.Paint;
begin
  with ViewInfo do
    GridViewInfo.LookAndFeelPainter.DrawScaledFilterDropDownButton(Self.Canvas, Bounds, ButtonState, Active, ScaleFactor);
end;

{ TcxGridCardRowCaptionPainter }

function TcxGridCardRowCaptionPainter.GetViewInfo: TcxGridCardRowCaptionViewInfo;
begin
  Result := TcxGridCardRowCaptionViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardRowCaptionPainter.DrawContent;
begin
  inherited;
  if ViewInfo.FilterButtonViewInfo <> nil then
    ViewInfo.FilterButtonViewInfo.Paint(Canvas);
end;

{ TcxGridCardRowPainter }

function TcxGridCardRowPainter.GetViewInfo: TcxGridCardRowViewInfo;
begin
  Result := TcxGridCardRowViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardRowPainter.DrawExpandButton;
begin
  Canvas.FillRect(ViewInfo.ExpandButtonAreaBounds, ViewInfo.Params);
  ViewInfo.LookAndFeelPainter.DrawScaledExpandButton(Canvas, ViewInfo.ExpandButtonBounds, ViewInfo.Expanded, ScaleFactor);
end;

procedure TcxGridCardRowPainter.DrawFocusRect;
begin
  with ViewInfo do
    if DataViewInfo.Focused then
      GridViewInfo.Painter.DrawFocusRect(ContentBounds, CardViewInfo.HideFocusRectOnExit);
end;

procedure TcxGridCardRowPainter.DrawIndent;
begin
  Canvas.FillRect(ViewInfo.IndentBounds, ViewInfo.GetIndentViewParams);
end;

procedure TcxGridCardRowPainter.Paint;
begin
  if ViewInfo.HasIndent then DrawIndent;
  if ViewInfo.HasExpandButton then DrawExpandButton;
  ViewInfo.CaptionViewInfo.Paint(Canvas);
  ViewInfo.DataViewInfo.Paint(Canvas);
  DrawBorders;
  DrawFocusRect;
end;

{ TcxGridCardScrollButtonPainter }

function TcxGridCardScrollButtonPainter.GetViewInfo: TcxCustomGridCardScrollButtonViewInfo;
begin
  Result := TcxCustomGridCardScrollButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardScrollButtonPainter.DrawContent;
const
  ScrollBarParts: array[Boolean] of TcxScrollBarPart = (sbpLineUp, sbpLineDown);
begin
  ViewInfo.GridViewInfo.LookAndFeelPainter.DrawScaledScrollBarPart(Canvas, False,
    ViewInfo.Bounds, ScrollBarParts[ViewInfo.IsDownButton], ViewInfo.ButtonState, ScaleFactor);
end;

function TcxGridCardScrollButtonPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridCardExpandButtonPainter }

function TcxGridCardExpandButtonPainter.GetViewInfo: TcxGridCardExpandButtonViewInfo;
begin
  Result := TcxGridCardExpandButtonViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardExpandButtonPainter.Paint;
begin
  ViewInfo.LookAndFeelPainter.DrawScaledGroupExpandButton(Canvas,
    ViewInfo.Bounds, ViewInfo.CardViewInfo.Expanded, ViewInfo.ButtonState, ScaleFactor);
end;

{ TcxGridCardPainter }

function TcxGridCardPainter.GetViewInfo: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited ViewInfo);
end;

procedure TcxGridCardPainter.AfterPaint;
begin
  inherited;
  Canvas.SetClipRegion(FClipRegion, roSet);
end;

procedure TcxGridCardPainter.BeforePaint;
begin
  FClipRegion := Canvas.GetClipRegion;
  Canvas.IntersectClipRect(ViewInfo.Bounds);
  inherited;
end;

procedure TcxGridCardPainter.DrawBackground;
begin
  with ViewInfo do
    if Transparent then
      inherited
    else
    begin
      Self.Canvas.Brush.Color := Params.Color;
      Self.Canvas.FillRect(BackgroundBitmapBounds);
    end;
end;

procedure TcxGridCardPainter.DrawCardBorder;
var
  ABounds: TRect;
  AParams: TcxViewParams;
begin
  ABounds := ViewInfo.Bounds;
  ViewInfo.GetCardBorderViewParams(AParams);
  if AParams.Bitmap = nil then
    Canvas.FrameRect(ABounds, AParams.Color, ViewInfo.CardBorderWidth)
  else
    with ViewInfo.ContentBounds do
    begin
      Canvas.FillRect(Rect(ABounds.Left, ABounds.Top, ABounds.Right, Top), AParams.Bitmap);
      Canvas.FillRect(Rect(ABounds.Left, Top, Left, Bottom), AParams.Bitmap);
      Canvas.FillRect(Rect(Right, Top, ABounds.Right, Bottom), AParams.Bitmap);
      Canvas.FillRect(Rect(ABounds.Left, Bottom, ABounds.Right, ABounds.Bottom), AParams.Bitmap);
    end;
end;

procedure TcxGridCardPainter.DrawExpandButton;
begin
  ViewInfo.ExpandButtonViewInfo.Paint(Canvas);
end;

function TcxGridCardPainter.DrawExpandButtonBeforePaint: Boolean;
begin
  Result := False;
end;

procedure TcxGridCardPainter.DrawLayerSeparators;
var
  I: Integer;
begin
  Canvas.Brush.Color := ViewInfo.LayerSeparatorColor;
  for I := 0 to ViewInfo.Layout.SeparatorCount - 1 do
    Canvas.FillRect(ViewInfo.Layout.GetSeparatorBounds(I), nil, True);
end;

procedure TcxGridCardPainter.DrawRows;
var
  APrevClipRegion, ARestSpace: TcxRegion;
  I: Integer;
  ARow: TcxGridCardRowViewInfo;
begin
  APrevClipRegion := Canvas.GetClipRegion;
  try
    ARestSpace := TcxRegion.Create(ViewInfo.ContentBounds);
    try
      Canvas.IntersectClipRect(ViewInfo.ContentBounds);
      for I := ViewInfo.TopRowIndex to ViewInfo.TopRowIndex + ViewInfo.PartVisibleRowCount - 1 do
      begin
        ARow := ViewInfo.VisibleRowViewInfos[I];
        ARow.Paint(Canvas);
        ARestSpace.Combine(ARow.Bounds, roSubtract);
      end;
      Canvas.SetClipRegion(ARestSpace, roIntersect, False);
      Canvas.FillRect(ViewInfo.ContentBounds, ViewInfo.Params);
    finally
      ARestSpace.Free;
    end;
  finally
    Canvas.SetClipRegion(APrevClipRegion, roSet);
  end;
end;

procedure TcxGridCardPainter.DrawScrollButtons;
begin
  ViewInfo.ScrollButtonUp.Paint(Canvas);
  ViewInfo.ScrollButtonDown.Paint(Canvas);
end;

procedure TcxGridCardPainter.Paint;
begin
  DrawCardBorder;
  DrawScrollButtons;
  if ViewInfo.HasLayerSeparators then DrawLayerSeparators;
  DrawRows;
  inherited;
end;

{ TcxGridCardViewColumn }

function TcxGridCardViewColumn.GetNearestRow(APos: Integer): TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited GetNearest(APos));
end;

function TcxGridCardViewColumn.GetLastRow: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited LastVisible);
end;

function TcxGridCardViewColumn.GetRow(Index: Integer): TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited Items[Index]);
end;

procedure TcxGridCardViewColumn.SetRow(Index: Integer; Value: TcxGridCardViewInfo);
begin
  inherited Items[Index] := Value;
end;

{ TcxGridCardViewColumns }

function TcxGridCardViewColumns.GetBandClass: TcxGridCustomLayoutViewBandClass;
begin
  Result := TcxGridCardViewColumn;
end;

function TcxGridCardViewColumns.GetCardsViewInfo: TcxGridCardsViewInfo;
begin
  Result := TcxGridCardsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridCardViewColumns.GetItem(Index: Integer): TcxGridCardViewColumn;
begin
  Result := TcxGridCardViewColumn(inherited Items[Index]);
end;

function TcxGridCardViewColumns.GetLast: TcxGridCardViewColumn;
begin
  Result := TcxGridCardViewColumn(inherited LastVisible);
end;

{ TcxGridCardRowCellViewInfo }

constructor TcxGridCardRowCellViewInfo.Create(ARowViewInfo: TcxGridCardRowViewInfo);
begin
  FRowViewInfo := ARowViewInfo;
  inherited Create(FRowViewInfo.CardViewInfo, FRowViewInfo.Row);
end;

function TcxGridCardRowCellViewInfo.GetCardViewInfo: TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(RecordViewInfo);
end;

function TcxGridCardRowCellViewInfo.GetGridRecord: TcxGridCard;
begin
  Result := TcxGridCard(inherited GridRecord);
end;

function TcxGridCardRowCellViewInfo.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardRowCellViewInfo.GetRow: TcxGridCardViewRow;
begin
  Result := FRowViewInfo.Row;
end;

function TcxGridCardRowCellViewInfo.GetAreaBounds: TRect;
begin
  Result := inherited GetAreaBounds;
  IntersectRect(Result, Result, CardViewInfo.ScrollableAreaBounds);
end;

function TcxGridCardRowCellViewInfo.GetMultiLine: Boolean;
begin
  Result := (Row.Position.LineCount = 1) and AutoHeight;
end;

function TcxGridCardRowCellViewInfo.GetMultiLinePainting: Boolean;
begin
  Result := inherited GetMultiLinePainting or (Row.Position.LineCount > 1);
end;

function TcxGridCardRowCellViewInfo.GetTransparent: Boolean;
begin
  Result := BackgroundBitmap <> nil;
end;

function TcxGridCardRowCellViewInfo.HasFocusRect: Boolean;
begin
  Result := False;
end;

function TcxGridCardRowCellViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  AGridViewInfo: TcxGridCardViewViewInfo;
  ASelfLink: TcxObjectLink;
begin
  AGridViewInfo := GridView.ViewInfo;
  ASelfLink := cxAddObjectLink(Self);
  try
    Result := inherited MouseDown(AHitTest, AButton, AShift);
    if (ASelfLink.Ref <> nil) and (AButton = mbLeft) then
      if ssDouble in AShift then
        if Result and Row.CanExpand and GridView.OptionsBehavior.ExpandRowOnDblClick then
        begin
          Row.Expanded := not Row.Expanded;
          AGridViewInfo.Controller.MakeFocusedRecordVisible;
        end
        else
      else
        if GridView.IsDesigning then
        begin
          GridView.Controller.DesignController.SelectObject(Row, not (ssShift in AShift));
          Result := True;
        end;
  finally
    cxRemoveObjectLink(ASelfLink);
  end;
end;

{ TcxGridCardRowFilterButtonViewInfo }

constructor TcxGridCardRowFilterButtonViewInfo.Create(ARowCaptionViewInfo: TcxGridCardRowCaptionViewInfo);
begin
  inherited Create(ARowCaptionViewInfo.GridViewInfo);
  FRowCaptionViewInfo := ARowCaptionViewInfo;
end;

function TcxGridCardRowFilterButtonViewInfo.GetRow: TcxGridCardViewRow;
begin
  Result := FRowCaptionViewInfo.Row;
end;

function TcxGridCardRowFilterButtonViewInfo.GetItem: TcxCustomGridTableItem;
begin
  Result := Row;
end;

function TcxGridCardRowFilterButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  if GridView.IsDesigning then
    Result := nil
  else
    Result := TcxGridCardRowFilterButtonHitTest;
end;

function TcxGridCardRowFilterButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardRowFilterButtonPainter;
end;

function TcxGridCardRowFilterButtonViewInfo.GetVisible: Boolean;
begin
  Result := OccupiesSpace or
    (RowCaptionViewInfo.State <> gcsNone) or (State <> gcsNone);
end;

procedure TcxGridCardRowFilterButtonViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  with TcxGridCardRowFilterButtonHitTest(AHitTest) do
  begin
    GridRecord := FRowCaptionViewInfo.GridRecord;
    Item := Row;
  end;
end;

function TcxGridCardRowFilterButtonViewInfo.GetDropDownWindowOwnerBounds: TRect;
begin
  Result := inherited GetDropDownWindowOwnerBounds;
  if dxGetFilterPopupActualMode(GetFilterPopupMode) <> fpmExcel then
  begin
    Result.Left := FRowCaptionViewInfo.Bounds.Left;
    Result.Right := FRowCaptionViewInfo.Bounds.Right;
  end;
end;

function TcxGridCardRowFilterButtonViewInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := Row.GetFilterPopupMode;
end;

{ TcxGridCardRowCaptionViewInfo }

constructor TcxGridCardRowCaptionViewInfo.Create(ARowViewInfo: TcxGridCardRowViewInfo);
begin
  inherited;
  if CanFilter then
    FFilterButtonViewInfo := GetFilterButtonViewInfoClass.Create(Self);
end;

destructor TcxGridCardRowCaptionViewInfo.Destroy;
begin
  FFilterButtonViewInfo.Free;
  inherited Destroy;
end;

procedure TcxGridCardRowCaptionViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  if FilterButtonViewInfo <> nil then
    FilterButtonViewInfo.Calculate(FilterButtonBounds)
end;

procedure TcxGridCardRowCaptionViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  if FilterButtonViewInfo <> nil then
    FilterButtonViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridCardRowCaptionViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if (FilterButtonViewInfo <> nil) and FilterButtonViewInfo.VisibleForHitTest then
  begin
    Result := FilterButtonViewInfo.GetHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := inherited GetHitTest(P);
end;

function TcxGridCardRowCaptionViewInfo.GetRealWidth: Integer;
begin
  FCalculatingRealWidth := True;
  try
    CalculateParams;
    GetViewParams(Params);
    Result := CalculateRealWidth;
  finally
    FCalculatingRealWidth := False;
  end;
  GetViewParams(Params);
end;

function TcxGridCardRowCaptionViewInfo.CalculateHeight: Integer;
begin
  CalculateParams;
  if MultiLine then
    Result := GetTextCellHeight(GridViewInfo, LookAndFeelPainter)
  else
    Result := CalculateSimpleHeight(Row, Canvas, Params.Font);
end;

class function TcxGridCardRowCaptionViewInfo.CalculateSimpleHeight(ARow: TcxGridCardViewRow;
  ACanvas: TcxCanvas; AFont: TFont): Integer;
var
  ADefaultCaptionHeight: Integer;
begin
  ADefaultCaptionHeight := GetCellHeight(ACanvas.FontHeight(AFont), ARow.GridView.LookAndFeelPainter, ARow.GridView.ScaleFactor);
  dxAdjustToTouchableSize(ADefaultCaptionHeight, ARow.GridView.ScaleFactor);
  Result := ADefaultCaptionHeight * ARow.Position.LineCount;
end;

function TcxGridCardRowCaptionViewInfo.CalculateRealWidth: Integer;
begin
  Result := TextWidthWithOffset;
  if Row.HasCardExpandButton and
    ((CardViewInfo.ExpandButtonAlignment = cebaLeft) or
     (CardViewInfo.ExpandButtonAlignment = cebaRight) and not RowViewInfo.DataViewInfo.Visible) then
    Inc(Result, CardViewInfo.ExpandButtonAreaWidth);
  if (FilterButtonViewInfo <> nil) and FilterButtonViewInfo.OccupiesSpace then
    Inc(Result, ScaleFactor.Apply(cxGridCellTextOffset) + FilterButtonViewInfo.Width);
end;

function TcxGridCardRowCaptionViewInfo.CalculateWidth: Integer;
begin
  Result := FRowViewInfo.CaptionWidth;
end;

function TcxGridCardRowCaptionViewInfo.CanFilter: Boolean;
begin
  Result := Row.CanFilter(True);
end;

function TcxGridCardRowCaptionViewInfo.CanShowAutoHint: Boolean;
begin
  Result := GridView.OptionsBehavior.RowCaptionHints;
end;

function TcxGridCardRowCaptionViewInfo.CanShowCustomHint: Boolean;
begin
  Result := False;
end;

function TcxGridCardRowCaptionViewInfo.CanShowEdit: Boolean;
begin
  Result := False;
end;

function TcxGridCardRowCaptionViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := Row.CaptionAlignmentHorz;
end;

function TcxGridCardRowCaptionViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := Row.CaptionAlignmentVert;
end;

function TcxGridCardRowCaptionViewInfo.GetAutoHeight: Boolean;
begin
  Result := GridView.OptionsView.RowCaptionAutoHeight;
end;

function TcxGridCardRowCaptionViewInfo.GetFilterButtonBounds: TRect;
begin
  Result := TextAreaBounds;
  with Result do
  begin
    if FilterButtonViewInfo.OccupiesSpace then
      Left := Max(Left, Right + ScaleFactor.Apply(cxGridCellTextOffset))
    else
      Left := Max(Left, Right - FilterButtonViewInfo.Width);
    Right := Left + FilterButtonViewInfo.Width;
  end;
end;

function TcxGridCardRowCaptionViewInfo.GetFilterButtonViewInfoClass: TcxGridCardRowFilterButtonViewInfoClass;
begin
  Result := TcxGridCardRowFilterButtonViewInfo;
end;

function TcxGridCardRowCaptionViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridCardRowCaptionHitTest;
end;

function TcxGridCardRowCaptionViewInfo.GetHotTrack: Boolean;
begin
  Result := (FilterButtonViewInfo <> nil) and FilterButtonViewInfo.NeedsContainerHotTrack;
end;

function TcxGridCardRowCaptionViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardRowCaptionPainter;
end;

function TcxGridCardRowCaptionViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.RowCaptionEndEllipsis;
end;

function TcxGridCardRowCaptionViewInfo.GetText: string;
begin
  Result := Row.VisibleCaption;
end;

function TcxGridCardRowCaptionViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  if (FilterButtonViewInfo <> nil) and FilterButtonViewInfo.OccupiesSpace then
    Dec(Result.Right, ScaleFactor.Apply(cxGridCellTextOffset) + FilterButtonViewInfo.Width);
end;

procedure TcxGridCardRowCaptionViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FRowViewInfo.GetCaptionViewParams(AParams, FCalculatingRealWidth);
end;

function TcxGridCardRowCaptionViewInfo.GetVisible: Boolean;
begin
  Result := Row.Options.ShowCaption;
end;

procedure TcxGridCardRowCaptionViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridCardRowCaptionHitTest(AHitTest).RowContainerKind := ckRows;
end;

function TcxGridCardRowCaptionViewInfo.UseStandardNeedShowHint: Boolean;
begin
  Result := True;
end;

{ TcxGridCardRowDataViewInfo }

function TcxGridCardRowDataViewInfo.CalculateHeight: Integer;
begin
  CalculateParams;
  if MultiLine then
    Result := CardViewInfo.RecordsViewInfo.GetCellHeight(inherited CalculateHeight)
  else
    Result := CalculateSimpleHeight(Row, Canvas, Params.Font);
end;

class function TcxGridCardRowDataViewInfo.CalculateSimpleHeight(ARow: TcxGridCardViewRow;
  ACanvas: TcxCanvas; AFont: TFont): Integer;
var
  ADefaultCellHeight: Integer;
begin
  ADefaultCellHeight := ARow.CalculateDefaultCellHeight(ACanvas, AFont);
  dxAdjustToTouchableSize(ADefaultCellHeight, ARow.GridView.ScaleFactor);
  Result := ADefaultCellHeight * ARow.Position.LineCount;
  Result := ARow.GridView.ViewInfo.RecordsViewInfo.GetCellHeight(Result);
end;

function TcxGridCardRowDataViewInfo.CalculateWidth: Integer;
begin
  Result := FRowViewInfo.DataWidth;
end;

function TcxGridCardRowDataViewInfo.GetAutoHeight: Boolean;
begin
  Result := CardViewInfo.RecordsViewInfo.AutoDataCellHeight;
end;

function TcxGridCardRowDataViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  if GridView.IsDesigning or GridView.Controller.Customization then
    Result := TcxGridCardRowCellHitTest
  else
    Result := inherited GetHitTestClass;
end;

function TcxGridCardRowDataViewInfo.GetText: string;
begin
  Result := inherited GetText;
  if (Result = '') and not RowViewInfo.CaptionViewInfo.Visible then
    Result := '<' + Row.Caption +  '>';
end;

procedure TcxGridCardRowDataViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  FRowViewInfo.GetDataViewParams(AParams);
end;

function TcxGridCardRowDataViewInfo.GetVisible: Boolean;
begin
  Result := Row.Options.ShowData;
end;

{ TcxGridCardRowViewInfo }

constructor TcxGridCardRowViewInfo.Create(ACardViewInfo: TcxGridCardViewInfo;
  AIndex: Integer);
begin
  inherited Create(ACardViewInfo.GridViewInfo);
  FCardViewInfo := ACardViewInfo;
  FIndex := AIndex;
  CreateViewInfos;
end;

destructor TcxGridCardRowViewInfo.Destroy;
begin
  DestroyViewInfos;
  inherited;
end;

function TcxGridCardRowViewInfo.GetExpandButtonAreaSizeValue: Integer;
begin
  Result := GetExpandButtonAreaSize(LookAndFeelPainter, ScaleFactor);
end;

function TcxGridCardRowViewInfo.GetExpandButtonSizeValue: Integer;
begin
  Result := GetExpandButtonSize(LookAndFeelPainter, ScaleFactor);
end;

function TcxGridCardRowViewInfo.GetExpanded: Boolean;
begin
  Result := Row.Expanded;
end;

function TcxGridCardRowViewInfo.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardRowViewInfo.GetGridRecord: TcxGridCard;
begin
  Result := FCardViewInfo.GridRecord;
end;

function TcxGridCardRowViewInfo.GetGridViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited GridViewInfo);
end;

{function TcxGridCardRowViewInfo.GetMaxHeight: Integer;
begin  //!!!
  Result := FCardViewInfo.MaxRowViewInfoHeight;
end;}

function TcxGridCardRowViewInfo.GetRow: TcxGridCardViewRow;
begin
  Result := GridView.VisibleRows[FIndex];
end;

procedure TcxGridCardRowViewInfo.SetExpanded(Value: Boolean);
begin
  Row.Expanded := Value;
end;

function TcxGridCardRowViewInfo.CalculatePartVisible: Boolean;
begin
  Result := CardViewInfo.IsRowPartiallyVisible(Self);
end;

function TcxGridCardRowViewInfo.CalculateVisible: Boolean;
begin
  Result := CardViewInfo.IsRowVisible(Self);
end;

procedure TcxGridCardRowViewInfo.CreateViewInfos;
begin
  FCaptionViewInfo := GetCaptionViewInfoClass.Create(Self);
  FDataViewInfo := GetDataViewInfoClass.Create(Self);
end;

procedure TcxGridCardRowViewInfo.DestroyViewInfos;
begin
  FreeAndNil(FDataViewInfo);
  FreeAndNil(FCaptionViewInfo);
end;

function TcxGridCardRowViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  if HasIndent then
    Inc(Result.Left, Indent);
  if HasExpandButton then
    Inc(Result.Left, ExpandButtonAreaSize);
end;

function TcxGridCardRowViewInfo.CalculateHeaderWidth: Integer;
begin
  CalculateParams;
  Result := Width - ContentWidth + CaptionViewInfo.RealWidth + CardDataIndent;
end;

function TcxGridCardRowViewInfo.CalculateHeight: Integer;
var
  ACaptionHeight, ADataHeight: Integer;
begin
  if CardViewInfo.RecordsViewInfo.UseCardRowHeights then
    Result := CardViewInfo.RecordsViewInfo.CardRowHeights[Index]
  else
  begin
    CalculateParams;
    if FCaptionViewInfo.Visible then
      ACaptionHeight := FCaptionViewInfo.CalculateHeight
    else
      ACaptionHeight := 0;
    if FDataViewInfo.Visible then
      ADataHeight := FDataViewInfo.CalculateHeight
    else
      ADataHeight := 0;
    Result := Max(ACaptionHeight, ADataHeight);
    CheckHeight(TcxGridCardViewInfoClass(CardViewInfo.ClassType), Row, Self, LookAndFeelPainter, ScaleFactor, Result);
  end;
end;

class function TcxGridCardRowViewInfo.CalculateSimpleHeight(ARow: TcxGridCardViewRow;
  ACanvas: TcxCanvas): Integer;

  function CalculateCaptionHeight: Integer;
  begin
    if ARow.Options.ShowCaption then
      Result := GetCaptionViewInfoClass.CalculateSimpleHeight(ARow, ACanvas,
        GetSimpleViewParams(ARow, True).Font)
    else
      Result := 0;
  end;

  function CalculateDataHeight: Integer;
  begin
    if ARow.Options.ShowData then
      Result := GetDataViewInfoClass.CalculateSimpleHeight(ARow, ACanvas,
        GetSimpleViewParams(ARow, False).Font)
    else
      Result := 0;
  end;

begin
  Result := Max(CalculateCaptionHeight, CalculateDataHeight);
  CheckHeight(TcxGridCardViewInfoClass(ARow.GridView.ViewInfo.RecordsViewInfo.GetItemViewInfoClass),
    ARow, nil, ARow.GridView.LookAndFeelPainter, ARow.GridView.ScaleFactor, Result);
end;

function TcxGridCardRowViewInfo.CalculateWidth: Integer;
begin
  if CardViewInfo.Layout.Simple then
    Result := CardViewInfo.ContentWidth
  else
    Result := Row.Position.Width;
end;

class procedure TcxGridCardRowViewInfo.CheckHeight(ACardViewInfoClass: TcxGridCardViewInfoClass;
  ARow: TcxGridCardViewRow; ARowViewInfo: TcxGridCardRowViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter;
  AScaleFactor: TdxScaleFactor; var AHeight: Integer);
begin
  if ARow.HasCardExpandButton then
    AHeight := Max(AHeight, ACardViewInfoClass.GetExpandButtonSize(ALookAndFeelPainter, AScaleFactor));
  if ARow.HasExpandButton then
    AHeight := Max(AHeight, GetExpandButtonAreaSize(ALookAndFeelPainter, AScaleFactor));
end;

class function TcxGridCardRowViewInfo.GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass;
begin
  Result := TcxGridCardRowCaptionViewInfo;
end;

function TcxGridCardRowViewInfo.GetCaptionWidth: Integer;
begin
  Result := ContentWidth - DataWidth;
end;

class function TcxGridCardRowViewInfo.GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass;
begin
  Result := TcxGridCardRowDataViewInfo;
end;

function TcxGridCardRowViewInfo.GetDataWidth: Integer;
begin
  if FDataViewInfo.Visible then
    if FCaptionViewInfo.Visible then
    begin
      Result := CardViewInfo.Layout.GetHeaderWidth(Self);
      if Result = -1 then
        Result := HeaderWidth;
      Result := Max(0, Width - Result);
    end
    else
      Result := ContentWidth
  else
    Result := 0;
end;

function TcxGridCardRowViewInfo.GetDesignSelectionBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxGridCardRowViewInfo.GetExpandButtonAreaBounds: TRect;
begin
  Result := ContentBounds;
  Result.Right := Result.Left;
  Result.Left := Result.Right - ExpandButtonAreaSize;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, ContentBounds);
end;

class function TcxGridCardRowViewInfo.GetExpandButtonAreaSize(
  APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := CardRowExpandButtonOffset + GetExpandButtonSize(APainter, AScaleFactor) + CardRowExpandButtonOffset;
  dxAdjustToTouchableSize(Result, AScaleFactor);
end;

function TcxGridCardRowViewInfo.GetExpandButtonBounds: TRect;
begin
  Result := cxRectCenter(ExpandButtonAreaBounds, ExpandButtonSize, ExpandButtonSize);
end;

class function TcxGridCardRowViewInfo.GetExpandButtonSize(
  APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := APainter.ScaledExpandButtonSize(AScaleFactor);
end;

function TcxGridCardRowViewInfo.GetHeaderWidth: Integer;
begin
  Result := GridView.OptionsView.CaptionWidth;
  if Result = 0 then
    Result := CalculateHeaderWidth;
end;

function TcxGridCardRowViewInfo.GetHeight: Integer;
begin
  if FHeight = 0 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TcxGridCardRowViewInfo.GetHidden: Boolean;
begin
  Result := FCardViewInfo.VisibleRowViewInfoIndexOf(Self) = -1;
end;

function TcxGridCardRowViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := nil;
end;

function TcxGridCardRowViewInfo.GetIndent: Integer;
begin
  Result := GridView.OptionsView.CategoryIndent;
end;

function TcxGridCardRowViewInfo.GetIndentBounds: TRect;
begin
  Result := Bounds;
  Result.Right := Result.Left + Indent;
end;

function TcxGridCardRowViewInfo.GetIndentViewParams: TcxViewParams;
begin
  GridView.Styles.GetDataCellContentParams(GridRecord, Row.CategoryRow, Result);
end;

function TcxGridCardRowViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(Row);
end;

function TcxGridCardRowViewInfo.GetIsVisibleForPainting: Boolean;
begin
  Result := FPartVisible;
end;

function TcxGridCardRowViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardRowPainter;
end;

function TcxGridCardRowViewInfo.GetRealIndentBounds: TRect;
begin
  Result := GetIndentBounds;
  if IsRightToLeftConverted then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

class function TcxGridCardRowViewInfo.GetSimpleViewParams(ARow: TcxGridCardViewRow;
  AIsCaption: Boolean): TcxViewParams;
begin
  ARow.GridView.Styles.GetDataCellContentParams(nil, ARow, Result);
end;

function TcxGridCardRowViewInfo.GetWidth: Integer;
begin
  CalculateParams;
  Result := inherited GetWidth;
end;

procedure TcxGridCardRowViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  if AHitTest is TcxGridRecordCellHitTest then
    with TcxGridRecordCellHitTest(AHitTest) do
    begin
      GridRecord := Self.GridRecord;
      Item := Self.Row;
    end;
end;

procedure TcxGridCardRowViewInfo.BeforeRecalculation;
begin
  inherited;
  FCaptionViewInfo.BeforeRecalculation;
  FDataViewInfo.BeforeRecalculation;
  Visible := False;
  FPartVisible := False;
end;

procedure TcxGridCardRowViewInfo.Calculate(const ABounds: TRect);
begin
  Height := ABounds.Bottom - ABounds.Top;
  inherited;
  CalculateVisibles;
  FCaptionViewInfo.Calculate(ContentBounds.Left, ContentBounds.Top, -1, ContentHeight);
  FDataViewInfo.Calculate(ContentBounds.Left + FCaptionViewInfo.Width,
    ContentBounds.Top, -1, ContentHeight);
end;

function TcxGridCardRowViewInfo.CalculateVisibleCounts(var AVisibleRowCount, APartVisibleRowCount: Integer): Boolean;
begin
  if PartVisible then
    Inc(APartVisibleRowCount);
  if Visible then
    Inc(AVisibleRowCount);
  Result := Visible;
end;

procedure TcxGridCardRowViewInfo.CalculateVisibles;
begin
  FPartVisible := CalculatePartVisible;
  Visible := CalculateVisible;
end;

procedure TcxGridCardRowViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FCaptionViewInfo.RightToLeftConversion(ABounds);
  FDataViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridCardRowViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if HasIndent and PtInRect(IndentBounds, P) then
  begin
    Result := TcxGridCardRowIndentHitTest.Instance(P);
    InitHitTest(Result);
    Result.ViewInfo := CardViewInfo;
  end
  else
    if HasExpandButton and PtInRect(ExpandButtonAreaBounds, P) then
    begin
      Result := TcxGridCardRowExpandButtonHitTest.Instance(P);
      InitHitTest(Result);
    end
    else
    begin
      Result := CaptionViewInfo.GetHitTest(P);
      if Result = nil then
        Result := DataViewInfo.GetHitTest(P);
    end;
end;

function TcxGridCardRowViewInfo.HasExpandButton: Boolean;
begin
  Result := Row.HasExpandButton;
end;

function TcxGridCardRowViewInfo.HasIndent: Boolean;
begin
  Result := CardViewInfo.HasIndent(Self);
end;

function TcxGridCardRowViewInfo.HasLimitedHeaderSpace: Boolean;
begin
  Result := CaptionViewInfo.Visible and DataViewInfo.Visible;
end;

function TcxGridCardRowViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  ARow: TcxGridCardViewRow;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if not Result and (AButton = mbLeft) and
    (AHitTest.HitTestCode = htCardRowExpandButton) then
  begin
    ARow := Row;
    Result := CardViewInfo.MouseDown(AHitTest, AButton, AShift);
    if Result or ARow.IsDesigning then
    begin
      ARow.Expanded := not ARow.Expanded;
      ARow.Controller.MakeFocusedRecordVisible;
      ARow.Controller.DesignerModified;
    end;
  end;
end;

{ TcxGridCardDataRowDataViewInfo }

function TcxGridCardDataRowDataViewInfo.CalculateSelected: Boolean;
begin
  Result := False;
end;

{ TcxGridCardDataRowViewInfo }

procedure TcxGridCardDataRowViewInfo.GetCaptionViewParams(var AParams: TcxViewParams;
  AIgnoreSelection: Boolean);
begin
  GridView.Styles.GetDataCellParams(GridRecord, Row, AParams, False, nil, AIgnoreSelection);
end;

class function TcxGridCardDataRowViewInfo.GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass;
begin
  Result := TcxGridCardDataRowDataViewInfo;
end;

procedure TcxGridCardDataRowViewInfo.GetDataViewParams(var AParams: TcxViewParams);
begin
  Row.Styles.GetContentParams(GridRecord, AParams);
end;

class function TcxGridCardDataRowViewInfo.GetSimpleViewParams(ARow: TcxGridCardViewRow;
  AIsCaption: Boolean): TcxViewParams;
begin
  if AIsCaption then
    Result := inherited GetSimpleViewParams(ARow, AIsCaption)
  else
    ARow.Styles.GetContentParams(nil, Result);
end;

{ TcxGridCardCaptionRowCaptionViewInfo }

function TcxGridCardCaptionRowCaptionViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  if Row.HasCardExpandButton then
    case CardViewInfo.ExpandButtonAlignment of
      cebaLeft:
        Inc(Result.Left, CardViewInfo.ExpandButtonAreaWidth);
      cebaRight:
        if not RowViewInfo.DataViewInfo.Visible then
          Dec(Result.Right, CardViewInfo.ExpandButtonAreaWidth);
    end;
end;

{ TcxGridCardCaptionRowDataViewInfo }

procedure TcxGridCardCaptionRowDataViewInfo.GetEditViewDataContentOffsets(var R: TRect);
var
  R1, R2: TRect;
begin
  inherited;
  R1 := inherited GetTextAreaBounds;
  R2 := TextAreaBounds;
  Inc(R.Left, R2.Left - R1.Left);
  Inc(R.Right, R1.Right - R2.Right);
end;

function TcxGridCardCaptionRowDataViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridCardRowCellHitTest;
end;

function TcxGridCardCaptionRowDataViewInfo.GetTextAreaBounds: TRect;
begin
  Result := inherited GetTextAreaBounds;
  if Row.HasCardExpandButton then
    case CardViewInfo.ExpandButtonAlignment of
      cebaLeft:
        if not RowViewInfo.CaptionViewInfo.Visible then
          Inc(Result.Left, CardViewInfo.ExpandButtonAreaWidth);
      cebaRight:
        Dec(Result.Right, CardViewInfo.ExpandButtonAreaWidth);
    end;
end;

{ TcxGridCardCaptionRowViewInfo }

class function TcxGridCardCaptionRowViewInfo.GetCaptionViewInfoClass: TcxGridCardRowCaptionViewInfoClass;
begin
  Result := TcxGridCardCaptionRowCaptionViewInfo;
end;

procedure TcxGridCardCaptionRowViewInfo.GetCaptionViewParams(var AParams: TcxViewParams;
  AIgnoreSelection: Boolean);
begin
  GridView.Styles.GetDataCellParams(GridRecord, Row, AParams, False, nil, AIgnoreSelection);
end;

class function TcxGridCardCaptionRowViewInfo.GetDataViewInfoClass: TcxGridCardRowDataViewInfoClass;
begin
  Result := TcxGridCardCaptionRowDataViewInfo;
end;

procedure TcxGridCardCaptionRowViewInfo.GetDataViewParams(var AParams: TcxViewParams);
begin
  GetViewParams(AParams);
end;

procedure TcxGridCardCaptionRowViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetDataCellParams(GridRecord, Row, AParams);
end;

{ TcxGridCardCategoryRowViewInfo }

class procedure TcxGridCardCategoryRowViewInfo.CheckHeight(ACardViewInfoClass: TcxGridCardViewInfoClass;
  ARow: TcxGridCardViewRow; ARowViewInfo: TcxGridCardRowViewInfo; ALookAndFeelPainter: TcxCustomLookAndFeelPainter;
  AScaleFactor: TdxScaleFactor; var AHeight: Integer);
begin
  inherited;
  if (ARowViewInfo = nil) and HasSeparator(ACardViewInfoClass, ARow) or
    (ARowViewInfo <> nil) and TcxGridCardCategoryRowViewInfo(ARowViewInfo).HasSeparator then
    Inc(AHeight, ARow.GridView.OptionsView.CategorySeparatorWidth);
end;

function TcxGridCardCategoryRowViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  if AIndex = bTop then
    Result := SeparatorColor
  else
    Result := inherited GetBorderColor(AIndex);
end;

function TcxGridCardCategoryRowViewInfo.GetBorders: TcxBorders;
begin
  Result := inherited GetBorders;
  if HasSeparator then Include(Result, bTop);
end;

function TcxGridCardCategoryRowViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  if AIndex = bTop then
    Result := GridView.OptionsView.CategorySeparatorWidth
  else
    Result := inherited GetBorderWidth(AIndex);
end;

procedure TcxGridCardCategoryRowViewInfo.GetDataViewParams(var AParams: TcxViewParams);
begin
  if Row.Editable then
    Row.Styles.GetContentParams(GridRecord, AParams)
  else
    inherited;
end;

function TcxGridCardCategoryRowViewInfo.GetSeparatorColor: TColor;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetViewParams(vsCategorySeparator, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxGridCardCategoryRowViewInfo.HasSeparator: Boolean;
begin
  Result := CardViewInfo.HasCategorySeparator(Self);
end;

class function TcxGridCardCategoryRowViewInfo.HasSeparator(ACardViewInfoClass: TcxGridCardViewInfoClass;
  ARow: TcxGridCardViewRow): Boolean;
begin
  Result := ACardViewInfoClass.HasCategorySeparator(ARow);
end;

function TcxGridCardCategoryRowViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if HasSeparator and PtInRect(BorderBounds[bTop], P) then
  begin
    Result := TcxGridCardRowCellHitTest.Instance(P);
    InitHitTest(Result);
  end
  else
    Result := inherited GetHitTest(P);
end;

{ TcxCustomGridCardScrollButtonViewInfo }

constructor TcxCustomGridCardScrollButtonViewInfo.Create(ACardViewInfo: TcxGridCardViewInfo);
begin
  inherited Create(ACardViewInfo.GridViewInfo);
  FCardViewInfo := ACardViewInfo;
end;

procedure TcxCustomGridCardScrollButtonViewInfo.ScrollTimerHandler(Sender: TObject);
begin
  if Visible then
    Scroll
  else
    StopAutoScrolling;
end;

function TcxCustomGridCardScrollButtonViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxCustomGridCardScrollButtonViewInfo.CalculateWidth: Integer;
begin
  Result := Width;
end;

function TcxCustomGridCardScrollButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCardScrollButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxCustomGridCardScrollButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardScrollButtonPainter;
end;

procedure TcxCustomGridCardScrollButtonViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FCardViewInfo.InitHitTest(AHitTest);
  inherited;
end;

procedure TcxCustomGridCardScrollButtonViewInfo.StateChanged(APrevState: TcxGridCellState);
begin
  inherited;
  case State of
    gcsSelected:
      StopAutoScrolling;
    gcsPressed:
      begin
        Scroll;
        StartAutoScrolling;
      end;
    gcsNone:
      StopAutoScrolling;
  end;
end;

procedure TcxCustomGridCardScrollButtonViewInfo.StartAutoScrolling;
begin
  FScrollTimer := TcxTimer.Create(nil);
  with FScrollTimer do
  begin
    Interval := CardScrollingInterval;
    OnTimer := ScrollTimerHandler;
  end;
end;

procedure TcxCustomGridCardScrollButtonViewInfo.StopAutoScrolling;
begin
  FreeAndNil(FScrollTimer);
end;

{ TcxGridCardScrollButtonDownViewInfo }

function TcxGridCardScrollButtonDownViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridCardScrollButtonDownHitTest;
end;

function TcxGridCardScrollButtonDownViewInfo.GetIsDownButton: Boolean;
begin
  Result := True;
end;

function TcxGridCardScrollButtonDownViewInfo.GetVisible: Boolean;
begin
  Result := CardViewInfo.NeedsScrollingDown;
end;

procedure TcxGridCardScrollButtonDownViewInfo.Scroll;
begin
  with CardViewInfo do
    TopRowIndex := TopRowIndex + 1;
end;

{ TcxGridCardScrollButtonUpViewInfo }

function TcxGridCardScrollButtonUpViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridCardScrollButtonUpHitTest;
end;

function TcxGridCardScrollButtonUpViewInfo.GetIsDownButton: Boolean;
begin
  Result := False;
end;

function TcxGridCardScrollButtonUpViewInfo.GetVisible: Boolean;
begin
  Result := CardViewInfo.NeedsScrollingUp;
end;

procedure TcxGridCardScrollButtonUpViewInfo.Scroll;
begin
  with CardViewInfo do
    TopRowIndex := TopRowIndex - 1;
end;

{ TcxGridCardExpandButtonViewInfo }

constructor TcxGridCardExpandButtonViewInfo.Create(ACardViewInfo: TcxGridCardViewInfo);
begin
  inherited Create(ACardViewInfo.GridViewInfo);
  FCardViewInfo := ACardViewInfo;
end;

function TcxGridCardExpandButtonViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridCardExpandButtonViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridCardExpandButtonViewInfo.CaptureMouseOnPress: Boolean;
begin
  Result := True;
end;

procedure TcxGridCardExpandButtonViewInfo.Click;
begin
  inherited;
  FCardViewInfo.GridRecord.ToggleExpanded;
end;

function TcxGridCardExpandButtonViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridExpandButtonHitTest;
end;

function TcxGridCardExpandButtonViewInfo.GetHotTrack: Boolean;
begin
  Result := True;
end;

function TcxGridCardExpandButtonViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardExpandButtonPainter;
end;

function TcxGridCardExpandButtonViewInfo.GetVisible: Boolean;
begin
  Result := FCardViewInfo.GridRecord.Expandable;
end;

procedure TcxGridCardExpandButtonViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  TcxGridExpandButtonHitTest(AHitTest).GridRecord := FCardViewInfo.GridRecord;
end;

function TcxGridCardExpandButtonViewInfo.HasPoint(const P: TPoint): Boolean;
begin
  Result := inherited HasPoint(P) and
    LookAndFeelPainter.IsPointOverGroupExpandButton(Bounds, P);
end;

{ TcxGridCardRowLayer }

constructor TcxGridCardRowLayer.Create(AOwner: TcxGridCardRowLayout; AIndex: Integer);
begin
  inherited Create;
  FOwner := AOwner;
  FIndex := AIndex;
  BeforeCalculation;
end;

function TcxGridCardRowLayer.GetBounds: TRect;
begin
  Result.TopLeft := Rows[0].Bounds.TopLeft;
  Result.BottomRight := Rows[RowCount - 1].Bounds.BottomRight;
end;

function TcxGridCardRowLayer.GetLength: Integer;
begin
  if FLength = -1 then
    FLength := CalculateLength;
  Result := FLength;
end;

function TcxGridCardRowLayer.GetRow(AIndex: Integer): TcxGridCardRowViewInfo;
begin
  Result := Owner.LayerRowViewInfos[Index, AIndex];
end;

function TcxGridCardRowLayer.GetRowCount: Integer;
begin
  Result := Owner.LayerRowCount[Index];
end;

function TcxGridCardRowLayer.GetThickness: Integer;
begin
  if FThickness = -1 then
    FThickness := CalculateThickness;
  Result := FThickness;
end;

procedure TcxGridCardRowLayer.BeforeCalculation;
begin
  FLength := -1;
  FThickness := -1;
end;

procedure TcxGridCardRowLayer.SetThickness(Value: Integer);
begin
  FThickness := Value;
end;

procedure TcxGridCardRowLayer.Calculate(AOwnerWidth: Integer);
begin
end;

{ TcxGridCardRowLayout }

constructor TcxGridCardRowLayout.Create(ACardViewInfo: TcxGridCardViewInfo;
  ASimple: Boolean);
begin
  inherited Create(ACardViewInfo.GridRecord);
  FCardViewInfo := ACardViewInfo;
  FSimple := ASimple;
  CreateLayers;
end;

destructor TcxGridCardRowLayout.Destroy;
begin
  DestroyLayers;
  inherited;
end;

function TcxGridCardRowLayout.GetLayer(Index: Integer): TcxGridCardRowLayer;
begin
  Result := FLayers[Index];
end;

function TcxGridCardRowLayout.GetLayerRowViewInfo(ALayerIndex, AIndex: Integer): TcxGridCardRowViewInfo;
begin
  Result := CardViewInfo.RowViewInfos[LayerRows[ALayerIndex, AIndex].VisibleIndex];
end;

function TcxGridCardRowLayout.GetSeparatorCount: Integer;
begin
  Result := LayerCount - 1;
end;

procedure TcxGridCardRowLayout.CreateLayers;
var
  I: Integer;
begin
  SetLength(FLayers, LayerCount);
  for I := 0 to LayerCount - 1 do
    FLayers[I] := GetLayerClass.Create(Self, I);
end;

procedure TcxGridCardRowLayout.DestroyLayers;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].Free;
  FLayers := nil;
end;

procedure TcxGridCardRowLayout.BeforeCalculation;
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].BeforeCalculation;
end;

procedure TcxGridCardRowLayout.CalculateLayers(AWidth: Integer);
var
  I: Integer;
begin
  for I := 0 to LayerCount - 1 do
    Layers[I].Calculate(AWidth);
end;

function TcxGridCardRowLayout.CalculateRowsHeaderWidth(ARows: TList; AWidth: Integer): Integer;
var
  I: Integer;
  ARowViewInfo: TcxGridCardRowViewInfo;
begin
  Result := 0;
  for I := 0 to ARows.Count - 1 do
  begin
    ARowViewInfo := TcxGridCardRowViewInfo(ARows[I]);
    if ARowViewInfo.HasLimitedHeaderSpace then
      Result := Max(Result, ARowViewInfo.HeaderWidth);
  end;
  Result := Max(0, Min(Result, AWidth));
end;

function TcxGridCardRowLayout.GetLayerClass: TcxGridCardRowLayerClass;
begin
  Result := TcxGridCardRowLayer;
end;

function TcxGridCardRowLayout.GetLayersLength: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to LayerCount - 1 do
    Result := Max(Result, Layers[I].Length);
end;

function TcxGridCardRowLayout.GetLayersThickness: Integer;
var
  I: Integer;
begin
  Result := NonContentThickness;
  for I := 0 to LayerCount - 1 do
    Inc(Result, Layers[I].Thickness);
end;

function TcxGridCardRowLayout.GetNonContentThickness: Integer;
begin
  Result := SeparatorCount * SeparatorWidth;
end;

function TcxGridCardRowLayout.HasSeparator(ALayer: TcxGridCardRowLayer): Boolean;
begin
  Result := ALayer.Index > 0;
end;

function TcxGridCardRowLayout.IsLayerVisible(ALayer: TcxGridCardRowLayer;
  ATopRowIndex: Integer): Boolean;
begin
  Result := True;
end;

function TcxGridCardRowLayout.IsLayerVisible(const ABounds, ALayerBounds: TRect): Boolean;
begin
  Result := True;
end;

procedure TcxGridCardRowLayout.Calculate(AWidth: Integer);
begin
  BeforeCalculation;
  CalculateLayers(AWidth);
end;

procedure TcxGridCardRowLayout.CalculateRows(const ABounds: TRect;
  ATopRowIndex, AMaxRowHeight: Integer; var AVisibleRowCount, APartVisibleRowCount: Integer);
var
  R: TRect;
  I: Integer;
begin
  AVisibleRowCount := 0;
  APartVisibleRowCount := 0;

  R := ABounds;
  R.BottomRight := R.TopLeft;

  for I := 0 to LayerCount - 1 do
    if IsLayerVisible(Layers[I], ATopRowIndex) then
    begin
      CalculateLayerBounds(Layers[I], AMaxRowHeight, R);
      if IsLayerVisible(ABounds, R) then
        Layers[I].CalculateRows(R, ATopRowIndex, AVisibleRowCount, APartVisibleRowCount);
    end;
end;

function TcxGridCardRowLayout.GetIndexInLayer(ARowViewInfo: TcxGridCardRowViewInfo): Integer;
begin
  Result := GetIndexInLayer(ARowViewInfo.Row);
end;

function TcxGridCardRowLayout.GetLayerIndex(ARowViewInfo: TcxGridCardRowViewInfo): Integer;
begin
  Result := GetLayerIndex(ARowViewInfo.Row);
end;

function TcxGridCardRowLayout.GetLayerIndex(const P: TPoint): Integer;
begin
  for Result := 0 to LayerCount - 1 do
    if Layers[Result].HasPoint(P) then Exit;
  Result := -1;
end;

{ TcxGridCardRowHorizontalLayer }

function TcxGridCardRowHorizontalLayer.GetOwner: TcxGridCardRowHorizontalLayout;
begin
  Result := TcxGridCardRowHorizontalLayout(inherited Owner);
end;

function TcxGridCardRowHorizontalLayer.CalculateLength: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Inc(Result, Rows[I].Width);
end;

procedure TcxGridCardRowHorizontalLayer.CalculateRowWidths(AAvailableWidth: Integer);
var
  AWidths: array of Integer;
  I: Integer;
begin
  SetLength(AWidths, RowCount);
  for I := 0 to RowCount - 1 do
    AWidths[I] := Rows[I].Width;
  CalculateCardRowWidths(AWidths, AAvailableWidth);
  for I := 0 to RowCount - 1 do
    Rows[I].Width := AWidths[I];
end;

function TcxGridCardRowHorizontalLayer.CalculateThickness: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].Height);
end;

procedure TcxGridCardRowHorizontalLayer.Calculate(AOwnerWidth: Integer);
begin
  inherited;
  CalculateRowWidths(AOwnerWidth);
end;

procedure TcxGridCardRowHorizontalLayer.CalculateRows(const ABounds: TRect;
  ATopRowIndex: Integer; var AVisibleRowCount, APartVisibleRowCount: Integer);
var
  R: TRect;
  I: Integer;
  ARowViewInfo: TcxGridCardRowViewInfo;
begin
  R := ABounds;
  for I := 0 to RowCount - 1 do
  begin
    ARowViewInfo := Rows[I];
    R.Right := R.Left + ARowViewInfo.Width;
    ARowViewInfo.Calculate(R);
    R.Left := R.Right;
    ARowViewInfo.CalculateVisibleCounts(AVisibleRowCount, APartVisibleRowCount);
  end
end;

function TcxGridCardRowHorizontalLayer.HasPoint(const P: TPoint): Boolean;
begin
  with Rows[0].Bounds do
    Result := (Top <= P.Y) and (P.Y < Bottom);
end;

{ TcxGridCardRowHorizontalLayout }

constructor TcxGridCardRowHorizontalLayout.Create(ACardViewInfo: TcxGridCardViewInfo;
  ASimple: Boolean);
begin
  inherited;
  FFirstColumnRowsHeaderWidth := -1;
end;

function TcxGridCardRowHorizontalLayout.CalculateFirstColumnRowsHeaderWidth(AWidth: Integer): Integer;
var
  ARows: TList;
  I: Integer;
begin
  ARows := TList.Create;
  try
    for I := 0 to LayerCount - 1 do
      ARows.Add(Layers[I].Rows[0]);
    Result := CalculateRowsHeaderWidth(ARows, AWidth);
  finally
    ARows.Free;
  end;
end;

procedure TcxGridCardRowHorizontalLayout.CalculateLayerBounds(ALayer: TcxGridCardRowLayer;
  AMaxRowHeight: Integer; var ABounds: TRect);
begin
  ABounds.Top := ABounds.Bottom;
  if HasSeparator(ALayer) then
    Inc(ABounds.Top, SeparatorWidth);
  ABounds.Bottom := ABounds.Top + Min(ALayer.Thickness, AMaxRowHeight);
end;

function TcxGridCardRowHorizontalLayout.GetLayerClass: TcxGridCardRowLayerClass;
begin
  Result := TcxGridCardRowHorizontalLayer;
end;

function TcxGridCardRowHorizontalLayout.GetLayersHeight: Integer;
begin
  Result := LayersThickness;
end;

function TcxGridCardRowHorizontalLayout.IsLayerVisible(ALayer: TcxGridCardRowLayer;
  ATopRowIndex: Integer): Boolean;
begin
  Result := inherited IsLayerVisible(ALayer, ATopRowIndex) and
    (not Simple or (ALayer.Index >= ATopRowIndex));
end;

function TcxGridCardRowHorizontalLayout.IsLayerVisible(const ABounds, ALayerBounds: TRect): Boolean;
begin
  Result := inherited IsLayerVisible(ABounds, ALayerBounds) and
    (not Simple or (ABounds.Top <= ALayerBounds.Top) and (ALayerBounds.Top < ABounds.Bottom));
end;

procedure TcxGridCardRowHorizontalLayout.Calculate(AWidth: Integer);
begin
  inherited;
  FFirstColumnRowsHeaderWidth := CalculateFirstColumnRowsHeaderWidth(AWidth);
end;

function TcxGridCardRowHorizontalLayout.GetHeaderWidth(ARowViewInfo: TcxGridCardRowViewInfo): Integer;
begin
  if IsLeft(ARowViewInfo) then
    Result := FirstColumnRowsHeaderWidth
  else
    Result := -1;
end;

function TcxGridCardRowHorizontalLayout.GetSeparatorBounds(AIndex: Integer): TRect;
begin
  Result := CardViewInfo.LayerSeparatorAreaBounds;
  Result.Bottom := Layers[1 + AIndex].Bounds.Top;
  Result.Top := Result.Bottom - SeparatorWidth;
end;

function TcxGridCardRowHorizontalLayout.IsLeft(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := GetIndexInLayer(ARowViewInfo) = 0;
end;

function TcxGridCardRowHorizontalLayout.IsTop(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := GetLayerIndex(ARowViewInfo) = 0;
end;

function TcxGridCardRowHorizontalLayout.IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean;
begin
  Result := AZone.InsertionPos <> ripNewLayer;
end;

{ TcxGridCardRowVerticalLayer }

constructor TcxGridCardRowVerticalLayer.Create(AOwner: TcxGridCardRowLayout;
  AIndex: Integer);
begin
  inherited;
  FRowsHeaderWidth := -1;
end;

function TcxGridCardRowVerticalLayer.CalculateLength: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Inc(Result, Rows[I].Height);
end;

function TcxGridCardRowVerticalLayer.CalculateRowsHeaderWidth: Integer;
var
  ARows: TList;
  I: Integer;
begin
  ARows := TList.Create;
  try
    for I := 0 to RowCount - 1 do
      ARows.Add(Rows[I]);
    Result := Owner.CalculateRowsHeaderWidth(ARows, Thickness);
  finally
    ARows.Free;
  end;
end;

function TcxGridCardRowVerticalLayer.CalculateThickness: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to RowCount - 1 do
    Result := Max(Result, Rows[I].Width);
end;

procedure TcxGridCardRowVerticalLayer.SetThickness(Value: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to RowCount - 1 do
    Rows[I].Width := Thickness;
end;

procedure TcxGridCardRowVerticalLayer.Calculate(AOwnerWidth: Integer);
begin
  inherited;
  FRowsHeaderWidth := CalculateRowsHeaderWidth;
end;

procedure TcxGridCardRowVerticalLayer.CalculateRows(const ABounds: TRect;
  ATopRowIndex: Integer; var AVisibleRowCount, APartVisibleRowCount: Integer);
var
  R: TRect;
  I: Integer;
  ARowViewInfo: TcxGridCardRowViewInfo;
begin
  R := ABounds;
  for I := ATopRowIndex to RowCount - 1 do
  begin
    ARowViewInfo := Rows[I];
    R.Bottom := R.Top + ARowViewInfo.Height;
    ARowViewInfo.Calculate(R);
    R.Top := R.Bottom;
    if not ARowViewInfo.CalculateVisibleCounts(AVisibleRowCount, APartVisibleRowCount) then
      Break;
  end;
end;

function TcxGridCardRowVerticalLayer.HasPoint(const P: TPoint): Boolean;
begin
  with Rows[0].Bounds do
    Result := (Left <= P.X) and (P.X < Right);
end;

{ TcxGridCardRowVerticalLayout }

procedure TcxGridCardRowVerticalLayout.CalculateLayerBounds(ALayer: TcxGridCardRowLayer;
  AMaxRowHeight: Integer; var ABounds: TRect);
begin
  ABounds.Left := ABounds.Right;
  if HasSeparator(ALayer) then
    Inc(ABounds.Left, SeparatorWidth);
  ABounds.Right := ABounds.Left + ALayer.Thickness;
end;

procedure TcxGridCardRowVerticalLayout.CalculateLayers(AWidth: Integer);
begin
  CalculateLayerWidths(AWidth - NonContentThickness);
  inherited;
end;

procedure TcxGridCardRowVerticalLayout.CalculateLayerWidths(AAvailableWidth: Integer);
var
  AWidths: array of Integer;
  I: Integer;
begin
  SetLength(AWidths, LayerCount);
  for I := 0 to LayerCount - 1 do
    AWidths[I] := Layers[I].Thickness;
  CalculateCardRowWidths(AWidths, AAvailableWidth);
  for I := 0 to LayerCount - 1 do
    Layers[I].Thickness := AWidths[I];
end;

function TcxGridCardRowVerticalLayout.GetLayerClass: TcxGridCardRowLayerClass;
begin
  Result := TcxGridCardRowVerticalLayer;
end;

function TcxGridCardRowVerticalLayout.GetLayersHeight: Integer;
begin
  Result := LayersLength;
end;

function TcxGridCardRowVerticalLayout.GetHeaderWidth(ARowViewInfo: TcxGridCardRowViewInfo): Integer;
begin
  Result := TcxGridCardRowVerticalLayer(Layers[GetLayerIndex(ARowViewInfo)]).RowsHeaderWidth;
end;

function TcxGridCardRowVerticalLayout.GetSeparatorBounds(AIndex: Integer): TRect;
begin
  Result := CardViewInfo.LayerSeparatorAreaBounds;
  Result.Right := Layers[1 + AIndex].Bounds.Left;
  Result.Left := Result.Right - SeparatorWidth;
end;

function TcxGridCardRowVerticalLayout.IsLeft(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := True;
end;

function TcxGridCardRowVerticalLayout.IsTop(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := GetIndexInLayer(ARowViewInfo) = 0;
end;

function TcxGridCardRowVerticalLayout.IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean;
begin
  Result := AZone.InsertionPos = ripNewLayer;
end;

{ TcxGridCardViewInfo }

constructor TcxGridCardViewInfo.Create(ARecordsViewInfo: TcxCustomGridRecordsViewInfo;
  ARecord: TcxCustomGridRecord);
begin
  inherited;
  CreateRowViewInfos;
  FLayout := GridView.RowLayoutController.CreateCardRowLayout(Self);
  if HasLayerSeparators then
    FLayout.SeparatorWidth := LayerSeparatorWidth;
  CreateScrollButtons;
  FExpandButtonViewInfo := GetExpandButtonViewInfoClass.Create(Self);
end;

destructor TcxGridCardViewInfo.Destroy;
begin
  FreeAndNil(FExpandButtonViewInfo);
  DestroyScrollButtons;
  FreeAndNil(FLayout);
  DestroyRowViewInfos;
  inherited;
end;

function TcxGridCardViewInfo.GetCacheItem: TcxGridCardViewInfoCacheItem;
begin
  Result := TcxGridCardViewInfoCacheItem(inherited CacheItem);
end;

function TcxGridCardViewInfo.GetCardBorderWidth: Integer;
begin
  Result := RecordsViewInfo.CardBorderWidth;
end;

function TcxGridCardViewInfo.GetExpandButtonSizeValue: Integer;
begin
  Result := GetExpandButtonSize(LookAndFeelPainter, ScaleFactor);
end;

function TcxGridCardViewInfo.GetFirstCaptionRowViewInfo: TcxGridCardRowViewInfo;
var
  ARow: TcxGridCardViewRow;
begin
  ARow := GridView.FirstCaptionRow;
  if ARow = nil then
    Result := nil
  else
    Result := RowViewInfos[ARow.VisibleIndex];
end;

function TcxGridCardViewInfo.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewInfo.GetGridRecord: TcxGridCard;
begin
  Result := TcxGridCard(inherited GridRecord);
end;

function TcxGridCardViewInfo.GetRecordsViewInfo: TcxGridCardsViewInfo;
begin
  Result := TcxGridCardsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridCardViewInfo.GetRowViewInfo(Index: Integer): TcxGridCardRowViewInfo;
begin
  Result := TcxGridCardRowViewInfo(FRowViewInfos[Index]);
end;

function TcxGridCardViewInfo.GetRowViewInfoCount: Integer;
begin
  Result := FRowViewInfos.Count;
end;

function TcxGridCardViewInfo.GetTopRowIndex: Integer;
begin
  if CacheItem.IsTopRowIndexAssigned then
    Result := CacheItem.TopRowIndex
  else
  begin
    Result := 0;
    CacheItem.TopRowIndex := Result;
  end;
end;

function TcxGridCardViewInfo.GetVisibleRowViewInfo(Index: Integer): TcxGridCardRowViewInfo;
begin
  Result := RowViewInfos[Layout.Rows[Index].VisibleIndex];
end;

function TcxGridCardViewInfo.GetVisibleRowViewInfoCount: Integer;
begin
  Result := Layout.RowCount;
end;

procedure TcxGridCardViewInfo.SetTopRowIndex(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if TopRowIndex <> Value then
  begin
    CacheItem.TopRowIndex := Value;
    Recalculate;
    Invalidate;
  end;
end;

procedure TcxGridCardViewInfo.CreateRowViewInfos;
var
  I: Integer;
  ARow: TcxGridCardViewRow;
begin
  FRowViewInfos := TList.Create;
  for I := 0 to GridView.VisibleRowCount - 1 do
  begin
    ARow := GridView.VisibleRows[I];
    FRowViewInfos.Add(GetRowViewInfoClass(ARow).Create(Self, I));
  end;
end;

procedure TcxGridCardViewInfo.DestroyRowViewInfos;
var
  I: Integer;
begin
  for I := 0 to RowViewInfoCount - 1 do RowViewInfos[I].Free;
  FreeAndNil(FRowViewInfos);
end;

procedure TcxGridCardViewInfo.CreateScrollButtons;
begin
  FScrollButtonDown := GetScrollButtonDownViewInfoClass.Create(Self);
  FScrollButtonUp := GetScrollButtonUpViewInfoClass.Create(Self);
end;

procedure TcxGridCardViewInfo.DestroyScrollButtons;
begin
  FreeAndNil(FScrollButtonUp);
  FreeAndNil(FScrollButtonDown);
end;

procedure TcxGridCardViewInfo.CalculateExpandButtonBounds(var ABounds: TRect);
var
  ASize: Integer;
begin
  ABounds := ExpandButtonAreaBounds;
  if IsRectEmpty(ABounds) then Exit;
  ASize := ExpandButtonSize;
  ABounds.Left := (ABounds.Left + ABounds.Right - ASize) div 2;
  ABounds.Right := ABounds.Left + ASize;
  ABounds.Top := (ABounds.Top + ABounds.Bottom - ASize) div 2;
  ABounds.Bottom := ABounds.Top + ASize;
end;

function TcxGridCardViewInfo.CalculateHeight: Integer;
begin
  if RecordsViewInfo.UseCardHeight then
    Result := RecordsViewInfo.RecordHeight
  else
  begin
    CalculateParams;
    DoCalculateLayout;
    Result := 2 * CardBorderWidth;
    Inc(Result, Layout.LayersHeight);
  end;
{  if Result > RecordsViewInfo.MaxCardHeight then
    Result := RecordsViewInfo.MaxCardHeight;}
end;

procedure TcxGridCardViewInfo.CalculateLayout;
begin
  Layout.Calculate(ContentWidth);
end;

procedure TcxGridCardViewInfo.DoCalculateLayout;
begin
  if not FLayoutCalculated then
  begin
    CalculateLayout;
    FLayoutCalculated := True;
  end;
end;

procedure TcxGridCardViewInfo.CalculateRows;
var
  R: TRect;
begin
  R := ContentBounds;
  if NeedsScrollingUp then
    Inc(R.Top, ScrollButtonHeight);
  Layout.CalculateRows(R, TopRowIndex, MaxRowViewInfoHeight,
    FVisibleRowCount, FPartVisibleRowCount);
end;

procedure TcxGridCardViewInfo.CalculateRowVisibles;
var
  I: Integer;
  ARowViewInfo: TcxGridCardRowViewInfo;
begin
  FVisibleRowCount := 0;
  FPartVisibleRowCount := 0;
  for I := TopRowIndex to VisibleRowViewInfoCount - 1 do
  begin
    ARowViewInfo := VisibleRowViewInfos[I];
    ARowViewInfo.CalculateVisibles;
    if not ARowViewInfo.CalculateVisibleCounts(FVisibleRowCount, FPartVisibleRowCount) then
      Break;
  end;
end;

function TcxGridCardViewInfo.CalculateWidth: Integer;
begin
  Result := Width;
end;

function TcxGridCardViewInfo.CanGenerateExpandButtonHitTest: Boolean;
begin
  Result := False;
end;

function TcxGridCardViewInfo.GetAutoHeight: Boolean;
begin
  Result := RecordsViewInfo.AutoDataRecordHeight;
end;

function TcxGridCardViewInfo.GetBackgroundBitmapBounds: TRect;
begin
  Result := RestSpaceBounds;
end;

function TcxGridCardViewInfo.GetContentBounds: TRect;
begin
  Result := inherited GetContentBounds;
  InflateRect(Result, -CardBorderWidth, -CardBorderWidth);
  Inc(Result.Top, FScrollButtonUp.Height);
  Dec(Result.Bottom, FScrollButtonDown.Height);
end;

function TcxGridCardViewInfo.GetContentHeight: Integer;
begin
  with ContentBounds do
    Result := Bottom - Top;
end;

function TcxGridCardViewInfo.GetContentWidth: Integer;
begin
  with ContentBounds do
    Result := Right - Left;
end;

function TcxGridCardViewInfo.GetExpandButtonAlignment: TcxGridCardExpandButtonAlignment;
begin
  Result := GridView.OptionsView.CardExpandButtonAlignment;
end;

function TcxGridCardViewInfo.GetExpandButtonAreaBounds: TRect;
var
  ARowViewInfo: TcxGridCardRowViewInfo;
  AOwnerViewInfo: TcxGridCardRowCellViewInfo;
begin
  ARowViewInfo := FirstCaptionRowViewInfo;
  if (ARowViewInfo = nil) or not ARowViewInfo.PartVisible then
    SetRectEmpty(Result)
  else
  begin
    Result := ARowViewInfo.Bounds;
    case ExpandButtonAlignment of
      cebaLeft:
        begin
          Result.Right := Result.Left + ExpandButtonAreaWidth;
          if ARowViewInfo.CaptionViewInfo.Visible then
            AOwnerViewInfo := ARowViewInfo.CaptionViewInfo
          else
            AOwnerViewInfo := ARowViewInfo.DataViewInfo;
        end;
      cebaRight:
        begin
          Result.Left := Result.Right - ExpandButtonAreaWidth;
          if ARowViewInfo.DataViewInfo.Visible then
            AOwnerViewInfo := ARowViewInfo.DataViewInfo
          else
            AOwnerViewInfo := ARowViewInfo.CaptionViewInfo;
        end;
    else
      AOwnerViewInfo := nil;
    end;
    if IsRectEmpty(AOwnerViewInfo.TextAreaBounds) then
      SetRectEmpty(Result);
  end;
end;

function TcxGridCardViewInfo.GetExpandButtonAreaWidth: Integer;
begin
  Result := CardExpandButtonOffset + ExpandButtonSize + CardExpandButtonOffset;
end;

function TcxGridCardViewInfo.GetHeight: Integer;
begin
  Result := Min(inherited GetHeight, RecordsViewInfo.MaxRecordHeight);
end;

function TcxGridCardViewInfo.GetLayerSeparatorAreaBounds: TRect;
begin
  Result := ContentBounds;
end;

function TcxGridCardViewInfo.GetLayerSeparatorColor: TColor;
var
  AParams: TcxViewParams;
begin
  GridView.Styles.GetViewParams(vsLayerSeparator, nil, nil, AParams);
  Result := AParams.Color;
end;

function TcxGridCardViewInfo.GetLayerSeparatorWidth: Integer;
begin
  Result := GridView.OptionsView.LayerSeparatorWidth;
end;

function TcxGridCardViewInfo.GetMaxRowViewInfoHeight: Integer;
begin
  Result := ContentHeight;
  if NeedsScrollingUp then
    Dec(Result, ScrollButtonHeight);
end;

function TcxGridCardViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridCardPainter;
end;

function TcxGridCardViewInfo.GetRestSpaceBounds: TRect;
begin
  if (VisibleRowCount = 0) or not NeedsScrollingUp or NeedsScrollingDown then
    SetRectEmpty(Result)
  else
  begin
    Result := ContentBounds;
    Result.Top := RowViewInfos[TopRowIndex + VisibleRowCount - 1].Bounds.Bottom;
  end;
end;

function TcxGridCardViewInfo.GetScrollableAreaBounds: TRect;
begin
  Result := ContentBounds;
  if FScrollButtonUp.Visible then
    Result.Top := FScrollButtonUp.Bounds.Bottom;
  if FScrollButtonDown.Visible then
    Result.Bottom := FScrollButtonDown.Bounds.Top;
end;

function TcxGridCardViewInfo.GetScrollButtonDownBounds: TRect;
begin
  Result := ContentBounds;
  with Result do
    Top := Bottom - ScrollButtonHeight;
end;

function TcxGridCardViewInfo.GetScrollButtonDownViewInfoClass: TcxGridCardScrollButtonDownViewInfoClass;
begin
  Result := TcxGridCardScrollButtonDownViewInfo;
end;

function TcxGridCardViewInfo.GetScrollButtonUpViewInfoClass: TcxGridCardScrollButtonUpViewInfoClass;
begin
  Result := TcxGridCardScrollButtonUpViewInfo;
end;

function TcxGridCardViewInfo.GetScrollButtonHeight: Integer;
begin
  Result := MulDiv(GetScaledScrollBarSize(ScaleFactor).cy, 3, 4);
end;

function TcxGridCardViewInfo.GetScrollButtonUpBounds: TRect;
begin
  Result := ContentBounds;
  with Result do
    Bottom := Top + ScrollButtonHeight;
end;

procedure TcxGridCardViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetRecordContentParams(GridRecord, nil, AParams);
end;

function TcxGridCardViewInfo.GetWidth: Integer;
begin
  Result := RecordsViewInfo.RecordWidth;
end;

function TcxGridCardViewInfo.HasCategorySeparator(ARowViewInfo: TcxGridCardCategoryRowViewInfo): Boolean;
begin
  Result := not Layout.IsTop(ARowViewInfo) and Layout.IsWholeLine(ARowViewInfo.Row);
end;

class function TcxGridCardViewInfo.HasCategorySeparator(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := (ARow.Position.VisibleRowIndex <> 0) and ARow.Position.IsWholeLine;
end;

function TcxGridCardViewInfo.HasIndent(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := (ARowViewInfo.Row.CategoryRow <> nil) and Layout.IsLeft(ARowViewInfo);
end;

function TcxGridCardViewInfo.HasLayerSeparators: Boolean;
begin
  Result := not Layout.Simple and (LayerSeparatorWidth <> 0);
end;

function TcxGridCardViewInfo.IsRowPartiallyVisible(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := not Layout.Simple or (ARowViewInfo.Bounds.Top < ContentBounds.Bottom);
end;

function TcxGridCardViewInfo.IsRowVisible(ARowViewInfo: TcxGridCardRowViewInfo): Boolean;
begin
  Result := not Layout.Simple or (ARowViewInfo.Bounds.Bottom <= ContentBounds.Bottom);
end;

function TcxGridCardViewInfo.SupportsScrolling: Boolean;
begin
  Result := Layout.Simple;
end;

function TcxGridCardViewInfo.GetZone(AHitTest: TcxCustomGridHitTest): TcxGridItemContainerZone;

  function GetPointPosition(const ARect: TRect; const P: TPoint;
    AHorzSeparation: Boolean): TcxPosition;
  const
    HorzAreaMaxWidth = 30;
  var
    AHorzAreaWidth: Integer;
  begin
    if P.Y < GetRangeCenter(ARect.Top, ARect.Bottom) then
      Result := posTop
    else
      Result := posBottom;
    if AHorzSeparation then
    begin
      AHorzAreaWidth := Min(HorzAreaMaxWidth, (ARect.Right - ARect.Left) div 4);
      if P.X < ARect.Left + AHorzAreaWidth then
        Result := posLeft
      else
        if P.X >= ARect.Right - AHorzAreaWidth then
          Result := posRight;
    end;
  end;

var
  AInsertionIndex: Integer;
  AInsertionPos: TcxGridCardRowInsertionPos;
  ARow: TcxGridCardViewRow;
  APosition: TcxPosition;
begin
  if VisibleRowCount = 0 then
  begin
    AInsertionIndex := 0;
    AInsertionPos := ripNewLayer;
  end
  else
  begin
    if AHitTest is TcxGridRecordCellHitTest then
    begin
      ARow := TcxGridCardViewRow(TcxGridRecordCellHitTest(AHitTest).Item);
      APosition := GetPointPosition(RowViewInfos[ARow.VisibleIndex].ContentBounds,
        AHitTest.Pos, GridView.SupportsLayeredRows);
    end
    else
      if AHitTest.HitTestCode = htCardScrollButtonUp then
      begin
        ARow := VisibleRowViewInfos[TopRowIndex].Row;
        APosition := posTop;
      end
      else
        if (AHitTest.HitTestCode = htCardScrollButtonDown) or PtInRect(RestSpaceBounds, AHitTest.Pos) then
        begin
          ARow := VisibleRowViewInfos[TopRowIndex + PartVisibleRowCount - 1].Row;
          APosition := posBottom;
        end
        else
        begin
          ARow := nil;
          APosition := posNone;
        end;
    if ARow <> nil then
      Layout.GetInsertionParams(ARow, APosition, AInsertionIndex, AInsertionPos)
    else
      if PtInRect(ContentBounds, AHitTest.Pos) then
      begin
        AInsertionIndex := Layout.GetLayerIndex(AHitTest.Pos);
        if AInsertionIndex <> -1 then
        begin
          AInsertionIndex := Layout.LayerFirstRowIndex[AInsertionIndex + 1];
          AInsertionPos := ripPrevLayer;
        end;
      end
      else
        AInsertionIndex := -1;
  end;
  if AInsertionIndex <> -1 then
    Result := TcxGridCardRowContainerZone.Create(GridRecord, AInsertionIndex, AInsertionPos)
  else
    Result := nil;
end;

function TcxGridCardViewInfo.GetZoneBounds(AZone: TcxGridCardRowContainerZone): TRect;
var
  AUseFirstPart: Boolean;
begin
  if VisibleRowCount = 0 then
    Result := ContentBounds
  else
  begin
    AUseFirstPart := False;
    if AZone.ItemIndex > VisibleRowViewInfos[TopRowIndex + PartVisibleRowCount - 1].Row.VisibleIndex then
      Result := VisibleRowViewInfos[TopRowIndex + PartVisibleRowCount - 1].Bounds
    else
      if AZone.InsertionPos = ripPrevLayer then
        Result := VisibleRowViewInfos[VisibleRowViewInfoIndexOf(RowViewInfos[AZone.ItemIndex]) - 1].Bounds
      else
      begin
        Result := RowViewInfos[AZone.ItemIndex].Bounds;
        AUseFirstPart := True;
      end;
    if not AUseFirstPart then
      if IsZoneVertical(AZone) then
        if IsRightToLeftConverted then
          Result.Right := Result.Left
        else
          Result.Left := Result.Right
      else
        Result.Top := Result.Bottom;

    if AZone.InsertionPos = ripNewLayer then
      if IsZoneVertical(AZone) then
      begin
        Result.Top := ContentBounds.Top;
        Result.Bottom := ContentBounds.Bottom;
      end
      else
      begin
        Result.Left := ContentBounds.Left;
        Result.Right := ContentBounds.Right;
      end;
  end;
end;

function TcxGridCardViewInfo.IsZoneVertical(AZone: TcxGridCardRowContainerZone): Boolean;
begin
  Result := Layout.IsZoneVertical(AZone);
end;

function TcxGridCardViewInfo.GetExpandButtonViewInfoClass: TcxGridCardExpandButtonViewInfoClass;
begin
  Result := TcxGridCardExpandButtonViewInfo;
end;

class function TcxGridCardViewInfo.GetRowViewInfoClass(ARow: TcxGridCardViewRow): TcxGridCardRowViewInfoClass;
const
  RowViewInfoClasses: array[TcxGridCardViewRowKind] of TcxGridCardRowViewInfoClass =
    (TcxGridCardDataRowViewInfo, TcxGridCardCaptionRowViewInfo, TcxGridCardCategoryRowViewInfo);
begin
  Result := RowViewInfoClasses[ARow.Kind];
end;

procedure TcxGridCardViewInfo.BeforeRecalculation;
var
  I: Integer;
begin
  FVisibleRowCount := 0;
  FPartVisibleRowCount := 0;
  FLayoutCalculated := False;
  inherited;
  for I := 0 to RowViewInfoCount - 1 do
    RowViewInfos[I].BeforeRecalculation;
  FExpandButtonViewInfo.BeforeRecalculation;
end;

procedure TcxGridCardViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  inherited;
  DoCalculateLayout;
  SetRectEmpty(FScrollButtonDown.Bounds);
  SetRectEmpty(FScrollButtonUp.Bounds);
  CalculateRows;
  if SupportsScrolling then
  begin
    if FScrollButtonDown.Visible then
      FScrollButtonDown.Calculate(ScrollButtonDownBounds);
    if FScrollButtonUp.Visible then
      FScrollButtonUp.Calculate(ScrollButtonUpBounds);
    CalculateRowVisibles;
  end;
end;

procedure TcxGridCardViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
  ARowViewInfo: TcxGridCardRowViewInfo;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := TopRowIndex to VisibleRowViewInfoCount - 1 do
  begin
    ARowViewInfo := VisibleRowViewInfos[I];
    if ARowViewInfo.PartVisible then
      ARowViewInfo.RightToLeftConversion(ABounds);
  end;
  if FExpandButtonViewInfo.Visible then
    FExpandButtonViewInfo.RightToLeftConversion(ABounds);
  if SupportsScrolling then
  begin
    if FScrollButtonDown.Visible then
      FScrollButtonDown.RightToLeftConversion(ABounds);
    if FScrollButtonUp.Visible then
      FScrollButtonUp.RightToLeftConversion(ABounds);
  end;
end;

function TcxGridCardViewInfo.GetBoundsForItem(AItem: TcxCustomGridTableItem): TRect;
begin
  with RowViewInfos[AItem.VisibleIndex] do
    if Hidden then
      Result := inherited GetBoundsForItem(AItem)
    else
      Result := Bounds;
end;

procedure TcxGridCardViewInfo.GetCardBorderViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetCardBorderVisualParams(GridRecord, AParams);
end;

function TcxGridCardViewInfo.GetCellViewInfoByItem(AItem: TcxCustomGridTableItem): TcxGridTableDataCellViewInfo;
begin
  if AItem.VisibleIndex = -1 then
    Result := nil
  else
    Result := RowViewInfos[AItem.VisibleIndex].DataViewInfo;
end;

function TcxGridCardViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
  AHitTest: TcxCustomGridHitTest;
begin
  Result := FScrollButtonUp.GetHitTest(P);
  if Result = nil then
  begin
    Result := FScrollButtonDown.GetHitTest(P);
    if Result = nil then
    begin
      Result := FExpandButtonViewInfo.GetHitTest(P);
      if Result = nil then
      begin
        Result := inherited GetHitTest(P);
        if Result <> nil then
          for I := TopRowIndex to TopRowIndex + PartVisibleRowCount - 1 do
          begin
            AHitTest := VisibleRowViewInfos[I].GetHitTest(P);
            if AHitTest <> nil then
            begin
              Result := AHitTest;
              Exit;
            end;
          end;
      end;
    end;
  end;
end;

procedure TcxGridCardViewInfo.MainCalculate(ALeftBound, ATopBound: Integer);
begin
  inherited;
  if FExpandButtonViewInfo.Visible then
    FExpandButtonViewInfo.Calculate(ExpandButtonBounds);
end;

procedure TcxGridCardViewInfo.MakeRowVisible(ARow: TcxGridCardViewRow);
var
  AVisibleRowIndex: Integer;
begin
  AVisibleRowIndex := VisibleRowViewInfoIndexOf(ARow);
  if AVisibleRowIndex = -1 then Exit;
  if AVisibleRowIndex <= TopRowIndex then
    TopRowIndex := AVisibleRowIndex
  else
    if AVisibleRowIndex >= TopRowIndex + VisibleRowCount then
      repeat
        TopRowIndex := TopRowIndex + 1;
      until VisibleRowViewInfos[AVisibleRowIndex].Visible or (AVisibleRowIndex = TopRowIndex);
end;

function TcxGridCardViewInfo.NeedsScrollingDown: Boolean;
begin
  Result := (TopRowIndex + VisibleRowCount < VisibleRowViewInfoCount) and
    ((VisibleRowCount > 0) or (TopRowIndex <> VisibleRowViewInfoCount - 1));
end;

function TcxGridCardViewInfo.NeedsScrollingUp: Boolean;
begin
  Result := TopRowIndex > 0;
end;

function TcxGridCardViewInfo.VisibleRowViewInfoIndexOf(ARow: TcxGridCardViewRow): Integer;
begin
  if ARow.VisibleIndex = -1 then
    Result := -1
  else
    Result := VisibleRowViewInfoIndexOf(RowViewInfos[ARow.VisibleIndex]);
end;

function TcxGridCardViewInfo.VisibleRowViewInfoIndexOf(ARowViewInfo: TcxGridCardRowViewInfo): Integer;
begin
  Result := Layout.GetRowIndex(ARowViewInfo.Row);
end;

{ TcxGridCardsViewInfoHorizontalCalculator }

function TcxGridCardsViewInfoHorizontalCalculator.GetRecordSpaceVert: Integer;
begin
  Result := GetRecordIndent;
end;

{ TcxGridCardsViewInfoVerticalCalculator }

function TcxGridCardsViewInfoVerticalCalculator.GetRecordSpaceHorz: Integer;
begin
  Result := GetRecordIndent;
end;
{ TcxGridCardsViewInfo }

function TcxGridCardsViewInfo.GetCardBorderWidth: Integer;
begin
  Result := GridView.OptionsView.CardBorderWidth;
end;

function TcxGridCardsViewInfo.GetCardContentWidth: Integer;
begin
  Result := RecordWidth - 2 * CardBorderWidth;
end;

function TcxGridCardsViewInfo.GetColumns: TcxGridCardViewColumns;
begin
  Result := TcxGridCardViewColumns(inherited Bands);
end;

function TcxGridCardsViewInfo.GetColumnWidth: Integer;
begin
  Result := RecordWidth + RecordSpaceHorz;
end;

function TcxGridCardsViewInfo.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardsViewInfo.GetGridViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited GridViewInfo);
end;

function TcxGridCardsViewInfo.GetItem(Index: Integer): TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited Items[Index]);
end;

function TcxGridCardsViewInfo.GetRowHeight: Integer;
begin
  Result := RecordHeight + RecordSpaceVert;
end;

function TcxGridCardsViewInfo.GetViewData: TcxGridCardViewViewData;
begin
  Result := TcxGridCardViewViewData(inherited ViewData);
end;

function TcxGridCardsViewInfo.CalculateRecordHeight: Integer;
var
  I: Integer;
begin
  if GridView.VisibleRowCount = 0 then
    Result := EmptyCardHeight
  else
  begin
    Result := 2 * CardBorderWidth;
    SetLength(CardRowHeights, GridView.VisibleRowCount);
    for I := 0 to GridView.VisibleRowCount - 1 do
    begin
      CardRowHeights[I] := CalculateCardRowHeight(GridView.VisibleRows[I]);
      Inc(Result, CardRowHeights[I]);
    end;
  end;
end;

function TcxGridCardsViewInfo.CalculateCardRowHeight(ARow: TcxGridCardViewRow): Integer;
begin
  Result := TcxGridCardViewInfoClass(GetItemViewInfoClass).GetRowViewInfoClass(ARow).CalculateSimpleHeight(ARow, Canvas);
end;

function TcxGridCardsViewInfo.CalculateRecordWidth: Integer;
var
  ASpace, ACardColCount: Integer;
begin
  Result := GridView.OptionsView.CardWidth;
  if GridView.OptionsView.CardAutoWidth then
  begin
    ASpace := ContentBounds.Right - ContentBounds.Left + RecordSpaceHorz;
    ACardColCount := ASpace div (Result + RecordSpaceHorz);
    if ACardColCount < 1 then ACardColCount := 1;
    Result := ASpace div ACardColCount - RecordSpaceHorz;
    if Result < cxGridCardMinWidth then Result := cxGridCardMinWidth;
  end;
end;

function TcxGridCardsViewInfo.GetAutoCellHeight: Boolean;
begin
  Result := AutoDataCellHeight or GridView.OptionsView.RowCaptionAutoHeight;
end;

function TcxGridCardsViewInfo.GetAutoDataRecordHeight: Boolean;
begin
  Result := AutoCellHeight or not GridView.OptionsView.EmptyRows or
    GridView.OptionsCustomize.CardExpanding;
end;

function TcxGridCardsViewInfo.GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass;
begin
  Result := TcxGridCardViewInfo;
end;

function TcxGridCardsViewInfo.GetSeparatorWidth: Integer;
begin
  Result := GridViewInfo.SeparatorsViewInfo.Width;
end;

function TcxGridCardsViewInfo.GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass;
begin
  case GridView.LayoutDirection of
    ldHorizontal:
      Result := TcxGridCardsViewInfoHorizontalCalculator;
    ldVertical:
      Result := TcxGridCardsViewInfoVerticalCalculator;
  else
    Result := inherited GetCalculatorClass;
  end;
end;

function TcxGridCardsViewInfo.GetBandsClass: TcxGridCustomLayoutViewBandsClass;
begin
  Result := TcxGridCardViewColumns;
end;

function TcxGridCardsViewInfo.GetRealItem(ARecord: TcxCustomGridRecord): TcxGridCardViewInfo;
begin
  Result := TcxGridCardViewInfo(inherited GetRealItem(ARecord));
end;

function TcxGridCardsViewInfo.GetZone(AHitTest: TcxCustomGridHitTest): TcxGridItemContainerZone;
begin
  if (AHitTest is TcxGridRecordHitTest) and
    TcxGridCardViewController(Controller).CanHandleHitTest(AHitTest) then
    Result := GetRealItem(TcxGridRecordHitTest(AHitTest).GridRecord).GetZone(AHitTest)
  else
    Result := nil;
end;

function TcxGridCardsViewInfo.UseCardHeight: Boolean;
begin
  Result := UseCardRowHeights and not AutoDataRecordHeight and
    GridView.RowLayoutController.IsSimpleLayout;
end;

function TcxGridCardsViewInfo.UseCardRowHeights: Boolean;
begin
  Result := (CardRowHeights <> nil) and not AutoCellHeight and
    (not AutoDataRecordHeight or (GridView.OptionsView.CategorySeparatorWidth = 0) {or (no category rows)});
end;

{ TcxGridCardViewSeparatorsViewInfo }

function TcxGridCardViewSeparatorsViewInfo.GetGridView: TcxGridCardView;
begin
  Result := GridViewInfo.GridView;
end;

function TcxGridCardViewSeparatorsViewInfo.GetGridViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited GridViewInfo);
end;

function TcxGridCardViewSeparatorsViewInfo.GetCardsViewInfo: TcxGridCardsViewInfo;
begin
  Result := TcxGridCardsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridCardViewSeparatorsViewInfo.GetHitTest(const P: TPoint): TcxGridCardViewSeparatorHitTest;
var
  I: Integer;
begin
  if GridView.SupportsCardSizing and PtInRect(GridViewInfo.ClientBounds, P) then
    for I := 0 to HitTestItemCount - 1 do
      if PtInRect(HitTestItems[I], P) then
      begin
        Result := TcxGridCardViewSeparatorHitTest(TcxGridCardViewSeparatorHitTest.Instance(P));
        Result.Separators := Self;
        Result.Index := I;
        Exit;
      end;
  Result := nil;
end;

{ TcxGridCardViewSeparatorsHorizontalViewInfo }

function TcxGridCardViewSeparatorsHorizontalViewInfo.GetHitTestItem(Index: Integer): TRect;
begin
  Result := Items[Index];
  InflateRect(Result, SeparatorSizingAddZone, 0);
end;

function TcxGridCardViewSeparatorsHorizontalViewInfo.GetHitTestItemCount: Integer;
begin
  Result := Count;
end;

procedure TcxGridCardViewSeparatorsHorizontalViewInfo.DoCalculate;
var
  R: TRect;
  I: Integer;
begin
  R := Bounds;
  for I := 0 to CardsViewInfo.Columns.Count - 1 do
  begin
    R.Left := CardsViewInfo.Columns[I][0].Bounds.Right + CardsViewInfo.RecordIndent;
    R.Right := R.Left + Width;
    Items[I] := R;
  end;
end;

{ TcxGridCardViewSeparatorsVerticalViewInfo }

function TcxGridCardViewSeparatorsVerticalViewInfo.GetHitTestItem(Index: Integer): TRect;
begin
  Result := Bounds;
  Result.Left := CardsViewInfo.Columns[Index][0].Bounds.Right +
    (CardsViewInfo.RecordSpaceHorz - SeparatorSizingZone) div 2;
  Result.Right := Result.Left + SeparatorSizingZone;
end;

function TcxGridCardViewSeparatorsVerticalViewInfo.GetHitTestItemCount: Integer;
begin
  Result := CardsViewInfo.Columns.Count;
end;

procedure TcxGridCardViewSeparatorsVerticalViewInfo.DoCalculate;

  function GetRowBottom(ARowIndex: Integer): Integer;
  var
    I: Integer;
    ABand: TcxGridCustomLayoutViewBand;
  begin
    Result := 0;
    for I := 0 to CardsViewInfo.Columns.Count - 1 do
    begin
      ABand := CardsViewInfo.Columns[I];
      if ARowIndex < ABand.Count then
        with ABand[ARowIndex] do
          if Result < Bounds.Bottom then Result := Bounds.Bottom;
    end;
  end;

var
  R: TRect;
  I: Integer;
begin
  if CardsViewInfo.Columns.Count = 0 then Exit;
  R := Bounds;
  R.Right := Max(R.Right, CardsViewInfo.Columns[0][0].Bounds.Right);
  for I := 0 to CardsViewInfo.Columns[0].Count - 1 do
  begin
    R.Top := GetRowBottom(I) + CardsViewInfo.RecordIndent;
    R.Bottom := R.Top + Width;
    Items[I] := R;
  end;
end;

{ TcxGridCardViewViewInfo }

function TcxGridCardViewViewInfo.GetController: TcxGridCardViewController;
begin
  Result := TcxGridCardViewController(inherited Controller);
end;

function TcxGridCardViewViewInfo.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewViewInfo.GetRecordsViewInfo: TcxGridCardsViewInfo;
begin
  Result := TcxGridCardsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridCardViewViewInfo.GetSeparatorsViewInfo: TcxGridCardViewSeparatorsViewInfo;
begin
  Result := TcxGridCardViewSeparatorsViewInfo(inherited SeparatorsViewInfo);
end;

function TcxGridCardViewViewInfo.GetViewData: TcxGridCardViewViewData;
begin
  Result := TcxGridCardViewViewData(inherited ViewData);
end;

function TcxGridCardViewViewInfo.DoGetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := SeparatorsViewInfo.GetHitTest(P);
  if Result = nil then
    Result := inherited DoGetHitTest(P);
end;

function TcxGridCardViewViewInfo.GetDefaultGridModeBufferCount: Integer;
begin
  if (RecordsViewInfo.RecordWidth = 0) or (RecordsViewInfo.RecordHeight = 0) then
  begin
    Controller.PostGridModeBufferCountUpdate;
    Result := 0;
  end
  else
    Result := RoundDiv(Screen.Width, RecordsViewInfo.ColumnWidth) *
      RoundDiv(Screen.Height, RecordsViewInfo.RowHeight) + 2;
end;

function TcxGridCardViewViewInfo.GetScrollableAreaBoundsForEdit: TRect;
begin
  Result := inherited GetScrollableAreaBoundsForEdit;
  IntersectRect(Result, Result, Controller.FocusedCardViewInfo.ScrollableAreaBounds);
end;

function TcxGridCardViewViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridCardsViewInfo;
end;

function TcxGridCardViewViewInfo.GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass;
begin
  case GridView.LayoutDirection of
    ldHorizontal:
      Result := TcxGridCardViewSeparatorsHorizontalViewInfo;
    ldVertical:
      Result := TcxGridCardViewSeparatorsVerticalViewInfo;
  else
    Result := nil;
  end;
end;

{ TcxGridCardViewInfoCacheItem }

procedure TcxGridCardViewInfoCacheItem.SetTopRowIndex(Value: Integer);
begin
  FTopRowIndex := Value;
  FIsTopRowIndexAssigned := True;
end;

procedure TcxGridCardViewInfoCacheItem.UnassignValues(AKeepMaster: Boolean);
begin
  inherited;
  FIsTopRowIndexAssigned := False;
end;

{ TcxGridCardViewRowOptions }

constructor TcxGridCardViewRowOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FExpanding := True;
  FShowData := True;
end;

procedure TcxGridCardViewRowOptions.SetExpanding(Value: Boolean);
begin
  if FExpanding <> Value then
  begin
    FExpanding := Value;
    Changed(ticSize);
  end;
end;

procedure TcxGridCardViewRowOptions.SetShowData(Value: Boolean);
begin
  if FShowData <> Value then
  begin
    if not Value then
      ShowCaption := True;
    FShowData := Value;
    Changed(ticSize);
  end;
end;

procedure TcxGridCardViewRowOptions.BeforeShowCaptionChange;
begin
  inherited;
  if ShowCaption then ShowData := True;
end;

procedure TcxGridCardViewRowOptions.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridCardViewRowOptions then
    with TcxGridCardViewRowOptions(Source) do
    begin
      Self.Expanding := Expanding;
      Self.ShowData := ShowData;
    end;
end;

{ TcxGridCardViewRowPosition }

constructor TcxGridCardViewRowPosition.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FLineCount := 1;
end;

procedure TcxGridCardViewRowPosition.ChangeScale(M, D: Integer);
begin
  if Width > 0 then
    Width := Max(MulDiv(Width, M, D), 1);
end;

function TcxGridCardViewRowPosition.GetBeginsLayer: Boolean;
begin
  Result := GridView.RowLayoutController.BeginsLayer[Item];
end;

function TcxGridCardViewRowPosition.GetColIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetCoordinates(Item).ColIndex;
end;

function TcxGridCardViewRowPosition.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewRowPosition.GetIndexInLayer: Integer;
begin
  Result := GridView.RowLayoutController.GetPosition(Item).IndexInLayer;
end;

function TcxGridCardViewRowPosition.GetItem: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(inherited Item);
end;

function TcxGridCardViewRowPosition.GetLayerIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetPosition(Item).LayerIndex;
end;

function TcxGridCardViewRowPosition.GetLayerVisibleIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetVisiblePosition(Item).LayerIndex;
end;

function TcxGridCardViewRowPosition.GetRowIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetCoordinates(Item).RowIndex;
end;

function TcxGridCardViewRowPosition.GetVisibleColIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetVisibleCoordinates(Item).ColIndex;
end;

function TcxGridCardViewRowPosition.GetVisibleIndexInLayer: Integer;
begin
  Result := GridView.RowLayoutController.GetVisiblePosition(Item).IndexInLayer;
end;

function TcxGridCardViewRowPosition.GetVisibleRowIndex: Integer;
begin
  Result := GridView.RowLayoutController.GetVisibleCoordinates(Item).RowIndex;
end;

procedure TcxGridCardViewRowPosition.SetBeginsLayer(Value: Boolean);
begin
  if GridView.IsUpdating or GridView.IsRestoring then
    FBeginsLayer := Value
  else
    GridView.RowLayoutController.BeginsLayer[Item] := Value;
end;

procedure TcxGridCardViewRowPosition.SetColIndex(Value: Integer);
begin
  GridView.RowLayoutController.SetCoordinates(Item, Value, -1);
end;

procedure TcxGridCardViewRowPosition.SetIndexInLayer(Value: Integer);
begin
  GridView.RowLayoutController.SetPosition(Item, -1, Value);
end;

procedure TcxGridCardViewRowPosition.SetLayerIndex(Value: Integer);
begin
  GridView.RowLayoutController.SetPosition(Item, Value, -1);
end;

procedure TcxGridCardViewRowPosition.SetLineCount(Value: Integer);
begin
  Value := Max(Value, 1);
  if FLineCount <> Value then
  begin
    FLineCount := Value;
    Changed(ticSize);
  end;
end;

procedure TcxGridCardViewRowPosition.SetRowIndex(Value: Integer);
begin
  GridView.RowLayoutController.SetCoordinates(Item, -1, Value);
end;

procedure TcxGridCardViewRowPosition.SetWidth(Value: Integer);
begin
  Value := Max(Value, 0);
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(ticSize);
  end;
end;

procedure TcxGridCardViewRowPosition.AssignParams;
begin
  BeginsLayer := FBeginsLayer;
end;

procedure TcxGridCardViewRowPosition.SaveParams;
begin
  FBeginsLayer := BeginsLayer;
end;

procedure TcxGridCardViewRowPosition.Assign(Source: TPersistent);
begin
  inherited;
  with TcxGridCardViewRowPosition(Source) do
  begin
    Self.BeginsLayer := BeginsLayer;
    {Self.IndexInLayer := IndexInLayer;
    Self.LayerIndex := LayerIndex;}
    Self.LineCount := LineCount;
    Self.Width := Width;
  end;
end;

function TcxGridCardViewRowPosition.IsWholeLine: Boolean;
begin
  Result := GridView.RowLayoutController.IsWholeLine(Item);
end;

{ TcxGridCardViewRowStyles }

constructor TcxGridCardViewRowStyles.Create(AOwner: TPersistent);
begin
  inherited;
  BitmapInViewParams := True;
end;

function TcxGridCardViewRowStyles.GetGridViewValue: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewRowStyles.GetItem: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(inherited Item);
end;

procedure TcxGridCardViewRowStyles.SetOnGetCaptionRowStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetCaptionRowStyle, Value) then
  begin
    FOnGetCaptionRowStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridCardViewRowStyles.SetOnGetCaptionStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetCaptionStyle, Value) then
  begin
    FOnGetCaptionStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridCardViewRowStyles.SetOnGetCategoryRowStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetCategoryRowStyle, Value) then
  begin
    FOnGetCategoryRowStyle := Value;
    Item.Changed(ticProperty);
  end;
end;

procedure TcxGridCardViewRowStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  case Index of
    isCaptionRow:
      GridView.Styles.GetCaptionRowParams(TcxCustomGridRecord(AData), Item, AParams);
    isCategoryRow:
      GridView.Styles.GetCategoryRowParams(TcxCustomGridRecord(AData), Item, AParams);
    isRowCaption:
      GridView.Styles.GetRowCaptionParams(TcxCustomGridRecord(AData), Item, AParams);
  else
    inherited;
  end;
end;

procedure TcxGridCardViewRowStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridCardViewRowStyles then
    with TcxGridCardViewRowStyles(Source) do
    begin
      Self.Caption := Caption;
      Self.CaptionRow := CaptionRow;
      Self.CategoryRow := CategoryRow;
      Self.OnGetCaptionRowStyle := OnGetCaptionRowStyle;
      Self.OnGetCaptionStyle := OnGetCaptionStyle;
      Self.OnGetCategoryRowStyle := OnGetCategoryRowStyle;
    end;
end;

procedure TcxGridCardViewRowStyles.GetCaptionParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCaptionStyle) then
    FOnGetCaptionStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isRowCaption, ARecord, AStyle, AParams);
end;

procedure TcxGridCardViewRowStyles.GetCaptionRowParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCaptionRowStyle) then
    FOnGetCaptionRowStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isCaptionRow, ARecord, AStyle, AParams);
end;

procedure TcxGridCardViewRowStyles.GetCategoryRowParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCategoryRowStyle) then
    FOnGetCategoryRowStyle(GridView, ARecord, Item, AStyle);
  GetViewParams(isCategoryRow, ARecord, AStyle, AParams);
end;

{ TcxGridCardViewRow }

constructor TcxGridCardViewRow.Create(AOwner: TComponent);
begin
  inherited;
  FCategoryItems := TList.Create;
end;

destructor TcxGridCardViewRow.Destroy;
begin
  FCategoryItems.Free;
  inherited;
end;

function TcxGridCardViewRow.GetCaptionAlignmentHorz: TAlignment;
begin
  Result := HeaderAlignmentHorz;
end;

function TcxGridCardViewRow.GetCaptionAlignmentVert: TcxAlignmentVert;
begin
  Result := HeaderAlignmentVert;
end;

function TcxGridCardViewRow.GetCategoryItem(AIndex: Integer): TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(FCategoryItems[AIndex]);
end;

function TcxGridCardViewRow.GetCategoryItemCount: Integer;
begin
  Result := FCategoryItems.Count;
end;

function TcxGridCardViewRow.GetExpanded: Boolean;
begin
  Result := HasExpandableItems and FExpanded;
end;

function TcxGridCardViewRow.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewRow.GetOptions: TcxGridCardViewRowOptions;
begin
  Result := TcxGridCardViewRowOptions(inherited Options);
end;

function TcxGridCardViewRow.GetStyles: TcxGridCardViewRowStyles;
begin
  Result := TcxGridCardViewRowStyles(inherited Styles);
end;

procedure TcxGridCardViewRow.SetCaptionAlignmentHorz(Value: TAlignment);
begin
  HeaderAlignmentHorz := Value;
end;

procedure TcxGridCardViewRow.SetCaptionAlignmentVert(Value: TcxAlignmentVert);
begin
  HeaderAlignmentVert := Value;
end;

procedure TcxGridCardViewRow.SetCategoryRow(Value: TcxGridCardViewRow);
begin
  if (FKind <> rkCategory) and (CategoryRow <> Value) then
    if Value = nil then
      Index := GridView.FirstCategoryRow.Index
    else
      if Value.Kind = rkCategory then
        Index := Value.Index + Value.CategoryItemCount + 1 - Ord(Index < Value.Index);
end;

procedure TcxGridCardViewRow.SetCategoryRowValue(Value: TcxGridCardViewRow);
begin
  FCategoryRow := Value;
  FCategoryItems.Clear;
  if FCategoryRow <> nil then
    FCategoryRow.FCategoryItems.Add(Self);
end;

procedure TcxGridCardViewRow.SetExpanded(Value: Boolean);
begin
  if (Expanded <> Value) and (IsLoading or HasExpandableItems) then
  begin
    if not GridView.RowExpandedChanging(Self, Value) then Exit;
    GridView.BeginUpdate;
    try
      GridView.SaveItemVisibles;
      FExpanded := Value;
      Changed(ticLayout);
      GridView.CheckItemVisibles;
    finally
      GridView.EndUpdate;
    end;
    GridView.RowExpandedChanged(Self);
  end;
end;

procedure TcxGridCardViewRow.SetKind(Value: TcxGridCardViewRowKind);
begin
  if FKind <> Value then
  begin
    GridView.BeginUpdate;
    try
      GridView.SaveItemVisibles;
      FKind := Value;
      GridView.RefreshCategoryRowLinks;
      if Visible then
      begin
        if not CanFocus(nil) then Focused := False;
        Changed(ticSize);
      end
      else
        Changed(ticProperty);
      GridView.CheckItemVisibles;
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridCardViewRow.SetOptions(Value: TcxGridCardViewRowOptions);
begin
  inherited Options := Value;
end;

procedure TcxGridCardViewRow.SetPosition(Value: TcxGridCardViewRowPosition);
begin
  FPosition.Assign(Value);
end;

procedure TcxGridCardViewRow.SetStyles(Value: TcxGridCardViewRowStyles);
begin
  inherited Styles := Value;
end;

function TcxGridCardViewRow.IsCaptionAlignmentHorzStored: Boolean;
begin
  Result := IsHeaderAlignmentHorzStored;
end;

function TcxGridCardViewRow.IsCaptionAlignmentVertStored: Boolean;
begin
  Result := IsHeaderAlignmentVertStored;
end;

function TcxGridCardViewRow.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('BeginsLayer');
  if Expandable then
    AProperties.Add('Expanded');
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxGridCardViewRow.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'BeginsLayer' then
    AValue := Position.BeginsLayer
  else
    if AName = 'Expanded' then
      AValue := Expanded
    else
      inherited;
end;

procedure TcxGridCardViewRow.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'BeginsLayer' then
    Position.BeginsLayer := AValue
  else
    if AName = 'Expanded' then
      Expanded := AValue
    else
      inherited;
end;

procedure TcxGridCardViewRow.CreateSubClasses;
begin
  inherited;
  FPosition := GetPositionClass.Create(Self);
end;

procedure TcxGridCardViewRow.DestroySubClasses;
begin
  FreeAndNil(FPosition);
  inherited;
end;

function TcxGridCardViewRow.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridCardViewRowOptions;
end;

function TcxGridCardViewRow.GetPositionClass: TcxGridCardViewRowPositionClass;
begin
  Result := TcxGridCardViewRowPosition;
end;

function TcxGridCardViewRow.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxGridCardViewRowStyles;
end;

function TcxGridCardViewRow.CanEdit: Boolean;
begin
  Result := inherited CanEdit and Options.ShowData;
end;

function TcxGridCardViewRow.CanExpand: Boolean;
begin
  Result := Expandable and GridView.OptionsCustomize.RowExpanding and Options.Expanding;
end;

function TcxGridCardViewRow.CanFilter(AVisually: Boolean): Boolean;
begin
  Result := inherited CanFilter(AVisually);
  if Result and AVisually then
    Result := Options.ShowData;
end;

function TcxGridCardViewRow.CanFocus(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := inherited CanFocus(ARecord) and (FKind <> rkCaption) and
    ((ARecord = nil) or IsVisibleInCard(TcxGridCard(ARecord)));
end;

procedure TcxGridCardViewRow.ChangeGroupIndex(Value: Integer);
begin
  // do nothing
end;

procedure TcxGridCardViewRow.ChangeScale(M, D: Integer);
begin
  inherited;
  Position.ChangeScale(M, D);
end;

function TcxGridCardViewRow.DefaultAlternateCaption: string;
begin
  Result := inherited DefaultAlternateCaption;
  if GridView.OptionsView.CategoryRowCaptionInRowAlternateCaption and (CategoryRow <> nil) then
    Result := CategoryRow.GetAlternateCaption + cxGridCardViewAlternateCaptionSeparator + Result;
end;

function TcxGridCardViewRow.DefaultWidth: Integer;
begin
  Result := DataBinding.DefaultWidth(False);
end;

function TcxGridCardViewRow.GetActuallyVisible: Boolean;
begin
  Result := inherited GetActuallyVisible;
  if Result and (CategoryRow <> nil) then
    Result := CategoryRow.ActuallyVisible and CategoryRow.Expanded;
end;

function TcxGridCardViewRow.GetExpandable: Boolean;
begin
  Result := FKind = rkCategory;
end;

procedure TcxGridCardViewRow.GetItems(ARows: TList; AIncludeSubItems: Boolean);
begin
  if AIncludeSubItems and (FKind = rkCategory) then
    dxCopyList(CategoryItemsList, ARows)
  else
    ARows.Clear;
  ARows.Insert(0, Self);
end;

function TcxGridCardViewRow.GetVisibleCaption: string;
begin
  Result := inherited GetVisibleCaption;
  if Options.ShowData and (GridView.OptionsView.CaptionSeparator <> #0) then
    Result := Result + GridView.OptionsView.CaptionSeparator;
end;

function TcxGridCardViewRow.HasExpandableItems: Boolean;
begin
  Result := Expandable and (CategoryItemCount <> 0);
end;

procedure TcxGridCardViewRow.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardViewRow then
    with TcxGridCardViewRow(Source) do
    begin
      Self.Expanded := Expanded;
      Self.Kind := Kind;
      Self.Position := Position;
    end;
  inherited;
end;

function TcxGridCardViewRow.HasCardExpandButton: Boolean;
begin
  Result := GridView.OptionsCustomize.CardExpanding and
    (FKind = rkCaption) and (GridView.FirstCaptionRow = Self);
end;

function TcxGridCardViewRow.HasExpandButton: Boolean;
begin
  Result := CanExpand;
end;

function TcxGridCardViewRow.IsVisibleInCard(ACard: TcxGridCard): Boolean;
begin
  Result := ActuallyVisible;
  if Result and (FKind <> rkCaption) then
    Result := ACard.Expanded and
      ((FKind <> rkData) or GridView.OptionsView.EmptyRows or
       ACard.IsEditing or not VarIsNull(ACard.Values[Index]));
end;

procedure TcxGridCardViewRow.MoveTo(AIndex: Integer; AMoveSubItems: Boolean);
var
  ARows: TList;
begin
  if AMoveSubItems then
  begin
    ARows := TList.Create;
    try
      GetItems(ARows, True);
      GridView.RowLayoutController.LayoutObject.MoveRows(ARows, AIndex);
    finally
      ARows.Free;
    end;
  end
  else
    Index := AIndex;
end;

{ TcxGridCardViewBackgroundBitmaps }

function TcxGridCardViewBackgroundBitmaps.GetBitmapStyleIndex(Index: Integer): Integer;
begin
  case Index of
    bbCaptionRow:
      Result := vsCaptionRow;
    bbCardBorder:
      Result := vsCardBorder;
    bbRowCaption:
      Result := vsRowCaption;
  else
    Result := inherited GetBitmapStyleIndex(Index);
  end;
end;

procedure TcxGridCardViewBackgroundBitmaps.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardViewBackgroundBitmaps then
    with TcxGridCardViewBackgroundBitmaps(Source) do
    begin
      Self.CaptionRow := CaptionRow;
      Self.CardBorder := CardBorder;
      Self.RowCaption := RowCaption;
    end;
  inherited;
end;

function TcxGridCardViewBackgroundBitmaps.GetBitmap(Index: Integer): TBitmap;
begin
  Result := inherited GetBitmap(Index);
  if Result = nil then
    case Index of
      bbRowCaption:
        Result := GetBitmap(bbContent);
    end;
end;

{ TcxGridCardViewFiltering }

function TcxGridCardViewFiltering.GetRowAddValueItems: Boolean;
begin
  Result := ItemAddValueItems;
end;

function TcxGridCardViewFiltering.GetRowExcelPopup: TcxGridItemExcelFilterPopupOptions;
begin
  Result := ItemExcelPopup;
end;

function TcxGridCardViewFiltering.GetRowFilteredItemsList: Boolean;
begin
  Result := ItemFilteredItemsList;
end;

function TcxGridCardViewFiltering.GetRowMRUItemsList: Boolean;
begin
  Result := ItemMRUItemsList;
end;

function TcxGridCardViewFiltering.GetRowMRUItemsListCount: Integer;
begin
  Result := ItemMRUItemsListCount;
end;

function TcxGridCardViewFiltering.GetRowPopup: TcxGridItemFilterPopupOptions;
begin
  Result := ItemPopup;
end;

function TcxGridCardViewFiltering.GetRowPopupMode: TdxFilterPopupWindowMode;
begin
  Result := ItemPopupMode;
end;

procedure TcxGridCardViewFiltering.SetRowAddValueItems(Value: Boolean);
begin
  ItemAddValueItems := Value;
end;

procedure TcxGridCardViewFiltering.SetRowExcelPopup(Value: TcxGridItemExcelFilterPopupOptions);
begin
  ItemExcelPopup := Value;
end;

procedure TcxGridCardViewFiltering.SetRowFilteredItemsList(Value: Boolean);
begin
  ItemFilteredItemsList := Value;
end;

procedure TcxGridCardViewFiltering.SetRowMRUItemsList(Value: Boolean);
begin
  ItemMRUItemsList := Value;
end;

procedure TcxGridCardViewFiltering.SetRowMRUItemsListCount(Value: Integer);
begin
  ItemMRUItemsListCount := Value;
end;

procedure TcxGridCardViewFiltering.SetRowPopup(Value: TcxGridItemFilterPopupOptions);
begin
  ItemPopup := Value;
end;

procedure TcxGridCardViewFiltering.SetRowPopupMode(Value: TdxFilterPopupWindowMode);
begin
  ItemPopupMode := Value;
end;

procedure TcxGridCardViewFiltering.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('RowPopupDropDownWidth', ReadItemPopupDropDownWidth, nil, True);
  Filer.DefineProperty('RowPopupMaxDropDownItemCount', ReadItemPopupMaxDropDownCount, nil, True);
end;

{ TcxGridCardViewOptionsBehavior }

constructor TcxGridCardViewOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FExpandRowOnDblClick := True;
  FocusCellOnCycle := True;
  FRowCaptionHints := True;
end;

procedure TcxGridCardViewOptionsBehavior.SetExpandRowOnDblClick(Value: Boolean);
begin
  if FExpandRowOnDblClick <> Value then
  begin
    FExpandRowOnDblClick := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewOptionsBehavior.SetRowCaptionHints(Value: Boolean);
begin
  if FRowCaptionHints <> Value then
  begin
    FRowCaptionHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardViewOptionsBehavior then
    with TcxGridCardViewOptionsBehavior(Source) do
    begin
      Self.ExpandRowOnDblClick := ExpandRowOnDblClick;
      Self.RowCaptionHints := RowCaptionHints;
    end;
  inherited;
end;

{ TcxGridCardViewOptionsCustomize }

constructor TcxGridCardViewOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  RowMoving := False;
  FCardSizing := True;
  FRowExpanding := True;
end;

function TcxGridCardViewOptionsCustomize.GetCardExpanding: Boolean;
begin
  Result := Expandable;
end;

function TcxGridCardViewOptionsCustomize.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewOptionsCustomize.GetRowFiltering: Boolean;
begin
  Result := ItemFiltering;
end;

function TcxGridCardViewOptionsCustomize.GetRowHiding: Boolean;
begin
  Result := ItemHiding;
end;

function TcxGridCardViewOptionsCustomize.GetRowMoving: Boolean;
begin
  Result := ItemMoving;
end;

procedure TcxGridCardViewOptionsCustomize.SetCardExpanding(Value: Boolean);
begin
  Expandable := Value;
end;

procedure TcxGridCardViewOptionsCustomize.SetCardSizing(Value: Boolean);
begin
  if FCardSizing <> Value then
  begin
    FCardSizing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewOptionsCustomize.SetLayeredRows(Value: Boolean);
begin
  if FLayeredRows <> Value then
  begin
    FLayeredRows := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewOptionsCustomize.SetRowExpanding(Value: Boolean);
begin
  if FRowExpanding <> Value then
  begin
    FRowExpanding := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsCustomize.SetRowFiltering(Value: Boolean);
begin
  ItemFiltering := Value;
end;

procedure TcxGridCardViewOptionsCustomize.SetRowHiding(Value: Boolean);
begin
  ItemHiding := Value;
end;

procedure TcxGridCardViewOptionsCustomize.SetRowMoving(Value: Boolean);
begin
  ItemMoving := Value;
end;

procedure TcxGridCardViewOptionsCustomize.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardViewOptionsCustomize then
    with TcxGridCardViewOptionsCustomize(Source) do
    begin
      Self.CardSizing := CardSizing;
      Self.LayeredRows := LayeredRows;
      Self.RowExpanding := RowExpanding;
    end;
  inherited;
end;

{ TcxGridCardViewOptionsSelection }

constructor TcxGridCardViewOptionsSelection.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCardBorderSelection := True;
end;

procedure TcxGridCardViewOptionsSelection.SetCardBorderSelection(Value: Boolean);
begin
  if FCardBorderSelection <> Value then
  begin
    FCardBorderSelection := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridCardViewOptionsSelection.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridCardViewOptionsSelection then
    CardBorderSelection := TcxGridCardViewOptionsSelection(Source).CardBorderSelection;
end;

{ TcxGridCardViewOptionsView }

constructor TcxGridCardViewOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCardBorderWidth := cxGridDefaultCardBorderWidth;
  FCardExpandButtonAlignment := cebaRight;
  FCardWidth := cxGridCardDefaultWidth;
  FCategoryIndent := cxGridCardDefaultCategoryIndent;
  FCategorySeparatorWidth := cxGridCardDefaultCategorySeparatorWidth;
  FEmptyRows := True;
  FLayerSeparatorWidth := cxGridCardDefaultLayerSeparatorWidth;
end;

procedure TcxGridCardViewOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  if CaptionWidth > 0 then
    CaptionWidth := Max(MulDiv(CaptionWidth, M, D), 1);
  CardBorderWidth := MulDiv(CardBorderWidth, M, D);
  LayerSeparatorWidth := MulDiv(LayerSeparatorWidth, M, D);
  CategorySeparatorWidth := MulDiv(CategorySeparatorWidth, M, D);
  CategoryIndent := MulDiv(CategoryIndent, M, D);
  CardWidth := MulDiv(CardWidth, M, D);
end;

function TcxGridCardViewOptionsView.GetCardIndent: Integer;
begin
  Result := Indent;
end;

function TcxGridCardViewOptionsView.GetGridView: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

function TcxGridCardViewOptionsView.GetRowCaptionAutoHeight: Boolean;
begin
  Result := ItemCaptionAutoHeight;
end;

function TcxGridCardViewOptionsView.GetRowCaptionEndEllipsis: Boolean;
begin
  Result := ItemCaptionEndEllipsis;
end;

function TcxGridCardViewOptionsView.GetShowRowFilterButtons: TcxGridShowItemFilterButtons;
begin
  Result := ShowItemFilterButtons;
end;

procedure TcxGridCardViewOptionsView.SetCaptionWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCaptionWidth <> Value then
  begin
    FCaptionWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCardAutoWidth(Value: Boolean);
begin
  if FCardAutoWidth <> Value then
  begin
    FCardAutoWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCardBorderWidth(Value: Integer);
begin
  if Value < cxGridCardBorderMinWidth then Value := cxGridCardBorderMinWidth;
  if FCardBorderWidth <> Value then
  begin
    FCardBorderWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCardExpandButtonAlignment(Value: TcxGridCardExpandButtonAlignment);
begin
  if FCardExpandButtonAlignment <> Value then
  begin
    FCardExpandButtonAlignment := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCardIndent(Value: Integer);
begin
  Indent := Value;
end;

procedure TcxGridCardViewOptionsView.SetCardWidth(Value: Integer);
begin
  if Value < cxGridCardMinWidth then Value := cxGridCardMinWidth;
  if FCardWidth <> Value then
  begin
    FCardWidth := Value;
    Changed(vcSize);
    TcxGridCardView(GridView).Controller.PostGridModeBufferCountUpdate;
  end;
end;

procedure TcxGridCardViewOptionsView.SetCategoryIndent(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCategoryIndent <> Value then
  begin
    FCategoryIndent := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCategoryRowCaptionInRowAlternateCaption(Value: Boolean);
begin
  if FCategoryRowCaptionInRowAlternateCaption <> Value then
  begin
    FCategoryRowCaptionInRowAlternateCaption := Value;
    GridView.ItemCaptionChanged(nil);
  end;
end;

procedure TcxGridCardViewOptionsView.SetCategorySeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FCategorySeparatorWidth <> Value then
  begin
    FCategorySeparatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetEmptyRows(Value: Boolean);
begin
  if FEmptyRows <> Value then
  begin
    if not Value then
      GridView.Controller.FocusedItem := nil;
    FEmptyRows := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetLayerSeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FLayerSeparatorWidth <> Value then
  begin
    FLayerSeparatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardViewOptionsView.SetRowCaptionAutoHeight(Value: Boolean);
begin
  ItemCaptionAutoHeight := Value;
end;

procedure TcxGridCardViewOptionsView.SetRowCaptionEndEllipsis(Value: Boolean);
begin
  ItemCaptionEndEllipsis := Value;
end;

procedure TcxGridCardViewOptionsView.SetShowRowFilterButtons(Value: TcxGridShowItemFilterButtons);
begin
  ShowItemFilterButtons := Value;
end;

procedure TcxGridCardViewOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxGridCardViewOptionsView then
    with TcxGridCardViewOptionsView(Source) do
    begin
      Self.CaptionWidth := CaptionWidth;
      Self.CardAutoWidth := CardAutoWidth;
      Self.CardBorderWidth := CardBorderWidth;
      Self.CardExpandButtonAlignment := CardExpandButtonAlignment;
      Self.CardWidth := CardWidth;
      Self.CategoryIndent := CategoryIndent;
      Self.CategoryRowCaptionInRowAlternateCaption := CategoryRowCaptionInRowAlternateCaption;
      Self.CategorySeparatorWidth := CategorySeparatorWidth;
      Self.EmptyRows := EmptyRows;
      Self.LayerSeparatorWidth := LayerSeparatorWidth;
    end;
  inherited;
end;

{ TcxGridCardViewStyles }

constructor TcxGridCardViewStyles.Create(AOwner: TPersistent);
begin
  inherited;
  BitmapInViewParams := True;
end;

function TcxGridCardViewStyles.GetGridViewValue: TcxGridCardView;
begin
  Result := TcxGridCardView(inherited GridView);
end;

procedure TcxGridCardViewStyles.SetOnGetCaptionRowStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetCaptionRowStyle, Value) then
  begin
    FOnGetCaptionRowStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewStyles.SetOnGetCardBorderStyle(Value: TcxGridGetRecordStyleEvent);
begin
  if not dxSameMethods(FOnGetCardBorderStyle, Value) then
  begin
    FOnGetCardBorderStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewStyles.SetOnGetCategoryRowStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetCategoryRowStyle, Value) then
  begin
    FOnGetCategoryRowStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridCardViewStyles.SetOnGetRowCaptionStyle(Value: TcxGridGetCellStyleEvent);
begin
  if not dxSameMethods(FOnGetRowCaptionStyle, Value) then
  begin
    FOnGetRowCaptionStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

function TcxGridCardViewStyles.GetBackgroundBitmapIndex(Index: Integer): Integer;
begin
  case Index of
    vsCardBorder:
      Result := bbCardBorder;
    vsContent:
      Result := bbContent;
    vsCaptionRow:
      Result := bbCaptionRow;
    vsRowCaption:
      Result := bbRowCaption;
  else
    Result := -1;
  end;
end;

function TcxGridCardViewStyles.GetDefaultBitmap(Index: Integer): TBitmap;
begin
  Index := GetBackgroundBitmapIndex(Index);
  if Index = -1 then
    Result := nil
  else
    Result := GridView.BackgroundBitmaps.GetBitmap(Index);
end;

procedure TcxGridCardViewStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  if Index = vsCardBorder then
    GetRecordContentParams(TcxCustomGridRecord(AData), nil, AParams)
  else
  begin
    inherited;
    with AParams, LookAndFeelPainter do
      case Index of
        vsCaptionRow, vsCategoryRow, vsRowCaption:
          begin
            if AData <> nil then
              with TcxGridDataCellPos(AData) do
                Item.Styles.GetContentParams(GridRecord, AParams);
            case Index of
              vsCaptionRow:
                begin
                  Color := DefaultHeaderColor;
                  TextColor := DefaultHeaderTextColor;
                end;
              vsCategoryRow:
                begin
                  Color := DefaultGroupColor;
                  TextColor := DefaultGroupTextColor;
                end;
            end;
          end;
        vsCategorySeparator:
          Color := DefaultRecordSeparatorColor;
        vsLayerSeparator:
          Color := DefaultSeparatorColor;
      end;
  end;
  AParams.Bitmap := GetDefaultBitmap(Index);
end;

procedure TcxGridCardViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridCardViewStyles then
    with TcxGridCardViewStyles(Source) do
    begin
      Self.CaptionRow := CaptionRow;
      Self.CardBorder := CardBorder;
      Self.CategoryRow := CategoryRow;
      Self.CategorySeparator := CategorySeparator;
      Self.LayerSeparator := LayerSeparator;
      Self.RowCaption := RowCaption;
      Self.OnGetCaptionRowStyle := OnGetCaptionRowStyle;
      Self.OnGetCardBorderStyle := OnGetCardBorderStyle;
      Self.OnGetCategoryRowStyle := OnGetCategoryRowStyle;
      Self.OnGetRowCaptionStyle := OnGetRowCaptionStyle;
    end;
end;

procedure TcxGridCardViewStyles.GetCaptionRowParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCaptionRowStyle) then
    FOnGetCaptionRowStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsCaptionRow, ADataCellPos, AStyle, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridCardViewStyles.GetCardBorderParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCardBorderStyle) then
    FOnGetCardBorderStyle(GridView, ARecord, AStyle);
  GetViewParams(vsCardBorder, ARecord, AStyle, AParams);
end;

procedure TcxGridCardViewStyles.GetCardBorderVisualParams(ARecord: TcxCustomGridRecord;
  out AParams: TcxViewParams);
begin
  if GridView.OptionsSelection.CardBorderSelection and
    GridView.DrawCardBorderSelected(ARecord) then
    GetSelectionParams(ARecord, nil, AParams)
  else
    GetCardBorderParams(ARecord, AParams);
end;

procedure TcxGridCardViewStyles.GetCategoryRowParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetCategoryRowStyle) then
    FOnGetCategoryRowStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsCategoryRow, ADataCellPos, AStyle, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

procedure TcxGridCardViewStyles.GetDataCellContentParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
begin
  if AItem = nil then
    inherited
  else
    case TcxGridCardViewRow(AItem).Kind of
      rkData:
        TcxGridCardViewRow(AItem).Styles.GetCaptionParams(ARecord, AParams);
      rkCaption:
        TcxGridCardViewRow(AItem).Styles.GetCaptionRowParams(ARecord, AParams);
      rkCategory:
        TcxGridCardViewRow(AItem).Styles.GetCategoryRowParams(ARecord, AParams);
    end;
end;

procedure TcxGridCardViewStyles.GetRowCaptionParams(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
  ADataCellPos: TcxGridDataCellPos;
begin
  AStyle := nil;
  if (ARecord <> nil) and Assigned(FOnGetRowCaptionStyle) then
    FOnGetRowCaptionStyle(GridView, ARecord, AItem, AStyle);
  ADataCellPos := TcxGridDataCellPos.Create(ARecord, AItem);
  try
    GetViewParams(vsRowCaption, ADataCellPos, AStyle, AParams);
  finally
    ADataCellPos.Free;
  end;
end;

{ TcxGridCardViewStyleSheet }

function TcxGridCardViewStyleSheet.GetStylesValue: TcxGridCardViewStyles;
begin
  Result := TcxGridCardViewStyles(GetStyles);
end;

procedure TcxGridCardViewStyleSheet.SetStylesValue(Value: TcxGridCardViewStyles);
begin
  SetStyles(Value);
end;

class function TcxGridCardViewStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxGridCardViewStyles;
end;

{ TcxGridCardView }

function TcxGridCardView.GetBackgroundBitmaps: TcxGridCardViewBackgroundBitmaps;
begin
  Result := TcxGridCardViewBackgroundBitmaps(inherited BackgroundBitmaps);
end;

function TcxGridCardView.GetControl: TcxCustomGrid;
begin
  Result := TcxCustomGrid(inherited Control);
end;

function TcxGridCardView.GetController: TcxGridCardViewController;
begin
  Result := TcxGridCardViewController(inherited Controller);
end;

function TcxGridCardView.GetDataController: TcxGridDataController;
begin
  Result := TcxGridDataController(FDataController);
end;

function TcxGridCardView.GetDateTimeHandling: TcxGridCardViewDateTimeHandling;
begin
  Result := TcxGridCardViewDateTimeHandling(inherited DateTimeHandling);
end;

function TcxGridCardView.GetFiltering: TcxGridCardViewFiltering;
begin
  Result := TcxGridCardViewFiltering(inherited Filtering);
end;

function TcxGridCardView.GetFirstCaptionRow: TcxGridCardViewRow;
var
  I: Integer;
begin
  for I := 0 to VisibleRowCount - 1 do
  begin
    Result := VisibleRows[I];
    if Result.Kind = rkCaption then Exit;
  end;
  Result := nil;
end;

function TcxGridCardView.GetFirstCategoryRow: TcxGridCardViewRow;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
  begin
    Result := Rows[I];
    if Result.Kind = rkCategory then Exit;
  end;
  Result := nil;
end;

function TcxGridCardView.GetOptionsBehavior: TcxGridCardViewOptionsBehavior;
begin
  Result := TcxGridCardViewOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridCardView.GetOptionsCustomize: TcxGridCardViewOptionsCustomize;
begin
  Result := TcxGridCardViewOptionsCustomize(inherited OptionsCustomize);
end;

function TcxGridCardView.GetOptionsData: TcxGridCardViewOptionsData;
begin
  Result := TcxGridCardViewOptionsData(inherited OptionsData);
end;

function TcxGridCardView.GetOptionsSelection: TcxGridCardViewOptionsSelection;
begin
  Result := TcxGridCardViewOptionsSelection(inherited OptionsSelection);
end;

function TcxGridCardView.GetOptionsView: TcxGridCardViewOptionsView;
begin
  Result := TcxGridCardViewOptionsView(inherited OptionsView);
end;

function TcxGridCardView.GetPainter: TcxGridCardViewPainter;
begin
  Result := TcxGridCardViewPainter(inherited Painter);
end;

function TcxGridCardView.GetRow(Index: Integer): TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(Items[Index]);
end;

function TcxGridCardView.GetRowCount: Integer;
begin
  Result := ItemCount;
end;

function TcxGridCardView.GetStyles: TcxGridCardViewStyles;
begin
  Result := TcxGridCardViewStyles(inherited Styles);
end;

function TcxGridCardView.GetViewData: TcxGridCardViewViewData;
begin
  Result := TcxGridCardViewViewData(inherited ViewData);
end;

function TcxGridCardView.GetViewInfo: TcxGridCardViewViewInfo;
begin
  Result := TcxGridCardViewViewInfo(inherited ViewInfo);
end;

function TcxGridCardView.GetVisibleRow(Index: Integer): TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(VisibleItems[Index]);
end;

function TcxGridCardView.GetVisibleRowCount: Integer;
begin
  Result := VisibleItemCount;
end;

procedure TcxGridCardView.SetBackgroundBitmaps(Value: TcxGridCardViewBackgroundBitmaps);
begin
  inherited BackgroundBitmaps := Value;
end;

procedure TcxGridCardView.SetDataController(Value: TcxGridDataController);
begin
  FDataController.Assign(Value);
end;

procedure TcxGridCardView.SetDateTimeHandling(Value: TcxGridCardViewDateTimeHandling);
begin
  inherited DateTimeHandling := Value;
end;

procedure TcxGridCardView.SetFiltering(Value: TcxGridCardViewFiltering);
begin
  inherited Filtering := Value;
end;

procedure TcxGridCardView.SetLayoutDirection(Value: TcxGridCardViewLayoutDirection);
begin
  if FLayoutDirection <> Value then
  begin
    FLayoutDirection := Value;
    Controller.LayoutDirectionChanged;
    Changed(vcSize);
  end;
end;

procedure TcxGridCardView.SetOnRowCollapsed(Value: TcxGridCardRowEvent);
begin
  if not dxSameMethods(FOnRowCollapsed, Value) then
  begin
    FOnRowCollapsed := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardView.SetOnRowCollapsing(Value: TcxGridCardRowChangingEvent);
begin
  if not dxSameMethods(FOnRowCollapsing, Value) then
  begin
    FOnRowCollapsing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardView.SetOnRowExpanded(Value: TcxGridCardRowEvent);
begin
  if not dxSameMethods(FOnRowExpanded, Value) then
  begin
    FOnRowExpanded := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardView.SetOnRowExpanding(Value: TcxGridCardRowChangingEvent);
begin
  if not dxSameMethods(FOnRowExpanding, Value) then
  begin
    FOnRowExpanding := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardView.SetOnRowPosChanged(Value: TcxGridCardRowEvent);
begin
  if not dxSameMethods(FOnRowPosChanged, Value) then
  begin
    FOnRowPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridCardView.SetOptionsBehavior(Value: TcxGridCardViewOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxGridCardView.SetOptionsCustomize(Value: TcxGridCardViewOptionsCustomize);
begin
  inherited OptionsCustomize := Value;
end;

procedure TcxGridCardView.SetOptionsData(Value: TcxGridCardViewOptionsData);
begin
  inherited OptionsData := Value;
end;

procedure TcxGridCardView.SetOptionsSelection(Value: TcxGridCardViewOptionsSelection);
begin
  inherited OptionsSelection := Value;
end;

procedure TcxGridCardView.SetOptionsView(Value: TcxGridCardViewOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxGridCardView.SetRow(Index: Integer; Value: TcxGridCardViewRow);
begin
  Items[Index] := Value;
end;

procedure TcxGridCardView.SetRowLayout(Value: TcxGridCardViewRowLayout);
var
  ACoordinates: TcxGridCardRowCoordinatesArray;
begin
  if FRowLayout <> Value then
  begin
    ACoordinates := GetRowCoordinates;
    FRowLayout := Value;
    SetRowCoordinates(ACoordinates);
  end;
end;

procedure TcxGridCardView.SetStyles(Value: TcxGridCardViewStyles);
begin
  inherited Styles := Value;
end;

procedure TcxGridCardView.RefreshCategoryRowLinks;
var
  ACategoryRow, ARow: TcxGridCardViewRow;
  I: Integer;
begin
  ACategoryRow := nil;
  for I := 0 to RowCount - 1 do
  begin
    ARow := Rows[I];
    if ARow.Kind = rkCategory then
    begin
      ACategoryRow := ARow;
      ARow.SetCategoryRowValue(nil);
    end
    else
      ARow.SetCategoryRowValue(ACategoryRow);
  end;
end;

function TcxGridCardView.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('CardWidth');
  Result := inherited GetProperties(AProperties);
end;

procedure TcxGridCardView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'CardWidth' then
    AValue := OptionsView.CardWidth
  else
    inherited;
end;

procedure TcxGridCardView.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'CardWidth' then
   OptionsView.CardWidth := AValue
  else
    inherited;
end;

procedure TcxGridCardView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
  inherited;
  OptionsView.CardWidth := (ALayoutView as TcxGridCardView).OptionsView.CardWidth;
end;

function TcxGridCardView.GetLayoutCustomizationFormButtonCaption: string;
begin
  Result := 'Rows Customization...';
end;

procedure TcxGridCardView.CreateHandlers;
begin
  FRowLayoutController := GetRowLayoutControllerClass.Create(Self);
  inherited;
end;

procedure TcxGridCardView.DestroyHandlers;
begin
  inherited;
  FreeAndNil(FRowLayoutController);
end;

function TcxGridCardView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridCardViewController;
end;

function TcxGridCardView.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxGridDataController;
end;

function TcxGridCardView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridCardViewPainter;
end;

function TcxGridCardView.GetRowLayoutControllerClass: TcxGridCardViewRowLayoutControllerClass;
begin
  Result := TcxGridCardViewRowLayoutController;
end;

function TcxGridCardView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridCardViewViewData;
end;

function TcxGridCardView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridCardViewViewInfo;
end;

procedure TcxGridCardView.SaveRowParams;
var
  I: Integer;
begin
  for I := 0 to RowCount - 1 do
    Rows[I].Position.SaveParams;
end;

procedure TcxGridCardView.AssignRowParams;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to RowCount - 1 do
      Rows[I].Position.AssignParams;
  finally
    EndUpdate;
  end;
end;

function TcxGridCardView.GetRowCoordinates: TcxGridCardRowCoordinatesArray;
var
  I: Integer;
begin
  SetLength(Result, RowCount);
  for I := 0 to RowCount - 1 do
    Result[I] := RowLayoutController.GetCoordinates(Rows[I]);
end;

procedure TcxGridCardView.SetRowCoordinates(const ACoordinates: TcxGridCardRowCoordinatesArray);
var
  ARows: TList;
  I: Integer;
begin
  ARows := TList.Create;
  BeginUpdate;
  try
    dxCopyList(ItemsList, ARows);
    for I := 0 to ARows.Count - 1 do
      RowLayoutController.SetCoordinates(TcxGridCardViewRow(ARows[I]), ACoordinates[I]);
  finally
    EndUpdate;
    ARows.Free;
  end;
end;

procedure TcxGridCardView.Updating;
begin
  SaveRowParams;
  inherited;
end;

procedure TcxGridCardView.Updated;
begin
  inherited;
  AssignRowParams;
end;

procedure TcxGridCardView.BeforeRestoring;
begin
  SaveRowParams;
  inherited;
end;

procedure TcxGridCardView.AfterRestoring;
begin
  inherited;
  AssignRowParams;
end;

procedure TcxGridCardView.ChangeItemIndex(AItem: TcxCustomGridTableItem;
  Value: Integer);
begin
  SaveItemVisibles;
  inherited;
  CheckItemVisibles;
end;

procedure TcxGridCardView.DoAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxGridCardView then
    with TcxGridCardView(ASource) do
    begin
      Self.LayoutDirection := LayoutDirection;
      Self.RowLayout := RowLayout;
      Self.OnRowExpanded := OnRowExpanded;
      Self.OnRowExpanding := OnRowExpanding;
      Self.OnRowCollapsed := OnRowCollapsed;
      Self.OnRowCollapsing := OnRowCollapsing;
      Self.OnRowPosChanged := OnRowPosChanged;
    end;
  inherited;
end;

function TcxGridCardView.GetResizeOnBoundsChange: Boolean;
begin
  Result := True;
end;

procedure TcxGridCardView.ItemIndexChanged(AItem: TcxCustomGridTableItem;
  AOldIndex: Integer);
begin
  inherited;
  RefreshCategoryRowLinks;
  RowLayoutController.RowIndexChanged(TcxGridCardViewRow(AItem), AOldIndex);
end;

procedure TcxGridCardView.ItemVisibilityChanged(AItem: TcxCustomGridTableItem;
  Value: Boolean);
begin
  if not Value and AItem.Focused and (TcxGridCardViewRow(AItem).CategoryRow <> nil) then
    TcxGridCardViewRow(AItem).CategoryRow.Focused := True;
  inherited;
end;

function TcxGridCardView.IsRecordPixelScrolling: Boolean;
begin
  Result := False;
end;

procedure TcxGridCardView.RefreshVisibleItemsList;
begin
  inherited;
  RowLayoutController.VisibleRowsListChanged;
end;

procedure TcxGridCardView.RowExpandedChanged(ARow: TcxGridCardViewRow);
begin
  Controller.RowExpandedChanged(ARow);
  if ARow.Expanded then
    DoRowExpanded(ARow)
  else
    DoRowCollapsed(ARow);
end;

function TcxGridCardView.RowExpandedChanging(ARow: TcxGridCardViewRow; AValue: Boolean): Boolean;
begin
  if AValue then
    Result := DoRowExpanding(ARow)
  else
    Result := DoRowCollapsing(ARow);
  if Result then
    Controller.RowExpandedChanging(ARow, AValue);
end;

function TcxGridCardView.CalculateDataCellSelected(ARecord: TcxCustomGridRecord;
  AItem: TcxCustomGridTableItem; AUseViewInfo: Boolean; ACellViewInfo: TcxGridTableCellViewInfo): Boolean;
begin
  if TcxGridCardViewRow(AItem).Kind = rkCaption then
    Result := inherited CalculateDataCellSelected(ARecord, AItem, AUseViewInfo, ACellViewInfo)
  else
    Result := DrawRecordFocused(ARecord) and AItem.Focused;
end;

function TcxGridCardView.DrawCardBorderSelected(ARecord: TcxCustomGridRecord): Boolean;
begin
  Result := DrawRecordSelected(ARecord) and DrawSelection;
end;

function TcxGridCardView.GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass;
begin
  Result := TcxGridCardViewBackgroundBitmaps;
end;

function TcxGridCardView.GetDateTimeHandlingClass: TcxCustomGridTableDateTimeHandlingClass;
begin
  Result := TcxGridCardViewDateTimeHandling;
end;

function TcxGridCardView.GetFilteringClass: TcxCustomGridTableFilteringClass;
begin
  Result := TcxGridCardViewFiltering;
end;

function TcxGridCardView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridCardViewRow;
end;

function TcxGridCardView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridCardViewOptionsBehavior;
end;

function TcxGridCardView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxGridCardViewOptionsCustomize;
end;

function TcxGridCardView.GetOptionsDataClass: TcxCustomGridOptionsDataClass;
begin
  Result := TcxGridCardViewOptionsData;
end;

function TcxGridCardView.GetOptionsSelectionClass: TcxCustomGridOptionsSelectionClass;
begin
  Result := TcxGridCardViewOptionsSelection;
end;

function TcxGridCardView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridCardViewOptionsView;
end;

function TcxGridCardView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridCardViewStyles;
end;

function TcxGridCardView.SupportsCardSizing: Boolean;
begin
  Result := OptionsCustomize.CardSizing;
end;

function TcxGridCardView.SupportsLayeredRows: Boolean;
begin
  Result := IsDesigning or OptionsCustomize.LayeredRows;
end;

procedure TcxGridCardView.DoRowCollapsed(ARow: TcxGridCardViewRow);
begin
  if Assigned(FOnRowCollapsed) then FOnRowCollapsed(Self, ARow);
end;

function TcxGridCardView.DoRowCollapsing(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := True;
  if Assigned(FOnRowCollapsing) then FOnRowCollapsing(Self, ARow, Result);
end;

procedure TcxGridCardView.DoRowExpanded(ARow: TcxGridCardViewRow);
begin
  if Assigned(FOnRowExpanded) then FOnRowExpanded(Self, ARow);
end;

function TcxGridCardView.DoRowExpanding(ARow: TcxGridCardViewRow): Boolean;
begin
  Result := True;
  if Assigned(FOnRowExpanding) then FOnRowExpanding(Self, ARow, Result);
end;

procedure TcxGridCardView.DoRowPositionChanged(ARow: TcxGridCardViewRow);
begin
  if Assigned(FOnRowPosChanged) then FOnRowPosChanged(Self, ARow);
end;

function TcxGridCardView.CreateRow: TcxGridCardViewRow;
begin
  Result := TcxGridCardViewRow(CreateItem);
end;

initialization
  cxGridRegisteredViews.Register(TcxGridCardView, 'Cards');
  Classes.RegisterClasses([TcxGridCardViewRow, TcxGridCardViewStyleSheet]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridCardView);

end.

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

unit cxGridBandedTableView;

{$I cxVer.inc}

interface

uses
  Types, Variants,
  Windows, Classes, Graphics, Controls, Forms, ComCtrls, Menus, dxCoreClasses,
  dxCore, cxClasses, cxControls, cxGraphics, cxLookAndFeels, cxLookAndFeelPainters,
  cxGeometry, cxStyles, cxStorage, cxPC, dxUIElementPopupWindow,
  cxGridCommon, cxGridCustomView, cxGridCustomTableView, cxGridTableView, cxGridRows, cxListBox;

const
  cxGridBandedTableViewAlternateCaptionSeparator: string = '-';

  cxGridDefaultFixedBandSeparatorWidth = 2;
  cxGridDefaultEmptyBandWidth = 50;

  htBandedGridBase = htGridBase + 50;
  htColumnHeaderVertSizingEdge = htBandedGridBase + 1;
  htBand = htBandedGridBase + 2;
  htBandHeader = htBandedGridBase + 3;
  htBandHeaderSizingEdge = htBandedGridBase + 4;
  htIndicatorBandHeader = htBandedGridBase + 5;

  bbBandedTableFirst = bbTableLast + 1;
  bbBandBackground = bbBandedTableFirst;
  bbBandHeader = bbBandedTableFirst + 1;
  bbBandedTableLast = bbBandHeader;

  bsFirst = 0;
  bsBackground = bsFirst;
  bsContent = bsFirst + 1;
  bsHeader = bsFirst + 2;
  bsLast = bsHeader;

  vsBandedTableFirst = vsTableLast + 1;
  vsBandBackground = vsBandedTableFirst;
  vsBandHeader = vsBandedTableFirst + 1;
  vsBandedTableLast = vsBandHeader;

type
  TcxGridBandedTableCustomizationForm = class;
  TcxGridBandedTableController = class;
  TcxGridBandedColumnContainerZone = class;
  TcxGridBandHeaderViewInfo = class;
  TcxGridIndicatorBandHeaderItemViewInfo = class;
  TcxGridBandRowViewInfo = class;
  TcxGridBandRowsViewInfo = class;
  TcxGridBandViewInfo = class;
  TcxGridBandsViewInfo = class;
  TcxGridBandedHeaderViewInfo = class;
  TcxGridBandedDataRowCellsAreaItemViewInfo = class;
  TcxGridBandedDataRowCellsAreaViewInfo = class;
  TcxGridBandedRowsViewInfo = class;
  TcxGridBandedTableViewInfo = class;
  TcxGridBandedColumnPosition = class;
  TcxGridBandedColumn = class;
  TcxGridBandRow = class;
  TcxGridBandRows = class;
  TcxGridBand = class;
  TcxGridBands = class;
  TcxGridBandedTableView = class;

  TcxGridBandFixedKind = (fkNone, fkLeft, fkRight);

  { hit tests }

  TcxGridColumnHeaderVertSizingEdgeHitTest = class(TcxCustomGridColumnHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridBandContainerKind = (bcHeader, bcCustomizationForm);

  TcxGridBandHitTest = class(TcxCustomGridViewHitTest)
  private
    FBand: TcxGridBand;
    FBandContainerKind: TcxGridBandContainerKind;
    FVisibleRowIndex: Integer;
  protected
    procedure Assign(Source: TcxCustomGridHitTest); override;
    class function GetHitTestCode: Integer; override;
  public
    property Band: TcxGridBand read FBand write FBand;
    property BandContainerKind: TcxGridBandContainerKind read FBandContainerKind
      write FBandContainerKind;
    property VisibleRowIndex: Integer read FVisibleRowIndex write FVisibleRowIndex;
  end;

  TcxGridBandHeaderHitTest = class(TcxGridBandHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridBandHeaderSizingEdgeHitTest = class(TcxGridBandHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  public
    function Cursor: TCursor; override;
    function DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass; override;
  end;

  TcxGridIndicatorBandHeaderHitTest = class(TcxGridIndicatorHitTest)
  protected
    class function GetHitTestCode: Integer; override;
  end;

  { controller }

  // drag & drop objects

  TcxGridBandedColumnContainerZone = class(TcxGridItemContainerZone)
  public
    Band: TcxGridBand;
    ColIndex: Integer;
    RowIndex: Integer;
    constructor Create(AColumnIndex: Integer;
      ABand: TcxGridBand; AColIndex, ARowIndex: Integer);
    function IsEqual(Value: TcxGridItemContainerZone): Boolean; override;
    function IsInsertion: Boolean;
  end;

  TcxGridBandedColumnHeaderMovingObject = class(TcxGridColumnHeaderMovingObject)
  private
    function GetDestZone: TcxGridBandedColumnContainerZone;
    function GetSourceItem: TcxGridBandedColumn;
    function GetSourcePosition: TcxGridBandedColumnPosition;
    function GetViewInfo: TcxGridBandedTableViewInfo;
    procedure SetSourceItem(Value: TcxGridBandedColumn);
  protected
    function AreArrowsVertical: Boolean; override;
    procedure CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind); override;
    procedure DoColumnMovingToHeader; override;
    function GetArrowAreaBoundsForHeader(APlace: TcxGridArrowPlace): TRect; override;
    function GetArrowsClientRect: TRect; override;
    function IsValidDestinationForVisibleSource: Boolean; override;
    property DestZone: TcxGridBandedColumnContainerZone read GetDestZone;
    property SourceItem: TcxGridBandedColumn read GetSourceItem write SetSourceItem;
    property SourcePosition: TcxGridBandedColumnPosition read GetSourcePosition;
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
  end;

  TcxGridBandHeaderMovingObject = class(TcxCustomGridTableMovingObject)
  private
    FDestBand: TcxGridBand;
    FDestBandContainerKind: TcxGridBandContainerKind;
    FDestInsertPosition: TcxPosition;
    FIsEmptyViewInsert: Boolean;
    FSourceBandContainerKind: TcxGridBandContainerKind;
    function GetController: TcxGridBandedTableController;
    function GetCustomizationForm: TcxGridBandedTableCustomizationForm;
    function GetGridView: TcxGridBandedTableView;
    function GetSourceBand: TcxGridBand;
    function GetViewInfo: TcxGridBandedTableViewInfo;
    procedure SetDestBand(Value: TcxGridBand);
    procedure SetDestBandContainerKind(Value: TcxGridBandContainerKind);
    procedure SetDestInsertPosition(Value: TcxPosition);
    procedure SetIsEmptyViewInsert(Value: Boolean);
    procedure SetSourceBand(Value: TcxGridBand);
  protected
    function AreArrowsVertical: Boolean; override;
    function CalculateIsEmptyViewInsert(ACheckMousePos: Boolean): Boolean;
    function CanRemove: Boolean; override;
    procedure CheckDestParams; virtual;
    function GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect; override;
    function GetArrowsClientRect: TRect; override;
    function GetCustomizationFormListBox: TcxCustomGridItemsListBox; override;
    function GetSourceItemViewInfo: TcxCustomGridCellViewInfo; override;
    function IsSourceCustomizationForm: Boolean; override;
    function IsValidDestination: Boolean; override;

    procedure BeginDragAndDrop; override;
    procedure DragAndDrop(const P: TPoint; var Accepted: Boolean); override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    property Controller: TcxGridBandedTableController read GetController;
    property CustomizationForm: TcxGridBandedTableCustomizationForm read GetCustomizationForm;
    property DestBand: TcxGridBand read FDestBand write SetDestBand;
    property DestBandContainerKind: TcxGridBandContainerKind read FDestBandContainerKind
      write SetDestBandContainerKind;
    property DestInsertPosition: TcxPosition read FDestInsertPosition
      write SetDestInsertPosition;
    property GridView: TcxGridBandedTableView read GetGridView;
    property IsEmptyViewInsert: Boolean read FIsEmptyViewInsert write SetIsEmptyViewInsert;
    property SourceBand: TcxGridBand read GetSourceBand write SetSourceBand;
    property SourceBandContainerKind: TcxGridBandContainerKind read FSourceBandContainerKind
      write FSourceBandContainerKind;
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  TcxGridColumnVertSizingObject = class(TcxCustomGridColumnSizingObject)
  private
    function GetColumn: TcxGridBandedColumn;
    function GetController: TcxGridBandedTableController;
    function GetLineHeight: Integer;
  protected
    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function GetCurrentSize: Integer; override;
    function GetIsHorizontalSizing: Boolean; override;
    property Column: TcxGridBandedColumn read GetColumn;
    property Controller: TcxGridBandedTableController read GetController;
    property LineHeight: Integer read GetLineHeight;
  end;

  TcxGridBandSizingObject = class(TcxCustomGridSizingObject)
  private
    FBand: TcxGridBand;
    function GetBandViewInfo: TcxGridBandViewInfo;
    function GetController: TcxGridBandedTableController;
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure BeginDragAndDrop; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;

    function GetCurrentSize: Integer; override;
    function GetSizingItemBounds: TRect; override;
    function GetSizingMarkWidth: Integer; override;

    property Band: TcxGridBand read FBand write FBand;
    property BandViewInfo: TcxGridBandViewInfo read GetBandViewInfo;
    property Controller: TcxGridBandedTableController read GetController;
    property GridView: TcxGridBandedTableView read GetGridView;
  public
    procedure Init(const P: TPoint; AParams: TcxCustomGridHitTest); override;
  end;

  // customization form

  TcxGridBandedTableBandsListBoxClass = class of TcxGridBandedTableBandsListBox;

  TcxGridBandedTableBandsListBox = class(TcxGridTableItemsListBox)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure DoRefreshItems; override;
    function DrawItemDrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    function GetDragAndDropParams: TcxCustomGridHitTest; override;
    function GetItemEndEllipsis: Boolean; override;
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

  TcxGridBandedTableCustomizationForm = class(TcxGridTableCustomizationForm)
  private
    FBandsListBox: TcxGridBandedTableBandsListBox;
    FBandsPage: TcxTabSheet;
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure CreateControls; override;
    procedure InitPageControl; override;

    function GetBandsListBoxClass: TcxGridBandedTableBandsListBoxClass; virtual;

    property BandsListBox: TcxGridBandedTableBandsListBox read FBandsListBox;
    property GridView: TcxGridBandedTableView read GetGridView;
  public
    procedure RefreshData; override;
    property BandsPage: TcxTabSheet read FBandsPage;
  end;

  // popup

  TcxGridBandsCustomizationPopupClass = class of TcxGridBandsCustomizationPopup;

  TcxGridBandsCustomizationPopup = class(TcxCustomGridCustomizationPopup)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure AddCheckListBoxItems; override;
    function CanAddCommands: Boolean; override;
    function CanMoveItem(AItem: Pointer): Boolean; override;
    function GetDropDownCount: Integer; override;
    function IsCheckListBoxSorted: Boolean; override;
    function IsMultiColumnMode: Boolean; override;
    function IsGridItem(AItem: TObject): Boolean; override;
    function IsVisibleGridItem(AItem: TObject): Boolean; override;
    procedure SetQuickCustomizationSorted(AValue: Boolean); override;
    function SupportsItemMoving: Boolean; override;
    procedure SynchronizeTableItemsVisibilityWithListBox; override;

    procedure DoItemPosChanged(AItem: TObject); override;
    procedure HandleItemClicked(AItem: TObject; AChecked: Boolean); override;
    function GetItemIndex(AItem: TObject): Integer; override;
    procedure SetItemIndex(AItem: TObject; AIndex: Integer); override;
  public
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

  { TcxGridBandColumnsCustomizationPopup }

  TcxGridBandedColumnsCustomizationPopup = class(TcxGridColumnsCustomizationPopup)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    procedure ApplyCheckListBoxSorting; override;
  public
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

  // controller

  TcxGridBandedTableController = class(TcxGridTableController)
  private
    FBandsCustomizationPopup: TcxGridBandsCustomizationPopup;
    FForcingWidthBand: TcxGridBand;
    FMovingBand: TcxGridBand;
    FPressedBand: TcxGridBand;
    FSizingBand: TcxGridBand;
    FVertSizingColumn: TcxGridBandedColumn;
    function GetBandsCustomizationPopup: TcxGridBandsCustomizationPopup;
    function GetGridView: TcxGridBandedTableView;
    function GetIsBandMoving: Boolean;
    function GetIsBandSizing: Boolean;
    function GetIsColumnVertSizing: Boolean;
    function GetViewInfo: TcxGridBandedTableViewInfo;
    procedure SetPressedBand(Value: TcxGridBand);
  protected
    function FindNextItemFollowVisualOrder(AFocusedItemIndex: Integer; AGoForward, AGoOnCycle: Boolean;
      out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer; virtual;
    function FindNextItemTabOrder(AFocusedItemIndex: Integer; AGoForward, AGoOnCycle: Boolean;
      out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer; virtual;
    function GetBandsCustomizationPopupClass: TcxGridBandsCustomizationPopupClass; virtual;
    function GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass; override;
    function GetColumnHeaderDragAndDropObjectClass: TcxGridColumnHeaderMovingObjectClass; override;
    procedure GetColumnNeighbors(AColumn: TcxGridBandedColumn; AGoForward: Boolean;
      AList: TList); virtual;
    function GetCustomizationFormClass: TcxCustomGridCustomizationFormClass; override;
    function GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean; override;
    function GetPatternObject(AObject: TPersistent): TPersistent; override;
    function IsBandFixedDuringSizing(ABand: TcxGridBand): Boolean; virtual;
    function IsColumnFixedDuringHorzSizing(AColumn: TcxGridColumn): Boolean; override;
    procedure LeftPosChanged; override;
    procedure PopulateTabOrderList(AList: TList);

    //design
    procedure CreateGridViewItem(Sender: TObject); override;
    procedure CreateChildBandForSelection(Sender: TObject); virtual;
    procedure CreateParentBandForSelection(Sender: TObject); virtual;
    procedure DeleteGridViewItem(AItem: TPersistent); override;
    procedure PopulateColumnHeaderDesignPopupMenu(AMenu: TPopupMenu); override;

    // cells selection
    function GetCellMultiSelect: Boolean; override;

    property ForcingWidthBand: TcxGridBand read FForcingWidthBand write FForcingWidthBand;
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
  public
    destructor Destroy; override;
    procedure DoCancelMode; override;
    procedure EndDragAndDrop(Accepted: Boolean); override;
    function HasBandsCustomizationPopup: Boolean;

    function FindNextColumnVertically(AFocusedItemIndex: Integer;
      AGoForward, AGoOnCycle: Boolean): Integer; virtual;
    function FindNextItem(AFocusedItemIndex: Integer;
      AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean; out ACycleChanged: Boolean;
      ARecord: TcxCustomGridRecord): Integer; override;
    function FocusNextColumnVertically(AFocusedColumnIndex: Integer;
      AGoForward, AGoOnCycle: Boolean): Boolean;

    procedure KeyDown(var Key: Word; Shift: TShiftState); override;

    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;

    property BandsCustomizationPopup: TcxGridBandsCustomizationPopup read GetBandsCustomizationPopup;
    property GridView: TcxGridBandedTableView read GetGridView;
    property IsBandMoving: Boolean read GetIsBandMoving;
    property IsBandSizing: Boolean read GetIsBandSizing;
    property IsColumnVertSizing: Boolean read GetIsColumnVertSizing;
    property MovingBand: TcxGridBand read FMovingBand;
    property PressedBand: TcxGridBand read FPressedBand write SetPressedBand;
    property SizingBand: TcxGridBand read FSizingBand;
    property VertSizingColumn: TcxGridBandedColumn read FVertSizingColumn;
  end;

  { painters }

  TcxGridBandHeaderPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridBandHeaderViewInfo;
  protected
    procedure DrawBorders; override;
    procedure DrawContent; override;
    procedure DrawPressed; virtual;
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;
    property ViewInfo: TcxGridBandHeaderViewInfo read GetViewInfo;
  end;

  TcxGridBandPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridBandViewInfo;
  protected
    procedure DoExcludeFromClipRect; override;
    procedure DrawChildBands; virtual;
    procedure DrawColumnHeaders; virtual;
    procedure DrawContent; override;
    procedure DrawHeader; virtual;
    function ExcludeFromClipRect: Boolean; override;
    property ViewInfo: TcxGridBandViewInfo read GetViewInfo;
  end;

  TcxGridBandedHeaderPainter = class(TcxGridHeaderPainter)
  private
    function GetViewInfo: TcxGridBandedHeaderViewInfo;
  protected
    procedure DrawBands; virtual;
    procedure DrawItems; override;
    property ViewInfo: TcxGridBandedHeaderViewInfo read GetViewInfo;
  end;

  TcxGridBandedFooterPainter = class(TcxGridFooterPainter)
  private
    function GetBandsViewInfo: TcxGridBandsViewInfo; inline;
  protected
    procedure DrawItems; override;
    property BandsViewInfo: TcxGridBandsViewInfo read GetBandsViewInfo;
  end;

  TcxGridIndicatorBandHeaderItemPainter = class(TcxGridIndicatorHeaderItemPainter)
  private
   function GetViewInfo: TcxGridIndicatorBandHeaderItemViewInfo;
  protected
    function DrawBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    property ViewInfo: TcxGridIndicatorBandHeaderItemViewInfo read GetViewInfo;
  end;

  TcxGridBandedDataRowCellsAreaItemPainter = class(TcxCustomGridCellPainter)
  private
    function GetViewInfo: TcxGridBandedDataRowCellsAreaItemViewInfo;
  protected
    procedure DrawFixedBandsSeparator; virtual;
    procedure DrawContent; override;
    procedure DrawLines; virtual;
    function ExcludeFromClipRect: Boolean; override;
    procedure Paint; override;
    property ViewInfo: TcxGridBandedDataRowCellsAreaItemViewInfo read GetViewInfo;
  end;

  TcxGridBandedRowsPainter = class(TcxGridRowsPainter)
  public
    class procedure DrawDataRowCells(ARowViewInfo: TcxCustomGridRowViewInfo); override;
  end;

  TcxGridBandedTablePainter = class(TcxGridTablePainter)
  private
    function GetViewInfo: TcxGridBandedTableViewInfo;
  protected
    function CanOffset(AItemsOffset, DX, DY: Integer): Boolean; override;
  public
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
  end;

  { view infos }

  // column container

  // column header

  TcxGridBandedColumnHeaderVertSizingEdgeViewInfo = class(TcxGridColumnHeaderAreaViewInfo)
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function OccupiesSpace: Boolean; override;
    function ResidesInContent: Boolean; override;
  public
    procedure Calculate(const ABounds: TRect; var ATextAreaBounds: TRect); override;
  end;

  TcxGridBandedColumnHeaderViewInfo = class(TcxGridColumnHeaderViewInfo)
  private
    function GetBandViewInfo: TcxGridBandViewInfo;
    function GetColumn: TcxGridBandedColumn;
    function GetContainer: TcxGridBandedHeaderViewInfo;
    function GetRowViewInfo: TcxGridBandRowViewInfo;
  protected
    function CanVertSize: Boolean; virtual;
    procedure GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc); override;
    function GetMaxWidth: Integer; override;

    property BandViewInfo: TcxGridBandViewInfo read GetBandViewInfo;
    property Container: TcxGridBandedHeaderViewInfo read GetContainer;
    property RowViewInfo: TcxGridBandRowViewInfo read GetRowViewInfo;
  public
    property Column: TcxGridBandedColumn read GetColumn;
  end;

  // bands

  TcxGridBandHeaderViewInfoClass = class of TcxGridBandHeaderViewInfo;

  TcxGridBandHeaderViewInfo = class(TcxCustomGridViewCellViewInfo)
  private
    FBandViewInfo: TcxGridBandViewInfo;
    function GetBand: TcxGridBand;
    function GetVerticalBorderOverlapSize: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetRowCount: Integer;
  protected
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CanShowHint: Boolean; override;
    function CustomDraw(ACanvas: TcxCanvas): Boolean; override;
    function GetActualState: TcxGridCellState; override;
    function GetAlignmentHorz: TAlignment; override;
    function GetAlignmentVert: TcxAlignmentVert; override;
    function GetAreaBounds: TRect; override;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetCanvas: TcxCanvas; override;
    class function GetCellHeight(ATextHeight: Integer;
      ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; override;
    function GetHeight: Integer; override;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetHotTrack: Boolean; override;
    function GetIsDesignSelected: Boolean; override;
    function GetIsPressed: Boolean; virtual;
    function GetMultiLine: Boolean; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetShowEndEllipsis: Boolean; override;
    function GetSizingEdgeBounds: TRect; virtual;
    function GetText: string; override;
    function GetTextAreaBounds: TRect; override;
    function GetVisible: Boolean; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function HasCustomDraw: Boolean; override;
    function HasDesignPopupMenu: Boolean; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure PopulateDesignPopupMenu(AMenu: TPopupMenu); override;
    property SizingEdgeBounds: TRect read GetSizingEdgeBounds;
  public
    constructor Create(ABandViewInfo: TcxGridBandViewInfo); reintroduce; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function MouseDown(AHitTest: TcxCustomGridHitTest; AButton: TMouseButton;
      AShift: TShiftState): Boolean; override;
    property Band: TcxGridBand read GetBand;
    property BandViewInfo: TcxGridBandViewInfo read FBandViewInfo;
    property GridView: TcxGridBandedTableView read GetGridView;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
    property IsPressed: Boolean read GetIsPressed;
    property RowCount: Integer read GetRowCount;
  end;

  TcxGridBandRowViewInfoClass = class of TcxGridBandRowViewInfo;

  TcxGridBandRowViewInfo = class
  private
    FColumnViewInfos: TList;
    FHeight: Integer;
    FIndex: Integer;
    FIsRightToLeftConverted: Boolean;
    FRowsViewInfo: TcxGridBandRowsViewInfo;
    function GetBandRow: TcxGridBandRow;
    function GetBandViewInfo: TcxGridBandViewInfo;
    function GetColumnViewInfo(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
    function GetColumnViewInfoCount: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetHeight: Integer;
    function GetLineHeight: Integer;
    function GetMinWidth: Integer;
    function GetWidth: Integer;
  protected
    procedure AddColumnViewInfos; virtual;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateColumnWidths; virtual;
    function CalculateHeight: Integer; virtual;
    function CalculateLineHeight: Integer; virtual;
    function CalculateWidth: Integer; virtual;
    property GridView: TcxGridBandedTableView read GetGridView;
    property LineHeight: Integer read GetLineHeight;
  public
    Bounds: TRect;
    constructor Create(ARowsViewInfo: TcxGridBandRowsViewInfo; AIndex: Integer); virtual;
    destructor Destroy; override;
    procedure AssignColumnWidths;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    procedure Offset(DX, DY: Integer); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    property BandRow: TcxGridBandRow read GetBandRow;
    property BandViewInfo: TcxGridBandViewInfo read GetBandViewInfo;
    property ColumnViewInfoCount: Integer read GetColumnViewInfoCount;
    property ColumnViewInfos[Index: Integer]: TcxGridBandedColumnHeaderViewInfo read GetColumnViewInfo; default;
    property Index: Integer read FIndex;
    property MinWidth: Integer read GetMinWidth;
    property RowsViewInfo: TcxGridBandRowsViewInfo read FRowsViewInfo;
    property Height: Integer read GetHeight;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Width: Integer read GetWidth;
  end;

  TcxGridBandRowsViewInfoClass = class of TcxGridBandRowsViewInfo;

  TcxGridBandRowsViewInfo = class
  private
    FBandViewInfo: TcxGridBandViewInfo;
    FIsRightToLeftConverted: Boolean;
    FItems: TList;
    FWidth: Integer;
    function GetBandRows: TcxGridBandRows;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxGridBandRowViewInfo;
    function GetMinWidth: Integer;
    function GetWidth: Integer;
    procedure CreateItems;
    procedure DestroyItems;
  protected
    procedure AssignColumnWidths;
    procedure Calculate(const ABounds: TRect); virtual;
    procedure CalculateColumnWidths; virtual;
    function CalculateWidth: Integer; virtual;
    function GetBandRowViewInfoClass: TcxGridBandRowViewInfoClass; virtual;
  public
    Bounds: TRect;
    constructor Create(ABandViewInfo: TcxGridBandViewInfo); virtual;
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    //function GetColumnsHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function IndexAtPos(const P: TPoint): Integer;
    procedure Offset(DX, DY: Integer); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    property BandRows: TcxGridBandRows read GetBandRows;
    property BandViewInfo: TcxGridBandViewInfo read FBandViewInfo;
    property Count: Integer read GetCount;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Items[Index: Integer]: TcxGridBandRowViewInfo read GetItem; default;
    property MinWidth: Integer read GetMinWidth;
    property Width: Integer read GetWidth;
  end;

  TcxGridBandViewInfoClass = class of TcxGridBandViewInfo;

  TcxGridBandViewInfo = class(TcxCustomGridCellViewInfo)
  private
    FBandsViewInfo: TcxGridBandsViewInfo;
    FBottomIndex: Integer;
    FColumnViewInfos: TList;
    FExcludeBounds: TRect;
    FHeaderViewInfo: TcxGridBandHeaderViewInfo;
    FIndex: Integer;
    FRowsViewInfo: TcxGridBandRowsViewInfo;
    FWidth: Integer;
    function GetBand: TcxGridBand;
    function GetBorderOverlapSize: Integer; inline;
    function GetBoundsForBandInsert: TRect;
    function GetChildBandsHorizontalBorderOverlapSize: Integer;
    function GetChildBandViewInfo(Index: Integer): TcxGridBandViewInfo;
    function GetChildBandViewInfoCount: Integer;
    function GetColumnViewInfo(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
    function GetColumnViewInfoCount: Integer;
    function GetContainerViewInfo: TcxGridBandedHeaderViewInfo;
    function GetFixedKind: TcxGridBandFixedKind;
    function GetGridView: TcxGridBandedTableView;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetIsBottom: Boolean;
    function GetIsFixed: Boolean;
    function GetIsRight: Boolean;
    function GetIsRoot: Boolean;
    function GetParentBandViewInfo: TcxGridBandViewInfo;
    function GetRowCount: Integer;
    function GetSameLevelAutoWidth: Integer;
    function GetSameLevelItem(Index: Integer): TcxGridBandViewInfo;
    function GetSameLevelItemCount: Integer;
  protected
    procedure AddColumnViewInfos; virtual;
    procedure AssignChildBandWidths;
    procedure AssignColumnWidths;
    procedure CalculateChildBandWidths; virtual;
    procedure CalculateChildBands(R: TRect); virtual;
    function CalculateChildBandsBounds: TRect; virtual;
    function CalculateColumnsBounds: TRect; virtual;
    procedure CalculateColumnWidths; virtual;
    function CalculateHeaderBounds: TRect; virtual;
    //function CalculateHeaderHeightWithChildren: Integer;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function CanSize: Boolean; virtual;
    procedure CheckWidth(var Value: Integer); virtual;
    function CustomDrawBackground(ACanvas: TcxCanvas): Boolean; override;
    function GetAreAllColumnsFixed: Boolean; virtual;
    function GetBackgroundBitmap: TBitmap; override;
    function GetBorderColor(AIndex: TcxBorder): TColor; override;
    function GetBorders: TcxBorders; override;
    function GetBorderWidth(AIndex: TcxBorder): Integer; override;
    function GetCanvas: TcxCanvas; override;
    function GetContentWidth: Integer; override;
    class function GetHeaderViewInfoClass: TcxGridBandHeaderViewInfoClass; virtual;
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetMaxContentWidth: Integer; virtual;
    function GetMaxWidth: Integer; virtual;
    function GetMinContentWidth: Integer; virtual;
    function GetMinWidth: Integer; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetRowsViewInfoClass: TcxGridBandRowsViewInfoClass; virtual;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function GetWidth: Integer; override;
    procedure InitHitTest(AHitTest: TcxCustomGridHitTest); override;
    procedure Offset(DX, DY: Integer); override;
    procedure SetWidth(Value: Integer); override;

    property BorderOverlapSize: Integer read GetBorderOverlapSize;
    property BoundsForBandInsert: TRect read GetBoundsForBandInsert;
    property ExcludeBounds: TRect read FExcludeBounds;
  public
    constructor Create(ABandsViewInfo: TcxGridBandsViewInfo; AIndex: Integer); virtual;
    destructor Destroy; override;
    procedure Calculate(const ABounds: TRect); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    procedure InitAutoWidthItem(AAutoWidthItem: TcxAutoWidthItem);
    function InsertPositionAtPos(const P: TPoint; out ABand: TcxGridBand;
      out AInsertPosition: TcxPosition): Boolean;

    property AreAllColumnsFixed: Boolean read GetAreAllColumnsFixed;
    property Band: TcxGridBand read GetBand;
    property BandsViewInfo: TcxGridBandsViewInfo read FBandsViewInfo;
    property BottomIndex: Integer read FBottomIndex;
    property ChildBandViewInfoCount: Integer read GetChildBandViewInfoCount;
    property ChildBandViewInfos[Index: Integer]: TcxGridBandViewInfo read GetChildBandViewInfo;
    property ColumnViewInfoCount: Integer read GetColumnViewInfoCount;
    property ColumnViewInfos[Index: Integer]: TcxGridBandedColumnHeaderViewInfo read GetColumnViewInfo; default;
    property ContainerViewInfo: TcxGridBandedHeaderViewInfo read GetContainerViewInfo;
    property FixedKind: TcxGridBandFixedKind read GetFixedKind;
    property GridView: TcxGridBandedTableView read GetGridView;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
    property HeaderViewInfo: TcxGridBandHeaderViewInfo read FHeaderViewInfo;
    property Index: Integer read FIndex;
    property IsBottom: Boolean read GetIsBottom;
    property IsFixed: Boolean read GetIsFixed;
    property IsRight: Boolean read GetIsRight;
    property IsRoot: Boolean read GetIsRoot;
    property MaxContentWidth: Integer read GetMaxContentWidth;
    property MaxWidth: Integer read GetMaxWidth;
    property MinContentWidth: Integer read GetMinContentWidth;
    property MinWidth: Integer read GetMinWidth;
    property ParentBandViewInfo: TcxGridBandViewInfo read GetParentBandViewInfo;
    property RowCount: Integer read GetRowCount;
    property RowsViewInfo: TcxGridBandRowsViewInfo read FRowsViewInfo;
    property SameLevelItemCount: Integer read GetSameLevelItemCount;
    property SameLevelItems[Index: Integer]: TcxGridBandViewInfo read GetSameLevelItem;
    property SameLevelAutoWidth: Integer read GetSameLevelAutoWidth;
  end;

  TcxGridBandsViewInfoClass = class of TcxGridBandsViewInfo;

  TcxGridBandsViewInfo = class
  private
    FContainerViewInfo: TcxGridBandedHeaderViewInfo;
    FBandHeaderRowHeights: TList;
    FBandHeadersAreaHeight: Integer;
    FIsRightToLeftConverted: Boolean;
    FItems: TList;
    FLineCount: Integer;
    FRowCount: Integer;
    function GetBandHeaderRowCount: Integer;
    function GetBandHeaderRowHeight(Index: Integer): Integer;
    function GetBandHeadersAreaHeight: Integer;
    function GetBandHeadersVerticalBorderOverlapSize: Integer;
    function GetBands: TcxGridBands;
    function GetBorderOverlapSize: Integer; inline;
    function GetBottomItem(Index: Integer): TcxGridBandViewInfo;
    function GetBottomItemCount: Integer;
    function GetColumnHeadersAreaHeight: Integer;
    function GetCount: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetInternalItem(Index: Integer): TcxGridBandViewInfo;
    function GetItem(Index: Integer): TcxGridBandViewInfo;
    function GetLastFixedItem(AFixedKind: TcxGridBandFixedKind; ALevelIndex: Integer): TcxGridBandViewInfo;
    function GetLineCount: Integer;
    function GetRootItem(Index: Integer): TcxGridBandViewInfo;
    function GetRootItemCount: Integer;
    function GetRootItemsHorizontalBorderOverlapSize: Integer;
    function GetRowCount: Integer;
    procedure SetBandHeaderRowHeight(Index: Integer; Value: Integer);
    procedure CreateItems;
    procedure DestroyItems;
  protected
    function AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo; ATopBound: Integer): Boolean; virtual;
    procedure AddAdornerTargetElements(AList: TStrings); virtual;
    procedure Calculate; virtual;
    function CalculateBandHeaderHeight(ABandHeaderViewInfo: TcxGridBandHeaderViewInfo): Integer; virtual;
    procedure CalculateBandHeaderRowHeights; virtual;
    function CalculateBandHeadersAreaHeight: Integer; virtual;
    procedure CalculateColumnWidths; virtual;
    function CalculateHeight: Integer; virtual;
    function CalculateWidth: Integer; virtual;
    function DrawBandHeaderBackgroundHandler(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; virtual;
    function GetBandBackgroundBitmap: TBitmap; virtual;
    function GetBandHeaderBackgroundBitmap: TBitmap; virtual;
    function GetItemClass: TcxGridBandViewInfoClass; virtual;
    function GetRootItemsAreaBounds: TRect;
    function IsBandHeaderHeightAssigned: Boolean; virtual;
    function ShowBandHeaders: Boolean; virtual;
    function ShowColumnHeaders: Boolean; virtual;

    property BandHeadersAreaHeight: Integer read GetBandHeadersAreaHeight;
    property BorderOverlapSize: Integer read GetBorderOverlapSize;
    property ColumnHeadersAreaHeight: Integer read GetColumnHeadersAreaHeight;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
    property InternalItems[Index: Integer]: TcxGridBandViewInfo read GetInternalItem;
  public
    constructor Create(AContainerViewInfo: TcxGridBandedHeaderViewInfo); virtual;
    destructor Destroy; override;
    procedure AssignRootItemWidths;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    //function GetColumnsHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; virtual;
    function GetItemAreaBounds(ABand: TcxGridBand): TRect; virtual;
    procedure InsertPositionAtPos(const P: TPoint; out ABand: TcxGridBand;
      out AInsertPosition: TcxPosition);
    procedure Offset(DX, DY: Integer); virtual;
    procedure RightToLeftConversion(const ABounds: TRect);
    property BandBackgroundBitmap: TBitmap read GetBandBackgroundBitmap;
    property BandHeaderBackgroundBitmap: TBitmap read GetBandHeaderBackgroundBitmap;
    property BandHeaderRowCount: Integer read GetBandHeaderRowCount;
    property BandHeaderRowHeights[Index: Integer]: Integer read GetBandHeaderRowHeight write SetBandHeaderRowHeight;
    property Bands: TcxGridBands read GetBands;
    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItems[Index: Integer]: TcxGridBandViewInfo read GetBottomItem;
    property ContainerViewInfo: TcxGridBandedHeaderViewInfo read FContainerViewInfo;
    property Count: Integer read GetCount;
    property GridView: TcxGridBandedTableView read GetGridView;
    property IsRightToLeftConverted: Boolean read FIsRightToLeftConverted write FIsRightToLeftConverted;
    property Items[Index: Integer]: TcxGridBandViewInfo read GetItem; default;
    property LastFixedItems[AFixedKind: TcxGridBandFixedKind; ALevelIndex: Integer]: TcxGridBandViewInfo read GetLastFixedItem;
    property LineCount: Integer read GetLineCount;
    property RootItemCount: Integer read GetRootItemCount;
    property RootItems[Index: Integer]: TcxGridBandViewInfo read GetRootItem;
    property RowCount: Integer read GetRowCount;
  end;

  // header

  TcxGridBandedHeaderViewInfoSpecific = class(TcxGridHeaderViewInfoSpecific)
  private
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
  protected
    function CalculateHeight: Integer; override;
  public
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
  end;

  TcxGridBandedHeaderViewInfo = class(TcxGridHeaderViewInfo)
  private
    FBandsViewInfo: TcxGridBandsViewInfo;
    function GetGridView: TcxGridBandedTableView;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetItem(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
    function GetLineCount: Integer;
    function GetRowCount: Integer;
    function GetSpecific: TcxGridBandedHeaderViewInfoSpecific;
  protected
    procedure AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo; ATopBound: Integer); override;
    procedure CalculateColumnAutoWidths; override;
    procedure CalculateColumnWidths; override;
    function CalculateHeight: Integer; override;
    procedure CalculateItems; override;
    function GetBandsViewInfoClass: TcxGridBandsViewInfoClass; virtual;
    function GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors; override;
    function GetColumnAdditionalWidth(AColumn: TcxGridColumn): Integer; override;
    function GetColumnWidth(AColumn: TcxGridColumn): Integer; override;
    function GetIsScrollable: Boolean; override;
    function GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect; override;
    function GetItemClass: TcxGridColumnHeaderViewInfoClass; override;
    function GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean; override;
    function GetItemsAreaBounds: TRect; override;
    function GetItemsHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetVisible: Boolean; override;
    function GetWidth: Integer; override;
    function IsHeightAssigned: Boolean; override;
    procedure Offset(DX, DY: Integer); override;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); override;
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetHitTest(const P: TPoint): TcxCustomGridHitTest; override;
    function GetZone(const P: TPoint): TcxGridItemContainerZone; override;
    property BandsViewInfo: TcxGridBandsViewInfo read FBandsViewInfo;
    property GridView: TcxGridBandedTableView read GetGridView;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
    property Items[Index: Integer]: TcxGridBandedColumnHeaderViewInfo read GetItem; default;
    property LineCount: Integer read GetLineCount;
    property RowCount: Integer read GetRowCount;
    property Specific: TcxGridBandedHeaderViewInfoSpecific read GetSpecific;
  end;

  // footer

  TcxGridBandedFooterViewInfo = class(TcxGridFooterViewInfo)
  protected
    function CompareCellData(Item1, Item2: Pointer): Integer;
    function GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean; override;
    function IsColumnOnFirstLayer(AColumnIndex: Integer): Boolean; override;
    function IsMultilayerLayout: Boolean; override;
    procedure Prepare(ACellDataList: TdxFastObjectList); override;
  end;

  // indicator

  TcxGridIndicatorBandHeaderItemViewInfo = class(TcxGridIndicatorHeaderItemViewInfo)
  private
    function GetGridView: TcxGridBandedTableView;
  protected
    function GetHitTestClass: TcxCustomGridHitTestClass; override;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    function GetText: string; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    function SupportsQuickCustomization: Boolean; override;

    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
  public
    property GridView: TcxGridBandedTableView read GetGridView;
  end;

  TcxGridBandedIndicatorViewInfo = class(TcxGridIndicatorViewInfo)
  protected
    function GetAlwaysVisible: Boolean; override;
  end;

  // rows

  TcxGridFixedBandsSeparatorLocation = (slNone, slLeft, slRight);

  TcxGridBandedDataRowCellsAreaItemViewInfoClass = class of TcxGridBandedDataRowCellsAreaItemViewInfo;

  TcxGridBandedDataRowCellsAreaItemViewInfo = class(TcxGridCellViewInfo)
  private
    FBandViewInfo: TcxGridBandViewInfo;
    FCellsAreaViewInfo: TcxGridBandedDataRowCellsAreaViewInfo;
    FLineBounds: TList;
    function GetFixedBandsSeparatorLocation: TcxGridFixedBandsSeparatorLocation;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetLineBounds(Index: Integer): TRect;
    function GetLineCount: Integer;
    function GetRecordsViewInfo: TcxGridBandedRowsViewInfo;
    function GetRecordViewInfo: TcxGridDataRowViewInfo;
    procedure ClearLines;
    procedure CreateLineBounds;
    procedure DestroyLineBounds;
  protected
    procedure AddLine(const ABounds: TRect);
    procedure AddLines; virtual;
    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetBorders: TcxBorders; override;
    function GetFixedBandsSeparatorBounds: TRect; virtual;
    function GetPainterClass: TcxCustomGridCellPainterClass; override;
    procedure GetViewParams(var AParams: TcxViewParams); override;
    procedure Offset(DX, DY: Integer); override;

    property BandViewInfo: TcxGridBandViewInfo read FBandViewInfo;
    property CellsAreaViewInfo: TcxGridBandedDataRowCellsAreaViewInfo read FCellsAreaViewInfo;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
    property RecordViewInfo: TcxGridDataRowViewInfo read GetRecordViewInfo;
    property RecordsViewInfo: TcxGridBandedRowsViewInfo read GetRecordsViewInfo;
  public
    constructor Create(ACellsAreaViewInfo: TcxGridBandedDataRowCellsAreaViewInfo;
      ABandViewInfo: TcxGridBandViewInfo); reintroduce; virtual;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    function CanDrawSelected: Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property FixedBandsSeparatorBounds: TRect read GetFixedBandsSeparatorBounds;
    property FixedBandsSeparatorLocation: TcxGridFixedBandsSeparatorLocation read
      GetFixedBandsSeparatorLocation;
    property LineBounds[Index: Integer]: TRect read GetLineBounds;
    property LineCount: Integer read GetLineCount;
  end;

  TcxGridBandedDataRowCellsAreaViewInfo = class(TcxGridDataRowCellsAreaViewInfo)
  private
    FItems: TList;
    function GetBandsViewInfo: TcxGridBandsViewInfo;
    function GetCount: Integer;
    function GetGridViewInfo: TcxGridBandedTableViewInfo;
    function GetItem(Index: Integer): TcxGridBandedDataRowCellsAreaItemViewInfo;
    procedure CreateItems;
    procedure DestroyItems;
  protected
    function CalculateVisible: Boolean; override;
    function GetItemClass: TcxGridBandedDataRowCellsAreaItemViewInfoClass; virtual;
    procedure Offset(DX, DY: Integer); override;
    property BandsViewInfo: TcxGridBandsViewInfo read GetBandsViewInfo;
    property GridViewInfo: TcxGridBandedTableViewInfo read GetGridViewInfo;
  public
    constructor Create(ARecordViewInfo: TcxCustomGridRecordViewInfo); override;
    destructor Destroy; override;
    procedure BeforeRecalculation; override;
    procedure Calculate(ALeftBound, ATopBound: Integer; AWidth: Integer = -1;
      AHeight: Integer = -1); override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function DrawMergedCells: Boolean; override;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxGridBandedDataRowCellsAreaItemViewInfo read GetItem; default;
  end;

  TcxGridBandedRowsViewInfo = class(TcxGridRowsViewInfo)
  private
    FRowCellsAreaVisible: Boolean;
    function GetHeaderViewInfo: TcxGridBandedHeaderViewInfo;
  protected
    function CalculateDataRowHeight: Integer; override;
    function CalculateRowCellsAreaVisible: Boolean; virtual;
    function GetAreaBoundsForCell(ACellViewInfo: TcxGridTableDataCellViewInfo): TRect; override;
    function GetPainterClass: TcxCustomGridRecordsPainterClass; override;
    function GetRowWidth: Integer; override;
    function GetShowBandSeparators: Boolean; virtual;
    function GetShowCellLeftLines: Boolean; virtual;
    function GetShowCellTopLines: Boolean; virtual;

    property HeaderViewInfo: TcxGridBandedHeaderViewInfo read GetHeaderViewInfo;
    property ShowBandSeparators: Boolean read GetShowBandSeparators;
    property ShowCellLeftLines: Boolean read GetShowCellLeftLines;
    property ShowCellTopLines: Boolean read GetShowCellTopLines;
  public
    procedure AfterConstruction; override;
    function GetDataRowCellsAreaViewInfoClass: TClass; override;
    function IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean; override;
    property RowCellsAreaVisible: Boolean read FRowCellsAreaVisible;
  end;

  // table

  TcxGridBandedTableViewInfo = class(TcxGridTableViewInfo)
  private
    function GetController: TcxGridBandedTableController;
    function GetFixedBandSeparatorColor: TColor;
    function GetFixedBandSeparatorWidth: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetHeaderViewInfo: TcxGridBandedHeaderViewInfo;
  protected
    function GetColumnFooterWidth(AFooterViewInfo: TcxGridFooterViewInfo; AColumn: TcxGridColumn): Integer; override;
    function GetFirstItemAdditionalWidth: Integer; override;
    function GetScrollableAreaBoundsForEdit: TRect; override;
    function GetScrollableAreaBoundsHorz: TRect; override;
    function SupportsAutoHeight: Boolean; override;
    function SupportsGroupSummariesAlignedWithColumns: Boolean; override;
    function SupportsMultipleFooterSummaries: Boolean; override;

    function GetFooterPainterClass: TcxGridFooterPainterClass; override;
    function GetFooterViewInfoClass: TcxGridFooterViewInfoClass; override;
    function GetHeaderViewInfoClass: TcxGridHeaderViewInfoClass; override;
    function GetHeaderViewInfoSpecificClass: TcxGridHeaderViewInfoSpecificClass; override;
    function GetIndicatorViewInfoClass: TcxGridIndicatorViewInfoClass; override;
    function GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass; override;

    property Controller: TcxGridBandedTableController read GetController;
  public
    function CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer): Boolean; override;
    function GetCellHeight(AIndex, ACellHeight: Integer): Integer; override;
    function GetCellTopOffset(AIndex, ACellHeight: Integer): Integer; override;
    property FixedBandSeparatorColor: TColor read GetFixedBandSeparatorColor;
    property FixedBandSeparatorWidth: Integer read GetFixedBandSeparatorWidth;
    property GridView: TcxGridBandedTableView read GetGridView;
    property HeaderViewInfo: TcxGridBandedHeaderViewInfo read GetHeaderViewInfo;
  end;

  { column }

  TcxCustomGridBandedColumnOptions = class(TcxCustomGridColumnOptions)
  private
    FVertSizing: Boolean;
    procedure SetVertSizing(Value: Boolean);
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(AItem: TcxCustomGridTableItem); override;

    property VertSizing: Boolean read FVertSizing write SetVertSizing default True;
  end;

  TcxGridBandedColumnOptions = class(TcxCustomGridBandedColumnOptions)
  published
    property AutoWidthSizable;
    property CellMerging;
    property EditAutoHeight;
    property GroupFooters;
    property Grouping;
    property HorzSizing;
    property Moving;
    property ShowCaption;
    property SortByDisplayText;
    property Sorting;
    property VertSizing;
  end;

  TcxGridBandedColumnStyles = class(TcxGridColumnStyles)
  private
    function GetItem: TcxGridBandedColumn;
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    property Item: TcxGridBandedColumn read GetItem;
  end;

  TcxGridBandedColumnPosition = class(TcxCustomGridTableItemCustomOptions)
  private
    FBand: TcxGridBand;
    FBandIndex: Integer;
    FColIndex: Integer;
    FLineCount: Integer;
    FRow: TcxGridBandRow;
    FRowIndex: Integer;
    FVisibleColIndex: Integer;
    function GetBandIndex: Integer;
    function GetColIndex: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetItem: TcxGridBandedColumn;
    function GetRowIndex: Integer;
    function GetVisibleBandIndex: Integer;
    function GetVisibleRowIndex: Integer;
    procedure SetBand(Value: TcxGridBand);
    procedure SetBandIndex(Value: Integer);
    procedure SetColIndex(Value: Integer);
    procedure SetLineCount(Value: Integer);
    procedure SetRowIndex(Value: Integer);
  protected
    procedure CheckLineCount(var Value: Integer);
    procedure SaveParams(ABandIndexOnly: Boolean = False);
  public
    constructor Create(AItem: TcxCustomGridTableItem); override;
    procedure Assign(Source: TPersistent); override;
    property Band: TcxGridBand read FBand;
    property GridView: TcxGridBandedTableView read GetGridView;
    property Item: TcxGridBandedColumn read GetItem;
    property Row: TcxGridBandRow read FRow;
    property VisibleBandIndex: Integer read GetVisibleBandIndex;
    property VisibleColIndex: Integer read FVisibleColIndex;
    property VisibleRowIndex: Integer read GetVisibleRowIndex;
  published
    property BandIndex: Integer read GetBandIndex write SetBandIndex;
    property ColIndex: Integer read GetColIndex write SetColIndex;
    property LineCount: Integer read FLineCount write SetLineCount default 1;
    property RowIndex: Integer read GetRowIndex write SetRowIndex;
  end;

  TcxGridBandedColumn = class(TcxGridColumn)
  private
    FPosition: TcxGridBandedColumnPosition;
    function GetGridView: TcxGridBandedTableView;
    function GetOptions: TcxGridBandedColumnOptions;
    function GetStyles: TcxGridBandedColumnStyles;
    procedure SetOptions(Value: TcxGridBandedColumnOptions);
    procedure SetPosition(Value: TcxGridBandedColumnPosition);
    procedure SetStyles(Value: TcxGridBandedColumnStyles);
  protected
    // IcxStoredObject
    function GetStoredProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;

    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    function GetOptionsClass: TcxCustomGridTableItemOptionsClass; override;
    function GetStylesClass: TcxCustomGridTableItemStylesClass; override;

    procedure AssignColumnWidths; override;
    function CanCellMerging: Boolean; override;
    function CanScroll: Boolean; override;
    function CanVertSize: Boolean; virtual;
    function DefaultAlternateCaption: string; override;
    function GetActuallyVisible: Boolean; override;
    function GetEditPartVisible: Boolean; override;
    function GetIsBottom: Boolean; override;
    function GetIsLeft: Boolean; override;
    function GetIsMostBottom: Boolean; override;
    function GetIsMostLeft: Boolean; override;
    function GetIsMostRight: Boolean; override;
    function GetIsRight: Boolean; override;
    function GetIsTop: Boolean; override;
    function GetVisibleInQuickCustomizationPopup: Boolean; override;
    procedure VisibleChanged; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxGridBandedTableView read GetGridView;
  published
    property Options: TcxGridBandedColumnOptions read GetOptions write SetOptions;
    property Position: TcxGridBandedColumnPosition read FPosition write SetPosition;
    property Styles: TcxGridBandedColumnStyles read GetStyles write SetStyles;
  end;

  { view }

  // row, rows, band, bands

  TcxGridBandRow = class
  private
    FBandRows: TcxGridBandRows;
    FItems: TList;
    FVisibleItems: TList;
    function GetCount: Integer;
    function GetIndex: Integer;
    function GetIsFirst: Boolean;
    function GetIsLast: Boolean;
    function GetItem(Index: Integer): TcxGridBandedColumn;
    function GetLineCount: Integer;
    function GetLineOffset: Integer;
    function GetVisible: Boolean;
    function GetVisibleCount: Integer;
    function GetVisibleIndex: Integer;
    function GetVisibleItem(Index: Integer): TcxGridBandedColumn;
    function GetWidth: Integer;
  protected
    procedure CheckIndexForInsert(var AIndex: Integer; AExistingItem: Boolean);
    procedure PopulateTabOrderList(AList: TList);
    procedure RefreshVisibleItemsList;
    property VisibleItemsList: TList read FVisibleItems;
    property Width: Integer read GetWidth;
  public
    constructor Create(ABandRows: TcxGridBandRows);
    destructor Destroy; override;
    procedure ApplyBestFit(ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False);
    procedure Delete(AIndex: Integer);
    function IndexOf(AColumn: TcxGridBandedColumn): Integer;
    procedure Insert(AIndex: Integer; AColumn: TcxGridBandedColumn);
    procedure Move(ACurIndex, ANewIndex: Integer);

    property BandRows: TcxGridBandRows read FBandRows;
    property Count: Integer read GetCount;
    property Index: Integer read GetIndex;
    property IsFirst: Boolean read GetIsFirst;
    property IsLast: Boolean read GetIsLast;
    property Items[Index: Integer]: TcxGridBandedColumn read GetItem; default;
    property LineCount: Integer read GetLineCount;
    property LineOffset: Integer read GetLineOffset;
    property Visible: Boolean read GetVisible;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleItems[Index: Integer]: TcxGridBandedColumn read GetVisibleItem;
  end;

  TcxGridBandRows = class
  private
    FBand: TcxGridBand;
    FItems: TList;
    FVisibleItems: TList;
    function GetCount: Integer;
    function GetFirstVisible: TcxGridBandRow;
    function GetGridView: TcxGridBandedTableView;
    function GetItem(Index: Integer): TcxGridBandRow;
    function GetLastVisible: TcxGridBandRow;
    function GetLineCount: Integer;
    function GetVisibleCount: Integer;
    function GetVisibleItem(Index: Integer): TcxGridBandRow;
    function GetWidth: Integer;
    procedure SetCount(Value: Integer);
    procedure RemoveItem(ARow: TcxGridBandRow);
  protected
    procedure PopulateTabOrderList(AList: TList);
    procedure RefreshVisibleItemsList;
    property GridView: TcxGridBandedTableView read GetGridView;
    property Width: Integer read GetWidth;
  public
    constructor Create(ABand: TcxGridBand); virtual;
    destructor Destroy; override;
    procedure ApplyBestFit(ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False);
    function GetLineIndex(ARowIndex: Integer): Integer;
    function GetRowIndex(ALineIndex: Integer): Integer;
    function Insert(AIndex: Integer): TcxGridBandRow;
    procedure MoveColumn(AColumn: TcxGridBandedColumn; ARowIndex, AColIndex: Integer);

    property Band: TcxGridBand read FBand;
    property Count: Integer read GetCount write SetCount;
    property FirstVisible: TcxGridBandRow read GetFirstVisible;
    property Items[Index: Integer]: TcxGridBandRow read GetItem; default;
    property LastVisible: TcxGridBandRow read GetLastVisible;
    property LineCount: Integer read GetLineCount;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TcxGridBandRow read GetVisibleItem;
  end;

  TcxGridBandChange = (bcProperty, bcLayout, bcSize);

  TcxGridBandCustomOptions = class(TPersistent)
  private
    FBand: TcxGridBand;
  protected
    procedure Changed(AChange: TcxGridBandChange); virtual;
  public
    constructor Create(ABand: TcxGridBand); virtual;
    procedure Assign(Source: TPersistent); override;
    property Band: TcxGridBand read FBand;
  end;

  TcxGridBandOptionsClass = class of TcxGridBandOptions;

  TcxGridBandOptions = class(TcxGridBandCustomOptions)
  private
    FHoldOwnColumnsOnly: Boolean;
    FMoving: Boolean;
    FSizing: Boolean;
    procedure SetHoldOwnColumnsOnly(Value: Boolean);
    procedure SetMoving(Value: Boolean);
    procedure SetSizing(Value: Boolean);
  public
    constructor Create(ABand: TcxGridBand); override;
    procedure Assign(Source: TPersistent); override;
  published
    property HoldOwnColumnsOnly: Boolean read FHoldOwnColumnsOnly write SetHoldOwnColumnsOnly default False;
    property Moving: Boolean read FMoving write SetMoving default True;
    property Sizing: Boolean read FSizing write SetSizing default True;
  end;

  TcxGridBandPosition = class(TcxGridBandCustomOptions)
  private
    FBandIndex: Integer;
    FColIndex: Integer;
    function GetBandIndex: Integer;
    function GetColIndex: Integer;
    function GetGridView: TcxGridBandedTableView;
    function GetParentBand: TcxGridBand;
    function GetVisibleColIndex: Integer;
    procedure SetBandIndex(Value: Integer);
    procedure SetColIndex(Value: Integer);
    function IsColIndexStored: Boolean;
  protected
    function CheckBandIndex(var Value: Integer): Boolean;
    procedure SaveParams;
  public
    constructor Create(ABand: TcxGridBand); override;
    procedure Assign(Source: TPersistent); override;
    property GridView: TcxGridBandedTableView read GetGridView;
    property ParentBand: TcxGridBand read GetParentBand;
    property VisibleColIndex: Integer read GetVisibleColIndex;
  published
    property BandIndex: Integer read GetBandIndex write SetBandIndex default -1;
    property ColIndex: Integer read GetColIndex write SetColIndex stored IsColIndexStored;
  end;

  TcxGridBandGetHeaderStyle = procedure(Sender: TcxGridBandedTableView; ABand: TcxGridBand;
    {$IFDEF BCBCOMPATIBLE}var{$ELSE}out{$ENDIF} AStyle: TcxStyle) of object;

  TcxGridBandStylesClass = class of TcxGridBandStyles;

  TcxGridBandStyles = class(TcxCustomGridStyles)
  private
    FOnGetHeaderStyle: TcxGridBandGetHeaderStyle;
    function GetBand: TcxGridBand;
    function GetGridViewValue: TcxGridBandedTableView;
    procedure SetOnGetHeaderStyle(Value: TcxGridBandGetHeaderStyle);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
    function GetGridView: TcxCustomGridView; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetHeaderParams(out AParams: TcxViewParams); virtual;
    property Band: TcxGridBand read GetBand;
    property GridView: TcxGridBandedTableView read GetGridViewValue;
  published
    property Background: TcxStyle index bsBackground read GetValue write SetValue;
    property Content: TcxStyle index bsContent read GetValue write SetValue;
    property Header: TcxStyle index bsHeader read GetValue write SetValue;
    property OnGetHeaderStyle: TcxGridBandGetHeaderStyle read FOnGetHeaderStyle
      write SetOnGetHeaderStyle;
  end;

  TcxGridBandGetStoredPropertiesEvent = procedure(Sender: TcxGridBand;
    AProperties: TStrings) of object;
  TcxGridBandGetStoredPropertyValueEvent = procedure(Sender: TcxGridBand;
    const AName: string; var AValue: Variant) of object;
  TcxGridBandSetStoredPropertyValueEvent = procedure(Sender: TcxGridBand;
    const AName: string; const AValue: Variant) of object;

  TcxGridBandClass = class of TcxGridBand;

  TcxGridBand = class(TcxInterfacedCollectionItem, IcxStoredObject)
  private
    FAlternateCaption: string;
    FCaption: string;
    FChildBands: TList;
    FColumns: TList;
    FFixedKind: TcxGridBandFixedKind;
    FHeaderAlignmentHorz: TAlignment;
    FHeaderAlignmentVert: TcxAlignmentVert;
    FID: Integer;
    FIgnoreLoadingStatus: Boolean;
    FIsDestroying: Boolean;
    FLoadedIndex: Integer;
    FOptions: TcxGridBandOptions;
    FParentBand: TcxGridBand;
    FPosition: TcxGridBandPosition;
    FRows: TcxGridBandRows;
    FSavedVisible: Boolean;
    FStyles: TcxGridBandStyles;
    FTag: TcxTag;
    FVisible: Boolean;
    FVisibleChildBands: TList;
    FVisibleForCustomization: Boolean;
    FWidth: Integer;
    FSubClassEvents: TNotifyEvent;

    FOnHeaderClick: TNotifyEvent;
    FOnGetStoredProperties: TcxGridBandGetStoredPropertiesEvent;
    FOnGetStoredPropertyValue: TcxGridBandGetStoredPropertyValueEvent;
    FOnSetStoredPropertyValue: TcxGridBandSetStoredPropertyValueEvent;

    function GetBandLevelIndex: Integer;
    function GetBands: TcxGridBands;
    function GetChildBand(Index: Integer): TcxGridBand;
    function GetChildBandCount: Integer;
    function GetChildItem(Index: Integer): TObject;
    function GetChildItemCount: Integer;
    function GetChildItemVisible(Index: Integer): Boolean;
    function GetColumnCount: Integer;
    function GetColumn(Index: Integer): TcxGridBandedColumn;
    function GetFirstChildBottomBand: TcxGridBand;
    function GetFirstVisibleChildBottomBand: TcxGridBand;
    function GetGridView: TcxGridBandedTableView;
    function GetHidden: Boolean;
    function GetIsBottom: Boolean;
    function GetIsEmpty: Boolean;
    function GetIsFirst: Boolean;
    function GetIsFirstNonEmpty: Boolean;
    function GetIsLast: Boolean;
    function GetIsLastAsChild: Boolean;
    function GetIsLastNonEmpty: Boolean;
    function GetIsLoading: Boolean;
    function GetIsMostRight: Boolean;
    function GetIsRoot: Boolean;
    function GetIsUpdating: Boolean;
    function GetIsVisibleBottom: Boolean;
    function GetMinWidth: Integer;
    function GetParentBandWithAssignedWidth: TcxGridBand;
    function GetRootIndex: Integer;
    function GetRootParentBand: TcxGridBand;
    function GetVisibleBandLevelCount: Integer;
    function GetVisibleBottomIndex: Integer;
    function GetVisibleChildBand(Index: Integer): TcxGridBand;
    function GetVisibleChildBandCount: Integer;
    function GetVisibleIndex: Integer;
    function GetVisibleRootIndex: Integer;
    function IsTagStored: Boolean;
    procedure SetAlternateCaption(const Value: string);
    procedure SetCaption(const Value: string);
    procedure SetFixedKind(Value: TcxGridBandFixedKind);
    procedure SetHeaderAlignmentHorz(Value: TAlignment);
    procedure SetHeaderAlignmentVert(Value: TcxAlignmentVert);
    procedure SetHidden(Value: Boolean);
    procedure SetOnHeaderClick(Value: TNotifyEvent);
    procedure SetOnGetStoredProperties(Value: TcxGridBandGetStoredPropertiesEvent);
    procedure SetOnGetStoredPropertyValue(Value: TcxGridBandGetStoredPropertyValueEvent);
    procedure SetOnSetStoredPropertyValue(Value: TcxGridBandSetStoredPropertyValueEvent);
    procedure SetOptions(Value: TcxGridBandOptions);
    procedure SetPosition(Value: TcxGridBandPosition);
    procedure SetRootIndex(Value: Integer);
    procedure SetStyles(Value: TcxGridBandStyles);
    procedure SetTag(Value: TcxTag);
    procedure SetVisible(Value: Boolean);
    procedure SetVisibleForCustomization(Value: Boolean);
    procedure SetWidth(Value: Integer);

    procedure ReadHidden(Reader: TReader);

    procedure AddBand(ABand: TcxGridBand);
    procedure RemoveBand(ABand: TcxGridBand);
    procedure RefreshVisibleChildBandsList;

    procedure AddColumn(AColumn: TcxGridBandedColumn);
    procedure RemoveColumn(AColumn: TcxGridBandedColumn);
  protected
    procedure DefineProperties(Filer: TFiler); override;

    // IcxStoredObject
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;

    procedure ApplyLoadedIndex;
    procedure AssignChildBandWidths;
    procedure AssignColumnWidths;
    function CanHide: Boolean; virtual;
    function CanMove: Boolean; virtual;
    function CanSize: Boolean; virtual;
    procedure Changed(AChange: TcxGridBandChange); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function DefaultAlternateCaption: string; virtual;
    procedure ForceWidth(Value: Integer); virtual;
    function GetActuallyVisible: Boolean; virtual;
    function GetDisplayName: string; override;
    function GetFixed: Boolean; virtual;
    function GetParentInParent(ABand: TcxGridBand): TcxGridBand;
    function HasFixedWidth: Boolean; virtual;
    function HasParentWithAssignedWidth: Boolean;
    function ColIndexOf(ABand: TcxGridBand): Integer; overload;
    procedure MoveBandsToRoot;
    procedure PopulateTabOrderList(AList: TList);
    procedure ResetLoadedIndex;
    procedure SetIndex(Value: Integer); override;
    function VisibleColIndexOf(ABand: TcxGridBand): Integer; overload;
    procedure VisibleForCustomizationChanged; virtual;

    procedure CheckChildrenVisibles;
    procedure CheckVisible;
    procedure SaveChildrenVisibles;
    procedure SaveColumnsVisibles;
    procedure SaveVisible;
    property ChildItemCount: Integer read GetChildItemCount;
    property ChildItems[Index: Integer]: TObject read GetChildItem;
    property ChildItemVisibles[Index: Integer]: Boolean read GetChildItemVisible;
    property SavedVisible: Boolean read FSavedVisible;

    function GetOptionsClass: TcxGridBandOptionsClass; virtual;
    function GetStylesClass: TcxGridBandStylesClass; virtual;

    procedure DoHeaderClick; virtual;

    property Fixed: Boolean read GetFixed;
    property IgnoreLoadingStatus: Boolean read FIgnoreLoadingStatus write FIgnoreLoadingStatus;
    property IsDestroying: Boolean read FIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsUpdating: Boolean read GetIsUpdating;
    property ParentBandWithAssignedWidth: TcxGridBand read GetParentBandWithAssignedWidth;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure ApplyBestFit(ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False); virtual;
    function GetAlternateCaption: string;
    function HasAsParent(ABand: TcxGridBand): Boolean;
    procedure MoveBand(ABand: TcxGridBand; AColIndex: Integer);
    procedure MoveColumn(AColumn: TcxGridBandedColumn; ARowIndex, AColIndex: Integer);
    procedure MoveColumns(ABand: TcxGridBand);

    property ActuallyVisible: Boolean read GetActuallyVisible;
    property BandLevelIndex: Integer read GetBandLevelIndex;
    property Bands: TcxGridBands read GetBands;
    property ChildBandCount: Integer read GetChildBandCount;
    property ChildBands[Index: Integer]: TcxGridBand read GetChildBand;
    property ColumnCount: Integer read GetColumnCount;
    property Columns[Index: Integer]: TcxGridBandedColumn read GetColumn;
    property FirstChildBottomBand: TcxGridBand read GetFirstChildBottomBand;
    property FirstVisibleChildBottomBand: TcxGridBand read GetFirstVisibleChildBottomBand;
    property GridView: TcxGridBandedTableView read GetGridView;
    property Hidden: Boolean read GetHidden write SetHidden;  // obsolete, use VisibleForCustomization
    property ID: Integer read FID;
    property IsBottom: Boolean read GetIsBottom;
    property IsEmpty: Boolean read GetIsEmpty;
    property IsFirst: Boolean read GetIsFirst;
    property IsFirstNonEmpty: Boolean read GetIsFirstNonEmpty;
    property IsLast: Boolean read GetIsLast;
    property IsLastAsChild: Boolean read GetIsLastAsChild;
    property IsLastNonEmpty: Boolean read GetIsLastNonEmpty;
    property IsMostRight: Boolean read GetIsMostRight;
    property IsRoot: Boolean read GetIsRoot;
    property IsVisibleBottom: Boolean read GetIsVisibleBottom;
    property MinWidth: Integer read GetMinWidth;
    property ParentBand: TcxGridBand read FParentBand;
    property RootIndex: Integer read GetRootIndex write SetRootIndex;
    property RootParentBand: TcxGridBand read GetRootParentBand;
    property Rows: TcxGridBandRows read FRows;
    property VisibleBandLevelCount: Integer read GetVisibleBandLevelCount;
    property VisibleBottomIndex: Integer read GetVisibleBottomIndex;
    property VisibleChildBandCount: Integer read GetVisibleChildBandCount;
    property VisibleChildBands[Index: Integer]: TcxGridBand read GetVisibleChildBand;
    property VisibleIndex: Integer read GetVisibleIndex;
    property VisibleRootIndex: Integer read GetVisibleRootIndex;
  published
    property Alignment: TAlignment read FHeaderAlignmentHorz write SetHeaderAlignmentHorz stored False;  // for compatibility
    property AlternateCaption: string read FAlternateCaption write SetAlternateCaption;
    property Caption: string read FCaption write SetCaption;
    property FixedKind: TcxGridBandFixedKind read FFixedKind write SetFixedKind default fkNone;
    property HeaderAlignmentHorz: TAlignment read FHeaderAlignmentHorz write SetHeaderAlignmentHorz default taCenter;
    property HeaderAlignmentVert: TcxAlignmentVert read FHeaderAlignmentVert write SetHeaderAlignmentVert default vaCenter;
    property Options: TcxGridBandOptions read FOptions write SetOptions;
    property Position: TcxGridBandPosition read FPosition write SetPosition;
    property Styles: TcxGridBandStyles read FStyles write SetStyles;
    property Tag: TcxTag read FTag write SetTag stored IsTagStored;
    property Visible: Boolean read FVisible write SetVisible default True;
    property VisibleForCustomization: Boolean read FVisibleForCustomization
      write SetVisibleForCustomization default True;
    property Width: Integer read FWidth write SetWidth default 0;
    property StylesEvents: TNotifyEvent read FSubClassEvents write FSubClassEvents;
    property OnHeaderClick: TNotifyEvent read FOnHeaderClick write SetOnHeaderClick;
    property OnGetStoredProperties: TcxGridBandGetStoredPropertiesEvent
      read FOnGetStoredProperties write SetOnGetStoredProperties;
    property OnGetStoredPropertyValue: TcxGridBandGetStoredPropertyValueEvent
      read FOnGetStoredPropertyValue write SetOnGetStoredPropertyValue;
    property OnSetStoredPropertyValue: TcxGridBandSetStoredPropertyValueEvent
      read FOnSetStoredPropertyValue write SetOnSetStoredPropertyValue;
  end;

  TcxGridBandsLayout = (blNonFixed, blLeftFixed, blRightFixed, blLeftRightFixed);

  TcxGridBandsClass = class of TcxGridBands;
  TcxGridBands = class(TcxOwnedInterfacedCollection, IcxStoredObject, IcxStoredParent)
  private
    FBottomItems: TList;
    FGridView: TcxGridBandedTableView;
    FNextID: Integer;
    FRootItems: TList;
    FVisibleBottomItems: TList;
    FVisibleItems: TList;
    FVisibleLeftFixedCount: Integer;
    FVisibleRightFixedCount: Integer;
    FVisibleRootItems: TList;
    FVisibleRootLeftFixedCount: Integer;
    FVisibleRootRightFixedCount: Integer;

    function GetBottomItem(Index: Integer): TcxGridBand;
    function GetBottomItemCount: Integer;
    function GetFirstVisibleNonEmpty: TcxGridBand;
    function GetItem(Index: Integer): TcxGridBand;
    function GetLastVisibleNonEmpty: TcxGridBand;
    function GetLayout: TcxGridBandsLayout;
    function GetLineCount: Integer;
    function GetRootItem(Index: Integer): TcxGridBand;
    function GetRootItemCount: Integer;
    function GetVisibleBottomItem(Index: Integer): TcxGridBand;
    function GetVisibleBottomItemCount: Integer;
    function GetVisibleCount: Integer;
    function GetVisibleItem(Index: Integer): TcxGridBand;
    function GetVisibleRootItem(Index: Integer): TcxGridBand;
    function GetVisibleRootItemCount: Integer;
    function GetVisibleRowCount: Integer;
    procedure SetItem(Index: Integer; Value: TcxGridBand);

    procedure AddItem(AItem: TcxGridBand);
    procedure RemoveItem(AItem: TcxGridBand);

    procedure RefreshBottomItemsList;
    procedure RefreshRootItemsList;
    procedure RefreshVisibleBottomItemsList;
    procedure RefreshVisibleItemsList;
    procedure RefreshVisibleRootItemsList;
  protected
    // IcxStoredObject
    function GetObjectName: string; virtual;
    function GetProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); virtual;
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject; virtual;
    procedure DeleteChild(const AObjectName: string; AObject: TObject); virtual;
    procedure GetChildren(AChildren: TStringList); virtual;

    procedure BandVisibilityChanged(ABand: TcxGridBand; Value: Boolean); virtual;
    procedure ChangeScale(M, D: Integer); virtual;
    function GetBandClass: TcxGridBandClass; virtual;
    function GetNextID: Integer;
    procedure ReleaseID(AID: Integer);
    procedure PopulateTabOrderList(AList: TList);

    property NextID: Integer read FNextID;
  public
    constructor Create(AGridView: TcxGridBandedTableView); virtual;
    destructor Destroy; override;
    function Add: TcxGridBand;
    function AreNested: Boolean;
    procedure Assign(Source: TPersistent); override;
    procedure Clear;
    function FindItemID(AID: Integer): TcxGridBand;
    function GetFirstVisibleIndex(AFixedKind: TcxGridBandFixedKind): Integer;
    function GetFirstVisibleRootIndex(AFixedKind: TcxGridBandFixedKind): Integer;
    function GetLastVisibleIndex(AFixedKind: TcxGridBandFixedKind): Integer;
    function GetLastVisibleRootIndex(AFixedKind: TcxGridBandFixedKind): Integer;
    function HaveFixedItems: Boolean;

    property BottomItemCount: Integer read GetBottomItemCount;
    property BottomItems[Index: Integer]: TcxGridBand read GetBottomItem;
    property FirstVisibleNonEmpty: TcxGridBand read GetFirstVisibleNonEmpty;
    property GridView: TcxGridBandedTableView read FGridView;
    property Items[Index: Integer]: TcxGridBand read GetItem write SetItem; default;
    property Layout: TcxGridBandsLayout read GetLayout;
    property LastVisibleNonEmpty: TcxGridBand read GetLastVisibleNonEmpty;
    property LineCount: Integer read GetLineCount;
    property RootItemCount: Integer read GetRootItemCount;
    property RootItems[Index: Integer]: TcxGridBand read GetRootItem;
    property VisibleBottomItemCount: Integer read GetVisibleBottomItemCount;
    property VisibleBottomItems[Index: Integer]: TcxGridBand read GetVisibleBottomItem;
    property VisibleCount: Integer read GetVisibleCount;
    property VisibleItems[Index: Integer]: TcxGridBand read GetVisibleItem;
    property VisibleLeftFixedCount: Integer read FVisibleLeftFixedCount;
    property VisibleRightFixedCount: Integer read FVisibleRightFixedCount;
    property VisibleRootItemCount: Integer read GetVisibleRootItemCount;
    property VisibleRootItems[Index: Integer]: TcxGridBand read GetVisibleRootItem;
    property VisibleRootLeftFixedCount: Integer read FVisibleRootLeftFixedCount;
    property VisibleRootRightFixedCount: Integer read FVisibleRootRightFixedCount;
    property VisibleRowCount: Integer read GetVisibleRowCount;
  end;

  // options

  TcxGridBandedTableBackgroundBitmaps = class(TcxGridTableBackgroundBitmaps)
  protected
    function GetBitmapStyleIndex(Index: Integer): Integer; override;
  public
    procedure Assign(Source: TPersistent); override;
  published
    property BandBackground: TBitmap index bbBandBackground read GetValue write SetValue;
    property BandHeader: TBitmap index bbBandHeader read GetValue write SetValue;
  end;

  TcxGridBandedTableOptionsBehavior = class(TcxGridTableOptionsBehavior)
  private
    FBandHeaderHints: Boolean;
    procedure SetBandHeaderHints(Value: Boolean);
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
  published
    property BandHeaderHints: Boolean read FBandHeaderHints write SetBandHeaderHints default True;
  end;

  TcxGridBandedTableOptionsCustomize = class(TcxGridTableOptionsCustomize)
  private
    FBandHiding: Boolean;
    FBandMoving: Boolean;
    FBandSizing: Boolean;
    FBandsQuickCustomization: Boolean;
    FBandsQuickCustomizationMaxDropDownCount: Integer;
    FBandsQuickCustomizationMultiColumnMode: Boolean;
    FBandsQuickCustomizationReordering: TcxGridQuickCustomizationReordering;
    FBandsQuickCustomizationShowCommands: Boolean;
    FBandsQuickCustomizationSorted: Boolean;
    FColumnVertSizing: Boolean;
    FNestedBands: Boolean;
    function GetGridView: TcxGridBandedTableView;
    procedure SetBandHiding(Value: Boolean);
    procedure SetBandMoving(Value: Boolean);
    procedure SetBandSizing(Value: Boolean);
    procedure SetBandsQuickCustomization(Value: Boolean);
    procedure SetBandsQuickCustomizationMaxDropDownCount(Value: Integer);
    procedure SetBandsQuickCustomizationMultiColumnMode(Value: Boolean);
    procedure SetBandsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
    procedure SetColumnVertSizing(Value: Boolean);
    procedure SetNestedBands(Value: Boolean);
  protected
    property BandsQuickCustomizationMultiColumnMode: Boolean read FBandsQuickCustomizationMultiColumnMode
      write SetBandsQuickCustomizationMultiColumnMode default True;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function SupportsBandsQuickCustomizationReordering: Boolean; virtual;
    function SupportsItemsQuickCustomizationReordering: Boolean; override;
    property GridView: TcxGridBandedTableView read GetGridView;
  published
    property BandHiding: Boolean read FBandHiding write SetBandHiding default False;
    property BandMoving: Boolean read FBandMoving write SetBandMoving default True;
    property BandSizing: Boolean read FBandSizing write SetBandSizing default True;
    property BandsQuickCustomization: Boolean read FBandsQuickCustomization
      write SetBandsQuickCustomization default False;
    property BandsQuickCustomizationMaxDropDownCount: Integer read FBandsQuickCustomizationMaxDropDownCount
      write SetBandsQuickCustomizationMaxDropDownCount default 0;
    property BandsQuickCustomizationReordering: TcxGridQuickCustomizationReordering
      read FBandsQuickCustomizationReordering write SetBandsQuickCustomizationReordering default qcrDefault;
    property BandsQuickCustomizationShowCommands: Boolean
      read FBandsQuickCustomizationShowCommands write FBandsQuickCustomizationShowCommands default True;
    property BandsQuickCustomizationSorted: Boolean
      read FBandsQuickCustomizationSorted write FBandsQuickCustomizationSorted default False;
    property ColumnVertSizing: Boolean read FColumnVertSizing write SetColumnVertSizing default True;
    property NestedBands: Boolean read FNestedBands write SetNestedBands default True;
  end;

  { TcxGridBandedTableOptionsView }

  TcxGridBandedTableOptionsView = class(TcxGridTableOptionsView)
  strict private
    FBandCaptionsInColumnAlternateCaption: Boolean;
    FBandHeaderEndEllipsis: Boolean;
    FBandHeaderHeight: Integer;
    FBandHeaderLineCount: Integer;
    FBandHeaders: Boolean;
    FFixedBandSeparatorColor: TColor;
    FFixedBandSeparatorWidth: Integer;

    function GetGridView: TcxGridBandedTableView;
    procedure SetBandCaptionsInColumnAlternateCaption(Value: Boolean);
    procedure SetBandHeaderEndEllipsis(Value: Boolean);
    procedure SetBandHeaderHeight(Value: Integer);
    procedure SetBandHeaderLineCount(Value: Integer);
    procedure SetBandHeaders(Value: Boolean);
    procedure SetFixedBandSeparatorColor(Value: TColor);
    procedure SetFixedBandSeparatorWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;
    function GetFixedBandSeparatorColor: TColor;
    property GridView: TcxGridBandedTableView read GetGridView;
  published
    property BandCaptionsInColumnAlternateCaption: Boolean read FBandCaptionsInColumnAlternateCaption write SetBandCaptionsInColumnAlternateCaption default False;
    property BandHeaderEndEllipsis: Boolean read FBandHeaderEndEllipsis write SetBandHeaderEndEllipsis default False;
    property BandHeaderHeight: Integer read FBandHeaderHeight write SetBandHeaderHeight default 0;
    property BandHeaderLineCount: Integer read FBandHeaderLineCount write SetBandHeaderLineCount default 1;
    property BandHeaders: Boolean read FBandHeaders write SetBandHeaders default True;
    property FixedBandSeparatorColor: TColor read FFixedBandSeparatorColor write SetFixedBandSeparatorColor default clDefault;
    property FixedBandSeparatorWidth: Integer read FFixedBandSeparatorWidth write SetFixedBandSeparatorWidth default cxGridDefaultFixedBandSeparatorWidth;
  end;

  // styles

  TcxGridBandedTableViewStyles = class(TcxGridTableViewStyles)
  private
    FOnGetBandHeaderStyle: TcxGridBandGetHeaderStyle;
    function GetGridViewValue: TcxGridBandedTableView;
    procedure SetOnGetBandHeaderStyle(Value: TcxGridBandGetHeaderStyle);
  protected
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure GetBandHeaderParams(ABand: TcxGridBand; out AParams: TcxViewParams); virtual;
    property GridView: TcxGridBandedTableView read GetGridViewValue;
  published
    property BandBackground: TcxStyle index vsBandBackground read GetValue write SetValue;
    property BandHeader: TcxStyle index vsBandHeader read GetValue write SetValue;
    property StyleSheet;
    property OnGetBandHeaderStyle: TcxGridBandGetHeaderStyle read FOnGetBandHeaderStyle
      write SetOnGetBandHeaderStyle;
  end;

  TcxGridBandedTableViewStyleSheet = class(TcxCustomStyleSheet)
  private
    function GetStylesValue: TcxGridBandedTableViewStyles;
    procedure SetStylesValue(Value: TcxGridBandedTableViewStyles);
  public
    class function GetStylesClass: TcxCustomStylesClass; override;
  published
    property Styles: TcxGridBandedTableViewStyles read GetStylesValue write SetStylesValue;
  end;

  // grid view

  TcxGridBandCustomDrawHeaderEvent = procedure(Sender: TcxGridBandedTableView;
    ACanvas: TcxCanvas; AViewInfo: TcxGridBandHeaderViewInfo; var ADone: Boolean) of object;
  TcxGridBandEvent = procedure(Sender: TcxGridBandedTableView; ABand: TcxGridBand) of object;

  TcxGridBandQuickCustomizationColumnCompareEvent = procedure(var ACompareFunction: TListSortCompare) of object;

  TcxGridBandedTableView = class(TcxGridTableView)
  strict private
    FBands: TcxGridBands;
    FIsAssigningBands: Boolean;

    FOnBandPosChanged: TcxGridBandEvent;
    FOnBandSizeChanged: TcxGridBandEvent;
    FOnCustomDrawBandHeader: TcxGridBandCustomDrawHeaderEvent;
    FOnQuickCustomizationColumnCompare: TcxGridBandQuickCustomizationColumnCompareEvent;

    function GetBackgroundBitmaps: TcxGridBandedTableBackgroundBitmaps;
    function GetColumn(Index: Integer): TcxGridBandedColumn;
    function GetController: TcxGridBandedTableController;
    function GetGroupedColumn(Index: Integer): TcxGridBandedColumn;
    function GetOptionsBehavior: TcxGridBandedTableOptionsBehavior;
    function GetOptionsCustomize: TcxGridBandedTableOptionsCustomize;
    function GetOptionsView: TcxGridBandedTableOptionsView;
    function GetStyles: TcxGridBandedTableViewStyles;
    function GetViewInfo: TcxGridBandedTableViewInfo;
    function GetVisibleColumn(Index: Integer): TcxGridBandedColumn;
    procedure SetBackgroundBitmaps(Value: TcxGridBandedTableBackgroundBitmaps);
    procedure SetBands(Value: TcxGridBands);
    procedure SetColumn(Index: Integer; Value: TcxGridBandedColumn);
    procedure SetOnBandPosChanged(Value: TcxGridBandEvent);
    procedure SetOnBandSizeChanged(Value: TcxGridBandEvent);
    procedure SetOnCustomDrawBandHeader(Value: TcxGridBandCustomDrawHeaderEvent);
    procedure SetOptionsBehavior(Value: TcxGridBandedTableOptionsBehavior);
    procedure SetOptionsCustomize(Value: TcxGridBandedTableOptionsCustomize);
    procedure SetOptionsView(Value: TcxGridBandedTableOptionsView);
    procedure SetStyles(Value: TcxGridBandedTableViewStyles);
  protected
    // IcxStoredObject
    function GetProperties(AProperties: TStrings): Boolean; override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IcxStoredParent
    procedure DeleteChild(const AObjectName: string; AObject: TObject); override;
    procedure GetStoredChildren(AChildren: TStringList); override;
    // IcxGridViewLayoutEditorSupport - for design-time layout editor
    procedure AssignLayout(ALayoutView: TcxCustomGridView); override;
    // IdxAdornerTargetElementCollection
    procedure GetAdornerTargetElements(AList: TStrings); override;

    procedure ReadState(Reader: TReader); override;
    procedure Updated; override;
    procedure Updating; override;

    procedure BeginAssignBands;
    function CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure DoAssign(ASource: TcxCustomGridView); override;
    procedure DoItemsAssigned; override;
    procedure EndAssignBands;
    procedure GetFakeComponentLinks(AList: TList); override;
    function GetIsControlFocused: Boolean; override;
    function HasCustomDrawBandHeader: Boolean;

    procedure CreateOptions; override;
    procedure DestroyOptions; override;

    procedure AddItem(AItem: TcxCustomGridTableItem); override;
    procedure AssignBandsParams;
    procedure AssignColumnsParams(AAssignBandIndexes, AAssignOtherIndexes: Boolean);
    procedure BeforeRestoring; override;
    procedure GetVisibleItemsList(AItems: TList); override;
    procedure LoadingComplete; override;
    procedure PopulateTabOrderList(AList: TList);
    procedure RefreshVisibleItemsList; override;
    procedure RestoringComplete; override;
    procedure SaveBandsParams;
    procedure SaveColumnsParams(ABandIndexOnly: Boolean = False);

    function GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass; override;
    function GetBandsClass: TcxGridBandsClass; virtual;
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetItemClass: TcxCustomGridTableItemClass; override;
    function GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetStylesClass: TcxCustomGridViewStylesClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    procedure DoBandPosChanged(ABand: TcxGridBand); virtual;
    procedure DoBandSizeChanged(ABand: TcxGridBand); virtual;
    procedure DoCustomDrawBandHeader(ACanvas: TcxCanvas; AViewInfo: TcxGridBandHeaderViewInfo; var ADone: Boolean); virtual;

    procedure Initialize; override;

    property IsAssigningBands: Boolean read FIsAssigningBands;
  public
    procedure ApplyBestFit(AItem: TcxCustomGridTableItem = nil; ACheckSizingAbility: Boolean = False;
      AFireEvents: Boolean = False); override;
    function CreateColumn: TcxGridBandedColumn;

    property Columns[Index: Integer]: TcxGridBandedColumn read GetColumn write SetColumn;
    property Controller: TcxGridBandedTableController read GetController;
    property GroupedColumns[Index: Integer]: TcxGridBandedColumn read GetGroupedColumn;
    property ViewInfo: TcxGridBandedTableViewInfo read GetViewInfo;
    property VisibleColumns[Index: Integer]: TcxGridBandedColumn read GetVisibleColumn;
  published
    property BackgroundBitmaps: TcxGridBandedTableBackgroundBitmaps read GetBackgroundBitmaps write SetBackgroundBitmaps;
    property Bands: TcxGridBands read FBands write SetBands;
    property OptionsBehavior: TcxGridBandedTableOptionsBehavior read GetOptionsBehavior write SetOptionsBehavior;
    property OptionsCustomize: TcxGridBandedTableOptionsCustomize read GetOptionsCustomize write SetOptionsCustomize;
    property OptionsView: TcxGridBandedTableOptionsView read GetOptionsView write SetOptionsView;
    property Styles: TcxGridBandedTableViewStyles read GetStyles write SetStyles;
    property OnBandPosChanged: TcxGridBandEvent read FOnBandPosChanged write SetOnBandPosChanged;
    property OnBandSizeChanged: TcxGridBandEvent read FOnBandSizeChanged write SetOnBandSizeChanged;
    property OnCustomDrawBandHeader: TcxGridBandCustomDrawHeaderEvent read FOnCustomDrawBandHeader write SetOnCustomDrawBandHeader;
    property OnQuickCustomizationColumnCompare: TcxGridBandQuickCustomizationColumnCompareEvent read FOnQuickCustomizationColumnCompare write FOnQuickCustomizationColumnCompare;
  end;

implementation

uses
  SysUtils, Math, Contnrs, cxGridStrs, cxCustomData, cxCheckListBox, cxLibraryConsts;

const
  ColumnHeaderInsertZoneSize = 20;
  BandHeaderMovingZoneSize = 20;
  BandSizingMarkWidth = 2;
  EmptyBandMinWidth = 20;

  BandHeaderHitTests = [htBandHeader, htBandHeaderSizingEdge];

type
  TdxCustomGridQuickCustomizationControlAccess = class(TdxCustomGridQuickCustomizationControl);

  TData = class
    Columns: TList;
    GridRecord: TcxCustomGridRecord;
  end;

procedure CheckItemIndexForInsert(var AIndex: Integer; ACount: Integer; AExistingItem: Boolean);
begin
  if AIndex < 0 then AIndex := 0;
  if AIndex > ACount - Ord(AExistingItem) then AIndex := ACount - Ord(AExistingItem);
end;

function CompareVisibleColumnPositions(APosition1, APosition2: TcxGridBandedColumnPosition): Integer;
begin
  Result := APosition1.Band.VisibleIndex - APosition2.Band.VisibleIndex;
  if Result = 0 then
  begin
    Result := APosition1.VisibleRowIndex - APosition2.VisibleRowIndex;
    if Result = 0 then
    begin
      Result := APosition1.VisibleColIndex - APosition2.VisibleColIndex;
      if Result = 0 then
        Result := APosition1.Item.VisibleIndex - APosition2.Item.VisibleIndex;
    end;
  end;
end;

{ TcxGridColumnHeaderVertSizingEdgeHitTest }

class function TcxGridColumnHeaderVertSizingEdgeHitTest.GetHitTestCode: Integer;
begin
  Result := htColumnHeaderVertSizingEdge;
end;

function TcxGridColumnHeaderVertSizingEdgeHitTest.Cursor: TCursor;
begin
  Result := crcxGridVertSize;
end;

function TcxGridColumnHeaderVertSizingEdgeHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := TcxGridColumnVertSizingObject;
end;

{ TcxGridBandHitTest }

procedure TcxGridBandHitTest.Assign(Source: TcxCustomGridHitTest);
begin
  inherited Assign(Source);
  if Source is TcxGridBandHitTest then
  begin
    FBand := TcxGridBandHitTest(Source).FBand;
    FBandContainerKind := TcxGridBandHitTest(Source).FBandContainerKind;
    FVisibleRowIndex := TcxGridBandHitTest(Source).FVisibleRowIndex;
  end;
end;

class function TcxGridBandHitTest.GetHitTestCode: Integer;
begin
  Result := htBand;
end;

{ TcxGridBandHeaderHitTest }

class function TcxGridBandHeaderHitTest.GetHitTestCode: Integer;
begin
  Result := htBandHeader;
end;

function TcxGridBandHeaderHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  if Band.CanMove then
    Result := TcxGridBandHeaderMovingObject
  else
    Result := nil;
end;

{ TcxGridBandHeaderSizingEdgeHitTest }

class function TcxGridBandHeaderSizingEdgeHitTest.GetHitTestCode: Integer;
begin
  Result := htBandHeaderSizingEdge;
end;

function TcxGridBandHeaderSizingEdgeHitTest.Cursor: TCursor;
begin
  Result := crcxGridHorzSize;
end;

function TcxGridBandHeaderSizingEdgeHitTest.DragAndDropObjectClass: TcxCustomGridDragAndDropObjectClass;
begin
  Result := TcxGridBandSizingObject;
end;

{ TcxGridIndicatorBandHeaderHitTest }

class function TcxGridIndicatorBandHeaderHitTest.GetHitTestCode: Integer;
begin
  Result := htIndicatorBandHeader;
end;

{ TcxGridBandedColumnContainerZone }

constructor TcxGridBandedColumnContainerZone.Create(AColumnIndex: Integer;
  ABand: TcxGridBand; AColIndex, ARowIndex: Integer);
begin
  inherited Create(AColumnIndex);
  Band := ABand;
  ColIndex := AColIndex;
  RowIndex := ARowIndex;
end;

function TcxGridBandedColumnContainerZone.IsEqual(Value: TcxGridItemContainerZone): Boolean;
var
  AValue: TcxGridBandedColumnContainerZone;
begin
  AValue := TcxGridBandedColumnContainerZone(Value);
  Result := inherited IsEqual(Value) and (ClassType = Value.ClassType) and
    (Band = AValue.Band) and (ColIndex = AValue.ColIndex) and (RowIndex = AValue.RowIndex);
end;

function TcxGridBandedColumnContainerZone.IsInsertion: Boolean;
begin
  Result := ItemIndex = -1;
end;

{ TcxGridBandedColumnHeaderMovingObject }

function TcxGridBandedColumnHeaderMovingObject.GetDestZone: TcxGridBandedColumnContainerZone;
begin
  Result := TcxGridBandedColumnContainerZone(inherited DestZone);
end;

function TcxGridBandedColumnHeaderMovingObject.GetSourceItem: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited SourceItem);
end;

function TcxGridBandedColumnHeaderMovingObject.GetSourcePosition: TcxGridBandedColumnPosition;
begin
  Result := TcxGridBandedColumn(SourceItem).Position;
end;

function TcxGridBandedColumnHeaderMovingObject.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandedColumnHeaderMovingObject.SetSourceItem(Value: TcxGridBandedColumn);
begin
  inherited SourceItem := Value;
end;

function TcxGridBandedColumnHeaderMovingObject.AreArrowsVertical: Boolean;
begin
  if (DestItemContainerKind = ckHeader) and (DestZone.ItemIndex = -1) then
    Result := False
  else
    Result := inherited AreArrowsVertical;
end;

procedure TcxGridBandedColumnHeaderMovingObject.CheckDestItemContainerKind(var AValue: TcxGridItemContainerKind);
begin
  inherited;
  if (AValue = ckHeader) and
    (DestZone.Band.Options.HoldOwnColumnsOnly or
     (SourceItem.Position.Band <> nil) and SourceItem.Position.Band.Options.HoldOwnColumnsOnly) and
    (SourceItem.Position.Band <> DestZone.Band) then
    AValue := ckNone;
end;

procedure TcxGridBandedColumnHeaderMovingObject.DoColumnMovingToHeader;

  function GetRowIndex: Integer;
  var
    ABand: TcxGridBand;
  begin
    ABand := DestZone.Band;
    if DestZone.RowIndex = ABand.Rows.VisibleCount then
      Result := ABand.Rows.Count
    else
      Result := ABand.Rows.VisibleItems[DestZone.RowIndex].Index;
  end;

  function GetColIndex: Integer;
  var
    ARow: TcxGridBandRow;
  begin
    ARow := SourcePosition.Row;
    if DestZone.ColIndex = ARow.VisibleCount then
      Result := ARow.Count
    else
    begin
      Result := ARow.VisibleItems[DestZone.ColIndex].Position.ColIndex;
      if SourcePosition.ColIndex < Result then Dec(Result);
    end;
  end;

begin
  with SourceItem do
  begin
    Position.BandIndex := DestZone.Band.Index;
    if DestZone.IsInsertion then
      Position.Band.Rows.Insert(GetRowIndex);
    Position.RowIndex := GetRowIndex;
    Position.ColIndex := GetColIndex;
  end;
end;

function TcxGridBandedColumnHeaderMovingObject.GetArrowAreaBoundsForHeader(APlace: TcxGridArrowPlace): TRect;
var
  ARowsViewInfo: TcxGridBandRowsViewInfo;
  AColumn: TcxGridBandedColumn;
begin
  if DestZone.IsInsertion then
  begin
    ARowsViewInfo := ViewInfo.HeaderViewInfo.BandsViewInfo[DestZone.Band.VisibleIndex].RowsViewInfo;
    Result := ARowsViewInfo.Bounds;
    if DestZone.RowIndex > 0 then
      Result.Top := ARowsViewInfo[DestZone.RowIndex - 1].Bounds.Bottom;
  end
  else
  begin
    Result := inherited GetArrowAreaBoundsForHeader(APlace);
    AColumn := TcxGridBandedColumn(GridView.VisibleColumns[DestZone.ItemIndex]);
    if DestZone.ColIndex <> AColumn.Position.VisibleColIndex then
      if ViewInfo.UseRightToLeftAlignment then
        Result.Right := Result.Left
      else
        Result.Left := Result.Right;
  end;
end;

function TcxGridBandedColumnHeaderMovingObject.GetArrowsClientRect: TRect;
begin
  Result := inherited GetArrowsClientRect;
  if (DestItemContainerKind = ckHeader) and (DestZone.Band.FixedKind = fkNone) then
    Result := ViewInfo.ScrollableAreaBoundsHorz;
end;

function TcxGridBandedColumnHeaderMovingObject.IsValidDestinationForVisibleSource: Boolean;

  function IsValidForInsertion: Boolean;
  begin
    Result :=
      (SourcePosition.Row.VisibleCount <> 1) or
      (DestZone.RowIndex < SourcePosition.VisibleRowIndex) or
      (SourcePosition.VisibleRowIndex + 1 < DestZone.RowIndex);
  end;

  function IsValidForMoving: Boolean;
  begin
    Result :=
      (DestZone.RowIndex <> SourcePosition.VisibleRowIndex) or
      (DestZone.ColIndex < SourcePosition.VisibleColIndex) or
      (SourcePosition.VisibleColIndex + 1 < DestZone.ColIndex);
  end;

begin
  if DestItemContainerKind = ckHeader then
    Result :=
      (SourceItemContainerKind = ckGroupByBox) or
      (DestZone.Band <> SourcePosition.Band) or
      DestZone.IsInsertion and IsValidForInsertion or
      not DestZone.IsInsertion and IsValidForMoving
  else
    Result := inherited IsValidDestinationForVisibleSource;
end;

{ TcxGridBandHeaderMovingObject }

function TcxGridBandHeaderMovingObject.GetController: TcxGridBandedTableController;
begin
  Result := TcxGridBandedTableController(inherited Controller);
end;

function TcxGridBandHeaderMovingObject.GetCustomizationForm: TcxGridBandedTableCustomizationForm;
begin
  Result := TcxGridBandedTableCustomizationForm(inherited CustomizationForm);
end;

function TcxGridBandHeaderMovingObject.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandHeaderMovingObject.GetSourceBand: TcxGridBand;
begin
  Result := SourceItem as TcxGridBand;
end;

function TcxGridBandHeaderMovingObject.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandHeaderMovingObject.SetDestBand(Value: TcxGridBand);
begin
  if FDestBand <> Value then
  begin
    Dirty := True;
    FDestBand := Value;
  end;
end;

procedure TcxGridBandHeaderMovingObject.SetDestBandContainerKind(Value: TcxGridBandContainerKind);
begin
  if FDestBandContainerKind <> Value then
  begin
    Dirty := True;
    FDestBandContainerKind := Value;
  end;
end;

procedure TcxGridBandHeaderMovingObject.SetDestInsertPosition(Value: TcxPosition);
begin
  if FDestInsertPosition <> Value then
  begin
    Dirty := True;
    FDestInsertPosition := Value;
  end;
end;

procedure TcxGridBandHeaderMovingObject.SetIsEmptyViewInsert(Value: Boolean);
begin
  if FIsEmptyViewInsert <> Value then
  begin
    Dirty := True;
    FIsEmptyViewInsert := Value;
  end;
end;

procedure TcxGridBandHeaderMovingObject.SetSourceBand(Value: TcxGridBand);
begin
  SourceItem := Value;
end;

function TcxGridBandHeaderMovingObject.AreArrowsVertical: Boolean;
begin
  Result := FDestInsertPosition in [posLeft, posRight];
end;

function TcxGridBandHeaderMovingObject.CalculateIsEmptyViewInsert(ACheckMousePos: Boolean): Boolean;
begin
  Result := (DestBandContainerKind = bcHeader) and
    (GridView.Bands.VisibleRootItemCount = 0) and
    (not ACheckMousePos or PtInRect(GridView.ViewInfo.ScrollableAreaBoundsHorz, CurMousePos));
end;

function TcxGridBandHeaderMovingObject.CanRemove: Boolean;
begin
  Result := SourceBand.VisibleForCustomization and
    (SourceBandContainerKind = bcHeader) and SourceBand.CanHide and
    (GridView.OptionsCustomize.BandHiding or GridView.Controller.Customization);
end;

procedure TcxGridBandHeaderMovingObject.CheckDestParams;
var
  AColumnsSource, AColumnsDestination: TcxGridBand;
begin
  if (DestBand <> nil) and (DestInsertPosition in [posTop, posBottom]) then
  begin
    AColumnsSource := nil;
    AColumnsDestination := nil;
    if (DestInsertPosition = posBottom) and DestBand.IsBottom then
      if DestBand.ColumnCount <> 0 then
      begin
        AColumnsSource := DestBand;
        AColumnsDestination := SourceBand.FirstVisibleChildBottomBand;
      end
      else
    else
      if SourceBand.IsBottom and (SourceBand.ColumnCount <> 0) then
      begin
        AColumnsSource := SourceBand;
        AColumnsDestination := DestBand.FirstVisibleChildBottomBand;
      end;
    if (AColumnsSource <> AColumnsDestination) and
      ((AColumnsSource <> nil) and AColumnsSource.Options.HoldOwnColumnsOnly or
       (AColumnsDestination <> nil) and AColumnsDestination.Options.HoldOwnColumnsOnly) then
      DestBand := nil;
  end;
end;

function TcxGridBandHeaderMovingObject.GetArrowAreaBounds(APlace: TcxGridArrowPlace): TRect;
begin
  if IsEmptyViewInsert then
    Result := ViewInfo.HeaderViewInfo.Bounds
  else
  begin
    Result := ViewInfo.HeaderViewInfo.BandsViewInfo[DestBand.VisibleIndex].HeaderViewInfo.Bounds;
    case DestInsertPosition of
      posLeft:
      begin
        if ViewInfo.UseRightToLeftAlignment then
          Result.Right := Result.Left;
      end;
      posRight:
      begin
        if not ViewInfo.UseRightToLeftAlignment then
          Result.Left := Result.Right;
      end;
      posBottom:
        Result.Top := Result.Bottom;
    end;
  end;
end;

function TcxGridBandHeaderMovingObject.GetArrowsClientRect: TRect;
begin
  Result := inherited GetArrowsClientRect;
  if (DestBandContainerKind = bcHeader) and (SourceBand.FixedKind = fkNone) then
    Result := ViewInfo.ScrollableAreaBoundsHorz;
end;

function TcxGridBandHeaderMovingObject.GetCustomizationFormListBox: TcxCustomGridItemsListBox;
begin
  Result := CustomizationForm.BandsListBox;
end;

function TcxGridBandHeaderMovingObject.GetSourceItemViewInfo: TcxCustomGridCellViewInfo;
begin
  if SourceBandContainerKind = bcHeader then
    Result := ViewInfo.HeaderViewInfo.BandsViewInfo[SourceBand.VisibleIndex].HeaderViewInfo
  else
    Result := inherited GetSourceItemViewInfo;
end;

function TcxGridBandHeaderMovingObject.IsSourceCustomizationForm: Boolean;
begin
  Result := SourceBandContainerKind = bcCustomizationForm;
end;

function TcxGridBandHeaderMovingObject.IsValidDestination: Boolean;

  function GetNearestColIndex(AForward: Boolean): Integer;
  begin
    Result := SourceBand.Position.VisibleColIndex;
    if AForward then
      Inc(Result)
    else
      Dec(Result);
  end;

  function GetVisibleBandCount(AFixedKind: TcxGridBandFixedKind): Integer;
  begin
    with GridView.Bands do
      Result := GetLastVisibleIndex(AFixedKind) - GetFirstVisibleIndex(AFixedKind) + 1;
  end;

var
  APos: TcxPosition;
begin
  if IsEmptyViewInsert then
  begin
    Result := True;
    Exit;
  end;
  Result := (DestBand <> nil) and (DestBand <> SourceBand);
  if Result then
  begin
    Result := DestBand.FixedKind = SourceBand.FixedKind;

    if not Result and not SourceBand.Visible and
      (GetVisibleBandCount(SourceBand.FixedKind) = 0) and DestBand.IsRoot then
      case SourceBand.FixedKind of
        fkLeft:
          Result := ViewInfo.UseRightToLeftAlignment and (DestInsertPosition = posRight) and
            (DestBand.VisibleRootIndex = GridView.Bands.GetFirstVisibleRootIndex(fkNone)) or
            not ViewInfo.UseRightToLeftAlignment and (DestInsertPosition = posLeft) and
            (DestBand.VisibleRootIndex = GridView.Bands.GetFirstVisibleRootIndex(fkNone));
        fkNone:
          Result :=
            (DestInsertPosition = posRight) and
            (ViewInfo.UseRightToLeftAlignment and (DestBand.VisibleRootIndex = GridView.Bands.GetLastVisibleRootIndex(fkRight)) or
              not ViewInfo.UseRightToLeftAlignment and (DestBand.VisibleRootIndex = GridView.Bands.GetLastVisibleRootIndex(fkLeft))) or
            (DestInsertPosition = posLeft) and
            (ViewInfo.UseRightToLeftAlignment and (DestBand.VisibleRootIndex = GridView.Bands.GetFirstVisibleRootIndex(fkLeft)) or
              not ViewInfo.UseRightToLeftAlignment and (DestBand.VisibleRootIndex = GridView.Bands.GetFirstVisibleRootIndex(fkRight)));
        fkRight:
          Result := ViewInfo.UseRightToLeftAlignment and (DestInsertPosition = posLeft) and
            (DestBand.VisibleRootIndex = GridView.Bands.GetLastVisibleRootIndex(fkNone)) or
            not ViewInfo.UseRightToLeftAlignment and (DestInsertPosition = posRight) and
            (DestBand.VisibleRootIndex = GridView.Bands.GetLastVisibleRootIndex(fkNone));
      end;

    if Result and SourceBand.Visible then
      case DestInsertPosition of
        posLeft, posRight:
          begin
            if ViewInfo.UseRightToLeftAlignment then
              APos := posRight
            else
              APos := posLeft;
            Result := (DestBand.ParentBand <> SourceBand.ParentBand) or
              (DestBand.Position.VisibleColIndex <> GetNearestColIndex(DestInsertPosition = APos));
          end;
        posTop:
          Result := (DestBand.ParentBand <> SourceBand) or (SourceBand.VisibleChildBandCount > 1);
        posBottom:
          Result := (SourceBand.ParentBand <> DestBand) or (DestBand.VisibleChildBandCount > 1);
      end;
  end;
end;

procedure TcxGridBandHeaderMovingObject.BeginDragAndDrop;
begin
  if CustomizationForm <> nil then
    with CustomizationForm do
      ActivatePage(BandsPage);
  Controller.FMovingBand := SourceBand;
  inherited;
end;

procedure TcxGridBandHeaderMovingObject.DragAndDrop(const P: TPoint; var Accepted: Boolean);
var
  AHitTest: TcxCustomGridHitTest;
  ABand: TcxGridBand;
  AInsertPosition: TcxPosition;
begin
  AHitTest := ViewInfo.GetHitTest(P);
  if AHitTest.HitTestCode = htCustomizationForm then
  begin
    DestBandContainerKind := bcCustomizationForm;
    DestBand := nil;
  end
  else
  begin
    DestBandContainerKind := bcHeader;
    ViewInfo.HeaderViewInfo.BandsViewInfo.InsertPositionAtPos(P, ABand, AInsertPosition);
    DestBand := ABand;
    DestInsertPosition := AInsertPosition;
  end;
  IsEmptyViewInsert := CalculateIsEmptyViewInsert(True);
  CheckDestParams;
  Accepted := (DestBandContainerKind = bcCustomizationForm) or
    (DestBand <> nil) or IsEmptyViewInsert;
  inherited;
end;

procedure TcxGridBandHeaderMovingObject.EndDragAndDrop(Accepted: Boolean);
var
  ABandPosChanged: Boolean;

  function GetParentBandIndex: Integer;
  begin
    if FDestInsertPosition = posBottom then
      Result := FDestBand.Index
    else
      Result := FDestBand.Position.BandIndex;
  end;

  function GetColIndex: Integer;
  var
    APos: TcxPosition;
  begin
    if FDestInsertPosition = posBottom then
      Result := 0
    else
    begin
      Result := FDestBand.Position.ColIndex;
      if ViewInfo.UseRightToLeftAlignment then
        APos := posLeft
      else
        APos := posRight;
      if FDestInsertPosition = APos then Inc(Result);
      if SourceBand.Position.ColIndex < Result then Dec(Result);
    end;
  end;

  procedure BeforePositionChange;
  var
    AColIndex, I: Integer;
  begin
    if (DestInsertPosition <> posBottom) and (GetParentBandIndex = SourceBand.Index) then
    begin
      AColIndex := SourceBand.Position.ColIndex;
      for I := SourceBand.ChildBandCount - 1 downto 0 do
        with SourceBand.ChildBands[I].Position do
        begin
          BandIndex := SourceBand.Position.BandIndex;
          ColIndex := AColIndex;
        end;
    end;
  end;

  procedure AfterPositionChange;
  var
    I: Integer;
  begin
    case DestInsertPosition of
      posTop:
        begin
          DestBand.Position.BandIndex := SourceBand.Index;
          DestBand.Position.ColIndex := 0;
        end;
      posBottom:
        begin
          I := 0;
          while I < DestBand.VisibleChildBandCount do
            if DestBand.VisibleChildBands[I] <> SourceBand then
              DestBand.VisibleChildBands[I].Position.BandIndex := SourceBand.Index
            else
              Inc(I);
        end;
    end;
  end;

begin
  inherited;
  Controller.FMovingBand := nil;
  if Accepted then
  begin
    ABandPosChanged := False;
    GridView.BeginUpdate;
    try
      if DestBand = nil then
        if CanRemove then
        begin
          SourceBand.Visible := False;
          ABandPosChanged := True;
        end
        else
          if IsEmptyViewInsert then
          begin
            SourceBand.Position.BandIndex := -1;
            SourceBand.Visible := True;
            ABandPosChanged := True;
          end
          else
      else
        if IsValidDestination then
        begin
          BeforePositionChange;
          SourceBand.Position.BandIndex := GetParentBandIndex;
          SourceBand.Position.ColIndex := GetColIndex;
          AfterPositionChange;
          SourceBand.Visible := True;
          ABandPosChanged := True;
        end;
    finally
      GridView.EndUpdate;
    end;
    if ABandPosChanged then
      GridView.DoBandPosChanged(SourceBand);
  end;
end;

procedure TcxGridBandHeaderMovingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  with AParams as TcxGridBandHeaderHitTest do
  begin
    SourceBand := Band;
    SourceBandContainerKind := BandContainerKind;
  end;
end;

{ TcxGridColumnVertSizingObject }

function TcxGridColumnVertSizingObject.GetColumn: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited Column);
end;

function TcxGridColumnVertSizingObject.GetController: TcxGridBandedTableController;
begin
  Result := TcxGridBandedTableController(inherited Controller);
end;

function TcxGridColumnVertSizingObject.GetLineHeight: Integer;
begin
  Result := ViewInfo.HeaderViewInfo.ItemHeight;
end;

procedure TcxGridColumnVertSizingObject.BeginDragAndDrop;
begin
  OriginalSize := ColumnHeaderViewInfo.Height;
  Controller.FVertSizingColumn := Column;
  inherited;
end;

procedure TcxGridColumnVertSizingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  if Accepted then
  begin
    Column.Position.LineCount := CurrentSize div LineHeight;
    TcxGridBandedTableView(GridView).DoColumnSizeChanged(Column);
  end;
  Controller.FVertSizingColumn := nil;
end;

function TcxGridColumnVertSizingObject.GetCurrentSize: Integer;
begin
  Result := MulDiv(inherited GetCurrentSize, 1, LineHeight);
  Column.Position.CheckLineCount(Result);
  Result := Result * LineHeight;
end;

function TcxGridColumnVertSizingObject.GetIsHorizontalSizing: Boolean;
begin
  Result := False;
end;

{ TcxGridBandSizingObject }

function TcxGridBandSizingObject.GetBandViewInfo: TcxGridBandViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(ViewInfo).HeaderViewInfo.BandsViewInfo[Band.VisibleIndex];
end;

function TcxGridBandSizingObject.GetController: TcxGridBandedTableController;
begin
  Result := TcxGridBandedTableController(inherited Controller);
end;

function TcxGridBandSizingObject.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandSizingObject.BeginDragAndDrop;
begin
  OriginalSize := BandViewInfo.ContentWidth;
  Controller.FSizingBand := Band;
  inherited;
end;

procedure TcxGridBandSizingObject.EndDragAndDrop(Accepted: Boolean);
begin
  inherited;
  Controller.FSizingBand := nil;
  if Accepted and (CurrentSize <> OriginalSize) then
  begin
    Band.ForceWidth(CurrentSize);
    GridView.DoBandSizeChanged(Band);
  end;
end;

function TcxGridBandSizingObject.GetCurrentSize: Integer;
begin
  Result := inherited GetCurrentSize;
  BandViewInfo.CheckWidth(Result);
end;

function TcxGridBandSizingObject.GetSizingItemBounds: TRect;
begin
  Result := BandViewInfo.ContentBounds;
end;

function TcxGridBandSizingObject.GetSizingMarkWidth: Integer;
begin
  Result := BandSizingMarkWidth;
end;

procedure TcxGridBandSizingObject.Init(const P: TPoint; AParams: TcxCustomGridHitTest);
begin
  inherited;
  Band := (AParams as TcxGridBandHeaderSizingEdgeHitTest).Band;
end;

{ TcxGridBandedTableBandsListBox }

function TcxGridBandedTableBandsListBox.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableBandsListBox.DoRefreshItems;
var
  I: Integer;
  ABand: TcxGridBand;
begin
  inherited;
  with Items do
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to GridView.Bands.Count - 1 do
      begin
        ABand := GridView.Bands[I];
        if ABand.CanMove and ABand.VisibleForCustomization and not ABand.Visible then
          AddObject(ABand.GetAlternateCaption, ABand);
      end;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxGridBandedTableBandsListBox.DrawItemDrawBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin
  Result := GridView.ViewInfo.HeaderViewInfo.BandsViewInfo.DrawBandHeaderBackgroundHandler(ACanvas, ABounds);
end;

function TcxGridBandedTableBandsListBox.GetDragAndDropParams: TcxCustomGridHitTest;
begin
  Result := TcxGridBandHeaderHitTest.Instance(Point(-1, -1));
  with TcxGridBandHeaderHitTest(Result) do
  begin
    GridView := Self.GridView;
    Band := TcxGridBand(DragAndDropItem);
    BandContainerKind := bcCustomizationForm;
  end;
end;

function TcxGridBandedTableBandsListBox.GetItemEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.BandHeaderEndEllipsis;
end;

{ TcxGridBandedTableCustomizationForm }

function TcxGridBandedTableCustomizationForm.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableCustomizationForm.CreateControls;

  procedure CreateBandsListBox;
  begin
    FBandsListBox := GetBandsListBoxClass.Create(Self);
    with FBandsListBox do
    begin
      Align := alClient;
      Parent := FBandsPage;
      RefreshItems;
    end;
  end;

begin
  inherited;
  CreateBandsListBox;
end;

procedure TcxGridBandedTableCustomizationForm.InitPageControl;
var
  AIsBandsVisible: Boolean;
begin
  inherited;
  AIsBandsVisible := GridView.OptionsCustomize.BandMoving;
  FBandsPage := CreatePage(cxGetResourceString(@scxGridCustomizationFormBandsPageCaption), AIsBandsVisible);
  if AIsBandsVisible then
  begin
    FBandsPage.PageIndex := 0;
    ActivatePage(FBandsPage);
  end;
end;

function TcxGridBandedTableCustomizationForm.GetBandsListBoxClass: TcxGridBandedTableBandsListBoxClass;
begin
  Result := TcxGridBandedTableBandsListBox;
end;

procedure TcxGridBandedTableCustomizationForm.RefreshData;
begin
  inherited;
  FBandsListBox.RefreshItems;
end;

{ TcxGridBandsCustomizationPopup }

function TcxGridBandsCustomizationPopup.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandsCustomizationPopup.AddCheckListBoxItems;

  procedure AddBandToItems(AItems: TdxCustomCheckListBoxItems; ABand: TcxGridBand);
  var
    I: Integer;
  begin
    if ABand.VisibleForCustomization then
      AItems.AddObject(ABand.GetAlternateCaption, ABand).Checked := ABand.Visible;
    if ABand.Visible then
      for I := 0 to ABand.ChildBandCount - 1 do
        AddBandToItems(AItems, ABand.ChildBands[I]);
  end;

var
  I: Integer;
begin
  CheckListBox.Items.BeginUpdate;
  try
    CheckListBox.Items.Clear;
    CheckListBox.Sorted := False;
    for I := 0 to GridView.Bands.RootItemCount - 1 do
      AddBandToItems(CheckListBox.Items, GridView.Bands.RootItems[I]);
    SetQuickCustomizationSortOptions;
    ApplyCheckListBoxSorting;
  finally
    CheckListBox.Items.EndUpdate;
  end;
end;

function TcxGridBandsCustomizationPopup.CanAddCommands: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomizationShowCommands;
end;

function TcxGridBandsCustomizationPopup.CanMoveItem(AItem: Pointer): Boolean;
begin
  Result := TcxGridBand(AItem).Options.Moving and not GridView.OptionsCustomize.BandsQuickCustomizationSorted;
end;

function TcxGridBandsCustomizationPopup.GetDropDownCount: Integer;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomizationMaxDropDownCount;
end;

function TcxGridBandsCustomizationPopup.IsCheckListBoxSorted: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomizationSorted;
end;

function TcxGridBandsCustomizationPopup.IsMultiColumnMode: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomizationMultiColumnMode;
end;

function TcxGridBandsCustomizationPopup.IsGridItem(AItem: TObject): Boolean;
begin
  Result := AItem is TcxGridBand;
end;

function TcxGridBandsCustomizationPopup.IsVisibleGridItem(AItem: TObject): Boolean;
begin
  Result := IsGridItem(AItem) and TcxGridBand(AItem).Visible;
end;

procedure TcxGridBandsCustomizationPopup.SetQuickCustomizationSorted(AValue: Boolean);
begin
  GridView.OptionsCustomize.BandsQuickCustomizationSorted := AValue;
end;

function TcxGridBandsCustomizationPopup.SupportsItemMoving: Boolean;
begin
  Result := GridView.OptionsCustomize.SupportsBandsQuickCustomizationReordering;
end;

procedure TcxGridBandsCustomizationPopup.DoItemPosChanged(AItem: TObject);
begin
  GridView.DoBandPosChanged(TcxGridBand(AItem));
end;

procedure TcxGridBandsCustomizationPopup.HandleItemClicked(AItem: TObject; AChecked: Boolean);
var
  AItems: TList;
  AFocusedItem: TObject;
  ASize: TSize;
  ACount: Integer;
  ACustomizationControl: TdxCustomGridQuickCustomizationControlAccess;
  P: TPoint;
begin
  ACount := CheckListBox.Count;
  TcxGridBand(AItem).Visible := AChecked;
  AItems := TList.Create;
  try
    GetCheckListBoxSelectedItems(AItems, AFocusedItem);
    RefreshCheckListBoxItems;
    SetCheckListBoxSelectedItems(AItems, AFocusedItem);
  finally
    AItems.Free;
  end;
  if ACount <> CheckListBox.Count then
  begin
    SetVisibleRowCount(GetDropDownCount);
    ACustomizationControl := TdxCustomGridQuickCustomizationControlAccess(CustomizationControl);
    ACustomizationControl.AdjustBounds;
    ASize := cxSize(
      BorderWidths[bLeft] + CustomizationControl.Width + BorderWidths[bRight],
      BorderWidths[bTop] + CustomizationControl.Height + BorderWidths[bBottom]);
    P := CalculatePosition(ASize);
    CorrectBoundsWithDesktopWorkArea(P, ASize);
    ACustomizationControl.RecreateWnd;
    SetBounds(P.X, P.Y, ASize.cx, ASize.cy);
    FocusControl(CheckListBox);
  end;

  DoItemPosChanged(AItem);
  SetGridModified;
end;

procedure TcxGridBandsCustomizationPopup.SynchronizeTableItemsVisibilityWithListBox;
var
  I: Integer;
begin
  for I := 0 to CheckListBox.Count - 1 do
    (CheckListBox.Items[I].Data as TcxGridBand).Visible := CheckListBox.Checked[I];
end;

function TcxGridBandsCustomizationPopup.GetItemIndex(AItem: TObject): Integer;
begin
  Result := TcxGridBand(AItem).Index;
end;

procedure TcxGridBandsCustomizationPopup.SetItemIndex(AItem: TObject; AIndex: Integer);
begin
  if TcxGridBand(AItem).Index < AIndex then
    Dec(AIndex);
  TcxGridBand(AItem).Index := AIndex;
  GridView.Controller.DesignerModified;
  DoItemPosChanged(AItem);
end;

{ TcxGridBandColumnsCustomizationPopup }

function dxCompareBandedColumnsByActualPosition(AItem1, AItem2: TdxCustomCheckListBoxItem): Integer;

  function GetRootBandIndexOf(ABand: TcxGridBand): Integer;
  begin
    Result := ABand.Index;
    while ABand.ParentBand <> nil do
    begin
      ABand := ABand.ParentBand;
      Result := ABand.Index;
    end;
  end;

var
  AColumn1, AColumn2: TcxGridBandedColumn;
begin
  AColumn1 := AItem1.Data as TcxGridBandedColumn;
  AColumn2 := AItem2.Data as TcxGridBandedColumn;
  Result := GetRootBandIndexOf(AColumn1.Position.Band) - GetRootBandIndexOf(AColumn2.Position.Band);
  if Result = 0 then
    Result := AColumn1.Position.Band.Position.BandIndex - AColumn2.Position.Band.Position.BandIndex;
  if Result = 0 then
    Result := AColumn1.Position.Band.Position.ColIndex - AColumn2.Position.Band.Position.ColIndex;
  if Result = 0 then
    Result := AColumn1.Position.ColIndex - AColumn2.Position.ColIndex;
  if Result = 0 then
    Result := AColumn1.Position.RowIndex - AColumn2.Position.RowIndex;
end;

procedure TcxGridBandedColumnsCustomizationPopup.ApplyCheckListBoxSorting;
var
  ACompareFunction: TListSortCompare;
begin
  if IsCheckListBoxSorted then
    inherited
  else
  begin
    ACompareFunction := @dxCompareBandedColumnsByActualPosition;
    if Assigned(GridView.OnQuickCustomizationColumnCompare) then
      GridView.OnQuickCustomizationColumnCompare(ACompareFunction);
    CheckListBox.Items.Sort(ACompareFunction);
  end;
end;

function TcxGridBandedColumnsCustomizationPopup.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

{ TcxGridBandedTableController }

destructor TcxGridBandedTableController.Destroy;
begin
  FBandsCustomizationPopup.Free;
  inherited;
end;

function TcxGridBandedTableController.GetBandsCustomizationPopup: TcxGridBandsCustomizationPopup;
begin
  if FBandsCustomizationPopup = nil then
    FBandsCustomizationPopup := GetBandsCustomizationPopupClass.Create(GridView);
  Result := FBandsCustomizationPopup;
end;

function TcxGridBandedTableController.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedTableController.GetIsBandMoving: Boolean;
begin
  Result := FMovingBand <> nil;
end;

function TcxGridBandedTableController.GetIsBandSizing: Boolean;
begin
  Result := FSizingBand <> nil;
end;

function TcxGridBandedTableController.GetIsColumnVertSizing: Boolean;
begin
  Result := FVertSizingColumn <> nil;
end;

function TcxGridBandedTableController.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandedTableController.SetPressedBand(Value: TcxGridBand);
var
  R: TRect;

  function GetUpdateRect: TRect;
  begin
    if FPressedBand.VisibleIndex = -1 then
      SetRectEmpty(Result)
    else
      Result := ViewInfo.HeaderViewInfo.BandsViewInfo[FPressedBand.VisibleIndex].HeaderViewInfo.Bounds;
  end;

begin
  if FPressedBand <> Value then
  begin
    if Value = nil then
      R := GetUpdateRect;
    FPressedBand := Value;
    if Value <> nil then
      R := GetUpdateRect;
    GridView.ViewChanged(R);
  end;
end;

function CanFocusColumn(AOwner: TcxCustomGridTableView; AItemIndex: Integer;
  AData: TObject): Boolean;
begin
  Result := TcxGridBandedColumn(TData(AData).Columns[AItemIndex]).CanFocus(TData(AData).GridRecord);
end;

function TcxGridBandedTableController.FindNextItemFollowVisualOrder(AFocusedItemIndex: Integer; AGoForward,
  AGoOnCycle: Boolean; out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer;
var
  AColumns: TList;
  AColumn: TcxGridBandedColumn;
  AData: TData;
begin
  ACycleChanged := False;
  AColumns := TList.Create;
  try
    if AFocusedItemIndex = -1 then
      AColumn := nil
    else
      AColumn := GridView.VisibleColumns[AFocusedItemIndex];
    GetColumnNeighbors(AColumn, AGoForward, AColumns);

    AData := TData.Create;
    try
      AData.Columns := AColumns;
      AData.GridRecord := ARecord;
      if FindNextCustomItem(-1, AColumns.Count, True, False,
        @CanFocusColumn, AData, Result, ACycleChanged) then
          Result := TcxGridBandedColumn(AColumns[Result]).VisibleIndex
      else
        if AGoOnCycle then
        begin
          Result := FindNextItem(-1, AGoForward, False, True, ACycleChanged, ARecord);
          ACycleChanged := True;
        end
        else
          Result := -1;
    finally
      AData.Free;
    end;
  finally
    AColumns.Free;
  end;
end;

function TcxGridBandedTableController.FindNextItemTabOrder(AFocusedItemIndex: Integer; AGoForward,
  AGoOnCycle: Boolean; out ACycleChanged: Boolean; ARecord: TcxCustomGridRecord): Integer;

  function GetItemIndexInTabOrderList(AList: TList; AIndex: Integer): Integer;
  var
    AColumn: TcxGridBandedColumn;
  begin
    if AIndex <> -1 then
    begin
      AColumn := GridView.VisibleColumns[AIndex];
      Result := AList.IndexOf(AColumn);
    end
    else
      Result := -1;
  end;

var
  AData: TData;
  ATabOrderList: TList;
  AItemIndex: Integer;
begin
  Result := -1;
  ATabOrderList := TList.Create;
  try
    PopulateTabOrderList(ATabOrderList);
    AData := TData.Create;
    try
      AData.Columns := ATabOrderList;
      AData.GridRecord := ARecord;
      AItemIndex := GetItemIndexInTabOrderList(ATabOrderList, AFocusedItemIndex);
      if FindNextCustomItem(AItemIndex, ATabOrderList.Count, AGoForward, AGoOnCycle,
        @CanFocusColumn, AData, AItemIndex, ACycleChanged) then
          Result := TcxGridBandedColumn(ATabOrderList[AItemIndex]).VisibleIndex;
    finally
      AData.Free;
    end;
  finally
    ATabOrderList.Free;
  end;
end;

function TcxGridBandedTableController.GetBandsCustomizationPopupClass: TcxGridBandsCustomizationPopupClass;
begin
  Result := TcxGridBandsCustomizationPopup;
end;

function TcxGridBandedTableController.GetItemsCustomizationPopupClass: TcxCustomGridItemsCustomizationPopupClass;
begin
  Result := TcxGridBandedColumnsCustomizationPopup;
end;

function TcxGridBandedTableController.GetColumnHeaderDragAndDropObjectClass: TcxGridColumnHeaderMovingObjectClass;
begin
  Result := TcxGridBandedColumnHeaderMovingObject;
end;

procedure TcxGridBandedTableController.GetColumnNeighbors(AColumn: TcxGridBandedColumn;
  AGoForward: Boolean; AList: TList);
var
  ABands: TcxGridBands;
  ANeighborColumn: TcxGridBandedColumn;

  function GetNextColumn(AColumn: TcxGridBandedColumn): TcxGridBandedColumn;
  var
    ABand: TcxGridBand;
    ARow: TcxGridBandRow;
    ARowIndex, AColIndex: Integer;

    function InitializeValues: Boolean;
    begin
      Result := False;
      if AColumn = nil then
        if AGoForward then
        begin
          ABand := ABands.FirstVisibleNonEmpty;
          if ABand = nil then Exit;
          ARow := ABand.Rows.FirstVisible;
          AColIndex := -1;
        end
        else
        begin
          ABand := ABands.LastVisibleNonEmpty;
          if ABand = nil then Exit;
          ARow := ABand.Rows.LastVisible;
          AColIndex := ARow.VisibleCount;
        end
      else
        with AColumn.Position do
        begin
          ABand := Band;
          ARow := Row;
          ARowIndex := VisibleRowIndex;
          AColIndex := VisibleColIndex;
        end;
      Result := ARow <> nil;
    end;

    function ProcessRow: Boolean;
    begin
      Result :=
        (AColumn <> nil) and (AGoForward and not AColumn.IsRight or not AGoForward and not AColumn.IsLeft) or
        (AColumn = nil) and (ARow.VisibleCount <> 0);
      if Result then
        GetNextColumn := ARow.VisibleItems[AColIndex + 2 * Ord(AGoForward) - 1];
    end;

    function ProcessBand: Boolean;
    var
      ALineIndex, AOriginalRowIndex: Integer;
      ARowChanged: Boolean;

      procedure FindNextBand;
      begin
        if AGoForward and ABand.IsLast then
        begin
          ABand := ABands.FirstVisibleNonEmpty;
          ARowIndex := AOriginalRowIndex + 1;
          ARowChanged := True;
        end
        else
          if not AGoForward and ABand.IsFirst then
          begin
            ABand := ABands.LastVisibleNonEmpty;
            ARowIndex := AOriginalRowIndex - 1;
            ARowChanged := True;
          end
          else
          begin
            ABand := ABands.VisibleBottomItems[ABand.VisibleBottomIndex + 2 * Ord(AGoForward) - 1];
            if not ARowChanged then
              ARowIndex := ABand.Rows.GetRowIndex(ALineIndex);
          end;
      end;

    begin
      if ARowIndex = -1 then
        ALineIndex := 0
      else
        ALineIndex := ABand.Rows.GetLineIndex(ARowIndex);
      AOriginalRowIndex := ARowIndex;
      ARowChanged := False;
      repeat
        FindNextBand;
        Result := (0 <= ARowIndex) and (ARowIndex < ABand.Rows.VisibleCount);
      until Result or
        AGoForward and ABand.IsFirstNonEmpty and (ARowIndex = ABands.VisibleRowCount) or
        not AGoForward and ABand.IsLastNonEmpty and (ARowIndex = -1);
      if Result then
      begin
        AColumn := nil;
        ARow := ABand.Rows.VisibleItems[ARowIndex];
        if AGoForward then
          AColIndex := -1
        else
          AColIndex := ARow.VisibleCount;
      end;
    end;

  begin
    Result := nil;
    if not InitializeValues then Exit;
    while not ProcessRow do
      if not ProcessBand then Break;
  end;

begin
  AList.Clear;
  if (AColumn <> nil) and not AColumn.ActuallyVisible then Exit;
  ABands := GridView.Bands;
  ANeighborColumn := AColumn;
  repeat
    ANeighborColumn := GetNextColumn(ANeighborColumn);
    if (ANeighborColumn = nil) or (AList.IndexOf(ANeighborColumn) <> -1) then
      Break
    else
      AList.Add(ANeighborColumn);
  until False;
end;

function TcxGridBandedTableController.GetCustomizationFormClass: TcxCustomGridCustomizationFormClass;
begin
  Result := TcxGridBandedTableCustomizationForm;
end;

function TcxGridBandedTableController.GetDesignHitTest(AHitTest: TcxCustomGridHitTest): Boolean;
begin
  Result := inherited GetDesignHitTest(AHitTest);
  if not Result then
    Result := AHitTest.HitTestCode in [htBandHeader, htIndicatorBandHeader];
end;

function TcxGridBandedTableController.GetPatternObject(AObject: TPersistent): TPersistent;
begin
  if AObject is TcxGridBand then
    Result := TcxGridBandedTableView(GridView.PatternGridView).Bands.FindItemID(TcxGridBand(AObject).ID)
  else
    Result := inherited GetPatternObject(AObject);
end;

function TcxGridBandedTableController.IsBandFixedDuringSizing(ABand: TcxGridBand): Boolean;
begin
  Result := (ABand = ForcingWidthBand) or
    not ForcingWidthBand.IsLastAsChild and (ABand.ParentBand = ForcingWidthBand.ParentBand) and
    (ABand.Position.VisibleColIndex < ForcingWidthBand.Position.VisibleColIndex);
end;

function TcxGridBandedTableController.IsColumnFixedDuringHorzSizing(AColumn: TcxGridColumn): Boolean;
var
  APosition1, APosition2: TcxGridBandedColumnPosition;
begin
  APosition1 := TcxGridBandedColumn(AColumn).Position;
  APosition2 := TcxGridBandedColumn(ForcingWidthItem).Position;
  Result :=
    (APosition1 = APosition2) or
    not APosition2.Item.IsRight and (APosition1.Row = APosition2.Row) and
    (APosition1.VisibleColIndex < APosition2.VisibleColIndex);
end;

procedure TcxGridBandedTableController.LeftPosChanged;
begin
  with GridView do
    if Preview.Active and Preview.AutoHeight and (Bands.Layout <> blNonFixed) then
      SizeChanged
    else
      inherited;
end;

procedure TcxGridBandedTableController.PopulateTabOrderList(AList: TList);
begin
  GridView.PopulateTabOrderList(AList);
end;

function SortByBandLevel(Item1, Item2: Pointer): Integer;
var
  ALevel1, ALevel2: Integer;
begin
  ALevel1 := -1;
  if TObject(Item1) is TcxGridBand then
    ALevel1 := TcxGridBand(Item1).BandLevelIndex
  else
    if TcxGridBandedColumn(Item1).Position.Band <> nil then
      ALevel1 := TcxGridBandedColumn(Item1).Position.Band.BandLevelIndex;
  ALevel2 := -1;
  if TObject(Item2) is TcxGridBand then
    ALevel2 := TcxGridBand(Item2).BandLevelIndex
  else
    if TcxGridBandedColumn(Item2).Position.Band <> nil then
      ALevel2 := TcxGridBandedColumn(Item2).Position.Band.BandLevelIndex;
  Result := ALevel1 - ALevel2;
end;

procedure TcxGridBandedTableController.CreateGridViewItem(Sender: TObject);
var
  AColumn: TcxGridBandedColumn;
  AList: TObjectList;
  I, ABandIndex, AColIndex: Integer;
  AGridView: TcxCustomGridView;
begin
  if DesignController.CanAddComponent then
  begin
    AGridView := GridView.PatternGridView;
    AGridView.BeginUpdate;
    try
      AList := TObjectList.Create(False);
      try
        DesignController.GetSelection(AList);
        ABandIndex := -1;
        for I := AList.Count - 1 downto 0 do
        begin
          if AList[I] is TcxGridBand then
          begin
            ABandIndex := TcxGridBand(AList[I]).Index;
            Break;
          end;
        end;
        AColIndex := -1;
        for I := AList.Count - 1 downto 0 do
        begin
          if AList[I] is TcxGridBandedColumn then
            with TcxGridBandedColumn(AList[I]).Position do
            begin
              AColIndex := ColIndex + 1;
              if ABandIndex < 0 then
                ABandIndex := BandIndex;
              Break;
            end;
        end;
      finally
        AList.Free;
      end;
      AColumn := CreateViewItem(AGridView as TcxGridBandedTableView) as TcxGridBandedColumn;
      AColumn.Position.BandIndex := ABandIndex;
      if AColIndex >= 0 then
        AColumn.Position.ColIndex := AColIndex;
      DesignController.SelectObject(AColumn, True);
    finally
      AGridView.EndUpdate;
      DesignController.NotifyEditors;
    end;
  end;
end;

procedure TcxGridBandedTableController.CreateChildBandForSelection(Sender: TObject);
var
  AList: TObjectList;
  I: Integer;
  AGridView: TcxGridBandedTableView;
  AParentBand, ABand: TcxGridBand;
begin
  AGridView := GridView.PatternGridView as TcxGridBandedTableView;
  AList := TObjectList.Create(False);
  try
    DesignController.GetSelection(AList);
    for I := AList.Count - 1 downto 0 do
      if AList[I] is TcxGridBand then
      begin
        AParentBand := TcxGridBand(AList[I]);
        AGridView.BeginUpdate;
        try
          ABand := AGridView.Bands.Add;
          ABand.Position.BandIndex := AParentBand.Index;
          DesignController.SelectObject(ABand, True);
        finally
          AGridView.EndUpdate;
        end;
        Break;
      end;
  finally
    AList.Free;
    SetDesignerModified(AGridView);
    DesignController.NotifyEditors;
  end;
end;

procedure TcxGridBandedTableController.CreateParentBandForSelection(Sender: TObject);

  function HasAsChild(AParentBand, ABand: TcxGridBand): Boolean; overload;
  begin
    Result := AParentBand = nil;
    if not Result then
    begin
      while ABand <> nil do
      begin
        if ABand.ParentBand = AParentBand then
        begin
          Result := True;
          Break;
        end;
        ABand := ABand.ParentBand;
      end;
    end;
  end;

  function HasAsChild(AParentBand: TcxGridBand; AColumn: TcxGridBandedColumn): Boolean; overload;
  var
    ABand: TcxGridBand;
  begin
    Result := (AParentBand = nil) or (AColumn.Position.Band = AParentBand);
    if not Result then
    begin
      ABand := AColumn.Position.Band;
      while ABand <> nil do
      begin
        if ABand.ParentBand = AParentBand then
        begin
          Result := True;
          Break;
        end;
        ABand := ABand.ParentBand;
      end;
    end;
  end;

  procedure UpdateCommonParentBand(var AParentBand: TcxGridBand; ABand: TcxGridBand); overload;
  begin
    if AParentBand = nil then
      Exit;
    if ABand = nil then
    begin
      AParentBand := nil;
      Exit;
    end;
    if HasAsChild(ABand, AParentBand) then
    begin
      AParentBand := ABand;
      Exit;
    end;
    while AParentBand <> nil do
    begin
      if HasAsChild(AParentBand, ABand) then
        Break;
      AParentBand := AParentBand.ParentBand;
    end;
  end;

  procedure UpdateCommonParentBand(var AParentBand: TcxGridBand; AColumn: TcxGridBandedColumn); overload;
  var
    ABand: TcxGridBand;
  begin
    if AParentBand = nil then
      Exit;
    ABand := AColumn.Position.Band;
    if ABand = nil then
    begin
      AParentBand := nil;
      Exit;
    end;
    while AParentBand <> nil do
    begin
      if HasAsChild(AParentBand, AColumn) then
        Break;
      AParentBand := AParentBand.ParentBand;
    end;
  end;

  function FindBestColIndex(AList: TList): Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := AList.Count - 1 downto 0 do
      if TObject(AList[I]) is TcxGridBand then
      begin
        Result := TcxGridBand(AList[I]).Position.ColIndex;
        Break;
      end;
  end;

var
  AList: TList;
  I: Integer;
  AColumn: TcxGridBandedColumn;
  AFirstBand: Boolean;
  AGridView: TcxGridBandedTableView;
  AParentBand, ABand: TcxGridBand;
begin
  AGridView := GridView.PatternGridView as TcxGridBandedTableView;
  AList := TList.Create;
  try
    DesignController.GetSelection(AList);
    AFirstBand := False;
    AParentBand := nil;
    for I := AList.Count - 1 downto 0 do
    begin
      if TObject(AList[I]) is TcxGridBand then
      begin
        ABand := TcxGridBand(AList[I]);
        if not AFirstBand then
        begin
          AFirstBand := True;
          if ABand <> nil then
            AParentBand := ABand.ParentBand;
        end
        else
          UpdateCommonParentBand(AParentBand, ABand);
      end
      else
        if TObject(AList[I]) is TcxGridBandedColumn then
        begin
          AColumn := TcxGridBandedColumn(AList[I]);
          if not AFirstBand then
          begin
            AFirstBand := True;
            AParentBand := AColumn.Position.Band;
          end
          else
            UpdateCommonParentBand(AParentBand, AColumn);
        end
        else
        begin
          AList.Delete(I);
          Continue;
        end;
    end;
    AList.Sort(SortByBandLevel);
    AGridView.BeginUpdate;
    try
      ABand := AGridView.Bands.Add;
      DesignController.SelectObject(ABand, True);
      if AParentBand <> nil then
        ABand.Position.BandIndex := AParentBand.Index
      else
        ABand.Position.ColIndex := FindBestColIndex(AList);
      AParentBand := ABand;
      for I := AList.Count - 1 downto 0 do
      begin
        if TObject(AList[I]) is TcxGridBand then
        begin
          ABand := TcxGridBand(AList[I]);
          if not HasAsChild(AParentBand, ABand) then
            ABand.Position.BandIndex := AParentBand.Index
        end
        else
          if TObject(AList[I]) is TcxGridBandedColumn then
          begin
            AColumn := TcxGridBandedColumn(AList[I]);
            if not HasAsChild(AParentBand, AColumn) then
              AColumn.Position.BandIndex := ABand.Index;
          end;
      end;
    finally
      AGridView.EndUpdate;
    end;
  finally
    AList.Free;
    SetDesignerModified(AGridView);
    DesignController.NotifyEditors;
  end;
end;

procedure TcxGridBandedTableController.DeleteGridViewItem(AItem: TPersistent);
begin
  if AItem is GridView.Bands.GetBandClass then
  begin
    AItem.Free;
    SetDesignerModified(GridView);
  end
  else
    inherited;
end;

procedure TcxGridBandedTableController.PopulateColumnHeaderDesignPopupMenu(AMenu: TPopupMenu);

  function CanCreateChildBandForSelection: Boolean;
  var
    AList: TObjectList;
  begin
    AList := TObjectList.Create(False);
    try
      DesignController.GetSelection(AList);
      Result := (AList.Count = 1) and (AList[0] is TcxGridBand)
    finally
      AList.Free;
    end;
  end;

begin
  AMenu.Items.Add(NewItem('Create Parent Band', 0, False, True,
    CreateParentBandForSelection, 0, 'chmiCreateParentBand'));
  AMenu.Items.Add(NewItem('Create Child Band', 0, False, CanCreateChildBandForSelection,
    CreateChildBandForSelection, 0, 'chmiCreateChildBand'));
  AMenu.Items.Add(NewItem('Create Column', 0, False, True, CreateGridViewItem, 0, 'chmiCreateColumn'));
  AMenu.Items.Add(NewLine);
  AMenu.Items.Add(NewItem('Delete', 0, False, True, DeleteGridViewItems, 0, 'chmiDelete'));
end;

function TcxGridBandedTableController.GetCellMultiSelect: Boolean;
begin
  Result := inherited GetCellMultiSelect and (ViewInfo.HeaderViewInfo.RowCount = 1);
end;

procedure TcxGridBandedTableController.DoCancelMode;
begin
  inherited;
  PressedBand := nil;
end;

procedure TcxGridBandedTableController.EndDragAndDrop(Accepted: Boolean);
begin
  PressedBand := nil;
  inherited;
end;

function TcxGridBandedTableController.HasBandsCustomizationPopup: Boolean;
begin
  Result := FBandsCustomizationPopup <> nil;
end;

function CanFocusColumnVertically(AOwner: TcxCustomGridTableView;
  AItemIndex: Integer; AData: TObject): Boolean;
begin
  Result := TcxGridBandedColumn(TList(AData)[AItemIndex]).CanFocus(nil);
end;

function TcxGridBandedTableController.FindNextColumnVertically(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle: Boolean): Integer;
var
  AColumn: TcxGridBandedColumn;
  AColumns: TList;
  ACycleChanged: Boolean;

  procedure InitializeColumns;
  var
    AColIndex, I, AIndex: Integer;
  begin
    AColIndex := AColumn.Position.VisibleColIndex;
    with AColumn.Position.Band.Rows do
      for I := 0 to VisibleCount - 1 do
        with VisibleItems[I] do
        begin
          AIndex := AColIndex;
          if AIndex > VisibleCount - 1 then
            AIndex := VisibleCount - 1;
          AColumns.Add(VisibleItems[AIndex]);
        end;
  end;

begin
  if AFocusedItemIndex = -1 then
    Result := -1
  else
  begin
    AColumn := GridView.VisibleColumns[AFocusedItemIndex];
    AColumns := TList.Create;
    try
      InitializeColumns;
      if FindNextCustomItem(AColumns.IndexOf(AColumn), AColumns.Count,
        AGoForward, AGoOnCycle, @CanFocusColumnVertically, AColumns, Result,
          ACycleChanged) then
            Result := TcxGridBandedColumn(AColumns[Result]).VisibleIndex
      else
        Result := -1;
    finally
      AColumns.Free;
    end;
  end;
end;

function TcxGridBandedTableController.FindNextItem(AFocusedItemIndex: Integer;
  AGoForward, AGoOnCycle, AFollowVisualOrder: Boolean; out ACycleChanged: Boolean;
  ARecord: TcxCustomGridRecord): Integer;
begin
  if AFollowVisualOrder then
    Result := FindNextItemFollowVisualOrder(AFocusedItemIndex, AGoForward, AGoOnCycle, ACycleChanged, ARecord)
  else
    Result := FindNextItemTabOrder(AFocusedItemIndex, AGoForward, AGoOnCycle, ACycleChanged, ARecord);
end;

function TcxGridBandedTableController.FocusNextColumnVertically(AFocusedColumnIndex: Integer;
  AGoForward, AGoOnCycle: Boolean): Boolean;
var
  ANextItemIndex: Integer;
begin
  ANextItemIndex := FindNextColumnVertically(AFocusedColumnIndex, AGoForward, AGoOnCycle);
  Result := ANextItemIndex <> -1;
  if Result then
    GridView.VisibleItems[ANextItemIndex].Focused := True;
end;

procedure TcxGridBandedTableController.KeyDown(var Key: Word; Shift: TShiftState);
var
  APrevFocusedRowIndex: Integer;
  APrevFocusedRowHasColumns: Boolean;

  function IsFocusedRecordChanged: Boolean;
  begin
    Result := (FocusedRowIndex <> APrevFocusedRowIndex) or
      DataController.IsGridMode and
        ((Key = VK_UP) and not DataController.IsBOF or (Key = VK_DOWN) and not DataController.IsEOF);
  end;

begin
  if InplaceEditForm.Visible then
    inherited KeyDown(Key, Shift)
  else
  begin
    APrevFocusedRowIndex := FocusedRowIndex;
    APrevFocusedRowHasColumns := (FocusedRow <> nil) and
      FocusedRow.HasCells and GridView.OptionsSelection.CellSelect;
    if APrevFocusedRowHasColumns then
      case Key of
        VK_UP:
          if FocusNextColumnVertically(FocusedColumnIndex, False, False) then
            Key := 0;
        VK_DOWN:
          if FocusNextColumnVertically(FocusedColumnIndex, True, False) then
            Key := 0;
      end;
    inherited KeyDown(Key, Shift);
    if APrevFocusedRowHasColumns and IsFocusedRecordChanged and
      (FocusedRow <> nil) and FocusedRow.HasCells then
      case Key of
        VK_UP:
          FocusNextColumnVertically(FocusedColumnIndex, False, True);
        VK_DOWN:
          FocusNextColumnVertically(FocusedColumnIndex, True, True);
      end;
  end;
end;

procedure TcxGridBandedTableController.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: Integer);
begin
  try
    inherited;
    if Site.IsMouseInPressedArea(X, Y) and (PressedBand <> nil) then
      PressedBand.DoHeaderClick;
  finally
    PressedBand := nil;
  end;
end;

{ TcxGridBandHeaderPainter }

function TcxGridBandHeaderPainter.GetViewInfo: TcxGridBandHeaderViewInfo;
begin
  Result := TcxGridBandHeaderViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandHeaderPainter.DrawBorders;
begin
  // inherited;
end;

procedure TcxGridBandHeaderPainter.DrawContent;
var
  AState: TcxButtonState;
begin
  with ViewInfo do
  begin
    if IsMainCanvasInUse then
      AState := ButtonState
    else
      AState := cxbsNormal;
    LookAndFeelPainter.DrawScaledHeader(Self.Canvas, Bounds, TextAreaBounds, [],
      Borders, AState, AlignmentHorz, AlignmentVert, MultiLinePainting, ShowEndEllipsis,
      Text, Params.Font, Params.TextColor, Params.Color, ScaleFactor,
      BandViewInfo.BandsViewInfo.DrawBandHeaderBackgroundHandler, Band.IsMostRight);
  end;
end;

procedure TcxGridBandHeaderPainter.DrawPressed;
begin
  with ViewInfo do
    LookAndFeelPainter.DrawHeaderPressed(Self.Canvas, Bounds);
end;

function TcxGridBandHeaderPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxGridBandHeaderPainter.Paint;
begin
  inherited;
  if ViewInfo.IsPressed and IsMainCanvasInUse then DrawPressed;
end;

{ TcxGridBandPainter }

function TcxGridBandPainter.GetViewInfo: TcxGridBandViewInfo;
begin
  Result := TcxGridBandViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandPainter.DoExcludeFromClipRect;
begin
  Canvas.ExcludeClipRect(ViewInfo.ExcludeBounds);
end;

procedure TcxGridBandPainter.DrawChildBands;
var
  I: Integer;
begin
  with ViewInfo do
    for I := 0 to ChildBandViewInfoCount - 1 do
      ChildBandViewInfos[I].Paint;
end;

procedure TcxGridBandPainter.DrawColumnHeaders;
var
  I, J: Integer;
begin
  with ViewInfo do
    for I := 0 to RowCount - 1 do
      with RowsViewInfo[I] do
        for J := 0 to ColumnViewInfoCount - 1 do
          ColumnViewInfos[J].Paint;
end;

procedure TcxGridBandPainter.DrawContent;
begin
  DrawHeader;
  if ViewInfo.IsBottom then
    DrawColumnHeaders
  else
    DrawChildBands;
  inherited;
end;

procedure TcxGridBandPainter.DrawHeader;
begin
  ViewInfo.HeaderViewInfo.Paint;
end;

function TcxGridBandPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

{ TcxGridBandedHeaderPainter }

procedure TcxGridBandedHeaderPainter.DrawBands;

  procedure ProcessItems(AFixedKind: TcxGridBandFixedKind);
  var
    I: Integer;
    AItem: TcxGridBandViewInfo;
  begin
    with ViewInfo.BandsViewInfo do
      for I := 0 to RootItemCount - 1 do
      begin
        AItem := RootItems[I];
        if AItem.FixedKind = AFixedKind then
          AItem.Paint;
      end;
  end;

begin
  ProcessItems(fkLeft);
  ProcessItems(fkRight);
  ProcessItems(fkNone);
end;

procedure TcxGridBandedHeaderPainter.DrawItems;
begin
  DrawBands;
end;

function TcxGridBandedHeaderPainter.GetViewInfo: TcxGridBandedHeaderViewInfo;
begin
  Result := TcxGridBandedHeaderViewInfo(inherited ViewInfo);
end;

{ TcxGridBandedFooterPainter }

procedure TcxGridBandedFooterPainter.DrawItems;
var
  AClipRegion: TcxRegion;
  AFixedPartExcluded: array[TcxGridBandFixedKind] of Boolean;
  AFixedKind: TcxGridBandFixedKind;
  I: Integer;
  AViewInfo: TcxGridColumnHeaderViewInfo;
  ABand: TcxGridBand;
  ANonFixedItems: TList;
  AHasRightFixedFooters: Boolean;

  function GetFixedPartBounds(AFixedKind: TcxGridBandFixedKind): TRect;
  var
    ABandViewInfo: TcxGridBandViewInfo;
  begin
    Result := ViewInfo.Bounds;
    ABandViewInfo := BandsViewInfo.LastFixedItems[AFixedKind, 0];
    if ABandViewInfo <> nil then
      case AFixedKind of
        fkLeft:
          if ViewInfo.UseRightToLeftAlignment then
            Result.Left := ABandViewInfo.Bounds.Left
          else
            Result.Right := ABandViewInfo.Bounds.Right;
        fkRight:
          if ViewInfo.UseRightToLeftAlignment then
            Result.Right := ABandViewInfo.Bounds.Right
          else
            Result.Left := ABandViewInfo.Bounds.Left;
      end
    else
      Result := cxNullRect;
  end;

  procedure ExcludeFixedPart(AFixedKind: TcxGridBandFixedKind);
  begin
    Canvas.ExcludeClipRect(GetFixedPartBounds(AFixedKind));
    AFixedPartExcluded[AFixedKind] := True;
  end;

  procedure IncludeFixedPart(AFixedKind: TcxGridBandFixedKind);
  var
    ARegion: TcxRegion;
  begin
    if not AFixedPartExcluded[AFixedKind] then Exit;
    ARegion := TcxRegion.Create(GetFixedPartBounds(AFixedKind));
    Canvas.SetClipRegion(ARegion, roAdd);
    AFixedPartExcluded[AFixedKind] := False;
  end;

  procedure ChangeFixedKind(Value: TcxGridBandFixedKind);
  begin
    case AFixedKind of
      fkLeft:
        begin
          AClipRegion := Canvas.GetClipRegion;
          ExcludeFixedPart(fkLeft);
        end;
      fkRight:
        begin
          IncludeFixedPart(fkLeft);
          Canvas.SetClipRegion(AClipRegion, roIntersect);
          AClipRegion := Canvas.GetClipRegion;
          ExcludeFixedPart(fkLeft);
          ExcludeFixedPart(fkRight);
        end;
    end;
    if Value = fkRight then
      AHasRightFixedFooters := True;
    AFixedKind := Value;
  end;

  procedure RestoreClipRegion;
  begin
    if AClipRegion <> nil then
    begin
      IncludeFixedPart(fkLeft);
      IncludeFixedPart(fkRight);
      Canvas.SetClipRegion(AClipRegion, roIntersect);
      AClipRegion := nil;
    end;
  end;

begin
  AClipRegion := nil;
  for AFixedKind := Low(AFixedKind) to High(AFixedKind) do
    AFixedPartExcluded[AFixedKind] := False;

  ANonFixedItems := TList.Create;
  try
    AHasRightFixedFooters := False;
    AFixedKind := fkLeft;
    for I := 0 to ViewInfo.Count - 1 do
    begin
      AViewInfo := ViewInfo.InternalItems[I];
      if AViewInfo <> nil then
      begin
        ABand := TcxGridBandedColumn(AViewInfo.Column).Position.Band;
        if ABand.FixedKind = fkNone then
          ANonFixedItems.Add(AViewInfo)
        else
        begin
          if ABand.FixedKind <> AFixedKind then
            ChangeFixedKind(ABand.FixedKind);
          AViewInfo.Paint;
        end;
      end;
    end;

    if ANonFixedItems.Count > 0 then
    begin
      if not AHasRightFixedFooters then
        ChangeFixedKind(fkRight);
      ChangeFixedKind(fkNone);
    end;
    for I := 0 to ANonFixedItems.Count - 1 do
      TcxGridColumnHeaderViewInfo(ANonFixedItems.Items[I]).Paint;
  finally
    ANonFixedItems.Free;
  end;

  RestoreClipRegion;
end;

function TcxGridBandedFooterPainter.GetBandsViewInfo: TcxGridBandsViewInfo;
begin
  Result := TcxGridBandedHeaderViewInfo(ViewInfo.GridViewInfo.HeaderViewInfo).BandsViewInfo;
end;

{ TcxGridIndicatorBandHeaderItemPainter }

function TcxGridIndicatorBandHeaderItemPainter.GetViewInfo: TcxGridIndicatorBandHeaderItemViewInfo;
begin
  Result := TcxGridIndicatorBandHeaderItemViewInfo(inherited ViewInfo);
end;

function TcxGridIndicatorBandHeaderItemPainter.DrawBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin
  Result := ViewInfo.GridView.ViewInfo.HeaderViewInfo.BandsViewInfo.DrawBandHeaderBackgroundHandler(ACanvas, ABounds);
end;

{ TcxGridBandedDataRowCellsAreaItemPainter }

function TcxGridBandedDataRowCellsAreaItemPainter.GetViewInfo: TcxGridBandedDataRowCellsAreaItemViewInfo;
begin
  Result := TcxGridBandedDataRowCellsAreaItemViewInfo(inherited ViewInfo);
end;

procedure TcxGridBandedDataRowCellsAreaItemPainter.DrawFixedBandsSeparator;
var
  R: TRect;
begin
  with Canvas do
  begin
    R := ViewInfo.FixedBandsSeparatorBounds;
    Brush.Color := ViewInfo.GridViewInfo.FixedBandSeparatorColor;
    FillRect(R);
    ExcludeClipRect(R);
  end;
end;

procedure TcxGridBandedDataRowCellsAreaItemPainter.DrawContent;
begin
  DrawLines;
  inherited;
end;

procedure TcxGridBandedDataRowCellsAreaItemPainter.DrawLines;
var
  I: Integer;
  R: TRect;
begin
  Canvas.Brush.Color := ViewInfo.GridViewInfo.GridLineColor;
  for I := 0 to ViewInfo.LineCount - 1 do
  begin
    R := ViewInfo.LineBounds[I];
    Canvas.FillRect(R);
    Canvas.ExcludeClipRect(R);
  end;
end;

function TcxGridBandedDataRowCellsAreaItemPainter.ExcludeFromClipRect: Boolean;
begin
  Result := True;
end;

procedure TcxGridBandedDataRowCellsAreaItemPainter.Paint;
begin
  if ViewInfo.FixedBandsSeparatorLocation <> slNone then
    DrawFixedBandsSeparator;
  inherited;
end;

{ TcxGridBandedRowsPainter }

class procedure TcxGridBandedRowsPainter.DrawDataRowCells(ARowViewInfo: TcxCustomGridRowViewInfo);
var
  ADataRowViewInfo: TcxGridDataRowViewInfo;
  ACellsAreaViewInfo: TcxGridBandedDataRowCellsAreaViewInfo;
  ACanvas: TcxCanvas;
  AClipRegion: TcxRegion;

  procedure DrawBandCells(ABandViewInfo: TcxGridBandViewInfo);
  var
    I: Integer;
    ACellViewInfo: TcxGridDataCellViewInfo;
  begin
    for I := 0 to ABandViewInfo.ColumnViewInfoCount - 1 do
    begin
      ACellViewInfo := ADataRowViewInfo.InternalCellViewInfos[ABandViewInfo[I].Index];
      if ACellViewInfo <> nil then ACellViewInfo.Paint;
    end;
    if ACellsAreaViewInfo.Visible then
      ACellsAreaViewInfo[ABandViewInfo.BottomIndex].Paint;
  end;

  procedure DrawBands(AFixedKind: TcxGridBandFixedKind);
  var
    I: Integer;
    ABandViewInfo: TcxGridBandViewInfo;
  begin
    with TcxGridBandedTableViewInfo(ARowViewInfo.GridViewInfo).HeaderViewInfo.BandsViewInfo do
      for I := 0 to BottomItemCount - 1 do
      begin
        ABandViewInfo := BottomItems[I];
        if ABandViewInfo.FixedKind = AFixedKind then
          DrawBandCells(ABandViewInfo);
      end;
  end;

begin
  ADataRowViewInfo := TcxGridDataRowViewInfo(ARowViewInfo);
  ACellsAreaViewInfo := TcxGridBandedDataRowCellsAreaViewInfo(ADataRowViewInfo.CellsAreaViewInfo);
  ACanvas := ARowViewInfo.GridViewInfo.Canvas;
  if ACellsAreaViewInfo.Visible then
  begin
    AClipRegion := ACanvas.GetClipRegion;
    ACanvas.IntersectClipRect(ACellsAreaViewInfo.Bounds);  // for layout with the fixed bands
  end
  else
    AClipRegion := nil;
  try
    {if ACellsAreaViewInfo.Count = 0 then
      ACellsAreaViewInfo.Paint;}
    DrawBands(fkLeft);
    DrawBands(fkRight);
    DrawBands(fkNone);
    ACellsAreaViewInfo.Paint;
  finally
    if AClipRegion <> nil then
    begin
      ACanvas.SetClipRegion(AClipRegion, roSet);
      ACanvas.ExcludeClipRect(ACellsAreaViewInfo.Bounds);
    end;
  end;
end;

{ TcxGridBandedTablePainter }

function TcxGridBandedTablePainter.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

function TcxGridBandedTablePainter.CanOffset(AItemsOffset, DX, DY: Integer): Boolean;
begin
  with ViewInfo.HeaderViewInfo.BandsViewInfo do
    Result := inherited CanOffset(AItemsOffset, DX, DY) and
      ((AItemsOffset <> 0) or (BandBackgroundBitmap = nil) and (BandHeaderBackgroundBitmap = nil) and
       ((GridView.Bands.Layout = blNonFixed) or
        (GridView.GroupedColumnCount = 0) and not GridView.Preview.Active));
end;

{ TcxGridBandedColumnHeaderVertSizingEdgeViewInfo }

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.CalculateHeight: Integer;
begin
  Result := ScaleFactor.Apply(cxGridHeaderSizingEdgeSize);
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := vaBottom;
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridColumnHeaderVertSizingEdgeHitTest;
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := nil;
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.OccupiesSpace: Boolean;
begin
  Result := False;
end;

function TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.ResidesInContent: Boolean;
begin
  Result := False;
end;

procedure TcxGridBandedColumnHeaderVertSizingEdgeViewInfo.Calculate(const ABounds: TRect;
  var ATextAreaBounds: TRect);
begin
  inherited;
  OffsetRect(Bounds, 0, Height div 2);
end;

{ TcxGridBandedColumnHeaderViewInfo }

function TcxGridBandedColumnHeaderViewInfo.GetBandViewInfo: TcxGridBandViewInfo;
begin
  Result := Container.BandsViewInfo[Column.Position.VisibleBandIndex];
end;

function TcxGridBandedColumnHeaderViewInfo.GetColumn: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited Column);
end;

function TcxGridBandedColumnHeaderViewInfo.GetContainer: TcxGridBandedHeaderViewInfo;
begin
  Result := TcxGridBandedHeaderViewInfo(inherited Container);
end;

function TcxGridBandedColumnHeaderViewInfo.GetRowViewInfo: TcxGridBandRowViewInfo;
begin
  Result := BandViewInfo.RowsViewInfo[Column.Position.VisibleRowIndex];
end;

function TcxGridBandedColumnHeaderViewInfo.CanVertSize: Boolean;
begin
  Result := Column.CanVertSize and (Container.Kind = ckHeader);
end;

procedure TcxGridBandedColumnHeaderViewInfo.GetAreaViewInfoClasses(AProc: TcxGridClassEnumeratorProc);
begin
  inherited;
  if CanVertSize then AProc(TcxGridBandedColumnHeaderVertSizingEdgeViewInfo);
end;

function TcxGridBandedColumnHeaderViewInfo.GetMaxWidth: Integer;
var
  AColIndex, I: Integer;
begin
  if (BandViewInfo.Band.Width <> 0) or BandViewInfo.Band.HasParentWithAssignedWidth then
  begin
    if BandViewInfo.Band.Width = 0 then
      Result := BandViewInfo.MaxContentWidth
    else
      Result := RowViewInfo.Width;
    AColIndex := Column.Position.VisibleColIndex;
    if AColIndex = RowViewInfo.ColumnViewInfoCount - 1 then
      for I := 0 to AColIndex - 1 do
        Dec(Result, RowViewInfo[I].MinWidth)
    else
      for I := 0 to RowViewInfo.ColumnViewInfoCount - 1 do
      begin
        if I < AColIndex then
          Dec(Result, RowViewInfo[I].Width);
        if I > AColIndex then
          Dec(Result, RowViewInfo[I].MinWidth);
      end;
    if Result < MinWidth then Result := MinWidth;
  end
  else
    Result := cxMaxRectSize;
end;

{ TcxGridBandHeaderViewInfo }

constructor TcxGridBandHeaderViewInfo.Create(ABandViewInfo: TcxGridBandViewInfo);
begin
  inherited Create(ABandViewInfo.GridViewInfo);
  FBandViewInfo := ABandViewInfo;
end;

function TcxGridBandHeaderViewInfo.GetBand: TcxGridBand;
begin
  Result := FBandViewInfo.Band;
end;

function TcxGridBandHeaderViewInfo.GetVerticalBorderOverlapSize: Integer;
var
  ANumOverlap: Integer;
begin
  ANumOverlap := (BandViewInfo.BandsViewInfo.BandHeaderRowCount - Band.BandLevelIndex) - 1;
  if ANumOverlap > 0 then
    Result := ANumOverlap * GridViewInfo.BorderOverlapSize
  else
    Result := 0;
end;

function TcxGridBandHeaderViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandHeaderViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := FBandViewInfo.GridViewInfo;
end;

function TcxGridBandHeaderViewInfo.GetRowCount: Integer;
begin
  if FBandViewInfo.IsBottom then
    Result := FBandViewInfo.BandsViewInfo.BandHeaderRowCount - Band.BandLevelIndex
  else
    Result := 1;
end;

function TcxGridBandHeaderViewInfo.CalculateHeight: Integer;
begin
  Result := FBandViewInfo.BandsViewInfo.CalculateBandHeaderHeight(Self);
end;

function TcxGridBandHeaderViewInfo.CalculateWidth: Integer;
begin
  with Bounds do
    Result := Right - Left;
end;

function TcxGridBandHeaderViewInfo.CanShowHint: Boolean;
begin
  Result := GridView.OptionsBehavior.BandHeaderHints;
end;

function TcxGridBandHeaderViewInfo.CustomDraw(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDraw(ACanvas);
  GridViewInfo.GridView.DoCustomDrawBandHeader(ACanvas, Self, Result);
end;

function TcxGridBandHeaderViewInfo.GetActualState: TcxGridCellState;
begin
  if IsPressed then
    Result := gcsPressed
  else
    Result := inherited GetActualState;
end;

function TcxGridBandHeaderViewInfo.GetAlignmentHorz: TAlignment;
begin
  Result := Band.HeaderAlignmentHorz;
end;

function TcxGridBandHeaderViewInfo.GetAlignmentVert: TcxAlignmentVert;
begin
  Result := Band.HeaderAlignmentVert;
end;

function TcxGridBandHeaderViewInfo.GetAreaBounds: TRect;
begin
  Result := BandViewInfo.BandsViewInfo.GetItemAreaBounds(Band);
end;

function TcxGridBandHeaderViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := BandViewInfo.BandsViewInfo.BandHeaderBackgroundBitmap;
end;

function TcxGridBandHeaderViewInfo.GetBorders: TcxBorders;
begin
  Result := cxBordersAll;
end;

function TcxGridBandHeaderViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := LookAndFeelPainter.HeaderBorderSize;
end;

function TcxGridBandHeaderViewInfo.GetCanvas: TcxCanvas;
begin
  Result := FBandViewInfo.Canvas;
end;

class function TcxGridBandHeaderViewInfo.GetCellHeight(ATextHeight: Integer;
  ALookAndFeelPainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := ATextHeight + 2 * ALookAndFeelPainter.HeaderBorderSize +
    cxMarginsHeight(ALookAndFeelPainter.HeaderContentOffsets(AScaleFactor));
end;

function TcxGridBandHeaderViewInfo.GetHeight: Integer;
var
  I: Integer;
begin
  if FBandViewInfo.IsBottom then
  begin
    Result := 0;
    for I := Band.BandLevelIndex to FBandViewInfo.BandsViewInfo.BandHeaderRowCount - 1 do
      Inc(Result, BandViewInfo.BandsViewInfo.BandHeaderRowHeights[I]);
    if Result > 0 then
      Dec(Result, GetVerticalBorderOverlapSize);
  end
  else
    Result := FBandViewInfo.BandsViewInfo.BandHeaderRowHeights[Band.BandLevelIndex];
end;

function TcxGridBandHeaderViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridBandHeaderHitTest;
end;

function TcxGridBandHeaderViewInfo.GetHotTrack: Boolean;
begin
  Result := LookAndFeelPainter.IsHeaderHotTrack;
end;

function TcxGridBandHeaderViewInfo.GetIsDesignSelected: Boolean;
begin
  Result := GridView.IsDesigning and
    GridView.Controller.DesignController.IsObjectSelected(Band);
end;

function TcxGridBandHeaderViewInfo.GetIsPressed: Boolean;
begin
  Result := (State = gcsPressed) or (GridViewInfo.Controller.PressedBand = Band);
end;

function TcxGridBandHeaderViewInfo.GetMultiLine: Boolean;
begin
  Result := inherited GetMultiLine or
    FBandViewInfo.BandsViewInfo.IsBandHeaderHeightAssigned;
end;

function TcxGridBandHeaderViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridBandHeaderPainter;
end;

function TcxGridBandHeaderViewInfo.GetShowEndEllipsis: Boolean;
begin
  Result := GridView.OptionsView.BandHeaderEndEllipsis;
end;

function TcxGridBandHeaderViewInfo.GetSizingEdgeBounds: TRect;
begin
  Result := Bounds;
  Result.Left := Result.Right - ScaleFactor.Apply(cxGridHeaderSizingEdgeSize) div 2;
  Result.Right := Result.Left + ScaleFactor.Apply(cxGridHeaderSizingEdgeSize);
  if UseRightToLeftAlignment then
    Result := TdxRightToLeftLayoutConverter.ConvertRect(Result, Bounds);
end;

function TcxGridBandHeaderViewInfo.GetText: string;
begin
  Result := Band.Caption;
end;

function TcxGridBandHeaderViewInfo.GetTextAreaBounds: TRect;
begin
  Result := cxRectContent(ContentBounds, LookAndFeelPainter.HeaderContentOffsets(ScaleFactor));
end;

function TcxGridBandHeaderViewInfo.GetVisible: Boolean;
begin
  Result := FBandViewInfo.BandsViewInfo.ShowBandHeaders;
end;

procedure TcxGridBandHeaderViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Band.Styles.GetHeaderParams(AParams);
end;

function TcxGridBandHeaderViewInfo.HasCustomDraw: Boolean;
begin
  Result := GridView.HasCustomDrawBandHeader;
end;

function TcxGridBandHeaderViewInfo.HasDesignPopupMenu: Boolean;
begin
  Result := True;
end;

procedure TcxGridBandHeaderViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  FBandViewInfo.InitHitTest(AHitTest);
  inherited;
end;

procedure TcxGridBandHeaderViewInfo.PopulateDesignPopupMenu(AMenu: TPopupMenu);
begin
  GridView.Controller.PopulateColumnHeaderDesignPopupMenu(AMenu);
end;

function TcxGridBandHeaderViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  if FBandViewInfo.CanSize and PtInRect(SizingEdgeBounds, P) then
  begin
    Result := TcxGridBandHeaderSizingEdgeHitTest.Instance(P);
    InitHitTest(Result);
  end
  else
    Result := inherited GetHitTest(P);
end;

function TcxGridBandHeaderViewInfo.MouseDown(AHitTest: TcxCustomGridHitTest;
  AButton: TMouseButton; AShift: TShiftState): Boolean;
var
  ABand: TcxGridBand;
begin
  Result := inherited MouseDown(AHitTest, AButton, AShift);
  if AButton = mbLeft then
    case AHitTest.HitTestCode of
      htBandHeader:
        if not (ssDouble in AShift) then
        begin
          if GridView.IsDesigning then
            GridView.Controller.DesignController.SelectObject(Band, not (ssShift in AShift))
          else
            GridView.Controller.PressedBand := Band;
          Result := True;
        end;
      htBandHeaderSizingEdge:
        if ssDouble in AShift then
        begin
          ABand := Band;
          ABand.ApplyBestFit(True);
          ABand.GridView.DoBandSizeChanged(ABand);
          Result := True;
        end;
    end
    else
      if (AButton = mbRight) and GridView.IsDesigning then
        if not GridView.Controller.DesignController.IsObjectSelected(Band) then
          GridView.Controller.DesignController.SelectObject(Band, not (ssShift in AShift));
end;

{ TcxGridBandRowViewInfo }

constructor TcxGridBandRowViewInfo.Create(ARowsViewInfo: TcxGridBandRowsViewInfo;
  AIndex: Integer);
begin
  inherited Create;
  FRowsViewInfo := ARowsViewInfo;
  FIndex := AIndex;
  FColumnViewInfos := TList.Create;
  FHeight := -1;
  AddColumnViewInfos;
end;

destructor TcxGridBandRowViewInfo.Destroy;
begin
  FColumnViewInfos.Free;
  inherited;
end;

function TcxGridBandRowViewInfo.GetBandRow: TcxGridBandRow;
begin
  Result := FRowsViewInfo.BandRows.VisibleItems[FIndex];
end;

function TcxGridBandRowViewInfo.GetBandViewInfo: TcxGridBandViewInfo;
begin
  Result := FRowsViewInfo.BandViewInfo;
end;

function TcxGridBandRowViewInfo.GetColumnViewInfo(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
begin
  Result := TcxGridBandedColumnHeaderViewInfo(FColumnViewInfos[Index]);
end;

function TcxGridBandRowViewInfo.GetColumnViewInfoCount: Integer;
begin
  Result := FColumnViewInfos.Count;
end;

function TcxGridBandRowViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := FRowsViewInfo.BandViewInfo.GridView;
end;

function TcxGridBandRowViewInfo.GetHeight: Integer;
begin
  if FHeight = -1 then
    FHeight := CalculateHeight;
  Result := FHeight;
end;

function TcxGridBandRowViewInfo.GetLineHeight: Integer;
begin
  Result := BandViewInfo.ContainerViewInfo.ItemHeight;
end;

function TcxGridBandRowViewInfo.GetMinWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ColumnViewInfoCount - 1 do
    Inc(Result, ColumnViewInfos[I].MinWidth);
end;

function TcxGridBandRowViewInfo.GetWidth: Integer;
begin
  with Bounds do
    Result := Right - Left;
end;

procedure TcxGridBandRowViewInfo.AddColumnViewInfos;
var
  AHeaderViewInfo: TcxGridBandedHeaderViewInfo;
  I: Integer;
  AColumnViewInfo: TcxGridBandedColumnHeaderViewInfo;
begin
  AHeaderViewInfo := BandViewInfo.GridViewInfo.HeaderViewInfo;
  for I := 0 to BandRow.VisibleCount - 1 do
  begin
    AColumnViewInfo := AHeaderViewInfo[BandRow.VisibleItems[I].VisibleIndex];
    FColumnViewInfos.Add(AColumnViewInfo);
  end;
end;

procedure TcxGridBandRowViewInfo.Calculate(const ABounds: TRect);
var
  ALeftBound, ATopBound, ALineHeight, I, AWidth: Integer;
  AColumnViewInfo: TcxGridBandedColumnHeaderViewInfo;
begin
  Bounds := ABounds;
  CalculateColumnWidths;
  ALeftBound := Bounds.Left;
  ATopBound := Bounds.Top;
  ALineHeight := LineHeight;
  for I := 0 to ColumnViewInfoCount - 1 do
  begin
    AColumnViewInfo := ColumnViewInfos[I];
    AWidth := AColumnViewInfo.CalculateWidth;
    AColumnViewInfo.Calculate(ALeftBound, ATopBound, AWidth,
      AColumnViewInfo.Column.Position.LineCount * ALineHeight);
    Inc(ALeftBound, AWidth);
  end;
end;

procedure TcxGridBandRowViewInfo.CalculateColumnWidths;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(ColumnViewInfoCount);
  try
    for I := 0 to ColumnViewInfoCount - 1 do
      ColumnViewInfos[I].InitAutoWidthItem(AAutoWidthObject.AddItem);
    AAutoWidthObject.AvailableWidth := FRowsViewInfo.Width;
    AAutoWidthObject.Calculate;
    for I := 0 to ColumnViewInfoCount - 1 do
      ColumnViewInfos[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

function TcxGridBandRowViewInfo.CalculateHeight: Integer;
begin
  Result := BandRow.LineCount * LineHeight;
end;

function TcxGridBandRowViewInfo.CalculateLineHeight: Integer;
var
  I, AColumnHeaderHeight: Integer;
begin
  Result := 0;
  for I := 0 to ColumnViewInfoCount - 1 do
  begin
    AColumnHeaderHeight := ColumnViewInfos[I].CalculateHeight;
    if AColumnHeaderHeight > Result then
      Result := AColumnHeaderHeight;
  end;
end;

function TcxGridBandRowViewInfo.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to ColumnViewInfoCount - 1 do
    Inc(Result, ColumnViewInfos[I].CalculateWidth);
end;

procedure TcxGridBandRowViewInfo.AssignColumnWidths;
var
  I: Integer;
begin
  GridView.BeginUpdate;
  try
    for I := 0 to ColumnViewInfoCount - 1 do
      with ColumnViewInfos[I] do
        Column.Width := RealWidth;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridBandRowViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
end;

procedure TcxGridBandRowViewInfo.Offset(DX, DY: Integer);
begin
  OffsetRect(Bounds, DX, DY);
end;

procedure TcxGridBandRowViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridBandRowsViewInfo }

constructor TcxGridBandRowsViewInfo.Create(ABandViewInfo: TcxGridBandViewInfo);
begin
  inherited Create;
  FBandViewInfo := ABandViewInfo;
  FWidth := -1;
  CreateItems;
end;

destructor TcxGridBandRowsViewInfo.Destroy;
begin
  DestroyItems;
  inherited;
end;

function TcxGridBandRowsViewInfo.GetBandRows: TcxGridBandRows;
begin
  Result := FBandViewInfo.Band.Rows;
end;

function TcxGridBandRowsViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridBandRowsViewInfo.GetItem(Index: Integer): TcxGridBandRowViewInfo;
begin
  Result := TcxGridBandRowViewInfo(FItems[Index]);
end;

function TcxGridBandRowsViewInfo.GetMinWidth: Integer;
var
  I, AMinWidth: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    AMinWidth := Items[I].MinWidth;
    if AMinWidth > Result then Result := AMinWidth;
  end;
end;

function TcxGridBandRowsViewInfo.GetWidth: Integer;
begin
  {with Bounds do
    Result := Right - Left;}
  Result := FBandViewInfo.ContentWidth;
end;

procedure TcxGridBandRowsViewInfo.CreateItems;
var
  I: Integer;
begin
  FItems := TList.Create;
  for I := 0 to BandRows.VisibleCount - 1 do
    FItems.Add(GetBandRowViewInfoClass.Create(Self, I));
end;

procedure TcxGridBandRowsViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I].Free;
  FItems.Free;
end;

procedure TcxGridBandRowsViewInfo.AssignColumnWidths;
var
  I: Integer;
begin
  BandViewInfo.GridView.BeginUpdate;
  try
    for I := 0 to Count - 1 do
      Items[I].AssignColumnWidths;
  finally
    BandViewInfo.GridView.EndUpdate;
  end;
end;

procedure TcxGridBandRowsViewInfo.Calculate(const ABounds: TRect);
var
  ARowBounds: TRect;
  I: Integer;
begin
  Bounds := ABounds;
  ARowBounds := Bounds;
  for I := 0 to Count - 1 do
  begin
    ARowBounds.Bottom := ARowBounds.Top + Items[I].Height;
    Items[I].Calculate(ARowBounds);
    ARowBounds.Top := ARowBounds.Bottom;
  end;
end;

procedure TcxGridBandRowsViewInfo.CalculateColumnWidths;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].CalculateColumnWidths;
end;

function TcxGridBandRowsViewInfo.CalculateWidth: Integer;
var
  I, ARowWidth: Integer;
begin
  if Count = 0 then
    Result := cxGridDefaultEmptyBandWidth
  else
  begin
    Result := 0;
    for I := 0 to Count - 1 do
    begin
      ARowWidth := Items[I].CalculateWidth;
      if ARowWidth > Result then Result := ARowWidth;
    end;
  end;
end;

procedure TcxGridBandRowsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  Bounds := TdxRightToLeftLayoutConverter.ConvertRect(Bounds, ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridBandRowsViewInfo.GetBandRowViewInfoClass: TcxGridBandRowViewInfoClass;
begin
  Result := TcxGridBandRowViewInfo;
end;

{function TcxGridBandRowsViewInfo.GetColumnsHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I, J: Integer;
begin
  for I := 0 to Count - 1 do
    for J := 0 to Items[I].ColumnViewInfoCount - 1 do
    begin
      Result := Items[I][J].GetHitTest(P);
      if Result <> nil then Exit;
    end;
  Result := nil;
end;}

function TcxGridBandRowsViewInfo.IndexAtPos(const P: TPoint): Integer;
var
  R: TRect;
begin
  R := Bounds;
  if FBandViewInfo.GridView.Controller.IsItemMoving then
    Inc(R.Bottom, ColumnHeaderInsertZoneSize);
  if PtInRect(R, P) then
  begin
    for Result := 0 to Count - 1 do
      if PtInRect(Items[Result].Bounds, P) then Exit;
    Result := Count;
  end
  else
    Result := -1;
end;

procedure TcxGridBandRowsViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  OffsetRect(Bounds, DX, 0);
  for I := 0 to Count - 1 do
    Items[I].Offset(DX, DY);
end;

procedure TcxGridBandRowsViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridBandViewInfo }

constructor TcxGridBandViewInfo.Create(ABandsViewInfo: TcxGridBandsViewInfo; AIndex: Integer);
begin
  inherited Create;
  FBandsViewInfo := ABandsViewInfo;
  FColumnViewInfos := TList.Create;
  FIndex := AIndex;
  FBottomIndex := Band.VisibleBottomIndex;
  FHeaderViewInfo := GetHeaderViewInfoClass.Create(Self);
  if IsBottom then
  begin
    AddColumnViewInfos;
    FRowsViewInfo := GetRowsViewInfoClass.Create(Self);
  end;
  FWidth := -1;
end;

destructor TcxGridBandViewInfo.Destroy;
begin
  FHeaderViewInfo.Free;
  FRowsViewInfo.Free;
  FColumnViewInfos.Free;
  inherited;
end;

function TcxGridBandViewInfo.GetBand: TcxGridBand;
begin
  Result := GridView.Bands.VisibleItems[FIndex];
end;

function TcxGridBandViewInfo.GetBorderOverlapSize: Integer;
begin
  Result := GridViewInfo.BorderOverlapSize;
end;

function TcxGridBandViewInfo.GetBoundsForBandInsert: TRect;
begin
  Result := FHeaderViewInfo.Bounds;
  if Band.IsMostRight then
    if UseRightToLeftAlignment then
      Result.Left := GridViewInfo.ClientBounds.Left
    else
      Result.Right := GridViewInfo.ClientBounds.Right;
  if IsRoot then
    Dec(Result.Top, BandHeaderMovingZoneSize);
  if IsBottom then
    Inc(Result.Bottom, BandHeaderMovingZoneSize);
end;

function TcxGridBandViewInfo.GetChildBandsHorizontalBorderOverlapSize: Integer;
var
  ANumOverlaps: Integer;
begin
  ANumOverlaps := ChildBandViewInfoCount - 1;
  if ANumOverlaps > 0 then
    Result := ANumOverlaps * BorderOverlapSize
  else
    Result := 0;
end;

function TcxGridBandViewInfo.GetChildBandViewInfo(Index: Integer): TcxGridBandViewInfo;
begin
  Result := FBandsViewInfo[Band.VisibleChildBands[Index].VisibleIndex];
end;

function TcxGridBandViewInfo.GetChildBandViewInfoCount: Integer;
begin
  Result := Band.VisibleChildBandCount;
end;

function TcxGridBandViewInfo.GetColumnViewInfo(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
begin
  Result := TcxGridBandedColumnHeaderViewInfo(FColumnViewInfos[Index]);
end;

function TcxGridBandViewInfo.GetColumnViewInfoCount: Integer;
begin
  Result := FColumnViewInfos.Count;
end;

function TcxGridBandViewInfo.GetContainerViewInfo: TcxGridBandedHeaderViewInfo;
begin
  Result := FBandsViewInfo.ContainerViewInfo;
end;

function TcxGridBandViewInfo.GetFixedKind: TcxGridBandFixedKind;
begin
  Result := Band.FixedKind;
end;

function TcxGridBandViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := FBandsViewInfo.GridView;
end;

function TcxGridBandViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := ContainerViewInfo.GridViewInfo;
end;

function TcxGridBandViewInfo.GetIsBottom: Boolean;
begin
  Result := Band.IsVisibleBottom;
end;

function TcxGridBandViewInfo.GetIsFixed: Boolean;
begin
  Result := Band.Fixed;
end;

function TcxGridBandViewInfo.GetIsRight: Boolean;
begin
  Result := FBottomIndex = FBandsViewInfo.BottomItemCount - 1;
end;

function TcxGridBandViewInfo.GetIsRoot: Boolean;
begin
  Result := Band.IsRoot;
end;

function TcxGridBandViewInfo.GetParentBandViewInfo: TcxGridBandViewInfo;
begin
  Result := BandsViewInfo[Band.ParentBand.VisibleIndex];
end;

function TcxGridBandViewInfo.GetRowCount: Integer;
begin
  Result := FRowsViewInfo.Count;
end;

function TcxGridBandViewInfo.GetSameLevelAutoWidth: Integer;
begin
  if IsRoot then
    Result := GridViewInfo.ClientWidth
  else
    if ParentBandViewInfo.Band.Width = 0 then
      Result := ParentBandViewInfo.MaxWidth
    else
      Result := ParentBandViewInfo.Width;
end;

function TcxGridBandViewInfo.GetSameLevelItem(Index: Integer): TcxGridBandViewInfo;
begin
  if IsRoot then
    Result := BandsViewInfo.RootItems[Index]
  else
    Result := ParentBandViewInfo.ChildBandViewInfos[Index];
end;

function TcxGridBandViewInfo.GetSameLevelItemCount: Integer;
begin
  if IsRoot then
    Result := BandsViewInfo.RootItemCount
  else
    Result := ParentBandViewInfo.ChildBandViewInfoCount;
end;

function CompareColumnViewInfos(
  Item1, Item2: Pointer): Integer;
begin
  Result := CompareVisibleColumnPositions(
    TcxGridBandedColumnHeaderViewInfo(Item1).Column.Position,
    TcxGridBandedColumnHeaderViewInfo(Item2).Column.Position);
end;

procedure TcxGridBandViewInfo.AddColumnViewInfos;
var
  I: Integer;
  AColumnHeaderViewInfo: TcxGridBandedColumnHeaderViewInfo;
begin
  for I := 0 to ContainerViewInfo.Count - 1 do
  begin
    AColumnHeaderViewInfo := ContainerViewInfo[I];
    if AColumnHeaderViewInfo.Column.Position.Band = Band then
      FColumnViewInfos.Add(AColumnHeaderViewInfo);
  end;
  FColumnViewInfos.Sort(CompareColumnViewInfos);
end;

procedure TcxGridBandViewInfo.AssignChildBandWidths;
var
  I: Integer;
begin
  GridView.BeginUpdate;
  try
    for I := 0 to ChildBandViewInfoCount - 1 do
      with ChildBandViewInfos[I] do
        Band.Width := ContentWidth;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridBandViewInfo.AssignColumnWidths;
var
  I: Integer;
begin
  if IsBottom then
    FRowsViewInfo.AssignColumnWidths
  else
  begin
    GridView.BeginUpdate;
    try
      for I := 0 to ChildBandViewInfoCount - 1 do
        ChildBandViewInfos[I].AssignColumnWidths;
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridBandViewInfo.CalculateChildBandWidths;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(ChildBandViewInfoCount);
  try
    for I := 0 to ChildBandViewInfoCount - 1 do
      ChildBandViewInfos[I].InitAutoWidthItem(AAutoWidthObject.AddItem);
    AAutoWidthObject.AvailableWidth := Width + GetChildBandsHorizontalBorderOverlapSize;
    AAutoWidthObject.Calculate;
    for I := 0 to ChildBandViewInfoCount - 1 do
      ChildBandViewInfos[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

procedure TcxGridBandViewInfo.CalculateChildBands(R: TRect);
var
  I: Integer;
  AChildBandViewInfo: TcxGridBandViewInfo;
begin
  CalculateChildBandWidths;
  for I := 0 to ChildBandViewInfoCount - 1 do
  begin
    AChildBandViewInfo := ChildBandViewInfos[I];
    R.Right := R.Left + AChildBandViewInfo.Width;
    AChildBandViewInfo.Calculate(R);
    R.Left := R.Right;
    Dec(R.Left, BorderOverlapSize);
  end;
end;

function TcxGridBandViewInfo.CalculateChildBandsBounds: TRect;
begin
  Result := Bounds;
  Result.Top := CalculateHeaderBounds.Bottom;
  if BandsViewInfo.ShowBandHeaders then
    Dec(Result.Top, BorderOverlapSize);
end;

function TcxGridBandViewInfo.CalculateColumnsBounds: TRect;
begin
  Result := ContentBounds;
  Result.Top := CalculateHeaderBounds.Bottom;
  if BandsViewInfo.ShowBandHeaders then
    Dec(Result.Top, BorderOverlapSize);
end;

procedure TcxGridBandViewInfo.CalculateColumnWidths;
begin
  FRowsViewInfo.CalculateColumnWidths;
end;

function TcxGridBandViewInfo.CalculateHeaderBounds: TRect;
begin
  Result := ContentBounds;
  Result.Bottom := Result.Top + HeaderViewInfo.Height;
end;

function TcxGridBandViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridBandViewInfo.CalculateWidth: Integer;
var
  AMinWidth: Integer;

  function CalculateChildBandsWidth: Integer;
  var
    I: Integer;
  begin
    Result := 0;
    for I := 0 to ChildBandViewInfoCount - 1 do
      Inc(Result, ChildBandViewInfos[I].CalculateWidth);
  end;

begin
  if FWidth = -1 then
  begin
    FWidth := Band.Width;
    CalculateParams;
    if FWidth = 0 then
      if IsBottom then
        FWidth := BorderSize[bLeft] + FRowsViewInfo.CalculateWidth + BorderSize[bRight]
      else
        FWidth := CalculateChildBandsWidth
    else
      Inc(FWidth, BorderSize[bLeft] + BorderSize[bRight]);
    AMinWidth := MinWidth;
    if FWidth < AMinWidth then FWidth := AMinWidth;
  end;
  Result := FWidth;
end;

function TcxGridBandViewInfo.CanSize: Boolean;
begin
  Result := Band.CanSize;
end;

procedure TcxGridBandViewInfo.CheckWidth(var Value: Integer);
begin
  if Value < MinContentWidth then Value := MinContentWidth;
  if Value > MaxContentWidth then Value := MaxContentWidth;
end;

function TcxGridBandViewInfo.CustomDrawBackground(ACanvas: TcxCanvas): Boolean;
begin
  Result := inherited CustomDrawBackground(ACanvas);
  GridView.DoCustomDrawPartBackground(ACanvas, Self, Result);
end;

function TcxGridBandViewInfo.GetAreAllColumnsFixed: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to ColumnViewInfoCount - 1 do
  begin
    Result := ColumnViewInfos[I].IsFixed;
    if not Result then Exit;
  end;
end;

function TcxGridBandViewInfo.GetBackgroundBitmap: TBitmap;
begin
  Result := BandsViewInfo.BandBackgroundBitmap;
end;

function TcxGridBandViewInfo.GetBorderColor(AIndex: TcxBorder): TColor;
begin
  Result := GridViewInfo.FixedBandSeparatorColor;
end;

function TcxGridBandViewInfo.GetBorders: TcxBorders;
begin
  Result := [];
  if (FixedKind <> fkNone) and
    (BandsViewInfo.LastFixedItems[FixedKind, Band.BandLevelIndex] = Self) then
    case FixedKind of
      fkLeft:
        Include(Result, bRight);
      fkRight:
        Include(Result, bLeft);
    end;
end;

function TcxGridBandViewInfo.GetBorderWidth(AIndex: TcxBorder): Integer;
begin
  Result := GridViewInfo.FixedBandSeparatorWidth;
end;

function TcxGridBandViewInfo.GetCanvas: TcxCanvas;
begin
  Result := ContainerViewInfo.Canvas;
end;

function TcxGridBandViewInfo.GetContentWidth: Integer;
begin
  //CalculateParams;
  Result := Width - (BorderSize[bLeft] + BorderSize[bRight]);
end;

class function TcxGridBandViewInfo.GetHeaderViewInfoClass: TcxGridBandHeaderViewInfoClass;
begin
  Result := TcxGridBandHeaderViewInfo;
end;

function TcxGridBandViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridBandHitTest;
end;

function TcxGridBandViewInfo.GetMaxContentWidth: Integer;
begin
  CalculateParams;
  Result := MaxWidth - (BorderSize[bLeft] + BorderSize[bRight]);
end;

function TcxGridBandViewInfo.GetMaxWidth: Integer;
var
  AIndex, I: Integer;
begin
  if GridView.OptionsView.ColumnAutoWidth or Band.HasParentWithAssignedWidth then
  begin
    Result := SameLevelAutoWidth;
    AIndex := Band.Position.VisibleColIndex;
    if Band.IsLastAsChild then
      for I := 0 to AIndex - 1 do
        Dec(Result, SameLevelItems[I].MinWidth)
    else
      for I := 0 to SameLevelItemCount - 1 do
      begin
        if I < AIndex then
          Dec(Result, SameLevelItems[I].Width);
        if I > AIndex then
          Dec(Result, SameLevelItems[I].MinWidth);
      end;
    if Result < MinWidth then Result := MinWidth;
  end
  else
    Result := cxMaxRectSize;
end;

function TcxGridBandViewInfo.GetMinContentWidth: Integer;
var
  I: Integer;
begin
  if IsBottom then
  begin
    CalculateParams;
    Result := FRowsViewInfo.MinWidth;
    if Band.IsFirst then
      Result := Max(Result, GridViewInfo.FirstItemAdditionalWidth);
  end
  else
  begin
    Result := 0;
    for I := 0 to ChildBandViewInfoCount - 1 do
      Inc(Result, ChildBandViewInfos[I].MinContentWidth);
  end;
  if Result < Band.MinWidth then Result := Band.MinWidth;
end;

function TcxGridBandViewInfo.GetMinWidth: Integer;
begin
  CalculateParams;
  Result := MinContentWidth + (BorderSize[bLeft] + BorderSize[bRight]);
end;

function TcxGridBandViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridBandPainter;
end;

function TcxGridBandViewInfo.GetRowsViewInfoClass: TcxGridBandRowsViewInfoClass;
begin
  Result := TcxGridBandRowsViewInfo;
end;

procedure TcxGridBandViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  Band.Styles.GetViewParams(bsBackground, Band, nil, AParams);
end;

function TcxGridBandViewInfo.GetWidth: Integer;
begin
  if FWidth = -1 then
    Result := CalculateWidth
  else
    Result := FWidth;
end;

procedure TcxGridBandViewInfo.InitHitTest(AHitTest: TcxCustomGridHitTest);
begin
  inherited;
  with AHitTest as TcxGridBandHitTest do
  begin
    GridView := Self.GridView;
    Band := Self.Band;
    BandContainerKind := bcHeader;
    VisibleRowIndex := -1;
  end;
end;

procedure TcxGridBandViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  OffsetRect(FExcludeBounds, DX, DY);
  FHeaderViewInfo.DoOffset(DX, DY);
  if IsBottom then
    FRowsViewInfo.Offset(DX, DY);
end;

procedure TcxGridBandViewInfo.SetWidth(Value: Integer);
begin
  inherited;
  FWidth := Value;
end;

procedure TcxGridBandViewInfo.Calculate(const ABounds: TRect);
begin
  inherited;
  FHeaderViewInfo.Calculate(CalculateHeaderBounds);
  if IsBottom then
    FRowsViewInfo.Calculate(CalculateColumnsBounds)
  else
    CalculateChildBands(CalculateChildBandsBounds);
  FExcludeBounds := Bounds;
  if not IsRight then
   Dec(FExcludeBounds.Right, BorderOverlapSize);
end;

procedure TcxGridBandViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FExcludeBounds := TdxRightToLeftLayoutConverter.ConvertRect(FExcludeBounds, ABounds);
  FHeaderViewInfo.RightToLeftConversion(ABounds);
  if IsBottom then
    FRowsViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridBandViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I, ARowIndex: Integer;
begin
  Result := FHeaderViewInfo.GetHitTest(P);
  if Result = nil then
    if IsBottom then
    begin
      if BandsViewInfo.ShowColumnHeaders then
        for I := 0 to ColumnViewInfoCount - 1 do
        begin
          Result := ColumnViewInfos[I].GetHitTest(P);
          if Result <> nil then Exit;
        end;
      Result := inherited GetHitTest(P);
      ARowIndex := FRowsViewInfo.IndexAtPos(P);
      if ARowIndex <> -1 then
      begin
        if Result = nil then
        begin
          Result := GetHitTestClass.Instance(P);
          InitHitTest(Result);
        end;
        TcxGridBandHitTest(Result).VisibleRowIndex := ARowIndex;
      end;
    end
    else
      for I := 0 to ChildBandViewInfoCount - 1 do
      begin
        Result := ChildBandViewInfos[I].GetHitTest(P);
        if Result <> nil then Break;
      end;
end;

procedure TcxGridBandViewInfo.InitAutoWidthItem(AAutoWidthItem: TcxAutoWidthItem);
begin
  AAutoWidthItem.MinWidth := MinWidth;
  AAutoWidthItem.Width := CalculateWidth;
  AAutoWidthItem.Fixed := IsFixed;
end;

function TcxGridBandViewInfo.InsertPositionAtPos(const P: TPoint;
  out ABand: TcxGridBand; out AInsertPosition: TcxPosition): Boolean;
var
  I: Integer;
begin
  Result := PtInRect(BoundsForBandInsert, P);
  if Result then
  begin
    ABand := Band;
    AInsertPosition := GetPointPosition(FHeaderViewInfo.Bounds, P,
      True, GridView.OptionsCustomize.NestedBands);
  end
  else
    if GridView.OptionsCustomize.NestedBands and not IsBottom then
      for I := 0 to ChildBandViewInfoCount - 1 do
      begin
        Result := ChildBandViewInfos[I].InsertPositionAtPos(P, ABand, AInsertPosition);
        if Result then Break;
      end;
end;

{ TcxGridBandsViewInfo }

constructor TcxGridBandsViewInfo.Create(AContainerViewInfo: TcxGridBandedHeaderViewInfo);
begin
  inherited Create;
  FContainerViewInfo := AContainerViewInfo;
  FBandHeadersAreaHeight := -1;
  FLineCount := -1;
  FRowCount := -1;
  FBandHeaderRowHeights := TList.Create;
  CreateItems;
end;

destructor TcxGridBandsViewInfo.Destroy;
begin
  DestroyItems;
  FBandHeaderRowHeights.Free;
  inherited;
end;

function TcxGridBandsViewInfo.GetBandHeaderRowCount: Integer;
begin
  Result := FBandHeaderRowHeights.Count;
end;

function TcxGridBandsViewInfo.GetBandHeaderRowHeight(Index: Integer): Integer;
var
  APrevCount, I: Integer;
begin
  if Index >= FBandHeaderRowHeights.Count then
  begin
    APrevCount := FBandHeaderRowHeights.Count;
    FBandHeaderRowHeights.Count := Index + 1;
    for I := APrevCount to Index do
      BandHeaderRowHeights[I] := 0;
  end;
  Result := Integer(FBandHeaderRowHeights[Index]);
end;

function TcxGridBandsViewInfo.GetBandHeadersAreaHeight: Integer;
begin
  if FBandHeadersAreaHeight = -1 then
    if ShowBandHeaders then
      FBandHeadersAreaHeight := CalculateBandHeadersAreaHeight
    else
      FBandHeadersAreaHeight := 0;
  Result := FBandHeadersAreaHeight;
end;

function TcxGridBandsViewInfo.GetBandHeadersVerticalBorderOverlapSize: Integer;
var
  ANumOverlap: Integer;
begin
  ANumOverlap := BandHeaderRowCount - 1;
  if ANumOverlap > 0 then
    Result := ANumOverlap * BorderOverlapSize
  else
    Result := 0;
end;

function TcxGridBandsViewInfo.GetBands: TcxGridBands;
begin
  Result := GridView.Bands;
end;

function TcxGridBandsViewInfo.GetBorderOverlapSize: Integer;
begin
  Result := GridViewInfo.BorderOverlapSize;
end;

function TcxGridBandsViewInfo.GetBottomItem(Index: Integer): TcxGridBandViewInfo;
begin
  Result := Items[GridView.Bands.VisibleBottomItems[Index].VisibleIndex];
end;

function TcxGridBandsViewInfo.GetBottomItemCount: Integer;
begin
  Result := GridView.Bands.VisibleBottomItemCount;
end;

function TcxGridBandsViewInfo.GetColumnHeadersAreaHeight: Integer;
begin
  if ShowColumnHeaders then
    Result := FContainerViewInfo.Specific.Height
  else
    Result := 0;
end;

function TcxGridBandsViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridBandsViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := FContainerViewInfo.GridView;
end;

function TcxGridBandsViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := FContainerViewInfo.GridViewInfo;
end;

function TcxGridBandsViewInfo.GetInternalItem(Index: Integer): TcxGridBandViewInfo;
begin
  Result := TcxGridBandViewInfo(FItems[Index]);
end;

function TcxGridBandsViewInfo.GetItem(Index: Integer): TcxGridBandViewInfo;
begin
  Result := InternalItems[Index];
  if Result = nil then
  begin
    Result := GetItemClass.Create(Self, Index);
    FItems[Index] := Result;
  end;
end;

function TcxGridBandsViewInfo.GetLastFixedItem(AFixedKind: TcxGridBandFixedKind;
  ALevelIndex: Integer): TcxGridBandViewInfo;

  function GetLastLeftFixedItem: TcxGridBandViewInfo;
  begin
    Result := RootItems[Bands.VisibleRootLeftFixedCount - 1];
    while Result.Band.BandLevelIndex <> ALevelIndex do
      if Result.IsBottom then
      begin
        Result := nil;
        Break;
      end
      else
        Result := Result.ChildBandViewInfos[Result.ChildBandViewInfoCount - 1];
  end;

  function GetFirstRightFixedItem: TcxGridBandViewInfo;
  begin
    Result := RootItems[RootItemCount - Bands.VisibleRootRightFixedCount];
    while Result.Band.BandLevelIndex <> ALevelIndex do
      if Result.IsBottom then
      begin
        Result := nil;
        Break;
      end
      else
        Result := Result.ChildBandViewInfos[0];
  end;

begin
  Result := nil;
  case AFixedKind of
    fkLeft:
      if Bands.VisibleLeftFixedCount <> 0 then
        Result := GetLastLeftFixedItem;
    fkRight:
      if Bands.VisibleRightFixedCount <> 0 then
        Result := GetFirstRightFixedItem;
  end;
end;

function TcxGridBandsViewInfo.GetLineCount: Integer;
begin
  if FLineCount = -1 then
    FLineCount := Bands.LineCount;
  Result := FLineCount;
end;

function TcxGridBandsViewInfo.GetRootItem(Index: Integer): TcxGridBandViewInfo;
begin
  Result := Items[GridView.Bands.VisibleRootItems[Index].VisibleIndex];
end;

function TcxGridBandsViewInfo.GetRootItemCount: Integer;
begin
  Result := GridView.Bands.VisibleRootItemCount;
end;

function TcxGridBandsViewInfo.GetRootItemsHorizontalBorderOverlapSize: Integer;
var
  ANumOverlap: Integer;
begin
  ANumOverlap := RootItemCount - 1;
  if GridViewInfo.HasFirstBorderOverlap then
    Inc(ANumOverlap);
  if ANumOverlap > 0 then
    Result := ANumOverlap * BorderOverlapSize
  else
    Result := 0;
end;

function TcxGridBandsViewInfo.GetRowCount: Integer;
begin
  if FRowCount = -1 then
    FRowCount := Bands.VisibleRowCount;
  Result := FRowCount;
end;

procedure TcxGridBandsViewInfo.SetBandHeaderRowHeight(Index: Integer; Value: Integer);
begin
    FBandHeaderRowHeights[Index] := Pointer(Value);
end;

procedure TcxGridBandsViewInfo.CreateItems;
begin
  FItems := TList.Create;
  FItems.Count := GridView.Bands.VisibleCount;
end;

procedure TcxGridBandsViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    InternalItems[I].Free;
  FItems.Free;
end;

function TcxGridBandsViewInfo.AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo;
  ATopBound: Integer): Boolean;
begin
  Result := AIndicatorViewInfo.AlwaysVisible;
  if not Result then Exit;
  if ShowBandHeaders then
  begin
    AIndicatorViewInfo.AddItem(ATopBound, BandHeadersAreaHeight, TcxGridIndicatorBandHeaderItemViewInfo);
    Inc(ATopBound, BandHeadersAreaHeight);
  end;
  if ShowColumnHeaders then
  begin
    if ShowBandHeaders then
      Dec(ATopBound, BorderOverlapSize);
    AIndicatorViewInfo.AddItem(ATopBound, ColumnHeadersAreaHeight, TcxGridIndicatorHeaderItemViewInfo);
  end;
end;

procedure TcxGridBandsViewInfo.AddAdornerTargetElements(AList: TStrings);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AList.AddObject('BandHeader' + IntToStr(Items[I].Index), Items[I].HeaderViewInfo);
end;

procedure TcxGridBandsViewInfo.Calculate;
var
  AScrollableAreaBounds, ABounds: TRect;

  procedure ProcessLeftFixedItems;
  var
    I: Integer;
    AItem: TcxGridBandViewInfo;
  begin
    ABounds := GetRootItemsAreaBounds;
    for I := 0 to RootItemCount - 1 do
    begin
      AItem := RootItems[I];
      if AItem.FixedKind = fkLeft then
      begin
        ABounds.Right := ABounds.Left + AItem.Width;
        AItem.Calculate(ABounds);
        ABounds.Left := ABounds.Right;

        Dec(ABounds.Left, BorderOverlapSize);
      end;
    end;
    AScrollableAreaBounds.Left := ABounds.Left;
  end;

  procedure ProcessRightFixedItems;
  var
    I: Integer;
    AItem: TcxGridBandViewInfo;
  begin
    ABounds := GetRootItemsAreaBounds;
    for I := RootItemCount - 1 downto 0 do
    begin
      AItem := RootItems[I];
      if AItem.FixedKind = fkRight then
      begin
        ABounds.Left := ABounds.Right - AItem.Width;
        AItem.Calculate(ABounds);
        ABounds.Right := ABounds.Left;

        Inc(ABounds.Right, BorderOverlapSize);
      end;
    end;
    AScrollableAreaBounds.Right := ABounds.Right;
  end;

  procedure ProcessNonFixedItems;
  var
    I: Integer;
    AItem: TcxGridBandViewInfo;
  begin
    ABounds := AScrollableAreaBounds;
    if not FContainerViewInfo.IsScrollable then
      Dec(ABounds.Left, GridViewInfo.LeftPos);
    for I := 0 to RootItemCount - 1 do
    begin
      AItem := RootItems[I];
      if AItem.FixedKind = fkNone then
      begin
        ABounds.Right := ABounds.Left + AItem.Width;
        AItem.Calculate(ABounds);
        ABounds.Left := ABounds.Right;

        Dec(ABounds.Left, BorderOverlapSize);
      end;
    end;
  end;

begin
  AScrollableAreaBounds := GetRootItemsAreaBounds;
  ProcessLeftFixedItems;
  ProcessRightFixedItems;
  ProcessNonFixedItems;
end;

function TcxGridBandsViewInfo.CalculateBandHeaderHeight(ABandHeaderViewInfo: TcxGridBandHeaderViewInfo): Integer;

  function GetBaseBandHeaderHeight: Integer;
  var
    AParams: TcxViewParams;
    ATouchableElementHeight: Integer;
  begin
    if ABandHeaderViewInfo = nil then
    begin
      GridView.Styles.GetBandHeaderParams(nil, AParams);
      Result := GetItemClass.GetHeaderViewInfoClass.GetCellHeight(
        GridViewInfo.GetFontHeight(AParams.Font), FContainerViewInfo.LookAndFeelPainter, GridViewInfo.ScaleFactor);
    end
    else
      Result := ABandHeaderViewInfo.GetTextCellHeight(GridViewInfo, FContainerViewInfo.LookAndFeelPainter);

    if cxIsTouchModeEnabled then
    begin
      ATouchableElementHeight := Result;
      Dec(ATouchableElementHeight, 2 * GridViewInfo.ScaleFactor.Apply(cxGridCellTextOffset));
      dxAdjustToTouchableSize(ATouchableElementHeight, GridViewInfo.ScaleFactor);
      Inc(ATouchableElementHeight, 2 * GridViewInfo.ScaleFactor.Apply(cxGridCellTextOffset));
      Result := ATouchableElementHeight;
    end;
  end;

begin
  if IsBandHeaderHeightAssigned then
    Result := GridView.OptionsView.BandHeaderHeight
  else
    Result := GetBaseBandHeaderHeight * GridView.OptionsView.BandHeaderLineCount;
end;

procedure TcxGridBandsViewInfo.CalculateBandHeaderRowHeights;
var
  I, ABandHeaderHeight, ABandLevelIndex: Integer;
begin
  if BandHeaderRowCount <> 0 then Exit;
  for I := 0 to Count - 1 do
  begin
    ABandHeaderHeight := Items[I].HeaderViewInfo.CalculateHeight;
    ABandLevelIndex := Items[I].Band.BandLevelIndex;
    if BandHeaderRowHeights[ABandLevelIndex] < ABandHeaderHeight then
      BandHeaderRowHeights[ABandLevelIndex] := ABandHeaderHeight;
  end;
end;

function TcxGridBandsViewInfo.CalculateBandHeadersAreaHeight: Integer;
var
  I: Integer;
begin
  CalculateBandHeaderRowHeights;
  Result := 0;
  for I := 0 to BandHeaderRowCount - 1 do
    Inc(Result, BandHeaderRowHeights[I]);
  if Result = 0 then
    Result := CalculateBandHeaderHeight(nil)
  else
    Dec(Result, GetBandHeadersVerticalBorderOverlapSize);
end;

procedure TcxGridBandsViewInfo.CalculateColumnWidths;
var
  I: Integer;
begin
  for I := 0 to BottomItemCount - 1 do
    BottomItems[I].CalculateColumnWidths;
end;

function TcxGridBandsViewInfo.CalculateHeight: Integer;
begin
  Result := BandHeadersAreaHeight + ColumnHeadersAreaHeight;

  if (BandHeadersAreaHeight > 0) and (ColumnHeadersAreaHeight > 0) then
//  if ShowBandHeaders and ShowColumnHeaders then
    Dec(Result, BorderOverlapSize);
end;

function TcxGridBandsViewInfo.CalculateWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  if not GridView.IsLoading {Count <> 0} then
  begin
    for I := 0 to RootItemCount - 1 do
      Inc(Result, RootItems[I].CalculateWidth);
    Dec(Result, GetRootItemsHorizontalBorderOverlapSize);
  end;
end;

function TcxGridBandsViewInfo.DrawBandHeaderBackgroundHandler(ACanvas: TcxCanvas;
  const ABounds: TRect): Boolean;
begin
  Result := BandHeaderBackgroundBitmap <> nil;
  if Result then
    ACanvas.FillRect(ABounds, BandHeaderBackgroundBitmap);
end;

function TcxGridBandsViewInfo.GetBandBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbBandBackground);
end;

function TcxGridBandsViewInfo.GetBandHeaderBackgroundBitmap: TBitmap;
begin
  Result := GridView.BackgroundBitmaps.GetBitmap(bbBandHeader);
end;

function TcxGridBandsViewInfo.GetRootItemsAreaBounds: TRect;
begin
  Result := ContainerViewInfo.Bounds;
  if GridViewInfo.HasFirstBorderOverlap then
    Dec(Result.Left, BorderOverlapSize);
end;

function TcxGridBandsViewInfo.GetItemClass: TcxGridBandViewInfoClass;
begin
  Result := TcxGridBandViewInfo;
end;

function TcxGridBandsViewInfo.IsBandHeaderHeightAssigned: Boolean;
begin
  Result := GridView.OptionsView.BandHeaderHeight <> 0;
end;

function TcxGridBandsViewInfo.ShowBandHeaders: Boolean;
begin
  Result := GridView.OptionsView.BandHeaders;
end;

function TcxGridBandsViewInfo.ShowColumnHeaders: Boolean;
begin
  Result := GridView.OptionsView.Header;
end;

procedure TcxGridBandsViewInfo.AssignRootItemWidths;
var
  I: Integer;
begin
  GridView.BeginUpdate;
  try
    for I := 0 to RootItemCount - 1 do
      with RootItems[I] do
        Band.Width := ContentWidth;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridBandsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

{function TcxGridBandsViewInfo.GetColumnsHitTest(const P: TPoint): TcxCustomGridHitTest;
var
  I: Integer;
begin
  for I := 0 to BottomItemCount - 1 do
  begin
    Result := BottomItems[I].RowsViewInfo.GetColumnsHitTest(P);
    if Result <> nil then Exit;
  end;
  Result := nil;
end;}

function TcxGridBandsViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;

  procedure ProcessItems(AFixedKind: TcxGridBandFixedKind);
  var
    I: Integer;
  begin
    if Result = nil then
      for I := 0 to RootItemCount - 1 do
        if RootItems[I].FixedKind = AFixedKind then
        begin
          Result := RootItems[I].GetHitTest(P);
          if Result <> nil then Break;
        end;
  end;

begin
  Result := nil;
  ProcessItems(fkLeft);
  ProcessItems(fkRight);
  ProcessItems(fkNone);
end;

function TcxGridBandsViewInfo.GetItemAreaBounds(ABand: TcxGridBand): TRect;
begin
  Result := GridViewInfo.ScrollableAreaBoundsHorz;
  if ABand.FixedKind <> fkNone then
    with Items[ABand.VisibleIndex].Bounds do
    begin
      Result.Left := Left;
      Result.Right := Right;
    end;
end;

procedure TcxGridBandsViewInfo.InsertPositionAtPos(const P: TPoint;
  out ABand: TcxGridBand; out AInsertPosition: TcxPosition);
var
  I: Integer;
begin
  ABand := nil;
  AInsertPosition := posLeft;
  for I := 0 to RootItemCount - 1 do
    if RootItems[I].InsertPositionAtPos(P, ABand, AInsertPosition) then Break;
end;

procedure TcxGridBandsViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, DY);
end;

procedure TcxGridBandsViewInfo.RightToLeftConversion(const ABounds: TRect);
begin
  if not IsRightToLeftConverted then
  begin
    DoRightToLeftConversion(ABounds);
    IsRightToLeftConverted := True;
  end;
end;

{ TcxGridBandedHeaderViewInfoSpecific }

function TcxGridBandedHeaderViewInfoSpecific.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited GridViewInfo);
end;

function TcxGridBandedHeaderViewInfoSpecific.CalculateHeight: Integer;
begin
  Result := Max(1, GridViewInfo.HeaderViewInfo.LineCount) * ItemHeight;
end;

{ TcxGridBandedHeaderViewInfo }

constructor TcxGridBandedHeaderViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited;
  FBandsViewInfo := GetBandsViewInfoClass.Create(Self);
end;

destructor TcxGridBandedHeaderViewInfo.Destroy;
begin
  FBandsViewInfo.Free;
  inherited;
end;

procedure TcxGridBandedHeaderViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FBandsViewInfo.RightToLeftConversion(ABounds);
end;

function TcxGridBandedHeaderViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedHeaderViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited GridViewInfo);
end;

function TcxGridBandedHeaderViewInfo.GetItem(Index: Integer): TcxGridBandedColumnHeaderViewInfo;
begin
  Result := TcxGridBandedColumnHeaderViewInfo(inherited Items[Index]);
end;

function TcxGridBandedHeaderViewInfo.GetLineCount: Integer;
begin
  Result := FBandsViewInfo.LineCount;
end;

function TcxGridBandedHeaderViewInfo.GetRowCount: Integer;
begin
  Result := FBandsViewInfo.RowCount;
end;

function TcxGridBandedHeaderViewInfo.GetSpecific: TcxGridBandedHeaderViewInfoSpecific;
begin
  Result := TcxGridBandedHeaderViewInfoSpecific(inherited Specific);
end;

procedure TcxGridBandedHeaderViewInfo.AddIndicatorItems(AIndicatorViewInfo: TcxGridIndicatorViewInfo;
  ATopBound: Integer);
begin
  if not FBandsViewInfo.AddIndicatorItems(AIndicatorViewInfo, ATopBound) then
    inherited;
end;

procedure TcxGridBandedHeaderViewInfo.CalculateColumnAutoWidths;
var
  AAutoWidthObject: TcxAutoWidthObject;
  I: Integer;
begin
  AAutoWidthObject := TcxAutoWidthObject.Create(FBandsViewInfo.Count);
  try
    for I := 0 to FBandsViewInfo.RootItemCount - 1 do
      BandsViewInfo.RootItems[I].InitAutoWidthItem(AAutoWidthObject.AddItem);

    AAutoWidthObject.AvailableWidth := GridViewInfo.ClientWidth +
      BandsViewInfo.GetRootItemsHorizontalBorderOverlapSize - IfThen(FBandsViewInfo.RootItemCount > 0, GridView.GetVerticalScrollBarAreaWidth);

    AAutoWidthObject.Calculate;
    for I := 0 to BandsViewInfo.RootItemCount - 1 do
      BandsViewInfo.RootItems[I].Width := AAutoWidthObject[I].AutoWidth;
  finally
    AAutoWidthObject.Free;
  end;
end;

procedure TcxGridBandedHeaderViewInfo.CalculateColumnWidths;
begin
  inherited;
  FBandsViewInfo.CalculateColumnWidths;
end;

function TcxGridBandedHeaderViewInfo.CalculateHeight: Integer;
begin
  Result := FBandsViewInfo.CalculateHeight;
end;

procedure TcxGridBandedHeaderViewInfo.CalculateItems;
begin
  FBandsViewInfo.Calculate;
end;

function TcxGridBandedHeaderViewInfo.GetBandsViewInfoClass: TcxGridBandsViewInfoClass;
begin
  Result := TcxGridBandsViewInfo;
end;

function TcxGridBandedHeaderViewInfo.GetColumnNeighbors(AColumn: TcxGridColumn): TcxNeighbors;
begin
  Result := [];
  if not AColumn.IsLeft then
    Include(Result, nLeft);
  if not AColumn.IsRight then
    Include(Result, nRight);
  if not AColumn.IsTop then
    Include(Result, nTop);
  if not AColumn.IsBottom then
    Include(Result, nBottom);
end;

function TcxGridBandedHeaderViewInfo.GetColumnAdditionalWidth(AColumn: TcxGridColumn): Integer;
begin
  if AColumn.IsMostLeft then
    Result := GridViewInfo.FirstItemAdditionalWidth
  else
    if AColumn.IsLeft then
      Result := GridViewInfo.BorderOverlapSize
    else
      Result := 0;
end;

function TcxGridBandedHeaderViewInfo.GetColumnWidth(AColumn: TcxGridColumn): Integer;
begin
  Result := inherited GetColumnWidth(AColumn);
end;

function TcxGridBandedHeaderViewInfo.GetIsScrollable: Boolean;
begin
  Result := FBandsViewInfo.Bands.Layout in [blNonFixed, blRightFixed];
end;

function TcxGridBandedHeaderViewInfo.GetItemAreaBounds(AItem: TcxGridColumnHeaderViewInfo): TRect;
begin
  Result := FBandsViewInfo.GetItemAreaBounds(TcxGridBandedColumn(AItem.Column).Position.Band);
end;

function TcxGridBandedHeaderViewInfo.GetItemClass: TcxGridColumnHeaderViewInfoClass;
begin
  Result := TcxGridBandedColumnHeaderViewInfo;
end;

function TcxGridBandedHeaderViewInfo.GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean;
begin
  Result := inherited GetItemMultiLinePainting(AItem) or
    (TcxGridBandedColumn(AItem.Column).Position.LineCount > 1);
end;

function TcxGridBandedHeaderViewInfo.GetItemsAreaBounds: TRect;
begin
  Result := inherited GetItemsAreaBounds;
  Inc(Result.Top, FBandsViewInfo.BandHeadersAreaHeight);
end;

function TcxGridBandedHeaderViewInfo.GetItemsHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := nil;
end;

function TcxGridBandedHeaderViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridBandedHeaderPainter;
end;

function TcxGridBandedHeaderViewInfo.GetVisible: Boolean;
begin
  with FBandsViewInfo do
    Result := ShowBandHeaders or ShowColumnHeaders;
end;

function TcxGridBandedHeaderViewInfo.GetWidth: Integer;
begin
  Result := FBandsViewInfo.CalculateWidth;
end;

function TcxGridBandedHeaderViewInfo.IsHeightAssigned: Boolean;
begin
  Result := inherited IsHeightAssigned and (LineCount <= 1);
end;

procedure TcxGridBandedHeaderViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  FBandsViewInfo.Offset(DX, 0);
end;

function TcxGridBandedHeaderViewInfo.GetHitTest(const P: TPoint): TcxCustomGridHitTest;
begin
  Result := FBandsViewInfo.GetHitTest(P);
  if Result = nil then
    Result := inherited GetHitTest(P);
end;

function TcxGridBandedHeaderViewInfo.GetZone(const P: TPoint): TcxGridItemContainerZone;
var
  AHitTest: TcxCustomGridHitTest;
  AColumn: TcxGridBandedColumn;
  APosition: TcxGridBandedColumnPosition;
  ABounds: TRect;
  ABand: TcxGridBand;
  AColIndex, ARowIndex: Integer;
begin
  Result := nil;
  AHitTest := GridViewInfo.GetHitTest(P);
  if GridViewInfo.Controller.CanHandleHitTest(AHitTest) then
    if AHitTest is TcxCustomGridColumnHitTest then
    begin
      AColumn := TcxGridBandedColumn(TcxCustomGridColumnHitTest(AHitTest).Column);
      ABounds := Items[AColumn.VisibleIndex].Bounds;
      APosition := AColumn.Position;
      if UseRightToLeftAlignment then
        AColIndex := APosition.VisibleColIndex + Ord(P.X <= (ABounds.Left + ABounds.Right) div 2)
      else
        AColIndex := APosition.VisibleColIndex + Ord(P.X >= (ABounds.Left + ABounds.Right) div 2);
      Result := TcxGridBandedColumnContainerZone.Create(AColumn.VisibleIndex, APosition.Band,
        AColIndex, APosition.VisibleRowIndex);
    end
    else
      if AHitTest is TcxGridBandHitTest then
      begin
        with TcxGridBandHitTest(AHitTest) do
        begin
          ABand := Band;
          ARowIndex := VisibleRowIndex;
        end;
        if ARowIndex <> -1 then
        begin
          if ARowIndex <> ABand.Rows.VisibleCount then
            Inc(ARowIndex);
          Result := TcxGridBandedColumnContainerZone.Create(-1, ABand, 0, ARowIndex);
        end;
      end;
end;

{ TcxGridBandedFooterViewInfo }

function TcxGridBandedFooterViewInfo.GetItemMultiLinePainting(AItem: TcxGridColumnHeaderViewInfo): Boolean;
begin
  Result := inherited GetItemMultiLinePainting(AItem) or
    (TcxGridBandedColumn(AItem.Column).Position.LineCount > 1);
end;

function TcxGridBandedFooterViewInfo.IsColumnOnFirstLayer(AColumnIndex: Integer): Boolean;
begin
  Result := TcxGridBandedColumn(GridView.VisibleColumns[AColumnIndex]).Position.Band.FixedKind <> fkNone;
end;

function TcxGridBandedFooterViewInfo.IsMultilayerLayout: Boolean;
begin
  Result := True;
end;

function TcxGridBandedFooterViewInfo.CompareCellData(Item1, Item2: Pointer): Integer;
var
  ASummaryItem1, ASummaryItem2: TcxDataSummaryItem;
  AFooterCellData1, AFooterCellData2: TcxGridFooterCellData;
begin
  AFooterCellData1 := TcxGridFooterCellData(Item1);
  AFooterCellData2 := TcxGridFooterCellData(Item2);
  ASummaryItem1 := TcxDataSummaryItem(AFooterCellData1.SummaryItem);
  ASummaryItem2 := TcxDataSummaryItem(AFooterCellData2.SummaryItem);
  Result := TcxGridColumn(ASummaryItem1.ItemLink).VisibleIndex -
    TcxGridColumn(ASummaryItem2.ItemLink).VisibleIndex;
  if Result = 0 then
    Result := ASummaryItem1.Index - ASummaryItem2.Index;
end;

procedure TcxGridBandedFooterViewInfo.Prepare(ACellDataList: TdxFastObjectList);
begin
  inherited;
  ACellDataList.Sort(CompareCellData);
end;

{ TcxGridIndicatorBandHeaderItemViewInfo }

function TcxGridIndicatorBandHeaderItemViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridIndicatorBandHeaderItemViewInfo.GetHitTestClass: TcxCustomGridHitTestClass;
begin
  Result := TcxGridIndicatorBandHeaderHitTest;
end;

function TcxGridIndicatorBandHeaderItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridIndicatorBandHeaderItemPainter;
end;

function TcxGridIndicatorBandHeaderItemViewInfo.GetText: string;
begin
  Result := cxGetResourceString(@scxGridBandsQuickCustomizationHint);
end;

procedure TcxGridIndicatorBandHeaderItemViewInfo.GetViewParams(var AParams: TcxViewParams);
begin
  GridView.Styles.GetBandHeaderParams(nil, AParams);
end;

function TcxGridIndicatorBandHeaderItemViewInfo.SupportsQuickCustomization: Boolean;
begin
  Result := GridView.OptionsCustomize.BandsQuickCustomization;
end;

function TcxGridIndicatorBandHeaderItemViewInfo.DropDownWindowExists: Boolean;
begin
  Result := GridView.Controller.HasBandsCustomizationPopup;
end;

function TcxGridIndicatorBandHeaderItemViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := GridView.Controller.BandsCustomizationPopup;
end;

{ TcxGridBandedIndicatorViewInfo }

function TcxGridBandedIndicatorViewInfo.GetAlwaysVisible: Boolean;
begin
  Result := inherited GetAlwaysVisible or
    TcxGridBandedTableView(GridView).OptionsCustomize.BandsQuickCustomization;
end;

{ TcxGridBandedDataRowCellsAreaItemViewInfo }

constructor TcxGridBandedDataRowCellsAreaItemViewInfo.Create(ACellsAreaViewInfo: TcxGridBandedDataRowCellsAreaViewInfo;
  ABandViewInfo: TcxGridBandViewInfo);
begin
  inherited Create(ACellsAreaViewInfo.RecordViewInfo);
  FCellsAreaViewInfo := ACellsAreaViewInfo;
  FBandViewInfo := ABandViewInfo;
  CreateLineBounds;
end;

destructor TcxGridBandedDataRowCellsAreaItemViewInfo.Destroy;
begin
  DestroyLineBounds;
  inherited;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetFixedBandsSeparatorLocation: TcxGridFixedBandsSeparatorLocation;
begin
  if bLeft in FBandViewInfo.Borders then
    Result := slLeft
  else
    if bRight in FBandViewInfo.Borders then
      Result := slRight
    else
      Result := slNone;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited GridViewInfo);
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetLineBounds(Index: Integer): TRect;
begin
  Result := PRect(FLineBounds[Index])^;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetLineCount: Integer;
begin
  Result := FLineBounds.Count;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetRecordsViewInfo: TcxGridBandedRowsViewInfo;
begin
  Result := TcxGridBandedRowsViewInfo(RecordViewInfo.RecordsViewInfo);
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetRecordViewInfo: TcxGridDataRowViewInfo;
begin
  Result := TcxGridDataRowViewInfo(inherited RecordViewInfo);
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.ClearLines;
var
  I: Integer;
begin
  for I := 0 to LineCount - 1 do
    Dispose(PRect(FLineBounds[I]));
  FLineBounds.Clear;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.CreateLineBounds;
begin
  FLineBounds := TList.Create;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.DestroyLineBounds;
begin
  ClearLines;
  FLineBounds.Free;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.AddLine(const ABounds: TRect);
var
  ALineBounds: PRect;
begin
  New(ALineBounds);
  ALineBounds^ := ABounds;
  FLineBounds.Add(ALineBounds);
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.AddLines;

  procedure AddCellsLines(AShowLeftLines, AShowTopLines: Boolean);
  var
    I: Integer;
    ACellViewInfo: TcxGridDataCellViewInfo;
    APosition: TcxGridBandedColumnPosition;
    R: TRect;

    procedure AddLeftLine;
    begin
      R := ACellViewInfo.Bounds;
      R.Right := R.Left;
      Dec(R.Left, GridViewInfo.GridLineWidth);
      AddLine(R);
    end;

    procedure AddTopLine;
    begin
      R := ACellViewInfo.Bounds;
      R.Bottom := R.Top;
      Dec(R.Top, GridViewInfo.GridLineWidth);
      AddLine(R);
    end;

  begin
    if not (AShowLeftLines or AShowTopLines) then Exit;
    for I := 0 to FBandViewInfo.ColumnViewInfoCount - 1 do
    begin
      ACellViewInfo := RecordViewInfo.InternalCellViewInfos[FBandViewInfo[I].Index];
      if ACellViewInfo <> nil then
      begin
        APosition := TcxGridBandedColumn(ACellViewInfo.Item).Position;
        if AShowLeftLines and (APosition.VisibleColIndex > 0) then
          AddLeftLine;
        if AShowTopLines and (APosition.VisibleRowIndex > 0) then
          AddTopLine;
      end;
    end;
  end;

begin
  if Borders <> [] then
    AddCellsLines(RecordsViewInfo.ShowCellLeftLines, RecordsViewInfo.ShowCellTopLines);
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetBorders: TcxBorders;
begin
  Result := RecordViewInfo.GetCellBorders(FBandViewInfo.IsRight, CellsAreaViewInfo.IsBottom);
  if GridViewInfo.FixedBandSeparatorWidth <> 0 then
    if FBandViewInfo.IsRightToLeftConverted then
      Result := Result - TdxRightToLeftLayoutConverter.ConvertBorders(FBandViewInfo.Borders)
    else
      Result := Result - FBandViewInfo.Borders;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetFixedBandsSeparatorBounds: TRect;
begin
  Result := ContentBounds;
  with Result do
    case FixedBandsSeparatorLocation of
      slLeft:
        Right := Left + GridViewInfo.FixedBandSeparatorWidth;
      slRight:
        Left := Right - GridViewInfo.FixedBandSeparatorWidth;
    end;
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.GetPainterClass: TcxCustomGridCellPainterClass;
begin
  Result := TcxGridBandedDataRowCellsAreaItemPainter;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.GetViewParams(var AParams: TcxViewParams);
var
  ACellPos: TcxGridDataCellPos;
begin
  if RecordViewInfo.Selected then
    inherited
  else
  begin
    ACellPos := TcxGridDataCellPos.Create(RecordViewInfo.GridRecord, nil);
    try
      FBandViewInfo.Band.Styles.GetViewParams(bsContent, ACellPos, nil, AParams);
    finally
      ACellPos.Free;
    end;
  end;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.Offset(DX, DY: Integer);
begin
  inherited;
  ClearLines;
  AddLines;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.BeforeRecalculation;
begin
  inherited;
  ClearLines;
  RecordViewInfo.BeforeCellRecalculation(Self);
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
begin
  if GridViewInfo.IsRightToLeftConverted then
  begin
    if FixedBandsSeparatorLocation = slRight then
      Dec(AWidth, GridViewInfo.BorderOverlapSize);
  end
  else
    if FixedBandsSeparatorLocation = slLeft then
    begin
      Inc(ALeftBound, GridViewInfo.BorderOverlapSize);
      Dec(AWidth, GridViewInfo.BorderOverlapSize);
    end;
  inherited;
  AddLines;
  if GridViewInfo.IsRightToLeftConverted then
    RightToLeftConversion(Bounds);
end;

function TcxGridBandedDataRowCellsAreaItemViewInfo.CanDrawSelected: Boolean;
begin
  Result := True;
end;

procedure TcxGridBandedDataRowCellsAreaItemViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to LineCount - 1 do
    PRect(FLineBounds[I])^ := TdxRightToLeftLayoutConverter.ConvertRect(LineBounds[I], ABounds);
end;

{ TcxGridBandedDataRowCellsAreaViewInfo }

constructor TcxGridBandedDataRowCellsAreaViewInfo.Create(ARecordViewInfo: TcxCustomGridRecordViewInfo);
begin
  inherited;
  CreateItems;
end;

destructor TcxGridBandedDataRowCellsAreaViewInfo.Destroy;
begin
  DestroyItems;
  inherited;
end;

function TcxGridBandedDataRowCellsAreaViewInfo.GetBandsViewInfo: TcxGridBandsViewInfo;
begin
  Result := GridViewInfo.HeaderViewInfo.BandsViewInfo;
end;

function TcxGridBandedDataRowCellsAreaViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridBandedDataRowCellsAreaViewInfo.GetGridViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited GridViewInfo);
end;

function TcxGridBandedDataRowCellsAreaViewInfo.GetItem(Index: Integer): TcxGridBandedDataRowCellsAreaItemViewInfo;
begin
  Result := TcxGridBandedDataRowCellsAreaItemViewInfo(FItems[Index]);
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.CreateItems;
var
  I: Integer;
begin
  FItems := TList.Create;
  for I := 0 to BandsViewInfo.BottomItemCount - 1 do
    FItems.Add(GetItemClass.Create(Self, BandsViewInfo.BottomItems[I]));
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.DestroyItems;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do Items[I].Free;
  FItems.Free;
end;

function TcxGridBandedDataRowCellsAreaViewInfo.CalculateVisible: Boolean;
begin
  Result := TcxGridBandedRowsViewInfo(RecordViewInfo.RecordsViewInfo).RowCellsAreaVisible;
end;

function TcxGridBandedDataRowCellsAreaViewInfo.GetItemClass: TcxGridBandedDataRowCellsAreaItemViewInfoClass;
begin
  Result := TcxGridBandedDataRowCellsAreaItemViewInfo;
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.Offset(DX, DY: Integer);
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].DoOffset(DX, DY);
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.BeforeRecalculation;
var
  I: Integer;
begin
  inherited;
  for I := 0 to Count - 1 do
    Items[I].BeforeRecalculation;
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.Calculate(ALeftBound, ATopBound: Integer;
  AWidth: Integer = -1; AHeight: Integer = -1);
var
  R, ABandBounds: TRect;
  I: Integer;
begin
  inherited;
  R := Bounds;
  for I := 0 to Count - 1 do
  begin
    ABandBounds := Items[I].BandViewInfo.Bounds;
    if GridViewInfo.IsRightToLeftConverted then
    begin
      if I <> 0 then
        R.Right := ABandBounds.Right;
      R.Left := ABandBounds.Left;
    end
    else
    begin
      if I <> 0 then
        R.Left := ABandBounds.Left;
      R.Right := ABandBounds.Right;
    end;
    Items[I].Calculate(R);
  end;
end;

procedure TcxGridBandedDataRowCellsAreaViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridBandedDataRowCellsAreaViewInfo.DrawMergedCells: Boolean;
begin
  Result := True;
end;

{ TcxGridBandedRowsViewInfo }

function TcxGridBandedRowsViewInfo.GetHeaderViewInfo: TcxGridBandedHeaderViewInfo;
begin
  Result := TcxGridBandedHeaderViewInfo(inherited HeaderViewInfo);
end;

function TcxGridBandedRowsViewInfo.CalculateDataRowHeight: Integer;
begin
  Result := inherited CalculateDataRowHeight;
  if HeaderViewInfo.LineCount <> 0 then
    Result := Result * HeaderViewInfo.LineCount;
end;

function TcxGridBandedRowsViewInfo.CalculateRowCellsAreaVisible: Boolean;
var
  I: Integer;
begin
  with HeaderViewInfo.BandsViewInfo do
  begin
    Result := LineCount > 1;
    if not Result then
      if Count = 0 then
        Result := True
      else
        for I := 0 to Count - 1 do
          with Items[I] do
            if (ColumnViewInfoCount = 0) or (FixedKind <> fkNone) or AreAllColumnsFixed then
            begin
              Result := True;
              Break;
            end;
  end;
end;

function TcxGridBandedRowsViewInfo.GetAreaBoundsForCell(ACellViewInfo: TcxGridTableDataCellViewInfo): TRect;
var
  ABand: TcxGridBand;
begin
  ABand := TcxGridBandedColumn(ACellViewInfo.Item).Position.Band;
  if (ABand <> nil) and (ABand.FixedKind = fkNone) then
    Result := GridViewInfo.ScrollableAreaBoundsHorz
  else
    Result := inherited GetAreaBoundsForCell(ACellViewInfo);
end;

function TcxGridBandedRowsViewInfo.GetPainterClass: TcxCustomGridRecordsPainterClass;
begin
  Result := TcxGridBandedRowsPainter;
end;

function TcxGridBandedRowsViewInfo.GetRowWidth: Integer;
var
  AClientWidth: Integer;
begin
  Result := inherited GetRowWidth;
  AClientWidth := GridViewInfo.ClientWidth;
  if Result > AClientWidth then
    case HeaderViewInfo.BandsViewInfo.Bands.Layout of
      blLeftFixed:
        Dec(Result, GridViewInfo.LeftPos);
      blRightFixed:
        Result := AClientWidth + GridViewInfo.LeftPos;
      blLeftRightFixed:
        Result := AClientWidth;
    end;
end;

function TcxGridBandedRowsViewInfo.GetShowBandSeparators: Boolean;
begin
  Result := (HeaderViewInfo.LineCount > 1) and (GridLines in [glBoth, glVertical]);
end;

function TcxGridBandedRowsViewInfo.GetShowCellLeftLines: Boolean;
begin
  Result := (HeaderViewInfo.LineCount > 1) and (GridLines in [glBoth, glVertical]);
end;

function TcxGridBandedRowsViewInfo.GetShowCellTopLines: Boolean;
begin
  Result := (HeaderViewInfo.RowCount > 1) and (GridLines = glBoth);
end;

procedure TcxGridBandedRowsViewInfo.AfterConstruction;
begin
  FRowCellsAreaVisible := CalculateRowCellsAreaVisible;
  inherited;
end;

function TcxGridBandedRowsViewInfo.GetDataRowCellsAreaViewInfoClass: TClass;
begin
  Result := TcxGridBandedDataRowCellsAreaViewInfo;
end;

function TcxGridBandedRowsViewInfo.IsCellMultiLine(AItem: TcxCustomGridTableItem): Boolean;
begin
  Result := inherited IsCellMultiLine(AItem) or
    (TcxGridBandedColumn(AItem).Position.LineCount > 1);
end;

{ TcxGridBandedTableViewInfo }

function TcxGridBandedTableViewInfo.GetController: TcxGridBandedTableController;
begin
  Result := TcxGridBandedTableController(inherited Controller);
end;

function TcxGridBandedTableViewInfo.GetFixedBandSeparatorColor: TColor;
begin
  Result := GridView.OptionsView.GetFixedBandSeparatorColor;
end;

function TcxGridBandedTableViewInfo.GetFixedBandSeparatorWidth: Integer;
begin
  Result := GridView.OptionsView.FixedBandSeparatorWidth;
end;

function TcxGridBandedTableViewInfo.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedTableViewInfo.GetHeaderViewInfo: TcxGridBandedHeaderViewInfo;
begin
  Result := TcxGridBandedHeaderViewInfo(inherited HeaderViewInfo);
end;

function TcxGridBandedTableViewInfo.GetColumnFooterWidth(
  AFooterViewInfo: TcxGridFooterViewInfo; AColumn: TcxGridColumn): Integer;
begin
  Result := HeaderViewInfo[AColumn.VisibleIndex].Width;
  if HasFirstBorderOverlap and AColumn.IsMostLeft then
    Dec(Result, AFooterViewInfo.BorderSize[bLeft]);
  if AColumn.IsLeft then
    Dec(Result, BorderOverlapSize);
  if AColumn.IsMostRight then
    Dec(Result, AFooterViewInfo.BorderSize[bRight]);
end;

function TcxGridBandedTableViewInfo.GetFirstItemAdditionalWidth: Integer;
begin
  Result := inherited GetFirstItemAdditionalWidth;
  if HasFirstBorderOverlap then
    Inc(Result, BorderOverlapSize);
end;

function TcxGridBandedTableViewInfo.GetScrollableAreaBoundsForEdit: TRect;
begin
  Result := inherited GetScrollableAreaBoundsForEdit;
  if not GridView.InplaceEditForm.Visible then
    with ScrollableAreaBoundsHorz do
    begin
      Result.Left := Left;
      Result.Right := Right;
    end;
end;

function TcxGridBandedTableViewInfo.GetScrollableAreaBoundsHorz: TRect;
var
  ABands: TcxGridBands;
begin
  Result := inherited GetScrollableAreaBoundsHorz;
  if not GridView.InplaceEditForm.Visible then
  begin
    ABands := GridView.Bands;
    if ABands.VisibleLeftFixedCount <> 0 then
      if IsRightToLeftConverted then
        Result.Right := HeaderViewInfo.BandsViewInfo.LastFixedItems[fkLeft, 0].Bounds.Left
      else
        Result.Left := HeaderViewInfo.BandsViewInfo.LastFixedItems[fkLeft, 0].Bounds.Right;
    if ABands.VisibleRightFixedCount <> 0 then
      if IsRightToLeftConverted then
        Result.Left := HeaderViewInfo.BandsViewInfo.LastFixedItems[fkRight, 0].Bounds.Right
      else
        Result.Right := HeaderViewInfo.BandsViewInfo.LastFixedItems[fkRight, 0].Bounds.Left;
  end;
end;

function TcxGridBandedTableViewInfo.SupportsAutoHeight: Boolean;
begin
  Result := inherited SupportsAutoHeight and (HeaderViewInfo.LineCount = 1);
end;

function TcxGridBandedTableViewInfo.SupportsGroupSummariesAlignedWithColumns: Boolean;
begin
  Result := inherited SupportsGroupSummariesAlignedWithColumns and
    (HeaderViewInfo.RowCount = 1) and (GridView.Bands.Layout = blNonFixed);
end;

function TcxGridBandedTableViewInfo.SupportsMultipleFooterSummaries: Boolean;
begin
  Result := inherited SupportsMultipleFooterSummaries and (HeaderViewInfo.RowCount = 1);
end;

function TcxGridBandedTableViewInfo.GetFooterPainterClass: TcxGridFooterPainterClass;
begin
  Result := TcxGridBandedFooterPainter;
end;

function TcxGridBandedTableViewInfo.GetFooterViewInfoClass: TcxGridFooterViewInfoClass;
begin
  Result := TcxGridBandedFooterViewInfo;
end;

function TcxGridBandedTableViewInfo.GetHeaderViewInfoClass: TcxGridHeaderViewInfoClass;
begin
  Result := TcxGridBandedHeaderViewInfo;
end;

function TcxGridBandedTableViewInfo.GetHeaderViewInfoSpecificClass: TcxGridHeaderViewInfoSpecificClass;
begin
  Result := TcxGridBandedHeaderViewInfoSpecific;
end;

function TcxGridBandedTableViewInfo.GetIndicatorViewInfoClass: TcxGridIndicatorViewInfoClass;
begin
  Result := TcxGridBandedIndicatorViewInfo;
end;

function TcxGridBandedTableViewInfo.GetRecordsViewInfoClass: TcxCustomGridRecordsViewInfoClass;
begin
  Result := TcxGridBandedRowsViewInfo;
end;

function TcxGridBandedTableViewInfo.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY: Integer): Boolean;
begin
  Result := inherited CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta, DX, DY) and
    ((ARecordCountDelta <> 0) or (APixelScrollRecordOffsetDelta <> 0) or (GridView.Bands.Layout = blNonFixed));
end;

function TcxGridBandedTableViewInfo.GetCellHeight(AIndex, ACellHeight: Integer): Integer;
begin
  Result := HeaderViewInfo[AIndex].Column.Position.LineCount * ACellHeight;
end;

function TcxGridBandedTableViewInfo.GetCellTopOffset(AIndex, ACellHeight: Integer): Integer;
begin
  Result := HeaderViewInfo[AIndex].Column.Position.Row.LineOffset * ACellHeight;
end;

{ TcxCustomGridBandedColumnOptions }

constructor TcxCustomGridBandedColumnOptions.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FVertSizing := True;
end;

procedure TcxCustomGridBandedColumnOptions.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedColumnOptions then
    with TcxGridBandedColumnOptions(Source) do
      Self.VertSizing := VertSizing;
  inherited;
end;

procedure TcxCustomGridBandedColumnOptions.SetVertSizing(Value: Boolean);
begin
  if FVertSizing <> Value then
  begin
    FVertSizing := Value;
    Changed;
  end;
end;

{ TcxGridBandedColumnStyles }

function TcxGridBandedColumnStyles.GetItem: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited Item);
end;

procedure TcxGridBandedColumnStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
var
  ADataCellPos: TcxGridDataCellPos;
begin
  if (Index = isContent) and (Item.Position.Band <> nil) then
  begin
    ADataCellPos := TcxGridDataCellPos.Create(TcxCustomGridRecord(AData), Item);
    try
      Item.Position.Band.Styles.GetViewParams(bsContent, ADataCellPos, nil, AParams);
    finally
      ADataCellPos.Free;
    end;
  end
  else
    inherited;
end;

{ TcxGridBandedColumnPosition }

constructor TcxGridBandedColumnPosition.Create(AItem: TcxCustomGridTableItem);
begin
  inherited;
  FBandIndex := -1;
  FColIndex := -1;
  FLineCount := 1;
  FRowIndex := -1;
  FVisibleColIndex := -1;
end;

function TcxGridBandedColumnPosition.GetBandIndex: Integer;
begin
  if FBand = nil then
    Result := -1
  else
    Result := FBand.Index;
end;

function TcxGridBandedColumnPosition.GetColIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.IndexOf(Item);
end;

function TcxGridBandedColumnPosition.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedColumnPosition.GetItem: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited Item);
end;

function TcxGridBandedColumnPosition.GetRowIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.Index;
end;

function TcxGridBandedColumnPosition.GetVisibleBandIndex: Integer;
begin
  if FBand = nil then
    Result := -1
  else
    Result := FBand.VisibleIndex;
end;

function TcxGridBandedColumnPosition.GetVisibleRowIndex: Integer;
begin
  if Row = nil then
    Result := -1
  else
    Result := Row.VisibleIndex;
end;

procedure TcxGridBandedColumnPosition.SetBand(Value: TcxGridBand);
begin
  FBand := Value;
  if FBand = nil then
    FVisibleColIndex := -1;
end;

procedure TcxGridBandedColumnPosition.SetBandIndex(Value: Integer);
var
  ANewBand: TcxGridBand;
  APrevVisible: Boolean;
begin
  if BandIndex <> Value then
  begin
    if (0 <= Value) and (Value < GridView.Bands.Count) then
      ANewBand := GridView.Bands[Value]
    else
      ANewBand := nil;
    if (ANewBand <> nil) and not ANewBand.IsBottom then
      ANewBand.MoveBandsToRoot;
    APrevVisible := Item.ActuallyVisible;
    GridView.BeginUpdate;
    try
      if FBand <> nil{BandIndex <> -1} then
        FBand.RemoveColumn(Item);
      if ANewBand <> nil then
        ANewBand.AddColumn(Item);
      if Item.IsReading or Item.IsUpdating then
        FBandIndex := BandIndex;
      if Item.ActuallyVisible <> APrevVisible then
        GridView.ItemVisibilityChanged(Item, Item.ActuallyVisible)
      else
        GridView.RefreshVisibleItemsList;
      Changed(ticSize);
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridBandedColumnPosition.SetColIndex(Value: Integer);
begin
  if FBand <> nil then
    if Item.IsLoading or Item.IsUpdating or GridView.IsAssigningItems then
      FColIndex := Value
    else
      if Value >= 0 then
      begin
        FBand.MoveColumn(Item, RowIndex, Value);
        Changed(ticSize);
      end;
end;

procedure TcxGridBandedColumnPosition.SetLineCount(Value: Integer);
begin
  CheckLineCount(Value);
  if FLineCount <> Value then
  begin
    FLineCount := Value;
    Changed(ticSize);
  end;
end;

procedure TcxGridBandedColumnPosition.SetRowIndex(Value: Integer);
begin
  if (FBand <> nil) and (RowIndex <> Value) then
    if Item.IsLoading or Item.IsUpdating or GridView.IsAssigningItems then
      FRowIndex := Value
    else
      if Value >= 0 then
      begin
        FBand.MoveColumn(Item, Value, -1);
        Changed(ticSize);
      end;
end;

procedure TcxGridBandedColumnPosition.CheckLineCount(var Value: Integer);
begin
  if Value < 1 then Value := 1;
end;

procedure TcxGridBandedColumnPosition.SaveParams(ABandIndexOnly: Boolean = False);
begin
  FBandIndex := BandIndex;
  if not ABandIndexOnly then
  begin
    FColIndex := ColIndex;
    FRowIndex := RowIndex;
  end;
end;

procedure TcxGridBandedColumnPosition.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedColumnPosition then
    with TcxGridBandedColumnPosition(Source) do
    begin
      Self.BandIndex := BandIndex;
      Self.RowIndex := RowIndex;
      Self.ColIndex := ColIndex;
      Self.LineCount := LineCount;
    end;
  inherited;
end;

{ TcxGridBandedColumn }

destructor TcxGridBandedColumn.Destroy;
begin
  FPosition.BandIndex := -1;
  inherited;
end;

function TcxGridBandedColumn.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

function TcxGridBandedColumn.GetOptions: TcxGridBandedColumnOptions;
begin
  Result := TcxGridBandedColumnOptions(inherited Options);
end;

function TcxGridBandedColumn.GetStyles: TcxGridBandedColumnStyles;
begin
  Result := TcxGridBandedColumnStyles(inherited Styles);
end;

procedure TcxGridBandedColumn.SetOptions(Value: TcxGridBandedColumnOptions);
begin
  Options.Assign(Value);
end;

procedure TcxGridBandedColumn.SetPosition(Value: TcxGridBandedColumnPosition);
begin
  FPosition.Assign(Value);
end;

procedure TcxGridBandedColumn.SetStyles(Value: TcxGridBandedColumnStyles);
begin
  inherited Styles := Value;
end;

function TcxGridBandedColumn.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('BandIndex');
    Add('ColIndex');
    Add('RowIndex');
    Add('LineCount');
  end;
  Result := inherited GetStoredProperties(AProperties);
end;

procedure TcxGridBandedColumn.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'BandIndex' then
    AValue := Position.BandIndex
  else
    if AName = 'ColIndex' then
      AValue := Position.ColIndex
    else
      if AName = 'RowIndex' then
        AValue := Position.RowIndex
      else
        if AName = 'LineCount' then
          AValue := Position.LineCount
        else
          inherited;
end;

procedure TcxGridBandedColumn.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'BandIndex' then
    Position.FBandIndex := AValue
  else
    if AName = 'ColIndex' then
      Position.FColIndex := AValue
    else
      if AName = 'RowIndex' then
        Position.FRowIndex := AValue
      else
        if AName = 'LineCount' then
          Position.LineCount := AValue
        else
          inherited;
end;

procedure TcxGridBandedColumn.CreateSubClasses;
begin
  inherited;
  FPosition := TcxGridBandedColumnPosition.Create(Self);
end;

procedure TcxGridBandedColumn.DestroySubClasses;
begin
  FreeAndNil(FPosition);
  inherited;
end;

function TcxGridBandedColumn.GetOptionsClass: TcxCustomGridTableItemOptionsClass;
begin
  Result := TcxGridBandedColumnOptions;
end;

function TcxGridBandedColumn.GetStylesClass: TcxCustomGridTableItemStylesClass;
begin
  Result := TcxGridBandedColumnStyles;
end;

function TcxGridBandedColumn.CanCellMerging: Boolean;
begin
  Result := inherited CanCellMerging and
    (FPosition.LineCount = GridView.ViewInfo.HeaderViewInfo.LineCount);
end;

procedure TcxGridBandedColumn.AssignColumnWidths;

  function GetRowViewInfo: TcxGridBandRowViewInfo;
  begin
    Result := GridView.ViewInfo.HeaderViewInfo.BandsViewInfo[Position.VisibleBandIndex].RowsViewInfo[Position.VisibleRowIndex];
  end;

begin
  if Position.Band.Width <> 0 then
    GetRowViewInfo.AssignColumnWidths
  else
    if Position.Band.HasParentWithAssignedWidth then
      Position.Band.ParentBandWithAssignedWidth.AssignColumnWidths
    else
      if (Position.Band.Rows.VisibleCount > 1) and
        (Position.Row.Width < Position.Band.Rows.Width) then
        GetRowViewInfo.AssignColumnWidths
      else
        inherited;
end;

function TcxGridBandedColumn.CanScroll: Boolean;
begin
  Result := inherited CanScroll and
    (Position.Band <> nil) and (Position.Band.FixedKind = fkNone);
end;

function TcxGridBandedColumn.CanVertSize: Boolean;
begin
  Result := GridView.OptionsCustomize.ColumnVertSizing and Options.VertSizing;
end;

function TcxGridBandedColumn.DefaultAlternateCaption: string;
var
  ABand: TcxGridBand;
begin
  Result := inherited DefaultAlternateCaption;
  if GridView.OptionsView.BandCaptionsInColumnAlternateCaption then
  begin
    ABand := Position.Band;
    while ABand <> nil do
    begin
      Result := ABand.GetAlternateCaption + cxGridBandedTableViewAlternateCaptionSeparator + Result;
      ABand := ABand.ParentBand;
    end;
  end;
end;

function TcxGridBandedColumn.GetActuallyVisible: Boolean;
begin
  Result := inherited GetActuallyVisible and
    (Position.Band <> nil) and Position.Band.ActuallyVisible;
end;

function TcxGridBandedColumn.GetEditPartVisible: Boolean;
var
  R: TRect;

  function HasPoint(X, Y: Integer): Boolean;
  var
    AHitTest: TcxCustomGridHitTest;
  begin
    AHitTest := GridView.ViewInfo.GetHitTest(X, Y);
    Result := (AHitTest is TcxGridRecordCellHitTest) and
      (TcxGridRecordCellHitTest(AHitTest).Item = Self);
  end;

begin
  if CanDataCellScroll then
    Result := inherited GetEditPartVisible
  else
    with GridView.ViewInfo.ScrollableAreaBoundsForEdit do
    begin
      Result := Right < Left;  // left fixed and right fixed bands overlap
      if Result then
      begin
        R := FocusedCellViewInfo.EditBounds;
        // R.Top is ignored because it is always visible if R.Bottom - 1 is visible
        // Also R.Top might intersect with the header sizing hittest area
        Result := not HasPoint(R.Left, R.Bottom - 1) or not HasPoint(R.Right - 1, R.Bottom - 1);
      end
      else
        Result := FocusedCellViewInfo.EditBounds.Bottom > Bottom;
    end;
end;

function TcxGridBandedColumn.GetIsBottom: Boolean;
begin
  Result := (Position.Row <> nil) and Position.Row.IsLast;
end;

function TcxGridBandedColumn.GetIsLeft: Boolean;
begin
  Result := Position.VisibleColIndex = 0;
end;

function TcxGridBandedColumn.GetIsMostBottom: Boolean;
begin
  Result := IsBottom and
    (Position.VisibleRowIndex = GridView.Bands.VisibleRowCount - 1);
end;

function TcxGridBandedColumn.GetIsMostLeft: Boolean;
begin
  Result := IsLeft and Position.Band.IsFirst;
end;

function TcxGridBandedColumn.GetIsMostRight: Boolean;
begin
  Result := IsRight and Position.Band.IsLast;
end;

function TcxGridBandedColumn.GetIsRight: Boolean;
begin
  Result := (Position.Row <> nil) and
    (Position.VisibleColIndex = Position.Row.VisibleCount - 1);
end;

function TcxGridBandedColumn.GetIsTop: Boolean;
begin
  Result := (Position.Row <> nil) and Position.Row.IsFirst;
end;

function TcxGridBandedColumn.GetVisibleInQuickCustomizationPopup: Boolean;
begin
  Result := inherited GetVisibleInQuickCustomizationPopup and
    (FPosition.Band <> nil) and FPosition.Band.ActuallyVisible;
end;

procedure TcxGridBandedColumn.VisibleChanged;
begin
  inherited;
  if Position.Row <> nil then
    Position.Row.RefreshVisibleItemsList;
end;

procedure TcxGridBandedColumn.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedColumn then
    with TcxGridBandedColumn(Source) do
    begin
      Self.Position := Position;
    end;
  inherited;
end;

{ TcxGridBandRow }

constructor TcxGridBandRow.Create(ABandRows: TcxGridBandRows);
begin
  inherited Create;
  FBandRows := ABandRows;
  FItems := TList.Create;
  FVisibleItems := TList.Create;
end;

destructor TcxGridBandRow.Destroy;
begin
  FBandRows.RemoveItem(Self);
  FVisibleItems.Free;
  FItems.Free;
  inherited;
end;

function TcxGridBandRow.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridBandRow.GetIndex: Integer;
begin
  Result := FBandRows.FItems.IndexOf(Self);
end;

function TcxGridBandRow.GetIsFirst: Boolean;
begin
  Result := VisibleIndex = 0;
end;

function TcxGridBandRow.GetIsLast: Boolean;
begin
  Result := VisibleIndex = FBandRows.VisibleCount - 1;
end;

function TcxGridBandRow.GetItem(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(FItems[Index]);
end;

function TcxGridBandRow.GetLineCount: Integer;
var
  I, ALineCount: Integer;
begin
  Result := 1;
  for I := 0 to VisibleCount - 1 do
  begin
    ALineCount := VisibleItems[I].Position.LineCount;
    if ALineCount > Result then Result := ALineCount;
  end;
end;

function TcxGridBandRow.GetLineOffset: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleIndex - 1 do
    Inc(Result, FBandRows.VisibleItems[I].LineCount);
end;

function TcxGridBandRow.GetVisible: Boolean;
begin
  Result := (VisibleCount <> 0) or (Count = 0); // for new just inserted empty rows
end;

function TcxGridBandRow.GetVisibleCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxGridBandRow.GetVisibleIndex: Integer;
begin
  Result := FBandRows.FVisibleItems.IndexOf(Self);
end;

function TcxGridBandRow.GetVisibleItem(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(FVisibleItems[Index]);
end;

function TcxGridBandRow.GetWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleCount - 1 do
    Inc(Result, VisibleItems[I].Width);
end;

procedure TcxGridBandRow.CheckIndexForInsert(var AIndex: Integer; AExistingItem: Boolean);
begin
  CheckItemIndexForInsert(AIndex, Count, AExistingItem);
end;

procedure TcxGridBandRow.PopulateTabOrderList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to VisibleCount - 1 do
    AList.Add(VisibleItems[I]);
end;

procedure TcxGridBandRow.RefreshVisibleItemsList;
var
  I: Integer;
begin
  FVisibleItems.Clear;
  for I := 0 to Count - 1 do
    if Items[I].Visible then
      Items[I].Position.FVisibleColIndex := FVisibleItems.Add(Items[I])
    else
      Items[I].Position.FVisibleColIndex := -1;
  FBandRows.RefreshVisibleItemsList;
end;

procedure TcxGridBandRow.ApplyBestFit(ACheckSizingAbility: Boolean = False;
  AFireEvents: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to VisibleCount - 1 do
    VisibleItems[I].ApplyBestFit(ACheckSizingAbility, AFireEvents);
end;

procedure TcxGridBandRow.Delete(AIndex: Integer);
begin
  Items[AIndex].Position.FRow := nil;
  FItems.Delete(AIndex);
  if Count = 0 then
    Free
  else
    RefreshVisibleItemsList;
end;

function TcxGridBandRow.IndexOf(AColumn: TcxGridBandedColumn): Integer;
begin
  Result := FItems.IndexOf(AColumn);
end;

procedure TcxGridBandRow.Insert(AIndex: Integer; AColumn: TcxGridBandedColumn);
begin
  CheckIndexForInsert(AIndex, False);
  FItems.Insert(AIndex, AColumn);
  AColumn.Position.FRow := Self;
  RefreshVisibleItemsList;
end;

procedure TcxGridBandRow.Move(ACurIndex, ANewIndex: Integer);
begin
  CheckIndexForInsert(ANewIndex, True);
  FItems.Move(ACurIndex, ANewIndex);
  RefreshVisibleItemsList;
end;

{ TcxGridBandRows }

constructor TcxGridBandRows.Create(ABand: TcxGridBand);
begin
  inherited Create;
  FBand := ABand;
  FItems := TList.Create;
  FVisibleItems := TList.Create;
end;

destructor TcxGridBandRows.Destroy;
begin
  Count := 0;
  FVisibleItems.Free;
  FItems.Free;
  inherited;
end;

function TcxGridBandRows.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridBandRows.GetFirstVisible: TcxGridBandRow;
begin
  Result := VisibleItems[0];
end;

function TcxGridBandRows.GetGridView: TcxGridBandedTableView;
begin
  Result := FBand.GridView;
end;

function TcxGridBandRows.GetItem(Index: Integer): TcxGridBandRow;
begin
  Result := TcxGridBandRow(FItems[Index]);
end;

function TcxGridBandRows.GetLastVisible: TcxGridBandRow;
begin
  Result := VisibleItems[VisibleCount - 1];
end;

function TcxGridBandRows.GetLineCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleCount - 1 do
    Inc(Result, VisibleItems[I].LineCount);
end;

function TcxGridBandRows.GetVisibleCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxGridBandRows.GetVisibleItem(Index: Integer): TcxGridBandRow;
begin
  Result := TcxGridBandRow(FVisibleItems[Index]);
end;

function TcxGridBandRows.GetWidth: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleCount - 1 do
    Inc(Result, VisibleItems[I].Width);
end;

procedure TcxGridBandRows.SetCount(Value: Integer);
var
  APrevCount, I: Integer;
begin
  if Value < 0 then Value := 0;
  if Count <> Value then
  begin
    if Value > Count then
    begin
      APrevCount := Count;
      FItems.Count := Value;
      for I := APrevCount to Count - 1 do
        FItems[I] := TcxGridBandRow.Create(Self);
    end
    else
    begin
      for I := Value to Count - 1 do Items[I].Free;
      FItems.Count := Value;
    end;
    RefreshVisibleItemsList;
  end;
end;

procedure TcxGridBandRows.RemoveItem(ARow: TcxGridBandRow);
begin
  FItems.Remove(ARow);
  RefreshVisibleItemsList;
end;

procedure TcxGridBandRows.PopulateTabOrderList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to VisibleCount - 1 do
    VisibleItems[I].PopulateTabOrderList(AList);
end;

procedure TcxGridBandRows.RefreshVisibleItemsList;
var
  I: Integer;
begin
  FVisibleItems.Clear;
  for I := 0 to Count - 1 do
    if Items[I].Visible then
      FVisibleItems.Add(Items[I]);
end;

procedure TcxGridBandRows.ApplyBestFit(ACheckSizingAbility: Boolean = False;
  AFireEvents: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to VisibleCount - 1 do
    VisibleItems[I].ApplyBestFit(ACheckSizingAbility, AFireEvents);
end;

function TcxGridBandRows.GetLineIndex(ARowIndex: Integer): Integer;
begin
  Result := VisibleItems[ARowIndex].LineOffset;
end;

function TcxGridBandRows.GetRowIndex(ALineIndex: Integer): Integer;
begin
  for Result := 0 to VisibleCount - 1 do
    with VisibleItems[Result] do
      if (LineOffset <= ALineIndex) and (ALineIndex < LineOffset + LineCount) then
        Exit;
  Result := -1;
end;

function TcxGridBandRows.Insert(AIndex: Integer): TcxGridBandRow;
begin
  if AIndex > Count then AIndex := Count;
  Result := TcxGridBandRow.Create(Self);
  FItems.Insert(AIndex, Result);
  RefreshVisibleItemsList;
end;

procedure TcxGridBandRows.MoveColumn(AColumn: TcxGridBandedColumn; ARowIndex, AColIndex: Integer);
var
  AColumnColIndex, AColumnRowIndex, APrevCount: Integer;
begin
  AColumnRowIndex := AColumn.Position.RowIndex;
  AColumnColIndex := AColumn.Position.ColIndex;

  if AColumnRowIndex <> ARowIndex then
  begin
    if AColumnRowIndex <> -1 then
    begin
      APrevCount := Count;
      Items[AColumnRowIndex].Delete(AColumnColIndex);
      if (Count <> APrevCount) and (AColumnRowIndex < ARowIndex) then
        Dec(ARowIndex);
    end;
    if ARowIndex <> -1 then
    begin
      if ARowIndex > Count then ARowIndex := Count;
      if ARowIndex = Count then Count := ARowIndex + 1;
      if AColIndex = -1 then
        AColIndex := Items[ARowIndex].Count;
      Items[ARowIndex].Insert(AColIndex, AColumn);
    end;
  end
  else
    if (ARowIndex <> -1) and (AColumnColIndex <> AColIndex) then
      if AColumnColIndex = -1 then
        Items[ARowIndex].Insert(AColIndex, AColumn)
      else
        if AColIndex = -1 then
          Items[ARowIndex].Delete(AColumnColIndex)
        else
          Items[ARowIndex].Move(AColumnColIndex, AColIndex);

  GridView.RefreshVisibleItemsList;
end;

{ TcxGridBandCustomOptions }

constructor TcxGridBandCustomOptions.Create(ABand: TcxGridBand);
begin
  inherited Create;
  FBand := ABand;
end;

procedure TcxGridBandCustomOptions.Changed(AChange: TcxGridBandChange);
begin
  FBand.Changed(AChange);
end;

procedure TcxGridBandCustomOptions.Assign(Source: TPersistent);
begin
  if not (Source is TcxGridBandCustomOptions) then
    inherited;
end;

{ TcxGridBandOptions }

constructor TcxGridBandOptions.Create(ABand: TcxGridBand);
begin
  inherited;
  FMoving := True;
  FSizing := True;
end;

procedure TcxGridBandOptions.SetHoldOwnColumnsOnly(Value: Boolean);
begin
  if FHoldOwnColumnsOnly <> Value then
  begin
    FHoldOwnColumnsOnly := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBandOptions.SetMoving(Value: Boolean);
begin
  if FMoving <> Value then
  begin
    FMoving := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBandOptions.SetSizing(Value: Boolean);
begin
  if FSizing <> Value then
  begin
    FSizing := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBandOptions.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandOptions then
    with TcxGridBandOptions(Source) do
    begin
      Self.HoldOwnColumnsOnly := HoldOwnColumnsOnly;
      Self.Moving := Moving;
      Self.Sizing := Sizing;
    end;
  inherited;
end;

{ TcxGridBandPosition }

constructor TcxGridBandPosition.Create(ABand: TcxGridBand);
begin
  inherited;
  FBandIndex := -1;
  FColIndex := -1;
end;

function TcxGridBandPosition.GetBandIndex: Integer;
begin
  if ParentBand = nil then
    Result := -1
  else
    Result := ParentBand.Index;
end;

function TcxGridBandPosition.GetColIndex: Integer;
begin
  if ParentBand = nil then
    Result := FBand.RootIndex
  else
    Result := ParentBand.ColIndexOf(FBand);
end;

function TcxGridBandPosition.GetGridView: TcxGridBandedTableView;
begin
  Result := FBand.GridView;
end;

function TcxGridBandPosition.GetParentBand: TcxGridBand;
begin
  Result := FBand.ParentBand;
end;

function TcxGridBandPosition.GetVisibleColIndex: Integer;
begin
  if ParentBand = nil then
    Result := FBand.VisibleRootIndex
  else
    Result := ParentBand.VisibleColIndexOf(FBand);
end;

procedure TcxGridBandPosition.SetBandIndex(Value: Integer);
begin
  if not FBand.IsDestroying and ((ParentBand = nil) or not ParentBand.IsDestroying) and
    (FBand.IsLoading or FBand.IsUpdating or GridView.IsAssigningBands) then
  begin
    FBandIndex := Value;
    if FBand.IsLoading or GridView.IsAssigningBands then Exit;
  end;
  if not CheckBandIndex(Value) then Exit;
  if BandIndex <> Value then
  begin
    FBand.SaveVisible;
    if not GridView.IsDestroying then
      GridView.BeginUpdate;
    try
      if ParentBand <> nil then
        ParentBand.RemoveBand(FBand);
      if not GridView.IsDestroying and
        (0 <= Value) and (Value < GridView.Bands.Count) then
        GridView.Bands[Value].AddBand(FBand);
      if not GridView.IsDestroying then
      begin
        GridView.Bands.RefreshRootItemsList;
        GridView.Bands.RefreshVisibleItemsList;
      end;
    finally
      if not GridView.IsDestroying then
        GridView.EndUpdate;
      FBand.CheckVisible;
      Changed(bcProperty);
    end;
  end;
end;

procedure TcxGridBandPosition.SetColIndex(Value: Integer);
begin
  if FBand.IsLoading or FBand.IsUpdating or GridView.IsAssigningBands then
    FColIndex := Value
  else
    if ColIndex <> Value then
      if ParentBand = nil then
        FBand.RootIndex := Value
      else
      begin
        ParentBand.MoveBand(FBand, Value);
        if FBand.ActuallyVisible then
          GridView.Bands.RefreshVisibleBottomItemsList;
        Changed(bcSize);
      end;
end;

function TcxGridBandPosition.IsColIndexStored: Boolean;
begin
  Result := not Band.IsRoot;
end;

function TcxGridBandPosition.CheckBandIndex(var Value: Integer): Boolean;
var
  ABand: TcxGridBand;
  AColIndex, I: Integer;
begin
  Result := GridView.IsDestroying;
  if Result then Exit;
  Result := (Value = -1) or
    (0 <= Value) and (Value < GridView.Bands.Count) and (FBand.Index <> Value);
  if Result and (Value <> -1) then
  begin
    ABand := GridView.Bands[Value];
    Result := not ABand.HasAsParent(FBand);
    if not Result then
    begin
      AColIndex := ColIndex;
      GridView.BeginUpdate;
      try
        for I := FBand.ChildBandCount - 1 downto 0 do
          with FBand.ChildBands[I].Position do
          begin
            BandIndex := Self.BandIndex;
            ColIndex := AColIndex;
          end;
        Value := ABand.Index;
        Result := True;
      finally
        GridView.EndUpdate;
      end;
    end;
  end;
end;

procedure TcxGridBandPosition.SaveParams;
begin
  FBandIndex := BandIndex;
  FColIndex := ColIndex;
end;

procedure TcxGridBandPosition.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandPosition then
    with TcxGridBandPosition(Source) do
    begin
      Self.BandIndex := BandIndex;
      Self.ColIndex := ColIndex;
    end;
  inherited;
end;

{ TcxGridBandStyles }

function TcxGridBandStyles.GetBand: TcxGridBand;
begin
  Result := TcxGridBand(GetOwner);
end;

function TcxGridBandStyles.GetGridViewValue: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandStyles.SetOnGetHeaderStyle(Value: TcxGridBandGetHeaderStyle);
begin
  if not dxSameMethods(FOnGetHeaderStyle, Value) then
  begin
    FOnGetHeaderStyle := Value;
    Band.Changed(bcProperty);
  end;
end;

procedure TcxGridBandStyles.GetDefaultViewParams(Index: Integer; AData: TObject;
  out AParams: TcxViewParams);
begin
  case Index of
    bsBackground:
      GridView.Styles.GetViewParams(vsBandBackground, AData, nil, AParams);
    bsContent:
      with TcxGridDataCellPos(AData) do
        GridView.Styles.GetRecordContentParams(GridRecord, Item, AParams);
    bsHeader:
      GridView.Styles.GetBandHeaderParams(TcxGridBand(AData), AParams);
  else
    inherited;
  end;
end;

function TcxGridBandStyles.GetGridView: TcxCustomGridView;
begin
  Result := Band.GridView;
end;

procedure TcxGridBandStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridBandStyles then
    with TcxGridBandStyles(Source) do
     begin
       Self.Background := Background;
       Self.Content := Content;
       Self.Header := Header;
       Self.OnGetHeaderStyle := OnGetHeaderStyle;
     end;
end;

procedure TcxGridBandStyles.GetHeaderParams(out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetHeaderStyle) then
    FOnGetHeaderStyle(GridView, Band, AStyle);
  GetViewParams(bsHeader, Band, AStyle, AParams);
end;

{ TcxGridBand }

constructor TcxGridBand.Create(Collection: TCollection);
begin
  inherited;
  FChildBands := TList.Create;
  FColumns := TList.Create;
  FHeaderAlignmentHorz := taCenter;
  FHeaderAlignmentVert := vaCenter;
  ResetLoadedIndex;
  FOptions := GetOptionsClass.Create(Self);
  FPosition := TcxGridBandPosition.Create(Self);
  FRows := TcxGridBandRows.Create(Self);
  FStyles := GetStylesClass.Create(Self);
  FVisible := True;
  FVisibleChildBands := TList.Create;
  FVisibleForCustomization := True;
  Bands.AddItem(Self);
end;

destructor TcxGridBand.Destroy;
var
  ABands: TcxGridBands;

  procedure RemoveBands;
  var
    AColIndex, I: Integer;
  begin
    AColIndex := FPosition.ColIndex;
    for I := ChildBandCount - 1 downto 0 do
      with ChildBands[I].Position do
      begin
        BandIndex := FPosition.BandIndex;
        ColIndex := AColIndex;
      end;
  end;

  procedure RemoveColumns;
  var
    I: Integer;
  begin
    for I := ColumnCount - 1 downto 0 do
      Columns[I].Position.BandIndex := -1;
  end;

begin
  if not (GridView.IsLoading or GridView.IsDestroying) and GridView.IsDesigning then
    GridView.Controller.DesignController.UnselectObject(Self);
  FIsDestroying := True;
  ABands := Bands;
  if not GridView.IsDestroying then
    GridView.BeginUpdate;
  try
    RemoveBands;
    RemoveColumns;
    FPosition.BandIndex := -1;
  finally
    if not GridView.IsDestroying then
      GridView.CancelUpdate;
  end;
  FVisibleChildBands.Free;
  FStyles.Free;
  FRows.Free;
  FPosition.Free;
  FOptions.Free;
  FColumns.Free;
  FChildBands.Free;
  inherited;
  ABands.RemoveItem(Self);
end;

function TcxGridBand.GetBandLevelIndex: Integer;
var
  ABand: TcxGridBand;
begin
  Result := 0;
  ABand := Self;
  while ABand.ParentBand <> nil do
  begin
    Inc(Result);
    ABand := ABand.ParentBand;
  end;
end;

function TcxGridBand.GetBands: TcxGridBands;
begin
  Result := TcxGridBands(Collection);
end;

function TcxGridBand.GetChildBand(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FChildBands[Index]);
end;

function TcxGridBand.GetChildBandCount: Integer;
begin
  Result := FChildBands.Count;
end;

function TcxGridBand.GetChildItem(Index: Integer): TObject;
begin
  if IsBottom then
    Result := Columns[Index]
  else
    Result := ChildBands[Index];
end;

function TcxGridBand.GetChildItemCount: Integer;
begin
  if IsBottom then
    Result := ColumnCount
  else
    Result := ChildBandCount;
end;

function TcxGridBand.GetChildItemVisible(Index: Integer): Boolean;
var
  AChildItem: TObject;
begin
  AChildItem := ChildItems[Index];
  if AChildItem is TcxGridBand then
    Result := TcxGridBand(AChildItem).SavedVisible
  else
    Result := TcxGridBandedColumn(AChildItem).SavedVisible;
end;

function TcxGridBand.GetColumnCount: Integer;
begin
  Result := FColumns.Count;
end;

function TcxGridBand.GetColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(FColumns[Index]);
end;

function TcxGridBand.GetFirstChildBottomBand: TcxGridBand;
begin
  if ChildBandCount = 0 then
    Result := Self
  else
  begin
    Result := ChildBands[0];
    while not Result.IsBottom do
      Result := Result.ChildBands[0];
  end;
end;

function TcxGridBand.GetFirstVisibleChildBottomBand: TcxGridBand;
begin
  if VisibleChildBandCount = 0 then
    Result := Self
  else
  begin
    Result := VisibleChildBands[0];
    while not Result.IsVisibleBottom do
      Result := Result.VisibleChildBands[0];
  end;
end;

function TcxGridBand.GetGridView: TcxGridBandedTableView;
begin
  if Bands = nil then
    Result := nil
  else
    Result := Bands.GridView;
end;

function TcxGridBand.GetHidden: Boolean;
begin
  Result := not VisibleForCustomization;
end;

function TcxGridBand.GetIsBottom: Boolean;
begin
  Result := ChildBandCount = 0;
end;

function TcxGridBand.GetIsEmpty: Boolean;
begin
  if IsVisibleBottom then
    Result := FRows.VisibleCount = 0
  else
    Result := VisibleChildBandCount = 0;
end;

function TcxGridBand.GetIsFirst: Boolean;
begin
  Result := VisibleBottomIndex = 0;
end;

function TcxGridBand.GetIsFirstNonEmpty: Boolean;
begin
  Result := Bands.FirstVisibleNonEmpty = Self;
end;

function TcxGridBand.GetIsLast: Boolean;
begin
  Result := VisibleBottomIndex = Bands.VisibleBottomItemCount - 1;
end;

function TcxGridBand.GetIsLastAsChild: Boolean;

  function GetVisibleBandCount: Integer;
  begin
    if FParentBand = nil then
      Result := Bands.VisibleRootItemCount
    else
      Result := FParentBand.VisibleChildBandCount;
  end;

begin
  Result := FPosition.VisibleColIndex = GetVisibleBandCount - 1;
end;

function TcxGridBand.GetIsLastNonEmpty: Boolean;
begin
  Result := Bands.LastVisibleNonEmpty = Self;
end;

function TcxGridBand.GetIsLoading: Boolean;
begin
  Result := not FIgnoreLoadingStatus and (GridView <> nil) and GridView.IsLoading;
end;

function TcxGridBand.GetIsMostRight: Boolean;
begin
  if IsRoot then
    Result := IsLastAsChild
  else
    Result := FParentBand.IsMostRight and IsLastAsChild;
end;

function TcxGridBand.GetIsRoot: Boolean;
begin
  Result := FPosition.BandIndex = -1;
end;

function TcxGridBand.GetIsUpdating: Boolean;
begin
  Result := (GridView <> nil) and GridView.IsUpdating;
end;

function TcxGridBand.GetIsVisibleBottom: Boolean;
begin
  Result := VisibleChildBandCount = 0;
end;

function TcxGridBand.GetMinWidth: Integer;
begin
  if HasFixedWidth then
    Result := FWidth
  else
    if IsEmpty then
      Result := EmptyBandMinWidth
    else
      Result := 0;
end;

function TcxGridBand.GetParentBandWithAssignedWidth: TcxGridBand;
begin
  Result := FParentBand;
  while Result <> nil do
  begin
    if Result.Width <> 0 then Break;
    Result := Result.ParentBand;
  end;
end;

function TcxGridBand.GetRootIndex: Integer;
begin
  if Bands.FRootItems = nil{GridView.IsDestroying} then
    Result := -1
  else
    Result := Bands.FRootItems.IndexOf(Self); {!!! optimize}
end;

function TcxGridBand.GetRootParentBand: TcxGridBand;
begin
  if IsRoot then
    Result := nil
  else
    Result := GetParentInParent(nil);
end;

function TcxGridBand.GetVisibleBandLevelCount: Integer;
var
  I, AChildLevelCount: Integer;
begin
  Result := 0;
  for I := 0 to VisibleChildBandCount - 1 do
  begin
    AChildLevelCount := VisibleChildBands[I].VisibleBandLevelCount;
    if Result < AChildLevelCount then Result := AChildLevelCount;
  end;
  Inc(Result);
end;

function TcxGridBand.GetVisibleBottomIndex: Integer;
begin
  Result := Bands.FVisibleBottomItems.IndexOf(Self); {!!!optimize}
end;

function TcxGridBand.GetVisibleChildBand(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FVisibleChildBands[Index]);
end;

function TcxGridBand.GetVisibleChildBandCount: Integer;
begin
  Result := FVisibleChildBands.Count;
end;

function TcxGridBand.GetVisibleIndex: Integer;
begin
  Result := Bands.FVisibleItems.IndexOf(Self); {!!!optimize}
end;

function TcxGridBand.GetVisibleRootIndex: Integer;
begin
  Result := Bands.FVisibleRootItems.IndexOf(Self); {!!!optimize}
end;

function TcxGridBand.IsTagStored: Boolean;
begin
  Result := Tag <> 0;
end;

procedure TcxGridBand.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(bcLayout);
  end;
end;

procedure TcxGridBand.SetAlternateCaption(const Value: string);
begin
  if FAlternateCaption <> Value then
  begin
    FAlternateCaption := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetFixedKind(Value: TcxGridBandFixedKind);
var
  I: Integer;
begin
  if FFixedKind <> Value then
  begin
    FFixedKind := Value;
    GridView.BeginUpdate;
    try
      for I := 0 to ChildBandCount - 1 do
        ChildBands[I].FixedKind := Value;
      GridView.RefreshVisibleItemsList;
      Bands.RefreshVisibleItemsList;
      if not IsRoot and (FFixedKind <> FParentBand.FixedKind) then
        FPosition.BandIndex := -1;
      Changed(bcProperty);
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridBand.SetHeaderAlignmentHorz(Value: TAlignment);
begin
  if FHeaderAlignmentHorz <> Value then
  begin
    FHeaderAlignmentHorz := Value;
    Changed(bcLayout);
  end;
end;

procedure TcxGridBand.SetHeaderAlignmentVert(Value: TcxAlignmentVert);
begin
  if FHeaderAlignmentVert <> Value then
  begin
    FHeaderAlignmentVert := Value;
    Changed(bcLayout);
  end;
end;

procedure TcxGridBand.SetHidden(Value: Boolean);
begin
  VisibleForCustomization := not Value;
end;

procedure TcxGridBand.SetOnHeaderClick(Value: TNotifyEvent);
begin
  if not dxSameMethods(FOnHeaderClick, Value) then
  begin
    FOnHeaderClick := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetOnGetStoredProperties(Value: TcxGridBandGetStoredPropertiesEvent);
begin
  if not dxSameMethods(FOnGetStoredProperties, Value) then
  begin
    FOnGetStoredProperties := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetOnGetStoredPropertyValue(Value: TcxGridBandGetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnGetStoredPropertyValue, Value) then
  begin
    FOnGetStoredPropertyValue := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetOnSetStoredPropertyValue(Value: TcxGridBandSetStoredPropertyValueEvent);
begin
  if not dxSameMethods(FOnSetStoredPropertyValue, Value) then
  begin
    FOnSetStoredPropertyValue := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetOptions(Value: TcxGridBandOptions);
begin
  FOptions.Assign(Value);
end;

procedure TcxGridBand.SetPosition(Value: TcxGridBandPosition);
begin
  FPosition.Assign(Value);
end;

procedure TcxGridBand.SetRootIndex(Value: Integer);
begin
  if not IsRoot or GridView.IsDestroying then Exit;
  CheckItemIndexForInsert(Value, Bands.RootItemCount, True);
  if RootIndex <> Value then
    Index := Bands.RootItems[Value].Index;
end;

procedure TcxGridBand.SetStyles(Value: TcxGridBandStyles);
begin
  FStyles.Assign(Value);
end;

procedure TcxGridBand.SetTag(Value: TcxTag);
begin
  if FTag <> Value then
  begin
    FTag := Value;
    Changed(bcProperty);
  end;
end;

procedure TcxGridBand.SetVisible(Value: Boolean);
{var
  APrevParentIsVisibleBottom: Boolean;}
begin
  if FVisible <> Value then
  begin
    GridView.BeginUpdate;
    try
  //    APrevParentIsVisibleBottom := (FParentBand <> nil) and FParentBand.IsVisibleBottom;
      SaveVisible;
      FVisible := Value;
      if FParentBand <> nil then
        FParentBand.RefreshVisibleChildBandsList;
      GridView.RefreshVisibleItemsList;
      Bands.RefreshVisibleItemsList;
      {if APrevParentIsVisibleBottom then
        FParentBand.MoveColumns(Self);}
      CheckVisible;
      GridView.RefreshCustomizationForm;
      Changed(bcProperty);
    finally
      GridView.EndUpdate;
    end;
  end;
end;

procedure TcxGridBand.SetVisibleForCustomization(Value: Boolean);
begin
  if FVisibleForCustomization <> Value then
  begin
    FVisibleForCustomization := Value;
    VisibleForCustomizationChanged;
  end;
end;

procedure TcxGridBand.SetWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(bcSize);
  end;
end;

procedure TcxGridBand.ReadHidden(Reader: TReader);
begin
  Hidden := Reader.ReadBoolean;
end;

procedure TcxGridBand.AddBand(ABand: TcxGridBand);
var
  APrevIsBottom{, APrevIsVisibleBottom}: Boolean;
begin
  APrevIsBottom := IsBottom;
  //APrevIsVisibleBottom := IsVisibleBottom;
  SaveColumnsVisibles;
  ABand.FParentBand := Self;
  FChildBands.Add(ABand);
  RefreshVisibleChildBandsList;
  if APrevIsBottom then
  begin
    Bands.RefreshBottomItemsList;
    MoveColumns(FirstChildBottomBand);
  end
  {else
    if APrevIsVisibleBottom and not IsVisibleBottom then
      MoveColumns(FirstVisibleChildBottomBand)};
  ABand.FixedKind := FFixedKind;
end;

procedure TcxGridBand.RemoveBand(ABand: TcxGridBand);
begin
  ABand.FParentBand := nil;
  FChildBands.Remove(ABand);
  RefreshVisibleChildBandsList;
  if IsBottom then
    Bands.RefreshBottomItemsList;
end;

procedure TcxGridBand.RefreshVisibleChildBandsList;
var
  I: Integer;
  ABand: TcxGridBand;
begin
  FVisibleChildBands.Clear;
  for I := 0 to ChildBandCount - 1 do
  begin
    ABand := ChildBands[I];
    if ABand.Visible then FVisibleChildBands.Add(ABand);
  end;
end;

procedure TcxGridBand.AddColumn(AColumn: TcxGridBandedColumn);
begin
  FColumns.Add(AColumn);
  AColumn.Position.SetBand(Self);
  AColumn.Position.RowIndex := 0;
end;

procedure TcxGridBand.RemoveColumn(AColumn: TcxGridBandedColumn);
begin
  MoveColumn(AColumn, -1, -1);
  AColumn.Position.SetBand(nil);
  FColumns.Remove(AColumn);
end;

procedure TcxGridBand.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('Hidden', ReadHidden, nil, True);
end;

function TcxGridBand.GetObjectName: string;
begin
  Result := 'Band' + IntToStr(ID{Index});
end;

function TcxGridBand.GetProperties(AProperties: TStrings): Boolean;
begin
  with AProperties do
  begin
    Add('Width');
    Add('Visible');
    Add('Index');
    Add('BandIndex');
    Add('ColIndex');
  end;
  if Assigned(FOnGetStoredProperties) then
    FOnGetStoredProperties(Self, AProperties);
  Result := True;
end;

procedure TcxGridBand.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Width' then
    AValue := Width
  else
    if AName = 'Visible' then
      AValue := Visible
    else
      if AName = 'Index' then
        AValue := Index
      else
        if AName = 'BandIndex' then
          AValue := FPosition.BandIndex
        else
          if AName = 'ColIndex' then
            AValue := FPosition.ColIndex
          else
            if Assigned(FOnGetStoredPropertyValue) then
              FOnGetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxGridBand.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Width' then
    Width := AValue
  else
    if AName = 'Visible' then
      Visible := AValue
    else
      if AName = 'Index' then
        FLoadedIndex := AValue
      else
        if AName = 'BandIndex' then
          FPosition.FBandIndex := AValue
        else
          if AName = 'ColIndex' then
            FPosition.FColIndex := AValue
          else
            if Assigned(FOnSetStoredPropertyValue) then
              FOnSetStoredPropertyValue(Self, AName, AValue);
end;

procedure TcxGridBand.ApplyLoadedIndex;
begin
  if (FLoadedIndex >= 0) and (Index <> FLoadedIndex) then
    Index := FLoadedIndex;
end;

procedure TcxGridBand.AssignChildBandWidths;
begin
  GridView.ViewInfo.HeaderViewInfo.BandsViewInfo[VisibleIndex].AssignChildBandWidths;
end;

procedure TcxGridBand.AssignColumnWidths;
begin
  GridView.ViewInfo.HeaderViewInfo.BandsViewInfo[VisibleIndex].AssignColumnWidths;
end;

function TcxGridBand.CanHide: Boolean;
begin
  Result := (Bands.VisibleRootItemCount > 1) or not IsRoot;
end;

function TcxGridBand.CanMove: Boolean;
begin
  Result := GridView.IsDesigning or GridView.OptionsCustomize.BandMoving and FOptions.Moving;
end;

function TcxGridBand.CanSize: Boolean;
begin
  Result := not HasFixedWidth and GridView.OptionsCustomize.BandSizing;
end;

procedure TcxGridBand.Changed(AChange: TcxGridBandChange);
begin
  if GridView <> nil then
    GridView.Changed(TcxGridViewChangeKind(AChange));
end;

procedure TcxGridBand.ChangeScale(M, D: Integer);
begin
  if Width > 0 then
    Width := Max(MulDiv(Width, M, D), 1);
end;

function TcxGridBand.DefaultAlternateCaption: string;
begin
  Result := Caption;
end;

procedure TcxGridBand.ForceWidth(Value: Integer);

  procedure AssignBandWidths;

    procedure AssignBandWidths;
    var
      ABand: TcxGridBand;
    begin
      ABand := FParentBand;
      while ABand <> nil do
      begin
        ABand.AssignChildBandWidths;
        ABand := ABand.ParentBand;
      end;
      GridView.ViewInfo.HeaderViewInfo.BandsViewInfo.AssignRootItemWidths;
    end;

  begin
    with GridView do
    begin
      BeginUpdate;
      try
        if HasParentWithAssignedWidth then
          FParentBand.AssignChildBandWidths;
        if OptionsView.ColumnAutoWidth then
          AssignBandWidths;
      finally
        EndUpdate;
      end;
    end;
  end;

begin
  AssignBandWidths;
  GridView.Controller.ForcingWidthBand := Self;
  try
    Width := Value;
  finally
    GridView.Controller.ForcingWidthBand := nil;
  end;
  AssignBandWidths;
  Changed(bcSize);
end;

function TcxGridBand.GetActuallyVisible: Boolean;
begin
  Result := FVisible and ((FParentBand = nil) or FParentBand.ActuallyVisible);
end;

function TcxGridBand.GetDisplayName: string;
begin
  Result := Caption;
  if Result = '' then
    Result := '<Empty caption>';
end;

function TcxGridBand.GetFixed: Boolean;
begin
  with GridView.Controller do
    Result := HasFixedWidth or
      (ForcingWidthBand <> nil) and IsBandFixedDuringSizing(Self) or
      (ForcingWidthItem <> nil) and
      (TcxGridBandedColumn(ForcingWidthItem).Position.Band.ParentBandWithAssignedWidth = Self);
end;

function TcxGridBand.GetParentInParent(ABand: TcxGridBand): TcxGridBand;
var
  AParentBand: TcxGridBand;
  AFound: Boolean;
begin
  AParentBand := Self;
  repeat
    Result := AParentBand;
    AParentBand := AParentBand.ParentBand;
    AFound := AParentBand = ABand;
  until (AParentBand = nil) or AFound;
  if not AFound then Result := nil;
end;

function TcxGridBand.HasFixedWidth: Boolean;
begin
  Result := not FOptions.Sizing;
end;

function TcxGridBand.HasParentWithAssignedWidth: Boolean;
begin
  Result := ParentBandWithAssignedWidth <> nil;
end;

function TcxGridBand.ColIndexOf(ABand: TcxGridBand): Integer;
begin
  Result := FChildBands.IndexOf(ABand);
end;

procedure TcxGridBand.MoveBandsToRoot;
var
  AColIndex, I: Integer;
begin
  if RootParentBand = nil then
    AColIndex := FPosition.ColIndex + 1
  else
    AColIndex := RootParentBand.Position.ColIndex + 1;
  GridView.BeginUpdate;
  try
    for I := ChildBandCount - 1 downto 0 do
      with ChildBands[I].Position do
      begin
        BandIndex := -1;
        ColIndex := AColIndex;
      end;
  finally
    GridView.EndUpdate;
  end;
end;

procedure TcxGridBand.PopulateTabOrderList(AList: TList);
begin
  Rows.PopulateTabOrderList(AList);
end;

procedure TcxGridBand.ResetLoadedIndex;
begin
  FLoadedIndex := -1;
end;

procedure TcxGridBand.SetIndex(Value: Integer);
var
  APrevIndex: Integer;
begin
  APrevIndex := Index;
  inherited;
  if Index <> APrevIndex then
  begin
    if Visible then Bands.RefreshVisibleItemsList;
    Bands.RefreshRootItemsList;
    Bands.RefreshBottomItemsList;
    Changed(bcProperty);
  end;
end;

function TcxGridBand.VisibleColIndexOf(ABand: TcxGridBand): Integer;
begin
  Result := FVisibleChildBands.IndexOf(ABand);
end;

procedure TcxGridBand.VisibleForCustomizationChanged;
begin
  GridView.RefreshCustomizationForm;
  Changed(bcProperty);
end;

procedure TcxGridBand.CheckChildrenVisibles;
var
  I: Integer;
  AChildItem: TObject;
begin
  for I := 0 to ChildItemCount - 1 do
  begin
    AChildItem := ChildItems[I];
    if AChildItem is TcxGridBand then
      TcxGridBand(AChildItem).CheckVisible
    else
      TcxGridBandedColumn(AChildItem).CheckVisible;
  end;
end;

procedure TcxGridBand.CheckVisible;
begin
  if ActuallyVisible <> FSavedVisible then
  begin
    Bands.BandVisibilityChanged(Self, ActuallyVisible);
    CheckChildrenVisibles;
  end;
end;

procedure TcxGridBand.SaveChildrenVisibles;
var
  I: Integer;
  AChildItem: TObject;
begin
  for I := 0 to ChildItemCount - 1 do
  begin
    AChildItem := ChildItems[I];
    if AChildItem is TcxGridBand then
      TcxGridBand(AChildItem).SaveVisible
    else
      TcxGridBandedColumn(AChildItem).SaveVisible;
  end;
end;

procedure TcxGridBand.SaveColumnsVisibles;
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    Columns[I].SaveVisible;
end;

procedure TcxGridBand.SaveVisible;
begin
  FSavedVisible := ActuallyVisible;
  SaveChildrenVisibles;
end;

function TcxGridBand.GetOptionsClass: TcxGridBandOptionsClass;
begin
  Result := TcxGridBandOptions;
end;

function TcxGridBand.GetStylesClass: TcxGridBandStylesClass;
begin
  Result := TcxGridBandStyles;
end;

procedure TcxGridBand.DoHeaderClick;
begin
  if Assigned(FOnHeaderClick) then FOnHeaderClick(Self);
end;

procedure TcxGridBand.Assign(Source: TPersistent);
begin
  if Source is TcxGridBand then
    with TcxGridBand(Source) do
    begin
      Self.AlternateCaption := AlternateCaption;
      Self.HeaderAlignmentHorz := HeaderAlignmentHorz;
      Self.HeaderAlignmentVert := HeaderAlignmentVert;
      Self.Caption := Caption;
      Self.FixedKind := FixedKind;
      Self.Options := Options;
      Self.Position := Position;
      Self.Styles := Styles;
      Self.Tag := Tag;
      Self.Visible := Visible;
      Self.VisibleForCustomization := VisibleForCustomization;
      Self.Width := Width;
      Self.OnHeaderClick := OnHeaderClick;
      Self.OnGetStoredProperties := OnGetStoredProperties;
      Self.OnGetStoredPropertyValue := OnGetStoredPropertyValue;
      Self.OnSetStoredPropertyValue := OnSetStoredPropertyValue;
    end
  else
    inherited;
end;

procedure TcxGridBand.ApplyBestFit(ACheckSizingAbility: Boolean = False;
  AFireEvents: Boolean = False);
var
  I, ABestFitWidth: Integer;
  AViewInfo: TcxGridBandViewInfo;
begin
  if ACheckSizingAbility and not CanSize then Exit;
  Width := 0;
  GridView.BeginBestFitUpdate;
  try
    if IsBottom then
      FRows.ApplyBestFit(ACheckSizingAbility, AFireEvents)
    else
      for I := 0 to VisibleChildBandCount - 1 do
        VisibleChildBands[I].ApplyBestFit(ACheckSizingAbility, AFireEvents);
    if GridView.OptionsView.BandHeaders and ActuallyVisible and GridView.Visible then
    begin
      AViewInfo := GridView.ViewInfo.HeaderViewInfo.BandsViewInfo[VisibleIndex];
      ABestFitWidth := AViewInfo.HeaderViewInfo.GetBestFitWidth;
      if ABestFitWidth > AViewInfo.ContentWidth then
        ForceWidth(ABestFitWidth);
    end;
  finally
    GridView.EndBestFitUpdate;
  end;
  GridView.Controller.DesignerModified;
end;

function TcxGridBand.GetAlternateCaption: string;
begin
  Result := FAlternateCaption;
  if Result = '' then
    Result := DefaultAlternateCaption;
end;

function TcxGridBand.HasAsParent(ABand: TcxGridBand): Boolean;
begin
  Result := GetParentInParent(ABand) <> nil;
end;

procedure TcxGridBand.MoveBand(ABand: TcxGridBand; AColIndex: Integer);
begin
  ABand.Position.BandIndex := Index;
  CheckItemIndexForInsert(AColIndex, FChildBands.Count, True);
  FChildBands.Move(ABand.Position.ColIndex, AColIndex);
  RefreshVisibleChildBandsList;
end;

procedure TcxGridBand.MoveColumn(AColumn: TcxGridBandedColumn; ARowIndex, AColIndex: Integer);
begin
  AColumn.Position.BandIndex := Index;
  FRows.MoveColumn(AColumn, ARowIndex, AColIndex);
end;

procedure TcxGridBand.MoveColumns(ABand: TcxGridBand);
var
  I, J: Integer;
begin
  if ABand = nil then Exit;
  for I := 0 to Rows.Count - 1 do
    for J := 0 to Rows[0].Count - 1 do
      ABand.MoveColumn(Rows[0][0], I, J);
end;

{ TcxGridBands }

constructor TcxGridBands.Create(AGridView: TcxGridBandedTableView);
begin
  inherited Create(AGridView, GetBandClass);
  FGridView := AGridView;
  FBottomItems := TList.Create;
  FRootItems := TList.Create;
  FVisibleBottomItems := TList.Create;
  FVisibleItems := TList.Create;
  FVisibleRootItems := TList.Create;
end;

destructor TcxGridBands.Destroy;
begin
  FreeAndNil(FVisibleRootItems);
  FreeAndNil(FVisibleItems);
  FreeAndNil(FVisibleBottomItems);
  FreeAndNil(FRootItems);
  FreeAndNil(FBottomItems);
  inherited;
end;

function TcxGridBands.GetBottomItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FBottomItems[Index]);
end;

function TcxGridBands.GetBottomItemCount: Integer;
begin
  Result := FBottomItems.Count;
end;

function TcxGridBands.GetFirstVisibleNonEmpty: TcxGridBand;
var
  I: Integer;
begin
  for I := 0 to VisibleBottomItemCount - 1 do
  begin
    Result := VisibleBottomItems[I];
    if Result.Rows.VisibleCount <> 0 then Exit;
  end;
  Result := nil;
end;

function TcxGridBands.GetItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(inherited GetItem(Index));
end;

function TcxGridBands.GetLastVisibleNonEmpty: TcxGridBand;
var
  I: Integer;
begin
  for I := VisibleBottomItemCount - 1 downto 0 do
  begin
    Result := VisibleBottomItems[I];
    if Result.Rows.VisibleCount <> 0 then Exit;
  end;
  Result := nil;
end;

function TcxGridBands.GetLayout: TcxGridBandsLayout;
begin
  if (VisibleLeftFixedCount <> 0) and (VisibleRightFixedCount <> 0) then
    Result := blLeftRightFixed
  else
    if VisibleLeftFixedCount <> 0 then
      Result := blLeftFixed
    else
      if VisibleRightFixedCount <> 0 then
        Result := blRightFixed
      else
        Result := blNonFixed;
end;

function TcxGridBands.GetLineCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to VisibleBottomItemCount - 1 do
    Result := Max(Result, VisibleBottomItems[I].Rows.LineCount);
end;

function TcxGridBands.GetRootItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FRootItems[Index]);
end;

function TcxGridBands.GetRootItemCount: Integer;
begin
  Result := FRootItems.Count;
end;

function TcxGridBands.GetVisibleBottomItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FVisibleBottomItems[Index]);
end;

function TcxGridBands.GetVisibleBottomItemCount: Integer;
begin
  Result := FVisibleBottomItems.Count;
end;

function TcxGridBands.GetVisibleCount: Integer;
begin
  Result := FVisibleItems.Count;
end;

function TcxGridBands.GetVisibleItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FVisibleItems[Index]);
end;

function TcxGridBands.GetVisibleRootItem(Index: Integer): TcxGridBand;
begin
  Result := TcxGridBand(FVisibleRootItems[Index]);
end;

function TcxGridBands.GetVisibleRootItemCount: Integer;
begin
  Result := FVisibleRootItems.Count;
end;

function TcxGridBands.GetVisibleRowCount: Integer;
var
  I, ACount: Integer;
begin
  Result := 0;
  for I := 0 to VisibleBottomItemCount - 1 do
  begin
    ACount := VisibleBottomItems[I].Rows.VisibleCount;
    if ACount > Result then Result := ACount;
  end;
end;

procedure TcxGridBands.SetItem(Index: Integer; Value: TcxGridBand);
begin
  inherited SetItem(Index, Value);
end;

procedure TcxGridBands.AddItem(AItem: TcxGridBand);
begin
  AItem.FID := GetNextID;
  RefreshVisibleItemsList;
  RefreshRootItemsList;
  RefreshBottomItemsList;
  GridView.RefreshCustomizationForm;
  GridView.Synchronize;
end;

procedure TcxGridBands.RemoveItem(AItem: TcxGridBand);
begin
  ReleaseID(AItem.ID);
  RefreshVisibleItemsList;
  RefreshRootItemsList;
  RefreshBottomItemsList;
  GridView.RefreshCustomizationForm;
  GridView.Synchronize;
end;

procedure TcxGridBands.RefreshBottomItemsList;
var
  I: Integer;
  AItem: TcxGridBand;
begin
  if FBottomItems = nil then Exit;
  FBottomItems.Clear;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.IsBottom then FBottomItems.Add(AItem);
  end;
end;

procedure TcxGridBands.RefreshRootItemsList;
var
  I: Integer;
  AItem: TcxGridBand;
begin
  if FRootItems = nil then Exit;
  FRootItems.Clear;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.IsRoot then FRootItems.Add(AItem);
  end;
end;

procedure TcxGridBands.RefreshVisibleBottomItemsList;
var
  I: Integer;

  procedure AddBottomItems(ABand: TcxGridBand);
  var
    I: Integer;
  begin
    if ABand.IsVisibleBottom then
      FVisibleBottomItems.Add(ABand)
    else
      for I := 0 to ABand.VisibleChildBandCount - 1 do
        AddBottomItems(ABand.VisibleChildBands[I]);
  end;

begin
  if FVisibleBottomItems = nil then Exit;
  FVisibleBottomItems.Clear;
  for I := 0 to VisibleRootItemCount - 1 do
    AddBottomItems(VisibleRootItems[I]);
end;

procedure TcxGridBands.RefreshVisibleItemsList;
var
  I: Integer;
begin
  if FVisibleItems = nil then Exit;
  FVisibleItems.Clear;
  FVisibleLeftFixedCount := 0;
  FVisibleRightFixedCount := 0;
  for I := 0 to Count - 1 do
    if Items[I].ActuallyVisible then
    begin
      FVisibleItems.Add(Items[I]);
      case Items[I].FixedKind of
        fkNone:
          FVisibleItems.Move(VisibleCount - 1, VisibleCount - 1 - FVisibleRightFixedCount);
        fkLeft:
          begin
            FVisibleItems.Move(VisibleCount - 1, FVisibleLeftFixedCount);
            Inc(FVisibleLeftFixedCount);
          end;
        fkRight:
          Inc(FVisibleRightFixedCount);
      end;
    end;
  RefreshVisibleRootItemsList;
  RefreshVisibleBottomItemsList;
  GridView.RefreshVisibleItemsList;
  GridView.SizeChanged;
end;

procedure TcxGridBands.RefreshVisibleRootItemsList;
var
  I: Integer;
  AItem: TcxGridBand;
begin
  if FVisibleRootItems = nil then Exit;
  FVisibleRootItems.Clear;
  FVisibleRootLeftFixedCount := 0;
  FVisibleRootRightFixedCount := 0;
  for I := 0 to VisibleCount - 1 do
  begin
    AItem := VisibleItems[I];
    if AItem.IsRoot then
    begin
      FVisibleRootItems.Add(AItem);
      case AItem.FixedKind of
        fkLeft:
          Inc(FVisibleRootLeftFixedCount);
        fkRight:
          Inc(FVisibleRootRightFixedCount);
      end;
    end;
  end;
end;

function TcxGridBands.GetObjectName: string;
begin
  Result := 'Bands';
end;

function TcxGridBands.GetProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
end;

procedure TcxGridBands.GetPropertyValue(const AName: string; var AValue: Variant);
begin
end;

procedure TcxGridBands.SetPropertyValue(const AName: string; const AValue: Variant);
begin
end;

function TcxGridBands.CreateChild(const AObjectName, AClassName: string): TObject;
begin
  if AClassName = GetBandClass.ClassName then
    Result := Add
  else
    Result := nil;
  GridView.InitChildComponent(Result, AObjectName);
end;

procedure TcxGridBands.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

procedure TcxGridBands.GetChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    AChildren.AddObject('', Items[I]);
end;

procedure TcxGridBands.BandVisibilityChanged(ABand: TcxGridBand; Value: Boolean);
begin
  // do nothing
end;

procedure TcxGridBands.ChangeScale(M, D: Integer);
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

function TcxGridBands.GetBandClass: TcxGridBandClass;
begin
  Result := TcxGridBand;
end;

function TcxGridBands.GetNextID: Integer;
begin
  Result := FNextID;
  Inc(FNextID);
end;

procedure TcxGridBands.ReleaseID(AID: Integer);
begin
  if AID = FNextID - 1 then Dec(FNextID);
end;

procedure TcxGridBands.PopulateTabOrderList(AList: TList);
var
  I: Integer;
begin
  for I := 0 to VisibleBottomItemCount - 1 do
    VisibleBottomItems[I].PopulateTabOrderList(AList);
end;

function TcxGridBands.Add: TcxGridBand;
begin
  Result := TcxGridBand(inherited Add);
end;

function TcxGridBands.AreNested: Boolean;
begin
  Result := Count <> BottomItemCount;
end;

procedure TcxGridBands.Assign(Source: TPersistent);
var
  I: Integer;
  ASource: TcxGridBands;
  AItem: TcxGridBand;
begin
  ASource := Source as TcxGridBands;
  GridView.BeginAssignBands;
  try
    for I := 0 to ASource.Count - 1 do
    begin
      AItem := FindItemID(ASource[I].ID);
      if AItem = nil then
      begin
        AItem := Add;
        AItem.FID := ASource[I].ID;
      end;
      AItem.Index := I;
      AItem.Assign(ASource[I]);
    end;
    for I := Count - 1 downto ASource.Count do
      Delete(I);
  finally
    GridView.EndAssignBands;
  end;
  FNextID := ASource.NextID;
end;

procedure TcxGridBands.Clear;
begin
  inherited;
  FNextID := 0;
end;

function TcxGridBands.FindItemID(AID: Integer): TcxGridBand;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if Result.ID = AID then Exit;
  end;
  Result := nil;
end;

function TcxGridBands.GetFirstVisibleIndex(AFixedKind: TcxGridBandFixedKind): Integer;
begin
  case AFixedKind of
    fkLeft:
      Result := 0;
    fkNone:
      Result := VisibleLeftFixedCount;
    fkRight:
      Result := VisibleCount - VisibleRightFixedCount;
  else
    Result := -1;
  end;
end;

function TcxGridBands.GetFirstVisibleRootIndex(AFixedKind: TcxGridBandFixedKind): Integer;
begin
  case AFixedKind of
    fkLeft:
      Result := 0;
    fkNone:
      Result := VisibleRootLeftFixedCount;
    fkRight:
      Result := VisibleRootItemCount - VisibleRootRightFixedCount;
  else
    Result := -1;
  end;
end;

function TcxGridBands.GetLastVisibleIndex(AFixedKind: TcxGridBandFixedKind): Integer;
begin
  case AFixedKind of
    fkLeft:
      Result := GetFirstVisibleIndex(fkNone) - 1;
    fkNone:
      Result := GetFirstVisibleIndex(fkRight) - 1;
    fkRight:
      Result := VisibleCount - 1;
  else
    Result := -1;
  end;
end;

function TcxGridBands.GetLastVisibleRootIndex(AFixedKind: TcxGridBandFixedKind): Integer;
begin
  case AFixedKind of
    fkLeft:
      Result := GetFirstVisibleRootIndex(fkNone) - 1;
    fkNone:
      Result := GetFirstVisibleRootIndex(fkRight) - 1;
    fkRight:
      Result := VisibleRootItemCount - 1;
  else
    Result := -1;
  end;
end;

function TcxGridBands.HaveFixedItems: Boolean;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].FixedKind <> fkNone;
    if Result then Exit;
  end;
  Result := False;
end;

{ TcxGridBandedTableBackgroundBitmaps }

function TcxGridBandedTableBackgroundBitmaps.GetBitmapStyleIndex(Index: Integer): Integer;
begin
  case Index of
    bbBandBackground:
      Result := vsBandBackground;
    bbBandHeader:
      Result := vsBandHeader;
  else
    Result := inherited GetBitmapStyleIndex(Index);
  end;
end;

procedure TcxGridBandedTableBackgroundBitmaps.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedTableBackgroundBitmaps then
    with TcxGridBandedTableBackgroundBitmaps(Source) do
    begin
      Self.BandBackground := BandBackground;
      Self.BandHeader := BandHeader;
    end;
  inherited;
end;

{ TcxGridBandedTableOptionsBehavior }

constructor TcxGridBandedTableOptionsBehavior.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FBandHeaderHints := True;
end;

procedure TcxGridBandedTableOptionsBehavior.SetBandHeaderHints(Value: Boolean);
begin
  if FBandHeaderHints <> Value then
  begin
    FBandHeaderHints := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsBehavior.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedTableOptionsBehavior then
    with TcxGridBandedTableOptionsBehavior(Source) do
      Self.BandHeaderHints := BandHeaderHints;
  inherited;
end;

{ TcxGridBandedTableOptionsCustomize }

constructor TcxGridBandedTableOptionsCustomize.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FBandMoving := True;
  FBandSizing := True;
  FColumnVertSizing := True;
  FNestedBands := True;
  FBandsQuickCustomizationShowCommands := True;
  FBandsQuickCustomizationMultiColumnMode := True;
end;

function TcxGridBandedTableOptionsCustomize.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandHiding(Value: Boolean);
begin
  if FBandHiding <> Value then
  begin
    FBandHiding := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandMoving(Value: Boolean);
begin
  if FBandMoving <> Value then
  begin
    FBandMoving := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandSizing(Value: Boolean);
begin
  if FBandSizing <> Value then
  begin
    FBandSizing := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandsQuickCustomization(Value: Boolean);
begin
  if FBandsQuickCustomization <> Value then
  begin
    FBandsQuickCustomization := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandsQuickCustomizationMaxDropDownCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBandsQuickCustomizationMaxDropDownCount <> Value then
  begin
    FBandsQuickCustomizationMaxDropDownCount := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandsQuickCustomizationMultiColumnMode(Value: Boolean);
begin
  if FBandsQuickCustomizationMultiColumnMode <> Value then
  begin
    FBandsQuickCustomizationMultiColumnMode := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetBandsQuickCustomizationReordering(Value: TcxGridQuickCustomizationReordering);
begin
  if FBandsQuickCustomizationReordering <> Value then
  begin
    FBandsQuickCustomizationReordering := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetColumnVertSizing(Value: Boolean);
begin
  if FColumnVertSizing <> Value then
  begin
    FColumnVertSizing := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.SetNestedBands(Value: Boolean);
begin
  if FNestedBands <> Value then
  begin
    FNestedBands := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableOptionsCustomize.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedTableOptionsCustomize then
    with TcxGridBandedTableOptionsCustomize(Source) do
    begin
      Self.BandHiding := BandHiding;
      Self.BandMoving := BandMoving;
      Self.BandSizing := BandSizing;
      Self.BandsQuickCustomization := BandsQuickCustomization;
      Self.BandsQuickCustomizationMaxDropDownCount := BandsQuickCustomizationMaxDropDownCount;
      Self.BandsQuickCustomizationMultiColumnMode := BandsQuickCustomizationMultiColumnMode;
      Self.BandsQuickCustomizationReordering := BandsQuickCustomizationReordering;
      Self.BandsQuickCustomizationShowCommands := BandsQuickCustomizationShowCommands;
      Self.BandsQuickCustomizationSorted := BandsQuickCustomizationSorted;
      Self.ColumnVertSizing := ColumnVertSizing;
      Self.NestedBands := NestedBands;
    end;
  inherited;
end;

function TcxGridBandedTableOptionsCustomize.SupportsBandsQuickCustomizationReordering: Boolean;
begin
  Result := not GridView.Bands.AreNested and not GridView.Bands.HaveFixedItems and
    (not BandsQuickCustomizationSorted and
     (GridView.IsDesigning or
     (BandsQuickCustomizationReordering = qcrEnabled) or
     (BandsQuickCustomizationReordering = qcrDefault) and BandMoving));
end;

function TcxGridBandedTableOptionsCustomize.SupportsItemsQuickCustomizationReordering: Boolean;
begin
  Result := False;
end;

{ TcxGridBandedTableOptionsView }

constructor TcxGridBandedTableOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FBandHeaderLineCount := 1;
  FBandHeaders := True;
  FFixedBandSeparatorColor := clDefault;
  FFixedBandSeparatorWidth := cxGridDefaultFixedBandSeparatorWidth;
end;

procedure TcxGridBandedTableOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  FixedBandSeparatorWidth := MulDiv(FixedBandSeparatorWidth, M, D);
  if BandHeaderHeight > 0 then
    BandHeaderHeight := Max(MulDiv(BandHeaderHeight, M, D), 1);
end;

function TcxGridBandedTableOptionsView.GetGridView: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableOptionsView.SetBandCaptionsInColumnAlternateCaption(Value: Boolean);
begin
  if FBandCaptionsInColumnAlternateCaption <> Value then
  begin
    FBandCaptionsInColumnAlternateCaption := Value;
    GridView.ItemCaptionChanged(nil);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetBandHeaderEndEllipsis(Value: Boolean);
begin
  if FBandHeaderEndEllipsis <> Value then
  begin
    FBandHeaderEndEllipsis := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetBandHeaderHeight(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FBandHeaderHeight <> Value then
  begin
    FBandHeaderHeight := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetBandHeaderLineCount(Value: Integer);
begin
  if Value < 1 then Value := 1;
  if FBandHeaderLineCount <> Value then
  begin
    FBandHeaderLineCount := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetBandHeaders(Value: Boolean);
begin
  if FBandHeaders <> Value then
  begin
    FBandHeaders := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetFixedBandSeparatorColor(Value: TColor);
begin
  if FFixedBandSeparatorColor <> Value then
  begin
    FFixedBandSeparatorColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridBandedTableOptionsView.SetFixedBandSeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FFixedBandSeparatorWidth <> Value then
  begin
    FFixedBandSeparatorWidth := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridBandedTableOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxGridBandedTableOptionsView then
    with TcxGridBandedTableOptionsView(Source) do
    begin
      Self.BandCaptionsInColumnAlternateCaption := BandCaptionsInColumnAlternateCaption;
      Self.BandHeaderEndEllipsis := BandHeaderEndEllipsis;
      Self.BandHeaderHeight := BandHeaderHeight;
      Self.BandHeaderLineCount := BandHeaderLineCount;
      Self.BandHeaders := BandHeaders;
      Self.FixedBandSeparatorColor := FixedBandSeparatorColor;
      Self.FixedBandSeparatorWidth := FixedBandSeparatorWidth;
    end;
  inherited;
end;

function TcxGridBandedTableOptionsView.GetFixedBandSeparatorColor: TColor;
begin
  Result := FFixedBandSeparatorColor;
  if Result = clDefault then
    Result := LookAndFeelPainter.DefaultFixedSeparatorColor;
end;

{ TcxGridBandedTableViewStyles }

function TcxGridBandedTableViewStyles.GetGridViewValue: TcxGridBandedTableView;
begin
  Result := TcxGridBandedTableView(inherited GridView);
end;

procedure TcxGridBandedTableViewStyles.SetOnGetBandHeaderStyle(Value: TcxGridBandGetHeaderStyle);
begin
  if not dxSameMethods(FOnGetBandHeaderStyle, Value) then
  begin
    FOnGetBandHeaderStyle := Value;
    GridView.Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableViewStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  inherited;
  with AParams, LookAndFeelPainter do
    case Index of
      vsBandBackground:
        begin
          Color := DefaultHeaderBackgroundColor;
          TextColor := DefaultHeaderBackgroundTextColor;
        end;
      vsBandHeader:
        begin
          Color := DefaultHeaderColor;
          TextColor := DefaultHeaderTextColor;
        end;
    end;
end;

procedure TcxGridBandedTableViewStyles.Assign(Source: TPersistent);
begin
  inherited;
  if Source is TcxGridBandedTableViewStyles then
    with TcxGridBandedTableViewStyles(Source) do
    begin
      Self.BandBackground := BandBackground;
      Self.BandHeader := BandHeader;
      Self.OnGetBandHeaderStyle := OnGetBandHeaderStyle;
    end;
end;

procedure TcxGridBandedTableViewStyles.GetBandHeaderParams(ABand: TcxGridBand;
  out AParams: TcxViewParams);
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetBandHeaderStyle) then
    FOnGetBandHeaderStyle(GridView, ABand, AStyle);
  GetViewParams(vsBandHeader, ABand, AStyle, AParams);
end;

{ TcxGridBandedTableViewStyleSheet }

function TcxGridBandedTableViewStyleSheet.GetStylesValue: TcxGridBandedTableViewStyles;
begin
  Result := TcxGridBandedTableViewStyles(GetStyles);
end;

procedure TcxGridBandedTableViewStyleSheet.SetStylesValue(Value: TcxGridBandedTableViewStyles);
begin
  SetStyles(Value);
end;

class function TcxGridBandedTableViewStyleSheet.GetStylesClass: TcxCustomStylesClass;
begin
  Result := TcxGridBandedTableViewStyles;
end;

{ TcxGridBandedTableView }

function TcxGridBandedTableView.GetBackgroundBitmaps: TcxGridBandedTableBackgroundBitmaps;
begin
  Result := TcxGridBandedTableBackgroundBitmaps(inherited BackgroundBitmaps);
end;

function TcxGridBandedTableView.GetColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited Columns[Index]);
end;

function TcxGridBandedTableView.GetController: TcxGridBandedTableController;
begin
  Result := TcxGridBandedTableController(inherited Controller);
end;

function TcxGridBandedTableView.GetGroupedColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited GroupedColumns[Index]);
end;

function TcxGridBandedTableView.GetOptionsBehavior: TcxGridBandedTableOptionsBehavior;
begin
  Result := TcxGridBandedTableOptionsBehavior(inherited OptionsBehavior);
end;

function TcxGridBandedTableView.GetOptionsCustomize: TcxGridBandedTableOptionsCustomize;
begin
  Result := TcxGridBandedTableOptionsCustomize(inherited OptionsCustomize);
end;

function TcxGridBandedTableView.GetOptionsView: TcxGridBandedTableOptionsView;
begin
  Result := TcxGridBandedTableOptionsView(inherited OptionsView);
end;

function TcxGridBandedTableView.GetStyles: TcxGridBandedTableViewStyles;
begin
  Result := TcxGridBandedTableViewStyles(inherited Styles);
end;

function TcxGridBandedTableView.GetViewInfo: TcxGridBandedTableViewInfo;
begin
  Result := TcxGridBandedTableViewInfo(inherited ViewInfo);
end;

function TcxGridBandedTableView.GetVisibleColumn(Index: Integer): TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited VisibleColumns[Index]);
end;

procedure TcxGridBandedTableView.SetBackgroundBitmaps(Value: TcxGridBandedTableBackgroundBitmaps);
begin
  inherited BackgroundBitmaps := Value;
end;

procedure TcxGridBandedTableView.SetBands(Value: TcxGridBands);
begin
  FBands.Assign(Value);
end;

procedure TcxGridBandedTableView.SetColumn(Index: Integer;
  Value: TcxGridBandedColumn);
begin
  inherited Columns[Index] := Value;
end;

procedure TcxGridBandedTableView.SetOnBandPosChanged(Value: TcxGridBandEvent);
begin
  if not dxSameMethods(FOnBandPosChanged, Value) then
  begin
    FOnBandPosChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableView.SetOnBandSizeChanged(Value: TcxGridBandEvent);
begin
  if not dxSameMethods(FOnBandSizeChanged, Value) then
  begin
    FOnBandSizeChanged := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableView.SetOnCustomDrawBandHeader(Value: TcxGridBandCustomDrawHeaderEvent);
begin
  if not dxSameMethods(FOnCustomDrawBandHeader, Value) then
  begin
    FOnCustomDrawBandHeader := Value;
    Changed(vcProperty);
  end;
end;

procedure TcxGridBandedTableView.SetOptionsBehavior(Value: TcxGridBandedTableOptionsBehavior);
begin
  inherited OptionsBehavior := Value;
end;

procedure TcxGridBandedTableView.SetOptionsCustomize(Value: TcxGridBandedTableOptionsCustomize);
begin
  inherited OptionsCustomize := Value;
end;

procedure TcxGridBandedTableView.SetOptionsView(Value: TcxGridBandedTableOptionsView);
begin
  inherited OptionsView := Value;
end;

procedure TcxGridBandedTableView.SetStyles(Value: TcxGridBandedTableViewStyles);
begin
  inherited Styles := Value;
end;

function TcxGridBandedTableView.GetProperties(AProperties: TStrings): Boolean;
begin
  AProperties.Add('BandsQuickCustomizationSorted');
  Result := inherited GetProperties(AProperties);
end;

procedure TcxGridBandedTableView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'BandsQuickCustomizationSorted' then
    AValue := OptionsCustomize.BandsQuickCustomizationSorted
  else
    inherited;
end;

procedure TcxGridBandedTableView.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'BandsQuickCustomizationSorted' then
    OptionsCustomize.BandsQuickCustomizationSorted := AValue
  else
    inherited;
end;

procedure TcxGridBandedTableView.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  if AObject <> Bands then inherited;
end;

procedure TcxGridBandedTableView.GetStoredChildren(AChildren: TStringList);
begin
  AChildren.AddObject('', Bands);
  inherited;
end;

procedure TcxGridBandedTableView.AssignLayout(ALayoutView: TcxCustomGridView);
begin
  Bands := (ALayoutView as TcxGridBandedTableView).Bands;
  inherited;
end;

procedure TcxGridBandedTableView.GetAdornerTargetElements(AList: TStrings);
begin
  inherited GetAdornerTargetElements(AList);
  ViewInfo.HeaderViewInfo.BandsViewInfo.AddAdornerTargetElements(AList);
end;

procedure TcxGridBandedTableView.ReadState(Reader: TReader);
begin
  //if csAncestor in ComponentState then  - does not exist in run-time
    SaveColumnsParams(True);
  inherited;
  //if csAncestor in ComponentState then
    AssignColumnsParams(True, False);
end;

procedure TcxGridBandedTableView.Updated;
begin
  inherited;
  BeginUpdate;
  try
    AssignBandsParams;
    AssignColumnsParams(True, True);
  finally
    EndUpdate;
  end;
end;

procedure TcxGridBandedTableView.Updating;
begin
  SaveBandsParams;
  SaveColumnsParams;
  inherited;
end;

procedure TcxGridBandedTableView.BeginAssignBands;
begin
  FIsAssigningBands := True;
end;

function TcxGridBandedTableView.CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta: Integer): Boolean;
begin
  Result := inherited CanOffset(ARecordCountDelta, APixelScrollRecordOffsetDelta) and
    ((ARecordCountDelta <> 0) or (APixelScrollRecordOffsetDelta <> 0) or (Bands.Layout = blNonFixed));
end;

procedure TcxGridBandedTableView.DoAssign(ASource: TcxCustomGridView);
begin
  if ASource is TcxGridBandedTableView then
    with TcxGridBandedTableView(ASource) do
    begin
      if not Self.AssigningSettings then
        Self.Bands := Bands;
      Self.OnBandPosChanged := OnBandPosChanged;
      Self.OnBandSizeChanged := OnBandSizeChanged;
      Self.OnCustomDrawBandHeader := OnCustomDrawBandHeader;
    end;
  inherited;
end;

procedure TcxGridBandedTableView.DoItemsAssigned;
begin
  inherited;
  AssignColumnsParams(False, True);
end;

procedure TcxGridBandedTableView.EndAssignBands;
begin
  FIsAssigningBands := False;
  AssignBandsParams;
end;

procedure TcxGridBandedTableView.GetFakeComponentLinks(AList: TList);
var
  I: Integer;
begin
  inherited;
  for I := 0 to FBands.Count - 1 do
    FBands[I].Styles.GetFakeComponentLinks(AList);
end;

function TcxGridBandedTableView.GetIsControlFocused: Boolean;
begin
  Result := inherited GetIsControlFocused or
    Controller.HasBandsCustomizationPopup and Controller.BandsCustomizationPopup.Visible;
end;

function TcxGridBandedTableView.HasCustomDrawBandHeader: Boolean;
begin
  Result := Assigned(FOnCustomDrawBandHeader);
end;

procedure TcxGridBandedTableView.ChangeScale(M, D: Integer);
begin
  inherited;
  FBands.ChangeScale(M, D);
end;

procedure TcxGridBandedTableView.CreateOptions;
begin
  inherited;
  FBands := GetBandsClass.Create(Self);
end;

procedure TcxGridBandedTableView.DestroyOptions;
begin
  FreeAndNil(FBands);
  inherited;
end;

procedure TcxGridBandedTableView.AddItem(AItem: TcxCustomGridTableItem);
begin
  inherited;
  if not IsLoading and (FBands.VisibleBottomItemCount <> 0) then
    TcxGridBandedColumn(AItem).Position.BandIndex := FBands.VisibleBottomItems[0].Index;
end;

function CompareLoadedBandPositions(
  Item1, Item2: Pointer): Integer;
var
  APosition1, APosition2: TcxGridBandPosition;
begin
  APosition1 := TcxGridBandPosition(Item1);
  APosition2 := TcxGridBandPosition(Item2);
  Result := APosition1.FBandIndex - APosition2.FBandIndex;
  if Result = 0 then
    Result := APosition1.FColIndex - APosition2.FColIndex;
end;

function CompareLoadedBandIndexes(
  Item1, Item2: Pointer): Integer;
begin
  Result := TcxGridBand(Item1).FLoadedIndex - TcxGridBand(Item2).FLoadedIndex;
end;

procedure TcxGridBandedTableView.AssignBandsParams;
var
  ABandPositions: TList;
  I: Integer;

  procedure GetSortedBandPositionList(AList: TList);
  var
    I: Integer;
  begin
    AList.Count := FBands.Count;
    for I := 0 to FBands.Count - 1 do
      AList[I] := FBands[I].Position;
    AList.Sort(CompareLoadedBandPositions);
  end;

  procedure AssignBandIndexes;
  var
    I: Integer;
    ABandIndexes: TList;
  begin
    ABandIndexes := TList.Create;
    try
      for I := 0 to Bands.Count - 1 do
        ABandIndexes.Add(Bands[I]);
      ABandIndexes.Sort(CompareLoadedBandIndexes);
      for I := 0 to ABandIndexes.Count - 1 do
        TcxGridBand(ABandIndexes[I]).ApplyLoadedIndex;
    finally
      ABandIndexes.Free;
    end;
  end;

begin
  BeginUpdate;
  try
    ABandPositions := TList.Create;
    try
      AssignBandIndexes;
      GetSortedBandPositionList(ABandPositions);
      for I := 0 to ABandPositions.Count - 1 do
        with TcxGridBandPosition(ABandPositions[I]) do
        begin
          Band.IgnoreLoadingStatus := True;
          try
            BandIndex := FBandIndex;
            if not Band.IsRoot then
              ColIndex := FColIndex;
          finally
            Band.IgnoreLoadingStatus := False;
          end;
        end;
    finally
      ABandPositions.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function CompareLoadedColumnPositions(Item1, Item2:
  Pointer): Integer;
var
  APosition1, APosition2: TcxGridBandedColumnPosition;
begin
  APosition1 := TcxGridBandedColumnPosition(Item1);
  APosition2 := TcxGridBandedColumnPosition(Item2);
  Result := APosition1.BandIndex - APosition2.BandIndex;
  if Result = 0 then
  begin
    Result := APosition1.FRowIndex - APosition2.FRowIndex;
    if Result = 0 then
      Result := APosition1.FColIndex - APosition2.FColIndex;
  end;
end;

procedure TcxGridBandedTableView.AssignColumnsParams(AAssignBandIndexes, AAssignOtherIndexes: Boolean);
var
  AColumnPositions: TList;
  I: Integer;

  procedure GetSortedColumnPositionList(AList: TList);
  var
    I: Integer;
  begin
    AList.Count := ColumnCount;
    for I := 0 to ColumnCount - 1 do
      AList[I] := Columns[I].Position;
    AList.Sort(CompareLoadedColumnPositions);
  end;

begin
  BeginUpdate;
  try
    if AAssignBandIndexes then
      for I := 0 to ColumnCount - 1 do
        with Columns[I].Position do
          BandIndex := FBandIndex;
    if AAssignOtherIndexes then
    begin
      AColumnPositions := TList.Create;
      try
        GetSortedColumnPositionList(AColumnPositions);
        for I := 0 to AColumnPositions.Count - 1 do
          with TcxGridBandedColumnPosition(AColumnPositions[I]) do
          begin
            Item.IgnoreLoadingStatus := True;
            try
              RowIndex := FRowIndex;
              ColIndex := FColIndex;
            finally
              Item.IgnoreLoadingStatus := False;
            end;
          end;
      finally
        AColumnPositions.Free;
      end;
    end;
    RefreshVisibleItemsList;
  finally
    EndUpdate;
  end;
end;

procedure TcxGridBandedTableView.BeforeRestoring;
var
  I: Integer;
begin
  inherited BeforeRestoring;
  for I := 0 to Bands.Count - 1 do
    Bands[I].ResetLoadedIndex;
  for I := 0 to ColumnCount - 1 do
    Columns[I].Position.SaveParams(True);
end;

procedure TcxGridBandedTableView.GetVisibleItemsList(AItems: TList);
var
  I: Integer;

  procedure ProcessBand(ABand: TcxGridBand);
  var
    I: Integer;
  begin
    if ABand.Rows.VisibleCount <> 0 then
      with ABand.Rows.VisibleItems[0].VisibleItemsList do
        for I := 0 to Count - 1 do
          AItems.Add(Items[I]);
  end;

begin
  for I := 0 to FBands.VisibleBottomItemCount - 1 do
    ProcessBand(FBands.VisibleBottomItems[I]);
end;

procedure TcxGridBandedTableView.LoadingComplete;
begin
  inherited;
  BeginUpdate;
  try
    AssignBandsParams;
    AssignColumnsParams(False, True);
  finally
    EndUpdate;
  end;
end;

procedure TcxGridBandedTableView.PopulateTabOrderList(AList: TList);
begin
  Bands.PopulateTabOrderList(AList);
end;

function CompareColumns(Item1, Item2:
  Pointer): Integer;
var
  AColumn1, AColumn2: TcxGridBandedColumn;
  AFixedKind1, AFixedKind2: TcxGridBandFixedKind;
begin
  AColumn1 := TcxGridBandedColumn(Item1);
  AColumn2 := TcxGridBandedColumn(Item2);
  AFixedKind1 := AColumn1.Position.Band.FixedKind;
  AFixedKind2 := AColumn2.Position.Band.FixedKind;

  if AFixedKind1 = AFixedKind2 then
    Result := CompareVisibleColumnPositions(AColumn1.Position, AColumn2.Position)
  else
    if AFixedKind1 = fkLeft then
      Result := -1
    else
      if AFixedKind2 = fkLeft then
        Result := 1
      else
        Result := Ord(AFixedKind1) - Ord(AFixedKind2);
end;

procedure TcxGridBandedTableView.RefreshVisibleItemsList;
begin
  inherited;
  VisibleItemsList.Sort(CompareColumns);
  AssignVisibleItemsIndexes;
end;

procedure TcxGridBandedTableView.RestoringComplete;
begin
  inherited;
  BeginUpdate;
  try
    AssignBandsParams;
    AssignColumnsParams(True, True);
  finally
    EndUpdate;
  end;
end;

procedure TcxGridBandedTableView.SaveBandsParams;
var
  I: Integer;
begin
  for I := 0 to FBands.Count - 1 do
    FBands[I].Position.SaveParams;
end;

procedure TcxGridBandedTableView.SaveColumnsParams(ABandIndexOnly: Boolean = False);
var
  I: Integer;
begin
  for I := 0 to ColumnCount - 1 do
    Columns[I].Position.SaveParams(ABandIndexOnly);
end;

function TcxGridBandedTableView.GetBackgroundBitmapsClass: TcxCustomGridBackgroundBitmapsClass;
begin
  Result := TcxGridBandedTableBackgroundBitmaps;
end;

function TcxGridBandedTableView.GetBandsClass: TcxGridBandsClass;
begin
  Result := TcxGridBands;
end;

function TcxGridBandedTableView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridBandedTableController;
end;

function TcxGridBandedTableView.GetItemClass: TcxCustomGridTableItemClass;
begin
  Result := TcxGridBandedColumn;
end;

function TcxGridBandedTableView.GetOptionsBehaviorClass: TcxCustomGridOptionsBehaviorClass;
begin
  Result := TcxGridBandedTableOptionsBehavior;
end;

function TcxGridBandedTableView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxGridBandedTableOptionsCustomize;
end;

function TcxGridBandedTableView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridBandedTableOptionsView;
end;

function TcxGridBandedTableView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridBandedTablePainter;
end;

function TcxGridBandedTableView.GetStylesClass: TcxCustomGridViewStylesClass;
begin
  Result := TcxGridBandedTableViewStyles;
end;

function TcxGridBandedTableView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridBandedTableViewInfo;
end;

procedure TcxGridBandedTableView.DoBandPosChanged(ABand: TcxGridBand);
begin
  if Assigned(FOnBandPosChanged) then FOnBandPosChanged(Self, ABand);
end;

procedure TcxGridBandedTableView.DoBandSizeChanged(ABand: TcxGridBand);
begin
  if Assigned(FOnBandSizeChanged) then FOnBandSizeChanged(Self, ABand);
end;

procedure TcxGridBandedTableView.DoCustomDrawBandHeader(ACanvas: TcxCanvas;
  AViewInfo: TcxGridBandHeaderViewInfo; var ADone: Boolean);
begin
  if HasCustomDrawBandHeader then
    FOnCustomDrawBandHeader(Self, ACanvas, AViewInfo, ADone);
end;

procedure TcxGridBandedTableView.Initialize;
begin
  inherited Initialize;
  if (Owner <> nil) and ([csDesigning, csReading] * Owner.ComponentState = [csDesigning]) then
    Bands.Add;
end;

procedure TcxGridBandedTableView.ApplyBestFit(AItem: TcxCustomGridTableItem = nil;
  ACheckSizingAbility: Boolean = False; AFireEvents: Boolean = False);
var
  I: Integer;
begin
  if AItem = nil then
  begin
    BeginBestFitUpdate;
    try
      for I := 0 to FBands.VisibleRootItemCount - 1 do
        FBands.VisibleRootItems[I].ApplyBestFit(ACheckSizingAbility, AFireEvents);
    finally
      EndBestFitUpdate;
    end;
  end
  else
    inherited;
end;

function TcxGridBandedTableView.CreateColumn: TcxGridBandedColumn;
begin
  Result := TcxGridBandedColumn(inherited CreateColumn);
end;

initialization
  cxGridRegisteredViews.Register(TcxGridBandedTableView, 'Banded Table');
  Classes.RegisterClasses([TcxGridBandedColumn, TcxGridBandedTableViewStyleSheet]);

finalization
  cxGridRegisteredViews.Unregister(TcxGridBandedTableView);

end.

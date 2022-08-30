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

unit cxGridCustomLayoutView;

{$I cxVer.inc}

interface

uses
  Types, Classes, Controls, Graphics, Windows, Forms, StdCtrls, Contnrs, dxFilterPopupWindow,
  dxCoreClasses, cxClasses, cxGeometry, cxCustomData, cxLookAndFeelPainters, cxControls, dxFilterValueContainer,
  cxGridCustomView, cxGridCustomTableView, cxGridViewLayoutContainer, dxUIElementPopupWindow;

const
  cxGridCustomLayoutViewRecordDefaultIndent = 7;
  cxGridCustomLayoutViewSeparatorDefaultColor = clDefault;
  cxGridCustomLayoutViewSeparatorDefaultWidth = 2;
  cxGridCustomLayoutViewDefaultRecordCaptionSeparator = ':';

type
  TcxGridCustomLayoutView = class;
  TcxGridCustomLayoutRecord = class;

  TcxGridCustomLayoutViewBands = class;

  TcxGridCustomLayoutViewController = class;
  TcxGridCustomLayoutViewControllerClass = class of TcxGridCustomLayoutViewController;

  TcxGridCustomLayoutViewViewInfo = class;
  TcxGridCustomLayoutRecordsViewInfo = class;
  TcxGridCustomLayoutRecordViewInfo = class;
  TcxGridCustomLayoutRecordViewInfoClass = class of TcxGridCustomLayoutRecordViewInfo;
  TcxGridCustomLayoutViewSeparatorsViewInfo = class;
  TcxGridCustomLayoutViewSeparatorsViewInfoClass = class of TcxGridCustomLayoutViewSeparatorsViewInfo;

  { TcxGridCustomLayoutViewBand }

  TcxGridCustomLayoutViewBand = class
  private
    FIndex: Integer;
    FItems: TList;
    function GetLastVisible: TcxGridCustomLayoutRecordViewInfo;
    function GetItem(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
    function GetCount: Integer;
    function GetFirstVisible: TcxGridCustomLayoutRecordViewInfo;
    function GetVisibleCount: Integer;
    procedure SetItem(Index: Integer; Value: TcxGridCustomLayoutRecordViewInfo);
  public
    constructor Create(AIndex: Integer);
    destructor Destroy; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function GetNearest(APos: Integer): TcxGridCustomLayoutRecordViewInfo;
    function IsItemVisible(Index: Integer): Boolean;

    property Index: Integer read FIndex;
    property LastVisible: TcxGridCustomLayoutRecordViewInfo read GetLastVisible;
    property Count: Integer read GetCount;
    property FirstVisible: TcxGridCustomLayoutRecordViewInfo read GetFirstVisible;
    property Items[Index: Integer]: TcxGridCustomLayoutRecordViewInfo read GetItem write SetItem; default;
    property VisibleCount: Integer read GetVisibleCount;
  end;

  TcxGridCustomLayoutViewBandClass = class of TcxGridCustomLayoutViewBand;

  { TcxGridCustomLayoutViewBands }

  TcxGridCustomLayoutViewBands = class
  private
    FRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
    FItems: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxGridCustomLayoutViewBand;
    function GetLastVisible: TcxGridCustomLayoutViewBand;
    function GetVisibleCount: Integer;
  protected
    function GetBandClass: TcxGridCustomLayoutViewBandClass; virtual;
    property RecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo read FRecordsViewInfo;

    procedure CreateItems;
    procedure DestroyItems;
  public
    constructor Create(ARecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo);
    destructor Destroy; override;

    procedure Clear;
    procedure DoRightToLeftConversion(const ABounds: TRect); virtual;
    function IsItemVisible(AColumn, ARow: Integer): Boolean;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxGridCustomLayoutViewBand read GetItem; default;
    property LastVisible: TcxGridCustomLayoutViewBand read GetLastVisible;
    property VisibleCount: Integer read GetVisibleCount;
  end;
  TcxGridCustomLayoutViewBandsClass = class of TcxGridCustomLayoutViewBands;

  { View Data }

  TcxGridCustomLayoutViewViewData = class(TcxCustomGridTableViewData)
  private
    function GetRecord(Index: Integer): TcxGridCustomLayoutRecord;
  protected
    function GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass; override;
  public
    property Records[Index: Integer]: TcxGridCustomLayoutRecord read GetRecord;
  end;

  { Controllers }

  TcxGridCustomLayoutViewControllerHelper = class
  private
    FController: TcxGridCustomLayoutViewController;
    function GetControl: TcxControl;
    function GetDataController: TcxCustomDataController;
    function GetScrollBarOffsetBegin: Integer;
    function GetScrollBarOffsetEnd: Integer;
    function GetViewData: TcxGridCustomLayoutViewViewData;
    function GetViewInfo: TcxGridCustomLayoutViewViewInfo;
  protected
    function CanScrollBarVisible(AKind: TScrollBarKind): Boolean; virtual;
    function GetPageRecordCount(AVisibleRecordCount: Integer): Integer; virtual;
    procedure DoInitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean); virtual;
    procedure InitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean);
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; virtual;

    property Control: TcxControl read GetControl;
    property Controller: TcxGridCustomLayoutViewController read FController;
    property DataController: TcxCustomDataController read GetDataController;
    property ScrollBarOffsetBegin: Integer read GetScrollBarOffsetBegin;
    property ScrollBarOffsetEnd: Integer read GetScrollBarOffsetEnd;
    property ViewData: TcxGridCustomLayoutViewViewData read GetViewData;
    property ViewInfo: TcxGridCustomLayoutViewViewInfo read GetViewInfo;
  public
    constructor Create(AController: TcxGridCustomLayoutViewController); virtual;
    function GetIsRecordsScrollHorizontal: Boolean; virtual;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; virtual; abstract;
    function GetScrollDelta: Integer; virtual; abstract;
    procedure InitScrollBarsParameters;
    procedure KeyDown(var Key: Word; Shift: TShiftState); dynamic;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); virtual;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); virtual;
  end;

  TcxGridCustomLayoutViewControllerHelperClass = class of TcxGridCustomLayoutViewControllerHelper;

  TcxGridCustomLayoutViewControllerHorizontalHelper = class(TcxGridCustomLayoutViewControllerHelper)
  private
    procedure FocusRecordInNextColumn(ADirection: TcxDirection; AIsSelecting: Boolean);
  protected
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; override;
  public
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollDelta: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TcxGridCustomLayoutViewControllerVerticalHelper = class(TcxGridCustomLayoutViewControllerHelper)
  private
    procedure FocusRecordInNextRow(ADirection: TcxDirection; AIsSelecting: Boolean);
  protected
    function IsDataScrollBar(AKind: TScrollBarKind): Boolean; override;
  public
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollDelta: Integer; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  TcxGridCustomLayoutViewController = class(TcxGridCustomLayoutController)
  private
    FHelper: TcxGridCustomLayoutViewControllerHelper;
    FScrollRecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
    function GetFocusedRecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
    function GetViewData: TcxGridCustomLayoutViewViewData;
    function GetViewInfo: TcxGridCustomLayoutViewViewInfo;
    procedure SetScrollRecordViewInfo(Value: TcxGridCustomLayoutRecordViewInfo);
  protected
    //scrolling
    procedure DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    function GetDragScrollInterval: Integer; override;
    function GetIsRecordsScrollHorizontal: Boolean; override;
    function GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind; override;
    function GetScrollDelta: Integer; override;
    function GetPageRecordCount: Integer; override;
    procedure ScrollContentScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;
    procedure ScrollData(ADirection: TcxDirection); override;
    procedure ScrollDataScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer); virtual;

    procedure CreateHelper; virtual;
    procedure DestroyHelper; virtual;
    function GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass; virtual;
    procedure InternalKeyDown(var Key: Word; Shift: TShiftState); virtual;
    function ProcessFocusedItemKeys(var AKey: Word; AShift: TShiftState; ARowGoOnCycle: Boolean): Boolean; virtual;

    property FocusedRecordViewInfo: TcxGridCustomLayoutRecordViewInfo read GetFocusedRecordViewInfo;
    property Helper: TcxGridCustomLayoutViewControllerHelper read FHelper;
    property ScrollRecordViewInfo: TcxGridCustomLayoutRecordViewInfo read FScrollRecordViewInfo write SetScrollRecordViewInfo;
    property ViewData: TcxGridCustomLayoutViewViewData read GetViewData;
    property ViewInfo: TcxGridCustomLayoutViewViewInfo read GetViewInfo;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    destructor Destroy; override;

    function IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MakeItemVisible(AItem: TcxCustomGridTableItem); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure InitScrollBarsParameters; override;

    function FocusNextItemHorizontally(AGoForward, AGoOnCycle: Boolean): Boolean; virtual;
    function FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean; virtual;

    function CanScrollFocusedRecord(ADown: Boolean): Boolean; virtual;
    function ScrollFocusedRecord(ADown: Boolean): Boolean; virtual;

    procedure LayoutDirectionChanged;
  end;

  { Painters }

  { TcxGridCustomLayoutViewPainter }

  TcxGridCustomLayoutViewPainter = class(TcxCustomGridTablePainter)
  private
    function GetViewInfo: TcxGridCustomLayoutViewViewInfo;
  protected
    procedure DrawBackground; override;
    procedure DrawSeparators; virtual;
    procedure PaintContent; override;
  public
    property ViewInfo: TcxGridCustomLayoutViewViewInfo read GetViewInfo;
  end;

  { Cache }

  TcxGridCustomLayoutViewInfoCacheItem = class(TcxCustomGridTableViewInfoCacheItem);

  { ViewInfos }

  { TcxGridCustomLayoutViewItemFilterButtonViewInfo }

  TcxGridCustomLayoutViewItemFilterButtonViewInfo = class(TcxCustomGridViewCellViewInfo,
    IdxFilterPopupWindowOwner)
  private
    function GetActive: Boolean;
    function GetDropDownWindowValue: TdxFilterPopupWindow;
    function GetGridView: TcxGridCustomLayoutView;
  protected
    { IdxFilterPopupWindowOwner }
    function IdxFilterPopupWindowOwner.GetLinkComponent = GetFilterDropDownWindowLinkComponent;
    function IdxFilterPopupWindowOwner.GetMode = GetFilterPopupMode;
    function IdxFilterPopupWindowOwner.GetOptions = GetFilterDropDownWindowOptions;
    function GetFilterDropDownWindowLinkComponent: TComponent; virtual;
    function GetFilterPopupMode: TdxFilterPopupWindowMode; virtual;
    function GetFilterDropDownWindowOptions: TObject; virtual;

    function CalculateHeight: Integer; override;
    function CalculateWidth: Integer; override;
    function GetItem: TcxCustomGridTableItem; virtual; abstract;
    function GetVisibleForHitTest: Boolean; override;
    function GetWidth: Integer; override;
    function EmulateMouseMoveAfterCalculate: Boolean; override;
    function HasInitialHitTest: Boolean;
    function NeedsContainerHotTrack: Boolean; virtual;
    function OccupiesSpace: Boolean; virtual;

    function CloseDropDownWindowOnDestruction: Boolean; override;
    function DropDownWindowExists: Boolean; override;
    function GetDropDownWindow: TdxUIElementPopupWindow; override;
    function GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz; override;
    function IsDropDownWindowOwner: Boolean; override;

    property DropDownWindow: TdxFilterPopupWindow read GetDropDownWindowValue;
  public
    property Active: Boolean read GetActive;

    property GridView: TcxGridCustomLayoutView read GetGridView;
  end;

  { TcxGridCustomLayoutRecordViewInfo }

  TcxGridCustomLayoutRecordViewInfo = class(TcxCustomGridRecordViewInfo)
  private
    FCol: Integer;
    FRow: Integer;
    FCalculationPosition: TPoint;
    function GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
  protected
    function GetVisible: Boolean; override;
    function GetPixelScrollSize: Integer; override;
    property CalculationPosition: TPoint read FCalculationPosition write FCalculationPosition;
  public
    class function GetExpandButtonSize(APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer; virtual;
    function IsFullyVisible: Boolean;

    property RecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo read GetRecordsViewInfo;
    property Col: Integer read FCol;
    property Row: Integer read FRow;
  end;

  { TcxGridCustomLayoutViewSeparatorsViewInfo }

  TcxGridCustomLayoutViewSeparatorsViewInfo = class
  private
    FGridViewInfo: TcxGridCustomLayoutViewViewInfo;
    FItems: TList;
    function GetCount: Integer;
    function GetGridView: TcxGridCustomLayoutView;
    function GetItem(Index: Integer): TRect;
    function GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
    procedure SetItem(Index: Integer; const Value: TRect);
    procedure CreateItems;
    procedure DestroyItems;
  protected
    procedure Add(const R: TRect);
    procedure DoCalculate; virtual;
    function GetBounds: TRect; virtual;
    function GetColor: TColor; virtual;
    function GetWidth: Integer; virtual;

    property Bounds: TRect read GetBounds;
    property GridView: TcxGridCustomLayoutView read GetGridView;
    property GridViewInfo: TcxGridCustomLayoutViewViewInfo read FGridViewInfo;
    property RecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo read GetRecordsViewInfo;
  public
    constructor Create(AGridViewInfo: TcxGridCustomLayoutViewViewInfo); virtual;
    destructor Destroy; override;

    procedure Calculate;
    procedure Clear;

    property Color: TColor read GetColor;
    property Count: Integer read GetCount;
    property Items[Index: Integer]: TRect read GetItem write SetItem; default;
    property Width: Integer read GetWidth;
  end;

  { calculators }

  TcxGridCustomLayoutRecordsViewInfoBasedCalculator = class
  private
    FRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
    function GetRecord(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
  protected
    procedure AdjustContentBounds(var ABounds: TRect); virtual;
    procedure AfterCalculate; virtual;
    procedure BeforeCalculate; virtual;
    procedure Calculate(const AContentBounds: TRect);
    procedure DoCalculate(const AContentBounds: TRect); virtual;

    function GetRecordIndent: Integer;
    function GetRecordSpaceHorz: Integer; virtual;
    function GetRecordSpaceVert: Integer; virtual;
    function GetSeparatorWidth: Integer;

    function GetMaxCount: Integer;

    function GetMaxColumnCount: Integer; virtual;
    function GetMaxRowCount: Integer; virtual;

    property Records[Index: Integer]: TcxGridCustomLayoutRecordViewInfo read GetRecord;
  public
    constructor Create(ARecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo); virtual;

    property RecordSpaceHorz: Integer read GetRecordSpaceHorz;
    property RecordSpaceVert: Integer read GetRecordSpaceVert;
    property RecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo read FRecordsViewInfo;
  end;

  TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass = class of TcxGridCustomLayoutRecordsViewInfoBasedCalculator;

  TcxGridCustomLayoutViewInfoHorizontalCalculator = class(TcxGridCustomLayoutRecordsViewInfoBasedCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    procedure DoCalculate(const AContentBounds: TRect); override;
    function GetRecordSpaceHorz: Integer; override;
    function GetRecordSpaceVert: Integer; override;
  end;

  TcxGridCustomLayoutViewInfoVerticalCalculator = class(TcxGridCustomLayoutRecordsViewInfoBasedCalculator)
  protected
    procedure AdjustContentBounds(var ABounds: TRect); override;
    procedure DoCalculate(const AContentBounds: TRect); override;
    function GetRecordSpaceHorz: Integer; override;
    function GetRecordSpaceVert: Integer; override;
  end;

  { TcxGridCustomLayoutRecordsViewInfo }

  TcxGridCustomLayoutRecordsViewInfo = class(TcxCustomGridRecordsViewInfo)
  private
    FBands: TcxGridCustomLayoutViewBands;
    FCalculator: TcxGridCustomLayoutRecordsViewInfoBasedCalculator;
    FMaxRecordHeight: Integer;
    FRecordHeight: Integer;
    FRecordSpaceHorz: Integer;
    FRecordSpaceVert: Integer;
    FRecordWidth: Integer;
    function IsVertical: Boolean;
    function GetItem(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
    function GetGridView: TcxGridCustomLayoutView;
    function GetGridViewInfo: TcxGridCustomLayoutViewViewInfo;
  protected
    procedure Calculate; override;
    function CalculateContentBounds: TRect; override;
    function GetPixelScrollContentSize: Integer; override;

    function GetItemLeftBound(AIndex: Integer): Integer; override;
    function GetItemsOffset(AItemCountDelta: Integer): Integer; override;
    function GetItemTopBound(AIndex: Integer): Integer; override;
    procedure OffsetItem(AIndex, AOffset: Integer); override;

    function CalculateHeight(var AFullyVisible: Boolean): Integer; virtual;
    function CalculateMaxRecordHeight: Integer; virtual;
    function CalculateRecordHeight: Integer; virtual;
    function CalculateRecordWidth: Integer; virtual;
    function CalculateWidth(AMaxWidth: Integer): Integer; virtual;

    function GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass; virtual;
    function GetBandsClass: TcxGridCustomLayoutViewBandsClass; virtual;
    function GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass; virtual;

    function GetMaxRecordWidth(AColumn: Integer): Integer; virtual;
    function GetMaxRecordHeight(ARow: Integer): Integer; virtual;

    function GetRecordIndent: Integer; virtual;
    function GetSeparatorWidth: Integer; virtual;

    property Calculator: TcxGridCustomLayoutRecordsViewInfoBasedCalculator read FCalculator;
    property RecordIndent: Integer read GetRecordIndent;
    property RecordSpaceHorz: Integer read FRecordSpaceHorz;
    property RecordSpaceVert: Integer read FRecordSpaceVert;
  public
    constructor Create(AGridViewInfo: TcxCustomGridTableViewInfo); override;
    destructor Destroy; override;

    function CanOffset(AItemCountDelta: Integer): Boolean; override;
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    function GetRealItem(ARecord: TcxCustomGridRecord): TcxGridCustomLayoutRecordViewInfo; reintroduce; virtual;

    property Bands: TcxGridCustomLayoutViewBands read FBands;
    property GridView: TcxGridCustomLayoutView read GetGridView;
    property GridViewInfo: TcxGridCustomLayoutViewViewInfo read GetGridViewInfo;
    property Items[Index: Integer]: TcxGridCustomLayoutRecordViewInfo read GetItem; default;
    property MaxRecordHeight: Integer read FMaxRecordHeight;
    property RecordHeight: Integer read FRecordHeight;
    property RecordWidth: Integer read FRecordWidth;
  end;

  { TcxGridCustomLayoutViewViewInfo }

  TcxGridCustomLayoutViewViewInfo = class(TcxCustomGridTableViewInfo)
  private
    FPrevRecordHeight: Integer;
    FSeparatorsViewInfo: TcxGridCustomLayoutViewSeparatorsViewInfo;
    function GetController: TcxGridCustomLayoutViewController;
    function GetGridView: TcxGridCustomLayoutView;
    function GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
  protected
    procedure AfterCalculating; override;
    procedure BeforeCalculating; override;
    procedure Calculate; override;
    procedure CalculateHeight(const AMaxSize: TPoint; var AHeight: Integer;
      var AFullyVisible: Boolean); override;
    procedure CalculateWidth(const AMaxSize: TPoint; var AWidth: Integer); override;
    procedure CreateViewInfos; override;
    procedure DestroyViewInfos(AIsRecreating: Boolean); override;

    function GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass; virtual;
  public
    procedure DoRightToLeftConversion(const ABounds: TRect); override;
    property Controller: TcxGridCustomLayoutViewController read GetController;
    property GridView: TcxGridCustomLayoutView read GetGridView;
    property RecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo read GetRecordsViewInfo;
    property SeparatorsViewInfo: TcxGridCustomLayoutViewSeparatorsViewInfo read FSeparatorsViewInfo;
  end;

  { TcxGridCustomLayoutRecord }

  TcxGridCustomLayoutRecord = class(TcxCustomGridRecord)
  private
    FExpanded: Boolean;
    function GetGridView: TcxGridCustomLayoutView;
    procedure SetExpanded(Value: Boolean);
  protected
    procedure DoCollapse(ARecurse: Boolean); override;
    procedure DoExpand(ARecurse: Boolean); override;
    function GetExpandable: Boolean; override;
    function GetExpanded: Boolean; override;
    function GetHasCells: Boolean; override;
    function GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass; override;
    function GetViewInfoClass: TcxCustomGridRecordViewInfoClass; override;

    property Expanded: Boolean read GetExpanded write SetExpanded;
  public
    constructor Create(AViewData: TcxCustomGridTableViewData; AIndex: Integer; const ARecordInfo: TcxRowInfo); override;
    property GridView: TcxGridCustomLayoutView read GetGridView;
  end;

  { TcxGridCustomLayoutViewOptionsCustomize }

  TcxGridCustomLayoutViewOptionsCustomize = class(TcxCustomGridTableOptionsCustomize)
  private
    FExpandable: Boolean;
    function GetGridView: TcxGridCustomLayoutView;
    procedure SetExpandable(Value: Boolean);
  protected
    property Expandable: Boolean read FExpandable write SetExpandable default False;
  public
    procedure Assign(Source: TPersistent); override;

    property GridView: TcxGridCustomLayoutView read GetGridView;
  end;

  { TcxGridCustomLayoutViewOptionsView }

  TcxGridCustomLayoutViewOptionsView = class(TcxCustomGridTableOptionsView)
  private
    FCaptionSeparator: Char;
    FIndent: Integer;
    FSeparatorColor: TColor;
    FSeparatorWidth: Integer;
    procedure SetCaptionSeparator(Value: Char);
    procedure SetIndent(Value: Integer);
    procedure SetSeparatorColor(Value: TColor);
    procedure SetSeparatorWidth(Value: Integer);
  protected
    procedure ChangeScale(M, D: Integer); override;

    property CaptionSeparator: Char read FCaptionSeparator write SetCaptionSeparator default cxGridCustomLayoutViewDefaultRecordCaptionSeparator;
    property Indent: Integer read FIndent write SetIndent default cxGridCustomLayoutViewRecordDefaultIndent;
    property SeparatorColor: TColor read FSeparatorColor write SetSeparatorColor default cxGridCustomLayoutViewSeparatorDefaultColor;
    property SeparatorWidth: Integer read FSeparatorWidth write SetSeparatorWidth default cxGridCustomLayoutViewSeparatorDefaultWidth;
  public
    constructor Create(AGridView: TcxCustomGridView); override;
    procedure Assign(Source: TPersistent); override;

    function GetSeparatorColor: TColor;
  end;

  { TcxGridCustomLayoutView }

  TcxGridCustomLayoutView = class(TcxCustomGridTableView)
  private
    function GetOptionsCustomize: TcxGridCustomLayoutViewOptionsCustomize; inline;
    function GetOptionsView: TcxGridCustomLayoutViewOptionsView; inline;
    function GetViewInfo: TcxGridCustomLayoutViewViewInfo; inline;
    procedure SetOptionsCustomize(Value: TcxGridCustomLayoutViewOptionsCustomize); inline;
    procedure SetOptionsView(Value: TcxGridCustomLayoutViewOptionsView); inline;
  protected
    function GetControllerClass: TcxCustomGridControllerClass; override;
    function GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass; override;
    function GetOptionsViewClass: TcxCustomGridOptionsViewClass; override;
    function GetPainterClass: TcxCustomGridPainterClass; override;
    function GetViewDataClass: TcxCustomGridViewDataClass; override;
    function GetViewInfoClass: TcxCustomGridViewInfoClass; override;

    procedure Reposition;
  public
    property OptionsCustomize: TcxGridCustomLayoutViewOptionsCustomize read GetOptionsCustomize write SetOptionsCustomize;
    property OptionsView: TcxGridCustomLayoutViewOptionsView read GetOptionsView write SetOptionsView;
    property ViewInfo: TcxGridCustomLayoutViewViewInfo read GetViewInfo;
  end;

implementation

uses
  Math, SysUtils, cxGridCommon, cxLookAndFeels;

{ TcxGridCustomLayoutViewColumn }

constructor TcxGridCustomLayoutViewBand.Create(AIndex: Integer);
begin
  inherited Create;
  FIndex := AIndex;
  FItems := TList.Create;
end;

destructor TcxGridCustomLayoutViewBand.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TcxGridCustomLayoutViewBand.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].RightToLeftConversion(ABounds);
end;

function TcxGridCustomLayoutViewBand.GetNearest(APos: Integer): TcxGridCustomLayoutRecordViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if APos < Result.Bounds.Bottom then Break;
  end;
end;

function TcxGridCustomLayoutViewBand.IsItemVisible(Index: Integer): Boolean;
begin
  Result := (Index < Count) and (Items[Index] <> nil) and Items[Index].Visible;
end;

function TcxGridCustomLayoutViewBand.GetLastVisible: TcxGridCustomLayoutRecordViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if IsItemVisible(I) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxGridCustomLayoutViewBand.GetItem(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
begin
  Result := TcxGridCustomLayoutRecordViewInfo(FItems[Index]);
end;

function TcxGridCustomLayoutViewBand.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridCustomLayoutViewBand.GetFirstVisible: TcxGridCustomLayoutRecordViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if IsItemVisible(I) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxGridCustomLayoutViewBand.GetVisibleCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Count - 1 do
  begin
    if IsItemVisible(I) then
      Inc(Result);
  end
end;

procedure TcxGridCustomLayoutViewBand.SetItem(Index: Integer; Value: TcxGridCustomLayoutRecordViewInfo);
var
  I: Integer;
begin
  if Index < Count then
    FItems[Index] := Value
  else
  begin
    for I := Count to Index - 1 do
      FItems.Add(nil);
    FItems.Add(Value);
    Value.FCol := Self.Index;
    Value.FRow := Index;
  end
end;

{ TcxGridCustomCardViewColumns }

constructor TcxGridCustomLayoutViewBands.Create(ARecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo);
begin
  inherited Create;
  FRecordsViewInfo := ARecordsViewInfo;
  CreateItems;
end;

destructor TcxGridCustomLayoutViewBands.Destroy;
begin
  DestroyItems;
  inherited;
end;

procedure TcxGridCustomLayoutViewBands.Clear;
begin
  FItems.Clear;
end;

function TcxGridCustomLayoutViewBands.IsItemVisible(AColumn, ARow: Integer): Boolean;
begin
  Result := (AColumn < VisibleCount) and Items[AColumn].IsItemVisible(ARow);
end;

function TcxGridCustomLayoutViewBands.GetBandClass: TcxGridCustomLayoutViewBandClass;
begin
  Result := TcxGridCustomLayoutViewBand;
end;

function TcxGridCustomLayoutViewBands.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridCustomLayoutViewBands.GetItem(Index: Integer): TcxGridCustomLayoutViewBand;
begin
  if Index < Count then
    Result := TcxGridCustomLayoutViewBand(FItems[Index])
  else
  begin
    Result := GetBandClass.Create(Index);
    FItems.Add(Result);
  end;
end;

function TcxGridCustomLayoutViewBands.GetLastVisible: TcxGridCustomLayoutViewBand;
var
  I: Integer;
begin
  Result := nil;
  for I := Count - 1 downto 0 do
    if Items[I].VisibleCount > 0 then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxGridCustomLayoutViewBands.GetVisibleCount: Integer;
begin
  if Count = 0 then
    Result := 0
  else
  begin
    for Result := Count - 1 downto 0 do
      if Items[Result].VisibleCount > 0 then Break;
    Inc(Result);
  end;
end;

procedure TcxGridCustomLayoutViewBands.CreateItems;
begin
  FItems := TObjectList.Create;
end;

procedure TcxGridCustomLayoutViewBands.DestroyItems;
begin
  FItems.Free;
end;

procedure TcxGridCustomLayoutViewBands.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].DoRightToLeftConversion(ABounds);
end;

{ TcxGridCustomLayoutViewViewData }

function TcxGridCustomLayoutViewViewData.GetRecordClass(const ARecordInfo: TcxRowInfo): TcxCustomGridRecordClass;
begin
  Result := TcxGridCustomLayoutRecord;
end;

function TcxGridCustomLayoutViewViewData.GetRecord(Index: Integer): TcxGridCustomLayoutRecord;
begin
  Result := TcxGridCustomLayoutRecord(inherited Records[Index]);
end;

{ TcxGridCustomCardViewControllerImpl }

constructor TcxGridCustomLayoutViewControllerHelper.Create(AController: TcxGridCustomLayoutViewController);
begin
  inherited Create;
  FController := AController;
end;

function TcxGridCustomLayoutViewControllerHelper.GetIsRecordsScrollHorizontal: Boolean;
begin
  Result := IsDataScrollBar(sbHorizontal);
end;

procedure TcxGridCustomLayoutViewControllerHelper.InitScrollBarsParameters;
begin
  InitScrollBarParameters(sbHorizontal, Controller.CanHScrollBarHide);
  InitScrollBarParameters(sbVertical, True);
end;

procedure TcxGridCustomLayoutViewControllerHelper.KeyDown(var Key: Word; Shift: TShiftState);
begin
//do nothing
end;

procedure TcxGridCustomLayoutViewControllerHelper.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//do nothing
end;

procedure TcxGridCustomLayoutViewControllerHelper.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
//do nothing
end;

procedure TcxGridCustomLayoutViewControllerHelper.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
//do nothing
end;

function TcxGridCustomLayoutViewControllerHelper.CanScrollBarVisible(AKind: TScrollBarKind): Boolean;
begin
  Result := IsDataScrollBar(AKind);
end;

function TcxGridCustomLayoutViewControllerHelper.GetPageRecordCount(AVisibleRecordCount: Integer): Integer;
begin
  Result := AVisibleRecordCount;
end;

procedure TcxGridCustomLayoutViewControllerHelper.DoInitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean);
begin
  if IsDataScrollBar(AKind) then
    Controller.SetScrollBarInfo(AKind, 0, Controller.DataScrollSize - 1,
      Controller.ScrollDelta, Controller.VisibleDataScrollSize, Controller.ScrollBarPos, True, ACanHide)
end;

procedure TcxGridCustomLayoutViewControllerHelper.InitScrollBarParameters(AKind: TScrollBarKind; ACanHide: Boolean);
begin
  if CanScrollBarVisible(AKind) then
    DoInitScrollBarParameters(AKind, ACanHide)
  else
    Controller.SetScrollBarInfo(AKind, 0, -1, 0, 0, 0, True, True);
end;

function TcxGridCustomLayoutViewControllerHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

function TcxGridCustomLayoutViewControllerHelper.GetControl: TcxControl;
begin
  Result := Controller.Control;
end;

function TcxGridCustomLayoutViewControllerHelper.GetDataController: TcxCustomDataController;
begin
  Result := Controller.DataController;
end;

function TcxGridCustomLayoutViewControllerHelper.GetScrollBarOffsetBegin: Integer;
begin
  Result := Controller.ScrollBarOffsetBegin;
end;

function TcxGridCustomLayoutViewControllerHelper.GetScrollBarOffsetEnd: Integer;
begin
  Result := Controller.ScrollBarOffsetEnd;
end;

function TcxGridCustomLayoutViewControllerHelper.GetViewData: TcxGridCustomLayoutViewViewData;
begin
  Result := Controller.ViewData;
end;

function TcxGridCustomLayoutViewControllerHelper.GetViewInfo: TcxGridCustomLayoutViewViewInfo;
begin
  Result := Controller.ViewInfo;
end;

{ TcxGridCustomCardViewControllerHorizontalImpl }

function TcxGridCustomLayoutViewControllerHorizontalHelper.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskHorizontal;
end;

function TcxGridCustomLayoutViewControllerHorizontalHelper.GetScrollDelta: Integer;
begin
  if ViewInfo.RecordsViewInfo.Bands.Count = 0 then
    Result := 0
  else
    Result := ViewInfo.RecordsViewInfo.Bands[0].Count;
end;

procedure TcxGridCustomLayoutViewControllerHorizontalHelper.KeyDown(var Key: Word; Shift: TShiftState);
var
  AKey: Word;
begin
  inherited;
  if ViewInfo.UseRightToLeftAlignment then
    AKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key)
  else
    AKey := Key;
  case AKey of
    VK_LEFT:
      FocusRecordInNextColumn(dirLeft, ssShift in Shift);
    VK_RIGHT:
      FocusRecordInNextColumn(dirRight, ssShift in Shift);
    VK_UP:
      Controller.GoToPrev(not (ssShift in Shift), False);
    VK_DOWN:
      Controller.GoToNext(not (ssShift in Shift), False);
  end;
end;

function TcxGridCustomLayoutViewControllerHorizontalHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbHorizontal;
end;

procedure TcxGridCustomLayoutViewControllerHorizontalHelper.FocusRecordInNextColumn(ADirection: TcxDirection; AIsSelecting: Boolean);
var
  ARecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
  ACheckPos: Integer;

  function MarginCol(AIsLeftDirection: Boolean): Integer;
  begin
    if AIsLeftDirection then
      Result := 0
    else
      Result := ViewInfo.RecordsViewInfo.Bands.VisibleCount - 1;
  end;

  function NextCol: Integer;
  begin
    if ARecordViewInfo = nil then
      Result := MarginCol(ADirection = dirRight)
    else
      if ADirection = dirLeft then
        Result := ARecordViewInfo.Col - 1
      else
        Result := ARecordViewInfo.Col + 1;
  end;

  function MarginRecordIndex: Integer;
  begin
    if ADirection = dirLeft then
      Result := 0
    else
      Result := ViewData.RecordCount - 1;
  end;

begin
  with Controller do
    try
      if FocusedRecordIndex = -1 then
      begin
        if ADirection = dirLeft then
          GoToFirst
        else
          GoToLast(False);
        Exit;
      end;
      MakeFocusedRecordVisible;
      ARecordViewInfo := ViewInfo.RecordsViewInfo.GetRealItem(FocusedRecord);
      ACheckPos := ARecordViewInfo.Bounds.Top;
      if ARecordViewInfo.Col = MarginCol(ADirection = dirLeft) then
      begin
        ScrollData(ADirection);
        ARecordViewInfo := ViewInfo.RecordsViewInfo.GetRealItem(FocusedRecord);
        if (ARecordViewInfo <> nil) and
          (ARecordViewInfo.Col = MarginCol(ADirection = dirLeft)) then
        begin
          if ARecordViewInfo.Bounds.Top <> ACheckPos then
            GoToPrev(False)
          else
            if GridView.IsDetail then
              FocusNextRecord(MarginRecordIndex, ADirection = dirRight, False,
                not AIsSelecting, not AIsSelecting);
          Exit;
        end;
      end;
      ARecordViewInfo := ViewInfo.RecordsViewInfo.Bands[NextCol].GetNearest(ACheckPos);
      FocusedRecord := ARecordViewInfo.GridRecord;
    finally
      MakeFocusedItemVisible;
    end;
end;

{ TcxGridCustomCardViewControllerVerticalImpl }

function TcxGridCustomLayoutViewControllerVerticalHelper.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := mwskVertical;
end;

function TcxGridCustomLayoutViewControllerVerticalHelper.GetScrollDelta: Integer;
begin
  Result := ViewInfo.RecordsViewInfo.Bands.Count;
end;

procedure TcxGridCustomLayoutViewControllerVerticalHelper.KeyDown(var Key: Word; Shift: TShiftState);
var
  AKey: Word;
begin
  inherited;
  if ViewInfo.UseRightToLeftAlignment then
    AKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(Key)
  else
    AKey := Key;
  case AKey of
    VK_LEFT:
      begin
        Controller.GoToPrev(not (ssShift in Shift), False);
        Controller.MakeFocusedItemVisible;
      end;
    VK_RIGHT:
      begin
        Controller.GoToNext(not (ssShift in Shift), False);
        Controller.MakeFocusedItemVisible;
      end;
    VK_UP:
      FocusRecordInNextRow(dirUp, ssShift in Shift);
    VK_DOWN:
      FocusRecordInNextRow(dirDown, ssShift in Shift);
  end;
end;

function TcxGridCustomLayoutViewControllerVerticalHelper.IsDataScrollBar(AKind: TScrollBarKind): Boolean;
begin
  Result := AKind = sbVertical;
end;

procedure TcxGridCustomLayoutViewControllerVerticalHelper.FocusRecordInNextRow(ADirection: TcxDirection; AIsSelecting: Boolean);
var
  ANewFocusedRecordIndex: Integer;
  AIsNewFocusedRecordOutOfRange: Boolean;

  function NextRecordIndex: Integer;
  begin
    with Controller do
      if ADirection = dirUp then
        Result := FocusedRecordIndex - ScrollDelta
      else
        Result := FocusedRecordIndex + ScrollDelta;
  end;

  function IsOnBound: Boolean;
  begin
    Result :=
      DataController.IsGridMode and
        (DataController.IsBOF and (ADirection = dirUp) or
         DataController.IsEOF and (ADirection = dirDown)) or
      not DataController.IsGridMode and AIsNewFocusedRecordOutOfRange;
  end;

  function MarginRecordIndex: Integer;
  begin
    if ADirection = dirUp then
      Result := 0
    else
      Result := ViewData.RecordCount - 1;
  end;

begin
  with Controller do
  begin
    if FocusedRecordIndex = -1 then
    begin
      if ADirection = dirUp then
        GoToFirst
      else
        GoToLast(False);
      Exit;
    end;
    MakeFocusedRecordVisible;
    ANewFocusedRecordIndex := NextRecordIndex;
    AIsNewFocusedRecordOutOfRange := (ANewFocusedRecordIndex < 0) or
      (ANewFocusedRecordIndex > ViewData.RecordCount - 1);
    if IsOnBound then
    begin
      if GridView.IsDetail then
        FocusNextRecord(MarginRecordIndex, ADirection = dirDown, False,
          not AIsSelecting, not AIsSelecting);
      Exit;
    end;
    if DataController.IsGridMode and AIsNewFocusedRecordOutOfRange or
      not ViewData.Records[ANewFocusedRecordIndex].Visible then
    begin
      ScrollData(ADirection);
      if AIsNewFocusedRecordOutOfRange then
      begin
        if ANewFocusedRecordIndex < 0 then ANewFocusedRecordIndex := 0;
        if ANewFocusedRecordIndex > ViewData.RecordCount - 1 then
          ANewFocusedRecordIndex := ViewData.RecordCount - 1;
      end;
    end;
    FocusedRecordIndex := ANewFocusedRecordIndex;
  end;
end;

{ TcxGridCustomCardViewController }

constructor TcxGridCustomLayoutViewController.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  CreateHelper;
end;

destructor TcxGridCustomLayoutViewController.Destroy;
begin
  DestroyHelper;
  inherited;
end;

function TcxGridCustomLayoutViewController.IsDataFullyVisible(AIsCallFromMaster: Boolean = False): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to ViewInfo.RecordsViewInfo.PartVisibleCount - 1 do
  begin
    Result := ViewInfo.RecordsViewInfo[I].IsFullyVisible;
    if not Result then Break;
  end;
  Result := Result and inherited IsDataFullyVisible(AIsCallFromMaster);
end;

procedure TcxGridCustomLayoutViewController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  InternalKeyDown(Key, Shift);
end;

procedure TcxGridCustomLayoutViewController.MakeItemVisible(AItem: TcxCustomGridTableItem);
begin
  MakeFocusedRecordVisible;
end;

procedure TcxGridCustomLayoutViewController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseDown(Button, Shift, X, Y);
  Helper.MouseDown(Button, Shift, X, Y);
end;

procedure TcxGridCustomLayoutViewController.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseMove(Shift, X, Y);
  Helper.MouseMove(Shift, X, Y);
end;

procedure TcxGridCustomLayoutViewController.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  Helper.MouseUp(Button, Shift, X, Y);
end;

procedure TcxGridCustomLayoutViewController.InitScrollBarsParameters;
begin
  Helper.InitScrollBarsParameters;
end;

function TcxGridCustomLayoutViewController.FocusNextItemHorizontally(AGoForward, AGoOnCycle: Boolean): Boolean;
begin
  Result := False;
end;

function TcxGridCustomLayoutViewController.FocusNextItemVertically(AGoForward, AGoOnCycle: Boolean): Boolean;
begin
  Result := False;
end;

function TcxGridCustomLayoutViewController.CanScrollFocusedRecord(ADown: Boolean): Boolean;
begin
  Result := False;
end;

function TcxGridCustomLayoutViewController.ScrollFocusedRecord(ADown: Boolean): Boolean;
begin
  Result := False;
end;

procedure TcxGridCustomLayoutViewController.LayoutDirectionChanged;
begin
  DestroyHelper;
  CreateHelper;
end;

procedure TcxGridCustomLayoutViewController.CreateHelper;
begin
  FHelper := GetHelperClass.Create(Self);
end;

procedure TcxGridCustomLayoutViewController.DoScroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  inherited DoScroll(AScrollBarKind, AScrollCode, AScrollPos);
  if Helper.IsDataScrollBar(AScrollBarKind) then
    ScrollDataScrollBar(AScrollCode, AScrollPos)
  else
    ScrollContentScrollBar(AScrollCode, AScrollPos);
end;

function TcxGridCustomLayoutViewController.GetDragScrollInterval: Integer;
begin
  Result := 300;
end;

function TcxGridCustomLayoutViewController.GetIsRecordsScrollHorizontal: Boolean;
begin
  Result := Helper.GetIsRecordsScrollHorizontal;
end;

function TcxGridCustomLayoutViewController.GetMouseWheelScrollingKind: TcxMouseWheelScrollingKind;
begin
  Result := Helper.GetMouseWheelScrollingKind;
end;

function TcxGridCustomLayoutViewController.GetScrollDelta: Integer;
begin
  Result := Helper.GetScrollDelta;
end;

function TcxGridCustomLayoutViewController.GetPageRecordCount: Integer;
begin
  Result := Helper.GetPageRecordCount(inherited GetPageRecordCount);
end;

procedure TcxGridCustomLayoutViewController.ScrollContentScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
//do nothing
end;

procedure TcxGridCustomLayoutViewController.ScrollData(ADirection: TcxDirection);
begin
  if Site.DragAndDropState = ddsNone then
    ScrollRecords(ADirection in [dirRight, dirDown], ScrollDelta);
end;

procedure TcxGridCustomLayoutViewController.ScrollDataScrollBar(AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  case AScrollCode of
    scLineUp:
      ScrollData(dirLeft);
    scLineDown:
      ScrollData(dirRight);
    scPageUp:
      ScrollPage(False);
    scPageDown:
      ScrollPage(True);
    scTrack:
      if not DataController.IsGridMode then
        ScrollBarPos := AScrollPos;
    scPosition:
      if DataController.IsGridMode then
        ScrollBarPos := AScrollPos;
  end;
  AScrollPos := ScrollBarPos;
end;

procedure TcxGridCustomLayoutViewController.DestroyHelper;
begin
  FreeAndNil(FHelper);
end;

function TcxGridCustomLayoutViewController.GetHelperClass: TcxGridCustomLayoutViewControllerHelperClass;
begin
  Result := TcxGridCustomLayoutViewControllerHelper;
end;

procedure TcxGridCustomLayoutViewController.InternalKeyDown(var Key: Word; Shift: TShiftState);
var
  AGridViewLink: TcxObjectLink;
  APrevFocusedRecordIndex: Integer;
begin
  if FocusedRecordIndex <> -1 then
  begin
    if GridView.OptionsSelection.CellSelect and ProcessFocusedItemKeys(Key, Shift, False) then
      Exit;
    if ((Key = VK_UP) or (Key = VK_DOWN)) and ScrollFocusedRecord(Key = VK_DOWN) then
      Exit;
  end;
  APrevFocusedRecordIndex := FocusedRecordIndex;
  case Key of
    VK_HOME:
      GoToFirst(False);
    VK_END:
      GoToLast(False, False);
  end;
  AGridViewLink := cxAddObjectLink(GridView);
  try
    Helper.KeyDown(Key, Shift);
    if AGridViewLink.Ref = nil then
      Exit;
  finally
    cxRemoveObjectLink(AGridViewLink);
  end;
  if (FocusedRecordIndex <> APrevFocusedRecordIndex) and
    (FocusedRecordIndex <> -1) and GridView.OptionsSelection.CellSelect then
    ProcessFocusedItemKeys(Key, Shift, True);
end;

function TcxGridCustomLayoutViewController.ProcessFocusedItemKeys(var AKey: Word; AShift: TShiftState; ARowGoOnCycle: Boolean): Boolean;
var
  ARtlKey: Word;
begin
  if ViewInfo.UseRightToLeftAlignment then
    ARtlKey := TdxRightToLeftLayoutConverter.ConvertVirtualKeyCode(AKey)
  else
    ARtlKey := AKey;
  Result := False;
  if FocusedItem <> nil then
    case ARtlKey of
      VK_LEFT, VK_RIGHT:
        Result := FocusNextItemHorizontally(ARtlKey = VK_RIGHT, ARowGoOnCycle);
      VK_UP, VK_DOWN:
        Result := FocusNextItemVertically(ARtlKey = VK_DOWN, ARowGoOnCycle);
      VK_HOME:
        if AShift = [] then
        begin
          FocusNextItem(-1, True, False, False, True);
          Result := True;
        end;
      VK_END:
        if AShift = [] then
        begin
          FocusNextItem(-1, False, True, False, True);
          Result := True;
        end;
      VK_ESCAPE:
        if GridView.OptionsSelection.InvertSelect and (FocusedItem <> nil) then
        begin
          FocusedItem := nil;
          Result := True;
        end;
    end
  else
    if ARtlKey = VK_F2 then
      Result := FocusNextItem(-1, True, False, False, True);
end;

function TcxGridCustomLayoutViewController.GetFocusedRecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
begin
  if FocusedRecord = nil then
    Result := nil
  else
    Result := TcxGridCustomLayoutRecordViewInfo(FocusedRecord.ViewInfo);
end;

function TcxGridCustomLayoutViewController.GetViewData: TcxGridCustomLayoutViewViewData;
begin
  Result := TcxGridCustomLayoutViewViewData(inherited ViewData);
end;

function TcxGridCustomLayoutViewController.GetViewInfo: TcxGridCustomLayoutViewViewInfo;
begin
  Result := TcxGridCustomLayoutViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridCustomLayoutViewController.SetScrollRecordViewInfo(Value: TcxGridCustomLayoutRecordViewInfo);
var
  AScrollDirection: TcxDirection;
begin
  if FScrollRecordViewInfo <> Value then
  begin
    AScrollDirection := ScrollDirection;
    ScrollDirection := dirNone;
    FScrollRecordViewInfo := Value;
    if FScrollRecordViewInfo <> nil then
      ScrollDirection := AScrollDirection;
  end;
end;

{ TcxGridCustomCardViewPainter }

procedure TcxGridCustomLayoutViewPainter.DrawBackground;
begin
  DrawSeparators;
  inherited;
end;

procedure TcxGridCustomLayoutViewPainter.DrawSeparators;
var
  I: Integer;
  R: TRect;
begin
  Canvas.Brush.Color := ViewInfo.SeparatorsViewInfo.Color;
  for I := 0 to ViewInfo.SeparatorsViewInfo.Count - 1 do
  begin
    R := ViewInfo.SeparatorsViewInfo[I];
    Canvas.FillRect(R);
    Canvas.ExcludeClipRect(R);
  end;
end;

procedure TcxGridCustomLayoutViewPainter.PaintContent;
begin
  DrawFindPanel;
  DrawFilterBar;
  inherited;
end;

function TcxGridCustomLayoutViewPainter.GetViewInfo: TcxGridCustomLayoutViewViewInfo;
begin
  Result := TcxGridCustomLayoutViewViewInfo(inherited ViewInfo);
end;

{ TcxGridCustomLayoutViewItemFilterButtonViewInfo }

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetFilterDropDownWindowLinkComponent: TComponent;
begin
  Result := GetItem;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetFilterPopupMode: TdxFilterPopupWindowMode;
begin
  Result := fpmDefault;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetFilterDropDownWindowOptions: TObject;
begin
  Result := GetItem.Options;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.CalculateHeight: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.CalculateWidth: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.CloseDropDownWindowOnDestruction: Boolean;
begin
  Result := False;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.DropDownWindowExists: Boolean;
begin
  Result := TcxGridCustomLayoutView(GridView).Controller.HasFilterPopup;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetActive: Boolean;
begin
  Result := GetItem.Filtered;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetDropDownWindow: TdxUIElementPopupWindow;
begin
  Result := GridView.Controller.FilterPopup;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetDropDownWindowDefaultAlignHorz: TcxPopupAlignHorz;
begin
  if dxGetFilterPopupActualMode(GetFilterPopupMode) = fpmExcel then
    Result := pahLeft
  else
    Result := inherited GetDropDownWindowDefaultAlignHorz;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetDropDownWindowValue: TdxFilterPopupWindow;
begin
  Result := TdxFilterPopupWindow(inherited DropDownWindow);
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetGridView: TcxGridCustomLayoutView;
begin
  Result := TcxGridCustomLayoutView(inherited GridView);
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetWidth: Integer;
begin
  Result := GridViewInfo.LookAndFeelPainter.ScaledFilterDropDownButtonSize(ScaleFactor).X;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.GetVisibleForHitTest: Boolean;
begin
  Result := HasInitialHitTest or inherited GetVisibleForHitTest;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.EmulateMouseMoveAfterCalculate: Boolean;
begin
  Result := HasInitialHitTest;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.HasInitialHitTest: Boolean;
begin
  Result := PtInRect(Bounds, GridViewInfo.MousePos);
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.IsDropDownWindowOwner: Boolean;
begin
  Result := inherited IsDropDownWindowOwner and (DropDownWindow.LinkComponent = GetFilterDropDownWindowLinkComponent);
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.NeedsContainerHotTrack: Boolean;
begin
  Result := not OccupiesSpace;
end;

function TcxGridCustomLayoutViewItemFilterButtonViewInfo.OccupiesSpace: Boolean;
begin
  Result := (GridView.OptionsView.ShowItemFilterButtons = sfbAlways) or
    (GridView.OptionsView.ShowItemFilterButtons = sfbDefault) and cxIsTouchModeEnabled;
end;

{ TcxGridCustomLayoutRecordViewInfo }

class function TcxGridCustomLayoutRecordViewInfo.GetExpandButtonSize(
  APainter: TcxCustomLookAndFeelPainter; AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := APainter.ScaledGroupExpandButtonSize(AScaleFactor)
end;

function TcxGridCustomLayoutRecordViewInfo.IsFullyVisible: Boolean;
begin
  Result := inherited GetHeight <= RecordsViewInfo.MaxRecordHeight;
end;

function TcxGridCustomLayoutRecordViewInfo.GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
begin
  Result := TcxGridCustomLayoutRecordsViewInfo(inherited RecordsViewInfo);
end;

function TcxGridCustomLayoutRecordViewInfo.GetVisible: Boolean;
begin
  Result := Index < RecordsViewInfo.PartVisibleCount;
end;

function TcxGridCustomLayoutRecordViewInfo.GetPixelScrollSize: Integer;
begin
  if TcxGridCustomLayoutViewController(Controller).GetMouseWheelScrollingKind = mwskVertical then
    Result := RecordsViewInfo.RecordSpaceVert + Height
  else
    Result := RecordsViewInfo.RecordSpaceHorz + Width;
end;

{ TcxGridCustomLayoutViewSeparatorsViewInfo }

constructor TcxGridCustomLayoutViewSeparatorsViewInfo.Create(AGridViewInfo: TcxGridCustomLayoutViewViewInfo);
begin
  inherited Create;
  FGridViewInfo := AGridViewInfo;
  CreateItems;
end;

destructor TcxGridCustomLayoutViewSeparatorsViewInfo.Destroy;
begin
  DestroyItems;
  inherited Destroy;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.Calculate;
begin
  if RecordsViewInfo.Bands.VisibleCount > 0 then
    DoCalculate;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    Dispose(PRect(FItems[I]));
  FItems.Clear;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.DoCalculate;
begin
  Clear;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetGridView: TcxGridCustomLayoutView;
begin
  Result := GridViewInfo.GridView;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetItem(Index: Integer): TRect;
begin
  Result := PRect(FItems[Index])^;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
begin
  Result := FGridViewInfo.RecordsViewInfo;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetWidth: Integer;
begin
  Result := GridView.OptionsView.SeparatorWidth;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.SetItem(Index: Integer; const Value: TRect);
var
  ARect: PRect;
begin
  if Index >= Count then
  begin
    New(ARect);
    FItems.Add(ARect);
  end;
  PRect(FItems[Index])^ := Value;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.CreateItems;
begin
  FItems := TList.Create;
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.DestroyItems;
begin
  Clear;
  FreeAndNil(FItems);
end;

procedure TcxGridCustomLayoutViewSeparatorsViewInfo.Add(const R: TRect);
begin
  Items[Count] := R;
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetBounds: TRect;
begin
  Result := GridViewInfo.ClientBounds;
  InflateRect(Result, -RecordsViewInfo.RecordIndent, -RecordsViewInfo.RecordIndent);
end;

function TcxGridCustomLayoutViewSeparatorsViewInfo.GetColor: TColor;
begin
  Result := GridView.OptionsView.GetSeparatorColor;
end;

{ TcxGridCustomCardsViewInfoBasedCalculator }

constructor TcxGridCustomLayoutRecordsViewInfoBasedCalculator.Create(ARecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo);
begin
  inherited Create;
  FRecordsViewInfo := ARecordsViewInfo;
end;

procedure TcxGridCustomLayoutRecordsViewInfoBasedCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  Inc(ABounds.Left, RecordsViewInfo.RecordIndent);
  Inc(ABounds.Top, RecordsViewInfo.RecordIndent);
end;

procedure TcxGridCustomLayoutRecordsViewInfoBasedCalculator.AfterCalculate;
var
  I: Integer;
begin
  if RecordsViewInfo.GridViewInfo.CalculateDown then
    for I := 0 to RecordsViewInfo.Count - 1 do
      with Records[I] do
        if Visible then
          MainCalculate(CalculationPosition.X, CalculationPosition.Y);
end;

procedure TcxGridCustomLayoutRecordsViewInfoBasedCalculator.BeforeCalculate;
begin
  RecordsViewInfo.Bands.Clear;
  RecordsViewInfo.FVisibleCount := 0;
  RecordsViewInfo.FPartVisibleCount := 0;
end;

procedure TcxGridCustomLayoutRecordsViewInfoBasedCalculator.Calculate(const AContentBounds: TRect);
begin
  BeforeCalculate;
  DoCalculate(AContentBounds);
  AfterCalculate;
end;

procedure TcxGridCustomLayoutRecordsViewInfoBasedCalculator.DoCalculate(const AContentBounds: TRect);
begin
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetRecordIndent: Integer;
begin
  Result := RecordsViewInfo.RecordIndent;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetRecordSpaceHorz: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetRecordSpaceVert: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetSeparatorWidth: Integer;
begin
  Result := RecordsViewInfo.GetSeparatorWidth;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetMaxCount: Integer;
begin
  Result := RecordsViewInfo.MaxCount;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetMaxColumnCount: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetMaxRowCount: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfoBasedCalculator.GetRecord(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
begin
  Result := RecordsViewInfo.Items[Index];
end;

{ TcxGridCustomCardsViewInfoHorizontalCalculator }

procedure TcxGridCustomLayoutViewInfoHorizontalCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited;
  Dec(ABounds.Bottom, RecordsViewInfo.RecordIndent);
end;

procedure TcxGridCustomLayoutViewInfoHorizontalCalculator.DoCalculate(const AContentBounds: TRect);
var
  ALeftBound, ATopBound, ACol, ARow, I: Integer;
  ARecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
  AMaxColumnCount, AMaxRowCount: Integer;
  ANeedBreak: Boolean;

  function IsMaxColumn: Boolean;
  begin
    Result := (AMaxColumnCount > 0) and (ACol = AMaxColumnCount);
  end;

  function IsMaxRow: Boolean;
  begin
    Result := (AMaxRowCount > 0) and (ARow = AMaxRowCount);
  end;

begin
  inherited;
  ACol := 0;
  ARow := 0;
  ALeftBound := AContentBounds.Left + RecordsViewInfo.GridViewInfo.PixelScrollRecordOffset;
  ATopBound := AContentBounds.Top;

  AMaxColumnCount := GetMaxColumnCount;
  AMaxRowCount := GetMaxRowCount;
  ANeedBreak := False;

  for I := 0 to GetMaxCount - 1 do
  begin
    ARecordViewInfo := Records[I];
    if I <> 0 then
    begin
      if (ATopBound + ARecordViewInfo.Height > AContentBounds.Bottom) or IsMaxRow then
      begin
        if ANeedBreak then
          Break;
        Inc(ACol);
        Inc(ALeftBound, RecordsViewInfo.GetMaxRecordWidth(ACol - 1) + GetRecordSpaceHorz);
        ATopBound := AContentBounds.Top;
        ARow := 0;
      end;
    end;

    RecordsViewInfo.Bands[ACol][ARow] := ARecordViewInfo;
    if RecordsViewInfo.GridViewInfo.CalculateDown then
      ARecordViewInfo.CalculationPosition := Point(ALeftBound, ATopBound);

    if not ANeedBreak then
    begin
      ANeedBreak := IsMaxColumn or (ALeftBound + ARecordViewInfo.Width > AContentBounds.Right);
      if ANeedBreak then
        Dec(RecordsViewInfo.FVisibleCount, ARow);
    end;

    if not ANeedBreak or (ACol = 0) then
      Inc(RecordsViewInfo.FVisibleCount);
    if not IsMaxColumn then
      Inc(RecordsViewInfo.FPartVisibleCount);
    Inc(ARow);
    Inc(ATopBound, ARecordViewInfo.Height + GetRecordSpaceVert);
  end;
end;

function TcxGridCustomLayoutViewInfoHorizontalCalculator.GetRecordSpaceHorz: Integer;
begin
  Result := RecordsViewInfo.RecordIndent + GetSeparatorWidth + RecordsViewInfo.RecordIndent;
end;

function TcxGridCustomLayoutViewInfoHorizontalCalculator.GetRecordSpaceVert: Integer;
begin
  Result := RecordsViewInfo.RecordIndent + GetSeparatorWidth + RecordsViewInfo.RecordIndent;
end;

{ TcxGridCustomCardsViewInfoVerticalCalculator }

procedure TcxGridCustomLayoutViewInfoVerticalCalculator.AdjustContentBounds(var ABounds: TRect);
begin
  inherited;
  Dec(ABounds.Right, RecordsViewInfo.RecordIndent);
end;

procedure TcxGridCustomLayoutViewInfoVerticalCalculator.DoCalculate(const AContentBounds: TRect);
var
  ALeftBound, ATopBound, ARowHeight, ACol, ARow, I: Integer;
  ARecordViewInfo: TcxGridCustomLayoutRecordViewInfo;
  AMaxColumnCount, AMaxRowCount: Integer;
  ANeedBreak: Boolean;
begin
  inherited;
  ACol := 0;
  ARow := 0;
  ALeftBound := AContentBounds.Left;
  ATopBound := AContentBounds.Top + RecordsViewInfo.GridViewInfo.PixelScrollRecordOffset;
  ARowHeight := 0;

  AMaxColumnCount := GetMaxColumnCount;
  AMaxRowCount := GetMaxRowCount;
  ANeedBreak := False;

  for I := 0 to GetMaxCount - 1 do
  begin
    ARecordViewInfo := Records[I];
    if I <> 0 then
      if (ALeftBound + ARecordViewInfo.Width > AContentBounds.Right) or ((AMaxColumnCount > 0) and (AMaxColumnCount = ACol)) then
      begin
        if ANeedBreak then
          Break;
        ACol := 0;
        Inc(ARow);
        ALeftBound := AContentBounds.Left;
        Inc(ATopBound, ARowHeight + GetRecordSpaceVert);
        ARowHeight := 0;
      end;

    RecordsViewInfo.Bands[ACol][ARow] := ARecordViewInfo;
    if RecordsViewInfo.GridViewInfo.CalculateDown then
      ARecordViewInfo.CalculationPosition := Point(ALeftBound, ATopBound);

    if not ANeedBreak then
    begin
      ANeedBreak := (ATopBound + ARecordViewInfo.Height > AContentBounds.Bottom) or ((AMaxRowCount > 0) and (AMaxRowCount = ARow));
      if ANeedBreak then
        Dec(RecordsViewInfo.FVisibleCount, ACol);
    end;

    Inc(RecordsViewInfo.FPartVisibleCount);
    if not ANeedBreak or (ARow = 0) then
      Inc(RecordsViewInfo.FVisibleCount);

    Inc(ACol);
    ARowHeight := Max(ARowHeight, ARecordViewInfo.Height);
    Inc(ALeftBound, ARecordViewInfo.Width + GetRecordSpaceHorz);
  end;
end;

function TcxGridCustomLayoutViewInfoVerticalCalculator.GetRecordSpaceHorz: Integer;
begin
  Result := RecordsViewInfo.RecordIndent + GetSeparatorWidth + RecordsViewInfo.RecordIndent;
end;

function TcxGridCustomLayoutViewInfoVerticalCalculator.GetRecordSpaceVert: Integer;
begin
  Result := RecordsViewInfo.RecordIndent + GetSeparatorWidth + RecordsViewInfo.RecordIndent;
end;

{ TcxGridCustomLayoutRecordsViewInfo }

constructor TcxGridCustomLayoutRecordsViewInfo.Create(AGridViewInfo: TcxCustomGridTableViewInfo);
begin
  inherited;
  FRecordHeight := -1;
  FBands := GetBandsClass.Create(Self);
end;

destructor TcxGridCustomLayoutRecordsViewInfo.Destroy;
begin
  FreeAndNil(FBands);
  inherited;
end;

function TcxGridCustomLayoutRecordsViewInfo.CanOffset(AItemCountDelta: Integer): Boolean;
begin
  Result := False;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetRealItem(ARecord: TcxCustomGridRecord): TcxGridCustomLayoutRecordViewInfo;
begin
  Result := TcxGridCustomLayoutRecordViewInfo(inherited GetRealItem(ARecord));
end;

procedure TcxGridCustomLayoutRecordsViewInfo.Calculate;
begin
  FCalculator := GetCalculatorClass.Create(Self);
  try
    FRecordSpaceHorz := FCalculator.GetRecordSpaceHorz;
    FRecordSpaceVert := FCalculator.GetRecordSpaceVert;
    inherited Calculate;
    FRecordWidth := CalculateRecordWidth;
    FRecordHeight := CalculateRecordHeight;
    FMaxRecordHeight := CalculateMaxRecordHeight;
    FCalculator.Calculate(ContentBounds);
  finally
    FCalculator.Free;
  end;
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateContentBounds: TRect;
begin
  Result := inherited CalculateContentBounds;
  FCalculator.AdjustContentBounds(Result);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetPixelScrollContentSize: Integer;
begin
  if IsVertical then
    Result := cxRectHeight(ContentBounds)
  else
    Result := cxRectWidth(ContentBounds);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetItemLeftBound(AIndex: Integer): Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetItemsOffset(AItemCountDelta: Integer): Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetItemTopBound(AIndex: Integer): Integer;
begin
  Result := 0;
end;

procedure TcxGridCustomLayoutRecordsViewInfo.OffsetItem(AIndex, AOffset: Integer);
begin
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateHeight(var AFullyVisible: Boolean): Integer;
var
  AIsParentViewInfoCalculatedDown: Boolean;
  I, AColumnHeight: Integer;
  ALast: TcxGridCustomLayoutRecordViewInfo;
begin
  if Bands.Count = 0 then
    Result := 2 * RecordIndent + RecordHeight
  else
  begin
    Result := 0;
    AIsParentViewInfoCalculatedDown :=
      (GridView.MasterGridView.ViewInfo as TcxCustomGridTableViewInfo).CalculateDown;
    for I := 0 to Bands.VisibleCount - 1 do
    begin
      ALast := Bands[I].LastVisible;
      AColumnHeight := Bands[I].LastVisible.Bounds.Bottom;
      Inc(AColumnHeight, RecordIndent);
      Result := Max(Result, AColumnHeight);
      if AIsParentViewInfoCalculatedDown and AFullyVisible then
        AFullyVisible := ALast.IsFullyVisible;
    end;
  end;
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateRecordHeight: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateRecordWidth: Integer;
begin
  Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateMaxRecordHeight: Integer;
begin
  with ContentBounds do
    Result := Bottom - Top;
  if Result < 0 then Result := 0;
end;

function TcxGridCustomLayoutRecordsViewInfo.CalculateWidth(AMaxWidth: Integer): Integer;
begin
  if Bands.Count = 0 then
    Result := 0
  else
    if Bands.Count = Bands.VisibleCount then
      Result := Bands.LastVisible.FirstVisible.Bounds.Right + RecordIndent
    else
      Result := AMaxWidth;
end;

procedure TcxGridCustomLayoutRecordsViewInfo.DoRightToLeftConversion(const ABounds: TRect);
begin
  inherited DoRightToLeftConversion(ABounds);
  FBands.DoRightToLeftConversion(ABounds);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetCalculatorClass: TcxGridCustomLayoutRecordsViewInfoBasedCalculatorClass;
begin
  Result := TcxGridCustomLayoutRecordsViewInfoBasedCalculator;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetBandsClass: TcxGridCustomLayoutViewBandsClass;
begin
  Result := TcxGridCustomLayoutViewBands;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetItemViewInfoClass: TcxGridCustomLayoutRecordViewInfoClass;
begin
  Result := TcxGridCustomLayoutRecordViewInfo;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetMaxRecordWidth(AColumn: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  if Bands.Count > AColumn then
    for I := 0 to Bands[AColumn].Count - 1 do
      if Bands.IsItemVisible(AColumn, I) then
        Result := Max(Result, Bands[AColumn][I].Width);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetMaxRecordHeight(ARow: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Bands.Count - 1 do
    if Bands.IsItemVisible(I, ARow) then
      Result := Max(Result, Bands[I][ARow].Height);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetRecordIndent: Integer;
begin
  Result := GridView.OptionsView.Indent;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetSeparatorWidth: Integer;
begin
  Result := GridViewInfo.SeparatorsViewInfo.Width;
end;

function TcxGridCustomLayoutRecordsViewInfo.IsVertical: Boolean;
begin
  Result := TcxGridCustomLayoutViewController(Controller).GetMouseWheelScrollingKind = mwskVertical;
end;

function TcxGridCustomLayoutRecordsViewInfo.GetItem(Index: Integer): TcxGridCustomLayoutRecordViewInfo;
begin
  Result := TcxGridCustomLayoutRecordViewInfo(inherited Items[Index]);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetGridView: TcxGridCustomLayoutView;
begin
  Result := TcxGridCustomLayoutView(inherited GridView);
end;

function TcxGridCustomLayoutRecordsViewInfo.GetGridViewInfo: TcxGridCustomLayoutViewViewInfo;
begin
  Result := TcxGridCustomLayoutViewViewInfo(inherited GridViewInfo);
end;

{ TcxGridCustomLayoutViewInfo }

procedure TcxGridCustomLayoutViewViewInfo.CalculateHeight(
  const AMaxSize: TPoint; var AHeight: Integer; var AFullyVisible: Boolean);
begin
  MainCalculate(Classes.Bounds(cxInvisibleCoordinate, 0, AMaxSize.X, AMaxSize.Y));
  AFullyVisible := Controller.IsDataFullyVisible;
  AHeight := RecordsViewInfo.CalculateHeight(AFullyVisible);
  Inc(AHeight, GetNonRecordsAreaHeight(False));
  inherited CalculateHeight(AMaxSize, AHeight, AFullyVisible);
end;

procedure TcxGridCustomLayoutViewViewInfo.CalculateWidth(const AMaxSize: TPoint;
  var AWidth: Integer);
begin
//  AWidth := AMaxSize.X;
  //MainCalculate(Bounds(cxInvisibleCoordinate, 0, AMaxSize.X, AMaxSize.Y));  it is supposed that calculation was called from GetHeight
  AWidth := RecordsViewInfo.CalculateWidth(AMaxSize.X);
  inherited CalculateWidth(AMaxSize, AWidth);
end;

procedure TcxGridCustomLayoutViewViewInfo.AfterCalculating;
begin
  if Visible and (RecordsViewInfo.RecordHeight <> FPrevRecordHeight) then
    Controller.PostGridModeBufferCountUpdate;
  inherited AfterCalculating;
end;

procedure TcxGridCustomLayoutViewViewInfo.BeforeCalculating;
begin
  if Visible then
    FPrevRecordHeight := RecordsViewInfo.RecordHeight;
  RecreateViewInfos;
  inherited BeforeCalculating;
end;

procedure TcxGridCustomLayoutViewViewInfo.Calculate;
begin
  FindPanelViewInfo.MainCalculate;
  FilterViewInfo.MainCalculate;
  inherited Calculate;
  SeparatorsViewInfo.Calculate;
end;

procedure TcxGridCustomLayoutViewViewInfo.CreateViewInfos;
begin
  inherited CreateViewInfos;
  FSeparatorsViewInfo := GetSeparatorsViewInfoClass.Create(Self);
end;

procedure TcxGridCustomLayoutViewViewInfo.DestroyViewInfos(AIsRecreating: Boolean);
begin
  FreeAndNil(FSeparatorsViewInfo);
  inherited;
end;

procedure TcxGridCustomLayoutViewViewInfo.DoRightToLeftConversion(const ABounds: TRect);
var
  I: Integer;
begin
  inherited DoRightToLeftConversion(ABounds);
  FindPanelViewInfo.RightToLeftConversion(ABounds);
  FilterViewInfo.RightToLeftConversion(ABounds);
  for I := 0 to FSeparatorsViewInfo.Count - 1 do
    FSeparatorsViewInfo[I] := TdxRightToLeftLayoutConverter.ConvertRect(FSeparatorsViewInfo[I], ABounds);
end;

function TcxGridCustomLayoutViewViewInfo.GetSeparatorsViewInfoClass: TcxGridCustomLayoutViewSeparatorsViewInfoClass;
begin
  Result := TcxGridCustomLayoutViewSeparatorsViewInfo;
end;

function TcxGridCustomLayoutViewViewInfo.GetController: TcxGridCustomLayoutViewController;
begin
  Result := TcxGridCustomLayoutViewController(inherited Controller);
end;

function TcxGridCustomLayoutViewViewInfo.GetGridView: TcxGridCustomLayoutView;
begin
  Result := TcxGridCustomLayoutView(inherited GridView);
end;

function TcxGridCustomLayoutViewViewInfo.GetRecordsViewInfo: TcxGridCustomLayoutRecordsViewInfo;
begin
  Result := TcxGridCustomLayoutRecordsViewInfo(inherited RecordsViewInfo);
end;

{ TcxGridCustomLayoutRecord }

constructor TcxGridCustomLayoutRecord.Create(AViewData: TcxCustomGridTableViewData; AIndex: Integer;
  const ARecordInfo: TcxRowInfo);
begin
  inherited;
  FExpanded := True;
end;

procedure TcxGridCustomLayoutRecord.DoCollapse(ARecurse: Boolean);
begin
  Expanded := False;
end;

procedure TcxGridCustomLayoutRecord.DoExpand(ARecurse: Boolean);
begin
  Expanded := True;
end;

function TcxGridCustomLayoutRecord.GetExpandable: Boolean;
begin
  Result := GridView.OptionsCustomize.Expandable;
end;

function TcxGridCustomLayoutRecord.GetExpanded: Boolean;
begin
  Result := not Expandable or FExpanded;
end;

function TcxGridCustomLayoutRecord.GetHasCells: Boolean;
begin
  Result := True;
end;

function TcxGridCustomLayoutRecord.GetViewInfoCacheItemClass: TcxCustomGridViewInfoCacheItemClass;
begin
  Result := TcxGridCustomLayoutViewInfoCacheItem;
end;

function TcxGridCustomLayoutRecord.GetViewInfoClass: TcxCustomGridRecordViewInfoClass;
begin
  Result := GridView.ViewInfo.RecordsViewInfo.GetItemViewInfoClass;
end;

function TcxGridCustomLayoutRecord.GetGridView: TcxGridCustomLayoutView;
begin
  Result := TcxGridCustomLayoutView(inherited GridView);
end;

procedure TcxGridCustomLayoutRecord.SetExpanded(Value: Boolean);
begin
  if FExpanded <> Value then
  begin
    if FExpanded and Focused then
      GridView.Controller.FocusedItem := nil;
    FExpanded := Value;
    GridView.SizeChanged;
    if FExpanded and Focused then
      GridView.Controller.MakeFocusedRecordVisible;
  end;
end;

{ TcxGridCustomLayoutViewOptionsCustomize }

procedure TcxGridCustomLayoutViewOptionsCustomize.Assign(Source: TPersistent);
begin
  if Source is TcxGridCustomLayoutViewOptionsCustomize then
    Self.Expandable := TcxGridCustomLayoutViewOptionsCustomize(Source).Expandable;
  inherited;
end;

function TcxGridCustomLayoutViewOptionsCustomize.GetGridView: TcxGridCustomLayoutView;
begin
  Result := TcxGridCustomLayoutView(inherited GridView);
end;

procedure TcxGridCustomLayoutViewOptionsCustomize.SetExpandable(Value: Boolean);
begin
  if FExpandable <> Value then
  begin
    if Value then
      GridView.Controller.FocusedItem := nil;
    FExpandable := Value;
    Changed(vcSize);
  end;
end;

{ TcxGridCustomLayoutViewOptionsView }

constructor TcxGridCustomLayoutViewOptionsView.Create(AGridView: TcxCustomGridView);
begin
  inherited;
  FCaptionSeparator := cxGridCustomLayoutViewDefaultRecordCaptionSeparator;
  FIndent := cxGridCustomLayoutViewRecordDefaultIndent;
  FSeparatorColor := cxGridCustomLayoutViewSeparatorDefaultColor;
  FSeparatorWidth := cxGridCustomLayoutViewSeparatorDefaultWidth;
end;

procedure TcxGridCustomLayoutViewOptionsView.Assign(Source: TPersistent);
begin
  if Source is TcxGridCustomLayoutViewOptionsView then
  begin
    CaptionSeparator := TcxGridCustomLayoutViewOptionsView(Source).CaptionSeparator;
    Indent := TcxGridCustomLayoutViewOptionsView(Source).Indent;
    SeparatorColor := TcxGridCustomLayoutViewOptionsView(Source).SeparatorColor;
    SeparatorWidth := TcxGridCustomLayoutViewOptionsView(Source).SeparatorWidth;
  end;
  inherited;
end;

procedure TcxGridCustomLayoutViewOptionsView.ChangeScale(M, D: Integer);
begin
  inherited;
  Indent := MulDiv(Indent, M, D);
  SeparatorWidth := MulDiv(SeparatorWidth, M, D);
end;

function TcxGridCustomLayoutViewOptionsView.GetSeparatorColor: TColor;
begin
  Result := SeparatorColor;
  if Result = cxGridCustomLayoutViewSeparatorDefaultColor then
    Result := LookAndFeelPainter.DefaultSeparatorColor;
end;

procedure TcxGridCustomLayoutViewOptionsView.SetCaptionSeparator(Value: Char);
begin
  if FCaptionSeparator <> Value then
  begin
    FCaptionSeparator := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCustomLayoutViewOptionsView.SetIndent(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FIndent <> Value then
  begin
    FIndent := Value;
    Changed(vcSize);
  end;
end;

procedure TcxGridCustomLayoutViewOptionsView.SetSeparatorColor(Value: TColor);
begin
  if FSeparatorColor <> Value then
  begin
    FSeparatorColor := Value;
    Changed(vcLayout);
  end;
end;

procedure TcxGridCustomLayoutViewOptionsView.SetSeparatorWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if FSeparatorWidth <> Value then
  begin
    FSeparatorWidth := Value;
    Changed(vcLayout);
  end;
end;

{ TcxGridCustomLayoutView }

function TcxGridCustomLayoutView.GetControllerClass: TcxCustomGridControllerClass;
begin
  Result := TcxGridCustomLayoutViewController;
end;

function TcxGridCustomLayoutView.GetOptionsCustomizeClass: TcxCustomGridTableOptionsCustomizeClass;
begin
  Result := TcxGridCustomLayoutViewOptionsCustomize;
end;

function TcxGridCustomLayoutView.GetOptionsViewClass: TcxCustomGridOptionsViewClass;
begin
  Result := TcxGridCustomLayoutViewOptionsView;
end;

function TcxGridCustomLayoutView.GetPainterClass: TcxCustomGridPainterClass;
begin
  Result := TcxGridCustomLayoutViewPainter;
end;

function TcxGridCustomLayoutView.GetViewDataClass: TcxCustomGridViewDataClass;
begin
  Result := TcxGridCustomLayoutViewViewData;
end;

function TcxGridCustomLayoutView.GetViewInfoClass: TcxCustomGridViewInfoClass;
begin
  Result := TcxGridCustomLayoutViewViewInfo;
end;

procedure TcxGridCustomLayoutView.Reposition;
begin
  Changed(vcSize);
end;

function TcxGridCustomLayoutView.GetOptionsCustomize: TcxGridCustomLayoutViewOptionsCustomize;
begin
  Result := TcxGridCustomLayoutViewOptionsCustomize(inherited OptionsCustomize);
end;

function TcxGridCustomLayoutView.GetOptionsView: TcxGridCustomLayoutViewOptionsView;
begin
  Result := TcxGridCustomLayoutViewOptionsView(inherited OptionsView);
end;

function TcxGridCustomLayoutView.GetViewInfo: TcxGridCustomLayoutViewViewInfo;
begin
  Result := TcxGridCustomLayoutViewViewInfo(inherited ViewInfo);
end;

procedure TcxGridCustomLayoutView.SetOptionsCustomize(Value: TcxGridCustomLayoutViewOptionsCustomize);
begin
  inherited OptionsCustomize := Value;
end;

procedure TcxGridCustomLayoutView.SetOptionsView(Value: TcxGridCustomLayoutViewOptionsView);
begin
  inherited OptionsView := Value;
end;

end.

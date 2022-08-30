{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressScheduler                                         }
{                                                                    }
{           Copyright (c) 2003-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSSCHEDULER AND ALL ACCOMPANYING }
{   VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY.              }
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

unit cxSchedulerAgendaView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, DateUtils, Windows, Forms, Classes, SysUtils, Math, StdCtrls, Graphics, Controls, ExtCtrls, Menus, Variants, StrUtils,
  Contnrs, Generics.Collections, dxCore, cxControls, cxGraphics, cxStyles, cxGeometry, cxClasses, cxFormats, cxVariants,
  cxContainer, cxLookAndFeelPainters, cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerUtils, cxSchedulerStorage, cxSchedulerStrs, cxEdit, dxTouch, dxCoreClasses, cxSchedulerDayView;

type
  TcxSchedulerAgendaViewDayHeaderOrientation = (dhoHorizontal, dhoVertical);
  TcxSchedulerAgendaViewDisplayMode = (avmAllDays, avmSelectedDays, avmSelectedNonEmptyDays);
  TcxSchedulerAgendaViewTimeAreaArrows = (avtaNone, avtaContinueArrowStart, avtaContinueArrowEnd, avtaContinueArrowsBoth);

  TcxSchedulerAgendaView = class;
  TcxSchedulerAgendaViewHitTest = class;
  TcxSchedulerAgendaViewPainter = class;
  TcxSchedulerAgendaViewViewInfo = class;
  TcxSchedulerAgendaViewController = class;
  TcxSchedulerAgendaViewDayHeaderCellViewInfo = class;
  TcxSchedulerAgendaViewEventCellViewInfo = class;
  TcxSchedulerAgendaViewFreeDayCellViewInfo = class;

  { TcxSchedulerAgendaViewController }

  TcxSchedulerAgendaViewController = class(TcxSchedulerCustomResourceViewController)
  private
    FEventDeletingInProcess: Boolean;
    FFocusedCell: TcxSchedulerCustomViewInfoItem;
    FFreeDaysSelection: TList<TDate>;
    FInDraggingProcess: Boolean;
    FIsDeletingOfFocusedEvent: Boolean;
    FIsTopDayCanBeChangedInsideOfScrolling: Boolean;
    FKeepEventsSelection: Boolean;
    FStoredFocusedCellDate: TDate;
    FStoredFocusedEventKey: TcxSchedulerControlEventID;
    FTimer: TTimer;
    procedure ClearFreeDaysSelection(AShift: TShiftState);
    function GetAgendaView: TcxSchedulerAgendaView;
    function GetCellIndexOnNextPage(ACurrentIndex: Integer; AMostBottom: Boolean): Integer;
    function GetCellIndexOnPriorPage(ACurrentIndex: Integer; AMostTop: Boolean): Integer;
    function GetEventSelection: TcxSchedulerEventSelection;
    function GetFirstEventCell(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerAgendaViewEventCellViewInfo;
    function GetFirstInfoCell(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerCustomViewInfoItem;
    function GetFreeDayCell(ADate: TDate): TcxSchedulerAgendaViewFreeDayCellViewInfo;
    function GetNewFocusedEventCellAfterDeletingOfCurrent: TcxSchedulerAgendaViewEventCellViewInfo;
    function GetViewInfo: TcxSchedulerAgendaViewViewInfo;
    function NeedUnselectStartCell(const AStartIndex, AEndIndex: Integer): Boolean;
    procedure SelectFreeDayCell(ADate: TDate; AShift: TShiftState; ANeedRefresh: Boolean);
    procedure SelectCell(ACell: TcxSchedulerCustomViewInfoItem; AShift: TShiftState; ANeedRefresh: Boolean); overload;
    procedure SelectCell(AIndex: Integer; AShift: TShiftState; ANeedRefresh: Boolean); overload;
    procedure SelectCells(AStartIndex, AEndIndex: Integer; const ANeedUnselectByMouse: Boolean);
    procedure SetFocusedCell(ACell: TcxSchedulerCustomViewInfoItem);
    procedure DoSwitchCellSelection(ACell: TcxSchedulerCustomViewInfoItem);
    procedure SwitchCellSelection(ACell: TcxSchedulerCustomViewInfoItem);
    procedure SwitchFocusedCellSelection;
  protected
    FDragEventStoredSelection: TcxObjectList;
    FCurrentDragState: TDragState;

    procedure CancelScroll; override;
    procedure CheckScrolling(const APos: TPoint); override;
    function CreateDragEventHelper: TcxDragEventHelper; override;
    procedure DoFocusFirstEventCell(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo);
    procedure EndDrag(Target: TObject; X, Y: Integer); override;
    function GetDragEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
    function GetFocusedCellIndex: Integer;
    function GetLastInfoCellTime: TDateTime;
    procedure GotoNextPage(AForward, AMostFinal: Boolean; AShift: TShiftState);
    procedure InitTimer(AllowStart: Boolean; AScrollCode: TScrollCode);
    function IsSelected(ACell: TcxSchedulerCustomViewInfoItem): Boolean;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure NavigationButtonDownCustomProcessing; override;
    procedure OnTimer(Sender: TObject);
    procedure PopulateDragEventStoredSelection;
    procedure ProcessDeletedEvent(AEvent: TcxSchedulerControlEvent);
    procedure ResetDeletedEventInfo;
    procedure RestoreEventSelectionFromDragEventStoredSelection;
    procedure SelectNextCell(AForward: Boolean; AShift: TShiftState); reintroduce; virtual;
    procedure SetCellAsFocused(ACell: TcxSchedulerCustomViewInfoItem);
    procedure SilentAddEventSelection(AEvent: TcxSchedulerEvent);
    procedure StartDrag(var DragObject: TDragObject); override;
    procedure StoreFocusedCellAttributes;
    procedure StoreFocusedCellDateAsActualPeriodForEventInitialize(var ADate: TDateTime);
    procedure UnselectAllByESC;
    procedure UnselectEvents; override;

    property EventSelection: TcxSchedulerEventSelection read GetEventSelection;
    property FreeDaysSelection: TList<TDate> read FFreeDaysSelection;
    property InDraggingProcess: Boolean read FInDraggingProcess;
    property IsTopDayCanBeChangedInsideOfScrolling: Boolean read FIsTopDayCanBeChangedInsideOfScrolling write FIsTopDayCanBeChangedInsideOfScrolling;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    function CanCreateEventUsingDialog: Boolean; override;
    function CanCreateEventUsingInplaceEdit: Boolean; override;
    procedure ClearSelection; virtual;

    property AgendaView: TcxSchedulerAgendaView read GetAgendaView;
    property FocusedCell: TcxSchedulerCustomViewInfoItem read FFocusedCell write SetFocusedCell;
    property ViewInfo: TcxSchedulerAgendaViewViewInfo read GetViewInfo;
  end;

  { TcxAgendaViewDragEventHelper }

  TcxAgendaViewDragEventHelper = class(TcxDragEventHelper)
  private
    FDestinationTime: TDateTime;
    FHitPointInTopHalfOfCell: Boolean;
    FNeedDockToNextCell: Boolean;
    function GetAgendaView: TcxSchedulerAgendaView;
    function GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
    function GetController: TcxSchedulerAgendaViewController;
    function GetDragEvent: TcxSchedulerControlEvent;
    function GetHitTest: TcxSchedulerAgendaViewHitTest;
    procedure ExtractCellStartAndFinish(ACell: TcxSchedulerCustomViewInfoItem; const AIsNext: Boolean; var AStart, AFinish: TDateTime);
    function GetNextCell(AEventCell: TcxSchedulerAgendaViewEventCellViewInfo; var AStart, AFinish: TDateTime): TcxSchedulerCustomViewInfoItem;
    function GetPriorCell(ACell: TcxSchedulerCustomViewInfoItem; var AStart, AFinish: TDateTime): TcxSchedulerCustomViewInfoItem;
  protected
    procedure DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean); override;
    procedure EndDrag(Accepted: Boolean); override;
    function GetDragEventCellDestinationTime: TDateTime;
    function GetOriginHitTestMask: Int64; override;
    procedure GetOriginState; override;
    function HasChangedState: Boolean; override;
    function HitAtEvent: Boolean;
    function IsHitPointInTopHalfOfCell: Boolean;
    function IsShowResources: Boolean; override;
    function IsValidTime: Boolean; override;
    procedure PrepareClones; override;
    procedure UpdateViewClonesResources; override;
    procedure UpdateViewClonesTime; override;

    property AgendaView: TcxSchedulerAgendaView read GetAgendaView;
    property AgendaViewInfo: TcxSchedulerAgendaViewViewInfo read GetAgendaViewInfo;
    property Controller: TcxSchedulerAgendaViewController read GetController;
    property DestinationTime: TDateTime read FDestinationTime;
    property DragEvent: TcxSchedulerControlEvent read GetDragEvent;
    property HitTest: TcxSchedulerAgendaViewHitTest read GetHitTest;
    property NeedDockToNextCell: Boolean read FNeedDockToNextCell;
  end;
  { TcxSchedulerAgendaView }

  TcxSchedulerAgendaView = class(TcxSchedulerCustomResourceView)
  private
    FActualFinishForEventInitialize: TDateTime;
    FActualStartForEventInitialize: TDateTime;
    FDayHeaderOrientation: TcxSchedulerAgendaViewDayHeaderOrientation;
    FDisplayMode: TcxSchedulerAgendaViewDisplayMode;
    FEventTextMinWidth: Integer;
    FIsActualPeriodForEventInitializeStored: Boolean;
    FIsSelectedDaysChanged: Boolean;
    FIsTrackingScroll: Boolean;
    FShowLocations: Boolean;
    FShowResources: Boolean;

    function GetController: TcxSchedulerAgendaViewController;
    function GetHitTest: TcxSchedulerAgendaViewHitTest;
    function GetViewInfo: TcxSchedulerAgendaViewViewInfo;
    procedure SetDayHeaderOrientation(AValue: TcxSchedulerAgendaViewDayHeaderOrientation);
    procedure SetDisplayMode(AValue: TcxSchedulerAgendaViewDisplayMode);
    procedure SetEventTextMinWidth(AValue: Integer);
    procedure SetShowLocations(AValue: Boolean);
    procedure SetShowResources(AValue: Boolean);
  protected
    function CanDeactivateOnDateNavigatorSelectionChange: Boolean; override;
    procedure ChangeScale(M, D: Integer); override;
    procedure ClearCachedData; override;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreatePainter: TcxSchedulerSubControlPainter; override;
    function CreateViewAdapter: TcxCustomResourceViewAdapter; override;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    procedure DoAutoSelectTopDay(const ADate: TDateTime);
    procedure DoAfterEventDeleting; override;
    procedure DoBeforeReallyDeleting(AEvent: TcxSchedulerControlEvent); override;
    function DoShowPopupMenu(X, Y: Integer): Boolean; override;
    function GetGroupingKind: TcxSchedulerGroupingKind; override;
    function GetIsTrackingScroll: Boolean;
    function GetNextDayWithEvents(AStart: TDate; const AForward: Boolean): TDate;
    procedure GetRangeControlRange(out AMin, AMax: TDateTime); override;
    function GetSelFinishForInitEventBySelectedTime: TDateTime; override;
    function GetSelStartForInitEventBySelectedTime: TDateTime; override;
    function GetScrollBarKind: TScrollBarKind; virtual;
    procedure InitScrollBarsParameters; override;
    function IsInplaceEditingEnabled: Boolean; override;
    function IsResourceNavigatorAllowed: Boolean; override;
    procedure MakeEventVisible(AEvent: TcxSchedulerControlEvent;
      const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem); override;
    procedure ResetIsSelectedDaysChanged;
    procedure SelectedDaysChanged; override;
    procedure SetActualPeriodForEventInitialize(const AStart, AFinish: TDateTime);
    procedure Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure VisibleChanged; override;

    property Controller: TcxSchedulerAgendaViewController read GetController;
    property ViewInfo: TcxSchedulerAgendaViewViewInfo read GetViewInfo;
    property IsSelectedDaysChanged: Boolean read FIsSelectedDaysChanged;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;

    property HitTest: TcxSchedulerAgendaViewHitTest read GetHitTest;
  published
    property CanShow;
    property Active;
    property DayHeaderOrientation: TcxSchedulerAgendaViewDayHeaderOrientation read FDayHeaderOrientation write SetDayHeaderOrientation default dhoHorizontal;
    property DisplayMode: TcxSchedulerAgendaViewDisplayMode read FDisplayMode write SetDisplayMode default avmAllDays;
    property EventTextMinWidth: Integer read FEventTextMinWidth write SetEventTextMinWidth default 121;
    property ShowLocations: Boolean read FShowLocations write SetShowLocations default True;
    property ShowResources: Boolean read FShowResources write SetShowResources default True;
    property ShowTimeAsClock;
  end;

  { TcxSchedulerAgendaViewHitTest }

  TcxSchedulerAgendaViewHitTest = class(TcxSchedulerCustomResourceViewHitTest)
  protected
    procedure StoreHitEventCellParameters(AEventCell: TcxSchedulerAgendaViewEventCellViewInfo);
  end;

  { TcxSchedulerAgendaViewPainter }

  TcxSchedulerAgendaViewPainter = class(TcxSchedulerCustomViewPainter)
  private
    function GetViewInfo: TcxSchedulerAgendaViewViewInfo;
  protected
    procedure DrawBackground; virtual;
  public
    procedure Paint; override;

    property ViewInfo: TcxSchedulerAgendaViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerAgendaViewDayHeaderCellViewInfo }

  TcxSchedulerAgendaViewDayHeaderCellViewInfo = class(TcxSchedulerDayHeaderCellViewInfo)
  private
    FDayName: string;
    FDayNameRect: TRect;
    FDayNum: string;
    FMonthName: string;
    FVerticalDayHeaderDayNumberRect: TRect;
    FYearText: string;
    FYearTextRect: TRect;
    FRealBounds: TRect;
    FRealDayNameRect: TRect;
    FRealYearTextRect: TRect;
    FRealTextRect: TRect;
    FRealVerticalDayHeaderDayNumberRect: TRect;
    function GetOrientation: TcxSchedulerAgendaViewDayHeaderOrientation;
  protected
    FAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
    procedure CalculateTextRects; virtual;
    function CanDrawYearTextOnTheRight: Boolean;
    procedure CorrectBoundsAsTopHeader;
    procedure CorrectBoundsForVerticalOrientation(ACurrentDayLastCell: TcxSchedulerCustomViewInfoItem);
    procedure DrawCaption(ACanvas: TcxCanvas = nil); override;
    procedure DrawHorizontalHeader; override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DoScroll(const DX, DY: Integer); virtual;
    class function GetLongDayName(const ADayNum: Integer): string;
    procedure Scroll(const DX, DY: Integer);
    procedure RestoreRealAreas; virtual;
    procedure StoreRealAreas; virtual;

    property AgendaViewInfo: TcxSchedulerAgendaViewViewInfo read FAgendaViewInfo;
    property Orientation: TcxSchedulerAgendaViewDayHeaderOrientation read GetOrientation;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;
    procedure Calculate(const AText: string); override;
    procedure CalculateImageLayout; override;
    function ConvertDateToDisplayText(AType: Integer = 0): Integer; override;
  end;

  { TcxSchedulerAgendaViewFreeDayCellViewInfo }

  TcxSchedulerAgendaViewFreeDayCellViewInfo = class(TcxSchedulerContentCellViewInfo)
  private
    FCaption: string;
    FCaptionRect: TRect;
    FDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    function GetAgendaView: TcxSchedulerAgendaView;
    function GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
    function GetFocused: Boolean;
    function GetSelected: Boolean;
  protected
    procedure Calculate; virtual;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure DoDraw; override;
    procedure DrawCaption;
    procedure InitializeDayHeader(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo);
    procedure Scroll(const DX, DY: Integer); virtual;

    property AgendaView: TcxSchedulerAgendaView read GetAgendaView;
    property AgendaViewInfo: TcxSchedulerAgendaViewViewInfo read GetAgendaViewInfo;
    property DayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo read FDayHeader;
  public
    property Focused: Boolean read GetFocused;
    property Selected: Boolean read GetSelected;
  end;

  { TcxSchedulerAgendaViewAdapter }

  TcxSchedulerAgendaViewAdapter = class(TcxCustomResourceViewAdapter)
  public
    procedure GetPageResources(AResources: TcxObjectList); override;
  end;

  { TcxSchedulerAgendaViewEventCellViewInfo }

  TcxSchedulerAgendaViewEventCellViewInfo = class(TcxSchedulerEventCellViewInfo)
  private
    FBlended: Boolean;
    FContentTop: Integer;
    FDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    FIsOnlyEventInDay: Boolean;
    FLabelRect: TRect;
    FResourceRect: TRect;
    FResourceText: string;
    FTimeAreaArrows: TcxSchedulerAgendaViewTimeAreaArrows;
    FTimeAreaRect: TRect;
    FTimeTextRect: TRect;
    function GetAgendaView: TcxSchedulerAgendaView;
    function GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
    function GetFocused: Boolean;
    function GetResourceID: Variant;
  protected
    procedure CalculateLabelRect; virtual;
    procedure CalculateLocationRect; override;
    procedure CalculateTextItemsLayout; virtual;
    procedure CalculateTimeArea;
    procedure CalculateTimeCaption; virtual;
    procedure CalculateTimeLineLayout; override;
    procedure CalculateTimeRectsAsClockView; virtual;
    procedure CalculateTimeRectsAsTextView; virtual;
    function CreateResourceText: string; virtual;

    procedure CalculateEventTimeVisibility; override;
    procedure CalculateItemsLayout; override;
    procedure CalculateShowTimeAsClock; override;
    procedure CalculateActualEditRect(AEditViewData: TcxCustomEditViewData; var AMessageRect: TRect); override;
    function GetActualCellFinish: TDateTime;
    function GetActualCellStart: TDateTime;
    function GetDetailCaptionFlagValue: Boolean; override;
    function GetDetailInfoFlagValue: Boolean; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure InitializeEditViewInfo(var AMessageRect: TRect); override;

    procedure DrawCloneEvent; virtual;
    procedure DrawContent; override;
    procedure DrawContinueArrowsAndCaptions; override;
    procedure DrawLabel; virtual;
    procedure DrawLocation; override;
    procedure DrawMessageSeparator; override;
    procedure DrawResource; virtual;
    procedure DrawState; override;
    procedure DrawTime; override;
    procedure DrawTimeAsClock; virtual;
    procedure DrawTimeAsText; virtual;

    procedure CorrectContentRects; virtual;
    procedure DefineTimeAreaArrows;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    function GetCaptionFontStyle: TFontStyles; override;
    function GetCaptionHeight: Integer;
    function GetDrawCaptionFlags: Cardinal; override;
    function GetEditStyleColor: TColor; override;
    function HintTriggerArea: TRect; override;
    procedure MeasureCaptionExtent(var R: TRect); override;

    procedure InitializeServiceFields(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo; const AIsOnlyEventInDay: Boolean);
    function IsNeedDrawTime: Boolean; override;
    function IsResourcePresent: Boolean;
    procedure Scroll(const DX, DY: Integer); virtual;


    property ActualFinish: TDateTime read GetActualCellFinish;
    property ActualStart: TDateTime read GetActualCellStart;
    property AgendaView: TcxSchedulerAgendaView read GetAgendaView;
    property AgendaViewInfo: TcxSchedulerAgendaViewViewInfo read GetAgendaViewInfo;
    property DayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo read FDayHeader;
    property IsOnlyEventInDay: Boolean read FIsOnlyEventInDay;
  public
    property Focused: Boolean read GetFocused;
    property LabelRect: TRect read FLabelRect;
    property LocationRect;
    property ResourceID: Variant read GetResourceID;
    property ResourceRect: TRect read FResourceRect;
    property ResourceText: string read FResourceText;
  end;

  TcxSchedulerAgendaViewEventCellViewInfoClass = class of TcxSchedulerAgendaViewEventCellViewInfo;

  { TcxSchedulerAgendaViewEventCellModernViewInfo }

  TcxSchedulerAgendaViewEventCellModernViewInfo = class(TcxSchedulerAgendaViewEventCellViewInfo)
  protected
    procedure CalculateTimeLineLayout; override;
    procedure DrawState; override;
    function GetViewStyle: TcxSchedulerViewStyle; override;
    function HintTriggerArea: TRect; override;
  end;

  { TcxSchedulerAgendaViewViewInfo }

  TcxSchedulerAgendaViewViewInfo = class(TcxSchedulerCustomResourceViewViewInfo)
  private
    FActualVisibleDays: TcxSchedulerDateList;
    FAllDayCaptionWidth: Integer;
    FBottomVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    FCellDefaultHeight: Integer;
    FContentHeight: Integer;
    FDayHeaderBoundsPattern: TRect;
    FDayHeaderDefaultHeight: Integer;
    FDayHeaderDigitWidth: Integer;
    FDayHeaderLineHeight: Integer;
    FEventCellBoundsPattern: TRect;
    FEventCellCaptionRectPattern: TRect;
    FEventCellLabelRectPattern: TRect;
    FEventCellLineHeight: Integer;
    FEventCellLocationRectPattern: TRect;
    FEventCellResourceRectPattern: TRect;
    FEventCellStateRectPattern: TRect;
    FEventCellTimeAreaPattern: TRect;
    FFreeDayDefaultHeight: Integer;
    FFreeDayBoundsPattern: TRect;
    FHorizontalDayHeaderYearTextWidth: Integer;
    FInfoCells: TcxObjectList;
    FNearestEventDateAfterSelection: TDateTime;
    FNearestEventDateBeforeSelection: TDateTime;
    FNextTop: Integer;
    FTopScrollPos: Integer;
    FTopVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    FVerticalDayHeaderDayNumberFontSize: Integer;
    FVerticalDayHeaderDayNumberWidth: Integer;
    FVerticalDayHeaderTextHeight: Integer;
    FVerticalDayHeaderWidth: Integer;
    FVerticalDayHeaderYearCaptionWidth: Integer;

    procedure CheckAsNewFocusedCell(ACell: TcxSchedulerCustomViewInfoItem);
    function GetActualSelectedDayFinish: TDateTime;
    function GetActualVisibleDayStart: TDateTime;
    function GetAgendaView: TcxSchedulerAgendaView;
    function GetBottomVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    function GetController: TcxSchedulerAgendaViewController;
    function GetDayHeaderOrientation: TcxSchedulerAgendaViewDayHeaderOrientation;
    function GetDragEventHelper: TcxAgendaViewDragEventHelper;
    procedure SetTopScrollPos(AValue: Integer);
  protected
    FCloneEventCells: TcxSchedulerEventCellViewInfoList;

    function AddDayHeaderInfo(const ADate: TDateTime): TcxSchedulerAgendaViewDayHeaderCellViewInfo; virtual;
    function AddEventInfo(AEvent: TcxSchedulerControlEvent; ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
      const AIsOnlyEventInDay, AInFreeDay: Boolean): TcxSchedulerAgendaViewEventCellViewInfo; virtual;
    function AddFreeDayInfo(
      ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerAgendaViewFreeDayCellViewInfo; virtual;

    function AreDisplayedAnyEvents: Boolean; virtual;
    function AreThereEventsInVisibleInterval(AResourceIndex: Integer): Boolean; override;
    procedure CalculateContentNavigationButtons; override;
    procedure CalculateDayHeaderPatterns; virtual;
    procedure CalculateEventCellPatterns; virtual;
    procedure CalculateFreeDayPattern; virtual;
    procedure CalculateEventCellStateRectPattern; virtual;
    procedure CalculateEventCellTimeAreaPattern; virtual;
    procedure CalculateEventCellLabelRectPattern; virtual;
    procedure CalculateEventCellTextItemsPatterns; virtual;
    procedure CalculateItemsLayoutInfo; virtual;
    procedure CalculateMetrics; override;
    procedure CalculateMetricsForVerticalDayHeader(AFont: TFont);
    procedure CalculatePatterns; virtual;
    procedure CheckNewTopScrollPos(var AValue: Integer);
    procedure CheckScrollPositions;
    procedure CheckVisibleDayHeaders;
    procedure Clear; override;
    procedure CorrectTopVisibleHeaderBounds;
    procedure DoCalculate; override;
    procedure DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); override;
    procedure DoRightToLeftConversion(const AClientBounds: TRect); override;
    procedure ExtractEvents(ASource, ADest: TcxSchedulerEventList; ADate: TDateTime); virtual;
    procedure ExtractCloneEvents(ADest: TcxSchedulerEventList; ADate: TDateTime);
    function GetCellDate(ACell: TcxSchedulerCustomViewInfoItem): TDate;
    function GetCurrentFont: TFont;
    function GetDayContentHeight(const ADate: TDate): Integer;
    function GetDayHeader(ACell: TcxSchedulerCustomViewInfoItem): TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    function GetEventCellColor(const AIsSelected: Boolean): TColor;
    function GetEventCellTextColor(const AIsSelected: Boolean): TColor;
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; override;
    function GetEventHint(AEvent: TcxSchedulerControlEvent): string; override;
    function GetEventStart(AEvent: TcxSchedulerControlEvent; ADate: TDateTime): TDateTime;
    function GetEventFinish(AEvent: TcxSchedulerControlEvent; ADate: TDateTime): TDateTime;
    function GetInfoCell(AIndex: Integer): TcxSchedulerCustomViewInfoItem; overload;
    function GetInfoCell(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime): TcxSchedulerCustomViewInfoItem; overload;
    function GetTopScrollPosPreliminaryValue(ACell: TcxSchedulerCustomViewInfoItem): Integer;
    function GetTopScrollPosToDisplayCell(ACell: TcxSchedulerCustomViewInfoItem): Integer;
    function GetTopVisibleCell: TcxSchedulerCustomViewInfoItem;
    function GetVertScrollPage: Integer; virtual;
    procedure HideCloneEventsOnDragDrop; override;
    procedure HideSourceEventsOnDragDrop; override;
    function IsNavigationButtonsVertical: Boolean; override;
    function IsResourcePresent(AEventResourceID: Variant): Boolean;
    function IsScrollBarsParametersWasChanged: Boolean;
    function IsTopVisibleCell(ACell: TcxSchedulerCustomViewInfoItem): Boolean;
    procedure MakeFirstSelectedDayAsTop;
    procedure MeasureVerticalDayHeaderDayNumberFontSize(const AAreaHeight: Integer; var AFont: TFont);
    function NeedClearResources(AGroupingKind: TcxSchedulerGroupingKind): Boolean; override;
    procedure OnContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); override;
    procedure PopulateActualVisibleDays;
    procedure PutPriorTopVisibleDayHeaderAboveCurrent;
    procedure ResetTopScrollPos;
    procedure ReturnVisibleInterval(var AStart, AEnd: TDateTime); override;
    procedure DoScroll(const DY: Integer); virtual;
    procedure Scroll(const DY: Integer);
    procedure SetContentNavigationButtonsIntervals; override;
    procedure TryAddContentNavigationButtonsAtScrolling;

    procedure RestoreCanvasFontParams(const AFontSize: Integer; const AFontStyle: TFontStyles);
    procedure StoreCanvasFontParams(var AFontSize: Integer; var AFontStyle: TFontStyles);

    property ActualVisibleDays: TcxSchedulerDateList read FActualVisibleDays;
    property ActualSelectedDayFinish: TDateTime read GetActualSelectedDayFinish;
    property ActualVisibleDayStart: TDateTime read GetActualVisibleDayStart;
    property AllDayCaptionWidth: Integer read FAllDayCaptionWidth;
    property BottomVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo read FBottomVisibleDayHeader;
    property CellDefaultHeight: Integer read FCellDefaultHeight;
    property CloneEventCells: TcxSchedulerEventCellViewInfoList read FCloneEventCells;
    property DayHeaderDefaultHeight: Integer read FDayHeaderDefaultHeight;
    property DayHeaderLineHeight: Integer read FDayHeaderLineHeight;
    property DayHeaderOrientation: TcxSchedulerAgendaViewDayHeaderOrientation read GetDayHeaderOrientation;
    property DragEventHelper: TcxAgendaViewDragEventHelper read GetDragEventHelper;
    property EventCellCaptionRectPattern: TRect read FEventCellCaptionRectPattern;
    property EventCellLabelRectPattern: TRect read FEventCellLabelRectPattern;
    property EventCellLineHeight: Integer read FEventCellLineHeight;
    property EventCellLocationRectPattern: TRect read FEventCellLocationRectPattern;
    property EventCellResourceRectPattern: TRect read FEventCellResourceRectPattern;
    property EventCellStateRectPattern: TRect read FEventCellStateRectPattern;
    property EventCellTimeAreaPattern: TRect read FEventCellTimeAreaPattern;
    property FreeDayDefaultHeight: Integer read FFreeDayDefaultHeight;
    property InfoCell[Index: Integer]: TcxSchedulerCustomViewInfoItem read GetInfoCell;
    property InfoCells: TcxObjectList read FInfoCells;
    property TopVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo read FTopVisibleDayHeader;
    property Controller: TcxSchedulerAgendaViewController read GetController;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure Calculate; override;
    procedure CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    function GetResourceText(AEventResourceID: Variant): string;
    procedure MakeCellVisible(ACell: TcxSchedulerCustomViewInfoItem);
    procedure MakeFocusedCellVisible;

    property AgendaView: TcxSchedulerAgendaView read GetAgendaView;
    property ContentHeight: Integer read FContentHeight;
    property TopScrollPos: Integer read FTopScrollPos write SetTopScrollPos;
    property VertScrollPage: Integer read GetVertScrollPage;
  end;

implementation

uses
  cxDrawTextUtils;

const
  dxDPIEventCellContentVerticalOffset: Integer = 5;
  cxSchedulerAgendaViewScrollStep = 20;
  dxDPITextOffset: Integer = cxTextOffset;
  dxDPIDoubleTextOffset: Integer = cxTextOffset * 2;
  dxDPITripleTextOffset: Integer = cxTextOffset * 3;
  dxDPIFourfoldTextOffset: Integer = cxTextOffset * 4;
  dxDPIQuintupleTextOffset: Integer = cxTextOffset * 5;
  dxSchedulerAgendaViewCloneEventHeight = 4;

  Direction: array[Boolean] of Integer = (-1, 1);

type
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);
  TcxSchedulerControlEventAccess = class(TcxSchedulerControlEvent);
  TcxSchedulerEventImagesAccess = class(TcxSchedulerEventImages);
  TcxSchedulerResourceViewInfoAccess = class(TcxSchedulerResourceViewInfo);
  TcxSchedulerCustomDateNavigatorAccess = class(TcxSchedulerCustomDateNavigator);
  TcxSchedulerCustomViewInfoItemAccess = class(TcxSchedulerCustomViewInfoItem);
  TcxSchedulerCustomResourceViewInfoItemAccess = class(TcxSchedulerCustomResourceViewInfoItem);
  TcxSchedulerEventSelectionAccess = class(TcxSchedulerEventSelection);
  TcxSchedulerControlEventIDAccess = class(TcxSchedulerControlEventID);
  TcxSchedulerViewInfoCellListAccess = class(TcxSchedulerViewInfoCellList);

procedure cxRectCenterVerticallyRelativelyOfArea(var ARect: TRect; const AArea: TRect);
var
  AHalfRect, AHalfArea: Integer;
begin
  AHalfRect := (ARect.Top + ARect.Bottom) div 2;
  AHalfArea := (AArea.Top + AArea.Bottom) div 2;
  ARect := cxRectOffset(ARect, 0, AHalfArea - AHalfRect);
end;

{ TcxSchedulerAgendaViewController }

constructor TcxSchedulerAgendaViewController.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(nil);
  FFreeDaysSelection := TList<TDate>.Create;
  FDragEventStoredSelection := TcxObjectList.Create;
end;

destructor TcxSchedulerAgendaViewController.Destroy;
begin
  FreeAndNil(FTimer);
  FreeAndNil(FStoredFocusedEventKey);
  FreeAndNil(FFreeDaysSelection);
  FreeAndNil(FDragEventStoredSelection);
  inherited Destroy;
end;

function TcxSchedulerAgendaViewController.CanCreateEventUsingDialog: Boolean;
var
  ADate: TDateTime;
begin
  Result := inherited CanCreateEventUsingDialog;
  if Result then
    StoreFocusedCellDateAsActualPeriodForEventInitialize(ADate);
end;

function TcxSchedulerAgendaViewController.CanCreateEventUsingInplaceEdit: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerAgendaViewController.CancelScroll;
begin
  InitTimer(False, scEndScroll);
end;

procedure TcxSchedulerAgendaViewController.CheckScrolling(const APos: TPoint);
var
  ACanScroll: Boolean;
  AScrollCode: TScrollCode;
  ATop, ABottom: TRect;
begin
  ATop := ViewInfo.Bounds;
  ATop.Bottom := ATop.Top + cxScrollZoneSize;
  ABottom := ViewInfo.Bounds;
  ABottom.Top := ABottom.Bottom - cxScrollZoneSize;
  ACanScroll := True;
  AScrollCode := scLineUp;
  if PtInRect(ATop, APos) then
    AScrollCode := scLineUp
  else
    if PtInRect(ABottom, APos) then
      AScrollCode := scLineDown
    else
     ACanScroll := False;
  if (ACanScroll <> FTimer.Enabled) or (Integer(AScrollCode) <> FTimer.Tag) then
    InitTimer(ACanScroll, AScrollCode);
end;

procedure TcxSchedulerAgendaViewController.ClearFreeDaysSelection(AShift: TShiftState);
begin
  if [ssShift, ssCtrl] * AShift = [] then
    FreeDaysSelection.Clear;
end;

procedure TcxSchedulerAgendaViewController.ClearSelection;
begin
  FreeDaysSelection.Clear;
  Scheduler.UnselectEvents;
end;

function TcxSchedulerAgendaViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxAgendaViewDragEventHelper.Create(Scheduler);
end;

procedure TcxSchedulerAgendaViewController.DoFocusFirstEventCell(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo);
var
  ACell: TcxSchedulerAgendaViewEventCellViewInfo;
begin
  ACell := GetFirstEventCell(ADayHeader);
  FocusedCell := ACell;
  SelectCell(ACell, [], True);
end;

procedure TcxSchedulerAgendaViewController.EndDrag(Target: TObject; X, Y: Integer);
begin
  FInDraggingProcess := False;
  inherited EndDrag(Target, X, Y);
end;

function TcxSchedulerAgendaViewController.GetAgendaView: TcxSchedulerAgendaView;
begin
  Result := TcxSchedulerAgendaView(inherited View)
end;

function TcxSchedulerAgendaViewController.GetCellIndexOnNextPage(ACurrentIndex: Integer; AMostBottom: Boolean): Integer;
var
  I, ATop, APageHeight: Integer;
  ABounds: TRect;
begin
  if AMostBottom then
    Result := ViewInfo.InfoCells.Count - 1
  else
  begin
    Result := ACurrentIndex;
    ATop := ViewInfo.InfoCell[Result].Bounds.Top;
    APageHeight := ViewInfo.VertScrollPage;
    for I := ACurrentIndex + 1 to ViewInfo.InfoCells.Count - 1 do
    begin
      Result := I;
      ABounds := ViewInfo.InfoCell[Result].Bounds;
      if (ABounds.Top >= APageHeight) and ((ABounds.Top - ATop > APageHeight) or (ABounds.Bottom - ATop > APageHeight)) then
      begin
        Dec(Result);
        Break;
      end;
    end;
  end;
end;

function TcxSchedulerAgendaViewController.GetCellIndexOnPriorPage(ACurrentIndex: Integer; AMostTop: Boolean): Integer;
var
  I, ATop, AHeaderHeight, APageHeight: Integer;
  ACell: TcxSchedulerCustomViewInfoItem;
  ABounds: TRect;
begin
  if AMostTop then
    Result := 0
  else
  begin
    Result := ACurrentIndex;
    ACell := ViewInfo.InfoCell[Result];
    ATop := ACell.Bounds.Top;
    AHeaderHeight := cxRectHeight(ViewInfo.GetDayHeader(ACell).Bounds);
    APageHeight := ViewInfo.VertScrollPage - AHeaderHeight;
    for I := ACurrentIndex - 1 downto 0 do
    begin
      Result := I;
      ABounds := ViewInfo.InfoCell[Result].Bounds;
      if (ABounds.Bottom <= AHeaderHeight) and ((ATop - ABounds.Top > APageHeight) or (ATop - ABounds.Bottom > APageHeight)) then
      begin
        Inc(Result);
        Break;
      end;
    end;
  end;
end;

function TcxSchedulerAgendaViewController.GetDragEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  if DragEventHelper = nil then
    Exit;
  for I := 0 to ViewInfo.CloneEventCells.Count - 1 do
  begin
    Result := TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.CloneEventCells[I]);
    if (Result.Event.Source = DragEvent) and
       (Int(TcxAgendaViewDragEventHelper(DragEventHelper).DestinationTime) = Int(Result.ActualStart)) then
      Break
    else
      Result := nil;
  end;
end;

function TcxSchedulerAgendaViewController.GetEventSelection: TcxSchedulerEventSelection;
begin
  Result := TcxCustomSchedulerAccess(Scheduler).EventList.Selection;
end;

function TcxSchedulerAgendaViewController.GetFirstEventCell(
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerAgendaViewEventCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ViewInfo.EventCells.Count - 1 do
    if TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.EventCells[I]).DayHeader = ADayHeader then
    begin
      Result := TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.EventCells[I]);
      Break;
    end;
end;

function TcxSchedulerAgendaViewController.GetFirstInfoCell(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerCustomViewInfoItem;
var
  I: Integer;
begin
  Result := GetFirstEventCell(ADayHeader);
  if Result = nil then
    for I := 0 to ViewInfo.InfoCells.Count - 1 do
      if (ViewInfo.InfoCell[I] is TcxSchedulerAgendaViewFreeDayCellViewInfo) and
        (TcxSchedulerAgendaViewFreeDayCellViewInfo(ViewInfo.InfoCell[I]).DayHeader = ADayHeader) then
      begin
        Result := ViewInfo.InfoCell[I];
        Break;
      end;
end;

function TcxSchedulerAgendaViewController.GetFocusedCellIndex: Integer;
begin
  Result := ViewInfo.InfoCells.IndexOf(FFocusedCell);
end;

function TcxSchedulerAgendaViewController.GetLastInfoCellTime: TDateTime;
var
  AIndex: Integer;
begin
  if ViewInfo.InfoCells.Count > 0 then
  begin
    AIndex := ViewInfo.InfoCells.Count - 1;
    if ViewInfo.InfoCell[AIndex] is TcxSchedulerAgendaViewFreeDayCellViewInfo then
      Result := TcxSchedulerAgendaViewFreeDayCellViewInfo(ViewInfo.InfoCell[AIndex]).DayHeader.DateTime
    else
      Result := TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.InfoCell[AIndex]).EventFinish;
  end
  else
    if ViewInfo.ActualVisibleDays.Count > 0 then
      Result := Trunc(ViewInfo.ActualVisibleDays[0])
    else
      Result := 0;
end;

function TcxSchedulerAgendaViewController.GetNewFocusedEventCellAfterDeletingOfCurrent: TcxSchedulerAgendaViewEventCellViewInfo;

  function CanBeNewFocusedEventCell(const AIndex: Integer; const ACurrentDate: TDate): Boolean;
  begin
    Result := (ViewInfo.InfoCell[AIndex] is TcxSchedulerAgendaViewEventCellViewInfo) and
      ((AgendaView.DisplayMode = avmSelectedNonEmptyDays) or
       (Int(TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.InfoCell[AIndex]).FDateTime) = ACurrentDate))
  end;

var
  AIndex: Integer;
  ACurrentFocusedCellDate: TDate;
begin
  Result := nil;
  AIndex := ViewInfo.InfoCells.IndexOf(FocusedCell);
  ACurrentFocusedCellDate := Int((FocusedCell as TcxSchedulerAgendaViewEventCellViewInfo).FDateTime);
  if (AIndex + 1 < ViewInfo.InfoCells.Count) and CanBeNewFocusedEventCell(AIndex + 1, ACurrentFocusedCellDate) then
    Result := TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.InfoCell[AIndex + 1])
  else
    if (AIndex > 0) and CanBeNewFocusedEventCell(AIndex - 1, ACurrentFocusedCellDate)  then
      Result := TcxSchedulerAgendaViewEventCellViewInfo(ViewInfo.InfoCell[AIndex - 1]);
end;

function TcxSchedulerAgendaViewController.GetFreeDayCell(ADate: TDate): TcxSchedulerAgendaViewFreeDayCellViewInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to ViewInfo.InfoCells.Count - 1 do
    if (ViewInfo.InfoCell[I] is TcxSchedulerAgendaViewFreeDayCellViewInfo) and
      (TcxSchedulerAgendaViewFreeDayCellViewInfo(ViewInfo.InfoCell[I]).DateTime = ADate) then
    begin
      Result := TcxSchedulerAgendaViewFreeDayCellViewInfo(ViewInfo.InfoCell[I]);
      Break;
    end;
end;

function TcxSchedulerAgendaViewController.GetViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := AgendaView.ViewInfo;
end;

procedure TcxSchedulerAgendaViewController.GotoNextPage(AForward, AMostFinal: Boolean; AShift: TShiftState);
var
  AIndex, ANewIndex: Integer;
begin
  if ViewInfo.InfoCells.Count = 0 then
  begin
    SetCellAsFocused(nil);
    Exit;
  end;

  if FFocusedCell <> nil then
    AIndex := Max(0, GetFocusedCellIndex)
  else
    begin
      SetCellAsFocused(ViewInfo.GetTopVisibleCell);
      AIndex := Max(0, GetFocusedCellIndex);
    end;
  if AForward then
    ANewIndex := GetCellIndexOnNextPage(AIndex, AMostFinal)
  else
    ANewIndex := GetCellIndexOnPriorPage(AIndex, AMostFinal);

    if ANewIndex <> AIndex then
    begin
      SetCellAsFocused(ViewInfo.InfoCell[ANewIndex]);
      if ssShift in AShift then
        SelectCells(AIndex, ANewIndex, False)
      else
        SelectCell(ANewIndex, [], True)
    end
    else
      if not(ssShift in AShift) then
        SelectCell(ANewIndex, [], True);
  ViewInfo.MakeFocusedCellVisible;
end;

procedure TcxSchedulerAgendaViewController.InitTimer(AllowStart: Boolean; AScrollCode: TScrollCode);
begin
  if not AllowStart then
    FTimer.OnTimer := nil
  else
    FTimer.OnTimer := OnTimer;
  FTimer.Enabled := AllowStart;
  FTimer.Interval := cxScrollInterval;
  FTimer.Tag := Integer(AScrollCode);
end;


function TcxSchedulerAgendaViewController.IsSelected(ACell: TcxSchedulerCustomViewInfoItem): Boolean;
begin
  Result := False;
  if ACell <> nil then
    if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
      Result := TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event.Selected
    else
      Result := TcxSchedulerAgendaViewFreeDayCellViewInfo(ACell).Selected;
end;

procedure TcxSchedulerAgendaViewController.KeyDown(var Key: Word; Shift: TShiftState);
var
  APrevVScrollPos: Integer;
begin
  APrevVScrollPos := TcxCustomSchedulerAccess(Scheduler).VScrollBar.Position;
  TcxCustomSchedulerAccess(Scheduler).HintController.Hide;
  if View.Active then
    case Key of
      VK_TAB:
        SelectNextCell(not (ssShift in Shift), []);
      VK_ESCAPE:
          UnselectAllByESC;
      VK_SPACE:
        if (ssCtrl in Shift) and not (ssShift in Shift) then
          SwitchFocusedCellSelection;
      VK_PRIOR:
        GotoNextPage(False, False, Shift);
      VK_NEXT:
        GotoNextPage(True, False, Shift);
      VK_END:
        GotoNextPage(True, True, Shift);
      VK_HOME:
        GotoNextPage(False, True, Shift);
      VK_DOWN:
        SelectNextCell(True, Shift);
      VK_UP:
        SelectNextCell(False, Shift);
      VK_INSERT:
        if CanCreateEventUsingDialog then
          Scheduler.CreateEventUsingDialog;
      VK_DELETE:
        DeleteSelectedEvents;
    end;
  if APrevVScrollPos <> TcxCustomSchedulerAccess(Scheduler).VScrollBar.Position then
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
end;

procedure TcxSchedulerAgendaViewController.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);

  procedure PrepareSelectionInfo(var AOldFocusedCellIndex, ANewFocusedCellIndex: Integer; var AIsFreeDay, ANeedUnselect: Boolean);
  begin
    AOldFocusedCellIndex := GetFocusedCellIndex;
    ClearFreeDaysSelection(Shift);
    if HitTest.HitAtEvent then
      SetCellAsFocused(HitTest.EventCell)
    else
    begin
      AIsFreeDay := True;
      FKeepEventsSelection := ssShift in Shift;
      SetCellAsFocused(HitTest.ContentCell);
    end;
    ANewFocusedCellIndex := GetFocusedCellIndex;
    ANeedUnselect := IsSelected(ViewInfo.InfoCell[ANewFocusedCellIndex]);
  end;

  procedure ApplySelectionInfo(const AOldFocusedCellIndex, ANewFocusedCellIndex: Integer;
    const ADate: TDateTime; const AIsFreeDay: Boolean; const ANeedUnselect: Boolean);
  begin
    if (ssShift in Shift) and (AOldFocusedCellIndex >= 0) then
    begin
      FIsTopDayCanBeChangedInsideOfScrolling := True;
      try
        if ViewInfo.InfoCell[ANewFocusedCellIndex] is TcxSchedulerAgendaViewEventCellViewInfo then
          DoSwitchCellSelection(ViewInfo.InfoCell[ANewFocusedCellIndex]);
        SelectCells(AOldFocusedCellIndex, ANewFocusedCellIndex, ANeedUnselect);
      finally
        FIsTopDayCanBeChangedInsideOfScrolling := False;
      end;
    end
    else
      if AIsFreeDay then
        SelectFreeDayCell(ADate, Shift, True);
    FKeepEventsSelection := False;
  end;

var
  ADate: TDateTime;
  AIsFreeDay, AIsInfoCell, AObligatoryUpdate, ANeedUnselect: Boolean;
  AOldFocusedCellIndex, ANewFocusedCellIndex: Integer;
  AIsDayHeader: Boolean;
begin
  AIsFreeDay := False;
  ADate := -1;
  AIsInfoCell := False;
  AObligatoryUpdate := False;
  AIsDayHeader := HitTest.HitAtDayHeader;
  if AIsDayHeader then
  begin
    ADate := HitTest.Time;
    AgendaView.SetActualPeriodForEventInitialize(ADate, ADate + 1);
    AObligatoryUpdate := (FFocusedCell is TcxSchedulerAgendaViewFreeDayCellViewInfo) and
      (FreeDaysSelection.Count = 1) and (TcxCustomSchedulerAccess(Scheduler).EventList.Selection.Count = 0) and
      (ADate = TcxSchedulerAgendaViewFreeDayCellViewInfo(FFocusedCell).DateTime);
  end
  else
  begin
    if HitTest.HitAtEvent or HitTest.HitAtContent then
    begin
      AIsInfoCell := True;
      PrepareSelectionInfo(AOldFocusedCellIndex, ANewFocusedCellIndex, AIsFreeDay, ANeedUnselect);
    end;
    StoreFocusedCellDateAsActualPeriodForEventInitialize(ADate);
    ViewInfo.FTopScrollPos := ViewInfo.GetTopScrollPosPreliminaryValue(FFocusedCell);
  end;

  FIsTopDayCanBeChangedInsideOfScrolling := True;
  AgendaView.FIsActualPeriodForEventInitializeStored := True;
  FKeepEventsSelection := FKeepEventsSelection or AIsDayHeader;
  try
    inherited MouseDown(Button, Shift, X, Y);
    if AObligatoryUpdate then
      AgendaView.LayoutChanged;
  finally
    FIsTopDayCanBeChangedInsideOfScrolling := False;
    AgendaView.FIsActualPeriodForEventInitializeStored := False;
    if AIsDayHeader then
      FKeepEventsSelection := False;
  end;

  if AIsInfoCell then
    ApplySelectionInfo(AOldFocusedCellIndex, ANewFocusedCellIndex, ADate, AIsFreeDay, ANeedUnselect);
end;

procedure TcxSchedulerAgendaViewController.NavigationButtonDownCustomProcessing;
begin
  EventSelection.Clear;
  FreeDaysSelection.Clear;
end;

procedure TcxSchedulerAgendaViewController.OnTimer(Sender: TObject);

  function GetShiftState: TShiftState;
  const
    Buttons: array[Boolean] of Integer = (VK_LBUTTON, VK_RBUTTON);
  begin
    Result := [];
    if GetAsyncKeyState(Buttons[GetSystemMetrics(SM_SWAPBUTTON) <> 0]) < 0 then
      Include(Result, ssLeft);
    if GetAsyncKeyState(VK_CONTROL) < 0 then
      Include(Result, ssCtrl);
  end;

var
  APos: Integer;
  AMousePos: TPoint;
  AShift: TShiftState;
begin
  AShift := GetShiftState;
  if not (ssLeft in AShift) then
  begin
    CancelScroll;
    SetCaptureControl(nil);
  end
  else
  begin
    APos := ViewInfo.TopScrollPos;
    AgendaView.Scroll(sbVertical, TScrollCode(FTimer.Tag), APos);
    AMousePos := View.ScreenToClient(GetMouseCursorPos);
    MouseMove(AShift, AMousePos.X, AMousePos.Y);
  end;
end;

procedure TcxSchedulerAgendaViewController.PopulateDragEventStoredSelection;
var
  I: Integer;
begin
  FDragEventStoredSelection.Clear;
  for I := 0 to ViewInfo.Events.Clones.Count - 1 do
    FDragEventStoredSelection.Add(TcxSchedulerControlEventID.Create(TcxSchedulerEvent(ViewInfo.Events.Clones[I])));
end;

procedure TcxSchedulerAgendaViewController.RestoreEventSelectionFromDragEventStoredSelection;
var
  I: Integer;
begin
  EventSelection.Clear;
  for I := 0 to FDragEventStoredSelection.Count - 1 do
    TcxSchedulerEventSelectionAccess(EventSelection).FKeys.Add(
      TcxSchedulerControlEventIDAccess.Create(FDragEventStoredSelection[I] as TcxSchedulerControlEventID));
  EventSelection.Update;
end;

procedure TcxSchedulerAgendaViewController.ProcessDeletedEvent(AEvent: TcxSchedulerControlEvent);
var
  AFocusedEventCell, ANewFocusedEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
begin
  FEventDeletingInProcess := True;
  if (FocusedCell <> nil) and (FocusedCell is TcxSchedulerAgendaViewEventCellViewInfo) and
     (TcxSchedulerAgendaViewEventCellViewInfo(FocusedCell).Event = AEvent) then
  begin
    FIsDeletingOfFocusedEvent := True;
    AFocusedEventCell := TcxSchedulerAgendaViewEventCellViewInfo(FocusedCell);
    FreeAndNil(FStoredFocusedEventKey);
    FStoredFocusedCellDate := Int(AFocusedEventCell.FDateTime);
    ANewFocusedEventCell := GetNewFocusedEventCellAfterDeletingOfCurrent;
    if ANewFocusedEventCell <> nil then
    begin
      FStoredFocusedCellDate := Int(ANewFocusedEventCell.FDateTime);
      FStoredFocusedEventKey := TcxSchedulerControlEventID.Create(ANewFocusedEventCell.Event);
    end
  end;
end;

procedure TcxSchedulerAgendaViewController.ResetDeletedEventInfo;
begin
  FEventDeletingInProcess := False;
  if FIsDeletingOfFocusedEvent and (FocusedCell <> nil) then
    SelectCell(FocusedCell, [], False);
  FIsDeletingOfFocusedEvent := False;
end;

procedure TcxSchedulerAgendaViewController.SelectFreeDayCell(ADate: TDate; AShift: TShiftState; ANeedRefresh: Boolean);
begin
  SelectCell(GetFreeDayCell(ADate), AShift, ANeedRefresh);
end;

procedure TcxSchedulerAgendaViewController.SelectCell(ACell: TcxSchedulerCustomViewInfoItem;
  AShift: TShiftState; ANeedRefresh: Boolean);
var
  ADate: TDate;
begin
  if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
  begin
    ClearFreeDaysSelection(AShift);
    if not (ssCtrl in AShift) then
      TcxCustomSchedulerAccess(Scheduler).EventList.Selection.Add(TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event, AShift);
  end
  else
  begin
    ADate := ViewInfo.GetCellDate(ACell);
    ClearFreeDaysSelection(AShift);
    if [ssShift, ssCtrl] * AShift = [] then
      TcxCustomSchedulerAccess(Scheduler).EventList.Selection.Clear;
    if not(ssCtrl in AShift) and (FreeDaysSelection.IndexOf(ADate) < 0) then
      FreeDaysSelection.Add(ADate);
  end;
  if ANeedRefresh then
  begin
    FIsTopDayCanBeChangedInsideOfScrolling := True;
    try
      AgendaView.Refresh;
    finally
      FIsTopDayCanBeChangedInsideOfScrolling := False;
    end;
  end;
end;

procedure TcxSchedulerAgendaViewController.SelectCell(AIndex: Integer; AShift: TShiftState; ANeedRefresh: Boolean);
begin
  SelectCell(ViewInfo.InfoCell[AIndex], AShift, ANeedRefresh);
end;

function TcxSchedulerAgendaViewController.NeedUnselectStartCell(const AStartIndex, AEndIndex: Integer): Boolean;
var
  AEndCell, ACell: TcxSchedulerCustomViewInfoItem;
  AEndEvent: TcxSchedulerControlEvent;
  I: Integer;
begin
  AEndCell := ViewInfo.InfoCell[AEndIndex];
  Result := IsSelected(ViewInfo.InfoCell[AStartIndex]) and IsSelected(AEndCell);
  if Result and (AEndCell is TcxSchedulerAgendaViewEventCellViewInfo) then
  begin
    AEndEvent := TcxSchedulerAgendaViewEventCellViewInfo(AEndCell).Event;
    I := AStartIndex;
    if Int(AEndEvent.Start) < Int(AEndEvent.Finish) then
      while ((AStartIndex <= AEndIndex) and (I - 1 >= 0)) or ((AStartIndex > AEndIndex) and (I + 1 < ViewInfo.InfoCells.Count))  do
      begin
        ACell := ViewInfo.InfoCell[I];
        if not IsSelected(ACell) then
          Break;
        if (ACell is TcxSchedulerAgendaViewEventCellViewInfo) and (TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event = AEndEvent) then
          Result := False;
        if not Result then
          Break;
        Dec(I, Direction[AStartIndex <= AEndIndex]);
      end;
  end;
end;

procedure TcxSchedulerAgendaViewController.SelectCells(AStartIndex, AEndIndex: Integer; const ANeedUnselectByMouse: Boolean);
var
  AIndex: Integer;
  ANeedUnselect: Boolean;
  ACell: TcxSchedulerCustomViewInfoItem;
begin
  ANeedUnselect := ANeedUnselectByMouse or NeedUnselectStartCell(AStartIndex, AEndIndex);
  AIndex := AStartIndex;
  repeat
    ACell := ViewInfo.InfoCell[AIndex];
    if (AIndex = AEndIndex) or (not ANeedUnselect) then
    begin
      if not IsSelected(ACell) then
        SelectCell(ACell, [ssShift], False);
    end
    else
      if IsSelected(ACell) then
        DoSwitchCellSelection(ACell);

    Inc(AIndex, Direction[AStartIndex <= AEndIndex]);
  until ((AStartIndex <= AEndIndex) and (AIndex > AEndIndex)) or ((AStartIndex >= AEndIndex) and (AIndex < AEndIndex));
  AgendaView.Refresh;
end;

procedure TcxSchedulerAgendaViewController.DoSwitchCellSelection(ACell: TcxSchedulerCustomViewInfoItem);
var
  ADate: TDateTime;
  AIndex: Integer;
begin
  if ACell = nil then
    Exit;
  if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
    EventSelection.Add(TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event, [ssCtrl])
  else
  begin
    ADate := TcxSchedulerAgendaViewFreeDayCellViewInfo(ACell).DateTime;
    AIndex := FreeDaysSelection.IndexOf(ADate);
    if AIndex < 0 then
      FreeDaysSelection.Add(ADate)
    else
      FreeDaysSelection.Delete(AIndex);
  end;
end;

procedure TcxSchedulerAgendaViewController.SwitchCellSelection(ACell: TcxSchedulerCustomViewInfoItem);
begin
  DoSwitchCellSelection(ACell);
  AgendaView.Refresh;
end;

procedure TcxSchedulerAgendaViewController.SwitchFocusedCellSelection;
begin
  if FocusedCell <> nil then
    SwitchCellSelection(FocusedCell)
  else
  if ViewInfo.InfoCells.Count = 0 then
    SetCellAsFocused(nil)
  else
  begin
    FocusedCell := ViewInfo.InfoCell[ViewInfo.InfoCells.IndexOf(GetFirstInfoCell(ViewInfo.TopVisibleDayHeader))];
    SwitchCellSelection(FocusedCell);
  end;
end;

procedure TcxSchedulerAgendaViewController.SelectNextCell(AForward: Boolean; AShift: TShiftState);

  function CellListWasUpdated(const AOldCount: Integer): Boolean;
  begin
    Result := AOldCount <> ViewInfo.InfoCells.Count;
  end;

  procedure CheckCellsIndexes(ACell, ANewCell: TcxSchedulerCustomViewInfoItem; var AIndex, ANewIndex: Integer);
  var
    I: Integer;
    AItem: TcxSchedulerCustomViewInfoItem;
  begin
    AIndex := -1;
    ANewIndex := -1;
    for I := ViewInfo.InfoCells.Count - 1 downto 0 do
    begin
      AItem := ViewInfo.InfoCell[I];
      if AItem = ACell then
        AIndex := I;
      if AItem = ANewCell then
        ANewIndex := I;
      if (AIndex >= 0) and (ANewIndex >= 0) then
        Break;
    end;
  end;

var
  ACount, AIndex, ANewIndex: Integer;
  ANeedSelectCurrentCell: Boolean;
  ACell, ANewCell: TcxSchedulerCustomViewInfoItem;
begin
  ACount := ViewInfo.InfoCells.Count;
  if ACount = 0 then
    SetCellAsFocused(nil)
  else
  begin
    AIndex := GetFocusedCellIndex;
    ANeedSelectCurrentCell := False;
    if AIndex = -1 then
    begin
      AIndex := ViewInfo.InfoCells.IndexOf(GetFirstInfoCell(ViewInfo.TopVisibleDayHeader));
      ANeedSelectCurrentCell := True;
    end;
    ANewIndex := Min(Max(0, AIndex + Direction[AForward]), ACount - 1);
    if ANewIndex <> AIndex then
    begin
      ACell := ViewInfo.InfoCell[AIndex];
      ANewCell := ViewInfo.InfoCell[ANewIndex];
      FocusedCell := ANewCell;
      if CellListWasUpdated(ACount) then
        CheckCellsIndexes(ACell, ANewCell, AIndex, ANewIndex);
      if ANeedSelectCurrentCell then
      begin
        if AIndex >=0 then
          SelectCell(ViewInfo.InfoCell[AIndex], AShift, False);
      end
      else
        if (ssShift in AShift) and (AIndex >= 0) and (ANewIndex >= 0) and NeedUnselectStartCell(AIndex, ANewIndex) then
          DoSwitchCellSelection(ViewInfo.InfoCell[AIndex]);
      SelectCell(FocusedCell, AShift, True);
    end
    else
      if ANeedSelectCurrentCell then
        SelectCell(ViewInfo.InfoCell[AIndex], AShift, True);
  end;
end;

procedure TcxSchedulerAgendaViewController.SetCellAsFocused(ACell: TcxSchedulerCustomViewInfoItem);
begin
  FFocusedCell := ACell;
  StoreFocusedCellAttributes;
end;

procedure TcxSchedulerAgendaViewController.SetFocusedCell(ACell: TcxSchedulerCustomViewInfoItem);
begin
  SetCellAsFocused(ACell);
  ViewInfo.MakeCellVisible(FocusedCell);
end;

procedure TcxSchedulerAgendaViewController.SilentAddEventSelection(AEvent: TcxSchedulerEvent);
begin
  if AEvent = nil then
    Exit;
  TcxSchedulerEventSelectionAccess(EventSelection).FKeys.Add(
    TcxSchedulerEventSelectionAccess(EventSelection).CreateItem(AEvent));
  EventSelection.Update;
end;

procedure TcxSchedulerAgendaViewController.StartDrag(var DragObject: TDragObject);
begin
  FInDraggingProcess := True;
  inherited StartDrag(DragObject);
end;

procedure TcxSchedulerAgendaViewController.StoreFocusedCellAttributes;
begin
  if FIsDeletingOfFocusedEvent then
    Exit;
  FreeAndNil(FStoredFocusedEventKey);
  FStoredFocusedCellDate := -1;
  if FFocusedCell <> nil then
  begin
    FStoredFocusedCellDate := ViewInfo.GetCellDate(FFocusedCell);
    if FFocusedCell is TcxSchedulerAgendaViewEventCellViewInfo then
      FStoredFocusedEventKey :=
        TcxSchedulerControlEventID.Create(TcxSchedulerAgendaViewEventCellViewInfo(FFocusedCell).Event);
  end;
end;

procedure TcxSchedulerAgendaViewController.StoreFocusedCellDateAsActualPeriodForEventInitialize(var ADate: TDateTime);
begin
  if AgendaView.FIsActualPeriodForEventInitializeStored then
    Exit;
  if FFocusedCell <> nil then
    ADate := Int(TcxSchedulerCustomViewInfoItemAccess(FFocusedCell).FDateTime)
  else
    ADate := ViewInfo.SelectedDays[ViewInfo.SelectedDays.Count - 1];
  AgendaView.SetActualPeriodForEventInitialize(ADate, ADate + 1);
end;

procedure TcxSchedulerAgendaViewController.UnselectAllByESC;
begin
  if not IsSelected(FocusedCell) then
    ClearSelection
  else
  if (FreeDaysSelection.Count + EventSelection.Count) > 1 then
  begin
    FreeDaysSelection.Clear;
    EventSelection.Clear;
    if FocusedCell is TcxSchedulerAgendaViewEventCellViewInfo then
      SilentAddEventSelection(TcxSchedulerAgendaViewEventCellViewInfo(FocusedCell).Event)
    else
      FreeDaysSelection.Add(TcxSchedulerAgendaViewFreeDayCellViewInfo(FocusedCell).DateTime);
  end;
  AgendaView.Refresh;
end;

procedure TcxSchedulerAgendaViewController.UnselectEvents;
begin
  if not FKeepEventsSelection then
    ClearSelection;
end;

{ TcxAgendaViewDragEventHelper }

procedure TcxAgendaViewDragEventHelper.DragOver(const P: TPoint; State: TDragState; var Accepted: Boolean);
begin
  Controller.FCurrentDragState := State;
  inherited DragOver(P, State, Accepted);
end;

procedure TcxAgendaViewDragEventHelper.EndDrag(Accepted: Boolean);
begin
  if not IsDragCopy then
    Controller.PopulateDragEventStoredSelection;
  TcxSchedulerEventSelectionAccess(Controller.EventSelection).InternalClear;
  if not Accepted then
    Controller.FStoredFocusedCellDate := DateOf(Controller.StartDragHitTime)
  else
    if IsDragCopy then
      Controller.FStoredFocusedCellDate := -1
    else
      Controller.FStoredFocusedCellDate := Int(FDestinationTime);
  inherited EndDrag(Accepted);
  Controller.FDragEventStoredSelection.Clear;
end;

function TcxAgendaViewDragEventHelper.GetAgendaView: TcxSchedulerAgendaView;
begin
  Result := Controller.AgendaView;
end;

function TcxAgendaViewDragEventHelper.GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := Controller.ViewInfo;
end;

function TcxAgendaViewDragEventHelper.GetController: TcxSchedulerAgendaViewController;
begin
  Result := TcxSchedulerAgendaViewController(inherited Controller);
end;

function TcxAgendaViewDragEventHelper.GetDragEvent: TcxSchedulerControlEvent;
begin
  Result := Controller.DragEvent;
end;

function TcxAgendaViewDragEventHelper.GetHitTest: TcxSchedulerAgendaViewHitTest;
begin
  Result := TcxSchedulerAgendaViewHitTest(inherited HitTest);
end;

function TcxAgendaViewDragEventHelper.GetOriginHitTestMask: Int64;
const
  Mask = (1 shl htcControl) or (1 shl htcTime);
begin
  Result := Mask;
end;

procedure TcxAgendaViewDragEventHelper.GetOriginState;
begin
  inherited GetOriginState;
  FHitPointInTopHalfOfCell := IsHitPointInTopHalfOfCell;
end;

function TcxAgendaViewDragEventHelper.HasChangedState: Boolean;
var
  ATopHalf: Boolean;
begin
  Result := inherited HasChangedState;
  if not Result and not IsAtOrigin and (HitTest.HitAtDayHeader or HitAtEvent) then
  begin
    ATopHalf := IsHitPointInTopHalfOfCell;
    Result := FHitPointInTopHalfOfCell <> ATopHalf;
    FHitPointInTopHalfOfCell := ATopHalf;
  end;
end;

function TcxAgendaViewDragEventHelper.HitAtEvent: Boolean;
begin
  Result := HitTest.HitAtEvent and not HitTest.Event.IsClone;
end;

function TcxAgendaViewDragEventHelper.IsHitPointInTopHalfOfCell: Boolean;
begin
  Result := InRange(HitTest.HitY,
    HitTest.FHitObjectBounds.Top, (HitTest.FHitObjectBounds.Top + HitTest.FHitObjectBounds.Bottom) div 2);
end;

function TcxAgendaViewDragEventHelper.IsShowResources: Boolean;
begin
  Result := False;
end;

function TcxAgendaViewDragEventHelper.IsValidTime: Boolean;
begin
  Result := HitTest.HitAtControl or IsValidNavigatorDate;
end;

procedure TcxAgendaViewDragEventHelper.PrepareClones;
begin
  Events.Selection.Add(Controller.DragEvent, []);
  Events.CreateClones;
end;

procedure TcxAgendaViewDragEventHelper.UpdateViewClonesResources;
begin
//
end;

procedure TcxAgendaViewDragEventHelper.ExtractCellStartAndFinish(
  ACell: TcxSchedulerCustomViewInfoItem; const AIsNext: Boolean; var AStart, AFinish: TDateTime);
begin
  if ACell = nil then
  begin
    if AIsNext then
      ACell := AgendaViewInfo.InfoCell[Controller.ViewInfo.InfoCells.Count - 1]
    else
      ACell := AgendaViewInfo.TopVisibleDayHeader;
  end;
  if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
  begin
    AStart := TcxSchedulerAgendaViewEventCellViewInfo(ACell).ActualStart;
    AFinish := TcxSchedulerAgendaViewEventCellViewInfo(ACell).ActualFinish;
  end
  else
  begin
    AStart := TcxSchedulerCustomResourceViewInfoItemAccess(ACell).DateTime;
    AFinish := AStart;
  end;
end;

function TcxAgendaViewDragEventHelper.GetNextCell(AEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
  var AStart, AFinish: TDateTime): TcxSchedulerCustomViewInfoItem;
var
  AIndex: Integer;
begin
  Result := nil;
  AIndex := AgendaViewInfo.InfoCells.IndexOf(AEventCell);
  if AIndex + 1 <= AgendaViewInfo.InfoCells.Count - 1 then
    Result := AgendaViewInfo.InfoCell[AIndex + 1];
  if (Result <> nil) and
    ((Result is TcxSchedulerAgendaViewEventCellViewInfo) and
     (TcxSchedulerAgendaViewEventCellViewInfo(Result).DayHeader <> AEventCell.DayHeader)) then
    Result := TcxSchedulerAgendaViewEventCellViewInfo(Result).DayHeader;
  ExtractCellStartAndFinish(Result, True, AStart, AFinish);
end;

function TcxAgendaViewDragEventHelper.GetPriorCell(ACell: TcxSchedulerCustomViewInfoItem;
  var AStart, AFinish: TDateTime): TcxSchedulerCustomViewInfoItem;
var
  AIndex: Integer;
begin
  Result := nil;
  if ACell is TcxSchedulerAgendaViewDayHeaderCellViewInfo then
  begin
    AIndex := AgendaViewInfo.InfoCells.IndexOf(Controller.GetFirstInfoCell(TcxSchedulerAgendaViewDayHeaderCellViewInfo(ACell)));
    if AIndex > 0 then
      Result := AgendaViewInfo.InfoCell[AIndex - 1];
  end
  else
  begin
    AIndex := AgendaViewInfo.InfoCells.IndexOf(ACell);
    if AgendaView.DayHeaderOrientation = dhoVertical then
    begin
      if AIndex > 0 then
        Result := AgendaViewInfo.InfoCell[AIndex - 1];
    end
    else
      if AIndex > 0 then
      begin
        Result := AgendaViewInfo.InfoCell[AIndex - 1];
        if Result.Bounds.Bottom < (ACell as TcxSchedulerAgendaViewEventCellViewInfo).DayHeader.Bounds.Bottom then
          Result := TcxSchedulerAgendaViewEventCellViewInfo(ACell).DayHeader;
      end
      else
        Result := (ACell as TcxSchedulerAgendaViewEventCellViewInfo).DayHeader;
  end;

  ExtractCellStartAndFinish(Result, False, AStart, AFinish);
end;

function TcxAgendaViewDragEventHelper.GetDragEventCellDestinationTime: TDateTime;
var
  ANextCell, APriorCell: TcxSchedulerCustomViewInfoItem;
  AEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
  ACellStart, ACellFinish, ANextDate: TDateTime;
  AIsTopHalf: Boolean;
begin
  Result := HitTest.Time;
  FNeedDockToNextCell := False;
  if HitTest.FHitObject = nil then
  begin
    if (AgendaViewInfo.InfoCells.Count > 0) then
      if (AgendaViewInfo.InfoCells[AgendaViewInfo.InfoCells.Count - 1] is TcxSchedulerAgendaViewEventCellViewInfo) then
      begin
        AEventCell := TcxSchedulerAgendaViewEventCellViewInfo(AgendaViewInfo.InfoCells[AgendaViewInfo.InfoCells.Count - 1]);
        if AEventCell.Event.IsAllDayOrLonger then
          Result := Int(AEventCell.ActualStart)
        else
        begin
          Result := AEventCell.ActualFinish;
          if Result >= Int(HitTest.Time) + 1 then
            Result := (AEventCell.ActualStart + Int(HitTest.Time) + 1) / 2;
        end;
      end
      else
        FNeedDockToNextCell := AgendaViewInfo.InfoCells[AgendaViewInfo.InfoCells.Count - 1] is TcxSchedulerAgendaViewFreeDayCellViewInfo;
    Exit;
  end;

  AIsTopHalf := IsHitPointInTopHalfOfCell;
  if (HitTest.FHitObject is TcxSchedulerAgendaViewFreeDayCellViewInfo) or
     ((HitTest.FHitObject is TcxSchedulerAgendaViewDayHeaderCellViewInfo) and
      ((AgendaView.DayHeaderOrientation = dhoVertical) or not AIsTopHalf)) then
  begin
    FNeedDockToNextCell := HitTest.FHitObject is TcxSchedulerAgendaViewFreeDayCellViewInfo;
    Exit;
  end;

  if (HitTest.FHitObject is TcxSchedulerAgendaViewEventCellViewInfo) and
     TcxSchedulerAgendaViewEventCellViewInfo(HitTest.FHitObject).Event.IsAllDayOrLonger then
  begin
    if DragEvent.IsAllDayOrLonger then
      Result := Int(Result)
    else
    begin
      AEventCell := TcxSchedulerAgendaViewEventCellViewInfo(HitTest.FHitObject);
      ANextCell := GetNextCell(AEventCell, ACellStart, ACellFinish);
      while (ANextCell is TcxSchedulerAgendaViewEventCellViewInfo) and
            TcxSchedulerAgendaViewEventCellViewInfo(ANextCell).Event.IsAllDayOrLonger and (ACellStart = Int(Result)) do
      begin
        AEventCell := TcxSchedulerAgendaViewEventCellViewInfo(ANextCell);
        ANextCell := GetNextCell(AEventCell, ACellStart, ACellFinish);
      end;
      if (ANextCell = nil) or (ANextCell is TcxSchedulerAgendaViewDayHeaderCellViewInfo) or
         (ANextCell is TcxSchedulerAgendaViewFreeDayCellViewInfo) then
        Result := Int(Result)
      else
        Result := Max(Result, ACellStart - DragEvent.Duration);
      FNeedDockToNextCell := (ANextCell is TcxSchedulerAgendaViewEventCellViewInfo) and
        (AEventCell.ActualStart = Int(TcxSchedulerAgendaViewEventCellViewInfo(ANextCell).ActualStart));
    end;
    Exit;
  end;

  if AIsTopHalf then
  begin
    APriorCell := GetPriorCell(HitTest.FHitObject as TcxSchedulerCustomViewInfoItem, ACellStart, ACellFinish);
    if HitTest.FHitObject is TcxSchedulerAgendaViewDayHeaderCellViewInfo then
    begin
      Result := ACellFinish;
      ADayHeader := TcxSchedulerAgendaViewDayHeaderCellViewInfo(HitTest.FHitObject);
      if Result >= ADayHeader.DateTime then
        Result := (ACellStart + ADayHeader.DateTime) / 2;
    end
    else
    begin
      AEventCell := TcxSchedulerAgendaViewEventCellViewInfo(HitTest.FHitObject);
      if APriorCell is TcxSchedulerAgendaViewFreeDayCellViewInfo then
      begin
        ACellFinish := ACellStart + 1;
        Result := AEventCell.ActualStart - DragEvent.Duration;
        if Result < ACellFinish then
          Result := (ACellFinish + AEventCell.EventStart) / 2;
      end
      else
      begin
        Result := AEventCell.ActualStart - DragEvent.Duration;
        if Result < ACellStart then
          Result := (ACellStart + AEventCell.EventStart) / 2;
      end;
      FNeedDockToNextCell := True;
    end;
  end

  else
  begin
    AEventCell := TcxSchedulerAgendaViewEventCellViewInfo(HitTest.FHitObject);
    Result := AEventCell.EventFinish;
    ANextCell := GetNextCell(HitTest.FHitObject as TcxSchedulerAgendaViewEventCellViewInfo, ACellStart, ACellFinish);
    if (ANextCell = nil) or (ANextCell is TcxSchedulerAgendaViewDayHeaderCellViewInfo) then
    begin
      ANextDate := Int(AEventCell.EventStart) + 1;
      if Result > ANextDate then
        Result := Min(IncMinute(AEventCell.EventStart, 1), IncMinute(ANextDate, -1));
    end
    else
    begin
      if Result > ACellStart then
        Result := (ACellStart + AEventCell.ActualStart) / 2;
    end;
  end;
end;

procedure TcxAgendaViewDragEventHelper.UpdateViewClonesTime;
var
  I: Integer;
  ADelta: TDateTime;
  AEvent: TcxSchedulerControlEvent;
begin
  FDestinationTime := GetDragEventCellDestinationTime;
  if (TimeOf(FDestinationTime) = 0) or DragEvent.AllDayEvent then
    ADelta := DateOf(FDestinationTime) - DateOf(Controller.StartDragHitTime)
  else
    ADelta := FDestinationTime - Controller.StartDragHitTime;
  for I := 0 to Events.Clones.Count - 1 do
  begin
    AEvent := Events.Clones[I];
    AEvent.MoveTo(AEvent.Source.Start + ADelta);
  end;
end;

{ TcxSchedulerAgendaView }

constructor TcxSchedulerAgendaView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FShowLocations := True;
  FShowResources := True;
  FDisplayMode := avmAllDays;
  FEventTextMinWidth := 121;
end;

procedure TcxSchedulerAgendaView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerAgendaView then
  begin
    FDayHeaderOrientation := TcxSchedulerAgendaView(Source).DayHeaderOrientation;
    FDisplayMode := TcxSchedulerAgendaView(Source).DisplayMode;
    FEventTextMinWidth  := TcxSchedulerAgendaView(Source).EventTextMinWidth;
    FShowLocations := TcxSchedulerAgendaView(Source).ShowLocations;
    FShowResources := TcxSchedulerAgendaView(Source).ShowResources;
  end;
  inherited Assign(Source);
end;

function TcxSchedulerAgendaView.CanDeactivateOnDateNavigatorSelectionChange: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerAgendaView.ChangeScale(M, D: Integer);
begin
  EventTextMinWidth := MulDiv(EventTextMinWidth, M, D);
end;

procedure TcxSchedulerAgendaView.ClearCachedData;
begin
  if Scheduler.IsLoading then
    Exit;
  ViewInfo.ResetTopScrollPos;
  inherited ClearCachedData;
end;

function TcxSchedulerAgendaView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerAgendaViewController.Create(Self);
end;

function TcxSchedulerAgendaView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerAgendaViewHitTest.Create(Self);
end;

function TcxSchedulerAgendaView.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerAgendaViewPainter.Create(Self);
end;

function TcxSchedulerAgendaView.CreateViewAdapter: TcxCustomResourceViewAdapter;
begin
  Result := TcxSchedulerAgendaViewAdapter.Create(Self);
end;

function TcxSchedulerAgendaView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerAgendaViewViewInfo.Create(Self);
end;

function TcxSchedulerAgendaView.DoShowPopupMenu(X, Y: Integer): Boolean;
begin
  if HitTest.HitAtContent then
      Result := Scheduler.ContentPopupMenu.Popup(X, Y)
  else
  begin
    FIsActualPeriodForEventInitializeStored := True;
    try
      Result := inherited DoShowPopupMenu(X, Y);
    finally
      FIsActualPeriodForEventInitializeStored := False;
    end;
  end;
end;

function TcxSchedulerAgendaView.GetController: TcxSchedulerAgendaViewController;
begin
  Result := TcxSchedulerAgendaViewController(inherited Controller);
end;

function TcxSchedulerAgendaView.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := gkNone;
end;

function TcxSchedulerAgendaView.GetHitTest: TcxSchedulerAgendaViewHitTest;
begin
  Result := TcxSchedulerAgendaViewHitTest(inherited HitTest);
end;

function TcxSchedulerAgendaView.GetIsTrackingScroll: Boolean;
begin
  Result := FIsTrackingScroll;
end;

function TcxSchedulerAgendaView.GetViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := TcxSchedulerAgendaViewViewInfo(inherited ViewInfo);
end;

function TcxSchedulerAgendaView.GetScrollBarKind: TScrollBarKind;
begin
  Result := sbVertical;
end;

function TcxSchedulerAgendaView.GetSelFinishForInitEventBySelectedTime: TDateTime;
begin
  Result := FActualFinishForEventInitialize;
end;

function TcxSchedulerAgendaView.GetSelStartForInitEventBySelectedTime: TDateTime;
begin
  Result := FActualStartForEventInitialize;
end;

procedure TcxSchedulerAgendaView.InitScrollBarsParameters;
var
  AMin, AMax: Integer;
begin
  AMin := Min(0, ViewInfo.TopScrollPos);
  AMax := ViewInfo.ContentHeight - 1 + AMin;
  SetScrollBarInfo(GetScrollBarKind,  AMin, AMax, 1, ViewInfo.VertScrollPage, ViewInfo.TopScrollPos, True, True);
end;

function TcxSchedulerAgendaView.IsInplaceEditingEnabled: Boolean;
begin
  Result := False;
end;

function TcxSchedulerAgendaView.IsResourceNavigatorAllowed: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerAgendaView.MakeEventVisible(AEvent: TcxSchedulerControlEvent;
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem);
begin
  ViewInfo.MakeCellVisible(ViewInfo.GetInfoCell(AEvent, ADate));
end;

function TcxSchedulerAgendaView.GetNextDayWithEvents(AStart: TDate; const AForward: Boolean): TDate;
var
  AIndex: Integer;
  AEventDays: TcxSchedulerDateList;
begin
  Result := AStart;
  AEventDays := TcxSchedulerCustomDateNavigatorAccess(Scheduler.DateNavigator).EventDays;
  AIndex := AEventDays.IndexOf(AStart) + Direction[AForward];
  if InRange(AIndex, 0, AEventDays.Count - 1) then
    Result := AEventDays[AIndex];
end;

procedure TcxSchedulerAgendaView.Scroll(AScrollBarKind: TScrollBarKind; AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if AScrollBarKind <> GetScrollBarKind then Exit;
  TcxCustomSchedulerAccess(Scheduler).HintController.Hide;
  if not (AScrollCode in [scTrack, scPosition]) then
    AScrollPos := ViewInfo.TopScrollPos;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, cxSchedulerAgendaViewScrollStep);
    scLineDown:
      Inc(AScrollPos, cxSchedulerAgendaViewScrollStep);
    scPageUp:
      Dec(AScrollPos, ViewInfo.VertScrollPage);
    scPageDown:
      Inc(AScrollPos, ViewInfo.VertScrollPage);
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := ViewInfo.ContentHeight;
  end;
  if FIsTrackingScroll and (AScrollCode = scPosition) and (DisplayMode = avmAllDays) then
  begin
    FIsTrackingScroll := AScrollCode = scTrack;
    DoAutoSelectTopDay(TcxSchedulerAgendaViewViewInfo(ViewInfo).TopVisibleDayHeader.DateTime);
  end
  else
  begin
    FIsTrackingScroll := AScrollCode = scTrack;
    ViewInfo.TopScrollPos := AScrollPos;
    if DisplayMode <> avmSelectedNonEmptyDays then
        ViewInfo.TryAddContentNavigationButtonsAtScrolling;
    if DisplayMode = avmAllDays then
      VisibleRangeChanged;
  end;
end;

procedure TcxSchedulerAgendaView.ResetIsSelectedDaysChanged;
begin
  FIsSelectedDaysChanged := False;
end;

procedure TcxSchedulerAgendaView.SelectedDaysChanged;
begin
  FIsSelectedDaysChanged := True;
end;

procedure TcxSchedulerAgendaView.SetActualPeriodForEventInitialize(const AStart, AFinish: TDateTime);
begin
  FActualStartForEventInitialize := AStart;
  FActualFinishForEventInitialize := AFinish;
end;

procedure TcxSchedulerAgendaView.SetDayHeaderOrientation(AValue: TcxSchedulerAgendaViewDayHeaderOrientation);
begin
  if FDayHeaderOrientation <> AValue then
  begin
    FDayHeaderOrientation := AValue;
    ViewInfo.FTopScrollPos := 0;
    Changed;
    ViewInfo.MakeFocusedCellVisible;
  end;
end;

procedure TcxSchedulerAgendaView.SetEventTextMinWidth(AValue: Integer);
begin
  AValue := Max(10, AValue);
  if FEventTextMinWidth <> AValue then
  begin
    FEventTextMinWidth := AValue;
    Changed;
    ViewInfo.MakeFocusedCellVisible;
  end;
end;

procedure TcxSchedulerAgendaView.SetDisplayMode(AValue: TcxSchedulerAgendaViewDisplayMode);
begin
  if FDisplayMode <> AValue then
  begin
    FDisplayMode := AValue;
    Scheduler.FullRefresh;
    ViewInfo.MakeFocusedCellVisible;
  end;
end;

procedure TcxSchedulerAgendaView.DoAutoSelectTopDay(const ADate: TDateTime);
begin
  Controller.IsTopDayCanBeChangedInsideOfScrolling := True;
  try
    SelectedDays.Clear;
    SelectedDays.Add(ADate);
    TcxSchedulerCustomDateNavigatorAccess(Scheduler.DateNavigator).EnsureSelectionVisible;
  finally
    Controller.IsTopDayCanBeChangedInsideOfScrolling := False;
  end;
end;

procedure TcxSchedulerAgendaView.DoAfterEventDeleting;
begin
  inherited DoAfterEventDeleting;
  Controller.ResetDeletedEventInfo;
  Refresh;
end;

procedure TcxSchedulerAgendaView.DoBeforeReallyDeleting(AEvent: TcxSchedulerControlEvent);
begin
  Controller.ProcessDeletedEvent(AEvent);
end;

procedure TcxSchedulerAgendaView.GetRangeControlRange(out AMin, AMax: TDateTime);
begin
  if DisplayMode <> avmAllDays then
  begin
    AMin := GetFirstVisibleDate;
    AMax := GetLastVisibleDate;
  end
  else
  begin
    if ViewInfo.TopVisibleDayHeader = nil then
      AMin := Date
    else
      AMin := ViewInfo.TopVisibleDayHeader.DateTime;
    if ViewInfo.BottomVisibleDayHeader = nil then
      AMax := AMin
    else
      AMax := ViewInfo.BottomVisibleDayHeader.DateTime;
  end;
end;

procedure TcxSchedulerAgendaView.SetShowLocations(AValue: Boolean);
begin
  if FShowLocations <> AValue then
  begin
    FShowLocations := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerAgendaView.SetShowResources(AValue: Boolean);
begin
  if FShowResources <> AValue then
  begin
    FShowResources := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerAgendaView.VisibleChanged;
begin
  TcxSchedulerAgendaViewController(Controller).SetCellAsFocused(nil);
  inherited VisibleChanged;
end;

{ TcxSchedulerAgendaViewHitTest }

procedure TcxSchedulerAgendaViewHitTest.StoreHitEventCellParameters(AEventCell: TcxSchedulerAgendaViewEventCellViewInfo);
begin
  FHitObject := AEventCell;
  SetHitTime(htcTime, AEventCell.ActualStart);
  FHitObjectBounds := AEventCell.Bounds;
end;

{ TcxSchedulerAgendaViewPainter }

procedure TcxSchedulerAgendaViewPainter.DrawBackground;
const
  AViewParams: TcxViewParams = (
    Bitmap: nil;
    Color: clWhite;
    Font: nil;
    TextColor: clDefault
  );
begin
  AViewParams.Color := ViewInfo.LookAndFeelPainter.DefaultContentColor;
  Canvas.Rectangle(ViewInfo.Bounds, AViewParams, []);
end;

function TcxSchedulerAgendaViewPainter.GetViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := TcxSchedulerAgendaViewViewInfo(inherited ViewInfo);
end;

procedure TcxSchedulerAgendaViewPainter.Paint;
begin
  inherited Paint;
  DrawBackground;
  ViewInfo.DayHeaderCells.Draw(Canvas, DrawHeaderCell);
  ViewInfo.ContentCells.Draw(Canvas, DrawContentCell);
  ViewInfo.EventCells.Draw(Canvas, DrawEventCell);
  ViewInfo.CloneEventCells.Draw(Canvas, DrawEventCell);
  Canvas.Brush.Style := bsClear;
  ViewInfo.DrawNavigationButtons(Canvas, DrawButtonCell);
end;

{ TcxSchedulerAgendaViewDayHeaderCellViewInfo }

constructor TcxSchedulerAgendaViewDayHeaderCellViewInfo.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(APainter, ABounds, AVisibleRect, AViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FVisibleRect := AVisibleRect;
  AlignHorz := taLeftJustify;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.Calculate(const AText: string);
const
  ABorders: array[Boolean] of TcxBorders = ([bBottom], [bBottom, bRight]);
begin
  ConvertDateToDisplayText;
  CalculateTextRects;
  Borders := ABorders[Orientation = dhoVertical];
  StoreRealAreas;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.CalculateImageLayout;
begin
  DisplayBounds := FTextRect;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.CalculateTextRects;
var
  ANeedShiftYearCaption: Boolean;
  AMax, DY: Integer;
begin
  FTextRect := cxRectInflate(TextRect, -ScaleFactor.Apply(dxDPITextOffset), 0);
  FTextRect.Bottom := FTextRect.Top + AgendaViewInfo.DayHeaderLineHeight;
  FYearTextRect := TextRect;
  ANeedShiftYearCaption := CanDrawYearTextOnTheRight and (fsItalic in Font.Style);
  if ANeedShiftYearCaption then
    Dec(FYearTextRect.Left, AgendaViewInfo.FDayHeaderDigitWidth div 2);
  AMax := Max(Bounds.Top + AgendaViewInfo.DayHeaderDefaultHeight, FTextRect.Bottom + 2 * ScaleFactor.Apply(dxDPITextOffset));
  FVerticalDayHeaderDayNumberRect := cxNullRect;
  if Orientation = dhoVertical then
  begin
    AlignVert := vaTop;
    FTextRect := cxRectOffset(cxRectInflate(FTextRect, -ScaleFactor.Apply(dxDPITextOffset), 0),
      0, ScaleFactor.Apply(dxDPIDoubleTextOffset));
    FYearTextRect := TextRect;
    FTextRect.Bottom := FTextRect.Top + AgendaViewInfo.FVerticalDayHeaderTextHeight;
    FTextRect.Right := FTextRect.Left + AgendaViewInfo.FVerticalDayHeaderDayNumberWidth;
    FTextRect := cxRectOffsetVert(FTextRect, FBounds.Top - FTextRect.Top);
    FYearTextRect.Left := FTextRect.Right + ScaleFactor.Apply(dxDPITripleTextOffset);
    FYearTextRect.Right := FYearTextRect.Left + AgendaViewInfo.FVerticalDayHeaderYearCaptionWidth;
    FDayNameRect := cxRectOffsetVert(FYearTextRect, AgendaViewInfo.DayHeaderLineHeight + ScaleFactor.Apply(dxDPITextOffset));
    AMax := Max(Bounds.Top + AgendaViewInfo.DayHeaderDefaultHeight,
      Max(FTextRect.Bottom, FDayNameRect.Bottom) + ScaleFactor.Apply(dxDPITextOffset));

    FVerticalDayHeaderDayNumberRect := TextRect;
    if Length(FDayNum) < 2 then
      Inc(FVerticalDayHeaderDayNumberRect.Left, AgendaViewInfo.FDayHeaderDigitWidth);
    FVerticalDayHeaderDayNumberRect.Right := FYearTextRect.Left;
  end;
  FBounds.Bottom := AMax;
  if Orientation = dhoHorizontal then
  begin
    DY := (FBounds.Bottom - FTextRect.Bottom) div 2;
    FTextRect := cxRectOffsetVert(FTextRect, DY);
    FYearTextRect := cxRectOffsetVert(FYearTextRect, DY);
    FYearTextRect.Left := FYearTextRect.Right - AgendaViewInfo.FHorizontalDayHeaderYearTextWidth;
    if ANeedShiftYearCaption then
      Dec(FYearTextRect.Left, AgendaViewInfo.FDayHeaderDigitWidth div 2);
  end
end;

function TcxSchedulerAgendaViewDayHeaderCellViewInfo.CanDrawYearTextOnTheRight: Boolean;
begin
  Result := (Orientation = dhoHorizontal) and
    (cxTextWidth(AgendaViewInfo.GetCurrentFont, FDisplayText) < cxRectWidth(TextRect));
end;

function TcxSchedulerAgendaViewDayHeaderCellViewInfo.ConvertDateToDisplayText(AType: Integer = 0): Integer;

  function InternalExtractMonth(const ADayAsStr: string): string;
  begin
    Result := ADayAsStr;
    Delete(Result, Pos(AnsiUpperCase(FDayName), AnsiUpperCase(Result)), Length(FDayName));
    Delete(Result, Pos(FDayNum, Result), Length(FDayNum));
    Result := ReplaceText(Result, ' ', '');
    Result := ReplaceText(Result, ',', '');
  end;

begin
  FYearText := IntToStr(YearOf(DateTime));
  FDayName := GetLongDayName(DayOfWeek(DateTime));
  FDisplayText := TcxSchedulerDateTimeHelper.DayToStr(DateTime, 0, False);
  FDayNum := IntToStr(DayOf(DateTime));
  if Pos('0' + FDayNum, FDisplayText) > 0 then
    FDayNum := '0' + FDayNum;
  FMonthName := InternalExtractMonth(FDisplayText);
  FDisplayText := FDisplayText + ', ' + FYearText;
  if Pos(AnsiUpperCase(FDayName), AnsiUpperCase(FDisplayText)) = 0 then
    FDisplayText := FDayName + ', ' + FDisplayText;
  Result := 0;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.CorrectBoundsAsTopHeader;
var
  DY: Integer;
begin
  StoreRealAreas;
  if Bounds.Top < 0 then
    if Orientation = dhoHorizontal then
      DoScroll(0, -Bounds.Top)
    else
    begin
      DY := -FBounds.Top;
      if FDayNameRect.Bottom + ScaleFactor.Apply(dxDPITripleTextOffset) + DY > FBounds.Bottom then
        DY := FBounds.Bottom - FDayNameRect.Bottom - ScaleFactor.Apply(dxDPITripleTextOffset);
      OffsetRect(FTextRect, 0, DY);
      OffsetRect(FYearTextRect, 0, DY);
      OffsetRect(FDayNameRect, 0, DY);
      OffsetRect(FVerticalDayHeaderDayNumberRect, 0, DY);
      FBounds.Top := 0;
      CalculateCellBounds(Bounds, VisibleRect);
    end;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.CorrectBoundsForVerticalOrientation(
  ACurrentDayLastCell: TcxSchedulerCustomViewInfoItem);
begin
  if AgendaViewInfo.Scheduler.OptionsView.EventHeight > 0 then
    FBounds.Bottom := ACurrentDayLastCell.Bounds.Bottom
  else
  begin
    FBounds.Bottom := Max(FBounds.Bottom, ACurrentDayLastCell.Bounds.Bottom);
    TcxSchedulerCustomViewInfoItemAccess(ACurrentDayLastCell).FBounds.Bottom := FBounds.Bottom;
  end;
  StoreRealAreas;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.DrawCaption(ACanvas: TcxCanvas = nil);

  procedure InternalPrepareRealTexts(ACanDrawYearTextOnTheRight: Boolean);
  begin
    if (Orientation = dhoHorizontal) and (not ACanDrawYearTextOnTheRight) then
      Exit;
    if Orientation = dhoHorizontal then
      Delete(FDisplayText, Length(FDisplayText) - 5, 6)
    else
    begin
      FDisplayText := FDayNum;
      FYearText := FMonthName + ', ' + FYearText;
    end;
  end;

const
  AAlignVert: array[Boolean] of TcxAlignmentVert = (vaTop, vaCenter);
  AAlignHorz: array[Boolean] of TAlignment = (taLeftJustify, taRightJustify);
var
  AFontSize: Integer;
  AFontStyle: TFontStyles;
  AFont: TFont;
  AOriginalDisplayText, AOriginalYearText: string;
  ACanDrawYearTextOnTheRight: Boolean;
begin
  ACanDrawYearTextOnTheRight := CanDrawYearTextOnTheRight;
  AOriginalDisplayText := FDisplayText;
  AOriginalYearText := FYearText;
  try
    InternalPrepareRealTexts(ACanDrawYearTextOnTheRight);
    if Orientation = dhoHorizontal then
      inherited DrawCaption(Canvas)
    else
    begin
      AgendaViewInfo.StoreCanvasFontParams(AFontSize, AFontStyle);
      AFont := AgendaViewInfo.GetCurrentFont;
      AFont.Size := AgendaViewInfo.FVerticalDayHeaderDayNumberFontSize;
      Canvas.SaveClipRegion;
      Canvas.IntersectClipRect(FVerticalDayHeaderDayNumberRect);
      try
        Canvas.Font.Color := TextColor;
        Canvas.DrawTexT(DisplayText, FVerticalDayHeaderDayNumberRect, GetTextOutcxFlags);
      finally
        Canvas.RestoreClipRegion;
        AgendaViewInfo.RestoreCanvasFontParams(AFontSize, AFontStyle);
      end;
    end;
    if (Orientation = dhoVertical) or ACanDrawYearTextOnTheRight then
      Canvas.DrawTexT(FYearText, FYearTextRect,
        MakeTextOutcxFlags(AAlignHorz[UseRightToLeftAlignment], AAlignVert[Orientation = dhoHorizontal], AutoHeight));
    if Orientation = dhoVertical then
      Canvas.DrawTexT(FDayName, FDayNameRect,
        MakeTextOutcxFlags(AAlignHorz[UseRightToLeftAlignment], vaTop, AutoHeight));
  finally
    FDisplayText := AOriginalDisplayText;
    FYearText := AOriginalYearText;
  end;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.DrawHorizontalHeader;
var
  ABounds: TRect;
begin
  ABounds := Bounds;
  try
    if (Painter.LookAndFeelStyle = lfsNative) and IsWinVistaOrLater then
      Inc(FBounds.Bottom);
    inherited DrawHorizontalHeader;
  finally
    FBounds := ABounds;
  end;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  DoRightToLeftConversionWithChecking(FYearTextRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FDayNameRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FVerticalDayHeaderDayNumberRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FRealBounds, AClientBounds);
  DoRightToLeftConversionWithChecking(FRealTextRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FRealYearTextRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FRealDayNameRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FRealVerticalDayHeaderDayNumberRect, AClientBounds);
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.DoScroll(const DX, DY: Integer);
begin
  OffsetRect(FBounds, DX, DY);
  OffsetRect(FTextRect, DX, DY);
  OffsetRect(FYearTextRect, DX, DY);
  if Orientation = dhoVertical then
  begin
    OffsetRect(FDayNameRect, DX, DY);
    OffsetRect(FVerticalDayHeaderDayNumberRect, DX, DY);
  end;
  CalculateCellBounds(Bounds, VisibleRect);
end;

function TcxSchedulerAgendaViewDayHeaderCellViewInfo.GetOrientation: TcxSchedulerAgendaViewDayHeaderOrientation;
begin
  Result := AgendaViewInfo.DayHeaderOrientation;
end;

class function TcxSchedulerAgendaViewDayHeaderCellViewInfo.GetLongDayName(const ADayNum: Integer): string;
begin
  Result := dxFormatSettings.LongDayNames[ADayNum];
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.Scroll(const DX, DY: Integer);
begin
  RestoreRealAreas;
  DoScroll(DX, DY);
  StoreRealAreas;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.RestoreRealAreas;
begin
  FBounds := FRealBounds;
  FTextRect := FRealTextRect;
  FYearTextRect := FRealYearTextRect;
  FDayNameRect := FRealDayNameRect;
  FVerticalDayHeaderDayNumberRect := FRealVerticalDayHeaderDayNumberRect;
end;

procedure TcxSchedulerAgendaViewDayHeaderCellViewInfo.StoreRealAreas;
begin
  FRealBounds := FBounds;
  FRealTextRect := FTextRect;
  FRealYearTextRect := FYearTextRect;
  FRealDayNameRect := FDayNameRect;
  FRealVerticalDayHeaderDayNumberRect := FVerticalDayHeaderDayNumberRect;
end;

{ TcxSchedulerAgendaViewFreeDayCellViewInfo }

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.Calculate;
var
  dH: Integer;
begin
  if AgendaView.DayHeaderOrientation = dhoVertical then
  begin
    dH := cxRectHeight(DayHeader.Bounds) - cxRectHeight(Bounds);
    Inc(FClipRect.Bottom, dH);
    Inc(FBounds.Bottom, dH);
  end;
  FCaption := cxGetResourceString(@scxEmptyDayCaption);
  FCaptionRect := Bounds;
  FCaptionRect.Left := AgendaViewInfo.EventCellLabelRectPattern.Left;
  CalculateCellBounds(Bounds, VisibleRect);
end;

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.DoDraw;
begin
  Borders := [bBottom];
  Canvas.SaveClipRegion;
  try
    Canvas.IntersectClipRect(FClipRect);
    DrawRect(Bounds, Borders, AgendaView.LookAndFeelPainter.DefaultGridLineColor);
    if Focused then
      ExternalPainter.DrawCellFocus(Self, AgendaViewInfo.GetEventCellTextColor(Selected));
    DrawCaption;
  finally
    Canvas.RestoreClipRegion ;
  end;
end;

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  DoRightToLeftConversionWithChecking(FCaptionRect, AClientBounds);
end;

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.DrawCaption;
const
  AHorizontalAlignment: array[Boolean] of Cardinal = (cxAlignLeft, cxAlignRight);
begin
  Canvas.IntersectClipRect(FCaptionRect);
  Canvas.Font.Color := FViewParams.TextColor;
  Canvas.DrawText(FCaption, FCaptionRect, AHorizontalAlignment[UseRightToLeftAlignment] or cxAlignVCenter or cxShowEndEllipsis);
end;

function TcxSchedulerAgendaViewFreeDayCellViewInfo.GetAgendaView: TcxSchedulerAgendaView;
begin
  Result := DayHeader.AgendaViewInfo.AgendaView;
end;

function TcxSchedulerAgendaViewFreeDayCellViewInfo.GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := DayHeader.AgendaViewInfo;
end;

function TcxSchedulerAgendaViewFreeDayCellViewInfo.GetFocused: Boolean;
begin
  Result := Self = AgendaView.Controller.FocusedCell;
end;

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.InitializeDayHeader(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo);
begin
  FDayHeader := ADayHeader;
  FDateTime := FDayHeader.DateTime;
end;

procedure TcxSchedulerAgendaViewFreeDayCellViewInfo.Scroll(const DX, DY: Integer);
begin
  OffsetRect(FBounds, DX, DY);
  OffsetRect(FCaptionRect, DX, DY);
  FreeAndNil(FClipRgn);
  CalculateCellBounds(Bounds, VisibleRect);
end;

function TcxSchedulerAgendaViewFreeDayCellViewInfo.GetSelected: Boolean;
begin
  Result := AgendaViewInfo.Controller.FreeDaysSelection.IndexOf(DateTime) > -1;
end;

{ TcxSchedulerAgendaViewAdapter }

procedure TcxSchedulerAgendaViewAdapter.GetPageResources(AResources: TcxObjectList);
var
  ACount, I: Integer;
begin
  if Resources = nil then Exit;
  ACount := Resources.VisibleResourceCount;
  for I := 0 to ACount - 1 do
    AResources.Add(TcxSchedulerResourceViewInfo.Create(Resources.VisibleResources[I]));
end;

{ TcxSchedulerAgendaViewEventCellViewInfo }

function TcxSchedulerAgendaViewEventCellViewInfo.HintTriggerArea: TRect;
begin
  Result := FCaptionRect;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTimeLineLayout;
begin
  FEventTimeRect := cxRectSetTop(AgendaViewInfo.EventCellStateRectPattern, Bounds.Top);
  FEventTimeRect.Top := Bounds.Top + 1;
  FEventTimeRect.Bottom := Bounds.Bottom - 2;
  FTimeLineRect := cxRectOffsetHorz(FEventTimeRect, 1);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateLabelRect;
begin
  FLabelRect := cxRectOffsetVert(AgendaViewInfo.EventCellLabelRectPattern, FContentTop);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateLocationRect;
begin
  inherited CalculateLocationRect;
  if AgendaView.ShowLocations then
    FLocationRect := cxRectSetTop(AgendaViewInfo.EventCellLocationRectPattern, FContentTop);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTextItemsLayout;

  function InternalGetImagesBounds: TRect;
  begin
    Result := cxRect(Bounds.Left, Bounds.Top + cxEventBorderWidth,
      (Bounds.Left + Bounds.Right - ScaleFactor.Apply(cxTextOffset)) div 2, Bounds.Bottom -
        (ScaleFactor.Apply(cxEventImagesOffset) + cxEventBorderWidth));
  end;

var
  AOffsetHorz, AOffsetVert, AImagesWidth: Integer;
begin
  FCaptionRect := cxRectSetTop(AgendaViewInfo.EventCellCaptionRectPattern, FContentTop);
  CalculateLocationRect;
  FResourceText := CreateResourceText;
  if AgendaView.ShowResources then
    FResourceRect := cxRectSetTop(AgendaViewInfo.EventCellResourceRectPattern, FContentTop);
  if FShowMessage then
  begin
    AOffsetVert := ScaleFactor.Apply(dxDPIEventCellContentVerticalOffset);
    FMessageRect := cxRectSetTop(FCaptionRect, FCaptionRect.Top + GetCaptionHeight + AOffsetVert, 1);
    InitializeEditViewInfo(FMessageRect);
  end;
  if Images.Count > 0 then
  begin
    AOffsetHorz := ScaleFactor.Apply(dxDPITripleTextOffset);
    AOffsetVert := FLabelRect.Top - Bounds.Top - ScaleFactor.Apply(dxDPITextOffset);
    AImagesWidth := TcxSchedulerEventImagesAccess(Images).Calculate(InternalGetImagesBounds);
    TcxSchedulerEventImagesAccess(Images).Offset(FCaptionRect.Left - Bounds.Left, AOffsetVert);
    Inc(FCaptionRect.Left, AImagesWidth + AOffsetHorz);
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateActualEditRect(AEditViewData: TcxCustomEditViewData; var AMessageRect: TRect);
var
  ASize: TSize;
  AProperties: TcxEditSizeProperties;
begin
  AProperties.MaxLineCount := 0;
  AProperties.Height := -1;
  AProperties.Width := cxRectWidth(AMessageRect);
  ASize := AEditViewData.GetEditSize(Canvas, Message, AProperties);
  AMessageRect.Bottom := AMessageRect.Top + ASize.cy;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetActualCellFinish: TDateTime;
begin
  Result := Min(Event.Finish, FDateTime + 1);
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetActualCellStart: TDateTime;
begin
  Result := Max(Event.Start, FDateTime);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.InitializeEditViewInfo(var AMessageRect: TRect);
begin
  inherited InitializeEditViewInfo(AMessageRect);
  Dec(FEditViewInfo.Top, FEditViewInfo.ClientRect.Top + 1);
  Dec(FEditViewInfo.Left, FEditViewInfo.ClientRect.Left + 1);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTimeCaption;
begin
  DefineTimeAreaArrows;
  if ViewData.ShowTimeAsClock then
    Exit;

  case FTimeAreaArrows of
    avtaContinueArrowsBoth:
      FStartText := cxGetResourceString(@secxAllDay);
    avtaContinueArrowStart:
      if Event.Finish = FDateTime + 1 then
        FStartText := cxGetResourceString(@secxAllDay)
      else
        FStartText := DateTimeHelper.TimeToStr(Event.Finish);
    avtaContinueArrowEnd:
      if Event.Start = FDateTime then
        FStartText := cxGetResourceString(@secxAllDay)
      else
        FStartText := DateTimeHelper.TimeToStr(Event.Start);
  else
    if Event.AllDayEvent then
      FStartText := cxGetResourceString(@secxAllDay)
    else
      FStartText := Format(cxGetResourceString(@secxTimeRange),
        [DateTimeHelper.TimeToStr(Event.Start), DateTimeHelper.TimeToStr(Event.Finish)]);
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTimeArea;
begin
  CalculateTimeCaption;
  FTimeAreaRect := cxRectSetTop(AgendaViewInfo.EventCellTimeAreaPattern, FContentTop);
  if ViewData.ShowTimeAsClock then
    CalculateTimeRectsAsClockView
  else
    CalculateTimeRectsAsTextView;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTimeRectsAsClockView;
var
  AClockSize: TSize;
  ASizeStart, ASizeEnd: TSize;
begin
  AClockSize.cx := TcxSchedulerPainterHelper.IconsWidth(ScaleFactor);
  AClockSize.cy := TcxSchedulerPainterHelper.IconsHeight(ScaleFactor);
  ASizeStart := ScaleFactor.Apply(TcxSchedulerPainterHelper.GetStartContinueArrowSize);
  ASizeEnd := ScaleFactor.Apply(TcxSchedulerPainterHelper.GetEndContinueArrowSize);
  case FTimeAreaArrows of
    avtaNone:
    begin
      FStartRect := cxRectSetSize(FTimeAreaRect, AClockSize);
      FFinishRect := cxRectOffset(FStartRect, FStartRect.Right - FStartRect.Left + 2 * ScaleFactor.Apply(cxTextOffset), 0);
      cxRectCenterVerticallyRelativelyOfArea(FStartRect, FTimeAreaRect);
      cxRectCenterVerticallyRelativelyOfArea(FFinishRect, FTimeAreaRect);
    end;

    avtaContinueArrowStart:
    begin
      FStartContinueArrowRect := cxRectSetSize(FTimeAreaRect, ASizeStart);
      FFinishRect := cxRectOffset(cxRectSetSize(FTimeAreaRect, AClockSize),
        FStartContinueArrowRect.Right - FStartContinueArrowRect.Left + 2 * ScaleFactor.Apply(cxTextOffset), 0);
      cxRectCenterVerticallyRelativelyOfArea(FStartContinueArrowRect, FTimeAreaRect);
    end;

    avtaContinueArrowEnd:
    begin
      FStartRect := cxRectSetSize(FTimeAreaRect, AClockSize);
      FEndContinueArrowRect := cxRectOffset(cxRectSetSize(FStartRect, ASizeEnd),
        FStartRect.Right - FStartRect.Left + 2 * ScaleFactor.Apply(cxTextOffset), 0);
      cxRectCenterVerticallyRelativelyOfArea(FStartRect, FTimeAreaRect);
      cxRectCenterVerticallyRelativelyOfArea(FEndContinueArrowRect, FTimeAreaRect);
    end

  else
    begin
      FStartContinueArrowRect := cxRectSetSize(FTimeAreaRect, ASizeStart);
      FStartRect := cxRectOffset(cxRectSetSize(FTimeAreaRect, AClockSize),
        FStartContinueArrowRect.Right - FStartContinueArrowRect.Left + 2 * ScaleFactor.Apply(cxTextOffset), 0);
      FEndContinueArrowRect := cxRectOffset(cxRectSetSize(FStartRect, ASizeEnd),
        FStartRect.Right - FStartRect.Left + 2 * ScaleFactor.Apply(cxTextOffset), 0);
      cxRectCenterVerticallyRelativelyOfArea(FStartRect, FTimeAreaRect);
      cxRectCenterVerticallyRelativelyOfArea(FStartContinueArrowRect, FTimeAreaRect);
      cxRectCenterVerticallyRelativelyOfArea(FEndContinueArrowRect, FTimeAreaRect);
    end;
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateTimeRectsAsTextView;
var
  ASizeStart, ASizeEnd: TSize;
begin
  FTimeTextRect := FTimeAreaRect;
  ASizeStart := ScaleFactor.Apply(TcxSchedulerPainterHelper.GetStartContinueArrowSize);
  ASizeEnd := ScaleFactor.Apply(TcxSchedulerPainterHelper.GetEndContinueArrowSize);
  case FTimeAreaArrows of
    avtaContinueArrowsBoth:
    begin
      FStartContinueArrowRect := cxRectSetSize(FTimeTextRect, ASizeStart);
      FTimeTextRect.Left := FStartContinueArrowRect.Right + 2 * ScaleFactor.Apply(cxTextOffset);
      FTimeTextRect.Right := FTimeTextRect.Left + AgendaViewInfo.AllDayCaptionWidth;
      FEndContinueArrowRect := cxRectSetHeight(cxRectSetWidth(FTimeTextRect, FTimeTextRect.Right, ASizeEnd.cx), ASizeEnd.cy);
      cxRectCenterVerticallyRelativelyOfArea(FStartContinueArrowRect, FTimeAreaRect);
      cxRectCenterVerticallyRelativelyOfArea(FEndContinueArrowRect, FTimeAreaRect);
    end;
    avtaContinueArrowStart:
    begin
      FStartContinueArrowRect := cxRectSetSize(FTimeTextRect, ASizeStart);
      FTimeTextRect := cxRectSetWidth(FTimeTextRect, FStartContinueArrowRect.Right + ScaleFactor.Apply(dxDPITextOffset),
        cxTextWidth(Canvas.Font, FStartText) + ScaleFactor.Apply(dxDPITextOffset));
      cxRectCenterVerticallyRelativelyOfArea(FStartContinueArrowRect, FTimeAreaRect);
    end;
    avtaContinueArrowEnd:
    begin
      FTimeTextRect := cxRectSetWidth(FTimeTextRect, cxTextWidth(Canvas.Font, FStartText) + ScaleFactor.Apply(dxDPIDoubleTextOffset));
      FEndContinueArrowRect := cxRectSetHeight(cxRectSetWidth(FTimeTextRect, FTimeTextRect.Right, ASizeEnd.cx), ASizeEnd.cy);
      cxRectCenterVerticallyRelativelyOfArea(FEndContinueArrowRect, FTimeAreaRect);
    end;
  end;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.CreateResourceText: string;

  function GetSimpleResourceCaption(const AResourceID: Variant): string;
  var
    I: Integer;
    AInfo: TcxSchedulerResourceViewInfo;
  begin
    Result := '';
    for I := 0 to AgendaViewInfo.ResourceCount - 1 do
    begin
      AInfo := AgendaViewInfo.Resources[I];
      if (VarCompare(AInfo.ResourceItem.ResourceID, AResourceID) = 0) and AInfo.ResourceItem.Visible then
      begin
        Result := AInfo.Caption;
        Break;
      end;
    end;
  end;

var
  I, ALow, AHigh: Integer;
  ADelimiter, ACaption: string;
begin
  Result := '';
  if not IsResourcePresent then Exit;
  if VarIsArray(ResourceID) then
  begin
    ADelimiter := dxFormatSettings.ListSeparator + ' ';
    ALow := VarArrayLowBound(ResourceID, 1);
    AHigh := VarArrayHighBound(ResourceID, 1);
    for I := ALow to AHigh do
    begin
      ACaption := GetSimpleResourceCaption(ResourceID[I]);
      if ACaption <> '' then
      begin
        if Result <> '' then
          Result := Result + ADelimiter + ACaption
        else
          Result := ACaption;
      end;
    end;
  end
  else
    Result := GetSimpleResourceCaption(ResourceID);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateEventTimeVisibility;
begin
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetCaptionHeight: Integer;
begin
  Result := cxTextHeight(Canvas.Font, ViewData.Caption);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateItemsLayout;
begin
  FContentTop := Bounds.Top + ScaleFactor.Apply(dxDPIEventCellContentVerticalOffset);
  CalculateTimeArea;
  CalculateLabelRect;
  CalculateTextItemsLayout;
  CorrectContentRects;
  CalculateTimeLineLayout;
  CalculateCellBounds(Bounds, ViewData.VisibleRect);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CorrectContentRects;
var
  dH: Integer;
  R: TRect;
begin
  dH := 0;
  R := MessageRect;
  if cxRectIsNull(MessageRect) then
  begin
    R := CaptionRect;
    R.Bottom := R.Bottom + ScaleFactor.Apply(dxDPIEventCellContentVerticalOffset);
  end;
  if not cxRectIsNull(R) then
    dH := R.Bottom - Bounds.Bottom
  else
    if (AgendaView.DayHeaderOrientation = dhoVertical) and IsOnlyEventInDay then
      dH := cxRectHeight(DayHeader.Bounds) - cxRectHeight(Bounds);

  if dH > 0 then
  begin
    Inc(FClipRect.Bottom, dH);
    Inc(FEventTimeRect.Bottom, dH);
    Inc(FBounds.Bottom, dH);
    FCaptionRect.Bottom := FBounds.Bottom;
  end;

  if AgendaView.Scheduler.OptionsView.EventHeight > 0  then
  begin
    dH := AgendaView.Scheduler.OptionsView.EventHeight - cxRectHeight(Bounds);
    Inc(FClipRect.Bottom, dH);
    Inc(FEventTimeRect.Bottom, dH);
    Inc(FBounds.Bottom, dH);
    FCaptionRect.Bottom := FBounds.Bottom;
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.CalculateShowTimeAsClock;
begin
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DefineTimeAreaArrows;
begin
  FDateTime := ViewData.CurrentDate;
  FTimeAreaArrows := avtaNone;
  if Event.AllDayEvent or ((Event.Start = FDateTime) and (Event.Finish = FDateTime + 1)) then Exit;

  if Event.Start < FDateTime then
    if Event.Finish > FDateTime + 1 then
      FTimeAreaArrows := avtaContinueArrowsBoth
    else
      FTimeAreaArrows := avtaContinueArrowStart
  else

    if Event.Finish > FDateTime + 1 then
      FTimeAreaArrows := avtaContinueArrowEnd;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  DoRightToLeftConversionWithChecking(FLabelRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FResourceRect, AClientBounds);
  DoRightToLeftConversionWithChecking(FTimeTextRect, AClientBounds);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawCloneEvent;
begin
  Canvas.Rectangle(Bounds, FViewParams, cxBordersAll, FViewParams.TextColor);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawContent;
var
  ASavedColor: TColor;
  ABitmap: TcxBitmap;
  ABounds: TRect;
begin
  if not Event.IsClone then
  begin
    Borders := [bBottom];
    ASavedColor := Canvas.Font.Color;
    try
      Canvas.Font.Color := FViewParams.TextColor;
      inherited DrawContent;
      DrawState;
      DrawLabel;
      DrawResource;
      if Focused and not FBlended then
        ExternalPainter.DrawCellFocus(Self, AgendaViewInfo.GetEventCellTextColor(Selected));
      if FBlended then
      begin
        ABitmap := TcxBitmap.CreateSize(Bounds, pf32bit);
        try
          ABounds := Bounds;
          Dec(ABounds.Top);
          cxBitBlt(ABitmap.Canvas.Handle, Canvas.Handle, ABitmap.ClientRect, ABounds.TopLeft, SRCCOPY);
          cxAlphaBlend(ABitmap, cxRectOffset(ABounds, -ABounds.Left, -ABounds.Top), ColorToRgb(clWhite), 128);
          cxBitBlt(Canvas.Handle, ABitmap.Canvas.Handle, ABounds, cxNullPoint, SRCCOPY);
        finally
          ABitmap.Free;
        end;
      end;
    finally
      Canvas.Font.Color := ASavedColor;
    end;
  end
  else
    if not FHidden then
      DrawCloneEvent;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawContinueArrowsAndCaptions;
begin
  if FTimeAreaArrows in [avtaContinueArrowStart, avtaContinueArrowsBoth] then
    DoDrawStartContinueArrow;
  if FTimeAreaArrows in [avtaContinueArrowEnd, avtaContinueArrowsBoth] then
    DoDrawEndContinueArrow;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawLabel;
var
  AColor: TColor;
begin
  if Event.LabelColor <> clDefault then
    AColor := Event.LabelColor
  else
    AColor := Painter.DefaultSchedulerEventColorClassic(False);
  ExternalPainter.DrawEventLabel(Self, FLabelRect, AColor);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawLocation;
const
  AHorizontalAlignment: array[Boolean] of Word = (CXTO_LEFT, CXTO_RIGHT);
begin
  if AgendaView.ShowLocations and (Event.Location <> '') and (cxRectWidth(LocationRect) > 0) then
    cxTextOut(Canvas.Canvas, Event.Location, FLocationRect,
      AHorizontalAlignment[UseRightToLeftAlignment] or CXTO_TOP or CXTO_SINGLELINE or CXTO_END_ELLIPSIS,
      0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, Canvas.Font.Color);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawMessageSeparator;
begin
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawResource;
const
  AHorizontalAlignment: array[Boolean] of Word = (CXTO_LEFT, CXTO_RIGHT);
var
  ATextColor: TColor;
  AStyle: TcxStyle;
begin
  if IsResourcePresent and (cxRectWidth(FResourceRect) > 0) then
  begin
    ATextColor := Canvas.Font.Color;
    if not Selected then
    begin
      AStyle := AgendaView.Styles.ResourceHeader;
      if (AStyle <> nil) and (cxStyles.svTextColor in AStyle.AssignedValues) and (AStyle.TextColor <> clDefault) then
        ATextColor := AStyle.TextColor;
    end;
    cxTextOut(Canvas.Canvas, ResourceText, FResourceRect,
      AHorizontalAlignment[UseRightToLeftAlignment] or CXTO_TOP or CXTO_SINGLELINE or CXTO_END_ELLIPSIS,
      0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ATextColor);
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawState;
begin
  PainterHelper.DrawState(Canvas, FEventTimeRect, Event.State, [bLeft, bRight],  clNone);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawTime;
begin
  if ViewData.ShowTimeAsClock then
    DrawTimeAsClock
  else
    DrawTimeAsText;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawTimeAsClock;
var
  AStart: TDateTime;
begin
  if FTimeAreaArrows <> avtaContinueArrowStart then
  begin
    AStart := Event.Start;
    if FTimeAreaArrows = avtaContinueArrowsBoth then
      AStart := 0;
    PainterHelper.DrawClock(Canvas, FStartRect, AStart, FViewParams, ScaleFactor)
  end;

  if FTimeAreaArrows in [avtaNone, avtaContinueArrowStart] then
    PainterHelper.DrawClock(Canvas, FFinishRect, Event.Finish - 1/24/60/60, FViewParams, ScaleFactor);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.DrawTimeAsText;
const
  AHorizontalAlignment: array[Boolean] of Cardinal = (cxAlignLeft, cxAlignRight);
begin
  DrawText(FTimeTextRect, FStartText, AHorizontalAlignment[UseRightToLeftAlignment] or cxAlignTop or cxSingleLine);
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetAgendaView: TcxSchedulerAgendaView;
begin
  Result := DayHeader.AgendaViewInfo.AgendaView;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetAgendaViewInfo: TcxSchedulerAgendaViewViewInfo;
begin
  Result := DayHeader.AgendaViewInfo;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetCaptionFontStyle: TFontStyles;
begin
  Result := inherited GetCaptionFontStyle + [fsBold];
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetDetailCaptionFlagValue: Boolean;
begin
  Result := True;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetDetailInfoFlagValue: Boolean;
begin
  Result := True;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetDrawCaptionFlags: Cardinal;
const
  AHorizontalAlignment: array[Boolean] of Cardinal = (cxAlignLeft, cxAlignRight);
begin
  Result := AHorizontalAlignment[UseRightToLeftAlignment] or cxAlignTop or cxShowEndEllipsis;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetEditStyleColor: TColor;
begin
  Result := ViewData.ViewParams.Color;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetFocused: Boolean;
begin
  Result := Self = AgendaView.Controller.FocusedCell;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  TcxSchedulerAgendaViewHitTest(AHitTest).StoreHitEventCellParameters(Self);
end;

function TcxSchedulerAgendaViewEventCellViewInfo.GetResourceID: Variant;
begin
  Result := Event.ResourceID;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.IsNeedDrawTime: Boolean;
begin
  Result := True;
end;

function TcxSchedulerAgendaViewEventCellViewInfo.IsResourcePresent: Boolean;
begin
  Result := AgendaViewInfo.IsResourcePresent(ResourceID);
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.InitializeServiceFields(ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
  const AIsOnlyEventInDay: Boolean);
begin
  FDayHeader := ADayHeader;
  FIsOnlyEventInDay := AIsOnlyEventInDay;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.MeasureCaptionExtent(var R: TRect);
var
  ASavedStyle: TFontStyles;
begin
  ASavedStyle := Canvas.Font.Style;
  try
    Canvas.Font.Style := GetCaptionFontStyle;
    Canvas.TextExtent(Hint, R, GetDrawCaptionFlags xor cxShowEndEllipsis);
  finally
    Canvas.Font.Style := ASavedStyle;
  end;
end;

procedure TcxSchedulerAgendaViewEventCellViewInfo.Scroll(const DX, DY: Integer);
begin
  OffsetRect(FBounds, DX, DY);
  OffsetRect(FEventTimeRect, DX, DY);
  OffsetRect(FTimeLineRect, DX, DY);
  OffsetRect(FTimeTextRect, DX, DY);
  OffsetRect(FCaptionRect, DX, DY);
  OffsetRect(FLabelRect, DX, DY);
  if FShowMessage then
  begin
    OffsetRect(FMessageRect, DX, DY);
    FEditViewInfo.Left := FEditViewInfo.Left + DX;
    FEditViewInfo.Top := FEditViewInfo.Top + DY;
  end;
  ShiftRect(FLocationRect, DX, DY);
  if IsResourcePresent then
    OffsetRect(FResourceRect, DX, DY);
  ShiftRect(FStartContinueArrowRect, DX, DY);
  ShiftRect(FEndContinueArrowRect, DX, DY);
  ShiftRect(FStartRect, DX, DY);
  ShiftRect(FFinishRect, DX, DY);
  TcxSchedulerEventImagesAccess(Images).Offset(DX, DY);
  FreeAndNil(FClipRgn);
  CalculateCellBounds(Bounds, ViewData.VisibleRect);
end;

{ TcxSchedulerAgendaViewEventCellModernViewInfo }

procedure TcxSchedulerAgendaViewEventCellModernViewInfo.CalculateTimeLineLayout;
begin
  inherited CalculateTimeLineLayout;
  FEventTimeRect := cxRectInflate(FEventTimeRect, -1, -1);
end;

procedure TcxSchedulerAgendaViewEventCellModernViewInfo.DrawState;
begin
  DrawState(Canvas, FTimeLineRect, [], GetTimeLineBorderColor);
end;

function TcxSchedulerAgendaViewEventCellModernViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsModern;
end;

function TcxSchedulerAgendaViewEventCellModernViewInfo.HintTriggerArea: TRect;
begin
  Result := ClipRect;
end;

{ TcxSchedulerAgendaViewViewInfo }

constructor TcxSchedulerAgendaViewViewInfo.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FActualVisibleDays := TcxSchedulerDateList.Create;
  FInfoCells := TcxObjectList.Create(False);
  FCloneEventCells := TcxSchedulerEventCellViewInfoList.Create;
end;

destructor TcxSchedulerAgendaViewViewInfo.Destroy;
begin
  FActualVisibleDays.Free;
  FCloneEventCells.Free;
  FInfoCells.Free;
  inherited Destroy;
end;

function TcxSchedulerAgendaViewViewInfo.AddDayHeaderInfo(const ADate: TDateTime): TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  CreateCellInstance(TcxSchedulerAgendaViewDayHeaderCellViewInfo, cxRectOffsetVert(FDayHeaderBoundsPattern, FNextTop),
    StylesAdapter.GetDayHeaderParams(ADate), UseRightToLeftAlignment, Result);
  Result.FAgendaViewInfo := Self;
  AssignResourceID(Result, -1);
  Result.DateTime := ADate;
  Result.Calculate('');
  DayHeaderCells.Add(Result);
  if DayHeaderOrientation = dhoHorizontal then
    FNextTop := Result.Bounds.Bottom;
end;

function TcxSchedulerAgendaViewViewInfo.AddFreeDayInfo(
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo): TcxSchedulerAgendaViewFreeDayCellViewInfo;
var
  AIsSelected: Boolean;
begin
  CreateCellInstance(TcxSchedulerAgendaViewFreeDayCellViewInfo, cxRectOffsetVert(FFreeDayBoundsPattern, FNextTop),
    StylesAdapter.GetEventParams(nil), UseRightToLeftAlignment, Result);
  Result.InitializeDayHeader(ADayHeader);
  CheckAsNewFocusedCell(Result);
  AIsSelected := Result.Selected;
  Result.FViewParams.Color := GetEventCellColor(AIsSelected);
  Result.FViewParams.TextColor := GetEventCellTextColor(AIsSelected);
  if DayHeaderOrientation = dhoHorizontal then
    Result.FVisibleRect.Top := cxRectHeight(ADayHeader.Bounds);
  Result.Calculate;
  ContentCells.Add(Result);
  FNextTop := Result.Bounds.Bottom;
  FInfoCells.Add(Result);
end;

function TcxSchedulerAgendaViewViewInfo.AddEventInfo(AEvent: TcxSchedulerControlEvent;
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
  const AIsOnlyEventInDay, AInFreeDay: Boolean): TcxSchedulerAgendaViewEventCellViewInfo;

  function NeedCorrectCloneEventBounds: Boolean;
  begin
    Result := AInFreeDay or (
      (AEvent.Source = Controller.DragEvent) and (DragEventHelper <> nil) and
      (Int(DragEventHelper.DestinationTime) = ADayHeader.DateTime) and DragEventHelper.NeedDockToNextCell);
  end;

var
  AViewData: TcxSchedulerEventViewData;
  ADate: TDateTime;
  AIsSelected: Boolean;
  ABounds: TRect;
begin
  ADate := ADayHeader.DateTime;
  ABounds := FEventCellBoundsPattern;
  if AEvent.IsClone then
  begin
    ABounds.Bottom := ABounds.Top + 1;
    Dec(ABounds.Top, dxSchedulerAgendaViewCloneEventHeight - 1);
    if NeedCorrectCloneEventBounds then
      ABounds := cxRectOffsetVert(ABounds, 2);
    AViewData := CreateEventViewData(AEvent, cxRectOffsetVert(ABounds, FNextTop),
      GetEventStart(AEvent, ADate), GetEventFinish(AEvent, ADate), nil, ADate);
    AViewData.ViewParams.Color := LookAndFeelPainter.DefaultContentColor;
    AViewData.ViewParams.TextColor := LookAndFeelPainter.DefaultContentTextColor;
    Result := CreateEventCellViewInfo(AViewData, False) as TcxSchedulerAgendaViewEventCellViewInfo;
    Result.FDayHeader := ADayHeader;
    CloneEventCells.Add(Result);
    Exit;
  end;
  if AIsOnlyEventInDay and (DayHeaderOrientation = dhoVertical) then
    ABounds.Bottom := ABounds.Top + cxRectHeight(ADayHeader.Bounds);
  AViewData := CreateEventViewData(AEvent, cxRectOffsetVert(ABounds, FNextTop),
    GetEventStart(AEvent, ADate), GetEventFinish(AEvent, ADate), nil, ADate);
  AViewData.BorderColor := LookAndFeelPainter.DefaultGridLineColor;
  if DayHeaderOrientation = dhoHorizontal then
    AViewData.VisibleRect.Top := cxRectHeight(ADayHeader.Bounds);
  Result := AddEventCell(AViewData, False) as TcxSchedulerAgendaViewEventCellViewInfo;
  Result.InitializeServiceFields(ADayHeader, AIsOnlyEventInDay);
  if not Controller.InDraggingProcess then
    CheckAsNewFocusedCell(Result);
  if Controller.NavigationButtonClicked and (Result = Controller.FocusedCell) and (Events.Clones.Count = 0) then
    Controller.SilentAddEventSelection(AEvent);
  AIsSelected := AEvent.Selected;
  AViewData.ViewParams.Color := GetEventCellColor(AIsSelected);
  AViewData.ViewParams.TextColor := GetEventCellTextColor(AIsSelected);
  AViewData.Caption := DoGetEventDisplayText(AEvent);
  Result.Calculate;
  FNextTop := Result.Bounds.Bottom;
  FInfoCells.Add(Result);
end;

function TcxSchedulerAgendaViewViewInfo.AreDisplayedAnyEvents: Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to EventCells.Count - 1 do
  begin
    Result := cxRectIntersect(EventCells[I].Bounds,
      TcxSchedulerAgendaViewEventCellViewInfo(EventCells[I]).ViewData.VisibleRect);
    if Result then Break;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.AreThereEventsInVisibleInterval(AResourceIndex: Integer): Boolean;
begin
  Result := AreDisplayedAnyEvents;
end;

function cxCompareAgendaViewEvents(AEvent1, AEvent2: TcxSchedulerControlEvent): Integer;
var
  AAllDay1, AAllDay2: Boolean;
begin
  AAllDay1 := AEvent1.AllDayEvent;
  AAllDay2 := AEvent2.AllDayEvent;
  Result := Byte(AAllDay2) - Byte(AAllDay1);
  if (Result <> 0) or AAllDay1 then
  begin
    if Result = 0 then
      Result := AEvent1.Index - AEvent2.Index;
  end
  else
    if AEvent1.Start < AEvent2.Start then
      Result := -1
    else
      if AEvent1.Start > AEvent2.Start then
        Result := 1
      else
        if AEvent1.Finish > AEvent2.Finish then
          Result := -1
        else
          if AEvent1.Finish < AEvent2.Finish then
            Result := 1
          else
            Result := AEvent1.Index - AEvent2.Index;
end;

procedure TcxSchedulerAgendaViewViewInfo.Calculate;
begin
  inherited Calculate;
  if AgendaView.IsSelectedDaysChanged and (AgendaView.DisplayMode <> avmAllDays) then
    MakeFocusedCellVisible;
  AgendaView.ResetIsSelectedDaysChanged;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited CalculateHitTest(AHitTest);
  if TcxSchedulerAgendaViewHitTest(AHitTest).Flags = 1 shl htcControl then
    TcxSchedulerAgendaViewHitTest(AHitTest).SetHitTime(htcTime, Controller.GetLastInfoCellTime);
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateContentNavigationButtons;
begin
  if CanCalculateNavigationButtons then
    AddContentNavigationButton(Bounds, -1, cprSingle);
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateDayHeaderPatterns;
begin
  FDayHeaderBoundsPattern := cxRectSetTop(Bounds, 0, DayHeaderDefaultHeight);
  if DayHeaderOrientation = dhoVertical then
    FDayHeaderBoundsPattern.Right := FDayHeaderBoundsPattern.Left + FVerticalDayHeaderWidth;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateEventCellPatterns;
begin
  FEventCellBoundsPattern := cxRectSetTop(Bounds, 0, CellDefaultHeight);
  if DayHeaderOrientation = dhoVertical then
    FEventCellBoundsPattern.Left := FDayHeaderBoundsPattern.Right;
  CalculateEventCellStateRectPattern;
  CalculateEventCellTimeAreaPattern;
  CalculateEventCellLabelRectPattern;
  CalculateEventCellTextItemsPatterns;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateFreeDayPattern;
begin
  FFreeDayBoundsPattern := FEventCellBoundsPattern;
  FFreeDayBoundsPattern.Bottom := FFreeDayBoundsPattern.Top + FreeDayDefaultHeight;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateEventCellStateRectPattern;
begin
  FEventCellStateRectPattern := cxRectSetLeft(FEventCellBoundsPattern,
    FEventCellBoundsPattern.Left, ScaleFactor.Apply(cxTimeLineWidth));
  Dec(FEventCellStateRectPattern.Right, ScaleFactor.Apply(2));
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateEventCellTimeAreaPattern;
var
  ASize1, ASize2: TSize;
  APattern: string;
  AWidth: Integer;
begin
  ASize1 := TcxSchedulerPainterHelper.GetStartContinueArrowSize;
  ASize2 := TcxSchedulerPainterHelper.GetEndContinueArrowSize;
  APattern := TcxSchedulerDateTimeHelper.TimeToStr(0.95);
  AWidth := Max(TcxSchedulerPainterHelper.IconsWidth(ScaleFactor) * 3 + ScaleFactor.Apply(dxDPIFourfoldTextOffset),
    Max(cxTextWidth(StylesAdapter.GetEventParams(nil).Font, Format(cxGetResourceString(@secxTimeRange),
    [APattern, APattern])) + ScaleFactor.Apply(dxDPIDoubleTextOffset), AllDayCaptionWidth + ASize1.cx + ASize2.cx));
  FEventCellTimeAreaPattern := cxRectSetLeft(FEventCellBoundsPattern,
    EventCellStateRectPattern.Right + ScaleFactor.Apply(5), AWidth);
  FEventCellTimeAreaPattern.Bottom := FEventCellTimeAreaPattern.Top + FEventCellLineHeight;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateEventCellLabelRectPattern;
var
  ASize: TSize;
begin
  ASize := AgendaView.ExternalPainter.GetEventLabelSize(ScaleFactor);
  FEventCellLabelRectPattern := cxRectSetWidth(FEventCellBoundsPattern,
    EventCellTimeAreaPattern.Right + ScaleFactor.Apply(10), ASize.cx);
  if ASize.cy < cxRectHeight(FEventCellTimeAreaPattern) then
    FEventCellLabelRectPattern.Top :=
      (FEventCellTimeAreaPattern.Top + FEventCellTimeAreaPattern.Bottom) div 2 - ASize.cy div 2;
  FEventCellLabelRectPattern.Bottom := FEventCellLabelRectPattern.Top + ASize.cy
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateEventCellTextItemsPatterns;
var
  AOffsetHorz, ASmallWidth, ABase, ALeft2, ALeft3: Integer;
begin
  AOffsetHorz := ScaleFactor.Apply(dxDPITripleTextOffset);
  FEventCellCaptionRectPattern := cxRectSetLeft(FEventCellBoundsPattern, EventCellLabelRectPattern.Right + AOffsetHorz * 2);
  FEventCellCaptionRectPattern.Bottom := FEventCellTimeAreaPattern.Bottom;
  FEventCellCaptionRectPattern.Right := FEventCellBoundsPattern.Right;
  if AgendaView.ShowLocations or AgendaView.ShowResources then
  begin
    ASmallWidth := Max((FEventCellBoundsPattern.Right - FEventCellCaptionRectPattern.Left) div 5 - AOffsetHorz, 10);
    ABase := Max(FEventCellCaptionRectPattern.Left + AgendaView.EventTextMinWidth,
      FEventCellBoundsPattern.Right - 2 * (AOffsetHorz + ASmallWidth));
    ALeft2 := ABase + AOffsetHorz;
    ALeft3 := ALeft2 + ASmallWidth + AOffsetHorz;
    if AgendaView.ShowLocations then
    begin
      FEventCellLocationRectPattern := cxRectSetWidth(FEventCellBoundsPattern, ALeft2, ASmallWidth);
      FEventCellLocationRectPattern.Bottom := FEventCellTimeAreaPattern.Bottom;
      if not AgendaView.ShowResources then
      begin
        FEventCellLocationRectPattern.Left := ALeft3;
        FEventCellLocationRectPattern.Right := FEventCellBoundsPattern.Right;
      end;
      FEventCellCaptionRectPattern.Right := FEventCellLocationRectPattern.Left - AOffsetHorz;
    end;
    if AgendaView.ShowResources then
    begin
      FEventCellResourceRectPattern := cxRectSetWidth(FEventCellBoundsPattern, ALeft3, ASmallWidth);
      FEventCellResourceRectPattern.Right := FEventCellBoundsPattern.Right;
      FEventCellResourceRectPattern.Bottom := FEventCellTimeAreaPattern.Bottom;
      if not AgendaView.ShowLocations then
        FEventCellCaptionRectPattern.Right := FEventCellResourceRectPattern.Left - AOffsetHorz;
    end;
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateItemsLayoutInfo;
var
  I, J: Integer;
  AList: TcxSchedulerEventList;
  ACount: Integer;
  ADate: TDateTime;
  AHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
  AEventCell, ALastEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
  AFreeDayInfo: TcxSchedulerAgendaViewFreeDayCellViewInfo;
  AIsVerticalDayHeaders, AIsOnlyEventInDay, AIsFreeDay: Boolean;
begin
  AIsVerticalDayHeaders := DayHeaderOrientation = dhoVertical;
  AList := TcxSchedulerEventList.Create;
  try
    for I := 0 to ActualVisibleDays.Count - 1 do
    begin
      AList.Clear;
      ADate := Trunc(ActualVisibleDays[I]);
      ExtractEvents(Events, AList, ADate);
      AIsOnlyEventInDay := AList.Count = 1;
      AIsFreeDay := AList.Count = 0;
      ExtractCloneEvents(AList, ADate);
      ACount := AList.Count;
      if (ACount > 0) or ((Scheduler.Storage <> nil) and (AgendaView.DisplayMode <> avmSelectedNonEmptyDays)) then
      begin
        AHeader := AddDayHeaderInfo(ADate);
        ALastEventCell := nil;
        if ACount > 0 then
        begin
          AList.Sort(@cxCompareAgendaViewEvents);
          for J := 0 to ACount - 1 do
          begin
            AEventCell := AddEventInfo(TcxSchedulerControlEvent(AList[J]), AHeader, AIsOnlyEventInDay, AIsFreeDay);
            if not AEventCell.Event.IsClone then
              ALastEventCell := AEventCell;
          end;
          if AIsVerticalDayHeaders and (ALastEventCell <> nil) then
          begin
            AHeader.CorrectBoundsForVerticalOrientation(ALastEventCell);
            FNextTop := AHeader.Bounds.Bottom;
          end;
        end;
        if AIsFreeDay then
        begin
          AFreeDayInfo := AddFreeDayInfo(AHeader);
          if AIsVerticalDayHeaders then
          begin
            AHeader.CorrectBoundsForVerticalOrientation(AFreeDayInfo);
            FNextTop := AHeader.Bounds.Bottom;
          end;
        end;
        if AIsVerticalDayHeaders then
          AHeader.FClipRect := AHeader.CalculateClipRect(AHeader.Bounds, Bounds);
      end;
    end;
  finally
    AList.Free;
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateMetrics;
var
  AFont: TFont;
begin
  inherited CalculateMetrics;
  AFont := TFont.Create;
  try
    AFont.Assign(StylesAdapter.GetEventParams(nil).Font);
    FEventCellLineHeight := cxTextHeight(AFont);
    FFreeDayDefaultHeight := FEventCellLineHeight +
      2 * ScaleFactor.Apply(cxTextOffset + dxDPIEventCellContentVerticalOffset);
    FAllDayCaptionWidth := cxTextWidth(AFont, cxGetResourceString(@secxAllDay)) + ScaleFactor.Apply(dxDPIDoubleTextOffset);
    if Scheduler.OptionsView.EventHeight > 0 then
      FCellDefaultHeight := Scheduler.OptionsView.EventHeight
    else
      FCellDefaultHeight := FFreeDayDefaultHeight;
    FDayHeaderDefaultHeight := FFreeDayDefaultHeight;

    AFont.Assign(StylesAdapter.GetDayHeaderParams(Now).Font);
    FDayHeaderLineHeight := cxTextHeight(AFont);
    FDayHeaderDigitWidth := cxTextWidth(AFont, '0');
    FHorizontalDayHeaderYearTextWidth := cxTextWidth(AFont, '2000');
    if DayHeaderOrientation = dhoVertical then
      CalculateMetricsForVerticalDayHeader(AFont);
  finally
    FreeAndNil(AFont);
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculateMetricsForVerticalDayHeader(AFont: TFont);

   function InternalGetYearTextMaxLength(AFont: TFont): Integer;
   const
     AYear = 2000;
   var
     AMonth: Integer;
   begin
     Result := 0;
     for AMonth := 1 to 12 do
       Result := Max(Result, cxTextWidth(AFont,
         dxFormatSettings.LongMonthNames[MonthOf(EncodeDate(AYear, AMonth, 1))] + ', 2000'));
   end;

   function InternalGetDayNameMaxLength(AFont: TFont): Integer;
   var
     ADay: Integer;
   begin
    Result := 0;
    for ADay := 1 to 7 do
      Result := Max(Result, cxTextWidth(AFont, TcxSchedulerAgendaViewDayHeaderCellViewInfo.GetLongDayName(ADay)));
   end;

var
  AIsItalic: Boolean;
  AYearTextMaxLength, ADayNameMaxText: Integer;
begin
  AIsItalic := fsItalic in AFont.Style;
  AYearTextMaxLength := InternalGetYearTextMaxLength(AFont);
  ADayNameMaxText := InternalGetDayNameMaxLength(AFont);
  if AIsItalic then
  begin
    Inc(AYearTextMaxLength, cxTextWidth(AFont, '0'));
    Inc(ADayNameMaxText, cxTextWidth(AFont, '0'));
  end;
  FVerticalDayHeaderYearCaptionWidth := Max(AYearTextMaxLength, ADayNameMaxText);
  MeasureVerticalDayHeaderDayNumberFontSize(2 * FDayHeaderLineHeight + ScaleFactor.Apply(dxDPIQuintupleTextOffset), AFont);
  FVerticalDayHeaderTextHeight := cxTextHeight(AFont);
  FDayHeaderDigitWidth := cxTextWidth(AFont, '0');
  if AIsItalic then
    FVerticalDayHeaderDayNumberWidth := cxTextWidth(AFont, '000') - FDayHeaderDigitWidth div 2
  else
    FVerticalDayHeaderDayNumberWidth := cxTextWidth(AFont, '00');
  FVerticalDayHeaderWidth := 3 * ScaleFactor.Apply(dxDPITripleTextOffset) + FVerticalDayHeaderDayNumberWidth +
    FVerticalDayHeaderYearCaptionWidth;
end;

procedure TcxSchedulerAgendaViewViewInfo.CalculatePatterns;
begin
  CalculateDayHeaderPatterns;
  CalculateEventCellPatterns;
  CalculateFreeDayPattern;
end;

procedure TcxSchedulerAgendaViewViewInfo.CheckScrollPositions;
begin
  if InfoCells.Count = 0 then
  begin
    FTopScrollPos := 0;
    FNextTop := 0;
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.CheckVisibleDayHeaders;
var
  I: Integer;
  ACell: TcxSchedulerCustomViewInfoItem;
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  FTopVisibleDayHeader := nil;
  for I := 0 to InfoCells.Count - 1 do
  begin
    ACell := InfoCell[I];
    if IsTopVisibleCell(ACell) then
    begin
      FTopVisibleDayHeader := GetDayHeader(ACell);
      Break;
    end;
  end;
  if FTopVisibleDayHeader = nil then
    for I := 0 to DayHeaderCells.Count - 1 do
    begin
      ADayHeader := TcxSchedulerAgendaViewDayHeaderCellViewInfo(DayHeaderCells[I]);
      if ADayHeader.Bounds.Top >= 0 then
      begin
        FTopVisibleDayHeader := ADayHeader;
        Break;
      end;
    end;
  CorrectTopVisibleHeaderBounds;
  FBottomVisibleDayHeader := GetBottomVisibleDayHeader;
end;

procedure TcxSchedulerAgendaViewViewInfo.Clear;
begin
  FInfoCells.Clear;
  FCloneEventCells.Clear;
  FNearestEventDateBeforeSelection := -1;
  if Controller.FIsDeletingOfFocusedEvent then
    Controller.FocusedCell := nil;
  inherited Clear;
end;

procedure TcxSchedulerAgendaViewViewInfo.CorrectTopVisibleHeaderBounds;
begin
  if TopVisibleDayHeader = nil then
    Exit;
  TopVisibleDayHeader.CorrectBoundsAsTopHeader;
  if (DayHeaderOrientation = dhoHorizontal) and (TopVisibleDayHeader.Bounds.Top > 0) then
    PutPriorTopVisibleDayHeaderAboveCurrent;
end;

procedure TcxSchedulerAgendaViewViewInfo.DoCalculate;
begin
  if Controller.FDragEventStoredSelection.Count > 0 then
    Controller.RestoreEventSelectionFromDragEventStoredSelection;
  inherited DoCalculate;
  CalculatePatterns;
  PopulateActualVisibleDays;
  FNextTop := -TopScrollPos;
  Controller.FFocusedCell := nil;
  CalculateItemsLayoutInfo;
  if IsScrollBarsParametersWasChanged then
  begin
    Clear;
    DoCalculate;
    Exit;
  end;
  CheckScrollPositions;
  FContentHeight := FNextTop + TopScrollPos;
  if Controller.FIsDeletingOfFocusedEvent and (TopScrollPos > 0) and (FNextTop < Bounds.Bottom) then
    Scroll(FNextTop - Bounds.Bottom);
  if InfoCells.Count > 0 then
    if AgendaView.DisplayMode = avmAllDays then
      MakeFirstSelectedDayAsTop
    else
      CheckVisibleDayHeaders;
  CalculateContentNavigationButtons;
end;

procedure TcxSchedulerAgendaViewViewInfo.DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo);
begin
  SelectedDays.ShiftPeriod(Sender.Interval);
  inherited DoContentNavigationButtonClick(Sender);
end;

procedure TcxSchedulerAgendaViewViewInfo.DoRightToLeftConversion(const AClientBounds: TRect);
begin
  inherited DoRightToLeftConversion(AClientBounds);
  TcxSchedulerViewInfoCellListAccess(CloneEventCells).DoRightToLeftConversion(AClientBounds);
end;

procedure TcxSchedulerAgendaViewViewInfo.ExtractEvents(ASource, ADest: TcxSchedulerEventList; ADate: TDateTime);
var
  ADateWithoutTime: Integer;
  AEvent: TcxSchedulerEvent;
  I: Integer;
begin
  ADateWithoutTime := Trunc(ADate);
  for I := 0 to ASource.Count - 1 do
  begin
    AEvent := TcxSchedulerEvent(ASource.List.List[I]);
    if AEvent.IsDayEvent(ADateWithoutTime) then
      ADest.Add(AEvent);
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.ExtractCloneEvents(ADest: TcxSchedulerEventList; ADate: TDateTime);
var
  ADateWithoutTime: Integer;
  AEvent: TcxSchedulerEvent;
  I: Integer;
begin
  if (Events.Clones.Count = 0) or not Controller.HitTest.HitAtControl or (Controller.FCurrentDragState = dsDragLeave) then
    Exit;
  ADateWithoutTime := Trunc(ADate);
  for I := 0 to Events.Clones.Count - 1 do
  begin
    AEvent := TcxSchedulerEvent(Events.Clones[I]);
    if AEvent.IsDayEvent(ADateWithoutTime) then
      ADest.Add(AEvent);
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetActualSelectedDayFinish: TDateTime;
begin
  if ActualVisibleDays.Count > 0 then
    Result := ActualVisibleDays[ActualVisibleDays.Count - 1]
  else
    Result := AgendaView.GetFirstVisibleDate;
end;

function TcxSchedulerAgendaViewViewInfo.GetActualVisibleDayStart: TDateTime;
begin
  if ActualVisibleDays.Count > 0 then
    Result := ActualVisibleDays[0]
  else
    Result := AgendaView.GetFirstVisibleDate;
end;

function TcxSchedulerAgendaViewViewInfo.GetAgendaView: TcxSchedulerAgendaView;
begin
  Result := TcxSchedulerAgendaView(Owner);
end;

function TcxSchedulerAgendaViewViewInfo.GetBottomVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
var
  I: Integer;
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  Result := nil;
  for I := DayHeaderCells.Count - 1 downto 0 do
  begin
    ADayHeader := TcxSchedulerAgendaViewDayHeaderCellViewInfo(DayHeaderCells[I]);
    if ADayHeader.Bounds.Top < Bounds.Bottom then
    begin
      Result := ADayHeader;
      Break;
    end;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetCellDate(ACell: TcxSchedulerCustomViewInfoItem): TDate;
begin
  if ACell = nil then
    Result := -1
  else
    Result := GetDayHeader(ACell).DateTime;
end;

function TcxSchedulerAgendaViewViewInfo.GetController: TcxSchedulerAgendaViewController;
begin
  Result := AgendaView.Controller;
end;

function TcxSchedulerAgendaViewViewInfo.GetCurrentFont: TFont;
begin
  Result := AgendaView.Canvas.Font;
end;

function TcxSchedulerAgendaViewViewInfo.GetDayContentHeight(const ADate: TDate): Integer;
var
  I, AIndexBeg, AIndexEnd: Integer;
  ABegCell: TcxSchedulerCustomViewInfoItem;
begin
  Result := 0;
  AIndexBeg := -1;
  AIndexEnd := -1;
  for I := 0 to InfoCells.Count - 1 do
    if GetCellDate(InfoCell[I]) = ADate then

      if AIndexBeg = -1 then
      begin
        AIndexBeg := I;
        AIndexEnd := I;
      end
      else
        AIndexEnd := I

    else
      if AIndexEnd <> -1 then
        Break;

  if AIndexBeg > -1 then
  begin
    ABegCell := InfoCell[AIndexBeg];
    Result := InfoCell[AIndexEnd].Bounds.Bottom - ABegCell.Bounds.Top;
    if DayHeaderOrientation = dhoHorizontal then
      Inc(Result, cxRectHeight(GetDayHeader(ABegCell).Bounds));
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetDayHeader(ACell: TcxSchedulerCustomViewInfoItem): TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
    Result := TcxSchedulerAgendaViewEventCellViewInfo(ACell).DayHeader
  else
    Result := TcxSchedulerAgendaViewFreeDayCellViewInfo(ACell).DayHeader;
end;

function TcxSchedulerAgendaViewViewInfo.GetDayHeaderOrientation: TcxSchedulerAgendaViewDayHeaderOrientation;
begin
  Result := AgendaView.DayHeaderOrientation;
end;

function TcxSchedulerAgendaViewViewInfo.GetDragEventHelper: TcxAgendaViewDragEventHelper;
begin
  Result := Controller.DragEventHelper as TcxAgendaViewDragEventHelper;
end;

function TcxSchedulerAgendaViewViewInfo.GetEventCellColor(const AIsSelected: Boolean): TColor;
var
  AStyle: TcxStyle;
begin
  Result := clDefault;
  if AIsSelected then
  begin
    AStyle := Scheduler.Styles.Selection;
    if (AStyle <> nil) and (cxStyles.svColor in AStyle.AssignedValues) then
      Result := AStyle.Color;
    if Result = clDefault then
      Result := LookAndFeelPainter.DefaultSelectionColor
  end
  else
  begin
    AStyle := Scheduler.Styles.Event;
    if (AStyle <> nil) and (cxStyles.svColor in AStyle.AssignedValues) then
      Result := AStyle.Color;
    if Result = clDefault then
      Result := LookAndFeelPainter.DefaultContentColor;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetEventCellTextColor(const AIsSelected: Boolean): TColor;
var
  AStyle: TcxStyle;
begin
  Result := clDefault;
  if AIsSelected then
  begin
    AStyle := Scheduler.Styles.Selection;
    if (AStyle <> nil) and (cxStyles.svTextColor in AStyle.AssignedValues) then
      Result := AStyle.TextColor;
    if Result = clDefault then
      Result := LookAndFeelPainter.DefaultSelectionTextColor;
  end
  else
  begin
    AStyle := Scheduler.Styles.Event;
    if (AStyle <> nil) and (cxStyles.svTextColor in AStyle.AssignedValues) then
      Result := AStyle.TextColor;
    if Result = clDefault then
      Result := LookAndFeelPainter.DefaultContentTextColor;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerEventCellViewInfoClass =
    (TcxSchedulerAgendaViewEventCellViewInfo, TcxSchedulerAgendaViewEventCellModernViewInfo);
begin
  Result := AClass[SchedulerStyle = svsModern];
end;

function TcxSchedulerAgendaViewViewInfo.GetEventHint(AEvent: TcxSchedulerControlEvent): string;
begin
  Result := AEvent.Caption;
  TcxCustomSchedulerAccess(Scheduler).GetEventUserHintText(AEvent, Result);
end;

function TcxSchedulerAgendaViewViewInfo.GetEventStart(AEvent: TcxSchedulerControlEvent; ADate: TDateTime): TDateTime;
begin
  Result := Max(Int(ADate), AEvent.Start);
end;

function TcxSchedulerAgendaViewViewInfo.GetEventFinish(AEvent: TcxSchedulerControlEvent; ADate: TDateTime): TDateTime;
begin
  Result := Min(Int(ADate + 1), AEvent.Finish);
end;

function TcxSchedulerAgendaViewViewInfo.GetInfoCell(AIndex: Integer): TcxSchedulerCustomViewInfoItem;
begin
  Result := TcxSchedulerCustomViewInfoItem(FInfoCells[AIndex]);
end;

function TcxSchedulerAgendaViewViewInfo.GetInfoCell(AEvent: TcxSchedulerControlEvent;
  const ADate: TDateTime): TcxSchedulerCustomViewInfoItem;
var
  I: Integer;
  ACell: TcxSchedulerCustomViewInfoItem;
begin
  Result := nil;
  if AEvent = nil then
    Exit;
  for I := 0 to InfoCells.Count - 1 do
  begin
    ACell := InfoCell[I];
    if (ACell is TcxSchedulerAgendaViewEventCellViewInfo) and
       (AEvent = TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event) and
       (ADate = TcxSchedulerAgendaViewEventCellViewInfo(ACell).FDateTime) then
    begin
      Result := ACell;
      Break;
    end;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetTopScrollPosPreliminaryValue(ACell: TcxSchedulerCustomViewInfoItem): Integer;
begin
  Result := GetTopScrollPosToDisplayCell(ACell);
  CheckNewTopScrollPos(Result);
end;

function TcxSchedulerAgendaViewViewInfo.GetTopScrollPosToDisplayCell(ACell: TcxSchedulerCustomViewInfoItem): Integer;
var
  AVisibleTop: Integer;
begin
  Result := TopScrollPos;
  if (ACell = nil) or (InfoCells.Count = 0) then
    Exit;
  if ACell = InfoCell[InfoCells.Count - 1] then
    Result := ContentHeight
  else
  begin
    AVisibleTop := 0;
    if DayHeaderOrientation = dhoHorizontal then
      AVisibleTop := cxRectHeight(GetDayHeader(ACell).Bounds);
    if ACell.Bounds.Top < AVisibleTop then
      Result := TopScrollPos - (AVisibleTop - ACell.Bounds.Top)
    else
      if ACell.Bounds.Bottom > Bounds.Bottom then
        Result := TopScrollPos + ACell.Bounds.Bottom - Bounds.Bottom;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetTopVisibleCell: TcxSchedulerCustomViewInfoItem;
var
  I: Integer;
  ACell: TcxSchedulerCustomViewInfoItem;
begin
  Result := nil;
  for I := 0 to InfoCells.Count - 1 do
  begin
    ACell := InfoCell[I];
    if IsTopVisibleCell(ACell) then
    begin
      Result := ACell;
      Break;
    end;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.GetVertScrollPage: Integer;
begin
  Result := cxRectHeight(Bounds);
end;

function TcxSchedulerAgendaViewViewInfo.GetResourceText(AEventResourceID: Variant): string;

  function GetSimpleResourceCaption(const AResourceID: Variant): string;
  var
    I: Integer;
    AInfo: TcxSchedulerResourceViewInfo;
  begin
    Result := '';
    for I := 0 to ResourceCount - 1 do
    begin
      AInfo := Resources[I];
      if (VarCompare(AInfo.ResourceItem.ResourceID, AResourceID) = 0) and AInfo.ResourceItem.Visible then
      begin
        Result := AInfo.Caption;
        Break;
      end;
    end;
  end;

const
  ADelimiter = '; ';
var
  I, ALow, AHigh: Integer;
begin
  Result := '';
  if not IsResourcePresent(AEventResourceID) then Exit;
  if VarIsArray(AEventResourceID) then
  begin
    ALow := VarArrayLowBound(AEventResourceID, 1);
    AHigh := VarArrayHighBound(AEventResourceID, 1);
    for I := ALow to AHigh do
      Result := Result + GetSimpleResourceCaption(AEventResourceID[I]) + ADelimiter;
    while (Length(Result) > 1) and (Copy(Result, Length(Result) - 1, 2) = ADelimiter) do
      Delete(Result, Length(Result) - 1, 2);
  end
  else
    Result := GetSimpleResourceCaption(AEventResourceID);
end;

procedure TcxSchedulerAgendaViewViewInfo.HideCloneEventsOnDragDrop;
var
  I: Integer;
begin
  for I := 0 to CloneEventCells.Count - 1 do
    CloneEventCells[I].Hidden := True;
end;

procedure TcxSchedulerAgendaViewViewInfo.HideSourceEventsOnDragDrop;
var
  I: Integer;
  AEventCell: TcxSchedulerAgendaViewEventCellViewInfo;
begin
  for I := 0 to EventCells.Count - 1 do
  begin
    AEventCell := EventCells[I] as TcxSchedulerAgendaViewEventCellViewInfo;
    if HasClone(AEventCell.Event) then
      AEventCell.FBlended := True;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.IsNavigationButtonsVertical: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerAgendaViewViewInfo.CheckAsNewFocusedCell(ACell: TcxSchedulerCustomViewInfoItem);
var
  AEvent: TcxSchedulerControlEvent;
  AIsNewFocusedCell: Boolean;
  AKey: TcxSchedulerControlEventID;
begin
  AEvent := nil;
  if ACell is TcxSchedulerAgendaViewEventCellViewInfo then
    AEvent := TcxSchedulerAgendaViewEventCellViewInfo(ACell).Event;
  AIsNewFocusedCell := (AEvent <> nil) and AEvent.IsClone and
    Controller.FStoredFocusedEventKey.SameEvent(AEvent.Source);
  if not AIsNewFocusedCell then
  begin
    AIsNewFocusedCell := GetCellDate(ACell) = Int(Controller.FStoredFocusedCellDate);
    if Controller.FIsDeletingOfFocusedEvent then
      AIsNewFocusedCell := AIsNewFocusedCell and (Controller.FocusedCell = nil);
    if not AIsNewFocusedCell then
      Exit;
    if AEvent <> nil then
    begin
      AIsNewFocusedCell := Controller.FIsDeletingOfFocusedEvent or (Controller.FStoredFocusedEventKey <> nil);
      if AIsNewFocusedCell then
      begin
        AKey := TcxSchedulerControlEventID.Create(AEvent);
        try
          AIsNewFocusedCell := cxCompareSelectionKeys(Controller.FStoredFocusedEventKey, AKey) = 0;
        finally
          AKey.Free;
        end;
      end;
    end
    else
      AIsNewFocusedCell := Controller.FIsDeletingOfFocusedEvent or (Controller.FStoredFocusedEventKey = nil);
  end;
  if AIsNewFocusedCell then
    Controller.SetCellAsFocused(ACell);
end;

function TcxSchedulerAgendaViewViewInfo.IsResourcePresent(AEventResourceID: Variant): Boolean;
begin
  Result := AgendaView.ShowResources and not(VarIsNull(AEventResourceID) or VarIsEmpty(AEventResourceID));
end;

function TcxSchedulerAgendaViewViewInfo.IsScrollBarsParametersWasChanged: Boolean;
var
  APos: Integer;
begin
  Result := False;
  if VertScrollPage + TopScrollPos > ContentHeight then
  begin
    APos := TopScrollPos;
    FTopScrollPos := Max(0, FContentHeight - VertScrollPage);
    Result := TopScrollPos <> APos;
  end;
end;

function TcxSchedulerAgendaViewViewInfo.IsTopVisibleCell(ACell: TcxSchedulerCustomViewInfoItem): Boolean;
var
  AHeaderHeight: Integer;
  ABounds: TRect;
begin
  ABounds := ACell.Bounds;
  if DayHeaderOrientation = dhoHorizontal then
  begin
    AHeaderHeight := cxRectHeight(GetDayHeader(ACell).Bounds);
    Result := (ABounds.Top <= AHeaderHeight) and (ABounds.Bottom > AHeaderHeight);
  end
  else
    Result := (ABounds.Top <= 0) and (ABounds.Bottom > 0);
end;

procedure TcxSchedulerAgendaViewViewInfo.MakeCellVisible(ACell: TcxSchedulerCustomViewInfoItem);
begin
  TopScrollPos := GetTopScrollPosToDisplayCell(ACell);
end;

procedure TcxSchedulerAgendaViewViewInfo.MakeFocusedCellVisible;
begin
  MakeCellVisible(Controller.FocusedCell);
end;

procedure TcxSchedulerAgendaViewViewInfo.MakeFirstSelectedDayAsTop;
var
  ADate: TDateTime;
  I, AHeaderHeight: Integer;
  ADayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
  ADayContentBounds: TRect;
begin
  ADate := SelectedDays[0];
  FTopVisibleDayHeader := nil;
  for I := 0 to DayHeaderCells.Count - 1 do
  begin
    ADayHeader := TcxSchedulerAgendaViewDayHeaderCellViewInfo(DayHeaderCells[I]);
    if ADayHeader.DateTime = ADate then
    begin
      FTopVisibleDayHeader := ADayHeader;
      Break;
    end;
  end;

  ADayContentBounds := cxRectSetHeight(TopVisibleDayHeader.Bounds, GetDayContentHeight(ADate));
  AHeaderHeight := cxRectHeight(TopVisibleDayHeader.Bounds);
  if (Controller.IsTopDayCanBeChangedInsideOfScrolling or
      Scheduler.Storage.EditingEventInfoList.IsLocked or not AgendaView.IsSelectedDaysChanged) and
    (((ADayContentBounds.Top < 0) and (ADayContentBounds.Bottom > 0)) or (InRange(ADayContentBounds.Top, 1, AHeaderHeight - 1))) then
  begin
    CorrectTopVisibleHeaderBounds;
    FBottomVisibleDayHeader := GetBottomVisibleDayHeader;
  end
  else
    DoScroll(FTopVisibleDayHeader.Bounds.Top);
end;

procedure TcxSchedulerAgendaViewViewInfo.MeasureVerticalDayHeaderDayNumberFontSize(const AAreaHeight: Integer; var AFont: TFont);
var
  AMaxSize, ASize, AHeight: Integer;
begin
  AMaxSize := Max(AFont.Size * 4, ScaleFactor.Apply(10));
  for ASize := AFont.Size * 2 to AMaxSize do
  begin
    AFont.Size := ASize;
    AHeight := cxTextHeight(AFont);
    if AHeight >= AAreaHeight then
      Break;
  end;
  FVerticalDayHeaderDayNumberFontSize := AFont.Size + 1;
end;

function TcxSchedulerAgendaViewViewInfo.NeedClearResources(AGroupingKind: TcxSchedulerGroupingKind): Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerAgendaViewViewInfo.OnContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo);
begin
  inherited OnContentNavigationButtonClick(Sender);
  Controller.DoFocusFirstEventCell(TopVisibleDayHeader);
end;

procedure TcxSchedulerAgendaViewViewInfo.PopulateActualVisibleDays;
var
  I: Integer;
  ADate, AStart, AFinish: TDateTime;
begin
  if AgendaView.DisplayMode <> avmAllDays then
  begin
    FActualVisibleDays.Clear;
    for I := 0 to SelectedDays.Count - 1 do
      FActualVisibleDays.Add(SelectedDays[I]);
  end
  else

  if not AgendaView.GetIsTrackingScroll then
  begin
    ADate := AgendaView.GetFirstVisibleDate;
    AStart := Max(Events.Start, ADate - AgendaView.GetVisibleDaysRange);
    AFinish := Min(Events.Finish, ADate + 3 * AgendaView.GetVisibleDaysRange);
    if (ActualVisibleDayStart - AStart > AgendaView.GetVisibleDaysRange div 2) or
       (AFinish - ActualSelectedDayFinish > AgendaView.GetVisibleDaysRange div 2) then
    begin
      FTopScrollPos := 0;
      FActualVisibleDays.Clear;
      for I := Trunc(AStart) to Trunc(AFinish) do
        FActualVisibleDays.Add(I);
    end;
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.PutPriorTopVisibleDayHeaderAboveCurrent;
var
  AIndex: Integer;
  AHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  AIndex := DayHeaderCells.IndexOf(TopVisibleDayHeader) - 1;
  if AIndex >= 0 then
  begin
    AHeader := DayHeaderCells[AIndex] as TcxSchedulerAgendaViewDayHeaderCellViewInfo;
    AHeader.StoreRealAreas;
    AHeader.DoScroll(0, -(AHeader.Bounds.Top + cxRectHeight(AHeader.Bounds) - TopVisibleDayHeader.Bounds.Top));
  end;
end;

procedure TcxSchedulerAgendaViewViewInfo.ResetTopScrollPos;
begin
  if (AgendaView.DisplayMode <> avmAllDays) and not Controller.FEventDeletingInProcess and
     ((Scheduler.Storage = nil) or not Scheduler.Storage.EditingEventInfoList.IsLocked) then
    FTopScrollPos := 0;
end;

procedure TcxSchedulerAgendaViewViewInfo.ReturnVisibleInterval(var AStart, AEnd: TDateTime);
begin
  if AgendaView.DisplayMode = avmAllDays then
  begin
    AStart := SelectedDays[0];
    AEnd := AStart;
  end
  else
    inherited ReturnVisibleInterval(AStart, AEnd);
end;

procedure TcxSchedulerAgendaViewViewInfo.DoScroll(const DY: Integer);
var
  I: Integer;
  AOldTopVisibleDayHeader: TcxSchedulerAgendaViewDayHeaderCellViewInfo;
begin
  Inc(FTopScrollPos, DY);
  for I := 0 to DayHeaderCells.Count - 1 do
    TcxSchedulerAgendaViewDayHeaderCellViewInfo(DayHeaderCells[I]).Scroll(0, -DY);
  for I := 0 to InfoCells.Count - 1 do
    if InfoCell[I] is TcxSchedulerAgendaViewEventCellViewInfo then
      TcxSchedulerAgendaViewEventCellViewInfo(InfoCell[I]).Scroll(0, -DY)
    else
      TcxSchedulerAgendaViewFreeDayCellViewInfo(InfoCell[I]).Scroll(0, -DY);
  AOldTopVisibleDayHeader := TopVisibleDayHeader;
  CheckVisibleDayHeaders;
  if (AOldTopVisibleDayHeader <> TopVisibleDayHeader) and (AgendaView.DisplayMode = avmAllDays) then
    AgendaView.DoAutoSelectTopDay(TopVisibleDayHeader.DateTime);
end;

procedure TcxSchedulerAgendaViewViewInfo.Scroll(const DY: Integer);
begin
  DoScroll(DY);
  AgendaView.HitTest.HitPoint := AgendaView.Scheduler.ScreenToClient(GetMouseCursorPos);
  AgendaView.Scheduler.Invalidate;
  TcxCustomSchedulerAccess(AgendaView.Scheduler).UpdateScrollBars;
end;

procedure TcxSchedulerAgendaViewViewInfo.SetContentNavigationButtonsIntervals;

  function GetPreviousEventDateInsideSelection(const ADate: TDateTime): TDateTime;
  var
    I: Integer;
  begin
    Result := MaxDateTime;
    if AgendaView.DisplayMode <> avmAllDays then
      for I := 0 to EventCells.Count - 1 do
        if TcxSchedulerAgendaViewEventCellViewInfo(EventCells[I]).FDateTime < ADate then
          Result := TcxSchedulerAgendaViewEventCellViewInfo(EventCells[I]).FDateTime
        else
          Break;
  end;

  function GetNextEventDateInsideSelection(const ADate: TDateTime): TDateTime;
  var
    I: Integer;
  begin
    Result := -MaxDateTime;
    if AgendaView.DisplayMode <> avmAllDays then
      for I := EventCells.Count - 1 downto 0 do
        if TcxSchedulerAgendaViewEventCellViewInfo(EventCells[I]).FDateTime > ADate then
          Result := TcxSchedulerAgendaViewEventCellViewInfo(EventCells[I]).FDateTime
        else
          Break;
  end;

  procedure InternalCheck(var APrevious, ANext: TDateTime);
  var
    ACalculator: TcxSchedulerContentNavigationCalculator;
    AStart, AFinish: TDateTime;
    AIntervals: TObjectList;
    ABefore: TDateTime;
  begin
    if (Abs(APrevious) <> MaxDateTime) and (Abs(ANext) <> MaxDateTime) then
      Exit;
    if FNearestEventDateBeforeSelection = -1 then
    begin
       ReturnVisibleInterval(AStart, AFinish);
       AIntervals := TObjectList.Create;
       try
         ACalculator := TcxSchedulerContentNavigationCalculator.Create;
         try
           AIntervals.Add(TcxSchedulerContentNavigationInfo.Create(0));
           ACalculator.FindNavigationIntervals(Scheduler.Storage, AIntervals, AStart, AFinish, True, False, GetScaleUnit);
         finally
           ACalculator.Free;
         end;

         if Abs(APrevious) = MaxDateTime then
         begin
           ABefore := TcxSchedulerContentNavigationInfo(AIntervals[0]).FIntervalBefore;
           FNearestEventDateBeforeSelection := AStart - Trunc(ABefore);
           if Frac(ABefore) <> 0 then
             FNearestEventDateBeforeSelection := FNearestEventDateBeforeSelection - 1;
         end
         else
           FNearestEventDateBeforeSelection := APrevious;

         if Abs(ANext) = MaxDateTime then
           FNearestEventDateAfterSelection := AFinish +
             Trunc(TcxSchedulerContentNavigationInfo(AIntervals[0]).FIntervalAfter)
         else
           FNearestEventDateAfterSelection := ANext;
       finally
         AIntervals.Clear;
         AIntervals.Free;
       end;
    end;
    APrevious := FNearestEventDateBeforeSelection;
    ANext := FNearestEventDateAfterSelection;
  end;

var
  APrevious, ANext: TDateTime;
  ANavigationButton: TcxSchedulerContentNavigationButtonViewInfo;
begin
  if NavigationButtons.Count = 0 then
    Exit;

  if AgendaView.DisplayMode = avmSelectedNonEmptyDays then
    inherited SetContentNavigationButtonsIntervals
  else
  if (NavigationButtons.Count > 0) and (TopVisibleDayHeader <> nil) then
  begin
    APrevious := GetPreviousEventDateInsideSelection(TopVisibleDayHeader.DateTime);
    ANext := GetNextEventDateInsideSelection(TopVisibleDayHeader.DateTime);
    InternalCheck(APrevious, ANext);

    ANavigationButton := TcxSchedulerContentNavigationButtonViewInfo(FNavigationButtons[0]);
    if Abs(APrevious) < MaxDateTime then
      ANavigationButton.Interval := APrevious - SelectedDays[0]
    else
      ANavigationButton.Interval := MaxDateTime;

    ANavigationButton := TcxSchedulerContentNavigationButtonViewInfo(FNavigationButtons[1]);
    if Abs(ANext) < MaxDateTime then
      ANavigationButton.Interval := ANext - SelectedDays[0]
    else
      ANavigationButton.Interval := MaxDateTime;
  end
end;

procedure TcxSchedulerAgendaViewViewInfo.TryAddContentNavigationButtonsAtScrolling;
begin
  NavigationButtons.Clear;
  CalculateContentNavigationButtons;
  SetContentNavigationButtonsIntervals;
end;

procedure TcxSchedulerAgendaViewViewInfo.CheckNewTopScrollPos(var AValue: Integer);
begin
  AValue := Max(0, Min(AValue, ContentHeight - VertScrollPage));
end;

procedure TcxSchedulerAgendaViewViewInfo.SetTopScrollPos(AValue: Integer);
begin
  CheckNewTopScrollPos(AValue);
  if AValue <> TopScrollPos then
    Scroll(AValue - TopScrollPos);
end;

procedure TcxSchedulerAgendaViewViewInfo.RestoreCanvasFontParams(const AFontSize: Integer; const AFontStyle: TFontStyles);
begin
  GetCurrentFont.Size := AFontSize;
  GetCurrentFont.Style := AFontStyle;
end;

procedure TcxSchedulerAgendaViewViewInfo.StoreCanvasFontParams(var AFontSize: Integer; var AFontStyle: TFontStyles);
begin
  AFontSize := GetCurrentFont.Size;
  AFontStyle := GetCurrentFont.Style;
end;

end.


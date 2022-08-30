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

unit cxSchedulerDayView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, DateUtils, Windows, Forms, Classes, SysUtils, Math, StdCtrls, Graphics, Controls, ExtCtrls, RTLConsts,
  cxClasses, dxCore, cxControls, cxGraphics, dxCoreGraphics, cxStyles, cxGeometry, Menus,
  cxLookAndFeelPainters, cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerUtils, cxSchedulerStorage, cxSchedulerStrs, cxEdit, dxTouch, dxCoreClasses;

const

  cxcsHeaderContainer         = 0;
  cxcsSelectedHeaderContainer = 1;
  cxcsTimeLine                = 2;
  cxcsTimeRuler               = 3;
  cxcsMaxDayViewValue         = cxcsTimeRuler;
  //for PS
  cxcsDayViewStyleFirst = cxcsHeaderContainer;
  cxcsDayViewStyleLast = cxcsMaxDayViewValue;

  cxSchedulerAllDayEventContainerMaxLineCount: Integer = 0;

type
  TcxSchedulerDayViewViewInfo = class;
  TcxEventLayoutInfo = class;
  TcxCalculateEventLayout = class;
  TcxSchedulerDayViewHitTest = class;
  TcxSchedulerDayViewController = class;
  TcxSchedulerTimeRulerPopupMenu = class;
  TcxSchedulerAllDayContainerGestureScrollHelper = class;

  TcxCalculateEventLayoutClass = class of TcxCalculateEventLayout;

  TcxSchedulerAllDayAreaScrollBar = (adsbDefault, adsbNever, adsbAlways);

  IcxSchedulerTimeRulerParams = interface
  ['{FEAA09ED-8FE1-4968-9BCB-0FC26B13A391}']
    function GetTimeRulerParams: TcxViewParams;
  end;

  { TcxSchedulerDayViewStyles }

  TcxSchedulerDayViewStyles = class(TcxStyles)
  private
    FScheduler: TcxCustomScheduler;
    function GetPainter: TcxCustomLookAndFeelPainter;
    function GetSchedulerViewStyle: TcxSchedulerViewStyle;
  protected
    procedure Changed(AIndex: Integer); override;
    procedure GetDefaultHeaderContainerViewParams(var AParams: TcxViewParams); virtual;
    procedure GetDefaultSelectedHeaderContainerViewParams(var AParams: TcxViewParams);
    procedure GetDefaultTimeRulerViewParams(var AParams: TcxViewParams); virtual;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject; out AParams: TcxViewParams); override;

    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
    property SchedulerViewStyle: TcxSchedulerViewStyle read GetSchedulerViewStyle;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetHeaderContainerParams(const ADate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem; ASelected: Boolean): TcxViewParams;
    function GetTimeRulerParams: TcxViewParams;
    function GetTimeLineParams: TcxViewParams;
    // define colors functions
    property Scheduler: TcxCustomScheduler read FScheduler;
  published
    property HeaderContainer: TcxStyle index cxcsHeaderContainer read GetValue write SetValue;
    property SelectedHeaderContainer: TcxStyle index cxcsSelectedHeaderContainer read GetValue write SetValue;
    property TimeLine: TcxStyle index cxcsTimeLine read GetValue write SetValue;
    property TimeRuler: TcxStyle index cxcsTimeRuler read GetValue write SetValue;
  end;

  { TcxSchedulerDayView }

  TcxSchedulerCustomDrawContainerEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; AViewInfo: TcxSchedulerContainerCellViewInfo; var ADone: Boolean) of object;
  TcxSchedulerCustomDrawTimeRulerEvent = procedure(Sender: TObject;
    ACanvas: TcxCanvas; AViewInfo: TcxSchedulerTimeRulerCellViewInfo; var ADone: Boolean) of object;

  TcxSchedulerDayView = class(TcxSchedulerCustomResourceView, IcxSchedulerViewTimeScaleStep,
    IdxTouchScrollUIOwner, IdxHybridScrollbarOwner)
  private
    FAllDayAreaScrollBar: TcxSchedulerAllDayAreaScrollBar;
    FAllDayContainerGestureScrollHelper: TcxSchedulerAllDayContainerGestureScrollHelper;
    FAllDayScrollBar: TdxScrollBarWrapper;
    FAlwaysShowEventTime: Boolean;
    FAutoContentHeight: Boolean;
    FDayHeaderArea: Boolean;
    FDayHeaderModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
    FEventShadows: Boolean;
    FHeaderContainer: Boolean;
    FHybridScrollbarsManager: TdxHybridScrollbarsManager;
    FShowAllDayEventsInContentArea: Boolean;
    FStyles: TcxSchedulerDayViewStyles;
    FTimeRulerMinutes: Boolean;
    FTimeRulerPopupMenu: TcxSchedulerTimeRulerPopupMenu;
    FTimeScale: Integer;
    FWorkTimeOnly: Boolean;
    FOnCustomDrawContainer: TcxSchedulerCustomDrawContainerEvent;
    FOnCustomDrawTimeRuler: TcxSchedulerCustomDrawTimeRulerEvent;
    procedure DoAllDayScroll(Sender: TObject; ScrollCode: TScrollCode; var ScrollPos: Integer);
    function GetHitTest: TcxSchedulerDayViewHitTest;
    function GetViewInfo: TcxSchedulerDayViewViewInfo;
    procedure SetAlwaysShowEventTime(AValue: Boolean);
    procedure SetAutoContentHeight(AValue: Boolean);
    procedure SetEventShadows(AValue: Boolean);
    procedure SetHeaderContainer(AValue: Boolean);
    procedure SetAllDayAreaScrollBar(AValue: TcxSchedulerAllDayAreaScrollBar);
    procedure SetDayHeaderArea(AValue: Boolean);
    procedure SetDayHeaderModernStyleDisplayMode(AValue: TcxSchedulerDayHeaderModernStyleDisplayMode);
    procedure SetShowAllDayEventsInContentArea(AValue: Boolean);
    procedure SetStyles(AValue: TcxSchedulerDayViewStyles);
    procedure SetTimeRulerMinutes(AValue: Boolean);
    procedure SetTimeRulerPopupMenu(AValue: TcxSchedulerTimeRulerPopupMenu);
    procedure SetTimeScale(AValue: Integer);
    procedure SetWorkTimeOnly(AValue: Boolean);
  protected
    procedure DeactivateView; override;
    // store interface
    procedure GetProperties(AProperties: TStrings); override;
    procedure GetPropertyValue(const AName: string; var AValue: Variant); override;
    procedure SetPropertyValue(const AName: string; const AValue: Variant); override;
    // IcxSchedulerViewTimeScaleStep
    function GetTimeScaleStep: Integer;
    procedure SetTimeScaleStep(const AValue: Integer);
    // IdxTouchScrollUIOwner
    procedure CheckUIPosition;
    function GetOwnerControl: TcxControl;
    function HasVisibleUI: Boolean;
    procedure HideUI;
    // IdxHybridScrollbarOwner
    function IdxHybridScrollbarOwner.GetBaseColor=GetHybridScrollbarBaseColor;
    function GetHybridScrollbarBaseColor: TColor;
    function GetManager: TdxHybridScrollbarsManager;
    procedure IdxHybridScrollbarOwner.Invalidate=InvalidateScrollbars;
    procedure InvalidateScrollbars;
    //
    procedure CreateSubClasses; override;
    procedure DestroySubClasses; override;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreatePainter: TcxSchedulerSubControlPainter; override;
    function CreateStyles: TcxSchedulerDayViewStyles; virtual;
    function CreateTimeRulerPopupMenu: TcxSchedulerTimeRulerPopupMenu; virtual;
    function CreateViewAdapter: TcxCustomResourceViewAdapter; override;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    procedure DoCustomDrawContainer(ACell: TcxSchedulerContainerCellViewInfo; var ADone: Boolean); virtual;
    procedure DoCustomDrawTimeRuler(ACell: TcxSchedulerTimeRulerCellViewInfo; var ADone: Boolean); virtual;
    function DoShowPopupMenu(X, Y: Integer): Boolean; override;
    function EventContentSelected(AEvent: TcxSchedulerControlEvent): Boolean; override;
    function GetEditWithSingleLineEditor(AEvent: TcxSchedulerControlEvent): Boolean; override;
    function GetFirstVisibleTime: TDateTime; override;
    function GetGestureClient(const APoint: TPoint): IdxGestureClient; override;
    function GetLastVisibleTime: TDateTime; override;
    function GetTimeIncrement: TDateTime; override;
    function GetViewContentRect: TRect; override;
    function IsDayView: Boolean; override;
    procedure MakeEventVisible(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime;
      AResource: TcxSchedulerStorageResourceItem); override;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure ScrollVisibleDays(AScrollUp: Boolean); override;

    //
    procedure DoCreateScrollBars; override;
    procedure DoDestroyScrollBars; override;
    function GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner; override;
    procedure InitScrollBars; override;

    procedure TimeChanged; override;
    procedure VisibleChanged; override;

    property AllDayScrollBar: TdxScrollBarWrapper read FAllDayScrollBar;
    property ViewInfo: TcxSchedulerDayViewViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure MakeWorkTimeVisible;

    property HitTest: TcxSchedulerDayViewHitTest read GetHitTest;
    property Scheduler;
    property WorkDays;
    property WorkStart;
    property WorkFinish;
  published
    property CanShow; //before active
    property Active;
    property AllDayAreaScrollBar: TcxSchedulerAllDayAreaScrollBar
      read FAllDayAreaScrollBar write SetAllDayAreaScrollBar default adsbDefault;
    property AlwaysShowEventTime: Boolean read FAlwaysShowEventTime write SetAlwaysShowEventTime default False;
    property AutoContentHeight: Boolean read FAutoContentHeight write SetAutoContentHeight default False;
    property DayHeaderArea: Boolean read FDayHeaderArea write SetDayHeaderArea default True;
    property DayHeaderModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode
      read FDayHeaderModernStyleDisplayMode write SetDayHeaderModernStyleDisplayMode default hdmDefault;

    property EventImagesLayout default eilAuto;
    property EventShadows: Boolean read FEventShadows write SetEventShadows default True;
    property GroupingKind;
    property HeaderContainer: Boolean read FHeaderContainer write SetHeaderContainer default True;
    property ShowAllDayEventsInContentArea: Boolean read FShowAllDayEventsInContentArea
      write SetShowAllDayEventsInContentArea default False;
    property Styles: TcxSchedulerDayViewStyles read FStyles write SetStyles;
    property TimeRulerMinutes: Boolean read FTimeRulerMinutes write SetTimeRulerMinutes default False;
    property TimeRulerPopupMenu: TcxSchedulerTimeRulerPopupMenu read FTimeRulerPopupMenu write SetTimeRulerPopupMenu;
    property TimeScale: Integer read FTimeScale write SetTimeScale default cxDefaultTimeScale;
    property WorkTimeOnly: Boolean read FWorkTimeOnly write SetWorkTimeOnly default False;
    property OnCustomDrawContainer: TcxSchedulerCustomDrawContainerEvent read FOnCustomDrawContainer write FOnCustomDrawContainer;
    property OnCustomDrawTimeRuler: TcxSchedulerCustomDrawTimeRulerEvent read FOnCustomDrawTimeRuler write FOnCustomDrawTimeRuler;
  end;

  TcxSchedulerAllDayContainerGestureScrollHelper = class(TcxIUnknownObject, IdxGestureClient)
  private
    FView: TcxSchedulerDayView;
  protected
    function AllowGesture(AGestureId: Integer): Boolean;
    function AllowPan(AScrollKind: TScrollBarKind): Boolean;
    procedure BeginGestureScroll(APos: TPoint);
    procedure EndGestureScroll;
    procedure GestureScroll(ADeltaX, ADeltaY: Integer);
    function GetPanOptions: Integer;
    function IsPanArea(const APoint: TPoint): Boolean;
    function NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
  public
    constructor Create(AView: TcxSchedulerDayView);
  end;

  { TcxSchedulerDayViewAdapter }

  TcxSchedulerDayViewAdapter = class(TcxCustomResourceViewAdapter)
  protected
    FTopIndex: Integer;
    function GetPrintRange(Index: Integer): TDateTime; override;
    procedure Store; override;
    procedure Restore; override;
  end;

  { TcxSchedulerDayViewContentCellCustomViewInfo }

  TcxSchedulerDayViewContentCellCustomViewInfo = class(TcxSchedulerContentCellViewInfo)
  private
    FIsHourSeparator: Boolean;
  protected
    function GetViewStyle: TcxSchedulerViewStyle; virtual; abstract;
    procedure SetBorderColor(const AResourceColor: Integer; AIsHourSeparator, AIsWorkTime: Boolean); virtual;

    property IsHourSeparator: Boolean read FIsHourSeparator;
  end;

  { TcxSchedulerDayViewContentCellViewInfo }

  TcxSchedulerDayViewContentCellViewInfo = class(TcxSchedulerDayViewContentCellCustomViewInfo)
  protected
    function GetViewStyle: TcxSchedulerViewStyle; override;
  end;

  { TcxSchedulerDayViewContentCellModernViewInfo }

  TcxSchedulerDayViewContentCellModernViewInfo = class(TcxSchedulerDayViewContentCellCustomViewInfo)
  private
    FLeftBorderColor: TColor;
  protected
    procedure DoDraw; override;
    function GetViewStyle: TcxSchedulerViewStyle; override;
    procedure SetBorderColor(const AResourceColor: Integer; AIsHourSeparator, AIsWorkTime: Boolean); override;
  public
    constructor Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
      const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;

    property LeftBorderColor: TColor read FLeftBorderColor write FLeftBorderColor;
  end;

  { TcxSchedulerDayViewEventCellCustomViewInfo }

  TcxSchedulerDayViewEventCellCustomViewInfo = class(TcxSchedulerEventCellViewInfo)
  protected
    procedure CalculateEventTimeVisibility; override;
    procedure CalculateShowTimeAsClock; override;
    function GetDetailCaptionFlagValue: Boolean; override;
    function GetDetailInfoFlagValue: Boolean; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    function IsHorzSizing: Boolean; override;
    function IsNeedDrawTime: Boolean; override;
    function IsShowHeaderEventsInContentArea: Boolean;
  end;

  { TcxSchedulerDayViewEventCellViewInfo }

  TcxSchedulerDayViewEventCellViewInfo = class(TcxSchedulerDayViewEventCellCustomViewInfo)
  protected
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    procedure CalculateCaptions; override;
    procedure CalculateItemsLayout; override;
    procedure CalculateNonDetailEventTimeVisibility; override;
    procedure CalculateTimeLineLayout; override;
    function CanShowHint: Boolean; override;
    procedure DrawSelection; override;
    procedure DrawShadow;
    procedure DrawState; override;
    function GetBoundsForHitTest: TRect; override;
    function GetEditingRect: TRect; override;
    function GetTimeLineBorderColor: TColor; override;
    function GetSelectionBoundsExtends: TRect; override;
    function IsDrawShadowFirst: Boolean;
    function IsTimeLineVisible: Boolean; override;
  end;

  { TcxSchedulerDayViewEventCellModernViewInfo }

  TcxSchedulerDayViewEventCellModernViewInfo = class(TcxSchedulerDayViewEventCellCustomViewInfo)
  private
    FRealBoundsLeft: Integer;
    FRealBorderColor: TColor;
  protected
    FTimeLineHeight: Integer;
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    procedure CalculateBorderAttributes; override;
    procedure CalculateCaptions; override;
    procedure CalculateDetailInfo; override;
    procedure CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer); override;
    procedure CalculateNonDetailEventTimeVisibility; override;
    procedure CalculateLocationRect; override;
    function CanBeHeaderEventContinueArrows: Boolean; override;
    function CanMoveLeftBorder: Boolean; override;
    function CanMoveRightBorder: Boolean; override;
    function CanShowHeaderEventClock(const ARect: TRect): Boolean; override;
    function CheckAutoLayoutImagesSingleLineCaption(const ACaptionWidth, AAvailableWidth: Integer): Boolean; override;
    procedure DoDrawCaption; override;
    procedure DrawSelection; override;
    function EnableModernSelection: Boolean; override;
    function GetCaptionFlags: Cardinal; override;
    function GetCaptionFontStyle: TFontStyles; override;
    function GetEditingRect: TRect; override;
    function GetHeaderEventCaptionLeft(const ARect: TRect; AContentWidth: Integer): Integer; override;
    function GetHeaderImagesPossibleRect(const ACaptionRect: TRect; ATextWidth: Integer): TRect; override;
    function GetOffsetSelectionBorderFromTimeLineRect: Integer; override;
    function GetImagesHorizontalOffset: Integer; override;
    function GetMessageRectOffset: Integer; override;
    function GetTimeLineHeight: Integer; override;
    function GetTimeLineRectRight: Integer; override;
    function GetViewStyle: TcxSchedulerViewStyle; override;
    function IsTimeLineVisible: Boolean; override;
    function MeasureAutoLayoutImagesLocationHeight(const R: TRect): Integer; override;
    function NeedBorderAroundOfTimeLineRect: Boolean; override;
    function NeedHeaderEventStartContinueArrow: Boolean; override;
    function NeedHeaderEventFinishContinueArrow: Boolean; override;
    procedure TuneClipping(var AClipRgn: TcxRegion); override;
  end;
  { TcxSchedulerDayViewViewInfo }

  TcxSchedulerDayViewViewInfo = class(TcxSchedulerCustomResourceViewViewInfo)
  private
    function GetAllDayEventMaxCount: Integer;
    function GetAutoHeight: Boolean;
    function GetColumnInGroup: Integer;
    function GetContentHeight: Integer;
    function GetController: TcxSchedulerDayViewController;
    function GetDayView: TcxSchedulerDayView; inline;
    function GetGroupCount: Integer;
    function GetHeaderContainer: Boolean;
    function GetPrintRowCount: Integer;
    function GetLinePerHour: Integer;
    function NeedAllDayScrollBar: Boolean;
    procedure SetLargeFont(AFont: TFont);
    procedure UpdateAllDayScrollBarParams;
  protected
    Builder: TcxSchedulerEventLayoutBuilder;
    FAdditionalTimeZone: Integer;
    FAdditionalTimeZoneBiasDelta: Integer;
    FAllDayAreaBounds: TRect;
    FAllDayTopIndex: Integer;
    FBackground: TcxSchedulerBackgroundCellViewInfo;
    FColCount: Integer;
    FContentOffset: Integer;
    FCurrentTimeZone: Integer;
    FDayRowCount: Integer;
    FEventLayout: TcxCalculateEventLayout;
    FHeaderContainerCaptionHeight: Integer;
    FHeaderContainerContentOffset: Integer;
    FHeaderLineCount: Integer;
    FHeaderLineHeight: Integer;
    FHeaderOffsetLeft: Integer;
    FIsContainerSelected: Boolean;
    FLargeFont: TFont;
    FPrintStartRow: Integer;
    FPrintFinishRow: Integer;
    FRowCount: Integer;
    FTimeRulerCells: TcxSchedulerViewInfoCellList;
    FTopIndex: Integer;
    FTimeScale: Integer;
    FUnusedRowCount: Integer;
    FVisibleRowCount: Integer;
    function AddContentCell(const ARect: TRect; const AStart, AFinish: TDateTime;
      AResourceIndex: Integer): TcxSchedulerContentCellViewInfo; override;
    procedure AddContainerCell(AColumn: Integer; const ABounds: TRect);
    procedure AddHeaderContainerEvent(APlace: TcxSchedulerEventPlace); virtual;
    procedure AddHeaderContainerEventPlace(AEvent: TcxSchedulerControlEvent; var AColIndex: Integer);
    procedure AddMoreEventsButton(const AContainerBounds: TRect; const ARowIndex: Integer; AIsTop: Boolean; AEvent: TcxSchedulerEvent);
    procedure CalculateAllDayAreaBounds;
    procedure CalculateAllDayScrollBar;
    procedure CalculateBackground; virtual;
    procedure CalculateContent; virtual;
    procedure CalculateContentNavigationButtons; override;
    procedure CalculateEvents; virtual;
    procedure CalculateEventsForColumn(AIndex: Integer;
      AContainer: TcxSchedulerContainerCellViewInfo; AEvents: TcxSchedulerEventList); virtual;
    procedure CalculateHeaderEvents; virtual;
    procedure CalculateHeaderEventsPlace;
    procedure CalculateHeaderLineCount;
    procedure CalculateHeaders; virtual;
    procedure CalculateMetrics; override;
    procedure CalculateMetricsForPrinting;
    procedure CalculateDisplayRows;
    procedure CalculateTimeLine; virtual;
    procedure CalculateTimeZoneBiasDelta;
    procedure CalculateVisibleRows;
    procedure CalculateUnusedRowCount;
    function CheckDisplayRows(var AStartRow, AFinishRow: Integer): Boolean;
    procedure CheckEventRowsVisibility(const AContainerBounds: TRect; AEvent: TcxSchedulerControlEvent; AStartRow, AFinishRow: Integer);
    procedure Clear; override;
    function ContentCellClass: TcxSchedulerContentCellViewInfoClass; override;
    function CreateContentEventViewData(AEventInfo: TcxEventLayoutInfo; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;
    function CreateHeaderEventViewData(AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
      const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;
    function CreateEventLayout: TcxCalculateEventLayout; virtual;
    procedure DoCalculate; override;
    procedure DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); override;
    procedure DoMoreEventsButtonClick(Sender: TcxSchedulerMoreEventsButtonViewInfo); override;
    procedure ExtractEvents(ASource, ADest: TcxSchedulerEventList; AIndex: Integer);
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; override;
    function GetActualDayHeaderModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode; virtual;
    function GetCalculateEventLayoutClass: TcxCalculateEventLayoutClass;
    function GetContentParams(const ATime: TDateTime;
      AResource: TcxSchedulerResourceViewInfo; var AIsSelected: Boolean): TcxViewParams; reintroduce; virtual;
    function GetColumnDate(AIndex: Integer): Integer;
    function GetColumnPositionInResource(AColumnIndex, AResourceIndex: Integer): TcxSchedulerColumnPositionInResource;
    function GetColumnResource(AIndex: Integer): TcxSchedulerResourceViewInfo;
    function GetColumnResourceIndex(AIndex: Integer): Integer;
    function GetContainer(AIndex: Integer): TcxSchedulerContainerCellViewInfo; virtual;
    function GetContainerCount: Integer; virtual;
    function GetDayHeaderHeight: Integer; override;
    procedure GetHeaderEventInfo(APlace: TcxSchedulerEventPlace;
      out ALeft, ARight: Integer; out AStart, AFinish: TDateTime); virtual;
    function GetEventColumnRow(const ATime: TDateTime; AColIndex: Integer; AStart: Boolean): Integer;
    function GetEventSlotDisplayText(AEvent: TcxSchedulerControlEvent): string;
    function GetTimeLineParams: TcxViewParams; override;
    function GetTimeRulerParams: TcxViewParams;
    function GetTimeRow(const ATime: TDateTime; AIsStart: Boolean): Integer;
    function GetResourcesContentWidth: Integer; override;
    function GetRowTime(AColumn, ARow: Integer): TDateTime;
    function GetSeparatorCount: Integer; override;
    function HeaderContainerCellClass: TcxSchedulerContainerCellViewInfoClass; virtual;
    function IsColumnEvent(AEvent: TcxSchedulerEvent; AIndex: Integer): Boolean; overload;
    function IsColumnEvent(AEvent: TcxSchedulerEvent; AIndex: Integer; IsHeaderEvent: Boolean): Boolean; overload;
    function IsContainerSelected(AResource: TcxSchedulerStorageResourceItem; ADate: TDateTime): Boolean;
    function IsEventVisible(AEvent: TcxSchedulerEvent): Boolean; overload;
    function IsEventVisible(var AStartRow, AStopRow: Integer): Boolean; overload;
    function IsHourVisible(AIndex: Integer): Boolean;
    function IsRowVisible(AIndex: Integer): Boolean;
    procedure MakeTimeVisible(const ATime: TDateTime); override;
    function NavigationButtonOffset(AKind: TcxSchedulerContentNavigationButtonKind;
      AResourceIndex: Integer): Integer; override;
    procedure SetColumnTimeLineStyle(AEventInfo: TcxSchedulerEventCellViewInfo; AColIndex: Integer);
    procedure SetContainerCellHeaderEvent(AEventInfo: TcxSchedulerEventCellViewInfo);
    function ShowEventTimeLineOnContentCell: Boolean; virtual;
    function ShowTimeLineOnHeaderEvent: Boolean; virtual;
    function TimeRulerCellClass: TcxSchedulerTimeRulerCellViewInfoClass;

    property ColCount: Integer read FColCount;
    property ColumnInGroup: Integer read GetColumnInGroup;
    property ContainerCount: Integer read GetContainerCount;
    property Containers[AIndex: Integer]: TcxSchedulerContainerCellViewInfo read GetContainer;
    property Controller: TcxSchedulerDayViewController read  GetController;
    property GroupCount: Integer read GetGroupCount;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure InitScrollBarsParameters; override;
    procedure ScrollVertical(AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure AllDayScroll(AScrollCode: TScrollCode; var AScrollPos: Integer);

    property AllDayTopIndex: Integer read FAllDayTopIndex;
    property AutoHeight: Boolean read GetAutoHeight;
    property Background: TcxSchedulerBackgroundCellViewInfo read FBackground;
    property ContentHeight: Integer read GetContentHeight;
    property DayView: TcxSchedulerDayView read GetDayView;
    property HeaderContainer: Boolean read GetHeaderContainer;
    property HeaderContainerCells: TcxSchedulerViewInfoCellList read FHeaderContainerCells;
    property HeaderLineHeight: Integer read FHeaderLineHeight;
    property LargeFont: TFont read FLargeFont write SetLargeFont;
    property LinePerHour: Integer read GetLinePerHour;
    property PrintRowCount: Integer read GetPrintRowCount;
    property PrintFinishRow: Integer read FPrintFinishRow;
    property PrintStartRow: Integer read FPrintStartRow;
    property TimeScale: Integer read FTimeScale;
    property TimeRulerCells: TcxSchedulerViewInfoCellList read FTimeRulerCells;
    property TopIndex: Integer read FTopIndex;
    property UnusedRowCount: Integer read FUnusedRowCount;
    property VisibleRowCount: Integer read FVisibleRowCount;
  end;

  { TcxSchedulerDayViewHitTest }

  TcxSchedulerDayViewHitTest = class(TcxSchedulerCustomResourceViewHitTest)
  private
    function GetContainerCell: TcxSchedulerContainerCellViewInfo;
    function GetTimeRulerCell: TcxSchedulerTimeRulerCellViewInfo;
  public
    property ContainerCell: TcxSchedulerContainerCellViewInfo read GetContainerCell;
    property HitAtContainer: Boolean index htcContainer read GetBitState;
    property HitAtTimeRuler: Boolean index htcTimeRuler read GetBitState;
    property TimeRulerCell: TcxSchedulerTimeRulerCellViewInfo read GetTimeRulerCell;
    property HitAtTimeZoneLabel;
    property TimeZone;
  end;

  { TcxDayViewDragEventHelper }

  TcxDayViewDragEventHelper = class(TcxDragEventHelper)
  private
    FStartInHeader: Boolean;
    function GetController: TcxSchedulerDayViewController;
    function GetHitTest: TcxSchedulerDayViewHitTest;
    function GetShowHeaderContainer: Boolean;
  protected
    function GetOriginHitTestMask: Int64; override;
    procedure GetOriginState; override;
    function IsAtOrigin: Boolean; override;
    function InHeader: Boolean; virtual;
    procedure SetCloneEventsTimeDelta(AStart, ACurrent: TDateTime; AInHeader: Boolean);
    procedure UpdateViewClonesTime; override;
    procedure UpdateEventStates;

    property Controller: TcxSchedulerDayViewController read GetController;
    property HitTest: TcxSchedulerDayViewHitTest read GetHitTest;
    property ShowHeaderContainer: Boolean read GetShowHeaderContainer;
  end;

  { TcxDayViewEventSizing }

  TcxDayViewEventSizing = class(TcxEventSizingHelper)
  private
    FFixedBoundTime: TDateTime;
    function GetController: TcxSchedulerDayViewController;
    function GetHitTest: TcxSchedulerDayViewHitTest;
  protected
    function GetDragCursor(Accepted: Boolean): TCursor; override;
    function GetOriginHitTestMask: Int64; override;
    // Event handling
    procedure CalcAllDayEvent; override;
    function GetFinishTime: TDateTime; override;
    procedure GetOriginState; override;
    function GetSizingTime: TDateTime;
    function GetStartTime: TDateTime; override;

    property Controller: TcxSchedulerDayViewController read GetController;
    property HitTest: TcxSchedulerDayViewHitTest read GetHitTest;
  end;

  { TcxSchedulerDayNavigation }

  TcxSchedulerDayNavigation = class(TcxSchedulerCustomResourceViewNavigation)
  private
    function GetDayView: TcxSchedulerDayView;
    function GetSelectedDays: TcxSchedulerDateList;
    function GetViewInfo: TcxSchedulerDayViewViewInfo;
  protected
    FSelRow: Integer;
    function ColCount: Integer;
    procedure DoNextColumn(AGoToNext: Boolean; AColumn: Integer;
      const ATime: TDateTime; AResource: TcxSchedulerStorageResourceItem); virtual;
    function DoNextPage(AGoForward: Boolean; var ATime: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem): Boolean; virtual;
    function GetColumnByDate(const ADate: TDateTime; AResource: TObject): Integer;
    function GetColumnDate(AColumn: Integer): TDateTime;
    function GetColumnResource(AColumn: Integer): TcxSchedulerStorageResourceItem;
    function GetRowTime(const ARow: Integer; AFinish: Boolean = False): TDateTime;
    function GetTimeRow(const ATime: TDateTime): Integer;
    function IsResourceNavigation(AGoToNext: Boolean;
      AColumn: Integer; const ATime: TDateTime): Boolean;
  public
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure ValidateSelection(var ASelStart, ASelFinish: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem); override;
    function ValidateTimeVisibility(var ADateTime: TDateTime): TDateTime;

    property DayView: TcxSchedulerDayView read GetDayView;
    property SelectedDays: TcxSchedulerDateList read GetSelectedDays;
    property ViewInfo: TcxSchedulerDayViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerDayViewController }

  TcxSchedulerDayViewController = class(TcxSchedulerCustomResourceViewController)
  private
    FLockScrolling: Boolean;
    FTimer: TTimer;
    function GetDragEventHelper: TcxDayViewDragEventHelper;
    function GetHitTest: TcxSchedulerDayViewHitTest;
    function GetTimeRulerTime: TDateTime;
    function GetView: TcxSchedulerDayView;
    function GetViewInfo: TcxSchedulerDayViewViewInfo;
  protected
    FIsEditingBeforeMouseDown: Boolean;
    FScrollAreaRects: TList;
    procedure BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure CancelScroll; override;
    procedure CheckDragDropScrolling(const P: TPoint);
    procedure CheckScrolling(const APos: TPoint); override;
    procedure CheckTimeRulerTime;
    function CreateDragEventHelper: TcxDragEventHelper; override;
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function CreateResizeEventHelper: TcxEventSizingHelper; override;
    procedure InitTimer(AllowStart: Boolean; AScrollCode: TScrollCode); virtual;

    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure OnTimer(Sender: TObject); virtual;
    procedure SelectNextEvent(AForward: Boolean); override;
    //
    procedure CheckNavigatorScrollArea(const APoint: TPoint); override;
    procedure DoneNavigatorScrollArea; override;
    procedure InitNavigatorScrollArea; override;

    property DragEventHelper: TcxDayViewDragEventHelper read GetDragEventHelper;
    property HitTest: TcxSchedulerDayViewHitTest read GetHitTest;
    property View: TcxSchedulerDayView read GetView;
    property ViewInfo: TcxSchedulerDayViewViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
  end;

  { TcxSchedulerDayViewPainter }

  TcxSchedulerDayViewPainter = class(TcxSchedulerCustomViewPainter)
  private
    function GetView: TcxSchedulerDayView;
    function GetViewInfo: TcxSchedulerDayViewViewInfo;
  protected
    procedure DrawContainerCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawDayViewEventCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
    procedure DrawTimeRulerCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean); virtual;
  public
    procedure Paint; override;

    property View: TcxSchedulerDayView read GetView;
    property ViewInfo: TcxSchedulerDayViewViewInfo read GetViewInfo;
  end;

  { TcxEventLayoutInfo }

  TcxEventLayoutInfo = class
  protected
    ColStart, ColEnd, ColCount: Integer;
    Event: TcxSchedulerControlEvent;
    FDisplayRect: TRect;
    FFinishTime: TDateTime;
    FStartTime: TDateTime;
    IsBreakEvent: Boolean;
    RowCount: Integer;
    StartRow, StopRow: Integer;
    function ShadowSize(AScaleFactor: TdxScaleFactor): Integer;
    function GetFinishCoordinate(AScale: Integer): Integer; virtual;
    procedure CalculateDisplayRect(AFullColWidth, ARowHeight: Integer; AScaleFactor: TdxScaleFactor); virtual;
    procedure CalculateTimes(const ADate: TDateTime; AScale, AOffset: Integer);
  public
    constructor Create(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime; AScale: Integer); virtual;

    property DisplayRect: TRect read FDisplayRect;
    property Finish: TDateTime read FFinishTime;
    property Start: TDateTime read FStartTime;
  end;

  TcxEventLayoutInfoClass = class of TcxEventLayoutInfo;

  { TcxEventModernLayoutInfo }

  TcxEventModernLayoutInfo = class(TcxEventLayoutInfo)
  private
    FRealHeight: Integer;
    FIntersectedInfos: TcxObjectList;
    function GetIntersectedInfo(AIndex: Integer): TcxEventModernLayoutInfo;
    function GetIntersectionCount: Integer;
  protected
    procedure AddIntersectedInfo(AInfo: TcxEventModernLayoutInfo);
    procedure CalculateDisplayRect(AFullColWidth, ARowHeight: Integer; AScaleFactor: TdxScaleFactor); override;
    function GetFinishCoordinate(AScale: Integer): Integer; override;

    property IntersectedInfo[AIndex: Integer]: TcxEventModernLayoutInfo read GetIntersectedInfo;
    property IntersectionCount: Integer read GetIntersectionCount;
    property RealHeight: Integer read FRealHeight;
  public
    constructor Create(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime; AScale: Integer); override;
    destructor Destroy; override;
  end;

  { TcxCalculateEventLayout }

  TcxCalculateEventLayout = class(TList)
  private
    FDate: TDateTime;
    FLeft: Integer;
    FRowHeight: Integer;
    FTimeScale: Integer;
    FTop: Integer;
    FWidth: Integer;
    function GetInfo(AIndex: Integer): TcxEventLayoutInfo;
  protected
    FViewInfo: TcxSchedulerDayViewViewInfo;
    procedure CalculateColCount(AInfo: TcxEventLayoutInfo); virtual;
    procedure CalculateColEnd(AInfo: TcxEventLayoutInfo); virtual;
    procedure CalculateColStart(AInfo: TcxEventLayoutInfo; AIndex: Integer); virtual;
    procedure CalculateColStartAndColCount(AInfo: TcxEventLayoutInfo; AIndex: Integer); virtual;
    procedure CalculateInfoRowsAndTimes(AInfo: TcxEventLayoutInfo);
    function CreateEventInfo(AEvent: TcxSchedulerControlEvent): TcxEventLayoutInfo;
    function GetLayoutInfoClass: TcxEventLayoutInfoClass; virtual;
    function GetScaleFactor: TdxScaleFactor;
    function Intersect(AInfo1, AInfo2: TcxEventLayoutInfo): Boolean; virtual;
    procedure OffsetDisplayRect(AInfo: TcxEventLayoutInfo); virtual;
    procedure PrepareInfoBeforeCalculateColStartAndColCount(AInfo: TcxEventLayoutInfo); virtual;
    procedure PrepareInfoBeforeCalculateDisplayRect(AInfo: TcxEventLayoutInfo); virtual;
    procedure SetColCount(AInfo: TcxEventLayoutInfo); virtual;

    property ScaleFactor: TdxScaleFactor read GetScaleFactor;
  public
    procedure Add(AEvent: TcxSchedulerControlEvent);
    procedure Calculate;
    procedure Clear; override;
    procedure SetParams(const ADate: TDateTime; ATimeScale, ARowHeight: Integer;
      ALeft, ATop, AWidth: Integer);

    property Date: TDateTime read FDate write FDate;
    property Infos[Index: Integer]: TcxEventLayoutInfo read GetInfo;
    property Left: Integer read FLeft;
    property RowHeight: Integer read FRowHeight;
    property TimeScale: Integer read FTimeScale;
    property Top: Integer read FTop;
    property Width: Integer read FWidth;
  end;

  { TcxCalculateModernEventLayout }

  TcxCalculateModernEventLayout = class(TcxCalculateEventLayout)
  protected
    procedure CalculateColEnd(AInfo: TcxEventLayoutInfo); override;
    procedure CalculateColStartAndColCount(AInfo: TcxEventLayoutInfo; AIndex: Integer); override;
    function GetLayoutInfoClass: TcxEventLayoutInfoClass; override;
    function Intersect(AInfo1, AInfo2: TcxEventLayoutInfo): Boolean; override;
    procedure OffsetDisplayRect(AInfo: TcxEventLayoutInfo); override;
    procedure PrepareInfoBeforeCalculateColStartAndColCount(AInfo: TcxEventLayoutInfo); override;
    procedure PrepareInfoBeforeCalculateDisplayRect(AInfo: TcxEventLayoutInfo); override;
    procedure SetColCount(AInfo: TcxEventLayoutInfo); override;
  end;

  { TcxSchedulerTimeRulerPopupMenu }

  TcxSchedulerTimeRulerPopupMenuItem = (rpmiNewEvent, rpmiNewAllDayEvent,
    rpmiNewReccuringEvent, rpmi60min, rpmi30min, rpmi15min, rpmi10min,
    rpmi6min, rpmi5min);
  TcxSchedulerTimeRulerPopupMenuItems = set of TcxSchedulerTimeRulerPopupMenuItem;

  TcxSchedulerTimeRulerPopupMenuPopupEvent = procedure (
    Sender: TcxSchedulerTimeRulerPopupMenu; ABuiltInMenu: TPopupMenu;
    var AHandled: Boolean) of object;
  TcxSchedulerTimeRulerPopupMenuClickEvent = procedure (
    Sender: TcxSchedulerTimeRulerPopupMenu;
    AItem: TcxSchedulerTimeRulerPopupMenuItem; var AHandled: Boolean) of object;

  TcxSchedulerTimeRulerPopupMenu = class(TcxSchedulerCustomContentPopupMenu)
  private
    FItems: TcxSchedulerTimeRulerPopupMenuItems;
    FOnPopup: TcxSchedulerTimeRulerPopupMenuPopupEvent;
    FOnClick: TcxSchedulerTimeRulerPopupMenuClickEvent;
    function GetDayView: TcxSchedulerDayView;
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function DoOnClick(ACommand: Integer): Boolean; override;
    function DoOnPopup: Boolean; override;
    function IsValidCommand(ACommand: Integer): Boolean; override;

    property DayView: TcxSchedulerDayView read GetDayView;
  public
    constructor Create(AScheduler: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(AItem: TcxSchedulerTimeRulerPopupMenuItem);
    function GetMenuItem(AItem: TcxSchedulerTimeRulerPopupMenuItem): TMenuItem;
  published
    property Items: TcxSchedulerTimeRulerPopupMenuItems
      read FItems write FItems default [rpmiNewEvent, rpmiNewAllDayEvent,
        rpmiNewReccuringEvent, rpmi60min, rpmi30min, rpmi15min, rpmi10min,
        rpmi6min, rpmi5min];
    property PopupMenu;
    property UseBuiltInPopupMenu;

    property OnClick: TcxSchedulerTimeRulerPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxSchedulerTimeRulerPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

implementation

uses
  cxVariants, cxDateUtils, cxDrawTextUtils, cxScrollBar, cxLibraryConsts;

type
  TcxControlPopupScrollBarAccess = class(TcxControlPopupScrollBar);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);
  TcxSchedulerContentCellViewInfoAccess = class(TcxSchedulerContentCellViewInfo);
  TcxSchedulerCustomDateNavigatorAccess = class(TcxSchedulerCustomDateNavigator);
  TcxSchedulerContainerCellViewInfoAccess = class(TcxSchedulerContainerCellViewInfo);

const
  cxMinutesPerDay = 24 * 60;
  RulerScales: array[0..5] of Integer = (60, 30, 15, 10, 6, 5);

function GetTimeMinutes(const ATime: TDateTime): Integer;
var
  H, M, S, MS: Word;
begin
  DecodeTime(ATime, H, M, S, MS);
  Result := H * 60 + M;
end;

{ TcxSchedulerDayViewStyles }

constructor TcxSchedulerDayViewStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FScheduler := TcxSchedulerDayView(GetOwner).Scheduler;
  BitmapInViewParams := True;
end;

procedure TcxSchedulerDayViewStyles.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerDayViewStyles then
  begin
    for I := 0 to cxcsMaxDayViewValue do
      SetValue(I, TcxSchedulerDayViewStyles(Source).GetValue(I))
  end;
  inherited Assign(Source);
end;

procedure TcxSchedulerDayViewStyles.Changed(AIndex: Integer);
begin
  TcxSchedulerDayView(GetOwner).Refresh;
end;

procedure TcxSchedulerDayViewStyles.GetDefaultHeaderContainerViewParams(var AParams: TcxViewParams);
begin
  if SchedulerViewStyle = svsClassic then
  begin
    AParams.Color := Painter.DefaultHeaderBackgroundColor;
    AParams.TextColor := Painter.DefaultHeaderBackgroundTextColor;
  end
  else
  begin
    AParams.Color := Painter.DefaultSchedulerHeaderContainerBackgroundColor(False);
    AParams.TextColor := Painter.DefaultSchedulerHeaderContainerTextColor(False);
  end
end;

procedure TcxSchedulerDayViewStyles.GetDefaultSelectedHeaderContainerViewParams(var AParams: TcxViewParams);
begin
  if SchedulerViewStyle = svsClassic then
  begin
    AParams.Color := Painter.DefaultContentColor;
    AParams.TextColor := Painter.DefaultContentTextColor;
  end
  else
  begin
    AParams.Color := Painter.DefaultSchedulerHeaderContainerBackgroundColor(True);
    AParams.TextColor := Painter.DefaultSchedulerHeaderContainerTextColor(True);
  end
end;

procedure TcxSchedulerDayViewStyles.GetDefaultTimeRulerViewParams(var AParams: TcxViewParams);
begin
  if SchedulerViewStyle = svsClassic then
  begin
    AParams.Color := Painter.DefaultSchedulerTimeRulerColorClassic;
    AParams.TextColor := Painter.DefaultSchedulerTimeRulerTextColorClassic;
  end
  else
  begin
    AParams.Color := Painter.DefaultSchedulerTimeRulerColor;;
    AParams.TextColor := Painter.DefaultSchedulerTimeRulerTextColor;
  end;
end;

procedure TcxSchedulerDayViewStyles.GetDefaultViewParams(Index: Integer;
  AData: TObject; out AParams: TcxViewParams);
begin
  AParams.Bitmap := nil;
  AParams.Font := Scheduler.Font;
  AParams.TextColor := clBlack;
  AParams.Color := Scheduler.Color;
  case Index of
    cxcsHeaderContainer:
      GetDefaultHeaderContainerViewParams(AParams);
    cxcsSelectedHeaderContainer:
      GetDefaultSelectedHeaderContainerViewParams(AParams);
    cxcsTimeLine:
      begin
        AParams.Color := clWhite;
        AParams.TextColor := Scheduler.OptionsView.DayBorderColor;
        if AParams.TextColor = clDefault then
          AParams.TextColor := clWindowText;
      end;
    cxcsTimeRuler:
      GetDefaultTimeRulerViewParams(AParams);
  end;
end;

function TcxSchedulerDayViewStyles.GetHeaderContainerParams(
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem;
  ASelected: Boolean): TcxViewParams;
begin
  GetViewParams(cxcsHeaderContainer + Byte(ASelected), nil, nil, Result);
end;

function TcxSchedulerDayViewStyles.GetTimeLineParams: TcxViewParams;
begin
  GetViewParams(cxcsTimeLine, nil, nil, Result);
end;

function TcxSchedulerDayViewStyles.GetTimeRulerParams: TcxViewParams;
begin
  GetViewParams(cxcsTimeRuler, nil, nil, Result);
end;

function TcxSchedulerDayViewStyles.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := TcxSchedulerDayView(GetOwner).LookAndFeelPainter;
end;

function TcxSchedulerDayViewStyles.GetSchedulerViewStyle: TcxSchedulerViewStyle;
begin
  Result := Scheduler.OptionsView.Style;
end;

//procedures
function cxCompareTime(const ATime1, ATime2: TDateTime): Integer;
begin
  Result := 0;
  if ATime1 < ATime2 then
    Result := -1
  else
    if ATime1 > ATime2 then
      Result := 1;
end;

function cxCompareButtons(AItem1, AItem2: TcxSchedulerMoreEventsButtonViewInfo): Integer;
begin
  Result := Byte(AItem1 <> nil) - Byte(AItem2 <> nil);
  if Result = 0 then
    Result := Byte(not AItem1.IsDown) - Byte(not AItem2.IsDown);
  if Result = 0 then
  begin
    if AItem1.IsDown then
      Result := cxCompareTime(AItem1.DateTime, AItem2.DateTime)
    else
      Result := cxCompareTime(AItem2.DateTime, AItem1.DateTime)
  end;
end;

function cxCompareEventViewInfoOrders(
  AInfo1, AInfo2: TcxSchedulerEventCellViewInfo): Integer;
var
  AAllDay1, AAllDay2: Boolean;
  AEvent1, AEvent2: TcxSchedulerEvent;
begin
  Result := Byte(AInfo2.Visible) - Byte(AInfo1.Visible);
  if Result = 0 then
    Result := Byte(AInfo1.Selected) - Byte(AInfo2.Selected);
  if Result = 0 then
    Result := Byte(AInfo2.Hidden) - Byte(AInfo1.Hidden);
  if Result = 0 then
  begin
    AEvent1 := AInfo1.Event;
    AEvent2 := AInfo2.Event;
    AAllDay1 := AEvent1.IsAllDayOrLonger;
    AAllDay2 := AEvent2.IsAllDayOrLonger;
    Result := Byte(AAllDay2) - Byte(AAllDay1);
    if Result <> 0 then Exit;
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
            Result := 0;
  end;
end;

{ TcxEventLayoutInfo }

constructor TcxEventLayoutInfo.Create(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime; AScale: Integer);
var
  AFinish: Integer;
begin
  Event := AEvent;
  AFinish := GetFinishCoordinate(AScale);
  if ADate > dxDateOf(Event.Start) then
    StartRow := 0
  else
    StartRow := Round(dxTimeOf(AEvent.Start) / MinuteToTime) div AScale;
  if ADate < dxDateOf(Event.Finish) then
    StopRow := cxMinutesPerDay div AScale - 1
  else
    StopRow := AFinish div AScale;
  if (StopRow <> StartRow) and (ADate = dxDateOf(Event.Finish)) and (AFinish mod AScale = 0) then
    Dec(StopRow);

  if dxDateOf(Event.Start) <> dxDateOf(Event.Finish) then
  begin
    if dxDateOf(ADate) = dxDateOf(Event.Start) then
      StopRow := cxMinutesPerDay div AScale - 1
    else
      StartRow := 0;
  end;
  IsBreakEvent := (StopRow = cxMinutesPerDay div AScale - 1) and (ADate = dxDateOf(Event.Finish));
end;

function TcxEventLayoutInfo.GetFinishCoordinate(AScale: Integer): Integer;
begin
  Result := Round(dxTimeOf(Event.Finish) / MinuteToTime);
end;

function TcxEventLayoutInfo.ShadowSize(AScaleFactor: TdxScaleFactor): Integer;
begin
  Result := TcxSchedulerPainterHelper.ShadowSize(AScaleFactor);
end;

procedure TcxEventLayoutInfo.CalculateDisplayRect(AFullColWidth, ARowHeight: Integer; AScaleFactor: TdxScaleFactor);
begin
  FDisplayRect := Rect(MulDiv(AFullColWidth, ColStart, ColCount), StartRow * ARowHeight,
    MulDiv(AFullColWidth, ColEnd, ColCount) - ShadowSize(AScaleFactor), (StopRow + 1) * ARowHeight + Byte(IsBreakEvent));
end;

procedure TcxEventLayoutInfo.CalculateTimes(const ADate: TDateTime; AScale, AOffset: Integer);
begin
  FFinishTime := ADate + (StopRow + 1 + AOffset) * AScale * MinuteToTime;
  FStartTime := ADate + (StartRow + AOffset)* AScale * MinuteToTime;
end;

{ TcxEventModernLayoutInfo }

constructor TcxEventModernLayoutInfo.Create(AEvent: TcxSchedulerControlEvent; const ADate: TDateTime; AScale: Integer);
begin
  inherited Create(AEvent, ADate, AScale);
  FIntersectedInfos := TcxObjectList.Create(False);
end;

destructor TcxEventModernLayoutInfo.Destroy;
begin
  FreeAndNil(FIntersectedInfos);
  inherited Destroy;
end;

procedure TcxEventModernLayoutInfo.AddIntersectedInfo(AInfo: TcxEventModernLayoutInfo);
begin
  FIntersectedInfos.Add(AInfo);
  AInfo.FIntersectedInfos.Add(Self);
end;

procedure TcxEventModernLayoutInfo.CalculateDisplayRect(AFullColWidth, ARowHeight: Integer; AScaleFactor: TdxScaleFactor);

  function GetRectLeft(const AInternalColStart, AInternalColWidth: Integer): Integer;
  begin
    Result := AInternalColStart * AInternalColWidth + AInternalColStart;
  end;

var
  AMaxRight, AInternalColWidth, ALeft, ATop: Integer;
  LTime: Double;
  AStart, AFinish: TDateTime;
begin
  AMaxRight := AFullColWidth - ShadowSize(AScaleFactor);
  AInternalColWidth := AMaxRight div ColCount;
  ALeft := GetRectLeft(ColStart, AInternalColWidth);
  AStart := Max(Event.Start, Start);
  AFinish := Min(Event.Finish, Finish);
  if (Finish - Start) <= 0 then
  begin
    FRealHeight := 0;
    ATop := 0;
  end
  else
  begin
    LTime := ((StopRow - StartRow + 1) * ARowHeight - 1) / (Finish - Start);
    ATop := Round((AStart - Start) * LTime);
    FRealHeight := Round((AFinish - Start) * LTime + 1) - ATop;
    Inc(ATop, StartRow * ARowHeight);
  end;
  FDisplayRect := cxRect(ALeft, ATop,
    Min(GetRectLeft(ColEnd, AInternalColWidth) - 1, AMaxRight),
    ATop + Max(FRealHeight - 1, ARowHeight - 1));
end;

function TcxEventModernLayoutInfo.GetFinishCoordinate(AScale: Integer): Integer;
begin
  Result := Round(dxTimeOf(Max(Event.Finish, Event.Start + AScale / cxMinutesPerDay)) / MinuteToTime);
  Result := IfThen(Result = 0, cxMinutesPerDay, Result);
end;

function TcxEventModernLayoutInfo.GetIntersectedInfo(AIndex: Integer): TcxEventModernLayoutInfo;
begin
  Result := nil;
  if (AIndex >= 0) and (AIndex < IntersectionCount)  then
    Result := FIntersectedInfos[AIndex] as TcxEventModernLayoutInfo;
end;

function TcxEventModernLayoutInfo.GetIntersectionCount: Integer;
begin
  Result := FIntersectedInfos.Count;
end;

{ TcxCalculateEventLayout }

procedure TcxCalculateEventLayout.Add(AEvent: TcxSchedulerControlEvent);
var
  AEventInfo: TcxEventLayoutInfo;
begin
  AEventInfo := CreateEventInfo(AEvent);
  if AEventInfo <> nil then
    inherited Add(AEventInfo);
end;

procedure TcxCalculateEventLayout.Calculate;
var
  I: Integer;
  AInfo: TcxEventLayoutInfo;
begin
  for I := 0 to Count - 1 do
  begin
    AInfo := Infos[I];
    PrepareInfoBeforeCalculateColStartAndColCount(AInfo);
    CalculateColStartAndColCount(AInfo, I);
    SetColCount(AInfo);
  end;
  for I := 0 to Count - 1 do
    CalculateColEnd(Infos[I]);
  for I := 0 to Count - 1 do
  begin
    AInfo := Infos[I];
    PrepareInfoBeforeCalculateDisplayRect(AInfo);
    AInfo.CalculateDisplayRect(FWidth, FRowHeight, ScaleFactor);
    OffsetDisplayRect(AInfo);
    AInfo.FDisplayRect.Right := Max(AInfo.FDisplayRect.Right,
      AInfo.FDisplayRect.Left + ScaleFactor.Apply(cxTimeLineWidth + cxTextOffset));
  end;
end;

procedure TcxCalculateEventLayout.Clear;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Infos[I].Free;
  inherited Clear;
end;

procedure TcxCalculateEventLayout.SetParams(const ADate: TDateTime;
  ATimeScale, ARowHeight: Integer; ALeft, ATop, AWidth: Integer);
begin
  FDate := ADate;
  FTimeScale := ATimeScale;
  FRowHeight := ARowHeight;
  FLeft := ALeft;
  FTop := ATop;
  FWidth := AWidth;
end;

procedure TcxCalculateEventLayout.CalculateColCount(AInfo: TcxEventLayoutInfo);
var
  I, J: Integer;
begin
  for I := AInfo.StartRow to AInfo.StopRow do
  begin
    AInfo.ColEnd := 0;
    for J := 0 to Count - 1 do
      if (I >= Infos[J].StartRow) and (I <= Infos[J].StopRow) then
        Inc(AInfo.ColEnd);
    if AInfo.ColCount < AInfo.ColEnd then
      AInfo.ColCount := AInfo.ColEnd;
  end;
end;

procedure TcxCalculateEventLayout.CalculateColEnd(AInfo: TcxEventLayoutInfo);
var
  I: Integer;
  AEventInfo: TcxEventLayoutInfo;
begin
  AInfo.ColEnd := AInfo.ColCount;
  for I := 0 to Count - 1 do
  begin
    AEventInfo := Infos[I];
    if (AEventInfo.ColStart > AInfo.ColStart) and
       (AEventInfo.ColStart < AInfo.ColEnd) and Intersect(AInfo, AEventInfo) then
    begin
      AInfo.ColEnd := AEventInfo.ColStart;
      if AInfo.ColEnd = AInfo.ColStart + 1 then Break;
    end;
  end;
end;

procedure TcxCalculateEventLayout.CalculateColStart(AInfo: TcxEventLayoutInfo; AIndex: Integer);
var
  I: Integer;
begin
  repeat
    AInfo.ColEnd := AInfo.ColStart;
    for I := AIndex - 1 downto 0 do
      if (Infos[I].ColStart = AInfo.ColStart) and Intersect(Infos[I], AInfo) then
      begin
        Inc(AInfo.ColStart);
        Break;
      end;
  until (AInfo.ColStart = AInfo.ColEnd) or (AInfo.ColStart = (AInfo.ColCount - 1));
end;

procedure TcxCalculateEventLayout.CalculateColStartAndColCount(AInfo: TcxEventLayoutInfo; AIndex: Integer);
begin
  CalculateColCount(AInfo);
  CalculateColStart(AInfo, AIndex);
end;

procedure TcxCalculateEventLayout.CalculateInfoRowsAndTimes(AInfo: TcxEventLayoutInfo);
begin
  AInfo.StartRow := Max(0, AInfo.StartRow - FViewInfo.PrintStartRow);
  AInfo.StopRow := Min(FViewInfo.FRowCount, AInfo.StopRow - FViewInfo.PrintStartRow);
  AInfo.CalculateTimes(FDate, FTimeScale, FViewInfo.PrintStartRow);
end;

function TcxCalculateEventLayout.CreateEventInfo(AEvent: TcxSchedulerControlEvent): TcxEventLayoutInfo;
begin
  Result := GetLayoutInfoClass.Create(AEvent, FDate, FTimeScale);
  with FViewInfo do
    if HasVisibleBounds and not IsEventVisible(Result.StartRow, Result.StopRow) then
      FreeAndNil(Result);
end;

function TcxCalculateEventLayout.GetLayoutInfoClass: TcxEventLayoutInfoClass;
begin
  Result := TcxEventLayoutInfo;
end;

function TcxCalculateEventLayout.GetScaleFactor: TdxScaleFactor;
begin
  Result := FViewInfo.ScaleFactor;
end;

function TcxCalculateEventLayout.Intersect(AInfo1, AInfo2: TcxEventLayoutInfo): Boolean;
begin
  Result := (AInfo1.StartRow <= AInfo2.StopRow) and
    (AInfo2.StartRow <= AInfo1.StopRow);
end;

procedure TcxCalculateEventLayout.OffsetDisplayRect(AInfo: TcxEventLayoutInfo);
begin
  OffsetRect(AInfo.FDisplayRect, Left, Top - Byte(FViewInfo.Adapter.IsPrinting));
end;

procedure TcxCalculateEventLayout.PrepareInfoBeforeCalculateColStartAndColCount(AInfo: TcxEventLayoutInfo);
begin
//
end;

procedure TcxCalculateEventLayout.PrepareInfoBeforeCalculateDisplayRect(AInfo: TcxEventLayoutInfo);
begin
  CalculateInfoRowsAndTimes(AInfo);
end;

procedure TcxCalculateEventLayout.SetColCount(
  AInfo: TcxEventLayoutInfo);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    if (Infos[I].ColCount < AInfo.ColCount) and Intersect(Infos[I], AInfo) then
    begin
      Infos[I].ColCount := AInfo.ColCount;
      SetColCount(Infos[I]);
    end;
  end;
end;

function TcxCalculateEventLayout.GetInfo(
  AIndex: Integer): TcxEventLayoutInfo;
begin
  Result := TcxEventLayoutInfo(List[AIndex]);
end;

{ TcxCalculateModernEventLayout }

procedure TcxCalculateModernEventLayout.CalculateColEnd(AInfo: TcxEventLayoutInfo);
var
  I, ACount: Integer;
  AEventInfo: TcxEventLayoutInfo;
begin
  AInfo.ColEnd := AInfo.ColCount;
  ACount := (AInfo as TcxEventModernLayoutInfo).IntersectionCount;
  for I := 0 to ACount - 1 do
  begin
    AEventInfo := (AInfo as TcxEventModernLayoutInfo).IntersectedInfo[I];
    if AEventInfo.ColStart > AInfo.ColStart then
      AInfo.ColEnd := Min(AInfo.ColEnd, AEventInfo.ColStart);
    if AInfo.ColEnd = 1 then
      Break;
  end
end;

procedure TcxCalculateModernEventLayout.CalculateColStartAndColCount(AInfo: TcxEventLayoutInfo; AIndex: Integer);

  procedure InternalRecheckColStart;
  var
    I, ACount: Integer;
    AEventInfo: TcxEventLayoutInfo;
    AIsColStartChanged: Boolean;
  begin
    repeat
      AIsColStartChanged := False;
      ACount := (AInfo as TcxEventModernLayoutInfo).IntersectionCount;
      for I := 0 to ACount - 1 do
      begin
        AEventInfo := (AInfo as TcxEventModernLayoutInfo).IntersectedInfo[I];
        if AInfo.ColStart = AEventInfo.ColStart then
        begin
          AInfo.ColStart := AEventInfo.ColStart + 1;
          AIsColStartChanged := True;
        end;
      end;
    until not AIsColStartChanged;
  end;

var
  I: Integer;
  AEventInfo: TcxEventLayoutInfo;
begin
  AInfo.ColStart := 0;
  AInfo.ColCount:= 1;
  for I := AIndex - 1 downto 0 do
  begin
    AEventInfo := Infos[I];
    if Intersect(AInfo, AEventInfo) then
    begin
      if AInfo.ColStart = AEventInfo.ColStart then
      begin
        AInfo.ColStart := AEventInfo.ColStart + 1;
        InternalRecheckColStart;
      end;
      AInfo.ColCount := Max(AInfo.ColStart + 1, AEventInfo.ColCount);
      (AInfo as TcxEventModernLayoutInfo).AddIntersectedInfo(AEventInfo as TcxEventModernLayoutInfo);
    end;
  end;
end;

function TcxCalculateModernEventLayout.GetLayoutInfoClass: TcxEventLayoutInfoClass;
begin
  Result := TcxEventModernLayoutInfo;
end;

function TcxCalculateModernEventLayout.Intersect(AInfo1, AInfo2: TcxEventLayoutInfo): Boolean;
begin
  AInfo1.CalculateDisplayRect(FWidth, FRowHeight, ScaleFactor);
  AInfo2.CalculateDisplayRect(FWidth, FRowHeight, ScaleFactor);
  Result := Min(AInfo1.DisplayRect.Bottom, AInfo2.DisplayRect.Bottom) >= Max(AInfo1.DisplayRect.Top, AInfo2.DisplayRect.Top);
end;

procedure TcxCalculateModernEventLayout.OffsetDisplayRect(AInfo: TcxEventLayoutInfo);
begin
  OffsetRect(AInfo.FDisplayRect, Left, Top - Byte(FViewInfo.Adapter.IsPrinting) - 1);
end;

procedure TcxCalculateModernEventLayout.PrepareInfoBeforeCalculateColStartAndColCount(AInfo: TcxEventLayoutInfo);
begin
  CalculateInfoRowsAndTimes(AInfo);
end;

procedure TcxCalculateModernEventLayout.PrepareInfoBeforeCalculateDisplayRect(AInfo: TcxEventLayoutInfo);
begin
//
end;

procedure TcxCalculateModernEventLayout.SetColCount(AInfo: TcxEventLayoutInfo);
var
  I, ACount: Integer;
  AEventInfo: TcxEventLayoutInfo;
begin
  ACount := (AInfo as TcxEventModernLayoutInfo).IntersectionCount;
  for I := 0 to ACount - 1 do
  begin
    AEventInfo := (AInfo as TcxEventModernLayoutInfo).IntersectedInfo[I];
    if AInfo.ColCount > AEventInfo.ColCount then
    begin
      AEventInfo.ColCount := AInfo.ColCount;
      SetColCount(AEventInfo);
    end;
  end
end;

{ TcxSchedulerDayView }

constructor TcxSchedulerDayView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FAllDayAreaScrollBar := adsbDefault;
  FDayHeaderArea := True;
  FDayHeaderModernStyleDisplayMode := hdmDefault;
  FTimeScale := cxDefaultTimeScale;
  FHeaderContainer := True;
  EventImagesLayout := eilAuto;
  FEventShadows := True;
  FShowAllDayEventsInContentArea := False;
  FHybridScrollbarsManager := TdxHybridScrollbarsManager.Create(Self);
  FAllDayScrollBar := TdxScrollBarWrapper.Create(Self);
  FAllDayScrollBar.OnScroll := DoAllDayScroll;
end;

destructor TcxSchedulerDayView.Destroy;
begin
  FreeAndNil(FHybridScrollbarsManager);
  FreeAndNil(FAllDayScrollBar);
  inherited Destroy;
end;

procedure TcxSchedulerDayView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerDayView then
  begin
    with Source as TcxSchedulerDayView do
    begin
      Self.FAllDayAreaScrollBar := AllDayAreaScrollBar;
      Self.FAlwaysShowEventTime := AlwaysShowEventTime;
      Self.FDayHeaderArea := DayHeaderArea;
      Self.FHeaderContainer := HeaderContainer;
      Self.FTimeRulerMinutes := TimeRulerMinutes;
      Self.FEventShadows := EventShadows;
      Self.FShowAllDayEventsInContentArea := ShowAllDayEventsInContentArea;
      Self.FTimeScale := TimeScale;
      Self.TimeRulerPopupMenu := TimeRulerPopupMenu;
      Self.FWorkTimeOnly := WorkTimeOnly;
      Self.FDayHeaderModernStyleDisplayMode := FDayHeaderModernStyleDisplayMode;
    end;
  end;
  inherited Assign(Source);
end;

procedure TcxSchedulerDayView.MakeWorkTimeVisible;
begin
  if Visible then
  begin
    if WorkTimeOnly then
      ViewInfo.FTopIndex := 0
    else
      ViewInfo.FTopIndex := Round(WorkStart / MinuteToTime) div TimeScale;
  end;
end;

procedure TcxSchedulerDayView.DeactivateView;
begin
  AllDayScrollBar.Visible := False;
  inherited;
end;

procedure TcxSchedulerDayView.GetProperties(AProperties: TStrings);
begin
  inherited GetProperties(AProperties);
  AProperties.Add('TimeScale')
end;

procedure TcxSchedulerDayView.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  inherited GetPropertyValue(AName, AValue);
  if AName = 'TimeScale' then
    AValue := TimeScale;
end;

procedure TcxSchedulerDayView.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  inherited SetPropertyValue(AName, AValue);
  if AName = 'TimeScale' then
    TimeScale := AValue;
end;

function TcxSchedulerDayView.GetTimeScaleStep: Integer;
begin
  Result := TimeScale;
end;

procedure TcxSchedulerDayView.SetTimeScaleStep(const AValue: Integer);
begin
  TimeScale := AValue;
end;

procedure TcxSchedulerDayView.CheckUIPosition;
begin
  ViewInfo.CalculateAllDayScrollBar;
end;

function TcxSchedulerDayView.GetOwnerControl: TcxControl;
begin
  Result := Scheduler;
end;

function TcxSchedulerDayView.HasVisibleUI: Boolean;
begin
  Result := AllDayScrollBar.Visible;
end;

procedure TcxSchedulerDayView.HideUI;
begin
  AllDayScrollBar.Visible := False;
end;

// IdxHybridScrollbarOwner
function TcxSchedulerDayView.GetHybridScrollbarBaseColor: TColor;
begin
  Result := TcxCustomSchedulerAccess(Scheduler).GetHybridScrollbarBaseColor;
end;

function TcxSchedulerDayView.GetManager: TdxHybridScrollbarsManager;
begin
  Result := FHybridScrollbarsManager;
end;

procedure TcxSchedulerDayView.InvalidateScrollbars;
begin
  FAllDayScrollBar.Invalidate;
end;
//

procedure TcxSchedulerDayView.CreateSubClasses;
begin
  FStyles := CreateStyles;
  inherited CreateSubClasses;
  FTimeRulerPopupMenu := CreateTimeRulerPopupMenu;
  FAllDayContainerGestureScrollHelper := TcxSchedulerAllDayContainerGestureScrollHelper.Create(Self);
end;

procedure TcxSchedulerDayView.DestroySubClasses;
begin
  FreeAndNil(FAllDayContainerGestureScrollHelper);
  FTimeRulerPopupMenu.Free;
  inherited DestroySubClasses;
  FStyles.Free;
end;

function TcxSchedulerDayView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerDayViewController.Create(Self);
end;

function TcxSchedulerDayView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerDayViewHitTest.Create(Self);
end;

function TcxSchedulerDayView.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerDayViewPainter.Create(Self);
end;

function TcxSchedulerDayView.CreateStyles: TcxSchedulerDayViewStyles;
begin
  Result := TcxSchedulerDayViewStyles.Create(Self);
end;

function TcxSchedulerDayView.CreateTimeRulerPopupMenu: TcxSchedulerTimeRulerPopupMenu;
begin
  Result := TcxSchedulerTimeRulerPopupMenu.Create(Scheduler);
end;

function TcxSchedulerDayView.CreateViewAdapter: TcxCustomResourceViewAdapter;
begin
  Result := TcxSchedulerDayViewAdapter.Create(Self);
end;

function TcxSchedulerDayView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerDayViewViewInfo.Create(Self);
end;

procedure TcxSchedulerDayView.DoCustomDrawContainer(ACell: TcxSchedulerContainerCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawContainer) then
    FOnCustomDrawContainer(Self, Canvas, ACell, ADone);
end;

procedure TcxSchedulerDayView.DoCustomDrawTimeRuler(ACell: TcxSchedulerTimeRulerCellViewInfo; var ADone: Boolean);
begin
  if Assigned(FOnCustomDrawTimeRuler) then
    FOnCustomDrawTimeRuler(Self, Canvas, ACell, ADone);
end;

function TcxSchedulerDayView.DoShowPopupMenu(X, Y: Integer): Boolean;
begin
  if HitTest.HitAtTimeRuler then
    Result := TimeRulerPopupMenu.Popup(X, Y)
  else
    Result := inherited DoShowPopupMenu(X, Y);
end;

function TcxSchedulerDayView.EventContentSelected(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := AEvent.IsAllDayOrLonger and not ShowAllDayEventsInContentArea;
end;

function TcxSchedulerDayView.GetEditWithSingleLineEditor(AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := IsHeaderEvent(AEvent) and not ShowAllDayEventsInContentArea;
end;

function TcxSchedulerDayView.GetFirstVisibleTime: TDateTime;
begin
  Result := ViewInfo.TopIndex * TimeScale / 24 / 60;
  if WorkTimeOnly then
    Result := Result + Scheduler.OptionsView.WorkStart;
end;

function TcxSchedulerDayView.GetGestureClient(const APoint: TPoint): IdxGestureClient;
begin
  if cxRectPtIn(ViewInfo.FAllDayAreaBounds, APoint) then
    Result := FAllDayContainerGestureScrollHelper
  else
    Result := inherited GetGestureClient(APoint);
end;

function TcxSchedulerDayView.GetLastVisibleTime: TDateTime;
begin
  Result := GetFirstVisibleTime + (ViewInfo.VisibleRowCount - 1) * TimeScale / 24 / 60;
end;

function TcxSchedulerDayView.GetTimeIncrement: TDateTime;
begin
  Result := dxTimeOf(TimeScale * MinuteToTime);
end;

function TcxSchedulerDayView.GetViewContentRect: TRect;
begin
  Result := inherited GetViewContentRect;
  Result.Left := ViewInfo.FHeaderOffsetLeft;
end;

function TcxSchedulerDayView.IsDayView: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerDayView.MakeEventVisible(AEvent: TcxSchedulerControlEvent;
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem);
var
  I, DH: Integer;
  R, R1: TRect;
  AViewInfo: TcxSchedulerEventCellViewInfo;
begin
  if Visible and AEvent.IsAllDayOrLonger then Exit;
  for I := 0 to 1 do
  begin
    if FindEventViewInfo(AEvent, ADate, AResource, AViewInfo) and
      not AViewInfo.Hidden then
    begin
      if not AViewInfo.Visible then
        DH := ViewInfo.GetTimeRow(AViewInfo.ContentStart, True)
      else
      begin
        DH := 0;
        R := AViewInfo.Bounds;
        R1 := AViewInfo.ClipRect;
        if cxRectIsEqual(R, R1) then Exit;
        if R.Top < R1.Top then
          DH := (R.Top - R1.Top) div ViewInfo.FContentLineHeight
        else
          if (I = 0) and (R.Bottom > R1.Bottom) then
            DH := (R.Bottom - R1.Bottom) div ViewInfo.FContentLineHeight;
        Inc(DH, ViewInfo.FTopIndex);
      end;
      if ViewInfo.FTopIndex <> DH then
      begin
        ViewInfo.FTopIndex := DH;
        LayoutChanged;
      end;
    end
    else
    begin
      ViewInfo.FTopIndex := ViewInfo.GetTimeRow(AEvent.Start, True);
      LayoutChanged;
    end;
  end;
end;

function TcxSchedulerDayView.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerDayView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  TimeRulerPopupMenu.Notification(AComponent, Operation);
end;

procedure TcxSchedulerDayView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  HideHintOnScroll(AScrollCode);
  if AScrollCode = scEndScroll then
    Exit;
  inherited Scroll(AScrollBarKind, AScrollCode, AScrollPos);
  if AScrollCode = scTrack then
    ShowHintOnScroll(GetScrollTimeHint, AScrollBarKind);
end;

procedure TcxSchedulerDayView.ScrollVisibleDays(AScrollUp: Boolean);
begin
  ScrollSelectedDays(Byte(AScrollUp) * 2 - 1);
end;

procedure TcxSchedulerDayView.DoCreateScrollBars;
begin
  FAllDayScrollBar.CreateInnerScrollBar;
end;

procedure TcxSchedulerDayView.DoDestroyScrollBars;
begin
  FAllDayScrollBar.DestroyInnerScrollBar;
end;

procedure TcxSchedulerDayView.InitScrollBars;
begin
  FAllDayScrollBar.InitControl;
end;

procedure TcxSchedulerDayView.TimeChanged;
begin
  Invalidate;
end;

procedure TcxSchedulerDayView.VisibleChanged;
begin
  MakeWorkTimeVisible;
  inherited VisibleChanged;
end;

procedure TcxSchedulerDayView.DoAllDayScroll(Sender: TObject;
  ScrollCode: TScrollCode; var ScrollPos: Integer);
begin
  ViewInfo.AllDayScroll(ScrollCode, ScrollPos);
end;

function TcxSchedulerDayView.GetHitTest: TcxSchedulerDayViewHitTest;
begin
  Result := TcxSchedulerDayViewHitTest(inherited HitTest);
end;

function TcxSchedulerDayView.GetTouchScrollUIOwner(const APoint: TPoint): IdxTouchScrollUIOwner;
begin
  if cxRectPtIn(ViewInfo.FAllDayAreaBounds, APoint) then
    Result := Self
  else
    Result := inherited GetTouchScrollUIOwner(APoint);
end;

function TcxSchedulerDayView.GetViewInfo: TcxSchedulerDayViewViewInfo;
begin
  Result := TcxSchedulerDayViewViewInfo(inherited ViewInfo)
end;

procedure TcxSchedulerDayView.SetAlwaysShowEventTime(AValue: Boolean);
begin
  if AValue <> FAlwaysShowEventTime then
  begin
    FAlwaysShowEventTime := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetAutoContentHeight(AValue: Boolean);
begin
  if AValue <> FAutoContentHeight then
  begin
    FAutoContentHeight := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetEventShadows(AValue: Boolean);
begin
  if AValue <> FEventShadows then
  begin
    FEventShadows := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetHeaderContainer(AValue: Boolean);
begin
  if AValue <> FHeaderContainer then
  begin
    FHeaderContainer := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetAllDayAreaScrollBar(AValue:
  TcxSchedulerAllDayAreaScrollBar);
begin
  if AValue <> FAllDayAreaScrollBar then
  begin
    FAllDayAreaScrollBar := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetDayHeaderArea(
  AValue: Boolean);
begin
  if AValue <> FDayHeaderArea then
  begin
    FDayHeaderArea := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetDayHeaderModernStyleDisplayMode(AValue: TcxSchedulerDayHeaderModernStyleDisplayMode);
begin
  if AValue <> FDayHeaderModernStyleDisplayMode then
  begin
    FDayHeaderModernStyleDisplayMode := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetTimeRulerMinutes(AValue: Boolean);
begin
  if AValue <> FTimeRulerMinutes then
  begin
    FTimeRulerMinutes := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetShowAllDayEventsInContentArea(AValue: Boolean);
begin
  if AValue <> FShowAllDayEventsInContentArea then
  begin
    FShowAllDayEventsInContentArea := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerDayView.SetStyles(AValue: TcxSchedulerDayViewStyles);
begin
  FStyles.Assign(AValue);
end;

procedure TcxSchedulerDayView.SetTimeRulerPopupMenu(AValue: TcxSchedulerTimeRulerPopupMenu);
begin
  FTimeRulerPopupMenu.Assign(AValue);
end;

procedure TcxSchedulerDayView.SetTimeScale(AValue: Integer);
begin
  AValue := Min(60, Max(5, AValue));
  AValue := 60 div Max(1, 60 div AValue);
  if AValue <> FTimeScale then
  begin
    while 60 mod AValue <> 0 do Inc(AValue);
    FTimeScale := AValue;
    MakeWorkTimeVisible;
    Refresh;
  end;
end;

procedure TcxSchedulerDayView.SetWorkTimeOnly(AValue: Boolean);
begin
  if AValue <> FWorkTimeOnly then
  begin
    FWorkTimeOnly := AValue;
    if (ViewInfo <> nil) then
      ViewInfo.FTopIndex := 0;
    Changed;
  end;
end;

{ TcxSchedulerAllDayContainerGestureScrollHelper }

constructor TcxSchedulerAllDayContainerGestureScrollHelper.Create(
  AView: TcxSchedulerDayView);
begin
  inherited Create;
  FView := AView;
end;

function TcxSchedulerAllDayContainerGestureScrollHelper.AllowGesture(AGestureId: Integer): Boolean;
begin
  Result := AGestureId = GID_PAN;
end;

function TcxSchedulerAllDayContainerGestureScrollHelper.AllowPan(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbVertical;
end;

procedure TcxSchedulerAllDayContainerGestureScrollHelper.BeginGestureScroll(APos: TPoint);
begin
  TcxCustomSchedulerAccess(FView.Scheduler).ShowTouchScrollUI(FView);
end;

procedure TcxSchedulerAllDayContainerGestureScrollHelper.EndGestureScroll;
begin
  TcxCustomSchedulerAccess(FView.Scheduler).HideTouchScrollUI(FView);
end;

procedure TcxSchedulerAllDayContainerGestureScrollHelper.GestureScroll(ADeltaX, ADeltaY: Integer);
var
  ANewPos: Integer;
begin
  ANewPos := FView.AllDayScrollBar.Position - ADeltaY div 10;
  TcxCustomSchedulerAccess(FView.Scheduler).GestureHelper.CheckOverpan(FView.AllDayScrollBar.Kind, ANewPos,
    0, FView.AllDayScrollBar.Max, ADeltaX, ADeltaY);
  ANewPos := EnsureRange(ANewPos, 0, FView.AllDayScrollBar.Max);
  FView.ViewInfo.AllDayScroll(scTrack, ANewPos);
end;

function TcxSchedulerAllDayContainerGestureScrollHelper.GetPanOptions: Integer;
begin
  Result := dxTouchPanOptions and not GC_PAN_WITH_SINGLE_FINGER_HORIZONTALLY;
end;

function TcxSchedulerAllDayContainerGestureScrollHelper.IsPanArea(const APoint: TPoint): Boolean;
begin
  Result := True;
end;

function TcxSchedulerAllDayContainerGestureScrollHelper.NeedPanningFeedback(AScrollKind: TScrollBarKind): Boolean;
begin
  Result := AScrollKind = sbVertical;
end;

{ TcxSchedulerDayViewAdapter }

function TcxSchedulerDayViewAdapter.GetPrintRange(Index: Integer): TDateTime;
begin
  with TcxSchedulerDayView(View) do
  begin
    if not WorkTimeOnly then
      Result := inherited GetPrintRange(Index)
    else
    begin
      if Index = 0 then
        Result := dxTimeOf(WorkStart)
      else
        Result := dxTimeOf(WorkFinish);
    end;
  end;
end;

procedure TcxSchedulerDayViewAdapter.Store;
begin
  FTopIndex := TcxSchedulerDayViewViewInfo(ViewInfo).FTopIndex;
end;

procedure TcxSchedulerDayViewAdapter.Restore;
begin
  TcxSchedulerDayViewViewInfo(ViewInfo).FTopIndex := FTopIndex;
end;

{ TcxSchedulerDayViewContentCellCustomViewInfo }

procedure TcxSchedulerDayViewContentCellCustomViewInfo.SetBorderColor(const AResourceColor: Integer;
  AIsHourSeparator, AIsWorkTime: Boolean);
begin
  FIsHourSeparator := AIsHourSeparator;
  FBorderColor := PainterHelper.GetSeparatorColor(AResourceColor, AIsHourSeparator, AIsWorkTime, GetViewStyle);
end;

{ TcxSchedulerDayViewContentCellViewInfo }

function TcxSchedulerDayViewContentCellViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsClassic;
end;

{ TcxSchedulerDayViewContentCellModernViewInfo }

constructor TcxSchedulerDayViewContentCellModernViewInfo.Create(APainter: TcxCustomLookAndFeelPainter; const ABounds, AVisibleRect: TRect;
  const AViewParams: TcxViewParams; AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(APainter, ABounds, AVisibleRect, AViewParams, AScaleFactor, AUseRightToLeftAlignment);
  FLeftBorderColor := BorderColor;
end;

procedure TcxSchedulerDayViewContentCellModernViewInfo.DoDraw;
const
  ALeftBorderRTL: array[Boolean] of TcxBorders = ([bLeft], [bRight]);
var
  ABorderColor1: TColor;
  ABorderColor2: TColor;
  AContentColor: TColor;
begin
  ABorderColor1 := BorderColor;
  AContentColor := ViewParams.Color;
  ABorderColor2 := AContentColor;

  if (TimeEvent <> nil) and (TimeEvent.Event.State > 0) and not Selected then
  begin
    ABorderColor1 := ExternalPainter.GetColorizedColor(ABorderColor1, TimeEvent);
    AContentColor := ExternalPainter.GetColorizedColor(AContentColor, TimeEvent);
    ABorderColor2 := AContentColor;

    SpecialDrawRect(Bounds, [], AContentColor, ABorderColor1);
    if TimeEvent.Event.State = 1 then
      cxFillRectWithCustomBrush(Canvas.Canvas, StateBrushes[9], Bounds, AContentColor, ABorderColor1, ViewParams.Bitmap <> nil);

    AContentColor := clNone;
  end;

  if IsHourSeparator then
  begin
    SpecialDrawRect(Bounds, Borders - ALeftBorderRTL[UseRightToLeftAlignment], AContentColor, ABorderColor1);
    Canvas.FrameRect(Bounds, LeftBorderColor, 1, ALeftBorderRTL[UseRightToLeftAlignment]);
  end
  else
  begin
    SpecialDrawRect(Bounds, Borders - [bBottom], AContentColor, LeftBorderColor);
    cxFillRectWithCustomBrush(Canvas.Canvas,
      cxSchedulerModernMinorTimeHorizontalSeparatorBrush,
      cxRectSetBottom(Bounds, Bounds.Bottom, 1), ABorderColor2, ABorderColor1);
  end;
end;

function TcxSchedulerDayViewContentCellModernViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsModern;
end;

procedure TcxSchedulerDayViewContentCellModernViewInfo.SetBorderColor(const AResourceColor: Integer; AIsHourSeparator, AIsWorkTime: Boolean);
begin
  inherited SetBorderColor(AResourceColor, AIsHourSeparator, AIsWorkTime);
  if not AIsHourSeparator then
    FLeftBorderColor := PainterHelper.GetSeparatorColor(AResourceColor, False, AIsWorkTime, GetViewStyle)
  else
    FLeftBorderColor := BorderColor;
end;

{ TcxSchedulerDayViewEventCellCustomViewInfo }

procedure TcxSchedulerDayViewEventCellCustomViewInfo.CalculateEventTimeVisibility;
begin
  inherited CalculateEventTimeVisibility;
  if ShowTimeAsClock and ViewData.ShowFinishTime then
    ViewData.ShowFinishTime := ViewData.ContentFinish > EventFinish;
  if ShowTimeAsClock and ViewData.ShowStartTime then
    ViewData.ShowStartTime := ViewData.ContentStart < EventStart;
end;

procedure TcxSchedulerDayViewEventCellCustomViewInfo.CalculateShowTimeAsClock;
begin
  ViewData.ShowTimeAsClock := IsHorzSizing or (ViewData.ShowTimeAsClock and not IsDetailInfo);
end;

function TcxSchedulerDayViewEventCellCustomViewInfo.GetDetailCaptionFlagValue: Boolean;
begin
  Result := inherited GetDetailCaptionFlagValue or IsShowHeaderEventsInContentArea;
end;

function TcxSchedulerDayViewEventCellCustomViewInfo.GetDetailInfoFlagValue: Boolean;
begin
  Result := inherited GetDetailInfoFlagValue or IsShowHeaderEventsInContentArea;
end;

procedure TcxSchedulerDayViewEventCellCustomViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  if IsDetailInfo then
  begin
    if ShowTimeLine and PtInRect(FTimeLineRect, AHitTest.HitPoint) then
      AHitTest.SetDragKind(edkEventDragRect)
    else
    begin
      if (AHitTest.HitY - Bounds.Top) <= cxHitDelta then
        AHitTest.SetDragKind(edkResizeStart)
      else
      if (Bounds.Bottom - AHitTest.HitY) <= cxHitDelta then
        AHitTest.SetDragKind(edkResizeEnd);
    end;
  end
  else
    if IsHeaderEvent and not IsShowHeaderEventsInContentArea then
      InitHitTestForHorzEvent(AHitTest);
end;

function TcxSchedulerDayViewEventCellCustomViewInfo.IsHorzSizing: Boolean;
begin
  Result := IsHeaderEvent and not IsShowHeaderEventsInContentArea;
end;

function TcxSchedulerDayViewEventCellCustomViewInfo.IsNeedDrawTime: Boolean;
begin
  if IsDetailInfo then
    Result := False
  else
    Result := inherited IsNeedDrawTime;
end;

function TcxSchedulerDayViewEventCellCustomViewInfo.IsShowHeaderEventsInContentArea: Boolean;
begin
  Result := ViewData.ShowAllDayEventsInContentArea;
end;

{ TcxSchedulerDayViewEventCellViewInfo }

procedure TcxSchedulerDayViewEventCellViewInfo.AfterDraw;
begin
  if not IsDrawShadowFirst then
    DrawShadow;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.BeforeDraw;
begin
  if IsDrawShadowFirst then
    DrawShadow;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.CalculateCaptions;
begin
  if IsDetailInfo and ShowStartTime then
  begin
    inherited CalculateCaptions; //todo: optimization - check with PS
    if EventStart <> EventFinish then
      ViewData.Caption := StartText + '-' + FinishText + ' ' + ViewData.Caption
    else
      ViewData.Caption := StartText + ' ' + ViewData.Caption;
  end;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.CalculateItemsLayout;
begin
  inherited CalculateItemsLayout;
  if ViewData.ShowTimeLine then
      Exclude(FBorders, bLeft);
end;

procedure TcxSchedulerDayViewEventCellViewInfo.CalculateNonDetailEventTimeVisibility;
begin
  if Event.AllDayEvent and not IsShowHeaderEventsInContentArea then
  begin
    ViewData.ShowFinishTime := False;
    ViewData.ShowStartTime := False;
  end
  else
  begin
    ViewData.ShowFinishTime := (ViewData.ContentFinish >= EventFinish) and
      (ViewData.ShowFinishTime or (Event.Duration >= 1));
    ViewData.ShowStartTime := (ContentStart <= EventStart);
  end;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.CalculateTimeLineLayout;
var
  LTime: Double;
  AStart, AFinish: TDateTime;
  ATop, AHeight: Integer;
begin
  AStart := Max(Event.Start, ContentStart);
  AFinish := Min(Event.Finish, ContentFinish);
  if (ContentFinish - ContentStart) <= 0 then
  begin
    AHeight := 0;
    ATop := 0;
  end
  else
  begin
    LTime := (cxRectHeight(FBounds) - 2) / (ContentFinish - ContentStart);
    ATop := Round((AStart - ContentStart) * LTime) + 1;
    AHeight := Round((AFinish - ContentStart) * LTime + 1) - ATop;
  end;
  Inc(ATop, FBounds.Top);
  FTimeLineRect := cxRectSetRight(FBounds, FBounds.Left, ScaleFactor.Apply(cxTimeLineWidth));
  FEventTimeRect := cxRectBounds(FTimeLineRect.Left, ATop, ScaleFactor.Apply(cxTimeLineWidth), AHeight);
end;

function TcxSchedulerDayViewEventCellViewInfo.CanShowHint: Boolean;
begin
  Result := inherited CanShowHint and not IsShowHeaderEventsInContentArea;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.DrawSelection;
var
  R: TRect;
begin
  if ShowTimeLine and Selected and FExternalPainter.NeedDrawSelection then
  begin
    R := cxRectSetTop(Bounds, Bounds.Top - ScaleFactor.Apply(cxTimeLineWidth - 1), ScaleFactor.Apply(cxTimeLineWidth));
    if not UseRightToLeftAlignment then
      Dec(R.Left, ScaleFactor.Apply(cxTimeLineWidth))
    else
      Inc(R.Right, ScaleFactor.Apply(cxTimeLineWidth));
    PainterHelper.DrawState(Canvas, R, Event.State, cxBordersAll, ViewData.BorderColor);
    R := cxRectSetTop(R, Bounds.Bottom - 1);
    PainterHelper.DrawState(Canvas, R, Event.State, cxBordersAll, ViewData.BorderColor);
  end;
end;

procedure TcxSchedulerDayViewEventCellViewInfo.DrawShadow;
begin
  if ViewData.DrawShadows and not Hidden and not Selected and ShowTimeLine then
    ExternalPainter.DrawShadow(Canvas, Bounds, ViewData.VisibleRect, ScaleFactor);
end;

procedure TcxSchedulerDayViewEventCellViewInfo.DrawState;
begin
  Canvas.Rectangle(FTimeLineRect, ViewData.TimeLineParams, cxBordersAll, GetTimeLineBorderColor);
  DrawState(Canvas, FEventTimeRect, [bLeft, bRight], GetTimeLineBorderColor);
end;

function TcxSchedulerDayViewEventCellViewInfo.GetBoundsForHitTest: TRect;
begin
  Result := inherited GetBoundsForHitTest;
  if Selected then
  begin
    Dec(Result.Top, ScaleFactor.Apply(cxTimeLineWidth));
    Inc(Result.Bottom, ScaleFactor.Apply(cxTimeLineWidth));
  end
  else
    Result := inherited GetBoundsForHitTest;
end;

function TcxSchedulerDayViewEventCellViewInfo.GetEditingRect: TRect;
begin
  Result := inherited GetEditingRect;
  if IsDetailCaption then
    Inc(Result.Top);
end;

function TcxSchedulerDayViewEventCellViewInfo.GetTimeLineBorderColor: TColor;
begin
  Result := ViewData.BorderColor;
end;

function TcxSchedulerDayViewEventCellViewInfo.GetSelectionBoundsExtends: TRect;
begin
  Result := cxNullRect;
  if Selected then
  begin
    Result := ExternalPainter.GetEventSelectionExtends(Self);
    if ViewData.ShowTimeLine then
    begin
      Result.Bottom := Max(Result.Bottom, ScaleFactor.Apply(cxTimeLineWidth));
      Result.Top := Max(Result.Top, ScaleFactor.Apply(cxTimeLineWidth));
    end;
  end;
end;

function TcxSchedulerDayViewEventCellViewInfo.IsDrawShadowFirst: Boolean;
begin
  Result := ExternalPainter.DrawShadowFirst;
end;

function TcxSchedulerDayViewEventCellViewInfo.IsTimeLineVisible: Boolean;
begin
  Result := not IsHeaderEvent or IsShowHeaderEventsInContentArea;
end;

{ TcxSchedulerDayViewEventCellModernViewInfo }

procedure TcxSchedulerDayViewEventCellModernViewInfo.AfterDraw;
begin
  ViewData.BorderColor := FRealBorderColor;
  FBounds.Left := FRealBoundsLeft;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.BeforeDraw;
begin
  FRealBorderColor := ViewData.BorderColor;
  FRealBoundsLeft := Bounds.Left;
  if ActualBorderColorIsNone then
  begin
    ViewData.BorderColor := ViewParams.Color;
    Dec(FBounds.Left);
  end;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateBorderAttributes;
begin
  inherited CalculateBorderAttributes;
  ViewData.BorderColor := ExternalPainter.GetEventBorderColor(Self);
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateCaptions;
var
  ACaption: string;
begin
  FLocation := Trim(Event.Location);
  ACaption := ViewData.Caption;
  if (FLocation <> '') and FCaptionAndLocationCanPlaceOneLineOnly then
    ACaption := ACaption + ';';
  ViewData.Caption := ACaption;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateDetailInfo;
var
  AStyle: TFontStyles;
begin
  inherited CalculateDetailInfo;
  FCaptionIsBold := IsDetailInfo or (Event.Duration > 1);
  AStyle := Canvas.Font.Style;
  Canvas.Font.Style := GetCaptionFontStyle;
  FCaptionAndLocationCanPlaceOneLineOnly :=
    (cxRectHeight(Bounds) < 2 * (Canvas.FontHeight(Canvas.Font) + FDetailCaptionVertOffset));
  Canvas.Font.Style := AStyle;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer);
var
  AStyle: TFontStyles;
begin
  if not FCaptionIsBold then
    inherited CalculateHeaderEventNeededCaptionWidth(AFullWidth, ACaptionOnlyWidth)
  else
  begin
    ACaptionOnlyWidth := 0;
    AStyle := Canvas.Font.Style;
    Canvas.Font.Style := GetCaptionFontStyle;
    ACaptionOnlyWidth := cxTextWidth(Canvas.Font, Caption);
    Canvas.Font.Style := AStyle;
    AFullWidth := ACaptionOnlyWidth;
    if FLocation <> '' then
      Inc(AFullWidth, cxTextWidth(Canvas.Font, FLocation));
  end;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateNonDetailEventTimeVisibility;
begin
  if Event.AllDayEvent and not IsShowHeaderEventsInContentArea then
  begin
    ViewData.ShowFinishTime := False;
    ViewData.ShowStartTime := False;
  end
  else
  begin
    ViewData.ShowFinishTime := dxTimeOf(EventFinish) <> 0;
    ViewData.ShowStartTime := dxTimeOf(EventStart) <> 0;
  end;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.CalculateLocationRect;
var
  AStyle: TFontStyles;
begin
  inherited CalculateLocationRect;
  if FLocation = '' then
    Exit;
  FLocationRect := FCaptionRect;
  AStyle := Canvas.Font.Style;
  Canvas.Font.Style := GetCaptionFontStyle;
  if FCaptionAndLocationCanPlaceOneLineOnly then
    FLocationRect.Left := Min(CaptionRect.Left + cxTextWidth(Canvas.Font, Caption + ' '), FLocationRect.Right)
  else
    FLocationRect.Top := Min(CaptionRect.Top + GetCaptionAutoHeight(CaptionRect) + 1, FLocationRect.Bottom);
  Canvas.Font.Style := AStyle;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.CanBeHeaderEventContinueArrows: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.CanMoveLeftBorder: Boolean;
begin
  Result := NeedLeftSizingHandler;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.CanShowHeaderEventClock(const ARect: TRect): Boolean;
var
  AWidth: Integer;
begin
  Result := ShowStartTime or ShowFinishTime;
  if Result then
  begin
    AWidth := 0;
    if ShowStartTime then
      AWidth := EventImages.Width + ScaleFactor.Apply(cxEventImagesGap);
    if ShowFinishTime then
      Inc(AWidth, EventImages.Width + ScaleFactor.Apply(cxEventImagesGap));
    if Length(Caption) > 0 then
      Inc(AWidth, 2 * cxTextWidth(Canvas.Font, Caption[1]));
    Result := cxRectWidth(ARect) > AWidth;
  end;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.CanMoveRightBorder: Boolean;
begin
  Result := NeedRightSizingHandler;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.CheckAutoLayoutImagesSingleLineCaption(
  const ACaptionWidth, AAvailableWidth: Integer): Boolean;
begin
  Result := AAvailableWidth >= ACaptionWidth div 2;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.DoDrawCaption;
begin
  cxTextOut(Canvas.Canvas, Caption, FCaptionRect, GetCaptionFlags,
    0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ViewParams.TextColor);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetTimeLineHeight: Integer;
var
  AMin: Integer;
begin
  Result := cxRectHeight(FBounds);
  if not IsHeaderEvent then
    Result := Min(Result, FTimeLineHeight);
  AMin := ScaleFactor.Apply(3);
  Result := Max(Result, AMin);
  if not ActualBorderColorIsNone and (Result > AMin + BorderSize * 2) then
    Dec(Result, BorderSize * 2);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetTimeLineRectRight: Integer;
begin
  if ActualBorderColorIsNone then
    Result := Bounds.Left
  else
    Result := inherited GetTimeLineRectRight;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.DrawSelection;
const
  ABorders: array[Boolean] of TcxBorders = ([bTop, bRight], [bTop, bLeft]);
var
  R, ACutOut: TRect;
  ARgn: TcxRegion;
begin
  if IsHeaderEvent or (FTimeLineHeight = cxRectHeight(Bounds)) then
  begin
    inherited DrawSelection;
    Exit;
  end;
  R := SelectionBorderRect;
  ACutOut := R;
  ACutOut.Top := FTimeLineRect.Bottom;
  if not UseRightToLeftAlignment then
    ACutOut.Right := FTimeLineRect.Right
  else
    ACutOut.Left := FTimeLineRect.Left;
  Canvas.SaveClipRegion;
  try
    ARgn := TcxRegion.Create(R);
    try
      ARgn.Combine(ACutOut, roSubtract);
      ARgn.Combine(ClipRect, roIntersect);
      Canvas.SetClipRegion(ARgn, roSet, False);
      DrawSizingHandler(SizingHandlerRect1, ARgn);
      DrawSizingHandler(SizingHandlerRect2, ARgn);
      ARgn.Combine(FClipRgn, roIntersect, False);
      Canvas.SetClipRegion(ARgn, roSet, False);
      Canvas.FrameRect(R, GetSelectedBorderColor, SelectionBorderSize);
      ARgn.Combine(ACutOut, roSet);
      ARgn.Combine(ClipRect, roIntersect);
      Canvas.SetClipRegion(ARgn, roSet, False);
      Canvas.FrameRect(ACutOut, GetSelectedBorderColor, SelectionBorderSize, ABorders[UseRightToLeftAlignment]);
    finally
      ARgn.Free;
    end;
  finally
    Canvas.RestoreClipRegion;
  end;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.EnableModernSelection: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetCaptionFlags: Cardinal;
const
  AHorizontalAlignment: array[Boolean] of Cardinal = (CXTO_LEFT, CXTO_RIGHT);
  AVerticalAlignment: array[Boolean] of Cardinal = (CXTO_TOP, CXTO_CENTER_VERTICALLY);
  ASingleLine: array[Boolean] of Cardinal = (CXTO_WORDBREAK, CXTO_SINGLELINE);
begin
  Result := AHorizontalAlignment[UseRightToLeftAlignment] or AVerticalAlignment[not IsDetailInfo] or CXTO_EXCELSTYLE or
    CXTO_END_ELLIPSIS or ASingleLine[not IsDetailInfo] or CXTO_EDITCONTROL;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetCaptionFontStyle: TFontStyles;
begin
  Result := inherited GetCaptionFontStyle;
  if FCaptionIsBold then
    Result := Result + [fsBold];
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetEditingRect: TRect;
begin
  Result := inherited GetEditingRect;
  if IsHeaderEvent then
    Dec(Result.Right);
  Inc(Result.Top);
  Dec(Result.Bottom);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetHeaderEventCaptionLeft(
  const ARect: TRect; AContentWidth: Integer): Integer;
begin
  if not FCaptionIsBold then
    Result := ARect.Left
  else
    Result := inherited GetHeaderEventCaptionLeft(ARect, AContentWidth);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetHeaderImagesPossibleRect(const ACaptionRect: TRect; ATextWidth: Integer): TRect;
begin
  if not FCaptionIsBold then
  begin
    Result := ACaptionRect;
    Result.Left := 0;
  end
  else
    Result := inherited GetHeaderImagesPossibleRect(ACaptionRect, ATextWidth);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetOffsetSelectionBorderFromTimeLineRect: Integer;
begin
  if ActualBorderColorIsNone then
    Result := 0
  else
    Result := inherited GetOffsetSelectionBorderFromTimeLineRect;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetImagesHorizontalOffset: Integer;
begin
  Result := ScaleFactor.Apply(cxEventImagesOffset);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetMessageRectOffset: Integer;
begin
  Result := 2 * ScaleFactor.Apply(cxEventImagesOffset);
end;

function TcxSchedulerDayViewEventCellModernViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsModern;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.IsTimeLineVisible: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.MeasureAutoLayoutImagesLocationHeight(const R: TRect): Integer;
begin
  Result := GetLocationAutoHeight(R);
  if Result > 0 then
    Inc(Result, ScaleFactor.Apply(cxTextOffset));
end;

function TcxSchedulerDayViewEventCellModernViewInfo.NeedBorderAroundOfTimeLineRect: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.NeedHeaderEventStartContinueArrow: Boolean;
begin
  Result := EventStart < ContentStart;
end;

function TcxSchedulerDayViewEventCellModernViewInfo.NeedHeaderEventFinishContinueArrow: Boolean;
begin
  Result := EventFinish > ContentFinish;
end;

procedure TcxSchedulerDayViewEventCellModernViewInfo.TuneClipping(var AClipRgn: TcxRegion);
var
  R: TRect;
begin
  inherited TuneClipping(AClipRgn);
  if Selected and (TimeLineRect.Bottom + Integer(not ActualBorderColorIsNone) < Bounds.Bottom) then
  begin
    R := SelectionBorderRect;
    R.Top := TimeLineRect.Bottom + Integer(not ActualBorderColorIsNone);
    if not UseRightToLeftAlignment then
      R.Right := TimeLineRect.Right
    else
      R.Left := TimeLineRect.Left;
    AClipRgn.Combine(R, roSubtract);
  end;
end;

{ TcxSchedulerDayViewViewInfo }

constructor TcxSchedulerDayViewViewInfo.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  Builder := TcxSchedulerEventLayoutBuilder.Create;
  FLargeFont := TFont.Create;
  FTimeRulerCells := TcxSchedulerViewInfoCellList.Create;
  FCells.Insert(1, FTimeRulerCells);
  FAllDayTopIndex := 0;
end;

destructor TcxSchedulerDayViewViewInfo.Destroy;
begin
  Builder.Free;
  FBackground.Free;
  FLargeFont.Free;
  inherited Destroy;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited CalculateHitTest(AHitTest);
  TimeRulerCells.CalculateHitTest(AHitTest);
end;

procedure TcxSchedulerDayViewViewInfo.InitScrollBarsParameters;
begin
  if (FVisibleRowCount = 0) or (FVisibleRowCount >= FRowCount) or
     (cxRectWidth(Bounds) <= 0) then
    DayView.SetScrollBarInfo(sbVertical, 0, 2, 1, 1, 0, False, True)
  else
    DayView.SetScrollBarInfo(sbVertical, 0, FRowCount - 1, 1,
      FVisibleRowCount, TopIndex, True, True);
  inherited InitScrollBarsParameters;
end;

procedure TcxSchedulerDayViewViewInfo.ScrollVertical(AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if not (AScrollCode in [scPosition, scTrack]) then
    AScrollPos := TopIndex;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, 1);
    scLineDown:
      Inc(AScrollPos, 1);
    scPageUp:
      Dec(AScrollPos, VisibleRowCount);
    scPageDown:
      Inc(AScrollPos, VisibleRowCount);
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := FRowCount;
  end;
  if AScrollPos + VisibleRowCount > (FRowCount - 1) then
    AScrollPos := FRowCount - VisibleRowCount;
  if AScrollPos < 0 then
    AScrollPos := 0;
  if FTopIndex <> AScrollPos then
  begin
    FTopIndex := AScrollPos;
    DayView.LayoutChanged;
    AScrollPos := FTopIndex;
  end;
end;

procedure TcxSchedulerDayViewViewInfo.AllDayScroll(AScrollCode: TScrollCode; var AScrollPos: Integer);
begin
  if not NeedAllDayScrollBar then
    Exit;
  if not (AScrollCode in [scPosition, scTrack]) then
    AScrollPos := FAllDayTopIndex;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, 1);
    scLineDown:
      Inc(AScrollPos, 1);
    scPageUp:
      Dec(AScrollPos, 1);
    scPageDown:
      Inc(AScrollPos, 1);
    scTop:
      AScrollPos := 0;
    scBottom:
      AScrollPos := DayView.AllDayScrollBar.Max;
  end;
  AScrollPos := Max(AScrollPos, DayView.AllDayScrollBar.Min);
  AScrollPos := Min(AScrollPos, DayView.AllDayScrollBar.Max);
  if FAllDayTopIndex <> AScrollPos then
  begin
    FAllDayTopIndex := AScrollPos;
    DayView.LayoutChanged;
  end;
end;

function TcxSchedulerDayViewViewInfo.AddContentCell(
  const ARect: TRect; const AStart, AFinish: TDateTime;
  AResourceIndex: Integer): TcxSchedulerContentCellViewInfo;
var
  R: TRect;
  P: TcxViewParams;
  AIsSelected: Boolean;
begin
  R := Bounds;
  R.Top := FContentOffset;
  AIsSelected := False;
  if cxRectIntersect(R, ARect) then
    P := GetContentParams(AStart, Resources[AResourceIndex], AIsSelected)
  else
    FillChar(P, SizeOf(P), 0);
  CreateCellInstance(ContentCellClass, ARect, R, P, UseRightToLeftAlignment, Result);
  TcxSchedulerContentCellViewInfoAccess(Result).FSelected := AIsSelected;
  Result.SetTime(AStart, AFinish);
  AssignResourceID(Result, AResourceIndex);
  ContentCells.Add(Result);
end;

procedure TcxSchedulerDayViewViewInfo.AddContainerCell(
  AColumn: Integer; const ABounds: TRect);
var
  ACell: TcxSchedulerContainerCellViewInfo;
  AParams: TcxViewParams;
  ASelected: Boolean;
  AResource: TcxSchedulerStorageResourceItem;
  ADate: TDateTime;
begin
  ADate := GetColumnDate(AColumn);
  AResource := GetResourceItemByIndex(GetColumnResourceIndex(AColumn));
  ASelected := IsContainerSelected(AResource, ADate);
  AParams := DayView.Styles.GetHeaderContainerParams(ADate, AResource, ASelected);
  CreateCellInstance(HeaderContainerCellClass, ABounds, AParams, UseRightToLeftAlignment, ACell);
  ACell.Calculate(ADate, ASelected, GetActualDayHeaderModernStyleDisplayMode,
    Byte(GroupByResource or GroupByDate) * FResourceHeaderHeight + DayHeaderHeight);
  AssignResourceID(ACell, GetColumnResourceIndex(AColumn));
  Inc(AColumn);
  if (AColumn = FColCount) or (HasSeparator and (GroupingKind <> gkNone) and
    (AColumn mod ColumnInGroup = 0)) then
    ACell.Borders := [bBottom]
  else
    ACell.Borders := [bBottom, bRight];
  if HasVisibleBounds and (AColumn = FColCount) then
    ACell.Borders := [bBottom, bRight];
  TcxSchedulerContainerCellViewInfoAccess(ACell).FFirstEventTop := FHeaderContainerContentOffset;
  FHeaderContainerCells.Add(ACell);
end;

procedure TcxSchedulerDayViewViewInfo.AddHeaderContainerEvent(APlace: TcxSchedulerEventPlace);

  function GetBaseTopOffset: Integer;
  const
    AOffset: array[Boolean] of Integer = (0, 1);
  var
    AMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
  begin
    AMode:= GetActualDayHeaderModernStyleDisplayMode;
    if (AMode = hdmClassic) or Adapter.IsPrinting then
      Result := ScaleFactor.Apply(cxTextOffset)
    else
      Result := AOffset[AMode <> hdmDefault];
  end;

var
  I, ATop, ATopOffset: Integer;
  ABounds: TRect;
  AStart, AFinish: TDateTime;
  ACell: TcxSchedulerEventCellViewInfo;
begin
  if APlace.LineStart >= FHeaderLineCount then Exit;
  ATop := Bounds.Top + FHeaderContainerContentOffset;
  GetHeaderEventInfo(APlace, ABounds.Left, ABounds.Right, AStart, AFinish);
  ATopOffset := GetBaseTopOffset;
  ABounds.Top := ATop + ATopOffset;
  ABounds.Bottom := ABounds.Top + FHeaderLineHeight;
  if (GetActualDayHeaderModernStyleDisplayMode = hdmDefault) and not Adapter.IsPrinting then
    ATopOffset := Integer(APlace.LineStart > 0);
  OffsetRect(ABounds, 0, APlace.LineStart * (FHeaderLineHeight + ATopOffset));
  if (APlace.ColFinish >= FColCount - 1) and NeedAllDayScrollBar and not TcxCustomSchedulerAccess(Scheduler).IsPopupScrollBars then
    if Scheduler.UseRightToLeftScrollBar then
      Inc(ABounds.Left, TcxCustomSchedulerAccess(Scheduler).GetScrollBarSize.cx)
    else
      Dec(ABounds.Right, TcxCustomSchedulerAccess(Scheduler).GetScrollBarSize.cx);
  ACell := AddEventCell(CreateHeaderEventViewData(
    TcxSchedulerControlEvent(APlace.Event), ABounds,
    AStart, AFinish, GetColumnResource(APlace.ColStart)));
  if not ACell.Event.IsClone then
  begin
    for I := APlace.ColStart to APlace.ColFinish do
      SetColumnTimeLineStyle(ACell, I);
    SetContainerCellHeaderEvent(ACell);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.AddHeaderContainerEventPlace(
  AEvent: TcxSchedulerControlEvent; var AColIndex: Integer);
var
  I, J: Integer;
  ADate: TDateTime;
begin
  J := AColIndex;
  ADate := GetColumnDate(J);
  for I := AColIndex + 1 to FColCount - 1 do
    if IsColumnEvent(AEvent, I, True) and (GetColumnDate(I) > ADate) then
      J := I
    else
      Break;
  Builder.AddEventPlace(AEvent, AColIndex, J);
  AColIndex := J + 1;
end;

procedure TcxSchedulerDayViewViewInfo.AddMoreEventsButton(const AContainerBounds: TRect;
  const ARowIndex: Integer; AIsTop: Boolean; AEvent: TcxSchedulerEvent);
var
  R: TRect;
begin
  if ViewStyle = svsClassic then
  begin
    R := Bounds;
    R.Right := FHeaderOffsetLeft;
  end
  else
  begin
    R := AContainerBounds;
    R.Bottom := Bounds.Bottom;
  end;
  R.Top := FContentOffset;
  AddButton(R, ARowIndex / FRowCount, not AIsTop, AEvent);
end;

procedure TcxSchedulerDayViewViewInfo.CalculateAllDayAreaBounds;
begin
  FAllDayAreaBounds := cxNullRect;
  if GetHeaderContainer then
  begin
    FAllDayAreaBounds.Left := FHeaderOffsetLeft;
    FAllDayAreaBounds.Right := Bounds.Right;
    FAllDayAreaBounds.Top := FResourceHeaderHeight * Byte(GroupingKind <> gkNone) + DayHeaderHeight;
    FAllDayAreaBounds.Bottom := FContentOffset;
    if Scheduler.ControlBox.Visible or
      TcxSchedulerCustomDateNavigatorAccess(Scheduler.DateNavigator).Visible then
      FAllDayAreaBounds := cxRectOffset(FAllDayAreaBounds,
        IfThen(Scheduler.OptionsView.ViewPosition = vpRight,
          cxRectWidth(Scheduler.ControlBox.Bounds) + Scheduler.OptionsView.VertSplitterWidth),
        IfThen(Scheduler.OptionsView.ViewPosition = vpBottom,
          cxRectHeight(Scheduler.ControlBox.Bounds) + Scheduler.OptionsView.HorzSplitterWidth));
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateAllDayScrollBar;
var
  ARect: TRect;
  AVisible: Boolean;
  AScrollBar: TdxScrollBarWrapper;
begin
  AScrollBar := DayView.AllDayScrollBar;
  AVisible := GetHeaderContainer and NeedAllDayScrollBar;
  if AVisible then
  begin
    UpdateAllDayScrollBarParams;
    ARect := FAllDayAreaBounds;
    Inc(ARect.Right);
    Inc(ARect.Top);
    if Scheduler.UseRightToLeftScrollBar then
      ARect.Right := ARect.Left + TcxCustomSchedulerAccess(Scheduler).GetScrollBarSize.cx
    else
      ARect.Left := ARect.Right - TcxCustomSchedulerAccess(Scheduler).GetScrollBarSize.cx;
    AScrollBar.BoundsRect := ARect;
  end;
  AScrollBar.Visible := AVisible;
  if not AVisible or not AScrollBar.Enabled then
    FAllDayTopIndex := 0;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateBackground;

  procedure CreateBackgroundCell(const ABounds: TRect);
  begin
    if ABounds.Top < ABounds.Bottom then
    begin
      CreateCellInstance(TcxSchedulerBackgroundCellViewInfo, ABounds, Styles.GetBackgroundParams, UseRightToLeftAlignment, FBackground);
      FBackground.Borders := [bTop];
    end;
  end;

  procedure CreateBackgroundCellSlots(const ABounds: TRect);
  var
    R: TRect;
    AEventIndex, I, J, K: Integer;
    AEvent: TcxSchedulerControlEvent;
    ASlot: TcxSchedulerBackgroundSlotCellViewInfo;
  const
    ABorders: array[Boolean] of TcxBorders = ([bRight, bTop], [bTop]);
  begin
    if ABounds.Top >= ABounds.Bottom then Exit;
    TcxSchedulerTimeRulerCellViewInfo(TimeRulerCells[TimeRulerCells.Count - 1]).SetBottom(ABounds.Top);
    for J := -1 to ColCount - 1 do
    begin
      for I := 0 to UnusedRowCount - 1 do
      begin
        R := cxRectSetTop(ABounds, ABounds.Top + I * FContentLineHeight, FContentLineHeight);
        if J = -1 then
          R.Right := FHeaderOffsetLeft
        else
          R := cxRectSetLeft(R, GetContainer(J).Bounds.Left, cxRectWidth(GetContainer(J).Bounds));
        if J = ColCount - 1 then Dec(R.Right);
        ASlot := AddBackgroundSlot(R, ABorders[J = ColCount - 1], IntToStr(I));
        if J >= 0 then
        begin
          AEventIndex := 0;
          for K := 0 to Events.Count - 1 do
          begin
            AEvent := Events[K];
            if IsColumnEvent(AEvent, J) and not IsEventVisible(AEvent) then
            begin
              if AEventIndex = I then
              begin
                ASlot.DisplayText := GetEventSlotDisplayText(AEvent);
                Break;
              end;
              Inc(AEventIndex);
            end;
          end;
        end;
      end;
    end;
  end;

var
  R: TRect;
begin
  FreeAndNil(FBackground);
  if AutoHeight and not HasVisibleBounds then Exit;
  R := Bounds;
  if Adapter.IsPrinting then
  begin
    R.Top := FContentOffset + (PrintFinishRow - PrintStartRow + 1) * FContentLineHeight;
    CreateBackgroundCellSlots(R);
  end
  else
  begin
    R.Top := FContentOffset + FRowCount * FContentLineHeight;
    CreateBackgroundCell(R);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateContent;
const
  AIsPrintingBorders: array[Boolean] of TcxBorders = ([bRight], [bLeft, bRight]);
  AIsNoPrintingBorders: array[Boolean] of TcxBorders = ([bBottom], [bLeft, bBottom]);
var
  I, J: Integer;
  ACell: TcxSchedulerDayViewContentCellCustomViewInfo;
  AColor: TColor;
  ARect, ABounds: TRect;
  AIndex: Integer;
  AResource: TcxSchedulerStorageResourceItem;
begin
  ABounds := Bounds;
  ABounds.TopLeft := Point(FHeaderOffsetLeft, FContentOffset);
  for I := 0 to FColCount - 1 do
  begin
    AIndex := GetColumnResourceIndex(I);
    AResource := GetResourceItemByIndex(AIndex);
    AColor := StylesAdapter.GetContentParams(GetColumnDate(I), True, AResource).Color;
    ARect := cxRectSetTop(Containers[I].Bounds, ABounds.Top, ContentLineHeight);
    if HasVisibleBounds and not Adapter.IsPrinting then
      OffsetRect(ARect, 0, -FTopIndex * ContentLineHeight);
    if not Adapter.IsPrinting then
    begin
      if (ColumnInGroup > 1) or ((ColumnInGroup = 1) and (DayView.OptionsView.GroupSeparatorWidth = 0))  then
      begin
        if (I + 1) mod ColumnInGroup <> 0 then
          Dec(ARect.Right, 1);
        if ((I mod ColumnInGroup > 0) or not HasSeparator) then
          Dec(ARect.Left);
      end;
    end;
    for J := 0 to FDayRowCount - 1 do
      if IsRowVisible(J) then
      begin
        ACell := AddContentCell(ARect, GetRowTime(I, J), GetRowTime(I, J + 1), AIndex) as TcxSchedulerDayViewContentCellCustomViewInfo;
        ACell.Borders := [];
        if Adapter.IsPrinting and (I < (FColCount - 1)) then
          ACell.Borders := AIsPrintingBorders[ViewStyle = svsModern];
        if not Adapter.IsPrinting or (J <> PrintFinishRow) then
          ACell.Borders := ACell.Borders + AIsNoPrintingBorders[ViewStyle = svsModern];
        ACell.SetBorderColor(AColor, (J + 1) mod LinePerHour = 0, DayView.IsWorkTime(AResource, ACell.TimeStart));
        ACell.ShowTimeLine := ViewStyle = svsClassic;
        if ACell.ShowTimeLine then
        begin
          if I = 0 then
            ACell.TimeLineBorders := [bRight];
          if HasSeparator and (I mod ColumnInGroup = 0) then
            ACell.TimeLineBorders := ACell.TimeLineBorders * LookAndFeelPainter.HeaderBorders([nLeft]);
           ACell.TimeLineParams := DayView.Styles.GetTimeLineParams;
        end;
        OffsetRect(ARect, 0, FContentLineHeight);
      end;
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateContentNavigationButtons;

  function LineInLine(ADistance1, ADistance2: Integer): Boolean;
  begin
    Result := ADistance1 <= ADistance2;
  end;

  function GetContainerWidth(AContainerID: Integer): Integer;
  begin
    Result := Containers[AContainerID].Bounds.Right - Containers[AContainerID].Bounds.Left;
  end;

  function GetResourceWidth(AContainerID: Integer; out TheSameResource: Boolean): Integer;
  var
    I, AResourceCount: Integer;
  begin
    Result := MaxInt;
    TheSameResource := True;
    AResourceCount := Max(ResourceCount, 1);
    if (DayCount <> 0) and (AContainerID mod DayCount = 0) then
    begin
      TheSameResource := False;
      Result := GetContainerWidth(AContainerID);
      I := AContainerID + 1;
      while (I < DayCount * AResourceCount) and (I mod DayCount <> 0) do
      begin
        Inc(Result, GetContainerWidth(I));
        Inc(I);
      end;
    end;
  end;

  function IsValidButtonPlace(AContainerID: Integer;  AColumnPosition: TcxSchedulerColumnPositionInResource;
    const AColumnRect: TRect): Boolean;
  var
    AButtonWidth1, AButtonWidth2, AResourceWidth, AColumnWidth: Integer;
    AKind: TcxSchedulerContentNavigationButtonKind;
    ANewColumnRect: TRect;
    TheSameResource: Boolean;
  begin
    Result := True;
    AColumnWidth := AColumnRect.Right - AColumnRect.Left;
    if FColCount = ResourceCount then
    begin
      CalculateNavigationButtonParams(AColumnRect, nbkPrevious, AButtonWidth1);
      CalculateNavigationButtonParams(AColumnRect, nbkNext, AButtonWidth2);
      if GroupByDate and (AContainerID < ResourceCount - 1) then
        Inc(AButtonWidth2);
      Result := LineInLine(AButtonWidth1 + AButtonWidth2, AColumnWidth);
    end
    else
      if GroupByDate then
      begin
        case AColumnPosition of
          cprFirst: AKind := nbkPrevious;
          cprLast: AKind := nbkNext;
        else
          raise EdxException.Create('');
        end;
        CalculateNavigationButtonParams(AColumnRect, AKind, AButtonWidth1);
        Result := LineInLine(AButtonWidth1, AColumnWidth);
      end
      else
      begin
        AResourceWidth := GetResourceWidth(AContainerID, TheSameResource);
        if not TheSameResource then
        begin
          if GroupByResource then
            ANewColumnRect := cxRectSetWidth(AColumnRect, AResourceWidth)
          else
            ANewColumnRect := AColumnRect;
          CalculateNavigationButtonParams(ANewColumnRect, nbkPrevious, AButtonWidth1);
          CalculateNavigationButtonParams(ANewColumnRect, nbkNext, AButtonWidth2);
          Result := LineInLine(AButtonWidth1 + AButtonWidth2, AResourceWidth);
        end;
      end;
  end;

type
  TStorageItem = record
    AIndex: Integer;
    AColumnPosition: TcxSchedulerColumnPositionInResource;
    AColumnRect: TRect;
  end;

var
  I, AIndex: Integer;
  AColumnPosition: TcxSchedulerColumnPositionInResource;
  AColumnRect, ABounds: TRect;
  ADrawButtons: Boolean;
  AStorage: array of TStorageItem;
begin
  if not CanCalculateNavigationButtons then
    Exit;

  SetLength(AStorage, FColCount);
  I := 0;
  ADrawButtons := True;
  while ADrawButtons and (I < FColCount) do
  begin
    AIndex := GetColumnResourceIndex(I);
    AColumnPosition := GetColumnPositionInResource(I, AIndex);
    AStorage[I].AColumnPosition := AColumnPosition;
    if AColumnPosition <> cprIndefinite then
    begin
      ABounds := Containers[I].Bounds;
      AColumnRect := cxRectSetHeight(ABounds, ABounds.Bottom - ABounds.Top + VisibleRowCount * ContentLineHeight);
      ADrawButtons := IsValidButtonPlace(I, AColumnPosition, AColumnRect);
      AStorage[I].AIndex := AIndex;
      AStorage[I].AColumnRect := AColumnRect;
    end;
    Inc(I);
  end;

  if ADrawButtons then
    for I := 0 to FColCount - 1 do
      if AStorage[I].AColumnPosition <> cprIndefinite then
        AddContentNavigationButton(AStorage[I].AColumnRect, AStorage[I].AIndex, AStorage[I].AColumnPosition);
end;

procedure TcxSchedulerDayViewViewInfo.CalculateEvents;
var
  I: Integer;
  AList: TcxSchedulerEventList;
  AContainer: TcxSchedulerContainerCellViewInfo;
begin
  AList := TcxSchedulerEventList.Create;
  try
    for I := 0 to ContainerCount - 1 do
    begin
      AContainer := Containers[I];
      // for source events
      AList.Clear;
      ExtractEvents(Events, AList, I);
      CalculateEventsForColumn(I, AContainer, AList);
      // for clones
      AList.Clear;
      ExtractEvents(Events.Clones, AList, I);
      CalculateEventsForColumn(I, AContainer, AList);
    end;
  finally
    AList.Free;
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateEventsForColumn(AIndex: Integer;
  AContainer: TcxSchedulerContainerCellViewInfo; AEvents: TcxSchedulerEventList);
var
  ATop, I: Integer;
  AViewData: TcxSchedulerEventViewData;
  AInfo: TcxEventLayoutInfo;
  AEventCell: TcxSchedulerDayViewEventCellModernViewInfo;
begin
  if AEvents.Count = 0 then Exit;
  FEventLayout.Clear;
  I := Byte(((AIndex mod ColumnInGroup <> 0) or not HasSeparator) and not (ColCount = 1));
  ATop := FContentOffset;
  if not Adapter.IsPrinting then
    Dec(ATop, TopIndex * FContentLineHeight);
  FEventLayout.SetParams(dxDateOf(AContainer.DateTime), TimeScale,
    FContentLineHeight, AContainer.Bounds.Left - I, ATop, cxRectWidth(AContainer.Bounds));
  for I := 0 to AEvents.Count - 1 do
    FEventLayout.Add(TcxSchedulerControlEvent(AEvents[I]));
  FEventLayout.Calculate;
  for I := 0 to FEventLayout.Count - 1 do
  begin
    AInfo := FEventLayout.Infos[I];
    CheckEventRowsVisibility(AContainer.Bounds, AInfo.Event, AInfo.StartRow, AInfo.StopRow + 1);
    AViewData := CreateContentEventViewData(AInfo, AContainer.Resource);
    if AViewData <> nil then
    begin
      if Adapter.IsPrinting and (FColCount > 1) then
        if (AIndex mod ColumnInGroup > 0) or ((ColumnInGroup = 1) and
        (DayView.OptionsView.GroupSeparatorWidth = 0)) then
          OffsetRect(AViewData.Bounds, 1, 0);
      AViewData.DrawShadows := DayView.EventShadows and (SchedulerStyle = svsClassic);
      if ViewStyle = svsClassic then
        AddEventCell(AViewData)
      else
      begin
        AEventCell := AddEventCell(AViewData, False) as TcxSchedulerDayViewEventCellModernViewInfo;
        AEventCell.FTimeLineHeight := (AInfo as TcxEventModernLayoutInfo).RealHeight;
        AEventCell.Calculate;
      end;
    end;
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateHeaderEvents;
var
  I: Integer;
begin
  for I := 0 to Builder.EventPlaceCount - 1 do
    AddHeaderContainerEvent(Builder.EventPlaces[I]);
end;

procedure TcxSchedulerDayViewViewInfo.CalculateHeaderEventsPlace;
var
  I, J: Integer;
begin
  if not HeaderContainer or DayView.ShowAllDayEventsInContentArea then Exit;
  for I := 0 to Events.AbsoluteCount - 1 do
  begin
    J := 0;
    while J < FColCount do
    begin
      if IsColumnEvent(Events.AbsoluteItems[I], J, True) then
        AddHeaderContainerEventPlace(Events.AbsoluteItems[I], J)
      else
        Inc(J);
    end;
  end;
  Builder.Calculate(ViewStyle);
end;

procedure TcxSchedulerDayViewViewInfo.CalculateHeaderLineCount;

  procedure CheckEventPlace;
  var
    I: Integer;
    AMaxCount: Integer;
  begin
    AMaxCount := GetAllDayEventMaxCount;
    for I := 0 to Builder.EventPlaceCount - 1 do
      with Builder.EventPlaces[I] do
        if not Assigned(TcxSchedulerControlEvent(Event).Source) then
        begin
          if FAllDayTopIndex > LineFinish then
            FAllDayTopIndex := LineFinish
          else
            if (FAllDayTopIndex + FHeaderLineCount - 1) < LineFinish then
              FAllDayTopIndex := LineFinish - FHeaderLineCount + 1;
        end;
    for I := 0 to Builder.EventPlaceCount - 1 do
      with Builder.EventPlaces[I] do
      begin
        Dec(LineFinish, FAllDayTopIndex);
        Dec(LineStart, FAllDayTopIndex);
        if LineFinish < 0 then
          Inc(LineFinish, AMaxCount + 1);
        if LineStart < 0 then
          Inc(LineStart, AMaxCount + 1);
      end;
  end;

var
  I: Integer;
begin
  if not HeaderContainer then
  begin
    FHeaderLineCount := 0;
    Exit;
  end;
  CalculateHeaderEventsPlace;
  FHeaderLineCount := 1;
  for I := 0 to Builder.EventPlaceCount - 1 do
    FHeaderLineCount := Max(Builder.EventPlaces[I].LineFinish + 1, FHeaderLineCount);
  if cxSchedulerAllDayEventContainerMaxLineCount = 0 then
    I := (Bounds.Bottom - Bounds.Top - DayHeaderHeight -
      FResourceHeaderHeight) div FContentLineHeight div 2
  else
    I := cxSchedulerAllDayEventContainerMaxLineCount;
  FHeaderLineCount := Min(FHeaderLineCount, Max(1, I));
  if NeedAllDayScrollBar then
  begin
    UpdateAllDayScrollBarParams;
    CheckEventPlace;
  end
end;

procedure TcxSchedulerDayViewViewInfo.CalculateHeaders;
var
  I, J, W: Integer;
  AHeaderBounds, AGroupBounds: TRect;
const
  ANeighbors: array[TcxSchedulerGroupingKind, 0..1] of TcxNeighbors =
    (([], []), ([], []), ([nTop], []), ([], [nTop]));
begin
  AGroupBounds := cxRectSetHeight(Bounds, Byte(GroupByResource) * FResourceHeaderHeight +
    Byte(GroupByDate) * DayHeaderHeight);
  AHeaderBounds := cxRectSetTop(Bounds, AGroupBounds.Bottom,
    Byte(GroupByDate) * FResourceHeaderHeight + Byte(not GroupByDate) * DayHeaderHeight);
  W := Bounds.Right - FHeaderOffsetLeft - (GroupCount - 1) * SeparatorWidth;
  for I := 0 to GroupCount - 1 do
  begin
    AGroupBounds.Left := MulDiv(W, I, GroupCount) + FHeaderOffsetLeft + SeparatorWidth * I;
    AGroupBounds.Right := MulDiv(W, I + 1, GroupCount) + FHeaderOffsetLeft + SeparatorWidth * I;
    for J := 0 to ColumnInGroup - 1 do
    begin
      AHeaderBounds.Left := AGroupBounds.Left +
        MulDiv(cxRectWidth(AGroupBounds), J, ColumnInGroup);
      AHeaderBounds.Right := AGroupBounds.Left +
        MulDiv(cxRectWidth(AGroupBounds), J + 1, ColumnInGroup);
      if GroupByDate then
        AddResourceHeader(J, AHeaderBounds).DateTime :=
          GetColumnDate(I * ColumnInGroup + J)
      else
        AddDayHeader(GetColumnDate(I * ColumnInGroup + J), AHeaderBounds,
          GetColumnResourceIndex(I * ColumnInGroup + J), GetActualDayHeaderModernStyleDisplayMode);
      AddContainerCell(I * ColumnInGroup + J,
        cxRectSetTop(AHeaderBounds, Bounds.Top, FContentOffset - Bounds.Top));
    end;
    if GroupByResource then
      AddResourceHeader(I, AGroupBounds)
    else
      if GroupByDate then
        AddDayHeader(Days[I], AGroupBounds, -1, GetActualDayHeaderModernStyleDisplayMode);
    if HasSeparator then AddGroupVertSeparator(AGroupBounds.Right);
  end;
  ProcessCheckBorders(ResourceHeaderCells, False, ANeighbors[GroupingKind, 0]);
  ProcessDateToDisplayText(True);
  ProcessCheckBorders(DayHeaderCells, False, ANeighbors[GroupingKind, 1]);
end;

procedure TcxSchedulerDayViewViewInfo.CalculateMetrics;

   function GetExtraContentOffset: Integer;
   begin
     if HeaderContainer then
       Result := ScaleFactor.Apply(cxTextOffset) + FHeaderLineCount * (FHeaderLineHeight + ScaleFactor.Apply(cxTextOffset))
     else
       Result := 0;
   end;

begin
  inherited CalculateMetrics;
  FHeaderLineHeight := FContentLineHeight;
  if (GetActualDayHeaderModernStyleDisplayMode = hdmDefault) and not Adapter.IsPrinting then
    FHeaderContainerCaptionHeight :=
      LookAndFeelPainter.ScaledHeaderHeight(GetFontHeight(DayView.Styles.HeaderContainer), ScaleFactor)
  else
    FHeaderContainerCaptionHeight := 0;
  FAdditionalTimeZone := -1;
  FCurrentTimeZone := DayView.OptionsView.CurrentTimeZone;
  if FCurrentTimeZone = -1 then
    FCurrentTimeZone := DayView.DateTimeHelper.CurrentTimeZone;
  if DayView.OptionsView.ShowAdditionalTimeZone then
  begin
    FAdditionalTimeZone := DayView.OptionsView.AdditionalTimeZone;
    if FAdditionalTimeZone = -1 then
      FAdditionalTimeZone := DayView.DateTimeHelper.CurrentTimeZone;
  end;
  FColCount := DayCount;
  if GroupingKind <> gkNone then
    FColCount := FColCount * ResourceCount;
  LargeFont := GetTimeRulerParams.Font;
  CalculateHeaderLineCount;
  CalculateVisibleRows;
  FVisibleRowCount := Max(1, ContentHeight div FContentLineHeight);
  if not Adapter.IsPrinting then
  begin
    if AutoHeight and (FVisibleRowCount > (FRowCount +  FHeaderLineCount)) then
      FContentLineHeight := (ContentHeight - GetExtraContentOffset) div FRowCount;

    FContentOffset := FContentOffset + GetExtraContentOffset;
    FVisibleRowCount := Min(FRowCount, Max(1, ContentHeight div FContentLineHeight));
    if AutoHeight or (ContentHeight div FContentLineHeight < FRowCount) then
      Inc(FContentOffset, ContentHeight - FContentLineHeight * FVisibleRowCount);
  end;
  if not HeaderContainer then
    FDayHeaderHeight := FContentOffset - Byte(GroupingKind <> gkNone) * FResourceHeaderHeight;
  while (FTopIndex > 0) and ((FTopIndex + FVisibleRowCount) > FRowCount) do
    Dec(FTopIndex);
  FHeaderContainerContentOffset := DayHeaderHeight + FHeaderContainerCaptionHeight +
    FResourceHeaderHeight * Byte(GroupingKind <> gkNone)
end;

procedure TcxSchedulerDayViewViewInfo.CalculateMetricsForPrinting;
var
  AContentHeight: Integer;
  HeightAdjusted: Boolean;
begin
  FContentOffset := Byte(GroupingKind <> gkNone) * FResourceHeaderHeight * Byte(ResourceCount >= 1) +
    FHeaderLineCount * (FHeaderLineHeight + ScaleFactor.Apply(cxTextOffset)) + ScaleFactor.Apply(cxTextOffset);
  if (GroupingKind = gkByResource) or (SelectedDays.Count > 1) then
    Inc(FContentOffset, DayHeaderHeight);
  CalculateUnusedRowCount;
  FTimeScale := DayView.TimeScale;
  AContentHeight := Bounds.Bottom - Bounds.Top - FContentOffset;
  HeightAdjusted := AContentHeight < FContentLineHeight;
  while not HeightAdjusted do
  begin
    CalculateDisplayRows;
    FTopIndex := PrintStartRow;
    HeightAdjusted := AContentHeight >= FContentLineHeight * PrintRowCount;
    if not HeightAdjusted then
    begin
      if FTimeScale < 60 then
      begin
        FTimeScale := FTimeScale * 2;
        while 60 mod FTimeScale <> 0 do Inc(FTimeScale);
      end
      else
        if UnusedRowCount > 0 then
          Dec(FUnusedRowCount)
        else
          Break;
    end;
  end;
  CalculateDisplayRows;
  if HeightAdjusted then
  begin
    FContentLineHeight := AContentHeight div PrintRowCount;
    Inc(FContentOffset, AContentHeight - FContentLineHeight * PrintRowCount);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateDisplayRows;

  function GetTimeRow(ATime: TDateTime; IsFinish: Boolean): Integer;
  begin
    Result := Round(ATime / MinuteToTime);
    Result := Result div FTimeScale  - Byte(IsFinish) + Byte(IsFinish and (Result mod FTimeScale <> 0))
  end;

begin
  FDayRowCount := cxMinutesPerDay div TimeScale;
  if HasVisibleBounds then
  begin
    FPrintStartRow := GetTimeRow(PrintFrom, False);
    FPrintFinishRow := Max(FPrintStartRow, GetTimeRow(PrintTo, True));
  end
  else
  begin
    FPrintStartRow := 0;
    FPrintFinishRow := FDayRowCount - 1;
  end;
  FRowCount := FPrintFinishRow - FPrintStartRow + 1;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateTimeLine;
var
  ABounds, R: TRect;
  AOffset, I, AHour: Integer;
  AViewParams: TcxViewParams;
  ACommonCell, ARulerCell: TcxSchedulerTimeRulerCellViewInfo;
begin
  R := cxRectSetTop(Bounds, FContentOffset);
  AViewParams := GetTimeRulerParams;
  FHeaderOffsetLeft := R.Left + TimeRulerCellClass.CalculateWidth(
    1 + Byte(FAdditionalTimeZone >= 0), LinePerHour, AViewParams.Font, LargeFont, ScaleFactor, DayView.TimeRulerMinutes);
  R.Right := FHeaderOffsetLeft;
  CalculateTimeZoneBiasDelta;
  // add common area
  CreateCellInstance(TimeRulerCellClass, cxRectSetTop(R, 0, R.Top), AViewParams, UseRightToLeftAlignment, ACommonCell);
  ACommonCell.Calculate(DayView.OptionsView.CurrentTimeZoneLabel, DayView.OptionsView.AdditionalTimeZoneLabel,
    FCurrentTimeZone, FAdditionalTimeZone, FAdditionalTimeZoneBiasDelta);
  // add ruler cells
  AOffset := FPrintStartRow * FContentLineHeight - FContentOffset;
  if not Adapter.IsPrinting then
    Inc(AOffset, TopIndex * FContentLineHeight);
  R := cxRectSetTop(R, 0, LinePerHour * FContentLineHeight);
  ABounds := Bounds;
  ABounds.Top := FContentOffset;
  ABounds.Bottom := Min(FContentOffset + FRowCount * FContentLineHeight, Bounds.Bottom);
  FTimeRulerCells.Add(ACommonCell);
  I := 0;
  while I < FDayRowCount do
  begin
    if IsRowVisible(I) then
    begin
      AHour := I div LinePerHour;
      CreateCellInstance(TimeRulerCellClass,
        cxRectSetTop(R, R.Bottom * AHour - AOffset), ABounds, AViewParams, UseRightToLeftAlignment, ARulerCell);
      ARulerCell.ShowMinutes := DayView.TimeRulerMinutes;
      ARulerCell.Calculate(AHour, LinePerHour, FCurrentTimeZone,
        FAdditionalTimeZone, LargeFont, FAdditionalTimeZoneBiasDelta, SelectedDays);
      ARulerCell.LastVisibleHour := not IsHourVisible(AHour + 1);
      FTimeRulerCells.Add(ARulerCell);
      I := (AHour + 1) * LinePerHour;
    end
    else
      Inc(I);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateTimeZoneBiasDelta;

  function GetDaylightBiasDelta(ATimeZone: Integer): Integer;
  begin
    with TcxSchedulerDateTimeHelper do
    begin
      Result := TimeZoneDaylightBias(Integer(SelectedDays.First), ATimeZone);
      if Result = 0 then
        Result := TimeZoneDaylightBias(Integer(SelectedDays.Last), ATimeZone);
    end;
  end;

begin
  FAdditionalTimeZoneBiasDelta := 0;
  with DayView.OptionsView do
  begin
    if CurrentTimeZoneDaylightSaving then
      FAdditionalTimeZoneBiasDelta := GetDaylightBiasDelta(FCurrentTimeZone);
    if AdditionalTimeZoneDaylightSaving then
      Dec(FAdditionalTimeZoneBiasDelta, GetDaylightBiasDelta(FAdditionalTimeZone));
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateVisibleRows;
begin
  if not Adapter.IsPrinting then
  begin
    FTimeScale := DayView.TimeScale;
    CalculateDisplayRows;
    FContentOffset := DayHeaderHeight + FHeaderContainerCaptionHeight + Byte(GroupingKind <> gkNone) * FResourceHeaderHeight;
    if HeaderContainer then
      Inc(FContentOffset, ScaleFactor.Apply(cxTextOffset));
  end
  else
    CalculateMetricsForPrinting;
end;

procedure TcxSchedulerDayViewViewInfo.CalculateUnusedRowCount;
var
  AColRowCount: Integer;
  AEvent: TcxSchedulerEvent;
  I, J: Integer;
begin
  FUnusedRowCount := 3;
  for I := 0 to ColCount - 1 do
  begin
    AColRowCount := 0;
    for J := 0 to Events.Count - 1 do
    begin
      AEvent := Events[J];
      if IsColumnEvent(AEvent, I) and not IsEventVisible(AEvent) then
        Inc(AColRowCount);
    end;
    FUnusedRowCount := Max(FUnusedRowCount, AColRowCount);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.Clear;
begin
  inherited Clear;
  FTimeRulerCells.Clear;
end;

function TcxSchedulerDayViewViewInfo.CheckDisplayRows(var AStartRow, AFinishRow: Integer): Boolean;
begin
  Result := not HasVisibleBounds or ((AStartRow <= PrintFinishRow) and
    (PrintStartRow <= AFinishRow));
  if Result then
  begin
    AStartRow := Max(AStartRow, PrintStartRow);
    AFinishRow := Min(AFinishRow, PrintFinishRow);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.CheckEventRowsVisibility(const AContainerBounds: TRect;
  AEvent: TcxSchedulerControlEvent; AStartRow, AFinishRow: Integer);
begin
  if Adapter.IsPrinting then Exit;
  if AStartRow < FTopIndex then
    AddMoreEventsButton(AContainerBounds, AStartRow, True, AEvent);
  if (AFinishRow > (FTopIndex + VisibleRowCount)) then
  begin
    if AFinishRow <= FRowCount then
      AddMoreEventsButton(AContainerBounds, AFinishRow, False, AEvent)
    else
      if AStartRow > (FTopIndex + VisibleRowCount) then
        AddMoreEventsButton(AContainerBounds, AStartRow, False, AEvent);
  end;
end;

function TcxSchedulerDayViewViewInfo.ContentCellClass: TcxSchedulerContentCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerContentCellViewInfoClass =
    (TcxSchedulerDayViewContentCellViewInfo, TcxSchedulerDayViewContentCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerDayViewViewInfo.HeaderContainerCellClass: TcxSchedulerContainerCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerContainerCellViewInfoClass =
    (TcxSchedulerContainerCellViewInfo, TcxSchedulerContainerModernCellViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

procedure TcxSchedulerDayViewViewInfo.DoCalculate;
begin
  Builder.Clear;
  if Adapter.IsPrinting then
    Inc(FBounds.Right);
  FEventLayout := CreateEventLayout;
  inherited DoCalculate;
  with DayView.Scheduler do
  begin
    FIsContainerSelected := (GetTimeMinutes(SelFinish) = 0) and
      (GetTimeMinutes(SelStart) = 0);
  end;
  try
    CalculateTimeLine;
    CalculateHeaders;
    CalculateContent;
    CalculateEvents;
    CalculateHeaderEvents;
    CalculateBackground;
    CalculateContentNavigationButtons;
    CalculateAllDayAreaBounds;
    CalculateAllDayScrollBar;
  finally
    FEventLayout.Free;
    FButtons.Sort(TListSortCompare(@cxCompareButtons));
    if FEventCells.Count > 1 then
      FEventCells.Sort(TListSortCompare(@cxCompareEventViewInfoOrders));
  end;
end;

procedure TcxSchedulerDayViewViewInfo.DoContentNavigationButtonClick(
  Sender: TcxSchedulerContentNavigationButtonViewInfo);

  procedure ShowEvent(ANavigationButtonKind: TcxSchedulerContentNavigationButtonKind);
  var
    I: Integer;
    ATime, AEventTime: TDateTime;
    AButton: TcxSchedulerMoreEventsButtonViewInfo;

    procedure SaveButton(AMoreButton: TcxSchedulerMoreEventsButtonViewInfo);
    begin
      ATime := AEventTime;
      AButton := AMoreButton;
    end;

  var
    AEventStart: TDateTime;
  begin
    AButton := nil;
    if ANavigationButtonKind = nbkPrevious then
      ATime := -1
    else
      ATime := MaxDateTime;
    for I := 0 to Buttons.Count - 1 do
      if Buttons[I] is TcxSchedulerMoreEventsButtonViewInfo then
      begin
        AEventStart := TcxSchedulerMoreEventsButtonViewInfo(Buttons[I]).Event.Start;
        AEventTime := AEventStart - Trunc(AEventStart);
        if ANavigationButtonKind = nbkPrevious then
        begin
          if ATime < AEventTime then
            SaveButton(TcxSchedulerMoreEventsButtonViewInfo(Buttons[I]));
        end
        else
          if ATime > AEventTime then
            SaveButton(TcxSchedulerMoreEventsButtonViewInfo(Buttons[I]));
      end;
    if AButton <> nil then
      AButton.Click;
  end;
var
  ANavigationButtonKind: TcxSchedulerContentNavigationButtonKind;
begin
  ANavigationButtonKind := Sender.Kind;
  SelectedDays.ShiftPeriod(Sender.Interval);
  inherited DoContentNavigationButtonClick(Sender);
  ShowEvent(ANavigationButtonKind);
end;

procedure TcxSchedulerDayViewViewInfo.DoMoreEventsButtonClick(
  Sender: TcxSchedulerMoreEventsButtonViewInfo);
var
  ARow: Integer;
  ADateTime: TDateTime;
begin
  ARow := Min(FRowCount, Max(0, Round(Sender.DateTime * FRowCount)));
  if not Sender.IsDown then
    FTopIndex := ARow
  else
    FTopIndex := Max(0, ARow - VisibleRowCount);
  with Controller.Navigation do
  begin
    ADateTime := dxDateOf(SelStart) + dxTimeOf(Sender.DateTime);
    ReplaceSelParams(ADateTime, ADateTime);
  end;
  View.LayoutChanged;
end;

function TcxSchedulerDayViewViewInfo.CreateContentEventViewData(
  AEventInfo: TcxEventLayoutInfo; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;
begin
  if (AEventInfo.DisplayRect.Bottom <= FContentOffset) or
    (AEventInfo.DisplayRect.Top >= Bounds.Bottom) then
    Result := nil
  else
  begin
    Result := CreateEventViewData(AEventInfo.Event,
      AEventInfo.DisplayRect, AEventInfo.Start, AEventInfo.Finish, AResource);
    Result.AlwaysShowTime := DayView.AlwaysShowEventTime;
    Result.VisibleRect.TopLeft := Point(FHeaderOffsetLeft, FContentOffset);
    Result.ShowTimeLine := True;
    Result.ShowAllDayEventsInContentArea := DayView.ShowAllDayEventsInContentArea;
  end;
end;

function TcxSchedulerDayViewViewInfo.CreateHeaderEventViewData(
  AEvent: TcxSchedulerControlEvent; const ABounds: TRect;
  const AStart, AFinish: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxSchedulerEventViewData;
begin
  Result := CreateEventViewData(AEvent, ABounds, AStart, AFinish, AResource);
  Result.VisibleRect := cxRect(FHeaderOffsetLeft, DayHeaderHeight, Bounds.Right, FContentOffset);
  Result.ShowTimeLine := ViewStyle = svsModern;
end;

function TcxSchedulerDayViewViewInfo.CreateEventLayout: TcxCalculateEventLayout;
begin
  Result := GetCalculateEventLayoutClass.Create;
  Result.FViewInfo := Self;
end;

procedure TcxSchedulerDayViewViewInfo.ExtractEvents(
  ASource, ADest: TcxSchedulerEventList; AIndex: Integer);
begin
  ADest.List.Count := ASource.Count;
  TdxMultithreadedListHelper.IterateItems(ASource.List,
    procedure(AEvent: Pointer; AItemIndex: Integer)
    begin
      if IsColumnEvent(TcxSchedulerEvent(AEvent), AIndex) then
        ADest.List[AItemIndex] := AEvent;
    end
  );
  ADest.List.Pack();
end;

function TcxSchedulerDayViewViewInfo.GetCalculateEventLayoutClass: TcxCalculateEventLayoutClass;
const
  AClass: array[Boolean] of TcxCalculateEventLayoutClass = (TcxCalculateEventLayout, TcxCalculateModernEventLayout);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerDayViewViewInfo.GetContentParams(const ATime: TDateTime;
  AResource: TcxSchedulerResourceViewInfo; var AIsSelected: Boolean): TcxViewParams;
begin
  Result := inherited GetContentParams(ATime, AResource);
  AIsSelected := not HiddenSelection and not FIsContainerSelected and IsTimeSelected(ATime, AResource);
  if AIsSelected then
    Result := GetSelectionParams(Result);
end;

function TcxSchedulerDayViewViewInfo.GetColumnDate(AIndex: Integer): Integer;
begin
  if GroupingKind = gkByDate then
    AIndex := AIndex div ResourceCount
  else
    if GroupingKind = gkByResource then
      AIndex := AIndex mod DayCount;
  Result := Integer(SelectedDays.List[AIndex])
end;

function TcxSchedulerDayViewViewInfo.GetColumnPositionInResource(AColumnIndex,
  AResourceIndex: Integer): TcxSchedulerColumnPositionInResource;
begin
  Result := cprIndefinite;
  if ResourceCount > 0 then
  begin
    if FColCount = ResourceCount then
      Result := cprSingle
    else
      case GroupingKind of
        gkByDate:
          begin
            if AColumnIndex = AResourceIndex then
              Result := cprFirst
            else
              if AColumnIndex > FColCount - 1 - ResourceCount then
                Result := cprLast;
          end;
        gkByResource:
          begin
            if AColumnIndex = DayCount * AResourceIndex then
              Result := cprFirst
            else
              if AColumnIndex = DayCount * (AResourceIndex + 1) - 1 then
                Result := cprLast;
          end;
      end;
  end
  else
    if FColCount = 1 then
      Result := cprSingle
    else
      if AColumnIndex = 0 then
        Result := cprFirst
      else
        if AColumnIndex = FColCount - 1 then
          Result := cprLast;
end;

function TcxSchedulerDayViewViewInfo.GetColumnResource(
  AIndex: Integer): TcxSchedulerResourceViewInfo;
begin
  Result := Resources[GetColumnResourceIndex(AIndex)];
end;

function TcxSchedulerDayViewViewInfo.GetColumnResourceIndex(
  AIndex: Integer): Integer;
begin
  Result := -1;
  if ResourceCount > 0 then
  begin
    if GroupingKind = gkByDate then
      Result := AIndex mod ResourceCount
    else
      if GroupingKind = gkByResource then
        Result := AIndex div DayCount;
  end;
end;

function TcxSchedulerDayViewViewInfo.GetContainer(
  AIndex: Integer): TcxSchedulerContainerCellViewInfo;
begin
  Result := TcxSchedulerContainerCellViewInfo(HeaderContainerCells[AIndex]);
end;

function TcxSchedulerDayViewViewInfo.GetContainerCount: Integer;
begin
  Result := HeaderContainerCells.Count;
end;

function TcxSchedulerDayViewViewInfo.GetDayHeaderHeight: Integer;
begin
  Result := inherited GetDayHeaderHeight;
  if ((SelectedDays.Count = 1) and Adapter.IsPrinting and (GroupingKind <> gkByResource)) or
    not DayView.DayHeaderArea then
    Result := 0;
end;

function TcxSchedulerDayViewViewInfo.GetEventSlotDisplayText(
  AEvent: TcxSchedulerControlEvent): string;
begin
  Result := DoGetEventDisplayText(AEvent);
  with DateTimeHelper do
  begin
    Result := TimeToStr(AEvent.Start) + '-' + TimeToStr(AEvent.Finish) + ' ' + Result;
  end;
end;

function TcxSchedulerDayViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerEventCellViewInfoClass =
    (TcxSchedulerDayViewEventCellViewInfo, TcxSchedulerDayViewEventCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerDayViewViewInfo.GetActualDayHeaderModernStyleDisplayMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
var
  AMode: TcxSchedulerDayHeaderModernStyleDisplayMode;
begin
  AMode := DayView.DayHeaderModernStyleDisplayMode;
  if (ViewStyle = svsClassic) or (AMode = hdmClassic) then
    Result := hdmClassic
  else
    Result := AMode;
end;

function TcxSchedulerDayViewViewInfo.GetEventColumnRow(
  const ATime: TDateTime; AColIndex: Integer; AStart: Boolean): Integer;

  function GetTimeRowEx: Integer;
  var
    ARow: Integer;
  begin
    ARow := Round(dxTimeOf(ATime) / MinuteToTime);
    Result := ARow div TimeScale;
    if not AStart then
      if ViewStyle = svsClassic then
        Result := Result - Byte((ARow mod TimeScale) <= (TimeScale / 2))
      else
        Result := Result - Byte((ARow mod TimeScale) = 0);
    Result := Max(0, Min(FRowCount - 1, Result - FPrintStartRow));
  end;

begin
  if AStart and (ATime <= GetColumnDate(AColIndex)) then
    Result := 0
  else
    if not AStart and (ATime >= GetColumnDate(AColIndex)) then
    begin
      if HasVisibleBounds then
        Result := FRowCount - 1
      else
        Result := VisibleRowCount - 1
    end
    else
    begin
      if HasVisibleBounds then
        Result := GetTimeRowEx
      else
        Result := Min(VisibleRowCount, GetTimeRowEx - TopIndex);
      if AStart then
        Result := Max(0, Result);
    end;
end;

procedure TcxSchedulerDayViewViewInfo.GetHeaderEventInfo(APlace: TcxSchedulerEventPlace;
  out ALeft, ARight: Integer; out AStart, AFinish: TDateTime);
var
  AColStart, AColFinish: Integer;
begin
  AColStart := APlace.ColStart;
  AColFinish := APlace.ColFinish;
  with Containers[AColStart] do
  begin
    AStart := DateTime;
    ALeft := Bounds.Left;
    if ViewStyle = svsClassic then
      Inc(ALeft, ScaleFactor.Apply(cxTextOffset));
  end;
  with Containers[AColFinish] do
  begin
    AFinish := DateTime + 1;
    ARight := Bounds.Right;
    if ViewStyle = svsClassic then
      Dec(ARight, ScaleFactor.Apply(cxTextOffset + 1))
    else
      if APlace.Event.Finish <= AFinish then
        Dec(ARight, TcxSchedulerPainterHelper.ShadowSize(ScaleFactor) + 1);
  end;
end;

function TcxSchedulerDayViewViewInfo.GetTimeLineParams: TcxViewParams;
begin
  Result := DayView.Styles.GetTimeLineParams;
end;

function TcxSchedulerDayViewViewInfo.GetTimeRulerParams: TcxViewParams;
var
  AIntf: IcxSchedulerTimeRulerParams;
begin
  if Supports(StylesAdapter, IcxSchedulerTimeRulerParams, AIntf) then
    Result := AIntf.GetTimeRulerParams
  else
    Result := DayView.Styles.GetTimeRulerParams;
end;

function TcxSchedulerDayViewViewInfo.GetTimeRow(const ATime: TDateTime; AIsStart: Boolean): Integer;
var
  ACount: Double;
begin
  ACount := Min(FRowCount - 1, dxTimeOf(ATime) / MinuteToTime / TimeScale - FPrintStartRow);
  Result := Round(ACount);
  if not AIsStart and (dxTimeOf(ACount) = 0) then
    Dec(Result);
end;

function TcxSchedulerDayViewViewInfo.GetResourcesContentWidth: Integer;
var
  AFont: TFont;
begin
  AFont := GetTimeRulerParams.Font;
  LargeFont := AFont;
  Result := (Bounds.Right - Bounds.Top) - TimeRulerCellClass.CalculateWidth(
    1 + Byte(FAdditionalTimeZone >= 0), 60 div DayView.TimeScale, AFont, LargeFont, ScaleFactor, DayView.TimeRulerMinutes);
  if GroupByDate then
    Result := Result div DayCount;
end;

function TcxSchedulerDayViewViewInfo.GetRowTime(AColumn, ARow: Integer): TDateTime;
begin
  Result := GetColumnDate(AColumn) + (ARow * TimeScale) * MinuteToTime;
end;

function TcxSchedulerDayViewViewInfo.GetSeparatorCount: Integer;
begin
  Result := inherited GetSeparatorCount;
  if GroupByDate then
    Result := DayCount - 1;
end;

function TcxSchedulerDayViewViewInfo.IsColumnEvent(AEvent: TcxSchedulerEvent; AIndex: Integer): Boolean;
begin

  Result := AEvent.IsDayEvent(GetColumnDate(AIndex));
  if Result and not DayView.ShowAllDayEventsInContentArea then
    Result := not AEvent.IsAllDayOrLonger;
  if Result then
    Result := AEvent.IsResourceEvent(GetResourceItemByIndex(GetColumnResourceIndex(AIndex)), ShowEventsWithoutResource);
end;

function TcxSchedulerDayViewViewInfo.IsColumnEvent(AEvent: TcxSchedulerEvent; AIndex: Integer; IsHeaderEvent: Boolean): Boolean;
begin
  Result := AEvent.IsDayEvent(GetColumnDate(AIndex)) and (IsHeaderEvent = AEvent.IsAllDayOrLonger);
  if Result then
    Result := AEvent.IsResourceEvent(GetResourceItemByIndex(GetColumnResourceIndex(AIndex)), ShowEventsWithoutResource);
end;

function TcxSchedulerDayViewViewInfo.IsContainerSelected(
  AResource: TcxSchedulerStorageResourceItem; ADate: TDateTime): Boolean;
begin
  Result := False;
  with DayView.Scheduler do
  begin
    if SelectedEventCount > 0 then Exit;
    if (AResource = SelResource) and (dxDateOf(ADate + 1) <= dxDateOf(SelFinish)) then
    begin
      if dxDateOf(ADate) > dxDateOf(SelStart) then
        Result := True
      else
        Result := (dxDateOf(ADate) = dxDateOf(SelStart)) and
          (GetTimeMinutes(SelStart) = 0);
    end;
  end;
  Result := Result and CanSelected;
end;

function TcxSchedulerDayViewViewInfo.IsEventVisible(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := (dxTimeOf(AEvent.Start) <= PrintTo) and (dxTimeOf(AEvent.Finish) >= PrintFrom);
end;

function TcxSchedulerDayViewViewInfo.IsEventVisible(var AStartRow, AStopRow: Integer): Boolean;
begin
  Result := (AStopRow >= PrintStartRow) and (AStartRow <= PrintFinishRow);
  if Result then
  begin
    AStopRow := Min(PrintFinishRow, AStopRow);
    AStartRow := Max(PrintStartRow, AStartRow);
  end;
end;

function TcxSchedulerDayViewViewInfo.IsHourVisible(AIndex: Integer): Boolean;
begin
  Result := not HasVisibleBounds or ((PrintStartRow div LinePerHour <= AIndex) and
    (AIndex <= PrintFinishRow div LinePerHour));
end;

function TcxSchedulerDayViewViewInfo.IsRowVisible(AIndex: Integer): Boolean;
begin
  if not HasVisibleBounds then
    Result := (AIndex >= FTopIndex) and (AIndex <= FTopIndex + FVisibleRowCount)
  else
    Result := (AIndex >= PrintStartRow) and (AIndex <= PrintFinishRow);
end;

procedure TcxSchedulerDayViewViewInfo.MakeTimeVisible(
  const ATime: TDateTime);
var
  ARow: Integer;
begin
  ARow := GetTimeRow(ATime, True);
  if (ARow < FTopIndex) or (ARow >= (FTopIndex + VisibleRowCount)) then
  begin
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
    if ARow < FTopIndex then
      FTopIndex := ARow
    else
      FTopIndex := Max(0, (ARow + 1) - VisibleRowCount);
  end;
end;

function TcxSchedulerDayViewViewInfo.NavigationButtonOffset(AKind: TcxSchedulerContentNavigationButtonKind;
  AResourceIndex: Integer): Integer;
begin
  if (AKind = nbkNext) and GroupByDate and (AResourceIndex < ResourceCount - 1) then
    Result := -ScaleFactor.Apply(1)
  else
    Result := 0;
end;

procedure TcxSchedulerDayViewViewInfo.SetColumnTimeLineStyle(
  AEventInfo: TcxSchedulerEventCellViewInfo; AColIndex: Integer);
var
  ACell: TcxSchedulerContentCellViewInfo;
  I, AOffset, StartRow, StopRow: Integer;
begin
  AOffset := (ContentCells.Count div ColCount) * AColIndex;
  StartRow := GetEventColumnRow(AEventInfo.Event.Start, AColIndex, True) + AOffset;
  StopRow := GetEventColumnRow(AEventInfo.Event.Finish - 1, AColIndex, False) + AOffset;
  for I := StartRow to StopRow do
    if I < ContentCells.Count then
    begin
      ACell := TcxSchedulerContentCellViewInfo(ContentCells[I]);
      if SchedulerStyle = svsClassic then
      begin
        if ACell.TimeEvent = nil then
          ACell.TimeEvent := AEventInfo;
      end
      else
        if (ACell.TimeEvent = nil) or
          ((ACell.TimeEvent.Event.State <> 3) and (ACell.TimeEvent.Event.State < AEventInfo.Event.State)) then
         ACell.TimeEvent := AEventInfo;
    end;
end;

procedure TcxSchedulerDayViewViewInfo.SetContainerCellHeaderEvent(AEventInfo: TcxSchedulerEventCellViewInfo);
var
  I: Integer;
  AContainerCell: TcxSchedulerContainerCellViewInfo;
  ADate: TDateTime;
begin
  for I := 0 to HeaderContainerCells.Count - 1 do
  begin
    AContainerCell := GetContainer(I);
    ADate := Int(AContainerCell.DateTime);
    if TcxSchedulerDateTimeHelper.Intersect(AEventInfo.Event.Start, AEventInfo.Event.Finish, ADate, ADate + 1) then
      if SchedulerStyle = svsClassic then
      begin
       if AContainerCell.HeaderEvent = nil then
          AContainerCell.HeaderEvent := AEventInfo
      end
      else
        if ((AContainerCell.HeaderEvent = nil) or
            ((AContainerCell.HeaderEvent <> nil) and (AContainerCell.HeaderEvent.Event.State <> 3) and
             (AContainerCell.HeaderEvent.Event.State < AEventInfo.Event.State))) and
           ((AContainerCell.Resource = nil) or VarEquals(AContainerCell.Resource.ResourceID, AEventInfo.Event.ResourceID)) then
         AContainerCell.HeaderEvent := AEventInfo;
  end;
end;

function TcxSchedulerDayViewViewInfo.ShowEventTimeLineOnContentCell: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDayViewViewInfo.ShowTimeLineOnHeaderEvent: Boolean;
begin
  Result := False;
end;

function TcxSchedulerDayViewViewInfo.TimeRulerCellClass: TcxSchedulerTimeRulerCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerTimeRulerCellViewInfoClass =
    (TcxSchedulerTimeRulerCellViewInfo, TcxSchedulerTimeRulerModernCellViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerDayViewViewInfo.GetAllDayEventMaxCount: Integer;
var
  I, J: Integer;
  ACount: Integer;
begin
  Result := 0;
  for J := 0 to FColCount - 1 do
  begin
    ACount := 0;
    for I := 0 to Builder.EventPlaceCount - 1 do
      if (Builder.EventPlaces[I].ColStart <= J) and (Builder.EventPlaces[I].ColFinish >= J) and
        not DayView.ShowAllDayEventsInContentArea then
          Inc(ACount);
    Result := Max(Result, ACount);
  end;
end;

function TcxSchedulerDayViewViewInfo.GetAutoHeight: Boolean;
begin
  Result := DayView.AutoContentHeight or Adapter.IsPrinting;
end;

function TcxSchedulerDayViewViewInfo.GetColumnInGroup: Integer;
begin
  Result := DayCount;
  if GroupingKind = gkByDate then
    Result := ResourceCount;
end;

function TcxSchedulerDayViewViewInfo.GetContentHeight: Integer;
begin
  Result := Bounds.Bottom - FContentOffset;
end;

function TcxSchedulerDayViewViewInfo.GetController: TcxSchedulerDayViewController;
begin
  Result := TcxSchedulerDayViewController(DayView.Controller);
end;

function TcxSchedulerDayViewViewInfo.GetDayView: TcxSchedulerDayView;
begin
  Result := TcxSchedulerDayView(Owner);
end;

function TcxSchedulerDayViewViewInfo.GetGroupCount: Integer;
begin
  if GroupingKind = gkByDate then
    Result := DayCount
  else
    if GroupingKind = gkByResource then
      Result := ResourceCount
    else
      Result := 1;
end;

function TcxSchedulerDayViewViewInfo.GetHeaderContainer: Boolean;
begin
  Result := DayView.HeaderContainer;
end;

function TcxSchedulerDayViewViewInfo.GetPrintRowCount: Integer;
begin
  Result := FPrintFinishRow - FPrintStartRow + UnusedRowCount + 1;
end;

function TcxSchedulerDayViewViewInfo.GetLinePerHour: Integer;
begin
  Result := 60 div TimeScale;
end;

function TcxSchedulerDayViewViewInfo.NeedAllDayScrollBar: Boolean;
begin
  Result := False;
  if not DayView.Active then
    Exit;
  case DayView.AllDayAreaScrollBar of
    adsbNever:
      Result := False;
    adsbAlways:
      Result := True;
    else
      Result := (FHeaderLineCount < GetAllDayEventMaxCount);
  end;
end;

procedure TcxSchedulerDayViewViewInfo.SetLargeFont(AFont: TFont);
begin
  FLargeFont.Assign(AFont);
  if SchedulerStyle = svsClassic then
    FLargeFont.Size := AFont.Size * 2
  else
    FLargeFont.Size := (AFont.Size * 3) div 2;
end;

procedure TcxSchedulerDayViewViewInfo.UpdateAllDayScrollBarParams;
var
  AScrollBar: IcxControlScrollBar;
begin
  AScrollBar := DayView.AllDayScrollBar;
  AScrollBar.Kind := sbVertical;
  AScrollBar.Min := 0;
  AScrollBar.Max := Math.Max(AScrollBar.Min, GetAllDayEventMaxCount - FHeaderLineCount);
  AScrollBar.Enabled := AScrollBar.Max > AScrollBar.Min;
  AScrollBar.Position := Math.Min(AScrollBar.Max, FAllDayTopIndex);
  FAllDayTopIndex := AScrollBar.Position;
end;

{ TcxSchedulerDayViewHitTest }

function TcxSchedulerDayViewHitTest.GetContainerCell: TcxSchedulerContainerCellViewInfo;
begin
  if HitAtContainer then
    Result := TcxSchedulerContainerCellViewInfo(FHitObject)
  else
    Result := nil;
end;

function TcxSchedulerDayViewHitTest.GetTimeRulerCell: TcxSchedulerTimeRulerCellViewInfo;
begin
  if HitAtTimeRuler then
    Result := TcxSchedulerTimeRulerCellViewInfo(FHitObject)
  else
    Result := nil;
end;

{ TcxDayViewDragEventHelper }

function TcxDayViewDragEventHelper.GetOriginHitTestMask: Int64;
const
  Mask = (1 shl htcControl) or (1 shl htcDayHeader) or (1 shl htcTimeRuler)
    or (1 shl htcContainer) or (1 shl htcContent);
begin
  Result := Mask;
end;

procedure TcxDayViewDragEventHelper.GetOriginState;
begin
  inherited GetOriginState;
  FStartInHeader := FStartHitFlags and (1 shl htcContainer) <> 0;
end;

function TcxDayViewDragEventHelper.IsAtOrigin: Boolean;
begin
  Result := inherited IsAtOrigin or
    (((not ShowHeaderContainer and HitTest.HitAtDayHeader) or (FStartInHeader and InHeader)) and
    (Controller.StartDragHitTime = HitTest.Time) and
    ((FStartResource = nil) or (FStartResource = HitTest.Resource)));

end;

function TcxDayViewDragEventHelper.InHeader: Boolean;
begin
  with HitTest do
    Result := ShowHeaderContainer and (HitAtDayHeader or HitAtContainer);
end;

procedure TcxDayViewDragEventHelper.SetCloneEventsTimeDelta(AStart, ACurrent: TDateTime;
  AInHeader: Boolean);
var
  I: Integer;
  ADelta: TDateTime;
begin
  for I := 0 to Clones.Count - 1 do
  begin
    with Clones[I] do
    begin
      AllDayEvent := AInHeader and (Source.AllDayEvent or (Source.Duration < 1));
      if AInHeader then
      begin
        ADelta := dxDateOf(AStart) - dxDateOf(ACurrent);
        if not IsHeaderEvent(Source) then
        begin
          Duration := 1;
          MoveTo(dxDateOf(Source.Start) - ADelta);
        end
        else
        begin
          Duration := Source.Duration;
          MoveTo(Source.Start - ADelta);
        end;
      end
      else
        if not IsHeaderEvent(Source) or
          Controller.View.ShowAllDayEventsInContentArea then
        begin
          Duration := Source.Duration;
          MoveTo(Source.Start - (AStart - ACurrent));
        end
        else
        begin
          Duration := cxHalfHour;
          MoveTo(ACurrent);
        end;
    end;
  end;
end;

procedure TcxDayViewDragEventHelper.UpdateViewClonesTime;
begin
  with HitTest do
  begin
    if HitAtTimeRuler then
      // correct event's times
      SetCloneEventsTimeDelta(Controller.StartDragHitTime, dxDateOf(Controller.StartDragHitTime) + Time, False)
    else
      SetCloneEventsTimeDelta(Controller.StartDragHitTime, Time, InHeader);
  end;
  UpdateEventStates;
end;

procedure TcxDayViewDragEventHelper.UpdateEventStates;
var
  I: Integer;
begin
  for I := 0 to Clones.Count - 1 do
    CheckEventState(Clones[I]);
end;

function TcxDayViewDragEventHelper.GetController: TcxSchedulerDayViewController;
begin
  Result := TcxSchedulerDayViewController(inherited Controller);
end;

function TcxDayViewDragEventHelper.GetHitTest: TcxSchedulerDayViewHitTest;
begin
  Result := TcxSchedulerDayViewHitTest(inherited HitTest);
end;

function TcxDayViewDragEventHelper.GetShowHeaderContainer: Boolean;
begin
  Result := TcxSchedulerDayView(HitTest.View).HeaderContainer;
end;

{ TcxDayViewEventSizing }

function TcxDayViewEventSizing.GetDragCursor(Accepted: Boolean): TCursor;
const
  Cursors: array[Boolean] of TCursor = (crSchedulerVertResize,
    crSchedulerHorzResize);
begin
  if HasConflicts then
    Result := crNoDrop
  else
    Result := Cursors[IsHeaderEvent(Event) and HitTest.HitAtContainer];
end;

function TcxDayViewEventSizing.GetOriginHitTestMask: Int64;
const
  Mask = (1 shl htcControl) or (1 shl htcDayHeader) or (1 shl htcTimeRuler) or (1 shl htcContainer) or (1 shl htcContent);
begin
  Result := Mask;
end;

procedure TcxDayViewEventSizing.CalcAllDayEvent;
begin
  with Event do
  begin
    AllDayEvent := (HitTest.HitAtContainer or ((dxTimeOf(GetSizingTime) = 0) {and
      not Controller.View.ShowAllDayEventsInContentArea})) and (Source.AllDayEvent or (dxTimeOf(FFixedBoundTime) = 0));
    if Controller.DragKind = edkResizeStart then
      Finish := FFixedBoundTime
    else
      Start := FFixedBoundTime;
  end;
end;

function TcxDayViewEventSizing.GetFinishTime: TDateTime;
begin
  Result := HitTest.Time;
  if HitTest.HitAtTimeRuler then
    Result := Result + dxDateOf(Event.Finish)
  else
    if HitTest.HitAtContent then
      Result := Result +
        TcxSchedulerDayView(Scheduler.CurrentView).TimeScale * MinuteToTime
    else
      if (Controller.DragKind = edkResizeEnd) and not Event.AllDayEvent and
        HitTest.HitAtContainer and (dxTimeOf(Result) = 0) then Result := Result + 1;
end;

procedure TcxDayViewEventSizing.GetOriginState;
begin
  inherited GetOriginState;
  with Event do
  begin
    if Controller.DragKind = edkResizeStart then
      FFixedBoundTime := Finish
    else
      FFixedBoundTime := Start;
  end;
end;

function TcxDayViewEventSizing.GetSizingTime: TDateTime;
begin
  if Controller.DragKind = edkResizeStart then
    Result := GetStartTime
  else
    Result := GetFinishTime;
end;

function TcxDayViewEventSizing.GetStartTime: TDateTime;
begin
  Result := HitTest.Time;
  if HitTest.HitAtTimeRuler then
    Result := Result + dxDateOf(Event.Start);
end;

function TcxDayViewEventSizing.GetController: TcxSchedulerDayViewController;
begin
  Result := TcxSchedulerDayViewController(inherited Controller);
end;

function TcxDayViewEventSizing.GetHitTest: TcxSchedulerDayViewHitTest;
begin
  Result := TcxSchedulerDayViewHitTest(inherited HitTest);
end;

{ TcxSchedulerDayNavigation }

procedure TcxSchedulerDayNavigation.KeyDown(var AKey: Word; AShift: TShiftState);
const
  ANextCellKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
var
  ATime, ATimeDelta: TDateTime;
begin
  FSelRow := GetTimeRow(FCurrentAnchor);
  ATime := dxDateOf(FCurrentAnchor);
  ATimeDelta := 0;
  case AKey of
    VK_UP:
    begin
      if FSelRow = 0 then
        Exit
      else
        ATimeDelta := GetRowTime(FSelRow - 1);
    end;
    VK_DOWN:
    begin
      if FSelRow >= ViewInfo.FRowCount - 1 then
        Exit
      else
        ATimeDelta := GetRowTime(FSelRow + 1);
    end;
    VK_HOME:
      if not (ssCtrl in AShift) or DayView.WorkTimeOnly then
        ATimeDelta := dxTimeOf(DayView.WorkStart);
    VK_END:
    begin
      if (ssCtrl in AShift) and not DayView.WorkTimeOnly  then
        ATimeDelta := 1 - DayView.GetTimeIncrement
      else
        ATimeDelta := dxTimeOf(DayView.WorkFinish) - DayView.GetTimeIncrement;
    end;
    VK_LEFT, VK_RIGHT:
    begin
      DoNextColumn(AKey = ANextCellKey[DayView.UseRightToLeftAlignment], GetColumnByDate(ATime, ResourceObject),
        ATime, FCurrentResource);
      Exit;
    end;
    VK_NEXT, VK_PRIOR:
      if not DoNextPage(AKey = VK_NEXT, ATime, FCurrentResource) then Exit;
  end;
  ATime := ATime + ATimeDelta;
  ViewInfo.MakeTimeVisible(ATime);
  SetSelAnchor(ATime, AShift, FCurrentResource);
end;

procedure TcxSchedulerDayNavigation.ValidateSelection(var ASelStart, ASelFinish: TDateTime;
  var AResource: TcxSchedulerStorageResourceItem);
var
  I: Integer;
  AStart, AFinish, ADate: TDateTime;
begin
  inherited ValidateSelection(ASelStart, ASelFinish, AResource);
  if DayView.WorkTimeOnly and ((dxTimeOf(ASelStart) <> 0) or
    (dxTimeOf(ASelFinish + ViewInfo.DayView.GetTimeIncrement) <> 0)) then
  begin
    ASelStart := ValidateTimeVisibility(ASelStart);
    ASelFinish := ValidateTimeVisibility(ASelFinish);
  end;
  AStart := dxDateOf(Min(ASelFinish, ASelStart));
  AFinish := dxDateOf(Max(ASelFinish, ASelStart));
  for I := 0 to ViewInfo.DayCount - 1 do
  begin
    ADate := ViewInfo.Days[I];
    if (ADate >= AStart) and (ADate <= AFinish) then Exit
  end;
  ASelFinish := GetColumnDate(0) + dxTimeOf(ASelFinish);
  ASelStart := ASelFinish;
end;

function TcxSchedulerDayNavigation.ValidateTimeVisibility(var ADateTime: TDateTime): TDateTime;
begin
  Result := dxDateOf(ADateTime) + Min(DayView.WorkFinish,
    Max(DayView.WorkStart, dxTimeOf(ADateTime)));
end;

function TcxSchedulerDayNavigation.ColCount: Integer;
begin
  Result := ViewInfo.ColCount;
end;

procedure TcxSchedulerDayNavigation.DoNextColumn(AGoToNext: Boolean; AColumn: Integer;
  const ATime: TDateTime; AResource: TcxSchedulerStorageResourceItem);
const
  AColInc: array[Boolean] of Integer = (-1, 1);
begin
  if IsResourceNavigation(AGoToNext, AColumn, ATime) then Exit;
  if ((AGoToNext and (AColumn = ColCount - 1)) or (not AGoToNext and (AColumn = 0))) then
  begin
    DayView.ScrollVisibleDays(not AGoToNext);
    ReplaceDate(ATime + AColInc[AGoToNext], GetColumnResource(AColumn));
  end
  else
  begin
    Inc(AColumn, AColInc[AGoToNext]);
    if (AResource <> GetColumnResource(AColumn)) then
      ReplaceDate(GetColumnDate(AColumn), GetColumnResource(AColumn))
    else
      if not (ssShift in FShift) then
        ReplaceDate(GetColumnDate(AColumn), GetColumnResource(AColumn))
      else
        SetSelAnchor(GetColumnDate(AColumn) + dxTimeOf(FCurrentAnchor), FShift, AResource);
  end;
end;

function TcxSchedulerDayNavigation.DoNextPage(AGoForward: Boolean;
  var ATime: TDateTime; var AResource: TcxSchedulerStorageResourceItem): Boolean;
var
  ARow: Integer;
begin
  if AGoForward then
    Result := FSelRow < (ViewInfo.FRowCount - 1)
  else
    Result := FSelRow > 0;
  if Result then
  begin
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
    if AGoForward then
    begin
      ARow := Min(FSelRow + ViewInfo.VisibleRowCount, ViewInfo.FRowCount - 1);
      ViewInfo.FTopIndex := Min(ViewInfo.FTopIndex + ViewInfo.VisibleRowCount, ViewInfo.FRowCount - 1)
    end
    else
    begin
      ARow := Max(0, FSelRow - ViewInfo.VisibleRowCount);
      ViewInfo.FTopIndex := Max(0, ViewInfo.FTopIndex - ViewInfo.VisibleRowCount);
    end;
    ATime := ATime + GetRowTime(ARow);
  end;
end;

function TcxSchedulerDayNavigation.GetColumnByDate(const ADate: TDateTime; AResource: TObject): Integer;
begin
  for Result := 0 to ViewInfo.ColCount - 1 do
    if (ViewInfo.GetColumnDate(Result) >= dxDateOf(ADate)) and (ViewInfo.GetColumnResource(Result) = AResource) then
      Exit;
  Result := 0;
end;

function TcxSchedulerDayNavigation.GetColumnDate(AColumn: Integer): TDateTime;
begin
  Result := ViewInfo.GetColumnDate(AColumn);
end;

function TcxSchedulerDayNavigation.GetColumnResource(AColumn: Integer): TcxSchedulerStorageResourceItem;
begin
  Result := GetResourceFromViewInfo(ViewInfo.Containers[AColumn].Resource);
end;

function TcxSchedulerDayNavigation.GetRowTime(const ARow: Integer; AFinish: Boolean = False): TDateTime;
begin
  Result := RoundTime((ARow + ViewInfo.PrintStartRow + Byte(AFinish)) * DayView.TimeScale * MinuteToTime);
end;

function TcxSchedulerDayNavigation.GetTimeRow(const ATime: TDateTime): Integer;
begin
  Result := ViewInfo.GetTimeRow(ATime, True);
end;

function TcxSchedulerDayNavigation.IsResourceNavigation(AGoToNext: Boolean;
  AColumn: Integer; const ATime: TDateTime): Boolean;

  function GetActualDate(AIsFirst: Boolean): TDateTime;
  begin
    if AIsFirst then
      Result := Integer(ViewInfo.SelectedDays.First)
    else
      Result := Integer(ViewInfo.SelectedDays.Last);
  end;

var
  AResource: TcxSchedulerStorageResourceItem;
begin
  Result := True;
  if ViewInfo.GroupByResource and ((AGoToNext and (AColumn = ColCount - 1)) or
    (not AGoToNext and (AColumn = 0))) then
  begin
    if ScrollResourcesEx(AGoToNext, AResource) then
    begin
      ReplaceDate(GetActualDate(AGoToNext), AResource);
      Exit;
    end;
  end
  else
    if ViewInfo.GroupByDate and ((IsLastResource and AGoToNext) or
      (IsFirstResource and not AGotoNext)) then
    begin
      if ScrollResourcesEx(AGoToNext, AResource) then
      begin
        TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
        ReplaceSelParams(AResource);
        Exit;
      end
      else
        if not ((AGoToNext and (AColumn = ColCount - 1)) or
          (not AGoToNext and (AColumn = 0))) and  ScrollResourcesCycled(AGoToNext, AResource) then
             ReplaceSelParams(AResource);
    end;
  Result := False;
end;

function TcxSchedulerDayNavigation.GetDayView: TcxSchedulerDayView;
begin
  Result := ViewInfo.DayView;
end;

function TcxSchedulerDayNavigation.GetSelectedDays: TcxSchedulerDateList;
begin
  Result := ViewInfo.SelectedDays;
end;

function TcxSchedulerDayNavigation.GetViewInfo: TcxSchedulerDayViewViewInfo;
begin
  Result := TcxSchedulerDayViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerDayViewController }

constructor TcxSchedulerDayViewController.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FTimer := TTimer.Create(nil);
  FScrollAreaRects := TList.Create;
end;

destructor TcxSchedulerDayViewController.Destroy;
begin
  FScrollAreaRects.Free;
  FTimer.Free;
  inherited Destroy;
end;

procedure TcxSchedulerDayViewController.BeforeMouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FIsEditingBeforeMouseDown := IsEditing;
  inherited BeforeMouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerDayViewController.CancelScroll;
begin
  InitTimer(False, scEndScroll);
end;

procedure TcxSchedulerDayViewController.CheckDragDropScrolling(const P: TPoint);
var
  AAccepted: Boolean;
begin
  if DragEventHelper <> nil then
  begin
    AAccepted := True;
    DoSchedulerDragOver(P, dsDragMove, AAccepted);
    AAccepted := AAccepted and cxRectPtIn(View.ViewInfo.Bounds, P);
    DragEventHelper.DragOver(P, dsDragMove, AAccepted);
    DragEventHelper.UpdateHelperState(AAccepted);
  end;
end;

procedure TcxSchedulerDayViewController.CheckScrolling(const APos: TPoint);
var
  ACanScroll: Boolean;
  AScrollCode: TScrollCode;
  R: TRect;
begin
  if FLockScrolling then Exit;
  R := ViewInfo.Bounds;
  R.Top := ViewInfo.FContentOffset;
  ACanScroll := cxRectPtIn(R, APos);
  AScrollCode := TScrollCode(FTimer.Tag);
  if (APos.Y >= R.Top) and (APos.Y < (R.Top + cxScrollZoneSize)) then
    AScrollCode := scLineUp
  else
    if (APos.Y < R.Bottom) and (APos.Y >= (R.Bottom - cxScrollZoneSize)) then
      AScrollCode := scLineDown
    else
      ACanScroll := False;
  if (ACanScroll <> FTimer.Enabled) or (Integer(AScrollCode) <> FTimer.Tag) then
    InitTimer(ACanScroll, AScrollCode);
end;

procedure TcxSchedulerDayViewController.CheckTimeRulerTime;
begin
  if HitTest.HitAtTime and HitTest.HitAtTimeRuler and (DragEventHelper = nil) then
    HitTest.SetHitTime(htcTime, dxDateOf(Navigation.SelAnchor) + dxTimeOf(HitTest.Time));
end;

function TcxSchedulerDayViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxDayViewDragEventHelper.Create(Scheduler);
end;

function TcxSchedulerDayViewController.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerDayNavigation.Create(View);
end;

function TcxSchedulerDayViewController.CreateResizeEventHelper: TcxEventSizingHelper;
begin
  Result := TcxDayViewEventSizing.Create(Scheduler);
end;

procedure TcxSchedulerDayViewController.InitTimer(
  AllowStart: Boolean; AScrollCode: TScrollCode);
begin
  if AllowStart and FIsEditingBeforeMouseDown then
  begin
    FIsEditingBeforeMouseDown := False;
    AllowStart := False;
  end;
  if not AllowStart then
    FTimer.OnTimer := nil
  else
    FTimer.OnTimer := OnTimer;
  FTimer.Enabled := AllowStart;
  FTimer.Interval := cxScrollInterval;
  FTimer.Tag := Integer(AScrollCode);
end;

procedure TcxSchedulerDayViewController.MouseDown(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (DragKind = edkNone) and not HitTest.HitAtEvent then
  begin
    FLockScrolling := False;
    CloseInplaceEdit;
    UnselectEvents;
    if HitTest.HitAtContainer and not HitTest.HitAtDayHeader then
    begin
      FLockScrolling := True;
      with HitTest.ContainerCell do
      begin
        Navigation.ReplaceSelParams(DateTime, DateTime + 1 - Navigation.TimeIncrement, HitTest.Resource);
        FStartSelAnchor := DateTime - Navigation.TimeIncrement;
      end;
      if (Shift = [ssLeft, ssDouble]) and CanCreateEventUsingDialog then
        Scheduler.CreateEventUsingDialog
      else
        View.Refresh;
      Exit;
    end
    else
      if HitTest.HitAtTimeRuler then
      begin
        HitTest.SetHitTime(htcTime, GetTimeRulerTime);
        FLockScrolling := HitTest.HitAtButton;
      end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TcxSchedulerDayViewController.MouseMove(
  Shift: TShiftState; X, Y: Integer);
var
  AStart, AFinish: TDateTime;
begin
  CheckTimeRulerTime;
  if CanProcessMouseMove and (ssLeft in Shift) and (DragKind = edkNone) then
  begin
    if HitTest.HitAtContainer and not HitTest.HitAtDayHeader then
    begin
      InitTimer(False, scEndScroll);
      AStart := dxDateOf(Navigation.SelRealStart);
      AFinish := HitTest.ContainerCell.DateTime;
      HitTest.SetBitState(htcTime, False);
      HitTest.SetBitState(htcContainer, False);
      if Navigation.SelResource = HitTest.Resource then
      begin
        if AFinish >= AStart then
          Navigation.ReplaceSelParams(AStart, AFinish + 1 - Navigation.TimeIncrement)
        else
          Navigation.ReplaceSelParams(AStart + 1 - Navigation.TimeIncrement, AFinish);
      end
      else
        Navigation.ReplaceSelParams(AStart, AStart + 1 - Navigation.TimeIncrement,
          Navigation.SelResource);
      View.Refresh;
      Exit;
    end
    else
      if HitTest.HitAtTimeRuler then
      begin
        Navigation.SetSelAnchor(GetTimeRulerTime, [ssShift] + Shift);
        CheckScrolling(cxPoint(X, Y));
        Exit;
      end;
  end;
  CheckDragDropScrolling(cxPoint(X, Y));
  inherited MouseMove(Shift, X, Y);
  if CanProcessMouseMove and (ssLeft in Shift) then
    CheckScrolling(cxPoint(X, Y));
end;

procedure TcxSchedulerDayViewController.MouseUp(
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  FLockScrolling := False;
  CancelScroll;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TcxSchedulerDayViewController.OnTimer(Sender: TObject);

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
    APos := ViewInfo.FTopIndex;
    ViewInfo.ScrollVertical(TScrollCode(FTimer.Tag), APos);
    AMousePos := View.ScreenToClient(GetMouseCursorPos);
    MouseMove(AShift, AMousePos.X, AMousePos.Y);
    if (DragKind in [edkResizeStart, edkResizeEnd]) then
      CheckUpdateEventBounds;
  end;
end;

procedure TcxSchedulerDayViewController.SelectNextEvent(AForward: Boolean);
var
  ASelectedIndex: Integer;
  I: Integer;
  AAllDayTopIndex: Integer;
  AEventInHeaderIndex: Integer;
  AToDown: Boolean;

  function GetEventInHeaderIndex(AEvent: TcxSchedulerEvent): Integer;
  var
    I: Integer;
    AMaxCount: Integer;
  begin
    Result := -1;
    AMaxCount := ViewInfo.GetAllDayEventMaxCount;
    for I := 0 to ViewInfo.Builder.EventPlaceCount - 1 do
      if ViewInfo.Builder.EventPlaces[I].Event = AEvent then
      begin
        Result := ViewInfo.Builder.EventPlaces[I].LineStart + AAllDayTopIndex;
        if Result > AMaxCount then
          Result := Result - AMaxCount - 1;
        Break;
      end;
  end;

  procedure GetNextSelectedIndex(var AIndex: Integer);
  begin
    if AForward then
      Inc(AIndex)
    else
      Dec(AIndex);
  end;

begin
  if View.EventList.Count = 0 then
    Exit;
  if not ViewInfo.NeedAllDayScrollBar then
  begin
    inherited SelectNextEvent(AForward);
    Exit;
  end;
  ASelectedIndex := -1;
  for I := 0 to View.EventList.Count - 1 do
    if View.EventList[I].Selected then
    begin
      ASelectedIndex := I;
      Break;
    end;
  GetNextSelectedIndex(ASelectedIndex);
  if ASelectedIndex < -1 then
    ASelectedIndex := View.EventList.Count - 1;
  if (ASelectedIndex < View.EventList.Count) and
    (ASelectedIndex >= 0) then
  begin
    AAllDayTopIndex := ViewInfo.AllDayTopIndex;
    AEventInHeaderIndex := GetEventInHeaderIndex(View.EventList[ASelectedIndex]);
    while ((ASelectedIndex < View.EventList.Count) and (ASelectedIndex >= 0)) and
      (not View.GetEventVisibility(View.EventList[ASelectedIndex]) and (AEventInHeaderIndex = -1)) do
    begin
      GetNextSelectedIndex(ASelectedIndex);
      AEventInHeaderIndex := GetEventInHeaderIndex(View.EventList[ASelectedIndex]);
    end;
    if (ASelectedIndex < View.EventList.Count) and
      (ASelectedIndex >= 0) then
    begin
      SelectSingleEvent(View.EventList[ASelectedIndex], NullDate);
      if AEventInHeaderIndex <> -1 then
      begin
        AToDown := AEventInHeaderIndex > ViewInfo.AllDayTopIndex;
        while not View.GetEventVisibility(View.EventList[ASelectedIndex]) do
        begin
          if AToDown then
            ViewInfo.AllDayScroll(scLineDown, AAllDayTopIndex)
          else
            ViewInfo.AllDayScroll(scLineUp, AAllDayTopIndex);
          if AEventInHeaderIndex = ViewInfo.AllDayTopIndex then
            Break;
        end;
      end;
    end
    else
      UnselectEvents;
  end
  else
    UnselectEvents;
  View.Refresh;
end;

procedure TcxSchedulerDayViewController.CheckNavigatorScrollArea(
  const APoint: TPoint);

  function CheckArea(const ARect: TRect; AIsUpArea: Boolean): Boolean;
  var
    R: TRect;
    DW, I: Integer;
  begin
    Result := PtInArea(ARect, APoint, AIsUpArea);
    if not Result and (FScrollAreaRects.Count > 0) then
    begin
      DW := View.OptionsView.GroupSeparatorWidth div 2;
      for I := 0 to FScrollAreaRects.Count - 1 do
      begin
        if I mod 2 <> Byte(AIsUpArea) then Continue;
        if not AIsUpArea then
          R := cxRectSetLeft(ARect, Integer(FScrollAreaRects[I]) - DW, cxScrollZoneSize)
        else
          R := cxRectSetRight(ARect, Integer(FScrollAreaRects[I]) + DW + 1, cxScrollZoneSize);
        Result := PtInRect(R, APoint);
        if Result then Break;
      end;
    end;
  end;

var
  ACode: TScrollCode;
begin
  if CheckArea(FUpScrollArea, False) then
    ACode := scLineDown
  else
    if CheckArea(FDownScrollArea, True) then
      ACode := scLineUp
    else
      ACode := scEndScroll;
  NavigatorTimer.Tag := Byte(ACode);
  if not NavigatorTimer.Enabled then
    NavigatorTimer.Interval := cxNavigatorStartTimer;
  NavigatorTimer.Enabled := ACode <> scEndScroll;
end;

procedure TcxSchedulerDayViewController.DoneNavigatorScrollArea;
begin
  FScrollAreaRects.Clear;
  inherited DoneNavigatorScrollArea;
end;

procedure TcxSchedulerDayViewController.InitNavigatorScrollArea;
var
  I: Integer;
begin
  FScrollAreaRects.Clear;
  if ViewInfo.GroupingKind = gkByDate then
  begin
    for I := 0 to ViewInfo.GroupSeparatorCells.Count - 1 do
      with ViewInfo.GroupSeparatorCells[I] do
      begin
        FScrollAreaRects.Add(Pointer(Bounds.Left));
        FScrollAreaRects.Add(Pointer(Bounds.Right));
      end;
  end;
  inherited InitNavigatorScrollArea;
end;

function TcxSchedulerDayViewController.GetDragEventHelper: TcxDayViewDragEventHelper;
begin
  Result := TcxDayViewDragEventHelper(inherited DragEventHelper);
end;

function TcxSchedulerDayViewController.GetHitTest: TcxSchedulerDayViewHitTest;
begin
  Result := View.HitTest;
end;

function TcxSchedulerDayViewController.GetTimeRulerTime: TDateTime;
begin
  Result := dxTimeOf(HitTest.Time) + dxDateOf(Navigation.SelAnchor);
end;

function TcxSchedulerDayViewController.GetView: TcxSchedulerDayView;
begin
  Result := TcxSchedulerDayView(inherited View);
end;

function TcxSchedulerDayViewController.GetViewInfo: TcxSchedulerDayViewViewInfo;
begin
  Result := View.ViewInfo;
end;

{ TcxSchedulerDayViewPainter }

procedure TcxSchedulerDayViewPainter.Paint;
begin
  inherited Paint;
  ViewInfo.ContentCells.Draw(Canvas, DrawContentCell);
  ViewInfo.HeaderContainerCells.Draw(Canvas, DrawContainerCell);
  ViewInfo.EventCells.Draw(Canvas, DrawDayViewEventCell);
  ViewInfo.ResourceHeaderCells.Draw(Canvas, DrawResourceHeaderCell);
  ViewInfo.GroupSeparatorCells.Draw(Canvas, DrawGroupSeparatorCell);
  ViewInfo.DayHeaderCells.Draw(Canvas, DrawHeaderCell);
  ViewInfo.TimeRulerCells.Draw(Canvas, DrawTimeRulerCell);
  ViewInfo.Buttons.Draw(Canvas, DrawButtonCell);
  ViewInfo.DrawNavigationButtons(Canvas, DrawButtonCell);
  DrawBackgroundCell(ViewInfo.Background);
end;

procedure TcxSchedulerDayViewPainter.DrawContainerCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  View.DoCustomDrawContainer(TcxSchedulerContainerCellViewInfo(AItem), ADone);
end;

procedure TcxSchedulerDayViewPainter.DrawDayViewEventCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  DrawEventCell(AItem, ADone);
end;

procedure TcxSchedulerDayViewPainter.DrawTimeRulerCell(AItem: TcxSchedulerCustomViewInfoItem; var ADone: Boolean);
begin
  View.DoCustomDrawTimeRuler(TcxSchedulerTimeRulerCellViewInfo(AItem), ADone);
end;

function TcxSchedulerDayViewPainter.GetView: TcxSchedulerDayView;
begin
  Result := ViewInfo.DayView;
end;

function TcxSchedulerDayViewPainter.GetViewInfo: TcxSchedulerDayViewViewInfo;
begin
  Result := TcxSchedulerDayViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerTimeRulerPopupMenu }

constructor TcxSchedulerTimeRulerPopupMenu.Create(AScheduler: TcxCustomScheduler);
begin
  inherited Create(AScheduler);
  FItems := [rpmiNewEvent, rpmiNewAllDayEvent, rpmiNewReccuringEvent, rpmi60min,
    rpmi30min, rpmi15min, rpmi10min, rpmi6min, rpmi5min];
end;

procedure TcxSchedulerTimeRulerPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerTimeRulerPopupMenu then
  begin
    Items := TcxSchedulerTimeRulerPopupMenu(Source).Items;
    OnClick := TcxSchedulerTimeRulerPopupMenu(Source).OnClick;
    OnPopup := TcxSchedulerTimeRulerPopupMenu(Source).OnPopup;
  end;
end;

procedure TcxSchedulerTimeRulerPopupMenu.Execute(AItem: TcxSchedulerTimeRulerPopupMenuItem);
begin
  ExecuteCommand(Ord(AItem));
end;

function TcxSchedulerTimeRulerPopupMenu.GetMenuItem(AItem: TcxSchedulerTimeRulerPopupMenuItem): TMenuItem;
begin
  Result := FindItemByCommand(Root, Ord(AItem));
end;

procedure TcxSchedulerTimeRulerPopupMenu.CreateItems;

  function GetChecked(ACommand: Integer): Boolean;
  var
    AIndex: Integer;
  begin
    Result := False;
    AIndex := ACommand - Ord(rpmi60min);
    if (AIndex < 0) or (AIndex > 5) then Exit;
    Result := DayView.TimeScale = RulerScales[AIndex];
  end;

  procedure CreateRulerItem(const ACaption: string;
    AItem: TcxSchedulerTimeRulerPopupMenuItem);
  var
    ACommand: Integer;
  begin
    if AItem in Items then
    begin
      ACommand := Ord(AItem);
      CreateSubItem(Root, ACaption, ACommand, -1, True, GetChecked(ACommand));
    end;
  end;

begin
  CreateNewEventItems(rpmiNewEvent in FItems, rpmiNewAllDayEvent in FItems,
    rpmiNewReccuringEvent in FItems, Ord(rpmiNewEvent), Ord(rpmiNewAllDayEvent),
    Ord(rpmiNewReccuringEvent));
  if Items * [rpmi60min, rpmi30min, rpmi15min, rpmi10min, rpmi6min, rpmi5min] <> [] then
  begin
    AddValidSeparator(Root);
    CreateRulerItem(cxGetResourceString(@scxpm60Minutes), rpmi60min);
    CreateRulerItem(cxGetResourceString(@scxpm30Minutes), rpmi30min);
    CreateRulerItem(cxGetResourceString(@scxpm15Minutes), rpmi15min);
    CreateRulerItem(cxGetResourceString(@scxpm10Minutes), rpmi10min);
    CreateRulerItem(cxGetResourceString(@scxpm6Minutes), rpmi6min);
    CreateRulerItem(cxGetResourceString(@scxpm5Minutes), rpmi5min);
  end;
end;

procedure TcxSchedulerTimeRulerPopupMenu.DoExecute(ACommand: Integer);
var
  AIndex: Integer;
begin
  if ACommand in [Ord(rpmiNewEvent), Ord(rpmiNewAllDayEvent), Ord(rpmiNewReccuringEvent)] then
    inherited DoExecute(ACommand)
  else
  begin
    AIndex := ACommand - Ord(rpmi60min);
    if AIndex in [0..5] then
      DayView.TimeScale := RulerScales[AIndex];
  end;
end;

function TcxSchedulerTimeRulerPopupMenu.DoOnClick(ACommand: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnClick) then
    FOnClick(Self, TcxSchedulerTimeRulerPopupMenuItem(ACommand), Result);
end;

function TcxSchedulerTimeRulerPopupMenu.DoOnPopup: Boolean;
begin
  Result := False;
  if Assigned(FOnPopup) then
    FOnPopup(Self, InternalMenu, Result);
end;

function TcxSchedulerTimeRulerPopupMenu.IsValidCommand(ACommand: Integer): Boolean;
begin
  Result := (ACommand >= Ord(rpmiNewEvent)) and (ACommand <= Ord(rpmi5min));
end;

function TcxSchedulerTimeRulerPopupMenu.GetDayView: TcxSchedulerDayView;
begin
  Result := Scheduler.CurrentView as TcxSchedulerDayView;
end;

end.


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

unit cxSchedulerWeekView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Types, Windows, Forms, Classes, cxClasses, SysUtils, Graphics, DateUtils, Math, RTLConsts,
  StdCtrls, dxCore, cxGraphics, cxDateUtils, Controls, cxControls, cxSchedulerCustomControls,
  dxCoreClasses, cxLookAndFeelPainters, cxGeometry, cxStyles, dxRangeControl, cxDrawTextUtils,
  cxSchedulerStorage, cxSchedulerUtils, cxSchedulerCustomResourceView;

type
  TcxSchedulerCustomWeekView = class;
  TcxSchedulerWeekViewViewInfo = class;
  TcxSchedulerWeeksViewViewInfo = class;
  TcxSchedulerCustomWeekViewNavigation = class;

  { TcxSchedulerCustomWeekViewController }

  TcxSchedulerCustomWeekViewController = class(TcxSchedulerCustomResourceViewController)
  private
    function GetNavigation: TcxSchedulerCustomWeekViewNavigation;
    function GetView: TcxSchedulerCustomWeekView;
  protected
    function CreateDragEventHelper: TcxDragEventHelper; override;
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function CreateResizeEventHelper: TcxEventSizingHelper; override;
    function GetDayHeaderDate: TDateTime; override;
    function NeedShowDayHeaderHint: Boolean; override;

    property View: TcxSchedulerCustomWeekView read GetView;
  public
    property Navigation: TcxSchedulerCustomWeekViewNavigation read GetNavigation;
  end;

  { TcxCustomWeekViewDragEventHelper }

  TcxCustomWeekViewDragEventHelper = class(TcxDragEventHelper)
  protected
    procedure UpdateViewClonesTime; override;
  end;

  { TcxCustomWeekViewEventSizing }

  TcxCustomWeekViewEventSizing = class(TcxEventSizingHelper)
  protected
    procedure UpdateEventBounds; override;
  end;

  TcxSchedulerWeekViewDaysLayout = (wdlTwoColumns, wdlOneColumn);

  { TcxSchedulerCustomWeekView }

  TcxSchedulerCustomWeekView = class(TcxSchedulerCustomResourceView)
  private
    FCompressWeekEnd: Boolean;
    FDaysLayout: TcxSchedulerWeekViewDaysLayout;
    FHideWeekEnd: Boolean;
    FForceShowWeekEnd: Boolean;
    function GetController: TcxSchedulerCustomWeekViewController;
    function GetScrollPos(const ADatePos: TDateTime; ACheckBounds: Boolean = True): Integer;
    function GetHideWeekEnd: Boolean;
    function GetWeekDayCount: Integer;
    procedure SetCompressWeekEnd(const AValue: Boolean);
    procedure SetDaysLayout(Value: TcxSchedulerWeekViewDaysLayout);
    procedure SetHideWeekEnd(Value: Boolean);
    function GetWorkWeekDayCount: Integer;
  protected
    FWeeks: TcxSchedulerDateList;
    function CanSelectIndividualDays: Boolean; override;
    function CheckDate(const ADate: TDateTime): Integer;
    procedure CheckWorkDays;
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateNavigation: TcxSchedulerViewNavigation; virtual;
    function CreatePainter: TcxSchedulerSubControlPainter; override;
    function GetCompressWeekEnd: Boolean; override;
    function GetDateScrollBarKind: TScrollBarKind;
    function GetFirstVisibleDate: TDateTime; override;
    function GetLastVisibleDate: TDateTime; override;
    function GetMaxScrollPos: Integer; virtual;
    function GetMinScrollPos: Integer; virtual;
    procedure GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList); override;
    function GetScrollTimeHint: string; override;
    function GetWeekCount: Integer;
    function GetStartOfWeek: TDay; override;
    function GetViewContentRect: TRect; override;
    procedure InitEventBySelectedTime(AEvent: TcxSchedulerEvent;
      AllDay: Boolean; ARecurrence: Boolean; AInplaceEditing: Boolean); override;
    procedure InitScrollBarsParameters; override;
    function IsWeekStartOnMonday: Boolean;
    function IsWholeWeekShown: Boolean;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;

    property Controller: TcxSchedulerCustomWeekViewController read GetController;
    property CompressWeekEnd: Boolean read FCompressWeekEnd write SetCompressWeekEnd default True;
    property DaysLayout: TcxSchedulerWeekViewDaysLayout read FDaysLayout write SetDaysLayout default wdlTwoColumns;
    property HideWeekEnd: Boolean read GetHideWeekEnd write SetHideWeekEnd default False;
    property WeekCount: Integer read GetWeekCount;
    property WeekDayCount: Integer read GetWeekDayCount;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property FirstVisibleDate;
    property LastVisibleDate;
    property StartOfWeek;
  end;

  { TcxSchedulerCustomWeekViewNavigation }

  TcxSchedulerCustomWeekViewNavigation = class(TcxSchedulerCustomResourceViewNavigation)
  private
    function GetCompress: Boolean;
    function GetFirstVisibleDate: TDateTime;
    function GetLastVisibleDate: TDateTime;
    function GetView: TcxSchedulerCustomWeekView;
    function GetWeeks: TcxSchedulerDateList;
    function IsWholeWeekAnyway: Boolean;
    function VisibleDaysRangeFinish: TDateTime;
    function VisibleDaysRangeStart: TDateTime;
  protected
    FYear: Word;
    FWeekOfYear: Word;
    FWeekIndex: Integer;
    FDayOfWeek: Word;
    IsFirstWeek, IsLastWeek: Boolean;
    function DateScrollBarKind: TScrollBarKind;
    function GetWeekFinish(AWeekIndex: Integer): TDateTime;
    function GetWeekStart(AWeekIndex: Integer; ConsiderShowWeekEnd: Boolean): TDateTime;
    procedure GotoCornerCell(AGotoEnd: Boolean); virtual;
    procedure GotoNextCellHorz(AGotoNext: Boolean); virtual;
    procedure GotoNextCellVert(AGotoDown: Boolean); virtual;
    procedure GotoNextPage(AGotoForward: Boolean); virtual;
    function IsResourceNavigation(AGoToDown: Boolean): Boolean;
  public
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure ValidateSelection(var ASelStart, ASelFinish: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem); override;
    property Compress: Boolean read GetCompress;
    property FirstVisibleDate: TDateTime read GetFirstVisibleDate;
    property LastVisibleDate: TDateTime read GetLastVisibleDate;
    property View: TcxSchedulerCustomWeekView read GetView;
    property Weeks: TcxSchedulerDateList read GetWeeks;
  end;

  { TcxSchedulerWeekView }

  TcxSchedulerWeekView = class(TcxSchedulerCustomWeekView)
  private
    function GetViewInfo: TcxSchedulerWeekViewViewInfo;
  protected
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    function GetCompressWeekEnd: Boolean; override;
    procedure GetRangeControlTotalRange(out AMin, AMax: TDateTime); override;
    property ViewInfo: TcxSchedulerWeekViewViewInfo read GetViewInfo;
  public
    property SelectedDays;
    property WorkDays;
    property WorkFinish;
    property WorkStart;
  published
    property CanShow;
    property Active;
    property CompressWeekEnd;
    property DaysLayout;
    property GroupingKind;
    property HideWeekEnd;
    property ShowEndTime;
    property ShowTimeAsClock;
  end;

  { TcxSchedulerWeekViewNavigation }

  TcxSchedulerWeekViewNavigation = class(TcxSchedulerCustomWeekViewNavigation)
  private
    function GetViewInfo: TcxSchedulerWeekViewViewInfo;
  protected
    procedure GotoNextCellHorz(AGotoNext: Boolean); override;
    procedure GotoNextCellVert(AGotoDown: Boolean); override;
  public
    function ScrollResourcesEx(AGoForward: Boolean; var AResource: TcxSchedulerStorageResourceItem): Boolean; override;
    property ViewInfo: TcxSchedulerWeekViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerWeeksView }

  TcxSchedulerWeeksView = class(TcxSchedulerCustomWeekView)
  private
    function GetViewInfo: TcxSchedulerWeeksViewViewInfo;
  protected
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    function GetMaxScrollPos: Integer; override;
    function GetMinScrollPos: Integer; override;
    procedure GetRangeControlTotalRange(out AMin, AMax: TDateTime); override;

    property ViewInfo: TcxSchedulerWeeksViewViewInfo read GetViewInfo;
  public
    property SelectedDays;
    property WeekCount;
    property WorkDays;
    property WorkFinish;
    property WorkStart;
  published
    property CanShow;
    property Active;
    property CompressWeekEnd;
    property GroupingKind;
    property HideWeekEnd;
    property ShowEndTime;
    property ShowTimeAsClock;
  end;

  { TcxSchedulerWeeksViewNavigation }

  TcxSchedulerWeeksViewNavigation = class(TcxSchedulerCustomWeekViewNavigation)
  private
    function GetViewInfo: TcxSchedulerWeeksViewViewInfo;
    function IsFirstDayOfWeek(ADayIndex: Integer): Boolean;
    function IsLastDayOfWeek(ADayIndex: Integer): Boolean;
  protected
    procedure GotoNextCellHorz(AGotoNext: Boolean); override;
    procedure GotoNextCellVert(AGotoDown: Boolean); override;
    function IsScrollResource(AGotoNext: Boolean): Boolean;
  public
    property ViewInfo: TcxSchedulerWeeksViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerWeekViewCustomEventCellViewInfo }

  TcxSchedulerWeekViewEventCellCustomViewInfo = class(TcxSchedulerEventCellViewInfo)
  protected
    procedure CalculateBorders; override;
    procedure CalculateEventTimeVisibility; override;
    procedure CalculateItemsLayout; override;
    function GetEditingRect: TRect; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  end;

  { TcxSchedulerWeekViewEventCellViewInfo }

  TcxSchedulerWeekViewEventCellViewInfo = class(TcxSchedulerWeekViewEventCellCustomViewInfo)
  protected
    procedure CalculateCaptions; override;
    function GetCaptionFlags: Cardinal; override;
  end;

  { TcxSchedulerWeekViewEventCellModernViewInfo }

  TcxSchedulerWeekViewEventCellModernViewInfo = class(TcxSchedulerWeekViewEventCellCustomViewInfo)
  private
    FCaptionIsBold: Boolean;
    FRealBorderColor: TColor;
  protected
    procedure AfterDraw; override;
    procedure BeforeDraw; override;
    procedure CalculateBorderAttributes; override;
    procedure CalculateCaptions; override;
    procedure CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer); override;
    procedure CalculateItemsLayout; override;
    procedure CalculateLocationRect; override;
    function CanBeHeaderEventContinueArrows: Boolean; override;
    procedure DoDrawCaption; override;
    function EnableModernSelection: Boolean; override;
    function GetBoundsWithoutTimeLineRect: TRect; override;
    function GetCaptionFlags: Cardinal; override;
    function GetCaptionFontStyle: TFontStyles; override;
    function GetHeaderEventCaptionLeft(const ARect: TRect; AContentWidth: Integer): Integer; override;
    function GetHeaderEventStartClockOriginLeft: Integer; override;
    function GetNonDetailEventPossibleCaptionRect: TRect; override;
    function GetTimeLineRectRight: Integer; override;
    function GetViewStyle: TcxSchedulerViewStyle; override;
    function IsTimeLineVisible: Boolean; override;
    function NeedHeaderEventStartContinueArrow: Boolean; override;
    function NeedHeaderEventFinishContinueArrow: Boolean; override;
    function NeedLeftSizingHandler: Boolean; override;
    function NeedRightSizingHandler: Boolean; override;
  public
    constructor Create(AViewData: TcxSchedulerEventViewData; ACanResize: Boolean;
      AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean); override;
  end;

  { TcxSchedulerWeekViewViewInfo }

  TcxSchedulerWeekViewViewInfo = class(TcxSchedulerCustomResourceViewViewInfo)
  private
    function GetDayCell(ADay: Integer): TcxSchedulerContentCellViewInfo;
    function GetDayCellCount: Integer;
    function GetRotateCaptions: Boolean;
    function GetView: TcxSchedulerWeekView;
    function GetWeeks: TcxSchedulerDateList;
  protected
    Builder: TcxSchedulerEventLayoutBuilder;
    FDayHeaderOffsetTop: Integer;
    FHeaderOffsetLeft: Integer;
    FPrintedMonth: Word;
    FPrintExactlyOneMonth: Boolean;
    FResourceHeaderWidth: Integer;
    FRowCols: Integer;
    FRowColsStart: Integer;
    FRowColsStartDateOffset: Integer;
    FWeekEndCells: Integer;
    function AddContentCell(const ARect: TRect; const AStart, AFinish: TDateTime;
      AResourceIndex: Integer): TcxSchedulerContentCellViewInfo; override;
    procedure AddDayCellEvent(APlace: TcxSchedulerEventPlace);
    function AddDayCellEventPlace(AEvent: TcxSchedulerControlEvent;
      var AIndex: Integer; AStartDay: TcxSchedulerContentCellViewInfo): Boolean;
    function AddWeekDayHeader(AWeekDay: Integer; ALeft, ARight: Integer): TcxSchedulerWeekDayHeaderCellViewInfo;
    function WeekDayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
    procedure CalculateDaysLayout; virtual;
    procedure CalculateDaysLayoutForResource(const ABounds: TRect; AResourceIndex: Integer); virtual;
    procedure CalculateEventsOrder;
    procedure CalculateEventsViewInfo; virtual;
    procedure CalculateGroupByDateForResource(const ABounds: TRect; AResourceIndex: Integer); virtual;
    procedure CalculateHeaderViewInfo; virtual;
    procedure CalculateMetrics; override;
    procedure CalculatePrintRowCols; virtual;
    procedure CalculateResourceHeadersAutoHeight(AWidth: Integer); override;
    procedure CalculateWeekDates;
    procedure CalculateWeekDisplayRectsHorz(const AStartDate: TDateTime; AResourceID: Integer;
      {const }ABounds: TRect; AIsTopWeek, AIsLastWeek: Boolean); virtual;
    procedure CalculateWeekDisplayRectsVert(const AStartDate: TDateTime; AResourceID: Integer;
      const ABounds: TRect; AddBorders: TcxBorders = []); virtual;
    procedure CalculateWeekDisplayRectsVertEx(const AStartDate: TDateTime; AResourceID: Integer;
      const ABounds: TRect; AddBorders: TcxBorders = []);
    function CanEventVisible(AEvent: TcxSchedulerControlEvent): Boolean;
    function CanVisibleNextLine(ADay: TcxSchedulerContentCellViewInfo; ALine: Integer = -1): Boolean;
    procedure CheckDaysTimeEvent(AEventInfo: TcxSchedulerEventCellViewInfo; APlace: TcxSchedulerEventPlace);
    procedure CheckHeaderBorders; virtual;
    function ContentCellClass: TcxSchedulerContentCellViewInfoClass; override;
    procedure CorrectWeekDay(var AWeekDay: Integer);
    procedure DoCalculate; override;
    procedure DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); override;
    procedure DoMoreEventsButtonClick(Sender: TcxSchedulerMoreEventsButtonViewInfo); override;
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; override;
    function GetCompressWeekEnd: Boolean; virtual;
    procedure GetEventInfo(APlace: TcxSchedulerEventPlace; var ALeft, ARight: Integer; var AStart, AFinish: TDateTime;
      var AVisibleRect: TRect; var AResource: TcxSchedulerResourceViewInfo);
    function GetResourcesContentWidth: Integer; override;
    function GetResourceImagesSize: TSize; override;
    function GetResourceScrollBarKind: TScrollBarKind; override;
    function GetContentParams(const ATime: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxViewParams; override;
    function GetDayCellContentHeight(ADay: TcxSchedulerContentCellViewInfo): Integer; virtual;
    function GetDayCellHeaderHeight: Integer; virtual;
    function GetHideWeekEnd: Boolean; virtual;
    function GetStartDate(Index: Integer): TDateTime; override;
    function IsCalculateHeaderFirst: Boolean; virtual;
    function IsCellEvent(AEvent: TcxSchedulerControlEvent;
      ACell: TcxSchedulerContentCellViewInfo): Boolean;
    function IsSingleLine(ADay1, ADay2: TcxSchedulerContentCellViewInfo): Boolean;
    function IsTimeSelected(ATime: TDateTime; AResource: TObject): Boolean; override;
    function IsWeekStartOnMonday: Boolean; virtual;
    procedure ProcessGroupByDate; virtual;
    procedure ProcessGroupByNone; virtual;
    procedure ProcessGroupByResource; virtual;

    property CompressWeekEnd: Boolean read GetCompressWeekEnd;
    property DayCellCount: Integer read GetDayCellCount;
    property DayCells[ADay: Integer]: TcxSchedulerContentCellViewInfo read GetDayCell;
    property HideWeekEnd: Boolean read GetHideWeekEnd;
    property RowColsStart: Integer read FRowColsStart;
    property Weeks: TcxSchedulerDateList read GetWeeks;
    property WeekEndCells: Integer read FWeekEndCells;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure ScrollVertical(AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    property PrintExactlyOneMonth: Boolean read FPrintExactlyOneMonth;
    property PrintedMonth: Word read FPrintedMonth;
    property ResourceHeaderWidth: Integer read FResourceHeaderWidth;
    property RotateCaptions: Boolean read GetRotateCaptions;
    property StartDates;
    property View: TcxSchedulerWeekView read GetView;
  end;

  { TcxSchedulerWeeksViewEventCellViewInfo }

  TcxSchedulerWeeksViewEventCellViewInfo = class(TcxSchedulerWeekViewEventCellViewInfo)
  protected
    function CanAutoHideStandardImages: Boolean; override;
  end;

  { TcxSchedulerWeeksViewEventCellModernViewInfo }

  TcxSchedulerWeeksViewEventCellModernViewInfo = class(TcxSchedulerWeekViewEventCellModernViewInfo)
  protected
    function CanAutoHideStandardImages: Boolean; override;
  end;

  { TcxSchedulerWeeksViewViewInfo }

  TcxSchedulerWeeksViewViewInfo = class(TcxSchedulerWeekViewViewInfo)
  private
    function GetView: TcxSchedulerWeeksView;
  protected
    FContentOffset: Integer;
    procedure CalculateDaysLayoutForResource(const ABounds: TRect; AResourceIndex: Integer); override;
    procedure CalculateGroupByDateForResource(const ABounds: TRect; AResourceIndex: Integer); override;
    procedure CalculateHeaderViewInfo; override;
    procedure CalculateMetrics; override;
    procedure CheckHeaderBorders; override;
    function ContentCellClass: TcxSchedulerContentCellViewInfoClass; override;
    function DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass; override;
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; override;
    function GetCompressWeekEnd: Boolean; override;
    function GetContentParams(const ATime: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxViewParams; override;
    function GetDayCellHeaderHeight: Integer; override;
    function IsCalculateHeaderFirst: Boolean; override;
    function IsWeekStartOnMonday: Boolean; override;
    procedure ProcessGroupByNone; override;
    procedure ProcessGroupByResource; override;
  public
    property View: TcxSchedulerWeeksView read GetView;
  end;

  { TcxSchedulerWeekViewPainter }

  TcxSchedulerWeekViewPainter = class(TcxSchedulerCustomViewPainter)
  private
    function GetViewInfo: TcxSchedulerWeekViewViewInfo;
  public
    procedure Paint; override;

    property ViewInfo: TcxSchedulerWeekViewViewInfo read GetViewInfo;
  end;

implementation

uses
  cxSchedulerDateNavigator;

type
  TcxSchedulerDaysRect = array[0..6] of TRect;
  TcxSchedulerDaysBorders = array[0..6] of TcxBorders;

  TcxSchedulerContentCellViewInfoAccess = class(TcxSchedulerContentCellViewInfo);
  TcxSchedulerExternalPainterAccess = class(TcxSchedulerExternalPainter);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);

const
  WeekViewDayBorders: TcxSchedulerDaysBorders =(
    [bTop, bRight], [bTop, bRight], [bTop, bRight],
    [bTop], [bTop], [bTop], [bTop]);
  DayInc: array[Boolean] of Integer = (-1, 1);
  WholeWeekDayCount = 7;

function IsWorkDay(AWeekDay: Integer; AScheduler: TcxCustomScheduler): Boolean;
begin
  Result := (TDay(AWeekDay) in AScheduler.OptionsView.WorkDays);
end;

{ TcxSchedulerCustomWeekViewController }

function TcxSchedulerCustomWeekViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxCustomWeekViewDragEventHelper.Create(Scheduler);
end;

function TcxSchedulerCustomWeekViewController.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerCustomWeekView(View).CreateNavigation;
end;

function TcxSchedulerCustomWeekViewController.CreateResizeEventHelper: TcxEventSizingHelper;
begin
  Result := TcxCustomWeekViewEventSizing.Create(Scheduler);
end;

function TcxSchedulerCustomWeekViewController.GetDayHeaderDate: TDateTime;
begin
  Result := inherited GetDayHeaderDate;
end;

function TcxSchedulerCustomWeekViewController.NeedShowDayHeaderHint: Boolean;
var
  R: TRect;
  P: TPoint;
begin
  R := cxNullRect;
  if (HitTest.ContentCell <> nil) then
    with HitTest.ContentCell.Bounds do
      R := Rect(Left, Top, Right, Top + View.ViewInfo.DayHeaderHeight);
  P := Point(HitTest.HitX, HitTest.HitY);
  Result := Scheduler.OptionsView.ShowHints and(((HitTest.ContentCell <> nil) and
    (TcxSchedulerContentCellViewInfoAccess(HitTest.ContentCell).FDisplayText <> '') and
    (HitTest.HitAtTime) and (PtInRect(R, P))) or
    (HitTest.HitAtDayHeader and HitTest.HitAtTime));
end;

function TcxSchedulerCustomWeekViewController.GetNavigation: TcxSchedulerCustomWeekViewNavigation;
begin
  Result := TcxSchedulerCustomWeekViewNavigation(inherited Navigation);
end;

function TcxSchedulerCustomWeekViewController.GetView: TcxSchedulerCustomWeekView;
begin
  Result := TcxSchedulerCustomWeekView(inherited View);
end;

{cxCustomWeekViewDragEventHelper }

procedure TcxCustomWeekViewDragEventHelper.UpdateViewClonesTime;
var
  I: Integer;
  ADelta: TDateTime;
begin
  ADelta := Controller.StartDragHitTime - HitTest.Time;
  for I := 0 to Clones.Count - 1 do
    with Clones[I] do
      MoveTo(Source.Start - ADelta);
end;

{ TcxCustomWeekViewEventSizing }

procedure TcxCustomWeekViewEventSizing.UpdateEventBounds;
begin
  if Event.AllDayEvent then
    inherited UpdateEventBounds
  else
  begin
    if Controller.DragKind = edkResizeStart then
    begin
      Event.AllDayEvent := dxTimeOf(Event.Finish) = 0;
      Event.Start := HitTest.Time;
    end
    else
    begin
      Event.AllDayEvent := dxTimeOf(Event.Start) = 0;
      if Event.AllDayEvent then
        Event.Finish := HitTest.Time
      else
        Event.Finish := HitTest.Time + 1;
    end;
    CheckEventState(Event);
    RefreshCurrentView;
  end;
end;

{ TcxSchedulerCustomWeekView }

constructor TcxSchedulerCustomWeekView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FCompressWeekEnd := True;
  EventImagesLayout := eilHorizontal;
  FWeeks := TcxSchedulerDateList.Create;
end;

destructor TcxSchedulerCustomWeekView.Destroy;
begin
  FWeeks.Free;
  inherited Destroy;
end;

procedure TcxSchedulerCustomWeekView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerCustomWeekView then
  begin
    CompressWeekEnd := TcxSchedulerCustomWeekView(Source).FCompressWeekEnd;
    HideWeekEnd := TcxSchedulerCustomWeekView(Source).FHideWeekEnd;
    FDaysLayout := TcxSchedulerCustomWeekView(Source).DaysLayout;
  end;
  inherited Assign(Source);
end;

function TcxSchedulerCustomWeekView.CanSelectIndividualDays: Boolean;
begin
  Result := False;
end;

function TcxSchedulerCustomWeekView.CheckDate(
  const ADate: TDateTime): Integer;
begin
  //DELPHI8! check Trunc(ADate)
  Result := Trunc(ADate);
  if DayOfWeek(ADate) <> (1 + Byte(CompressWeekEnd)) then
    Result := Result - DayOfWeek(ADate) + 1 + Byte(CompressWeekEnd);
end;

procedure TcxSchedulerCustomWeekView.CheckWorkDays;
var
  I: Integer;
begin
  FForceShowWeekEnd := True;
  I := 0;
  while (I < 7) and FForceShowWeekEnd do
  begin
    FForceShowWeekEnd := not IsWorkDay(I, Scheduler);
    Inc(I);
  end;
end;

function TcxSchedulerCustomWeekView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerCustomWeekViewController.Create(Self);
end;

function TcxSchedulerCustomWeekView.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerCustomWeekViewNavigation.Create(Self);
end;

function TcxSchedulerCustomWeekView.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerWeekViewPainter.Create(Self);
end;

function TcxSchedulerCustomWeekView.GetCompressWeekEnd: Boolean;
begin
  Result := CompressWeekEnd;
end;

function TcxSchedulerCustomWeekView.GetDateScrollBarKind: TScrollBarKind;
const
  ScrollKind: array[TScrollBarKind] of TScrollBarKind =
    (sbVertical, sbHorizontal);
begin
  Result := ScrollKind[TcxSchedulerWeekViewViewInfo(ViewInfo).GetResourceScrollBarKind];
end;

function TcxSchedulerCustomWeekView.GetFirstVisibleDate: TDateTime;
begin
  Result := inherited GetFirstVisibleDate;
  if DayOfWeek(Result) <> 1 then
    Result := StartOfTheWeek(Result);
  if IsWeekStartOnMonday then
    Result := Result + GetDateOffset;
end;

function TcxSchedulerCustomWeekView.GetLastVisibleDate: TDateTime;
begin
  if SelectedDays.Count = 1 then
    Result := GetFirstVisibleDate + 7
  else
  begin
    Result := inherited GetLastVisibleDate;
    if IsWeekStartOnMonday then
      Result := Result + GetDateOffset;
  end;
end;

function TcxSchedulerCustomWeekView.GetMaxScrollPos: Integer;
begin
  Result := 25;
end;

function TcxSchedulerCustomWeekView.GetMinScrollPos: Integer;
begin
  Result := -25;
end;

function TcxSchedulerCustomWeekView.GetScrollTimeHint: string;
begin
  Result := cxDateToStr(GetFirstVisibleDate) + ' - ' + cxDateToStr(GetLastVisibleDate);
end;

function TcxSchedulerCustomWeekView.GetWeekCount: Integer;
begin
  Result := Max(1, SelectedDays.Count div 7);
end;

function TcxSchedulerCustomWeekView.GetStartOfWeek: TDay;
begin
  if IsWeekStartOnMonday then
    Result := dMonday
  else
    Result := inherited GetStartOfWeek;
end;

function TcxSchedulerCustomWeekView.GetViewContentRect: TRect;
begin
  Result := inherited GetViewContentRect;
  if ViewInfo.GroupingKind = gkByDate then
    with TcxSchedulerWeekViewViewInfo(ViewInfo) do
      Result.TopLeft := Point(FResourceHeaderHeight, FDayHeaderHeight);
end;

procedure TcxSchedulerCustomWeekView.InitEventBySelectedTime(
  AEvent: TcxSchedulerEvent; AllDay: Boolean; ARecurrence: Boolean;
  AInplaceEditing: Boolean);
var
  AStart, AFinish: TDateTime;
begin
  AStart := Scheduler.SelStart;
  AFinish := Scheduler.SelFinish;
  AEvent.AllDayEvent := AllDay or (AFinish - AStart > 1) or (AInplaceEditing and not ARecurrence);
  if not AEvent.AllDayEvent then
  begin
    AStart := AStart + cxTime8AM;
    AFinish := (AFinish - 1) + cxTime8AM + cxHalfHour;
  end;
  AEvent.Start := AStart;
  AEvent.Finish := AFinish;
end;

procedure TcxSchedulerCustomWeekView.InitScrollBarsParameters;
begin
  SetScrollBarInfo(GetDateScrollBarKind,  GetMinScrollPos, GetMaxScrollPos, 1,
    WeekCount, GetScrollPos(FirstVisibleDate), True, True);
  inherited InitScrollBarsParameters;
end;

function TcxSchedulerCustomWeekView.IsWeekStartOnMonday: Boolean;
begin
  Result := TcxSchedulerWeekViewViewInfo(ViewInfo).IsWeekStartOnMonday;
end;

function TcxSchedulerCustomWeekView.IsWholeWeekShown: Boolean;
begin
  Result := not HideWeekEnd or (Self is TcxSchedulerWeekView) and
    not ViewInfo.GroupByDate and (DaysLayout = wdlTwoColumns);
end;

procedure TcxSchedulerCustomWeekView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  ANewSelDate: TDateTime;
  ADelta: Integer;
begin
  TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
  HideHintOnScroll(AScrollCode);
  if (AScrollBarKind <> GetDateScrollBarKind) or (AScrollCode = scEndScroll) then Exit;
  ADelta := GetScrollPos(FirstVisibleDate, False);
  if not (AScrollCode in [scPosition, scTrack]) then
    AScrollPos := ADelta;
  ANewSelDate := Controller.Navigation.SelStart;
  case AScrollCode of
    scLineUp, scLineDown:
    begin
      Inc(AScrollPos, DayInc[AScrollCode <> scLineUp]);
      ANewSelDate := ANewSelDate + DayInc[AScrollCode <> scLineUp] * 7;
    end;
    scPageUp, scPageDown:
    begin
      Inc(AScrollPos, DayInc[AScrollCode <> scPageUp] * WeekCount);
      with Controller.Navigation do
        ANewSelDate := ANewSelDate + DayInc[AScrollCode <> scPageUp] * Weeks.Count * 7;
    end;
    scTop:
      AScrollPos := -GetMinScrollPos;
    scBottom:
      AScrollPos := GetMaxScrollPos;
  end;
  ADelta := ADelta - AScrollPos;
  if ADelta <> 0 then
  begin
    ScrollSelectedDays(ADelta);
    AScrollPos := GetScrollPos(FirstVisibleDate);
  end;
  if AScrollCode = scTrack then
    ShowHintOnScroll(GetScrollTimeHint, AScrollBarKind);
  with Scheduler do
    SelectTime(ANewSelDate, ANewSelDate, SelResource);
end;

function TcxSchedulerCustomWeekView.GetController: TcxSchedulerCustomWeekViewController;
begin
  Result := TcxSchedulerCustomWeekViewController(inherited Controller);
end;

function TcxSchedulerCustomWeekView.GetHideWeekEnd: Boolean;
begin
  Result := (FHideWeekEnd and not FForceShowWeekEnd) or ViewInfo.Adapter.DontPrintWeekEnd;
end;

procedure TcxSchedulerCustomWeekView.GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList);
begin
  AValues.Add(AScales.Month);
  AValues.Add(AScales.Week);
end;

function TcxSchedulerCustomWeekView.GetScrollPos(
  const ADatePos: TDateTime; ACheckBounds: Boolean = True): Integer;
begin
  Result := Round(ADatePos - CheckDate(Date)) div 7;
  if ACheckBounds then
  begin
    if Result < GetMinScrollPos then
      Result := GetMinScrollPos;
    if Result > GetMaxScrollPos then
      Result := GetMaxScrollPos;
  end;
end;

function TcxSchedulerCustomWeekView.GetWeekDayCount: Integer;
begin
  if IsWholeWeekShown then
    Result := WholeWeekDayCount
  else
    Result := GetWorkWeekDayCount;
end;

procedure TcxSchedulerCustomWeekView.SetCompressWeekEnd(
  const AValue: Boolean);
begin
  if FCompressWeekEnd <> AValue then
  begin
    FCompressWeekEnd := AValue;
    LayoutChanged;
  end;
end;

procedure TcxSchedulerCustomWeekView.SetDaysLayout(
  Value: TcxSchedulerWeekViewDaysLayout);
begin
  if Value <> FDaysLayout then
  begin
    FDaysLayout := Value;
    LayoutChanged;
  end;
end;

procedure TcxSchedulerCustomWeekView.SetHideWeekEnd(Value: Boolean);
begin
  if FHideWeekEnd <> Value then
  begin
    FHideWeekEnd := Value;
    LayoutChanged;
  end;
end;

function TcxSchedulerCustomWeekView.GetWorkWeekDayCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to 6 do
    if IsWorkDay(I, Scheduler) then
      Inc(Result);
end;

{ TcxSchedulerCustomWeekViewNavigation }

procedure TcxSchedulerCustomWeekViewNavigation.KeyDown(
  var AKey: Word; AShift: TShiftState);
const
  ANextCellKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
var
  APos: Integer;
begin
  DecodeDateWeek(FCurrentAnchor + 1 - Byte(View.StartOfWeek),
    FYear, FWeekOfYear, FDayOfWeek);
  Dec(FDayOfWeek);
  FWeekIndex := Max(0, Weeks.IndexOf(Trunc(EncodeDateWeek(FYear, FWeekOfYear, 1))));
  IsFirstWeek := (FWeekIndex = 0) and IsFirstResource;
  IsLastWeek := (FWeekIndex = Weeks.Count - 1) and IsLastResource;
  case AKey of
    VK_Left, VK_Right:
      GotoNextCellHorz(AKey = ANextCellKey[View.UseRightToLeftAlignment]);
    VK_UP, VK_DOWN:
      GotoNextCellVert(AKey = VK_DOWN);
    VK_HOME, VK_END:
      GotoCornerCell(AKey = VK_END);
    VK_NEXT, VK_PRIOR:
      GotoNextPage(AKey = VK_NEXT);
  end;
  APos := View.GetScrollPos(View.FirstVisibleDate, False);
  if (ssShift in FShift) and ((FCurrentResource <> SelResource) or
    (dxDateOf(FCurrentAnchor) < FirstVisibleDate) or (dxDateOf(FCurrentAnchor) > LastVisibleDate)) then
    Exit
  else
    if dxDateOf(FCurrentAnchor) < FirstVisibleDate then
    begin
      View.Scroll(DateScrollBarKind, scLineUp, APos);
      SetSelAnchor(FCurrentAnchor, [], FCurrentResource);
    end
    else
      if dxDateOf(FCurrentAnchor) > LastVisibleDate then
      begin
        View.Scroll(DateScrollBarKind, scLineDown, APos);
        SetSelAnchor(FCurrentAnchor, [], FCurrentResource);
      end
      else
        SetSelAnchor(FCurrentAnchor, FShift, FCurrentResource);
end;

procedure TcxSchedulerCustomWeekViewNavigation.ValidateSelection(
  var ASelStart, ASelFinish: TDateTime;
  var AResource: TcxSchedulerStorageResourceItem);
begin
  inherited ValidateSelection(ASelStart, ASelFinish, AResource);
  if View.SelectedDays.Count = 0 then
    ASelFinish := Date
  else
    if (dxDateOf(ASelFinish) < FirstVisibleDate) or (dxDateOf(ASelStart) > LastVisibleDate) then
    begin
      ASelFinish := FirstVisibleDate;
      ASelStart := ASelFinish;
    end;
end;

function TcxSchedulerCustomWeekViewNavigation.DateScrollBarKind: TScrollBarKind;
begin
  Result := View.GetDateScrollBarKind;
end;

function TcxSchedulerCustomWeekViewNavigation.GetWeekFinish(AWeekIndex: Integer): TDateTime;
var
  I, K: Integer;
begin
  Result := 0;
  if (AWeekIndex < 0) or (AWeekIndex >= Weeks.Count) then Exit;
  Result := Weeks[AWeekIndex];
  if View.HideWeekEnd and not IsWholeWeekAnyway then
  begin
    I := Integer(View.StartOfWeek) + WholeWeekDayCount - 1;
    TcxSchedulerWeekViewViewInfo(ViewInfo).CorrectWeekDay(I);
    K := 0;
    while (K < WholeWeekDayCount) and not IsWorkDay(I, Scheduler) do
    begin
      Dec(I);
      TcxSchedulerWeekViewViewInfo(ViewInfo).CorrectWeekDay(I);
      Inc(K);
    end;
    if K < WholeWeekDayCount then
      Result := Result + WholeWeekDayCount - 1 - K;
  end
  else
    Result := Result + WholeWeekDayCount - 1;
  if not View.IsWeekStartOnMonday then
    Result := Result - View.GetDateOffset;
end;

function TcxSchedulerCustomWeekViewNavigation.GetWeekStart(AWeekIndex: Integer;
  ConsiderShowWeekEnd: Boolean): TDateTime;
var
  I, K: Integer;
begin
  Result := 0;
  if (AWeekIndex < 0) or (AWeekIndex >= Weeks.Count) then Exit;
  Result := Weeks[AWeekIndex];
  if ConsiderShowWeekEnd and View.HideWeekEnd and not IsWholeWeekAnyway then
  begin
    I := Integer(View.StartOfWeek);
    K := 0;
    while (K < WholeWeekDayCount) and not IsWorkDay(I, Scheduler) do
    begin
      Inc(I);
      TcxSchedulerWeekViewViewInfo(ViewInfo).CorrectWeekDay(I);
      Inc(K);
    end;
    if K < WholeWeekDayCount then
      Result := Result + K;
  end;
  if not View.IsWeekStartOnMonday then
    Result := Result - View.GetDateOffset;
end;

procedure TcxSchedulerCustomWeekViewNavigation.GotoCornerCell(
  AGotoEnd: Boolean);
begin
  if ssCtrl in FShift then
  begin
    if AGotoEnd then
      FCurrentAnchor := VisibleDaysRangeFinish
    else
      FCurrentAnchor := VisibleDaysRangeStart;
  end
  else
  begin
    if AGotoEnd then
      FCurrentAnchor := GetWeekFinish(FWeekIndex)
    else
      FCurrentAnchor := GetWeekStart(FWeekIndex, True);
  end;
end;

procedure TcxSchedulerCustomWeekViewNavigation.GotoNextCellHorz(
  AGotoNext: Boolean);

  function GetStep(AGotoNext: Boolean): Integer;
  var
    ADelta, ADay: Integer;
  begin
    ADelta := DayInc[AGoToNext];
    Result := 0;
    repeat
      Inc(Result, ADelta);
      ADay := DayOfWeek(FCurrentAnchor + Result) - 1;
    until IsWorkDay(ADay, Scheduler);
  end;

begin
  if View.HideWeekEnd and
    (ViewInfo.GroupByDate or (View.DaysLayout <> wdlTwoColumns) or
    (ViewInfo.ClassType = TcxSchedulerWeeksViewViewInfo)) then
    FCurrentAnchor := FCurrentAnchor + GetStep(AGotoNext)
  else
    FCurrentAnchor := FCurrentAnchor + DayInc[AGoToNext];
end;

procedure TcxSchedulerCustomWeekViewNavigation.GotoNextCellVert(
  AGotoDown: Boolean);
begin
  Inc(FWeekIndex, DayInc[AGotoDown]);
  if (FWeekIndex >= 0) and (FWeekIndex < Weeks.Count) then
    FCurrentAnchor := GetWeekStart(FWeekIndex, False)
  else
    if AGotoDown then
    begin
      if IsLastWeek then
      begin
        if not IsResourceNavigation(AGoToDown) then
          FCurrentAnchor := LastVisibleDate + 1
        else
          Exit;
      end
      else
        if FWeekIndex = Weeks.Count then
        begin
          FCurrentAnchor := FirstVisibleDate;
          FCurrentResource := GetNextResource(True)
        end
    end
    else
    begin
      if IsFirstWeek then
      begin
        if not IsResourceNavigation(AGoToDown) then
          FCurrentAnchor := FirstVisibleDate - 7
        else
          Exit;
      end
      else
        if FWeekIndex <= 0 then
        begin
          FCurrentAnchor := LastVisibleDate - 7 + 1;
          FCurrentResource := GetNextResource(False);
        end
    end;
  FCurrentAnchor := FCurrentAnchor + FDayOfWeek;
end;

procedure TcxSchedulerCustomWeekViewNavigation.GotoNextPage(
  AGotoForward: Boolean);
begin
  if not (ssShift in FShift) then
  begin
    FCurrentAnchor := FCurrentAnchor + DayInc[AGotoForward] * Weeks.Count * 7;
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
    View.ScrollSelectedDays(DayInc[not AGotoForward] * Weeks.Count);
  end
  else
  begin
    if AGotoForward then
      FCurrentAnchor := VisibleDaysRangeFinish
    else
      FCurrentAnchor := VisibleDaysRangeStart;
  end;
end;

function TcxSchedulerCustomWeekViewNavigation.IsResourceNavigation(AGoToDown: Boolean): Boolean;
begin
  Result := (ViewInfo.GroupingKind <> gkNone);
  if Result then
    Result := ScrollResourcesEx(AGoToDown, FCurrentResource);
  if Result then
    FCurrentAnchor := FCurrentAnchor - DayInc[AGoToDown] * (View.WeekCount - 1) * 7;
end;

function TcxSchedulerCustomWeekViewNavigation.GetCompress: Boolean;
begin
  Result := View.GetCompressWeekEnd;
end;

function TcxSchedulerCustomWeekViewNavigation.GetFirstVisibleDate: TDateTime;
begin
  Result := View.FirstVisibleDate;
end;

function TcxSchedulerCustomWeekViewNavigation.GetLastVisibleDate: TDateTime;
begin
  Result := View.LastVisibleDate;
end;

function TcxSchedulerCustomWeekViewNavigation.GetView: TcxSchedulerCustomWeekView;
begin
  Result := TcxSchedulerCustomWeekView(inherited View);
end;

function TcxSchedulerCustomWeekViewNavigation.GetWeeks: TcxSchedulerDateList;
begin
  Result := View.FWeeks;
end;

function TcxSchedulerCustomWeekViewNavigation.IsWholeWeekAnyway: Boolean;
begin
  Result := (View is TcxSchedulerWeekView) and
    not ViewInfo.GroupByDate and (View.DaysLayout = wdlTwoColumns);
end;

function TcxSchedulerCustomWeekViewNavigation.VisibleDaysRangeFinish: TDateTime;
begin
  if not View.HideWeekEnd then
    Result := LastVisibleDate
  else
    Result := GetWeekFinish(Weeks.Count - 1);
end;

function TcxSchedulerCustomWeekViewNavigation.VisibleDaysRangeStart: TDateTime;
begin
  if not View.HideWeekEnd then
    Result := FirstVisibleDate
  else
    Result := GetWeekStart(0, True);
end;

{ TcxSchedulerWeekView }

function TcxSchedulerWeekView.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerWeekViewNavigation.Create(Self);
end;

function TcxSchedulerWeekView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerWeekViewViewInfo.Create(Self);
end;

function TcxSchedulerWeekView.GetCompressWeekEnd: Boolean;
begin
  Result := ViewInfo.CompressWeekEnd;
end;

procedure TcxSchedulerWeekView.GetRangeControlTotalRange(out AMin, AMax: TDateTime);
var
  AStartOfTheWeek: TDateTime;
begin
  AStartOfTheWeek := StartOfTheWeek(FirstVisibleDate);
  AMin := IncWeek(AStartOfTheWeek, -8);
  AMax := IncWeek(AStartOfTheWeek, 9);
end;

function TcxSchedulerWeekView.GetViewInfo: TcxSchedulerWeekViewViewInfo;
begin
  Result := TcxSchedulerWeekViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerWeekViewNavigation }

function TcxSchedulerWeekViewNavigation.ScrollResourcesEx(AGoForward: Boolean;
  var AResource: TcxSchedulerStorageResourceItem): Boolean;
begin
  if not (ViewInfo.GroupByDate or ViewInfo.GroupByResource) then
    Result := False
  else
    Result := inherited ScrollResourcesEx(AGoForward, AResource);
end;

procedure TcxSchedulerWeekViewNavigation.GotoNextCellHorz(AGotoNext: Boolean);
const
  DateInc: array[Boolean, 0..6] of Integer =
    ((-4, -4, -4, -3, -3, -3, -4), (3, 3, 3, 4, 4, 4, 3));
begin
  if (View.DaysLayout = wdlOneColumn) and (GroupingKind <> gkByDate) then
  begin
    if IsFirstWeek and not AGotoNext then
    begin
      if not ScrollResourcesEx(AGotoNext, FCurrentResource) then
        FCurrentAnchor := FCurrentAnchor - 7
    end
    else
      if IsLastWeek and AGotoNext then
      begin
        if not ScrollResourcesEx(AGotoNext, FCurrentResource) then
          FCurrentAnchor := FCurrentAnchor + 7;
      end
      else
        FCurrentResource := GetNextResource(AGotoNext);
  end
  else
    if GroupingKind <> gkByDate then
    begin
      FCurrentAnchor := FCurrentAnchor + DateInc[AGotoNext, FDayOfWeek];
      if (AGotoNext and not IsLastWeek and (FDayOfWeek in [3..6])) or
        (not AGotoNext and not IsFirstWeek and (FDayOfWeek in [0..2])) then
      begin
        FCurrentAnchor := FCurrentAnchor - DayInc[AGotoNext] * 7;
        FCurrentResource := GetNextResource(AGotoNext);
      end
      else
      begin
        if (IsLastWeek and AGotoNext and (FDayOfWeek in [3..6]))
          or (IsFirstWeek and not AGotoNext and (FDayOfWeek in [0..2])) then
        begin
          if IsResourceNavigation(AGotoNext) then
            FCurrentAnchor := FCurrentAnchor - DayInc[AGoToNext] * 7;
        end;
      end;
    end
    else
      inherited GoToNextCellHorz(AGotoNext);
end;

procedure TcxSchedulerWeekViewNavigation.GotoNextCellVert(AGotoDown: Boolean);
begin
  if GroupingKind <> gkByDate then
    inherited GotoNextCellHorz(AGotoDown)
  else
    inherited GotoNextCellVert(AGotoDown)
end;

function TcxSchedulerWeekViewNavigation.GetViewInfo: TcxSchedulerWeekViewViewInfo;
begin
  Result := TcxSchedulerWeekViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerWeeksView }

function TcxSchedulerWeeksView.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerWeeksViewNavigation.Create(Self);
end;

function TcxSchedulerWeeksView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerWeeksViewViewInfo.Create(Self);
end;

function TcxSchedulerWeeksView.GetMaxScrollPos: Integer;
begin
  Result := 51;
end;

function TcxSchedulerWeeksView.GetMinScrollPos: Integer;
begin
  Result := -51;
end;

procedure TcxSchedulerWeeksView.GetRangeControlTotalRange(out AMin, AMax: TDateTime);
var
  AStartOfTheMonth: TDateTime;
begin
  AStartOfTheMonth := StartOfTheMonth(FirstVisibleDate);
  AMin := IncMonth(AStartOfTheMonth, -5);
  AMax := IncMonth(AStartOfTheMonth, 6);
end;

function TcxSchedulerWeeksView.GetViewInfo: TcxSchedulerWeeksViewViewInfo;
begin
  Result := TcxSchedulerWeeksViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerWeeksViewNavigation }

procedure TcxSchedulerWeeksViewNavigation.GotoNextCellHorz(AGotoNext: Boolean);
begin
  if ViewInfo.GroupByResource then
  begin
    if IsScrollResource(AGoToNext) then Exit;
    if (AGoToNext and IsLastDayOfWeek(FDayOfWeek) and not IsLastResource) or
       (not AGoToNext and IsFirstDayOfWeek(FDayOfWeek) and not IsFirstResource) then
    begin
      if not AGoToNext then
        FCurrentAnchor := GetWeekFinish(FWeekIndex)
      else
        FCurrentAnchor := GetWeekStart(FWeekIndex, True);
      FCurrentResource := GetNextResource(AGoToNext);
    end
    else
      inherited GotoNextCellHorz(AGotoNext);
  end
  else
    inherited GotoNextCellHorz(AGotoNext);
end;

procedure TcxSchedulerWeeksViewNavigation.GotoNextCellVert(AGotoDown: Boolean);
begin
  if ViewInfo.GroupByResource then
  begin
    if not AGotoDown and (FWeekIndex <= 0) then
      FCurrentAnchor :=  FCurrentAnchor - 7
    else
      if AGotoDown and (FWeekIndex >= Weeks.Count - 1) then
        FCurrentAnchor :=  FCurrentAnchor + 7
      else
        inherited GotoNextCellVert(AGoToDown);
  end
  else
    inherited GotoNextCellVert(AGoToDown);
end;

function TcxSchedulerWeeksViewNavigation.IsScrollResource(AGotoNext: Boolean): Boolean;
begin
  Result := (AGoToNext and IsLastDayOfWeek(FDayOfWeek) and IsLastResource) or
    (not AGoToNext and IsFirstDayOfWeek(FDayOfWeek) and IsFirstResource);
  if Result then
    Result := ScrollResourcesEx(AGotoNext, FCurrentResource);
  if Result then
  begin
    if not AGoToNext then
      FCurrentAnchor := GetWeekFinish(FWeekIndex)
    else
      FCurrentAnchor := GetWeekStart(FWeekIndex, True);
  end;
end;

function TcxSchedulerWeeksViewNavigation.GetViewInfo: TcxSchedulerWeeksViewViewInfo;
begin
  Result := TcxSchedulerWeeksViewViewInfo(inherited ViewInfo);
end;

function TcxSchedulerWeeksViewNavigation.IsFirstDayOfWeek(ADayIndex: Integer): Boolean;
var
  ADIndex: Integer;
begin
  if not View.HideWeekEnd then
    Result := ADayIndex = 0
  else
  begin
    Result := True;
    Dec(ADayIndex);
    while Result and (ADayIndex >= 0) do
    begin
      ADIndex := ADayIndex + Ord(Scheduler.OptionsView.ActualStartOfWeek);
      ViewInfo.CorrectWeekDay(ADIndex);
      Result := not IsWorkDay(ADIndex, Scheduler);
      Dec(ADayIndex);
    end;
  end;
end;

function TcxSchedulerWeeksViewNavigation.IsLastDayOfWeek(ADayIndex: Integer): Boolean;
var
  ADIndex: Integer;
begin
  if not View.HideWeekEnd then
    Result := ADayIndex = WholeWeekDayCount - 1
  else
  begin
    Result := True;
    Inc(ADayIndex);
    while Result and (ADayIndex < WholeWeekDayCount) do
    begin
      ADIndex := ADayIndex + Ord(Scheduler.OptionsView.ActualStartOfWeek);
      ViewInfo.CorrectWeekDay(ADIndex);
      Result := not IsWorkDay(ADIndex, Scheduler);
      Inc(ADayIndex);
    end;
  end;
end;

{ TcxSchedulerWeekViewEventCellCustomViewInfo }

procedure TcxSchedulerWeekViewEventCellCustomViewInfo.CalculateEventTimeVisibility;
begin
  CalculateNonDetailEventTimeVisibility;
  if not IsHeaderEvent then
    ViewData.ShowStartTime := True
  else
    if ViewData.ShowTimeAsClock and not Event.AllDayEvent then
    begin
      ViewData.ShowStartTime := ViewData.ShowStartTime and (dxTimeOf(Event.Start) <> 0);
      ViewData.ShowFinishTime := ViewData.ShowFinishTime and (dxTimeOf(Event.Finish) <> 0);
    end;
end;

procedure TcxSchedulerWeekViewEventCellCustomViewInfo.CalculateBorders;
begin
  if ViewData.ContentFinish < EventFinish then
    Exclude(FBorders, bRight);
  if ViewData.ContentStart > EventStart then
    Exclude(FBorders, bLeft);
end;

procedure TcxSchedulerWeekViewEventCellCustomViewInfo.CalculateItemsLayout;
begin
  if ViewData.ShowTimeLine then
    CalculateTimeLineLayout;
  if IsHeaderEvent then
    CalculateHeaderEventLayout
  else
    CalculateNonDetailEventLayout;
end;

function TcxSchedulerWeekViewEventCellCustomViewInfo.GetEditingRect: TRect;
begin
  Result := inherited GetEditingRect;
  with Result do
  begin
    if not IsHeaderEvent then
    begin
      if not cxRectIsEmpty(FFinishRect) then
        Left := FFinishRect.Right
      else
        if not cxRectIsEmpty(FStartRect) then
          Left := FStartRect.Right
        else
          Left := FBounds.Left;
      Inc(Left, ScaleFactor.Apply(cxTextOffset + cxEventImagesGap));
      Dec(Right);
      Inc(Top);
      Dec(Bottom);
    end;
  end;
end;

procedure TcxSchedulerWeekViewEventCellCustomViewInfo.InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  if IsHeaderEvent then
    InitHitTestForHorzEvent(AHitTest)
  else
    if (ShowStartTime and (AHitTest.HitX <= FStartRect.Right)) or
      (ShowFinishTime and (AHitTest.HitX <= FFinishRect.Right)) then
        AHitTest.SetDragKind(edkEventDragRect);
end;

{ TcxSchedulerWeekViewEventCellViewInfo }

procedure TcxSchedulerWeekViewEventCellViewInfo.CalculateCaptions;
begin
  if (dxDateOf(EventStart) <> dxDateOf(EventFinish)) and (Event.Duration < 1) then
  begin
    FStartText := DateTimeHelper.TimeToStr(Max(EventStart, ContentStart));
    FFinishText := DateTimeHelper.TimeToStr(Min(EventFinish, ContentFinish));
  end
  else
    inherited CalculateCaptions;
end;

function TcxSchedulerWeekViewEventCellViewInfo.GetCaptionFlags: Cardinal;
begin
  Result := CXTO_LEFT or CXTO_CENTER_VERTICALLY or CXTO_WORDBREAK or CXTO_EDITCONTROL;
end;

{ TcxSchedulerWeekViewEventCellModernViewInfo }

constructor TcxSchedulerWeekViewEventCellModernViewInfo.Create(AViewData: TcxSchedulerEventViewData;
  ACanResize: Boolean;  AScaleFactor: TdxScaleFactor; AUseRightToLeftAlignment: Boolean);
begin
  inherited Create(AViewData, ACanResize, AScaleFactor, AUseRightToLeftAlignment);
  FCaptionAndLocationCanPlaceOneLineOnly := True;
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.AfterDraw;
begin
  ViewData.BorderColor := FRealBorderColor;
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.BeforeDraw;
begin
  FRealBorderColor := ViewData.BorderColor;
  if ActualBorderColorIsNone then
    ViewData.BorderColor := ViewParams.Color;
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.CalculateBorderAttributes;
begin
  inherited CalculateBorderAttributes;
  ViewData.BorderColor := ExternalPainter.GetEventBorderColor(Self);
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.CalculateCaptions;
var
  ACaption: string;
begin
  inherited CalculateCaptions;
  FLocation := Trim(Event.Location);
  ACaption := ViewData.Caption;
  if FLocation <> '' then
    ACaption := ACaption + ';';
  ViewData.Caption := ACaption;
  FCaptionIsBold := IsHeaderEvent and (Event.Duration > 1);
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.CalculateHeaderEventNeededCaptionWidth(var AFullWidth, ACaptionOnlyWidth: Integer);
var
  AStyle: TFontStyles;
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

procedure TcxSchedulerWeekViewEventCellModernViewInfo.CalculateItemsLayout;
begin
  inherited CalculateItemsLayout;
  CalculateLocationRect;
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.CalculateLocationRect;
var
  ASavedStyle: TFontStyles;
begin
  inherited CalculateLocationRect;
  if FLocation = '' then
    Exit;
  FLocationRect := FCaptionRect;
  ASavedStyle := Canvas.Font.Style;
  Canvas.Font.Style := GetCaptionFontStyle;
  FLocationRect.Left := Min(CaptionRect.Left + cxTextWidth(Canvas.Font, Caption + ' '), FLocationRect.Right);
  Canvas.Font.Style := ASavedStyle;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.CanBeHeaderEventContinueArrows: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerWeekViewEventCellModernViewInfo.DoDrawCaption;
begin
  cxTextOut(Canvas.Canvas, Caption, FCaptionRect, GetCaptionFlags,
    0, 0, Canvas.Font, clNone, clNone, 0, 0, 0, ViewParams.TextColor);
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.EnableModernSelection: Boolean;
begin
  Result := True;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetBoundsWithoutTimeLineRect: TRect;
begin
  Result := Bounds;
  Result.Left := TimeLineRect.Right;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetCaptionFlags: Cardinal;
const
  AHorizontalAlignment: array[Boolean] of Word = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := AHorizontalAlignment[UseRightToLeftAlignment] or CXTO_CENTER_VERTICALLY or
    CXTO_WORDBREAK or CXTO_EDITCONTROL or CXTO_EXCELSTYLE or CXTO_SINGLELINE or CXTO_END_ELLIPSIS;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetCaptionFontStyle: TFontStyles;
begin
  Result := inherited GetCaptionFontStyle;
  if FCaptionIsBold then
    Result := Result + [fsBold];
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetHeaderEventCaptionLeft(
  const ARect: TRect; AContentWidth: Integer): Integer;
begin
  if not FCaptionIsBold then
    Result := ARect.Left
  else
    Result := inherited GetHeaderEventCaptionLeft(ARect, AContentWidth);
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetHeaderEventStartClockOriginLeft: Integer;
begin
  Result := TimeLineRect.Right;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetNonDetailEventPossibleCaptionRect: TRect;
begin
  Result := inherited GetNonDetailEventPossibleCaptionRect;
  Result.Left := Max(Result.Left, TimeLineRect.Right + ScaleFactor.Apply(cxTextOffset));
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetTimeLineRectRight: Integer;
begin
  Result := Bounds.Left + ScaleFactor.Apply(cxTimeLineWidth) + BorderSize;
  if ActualBorderColorIsNone then
    Dec(Result, BorderSize);
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.GetViewStyle: TcxSchedulerViewStyle;
begin
  Result := svsModern;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.IsTimeLineVisible: Boolean;
begin
  Result := True;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.NeedHeaderEventStartContinueArrow: Boolean;
begin
  Result := (ContentStart = ViewData.ViewContentStart) and
    (EventStart < ContentStart) and (EventStart < ViewData.ViewContentStart);
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.NeedHeaderEventFinishContinueArrow: Boolean;
begin
  Result := (ContentFinish - 1 = ViewData.ViewContentFinish) and
    (EventFinish > ContentFinish) and (EventFinish > ViewData.ViewContentFinish);
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.NeedLeftSizingHandler: Boolean;
begin
  Result := IsHeaderEvent and HasLeftBorder;
end;

function TcxSchedulerWeekViewEventCellModernViewInfo.NeedRightSizingHandler: Boolean;
begin
  Result := IsHeaderEvent and HasRightBorder;
end;

{ TcxSchedulerWeekViewViewInfo }

constructor TcxSchedulerWeekViewViewInfo.Create(AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  Builder := TcxSchedulerEventLayoutBuilder.Create;
end;

destructor TcxSchedulerWeekViewViewInfo.Destroy;
begin
  Builder.Free;
  inherited Destroy;
end;

procedure TcxSchedulerWeekViewViewInfo.ScrollVertical(
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  ADelta: Integer;
begin
  ADelta := View.GetScrollPos(StartDates[0], False);
  if not (AScrollCode in [scPosition, scTrack]) then
    AScrollPos := ADelta;
  case AScrollCode of
    scLineUp:
      Dec(AScrollPos, 1);
    scLineDown:
      Inc(AScrollPos, 1);
    scPageUp:
      Dec(AScrollPos, Weeks.Count);
    scPageDown:
      Inc(AScrollPos, Weeks.Count);
    scTop:
      AScrollPos := -View.GetMinScrollPos;
    scBottom:
      AScrollPos := View.GetMaxScrollPos;
  end;
  ADelta := ADelta - AScrollPos;
  if ADelta <> 0 then
  begin
    View.ScrollSelectedDays(ADelta);
    View.LayoutChanged;
    AScrollPos := View.GetScrollPos(StartDates[0]);
  end;
end;

function TcxSchedulerWeekViewViewInfo.AddContentCell(const ARect: TRect;
  const AStart, AFinish: TDateTime; AResourceIndex: Integer): TcxSchedulerContentCellViewInfo;
begin
  Result := inherited AddContentCell(ARect, AStart, AFinish, AResourceIndex);
  Result.DontPrint := PrintExactlyOneMonth and (MonthOf(AStart) <> PrintedMonth);
  TcxSchedulerContentCellViewInfoAccess(Result).SetContentSelectionParameters(
    IsTimeSelected(AStart, Result.Resource), FSelectionParams);
  TcxSchedulerContentCellViewInfoAccess(Result).FFirstEventTop := ARect.Top + GetDayCellHeaderHeight;
end;

procedure TcxSchedulerWeekViewViewInfo.AddDayCellEvent(APlace: TcxSchedulerEventPlace);
const
  AIndent: array[Boolean] of Integer = (cxTextOffset, 1);
var
  I, AOffset: Integer;
  AStart, AFinish: TDateTime;
  ABounds, AVisibleRect: TRect;
  AResource: TcxSchedulerResourceViewInfo;
  AViewData: TcxSchedulerEventViewData;
  ACell: TcxSchedulerEventCellViewInfo;
  AEvent: TcxSchedulerControlEvent;
begin
  AOffset := TcxSchedulerContentCellViewInfoAccess(DayCells[APlace.ColStart]).FFirstEventTop;
  AEvent := TcxSchedulerControlEvent(APlace.Event);
  ABounds.Top := 0;
  ABounds.Bottom := FContentLineHeight;
  GetEventInfo(APlace, ABounds.Left, ABounds.Right, AStart, AFinish, AVisibleRect, AResource);
  OffsetRect(ABounds, 0, AOffset + APlace.LineStart * (FContentLineHeight + AIndent[ViewStyle = svsModern]));
  if ABounds.Bottom <= AVisibleRect.Bottom then
  begin
    AViewData := CreateEventViewData(AEvent, ABounds, AStart, AFinish, AResource);
    AViewData.VisibleRect := AVisibleRect;
    AViewData.ViewContentStart := TcxSchedulerContentCellViewInfoAccess(DayCells[0]).DateTime;
    AViewData.ViewContentFinish := TcxSchedulerContentCellViewInfoAccess(DayCells[DayCellCount - 1]).DateTime;
    if AEvent.Selected and (SchedulerStyle = svsClassic) then
      with Styles.GetSelectionParams do
      begin
        AViewData.ViewParams.Color := Color;
        AViewData.ViewParams.TextColor := TextColor;
        AViewData.ViewParams.Font := Font;
      end;
    ACell := AddEventCell(AViewData);
    if not AEvent.IsAllDayOrLonger and (ViewStyle = svsClassic) then
    begin
      ACell.Transparent := not (AEvent.Selected or Styles.IsEventStyleAssigned(AEvent));
      ACell.Borders := [];
    end;
    CheckDaysTimeEvent(ACell, APlace);
  end
  else
    for I := APlace.ColStart to APlace.ColFinish do
    begin
       if DayCells[I].LineCount <> MaxInt then
       begin
         AddButton(DayCells[I].Bounds, DayCells[I].TimeStart, True, AEvent);
         DayCells[I].LineCount := MaxInt;
       end;
    end;
end;

function TcxSchedulerWeekViewViewInfo.AddDayCellEventPlace(
  AEvent: TcxSchedulerControlEvent; var AIndex: Integer; AStartDay: TcxSchedulerContentCellViewInfo): Boolean;
var
  I, AStartCol, AFinishCol: Integer;
  ADay: TcxSchedulerContentCellViewInfo;
begin
  Result := False;
  if not CanEventVisible(AEvent) then
  begin
    Inc(AIndex);
    Exit;
  end;
  // skip days for event on invisible line
  if (AStartDay.LineCount = MaxInt) then
  begin
    while (AIndex < (DayCellCount - 1)) and IsSingleLine(DayCells[AIndex], AStartDay) do
      Inc(AIndex);
    Exit;
  end;
  AStartCol := AIndex;
  AFinishCol := AIndex;
  Result := True;
  for I := AIndex + 1 to DayCellCount - 1 do
  begin
    ADay := DayCells[I];
    Result := IsCellEvent(AEvent, ADay);
    if Result and IsSingleLine(AStartDay, ADay) then
    begin
      ADay.LineCount := AStartDay.LineCount + 1;
      if CanVisibleNextLine(ADay, AStartDay.LineCount) then
        AFinishCol := I
      else
      begin
        AddButton(ADay.Bounds, ADay.TimeStart, True, AEvent);
        AIndex := I + 1;
        Break;
      end;
    end
    else
      Break;
  end;
  AIndex := Max(AFinishCol + 1, AIndex);
  Builder.AddEventPlace(AEvent, AStartCol, AFinishCol);
  AStartDay.LineCount := AStartDay.LineCount + 1;
end;

function TcxSchedulerWeekViewViewInfo.AddWeekDayHeader(
  AWeekDay: Integer; ALeft, ARight: Integer): TcxSchedulerWeekDayHeaderCellViewInfo;
begin
  CreateCellInstance(WeekDayHeaderClass, cxRect(ALeft, FDayHeaderOffsetTop,
    ARight, FDayHeaderOffsetTop + FDayHeaderHeight),
    StylesAdapter.GetDayHeaderParams(View.FirstVisibleDate + AWeekDay), UseRightToLeftAlignment, Result);
  if CompressWeekEnd and not HideWeekEnd and (AWeekDay >= 6) then
    Result.DateTime := 8
  else
    Result.DateTime := AWeekDay;
  DayHeaderCells.Add(Result);
end;

function TcxSchedulerWeekViewViewInfo.WeekDayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerDayHeaderCellViewInfoClass =
    (TcxSchedulerWeekDayHeaderCellViewInfo, TcxSchedulerWeekDayHeaderCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateDaysLayout;
begin
  if GroupByResource then
    ProcessGroupByResource
  else
    if GroupByDate then
      ProcessGroupByDate
    else
      ProcessGroupByNone;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateDaysLayoutForResource(
  const ABounds: TRect; AResourceIndex: Integer);
const
  AddBorders: array[Boolean] of TcxBorders = ([bRight], []);
begin
  CalculateWeekDisplayRectsVert(StartDates[0], AResourceIndex, ABounds,
    AddBorders[HasSeparator or (AResourceIndex = (ResourceCount - 1))]);
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateEventsOrder;
var
  I: Integer;
  AList: TList;
  AEvent: TcxSchedulerEvent;
begin
  AList := TList.Create;
  try
    AList.Capacity := Events.AbsoluteCount;
    while AList.Count <> Events.AbsoluteCount do
    begin
      I := 0;
      while I <  Events.AbsoluteCount do
      begin
        AEvent := Events.AbsoluteItems[I];
        Events.AbsoluteItemsList[I] := nil;
        Inc(I);
        if AEvent = nil then Continue;
        AList.Add(AEvent);
        while (I < Events.AbsoluteCount) and ((Events.AbsoluteItems[I] = nil) or
          AEvent.IsDayEvent(Integer(Trunc(Events.AbsoluteItems[I].Start)))) do Inc(I);
      end;
    end;
  finally
    Events.AbsoluteItemsList.Assign(AList);
    AList.Free;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateEventsViewInfo;
var
  I, J: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  CalculateEventsOrder;
  for I := 0 to Events.AbsoluteCount - 1 do
  begin
    J := 0;
    AEvent := Events.AbsoluteItems[I];
    while J < DayCellCount do
    begin
      if IsCellEvent(AEvent, DayCells[J]) then
      begin
        if not AddDayCellEventPlace(AEvent, J, DayCells[J]) and (ResourceCount = 0) then
          Break;
      end
      else
        Inc(J);
    end;
  end;
  Builder.Calculate(SchedulerStyle);
  for I := 0 to Builder.EventPlaceCount - 1 do
    AddDayCellEvent(Builder.EventPlaces[I]);
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateGroupByDateForResource(
  const ABounds: TRect; AResourceIndex: Integer);
begin
  CalculateWeekDisplayRectsHorz(StartDates[0], AResourceIndex, ABounds, True, True);
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateHeaderViewInfo;
const
  ADisplayMode: array[Boolean] of TcxSchedulerDayHeaderModernStyleDisplayMode =
    (hdmClassic, hdmDayAndDate);
var
  R: TRect;
  I, AIndex, AWeekDay: Integer;
  ACell: TcxSchedulerContentCellViewInfo;
  AHeader: TcxSchedulerDayHeaderCellViewInfo;
const
  ANeighbors: array[Boolean] of TcxNeighbors = ([], [nLeft]);
begin
  if GroupByDate then
  begin
    for I := 0 to FRowCols - 1 do
    begin
      ACell := DayCells[I];
      AWeekDay := DayOfWeek(ACell.TimeStart) - 1;
      CorrectWeekDay(AWeekDay);
      AHeader := AddWeekDayHeader(AWeekDay, ACell.Bounds.Left, ACell.Bounds.Right);
      AHeader.Compressed := (I = 5) and CompressWeekEnd;
    end;
  end
  else
  begin
    AIndex := Byte(ResourceCount > 0) - 1;
    for I := 0 to ContentCells.Count - 1 do
    begin
      ACell := TcxSchedulerContentCellViewInfo(ContentCells[I]);
      R := cxRectSetHeight(ACell.Bounds, FDayHeaderHeight);

      if (ViewStyle = svsClassic) or TcxSchedulerContentCellViewInfoAccess(ACell).IsDrawBySkin then
      begin
        OffsetRect(R, 0, 1);
        if HasVisibleBounds then
        begin
          Inc(R.Right);
          OffsetRect(R, 0, -1);
        end;
        if (View.DaysLayout <> wdlOneColumn) and (not HasSeparator or ((I mod 7) <= 2)) then
          Dec(R.Right);
      end
      else
        if (View.DaysLayout <> wdlOneColumn) and (R.Right < Bounds.Right) then
          Dec(R.Right);
      with AddDayHeader(ACell.TimeStart, R, AIndex, ADisplayMode[ViewStyle = svsModern]) do
      begin
        AlignHorz := taRightJustify;
        Selected := IsTimeSelected(DateTime, Resource);
        if Selected and not HideSelection and (ViewStyle = svsClassic) then
        begin
          SelectionColor := FSelectionParams.Color;
          SelectionTextColor := FSelectionParams.TextColor;
        end;
      end;
      if (I + 1) mod View.GetWeekDayCount = 0 then
        Inc(AIndex);
    end;
  end;
  ProcessDateToDisplayText(True);
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateMetrics;
begin
  inherited CalculateMetrics;
  FDayHeaderOffsetTop := Bounds.Top;
  FHeaderOffsetLeft := Bounds.Left;
  FResourceHeaderWidth := FResourceHeaderHeight;
  Weeks.Clear;
  CalculatePrintRowCols;
  CalculateWeekDates;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculatePrintRowCols;
var
  I: Integer;
begin
  FWeekEndCells := Byte(not PrintWeekEnds) * (Byte(not CompressWeekEnd) + 1);
  FRowColsStart := 0;
  FRowColsStartDateOffset := 0;
  if View.IsWholeWeekShown then
    FRowCols := WholeWeekDayCount - Byte(CompressWeekEnd) - FWeekEndCells
  else
    FRowCols := View.GetWorkWeekDayCount;
  if PagesPerWeek = 2 then
  begin
    if PrimaryPage then
      FRowCols := Min(3, View.GetWorkWeekDayCount)
    else
    begin
      FRowColsStart := 3;
      FRowColsStartDateOffset := FRowColsStart;
      for I := 0 to FRowCols - 1 do
        if not IsWorkDay(I, Scheduler) and HideWeekEnd then
          Inc(FRowColsStartDateOffset);
      FRowCols := FRowCols - FRowColsStart;
    end;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateResourceHeadersAutoHeight(
  AWidth: Integer);
begin
  if GroupByDate and not RotateCaptions then
    FResourceHeaderHeight := CalculateResourceHeaderWidth
  else
    inherited CalculateResourceHeadersAutoHeight(AWidth);
  FResourceHeaderWidth := FResourceHeaderHeight;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateWeekDates;
var
  I, ADate: Integer;
  AYear, APrevYear, AWeek, APrevWeek, AWeekDay: Word;
begin
  APrevYear := 0;
  APrevWeek := 0;
  for I := 0 to DayCount - 1 do
  begin
    DecodeDateWeek(Days[I] + View.GetDateOffset, AYear, AWeek, AWeekDay);
    if (AYear <> APrevYear) or (AWeek <> APrevWeek) then
    begin
      APrevYear := AYear;
      APrevWeek := AWeek;
      //DELPHI8! check Trunc(EncodeDateWeek(AYear, AWeek, 1))
      ADate := Trunc(EncodeDateWeek(AYear, AWeek, 1));
      Weeks.Add(ADate);
    end;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateWeekDisplayRectsHorz(
  const AStartDate: TDateTime; AResourceID: Integer;
  {const }ABounds: TRect; AIsTopWeek, AIsLastWeek: Boolean);
var
  I, J, W, K, AWeekDay: Integer;
  ARects: TcxSchedulerDaysRect;
  ACellBorders: TcxBorders;
const
  ABorders: array[Boolean] of TcxBorders = ([bTop, bRight], [bTop]);
begin
  W := ABounds.Right - ABounds.Left;
  for I := 0 to FRowCols - 1 do
  begin
    J := I + FRowColsStart;
    ARects[J] := cxRect(ABounds.Left + MulDiv(W, I, FRowCols),
      ABounds.Top - 1, ABounds.Left + MulDiv(W, I + 1, FRowCols), ABounds.Bottom);
    if CompressWeekEnd and not HideWeekEnd and (J = 5) then
      cxRectSplitVert(ARects[5], ARects[5], ARects[6]);
  end;

  K := -1;
  for I := FRowColsStartDateOffset to WholeWeekDayCount - 1 do
  begin
    AWeekDay := I + Integer(View.StartOfWeek);
    CorrectWeekDay(AWeekDay);
    if IsWorkDay(AWeekDay, Scheduler) or not HideWeekEnd then
    begin
      Inc(K);
      ACellBorders := ABorders[(K >= (FRowCols - 1)) and (HasSeparator or (AResourceID = (ResourceCount - 1)))];
      if AIsTopWeek and (K <> FRowCols) then
        Exclude(ACellBorders, bTop);
      if AIsLastWeek and not HasSeparator and (K <> FRowCols - 1) and (AResourceID <> ResourceCount - 1) then
        Include(ACellBorders, bBottom);
      J := K + FRowColsStart;
      AddContentCell(ARects[J], AStartDate + I,
        AStartDate + I + 1, AResourceID).Borders := ACellBorders;
    end;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateWeekDisplayRectsVert(
  const AStartDate: TDateTime; AResourceID: Integer;
  const ABounds: TRect; AddBorders: TcxBorders = []);
var
  R: TRect;
  I, J, AContentCount: Integer;
  ARects: TcxSchedulerDaysRect;
  ASize: TSize;
begin
  if View.DaysLayout = wdlOneColumn then
  begin
    CalculateWeekDisplayRectsVertEx(AStartDate, AResourceID, ABounds, AddBorders);
    Exit;
  end;
  FillChar(ARects, SizeOf(ARects), 0);
  ASize := cxRectSize(ABounds);
  for I := 0 to 2 do
  begin
    R := cxRect(ABounds.Left, ABounds.Top + MulDiv(ASize.CY, I, 3),
      ABounds.Right, ABounds.Top + MulDiv(ASize.CY, I + 1, 3));
    if PagesPerWeek = 1 then
      cxRectSplitHorz(R, ARects[I], ARects[I + 3])
    else
    begin
      ARects[I] := R;
      ARects[I + 3] := R;
    end;
  end;
  cxRectSplitVert(ARects[5], ARects[5], ARects[6]);
  if PagesPerWeek = 1 then
    AContentCount := 7
  else
    if PrimaryPage then
      AContentCount := 3
    else
      AContentCount := 4;
  for I := 0 to AContentCount - 1 do
  begin
    J := RowColsStart + I;
    AddContentCell(ARects[J], AStartDate + J, AStartDate + J + 1,
      AResourceID).Borders := WeekViewDayBorders[J] + AddBorders;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.CalculateWeekDisplayRectsVertEx(
  const AStartDate: TDateTime; AResourceID: Integer;
  const ABounds: TRect; AddBorders: TcxBorders = []);
var
  R: TRect;
  I, J, K, AWeekDay: Integer;
  ARects: TcxSchedulerDaysRect;
  ASize: TSize;
begin
  FillChar(ARects, SizeOf(ARects), 0);
  ASize := cxRectSize(ABounds);
  for I := 0 to FRowCols - 1 do
  begin
    R := cxRect(ABounds.Left, ABounds.Top + MulDiv(ASize.CY, I, FRowCols),
      ABounds.Right, ABounds.Top + MulDiv(ASize.CY, I + 1, FRowCols));
    ARects[RowColsStart + I] := R;
  end;
  if View.CompressWeekEnd and not HideWeekEnd then
    cxRectSplitVert(ARects[5], ARects[5], ARects[6]);
  K := -1;
  for I := 0 to WholeWeekDayCount - 1 do
  begin
    AWeekDay := I + Integer(View.StartOfWeek);
    CorrectWeekDay(AWeekDay);
    if IsWorkDay(AWeekDay, Scheduler) or not HideWeekEnd then
    begin
      Inc(K);
      J := RowColsStart + I;
      AddContentCell(ARects[RowColsStart + K], AStartDate + J, AStartDate + J + 1,
        AResourceID).Borders := [bTop] + AddBorders;
    end;
  end;
end;

function TcxSchedulerWeekViewViewInfo.CanEventVisible(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := not (AEvent.IsClone and FHideClones);
end;

function TcxSchedulerWeekViewViewInfo.CanVisibleNextLine(
  ADay: TcxSchedulerContentCellViewInfo; ALine: Integer = -1): Boolean;
begin
  if ALine = -1 then
    ALine := ADay.LineCount;
  Result := (ALine <> MaxInt) and((GetDayCellContentHeight(ADay)) >=
    (ALine + 1) * FContentLineHeight);
end;

procedure TcxSchedulerWeekViewViewInfo.CheckDaysTimeEvent(AEventInfo: TcxSchedulerEventCellViewInfo; APlace: TcxSchedulerEventPlace);
var
  I: Integer;
  ADay: TcxSchedulerContentCellViewInfo;
begin
  if (AEventInfo.Event.State <> 0) and (AEventInfo.Event.IsAllDayOrLonger) then
    for I := APlace.ColStart to APlace.ColFinish do
    begin
      ADay := DayCells[I];
      if (ADay.TimeEvent <> nil) and (ADay.TimeEvent.Event.State = 3) then
        Continue;
      if IsCellEvent(AEventInfo.Event, ADay) and
        ((ADay.TimeEvent = nil) or (ADay.TimeEvent.Event.State < AEventInfo.Event.State)) then
        ADay.TimeEvent := AEventInfo;
    end;
end;

procedure TcxSchedulerWeekViewViewInfo.CheckHeaderBorders;
var
  I: Integer;
const
  ANeighbors: array[Boolean] of TcxNeighbors =
    ([], [nTop]);
  ADayHeader: array[Boolean] of TcxNeighbors =
    ([], [nLeft]);
  ABorders: array[Boolean] of TcxBorders =
    ([], [bBottom]);
begin
  ProcessCheckBorders(DayHeaderCells, GroupingKind = gkByDate,
    ANeighbors[not GroupByDate] + ADayHeader[GroupByDate]);
  ProcessCheckBorders(ResourceHeaderCells, False, [], ABorders[GroupByResource]);
  if not GroupByDate then
  begin
    if GroupByResource then
      with TcxSchedulerDayHeaderCellViewInfo(ResourceHeaderCells[0]) do
        Borders := Borders - [bLeft];
    for I := 0 to DayHeaderCells.Count - 1 do
      with TcxSchedulerDayHeaderCellViewInfo(DayHeaderCells[I]) do
        Borders := [bBottom, bTop] * LookAndFeelPainter.HeaderBorders([nTop]);
  end;
end;

function TcxSchedulerWeekViewViewInfo.ContentCellClass: TcxSchedulerContentCellViewInfoClass;
const
  AClass: array[Boolean, Boolean] of TcxSchedulerContentCellViewInfoClass =
    ((TcxSchedulerContentCellViewInfo, TcxSchedulerWeekDayContentCellModernViewInfo),
     (TcxSchedulerMonthDayContentCellViewInfo, TcxSchedulerMonthDayContentCellModernViewInfo));
begin
  Result := AClass[GroupByDate, ViewStyle = svsModern]
end;

procedure TcxSchedulerWeekViewViewInfo.CorrectWeekDay(var AWeekDay: Integer);
begin
  if AWeekDay > 6 then Dec(AWeekDay, 7);
  if AWeekDay < 0 then Inc(AWeekDay, 7);
end;

procedure TcxSchedulerWeekViewViewInfo.DoCalculate;
begin
  View.CheckWorkDays;
  FPrintWeekEnds := True;
  Builder.Clear;
  inherited DoCalculate;
  FPrintExactlyOneMonth := Adapter.PrintExactlyOneMonth;
  if FPrintExactlyOneMonth then
    FPrintedMonth := MonthOf(Days[DayCount div 2]);
  if IsCalculateHeaderFirst then
  begin
    CalculateHeaderViewInfo;
    CalculateDaysLayout;
  end
  else
  begin
    CalculateDaysLayout;
    CalculateHeaderViewInfo;
  end;
  CalculateEventsViewInfo;
  CheckHeaderBorders;
  CalculateContentNavigationButtons;
end;

procedure TcxSchedulerWeekViewViewInfo.DoContentNavigationButtonClick(
  Sender: TcxSchedulerContentNavigationButtonViewInfo);
const
  DaysInWeek = 7;
var
  AInterval: TDateTime;
begin
  if Sender.Interval < 0 then
  begin
    if Sender.Interval / DaysInWeek = Trunc(Sender.Interval / DaysInWeek) then
      AInterval := DaysInWeek * Trunc(Sender.Interval / DaysInWeek)
    else
      AInterval := DaysInWeek * (Trunc(Sender.Interval / DaysInWeek) - 1);
  end
  else
    AInterval := DaysInWeek * (Trunc((Sender.Interval - 1) / DaysInWeek) + 1);
  SelectedDays.ShiftPeriod(AInterval);
  inherited DoContentNavigationButtonClick(Sender);
end;

procedure TcxSchedulerWeekViewViewInfo.DoMoreEventsButtonClick(
  Sender: TcxSchedulerMoreEventsButtonViewInfo);
begin
  SelectedDays.Clear;
  SelectedDays.Add(Sender.DateTime);
  View.PeriodChanged;
end;

function TcxSchedulerWeekViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerEventCellViewInfoClass =
    (TcxSchedulerWeekViewEventCellViewInfo, TcxSchedulerWeekViewEventCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerWeekViewViewInfo.GetCompressWeekEnd: Boolean;
begin
  Result := View.CompressWeekEnd or (not GroupByDate and (View.DaysLayout = wdlTwoColumns));
end;

procedure TcxSchedulerWeekViewViewInfo.GetEventInfo(APlace: TcxSchedulerEventPlace;
  var ALeft, ARight: Integer; var AStart, AFinish: TDateTime;
  var AVisibleRect: TRect; var AResource: TcxSchedulerResourceViewInfo);
var
  AColStart, AColFinish: Integer;
begin
  AColStart := APlace.ColStart;
  AColFinish := APlace.ColFinish;
  with DayCells[AColStart] do
  begin
    AStart := TimeStart;
    ALeft := Bounds.Left + cxTextOffset;
    AResource := Resource;
    AVisibleRect := Bounds;
  end;
  with DayCells[AColFinish] do
  begin
    AFinish := TimeFinish;
    ARight := Bounds.Right - cxTextOffset - 1;
    if (SchedulerStyle = svsModern) and (APlace.Event.Finish <= AFinish) then
      ARight := Bounds.Right - 2 * TcxSchedulerPainterHelper.ShadowSize(ScaleFactor)
    else
      ARight := Bounds.Right - cxTextOffset - 1;
    AVisibleRect.Right := Bounds.Right;
  end;
  Dec(AVisibleRect.Bottom, cxTextOffset);
end;

function TcxSchedulerWeekViewViewInfo.GetResourcesContentWidth: Integer;
begin
  if GroupByResource then
    Result := Bounds.Right - Bounds.Left
  else
    Result := inherited GetResourcesContentWidth;
end;

function TcxSchedulerWeekViewViewInfo.GetResourceImagesSize: TSize;
begin
  if ResourceImages = nil then
    Result := TSize(cxNullSize)
  else
  begin
    Result := inherited GetResourceImagesSize;
    if GroupByDate and ResourceHeaders.RotateCaptions then
      Result := ScaleFactor.Apply(cxSize(ResourceImages.Height, ResourceImages.Width));
  end;
end;

function TcxSchedulerWeekViewViewInfo.GetResourceScrollBarKind: TScrollBarKind;
begin
  if (FGroupingKind = gkByDate) and ResourceNavigator.NeedScrollBar then
    Result := sbVertical
  else
    Result := sbHorizontal
end;

function TcxSchedulerWeekViewViewInfo.GetContentParams(
  const ATime: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxViewParams;
begin
  if AResource = nil then
    Result := StylesAdapter.GetContentParams(ATime, True, nil)
  else
    Result := StylesAdapter.GetContentParams(ATime, True, AResource.ResourceItem);
end;

function TcxSchedulerWeekViewViewInfo.GetDayCellContentHeight(
  ADay: TcxSchedulerContentCellViewInfo): Integer;
begin
  Result := cxRectHeight(PainterHelper.ExcludeBorders(ADay.Bounds,
    ADay.Borders)) - GetDayCellHeaderHeight - cxTextOffset;
end;

function TcxSchedulerWeekViewViewInfo.GetDayCellHeaderHeight: Integer;
begin
  if GroupByDate then
    Result := cxTextOffset + FContentFontHeight
  else
    Result := FDayHeaderHeight + cxTextOffset;
end;

function TcxSchedulerWeekViewViewInfo.GetHideWeekEnd: Boolean;
begin
  Result := View.HideWeekEnd;
end;

function TcxSchedulerWeekViewViewInfo.GetStartDate(Index: Integer): TDateTime;
begin
  Result := Weeks[Index];
  if not IsWeekStartOnMonday then
    Result := Result - View.GetDateOffset;
end;

function TcxSchedulerWeekViewViewInfo.IsCalculateHeaderFirst: Boolean;
begin
  Result := False;
end;

function TcxSchedulerWeekViewViewInfo.IsCellEvent(
  AEvent: TcxSchedulerControlEvent; ACell: TcxSchedulerContentCellViewInfo): Boolean;
begin
  Result := (ACell <> nil) and AEvent.IsDayEvent(Integer(Trunc(ACell.TimeStart)));
  if Result and (GroupingKind <> gkNone) then
    Result := AEvent.IsResourceEvent(ACell.Resource.ResourceItem, ShowEventsWithoutResource);
  if Result and PrintExactlyOneMonth then
    Result := MonthOf(ACell.TimeStart) = PrintedMonth;
end;

function TcxSchedulerWeekViewViewInfo.IsSingleLine(
  ADay1, ADay2: TcxSchedulerContentCellViewInfo): Boolean;
begin
  Result := (ADay1 <> nil) and (ADay2 <> nil) and
    (ADay1.Bounds.Top = ADay2.Bounds.Top);
end;

function TcxSchedulerWeekViewViewInfo.IsTimeSelected(
  ATime: TDateTime; AResource: TObject): Boolean;
begin
  ATime := DateTimeHelper.RoundTime(ATime);
  Result := not HideSelection and (AResource = FSelResource) and
    (ATime >= SelStart) and (ATime <= SelFinish);
  Result := Result and CanSelected;
end;

function TcxSchedulerWeekViewViewInfo.IsWeekStartOnMonday: Boolean;
begin
  if GroupByDate then
    Result := not HideWeekEnd and CompressWeekEnd
  else
    Result := not HideWeekEnd or (View.DaysLayout = wdlTwoColumns);
end;

procedure TcxSchedulerWeekViewViewInfo.ProcessGroupByDate;

  function IsSameMonth(ACell1, ACell2: TcxSchedulerMonthDayContentCellViewInfo): Boolean;
  begin
    Result := (ACell1 <> nil) and (ACell2 <> nil);
    if Result then
      Result := ACell1.Resource = ACell2.Resource;
    if Result then
      Result := MonthOf(ACell1.TimeStart) = MonthOf(ACell2.TimeStart);
  end;

var
  I, H: Integer;
  ABounds: TRect;

  ACell: TcxSchedulerHeaderCellViewInfo;
  APrevCell, AContentCell: TcxSchedulerMonthDayContentCellViewInfo;
begin
  ABounds := Bounds;
  Inc(ABounds.Left, ResourceHeaderWidth);
  H := cxRectHeight(ABounds) - DayHeaderHeight -
    SeparatorWidth * (ResourceCount - 1);
  AddResourceHeader(-1, cxRectSetSize(Bounds, ResourceHeaderWidth, DayHeaderHeight));
  for I := 0 to ResourceCount - 1 do
  begin
    ABounds.Top := Bounds.Top + DayHeaderHeight +
      MulDiv(H, I, ResourceCount) + SeparatorWidth * I + 1;
    ABounds.Bottom := Bounds.Top + DayHeaderHeight +
      MulDiv(H, I + 1, ResourceCount) + SeparatorWidth * I;
    AddResourceBounds(I, ABounds);
    CalculateGroupByDateForResource(ABounds, I);
    if I < (ResourceCount - 1) then
      AddGroupHorzSeparator(ABounds.Bottom);
    Dec(ABounds.Top);
    ACell := AddResourceHeader(I, cxRectSetWidth(ABounds,
      Bounds.Left, ResourceHeaderWidth));
    ACell.RotateText := RotateCaptions;
    ACell.RotateHeader := True;
  end;
  APrevCell := nil;
  for I := 0 to ContentCells.Count - 1 do
  begin
    AContentCell := TcxSchedulerMonthDayContentCellViewInfo(ContentCells[I]);
    AContentCell.SetContentState(IsSameMonth(APrevCell, AContentCell),
      IsTimeSelected(AContentCell.TimeStart, AContentCell.Resource),
      FContentFontHeight, FSelectionParams);
    APrevCell := AContentCell;
  end;
end;

procedure TcxSchedulerWeekViewViewInfo.ProcessGroupByNone;
begin
  CalculateWeekDisplayRectsVert(StartDates[0], -1, Bounds);
end;

procedure TcxSchedulerWeekViewViewInfo.ProcessGroupByResource;
var
  I, W: Integer;
  ABounds: TRect;
  ACell: TcxSchedulerHeaderCellViewInfo;
begin
  ABounds := Bounds;
  W := ABounds.Right - ABounds.Left - SeparatorWidth * (ResourceCount - 1);
  Inc(ABounds.Top, ResourceHeaderHeight);
  for I := 0 to ResourceCount - 1 do
  begin
    ABounds.Left := MulDiv(W, I, ResourceCount) + SeparatorWidth * I + Bounds.Left;
    ABounds.Right := MulDiv(W, I + 1, ResourceCount) + SeparatorWidth * I + Bounds.Left;
    AddResourceBounds(I, ABounds);
    CalculateDaysLayoutForResource(ABounds, I);
    if I < (ResourceCount - 1) then
      AddGroupVertSeparator(ABounds.Right);
    ACell := AddResourceHeader(I, cxRectSetTop(ABounds, Bounds.Top, ResourceHeaderHeight));
    ACell.Borders := LookAndFeelPainter.HeaderBorders([nLeft, nRight, nBottom]);
    if not (bLeft in ACell.Borders) then
      ACell.Borders := ACell.Borders - [bRight, bBottom];
  end;
end;

function TcxSchedulerWeekViewViewInfo.GetDayCell(
  ADay: Integer): TcxSchedulerContentCellViewInfo;
begin
  Result := TcxSchedulerContentCellViewInfo(ContentCells[ADay]);
end;

function TcxSchedulerWeekViewViewInfo.GetDayCellCount: Integer;
begin
  Result := ContentCells.Count;
end;

function TcxSchedulerWeekViewViewInfo.GetRotateCaptions: Boolean;
begin
  Result := View.OptionsView.RotateResourceCaptions;
end;

function TcxSchedulerWeekViewViewInfo.GetView: TcxSchedulerWeekView;
begin
  Result := TcxSchedulerWeekView(inherited View);
end;

function TcxSchedulerWeekViewViewInfo.GetWeeks: TcxSchedulerDateList;
begin
  Result := View.FWeeks;
end;

{ TcxSchedulerWeeksViewEventCellViewInfo }

function TcxSchedulerWeeksViewEventCellViewInfo.CanAutoHideStandardImages: Boolean;
begin
  Result := True;
end;

{ TcxSchedulerWeeksViewEventCellModernViewInfo }

function TcxSchedulerWeeksViewEventCellModernViewInfo.CanAutoHideStandardImages: Boolean;
begin
  Result := True;
end;

{ TcxSchedulerWeeksViewViewInfo }

procedure TcxSchedulerWeeksViewViewInfo.CalculateDaysLayoutForResource(
  const ABounds: TRect; AResourceIndex: Integer);
var
  R: TRect;
  C, I, H, ATop, APrevMonth: Integer;
  ACell: TcxSchedulerMonthDayContentCellViewInfo;
begin
  ATop := ABounds.Top;
  if not GroupByDate then
    Inc(ATop, FDayHeaderHeight);
  H := ABounds.Bottom - ATop;
  C := ContentCells.Count;
  for I := 0 to Weeks.Count - 1 do
  begin
    R := cxRect(ABounds.Left, ATop + MulDiv(H, I, Weeks.Count),
      ABounds.Right, ATop + MulDiv(H, I + 1, Weeks.Count));
    if (I > 0) or not GroupByDate then Inc(R.Top);
    CalculateWeekDisplayRectsHorz(StartDates[I], AResourceIndex, R, I = 0, I = Weeks.Count - 1);
  end;
  APrevMonth := -1;
  for I := C to ContentCells.Count - 1 do
  begin
    ACell := TcxSchedulerMonthDayContentCellViewInfo(ContentCells[I]);
    ACell.SetContentState(APrevMonth = MonthOf(ACell.TimeStart),
      IsTimeSelected(ACell.TimeStart, ACell.Resource), FContentFontHeight, FSelectionParams);
    APrevMonth := MonthOf(ACell.TimeStart);
  end;
end;

procedure TcxSchedulerWeeksViewViewInfo.CalculateGroupByDateForResource(
  const ABounds: TRect; AResourceIndex: Integer);
begin
  CalculateDaysLayoutForResource(ABounds, AResourceIndex);
end;

procedure TcxSchedulerWeeksViewViewInfo.CalculateHeaderViewInfo;
var
  I, J, W, AWeekDay: Integer;
begin
  if GroupByResource then Exit;
  if GroupByDate then
    FHeaderOffsetLeft := ResourceHeaderWidth + Bounds.Left;
  W := Bounds.Right - FHeaderOffsetLeft;
  J := 0;
  I := 0;
  while J < FRowCols do
  begin
    AWeekDay := I + Integer(View.StartOfWeek) + FRowColsStartDateOffset;
    CorrectWeekDay(AWeekDay);
    if IsWorkDay(AWeekDay, Scheduler) or not HideWeekEnd then
    begin
      if ((J + FRowColsStartDateOffset) < 6) or HideWeekEnd or not CompressWeekEnd then
        AddWeekDayHeader(AWeekDay,
          MulDiv(W, J, FRowCols) + FHeaderOffsetLeft,
          MulDiv(W, J + 1, FRowCols) + FHeaderOffsetLeft);
      Inc(J);
    end;
    Inc(I);
  end;
  ProcessDateToDisplayText(True);
  FContentOffset := FDayHeaderHeight;
end;

procedure TcxSchedulerWeeksViewViewInfo.CalculateMetrics;
begin
  FPrintWeekEnds := not Adapter.DontPrintWeekEnd;
  inherited CalculateMetrics;
end;

procedure TcxSchedulerWeeksViewViewInfo.CheckHeaderBorders;
const
  ANeighbors: array[Boolean] of TcxNeighbors =
    ([], [nTop]);
begin
  ProcessCheckBorders(DayHeaderCells, False, ANeighbors[GroupByResource]);
  ProcessCheckBorders(ResourceHeaderCells, False);
  if not GroupByDate or not (bLeft in LookAndFeelPainter.HeaderBorders([nLeft])) then
  begin
    with TcxSchedulerHeaderCellViewInfo(DayHeaderCells[0]) do
      Borders := Borders - [bLeft];
    if GroupByResource then
      with TcxSchedulerDayHeaderCellViewInfo(ResourceHeaderCells[0]) do
        Borders := Borders - [bLeft];
  end;
end;

function TcxSchedulerWeeksViewViewInfo.ContentCellClass: TcxSchedulerContentCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerContentCellViewInfoClass =
    (TcxSchedulerMonthDayContentCellViewInfo, TcxSchedulerMonthDayContentCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerWeeksViewViewInfo.DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
begin
  Result := WeekDayHeaderClass;
end;

function TcxSchedulerWeeksViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerEventCellViewInfoClass =
    (TcxSchedulerWeeksViewEventCellViewInfo, TcxSchedulerWeeksViewEventCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerWeeksViewViewInfo.GetCompressWeekEnd: Boolean;
begin
  Result := View.CompressWeekEnd;
end;

function TcxSchedulerWeeksViewViewInfo.GetContentParams(
  const ATime: TDateTime; AResource: TcxSchedulerResourceViewInfo): TcxViewParams;
begin
  if AResource = nil then
    Result := StylesAdapter.GetContentParams(ATime, not Odd(MonthOf(ATime)), nil)
  else
    Result := StylesAdapter.GetContentParams(ATime, not Odd(MonthOf(ATime)),
       AResource.ResourceItem);
end;

function TcxSchedulerWeeksViewViewInfo.GetDayCellHeaderHeight: Integer;
begin
  Result := cxTextOffset + FContentFontHeight;
end;

function TcxSchedulerWeeksViewViewInfo.IsCalculateHeaderFirst: Boolean;
begin
  Result := True;
end;

function TcxSchedulerWeeksViewViewInfo.IsWeekStartOnMonday: Boolean;
begin
  Result := not HideWeekEnd and CompressWeekEnd;
end;

procedure TcxSchedulerWeeksViewViewInfo.ProcessGroupByNone;
begin
  CalculateDaysLayoutForResource(Bounds, -1);
end;

procedure TcxSchedulerWeeksViewViewInfo.ProcessGroupByResource;
var
  I, J, K, W, AWeekDay: Integer;
  ACell: TcxSchedulerHeaderCellViewInfo;
begin
  inherited ProcessGroupByResource;
  Inc(FDayHeaderOffsetTop, FResourceHeaderHeight);
  for J := 0 to ResourceHeaderCells.Count - 1 do
  begin
    ACell := TcxSchedulerHeaderCellViewInfo(ResourceHeaderCells[J]);
    W := cxRectWidth(ACell.Bounds);
    K := 0;
    I := 0;
    while K < FRowCols do
    begin
      AWeekDay := I + Integer(View.StartOfWeek) + FRowColsStartDateOffset;
      CorrectWeekDay(AWeekDay);
      if IsWorkDay(AWeekDay, Scheduler) or not HideWeekEnd then
      begin
        if ((K + FRowColsStartDateOffset) < 6) or HideWeekEnd or not CompressWeekEnd then
          AddWeekDayHeader(AWeekDay,
            MulDiv(W, K, FRowCols) + ACell.Bounds.Left,
            MulDiv(W, K + 1, FRowCols) + ACell.Bounds.Left).Resource := ACell.Resource;
        Inc(K);
      end;
      Inc(I);
    end;
    ProcessDateToDisplayText(True);
  end;
end;

function TcxSchedulerWeeksViewViewInfo.GetView: TcxSchedulerWeeksView;
begin
  Result := TcxSchedulerWeeksView(inherited View);
end;

{ TcxSchedulerWeekViewPainter }

procedure TcxSchedulerWeekViewPainter.Paint;
begin
  inherited Paint;
  ViewInfo.ContentCells.Draw(Canvas, DrawContentCell);
  ViewInfo.DayHeaderCells.Draw(Canvas, DrawHeaderCell);
  ViewInfo.ResourceHeaderCells.Draw(Canvas, DrawResourceHeaderCell);
  ViewInfo.GroupSeparatorCells.Draw(Canvas, DrawGroupSeparatorCell);
  ViewInfo.EventCells.Draw(Canvas, DrawEventCell);
  ViewInfo.Buttons.Draw(Canvas, DrawButtonCell);
  ViewInfo.DrawNavigationButtons(Canvas, DrawButtonCell);
end;

function TcxSchedulerWeekViewPainter.GetViewInfo: TcxSchedulerWeekViewViewInfo;
begin
  Result := TcxSchedulerWeekViewViewInfo(inherited ViewInfo);
end;

end.

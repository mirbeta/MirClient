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

unit cxSchedulerYearView;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  UITypes,
{$ENDIF}
  Windows, Forms, Classes, SysUtils, Math, StdCtrls, Graphics,
  Controls, Menus, Types, DateUtils,
  dxCore, cxClasses, cxControls, cxGraphics, cxStyles, cxGeometry, cxLookAndFeelPainters,
  dxRangeControl, cxSchedulerCustomControls, cxSchedulerCustomResourceView,
  cxSchedulerUtils, cxSchedulerStorage, cxSchedulerStrs, cxDateUtils, cxSchedulerWeekView;

const
  cxMaxSelectedDaysCount         = 20;
  MaxYearViewWeekDays            = 37;
  cxYearViewScrollCurrentYearPos = 10;
  cxYearViewScrollMaxPos         = 21;
  cxYearViewScrollMinPos         = 0;

  cxcsMonthHeader                = 0;
  cxcsUnusedContent              = 1;
  cxcsMaxYearViewStyle           = cxcsUnusedContent;

  // HitTest
  htcYear                        = $11;
  htcMonth                       = $12;

type
  TcxSchedulerMonthHeaderCellViewInfo = class;
  TcxSchedulerMonthHeaderPopupMenu = class;
  TcxSchedulerYearView = class;
  TcxSchedulerYearViewViewInfo = class;
  TcxSchedulerYearViewNavigation = class;
  TcxSchedulerYearViewStyles = class;
  TcxSchedulerYearViewContentCellViewInfo = class;

  { TcxSchedulerYearViewController }

  TcxSchedulerYearViewController = class(TcxSchedulerCustomResourceViewController)
  protected
    function CreateDragEventHelper: TcxDragEventHelper; override;
    function CreateNavigation: TcxSchedulerViewNavigation; override;
    function CreateResizeEventHelper: TcxEventSizingHelper; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
  end;

  { TcxSchedulerYearViewHitTest }

  TcxSchedulerYearViewHitTest = class(TcxSchedulerCustomResourceViewHitTest)
  public
    property HitAtYear: Boolean index htcYear read GetBitState;
    property HitAtMonth: Boolean index htcMonth read GetBitState;
  end;

  { IcxSchedulerYearViewAdapter }

  IcxSchedulerYearViewAdapter = interface
  ['{7B39CB3C-256E-4269-9B24-5A59BC3D868D}']
    function GetMonthCount: Integer;
    function GetFirstMonth: Integer;
    function GetShowMonthHeaders: Boolean;
    function GetShowWeekDayHeaders: Boolean;
    function GetStartDayIndex: Integer;
    function GetDayCountPerPage: Integer;
  end;

  { TcxSchedulerYearViewPainter }

  TcxSchedulerYearViewPainter = class(TcxSchedulerCustomViewPainter)
   private
     function GetViewInfo: TcxSchedulerYearViewViewInfo;
   public
     procedure Paint; override;
     property ViewInfo: TcxSchedulerYearViewViewInfo read GetViewInfo;
  end;

  { TcxSchedulerYearViewEventCellViewInfo }

  TcxSchedulerYearViewEventCellViewInfo = class(TcxSchedulerEventCellViewInfo)
  protected
    procedure CalculateBorders; override;
    procedure CalculateCaptions; override;
    procedure CalculateEventTimeVisibility; override;
    function CanAutoHideStandardImages: Boolean; override;
    function GetForceShowClockInHeaderEvent: Boolean; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  end;

  { TcxSchedulerYearViewEventCellModernViewInfo }

  TcxSchedulerYearViewEventCellModernViewInfo = class(TcxSchedulerWeekViewEventCellModernViewInfo)
  protected
    function CanAutoHideStandardImages: Boolean; override;
    function GetContinueArrowCaptionDateFormat: string; override;
    function GetEditingRect: TRect; override;
    function GetForceShowClockInHeaderEvent: Boolean; override;
    function NeedHeaderEventFinishContinueArrow: Boolean; override;
  end;

  { TcxSchedulerYearViewViewInfo }

  TcxSchedulerYearViewViewInfo = class(TcxSchedulerCustomResourceViewViewInfo)
  private
    FAdapterFirstMonth: Integer;
    FAdapterMonthCount: Integer;
    function GetAdapterFirstMonth: Integer;
    function GetAdapterMonthCount: Integer;
    function GetDaysCell(AIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
    function GetDaysCellCount: Integer;
    function GetMonthBounds(AIndex: Integer): TRect;
    function GetYearView: TcxSchedulerYearView;
  protected
    FContentFont: TFont;
    FContentSmallFont: TFont;
    FDayCells: TList;
    FDayTextHeight: Integer;
    FMonthHeaderCells: TcxSchedulerViewInfoCellList;
    FMonthHeaderWidth: Integer;
    FShowMonthHeaders: Boolean;
    FStartDayIndex: Integer;
    FMaxYearViewWeekDays: Integer;
    FMaxEventsCount: Integer;
    FYear: Word;
    FYearViewExtraAdapter: IcxSchedulerYearViewAdapter;
    FRangeStart: Integer;
    FRangeFinish: Integer;
    procedure AddEventButtons(APlace: TcxSchedulerEventPlace);
    procedure AddEventForCalculation(ABuilder: TcxSchedulerEventLayoutBuilder;
      AEvent: TcxSchedulerControlEvent);
    procedure AddEventToYearView(APlace: TcxSchedulerEventPlace);
    function AddMonthHeader(AYear, AMonth: Integer; const ARect: TRect;
      AFirstMonth: Integer = 0;
      AMonthCount: Integer = 12): TcxSchedulerMonthHeaderCellViewInfo;
    function AddWeekDayHeader(AWeekDay: Integer;
      ARect: TRect): TcxSchedulerWeekDayHeaderCellViewInfo;
    function AddYearViewContentCell(ARect: TRect; ADate: TDate; ASelected: Boolean;
      AParams: TcxViewParams; AColIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
    procedure CalculateContentCellMonthParams(AMonth: Integer;
      out AMonthBounds: TRect; out AStartOfMonth: TDate;
      out AStartMonthColumn, ADaysPerMonth: Integer);
    procedure CalculateContentCells; virtual;
    procedure CalculateContentNavigationButtons; override;
    procedure CalculateDaysHeader; virtual;
    procedure CalculateEventsViewInfo; virtual;
    procedure CalculateMetrics; override;
    procedure CalculateMonthHeaderWidth;
    procedure CalculateMonthsHeader; virtual;
    procedure CheckLayout(ADate: TDateTime);
    procedure Clear; override;
    function ContentCellClass: TcxSchedulerContentCellViewInfoClass; override;
    function DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass; override;
    function EventCellClass: TcxSchedulerEventCellViewInfoClass; override;
    function MonthHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass; virtual;
    function GetPartOfTheYear(ADate: TDateTime): Word;
    procedure DoCalculate; override;
    procedure DoContentNavigationButtonClick(Sender: TcxSchedulerContentNavigationButtonViewInfo); override;
    function GetMonthColumnDate(AStartOfMonth: TDateTime;
      ADaysPerMonth, AStartMonthColumn, AColumn: Integer): TDateTime;
    function GetSmallFont(const AParams: TcxViewParams): TFont;
    function IsEventVisible(AEvent: TcxSchedulerControlEvent): Boolean;
    procedure PrepareSmallContentFont;

    // page splitting
    function GetFirstMonth: Integer; virtual;
    function GetMonthCountPerPage: Integer; virtual;

    property AdapterFirstMonth: Integer read GetAdapterFirstMonth write FAdapterFirstMonth;
    property AdapterMonthCount: Integer read GetAdapterMonthCount write FAdapterMonthCount;
    property FirstMonth: Integer read GetFirstMonth;
    property MonthCountPerPage: Integer read GetMonthCountPerPage;
  public
    constructor Create(AOwner: TcxSchedulerSubControl); override;
    destructor Destroy; override;
    procedure CalculateHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure UpdateSelection;

    property DaysCellCount: Integer read GetDaysCellCount;
    property DaysCells[AIndex: Integer]: TcxSchedulerYearViewContentCellViewInfo read GetDaysCell;
    property MonthBounds[AIndex: Integer]: TRect read GetMonthBounds;
    property MonthHeaderCells: TcxSchedulerViewInfoCellList read FMonthHeaderCells;
    property View: TcxSchedulerYearView read GetYearView;
    property Year: Word read FYear;
  end;

  { TcxSchedulerYearViewNavigation }

  TcxSchedulerYearViewNavigation = class(TcxSchedulerCustomResourceViewNavigation)
  private
    FCanChangeSelection: Boolean;
    FSaveSelFinish: TDateTime;
    FSaveSelStart: TDateTime;
    function GetYearView: TcxSchedulerYearView;
    function GetYearViewInfo: TcxSchedulerYearViewViewInfo;
  protected
    function ContentCell(AIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
    procedure CorrectCurrentAnchor;
    procedure DoKeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure GotoCornerCell(AGotoEnd: Boolean); virtual;
    procedure GotoNextCellHorz(AGotoNext: Boolean); virtual;
    procedure GotoNextCellVert(AGoForward: Boolean); virtual;
    procedure GotoNextPage(AGotoForward: Boolean); virtual;
  public
    procedure KeyDown(var AKey: Word; AShift: TShiftState); override;
    procedure ValidateSelection(var ASelStart, ASelFinish: TDateTime;
      var AResource: TcxSchedulerStorageResourceItem); override;

    property View: TcxSchedulerYearView read GetYearView;
    property ViewInfo: TcxSchedulerYearViewViewInfo read GetYearViewInfo;
  end;

  { TcxSchedulerMonthHeaderCellViewInfo }

  TcxSchedulerMonthHeaderCellViewInfo = class(TcxSchedulerDayHeaderCellViewInfo)
  private
    FFirstMonth: Integer;
    FMonth: Integer;
    FMonthCount: Integer;
    FYear: Integer;
    procedure SetFirstMonth(AValue: Integer);
    procedure SetMonth(AValue: Integer);
    procedure SetMonthCount(AValue: Integer);
    procedure SetYear(AValue: Integer);
  protected
    FColStart: Integer;
    procedure CalculateDisplayText; virtual;
    procedure DoDraw; override;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
    procedure Initialize(AYear, AMonth, AFirstMonth, AMonthCount: Integer; ARotateHeader: Boolean);
  public
    property FirstMonth: Integer read FFirstMonth write SetFirstMonth;
    property Month: Integer read FMonth write SetMonth;
    property MonthCount: Integer read FMonthCount write SetMonthCount;
    property Year: Integer read FYear write SetYear;
  end;

  { TcxSchedulerMonthHeaderCellModernViewInfo }

  TcxSchedulerMonthHeaderCellModernViewInfo = class(TcxSchedulerMonthHeaderCellViewInfo)
  protected
    function DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean; override;
    procedure DrawHorizontalHeader; override;
    procedure DrawVerticalHeader; override;
  end;

  { TcxSchedulerYearViewContentCellViewInfo }

  TcxSchedulerYearViewContentCellViewInfo = class(TcxSchedulerMonthDayContentCellViewInfo)
  protected
    FColIndex: Integer;
    procedure InitHitTest(AHitTest: TcxSchedulerCustomResourceViewHitTest); override;
  end;

  { TcxSchedulerYearViewContentCellModernViewInfo }

  TcxSchedulerYearViewContentCellModernViewInfo = class(TcxSchedulerYearViewContentCellViewInfo)
  protected
    procedure DoDrawBackground; override;
    function GetDefaultBorderColor: TColor; override;
    function GetDisplayText: string; override;
    function GetTextFlags: Cardinal; override;
    function GetTextLeft: Integer; override;
    function GetTextLeftForSmallFont: Integer; override;
    function SaveFontStyle(AFont: TFont): TFontStyles; override;
  end;

  { TcxYearViewDragEventHelper }

  TcxYearViewDragEventHelper = class(TcxDragEventHelper)
  protected
    procedure UpdateViewClonesTime; override;
  end;

  { TcxYearViewEventSizing }

  TcxYearViewEventSizing = class(TcxEventSizingHelper)
  protected
    procedure UpdateEventBounds; override;
  end;

  { TcxSchedulerYearViewStyles }

  TcxSchedulerYearViewGetUnusedContentStyleEvent = procedure(Sender: TcxSchedulerYearView;
    AYear, AMonth, ADayOfWeek: Integer; var AStyle: TcxStyle) of object;
  TcxSchedulerYearViewGetMonthHeaderStyleEvent = procedure(Sender: TcxSchedulerYearView;
    AYear, AMonth: Integer; var AStyle: TcxStyle) of object;

  TcxSchedulerYearViewStyles = class(TcxStyles)
  private
    FScheduler: TcxCustomScheduler;
    FYearView: TcxSchedulerYearView;
    FOnGetMonthHeaderStyle: TcxSchedulerYearViewGetMonthHeaderStyleEvent;
    FOnGetUnusedContentStyle: TcxSchedulerYearViewGetUnusedContentStyleEvent;
    function GetPainter: TcxCustomLookAndFeelPainter;
  protected
    procedure Changed(AIndex: Integer); override;
    procedure GetDefaultViewParams(Index: Integer; AData: TObject;
      out AParams: TcxViewParams); override;
    property Painter: TcxCustomLookAndFeelPainter read GetPainter;
  public
    constructor Create(AOwner: TPersistent); override;
    procedure Assign(Source: TPersistent); override;
    function GetMonthHeaderParams(AYear, AMonth: Integer): TcxViewParams;
    function GetUnusedContentParams(
      AYear, AMonth, ADayOfWeek: Integer): TcxViewParams;

    property Scheduler: TcxCustomScheduler read FScheduler;
    property YearView: TcxSchedulerYearView read FYearView;
  published
    property MonthHeader: TcxStyle index cxcsMonthHeader read GetValue write SetValue;
    property UnusedContent: TcxStyle index cxcsUnusedContent read GetValue write SetValue;
    property OnGetMonthHeaderStyle: TcxSchedulerYearViewGetMonthHeaderStyleEvent read FOnGetMonthHeaderStyle write FOnGetMonthHeaderStyle;
    property OnGetUnusedContentStyle: TcxSchedulerYearViewGetUnusedContentStyleEvent read FOnGetUnusedContentStyle write FOnGetUnusedContentStyle;
  end;

  { TcxSchedulerYearView }

  TcxSchedulerYearView = class(TcxSchedulerCustomResourceView)
  private
    FAllDayEventsOnly: Boolean;
    FMaxSelectedDaysCount: Integer;
    FMonthHeaderPopupMenu: TcxSchedulerMonthHeaderPopupMenu;
    FStyles: TcxSchedulerYearViewStyles;
    FScale: Integer;
    function GetHitTest: TcxSchedulerYearViewHitTest;
    function GetNavigation: TcxSchedulerYearViewNavigation;
    function GetViewInfo: TcxSchedulerYearViewViewInfo;
    function GetYear: Word;
    procedure SetAllDayEventsOnly(AValue: Boolean);
    procedure SetMaxSelectedDaysCount(AValue: Integer);
    procedure SetMonthHeaderPopupMenu(AValue: TcxSchedulerMonthHeaderPopupMenu);
    procedure SetScale(AValue: Integer);
    procedure SetStyles(AValue: TcxSchedulerYearViewStyles);
  protected
    procedure CreateSubClasses; override;
    function CreateViewAdapter: TcxCustomResourceViewAdapter; override;
    procedure DestroySubClasses; override;
    function CanDeactivateOnDateNavigatorSelectionChange: Boolean; override;
    function CanSelectPeriod: Boolean; override;
    procedure ChangeLayout(ACurrentAnchor: TDateTime);
    function CreateController: TcxSchedulerSubControlController; override;
    function CreateHitTest: TcxSchedulerSubControlHitTest; override;
    function CreateMonthHeaderPopupMenu: TcxSchedulerMonthHeaderPopupMenu; virtual;
    function CreatePainter: TcxSchedulerSubControlPainter; override;
    function CreateStyles: TcxSchedulerYearViewStyles; virtual;
    function CreateViewInfo: TcxSchedulerSubControlViewInfo; override;
    function DoShowPopupMenu(X, Y: Integer): Boolean; override;
    function GetCompressWeekEnd: Boolean; override;
    function GetGroupingKind: TcxSchedulerGroupingKind; override;
    function GetFirstVisibleDate: TDateTime; override;
    function GetLastVisibleDate: TDateTime; override;
    procedure GetRangeControlRange(out AMin, AMax: TDateTime); override;
    procedure GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList); override;
    procedure GetRangeControlTotalRange(out AMin, AMax: TDateTime); override;
    function GetScrollPos: Integer; virtual;
    function GetVisibleDaysRange: Integer; override;
    procedure InitScrollBarsParameters; override;
    procedure MakeEventVisible(AEvent: TcxSchedulerControlEvent;
      const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Scroll(AScrollBarKind: TScrollBarKind;
      AScrollCode: TScrollCode; var AScrollPos: Integer); override;
    procedure SelectedDaysChanged; override;
    procedure SetGroupingKind(AValue: TcxSchedulerGroupingKind); override;

    property Navigation: TcxSchedulerYearViewNavigation read GetNavigation;
    property ViewInfo: TcxSchedulerYearViewViewInfo read GetViewInfo;
  public
    constructor Create(AOwner: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;

    property HitTest: TcxSchedulerYearViewHitTest read GetHitTest;
    property Year: Word read GetYear;
  published
    property Active;
    property AllDayEventsOnly: Boolean read FAllDayEventsOnly write SetAllDayEventsOnly default False;
    property MaxSelectedDaysCount: Integer read FMaxSelectedDaysCount write SetMaxSelectedDaysCount default 0;
    property MonthHeaderPopupMenu: TcxSchedulerMonthHeaderPopupMenu read FMonthHeaderPopupMenu write SetMonthHeaderPopupMenu;
    property Scale: Integer read FScale write SetScale default 12;
    property Styles: TcxSchedulerYearViewStyles read FStyles write SetStyles;
  end;

  { TcxSchedulerYearViewAdapter }

  TcxSchedulerYearViewAdapter = class(TcxCustomResourceViewAdapter, IcxSchedulerYearViewAdapter)
  private
    FPredefinedCurrentAnchor: TDateTime;
  protected
    { IUnknown }
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    {  IcxSchedulerYearViewAdapter }
    function GetDayCountPerPage: Integer; virtual;
    function GetFirstMonth: Integer; virtual;
    function GetMonthCount: Integer; virtual;
    function GetShowMonthHeaders: Boolean; virtual;
    function GetShowWeekDayHeaders: Boolean; virtual;
    function GetStartDayIndex: Integer; virtual;

    property PredefinedCurrentAnchor: TDateTime read FPredefinedCurrentAnchor write FPredefinedCurrentAnchor;
  end;

  { TcxSchedulerMonthHeaderPopupMenu }

  TcxSchedulerMonthHeaderPopupMenuItem = (mhpmiNewEvent, mhpmiNewAllDayEvent,
    mhpmiNewReccuringEvent, mhpmiFullYear, mhpmiHalfYear,  mhpmiQuarter);
  TcxSchedulerMonthHeaderPopupMenuItems = set of TcxSchedulerMonthHeaderPopupMenuItem;

  TcxSchedulerMonthHeaderPopupMenuPopupEvent = procedure (
    Sender: TcxSchedulerMonthHeaderPopupMenu; ABuiltInMenu: TPopupMenu;
    var AHandled: Boolean) of object;
  TcxSchedulerMonthHeaderPopupMenuClickEvent = procedure (
    Sender: TcxSchedulerMonthHeaderPopupMenu;
    AItem: TcxSchedulerMonthHeaderPopupMenuItem; var AHandled: Boolean) of object;

  TcxSchedulerMonthHeaderPopupMenu = class(TcxSchedulerCustomContentPopupMenu)
  private
    FItems: TcxSchedulerMonthHeaderPopupMenuItems;
    FOnPopup: TcxSchedulerMonthHeaderPopupMenuPopupEvent;
    FOnClick: TcxSchedulerMonthHeaderPopupMenuClickEvent;
    function GetYearView: TcxSchedulerYearView;
  protected
    procedure CreateItems; override;
    procedure DoExecute(ACommand: Integer); override;
    function DoOnClick(ACommand: Integer): Boolean; override;
    function DoOnPopup: Boolean; override;
    function IsValidCommand(ACommand: Integer): Boolean; override;

    property YearView: TcxSchedulerYearView read GetYearView;
  public
    constructor Create(AScheduler: TcxCustomScheduler); override;
    procedure Assign(Source: TPersistent); override;
    procedure Execute(AItem: TcxSchedulerMonthHeaderPopupMenuItem);
    function GetMenuItem(AItem: TcxSchedulerMonthHeaderPopupMenuItem): TMenuItem;
  published
    property Items: TcxSchedulerMonthHeaderPopupMenuItems
      read FItems write FItems default [mhpmiNewEvent, mhpmiNewAllDayEvent,
        mhpmiNewReccuringEvent, mhpmiFullYear, mhpmiHalfYear, mhpmiQuarter];
    property PopupMenu;
    property UseBuiltInPopupMenu;

    property OnClick: TcxSchedulerMonthHeaderPopupMenuClickEvent read FOnClick write FOnClick;
    property OnPopup: TcxSchedulerMonthHeaderPopupMenuPopupEvent read FOnPopup write FOnPopup;
  end;

implementation

uses
  cxFormats, cxDrawTextUtils;

const
  DayInc: array[Boolean] of Integer = (-1, 1);
  YearScales: array[0..2] of Integer = (12, 6, 3);

type
  TcxSchedulerCustomViewInfoItemAccess = class(TcxSchedulerCustomViewInfoItem);
  TcxSchedulerCustomViewViewInfoAccess = class(TcxSchedulerCustomViewViewInfo);
  TcxCustomSchedulerAccess = class(TcxCustomScheduler);

{ TcxSchedulerYearViewController }

function TcxSchedulerYearViewController.CreateDragEventHelper: TcxDragEventHelper;
begin
  Result := TcxYearViewDragEventHelper.Create(Scheduler);
end;

function TcxSchedulerYearViewController.CreateNavigation: TcxSchedulerViewNavigation;
begin
  Result := TcxSchedulerYearViewNavigation.Create(View);
end;

function TcxSchedulerYearViewController.CreateResizeEventHelper: TcxEventSizingHelper;
begin
  Result := TcxYearViewEventSizing.Create(Scheduler);
end;

procedure TcxSchedulerYearViewController.KeyDown(var Key: Word; Shift: TShiftState);
begin
  if View.Active and not EditController.IsEditing and
    TcxSchedulerYearViewNavigation(Navigation).IsKeyNavigation(Key, Shift) then
    TcxSchedulerYearViewNavigation(Navigation).DoKeyDown(Key, Shift)
  else
    inherited KeyDown(Key, Shift);
end;

{ TcxSchedulerYearViewPainter }

procedure TcxSchedulerYearViewPainter.Paint;
begin
  inherited Paint;
  ViewInfo.MonthHeaderCells.Draw(Canvas, DrawHeaderCell);
  ViewInfo.DayHeaderCells.Draw(Canvas, DrawHeaderCell);
  ViewInfo.ContentCells.Draw(Canvas, DrawContentCell);
  ViewInfo.EventCells.Draw(Canvas, DrawEventCell);
  ViewInfo.Buttons.Draw(Canvas, DrawButtonCell);
  ViewInfo.DrawNavigationButtons(Canvas, DrawButtonCell);
end;

function TcxSchedulerYearViewPainter.GetViewInfo: TcxSchedulerYearViewViewInfo;
begin
  Result := TcxSchedulerYearViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerYearViewEventCellViewInfo }

procedure TcxSchedulerYearViewEventCellViewInfo.CalculateBorders;
begin
  if ViewData.ContentFinish < EventFinish then
    Exclude(FBorders, bRight);
  if ViewData.ContentStart > EventStart then
    Exclude(FBorders, bLeft);
end;

procedure TcxSchedulerYearViewEventCellViewInfo.CalculateCaptions;
begin
end;

procedure TcxSchedulerYearViewEventCellViewInfo.CalculateEventTimeVisibility;
var
  B: Boolean;
begin
  B := IsHeaderEvent and not Event.AllDayEvent;
  ViewData.ShowStartTime := B and (dxTimeOf(Event.Start) <> 0) and
    (EventStart > ViewData.ContentStart);
  ViewData.ShowFinishTime := B and (dxTimeOf(Event.Finish) <> 0) and
    (EventFinish < ViewData.ContentFinish);
end;

function TcxSchedulerYearViewEventCellViewInfo.CanAutoHideStandardImages: Boolean;
begin
  Result := True;
end;

function TcxSchedulerYearViewEventCellViewInfo.GetForceShowClockInHeaderEvent: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerYearViewEventCellViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  inherited InitHitTest(AHitTest);
  InitHitTestForHorzEvent(AHitTest);
end;

{ TcxSchedulerYearViewEventCellModernViewInfo }

function TcxSchedulerYearViewEventCellModernViewInfo.CanAutoHideStandardImages: Boolean;
begin
  Result := True;
end;

function TcxSchedulerYearViewEventCellModernViewInfo.GetContinueArrowCaptionDateFormat: string;
begin
  Result := 'd mmm yyyy';
end;

function TcxSchedulerYearViewEventCellModernViewInfo.GetEditingRect: TRect;
begin
  Result := inherited GetEditingRect;
  Dec(Result.Right);
end;

function TcxSchedulerYearViewEventCellModernViewInfo.GetForceShowClockInHeaderEvent: Boolean;
begin
  Result := False;
end;

function TcxSchedulerYearViewEventCellModernViewInfo.NeedHeaderEventFinishContinueArrow: Boolean;
begin
  Result := (ContentFinish = ViewData.ViewContentFinish) and
    (EventFinish > ContentFinish) and (EventFinish > ViewData.ViewContentFinish);
end;

{ TcxSchedulerYearViewViewInfo }

constructor TcxSchedulerYearViewViewInfo.Create(
  AOwner: TcxSchedulerSubControl);
begin
  inherited Create(AOwner);
  FContentSmallFont := TFont.Create;
  FDayCells := TList.Create;
  FDayCells.Capacity := 444;
  FMonthHeaderCells := TcxSchedulerViewInfoCellList.Create;
  FCells.Add(FMonthHeaderCells);
end;

destructor TcxSchedulerYearViewViewInfo.Destroy;
begin
  FContentSmallFont.Free;
  FDayCells.Free;
  inherited Destroy;
end;

procedure TcxSchedulerYearViewViewInfo.CalculateHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  if not MonthHeaderCells.CalculateHitTest(AHitTest) then
    inherited CalculateHitTest(AHitTest);
end;

procedure TcxSchedulerYearViewViewInfo.UpdateSelection;
var
  I: Integer;
  ACell: TcxSchedulerMonthDayContentCellViewInfo;
begin
  with View.Navigation do
  begin
    Self.FSelStart := DateTimeHelper.RoundTime(Min(SelStart, SelFinish));
    Self.FSelFinish := DateTimeHelper.RoundTime(Max(SelStart, SelFinish));
  end;
  for I := 0 to ContentCells.Count - 1 do
  begin
    ACell := TcxSchedulerMonthDayContentCellViewInfo(ContentCells[I]);
    if (ACell.TimeStart <> NullDate) and ACell.UpdateSelection(not HideSelection and
      (ACell.TimeStart >= SelStart) and (ACell.TimeStart <= SelFinish)) then
      View.InvalidateRect(ACell.Bounds);
  end;
end;

procedure TcxSchedulerYearViewViewInfo.AddEventButtons(
  APlace: TcxSchedulerEventPlace);
var
  I: Integer;
begin
  for I := APlace.ColStart to APlace.ColFinish do
    if DaysCells[I].LineCount = 0 then
    begin
      AddButton(DaysCells[I].Bounds, DaysCells[I].TimeStart, True, APlace.Event);
      DaysCells[I].LineCount := 1;
    end;
end;

procedure TcxSchedulerYearViewViewInfo.AddEventForCalculation(
  ABuilder: TcxSchedulerEventLayoutBuilder; AEvent: TcxSchedulerControlEvent);
var
  AStart, AFinish: TDateTime;
  AOffset, I, AStartIndex, AFinishIndex: Integer;
  AStartDay: TcxSchedulerYearViewContentCellViewInfo;
begin
  if (FYearViewExtraAdapter = nil) then
  begin
    AStart := Max(FRangeStart, AEvent.Start);
    AFinish := Min(FRangeFinish, AEvent.Finish);
    AOffset := DayOfTheYear(EncodeDate(Year, FirstMonth, 1));
    I := DayOfTheYear(AStart) - AOffset;
    AFinishIndex := Max(DayOfTheYear(AFinish) - 1 - AOffset, I);
    while (AFinishIndex < (DaysCellCount - 1)) and
      AEvent.IsDayEvent(Trunc(DaysCells[AFinishIndex + 1].TimeStart)) do Inc(AFinishIndex);
  end
  else
  begin
    I := 0;
    while (I < DaysCellCount) and not AEvent.IsDayEvent(Trunc(DaysCells[I].TimeStart)) do
      Inc(I);
    if I = DaysCellCount then Exit;
    AFinishIndex := I;
    while (AFinishIndex < DaysCellCount) and
      AEvent.IsDayEvent(Trunc(DaysCells[AFinishIndex].TimeStart)) do Inc(AFinishIndex);
    Dec(AFinishIndex);
  end;
  while I <= AFinishIndex do
  begin
    AStartIndex := I;
    AStartDay := DaysCells[I];
    while (I <= AFinishIndex) and (AStartDay.Bounds.Top = DaysCells[I].Bounds.Top) do
      Inc(I);
    ABuilder.AddEventPlace(AEvent, AStartIndex, I - 1, 1, TObject(MonthOf(AStartDay.DateTime)));
  end;
end;

procedure TcxSchedulerYearViewViewInfo.AddEventToYearView(
  APlace: TcxSchedulerEventPlace);
var
  ABounds: TRect;
  AViewData: TcxSchedulerEventViewData;
  AStart, AFinish: TDateTime;
begin
  ABounds := MonthBounds[Integer(APlace.Resource)];
  with DaysCells[APlace.ColStart] do
  begin
    AStart := TimeStart;
    ABounds.Left := Bounds.Left + cxTextOffset;
  end;
  with DaysCells[APlace.ColFinish] do
  begin
    AFinish := TimeFinish;
    ABounds.Right := Bounds.Right - cxTextOffset - 1;
  end;
  ABounds.Top := ABounds.Top + APlace.LineStart * (ContentLineHeight + cxTextOffset);
  ABounds.Bottom := ABounds.Top + ContentLineHeight;
  OffsetRect(ABounds, 0, FDayTextHeight);
  AViewData := CreateEventViewData(TcxSchedulerControlEvent(APlace.Event),
    ABounds, AStart, AFinish, nil);
  AViewData.VisibleRect := MonthBounds[Integer(APlace.Resource)];
  AViewData.ViewContentStart := DaysCells[0].TimeStart;
  AViewData.ViewContentFinish := DaysCells[DaysCellCount - 1].TimeFinish;
  Dec(AViewData.VisibleRect.Bottom, cxTextOffset);
  if AViewData.Bounds.Bottom > AViewData.VisibleRect.Bottom then
  begin
    //todo: lcm if not AViewData.Event.IsClone then
      AddEventButtons(APlace);
    AViewData.Free
  end
  else
    AddEventCell(AViewData);
end;

function TcxSchedulerYearViewViewInfo.AddMonthHeader(AYear, AMonth: Integer;
  const ARect: TRect; AFirstMonth, AMonthCount: Integer): TcxSchedulerMonthHeaderCellViewInfo;
var
  AParams: TcxViewParams;
begin
  AParams := View.Styles.GetMonthHeaderParams(AYear, AMonth);
  CreateCellInstance(MonthHeaderClass, ARect, AParams, UseRightToLeftAlignment, Result);
  Result.Initialize(AYear, AMonth, AFirstMonth, AMonthCount, True);
  MonthHeaderCells.Add(Result);
end;

function TcxSchedulerYearViewViewInfo.AddWeekDayHeader(AWeekDay: Integer;
  ARect: TRect): TcxSchedulerWeekDayHeaderCellViewInfo;
begin
  Inc(AWeekDay, Integer(View.StartOfWeek));
  if AWeekDay >= 7 then Dec(AWeekDay, 7);
  CreateCellInstance(DayHeaderClass, ARect, StylesAdapter.GetDayHeaderParams(AWeekDay), UseRightToLeftAlignment, Result);
  Result.DateTime := AWeekDay;
  DayHeaderCells.Add(Result);
end;

function TcxSchedulerYearViewViewInfo.AddYearViewContentCell(
  ARect: TRect; ADate: TDate; ASelected: Boolean;
  AParams: TcxViewParams; AColIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
var
  AColor: TColor;
begin
  CreateCellInstance(ContentCellClass, ARect, AParams, UseRightToLeftAlignment, Result);
  Result.FSmallFont := True;
  Result.SmallTextFont := GetSmallFont(AParams);
  if ADate <> NullDate then
  begin
    Result.SetTime(ADate, ADate + 1);
    Result.SetContentState(True, ASelected, FDayTextHeight - cxTextOffset, FSelectionParams);
    AColor := StylesAdapter.GetContentParams(ADate, True, nil).Color;
    Result.Color := PainterHelper.GetContentColor(AColor,
      View.IsWorkTime(nil, ADate + View.WorkStart), ViewStyle);
    FDayCells.Add(Result);
  end;
  Result.LineCount := 0;
  Result.FColIndex := AColIndex;
  ContentCells.Add(Result);
end;

procedure TcxSchedulerYearViewViewInfo.CalculateContentCellMonthParams(
  AMonth: Integer; out AMonthBounds: TRect; out AStartOfMonth: TDate;
  out AStartMonthColumn, ADaysPerMonth: Integer);
begin
  AMonthBounds := FMonthHeaderCells[AMonth + 1].Bounds;
  AStartOfMonth := EncodeDate(Year, AMonth + FirstMonth, 1);
  AStartMonthColumn := DayOfWeek(AStartOfMonth) - 1;
  Dec(AStartMonthColumn, Integer(View.GetStartOfWeek));
  if AStartMonthColumn < 0 then
    Inc(AStartMonthColumn, 7);
  ADaysPerMonth := DaysPerMonth(Year, AMonth + FirstMonth);
  TcxSchedulerMonthHeaderCellViewInfo(FMonthHeaderCells[AMonth + 1]).FColStart := AStartMonthColumn;
end;

procedure TcxSchedulerYearViewViewInfo.CalculateContentCells;
var
  I, J: Integer;
  R, DayBounds, MonthBounds: TRect;
  AStartOfMonth, ADate: TDate;
  ACell: TcxSchedulerYearViewContentCellViewInfo;
  AStartMonthColumn, ADaysPerMonth: Integer;
  ASelected: Boolean;
  AParams: TcxViewParams;
begin
  FMaxEventsCount := 0;
  for I := 0 to MonthCountPerPage - 1 do
  begin
    CalculateContentCellMonthParams(I, MonthBounds, AStartOfMonth,
      AStartMonthColumn, ADaysPerMonth);
    for J := 0 to FMaxYearViewWeekDays - 1 do
    begin
      DayBounds := DayHeaderCells[J].Bounds;
      R := Rect(DayBounds.Left, MonthBounds.Top,
        DayBounds.Right, MonthBounds.Bottom);
      ADate := GetMonthColumnDate(AStartOfMonth, ADaysPerMonth,
        AStartMonthColumn, J + FStartDayIndex);
      ASelected := not HideSelection and
        (ADate >= SelStart) and (ADate <= SelFinish);
      if ADate = NullDate then
        AParams := View.Styles.GetUnusedContentParams(FYear, (I + FirstMonth),
          (Byte(View.OptionsView.ActualStartOfWeek) + J + FStartDayIndex) mod 7)
      else
        AParams := GetContentParams(ADate, nil);
      ACell := AddYearViewContentCell(R, ADate, ASelected, AParams, J);
      FMaxEventsCount := Max(FMaxEventsCount, (R.Bottom - R.Top) div FContentLineHeight + 1);
      if I < MonthCountPerPage then
        ACell.Borders := [bBottom, bRight]
      else
        ACell.Borders := [bRight];
    end;
  end;
end;

procedure TcxSchedulerYearViewViewInfo.CalculateContentNavigationButtons;
var
  ABounds: TRect;
begin
  if not CanCalculateNavigationButtons then Exit;

  ABounds := Bounds;
  ABounds.Left := ABounds.Left + FMonthHeaderWidth;
  if IsValidNavigationButtonsPlace(ABounds) then
    AddContentNavigationButton(ABounds, -1, cprSingle);
end;

procedure TcxSchedulerYearViewViewInfo.CalculateDaysHeader;
var
  R: TRect;
  I, W: Integer;
begin
  W := Bounds.Right - Bounds.Left - FMonthHeaderWidth + 1;
  R := cxRectSetHeight(Bounds, FDayHeaderHeight);
  for I := 0 to FMaxYearViewWeekDays - 1 do
  begin
    R.Left := Bounds.Left + FMonthHeaderWidth + MulDiv(W, I, FMaxYearViewWeekDays);
    R.Right := Bounds.Left + FMonthHeaderWidth + MulDiv(W, I + 1, FMaxYearViewWeekDays);
    AddWeekDayHeader(((I + FStartDayIndex) mod 7), R);
  end;
  ProcessCheckBorders(DayHeaderCells, True, [nLeft]);
  ProcessDateToDisplayText(True);
end;

procedure TcxSchedulerYearViewViewInfo.CalculateEventsViewInfo;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
  ABuilder: TcxSchedulerEventLayoutBuilder;
begin
  ABuilder := TcxSchedulerEventLayoutBuilder.Create;
  try
    for I := 0 to Events.AbsoluteCount - 1 do
    begin
      AEvent := Events.AbsoluteItems[I];
      if IsEventVisible(AEvent) then
        AddEventForCalculation(ABuilder, AEvent);
    end;
    ABuilder.Calculate;
    for I := 0 to DaysCellCount - 1 do
      DaysCells[I].LineCount := 0;
    for I := ABuilder.EventPlaceCount - 1 downto 0 do
      AddEventToYearView(ABuilder.EventPlaces[I]);
  finally
    ABuilder.Free;
  end;
end;

procedure TcxSchedulerYearViewViewInfo.CalculateMetrics;
begin
  inherited CalculateMetrics;
  if FYearViewExtraAdapter = nil then
    FYear := View.Year
  else
    FYear := YearOf(SelectedDays[0]);
  FDayTextHeight := Round(FContentFontHeight * 2 / 3);
end;

procedure TcxSchedulerYearViewViewInfo.CalculateMonthHeaderWidth;
var
  I: Integer;
  AFont: TFont;
  AStyle: TcxStyle;
begin
  if not FShowMonthHeaders then
  begin
    FMonthHeaderWidth := 0;
    Exit;
  end;
  AStyle := View.Styles.MonthHeader;
  if (AStyle <> nil) and (cxStyles.svFont in AStyle.AssignedValues) then
    AFont := AStyle.Font
  else
    AFont := DefaultFont;
  FMonthHeaderWidth := cxTextWidth(AFont, 'WWWW') + 4 * cxTextOffset;
  for I := 1 to 12 do
    FMonthHeaderWidth := Max(FMonthHeaderWidth,
      cxTextWidth(AFont, dxFormatSettings.LongMonthNames[I]) + 4 * cxTextOffset);
end;

procedure TcxSchedulerYearViewViewInfo.CalculateMonthsHeader;
var
  R: TRect;
  I, H: Integer;
begin
  CalculateMonthHeaderWidth;
  H := Bounds.Bottom - Bounds.Top - FDayHeaderHeight;
  R := cxRectSetHeight(cxRectSetWidth(Bounds, FMonthHeaderWidth),
    FDayHeaderHeight);
  AddMonthHeader(FYear, 0, R, AdapterFirstMonth, AdapterMonthCount).RotateHeader := False;
  for I := 0 to MonthCountPerPage - 1 do
  begin
    R.Top := MulDiv(H, I, MonthCountPerPage) + FDayHeaderHeight;
    R.Bottom := MulDiv(H, I + 1, MonthCountPerPage) + FDayHeaderHeight;
    AddMonthHeader(FYear, I + FirstMonth, R);
  end;
  ProcessCheckBorders(MonthHeaderCells, True, [], []);
end;

procedure TcxSchedulerYearViewViewInfo.CheckLayout(ADate: TDateTime);
begin
  if (ADate < Trunc(TcxSchedulerCustomViewInfoItemAccess(FDayCells[0]).FDateTime)) or
    (ADate > Trunc(TcxSchedulerCustomViewInfoItemAccess(FDayCells[FDayCells.Count - 1]).FDateTime)) then
  begin
    TcxCustomSchedulerAccess(Scheduler).ShowTouchScrollUI(Scheduler, True);
    View.Changed;
    View.ChangeLayout(ADate);
  end;
end;

procedure TcxSchedulerYearViewViewInfo.Clear;
begin
  FDayCells.Clear;
  FMonthHeaderCells.Clear;
  inherited Clear;
end;

function TcxSchedulerYearViewViewInfo.ContentCellClass: TcxSchedulerContentCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerContentCellViewInfoClass =
    (TcxSchedulerYearViewContentCellViewInfo, TcxSchedulerYearViewContentCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerYearViewViewInfo.DayHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerDayHeaderCellViewInfoClass =
    (TcxSchedulerWeekDayHeaderCellViewInfo, TcxSchedulerWeekDayHeaderCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerYearViewViewInfo.EventCellClass: TcxSchedulerEventCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerEventCellViewInfoClass =
    (TcxSchedulerYearViewEventCellViewInfo, TcxSchedulerYearViewEventCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerYearViewViewInfo.MonthHeaderClass: TcxSchedulerDayHeaderCellViewInfoClass;
const
  AClass: array[Boolean] of TcxSchedulerDayHeaderCellViewInfoClass =
    (TcxSchedulerMonthHeaderCellViewInfo, TcxSchedulerMonthHeaderCellModernViewInfo);
begin
  Result := AClass[ViewStyle = svsModern];
end;

function TcxSchedulerYearViewViewInfo.GetPartOfTheYear(ADate: TDateTime): Word;
var
  AYear, AMonth, ADay: Word;
begin
  DecodeDate(ADate, AYear, AMonth, ADay);
  Result := 0;
  while AMonth >= (Result + 1) * AdapterMonthCount + 1 do
    Inc(Result);
end;

procedure TcxSchedulerYearViewViewInfo.DoCalculate;
var
  ADate: TDateTime;
begin
  ADate := SelectedDays[0];
  SelectedDays.Clear;
  SelectedDays.Add(ADate);
  PrepareSmallContentFont;
  FStartDayIndex := 0;
  FShowMonthHeaders := True;
  FMaxYearViewWeekDays := MaxYearViewWeekDays;
  Supports(TObject(Adapter), IcxSchedulerYearViewAdapter, FYearViewExtraAdapter);
  try
    inherited DoCalculate;
    if FYearViewExtraAdapter <> nil then
    begin
      if not FYearViewExtraAdapter.GetShowWeekDayHeaders then
        FDayHeaderHeight := 0;
      FShowMonthHeaders := FYearViewExtraAdapter.GetShowMonthHeaders;
      FStartDayIndex := FYearViewExtraAdapter.GetStartDayIndex;
      FMaxYearViewWeekDays := FYearViewExtraAdapter.GetDayCountPerPage;
      FAdapterFirstMonth := FYearViewExtraAdapter.GetFirstMonth;
      FAdapterMonthCount := FYearViewExtraAdapter.GetMonthCount;
    end;
    FRangeStart := Trunc(EncodeDate(Year, FirstMonth, 1));
    FRangeFinish := Trunc(EndOfTheMonth(EncodeDate(Year,
      FirstMonth + GetMonthCountPerPage - 1, 1)));

    CalculateMonthsHeader;
    CalculateDaysHeader;
    CalculateContentCells;
    CalculateEventsViewInfo;
    CalculateContentNavigationButtons;
  finally
    FYearViewExtraAdapter := nil;
  end;
end;

procedure TcxSchedulerYearViewViewInfo.DoContentNavigationButtonClick(
  Sender: TcxSchedulerContentNavigationButtonViewInfo);
var
  ARequiredDay: TDateTime;
begin
  ARequiredDay := SelectedDays[0] + Sender.Interval;
  View.Navigation.FCurrentAnchor := ARequiredDay;
  CheckLayout(ARequiredDay);
end;

function TcxSchedulerYearViewViewInfo.GetMonthColumnDate(
  AStartOfMonth: TDateTime;
  ADaysPerMonth, AStartMonthColumn, AColumn: Integer): TDateTime;
begin
  Result := NullDate;
  Dec(AColumn, AStartMonthColumn);
  if (AColumn >= 0) and (AColumn < ADaysPerMonth) then
    Result := AStartOfMonth + AColumn;
end;

function TcxSchedulerYearViewViewInfo.GetSmallFont(const AParams: TcxViewParams): TFont;
begin
  if AParams.Font = FContentFont then
    Result := FContentSmallFont
  else
    Result := nil;
end;

function TcxSchedulerYearViewViewInfo.GetFirstMonth: Integer;
begin
  if FYearViewExtraAdapter = nil then
    Result := 1
  else
    Result := FYearViewExtraAdapter.GetFirstMonth;
end;

function TcxSchedulerYearViewViewInfo.GetMonthCountPerPage: Integer;
begin
  if FYearViewExtraAdapter = nil then
    Result := 12
  else
    Result := FYearViewExtraAdapter.GetMonthCount;
end;

function TcxSchedulerYearViewViewInfo.IsEventVisible(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := not View.AllDayEventsOnly or AEvent.IsAllDayOrLonger;
  if Result then
    Result := (dxDateOf(AEvent.Start) <= FRangeFinish) and
    (AEvent.Finish >= FRangeStart);
end;

procedure TcxSchedulerYearViewViewInfo.PrepareSmallContentFont;
begin
  FContentFont := StylesAdapter.GetContentParams(NullDate, Nil).Font;
  FContentSmallFont.Assign(FContentFont);
  FContentSmallFont.Size := Round(FContentFont.Size * 2 / 3);
end;

function TcxSchedulerYearViewViewInfo.GetAdapterFirstMonth: Integer;
begin
  Result := Max(FAdapterFirstMonth, 1);
end;

function TcxSchedulerYearViewViewInfo.GetAdapterMonthCount: Integer;
begin
  Result := FAdapterMonthCount;
  if (Result < 1) or (Result > 12) then
    Result := 12;
end;

function TcxSchedulerYearViewViewInfo.GetDaysCell(
  AIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
begin
  Result := TcxSchedulerYearViewContentCellViewInfo(FDayCells.List[AIndex])
end;

function TcxSchedulerYearViewViewInfo.GetDaysCellCount: Integer;
begin
  Result := FDayCells.Count;
end;

function TcxSchedulerYearViewViewInfo.GetMonthBounds(AIndex: Integer): TRect;
begin
  Result := MonthHeaderCells[AIndex - FirstMonth + 1].Bounds;
  Result.Left := Result.Right;
  Result.Right := Bounds.Right;
end;

function TcxSchedulerYearViewViewInfo.GetYearView: TcxSchedulerYearView;
begin
  Result := TcxSchedulerYearView(inherited View);
end;

{ TcxSchedulerYearViewNavigation }

procedure TcxSchedulerYearViewNavigation.KeyDown(var AKey: Word;
  AShift: TShiftState);
const
  ANextCellKey: array[Boolean] of Word = (VK_RIGHT, VK_LEFT);
begin
  CorrectCurrentAnchor;
  FCanChangeSelection := True;
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
  if ViewInfo.FYearViewExtraAdapter <> nil then
    TcxSchedulerYearViewAdapter(ViewInfo.FYearViewExtraAdapter).PredefinedCurrentAnchor :=
      FCurrentAnchor;
  ViewInfo.CheckLayout(FCurrentAnchor);
  SetSelAnchor(FCurrentAnchor, AShift);
end;

procedure TcxSchedulerYearViewNavigation.ValidateSelection(
  var ASelStart, ASelFinish: TDateTime;
  var AResource: TcxSchedulerStorageResourceItem);
begin
  AResource := nil;
  FCanChangeSelection := FCanChangeSelection or View.HitTest.HitAtTime;
  if not FCanChangeSelection then
  begin
    ASelStart := FSaveSelStart;
    ASelFinish := FSaveSelFinish;
  end;
  ASelStart := Min(Max(ASelStart, View.FirstVisibleDate), View.LastVisibleDate);
  ASelFinish := Min(Max(ASelFinish, View.FirstVisibleDate), View.LastVisibleDate);
  if (View.MaxSelectedDaysCount > 0) then
    ASelFinish := Max(Min(ASelFinish, ASelStart + View.MaxSelectedDaysCount - 1),
      ASelStart - View.MaxSelectedDaysCount + 1);
  FSaveSelFinish := ASelFinish;
  FSaveSelStart := ASelStart;
  FCanChangeSelection := False;
end;

function TcxSchedulerYearViewNavigation.ContentCell(
  AIndex: Integer): TcxSchedulerYearViewContentCellViewInfo;
begin
  Result := TcxSchedulerYearViewContentCellViewInfo(ViewInfo.ContentCells.List[AIndex]);
end;

procedure TcxSchedulerYearViewNavigation.CorrectCurrentAnchor;
begin
  FCurrentAnchor := Min(Max(FCurrentAnchor, View.FirstVisibleDate),
    View.LastVisibleDate);
end;

procedure TcxSchedulerYearViewNavigation.DoKeyDown(
  var AKey: Word; AShift: TShiftState);
var
  APrevCurrentAnchor: TDateTime;
begin
  if Scheduler.SelectedEventCount > 0 then
    Scheduler.UnselectEvents;
  FCurrentAnchor := SelAnchor;
  FCurrentResource := SelResource;
  APrevCurrentAnchor := FCurrentAnchor;
  FShift := AShift;
  KeyDown(AKey, AShift);
  if YearOf(APrevCurrentAnchor) = YearOf(FCurrentAnchor) then
    ViewInfo.UpdateSelection;
end;

procedure TcxSchedulerYearViewNavigation.GotoCornerCell(AGotoEnd: Boolean);
var
  AMonth, AYear, ADay: Word;
begin
  if ssCtrl in FShift then
  begin
    if AGotoEnd then
      FCurrentAnchor := View.LastVisibleDate
    else
      FCurrentAnchor := View.FirstVisibleDate;
  end
  else
  begin
    DecodeDate(FCurrentAnchor, AYear, AMonth, ADay);
    if AGotoEnd then
      FCurrentAnchor := EncodeDate(AYear, AMonth, DaysPerMonth(AYear, AMonth))
    else
      FCurrentAnchor := EncodeDate(AYear, AMonth, 1);
  end;
end;

procedure TcxSchedulerYearViewNavigation.GotoNextCellHorz(AGotoNext: Boolean);
begin
  FCurrentAnchor := FCurrentAnchor + DayInc[AGotoNext];
end;

procedure TcxSchedulerYearViewNavigation.GotoNextCellVert(AGoForward: Boolean);

  function DayOfTheRange(ACurrentAnchor: TDateTime): Word;
  var
    AYear, AMonth, ADay: Word;
  begin
    DecodeDate(ACurrentAnchor, AYear, AMonth, ADay);
    Result := Trunc(ACurrentAnchor) -
      Trunc(EncodeDate(AYear, ViewInfo.AdapterFirstMonth, 1)) + 1;
  end;

var
  AYear, AMonth, ADay, APart: Word;
  AIndex, AColIndex: Integer;
begin
  APart := ViewInfo.GetPartOfTheYear(FCurrentAnchor);
  DecodeDate(FCurrentAnchor, AYear, AMonth, ADay);
  IncAMonth(AYear, AMonth, ADay, DayInc[AGoForward]);
  if (AYear = ViewInfo.Year) and
    (APart = ViewInfo.GetPartOfTheYear(EncodeDate(AYear, AMonth, ADay))) then
  begin
    AIndex := ViewInfo.FContentCells.IndexOf(ViewInfo.DaysCells[DayOfTheRange(FCurrentAnchor) - 1]);
    AColIndex := ContentCell(AIndex).FColIndex;
    AIndex := AIndex + DayInc[AGoForward];
    while ContentCell(AIndex).FColIndex <> AColIndex do
      Inc(AIndex, DayInc[AGoForward]);
    while ContentCell(AIndex).TimeStart = NullDate do
      Inc(AIndex, DayInc[AColIndex < 20]);
    FCurrentAnchor := ContentCell(AIndex).TimeStart;
  end
  else
    FCurrentAnchor := EncodeDate(AYear, AMonth, ADay);
end;

procedure TcxSchedulerYearViewNavigation.GotoNextPage(AGotoForward: Boolean);
var
  AYear, AMonth, ADay: Word;
  ANewMonth: Integer;
begin
  DecodeDate(FCurrentAnchor, AYear, AMonth, ADay);
  ANewMonth := AMonth;
  Inc(ANewMonth, DayInc[AGotoForward] * ViewInfo.AdapterMonthCount);
  if ANewMonth < 1 then
  begin
    Dec(AYear);
    Inc(ANewMonth, 12);
  end;
  if ANewMonth > 12 then
  begin
    Inc(AYear);
    Dec(ANewMonth, 12);
  end;
  AMonth := ANewMonth;
  ADay := Min(ADay, DayOfTheMonth(EndOfAMonth(AYear, AMonth)));
  FCurrentAnchor := EncodeDate(AYear, AMonth, ADay);
end;

function TcxSchedulerYearViewNavigation.GetYearView: TcxSchedulerYearView;
begin
  Result := TcxSchedulerYearView(inherited View);
end;

function TcxSchedulerYearViewNavigation.GetYearViewInfo: TcxSchedulerYearViewViewInfo;
begin
  Result := TcxSchedulerYearViewViewInfo(inherited ViewInfo);
end;

{ TcxSchedulerMonthHeaderCellViewInfo }

procedure TcxSchedulerMonthHeaderCellViewInfo.CalculateDisplayText;

  function GetPartOfYear: string;
  begin
    if MonthCount = 0 then Exit;
    Result := '';
    if MonthCount = 6 then
      Result := Result + scxHalfYearShort
    else
      if MonthCount = 3 then
        Result := Result + scxQuarterShort;
    if MonthCount <> 12 then
      Result := Result + IntToStr(FirstMonth div MonthCount + 1) + ' ';
  end;

begin
  DateTime := NullDate;
  if FMonth = 0 then
    DisplayText := GetPartOfYear + IntToStr(FYear)
  else
    DisplayText := dxFormatSettings.LongMonthNames[FMonth];
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.DoDraw;
begin
  inherited;
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  if FMonth = 0 then
    TcxSchedulerYearViewHitTest(AHitTest).SetHitTime(htcYear, EncodeDate(Year, 1, 1))
  else
    TcxSchedulerYearViewHitTest(AHitTest).SetHitTime(htcMonth, EncodeDate(Year, FMonth, 1));
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.Initialize(AYear, AMonth, AFirstMonth, AMonthCount: Integer; ARotateHeader: Boolean);
begin
  FMonth := AMonth;
  FYear := AYear;
  FFirstMonth := AFirstMonth;
  FMonthCount := AMonthCount;
  RotateHeader := True;
  CalculateDisplayText;
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.SetFirstMonth(AValue: Integer);
begin
  FFirstMonth := AValue;
  CalculateDisplayText;
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.SetMonth(AValue: Integer);
begin
  FMonth := AValue;
  CalculateDisplayText;
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.SetMonthCount(AValue: Integer);
begin
  FMonthCount := AValue;
  CalculateDisplayText;
end;

procedure TcxSchedulerMonthHeaderCellViewInfo.SetYear(AValue: Integer);
begin
  FYear := AValue;
  CalculateDisplayText;
end;

{ TcxSchedulerMonthHeaderCellModernViewInfo }

function TcxSchedulerMonthHeaderCellModernViewInfo.DrawBackground(ACanvas: TcxCanvas; const ABounds: TRect): Boolean;
begin
  Result := inherited DrawBackground(ACanvas, ABounds);
  if not Result then
    ACanvas.FillRect(ABounds, Color);
  Result := True;
end;

procedure TcxSchedulerMonthHeaderCellModernViewInfo.DrawHorizontalHeader;
const
  ABorders: array[Boolean] of TcxBorders = ([bRight, bBottom], [bLeft, bBottom]);
var
  AStyle: TFontStyles;
begin
  if not Transparent then
    Painter.DrawSchedulerDayHeader(Canvas, Bounds, TextRect, Neighbors, Borders,
        FButtonState, AlignHorz, AlignVert, MultiLine, ShowEndEllipsis, '',
        Font, TextColor, Color, ScaleFactor, DrawBackground, not (nRight in Neighbors));
  AStyle := Font.Style;
  if (Month = 0) and (Year = CurrentYear) then
    Font.Style := Font.Style + [fsBold];
  DrawCaption;
  Font.Style := AStyle;
  if not IsDrawBySkin then
    Canvas.FrameRect(Bounds, ExternalPainter.Painter.DefaultSchedulerDayHeaderBorderColor, 1, ABorders[UseRightToLeftAlignment]);
end;

procedure TcxSchedulerMonthHeaderCellModernViewInfo.DrawVerticalHeader;
const
  ABorders: array[Boolean] of TcxBorders = ([bRight, bBottom], [bRight, bTop]);
begin
  if IsDrawBySkin then
  begin
    inherited DrawVerticalHeader;
    Exit;
  end;
  if FRotateBitmap = nil then
  begin
    CreateRotatedBitmap;
    if not Transparent then
    begin
      FDrawRotatedBackground := DrawBackground(FRotateBitmap.cxCanvas, Bounds);
      FRotateBitmap.Rotate(raPlus90, True);
      FRotateBitmap.cxCanvas.FrameRect(FRotateBitmap.ClientRect,
        ExternalPainter.Painter.DefaultSchedulerDayHeaderBorderColor, 1, ABorders[UseRightToLeftAlignment]);
      FRotateBitmap.Rotate(raPlus90, True);
    end;
    if RotateText then
    begin
      FRotateBitmap.Rotate(raMinus90);
      DrawCaption(FRotateBitmap.cxCanvas);
      FRotateBitmap.Rotate(raPlus90);
    end
    else
      DrawCaption(FRotateBitmap.cxCanvas);
  end;
  Canvas.Draw(Bounds.Left, Bounds.Top, FRotateBitmap);
end;

{ TcxSchedulerYearViewContentCellViewInfo }

procedure TcxSchedulerYearViewContentCellViewInfo.InitHitTest(
  AHitTest: TcxSchedulerCustomResourceViewHitTest);
begin
  if Self.TimeStart <> NullDate then
    inherited InitHitTest(AHitTest);
end;

{ TcxSchedulerYearViewContentCellModernViewInfo }

procedure TcxSchedulerYearViewContentCellModernViewInfo.DoDrawBackground;
var
  R, ATodayRect: TRect;
begin
  R := PainterHelper.ExcludeBorders(Bounds, Borders);
  if IsToday then
  begin
    ATodayRect := R;
    ATodayRect.Bottom := FTextRect.Bottom;
    Canvas.FillRect(ATodayRect, ExternalPainter.Painter.DefaultSchedulerHeaderContainerAlternateBackgroundColor);
    R.Top := ATodayRect.Bottom;
  end;
  if Selected then
    Canvas.FillRect(R, SelectionColor);
end;

function TcxSchedulerYearViewContentCellModernViewInfo.GetDefaultBorderColor: TColor;
begin
  Result := ExternalPainter.Painter.DefaultSchedulerHeaderContainerBorderColor;
end;

function TcxSchedulerYearViewContentCellModernViewInfo.GetDisplayText: string;
begin
  Result := FormatDateTime('d mmm', TimeStart);
  if cxTextWidth(Font, Result) >= (cxRectWidth(FTextRect) div 2) then
    Result := IntToStr(DayOf(TimeStart));
end;

function TcxSchedulerYearViewContentCellModernViewInfo.GetTextFlags: Cardinal;
const
  AHorizontalAlignment: array[Boolean] of Word = (CXTO_LEFT, CXTO_RIGHT);
begin
  Result := CXTO_PREVENT_LEFT_EXCEED or CXTO_PREVENT_TOP_EXCEED or CXTO_CENTER_VERTICALLY or
    AHorizontalAlignment[UseRightToLeftAlignment];
end;

function TcxSchedulerYearViewContentCellModernViewInfo.GetTextLeft: Integer;
begin
  Result := Bounds.Left + ScaleFactor.Apply(cxTimeLineWidth);
end;

function TcxSchedulerYearViewContentCellModernViewInfo.GetTextLeftForSmallFont: Integer;
begin
  Result := FTextRect.Left + ScaleFactor.Apply(cxTextOffset);
end;

function TcxSchedulerYearViewContentCellModernViewInfo.SaveFontStyle(AFont: TFont): TFontStyles;
begin
  Result := inherited SaveFontStyle(AFont);
  if IsToday  or (DayOf(TimeStart) = 1) then
    AFont.Style := AFont.Style + [fsBold];
end;

{ TcxYearViewDragEventHelper }

procedure TcxYearViewDragEventHelper.UpdateViewClonesTime;
var
  I: Integer;
  ANewStart, ADelta: TDateTime;
  AShift: TDateTime;
  View: TcxSchedulerYearView;
begin
  View := TcxSchedulerYearViewViewInfo(ViewInfo).GetYearView;
  AShift := HitTest.Time - Controller.StartDragHitTime;
  for I := 0 to Clones.Count - 1 do
  begin
    ANewStart := AShift + Clones[I].Source.Start;
    ADelta := Max(0, View.GetFirstVisibleDate - dxDateOf(ANewStart));
    if ADelta = 0 then
    begin
      ADelta := Min(0, View.GetLastVisibleDate - dxDateOf(ANewStart) -
        dxDateOf(Clones[I].Duration) + Ord(Clones[I].AllDayEvent));
    end;
    Clones[I].MoveTo(ANewStart + ADelta);
  end;
end;

{ TcxYearViewEventSizing }

procedure TcxYearViewEventSizing.UpdateEventBounds;
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

{ TcxSchedulerYearViewStyles }

constructor TcxSchedulerYearViewStyles.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner);
  FYearView := TcxSchedulerYearView(GetOwner);
  FScheduler := FYearView.Scheduler;
  BitmapInViewParams := True;
end;

procedure TcxSchedulerYearViewStyles.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerYearViewStyles then
    for I := 0 to cxcsMaxYearViewStyle do
      SetValue(I, TcxSchedulerYearViewStyles(Source).GetValue(I));
  inherited Assign(Source);
end;

function TcxSchedulerYearViewStyles.GetMonthHeaderParams(
  AYear, AMonth: Integer): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetMonthHeaderStyle) then
    FOnGetMonthHeaderStyle(YearView, AYear, AMonth, AStyle);
  GetViewParams(cxcsMonthHeader, nil, AStyle, Result);
end;

function TcxSchedulerYearViewStyles.GetUnusedContentParams(
  AYear, AMonth, ADayOfWeek: Integer): TcxViewParams;
var
  AStyle: TcxStyle;
begin
  AStyle := nil;
  if Assigned(FOnGetUnusedContentStyle) then
    FOnGetUnusedContentStyle(YearView, AYear, AMonth, ADayOfWeek, AStyle);
  GetViewParams(cxcsUnusedContent, Pointer(ADayOfWeek), AStyle, Result);
  {with YearView do
  begin
    if (AStyle = nil) or not (svColor in AStyle.AssignedValues) then
    begin
      Result.Color := PainterHelper.GetContentColor(Result.Color,
         TDay(ADayOfWeek) in OptionsView.WorkDays);
    end;
  end;} //Do the same as GetDefaultViewParams
end;

procedure TcxSchedulerYearViewStyles.Changed(AIndex: Integer);
begin
  inherited Changed(AIndex);
  FYearView.Refresh;
end;

procedure TcxSchedulerYearViewStyles.GetDefaultViewParams(
  Index: Integer; AData: TObject; out AParams: TcxViewParams);
var
  AIsWorkTime: Boolean;
begin
  AParams.Bitmap := nil;
  AParams.Font := Scheduler.Font;
  AParams.TextColor := clBlack;
  AParams.Color := Scheduler.Color;
  case Index of
    cxcsUnusedContent:
      begin
        AIsWorkTime := TDay(Integer(AData)) in FYearView.OptionsView.WorkDays;
        AParams.Color := Painter.DefaultSchedulerYearViewUnusedContentColor(AIsWorkTime);
        if AParams.Color = clDefault then
          AParams.Color := FYearView.PainterHelper.GetContentColor(clBtnFace, AIsWorkTime, Scheduler.OptionsView.Style);
      end;
    cxcsMonthHeader:
      begin
        AParams.Color := Painter.DefaultHeaderColor;
        AParams.TextColor := Painter.DefaultHeaderTextColor;
      end;
  end;
end;

function TcxSchedulerYearViewStyles.GetPainter: TcxCustomLookAndFeelPainter;
begin
  Result := FYearView.LookAndFeelPainter;
end;

{ TcxSchedulerYearView }

constructor TcxSchedulerYearView.Create(AOwner: TcxCustomScheduler);
begin
  inherited Create(AOwner);
  FScale := 12;
end;

procedure TcxSchedulerYearView.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerYearView then
    with TcxSchedulerYearView(Source) do
    begin
      Self.FMaxSelectedDaysCount := FMaxSelectedDaysCount;
      Self.FAllDayEventsOnly := FAllDayEventsOnly;
      Self.FMonthHeaderPopupMenu := FMonthHeaderPopupMenu;
      Self.FStyles.Assign(FStyles);
      Self.FScale := FScale;
    end;
  inherited Assign(Source);
end;

procedure TcxSchedulerYearView.CreateSubClasses;
begin
  FStyles := CreateStyles;
  inherited CreateSubClasses;
  FMonthHeaderPopupMenu := CreateMonthHeaderPopupMenu;
end;

function TcxSchedulerYearView.CreateViewAdapter: TcxCustomResourceViewAdapter;
begin
  Result := TcxSchedulerYearViewAdapter.Create(Self);
end;

procedure TcxSchedulerYearView.DestroySubClasses;
begin
  FMonthHeaderPopupMenu.Free;
  inherited DestroySubClasses;
  FStyles.Free;
end;

function TcxSchedulerYearView.CanDeactivateOnDateNavigatorSelectionChange: Boolean;
begin
  Result := False;
end;

function TcxSchedulerYearView.CanSelectPeriod: Boolean;
begin
  Result := False;
end;

procedure TcxSchedulerYearView.ChangeLayout(ACurrentAnchor: TDateTime);
begin
  Scheduler.DateNavigator.BeginUpdate;
  try
    SelectedDays.Clear;
    SelectedDays.Add(EncodeDate(YearOf(ACurrentAnchor),
      ViewInfo.GetPartOfTheYear(ACurrentAnchor) *
      ViewInfo.AdapterMonthCount + 1, 1));
    UpdateDateNavigatorSelection;
    Changed;
    Scheduler.FullRefresh;
  finally
    Scheduler.DateNavigator.EndUpdate;
  end;
end;

function TcxSchedulerYearView.CreateController: TcxSchedulerSubControlController;
begin
  Result := TcxSchedulerYearViewController.Create(Self);
end;

function TcxSchedulerYearView.CreateHitTest: TcxSchedulerSubControlHitTest;
begin
  Result := TcxSchedulerYearViewHitTest.Create(Self);
end;

function TcxSchedulerYearView.CreateMonthHeaderPopupMenu: TcxSchedulerMonthHeaderPopupMenu;
begin
  Result := TcxSchedulerMonthHeaderPopupMenu.Create(Scheduler);
end;

function TcxSchedulerYearView.CreatePainter: TcxSchedulerSubControlPainter;
begin
  Result := TcxSchedulerYearViewPainter.Create(Self);
end;

function TcxSchedulerYearView.CreateStyles: TcxSchedulerYearViewStyles;
begin
  Result := TcxSchedulerYearViewStyles.Create(Self);
end;

function TcxSchedulerYearView.CreateViewInfo: TcxSchedulerSubControlViewInfo;
begin
  Result := TcxSchedulerYearViewViewInfo.Create(Self);
end;

function TcxSchedulerYearView.DoShowPopupMenu(X, Y: Integer): Boolean;
begin
  if HitTest.HitAtMonth then
    Result := MonthHeaderPopupMenu.Popup(X, Y)
  else
    Result := inherited DoShowPopupMenu(X, Y);
end;

function TcxSchedulerYearView.GetCompressWeekEnd: Boolean;
begin
  Result := False;
end;

function TcxSchedulerYearView.GetGroupingKind: TcxSchedulerGroupingKind;
begin
  Result := gkNone;
end;

function TcxSchedulerYearView.GetFirstVisibleDate: TDateTime;
begin
  Result := EncodeDate(Year, 1, 1);
end;

function TcxSchedulerYearView.GetLastVisibleDate: TDateTime;
begin
  Result := EncodeDate(Year, 12, 31)
end;

function TcxSchedulerYearView.GetScrollPos: Integer;
var
  AMonthCount: Integer;
begin
  AMonthCount := ViewInfo.AdapterMonthCount;
  if AMonthCount <> 0 then
    Result := Max(cxYearViewScrollMinPos,
      Min(MulDiv(cxYearViewScrollMaxPos, 12, AMonthCount),
      MulDiv((Year - CurrentYear), 12, AMonthCount) +
      ViewInfo.AdapterFirstMonth div AMonthCount -
      ViewInfo.GetPartOfTheYear(Now) +
      MulDiv(cxYearViewScrollCurrentYearPos, 12, AMonthCount)))
  else
    Result := Max(cxYearViewScrollMinPos, Min(cxYearViewScrollMaxPos,
      (Year - CurrentYear + cxYearViewScrollCurrentYearPos)));
end;

function TcxSchedulerYearView.GetVisibleDaysRange: Integer;
begin
  Result := Trunc(GetLastVisibleDate - GetFirstVisibleDate);
end;

procedure TcxSchedulerYearView.InitScrollBarsParameters;
var
  APos: Integer;
begin
  inherited InitScrollBarsParameters;
  APos := GetScrollPos;
  SetScrollBarInfo(sbVertical, cxYearViewScrollMinPos,
    MulDiv(cxYearViewScrollMaxPos, 12, ViewInfo.AdapterMonthCount),
    1, 1, APos, True, True);
end;

procedure TcxSchedulerYearView.MakeEventVisible(AEvent: TcxSchedulerControlEvent;
  const ADate: TDateTime; AResource: TcxSchedulerStorageResourceItem);
begin
  if ADate <> NullDate then
  begin
    ViewInfo.CheckLayout(ADate);
    LayoutChanged;
  end;
end;

procedure TcxSchedulerYearView.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = Styles.UnusedContent then
      Styles.UnusedContent := nil;
    if AComponent = Styles.MonthHeader then
      Styles.MonthHeader := nil;
  end;
  MonthHeaderPopupMenu.Notification(AComponent, Operation);
end;

procedure TcxSchedulerYearView.Scroll(AScrollBarKind: TScrollBarKind;
  AScrollCode: TScrollCode; var AScrollPos: Integer);
var
  AYear, AMonth, AMonthCount: Integer;
  ANavigation: TcxSchedulerYearViewNavigation;
begin
  ANavigation := TcxSchedulerYearViewNavigation(Navigation);
  HideHintOnScroll(AScrollCode);
  AMonthCount := ViewInfo.AdapterMonthCount;
  if AMonthCount = 0 then
    AYear := CurrentYear - cxYearViewScrollCurrentYearPos + AScrollPos
  else
    AYear := MulDiv(CurrentYear - cxYearViewScrollCurrentYearPos, 12, AMonthCount) +
      ViewInfo.GetPartOfTheYear(Now) + AScrollPos;
  ANavigation.CorrectCurrentAnchor;
  case AScrollCode of
    scTrack:
      begin
        ShowHintOnScroll(IntToStr(AYear), sbHorizontal);
        Exit;
      end;
    scPosition:
      if AMonthCount = 0 then
        ViewInfo.CheckLayout(EncodeDate(AYear, 1, 1))
      else
      begin
        AMonth := AYear mod (12 div AMonthCount) * AMonthCount + 1;
        AYear := AYear div (12 div AMonthCount);
        ANavigation.FCurrentAnchor := EncodeDate(AYear, AMonth, 1);
      end;
    scLineUp, scPageUp:
      ANavigation.GotoNextPage(False);
    scLineDown, scPageDown:
      ANavigation.GotoNextPage(True);
  end;
  ViewInfo.CheckLayout(ANavigation.FCurrentAnchor);
  AScrollPos := GetScrollPos;
end;

procedure TcxSchedulerYearView.SelectedDaysChanged;
begin
  if SelectedDays.Count > 0 then
    ViewInfo.CheckLayout(SelectedDays[0]);
end;

procedure TcxSchedulerYearView.SetGroupingKind(AValue: TcxSchedulerGroupingKind);
begin
  inherited SetGroupingKind(gkNone);
end;

function TcxSchedulerYearView.GetHitTest: TcxSchedulerYearViewHitTest;
begin
  Result := TcxSchedulerYearViewHitTest(inherited HitTest);
end;

function TcxSchedulerYearView.GetNavigation: TcxSchedulerYearViewNavigation;
begin
  Result := TcxSchedulerYearViewNavigation(Controller.Navigation);
end;

procedure TcxSchedulerYearView.GetRangeControlRange(out AMin, AMax: TDateTime);
begin
  if Scale = 3 then
  begin
    AMin := EncodeDate(Year, ViewInfo.AdapterFirstMonth, 1);
    AMax := TdxRangeControlDateTimeClientHelper.IncDate(AMin, rcduQuarter);
  end
  else
    if Scale = 6 then
    begin
      AMin := EncodeDate(Year, ViewInfo.AdapterFirstMonth, 1);
      AMax := TdxRangeControlDateTimeClientHelper.IncDate(AMin, rcduQuarter, 2);
    end
    else
    begin
      AMin := StartOfTheYear(FirstVisibleDate);
      AMax := IncYear(AMin);
    end;
end;

procedure TcxSchedulerYearView.GetRangeControlScales(AScales: TdxRangeControlDateTimeScales; AValues: TdxRangeControlDateTimeScaleList);
begin
  AValues.Add(AScales.Year);
  if Scale < 12 then
    AValues.Add(AScales.Quarter);
end;

procedure TcxSchedulerYearView.GetRangeControlTotalRange(out AMin, AMax: TDateTime);
var
  AStart: TDateTime;
begin
  AStart := EncodeDate(Year, 1, 1);
  AMin := IncYear(AStart, -3);
  AMax := IncYear(AStart, 4);
end;

function TcxSchedulerYearView.GetViewInfo: TcxSchedulerYearViewViewInfo;
begin
  Result := TcxSchedulerYearViewViewInfo(inherited ViewInfo);
end;

function TcxSchedulerYearView.GetYear: Word;
begin
  Result := YearOf(inherited GetFirstVisibleDate);
end;

procedure TcxSchedulerYearView.SetAllDayEventsOnly(AValue: Boolean);
begin
  if FAllDayEventsOnly <> AValue then
  begin
    FAllDayEventsOnly := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerYearView.SetMaxSelectedDaysCount(
  AValue: Integer);
begin
  AValue := Max(0, AValue);
  if AValue <> FMaxSelectedDaysCount then
  begin
    FMaxSelectedDaysCount := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerYearView.SetMonthHeaderPopupMenu(
  AValue: TcxSchedulerMonthHeaderPopupMenu);
begin
  FMonthHeaderPopupMenu.Assign(AValue);
end;

procedure TcxSchedulerYearView.SetScale(AValue: Integer);
begin
  if (AValue <> FScale) and (AValue in [3, 6, 12]) then
  begin
    FScale := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerYearView.SetStyles(AValue: TcxSchedulerYearViewStyles);
begin
  FStyles.Assign(AValue);
end;

{ TcxSchedulerYearViewAdapter }

function TcxSchedulerYearViewAdapter.QueryInterface(
  const IID: TGUID; out Obj): HResult; stdcall;
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

function TcxSchedulerYearViewAdapter._AddRef: Integer;
begin
  Result := -1;
end;

function TcxSchedulerYearViewAdapter._Release: Integer;
begin
  Result := -1;
end;

function TcxSchedulerYearViewAdapter.GetDayCountPerPage: Integer;
begin
  Result := MaxYearViewWeekDays;
end;

function TcxSchedulerYearViewAdapter.GetFirstMonth: Integer;
var
  AYear, AMonth, ADay: Word;
  AMonthCount: Integer;
  ACurrentAnchor: TDateTime;
begin
  if PredefinedCurrentAnchor <> 0 then
    ACurrentAnchor := PredefinedCurrentAnchor
  else
    ACurrentAnchor := TcxSchedulerCustomViewViewInfoAccess(TcxSchedulerYearView(View).ViewInfo).FSelectedDays[0];
  PredefinedCurrentAnchor := 0;
  DecodeDate(ACurrentAnchor, AYear, AMonth, ADay);
  AMonthCount := GetMonthCount;
  Result := 1;
  while Result + AMonthCount <= AMonth do
    Inc(Result, AMonthCount);
end;

function TcxSchedulerYearViewAdapter.GetMonthCount: Integer;
begin
  Result := TcxSchedulerYearView(View).Scale;
end;

function TcxSchedulerYearViewAdapter.GetShowMonthHeaders: Boolean;
begin
  Result := True;
end;

function TcxSchedulerYearViewAdapter.GetShowWeekDayHeaders: Boolean;
begin
  Result := True;
end;

function TcxSchedulerYearViewAdapter.GetStartDayIndex: Integer;
begin
  Result := 0;
end;

{ TcxSchedulerMonthHeaderPopupMenu }

constructor TcxSchedulerMonthHeaderPopupMenu.Create(AScheduler: TcxCustomScheduler);
begin
  inherited Create(AScheduler);
  FItems := [mhpmiNewEvent, mhpmiNewAllDayEvent, mhpmiNewReccuringEvent,
    mhpmiFullYear, mhpmiHalfYear, mhpmiQuarter];
end;

procedure TcxSchedulerMonthHeaderPopupMenu.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerMonthHeaderPopupMenu then
  begin
    Items := TcxSchedulerMonthHeaderPopupMenu(Source).Items;
    OnClick := TcxSchedulerMonthHeaderPopupMenu(Source).OnClick;
    OnPopup := TcxSchedulerMonthHeaderPopupMenu(Source).OnPopup;
  end;
end;

procedure TcxSchedulerMonthHeaderPopupMenu.Execute(AItem: TcxSchedulerMonthHeaderPopupMenuItem);
begin
  ExecuteCommand(Ord(AItem));
end;

function TcxSchedulerMonthHeaderPopupMenu.GetMenuItem(AItem: TcxSchedulerMonthHeaderPopupMenuItem): TMenuItem;
begin
  Result := FindItemByCommand(Root, Ord(AItem));
end;

procedure TcxSchedulerMonthHeaderPopupMenu.CreateItems;

  function GetChecked(ACommand: Integer): Boolean;
  var
    AIndex: Integer;
  begin
    Result := False;
    AIndex := ACommand - Ord(mhpmiFullYear);
    if (AIndex >= 0) and (AIndex <= 2) then
    Result := YearView.Scale = YearScales[AIndex];
  end;

  procedure CreateRulerItem(const ACaption: string;
    AItem: TcxSchedulerMonthHeaderPopupMenuItem);
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
  CreateNewEventItems(mhpmiNewEvent in FItems, mhpmiNewAllDayEvent in FItems,
    mhpmiNewReccuringEvent in FItems, Ord(mhpmiNewEvent), Ord(mhpmiNewAllDayEvent),
    Ord(mhpmiNewReccuringEvent));
  if Items * [mhpmiFullYear, mhpmiHalfYear, mhpmiQuarter] <> [] then
  begin
    AddValidSeparator(Root);
    CreateRulerItem(cxGetResourceString(@scxpmFullYear), mhpmiFullYear);
    CreateRulerItem(cxGetResourceString(@scxpmHalfYear), mhpmiHalfYear);
    CreateRulerItem(cxGetResourceString(@scxpmQuarter), mhpmiQuarter);
  end;
end;

function TcxSchedulerMonthHeaderPopupMenu.GetYearView: TcxSchedulerYearView;
begin
  Result := Scheduler.CurrentView as TcxSchedulerYearView;
end;

procedure TcxSchedulerMonthHeaderPopupMenu.DoExecute(ACommand: Integer);
var
  AIndex: Integer;
begin
  if ACommand in [Ord(mhpmiNewEvent), Ord(mhpmiNewAllDayEvent), Ord(mhpmiNewReccuringEvent)] then
    inherited DoExecute(ACommand)
  else
  begin
    AIndex := ACommand - Ord(mhpmiFullYear);
    if AIndex in [0..2] then
      YearView.Scale := YearScales[AIndex];
  end;
end;

function TcxSchedulerMonthHeaderPopupMenu.DoOnClick(ACommand: Integer): Boolean;
begin
  Result := False;
  if Assigned(FOnClick) then
    FOnClick(Self, TcxSchedulerMonthHeaderPopupMenuItem(ACommand), Result);
end;

function TcxSchedulerMonthHeaderPopupMenu.DoOnPopup: Boolean;
begin
  Result := False;
  if Assigned(FOnPopup) then
    FOnPopup(Self, InternalMenu, Result);
end;

function TcxSchedulerMonthHeaderPopupMenu.IsValidCommand(ACommand: Integer): Boolean;
begin
  Result := (ACommand >= Ord(mhpmiNewEvent)) and (ACommand <= Ord(mhpmiQuarter));
end;

end.

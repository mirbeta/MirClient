{$I TMSDEFS.INC}
{***********************************************************************}
{ TPlannerMonthView component                                           }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by :                                                          }
{            TMS Software                                               }
{            copyright © 2004 - 2015                                    }
{            Email : info@tmssoftware.com                               }
{            Website : http://www.tmssoftware.com                       }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The source       }
{ code remains property of the writer and may not be distributed        }
{ freely as such.                                                       }
{***********************************************************************}

{$DEFINE USEIMAGE}

unit PlannerMonthView;

{$DEFINE TMSSKINS}

{$T-}

interface

uses
  SysUtils, Windows, Messages, Classes, Graphics, Controls, Menus, PictureContainer,
  ComCtrls, Dialogs, ExtCtrls, Printers, AdvStyleIF, Planner, Forms
  , PlanXPVS
{$IFDEF USEIMAGE}
  , AdvImage
{$ENDIF}
  , DateUtils, Types
  {$IFDEF DELPHIXE3_LVL}
  , System.UITypes
  {$ENDIF}

  ;

const
  MAJ_VER = 3; // Major version nr.
  MIN_VER = 1; // Minor version nr.
  REL_VER = 0; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Jun, 2015'; // Month version

  // version history
  // 1.2.0.0 : added recurrency support
  //         : added printing support
  //         : added balloon support
  //         : added AutoThemeAdapt
  // 1.2.0.1 : fixed popupmenu handling with ShowWeeks = true
  // 1.2.0.2 : fixed MouseMove handling in browser area
  // 1.2.0.3 : fixed issue with Visible property
  // 1.2.0.4 : fixed issue with OnDaySelect with item scroll enabled
  // 1.2.0.5 : fixed issue with DateAtXY and ShowWeeks
  // 1.2.0.6 : fixed issue with OnItemDelete event
  // 1.2.0.7 : fixed issue with OnItemEnter, OnItemExit events
  // 1.2.0.8 : new: OnMonthChanged, OnYearChanged events added
  //         : new: function CreateNonDBItem added
  // 1.2.0.9 : fixed issue with DateAtXY and ShowWeeks from OnDragDrop event

  // 2.5.0.0 : release based on TPlanner / TDBPlanner
  // 2.5.0.1 : Fixed issue with ShowToday
  // 2.5.0.2 : Fixed issue with shadow painting
  // 2.5.0.3 : Fixed issue with FocusColor use
  //         : Fixed issue with ShowSelectionFull = false & cross month items
  // 2.5.0.4 : Fixed painting issue for background color
  // 2.5.0.5 : Fixed selected weekend color painting issue
  //         : Improved : PlannerItem.Cursor behaviour identical to Planner
  // 2.5.0.6 : Fixed issue with FindFirstItemAtDate, FindNextItemAtDate
  // 2.5.0.7 : Fixed issue with resizing while inplace edit
  // 2.5.0.8 : Fixed issue with mouse handling for items when ShowWeeks = true
  // 2.5.0.9 : Fixed issue with item resizing with ShowWeeks = false
  // 2.5.0.10: Fixed issue with OnItemMoving with Allow = false
  // 2.5.0.11: Fixed issue with OnItemText event
  // 2.5.1.0 : New : support for Office 2007 silver style added
  // 2.5.1.1 : Fixed : issue with programmatically setting the ImageList
  //         : Fixed : issue with multiday items with duration < 24hrs
  // 2.5.1.2 : Fixed : issue with showing a messagebox from the OnItemSizing event
  // 2.5.1.5 : TPlanner/TDBPlanner Compatibility update
  // 2.5.2.0 : Fixed : issue with MinDate & MaxDate use
  // 2.5.2.1 : Fixed : issue with Allow parameter in OnItemMoving
  // 2.5.3.0 : New : OnItemUnSelect event added
  // 2.5.3.1 : Fixed : hovering on monthlabel with large caption/font size
  // 2.5.3.2 : Fixed : issue with OnItemPopupPrepare
  // 2.5.3.3 : Fixed : issue with XYToDate for very small header size
  // 2.5.3.4 : Fixed : issue with DeleteGlyph & AttachmentGlyph on planner items
  // 2.5.3.5 : Fixed : issue with auto theme switching
  // 2.5.3.6 : Fixed : issue with balloon text
  // 2.5.4.0 : New : exposed Completion property
  // 2.5.4.1 : Fixed : issue with moving items with time part in DB version
  // 2.5.4.2 : Fixed : issue with auto delete of recurrent items
  // 2.5.5.0 : New : method PrintTo() added
  // 2.5.5.1 : Fixed : issue with programmatically setting date & AutoChangeMonth
  // 2.5.5.0 : New : Terminal, Windows Vista & Windows 7 styles
  // 2.5.6.1 : Improved : printout of TPlannerMonthView
  // 2.5.6.2 : Fixed : issue with ItemPopup access during OnItemPopupPrepare
  // 2.5.7.0 : New : ISO week number calculation support added
  // 2.5.7.1 : Fixed : Selection issue with AutoChangeMonth = false
  // 2.5.8.0 : Improved : Performance when adding lots of items
  // 2.5.9.0 : New : Properties ShowYearPopup, ShowMonthPopup added
  //         : New : Items.NumItemsAtDate() function added
  //         : Fixed : Issue with key generation for DB updates via events
  //         : Fixed : Issue with cursor when ShowCaption = false
  // 2.5.10.0: New : OnMouseWheelDown, OnMouseWheelUp events exposed
  // 2.5.10.1: Fixed : Issue with OnDayChange event in TDBPlannerMonthView
  // 2.5.10.2: Fixed : Issue with year selection popup menu
  // 2.5.11.0: New : Exposed RowScrollPosition[index]: integer property
  //         : Fixed : Issue with conflict position calculation when programmatically changing the month
  // 2.5.11.1: Fixed : Issue with setting ISO week nr. calculation at runtime
  // 3.0.0.0 : New : DragItemImage, DragItemAlways added
  //         : New : Compatible with Planner v3.0 item extensions
  // 3.0.0.1 : Fixed : Issue with use of MaxDate/MinDate and date selection
  // 3.0.0.2 : Fixed : Issue with OnDaySelect triggered incorrectly
  // 3.0.1.0 : New : Support for Layers
  //         : Fixed : Issue with balloons & ShowWeeks = true
  // 3.0.1.1 : Fixed : Issue with scroller indication for last day of week
  // 3.0.1.2 : Fixed : Small issue with SetDate() call
  // 3.0.1.3 : Fixed : Issue with layer conflict calculations
  // 3.0.2.0 : New : Made 2nd parameter of HasPlannerItem optional
  // 3.0.3.0 : New : OnItemDelete event added
  // 3.0.3.1 : Fixed : Issue with date selection and changing year via click on browser
  // 3.0.3.2 : Fixed : Issue with context menu for year & month selection on caption with large caption fonts
  // 3.1.0.0 : New : Windows 10, Office 2016 styles added



  adaysinmonth: array[1..13] of word = (31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31, 29);
  monames: array[1..12] of string[5] = ('JAN', 'FEB', 'MAR', 'APR',
    'MAY', 'JUNE', 'JULY', 'AUG', 'SEP', 'OCT', 'NOV', 'DEC');
  selstr = 'Select month';
  labelx = 30;
  labelw = 65;
  CW = 16;
  MM = 2;

  CORNER_EFFECT = 10;
  RtfStart = '{\';
  HtmlEndTagStart = '</';
  EDITOFFSET = 4;

  ScrollColumnSize = 12;

type
  {$IFDEF DELPHI_UNICODE}
  THintInfo = Controls.THintInfo;
  PHintInfo = Controls.PHintInfo;
  {$ENDIF}

  TPlannerMonthViewStyle = (pmsWindowsXP, pmsOffice2000, pmsOffice2003Blue, pmsOffice2003Olive, pmsOffice2003Silver, pmsFlat, pmsAvantGarde, pmsWhidbey, pmsOffice2007Luna, pmsOffice2007Obsidian, pmsOffice2003Classic, pmsCustom, pmsOffice2007Silver, pmsWindowsVista,
  pmsWindows7, pmsTerminal, pmsOffice2010Blue, pmsOffice2010Silver, pmsOffice2010Black,
  pmsWindows8, pmsOffice2013White, pmsOffice2013LightGray, pmsOffice2013Gray,
  pmsWindows10, pmsOffice2016White, pmsOffice2016Gray, pmsOffice2016Black );

  TCustomMonthViewPanel = class;
  TPlannerMonthView = class;

  TMonthPlannerItem = class(TPlannerItem)
  private
    FEndCell: integer;
    FBeginCell: integer;
    FPosSt: TStringList;
    FEndTime: TDateTime;
    FStartTime: TDateTime;
  protected
    property PosSt: TStringList read FPosSt;
    property BeginCell: integer read FBeginCell write FBeginCell;
    property EndCell: integer read FEndCell write FEndCell;
    procedure SetItemEndTime(const Value: TDateTime); override;
    procedure SetItemStartTime(const Value: TDateTime); override;
    function GetItemEndTime: TDateTime; override;
    function GetItemStartTime: TDateTime; override;
    function GetItemEndTimeStr: string; override;
    function GetItemStartTimeStr: string; override;
    function GetItemSpanTimeStr: string; override;
    procedure Repaint; override;
  public
    procedure Assign(Source: TPersistent); override;
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Update; override;
    function GetCaptionTimeString: string; override;
    procedure MoveMonthPlannerItem(NewStartTime, NewEndTime: TDateTime; Done: Boolean; var Allow: Boolean);
  published
  end;

  TMonthPlannerRichEdit = class(TRichEdit)
  private
    FPlannerItem: TMonthPlannerItem;
  protected
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure DoEnter; override;
    procedure DoExit; override;
  published
    property PlannerItem: TMonthPlannerItem read FPlannerItem write FPlannerItem;
  end;

  TPlannerItemsEx = class(TPlannerItems)
  public
    function GetItemClass: TCollectionItemClass; override;
  end;

  TPlannerEx = class(TPlanner)
  public
    function CreateItems: TPlannerItems; override;
  end;

  TDayStr = string;
  TMonthStr = string;

  TDayArray = array[1..14] of TDayStr;
  TMonthArray = array[1..12] of TMonthStr;

  TBackGroundPosition = (bpTopLeft, bpTopRight, bpBottomLeft, bpBottomRight,
    bpTiled, bpStretched, bpCenter);

  TDaySelectEvent = procedure(Sender: TObject; SelDate: TDateTime) of object;
  TDateChangeEvent = procedure(Sender: TObject; origDate, newDate: TDateTime) of object;

  TCancelledChangeEvent = procedure(Sender: TObject; CancelledDate: TDateTime) of object;

  TGetDateEvent = procedure(Sender: TObject; dt: tdatetime; var isEvent: Boolean) of object;

  TDateBalloonEvent = procedure(Sender: TObject; ADate: TDateTime; var ATitle: string; var AText: string; var AIcon: Integer) of object;  

  TGetDateEventHint = procedure(Sender: TObject; dt: tdatetime;
    var isEvent: Boolean; var EventHint: string) of object;

  TEventShape = (evsRectangle, evsCircle, evsSquare, evsTriangle, evsNone);
  TGradientDirection = (gdHorizontal, gdVertical);

  TTodayStyle = (tsSunken, tsRaised, tsFlat, tsCaption);

  TSelDateItem = class(TCollectionItem)
  private
    FDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
    procedure Changed;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Date: TDateTime read FDate write SetDate;
  end;

  TEventProp = class(TSelDateItem);

  TSelDateItems = class(TCollection)
  private
    FOwner: TCustomMonthViewPanel;
    FUpdateCount: Integer;
    FOnChange: TNotifyEvent;
    FVisible: Boolean;
    function GetItem(Index: integer): TSelDateItem;
    procedure SetItem(Index: integer; Value: TSelDateItem);
    procedure DoPaint;
    function GetDate(dt: TDateTime): TSelDateItem;
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function IsInList(da, mo, ye: integer): integer;
    function IsDateInList(dt: TDateTime): integer;
    constructor Create(AOwner: TCustomMonthViewPanel);
    function Add: TSelDateItem;
    function Insert(Index: integer): TSelDateItem;
    property Items[Index: integer]: TSelDateItem read GetItem write SetItem; default;
    property Dates[dt: TDateTime]: TSelDateItem read GetDate;
    procedure AddRange(dt1, dt2: TDateTime);
    procedure DelRange(dt1, dt2: TDateTime);
    procedure StartUpdate;
    procedure StopUpdate;
    procedure ResetUpdate;
    procedure RepaintDate(ADate: TDateTime);
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property Visible: Boolean read FVisible write FVisible;
  end;

  TEventPropEvent = procedure(Sender: TObject; dt: tdatetime; var Event: TEventProp) of object;

  TMinMaxDate = class(TPersistent)
  private
    FOwner: TPlannerMonthView;
    FDay: smallint;
    FMonth: smallint;
    FYear: smallint;
    FUse: Boolean;
    procedure SetDay(avalue: smallint);
    procedure SetMonth(avalue: smallint);
    procedure SetYear(avalue: smallint);
    procedure SetUse(avalue: Boolean);
    function GetDate: TDateTime;
    procedure SetDate(const Value: TDateTime);
  public
    constructor Create(aOwner: TPlannerMonthView);
    property Date: TDateTime read GetDate write SetDate;
  published
    property Day: smallint read fDay write SetDay default 0;
    property Month: smallint read fMonth write SetMonth default 0;
    property Year: smallint read fYear write SetYear default 0;
    property Use: Boolean read fUse write SetUse default False;
  end;

  TCalGlyphs = class(TPersistent)
  private
    FOwner: TComponent;
    FNextYear: TBitmap;
    FPrevYear: TBitmap;
    FNextMonth: TBitmap;
    FPrevMonth: TBitmap;
    FOnChange: TNotifyEvent;
    procedure SetNextYear(const Value: TBitmap);
    procedure SetPrevYear(const Value: TBitmap);
    procedure SetPrevMonth(const Value: TBitmap);
    procedure SetNextMonth(const Value: TBitmap);
    procedure Changed(Sender: TObject);
  public
    constructor Create(AOwner: TComponent);
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
  published
    property NextMonth: TBitmap read FNextMonth write SetNextMonth;
    property PrevMonth: TBitmap read FPrevMonth write SetPrevMonth;
    property NextYear: TBitmap read FNextYear write SetNextYear;
    property PrevYear: TBitmap read FPrevYear write SetPrevYear;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TYearStartAt = class(TPersistent)
  private
    FOwner: TCustomMonthViewPanel;
    FStartDay: integer;
    FStartMonth: integer;
    FPrevYearStartDay: integer;
    FPrevYearStartMonth: integer;
    FNextYearStartDay: integer;
    FNextYearStartMonth: integer;
    FISOWeekNumber: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetStartDay(d: integer);
    procedure SetStartMonth(m: integer);
    procedure SetPrevYearStartDay(d: integer);
    procedure SetPrevYearStartMonth(m: integer);
    procedure SetNextYearStartDay(d: integer);
    procedure SetNextYearStartMonth(m: integer);
    function ValidateDay(d: integer): Boolean;
    function ValidateMonth(m: integer): Boolean;
    procedure SetISOWeekNumber(const Value: Boolean);
  protected
    procedure Changed;
  public
    constructor Create(AOwner: TCustomMonthViewPanel);
    destructor Destroy; override;
  published
    property ISOWeekNumber: Boolean read FISOWeekNumber write SetISOWeekNumber default false;
    property StartDay: integer read FStartDay write SetStartDay default 1;
    property StartMonth: integer read FStartMonth write SetStartMonth default 1;
    property PrevYearStartDay: integer read FPrevYearStartDay write SetPrevYearStartDay default 1;
    property PrevYearStartMonth: integer read FPrevYearStartMonth write SetPrevYearStartMonth default 1;
    property NextYearStartDay: integer read FNextYearStartDay write SetNextYearStartDay default 1;
    property NextYearStartMonth: integer read FNextYearStartMonth write SetNextYearStartMonth default 1;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNameOfMonths = class(TPersistent)
  private
    FJanuary: TMonthStr;
    FFebruary: TMonthStr;
    FMarch: TMonthStr;
    FApril: TMonthStr;
    FMay: TMonthStr;
    FJune: TMonthStr;
    FJuly: TMonthStr;
    FAugust: TMonthStr;
    FSeptember: TMonthStr;
    FOctober: TMonthStr;
    FNovember: TMonthStr;
    FDecember: TMonthStr;
    FOnChange: TNotifyEvent;
    FUseIntlNames: Boolean;
    procedure SetApril(const Value: TMonthStr);
    procedure SetAugust(const Value: TMonthStr);
    procedure SetDecember(const Value: TMonthStr);
    procedure SetFebruary(const Value: TMonthStr);
    procedure SetJanuary(const Value: TMonthStr);
    procedure SetJuly(const Value: TMonthStr);
    procedure SetJune(const Value: TMonthStr);
    procedure SetMarch(const Value: TMonthStr);
    procedure SetMay(const Value: TMonthStr);
    procedure SetNovember(const Value: TMonthStr);
    procedure SetOctober(const Value: TMonthStr);
    procedure SetSeptember(const Value: TMonthStr);
    procedure SetUseIntlNames(const Value: Boolean);
  protected
    procedure Changed;
    procedure InitIntl;
  public
    constructor Create;
    destructor Destroy; override;
    function GetMonth(i: integer): string;
  published
    property January: TMonthStr read FJanuary write SetJanuary;
    property February: TMonthStr read FFebruary write SetFebruary;
    property March: TMonthStr read FMarch write SetMarch;
    property April: TMonthStr read FApril write SetApril;
    property May: TMonthStr read FMay write SetMay;
    property June: TMonthStr read FJune write SetJune;
    property July: TMonthStr read FJuly write SetJuly;
    property August: TMonthStr read FAugust write SetAugust;
    property September: TMonthStr read FSeptember write SetSeptember;
    property October: TMonthStr read FOctober write SetOctober;
    property November: TMonthStr read FNovember write SetNovember;
    property December: TMonthStr read FDecember write SetDecember;
    property UseIntlNames: Boolean read FUseIntlNames write SetUseIntlNames;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TNameOfDays = class(TPersistent)
  private
    FMonday: TDayStr;
    FTuesday: TDayStr;
    FWednesday: TDayStr;
    FThursday: TDayStr;
    FFriday: TDayStr;
    FSaturday: TDayStr;
    FSunday: TDayStr;
    FOnChange: TNotifyEvent;
    FUseIntlNames: Boolean;
    procedure SetFriday(const Value: TDayStr);
    procedure SetMonday(const Value: TDayStr);
    procedure SetSaturday(const Value: TDayStr);
    procedure SetSunday(const Value: TDayStr);
    procedure SetThursday(const Value: TDayStr);
    procedure SetTuesday(const Value: TDayStr);
    procedure SetWednesday(const Value: TDayStr);
    procedure SetUseIntlNames(const Value: Boolean);
  protected
    procedure Changed;
    procedure InitIntl;
  public
    constructor Create;
    destructor Destroy; override;
    function GetDay(i: integer): string;
  published
    property Monday: TDayStr read FMonday write SetMonday;
    property Tuesday: TDayStr read FTuesday write SetTuesday;
    property Wednesday: TDayStr read FWednesday write SetWednesday;
    property Thursday: TDayStr read FThursday write SetThursday;
    property Friday: TDayStr read FFriday write SetFriday;
    property Saturday: TDayStr read FSaturday write SetSaturday;
    property Sunday: TDayStr read FSunday write SetSunday;
    property UseIntlNames: Boolean read FUseIntlNames write SetUseIntlNames;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TCalendarBrowsers = class(TPersistent)
  private
    FPrevMonth: Boolean;
    FNextMonth: Boolean;
    FPrevYear: Boolean;
    FNextYear: Boolean;
    FOnChange: TNotifyEvent;
    procedure SetNextMonth(const Value: Boolean);
    procedure SetNextYear(const Value: Boolean);
    procedure SetPrevMonth(const Value: Boolean);
    procedure SetPrevYear(const Value: Boolean);
  public
    constructor Create;
    procedure Changed;
  published
    property PrevMonth: Boolean read FPrevMonth write SetPrevMonth default True;
    property PrevYear: Boolean read FPrevYear write SetPrevYear default True;
    property NextMonth: Boolean read FNextMonth write SetNextMonth default True;
    property NextYear: Boolean read FNextYear write SetNextYear default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TPlannerMonthViewPrintOptions = class(TPersistent)
  private
    FFooterSize: Integer;
    FLeftMargin: Integer;
    FRightMargin: Integer;
    FHeaderSize: Integer;
    FHeaderFont: TFont;
    FFooterFont: TFont;
    FOrientation: TPrinterOrientation;
    FFooter: TStrings;
    FHeader: TStrings;
    FHeaderAlignment: TAlignment;
    FFooterAlignment: TAlignment;
    FJobname: string;
    procedure SetFooter(const Value: TStrings);
    procedure SetFooterFont(const Value: TFont);
    procedure SetHeader(const Value: TStrings);
    procedure SetHeaderFont(const Value: TFont);
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Footer: TStrings read FFooter write SetFooter;
    property FooterAlignment: TAlignment read FFooterAlignment write FFooterAlignment default taLeftJustify;
    property FooterFont: TFont read FFooterFont write SetFooterFont;
    property FooterSize: Integer read FFooterSize write FFooterSize default 0;
    property Header: TStrings read FHeader write SetHeader;
    property HeaderAlignment: TAlignment read FHeaderAlignment write FHeaderAlignment default taLeftJustify;
    property HeaderFont: TFont read FHeaderFont write SetHeaderFont;
    property HeaderSize: Integer read FHeaderSize write FHeaderSize default 0;
    property JobName: string read FJobname write FJobname;
    property LeftMargin: Integer read FLeftMargin write FLeftMargin default 0;
    property Orientation: TPrinterOrientation read FOrientation write FOrientation default poPortrait;
    property RightMargin: Integer read FRightMargin write FRightMargin default 0;
  end;


  TPlannerMonthViewLook = (lookFlat, look3D);

  TCustomMonthViewPanel = class(TCustomPanel)
  private
  protected
    procedure UpdateYearStart; virtual;
    procedure DoPaint; virtual;
    procedure RepaintDate(dt: TDateTime); virtual;
  published
    property Visible;
  end;

  TImageChangeEvent = procedure(Sender: TObject) of object;

  TEditDoneEvent = procedure(Sender: TObject; ModalResult: TModalResult) of object;
  TItemMovedEvent = procedure(Sender: TObject; APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime) of object;
  TItemSizedEvent = TItemMovedEvent;

  TItemAllowMovingEvent = procedure(Sender: TObject; APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime; var Allow: Boolean) of object;
  TItemAllowSizingEvent = TItemAllowMovingEvent;
 
  TDayCellPaintEvent = procedure(Sender: TObject; Date: TDateTime; var Caption: string; CaptionBrush: TBrush; AFont: TFont; var BKColor, BKColorTo: TColor) of object;

  TDayDrawEvent = procedure(Sender: TObject; Date: TDateTime; Canvas: TCanvas; ARect: TRect; Selected: Boolean) of object;

  TPlannerMonthViewItems = class(TPlannerItems)
  private
    FPlannerMonthView: TPlannerMonthView;
  protected
    property PlannerMonthView: TPlannerMonthView read FPlannerMonthView write FPlannerMonthView;
  public
    function GetItemClass: TCollectionItemClass; override;
    procedure SetConflicts; override;
    function FindItemAtDate(ADate: TDateTime; X, Y: integer): TPlannerItem;
    function NumItemsAtDate(ADate: TDateTime): integer;
    procedure FocusItem(APlannerItem: TPlannerItem);
    function HasMonthPlannerItem(FromDate, ToDate: TDateTime): Boolean;
  published
  end;

  TItemScroller = class(TObject)
  private
    FMin: integer;
    FMax: integer;
    FPosition: integer;
    FVisible: Boolean;
    procedure SetMax(const Value: integer);
    procedure SetMin(const Value: integer);
    procedure SetPosition(const Value: integer);
    procedure SetVisible(const Value: Boolean);
  protected
  public
    constructor Create;
    function CanGoForward: Boolean;
    function CanGoBack: Boolean;
    property Min: integer read FMin write SetMin;
    property Max: integer read FMax write SetMax;
    property Position: integer read FPosition write SetPosition;
    property Visible: Boolean read FVisible write SetVisible;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TPlannerMonthView = class(TCustomMonthViewPanel, ITMSStyle)
  private
    FTMSStyle: TTMSStyle;
    FDesignTime: boolean;
    FHTMLFactor: Double;
    FPlanner: TPlanner;
    DateCol: TSelDateItems;
    xoffset, yoffset: integer;
    SelDate: TDatetime;
    Thedate: TDatetime;
    Clkdate: TDatetime;
    movdate: TDatetime;
    initdate: TDatetime;
    mousesel: Boolean;
    showhintbusy: Boolean;
    fLastHintPos: TPoint;
    dx, dy: word;
    lblx1, lblx2, lblx3: word;
    fFont: TFont;
    xposin, yposin: integer;
    flgl, flgr, flgla, dflgl, dflgr, flgt: Boolean;
    labels: string;
    EventHint: string;
    BrowserHint: string;
    FShowToday: Boolean;
    FLook: TPlannerMonthViewLook;
    FAllowItemEdit: Boolean;
    FBrowsers: TCalendarBrowsers;
    FMonthSelect: Boolean;
    FMultiSelect: Boolean;
    FDisjunctSelect: Boolean;
    FYearStartAt: TYearStartAt;
    FNameOfDays: TNameOfDays;
    FNameOfMonths: TNameOfMonths;
    FMaxDate: TMinMaxDate;
    FMinDate: TMinMaxDate;
    FTextcolor: TColor;
    FSelectColor: TColor;
    FSelectFontColor: TColor;
    FInactiveColor: TColor;
    FFocusColor: TColor;
    FInverscolor: TColor;
    FInversBkColor: TColor;
    FInversBkColorTo: TColor;
    FWeekendColor: TColor;
    FWeekendTextColor: TColor;
    FHeaderColor: TColor;
    FShowWeeks: Boolean;
    FStartDay: Integer;
    FDay, FMonth, FYear: word;
    FDayFont: TFont;
    FWeekFont: TFont;
    FWeekName: string;
    FOnDaySelect: TDaySelectEvent;
    FOnDblClick: TDaySelectEvent;
    FOnMonthSelect: TNotifyEvent;
    FOnGetDateEventHint: TGetDateEventHint;
    FOnDateChange: TDateChangeEvent;
    FOnMonthChange: TDateChangeEvent;
    FOnYearChange: TDateChangeEvent;
    FOnDayChange: TDateChangeEvent;
    FOnMonthChanged: TDateChangeEvent;
    FOnYearChanged: TDateChangeEvent;
{$IFDEF USEIMAGE}
    FImage: TAdvImage;
    FBackgroundPosition: TBackgroundPosition;
{$ENDIF}
    FShowDaysAfter: Boolean;
    FShowDaysBefore: Boolean;
    FShowSelection: Boolean;
    FShowSelectionFull: Boolean;
    FShowMonthPopup: Boolean;
    FShowYearPopup: Boolean;
    FShowFocusRectangle: Boolean;
    FWeekSelect: Boolean;
    FAllDaySelect: Boolean;
    FOnCancelledChange: TCancelledChangeEvent;
    FOnWeekSelect: TNotifyEvent;
    FOnAllDaySelect: TNotifyEvent;
    FUpdateCount: Integer;
    FCaptionColor: TColor;
    FReturnIsSelect: Boolean;
    FLineColor: TColor;
    FLine3D: Boolean;
    FGradientStartColor: TColor;
    FGradientEndColor: TColor;
    FGradientDirection: TGradientDirection;
    FMonthGradientStartColor: TColor;
    FMonthGradientEndColor: TColor;
    FMonthGradientDirection: TGradientDirection;
    FGlyphs: TCalGlyphs;
    FOldCursor: TCursor;
    FHintPrevYear: string;
    FHintPrevMonth: string;
    FHintNextMonth: string;
    FHintNextYear: string;
    FUseTheme: Boolean;
    FTodayStyle: TTodayStyle;
    FCanvas: TCanvas;
    FBorderXP: Boolean;
    FCaptionHoverColor: TColor;
    FWeekNameY: Integer;
    FDayNumberHeight: integer;
    FShowLines: Boolean;
    FTrackColor: TColor;
    FDefaultItem: TPlannerItem;
    FDefaultItems: TPlannerMonthViewItems;
    FOnItemExit: TItemEvent;
    FOnItemEnter: TItemEvent;
    FOnItemActivate: TItemEvent;
    FOnItemDeActivate: TItemEvent;
    FOnItemSelect: TItemEvent;
    FOnItemUnSelect: TItemEvent;
    FOnItemUpdate: TNotifyEvent;
    FOnItemStartEdit: TItemEvent;
    FOnItemEndEdit: TItemEvent;
    FTrackOnly: Boolean;
    FTrackBump: Boolean;
    FTrackProportional: Boolean;
    FTrackWidth: Integer;
    FURLGlyph: TBitmap;
    FURLColor: TColor;
    FDeleteGlyph: TBitmap;
    FShadowColor: TColor;
    FFlashColor: TColor;
    FFlashFontColor: TColor;
    FDragItem: Boolean;
    FDirectDrag: Boolean;
    FOnItemText: TPlannerItemText;
    FPaintMarginLX: Integer;
    FPaintMarginBY: Integer;
    FPaintMarginRX: Integer;
    FPaintMarginTY: Integer;
    FPlannerImages: TImageList;
    FContainer: TPictureContainer;
    FImageCache: THTMLPictureCache;
    FOnItemBalloon: TItemBalloonEvent;
    FOnDateBalloon: TDateBalloonEvent;
    FImageOffsetX: integer;
    FImageOffsetY: integer;
    FAttachementGlyph: TBitmap;
    FColorCurrent: TColor;
    FColorCurrentItem: TColor;
    FItemSpace: Integer;
    FPlannerMonthItems: TPlannerMonthViewItems;
    FCaptionFont: TFont;
    FCaptionHeight: Integer;
    FShowCaption: Boolean;
    FDayFontHeight: Integer;
    FMemo: TPlannerMemo;
    FMaskEdit: TPlannerMaskEdit;
    FToolTipPos: TDateTime;
    FRichEdit: TMonthPlannerRichEdit;
    FBalloon: TBalloonSettings;
    FOnCustomEdit: TCustomEditEvent;
    FDirectMove: Boolean;
    FMouseDownMove: Boolean;
    FMouseXY: TPoint;
    FStartMovedate: TDatetime;
    FOverlap: Boolean;
    FMouseOverTrack: Boolean;
    FMouseOnItemStart: Boolean;
    FMouseOnItemEnd: Boolean;
    FMouseDownResize: Boolean;
    FAutoInsDel: Boolean;
    FPopupPlannerItem: TPlannerItem;
    FHintItem: TPlannerItem;
    FOnItemCreated: TItemEvent;
    FOnItemDelete: TItemEvent;
    FOnItemDeleted: TItemEvent;
    FOnItemMove: TItemMovedEvent;
    FOnItemMoving: TItemAllowMovingEvent;    
    FOnItemSize: TItemSizedEvent;
    FOnItemSizing: TItemAllowSizingEvent;
    FCaptionColorTo: TColor;
    FCaptionGradientDirection: TGradientDirection;
    FOnGetDayProp: TDayCellPaintEvent;
    FOnDayDraw: TDayDrawEvent;
    FAutoChangeMonth: Boolean;
    FWeekWidth: Integer;
    FDragImage: TDragImageList;
    FDragItemAlways: boolean;
    FDragItemImage: boolean;
    {$IFDEF TMSSKINS}
    FSkin: TPlannerSkin;
    {$ENDIF}
    FItemPopup: TPopupMenu;
    FShowCurrent: Boolean;
    FShowCurrentItem: Boolean;
    FOnItemRightClick: TItemEvent;
    FOnItemLeftClick: TItemEvent;
    FOnItemDblClick: TItemEvent;
    FOnItemURLClick: TItemLinkEvent;
    FOnItemAttachementClick: TItemLinkEvent;
    FOnItemHint: TItemHintEvent;
    FOnItemPopupPrepare: TItemPopupPrepareEvent;
    FOnItemDrag: TItemDragEvent;
    FOnPrintHeader: TPlannerPrintHFEvent;
    FOnPrintFooter: TPlannerPrintHFEvent;
    FOnPrintStart: TPlannerPrintEvent;    
    FMaxItemsDisplayed: Integer;
    FShowScrollColumn: Boolean;
    FArrowColor: TColor;
    FOldSelDate: TDateTime;
    FDateFormat: string;
    FPrintOptions: TPlannerMonthViewPrintOptions;
    FItemScrollerAry: array[1..6] of TItemScroller;
    FHToolTip: THandle;
    FToolTipBuffer: array[0..4096] of char;
    FAutoThemeAdapt: Boolean;
    FStyle: TPlannerMonthViewStyle;
    FMouseDownStartDate: TDateTime;
    FMouseDownEndDate: TDateTime;
    FDayAlignment: TAlignment;
    FDayCaptionAlignment: TAlignment;
    FFindIndex: integer;
    FTodayColor: TColor;
    FTodayColorTo: TColor;
    FIsEditing: boolean;
    FCompletion: TCompletion;
    FLayer: integer;
    procedure WMNotify(var Message: TWMNOTIFY); message WM_NOTIFY;
    procedure WMCommand(var Message: TWMCommand); message WM_COMMAND;
    procedure WMEraseBkGnd(var Message: TWMEraseBkGnd); message WM_ERASEBKGND;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure CMWantSpecialKey(var Msg: TCMWantSpecialKey); message CM_WANTSPECIALKEY;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure BalloonInit;
    procedure BalloonDone;
    procedure BalloonChange(Sender: TObject);
    procedure YearStartAtChanged(Sender: TObject);
    procedure CreateToolTip;
    procedure AddToolTip(IconType: Integer; Text, Title: string);
    procedure DestroyToolTip;
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
    procedure SetLabel(mo, ye: word);
    procedure ChangeMonth(dx: integer);
    procedure ChangeYear(dx: integer);
    procedure DiffCheck(dt1, dt2: tdatetime);
    function DiffMonth(dx: integer): TDateTime;
    function CheckDateRange(dt: TDateTime): Boolean;
    function CheckMonth(dt: TDateTime): Boolean;
    function DaysInMonth(mo, ye: word): word;
    procedure PaintArrowLeft(PaintRect: TRect);
    procedure PaintArrowRight(PaintRect: TRect);
    procedure PaintDblArrowLeft(PaintRect: TRect);
    procedure PaintDblArrowRight(PaintRect: TRect);
    procedure PaintLabel(PaintRect: TRect);
    procedure PaintProc(PaintRect: TRect; Print: Boolean);
    procedure PaintMonthPlannerItem(Canvas: TCanvas; ARect: TRect; APlannerItem: TPlannerItem; ForwardArrow, BackArrow, Print: Boolean);
    procedure CheckAndDrawEvent(d: TDateTime; R: TRect; RowNo, ColNo: Integer; Print: Boolean);
    procedure PaintUpScrollBtn(RowNo: integer);
    procedure PaintDownScrollBtn(RowNo: integer);
    procedure SetLook(AValue: TPlannerMonthViewLook);
    procedure SetShowToday(AValue: Boolean);
    procedure SetDayFont(AValue: TFont);
    procedure SetWeekFont(AValue: TFont);
    procedure SetTextColor(AColor: TColor);
    procedure SetFocusColor(AColor: TColor);
    procedure SetInversColor(AColor: TColor);
    procedure SetInversBkColor(AColor: TColor);
    procedure SetInversBkColorTo(AColor: TColor);
    procedure SetWeekendTextColor(AColor: TColor);
    procedure SetWeekendColor(AColor: TColor);
    procedure SetSelectColor(AColor: TColor);
    procedure SetSelectFontColor(AColor: TColor);
    procedure SetInactiveColor(AColor: TColor);
    procedure SetHeaderColor(AColor: TColor);
    procedure SetWeekName(const Value: string);
    procedure PlanFontChanged(Sender: TObject);
    procedure SetFont(Value: TFont);
    procedure SetNameofDays(ANameofDays: TNameOfDays);
    procedure SetNameofMonths(ANameofMonths: TNameOfMonths);
    procedure SetShowWeeks(AValue: Boolean);
    procedure SetShowCaption(AValue: Boolean);
    procedure SetStartDay(AValue: integer);
    procedure SetCalDay(AValue: word);
    procedure SetCalMonth(AValue: word);
    procedure SetCalYear(AValue: word);
    procedure SetDayAlignment(const AAlignment: TAlignment);
    procedure SetDayCaptionAlignment(const AAlignment: TAlignment);
    function GetCalDay: word;
    function GetCalMonth: word;
    function GetMonth(var dt: TDateTime): word;
    function GetCalYear: word;
    function GetYear(dt: TDatetime): integer;
    function XYToDate(X, Y: integer; Change: Boolean): TDateTime; // Does change the month
    function GetDateProc: TDateTime;
    procedure SetDateCol(const Value: TSelDateItems);
    function GetDateCol: TSelDateItems;
    procedure DoMonthPopup;
    procedure DoYearPopup;
    procedure PropsChanged(Sender: TObject);
    procedure SetLineColor(AValue: TColor);
    procedure SetLine3D(AValue: Boolean);
    procedure DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; r: TRect; Direction: Boolean);
{$IFDEF USEIMAGE}
    procedure SetImage(const Value: TAdvImage);
    procedure SetBackgroundPosition(const Value: TBackgroundPosition);
    procedure BackgroundChanged(Sender: TObject);
{$ENDIF}
    function GetDatesAsText: string;
    procedure SetShowDaysAfter(const Value: Boolean);
    procedure SetShowDaysBefore(const Value: Boolean);
    procedure SetShowSelection(const Value: Boolean);
    procedure SetShowSelectionFull(const Value: Boolean);
    procedure SetShowFocusRectangle(const Value: Boolean);
    procedure SetCaptionColor(const Value: TColor);
    procedure SetGradientStartColor(AValue: TColor);
    procedure SetGradientEndColor(AValue: TColor);
    procedure SetGradientDirection(AValue: TGradientDirection);
    procedure SetMonthGradientStartColor(AValue: TColor);
    procedure SetMonthGradientEndColor(AValue: TColor);
    procedure SetMonthGradientDirection(AValue: TGradientDirection);
    procedure SetGlyphs(const Value: TCalGlyphs);
    procedure SetHintPrevYear(AValue: string);
    procedure SetHintPrevMonth(AValue: string);
    procedure SetHintNextMonth(AValue: string);
    procedure SetHintNextYear(AValue: string);
    procedure SetTodayStyle(const Value: TTodayStyle);
    procedure SetTodayColor(const Value: TColor);
    procedure SetTodayColorTo(const Value: TColor);
    function NumRows: Integer;
    function NumCols: Integer;
    procedure SetBorderXP(const Value: Boolean);
    procedure SetShowLines(const Value: Boolean);
    procedure SetTrackColor(const Value: TColor);
    procedure SetDefaultItem(const Value: TPlannerItem);
    procedure SetTrackBump(const Value: Boolean);
    procedure SetTrackOnly(const Value: Boolean);
    procedure SetTrackProportional(const Value: Boolean);
    procedure SetTrackWidth(const Value: Integer);
    procedure SetURLColor(const Value: TColor);
    procedure SetURLGlyph(const Value: TBitmap);
    procedure SetDeleteGlyph(const Value: TBitmap);
    procedure SetShadowColor(const Value: TColor);
    procedure SetFlashColor(const Value: TColor);
    procedure SetFlashFontColor(const Value: TColor);
    procedure SetImages(const Value: TImageList);
    procedure SetAttachementGlyph(const Value: TBitmap);
    procedure SetColorCurrent(const Value: TColor);
    procedure SetColorCurrentItem(const Value: TColor);
    procedure SetAutoThemeAdapt(const Value: Boolean);
    procedure SetStyle(const Value: TPlannerMonthViewStyle);
    procedure SetItemSpace(const Value: Integer);
    procedure SetCaptionFont(const Value: TFont);
    procedure SetCaptionHeight(const Value: integer);
    function GetCaptionHeight: Integer;

    procedure SetEditDirectSelection(ARect: TRect; X, Y: Integer);
    procedure StartEdit(APlannerItem: TPlannerItem; aDate: TDateTime; X, Y: Integer);
    function ItemRectAtDate(APlannerItem: TPlannerItem; ADate: TDateTime): TRect;
    function ItemRectAtRow(APlannerItem: TPlannerItem; RowNo: integer): TRect;
    function FullItemRectAtDate(APlannerItem: TPlannerItem; ADate: TDateTime): TRect;
    procedure SetCaptionColorTo(const Value: TColor);
    procedure SetCaptionGradientDirection(const Value: TGradientDirection);
    procedure SetWeekWidth(const Value: Integer);
    procedure SetShowCurrent(const Value: Boolean);
    procedure SetShowCurrentItem(const Value: Boolean);

    procedure URLGlyphOnChange(Sender: TObject);
    procedure AttachementGlyphOnChange(Sender: TObject);

    procedure PlannerItemActivate(Sender: TObject; Item: TPlannerItem);
    procedure PlannerItemDeActivate(Sender: TObject; Item: TPlannerItem);
    procedure PlannerItemEnter(Sender: TObject; Item: TPlannerItem);
    procedure PlannerItemExit(Sender: TObject; Item: TPlannerItem);
    procedure PlannerItemText(Sender: TObject; Item: TPlannerItem; var Text: string);
    procedure PlannerItemSelected(Sender: TObject; Item: TPlannerItem);
    procedure SetMaxItemsDisplayed(const Value: Integer);
    procedure SetShowScrollColumn(const Value: Boolean);
    function GetUpScrollBtnRect(RowNo: Integer): TRect;
    function GetDownScrollBtnRect(RowNo: Integer): TRect;
    function GetMaxConflict(FromDate, ToDate: TDateTime): integer;
    procedure SetVersion(const Value: string);
    function GetVersion: string;
    procedure SetCursorEx(const Value: TCursor);
    function GetCursorEx: TCursor;
    function GetCompletion: TCompletion;
    procedure SetCompletion(const Value: TCompletion);
    function GetRowScrollPositoin(index: integer): integer;
    procedure SetRowScrollPosition(index: integer; const Value: integer);
    procedure SetDragItemImage(const Value: boolean);
    procedure SetLayer(const Value: integer);
    procedure CaptionFontChange(Sender: TObject);
  protected
    procedure ReadTMSStyle(Reader: TReader);
    procedure WriteTMSStyle(Writer: TWriter);
    procedure DefineProperties(Filer: TFiler); override;
    procedure SetDateProc(const Value: TDatetime); virtual;
    procedure InvalidateRectangle(ARect: TRect; Bkg: Boolean);
    procedure UpdateYearStartAtISO; virtual;
    procedure DoPaint; override;
    procedure RepaintDate(dt: TDateTime); override;
    procedure PaintCalendar(ACanvas: TCanvas; PaintRect: TRect; Print: Boolean);
    procedure Paint; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure MouseMove(Shift: TShiftState; X, Y: integer); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: integer); override;
    procedure KeyPress(var Key: char); override;
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure Loaded; override;
    function GetDragImages: TDragImageList; override;
    procedure WndProc(var Msg: TMessage); override;
    function CanResize(var NewWidth, NewHeight: Integer): Boolean; override;
    procedure ThemeAdapt;
    procedure CreateDragImage(APlannerItem: TPlannerItem);
    procedure DateColChanged(Sender: TObject);
    procedure CompletionChanged(Sender: TObject);
    {$IFDEF TMSSKINS}
    procedure SkinChange(Sender: TObject);
    {$ENDIF}
    //function HasEvent(dt: TDateTime; var EventItem: TSelDateItem): Boolean; virtual;
    function DiffYear(dx: integer): TDateTime;
    procedure DoChangeMonth(dt1, dt2: TDateTime); virtual;
    procedure DoChangeYear(dt1, dt2: TDateTime); virtual;

    procedure DoMonthChanged(dt1, dt2: TDateTime); virtual;
    procedure DoYearChanged(dt1, dt2: TDateTime); virtual;
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure ItemSelected(Item: TPlannerItem); virtual;
    procedure ItemUnSelected(Item: TPlannerItem); virtual;
    procedure ItemUpdated(Sender:TObject); virtual;
    procedure ItemMoved(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime); virtual;
    procedure ItemMoving(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime; var Allow: Boolean); virtual;
    procedure ItemSized(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime); virtual;
    procedure ItemSizing(APlannerItem: TPlannerItem; FromStartDate, FromEndDate, ToStartDate, ToEndDate: TDateTime; var Allow: Boolean); virtual;
    procedure ItemEdited(Sender: TObject; Item: TPlannerItem); virtual;

    function GetVersionNr: Integer; virtual;
    function GetVersionString: string; virtual;

    function GetItemScroller(RowNo: integer): TItemScroller;
    procedure SetItemScrollerPosition;

    property UseTheme: Boolean read FUseTheme;
    property PaintMarginTY: Integer read FPaintMarginTY write FPaintMarginTY;
    property PaintMarginLX: Integer read FPaintMarginLX write FPaintMarginLX;
    property PaintMarginBY: Integer read FPaintMarginBY write FPaintMarginBY;
    property PaintMarginRX: Integer read FPaintMarginRX write FPaintMarginRX;
    property ImageOffsetX: integer read FImageOffsetX write FImageOffsetX;
    property ImageOffsetY: integer read FImageOffsetY write FImageOffsetY;

    property FlashColor: TColor read FFlashColor write SetFlashColor;
    property FlashFontColor: TColor read FFlashFontColor write SetFlashFontColor;
    function CreateItems(AOwner: TCustomPlanner): TPlannerMonthViewItems; virtual;
    procedure DoPrint(ACanvas: TCanvas);
    procedure DoItemPopupPrepare(Sender: TObject; PopupMenu:TPopupMenu; Item: TPlannerItem); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure CreateWnd; override;
    procedure SetBounds(ALeft, ATop, AWidth, AHeight: Integer); override;
    procedure SetComponentStyle(AStyle: TTMSStyle);
    function GetComponentStyle: TTMSStyle;

    procedure TextToRich(const RtfText: string);
    function RichToText: string;
    function DateToRect(dt: TDateTime): TRect;

    procedure SetDate(da, mo, ye: word);
    procedure GetDate(var da, mo, ye: word);
    procedure GetStartDate(var da, mo, ye: word);
    procedure GetEndDate(var da, mo, ye: word);
    procedure BeginUpdate;
    procedure EndUpdate;
    procedure ResetUpdate;

    property IsEditing: boolean read FIsEditing write FIsEditing;

    function CreateItemAtSelection: TPlannerItem; virtual;
    function CreateItem: TPlannerItem; virtual;
    procedure FreeItem(APlannerItem: TPlannerItem); virtual;
    procedure UpdateItem(APlannerItem:TPlannerItem); virtual;
    procedure DoDaySelect(ADay: TDateTime); virtual;
    function DateAtXY(x, y: Integer; var ADate: TDateTime): Boolean;
    function DateToXY(dt: TDateTime): TPoint;

    function FindFirstItemAtDate(ADate: TDateTime): TPlannerItem;
    function FindNextItemAtDate(ADate: TDateTime): TPlannerItem;
    function HasPlannerItem(FromDate: TDateTime; ToDate: TDateTime = 0): Boolean;

    property Date: TDatetime read GetDateProc write SetDateProc;
    property Dates: TSelDateItems read GetDateCol write SetDateCol;
    property DatesAsText: string read GetDatesAsText;
    property PopupPlannerItem: TPlannerItem read FPopupPlannerItem;

    property DirectDrag: Boolean read FDirectDrag write FDirectDrag;
    function FirstDate: TDateTime;
    function LastDate: TDateTime;

    procedure Print;
    procedure PrintTo(Canvas: TCanvas);
    property AllDaySelect: Boolean read FAllDaySelect write FAllDaySelect default False;

    property Layer: integer read FLayer write SetLayer;
    property Style: TPlannerMonthViewStyle read FStyle write SetStyle;

    property RowScrollPosition[index: integer]: integer read GetRowScrollPositoin write SetRowScrollPosition;

    property VersionNr: Integer read GetVersionNr;
    property VersionString: string read GetVersionString;
    property TMSStyle: TTMSStyle read FTMSStyle write FTMSStyle;
  published
    property Align;
    property Anchors;
    property Constraints;
    property AllowItemEdit: Boolean read FAllowItemEdit write FAllowItemEdit default True;
    property AttachementGlyph: TBitmap read FAttachementGlyph write SetAttachementGlyph;
    property AutoInsDel: Boolean read FAutoInsDel write FAutoInsDel default False;
    property AutoChangeMonth: Boolean read FAutoChangeMonth write FAutoChangeMonth default true;
    property AutoThemeAdapt: Boolean read FAutoThemeAdapt write SetAutoThemeAdapt default False;
{$IFDEF USEIMAGE}
    property Background: TAdvImage read FImage write SetImage;
    property BackgroundPosition: TBackgroundPosition read FBackgroundPosition write SetBackgroundPosition default bpTiled;
{$ENDIF}
    property Balloon: TBalloonSettings read FBalloon write FBalloon;
    property BevelInner;
    property BevelOuter;
    property BevelWidth;
    property BorderWidth;
    property BorderStyle;    
    property BorderXP: Boolean read FBorderXP write SetBorderXP default True;

    property Browsers: TCalendarBrowsers read FBrowsers write FBrowsers;
    property CaptionColor: TColor read FCaptionColor write SetCaptionColor default clNone;
    property CaptionColorTo: TColor read FCaptionColorTo write SetCaptionColorTo default clNone;
    property CaptionGradientDirection: TGradientDirection read FCaptionGradientDirection write SetCaptionGradientDirection default gdHorizontal;
    property CaptionHoverColor: TColor read FCaptionHoverColor write FCaptionHoverColor default clHighlight;
    property CaptionFont: TFont read FCaptionFont write SetCaptionFont;
    property CaptionHeight: Integer read GetCaptionHeight write SetCaptionHeight default 13;
    property Color;
    property ColorCurrent: TColor read FColorCurrent write SetColorCurrent default clYellow;
    property ColorCurrentItem: TColor read FColorCurrentItem write SetColorCurrentItem default clLime;
    property Completion: TCompletion read GetCompletion write SetCompletion;
    property Cursor: TCursor read GetCursorEx write SetCursorEx;
    property DateFormat: string read FDateFormat write FDateFormat;
    property DayAlignment: TAlignment read FDayAlignment write SetDayAlignment default taRightJustify;
    property DayCaptionAlignment: TAlignment read FDayCaptionAlignment write SetDayCaptionAlignment default taLeftJustify;

    property DayFont: TFont read FDayFont write SetDayFont;
    property DefaultItem: TPlannerItem read FDefaultItem write SetDefaultItem;
    property DeleteGlyph: TBitmap read FDeleteGlyph write SetDeleteGlyph;
    property DirectMove: Boolean read FDirectMove write FDirectMove default False;
    property DisjunctSelect: Boolean read FDisjunctSelect write FDisjunctSelect default False;
    property DragItem: Boolean read FDragItem write FDragItem default False;
    property DragItemAlways: boolean read FDragItemAlways write FDragItemAlways default False;
    property DragItemImage: boolean read FDragItemImage write SetDragItemImage default False;
    property DragMode;
    property FocusColor: TColor read FFocusColor write SetFocusColor default clHighlight;
    property Font: TFont read fFont write SetFont;
    property Glyphs: TCalGlyphs read FGlyphs write SetGlyphs;
    property GradientStartColor: TColor read FGradientStartColor write SetGradientStartColor default clWhite;
    property GradientEndColor: TColor read FGradientEndColor write SetGradientEndColor default clBtnFace;
    property GradientDirection: TGradientDirection read FGradientDirection write SetGradientDirection default gdHorizontal;
    property HeaderColor: TColor read FHeaderColor write SetHeaderColor default clNone;
    property HintPrevYear: string read FHintPrevYear write SetHintPrevYear;
    property HintPrevMonth: string read FHintPrevMonth write SetHintPrevMonth;
    property HintNextMonth: string read FHintNextMonth write SetHintNextMonth;
    property HintNextYear: string read FHintNextYear write SetHintNextYear;
    property InActiveColor: TColor read FInactiveColor write SetInactiveColor default clGray;
    property InversColor: TColor read FInversColor write SetInversColor default clTeal;
    property InversBkColor: TColor read FInversBkColor write SetInversBkColor default clNone;
    property InversBkColorTo: TColor read FInversBkColorTo write SetInversBkColorTo default clNone;
    property Items: TPlannerMonthViewItems read FPlannerMonthItems write FPlannerMonthItems;
    property ItemPopup: TPopupmenu read FItemPopup write FItemPopup;
    property ItemSpace: Integer read FItemSpace write SetItemSpace default 2;
    property LineColor: TColor read FLineColor write SetLineColor default clGray;
    property Line3D: Boolean read FLine3D write SetLine3D default true;
    property Look: TPlannerMonthViewLook read fLook write SetLook default lookFlat;
    property MaxDate: TMinMaxDate read FMaxDate write FMaxDate;
    property MinDate: TMinMaxDate read FMinDate write FMinDate;
    property MaxItemsDisplayed: Integer read FMaxItemsDisplayed write SetMaxItemsDisplayed default 3;
    property MonthGradientStartColor: TColor read FMonthGradientStartColor write SetMonthGradientStartColor default clNone;
    property MonthGradientEndColor: TColor read FMonthGradientEndColor write SetMonthGradientEndColor default clNone;
    property MonthGradientDirection: TGradientDirection read FMonthGradientDirection write SetMonthGradientDirection default gdHorizontal;
    property MultiSelect: Boolean read FMultiSelect write FMultiSelect default False;
    property NameOfDays: TNameOfDays read FNameOfDays write SetNameOfDays;
    property NameOfMonths: TNameOfMonths read FNameOfMonths write SetNameOfMonths;
    property PictureContainer: TPictureContainer read FContainer write FContainer;
    property PlannerImages: TImageList read FPlannerImages write SetImages;
    property PopupMenu;
    property PrintOptions: TPlannerMonthViewPrintOptions read FPrintOptions write FPrintOptions;
    property ReturnIsSelect: Boolean read FReturnIsSelect write FReturnIsSelect default False;
    property SelectColor: TColor read FSelectColor write SetSelectColor default clTeal;
    property SelectFontColor: TColor read FSelectFontColor write SetSelectFontColor default clWhite;
    property ShadowColor: TColor read FShadowColor write SetShadowColor default clGray;
    property ShowDaysBefore: Boolean read FShowDaysBefore write SetShowDaysBefore default True;
    property ShowDaysAfter: Boolean read FShowDaysAfter write SetShowDaysAfter default True;
    property ShowHint;
    {$IFDEF TMSSKINS}
    property Skin: TPlannerSkin read FSkin write FSkin;
    {$ENDIF}
    property Overlap: Boolean read FOverlap write FOverlap default True;
    property ParentShowHint;
    property ShowCaption: Boolean read FShowCaption write SetShowCaption default True;
    property ShowCurrent: Boolean read FShowCurrent write SetShowCurrent default False;
    property ShowCurrentItem: Boolean read FShowCurrentItem write SetShowCurrentItem default False;
    property ShowLines: Boolean read FShowLines write SetShowLines default True;
    property ShowScrollColumn: Boolean read FShowScrollColumn write SetShowScrollColumn default False;
    property ShowSelection: Boolean read FShowSelection write SetShowSelection default True;
    property ShowSelectionFull: Boolean read FShowSelectionFull write SetShowSelectionFull default True;
    property ShowYearPopup: Boolean read FShowYearPopup write FShowYearPopup default True;
    property ShowMonthPopup: Boolean read FShowMonthPopup write FShowMonthPopup default True;
    property ShowFocusRectangle: Boolean read FShowFocusRectangle write SetShowFocusRectangle default True;
    property ShowToday: Boolean read FShowToday write SetShowToday default False;
    property ShowWeeks: Boolean read FShowWeeks write SetShowWeeks default False;
    property StartDay: integer read FStartDay write SetStartDay default 7;
    property TextColor: TColor read FTextColor write SetTextColor default clBlack;
    property TabStop;
    property TabOrder;
    property TodayColor: TColor read FTodayColor write SetTodayColor default clHighlight;
    property TodayColorTo: TColor read FTodayColorTo write SetTodayColorTo default clNone;
    property TodayStyle: TTodayStyle read FTodayStyle write SetTodayStyle default tsSunken;
    property TrackColor: TColor read FTrackColor write SetTrackColor default clBlue;
    property TrackBump: Boolean read FTrackBump write SetTrackBump default False;
    property TrackOnly: Boolean read FTrackOnly write SetTrackOnly default False;
    property TrackProportional: Boolean read FTrackProportional write SetTrackProportional default False;
    property TrackWidth: Integer read FTrackWidth write SetTrackWidth default 4;
    property URLColor: TColor read FURLColor write SetURLColor default clBlue;
    property URLGlyph: TBitmap read FURLGlyph write SetURLGlyph;
    property WeekFont: TFont read FWeekFont write SetWeekFont;
    property WeekName: string read FWeekName write SetWeekName stored True;
    property WeekSelect: Boolean read FWeekSelect write FWeekSelect default False;
    property WeekendColor: TColor read FWeekendColor write SetWeekendColor default clNone;
    property WeekendTextColor: TColor read FWeekendTextColor write SetWeekendTextColor default clRed;
    property WeekWidth: Integer read FWeekWidth write SetWeekWidth default 30;
    property YearStartAt: TYearStartAt read FYearStartAt write FYearStartAt;
    property Day: word read GetCalDay write SetCalDay default 1;
    property Month: word read GetCalMonth write SetCalMonth default 1;
    property Year: word read GetCalYear write SetCalYear default 1;
    property Version: string read GetVersion write SetVersion;
    property Visible;
    property OnItemEnter: TItemEvent read FOnItemEnter write FOnItemEnter;
    property OnItemExit: TItemEvent read FOnItemExit write FOnItemExit;
    property OnItemActivate: TItemEvent read FOnItemActivate write FOnItemActivate;
    property OnItemDeActivate: TItemEvent read FOnItemDeActivate write FOnItemDeActivate;
    property OnItemDrag: TItemDragEvent read FOnItemDrag write FOnItemDrag;
    property OnItemLeftClick: TItemEvent read FOnItemLeftClick write FOnItemLeftClick;
    property OnItemRightClick: TItemEvent read FOnItemRightClick write FOnItemRightClick;
    property OnItemDblClick: TItemEvent read FOnItemDblClick write FOnItemDblClick;
    property OnItemURLClick: TItemLinkEvent read FOnItemURLClick write FOnItemURLClick;
    property OnItemAttachementClick: TItemLinkEvent read FOnItemAttachementClick write FOnItemAttachementClick;
    property OnItemHint: TItemHintEvent read FOnItemHint write FOnItemHint;
    property OnItemSelect: TItemEvent read FOnItemSelect write FOnItemSelect;
    property OnItemUnSelect: TItemEvent read FOnItemUnSelect write FOnItemUnSelect;
    property OnItemUpdate: TNotifyEvent read FOnItemUpdate write FOnItemUpdate;
    property OnItemStartEdit: TItemEvent read FOnItemStartEdit write FOnItemStartEdit;
    property OnItemEndEdit: TItemEvent read FOnItemEndEdit write FOnItemEndEdit;
    property OnItemText: TPlannerItemText read FOnItemText write FOnItemText;
    property OnItemPopupPrepare: TItemPopupPrepareEvent read FOnItemPopupPrepare write FOnItemPopupPrepare;
    property OnCustomEdit: TCustomEditEvent read FOnCustomEdit write FOnCustomEdit;
    property OnItemBalloon: TItemBalloonEvent read FOnItemBalloon write FOnItemBalloon;
    property OnDateBalloon: TDateBalloonEvent read FOnDateBalloon write FOnDateBalloon;
    property OnItemCreated: TItemEvent read FOnItemCreated write FOnItemCreated;
    property OnItemDelete: TItemEvent read FOnItemDelete write FOnItemDelete;
    property OnItemDeleted: TItemEvent read FOnItemDeleted write FOnItemDeleted;
    property OnItemMove: TItemMovedEvent read FOnItemMove write FOnItemMove;
    property OnItemMoving: TItemAllowMovingEvent read FOnItemMoving write FOnItemMoving;
    property OnItemSize: TItemSizedEvent read FOnItemSize write FOnItemSize;
    property OnItemSizing: TItemAllowSizingEvent read FOnItemSizing write FOnItemSizing;
    property OnGetDayProp: TDayCellPaintEvent read FOnGetDayProp write FOnGetDayProp;
    property OnDayDraw: TDayDrawEvent read FOnDayDraw write FOnDayDraw;
    property OnDaySelect: TDaySelectEvent read FOnDaySelect write FOnDaySelect;
    property OnDblClick: TDaySelectEvent read FOnDblClick write FOnDblClick;
    property OnMonthSelect: TNotifyEvent read FOnMonthSelect write FOnMonthSelect;
    property OnGetDateHintString: TGetDateEventHint read FOnGetDateEventHint
      write FOnGetDateEventHint;
    property OnMonthChange: TDateChangeEvent read FOnMonthChange write FOnMonthChange;
    property OnYearChange: TDateChangeEvent read FOnYearChange write FOnYearChange;
    property OnMonthChanged: TDateChangeEvent read FOnMonthChanged write FOnMonthChanged;
    property OnYearChanged: TDateChangeEvent read FOnYearChanged write FOnYearChanged;
    property OnDayChange: TDateChangeEvent read FOnDayChange write FOnDayChange;
    property OnDateChange: TDateChangeEvent read FOnDateChange write FOnDateChange;
    property OnCancelledChange: TCancelledChangeEvent
      read FOnCancelledChange write FOnCancelledChange;
    property OnWeekSelect: TNotifyEvent read FOnWeekSelect write FOnWeekSelect;
    property OnAllDaySelect: TNotifyEvent read FOnAllDaySelect write FOnAllDaySelect;
    property OnPrintHeader: TPlannerPrintHFEvent read FOnPrintHeader write FOnPrintHeader;
    property OnPrintFooter: TPlannerPrintHFEvent read FOnPrintFooter write FOnPrintFooter;
    property OnPrintStart: TPlannerPrintEvent read FOnPrintStart write FOnPrintStart;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseMove;
    property OnMouseDown;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnMouseUp;
    property OnDragDrop;
    property OnDragOver;
    property OnStartDrag;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnResize;
  end;


//{$R PLANNERMONTHVIEW.RES}

implementation

uses
  StdCtrls, Math, RichEdit, PlanHTML, PlanUtil, CommCtrl
  , ShellApi, ComObj
  ;

const
  DefaultPixelsPerInch = 96;
  // theme changed notifier
  WM_THEMECHANGED = $031A;

  {$IFDEF DELPHI_UNICODE}
  TTM_SETTITLE = (WM_USER + 33);
  {$ENDIF}
  {$IFNDEF DELPHI_UNICODE}
  TTM_SETTITLE = (WM_USER + 32);
  {$ENDIF}

  {$EXTERNALSYM TTM_SETTITLE}


{$I DELPHIXE.INC}


function CurrentXPTheme: XPColorScheme;
var
  FileName, ColorScheme, SizeName: WideString;

begin
  Result := xpNone;

  if not IsWinXP then
    Exit;

  if IsThemeActive then
  begin
    SetLength(FileName, 255);
    SetLength(ColorScheme, 255);
    SetLength(SizeName, 255);
    GetCurrentThemeName(PWideChar(FileName), 255,
    PWideChar(ColorScheme), 255, PWideChar(SizeName), 255);

    if(PWideChar(ColorScheme)='NormalColor') then
      Result := xpBlue
    else if (PWideChar(ColorScheme)='HomeStead') then
      Result := xpGreen
    else if (PWideChar(ColorScheme)='Metallic') then
      Result := xpGray
    else
      Result := xpNone;
  end
  else
   Result := xpNoTheme;
end;


{$IFNDEF DELPHI6_LVL}
function SpanOfNowAndThen(const ANow, AThen: TDateTime): TDateTime;
begin
  if ANow < AThen then
    Result := AThen - ANow
  else
    Result := ANow - AThen;
end;

function DaySpan(const ANow, AThen: TDateTime): Double;
begin
  Result := SpanOfNowAndThen(ANow, AThen);
end;

function DaysBetween(const ANow, AThen: TDateTime): Integer;
begin
  Result := Trunc(DaySpan(ANow, AThen));
end;
{$ENDIF}

function IsRtf(const Value: string): Boolean;
begin
  Result := (Pos(RtfStart, Value) = 1);
end;

{ TMonthPlannerItem }

procedure TMonthPlannerItem.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
end;

constructor TMonthPlannerItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FPosSt := TStringList.Create;
end;

destructor TMonthPlannerItem.Destroy;
begin
  FPosSt.Free;
  inherited;
end;

procedure TMonthPlannerItem.SetItemEndTime(const Value: TDateTime);
begin
  FEndTime := Value;
  ItemRealEndTime := Value;
  (Collection as TPlannerItems).SetConflicts;
end;

procedure TMonthPlannerItem.SetItemStartTime(const Value: TDateTime);
begin
  FStartTime := Value;
  ItemRealStartTime := Value;
  (Collection as TPlannerItems).SetConflicts;
end;

function TMonthPlannerItem.GetItemEndTime: TDateTime;
begin
  Result := FEndTime;
end;

function TMonthPlannerItem.GetItemStartTime: TDateTime;
begin
  Result := FStartTime;
end;

function TMonthPlannerItem.GetItemEndTimeStr: string;
var
  df: string;
begin
  if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
  begin
    df := (Collection as TPlannerMonthViewItems).PlannerMonthView.DateFormat;
    Result := FormatDateTime(df,ItemRealEndTime);
  end
  else
    Result := DateToStr(ItemRealEndTime);
end;

function TMonthPlannerItem.GetItemStartTimeStr: string;
var
  df: string;
begin
  if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
  begin
    df := (Collection as TPlannerMonthViewItems).PlannerMonthView.DateFormat;
    Result := FormatDateTime(df,ItemRealStartTime);
  end
  else
    Result := DateToStr(ItemRealStartTime);
end;

function TMonthPlannerItem.GetItemSpanTimeStr: string;
begin
  Result := ItemStartTimeStr + ' - ' + ItemEndTimeStr;
end;

procedure TMonthPlannerItem.Repaint;
begin
  if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
  begin
    (Collection as TPlannerMonthViewItems).PlannerMonthView.Invalidate;
  end;
end;

procedure TMonthPlannerItem.MoveMonthPlannerItem(NewStartTime, NewEndTime: TDateTime; Done: Boolean; var Allow: Boolean);
var
  OldStartTime, OldEndTime: TDateTime;
  OldRealStartTime, OldRealEndTime: TDateTime;

begin
  OldStartTime := ItemRealStartTime;
  OldEndTime := ItemRealEndTime;
  
  OldRealStartTime := ItemRealStartTime;
  OldRealEndTime := ItemRealEndTime;

  if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
    with ((Collection as TPlannerMonthViewItems).PlannerMonthView) do
    begin
     OldStartTime := FMouseDownStartDate;
     OldEndTime := FMouseDownEndDate;
    end;

  if Done then
  begin
    if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
      (Collection as TPlannerMonthViewItems).PlannerMonthView.ItemMoving(Self, OldStartTime, OldEndTime, NewStartTime, NewEndTime, Allow);

    if Allow then
    begin
      ItemStartTime := Int(NewStartTime);
      ItemEndTime := Int(NewEndTime);

      ItemRealStartTime := Int(ItemStartTime) + Frac(OldRealStartTime);
      ItemRealEndTime := Int(ItemEndTime) + Frac(OldRealEndTime);

      if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
        (Collection as TPlannerMonthViewItems).PlannerMonthView.ItemMoved(Self, OldStartTime, OldEndTime, ItemStartTime, ItemEndTime);
    end;
  end
  else
  begin

    if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
      (Collection as TPlannerMonthViewItems).PlannerMonthView.ItemMoving(Self, OldStartTime, OldEndTime, NewStartTime, NewEndTime, Allow);
    if Allow then
    begin
      ItemStartTime := NewStartTime;
      ItemEndTime := NewEndTime;

      ItemRealStartTime := Int(ItemStartTime) + Frac(OldRealStartTime);
      ItemRealEndTime := Int(ItemEndTime) + Frac(OldRealEndTime);
    end;
  end;
end;

function TMonthPlannerItem.GetCaptionTimeString: string;
begin
  Result := ItemStartTimeStr + ' - ' + ItemEndTimeStr;
end;

procedure TMonthPlannerItem.Update;
begin
  if Assigned((Collection as TPlannerMonthViewItems).PlannerMonthView) then
    (Collection as TPlannerMonthViewItems).PlannerMonthView.UpdateItem(self);
end;

{ TPlannerItemsEx }

function TPlannerItemsEx.GetItemClass: TCollectionItemClass;
begin
  Result := TMonthPlannerItem;
end;

{ TPlannerEx }

function TPlannerEx.CreateItems: TPlannerItems;
begin
  Result := TPlannerItemsEx.Create(Self);
end;

constructor TPlannerMonthView.Create(AOwner: TComponent);
var
  VerInfo: TOSVersioninfo;
  i: integer;
begin
  inherited Create(AOwner);

  ControlStyle := ControlStyle - [csAcceptsControls];

  FPlanner := TPlanner.Create(self);
  FPlanner.Mode.PlannerType := plMonth;
  FPlanner.ItemGap := 2;
  FPlanner.OnItemEndEdit := ItemEdited;
  FPlanner.OnItemActivate := PlannerItemActivate;
  FPlanner.OnItemDeActivate := PlannerItemDeActivate;
  FPlanner.OnItemEnter := PlannerItemEnter;
  FPlanner.OnItemExit := PlannerItemExit;
  FPlanner.OnItemText := PlannerItemText;
  FPlanner.OnItemSelect := PlannerItemSelected;
  FPlanner.TabStop := false;
  FPlanner.GridControl.TabStop := false;

//  FPlanner.OnConflictUpdate := ItemsChanged;
  FPlanner.OnItemUpdate := ItemUpdated;

  FMemo := TPlannerMemo.Create(Self);
  FMemo.Parent := Self;
  FMemo.Visible := False;
  FMemo.Width := 0;
  FMemo.Height := 0;
  FMemo.Planner := FPlanner;
  FMemo.TabStop := false;

  FMaskEdit := TPlannerMaskEdit.Create(Self);
  FMaskEdit.Parent := Self;
  FMaskEdit.Visible := False;
  FMaskEdit.Width := 0;
  FMaskEdit.Height := 0;
  FMaskEdit.TabStop := false;

  FRichEdit := TMonthPlannerRichEdit.Create(Self);
  FRichEdit.TabStop := false;
  FBalloon := TBalloonSettings.Create;
  FBalloon.OnEnableChange := BalloonChange;

  FWeekWidth := 30;
  FMaxItemsDisplayed := 3;
  FShowScrollColumn := False;
  FAllowItemEdit := True;
  FArrowColor := cl3DDkShadow;
  for i := 1 to 6 do
    FItemScrollerAry[i] := TItemScroller.Create;

  FNameOfDays := TNameofDays.Create;
  FNameOfDays.OnChange := PropsChanged;
  FNameOfMonths := TNameofMonths.Create;
  FNameOfMonths.OnChange := PropsChanged;
  FYearStartAt := TYearStartAt.Create(self);
  FYearStartAt.OnChange := YearStartAtChanged;
  FDayFont := TFont.Create;
  FWeekFont := TFont.Create;
  FFont := TFont.Create;
  FMinDate := TMinMaxDate.Create(self);
  FMaxDate := TMinMaxDate.Create(self);
  FDayAlignment := taRightJustify;
  FDayCaptionAlignment := taLeftJustify;
  DateCol := TSelDateItems.Create(self);
  DateCol.OnChange := DateColChanged;
{$IFDEF USEIMAGE}
  FImage := TAdvImage.Create;
  FImage.OnChange := BackgroundChanged;
  FBackgroundPosition := bpTiled;
{$ENDIF}
  FMonthSelect := True;
  FDateFormat := 'dd/mm/yyyy';
  if (csDesigning in ComponentState) then
    FWeekName := 'Wk';
  xoffset := 0;
  yoffset := 16;
  thedate := Now;
  seldate := thedate;
  FOldSelDate:= seldate;
  ChangeMonth(0);
  flgl := False;
  flgr := False;
  flgla := False;
  flgt := False;
  dflgl := False;
  dflgr := False;
  FUpdateCount := 0;

  FSelectColor := clTeal;
  FSelectFontColor := clWhite;
  FInactiveColor := clGray;
  FInversColor := clTeal;
  FInversBkColor := clNone;
  FInversBkColorTo := clNone;
  FFocusColor := clHighLight;
  FTextColor := clBlack;
  FWeekendTextColor := clRed;
  FWeekendColor := clNone;
  FHeaderColor := clNone;
  FStartDay := 7;

  FDragImage := TDragImageList.Create(Self);

  FDesignTime := (csDesigning in ComponentState) and not
                 ((csReading in Owner.ComponentState) or (csLoading in Owner.ComponentState));

  if FDesignTime then
    BorderWidth := 1;

  FBorderXP := True;
  BevelOuter := bvNone;
  DecodeDate(theDate, FYear, FMonth, FDay);
  Caption := '';
  Showhintbusy := False;
  FLastHintPos := Point(-1, -1);
  FFont.OnChange := PlanFontChanged;
  FDayFont.OnChange := PlanFontChanged;
  FWeekFont.OnChange := PlanFontChanged;
  FBrowsers := TCalendarBrowsers.Create;
  FBrowsers.OnChange := PropsChanged;
  FShowDaysBefore := True;
  FShowDaysAfter := True;
  FShowSelection := True;
  FShowSelectionFull := True;
  FShowFocusRectangle := True;
  FShowMonthPopup := True;
  FShowYearPopup := True;
  FCaptionColor := clNone;
  FCaptionColorTo := clNone;
  FTodayColor := clHighLight;
  FTodayColorTo := clNone;
  FGradientDirection := gdHorizontal;
  FLineColor := clGray;
  FLine3D := true;
  FGradientStartColor := clWhite;
  FGradientEndColor := clBtnFace;
  FGradientDirection := gdHorizontal;
  FMonthGradientStartColor := clNone;
  FMonthGradientEndColor := clNone;
  FMonthGradientDirection := gdHorizontal;
  FGlyphs := TCalGlyphs.Create(Self);
  FOldCursor := crDefault;

  FPrintOptions := TPlannerMonthViewPrintOptions.Create;

  //FTodayFormat := '"Today" DDD/mm, YYYY';

  if (csDesigning in ComponentState) then
  begin
    FHintPrevYear := 'Previous Year';
    FHintPrevMonth := 'Previous Month';
    FHintNextMonth := 'Next Month';
    FHintNextYear := 'Next Year';
  end;


  DoubleBuffered := True;

  VerInfo.dwOSVersionInfoSize := SizeOf(TOSVersionInfo);
  GetVersionEx(verinfo);

  FUseTheme := (verinfo.dwMajorVersion > 5) or
    ((verinfo.dwMajorVersion = 5) and (verinfo.dwMinorVersion >= 1));

  if FUseTheme then
    FUseTheme := FUseTheme and (IsThemeActive or (csDesigning in ComponentState));

  FPlannerMonthItems := CreateItems(FPlanner);
//  FPlannerMonthItems := TPlannerMonthViewItems.Create(FPlanner);

  FPlannerMonthItems.PlannerMonthView := self;
  FWeekNameY := 25;
  FCaptionHeight := 13;
  FDayFontHeight := 13;
  FDayNumberHeight := 0;
  FShowLines := true;
  FShowCaption := true;
  FOverlap := true;
  FDragItemImage := false;
  FDragItemAlways := false;

  FAutoChangeMonth := True;
  FColorCurrent := clYellow;
  FColorCurrentItem := clLime;
  FCaptionFont := TFont.Create;
  FCaptionFont.OnChange := CaptionFontChange;
  FCaptionHoverColor := clHighLight;
  FTrackColor := clBlue;
  FTrackOnly := False;
  FTrackWidth := 4;
  FItemSpace := 2;
  FShadowColor := clGray;
  FFlashColor := clRed;
  FFlashFontColor := clWhite;
  FURLColor := clBlue;

  FURLGlyph := TBitmap.Create;
  FURLGlyph.OnChange := URLGlyphOnChange;

  if (csDesigning in ComponentState) then
    FURLGlyph.LoadFromResourceID(Hinstance, 503);

  FDeleteGlyph := TBitmap.Create;

  if (csDesigning in ComponentState) then
    FDeleteGlyph.LoadFromResourceID(Hinstance, 504);

  FAttachementGlyph := TBitmap.Create;
  FAttachementGlyph.OnChange := AttachementGlyphOnChange;

  if (csDesigning in ComponentState) then
    FAttachementGlyph.LoadFromResourceID(Hinstance, 502);

  FImageCache := THTMLPictureCache.Create;

  {$IFDEF TMSSKINS}
  FSkin := TPlannerSkin.Create(Self);
  FSkin.OnChange := SkinChange;
  {$ENDIF}
  
  FDefaultItems := CreateItems(FPlanner);
  FDefaultItems.PlannerMonthView := self;
  FDefaultItem := FDefaultItems.Add;

  FCompletion := TCompletion.Create;
  FCompletion.OnChange := CompletionChanged;

  Width := 520;
  Height := 400;
end;

procedure TPlannerMonthView.DefineProperties(Filer: TFiler);
begin
  inherited;
  Filer.DefineProperty('TMSStyle',ReadTMSStyle, WriteTMSStyle, True);
end;

destructor TPlannerMonthView.Destroy;
var
  i: integer;
begin
  FNameOfDays.Destroy;
  FNameOfMonths.Destroy;
  FYearStartAt.Destroy;
  FFont.Free;
  FDayFont.Free;
  FWeekFont.Free;
  FMinDate.Free;
  FMaxDate.Free;
{$IFDEF USEIMAGE}
  FImage.Free;
{$ENDIF}
  DateCol.Free;
  //EventCol.Free;
  FBrowsers.Free;
  FGlyphs.Free;
  FPrintOptions.Free;

  FPlannerMonthItems.Free;
  FCaptionFont.Free;
  FURLGlyph.Free;
  FDeleteGlyph.Free;
  FAttachementGlyph.Free;
  FDefaultItem.Free;
  FDefaultItem := nil;
  FDefaultItems.Free;
  FMemo.Free;
  FMaskEdit.Free;
  BalloonDone;
  FImageCache.Free;
  FRichEdit.Free;
  FBalloon.Free;  
{$IFDEF TMSSKINS}
  FSkin.Free;
{$ENDIF}
  FCompletion.Free;
  FPlanner.Free;
  FDragImage.Free;
  FDragImage := nil;

  for i := 1 to 6 do
    FItemScrollerAry[i].Free;

  inherited Destroy;
end;

procedure TPlannerMonthView.CreateWnd;
begin
  inherited;

  FPlanner.Parent := self;
  FPlanner.Width := 0;
  FPlanner.Height := 0;
  FRichEdit.Parent := Self;
  FRichEdit.Left := 0;
  FRichEdit.Width := 0;
  FRichEdit.Visible := False;
  FRichEdit.BorderStyle := bsNone;

  if FDesignTime then
    SetComponentStyle(tsWindows7);
end;

function TPlannerMonthView.RichToText: string;
var
  MemoryStream: TMemoryStream;
begin
  Result := '';
  MemoryStream := TMemoryStream.Create;
  FRichEdit.Lines.SaveToStream(MemoryStream);
  MemoryStream.Position := 0;
  if MemoryStream.Size > 0 then
    SetString(Result, PChar(MemoryStream.Memory), MemoryStream.Size);
  MemoryStream.Free;
  Result := Result;
end;

procedure TPlannerMonthView.TextToRich(const RtfText: string);
var
  MemoryStream: TMemoryStream;
begin
  if (RtfText <> '') then
  begin
    MemoryStream := TMemoryStream.Create;
    MemoryStream.Write(RtfText[1], Length(RtfText));
    MemoryStream.Position := 0;
    FRichEdit.Lines.LoadFromStream(MemoryStream);
    MemoryStream.Free;
  end
  else
    FRichEdit.Clear;
end;

procedure TPlannerMonthView.SetFont(Value: tFont);
begin
  FFont.Assign(Value);
  Canvas.Font.Assign(FFont);
end;

procedure TPlannerMonthView.PlanFontChanged(Sender: TObject);
begin
  Canvas.Font.Assign(DayFont);
  FDayFontHeight := Canvas.TextHeight('gh');
  Canvas.Font.Assign(Font);
  DoPaint;
end;

procedure TPlannerMonthView.DoEnter;
begin
  inherited DoEnter;
  DoPaint;
end;

procedure TPlannerMonthView.DateColChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TPlannerMonthView.DoExit;
begin
  inherited DoExit;
  DoPaint;
end;

procedure TPlannerMonthView.SetLineColor(AValue: TColor);
begin
  FLineColor := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetLine3D(AValue: Boolean);
begin
  FLine3D := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.Loaded;

begin
  inherited Loaded;

  if FDay <= DaysInMonth(FMonth,FYear) then
    SelDate := EncodeDate(FYear, FMonth, FDay)
  else
    SelDate := EncodeDate(FYear, FMonth, 1);
      
  TheDate := SelDate;
  // Make sure all names are initialized to intl. settings when used.
  NameOfDays.UseIntlNames := NameOfDays.UseIntlNames;
  NameOfMonths.UseIntlNames := NameOfMonths.UseIntlNames;
  FOldCursor := Cursor;
  {$IFDEF TMSSKINS}
  FPlanner.Skin.Assign(Skin);
  {$ENDIF}
  if FBalloon.Enable then
    BalloonInit;
  
  FPlanner.PlannerImages := PlannerImages;

  if AutoThemeAdapt and not (csDesigning in ComponentState) then
    ThemeAdapt;

  FPlanner.DeleteGlyph.Assign(DeleteGlyph);
  FPlanner.AttachementGlyph.Assign(AttachementGlyph);
end;

procedure TPlannerMonthView.WndProc(var Msg: TMessage);
begin
  if (Msg.Msg = WM_THEMECHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  if (Msg.Msg = CM_SYSFONTCHANGED) and AutoThemeAdapt then
  begin
    ThemeAdapt;
  end;

  inherited;
end;

procedure TPlannerMonthView.WriteTMSStyle(Writer: TWriter);
begin
  Writer.WriteInteger(integer(FTMSStyle));
end;

procedure TPlannerMonthView.SetCompletion(const Value: TCompletion);
begin
  FPlanner.Footer.Completion.Assign(Value);
  FCompletion.Assign(Value);
end;

procedure TPlannerMonthView.SetComponentStyle(AStyle: TTMSStyle);
begin
  FTMSStyle := AStyle;
  Textcolor := clBlack;
  case AStyle of
    tsOffice2003Blue: Style := pmsOffice2003Blue;
    tsOffice2003Olive: Style := pmsOffice2003Olive;
    tsOffice2003Silver: Style := pmsOffice2003Silver;
    tsOffice2003Classic: Style := pmsOffice2003Classic;
    tsOffice2007Luna: Style := pmsOffice2007Luna;
    tsOffice2007Obsidian: Style := pmsOffice2007Obsidian;
    tsOffice2007Silver: Style := pmsOffice2007Silver;
    tsWindowsXP: Style := pmsWindowsXP;
    tsWhidbey: Style := pmsWhidbey;
    tsWindowsVista: Style := pmsWindowsVista;
    tsWindows7: Style := pmsWindows7;
    tsTerminal: Style := pmsTerminal;
    tsOffice2010Blue: Style := pmsOffice2010Blue;
    tsOffice2010Silver: Style := pmsOffice2010Silver;
    tsOffice2010Black: Style := pmsOffice2010Black;
    tsWindows8: Style := pmsWindows8;
    tsOffice2013White: Style := pmsOffice2013White;
    tsOffice2013LightGray: Style := pmsOffice2013LightGray;
    tsOffice2013Gray: Style := pmsOffice2013Gray;
    tsWindows10: Style := pmsWindows10;
    tsOffice2016White: Style := pmsOffice2016White;
    tsOffice2016Gray: Style := pmsOffice2016Gray;
    tsOffice2016Black: Style := pmsOffice2016Black;
  end;
end;

procedure TPlannerMonthView.SetStyle(const Value: TPlannerMonthViewStyle);
begin
  FStyle := Value;
  Look := LookFlat;

  case FStyle of
  pmsWindowsXP:
    begin
      GradientStartColor := clWhite;
      GradientEndColor := clBtnFace;
      MonthGradientStartColor := clNone;
      MonthGradientEndColor := clNone;
      InActiveColor := clTeal;
      Line3D := true;
      LineColor := clGray;
      Font.Style := [];
      InversBkColor := clNone;
      InversBkColorTo := clNone;
      InversColor := clTeal;
      SelectColor := clHighLight;
      FocusColor := clTeal;
      SelectFontColor := clWhite;

      TodayColor := clWhite;
      TodayColorTo := clBtnFace;

      DefaultItem.ColorTo := $F7F7F7;
      DefaultItem.Color := clWhite;
      DefaultItem.SelectColorTo := clInfoBk;
      DefaultItem.SelectColor := clWhite;
      DefaultItem.CaptionBkgTo := clSilver;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsWhidbey:
    begin
      GradientStartColor := clWhite;
      GradientEndColor := clBtnFace;
      MonthGradientStartColor := clNone;
      MonthGradientEndColor := clNone;
      InActiveColor := clTeal;
      Line3D := true;
      LineColor := clGray;
      Font.Style := [];
      InversBkColor := clNone;
      InversBkColorTo := clNone;
      InversColor := clTeal;
      SelectColor := clHighLight;
      FocusColor := clTeal;
      SelectFontColor := clWhite;

      TodayColor := clWhite;
      TodayColorTo := clBtnFace;

      DefaultItem.ColorTo := $F7F7F7;
      DefaultItem.Color := clWhite;
      DefaultItem.SelectColorTo := clInfoBk;
      DefaultItem.SelectColor := clWhite;
      DefaultItem.CaptionBkgTo := clSilver;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsOffice2000:
    begin
      Color := clBtnFace;
      GradientStartColor := clBtnFace;
      GradientEndColor := clNone;
      MonthGradientStartColor := clNone;
      MonthGradientEndColor := clNone;
      InActiveColor := clTeal;
      InversBkColor := clNone;
      InversBkColorTo := clNone;
      InversColor := clTeal;
      SelectColor := clHighLight;
      FocusColor := clTeal;

      TodayColor := clInfoBk;
      TodayColorTo := clNone;

      Line3D := false;
      LineColor := clGray;
      Font.Style := [];
      SelectFontColor := clBlack;
      DefaultItem.ColorTo := $F7F7F7;
      DefaultItem.Color := clWhite;
      DefaultItem.SelectColorTo := clInfoBk;
      DefaultItem.SelectColor := clWhite;
      DefaultItem.CaptionBkgTo := clSilver;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsOffice2003Blue:
    begin
      MonthGradientStartColor := $FCE1CB;
      MonthGradientEndColor := $E0A57D;
      InactiveColor := clWhite;
      InversBkColor := $B8F5FF;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := clHighlight;
      FocusColor := clHighlight;
      TodayColor := $D5FFFF;
      TodayColorTo := $5FCEFB;
      WeekendTextColor := clHighlight;

      Line3D := false;
      LineColor := clGray;
      Font.Style := [fsBold];

      Color := $D0FCFD;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.Color := $FCEAD9;
      DefaultItem.ColorTo := $E4A47C;
      DefaultItem.SelectColor := $81CCF9;
      DefaultItem.SelectColorTo := $5EB3FA;
      DefaultItem.CaptionBkg := $FCEAD9;
      DefaultItem.CaptionBkgTo := $E4A47C;
      DefaultItem.TrackColor := clBlue;

      DayFont.Color := clWhite;
      CaptionFont.Color := clWhite;
      CaptionColor := $00D58456;
      CaptionColorTo := $00943C07;
      ShadowColor := clGray;
    end;
  pmsOffice2003Olive:
    begin
      MonthGradientStartColor := $CFF0EA;
      MonthGradientEndColor := $8CC0B1;
      InactiveColor := clWhite;
      Line3D := false;
      LineColor := clGray;
      Font.Style := [fsBold];

      Color := $D0FCFD;
      GradientStartColor := clNone;

      InversBkColor := $B8F5FF;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := clHighlight;
      FocusColor := $CFF0EA;

      TodayColor := $D5FFFF;
      TodayColorTo := $5FCEFB;

      SelectFontColor := clWhite;

      DefaultItem.Color := $E4F1F2;
      DefaultItem.ColorTo := $AADADA;
      DefaultItem.SelectColor := $81CCF9;
      DefaultItem.SelectColorTo := $5EB3FA;
      DefaultItem.CaptionBkg := $E4F1F2;
      DefaultItem.CaptionBkgTo := $AADADA;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clWhite;
      CaptionColor := $0082C0AF;
      CaptionColorTo := $00447A63;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsOffice2003Silver:
    begin
      MonthGradientStartColor := $ECE2E1;
      MonthGradientEndColor := $B39698;
      InactiveColor := clWhite;
      Line3D := false;
      LineColor := clGray;      
      Font.Style := [fsBold];
      Color := $D0FCFD;
      GradientStartColor := clNone;

      InversBkColor := $B8F5FF;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := clHighlight;
      FocusColor := clHighlight;

      TodayColor := $D5FFFF;
      TodayColorTo := $5FCEFB;

      SelectFontColor := clWhite;

      DefaultItem.Color := $F7F3F3;
      DefaultItem.ColorTo := $E6D8D8;
      DefaultItem.SelectColor := $81CCF9;
      DefaultItem.SelectColorTo := $5EB3FA;
      DefaultItem.CaptionBkg := $F7F3F3;
      DefaultItem.CaptionBkgTo := $F7F3F3;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clWhite;
      CaptionColor := $00BDA4A5;
      CaptionColorTo := $00957475;
      DayFont.Color := clWhite;
      ShadowColor := clGray;
    end;
  pmsOffice2007Luna:
    begin
      MonthGradientStartColor := $F2E4D5;
      MonthGradientEndColor := $F2E4D5;
      InactiveColor := $F5E9E1;
      InversBkColor := $00F7EDE6;
      InversBkColorTo := clNone;
      InversColor := clBlack;
      SelectColor := $007A4C29;
      FocusColor := $007A4C29;

      TodayColor := $78D4F8;
      TodayColorTo := $2DA5F0;

      LineColor := $00F1E1D5;
      Line3D := false;
      Font.Style := [fsBold];

      CaptionFont.Color := clBlack;
      DayFont.Color := $CF9365;

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clBlack;

      DefaultItem.Color := clWhite;
      DefaultItem.ColorTo := $EAD3C1;
      DefaultItem.SelectColor := $BBEEFF;
      DefaultItem.SelectColorTo := $78DAFF;
      DefaultItem.CaptionBkg := $FFEFE3;
      DefaultItem.CaptionBkgTo := $FFD2AF;
      DefaultItem.TrackColor := $AD7C56;
      DefaultItem.BorderColor := $AD7C56;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      DayFont.Color := clBlack;
      CaptionFont.Color := clBlack;
      CaptionColor := $00FFEFE3;
      CaptionColorTo := $00FFD2AF;
      ShadowColor := $DFDFDF;

    end;
  pmsOffice2007Obsidian:
    begin
      MonthGradientStartColor := $EBEBEB;
      MonthGradientEndColor := $EBEBEB;
      InactiveColor := $F5E9E1;
      InversBkColor := $00F7EDE6;
      InversBkColorTo := clNone;
      InversColor := clBlack;
      SelectColor := $007A4C29;
      FocusColor := $007A4C29;

      TodayColor := $78D4F8;
      TodayColorTo := $2DA5F0;

      LineColor := $00F1E1D5;
      Line3D := false;
      Font.Style := [fsBold];

      CaptionFont.Color := $5C534C;
      DayFont.Color := $5C534C;

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.Color := clWhite;
      DefaultItem.ColorTo := $EAD3C1;
      DefaultItem.SelectColor := $BBEEFF;
      DefaultItem.SelectColorTo := $78DAFF;
      DefaultItem.CaptionBkg := $F2F1F0;
      DefaultItem.CaptionBkgTo := $C9C2BD;
      DefaultItem.TrackColor := $AD7C56;
      DefaultItem.BorderColor := $AD7C56;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      DayFont.Color := clBlack;
      CaptionFont.Color := clBlack;
      CaptionColor := $00F2F1F0;
      CaptionColorTo := $00C9C2BD;
      ShadowColor := $DFDFDF;
    end;
  pmsOffice2007Silver:
    begin
      MonthGradientStartColor := $F2F1F0;
      MonthGradientEndColor := $F2F1F0;
      InactiveColor := $F5E9E1;
      InversBkColor := $00ECEAE8;
      InversBkColorTo := clNone;
      InversColor := clBlack;
      SelectColor := $005C534C;
      FocusColor := $005C534C;

      TodayColor := $78D4F8;
      TodayColorTo := $2DA5F0;

      LineColor := $00A49991;
      Line3D := false;
      Font.Style := [fsBold];

      CaptionFont.Color := clBlack;
      DayFont.Color := $74706F;

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clBlack;

      DefaultItem.Color := clWhite;
      DefaultItem.ColorTo := $DBD7D4;
      DefaultItem.SelectColor := $BBEEFF;
      DefaultItem.SelectColorTo := $78DAFF;
      DefaultItem.CaptionBkg := $FAEEEB;
      DefaultItem.CaptionBkgTo := $D3C9C7;
      DefaultItem.TrackColor := $A49991;
      DefaultItem.BorderColor := clBlack;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      DayFont.Color := clBlack;
      CaptionFont.Color := clBlack;
      CaptionColor := $00FAEEEB;
      CaptionColorTo := $00D3C9C7;
      ShadowColor := $DFDFDF;
    end;
  pmsFlat:
    begin
      Color := clBtnFace;
      GradientStartColor := clBtnFace;
      GradientEndColor := clNone;
      MonthGradientStartColor := clBtnFace;
      MonthGradientEndColor := clNone;
      InactiveColor := clWhite;
      LineColor := clGray;      
      Line3D := false;
      Font.Style := [];
      InversBkColor := clNone;
      InversBkColorTo := clNone;
      InversColor := clTeal;
      SelectColor := clHighLight;
      FocusColor := clTeal;

      TodayColor := clInfoBk;
      TodayColorTo := clNone;

      SelectFontColor := clBlack;
      DefaultItem.ColorTo := $F7F7F7;
      DefaultItem.Color := clWhite;
      DefaultItem.SelectColorTo := clInfoBk;
      DefaultItem.SelectColor := clWhite;
      DefaultItem.CaptionBkgTo := clSilver;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsAvantGarde:
    begin
      GradientStartColor := $00FFD9B3;
      GradientEndColor := clNone;
      Color := $00FFD9B3;
      MonthGradientStartColor := $006580DA;
      MonthGradientEndColor := $00F807F1;
      InactiveColor := clWhite;
      LineColor := clGray;
      Line3D := false;
      Font.Style := [fsBold];
      InversBkColor := clNone;
      InversBkColorTo := clNone;
      InversColor := clTeal;
      SelectColor := clHighLight;
      FocusColor := clTeal;

      TodayColor := clWhite;
      TodayColorTo := clInfoBk;

      SelectFontColor := clWhite;
      DefaultItem.ColorTo := $00FFD9B3;
      DefaultItem.Color := $00DEF9B9;
      DefaultItem.SelectColorTo := clInfoBk;
      DefaultItem.SelectColor := clWhite;
      DefaultItem.CaptionBkgTo := $006580DA;
      DefaultItem.CaptionBkg := $00F807F1;
      DefaultItem.TrackColor := clWhite;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
  pmsOffice2003Classic:
    begin
      MonthGradientStartColor := clWhite;
      MonthGradientEndColor := $C9D1D5;
      InactiveColor := clWhite;
      InversBkColor := $B8F5FF;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := clHighlight;
      FocusColor := $FCE1CB;

      TodayColor := $B59285;
      TodayColorTo := $B59285;

      Line3D := false;
      LineColor := clBlack;
      Font.Style := [fsBold];

      Color := $D0FCFD;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.Color := $D8D5D4;
      DefaultItem.ColorTo := $D8D5D4;
      DefaultItem.SelectColor := $B59285;
      DefaultItem.SelectColorTo := $B59285;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.CaptionBkgTo := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;          
      ShadowColor := clGray;
    end;
    pmsWindowsVista:
    begin
      MonthGradientStartColor := RGB(255, 255, 255);
      MonthGradientEndColor := RGB(255, 255, 255);
      InactiveColor := $FFFFFF;
      InversBkColor := $00FDF8F1;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := $00F5D089;
      FocusColor := $00F5D089;

      TodayColor := $FEF9F0;
      TodayColorTo := $FDF0D7;

      Line3D := false;
      LineColor := $00FCF2DA;
      Font.Style := [fsBold];

      Color := RGB(255, 255, 255);
      GradientStartColor := clNone;
      SelectFontColor := clBlack;

      DefaultItem.Color := clgreen;
      DefaultItem.ColorTo := $E4A47C;
      DefaultItem.SelectColor := clred;
      DefaultItem.SelectColorTo := $5EB3FA;
      DefaultItem.CaptionBkg := $FCEAD9;
      DefaultItem.CaptionBkgTo := $E4A47C;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      CaptionColor := $00FDF8F1;
      CaptionColorTo := $00FCEFD5;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
    pmsWindows7:
    begin
      MonthGradientStartColor := RGB(255, 255, 255);
      MonthGradientEndColor := RGB(255, 255, 255);
      InactiveColor := $FFFFFF;
      InversBkColor := $00FCEBDC;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := $00F5D089;
      FocusColor := $00F5D089;

      TodayColor := $FCEBDC;
      TodayColorTo := $FCDBC1;

      CaptionColor := $00FCEBDC;
      CaptionColorTo := $00FCDBC1;
      Line3D := false;
      Look := LookFlat;
      LineColor := $00CEA27D;
      Font.Style := [fsBold];

      Color := RGB(255, 255, 255);
      GradientStartColor := clNone;
      SelectFontColor := clBlack;

      DefaultItem.ColorTo := $FFFFFF;
      DefaultItem.Color := $FFFFFF;
      DefaultItem.SelectColorTo := $FCEBDC;
      DefaultItem.SelectColor := $FCDBC1;
      DefaultItem.CaptionBkgTo := $F5D089;
      DefaultItem.CaptionBkg := $F5D089;
      DefaultItem.TrackColor := $FCDBC1;
      DefaultItem.TrackSelectColor := $00AD7C56;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
      WeekendTextColor := $00CEA27D;
    end;
    pmsTerminal:
    begin
      MonthGradientStartColor := clBtnFace;
      MonthGradientEndColor := clBtnFace;
      InactiveColor := clWhite;
      InversBkColor := clBtnFace;
      InversBkColorTo := clNone;
      InversColor := clGray;
      SelectColor := clHighLight;
      FocusColor := clHighLight;

      TodayColor := clHighLight;
      TodayColorTo := clHighLight;

      Line3D := false;
      LineColor := clGray;
      Font.Style := [fsBold];

      Color := clBtnFace;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.Color := clBtnFace;
      DefaultItem.ColorTo := clBtnFace;
      DefaultItem.SelectColor := clHighLight;
      DefaultItem.SelectColorTo := clHighLight;
      DefaultItem.CaptionBkg := clWhite;
      DefaultItem.CaptionBkgTo := clWhite;
      DefaultItem.TrackColor := clBlue;

      CaptionFont.Color := clBlack;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
    pmsOffice2010Blue:
    begin
      MonthGradientStartColor := $EDDBCD;
      MonthGradientEndColor := $EDDBCD;
      InactiveColor := $DEC1A9;
      InversBkColor := $00F7EDE6;
      InversBkColorTo := clNone;
      InversColor := $AD815E;
      SelectColor := $007A4C29;
      FocusColor := $007A4C29;

      TodayColor := $EDDBCD;
      TodayColorTo := $EDDBCD;

      Line3D := false;
      LineColor := $00F1E1D5;
      Font.Style := [fsBold];

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clBlack;

      DefaultItem.ColorTo := $E6CAB5;
      DefaultItem.Color := $F6E8DF;
      DefaultItem.SelectColorTo := $E6CAB5;
      DefaultItem.SelectColor := $F6E8DF;
      DefaultItem.CaptionBkgTo := $F0DAC7;
      DefaultItem.CaptionBkg := $FDF6EF;
      DefaultItem.TrackColor := clBlack;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;


      CaptionFont.Color := clBlack;
      CaptionColor := $00FDF6EF;
      CaptionColorTo := $00F0DAC7;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
    pmsOffice2010Silver:
    begin
      MonthGradientStartColor := $EDE9E5;
      MonthGradientEndColor := $EDE9E5;
      InactiveColor := $DEC1A9;
      InversBkColor := $00F7EDE6;
      InversBkColorTo := clNone;
      InversColor := $AD815E;
      SelectColor := $005C534C;
      FocusColor := $005C534C;

      TodayColor := $EDE9E5;
      TodayColorTo := $EDE9E5;

      Line3D := false;
      LineColor := $00ECE0D8;
      Font.Style := [fsBold];

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := $D1CBC6;
      DefaultItem.Color := $E8E8E8;
      DefaultItem.SelectColorTo := $D1CBC6;
      DefaultItem.SelectColor := $E8E8E8;
      DefaultItem.CaptionBkgTo := $EDE5E0;
      DefaultItem.CaptionBkg := $FFFFFF;
      DefaultItem.TrackColor := clBlack;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := clBlack;
      CaptionColor := clWhite;
      CaptionColorTo := $00EDE5E0;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
    pmsOffice2010Black:
    begin
      MonthGradientStartColor := $828282;
      MonthGradientEndColor := $828282;
      InactiveColor := $A3A3A3;
      InversBkColor := $00F7EDE6;
      InversBkColorTo := clNone;
      InversColor := $AD815E;
      SelectColor := $005C534C;
      FocusColor := $005C534C;

      TodayColor := $828282;
      TodayColorTo := $828282;

      Line3D := false;
      LineColor := $00ECE0D8;
      Font.Style := [fsBold];

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := $D2CCC7;
      DefaultItem.Color := $E7E7E7;
      DefaultItem.SelectColorTo := $D2CCC7;
      DefaultItem.SelectColor := $E7E7E7;
      DefaultItem.CaptionBkgTo := $919191;
      DefaultItem.CaptionBkg := $BFBFBF;
      DefaultItem.TrackColor := clBlack;
      DefaultItem.TrackSelectColor := clBlack;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := clWhite;
      CaptionColor := $00BFBFBF;
      CaptionColorTo := $00919191;
      DayFont.Color := clWhite;
      ShadowColor := clGray;
    end;

    pmsWindows8, pmsWindows10:
    begin
      MonthGradientStartColor := clWhite;
      MonthGradientEndColor := clWhite;
      InactiveColor := $EEEEEE;
      InversBkColor := $EEEEEE;
      InversBkColorTo := clNone;
      InversColor := clBlack;
      SelectColor := $DAA026;
      FocusColor := $DAA026;

      TodayColor := $DAA026;
      TodayColorTo := $DAA026;

      Line3D := false;
      LineColor := $DCDBDA;
      Font.Style := [fsBold];

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := clWhite;
      DefaultItem.Color := clWhite;
      DefaultItem.BorderColor :=  $DCDBDA;
      DefaultItem.SelectColorTo := $F7E0C9;
      DefaultItem.SelectColor := $F7E0C9;
      DefaultItem.CaptionBkgTo := $EEEEEE;
      DefaultItem.CaptionBkg := $EEEEEE;
      DefaultItem.TrackColor := $F9CEA4;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := clBlack;
      CaptionColor := $DCDBDA;
      CaptionColorTo := $DCDBDA;
      DayFont.Color := clBlack;
      ShadowColor := clGray;
    end;
    pmsOffice2016White:
    begin
      MonthGradientStartColor := clWhite;
      MonthGradientEndColor := clWhite;
      InactiveColor := $F0F0F0;
      InversBkColor := $F0F0F0;
      InversBkColorTo := clNone;
      InversColor := $666666;
      SelectColor := $C67200;
      FocusColor := $C67200;

      TodayColor := $C67200;
      TodayColorTo := $C67200;

      Line3D := false;
      LineColor := $E1E1E1;
      Font.Style := [fsBold];

      Color := clWhite;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := $FEE9C1;
      DefaultItem.Color := $FEE9C1;
      DefaultItem.BorderColor :=  $FEE9C1;
      DefaultItem.SelectColorTo := $FEE9C1;
      DefaultItem.SelectColor := $FEE9C1;
      DefaultItem.CaptionBkgTo := $FEE9C1;
      DefaultItem.CaptionBkg := $FEE9C1;
      DefaultItem.TrackColor := $F5D1A6;
      DefaultItem.TrackSelectColor := $F5D1A6;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := $262626;
      CaptionColor := $F5D1A6;
      CaptionColorTo := $F5D1A6;
      DayFont.Color := $666666;
      ShadowColor := clGray;
    end;
    pmsOffice2016Gray:
    begin
      MonthGradientStartColor := $F0F0F0;
      MonthGradientEndColor := $F0F0F0;
      InactiveColor := $D4D4D4;
      InversBkColor := $D4D4D4;
      InversBkColorTo := clNone;
      InversColor := $444444;
      SelectColor := $C67200;
      FocusColor := $C67200;

      TodayColor := $C67200;
      TodayColorTo := $C67200;

      Line3D := false;
      LineColor := $E1E1E1;
      Font.Style := [fsBold];

      Color := $F0F0F0;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := $FEE9C1;
      DefaultItem.Color := $FEE9C1;
      DefaultItem.BorderColor :=  $FEE9C1;
      DefaultItem.SelectColorTo := $FEE9C1;
      DefaultItem.SelectColor := $FEE9C1;
      DefaultItem.CaptionBkgTo := $FEE9C1;
      DefaultItem.CaptionBkg := $FEE9C1;
      DefaultItem.TrackColor := $F5D1A6;
      DefaultItem.TrackSelectColor := $F5D1A6;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := $444444;
      CaptionColor := $FEF3DD;
      CaptionColorTo := $FEF3DD;
      DayFont.Color := $444444;
      ShadowColor := clGray;
    end;
    pmsOffice2016Black:
    begin
      MonthGradientStartColor := $252525;
      MonthGradientEndColor := $252525;
      InactiveColor := $252525;
      InversBkColor := $252525;
      InversBkColorTo := clNone;
      InversColor := $F1F1F1;
      SelectColor := $FEB858;
      FocusColor := $FEB858;

      TodayColor := $C67200;
      TodayColorTo := $C67200;

      Line3D := false;
      LineColor := $626262;
      Font.Style := [fsBold];
      Textcolor := clWhite;

      Color := $252525;
      GradientStartColor := clNone;
      SelectFontColor := clWhite;

      DefaultItem.ColorTo := $D77800;
      DefaultItem.Color := $D77800;
      DefaultItem.BorderColor :=  $D77800;
      DefaultItem.SelectColorTo := $D77800;
      DefaultItem.SelectColor := $D77800;
      DefaultItem.CaptionBkgTo := $D77800;
      DefaultItem.CaptionBkg := $D77800;
      DefaultItem.TrackColor := $C35B0D;
      DefaultItem.TrackSelectColor := $C35B0D;
      DefaultItem.CaptionBkgDirection := Planner.gdHorizontal;
      DefaultItem.ColorDirection := Planner.gdVertical;

      CaptionFont.Color := $F0F0F0;
      CaptionColor := $D77800;
      CaptionColorTo := $D77800;
      DayFont.Color := $F0F0F0;
      ShadowColor := clGray;
    end;
  end;
end;

procedure TPlannerMonthView.ThemeAdapt;
var
  eTheme: XPColorScheme;
begin
  eTheme := CurrentXPTheme();

  case eTheme of
    xpBlue: Style := pmsOffice2003Blue;
    xpGreen: Style := pmsOffice2003Olive;
    xpGray: Style := pmsOffice2003Silver;
    xpNoTheme: Style := pmsWindowsXP;
  else
    Style := pmsOffice2000;
  end;
end;

procedure TPlannerMonthView.SetAutoThemeAdapt(const Value: Boolean);
begin
  FAutoThemeAdapt := Value;

  if Value and not (csDesigning in ComponentState) then
    ThemeAdapt;
end;


procedure TPlannerMonthView.PaintMonthPlannerItem(Canvas: TCanvas;
  ARect: TRect; APlannerItem: TPlannerItem; ForwardArrow,
  BackArrow, Print: Boolean);
var
  FWArrowRect, BkArrowRect: TRect;
  tw: integer;
begin
  FWArrowRect := ARect;
  BkArrowRect := ARect;

  ARect.Left := ARect.Left + 1;
  ARect.Right := ARect.Right - 1;

  Canvas.Brush.Color := self.Color;

  FPlanner.PreviewPaint(APlannerItem, Canvas, ARect, true, Print);

  SetBKMode(Canvas.Handle, TRANSPARENT);

  if ForwardArrow then
  begin
    FWArrowRect.Top := FWArrowRect.Bottom - (Canvas.TextHeight('>')) - 2 - FPlanner.TrackWidth;
    Canvas.Pen.Color := clBlack;
    tw := Canvas.TextWidth('>');
    Canvas.TextOut(FWArrowRect.Right - tw - 4, FWArrowRect.Top, '>');
    Canvas.TextOut(FWArrowRect.Right - tw - tw div 2 - 4, FWArrowRect.Top, '>');
  end;
  if BackArrow then
  begin
    BkArrowRect.Top := BkArrowRect.Bottom - (Canvas.TextHeight('>')) - 2 - FPlanner.TrackWidth;
    Canvas.Brush.Style := bsClear;

    tw := Canvas.TextWidth('>');
    Canvas.TextOut(BkArrowRect.Left + 4, BkArrowRect.Top, '<');
    Canvas.TextOut(BkArrowRect.Left + 4 + tw div 2, BkArrowRect.Top, '<');
  end;

end;


procedure TPlannerMonthView.CheckAndDrawEvent(d: TDateTime; R: TRect; RowNo, ColNo: integer; Print: Boolean);
var
  i, j: integer;
  R2, R3, R4, R5: TRect;
  MultiDay: Boolean;
  DyOfWk: integer;
  EventFirstDay, EventLastDay: Boolean;
  itemHeight: integer;
  showFwArrow, ShowBkArrow: boolean;
  S: string;
  ye, mo, da: word;
  //CrYe, CrMo, CrDa: Word;
begin
  R2 := R;
  R3 := R;
  if FDayNumberHeight = 0 then
  begin
    FDayNumberHeight := Canvas.TextHeight('gh');
  end;

  R3.Top := R3.Top + FDayNumberHeight + 2;
  R4 := R3;
  R5 := R3;

  R2.Top := R2.Top + FDayNumberHeight + 2;
  R2.Bottom := R2.Top + 15;

  DecodeDate(d, ye, mo, da);
  //DecodeDate(Now, CrYe, CrMo, CrDa);

  for i := 0 to FPlannerMonthItems.Count - 1 do
  begin
    itemHeight := R5.Bottom - R5.Top;

    if (d >= int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemStartTime)) and (d <= int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemEndTime)) then
    begin // Paint event here

      if FPlannerMonthItems.Items[i].Conflicts > 0 then
      begin
        ItemHeight := ItemHeight div min(FMaxItemsDisplayed, FPlannerMonthItems.Items[i].Conflicts);
        R3.Top := R5.Top + (FPlannerMonthItems.Items[i].ConflictPos - FItemScrollerAry[RowNo].Position) * ItemHeight;
        R4.Top := R3.Top;
        R3.Bottom := R3.top + itemHeight - ItemSpace;
        R4.Bottom := R4.Top + itemHeight - ItemSpace;
      end;

      FPlannerMonthItems.Items[i].Repainted := false;

      //if FShowCurrent and (da = CrDa) then
        // TMonthPlannerItem(FPlannerMonthItems.Items[i]).IsCurrent := true;

      if (TMonthPlannerItem(FPlannerMonthItems.Items[i]).Visible) and
         FPlannerMonthItems.InVisibleLayer(FPlannerMonthItems.Items[i].Layer) and
         (TMonthPlannerItem(FPlannerMonthItems.Items[i]).ConflictPos >= FItemScrollerAry[RowNo].Position) and
         (TMonthPlannerItem(FPlannerMonthItems.Items[i]).ConflictPos + 1 <= FItemScrollerAry[RowNo].Position + FMaxItemsDisplayed)
        {or (TMonthPlannerItem(FPlannerMonthItems.Items[i]).Conflicts <= FMaxItemsDisplayed)} then
      begin
        MultiDay := ((TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemEndTime - TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemStartTime) >= 1)
          or (int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemEndTime) <> int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemStartTime));

        if MultiDay then
        begin
          EventFirstDay := int(d) = int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemStartTime);
          EventLastDay := int(d) = int(TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemEndTime);
          DyOfWk := ColNo; //DayOfWeek(D);
          if (DyOfWk = 1) or EventFirstDay then
          begin
            if EventLastDay or (DyOfWk = 7) or (not ShowDaysAfter and (da = DaysInMonth(mo, ye))) then
            begin // Start and End
              showFwArrow := (DyOfWk = 7) and not EventLastDay or (not ShowDaysAfter and not EventLastDay and (da = DaysInMonth(mo, ye)));
              ShowBkArrow := not EventFirstDay;

              S := inttostr(RowNo) + '=' + inttostr(r3.Right); //0
              j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
              if j >= 0 then
                TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Delete(j);
              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R3.Left));

              PaintMonthPlannerItem(FCanvas, R3, FPlannerMonthItems.Items[i], showFwArrow, ShowBkArrow, Print);
            end
            else // Start only
            begin
              if EventFirstDay then
                S := inttostr(RowNo) + '=1'
              else
                S := inttostr(RowNo) + '=0';

              j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
              if j >= 0 then
                TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Delete(j);

              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R3.Left));
            end;
          end
          else // End Only
          begin
            if (DyOfWk = 7) or EventLastDay then
            begin
              showFwArrow := (DyOfWk = 7) and not EventLastDay;

              j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
              if j < 0 then
              begin
                S := inttostr(RowNo) + '=0';
                j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R4.Left));
              end;
              if TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Values[inttostr(RowNo)] = '1' then
                ShowBkArrow := false
              else
                ShowBkArrow := true;
              R4.Left := integer(TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Objects[j]);

              S := inttostr(RowNo) + '=' + inttostr(r4.Right); //0
              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Delete(j);
              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R4.Left));

              PaintMonthPlannerItem(FCanvas, R4, FPlannerMonthItems.Items[i], showFwArrow, ShowBkArrow, Print);
            end
            else if not ShowDaysAfter and (da = DaysInMonth(mo, ye)) then
            begin
              showFwArrow := not EventLastDay;

              j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
              if TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Values[inttostr(RowNo)] = '1' then
                ShowBkArrow := false
              else
                ShowBkArrow := true;
                
              R4.Left := integer(TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Objects[j]);

              S := inttostr(RowNo) + '=' + inttostr(r4.Right); //0
              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Delete(j);
              TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R4.Left));

              PaintMonthPlannerItem(Canvas, R4, FPlannerMonthItems.Items[i], showFwArrow, ShowBkArrow, Print);
            end
            else // just continue
            begin
              S := inttostr(RowNo) + '=0';

              j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
              if j < 0 then
                TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R3.Left));
            end;
          end;
        end
        else // Not MultiDay
        begin
          j := TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.IndexOfName(inttostr(RowNo));
          if j >= 0 then
            TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Delete(j);

          S := inttostr(RowNo) + '=' + inttostr(r3.Right); //0
          TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.AddObject(S, pointer(R3.Left));

          PaintMonthPlannerItem(FCanvas, R3, FPlannerMonthItems.Items[i], false, false, Print);
        end;
      end;
    end;
  end;
end;

procedure TPlannerMonthView.SetLook(avalue: TPlannerMonthViewLook);
begin
  FLook := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowToday(AValue: Boolean);
begin
  FShowToday := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetTodayColor(const Value: TColor);
begin
  FTodayColor := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetTodayColorTo(const Value: TColor);
begin
  FTodayColorTo := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetTodayStyle(const Value: TTodayStyle);
begin
  FTodayStyle := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetDayFont(AValue: TFont);
begin
  if Assigned(AValue) then
    FDayFont.Assign(AValue);
  Invalidate;
end;

procedure TPlannerMonthView.SetGlyphs(const Value: TCalGlyphs);
begin
  FGlyphs.Assign(Value);
end;

procedure TPlannerMonthView.SetWeekFont(AValue: TFont);
begin
  if Assigned(AValue) then
    FWeekFont.Assign(AValue);
  Invalidate;
end;

procedure TPlannerMonthView.SetWeekName(const Value: string);
begin
  FWeekName := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetTextColor(aColor: TColor);
begin
  FTextColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetInversColor(AColor: TColor);
begin
  FInversColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetInversBkColor(AColor: TColor);
begin
  FInversBkColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetInversBkColorTo(AColor: TColor);
begin
  FInversBkColorTo := AColor;
  Invalidate;
end;


procedure TPlannerMonthView.SetFocusColor(AColor: TColor);
begin
  FFocusColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetWeekendTextColor(AColor: TColor);
begin
  FWeekendTextColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetWeekendColor(AColor: TColor);
begin
  FWeekendColor := AColor;
  Invalidate;
end;


procedure TPlannerMonthView.SetSelectColor(AColor: TColor);
begin
  FSelectColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetSelectFontColor(AColor: TColor);
begin
  FSelectFontColor := AColor;
  Invalidate;
end;


procedure TPlannerMonthView.SetInActiveColor(AColor: TColor);
begin
  FInactiveColor := AColor;
  Invalidate;
end;

procedure TPlannerMonthView.SetHeaderColor(AColor: TColor);
begin
  FHeaderColor := Acolor;
  Invalidate;
end;

procedure TPlannerMonthView.SetLabel(mo, ye: word);
begin
  Labels := FNameofMonths.GetMonth(mo) + ' ' + IntToStr(ye);
end;

procedure TPlannerMonthView.SetLayer(const Value: integer);
begin
  if (FLayer <> Value) then
  begin
    FLayer := Value;
    FPlanner.Layer := Value;
    FPlannerMonthItems.SetConflicts;
    Invalidate;
  end;
end;

function TPlannerMonthView.DaysInMonth(mo, ye: word): word;
begin
  if mo <> 2 then
    DaysInMonth := ADaysinmonth[mo]
  else
  begin
    if (ye mod 4 = 0) then DaysInMonth := 29
    else
      DaysInMonth := 28;
    if (ye mod 100 = 0) then DaysInMonth := 28;
    if (ye mod 400 = 0) then DaysInmonth := 29;
  end;
end;

procedure TPlannerMonthView.SetStartDay(AValue: integer);
begin
  if AValue < 1 then
    AValue := 1;
  if AValue > 7 then
    AValue := 7;
  FStartDay := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowCaption(aValue: Boolean);
begin
  FShowCaption := AValue;
  CaptionHeight := CaptionHeight;
end;

procedure TPlannerMonthView.SetShowWeeks(aValue: Boolean);
begin
  if AValue then
    XOffset := FWeekWidth
  else
    if not (csLoading in ComponentState) then
      XOffset := 0;

  FShowWeeks := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetCalDay(AValue: word);
begin
  try
    SetDate(AValue, FMonth, FYear);
    FDay := AValue;
  except
    MessageDlg('Invalid date', mtError, [mbOK], 0);
  end;
  Invalidate;
end;

procedure TPlannerMonthView.SetCalMonth(AValue: word);
begin
  try
    SetDate(FDay, AValue, FYear);
    FMonth := AValue;
  except
    MessageDlg('Invalid date', mtError, [mbOK], 0);
  end;
  Items.SetConflicts;
  SetItemScrollerPosition;
  Invalidate;
end;

procedure TPlannerMonthView.SetCalYear(AValue: word);
begin
  try
    SetDate(FDay, FMonth, AValue);
    FYear := AValue;
  except
    MessageDlg('Invalid date', mtError, [mbOK], 0);
  end;
  Items.SetConflicts;
  SetItemScrollerPosition;
  Invalidate;
end;

function TPlannerMonthView.GetCalDay: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  Result := da;
end;

function TPlannerMonthView.GetCalMonth: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  Result := mo;
end;

function TPlannerMonthView.GetMonth(var dt: TDateTime): word;
var
  da, mo, ye: word;
begin
  DecodeDate(dt, ye, mo, da);
  Result := mo;
end;


function TPlannerMonthView.GetRowScrollPositoin(index: integer): integer;
begin
  Result := GetItemScroller(index).Position;
end;

function TPlannerMonthView.GetCalYear: word;
var
  da, mo, ye: word;
begin
  GetDate(da, mo, ye);
  Result := ye;
end;

function TPlannerMonthView.GetYear(dt: tdatetime): integer;
var
  da, mo, ye: word;
begin
  DecodeDate(dt, ye, mo, da);
  Result := ye;
end;

procedure TPlannerMonthView.SetDayAlignment(const AAlignment: TAlignment);
begin
  FDayAlignment := AAlignment;
  Invalidate;
end;

procedure TPlannerMonthView.SetDayCaptionAlignment(const AAlignment: TAlignment);
begin
  FDayCaptionAlignment := AAlignment;
  Invalidate;
end;

procedure TPlannerMonthView.SetNameofDays(ANameofDays: TNameofDays);
begin
  FNameofDays := ANameofDays;
  Invalidate;
end;

procedure TPlannerMonthView.SetNameofMonths(ANameofMonths: TNameofMonths);
begin
  FNameofMonths := ANameofMonths;
  Invalidate;
end;

procedure TPlannerMonthView.SetRowScrollPosition(index: integer;
  const Value: integer);
begin
  GetItemScroller(index).Position := Value;
  SetItemScrollerPosition;
  Invalidate;
end;

function TPlannerMonthView.CanResize(var NewWidth, NewHeight: Integer): Boolean;
begin
  Result := inherited CanResize(NewWidth, NewHeight);

  if FIsEditing then
  begin
    if FMemo.Visible then
    begin
      FMemo.StopEdit;
    end;
    if FMaskEdit.Visible then
    begin
      FMaskEdit.StopEdit;
    end;
  end;
end;

procedure TPlannerMonthView.CaptionFontChange(Sender: TObject);
begin
  Invalidate;
end;

procedure TPlannerMonthView.ChangeMonth(dx: integer);
var
  ye, mo, da: word;
  dt: TDateTime;
begin
  DecodeDate(thedate, ye, mo, da);

  mo := mo + dx;

  while mo > 12 do
  begin
    Inc(ye);
    mo := mo - 12;
  end;

  if mo = 0 then
  begin
    Dec(ye);
    mo := 12;
  end;

  if da > DaysInMonth(mo, ye) then
    da := DaysInMonth(mo, ye);

  dt := EncodeDate(ye, mo, da);

  if CheckDateRange(dt) then
  begin
    thedate := dt;
    seldate := thedate;
    SetLabel(mo, ye);

    FMonth := mo;
    FYear := ye;

    if Assigned(FPlannerMonthItems) and (FPlannerMonthItems.Count > 0) then
      FPlannerMonthItems.SetConflicts;

    Invalidate;
  end
  else
  begin
    if (MinDate.Use) and (dt < MinDate.Date)  then
      dt := MinDate.Date;

    if (MaxDate.Use) and (dt > MaxDate.Date)  then
      dt := MaxDate.Date;


    thedate := dt;

    DecodeDate(dt, ye, mo, da);

    seldate := thedate;
    SetLabel(mo, ye);

    FMonth := mo;
    FYear := ye;

    if Assigned(FPlannerMonthItems) and (FPlannerMonthItems.Count > 0) then
      FPlannerMonthItems.SetConflicts;

    //SetItemScrollerPosition;

    Invalidate;
  end;
end;

function TPlannerMonthView.CheckDateRange(dt: TDatetime): Boolean;
begin
  Result :=
    (not FMinDate.Use or (EncodeDate(FMinDate.Year, FMinDate.Month, FMinDate.Day) <= dt))
    and
    (not FMaxDate.Use or (EncodeDate(FMaxDate.Year, FMaxDate.Month, FMaxDate.Day) >= dt));
end;

function TPlannerMonthView.CheckMonth(dt: TDatetime): Boolean;
begin
  Result :=
    (not FMinDate.Use or (EncodeDate(FMinDate.Year, FMinDate.Month, 1) <= dt))
    and
    (not FMaxDate.Use or (EncodeDate(FMaxDate.Year, FMaxDate.Month, DaysInMonth(FMaxDate.Month, FMaxDate.Year)) >= dt));
end;

procedure TPlannerMonthView.DiffCheck(dt1, dt2: tdatetime);
var
  da1, da2, mo1, mo2, ye1, ye2: word;
begin
  DecodeDate(dt1, ye1, mo1, da1);
  DecodeDate(dt2, ye2, mo2, da2);

  if da1 <> da2 then
  begin
    if Assigned(FOnDayChange) then
      FOnDayChange(self, dt1, dt2);
  end;

  if mo1 <> mo2 then
  begin
    DoChangeMonth(dt1, dt2);
    DoMonthChanged(dt1, dt2);
  end;

  if ye1 <> ye2 then
  begin
    DoChangeYear(dt1, dt2);
    DoYearChanged(dt1, dt2);
  end;
end;

function TPlannerMonthView.DiffMonth(dx: integer): tdatetime;
var
  ye, mo, da: word;
  nmo: smallint;
begin
  DecodeDate(thedate, ye, mo, da);
  nmo := mo + dx;
  if nmo > 12 then
  begin
    nmo := nmo - 12;
    Inc(ye);
  end;
  if nmo < 1 then
  begin
    nmo := nmo + 12;
    Dec(ye);
  end;

  if dx < 0 then
    da := DaysInMonth(nmo, ye)
  else
    da := 1;

  if da > DaysInMonth(nmo, ye) then
    da := DaysInMonth(nmo, ye);

  Result := EncodeDate(ye, nmo, da);
end;

function TPlannerMonthView.DiffYear(dx: integer): TDateTime;
var
  ye, mo, da: word;
begin
  DecodeDate(thedate, ye, mo, da);
  ye := ye + dx;

  if da > DaysInMonth(mo, ye) then
    da := DaysInMonth(mo, ye);

  Result := EncodeDate(ye, mo, da);

  UpdateYearStartAtISO;
end;

procedure TPlannerMonthView.ChangeYear(dx: integer);
var
  ye, mo, da: word;
  dt: TDatetime;
begin
  DecodeDate(thedate, ye, mo, da);
  ye := ye + dx;

  if da > DaysInMonth(mo, ye) then
    da := DaysInMonth(mo, ye);

  dt := EncodeDate(ye, mo, da);

  if CheckDateRange(dt) then
  begin
    thedate := dt;
    seldate := thedate;
    SetLabel(mo, ye);

    FMonth := mo;
    FYear := ye;

    if Assigned(FPlannerMonthItems) and (FPlannerMonthItems.Count > 0) then
      FPlannerMonthItems.SetConflicts;

    DoPaint;
  end;
end;

procedure TPlannerMonthView.PaintArrowLeft(PaintRect: TRect);
var
  xoffs: integer;
begin
  if Browsers.PrevYear then
    xoffs := XOffset + 20
  else
    xoffs := XOffset;

  with FCanvas do
  begin
    if not FGlyphs.FPrevMonth.Empty then
    begin
      FGlyphs.PrevMonth.TransparentMode := tmAuto;
      FGlyphs.PrevMonth.Transparent := true;

      if FGlyphs.FPrevYear.Empty then
        FCanvas.Draw(PaintRect.Left + xoffs + 5, PaintRect.Top + 1 + BorderWidth, FGlyphs.FPrevMonth)
      else
        FCanvas.Draw(PaintRect.Left + XOffset + 10 + FGlyphs.FPrevYear.Width, PaintRect.Top + 1 + BorderWidth, FGlyphs.FPrevMonth);
    end
    else
    begin
      if flgl then
      begin
        Brush.Color := FCaptionHoverColor;
        Pen.Color := FCaptionHoverColor;
      end
      else
      begin
        Brush.Color := FCaptionFont.Color;
        Pen.Color := FCaptionFont.Color;
      end;

      if not CheckDateRange(Diffmonth(-1)) then
      begin
        Brush.Color := FInactiveColor;
        Pen.Color := FInactiveColor;
      end;

      if FGlyphs.FPrevYear.Empty then
        Polygon([Point(PaintRect.Left + xoffs + 10, PaintRect.Top + 1 + BorderWidth),
                 Point(PaintRect.Left + xoffs + 5, PaintRect.Top + 6 + BorderWidth),
                 Point(PaintRect.Left + xoffs + 10, PaintRect.Top + 11 + BorderWidth)])
      else
        Polygon([Point(PaintRect.Left + xoffs + 5 + FGlyphs.FPrevYear.Width, PaintRect.Top + 1 + BorderWidth),
                 Point(PaintRect.Left + xoffs + FGlyphs.FPrevYear.Width, PaintRect.Top + 6 + BorderWidth),
                 Point(PaintRect.Left + xoffs + 5 + FGlyphs.FPrevYear.Width, PaintRect.Top + 11 + BorderWidth)]);

      Brush.Color := Color;
    end;
  end;
end;

procedure TPlannerMonthView.PaintArrowRight(PaintRect: TRect);
var
  xoffs: Integer;
begin
  if Browsers.NextYear then
    xoffs := 25
  else
    xoffs := 5;

  with FCanvas do
  begin
    if not FGlyphs.FNextMonth.Empty then
    begin
      FGlyphs.NextMonth.TransparentMode := tmAuto;
      FGlyphs.NextMonth.Transparent := true;

      if FGlyphs.FNextYear.Empty or not Browsers.NextYear then
        FCanvas.Draw(PaintRect.Right - (xoffs + FGlyphs.FNextMonth.Width), PaintRect.Top + 1 + BorderWidth, FGlyphs.NextMonth)
      else
        FCanvas.Draw(PaintRect.Right - (FGlyphs.FNextMonth.Width + 10 + FGlyphs.FNextYear.Width), PaintRect.Top + 1 + BorderWidth, FGlyphs.NextMonth);
    end
    else
    begin
      if flgr then
      begin
        Brush.Color := FCaptionHoverColor;
        Pen.Color := FCaptionHoverColor;
      end
      else
      begin
        Brush.Color := FCaptionFont.Color;
        Pen.Color := FCaptionFont.Color;
      end;

      if not CheckDateRange(diffmonth(+1)) then
      begin
        Brush.Color := FInactiveColor;
        Pen.Color := FInactiveColor;
      end;

      if FGlyphs.FNextYear.Empty or not Browsers.NextYear then
        Polygon([Point(PaintRect.Right - 5 - xoffs, PaintRect.Top + 1 + BorderWidth),
                 Point(PaintRect.Right - 5 - xoffs, PaintRect.Top + 11 + BorderWidth),
                 Point(PaintRect.Right - xoffs, PaintRect.Top + 6 + BorderWidth)])
      else
        Polygon([Point(PaintRect.Right - (15 + FGlyphs.FNextYear.Width), PaintRect.Top + 1 + BorderWidth),
                 Point(PaintRect.Right - (15 + FGlyphs.FNextYear.Width), PaintRect.Top + 11 + BorderWidth),
                 Point(PaintRect.Right - (10 + FGlyphs.FNextYear.Width), PaintRect.Top + 6 + BorderWidth)]);

      Brush.Color := Color;
    end;
  end;
end;

procedure TPlannerMonthView.PaintDblArrowLeft(PaintRect: TRect);
begin
  with FCanvas do
  begin
    if not FGlyphs.FPrevYear.Empty then
    begin
      FGlyphs.PrevYear.TransparentMode := tmAuto;
      FGlyphs.PrevYear.Transparent := true;
      FCanvas.Draw(PaintRect.Left + xoffset + 5, PaintRect.Top + 1 + BorderWidth, FGlyphs.FPrevYear);
    end
    else
    begin
      if dflgl then
      begin
        Brush.Color := FCaptionHoverColor;
        Pen.Color := FCaptionHoverColor;
      end
      else
      begin
        Brush.Color := FCaptionFont.Color;
        Pen.Color := FCaptionFont.Color;
      end;

      if not checkdaterange(diffyear(-1)) then
      begin
        Brush.Color := FInactiveColor;
        Pen.Color := FInactiveColor;
      end;

      Polygon([Point(PaintRect.Left + xoffset + 10, PaintRect.Top + 1 + BorderWidth),
               Point(PaintRect.Left + xoffset + 5, PaintRect.Top + 6 + BorderWidth),
               Point(PaintRect.Left + xoffset + 10, PaintRect.Top + 11 + BorderWidth)]);
      Polygon([Point(PaintRect.Left + xoffset + 15, PaintRect.Top + 1 + BorderWidth),
               Point(PaintRect.Left + xoffset + 10, PaintRect.Top + 6 + BorderWidth),
               Point(PaintRect.Left + xoffset + 15, PaintRect.Top + 11 + BorderWidth)]);

      Brush.Color := Color;
    end;
  end;
end;

procedure TPlannerMonthView.PaintDblArrowRight(PaintRect: TRect);
begin
  with FCanvas do
  begin
    if not FGlyphs.FNextYear.Empty then
    begin
      FGlyphs.NextYear.TransparentMode := tmAuto;
      FGlyphs.NextYear.Transparent := true;
      FCanvas.Draw(PaintRect.Right - (FGlyphs.FNextYear.Width + 5), PaintRect.Top + 1 + BorderWidth, FGlyphs.FNextYear);
    end
    else
    begin
      if dflgr then
      begin
        Brush.Color := FCaptionHoverColor;
        Pen.Color := FCaptionHoverColor;
      end
      else
      begin
        Brush.Color := FCaptionFont.Color;
        Pen.Color := FCaptionFont.Color;
      end;

      if not Checkdaterange(diffyear(+1)) then
      begin
        Brush.Color := FInactiveColor;
        Pen.Color := FInactiveColor;
      end;

      Polygon([Point(PaintRect.Right - 10, PaintRect.Top + 1 + BorderWidth),
               Point(PaintRect.Right - 10, PaintRect.Top + 11 + BorderWidth),
               Point(PaintRect.Right - 5, PaintRect.Top + 6 + BorderWidth)]);
      Polygon([Point(PaintRect.Right - 15, PaintRect.Top + 1 + BorderWidth),
               Point(PaintRect.Right - 15, PaintRect.Top + 11 + BorderWidth),
               Point(PaintRect.Right - 10, PaintRect.Top + 6 + BorderWidth)]);

      Brush.Color := Color;
    end;
  end;
end;

procedure TPlannerMonthView.PaintLabel(PaintRect: TRect);
var
  l, yw: longint;
begin
  with FCanvas do
  begin
    FCanvas.Font.Assign(FCaptionFont);
    l := TextWidth(labels);
    yw := TextWidth(' 9999');

    if flgla then
      Font.Color := FCaptionHoverColor
    else
      Font.Color := FCaptionFont.Color;

    SetBKMode(FCanvas.Handle, TRANSPARENT);

    TextOut(PaintRect.Left + xoffset + ((PaintRect.Right - PaintRect.Left - loword(l) - xoffset) shr 1), PaintRect.Top + 2, labels);
    Font.Color := FTextColor;
    lblx1 := (PaintRect.Right - PaintRect.Left - loword(l) - xoffset) shr 1;
    lblx2 := lblx1 + loword(l) - yw;
    lblx3 := lblx1 + loword(l);
    FCanvas.Font.Assign(FFont);
  end;
end;

function TPlannerMonthView.LastDate: TDateTime;
begin
  Result := FirstDate + 41;
end;

function TPlannerMonthView.FirstDate: TDateTime;
var
  fd: Integer;
  da, mo, ye: word;
begin
  DecodeDate(TheDate, ye, mo, da);

  fd := DayOfWeek(EncodeDate(ye, mo, 1)) - 1 - StartDay;

  if fd < 0 then
    fd := fd + 7;
  Result := EncodeDate(ye, mo, 1) - (fd);
end;

procedure TPlannerMonthView.PaintProc(PaintRect: TRect; Print: Boolean);
var
  i, j, th, dnh: word;
  da, mo, ye, pmo, pye, nmo, nye, sda, cda, cmo, cye, dye, sye, snye, spye, wfh: word;
  fd, fmd: integer;
  d, pyd, pnd, pcd: TDateTime;
  dstr: string;
  //isEvent: Boolean;
  r, r2: TRect;
  oldStyle: TFontStyles;
  inlist: Boolean;
  CaptionS: string;
  CaptionBrush, TempBrush: TBrush;
  CaptionRect, TR: TRect;
  bkColor, bkColorTo: TColor;
  ShouldNotDrawEvent: Boolean;
  clkYe, clkMo, clkDa: word;
  CurDate: TDateTime;
  ScrollColW: integer;
  FocusCell: Boolean;
  FPWeekWidth: Integer;
  uDayFormat: cardinal;
  uDayCaptionFormat: cardinal;
  captionr: TRect;

  function SmallCaps(s: string): string;
  var
    buf: array[0..10] of char;
  begin
    strpcopy(buf, s);
    strlower(buf);
    s := strpas(buf);
    s[1] := upcase(s[1]);
    SmallCaps := s;
  end;

begin
  if not Assigned(FNameofDays) then
    Exit;

  if not Assigned(FNameofMonths) then
    Exit;

  ScrollColW := 0;
  if ShowScrollColumn then
    ScrollColW := ScrollColumnSize;

  CaptionBrush := TBrush.Create;
  TempBrush := TBrush.Create;
  CaptionBrush.Style := bsClear;
  CaptionS := '';

  dstr := 'gh';
  th := 0;
  FCanvas.Font.Assign(Font);
  dnh := FCanvas.TextHeight(dstr);
  FCanvas.Font.Assign(DayFont);
  wfh := FCanvas.TextHeight(dstr);

  uDayFormat:= DT_TOP or DT_SINGLELINE;

  case FDayAlignment of
  taLeftJustify: uDayFormat := uDayFormat or DT_LEFT;
  taCenter: uDayFormat := uDayFormat or DT_CENTER;
  else
    uDayFormat := uDayFormat or DT_RIGHT;
  end;

  uDayCaptionFormat:= DT_TOP or DT_SINGLELINE;

  case FDayCaptionAlignment of
  taLeftJustify: uDayCaptionFormat := uDayCaptionFormat or DT_LEFT;
  taCenter: uDayCaptionFormat := uDayCaptionFormat or DT_CENTER;
  else
   uDayCaptionFormat := uDayCaptionFormat or DT_RIGHT;
  end;

  if Print then
    FPWeekWidth := round(FWeekWidth * FHTMLFactor)
  else
    FPWeekWidth := FWeekWidth;

  DecodeDate(SelDate, ye, mo, sda);
  DecodeDate(TheDate, ye, mo, da);
  DecodeDate(Now, cye, cmo, cda);
  DecodeDate(clkDate, clkYe, clkMo, clkDa);

  CurDate := int(now);

  FCanvas.Font.Assign(FFont);

  if FShowWeeks then
    dx := ((PaintRect.Right - PaintRect.Left) - ScrollColW - NumCols - FPWeekWidth) div NumCols
  else
    dx := ((PaintRect.Right - PaintRect.Left) - ScrollColW - NumCols) div NumCols;

  if FShowWeeks then
    XOffset := FPWeekWidth
  else
    XOffset := BorderWidth - 1;

  dy := ((PaintRect.Bottom - PaintRect.Top) + 8) div NumRows;

  if (CaptionColor <> clNone) and FShowCaption then
  begin
    captionR.Top := PaintRect.Top + BorderWidth;
    captionR.Left := PaintRect.Left + BorderWidth;
    captionR.Right := PaintRect.Right - BorderWidth;
    FCanvas.Font.Assign(Font);

    if Print then
      captionR.Bottom := PaintRect.Top + Round(FHTMLFactor * CaptionHeight)
    else
      captionR.Bottom := PaintRect.Top + CaptionHeight;

    if (HeaderColor = clNone) then
      captionR.Bottom := captionR.Bottom + Fweeknamey;

    if CaptionColor <> clNone then
      DrawGradient(FCanvas, CaptionColor, CaptionColorTo, 80, CaptionR, FCaptionGradientDirection <> gdVertical);

    if (Look = Look3D) then
    begin
      FCanvas.Brush.Style := bsClear;
      FCanvas.Pen.Color := clGray;
      FCanvas.Rectangle(captionR.Left, captionR.Top, captionR.Right, captionR.Bottom);
      FCanvas.Pen.Color := clWhite;
      FCanvas.MoveTo(captionR.Left, captionR.Bottom);
      FCanvas.LineTo(captionR.Left, captionR.Top);
      FCanvas.LineTo(captionR.Right, captionR.Top);
    end;
  end;


  if (not Print) and (FShowCaption) then
  begin
    if FBrowsers.FPrevMonth then PaintArrowLeft(PaintRect);
    if FBrowsers.FNextMonth then PaintArrowRight(PaintRect);
    if FBrowsers.FPrevYear then PaintDblArrowLeft(PaintRect);
    if FBrowsers.FNextYear then PaintDblArrowRight(PaintRect);
  end;

  if FShowCaption then
    PaintLabel(PaintRect);

  d := EncodeDate(ye, mo, 1);

  //first day of the month
  fd := DayOfWeek(d) - 1 - StartDay;

  if fd < 0 then
    fd := fd + 7;

  //determine previous month
  if mo = 1 then
  begin
    pmo := 12;
    pye := ye - 1;
  end
  else
  begin
    pmo := mo - 1;
    pye := ye;
  end;

  //determine next month
  if mo = 12 then
  begin
    nmo := 1;
    nye := ye + 1;
  end
  else
  begin
    nmo := mo + 1;
    nye := ye;
  end;

  FCanvas.Font.Assign(FDayFont);

  with FCanvas do
  begin
    SetBKMode(Handle, TRANSPARENT);

    r := Rect(0, 0, 100, 100);

    //draw day names
    r.left := PaintRect.Left + XOffset + 2;
    r.right := r.left + dx * 7 + 2;
    r.top := PaintRect.Top + YOffset;
    r.bottom := r.top + dy - 8;

    dy := FWeekNameY;

    if (FLook = Look3D) then
    begin
      r := Rect(PaintRect.Left + BorderWidth + 1, PaintRect.Top + FWeekNameY, PaintRect.Right - borderwidth - 1, PaintRect.Top + FWeekNameY + wfh);
      if HeaderColor <> clNone then
      begin
        FCanvas.Brush.Color := HeaderColor;
        FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
      end;
      Frame3D(FCanvas, r, clWhite, clGray, 1);
    end;

    for i := 1 to 7 do
    begin
      r.left :=  PaintRect.Left + (i - 1) * dx + XOffset;
      r.right := r.left + dx;
      r.top := PaintRect.Top  + FWeekNameY;
      r.bottom := PaintRect.Top + FWeekNameY + wfh;
      dstr := FNameofDays.GetDay(i + startday - 1);
      DrawText(FCanvas.Handle, PChar(dstr), length(dstr), r, DT_CENTER or DT_TOP or DT_SINGLELINE);

    end;

    FCanvas.Brush.Color := self.Color;

    if Print then
      PaintRect.Top := PaintRect.Top + FWeekNameY;

    dy := (PaintRect.Bottom - PaintRect.Top - 8 - dy - wfh) div (NumRows - 2);

    if FShowWeeks then
    begin
      dy := PaintRect.Top + FWeekNameY;

      r.Top := PaintRect.Top + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;

      Pen.Color := FLineColor;
      Pen.Width := 1;

      MoveTo(PaintRect.Left + FPWeekWidth,  r.Top);
      LineTo(PaintRect.Left + FPWeekWidth, PaintRect.Bottom - 2);

      if Line3D then
      begin
        Pen.Color := clwhite;
        MoveTo(PaintRect.Left + FPWeekWidth + 1, r.Top);
        LineTo(PaintRect.Left + FPWeekWidth + 1, PaintRect.Bottom - 2);
      end;

      FCanvas.Font.Assign(FDayfont);

      r.Left := PaintRect.Left + 2;
      r.Right := r.Left + FPWeekWidth;

      if Print then
        r.Top := PaintRect.Top
      else
        r.Top := dy;

      r.Bottom := dy + wfh;

      SetBKMode(FCanvas.Handle, TRANSPARENT);
      DrawText(FCanvas.Handle, PChar(FWeekName), Length(FWeekName), r,
        DT_CENTER or DT_TOP or DT_SINGLELINE);


      dy := (PaintRect.Bottom - PaintRect.Top - 8 - dy - wfh) div (NumRows - 2); //

      FCanvas.Brush.Color := Color;
      FCanvas.Font.Assign(FWeekFont);

      SetBKMode(FCanvas.Handle, TRANSPARENT);

      fmd := DayOfWeek(EncodeDate(ye, mo, 1)) - StartDay;
      if fmd <= 0 then
        fmd := fmd + 7;

      //draw week numbers
      for i := 1 to 6 do
      begin
        sye := ye;
        if YearStartAt.StartMonth = 12 then Dec(sye);

        snye := ye + 1;
        if YearStartAt.NextYearStartMonth = 12 then
          Dec(snye);

        spye := ye - 1;
        if YearStartAt.PrevYearStartMonth = 12 then
          Dec(spye);

        d := Encodedate(ye, mo, 7 - fmd + 1);

        pcd := d + (i - 1) * 7;

        // compensate for week numbers of next / previous year

        if YearStartAt.StartMonth = 12 then
          pyd := EncodeDate(ye - 1, YearStartAt.StartMonth, YearStartAt.StartDay)
        else
          pyd := EncodeDate(ye, YearStartAt.StartMonth, YearStartAt.StartDay);

        if YearStartAt.NextYearStartMonth = 12 then
          pnd := EncodeDate(ye, YearStartAt.NextYearStartMonth,
            YearStartAt.NextYearStartDay)
        else
          pnd := EncodeDate(ye + 1, YearStartAt.NextYearStartMonth,
            YearStartAt.NextYearStartDay);

        // year of rightmost day in calendar
        dye := GetYear(pcd);

        if ((dye = ye) and (pcd >= pyd)) or ((pcd < pnd) and (mo = 12)) then
        begin
          dstr := IntToStr(1 + (round(d + (i - 1) * 7 - encodedate(sye,
            yearstartAt.StartMonth, yearStartAt.StartDay)) div 7));
        end
        else
        begin
          if dye > ye then
            dstr := IntToStr(1 + (round(d + (i - 1) * 7 - encodedate(snye,
              YearStartAt.NextYearStartMonth, YearStartAt.NextYearStartDay)) div 7));

          if dye <= ye then
            dstr := IntToStr(1 + (round(d + (i - 1) * 7 - encodedate(spye,
              YearStartAt.PrevYearStartMonth, YearStartAt.PrevYearStartDay)) div 7));
        end;

        with r do
        begin
          left := PaintRect.Left;
          right := PaintRect.Left + FPWeekWidth;
          top := PaintRect.Top + (i - 1) * dy + yoffset - 2 + FWeekNameY;
          bottom := r.Top + dy - 2;
        end;
        
        DrawText(FCanvas.Handle, PChar(dstr), length(dstr), r,
          DT_CENTER or DT_VCENTER or DT_SINGLELINE);

      end;
    end;



    for i := 0 to (FPlannerMonthItems.Count - 1) do
    begin
      if (FPlannerMonthItems.Count = 0) then
        break;

      if (int(Now) >= TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemStartTime) and (TMonthPlannerItem(FPlannerMonthItems.Items[i]).ItemEndTime >= int(Now)) and ShowCurrentItem then
        TMonthPlannerItem(FPlannerMonthItems.Items[i]).IsCurrent := True
      else
        TMonthPlannerItem(FPlannerMonthItems.Items[i]).IsCurrent := False;

      if Assigned(TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt) then
        TMonthPlannerItem(FPlannerMonthItems.Items[i]).PosSt.Clear;
    end;

    if MaxItemsDisplayed > 0 then
    begin
      for i := 1 to 6 do
      begin
        PaintUpScrollBtn(i);
        PaintDownScrollBtn(i);
      end;
    end;

    FCanvas.Font.Assign(FFont);
    OldStyle := Font.Style;

    FocusCell := False;
    SetBKMode(FCanvas.Handle, TRANSPARENT);

    // draw day numbers here
    for i := 1 to 7 do
    begin
      if FShowLines and (look <> look3d) and ((i <= 6) or FShowScrollColumn) then
      begin
        r.right := PaintRect.Left + xoffset + i * dx + 2;
        r.top := PaintRect.Top + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;
        r.bottom := r.top + 6 * dy + 8;
        r.left := r.right - dx + 2;

        FCanvas.Pen.Color := LineColor;
        FCanvas.Pen.Width := 1;

        if Line3D then
        begin
          with FCanvas do
          begin
            MoveTo(r.Right - 1, r.Top);
            LineTo(r.Right - 1, r.Bottom);
            FCanvas.Pen.Color := clWhite;
            MoveTo(r.Right, r.Top);
            LineTo(r.Right, r.Bottom);
          end;
        end
        else
        begin
          with FCanvas do
          begin
            MoveTo(r.Right, r.Top);
            LineTo(r.Right, PaintRect.Bottom);
          end;
        end;
      end;

      for j := 1 to 6 do
      begin
        FCanvas.Font.Assign(FFont);
        ShouldNotDrawEvent := false;

        r.Right := PaintRect.Left + xoffset + i * dx + 2;
        r.Top := PaintRect.Top + (j - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;
        r.Bottom := r.Top + dy;

        if (i = 1) then
        begin
          if ShowWeeks then
            r.Left := PaintRect.Left + xOffset
          else
            r.Left := PaintRect.Left;
        end
        else
          r.Left := r.Right - dx;

        if (i = 7) and not FShowScrollColumn then
          r.Right := PaintRect.Right;

        if (j = 6) then
          r.Bottom := PaintRect.Bottom;

        if FShowLines and (look <> look3d) and (i = 1) and (j <= 6) then
        begin
          r2.Right := PaintRect.Right - 2;
          r2.Top := PaintRect.Top + (j - 2) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;
          r2.Bottom := r2.top + dy;
          r2.Left := PaintRect.Left + 2;
          FCanvas.Pen.Color := LineColor;
          FCanvas.Pen.Width := 1;
          if Line3D then
          begin
            with FCanvas do
            begin
              MoveTo(r2.Left, r2.Bottom - 1);
              LineTo(r2.Right, r2.Bottom - 1);
              FCanvas.Pen.Color := clWhite;
              MoveTo(r2.Left, r2.Bottom);
              LineTo(r2.Right, r2.Bottom);
            end;
          end
          else
          begin
            with FCanvas do
            begin
              MoveTo(r2.Left, r2.Bottom);
              LineTo(r2.Right, r2.Bottom);
            end;
          end;
        end;

        Font.Style := OldStyle;
        if (fd >= (i + (j - 1) * 7)) then
        begin
          if FShowDaysBefore then
          begin

            d := EncodeDate(pye, pmo, daysinmonth(pmo, pye) - (fd - i));
            FocusCell := (GetFocus = self.Handle) and (FocusColor <> clNone) and ((FAutoChangeMonth and (da = i + (j - 1) * 7 - fd)) or (not FAutoChangeMonth and (clkDate = d)));

            inlist := (DateCol.IsDateInList(d) <> -1) and FShowSelection;
            if FShowCurrent and (d = CurDate) and (FColorCurrent <> clNone) then
            begin
              FCanvas.Brush.Color := FColorCurrent;
              R.Right := R.Right - 1;
              R.Bottom := R.Bottom - 1;
              FCanvas.FillRect(r);
              R.Right := R.Right + 1;
              R.Bottom := R.Bottom + 1;
            end;

            if inlist then
            begin
              bkColor := FInversBkColor;
              bkColorTo := FInversBkColorTo;
              r.Left := r.Left + 1;
              r.Top := r.Top + 1;

              //if ShowSelectionFull then
              if (bkColor <> clNone) then
                DrawGradient(FCanvas, bkColor, bkColorTo, 80, r, GradientDirection = gdVertical);

              if Line3D then
              begin
                r.Left := r.Left - 1;
                r.Top := r.Top - 1;
                r.Right := r.Right - 1;
                r.Bottom := r.Bottom - 1;
              end;

              if FocusCell then
                Brush.Color := FocusColor
              else
                Brush.Color := FSelectcolor;

              Pen.Color := Brush.Color;
              Font.Color := FSelectFontColor;

              if not FShowSelectionFull then
                r.Bottom := r.Top + dnh;

              FillRect(r);
            end
            else
              Brush.Color := self.Color;

            if not inlist then
            begin
              Font.Color := FInversColor;
            end;

            if not CheckDateRange(d) then
              Font.Color := FInactiveColor;

            bkColor := FInversBkColor;
            bkColorTo := FInversBkColorTo;
            CaptionRect := R;
            CaptionS := '';

            if FShowToday and (cmo = mo) and (cye = ye) and (cda = (i + (j - 1) * 7 - fd)) and (TodayStyle = tsCaption) then
            begin
              bkColor := FTodayColor;
              bkColorTo := FTodayColorTo;
            end;

            if Assigned(FOnGetDayProp) then
              FOnGetDayProp(self, d, CaptionS, CaptionBrush, FCanvas.Font, bkColor, bkColorTo);

            if FocusCell then
            begin
              bkColor := FocusColor;
              bkColorTo := FocusColor;
            end;

            if (bkColor <> clNone) and (not inlist) then
            begin
              if Line3D then
              begin
                r.Right := r.Right - 1;
                r.Bottom := r.Bottom - 1;
              end;

              r.Left := r.Left + 1;
              r.Top := r.Top + 1;

              if not FShowSelectionFull then
              begin
                DrawGradient(FCanvas, FInversBkColor, FInversBkColorTo, 80, r, GradientDirection = gdVertical);
                r.Bottom := r.Top + dnh;
              end;

              DrawGradient(FCanvas, bkColor, bkColorTo, 80, r, GradientDirection = gdVertical);
              r.Left := r.Left - 1;
              r.Top := r.Top - 1;

              if Line3D then
              begin
                r.Right := r.Right + 1;
                r.Bottom := r.Bottom + 1;
              end;
            end;

            dstr := IntToStr(daysinmonth(pmo, pye) - (fd - i));

            if CaptionS <> '' then
            begin
              TempBrush.Assign(FCanvas.Brush);
              FCanvas.Brush.Assign(CaptionBrush);

              case FDayAlignment of
              taRightJustify: CaptionRect.Right := CaptionRect.Right - 8 - FCanvas.TextWidth(dstr) - 2;
              taLeftJustify, taCenter: CaptionRect.Left := CaptionRect.Left + 8 + FCanvas.TextWidth(dstr) + 2;
              end;

              DrawText(FCanvas.Handle, PChar(CaptionS), length(CaptionS), CaptionRect, uDayCaptionFormat);

              FCanvas.Brush.Assign(TempBrush);
            end;

            if Assigned(OnDayDraw) then
              OnDayDraw(Self, d, FCanvas, r, False);

            SetBKMode(FCanvas.Handle, TRANSPARENT);

            r.Right := r.Right - 3;


            FDayNumberHeight := DrawText(FCanvas.Handle, PChar(dstr), length(dstr), r, uDayFormat);


            r.Right := r.Right + 3;
            //CheckAndDrawEvent(d, r, j);

            Brush.Color := self.Color;
            Pen.Color := FTextcolor;

          end
          else
            ShouldNotDrawEvent := true;

        end
        else
        begin
          if ((i + (j - 1) * 7 - fd) > DaysInMonth(mo, ye)) then
          begin
            if FShowDaysAfter then
            begin

              d := EncodeDate(nye, nmo, i + (j - 1) * 7 - fd - daysinmonth(mo, ye));
              FocusCell := (GetFocus = self.Handle) and (FocusColor <> clNone) and ((FAutoChangeMonth and (da = i + (j - 1) * 7 - fd)) or (not FAutoChangeMonth and (clkDate = d)));

              if FShowCurrent and (d = CurDate) and (FColorCurrent <> clNone) then
              begin
                FCanvas.Brush.Color := FColorCurrent;
                R.Right := R.Right - 1;
                R.Bottom := R.Bottom - 1;
                FCanvas.FillRect(r);
                R.Right := R.Right + 1;
                R.Bottom := R.Bottom + 1;
              end;

              Font.Color := FInversColor;
              Brush.Color := Color;
              inlist := (DateCol.IsDateInList(d) <> -1) and FShowSelection;

              if (inlist) then
              begin
                r.Left := r.Left + 1;
                r.Top := r.Top + 1;
                r.Bottom := r.Bottom -1;
                r.Right := r.Right - 1;

                if FInversBKColor <> clNone then
                  DrawGradient(FCanvas, FInversBkColor, FInversBkColorTo, 80, r, GradientDirection = gdVertical);

                if Line3D then
                begin
                  r.Left := r.Left - 1;
                  r.Top := r.Top - 1;
                end;  

                if FocusCell then
                  Brush.Color := FocusColor
                else
                  Brush.Color := FSelectColor;

                Pen.Color := FSelectColor;
                Font.Color := FSelectFontColor;

                if not FShowSelectionFull then
                begin
                  th := r.Bottom;
                  r.Bottom := r.Top + dnh;
                end;

                FillRect(r);
              end;


              if not CheckDateRange(d) then
                Font.Color := FInactiveColor;

              bkColor := FInversBkColor;
              bkColorTo := FInversBkColorTo;
              CaptionRect := R;
              CaptionS := '';

              if FShowToday and (cmo = mo) and (cye = ye) and (cda = (i + (j - 1) * 7 - fd)) and (TodayStyle = tsCaption) then
              begin
                bkColor := FTodayColor;
                bkColorTo := FTodayColorTo;
              end;

              if Assigned(FOnGetDayProp) then
                FOnGetDayProp(self, d, CaptionS, CaptionBrush, FCanvas.Font, bkColor, bkColorTo);

              if FocusCell then
              begin
                bkColor := FocusColor;
                bkColorTo := FocusColor;
              end;

              if (bkColor <> clNone) and not inlist then
              begin
                r.Left := r.Left + 1;
                r.Top := r.Top + 1;

                if r.Bottom >= Height then
                  r.Bottom := r.Bottom - 1;

                if r.Right >= Width then
                  r.Right := r.Right - 1;

                r.Right := r.Right - 1;

                if not FShowSelectionFull then
                begin
                  DrawGradient(FCanvas, FInversBkColor, FInversBkColorTo, 80, r, GradientDirection = gdVertical);
                  //r.Bottom := r.Top + dnh;
                end;

                DrawGradient(FCanvas, bkColor, bkColorTo, 80, r, GradientDirection = gdVertical);
                r.Left := r.Left - 1;
                r.Top := r.Top - 1;
                r.Bottom := r.Bottom + 1;
                r.Right := r.Right + 1;
              end;

              //if not FShowSelectionFull then
              //  r.Bottom := th;

              dstr := IntToStr(i + (j - 1) * 7 - fd - daysinmonth(mo, ye));

              if CaptionS <> '' then
              begin
                TempBrush.Assign(FCanvas.Brush);
                FCanvas.Brush.Assign(CaptionBrush);

                case FDayAlignment of
                taRightJustify: CaptionRect.Right := CaptionRect.Right - 8 - FCanvas.TextWidth(dstr) - 2;
                taLeftJustify, taCenter: CaptionRect.Left := CaptionRect.Left + 8 + FCanvas.TextWidth(dstr) + 2;
                end;

                DrawText(FCanvas.Handle, PChar(CaptionS), length(CaptionS), CaptionRect, uDayCaptionFormat);

                FCanvas.Brush.Assign(TempBrush);
              end;

              if Assigned(OnDayDraw) then
                OnDayDraw(Self, d, FCanvas, r, False);

              r.Right := r.Right - 3;

              SetBKMode(FCanvas.Handle, TRANSPARENT);
              DrawText(FCanvas.Handle, PChar(dstr), length(dstr), r, uDayFormat);


              r.Right := r.Right + 2;

              Brush.Color := self.Color;
              Pen.Color := FTextColor;

            end
            else
              ShouldNotDrawEvent := true;

          end
          else
          begin
            d := EncodeDate(ye, mo, (i + (j - 1) * 7 - fd));

            FocusCell := (GetFocus = self.Handle) and (FocusColor <> clNone) and ((FAutoChangeMonth and (da = i + (j - 1) * 7 - fd)) or (not FAutoChangeMonth and (clkDate = d)));

            inlist := (DateCol.IsDateInList(d) <> -1) and (FShowSelection);

            if inlist then
            begin
              if inlist then
              begin
                if FocusCell then
                  Brush.Color := FocusColor
                else
                  Brush.Color := FSelectColor;

                Font.Color := FSelectFontColor;
              end
              else
              begin
                if FocusCell then
                begin
                  Brush.Color := FFocusColor;
                  Font.Color := FInversColor;
                  Pen.Color := FFocusColor;
                end
                else
                begin
                  Brush.Color := FSelectColor;
                  Font.Color := FSelectFontColor;
                  Pen.Color := FSelectColor;
                end;
              end;

              if fLook = Look3d then
                r := Rect(r.Left + 1, r.Top + 1, r.Right -1, r.Bottom -1);

              if not Line3D then
              begin
                r.Bottom := r.Bottom  + 1;
                r.Right := r.Right  + 1;
              end;

              if not FShowSelectionFull then
              begin
                th := r.Bottom;
                r.Bottom := r.Top + dnh;
              end;

              if FShowToday and (cmo = mo) and (cye = ye) and (cda = (i + (j - 1) * 7 - fd)) and (TodayStyle = tsCaption) then
              begin
                tr := r;
                r.Left := r.Left + 1;
                r.Top := r.Top + 1;
                r.Bottom := r.Bottom + 1;

                if ShowSelectionFull then
                  r.Bottom := r.Top + dnh;

                DrawGradient(FCanvas, TodayColor, TodayColorTo, 20, r, False);

                if not ShowSelectionFull then
                  case DayAlignment of
                  taLeftJustify: r.Right := r.Left + TextWidth(dstr) + 16;
                  taRightJustify: r.Left := r.Right - TextWidth(dstr) - 16;
                  end;

                if ShowSelectionFull then
                begin
                  r.Top := tr.Top + dnh;
                  r.Bottom := tr.Bottom;
                end;

                if FocusCell then
                  Brush.Color := FocusColor
                else
                  Brush.Color := SelectColor;

                FillRect(r);

                r := tr;
              end
              else
              begin
                if not Line3D then
                begin
                  r.Left := r.Left + 1;
                  r.Top := r.Top + 1;
                end;

                if ShowSelectionFull then
                  r.Bottom := r.Bottom - 1;

                r.Right := r.Right - 1;

                if i = 1 then
                  r.Left := r.Left + 1;

                Fillrect(r);

                if not Line3D then
                begin
                  r.Left := r.Left - 1;
                  r.Top := r.Top - 1;
                end;
                r.Bottom := r.Bottom + 1;
                r.Right := r.Right + 1;

              end;

              if not FShowSelectionFull then
                r.Bottom := th;

              if not Line3D then
              begin
                r.Bottom := r.Bottom  - 1;
                r.Right := r.Right  - 1;
              end;

              if (FLook = Look3d) then
                Frame3d(FCanvas, r, clWhite, clGray, 1);

              bkColor := clNone;
              bkColorTo := clNone;
              CaptionRect := R;
              CaptionS := '';

              if Assigned(FOnGetDayProp) then
                FOnGetDayProp(self, d, CaptionS, CaptionBrush, FCanvas.Font, bkColor, bkColorTo);

              if FocusCell then
              begin
                bkColor := FocusColor;
                bkColorTo := FocusColor;
              end;

              if (bkColor <> clNone) and not inlist then
              begin
                r.Right := r.Right - 1;
                r.Bottom := r.Bottom - 1;
                DrawGradient(FCanvas, bkColor, bkColorTo, 80, r, GradientDirection = gdVertical);
                r.Right := r.Right + 1;
                r.Bottom := r.Bottom + 1;
              end;

              if CaptionS <> '' then
              begin
                TempBrush.Assign(FCanvas.Brush);
                FCanvas.Brush.Assign(CaptionBrush);

                case FDayAlignment of
                taRightJustify: CaptionRect.Right := CaptionRect.Right - 8 - FCanvas.TextWidth(IntToStr(i + (j - 1) * 7 - fd)) - 2;
                taLeftJustify, taCenter: CaptionRect.Left := CaptionRect.Left + 8 + FCanvas.TextWidth(IntToStr(i + (j - 1) * 7 - fd)) + 2;
                end;

                DrawText(FCanvas.Handle, PChar(CaptionS), length(CaptionS), CaptionRect, uDayCaptionFormat);
                FCanvas.Brush.Assign(TempBrush);
              end;

              if Assigned(OnDayDraw) then
                OnDayDraw(Self, d, FCanvas, r, False);

              r.Right := r.Right - 3;

              Setbkmode(FCanvas.Handle, TRANSPARENT);
              DrawText(FCanvas.Handle, PChar(IntToStr(i + (j - 1) * 7 - fd)),
                length(IntToStr(i + (j - 1) * 7 - fd)), r, uDayFormat);


              r.Right := r.Right + 3;

              if (DayOfWeek(d) in [1, 7]) and (WeekendColor <> clNone) and not ShowSelectionFull then
              begin
                Brush.Color := WeekendColor;
                Pen.Color := WeekendColor;
                r.Top := r.Top + dnh;
                r.Left := r.Left + 1;
                if Line3D then
                  r.Right := r.Right - 1;
                Fillrect(r);
                
                r.Left := r.Left - 1;
                if Line3D then
                  r.Right := r.Right + 1;
              end;

              if fLook = Look3d then
                r := Rect(r.Left - 1, r.Top - 1, r.Right + 1, r.Bottom + 1);

              Brush.Color := self.Color;
              Pen.Color := FTextColor;
            end
            else
            begin //check to see if weekend day here
              if true {not IsEvent} then
              begin
                if DayOfWeek(d) in [1, 7] then
                  Font.Color := FWeekendTextColor
                else
                  Font.Color := FTextcolor;
              end;
              if not CheckDateRange(d) then
                Font.Color := FInactiveColor;

              if FLook = Look3d then
              begin
                r := Rect(r.Left + 1, r.Top + 1, r.Right - 1, r.Bottom - 1);
                Frame3d(FCanvas, r, clWhite, clGray, 1);
              end;

              if FShowCurrent and (d = CurDate) and (FColorCurrent <> clNone) then
              begin
                FCanvas.Brush.Color := FColorCurrent;
                R.Right := R.Right - 1;
                R.Bottom := R.Bottom - 1;
                FCanvas.FillRect(r);
                R.Right := R.Right + 1;
                R.Bottom := R.Bottom + 1;
              end
              else
              begin
                if (DayOfWeek(d) in [1, 7]) and (WeekendColor <> clNone) then
                begin
                  FCanvas.Brush.Color := WeekendColor;
                  FCanvas.Pen.Width := 1;
                  FCanvas.Pen.Color := WeekendColor;

                  {
                  if ShowLines then
                    FCanvas.Pen.Color := LineColor
                  else
                    FCanvas.Pen.Color := WeekendColor;


                  if ShowLines then
                  begin
                    if Line3D then
                    begin
                      R.Left := R.Left - 1;
                      R.Top := R.Top - 1;
                    end
                    else
                    begin
                      R.Right := R.Right + 1;
                      R.Bottom  := R.Bottom + 1;
                    end;
                  end;
                  }
                  if (i = 1) and Line3D then
                    r.Left := r.Left + 1;

                  if Line3D then
                  begin
                    r.Right := r.Right - 1;
                    r.Bottom := r.Bottom - 1;

                  end
                  else
                  begin
                    r.Left := r.Left + 1;
                    r.Top := r.Top + 1;
                  end;

                  FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
                  {
                  if ShowLines then
                  begin
                    if Line3D then
                    begin
                      R.Left := R.Left + 1;
                      R.Top := R.Top + 1;
                    end
                    else
                    begin
                      R.Right := R.Right - 1;
                      R.Bottom  := R.Bottom - 1;
                    end;
                  end;
                  }
                end;
              end;

              bkColor := clNone;
              bkColorTo := clNone;
              CaptionRect := R;
              CaptionS := '';

              if FShowToday and (cmo = mo) and (cye = ye) and (cda = (i + (j - 1) * 7 - fd)) and (TodayStyle = tsCaption) then
              begin
                bkColor := FTodayColor;
                bkColorTo := FTodayColorTo;
              end;

              if Assigned(FOnGetDayProp) then
                FOnGetDayProp(self, d, CaptionS, CaptionBrush, FCanvas.Font, bkColor, bkColorTo);

              if FocusCell then
              begin
                bkColor := FocusColor;
                bkColorTo := FocusColor;
              end;

              if (bkColor <> clNone) then
              begin
                if not Line3D then
                begin
                  // r.Right := r.Right - 1;
                  // r.Bottom := r.Bottom + 2;
                  r.Top := r.Top + 1;
                  r.Left := r.Left + 1;
                  r.Bottom := r.Bottom - 1;

                  if not FShowSelectionFull then
                    r.Bottom := r.Top + dnh;
                end;

                DrawGradient(FCanvas, bkColor, bkColorTo, 20, r, GradientDirection = gdVertical);

                if not Line3D then
                begin
                  r.Top := r.Top - 1;
                  r.Bottom := r.Bottom + 1;
                  r.Left := r.Left - 1;
                  //r.Right := r.Right + 1;
                  //r.Bottom := r.Bottom - 2;
                end;
              end;

              if CaptionS <> '' then
              begin
                TempBrush.Assign(FCanvas.Brush);
                FCanvas.Brush.Assign(CaptionBrush);
                case FDayAlignment of
                taRightJustify: CaptionRect.Right := CaptionRect.Right - 8 - FCanvas.TextWidth(IntToStr(i + (j - 1) * 7 - fd)) - 2;
                taLeftJustify, taCenter: CaptionRect.Left := CaptionRect.Left + 8 + FCanvas.TextWidth(IntToStr(i + (j - 1) * 7 - fd)) - 2;
                end;

                DrawText(FCanvas.Handle, PChar(CaptionS), length(CaptionS), CaptionRect, uDayCaptionFormat);


                FCanvas.Brush.Assign(TempBrush);
              end;

              if Assigned(OnDayDraw) then
                OnDayDraw(Self, d, FCanvas, r, False);

              r.Right := r.Right - 3;

              SetBKMode(FCanvas.handle, TRANSPARENT);
              DrawText(FCanvas.Handle, PChar(IntToStr(i + (j - 1) * 7 - fd)),
                length(IntToStr(i + (j - 1) * 7 - fd)), r, uDayFormat);

              r.Right := r.Right + 3;

              if FLook = Look3d then
                r := Rect(r.Left - 1, r.Top - 1, r.Right + 1, r.Bottom + 1);
            end;
          end;
        end;

        if FocusCell and ShowFocusRectangle then
        begin
          Font.Color := clBlack;
          Windows.DrawFocusRect(FCanvas.Handle, r);
        end;

        //draw current day in active color
        if (cda = i + (j - 1) * 7 - fd) and (cmo = mo) and (cye = ye) and FShowToday and (TodayStyle <> tsCaption) then
        begin
          if FShowToday then
          begin
            case TodayStyle of
            tsSunken: Pen.Color := clGray;
            tsRaised: Pen.Color := clWhite;
            tsFlat: Pen.Color := LineColor;
            end;

            Canvas.Pen.Width := 1;

            MoveTo(r.Left + 1, r.Bottom - 1);
            LineTo(r.Left + 1, r.Top + 1);
            LineTo(r.Right - 1, r.Top + 1);

            case TodayStyle of
            tsSunken: Pen.Color := clWhite;
            tsRaised: Pen.Color := clGray;
            tsFlat: Pen.Color := LineColor;
            end;

            LineTo(r.Right - 1, r.Bottom - 1);
            LineTo(r.Left + 1, r.Bottom - 1);

            Pen.Color := clBlack;
          end;
        end;

        if not ShouldNotDrawEvent then
          CheckAndDrawEvent(d, r, j, i, Print);
      end;
    end;
  end;
  CaptionBrush.Free;
  TempBrush.Free;
end;

procedure TPlannerMonthView.SetDate(da, mo, ye: word);
var
  R: TRect;
  dt: TDateTime;
begin
  r := DateToRect(SelDate);

  if da > DaysInMonth(mo, ye) then
    da := DaysInMonth(mo, ye);

  dt := EncodeDate(ye, mo, da);
  TheDate := dt;
  SelDate := thedate;

  InvalidateRectangle(r, False);

  SetLabel(mo, ye);
  FMonth := mo;
  FYear := ye;
  InitDate := SelDate;
  DateCol.Clear;
  DateCol.Add.Date := seldate;

  Invalidate;
end;

procedure TPlannerMonthView.GetDate(var da, mo, ye: word);
begin
  DecodeDate(seldate, ye, mo, da);
end;

procedure TPlannerMonthView.GetEndDate(var da, mo, ye: word);
var
  d: TDateTime;
  fd: integer;
  nmo, nye: word;
begin

  DecodeDate(TheDate, ye, mo, da);
  d := EncodeDate(ye, mo, 1);
  //first day of the month
  fd := DayOfWeek(d) - 1 - StartDay;

  if fd < 0 then
    fd := fd + 7;

  //determine next month
  if mo = 12 then
  begin
    nmo := 1;
    nye := ye + 1;
  end
  else
  begin
    nmo := mo + 1;
    nye := ye;
  end;

  //d := EncodeDate(nye, nmo, i + (j - 1) * 7 - fd - daysinmonth(mo, ye));
  da := 7 + (6 - 1) * 7 - fd - daysinmonth(mo, ye);
  ye := nye;
  mo := nmo;
end;

procedure TPlannerMonthView.GetStartDate(var da, mo, ye: word);
var
  d: TDateTime;
  fd: integer;
  pmo, pye: word;
begin
  DecodeDate(TheDate, ye, mo, da);
  d := EncodeDate(ye, mo, 1);
  //first day of the month
  fd := DayOfWeek(d) - 1 - StartDay;

  if fd < 0 then
    fd := fd + 7;

  //determine previous month
  if fd = 0 then
  begin
    pmo := mo;
    pye := ye;
  end
  else
  begin
    if mo = 1 then
    begin
      pmo := 12;
      pye := ye - 1;
    end
    else
    begin
      pmo := mo - 1;
      pye := ye;
    end;
  end;
  //d := EncodeDate(pye, pmo, daysinmonth(pmo, pye) - (fd - 1));
  if fd = 0 then
    da := 1
  else
    da := daysinmonth(pmo, pye) - (fd - 1);
  ye := pye;
  mo := pmo;
end;

function TPlannerMonthView.GetDatesAsText: string;
var
  i: integer;
  s: string;
  Continuous: Boolean;
begin
  Continuous := False;
  s := '';
  with Dates do
  begin
    for i := 0 to Count - 2 do
    begin
      if Items[i].Date + 1 = Items[i + 1].Date then
      begin
        if not Continuous then
        begin
          s := s + DateToStr(Items[i].Date) + '-';
          Continuous := True;
        end
      end
      else
      begin
        s := s + DateToStr(Items[i].Date) + ',';
        Continuous := False;
      end;
    end;

    if Count > 0 then
      s := s + DateToStr(Items[Count - 1].Date);
  end;
  Result := s;
end;

procedure TPlannerMonthView.ReadTMSStyle(Reader: TReader);
begin
  FTMSStyle := TTMSStyle(Reader.ReadInteger);
end;

procedure TPlannerMonthView.RepaintDate(dt: tdatetime);
var
  pt: TPoint;
  r: TRect;
begin
  if FUpdateCount > 0 then
    Exit;
  pt := DateToXY(dt);
  if pt.x = 0 then
    pt.x := 7;

  r.top := FweekNameY + FDayFontHeight + (pt.y - 1) * dy;

  r.bottom := r.top + dy;
  r.left := xoffset + (pt.x - 1) * dx;
  r.right := r.left + dx + 2;
  if (Pt.X = 7) and not FShowScrollColumn then
    r.Right := width;

  InvalidateRectangle(r, True);
end;

function TPlannerMonthView.DateToRect(dt: tdatetime): trect;
var
  pt: tpoint;
  r: trect;
begin
  pt := datetoxy(dt);
  if pt.x = 0 then
    pt.x := 7;

  r.top := FweekNameY + FDayFontHeight + (pt.y - 1) * dy;

  r.bottom := r.top + dy;
  r.left := xoffset + (pt.x - 1) * dx;
  r.right := r.left + dx + 2;

  if (Pt.X = 7) and not FShowScrollColumn then
    r.Right := width;

  Result := r;
end;

function TPlannerMonthView.DateToXY(dt: tdatetime): tpoint;
var
  ye, mo, da: word;
  tmpdt: tdatetime;
  fd: integer;
  rx, ry: integer;

begin
  decodedate(thedate, ye, mo, da);

  tmpdt := encodedate(ye, mo, 1); {first day of month}

  fd := dayofweek(tmpdt) - 1 - startday;

  if fd < 0 then fd := fd + 7;

  tmpdt := tmpdt - fd; {this is the first day of the calendar}
  fd := round(dt - tmpdt) + 1;

  rx := (fd mod 7);
  ry := (fd div 7) + 1;

  if (rx = 0) then
  begin
    rx := 7;
    dec(ry);
  end;
  Result.x := rx;
  Result.y := ry;
end;

function TPlannerMonthView.DateAtXY(X, Y: Integer; var ADate: TDateTime): Boolean;
begin
  Result := False;

  if (X > 0) and (Y > YOffset + FWeekNameY - 2) then
  begin
    ADate := XYToDate(X - xoffset , Y, False);
    Result := True;
  end;
end;

function TPlannerMonthView.XYToDate(X, Y: integer; change: Boolean): TDateTime;
var
  ye, mo, da: word;
  xcal, ycal: integer;
  sda, fd: integer;
  tmpdt: tdatetime;
begin
  xposin := x;
  yposin := y;
  xcal := 0;
  ycal := 0;

  //DecodeDate(seldate, ye, mo, da);
  ye := FYear;
  mo := FMonth;

  tmpdt := EncodeDate(ye, mo, 1);

  fd := DayOfWeek(tmpdt) - 1 - StartDay;

  if (fd < 0) then
    fd := fd + 7;

  if (dx > 0) and (dy > 0) then
  begin
    xcal := (x - 1) div dx;
    ycal := ((y - (FweekNameY + FDayFontHeight))) div dy;
    if ycal < 0 then
      ycal := 0;
  end;

  if xcal > 6 then xcal := 6;
  if ycal > 5 then ycal := 5;

  sda := xcal + 7 * ycal - fd + 1;

  if sda < 1 then
  begin
    Dec(mo);
    if mo = 0 then
    begin
      mo := 12;
      Dec(ye);
    end;
    sda := DaysInMonth(mo, ye) + sda;
    if Change and FShowDaysBefore then
      ChangeMonth(-1);
  end;

  if sda > DaysInMonth(mo, ye) then
  begin
    sda := sda - DaysInMonth(mo, ye);
    Inc(mo);
    if mo > 12 then
    begin
      mo := 1;
      Inc(ye);
    end;
    if Change and FShowDaysAfter then
      ChangeMonth(+1);
  end;

  da := sda;
  Result := EncodeDate(ye, mo, da);
end;


procedure TPlannerMonthView.YearStartAtChanged(Sender: TObject);
begin
  DiffYear(-1);
end;

procedure TPlannerMonthView.MouseMove(Shift: TShiftState; X, Y: integer);
var
  dt, dfDate: TDateTime;
  newpt: TPoint;
  WidthX1, WidthX2, HeightY1, HeightY2: integer;
  APlannerItem: TPlannerItem;
  R: TRect;
  ad: TDateTime;
  OldStartTime, OldEndTime: TDateTime;
  NewTime, NewRealTime: TDateTime;
  Allow: Boolean;

begin
  inherited;

  x := x - xoffset;

  if ShowCaption then
  begin

    if (x >= lblx1) and (x <= lblx3) and (y > 0) and (y < CaptionHeight) and FMonthSelect then
    begin
      if not flgla then
      begin
        flgla := True;
        PaintLabel(ClientRect);
      end;
    end
    else if flgla then
    begin
      flgla := False;
      PaintLabel(ClientRect);
    end;

    BrowserHint := '';

    if FBrowsers.FPrevMonth then
    begin
      if FGlyphs.FPrevMonth.Empty then
      begin
        if FGlyphs.FPrevYear.Empty then
        begin
          WidthX1 := 25;
          WidthX2 := 35;
        end
        else
        begin
          WidthX1 := 25;
          WidthX2 := 25 + FGlyphs.FPrevYear.Width;
        end;
        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        if FGlyphs.FPrevYear.Empty then
        begin
          WidthX1 := 25;
          WidthX2 := 25 + FGlyphs.FPrevMonth.Width;
        end
        else
        begin
          WidthX1 := 10 + FGlyphs.FPrevYear.Width;
          WidthX2 := 10 + FGlyphs.FPrevYear.Width + FGlyphs.FPrevMonth.Width;
        end;

        HeightY1 := 0;
        HeightY2 := FGlyphs.FPrevMonth.Height;
      end;

      if not FBrowsers.FPrevYear then
      begin
        WidthX1 := WidthX1 - 20;
        WidthX2 := WidthX2 - 20;
      end;

      if (x > WidthX1 - MM) and (x < WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        Cursor := crHandPoint;
        BrowserHint := FHintPrevMonth;
        if FShowWeeks then
          FLastHintPos := Point((Width div 8) + WidthX1, -8)
        else
          FLastHintPos := Point(WidthX1, -8);

        if not flgl then
        begin
          flgl := True;
          PaintArrowLeft(ClientRect);
        end;
      end
      else
      begin
        if flgl then
        begin
          Cursor := FOldCursor;
          flgl := False;
          PaintArrowLeft(ClientRect);
        end;
      end;
    end;

    if FBrowsers.FPrevYear then
    begin
      if FGlyphs.FPrevYear.Empty then
      begin
        WidthX1 := 5;
        WidthX2 := 15;
        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        WidthX1 := 5;
        WidthX2 := 5 + FGlyphs.FPrevYear.Width;
        HeightY1 := 0;
        HeightY2 := FGlyphs.FPrevYear.Height;
      end;


      if (x > WidthX1 - MM) and (x < WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        Cursor := crHandPoint;
        BrowserHint := FHintPrevYear;
        if FShowWeeks then
          FLastHintPos := Point((Width div 8) + WidthX1, -8)
        else
          FLastHintPos := Point(WidthX1, -8);

        if not dflgl then
        begin
          dflgl := True;
          PaintDblArrowLeft(ClientRect);
        end;
      end
      else
      begin
        if dflgl then
        begin
          Cursor := FOldCursor;
          dflgl := False;
          PaintDblArrowLeft(ClientRect);
        end;
      end;
    end;

    if FBrowsers.FNextMonth then
    begin
      if not FGlyphs.FNextMonth.Empty then
      begin
        if FGlyphs.FNextYear.Empty then
        begin
          WidthX1 := 25 + FGlyphs.FNextMonth.Width;
          WidthX2 := 25;
        end
        else
        begin
          WidthX1 := 10 + FGlyphs.FNextYear.Width + FGlyphs.FNextMonth.Width;
          WidthX2 := 10 + FGlyphs.FNextYear.Width;
        end;

        HeightY1 := 0;
        HeightY2 := FGlyphs.FNextMonth.Height;
      end
      else
      begin
        if FGlyphs.FNextYear.Empty then
        begin
          WidthX1 := 30;
          WidthX2 := 25;
        end
        else
        begin
          WidthX1 := 15 + FGlyphs.FNextYear.Width;
          WidthX2 := 10 + FGlyphs.FNextYear.Width;
        end;

        HeightY1 := 0;
        HeightY2 := 15;
      end;

      if not FBrowsers.NextYear then
      begin
        WidthX1 := WidthX1 - 20;
        WidthX2 := WidthX2 - 20;
      end;

      if (x + xoffset > Width - WidthX1 - MM) and (x + xoffset < Width - WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        Cursor := crHandPoint;
        BrowserHint := FHintNextMonth;
        FLastHintPos := Point(Width - WidthX1, -8);
        if not flgr then
        begin
          flgr := True;
          PaintArrowRight(ClientRect);
        end;
      end
      else
      begin
        if flgr then
        begin
          Cursor := FOldCursor;
          flgr := False;
          PaintArrowRight(ClientRect);
        end;
      end;
    end;

    if FBrowsers.FNextYear then
    begin
      if FGlyphs.FNextYear.Empty then
      begin
        WidthX1 := 15;
        WidthX2 := 5;
        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        WidthX1 := FGlyphs.FNextYear.Width + 5;
        WidthX2 := 5;
        HeightY1 := 0;
        HeightY2 := FGlyphs.FNextYear.Height;
      end;

      if (x + xoffset > Width - WidthX1 - MM) and (x + xoffset < Width - WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        Cursor := crHandPoint;
        BrowserHint := FHintNextYear;
        FLastHintPos := Point(Width - WidthX1, -8);
        if dflgr = False then
        begin
          dflgr := True;
          PaintDblArrowRight(ClientRect);
        end;
      end
      else
      begin
        if dflgr then
        begin
          Cursor := FOldCursor;
          dflgr := False;
          PaintDblArrowRight(ClientRect);
        end;
      end;
    end;

    if not (flgl or flgr or dflgl or dflgr or flgt) and (Cursor <> FOldCursor) then
    begin
      Cursor := FOldCursor;
    end;
  end
  else
  begin
    if (Cursor <> FOldCursor) then
    begin
      Cursor := FOldCursor;
    end;
  end;

  dt := XYToDate(X + WeekWidth, Y, False);
  if (dt <> FToolTipPos) then
  begin
    SendMessage(FHToolTip, TTM_POP, 0, 0);
  end;
  FToolTipPos := dt;

  EventHint := '';

  if (y > (FweekNameY + FDayFontHeight)) and FMouseDownResize and Assigned(Items.Selected) then
  begin
    dt := XYToDate(X, Y, False);

    if (FStartMovedate <> dt) and CheckDateRange(dt) then
    begin
      dfDate := dt - FStartMovedate;
      if FMouseOnItemStart then
      begin
        if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime) then
        begin
          OldStartTime := Items.Selected.ItemStartTime;
          OldEndTime := Items.Selected.ItemEndTime;
          NewTime := Items.Selected.ItemStartTime + dfDate;
          NewRealTime := Int(NewTime) + Frac(Items.Selected.ItemRealStartTime);

          Allow := true;
          ItemSizing(Items.Selected, OldStartTime, OldEndTime, NewTime, Items.Selected.ItemEndTime, Allow);

          if Allow then
          begin
            FStartMovedate := dt;
            Items.Selected.ItemStartTime := NewTime;
            Items.Selected.ItemRealStartTime := NewRealTime;

            Invalidate;
          end
          else
          begin
            SendMessage(Handle,WM_LBUTTONUP,0,0);
            Exit;
          end;

        end;
      end
      else
      begin
        if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime, Items.Selected.ItemEndTime + dfDate) then
        begin
          OldStartTime := Items.Selected.ItemStartTime;
          OldEndTime := Items.Selected.ItemEndTime;

          NewTime := Items.Selected.ItemEndTime + dfDate;
          NewRealTime := Int(NewTime) + Frac(Items.Selected.ItemRealEndTime);

          Allow := true;

          ItemSizing(Items.Selected, OldStartTime, OldEndTime, Items.Selected.ItemStartTime, NewTime , Allow);

          if Allow then
          begin
            FStartMovedate := dt;
            Items.Selected.ItemEndTime := NewTime;
            Items.Selected.ItemRealEndTime := NewRealTime;

            Invalidate;
          end
          else
          begin
            SendMessage(Handle,WM_LBUTTONUP,0,0);
            Exit;
          end;

        end;
      end;
    end;
    Exit;
  end;


  if (y > (FweekNameY + FDayFontHeight)) and FMouseDownMove and Assigned(Items.Selected) then
  begin
      if (ssCtrl in Shift) or DragItemAlways (*or (GetKeystate(VK_MENU) and $8000 = $8000) or DirectDrag*) then
      begin
        if DragItem and (Cursor <> crSizeAll) and (Cursor <> crSizeNS) and (Cursor <> crSizeWE) then
        begin
          Allow := True;

          if Assigned(FPlanner.OnItemDrag) then
            FPlanner.OnItemDrag(FPlanner, FPlanner.Items.Selected, Allow);

          if Allow then
          begin
            inherited;

            if DragItemImage then
            begin
              CreateDragImage(Items.Selected);
              Items.Selected.Visible := false;
            end;

            BeginDrag(True, -1);
            FMouseDownMove := False;
            Exit;
          end;
        end;
      end;


    dt := XYToDate(X , Y, False);
    if (FStartMovedate <> dt) and CheckDateRange(dt) then
    begin

      if (*(ssCtrl in Shift) or (GetKeystate(VK_MENU) and $8000 = $8000) or *) DirectDrag then
      begin
        if DragItem and (Cursor <> crSizeAll) and (Cursor <> crSizeNS) and (Cursor <> crSizeWE) then
        begin
          Allow := True;

          if Assigned(FPlanner.OnItemDrag) then
            FPlanner.OnItemDrag(FPlanner, FPlanner.Items.Selected, Allow);

          if Allow then
          begin
            inherited;
            BeginDrag(True, -1);
            FMouseDownMove := False;
            Exit;
          end;
        end;
      end;

      dfDate := dt - FStartMovedate;


      if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime + dfDate) then
      begin
        Allow := true;
        TMonthPlannerItem(Items.Selected).MoveMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime + dfDate, false, Allow);
        if Allow then
        begin
          FStartMovedate := dt;


          Invalidate;
        end;
      end;
    end;
    Exit;
  end;

  FMouseOverTrack := false;
  FMouseOnItemStart := false;
  FMouseOnItemEnd := false;

  ad := XYToDate(X, Y, false);

  APlannerItem := FPlannerMonthItems.FindItemAtDate(ad, X + xoffset, Y);

  if (APlannerItem <> nil) and (APlannerItem = Items.Selected) then
  begin
    if APlannerItem.Focus then
    begin
      if (APlannerItem.CaptionType <> ctNone) then
      begin
        R := ItemRectAtRow(APlannerItem, DateToXY(ad).Y);
        R.Bottom := R.Top + 18;

        if APlannerItem.Attachement <> '' then
        begin
          R.Left := R.Right - FPlanner.AttachementGlyph.Width;
          
          if ShowWeeks then
            x := x + xoffset;

          if PtInRect(R, Point(X, Y)) then
          begin
            if Self.Cursor <> crHandPoint then
            begin
              Self.Cursor := crHandPoint;
            end;
            Exit;
          end;

          if ShowWeeks then
            x := x - xoffset;

          R.Right := R.Right - FPlanner.AttachementGlyph.Width;
        end;

        if APlannerItem.URL <> '' then
        begin
          R.Left := R.Right - FPlanner.URLGlyph.Width;
          if ShowWeeks then
            x := x + xoffset;

          if PtInRect(R, Point(X, Y)) then
          begin
            if Self.Cursor <> crHandPoint then
            begin
              Self.Cursor := crHandPoint;
            end;
            Exit;
          end;
          if ShowWeeks then
            x := x - xoffset;

        end;

      end;
    end;

    R := ItemRectAtDate(APlannerItem, ad);

    if not APlannerItem.FixedSize then
    begin
      x := x + xoffset;

      if (ad = int(APlannerItem.ItemStartTime)) and (X >= R.Left) and (X <= R.Left + 5) then
      begin
        if APlannerItem.cursor = crNone then
           self.Cursor := crSizeWE
        else
           self.Cursor := APlannerItem.Cursor;

        //if (Cursor <> crSizeWE) then
        //  Cursor := crSizeWE;
        FMouseOnItemStart := true;
        Exit;
      end
      else if (ad = int(APlannerItem.ItemEndTime)) and (X <= R.Right) and (X >= R.Right - 5) then
      begin
        if APlannerItem.cursor = crNone then
           self.Cursor := crSizeWE
        else
           self.Cursor := APlannerItem.Cursor;

        //if (Cursor <> crSizeWE) then
        //  Cursor := crSizeWE;

        FMouseOnItemEnd := true;
        Exit;

      end;

      x := x - xoffset;
    end;

    if not APlannerItem.FixedPos then
    begin
      if DirectMove and APlannerItem.NotEditable then
      begin
        if APlannerItem.cursor = crNone then
           self.Cursor := crSizeAll
        else
           self.Cursor := APlannerItem.Cursor;

        //if (Cursor <> crSizeAll) then
        //  Cursor := crSizeAll;
        Exit;
      end
      else
      begin
        if (y >= R.Top) and (Y <= R.Top + TrackWidth + TMonthPlannerItem(APlannerItem).GetCaptionHeight) then
        begin
          if APlannerItem.ShowDeleteButton and (X > R.Right - 20) then
          begin
            self.Cursor := crHandPoint;
          end
          else
          begin
            if APlannerItem.Cursor = crNone then
               self.Cursor := crSizeAll
            else
               self.Cursor := APlannerItem.Cursor;
            FMouseOverTrack := true;
          end;

          //if (Cursor <> crSizeAll) then
          //  Cursor := crSizeAll;
          Exit;
        end;
      end;
    end;
  end;

  if (y > (FweekNameY + FDayFontHeight)) then
  begin
    dt := XYToDate(X, Y, False);

    APlannerItem := FPlannerMonthItems.FindItemAtDate(dt, X, Y);

    if (APlannerItem <> FHintItem) then
      Application.CancelHint;

    FHintItem := APlannerItem;

    if (dx > 0) and (dy > 0) then
    begin
      newpt.x := x div dx;
      newpt.y := ((y - (FweekNameY + FDayFontHeight))) div dy;
    end;

    if ((newpt.x <> flasthintpos.x) or
      (newpt.y <> flasthintpos.y)) then
    begin
      Application.CancelHint;
    end;

    FLastHintPos := newpt;

    if MouseSel and (MovDate <> dt) and CheckDateRange(dt) then
    begin
      if MovDate <> -1 then
        DateCol.DelRange(clkdate, movdate);
      MovDate := dt;
      DateCol.AddRange(clkdate, movdate);
    end;
  end;
end;

procedure TPlannerMonthView.DoItemPopupPrepare(Sender: TObject; PopupMenu:TPopupMenu; Item: TPlannerItem);
begin
  if Assigned(OnItemPopupPrepare) then
    OnItemPopupPrepare(Sender, PopupMenu, Item);
end;

procedure TPlannerMonthView.MouseDown(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  ye, da, omo, nmo: word;
  lidx: integer;
  origdate: TDatetime;
  r, tr: TRect;
  flg: Boolean;
  WidthX1, WidthX2, HeightY1, HeightY2: integer;
  APlannerItem: TPlannerItem;
  TempD: TDateTime;
  AutoHandle: Boolean;
  ScreenPoint: TPoint;
  i,th: integer;
begin
  if Button <> mbLeft then
  begin
    inherited;

    if (Button = mbRight) and (y > (FweekNameY + FDayFontHeight)) and (x > 0) then
    begin
      TempD := XYToDate(X - xoffset, Y, false);
      ScreenPoint := ClientToScreen(Point(X, Y));

      APlannerItem := FPlannerMonthItems.FindItemAtDate(TempD, X, Y);
      if APlannerItem <> nil then
      begin
        if Assigned(FOnItemRightClick) then
          FOnItemRightClick(self, APlannerItem);

        if Assigned(APlannerItem.PopupMenu) then
        begin
          DoItemPopupPrepare(self, APlannerItem.PopupMenu, APlannerItem);

          //if Assigned(OnItemPopupPrepare) then
          //  FPlanner.OnItemPopupPrepare(self, APlannerItem.PopupMenu, APlannerItem);

          FPopupPlannerItem := APlannerItem;
          APlannerItem.PopupMenu.PopupComponent := Self;
          APlannerItem.PopupMenu.Popup(ScreenPoint.X, ScreenPoint.Y);
        end;

        if (Assigned(FItemPopup)) then
        begin
          DoItemPopupPrepare(self, FItemPopup, APlannerItem);

          //if Assigned(OnItemPopupPrepare) then
          //  OnItemPopupPrepare(self, ItemPopup, APlannerItem);

          FPopupPlannerItem := APlannerItem;
          FItemPopup.PopupComponent := Self;
          FItemPopup.Popup(ScreenPoint.X, ScreenPoint.Y);
        end;
      end;
    end;
    Exit;
  end;

  origdate := seldate;
  xposin := $7FFF;
  yposin := $7FFF;

  if not (GetFocus = self.Handle) then
    SetFocus;

  x := x - xoffset;
  flg := False;

  if ShowCaption then
  begin
    Canvas.Font.Assign(CaptionFont);
    th := Canvas.TextHeight('gh');

    if (x >= lblx1) and (x <= lblx2) and (y > 0) and (y <= th + 2) then
    begin
      if FShowMonthPopup then
        DoMonthPopup;
      inherited;
      Exit;
    end;

    if (x >= lblx2) and (x <= lblx3) and (y > 0) and (y <= th + 2) then
    begin
      if FShowYearPopup then
        DoYearPopup;
      inherited;
      Exit;
    end;

    if FBrowsers.FPrevMonth then
    begin
      if FGlyphs.FPrevMonth.Empty then
      begin
        if FGlyphs.FPrevYear.Empty then
        begin
          WidthX1 := 25;
          WidthX2 := 35;
        end
        else
        begin
          WidthX1 := 25;
          WidthX2 := 25 + FGlyphs.FPrevYear.Width;
        end;

        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        if FGlyphs.FPrevYear.Empty then
        begin
          WidthX1 := 25;
          WidthX2 := 25 + FGlyphs.FPrevMonth.Width;
        end
        else
        begin
          WidthX1 := 10 + FGlyphs.FPrevYear.Width;
          WidthX2 := 10 + FGlyphs.FPrevYear.Width + FGlyphs.FPrevMonth.Width;
        end;

        HeightY1 := 0;
        HeightY2 := FGlyphs.FPrevMonth.Height;

      end;

      if not FBrowsers.PrevYear then
      begin
        WidthX1 := WidthX1 - 20;
        WidthX2 := WidthX2 - 20;
      end;

      if (x > WidthX1 - MM) and (x < WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        ChangeMonth(-1);
        flg := True;
      end;
    end;

    if FBrowsers.FPrevYear then
    begin
      if FGlyphs.FPrevYear.Empty then
      begin
        WidthX1 := 5;
        WidthX2 := 15;
        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        WidthX1 := 5;
        WidthX2 := 5 + FGlyphs.FPrevYear.Width;
        HeightY1 := 0;
        HeightY2 := FGlyphs.FPrevYear.Height;
      end;
      if (x > WidthX1 - MM) and (x < WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        ChangeYear(-1);
        flg := True;
      end;
    end;

    if FBrowsers.FNextMonth then
    begin
      if not FGlyphs.FNextMonth.Empty then
      begin
        if FGlyphs.FNextYear.Empty then
        begin
          WidthX1 := 25 + FGlyphs.FNextMonth.Width;
          WidthX2 := 25;
        end
        else
        begin
          WidthX1 := 10 + FGlyphs.FNextYear.Width + FGlyphs.FNextMonth.Width;
          WidthX2 := 10 + FGlyphs.FNextYear.Width;
        end;

        HeightY1 := 0;
        HeightY2 := FGlyphs.FNextMonth.Height;
      end
      else
      begin
        if FGlyphs.FNextYear.Empty then
        begin
          WidthX1 := 30;
          WidthX2 := 25;
        end
        else
        begin
          WidthX1 := 15 + FGlyphs.FNextYear.Width;
          WidthX2 := 10 + FGlyphs.FNextYear.Width;
        end;

        HeightY1 := 0;
        HeightY2 := 15;
      end;

      if not FBrowsers.NextYear then
      begin
        WidthX1 := WidthX1 - 20;
        WidthX2 := WidthX2 - 20;
      end;


      if (x + xoffset > Width - WidthX1 - MM) and (x + xoffset < Width - WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        ChangeMonth(1);
        flg := True;
      end;
    end;

    if FBrowsers.FNextYear then
    begin
      if FGlyphs.FNextYear.Empty then
      begin
        WidthX1 := 15;
        WidthX2 := 5;
        HeightY1 := 0;
        HeightY2 := 15;
      end
      else
      begin
        WidthX1 := FGlyphs.FNextYear.Width + 5;
        WidthX2 := 5;
        HeightY1 := 0;
        HeightY2 := FGlyphs.FNextYear.Height;
      end;

      if (x + xoffset > Width - WidthX1 - MM) and (x + xoffset < Width - WidthX2 + MM) and (y > HeightY1) and (y < HeightY2) then
      begin
        ChangeYear(1);
        flg := True;
      end;
    end;
  end;

  if flg then
  begin
    DiffCheck(origdate, seldate);
    inherited;
    Exit;
  end;

  if ShowScrollColumn then
  begin
    if (X >= (7 * dx + 2)) and (Y >= (FWeekNameY + FDayFontHeight)) then
    begin
      for i := 1 to 6 do
      begin
        if PtInRect(GetUpScrollBtnRect(i), Point(X + Xoffset, Y)) then
        begin
          FItemScrollerAry[i].Position := FItemScrollerAry[i].Position - 1;
          Invalidate;
          Break;
        end;

        if PtInRect(GetDownScrollBtnRect(i), Point(X + Xoffset , Y)) then
        begin
          FItemScrollerAry[i].Position := FItemScrollerAry[i].Position + 1;
          Invalidate;
          Break;
        end;
      end;
      inherited;
      Exit;
    end;
  end;

  if FMultiSelect then
    MouseSel := True;

  movdate := -1;

  inherited;

  SetCapture(Handle);

  if (y > 15) and (y < FWeekNameY + YOffset) and
    (x > 0) and FAllDaySelect and FMultiSelect then
  begin
    ClkDate := XYToDate(x, dy + YOffset, False);
    DateCol.Clear;
    for lidx := 1 to 6 do
      with DateCol.Add do Date := ClkDate + (lidx - 1) * 7;
    Invalidate;
    if Assigned(FOnAllDaySelect) then
      FOnAllDaySelect(Self);
  end;

  if (x < 0) and (y > FWeekNameY + yoffset) and
    FShowWeeks and FWeekSelect and FMultiSelect then
  begin
    ClkDate := XYToDate(XOffset, y, False);
    DateCol.Clear;
    DateCol.AddRange(ClkDate , ClkDate + 6);
    Invalidate;

    if Assigned(FOnWeekSelect) then
      FOnWeekSelect(Self);
  end;

  if (y > (FweekNameY + FDayFontHeight)) and (x > 0) then
  begin
    FMouseXY := Point(X, Y);
    ClkDate := XYToDate(X, Y, false);

    APlannerItem := FPlannerMonthItems.FindItemAtDate(ClkDate, X + Xoffset, Y);

    if APlannerItem = nil then
    begin
      if Assigned(FPlannerMonthItems.Selected) then
      begin
        if Assigned(OnItemUnSelect) then
          OnItemUnSelect(self,FPlannerMonthItems.Selected);
      end;
      FPlannerMonthItems.UnSelectAll;
    end
    else
    begin
      if (APlannerItem = FPlannerMonthItems.Selected) and (APlannerItem.CaptionType <> ctNone) then
      begin
        tr := ItemRectAtRow(APlannerItem, DateToXY(clkDate).Y);
        tr.Bottom := tr.Top + 18;

        if APlannerItem.ShowDeleteButton and (X > tr.Right - 20) then
        begin
          if Assigned(OnItemDelete) then
            OnItemDelete(Self, APlannerItem);

          if Assigned(OnItemDeleted) then
            OnItemDeleted(Self, APlannerItem);

          FreeItem(APlannerItem);
          Exit;
        end;

        if APlannerItem.Attachement <> '' then
        begin
          tr.Left := tr.Right - FPlanner.AttachementGlyph.Width;
          if ShowWeeks then
            x := x + xoffset;

          if PtInRect(tr, Point(X, Y)) then
          begin
            AutoHandle := true;

            if Assigned(FOnItemLeftClick) then
              FOnItemLeftClick(self, APlannerItem);

            if Assigned(FOnItemAttachementClick) then
              FOnItemAttachementCLick(self, APlannerItem, APlannerItem.Attachement, AutoHandle);

            if AutoHandle then
              ShellExecute(0, 'open', PChar(APlannerItem.Attachement), nil, nil, SW_NORMAL);


            Exit;
          end;
          if ShowWeeks then
            x := x - xoffset;

          tr.Right := tr.Right - FPlanner.AttachementGlyph.Width;
        end;


        if APlannerItem.URL <> '' then
        begin
          tr.Left := tr.Right - FPlanner.URLGlyph.Width;
          if ShowWeeks then
            x := x + xoffset;

          if PtInRect(tr, Point(X, Y)) then
          begin
            AutoHandle := true;

            if Assigned(FOnItemLeftClick) then
              FOnItemLeftClick(self, APlannerItem);

            if Assigned(FOnItemURLClick) then
              FOnItemURLCLick(self, APlannerItem, APlannerItem.URL, AutoHandle);

            if AutoHandle then
              ShellExecute(0, 'open', PChar(APlannerItem.URL), nil, nil, SW_NORMAL);

            Exit;
          end;
          if ShowWeeks then
            x := x - xoffset;
        end;
      end;

      if (APlannerItem = FPlannerMonthItems.Selected) and Assigned(APlannerItem) and not (APlannerItem.NotEditable)
         and (not FMouseOverTrack) and not (FMouseOnItemStart or FMouseOnItemEnd) then
      begin
        StartEdit(APlannerItem, clkDate, X, Y);
      end
      else
      begin
        FPlannerMonthItems.FocusItem(APlannerItem);
        FMouseDownMove := not APlannerItem.FixedPos and not (FMouseOnItemStart or FMouseOnItemEnd);
        if not FDirectMove then
          FMouseDownMove := FMouseOverTrack;
        FMouseDownResize := (FMouseOnItemStart or FMouseOnItemEnd);
        FStartMovedate := clkDate;

        FMouseDownStartDate := APlannerItem.ItemRealStartTime;
        FMouseDownEndDate := APlannerItem.ItemRealEndTime;
      end;

      mousesel := false;
      if DateCol.Count > 0 then
        clkDate := DateCol.Items[0].Date;


      Invalidate;

      if Assigned(FOnItemLeftClick) then
        FOnItemLeftClick(Self, APlannerItem);
      Exit;
    end;

    ClkDate := XYToDate(X, Y, FAutoChangeMonth);

    if (GetMonth(ClkDate) > GetMonth(SelDate)) and not FShowDaysAfter then
    begin
      Exit;
    end;

    if (GetMonth(ClkDate) < GetMonth(SelDate)) and not FShowDaysBefore then
    begin
      Exit;
    end;

    if not CheckDateRange(clkdate) then
      Exit;

    lidx := Datecol.IsDateInList(origdate);


    if (ssCtrl in Shift) and FMultiSelect and FDisjunctSelect then
    begin
      lidx := DateCol.IsDateInList(clkdate);
      if lidx = -1 then
      begin
        with DateCol.Add do
          Date := ClkDate;
        SelDate := ClkDate;
      end
      else
      begin
        origdate := DateCol.items[lidx].Date;
        DateCol.items[lidx].Free;
      end;
    end
    else
    begin

      if (lidx <> -1) then
        DateCol.Items[lidx].Free;

      if FAutoChangeMonth then
      begin
        seldate := clkdate;
        thedate := seldate;
        DecodeDate(seldate,FYear, FMonth, FDay);
      end;

      datecol.Clear;
      lidx := datecol.IsDateInList(clkDate); // seldate
      if (lidx = -1) then
      begin
        with DateCol.Add do
          Date := clkdate; // seldate;
      end;
    end;

    DecodeDate(origdate, ye, omo, da);
    DecodeDate(clkdate, ye, nmo, da);

    if (omo = nmo) then
    begin
      r := DateToRect(origdate);
      InvalidateRectangle(r, True);
      r := DatetoRect(thedate);
      InvalidateRectangle(r, True);
    end
    else
      DoPaint;

    if FAutoChangeMonth then
      SetLabel(nmo, ye);

    if AutoChangeMonth then
      DiffCheck(origdate, seldate)
    else
      DiffCheck(origdate, clkdate);

    if not FAutoChangeMonth then
      FOldSelDate:= clkdate;

      Invalidate;
  end;
end;

procedure TPlannerMonthView.DoDaySelect(ADay: TDateTime);
begin
  if Assigned(FOnDaySelect) then
    FOnDaySelect(Self, ADay);
end;


procedure TPlannerMonthView.MouseUp(Button: TMouseButton;
  Shift: TShiftState; X, Y: integer);
var
  dt, dfDate: TDateTime;
  OldStartTime, OldEndTime: TDateTime;
  Allow: Boolean;

begin
  if Button <> mbLeft then
  begin
    inherited;
    Exit;
  end;

  x := x - xoffset;

  if (y > (FweekNameY + FDayFontHeight)) and FMouseDownResize and Assigned(Items.Selected) then
  begin
    dt := XYToDate(X, Y, False);
    if CheckDateRange(dt) then
    begin
      dfDate := dt - FStartMovedate;
      if FMouseOnItemStart then
      begin
        if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime) then
        begin
          //OldStartTime := Items.Selected.ItemStartTime;
          //OldEndTime := Items.Selected.ItemEndTime;
          OldStartTime := FMouseDownStartDate;
          OldEndTime := FMouseDownEndDate;

          ItemSized(Items.Selected, OldStartTime, OldEndTime, Items.Selected.ItemStartTime, Items.Selected.ItemEndTime);
          Invalidate;
        end;
      end
      else
      begin
        if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime, Items.Selected.ItemEndTime + dfDate) then
        begin
          //OldStartTime := Items.Selected.ItemStartTime;
          //OldEndTime := Items.Selected.ItemEndTime;
          OldStartTime := FMouseDownStartDate;
          OldEndTime := FMouseDownEndDate;

          {
          Items.Selected.ItemEndTime := Items.Selected.ItemEndTime + dfDate;
          Items.Selected.ItemRealEndTime := Int(Items.Selected.ItemEndTime) + Frac(Items.Selected.ItemRealEndTime);
          }
          ItemSized(Items.Selected, OldStartTime, OldEndTime, Items.Selected.ItemStartTime, Items.Selected.ItemEndTime);
          {
          FStartMovedate := dt;
          }
          Invalidate;
        end;
      end;
    end;
  end;

  if (y > (FweekNameY + FDayFontHeight)) and FMouseDownMove and Assigned(Items.Selected) then
  begin
    dt := XYToDate(X, Y, False);
    if CheckDateRange(dt) then
    begin
      if (ssCtrl in Shift) (*or (GetKeystate(VK_MENU) and $8000 = $8000) *)or DirectDrag then
      begin
        if DragItem and (Cursor <> crSizeAll) and (Cursor <> crSizeNS) and (Cursor <> crSizeWE) then
        begin
          Allow := True;

          if Assigned(FPlanner.OnItemDrag) then
            FPlanner.OnItemDrag(FPlanner, FPlanner.Items.Selected, Allow);

          if Allow then
          begin
            inherited;
            BeginDrag(True, -1);
            FMouseDownMove := False;
            Exit;
          end;
        end;
      end;

      dfDate := dt - FStartMovedate;


      if not Items.HasMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime + dfDate) then
      begin
        Allow := true;
        TMonthPlannerItem(Items.Selected).MoveMonthPlannerItem(Items.Selected.ItemStartTime + dfDate, Items.Selected.ItemEndTime + dfDate, true, Allow);

        if Allow then
        begin
          FStartMovedate := dt;


          Invalidate;
        end;
      end;
    end;
  end;

  FMouseDownResize := false;
  FMouseDownMove := False;
  mousesel := False;



  ReleaseCapture;

  inherited;

//  x := x - xoffset;

  if (Abs(x - xposin) < 4) and (Abs(y - yposin) < 4) and (clkDate <> 0) then
  begin
    DoDaySelect(clkDate);
  end;
end;

procedure TPlannerMonthView.Paint;
begin
  inherited Paint;
  PaintCalendar(Canvas, GetClientRect, False);
end;

procedure TPlannerMonthView.PaintCalendar(ACanvas: TCanvas; PaintRect: TRect; Print: Boolean);
var
  r, gr: TRect;
{$IFDEF USEIMAGE}
  xo, yo: integer;
  hrgn: THandle;
{$ENDIF}
  direction, MonthDirection: Boolean;
  bmp: TBitmap;
  clr: TColor;
  Theme: THandle;
  wfh: Integer;

  function Max(a, b: integer): integer;
  begin
    if a > b then
      Result := a
    else
      Result := b;
  end;

begin
  Caption := '';
  FCanvas := ACanvas;

  if FUpdateCount > 0 then
    Exit;

  r := PaintRect;

  if FUseTheme and FBorderXP then
  begin
    theme := OpenThemeData(Handle, 'Edit');
    bmp := TBitmap.Create;
    try
      bmp.Width := 12;
      bmp.Height := 12;
      r := rect(0, 0, 10, 10);
      DrawThemeBackground(theme, bmp.Canvas.handle, 1, 1, @r, nil);
      clr := bmp.Canvas.Pixels[0, 0];
      CloseThemeData(theme);
    finally
      bmp.free;
    end;
    r := PaintRect;
    FCanvas.Pen.Color := clr;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);
  end;

  r := PaintRect;
  InflateRect(r, -BorderWidth, -BorderWidth);
  if (BevelInner <> bvNone) or (BevelOuter <> bvNone) then
    InflateRect(r, -BevelWidth, -BevelWidth);

  FCanvas.Brush.Color := Color;
  FCanvas.Pen.Color := Color;
  FCanvas.Rectangle(r.Left, r.Top, r.Right, r.Bottom);

{$IFDEF USEIMAGE}
  if Assigned(FImage) then
  begin
    if not FImage.Empty and not Print then
    begin
      case FBackgroundPosition of
        bpTopLeft: FCanvas.Draw(r.Left, r.Top, FImage);
        bpTopRight: FCanvas.Draw(Max(r.Left, Width - FImage.Width - BevelWidth), r.Top, FImage);
        bpBottomLeft: FCanvas.Draw(r.Left, Max(r.top, Height - FImage.Height - BevelWidth),
            fImage);
        bpBottomRight: FCanvas.Draw(Max(r.Left, Width - FImage.Width - BevelWidth),
            Max(r.top, Height - fImage.Height - BevelWidth), fImage);
        bpCenter: FCanvas.Draw(Max(r.Left, Width - FImage.Width - BevelWidth) shr 1,
            Max(r.top, Height - fImage.Height - BevelWidth) shr 1, fImage);
        bpTiled:
          begin
            hrgn := CreateRectRgn(r.Left, r.Top, r.Right, r.Bottom);
            SelectClipRgn(FCanvas.Handle, hrgn);

            yo := r.Top;
            while yo < r.Bottom do
            begin
              xo := r.Left;
              while xo < r.Right do
              begin
                FCanvas.Draw(xo, yo, FImage);
                xo := xo + FImage.Width;
              end;
              yo := yo + FImage.Height;
            end;

            SelectClipRgn(FCanvas.Handle, 0);
            DeleteObject(hrgn);
          end;
        bpStretched: FCanvas.StretchDraw(r, FImage);
      end;
    end
    else
    begin
{$ENDIF}
      FCanvas.Font.Assign(DayFont);
      wfh := FCanvas.TextHeight('gh');

      if not (FMonthGradientStartColor = clNone) and not (FMonthGradientEndColor = clNone) then
      begin
        if (FMonthGradientDirection = gdHorizontal) then
          MonthDirection := false
        else
          MonthDirection := true;

        gr := r;

        gr.Bottom := (PaintRect.Top + FweekNameY + wfh) + 1;
        DrawGradient(FCanvas, FMonthGradientStartColor, FMonthGradientEndColor, 80, gr, MonthDirection);
      end;

      if not (FGradientStartColor = clNone) and not (FGradientEndColor = clNone) then
      begin
        if (FGradientDirection = gdHorizontal) then
          direction := false
        else
          direction := true;

        gr := r;

        if (FMonthGradientStartColor <> clNone) then
          //gr.Top := (Height + NumRows) div NumRows + wfh;
          gr.Top := (PaintRect.Top + FweekNameY + wfh);
        DrawGradient(FCanvas, FGradientStartColor, FGradientEndColor, 80, gr, Direction);
      end;
{$IFDEF USEIMAGE}
    end;
  end;
{$ENDIF}


  if Print then
  begin
    FCanvas.Brush.Color := clNone;
    FCanvas.Brush.Style := bsClear;
    FCanvas.Pen.Color := LineColor;
    FCanvas.Rectangle(PaintRect.Left, PaintRect.Top, PaintRect.Right, PaintRect.Bottom);
  end;

  PaintProc(PaintRect, Print);
end;

procedure TPlannerMonthView.KeyPress(var Key: char);
begin
  if (key = #27) then seldate := initdate;
  inherited;
end;

procedure TPlannerMonthView.CMMouseLeave(var Message: TMessage);
var
  r: trect;
begin
  inherited;
  if flgl or flgr or flgla or dflgl or dflgr or flgt then
  begin
    flgl := False;
    flgr := False;
    flgla := False;
    dflgl := False;
    dflgr := False;
    flgt := False;
    r := GetClientRect;
    r.bottom := (r.bottom - r.top) div 7;
    InvalidateRectangle(r, True);
  end;
end;

procedure TPlannerMonthView.CMWantSpecialKey(var Msg: TCMWantSpecialKey);
begin
  inherited;
  if msg.CharCode in [vk_up, vk_down, vk_left, vk_right] then
    msg.Result := 1;
end;

procedure TPlannerMonthView.CompletionChanged(Sender: TObject);
begin
  FPlanner.Footer.Completion.Assign(FCompletion);
  Invalidate;
end;

procedure TPlannerMonthView.WMEraseBkGnd(var Message: TWMEraseBkGnd);
begin
  Message.Result := 1;
end;


procedure TPlannerMonthView.WMNotify(var Message: TWMNOTIFY);
var
  buffer: array[0..255] of char;
  pt: TPoint;
  plIt: TPlannerItem;
  di: PNMTTDispInfo;
  ATitle, AText: string;
  AIcon: Integer;
  len1, len2, i: integer;
  CDate: TDateTime;

begin
  with Message.NMHdr^ do
    case code of
    TTN_NEEDTEXT:
    begin
      if not Balloon.Enable then
        Exit;

      di := PNMTTDispInfo(TMessage(Message).LParam);

      GetCursorPos(pt);
      pt := ScreenToClient(pt);

      if ShowWeeks then
      begin
        CDate := XYToDate(pt.X - WeekWidth, pt.Y, false);
        plIt := FPlannerMonthItems.FindItemAtDate(CDate, pt.X, pt.Y);
      end
      else
      begin
        CDate := XYToDate(pt.X, pt.Y, false);
        plIt := FPlannerMonthItems.FindItemAtDate(CDate, pt.X, pt.Y);
      end;

      if Assigned(plIt) then
      begin
        ATitle := HTMLStrip(plIt.CaptionText);

        if IsRtf(plIt.NotesText) then
        begin
          TextToRich(plIt.NotesText);
          AText := FRichEdit.Text;
        end
        else
          AText := HTMLStrip(plIt.NotesText);

        len1 := Length(AText);
        len2 := SizeOf(FTooltipBuffer) div 4;

        // balloon tooltip cannot handle tabs well
        i := 1;
        while i < len1 do
        begin
          if AText[i] = #9 then
            AText[i] := #32;
          inc(i);
        end;

        i := 1;
        if AText <> '' then
          while ( (AText[i] = #13) or (AText[i] = #10) or (AText[i] = #9)) do
            inc(i);

        if len1 > len2 then
          AText := Copy(AText, i, len2 - 3) + '...'
        else
          if i > 1 then
            AText := Copy(AText, i, len1 - i + 1);

        AIcon := 1;
        // for some reason, balloon tips will not show multiline when there is no title  ...
        if ATitle = '' then
          ATitle := ' ';

        if Assigned(OnItemBalloon) then
          OnItemBalloon(FPlanner, plIt, ATitle, AText, AIcon);
      end
      else
      begin
        AText := '';

        AIcon := 1;
        if Assigned(OnDateBalloon) then
          OnDateBalloon(FPlanner,CDate, ATitle, AText, AIcon);
      end;

      strpcopy(ftooltipbuffer,AText);

      if length(ATitle) > 100 then
        ATitle := copy(ATitle,1, 97) + '...';

      if ATitle = '' then
        ATitle := ' ';

      strpcopy(buffer,ATitle);

      di^.lpszText := @ftooltipbuffer;

      if Balloon.TextColor <> clNone then
        SendMessage(fhtooltip, TTM_SETTIPTEXTCOLOR, ColorToRgb(Balloon.TextColor), 0);
      if Balloon.BackgroundColor <> clNone then
        SendMessage(fhtooltip, TTM_SETTIPBKCOLOR, ColorToRgb(Balloon.BackgroundColor), 0);

      SendMessage(fhtooltip,TTM_SETTITLE,AIcon,LParam(@buffer));
    end;
    TTN_SHOW:
    begin
    end;
    TTN_POP:
    begin
    end;
    end;
end;

procedure TPlannerMonthView.KeyDown(var Key: Word; Shift: TShiftState);
var
  da, nmo, omo, ye: word;
  origdate, OldClkDate: tdatetime;
  dt: tdatetime;
  pt: tpoint;
  r: trect;
  lidx: integer;
  NewItm: TPlannerItem;

begin
  inherited;

  origdate := SelDate;
  OldClkDate:= clkdate;
  pt := DateToXY(seldate);

  if FAutoChangeMonth then
  begin
    DecodeDate(thedate, ye, omo, da);
    case Key of
      vk_left: dt := thedate - 1;
      vk_right: dt := thedate + 1;
      vk_up: dt := thedate - 7;
      vk_down: dt := thedate + 7;
    else
      dt := thedate;
    end;
  end
  else
  begin
    DecodeDate(clkdate, ye, omo, da);
    case Key of
      vk_left: dt := clkdate - 1;
      vk_right: dt := clkdate + 1;
      vk_up: dt := clkdate - 7;
      vk_down: dt := clkdate + 7;
    else
      dt := clkdate;
    end;
  end;


  if (GetMonth(dt) > GetMonth(SelDate)) and not FShowDaysAfter then
  begin
    if Assigned(FOnCancelledChange) then
      FOnCancelledChange(Self, dt);
    Exit;
  end;

  if (GetMonth(dt) < GetMonth(SelDate)) and not FShowDaysBefore then
  begin
    if Assigned(FOnCancelledChange) then
      FOnCancelledChange(Self, dt);
    Exit;
  end;

  if CheckDateRange(dt) then
  begin
    if FAutoChangeMonth then
    begin
      clkdate := dt;
      thedate := dt
    end
    else
    begin
      clkdate:= dt;
      Invalidate;
    end;
  end
  else
    Exit;

  if (Key = VK_SPACE) or
    ((Key = VK_RETURN) and (FReturnIsSelect)) then
  begin
    lidx := datecol.IsDateInList(thedate);
    if (lidx = -1) then
    begin
      if not FDisjunctSelect then DateCol.Clear;
      with DateCol.Add do
        Date := clkdate;
    end
    else
      datecol.Items[lidx].Free;

    Invalidate;

    SelDate := thedate;

    DoDaySelect(theDate);
  end;

  if Key in [vk_up, vk_down, vk_left, vk_right] then
  begin
    if FAutoChangeMonth then
    begin
      Seldate := thedate;
      Decodedate(thedate, ye, nmo, da);
    end
    else
    begin
      Seldate:= clkdate;
      Decodedate(thedate, ye, nmo, da);
    end;
    SetLabel(nmo, ye);

    if (ssShift in Shift) and (FMultiSelect) then
    begin
      if movdate = -1 then
      begin
        if FAutoChangeMonth then
        begin
          if not FDisjunctSelect then
          begin
            DateCol.Clear;
            Invalidate;
          end;
          clkdate := origdate;
          clkdate := oldclkdate;
          DateCol.AddRange(clkdate, seldate);
          MovDate := SelDate;
          movdate := -1;
          FOldSelDate := origdate;
        end
        else
        begin
          if not FDisjunctSelect then
          begin
            DateCol.Clear;
            Invalidate;
          end;  
          DateCol.AddRange(OldClkDate, seldate);
          MovDate := SelDate;
          FOldSelDate := OldClkDate;
        end;
      end
      else
      begin
        datecol.StartUpdate;
        if FAutoChangeMonth then
        begin
          datecol.DelRange(clkdate, origdate);
          datecol.AddRange(clkdate, seldate);
        end
        else
        begin
          datecol.DelRange(FOldSelDate, origdate);
          datecol.AddRange(FOldSelDate, seldate);
        end;
        datecol.StopUpdate;
      end;
    end
    else
    begin
      movdate := -1;
    end;


    if omo = nmo then
    begin
      r := DateToRect(origdate);
      InvalidateRectangle(r, True);
      if FAutoChangeMonth then
        r := DateToRect(thedate)
      else
        r := DateToRect(clkdate);
      InvalidateRectangle(r, True);
    end
    else
      Dopaint;
  end;

  if Key = VK_PRIOR then
  begin
    Self.Changemonth(-1);
  end;
  if Key = VK_NEXT then
  begin
    Self.Changemonth(+1);
  end;

  if Key in [vk_right, vk_up, vk_down, vk_left, vk_prior, vk_next] then
  begin
    if Assigned(FOnDateChange) then
      FOnDateChange(self, origdate, seldate);
  end;

  DiffCheck(origdate, seldate);

  if AutoInsDel then
  begin
    if (Key = VK_INSERT) then
    begin
      NewItm := CreateItemAtSelection;
      if Assigned(FOnItemCreated) then
        FOnItemCreated(self, NewItm);
    end;

    if (Key = VK_DELETE) then
    begin
      if Assigned(Items.Selected) then
      begin
        if Assigned(FOnItemDeleted) then
          FOnItemDeleted(self, Items.Selected);
        FreeItem(Items.Selected);
      end;
    end;
  end;

  if Key in [vk_up, vk_left, vk_right, vk_down, vk_next, vk_prior] then
    Key := 0;
end;

procedure TPlannerMonthView.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TPlannerMonthView.EndUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
    begin
      Items.SetConflicts;
      Invalidate;
    end;
  end;
end;

procedure TPlannerMonthView.ResetUpdate;
begin
  FUpdateCount := 0;
end;

procedure TPlannerMonthView.UpdateItem(APlannerItem: TPlannerItem);
begin
end;

procedure TPlannerMonthView.FreeItem(APlannerItem: TPlannerItem);
var
  i: integer;
  dbkey: string;
begin
  if (APlannerItem.Recurrent) and (APlannerItem.DBKey <> '') then
  begin
    dbkey := APlannerItem.DBKey;

    for i := Items.Count - 1 downto 0 do
    begin
      if (Items[i].DBKey = dbkey) and (Items[i] <> APlannerItem) then
        Items[i].Free;
    end;
  end;
  
  if Items.Selected = APlannerItem then
    Items.Selected := nil;
  APlannerItem.ParentItem.Free;
  Invalidate;
end;

function TPlannerMonthView.CreateItem: TPlannerItem;
begin
  Result := Items.Add;
  Result.Assign(DefaultItem);
end;

function TPlannerMonthView.CreateItemAtSelection: TPlannerItem;
begin
  Result := Items.Add;
  Result.Assign(DefaultItem);
  Result.ItemStartTime := DateCol.Items[0].Date;
  Result.ItemEndTime := DateCol.Items[DateCol.Count - 1].Date;
  Invalidate;
end;

procedure TPlannerMonthView.DrawGradient(Canvas: TCanvas; FromColor, ToColor: TColor; Steps: Integer; r: TRect; Direction: Boolean);
var
  diffr, startr, endr: Integer;
  diffg, startg, endg: Integer;
  diffb, startb, endb: Integer;
  iend: Integer;
  rstepr, rstepg, rstepb, rstepw: Real;
  i, stepw: Word;

begin
  if (ToColor = clNone) or (FromColor = ToColor) then
  begin
    Canvas.Pen.Color := FromColor;
    Canvas.Brush.Color := FromColor;
    Canvas.Rectangle(R.Left, R.Top, R.Right, R.Bottom);
    Exit;
  end;

  if Steps = 0 then
    Steps := 1;


  FromColor := ColorToRGB(FromColor);
  ToColor := ColorToRGB(ToColor);

  startr := (FromColor and $0000FF);
  startg := (FromColor and $00FF00) shr 8;
  startb := (FromColor and $FF0000) shr 16;
  endr := (ToColor and $0000FF);
  endg := (ToColor and $00FF00) shr 8;
  endb := (ToColor and $FF0000) shr 16;

  diffr := endr - startr;
  diffg := endg - startg;
  diffb := endb - startb;

  rstepr := diffr / steps;
  rstepg := diffg / steps;
  rstepb := diffb / steps;

  if Direction then
    rstepw := (R.Right - R.Left) / Steps
  else
    rstepw := (R.Bottom - R.Top) / Steps;

  with Canvas do
  begin
    for i := 0 to Steps - 1 do
    begin
      endr := startr + Round(rstepr * i);
      endg := startg + Round(rstepg * i);
      endb := startb + Round(rstepb * i);
      stepw := Round(i * rstepw);
      Pen.Color := endr + (endg shl 8) + (endb shl 16);
      Brush.Color := Pen.Color;
      if Direction then
      begin
        iend := R.Left + stepw + Trunc(rstepw) + 1;
        if iend > R.Right then
          iend := R.Right;
        Rectangle(R.Left + stepw, R.Top, iend, R.Bottom)
      end
      else
      begin
        iend := R.Top + stepw + Trunc(rstepw) + 1;
        if iend > r.Bottom then
          iend := r.Bottom;
        Rectangle(R.Left, R.Top + stepw, R.Right, iend);
      end;
    end;
  end;
end;


procedure TPlannerMonthView.Notification(AComponent: TComponent;
  AOperation: TOperation);
var
  i: Integer;
begin
  inherited;

  if csDestroying in ComponentState then
    Exit;

  if (AOperation = opRemove) and (AComponent = FPlannerImages) then
    FPlannerImages := nil;

  if (AOperation = opRemove) and (AComponent = FContainer) then
    FContainer := nil;

  if (AOperation = opRemove) and (AComponent = FItemPopup) then
    FItemPopup := nil;

{  if (AOperation = opRemove) and
     (AComponent = FPlanChecker) then
    FPlanChecker := nil;
}

  if (AOperation = opRemove) and Assigned(DefaultItem) then
  begin
    if DefaultItem.Alarm.Handler = AComponent then
      DefaultItem.Alarm.Handler := nil;
    if DefaultItem.Editor = AComponent then
      DefaultItem.Editor := nil;
    if DefaultItem.PopupMenu = AComponent then
      DefaultItem.PopupMenu := nil;
    if DefaultItem.DrawTool = AComponent then
      DefaultItem.DrawTool := nil;
  end;

  if (AOperation = opRemove) and Assigned(Items) then
  begin
    for i := 1 to Items.Count do
    begin
      if Items[i - 1].Alarm.Handler = AComponent then
        Items[i - 1].Alarm.Handler := nil;

      if Items[i - 1].Editor = AComponent then
        Items[i - 1].Editor := nil;

      if Items[i - 1].PopupMenu = AComponent then
        Items[i - 1].PopupMenu := nil;

      if Items[i - 1].DrawTool = AComponent then
        Items[i - 1].DrawTool := nil;
    end;
  end;
end;

procedure TPlannerMonthView.ItemSelected(Item: TPlannerItem);
begin
  if Assigned(OnItemSelect) then
    OnItemSelect(Self, Item);
end;

procedure TPlannerMonthView.ItemUnSelected(Item: TPlannerItem);
begin
  if Assigned(OnItemUnSelect) then
    OnItemUnSelect(Self, Item);
end;

procedure TPlannerMonthView.ItemUpdated(Sender: TObject);
begin
  if (csDestroying in ComponentState) then
    Exit;
  if Assigned(OnItemUpdate) then
    OnItemUpdate(Sender);
end;

procedure TPlannerMonthView.ItemEdited(Sender: TObject; Item: TPlannerItem);
begin
  PostMessage(self.handle, WM_LBUTTONUP, 0, 0);
  FIsEditing := false;
  if Assigned(FOnItemEndEdit) then
  begin
    FOnItemEndEdit(Self, Item);
  end;
end;

procedure TPlannerMonthView.ItemMoved(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime);
begin
  if Assigned(OnItemMove) then
    OnItemMove(Self, APlannerItem, FromStartDate, FromEndDate, ToStartDate, ToEndDate);
end;

procedure TPlannerMonthView.ItemMoving(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime; var Allow: Boolean);
begin
  if Assigned(OnItemMoving) then
    OnItemMoving(Self, APlannerItem, FromStartDate, FromEndDate, ToStartDate, ToEndDate, Allow);
end;

procedure TPlannerMonthView.ItemSized(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime);
begin
  if Assigned(OnItemSize) then
    OnItemSize(Self, APlannerItem, FromStartDate, FromEndDate, ToStartDate, ToEndDate);
end;

procedure TPlannerMonthView.ItemSizing(APlannerItem: TPlannerItem; FromStartDate, FromEndDate,
  ToStartDate, ToEndDate: TDateTime; var Allow: Boolean);
begin
  if Assigned(OnItemSizing) then
    OnItemSizing(Self, APlannerItem, FromStartDate, FromEndDate, ToStartDate, ToEndDate, Allow);
end;

function TPlannerMonthView.CreateItems(AOwner: TCustomPlanner): TPlannerMonthViewItems;
begin
  Result := TPlannerMonthViewItems.Create(AOwner);
end;

procedure TPlannerMonthView.SetTrackBump(const Value: Boolean);
begin
  FTrackBump := Value;
  FPlanner.TrackBump := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetTrackOnly(const Value: Boolean);
begin
  if FTrackOnly <> Value then
  begin
    FTrackOnly := Value;
    FPlanner.TrackOnly := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetTrackProportional(const Value: Boolean);
begin
  if FTrackProportional <> Value then
  begin
    FTrackProportional := Value;
    FPlanner.TrackProportional := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetTrackWidth(const Value: Integer);
begin
  if FTrackWidth <> Value then
  begin
    FTrackWidth := Value;
    FPlanner.TrackWidth := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetURLColor(const Value: TColor);
begin
  if FURLColor <> Value then
  begin
    FURLColor := Value;
    FPlanner.URLColor := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetURLGlyph(const Value: TBitmap);
begin
  FURLGlyph.Assign(Value);
  //FPlanner.URLGlyph.Assign(Value);
  //Invalidate;
end;

procedure TPlannerMonthView.SetDeleteGlyph(const Value: TBitmap);
begin
  FDeleteGlyph.Assign(Value);
  FPlanner.DeleteGlyph.Assign(Value);
  Invalidate;
end;

procedure TPlannerMonthView.SetDragItemImage(const Value: boolean);
begin
  if (FDragItemImage <> Value) then
  begin
    FDragItemImage := Value;
    if Value then
      FixControlStyles(GetParentForm(Self));
  end;
end;

procedure TPlannerMonthView.SetShadowColor(const Value: TColor);
begin
  if FShadowColor <> Value then
  begin
    FShadowColor := Value;
    if Assigned(FPlanner) then
      FPlanner.ShadowColor := Value;
    Invalidate;
  end;
end;


procedure TPlannerMonthView.SetFlashColor(const Value: TColor);
begin
  if FFlashColor <> Value then
  begin
    FFlashColor := Value;
    FPlanner.FlashColor := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetFlashFontColor(const Value: TColor);
begin
  if FFlashFontColor <> Value then
  begin
    FFlashFontColor := Value;
    FPlanner.FlashFontColor := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetImages(const Value: TImageList);
begin
  if FPlannerImages <> Value then
  begin
    FPlannerImages := Value;
    if Assigned(FPlanner) then
      FPlanner.PlannerImages := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetAttachementGlyph(const Value: TBitmap);
begin
  FAttachementGlyph.Assign(Value);
  //Invalidate;
end;

procedure TPlannerMonthView.SetColorCurrent(const Value: TColor);
begin
  FColorCurrent := Value;
  FPlanner.Display.ColorCurrent := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetColorCurrentItem(const Value: TColor);
begin
  FColorCurrentItem := Value;
  FPlanner.Display.ColorCurrentItem := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetItemSpace(const Value: Integer);
begin
  FItemSpace := Value;
  Invalidate;
end;


{ TNameOfDays }

procedure TNameOfDays.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TNameOfDays.Create;
begin
  inherited Create;
  FUseIntlNames := True;
  InitIntl;
end;

destructor TNameOfDays.Destroy;
begin
  inherited Destroy;
end;

function TNameOfDays.GetDay(i: integer): string;
begin
  case i of
    1, 8: Result := FMonday;
    2, 9: Result := FTuesday;
    3, 10: Result := FWednesday;
    4, 11: Result := FThursday;
    5, 12: Result := FFriday;
    6, 13: Result := FSaturday;
    7, 14: Result := FSunday;
  else
    Result := '';
  end;
end;

procedure TNameOfDays.InitIntl;
begin
  {$IFDEF DELPHIXE_LVL}
  FSunday := FormatSettings.ShortDayNames[1];
  FMonday := FormatSettings.ShortDayNames[2];
  FTuesday := FormatSettings.ShortDayNames[3];
  FWednesday := FormatSettings.ShortDayNames[4];
  FThursday := FormatSettings.ShortDayNames[5];
  FFriday := FormatSettings.ShortDayNames[6];
  FSaturday := FormatSettings.ShortDayNames[7];
  {$ENDIF}

  {$IFNDEF DELPHIXE_LVL}
  FSunday := ShortDayNames[1];
  FMonday := ShortDayNames[2];
  FTuesday := ShortDayNames[3];
  FWednesday := ShortDayNames[4];
  FThursday := ShortDayNames[5];
  FFriday := ShortDayNames[6];
  FSaturday := ShortDayNames[7];
  {$ENDIF}
  Changed;
end;

procedure TNameOfDays.SetFriday(const Value: TDayStr);
begin
  FFriday := Value;
  Changed;
end;

procedure TNameOfDays.SetMonday(const Value: TDayStr);
begin
  FMonday := Value;
  Changed;
end;

procedure TNameOfDays.SetSaturday(const Value: TDayStr);
begin
  FSaturday := Value;
  Changed;
end;

procedure TNameOfDays.SetSunday(const Value: TDayStr);
begin
  FSunday := Value;
  Changed;
end;

procedure TNameOfDays.SetThursday(const Value: TDayStr);
begin
  FThursday := Value;
  Changed;
end;

procedure TNameOfDays.SetTuesday(const Value: TDayStr);
begin
  FTuesday := Value;
  Changed;
end;

procedure TNameOfDays.SetUseIntlNames(const Value: Boolean);
begin
  FUseIntlNames := Value;
  if FUseIntlNames then InitIntl;
end;

procedure TNameOfDays.SetWednesday(const Value: TDayStr);
begin
  FWednesday := Value;
  Changed;
end;

{ TNameOfMonths }

constructor TNameofMonths.Create;
begin
  inherited Create;
  FUseIntlNames := True;
  InitIntl;
end;

destructor TNameOfMonths.Destroy;
begin
  inherited Destroy;
end;

procedure TNameOfMonths.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TNameOfMonths.InitIntl;
begin
  {$IFDEF DELPHIXE_LVL}
  FJanuary := FormatSettings.ShortMonthNames[1];
  FFebruary := FormatSettings.ShortMonthNames[2];
  FMarch := FormatSettings.ShortMonthNames[3];
  FApril := FormatSettings.ShortMonthNames[4];
  FMay := FormatSettings.ShortMonthNames[5];
  FJune := FormatSettings.ShortMonthNames[6];
  FJuly := FormatSettings.ShortMonthNames[7];
  FAugust := FormatSettings.ShortMonthNames[8];
  FSeptember := FormatSettings.ShortMonthNames[9];
  FOctober := FormatSettings.ShortMonthNames[10];
  FNovember := FormatSettings.ShortMonthNames[11];
  FDecember := FormatSettings.ShortMonthNames[12];
  {$ENDIF}
  {$IFNDEF DELPHIXE_LVL}
  FJanuary := ShortMonthNames[1];
  FFebruary := ShortMonthNames[2];
  FMarch := ShortMonthNames[3];
  FApril := ShortMonthNames[4];
  FMay := ShortMonthNames[5];
  FJune := ShortMonthNames[6];
  FJuly := ShortMonthNames[7];
  FAugust := ShortMonthNames[8];
  FSeptember := ShortMonthNames[9];
  FOctober := ShortMonthNames[10];
  FNovember := ShortMonthNames[11];
  FDecember := ShortMonthNames[12];
  {$ENDIF}
  Changed;
end;

procedure TNameOfMonths.SetUseIntlNames(const Value: Boolean);
begin
  FUseIntlNames := Value;
  if FUseIntlNames then InitIntl;
end;

function TNameOfMonths.GetMonth(i: integer): string;
begin
  case i of
    1: Result := FJanuary;
    2: Result := FFebruary;
    3: Result := FMarch;
    4: Result := FApril;
    5: Result := FMay;
    6: Result := FJune;
    7: Result := FJuly;
    8: Result := FAugust;
    9: Result := FSeptember;
    10: Result := FOctober;
    11: Result := FNovember;
    12: Result := FDecember;
  else
    Result := '';
  end;
end;

procedure TNameofMonths.SetApril(const Value: TMonthStr);
begin
  FApril := Value;
  Changed;
end;

procedure TNameofMonths.SetAugust(const Value: TMonthStr);
begin
  FAugust := Value;
  Changed;
end;

procedure TNameofMonths.SetDecember(const Value: TMonthStr);
begin
  FDecember := Value;
  Changed;
end;

procedure TNameofMonths.SetFebruary(const Value: TMonthStr);
begin
  FFebruary := Value;
  Changed;
end;

procedure TNameofMonths.SetJanuary(const Value: TMonthStr);
begin
  FJanuary := Value;
  Changed;
end;

procedure TNameofMonths.SetJuly(const Value: TMonthStr);
begin
  FJuly := Value;
  Changed;
end;

procedure TNameofMonths.SetJune(const Value: TMonthStr);
begin
  FJune := Value;
  Changed;
end;

procedure TNameofMonths.SetMarch(const Value: TMonthStr);
begin
  FMarch := Value;
  Changed;
end;

procedure TNameofMonths.SetMay(const Value: TMonthStr);
begin
  FMay := Value;
  Changed;
end;

procedure TNameofMonths.SetNovember(const Value: TMonthStr);
begin
  FNovember := Value;
  Changed;
end;

procedure TNameofMonths.SetOctober(const Value: TMonthStr);
begin
  FOctober := Value;
  Changed;
end;

procedure TNameofMonths.SetSeptember(const Value: TMonthStr);
begin
  FSeptember := Value;
  Changed;
end;

procedure TYearStartAt.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TYearStartAt.Create(AOwner: TCustomMonthViewPanel);
begin
  inherited Create;
  FStartDay := 1;
  FStartMonth := 1;
  FOwner := AOwner;
  NextYearStartDay := 1;
  NextYearStartMonth := 1;
  PrevYearStartDay := 1;
  PrevYearStartMonth := 1;
end;

destructor TYearStartAt.Destroy;
begin
  inherited Destroy;
end;

procedure TYearStartAt.SetISOWeekNumber(const Value: Boolean);
begin
  FISOWeekNumber := Value;
  FOwner.UpdateYearStart;
  Changed;
end;

procedure TYearStartAt.SetNextYearStartDay(d: integer);
begin
  if not ValidateDay(d) then
    Exit;
  FNextYearStartDay := d;
  FOwner.UpdateYearStart;
end;

procedure TYearStartAt.SetNextYearStartMonth(m: integer);
begin
  if not ValidateMonth(m) then
    Exit;
  FNextYearStartMonth := m;
  FOwner.UpdateYearStart;
end;

procedure TYearStartAt.SetPrevYearStartDay(d: integer);
begin
  if not ValidateDay(d) then
    Exit;
  FPrevYearStartDay := d;
  FOwner.UpdateYearStart;
end;

procedure TYearStartAt.SetPrevYearStartMonth(m: integer);
begin
  if not ValidateMonth(m) then
    Exit;
  FPrevYearStartMonth := m;
  FOwner.UpdateYearStart;
end;

procedure TYearStartAt.SetStartDay(d: integer);
begin
  if not ValidateDay(d) then
    Exit;
  FStartDay := d;
  FOwner.UpdateYearStart;
end;

procedure TYearStartAt.SetStartMonth(m: integer);
begin
  if not ValidateMonth(m) then
    Exit;
  FStartMonth := m;
  FOwner.UpdateYearStart;
end;

function TYearStartAt.ValidateDay(d: integer): Boolean;
begin
  Result := True;
  if (d <= 0) or (d > 31) then
  begin
    Messagedlg('Invalid day. Should be in [1..31]', mtError, [mbOK], 0);
    Result := False;
  end;
end;

function TYearStartAt.ValidateMonth(m: integer): Boolean;
begin
  Result := True;
  if (m <= 0) or (m > 12) then
  begin
    MessageDlg('Invalid month. Should be in [1..12]', mtError, [mbOK], 0);
    Result := False;
  end;
end;

function TPlannerMonthView.GetDateProc: TDatetime;
begin
  Result := SelDate;
end;

procedure TPlannerMonthView.SetDateProc(const Value: TDatetime);
begin
  if AutoChangeMonth or (Value < FirstDate) or (Value > LastDate) then
  begin
    DecodeDate(Value, FYear, FMonth, FDay);
    SetDate(FDay, FMonth, FYear);
    ChangeMonth(0);
  end
  else
  begin
    ClkDate := value;
    SelDate := Value;
    DateCol.Clear;
    DateCol.Add.Date := seldate;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetDateCol(const Value: TSelDateItems);
begin
  DateCol.Assign(Value);
  DateCol.DoPaint;
end;

function TPlannerMonthView.GetDateCol: TSelDateItems;
var
  d: tdatetime;
  i: integer;
  sorted: Boolean;
begin
  //sort the list
  repeat
    sorted := True;
    for i := 2 to datecol.Count do
    begin
      if (datecol.items[i - 2].Date > datecol.items[i - 1].Date) then
      begin
        d := datecol.items[i - 2].date;
        datecol.items[i - 2].date := datecol.items[i - 1].date;
        datecol.items[i - 1].date := d;
        sorted := False;
      end;
    end;
  until sorted;

  Result := Datecol;
end;

{$IFDEF TMSSKINS}
procedure TPlannerMonthView.SkinChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
    FPlanner.Skin.Assign(Skin);
end;
{$ENDIF}

function TPlannerMonthView.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER, REL_VER), MakeWord(MIN_VER, MAJ_VER));
end;

function TPlannerMonthView.GetVersionString: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn))) + ' ' + DATE_VER;
end;

function TPlannerMonthView.GetVersion: string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn))) + '.' + IntToStr(Lo(Hiword(vn))) + '.' + IntToStr(Hi(Loword(vn))) + '.' + IntToStr(Lo(Loword(vn)));
end;

procedure TPlannerMonthView.SetVersion(const Value: string);
begin

end;

procedure TPlannerMonthView.SetCursorEx(const Value: TCursor);
begin
  if (csLoading in ComponentState) then
    FOldCursor := Value;
  inherited Cursor := Value;
end;

function TPlannerMonthView.GetCursorEx: TCursor;
begin
  Result := inherited Cursor;
end;




procedure TPlannerMonthView.InvalidateRectangle(ARect: TRect; Bkg: Boolean);
begin
  InvalidateRect(Handle, @ARect, Bkg);

end;

procedure TPlannerMonthView.DoPaint;
begin
  InvalidateRect(Handle, nil, False);
end;

procedure TPlannerMonthView.DoMonthPopup;
var
  popmenu: THandle;
  buf: array[0..128] of char;
  pt: TPoint;
  ye, mo, da: word;
  flg: integer;
begin
  pt := ClientToScreen(point(0, 0));

  popmenu := CreatePopupMenu;

  DecodeDate(seldate, ye, mo, da);

  if not CheckMonth(EncodeDate(ye, 1, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 1,
    PChar(strpcopy(buf, fnameofmonths.january)));
  if not CheckMonth(encodedate(ye, 2, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 2,
    PChar(strpcopy(buf, fnameofmonths.february)));
  if not CheckMonth(encodedate(ye, 3, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 3,
    PChar(strpcopy(buf, fnameofmonths.march)));
  if not CheckMonth(encodedate(ye, 4, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 4,
    PChar(strpcopy(buf, fnameofmonths.april)));
  if not CheckMonth(encodedate(ye, 5, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 5,
    PChar(strpcopy(buf, fnameofmonths.may)));
  if not CheckMonth(encodedate(ye, 6, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 6,
    PChar(strpcopy(buf, fnameofmonths.june)));
  if not CheckMonth(encodedate(ye, 7, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 7,
    PChar(strpcopy(buf, fnameofmonths.july)));
  if not CheckMonth(encodedate(ye, 8, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 8,
    PChar(strpcopy(buf, fnameofmonths.august)));
  if not CheckMonth(encodedate(ye, 9, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 9,
    PChar(strpcopy(buf, fnameofmonths.september)));
  if not CheckMonth(encodedate(ye, 10, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 10,
    PChar(strpcopy(buf, fnameofmonths.october)));
  if not CheckMonth(encodedate(ye, 11, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 11,
    PChar(strpcopy(buf, fnameofmonths.november)));
  if not CheckMonth(encodedate(ye, 12, 1)) then flg := MF_GRAYED
  else
    flg := 0;
  InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, 12,
    PChar(strpcopy(buf, fnameofmonths.december)));

  TrackPopupMenu(popmenu, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x + lblx1 + xoffset,
    pt.y, 0, self.handle, nil);

  DestroyMenu(popmenu);
end;



procedure TPlannerMonthView.DoYearPopup;
var
  popmenu: THandle;
  pt: TPoint;
  i: integer;
  ye, mo, da: word;
  flg: integer;
begin
  pt := ClientToScreen(point(0, 0));

  popmenu := CreatePopupMenu;
  Decodedate(thedate, ye, mo, da);
  if (mo = 2) and (da = 29) then da := 28;

  for i := 1 to 10 do
  begin
    if CheckDateRange(EncodeDate(i + ye - 5, mo, da)) then
      flg := 0
    else
      flg := MF_GRAYED;
    InsertMenu(popmenu, $FFFFFFFF, MF_BYPOSITION or flg, i + 15,
      PChar(IntToStr(i + ye - 5)));
  end;

  TrackPopupMenu(popmenu, TPM_LEFTALIGN or TPM_LEFTBUTTON, pt.x + lblx2 + xoffset,
    pt.y, 0, Handle, nil);

  DestroyMenu(popmenu);
end;

procedure TPlannerMonthView.WMCommand(var Message: TWMCommand);
var
  ye, mo, da: word;
  origdate: TDateTime;
begin
  if (message.itemid <= 12) and (message.itemid >= 1) then
  begin
    origdate := seldate;
    DecodeDate(thedate, ye, mo, da);
    mo := Message.ItemId;
    thedate := EncodeDate(ye, mo, 1);
    SelDate := thedate;
    SetLabel(mo, ye);
    FDay := da;
    FMonth := mo;
    FYear := ye;
    DoPaint;
    DoChangeMonth(origdate, seldate);
    if Assigned(OnMonthSelect) then
      OnMonthSelect(self);
    DoMonthChanged(origdate, seldate);
  end;

  if (message.itemid >= 15) and (message.itemid <= 25) then
  begin
    Origdate := SelDate;
    DecodeDate(thedate, ye, mo, da);
    ye := ye + Message.itemid - 20;
    if (mo = 2) and (da = 29) then da := 28;
    thedate := EncodeDate(ye, mo, da);
    seldate := thedate;
    SetLabel(mo, ye);
    FDay := da;
    FMonth := mo;
    FYear := ye;
    DoPaint;
    DoChangeYear(origdate, seldate);
    DoYearChanged(origdate, seldate);
  end;

  inherited;
end;

{ TSelDateItems }

function TSelDateItems.Add: TSelDateItem;
begin
  Result := TSelDateItem(inherited Add);
end;

constructor TSelDateItems.Create(AOwner: TCustomMonthViewPanel);
begin
  inherited Create(TSelDateItem);
  FOwner := AOwner;
  FVisible := True;
end;

function TSelDateItems.GetItem(Index: integer): TSelDateItem;
begin
  Result := TSelDateItem(inherited GetItem(Index));
end;

function TSelDateItems.Insert(Index: integer): TSelDateItem;
begin
  Result := TSelDateItem(inherited Insert(Index));
end;

function TSelDateItems.IsInList(da, mo, ye: integer): integer;
var
  i: integer;
  dt: TDateTime;
begin
  Result := -1;
  dt := EncodeDate(ye, mo, da);
  for i := 1 to Count do
  begin
    if dt = Items[i - 1].Date then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

function TSelDateItems.IsDateInList(dt: TDateTime): integer;
var
  i: integer;
begin
  Result := -1;
  for i := 1 to Count do
  begin
    if Trunc(dt) = Trunc(Items[i - 1].Date) then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

procedure TSelDateItems.SetItem(Index: integer; Value: TSelDateItem);
begin
  inherited SetItem(Index, Value);
end;


procedure TSelDateItems.AddRange(dt1, dt2: TDateTime);
var
  swp: TDateTime;
begin
  if dt1 > dt2 then
  begin
    swp := dt1;
    dt1 := dt2;
    dt2 := swp;
  end;
  while dt1 <= dt2 do
  begin
    if IsDateInList(dt1) = -1 then
      with Add do Date := dt1;
    dt1 := dt1 + 1;
  end;
end;

procedure TSelDateItems.DelRange(dt1, dt2: TDateTime);
var
  swp: TDateTime;
  lidx: integer;
begin
  if dt1 > dt2 then
  begin
    swp := dt1;
    dt1 := dt2;
    dt2 := swp;
  end;

  while dt1 <= dt2 do
  begin
    lidx := IsDateInList(dt1);
    if lidx <> -1 then
      items[lidx].Free;
    dt1 := dt1 + 1;
  end;
end;

procedure TSelDateItems.StartUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TSelDateItems.DoPaint;
begin
  if FUpdateCount = 0 then
    FOwner.Dopaint;
end;

procedure TSelDateItems.StopUpdate;
begin
  if FUpdateCount > 0 then
  begin
    Dec(FUpdateCount);
    if FUpdateCount = 0 then
      FOwner.Dopaint;
  end;
end;

function TSelDateItems.GetDate(dt: TDateTime): TSelDateItem;
var
  i: integer;
begin
  Result := nil;
  for i := 1 to Count do
  begin
    if Trunc(dt) = Trunc(Items[i - 1].Date) then
    begin
      Result := Items[i - 1];
      Break;
    end;
  end;
end;

procedure TSelDateItems.Update(Item: TCollectionItem);
begin
  inherited;
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TSelDateItems.RepaintDate(ADate: TDateTime);
begin
  if not FVisible then Exit;

  if FOwner.HandleAllocated then
    FOwner.RepaintDate(ADate);
end;

procedure TSelDateItems.ResetUpdate;
begin
  if FUpdateCount > 0 then
    dec(FUpdateCount);
end;

{ TSelDateItem }

procedure TSelDateItem.Assign(Source: TPersistent);
begin
  FDate := TSelDateItem(Source).Date;
end;

procedure TSelDateItem.Changed;
begin
  if (Collection as TSelDateItems).UpdateCount = 0 then
    (Collection as TSelDateItems).RepaintDate(FDate);
end;

destructor TSelDateItem.Destroy;
begin
  Changed;
  inherited;
end;

procedure TSelDateItem.SetDate(const Value: TDateTime);
begin
  if Value <> FDate then
  begin
    FDate := Value;
    Changed;
  end;
end;

{ TMinMaxDate }

constructor TMinMaxDate.Create(AOwner: TPlannerMonthView);
begin
  inherited Create;
  FOwner := AOwner;
end;

function TMinMaxDate.GetDate: TDateTime;
begin
  Result := EncodeDate(Year, Month, Day);
end;

procedure TMinMaxDate.SetDate(const Value: TDateTime);
var
  fy,fm,fd: word;
begin
  DecodeDate(Value, fy, fm, fd);

  FYear := fy;
  FMonth := fm;
  FDay := fd;
end;

procedure TMinMaxDate.SetDay(avalue: smallint);
begin
  FDay := AValue;
  FOwner.Invalidate;
end;

procedure TMinMaxDate.SetMonth(avalue: smallint);
begin
  FMonth := AValue;
  FOwner.Invalidate;
end;

procedure TMinMaxDate.SetUse(avalue: Boolean);
begin
  Fuse := AValue;
  FOwner.Invalidate;
end;

procedure TMinMaxDate.SetYear(avalue: smallint);
begin
  FYear := AValue;
  FOwner.Invalidate;
end;

{$IFDEF USEIMAGE}

procedure TPlannerMonthView.SetBackgroundPosition(const Value: TBackgroundPosition);
begin
  FBackgroundPosition := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetImage(const Value: TAdvImage);
begin
  FImage.Assign(Value);
  Invalidate;
end;

procedure TPlannerMonthView.BackgroundChanged(Sender: TObject);
begin
  Invalidate;
  Changed;
end;
{$ENDIF}

procedure TPlannerMonthView.CreateToolTip;
begin
  fhToolTip := CreateWindowEx(0, 'Tooltips_Class32', nil, TTS_ALWAYSTIP or TTS_BALLOON,
    Integer(CW_USEDEFAULT), Integer(CW_USEDEFAULT),Integer(CW_USEDEFAULT),
    Integer(CW_USEDEFAULT), Handle, 0, hInstance, nil);

  if fhToolTip <> 0 then
    SetWindowPos(fhToolTip, HWND_TOPMOST, 0, 0, 0, 0, SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE);
end;

procedure TPlannerMonthView.DestroyToolTip;
begin
  DestroyWindow(fhToolTip);
end;


procedure TPlannerMonthView.AddToolTip(IconType: Integer; Text, Title: string);
var
  Item: THandle;
  Rect: TRect;
  ti: TToolInfo;
  buffer: array[0..255] of char;
begin
  Item := self.Handle;

  if (Item <> 0) AND (Windows.GetClientRect(Item, Rect)) then
  begin
    ti.cbSize := SizeOf(TToolInfo);

    ti.uFlags := TTF_SUBCLASS or TTF_IDISHWND ;
    ti.hInst := hInstance;

    ti.hwnd := Item;
    ti.Rect := Rect;
    ti.uId := Handle;

    ti.lpszText :=  LPSTR_TEXTCALLBACK;
    SendMessage(fhToolTip, TTM_ADDTOOL, 0, LParam(@ti));
    FillChar(buffer, sizeof(buffer), #0);
    lstrcpy(buffer, PChar(Title));
    if (IconType > 3) or (IconType < 0) then IconType := 0;
    SendMessage(fhToolTip, TTM_SETTITLE, IconType, LParam(@buffer));
  end;
end;


procedure TPlannerMonthView.BalloonInit;

  procedure WindowBlend(hwnd:thandle;colorkey:tcolor;alpha:byte;r:trect);
  var
   dw: dword;
   blnd: _BLENDFUNCTION;
   dskdc: thandle;
   size,src: tpoint;
   hdc: thandle;
  begin
   hdc := GetDC(hwnd);
   dw := GetWindowLong(hwnd, GWL_EXSTYLE);
   SetWindowLong(hwnd, GWL_EXSTYLE,dw or WS_EX_LAYERED);
   DynaLink_SetLayeredWindowAttributes(hwnd,DWORD(colorkey),alpha,2);
   blnd.BlendOp := AC_SRC_OVER;
   blnd.BlendFlags := 0;
   blnd.SourceConstantAlpha := 0;
   blnd.AlphaFormat := 0;
   dskdc := GetDC(0);
   size := Point(r.right-r.left,r.bottom-r.top);
   src := Point(r.left,r.top);
   DynaLink_UpdateLayeredWindow(hwnd,dskdc,nil,@size,hdc,@src,dword(colorkey), blnd,ULW_ALPHA);
   ReleaseDC(0,dskdc);
   ReleaseDC(hwnd,hdc);
  end;

begin
  CreateToolTip;
  AddToolTip(3,'Planner','ToolTipText');
  if FBalloon.InitialDelay <> -1 then
    SendMessage(fhtooltip,TTM_SETDELAYTIME, TTDT_INITIAL, FBalloon.InitialDelay);

  if FBalloon.ReshowDelay <> -1 then
    SendMessage(fhtooltip,TTM_SETDELAYTIME, TTDT_RESHOW, FBalloon.ReshowDelay);

  if FBalloon.AutoHideDelay <> -1 then
    SendMessage(fhtooltip,TTM_SETDELAYTIME, TTDT_AUTOPOP, FBalloon.AutoHideDelay);

  if (FBalloon.Transparency > 0) then
    WindowBlend(fhToolTip,0,255 - FBalloon.Transparency,Rect(0,0,100,100));
end;

procedure TPlannerMonthView.BalloonDone;
begin
  if FBalloon.Enable then
    DestroyToolTip;
end;

procedure TPlannerMonthView.BalloonChange(Sender: TObject);
begin
  if not (csLoading in ComponentState) then
  begin
    if FBalloon.Enable then
      BalloonInit
    else
      BalloonDone;
  end;
end;

procedure TPlannerMonthView.CMHintShow(var Msg: TMessage);

var
  hi: PHintInfo;
  CanShow: Boolean;
  ad: TDateTime;
  plIt: TPlannerItem;
  r: TRect;

begin
  hi := PHintInfo(Msg.LParam);
  ad := XYToDate(hi^.CursorPos.x - xoffset, hi^.Cursorpos.y, false);
  plIt := FPlannerMonthItems.FindItemAtDate(ad, hi^.CursorPos.x - xoffset, hi^.CursorPos.y);

  if Assigned(plIt) then
  begin
    hi^.HintStr := plit.Hint;
    if Assigned(OnItemHint) then
      OnItemHint(self, plIt, hi.HintStr);

    if plIt.HintIndicator then
    begin
      R := FullItemRectAtDate(plit, ad);
      hi.HintPos := ClientToScreen(point(r.Right, r.Top));

      if plIt.Shadow then
        hi.HintPos := Point(hi.HintPos.X - 2, hi.HintPos.Y);
    end;
  end
  else
  begin
    if Assigned(OnGetDateHintString) then
      OnGetDateHintString(self, ad, CanShow, hi^.HintStr);
  end;

  if (BrowserHint <> '') then
  begin
    hi := PHintInfo(Msg.LParam);

    Canshow := (EventHint <> '') and not ((FLastHintPos.x = -1) or (FLastHintPos.y = -1));
    ShowHintbusy := Canshow;
    if CanShow then
    begin
      Hi^.Hintpos.X := (FLastHintPos.x + 1) * dx;
      Hi^.Hintpos.y := FLastHintPos.y * dy + yoffset;
      Hi^.HintStr := EventHint;
      Hi^.Hintpos := ClientToScreen(Hi^.HintPos);
    end;

    if (BrowserHint <> '') then
    begin
      Hi^.Hintpos.X := FLastHintPos.x;
      Hi^.Hintpos.y := FLastHintPos.y;
      Hi^.HintStr := BrowserHint;
      Hi^.Hintpos := ClientToScreen(Hi^.HintPos);
    end;
  end;
end;

procedure TPlannerMonthView.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  dbclkD: TDateTime;
  x: integer;
  APlannerItem: TPlannerItem;
begin
  if (csDesigning in ComponentState) then
  begin
    //Message.Result := 1;
    Exit;
  end;
  inherited;

  if (Message.YPos > (FweekNameY + FDayFontHeight)) and (Message.XPos > 0) then
  begin
    x := Message.XPos;
    if ShowWeeks then
      x := x - WeekWidth;
    dbclkD := XYToDate(x, Message.YPos, false);
    APlannerItem := FPlannerMonthItems.FindItemAtDate(dbclkD, Message.XPos, Message.YPos);
    if APlannerItem <> nil then
    begin
      FMouseDownMove := false;
      FMouseOnItemStart := false;
      FMouseOnItemEnd := false;
      FMouseDownResize := false;
      if Assigned(OnItemDblClick) then
        OnItemDblClick(self, APlannerItem);
    end
    else
    begin
      if ShowScrollColumn then
        if Message.XPos > Width - ScrollColumnSize - 8 then
          Exit;
      if Assigned(OnDblClick) then
        OnDblClick(Self, dbclkD);
    end;
  end;

end;

procedure TPlannerMonthView.PropsChanged(Sender: TObject);
begin
  SetLabel(Month, Year);
  Invalidate;
end;

procedure TPlannerMonthView.SetShowDaysAfter(const Value: Boolean);
begin
  FShowDaysAfter := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowDaysBefore(const Value: Boolean);
begin
  FShowDaysBefore := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowSelection(const Value: Boolean);
begin
  FShowSelection := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowSelectionFull(const Value: Boolean);
begin
  FShowSelectionFull := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowFocusRectangle(const Value: Boolean);
begin
  FShowFocusRectangle := Value;
  Invalidate;
end;


procedure TPlannerMonthView.DoChangeMonth(dt1, dt2: TDateTime);
begin
  if Assigned(FOnMonthChange) then
    FOnMonthChange(self, dt1, dt2);

  if Assigned(FOnDateChange) then
   FOnDateChange(self, dt1, dt2);
end;

procedure TPlannerMonthView.DoMonthChanged(dt1, dt2: TDateTime);
begin
   if Assigned(OnMonthChanged) then
     FOnMonthChanged(Self, dt1, dt2);
end;
procedure TPlannerMonthView.DoChangeYear(dt1, dt2: TDateTime);
begin
  if Assigned(FOnYearChange) then
    FOnYearChange(self, dt1, dt2);

  if Assigned(FOnDateChange) then
   FOnDateChange(self, dt1, dt2);
end;

procedure TPlannerMonthView.DoYearChanged(dt1, dt2: TDateTime);
begin
   if Assigned(OnYearChanged) then
     FOnYearChanged(Self, dt1, dt2);
end;
procedure TPlannerMonthView.SetGradientDirection(
  AValue: TGradientDirection);
begin
  FGradientDirection := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetGradientEndColor(AValue: TColor);
begin
  FGradientEndColor := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetGradientStartColor(AValue: TColor);
begin
  FGradientStartColor := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetMonthGradientStartColor(AValue: TColor);
begin
  FMonthGradientStartColor := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetMonthGradientEndColor(AValue: TColor);
begin
  FMonthGradientEndColor := AValue;
  Invalidate;
end;

procedure TPlannerMonthView.SetMonthGradientDirection(AValue: TGradientDirection);
begin
  FMonthGradientDirection := AValue;
  Invalidate;
end;

function TPlannerMonthView.NumRows: Integer;
begin
  Result := 8;
end;

function TPlannerMonthView.NumCols: Integer;
begin
  Result := 7
end;

procedure TPlannerMonthView.SetCaptionFont(const Value: TFont);
begin
  FCaptionFont.Assign(Value);
  Invalidate;
end;

function TPlannerMonthView.GetCaptionHeight: Integer;
begin
  Result := FCaptionHeight;
end;

function TPlannerMonthView.GetCompletion: TCompletion;
begin
  Result := FCompletion;
end;

function TPlannerMonthView.GetComponentStyle: TTMSStyle;
begin
  Result := FTMSStyle;
end;

procedure TPlannerMonthView.SetCaptionHeight(const Value: integer);
begin
  if (Value > 10) then
  begin
    FCaptionHeight := Value;
    FweekNameY := 12 + FCaptionHeight;
    Invalidate;
  end;

  if not FShowCaption then
    FWeekNameY := 0;
end;

procedure TPlannerMonthView.SetEditDirectSelection(ARect: TRect; X,
  Y: Integer);
var
  CharacterCoordinate: Integer;
begin
  if (FPlanner.EditDirect) and (X > -1) then
  begin
    CharacterCoordinate := SendMessage(FMemo.Handle, EM_CHARFROMPOS, 0,
      MakeLong(X - ARect.Left, Y - ARect.Top));
    if (CharacterCoordinate = -1) then
      Exit;
    FMemo.SelStart := LoWord(CharacterCoordinate);
    FMemo.SelLength := 0;
  end
  else
    FMemo.SelectAll;
end;

procedure TPlannerMonthView.DoPrint(ACanvas: TCanvas);
var
  XSize, YSize: Integer;
  DrawRect: TRect;
  LeftIndent, TopIndent: Integer;
  a,sa,fa:string;
  XS, YS, ml, hl: Integer;
  cr,hr: TRect;
  CID,CV,CT:string;
  PaintRect: TRect;
  OldWeekNameY: Integer;
begin
  if Assigned(FOnPrintStart) then
    FOnPrintStart(Self, ACanvas);

  // Get the paper dimensions
  XSize := ACanvas.ClipRect.Right - ACanvas.ClipRect.Left -
    FPrintOptions.LeftMargin - FPrintOptions.RightMargin;

  YSize := ACanvas.ClipRect.Bottom - ACanvas.ClipRect.Top -
    FPrintOptions.HeaderSize - FPrintOptions.FooterSize;

  FHTMLFactor := CanvasToHTMLFactor(Canvas,ACanvas);

  LeftIndent := FPrintOptions.LeftMargin;
  TopIndent := FPrintOptions.HeaderSize;

  if (FPrintOptions.Header.Count > 0) or Assigned(FOnPrintHeader) then
  begin
    // Print Header over full size
    DrawRect.Left := LeftIndent;
    DrawRect.Right := XSize;
    DrawRect.Top := 0;
    DrawRect.Bottom := TopIndent;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Assign(FPrintOptions.HeaderFont);

    if Assigned(FOnPrintHeader) then
      FOnPrintHeader(Self, ACanvas, DrawRect)
    else
    begin
      if pos('</',FPrintOptions.Header.Text) > 0 then
        HTMLDrawEx(ACanvas, FPrintOptions.Header.Text, DrawRect, PlannerImages, 0, 0, -1, -1, 1, False, False,
        True, False, True, False, False
        ,False
        , FHTMLFactor, URLColor, clNone, clNone, clGray, a, sa, fa, XS, YS, ml, hl, hr
        , cr, CID, CV, CT, FImageCache, FContainer, Handle
        )
      else
        PrinterDrawString(ACanvas,FPrintOptions.Header.Text, DrawRect,
          AlignToFlag(FPrintOptions.HeaderAlignment));
    end;
  end;

  if (FPrintOptions.Footer.Count > 0) or Assigned(FOnPrintFooter) then
  begin
    // Print footer over full size
    DrawRect.Left := LeftIndent;
    DrawRect.Right := XSize;
    DrawRect.Top := YSize;
    DrawRect.Bottom := DrawRect.Top + FPrintOptions.FooterSize;
    ACanvas.Brush.Style := bsClear;
    ACanvas.Font.Assign(FPrintOptions.FooterFont);

    if Assigned(FOnPrintFooter) then
      FOnPrintFooter(Self, ACanvas, DrawRect)
    else
    begin
      if Pos('</',FPrintOptions.Footer.Text) > 0 then
        HTMLDrawEx(ACanvas, FPrintOptions.Footer.Text, DrawRect, PlannerImages, 0, 0, -1, -1, 1, False, False,
        True, False, True, False, False
        ,False
        , FHTMLFactor, URLColor, clNone, clNone, clGray, a, sa, fa, XS, YS, ml, hl, hr
        , cr, CID, CV, CT, FImageCache, FContainer, Handle
        )
      else
        PrinterDrawString(ACanvas,FPrintOptions.Footer.Text, DrawRect, AlignToFlag(FPrintOptions.FooterAlignment));
    end;
  end;

  FCanvas := ACanvas;
  PaintRect := Rect(LeftIndent, TopIndent, XSize  - FPrintOptions.RightMargin, YSize - FPrintOptions.FooterSize);

  OldWeekNameY := FWeekNameY;
  FWeekNameY := Round(FHTMLFactor * 25);

  PaintCalendar(ACanvas, PaintRect, True);

  FWeekNameY := OldWeekNameY;
end;


procedure TPlannerMonthView.PrintTo(Canvas: TCanvas);
var
  mi: Integer;
begin
  mi := MaxItemsDisplayed;
  MaxItemsDisplayed := 99; // make sure all items are printed
  try
    DoPrint(Canvas);
  finally
    MaxItemsDisplayed := mi;
  end;
end;

procedure TPlannerMonthView.Print;
var
  mi: Integer;
begin
  with Printer do
  begin
    Title := FPrintOptions.JobName;
    Orientation := FPrintOptions.Orientation;
    BeginDoc;
    mi := MaxItemsDisplayed;
    MaxItemsDisplayed := 99; // make sure all items are printed
    try
      DoPrint(Canvas);
    finally
      MaxItemsDisplayed := mi;
    end;
    EndDoc;
  end;
end;


procedure TPlannerMonthView.StartEdit(APlannerItem: TPlannerItem; aDate: TDateTime; X, Y: Integer);
var
  ColumnHeight, ih, iw, ew, tw, eh: Integer;
  s: string;
  ER: TRect;
  P: TPoint;
  ARect, R: TRect;
  ItemHeight, j: integer;
begin
  if APlannerItem = nil then
    Exit;

  if TMonthPlannerItem(APlannerItem).IsPopupEdit then
    Exit;

  if (not AllowItemEdit) then
    Exit;

  P := DateToXY(aDate);
  r.right := xoffset + P.X * dx + 2;
  r.top := (P.Y - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;
  r.bottom := r.top + dy;
  r.left := r.right - dx; // + 2;
  R.Top := R.Top + FDayNumberHeight + 2;

  itemHeight := R.Bottom - R.Top;
  itemHeight := itemHeight div min(FMaxItemsDisplayed, APlannerItem.Conflicts);
  R.Top := R.Top + (APlannerItem.ConflictPos - FItemScrollerAry[P.Y].Position) * itemHeight;
  R.Bottom := R.top + itemHeight - ItemSpace;


  j := TMonthPlannerItem(APlannerItem).PosSt.IndexOfName(inttostr(P.Y));
  R.Left := integer(TMonthPlannerItem(APlannerItem).PosSt.Objects[j]);

  R.Right := strtoint(TMonthPlannerItem(APlannerItem).PosSt.Values[inttostr(P.Y)]) - FTrackWidth - 2;

  ARect := R;
  if Assigned(APlannerItem.Editor) then
  begin
    if APlannerItem.Editor.EditorUse = euAlways then
    begin
      if Assigned(OnItemStartEdit) then
      begin
        OnItemStartEdit(Self, APlannerItem);
      end;

      APlannerItem.Editor.Edit(FPlanner, APlannerItem);

      if Assigned(OnItemEndEdit) then
        OnItemEndEdit(Self, APlannerItem);
      Exit;
    end;
  end;

  if FPlanner.InplaceEdit = ieNever then
    Exit;

  if Assigned(OnItemStartEdit) then
  begin
    OnItemStartEdit(Self, APlannerItem);
  end;

  ColumnHeight := 0;
  ih := 0;
  iw := 0;
  ew := 0;
  eh := 0;

  if (APlannerItem.Shape = psRect) and (APlannerItem.TrackVisible) then
    tw := FPlanner.TrackWidth
  else
    tw := 0;

  case APlannerItem.Shape of
    psRounded:
      begin
        ew := ew + (CORNER_EFFECT shr 1) - 2;
        ARect.Top := ARect.Top + 2;
      end;
    psHexagon:
      begin
        ew := ew + CORNER_EFFECT;
      end;
{$IFDEF TMSSKINS}
    psSkin:
      begin
        InflateRect(ARect, 0, -(FPlanner.Skin.SkinY div 2));
        ew := FPlanner.Skin.SkinX div 2;
      end;
{$ENDIF}
  end;

  if APlannerItem.Shadow then
  begin
    ew := ew + 1;
    eh := eh + 2;
  end;

  if APlannerItem.CompletionDisplay = cdVertical then
    ew := ew + 10; //12;

  if APlannerItem.CompletionDisplay = cdHorizontal then
    ARect.Top := ARect.Top + 11;

  if ((APlannerItem.ImageID >= 0) or (APlannerItem.ImageIndexList.Count > 0)) and
    Assigned(FPlanner.PlannerImages) then
  begin
    if not ((APlannerItem.CaptionType = ctNone) or (APlannerItem.ImagePosition = ipVertical)) then
      ih := FPlanner.PlannerImages.Height + 4 + FPlanner.ImageOffsetY;

    iw := FPlanner.PlannerImages.Width + EDITOFFSET;
  end;

  if (APlannerItem.CaptionType <> ctNone) or
    ((APlannerItem.ImageIndexList.Count > 1) and (APlannerItem.ImagePosition = ipHorizontal)) then
  begin
    Canvas.Font.Assign(APlannerItem.CaptionFont);
    ColumnHeight := Canvas.TextHeight('gh') + 6; //6;

    if APlannerItem.Shape = psRounded then
      ColumnHeight := ColumnHeight - 2;

    if (APlannerItem.ImagePosition = ipHorizontal) then
      iw := 0;
  end;

  if (ih > ColumnHeight) then
    ColumnHeight := ih;

  s := APlannerItem.Text.Text;



  if IsRtf(s) or (APlannerItem.InplaceEdit = peRichText) then
  begin
    FRichEdit.PlannerItem := TMonthPlannerItem(APlannerItem);
    self.TextToRich(s);
    FRichEdit.Top := ARect.Top + 6 + ColumnHeight;
    FRichEdit.Left := ARect.Left + 3 + iw + ew;
    FRichEdit.Width := ARect.Right - ARect.Left - 3 - iw - 2 * ew;
    FRichEdit.Height := ARect.Bottom - ARect.Top - FPlanner.ItemGap - ColumnHeight - FPlanner.TrackWidth - 6 - eh;

    FRichEdit.Visible := True;
    BringWindowToTop(FRichEdit.Handle);
    FRichEdit.SetFocus;
    FRichEdit.SelectAll;
    FIsEditing := true;
  end
  else
    case APlannerItem.InplaceEdit of
      peMaskEdit, peEdit:
        begin
          if APlannerItem.InplaceEdit = peMaskEdit then
            FMaskEdit.EditMask := APlannerItem.EditMask
          else
            FMaskEdit.EditMask := '';
          FMaskEdit.Font.Assign(FPlanner.Font);
          if APlannerItem.ShowSelection then
          begin
            FMaskEdit.Color := APlannerItem.SelectColor; //FPlanner.GetEditColor(APlannerItem, true);
            FMaskEdit.Font.Color := APlannerItem.SelectFontColor
          end
          else
            FMaskEdit.Color := APlannerItem.SelectColor; //FPlanner.GetEditColor(APlannerItem, false);

          if TMonthPlannerItem(APlannerItem).IsCurrent and ShowCurrentItem then
            FMaskEdit.Color := ColorCurrentItem;

          FMaskEdit.PlannerItem := APlannerItem;
          FMaskEdit.Top := ARect.Top + FPlanner.TrackWidth + 2 + ColumnHeight;
          FMaskEdit.Left := ARect.Left + tw + iw + ew;
          FMaskEdit.Width := ARect.Right - ARect.Left - tw - 3 - FPlanner.ItemGap -
            iw - 2 * ew;
          FMaskEdit.Height := ARect.Bottom - FMaskEdit.Top - FPlanner.ItemGap - FPlanner.TrackWidth - 2;

          FMaskEdit.BorderStyle := bsNone;
          FMaskEdit.Visible := True;
          if (APlannerItem.Text.Count > 0) then
            FMaskEdit.Text := APlannerItem.Text[0];
          BringWindowToTop(FMaskEdit.Handle);
          FMaskEdit.SetFocus;
          
          FIsEditing := true;
        end;
      peMemo:
        begin
          FMemo.Parent := Self;
          FMemo.ScrollBars := FPlanner.EditScroll;
          FMemo.Font.Assign(APlannerItem.Font);
          
          if APlannerItem.ShowSelection then
          begin
            FMemo.Color := APlannerItem.SelectColor;
            FMemo.Font.Color := APlannerItem.SelectFontColor;
          end
          else
            FMemo.Color := APlannerItem.SelectColor;

          if TMonthPlannerItem(APlannerItem).IsCurrent and ShowCurrentItem then
            FMemo.Color := ColorCurrentItem;

          FMemo.PlannerItem := APlannerItem;

          FMemo.Top := ARect.Top + FPlanner.TrackWidth + 2 + ColumnHeight;
          FMemo.Left := ARect.Left + 3 + iw + ew;
          FMemo.Width := ARect.Right - ARect.Left - tw - iw - 2*ew + 1;
          FMemo.Height := ARect.Bottom - ARect.Top - FPlanner.ItemGap - ColumnHeight - FPlanner.TrackWidth - 6 - eh;

          FMemo.BorderStyle := bsNone;
          FMemo.Visible := True;
          BringWindowToTop(FMemo.Handle);
          FMemo.Lines.Text := HTMLStrip(APlannerItem.Text.Text);
          FMemo.SetFocus;

          SetEditDirectSelection(ARect, X, Y);

          FIsEditing := true;
        end;
      peCustom:
        begin
          ER.Left := ARect.Left + 3 + iw + ew;
          ER.Top := ARect.Top + FPlanner.TrackWidth + 2 + ColumnHeight;
          ER.Right := ARect.Right - tw - 3 - iw - 2 * ew;
          ER.Bottom := ARect.Bottom - ARect.Top - FPlanner.ItemGap - ColumnHeight - FPlanner.TrackWidth - 6 - eh;
          if Assigned(FOnCustomEdit) then
            OnCustomEdit(Self, ER, APlannerItem);
        end;
    end;
end;


function TPlannerMonthView.ItemRectAtDate(APlannerItem: TPlannerItem; aDate: TDateTime): TRect;
var
  r: TRect;
  p: TPoint;
  ItemHeight: integer;
begin
  Result := Rect(0, 0, 0, 0);
  if (aDate >= int(TMonthPlannerItem(APlannerItem).ItemStartTime)) and (aDate <= int(TMonthPlannerItem(APlannerItem).ItemEndTime)) then
  begin
    p := DateToXY(aDate);
    r.right := xoffset + P.X * dx + 2;
    r.top := (P.Y - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13; //25
    r.bottom := r.top + dy;
    r.left := r.right - dx; // + 2;
    R.Top := R.Top + FDayNumberHeight + 2;

    itemHeight := R.Bottom - R.Top;

    if min(FMaxItemsDisplayed, APlannerItem.Conflicts) > 0 then
      itemHeight := itemHeight div min(FMaxItemsDisplayed, APlannerItem.Conflicts);

    R.Top := R.Top + (APlannerItem.ConflictPos - FItemScrollerAry[P.y].Position) * itemHeight;
    R.Bottom := R.top + itemHeight - ItemSpace;

    Result := R;
  end;
end;

function TPlannerMonthView.FullItemRectAtDate(APlannerItem: TPlannerItem; aDate: TDateTime): TRect;
var
  r: TRect;
  p: TPoint;
  j: integer;
begin
  R := ItemRectAtDate(APlannerItem, ADate);

  if (aDate >= int(TMonthPlannerItem(APlannerItem).ItemStartTime)) and (aDate <= int(TMonthPlannerItem(APlannerItem).ItemEndTime)) then
  begin
    P := DateToXY(aDate);
    j := TMonthPlannerItem(APlannerItem).PosSt.IndexOfName(inttostr(P.Y));
    R.Left := Integer(TMonthPlannerItem(APlannerItem).PosSt.Objects[j]);
    R.Right := StrToInt(TMonthPlannerItem(APlannerItem).PosSt.Values[IntToStr(P.Y)]);
    Result := R;
  end;
end;

function TPlannerMonthView.ItemRectAtRow(APlannerItem: TPlannerItem; RowNo: integer): TRect;
var
  r: TRect;
  ItemHeight, i: integer;
begin
  Result := Rect(0, 0, 0, 0);
  i := TMonthPlannerItem(APlannerItem).PosSt.IndexOfName(inttostr(RowNo));

  if (i >= 0) then
  begin
    r.Left := integer(TMonthPlannerItem(APlannerItem).PosSt.Objects[i]);
    r.Right := strtoint(TMonthPlannerItem(APlannerItem).PosSt.Values[inttostr(RowNo)]) - FTrackWidth - 2;

    r.top := (RowNo - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13;
    r.bottom := r.top + dy;
    R.Top := R.Top + FDayNumberHeight + 2;

    itemHeight := R.Bottom - R.Top;

    if min(FMaxItemsDisplayed, APlannerItem.Conflicts) > 0 then
      itemHeight := itemHeight div min(FMaxItemsDisplayed, APlannerItem.Conflicts);
      
    R.Top := R.Top + (APlannerItem.ConflictPos - FItemScrollerAry[RowNo].Position) * itemHeight;
    R.Bottom := R.top + itemHeight - ItemSpace;

    Result := R;
  end;
end;

procedure TPlannerMonthView.SetBounds(ALeft, ATop, AWidth,
  AHeight: Integer);
begin
  inherited;
end;

procedure TPlannerMonthView.SetCaptionColorTo(const Value: TColor);
begin
  if Value <> FCaptionColorTo then
  begin
    FCaptionColorTo := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetCaptionGradientDirection(
  const Value: TGradientDirection);
begin
  if Value <> FCaptionGradientDirection then
  begin
    FCaptionGradientDirection := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetWeekWidth(const Value: integer);
begin
  if (Value <> FWeekWidth) and (Value >= 0) then
  begin
    FWeekWidth := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetShowCurrent(const Value: Boolean);
begin
  if Value <> FShowCurrent then
  begin
    FShowCurrent := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetShowCurrentItem(const Value: Boolean);
begin
  if Value <> FShowCurrentItem then
  begin
    FShowCurrentItem := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.AttachementGlyphOnChange(Sender: TObject);
begin
  FPlanner.AttachementGlyph.Assign(FAttachementGlyph);
  Invalidate;
end;

procedure TPlannerMonthView.URLGlyphOnChange(Sender: TObject);
begin
  FPlanner.URLGlyph.Assign(FURLGlyph);
  Invalidate;
end;

procedure TPlannerMonthView.PlannerItemEnter(Sender: TObject;
  Item: TPlannerItem);
begin
  if Assigned(FOnItemEnter) then
    FOnItemEnter(self, Item);
end;

procedure TPlannerMonthView.PlannerItemExit(Sender: TObject;
  Item: TPlannerItem);
begin
  if Assigned(FOnItemExit) then
    FOnItemExit(self, Item);
end;

procedure TPlannerMonthView.PlannerItemSelected(Sender: TObject; Item: TPlannerItem);
begin
  ItemSelected(Item);
end;

procedure TPlannerMonthView.PlannerItemText(Sender: TObject; Item: TPlannerItem; var Text: string);
begin
  if Assigned(FOnItemText) then
    FOnItemText(self, Item, Text);
end;

procedure TPlannerMonthView.PlannerItemActivate(Sender: TObject;
  Item: TPlannerItem);
begin
  if Assigned(FOnItemActivate) then
    FOnItemActivate(self, Item);
end;

procedure TPlannerMonthView.PlannerItemDeActivate(Sender: TObject;
  Item: TPlannerItem);
begin
  if Assigned(FOnItemDeActivate) then
    FOnItemDeActivate(self, Item);
end;

procedure TPlannerMonthView.SetMaxItemsDisplayed(const Value: integer);
begin
  if (Value <> FMaxItemsDisplayed) and (Value > 0) then
  begin
    FMaxItemsDisplayed := Value;
    SetItemScrollerPosition;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetShowScrollColumn(const Value: Boolean);
begin
  if Value <> FShowScrollColumn then
  begin
    FShowScrollColumn := Value;
    Invalidate;
  end;
end;

function TPlannerMonthView.GetUpScrollBtnRect(RowNo: integer): TRect;
var
  ofs: integer;
begin
  Result := Rect(0, 0, 0, 0);
  if ShowScrollColumn and (RowNo >= 1) and (RowNo <= 6) then
  begin
    ofs := (Width - (xoffset + 7 * dx + 2) - ScrollColumnSize) div 2;
    Result.Left := (xoffset + 7 * dx + 2) + ofs;
    Result.Top := (RowNo - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13 + 2;
    Result.Bottom := Result.Top + ScrollColumnSize;
    Result.Right := Result.Left + ScrollColumnSize;
  end;
end;

procedure TPlannerMonthView.PaintUpScrollBtn(RowNo: integer);
var
  R: TRect;
begin
  if not FShowScrollColumn or not FItemScrollerAry[RowNo].Visible then
    Exit;

  R := GetUpScrollBtnRect(RowNo);
  with Canvas do
  begin
    Brush.color := clBtnFace;
    //FillRect(Rect(R.Left,R.Top,R.Left+13,R.Top+12));

    Pen.Color := FArrowColor;
      // |
    MoveTo(R.Left + 2, R.Top + 8);
    LineTo(R.Left + 10, R.Top + 8);
      // /
    MoveTo(R.Left + 2, R.Top + 8);
    LineTo(R.Left + 6, R.Top + 4);
      // \
    MoveTo(R.Left + 10, R.Top + 8);
    LineTo(R.Left + 5, R.Top + 3);
      // Fill arrow |
    if FItemScrollerAry[RowNo].CanGoBack then
    begin
      MoveTo(R.Left + 4, R.Top + 7);
      LineTo(R.Left + 9, R.Top + 7);
      MoveTo(R.Left + 5, R.Top + 6);
      LineTo(R.Left + 8, R.Top + 6);
      Pixels[R.Left + 6, R.Top + 5] := FArrowColor;
    end;
  end;
end;

procedure TPlannerMonthView.PaintDownScrollBtn(RowNo: integer);
var
  R: TRect;
begin
  if not FShowScrollColumn or not FItemScrollerAry[RowNo].Visible then
    Exit;

  R := GetDownScrollBtnRect(RowNo);
  with Canvas do
  begin
    Brush.color := clBtnFace;
    //FillRect(Rect(R.Left,R.Top,R.Left+13,R.Top+12));

    Pen.Color := FArrowColor;
      // |
    MoveTo(R.Left + 2, R.Top + 4);
    LineTo(R.Left + 10, R.Top + 4);
      // \
    MoveTo(R.Left + 2, R.Top + 4);
    LineTo(R.Left + 6, R.Top + 8);
      // /
    MoveTo(R.Left + 10, R.Top + 4);
    LineTo(R.Left + 5, R.Top + 9);
      // Fill Arrow |
    if FItemScrollerAry[RowNo].CanGoForward then
    begin
      MoveTo(R.Left + 4, R.Top + 5);
      LineTo(R.Left + 9, R.Top + 5);
      MoveTo(R.Left + 5, R.Top + 6);
      LineTo(R.Left + 8, R.Top + 6);
      Pixels[R.Left + 6, R.Top + 7] := FArrowColor;

    end;
  end;
end;

function TPlannerMonthView.GetDownScrollBtnRect(RowNo: integer): TRect;
var
  ofs: integer;
begin
  Result := Rect(0, 0, 0, 0);
  if ShowScrollColumn and (RowNo >= 1) and (RowNo <= 6) then
  begin
    ofs := (Width - (xoffset + 7 * dx + 2) - ScrollColumnSize) div 2;
    Result.Left := (xoffset + 7 * dx + 2) + ofs;
    Result.Top := ((RowNo - 1) * dy + yoffset - 2 + FWeekNameY + FDayFontHeight - 13) + dy - 3 - ScrollColumnSize;
    Result.Bottom := Result.Top + ScrollColumnSize;
    Result.Right := Result.Left + ScrollColumnSize;
  end;
end;

function TPlannerMonthView.GetDragImages: TDragImageList;
begin
  Result := FDragImage;
end;

function TPlannerMonthView.GetMaxConflict(FromDate, ToDate: TDateTime): integer;
var
  i: integer;
  APlannerItem: TPlannerItem;
begin
  Result := 0;
  for i := 0 to Items.Count - 1 do
  begin
    APlannerItem := Items[i];
    if (TMonthPlannerItem(APlannerItem).ItemStartTime >= FromDate) and (Int(TMonthPlannerItem(APlannerItem).ItemEndTime) <= ToDate)
      or (TMonthPlannerItem(APlannerItem).ItemStartTime <= FromDate) and (Int(TMonthPlannerItem(APlannerItem).ItemEndTime) >= FromDate)
      or (TMonthPlannerItem(APlannerItem).ItemStartTime >= FromDate) and (Int(TMonthPlannerItem(APlannerItem).ItemStartTime) <= ToDate)
      or (TMonthPlannerItem(APlannerItem).ItemStartTime <= FromDate) and (Int(TMonthPlannerItem(APlannerItem).ItemEndTime) >= ToDate) then
    begin
      Result := Max(Result, APlannerItem.Conflicts);
    end; {
    else
    begin
      if (TMonthPlannerItem(APlannerItem).ItemStartTime <= FromDate) and (TMonthPlannerItem(APlannerItem).ItemEndTime <= ToDate) then
      begin
        Result:= Max(Result, APlannerItem.Conflicts);
      end
      else
      begin
        if (TMonthPlannerItem(APlannerItem).ItemStartTime <= FromDate) and (TMonthPlannerItem(APlannerItem).ItemEndTime >= ToDate) then
        begin
          Result:= Max(Result, APlannerItem.Conflicts);
        end
        else
        begin
          if (TMonthPlannerItem(APlannerItem).ItemStartTime >= FromDate) and (TMonthPlannerItem(APlannerItem).ItemEndTime >= ToDate) then
          begin
            Result:= Max(Result, APlannerItem.Conflicts);
          end;
        end;
      end;
    end;  }
  end;
end;


function TPlannerMonthView.GetItemScroller(RowNo: integer): TItemScroller;
begin
  Result := nil;
  if (RowNo >= 1) and (RowNo <= 6) then
    Result := FItemScrollerAry[RowNo];
end;

procedure TPlannerMonthView.SetItemScrollerPosition;
var
  da, mo, ye, pmo, pye, nmo, nye: word;
  d: TDateTime;
  j, i, fd: integer;
  FromDate, ToDate: TDateTime;
  ValueAssign, FirstTime: Boolean;
  MaxConfl: integer;
begin
  DecodeDate(TheDate, ye, mo, da);
  d := EncodeDate(ye, mo, 1);

  //first day of the month
  fd := DayOfWeek(d) - 1 - StartDay;

  if fd < 0 then
    fd := fd + 7;

  //determine previous month
  if mo = 1 then
  begin
    pmo := 12;
    pye := ye - 1;
  end
  else
  begin
    pmo := mo - 1;
    pye := ye;
  end;

  //determine next month
  if mo = 12 then
  begin
    nmo := 1;
    nye := ye + 1;
  end
  else
  begin
    nmo := mo + 1;
    nye := ye;
  end;

  for j := 1 to 6 do
  begin
    FItemScrollerAry[j].Visible := false;
    FromDate := -1; //EncodeDate(ye, mo, daysinmonth(nmo, nye));
    //ToDate:= -1;//EncodeDate(pye, pmo, 1);
    FirstTime := true;
    for i := 1 to 7 do
    begin
      ValueAssign := false;
      if (fd >= (i + (j - 1) * 7)) then
      begin
        if FShowDaysBefore then
        begin
          d := EncodeDate(pye, pmo, daysinmonth(pmo, pye) - (fd - i));
          ValueAssign := true;
        end;
      end
      else
      begin
        if ((i + (j - 1) * 7 - fd) > DaysInMonth(mo, ye)) then
        begin
          if FShowDaysAfter then
          begin
            d := EncodeDate(nye, nmo, i + (j - 1) * 7 - fd - daysinmonth(mo, ye));
            ValueAssign := true;
          end;
        end
        else
        begin
          d := EncodeDate(ye, mo, (i + (j - 1) * 7 - fd));
          ValueAssign := true;
        end;
      end;

      if ValueAssign and FirstTime then
      begin
        FromDate := d;
        FirstTime := false;
      end;
    end;

    ToDate := d;
    //--- Set Scroller Position
    FItemScrollerAry[j].Min := 0;
    //FItemScrollerAry[j].Max:= 0;
    //FItemScrollerAry[j].Position:= 0;
    if not FirstTime then
    begin
      MaxConfl := GetMaxConflict(FromDate, ToDate);
      MaxConfl := MaxConfl - MaxItemsDisplayed;
      if MaxConfl > 0 then
      begin
        FItemScrollerAry[j].Max := MaxConfl;
        FItemScrollerAry[j].Visible := true;
        if FItemScrollerAry[j].Position > FItemScrollerAry[j].Max then
          FItemScrollerAry[j].Position := FItemScrollerAry[j].Max;
        //FItemScrollerAry[j].Min:= 1;
        //FItemScrollerAry[j].Position:= FItemScrollerAry[j].Min;
      end
      else
      begin
        FItemScrollerAry[j].Max := 0;
        FItemScrollerAry[j].Position := 0;
      end;
    end;

  end;
end;


procedure TPlannerMonthView.CreateDragImage(APlannerItem: TPlannerItem);
var
  b: TBitmap;
begin
  FDragImage.Width := Width div 7;
  FDragImage.Height := Height div 6;

  b := TBitmap.Create;
  b.Width := FDragImage.Width;
  b.Height := FDragImage.Height+20;
  FPlanner.PreviewPaint(APlannerItem,b.Canvas,Rect(0,0,FDragImage.Width, FDragImage.Height), false, false);

  FDragImage.Clear;
  FDragImage.Add(b,nil);
  b.Free;
end;

{ TCalendarBrowsers }

procedure TCalendarBrowsers.Changed;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

constructor TCalendarBrowsers.Create;
begin
  inherited;
  FNextMonth := True;
  FNextYear := True;
  FPrevMonth := True;
  FPrevYear := True;
end;

procedure TCalendarBrowsers.SetNextMonth(const Value: Boolean);
begin
  FNextMonth := Value;
  Changed;
end;

procedure TCalendarBrowsers.SetNextYear(const Value: Boolean);
begin
  FNextYear := Value;
  Changed;
end;

procedure TCalendarBrowsers.SetPrevMonth(const Value: Boolean);
begin
  FPrevMonth := Value;
  Changed;
end;

procedure TCalendarBrowsers.SetPrevYear(const Value: Boolean);
begin
  FPrevYear := Value;
  Changed;
end;

{ TCustomMonthViewPanel }

procedure TCustomMonthViewPanel.DoPaint;
begin
end;

procedure TCustomMonthViewPanel.RepaintDate(dt: TDateTime);
begin
end;

procedure TCustomMonthViewPanel.UpdateYearStart;
begin
  Invalidate;
end;

procedure TPlannerMonthView.UpdateYearStartAtISO;
var
  dow: integer;
begin
  if not YearStartAt.FISOWeekNumber then
    Exit;

  dow := dayofweek(encodedate(Year,1,1));

  case dow of
  2,3,4,5:
    begin
      YearStartAt.FStartDay := 1;
      YearStartAt.FStartMonth := 1;
    end;
  6:begin
      YearStartAt.FStartDay := 4;
      YearStartAt.FStartMonth := 1;
    end;
  7:begin
      YearStartAt.FStartDay := 3;
      YearStartAt.FStartMonth := 1;
    end;
  1:begin
      YearStartAt.FStartDay := 2;
      YearStartAt.FStartMonth := 1;
    end;
  end;

  dow := dayofweek(encodedate(Year + 1,1,1));

  case dow of
  2,3,4,5:
    begin
      YearStartAt.FNextYearStartDay := 1;
      YearStartAt.FNextYearStartMonth := 1;
    end;
  6:begin
      YearStartAt.FNextYearStartDay := 4;
      YearStartAt.FNextYearStartMonth := 1;
    end;
  7:begin
      YearStartAt.FNextYearStartDay := 3;
      YearStartAt.FNextYearStartMonth := 1;
    end;
  1:begin
      YearStartAt.FNextYearStartDay := 2;
      YearStartAt.FNextYearStartMonth := 1;
    end;
  end;

  dow := dayofweek(encodedate(Year - 1,1,1));

  case dow of
  2,3,4,5:
    begin
      YearStartAt.FPrevYearStartDay := 1;
      YearStartAt.FPrevYearStartMonth := 1;
    end;
  6:begin
      YearStartAt.FPrevYearStartDay := 4;
      YearStartAt.FPrevYearStartMonth := 1;
    end;
  7:begin
      YearStartAt.FPrevYearStartDay := 3;
      YearStartAt.FPrevYearStartMonth := 1;
    end;
  1:begin
      YearStartAt.FPrevYearStartDay := 2;
      YearStartAt.FPrevYearStartMonth := 1;
    end;
  end;
end;

procedure TPlannerMonthView.SetCaptionColor(const Value: TColor);
begin
  if Value = FCaptionColor then exit;
  FCaptionColor := Value;
  Invalidate;
end;

{ TCalGlyphs }

procedure TCalGlyphs.Assign(Source: TPersistent);
begin
  if (Source is TCalGlyphs) then
  begin
    NextMonth := (Source as TCalGlyphs).NextMonth;
    PrevMonth := (Source as TCalGlyphs).PrevMonth;
    NextYear := (Source as TCalGlyphs).NextYear;
    PrevYear := (Source as TCalGlyphs).PrevYear;
  end;
end;

constructor TCalGlyphs.Create(AOwner: TComponent);
begin
  inherited Create;
  FOwner := AOwner;
  FNextYear := TBitmap.Create;
  FNextYear.OnChange := Changed;
  FPrevYear := TBitmap.Create;
  FPrevYear.OnChange := Changed;
  FNextMonth := TBitmap.Create;
  FNextMonth.OnChange := Changed;
  FPrevMonth := TBitmap.Create;
  FPrevMonth.OnChange := Changed;
end;

destructor TCalGlyphs.Destroy;
begin
  FNextYear.Free;
  FPrevYear.Free;
  FNextMonth.Free;
  FPrevMonth.Free;
  inherited;
end;

procedure TCalGlyphs.Changed(Sender: TObject);
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TCalGlyphs.SetNextMonth(const Value: TBitmap);
begin
  FNextMonth.Assign(Value);
  if Assigned(FOwner) then
  begin
    (FOwner as TControl).Invalidate;
  end;
end;

procedure TCalGlyphs.SetNextYear(const Value: TBitmap);
begin
  FNextYear.Assign(Value);
  if Assigned(FOwner) then
  begin
    (FOwner as TControl).Invalidate;
  end;
end;

procedure TCalGlyphs.SetPrevMonth(const Value: TBitmap);
begin
  FPrevMonth.Assign(Value);
  if Assigned(FOwner) then
  begin
    (FOwner as TControl).Invalidate;
  end;
end;

procedure TCalGlyphs.SetPrevYear(const Value: TBitmap);
begin
  FPrevYear.Assign(Value);
  if Assigned(FOwner) then
  begin
    (FOwner as TControl).Invalidate;
  end;
end;

procedure TPlannerMonthView.SetHintNextMonth(AValue: string);
begin
  FHintNextMonth := AValue;
end;

procedure TPlannerMonthView.SetHintNextYear(AValue: string);
begin
  FHintNextYear := AValue;
end;

procedure TPlannerMonthView.SetHintPrevMonth(AValue: string);
begin
  FHintPrevMonth := AValue;
end;

procedure TPlannerMonthView.SetHintPrevYear(AValue: string);
begin
  FHintPrevYear := AValue;
end;

procedure TPlannerMonthView.SetBorderXP(const Value: Boolean);
begin
  FBorderXP := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetShowLines(const Value: Boolean);
begin
  FShowLines := Value;
  Invalidate;
end;

procedure TPlannerMonthView.SetTrackColor(const Value: TColor);
begin
  if FTrackColor <> Value then
  begin
    FTrackColor := Value;
    Invalidate;
  end;
end;

procedure TPlannerMonthView.SetDefaultItem(const Value: TPlannerItem);
begin
  FDefaultItem.Assign(Value);
end;


function TPlannerMonthView.FindFirstItemAtDate(ADate: TDateTime): TPlannerItem;
var
  i: integer;
begin
  Result := nil;

  if not ShowDaysBefore and (aDate < EncodeDate(Year, Month,1)) then
    Exit;

  if not ShowDaysAfter and (aDate > EncodeDate(Year, Month, self.DaysInMonth(Month, Year))) then
    Exit;

  FFindIndex := 0;

  for i := 0 to FPlannerMonthItems.Count - 1 do
  begin
    if (int(aDate) >= int(TMonthPlannerItem(Items[i]).ItemStartTime)) and
       (int(aDate) <= int(TMonthPlannerItem(Items[i]).ItemEndTime)) and
       (TMonthPlannerItem(Items[i]).Visible) then
    begin
      Result := TMonthPlannerItem(items[i]);
      FFindIndex := i;
      break;
    end;
  end;
end;

function TPlannerMonthView.FindNextItemAtDate(ADate: TDateTime): TPlannerItem;
var
  i: integer;
begin
  Result := nil;

  if not ShowDaysBefore and (aDate < EncodeDate(Year, Month,1)) then
    Exit;

  if not ShowDaysAfter and (aDate > EncodeDate(Year, Month, self.DaysInMonth(Month, Year))) then
    Exit;

  for i := FFindIndex + 1 to FPlannerMonthItems.Count - 1 do
  begin
    if (int(aDate) >= int(TMonthPlannerItem(Items[i]).ItemStartTime)) and
       (int(aDate) <= int(TMonthPlannerItem(Items[i]).ItemEndTime)) and
       (TMonthPlannerItem(Items[i]).Visible) then
    begin
      Result := TMonthPlannerItem(items[i]);
      FFindIndex := i;
      break;
    end;
  end;
end;

function TPlannerMonthView.HasPlannerItem(FromDate: TDateTime; ToDate: TDateTime = 0): Boolean;
var
  ItemIndex: Integer;
  APlannerItem: TPlannerItem;
  FToDate: TDateTime;
begin
  Result := False;

  FToDate := ToDate;
  if FToDate = 0 then
    FToDate := FromDate;


  for ItemIndex := 0 to FPlannerMonthItems.Count - 1 do
  begin
    APlannerItem := Self.Items[ItemIndex];
    if (
      ((APlannerItem.ItemStartTime <= FromDate) and (APlannerItem.ItemEndTime >= FromDate)) or
      ((APlannerItem.ItemStartTime <= FToDate) and (APlannerItem.ItemEndTime >= FToDate)) or
      ((FromDate <= APlannerItem.ItemEndTime) and (FToDate >= APlannerItem.ItemEndTime)) or
      ((FromDate <= APlannerItem.ItemStartTime) and (FToDate >= APlannerItem.ItemStartTime))
      ) and (APlannerItem.Visible) then
    begin
      Result := true;
      Break;
    end;
  end;
end;

{ TPlannerMonthViewItems }

function TPlannerMonthViewItems.GetItemClass: TCollectionItemClass;
begin
  Result := TMonthPlannerItem;
end;

function TPlannerMonthViewItems.NumItemsAtDate(aDate: TDateTime): integer;
var
  i,res: integer;
begin

  res := 0;
  for i := 0 to Count - 1 do
  begin
    if (aDate >= int(TMonthPlannerItem(Items[i]).ItemStartTime)) and (aDate <= int(TMonthPlannerItem(Items[i]).ItemEndTime)) then
    begin
      inc(res);
    end;
  end;

  Result := res;
end;

function TPlannerMonthViewItems.FindItemAtDate(aDate: TDateTime; X,
  Y: integer): TPlannerItem;
var
  i, itemHeight: integer;
  p: TPoint;
  R: TRect;
  da: word;

begin
  Result := nil;

  if not FPlannerMonthView.ShowDaysBefore and (aDate < EncodeDate(FPlannerMonthView.Year,FPlannerMonthView.Month,1)) then
    Exit;

  da := PlanUtil.DaysInMonth(FPlannerMonthView.Month, FPlannerMonthView.Year);

  if not FPlannerMonthView.ShowDaysAfter and (aDate > EncodeDate(FPlannerMonthView.Year,FPlannerMonthView.Month, da)) then
    Exit;


  for i := 0 to Count - 1 do
  begin
    if (aDate >= int(TMonthPlannerItem(Items[i]).ItemStartTime)) and (aDate <= int(TMonthPlannerItem(Items[i]).ItemEndTime))
      and (TMonthPlannerItem(Items[i]).Visible) and
        InVisibleLayer(TMonthPlannerItem(Items[i]).Layer) then
    begin
      p := FPlannerMonthView.DateToXY(aDate);

      R.Right := P.X * FPlannerMonthView.dx + 2;
      R.Top := (P.Y - 1) * FPlannerMonthView.dy + FPlannerMonthView.yoffset - 2 + FPlannerMonthView.FWeekNameY + FPlannerMonthView.FDayFontHeight - 13; //25
      R.Bottom := R.Top + FPlannerMonthView.dy;
      R.Left := R.Right - FPlannerMonthView.dx - 4;
      R.Top := R.Top + FPlannerMonthView.FDayNumberHeight + 2;

      if FPlannerMonthView.ShowWeeks then
        OffsetRect(R, FPlannerMonthView.XOffset, 0);

      itemHeight := R.Bottom - R.Top;

      if min(FPlannerMonthView.FMaxItemsDisplayed, Items[i].Conflicts) > 0 then
        itemHeight := itemHeight div min(FPlannerMonthView.MaxItemsDisplayed, Items[i].Conflicts);

      if Assigned(FPlannerMonthView.GetItemScroller(P.y)) then
        R.Top := R.Top + (Items[i].ConflictPos - FPlannerMonthView.GetItemScroller(P.y).Position) * itemHeight
      else
        R.Top := R.Top + (Items[i].ConflictPos * itemHeight);

      R.Bottom := R.top + itemHeight - FPlannerMonthView.ItemSpace;

      if PtInRect(R, Point(X, Y)) then
        Result := items[i];

    end;
  end;
end;

procedure TPlannerMonthViewItems.FocusItem(APlannerItem: TPlannerItem);
var
  i: integer;
  AlreadySelected: Boolean;
begin
  if APlannerItem <> nil then
  begin
    for i := 0 to count - 1 do
    begin
      if (APlannerItem <> Items[i]) then
      begin
        Items[i].Focus := False;
        //if not Owner.MultiSelect then
        Items[i].Selected := False;
        if Assigned(FPlannerMonthView) then
          FPlannerMonthView.ItemUnSelected(APlannerItem);
      end;
    end;
    AlreadySelected := APlannerItem.Selected;
    APlannerItem.Focus := True;
    APlannerItem.Selected := True;
    Selected := APlannerItem;
    if Assigned(FPlannerMonthView) and not AlreadySelected then
      FPlannerMonthView.ItemSelected(APlannerItem);
  end;
end;

procedure TPlannerMonthViewItems.SetConflicts;
type
  tbp = record
    X, Y, Z: Byte;
  end;

  confarray = array of tbp; //array of array of tbp;

var
  conf: confarray;

  m, mm, n, i, j, k, l: Integer;
  itemA, itemB, PlannerEvent: TPlannerItem; //MonthPlannerEvent
  from, too: TDateTime;
  ye, mo, da: word;
  DysBtween, DisplayLen: Integer;

  function IsOverlapping(APlannerItem: TPlannerItem; ItemBegin: Integer): boolean;
  begin
    Result := False;
    if (TMonthPlannerItem(APlannerItem).BeginCell <= ItemBegin) and
      (TMonthPlannerItem(APlannerItem).EndCell > ItemBegin) and
       (APlannerItem.Visible) {and
       not (APlannerItem.Background and APlannerItem.AllowOverlap)} and
       (InVisibleLayer(APlannerItem.Layer))then
    begin
      Result := True;
    end;
  end;

begin
  if Count <= 0 then //(Items.Count == 0)
    Exit;

  if FPlannerMonthView.FUpdateCount > 0 then
    Exit;

  DecodeDate(Now, ye, mo, da);
  mo := mo - 1;

  //from := EncodeDate(ye, mo, 1);
  //too:= EncodeDate(ye, mo, daysinmonth(mo, ye));
  FPlannerMonthView.GetStartDate(da, mo, ye);
  from := EncodeDate(ye, mo, da);
  FPlannerMonthView.GetEndDate(da, mo, ye);
  too := EncodeDate(ye, mo, da);

  dysBtween := DaysBetween(from, too);

  for l := 0 to Count - 1 do //(MonthPlannerEvent plannerEvent in Items)
  begin

    PlannerEvent := Items[l];

    if not PlannerEvent.Visible then Continue;

    if not InvisibleLayer(PlannerEvent.Layer) then Continue;

    if (too < TMonthPlannerItem(plannerEvent).ItemStartTime) then //(DateTime.Compare(to.Date,plannerEvent.StartTime.Date)<0)
    begin
      TMonthPlannerItem(plannerEvent).BeginExt := -1;
      TMonthPlannerItem(plannerEvent).EndExt := -1;
    end
    else
    begin
      if (TMonthPlannerItem(PlannerEvent).ItemEndTime < from) then //(DateTime.Compare(plannerEvent.EndTime,from.Date) < 0)
      begin
        TMonthPlannerItem(plannerEvent).BeginExt := -1;
        TMonthPlannerItem(plannerEvent).EndExt := -1;
      end
      else
      begin
        if false {PlannerEvent.EndTime< EncodeDate(ye, mo, 1)} then //(DateTime.Compare(plannerEvent.EndTime,new DateTime(this.Year,this.Month,1)) < 0)
        begin
          TMonthPlannerItem(plannerEvent).BeginExt := -1;
          TMonthPlannerItem(plannerEvent).EndExt := -1;
        end
        else
        begin
          if false {PlannerEvent.StartTime> EncodeDate(ye, mo, daysinmonth(mo, ye))} then //(DateTime.Compare(plannerEvent.StartTime,new DateTime(this.Year,this.Month,DateTime.DaysInMonth(this.Year,this.Month))) > 0)
          begin
            TMonthPlannerItem(plannerEvent).BeginExt := -1;
            TMonthPlannerItem(plannerEvent).EndExt := -1;
          end
          else
            if (TMonthPlannerItem(PlannerEvent).ItemStartTime >= from) and (TMonthPlannerItem(PlannerEvent).ItemEndTime <= too) then
                  //((DateTime.Compare(plannerEvent.StartTime.Date,from.Date) >= 0) &&
                  //(DateTime.Compare(plannerEvent.EndTime.Date,to.Date) <=0))
            begin
             { ts = plannerEvent.StartTime.Date.Subtract(from.Date);
              plannerEvent.BeginExt = (int) ts.TotalDays;
              ts = plannerEvent.EndTime.Date.Subtract(from.Date);
              plannerEvent.EndExt = (int) ts.TotalDays  + 1;}
              TMonthPlannerItem(PlannerEvent).BeginExt := DaysBetween(from, TMonthPlannerItem(PlannerEvent).ItemStartTime);
              TMonthPlannerItem(PlannerEvent).EndExt := DaysBetween(from, TMonthPlannerItem(PlannerEvent).ItemEndTime) + 1;
            end
            else
            begin
              if (TMonthPlannerItem(PlannerEvent).ItemStartTime <= from) and (TMonthPlannerItem(PlannerEvent).ItemEndTime <= too) then
                 //((DateTime.Compare(plannerEvent.StartTime.Date,from.Date) <= 0) &&
                 //(DateTime.Compare(plannerEvent.EndTime.Date,to.Date) <= 0))
              begin
               { plannerEvent.BeginExt = 0;
                ts = plannerEvent.EndTime.Date.Subtract(from.Date);
                plannerEvent.EndExt = (int) ts.TotalDays + 1;   }
                TMonthPlannerItem(plannerEvent).BeginExt := 0;
                TMonthPlannerItem(PlannerEvent).EndExt := DaysBetween(from, TMonthPlannerItem(PlannerEvent).ItemEndTime) + 1;
              end
              else
              begin
                if (TMonthPlannerItem(PlannerEvent).ItemStartTime <= from) and (TMonthPlannerItem(PlannerEvent).ItemEndTime >= too) then
                   //((DateTime.Compare(plannerEvent.StartTime.Date,from.Date) <= 0) &&
                   //(DateTime.Compare(plannerEvent.EndTime.Date,to.Date) >= 0))
                begin
                  {plannerEvent.BeginExt = 0;
                  plannerEvent.EndExt = (int) daysBetween.TotalDays + 1;}
                  TMonthPlannerItem(PlannerEvent).BeginExt := 0;
                  TMonthPlannerItem(PlannerEvent).EndExt := DysBtween + 1;
                end
                else
                begin
                  if (TMonthPlannerItem(PlannerEvent).ItemStartTime >= from) and (TMonthPlannerItem(PlannerEvent).ItemEndTime >= too) then
                     //((DateTime.Compare(plannerEvent.StartTime.Date,from.Date) >= 0) &&
                     //(DateTime.Compare(plannerEvent.EndTime.Date,to.Date) >=0))
                  begin
                   { ts = plannerEvent.StartTime.Date.Subtract(from.Date);
                    plannerEvent.BeginExt = (int) ts.TotalDays;
                    plannerEvent.EndExt = (int) daysBetween.TotalDays + 1; }
                    TMonthPlannerItem(PlannerEvent).BeginExt := DaysBetween(from, TMonthPlannerItem(PlannerEvent).ItemStartTime);
                    TMonthPlannerItem(PlannerEvent).EndExt := DysBtween + 1;
                  end;
                end;
              end;
            end;
        end;
      end;
    end;
    TMonthPlannerItem(plannerEvent).BeginCell := TMonthPlannerItem(plannerEvent).BeginExt;
    TMonthPlannerItem(plannerEvent).EndCell := TMonthPlannerItem(plannerEvent).EndExt;
  end;
//  }

  // Calculate worst-case item overlap count
  for i := 0 to Count - 1 do
  begin
    m := 0;
    itemA := Items[i];

    if not itemA.Visible then Continue;

    if not InVisibleLayer(itemA.Layer) then Continue;


    if (TMonthPlannerItem(ItemA).BeginExt <> -1) {and Canbedisplayed} then //((itemA.FBeginExt != -1) && (itemA.FBeginExt != -1) && (itemA.CanBeDisplayed()))
    begin
      for j := 0 to Count - 1 do
      begin
        itemB := Items[j];

        if (i <> j) then
        begin
          for k := TMonthPlannerItem(itemA).BeginCell to TMonthPlannerItem(itemA).EndCell - 1 do
          begin
            if (IsOverlapping(itemB, k)) then
            begin
              if (TMonthPlannerItem(itemB).BeginExt < TMonthPlannerItem(itemA).BeginExt) then
                TMonthPlannerItem(itemA).BeginExt := TMonthPlannerItem(itemB).BeginExt
              else
                TMonthPlannerItem(itemB).BeginExt := TMonthPlannerItem(itemA).BeginExt;

              if (TMonthPlannerItem(itemB).EndExt > TMonthPlannerItem(itemA).EndExt) then
                TMonthPlannerItem(itemA).EndExt := TMonthPlannerItem(itemB).EndExt
              else
                TMonthPlannerItem(itemB).EndExt := TMonthPlannerItem(itemA).EndExt;
              m := m + 1;
              break;
            end;
          end;
        end;
      end;
      TMonthPlannerItem(itemA).SetConflicts(m + 1);
    end;
  end;

  displayLen := 42; // too-from +1;

{Clear counters of overlapping Items}
  SetLength(conf, displayLen);


  for j := 0 to displayLen - 1 do
  begin
    conf[j].X := 0;
    conf[j].Y := 0;
    conf[j].Z := 0;
  end;

  //Count. nr of overlapping Items per cell}
  for l := 0 to Count - 1 do //(MonthPlannerEvent item in Items)
  begin
    itemA := Items[l];
    if not itemA.Visible then Continue;
    if not InVisibleLayer(itemA.Layer) then Continue;

    if ((TMonthPlannerItem(items[l]).BeginExt <> -1) and (TMonthPlannerItem(items[l]).EndExt <> -1) {&& (items[l].CanBeDisplayed())}) then
    begin
      for j := TMonthPlannerItem(items[l]).BeginExt to TMonthPlannerItem(items[l]).EndExt - 1 do
      begin
        if (items[l].Conflicts > conf[j].X) then
        begin
          conf[j].X := items[l].Conflicts;
        end;
      end;
    end;
  end;

  // Assign Items conflicts & conflict positions
  for l := 0 to Count - 1 do //(MonthPlannerEvent item in Items)
  begin
    itemA := Items[l];
    if not itemA.Visible then Continue;
    if not InVisibleLayer(itemA.Layer) then Continue;
  
    if ((TMonthPlannerItem(items[l]).BeginExt <> -1) and (TMonthPlannerItem(items[l]).EndExt <> -1) {&& (items[l].CanBeDisplayed())}) then
    begin
      m := 0;
      mm := 0;

      // get last assigned conflict position
      for j := TMonthPlannerItem(items[l]).BeginCell to TMonthPlannerItem(items[l]).EndCell - 1 do
      begin
        if (conf[j].Y > m) then
          m := conf[j].Y;

        if (conf[j].Z > mm) then
          mm := conf[j].Z;
      end;
      if (mm = m) then
        mm := 0;

      // get nr. of conflicts from extended zone
      n := 0;

      for j := TMonthPlannerItem(items[l]).BeginExt to TMonthPlannerItem(items[l]).EndExt - 1 do
      begin
        if (conf[j].X > n) then
          n := conf[j].X;
      end;

      TMonthPlannerItem(items[l]).SetConflicts(n);
      if (m < items[l].Conflicts) then
      begin
        TMonthPlannerItem(items[l]).SetConflictPos(m);
      end
      else
      begin
        TMonthPlannerItem(items[l]).SetConflictPos(mm);
      end;

      if (m >= items[l].Conflicts) then
      begin
        mm := mm + 1;
        for j := TMonthPlannerItem(items[l]).BeginCell to TMonthPlannerItem(items[l]).EndCell - 1 do
        begin
          conf[j].Z := mm;
        end;
      end;

      if (m < items[l].Conflicts) then
        m := m + 1;

      for j := TMonthPlannerItem(items[l]).BeginCell to TMonthPlannerItem(items[l]).EndCell - 1 do
      begin
        conf[j].Y := m;
      end;
    end;
  end;

  // If better than worst case position found, optimize
  for l := 0 to Count - 1 do //(MonthPlannerEvent item in Items)
  begin
    itemA := Items[l];
    if not itemA.Visible then Continue;
    if not InVisibleLayer(itemA.Layer) then Continue;

    if ((TMonthPlannerItem(items[l]).BeginExt <> -1) and (TMonthPlannerItem(items[l]).EndExt <> -1) {&& (item.CanBeDisplayed())}) then
    begin
      m := 0;
      for j := 0 to displayLen - 1 do
      begin
        if (conf[j].Y > m) then
          m := conf[j].Y;
      end;
      if (items[l].Conflicts > m) then
      begin
        TMonthPlannerItem(items[l]).SetConflicts(m);
      end;
    end
    else
    begin
      TMonthPlannerItem(items[l]).SetConflicts(1);
      TMonthPlannerItem(items[l]).SetConflictPos(0);
    end;
  end;
  FPlannerMonthView.SetItemScrollerPosition;
end;

function TPlannerMonthViewItems.HasMonthPlannerItem(FromDate,
  ToDate: TDateTime): Boolean;
var
  ItemIndex: Integer;
  APlannerItem: TPlannerItem;
begin
  Result := False;

  for ItemIndex := 0 to Self.Count - 1 do
  begin
    APlannerItem := Self.Items[ItemIndex];
    if (
      ((APlannerItem.ItemStartTime <= FromDate) and (APlannerItem.ItemEndTime >= {} FromDate)) or
      ((APlannerItem.ItemStartTime <= {} ToDate) and (APlannerItem.ItemEndTime >= ToDate)) or
      ((FromDate <= {} APlannerItem.ItemEndTime) and (ToDate >= APlannerItem.ItemEndTime)) or
      ((FromDate <= APlannerItem.ItemStartTime) and (ToDate >= {} APlannerItem.ItemStartTime))
      ) and
      (APlannerItem.Visible) and (APlannerItem <> Selected)
      and (InVisibleLayer(APlannerItem.Layer)) then
    begin
      Result := (not APlannerItem.AllowOverlap) or (not FPlannerMonthView.FOverlap);
      if Result then
        Break;
    end;
  end;
end;


{TMonthPlannerRichEdit}

procedure TMonthPlannerRichEdit.DoEnter;
begin
  inherited;
  SelStart := 0;
  SelLength := $FFFF;
end;

procedure TMonthPlannerRichEdit.DoExit;
begin
  inherited;
  if Assigned(PlannerItem) then
  begin
    PlannerItem.Text.Text := TPlannerMonthView(Parent).RichToText;
//    TPlannerMonthView(Parent).ItemEdited(PlannerItem);
  end;
  Self.Visible := False;
  Self.Parent.SetFocus;
end;

procedure TMonthPlannerRichEdit.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited;
  if Key = vk_Escape then
  begin
    TPlannerMonthView(Parent).TextToRich(PlannerItem.Text.Text);
    self.Visible := False;
    TPlannerMonthView(Parent).SetFocus;
  end;
end;

//------------------------------------------------------------------------------
//----------------------------{ TItemScroller }---------------------------------
//------------------------------------------------------------------------------

function TItemScroller.CanGoBack: Boolean;
begin
  Result := Position > Min;
end;

//------------------------------------------------------------------------------

function TItemScroller.CanGoForward: Boolean;
begin
  Result := Position < Max;
end;

//------------------------------------------------------------------------------

constructor TItemScroller.Create;
begin
  inherited;
  FMin := 0;
  FMax := 0;
  FPosition := 0;
  FVisible := false;
end;

//------------------------------------------------------------------------------

procedure TItemScroller.SetMax(const Value: integer);
begin
  if Value >= FMin then FMax := Value;
end;

//------------------------------------------------------------------------------

procedure TItemScroller.SetMin(const Value: integer);
begin
  if Value <= FMax then FMin := Value;
end;

//------------------------------------------------------------------------------

procedure TItemScroller.SetPosition(const Value: integer);
begin
  if (Value >= Min) and (Value <= Max) then
    FPosition := Value;
end;

//------------------------------------------------------------------------------

procedure TItemScroller.SetVisible(const Value: Boolean);
begin
  FVisible := Value;
end;

{ TPlannerMonthViewPrintOptions }

constructor TPlannerMonthViewPrintOptions.Create;
begin
  inherited Create;
  FHeaderFont := TFont.Create;
  FFooterFont := TFont.Create;
  FFooter := TStringList.Create;
  FHeader := TStringList.Create;
end;

destructor TPlannerMonthViewPrintOptions.Destroy;
begin
  FHeader.Free;
  FFooter.Free;
  FHeaderFont.Free;
  FFooterFont.Free;
  inherited;
end;

procedure TPlannerMonthViewPrintOptions.SetFooter(const Value: TStrings);
begin
  if Assigned(Value) then
    FFooter.Assign(Value);
end;

procedure TPlannerMonthViewPrintOptions.SetFooterFont(const Value: TFont);
begin
  if Assigned(Value) then
    FFooterFont.Assign(Value);
end;

procedure TPlannerMonthViewPrintOptions.SetHeader(const Value: TStrings);
begin
  if Assigned(Value) then
    FHeader.Assign(Value);
end;

procedure TPlannerMonthViewPrintOptions.SetHeaderFont(const Value: TFont);
begin
  if Assigned(Value) then
    FHeaderFont.Assign(Value);
end;

{$IFDEF FREEWARE}
function Scramble(s:string): string;
var
  r:string;
  i: integer;
  c: char;
  b: byte;
begin
  r := '';
  for i := 1 to length(s) do
  begin
    b := ord(s[i]);
    b := (b and $E0) + ((b and $1F) xor 5);
    c := chr(b);
    r := r + c;
  end;
  Result := r;
end;
{$ENDIF}

initialization
  {$IFDEF ISDELPHI}
  try
    RegisterClass(TPlannerMonthView);
  except
  end;
  {$ENDIF}  

  {$IFDEF FREEWARE}
  if (FindWindow(PChar(Scramble('QDuuilfdqljk')), nil) = 0) OR
     (FindWindow(PChar(Scramble('QDuuGplia`w')), nil) = 0) then
  begin
    MessageBox(0,PChar(Scramble('Duuilfdqljk%pv`v%qwldi%s`wvljk%jc%QHV%vjcqrdw`%fjhujk`kqv+')+#13#10+Scramble('Fjkqdfq%QHV%vjcqrdw`%mqqu?**rrr+qhvvjcqrdw`+fjh%cjw%sdila%ilf`kvlkb+')),PChar(Scramble('Rdwklkb')),MB_OK);
  end
  {$ENDIF}

end.

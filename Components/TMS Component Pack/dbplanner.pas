{$I TMSDEFS.INC}
{***********************************************************************}
{ TDBPlanner component                                                  }
{ for Delphi & C++Builder                                               }
{                                                                       }
{ written by TMS Software                                               }
{            copyright © 1999-2015                                      }
{            Email: info@tmssoftware.com                                }
{            Web: http://www.tmssoftware.com                            }
{                                                                       }
{ The source code is given as is. The author is not responsible         }
{ for any possible damage done due to the use of this code.             }
{ The component can be freely used in any application. The complete     }
{ source code remains property of the author and may not be distributed,}
{ published, given or sold in any form as such. No parts of the source  }
{ code can be included in any other component or application without    }
{ written authorization of the author.                                  }
{***********************************************************************}

                                                               
unit DBPlanner;

interface

uses
  Planner, Classes, DB, Windows, SysUtils, Graphics, Dialogs, ComObj,
  ActiveX, PlanRecurr;


const
  CDaysInMonth:array[1..12] of word = (31,28,31,30,31,30,31,31,30,31,30,31);

  MAJ_VER = 3; // Major version nr.
  MIN_VER = 4; // Minor version nr.
  REL_VER = 3; // Release nr.
  BLD_VER = 0; // Build nr.
  DATE_VER = 'Sep, 2015'; // Month version

  // version history
  // v2.2.1.1 : Fixed issue with monthly recurrency on fixed dates
  // v2.2.1.3 : Fixed issue with TDBDaySource active time display
  // v2.2.1.4 : TPlanner compatibility update
  // v2.2.1.5 : TPlanner compatibility update
  // v2.2.1.6 : Improved recurrency handling
  // v2.2.1.7 : Fix in TDBDaySource multiday items realtime setting
  // v2.5.0.0 : See separate release notes
  // v2.5.0.14: Fixed issue with changing active record for recurrent items
  // v2.5.0.15: Fixed issue with items after midnight in multiday scenarios
  // v2.5.0.16: Fixed issue with MultiMonthSource and items longer than viewed timespan.
  // v2.5.1.4 : Fixed : issue with CtrlDragCopy & TDBPlanner
  // v2.5.2.0 : TPlanner compatibility update
  // v2.5.3.4 : TPlanner compatibility update
  // v2.5.3.5 : Fixed : issue with AutoHeaderUpdate for TDBDisjunctDaySource
  // v2.5.6.1 : TPlanner compatibility update
  // v2.5.6.2 : Fixed : issue with ResourceDataSource updating
  // v2.5.6.6 : Fixed : issues with specific yearly recurrence types
  //          : Improved : exception handling with datasets
  // v2.5.6.7 : Fixed : issue with SelectionToAbsTime()
  // v2.5.6.8 : TPlanner compatibility update
  // v2.5.6.9 : TPlanner compatibility update
  // v2.5.6.10: TPlanner compatibility update
  // v2.5.6.13: TPlanner compatibility update
  // v2.5.6.24: TPlanner compatibility update
  // v2.5.6.27: TPlanner compatibility update
  // v2.5.7.1 : Fixed : issue with showing recurrent items with specific DisplayStart/DisplayEnd settings
  // v2.5.8.2 : TPlanner compatibility update
  // v2.5.8.3 : Fixed : issue with day-crossing in multiday/resource mode
  // v2.5.8.4 : Fixed : issue with OnDeleteItem in TDBTimeLineSource

  // v3.0.0.0 : New : HTMLTemplate handling
  //          : New : TDBActiveDayPeriodSource added
  //          : New : MonthIncrement property added in TDBMultiMonthSource
  //          : New : OnGetHTMLTemplate, OnGetHTMLTemplateData events added
  //          : New : Memo field support for HTML templates
  //          : Improved : Performance for retrieving records from the database
  //          : Improved : Handling of DB fields using OnGetText

  // v3.0.0.3 : Improved : Check if NotesField, SubjectField, KeyField is used added
  // v3.0.0.4 : Fixed : Issue with adding items with date in plDay mode with date overlapping time axis
  // v3.0.1.0 : New : Multi resource disjunct day views
  // v3.0.3.0 : New : Support added for import & export to Google & Windows Live Calendar via TMS Cloud Pack
  // v3.0.4.0 : New : Delphi XE5 & C++Builder XE5 support
  // v3.0.4.1 : Fixed : Issue with drag & drop of positions and resourcemap
  // v3.1.0.0 : New : Inherits TPlanner 3.1 features
  //          : New : Tag property added in TResourceMapItem
  // v3.1.0.1 : Fixed : Issue with resource loadig in the form create
  // v3.1.0.2 : Fixed : Issue with DBDaySource and SideBar positioned spRight
  // v3.3.0.0 : New : PDF export added
  // v3.4.0.0 : New : TPlannerCalendar : highly configurable non DB-aware calendar component
  //          : New : TDBPlannerCalendar : DB-aware calendar component that can automatically show events from a TMS Planner events database
  //          : New : TPlannerCalendarGroup : multimonth calendar component
  //          : New : TPlannerDatePicker : datepicker with highly configurable calendar dropdown
  //          : New : TDBPlannerDatePicker : DB-aware datepicker with calendar dropdown that can automatically show events from a TMS Planner events database
  //          : New : TDBPlannerMaskDatePicker : masked DB-aware datepicker with calendar dropdown that can automatically show events from a TMS Planner events database
  //          : New : TPlannerDBDatePicker : single DB-field DB-aware datepicker with support for null-date entry
  //          : New : TPlannerDBMaskDatePicker : masked single DB-field DB-aware datepicker with support for null-date entry
  //          : New : TPlannerRangeSelector : control to input a date range, going from a start date to an end date
  // v3.4.1.0 : New : Support for Delphi XE8 & C++Builder XE8 Prof, Ent. Architect added
  //          : Fixed : Issue with range selection in TPlannerCalendarGroup
  //          : Fixed : Issue with OnDayChange event in special cases in TPlannerCalendarGroup
  // v3.4.1.1 : Fixed : Issue with start time handling for multiday mode
  // v3.4.2.0 : TPlanner compatibility update
  // v3.4.2.1 : Fixed : Issue with TDBActiveDaySource is position span calculation
  // v3.4.3.0 : New : RAD Studio 10 Seattle support

type
  TDBItemSource = class;

  {$IFDEF DELPHIXE2_LVL}
  TTagInt = nativeint;
  {$ENDIF}
  {$IFNDEF DELPHIXE2_LVL}
  TTagInt = integer;
  {$ENDIF}


  TResourceMapItem = class(TCollectionItem)
  private
    FResourceIndex: Integer;
    FPositionIndex: Integer;
    FDisplayName: string;
    FTag: TTagInt;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
  published
    property ResourceIndex: Integer read FResourceIndex write FResourceIndex;
    property PositionIndex: Integer read FPositionIndex write FPositionIndex;
    property DisplayName: string read FDisplayName write FDisplayName;
    property Tag: TTagInt read FTag write FTag;
  end;

  TResourceMap = class(TCollection)
  private
    FOwner: TComponent;
    function GetItem(Index: Integer): TResourceMapItem;
    procedure SetItem(Index: Integer; const Value: TResourceMapItem);
  protected
  public
    function Add: TResourceMapItem;
    function Insert(index: Integer): TResourceMapItem;
    property Items[Index: Integer]: TResourceMapItem read GetItem write SetItem; default;
    constructor Create(AOwner: TComponent);
    function GetOwner: TPersistent; override;
    function ResToPosition(Value: Integer): Integer;
    function PositionToRes(Value: Integer): Integer;
    function PositionName(Value: Integer): string;
  end;

  TResourceDataSource = class(TPersistent)
  private
    FDBItemSource: TDBItemSource;
    FResourceIDField: string;
    FResourceNameField: string;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
  public
    constructor Create(AOwner: TDBItemSource);
    procedure Assign(Source: TPersistent); override;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ResourceIDField: string read FResourceIDField write FResourceIDField;
    property ResourceNameField: string read FResourceNameField write FResourceNameField;
  end;

  TCreateKeyEvent = procedure(Sender:TObject; APlannerItem: TPlannerItem; var Key:string) of object;

  TAcceptEvent = procedure(Sender:TObject; Fields:TFields; var Accept: Boolean) of object;

  TDBItemSQLEvent = procedure(Sender:TObject; APlannerItem:TPlannerItem) of object;

  THTMLTemplateEvent = procedure(Sender:TObject; APlannerItem: TPlannerItem; var HTMLTemplate: string) of object;
  THTMLTemplateDataEvent = procedure(Sender:TObject; APlannerItem: TPlannerItem; Field: TField; HTMLTag: string; var Value: string) of object;

  TItemSource = class(TComponent)
  private
    FPlanner: TPlanner;
    FUpdateCount: Integer;
    FOnCreateKey: TCreateKeyEvent;
    FActive: Boolean;
    FOnSetFilter: TNotifyEvent;
    FOnResetFilter: TNotifyEvent;
    FOnAccept: TAcceptEvent;
    FLocateIdx: Integer;
    FLocateSpecifier: string;
    FLocateParams: TFindTextParams;
    procedure SetPlanner(Value: TPlanner);
    procedure SetActive(const Value: Boolean);
  protected
  public
    procedure ClearDBItems; virtual;
    procedure ReadDBItems(UpdateKey: string); virtual;
    procedure WriteDBItem; virtual;
    procedure ReadDBItem; virtual;
    procedure SynchDBItems; virtual;
    procedure AddDBItem; virtual;
    procedure GotoDBItem; virtual;
    procedure DeleteDBItem(APlanner:TPlanner); virtual;
    procedure ItemChanged(DBKey:string); virtual;
    procedure Refresh; virtual;
    procedure Reload; virtual;
    procedure Next; virtual;
    procedure Prev; virtual;
    constructor Create(AOwner:TComponent); override;
    destructor Destroy; override;
    procedure PlannerChanged; virtual;
    property Planner: TPlanner read FPlanner write SetPlanner;
    procedure BeginUpdate;
    procedure EndUpdate;
    function IsUpdating: Boolean;
    function CreateKey:string; virtual;
    function LocateFirstItem(Specifier: string; Params: TFindTextParams;
      var StartTime,EndTime: TDateTime;var Key:string): Boolean; virtual;
    function LocateNextItem(var StartTime,EndTime: TDateTime;
      var Key:string):Boolean; virtual;
    function PosToDay(Pos: Integer): TDateTime; virtual;
    function PosToRes(Pos: Integer): Integer; virtual;
    procedure MapItemTimeOnPlanner(APlannerItem: TPlannerItem); virtual;
  published
    property Active: Boolean read FActive write SetActive default true;
    property OnCreateKey: TCreateKeyEvent read FOnCreateKey write FOnCreateKey;
    property OnResetFilter: TNotifyEvent read FOnResetFilter write FOnResetFilter;
    property OnSetFilter: TNotifyEvent read FOnSetFilter write FOnSetFilter;
    property OnAccept: TAcceptEvent read FOnAccept write FOnAccept;
  end;

  TDBPlannerDataLink = class(TDataLink)
  private
    FDBItemSource: TDBItemSource;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure RecordChanged(Field: TField); override;
  public
    constructor Create(ADBItemSource: TDBItemSource);
    destructor Destroy; override;
  end;

  TDBPlannerResourceDataLink = class(TDataLink)
  private
    FDBItemSource: TDBItemSource;
    FUpdateCount: Integer;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateResourceMap;
  public
    constructor Create(ADBItemSource: TDBItemSource);
    destructor Destroy; override;
  end;

  TFieldsItemEvent = procedure(Sender: TObject; Fields:TFields; Item: TPlannerItem) of object;

  TResourceToPosEvent = procedure(Sender: TObject; Field: TField;
    var Position: Integer; var Accept: Boolean) of object;

  TPosToResourceEvent = procedure(Sender: TObject; Field: TField;
    Position: Integer) of object;

  TFieldsToTimeEvent = procedure(Sender: TObject; Fields:TFields;
    var dtS: TDateTime; var dtE: TDateTime) of object;

  TTimeToFieldsEvent = procedure(Sender: TObject; Fields:TFields;
    dtS: TDateTime; dtE: TDateTime) of object;

  TDBItemSource = class(TItemSource)
  private
    FEndTimeField: string;
    FStartTimeField: string;
    FKeyField: string;
    FResourceField: string;
    FSubjectField: string;
    FNotesField: string;
    FRecurrencyField: string;
    FMinTimeField: string;
    FMaxTimeField: string;
    FAutoIncKey: Boolean;
    FDataLink: TDBPlannerDataLink;
    FResourceDataLink: TDBPlannerResourceDataLink;
    FOnFieldsToItem: TFieldsItemEvent;
    FOnItemToFields: TFieldsItemEvent;
    FOnChangeQuery: TNotifyEvent;
    FOnPositionToResource: TPosToResourceEvent;
    FOnResourceToPosition: TResourceToPosEvent;
    FReadOnly: Boolean;
    FNotesDBField: TField;
    FSubjectDBField: TField;
    FDBKeyDBField: TField;
    FOnInsertItem: TDBItemSQLEvent;
    FOnDeleteItem: TDBItemSQLEvent;
    FOnUpdateItem: TDBItemSQLEvent;
    FOnItemsRead: TNotifyEvent;
    FOnBeforeItemsRead: TNotifyEvent;
    FOnFieldsToTime: TFieldsToTimeEvent;
    FOnTimeToFields: TTimeToFieldsEvent;
    FUpdateByQuery: Boolean;
    FResourceMap: TResourceMap;
    FRecurrencyHandler: TRecurrencyHandler;
    FResourceDataSource: TResourceDataSource;
    FOnResourceChange: TNotifyEvent;
    function GetDataSource: TDataSource;
    procedure SetDataSource(const Value: TDataSource);
    procedure SetEndTimeField(const Value: string);
    procedure SetStartTimeField(const Value: string);
    procedure SetKeyField(const Value: string);
    procedure SetNotesField(const Value: string);
    procedure SetSubjectField(const Value: string);
    procedure SetResourceField(const Value: string);
    procedure SetRecurrencyField(const Value: string);
    procedure SetMinTimeField(const Value: string);
    procedure SetMaxTimeField(const Value: string);
    procedure SetReadOnly(const Value: Boolean);
    procedure SetResourceMap(const Value: TResourceMap);
    procedure SetResourceDataSource(const Value: TResourceDataSource);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    function CheckDataSet: Boolean;
    procedure MoveResource(FromPos, ToPos: Integer); virtual;
    procedure ResourceUpdate; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Next; override;
    procedure Prev; override;
    function LocateFirstItem(Specifier: string; Params: TFindTextParams;
      var StartTime,EndTime: TDateTime;var Key:string): Boolean; override;
    function LocateNextItem(var StartTime,EndTime: TDateTime; var Key:string):Boolean; override;
    procedure FieldsToItem(APlannerItem: TPlannerItem); virtual;
    procedure FieldsToTime(Fields:TFields; var dtS: TDateTime; var dtE: TDateTime); virtual;
    procedure TimeToFields(Fields:TFields; dtS: TDateTime; dtE: TDateTime); virtual;
    procedure FieldsToRes(Fields: TFields; var Position: Integer; var Accept: Boolean); virtual;
    procedure ResToFields(Fields: TFields; Position: Integer); virtual;
    property RecurrencyField: string read FRecurrencyField write SetRecurrencyField;
    property MaxTimeField: string read FMaxTimeField write SetMaxTimeField;
    property MinTimeField: string read FMinTimeField write SetMinTimeField;
  published
    property AutoIncKey: Boolean read FAutoIncKey write FAutoIncKey;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ResourceDataSource: TResourceDataSource read FResourceDataSource write SetResourceDataSource;
    property ResourceMap: TResourceMap read FResourceMap write SetResourceMap;
    property StartTimeField: string read FStartTimeField write SetStartTimeField;
    property EndTimeField: string read FEndTimeField write SetEndTimeField;
    property KeyField: string  read FKeyField write SetKeyField;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly;
    property ResourceField: string read FResourceField write SetResourceField;
    property SubjectField: string read FSubjectField write SetSubjectField;
    property NotesField: string read FNotesField write SetNotesField;
    property UpdateByQuery: Boolean read FUpdateByQuery write FUpdateByQuery;
    property OnBeforeItemsRead: TNotifyEvent read FOnBeforeItemsRead write FOnBeforeItemsRead;
    property OnFieldsToItem: TFieldsItemEvent read FOnFieldsToItem write FOnFieldsToItem;
    property OnItemToFields: TFieldsItemEvent read FOnItemToFields write FOnItemToFields;
    property OnChangeQuery: TNotifyEvent read FOnChangeQuery write FOnChangeQuery;
    property OnResourceToPosition: TResourceToPosEvent read FOnResourceToPosition
      write FOnResourceToPosition;
    property OnPositionToResource: TPosToResourceEvent read FOnPositionToResource
      write FOnPositionToResource;
    property OnInsertItem: TDBItemSQLEvent read FOnInsertItem write FOnInsertItem;
    property OnDeleteItem: TDBItemSQLEvent read FOnDeleteItem write FOnDeleteItem;
    property OnUpdateItem: TDBItemSQLEvent read FOnUpdateItem write FOnUpdateItem;
    property OnItemsRead: TNotifyEvent read FOnItemsRead write FOnItemsRead;
    property OnFieldsToTime: TFieldsToTimeEvent read FOnFieldsToTime write FOnFieldsToTime;
    property OnTimeToFields: TTimeToFieldsEvent read FOnTimeToFields write FOnTimeToFields;
    property OnResourceChange: TNotifyEvent read FOnResourceChange write FOnResourceChange;
  end;

  TDBDayMode = (dmMultiDay, dmMultiResource, dmMultiDayRes, dmMultiResDay);

  TResourceNameEvent = procedure(Sender: TObject; ResourceIndex: Integer; var ResourceName: string) of object;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBDaySource = class(TDBItemSource)
  private
    FMode: TDBDayMode;
    FDayIncrement: Integer;
    FDay: TDateTime;
    FAutoHeaderUpdate: Boolean;
    FDateFormat: string;
    FNumberOfDays: Integer;
    FNumberOfResources: Integer;
    FOnGetResourceName: TResourceNameEvent;
    procedure SetDay(const Value: TDateTime);
    procedure SetMode(const Value: TDBDayMode);
    procedure SetNumberOfDays(const Value: Integer);
    procedure SetNumberOfResources(const Value: Integer);
    procedure ConfigurePlanner(var Span: Integer);
    function CalcItemPos(DatePos,ResourcePos: Integer): Integer;
  protected
    procedure ResourceUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure Next; override;
    procedure Prev; override;
    procedure PlannerChanged; override;
    function PosToDay(Pos: Integer): TDateTime; override;
    function PosToRes(Pos: Integer): Integer; override;
    procedure MapItemTimeOnPlanner(APlannerItem: TPlannerItem); override;
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property DateFormat: string read FDateFormat write FDateFormat;
    property Day: TDateTime read FDay write SetDay;
    property DayIncrement: Integer read FDayIncrement write FDayIncrement default 1;
    property Mode: TDBDayMode read FMode write SetMode;
    property NumberOfDays: Integer read FNumberOfDays write SetNumberOfDays default 7;
    property NumberOfResources: Integer read FNumberOfResources write SetNumberOfResources default 1;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
    property OnGetResourceName: TResourceNameEvent read FOnGetResourceName write FOnGetResourceName;
  end;

  TDBCustomPeriodSource = class(TDBItemSource)
  private
    FNumberOfResources: Integer;
    FOnGetResourceName: TResourceNameEvent;
    FAutoHeaderUpdate: Boolean;
    procedure ConfigurePlanner;
    procedure SetNumberOfResources(const Value: Integer);
  protected
    procedure ResourceUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure Next; override;
    procedure Prev; override;
  public
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property NumberOfResources: Integer read FNumberOfResources write SetNumberOfResources default 1;
    property OnGetResourceName: TResourceNameEvent read FOnGetResourceName write FOnGetResourceName;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBPeriodSource = class(TDBCustomPeriodSource)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
  protected
    procedure SetEndDate(const Value: TDateTime); virtual;
    procedure SetStartDate(const Value: TDateTime); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure PlannerChanged; override;
    procedure UpdatePlanner; virtual;
    procedure SetDateRange(AStartDate,AEndDate: TDateTime);
  published
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property EndDate: TDateTime read FEndDate write SetEndDate;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBHalfDayPeriodSource = class(TDBPeriodSource)
  private
  public
    procedure PlannerChanged; override;
    procedure UpdatePlanner; override;
  published
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBActiveDayPeriodSource = class(TDBPeriodSource)
  private
  public
    procedure PlannerChanged; override;
    procedure UpdatePlanner; override;
  published
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBMonthSource = class(TDBCustomPeriodSource)
  private
    FMonth: integer;
    FYear: integer;
    procedure SetMonth(const Value: integer);
    procedure SetYear(const Value: integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure PlannerChanged; override;
    procedure UpdatePlanner;
  published
    property Month: integer read FMonth write SetMonth;
    property Year: integer read FYear write SetYear;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBWeekSource = class(TDBCustomPeriodSource)
  private
    FMonth: Integer;
    FYear: Integer;
    FWeekStart: Integer;
    FWeeks: Integer;
    procedure SetMonth(const Value: integer);
    procedure SetYear(const Value: integer);
    procedure SetWeekStart(const Value: Integer);
    procedure SetWeeks(const Value: Integer);
  public
    constructor Create(AOwner: TComponent); override;
    procedure Loaded; override;
    procedure PlannerChanged; override;
    procedure UpdatePlanner;
  published
    property Month: Integer read FMonth write SetMonth;
    property Year: Integer read FYear write SetYear;
    property WeekStart: Integer read FWeekStart write SetWeekStart;
    property Weeks: Integer read FWeeks write SetWeeks;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBMultiMonthSource = class(TDBItemSource)
  private
    FNumberOfMonths: Integer;
    FStartMonth: Integer;
    FYear: Integer;
    FStartDate, FEndDate: TDateTime;
    FAutoHeaderUpdate: Boolean;
    FMonthIncrement: integer;
    procedure ConfigurePlanner;    
    procedure SetNumberOfMonths(const Value: Integer);
    procedure SetStartMonth(const Value: Integer);
    procedure SetYear(const Value: Integer);
  protected
    procedure ResourceUpdate; override;
  public
    procedure Loaded; override;
    procedure PlannerChanged; override;
    procedure UpdatePlanner;
    constructor Create(AOwner: TComponent); override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure Next; override;
    procedure Prev; override;
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property MonthIncrement: integer read FMonthIncrement write FMonthIncrement default 1;
    property NumberOfMonths: Integer read FNumberOfMonths write SetNumberOfMonths;
    property StartMonth: Integer read FStartMonth write SetStartMonth;
    property Year: Integer read FYear write SetYear;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBTimeLineSource = class(TDBItemSource)
  private
    FStartDate: TDateTime;
    FEndDate: TDateTime;
    FAutoHeaderUpdate: Boolean;
    FNumberOfResources: Integer;
    FOnGetResourceName: TResourceNameEvent;
    procedure SetEndDate(const Value: TDateTime);
    procedure SetStartDate(const Value: TDateTime);
    procedure SetNumberOfResources(const Value: Integer);
    procedure ConfigurePlanner;
  protected
    procedure ResourceUpdate; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure Next; override;
    procedure Prev; override;
    procedure PlannerChanged; override;
    procedure UpdatePlanner;
    function PosToRes(Pos: Integer): Integer; override;
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property NumberOfResources: Integer read FNumberOfResources write SetNumberOfResources default 3;
    property StartDate: TDateTime read FStartDate write SetStartDate;
    property EndDate: TDateTime read FEndDate write SetEndDate;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
    property OnGetResourceName: TResourceNameEvent read FOnGetResourceName write FOnGetResourceName;
  end;

  TDateItem = class(TCollectionItem)
  private
    FDate: TDateTime;
  published
    property Date: TDateTime read FDate write FDate;
  end;

  TDateCollection = class(TCollection)
  private
    function GetItem(Index: Integer): TDateItem;
    procedure SetItem(Index: Integer; const Value: TDateItem);  
  public
    function Add: TDateItem;
    function Insert(index: Integer): TDateItem;
    property Items[Index: Integer]: TDateItem read GetItem write SetItem; default;
    constructor Create;
    function HasDate(ADate: TDateTime): Boolean;
    function DatePos(ADate: TDateTime): Integer;
    function PosDate(Pos: Integer): TDateTime;
  end;

  TDBDisjunctDayMode = (djMultiDayRes, djMultiResDay);

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBDisjunctDaySource = class(TDBItemSource)
  private
    FDates: TDateCollection;
    FAutoHeaderUpdate: Boolean;
    FDateFormat: string;
    FNumberOfResources: integer;
    FDisjunctDayMode: TDBDisjunctDayMode;
    procedure ConfigurePlanner;
    procedure SetDates(const Value: TDateCollection);
    procedure SetNumberOfResources(const Value: Integer);
    procedure SetMode(const Value: TDBDisjunctDayMode);
  protected
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure PlannerChanged; override;
    procedure MapItemTimeOnPlanner(APlannerItem: TPlannerItem); override;
    function PosToDay(Pos: Integer): TDateTime; override;
    function PosToRes(Pos: integer): integer; override;
    function DateToPos(Res: integer; dt: TDateTime): integer;
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property DateFormat: string read FDateFormat write FDateFormat;
    property Dates: TDateCollection read FDates write SetDates;
    property Mode: TDBDisjunctDayMode read FDisjunctDayMode write SetMode default djMultiDayRes;
    property NumberOfResources: Integer read FNumberOfResources write SetNumberOfResources default 1;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBActiveDaySource = class(TDBItemSource)
  private
    FMode: TDBDayMode;
    FDay: TDateTime;
    FAutoHeaderUpdate: Boolean;
    FDateFormat: string;
    FNumberOfDays: Integer;
    FNumberOfResources: Integer;
    FOnGetResourceName: TResourceNameEvent;
    procedure SetDay(const Value: TDateTime);
    procedure SetMode(const Value: TDBDayMode);
    procedure SetNumberOfDays(const Value: Integer);
    procedure SetNumberOfResources(const Value: Integer);
    procedure ConfigurePlanner(var Span: Integer);
    function CalcItemPos(DatePos,ResourcePos: Integer): Integer;
    function DaysToSpan(Days: Integer): Integer;
    function DayInSpan(ADay: Integer): TDateTime;
    function IDayToPos(ADate: TDateTime): Integer;
    function IPosToDay(ADay: Integer): TDateTime;
  protected
  public
    constructor Create(AOwner: TComponent); override;
    procedure SynchDBItems; override;
    procedure ReadDBItems(UpdateKey: string); override;
    procedure WriteDBItem; override;
    procedure ReadDBItem; override;
    procedure AddDBItem; override;
    procedure GotoDBItem; override;
    procedure DeleteDBItem(APlanner: TPlanner); override;
    procedure ItemChanged(DBKey:string); override;
    procedure Next; override;
    procedure Prev; override;
    procedure PlannerChanged; override;
    function PosToDay(Pos: Integer): TDateTime; override;
    function PosToRes(Pos: Integer): Integer; override;
    procedure MapItemTimeOnPlanner(APlannerItem: TPlannerItem); override;
  published
    property AutoHeaderUpdate: Boolean read FAutoHeaderUpdate write FAutoHeaderUpdate default False;
    property DateFormat: string read FDateFormat write FDateFormat;
    property Day: TDateTime read FDay write SetDay;
    property Mode: TDBDayMode read FMode write SetMode;
    property NumberOfDays: Integer read FNumberOfDays write SetNumberOfDays default 7;
    property NumberOfResources: Integer read FNumberOfResources write SetNumberOfResources default 1;
    property RecurrencyField;
    property MinTimeField;
    property MaxTimeField;
    property OnGetResourceName: TResourceNameEvent read FOnGetResourceName write FOnGetResourceName;
  end;

  {$IFDEF DELPHIXE2_LVL}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF}
  TDBPlanner = class(TPlanner)
  private
    FItemSource: TItemSource;
    FOnGetHTMLTemplateData: THTMLTemplateDataEvent;
    FOnGetHTMLTemplate: THTMLTemplateEvent;
    procedure SetItemSource(const Value: TItemSource);
  protected
    procedure Notification(AComponent: TComponent; AOperation: TOperation); override;
    procedure NextClick(Sender: TObject); override;
    procedure PrevClick(Sender: TObject); override;
    procedure ItemMoved(APlannerItem: TPlannerItem; FromBegin, FromEnd, FromPos, ToBegin, ToEnd, ToPos: Integer); override;
    procedure ItemSized(APlannerItem: TPlannerItem; FromBegin, FromEnd, ToBegin, ToEnd: Integer); override;
    procedure ItemEdited(Item: TPlannerItem); override;
    procedure ItemSelected(Item: TPlannerItem); override;
    procedure MoveResource(FromPos, ToPos: Integer); override;
    procedure LoadItems;
    procedure ClearDBKey(Key: string);
    procedure MapItemTimeOnPlanner(APlannerItem: TPlannerItem); override;
    function GetVersionNr: Integer; override;
    function GetVersionString:string; override;
    function IsDBAware: boolean; override;
    function HTMLDBReplace(APlannerItem: TPlannerItem; Data: TObject): string; override;
    procedure DoGetHTMLTemplate(APlannerItem: TPlannerItem; var HTMLTemplate: string); virtual;
    procedure DoGetHTMLTemplateData(APlannerItem: TPlannerItem; Field: TField; HTMLTag: string; var Value: string); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    procedure Refresh; override;
    procedure FreeItem(APlannerItem:TPlannerItem); override;
    procedure UpdateItem(APlannerItem:TPlannerItem); override;
    procedure RefreshItem(APlannerItem:TPlannerItem); override;
    function CreateItem: TPlannerItem; override;
    function CreateItemAtSelection:TPlannerItem; override;
    function CreateNonDBItem: TPlannerItem;
    function CreateNonDBItemAtSelection: TPlannerItem;
    function PosToDay(Pos: Integer): TDateTime; override;
  published
    property ItemSource: TItemSource read FItemSource write SetItemSource;
    property OnGetHTMLTemplateData: THTMLTemplateDataEvent read FOnGetHTMLTemplateData write FOnGetHTMLTemplateData;
    property OnGetHTMLTemplate: THTMLTemplateEvent read FOnGetHTMLTemplate write FOnGetHTMLTemplate;
  end;



implementation

uses
  PlanUtil
{$IFDEF TMSCODESITE}
//  , CsIntf
{$ENDIF TMSCODESITE}
  ;

{$IFDEF TMSCODESITE}
procedure SendMsg(s:string);
begin
  outputdebugstring(pchar(s));
end;
{$ENDIF TMSCODESITE}


{ TDBPlanner }

constructor TDBPlanner.Create(AOwner: TComponent);
begin
  inherited;
  FItemSource := nil;
  Caption.Title := 'TMS software <b>'+ClassName+'</b>';
end;

function TDBPlanner.CreateItem: TPlannerItem;
begin
  Result := inherited CreateItem;
  Items.DBItem := Result;
  if Assigned(FItemSource) then
    FItemSource.AddDBItem;
end;

function TDBPlanner.CreateNonDBItem: TPlannerItem;
begin
  Result := inherited CreateItem;
  Result.NonDBItem := True;
end;

function TDBPlanner.CreateNonDBItemAtSelection: TPlannerItem;
begin
  Result := inherited CreateItemAtSelection;
  Result.NonDBItem := True;
end;

destructor TDBPlanner.Destroy;
begin
  if Assigned(FItemSource) then
    FItemSource.Planner := nil;
  inherited;
end;

procedure TDBPlanner.DoGetHTMLTemplateData(APlannerItem: TPlannerItem;
  Field: TField; HTMLTag: string; var Value: string);
begin
  if Assigned(OnGetHTMLTemplateData) then
    OnGetHTMLTemplateData(self, APlannerItem, Field, HTMLTag, Value);
end;

procedure TDBPlanner.DoGetHTMLTemplate(APlannerItem: TPlannerItem;
  var HTMLTemplate: string);
begin
  if Assigned(OnGetHTMLTemplate) then
    OnGetHTMLTemplate(self, APlannerItem, HTMLTemplate);
end;

procedure TDBPlanner.FreeItem(APlannerItem: TPlannerItem);
begin
  Items.DBItem := APlannerItem.ParentItem;
  if not Items.DBItem.NonDBItem then
  begin
    if Assigned(FItemSource) then
      FItemSource.DeleteDBItem(Self);
  end;

  inherited FreeItem(APlannerItem);
end;

function TDBPlanner.IsDBAware: boolean;
begin
  Result := true;
end;

function TDBPlanner.HTMLDBReplace(APlannerItem: TPlannerItem; Data: TObject): string;
var
  dbfield: TField;
  beforetag, aftertag, fld, dbfld: string;
  i, j: integer;
  s: string;
  DataSet: TDataSet;
begin
  Result := '';

  if not (Data is TDataSet) then
    Exit;

  DataSet := Data as TDataSet;

  dbfield := nil;

  beforetag := '';
  s := APlannerItem.HTMLTemplate.Text;

  DoGetHTMLTemplate(APlannerItem, s);

  while Pos('<#', s) > 0 do
  begin
    i := pos('<#', s);
    beforetag := beforetag + copy(s, 1, i - 1); //part prior to the tag
    aftertag := copy(s, i, length(s)); //part after the tag
    j := pos('>', aftertag);
    fld := copy(aftertag, 1, j - 1);
    Delete(fld, 1, 2);
    Delete(s, 1, i + j - 1);
    dbfld := '';
    if Assigned(DataSet) then
    begin
      if DataSet.Active then
      begin
        try
          dbfield := DataSet.FieldByName(fld);
        except
        end;

        if Assigned(dbfield) then
        begin
          if dbfield.IsBlob then
          begin
            dbfld := dbfield.AsString;
          end
          else
            dbfld := dbfield.DisplayText;
        end;

        DoGetHTMLTemplateData(APlannerItem, dbfield, fld, dbfld);
      end
      else
        dbfld := '(' + fld + ')';
    end
    else dbfld := '(' + fld + ')';

    beforetag := beforetag + dbfld;
  end;

  Result := beforetag + s;
end;


procedure TDBPlanner.ItemEdited(Item: TPlannerItem);
var
  DBKey:string;
begin
  inherited;
  if Item.NonDBItem then
    Exit;

  Items.DBItem := Item.ParentItem;
  if Assigned(FItemSource) then
  begin
    FItemSource.WriteDBItem;
    if Items.DBItem.Recurrent then
    begin
      DBKey := Items.DBItem.DBKey;
      ClearDBKey(DBKey);
      FItemSource.ReadDBItems(DBKey);
    end;
  end;
end;

procedure TDBPlanner.ItemMoved(APlannerItem: TPlannerItem; FromBegin, FromEnd, FromPos, ToBegin, ToEnd,
  ToPos: Integer);
var
  DBKey: string;
begin
  inherited;

  if not Assigned(APlannerItem) then
    Exit;

  if APlannerItem.NonDBItem then
    Exit;

  Items.DBItem := APlannerItem.ParentItem;

  if Assigned(FItemSource) then
  begin
    FItemSource.WriteDBItem;
    if Items.DBItem.Recurrent then
    begin
      DBKey := Items.DBItem.DBKey;
      ClearDBKey(DBKey);
      FItemSource.ReadDBItems(DBKey);
    end;
  end;
end;

procedure TDBPlanner.ItemSized(APlannerItem: TPlannerItem; FromBegin, FromEnd, ToBegin,
  ToEnd: Integer);
var
  DBKey: string;
begin
  inherited;

  if APlannerItem.NonDBItem then
    Exit;

  Items.DBItem := APlannerItem.ParentItem;
  if Assigned(FItemSource) then
  begin
    FItemSource.WriteDBItem;
    if Items.DBItem.Recurrent then
    begin
      DBKey := Items.DBItem.DBKey;
      ClearDBKey(DBKey);
      FItemSource.ReadDBItems(DBKey);
    end;
  end;
end;

procedure TDBPlanner.Loaded;
begin
  inherited;
end;

procedure TDBPlanner.NextClick(Sender: TObject);
begin
  SetFocus;
  if Assigned(FItemSource) then
    FItemSource.Next;
  inherited;
end;

procedure TDBPlanner.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and
     (AComponent = FItemSource) then
    begin
      FItemSource := Nil;
      Items.Clear;
    end;
end;

procedure TDBPlanner.PrevClick(Sender: TObject);
begin
  SetFocus;
  if Assigned(FItemSource) then
    FItemSource.Prev;
  inherited;
end;

procedure TDBPlanner.SetItemSource(const Value: TItemSource);
begin
  if Assigned(Value) and Assigned(Value.Planner) then
  begin
    if FItemSource = Value then
      Exit;

    if Value.Planner = Self then
    begin
      ShowMessage('Cannot assign itemsource to multiple TDBPlanner components');
      Exit;
    end;
  end;

  if Assigned(FItemSource) and (Value <> FItemSource) then
    FItemSource.Planner := nil;

  FItemSource := Value;
  if Assigned(FItemSource) then
  begin
    FItemSource.Planner := Self;
    if not (csLoading in ComponentState) then LoadItems;
  end
  else
  begin
    Items.BeginUpdate;
    Items.Clear;
    Items.EndUpdate;
  end;
end;

procedure TDBPlanner.RefreshItem(APlannerItem: TPlannerItem);
var
  EditKey, DBKey: string;
  plIt: TPlannerItem;
begin
  inherited;

  if APlannerItem.NonDBItem then
    Exit;

  Items.DBItem := APlannerItem.ParentItem;

  EditKey := Items.DBItem.DBKey;

  if Assigned(FItemSource) then
  begin
    FItemSource.GotoDBItem;
    if (Items.DBItem.Recurrent) or (Items.DBItem.Recurrency <> '') then
    begin
      DBKey := Items.DBItem.DBKey;
      ClearDBKey(DBKey);
      FItemSource.ReadDBItems(DBKey);
    end
    else
      FItemSource.ReadDBItem;
  end;

  plIt := Items.FindKey(EditKey);
  if Assigned(plIt) then
    Items.Select(plIt);

  //if Assigned(Items.DBItem) then
  //  Items.Select(Items.DBItem);
end;

procedure TDBPlanner.UpdateItem(APlannerItem: TPlannerItem);
begin
  inherited;

  if APlannerItem.NonDBItem then
    Exit;

  Items.DBItem := APlannerItem.ParentItem;

  if Assigned(FItemSource) then
  begin
    FItemSource.WriteDBItem;
    if Assigned(APlannerItem) then
      RefreshItem(APlannerItem);
  end;
end;

procedure TDBPlanner.MapItemTimeOnPlanner(APlannerItem: TPlannerItem);
begin
  if Assigned(FItemSource) then
    FItemSource.MapItemTimeOnPlanner(APlannerItem);
end;

procedure TDBPlanner.ClearDBKey(Key: string);
var
  i: Integer;
begin
  i := 0;

  while (i < Items.Count) do
  begin
    if Items[i].DBKey = Key then
    begin
      Items[i].Free;
    end
    else
      inc(i);
  end;
end;

procedure TDBPlanner.LoadItems;
begin
  Items.BeginUpdate;
  Items.ClearDB;
  FItemSource.ReadDBItems('');
  Items.EndUpdate;
end;

procedure TDBPlanner.MoveResource(FromPos, ToPos: Integer);
begin
  if Assigned(ItemSource) then
  begin
    if ItemSource is TDBItemSource then
      (ItemSource as TDBItemSource).MoveResource(FromPos,ToPos);
  end;
end; 

procedure TDBPlanner.ItemSelected(Item: TPlannerItem);
begin
  if Item.NonDBItem then
  begin
    inherited;
    Exit;
  end;

  Items.DBItem := Item.ParentItem;
  if Assigned(FItemSource) then
    FItemSource.GotoDBItem;
  inherited;
end;

function TDBPlanner.CreateItemAtSelection: TPlannerItem;
begin
  Result := Inherited CreateItemAtSelection;
  Items.DBItem := Result;
  if Assigned(FItemSource) then
    FItemSource.AddDBItem;
end;

procedure TDBPlanner.Refresh;
begin
  inherited;
  if Assigned(ItemSource) then
    ItemSource.Reload;
end;

function TDBPlanner.PosToDay(Pos: Integer): TDateTime;
begin
  if Assigned(ItemSource) then
    Result := ItemSource.PosToDay(Pos)
  else
    Result := inherited PosToDay(Pos);
end;

function TDBPlanner.GetVersionNr: Integer;
begin
  Result := MakeLong(MakeWord(BLD_VER,REL_VER),MakeWord(MIN_VER,MAJ_VER));
end;

function TDBPlanner.GetVersionString:string;
var
  vn: Integer;
begin
  vn := GetVersionNr;
  Result := IntToStr(Hi(Hiword(vn)))+'.'+IntToStr(Lo(Hiword(vn)))+'.'+IntToStr(Hi(Loword(vn)))+'.'+IntToStr(Lo(Loword(vn)))+' '+DATE_VER;
end;



{ TItemSource }

procedure TItemSource.AddDBItem;
begin

end;

constructor TItemSource.Create(AOwner: TComponent);
begin
  inherited;
  FPlanner := Nil;
  FUpdateCount := 0;
  FActive := True;
end;


procedure TItemSource.DeleteDBItem(APlanner: TPlanner);
begin

end;

destructor TItemSource.Destroy;
begin
  inherited;
end;


procedure TItemSource.Next;
begin
  ClearDBItems;
  ReadDBItems('');
end;

procedure TItemSource.Prev;
begin
  ClearDBItems;
  ReadDBItems('');
end;

procedure TItemSource.ReadDBItems(UpdateKey: string);
begin
end;

procedure TItemSource.ItemChanged(DBKey: string);
begin
end;

procedure TItemSource.WriteDBItem;
begin
end;

procedure TItemSource.ReadDBItem;
begin

end;

procedure TItemSource.BeginUpdate;
begin
  Inc(FUpdateCount);
end;

procedure TItemSource.EndUpdate;
begin
  if (FUpdateCount > 0) then
    Dec(FUpdateCount);
end;

function TItemSource.IsUpdating: Boolean;
begin
  Result := FUpdateCount > 0;
end;

procedure TItemSource.SynchDBItems;
begin
end;

procedure TItemSource.ClearDBItems;
begin
  // 1.5.0.3 optimization
  if IsUpdating then
    Exit;
    
  if Assigned(FPlanner) then
  begin
    FPlanner.Items.BeginUpdate;
    FPlanner.Items.ClearDB;
    FPlanner.Items.EndUpdate;
    FPlanner.Items.Selected := nil;
  end;
end;

procedure TItemSource.Refresh;
begin
  if Assigned(FPlanner) then
    FPlanner.Invalidate;
end;

procedure TItemSource.GotoDBItem;
begin

end;

function TItemSource.CreateKey: string;
var
  GUID: TGUID;
  Key: string;
begin
  Key := '';

  if Assigned(FOnCreateKey) then
    FOnCreateKey(Self,FPlanner.Items.DBItem, Key)
  else
  begin

    CoCreateGUID(GUID);

    Key := GUIDToString(GUID);

  end;

  Result := Key;
end;

procedure TItemSource.SetPlanner(Value :TPlanner);
begin
  FPlanner := Value;
  PlannerChanged;
end;

procedure TItemSource.PlannerChanged;
begin

end;

procedure TItemSource.SetActive(const Value: Boolean);
begin
  if FActive <> Value then
  begin
    FActive := Value;
    if not FActive then
    begin
      ClearDBItems;
      Inc(FUpdateCount)
    end
    else
    begin
      if FUpdateCount > 0 then
      begin
        Dec(FUpdateCount);
        if FUpdateCount = 0 then
          Reload;
      end;  
    end;
  end;
end;

procedure TItemSource.Reload;
begin
  ClearDBItems;
  ReadDBItems('');
end;

function TItemSource.LocateFirstItem(Specifier: string; Params: TFindTextParams;
  var StartTime, EndTime: TDateTime;var Key:string): Boolean;
begin
  Result := False;
end;

function TItemSource.LocateNextItem(var StartTime,
  EndTime: TDateTime;var Key:string): Boolean;
begin
  Result := False;
end;

function TItemSource.PosToDay(Pos: Integer): TDateTime;
begin
  Result := 0;
end;

function TItemSource.PosToRes(Pos: Integer): Integer;
begin
  Result := 0;
end;

procedure TItemSource.MapItemTimeOnPlanner(APlannerItem: TPlannerItem);
begin

end;

{ TDBPlannerDataLink }

procedure TDBPlannerDataLink.ActiveChanged;
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('ActiveChanged');
  {$ENDIF}
  FDBItemSource.ClearDBItems;
  FDBItemSource.ReadDBItems('');
  FDBItemSource.Refresh;
end;


constructor TDBPlannerDataLink.Create(ADBItemSource: TDBItemSource);
begin
  inherited Create;
  FDBItemSource := ADBItemSource;
end;

procedure TDBPlannerDataLink.DataSetChanged;
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('DatasetChanged');
  {$ENDIF}
end;

procedure TDBPlannerDataLink.DataSetScrolled(Distance: Integer);
begin
  inherited;
  {$IFDEF TMSCODESITE}
  SendMsg('DatasetScrolled');
  {$ENDIF}
end;

destructor TDBPlannerDataLink.Destroy;
begin
  inherited;
end;

procedure TDBPlannerDataLink.RecordChanged(Field: TField);
begin
  inherited;

  if FDBItemSource.IsUpdating then
    Exit;

  {what is the connected planner ??}

  if Assigned(DataSet) then
    if DataSet.Active then
    begin
      {$IFDEF TMSCODESITE}
      if Assigned(Field) then
       SendMsg('RecordChanged : '+Field.FieldName +' of '+DataSet.FieldByName(FDBItemSource.KeyField).AsString)
      else
       SendMsg('RecordChanged : '+DataSet.FieldByName(FDBItemSource.KeyField).AsString);
      {$ENDIF}

      if DataSet.State = dsBrowse then
      begin
        FDBItemSource.ItemChanged(DataSet.FieldByName(FDBItemSource.KeyField).AsString);
      end;
    end;
end;


{ TDBItemSource }

constructor TDBItemSource.Create(AOwner: TComponent);
begin
  inherited;
  FDataLink := TDBPlannerDataLink.Create(Self);
  FResourceMap := TResourceMap.Create(Self);
  FRecurrencyHandler := TRecurrencyHandler.Create;
  FResourceDataLink := TDBPlannerResourceDataLink.Create(Self);
  FResourceDataSource := TResourceDataSource.Create(Self);
end;

destructor TDBItemSource.Destroy;
begin
  FDataLink.Free;
  FResourceDataLink.Free;
  FResourceMap.Free;
  FRecurrencyHandler.Free;
  FResourceDataSource.Free;
  inherited;
end;

function TDBItemSource.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TDBItemSource.SetDataSource(const Value: TDataSource);
begin
  if FDataLink.DataSource <> Value then
  begin
    FDataLink.DataSource := Value;
  end;
end;

procedure TDBItemSource.SetStartTimeField(const Value: string);
begin
  FStartTimeField := Value;
end;

procedure TDBItemSource.SetKeyField(const Value: string);
begin
  FKeyField := Value;
end;

procedure TDBItemSource.SetResourceField(const Value: string);
begin
  FResourceField := Value;
end;

procedure TDBItemSource.SetEndTimeField(const Value: string);
begin
  FEndTimeField := Value;
end;

procedure TDBItemSource.SetRecurrencyField(const Value: string);
begin
  FRecurrencyField := Value;
end;

procedure TDBItemSource.SetMinTimeField(const Value: string);
begin
  FMinTimeField := Value;
end;

procedure TDBItemSource.SetMaxTimeField(const Value: string);
begin
  FMaxTimeField := Value;
end;

procedure TDBItemSource.SetNotesField(const Value: string);
begin
  FNotesField := Value;
end;

procedure TDBItemSource.SetSubjectField(const Value: string);
begin
  FSubjectField := Value;
end;


procedure TDBItemSource.Next;
begin
  inherited;
  if Assigned(FOnChangeQuery) then
    FOnChangeQuery(Self);
end;

procedure TDBItemSource.Prev;
begin
  inherited;
  if Assigned(FOnChangeQuery) then
    FOnChangeQuery(Self);
end;

procedure TDBItemSource.Notification(AComponent: TComponent;
  AOperation: TOperation);
begin
  inherited;
  if (AOperation = opRemove) and (AComponent = FDataLink.DataSource) then
     FDataLink.DataSource := nil;

  if (AOperation = opRemove) and (AComponent = FResourceDataLink.DataSource) then
     FResourceDataLink.DataSource := nil;
end;

procedure TDBItemSource.SetReadOnly(const Value: Boolean);
begin
  FReadOnly := Value;
end;

function MatchItem(Specifier,Subject,Notes: string; Param: TFindTextParams): Boolean;
var
  SrchText: string;

begin
  Result := False;

  if fnCaptionText in Param then
  begin
    SrchText := Subject;

    if fnIgnoreHTMLTags in Param then
      SrchText := HTMLStrip(SrchText);

    if not (fnMatchCase in Param) then
    begin
      SrchText := AnsiUpperCase(SrchText);
      Specifier := AnsiUpperCase(Specifier);
    end;

    if fnMatchFull in Param then
      Result := (Specifier = SrchText)
    else
      if fnMatchStart in Param then
        Result := Pos(Specifier,SrchText) = 1
      else
        if fnMatchRegular in Param then
          Result := MatchStr(Specifier,SrchText,fnMatchCase in Param);
  end;

  if Result then Exit;

  if fnText in Param then
  begin
    SrchText := Notes;

    if fnIgnoreHTMLTags in Param then
      SrchText := HTMLStrip(SrchText);

    if not (fnMatchCase in Param) then
    begin
      SrchText := AnsiUpperCase(SrchText);
      Specifier := AnsiUpperCase(Specifier);
    end;

    if fnMatchFull in Param then
      Result := (Specifier = SrchText)
    else
      if fnMatchStart in Param then
        Result := Pos(Specifier,SrchText) = 1
      else
        if fnMatchRegular in Param then
          Result := MatchStr(Specifier,SrchText,fnMatchCase in Param);
  end;
end;


function TDBItemSource.LocateFirstItem(Specifier: string;
  Params: TFindTextParams; var StartTime, EndTime: TDateTime;var Key:string): Boolean;
var
  D: TDataSet;
  B: TBookMark;
  Subject,Notes: string;

begin
  Result := False;

  if IsUpdating then
    Exit;
    
  if not CheckDataSet then
    Exit;

  FLocateIdx := 0;
  FLocateSpecifier := Specifier;
  FLocateParams := Params;

  BeginUpdate;

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  while not D.Eof do
  begin
    Inc(FLocateIdx);

    Subject := '';
    Notes := '';

    if (SubjectField <> '') then
      Subject := D.FieldByName(SubjectField).AsString;

    if (NotesField <> '') then
      Notes := D.FieldByName(NotesField).AsString;

    if MatchItem(FLocateSpecifier,Subject,Notes,FLocateParams) then
    begin
      FieldsToTime(D.Fields,StartTime,EndTime);
      Key := D.FieldByName(KeyField).AsString;
      Result := True;
      Break;
    end;
    D.Next;

  end;

  D.GotoBookMark(B);

  D.EnableControls;

  D.FreeBookMark(B);

  EndUpdate;
end;

function TDBItemSource.LocateNextItem(var StartTime,
  EndTime: TDateTime;var Key:string): Boolean;
var
  D: TDataSet;
  B: TBookMark;
  Subject,Notes: string;

begin
  Result := False;

  if IsUpdating then
    Exit;
    
  if not CheckDataSet then
    Exit;

  BeginUpdate;

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;
  D.MoveBy(FLocateIdx);


  while not D.Eof do
  begin
    Inc(FLocateIdx);
    Subject := '';
    Notes := '';

    if (SubjectField <> '') then
      Subject := D.FieldByName(SubjectField).AsString;

    if (NotesField <> '') then
      Notes := D.FieldByName(NotesField).AsString;

    if MatchItem(FLocateSpecifier,Subject,Notes,FLocateParams) then
    begin
      FieldsToTime(D.Fields,StartTime,EndTime);	
      Key := D.FieldByName(KeyField).AsString;
      Result := True;
      Break;
    end;

    D.Next;


  end;

  D.GotoBookMark(B);

  D.EnableControls;

  D.FreeBookMark(B);

  EndUpdate;
end;

function TDBItemSource.CheckDataSet: Boolean;
begin
  Result := False;

  if KeyField = '' then Exit;
  if not Assigned(FPlanner) then Exit;

  if Assigned(DataSource) then
  begin
    if Assigned(DataSource.DataSet) then
    begin
      if DataSource.DataSet.Active then
      begin        
        Result := (((StartTimeField <> '') AND (EndTimeField <> '')) OR
          (Assigned(FOnFieldsToTime) AND Assigned(FOnTimeToFields))) AND (KeyField <> '');
      end;
    end
  end;
end;

procedure TDBItemSource.FieldsToItem(APlannerItem: TPlannerItem);
begin
  if (KeyField <> '') and Assigned(FDBKeyDBField) then
    APlannerItem.DBKey := FDBKeyDBField.AsString;

  if (SubjectField <> '') and Assigned(FSubjectDBField) then
    APlannerItem.CaptionText := FSubjectDBField.DisplayText;

  if (NotesField <> '') and Assigned(FNotesDBField) then
  begin
    if FNotesDBField.IsBlob then
      APlannerItem.Text.Text := FNotesDBField.AsString
    else
      APlannerItem.Text.Text := FNotesDBField.DisplayText;
  end;
end;


procedure TDBItemSource.FieldsToTime(Fields:TFields; var dtS: TDateTime; var dtE: TDateTime);
begin
  if Assigned(FOnFieldsToTime) then 
  begin
    FOnFieldsToTime(Self,Fields,dtS,dtE);
  end
  else
  begin
    dtS := Fields.FieldByName(StartTimeField).AsDateTime;
    dtE := Fields.FieldByName(EndTimeField).AsDateTime;
  end;
end;


procedure TDBItemSource.TimeToFields(Fields:TFields; dtS: TDateTime; dtE: TDateTime);
begin
  if Assigned(FOnTimeToFields) then 
  begin
    FOnTimeToFields(Self,Fields,dtS,dtE);
  end
  else
  begin
    Fields.FieldByName(StartTimeField).AsDateTime := dtS;
    Fields.FieldByName(EndTimeField).AsDateTime := dtE;
  end;
end;



procedure TDBItemSource.SetResourceMap(const Value: TResourceMap);
begin
  FResourceMap.Assign(Value);
end;

procedure TDBItemSource.FieldsToRes(Fields: TFields; var Position: Integer;
  var Accept: Boolean);
var
  e: integer;
begin
  Accept := True;

  if ResourceMap.Count > 0 then
  begin
    Position := ResourceMap.ResToPosition(StrToInt(Fields.FieldByName(ResourceField).Text));
    Accept := Position <> -1;
  end
  else
  begin
    if Assigned(FOnResourceToPosition) then
      FOnResourceToPosition(Self,Fields.FieldByName(ResourceField),Position, Accept)
    else
    begin
      val(Fields.FieldByName(ResourceField).Text, Position, e);
    end;
  end;
end;

procedure TDBItemSource.ResToFields(Fields: TFields; Position: Integer);
begin
  if ResourceMap.Count > 0 then
  begin
    Fields.FieldByName(ResourceField).Text := IntToStr(ResourceMap.PositionToRes(Position));
  end
  else
  begin
    if Assigned(FOnPositionToResource) then
      FOnPositionToResource(Self,Fields.FieldByName(ResourceField),Position)
    else
      Fields.FieldByName(ResourceField).Text := IntToStr(Position);
  end;
end;

procedure TDBItemSource.MoveResource(FromPos, ToPos: Integer);
var
  ri,rio: TResourceMapItem;
  i: integer;
begin
  if (ResourceMap.Count > FromPos) and (ResourceMap.Count > ToPos) then // move resource
  begin
    rio := ResourceMap.Add;
    rio.Assign(ResourceMap[FromPos]);

    if FromPos < ToPos then
      ri := ResourceMap.Insert(ToPos + 1)
    else
      ri := ResourceMap.Insert(ToPos);

    ri.Assign(rio);

    rio.Free;
    if FromPos < ToPos then
      ResourceMap[FromPos].Free
    else
      ResourceMap[FromPos + 1].Free;
  end;

  for i := 0 to ResourceMap.Count -1 do
  begin
    ResourceMap[i].PositionIndex := i;
    {$IFDEF TMSDEBUG}
    outputdebugstring(pchar(inttostr(i)+':'+ ResourceMap[i].DisplayName));
    {$ENDIF}
  end;
end;

procedure TDBItemSource.SetResourceDataSource(
  const Value: TResourceDataSource);
begin
  FResourceDataSource.Assign(Value);
end;

procedure TDBItemSource.ResourceUpdate;
begin
  if Assigned(OnResourceChange) then
    OnResourceChange(Self);
end;

{ TDBDaySource }

procedure TDBDaySource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dts,dte,rdts,rdte: TDateTime;
  plIt, cloneplIt: TPlannerItem;
  Span, j: Integer;
  Accept: Boolean;
  ResourcePos,DatePos: Integer;
  ZoneStart, ZoneEnd: TDateTime;
  dis, die: TDateTime;
  crossflg: Boolean;

begin
  if IsUpdating then
    Exit;

  if not CheckDataSet then
    Exit;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  ConfigurePlanner(Span);

  FPlanner.Mode.Date := Day;

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;

  ZoneStart := EncodeTime(j div 60,j mod 60,0,0);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;
  dis := EncodeTime(j div 60, j mod 60,0,0);

  j := FPlanner.Display.DisplayUnit * (FPlanner.Display.DisplayEnd + 1);
  crossflg := j >= 1440;

  j := j mod 1440;
  die := EncodeTime(j div 60, j mod 60,0,0);

  ZoneEnd := die + (Span - 1);
  if crossflg then
    ZoneEnd := ZoneEnd + 1;

  FPlanner.Items.BeginUpdate;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);
    rdts := dts;
    rdte := dte;

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := FDay + ZoneEnd + 1;
      FRecurrencyHandler.Generate;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
//        OutputdebugString(pchar(FPlanner.Name+' READ ITEM '+D.FieldByName(KeyField).AsString+':'+FormatDateTime('dd/mm/yyyy hh:nn',dts)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',dte)));
//        OutputDebugString(pchar('SPAN:'+FormatDateTime('dd/mm/yyyy hh:nn',FDay)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',FDay + Span)));

        Accept := True;

        // resource based item acception
        if (ResourceField <> '') and
           (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) and
           (dte >= FDay + ZoneStart) and (dts < FDay + ZoneEnd) then
        begin
          FieldsToRes(D.Fields, ResourcePos, Accept);
        end;

        if (Int(dte) = Int(dts)) and not crossflg and ((Frac(dte) < Frac(dis)) or (Frac(dts) > Frac(die))) then
          Accept := False;

        // date based item acception
        if (dte >= FDay + ZoneStart) and (dts < FDay + ZoneEnd) and Accept and Assigned(FOnAccept) then
        begin
          FOnAccept(Self,D.Fields,Accept);
        end;

        if ((dte >= FDay + ZoneStart) and (dts < FDay + ZoneEnd)) and Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' ADD ITEM '+D.FieldByName(KeyField).AsString +
              ' ' +DateToStr(dts) + ' ' +DateToStr(dte));
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          with plIt do
          begin
            Recurrent := FRecurrencyHandler.IsRecurrent;

            FPlanner.RemoveClones(plIt);

            // set time here correct

            ItemRealStartTime := dts;
            ItemRealEndTime := dte;

            RecurrentOrigStart := dts;
            RecurrentOrigEnd := dte;

            RecurrentStart := rdts;
            RecurrentEnd := rdte;

            ItemStartTime := dts;
            ItemEndTime := dte;

            Recurrency := FRecurrencyHandler.Recurrency;

            InHeader := dte - dts > 1;

            RealTime := true;

            // map to start of day
            if (Int(dts) < Int(FDay)) and not InHeader then
              ChangeCrossing;

            if (dts > FDay) then
            begin
              DatePos := Round(Int(dts - FDay));
                          // changed for items that have both start & end time in crossing day part
              //if crossflg and (frac(dte) < frac(dis)) and (frac(dts) < frac(dis)) then
              if crossflg and (ItemBeginPrecis > 1440) then
              begin
                DatePos := DatePos - 1;
              end;
            end
            else
              DatePos := 0;

            ItemPos := CalcItemPos(DatePos,ResourcePos);

            FieldsToItem(plIt);

            if FReadOnly then
            begin
              FixedPos := True;
              FixedSize := True;
              ReadOnly := True;
            end;
          end;

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);

          if plIt.InHeader then
          begin
            if dts < Day then
              dts := Day;

            for j := 1 to Round(Int(dte) - Int(dts)) do
            begin
              if (j + plIt.ItemPos < FPlanner.Positions) and (dts + j < Day + Span) then
              begin
                cloneplIt := FPlanner.CloneItem(plIt);
                with cloneplIt do
                begin
                  ItemRealStartTime := rdts;
                  ItemRealEndTime := dte;
                  RealTime := true;
                  DatePos := DatePos + 1;
                  ItemPos := CalcItemPos(DatePos,ResourcePos);
                  DBKey := plIt.DBKey;
                end;
              end;
            end;
          end;
        end;
      end;
    end;
    D.Next;
  end;

  D.GotoBookMark(B);

  D.EnableControls;

  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);

  if FPlanner.Header.AutoSize then
    FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;

  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}

  FPlanner.Items.ResolveLinks;
end;

procedure TDBDaySource.SetDay(const Value: TDateTime);
begin
  if FDay <> Value then
  begin
    FDay := Int(Value);
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBDaySource.Next;
begin
  FDay := FDay + FDayIncrement;
  inherited;
end;

procedure TDBDaySource.Prev;
begin
  FDay := FDay - FDayIncrement;
  inherited;
end;

procedure TDBDaySource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTE, DTS: TDateTime;
  ResPos: Integer;
  deltastart, deltaend: TDateTime;

begin
  if IsUpdating then
    Exit;

  if not CheckDataSet then
    Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

    D.Edit;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := FPlanner.Items.DBItem.ItemRealStartTime;
    DTE := FPlanner.Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := FPlanner.Items.DBItem.ItemStartTime;
    DTE := FPlanner.Items.DBItem.ItemEndTime;
  end;

  {$IFDEF TMSCODESITE}
  SendMsg('writedb:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
  {$ENDIF}

  FPlanner.Items.DBItem.RealTime := False;

  //  if DTE < DTS then
  //    DTE := DTE + 1;

  // recalculate for recurrent items
  //

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  TimeToFields(D.Fields,DTS,DTE);

  if (ResourceField <> '') and (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);
    ResToFields(D.Fields, ResPos);
  end;

  if (SubjectField <> '') and
     (D.FieldByName(SubjectField).CanModify) then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     (D.FieldByName(NotesField).CanModify) and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;


procedure TDBDaySource.SetMode(const Value: TDBDayMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    case FMode of
    dmMultiDay:
      if Assigned(FPlanner) then
      begin
        FPlanner.PositionGroup := 0;
        FPlanner.Positions := NumberOfDays;
      end;
    dmMultiResource:
      if Assigned(FPlanner) then
      begin
        FPlanner.PositionGroup := 0;
        FPlanner.Positions := NumberOfResources;
      end;
    end;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBDaySource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;
  ResPos: Integer;

begin
  inherited;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;

  DTS := FPlanner.Items.DBItem.ItemStartTime;
  DTE := FPlanner.Items.DBItem.ItemEndTime;

  FPlanner.Items.DBItem.ItemRealStartTime := DTS;
  FPlanner.Items.DBItem.ItemRealEndTime := DTE;

  TimeToFields(D.Fields,DTS,DTE);

  if (ResourceField <> '') and (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);

    ResToFields(D.Fields, ResPos);
  end;

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;

procedure TDBDaySource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;
  
  EndUpdate;
end;


procedure TDBDaySource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dt, dtE: TDateTime;
  Span: Integer;
  D: TDataSet;
  UpdKey: string;

begin
  inherited;
  if IsUpdating then
    Exit;

  if (DBKEY = '') then
    Exit;

  if not Assigned(FPlanner) then
    Exit;

  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}

    FPlanner.Items.DBItem := plIt;

    if plIt.Recurrent then
    begin
      UpdKey := plIt.DBKey;
      TDBPlanner(FPlanner).ClearDBKey(plIt.DBKey);
      EndUpdate;

      ReadDBItems(UpdKey);
      Planner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;
    FieldsToTime(D.Fields,dt,dtE);

    if Mode = dmMultiResource then
      Span := 1
    else
      Span := FPlanner.Positions;

    if (dt > FDay) and (dt < FDay + Span) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;

      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;
  end;

  EndUpdate;
end;

procedure TDBDaySource.ReadDBItem;
var
  D: TDataSet;
  dts,dte: TDateTime;
  ResourcePos,DatePos,j: Integer;
  Accept: Boolean;
  Zone: TDateTime;
  Span: Integer;
  dis: TDateTime;
  crossflg: Boolean;
  cloneplIt: TPlannerItem;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  FPlanner.RemoveClones(FPlanner.Items.DBItem);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;

  Zone := EncodeTime(j div 60,j mod 60,0,0);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;
  dis := EncodeTime(j div 60, j mod 60,0,0);

  j := FPlanner.Display.DisplayUnit * (FPlanner.Display.DisplayEnd + 1);
  crossflg := j >= 1440;

  //j := j mod 1440;
  //die := EncodeTime(j div 60, j mod 60,0,0);

  case Mode of
  dmMultiDay,dmMultiDayRes,dmMultiResDay: Span := NumberOfDays;
  dmMultiResource: Span := 1;
  end;

  ConfigurePlanner(Span);

  with FPlanner.Items.DBItem do
  begin
    FieldsToTime(D.Fields,dts,dte);

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := FDay + Span + Zone + 1;
    FRecurrencyHandler.Generate;

    {$IFDEF TMSCODESITE}
    SendMsg('read:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
    {$ENDIF}

    ItemStartTime := dts;
    ItemEndTime := dte;
    ItemRealStartTime := dts;
    ItemRealEndTime := dte;

    InHeader := dte - dts > 1;

    // resource based item acception
    if (ResourceField <> '') and
       (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) and
       (dte >= FDay + Zone) and (dts < FDay + Span + Zone) then
    begin
      FieldsToRes(D.Fields,ResourcePos, Accept);
    end;

    // date based item acception
    if (dte >= FDay + Zone) and (dts < FDay + Span + Zone) then
    begin
      FPlanner.RemoveClones(FPlanner.Items.DBItem);

      if (dts > FDay) then
      begin
        DatePos := Round(Int(dts - FDay));

        if crossflg and (frac(dte) < frac(dis)) and (frac(dts) < frac(dis)) then
        begin
          DatePos := DatePos - 1;
        end;
      end
      else
        DatePos := 0;

      ItemPos := CalcItemPos(DatePos,ResourcePos);

      if SubjectField <> '' then
        CaptionText := D.FieldByName(SubjectField).AsString;

      if NotesField <> '' then
        Text.Text := D.FieldByName(NotesField).AsString;

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;

      if InHeader then
      begin
        if dts < Day then
          dts := Day;

        for j := 1  to Round(Int(dte) - Int(dts)) do
        begin
          if (j + ItemPos < FPlanner.Positions) and (dts + j < Day + Span)  then
          begin
            cloneplIt := FPlanner.CloneItem(FPlanner.Items.DBItem);
            with cloneplIt do
            begin
              DatePos := DatePos + 1;
              ItemPos := CalcItemPos(DatePos,ResourcePos);
            end;
          end;
        end;
      end;


      if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
        FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

      if Assigned(FOnFieldsToItem) then
        FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);
    end
    else
    begin
      FPlanner.Items.DBItem.Free;
      FPlanner.Items.DBItem := nil;
    end;
  end;

  if FPlanner.Header.AutoSize then
   FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;

end;

procedure TDBDaySource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dt, dtE: TDateTime;
  plIt: TPlannerItem;
  Span,i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;

  if Mode = dmMultiResource then
    Span := 1
  else
  begin
    Span := FPlanner.Positions;
  end;

  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dt,dtE);

    if (dt > FDay) and (dt < FDay + Span) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;
    
    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
    begin
      if not FPlanner.Items[i - 1].NonDBItem then
        FPlanner.Items[i - 1].Free;
    end;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBDaySource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

constructor TDBDaySource.Create(AOwner: TComponent);
begin
  inherited;
  FDayIncrement := 1;
  // Initialize on US date format
  FDateFormat := 'mm/dd/yyyy';
  FNumberOfDays := 7;
  FNumberOfResources := 1;
end;

procedure TDBDaySource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
    FPlanner.Mode.PlannerType := plDay;
end;

function TDBDaySource.PosToDay(Pos: Integer): TDateTime;
begin
  case Mode of
  dmMultiDay:
    Result := FDay + Pos;
  dmMultiResource:
    Result := Day;
  dmMultiDayRes:
    Result := FDay + (Pos mod NumberOfDays);
  dmMultiResDay:
    Result := FDay + (Pos div NumberOfResources);
  else
    Result := FDay;
  end;
end;

function TDBDaySource.PosToRes(Pos: Integer): Integer;
begin
  case Mode of
  dmMultiDay:
    Result := 0;
  dmMultiResource:
  	Result := Pos;
  dmMultiDayRes:
    Result := Pos div NumberOfDays;
  dmMultiResDay:
    Result := Pos mod NumberOfResources;
  else
    Result := 0;
  end;
end;


procedure TDBDaySource.SetNumberOfDays(const Value: Integer);
begin
  if Value <= 0 then
    Exit;

  if FNumberOfDays <> Value then
  begin
    FNumberOfDays := Value;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBDaySource.SetNumberOfResources(const Value: Integer);
begin
  if Value <= 0 then
    Exit;
  if FNumberOfResources <> Value then
  begin
    FNumberOfResources := Value;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBDaySource.ConfigurePlanner(var Span: Integer);
var
  i,j,k,dx: Integer;
  ResName: string;
begin
  if not Assigned(FPlanner) then
    Exit;

  case Mode of
  dmMultiResource:
    begin
      FPlanner.Positions := NumberOfResources;
      Span := 1;
      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        if not (FPlanner.Sidebar.Position = spRight) then
          FPlanner.Header.Captions.Add('');

        for i := 1 to NumberOfResources do
        begin
          ResName := ResourceMap.PositionName(i - 1);
          if Assigned(FOnGetResourceName) then
            FOnGetResourceName(Self,i,ResName);
          if ResName <> '' then
          begin
            if FPlanner.Header.Captions.Count <= i then
              FPlanner.Header.Captions.Add(ResName)
            else
              FPlanner.Header.Captions.Strings[i] := ResName;
          end;
        end;
      end;
      FPlanner.Display.CurrentPosFrom := -1;
      FPlanner.Display.CurrentPosTo := -1;
    end;
  dmMultiDay:
    begin
      FPlanner.Positions := NumberOfDays;
      Span := NumberOfDays;

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        if not (FPlanner.Sidebar.Position = spRight) then
          FPlanner.Header.Captions.Add('');

        for i := 1 to NumberOfDays do
        begin
          if FPlanner.Header.Captions.Count <= i then
            FPlanner.Header.Captions.Add(FormatDateTime(FDateFormat,Day + i - 1))
          else
            FPlanner.Header.Captions.Strings[i] := FormatDateTime(FDateFormat,Day + i - 1);
        end;
      end;
      dx := Round(Int(Now) - Int(Day));
      FPlanner.Display.CurrentPosFrom := dx;
      FPlanner.Display.CurrentPosTo := dx;
    end;
  dmMultiDayRes:
    begin
      FPlanner.Positions := NumberOfResources * NumberOfDays;
      FPlanner.PositionGroup := NumberOfDays;
      Span := NumberOfDays;

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        if not (FPlanner.Sidebar.Position = spRight) then
          FPlanner.Header.Captions.Add('');
        FPlanner.Header.GroupCaptions.Clear;
        FPlanner.Header.GroupCaptions.Add('');

        for i := 1 to NumberOfResources do
        begin
          for j := 1 to NumberOfDays do
          begin
             k := j + (i - 1) * NumberOfDays;

             if FPlanner.Header.Captions.Count <= k then
               FPlanner.Header.Captions.Add(FormatDateTime(FDateFormat,Day + j - 1))
             else
               FPlanner.Header.Captions.Strings[k] := FormatDateTime(FDateFormat,Day + j - 1);
          end;

          ResName := ResourceMap.PositionName(i - 1);
          if Assigned(FOnGetResourceName) then
            FOnGetResourceName(Self,i,ResName);

          if FPlanner.Header.GroupCaptions.Count < i then
            FPlanner.Header.GroupCaptions.Add(ResName)
          else
            FPlanner.Header.GroupCaptions[i - 1] := ResName;
        end;
      end;
      FPlanner.Display.CurrentPosFrom := -1;
      FPlanner.Display.CurrentPosTo := -1;
    end;
  dmMultiResDay:
    begin
      FPlanner.PositionGroup := NumberOfResources;
      FPlanner.Positions := NumberOfResources * NumberOfDays;
      Span := NumberOfDays;

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        if not (FPlanner.Sidebar.Position = spRight) then
          FPlanner.Header.Captions.Add('');
        FPlanner.Header.GroupCaptions.Clear;
        FPlanner.Header.GroupCaptions.Add('');

        for i := 1 to NumberOfDays do
        begin
          if FPlanner.Header.GroupCaptions.Count < i then
            FPlanner.Header.GroupCaptions.Add(FormatDateTime(FDateFormat,Day + i - 1))
          else
            FPlanner.Header.GroupCaptions[i - 1] := FormatDateTime(FDateFormat,Day + i - 1);
        end;

        for i := 1 to NumberOfDays do
          for j := 1 to NumberOfResources do
          begin
            ResName := ResourceMap.PositionName(j - 1);
            if Assigned(FOnGetResourceName) then
              FOnGetResourceName(Self,j,ResName);
            k := j + (i - 1) * NumberOfResources;
            if FPlanner.Header.Captions.Count <= k then
              FPlanner.Header.Captions.Add(ResName)
            else
              FPlanner.Header.Captions.Strings[k] := ResName;
          end;
      end;
      dx := Round(Int(Now) - Int(Day));
      FPlanner.Display.CurrentPosFrom := dx * NumberOfResources;
      FPlanner.Display.CurrentPosTo := (dx + 1) * NumberOfResources - 1;
    end;
  end;

end;

function TDBDaySource.CalcItemPos(DatePos, ResourcePos: Integer): Integer;
begin
  case Mode of
  dmMultiDay:
    Result := DatePos;
  dmMultiResource:
    Result := ResourcePos;
  dmMultiResDay:
    Result := ResourcePos + NumberOfResources * DatePos;
  dmMultiDayRes:
    Result := ResourcePos * NumberOfDays + DatePos;
  else
    Result := DatePos;
  end;
end;


procedure TDBDaySource.MapItemTimeOnPlanner(APlannerItem: TPlannerItem);
var
  dts,dte: TDateTime;
  j, DatePos: Integer;
begin
  with APlannerItem do
  begin
    RealTime := True;
    dts := ItemRealStartTime;
    dte := ItemRealEndTime;

    // map to start of day
    if Int(dts) < Int(FDay) then
      ChangeCrossing;

    InHeader := dte - dts > 1;

    DatePos := Round(Int(dts - FDay));

    ItemPos := CalcItemPos(DatePos,ItemPos);

    if APlannerItem.InHeader then
    begin
      for j := 1  to round(Int(dte) - Int(dts)) do
        begin
          if j + APlannerItem.ItemPos < FPlanner.Positions then
            with FPlanner.CloneItem(APlannerItem) do
              ItemPos := ItemPos + j;
        end;
    end;
  end;
end;

procedure TDBDaySource.ResourceUpdate;
var
  Span: Integer;
begin
  inherited;
  ConfigurePlanner(Span);
end;

{ TDBCustomPeriodSource }

procedure TDBCustomPeriodSource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;    
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;

  TimeToFields(D.Fields,FPlanner.Items.DBItem.ItemRealStartTime,
    FPlanner.Items.DBItem.ItemRealEndTime);


  if (ResourceField <> '') then
  begin
    ResToFields(D.Fields, FPlanner.Items.DBItem.ItemPos);
  end;

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;


procedure TDBCustomPeriodSource.ConfigurePlanner;
var
  i: Integer;
  ResName: string;
begin
  if not Assigned(FPlanner) then
    Exit;

  FPlanner.Positions := NumberOfResources;

  if FAutoHeaderUpdate then
  begin
    FPlanner.Header.Captions.Clear;
    for i := 0 to NumberOfResources do
       FPlanner.Header.Captions.Add('');

    for i := 1 to NumberOfResources do
    begin
      ResName := ResourceMap.PositionName(i - 1);
      if Assigned(FOnGetResourceName) then
        FOnGetResourceName(Self,i,ResName);
      if ResName <> '' then
      begin
        FPlanner.Header.Captions.Strings[i] := ResName;
      end;
    end;
  end;
end;

constructor TDBCustomPeriodSource.Create(AOwner: TComponent);
begin
  inherited;
  FNumberOfResources := 1;
end;

procedure TDBCustomPeriodSource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;
  
  EndUpdate;
end;

procedure TDBCustomPeriodSource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

procedure TDBCustomPeriodSource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dts,dte: TDateTime;
  D: TDataSet;

begin
  inherited;
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}
    FPlanner.Items.DBItem := plIt;

    if plIt.Recurrent then
    begin
      TDBPlanner(FPlanner).ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      FPlanner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;

    FieldsToTime(D.Fields,dts,dte);

    if (dts < FPlanner.Mode.PeriodEndDate) and
       (dte > FPlanner.Mode.PeriodStartDate) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;

      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        EndUpdate;
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;
  end;

  EndUpdate;
end;

procedure TDBCustomPeriodSource.Next;
begin
  inherited;

end;

procedure TDBCustomPeriodSource.Prev;
begin
  inherited;

end;

procedure TDBCustomPeriodSource.ReadDBItem;
var
  D: TDataSet;
  dtS, dtE: TDateTime;
  ResourcePos: Integer;
  Accept: Boolean;
  B: TBookmark;


begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  B := D.GetBookMark;

  // avoid that read it triggered multiple times due to DataLink methods RecordChanged

  BeginUpdate;

  try
    D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

    if FPlanner.Items.DBItem.Recurrent then
    begin
      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := 0;
      FRecurrencyHandler.Generate;
    end;

    with FPlanner.Items.DBItem do
    begin
      FieldsToTime(D.Fields,dtS,dtE);
      ItemStartTime := dtS;
      ItemEndTime := dtE;

      if (ResourceField <> '') then
      begin
        FieldsToRes(D.Fields, ResourcePos, Accept);
        ItemPos := ResourcePos;
      end;

      if SubjectField <> '' then
        CaptionText := D.FieldByName(SubjectField).AsString;

      if NotesField <> '' then
        Text.Text := D.FieldByName(NotesField).AsString;

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;
    end;

    if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
      FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

    if Assigned(FOnFieldsToItem) then
      FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);

    D.GotoBookMark(B);

  finally
    EndUpdate;
  end;

  D.FreeBookMark(B);
end;

procedure TDBCustomPeriodSource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dts,dte,rdts,rdte: TDateTime;
  plIt: TPlannerItem;
  Accept: Boolean;
  ResourcePos: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  ConfigurePlanner;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

  FPlanner.Items.BeginUpdate;

  while not D.Eof do
  begin
    Accept := True;

    FieldsToTime(D.Fields,dts,dte);

    rdts := dts;
    rdte := dte;

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := FPlanner.Mode.PeriodEndDate + 1;
      FRecurrencyHandler.Generate;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
        if (ResourceField <> '') and
           (dts < FPlanner.Mode.PeriodEndDate + 1) and
           (dte >= FPlanner.Mode.PeriodStartDate) then
        begin
          FieldsToRes(D.Fields, ResourcePos, Accept);
        end;

        if (dts < FPlanner.Mode.PeriodEndDate + 1) and
           (dte >= FPlanner.Mode.PeriodStartDate) and
           Accept and Assigned(FOnAccept) then
        begin
          FOnAccept(Self,D.Fields,Accept);
        end;

        if (dts < FPlanner.Mode.PeriodEndDate + 1) and
           (dte >= FPlanner.Mode.PeriodStartDate) and Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' Add:'+D.FieldByName(KeyField).AsString);
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          with plIt do
          begin
            Assign(FPlanner.DefaultItem);

            Recurrent := FRecurrencyHandler.IsRecurrent;

            RecurrentOrigStart := dts;
            RecurrentOrigEnd := dte;

            RecurrentStart := rdts;
            RecurrentEnd := rdte;

            ItemStartTime := dts;
            ItemEndTime := dte;

            Recurrency := FRecurrencyHandler.Recurrency;

            if (ResourceField <> '') then
            begin
              FieldsToRes(D.Fields, ResourcePos, Accept);
              ItemPos := ResourcePos;
            end;

            FieldsToItem(plIt);

            if FReadOnly then
            begin
              FixedPos := True;
              FixedSize := True;
              ReadOnly := True;
            end;

          end;

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);
        end;
      end;
    end;
    D.Next;
  end;

  D.GotoBookMark(B);
  D.EnableControls;
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);  
  
  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}
  FPlanner.Items.ResolveLinks;
end;


procedure TDBCustomPeriodSource.ResourceUpdate;
begin
  inherited;
  ConfigurePlanner;
end;

procedure TDBCustomPeriodSource.SetNumberOfResources(const Value: Integer);
begin
  if Value <= 0 then
    Exit;
  if FNumberOfResources <> Value then
  begin
    FNumberOfResources := Value;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBCustomPeriodSource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dts,dte: TDateTime;
  plIt: TPlannerItem;
  i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;


  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);

    if (dts < FPlanner.Mode.PeriodEndDate) and
       (dte > FPlanner.Mode.PeriodStartDate) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;
    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
      FPlanner.Items[i - 1].Free;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBCustomPeriodSource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;
  deltastart, deltaend: TDateTime;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
    D.Edit;
  except
    D.FreeBookMark(B);    
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := FPlanner.Items.DBItem.ItemRealStartTime;
    DTE := FPlanner.Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := FPlanner.Items.DBItem.ItemStartTime;
    DTE := FPlanner.Items.DBItem.ItemEndTime;
  end;

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  FPlanner.Items.DBItem.RealTime := False;

  TimeToFields(D.Fields,DTS,DTE);

  if ResourceField <> '' then
  begin
    ResToFields(D.Fields, FPlanner.Items.DBItem.ItemPos);
  end;

  if (SubjectField <> '') and
     (D.FieldByName(SubjectField).CanModify) then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     (D.FieldByName(NotesField).CanModify) and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;

{ TDBPeriodSource }

constructor TDBPeriodSource.Create(AOwner: TComponent);
begin
  inherited;
  // initialize it to a default 14 day period
  FStartDate := Trunc(Now);
  FEndDate := Trunc(Now) + 14;
end;

procedure TDBPeriodSource.Loaded;
begin
  inherited;
  if Assigned(FPlanner) then
    UpdatePlanner;
end;

procedure TDBPeriodSource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
    UpdatePlanner;
end;

procedure TDBPeriodSource.SetDateRange(AStartDate, AEndDate: TDateTime);
begin
  FStartDate := AStartDate;
  FEndDate := AEndDate;

  if not Assigned(FPlanner) then
    Exit;
  if csLoading in ComponentState then
    Exit;

  FPlanner.Mode.PeriodStartDate := AStartDate;
  FPlanner.Mode.PeriodEndDate := AEndDate;
end;

procedure TDBPeriodSource.SetEndDate(const Value: TDateTime);
begin
  if (FEndDate <> Value) and ((Value >= FStartDate) or not Active) then
  begin
    FEndDate := Value;
    if not Assigned(FPlanner) then
      Exit;
    if csLoading in ComponentState then
      Exit;

    FPlanner.Mode.PeriodEndDate := Value;
  end;
end;

procedure TDBPeriodSource.SetStartDate(const Value: TDateTime);
begin
  if (FStartDate <> Value) and ((Value <= FEndDate) or not Active) then
  begin
    FStartDate := Value;
    if not Assigned(FPlanner) then
      Exit;
    if csLoading in ComponentState then
      Exit;

    FPlanner.Mode.PeriodStartDate := Value;
  end;
end;

procedure TDBPeriodSource.UpdatePlanner;
begin
  if Assigned(FPlanner) and (FUpdateCount = 0) then
  begin
    FPlanner.Mode.PlannerType := plDayPeriod;
    FPlanner.Mode.BeginUpdate;
    FPlanner.Mode.PeriodEndDate := FEndDate;
    FPlanner.Mode.PeriodStartDate := FStartDate;
    FPlanner.Mode.EndUpdate;
    FPlanner.Items.BeginUpdate;
    ConfigurePlanner;
    ClearDBItems;
    ReadDBItems('');
    FPlanner.Items.EndUpdate;
  end;
end;

{ TDBMonthSource }

function DaysInMonth(mo,ye:word):word;
begin
  if (mo < 1) or (mo > 12) then
    mo := 1;

  if (mo <> 2) then
    Result := CDaysInMonth[mo]
  else
  begin
    if ye mod 4 = 0 then
      Result := 29
    else
      Result := 28;

    if ye mod 100 = 0 then

      Result := 28;

    if ye mod 400 = 0 then
      Result := 29;
  end;
end;

constructor TDBMonthSource.Create(AOwner: TComponent);
var
  da,mo,ye: word;
begin
  inherited;
  DecodeDate(Now,ye,mo,da);
  FMonth := mo;
  FYear := ye;
end;

procedure TDBMonthSource.Loaded;
begin
  inherited;
  if Assigned(FPlanner) then
  begin
    UpdatePlanner;
    FPlanner.Mode.PlannerType := plMonth;
    FPlanner.Mode.Year := FYear;
    FPlanner.Mode.Month := FMonth;
    FPlanner.Display.DisplayStart := 0;
    FPlanner.Display.DisplayEnd := DaysInMonth(FMonth,FYear) - 1;
  end;
end;

procedure TDBMonthSource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
  begin
    FPlanner.Mode.PlannerType := plMonth;
    FPlanner.Mode.Year := FYear;
    FPlanner.Mode.Month := FMonth;
  end;
end;


procedure TDBMonthSource.SetMonth(const Value: integer);
begin
  if (Value > 0) and (Value < 13) and (Value <> FMonth) then
  begin
    FMonth := Value;

    if not Assigned(FPlanner) then Exit;
    if csLoading in ComponentState then Exit;
    FPlanner.Mode.Month := Value;

    { reload items to reflect the new month }
    if CheckDataSet then
      UpdatePlanner;
  end;
end;


procedure TDBMonthSource.SetYear(const Value: integer);
begin
  if (Value > 0) and (Value <> FYear) then
  begin
    FYear := Value;
    if not Assigned(FPlanner) then Exit;
    if csLoading in ComponentState then Exit;

    FPlanner.Mode.Year := Value;

    { reload items to reflect the new month }
    if CheckDataSet then
      UpdatePlanner;
  end;
end;

procedure TDBMonthSource.UpdatePlanner;
begin
  if Assigned(FPlanner) then
  begin
    FPlanner.Display.DisplayStart := 0;
    FPlanner.Display.DisplayEnd := DaysInMonth(FMonth,FYear) - 1;

    FPlanner.Items.BeginUpdate;
    ConfigurePlanner;
    ClearDBItems;
    ReadDBItems('');
    FPlanner.Items.EndUpdate;
  end;
end;

{ TDBWeekSource }

constructor TDBWeekSource.Create(AOwner: TComponent);
var
  da,mo,ye: word;
begin
  inherited;
  DecodeDate(Now,ye,mo,da);
  FMonth := mo;
  FYear := ye;
  FWeekStart := 0;
  FWeeks := 4;
end;


procedure TDBWeekSource.Loaded;
begin
  inherited;
  if Assigned(FPlanner) then
  begin
    FPlanner.Mode.PlannerType := plWeek;
    FPlanner.Mode.Year := FYear;
    FPlanner.Mode.Month := FMonth;
    FPlanner.Mode.WeekStart := FWeekStart;
    FPlanner.Display.DisplayStart := 0;
    FPlanner.Display.DisplayEnd := FWeeks * 7;
  end;
end;

procedure TDBWeekSource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
  begin
    FPlanner.Mode.PlannerType := plWeek;
    FPlanner.Mode.Year := FYear;
    FPlanner.Mode.Month := FMonth;
    FPlanner.Mode.WeekStart := FWeekStart;
  end;
end;

procedure TDBWeekSource.SetMonth(const Value: integer);
begin
  if (Value > 0) and (Value < 13) and (Value <> FMonth) then
  begin
    FMonth := Value;

    if not Assigned(FPlanner) then Exit;
    if csLoading in ComponentState then Exit;
    FPlanner.Mode.Month := Value;

    { reload items to reflect the new month }
    if CheckDataSet then
      UpdatePlanner;
  end;
end;

procedure TDBWeekSource.SetWeeks(const Value: Integer);
begin
  if FWeeks <> Value then
  begin
    FWeeks := Value;
    if CheckDataSet then
      UpdatePlanner;
  end;
end;

procedure TDBWeekSource.SetWeekStart(const Value: Integer);
begin
  FWeekStart := Value;
  if Assigned(FPlanner) then
  begin
    FPlanner.Mode.WeekStart := FWeekStart;
  end;
end;

procedure TDBWeekSource.SetYear(const Value: integer);
begin
  if (Value > 0) and (Value <> FYear) then
  begin
    FYear := Value;
    if not Assigned(FPlanner) then Exit;
    if csLoading in ComponentState then Exit;

    FPlanner.Mode.Year := Value;

    { reload items to reflect the new month }
    if CheckDataSet then
      UpdatePlanner;
  end;
end;

procedure TDBWeekSource.UpdatePlanner;
begin
  FPlanner.Display.DisplayStart := 0;
  FPlanner.Display.DisplayEnd := FWeeks * 7;

  FPlanner.Items.BeginUpdate;
  ClearDBItems;
  ReadDBItems('');
  FPlanner.Items.EndUpdate;
end;

{ TDBMultiMonthSource }

procedure TDBMultiMonthSource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;  

  TimeToFields(D.Fields,FPlanner.Items.DBItem.ItemRealStartTime,
    FPlanner.Items.DBItem.ItemRealEndTime);

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;

procedure TDBMultiMonthSource.ConfigurePlanner;
var
  i,j: Integer;
begin
  if not Assigned(FPlanner) then
    Exit;

  FPlanner.Positions := NumberOfMonths;
  FPlanner.Display.DisplayStart := 0;
  FPlanner.Display.DisplayEnd := 30;

  if FAutoHeaderUpdate then
  begin
    j := FStartMonth;
    for i := 1 to NumberOfMonths do
    begin
      {$IFNDEF DELPHIXE_LVL}
      FPlanner.Header.Captions.Strings[i] := LongMonthNames[j];
      {$ENDIF}
      {$IFDEF DELPHIXE_LVL}
      FPlanner.Header.Captions.Strings[i] := FormatSettings.LongMonthNames[j];
      {$ENDIF}
      inc(j);
      if j > 12 then
        j := 1;
    end;
  end;  
end;

constructor TDBMultiMonthSource.Create(AOwner: TComponent);
var
  da,mo,ye: word;
begin
  inherited;
  FStartMonth := 1;
  FMonthIncrement := 1;
  FNumberOfMonths := 3;
  DecodeDate(Now, ye, mo, da);
  FYear := ye;
end;

procedure TDBMultiMonthSource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;


  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;
  
  EndUpdate;
end;

procedure TDBMultiMonthSource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

procedure TDBMultiMonthSource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dts,dte: TDateTime;
  D: TDataSet;

begin
  inherited;
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;
  if not Assigned(FPlanner) then Exit;


  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  //D.DisableControls;
  //D.EnableControls;

  if Assigned(plIt) then
  begin

    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}

    FPlanner.Items.DBItem := plIt;

    if (plIt.Recurrent) then
    begin
      // this removes all recurrent items
      TDBPlanner(FPlanner).ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      FPlanner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
    
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;

    FieldsToTime(D.Fields,dts,dte);

    if (dts < Int(FEndDate) + 1) and
       (dte >= FStartDate) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;
      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        EndUpdate;
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;
  end;

  EndUpdate;
end;

procedure TDBMultiMonthSource.Loaded;
begin
  inherited;
  if Assigned(FPlanner) then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.Next;
var
  NSM,NSY: Integer;
begin
  NSM := StartMonth + FMonthIncrement;
  NSY := Year;
  if NSM > 12 then
  begin
    NSM := 1;
    NSY := Year + 1;
  end;
  FStartMonth := NSM;
  FYear := NSY;
  if CheckDataSet then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.Prev;
var
  NSM,NSY: Integer;
begin
  NSM := StartMonth - FMonthIncrement;
  NSY := Year;
  if NSM < 1 then
  begin
    NSM := 12;
    NSY := Year - 1;
  end;
  FStartMonth := NSM;
  FYear := NSY;
  if CheckDataSet then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.ReadDBItem;
var
  D: TDataSet;
  dt, dts, dte: TDateTime;
  smo,sye,sda: word;
  emo,eye,eda: word;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

//  B := D.GetBookMark;
//  D.Locate(KeyField,APlanner.Items.Selected.DBKey,[]);

  FPlanner.RemoveClones(FPlanner.Items.DBItem);

  FieldsToTime(D.Fields,dts,dte);

  dt := dts;

  if dts >= FStartDate then
    DecodeDate(dts,sye,smo,sda)
  else
    DecodeDate(FStartDate,sye,smo,sda);

  if dte < Int(FEndDate) + 1 then
    DecodeDate(dte,eye,emo,eda)
  else
    DecodeDate(FEndDate,eye,emo,eda);

  if (smo <> emo) then
  begin
    with FPlanner.Items.DBItem do
    begin
      Assign(FPlanner.DefaultItem);

      if dts >= FStartDate then
        ItemStartTime := dts
      else
        ItemStartTime := FStartDate;

//      if dte < Int(FEndDate) + 1 then
        ItemEndTime := EndOfMonth(dts);
//      else
//        ItemEndTime := FEndDate;

      FixedPos := True;
      FixedPosition := True;
      FixedSize := True;

      RealTime := True;
      ItemRealStartTime := dts;
      ItemRealEndTime := dte;

      FieldsToItem(FPlanner.Items.DBItem);

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;

      while (smo < emo) do
      begin
        dts := EndOfMonth(dts) + 1;
        smo := NextMonth(smo);
        with FPlanner.CloneItem(FPlanner.Items.DBItem) do
        begin
          ItemStartTime := dts;
          if smo = emo then
            ItemEndTime := dte
          else
            ItemEndTime := EndOfMonth(dts);

          RealTime := True;
          ItemRealStartTime := dt;
          ItemRealEndTime := dte;
        end;
      end;
    end;
  end
  else
  begin
    with FPlanner.Items.DBItem do
    begin
      Assign(FPlanner.DefaultItem);

      if dts >= FStartDate then
        ItemStartTime := dts
      else
        ItemStartTime := FStartDate;

      if dte < Int(FEndDate) + 1 then
        ItemEndTime := dte
      else
        ItemEndTime := FEndDate;

      RealTime := True;
      ItemRealStartTime := dts;
      ItemRealEndTime := dte;

      FieldsToItem(FPlanner.Items.DBItem);

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;

    end;
  end;

  if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
    FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

  if Assigned(FOnFieldsToItem) then
    FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);
end;

procedure TDBMultiMonthSource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dt,dts,dte,rdts,rdte,cdts,cdte: TDateTime;
  plIt: TPlannerItem;
  Accept: Boolean;
  smo,sye,sda: word;
  emo,eye,eda: word;


begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  ConfigurePlanner;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

  FPlanner.Items.BeginUpdate;

  while not D.Eof do
  begin
    Accept := True;

    FieldsToTime(D.Fields,dts,dte);

    dt := dts;

    rdts := dts;
    rdte := dte;

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := FEndDate;
      FRecurrencyHandler.Generate;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
        if (dts <= FEndDate) and (dte >= FStartDate) and
           Accept and Assigned(FOnAccept) then
        begin
          FOnAccept(Self,D.Fields,Accept);
        end;

        if (dts < Int(FEndDate) + 1) and
           (dte >= FStartDate) and Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' Add:'+D.FieldByName(KeyField).AsString);
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          if dts >= FStartDate then
            DecodeDate(dts,sye,smo,sda)
          else
            DecodeDate(FStartDate,sye,smo,sda);

          if dte < Int(FEndDate) + 1 then
            DecodeDate(dte,eye,emo,eda)
          else
            DecodeDate(FEndDate,eye,emo,eda);

          plIt.Assign(FPlanner.DefaultItem);
          plIt.Recurrent := FRecurrencyHandler.IsRecurrent;
          if dts >= FStartDate then
            cdts := dts
          else
            cdts := FStartDate;

          if (smo <> emo) then // month crossing
          begin
            with plIt do
            begin
              ItemStartTime := cdts;

//              if dte < Int(FEndDate) + 1 then
                cdte := EndOfMonth(dts);
//              else
//                cdte := FEndDate;

              ItemEndTime := cdte;

              FixedPosition := True;

              while (smo < emo) and (cdts < FEndDate) do
              begin
                cdts := EndOfMonth(cdts) + 1;
                smo := NextMonth(smo);

                with FPlanner.CloneItem(plIt) do
                begin
                  ItemStartTime := cdts;

                  if smo = emo then
                    ItemEndTime := dte
                  else
                    ItemEndTime := EndOfMonth(cdts);

                  RealTime := True;
                  ItemRealStartTime := dt;
                  ItemRealEndTime := dte;
                end;
              end;

            end;
          end
          else
          begin
            with plIt do
            begin
              ItemStartTime := cdts;

              if dte < Int(FEndDate) + 1 then
                cdte := dte
              else
                cdte := FEndDate;

              ItemEndTime := cdte;
            end;
          end;

          plIt.RealTime := True;
          plIt.ItemRealStartTime := dts;
          plIt.ItemRealEndTime := dte;

          if FReadOnly then
          begin
            plIt.FixedPos := True;
            plIt.FixedSize := True;
            plIt.ReadOnly := True;
          end;

          plIt.RecurrentOrigStart := dts;
          plIt.RecurrentOrigEnd := dte;

          plIt.RecurrentStart := rdts;
          plIt.RecurrentEnd := rdte;
          plIt.Recurrency := FRecurrencyHandler.Recurrency;

          plit.Visible := true;

          FieldsToItem(plIt);

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);
        end;
      end;
    end;
    D.Next;
  end;

  D.GotoBookMark(B);
  D.EnableControls;
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);

  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}
  FPlanner.Items.ResolveLinks;
end;

procedure TDBMultiMonthSource.ResourceUpdate;
begin
  inherited;
  ConfigurePlanner;
end;

procedure TDBMultiMonthSource.SetNumberOfMonths(const Value: Integer);
begin
  FNumberOfMonths := Value;
  if CheckDataSet then
    UpdatePlanner
  else
    if Assigned(FPlanner) then
      FPlanner.Positions := Value;
end;

procedure TDBMultiMonthSource.SetStartMonth(const Value: Integer);
begin
  if (Value <= 0) or (Value > 12) then
    Exit;

  FStartMonth := Value;
  if CheckDataSet then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.SetYear(const Value: Integer);
begin
  FYear := Value;
  { reload items to reflect the new year }
  if CheckDataSet then
    UpdatePlanner;
end;

procedure TDBMultiMonthSource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dts,dte: TDateTime;
  plIt: TPlannerItem;
  i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;

  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);

    if (dts < Int(FEndDate) + 1) and
       (dte >= FStartDate) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;
    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
      FPlanner.Items[i - 1].Free;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBMultiMonthSource.UpdatePlanner;
var
  Ey,Em: Integer;

begin
  FPlanner.Positions := FNumberOfMonths;
  FPlanner.Mode.Month := FStartMonth;
  FPlanner.Mode.Year := FYear;
  FPlanner.Mode.PlannerType := plMultiMonth;

  FStartDate := EncodeDate(FYear,FStartMonth,1);

//  Ey := FYear + (FStartMonth + FNumberOfMonths - 1) div 12;

  Ey := FYear;

  Em := (FStartMonth + FNumberOfMonths - 1);

  while Em > 12 do
  begin
    Em := Em - 12;
    Inc(Ey);
  end;

  FEndDate := EncodeDate(Ey,Em,DaysInMonth(Em,Ey));

  ConfigurePlanner;
  FPlanner.Items.BeginUpdate;
  ClearDBItems;
  ReadDBItems('');
  FPlanner.Items.EndUpdate;
end;

procedure TDBMultiMonthSource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;
  deltastart,deltaend: TDateTime;
begin
  if IsUpdating then Exit;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
    D.Edit;
  except
    D.FreeBookMark(B);    
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := FPlanner.Items.DBItem.ItemRealStartTime;
    DTE := FPlanner.Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := FPlanner.Items.DBItem.ItemStartTime;
    DTE := FPlanner.Items.DBItem.ItemEndTime;
  end;

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  TimeToFields(D.Fields,DTS, DTE);

  if (SubjectField <> '') and
     (D.FieldByName(SubjectField).CanModify) then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     (D.FieldByName(NotesField).CanModify) and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  //ReadDBItem;

  FPlanner.Items.Select(FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;

{ TDBTimeLineSource }

procedure TDBTimeLineSource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;    
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;

  TimeToFields(D.Fields,FPlanner.Items.DBItem.ItemStartTime,
    FPlanner.Items.DBItem.ItemEndTime);


  if (ResourceField <> '') then
  begin
    ResToFields(D.Fields,FPlanner.Items.DBItem.ItemPos);
  end;

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;

procedure TDBTimeLineSource.ConfigurePlanner;
var
  i: Integer;
  ResName: string;
begin
  if not Assigned(FPlanner) then
    Exit;

  FPlanner.Positions := NumberOfResources;

  if FAutoHeaderUpdate then
  begin
    FPlanner.Header.Captions.Clear;
    FPlanner.Header.Captions.Add('');

    for i := 1 to NumberOfResources do
    begin
      ResName := ResourceMap.PositionName(i - 1);
      if Assigned(FOnGetResourceName) then
        FOnGetResourceName(Self,i,ResName);
      if ResName <> '' then
      begin
        if FPlanner.Header.Captions.Count <= i then
          FPlanner.Header.Captions.Add(ResName)
        else
          FPlanner.Header.Captions.Strings[i] := ResName;
      end;
    end;
  end;
  FPlanner.Display.CurrentPosFrom := -1;
  FPlanner.Display.CurrentPosTo := -1;
end;

constructor TDBTimeLineSource.Create(AOwner: TComponent);
begin
  inherited;
  FStartDate := Int(Now);
  FEndDate := Int(Now) + 3;
  FNumberOfResources := 3;
end;

procedure TDBTimeLineSource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;


  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;
  
  EndUpdate;
end;

procedure TDBTimeLineSource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

procedure TDBTimeLineSource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dts,dte: TDateTime;
  D: TDataSet;

begin
  inherited;
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}
    FPlanner.Items.DBItem := plIt;

    if plIt.Recurrent then
    begin
      TDBPlanner(FPlanner).ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      FPlanner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;

    FieldsToTime(D.Fields,dts,dte);

    if (dts < FPlanner.Mode.PeriodEndDate) and
       (dte > FPlanner.Mode.PeriodStartDate) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;

      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        EndUpdate;
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;
  end;

  EndUpdate;
end;

procedure TDBTimeLineSource.Next;
begin
  inherited;
end;

procedure TDBTimeLineSource.PlannerChanged;
begin
  inherited;
  UpdatePlanner;
end;

function TDBTimeLineSource.PosToRes(Pos: Integer): Integer;
begin
  Result := Pos;
end;

procedure TDBTimeLineSource.Prev;
begin
  inherited;
end;

procedure TDBTimeLineSource.ReadDBItem;
var
  D: TDataSet;
  dts, dte: TDateTime;
  ResourcePos: Integer;
  Accept: Boolean;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;


//  B := D.GetBookMark;
//  D.Locate(KeyField,APlanner.Items.Selected.DBKey,[]);

  FPlanner.RemoveClones(FPlanner.Items.DBItem);

  FieldsToTime(D.Fields,dts,dte);

  with FPlanner.Items.DBItem do
  begin
    Assign(FPlanner.DefaultItem);

    ItemStartTime := dts;
    ItemEndTime := dte;

    // make sure that items that are within a timeslot still get displayed
    if ItemEnd = ItemBegin then
      ItemEnd := ItemEnd + 1;

    FieldsToItem(FPlanner.Items.DBItem);

    if RecurrencyField <> '' then
      Recurrency := D.FieldByName(RecurrencyField).AsString;

    if (ResourceField <> '') then
    begin
      FieldsToRes(D.Fields, ResourcePos, Accept);
      ItemPos := ResourcePos;
    end;

    if FReadOnly then
    begin
      FixedPos := True;
      FixedSize := True;
      ReadOnly := True;
    end;
  end;

  if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
    FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

  if Assigned(FOnFieldsToItem) then
    FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);
end;

procedure TDBTimeLineSource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dts,dte,rdts,rdte: TDateTime;
  plIt: TPlannerItem;
  Accept: Boolean;
  ResourcePos: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);
  
  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  ConfigurePlanner;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

  FPlanner.Items.BeginUpdate;

  {$IFDEF TMSDEBUG}
  outputdebugstring(pchar('datezone:'+formatdatetime('dd/mm/yyyy hh:nn:ss',FStartDate)+' to ' + formatdatetime('dd/mm/yyyy hh:nn:ss',FEndDate)));
  {$ENDIF}

  while not D.Eof do
  begin
    Accept := True;

    FieldsToTime(D.Fields,dts,dte);

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if (dts <= FEndDate) and
         (dte >= FStartDate) and
         Accept and Assigned(FOnAccept) then
      begin
        FOnAccept(Self,D.Fields,Accept);
      end;

      {$IFDEF TMSDEBUG}
      outputdebugstring(pchar('read:'+formatdatetime('dd/mm/yyyy hh:nn:ss',dts)+' to ' + formatdatetime('dd/mm/yyyy hh:nn:ss',dte)));
      {$ENDIF}

      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';   

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := FEndDate;
      FRecurrencyHandler.Generate;

      rdts := dts;
      rdte := dte;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
        if (int(dts) <= int(FEndDate)) and
           (int(dte) >= int(FStartDate)) and Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' Add:'+D.FieldByName(KeyField).AsString);
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          with plIt do
          begin
            Assign(FPlanner.DefaultItem);

            Recurrent := FRecurrencyHandler.IsRecurrent;

            ItemRealStartTime := dts;
            ItemRealEndTime := dte;

            RecurrentOrigStart := dts;
            RecurrentOrigEnd := dte;

            RecurrentStart := rdts;
            RecurrentEnd := rdte;

            ItemStartTime := dts;
            ItemEndTime := dte;

            Recurrency := FRecurrencyHandler.Recurrency;

            FieldsToItem(plIt);

            if (ResourceField <> '') then
            begin
              FieldsToRes(D.Fields, ResourcePos, Accept);
              ItemPos := ResourcePos;
            end;

            if FReadOnly then
            begin
              FixedPos := True;
              FixedSize := True;
              ReadOnly := True;
            end;
          end;

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);
        end;
      end;
    end;  
    D.Next;
  end;

  D.GotoBookMark(B);
  D.EnableControls;
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);  

  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}
  FPlanner.Items.ResolveLinks;  
end;

procedure TDBTimeLineSource.ResourceUpdate;
begin
  inherited;
  ConfigurePlanner;
end;

procedure TDBTimeLineSource.SetEndDate(const Value: TDateTime);
begin
  FEndDate := Value;
  UpdatePlanner;
end;

procedure TDBTimeLineSource.SetNumberOfResources(const Value: Integer);
begin
  FNumberOfResources := Value;
  UpdatePlanner;
end;

procedure TDBTimeLineSource.SetStartDate(const Value: TDateTime);
begin
  FStartDate := Value;
  UpdatePlanner;
end;

procedure TDBTimeLineSource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dts,dte: TDateTime;
  plIt: TPlannerItem;
  i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;

  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);

    if (dts <= FEndDate) and
       (dte >= FStartDate) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;
    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
      FPlanner.Items[i - 1].Free;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBTimeLineSource.UpdatePlanner;
var
  minsvisible: integer;
begin
  if Assigned(FPlanner) and Active then
  begin
    FPlanner.Mode.PlannerType := plTimeLine;
    FPlanner.Mode.TimeLineStart := int(FStartDate);

    FPlanner.Positions := FNumberOfResources;

    minsvisible := MININDAY;

    minsvisible := minsvisible - (FPlanner.Display.DisplayUnit * FPlanner.Mode.TimeLineNVUBegin);
    minsvisible := minsvisible - (FPlanner.Display.DisplayUnit * FPlanner.Mode.TimeLineNVUEnd);

    if (FPlanner.Display.DisplayUnit > 0) then
      FPlanner.Display.DisplayEnd := ((Round(1 + FEndDate - FStartDate)) * minsvisible) div FPlanner.Display.DisplayUnit - 1;
  end;   
end;

procedure TDBTimeLineSource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  ResPos: Integer;
  deltastart, deltaend: TDateTime;
  DTE, DTS: TDateTime;    
begin
  if IsUpdating then Exit;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
    D.Edit;
  except
    D.FreeBookMark(B);    
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := FPlanner.Items.DBItem.ItemRealStartTime;
    DTE := FPlanner.Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := FPlanner.Items.DBItem.ItemStartTime;
    DTE := FPlanner.Items.DBItem.ItemEndTime;
  end;

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  TimeToFields(D.Fields,dts,dte);

  if (SubjectField <> '') and
     D.FieldByName(SubjectField).CanModify then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     D.FieldByName(NotesField).CanModify and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if (ResourceField <> '') then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);

    ResToFields(D.FIelds, ResPos);
  end;

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  // ReadDBItem;

  FPlanner.Items.Select(FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;

{ TDBHalfDayPeriodSource }

procedure TDBHalfDayPeriodSource.PlannerChanged;
begin
  inherited;
  UpdatePlanner;
end;

procedure TDBHalfDayPeriodSource.UpdatePlanner;
begin
  if Assigned(FPlanner) then
  begin
    FPlanner.Display.DisplayStart := 0;
    FPlanner.Display.DisplayEnd := 2 * Round(Int(FEndDate) - Int(FStartDate));

    FPlanner.Mode.PlannerType := plHalfDayPeriod;

    FPlanner.Mode.BeginUpdate;
    FPlanner.Mode.PeriodEndDate := FEndDate;
    FPlanner.Mode.PeriodStartDate := FStartDate;
    FPlanner.Mode.EndUpdate;
    FPlanner.Items.BeginUpdate;
    ClearDBItems;
    ReadDBItems('');
    FPlanner.Items.EndUpdate;
  end;
end;

{ TDBDisjunctDaySource }

procedure TDBDisjunctDaySource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dts,dte,rdts,rdte: TDateTime;
  plIt: TPlannerItem;
  Accept: Boolean;
  DatePos, ResPos: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  ConfigurePlanner;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;


  FPlanner.Items.BeginUpdate;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);

    accept := true;
    FieldsToRes(D.Fields,ResPos,accept);

    rdts := dts;
    rdte := dte;

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := 0;
      FRecurrencyHandler.Generate;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
        {$IFDEF TMSCODESITE}
        SendMsg(FPlanner.Name+' READ ITEM '+D.FieldByName(KeyField).AsString);
        SendMsg(FormatDateTime('dd/mm/yyyy hh:nn',FDay)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',FDay+Span));
        {$ENDIF}

        Accept := False;

        DatePos := Dates.DatePos(dts);

        // date based item acception
        if DatePos >= 0 then
        begin
          Accept := True;
          if Assigned(FOnAccept) then
            FOnAccept(Self,D.Fields,Accept);
        end;

        if Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' ADD ITEM '+D.FieldByName(KeyField).AsString +
              ' ' +DateToStr(dts) + ' ' +DateToStr(dte));
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          with plIt do
          begin
            Recurrent := FRecurrencyHandler.IsRecurrent;

            FPlanner.RemoveClones(plIt);

            // set time here correct

            ItemRealStartTime := dts;
            ItemRealEndTime := dte;

            RecurrentOrigStart := dts;
            RecurrentOrigEnd := dte;

            RecurrentStart := rdts;
            RecurrentEnd := rdte;

            ItemStartTime := dts;
            ItemEndTime := dte;

            Recurrency := FRecurrencyHandler.Recurrency;

            InHeader := dte - dts > 1;
            RealTime := true;

            ItemPos := DateToPos(ResPos, dts);

            FieldsToItem(plIt);

            if FReadOnly then
            begin
              FixedPos := True;
              FixedSize := True;
              ReadOnly := True;
            end;
          end;

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);
        end;
      end;
    end;
    D.Next;
  end;

  D.GotoBookMark(B);

  D.EnableControls;

  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);

  if FPlanner.Header.AutoSize then
   FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;
   

  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}

  FPlanner.Items.ResolveLinks;
end;

procedure TDBDisjunctDaySource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTE, DTS: TDateTime;
  Day: TDateTime;
  deltastart, deltaend: TDateTime;
  ResPos: integer;

begin
  if IsUpdating then Exit;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
    D.Edit;
  except
    D.FreeBookMark(B);    
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  //Day := Int(Dates.PosDate (FPlanner.Items.DBItem.ItemPos));

  Day := PosToDay(FPlanner.Items.DBItem.ItemPos);

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := Day + Frac(FPlanner.Items.DBItem.ItemRealStartTime);
    DTE := Day + Frac(FPlanner.Items.DBItem.ItemRealEndTime);
  end
  else
  begin
    DTS := Day + Frac(FPlanner.Items.DBItem.ItemStartTime);
    DTE := Day + Frac(FPlanner.Items.DBItem.ItemEndTime);
  end;

  FPlanner.Items.DBItem.RealTime := False;

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  {$IFDEF TMSCODESITE}
  SendMsg('writedb:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
  {$ENDIF}

  TimeToFields(D.Fields,DTS,DTE);

  if (ResourceField <> '') then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);
    ResToFields(D.Fields, ResPos);
  end;


  if (SubjectField <> '') and
     (D.FieldByName(SubjectField).CanModify) then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     (D.FieldByName(NotesField).CanModify) and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;

end;

procedure TDBDisjunctDaySource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;
  Day: TDateTime;
  ResPos: integer;

begin
  inherited;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;    
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;

  //Day := Int(Dates.PosDate(FPlanner.Items.DBItem.ItemPos));
  Day := PosToDay(FPlanner.Items.DBItem.ItemPos);

  DTS := Day + Frac(FPlanner.Items.DBItem.ItemStartTime);
  DTE := Day + Frac(FPlanner.Items.DBItem.ItemEndTime);

  FPlanner.Items.DBItem.ItemRealStartTime := DTS;
  FPlanner.Items.DBItem.ItemRealEndTime := DTE;

  TimeToFields(D.Fields,DTS,DTE);

  if ResourceField <> '' then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);
    ResToFields(D.Fields, ResPos);
  end;

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;

procedure TDBDisjunctDaySource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;

  EndUpdate;
end;


procedure TDBDisjunctDaySource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dt, dtE: TDateTime;
  D: TDataSet;

begin
  inherited;
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}

    FPlanner.Items.DBItem := plIt;
    if plIt.Recurrent then
    begin
      TDBPlanner(FPlanner).ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      FPlanner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;
    FieldsToTime(D.Fields,dt,dtE);

    if Dates.HasDate(dt) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;
      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        EndUpdate;
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;

  end;

  EndUpdate;
end;

procedure TDBDisjunctDaySource.ReadDBItem;
var
  D: TDataSet;
  dts,dte: TDateTime;
  DatePos,ResPos: Integer;
  Accept: boolean;

begin
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  FPlanner.RemoveClones(FPlanner.Items.DBItem);

  ConfigurePlanner;

  with FPlanner.Items.DBItem do
  begin
    FieldsToTime(D.Fields,dts,dte);
    Accept := true;
    FieldsToRes(D.Fields,ResPos,Accept);

    {$IFDEF TMSCODESITE}
    SendMsg('read:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
    {$ENDIF}

    ItemStartTime := dts;
    ItemEndTime := dte;
    ItemRealStartTime := dts;
    ItemRealEndTime := dte;

    InHeader := dte - dts > 1;

    DatePos := Dates.DatePos(dts);

    // date based item acception
    if DatePos >= 0 then
    begin
      FPlanner.RemoveClones(FPlanner.Items.DBItem);

      ItemPos := DateToPos(ResPos, dts);
      //ItemPos := DatePos;

      if SubjectField <> '' then
        CaptionText := D.FieldByName(SubjectField).AsString;

      if NotesField <> '' then
        Text.Text := D.FieldByName(NotesField).AsString;

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;

      if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
        FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

      if Assigned(FOnFieldsToItem) then
        FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);
    end
    else
    begin
      FPlanner.Items.DBItem.Free;
      FPlanner.Items.DBItem := nil;
    end;
  end;

  if FPlanner.Header.AutoSize then
   FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;
   
end;

procedure TDBDisjunctDaySource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dt, dtE: TDateTime;
  plIt: TPlannerItem;
  i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;

  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dt,dtE);

    if Dates.HasDate(dt) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;

    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
      FPlanner.Items[i - 1].Free;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBDisjunctDaySource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

constructor TDBDisjunctDaySource.Create(AOwner: TComponent);
begin
  inherited;
  // Initialize on US date format
  FDateFormat := 'mm/dd/yyyy';
  FDates := TDateCollection.Create;
  FNumberOfResources := 1;
  FDisjunctDayMode := djMultiDayRes;
end;

procedure TDBDisjunctDaySource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
    FPlanner.Mode.PlannerType := plDay;
end;

procedure TDBDisjunctDaySource.ConfigurePlanner;
var
  i,j: Integer;
begin
  if not Assigned(FPlanner) then
    Exit;

  FPlanner.Positions := Dates.Count * FNumberOfResources;

  if FNumberOfResources > 1 then
    FPlanner.PositionGroup := FNumberOfResources
  else
    FPlanner.PositionGroup := 0;

  if FAutoHeaderUpdate then
  begin
    FPlanner.Header.Captions.Clear;
    FPlanner.Header.GroupCaptions.Clear;
    FPlanner.Header.Captions.Add('');

    if Mode = djMultiDayRes then
    begin
      for j := 1 to FNumberOfResources do
      begin
        for i := 1 to Dates.Count do
        begin
          FPlanner.Header.Captions.Add(FormatDateTime(DateFormat, Dates[i - 1].Date));
        end;
      end;
      for i := 1 to FNumberOfResources do
      begin
        if i <= FResourceMap.Count  then
          FPlanner.Header.GroupCaptions.Add(FResourceMap.Items[i - 1].DisplayName)
      end;
    end
    else
    begin
      for j := 1 to Dates.Count do
      begin
        for i := 1 to FNumberOfResources do
        begin
          if i <= FResourceMap.Count  then
            FPlanner.Header.Captions.Add(FResourceMap.Items[i - 1].DisplayName);
        end;
      end;

      for i := 1 to Dates.Count do
      begin
        FPlanner.Header.GroupCaptions.Add(FormatDateTime(DateFormat, Dates[i - 1].Date));
      end;
    end;
  end;

end;


function TDBDisjunctDaySource.DateToPos(Res: integer;dt: TDateTime): integer;
var
  dp: integer;
begin
  dp := Dates.DatePos(dt);

  if (FNumberOfResources > 1) then
  begin
    if Mode = djMultiDayRes then
    begin
      Result := (Dates.Count * Res) + dp;
    end
    else
    begin
      Result := (FNumberOfResources * dp) + Res;
    end;
  end
  else
    Result := dp;
end;


procedure TDBDisjunctDaySource.MapItemTimeOnPlanner(APlannerItem: TPlannerItem);
var
  dts,dte: TDateTime;

begin
  with APlannerItem do
  begin
    RealTime := True;
    dts := ItemRealStartTime;
    dte := ItemRealEndTime;
    InHeader := dte - dts > 1;
    ItemPos := DateToPos(ItemPos,dts);
  end;
end;


destructor TDBDisjunctDaySource.Destroy;
begin
  FDates.Free;
  inherited;
end;

procedure TDBDisjunctDaySource.SetDates(const Value: TDateCollection);
begin
  FDates.Assign(Value);
end;

procedure TDBDisjunctDaySource.SetMode(const Value: TDBDisjunctDayMode);
begin
  if (FDisjunctDayMode <> Value) then
  begin
    FDisjunctDayMode := Value;

    if FActive then
    begin
      ClearDBItems;
      ReadDBItems('');
    end;
  end;
end;

procedure TDBDisjunctDaySource.SetNumberOfResources(const Value: Integer);
begin
  if (FNumberOfResources <> Value) and (Value >= 1) then
  begin
    FNumberOfResources := Value;

    if FActive then
    begin
      ClearDBItems;
      ReadDBItems('');
    end;
  end;
end;

function TDBDisjunctDaySource.PosToDay(Pos: Integer): TDateTime;
begin
  if Mode = djMultiDayRes then
    Pos := Pos mod FNumberOfResources
  else
    Pos := Pos div FNumberOfResources;

  Result := Int(Dates.PosDate(Pos));
end;


function TDBDisjunctDaySource.PosToRes(Pos: integer): integer;
begin
  if Mode = djMultiDayRes then
    Pos := Pos div FNumberOfResources
  else
    Pos := Pos mod FNumberOfResources;

  Result := Pos;
end;

{ TDBActiveDaySource }

procedure TDBActiveDaySource.ReadDBItems(UpdateKey: string);
var
  D: TDataSet;
  B: TBookMark;
  dts,dte,rdts,rdte: TDateTime;
  plIt: TPlannerItem;
  Span, j: Integer;
  Accept: Boolean;
  ResourcePos,DatePos: Integer;
  Zone: TDateTime;
  dis, die: TDateTime;
  crossflg: Boolean;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' READ DB ITEMS : '+FPlanner.Name);
  {$ENDIF}

  if Assigned(OnBeforeItemsRead) then
    OnBeforeItemsRead(Self);

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;

  D.DisableControls;

  B := D.GetBookMark;

  D.First;

  if NotesField <> '' then
    FNotesDBField := D.FieldByName(NotesField)
  else
    FNotesDBField := nil;

  if SubjectField <> '' then
    FSubjectDBField := D.FieldByName(SubjectField)
  else
    FSubjectDBField := nil;

  if KeyField <> '' then
    FDBKeyDBField := D.FieldByName(KeyField)
  else
    FDBKeyDBField := nil;

  ConfigurePlanner(Span);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;

  Zone := EncodeTime(j div 60,j mod 60,0,0);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;
  dis := EncodeTime(j div 60, j mod 60,0,0);

  j := (FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayEnd) + 1;
  crossflg := j > 1440;
  j := j mod 1440;
  die := EncodeTime(j div 60, j mod 60,0,0);

  FPlanner.Items.BeginUpdate;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dts,dte);

    rdts := dts;
    rdte := dte;

//     outputdebugstring(pchar('->'+FormatDateTime('dd/mm/yyyy hh:nn',rdts)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',rdte)));
//     outputdebugstring(pchar('->'+FormatDateTime('dd/mm/yyyy hh:nn',dis)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',die)));

    if (UpdateKey = '') or (UpdateKey = D.FieldByName(KeyField).AsString) then
    begin
      if RecurrencyField <> '' then
        FRecurrencyHandler.Recurrency := D.FieldByName(RecurrencyField).AsString
      else
        FRecurrencyHandler.Recurrency := '';

      FRecurrencyHandler.StartTime := dts;
      FRecurrencyHandler.EndTime := dte;
      FRecurrencyHandler.TimeSpan := FDay + Span + Zone;
      FRecurrencyHandler.Generate;

      while FRecurrencyHandler.NextDate(dts,dte) do
      begin
        {$IFDEF TMSCODESITE}
        SendMsg(FPlanner.Name+' READ ITEM '+D.FieldByName(KeyField).AsString);
        SendMsg(FormatDateTime('dd/mm/yyyy hh:nn',FDay)+'->'+FormatDateTime('dd/mm/yyyy hh:nn',FDay+Span));
        {$ENDIF}

        Accept := True;

        // resource based item acception
        if (ResourceField <> '') and
           (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) and
           (dte >= FDay + Zone) and (dts < FDay + Span + Zone) then
        begin
          FieldsToRes(D.Fields, ResourcePos, Accept);
        end;

        if (Int(dte) = Int(dts)) and not crossflg and ((Frac(dte) < Frac(dis)) or (Frac(dts) > Frac(die))) then
        begin
          Accept := False;
        end;

        // date based item acception
        if (dte >= FDay + Zone) and (dts < FDay + Span + Zone) and Accept and Assigned(FOnAccept) then
        begin
          FOnAccept(Self,D.Fields,Accept);
        end;

        DatePos := IDayToPos(dts);

        if Accept then
          Accept := DatePos <> -1;

        if (dte >= FDay + Zone) and (dts < FDay + Span + Zone) and Accept then
        begin
          plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);

          if FRecurrencyHandler.IsRecurrent then
            plIt := nil;

          if not Assigned(plIt) then
          begin
            {$IFDEF TMSCODESITE}
            SendMsg(FPlanner.Name+' ADD ITEM '+D.FieldByName(KeyField).AsString +
              ' ' +DateToStr(dts) + ' ' +DateToStr(dte));
            {$ENDIF}
            plIt := FPlanner.Items.Add;
          end;

          with plIt do
          begin
            Recurrent := FRecurrencyHandler.IsRecurrent;

            FPlanner.RemoveClones(plIt);

            // set time here correct

            ItemRealStartTime := dts;
            ItemRealEndTime := dte;

            RecurrentOrigStart := dts;
            RecurrentOrigEnd := dte;

            RecurrentStart := rdts;
            RecurrentEnd := rdte;

            ItemStartTime := dts;
            ItemEndTime := dte;

            Recurrency := FRecurrencyHandler.Recurrency;

            InHeader := dte - dts > 1;
            RealTime := true;

            // map to start of day
            if (Int(dts) < Int(FDay)) and not InHeader then
              ChangeCrossing;

            ItemPos := CalcItemPos(DatePos,ResourcePos);

            FieldsToItem(plIt);

            if FReadOnly then
            begin
              FixedPos := True;
              FixedSize := True;
              ReadOnly := True;
            end;
          end;

          if (plIt.HTMLTemplate.Text <> '') then
            plIt.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(plIt, D);

          if Assigned(FOnFieldsToItem) then
            FOnFieldsToItem(Self, D.Fields, plIt);

          if plIt.InHeader then
          begin
            if dts < Day then
              dts := Day;

            for j := 1  to Round(Int(dte) - Int(dts)) do
            begin
              if (j + plIt.ItemPos < FPlanner.Positions) and (dts + j < Day + Span) then
                with FPlanner.CloneItem(plIt) do
                begin
                  ItemRealStartTime := dts;
                  ItemRealEndTime := dte;
                  RealTime := true;
                  DatePos := DatePos + 1;
                  ItemPos := CalcItemPos(DatePos,ResourcePos);
                end;
            end;
          end;
        end;
      end;
    end;
    D.Next;
  end;

  D.GotoBookMark(B);

  D.EnableControls;

  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;

  FPlanner.Items.EndUpdate;

  if Assigned(OnItemsRead) then
    OnItemsRead(Self);

  if FPlanner.Header.AutoSize then
   FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;
   

  {$IFDEF TMSCODESITE}
  SendMsg('DONE READ DB ITEMS');
  {$ENDIF}

  FPlanner.Items.ResolveLinks;
end;

procedure TDBActiveDaySource.SetDay(const Value: TDateTime);
begin
  if FDay <> Value then
  begin
    FDay := Int(Value);
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBActiveDaySource.Next;
var
  i: Integer;
begin
  if not Assigned(FPlanner) then
    Exit;

  i := 1;

  while (DayOfWeek(FDay + i) in FPlanner.InActive) do
  begin
    inc(i);
  end;

  FDay := FDay + i;
  inherited;
end;

procedure TDBActiveDaySource.Prev;
var
  i: Integer;
begin
  if not Assigned(FPlanner) then
    Exit;

  i := -1;

  while (DayOfWeek(FDay + i) in FPlanner.InActive) do
  begin
    dec(i);
  end;

  FDay := FDay + i;

  inherited;
end;

procedure TDBActiveDaySource.WriteDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTE, DTS: TDateTime;
  ResPos: Integer;
  deltastart, deltaend: TDateTime;


begin
  if IsUpdating then Exit;

  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('write selected here?');
  {$ENDIF}

  if Assigned(FOnUpdateItem) then
  begin
    BeginUpdate;
    FOnUpdateItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    if not FUpdateByQuery then
      Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

    D.Edit;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not put dataset in edit mode');
    Exit;
  end;

  if FPlanner.Items.DBItem.RealTime then
  begin
    DTS := FPlanner.Items.DBItem.ItemRealStartTime;
    DTE := FPlanner.Items.DBItem.ItemRealEndTime;
  end
  else
  begin
    DTS := FPlanner.Items.DBItem.ItemStartTime;
    DTE := FPlanner.Items.DBItem.ItemEndTime;
  end;

  {$IFDEF TMSCODESITE}
  SendMsg('writedb:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
  {$ENDIF}

  FPlanner.Items.DBItem.RealTime := False;

  //  if DTE < DTS then
  //    DTE := DTE + 1;

  if FPlanner.Items.DBItem.Recurrent then
  begin
    deltastart := DTS - FPlanner.Items.DBItem.RecurrentOrigStart;
    deltaend := DTE - FPlanner.Items.DBItem.RecurrentOrigEnd;

    DTS := FPlanner.Items.DBItem.RecurrentStart + deltastart;
    DTE := FPlanner.Items.DBItem.RecurrentEnd + deltaend;
  end;

  TimeToFields(D.Fields,DTS,DTE);

  if (ResourceField <> '') and (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);

    ResToFields(D.Fields, ResPos);
  end;

  if (SubjectField <> '') and
     (D.FieldByName(SubjectField).CanModify) then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and
     (D.FieldByName(NotesField).CanModify) and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if (RecurrencyField <> '') and
    (D.FieldByName(RecurrencyField).CanModify) then
    D.FieldByName(RecurrencyField).AsString := FPlanner.Items.DBItem.Recurrency;

  if (MinTimeField <> '') and (MaxTimeField <> '') then
  begin
    FRecurrencyHandler.Recurrency := FPlanner.Items.DBItem.Recurrency;

    FRecurrencyHandler.StartTime := dts;
    FRecurrencyHandler.EndTime := dte;
    FRecurrencyHandler.TimeSpan := 0;
    FRecurrencyHandler.Generate;

    D.FieldByName(MinTimeField).AsDateTime := FRecurrencyHandler.RecurrentMinDate;
    D.FieldByName(MaxTimeField).AsDateTime := FRecurrencyHandler.RecurrentMaxDate;
  end;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if (B <> nil) and D.BookmarkValid(B) then
      D.GotoBookMark(B);
  finally
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;


procedure TDBActiveDaySource.SetMode(const Value: TDBDayMode);
begin
  if FMode <> Value then
  begin
    FMode := Value;
    case FMode of
    dmMultiDay:
      if Assigned(FPlanner) then
      begin
        FPlanner.PositionGroup := 0;
        FPlanner.Positions := NumberOfDays;
      end;
    dmMultiResource:
      if Assigned(FPlanner) then
      begin
        FPlanner.PositionGroup := 0;
        FPlanner.Positions := NumberOfResources;
      end;
    end;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBActiveDaySource.AddDBItem;
var
  D: TDataSet;
  B: TBookMark;
  DTS, DTE: TDateTime;
  ResPos: Integer;

begin
  inherited;

  if not CheckDataSet then Exit;


  {$IFDEF TMSCODESITE}
  SendMsg('add selected here?');
  {$ENDIF}

  if Assigned(FOnInsertItem) then
  begin
    BeginUpdate;
    FPlanner.Items.DBItem.DBKey := CreateKey;    
    FOnInsertItem(Self,FPlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;

  D := DataSource.DataSet;

  B := D.GetBookMark;

  try
    D.Append;
  except
    D.FreeBookMark(B);
    EndUpdate;
    raise Exception.Create('Could not append a new record');
    Exit;
  end;

  if not AutoIncKey then
  begin
    D.FieldByName(KeyField).AsString := CreateKey;
    FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  end;

  DTS := FPlanner.Items.DBItem.ItemStartTime;
  DTE := FPlanner.Items.DBItem.ItemEndTime;

  FPlanner.Items.DBItem.ItemRealStartTime := DTS;
  FPlanner.Items.DBItem.ItemRealEndTime := DTE;

  TimeToFields(D.Fields,DTS,DTE);

  if (ResourceField <> '') and (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) then
  begin
    ResPos := PosToRes(FPlanner.Items.DBItem.ItemPos);
    ResToFields(D.Fields, ResPos);
  end;

  if SubjectField <> '' then
    D.FieldByName(SubjectField).AsString := FPlanner.Items.DBItem.CaptionText;

  if (NotesField <> '') and (FPlanner.Items.DBItem.HTMLTemplate.Text = '') then
    D.FieldByName(NotesField).AsString := FPlanner.Items.DBItem.ItemText;

  if Assigned(FOnItemToFields) then
    FOnItemToFields(Self, D.Fields, FPlanner.Items.DBItem);

  try
    D.Post;
    if AutoIncKey then
      FPlanner.Items.DBItem.DBKey := D.FieldByName(KeyField).AsString;
  finally
    D.GotoBookMark(B);
    D.FreeBookMark(B);
    EndUpdate;
  end;
end;

procedure TDBActiveDaySource.DeleteDBItem(APlanner: TPlanner);
var
  D: TDataSet;
begin
  inherited;
  if not CheckDataSet then Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('delete selected here?');
  {$ENDIF}

  if Assigned(FOnDeleteItem) then
  begin
    BeginUpdate;
    FOnDeleteItem(Self,APlanner.Items.DBItem);
    EndUpdate;
    Exit;
  end;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  try
     D.Delete;
  except
    EndUpdate;
    raise Exception.Create('Failed to delete record in dataset');
    Exit;
  end;
  
  EndUpdate;
end;


procedure TDBActiveDaySource.ItemChanged(DBKey: string);
var
  plIt: TPlannerItem;
  dt, dtE: TDateTime;
  Span: Integer;
  D: TDataSet;

begin
  inherited;
  if IsUpdating then Exit;
  if (DBKEY='') then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  {$IFDEF TMSCODESITE}
  SendMsg(FPlanner.Name+' Itemchanged: '+DBKEY);
  {$ENDIF}

  plIt := FPlanner.Items.FindKey(DBKey);

  if Assigned(plIt) then
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Found it : '+ plIt.Name);
    {$ENDIF}

    FPlanner.Items.DBItem := plIt;

    if plIt.Recurrent then
    begin
      TDBPlanner(FPlanner).ClearDBKey(DBKey);
      EndUpdate;
      ReadDBItems(DBKey);
      FPlanner.Items.DBItem := nil;
    end
    else
      ReadDBItem;

    if Assigned(FPlanner.Items.DBItem) then
      FPlanner.Items.Select(FPlanner.Items.DBItem);
  end
  else
  begin
    {$IFDEF TMSCODESITE}
    SendMsg('Add it to '+inttostr(FPlanner.Items.Count)+' items');
    {$ENDIF}

    if not CheckDataSet then Exit;

    D := DataSource.DataSet;
    FieldsToTime(D.Fields,dt,dtE);

    if Mode = dmMultiResource then
      Span := 1
    else
      Span := DaysToSpan(FPlanner.Positions);


    if (dt > FDay) and (dt < FDay + Span) then
    begin
      plIt := FPlanner.Items.Add;
      plIt.DBKey := DBKey;
      FPlanner.Items.DBItem := plIt;
      if plIt.Recurrent then
      begin
        TDBPlanner(FPlanner).ClearDBKey(DBKey);
        EndUpdate;
        ReadDBItems(DBKey);
      end
      else
        ReadDBItem;
    end;

  end;

  EndUpdate;
end;

procedure TDBActiveDaySource.ReadDBItem;
var
  D: TDataSet;
  dts,dte: TDateTime;
  ResourcePos,DatePos,j: Integer;
  Accept: Boolean;
  Zone: TDateTime;
  Span: Integer;

begin
  if not CheckDataSet then
    Exit;

  {$IFDEF TMSCODESITE}
  SendMsg('read selected');
  {$ENDIF}

  D := DataSource.DataSet;

  FPlanner.RemoveClones(FPlanner.Items.DBItem);

  j := FPlanner.Display.DisplayUnit * FPlanner.Display.DisplayStart;
  j := j mod 1440;

  Zone := EncodeTime(j div 60,j mod 60,0,0);

  case Mode of
  dmMultiDay,dmMultiDayRes,dmMultiResDay: Span := NumberOfDays;
  dmMultiResource: Span := 1;
  end;

  ConfigurePlanner(Span);

  with FPlanner.Items.DBItem do
  begin
    FieldsToTime(D.Fields,dts,dte);

    {$IFDEF TMSCODESITE}
    SendMsg('read:'+formatdatetime('dd/mm/yyyy hh:nn',dts)+'-'+formatdatetime('dd/mm/yyyy hh:nn',dte));
    {$ENDIF}

    ItemStartTime := dts;
    ItemEndTime := dte;
    ItemRealStartTime := dts;
    ItemRealEndTime := dte;

    InHeader := dte - dts > 1;

    // resource based item acception
    if (ResourceField <> '') and
       (Mode in [dmMultiResource,dmMultiDayRes,dmMultiResDay]) and
       (dte >= FDay + Zone) and (dts < FDay + Span + Zone) then
    begin
      FieldsToRes(D.Fields,ResourcePos, Accept);
    end;

    // date based item acception
    if (dte >= FDay + Zone) and (dts < FDay + Span + Zone) then
    begin
      FPlanner.RemoveClones(FPlanner.Items.DBItem);

      DatePos := IDayToPos(dts);

      ItemPos := CalcItemPos(DatePos,ResourcePos);

      if SubjectField <> '' then
        CaptionText := D.FieldByName(SubjectField).AsString;

      if NotesField <> '' then
        Text.Text := D.FieldByName(NotesField).AsString;

      if FReadOnly then
      begin
        FixedPos := True;
        FixedSize := True;
        ReadOnly := True;
      end;

      if InHeader then
      begin
        if dts < Day then
          dts := Day;

        for j := 1  to Round(Int(dte) - Int(dts)) do
        begin
          if (j + ItemPos < FPlanner.Positions) and (dts + j < Day + Span) then
            with FPlanner.CloneItem(FPlanner.Items.DBItem) do
            begin
              DatePos := DatePos + 1;
              ItemPos := CalcItemPos(DatePos,ResourcePos);
            end;
        end;
      end;

      if (FPlanner.Items.DBItem.HTMLTemplate.Text <> '') then
        FPlanner.Items.DBItem.Text.Text := (FPlanner as TDBPlanner).HTMLDBReplace(FPlanner.Items.DBItem, D);

      if Assigned(FOnFieldsToItem) then
        FOnFieldsToItem(Self, D.Fields, FPlanner.Items.DBItem);
    end
    else
    begin
      FPlanner.Items.DBItem.Free;
      FPlanner.Items.DBItem := nil;
    end;
  end;

  if FPlanner.Header.AutoSize then
   FPlanner.AutoSizeHeader
  else
    FPlanner.HeaderControl.Invalidate;
   
end;

procedure TDBActiveDaySource.SynchDBItems;
var
  D: TDataSet;
  B: TBookMark;
  dt, dtE: TDateTime;
  plIt: TPlannerItem;
  Span,i: Integer;

begin
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;

  if Assigned(FOnSetFilter) then
    FOnSetFilter(Self);

  D := DataSource.DataSet;
  B := D.GetBookMark;
  D.First;

  if Mode = dmMultiResource then
    Span := 1
  else
  begin
    Span := FPlanner.Positions;
  end;

  for i := 1 to FPlanner.Items.Count do
    FPlanner.Items[i - 1].Synched := False;

  while not D.Eof do
  begin
    FieldsToTime(D.Fields,dt,dtE);

    if (dt > FDay) and (dt < FDay + Span) then
    begin
      plIt := FPlanner.Items.FindKey(D.FieldByName(KeyField).AsString);
      if Assigned(plIt) then
        plIt.Synched := True;
    end;

    D.Next;
  end;

  for i := FPlanner.Items.Count downto 1 do
  begin
    if not FPlanner.Items[i - 1].Synched then
      FPlanner.Items[i - 1].Free;
  end;

  D.GotoBookMark(B);
  D.FreeBookMark(B);

  if Assigned(FOnResetFilter) then
    FOnResetFilter(Self);

  EndUpdate;
end;

procedure TDBActiveDaySource.GotoDBItem;
var
  D: TDataSet;
begin
  inherited;
  if IsUpdating then Exit;
  if not CheckDataSet then Exit;
  if not Assigned(FPlanner) then Exit;

  BeginUpdate;
  D := DataSource.DataSet;

  D.Locate(KeyField,FPlanner.Items.DBItem.DBKey,[]);

  EndUpdate;
end;

constructor TDBActiveDaySource.Create(AOwner: TComponent);
begin
  inherited;
  // Initialize on US date format
  FDateFormat := 'mm/dd/yyyy';
  FNumberOfDays := 7;
  FNumberOfResources := 1;
end;

procedure TDBActiveDaySource.PlannerChanged;
begin
  inherited;
  if Assigned(FPlanner) then
    FPlanner.Mode.PlannerType := plDay;
end;

function TDBActiveDaySource.PosToDay(Pos: Integer): TDateTime;
begin
  case Mode of
  dmMultiDay:
    Result := IPosToDay(Pos);
  dmMultiResource:
    Result := Day;
  dmMultiDayRes:
    Result := IPosToDay(Pos mod NumberOfDays);
  dmMultiResDay:
    Result := IPosToDay(Pos div NumberOfResources);
  else
    Result := FDay;
  end;
end;

function TDBActiveDaySource.PosToRes(Pos: Integer): Integer;
begin
  case Mode of
  dmMultiDay:
    Result := 0;
  dmMultiResource:
    Result := Pos;
  dmMultiDayRes:
    Result := Pos div NumberOfDays;
  dmMultiResDay:
    Result := Pos mod NumberOfResources;
  else
    Result := 0;
  end;
end;


procedure TDBActiveDaySource.SetNumberOfDays(const Value: Integer);
begin
  if Value <= 0 then
    Exit;

  if FNumberOfDays <> Value then
  begin
    FNumberOfDays := Value;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBActiveDaySource.SetNumberOfResources(const Value: Integer);
begin
  if Value <= 0 then
    Exit;
  if FNumberOfResources <> Value then
  begin
    FNumberOfResources := Value;
    ClearDBItems;
    ReadDBItems('');
  end;
end;

procedure TDBActiveDaySource.ConfigurePlanner(var Span: Integer);
var
  i,j,k,dx: Integer;
  ResName: string;
begin
  if not Assigned(FPlanner) then
    Exit;

  case Mode of
  dmMultiResource:
    begin
      FPlanner.Positions := NumberOfResources;
      Span := 1;
      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        FPlanner.Header.Captions.Add('');

        for i := 1 to NumberOfResources do
        begin
          ResName := ResourceMap.PositionName(i - 1);
          if Assigned(FOnGetResourceName) then
            FOnGetResourceName(Self,i,ResName);
          if ResName <> '' then
          begin
            if FPlanner.Header.Captions.Count <= i then
              FPlanner.Header.Captions.Add(ResName)
            else
              FPlanner.Header.Captions.Strings[i] := ResName;
          end;
        end;
      end;
      FPlanner.Display.CurrentPosFrom := -1;
      FPlanner.Display.CurrentPosTo := -1;
    end;
  dmMultiDay:
    begin
      FPlanner.Positions := NumberOfDays; // ie. nr. of active days

      Span := DaysToSpan(NumberOfDays);

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        FPlanner.Header.Captions.Add('');

        j := 1;
        for i := 1 to NumberOfDays do
        begin
          if FPlanner.Header.Captions.Count <= i then
            FPlanner.Header.Captions.Add(FormatDateTime(FDateFormat,DayInSpan(i - 1)))
          else
            FPlanner.Header.Captions.Strings[j] := FormatDateTime(FDateFormat,DayInSpan(i - 1));
        end;
      end;

      dx := Round(Int(Now) - Int(Day));
      FPlanner.Display.CurrentPosFrom := dx;
      FPlanner.Display.CurrentPosTo := dx;
    end;
  dmMultiDayRes:
    begin
      FPlanner.Positions := NumberOfResources * NumberOfDays;
      FPlanner.PositionGroup := NumberOfDays;
      Span := DaysToSpan(NumberOfDays);

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        FPlanner.Header.Captions.Add('');
        FPlanner.Header.GroupCaptions.Clear;
        FPlanner.Header.GroupCaptions.Add('');

        for i := 1 to NumberOfResources do
        begin
          for j := 1 to NumberOfDays do
          begin
             k := j + (i - 1) * NumberOfDays;

             if FPlanner.Header.Captions.Count <= k then
               FPlanner.Header.Captions.Add(FormatDateTime(FDateFormat,DayInSpan(j - 1)))
             else
               FPlanner.Header.Captions.Strings[k] := FormatDateTime(FDateFormat,DayInSpan(j - 1));
          end;

          ResName := ResourceMap.PositionName(i - 1);
          if Assigned(FOnGetResourceName) then
            FOnGetResourceName(Self,i,ResName);

          if FPlanner.Header.GroupCaptions.Count < i then
            FPlanner.Header.GroupCaptions.Add(ResName)
          else
            FPlanner.Header.GroupCaptions[i - 1] := ResName;
        end;
      end;
      FPlanner.Display.CurrentPosFrom := -1;
      FPlanner.Display.CurrentPosTo := -1;
    end;
  dmMultiResDay:
    begin
      FPlanner.PositionGroup := NumberOfResources;
      FPlanner.Positions := NumberOfResources * NumberOfDays;
      Span := DaysToSpan(NumberOfDays);

      if FAutoHeaderUpdate then
      begin
        FPlanner.Header.Captions.Clear;
        FPlanner.Header.Captions.Add('');
        FPlanner.Header.GroupCaptions.Clear;
        FPlanner.Header.GroupCaptions.Add('');

        for i := 1 to NumberOfDays do
        begin
          if FPlanner.Header.GroupCaptions.Count < i then
            FPlanner.Header.GroupCaptions.Add(FormatDateTime(FDateFormat,DayInSpan(i - 1)))
          else
            FPlanner.Header.GroupCaptions[i - 1] := FormatDateTime(FDateFormat,DayInSpan(i - 1));
        end;

        for i := 1 to NumberOfDays do
          for j := 1 to NumberOfResources do
          begin
            ResName := ResourceMap.PositionName(j - 1);
            if Assigned(FOnGetResourceName) then
              FOnGetResourceName(Self,j,ResName);
            k := j + (i - 1) * NumberOfResources;
            if FPlanner.Header.Captions.Count <= k then
              FPlanner.Header.Captions.Add(ResName)
            else
              FPlanner.Header.Captions.Strings[k] := ResName;
          end;
      end;
      dx := Round(Int(Now) - Int(Day));
      FPlanner.Display.CurrentPosFrom := dx * NumberOfResources;
      FPlanner.Display.CurrentPosTo := (dx + 1) * NumberOfResources - 1;
    end;
  end;

end;

function TDBActiveDaySource.CalcItemPos(DatePos, ResourcePos: Integer): Integer;
begin
  case Mode of
  dmMultiDay:
    Result := DatePos;
  dmMultiResource:
    Result := ResourcePos;
  dmMultiResDay:
    Result := ResourcePos + NumberOfResources * DatePos;
  dmMultiDayRes:
    Result := ResourcePos * NumberOfDays + DatePos;
  else
    Result := DatePos;
  end;
end;


procedure TDBActiveDaySource.MapItemTimeOnPlanner(APlannerItem: TPlannerItem);
var
  dts,dte: TDateTime;
  j, DatePos: Integer;
begin
  with APlannerItem do
  begin
    RealTime := True;
    dts := ItemRealStartTime;
    dte := ItemRealEndTime;

    // map to start of day
    if Int(dts) < Int(FDay) then
      ChangeCrossing;

    InHeader := dte - dts > 1;

    DatePos := Round(Int(dts - FDay));

    ItemPos := CalcItemPos(DatePos,ItemPos);

    if APlannerItem.InHeader then
    begin
      for j := 1  to round(Int(dte) - Int(dts)) do
        begin
          if j + APlannerItem.ItemPos < FPlanner.Positions then
            with FPlanner.CloneItem(APlannerItem) do
              ItemPos := ItemPos + j;
        end;
    end;
  end;
end;


function TDBActiveDaySource.DaysToSpan(Days: Integer): Integer;
var
  i,j: Integer;

begin
  Result := Days;

  if not Assigned(FPlanner) then
    Exit;

  j := 0;

  for i := 1 to Days do
  begin
    while (DayOfWeek(Day + i - 1 + j) in FPlanner.InActive) do
    begin
      inc(Result);
      inc(j);
    end;
  end;
end;

function TDBActiveDaySource.DayInSpan(ADay: Integer): TDateTime;
var
  i,j: Integer;
  flg: Boolean;
begin
  flg := False;

  for i := 0 to 6 do
  begin
    if not (i in FPlanner.InActive) then
      Flg := True;
  end;

  if not Flg then
    raise Exception.Create('No active days selected in planner');

  Result := Day;
  j := 0;

  for i := 0 to ADay do
  begin
    while (DayOfWeek(Day + i + j) in FPlanner.InActive) do
    begin
      Result := Result + 1;
      inc(j);
    end;
  end;

  Result := Result + ADay;
end;

function TDBActiveDaySource.IDayToPos(ADate: TDateTime): Integer;
var
  i: Integer;
begin
  Result := -1;

  for i := 1 to NumberOfDays do
  begin
    if int(IPosToDay(i - 1)) = int(ADate) then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

function TDBActiveDaySource.IPosToDay(ADay: Integer): TDateTime;
begin
  Result := DayInSpan(ADay);
end;

{ TResourceMap }

function TResourceMap.Add: TResourceMapItem;
begin
  Result := TResourceMapItem(inherited Add);
end;

constructor TResourceMap.Create(AOwner: TComponent);
begin
  inherited Create(TResourceMapItem);
  FOwner := AOwner;
end;

function TResourceMap.GetItem(Index: Integer): TResourceMapItem;
begin
  Result := TResourceMapItem(inherited Items[Index]);
end;

function TResourceMap.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TResourceMap.Insert(index: Integer): TResourceMapItem;
begin
  Result := TResourceMapItem(inherited Insert(Index));
end;

function TResourceMap.PositionName(Value: Integer): string;
var
  i: integer;
  auto: boolean;
begin
  Result := '';

  auto := true;
  for i := 1 to Count do
  begin
    if Items[i - 1].PositionIndex <> -1 then
      auto := false;
  end;

  if auto and (value < Count) and (Value >= 0) then
  begin
    Result := Items[Value].DisplayName;
    Exit;
  end;

  for i := 1 to Count do
  begin
    if Items[i - 1].PositionIndex = Value then
    begin
      Result := Items[i - 1].DisplayName;
      Break;
    end;
  end;

end;

function TResourceMap.PositionToRes(Value: Integer): Integer;
var
  i: integer;
  auto: boolean;
begin
  Result := -1;

  auto := true;
  for i := 1 to Count do
  begin
    if Items[i - 1].PositionIndex <> -1 then
      auto := false;
  end;

  if auto and (value < Count) and (Value >= 0) then
  begin
    Result := Items[Value].ResourceIndex;
    Exit;
  end;

  for i := 1 to Count do
  begin
    if Items[i - 1].PositionIndex = Value then
    begin
      Result := Items[i - 1].ResourceIndex;
      Break;
    end;
  end;
end;

function TResourceMap.ResToPosition(Value: Integer): Integer;
var
  i: integer;
  auto: boolean;
begin
  Result := -1;

  auto := true;
  for i := 1 to Count do
  begin
    if Items[i - 1].PositionIndex <> -1 then
      auto := false;
  end;

  for i := 1 to Count do
  begin
    if Items[i - 1].ResourceIndex = Value then
    begin
      if auto then
        Result := i - 1
      else
        Result := Items[i - 1].PositionIndex;

      Break;
    end;
  end;
end;

procedure TResourceMap.SetItem(Index: Integer;
  const Value: TResourceMapItem);
begin
  inherited Items[Index] := Value;
end;

{ TResourceMapItem }

procedure TResourceMapItem.Assign(Source: TPersistent);
begin
  if (Source is TResourceMapItem) then
  begin
    FResourceIndex := (Source as TResourceMapItem).ResourceIndex;
    FPositionIndex := (Source as TResourceMapItem).PositionIndex;
    FDisplayName := (Source as TResourceMapItem).DisplayName;
    FTag := (Source as TResourceMapItem).Tag;
  end;
end;

constructor TResourceMapItem.Create(Collection: TCollection);
begin
  inherited;
  FResourceIndex := Collection.Count - 1;
  FPositionIndex := - 1;
end;


{ TDateCollection }

function TDateCollection.Add: TDateItem;
begin
  Result := TDateItem(inherited Add);
end;

constructor TDateCollection.Create;
begin
  inherited Create(TDateItem);
end;

function TDateCollection.DatePos(ADate: TDateTime): Integer;
var
  i: Integer;
begin
  Result := -1;
  for i := 1 to Count do
  begin
    if int(ADate) = int(Items[i - 1].Date) then
    begin
      Result := i - 1;
      Break;
    end;
  end;
end;

function TDateCollection.GetItem(Index: Integer): TDateItem;
begin
  Result := TDateItem(inherited Items[Index]);
end;

function TDateCollection.HasDate(ADate: TDateTime): Boolean;
var
  i: Integer;
begin
  Result := False;
  for i := 1 to Count do
  begin
    if int(ADate) = int(Items[i - 1].Date) then
    begin
      Result := True;
      Break;
    end;
  end;
end;

function TDateCollection.Insert(index: Integer): TDateItem;
begin
  Result := TDateItem(inherited Insert(Index));
end;

function TDateCollection.PosDate(Pos: Integer): TDateTime;
begin
  if Pos < Count then
    Result := Items[Pos].Date
  else
    Result := 0;
end;

procedure TDateCollection.SetItem(Index: Integer; const Value: TDateItem);
begin
  inherited Items[Index] := Value;
end;

{ TDBPlannerResourceDataLink }

procedure TDBPlannerResourceDataLink.ActiveChanged;
begin
  inherited;
  if FUpdateCount = 0 then
    UpdateResourceMap;
end;

constructor TDBPlannerResourceDataLink.Create(ADBItemSource: TDBItemSource);
begin
  inherited Create;
  FDBItemSource := ADBItemSource;
end;

procedure TDBPlannerResourceDataLink.DataSetChanged;
begin
  inherited;
  // update here
  if FUpdateCount = 0 then
    UpdateResourceMap;
end;

destructor TDBPlannerResourceDataLink.Destroy;
begin
  inherited;
end;

procedure TDBPlannerResourceDataLink.RecordChanged(Field: TField);
begin
  inherited;
end;

procedure TDBPlannerResourceDataLink.UpdateResourceMap;
var
  D: TDataSet;
  B: TBookmark;
  id: integer;
  value: string;
  rm: TResourceMapItem;
begin
  // load resource map here

  if not Assigned(FDBItemSource.ResourceDataSource.DataSource) then
    Exit;

  if not Assigned(FDBItemSource.ResourceDataSource.DataSource.DataSet) then
    Exit;

  if not FDBItemSource.ResourceDataSource.DataSource.DataSet.Active then
    Exit;

  if (FDBItemSource.ResourceDataSource.DataSource.DataSet.State in [dsInsert,dsEdit]) then
    Exit;


  inc(FUpdateCount);

  D := FDBItemSource.ResourceDataSource.DataSource.DataSet;
  D.DisableControls;
  B := D.GetBookMark;

  try
    D.First;

    FDBItemSource.ResourceMap.Clear;

    while not D.Eof do
    begin
      id := -1;
      value := '';

      if (FDBItemSource.ResourceDataSource.ResourceIDField <> '') then
        id := D.FieldByName(FDBItemSource.ResourceDataSource.ResourceIDField).AsInteger;

      if (FDBItemSource.ResourceDataSource.ResourceNameField <> '') then
        value := D.FieldByName(FDBItemSource.ResourceDataSource.ResourceNameField).DisplayText;

      if (id <> -1) then
      begin
        rm := FDBItemSource.ResourceMap.Add;
        rm.ResourceIndex := id;
        rm.PositionIndex := -1;

        rm.DisplayName := value;
      end;
      D.Next;
    end;

  finally
    D.EnableControls;
    D.GotoBookmark(B);
    dec(FUpdateCount);
    D.FreeBookMark(B);
  end;

  FDBItemSource.ResourceUpdate;
end;

{ TResourceDataSource }

procedure TResourceDataSource.Assign(Source: TPersistent);
begin
  if (Source is TResourceDataSource) then
  begin
    FResourceIDField := (Source as TResourceDataSource).ResourceIDField;
    FResourceNameField := (Source as TResourceDataSource).ResourceNameField;
  end;
end;

constructor TResourceDataSource.Create(AOwner: TDBItemSource);
begin
  inherited Create;
  FDBItemSource := AOwner;
end;

function TResourceDataSource.GetDataSource: TDataSource;
begin
  Result := FDBItemSource.FResourceDataLink.DataSource;
end;

procedure TResourceDataSource.SetDataSource(const Value: TDataSource);
begin
  FDBItemSource.FResourceDataLink.DataSource := Value;
end;

{ TDBActiveDayPeriodSource }

procedure TDBActiveDayPeriodSource.PlannerChanged;
begin
  inherited;
  UpdatePlanner;
end;

procedure TDBActiveDayPeriodSource.UpdatePlanner;
begin
  inherited;
  if Assigned(FPlanner) then
  begin
    FPlanner.Display.DisplayStart := 0;
    FPlanner.Display.DisplayEnd := Round(Int(FEndDate) - Int(FStartDate));

    FPlanner.Mode.PlannerType := plActiveDayPeriod;

    FPlanner.Mode.BeginUpdate;
    FPlanner.Mode.PeriodEndDate := FEndDate;
    FPlanner.Mode.PeriodStartDate := FStartDate;
    FPlanner.Mode.EndUpdate;

    FPlanner.Items.BeginUpdate;
    ClearDBItems;
    ReadDBItems('');
    FPlanner.Items.EndUpdate;
  end;
end;

initialization

end.

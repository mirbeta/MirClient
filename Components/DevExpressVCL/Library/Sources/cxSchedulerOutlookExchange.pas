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

unit cxSchedulerOutlookExchange;

{$I cxVer.inc}

interface

uses
  Variants, Classes, dxCore, cxSchedulerStorage, ComObj, cxClasses, cxDateUtils,
  cxSchedulerRecurrence;

type
  TcxSchedulerExchangeProgressProc = procedure(ACurrent, ACount: Integer; var Abort: Boolean);
  TcxAcceptEventExportFunc = function(AEvent: TcxSchedulerEvent): Boolean;
  TcxAcceptAppointmentItemImportFunc = function(const AppointmentItem: OleVariant): Boolean;
  TcxAppointmentItemToEventProc = procedure(const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent);
  TcxEventToAppointmentItemProc = procedure(AEvent: TcxSchedulerEvent; const AppointmentItem: OleVariant);

  { TcxAppointmentInfo }

  TcxAppointmentInfo = class
  private
    FAppointmentItem: OleVariant;
    FEntryID: string;
  public
    constructor Create(const AAppointmentItem: OleVariant);

    property AppointmentItem: OleVariant read FAppointmentItem;
    property EntryID: string read FEntryID;
  end;

  { TcxAppointmentEntryIDList }

  TcxAppointmentEntryIDList = class(TcxObjectList)
  private
    function GetItems(Index: Integer): TcxAppointmentInfo;
  public
    procedure Add(const AppointmentItem: OleVariant);
    function ExtractAppointmentItem(AIndex: Integer): OleVariant;
    function FindEntryID(const AEntryID: string; var Index: Integer): Boolean;
    function IndexOfEntryID(const AEntryID: string): Integer;
    procedure Populate(ACalendarFolder: OleVariant);

    property Items[Index: Integer]: TcxAppointmentInfo read GetItems; default;
  end;

  { TcxEventInfo }

  TcxEventInfo = class
  private
    FEvent: TcxSchedulerEvent;
    FEntryID: string;
  public
    constructor Create(AEvent: TcxSchedulerEvent; AEntryIDField: TcxCustomSchedulerStorageField);

    property Event: TcxSchedulerEvent read FEvent;
    property EntryID: string read FEntryID;
  end;

  { TcxEventEntryIDList }

  TcxEventEntryIDList = class(TcxObjectList)
  private
    function GetItem(Index: Integer): TcxEventInfo;
  public
    procedure Add(AEvent: TcxSchedulerEvent; AEntryIDField: TcxCustomSchedulerStorageField);
    procedure DeleteEvent(AEvent: TcxSchedulerEvent);
    function ExtractEvent(AIndex: Integer): TcxSchedulerEvent;
    function FindEntryID(const AEntryID: string; var Index: Integer): Boolean;
    function IndexOfEntryID(const AEntryID: string): Integer;
    procedure Populate(AStorage: TcxCustomSchedulerStorage;
      AEntryIDField: TcxCustomSchedulerStorageField);

    property Items[Index: Integer]: TcxEventInfo read GetItem; default;
  end;

  { TcxOutlookExchange }

  TcxOutlookExchange = class
  private
    FCalendarFolder: OleVariant;
    FOleInitialized: Boolean;
    FOutlookApp: OleVariant;
    FStorage: TcxCustomSchedulerStorage;
    FWasCreated: Boolean;
    FOnAcceptAppointmentItem: TcxAcceptAppointmentItemImportFunc;
    FOnAcceptEvent: TcxAcceptEventExportFunc;
    FOnConvertAppointmentItemToEvent: TcxAppointmentItemToEventProc;
    FOnConvertEventToAppointmentItem: TcxEventToAppointmentItemProc;
    FOnProgress: TcxSchedulerExchangeProgressProc;
    function GetAllDayEvent(const AppointmentItem: OleVariant): Boolean;
    function GetDaysOfWeekFromMask(AMask: Integer): TDays;
    function GetDayTypeFromMask(AMask: Integer): TcxDayType;
    function GetMaskFromDaysOfWeek(ADays: TDays): Integer;
    function GetMaskFromDayType(ADayType: TcxDayType): Integer;
  protected
    function CanAcceptAppointmentItem(const AppointmentItem: OleVariant): Boolean;
    function CanAcceptEvent(AEvent: TcxSchedulerEvent): Boolean;
    function CanImportAppointmentItem(const AppointmentItem: OleVariant): Boolean; virtual;
    procedure ConvertAppointmentItemToEvent(const AppointmentItem: OleVariant;
      AEvent: TcxSchedulerEvent);
    procedure ConvertEventToAppointmentItem(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant);
    procedure ImportRecurrenceChain(APatternEvent: TcxSchedulerEvent;
      const ARecurrencePattern: OleVariant);
    procedure CheckRecurrencePatternExceptions(
      const ARecurrencePattern: OleVariant;
      APatternEvent: TcxSchedulerEvent);
    procedure ExportRecurrenceChain(APatternEvent: TcxSchedulerEvent; const AppointmentItem: OleVariant);
    procedure ExportRecurrenceException(APatternEvent: TcxSchedulerEvent; const AStart: TDateTime);

    function GetIsValid: Boolean; virtual;
    function InitOleObject: Boolean;
    procedure PostCustomOccurrence(const ARecurrencePattern, AException: OleVariant; APatternEvent: TcxSchedulerEvent);
    procedure PostRecurrenceException(AOccurrence: TcxSchedulerEvent); virtual;
    function RoundTime(ATime: TDateTime; ASaveDate: Boolean): TDateTime; virtual;
    procedure SetAppointmentItemRecurrence(
      const ARecurrencePattern: OleVariant; AEvent: TcxSchedulerEvent);
    procedure SetupAppointmentItemInfo(const AppointmentItem: OleVariant;
      const AEvent: TcxSchedulerEvent; ASetTime: Boolean = True);
    procedure SetupEventInfo(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant);
    procedure SetupEventRecurrenceInfo(AEvent: TcxSchedulerEvent;
      const ARecurrencePattern: OleVariant);
    procedure UpdateAppointmentItem(const AppointmentItem: OleVariant;
      AEvent: TcxSchedulerEvent); virtual;
    procedure UpdateEvent(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant; APostData: Boolean = True); virtual;
    procedure UpdateEventWithRecurrenceInfo(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant);
    function DoProgress(ACurrent, ACount: Integer): Boolean;

    property CalendarFolder: OleVariant read FCalendarFolder;
    property IsValid: Boolean read GetIsValid;
    property OutlookApp: OleVariant read FOutlookApp;
    property Storage: TcxCustomSchedulerStorage read FStorage;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage); virtual;
    destructor Destroy; override;
    procedure ExportEventToOutlook(AEvent: TcxSchedulerEvent);
    procedure ExportToOutlook;
    procedure ImportAppointmentItemFromOutlook(const AppointmentItem: OleVariant);
    procedure ImportFromOutlook;

    property OnAcceptAppointmentItem: TcxAcceptAppointmentItemImportFunc
      read FOnAcceptAppointmentItem write FOnAcceptAppointmentItem;
    property OnAcceptEvent: TcxAcceptEventExportFunc
      read FOnAcceptEvent write FOnAcceptEvent;
    property OnConvertAppointmentItemToEvent: TcxAppointmentItemToEventProc
      read FOnConvertAppointmentItemToEvent write FOnConvertAppointmentItemToEvent;
    property OnConvertEventToAppointmentItem: TcxEventToAppointmentItemProc
      read FOnConvertEventToAppointmentItem write FOnConvertEventToAppointmentItem;
    property OnProgress: TcxSchedulerExchangeProgressProc
      read FOnProgress write FOnProgress;
  end;

  { TcxOutlookSynchronize }

  TcxOutlookSynchronize = class(TcxOutlookExchange)
  private
    FEntryIDField: TcxCustomSchedulerStorageField;
  protected
    procedure DeleteEventExceptions(AEvent: TcxSchedulerEvent; AEntryIDs: TcxEventEntryIDList);
    procedure DeleteUnsynchronizedAppointments(AEntryIDs: TcxAppointmentEntryIDList);
    procedure DeleteUnsynchronizedEvents(AEntryIDs: TcxEventEntryIDList);
    function GetEntryIDFromEvent(AEvent: TcxSchedulerEvent): string;
    function GetIsValid: Boolean; override;
    function IsEqualRecurrencePattern(const AppointmentItem: OleVariant;
      AEvent: TcxSchedulerEvent): Boolean;
    procedure PostRecurrenceException(AOccurrence: TcxSchedulerEvent); override;
    procedure PurgeDeletedOccurrences(APatternEvent: TcxSchedulerEvent;
      AEntryIDs: TcxEventEntryIDList);
    procedure UpdateAppointmentItem(const AppointmentItem: OleVariant;
      AEvent: TcxSchedulerEvent); override;
    procedure UpdateExistingAppointmentItem(const AppointmentItem: OleVariant;
      AEvent: TcxSchedulerEvent);
    procedure UpdateExistingEvent(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant; AEntryIDs: TcxEventEntryIDList);
    procedure UpdateEvent(AEvent: TcxSchedulerEvent;
      const AppointmentItem: OleVariant; APostData: Boolean = True); override;
    procedure SynchronizeRecurrenceChainWithOutlook(
      APatternEvent: TcxSchedulerEvent; const ARecurrencePattern: OleVariant;
      AEntryIDs: TcxEventEntryIDList);
  public
    procedure SynchronizeWithOutlook(ADeleteUnsynchronizedEvents: Boolean = False);
    procedure SynchronizeWithStorage(ADeleteUnsynchronizedAppointments: Boolean = False);

    property EntryIDField: TcxCustomSchedulerStorageField
      read FEntryIDField write FEntryIDField;
  end;

procedure cxSchedulerExportToOutlook(AStorage: TcxCustomSchedulerStorage;
  AcceptFunc: TcxAcceptEventExportFunc = nil;
  AConvertProc: TcxEventToAppointmentItemProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);

procedure cxSchedulerImportFromOutlook(AStorage: TcxCustomSchedulerStorage;
  AcceptFunc: TcxAcceptAppointmentItemImportFunc = nil;
  AConvertProc: TcxAppointmentItemToEventProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);

procedure cxSchedulerSynchronizeOutlookWithStorage(AStorage: TcxCustomSchedulerStorage;
  AppointmentItemIDField: TcxCustomSchedulerStorageField;
  ADeleteUnsynchronizedAppointments: Boolean = False;
  AcceptFunc: TcxAcceptEventExportFunc = nil;
  AConvertProc: TcxEventToAppointmentItemProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);

procedure cxSchedulerSynchronizeStorageWithOutlook(AStorage: TcxCustomSchedulerStorage;
  AppointmentItemIDField: TcxCustomSchedulerStorageField;
  ADeleteUnsynchronizedEvents: Boolean = False;
  AcceptFunc: TcxAcceptAppointmentItemImportFunc = nil;
  AConvertProc: TcxAppointmentItemToEventProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);

implementation

uses
  Windows, SysUtils, cxSchedulerUtils, cxVariants, Forms, Math;

const
  olFolderCalendar  = $00000009;
  olAppointmentItem = $00000001;

  //recurrence
  olRecursDaily    = $00000000;
  olRecursWeekly   = $00000001;
  olRecursMonthly  = $00000002;
  olRecursMonthNth = $00000003;
  olRecursYearly   = $00000005;
  olRecursYearNth  = $00000006;

  // Constants for enum olDaysOfWeek
  olSunday      = $00000001;
  olMonday      = $00000002;
  olTuesday     = $00000004;
  olWednesday   = $00000008;
  olThursday    = $00000010;
  olFriday      = $00000020;
  olSaturday    = $00000040;
  olWeekDay     = olMonday + olTuesday + olWednesday + olThursday + olFriday;
  olWeekEndDay  = olSunday + olSaturday;
  olEveryDay    = olWeekDay + olWeekEndDay;

  // Constants for enum olBusyStatus
  olFree        = $00000000;
  olTentative   = $00000001;
  olBusy        = $00000002;
  olOutOfOffice = $00000003;

  scxOutlookApp = 'outlook.application';
  scxNameSpace  = 'MAPI';

  NullEntryID   = '0000000000000000000000000000000000000000';

{ TcxAppointmentInfo }

constructor TcxAppointmentInfo.Create(const AAppointmentItem: OleVariant);
begin
  FAppointmentItem := AAppointmentItem;
  FEntryID := UpperCase(AAppointmentItem.EntryID);
end;

{ TcxAppointmentEntryIDList }

procedure TcxAppointmentEntryIDList.Add(const AppointmentItem: OleVariant);
begin
  inherited Add(TcxAppointmentInfo.Create(AppointmentItem));
end;

function TcxAppointmentEntryIDList.ExtractAppointmentItem(
  AIndex: Integer): OleVariant;
var
  AppointmentInfo: TcxAppointmentInfo;
begin
  AppointmentInfo := Items[AIndex];
  Result := AppointmentInfo.AppointmentItem;
  AppointmentInfo.Free;
  Delete(AIndex);
end;

function TcxAppointmentEntryIDList.FindEntryID(const AEntryID: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  if Length(AEntryID) = 0 then Exit;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(Items[I].EntryID, AEntryID);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TcxAppointmentEntryIDList.IndexOfEntryID(const AEntryID: string): Integer;
begin
  if not FindEntryID(AEntryID, Result) then
    Result := -1;
end;

function CompareAppointments(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TcxAppointmentInfo(Item1).EntryID, TcxAppointmentInfo(Item2).EntryID);
end;

procedure TcxAppointmentEntryIDList.Populate(ACalendarFolder: OleVariant);
var
  I: Integer;
begin
  Capacity := ACalendarFolder.Items.Count;
  for I := 1 to ACalendarFolder.Items.Count do
    Add(ACalendarFolder.Items(I));
  Sort(CompareAppointments);
end;

function TcxAppointmentEntryIDList.GetItems(Index: Integer): TcxAppointmentInfo;
begin
  Result := TcxAppointmentInfo(List[Index]);
end;

{ TcxEventInfo }

constructor TcxEventInfo.Create(AEvent: TcxSchedulerEvent; AEntryIDField: TcxCustomSchedulerStorageField);
begin
  FEvent := AEvent;
  if AEvent.EventType in [etNone, etPattern] then
    FEntryID := Trim(VarToStr(AEvent.GetCustomFieldValue(AEntryIDField)))
  else
    FEntryID := NullEntryID;
end;

{ TcxEventEntryIDList }

procedure TcxEventEntryIDList.Add(AEvent: TcxSchedulerEvent;
  AEntryIDField: TcxCustomSchedulerStorageField);
begin
  inherited Add(TcxEventInfo.Create(AEvent, AEntryIDField));
end;

procedure TcxEventEntryIDList.DeleteEvent(AEvent: TcxSchedulerEvent);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Event = AEvent then
    begin
      Items[I].Free;
      Delete(I);
      break;
    end;
end;

function TcxEventEntryIDList.ExtractEvent(AIndex: Integer): TcxSchedulerEvent;
var
  AEventInfo: TcxEventInfo;
begin
  AEventInfo := Items[AIndex];
  Result := AEventInfo.Event;
  AEventInfo.Free;
  Delete(AIndex);
end;

function TcxEventEntryIDList.FindEntryID(const AEntryID: string;
  var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  if Length(AEntryID) = 0 then Exit;
  L := 0;
  H := Count - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := CompareStr(Items[I].EntryID, AEntryID);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        L := I;
      end;
    end;
  end;
  Index := L;
end;

function TcxEventEntryIDList.IndexOfEntryID(
  const AEntryID: string): Integer;
begin
  if not FindEntryID(AEntryID, Result) then
    Result := -1;
end;

function CompareEvents(Item1, Item2: Pointer): Integer;
begin
  Result := CompareStr(TcxEventInfo(Item1).EntryID, TcxEventInfo(Item2).EntryID);
end;

procedure TcxEventEntryIDList.Populate(AStorage: TcxCustomSchedulerStorage;
  AEntryIDField: TcxCustomSchedulerStorageField);
var
  I: Integer;
begin
  Capacity := AStorage.EventCount;
  for I := 0 to AStorage.EventCount - 1 do
    Add(AStorage.Events[I], AEntryIDField);
  Sort(CompareEvents);
end;

function TcxEventEntryIDList.GetItem(Index: Integer): TcxEventInfo;
begin
  Result := TcxEventInfo(List[Index]);
end;

{ TcxOutlookExchange }

constructor TcxOutlookExchange.Create(AStorage: TcxCustomSchedulerStorage);
begin
  inherited Create;
  FStorage := AStorage;
  FOleInitialized := InitOleObject;
end;

destructor TcxOutlookExchange.Destroy;
begin
  if FWasCreated then
    FOutlookApp := Unassigned;
  inherited Destroy;
end;

procedure TcxOutlookExchange.ExportEventToOutlook(AEvent: TcxSchedulerEvent);
var
  AppointmentItem: OleVariant;
begin
  if AEvent.EventType in [etNone, etPattern] then
  begin
    AppointmentItem := OutlookApp.CreateItem(olAppointmentItem);
    SetupAppointmentItemInfo(AppointmentItem, AEvent);
    if AEvent.IsRecurring then
      ExportRecurrenceChain(AEvent, AppointmentItem)
    else
      UpdateAppointmentItem(AppointmentItem, AEvent);
  end;
end;

procedure TcxOutlookExchange.ExportToOutlook;
var
  AEvent: TcxSchedulerEvent;
  I, ACount: Integer;
begin
  if not IsValid then Exit;
  Storage.BeginUpdate;
  try
    ACount := Storage.EventCount;
    for I := 0 to ACount - 1 do
    begin
      AEvent := Storage.Events[I];
      if CanAcceptEvent(AEvent) then
        ExportEventToOutlook(AEvent);
      if not DoProgress(I + 1, ACount) then
        Break;
      Application.ProcessMessages;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

procedure TcxOutlookExchange.ImportAppointmentItemFromOutlook(
  const AppointmentItem: OleVariant);
var
  AEvent: TcxSchedulerEvent;
  ARecurrencePattern: OleVariant;
begin
  AEvent := Storage.CreateEvent;
  SetupEventInfo(AEvent, AppointmentItem);
  if AppointmentItem.IsRecurring then
  begin
    ARecurrencePattern := AppointmentItem.GetRecurrencePattern;
    UpdateEventWithRecurrenceInfo(AEvent, AppointmentItem);
    ImportRecurrenceChain(AEvent, ARecurrencePattern);
  end
  else
    UpdateEvent(AEvent, AppointmentItem);
end;

procedure TcxOutlookExchange.ImportFromOutlook;
var
  I, ACount: Integer;
  AppointmentItem: OleVariant;
begin
  if not IsValid then Exit;
  ACount := CalendarFolder.Items.Count;
  for I := 1 to ACount do
  begin
    AppointmentItem := CalendarFolder.Items(I);
    if CanImportAppointmentItem(AppointmentItem) and CanAcceptAppointmentItem(AppointmentItem) then
      ImportAppointmentItemFromOutlook(AppointmentItem);
    if not DoProgress(I, ACount) then
      Break;
    Application.ProcessMessages;
  end;
end;

function TcxOutlookExchange.CanAcceptAppointmentItem(
  const AppointmentItem: OleVariant): Boolean;
begin
  Result := not Assigned(FOnAcceptAppointmentItem) or
    FOnAcceptAppointmentItem(AppointmentItem);
end;

function TcxOutlookExchange.CanAcceptEvent(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := not Assigned(FOnAcceptEvent) or FOnAcceptEvent(AEvent);
end;

function TcxOutlookExchange.CanImportAppointmentItem(
  const AppointmentItem: OleVariant): Boolean;
begin
  Result := not (AppointmentItem.IsRecurring and not Storage.IsRecurrenceAvailable);
end;

procedure TcxOutlookExchange.ConvertAppointmentItemToEvent(
  const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent);
begin
  if Assigned(FOnConvertAppointmentItemToEvent) then
    FOnConvertAppointmentItemToEvent(AppointmentItem, AEvent);
end;

procedure TcxOutlookExchange.ConvertEventToAppointmentItem(
  AEvent: TcxSchedulerEvent; const AppointmentItem: OleVariant);
begin
  if Assigned(FOnConvertEventToAppointmentItem) then
    FOnConvertEventToAppointmentItem(AEvent, AppointmentItem);
end;

procedure TcxOutlookExchange.ImportRecurrenceChain(
  APatternEvent: TcxSchedulerEvent; const ARecurrencePattern: OleVariant);
const
  ExceptionType: array[Boolean] of TcxEventType = (etCustom, etException);
var
  I, ACount: Integer;
  AException: OleVariant;
  ADeleted: Boolean;
begin
  ACount := ARecurrencePattern.Exceptions.Count;
  for I := 1 to ACount do
  begin
    AException := ARecurrencePattern.Exceptions.Item(I);
    ADeleted := AException.Deleted;
    if not ADeleted and not CanAcceptAppointmentItem(AException.AppointmentItem) then
      Continue;
    if not ADeleted then
      PostCustomOccurrence(ARecurrencePattern, AException, APatternEvent)
    else
      ExportRecurrenceException(APatternEvent, AException.OriginalDate);
  end;
end;

procedure TcxOutlookExchange.SetupEventRecurrenceInfo(AEvent: TcxSchedulerEvent;
  const ARecurrencePattern: OleVariant);
var
  AType, AInterval: Integer;
  ADate: TDateTime;
begin
  AEvent.EventType := etPattern;
  ADate := ARecurrencePattern.PatternStartDate;
  if not AEvent.AllDayEvent then
    ADate := ADate + RoundTime(ARecurrencePattern.StartTime, False);
  AEvent.RecurrenceInfo.Start := ADate;
  if ARecurrencePattern.NoEndDate then
    AEvent.RecurrenceInfo.Count := -1
  else
  begin
    ADate := ARecurrencePattern.PatternEndDate;
    if not AEvent.AllDayEvent then
      ADate := ADate + RoundTime(ARecurrencePattern.EndTime, False);
    AEvent.RecurrenceInfo.Finish := ADate;
    AEvent.RecurrenceInfo.Count := ARecurrencePattern.Occurrences;
  end;
  AType := ARecurrencePattern.RecurrenceType;
  case AType of
    olRecursDaily:
      begin
        AEvent.RecurrenceInfo.Recurrence := cxreDaily;
        AEvent.RecurrenceInfo.DayType := cxdtEveryDay;
        AEvent.RecurrenceInfo.Periodicity := ARecurrencePattern.Interval;
      end;
    olRecursWeekly:
      begin
        AInterval := ARecurrencePattern.Interval;
        if AInterval = 0 then
        begin
          AEvent.RecurrenceInfo.Recurrence := cxreDaily;
          AEvent.RecurrenceInfo.DayType := cxdtWeekDay;
        end
        else
        begin
          AEvent.RecurrenceInfo.Recurrence := cxreWeekly;
          AEvent.RecurrenceInfo.Periodicity := AInterval;
        end;
        AEvent.RecurrenceInfo.OccurDays :=
          GetDaysOfWeekFromMask(ARecurrencePattern.DayOfWeekMask);
      end;
    olRecursMonthly:
      begin
        AEvent.RecurrenceInfo.DayType := cxdtDay;
        AEvent.RecurrenceInfo.Recurrence := cxreMonthly;
        AEvent.RecurrenceInfo.Periodicity := ARecurrencePattern.Interval;
        AEvent.RecurrenceInfo.DayNumber := ARecurrencePattern.DayOfMonth
      end;
    olRecursMonthNth:
      begin
        AEvent.RecurrenceInfo.DayType := GetDayTypeFromMask(ARecurrencePattern.DayOfWeekMask);
        AEvent.RecurrenceInfo.Recurrence := cxreMonthly;
        AEvent.RecurrenceInfo.Periodicity := ARecurrencePattern.Interval;
        AEvent.RecurrenceInfo.DayNumber := ARecurrencePattern.Instance;
      end;
    olRecursYearly:
      begin
        AEvent.RecurrenceInfo.DayType := cxdtDay;
        AEvent.RecurrenceInfo.Recurrence := cxreYearly;
        AEvent.RecurrenceInfo.Periodicity := ARecurrencePattern.MonthOfYear;
        AEvent.RecurrenceInfo.DayNumber := ARecurrencePattern.DayOfMonth
      end;
    olRecursYearNth:
      begin
        AEvent.RecurrenceInfo.DayType := GetDayTypeFromMask(ARecurrencePattern.DayOfWeekMask);
        AEvent.RecurrenceInfo.Recurrence := cxreYearly;
        AEvent.RecurrenceInfo.Periodicity := ARecurrencePattern.MonthOfYear;
        AEvent.RecurrenceInfo.DayNumber := ARecurrencePattern.Instance;
      end;
  end;
end;

function TcxOutlookExchange.GetIsValid: Boolean;
begin
  Result := (FStorage <> nil) and FOleInitialized;
end;

{$HINTS OFF}
function TcxOutlookExchange.InitOleObject: Boolean;
var
  ANameSpace: OleVariant;
begin
  Result := False;
  FWasCreated := False;
  try
    FOutlookApp := GetActiveOleObject(scxOutlookApp);
    Result := True;
  except
    FOutlookApp := CreateOleObject(scxOutlookApp);
    FWasCreated := True;
    Result := True;
  end;
  if Result then
  begin
    ANameSpace := OutlookApp.GetNamespace(scxNameSpace);
    FCalendarFolder := ANameSpace.GetDefaultFolder(olFolderCalendar);
  end;
end;
{$HINTS ON}

procedure TcxOutlookExchange.PostCustomOccurrence(const ARecurrencePattern, AException: OleVariant;
  APatternEvent: TcxSchedulerEvent);
var
  AOccurrence: TcxSchedulerEvent;
  AOriginalDate, AOccurrenceStart: TDateTime;
  AChain: TcxSchedulerEventList;
  I: Integer;
begin
  AOriginalDate    := AException.OriginalDate;
  AOccurrenceStart := AException.AppointmentItem.Start;
  if dxDateOf(AOriginalDate) > dxDateOf(AOccurrenceStart) then
  begin
    with TcxSchedulerOccurrenceCalculator.Create(APatternEvent, AOccurrenceStart, AOriginalDate -1) do
    try
      while GetNextOccurrence do
        ExportRecurrenceException(APatternEvent, OccurrenceStart);
    finally
      Free;
    end;
  end;
  if dxDateOf(AOriginalDate) < dxDateOf(AOccurrenceStart) then
  begin
    AChain := APatternEvent.GetRecurrenceChain;
    for I := AChain.Count - 1 downto 0 do
    begin
      AOccurrence := AChain[I];
      if (dxDateOf(AOccurrence.Start) = dxDateOf(AOriginalDate)) and (AOccurrence.EventType = etException) then
      begin
        AOccurrence.EventType := etCustom;
        SetupEventInfo(AOccurrence, AException.AppointmentItem);
        UpdateEvent(AOccurrence, AException.AppointmentItem);
        Exit;
      end;
    end;
  end;
  if dxDateOf(AOriginalDate) <> dxDateOf(AOccurrenceStart) then
  begin
    with TcxSchedulerOccurrenceCalculator.Create(APatternEvent, AOccurrenceStart, AOccurrenceStart) do
    try
      if GetNextOccurrence and (dxDateOf(OccurrenceStart) = dxDateOf(AOccurrenceStart)) then
        ExportRecurrenceException(APatternEvent, OccurrenceStart);
    finally
      Free;
    end;
  end;
  AOccurrence := Storage.CreateOccurrence(APatternEvent, AException.OriginalDate, etCustom);
  SetupEventInfo(AOccurrence, AException.AppointmentItem);
  UpdateEvent(AOccurrence, AException.AppointmentItem);
end;

procedure TcxOutlookExchange.PostRecurrenceException(AOccurrence: TcxSchedulerEvent);
begin
  AOccurrence.Post;
end;

function TcxOutlookExchange.RoundTime(ATime: TDateTime; ASaveDate: Boolean): TDateTime;
begin
  if ASaveDate then
    Result := DateTimeHelper.RoundTime(ATime)
  else
    Result := DateTimeHelper.RoundTime(dxTimeOf(ATime));
end;

procedure TcxOutlookExchange.UpdateAppointmentItem(
  const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent);
begin
  ConvertEventToAppointmentItem(AEvent, AppointmentItem);
  try
    AppointmentItem.Save;
  except
  end;
end;

procedure TcxOutlookExchange.UpdateEvent(AEvent: TcxSchedulerEvent;
  const AppointmentItem: OleVariant; APostData: Boolean = True);
begin
  ConvertAppointmentItemToEvent(AppointmentItem, AEvent);
  if APostData then AEvent.Post;
end;

procedure TcxOutlookExchange.UpdateEventWithRecurrenceInfo(AEvent: TcxSchedulerEvent;
  const AppointmentItem: OleVariant);
begin
  AEvent.BeginEditing;
  try
    SetupEventRecurrenceInfo(AEvent, AppointmentItem.GetRecurrencePattern);
    UpdateEvent(AEvent, AppointmentItem, False);
  finally
    AEvent.EndEditing;
  end;
  AEvent.Post;
end;

function TcxOutlookExchange.DoProgress(ACurrent, ACount: Integer): Boolean;
var
  Abort: Boolean;
begin
  Abort := False;
  if Assigned(FOnProgress) then
    FOnProgress(ACurrent, ACount, Abort);
  Result := not Abort;
end;

function CompareOccurrences(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
var
  ADate1, ADate2: TDateTime;
begin
  ADate1 := dxDateOf(AEvent1.Start);
  ADate2 := dxDateOf(AEvent2.Start);
  if ADate1 = ADate2 then
  begin
    if AEvent1.EventType = AEvent2.EventType then
      Result := 0
    else
      if AEvent1.EventType = etException then
        Result := -1
      else
        Result := 1;
  end
  else
    if ADate1 < ADate2 then
      Result := -1
    else
      Result := 1;
end;

procedure TcxOutlookExchange.CheckRecurrencePatternExceptions(
  const ARecurrencePattern: OleVariant;
  APatternEvent: TcxSchedulerEvent);
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
  AppointmentItem: OleVariant;
begin
  with APatternEvent.GetRecurrenceChain do
  try
    Sort(CompareOccurrences);
    for I := 0 to Count - 1 do
    begin
      AEvent := Items[I];
      try
        AppointmentItem := ARecurrencePattern.GetOccurrence(AEvent.GetOriginalDate);
        if AEvent.EventType = etException then
          AppointmentItem.Delete
        else
          if (AEvent.EventType = etCustom) and CanAcceptEvent(AEvent) then
          begin
            SetupAppointmentItemInfo(AppointmentItem, AEvent);
            UpdateAppointmentItem(AppointmentItem, AEvent);
          end;
      except
      end;
    end;
  finally
    Free;
  end;
end;

procedure TcxOutlookExchange.ExportRecurrenceChain(
  APatternEvent: TcxSchedulerEvent; const AppointmentItem: OleVariant);
var
  ARecurrencePattern: OleVariant;
begin
  if APatternEvent.EventType = etPattern then
  begin
    ARecurrencePattern := AppointmentItem.GetRecurrencePattern;
    SetAppointmentItemRecurrence(ARecurrencePattern, APatternEvent);
    UpdateAppointmentItem(AppointmentItem, APatternEvent);
    CheckRecurrencePatternExceptions(ARecurrencePattern, APatternEvent);
  end;
end;

procedure TcxOutlookExchange.ExportRecurrenceException(APatternEvent: TcxSchedulerEvent; const AStart: TDateTime);
var
  AOccurrence: TcxSchedulerEvent;
begin
  AOccurrence := Storage.CreateOccurrence(APatternEvent, AStart, etException);
  if AOccurrence <> nil then
    PostRecurrenceException(AOccurrence);
end;

procedure TcxOutlookExchange.SetAppointmentItemRecurrence(
  const ARecurrencePattern: OleVariant; AEvent: TcxSchedulerEvent);
begin
  with AEvent.RecurrenceInfo do
  begin
    ARecurrencePattern.PatternStartDate := dxDateOf(AEvent.Start);
    ARecurrencePattern.StartTime := RoundTime(AEvent.Start, False);
    ARecurrencePattern.EndTime := RoundTime(AEvent.Finish, False);
    case Recurrence of
      cxreDaily:
        begin
          if DayType = cxdtEveryDay then
          begin
            ARecurrencePattern.RecurrenceType := olRecursDaily;
            ARecurrencePattern.Interval := Periodicity;
          end
          else
            if DayType = cxdtWeekDay then
            begin
              ARecurrencePattern.RecurrenceType := olRecursWeekly;
              ARecurrencePattern.DayOfWeekMask := olWeekDay;
            end;
        end;
      cxreWeekly:
        begin
          ARecurrencePattern.RecurrenceType := olRecursWeekly;
          ARecurrencePattern.DayOfWeekMask := GetMaskFromDaysOfWeek(OccurDays);
          ARecurrencePattern.Interval := Periodicity;
        end;
      cxreMonthly:
        begin
          if DayType = cxdtDay then
          begin
            ARecurrencePattern.RecurrenceType := olRecursMonthly;
            ARecurrencePattern.DayOfMonth := DayNumber;
            ARecurrencePattern.Interval := Periodicity;
          end
          else
          begin
            ARecurrencePattern.RecurrenceType := olRecursMonthNth;
            ARecurrencePattern.Interval := Periodicity;
            ARecurrencePattern.DayOfWeekMask := GetMaskFromDayType(DayType);
            ARecurrencePattern.Instance := DayNumber;
          end;
        end;
      cxreYearly:
        begin
          if DayType = cxdtDay then
          begin
            ARecurrencePattern.RecurrenceType := olRecursYearly;
            ARecurrencePattern.DayOfMonth := DayNumber;
            ARecurrencePattern.MonthOfYear := Periodicity;
          end
          else
          begin
            ARecurrencePattern.RecurrenceType := olRecursYearNth;
            ARecurrencePattern.MonthOfYear := Periodicity;
            ARecurrencePattern.DayOfWeekMask := GetMaskFromDayType(DayType);
            ARecurrencePattern.Instance := DayNumber;
          end;
        end;
    end;
    if Count = -1 then
      ARecurrencePattern.NoEndDate := True
    else
      if Count = 0 then
        ARecurrencePattern.PatternEndDate := Finish
      else
        ARecurrencePattern.Occurrences := Count;
  end;
end;

function TcxOutlookExchange.GetAllDayEvent(const AppointmentItem: OleVariant): Boolean;
var
  ARecurrencePattern: OleVariant;
begin
  Result := AppointmentItem.AllDayEvent;
  if Result then
  begin
    if AppointmentItem.IsRecurring then
    begin
      ARecurrencePattern := AppointmentItem.GetRecurrencePattern;
      if Integer(ARecurrencePattern.RecurrenceType) in
        [olRecursMonthly, olRecursMonthNth, olRecursYearly, olRecursYearNth] then
      begin
        Result := (RoundTime(ARecurrencePattern.StartTime, False) = 0) and
          (RoundTime(ARecurrencePattern.EndTime, False) = 0);
      end;
    end;
  end;
end;

function TcxOutlookExchange.GetDaysOfWeekFromMask(AMask: Integer): TDays;
var
  I: Integer;
begin
  Result := [];
  for I := 0 to 6 do
    if (1 shl I) and AMask <> 0 then
      Include(Result, TDay(I));
end;

function TcxOutlookExchange.GetDayTypeFromMask(AMask: Integer): TcxDayType;
begin
  case AMask of
    olSunday : Result := cxdtSunday;
    olMonday : Result := cxdtMonday;
    olTuesday : Result := cxdtTuesday;
    olWednesday : Result := cxdtWednesday;
    olThursday: Result := cxdtThursday;
    olFriday: Result := cxdtFriday;
    olSaturday: Result := cxdtSaturday;
    olWeekDay  : Result := cxdtWeekDay;
    olWeekEndDay: Result := cxdtWeekEndDay;
  else
    Result := cxdtEveryDay;
  end;
end;

function TcxOutlookExchange.GetMaskFromDaysOfWeek(ADays: TDays): Integer;
var
  I: TDay;
begin
  Result := 0;
  for I := dSunday to dSaturday do
    if I in ADays then
      Result := Result or (1 shl Ord(I));
end;

function TcxOutlookExchange.GetMaskFromDayType(ADayType: TcxDayType): Integer;
const
  Masks: array[TcxDayType] of Byte = (olEveryDay, olEveryDay, olWeekDay,
    olWeekEndDay, olSunday, olMonday, olTuesday, olWednesday, olThursday,
    olFriday, olSaturday);
begin
  Result := Masks[ADayType];
end;

{ TcxOutlookSynchronize }

procedure TcxOutlookSynchronize.SynchronizeWithOutlook(
  ADeleteUnsynchronizedEvents: Boolean = False);
var
  I, AIndex, ACount: Integer;
  AppointmentItem: OleVariant;
  AEvent: TcxSchedulerEvent;
  AEntryIDs: TcxEventEntryIDList;
begin
  if not IsValid then Exit;
  AEntryIDs := TcxEventEntryIDList.Create;
  try
    AEntryIDs.Populate(Storage, EntryIDField);
    ACount := CalendarFolder.Items.Count;
    for I := 1 to ACount do
    begin
      AppointmentItem := CalendarFolder.Items(I);
      if CanImportAppointmentItem(AppointmentItem) and CanAcceptAppointmentItem(AppointmentItem) then
      begin
        if AEntryIDs.FindEntryID(AppointmentItem.EntryID, AIndex) then
        begin
          AEvent := AEntryIDs.ExtractEvent(AIndex);
          UpdateExistingEvent(AEvent, AppointmentItem, AEntryIDs);
        end
        else
          ImportAppointmentItemFromOutlook(AppointmentItem);
      end;
      if not DoProgress(I, ACount) then
        Exit;
      Application.ProcessMessages;
    end;
    if ADeleteUnsynchronizedEvents then
      DeleteUnsynchronizedEvents(AEntryIDs);
  finally
    AEntryIDs.Free;
  end;
end;

procedure TcxOutlookSynchronize.SynchronizeWithStorage(
  ADeleteUnsynchronizedAppointments: Boolean = False);
var
  AppointmentItem: OleVariant;
  AEntryIDs: TcxAppointmentEntryIDList;
  AEvent: TcxSchedulerEvent;
  I, AIndex, ACount: Integer;
begin
  if not IsValid then Exit;
  AEntryIDs := TcxAppointmentEntryIDList.Create;
  try
    AEntryIDs.Populate(CalendarFolder);
    Storage.BeginUpdate;
    try
      ACount := Storage.EventCount;
      for I := 0 to ACount - 1 do
      begin
        AEvent := Storage.Events[I];
        if CanAcceptEvent(AEvent) then
        begin
          if AEntryIDs.FindEntryID(GetEntryIDFromEvent(AEvent), AIndex) then
          begin
            AppointmentItem := AEntryIDs.ExtractAppointmentItem(AIndex);
            UpdateExistingAppointmentItem(AppointmentItem, AEvent);
          end
          else
            ExportEventToOutlook(AEvent);
        end;
        if not DoProgress(I + 1, ACount) then
          Exit;
        Application.ProcessMessages;
      end;
      if ADeleteUnsynchronizedAppointments then
        DeleteUnsynchronizedAppointments(AEntryIDs);
    finally
      Storage.EndUpdate;
    end;
  finally
    AEntryIDs.Free;
  end;
end;

procedure TcxOutlookSynchronize.UpdateAppointmentItem(
  const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent);
var
  AEntryID: OleVariant;
begin
  ConvertEventToAppointmentItem(AEvent, AppointmentItem);
  try
    AppointmentItem.Save;
    AEntryID := AppointmentItem.EntryID;
    if not VarEqualsSoft(AEvent.GetCustomFieldValue(EntryIDField), AEntryID) then
    begin
      AEvent.SetCustomFieldValue(EntryIDField, AEntryID);
      AEvent.Post;
    end;
  except
  end;
end;

procedure TcxOutlookSynchronize.UpdateExistingAppointmentItem(
  const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent);
begin
  if AppointmentItem.IsRecurring then
    AppointmentItem.ClearRecurrencePattern;
  SetupAppointmentItemInfo(AppointmentItem, AEvent);
  if AEvent.IsRecurring then
    ExportRecurrenceChain(AEvent, AppointmentItem)
  else
    UpdateAppointmentItem(AppointmentItem, AEvent);
end;

procedure TcxOutlookSynchronize.SynchronizeRecurrenceChainWithOutlook(
  APatternEvent: TcxSchedulerEvent;
  const ARecurrencePattern: OleVariant;
  AEntryIDs: TcxEventEntryIDList);
var
  I, J: Integer;
  AppointmentItem: OleVariant;
  AException: OleVariant;
  AOccurrence: TcxSchedulerEvent;
  AOriginalDate: TDateTime;
  ARecurrenceChain: TcxSchedulerEventList;
begin
  PurgeDeletedOccurrences(APatternEvent, AEntryIDs);
  ARecurrenceChain := APatternEvent.GetRecurrenceChain;
  try
    for I := 1 to ARecurrencePattern.Exceptions.Count do
    begin
      AException := ARecurrencePattern.Exceptions.Item(I);
      AOriginalDate := dxDateOf(AException.OriginalDate);
      AOccurrence := nil;
      for J := 0 to ARecurrenceChain.Count - 1 do
      begin
        if dxDateOf(ARecurrenceChain[J].GetOriginalDate) = AOriginalDate then
        begin
          AOccurrence := ARecurrenceChain[J];
          AEntryIDs.DeleteEvent(AOccurrence);
          ARecurrenceChain.Delete(J);
          break;
        end;
      end;
      if not AException.Deleted then
      begin
        AppointmentItem := AException.AppointmentItem;
        if not CanAcceptAppointmentItem(AppointmentItem) then
          continue;
        if AOccurrence = nil then
          AOccurrence := Storage.CreateOccurrence(APatternEvent, AOriginalDate, etCustom);
        SetupEventInfo(AOccurrence, AppointmentItem);
        ConvertAppointmentItemToEvent(AppointmentItem, AOccurrence);
        AOccurrence.Post;
      end
      else
      begin
        if AOccurrence = nil then
          AOccurrence := APatternEvent.GetOccurrence(AOriginalDate);
        if AOccurrence <> nil then
        begin
          AOccurrence.EventType := etException;
          PostRecurrenceException(AOccurrence);
        end;
      end;
    end;
    for I := 0 to ARecurrenceChain.Count - 1 do
    begin
      AOccurrence := ARecurrenceChain[I];
      AEntryIDs.DeleteEvent(AOccurrence);
      AOccurrence.EventType := etNone;
      AOccurrence.Delete;
    end;
  finally
    ARecurrenceChain.Free;
  end;
end;

procedure TcxOutlookSynchronize.UpdateExistingEvent(AEvent: TcxSchedulerEvent;
  const AppointmentItem: OleVariant; AEntryIDs: TcxEventEntryIDList);
var
  ARecurrencePattern: OleVariant;
begin
  SetupEventInfo(AEvent, AppointmentItem);
  if AppointmentItem.IsRecurring then
  begin
    ARecurrencePattern := AppointmentItem.GetRecurrencePattern;
    if AEvent.IsRecurring then
    begin
      if not IsEqualRecurrencePattern(AppointmentItem, AEvent) then
        DeleteEventExceptions(AEvent, AEntryIDs);
      UpdateEventWithRecurrenceInfo(AEvent, AppointmentItem);
      SynchronizeRecurrenceChainWithOutlook(AEvent, ARecurrencePattern, AEntryIDs);
    end
    else
    begin
      UpdateEventWithRecurrenceInfo(AEvent, AppointmentItem);
      ImportRecurrenceChain(AEvent, AppointmentItem.GetRecurrencePattern);
    end;
  end
  else
  begin
    AEvent.RemoveRecurrence;
    UpdateEvent(AEvent, AppointmentItem);
  end;
end;

procedure TcxOutlookSynchronize.UpdateEvent(AEvent: TcxSchedulerEvent;
  const AppointmentItem: OleVariant; APostData: Boolean = True);
var
  AEntryID: string;
begin
  if AEvent.EventType in [etPattern, etNone] then
    AEntryID := AppointmentItem.EntryID
  else
    AEntryID := NullEntryID;
  AEvent.SetCustomFieldValue(EntryIDField, AEntryID);
  inherited UpdateEvent(AEvent, AppointmentItem, APostData);
end;

procedure TcxOutlookSynchronize.DeleteEventExceptions(AEvent: TcxSchedulerEvent;
  AEntryIDs: TcxEventEntryIDList);
var
  I: Integer;
begin
  AEvent.DeleteExceptions;
  for I := AEntryIDs.Count - 1 downto 0 do
    if (AEntryIDs[I].Event <> AEvent) and (AEntryIDs[I].Event.Pattern = AEvent) then
      AEntryIDs.Delete(I);
end;

procedure TcxOutlookSynchronize.DeleteUnsynchronizedAppointments(
  AEntryIDs: TcxAppointmentEntryIDList);
var
  I: Integer;
begin
  for I := 0 to AEntryIDs.Count - 1 do
  try
    AEntryIDs[I].AppointmentItem.Delete;
  except
  end;
end;

procedure TcxOutlookSynchronize.DeleteUnsynchronizedEvents(
  AEntryIDs: TcxEventEntryIDList);
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  Storage.BeginUpdate;
  try
    for I := 0 to AEntryIDs.Count - 1 do
    begin
      AEvent := AEntryIDs[I].Event;
      AEvent.EventType := etNone;
      AEvent.Delete;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

function TcxOutlookSynchronize.GetEntryIDFromEvent(
  AEvent: TcxSchedulerEvent): string;
begin
  Result := UpperCase(VarToStr(AEvent.GetCustomFieldValue(EntryIDField)))
end;

function TcxOutlookSynchronize.GetIsValid: Boolean;
begin
  Result := inherited GetIsValid and (FEntryIDField <> nil)
end;

procedure TcxOutlookSynchronize.PostRecurrenceException(
  AOccurrence: TcxSchedulerEvent);
begin
  AOccurrence.SetCustomFieldValue(EntryIDField, NullEntryID);
  AOccurrence.Post;
end;

procedure TcxOutlookSynchronize.PurgeDeletedOccurrences(
  APatternEvent: TcxSchedulerEvent; AEntryIDs: TcxEventEntryIDList);
var
  AEvent: TcxSchedulerEvent;
  L: TcxSchedulerEventList;
  I: Integer;
begin
  Storage.BeginUpdate;
  try
    L := APatternEvent.GetRecurrenceChain;
    try
      for I := 0 to L.Count - 1 do
      begin
        AEvent := L[I];
        if AEvent.EventType = etException then
        begin
          AEntryIDs.DeleteEvent(AEvent);
          AEvent.Delete;
        end;
      end;
    finally
      L.Free;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

function TcxOutlookSynchronize.IsEqualRecurrencePattern(
  const AppointmentItem: OleVariant; AEvent: TcxSchedulerEvent): Boolean;
var
  ARecurrencePattern: OleVariant;
begin
  with AEvent do
  begin
    Result := (AllDayEvent = GetAllDayEvent(AppointmentItem)) and
      (RoundTime(Start, True) = RoundTime(AppointmentItem.Start, True)) and
      (RoundTime(Finish, True) = RoundTime(AppointmentItem.End, True));
  end;
  if Result then
  begin
    ARecurrencePattern := AppointmentItem.GetRecurrencePattern;
    with AEvent.RecurrenceInfo do
    begin
      Result := (ARecurrencePattern.PatternStartDate = dxDateOf(AEvent.Start)) and
        (RoundTime(ARecurrencePattern.StartTime, False) = RoundTime(AEvent.Start, False)) and
        (RoundTime(ARecurrencePattern.EndTime, False) = RoundTime(AEvent.Finish, False));
      if not Result then Exit;
      case Recurrence of
        cxreDaily:
          if DayType = cxdtEveryDay then
            Result := (ARecurrencePattern.RecurrenceType = olRecursDaily) and
              (ARecurrencePattern.Interval = Periodicity)
          else
            if DayType = cxdtWeekDay then
              Result := (ARecurrencePattern.RecurrenceType = olRecursWeekly) and
                (ARecurrencePattern.DayOfWeekMask = olWeekDay)
            else
              Result := False; //unknown status
        cxreWeekly:
          Result := (ARecurrencePattern.RecurrenceType = olRecursWeekly) and
            (ARecurrencePattern.DayOfWeekMask = GetMaskFromDaysOfWeek(OccurDays)) and
            (ARecurrencePattern.Interval = Periodicity);
        cxreMonthly:
          if DayType = cxdtDay then
            Result := (ARecurrencePattern.RecurrenceType = olRecursMonthly) and
              (ARecurrencePattern.DayOfMonth = DayNumber) and
              (ARecurrencePattern.Interval = Periodicity)
          else
            Result := (ARecurrencePattern.RecurrenceType = olRecursMonthNth) and
              (ARecurrencePattern.Interval = Periodicity) and
              (ARecurrencePattern.DayOfWeekMask = GetMaskFromDayType(DayType)) and
              (ARecurrencePattern.Instance = DayNumber);
        cxreYearly:
          if DayType = cxdtDay then
            Result := (ARecurrencePattern.RecurrenceType = olRecursYearly) and
              (ARecurrencePattern.DayOfMonth = DayNumber) and
              (ARecurrencePattern.MonthOfYear = Periodicity)
          else
            Result := (ARecurrencePattern.RecurrenceType = olRecursYearNth) and
              (ARecurrencePattern.MonthOfYear = Periodicity) and
              (ARecurrencePattern.DayOfWeekMask = GetMaskFromDayType(DayType)) and
              (ARecurrencePattern.Instance = DayNumber);
      end;
      if Result then
      begin
        if Count = -1 then
          Result := ARecurrencePattern.NoEndDate
        else
          if Count = 0 then
            Result := (ARecurrencePattern.PatternEndDate = dxDateOf(Finish))
          else
            Result := (ARecurrencePattern.Occurrences = Count);
      end;
    end;
  end;
end;

//moved to the end because a bug in the delphi parser

procedure TcxOutlookExchange.SetupAppointmentItemInfo(const AppointmentItem: OleVariant;
  const AEvent: TcxSchedulerEvent; ASetTime: Boolean = True);
var
  AControlEvent: TcxSchedulerControlEvent;
begin
  AControlEvent := TcxSchedulerControlEvent.Create(AEvent);
  try
    with AControlEvent do
    begin
      if (AEvent.EventType = etCustom) and (AEvent.Pattern <> nil) then
        ParentID := AEvent.Pattern.ID;
      if ASetTime then
      begin
        AppointmentItem.AllDayEvent := AllDayEvent;
        AppointmentItem.Start := Start;
        AppointmentItem.End := Finish;
      end;
      AppointmentItem.Subject := Caption;
      AppointmentItem.Body := Message;
      AppointmentItem.BusyStatus := State;
      AppointmentItem.Location := Location;
      AppointmentItem.ReminderSet := Reminder;
      if Reminder then
        AppointmentItem.ReminderMinutesBeforeStart := ReminderMinutesBeforeStart;
    end;
  finally
    AControlEvent.Free;
  end;
end;

procedure TcxOutlookExchange.SetupEventInfo(AEvent: TcxSchedulerEvent;
  const AppointmentItem: OleVariant);
begin
  with AEvent do
  begin
    BeginEditing;
    try
      AllDayEvent := GetAllDayEvent(AppointmentItem);
      Start := AppointmentItem.Start;
      Finish := AppointmentItem.End;
      Caption := AppointmentItem.Subject;
      Message := AppointmentItem.Body;
      State := AppointmentItem.BusyStatus;
      Location := AppointmentItem.Location;
      Reminder := AppointmentItem.ReminderSet;
      if Reminder then
        ReminderMinutesBeforeStart := AppointmentItem.ReminderMinutesBeforeStart;
    finally
      EndEditing;
    end;
  end;
end;

// import/export/synchronization

procedure cxSchedulerExportToOutlook(AStorage: TcxCustomSchedulerStorage;
  AcceptFunc: TcxAcceptEventExportFunc = nil;
  AConvertProc: TcxEventToAppointmentItemProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);
begin
  with TcxOutlookExchange.Create(AStorage) do
  try
    OnAcceptEvent := AcceptFunc;
    OnConvertEventToAppointmentItem := AConvertProc;
    OnProgress := AProgressProc;
    ExportToOutlook;
  finally
    Free;
  end;
end;

procedure cxSchedulerImportFromOutlook(AStorage: TcxCustomSchedulerStorage;
  AcceptFunc: TcxAcceptAppointmentItemImportFunc = nil;
  AConvertProc: TcxAppointmentItemToEventProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);
begin
  with TcxOutlookExchange.Create(AStorage) do
  try
    OnAcceptAppointmentItem := AcceptFunc;
    OnConvertAppointmentItemToEvent := AConvertProc;
    OnProgress := AProgressProc;
    ImportFromOutlook;
  finally
    Free;
  end;
end;

procedure cxSchedulerSynchronizeOutlookWithStorage(AStorage: TcxCustomSchedulerStorage;
  AppointmentItemIDField: TcxCustomSchedulerStorageField;
  ADeleteUnsynchronizedAppointments: Boolean = False;
  AcceptFunc: TcxAcceptEventExportFunc = nil;
  AConvertProc: TcxEventToAppointmentItemProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);
begin
  with TcxOutlookSynchronize.Create(AStorage) do
  try
    EntryIDField := AppointmentItemIDField;
    OnAcceptEvent := AcceptFunc;
    OnConvertEventToAppointmentItem := AConvertProc;
    OnProgress := AProgressProc;
    SynchronizeWithStorage(ADeleteUnsynchronizedAppointments);
  finally
    Free;
  end;
end;

procedure cxSchedulerSynchronizeStorageWithOutlook(AStorage: TcxCustomSchedulerStorage;
  AppointmentItemIDField: TcxCustomSchedulerStorageField;
  ADeleteUnsynchronizedEvents: Boolean = False;
  AcceptFunc: TcxAcceptAppointmentItemImportFunc = nil;
  AConvertProc: TcxAppointmentItemToEventProc = nil;
  AProgressProc: TcxSchedulerExchangeProgressProc = nil);
begin
  with TcxOutlookSynchronize.Create(AStorage) do
  try
    EntryIDField := AppointmentItemIDField;
    OnAcceptAppointmentItem := AcceptFunc;
    OnConvertAppointmentItemToEvent := AConvertProc;
    OnProgress := AProgressProc;
    SynchronizeWithOutlook(ADeleteUnsynchronizedEvents);
  finally
    Free;
  end;
end;

end.

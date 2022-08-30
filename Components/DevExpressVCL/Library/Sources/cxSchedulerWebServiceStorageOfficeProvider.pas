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

unit cxSchedulerWebServiceStorageOfficeProvider;

{$I cxVer.inc}
{$SCOPEDENUMS ON}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections,
  cxClasses, dxCore, dxCoreClasses, dxWinInet, cxDateUtils, dxThreading,
  cxSchedulerStorage, cxSchedulerRecurrence,
  cxSchedulerWebServiceStorage,
  dxAuthorizationAgents;

type
  TcxSchedulerWebServiceStorageOfficeProvider = class;
  TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask = class;
  TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask = class;

  { TdxMicrosoftOfficeCalendarEvent }

  TdxMicrosoftOfficeCalendarEvent = class(TdxWebServiceCustomEvent)
  private type
  {$REGION 'private type'}
    TRecurrencePatternType = (AbsoluteYearly, RelativeYearly, AbsoluteMonthly, RelativeMonthly,
      Weekly, Daily);

    TRecurrencePattern = record
      &Type: TRecurrencePatternType;
      Interval: Integer;
      DayOfMonth: Integer;
      Month: Integer;
      DaysOfWeek: TDays;
      FirstDayOfWeek: TDay;
      Index: Integer;
    end;

    TRecurrenceRangeType = (EndDate, NoEnd, Numbered);

    TRecurrenceRange = record
      &Type: TRecurrenceRangeType;
      StartDate: TDateTime;
      EndDate: TDateTime;
      RecurrenceTimeZone: string;
      NumberOfOccurrences: Integer;
    end;

    TPatternedRecurrence = record
      Pattern: TRecurrencePattern;
      Range: TRecurrenceRange;
    end;
  {$ENDREGION}
  private class var
    FDayDictionary: TDictionary<string, TDay>;
    FEventTypeDictionary: TDictionary<string, TcxEventType>;
    FRecurrenceRangeTypeDictionary: TDictionary<string, TRecurrenceRangeType>;
    FRecurrencePatternTypeDictionary: TDictionary<string, TRecurrencePatternType>;
    FRecurrencePatternIndexDictionary: TDictionary<string, Integer>;
    FShowAsList: TStringList;
    class procedure Initialize; static;
    class procedure Finalize; static;
  strict private
    FIsReminderOn: Boolean;
    FPatternedRecurrence: TPatternedRecurrence;

    class function GetTimeZoneBias(ADate: TDateTime; const AName: string): TDateTime; static;
    class function DateTimeToJSON(AValue: TDateTime): TdxJSONObject; static;
    class function JSONToDateTime(AJSONValue: TdxJSONValue; out AIsUTC: Boolean): TDateTime; static;

    function GetProvider: TcxSchedulerWebServiceStorageOfficeProvider; inline;
  protected
    procedure DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent); override;
    procedure DoAssignTo(AEvent: TcxSchedulerWebServiceEvent); override;

    property Provider: TcxSchedulerWebServiceStorageOfficeProvider read GetProvider;
  public
    procedure AssignFrom(const AObject: TdxJSONValue); override;
    function CreateJSONObject: TdxJSONObject; override;

    class function GetOccurrenceID(const AEvent: IdxWebServiceEvent): string; static;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask }

  TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask = class(TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FGetEventListEndPoint = 'https://graph.microsoft.com/v1.0/me/calendars/%s/events';
  {$ENDREGION}
  strict private
    FNextLink: string;
    function GetProvider: TcxSchedulerWebServiceStorageOfficeProvider; inline;
  protected
    function CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean; override;
    procedure DoApplyChanges; override;
    function GetJSONItemList: TdxJSONObject; override;
    function IsFinished: Boolean; override;
    procedure Initialize; override;
    procedure ProcessItemListValues(AList: TdxJSONObject); override;

    function GetJSONItemListUri: string; virtual;

    property NextLink: string read FNextLink;
    property Provider: TcxSchedulerWebServiceStorageOfficeProvider read GetProvider;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask }

  TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask = class(TcxSchedulerWebServiceStorageProviderEventCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FDismissEventObjectName = '/v1.0/me/events/%s/dismissReminder';
  {$ENDREGION}
  strict private
    FDate: TDateTime;
  protected
    procedure DoApplyChanges; override;
    function ExpectResponse: Boolean; override;
    function Process: TdxJSONObject; override;
  public
    constructor Create(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime); reintroduce;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask }

  TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask = class(TcxSchedulerWebServiceStorageProviderCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FMaxOccurrenceOffset = 366;
  {$ENDREGION}
  strict private
    FStart: TDateTime;
    FFinish: TDateTime;
    FSeriesMasterID: string;
    function GetProvider: TcxSchedulerWebServiceStorageOfficeProvider; inline;
  protected
    procedure DoApplyChanges; override;
    function DoExecute: TdxTaskCompletedStatus; override;

    function CreateSyncOccurrencesTask(AEvent: TcxSchedulerWebServiceEvent; AStart, AFinish: TDateTime): TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask;

    property Provider: TcxSchedulerWebServiceStorageOfficeProvider read GetProvider;
  public
    constructor Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider; const ASeriesMasterID: string); reintroduce; overload;
    constructor Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider; const ASeriesMasterID: string;
      AStart, AFinish: TDateTime); reintroduce; overload;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask }

  TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask = class(TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask)
  strict private const
  {$REGION 'strict private const'}
    FGetEventCustomOccurrenceListEndPoint = 'https://graph.microsoft.com/v1.0/me/events/%s/instances?$top=1000&$orderby=Start%%2FDateTime&startDateTime=%s&endDateTime=%s';
  {$ENDREGION}
  strict private
    FLink: TcxObjectLink;
    FSeriesMaster: TcxSchedulerWebServiceEvent;
    FFinishDate: TDateTime;
    FStartDate: TDateTime;
    function HasCustomEvent(ADate: TDateTime; out AEvent: TcxSchedulerWebServiceEvent): Boolean;
    procedure InternalDoApplyChanges(AList: TdxWebServiceEventList);
  protected
    function CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean; override;
    function CanProcessing: Boolean; override;
    procedure DoApplyChanges; overload; override;
    function GetJSONItemListUri: string; override; final;

    property SeriesMaster: TcxSchedulerWebServiceEvent read FSeriesMaster;
    property FinishDate: TDateTime read FFinishDate;
    property StartDate: TDateTime read FStartDate;
  public
    constructor Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider; const ASeriesMaster: TcxSchedulerWebServiceEvent; AStartDate, AFinishDate: TDateTime); reintroduce;
    destructor Destroy; override;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask }

  TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask = class(TcxSchedulerWebServiceStorageProviderModifyEventCustomTask)
  protected
    function ExpectResponse: Boolean; override; final;
    function Process: TdxJSONObject; override; final;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask }

  TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask = class(TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask)
  protected
    procedure DoApplyChanges; override; final;
    function Process: TdxJSONObject; override; final;
  end;

  { TcxSchedulerWebServiceStorageOfficeProviderPostEventTask }

  TcxSchedulerWebServiceStorageOfficeProviderPostEventTask = class(TcxSchedulerWebServiceStorageProviderPostEventCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FPostEventObjectName = '/v1.0/me/calendars/%s/events';
    FGetEventCustomOccurrenceListEndPoint = 'https://graph.microsoft.com/v1.0/me/events/%s/instances?startDateTime=%s&endDateTime=%s';
  {$ENDREGION}
  strict private
    function GetOccurrenceCloudID(const AEvent: IdxWebServiceEvent): string; overload;
  protected
    class function GetOccurrenceCloudID(const AUserAgent, AHeader, ASeriesMasterID: string;
      AOccurrenceDate: TDateTime): string; overload; static;
  protected
    procedure DoApplyChanges; override; final;
    function ExpectResponse: Boolean; override; final;
    function Process: TdxJSONObject; override; final;
  end;

  { TcxSchedulerWebServiceStorageOfficeProvider }

  TcxSchedulerWebServiceStorageOfficeProvider = class(TcxSchedulerWebServiceStorageOAuth2CustomProvider)
  protected const
  {$REGION 'protected const'}
    EndPointServerName = 'graph.microsoft.com';
    UpdateEventObjectName = '/v1.0/me/events/%s';
  {$ENDREGION}
  strict private const
  {$REGION 'strict private const'}
    FCalendarListEndPoint = 'https://graph.microsoft.com/v1.0/me/calendars';
  {$ENDREGION}
  public type
  {$REGION 'public type'}
    TContentType = (Default, Text, HTML);
  {$ENDREGION}
  strict private
    FContentType: TContentType;
    FOccurrencesCheckedStart: TDateTime;
    FOccurrencesCheckedFinish: TDateTime;
    function GetAuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent; inline;
    procedure SetAuthorizationAgent(const Value: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);
  protected
    function GetWebServiceEventClass: TdxWebServiceEventClass; override;
    procedure OccurrencesCheckedTimeBoundsChanged(AStart, AFinish: TDateTime); override;
    procedure ResetOccurrencesCheckedTimeBounds;

  {$REGION 'Tasks'}
    function CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;
    function CreateDismissEventTask(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime): TcxSchedulerWebServiceStorageProviderCustomTask;
    function CreateModifyEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; overload; override;
    function CreatePostEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;
    function CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask; override;
    function CreateSyncSeriesMasterTask(const ASeriesMasterID: string): TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask; overload;
    function CreateSyncSeriesMasterTask(const ASeriesMasterID: string; AStart, AFinish: TDateTime): TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask; overload;
    function CreateUpdateEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override; final;
  {$ENDREGION}

    procedure PopulateCalendarList(const AList: TdxWebServiceCalendarList); override; final;
    procedure UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent); override; final;

    class function IsItemListValid(AList: TdxJSONObject): Boolean; override; final;
    class function IsItemValid(AItem: TdxJSONValue): Boolean; override; final;
    class function GetJSONItemArrayParamName: string; override; final;

    procedure AddException(const APattern: TcxSchedulerWebServiceEvent; ACalculator: TcxSchedulerRecurrenceCalculator);

    procedure DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime); override;

  {$REGION 'IdxOAuth2AuthorizationAgentScopeRequestor'}
    function GetScopes: TStringList; override; final;
  {$ENDREGION}
    property OccurrencesCheckedStart: TDateTime read FOccurrencesCheckedStart;
    property OccurrencesCheckedFinish: TDateTime read FOccurrencesCheckedFinish;
  public
    constructor Create(AOwner: TcxSchedulerWebServiceStorage); override;
    procedure Assign(Source: TPersistent); override;
    class function GetDisplayName: string; override;
  published
    property AuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent read GetAuthorizationAgent write SetAuthorizationAgent;
    property ContentType: TContentType read FContentType write FContentType default TContentType.Default;
  end;

implementation

uses
  Math,
  dxUriRecord, dxStringHelper, dxCoreGraphics,
  cxSchedulerUtils;

type
  TcxSchedulerEventRecurrenceInfoAccess = class(TcxSchedulerEventRecurrenceInfo);
  TcxSchedulerWebServiceEventAccess = class(TcxSchedulerWebServiceEvent);
  TcxSchedulerEventAccess = class(TcxSchedulerEvent);

  TdxNamedOrdinalDictionaryHelper = class
  public
    class function FindName<T>(ADictionary: TDictionary<string, T>; const Value: T): string; static;
  end;

{ TdxNamedOrdinalDictionaryHelper }

class function TdxNamedOrdinalDictionaryHelper.FindName<T>(ADictionary: TDictionary<string, T>; const Value: T): string;
var
  AResult: string;
begin
  for AResult in ADictionary.Keys do
    if TComparer<T>.Default.Compare(Value, ADictionary[AResult]) = 0 then
      Exit(AResult);
  Result := '';
end;

{ TdxMicrosoftOfficeCalendarEvent }

procedure TdxMicrosoftOfficeCalendarEvent.AssignFrom(const AObject: TdxJSONValue);

  function GetRecurrence: TPatternedRecurrence;
  var
    ARecurrence: TdxJSONValue;
    AUtc: Boolean;
    ADaysOfWeek: TdxJSONArray;
    I: Integer;
    ADay: TDay;
    S: string;
  begin
    ARecurrence := AObject.Get('recurrence');
    ZeroMemory(@Result, SizeOf(TPatternedRecurrence));
    if not FRecurrencePatternTypeDictionary.TryGetValue(ARecurrence.GetChildParamValue('pattern', 'type'), Result.Pattern.&Type) then
      Result.Pattern.&Type := TRecurrencePatternType.Daily;
    if not TryStrToInt(ARecurrence.GetChildParamValue('pattern', 'interval'), Result.Pattern.Interval) then
      Result.Pattern.Interval := -1;
    if not TryStrToInt(ARecurrence.GetChildParamValue('pattern', 'month'), Result.Pattern.Month) then
      Result.Pattern.Month := -1;
    if not TryStrToInt(ARecurrence.GetChildParamValue('pattern', 'dayOfMonth'), Result.Pattern.DayOfMonth) then
      Result.Pattern.DayOfMonth := -1;
    Result.Pattern.DaysOfWeek := [];
    if ARecurrence.GetChild('pattern', 'daysOfWeek').IsArray then
    begin
      ADaysOfWeek := TdxJSONArray(ARecurrence.GetChild('pattern', 'daysOfWeek'));
      for I := 0 to ADaysOfWeek.Count - 1 do
        if FDayDictionary.TryGetValue(ADaysOfWeek.Items[I].Value, ADay) then
          Result.Pattern.DaysOfWeek := Result.Pattern.DaysOfWeek + [ADay];
    end;
    if not FDayDictionary.TryGetValue(ARecurrence.GetChildParamValue('pattern', 'firstDayOfWeek'), Result.Pattern.FirstDayOfWeek) then
      Result.Pattern.FirstDayOfWeek := dxGetStartOfWeek;
    if not FRecurrencePatternIndexDictionary.TryGetValue(ARecurrence.GetChildParamValue('pattern', 'index'), Result.Pattern.Index) then
      Result.Pattern.Index := 0;

    if not FRecurrenceRangeTypeDictionary.TryGetValue(ARecurrence.GetChildParamValue('range', 'type'), Result.Range.&Type) then
      Result.Range.&Type := TRecurrenceRangeType.NoEnd;
    S := ARecurrence.GetChildParamValue('range', 'startDate') + #0;
    Result.Range.StartDate := TdxISO8601Helper.StringToDateTime(S, AUtc);
    S := ARecurrence.GetChildParamValue('range', 'endDate') + #0;
    Result.Range.EndDate := TdxISO8601Helper.StringToDateTime(S, AUtc);
    Result.Range.RecurrenceTimeZone := ARecurrence.GetChildParamValue('range', 'recurrenceTimeZone');
    if not TryStrToInt(ARecurrence.GetChildParamValue('range', 'numberOfOccurrences'), Result.Range.NumberOfOccurrences) then
      Result.Range.NumberOfOccurrences := -1;
  end;

var
  AStart, AFinish: TDateTime;
  AReminderMinutesBeforeStart: Integer;
begin
  if not FEventTypeDictionary.TryGetValue(AObject.GetParamValue('type'), FType) then
    FType := etNone;
  FCaption := AObject.GetParamValue('subject');
  FID := AObject.GetParamValue('id');
  AStart := JSONToDateTime(AObject.Get('start'), FIsStartUTC);
  AFinish := JSONToDateTime(AObject.Get('end'), FIsFinishUTC);
  FIsAllDayEvent := AObject.Get('isAllDay') is TdxJSONTrue;
  if FIsAllDayEvent then
  begin
    AStart := dxDateOf(AStart);
    AFinish := dxDateOf(AFinish);
  end;
  FStart := AStart;
  FFinish := AFinish;
  FDescription := AObject.GetChildParamValue('body', 'content');
  if TdxStringHelper.EndsWith(FDescription, #$D#$A) then
    FDescription := TdxStringHelper.Remove(FDescription, Length(FDescription) - 2);
  FIsRecurring := (AObject.GetParamValue('type') = 'seriesMaster') and AObject.HasParam('recurrence');
  FCancelled := AObject.Get('isCancelled') is TdxJSONTrue;
  FLocation := AObject.GetChildParamValue('location', 'displayName');
  FShowAs := FShowAsList.IndexOf(AObject.GetParamValue('showAs'));
  if not IsCancelled then
  begin
    FIsReminderOn := AObject.Get('isReminderOn') is TdxJSONTrue;
    if TryStrToInt(AObject.GetParamValue('reminderMinutesBeforeStart'), AReminderMinutesBeforeStart) then
      FReminders := TArray<Integer>.Create(AReminderMinutesBeforeStart);
  end;

  FColor := clNone;

  if IsRecurring and (FType = etPattern) then
    FPatternedRecurrence := GetRecurrence
  else
  if FType in [etOccurrence, etCustom, etException] then
  begin
    FRecurringID := AObject.GetParamValue('seriesMasterId');
    if FRecurringID <> '' then
    begin
      FOriginalStartTime := dxDateOf(Start);
      if IsOccurrence then
      begin
        if IsCancelled then
          FType := etException
        else
          FType := etCustom;
      end;
    end
    else
      FType := etNone;
  end;
end;

class function TdxMicrosoftOfficeCalendarEvent.GetOccurrenceID(const AEvent: IdxWebServiceEvent): string;
begin
  if AEvent.&Type in [etOccurrence, etException] then
    Result := Format('%s-%s', [AEvent.GetPatternID, TdxISO8601Helper.DateTimeToString(dxDateOf(AEvent.StartDate))])
  else
    Result := AEvent.ID;
end;

function TdxMicrosoftOfficeCalendarEvent.CreateJSONObject: TdxJSONObject;

  function DayToString(ADay: TDay): string;
  begin
    Result := TdxNamedOrdinalDictionaryHelper.FindName<TDay>(FDayDictionary, ADay);
  end;

  function GetDaysOfWeek: TdxJSONArray;
  var
    ADay: TDay;
  begin
    Result := TdxJSONArray.Create;
    for ADay in FPatternedRecurrence.Pattern.DaysOfWeek do
      Result.AddElement(TdxJSONString.Create(DayToString(ADay)));
  end;

  function GetRecurrencePattern: TdxJSONObject;
  begin
    Result := TdxJSONObject.Create;
    Result.AddPair('type', TdxNamedOrdinalDictionaryHelper.FindName<TRecurrencePatternType>(FRecurrencePatternTypeDictionary, FPatternedRecurrence.Pattern.&Type));
    Result.AddPair('firstDayOfWeek', DayToString(FPatternedRecurrence.Pattern.FirstDayOfWeek));
    Result.AddPair('interval', IntToStr(FPatternedRecurrence.Pattern.Interval));
    Result.AddPair('index', TdxNamedOrdinalDictionaryHelper.FindName<Integer>(FRecurrencePatternIndexDictionary, FPatternedRecurrence.Pattern.Index));
    case FPatternedRecurrence.Pattern.&Type of
      TRecurrencePatternType.AbsoluteYearly:
        begin
          Result.AddPair('month', IntToStr(FPatternedRecurrence.Pattern.Month));
          Result.AddPair('dayOfMonth', IntToStr(FPatternedRecurrence.Pattern.DayOfMonth));
        end;
      TRecurrencePatternType.RelativeYearly:
        begin
          Result.AddPair('month', IntToStr(FPatternedRecurrence.Pattern.Month));
          Result.AddPair('daysOfWeek', GetDaysOfWeek);
        end;
      TRecurrencePatternType.AbsoluteMonthly:
        Result.AddPair('dayOfMonth', IntToStr(FPatternedRecurrence.Pattern.DayOfMonth));
      TRecurrencePatternType.RelativeMonthly:
        Result.AddPair('daysOfWeek', GetDaysOfWeek);
      TRecurrencePatternType.Weekly:
        Result.AddPair('daysOfWeek', GetDaysOfWeek);
    end;
  end;

  function GetRecurrenceRange: TdxJSONObject;
  begin
    Result := TdxJSONObject.Create;
    Result.AddPair('type', TdxNamedOrdinalDictionaryHelper.FindName<TRecurrenceRangeType>(FRecurrenceRangeTypeDictionary, FPatternedRecurrence.Range.&Type));
    Result.AddPair('startDate', TdxISO8601Helper.DateTimeToString(FPatternedRecurrence.Range.StartDate));
    Result.AddPair('recurrenceTimeZone', TdxISO8601Helper.UTCTimeZoneName);
    case FPatternedRecurrence.Range.&Type of
      TRecurrenceRangeType.EndDate:
        Result.AddPair('endDate', TdxISO8601Helper.DateTimeToString(FPatternedRecurrence.Range.EndDate));
      TRecurrenceRangeType.Numbered:
        Result.AddPair('numberOfOccurrences', IntToStr(FPatternedRecurrence.Range.NumberOfOccurrences));
    end;
  end;

  function GetRecurrence: TdxJSONValue;
  var
    AResult: TdxJSONObject;
  begin
    if &Type <> etPattern then
      Result := TdxJSONNull.Create
    else
    begin
      AResult := TdxJSONObject.Create;
      AResult.AddPair('pattern', GetRecurrencePattern);
      AResult.AddPair('range', GetRecurrenceRange);
      Result := AResult;
    end;
  end;

var
  ABody: TdxJSONObject;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('subject', Caption);
  ABody := TdxJSONObject.Create;
  ABody.AddPair('contentType', 'text');
  ABody.AddPair('content', Description);
  Result.AddPair('body', ABody);
  Result.AddPair('end', DateTimeToJSON(Finish));
  Result.AddPair('start', DateTimeToJSON(Start));
  Result.AddPair('location', TdxJSONObject.Create(TdxJSONPair.Create('displayName', Location)));
  Result.AddPair('type', TdxNamedOrdinalDictionaryHelper.FindName<TcxEventType>(FEventTypeDictionary, &Type));
  Result.AddPair('isAllDay', TdxJSONValue.CreateBooleanValue(FIsAllDayEvent));
  Result.AddPair('showAs', FShowAsList[FShowAs]);
  if &Type in [etCustom, etException] then
    Result.AddPair('seriesMasterId', RecurringID)
  else
    Result.AddPair('recurrence', GetRecurrence);
  Result.AddPair('IsReminderOn', TdxJSONValue.CreateBooleanValue(FIsReminderOn));
  if Length(FReminders) >= 1 then
    Result.AddPair('reminderMinutesBeforeStart', IntToStr(FReminders[0]));
end;

procedure TdxMicrosoftOfficeCalendarEvent.DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent);

  function GetDaysOfWeek: TDays;
  begin
    case AEvent.RecurrenceInfo.DayType of
      cxdtEveryDay: Result := [dMonday, dTuesday, dWednesday, dThursday, dFriday, dSunday, dSaturday];
      cxdtWeekDay: Result := [dMonday, dTuesday, dWednesday, dThursday, dFriday];
      cxdtWeekEndDay: Result := [dSunday, dSaturday];
      cxdtSunday: Result := [dSunday];
      cxdtMonday: Result := [dMonday];
      cxdtTuesday: Result := [dTuesday];
      cxdtWednesday: Result := [dWednesday];
      cxdtThursday: Result := [dThursday];
      cxdtFriday: Result := [dFriday];
      cxdtSaturday: Result := [dSaturday];
    else
      Result := AEvent.RecurrenceInfo.OccurDays;
    end;
  end;

begin
  inherited DoAssignFrom(AEvent);
  if &Type <> etException then
  begin
    FIsReminderOn := AEvent.Reminder;
    FIsAllDayEvent := AEvent.AllDayEvent;
  end;
  ZeroMemory(@FPatternedRecurrence, SizeOf(TPatternedRecurrence));
  if &Type = etPattern then
  begin
    FPatternedRecurrence.Pattern.FirstDayOfWeek := TDay(DateTimeHelper.StartOfWeek);
    with AEvent.RecurrenceInfo do
    begin
      FPatternedRecurrence.Pattern.DayOfMonth := DayNumber;
      FPatternedRecurrence.Pattern.DaysOfWeek := OccurDays;
      case Recurrence of
        cxreWeekly:
          begin
            FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.Weekly;
          end;
        cxreMonthly:
          begin
            if DayType = cxdtDay then
              FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.AbsoluteMonthly
            else
              FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.RelativeMonthly;
          end;
        cxreYearly:
          begin
            if DayType = cxdtDay then
              FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.AbsoluteYearly
            else
              FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.RelativeYearly;
            FPatternedRecurrence.Pattern.Month := Periodicity;
          end;
      else // cxreDaily
        if DayType = cxdtEveryDay then
          FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.Daily
        else
          FPatternedRecurrence.Pattern.&Type := TRecurrencePatternType.Weekly;
      end;
      if Recurrence = cxreYearly then
        FPatternedRecurrence.Pattern.Interval := YearPeriodicity
      else
        FPatternedRecurrence.Pattern.Interval := Periodicity;
      FPatternedRecurrence.Pattern.Index := DayNumber - 1;
      FPatternedRecurrence.Pattern.DaysOfWeek := GetDaysOfWeek;
      FPatternedRecurrence.Range.NumberOfOccurrences := Count;
      FPatternedRecurrence.Range.StartDate := Start;
      FPatternedRecurrence.Range.EndDate := Finish;
      if FPatternedRecurrence.Range.NumberOfOccurrences > 0 then
        FPatternedRecurrence.Range.&Type := TRecurrenceRangeType.Numbered
      else
        if FPatternedRecurrence.Range.EndDate > FPatternedRecurrence.Range.StartDate then
          FPatternedRecurrence.Range.&Type := TRecurrenceRangeType.EndDate
        else
          FPatternedRecurrence.Range.&Type := TRecurrenceRangeType.NoEnd;
    end;
  end;
end;

procedure TdxMicrosoftOfficeCalendarEvent.DoAssignTo(AEvent: TcxSchedulerWebServiceEvent);
const
  ARecurrenceMap: array[TRecurrencePatternType] of TcxRecurrence = (cxreYearly, cxreYearly,
    cxreMonthly, cxreMonthly, cxreWeekly, cxreDaily);

  procedure SetRelativeRecurrence;
  begin
    with AEvent.RecurrenceInfo do
    begin
      if FPatternedRecurrence.Pattern.&Type in [TRecurrencePatternType.AbsoluteMonthly, TRecurrencePatternType.AbsoluteYearly] then
        DayType := cxdtDay
      else
      begin
        DayNumber := FPatternedRecurrence.Pattern.Index + 1;
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dMonday, dTuesday, dWednesday, dThursday, dFriday, dSunday, dSaturday] then
          DayType := cxdtEveryDay
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dSunday, dSaturday] then
          DayType := cxdtWeekEndDay
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dMonday, dTuesday, dWednesday, dThursday, dFriday] then
          DayType := cxdtWeekDay
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dMonday] then
          DayType := cxdtMonday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dTuesday] then
          DayType := cxdtTuesday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dWednesday] then
          DayType := cxdtWednesday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dThursday] then
          DayType := cxdtThursday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dFriday] then
          DayType := cxdtFriday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dSaturday] then
          DayType := cxdtSaturday
        else
        if FPatternedRecurrence.Pattern.DaysOfWeek = [dSunday] then
          DayType := cxdtSunday;
      end;
      OccurDays := [];
    end;
  end;

begin
  inherited DoAssignTo(AEvent);
  if IsPattern then
  begin
    with AEvent.RecurrenceInfo do
    begin
      Start := AEvent.Start + dxDateOf(FPatternedRecurrence.Range.StartDate) - dxDateOf(AEvent.Start);
      OccurDays := FPatternedRecurrence.Pattern.DaysOfWeek;
      OriginalStart := FPatternedRecurrence.Range.StartDate;
      Recurrence := ARecurrenceMap[FPatternedRecurrence.Pattern.&Type];
      DisplayTimeBias := GetTimeZoneBias(OriginalStart, FPatternedRecurrence.Range.RecurrenceTimeZone);
      DayNumber := FPatternedRecurrence.Pattern.DayOfMonth;
      if Recurrence = cxreYearly then
        YearPeriodicity := FPatternedRecurrence.Pattern.Interval
      else
        Periodicity := FPatternedRecurrence.Pattern.Interval;
      if (Recurrence = cxreWeekly) and (Periodicity = 0) then
      begin
        Recurrence := cxreDaily;
        DayType := cxdtWeekDay;
      end;
      if (Recurrence = cxreDaily) and (Periodicity > 0) then
        DayType := cxdtEveryDay;
      if Recurrence = cxreYearly then
      begin
        Periodicity := FPatternedRecurrence.Pattern.Month;
        SetRelativeRecurrence;
      end;
      if Recurrence = cxreMonthly then
        SetRelativeRecurrence;
      case FPatternedRecurrence.Range.&Type of
        TRecurrenceRangeType.Numbered:
          Count := FPatternedRecurrence.Range.NumberOfOccurrences;
        TRecurrenceRangeType.EndDate:
          begin
            Count := 0;
            Finish := FPatternedRecurrence.Range.EndDate;
          end;
      else
        Finish := -1;
        Count := -1;
      end;
    end;
  end;
  if &Type <> etException then
  begin
    AEvent.AllDayEvent := FIsAllDayEvent;
    AEvent.Reminder := FIsReminderOn;
    if AEvent.Reminder and (&Type = etPattern) then
      AEvent.RecurrenceInfo.DismissDate := Now;
  end;
end;

function TdxMicrosoftOfficeCalendarEvent.GetProvider: TcxSchedulerWebServiceStorageOfficeProvider;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProvider(inherited Provider);
end;

class function TdxMicrosoftOfficeCalendarEvent.GetTimeZoneBias(ADate: TDateTime;
  const AName: string): TDateTime;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to TdxTimeZoneHelper.TimeZoneCount - 1 do
    if CompareText(TdxTimeZoneHelper.TimeZoneInfo(I).StandardName, AName) = 0 then
    begin
      Result := TdxTimeZoneHelper.TimeZoneDaylightBias(ADate, I) / 24 / 60 / 60;
      Break;
    end;
end;

class function TdxMicrosoftOfficeCalendarEvent.DateTimeToJSON(AValue: TDateTime): TdxJSONObject;
var
  ATranslateValue: string;
begin
  Result := TdxJSONObject.Create;
  ATranslateValue := TdxISO8601Helper.DateTimeToString(AValue);
  Result.AddPair('dateTime', ATranslateValue);
  Result.AddPair('timeZone', TdxISO8601Helper.UTCTimeZoneName);
end;

class function TdxMicrosoftOfficeCalendarEvent.JSONToDateTime(AJSONValue: TdxJSONValue; out AIsUTC: Boolean): TDateTime;
var
  AValue: string;
  ATimeZone: string;
  ATimeZoneBias: TDateTime;
begin
  AValue := AJSONValue.GetParamValue('dateTime');
  Result := TdxISO8601Helper.StringToDateTime(AValue, AIsUTC);
  if not AIsUTC and (dxDateOf(Result) <> Result) and AJSONValue.HasParam('timeZone') then
  begin
    ATimeZone := AJSONValue.GetParamValue('timeZone');
    AIsUTC := CompareText(ATimeZone, TdxISO8601Helper.UTCTimeZoneName) = 0;
    if not AIsUTC then
    begin
      ATimeZoneBias := GetTimeZoneBias(Result, ATimeZone);
      Result := Result - ATimeZoneBias;
    end;
  end;
end;

class procedure TdxMicrosoftOfficeCalendarEvent.Initialize;
begin
  FShowAsList := TStringList.Create;
  FShowAsList.Add('free');
  FShowAsList.Add('tentative');
  FShowAsList.Add('busy');
  FShowAsList.Add('oof');
  FShowAsList.Add('workingElsewhere');
  FEventTypeDictionary := TDictionary<string, TcxEventType>.Create;
  FEventTypeDictionary.Add('occurrence', etOccurrence);
  FEventTypeDictionary.Add('exception', etCustom);
  FEventTypeDictionary.Add('seriesMaster', etPattern);
  FEventTypeDictionary.Add('singleInstance', etNone);
  FRecurrencePatternTypeDictionary := TDictionary<string, TRecurrencePatternType>.Create;
  FRecurrencePatternTypeDictionary.Add('absoluteYearly', TRecurrencePatternType.AbsoluteYearly);
  FRecurrencePatternTypeDictionary.Add('relativeYearly', TRecurrencePatternType.RelativeYearly);
  FRecurrencePatternTypeDictionary.Add('absoluteMonthly', TRecurrencePatternType.AbsoluteMonthly);
  FRecurrencePatternTypeDictionary.Add('relativeMonthly', TRecurrencePatternType.RelativeMonthly);
  FRecurrencePatternTypeDictionary.Add('weekly', TRecurrencePatternType.Weekly);
  FRecurrencePatternTypeDictionary.Add('daily', TRecurrencePatternType.Daily);
  FRecurrencePatternIndexDictionary := TDictionary<string, Integer>.Create;
  FRecurrencePatternIndexDictionary.Add('first', 0);
  FRecurrencePatternIndexDictionary.Add('second', 1);
  FRecurrencePatternIndexDictionary.Add('third', 2);
  FRecurrencePatternIndexDictionary.Add('fourth', 3);
  FRecurrencePatternIndexDictionary.Add('last', 4);
  FDayDictionary := TDictionary<string, TDay>.Create;
  FDayDictionary.Add('sunday', dSunday);
  FDayDictionary.Add('monday', dMonday);
  FDayDictionary.Add('tuesday', dTuesday);
  FDayDictionary.Add('wednesday', dWednesday);
  FDayDictionary.Add('thursday', dThursday);
  FDayDictionary.Add('friday', dFriday);
  FDayDictionary.Add('saturday', dSaturday);
  FRecurrenceRangeTypeDictionary := TDictionary<string, TRecurrenceRangeType>.Create;
  FRecurrenceRangeTypeDictionary.Add('endDate', TRecurrenceRangeType.EndDate);
  FRecurrenceRangeTypeDictionary.Add('noEnd', TRecurrenceRangeType.NoEnd);
  FRecurrenceRangeTypeDictionary.Add('numbered', TRecurrenceRangeType.Numbered);
end;

class procedure TdxMicrosoftOfficeCalendarEvent.Finalize;
begin
  FreeAndNil(FRecurrenceRangeTypeDictionary);
  FreeAndNil(FDayDictionary);
  FreeAndNil(FRecurrencePatternIndexDictionary);
  FreeAndNil(FRecurrencePatternTypeDictionary);
  FreeAndNil(FEventTypeDictionary);
  FreeAndNil(FShowAsList);
end;

{ TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask }

function TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean;
begin
  Result := (TdxMicrosoftOfficeCalendarEvent(AEvent).&Type <> etOccurrence) and inherited CanAddEvent(AEvent);
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.DoApplyChanges;
var
  AEvent: IdxWebServiceEvent;
  AList: TdxWebServiceEventList;
begin
  AList := TdxWebServiceEventList.Create;
  try
    for AEvent in List.Values do
      if AEvent.IsPattern then
        AList.Add(AEvent);
    inherited DoApplyChanges;
    for AEvent in AList do
      RunNewTask(Provider.CreateSyncSeriesMasterTask(AEvent.ID));
  finally
    AList.Free;
  end;
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.GetJSONItemList: TdxJSONObject;
begin
  if FNextLink = '' then
    Result := TdxHttpHelper.GetRequest(UserAgent, GetJSONItemListUri, Header)
  else
    Result := TdxHttpHelper.GetRequest(UserAgent, FNextLink, Header);
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.Initialize;
const
  AContentTypeMap: array[TcxSchedulerWebServiceStorageOfficeProvider.TContentType] of string =
    ('Prefer: outlook.body-content-type="text"',
    'Prefer: outlook.body-content-type="text"',
    'Prefer: outlook.body-content-type="HTML"');
begin
  inherited Initialize;
  Header := Format('%s'#13#10'%s', [Header, AContentTypeMap[Provider.ContentType]]);
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.ProcessItemListValues(
  AList: TdxJSONObject);
begin
  inherited ProcessItemListValues(AList);
  FNextLink := AList.GetParamValue('@odata.nextLink');
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.GetJSONItemListUri: string;
begin
  Result := Format(FGetEventListEndPoint, [CalendarID])
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.GetProvider: TcxSchedulerWebServiceStorageOfficeProvider;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProvider(inherited Provider);
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.IsFinished: Boolean;
begin
  Result := not CanProcessing or (FNextLink = '');
end;

{ TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask }

constructor TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask.Create(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime);
begin
  inherited Create(AEvent);
  FDate := ADate;
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask.DoApplyChanges;
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask.ExpectResponse: Boolean;
begin
  Result := False;
end;

function TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask.Process: TdxJSONObject;
var
  AEventID: string;
begin
  if Event.EventType = etPattern then
    AEventID := TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.GetOccurrenceCloudID(UserAgent,
      Header, Event.CloudID, FDate)
  else
    AEventID := Event.CloudID;
  Result := TdxHttpHelper.PostRequest(UserAgent,
    TcxSchedulerWebServiceStorageOfficeProvider.EndPointServerName,
    Format(FDismissEventObjectName, [AEventID]),
    Header, nil);
end;

{ TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask }

constructor TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.Create(
  AProvider: TcxSchedulerWebServiceStorageCustomProvider; const ASeriesMasterID: string);
begin
  inherited Create(AProvider);
  FSeriesMasterID := ASeriesMasterID;
  FStart := Provider.OccurrencesCheckedStart;
  FFinish := Provider.OccurrencesCheckedFinish;
end;

constructor TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.Create(
  AProvider: TcxSchedulerWebServiceStorageCustomProvider; const ASeriesMasterID: string;
  AStart, AFinish: TDateTime);
begin
  Create(AProvider, ASeriesMasterID);
  FStart := AStart;
  FFinish := AFinish;
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.DoApplyChanges;
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.DoExecute: TdxTaskCompletedStatus;
const
  ATimeRangeMap: array[TcxRecurrence] of Integer = (365, 365 * 4, 365 * 5, 365 * 5);

  procedure RunTask(AEvent: TcxSchedulerWebServiceEvent; AStart, AFinish: TDateTime);
  begin
    MainThreadSynchronize(
      procedure()
      begin
        dxTasksDispatcher.Run(CreateSyncOccurrencesTask(AEvent, AStart, AFinish));
      end);
  end;

var
  AEvent: TcxSchedulerWebServiceEvent;
  AMinDate, AMaxDate: TDateTime;
  AStart: TDateTime;
  ATimeRange: Integer;
  ACalculator: TcxSchedulerOccurrenceCalculator;
begin
  Result := TdxTaskCompletedStatus.Success;
  MainThreadSynchronize(
    procedure()
    begin
      Provider.FindEvent(FSeriesMasterID, AEvent);
    end);
  if AEvent = nil then
    Exit(TdxTaskCompletedStatus.Fail);
  if not CanProcessing then
    Exit(TdxTaskCompletedStatus.Cancelled);
  AMinDate := dxDateOf(AEvent.Start) - FMaxOccurrenceOffset;
  AMinDate := Max(FStart, AMinDate);
  if AEvent.RecurrenceInfo.Finish = -1 then
    AMaxDate := cxMaxDate
  else
    AMaxDate := dxDateOf(AEvent.RecurrenceInfo.Finish);
  AMaxDate := Min(AMaxDate, FFinish);
  if not CanProcessing then
    Exit(TdxTaskCompletedStatus.Fail);
  ACalculator := TcxSchedulerEventRecurrenceInfoAccess(AEvent.RecurrenceInfo).GetCalculatorClass.Create(AEvent, AMinDate, AMaxDate) as TcxSchedulerOccurrenceCalculator;
  try
    AStart := AMinDate;
    ATimeRange := ATimeRangeMap[AEvent.RecurrenceInfo.Recurrence];
    while ACalculator.GetNextOccurrence do
    begin
      if dxDateOf(ACalculator.OccurrenceFinish) > AMaxDate then
        Break;
      if dxDateOf(ACalculator.OccurrenceFinish) > dxDateOf(AStart + ATimeRange) then
      begin
        RunTask(AEvent, AStart, dxDateOf(ACalculator.OccurrenceFinish));
        AStart := dxDateOf(ACalculator.OccurrenceFinish);
      end;
      if not CanProcessing then
        Exit(TdxTaskCompletedStatus.Fail);
    end;
    RunTask(AEvent, AStart, Max(dxDateOf(ACalculator.OccurrenceFinish), AMaxDate) + 1);
  finally
    ACalculator.Free;
  end;
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.CreateSyncOccurrencesTask(AEvent: TcxSchedulerWebServiceEvent; AStart, AFinish: TDateTime): TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.Create(Provider, AEvent, AStart, AFinish);
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.GetProvider: TcxSchedulerWebServiceStorageOfficeProvider;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProvider(inherited Provider);
end;

{ TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask }

constructor TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.Create(
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
  const ASeriesMaster: TcxSchedulerWebServiceEvent;
  AStartDate, AFinishDate: TDateTime);
begin
  inherited Create(AProvider);
  FSeriesMaster := ASeriesMaster;
  FLink := cxAddObjectLink(FSeriesMaster);
  FStartDate := AStartDate;
  FFinishDate := AFinishDate;
end;

destructor TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.Destroy;
begin
  cxRemoveObjectLink(FLink);
  inherited Destroy;
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean;
begin
  Result := AEvent.&Type in [etOccurrence, etCustom];
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.CanProcessing: Boolean;
begin
  Result := inherited CanProcessing and (FLink.Ref <> nil);
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.DoApplyChanges;
var
  AList: TdxWebServiceEventList;
  AComparer: IComparer<IdxWebServiceEvent>;
begin
  if (List.Count = 0) and (FinishDate <= SeriesMaster.RecurrenceInfo.Start) then
    Exit;
  AList := TdxWebServiceEventList.Create(List.Values);
  try
    AComparer := TdxWebServiceEventStartDateComparer.Create;
    AList.Sort(AComparer);
    Provider.BeginSynchronize;
    try
      InternalDoApplyChanges(AList);
      SeriesMaster.RecurrenceInfo.Validate;
    finally
      Provider.EndSynchronize;
    end;
  finally
    AList.Free;
  end;
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.GetJSONItemListUri: string;
var
  AStart, AFinish: string;
begin
  AStart := TdxISO8601Helper.DateTimeToString(FStartDate);
  AFinish := TdxISO8601Helper.DateTimeToString(FFinishDate);
  Result := Format(FGetEventCustomOccurrenceListEndPoint, [FSeriesMaster.CloudID, AStart, AFinish]);
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.InternalDoApplyChanges(AList: TdxWebServiceEventList);

  procedure TryAddException(ACalculator: TcxSchedulerOccurrenceCalculator);
  var
    ASchedulerEvent: TcxSchedulerWebServiceEvent;
  begin
    if not HasCustomEvent(ACalculator.OccurrenceStart, ASchedulerEvent) then
      Provider.AddException(SeriesMaster, ACalculator)
    else
      ASchedulerEvent.Delete;
  end;

  procedure TryAddCustom(const AEvent: IdxWebServiceEvent; AIndex: Integer);
  var
    ASchedulerEvent: TcxSchedulerWebServiceEvent;
  begin
    if Provider.FindEvent(AEvent.ID, ASchedulerEvent) then
      AEvent.AssignTo(ASchedulerEvent)
    else
    begin
      ASchedulerEvent := Provider.AddNewEvent(AEvent);
      ASchedulerEvent.ParentID := SeriesMaster.ID;
    end;
    ASchedulerEvent.RecurrenceIndex := AIndex;
  end;

var
  AEvent: IdxWebServiceEvent;
  ACalculator: TcxSchedulerOccurrenceCalculator;
  ALastIndex: Integer;
begin
  Provider.Owner.BeginUpdate;
  try
    ACalculator := TcxSchedulerEventRecurrenceInfoAccess(SeriesMaster.RecurrenceInfo).GetCalculatorClass.Create(SeriesMaster, StartDate, FinishDate) as TcxSchedulerOccurrenceCalculator;
    try
      ALastIndex := ACalculator.Index;
      while ACalculator.GetNextOccurrence do
      begin
        if dxDateOf(ACalculator.OccurrenceStart) > FinishDate then
          Break;
        if AList.Count > 0 then
        begin
          AEvent := AList[0];
          if AEvent.&Type = etOccurrence then
          begin
            if dxDateOf(AEvent.StartDate) = dxDateOf(ACalculator.OccurrenceStart) then
              AList.Delete(0)
            else
            begin
              TryAddException(ACalculator);
            end;
          end
          else
          begin
            TryAddCustom(AEvent, ACalculator.Index);
            AList.Delete(0);
          end;
        end
        else
          TryAddException(ACalculator);
        ALastIndex := ACalculator.Index;
      end;
      while AList.Count > 0 do
      begin
        AEvent := AList[0];
        Inc(ALastIndex);
        TryAddCustom(AEvent, ALastIndex);
        AList.Delete(0);
      end;
    finally
      ACalculator.Free;
    end;
  finally
    Provider.Owner.EndUpdate;
  end;
end;

function TcxSchedulerWebServiceStorageOfficeProviderSyncOccurrencesTask.HasCustomEvent(ADate: TDateTime; out AEvent: TcxSchedulerWebServiceEvent): Boolean;
var
  ANext: TcxSchedulerEvent;
begin
  Result := False;
  ANext := TcxSchedulerEventAccess(SeriesMaster).Link;
  while ANext <> nil do
  begin
    if dxDateOf(ANext.Start) = dxDateOf(ADate) then
    begin
      AEvent := TcxSchedulerWebServiceEvent(ANext);
      Exit(True);
    end;
    ANext := TcxSchedulerEventAccess(ANext).Link;
  end;
end;

{ TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask }

function TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask.ExpectResponse: Boolean;
begin
  Result := WebServiceEvent.&Type <> etException;
end;

function TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask.Process: TdxJSONObject;
var
  AObject: TdxJSONObject;
begin
  if WebServiceEvent.GetID = '' then
    Exit(nil);
  AObject := ToJSONObject(WebServiceEvent);
  try
    Result := TdxHttpHelper.PatchRequest(UserAgent,
      TcxSchedulerWebServiceStorageOfficeProvider.EndPointServerName,
      Format(TcxSchedulerWebServiceStorageOfficeProvider.UpdateEventObjectName, [WebServiceEvent.ID]),
      Header, AObject);
  finally
    AObject.Free;
  end;
end;

{ TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask }

procedure TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask.DoApplyChanges;
begin
  if WebServiceEvent.&Type <> etException then
    inherited DoApplyChanges;
end;

function TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask.Process: TdxJSONObject;
begin
  if WebServiceEvent.GetID = '' then
    Exit(nil);
  Result := TdxHttpHelper.DeleteRequest(UserAgent,
    TcxSchedulerWebServiceStorageOfficeProvider.EndPointServerName,
    Format(TcxSchedulerWebServiceStorageOfficeProvider.UpdateEventObjectName, [WebServiceEvent.ID]),
    Header, nil);
end;

{ TcxSchedulerWebServiceStorageOfficeProviderPostEventTask }

function TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.GetOccurrenceCloudID(const AEvent: IdxWebServiceEvent): string;
begin
  Result := GetOccurrenceCloudID(UserAgent, Header, AEvent.GetPatternID, AEvent.GetOriginalOccurrenceStartTime);
end;

class function TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.GetOccurrenceCloudID(const AUserAgent, AHeader, ASeriesMasterID:
  string; AOccurrenceDate: TDateTime): string;
var
  AGetOccurrencesEndPoint: string;
  AObject: TdxJSONObject;
  AList: TdxJSONArray;
  ADate: TDateTime;
begin
  Result := '';
  ADate := dxDateOf(AOccurrenceDate);
  AGetOccurrencesEndPoint := Format(FGetEventCustomOccurrenceListEndPoint, [ASeriesMasterID, TdxISO8601Helper.DateTimeToString(ADate), TdxISO8601Helper.DateTimeToString(ADate + 1)]);
  AObject := TdxHttpHelper.GetRequest(AUserAgent, AGetOccurrencesEndPoint, AHeader);
  if Assigned(AObject) then
  try
    if AObject.HasParam('value') and AObject.GetPair('value').JsonValue.IsArray then
    begin
      AList := TdxJSONArray(AObject.GetPair('value').JsonValue);
      if AList.Count > 0 then
        Result := AList.Items[0].GetParamValue('id');
    end;
  finally
    AObject.Free;
  end;
end;

procedure TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.DoApplyChanges;
begin
  if not WebServiceEvent.IsOccurrence then
    inherited DoApplyChanges;
end;

function TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.ExpectResponse: Boolean;
begin
  Result := CanProcessing and not WebServiceEvent.IsOccurrence;
end;

function TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.Process: TdxJSONObject;
var
  AObject: TdxJSONObject;
  AItem: IdxWebServiceEvent;
begin
  if WebServiceEvent.IsOccurrence then
  begin
    AItem := WebServiceEvent;
    if AItem.ID = '' then
      AItem.ID := GetOccurrenceCloudID(AItem);
    if AItem.&Type = etException then
      RunDeleteEventTask(AItem)
    else
      RunModifyEventTask(AItem);
    Result := nil;
  end
  else
  begin
    AObject := ToJSONObject(WebServiceEvent);
    try
      Result := TdxHttpHelper.PostRequest(UserAgent,
        TcxSchedulerWebServiceStorageOfficeProvider.EndPointServerName,
        Format(FPostEventObjectName, [CalendarID]),
        Header, AObject);
    finally
      AObject.Free;
    end;
  end;
end;

{ TcxSchedulerWebServiceStorageOfficeProvider }

procedure TcxSchedulerWebServiceStorageOfficeProvider.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerWebServiceStorageOfficeProvider then
    FContentType := TcxSchedulerWebServiceStorageOfficeProvider(Source).ContentType;
  inherited Assign(Source);
end;

constructor TcxSchedulerWebServiceStorageOfficeProvider.Create(AOwner: TcxSchedulerWebServiceStorage);
begin
  inherited Create(AOwner);
  ResetOccurrencesCheckedTimeBounds;
end;

function TcxSchedulerWebServiceStorageOfficeProvider.GetScopes: TStringList;
begin
  Result := TStringList.Create;
  if ReadOnly then
    Result.Add('Calendars.Read')
  else
    Result.Add('Calendars.ReadWrite');
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderDeleteEventTask.Create(AEvent, AWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateDismissEventTask(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderDismissReminderTask.Create(AEvent, ADate);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateModifyEventTask(
  AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  if AEvent.EventType = etException then
    Result := CreateDeleteEventTask(AEvent, AWebServiceEvent)
  else
    Result := TcxSchedulerWebServiceStorageOfficeProviderModifyEventTask.Create(AEvent, AWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreatePostEventTask(
  AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderPostEventTask.Create(AEvent);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderSyncEventsTask.Create(Self);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateSyncSeriesMasterTask(const ASeriesMasterID: string): TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.Create(Self, ASeriesMasterID);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateSyncSeriesMasterTask(const ASeriesMasterID: string; AStart, AFinish: TDateTime): TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask;
begin
  Result := TcxSchedulerWebServiceStorageOfficeProviderSyncSeriesMasterTask.Create(Self, ASeriesMasterID, AStart, AFinish);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.CreateUpdateEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := nil;
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.PopulateCalendarList(const AList: TdxWebServiceCalendarList);
var
  AJSONObject: TdxJSONObject;
  APair: TdxJSONPair;
  AArray: TdxJSONArray;
  AItem: TdxJSONValue;
  I: Integer;
  ACalendar: TdxWebServiceCalendar;
begin
  AuthorizationAgent.ValidateAccessToken;
  AJSONObject := GetJSONObject(FCalendarListEndPoint);
  if AJSONObject <> nil then
  try
    if AJSONObject.HasParam('@odata.context') then
    begin
      APair := AJSONObject.GetPair('value');
      if (APair <> nil) and (APair.JsonValue is TdxJSONArray) then
      begin
        AArray := TdxJSONArray(APair.JsonValue);
        for I := 0 to AArray.Count - 1 do
        begin
          AItem := AArray.Items[I];
          if AItem.HasParam('id') then
          begin
            ACalendar := TdxWebServiceCalendar.CreateDefault;
            ACalendar.Name := AItem.GetParamValue('name');
            ACalendar.ID := AItem.GetParamValue('id');
            ACalendar.DefaultReminders := nil;
            ACalendar.ReadOnly := AItem.Get('canEdit') is TdxJSONFalse;
            AList.Add(ACalendar);
          end;
        end;
      end;
    end
    else
      DoError(AJSONObject);
  finally
    AJSONObject.Free;
  end;
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent);
var
  AHasOccurrences: Boolean;
begin
  AHasOccurrences := TcxSchedulerWebServiceEventAccess(AEvent).Link <> nil;
  TcxSchedulerWebServiceEventAccess(AEvent).InternalDeleteOccurrences;
  if AHasOccurrences then
    AddTask(CreateSyncSeriesMasterTask(AEvent.CloudID));
end;

class function TcxSchedulerWebServiceStorageOfficeProvider.IsItemListValid(AList: TdxJSONObject): Boolean;
begin
  Result := AList.HasParam('@odata.context');
end;

class function TcxSchedulerWebServiceStorageOfficeProvider.IsItemValid(AItem: TdxJSONValue): Boolean;
begin
  Result := AItem.HasParam('@odata.etag') and AItem.HasParam('id');
end;

class function TcxSchedulerWebServiceStorageOfficeProvider.GetJSONItemArrayParamName: string;
begin
  Result := 'value';
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.AddException(
  const APattern: TcxSchedulerWebServiceEvent; ACalculator: TcxSchedulerRecurrenceCalculator);
var
  AEvent: IdxWebServiceEvent;
begin
  AEvent := GetWebServiceEventClass.Create(Self);
  AEvent.AssignFrom(APattern);
  with TdxMicrosoftOfficeCalendarEvent(AEvent) do
  begin
    FStart := ACalculator.OccurrenceStart;
    FFinish := ACalculator.OccurrenceFinish;
    FOriginalStartTime := ACalculator.OccurrenceStart;
    FType := etException;
    FRecurringID := APattern.CloudID;
    FID := TdxMicrosoftOfficeCalendarEvent.GetOccurrenceID(AEvent);
  end;
  with AddNewEvent(AEvent) do
  begin
    ParentID := APattern.ID;
    RecurrenceIndex := ACalculator.Index;
  end;
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime);
begin
  inherited DismissEvent(AEvent, ADate);
  AddTask(CreateDismissEventTask(AEvent, ADate));
end;

function TcxSchedulerWebServiceStorageOfficeProvider.GetAuthorizationAgent: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent;
begin
  Result := TdxMicrosoftGraphAPIOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

function TcxSchedulerWebServiceStorageOfficeProvider.GetWebServiceEventClass: TdxWebServiceEventClass;
begin
  Result := TdxMicrosoftOfficeCalendarEvent;
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.OccurrencesCheckedTimeBoundsChanged(AStart, AFinish: TDateTime);

  procedure UpdateSeriesMasters(AStart, AFinish: TDateTime);
  var
    I: Integer;
  begin
    for I := 0 to Owner.EventCount - 1 do
      if (Owner.Events[I].EventType = etPattern) and (Owner.Events[I].ResourceID = CalendarID) then
        AddTask(CreateSyncSeriesMasterTask(TcxSchedulerWebServiceEvent(Owner.Events[I]).CloudID, AStart, AFinish));
  end;

var
  AOccurrencesCheckedStart: TDateTime;
  AOccurrencesCheckedFinish: TDateTime;
begin
  AOccurrencesCheckedStart := dxDateOf(AStart - 365);
  AOccurrencesCheckedFinish := dxDateOf(AFinish + 365);
  if AOccurrencesCheckedStart < FOccurrencesCheckedStart then
  begin
    UpdateSeriesMasters(AOccurrencesCheckedStart, FOccurrencesCheckedStart);
    FOccurrencesCheckedStart := AOccurrencesCheckedStart
  end;
  if AOccurrencesCheckedFinish > FOccurrencesCheckedFinish then
  begin
    UpdateSeriesMasters(FOccurrencesCheckedFinish, AOccurrencesCheckedFinish);
    FOccurrencesCheckedFinish := AOccurrencesCheckedFinish;
  end;
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.ResetOccurrencesCheckedTimeBounds;
begin
  FOccurrencesCheckedStart := dxDateOf(Now);
  FOccurrencesCheckedFinish := dxDateOf(Now);
end;

class function TcxSchedulerWebServiceStorageOfficeProvider.GetDisplayName: string;
begin
  Result := 'Microsoft Office Calendar';
end;

procedure TcxSchedulerWebServiceStorageOfficeProvider.SetAuthorizationAgent(
  const Value: TdxMicrosoftGraphAPIOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

initialization
  TdxMicrosoftOfficeCalendarEvent.Initialize;
  TcxSchedulerWebServiceStorageOfficeProvider.Register;

finalization
  TcxSchedulerWebServiceStorageOfficeProvider.Unregister;
  TdxMicrosoftOfficeCalendarEvent.Finalize;

end.

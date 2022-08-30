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

unit cxSchedulerWebServiceStorageGoogleProvider;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Windows, Classes, Graphics, Generics.Defaults, Generics.Collections,
  cxClasses, dxCore, dxCoreClasses, dxWinInet,
  cxSchedulerWebServiceStorage, dxAuthorizationAgents;

type
  TcxSchedulerWebServiceStorageGoogleProvider = class;

  { TdxGoogleCalendarEvent }

  TdxGoogleCalendarEvent = class(TdxWebServiceCustomEvent)
  public
    class function GetReminders(AOverrides: TdxJSONValue): TArray<Integer>; static;
  strict private
    FRecurrence: string;
    class function DateTimeToJSON(AValue: TDateTime): TdxJSONObject; static;
    class function JSONToDateTime(AJSONValue: TdxJSONValue; out AIsDatePart, AIsUTC: Boolean): TDateTime; static;

    function GetProvider: TcxSchedulerWebServiceStorageGoogleProvider; inline;
  protected
    procedure DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent); override;
    procedure DoAssignTo(AEvent: TcxSchedulerWebServiceEvent); override;

    property Provider: TcxSchedulerWebServiceStorageGoogleProvider read GetProvider;
  public
    procedure AssignFrom(const AObject: TdxJSONValue); override;
    function CreateJSONObject: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask }

  TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask = class(TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FGetEventListEndPoint = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?showDeleted=false';
    FGetEventListByPagesEndPoint = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?showDeleted=false&pageToken=%s';
    FGetEventListBetweenDatesEndPoint = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?showDeleted=false&timeMax=%s&timeMin=%s';
    FGetEventListBetweenDatesByPagesEndPoint = 'https://www.googleapis.com/calendar/v3/calendars/%s/events?showDeleted=false&timeMax=%s&timeMin=%s&pageToken=%s';
  {$ENDREGION}
  strict private
    FNextPageToken: string;
    function GetProvider: TcxSchedulerWebServiceStorageGoogleProvider; inline;
  protected
    function GetJSONItemList: TdxJSONObject; override;
    procedure Initialize; override;
    function IsFinished: Boolean; override;
    procedure ProcessItemListValues(AList: TdxJSONObject); override;

    property Provider: TcxSchedulerWebServiceStorageGoogleProvider read GetProvider;
    property NextPageToken: string read FNextPageToken;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderModifyEventTask }

  TcxSchedulerWebServiceStorageGoogleProviderModifyEventTask = class(TcxSchedulerWebServiceStorageProviderModifyEventCustomTask)
  protected
    function Process: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderDismissReminderTask }

  TcxSchedulerWebServiceStorageGoogleProviderDismissReminderTask = class(TcxSchedulerWebServiceStorageProviderModifyEventCustomTask)
  protected
    function Process: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderDeleteEventTask }

  TcxSchedulerWebServiceStorageGoogleProviderDeleteEventTask = class(TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask)
  protected
    function Process: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderUpdateEventTask }

  TcxSchedulerWebServiceStorageGoogleProviderUpdateEventTask = class(TcxSchedulerWebServiceStorageProviderUpdateEventCustomTask)
  protected
    function Process: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProviderPostEventTask }

  TcxSchedulerWebServiceStorageGoogleProviderPostEventTask = class(TcxSchedulerWebServiceStorageProviderPostEventCustomTask)
  strict private const
  {$REGION 'strict private const'}
    FPostEventObjectName = '/calendar/v3/calendars/%s/events';
    FGetOccurrencesEndPoint = 'https://www.googleapis.com/calendar/v3/calendars/%s/events/%s/instances?originalStart=%s';
  {$ENDREGION}
  strict private
    function GetOccurrenceCloudID(const AEvent: IdxWebServiceEvent): string;
  protected
    procedure DoApplyChanges; override;
    function Process: TdxJSONObject; override;
  end;

  { TcxSchedulerWebServiceStorageGoogleProvider }

  TcxSchedulerWebServiceStorageGoogleProvider = class(TcxSchedulerWebServiceStorageOAuth2CustomProvider)
  protected const
  {$REGION 'protected const'}
    ServerName = 'www.googleapis.com';
    UpdateEventObjectName = '/calendar/v3/calendars/%s/events/%s';
  {$ENDREGION}
  strict private const
  {$REGION 'strict private const'}
    FGetColorsEndPoint = 'https://www.googleapis.com/calendar/v3/colors';
    FGetCalendarListEndPoint = 'https://www.googleapis.com/calendar/v3/users/me/calendarList';
  {$ENDREGION}
  strict private
    FColors: TDictionary<Integer, Integer>;

    procedure PopulateColors;

    function GetAuthorizationAgent: TdxGoogleAPIOAuth2AuthorizationAgent;
    procedure SetAuthorizationAgent(const Value: TdxGoogleAPIOAuth2AuthorizationAgent);
  protected
    procedure ConnectedChanged; override;

    function GetWebServiceEventClass: TdxWebServiceEventClass; override;

    function CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;
    function CreateDismissEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
    function CreateModifyEventTask(AEvent: TcxSchedulerWebServiceEvent;
      const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;
    function CreatePostEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;
    function CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask; override;
    function CreateUpdateEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; override;

    class function IsItemListValid(AList: TdxJSONObject): Boolean; override;
    class function IsItemValid(AItem: TdxJSONValue): Boolean; override;
    procedure DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime); override;
    class function GetJSONItemArrayParamName: string; override;
    procedure PopulateCalendarList(const AList: TdxWebServiceCalendarList); override;
    procedure UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent); override;

  {$REGION 'IdxOAuth2AuthorizationAgentScopeRequestor'}
    function GetScopes: TStringList; override;
  {$ENDREGION}

    property Colors: TDictionary<Integer, Integer> read FColors;
  public
    constructor Create(AOwner: TcxSchedulerWebServiceStorage); override;
    destructor Destroy; override;

    class function GetDisplayName: string; override;
  published
    property AuthorizationAgent: TdxGoogleAPIOAuth2AuthorizationAgent read GetAuthorizationAgent write SetAuthorizationAgent;
  end;

implementation

uses
  Controls, DateUtils, cxDateUtils, dxCoreGraphics,
  dxUriRecord, cxSchedulerStorage, cxSchedulerRecurrence, cxSchedulerICalendar;

type
  TcxSchedulerEventRecurrenceInfoAccess = class(TcxSchedulerEventRecurrenceInfo);
  TcxSchedulerWebServiceEventAccess = class(TcxSchedulerWebServiceEvent);

  TcxSchedulerICalendarRecurrenceProperty = class(TcxSchedulerICalendarEventRecurrenceProperty)
  strict private
    FEvent: TcxSchedulerWebServiceEvent;
  protected
    function GetEvent: TcxSchedulerEvent; override;
  public
    constructor Create(AEvent: TcxSchedulerWebServiceEvent); reintroduce;

    class procedure ApplyRRULE(AEvent: TcxSchedulerWebServiceEvent; const RRULE: string); static;
    class function GetRRULE(AEvent: TcxSchedulerWebServiceEvent): string; static;
  end;

{ TcxSchedulerICalendarRecurrenceProperty }

constructor TcxSchedulerICalendarRecurrenceProperty.Create(AEvent: TcxSchedulerWebServiceEvent);
begin
  inherited Create(nil, nil);
  FEvent := AEvent;
end;

function TcxSchedulerICalendarRecurrenceProperty.GetEvent: TcxSchedulerEvent;
begin
  Result := FEvent;
end;

class procedure TcxSchedulerICalendarRecurrenceProperty.ApplyRRULE(AEvent: TcxSchedulerWebServiceEvent; const RRULE: string);
var
  AInfo: TcxSchedulerEventRecurrenceInfo;
begin
  with TcxSchedulerICalendarRecurrenceProperty.Create(AEvent) do
  try
    TcxSchedulerEventRecurrenceInfoAccess(RecurrenceInfo).AssignDefaultValues;
    SetValue(RRULE);
    AInfo := Event.RecurrenceInfo;
    if (AInfo.Recurrence = cxreDaily) and (AInfo.Periodicity > 0) then
      AInfo.DayType := cxdtEveryDay;
  finally
    Free;
  end;
end;

class function TcxSchedulerICalendarRecurrenceProperty.GetRRULE(AEvent: TcxSchedulerWebServiceEvent): string;
begin
  with TcxSchedulerICalendarRecurrenceProperty.Create(AEvent) do
  try
    Result := GetValue;
  finally
    Free;
  end;
end;

{ TdxGoogleCalendarEvent }

procedure TdxGoogleCalendarEvent.AssignFrom(const AObject: TdxJSONValue);
const
  AStateMap: array[Boolean] of Integer = (0, 2);

  function GetRecurrence: string;
  var
    AValue: TdxJSONValue;
  begin
    AValue := AObject.Get('recurrence');
    if AValue.IsArray then
      Result := TdxJSONArray(AValue).Items[0].Value
    else
      Result := '';
    if Copy(Result, 1, 6) = 'RRULE:' then
      Delete(Result, 1, 6);
  end;

  function GetReminders: TArray<Integer>;
  var
    AReminders: TdxJSONValue;
  begin
    Result := nil;
    AReminders := AObject.Get('reminders');
    if AReminders <> nil then
    begin
      if AReminders.GetParamValue('useDefault') = 'true' then
        Result := Provider.Calendar.DefaultReminders
      else
        Result := Self.GetReminders(AReminders.Get('overrides'));
    end;
  end;

var
  AColorId: Integer;
  AColor: Integer;
  AIsDatePart: Boolean;
  AIsUTC: Boolean;
begin
  FCaption := AObject.GetParamValue('summary');
  FId := AObject.GetParamValue('id');
  FStart := JSONToDateTime(AObject.Get('start'), AIsDatePart, FIsStartUTC);
  FIsAllDayEvent := AIsDatePart;
  FFinish := JSONToDateTime(AObject.Get('end'), AIsDatePart, FIsFinishUTC);
  FIsAllDayEvent := FIsAllDayEvent and AIsDatePart;
  FDescription := AObject.GetParamValue('description');
  FDescription := StringReplace(FDescription, #$A#$A, #$D#$A, [rfReplaceAll]);
  FIsRecurring := AObject.HasParam('recurrence');
  FCancelled := AObject.GetParamValue('status') = 'cancelled';
  FLocation := AObject.GetParamValue('location');
  FType := etNone;
  FShowAs := AStateMap[AObject.GetParamValue('transparency') <> 'transparent'];
  if TryStrToInt(AObject.GetParamValue('colorId'), AColorId) and
      Provider.Colors.TryGetValue(AColorId, AColor) then
    FColor := AColor
  else
    FColor := clNone;

  if IsRecurring then
  begin
    FRecurrence := GetRecurrence;
    FIsRecurring := FRecurrence <> '';
  end;
  if IsRecurring then
    FType := etPattern
  else
  begin
    FRecurringId := AObject.GetParamValue('recurringEventId');
    if RecurringId <> '' then
    begin
      FOriginalStartTime := JSONToDateTime(AObject.Get('originalStartTime'), AIsDatePart, AIsUTC);
      if IsCancelled then
        FType := etException
      else
        FType := etCustom;
    end;
  end;
  if not IsCancelled then
    FReminders := GetReminders;
end;

function TdxGoogleCalendarEvent.CreateJSONObject: TdxJSONObject;
var
  AReminders: TdxJSONObject;
  AReminder: TdxJSONObject;
  ADescription: string;
begin
  Result := TdxJSONObject.Create;
  Result.AddPair('summary', Caption);
  ADescription := StringReplace(Description, #$A, '\n', [rfReplaceAll]);
  ADescription := StringReplace(ADescription, #$D, '\r', [rfReplaceAll]);
  Result.AddPair('description', ADescription);
  Result.AddPair('end', DateTimeToJSON(Finish));
  Result.AddPair('start', DateTimeToJSON(Start));
  Result.AddPair('location', Location);
  if ShowAs = 0 then
    Result.AddPair('transparency', 'transparent')
  else
    Result.AddPair('transparency', 'opaque');
  if &Type = etException then
    Result.AddPair('status', 'cancelled')
  else
    Result.AddPair('status', 'confirmed');
  if &Type in [etCustom, etException] then
  begin
    Result.AddPair('recurringEventId', RecurringId);
    Result.AddPair('originalStartTime', DateTimeToJSON(OriginalStartTime));
  end
  else
  begin
    if &Type = etPattern then
      Result.AddPair('recurrence', TdxJSONArray.Create(TdxJSONString.Create(Format('RRULE:%s', [FRecurrence]))));
  end;
  if Provider.Owner.Reminders.Active then
  begin
    AReminders := TdxJSONObject.Create;
    AReminders.AddPair('useDefault', 'false');
    if Reminders <> nil then
    begin
      AReminder := TdxJSONObject.Create(TdxJSONPair.Create('minutes', IntToStr(Reminders[0])));
      AReminder.AddPair('method', 'popup');
      AReminders.AddPair('overrides', TdxJSONArray.Create(AReminder));
    end
    else
      AReminders.AddPair('overrides', TdxJSONArray.Create);
    Result.AddPair('reminders', AReminders);
  end;
end;

procedure TdxGoogleCalendarEvent.DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent);
begin
  inherited DoAssignFrom(AEvent);
  if AEvent.EventType = etPattern then
    FRecurrence := TcxSchedulerICalendarRecurrenceProperty.GetRRULE(AEvent);
end;

procedure TdxGoogleCalendarEvent.DoAssignTo(AEvent: TcxSchedulerWebServiceEvent);
begin
  inherited DoAssignTo(AEvent);
  if IsPattern then
    TcxSchedulerICalendarRecurrenceProperty.ApplyRRULE(AEvent, FRecurrence)
  else
    AEvent.EventType := &Type;
  if AEvent.EventType <> etException then
    AEvent.Reminder := Length(Reminders) > 0;
end;

class function TdxGoogleCalendarEvent.DateTimeToJSON(AValue: TDateTime): TdxJSONObject;
var
  ATranslateValue: string;
begin
  Result := TdxJSONObject.Create;
  ATranslateValue := TdxISO8601Helper.DateTimeToString(AValue);
  if dxDateOf(AValue) = AValue then
    Result.AddPair('date', ATranslateValue)
  else
  begin
    Result.AddPair('dateTime', ATranslateValue);
    Result.AddPair('timeZone', TdxISO8601Helper.UTCTimeZoneName);
  end;
end;

class function TdxGoogleCalendarEvent.JSONToDateTime(AJSONValue: TdxJSONValue; out AIsDatePart, AIsUTC: Boolean): TDateTime;
var
  AValue: string;
begin
  AValue := AJSONValue.GetParamValue('date');
  AIsDatePart := AValue <> '';
  if not AIsDatePart then
    AValue := AJSONValue.GetParamValue('dateTime');
  Result := TdxISO8601Helper.StringToDateTime(AValue, AIsUTC);
  AIsDatePart := AIsDatePart and not AIsUTC;
end;

class function TdxGoogleCalendarEvent.GetReminders(AOverrides: TdxJSONValue): TArray<Integer>;
var
  I: Integer;
  AValue: Integer;
  AArray: TdxJSONArray;
begin
  if (AOverrides = nil) or not AOverrides.IsArray then
    Exit(nil);
  AArray := TdxJSONArray(AOverrides);
  SetLength(Result, AArray.Count);
  for I := 0 to AArray.Count - 1 do
  begin
    if TryStrToInt(AArray.Items[I].GetParamValue('minutes'), AValue) then
      Result[I] := AValue
    else
      Result[I] := -1;
  end;
end;

function TdxGoogleCalendarEvent.GetProvider: TcxSchedulerWebServiceStorageGoogleProvider;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProvider(inherited Provider);
end;

{ TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask }

function TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.GetJSONItemList: TdxJSONObject;
begin
  if FNextPageToken = '' then
    Result := TdxHttpHelper.GetRequest(UserAgent,
      Format(FGetEventListEndPoint, [TdxURI.EscapeDataString(CalendarId)]), Header)
  else
    Result := TdxHttpHelper.GetRequest(UserAgent,
      Format(FGetEventListByPagesEndPoint, [TdxURI.EscapeDataString(CalendarId), NextPageToken]), Header);
end;

procedure TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.Initialize;
begin
  inherited Initialize;
  FNextPageToken := '';
end;

function TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.IsFinished: Boolean;
begin
  Result := inherited IsFinished or (FNextPageToken = '')
end;

procedure TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.ProcessItemListValues(AList: TdxJSONObject);
begin
  inherited ProcessItemListValues(AList);
  FNextPageToken := AList.GetParamValue('nextPageToken');
end;

function TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.GetProvider: TcxSchedulerWebServiceStorageGoogleProvider;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProvider(inherited Provider);
end;

{ TcxSchedulerWebServiceStorageGoogleProviderModifyEventTask }

function TcxSchedulerWebServiceStorageGoogleProviderModifyEventTask.Process: TdxJSONObject;
var
  AObject: TdxJSONObject;
  AObjectName: string;
begin
  if WebServiceEvent.GetId = '' then
    Exit(nil);
  AObject := ToJSONObject(WebServiceEvent);
  try
    AObjectName := Format(TcxSchedulerWebServiceStorageGoogleProvider.UpdateEventObjectName, [CalendarId, WebServiceEvent.GetId]);
    Result := TdxHttpHelper.PutRequest(UserAgent, TcxSchedulerWebServiceStorageGoogleProvider.ServerName, AObjectName, Header, AObject);
  finally
    AObject.Free;
  end;
end;

{ TcxSchedulerWebServiceStorageGoogleProviderDismissReminderTask }

function TcxSchedulerWebServiceStorageGoogleProviderDismissReminderTask.Process: TdxJSONObject;
var
  AObject: TdxJSONObject;
  AObjectName: string;
  AReminders: TdxJSONObject;
begin
  if WebServiceEvent.GetId = '' then
    Exit(nil);
  AObject := TdxJSONObject.Create;
  try
    AReminders := TdxJSONObject.Create;
    AReminders.AddPair('overrides', TdxJSONArray.Create);
    AReminders.AddPair('useDefault', 'false');
    AObject.AddPair('reminders', AReminders);
    AObjectName := Format(TcxSchedulerWebServiceStorageGoogleProvider.UpdateEventObjectName, [CalendarId, WebServiceEvent.GetId]);
    Result := TdxHttpHelper.PatchRequest(UserAgent, TcxSchedulerWebServiceStorageGoogleProvider.ServerName, AObjectName, Header, AObject);
  finally
    AObject.Free;
  end;
end;

{ TcxSchedulerWebServiceStorageGoogleProviderDeleteEventTask }

function TcxSchedulerWebServiceStorageGoogleProviderDeleteEventTask.Process: TdxJSONObject;
var
  AObjectName: string;
begin
  if WebServiceEvent.GetId = '' then
    Exit(nil);
  AObjectName := Format(TcxSchedulerWebServiceStorageGoogleProvider.UpdateEventObjectName, [CalendarId, WebServiceEvent.GetId]);
  Result := TdxHttpHelper.DeleteRequest(UserAgent, TcxSchedulerWebServiceStorageGoogleProvider.ServerName, AObjectName, Header, nil);
end;

{ TcxSchedulerWebServiceStorageGoogleProviderPostEventTask }

procedure TcxSchedulerWebServiceStorageGoogleProviderPostEventTask.DoApplyChanges;
begin
  if not WebServiceEvent.IsOccurrence then
    inherited DoApplyChanges;
end;

function TcxSchedulerWebServiceStorageGoogleProviderPostEventTask.Process: TdxJSONObject;
var
  AObject: TdxJSONObject;
  AObjectName: string;
  AItem: IdxWebServiceEvent;
begin
  if WebServiceEvent.IsOccurrence then
  begin
    AItem := WebServiceEvent;
    AItem.Id := GetOccurrenceCloudID(AItem);
    if AItem.Id <> '' then
      RunModifyEventTask(AItem);
    Result := nil;
    Exit;
  end
  else
  begin
    AObject := ToJSONObject(WebServiceEvent);
    try
      AObjectName := Format(FPostEventObjectName, [CalendarId]);
      Result := TdxHttpHelper.PostRequest(UserAgent, TcxSchedulerWebServiceStorageGoogleProvider.ServerName, AObjectName, Header, AObject);
    finally
      AObject.Free;
    end;
  end;
end;

function TcxSchedulerWebServiceStorageGoogleProviderPostEventTask.GetOccurrenceCloudID(const AEvent: IdxWebServiceEvent): string;
var
  AGetOccurrencesEndPoint: string;
  AObject: TdxJSONObject;
  AList: TdxWebServiceEventList;
  ASuccess: Boolean;
begin
  Result := '';
  AGetOccurrencesEndPoint := Format(FGetOccurrencesEndPoint, [CalendarId, AEvent.GetPatternId, TdxISO8601Helper.DateTimeToString(AEvent.GetOriginalOccurrenceStartTime)]);
  AObject := TdxHttpHelper.GetRequest(UserAgent, AGetOccurrencesEndPoint, Header);
  if Assigned(AObject) then
  try
    AList := JSONToEventList(AObject, ASuccess);
    try
      if ASuccess and (AList.Count = 1) then
        Result := AList[0].Id;
    finally
      AList.Free;
    end;
  finally
    AObject.Free;
  end;
end;

{ TcxSchedulerWebServiceStorageGoogleProviderUpdateEventTask }

function TcxSchedulerWebServiceStorageGoogleProviderUpdateEventTask.Process: TdxJSONObject;
var
  AObjectName: string;
begin
  if WebServiceEvent.Id = '' then
    Exit(nil);
  AObjectName := Format(TcxSchedulerWebServiceStorageGoogleProvider.UpdateEventObjectName, [CalendarId, WebServiceEvent.Id]);
  Result := TdxHttpHelper.GetRequest(UserAgent, TcxSchedulerWebServiceStorageGoogleProvider.ServerName, AObjectName, Header, nil);
end;

{ TcxSchedulerWebServiceStorageGoogleProvider }

constructor TcxSchedulerWebServiceStorageGoogleProvider.Create(AOwner: TcxSchedulerWebServiceStorage);
begin
  inherited Create(AOwner);
  FColors := TDictionary<Integer, Integer>.Create;
end;

destructor TcxSchedulerWebServiceStorageGoogleProvider.Destroy;
begin
  FColors.Free;
  inherited Destroy;
end;

class function TcxSchedulerWebServiceStorageGoogleProvider.GetDisplayName: string;
begin
  Result := 'Google Calendar';
end;

procedure TcxSchedulerWebServiceStorageGoogleProvider.ConnectedChanged;
begin
  inherited ConnectedChanged;
  if Connected then
    PopulateColors;
end;

function TcxSchedulerWebServiceStorageGoogleProvider.GetWebServiceEventClass: TdxWebServiceEventClass;
begin
  Result := TdxGoogleCalendarEvent;
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderDeleteEventTask.Create(AEvent, AWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreateDismissEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderDismissReminderTask.Create(AEvent);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreateModifyEventTask(
  AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderModifyEventTask.Create(AEvent, AWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreatePostEventTask(
  AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderPostEventTask.Create(AEvent);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderSyncEventsTask.Create(Self);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.CreateUpdateEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := TcxSchedulerWebServiceStorageGoogleProviderUpdateEventTask.Create(AEvent);
end;

function TcxSchedulerWebServiceStorageGoogleProvider.GetScopes: TStringList;
const
  AResultMap: array[Boolean] of string = ('https://www.googleapis.com/auth/calendar',
    'https://www.googleapis.com/auth/calendar.readonly');
begin
  Result := TStringList.Create;
  Result.Add(AResultMap[ReadOnly]);
end;

class function TcxSchedulerWebServiceStorageGoogleProvider.IsItemListValid(
  AList: TdxJSONObject): Boolean;
begin
  Result := AList.GetParamValue('kind') = 'calendar#events';
end;

class function TcxSchedulerWebServiceStorageGoogleProvider.IsItemValid(AItem: TdxJSONValue): Boolean;
begin
  Result := AItem.GetParamValue('kind') = 'calendar#event';
end;

procedure TcxSchedulerWebServiceStorageGoogleProvider.DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime);
begin
  inherited DismissEvent(AEvent, ADate);
  AddTask(CreateDismissEventTask(AEvent));
end;

class function TcxSchedulerWebServiceStorageGoogleProvider.GetJSONItemArrayParamName: string;
begin
  Result := 'items';
end;

procedure TcxSchedulerWebServiceStorageGoogleProvider.PopulateCalendarList(const AList: TdxWebServiceCalendarList);
var
  AJSONObject: TdxJSONObject;
  APair: TdxJSONPair;
  AArray: TdxJSONArray;
  AItem: TdxJSONValue;
  I: Integer;
  ACalendar: TdxWebServiceCalendar;
begin
  AuthorizationAgent.ValidateAccessToken;
  AJSONObject := GetJSONObject(FGetCalendarListEndPoint);
  if AJSONObject <> nil then
  try
    if AJSONObject.GetParamValue('kind') = 'calendar#calendarList' then
    begin
      APair := AJSONObject.GetPair('items');
      if (APair <> nil) and (APair.JsonValue is TdxJSONArray) then
      begin
        AArray := TdxJSONArray(APair.JsonValue);
        for I := 0 to AArray.Count - 1 do
        begin
          AItem := AArray.Items[I];
          if AItem.GetParamValue('kind') = 'calendar#calendarListEntry' then
          begin
            ACalendar := TdxWebServiceCalendar.CreateDefault;
            if AItem.HasParam('summaryOverride') then
              ACalendar.Name := AItem.GetParamValue('summaryOverride')
            else
              ACalendar.Name := AItem.GetParamValue('summary');
            ACalendar.Id := AItem.GetParamValue('id');
            ACalendar.TimeZone := AItem.GetParamValue('timeZone');
            ACalendar.EventColor := TdxAlphaColors.ToColor(TdxAlphaColors.FromHtml(AItem.GetParamValue('backgroundColor')));
            ACalendar.DefaultReminders := TdxGoogleCalendarEvent.GetReminders(AItem.Get('defaultReminders'));
            ACalendar.ReadOnly := AItem.GetParamValue('accessRole') = 'reader';
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

procedure TcxSchedulerWebServiceStorageGoogleProvider.UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent);
var
  ALink: TcxSchedulerWebServiceEvent;
begin
  ALink := TcxSchedulerWebServiceEvent(TcxSchedulerWebServiceEventAccess(AEvent).Link);
  while ALink <> nil do
  begin
    AddTask(CreateUpdateEventTask(ALink));
    ALink := TcxSchedulerWebServiceEvent(TcxSchedulerWebServiceEventAccess(ALink).Link);
  end;
end;

procedure TcxSchedulerWebServiceStorageGoogleProvider.PopulateColors;

  function JSONStringToColor(const Value: string): Integer;
  begin
    if Value[1] = '#' then
      Result := TdxAlphaColors.ToColor(TdxAlphaColors.FromHtml(Value))
    else
      if not TryStrToInt(Value, Result) then
        Result := -1;
  end;

var
  AJSONObject, AArray: TdxJSONObject;
  APair, AItem: TdxJSONPair;
  I: Integer;
  AColor: Integer;
begin
  if FColors.Count > 0 then
    Exit;
  AuthorizationAgent.ValidateAccessToken;

  AJSONObject := GetJSONObject(FGetColorsEndPoint);
  if AJSONObject <> nil then
  try
    if AJSONObject.GetParamValue('kind') = 'calendar#colors' then
    begin
      APair := AJSONObject.GetPair('event');
      if APair <> nil then
      begin
        AArray := TdxJSONObject(APair.JsonValue);
        for I := 0 to AArray.Count - 1 do
        begin
        {$IFDEF DELPHIXE6}
          AItem := AArray.Pairs[I];
        {$ELSE}
          AItem := AArray.Items[I];
        {$ENDIF}
          AColor := JSONStringToColor(AItem.JsonValue.GetParamValue('background'));
          if AColor <> -1 then
            FColors.Add(StrToInt(AItem.JsonString.Value), AColor);
        end;
      end;
    end
    else
      DoError(AJSONObject);
  finally
    AJSONObject.Free;
  end;
end;

function TcxSchedulerWebServiceStorageGoogleProvider.GetAuthorizationAgent: TdxGoogleAPIOAuth2AuthorizationAgent;
begin
  Result := TdxGoogleAPIOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

procedure TcxSchedulerWebServiceStorageGoogleProvider.SetAuthorizationAgent(const Value: TdxGoogleAPIOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

initialization
  TcxSchedulerWebServiceStorageGoogleProvider.Register;

finalization
  TcxSchedulerWebServiceStorageGoogleProvider.Unregister;

end.

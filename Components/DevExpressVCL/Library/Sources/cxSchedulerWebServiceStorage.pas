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

unit cxSchedulerWebServiceStorage;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI16}
  System.UITypes,
{$ENDIF}
  Types, SysUtils, Classes, Graphics, Generics.Collections, Generics.Defaults,
  WinInet, dxWinInet,
  dxCoreClasses, cxCustomData, cxClasses, dxThreading,
  cxSchedulerStorage, dxAuthorizationAgents;

type
  TcxSchedulerWebServiceEvent = class;
  TcxSchedulerWebServiceStorage = class;
  TcxSchedulerWebServiceStorageCustomProvider = class;
  TcxSchedulerWebServiceStorageResources = class;

  { TdxWebServiceCalendar }

  TdxWebServiceCalendar = record
  private
    FIsNull: Boolean;
  public
    ID: string;
    Name: string;
    TimeZone: string;
    BackgroundColor: TColor;
    EventColor: TColor;
    DefaultReminders: TArray<Integer>;
    ReadOnly: Boolean;
    function IsNull: Boolean;
    class function CreateDefault: TdxWebServiceCalendar; static;
    class function Null: TdxWebServiceCalendar; static;
  end;

  { TdxWebServiceCalendarList }

  TdxWebServiceCalendarList = class(TList<TdxWebServiceCalendar>)
  public
    function FindByID(const Id: string): TdxWebServiceCalendar;
    function FindByName(const AName: string): TdxWebServiceCalendar;
  end;

  { IdxWebServiceEvent }

  IdxWebServiceEvent = interface
    procedure AssignFrom(AEvent: TcxSchedulerWebServiceEvent); overload;
    procedure AssignFrom(const AObject: TdxJSONValue); overload;
    procedure AssignTo(AEvent: TcxSchedulerWebServiceEvent);
    function GetCalendarID: Variant;
    function GetID: Variant;
    function GetOriginalOccurrenceStartTime: TDateTime;
    function GetPatternID: Variant;
    function GetStartDate: TDateTime;
    function GetType: TcxEventType;
    procedure SetID(const Value: Variant);

    function CreateJSONObject: TdxJSONObject;

    function IsCancelled: Boolean;
    function IsOccurrence: Boolean;
    function IsPattern: Boolean;

    property ID: Variant read GetID write SetID;
    property StartDate: TDateTime read GetStartDate;
    property &Type: TcxEventType read GetType;
  end;

  { TdxWebServiceEventStartDateComparer }

  TdxWebServiceEventStartDateComparer = class(TInterfacedObject, IComparer<IdxWebServiceEvent>)
  strict private
    function Compare(const Left, Right: IdxWebServiceEvent): Integer;
  end;

  { TdxWebServiceCustomEvent }

  TdxWebServiceCustomEvent = class abstract(TInterfacedObject, IdxWebServiceEvent)
  strict private
    FProvider: TcxSchedulerWebServiceStorageCustomProvider;
    procedure SetId(const Value: Variant);
  protected
    FCalendarID: string;
    FCancelled: Boolean;
    FCaption: string;
    FColor: TColor;
    FDescription: string;
    FFinish: TDateTime;
    FID: string;
    FIsAllDayEvent: Boolean;
    FIsFinishUTC: Boolean;
    FIsRecurring: Boolean;
    FIsStartUTC: Boolean;
    FLocation: string;
    FOriginalStartTime: TDateTime;
    FRecurringID: string;
    FReminders: TArray<Integer>;
    FShowAs: Integer;
    FStart: TDateTime;
    FType: TcxEventType;

    procedure DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent); virtual;
    procedure DoAssignTo(AEvent: TcxSchedulerWebServiceEvent); virtual;
    property Provider: TcxSchedulerWebServiceStorageCustomProvider read FProvider;
  public
    constructor Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider);

    procedure AssignFrom(AEvent: TcxSchedulerWebServiceEvent); overload;
    procedure AssignFrom(const AObject: TdxJSONValue); overload; virtual;
    procedure AssignTo(AEvent: TcxSchedulerWebServiceEvent);
    function GetCalendarID: Variant;
    function GetID: Variant;
    function GetOriginalOccurrenceStartTime: TDateTime;
    function GetPatternID: Variant;
    function GetStartDate: TDateTime;
    function GetType: TcxEventType;

    function CreateJSONObject: TdxJSONObject; virtual;

    function IsCancelled: Boolean;
    function IsOccurrence: Boolean;
    function IsPattern: Boolean;

    property CalendarID: string read FCalendarID;
    property Cancelled: Boolean read FCancelled;
    property Caption: string read FCaption;
    property Color: TColor read FColor;
    property Description: string read FDescription;
    property Finish: TDateTime read FFinish;
    property ID: string read FID;
    property IsRecurring: Boolean read FIsRecurring;
    property Location: string read FLocation;
    property OriginalStartTime: TDateTime read FOriginalStartTime;
    property RecurringID: string read FRecurringID;
    property Reminders: TArray<Integer> read FReminders;
    property ShowAs: Integer read FShowAs;
    property Start: TDateTime read FStart;
    property &Type: TcxEventType read FType;
  end;
  TdxWebServiceEventClass = class of TdxWebServiceCustomEvent;
  TdxWebServiceEventDictionary = class(TDictionary<string, IdxWebServiceEvent>);
  TdxWebServiceEventList = class(TList<IdxWebServiceEvent>);

  { TcxSchedulerWebServiceEvent }

  TcxSchedulerWebServiceEvent = class(TcxSchedulerEvent)
  strict private
    FProvider: TcxSchedulerWebServiceStorageCustomProvider;
    function GetStorage: TcxSchedulerWebServiceStorage; inline;
    function GetProvider: TcxSchedulerWebServiceStorageCustomProvider; inline;
  protected
    FCloudID: string;
    FCloudOriginalStartDate: TDateTime;
    FCloudPatternID: string;
    function GetReadOnly: Boolean; override;

    procedure CalculateOriginalStartTime;
    function GetWebServiceEvent: IdxWebServiceEvent;
    procedure UpdateCloudPatternId;
    procedure UpdateRecurrenceIndex(APattern: TcxSchedulerWebServiceEvent); overload;
    procedure ValidatePattern;

    procedure InternalDeleteOccurrences;
    function IsOccurrence: Boolean;

    property CloudOriginalStartDate: TDateTime read FCloudOriginalStartDate;
    property Provider: TcxSchedulerWebServiceStorageCustomProvider read GetProvider;
  public
    procedure Assign(Source: TPersistent); override;
    procedure Delete; override;
    procedure DeleteExceptions; override;
    procedure UpdateRecurrenceIndex; overload;
    property CloudID: string read FCloudID;
    property CloudPatternID: string read FCloudPatternID;
    property Storage: TcxSchedulerWebServiceStorage read GetStorage;
  end;
  TdxWebServiceEventPatternList = class(TDictionary<string, TcxSchedulerWebServiceEvent>);

  { TcxSchedulerWebServiceStorageProviderCustomTask }

  TcxSchedulerWebServiceStorageProviderCustomTask = class abstract(TInterfacedObject, IdxTask)
  strict private
    FCalendarID: string;
    FHasError: Boolean;
    FHeader: string;
    FProvider: TcxSchedulerWebServiceStorageCustomProvider;
    FProviderLink: TcxObjectLink;
    FUserAgent: string;
    FCancelStatusCallback: TdxTaskCancelCallback;
    procedure SetHeader(const Value: string);
  protected
    procedure ApplyChanges;
    function CanApplyChanges: Boolean; virtual;
    procedure DoApplyChanges; virtual;
    function DoExecute: TdxTaskCompletedStatus; virtual;

    function CanProcessing: Boolean; virtual;
    procedure DoError(const AErrorObject: TObject);

    function IsAuthorizedValid: Boolean; virtual;

    procedure RunNewTask(const ATask: TcxSchedulerWebServiceStorageProviderCustomTask);

    procedure Initialize; virtual;
    procedure MainThreadSynchronize(AThreadProc: TThreadProcedure);

    property Provider: TcxSchedulerWebServiceStorageCustomProvider read FProvider;
    property Header: string read FHeader write SetHeader;
    property UserAgent: string read FUserAgent;

    property CalendarID: string read FCalendarID;
    property HasError: Boolean read FHasError;
  public
    constructor Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider); reintroduce;
    destructor Destroy; override;
    //IdxTask
    function Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
    procedure OnComplete(AStatus: TdxTaskCompletedStatus);
  end;

  { TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask }

  TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderCustomTask)
  strict private
    FList: TdxWebServiceEventDictionary;
  protected
    procedure DoApplyChanges; override;
    function DoExecute: TdxTaskCompletedStatus; override; final;

    function CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean; virtual;
    function GetItemArray(AList: TdxJSONObject): TdxJSONArray;
    function GetJSONItemList: TdxJSONObject; virtual;
    procedure Initialize; override;
    function IsItemListValid(AList: TdxJSONObject): Boolean;
    function IsItemValid(AItem: TdxJSONValue): Boolean;
    function IsFinished: Boolean; virtual;
    function ItemToEvent(AItem: TdxJSONValue): IdxWebServiceEvent;
    function ProcessEvent(const AEvent: IdxWebServiceEvent): TdxTaskCompletedStatus; virtual;

    procedure AfterProcess; virtual;
    procedure BeforeProcess; virtual;

    function Process: TdxTaskCompletedStatus; overload;
    function Process(AObject: TdxJSONObject): TdxTaskCompletedStatus; overload;
    procedure ProcessItemListValues(AList: TdxJSONObject); virtual;
    function ProcessItems(AItems: TdxJSONArray): TdxTaskCompletedStatus;

    property List: TdxWebServiceEventDictionary read FList;
  public
    destructor Destroy; override;
  end;

  { TcxSchedulerWebServiceStorageProviderEventCustomTask }

  TcxSchedulerWebServiceStorageProviderEventCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderCustomTask)
  strict private
    FWebServiceEvent: IdxWebServiceEvent;
    FEvent: TcxSchedulerWebServiceEvent;
    FModified: Boolean;
    FLink: TcxObjectLink;
  protected
    function CanApplyChanges: Boolean; override;
    function CanProcessing: Boolean; override;
    function DoExecute: TdxTaskCompletedStatus; override; final;
    function ExpectResponse: Boolean; virtual;

    procedure RunDeleteEventTask(const AEvent: IdxWebServiceEvent);
    procedure RunModifyEventTask(const AEvent: IdxWebServiceEvent);
    procedure RunUpdateEventTask;

    function Process: TdxJSONObject; virtual;

    function FromJSONObject(const AObject: TdxJSONValue): IdxWebServiceEvent;
    function JSONToEventList(const AObject: TdxJSONObject; var ASuccess: Boolean): TdxWebServiceEventList;
    function ToJSONObject: TdxJSONObject; overload;
    function ToJSONObject(const AEvent: IdxWebServiceEvent): TdxJSONObject; overload;

    property WebServiceEvent: IdxWebServiceEvent read FWebServiceEvent;
    property Event: TcxSchedulerWebServiceEvent read FEvent;
    property Link: TcxObjectLink read FLink;
    property Modified: Boolean read FModified;
  public
    constructor Create(AEvent: TcxSchedulerWebServiceEvent); overload;
    constructor Create(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent); overload;
    destructor Destroy; override;
  end;

  { TcxSchedulerWebServiceStorageProviderUpdateEventCustomTask }

  TcxSchedulerWebServiceStorageProviderUpdateEventCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderEventCustomTask)
  protected
    procedure DoApplyChanges; override;
  end;

  { TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask }

  TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderEventCustomTask)
  protected
    function CanProcessing: Boolean; override;
    procedure DoApplyChanges; override;
    function ExpectResponse: Boolean; override;
  end;

  { TcxSchedulerWebServiceStorageProviderModifyEventCustomTask }

  TcxSchedulerWebServiceStorageProviderModifyEventCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderEventCustomTask)
  protected
    procedure DoApplyChanges; override;
    procedure ModifyEvent; virtual;
  end;

  { TcxSchedulerWebServiceStorageProviderPostEventCustomTask }

  TcxSchedulerWebServiceStorageProviderPostEventCustomTask = class abstract(TcxSchedulerWebServiceStorageProviderModifyEventCustomTask);

  { TcxSchedulerWebServiceStorageCustomProvider }

  TcxSchedulerWebServiceStorageCustomProvider = class abstract(TInterfacedPersistent)
  strict private
    FAuthorizationAgent: TdxCustomAuthorizationAgent;
    FCalendar: TdxWebServiceCalendar;
    FCachedCalendarList: TdxWebServiceCalendarList;
    FConnected: Boolean;
    FFreeNotificator: TcxFreeNotificator;
    FOwner: TcxSchedulerWebServiceStorage;
    FReadOnly: Boolean;
    procedure FreeNotificationHandler(Sender: TComponent);
    function HasCalendarId: Boolean;
    function GetCalendarID: string;
    procedure SetAuthorizationAgent(const Value: TdxCustomAuthorizationAgent);
    procedure SetCalendarID(const Value: string);
    procedure SetConnected(const Value: Boolean);
    procedure SetReadOnly(const Value: Boolean);
  protected
    procedure AddTask(ATask: TcxSchedulerWebServiceStorageProviderCustomTask);
    function CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; overload;
    function CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; overload; virtual;
    function CreateModifyEventTask(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; overload; virtual;
    function CreateModifyEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; overload;
    function CreatePostEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; virtual;
    function CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask; virtual;
    function CreateUpdateEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask; virtual;

    function JSONToWebServiceEvent(AItem: TdxJSONValue): IdxWebServiceEvent;
    procedure PopulateCalendarList(const AList: TdxWebServiceCalendarList); virtual;

    class function GetJSONItemArray(AList: TdxJSONObject): TdxJSONArray;
    class function GetJSONItemArrayParamName: string; virtual;
    class function IsItemValid(AItem: TdxJSONValue): Boolean; virtual;
    class function IsItemListValid(AList: TdxJSONObject): Boolean; virtual;

    function IsAuthorizedValid: Boolean; virtual;

    function GetWebServiceEventClass: TdxWebServiceEventClass; virtual;
    function CreateWebServiceEvent: IdxWebServiceEvent; overload;
    function CreateWebServiceEvent(AEvent: TcxSchedulerWebServiceEvent): IdxWebServiceEvent; overload;

    function AddNewEvent(const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceEvent;
    procedure ClearEvents;
    function FindEvent(const ACloudId: string; out AEvent: TcxSchedulerWebServiceEvent): Boolean;
    procedure SynchronizeWebServiceEvents(AList: TdxWebServiceEventDictionary);
    procedure SynchronizeEvents; overload; virtual;
    procedure UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent); virtual;

    procedure DoError(const AErrorObject);

    class procedure Register;
    class procedure Unregister;

    procedure BeginSynchronize;
    procedure EndSynchronize;

    procedure SubscribeAuthorizationAgent; virtual;
    procedure UnsubscribeAuthorizationAgent; virtual;
    procedure Changed;
    procedure ConnectedChanged; virtual;
    procedure ReadOnlyChanged; virtual;

    function GetHeader: string; virtual;
    function GetJSONObject(const AUri: string): TdxJSONObject;
    function IsReady: Boolean; virtual;

    procedure DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime); virtual;

    procedure OccurrencesCheckedTimeBoundsChanged(AStart, AFinish: TDateTime); virtual;
  public
    constructor Create(AOwner: TcxSchedulerWebServiceStorage); virtual;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure BeforeDestruction; override;

    class function GetDisplayName: string; virtual;

    function GetCalendarList: TdxWebServiceCalendarList;

    property Calendar: TdxWebServiceCalendar read FCalendar;
    property Owner: TcxSchedulerWebServiceStorage read FOwner;
  published
    property AuthorizationAgent: TdxCustomAuthorizationAgent read FAuthorizationAgent write SetAuthorizationAgent;
    property CalendarID: string read GetCalendarID write SetCalendarID;
    property Connected: Boolean read FConnected write SetConnected default False;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly  default False;
  end;
  TcxSchedulerWebServiceStorageCustomProviderClass = class of TcxSchedulerWebServiceStorageCustomProvider;

  { TcxSchedulerWebServiceStorageOAuth2CustomProvider }

  TcxSchedulerWebServiceStorageOAuth2CustomProvider = class abstract(TcxSchedulerWebServiceStorageCustomProvider,
    IdxOAuth2AuthorizationAgentScopeRequestor)
  strict private
    function GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
    procedure SetAuthorizationAgent(const Value: TdxOAuth2AuthorizationAgent);
  protected
    procedure ConnectedChanged; override;
    function GetHeader: string; override;
    procedure ReadOnlyChanged; override;

    function IsAuthorizedValid: Boolean; override;

    procedure SubscribeAuthorizationAgent; override;
    procedure UnsubscribeAuthorizationAgent; override;
  {$REGION 'IdxOAuth2AuthorizationAgentScopeRequestor'}
    function GetScopes: TStringList; virtual;
  {$ENDREGION}
  public
    property AuthorizationAgent: TdxOAuth2AuthorizationAgent read GetAuthorizationAgent write SetAuthorizationAgent;
  end;

  { TcxSchedulerWebServiceStorageDataController }

  TcxSchedulerWebServiceStorageDataController = class(TcxSchedulerStorageDataController)
  strict private
    function GetStorage: TcxSchedulerWebServiceStorage; inline;
  protected
    property Storage: TcxSchedulerWebServiceStorage read GetStorage;
  end;

  { TcxSchedulerWebServiceStorageResourceItem }

  TcxSchedulerWebServiceStorageResourceItem = class(TcxSchedulerStorageResourceItem)
  strict private
    FEventColor: TColor;
    FProvider: TcxSchedulerWebServiceStorageCustomProvider;
    FProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass;

    procedure RecreateProvider;
    function GetProviderClassName: string; inline;
    function GetResources: TcxSchedulerWebServiceStorageResources; inline;
    procedure SetEventColor(const Value: TColor);
    procedure SetProvider(const Value: TcxSchedulerWebServiceStorageCustomProvider);
    procedure SetProviderClass(const Value: TcxSchedulerWebServiceStorageCustomProviderClass);
    procedure SetProviderClassName(const Value: string);
  protected
    function GetActualEventColor: TColor; override;
    function GetBackgroundColor: TColor; override;
    function GetReadOnly: Boolean; override;
    function GetResourceID: Variant; override;
    procedure SetReadOnly(const AValue: Boolean); override;
    procedure SetResourceID(const AValue: Variant); override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function CanModify: Boolean; override;
    property ProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass read FProviderClass write SetProviderClass;
    property Parent;
    property Resources: TcxSchedulerWebServiceStorageResources read GetResources;
  published
    property EventColor: TColor read FEventColor write SetEventColor default clDefault;
    property ReadOnly stored False;
    property ResourceID stored False;
    property ProviderClassName: string read GetProviderClassName write SetProviderClassName;
    property Provider: TcxSchedulerWebServiceStorageCustomProvider read FProvider write SetProvider;
  end;

  { TcxSchedulerWebServiceStorageResourceItems }

  TcxSchedulerWebServiceStorageResourceItems = class(TcxSchedulerStorageResourceItems)
  private
    function GetItem(Index: Integer): TcxSchedulerWebServiceStorageResourceItem; inline;
    function GetResources: TcxSchedulerWebServiceStorageResources; inline;
    function GetStorage: TcxSchedulerWebServiceStorage; inline;
    function GetVisibleResource(Index: Integer): TcxSchedulerWebServiceStorageResourceItem; inline;
    procedure SetItem(Index: Integer; const Value: TcxSchedulerWebServiceStorageResourceItem); inline;
  protected
    property Resources: TcxSchedulerWebServiceStorageResources read GetResources;
    property Storage: TcxSchedulerWebServiceStorage read GetStorage;
  public
    constructor Create(AOwner: TcxSchedulerWebServiceStorageResources); reintroduce;
    function Add: TcxSchedulerWebServiceStorageResourceItem;
    property Items[Index: Integer]: TcxSchedulerWebServiceStorageResourceItem read GetItem write SetItem; default;
    property VisibleResources[Index: Integer]: TcxSchedulerWebServiceStorageResourceItem read GetVisibleResource;
  end;

  { TcxSchedulerWebServiceStorageResources }

  TcxSchedulerWebServiceStorageResources = class(TcxSchedulerStorageResources)
  strict private
    function GetItems: TcxSchedulerWebServiceStorageResourceItems; inline;
    function GetStorage: TcxSchedulerWebServiceStorage; inline;
    procedure SetItems(const Value: TcxSchedulerWebServiceStorageResourceItems); inline;
  protected
    function CreateItems: TcxSchedulerStorageResourceItems; override;
    property Storage: TcxSchedulerWebServiceStorage read GetStorage;
  published
    property Items: TcxSchedulerWebServiceStorageResourceItems read GetItems write SetItems;
  end;

  { TcxSchedulerWebServiceReminder }

  TcxSchedulerWebServiceReminder = class(TcxSchedulerReminder)
  strict private
    function GetSchedulerWebServiceEvent: TcxSchedulerWebServiceEvent;
    function GetProvider: TcxSchedulerWebServiceStorageCustomProvider;
  protected
    procedure DismissEvent; override;
    procedure SnoozeEvent(const ASnoozeTime: TDateTime); override;

    property SchedulerWebServiceEvent: TcxSchedulerWebServiceEvent read GetSchedulerWebServiceEvent;
    property Provider: TcxSchedulerWebServiceStorageCustomProvider read GetProvider;
  end;

  { TcxSchedulerWebServiceReminders }

  TcxSchedulerWebServiceReminders = class(TcxSchedulerReminders)
  protected
    function GetReminderClass: TcxSchedulerReminderClass; override;
  end;

  { TcxSchedulerWebServiceStorage }

  TcxSchedulerWebServiceStorage = class(TcxCustomSchedulerStorage)
  public type
    TProviderErrorEvent = procedure(Sender: TObject; AProvider: TcxSchedulerWebServiceStorageCustomProvider; const AErrorObject) of object;
  private
    class var FRegisteredCalendarProviders: TcxRegisteredClasses;
    class function GetRegisteredCalendarProviders: TcxRegisteredClasses; static;
    class procedure Finalize; static;
  strict private
    FSynchronizeCount: Integer;
    FTasks: TdxTaskDispatcher;
    FOnProviderError: TProviderErrorEvent;
    function GetDataController: TcxSchedulerWebServiceStorageDataController; inline;
    function GetResources: TcxSchedulerWebServiceStorageResources; inline;
    procedure SetResources(const Value: TcxSchedulerWebServiceStorageResources);
  protected
    class procedure RegisterProvider(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass); static;
    class procedure UnregisterProvider(AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass); static;

    function CreateReminders: TcxSchedulerReminders; override;
    function CreateResources: TcxSchedulerStorageResources; override;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetEventClass: TcxSchedulerEventClass; override;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant; override;
    function GetStoreUsingGlobalTime: Boolean; override;
    procedure DoDeleteEvent(AEvent: TcxSchedulerEvent); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure SynchronizeEventsWithRecords; override;
    procedure PostEvent(AEvent: TcxSchedulerEvent); override;
    procedure PostEditingData(AEvent: TcxSchedulerEvent); override;
    procedure ModifyEvent(AEvent: TcxSchedulerEvent);

    procedure BeginSynchronize;
    procedure EndSynchronize;
    function IsSynchronizing: Boolean;

    procedure DoProviderError(AProvider: TcxSchedulerWebServiceStorageCustomProvider; const AErrorObject); virtual;

    procedure SynchronizeEvent(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent);
    procedure SynchronizeWebServiceEvents(AList: TdxWebServiceEventDictionary; AProvider: TcxSchedulerWebServiceStorageCustomProvider);
    procedure UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent);

    property DataController: TcxSchedulerWebServiceStorageDataController read GetDataController;
    property Tasks: TdxTaskDispatcher read FTasks;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function CreateProvider(AClass: TcxSchedulerWebServiceStorageCustomProviderClass): TcxSchedulerWebServiceStorageCustomProvider;
    function IsEventSharingAllowed: Boolean; override;
    function GetEvents(AList: TcxSchedulerFilteredEventList;
      const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean; overload; override;
    function IsActive: Boolean; override;
    property StoreUsingGlobalTime stored False;
    class property RegisteredCalendarProviders: TcxRegisteredClasses read GetRegisteredCalendarProviders;
  published
    property Holidays;
    property Reminders;
    property Resources: TcxSchedulerWebServiceStorageResources read GetResources write SetResources;
    property OnProviderError: TProviderErrorEvent read FOnProviderError write FOnProviderError;
  end;

implementation

uses
  Variants, RTLConsts, DateUtils,
  dxCore, dxUriRecord, cxVariants, cxDateUtils,
  cxSchedulerRecurrence, cxSchedulerUtils;

type
  TdxCustomAuthorizationAgentAccess = class(TdxCustomAuthorizationAgent);
  TcxSchedulerEventRecurrenceInfoAccess = class(TcxSchedulerEventRecurrenceInfo);

{ TdxWebServiceCalendar }

function TdxWebServiceCalendar.IsNull: Boolean;
begin
  Result := FIsNull;
end;

class function TdxWebServiceCalendar.CreateDefault: TdxWebServiceCalendar;
begin
  Result.ID := '';
  Result.Name := '';
  Result.TimeZone := '';
  Result.BackgroundColor := clDefault;
  Result.EventColor := clDefault;
  Result.DefaultReminders := nil;
  Result.ReadOnly := False;
  Result.FIsNull := False;
end;

class function TdxWebServiceCalendar.Null: TdxWebServiceCalendar;
begin
  Result := CreateDefault;
  Result.FIsNull := True;
end;

{ TdxWebServiceCalendarList }

function TdxWebServiceCalendarList.FindByID(const Id: string): TdxWebServiceCalendar;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].ID = Id then
      Exit(Items[I]);
  Result := TdxWebServiceCalendar.Null;
end;

function TdxWebServiceCalendarList.FindByName(const AName: string): TdxWebServiceCalendar;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    if Items[I].Name = AName then
      Exit(Items[I]);
  Result := TdxWebServiceCalendar.Null;
end;

{ TdxWebServiceEventStartDateComparer }

function TdxWebServiceEventStartDateComparer.Compare(const Left, Right: IdxWebServiceEvent): Integer;
begin
  Result := Trunc((Left.StartDate - Right.StartDate) * 24 * 60 * 60);
end;

{ TdxWebServiceCustomEvent }

constructor TdxWebServiceCustomEvent.Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider);
begin
  inherited Create;
  FProvider := AProvider;
end;

function TdxWebServiceCustomEvent.CreateJSONObject: TdxJSONObject;
begin
  dxAbstractError;
  Result := nil;
end;

procedure TdxWebServiceCustomEvent.AssignFrom(AEvent: TcxSchedulerWebServiceEvent);
begin
  FCaption := AEvent.Caption;
  FCalendarID := Provider.CalendarID;
  FID := AEvent.FCloudID;
  FRecurringID := AEvent.FCloudPatternID;
  FOriginalStartTime := AEvent.FCloudOriginalStartDate;
  if AEvent.AllDayEvent then
  begin
    FStart := AEvent.Start;
    FFinish := AEvent.Finish;
  end
  else
  begin
    FStart := AEvent.UTCStart;
    FFinish := AEvent.UTCFinish;
  end;
  FShowAs := AEvent.State;
  FLocation := AEvent.Location;
  FDescription := AEvent.Message;
  FIsAllDayEvent := AEvent.AllDayEvent;
  FType := AEvent.EventType;
  if FColor <> clNone then
    FColor := AEvent.LabelColor;
  if (&Type <> etException) and AEvent.Reminder then
    FReminders := TArray<Integer>.Create(AEvent.ReminderMinutesBeforeStart);
  DoAssignFrom(AEvent);
end;

procedure TdxWebServiceCustomEvent.AssignFrom(const AObject: TdxJSONValue);
begin
  dxAbstractError;
end;

procedure TdxWebServiceCustomEvent.AssignTo(AEvent: TcxSchedulerWebServiceEvent);
var
  I: Integer;
  AReminder: TDateTime;
  AReminders: TArray<Integer>;
begin
  AEvent.BeginEditing;
  try
    AEvent.Caption := Caption;
    AEvent.FCloudID := ID;
    AEvent.FCloudPatternID := RecurringID;
    AEvent.FCloudOriginalStartDate := OriginalStartTime;
    if &Type <> etException then
    begin
      AEvent.AllDayEvent := FIsAllDayEvent;
      if FIsStartUTC then
        AEvent.UTCStart := Start
      else
        AEvent.Start := Start;
      if FIsFinishUTC then
        AEvent.UTCFinish := Finish
      else
        AEvent.Finish := Finish;
      AEvent.State := ShowAs;
      AEvent.Location := Location;
      AEvent.Message := Description;
      if Color <> clNone then
        AEvent.LabelColor := Color;
      if Reminders <> nil then
      begin
        AReminders := Reminders;
        TArray.Sort<Integer>(AReminders);
        for I := Length(AReminders) - 1 to 0 do
        begin
          if AReminders[I] > 0 then
          begin
            AReminder := AEvent.Start - AReminders[I] / 24 / 60;
            if AReminder > Now then
              AEvent.ReminderMinutesBeforeStart := AReminders[I];
          end;
        end;
      end;
    end;
    DoAssignTo(AEvent);
    AEvent.EventType := &Type;
  finally
    AEvent.EndEditing;
  end;
end;

procedure TdxWebServiceCustomEvent.SetId(const Value: Variant);
begin
  FID := Value;
end;

procedure TdxWebServiceCustomEvent.DoAssignFrom(AEvent: TcxSchedulerWebServiceEvent);
begin
// do nothing
end;

procedure TdxWebServiceCustomEvent.DoAssignTo(AEvent: TcxSchedulerWebServiceEvent);
begin
// do nothing
end;

function TdxWebServiceCustomEvent.GetCalendarID: Variant;
begin
  Result := CalendarID;
end;

function TdxWebServiceCustomEvent.GetID: Variant;
begin
  Result := ID;
end;

function TdxWebServiceCustomEvent.GetOriginalOccurrenceStartTime: TDateTime;
begin
  Result := OriginalStartTime;
end;

function TdxWebServiceCustomEvent.GetPatternID: Variant;
begin
  Result := RecurringID;
end;

function TdxWebServiceCustomEvent.GetStartDate: TDateTime;
begin
  Result := Start;
end;

function TdxWebServiceCustomEvent.GetType: TcxEventType;
begin
  Result := FType;
end;

function TdxWebServiceCustomEvent.IsOccurrence: Boolean;
begin
  Result := &Type in [etCustom, etException];
end;

function TdxWebServiceCustomEvent.IsPattern: Boolean;
begin
  Result := &Type = etPattern;
end;

function TdxWebServiceCustomEvent.IsCancelled: Boolean;
begin
  Result := Cancelled;
end;

{ TcxSchedulerWebServiceStorageCustomProvider }

constructor TcxSchedulerWebServiceStorageCustomProvider.Create(AOwner: TcxSchedulerWebServiceStorage);
begin
  inherited Create;
  FOwner := AOwner;
  FCalendar := TdxWebServiceCalendar.CreateDefault;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := FreeNotificationHandler;
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateDeleteEventTask(
  AEvent: TcxSchedulerWebServiceEvent;
  const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  dxAbstractError;
  Result := nil;
end;

destructor TcxSchedulerWebServiceStorageCustomProvider.Destroy;
begin
  ClearEvents;
  cxClearObjectLinks(Self);
  FreeAndNil(FFreeNotificator);
  FreeAndNil(FCachedCalendarList);
  inherited Destroy;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.Assign(Source: TPersistent);
var
  ASource: TcxSchedulerWebServiceStorageCustomProvider;
begin
  if Source is TcxSchedulerWebServiceStorageCustomProvider then
  begin
    ASource := TcxSchedulerWebServiceStorageCustomProvider(Source);
    ReadOnly := ASource.ReadOnly;
    FCalendar := ASource.Calendar;
    Connected := ASource.Connected;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.BeforeDestruction;
begin
  Connected := False;
  AuthorizationAgent := nil;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.FreeNotificationHandler(Sender: TComponent);
begin
  if Sender = AuthorizationAgent then
    AuthorizationAgent := nil;
end;

function TcxSchedulerWebServiceStorageCustomProvider.HasCalendarId: Boolean;
begin
  Result := (CalendarID <> '');
end;

function TcxSchedulerWebServiceStorageCustomProvider.GetCalendarID: string;
begin
  Result := FCalendar.ID;
end;

class function TcxSchedulerWebServiceStorageCustomProvider.GetDisplayName: string;
begin
  Result := '';
end;

function TcxSchedulerWebServiceStorageCustomProvider.GetHeader: string;
begin
  dxAbstractError;
  Result := '';
end;

function TcxSchedulerWebServiceStorageCustomProvider.GetCalendarList: TdxWebServiceCalendarList;
begin
  Result := TdxWebServiceCalendarList.Create;
  if AuthorizationAgent = nil then
    Exit;
  if FCachedCalendarList <> nil then
  begin
    Result.AddRange(FCachedCalendarList);
    Exit;
  end;
  AuthorizationAgent.StartAuthorization;
  if not AuthorizationAgent.IsAuthorized then
    Exit;
  PopulateCalendarList(Result);
  FCachedCalendarList := TdxWebServiceCalendarList.Create;
  FCachedCalendarList.AddRange(Result);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SynchronizeEvents;
begin
  if not Connected then
    Exit;
  AddTask(CreateSyncEventsTask);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent);
begin
// do nothing
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SubscribeAuthorizationAgent;
begin
  if FAuthorizationAgent <> nil then
    cxAddFreeNotification(FFreeNotificator, FAuthorizationAgent);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.UnsubscribeAuthorizationAgent;
begin
  if FAuthorizationAgent <> nil then
    cxRemoveFreeNotification(FFreeNotificator, FAuthorizationAgent);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.Changed;
begin
  if not Owner.IsDestroying then
    Owner.Changed;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.ConnectedChanged;
var
  AList: TdxWebServiceCalendarList;
  AOldCalendarID: string;
begin
  if Connected then
  begin
    if FCalendar.IsNull then
    begin
      AOldCalendarID := CalendarID;
      AList := GetCalendarList;
      try
        FCalendar := AList.FindByID(CalendarID);
      finally
        AList.Free;
      end;
      if FCalendar.IsNull then
        FCalendar.ID := AOldCalendarID;
    end;
    SynchronizeEvents;
  end;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.ReadOnlyChanged;
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageCustomProvider.GetJSONObject(const AUri: string): TdxJSONObject;
begin
  Result := TdxHttpHelper.GetRequest(AuthorizationAgent.UserAgent, AUri, GetHeader);
end;

function TcxSchedulerWebServiceStorageCustomProvider.GetWebServiceEventClass: TdxWebServiceEventClass;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxSchedulerWebServiceStorageCustomProvider.IsAuthorizedValid: Boolean;
begin
  dxAbstractError;
  Result := False;
end;

class function TcxSchedulerWebServiceStorageCustomProvider.IsItemListValid(
  AList: TdxJSONObject): Boolean;
begin
  dxAbstractError;
  Result := False;
end;

class function TcxSchedulerWebServiceStorageCustomProvider.IsItemValid(
  AItem: TdxJSONValue): Boolean;
begin
  dxAbstractError;
  Result := False;
end;

function TcxSchedulerWebServiceStorageCustomProvider.IsReady: Boolean;
begin
  Result := (AuthorizationAgent <> nil) and TdxCustomAuthorizationAgentAccess(AuthorizationAgent).IsReady and HasCalendarId;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.DismissEvent(AEvent: TcxSchedulerWebServiceEvent; ADate: TDateTime);
begin
// do nothing
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.OccurrencesCheckedTimeBoundsChanged(AStart, AFinish: TDateTime);
begin
// do nothing
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.PopulateCalendarList(
  const AList: TdxWebServiceCalendarList);
begin
  dxAbstractError;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.AddTask(ATask: TcxSchedulerWebServiceStorageProviderCustomTask);
begin
  Owner.Tasks.Run(ATask);
end;

function TcxSchedulerWebServiceStorageCustomProvider.JSONToWebServiceEvent(AItem: TdxJSONValue): IdxWebServiceEvent;
begin
  Result := GetWebServiceEventClass.Create(Self);
  Result.AssignFrom(AItem);
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateDeleteEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := CreateDeleteEventTask(AEvent, AEvent.GetWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateModifyEventTask(AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  Result := CreateModifyEventTask(AEvent, AEvent.GetWebServiceEvent);
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreatePostEventTask(
  AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateSyncEventsTask: TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateUpdateEventTask(
  AEvent: TcxSchedulerWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  dxAbstractError;
  Result := nil;
end;

class function TcxSchedulerWebServiceStorageCustomProvider.GetJSONItemArray(AList: TdxJSONObject): TdxJSONArray;
var
  APair: TdxJSONPair;
begin
  APair := AList.GetPair(GetJSONItemArrayParamName);
  if (APair <> nil) and APair.JsonValue.IsArray then
    Result := TdxJSONArray(APair.JsonValue)
  else
    Result := nil;
end;

class function TcxSchedulerWebServiceStorageCustomProvider.GetJSONItemArrayParamName: string;
begin
  dxAbstractError;
  Result := '';
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateWebServiceEvent: IdxWebServiceEvent;
begin
  Result := GetWebServiceEventClass.Create(Self);
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateWebServiceEvent(AEvent: TcxSchedulerWebServiceEvent): IdxWebServiceEvent;
begin
  Result := CreateWebServiceEvent;
  Result.AssignFrom(AEvent);
end;

function TcxSchedulerWebServiceStorageCustomProvider.AddNewEvent(const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceEvent;
begin
  BeginSynchronize;
  try
    Result := TcxSchedulerWebServiceEvent(Owner.CreateEventInternal);
    Result.BeginEditing;
    try
      Result.AssignDefaultValues;
      AWebServiceEvent.AssignTo(Result);
      Result.ResourceID := CalendarID;
      Result.ID := Owner.GetEventID(Result);
    finally
      Result.EndEditing;
    end;
    Owner.SendNotification(Result, False, False);
  finally
    EndSynchronize;
  end;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.ClearEvents;
var
  AList: TdxWebServiceEventDictionary;
begin
  AList := TdxWebServiceEventDictionary.Create;
  try
    SynchronizeWebServiceEvents(AList);
  finally
    AList.Free;
  end;
end;

function TcxSchedulerWebServiceStorageCustomProvider.FindEvent(const ACloudId: string; out AEvent: TcxSchedulerWebServiceEvent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Owner.EventCount - 1 do
  begin
    AEvent := TcxSchedulerWebServiceEvent(Owner.Events[I]);
    Result := (AEvent.CloudID = ACloudId) and (AEvent.ResourceID = CalendarID);
    if Result then
      Break;
  end;
  if not Result then
    AEvent := nil;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SynchronizeWebServiceEvents(AList: TdxWebServiceEventDictionary);
begin
  if not Owner.IsDestroying then
    Owner.SynchronizeWebServiceEvents(AList, Self);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.DoError(const AErrorObject);
begin
  Owner.DoProviderError(Self, AErrorObject);
end;

class procedure TcxSchedulerWebServiceStorageCustomProvider.Register;
begin
  TcxSchedulerWebServiceStorage.RegisterProvider(Self);
end;

class procedure TcxSchedulerWebServiceStorageCustomProvider.Unregister;
begin
  TcxSchedulerWebServiceStorage.UnregisterProvider(Self);
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.BeginSynchronize;
begin
  Owner.BeginSynchronize;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.EndSynchronize;
begin
  Owner.EndSynchronize;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SetAuthorizationAgent(const Value: TdxCustomAuthorizationAgent);
begin
  if FAuthorizationAgent <> Value then
  begin
    Connected := False;
    ClearEvents;
    FreeAndNil(FCachedCalendarList);
    UnsubscribeAuthorizationAgent;
    FAuthorizationAgent := Value;
    SubscribeAuthorizationAgent;
  end;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SetConnected(
  const Value: Boolean);
var
  AOldConnected: Boolean;
begin
  if FConnected <> Value then
  begin
    AOldConnected := FConnected;
    FConnected := Value and IsReady;
    if Connected then
    begin
      AuthorizationAgent.StartAuthorization;
      FConnected := AuthorizationAgent.IsAuthorized;
    end;
    if AOldConnected <> FConnected then
    begin
      Changed;
      ConnectedChanged;
    end;
  end;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SetReadOnly(const Value: Boolean);
var
  AValue: Boolean;
begin
  AValue := Value or FCalendar.ReadOnly;
  if ReadOnly <> AValue then
  begin
    FReadOnly := AValue;
    ReadOnlyChanged;
    Changed;
  end;
end;

procedure TcxSchedulerWebServiceStorageCustomProvider.SetCalendarID(const Value: string);
var
  AList: TdxWebServiceCalendarList;
  AOldCalendarID: string;
begin
  if CalendarID <> Value then
  begin
    AOldCalendarID := CalendarID;
    AList := GetCalendarList;
    try
      FCalendar := AList.FindByID(Value);
      if FCalendar.IsNull then
        FCalendar.ID := Value;
    finally
      AList.Free;
    end;
    if CalendarID <> AOldCalendarID then
    begin
      Connected := False;
      ReadOnly := ReadOnly or FCalendar.ReadOnly;
      Changed;
    end;
  end;
end;

function TcxSchedulerWebServiceStorageCustomProvider.CreateModifyEventTask(
  AEvent: TcxSchedulerWebServiceEvent;
  const AWebServiceEvent: IdxWebServiceEvent): TcxSchedulerWebServiceStorageProviderCustomTask;
begin
  dxAbstractError;
  Result := nil;
end;

{ TcxSchedulerWebServiceStorageOAuth2CustomProvider }

procedure TcxSchedulerWebServiceStorageOAuth2CustomProvider.ConnectedChanged;
begin
  inherited ConnectedChanged;
  if AuthorizationAgent <> nil then
    AuthorizationAgent.RefreshScopes(Connected);
end;

procedure TcxSchedulerWebServiceStorageOAuth2CustomProvider.ReadOnlyChanged;
begin
  inherited ReadOnlyChanged;
  if AuthorizationAgent <> nil then
    AuthorizationAgent.RefreshScopes(Connected);
end;

function TcxSchedulerWebServiceStorageOAuth2CustomProvider.IsAuthorizedValid: Boolean;
begin
  AuthorizationAgent.ValidateAccessToken;
  Result := AuthorizationAgent.IsAccessTokenValid;
end;

procedure TcxSchedulerWebServiceStorageOAuth2CustomProvider.SubscribeAuthorizationAgent;
begin
  inherited SubscribeAuthorizationAgent;
  if AuthorizationAgent <> nil then
    AuthorizationAgent.RegisterScopeRequestor(Self);
end;

procedure TcxSchedulerWebServiceStorageOAuth2CustomProvider.UnsubscribeAuthorizationAgent;
begin
  if AuthorizationAgent <> nil then
    AuthorizationAgent.UnregisterScopeRequestor(Self);
  inherited UnsubscribeAuthorizationAgent;
end;

function TcxSchedulerWebServiceStorageOAuth2CustomProvider.GetHeader: string;
begin
  Result := TdxHttpHelper.ContentTypeJSONHeader + #13#10 + AuthorizationAgent.GetAuthorizationHeader;
end;

function TcxSchedulerWebServiceStorageOAuth2CustomProvider.GetScopes: TStringList;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxSchedulerWebServiceStorageOAuth2CustomProvider.GetAuthorizationAgent: TdxOAuth2AuthorizationAgent;
begin
  Result := TdxOAuth2AuthorizationAgent(inherited AuthorizationAgent);
end;

procedure TcxSchedulerWebServiceStorageOAuth2CustomProvider.SetAuthorizationAgent(const Value: TdxOAuth2AuthorizationAgent);
begin
  inherited AuthorizationAgent := Value;
end;

{ TcxSchedulerWebServiceEvent }

procedure TcxSchedulerWebServiceEvent.Assign(Source: TPersistent);
var
  ASource: TcxSchedulerWebServiceEvent;
begin
  if Source is TcxSchedulerWebServiceEvent then
  begin
    ASource := TcxSchedulerWebServiceEvent(Source);
    FCloudID := ASource.CloudID;
    FCloudPatternID := ASource.CloudPatternID;
    FCloudOriginalStartDate := ASource.FCloudOriginalStartDate;
  end;
  inherited Assign(Source);
end;

procedure TcxSchedulerWebServiceEvent.Delete;
var
  AIsModify: Boolean;
begin
  AIsModify := EventType = etCustom;
  inherited Delete;
  if AIsModify then
    Storage.ModifyEvent(Self);
end;

procedure TcxSchedulerWebServiceEvent.CalculateOriginalStartTime;
begin
  if not (EventType in [etCustom, etException]) then
    Exit;
  ValidatePattern;
  FCloudOriginalStartDate := GetOriginalDate + TimeBias;
end;

procedure TcxSchedulerWebServiceEvent.DeleteExceptions;
begin
// do nothing
end;

function TcxSchedulerWebServiceEvent.GetWebServiceEvent: IdxWebServiceEvent;
begin
  Result := Provider.CreateWebServiceEvent(Self);
end;

function TcxSchedulerWebServiceEvent.GetReadOnly: Boolean;
begin
  Result := inherited GetReadOnly or
    (Provider = nil) or not Provider.Connected;
end;

procedure TcxSchedulerWebServiceEvent.UpdateCloudPatternId;
var
  AEvent: TcxSchedulerWebServiceEvent;
begin
  if IsOccurrence then
  begin
    AEvent := TcxSchedulerWebServiceEvent(Storage.GetEventByID(ParentID));
    if AEvent <> nil then
      FCloudPatternID := AEvent.CloudID;
  end;
end;

procedure TcxSchedulerWebServiceEvent.ValidatePattern;
begin
  if not IsOccurrence then
    Exit;
  if Pattern = nil then
    FPattern := Storage.GetEventByID(ParentID);
end;

procedure TcxSchedulerWebServiceEvent.InternalDeleteOccurrences;
var
  ALink: TcxSchedulerEvent;
begin
  Storage.BeginUpdate;
  try
    ALink := Link;
    while ALink <> nil do
    begin
      Storage.DoDeleteEvent(ALink);
      ALink := TcxSchedulerWebServiceEvent(ALink).Link;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

function TcxSchedulerWebServiceEvent.IsOccurrence: Boolean;
begin
  Result := not VarIsNull(ParentID) and (EventType in [etCustom, etException]);
end;

procedure TcxSchedulerWebServiceEvent.UpdateRecurrenceIndex;
begin
  if not IsOccurrence then
    Exit;
  ValidatePattern;
  if Pattern <> nil then
    UpdateRecurrenceIndex(TcxSchedulerWebServiceEvent(Pattern));
end;

procedure TcxSchedulerWebServiceEvent.UpdateRecurrenceIndex(APattern: TcxSchedulerWebServiceEvent);
var
  ADate: TDate;
begin
  if not IsOccurrence then
    Exit;
  ADate := dxDateOf(CloudOriginalStartDate);
  with TcxSchedulerEventRecurrenceInfoAccess(RecurrenceInfo).GetCalculatorClass.Create(APattern, ADate, ADate + 1) do
  try
    if GetNextOccurrence then
    begin
      Self.RecurrenceIndex := Index;
      Exit;
    end;
  finally
    Free;
  end;
  RecurrenceIndex := -1;
end;

function TcxSchedulerWebServiceEvent.GetStorage: TcxSchedulerWebServiceStorage;
begin
  Result := TcxSchedulerWebServiceStorage(inherited Storage);
end;

function TcxSchedulerWebServiceEvent.GetProvider: TcxSchedulerWebServiceStorageCustomProvider;
var
  AResource: TcxSchedulerStorageResourceItem;
begin
  if FProvider <> nil then
    Exit(FProvider);
  AResource := GetResourceItem;
  if AResource <> nil then
    FProvider := TcxSchedulerWebServiceStorageResourceItem(AResource).Provider;
  Result := FProvider;
end;

{ TcxSchedulerWebServiceStorageProviderCustomTask }

constructor TcxSchedulerWebServiceStorageProviderCustomTask.Create(AProvider: TcxSchedulerWebServiceStorageCustomProvider);
begin
  inherited Create;
  FProvider := AProvider;
  FProviderLink := cxAddObjectLink(Provider);
  Initialize;
end;

destructor TcxSchedulerWebServiceStorageProviderCustomTask.Destroy;
begin
  cxRemoveObjectLink(FProviderLink);
  inherited Destroy;
end;

function TcxSchedulerWebServiceStorageProviderCustomTask.Run(const ACancelStatus: TdxTaskCancelCallback): TdxTaskCompletedStatus;
begin
  FCancelStatusCallback := ACancelStatus;
  if IsAuthorizedValid and CanProcessing then
  begin
    Result := DoExecute;
    if HasError then
      Result := TdxTaskCompletedStatus.Fail;
  end
  else
    Result := TdxTaskCompletedStatus.Fail;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.SetHeader(
  const Value: string);
begin
  FHeader := Value;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.OnComplete(AStatus: TdxTaskCompletedStatus);
begin
  if AStatus = TdxTaskCompletedStatus.Success then
    ApplyChanges;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.ApplyChanges;
begin
  if CanApplyChanges then
    DoApplyChanges;
end;

function TcxSchedulerWebServiceStorageProviderCustomTask.CanApplyChanges: Boolean;
begin
  Result := CanProcessing;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.Initialize;
begin
  FUserAgent := Provider.AuthorizationAgent.UserAgent;
  FHeader := Provider.GetHeader;
  FCalendarID := Provider.CalendarID;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.MainThreadSynchronize(AThreadProc: TThreadProcedure);
begin
  TThread.Synchronize(nil, AThreadProc);
end;

function TcxSchedulerWebServiceStorageProviderCustomTask.CanProcessing: Boolean;
begin
  Result := Assigned(FCancelStatusCallback) and not FCancelStatusCallback and
    (FProviderLink.Ref <> nil);
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.DoApplyChanges;
begin
  dxAbstractError;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.DoError(
  const AErrorObject: TObject);
begin
  FHasError := True;
  MainThreadSynchronize(
    procedure()
    begin
      if FProviderLink.Ref <> nil then
        Provider.DoError(AErrorObject);
    end);
end;

function TcxSchedulerWebServiceStorageProviderCustomTask.DoExecute: TdxTaskCompletedStatus;
begin
  dxAbstractError;
  Result := TdxTaskCompletedStatus.Fail;
end;

function TcxSchedulerWebServiceStorageProviderCustomTask.IsAuthorizedValid: Boolean;
var
  AResult: Boolean;
begin
  AResult := CanProcessing;
  if AResult then
    MainThreadSynchronize(
      procedure()
      begin
        AResult := (FProviderLink.Ref <> nil) and Provider.IsAuthorizedValid;
      end);
  Result := AResult;
end;

procedure TcxSchedulerWebServiceStorageProviderCustomTask.RunNewTask(
  const ATask: TcxSchedulerWebServiceStorageProviderCustomTask);
begin
  MainThreadSynchronize(
    procedure()
    begin
      if FProviderLink.Ref <> nil then
        Provider.AddTask(ATask);
    end);
end;

{ TcxSchedulerWebServiceStorageProviderEventCustomTask }

constructor TcxSchedulerWebServiceStorageProviderEventCustomTask.Create(AEvent: TcxSchedulerWebServiceEvent);
begin
  Create(AEvent, AEvent.GetWebServiceEvent);
end;

constructor TcxSchedulerWebServiceStorageProviderEventCustomTask.Create(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent);
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  AProvider := AEvent.Provider;
  inherited Create(AProvider);
  FEvent := AEvent;
  FWebServiceEvent := AWebServiceEvent;
  FLink := cxAddObjectLink(AEvent);
end;

destructor TcxSchedulerWebServiceStorageProviderEventCustomTask.Destroy;
begin
  cxRemoveObjectLink(FLink);
  inherited Destroy;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.CanApplyChanges: Boolean;
begin
  Result := inherited CanApplyChanges;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.CanProcessing: Boolean;
begin
  Result := inherited CanProcessing and (FLink.Ref <> nil);
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.DoExecute: TdxTaskCompletedStatus;
var
  AObject: TdxJSONObject;
begin
  AObject := Process;
  Result := TdxTaskCompletedStatus.Success;
  if Assigned(AObject) then
  try
    if ExpectResponse and Provider.IsItemValid(AObject) then
    begin
      FModified := True;
      FWebServiceEvent := FromJSONObject(AObject)
    end
    else
    begin
      DoError(AObject);
      Result := TdxTaskCompletedStatus.Fail;
    end;
  finally
    AObject.Free;
  end
  else
    if ExpectResponse then
      Result := TdxTaskCompletedStatus.Fail;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.ExpectResponse: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerWebServiceStorageProviderEventCustomTask.RunDeleteEventTask(const AEvent: IdxWebServiceEvent);
begin
  RunNewTask(Provider.CreateDeleteEventTask(Event, AEvent));
end;

procedure TcxSchedulerWebServiceStorageProviderEventCustomTask.RunModifyEventTask(const AEvent: IdxWebServiceEvent);
begin
  RunNewTask(Provider.CreateModifyEventTask(Event, AEvent));
end;

procedure TcxSchedulerWebServiceStorageProviderEventCustomTask.RunUpdateEventTask;
begin
  MainThreadSynchronize(
    procedure()
    begin
      Provider.AddTask(Provider.CreateUpdateEventTask(Event));
    end);
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.FromJSONObject(
  const AObject: TdxJSONValue): IdxWebServiceEvent;
var
  AResult: IdxWebServiceEvent;
begin
  MainThreadSynchronize(
    procedure()
    begin
      AResult := Provider.JSONToWebServiceEvent(AObject);
    end);
  Result := AResult;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.JSONToEventList(const AObject: TdxJSONObject; var ASuccess: Boolean): TdxWebServiceEventList;
var
  AItems: TdxJSONArray;
  AItem: TdxJSONValue;
  I: Integer;
  AEvent: IdxWebServiceEvent;
begin
  Result := TdxWebServiceEventList.Create;
  ASuccess := Provider.IsItemListValid(AObject);
  if ASuccess then
  begin
    AItems := Provider.GetJSONItemArray(AObject);
    ASuccess := AItems <> nil;
    if ASuccess then
    begin
      for I := 0 to AItems.Count - 1 do
      begin
        AItem := AItems.Items[I];
        ASuccess := Provider.IsItemValid(AItem);
        if not ASuccess then
          Break
        else
        begin
          AEvent := FromJSONObject(AItem);
          if not AEvent.IsOccurrence and AEvent.IsCancelled then
            Continue;
          Result.Add(AEvent);
        end;
      end;
    end;
  end;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.Process: TdxJSONObject;
begin
  dxAbstractError;
  Result := nil;
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.ToJSONObject: TdxJSONObject;
begin
  Result := ToJSONObject(WebServiceEvent);
end;

function TcxSchedulerWebServiceStorageProviderEventCustomTask.ToJSONObject(const AEvent: IdxWebServiceEvent): TdxJSONObject;
var
  AResult: TdxJSONObject;
  AWebServiceEvent: IdxWebServiceEvent;
begin
  AWebServiceEvent := AEvent;
  MainThreadSynchronize(
    procedure()
    begin
      AResult := AWebServiceEvent.CreateJSONObject;
    end);
  Result := AResult;
end;

{ TcxSchedulerWebServiceStorageProviderUpdateEventCustomTask }

procedure TcxSchedulerWebServiceStorageProviderUpdateEventCustomTask.DoApplyChanges;
begin
  if Modified and (WebServiceEvent.IsOccurrence or not WebServiceEvent.IsCancelled) then
    Provider.Owner.SynchronizeEvent(Event, WebServiceEvent)
  else
    if Link.Ref <> nil then
      Provider.Owner.DoDeleteEvent(Event);
end;

{ TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask }

function TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask.CanProcessing: Boolean;
begin
  Result := inherited CanProcessing or
    (not WebServiceEvent.IsOccurrence and (WebServiceEvent.GetID <> ''));
end;

procedure TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask.DoApplyChanges;
begin
  if Link.Ref <> nil then
    Provider.Owner.DoDeleteEvent(Event);
end;

function TcxSchedulerWebServiceStorageProviderDeleteEventCustomTask.ExpectResponse: Boolean;
begin
  Result := False;
end;

{ TcxSchedulerWebServiceStorageProviderModifyEventCustomTask }

procedure TcxSchedulerWebServiceStorageProviderModifyEventCustomTask.DoApplyChanges;
begin
  if Modified then
    ModifyEvent
  else
    Event.Cancel;
end;

procedure TcxSchedulerWebServiceStorageProviderModifyEventCustomTask.ModifyEvent;
begin
  Provider.Owner.SynchronizeEvent(Event, WebServiceEvent);
end;

{ TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask }

destructor TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.DoApplyChanges;
begin
  Provider.SynchronizeWebServiceEvents(FList);
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.DoExecute: TdxTaskCompletedStatus;
begin
  BeforeProcess;
  repeat
    if not IsAuthorizedValid then
      Exit(TdxTaskCompletedStatus.Fail);
    Result := Process;
  until (Result <> TdxTaskCompletedStatus.Success) or IsFinished;
  AfterProcess;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.CanAddEvent(const AEvent: IdxWebServiceEvent): Boolean;
begin
  Result := AEvent.IsOccurrence or not AEvent.IsCancelled;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.GetItemArray(AList: TdxJSONObject): TdxJSONArray;
begin
  Result := Provider.GetJSONItemArray(AList);
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.GetJSONItemList: TdxJSONObject;
begin
  dxAbstractError;
  Result := nil;
end;

procedure TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.Initialize;
begin
  inherited Initialize;
  FList := TdxWebServiceEventDictionary.Create;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.IsItemListValid(AList: TdxJSONObject): Boolean;
begin
  Result := CanProcessing and Provider.IsItemListValid(AList);
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.IsItemValid(AItem: TdxJSONValue): Boolean;
begin
  Result := CanProcessing and Provider.IsItemValid(AItem);
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.IsFinished: Boolean;
begin
  Result := not CanProcessing;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.ItemToEvent(AItem: TdxJSONValue): IdxWebServiceEvent;
var
  AResult: IdxWebServiceEvent;
begin
  MainThreadSynchronize(
    procedure()
    begin
      AResult := Provider.JSONToWebServiceEvent(AItem);
    end);
  Result := AResult;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.ProcessEvent(const AEvent: IdxWebServiceEvent): TdxTaskCompletedStatus;
begin
  FList.Add(AEvent.GetID, AEvent);
  Result := TdxTaskCompletedStatus.Success;
end;

procedure TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.AfterProcess;
begin
// do nothing
end;

procedure TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.BeforeProcess;
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.Process: TdxTaskCompletedStatus;
var
  AResultList: TdxJSONObject;
begin
  AResultList := GetJSONItemList;
  if AResultList <> nil then
  try
    Result := Process(AResultList);
  finally
    AResultList.Free;
  end
  else
    Result := TdxTaskCompletedStatus.Fail;
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.Process(AObject: TdxJSONObject): TdxTaskCompletedStatus;
var
  AItems: TdxJSONArray;
begin
  if IsItemListValid(AObject) then
  begin
    ProcessItemListValues(AObject);
    AItems := GetItemArray(AObject);
    Result := ProcessItems(AItems);
  end
  else
  begin
    DoError(AObject);
    Result := TdxTaskCompletedStatus.Fail;
  end;
end;

procedure TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.ProcessItemListValues(AList: TdxJSONObject);
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageProviderSyncEventsCustomTask.ProcessItems(AItems: TdxJSONArray): TdxTaskCompletedStatus;
var
  AItem: TdxJSONValue;
  I: Integer;
  AEvent: IdxWebServiceEvent;
begin
  if AItems <> nil then
  begin
    for I := 0 to AItems.Count - 1 do
    begin
      if not CanProcessing then
        Exit(TdxTaskCompletedStatus.Cancelled);
      AItem := AItems.Items[I];
      if IsItemValid(AItem) then
      begin
        AEvent := ItemToEvent(AItem);
        if not CanAddEvent(AEvent) then
          Continue;
        Result := ProcessEvent(AEvent);
        if Result <> TdxTaskCompletedStatus.Success then
          Exit;
      end;
    end;
  end;
  Result := TdxTaskCompletedStatus.Success;
end;

{ TcxSchedulerWebServiceStorageDataController }

function TcxSchedulerWebServiceStorageDataController.GetStorage: TcxSchedulerWebServiceStorage;
begin
  Result := TcxSchedulerWebServiceStorage(inherited Storage);
end;

{ TcxSchedulerWebServiceStorageResourceItem }

constructor TcxSchedulerWebServiceStorageResourceItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FEventColor := clDefault;
end;

destructor TcxSchedulerWebServiceStorageResourceItem.Destroy;
begin
  FreeAndNil(FProvider);
  inherited Destroy;
end;

procedure TcxSchedulerWebServiceStorageResourceItem.Assign(Source: TPersistent);
var
  ASource: TcxSchedulerWebServiceStorageResourceItem;
begin
  if Source is TcxSchedulerWebServiceStorageResourceItem then
  begin
    ASource := TcxSchedulerWebServiceStorageResourceItem(Source);
    ProviderClass := ASource.ProviderClass;
    Provider := ASource.Provider;
  end;
  inherited Assign(Source);
end;

function TcxSchedulerWebServiceStorageResourceItem.CanModify: Boolean;
begin
  Result := (Provider <> nil) and
    ((inherited CanModify and Provider.Connected) or Resources.Storage.IsSynchronizing);
end;

function TcxSchedulerWebServiceStorageResourceItem.GetActualEventColor: TColor;
begin
  Result := EventColor;
  if (Result = clDefault) and (Provider <> nil) then
    Result := Provider.Calendar.EventColor;
end;

function TcxSchedulerWebServiceStorageResourceItem.GetBackgroundColor: TColor;
begin
  Result := inherited GetBackgroundColor;
  if (Result = clDefault) and (Provider <> nil) then
    Result := Provider.Calendar.BackgroundColor;
end;

function TcxSchedulerWebServiceStorageResourceItem.GetReadOnly: Boolean;
begin
  Result := (Provider = nil) or Provider.ReadOnly;
end;

function TcxSchedulerWebServiceStorageResourceItem.GetResourceID: Variant;
begin
  if Provider <> nil then
    Result := Provider.CalendarID
  else
    Result := Null;
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetReadOnly(const AValue: Boolean);
begin
  if Provider <> nil then
    Provider.ReadOnly := AValue;
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetResourceID(const AValue: Variant);
begin
// do nothing
end;

function TcxSchedulerWebServiceStorageResourceItem.GetProviderClassName: string;
begin
  if Provider <> nil then
    Result := Provider.ClassName
  else
    Result := '';
end;

function TcxSchedulerWebServiceStorageResourceItem.GetResources: TcxSchedulerWebServiceStorageResources;
begin
  Result := TcxSchedulerWebServiceStorageResources(inherited Resources);
end;

procedure TcxSchedulerWebServiceStorageResourceItem.RecreateProvider;
begin
  FreeAndNil(FProvider);
  if FProviderClass <> nil then
    FProvider := FProviderClass.Create(Self.Resources.Storage);
  Changed(True);
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetEventColor(
  const Value: TColor);
begin
  if FEventColor <> Value then
  begin
    FEventColor := Value;
    Changed(True);
  end;
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetProvider(
  const Value: TcxSchedulerWebServiceStorageCustomProvider);
begin
  if (FProvider <> nil) and (Value <> nil) then
    FProvider.Assign(Value);
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetProviderClass(
  const Value: TcxSchedulerWebServiceStorageCustomProviderClass);
begin
  if FProviderClass <> Value then
  begin
    FProviderClass := Value;
    RecreateProvider;
  end;
end;

procedure TcxSchedulerWebServiceStorageResourceItem.SetProviderClassName(
  const Value: string);
begin
  if ProviderClassName <> Value then
    ProviderClass := TcxSchedulerWebServiceStorageCustomProviderClass(TcxSchedulerWebServiceStorage.RegisteredCalendarProviders.FindByClassName(Value));
end;

{ TcxSchedulerWebServiceStorageResourceItems }

constructor TcxSchedulerWebServiceStorageResourceItems.Create(
  AOwner: TcxSchedulerWebServiceStorageResources);
begin
  inherited Create(AOwner, TcxSchedulerWebServiceStorageResourceItem);
end;

function TcxSchedulerWebServiceStorageResourceItems.Add: TcxSchedulerWebServiceStorageResourceItem;
begin
  Result := TcxSchedulerWebServiceStorageResourceItem(inherited Add);
end;

function TcxSchedulerWebServiceStorageResourceItems.GetItem(
  Index: Integer): TcxSchedulerWebServiceStorageResourceItem;
begin
  Result := TcxSchedulerWebServiceStorageResourceItem(inherited Items[Index]);
end;

function TcxSchedulerWebServiceStorageResourceItems.GetResources: TcxSchedulerWebServiceStorageResources;
begin
  Result := TcxSchedulerWebServiceStorageResources(inherited Resources)
end;

function TcxSchedulerWebServiceStorageResourceItems.GetStorage: TcxSchedulerWebServiceStorage;
begin
  Result := TcxSchedulerWebServiceStorage(inherited Storage);
end;

function TcxSchedulerWebServiceStorageResourceItems.GetVisibleResource(
  Index: Integer): TcxSchedulerWebServiceStorageResourceItem;
begin
  Result := TcxSchedulerWebServiceStorageResourceItem(inherited VisibleResources[Index]);
end;

procedure TcxSchedulerWebServiceStorageResourceItems.SetItem(Index: Integer;
  const Value: TcxSchedulerWebServiceStorageResourceItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxSchedulerWebServiceStorageResources }

function TcxSchedulerWebServiceStorageResources.CreateItems: TcxSchedulerStorageResourceItems;
begin
  Result := TcxSchedulerWebServiceStorageResourceItems.Create(Self);
end;

function TcxSchedulerWebServiceStorageResources.GetItems: TcxSchedulerWebServiceStorageResourceItems;
begin
  Result := TcxSchedulerWebServiceStorageResourceItems(inherited Items);
end;

function TcxSchedulerWebServiceStorageResources.GetStorage: TcxSchedulerWebServiceStorage;
begin
  Result := TcxSchedulerWebServiceStorage(inherited Storage);
end;

procedure TcxSchedulerWebServiceStorageResources.SetItems(
  const Value: TcxSchedulerWebServiceStorageResourceItems);
begin
  inherited Items := Value;
end;

{ TcxSchedulerWebServiceReminder }

procedure TcxSchedulerWebServiceReminder.DismissEvent;
var
  ADate: TDateTime;
begin
  inherited DismissEvent;
  ADate := Event.ReminderDate;
  Provider.DismissEvent(SchedulerWebServiceEvent, ADate);
end;

procedure TcxSchedulerWebServiceReminder.SnoozeEvent(const ASnoozeTime: TDateTime);
begin
  inherited SnoozeEvent(ASnoozeTime);
end;

function TcxSchedulerWebServiceReminder.GetSchedulerWebServiceEvent: TcxSchedulerWebServiceEvent;
begin
  Result := TcxSchedulerWebServiceEvent(Event.Source);
end;

function TcxSchedulerWebServiceReminder.GetProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  Result := SchedulerWebServiceEvent.Provider;
end;

{ TcxSchedulerWebServiceReminders }

function TcxSchedulerWebServiceReminders.GetReminderClass: TcxSchedulerReminderClass;
begin
  Result := TcxSchedulerWebServiceReminder;
end;

{ TcxSchedulerWebServiceStorage }

class procedure TcxSchedulerWebServiceStorage.Finalize;
begin
  FreeAndNil(FRegisteredCalendarProviders);
end;

constructor TcxSchedulerWebServiceStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FTasks := TdxTaskDispatcher.Create;
  FTasks.MaxActiveTasks := 1;
  CalculateTimeBias;
end;

destructor TcxSchedulerWebServiceStorage.Destroy;
begin
  FreeAndNil(FTasks);
  inherited Destroy;
end;

function TcxSchedulerWebServiceStorage.CreateProvider(AClass: TcxSchedulerWebServiceStorageCustomProviderClass): TcxSchedulerWebServiceStorageCustomProvider;
begin
  Result := AClass.Create(Self);
end;

class function TcxSchedulerWebServiceStorage.GetRegisteredCalendarProviders: TcxRegisteredClasses;
begin
  if FRegisteredCalendarProviders = nil then
    FRegisteredCalendarProviders := TcxRegisteredClasses.Create;
  Result := FRegisteredCalendarProviders;
end;

function TcxSchedulerWebServiceStorage.GetResources: TcxSchedulerWebServiceStorageResources;
begin
  Result := TcxSchedulerWebServiceStorageResources(inherited Resources);
end;

procedure TcxSchedulerWebServiceStorage.DoDeleteEvent(AEvent: TcxSchedulerEvent);
var
  ASchedulerEvent: TcxSchedulerWebServiceEvent;
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  if not IsSynchronizing then
  begin
    AProvider := TcxSchedulerWebServiceEvent(AEvent).Provider;
    if AProvider <> nil then
    begin
      ASchedulerEvent := TcxSchedulerWebServiceEvent(AEvent);
      ASchedulerEvent.UpdateCloudPatternId;
      ASchedulerEvent.CalculateOriginalStartTime;
      Tasks.Run(AProvider.CreateDeleteEventTask(ASchedulerEvent));
    end;
  end;
  inherited DoDeleteEvent(AEvent)
end;

function TcxSchedulerWebServiceStorage.IsActive: Boolean;
begin
  Result := inherited IsActive and (ResourceCount > 0);
end;

class procedure TcxSchedulerWebServiceStorage.RegisterProvider(
  AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass);
begin
  RegisteredCalendarProviders.Register(AProviderClass, AProviderClass.GetDisplayName);
end;

class procedure TcxSchedulerWebServiceStorage.UnregisterProvider(
  AProviderClass: TcxSchedulerWebServiceStorageCustomProviderClass);
begin
  if FRegisteredCalendarProviders <> nil then
    FRegisteredCalendarProviders.Unregister(AProviderClass);
end;

function TcxSchedulerWebServiceStorage.CreateReminders: TcxSchedulerReminders;
begin
  Result := TcxSchedulerWebServiceReminders.Create(Self);
end;

function TcxSchedulerWebServiceStorage.CreateResources: TcxSchedulerStorageResources;
begin
  Result := TcxSchedulerWebServiceStorageResources.Create(Self);
end;

function TcxSchedulerWebServiceStorage.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxSchedulerWebServiceStorageDataController;
end;

procedure TcxSchedulerWebServiceStorage.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
end;

function TcxSchedulerWebServiceStorage.GetEventClass: TcxSchedulerEventClass;
begin
  Result := TcxSchedulerWebServiceEvent;
end;

function TcxSchedulerWebServiceStorage.GetEventID(
  AEvent: TcxSchedulerEvent): Variant;
var
  P: PByte;
begin
  Result := FEventIDCounter;
  P := PByte(AEvent.EditingRecordHandle);
  Inc(P);
  PInteger(P)^ := Result;
  Inc(FEventIDCounter);
end;

function TcxSchedulerWebServiceStorage.GetStoreUsingGlobalTime: Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerWebServiceStorage.SynchronizeEventsWithRecords;
var
  I: Integer;
begin
  inherited SynchronizeEventsWithRecords;
  for I := 0 to ResourceCount - 1 do
    if Resources.Items[I].Provider <> nil then
      Resources.Items[I].Provider.SynchronizeEvents;
end;

procedure TcxSchedulerWebServiceStorage.UpdateOccurrences(AEvent: TcxSchedulerWebServiceEvent);
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  AProvider := AEvent.Provider;
  if AProvider = nil then
    Exit;
  AProvider.UpdateOccurrences(AEvent);
end;

procedure TcxSchedulerWebServiceStorage.PostEvent(AEvent: TcxSchedulerEvent);
var
  ASchedulerEvent: TcxSchedulerWebServiceEvent;
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
begin
  if not IsSynchronizing then
  begin
    AProvider := TcxSchedulerWebServiceEvent(AEvent).Provider;
    if AProvider <> nil then
    begin
      ASchedulerEvent := TcxSchedulerWebServiceEvent(AEvent);
      ASchedulerEvent.UpdateCloudPatternId;
      ASchedulerEvent.CalculateOriginalStartTime;
      Tasks.Run(AProvider.CreatePostEventTask(ASchedulerEvent));
    end;
  end;
  BeginSynchronize;
  try
    inherited PostEvent(AEvent);
  finally
    EndSynchronize;
  end;
end;

procedure TcxSchedulerWebServiceStorage.SynchronizeEvent(AEvent: TcxSchedulerWebServiceEvent; const AWebServiceEvent: IdxWebServiceEvent);
begin
  BeginSynchronize;
  BeginUpdate;
  try
    AWebServiceEvent.AssignTo(AEvent);
    UpdateOccurrences(AEvent);
    AEvent.UpdateRecurrenceIndex;
  finally
    EndUpdate;
    EndSynchronize;
    SendNotification(nil, False, False);
  end;
end;

procedure TcxSchedulerWebServiceStorage.PostEditingData(AEvent: TcxSchedulerEvent);
begin
  ModifyEvent(AEvent);
  inherited PostEditingData(AEvent);
end;

procedure TcxSchedulerWebServiceStorage.ModifyEvent(AEvent: TcxSchedulerEvent);
var
  AProvider: TcxSchedulerWebServiceStorageCustomProvider;
  ASchedulerEvent: TcxSchedulerWebServiceEvent;
begin
  if not IsSynchronizing then
  begin
    AProvider := TcxSchedulerWebServiceEvent(AEvent).Provider;
    if AProvider <> nil then
    begin
      ASchedulerEvent := TcxSchedulerWebServiceEvent(AEvent);
      Tasks.Run(AProvider.CreateModifyEventTask(ASchedulerEvent));
    end;
  end;
end;

procedure TcxSchedulerWebServiceStorage.SynchronizeWebServiceEvents(AList: TdxWebServiceEventDictionary; AProvider: TcxSchedulerWebServiceStorageCustomProvider);
var
  ADeletedList: TdxFastObjectList;
  I: Integer;
  AEvent, APattern: TcxSchedulerWebServiceEvent;
  AWebServiceEvent: IdxWebServiceEvent;
  APatternList: TdxWebServiceEventPatternList;
  AOccurrenceList: TdxFastObjectList;
  AResourceID: Variant;
begin
  BeginSynchronize;
  BeginUpdate;
  try
    AResourceID := AProvider.CalendarID;
    AOccurrenceList := TdxFastObjectList.Create(False);
    APatternList := TdxWebServiceEventPatternList.Create;
    ADeletedList := TdxFastObjectList.Create(True);
    try
      ADeletedList.Assign(FEventsList.List);
      for I := 0 to FEventsList.Count - 1 do
      begin
        AEvent := TcxSchedulerWebServiceEvent(FEventsList[I]);
        if AEvent.ResourceID <> AResourceID then
        begin
          ADeletedList.Extract(AEvent);
          Continue;
        end;
        if not AList.TryGetValue(AEvent.CloudID, AWebServiceEvent) then
          Continue;
        ADeletedList.Extract(AEvent);
        if AWebServiceEvent.IsPattern then
          APatternList.Add(AEvent.CloudID, AEvent);
        if AWebServiceEvent.IsOccurrence then
          AOccurrenceList.Add(AEvent);
        AWebServiceEvent.AssignTo(AEvent);
        AList.Remove(AWebServiceEvent.GetID);
        if AList.Count = 0 then
          Break;
      end;
      if AList.Count > 0 then
        for AWebServiceEvent in AList.Values do
        begin
          AEvent := AProvider.AddNewEvent(AWebServiceEvent);
          if AWebServiceEvent.IsPattern then
            APatternList.Add(AEvent.CloudID, AEvent);
          if AWebServiceEvent.IsOccurrence then
            AOccurrenceList.Add(AEvent);
        end;
        for I := 0 to AOccurrenceList.Count - 1 do
        begin
          AEvent :=  TcxSchedulerWebServiceEvent(AOccurrenceList[I]);
          if APatternList.TryGetValue(AEvent.CloudPatternID, APattern) then
          begin
            AEvent.ParentID := APattern.ID;
            AEvent.UpdateRecurrenceIndex(APattern);
          end;
        end;
      for I := ADeletedList.Count - 1 downto 0 do
      begin
        AEvent := TcxSchedulerWebServiceEvent(ADeletedList[I]);
        if AEvent.ResourceID <> AResourceID then
          ADeletedList.Extract(AEvent)
        else
          SendNotification(AEvent, True, False);
      end;
    finally
      ADeletedList.Free;
      APatternList.Free;
      AOccurrenceList.Free;
    end;
  finally
    EndUpdate;
    EndSynchronize;
    SendNotification(nil);
  end;
end;

procedure TcxSchedulerWebServiceStorage.BeginSynchronize;
begin
  Inc(FSynchronizeCount);
end;

procedure TcxSchedulerWebServiceStorage.EndSynchronize;
begin
  Dec(FSynchronizeCount);
end;

function TcxSchedulerWebServiceStorage.IsEventSharingAllowed: Boolean;
begin
  Result := False;
end;

function TcxSchedulerWebServiceStorage.GetEvents(AList: TcxSchedulerFilteredEventList;
  const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean;
var
  I: Integer;
begin
  for I := 0 to ResourceCount - 1 do
    if (VarIsNull(AResourceID) or VarIsArray(AResourceID) or (ResourceIDs[I] = AResourceID)) and
        (Resources.Items[I].Provider <> nil) then
      Resources.Items[I].Provider.OccurrencesCheckedTimeBoundsChanged(AStart, AFinish);
  Result := inherited GetEvents(AList, AStart, AFinish, AResourceID);
end;

function TcxSchedulerWebServiceStorage.IsSynchronizing: Boolean;
begin
  Result := FSynchronizeCount > 0;
end;

procedure TcxSchedulerWebServiceStorage.DoProviderError(AProvider: TcxSchedulerWebServiceStorageCustomProvider; const AErrorObject);
begin
  if Assigned(FOnProviderError) then
    FOnProviderError(Self, AProvider, AErrorObject);
end;

function TcxSchedulerWebServiceStorage.GetDataController: TcxSchedulerWebServiceStorageDataController;
begin
  Result := TcxSchedulerWebServiceStorageDataController(inherited DataController);
end;

procedure TcxSchedulerWebServiceStorage.SetResources(
  const Value: TcxSchedulerWebServiceStorageResources);
begin
  inherited Resources := Value;
end;

initialization

finalization
  TcxSchedulerWebServiceStorage.Finalize;

end.


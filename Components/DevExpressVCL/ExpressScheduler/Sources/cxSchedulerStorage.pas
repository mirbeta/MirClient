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

unit cxSchedulerStorage;

{$I cxVer.inc}

interface

uses
  Variants, DateUtils, Classes, SysUtils, Windows, Forms, Math, Graphics, Contnrs,
  Types, ExtCtrls, Controls, ImgList, RTLConsts, Generics.Defaults, Generics.Collections,
  dxCore, dxCoreClasses, cxClasses, cxCustomData, cxGraphics, dxThreading,
  cxDataStorage, cxDateUtils, cxDataUtils, cxVariants, cxStorage, cxLookAndFeels,
  cxFormats, cxSchedulerUtils, cxSchedulerHolidays, cxDataConsts, cxSchedulerRecurrence, dxForms;

const
  // Options flag masks
  omAllDayEvent = $0001;
  omEnabled     = $0002;
  omReminder    = $0004;
  omCollapsed   = $0008;
  omGroup       = $0010;

  // Time line state
  tlsFree        = 0;
  tlsTentative   = 1;
  tlsBusy        = 2;
  tlsOutOfOffice = 3;

  cxDefaultEventDuration: TDateTime = 0.0034722222222;
  cxMaxDate = 767010; //Dec 31 3999

  cxInvalidRecordIndex = MinInt;
  ReminderRefreshInterval: Double = MinuteToTime / 2;

type
  TcxSchedulerEditingEventInfoList = class;
  TcxSchedulerEditingEventInfo = class;
  TcxSchedulerControlEventID = class;
  TcxCustomSchedulerStorage = class;

  TcxSchedulerStorage = class;
  TcxSchedulerEvent = class;
  TcxSchedulerEventRecurrenceInfo = class;
  TcxSchedulerControlEvent = class;
  TcxSchedulerCachedEventList = class;
  TcxSchedulerOccurrenceCalculator = class;
  TcxSchedulerEventList = class;
  TcxSchedulerFilteredEventList = class;
  TcxSchedulerStorageResources = class;
  TcxSchedulerStorageResourceItem = class;
  TcxCustomSchedulerStorageField = class;

  TcxCustomSchedulerStorageClass = class of TcxCustomSchedulerStorage;
  TcxSchedulerCachedEventListClass = class of TcxSchedulerCachedEventList;
  TcxSchedulerStorageResourcesClass = class of TcxSchedulerStorageResources;
  TcxSchedulerStorageResourceItemClass = class of TcxSchedulerStorageResourceItem;

  TcxSchedulerForEachEventHandlerProc = procedure(AEvent: TcxSchedulerEvent; AData: Pointer) of object;

  //reminders
  TcxSchedulerReminders = class;
  TcxSchedulerReminder = class;
  TcxSchedulerReminderClass = class of TcxSchedulerReminder;
  TcxSchedulerEventConflictsInfo = class;

  { TcxSchedulerEventEditInfo }

  TcxOnDeleteEventFunc = function (AEvent: TcxSchedulerControlEvent): Boolean of object;
  TcxOnAfterEditingProc = procedure (AEvent: TcxSchedulerControlEvent) of object;

  TcxSchedulerEventEditInfo = class
  public
    AllowDelete: Boolean;
    AllowHiddenEvents: Boolean;
    BiasTime: TDateTime;
    DialogsStyle: string;
    DisableShare: Boolean;
    Event: TcxSchedulerControlEvent;
    ForcePatternEditing: Boolean;
    Intersection: Boolean;
    IsRemindersActive: Boolean;
    LookAndFeel: TcxLookAndFeel;
    ReadOnly: Boolean;
    Recurrence: Boolean;
    RecurrenceButton: Boolean;
    ShowModal: Boolean;
    ShowResources: Boolean;
    ShowTaskComplete: Boolean;
    OnAfterEditingProc: TcxOnAfterEditingProc;
    OnDeleteFunc: TcxOnDeleteEventFunc;
  end;

  { IcxShedulerCustomEditorForm }

  IcxShedulerCustomEditorForm = interface
  ['{636671BA-B3E4-40F0-8100-F333AC526A45}']
    function GetModified: Boolean;
    procedure SetModified(AValue: Boolean);

    property Modified: Boolean read GetModified write SetModified;
  end;

  { IcxShedulerEventEditorForm }

  IcxSchedulerEventEditorForm = interface(IcxShedulerCustomEditorForm)
  ['{62D132ED-D331-4FB3-AB9E-6B16676148FF}']
    function GetForm: TForm;
    procedure Initialize(AEventInfo: TcxSchedulerEventEditInfo; AEditingInfo: TcxSchedulerEditingEventInfo);

    property Form: TForm read GetForm;
  end;

  TcxEventType = (etNone, etPattern, etOccurrence, etException, etCustom);

  TcxCompareEventsProc = function(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
  TcxGetRecurrenceDescriptionStringProc = function(
    ARecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
    AFullDescription: Boolean = False): string;

  TcxSchedulerReminderDueTimeElement = (dteMinute, dteHour, dteDay, dteWeek);

  TcxSchedulerReminderDueTimeKind = (dtkNow, dtkOverdue, dtkActual);

  TcxSchedulerReminderDueTimeInfo = record
    DueKind: TcxSchedulerReminderDueTimeKind;
    Element: TcxSchedulerReminderDueTimeElement;
    ElementValue: Integer;
    Minutes: Integer;
  end;

  TcxSchedulerReminderResource = packed record
    DismissDate: TDateTime;
    ResourceID: Variant;
    ReminderDate: TDateTime;
  end;

  TcxSchedulerReminderResources = array of TcxSchedulerReminderResource;

  TcxSchedulerReminderResourcesData = packed record
    Version: Byte;
    Resources: TcxSchedulerReminderResources;
  end;

  TcxDueTimeInfoToTextProc = function (
    const AInfo: TcxSchedulerReminderDueTimeInfo): string;

  { IcxSchedulerStorageListener }

  IcxSchedulerStorageListener = interface
  ['{87E0EBF3-F68A-4A51-8EA3-850D3819FBAB}']
    procedure StorageChanged(Sender: TObject);
    procedure StorageRemoved(Sender: TObject);
  end;

  IcxSchedulerStorageListener2 = interface(IcxSchedulerStorageListener)
  ['{4809FF3B-D9F8-4FD5-8647-33F8892599A4}']
    procedure AddEvent(AEvent: TcxSchedulerEvent);
    procedure RemoveEvent(AEvent: TcxSchedulerEvent);
  end;

  { IcxSchedulerSelectionAdapter }

  IcxSchedulerSelectionAdapter = interface
  ['{68B007E5-1057-40DE-BDA4-0D72F3780CC7}']
    procedure Add(AEvent: TcxSchedulerControlEvent; Shift: TShiftState);
    procedure Clear;
    function IsSelected(AEvent: TcxSchedulerControlEvent): Boolean;
    procedure Update;
  end;

  { TcxDoublyLinkedEventList }

  TcxDoublyLinkedEventListData = class(TcxDoublyLinkedData)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetNext: TcxDoublyLinkedEventListData;
    function GetPrev: TcxDoublyLinkedEventListData;
    procedure SetNext(const AValue: TcxDoublyLinkedEventListData);
    procedure SetPrev(const AValue: TcxDoublyLinkedEventListData);
  public
    property Event: TcxSchedulerEvent read GetEvent;
    property Next: TcxDoublyLinkedEventListData read GetNext write SetNext;
    property Prev: TcxDoublyLinkedEventListData read GetPrev write SetPrev;
  end;

  TcxDoublyLinkedEventList = class(TcxDoublyLinkedDataList)
  private
    function GetFirst: TcxDoublyLinkedEventListData;
    function GetLast: TcxDoublyLinkedEventListData;
  public
    function Add(AEvent: TcxSchedulerEvent): TcxDoublyLinkedEventListData; reintroduce;
    procedure FreeEvents; virtual;
    function GetLinkedObjectClass: TcxDoublyLinkedObjectClass; override;
    procedure ForEach(AProc: TcxSchedulerForEachEventHandlerProc; AData: Pointer; AGoForward: Boolean = True);

    property First: TcxDoublyLinkedEventListData read GetFirst;
    property Last: TcxDoublyLinkedEventListData read GetLast;
  end;

  { TcxSchedulerStorageDataController }

  TcxSchedulerStorageDataController = class(TcxCustomDataController)
  private
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    procedure ForEachEvent(AProc: TcxSchedulerForEachEventHandlerProc; AData: Pointer; AGoForward: Boolean);
    procedure InitializeEventFromRecordIndex(AEvent: TcxSchedulerEvent; AData: Pointer);
    procedure InitializeEventHandles;
    procedure InitializeRecordIndexes;
    procedure InitializeRecordIndexFromEvent(AEvent: TcxSchedulerEvent; AData: Pointer);
    // correct memory allocation when adding and removing fields
    procedure InsertValueDef(AField: TcxCustomSchedulerStorageField);
    procedure InsertValueDefProc(AEvent: TcxSchedulerEvent; AData: Pointer);
    procedure RemoveValueDef(AField: TcxCustomSchedulerStorageField);
    procedure RemoveValueDefProc(AEvent: TcxSchedulerEvent; AData: Pointer);
    //
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
    function UseRecordID: Boolean; override;

    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    procedure ChangeValueTypeClass(AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass); override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    function GetItemID(AItem: TObject): Integer; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;
  end;

  TcxSchedulerStorageDataControllerClass = class of TcxCustomDataController;

  { TcxCustomSchedulerStorageField }

  TcxCustomSchedulerStorageField = class(TCollectionItem)
  private
    FIndex: Integer;
    FIsUnique: Boolean;
    FName: string;
    FValueDef: TcxValueDef;
    function IsValueTypeStored: Boolean;
    function GetDataController: TcxSchedulerStorageDataController; inline;
    function GetStorage: TcxCustomSchedulerStorage; inline;
    function GetValue(AEvent: TcxSchedulerEvent): Variant;
    function GetValueCount: Integer;
    function GetValueDef: TcxValueDef; inline;
    function GetValueType: string;
    function GetValueTypeClass: TcxValueTypeClass;
    procedure SetName(const AValue: string);
    procedure SetValue(AEvent: TcxSchedulerEvent; const AValue: Variant);
    procedure SetValueType(const AValue: string);
    procedure SetValueTypeClass(AValue: TcxValueTypeClass); virtual;
  protected
    function GetDisplayName: string; override;
    function GetIsActive: Boolean; virtual;
    function GetIsBlob: Boolean; virtual;
    function GetIsTextStored: Boolean; virtual;

    property DataController: TcxSchedulerStorageDataController read GetDataController;
    property IsBlob: Boolean read GetIsBlob;
    property IsUnique: Boolean read FIsUnique;
    property Name: string read FName write SetName;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
    property ValueCount: Integer read GetValueCount;
    property ValueDef: TcxValueDef read GetValueDef;
    property Values[AEvent: TcxSchedulerEvent]: Variant read GetValue write SetValue;
    property ValueType: string read GetValueType write SetValueType stored IsValueTypeStored;
    property ValueTypeClass: TcxValueTypeClass read GetValueTypeClass write SetValueTypeClass;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property IsActive: Boolean read GetIsActive;
    property Index: Integer read FIndex;
  end;

  TcxSchedulerStorageFieldClass = class of TcxCustomSchedulerStorageField;

  { TcxSchedulerStorageField }

  TcxSchedulerStorageField = class(TcxCustomSchedulerStorageField)
  public
    procedure Assign(Source: TPersistent); override;

    property ValueCount;
    property Values;
    property ValueTypeClass;
  published
    property Index;
    property Name;
    property ValueType;
  end;

  { TcxCustomSchedulerStorageFields }

  TcxCustomSchedulerStorageFields = class(TCollection)
  private
    FOwner: TPersistent;
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    function FindFieldByName(const AName: string): TcxCustomSchedulerStorageField; virtual;
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  end;

  TcxSchedulerStorageFieldsClass = class of TcxCustomSchedulerStorageFields;

  { TcxSchedulerStorageFields }

  TcxSchedulerStorageFields = class(TcxCustomSchedulerStorageFields)
  private
    function GetItem(AIndex: Integer): TcxSchedulerStorageField;
    procedure SetItem(AIndex: Integer; AValue: TcxSchedulerStorageField);
  public
    function Add: TcxSchedulerStorageField;
    function ItemByName(const AName: string): TcxSchedulerStorageField;

    property Items[Index: Integer]: TcxSchedulerStorageField read GetItem write SetItem;
    property Storage;
  end;

  { TcxSchedulerEventRecurrenceInfo }

  TcxOccurrenceDateStatus = (odsValid, odsOccupied, odsIntersectRange);

  TcxSchedulerEventRecurrenceInfo = class(TcxSchedulerCustomRecurrenceInfo)
  private
    function GetEvent: TcxSchedulerEvent;
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    function GetFinish: TDateTime; override;
    function GetStart: TDateTime; override;
    procedure SetFinish(AValue: TDateTime); override;
    procedure SetStart(const AValue: TDateTime); override;

    function GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass; override;
    function GetData: TcxSchedulerCustomRecurrenceInfoData; override;
    function GetOccurrences(AList: TcxSchedulerFilteredEventList; const AStart, AFinish: TDateTime): Boolean;
    function GetValue(var AValue: AnsiString): Boolean; override;
    procedure SetDataItem(AOffset: Pointer; ASize: Integer; const AValue); override;
    procedure SetValue(const AValue: AnsiString); override;

    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    function GetValidStatus: TcxRecurrenceValidStatus;
    procedure Validate;

    property Event: TcxSchedulerEvent read GetEvent;
  end;

  TcxSchedulerEventRecurrenceInfoClass = class of TcxSchedulerEventRecurrenceInfo;

  { TcxSchedulerEventItemLink }

  TcxSchedulerEventRelation = (trFinishToStart, trStartToStart, trFinishToFinish, trStartToFinish);

  TcxSchedulerEventItemLink = class(TCollectionItem)
  private
    FIsDestroying: Boolean;
    FLink: TcxSchedulerEvent;
    FLinkRecurrenceIndex: Integer;
    FRelation: TcxSchedulerEventRelation;
    function GetEvent: TcxSchedulerEvent;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure SetLink(AValue: TcxSchedulerEvent);
    procedure SetRelation(AValue: TcxSchedulerEventRelation);
  protected
    function CheckLinked(AEvent: TcxSchedulerEvent): Boolean; overload;
    function CheckLinked(const ID: Variant; ARecurrenceIndex: Integer): Boolean; overload;
    function GetData: Variant; virtual;
    function IsAllowRelation(ARelation: TcxSchedulerEventRelation): Boolean;
    function IsValid: Boolean;
    procedure SetData(const AData: Variant); virtual;
    procedure UpdateLink; virtual;

    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    function GetRelationAsText: string;

    property Event: TcxSchedulerEvent read GetEvent;
    property Link: TcxSchedulerEvent read FLink write SetLink;
    property LinkRecurrenceIndex: Integer read FLinkRecurrenceIndex;
    property Relation: TcxSchedulerEventRelation read FRelation write SetRelation;
  end;

  { TcxSchedulerEventLinks }

  TcxSchedulerEventLinks = class(TCollection)
  private
    FEvent: TcxSchedulerEvent;
    function GetExpanded: Boolean;
    function GetItemLink(AIndex: Integer): TcxSchedulerEventItemLink;
    procedure SetExpanded(AValue: Boolean);
    procedure SetItemLink(AIndex: Integer; AValue: TcxSchedulerEventItemLink);
  protected
    function GetDisplayText: string; virtual;
    function GetOwner: TPersistent; override;
    procedure PrepareLinks; virtual;
    procedure RemoveLink(ALink: TcxSchedulerEvent); virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    function Add(AEvent: TcxSchedulerEvent; ARelation: TcxSchedulerEventRelation = trStartToFinish): TcxSchedulerEventItemLink; overload;
    function IsEventLinked(AEvent: TcxSchedulerEvent): Boolean;

    procedure UpdateLinksTime;

    property DisplayText: string read GetDisplayText;
    property Event: TcxSchedulerEvent read FEvent;
    property Expanded: Boolean read GetExpanded write SetExpanded;
    property ItemLinks[Index: Integer]: TcxSchedulerEventItemLink read GetItemLink write SetItemLink; default;
  end;

  { TcxSchedulerEventTimeCalculator }

  TcxSchedulerEventTimeCalculator = class(TPersistent)
  private
    FFinish: TDateTime;
    FEvent: TcxSchedulerEvent;
    FStart: TDateTime;
    function GetDuration: TDateTime;
    function GetStoredDuration: TDateTime;
    function GetStoredFinish: TDateTime;
    function GetStoredStart: TDateTime;
  protected
    procedure DoCalculate; virtual;
    function GetBaseFinish: TDateTime; virtual;
    function GetBaseStart: TDateTime; virtual;
    procedure GetValidTaskTimeRange(var AStart, AFinish: TDateTime); virtual;

    property StoredDuration: TDateTime read GetStoredDuration;
    property StoredFinish: TDateTime read GetStoredFinish;
    property StoredStart: TDateTime read GetStoredStart;
  public
    constructor Create(AEvent: TcxSchedulerEvent); virtual;

    procedure Calculate;

    property Duration: TDateTime read GetDuration;
    property Finish: TDateTime read FFinish;
    property Event: TcxSchedulerEvent read FEvent;
    property Start: TDateTime read FStart;
  end;

  TcxSchedulerEventTaskStatus = (tsNotStarted, tsInProgress, tsComplete, tsWaiting, tsDeferred);

  { TcxSchedulerEventGroupItems }

  TcxSchedulerEventGroupItems = class(TcxDoublyLinkedEventList)
  private
    FEvent: TcxSchedulerEvent;
    FExpanded: Boolean;
    function GetHasChildren: Boolean;
    function GetID: Variant;
    procedure SetExpanded(AValue: Boolean);
  protected
    procedure DeleteEvents;
  public
    constructor Create(AOwner: TcxSchedulerEvent); virtual;
    function IsChild(AEvent: TcxSchedulerEvent): Boolean;

    function Find(AEvent: TcxSchedulerEvent): TcxDoublyLinkedEventListData;
    procedure UpdateItemsTime;

    property Event: TcxSchedulerEvent read FEvent;
    property Expanded: Boolean read FExpanded write SetExpanded;
    property HasChildren: Boolean read GetHasChildren;
    property ID: Variant read GetID;
  end;

  TcxEventCalculatedInfo = packed record
    Options: Integer;
    ActualStart: Integer;
    ActualFinish: Integer;
    EventType: TcxEventType;
    Start, Finish, Duration: TDateTime;
    DayStart, DayFinish: Integer;
    State: Integer;
    ResourceID: Variant;
    ResourceIDHashTable: TdxVariantDictionary<Integer>;
    RecurrenceInfo: AnsiString;
    Visible: Boolean;
  end;

  { TcxSchedulerEvent }

  TcxSchedulerEvent = class(TPersistent)
  private
    FGroupItems: TcxSchedulerEventGroupItems;
    FID: Variant;
    FIsDataValid: Boolean;
    FIsModified: Boolean;
    FLink: TcxSchedulerEvent;
    FParentGroup: TcxSchedulerEvent;
    FSkipExceptions: Boolean;
    FStorage: TcxCustomSchedulerStorage;
    FPrevTaskComplete: Integer;
    FRecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
    function GetActualFinish: Integer;
    function GetActualStart: Integer;
    function GetActualLabelColor: Integer;
    function GetAllDayEvent: Boolean;
    function GetCaption: string;
    function GetDuration: TDateTime;
    function GetEditValue(AIndex: Integer): Variant;
    function GetEnabled: Boolean;
    function GetEventType: TcxEventType;
    function GetFinish: TDateTime;
    function GetGroupID: Variant;
    function GetIsBoundMode: Boolean;
    function GetIsEditing: Boolean;
    function GetIsGroup: Boolean;
    function GetIsNewEvent: Boolean;
    function GetLabelColor: Integer;
    function GetLocation: string;
    function Getmessage: string; {GetMessage conflicts with C++ macro}
    function GetOptionsFlag: Integer; inline;
    function GetRecurrenceIndex: Integer;
    function GetReminder: Boolean;
    function GetReminderDate: TDateTime;
    function GetReminderMinutesBeforeStart: Integer;
    function GetReminderResourcesData: TcxSchedulerReminderResourcesData;
    function GetResourceID: Variant;
    function GetResourceIDCount: Integer;
    function GetResourceIDs(Index: Integer): Variant;
    function GetShared: Boolean;
    function GetStart: TDateTime;
    function GetState: Integer;
    function GetTaskComplete: Integer;
    function GetTaskStatus: TcxSchedulerEventTaskStatus;
    function GetUTCFinish: TDateTime;
    function GetUTCStart: TDateTime;
    function GetValueCount: Integer;
    function GetValueDef(AFieldIndex: Integer): TcxValueDef;

    procedure InternalSetFinish(const AValue: TDateTime);
    procedure InternalSetStart(const AValue: TDateTime);
    procedure InternalSetTaskComplete(const AValue: Integer; AUpdateTaskStatus: Boolean = True);
    procedure InternalSetTaskStatus(AValue: TcxSchedulerEventTaskStatus; AUpdateTaskComplete: Boolean = True);
    procedure SetAllDayEvent(const AValue: Boolean);
    procedure SetCaption(const AValue: string);
    procedure SetDuration(const AValue: TDateTime);
    procedure SetEditValue(AIndex: Integer; const AValue: Variant);
    procedure SetEnabled(const AValue: Boolean);
    procedure SetEventType(AValue: TcxEventType);
    procedure SetFinish(const AValue: TDateTime);
    procedure SetGroupID(const AValue: Variant);
    procedure SetID(const AValue: Variant);
    procedure SetIsDataValid(Value: Boolean);
    procedure SetIsGroup(const AValue: Boolean);
    procedure SetLabelColor(const AValue: Integer);
    procedure SetLocation(const AValue: string);
    procedure SetMessage(const AValue: string);
    function SetOptionsFlag(const AMask: Integer; AValue: Boolean): Boolean;
    procedure SetParentGroup(AValue: TcxSchedulerEvent);
    procedure SetParentID(const AValue: Variant);
    procedure SetRecurrenceIndex(const AValue: Integer);
    procedure SetRecurrenceInfo(AValue: TcxSchedulerEventRecurrenceInfo);
    procedure SetReminder(const AValue: Boolean);
    procedure SetReminderDate(AValue: TDateTime);
    procedure SetReminderMinutesBeforeStart(const AValue: Integer);
    procedure SetReminderResourcesData(const AValue: TcxSchedulerReminderResourcesData);
    procedure SetResourceID(const AValue: Variant);
    procedure SetStart(const AValue: TDateTime);
    procedure SetState(const AValue: Integer);
    procedure SetTaskComplete(const AValue: Integer);
    procedure SetTaskIndex(const AValue: Integer);
    procedure SetTaskStatus(AValue: TcxSchedulerEventTaskStatus);
    procedure SetUTCFinish(const AValue: TDateTime);
    procedure SetUTCStart(const AValue: TDateTime);
  protected
    FData: Pointer;
    FEditCount: Integer;
    FEditingRecordHandle: Pointer;
    FPattern: TcxSchedulerEvent;
    FRecordHandle: Pointer;
    FInfo: TcxEventCalculatedInfo;
    FIndex: Integer;
    FIsDeletion: Boolean;
    FTaskLinks: TcxSchedulerEventLinks;
    FTaskLinkOwners: TcxSchedulerEventList;
    procedure AssignDefaultValues; virtual;
    procedure CalculateActualTimeRange;
    procedure CalculateActualTimeRangePost;
    function CanMoveTo(ANewTime: TDateTime): Boolean; virtual;
    procedure CheckCalculated; inline;
    procedure CheckLinksOnChangeEventType(ANewEventType: TcxEventType); virtual;
    procedure CheckRecurrenceLink(AEvent: TcxSchedulerEvent);
    procedure CheckRecurrenceLinkEx(AEvent: TcxSchedulerEvent);
    function CheckTimeRange(const AStartDate, AFinishDate: Integer): Boolean;
    function CreateGroupItems: TcxSchedulerEventGroupItems; virtual;
    function CreateRecurrenceInfo: TcxSchedulerEventRecurrenceInfo; virtual;
    function CreateReminderResourcesData: TcxSchedulerReminderResourcesData;
    procedure CreateSubClasses; virtual;
    function CreateTaskLinks: TcxSchedulerEventLinks; virtual;
    function CreateTaskLinkOwners: TcxSchedulerEventList; virtual;
    procedure DestroySubClasses; virtual;
    function GetDataController: TcxSchedulerStorageDataController;
    function GetID: Variant; virtual;
    function GetIndex: Integer; virtual;
    function GetIsFreeState: Boolean; virtual;
    function GetGroupItems: TcxSchedulerEventGroupItems; virtual;
    function GetOccurrenceByDate(const ADate: TDateTime): TcxSchedulerEvent;
    function GetOccurrenceByIndex(AIndex: Integer; var AOccurrence: TcxSchedulerEvent): Boolean;
    function GetOwner: TPersistent; override;
    function GetParentID: Variant; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetRecurrenceInfoValue(var AValue: AnsiString): Boolean; virtual;
    procedure GetStartFinishTime(var AStart, AFinish: TDateTime);
    function GetTaskIndex: Integer; virtual;
    function GetTaskLinks: TcxSchedulerEventLinks; virtual;
    function GetTaskLinkOwners: TcxSchedulerEventList; virtual;
    function GetTaskCompleteDuration: TDateTime; virtual;
    function GetValueByIndex(AIndex: Integer): Variant; virtual;
    function GetValueDefault(AField: TcxCustomSchedulerStorageField; const ADefValue: Variant): Variant; virtual;
    procedure DoGetValidTaskTimeRange(var AStart, AFinish: TDateTime); virtual;
    procedure InternalMoveTo(const AStartTime: TDateTime); virtual;
    procedure InvalidateReferences; virtual;
    function IsParentGroupForEvent(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure Modified;
    procedure PostEditingData;
    procedure PrepareTaskLinks; virtual;
    procedure RefreshTaskLinks;
    procedure ResetReferences;
    procedure RemoveTaskLink(ALink: TcxSchedulerEvent); virtual;
    procedure RemoveTaskLinkOwnersReferences; virtual;
    procedure ResetReminderResourcesData;
    procedure SetActualTimeRange(const ActualStart, ActualFinish: TDateTime);
    procedure SetRecurrenceInfoValue(const AValue: AnsiString); virtual;
    procedure SetValue(AField: TcxCustomSchedulerStorageField; const AValue: Variant); virtual;
    procedure SetValueByIndex(AIndex: Integer; const AValue: Variant); virtual;
    procedure TaskLinksChanged(Sender: TcxSchedulerEventLinks); virtual;
    procedure UpdateReminderDate;
    procedure UpdateParentGroup;
    procedure UpdateTemporaryData;

    procedure AddItem(AEvent: TcxSchedulerEvent);
    function CanAddItem(AEvent: TcxSchedulerEvent): Boolean;
    procedure RemoveItem(AEvent: TcxSchedulerEvent);

    property DataController: TcxSchedulerStorageDataController read GetDataController;
    property IsBoundMode: Boolean read GetIsBoundMode;
    property IsFreeState: Boolean read GetIsFreeState;
    property IsNewEvent: Boolean read GetIsNewEvent;
    property IsModified: Boolean read FIsModified write FIsModified;
    property Link: TcxSchedulerEvent read FLink;
    property ReminderResourcesData: TcxSchedulerReminderResourcesData read GetReminderResourcesData write SetReminderResourcesData;
    property ValueDefs[AFieldIndex: Integer]: TcxValueDef read GetValueDef;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage; ADisableEditing: Boolean = False); overload; virtual;
    constructor Create(AStorage: TcxCustomSchedulerStorage; ARecordHandle: Pointer); overload; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function AllowLink(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure AssignAttributes(ASource: TcxSchedulerEvent; AUseSourceTime: Boolean = True); virtual;
    procedure BeginEditing; virtual;
    procedure Cancel;
    function CanLink(AEvent: TcxSchedulerEvent): Boolean; virtual;
    function IsAllDayOrLonger: Boolean;
    function IsEventLinked(AHeaderEvent: TcxSchedulerEvent): Boolean; virtual;


    function Conflicts(AExceptEventsWithoutResources: Boolean): Boolean;
    function CreateConflictsInfo(AExceptEventsWithoutResources: Boolean): TcxSchedulerEventConflictsInfo;
    procedure Delete; virtual;
    procedure DeleteExceptions; virtual;
    procedure EndEditing; virtual;
    function GetOccurrence(ADate: TDateTime): TcxSchedulerEvent;
    function GetOriginalDate: TDateTime;
    function GetResourceItem: TcxSchedulerStorageResourceItem;
    function GetRecurrenceChain: TcxSchedulerEventList;
    function GetTaskLinkOwnerRelation(ATaskLinkOwner: TcxSchedulerEvent; var ARelation: TcxSchedulerEventRelation): Boolean; virtual;
    procedure GetValidTaskTimeRange(var AStart, AFinish: TDateTime);
    procedure GetValidTaskTimeRangeByRelation(const ARelation: TcxSchedulerEventRelation; AEvent: TcxSchedulerEvent; var AStart, AFinish: TDateTime); virtual;
    function HasAsParentGroup(AEvent: TcxSchedulerEvent): Boolean;
    function HasChildren: Boolean;
    function HasExceptions: Boolean;
    function HasReminderForResourceID(const AResourceID: Variant): Boolean;
    function IsDayEvent(ADate: Integer): Boolean; overload; inline;
    function IsDayEvent(const ADate: TDateTime): Boolean; overload;
    function IsRecurring: Boolean;
    function IsResourceEvent(AResource: TcxSchedulerStorageResourceItem; AllowUnassigned: Boolean): Boolean;
    procedure MoveTo(const AStartTime: TDateTime);
    procedure Post;
    procedure RemoveRecurrence; virtual;
    function TimeBias: Double; virtual;
    function ValidateOccurrenceTimeBounds(AOccurrence: TcxSchedulerEvent; const ANewStart, ANewFinish: TDateTime): TcxOccurrenceDateStatus;
    // resource sharing
    procedure ShareWithResource(AResourceItem: TcxSchedulerStorageResourceItem); overload;
    procedure ShareWithResource(AResourceID: Variant); overload;
    function IsSharedWithResource(AResourceItem: TcxSchedulerStorageResourceItem): Boolean; overload;
    function IsSharedWithResource(AResourceID: Variant): Boolean; overload;
    procedure UnshareWithResource(AResourceItem: TcxSchedulerStorageResourceItem); overload;
    procedure UnshareWithResource(AResourceID: Variant); overload;
    procedure ReplaceResourceID(AResourceID: Variant);
    //custom fields routines
    function GetCustomFieldValueByIndex(AIndex: Integer): Variant;
    function GetCustomFieldValueByName(const AName: string): Variant;
    function GetCustomFieldValue(ACustomField: TcxCustomSchedulerStorageField): Variant;
    procedure SetCustomFieldValueByIndex(AIndex: Integer; const AValue: Variant);
    procedure SetCustomFieldValueByName(const AName: string; const AValue: Variant);
    procedure SetCustomFieldValue(ACustomField: TcxCustomSchedulerStorageField; const AValue: Variant);

    property ActualStart: Integer read GetActualStart;
    property ActualFinish: Integer read GetActualFinish;
    property ActualLabelColor: Integer read GetActualLabelColor;
    property AllDayEvent: Boolean read GetAllDayEvent write SetAllDayEvent;
    property Data: Pointer read FData write FData;
    property Caption: string read GetCaption write SetCaption;
    property Duration: TDateTime read GetDuration write SetDuration;
    property EditingRecordHandle: Pointer read FEditingRecordHandle write FEditingRecordHandle;
    property EditValues[Index: Integer]: Variant read GetEditValue write SetEditValue;
    property Enabled: Boolean read GetEnabled write SetEnabled;
    property EventType: TcxEventType read GetEventType write SetEventType;
    property Finish: TDateTime read GetFinish write SetFinish;
    property GroupID: Variant read GetGroupID;
    property GroupItems: TcxSchedulerEventGroupItems read GetGroupItems;
    property ID: Variant read GetID write SetID;
    property Index: Integer read GetIndex;
    property IsDataValid: Boolean read FIsDataValid write SetIsDataValid;
    property IsEditing: Boolean read GetIsEditing;
    property IsGroup: Boolean read GetIsGroup write SetIsGroup;
    property LabelColor: Integer read GetLabelColor write SetLabelColor;
    property Location: string read GetLocation write SetLocation;
    property Message: string read Getmessage write SetMessage;
    property ParentID: Variant read GetParentID write SetParentID;
    property Pattern: TcxSchedulerEvent read FPattern;
    property ParentGroup: TcxSchedulerEvent read FParentGroup write SetParentGroup;
    property ReadOnly: Boolean read GetReadOnly;
    property RecordHandle: Pointer read FRecordHandle write FRecordHandle;
    property RecurrenceIndex: Integer read GetRecurrenceIndex write SetRecurrenceIndex;
    property RecurrenceInfo: TcxSchedulerEventRecurrenceInfo read FRecurrenceInfo write SetRecurrenceInfo;
    property Reminder: Boolean read GetReminder write SetReminder;
    property ReminderDate: TDateTime read GetReminderDate write SetReminderDate;
    property ReminderMinutesBeforeStart: Integer read GetReminderMinutesBeforeStart write SetReminderMinutesBeforeStart;
    property ResourceID: Variant read GetResourceID write SetResourceID;
    property ResourceIDCount: Integer read GetResourceIDCount;
    property ResourceIDs[Index: Integer]: Variant read GetResourceIDs;
    property Shared: Boolean read GetShared;
    property SkipExceptions: Boolean read FSkipExceptions write FSkipExceptions;
    property Start: TDateTime read GetStart write SetStart;
    property State: Integer read GetState write SetState;
    property Storage: TcxCustomSchedulerStorage read FStorage;
    property TaskComplete: Integer read GetTaskComplete write SetTaskComplete;
    property TaskIndex: Integer read GetTaskIndex write SetTaskIndex;
    property TaskLinks: TcxSchedulerEventLinks read GetTaskLinks;
    property TaskLinkOwners: TcxSchedulerEventList read GetTaskLinkOwners;
    property TaskStatus: TcxSchedulerEventTaskStatus read GetTaskStatus write SetTaskStatus;
    property UTCFinish: TDateTime read GetUTCFinish write SetUTCFinish;
    property UTCStart: TDateTime read GetUTCStart write SetUTCStart;
    property ValueCount: Integer read GetValueCount;
    property Values[Index: Integer]: Variant read GetValueByIndex write SetValueByIndex;
  end;

  { TcxSchedulerControlEvent }

  TcxSchedulerControlEvent = class(TcxSchedulerEvent)
  private
    FIsClone: Boolean;
    FIsEditing: Boolean;
    FIsSource: Boolean;
    FLineStart: Integer;
    FLockedResource: Variant;
    FSelectionAdapter: IcxSchedulerSelectionAdapter;
    FSource: TcxSchedulerEvent;
    FTimeBias: Double;
    FVisibleIndex: Integer;
    function GetNonExceptionLinkCount: Integer;
    function GetSelected: Boolean;
    procedure SetPattern(AValue: TcxSchedulerEvent);
    procedure SetSelected(AValue: Boolean);
  protected
    function CanMoveTo(ANewTime: TDateTime): Boolean; override;
    function CanMove: Boolean;
    function CanResize: Boolean;
    procedure CheckLinksOnChangeEventType(ANewEventType: TcxEventType); override;
    function CreateTaskLinks: TcxSchedulerEventLinks; override;
    function GetGroupItems: TcxSchedulerEventGroupItems; override;
    function GetIndex: Integer; override;
    function GetParentID: Variant; override;
    function GetReadOnly: Boolean; override;
    function GetTaskIndex: Integer; override;
    function GetTaskLinks: TcxSchedulerEventLinks; override;
    function GetTaskLinkOwners: TcxSchedulerEventList; override;
    function GetValueByIndex(AIndex: Integer): Variant; override;
    procedure DoGetValidTaskTimeRange(var AStart, AFinish: TDateTime); override;
    procedure InvalidateReferences; override;
    function IsParentGroupForEvent(AEvent: TcxSchedulerEvent): Boolean; override;
    procedure RemoveTaskLinkOwnersReferences; override;
    procedure PrepareTaskLinks; override;
    procedure SetSource(ASource: TcxSchedulerEvent);
    procedure SetValueByIndex(AIndex: Integer; const AValue: Variant); override;

    property LineStart: Integer read FLineStart write FLineStart;
    property SelectionAdapter: IcxSchedulerSelectionAdapter read FSelectionAdapter write FSelectionAdapter;
    property VisibleIndex: Integer read FVisibleIndex write FVisibleIndex;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage; ADisableEditing: Boolean = False); reintroduce; overload;
    constructor Create(ASource: TcxSchedulerEvent); reintroduce; overload;
    procedure InitFromSource(ASource: TcxSchedulerEvent);
    constructor Create(ASource: TcxSchedulerEvent; const AStart, AFinish: TDateTime); reintroduce; overload;
    constructor Create(ASource, AOriginal: TcxSchedulerEvent); reintroduce; overload;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure AssignDefaultValues; override;
    procedure BeginEditing; override;
    procedure Delete; override;
    procedure EndEditing; override;
    function GetTaskLinkOwnerRelation(ATaskLinkOwner: TcxSchedulerEvent; var ARelation: TcxSchedulerEventRelation): Boolean; override;
    function IsOrigin(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure lockResource(const ALockedResource: Variant);
    function OpenedInEditor: Boolean;
    function StorageTimeBias: Double;
    function TimeBias: Double; override;
    procedure unlockResource;

    property IsClone: Boolean read FIsClone;
    property IsEditing: Boolean read FIsEditing;
    property IsSource: Boolean read FIsSource;
    property NonExceptionLinkCount: Integer read GetNonExceptionLinkCount;
    property Selected: Boolean read GetSelected write SetSelected;
    property Source: TcxSchedulerEvent read FSource;
    property Pattern: TcxSchedulerEvent read FPattern write SetPattern;
  end;

  TcxSchedulerEventClass = class of TcxSchedulerEvent;

  TcxSchedulerGetResourceNameEvent = procedure(Sender: TObject;
    AResource: TcxSchedulerStorageResourceItem; var AResourceName: string) of object;

  TcxSchedulerGetResourceImageIndexEvent = procedure(Sender: TObject;
    AResource: TcxSchedulerStorageResourceItem; var AImageIndex: TcxImageIndex) of object;

  { TcxSchedulerStorageResourceItems }

  TcxSchedulerStorageResourceItems = class(TCollection)
  private
    FVisibleList: TdxFastList;
    FOwner: TcxSchedulerStorageResources;
    function GetItem(AIndex: Integer): TcxSchedulerStorageResourceItem;
    function GetStorage: TcxCustomSchedulerStorage;
    procedure SetItem(AIndex: Integer; AValue: TcxSchedulerStorageResourceItem);
    function GetVisibleResource(AIndex: Integer): TcxSchedulerStorageResourceItem;
    function GetVisibleResourceCount: Integer;
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;

    property Resources: TcxSchedulerStorageResources read FOwner;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor Create(AOwner: TcxSchedulerStorageResources; AItemClass: TcxSchedulerStorageResourceItemClass); virtual;
    destructor Destroy; override;
    function Add: TcxSchedulerStorageResourceItem;
    property Items[Index: Integer]: TcxSchedulerStorageResourceItem read GetItem write SetItem; default;
    property VisibleResourceCount: Integer read GetVisibleResourceCount;
    property VisibleResources[Index: Integer]: TcxSchedulerStorageResourceItem read GetVisibleResource;
  end;

  { TcxSchedulerStorageResourceItem }

  TcxSchedulerStorageResourceItem = class(TCollectionItem)
  private
    FColor: TColor;
    FImageIndex: TcxImageIndex;
    FName: string;
    FParent: TcxSchedulerStorageResourceItem;
    FReadOnly: Boolean;
    FResourceID: Variant;
    FVisible: Boolean;
    FWorkDays: TDays;
    FWorkFinish: TTime;
    FWorkFinishAssigned: Boolean;
    FWorkStart: TTime;
    FWorkStartAssigned: Boolean;
    function GetActualImageIndex: TcxImageIndex;
    function GetActuallyVisible: Boolean;
    function GetResources: TcxSchedulerStorageResources;
    function IsWorkDaysStored: Boolean;
    procedure ReadWorkFinish(AReader: TReader);
    procedure ReadWorkStart(AReader: TReader);
    procedure SetParent(AValue: TcxSchedulerStorageResourceItem);
    procedure WriteWorkFinish(AWriter: TWriter);
    procedure WriteWorkStart(AWriter: TWriter);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    // methods
    function GetDisplayName: string; override;

    function GetActualEventColor: TColor; virtual;
    function GetBackgroundColor: TColor; virtual;
    function GetReadOnly: Boolean; virtual;
    function GetResourceID: Variant; virtual;
    procedure SetColor(const AValue: TColor); virtual;
    procedure SetImageIndex(const AValue: TcxImageIndex); virtual;
    procedure SetName(const AValue: string); virtual;
    procedure SetReadOnly(const AValue: Boolean); virtual;
    procedure SetResourceID(const AValue: Variant); virtual;
    procedure SetVisible(const AValue: Boolean); virtual;
    procedure SetWorkDays(AValue: TDays); virtual;
    procedure SetWorkFinish(const AValue: TTime); virtual;
    procedure SetWorkStart(const AValue: TTime); virtual;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    function CanModify: Boolean; virtual;
    function HasAsParent(AItem: TcxSchedulerStorageResourceItem): Boolean;

    property ActualEventColor: TColor read GetActualEventColor;
    property ActualImageIndex: TcxImageIndex read GetActualImageIndex;
    property ActuallyVisible: Boolean read GetActuallyVisible;
    property BackgroundColor: TColor read GetBackgroundColor;
    property Resources: TcxSchedulerStorageResources read GetResources;
  published
    property Color: TColor read FColor write SetColor default clDefault;
    property ImageIndex: TcxImageIndex read FImageIndex write SetImageIndex default -1;
    property Name: string read FName write SetName;
    property Parent: TcxSchedulerStorageResourceItem read FParent write SetParent;
    property ReadOnly: Boolean read GetReadOnly write SetReadOnly default False;
    property ResourceID: Variant read GetResourceID write SetResourceID;
    property Visible: Boolean read FVisible write SetVisible default True;
    property WorkFinish: TTime read FWorkFinish write SetWorkFinish stored False;
    property WorkStart: TTime read FWorkStart write SetWorkStart stored False;
    property WorkDays: TDays read FWorkDays write SetWorkDays stored IsWorkDaysStored;
  end;

  { TcxSchedulerStorageResources }

  TcxSchedulerStorageResources = class(TcxInterfacedPersistent, IcxStoredObject)
  private
    FImages: TCustomImageList;
    FItems: TcxSchedulerStorageResourceItems;
    FItemsVisibility: TdxVariantToObjectDictionary<TcxSchedulerStorageResourceItem>;
    FLockCount: Integer;
    FIsNullResourceVisible: Boolean;
    FOwner: TcxCustomSchedulerStorage;
    FRestoringItems: TList;
    FOnGetResourceImageIndex: TcxSchedulerGetResourceImageIndexEvent;
    FOnGetResourceName: TcxSchedulerGetResourceNameEvent;
    function GetAreImagesUsed: Boolean;
    procedure SetImages(AValue: TCustomImageList);
    procedure SetItems(AValue: TcxSchedulerStorageResourceItems);
  protected
    function DecodePropertyName(const AName: string; var ASubValue: string): Integer;
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // methods
    procedure BeginUpdate;
    procedure Changed; virtual;
    function CreateItems: TcxSchedulerStorageResourceItems; virtual;
    procedure DoneRestore;
    function DoGetResourceImageIndex(AItem: TcxSchedulerStorageResourceItem): TcxImageIndex; virtual;
    function DoGetResourceName(AItem: TcxSchedulerStorageResourceItem): string; virtual;
    function GetOwner: TPersistent; override;
    function GetResourceItems: TcxSchedulerStorageResourceItems; virtual;
    procedure EndUpdate;
    procedure InitRestore;
    function IsLocked: Boolean; virtual;
    function IsResourceVisible(const AResourceID: Variant): Boolean;
    //
    property IsNullResourceVisible: Boolean read FIsNullResourceVisible write FIsNullResourceVisible;
  public
    constructor Create(AOwner: TcxCustomSchedulerStorage); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    procedure PopulateItemsAsTreeStructure(AList: TList);
    function GetResourceName(AResource: TcxSchedulerStorageResourceItem): string;
    function GetResourceNameByID(const AResource: Variant): string;

    property AreImagesUsed: Boolean read GetAreImagesUsed;
    property ResourceItems: TcxSchedulerStorageResourceItems read GetResourceItems;
    property Storage: TcxCustomSchedulerStorage read FOwner;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Items: TcxSchedulerStorageResourceItems read FItems write SetItems;
    property OnGetResourceImageIndex: TcxSchedulerGetResourceImageIndexEvent read FOnGetResourceImageIndex write FOnGetResourceImageIndex;
    property OnGetResourceName: TcxSchedulerGetResourceNameEvent read FOnGetResourceName write FOnGetResourceName;
  end;

  TcxSchedulerNotificationEvent = procedure(Sender: TObject;
    AEvent: TcxSchedulerEvent; var AHandled: Boolean) of object;

  TcxSchedulerFilterEventEvent = procedure (Sender: TcxCustomSchedulerStorage;
    AEvent: TcxSchedulerEvent; var Accept: Boolean) of object;

  { TcxSchedulerEventConflictsInfo }

  TcxSchedulerEventConflictsInfo = class
  private
    FExceptEventsWithoutResources: Boolean;
    FConflictEvents: TcxSchedulerFilteredEventList;
    FExcludedEvent: TcxSchedulerEvent;
    FEvent: TcxSchedulerEvent;
    FStorage: TcxCustomSchedulerStorage;
    FTimeRanges: TcxSchedulerTimeRanges;
    function GetHasConflicts: Boolean;
    function GetHasFreeTime: Boolean;
  protected
    ExcludedEventID: TcxSchedulerControlEventID;
    EventID: TcxSchedulerControlEventID;
    Start, Finish: TDateTime;
    ResourceID: Variant;
    procedure AddRange(AStart, AFinish: TDateTime);
    procedure CheckFreeTimeRanges;
    procedure CheckIntersectionWithEvents;
    procedure CheckSomeIntersection;
    function ExcludeFromCalculate(AEvent: TcxSchedulerEvent): Boolean;
    procedure ExpandRange(AEvent: TcxSchedulerEvent; var AIndex: Integer; var ALastPosition: TDateTime);
    procedure Init(AStorage: TcxCustomSchedulerStorage; AExceptEventsWithoutResources: Boolean; AResourceID: Variant;
      const AStart, AFinish: TDateTime);
    function IntersectEvents(ACheckedEvent: TcxSchedulerEvent): Boolean; overload;
    function IntersectEvents(AEvent1, AEvent2: TcxSchedulerEvent): Boolean; overload;
    function IntersectTime(const AStart, AFinish: TDateTime): Boolean; overload;
    function IntersectTime(const AStart, AFinish, AStart1, AFinish1: TDateTime): Boolean; overload;
    function IsSameEvent(ACheckedEvent: TcxSchedulerEvent): Boolean;
    function IsSameSeries(AEvent1, AEvent2: TcxSchedulerEvent): Boolean;
  public
    constructor Create(AEvent: TcxSchedulerEvent; AExceptEventsWithoutResources: Boolean); overload; virtual;
    constructor Create(AEvent: TcxSchedulerEvent; AExceptEventsWithoutResources: Boolean;
      const AStart, AFinish: TDateTime); overload; virtual;
    constructor Create(AStorage: TcxCustomSchedulerStorage; AExceptEventsWithoutResources: Boolean; const AStart, AFinish: TDateTime;
      AResourceID: Variant; AExcludedEvent: TcxSchedulerEvent = nil); overload; virtual;
    destructor Destroy; override;
    procedure Calculate(ACalculateFreeTime: Boolean = True); virtual;

    property ConflictEvents: TcxSchedulerFilteredEventList read FConflictEvents;
    property ExceptEventsWithoutResources: Boolean read FExceptEventsWithoutResources;
    property Event: TcxSchedulerEvent read FEvent;
    property HasConflicts: Boolean read GetHasConflicts;
    property HasFreeTime: Boolean read GetHasFreeTime;
    property Storage: TcxCustomSchedulerStorage read FStorage;
    property TimeRanges: TcxSchedulerTimeRanges read FTimeRanges;
  end;

  TcxSchedulerEventConflictsInfoClass = class of TcxSchedulerEventConflictsInfo;

  { TcxSchedulerEventLinksController }

  TcxSchedulerEventLinksController = class(TObject)
  private
    FStorage: TcxCustomSchedulerStorage;
  protected
    function IsLinked(AEvent, ALink: TcxSchedulerEvent; ACheckGroupItems: Boolean = True): Boolean;
    function DoAllowLink(AEvent, ALink: TcxSchedulerEvent): Boolean; virtual;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage); virtual;

    function AllowLink(AEvent, ALink: TcxSchedulerEvent): Boolean;

    property Storage: TcxCustomSchedulerStorage read FStorage;
  end;
  TcxSchedulerEventLinksControllerClass = class of TcxSchedulerEventLinksController;

  { TcxSchedulerEditingEventInfo }

  TcxSchedulerEditingEventInfo = class
  private
    FEditInfo: TcxSchedulerEventEditInfo;
    FEvent: TcxSchedulerControlEvent;
    FEventModified: Boolean;
    FEditor: TForm;
    FEventType: TcxEventType;
    FFreeNotificator: TcxFreeNotificator;
    FIsDestroying: Boolean;
    FOwner: TcxSchedulerEditingEventInfoList;
    FPatternID: Variant;
    FRecurrenceIndex: Integer;
    FRecurrenceInfoData: AnsiString;
    FSourceID: Variant;
    FStorage: TcxCustomSchedulerStorage;
    FOnAfterEditing: TcxOnAfterEditingProc;

    procedure DoCloseEditor(Sender: TComponent);
    function GetIsDestroying: Boolean;
    function GetPatternRecurrenceInfoData: AnsiString;
    procedure SetEvent(AValue: TcxSchedulerControlEvent);
    procedure SetOwner(AValue: TcxSchedulerEditingEventInfoList);
  protected
    procedure DoAfterEditing;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant;
    function IsEventDeleted: Boolean; virtual;
    function IsPatternValid: Boolean;
    procedure Reset;
    procedure SetStorage(AValue: TcxCustomSchedulerStorage);
    procedure Update; virtual;
    procedure UpdateStorage; virtual;

    property Editor: TForm read FEditor;
    property SourceID: Variant read FSourceID write FSourceID;
  public
    constructor Create(AOwner: TcxSchedulerEditingEventInfoList; const AEditInfo: TcxSchedulerEventEditInfo);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    procedure CloseEditor;
    function IsEventEditing(ASourceID, APatternID: Variant; ARecurrenceIndex: Integer;
      AEventType: TcxEventType): Boolean; virtual;
    function IsValid: Boolean;
    procedure ReplaceEvent(AEvent: TcxSchedulerControlEvent);
    function ShowEditor(out AModified: Boolean): Boolean;
    procedure Destroying;

    function CreateEvent(ASource: TcxSchedulerEvent; AOriginal: TcxSchedulerControlEvent;
      AForcePatternEditing: Boolean): TcxSchedulerControlEvent;
    procedure DeleteEvent(ADeleteSeries: Boolean);

    property EditInfo: TcxSchedulerEventEditInfo read FEditInfo;
    property Event: TcxSchedulerControlEvent read FEvent;
    property EventType: TcxEventType read FEventType write FEventType;
    property IsDestroying: Boolean read GetIsDestroying;
    property Owner: TcxSchedulerEditingEventInfoList read FOwner write SetOwner;
    property PatternID: Variant read FPatternID write FPatternID;
    property RecurrenceIndex: Integer read FRecurrenceIndex write FRecurrenceIndex;
    property Storage: TcxCustomSchedulerStorage read FStorage;
    property OnAfterEditing: TcxOnAfterEditingProc read FOnAfterEditing write FOnAfterEditing;
  end;

  TcxSchedulerEditingEventInfoClass = class of TcxSchedulerEditingEventInfo;

  { TcxSchedulerEditingEventInfoList }

  TcxSchedulerEditingEventInfoList = class(TList)
  private
    FIsDestroying: Boolean;
    FLockCount: Integer;
    FStorage: TcxCustomSchedulerStorage;

    function GetActiveEditingEvent: TcxSchedulerControlEvent;
    function GetItem(AIndex: Integer): TcxSchedulerEditingEventInfo; inline;
  protected
    procedure CloseExceptionEditor(AEvent: TcxSchedulerEvent);
    procedure DoDestroy; virtual;
    function DoShowEditor(const AEditInfo: TcxSchedulerEventEditInfo; out AModified: Boolean): Boolean;
    procedure DoUpdate; virtual;
    function FindExceptionInfo(const AExceptionEvent: TcxSchedulerEvent): TcxSchedulerEditingEventInfo; virtual;
    function GetCount: Integer; virtual;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant; virtual;
    function GetInfoClass: TcxSchedulerEditingEventInfoClass; virtual;
    function GetItemCore(AIndex: Integer): TcxSchedulerEditingEventInfo; virtual;
    function GetRootEvent(AEvent: TcxSchedulerEvent): TcxSchedulerEvent; virtual;
    function GetRootStorage(AEvent: TcxSchedulerEvent): TcxCustomSchedulerStorage;
    function IsEventEditing(ASourceID, APatternID: Variant;
      ARecurrenceIndex: Integer; AEventType: TcxEventType): Boolean; overload;

    function CreateEvent(ASource, AOriginal: TcxSchedulerEvent;
      AForcePatternEditing: Boolean): TcxSchedulerControlEvent; overload; virtual;
    function CreateEvent(const AEventInfo: TcxSchedulerEventEditInfo): TcxSchedulerControlEvent; overload;

    property IsDestroying: Boolean read FIsDestroying;
    property Storage: TcxCustomSchedulerStorage read FStorage;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage);
    destructor Destroy; override;

    procedure BeforeDestruction; override;

    procedure BeginUpdate;
    procedure CancelUpdate;
    procedure EndUpdate;
    function IsLocked: Boolean;

    procedure Update;

    procedure DeleteInfoByEvent(const AEvent: TcxSchedulerEvent);
    function FindInfo(const ASourceID: Variant): TcxSchedulerEditingEventInfo;

    function IsEventEditing(AEvent: TcxSchedulerEvent; ARecurrenceIndex: Integer;
      AEventType: TcxEventType): Boolean; overload; virtual;
    function IsEventEditing(AEvent: TcxSchedulerControlEvent; AForcePatternEditing: Boolean): Boolean; overload;
    function ShowEditor(const AEventInfo: TcxSchedulerEventEditInfo;
      out AModified: Boolean): Boolean; overload;
    procedure ShowEditor(const AEventInfo: TcxSchedulerEventEditInfo); overload;

    property ActiveEditingEvent: TcxSchedulerControlEvent read GetActiveEditingEvent;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TcxSchedulerEditingEventInfo read GetItem; default;
  end;

  TcxSchedulerEditingEventInfoListClass = class of TcxSchedulerEditingEventInfoList;

  { TcxCustomSchedulerStorage }

  TcxSchedulerEventIntersectEvent = procedure(Sender: TcxCustomSchedulerStorage; AEvent1, AEvent2: TcxSchedulerEvent;
    var Allow: Boolean) of object;

  TcxCustomSchedulerStorage = class(TcxCustomComponent, IcxSchedulerHolidaysListener)
  private
    FActive: Boolean;
    FCustomFields: TcxCustomSchedulerStorageFields;
    FDataController: TcxSchedulerStorageDataController;
    FEditor: TForm;
    FEditingEventInfoList: TcxSchedulerEditingEventInfoList;
    FHolidays: TcxSchedulerHolidays;
    FInternalFields: TcxCustomSchedulerStorageFields;
    FIsChanged: Boolean;
    FIsModified: Boolean;
    FIsSchedulerBiDiModeWasChanged: Boolean;
    FInternalUpdate: Boolean;
    FLinksController: TcxSchedulerEventLinksController;
    FListeners: TInterfaceList;
    FRefreshNeeded: Boolean;
    FReminders: TcxSchedulerReminders;
    FResources: TcxSchedulerStorageResources;
    FRemindersEvents: TNotifyEvent;
    FResourcesEvents: TNotifyEvent;
    FStoreUsingGlobalTime: Boolean;
    FStoringName: string;
    FStructureUpdatingCount: Integer;
    FTimeBias: Double;
    FUpdateRemindersTimer: TTimer;
    FUseActualTimeRange: Boolean;
    FOnEventDeleted: TcxSchedulerNotificationEvent;
    FOnEventInserted: TcxSchedulerNotificationEvent;
    FOnEventIntersect: TcxSchedulerEventIntersectEvent;
    FOnEventModified: TcxSchedulerNotificationEvent;
    FOnFilterEvent: TcxSchedulerFilterEventEvent;
    function GetDataField(AIndex: Integer): TcxCustomSchedulerStorageField;
    function GetEvent(AIndex: Integer): TcxSchedulerEvent;
    function GetEventCount: Integer;
    function GetField(AIndex: Integer): TcxCustomSchedulerStorageField;
    function GetFieldCount: Integer;
    function GetInternalFieldCount: Integer;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetIsLocked: Boolean;
    function GetCanUpdateReminders: Boolean;
    function GetIsUpdatingMode: Boolean; inline;
    function GetResourceCount: Integer;
    function GetResourceID(AIndex: Integer): Variant;
    function GetResourceName(AIndex: Integer): string;
    function GetSortedList: TdxFastList;
    procedure SetActive(AValue: Boolean);
    procedure SetEvent(AIndex: Integer; AValue: TcxSchedulerEvent);
    procedure SetHolidays(AValue: TcxSchedulerHolidays);
    procedure SetOnFilterEvent(AValue: TcxSchedulerFilterEventEvent);
    procedure SetReminders(AValue: TcxSchedulerReminders);
    procedure SetResources(AValue: TcxSchedulerStorageResources);
    procedure SetStoreUsingGlobalTime(AValue: Boolean);
    procedure StopUpdateRemindersTimer;
    procedure UpdateReminders;
    procedure UpdateRemindersTimerEvent(Sender: TObject);
  protected
    FDeletedEvents: TcxObjectList;
    FEventIDCounter: Integer;
    FEventsList: TcxSchedulerEventList;
    FFields: TdxFastList;
    FIsLoading: Boolean;
    FNewEvents: TcxSchedulerEventList;
    FSortedList: TdxFastList;
    LockCount: Integer;
    // internal fields
    FActualFinishField: TcxCustomSchedulerStorageField;
    FActualStartField: TcxCustomSchedulerStorageField;
    FCaptionField: TcxCustomSchedulerStorageField;
    FEventTypeField: TcxCustomSchedulerStorageField;
    FFinishField: TcxCustomSchedulerStorageField;
    FGroupIDField: TcxCustomSchedulerStorageField;
    FLabelColorField: TcxCustomSchedulerStorageField;
    FLocationField: TcxCustomSchedulerStorageField;
    FMessageField: TcxCustomSchedulerStorageField;
    FOptionsField: TcxCustomSchedulerStorageField;
    FParentIDField: TcxCustomSchedulerStorageField;
    FRecurrenceIndexField: TcxCustomSchedulerStorageField;
    FRecurrenceInfoField: TcxCustomSchedulerStorageField;
    FReminderDateField: TcxCustomSchedulerStorageField;
    FReminderMinutesBeforeStartField: TcxCustomSchedulerStorageField;
    FResourceIDField: TcxCustomSchedulerStorageField;
    FStartField: TcxCustomSchedulerStorageField;
    FStateField: TcxCustomSchedulerStorageField;
    // additional for tasks
    FTaskCompleteField: TcxCustomSchedulerStorageField;
    FTaskIndexField: TcxCustomSchedulerStorageField;
    FTaskLinksField: TcxCustomSchedulerStorageField;
    FTaskStatusField: TcxCustomSchedulerStorageField;
    // additional for reminders
    FReminderResourcesData: TcxCustomSchedulerStorageField;
    function ActualTimeRangeAvailable: Boolean; virtual;
    function AddEventRecord(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure AddInternalField(var AField: TcxCustomSchedulerStorageField;
      AValueType: TcxValueTypeClass; AIsUnique: Boolean = True); virtual;
    function AllocateRecord: Pointer; virtual;
    procedure AssignIndexes;
    //
    procedure AfterPostEditingData(AEvent: TcxSchedulerEvent); virtual;
    procedure BeforePostEditingData(AEvent: TcxSchedulerEvent); virtual;
    //
    procedure BeforeDeleteEvent(AEvent: TcxSchedulerEvent); virtual;
    procedure CancelEvent(AEvent: TcxSchedulerEvent); virtual;
    function CanGetValueFromPattern(AIndex: Integer): Boolean; virtual;
    procedure CalculateTimeBias;
    procedure Changed; virtual;
    procedure CheckEditingEventInfo; virtual;
    procedure CreateDefaultFields(const AVersion: Double); virtual;
    function CreateEventInternal: TcxSchedulerEvent; virtual;
    function CreateFields: TcxCustomSchedulerStorageFields; virtual;
    procedure CreateHolidayEvent(AHoliday: TcxSchedulerHolidaysLocationHoliday; AResourceID: Variant); virtual;
    function CreateReminders: TcxSchedulerReminders; virtual;
    function CreateResources: TcxSchedulerStorageResources; virtual;
    procedure CreateSubClasses; virtual;
    procedure DeleteEvents(AEvents: TcxObjectList); virtual;
    procedure DestroyDefaultFields(AVersion: Double);
    procedure DestroySubClasses; virtual;
    procedure DoDeleteEvent(AEvent: TcxSchedulerEvent); virtual;
    procedure DoDestroyEvent(AEvent: TcxSchedulerEvent); virtual;
    function DoEventDeleted(AEvent: TcxSchedulerEvent): Boolean; virtual;
    function DoEventInserted(AEvent: TcxSchedulerEvent): Boolean; virtual;
    function DoEventIntersect(AEvent1, AEvent2: TcxSchedulerEvent): Boolean; virtual;
    function DoEventModified(AEvent: TcxSchedulerEvent): Boolean; virtual;
    function DoFilterEvent(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure DoRefresh; virtual;
    procedure DoUpdateStructure(ANeedUpdateInfo: Boolean = True); virtual;
    procedure FreeRecord(var ARecordHandle: Pointer); virtual;
    procedure GenerateHolidayEventsBySchedulerHolidays(const AResourceID: Variant; AHolidays: TcxSchedulerHolidays); virtual;
    function GetDataControllerClass: TcxCustomDataControllerClass; virtual;
    function GetEditingEventInfoListClass: TcxSchedulerEditingEventInfoListClass; virtual;
    function GetEventClass: TcxSchedulerEventClass; virtual;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant; virtual;
    function GetFieldValueTypeClass(AField: TcxCustomSchedulerStorageField): TcxValueTypeClass; virtual;
    function GetLinksControllerClass: TcxSchedulerEventLinksControllerClass; virtual;
    function GetParentForm: TForm;
    function GetStoreUsingGlobalTime: Boolean; virtual;
    function GetEventActualValue(AEvent: TcxSchedulerEvent; AIndex: Integer): Variant;
    function GetEventValue(const AHandle: Pointer; AIndex: Integer): Variant; virtual;
    function GetIsBoundMode: Boolean; virtual;
    function HasEventIntersect(AEvent1, AEvent2: TcxSchedulerEvent): Boolean; virtual;
    procedure InternalGetEvents(AList: TcxSchedulerFilteredEventList;
      const AStart, AFinish: TDateTime; const AResourceID: Variant);
    procedure InternalDelete(AEvent: TcxSchedulerEvent); virtual;
    procedure InternalRemove(AEvent: TcxSchedulerEvent);
    function IsEventDeleting(AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure ItemAdded(AItem: TcxCustomSchedulerStorageField);
    procedure ItemRemoved(AItem: TcxCustomSchedulerStorageField);
    function IsDataSettingsValid: Boolean; virtual;
    procedure LayoutChanged; virtual;
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure PostEvent(AEvent: TcxSchedulerEvent); virtual;
    procedure PostEditingData(AEvent: TcxSchedulerEvent); virtual;
    procedure SendNotification(AEvent: TcxSchedulerEvent;
      AIsRemoved: Boolean = False; ACheckLockCount: Boolean = True); virtual;
    procedure SetEventValue(AEvent: TcxSchedulerEvent; AFieldIndex: Integer; const AValue: Variant); virtual;
    procedure SetPostFieldValue(AEvent: TcxSchedulerEvent; AFieldIndex: Integer); virtual;
    procedure SetValue(const ARecordHandle: Pointer; const AFieldIndex: Integer; const AValue: Variant); virtual;
    procedure SetValueBeforePost(AEvent: TcxSchedulerEvent; AItemIndex: Integer); virtual;
    procedure SynchronizeEventsWithRecords; virtual;
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); virtual;
    procedure UpdateData; virtual;
    procedure UpdateItemIndexes;
    procedure UpdateRemindersTimer;
    procedure UpdateStructure; virtual;

    procedure BeginUpdateDataController; virtual;
    procedure EndUpdateDataController; virtual;

    procedure BeginStructureUpdating;
    procedure EndStructureUpdating;
    function IsStructureUpdating: Boolean;

    property Active: Boolean read FActive write SetActive;
    property CanUpdateReminders: Boolean read GetCanUpdateReminders;
    property DataController: TcxSchedulerStorageDataController read FDataController;
    property SortedList: TdxFastList read GetSortedList;
    property DataFields[Index: Integer]: TcxCustomSchedulerStorageField read GetDataField;
    property Editor: TForm read FEditor write FEditor;
    property CustomFields: TcxCustomSchedulerStorageFields read FCustomFields;
    property InternalFields: TcxCustomSchedulerStorageFields read FInternalFields;
    property InternalFieldCount: Integer read GetInternalFieldCount;
    property IsChanged: Boolean read FIsChanged write FIsChanged;
    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsLocked: Boolean read GetIsLocked;
    property IsModified: Boolean read FIsModified;
    property IsSchedulerBiDiModeWasChanged: Boolean read FIsSchedulerBiDiModeWasChanged;
    property IsUpdatingMode: Boolean read GetIsUpdatingMode;
    property Listeners: TInterfaceList read FListeners;
    property RefreshNeeded: Boolean read FRefreshNeeded write FRefreshNeeded;
    property OnEventDeleted: TcxSchedulerNotificationEvent read FOnEventDeleted write FOnEventDeleted;
    property OnEventInserted: TcxSchedulerNotificationEvent read FOnEventInserted write FOnEventInserted;
    property OnEventIntersect: TcxSchedulerEventIntersectEvent read FOnEventIntersect write FOnEventIntersect;
    property OnEventModified: TcxSchedulerNotificationEvent read FOnEventModified write FOnEventModified;
    property OnFilterEvent: TcxSchedulerFilterEventEvent read FOnFilterEvent write SetOnFilterEvent;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure AddListener(AListener: IcxSchedulerStorageListener);
    procedure BeginUpdate; virtual;
    procedure CalculateEventActualTimeRanges;
    function CheckRequiredFields: Boolean; virtual;
    function createEvent: TcxSchedulerEvent; virtual;{CreateEvent conflicts with C++ macro}
    function CreateOccurrence(APattern: TcxSchedulerEvent; const ADate: TDateTime;
      AType: TcxEventType): TcxSchedulerEvent;
    procedure Clear; virtual;
    procedure DoneRestore;
    procedure EndUpdate;
    function IsEventSharingAllowed: Boolean; virtual;
    function FindAvailableAllDay(var AStart, AFinish: TDateTime;
      AResourceID: Variant; AExceptEventsWithoutResources: Boolean; ADuration: TDateTime = 0): Boolean;
    function FindAvailableTime(var AStart, AFinish: TDateTime; AllDay: Boolean; AResourceID: Variant;
      AExceptEventsWithoutResources: Boolean; ADuration: TDateTime = 0; AExcludedEvent: TcxSchedulerEvent = nil): Boolean; overload;
    function FindAvailableTime(AEvent: TcxSchedulerEvent;
      AExceptEventsWithoutResources: Boolean; var AStart, AFinish: TDateTime): Boolean; overload;
    procedure FullRefresh;
    procedure GenerateHolidayEvents(const AResourceID: Variant); virtual;
    function GetEventByID(const AID: Variant): TcxSchedulerEvent;
    function GetEvents(AList: TcxSchedulerFilteredEventList;
      const AStart, AFinish: TDateTime): Boolean; overload;
    function GetEvents(AList: TcxSchedulerFilteredEventList;
      const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean; overload; virtual;
    function GetFieldByName(const AName: string): TcxCustomSchedulerStorageField;
    function GetHolidayNamesByDate(ADate: TDate; var ANames: string;
      AOnlyVisible: Boolean = True): Boolean; virtual;
    function GetReminderEvents(ADateTime: TDateTime;
      AList: TcxSchedulerFilteredEventList): Boolean;
    procedure InitRestore;
    function IsActive: Boolean; virtual;
    function IsCaptionAvailable: Boolean; virtual;
    function IsEventEditing(AEvent: TcxSchedulerControlEvent): Boolean;
    function IsLabelColorAvailable: Boolean; virtual;
    function IsLocationAvailable: Boolean; virtual;
    function IsMessageAvailable: Boolean; virtual;
    function IsRecurrenceAvailable: Boolean; virtual;
    function IsReminderByResourceAvailable: Boolean; virtual;
    function IsReminderAvailable: Boolean; virtual;
    function IsStateAvailable: Boolean; virtual;
    procedure PopulateHolidayDates(AList: TcxSchedulerDateList; AStart, AFinish: TDate;
      AOnlyVisible: Boolean = True; AClearList: Boolean = True); virtual;
    procedure PostEvents; virtual;
    procedure RemoveListener(AListener: IcxSchedulerStorageListener);

    // IcxSchedulerHolidaysListener
    procedure HolidaysChanged(Sender: TObject);
    procedure HolidaysRemoved(Sender: TObject);

    property EditingEventInfoList: TcxSchedulerEditingEventInfoList read FEditingEventInfoList;
    property EventCount: Integer read GetEventCount;
    property Events[Index: Integer]: TcxSchedulerEvent read GetEvent write SetEvent;
    property FieldCount: Integer read GetFieldCount;
    property Fields[Index: Integer]: TcxCustomSchedulerStorageField read GetField;
    property Holidays: TcxSchedulerHolidays read FHolidays write
      SetHolidays;
    property InternalUpdate: Boolean read FInternalUpdate;
    property LinksController: TcxSchedulerEventLinksController read FLinksController;
    property Reminders: TcxSchedulerReminders read FReminders write SetReminders;
    property ResourceCount: Integer read GetResourceCount;
    property ResourceIDs[Index: Integer]: Variant read GetResourceID;
    property ResourceNames[Index: Integer]: string read GetResourceName;
    property Resources: TcxSchedulerStorageResources read FResources write SetResources;
    property StoringName: string read FStoringName write FStoringName;
    property TimeBias: Double read FTimeBias;
  published
    property UseActualTimeRange: Boolean read FUseActualTimeRange write FUseActualTimeRange default False;
    property StoreUsingGlobalTime: Boolean read GetStoreUsingGlobalTime write SetStoreUsingGlobalTime default False;
    property RemindersEvents: TNotifyEvent read FRemindersEvents write FRemindersEvents;
    property ResourcesEvents: TNotifyEvent read FResourcesEvents write FResourcesEvents;
  end;

  { TcxSchedulerStorage }

  TcxSchedulerStorage = class(TcxCustomSchedulerStorage)
  private
    function GetCustomFields: TcxSchedulerStorageFields;
    procedure SetCustomFields(const AValue: TcxSchedulerStorageFields);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant; override;
  public
    procedure Assign(Source: TPersistent); override;
    procedure SaveToFile(const AFileName: string);
    procedure SaveToStream(AStream: TStream); virtual;
    procedure LoadFromFile(const AFileName: string);
    procedure LoadFromStream(AStream: TStream); virtual;
  published
    property CustomFields: TcxSchedulerStorageFields read GetCustomFields write SetCustomFields;
    property Holidays;
    property Reminders;
    property Resources;
    property OnEventDeleted;
    property OnEventInserted;
    property OnEventIntersect;
    property OnEventModified;
    property OnFilterEvent;
  end;

  { TcxSchedulerControlEventID }

  TcxSchedulerControlEventID = class
  public
    EventType: TcxEventType;
    ID, ParentID, PatternID: Variant;
    RecurrenceIndex: Integer;
    constructor Create(AEvent: TcxSchedulerEvent); overload;
    constructor Create(AEventID: TcxSchedulerControlEventID); overload;
    function Equals(AValue: TcxSchedulerControlEventID): Boolean; reintroduce;
    function SameEvent(AEvent: TcxSchedulerEvent): Boolean;
  end;

  { TcxSchedulerEventSelection }

  TcxEventSelectionChangedEvent = procedure (AEvent: TcxSchedulerControlEvent) of object;

  TcxSchedulerEventSelection = class(TcxIUnknownObject, IcxSchedulerSelectionAdapter)
  private
    FOnEventSelectionChanged: TcxEventSelectionChangedEvent;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TcxSchedulerControlEvent;
    function GetKey(AIndex: Integer): TcxSchedulerControlEventID;
    function GetKeyCount: Integer;
  protected
    FKeys: TList;
    FEvents: TList;
    FSourceEvents: TcxSchedulerEventList;
    procedure ClearKeys;
    function CreateItem(AEvent: TcxSchedulerEvent): TcxSchedulerControlEventID;
    procedure DoEventSelectionChanged(AEvent: TcxSchedulerControlEvent);
    function KeyIndexOf(AEvent: TcxSchedulerControlEvent): Integer;
    function IndexOf(AEvent: TcxSchedulerControlEvent): Integer;
    procedure InternalClear;
    procedure ReplaceSelection;

    property KeyCount: Integer read GetKeyCount;
    property Keys[Index: Integer]: TcxSchedulerControlEventID read GetKey;
  public
    constructor Create(ASourceEvents: TcxSchedulerEventList); virtual;
    destructor Destroy; override;
    procedure Add(AEvent: TcxSchedulerControlEvent; AShift: TShiftState);
    procedure Clear;
    function IsSelected(AEvent: TcxSchedulerControlEvent): Boolean;
    procedure Select(AEvent: TcxSchedulerEvent);
    procedure Update;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxSchedulerControlEvent read GetItem; default;
    property OnEventSelectionChanged: TcxEventSelectionChangedEvent
      read FOnEventSelectionChanged write FOnEventSelectionChanged;
  end;

  { TcxSchedulerEventList }

  TcxSchedulerEventList = class
  private
    FItems: TdxFastList;
    function GetCount: Integer;
    function GetFirst: TcxSchedulerEvent;
    function GetEvent(AIndex: Integer): TcxSchedulerEvent;
    function GetLast: TcxSchedulerEvent;
  protected
    procedure BeforeDestroyEvent(AEvent: TcxSchedulerEvent); inline;
    procedure EventAdded(AEvent: TcxSchedulerEvent); virtual;
    procedure DestroyEvent(AEvent: TcxSchedulerEvent); virtual;
    procedure DoClear(AFreeItems: Boolean); virtual;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Assign(ASource: TcxSchedulerEventList; AOperator: TListAssignOp = laCopy); virtual;
    function Add(AEvent: TcxSchedulerEvent): Integer; virtual;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure DestroyItem(AIndex: Integer);
    procedure DestroyItems;
    function Remove(AEvent: TcxSchedulerEvent): Integer;
    function IndexOf(AEvent: TcxSchedulerEvent): Integer;
    procedure Sort(ACompare: TcxCompareEventsProc); virtual;
    //
    property Count: Integer read GetCount;
    property First: TcxSchedulerEvent read GetFirst;
    property Items[Index: Integer]: TcxSchedulerEvent read GetEvent; default;
    property Last: TcxSchedulerEvent read GetLast;
    property List: TdxFastList read FItems write FItems;
  end;

  { TcxSchedulerFilteredEventList }

  TcxSchedulerFilteredEventList = class(TcxSchedulerEventList)
  private
    FExceptEventsWithoutResources: Boolean;
    FFinish: TDateTime;
    FNow: TDateTime;
    FReminderEventsOnly: Boolean;
    FStart: TDateTime;
    FStorage: TcxCustomSchedulerStorage;
    FUseTimeRange: Boolean;
    function GetEvent(AIndex: Integer): TcxSchedulerControlEvent;
  protected
    PostponedInitialization: Boolean;
    function AddEvent(AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent;
    function AddOccurrence(ACalculator: TcxSchedulerOccurrenceCalculator): TcxSchedulerControlEvent;
    function CanAddEvent(ASource: TcxSchedulerEvent; ARecurrenceIndex: Integer): Boolean; virtual;
    function CanSmartUpdate(AStorage: TcxCustomSchedulerStorage;
      const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean; virtual;
    procedure Changed; virtual;
    procedure CheckEvent(AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean); virtual;
    function CheckResourceID(const AEventResourceID, AResourceID: Variant): Boolean;
    procedure CheckRecurrenceEvent(AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
    procedure CheckSimpleEvent(AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
    function CreateControlEvent(AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent; virtual;
    function GetTimeBias: Double; virtual;
    function GetTimeBiasDaylightSavingTime(ATime: TDateTime): TDateTime; virtual;
    procedure Init(const AStart, AFinish: TDateTime; AStorage: TcxCustomSchedulerStorage; AClearItems: Boolean); virtual;
    function NeedAddOccurrenceForReminder(ACalculator: TcxSchedulerOccurrenceCalculator): Boolean;
    procedure PostInitializeEvent(AEvent: TcxSchedulerControlEvent; ACheckType: Boolean);
    procedure SetCheckRange(AStart, AFinish: Integer); virtual;
    procedure SetUseTimeRange(AValue: Boolean); virtual;

    property UseTimeRange: Boolean read FUseTimeRange write SetUseTimeRange;
    property Now: TDateTime read FNow;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure ValidateTimeBias(AEvent: TcxSchedulerControlEvent);
    property ExceptEventsWithoutResources: Boolean read FExceptEventsWithoutResources write FExceptEventsWithoutResources;
    property Finish: TDateTime read FFinish;
    property ReminderEventsOnly: Boolean read FReminderEventsOnly;
    property Start: TDateTime read FStart;
    property Storage: TcxCustomSchedulerStorage read FStorage;
    property Items[AIndex: Integer]: TcxSchedulerControlEvent read GetEvent; default;
    property Count;
  end;

  { TcxSchedulerCachedEventList }

  TcxSchedulerCachedEventList = class(TcxSchedulerFilteredEventList, IUnknown,
    IcxSchedulerSelectionAdapter,
    IcxSchedulerStorageListener,
    IcxSchedulerStorageListener2)
  private
    FAbsoluteItems: TdxFastList;
    FAlwaysShowSelectedEvent: Boolean;
    FClones: TcxSchedulerFilteredEventList;
    FDaylightSaving: Boolean;
    FDictionary: TDictionary<TcxSchedulerEvent, Boolean>;
    FNewEvent: TcxSchedulerEvent;
    FSelStart: TDateTime;
    FSelFinish: TDateTime;
    FSelection: TcxSchedulerEventSelection;
    FShowEventsWithoutResource: Boolean;
    FTimeBias: Double;
    FTimeZone: Integer;
    function GetAbsoluteCount: Integer;
    function GetAbsoluteItem(AIndex: Integer): TcxSchedulerControlEvent;
    function GetAbsoluteCountInternal: Integer;
    function GetAbsoluteItemInternal(AIndex: Integer): TcxSchedulerControlEvent;
    function GetItem(AIndex: Integer): TcxSchedulerControlEvent;
    function GetShowEventsWithoutResource: Boolean;
    procedure SetSelFinish(const AValue: TDateTime);
    procedure SetSelStart(const AValue: TDateTime);
    procedure SetShowEventsWithoutResource(const AValue: Boolean);
    // IcxSchedulerSelectionAdapter
    procedure IcxSchedulerSelectionAdapter.Add = SelectionAdd;
    procedure IcxSchedulerSelectionAdapter.Clear = SelectionClear;
    procedure IcxSchedulerSelectionAdapter.Update = SelectionUpdate;
  protected
    FResourceID: Variant;
    FBeforePostCount: Integer;
    FChangeRef: Integer;
    FCheckStart: Integer;
    FCheckFinish: Integer;
    FExpandedTimeRange: Boolean;
    FHasClones: Boolean;
    FMaxNecessaryDate: TDateTime;
    FMinNecessaryDate: TDateTime;
    FNeedForceUpdate: Boolean;
    // IUnknown
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    // IcxSchedulerStorageListener2
    procedure AddEvent(AEvent: TcxSchedulerEvent);
    procedure RemoveEvent(AEvent: TcxSchedulerEvent);
    procedure StorageChanged(Sender: TObject);
    procedure StorageRemoved(Sender: TObject);
    //
    function CanAddEvent(ASource: TcxSchedulerEvent; ARecurrenceIndex: Integer): Boolean; override;
    function CanSmartUpdate(AStorage: TcxCustomSchedulerStorage;
      const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean; override;
    procedure Changed; override;
    procedure CheckEvent(AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean); override;
    function CheckEventVisibility(AEvent: TcxSchedulerEvent; AIncludeUnassigned: Boolean): Boolean;
    function CreateCloneList: TcxSchedulerFilteredEventList; virtual;
    function CreateSelection: TcxSchedulerEventSelection; virtual;
    procedure DestroyEvent(AEvent: TcxSchedulerEvent); override;
    procedure DoClear(AFreeItems: Boolean); override;
    function GetTimeBias: Double; override;
    function GetTimeBiasDaylightSavingTime(ATime: TDateTime): TDateTime; override;
    procedure EventAdded(AEvent: TcxSchedulerEvent); override;
    procedure Init(const AStart, AFinish: TDateTime; AStorage: TcxCustomSchedulerStorage; AClearItems: Boolean); override;
    procedure InternalPost(AEvent: TcxSchedulerControlEvent; ACopy: Boolean);
    function IsDayNoneEmpty(ADay: Integer): Boolean;
    function IsIntersect(AEvent1, AEvent2: TcxSchedulerEvent): Boolean;
    function IsSelected(AEvent: TcxSchedulerControlEvent): Boolean; virtual;
    procedure PopulateAbsoluteItems;
    procedure PostCloneForRecurrenceEvent(AEvent: TcxSchedulerControlEvent; ACopy: Boolean); virtual;
    function PostCloneForSimpleEvent(AEvent: TcxSchedulerControlEvent;
      ACopy: Boolean; AType: TcxEventType): TcxSchedulerEvent; virtual;
    function PostNewEvent(AEvent: TcxSchedulerControlEvent): TcxSchedulerEvent;
    procedure SelectionAdd(AEvent: TcxSchedulerControlEvent; Shift: TShiftState); virtual;
    procedure SelectionClear; virtual;
    procedure SelectionUpdate; virtual;
    procedure SetCheckRange(AStart, AFinish: Integer); override;
    procedure SetUseTimeRange(AValue: Boolean); override;
 public
    constructor Create; override;
    destructor Destroy; override;
    function Add(AEvent: TcxSchedulerEvent): Integer; override;
    procedure AfterEditing(AEvent: TcxSchedulerControlEvent);
    procedure BeforeEditing(AEvent: TcxSchedulerControlEvent; AIsInplace: Boolean);
    procedure BeforeUpdate;
    procedure CalculateClonesRange(var AMinDate, AMaxDate: TDateTime);
    function CalculateNecessaryDate(var AMinDate, AMaxDate: TDateTime): Boolean;
    procedure CalculateSelectionRange(var AMinDate, AMaxDate: TDateTime);
    procedure CancelClones;
    function CreateClones: TcxSchedulerEventList;
    function CreateEvent(AInsertToList: Boolean): TcxSchedulerControlEvent;
    procedure DeleteClone(AClone: TcxSchedulerControlEvent);
    procedure DeleteEvent(AEvent: TcxSchedulerControlEvent);
    procedure ExtractEvents(const ADate: TDateTime; AList: TcxSchedulerEventList);
    procedure ExtractEventsByResource(AList: TcxSchedulerEventList;
      const AResource: TcxSchedulerStorageResourceItem; AllowUnassigned: Boolean);
    procedure ExtractUsedDays(AList: TcxSchedulerDateList);
    function HasConflict(IsDragCopy: Boolean; AStartDrag: Boolean): Boolean;
    function HasIntersection(AEvent: TcxSchedulerControlEvent): Boolean; overload;
    function HasIntersection(AList: TList): Boolean; overload;
    function HasIntersection(AList1, AList2: TcxSchedulerEventList; AExcludeEquals: Boolean): Boolean; overload;
    procedure PostClones(ACopy: Boolean);
    function PostEvent(AEvent: TcxSchedulerControlEvent): TcxSchedulerEvent;
    procedure Sort(ACompare: TcxCompareEventsProc); override;
    //
    property AbsoluteCount: Integer read GetAbsoluteCount;
    property AbsoluteItems[Index: Integer]: TcxSchedulerControlEvent read GetAbsoluteItem;
    property AbsoluteItemsList: TdxFastList read FAbsoluteItems;
    property AlwaysShowSelectedEvent: Boolean read FAlwaysShowSelectedEvent write FAlwaysShowSelectedEvent;
    property TimeZone: Integer read FTimeZone write FTimeZone;
    property Clones: TcxSchedulerFilteredEventList read FClones;
    property DaylightSaving: Boolean read FDaylightSaving write FDaylightSaving;
    property NeedForceUpdate: Boolean read FNeedForceUpdate write FNeedForceUpdate;
    property ShowEventsWithoutResource: Boolean read GetShowEventsWithoutResource write SetShowEventsWithoutResource;
    property SelStart: TDateTime read FSelStart write SetSelStart;
    property SelFinish: TDateTime read FSelFinish write SetSelFinish;
    property Selection: TcxSchedulerEventSelection read FSelection;
    property Items[Index: Integer]: TcxSchedulerControlEvent read GetItem; default;
    property UseTimeRange;
  end;

  { TcxSchedulerOccurrenceCalculator }

  TcxSchedulerOccurrenceCalculator = class(TcxSchedulerRecurrenceCalculator)
  private
    FCalcForReminders: Boolean;
    FReminderAdvanceTime: TDateTime;

    function GetEvent: TcxSchedulerEvent;
    function GetReminderStart: TDateTime;
  protected
    FOccurrence: TcxSchedulerControlEvent;
    FOccurrencePos: Integer;

    function GetCalcForReminders: Boolean; override;
    function GetDuration: TDateTime; override;
    function GetRecurrenceInfo: TcxSchedulerCustomRecurrenceInfo; override;
    function GetStart: TDateTime; override;
    function GetTimeBias: Double; override;
    procedure InitTimes; override;
  public
    constructor Create(AEvent: TcxSchedulerEvent; const AStart, AFinish: TDateTime;
      ACalcForReminders: Boolean = False); reintroduce;
    procedure CalcNearestOccurrenceIntervals(AStart, AFinish: TDateTime;
      var AnIntervalBefore, AnIntervalAfter: TDateTime);
    function GetOccurrenceCount(AEndDate: TDateTime): Integer;
    function ValidateTimeBoundsForOccurrence(AEvent: TcxSchedulerEvent;
      AOldStart, AOldFinish, ANewStart, ANewFinish: TDateTime): TcxOccurrenceDateStatus;

    property Event: TcxSchedulerEvent read GetEvent;
    property DismissDate: TDateTime read FDismissDate;
    property ReminderStart: TDateTime read GetReminderStart;
  end;

  { TcxSchedulerContentNavigationInfo }

  TcxSchedulerContentNavigationInfo = class
  public
    FResourceID: Variant;
    FIntervalBefore: TDateTime;
    FIntervalAfter: TDateTime;
    constructor Create(AResourceID: Variant);
  end;

  { TcxSchedulerContentNavigationCalculator }

  TcxSchedulerContentNavigationCalculator = class
  private
    class procedure CalcAppointmentIntervals(AnEvent: TcxSchedulerEvent;
      AInfo: TcxSchedulerContentNavigationInfo; AStart, AFinish, AScaleUnit: TDateTime);
    class procedure CalcIntervals(AnEvent: TcxSchedulerEvent;
      AInfo: TcxSchedulerContentNavigationInfo; AStart, AFinish, AScaleUnit: TDateTime);
    class procedure CalcPatternIntervals(AnEvent: TcxSchedulerEvent;
      AInfo: TcxSchedulerContentNavigationInfo; AStart, AFinish: TDateTime);
    class procedure ChangeResourceNavigationIntervals(
      AContentNavigationInfo: TObjectList);
    class function IsEventSharedWithAnyResource(AStorage: TcxCustomSchedulerStorage;
      AnEventIndex: Integer; ConsiderVisibility: Boolean): Boolean;
  public
    class procedure FindNavigationIntervals(AStorage: TcxCustomSchedulerStorage;
      AContentNavigationInfo: TObjectList; AStart, AFinish: TDateTime;
      AWithoutResources: Boolean; AShowEventsWithoutResource: Boolean; AScaleUnit: TDateTime);
  end;

  { TcxSchedulerCustomReminderForm }

  TcxSchedulerCustomReminderForm = class(TdxForm)
  private
    FReminders: TcxSchedulerReminders;
    function GetStorage: TcxCustomSchedulerStorage;
  protected
    procedure CheckFormPosition; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DoClose(var Action: TCloseAction); override;
    procedure DoShow; override;
    procedure FlashCaption;
    function GetFormColor: TColor; virtual;
    function IsLocked: Boolean; virtual;
    procedure LayoutChanged; virtual;
    procedure OpenEvent(AEvent: TcxSchedulerControlEvent); virtual;
    function OpenEventSupported: Boolean; virtual;
    procedure RestoreSelection; virtual;
    procedure SaveSelection; virtual;
    procedure ShowInactive;
    procedure UpdateReminderList; virtual;
    procedure UpdateSelectionCaptions; virtual;

    property Reminders: TcxSchedulerReminders read FReminders;
    property Storage: TcxCustomSchedulerStorage read GetStorage;
  public
    constructor Create(AReminders: TcxSchedulerReminders); reintroduce; virtual;
    destructor Destroy; override;
  end;

  TcxSchedulerReminderFormClass = class of TcxSchedulerCustomReminderForm;

  { TcxSchedulerReminderEventID }

  TcxSchedulerReminderEventID = class(TcxSchedulerControlEventID)
  protected
    Values: array of Variant;
  public
    constructor Create(AEvent: TcxSchedulerControlEvent);
    destructor Destroy; override;
    function SameEvent(AEvent: TcxSchedulerControlEvent): Boolean;
  end;

  { TcxSchedulerEventReminders }

  TcxSchedulerEventReminders = class
  private
    FInvalid: Boolean;
  public
    EventID: TcxSchedulerReminderEventID;
    Reminders: array of TcxSchedulerReminder;
    constructor Create(AEvent: TcxSchedulerControlEvent);
    destructor Destroy; override;
    procedure AddReminder(AReminder: TcxSchedulerReminder);
    function Find(AReminderDate: TDateTime; const AResourceID: Variant): TcxSchedulerReminder;
    procedure FreeReminderByIndex(AIndex: Integer);
    procedure FreeReminders;
    procedure Invalidate;
    function IsValid: Boolean;
    procedure RemoveInvalidReminders;
    procedure Validate;
  end;

  { TcxSchedulerEventRemindersList }

  TcxSchedulerEventRemindersList = class(TcxObjectList)
  private
    function GetItem(AIndex: Integer): TcxSchedulerEventReminders; inline;
  public
    function CreateEventReminders(AEvent: TcxSchedulerControlEvent): TcxSchedulerEventReminders;
    function FindForEvent(AEvent: TcxSchedulerControlEvent): TcxSchedulerEventReminders;
    procedure Invalidate;
    procedure Remove(AEventReminders: TcxSchedulerEventReminders);
    procedure RemoveInvalidItems;

    property Items[Index: Integer]: TcxSchedulerEventReminders read GetItem;
  end;

  { TcxSchedulerReminder }

  TcxSchedulerReminder = class
  private
    FEvent: TcxSchedulerControlEvent;
    FEventReminders: TcxSchedulerEventReminders;
    FOwner: TcxSchedulerReminders;
    FReminderDate: TDateTime;
    FResourceID: Variant;
    FInvalid: Boolean;
    function GetResourcesData: TcxSchedulerReminderResourcesData;
  protected
    procedure ClearReminder;
    procedure DismissEvent; virtual;
    procedure DismissEventForResource; virtual;
    procedure DoDismiss; virtual;
    procedure DoSnooze(var ASnoozeTime: TDateTime); virtual;
    function GetRecurrenceDismissDate: TDateTime; virtual;
    procedure SetData(const AResourceID: Variant; AReminderDate: TDateTime);
    procedure SnoozeEvent(const ASnoozeTime: TDateTime); virtual;
    procedure SnoozeEventForResource(const ATime: TDateTime); virtual;
    procedure Validate(AEvent: TcxSchedulerControlEvent);

    property EventReminders: TcxSchedulerEventReminders read FEventReminders;
    property Invalid: Boolean read FInvalid write FInvalid;
  public
    constructor Create(AOwner: TcxSchedulerReminders;
      AEvent: TcxSchedulerControlEvent); virtual;
    destructor Destroy; override;
    procedure Dismiss;
    procedure Snooze(ASnoozeTime: TDateTime);
    property Event: TcxSchedulerControlEvent read FEvent;
    property Owner: TcxSchedulerReminders read FOwner;
    property ResourceID: Variant read FResourceID;
    property ReminderDate: TDateTime read FReminderDate;
  end;

  { TcxSchedulerReminderList }

  TcxSchedulerReminderList = class(TList)
  private
    FClearing: Boolean;
    function GetItem(Index: Integer): TcxSchedulerReminder; inline;
  public
    procedure Clear; override;

    property Clearing: Boolean read FClearing;
    property Items[Index: Integer]: TcxSchedulerReminder read GetItem; default;
  end;

  { TcxSchedulerReminders }

  TcxSchedulerReminderEvent = procedure (Sender: TcxSchedulerReminders;
    AReminder: TcxSchedulerReminder; var AHandled: Boolean) of object;
  TcxSchedulerRemindersEvent = procedure (Sender: TcxSchedulerReminders) of object;
  TcxSchedulerGetEventDueTimeTextEvent = procedure (
    Sender: TcxSchedulerReminders; AReminder: TcxSchedulerReminder;
    const ADueTimeInfo: TcxSchedulerReminderDueTimeInfo; var AText: string) of object;
  TcxSchedulerGetReminderWindowCaption = procedure (Sender: TcxSchedulerReminders;
    var ACaption: string) of object;
  TcxSchedulerSnoozeReminderEvent = procedure (Sender: TcxSchedulerReminders;
    AReminder: TcxSchedulerReminder; var ASnoozeTime: TDateTime; var AHandled: Boolean) of object;
  TcxSchedulerReminderOpenEvent = procedure (Sender: TcxSchedulerReminders;
    AEvent: TcxSchedulerControlEvent) of object;

  TcxSchedulerReminderEventsList = class(TcxSchedulerFilteredEventList)
  private
    FReminders: TcxSchedulerReminders;
  protected
    function CreateControlEvent(AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent; override;
    function GetTimeBias: Double; override;
    function GetTimeBiasDaylightSavingTime(ATime: TDateTime): TDateTime; override;

    property Reminders: TcxSchedulerReminders read FReminders;
  public
    constructor Create(AOwner: TcxSchedulerReminders); reintroduce; overload;
  end;

  TcxSchedulerReminders = class(TcxInterfacedPersistent,
    IcxFormatControllerListener,
    IcxFormatControllerListener2,
    IdxSkinSupport)
  private
    FActive: Boolean;
    FDefaultMinutesBeforeStart: Integer;
    FDefaultReminder: Boolean;
    FEvents: TcxSchedulerFilteredEventList;
    FEventRemindersList: TcxSchedulerEventRemindersList;
    FItems: TcxSchedulerReminderList;
    FLockCount: Integer;
    FReminderWindowLookAndFeel: TcxLookAndFeel;
    FNeedCloseWindow: Boolean;
    FNeedRestoreSelection: Boolean;
    FNeedShowWindow: Boolean;
    FReminderByResource: Boolean;
    FReminderWindow: TcxSchedulerCustomReminderForm;
    FShowResourcesInReminderWindow: Boolean;
    FStorage: TcxCustomSchedulerStorage;
    FTimeZone: Integer;
    FTimeZoneDaylightSaving: Boolean;
    FUpdateEventsTimer: TTimer;
    FUpdateRemindersTimer: TTimer;
    FUpdatingReminderList: Boolean;
    FUseReminderWindow: Boolean;
    FOnAlertReminder: TcxSchedulerReminderEvent;
    FOnDismissReminder: TcxSchedulerReminderEvent;
    FOnGetEventDueTimeText: TcxSchedulerGetEventDueTimeTextEvent;
    FOnOpenEvent: TcxSchedulerReminderOpenEvent;
    FOnSnoozeReminder: TcxSchedulerSnoozeReminderEvent;
    //reminder window events
    FOnGetReminderWindowCaption: TcxSchedulerGetReminderWindowCaption;
    FOnHideReminderWindow: TcxSchedulerRemindersEvent;
    FOnShowReminderWindow: TcxSchedulerRemindersEvent;
    procedure CreateTimers;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxSchedulerReminder;
    function GetTimeZone: Integer;
    procedure SetActive(AValue: Boolean);
    procedure SetDefaultMinutesBeforeStart(AValue: Integer);
    procedure SetReminderByResource(AValue: Boolean);
    procedure SetReminderWindowLookAndFeel(AValue: TcxLookAndFeel);
    procedure SetShowResourcesInReminderWindow(AValue: Boolean);
    procedure SetTimeZone(AValue: Integer);
    procedure StartReminder;
    procedure StopReminder;
    procedure UpdateReminderWindow;
    procedure OnUpdateEventsTimer(Sender: TObject);
    procedure OnUpdateRemindersTimer(Sender: TObject);
  protected
    //IcxFormatControllerListener
    procedure FormatChanged;
    //IcxFormatControllerListener2
    procedure TimeChanged;

    procedure AddReminder(AEvent: TcxSchedulerControlEvent;
      const AResourceID: Variant; AReminderDate: TDateTime;
      var AEventReminders: TcxSchedulerEventReminders);
    procedure AddReminderToEventReminders(AReminder: TcxSchedulerReminder;
      var AEventReminders: TcxSchedulerEventReminders);

    procedure CheckAddReminder(AEvent: TcxSchedulerControlEvent;
      const AResourceID: Variant; AReminderDate: TDateTime;
      var AEventReminders: TcxSchedulerEventReminders);
    procedure CheckAddReminders(AEvent: TcxSchedulerControlEvent; AEventReminders: TcxSchedulerEventReminders);
    procedure CheckRemindersForEvent(AEvent: TcxSchedulerControlEvent);
    function CreateReminderWindow: TcxSchedulerCustomReminderForm; virtual;
    function DoAlertReminder(AReminder: TcxSchedulerReminder): Boolean; virtual;
    function DoDismissReminder(AReminder: TcxSchedulerReminder): Boolean; virtual;
    procedure DoGetEventDueTimeText(AReminder: TcxSchedulerReminder;
      const ADueTimeInfo: TcxSchedulerReminderDueTimeInfo; var AText: string);
    procedure DoGetReminderWindowCaption(var ACaption: string); virtual;
    procedure DoOpenEvent(AEvent: TcxSchedulerControlEvent); virtual;
    function DoSnoozeReminder(AReminder: TcxSchedulerReminder; var ASnoozeTime: TDateTime): Boolean; virtual;
    procedure DoHideReminderWindow; virtual;
    procedure DoShowReminderWindow; virtual;

    function GetFirstReminderTimeForEvent(AEvent: TcxSchedulerEvent; ANow: TDateTime): TDateTime;
    function GetNextPopulateEventsCacheTime(ANow: TDateTime; AIsLocked: Boolean): TDateTime; virtual;
    function GetNextRefreshRemindersTime(ANow: TDateTime; AIsLocked: Boolean): TDateTime; virtual;
    function GetNow: TDateTime; virtual;
    function GetReminderClass: TcxSchedulerReminderClass; virtual;
    function GetReminderResourcesFromEvent(AEvent: TcxSchedulerControlEvent; AGetFromPattern: Boolean): TcxSchedulerReminderResources;
    function GetReminderWindowCaption: string;

    function IsNeedAddToReminders(AEvent: TcxSchedulerControlEvent;
       const AReminderDate: TDateTime): Boolean; virtual;
    function PopulateEventsCache: Boolean; virtual;
    procedure RefreshReminders; virtual;
    procedure RemoveEvent(AEvent: TcxSchedulerEvent);
    procedure RemoveEventReminders(AReminder: TcxSchedulerReminder);
    procedure StopTimers; virtual;
    procedure UpdateReminderList; virtual;
    procedure UpdateTimer(ATimer: TTimer; AInterval: TDateTime);
    procedure ValidateReminderList;
    procedure CheckRemindersForRecurringEvent(AEvent: TcxSchedulerControlEvent; var AIndex: Integer);
    procedure ValidateRemindersForEvent(AEventReminders: TcxSchedulerEventReminders;
      AEvent: TcxSchedulerControlEvent);

    property Events: TcxSchedulerFilteredEventList read FEvents;
    property NeedCloseWindow: Boolean read FNeedCloseWindow;
    property NeedRestoreSelection: Boolean read FNeedRestoreSelection write FNeedRestoreSelection;
    property NeedShowWindow: Boolean read FNeedShowWindow;
    property UpdateEventsTimer: TTimer read FUpdateEventsTimer;
    property UpdateRemindersTimer: TTimer read FUpdateRemindersTimer;
    property UpdatingReminderList: Boolean read FUpdatingReminderList;
  public
    constructor Create(AStorage: TcxCustomSchedulerStorage); reintroduce; virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure BeginUpdate;
    procedure EndUpdate;

    procedure DismissAll;
    procedure SnoozeAll(ASnoozeTime: TDateTime);

    function GetEventDueTimeText(AReminder: TcxSchedulerReminder;
      ATime: TDateTime): string;
    function HasReminders: Boolean;
    procedure HideReminderWindow;
    function IsLocked: Boolean; virtual;
    function IsReminderByResourceAvailable: Boolean;
    function IsReminderValid(AReminder: TcxSchedulerReminder): Boolean;
    function IsReminderWindowShown: Boolean;
    procedure Refresh;
    procedure ShowReminderWindow;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxSchedulerReminder read GetItem; default;
    property ReminderWindow: TcxSchedulerCustomReminderForm read FReminderWindow;
    property Storage: TcxCustomSchedulerStorage read FStorage;
  published
    property Active: Boolean read FActive write SetActive default True;
    property DefaultMinutesBeforeStart: Integer
      read FDefaultMinutesBeforeStart write SetDefaultMinutesBeforeStart default 15;
    property DefaultReminder: Boolean read FDefaultReminder write FDefaultReminder default False;
    property ReminderByResource: Boolean read FReminderByResource write SetReminderByResource default False;
    property ReminderWindowLookAndFeel: TcxLookAndFeel read FReminderWindowLookAndFeel write SetReminderWindowLookAndFeel;
    property ShowResourcesInReminderWindow: Boolean read FShowResourcesInReminderWindow write SetShowResourcesInReminderWindow default True;
    property TimeZone: Integer read GetTimeZone write SetTimeZone default -1;
    property TimeZoneDaylightSaving: Boolean read FTimeZoneDaylightSaving write FTimeZoneDaylightSaving default False;
    property UseReminderWindow: Boolean read FUseReminderWindow write FUseReminderWindow default True;
    // events
    property OnAlertReminder: TcxSchedulerReminderEvent
      read FOnAlertReminder write FOnAlertReminder;
    property OnDismissReminder: TcxSchedulerReminderEvent
      read FOnDismissReminder write FOnDismissReminder;
    property OnOpenEvent: TcxSchedulerReminderOpenEvent
      read FOnOpenEvent write FOnOpenEvent;
    property OnSnoozeReminder: TcxSchedulerSnoozeReminderEvent
      read FOnSnoozeReminder write FOnSnoozeReminder;

    property OnHideReminderWindow: TcxSchedulerRemindersEvent
      read FOnHideReminderWindow write FOnHideReminderWindow;
    property OnShowReminderWindow: TcxSchedulerRemindersEvent
      read FOnShowReminderWindow write FOnShowReminderWindow;

    property OnGetEventDueTimeText: TcxSchedulerGetEventDueTimeTextEvent
      read FOnGetEventDueTimeText write FOnGetEventDueTimeText;
    property OnGetReminderWindowCaption: TcxSchedulerGetReminderWindowCaption
      read FOnGetReminderWindowCaption write FOnGetReminderWindowCaption;
  end;

function cxGetEventID(AEvent: TcxSchedulerEvent): Variant;
procedure cxCheckDaysList(AList: TList; var AStartDay, AFinishDay: Integer);
function cxGetRecurrenceDescriptionString(ARecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
  AFullDescription: Boolean = False): string;
function cxMinutesToDueTimeInfo(AMinutes: Integer): TcxSchedulerReminderDueTimeInfo;
function cxDueTimeInfoToText(const AInfo: TcxSchedulerReminderDueTimeInfo): string;

function cxCompareSchedulerControlEvents(
  AEvent1, AEvent2: TcxSchedulerControlEvent): Integer;

function cxCompareSelectionKeys(AItem1, AItem2: TcxSchedulerControlEventID): Integer;

function cxFieldValueToVariant(const AValue: Variant): Variant;
function cxVariantToFieldValue(const AValue: Variant; ABLOBFormat: Boolean = False): Variant;
function cxReminderResourcesDataToFieldValue(const AValue: TcxSchedulerReminderResourcesData): Variant;
function cxFieldValueToReminderResourcesData(const AValue: Variant): TcxSchedulerReminderResourcesData;
function cxGetOccurrenceDateStatusMessage(AEvent: TcxSchedulerEvent; AStatus: TcxOccurrenceDateStatus): string;

function cxTaskLinksToFieldValue(ALinks: TcxSchedulerEventLinks): Variant;
procedure cxFieldValueToTaskLinks(const AValue: Variant; ALinks: TcxSchedulerEventLinks);

function GetStreamVersion(const AStream: TStream;
  var AFieldCount: Integer; ACustomFieldCount: Integer = 0): Double;

const
  cxGetRecurrenceDescriptionStringProc: TcxGetRecurrenceDescriptionStringProc =
    cxGetRecurrenceDescriptionString;
  cxDueTimeInfoToTextProc: TcxDueTimeInfoToTextProc = cxDueTimeInfoToText;
  cxSchedulerEventConflictsInfoClass: TcxSchedulerEventConflictsInfoClass =
    TcxSchedulerEventConflictsInfo;

  cxMaxCheckedDuration: Integer = 30;// div 2; todo:
  cxOriginalEventStates: array[Boolean] of Byte = (tlsBusy, tlsFree);

implementation

uses
  cxSchedulerStrs, cxSchedulerDialogs, cxSchedulerReminderWindow, Messages, AnsiStrings,
  cxSchedulerEditorFormManager, cxSchedulerCustomControls, cxControls;

type
  TFlashWindowExProc = function (var pfwi: FLASHWINFO): BOOL; stdcall;
  PRestoringItem = ^TRestoringItem;
  TRestoringItem = record
    Item: TcxSchedulerStorageResourceItem;
    Index: Integer;
    Visible: Boolean;
    WorkDays: TDays;
    WorkFinish: TTime;
    WorkStart: TTime;
  end;

const
  PatternValidStatus: array[Boolean] of TcxRecurrenceValidStatus =
    (rvsInvalidPattern, rvsValid);
  cxSchedulerEventDefaultParentID = -2;

  FlashWindowExProc: TFlashWindowExProc = nil;

  scxSchedulerSignature = 'dxScheduler';
  cxSchedulerStreamVersion: Double = 2011.2;
  cxRecurrenceInfoDataVersion    = 1;
  cxReminderResourcesDataVersion = 3;

  cxSchedulerFieldCountVersion1 = 15;
  cxSchedulerFieldCountVersion2 = 17;
  cxSchedulerFieldCountVersion3 = 22;

function cxSchedulerStorageFieldsCompare(AItem1, AItem2: TcxCustomSchedulerStorageField): Integer;
begin
  Result := AItem1.FIndex - AItem2.FIndex;
end;

function WorkDaysToInteger(ADay: TDays): Integer;
var
  AIndex: TDay;
begin
  Result := 0;
  for AIndex := Low(TDay) to High(TDay) do
    if AIndex in ADay then
      Result := Result or (1 shl Integer(AIndex));
end;

function IntegerToWorkDays(AValue: Integer): TDays;
var
  AIndex: TDay;
begin
  Result := [];
  for AIndex := Low(TDay) to High(TDay) do
    if (AValue shr Integer(AIndex)) and 1 <> 0 then
      Result := Result + [AIndex]
end;

function VarEqualsSoftEx(const V1, V2: Variant): Boolean;
begin
  Result := not VarIsEmpty(V1) and not VarIsEmpty(V2) and
    (VarEquals(V1, V2){ or VarIsNull(V1) or VarIsNull(V2)});
end;

function VarIsEmptyEx(const V: Variant): Boolean;
begin
  Result := VarIsEmpty(V) or VarIsNull(V) or (VarIsStr(V) and (V = ''))
end;

function cxIntSortProc(P1, P2: Pointer): Integer;
begin
  Result := TdxNativeInt(P1) - TdxNativeInt(P2);
end;

function cxCompareSelectionKeys(AItem1, AItem2: TcxSchedulerControlEventID): Integer;
const
  AWeight: array[TcxEventType] of SmallInt = (-1, 1, 1, 1, 1);
begin
  Result := AWeight[AItem1.EventType] - AWeight[AItem2.EventType];
  if Result <> 0 then
    Exit;
  if AItem1.EventType = etNone then
    Exit(VarCompare(AItem1.ID, AItem2.ID));
  Result := VarCompare(AItem1.PatternID, AItem2.PatternID);
  if Result <> 0 then
    Exit;
  if VarIsNull(AItem1.PatternID) then
    Result := VarCompare(AItem1.ParentID, AItem2.ParentID);
  if Result = 0 then
  begin
    if (VarIsNull(AItem1.PatternID) and (VarIsNull(AItem2.PatternID) or VarEquals(AItem1.PatternID, cxSchedulerEventDefaultParentID))) or
       (VarIsNull(AItem2.PatternID) and (VarIsNull(AItem1.PatternID) or VarEquals(AItem2.PatternID, cxSchedulerEventDefaultParentID))) then
      Result := VarCompare(AItem1.ID, AItem2.ID);
    if Result = 0 then
      Result := AItem1.RecurrenceIndex - AItem2.RecurrenceIndex;
  end;
end;

function cxGetEventID(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := Null;
  if AEvent <> nil then
    Result := AEvent.ID;
end;

procedure cxCheckDaysList(AList: TList; var AStartDay, AFinishDay: Integer);
begin
  AList.Sort(cxIntSortProc);
  AStartDay := Integer(AList[0]);
  AFinishDay := Integer(AList[AList.Count - 1]);
end;

function cxGetRecurrenceDescriptionString(
  ARecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
  AFullDescription: Boolean = False): string;

  function GetTimeBounds(APattern: TcxSchedulerEvent): string;
  begin
    if APattern.AllDayEvent and (APattern.Duration = 1) then Exit;
    Result := ' from ' + FormatDateTime('t', APattern.Start - ARecurrenceInfo.DisplayTimeBias);
    if APattern.Duration < 1 then
      Result := Result + ' to ' + FormatDateTime('t', APattern.Finish - ARecurrenceInfo.DisplayTimeBias)
    else
      Result := Result + ' for ' + cxMinutesToTextProc(Round(APattern.Duration / MinuteToTime));
  end;

  function GetDateBounds: string;
  begin
    Result := ' effective ' + FormatDateTime('ddddd', ARecurrenceInfo.Start);
    if ARecurrenceInfo.Count >= 0 then
      Result := Result + ' until ' + FormatDateTime('ddddd', ARecurrenceInfo.GetEndDate);
  end;

var
  APattern: TcxSchedulerEvent;
begin
  Result := '';
  if not Assigned(ARecurrenceInfo) or not ARecurrenceInfo.Event.IsRecurring then
    Exit;
  Result := cxGetCustomRecurrenceDescriptionString(ARecurrenceInfo);
  if ARecurrenceInfo.Event.EventType = etPattern then
    APattern := ARecurrenceInfo.Event
  else
  begin
    APattern := ARecurrenceInfo.Event.Pattern;
    if APattern = nil then
      APattern := ARecurrenceInfo.Event;
  end;
  if AFullDescription then
    Result := 'Occurs ' + Result + GetDateBounds + GetTimeBounds(APattern) + '.'
  else
    Result := Result + GetTimeBounds(APattern);
end;

function cxMinutesToDueTimeInfo(AMinutes: Integer): TcxSchedulerReminderDueTimeInfo;
const
  MinsPerWeek = MinsPerDay * DaysPerWeek;
begin
  with Result do
  begin
    if AMinutes = 0 then
    begin
      DueKind := dtkNow;
      Minutes := 0;
      Element := dteMinute;
      ElementValue := 0;
      Exit;
    end;
    if AMinutes < 0 then
      DueKind := dtkOverdue
    else
      DueKind := dtkActual;
    AMinutes := Abs(AMinutes);
    Minutes := AMinutes;
    case AMinutes of
      1..MinsPerHour - 1:
        begin
          Element := dteMinute;
          ElementValue := AMinutes;
        end;
      MinsPerHour..MinsPerDay - 1:
        begin
          Element := dteHour;
          ElementValue := AMinutes div MinsPerHour;
        end;
      MinsPerDay..(MinsPerWeek * 2) - 1:
        begin
          Element := dteDay;
          ElementValue := AMinutes div MinsPerDay;
        end
      else
      begin
        Element := dteWeek;
        ElementValue := AMinutes div MinsPerWeek;
      end
    end;
  end;
end;

function cxDueTimeInfoToText(const AInfo: TcxSchedulerReminderDueTimeInfo): string;
const
  PluralPostfix: array[Boolean] of string = ('', 's');
  ElementNames: array[TcxSchedulerReminderDueTimeElement] of string  =
    ('minute', 'hour', 'day', 'week');
  DueInFormatTexts: array[Boolean] of string = ('%d %s%s', '%d %s%s overdue');
begin
  with AInfo do
  begin
    if DueKind = dtkNow then
      Result := 'Now'
    else
      Result := Format(DueInFormatTexts[DueKind = dtkOverdue],
        [ElementValue, ElementNames[Element], PluralPostfix[ElementValue > 1]]);
  end;
end;

function cxCompareSchedulerControlEvents(
  AEvent1, AEvent2: TcxSchedulerControlEvent): Integer;
var
  AAllDay1, AAllDay2: Boolean;
begin
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
          Result := AEvent1.Index - AEvent2.Index;
end;

{ TcxDoublyLinkedEventListData }

function TcxDoublyLinkedEventListData.GetEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(Data);
end;

function TcxDoublyLinkedEventListData.GetNext: TcxDoublyLinkedEventListData;
begin
  Result := TcxDoublyLinkedEventListData(inherited Next);
end;

function TcxDoublyLinkedEventListData.GetPrev: TcxDoublyLinkedEventListData;
begin
  Result := TcxDoublyLinkedEventListData(inherited Prev);
end;

procedure TcxDoublyLinkedEventListData.SetNext(const AValue: TcxDoublyLinkedEventListData);
begin
  inherited Next := AValue;
end;

procedure TcxDoublyLinkedEventListData.SetPrev(const AValue: TcxDoublyLinkedEventListData);
begin
  inherited Prev := AValue;
end;

{  TcxDoublyLinkedEventList }

function TcxDoublyLinkedEventList.Add(AEvent: TcxSchedulerEvent): TcxDoublyLinkedEventListData;
begin
  Result := TcxDoublyLinkedEventListData(inherited Add(Pointer(AEvent)))
end;

procedure TcxDoublyLinkedEventList.FreeEvents;
var
  ALast: TcxDoublyLinkedEventListData;
begin
  ALast := Last;
  try
    while ALast <> nil do
    begin
      ALast.Event.Free;
      ALast := ALast.Prev;
    end;
  finally
    Clear;
  end;
end;

function TcxDoublyLinkedEventList.GetLinkedObjectClass: TcxDoublyLinkedObjectClass;
begin
  Result := TcxDoublyLinkedEventListData;
end;

procedure TcxDoublyLinkedEventList.ForEach(
  AProc: TcxSchedulerForEachEventHandlerProc; AData: Pointer; AGoForward: Boolean = True);
var
  ALink: TcxDoublyLinkedEventListData;
begin
  if AGoForward then
  begin
    ALink := First;
    while ALink <> nil do
    begin
      AProc(ALink.Event, AData);
      ALink := ALink.Next;
    end;
  end
  else
  begin
    ALink := Last;
    while ALink <> nil do
    begin
      AProc(ALink.Event, AData);
      ALink := ALink.Prev;
    end;
  end;
end;

function TcxDoublyLinkedEventList.GetFirst: TcxDoublyLinkedEventListData;
begin
  Result := TcxDoublyLinkedEventListData(inherited First);
end;

function TcxDoublyLinkedEventList.GetLast: TcxDoublyLinkedEventListData;
begin
  Result := TcxDoublyLinkedEventListData(inherited Last);
end;


{ TcxSchedulerStorageDataController }

procedure TcxSchedulerStorageDataController.ChangeValueTypeClass(
  AItemIndex: Integer; AValueTypeClass: TcxValueTypeClass);
var
  AField: TcxCustomSchedulerStorageField;
begin
  AField := TcxCustomSchedulerStorageField(Storage.Fields[AItemIndex]);
  if AField.ValueTypeClass = AValueTypeClass then Exit;
  RemoveValueDef(AField);
  TcxCustomDataHelper.SetTextStored(Fields[AItemIndex], AField.GetIsTextStored);
  inherited ChangeValueTypeClass(AItemIndex, AValueTypeClass);
  InsertValueDef(AField);
end;

function TcxSchedulerStorageDataController.GetItem(Index: Integer): TObject;
begin
  Result := Storage.FFields.List[Index];
end;

function TcxSchedulerStorageDataController.GetItemValueSource(
  AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxSchedulerStorageDataController.GetItemID(AItem: TObject): Integer;
begin
  if AItem is TcxCustomSchedulerStorageField then
    Result := TcxCustomSchedulerStorageField(AItem).FIndex
  else
    Result := -1;
end;

procedure TcxSchedulerStorageDataController.UpdateData;
begin
  Storage.UpdateData;
end;

procedure TcxSchedulerStorageDataController.UpdateItemIndexes;
begin
  Storage.UpdateItemIndexes;
  inherited UpdateItemIndexes;
end;

procedure TcxSchedulerStorageDataController.ForEachEvent(
  AProc: TcxSchedulerForEachEventHandlerProc; AData: Pointer; AGoForward: Boolean);
var
  I: Integer;
begin
  if AGoForward then
    for I := 0 to Storage.EventCount - 1 do
      AProc(Storage.Events[I], AData)
  else
    for I := Storage.EventCount - 1 downto 0 do
      AProc(Storage.Events[I], AData);
end;

procedure TcxSchedulerStorageDataController.InitializeEventFromRecordIndex(
  AEvent: TcxSchedulerEvent; AData: Pointer);
var
  P: PByte;
begin
  AEvent.FIndex := Integer(AEvent.RecordHandle);
  AEvent.RecordHandle := TcxDataStorageHelper.RemoveRecord(
    DataStorage, Integer(AEvent.RecordHandle));
  P := PByte(AEvent.RecordHandle);
  Inc(P);
  if Storage.FEventIDCounter <= PInteger(P)^ then
    Storage.FEventIDCounter := PInteger(P)^ + 1;
  AEvent.ID := PInteger(P)^;
end;

procedure TcxSchedulerStorageDataController.InitializeEventHandles;
begin
  Storage.FEventIDCounter := 0;
  ForEachEvent(InitializeEventFromRecordIndex, nil, False);
end;

procedure TcxSchedulerStorageDataController.InitializeRecordIndexes;
begin
  ForEachEvent(InitializeRecordIndexFromEvent, nil, True);
end;

procedure TcxSchedulerStorageDataController.InitializeRecordIndexFromEvent(
  AEvent: TcxSchedulerEvent; AData: Pointer);
begin
  AEvent.RecordHandle := Pointer(TcxDataStorageHelper.AppendRecord(
    DataStorage, AEvent.RecordHandle));
end;

type
  PcxSchedulerFieldValueDefData = ^TcxSchedulerFieldValueDefData;
  TcxSchedulerFieldValueDefData = packed record
    Index: Integer;
    ValueDef: TcxValueDef;
  end;

procedure TcxSchedulerStorageDataController.InsertValueDef(
  AField: TcxCustomSchedulerStorageField);
var
  AData: TcxSchedulerFieldValueDefData;
begin
  if AField.ValueDef = nil then Exit;
  AData.Index := AField.ValueDef.ValueDefs.Count;
  AData.ValueDef := AField.ValueDef;
  ForEachEvent(InsertValueDefProc, @AData, True);
end;

procedure TcxSchedulerStorageDataController.InsertValueDefProc(
  AEvent: TcxSchedulerEvent; AData: Pointer);
var
  PSource: PAnsiChar;
begin
  if AEvent.FRecordHandle = nil then Exit;
  ReallocMem(AEvent.FRecordHandle, PcxSchedulerFieldValueDefData(AData)^.ValueDef.ValueDefs.RecordSize);
  PSource := PAnsiChar(AEvent.FRecordHandle) + PcxSchedulerFieldValueDefData(AData)^.ValueDef.Offset;
  FillChar(PSource^, PcxSchedulerFieldValueDefData(AData)^.ValueDef.BufferSize, 0);
end;

procedure TcxSchedulerStorageDataController.RemoveValueDef(
  AField: TcxCustomSchedulerStorageField);
var
  AData: TcxSchedulerFieldValueDefData;
begin
  AData.ValueDef := AField.ValueDef;
  ForEachEvent(RemoveValueDefProc, @AData, True);
  AField.FValueDef := nil;
end;

procedure TcxSchedulerStorageDataController.RemoveValueDefProc(
  AEvent: TcxSchedulerEvent; AData: Pointer);
var
  AFreeRecord: Boolean;
  ARecordSize: Integer;
  PDest, PSource: PAnsiChar;
begin
  if (AEvent.FRecordHandle = nil) or (PcxSchedulerFieldValueDefData(AData)^.ValueDef = nil) then Exit;
  AFreeRecord := TcxDataStorageHelper.GetStoredCount(DataStorage) <= 1;
  ARecordSize := TcxDataStorageHelper.GetRecordSize(DataStorage);
  if AFreeRecord then
    Storage.FreeRecord(AEvent.FRecordHandle)
  else
    with PcxSchedulerFieldValueDefData(AData)^ do
    begin
      TcxDataStorageHelper.FreeBuffer(ValueDef, AEvent.FRecordHandle);
      PDest := PAnsiChar(AEvent.FRecordHandle) + ValueDef.Offset;
      PSource := PAnsiChar(PDest) + ValueDef.BufferSize;
      System.Move(PSource^, PDest^, ARecordSize - (PSource - PAnsiChar(AEvent.FRecordHandle)));
      ReallocMem(AEvent.FRecordHandle, ARecordSize - ValueDef.BufferSize);
    end;
end;

procedure TcxSchedulerStorageDataController.UpdateControl(
  AInfo: TcxUpdateControlInfo);
begin
  Storage.UpdateControl(AInfo);
end;

function TcxSchedulerStorageDataController.UseRecordID: Boolean;
begin
  Result := True;
end;

function TcxSchedulerStorageDataController.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := TcxCustomSchedulerStorage(GetOwner)
end;

{ TcxCustomSchedulerStorageField }

constructor TcxCustomSchedulerStorageField.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  Storage.ItemAdded(Self);
  ValueTypeClass := TcxStringValueType;
end;

destructor TcxCustomSchedulerStorageField.Destroy;
begin
  Storage.ItemRemoved(Self);
  Collection := nil;
  inherited Destroy;
end;

procedure TcxCustomSchedulerStorageField.Assign(Source: TPersistent);
begin
  if Source is TcxCustomSchedulerStorageField then
    ValueTypeClass := TcxCustomSchedulerStorageField(Source).ValueTypeClass
  else
    inherited Assign(Source);
end;

function TcxCustomSchedulerStorageField.GetDisplayName: string;
begin
  Result := FName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TcxCustomSchedulerStorageField.GetIsActive: Boolean;
begin
  Result := True;
end;

function TcxCustomSchedulerStorageField.GetIsBlob: Boolean;
begin
  Result := False;
end;

function TcxCustomSchedulerStorageField.GetIsTextStored: Boolean;
begin
  Result := False;
end;

function TcxCustomSchedulerStorageField.IsValueTypeStored: Boolean;
begin
  Result := ValueTypeClass <> TcxStringValueType;
end;

function TcxCustomSchedulerStorageField.GetDataController: TcxSchedulerStorageDataController;
begin
  Result := Storage.DataController;
end;

function TcxCustomSchedulerStorageField.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := TcxCustomSchedulerStorageFields(Collection).Storage;
end;

function TcxCustomSchedulerStorageField.GetValue(
  AEvent: TcxSchedulerEvent): Variant;
begin
  Result := Storage.GetEventActualValue(AEvent, FIndex);
end;

function TcxCustomSchedulerStorageField.GetValueCount: Integer;
begin
  Result := Storage.EventCount;
end;

function TcxCustomSchedulerStorageField.GetValueDef: TcxValueDef;
begin
  if FValueDef = nil then
    FValueDef := TcxCustomDataHelper.GetValueDef(DataController, Index);
  Result := FValueDef;
end;

function TcxCustomSchedulerStorageField.GetValueType: string;
begin
  if ValueTypeClass = nil then
    Result := ''
  else
    Result := ValueTypeClass.Caption;
end;

function TcxCustomSchedulerStorageField.GetValueTypeClass: TcxValueTypeClass;
begin
  Result := Storage.DataController.GetItemValueTypeClass(Index);
  FValueDef := GetValueDef;
end;

procedure TcxCustomSchedulerStorageField.SetName(
  const AValue: string);
begin
  if FName <> AValue then
  begin
    FName := AValue;
    Changed(True);
  end;
end;

procedure TcxCustomSchedulerStorageField.SetValue(
  AEvent: TcxSchedulerEvent; const AValue: Variant);
begin
  Storage.SetEventValue(AEvent, FIndex, AValue);
end;

procedure TcxCustomSchedulerStorageField.SetValueType(
  const AValue: string);
begin
  if ValueType <> AValue then
    ValueTypeClass := cxValueTypeClassList.ItemByCaption(AValue);
end;

procedure TcxCustomSchedulerStorageField.SetValueTypeClass(
  AValue: TcxValueTypeClass);
begin
  DataController.ChangeValueTypeClass(Index, AValue);
end;

{ TcxSchedulerStorageField }

procedure TcxSchedulerStorageField.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerStorageField then
    Name := TcxSchedulerStorageField(Source).Name;
end;

{ TcxCustomSchedulerStorageFields }

function TcxCustomSchedulerStorageFields.FindFieldByName(const AName: string): TcxCustomSchedulerStorageField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TcxCustomSchedulerStorageField(inherited Items[I]);
    if AnsiSameText(AName, Result.Name) then Exit;
  end;
  Result := nil;
end;

function TcxCustomSchedulerStorageFields.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxCustomSchedulerStorageFields.Update(Item: TCollectionItem);
begin
  inherited Update(Item);
  Storage.UpdateItemIndexes;
  Storage.Changed;
end;

function TcxCustomSchedulerStorageFields.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := TcxCustomSchedulerStorage(FOwner);
end;

{ TcxSchedulerStorageFields }

function TcxSchedulerStorageFields.Add: TcxSchedulerStorageField;
begin
  Result := TcxSchedulerStorageField(inherited Add);
end;

function TcxSchedulerStorageFields.ItemByName(
  const AName: string): TcxSchedulerStorageField;
begin
  Result := TcxSchedulerStorageField(FindFieldByName(AName));
end;

function TcxSchedulerStorageFields.GetItem(
  AIndex: Integer): TcxSchedulerStorageField;
begin
  Result := TcxSchedulerStorageField(inherited Items[AIndex]);
end;

procedure TcxSchedulerStorageFields.SetItem(
  AIndex: Integer; AValue: TcxSchedulerStorageField);
begin
  inherited Items[AIndex].Assign(AValue);
end;

{ TcxSchedulerEventRecurrenceInfo }

function TcxSchedulerEventRecurrenceInfo.GetValidStatus: TcxRecurrenceValidStatus;

  function IntersectOccurrences(APrevFinish, AStart: TDateTime): Boolean;
  begin
    if Event.AllDayEvent then
      Result := dxDateOf(AStart) < dxDateOf(APrevFinish)
    else
      Result := AStart < APrevFinish;
  end;

var
  AEdge, AFinish: TDateTime;
begin
  if not Event.IsRecurring then
  begin
    Result := rvsValid;
    Exit;
  end;
  case Recurrence of
    cxreDaily: Result := GetDailyPatternStatus;
    cxreWeekly: Result := GetWeeklyPatternStatus;
    cxreMonthly: Result := GetMonthlyPatternStatus;
    else Result := GetYearlyPatternStatus;
  end;
  if Result = rvsInvalidPattern then Exit;
  with TcxSchedulerOccurrenceCalculator.Create(Event,
    Event.RecurrenceInfo.Start, cxMaxDate) do
  try
    // find a first occurrence
    if GetNextOccurrence then
    begin
      if Event.RecurrenceInfo.Count = 1 then Exit;
      AFinish := OccurrenceFinish;
      AEdge := OccurrenceStart + 14; //two weeks
      repeat
        if not GetNextOccurrence then Exit;
        if IntersectOccurrences(AFinish, OccurrenceStart) then
        begin
          Result := rvsInvalidDuration;
          Exit;
        end;
        AFinish := OccurrenceFinish;
      until not (Recurrence in [cxreDaily, cxreWeekly]) or (OccurrenceStart > AEdge);
    end
    else
      Result := rvsInvalidDuration;
  finally
    Free;
  end;
end;

procedure TcxSchedulerEventRecurrenceInfo.Validate;
begin
  DisplayTimeBias := Event.TimeBias;
  with TcxSchedulerOccurrenceCalculator.Create(Event,
    Event.RecurrenceInfo.Start, cxMaxDate) do
  try
    if GetNextOccurrence then
      Event.MoveTo(OccurrenceStart)
    else
      cxSchedulerError(cxGetResourceString(@scxWrongPattern));
  finally
    Free;
  end;
end;

function TcxSchedulerEventRecurrenceInfo.GetFinish: TDateTime;
begin
  Result := GetData.Finish - Event.TimeBias;
end;

function TcxSchedulerEventRecurrenceInfo.GetStart: TDateTime;
begin
  Result := dxDateOf(Event.Start);
end;

procedure TcxSchedulerEventRecurrenceInfo.SetFinish(
  AValue: TDateTime);
begin
  AValue := DateTimeHelper.RoundTime(AValue) + Event.TimeBias;
  SetDataItem(@DefInfoData.Finish, SizeOf(AValue), AValue);
end;

procedure TcxSchedulerEventRecurrenceInfo.SetStart(
  const AValue: TDateTime);
begin
  Event.MoveTo(AValue);
end;


function TcxSchedulerEventRecurrenceInfo.GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass;
begin
  Result := TcxSchedulerOccurrenceCalculator;
end;

function TcxSchedulerEventRecurrenceInfo.GetData: TcxSchedulerCustomRecurrenceInfoData;
var
  S: AnsiString;
begin
  if GetValue(S) then
    Result := cxStringToRecurrenceInfoData(S)
  else
    Result := DefInfoData;
end;

function TcxSchedulerEventRecurrenceInfo.GetOccurrences(
  AList: TcxSchedulerFilteredEventList; const AStart, AFinish: TDateTime): Boolean;
var
  ActualStart: TDateTime;
begin
  ActualStart := AStart;
  if not (Event.EventType in [etPattern, etNone]) and (Event.Pattern <> nil) then
    ActualStart := Event.Pattern.Start;
  AList.Init(Trunc(ActualStart), AFinish, Storage, True);
  if (Event.EventType = etPattern) or Event.SkipExceptions then
  begin
    AList.FReminderEventsOnly := False;
    AList.CheckEvent(Event, Null, True);
    AList.Changed;
  end;
  Result := AList.Count > 0;
end;

function TcxSchedulerEventRecurrenceInfo.GetValue(
  var AValue: AnsiString): Boolean;
begin
  Result := Event.GetRecurrenceInfoValue(AValue);
end;

procedure TcxSchedulerEventRecurrenceInfo.SetDataItem(
  AOffset: Pointer; ASize: Integer; const AValue);
var
  S: AnsiString;
begin
  Dec(TdxNativeInt(AOffset), TdxNativeInt(@DefInfoData));
  if not GetValue(S) then
    S := cxRecurrenceInfoDataToString(DefInfoData);
  Move(AValue, S[1 + Integer(AOffset)], ASize);
  SetValue(S);
end;

procedure TcxSchedulerEventRecurrenceInfo.SetValue(
  const AValue: AnsiString);
begin
  Event.SetRecurrenceInfoValue(AValue);
end;

function TcxSchedulerEventRecurrenceInfo.GetEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(Owner);
end;

function TcxSchedulerEventRecurrenceInfo.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Event.Storage;
end;

{ TcxSchedulerEventItemLink }

constructor TcxSchedulerEventItemLink.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FLinkRecurrenceIndex := -1;
end;

destructor TcxSchedulerEventItemLink.Destroy;
begin
  FIsDestroying := True;
  Link := nil;
  inherited;
end;

function TcxSchedulerEventItemLink.GetRelationAsText: string;
begin
  Result := '(' + cxGetResourceString(sEventRelations[Integer(Relation)]) + ')';
end;

function TcxSchedulerEventItemLink.CheckLinked(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent = Link;
end;

function TcxSchedulerEventItemLink.CheckLinked(
  const ID: Variant; ARecurrenceIndex: Integer): Boolean;
begin
  Result := (Link <> nil) and VarEqualsSoft(Link.ID, ID);
  if Result and Link.IsRecurring then
    Result := (ARecurrenceIndex = ARecurrenceIndex);
end;

function TcxSchedulerEventItemLink.GetData: Variant;
begin
  Result := VarArrayCreate([0, 2], varVariant);
  Result[0] := Relation;
  if Link <> nil then
    Result[1] := Link.ID
  else
    Result[1] := Null;
  Result[2] := LinkRecurrenceIndex;
end;

function TcxSchedulerEventItemLink.IsAllowRelation(
  ARelation: TcxSchedulerEventRelation): Boolean;
begin
  Result := ((Link <> nil) and not Link.IsGroup) or
    (ARelation in [trFinishToStart, trStartToStart]);
end;

function TcxSchedulerEventItemLink.IsValid: Boolean;
begin
  Result := IsAllowRelation(Relation) and Event.AllowLink(Link);
end;

procedure TcxSchedulerEventItemLink.SetData(const AData: Variant);
var
  ACandidate: TcxSchedulerEvent;
begin
  Relation := AData[0];
  FLink := nil;
  if not VarIsNull(AData[1]) and (Storage <> nil) then
  begin
    ACandidate := Storage.GetEventByID(AData[1]);
    if (ACandidate <> nil) and Event.CanLink(ACandidate) then
      Link := ACandidate;
  end;
  FLinkRecurrenceIndex := AData[2];
end;

procedure TcxSchedulerEventItemLink.UpdateLink;
begin
  if (Link = nil) or (Event = nil) then Exit;
  case Relation of
    trFinishToStart:
      if Event.Finish > Link.Start then
        Link.MoveTo(Event.Finish);
    trStartToStart:
      if Event.Start > Link.Start then
        Link.MoveTo(Event.Start);
    trFinishToFinish:
      if Event.Finish < Link.Finish then
        Link.MoveTo(Event.Finish - Link.Duration);
    trStartToFinish:
      if Event.Start < Link.Finish then
        Link.MoveTo(Event.Start - Link.Duration);
  end;
end;

function TcxSchedulerEventItemLink.GetEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEventLinks(Collection).Event;
end;

function TcxSchedulerEventItemLink.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Event.Storage;
end;

procedure TcxSchedulerEventItemLink.SetLink(AValue: TcxSchedulerEvent);
begin
  if (FLink <> AValue) and ((AValue = nil) or Event.CanLink(AValue)) then
  begin
    if FLink <> nil then
    begin
      FLink.TaskLinkOwners.Remove(Event);
      FLink.IsDataValid := False;
    end;
    FLink := AValue;
    if FLink <> nil then
    begin
      FLink.TaskLinkOwners.Add(Event);
      FLink.IsDataValid := False;
    end;
    if not FIsDestroying then
      Changed(True);
  end;
end;

procedure TcxSchedulerEventItemLink.SetRelation(AValue: TcxSchedulerEventRelation);
begin
  if AValue = Relation then Exit;
  FRelation := AValue;
  Changed(True);
end;

{ TcxSchedulerEventLinks }

function TcxSchedulerEventLinks.Add(AEvent: TcxSchedulerEvent;
  ARelation: TcxSchedulerEventRelation = trStartToFinish): TcxSchedulerEventItemLink;
begin
  Result := nil;
  if Event.CanLink(AEvent) then
  begin
    BeginUpdate;
    try
      Result := inherited Add as TcxSchedulerEventItemLink;
      Result.FRelation := ARelation;
      Result.Link := AEvent;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxSchedulerEventLinks.IsEventLinked(AEvent: TcxSchedulerEvent): Boolean;
var
  AID: Variant;
  I, ARecurrenceIndex: Integer;
begin
  Result := False;
  if AEvent = nil then Exit;
  for I := 0 to Count - 1 do
  begin
    if AEvent.Index < 0 then
      Result := ItemLinks[I].CheckLinked(AEvent)
    else
    begin
      AID := AEvent.ID;
      ARecurrenceIndex := AEvent.RecurrenceIndex;
    Result := ItemLinks[I].CheckLinked(AEvent.ID, ARecurrenceIndex);
    end;
    if Result then Break;
  end;
end;

procedure TcxSchedulerEventLinks.UpdateLinksTime;
var
  I: Integer;
  ALink: TcxSchedulerEvent;
begin
  for I := 0 to Count - 1 do
  begin
    ALink := ItemLinks[I].Link;
    if ALink = nil then
      Continue;
    ALink.IsDataValid := False;
  end;
end;

function TcxSchedulerEventLinks.GetDisplayText: string;
var
  I: Integer;
  AText: string;
begin
  Result := '';
  for I := 0 to Count - 1 do
  begin
    AText := VarToStr(ItemLinks[I].ID);
    if (Result <> '') and (AText <> '') then
      Result := Result + dxFormatSettings.ListSeparator;
    Result := Result + AText;
  end;
end;

function TcxSchedulerEventLinks.GetOwner: TPersistent;
begin
  Result := FEvent;
end;

procedure TcxSchedulerEventLinks.PrepareLinks;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not ItemLinks[I].IsValid then
      Delete(I);
end;

procedure TcxSchedulerEventLinks.RemoveLink(ALink: TcxSchedulerEvent);
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if ItemLinks[I].Link = ALink then
      ItemLinks[I].Free;
end;

procedure TcxSchedulerEventLinks.Update(Item: TCollectionItem);
begin
  inherited;
  Event.TaskLinksChanged(Self);
end;

function TcxSchedulerEventLinks.GetItemLink(AIndex: Integer): TcxSchedulerEventItemLink;
begin
  Result := TcxSchedulerEventItemLink(inherited Items[AIndex]);
end;

procedure TcxSchedulerEventLinks.SetItemLink(AIndex: Integer; AValue: TcxSchedulerEventItemLink);
begin
  ItemLinks[AIndex].Assign(AValue);
end;

function TcxSchedulerEventLinks.GetExpanded: Boolean;
begin
  Result := Event.GetOptionsFlag and omCollapsed = 0;
end;

procedure TcxSchedulerEventLinks.SetExpanded(AValue: Boolean);
begin
  if Event.SetOptionsFlag(omCollapsed, not AValue) then
    Event.TaskLinksChanged(Self);
end;

{ TcxSchedulerEventTimeCalculator }

constructor TcxSchedulerEventTimeCalculator.Create(AEvent: TcxSchedulerEvent);
begin
  inherited Create;
  FEvent := AEvent;
end;

procedure TcxSchedulerEventTimeCalculator.Calculate;
begin
  DoCalculate;
end;

procedure TcxSchedulerEventTimeCalculator.DoCalculate;
var
  AStart, AFinish: TDateTime;
  ADuration: TDateTime;
begin
  FFinish := GetBaseFinish;
  FStart := GetBaseStart;
  ADuration := StoredDuration;
  GetValidTaskTimeRange(AStart, AFinish);
  if Finish > AFinish then
  begin
    FFinish := AFinish;
    FStart := Finish - ADuration;
  end;
  if Start < AStart then
  begin
    FStart := AStart;
    FFinish := Start + ADuration;
  end;
  FStart := DateTimeHelper.RoundTime(FStart);
  FFinish := DateTimeHelper.RoundTime(FFinish);
end;

function TcxSchedulerEventTimeCalculator.GetBaseFinish: TDateTime;
var
  ALink: TcxDoublyLinkedEventListData;
begin
  if Event.GroupItems.HasChildren then
  begin
    Result := MinDateTime;
    ALink := Event.GroupItems.First;
    while ALink <> nil do
    begin
      Result := Max(Result, ALink.Event.Finish);
      ALink := ALink.Next;
    end;
  end
  else
    Result := GetStoredFinish;
end;

function TcxSchedulerEventTimeCalculator.GetBaseStart: TDateTime;
var
  ALink: TcxDoublyLinkedEventListData;
begin
  if Event.GroupItems.HasChildren then
  begin
    Result := MaxDateTime;
    ALink := Event.GroupItems.First;
    while ALink <> nil do
    begin
      Result := Min(Result, ALink.Event.Start);
      ALink := ALink.Next;
    end;
  end
  else
    Result := GetStoredStart;
end;

function TcxSchedulerEventTimeCalculator.GetDuration: TDateTime;
begin
  if Event.IsDataValid then
    Result := FFinish - FStart
  else
    Result := StoredDuration;
end;

function TcxSchedulerEventTimeCalculator.GetStoredDuration: TDateTime;
begin
  Result := StoredFinish - StoredStart;
end;

function TcxSchedulerEventTimeCalculator.GetStoredFinish: TDateTime;
begin
  Result := TDateTime(Event.GetValueDefault(Event.Storage.FFinishField, 0)) - Event.TimeBias;
end;

function TcxSchedulerEventTimeCalculator.GetStoredStart: TDateTime;
begin
  Result := TDateTime(Event.GetValueDefault(Event.Storage.FStartField, 0)) - Event.TimeBias;
end;

procedure TcxSchedulerEventTimeCalculator.GetValidTaskTimeRange(var AStart, AFinish: TDateTime);
begin
  Event.GetValidTaskTimeRange(AStart, AFinish);
end;

{ TcxSchedulerEventGroupItems }

constructor TcxSchedulerEventGroupItems.Create(AOwner: TcxSchedulerEvent);
begin
  inherited Create;
  FEvent := AOwner;
  FExpanded := True;
end;

function TcxSchedulerEventGroupItems.IsChild(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent.ParentGroup = Event;
end;

function TcxSchedulerEventGroupItems.Find(AEvent: TcxSchedulerEvent): TcxDoublyLinkedEventListData;
begin
  Result := First;
  while (Result <> nil) and (Result.Event <> AEvent) do
    Result := Result.Next;
end;

procedure TcxSchedulerEventGroupItems.UpdateItemsTime;
var
  ALink: TcxDoublyLinkedEventListData;
begin
  if HasChildren then
  begin
    ALink := First;
    while ALink <> nil do
    begin
      ALink.Event.IsDataValid := False;
      ALink := ALink.Next;
    end;
  end
end;

procedure TcxSchedulerEventGroupItems.DeleteEvents;
var
  ALink: TcxDoublyLinkedEventListData;
  AEvent: TcxSchedulerEvent;
begin
  if HasChildren then
  begin
    ALink := First;
    while ALink <> nil do
    begin
      AEvent := ALink.Event;
      ALink := ALink.Next;
      AEvent.Delete;
    end;
  end;
end;

function TcxSchedulerEventGroupItems.GetHasChildren: Boolean;
begin
  Result := Count > 0;
end;

function TcxSchedulerEventGroupItems.GetID: Variant;
begin
  Result := Event.ID;
end;

procedure TcxSchedulerEventGroupItems.SetExpanded(AValue: Boolean);
begin
  if FExpanded <> AValue then
  begin
    FExpanded := AValue;
    Event.Modified;
    Event.Storage.Changed;
  end;
end;

{ TcxSchedulerEvent }

constructor TcxSchedulerEvent.Create(
  AStorage: TcxCustomSchedulerStorage; ADisableEditing: Boolean = False);
begin
  Create(AStorage, nil);
  if ADisableEditing then Exit;
  BeginEditing;
  AssignDefaultValues;
end;

constructor TcxSchedulerEvent.Create(
  AStorage: TcxCustomSchedulerStorage; ARecordHandle: Pointer);
begin
  FID := Null;
  FStorage := AStorage;
  FRecordHandle := ARecordHandle;
  FPrevTaskComplete := 0;
  CreateSubClasses;
end;

destructor TcxSchedulerEvent.Destroy;
begin
  cxClearObjectLinks(Self);
  FreeAndNil(FInfo.ResourceIDHashTable);
  ParentGroup := nil;
  FGroupItems.DeleteEvents;
  Storage.InternalRemove(Self);
  Storage.FreeRecord(FRecordHandle);
  Storage.FreeRecord(FEditingRecordHandle);
  RemoveTaskLinkOwnersReferences;
  DestroySubClasses;
  inherited Destroy;
end;

procedure TcxSchedulerEvent.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerEvent then
  begin
    BeginEditing;
    try
      Data := TcxSchedulerEvent(Source).Data;
      for I := 0 to Min(TcxSchedulerEvent(Source).ValueCount, ValueCount) - 1 do
        SetValueByIndex(I, TcxSchedulerEvent(Source).GetValueByIndex(I));
      if EventType = etNone then
        FPattern := nil;
      UTCStart := TcxSchedulerEvent(Source).UTCStart;
      UTCFinish := TcxSchedulerEvent(Source).UTCFinish;
      ReminderDate := TcxSchedulerEvent(Source).ReminderDate;
      RecurrenceInfo.DismissDate := TcxSchedulerEvent(Source).RecurrenceInfo.DismissDate;
      //must be the last
      ReminderResourcesData := TcxSchedulerEvent(Source).ReminderResourcesData;
      UpdateParentGroup;
      Modified;
    finally
      IsDataValid := False;
      EndEditing;
    end;
  end;
end;

procedure TcxSchedulerEvent.AssignAttributes(
  ASource: TcxSchedulerEvent; AUseSourceTime: Boolean = True);
begin
  BeginEditing;
  try
    if AUseSourceTime then
    begin
      Start := ASource.Start;
      Finish := ASource.Finish;
    end;
    Caption := ASource.Caption;
    Message := ASource.Message;
    State := ASource.State;
    LabelColor := ASource.LabelColor;
    ResourceID := ASource.ResourceID;
  finally
    EndEditing;
  end;
end;

procedure TcxSchedulerEvent.ShareWithResource(
  AResourceItem: TcxSchedulerStorageResourceItem);
begin
  if (AResourceItem = nil) then Exit;
  ShareWithResource(AResourceItem.ResourceID);
end;

procedure TcxSchedulerEvent.ShareWithResource(AResourceID: Variant);
var
  ASharedResources: Variant;
begin
  if IsSharedWithResource(AResourceID) or VarIsNull(AResourceID) or
    VarIsEmpty(AResourceID) then Exit;
  ASharedResources := ResourceID;
  if not VarIsArray(ASharedResources) then
    ASharedResources := VarListArrayCreate(ASharedResources);
  if VarIsNull(ASharedResources[0]) then
    ASharedResources[0] := AResourceID
  else
    VarListArrayAddValue(ASharedResources, AResourceID);
  ResourceID := ASharedResources;
end;

function TcxSchedulerEvent.IsSharedWithResource(
  AResourceItem: TcxSchedulerStorageResourceItem): Boolean;
var
  AIndex: Integer;
begin
  Result := False;
  if VarIsNull(ResourceID) or (AResourceItem = nil) then
    Exit;
  if VarIsNull(AResourceItem.ResourceID) then
  begin
    Result := True;
    Exit;
  end;
  Result := FInfo.ResourceIDHashTable.TryGetValue(VarToStr(AResourceItem.ResourceID), AIndex);
  if VarIsArray(FInfo.ResourceID) then
    Result := Result and VarEqualsSoftEx(FInfo.ResourceID[AIndex], AResourceItem.ResourceID)
  else
    Result := Result and VarEqualsSoftEx(FInfo.ResourceID, AResourceItem.ResourceID)
end;

function TcxSchedulerEvent.IsSharedWithResource(AResourceID: Variant): Boolean;
var
  I: Integer;
  AResources: Variant;

  function CheckSimpleID(AResID: Variant): Boolean;
  var
    I: Integer;
  begin
    if not VarIsArray(AResourceID) then
      Result := VarEqualsSoftEx(AResID, AResourceID)
    else
    begin
      Result := False;
      for I := VarArrayLowBound(AResourceID, 1) to VarArrayHighBound(AResourceID, 1) do
        if VarEqualsSoftEx(AResID, AResourceID[I]) then
        begin
          Result := True;
          Break;
        end;
    end;
  end;

begin
  Result := False;
  AResources := ResourceID;
  if not VarIsArray(AResources) then
    Result := VarIsNull(AResourceID) or CheckSimpleID(AResources)
  else
    for I := VarArrayLowBound(AResources, 1) to VarArrayHighBound(AResources, 1) do
    begin
      Result := CheckSimpleID(AResources[I]);
      if Result then Break;
    end;
end;

procedure TcxSchedulerEvent.UnshareWithResource(
  AResourceItem: TcxSchedulerStorageResourceItem);
begin
  if not IsSharedWithResource(AResourceItem) then Exit;
  UnshareWithResource(AResourceItem.ResourceID);
end;

procedure TcxSchedulerEvent.UnshareWithResource(AResourceID: Variant);
var
  I, C: Integer;
  ASharedResources: Variant;
  ANewSharedResources: Variant;
begin
  if not VarIsArray(ResourceID) and VarEquals(ResourceID, AResourceID) then
  begin
    ResourceID := Null;
    Exit;
  end
  else
  begin
    ASharedResources := ResourceID;
    C := VarArrayHighBound(ResourceID, 1) - VarArrayLowBound(ResourceID, 1);
    if C = 0 then
      ANewSharedResources := Null
    else
    begin
      ANewSharedResources := VarArrayCreate([0, C - 1], varVariant);
      C := 0;
      for I := VarArrayLowBound(ResourceID, 1) to VarArrayHighBound(ResourceID, 1) do
        if not VarEquals(AResourceID, ASharedResources[I]) then
        begin
          ANewSharedResources[C] := ASharedResources[I];
          Inc(C);
        end;
    end;
    ResourceID := ANewSharedResources;
  end;
end;

procedure TcxSchedulerEvent.ReplaceResourceID(AResourceID: Variant);
begin
  if Shared then
    ShareWithResource(AResourceID)
  else
    ResourceID := AResourceID;
end;

procedure TcxSchedulerEvent.BeginEditing;
var
  P: PByte;
  I: Integer;
begin
  if not IsEditing then
  begin
    FEditingRecordHandle := Storage.AllocateRecord;
    for I := 0 to ValueCount - 1 do
      Storage.SetValue(FEditingRecordHandle, I, GetValueByIndex(I));
    if VarIsOrdinal(FID) then
    begin
      P := PByte(FEditingRecordHandle);
      Inc(P);
      PInteger(P)^ := FID;
    end;
  end;
  Inc(FEditCount);
end;

procedure TcxSchedulerEvent.Cancel;
begin
  IsModified := False;
  if FEditCount = 1 then
  begin
    Storage.FreeRecord(FEditingRecordHandle);
    Storage.CancelEvent(Self);
  end
  else
    Dec(FEditCount);
end;

procedure TcxSchedulerEvent.EndEditing();
begin
  if FEditCount = 0 then Exit;
  try
    if (FEditCount = 1) and not IsNewEvent then
      PostEditingData;
  finally
    Dec(FEditCount);
    if FEditCount <= 0 then
    begin
      Storage.FreeRecord(FEditingRecordHandle);
    end;
  end;
end;

procedure TcxSchedulerEvent.Delete;
begin
  if Self = nil then Exit;
  case EventType of
    etCustom:
    begin
     Storage.BeginUpdate;
     try
       EventType := etException;
     finally
       Storage.EndUpdate;
     end;
    end;
    etOccurrence:
      begin
        Storage.BeginUpdate;
        try
          with Storage.CreateEvent do
          begin
            Assign(Self);
            ParentID := Self.ID;
            EventType := etException;
          end;
        finally
          Storage.EndUpdate;
        end;
      end;
  else
    Storage.DoDeleteEvent(Self);
  end;
end;

procedure TcxSchedulerEvent.DeleteExceptions;
var
  ALink: TcxSchedulerEvent;
begin
  if not HasExceptions then Exit;
  Storage.BeginUpdate;
  try
    while FLink <> nil do
    begin
      ALink := FLink.FLink;
      Storage.DoDeleteEvent(FLink);
      FLink := ALink;
    end;
  finally
    Storage.EndUpdate;
  end;
end;

function TcxSchedulerEvent.HasAsParentGroup(AEvent: TcxSchedulerEvent): Boolean;
var
  AParentGroup: TcxSchedulerEvent;
begin
  Result := False;
  AParentGroup := Self;
  while not Result and (AParentGroup <> nil) do
  begin
    Result := AParentGroup = AEvent;
    AParentGroup := AParentGroup.ParentGroup;
  end;
end;

function TcxSchedulerEvent.HasChildren: Boolean;
begin
  Result := IsGroup and GroupItems.HasChildren;
end;

function TcxSchedulerEvent.HasExceptions: Boolean;
begin
  Result := (EventType = etPattern) and (Link <> nil);
end;

function TcxSchedulerEvent.HasReminderForResourceID(const AResourceID: Variant): Boolean;
var
  I: Integer;
  AReminderResources: TcxSchedulerReminderResources;
begin
  AReminderResources := nil; //to avoid Delphi6 warning
  Result := Reminder;
  if Result and not VarIsNull(ResourceID) then
  begin
    if Shared then
    begin
      Result := False;
      AReminderResources := ReminderResourcesData.Resources;
      for I := Low(AReminderResources) to High(AReminderResources) do
        if VarEquals(AReminderResources[I].ResourceID, AResourceID) then
        begin
          Result := True;
          Break;
        end;
    end
    else
      Result := VarEquals(ResourceID, AResourceID);
  end;
end;

function TcxSchedulerEvent.Conflicts(AExceptEventsWithoutResources: Boolean): Boolean;
var
  AFinish: TDateTime;
begin
  Result := False;
  CheckCalculated;
  if IsFreeState then Exit;
  AFinish := Finish;
  if (EventType = etPattern) or SkipExceptions then
    AFinish := Finish + cxMaxCheckedDuration;
  with cxSchedulerEventConflictsInfoClass.Create(Self, AExceptEventsWithoutResources, Start, AFinish) do
  try
    Result := HasConflicts;
  finally
    Free;
  end;
end;

function TcxSchedulerEvent.CreateConflictsInfo(
  AExceptEventsWithoutResources: Boolean): TcxSchedulerEventConflictsInfo;
begin
  Result := cxSchedulerEventConflictsInfoClass.Create(Self, AExceptEventsWithoutResources);
end;

function TcxSchedulerEvent.GetCustomFieldValueByIndex(AIndex: Integer): Variant;
begin
  Result := GetValueDefault(TcxCustomSchedulerStorageField(Storage.CustomFields.Items[AIndex]), Null);
end;

function TcxSchedulerEvent.GetCustomFieldValueByName(
  const AName: String): Variant;
var
  AField: TcxCustomSchedulerStorageField;
begin
  AField := Storage.CustomFields.FindFieldByName(AName);
  if AField <> nil then
    Result := GetValueDefault(AField, Null)
  else
    cxSchedulerError(scxInvalidFieldName);
end;

function TcxSchedulerEvent.GetCustomFieldValue(
  ACustomField: TcxCustomSchedulerStorageField): Variant;
begin
  if (ACustomField <> nil) and (ACustomField.Storage = Storage) then
    Result := GetValueByIndex(ACustomField.Index)
  else
    cxSchedulerError(scxInvalidCustomField);
end;

procedure TcxSchedulerEvent.SetCustomFieldValueByIndex(AIndex: Integer;
  const AValue: Variant);
begin
  SetCustomFieldValue(TcxCustomSchedulerStorageField(
    Storage.CustomFields.Items[AIndex]), AValue);
end;

procedure TcxSchedulerEvent.SetCustomFieldValueByName(const AName: string;
  const AValue: Variant);
begin
  SetCustomFieldValue(Storage.CustomFields.FindFieldByName(AName), AValue);
end;

procedure TcxSchedulerEvent.SetCustomFieldValue(
  ACustomField: TcxCustomSchedulerStorageField; const AValue: Variant);
begin
  if (ACustomField <> nil) and (ACustomField.Storage = Storage) then
    SetValue(ACustomField, AValue)
  else
    cxSchedulerError(scxInvalidCustomField);
end;

function TcxSchedulerEvent.GetOccurrence(ADate: TDateTime): TcxSchedulerEvent;
var
  ACalculator: TcxSchedulerOccurrenceCalculator;
  ALink: TcxSchedulerEvent;
  AOriginalDate: TDateTime;
begin
  Result := nil;
  if EventType <> etPattern then Exit;
  ADate := dxDateOf(ADate);
  ALink := FLink;
  while ALink <> nil do
  begin
    AOriginalDate := dxDateOf(ALink.GetOriginalDate);
    if AOriginalDate = ADate then
    begin
      Result := ALink;
      Exit;
    end
    else
      if AOriginalDate > ADate then
        break
      else
        ALink := ALink.FLink;
  end;
  ACalculator := TcxSchedulerOccurrenceCalculator.Create(Self, Start, ADate + 1);
  try
    while ACalculator.GetNextOccurrence do
    begin
      AOriginalDate := dxDateOf(ACalculator.OccurrenceStart);
      if AOriginalDate > ADate then
        break
      else
        if AOriginalDate = ADate then
        begin
          Result := Storage.createEvent;
          Result.Assign(Self);
          Result.Start := ACalculator.OccurrenceStart;
          Result.Finish := ACalculator.OccurrenceFinish;
          Result.RecurrenceIndex := ACalculator.Index;
          Result.EventType := etOccurrence;
          Result.ParentID := ID;
          Result.FPattern := Self;
          break;
        end;
    end;
  finally
    ACalculator.Free;
  end;
end;

function TcxSchedulerEvent.GetOriginalDate: TDateTime;
begin
  if EventType in [etCustom, etException] then
  begin
    with TcxSchedulerOccurrenceCalculator.Create(Pattern, 0, cxMaxDate) do
    try
      CalcOccurrence(Self.RecurrenceIndex + 1);
      Result := OccurrenceStart;
    finally
      Free;
    end;
  end
  else
    Result := Start;
end;

function TcxSchedulerEvent.GetResourceItem: TcxSchedulerStorageResourceItem;
var
  I: Integer;
begin
  for I := 0 to Storage.ResourceCount - 1 do
  begin
    Result := Storage.Resources.ResourceItems[I];
    if IsResourceEvent(Result, False) then
      Exit;
  end;
  Result := nil;
end;

function TcxSchedulerEvent.GetRecurrenceChain: TcxSchedulerEventList;
var
  ALink: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEventList.Create;
  if EventType = etPattern then
  begin
    ALink := Link;
    while ALink <> nil do
    begin
      Result.Add(ALink);
      ALink := ALink.Link;
    end;
  end;
end;

function TcxSchedulerEvent.GetTaskLinkOwnerRelation(ATaskLinkOwner: TcxSchedulerEvent; var ARelation: TcxSchedulerEventRelation): Boolean;
var
  I: Integer;
begin
  Result := False;
  if (ATaskLinkOwner = nil) or (ATaskLinkOwner.TaskLinks = nil) then Exit;
  for I := 0 to ATaskLinkOwner.TaskLinks.Count - 1 do
    if ATaskLinkOwner.TaskLinks.ItemLinks[I].Link = Self then
    begin
      Result := True;
      ARelation := ATaskLinkOwner.TaskLinks.ItemLinks[I].Relation;
      Break;
    end;
end;

procedure TcxSchedulerEvent.GetValidTaskTimeRange(var AStart, AFinish: TDateTime);
begin
  AStart := MinDateTime;
  AFinish := MaxDateTime;
  DoGetValidTaskTimeRange(AStart, AFinish);
end;

procedure TcxSchedulerEvent.GetValidTaskTimeRangeByRelation(const ARelation: TcxSchedulerEventRelation; AEvent: TcxSchedulerEvent; var AStart, AFinish: TDateTime);
begin
  if AEvent = nil then Exit;
  case ARelation of
    trFinishToStart:
      AStart := Max(AStart, AEvent.Finish - GetTaskCompleteDuration);
    trStartToStart:
     AStart := Max(AStart, AEvent.Start - GetTaskCompleteDuration);
    trFinishToFinish:
      AFinish := Min(AFinish, AEvent.Finish);
    trStartToFinish:
      AFinish := Min(AFinish, AEvent.Start);
  end;
end;

procedure TcxSchedulerEvent.DoGetValidTaskTimeRange(var AStart, AFinish: TDateTime);
var
  I: Integer;
  ARelation: TcxSchedulerEventRelation;
begin
  if (TaskStatus <> tsComplete) and (TaskLinkOwners <> nil) then
  begin
    for I := 0 to TaskLinkOwners.Count - 1 do
      if GetTaskLinkOwnerRelation(TaskLinkOwners[I], ARelation) then
        GetValidTaskTimeRangeByRelation(ARelation, TaskLinkOwners[I], AStart, AFinish);
  end;
  if ParentGroup <> nil then
    ParentGroup.DoGetValidTaskTimeRange(AStart, AFinish);
end;

function TcxSchedulerEvent.IsDayEvent(ADate: Integer): Boolean;
var
  AFinish: TDateTime;
begin
  CheckCalculated;
  AFinish := Finish;
  if AFinish > ADate then
    Result := FInfo.DayStart <= ADate
  else
    if AFinish = ADate then
      Result := FInfo.DayStart = ADate
    else
      Result := False
end;


function TcxSchedulerEvent.IsDayEvent(const ADate: TDateTime): Boolean;
begin
  Result := IsDayEvent(Integer(Trunc(ADate)));
end;

function TcxSchedulerEvent.IsRecurring: Boolean;
begin
  Result := EventType in [etPattern..etCustom];
end;

function TcxSchedulerEvent.IsResourceEvent(
  AResource: TcxSchedulerStorageResourceItem; AllowUnassigned: Boolean): Boolean;
begin
  Result := (AResource = nil) or VarIsNull(AResource.ResourceID) or (AllowUnassigned and VarIsNull(ResourceID)) or
    IsSharedWithResource(AResource)
//    IsSharedWithResource(AResource.ResourceID);
end;

procedure TcxSchedulerEvent.MoveTo(const AStartTime: TDateTime);
begin
  if not CanMoveTo(AStartTime) then
    Exit;
  InternalMoveTo(AStartTime);
end;

procedure TcxSchedulerEvent.Post;
begin
  if (FEditCount = 1) and IsNewEvent then
    FStorage.PostEvent(Self);
end;

procedure TcxSchedulerEvent.RemoveRecurrence;
begin
  if EventType = etPattern then
  begin
    DeleteExceptions;
    EventType := etNone;
  end;
end;

procedure TcxSchedulerEvent.AssignDefaultValues;
begin
  AllDayEvent := False;
  Caption := '';
  Enabled := True;
  EventType := etNone;
  LabelColor := clDefault;
  Location := '';
  Message := '';
  RecurrenceIndex := -1;

  Reminder := Storage.Reminders.DefaultReminder;
  ReminderMinutesBeforeStart := Storage.Reminders.DefaultMinutesBeforeStart;

  Start := Now;
  Finish := Start + cxDefaultEventDuration;
  State := tlsBusy;
  RecurrenceInfo.AssignDefaultValues;
end;

procedure TcxSchedulerEvent.CalculateActualTimeRange;
var
  AStart, AFinish: TDateTime;
  ALink: TcxSchedulerEvent;
begin
  if not Storage.ActualTimeRangeAvailable then Exit;
  if EventType = etPattern then
  begin
    AStart := RecurrenceInfo.Start;
    AFinish := RecurrenceInfo.Finish;
    if RecurrenceInfo.IsInfinity then
      AFinish := cxMaxDate
    else
      with TcxSchedulerOccurrenceCalculator.Create(Self, RecurrenceInfo.Start, cxMaxDate) do
      try
        while GetNextOccurrence do
        begin
          AStart := Min(AStart, OccurrenceStart);
          AFinish := Max(AFinish, OccurrenceFinish);
        end;
      finally
        Free;
      end;
    ALink := Link;
    while ALink <> nil do
    begin
      AStart := Min(AStart, ALink.Start);
      AFinish := Max(AFinish, ALink.Finish);
      ALink := ALink.Link;
    end;
  end
  else
  begin
    AStart := Start;
    AFinish := Finish;
    if Pattern <> nil then
    begin
      AStart := Min(AStart, Pattern.Start);
      AFinish := Max(AFinish, Pattern.Finish);
    end;
  end;
  SetActualTimeRange(AStart, AFinish);
end;

procedure TcxSchedulerEvent.CalculateActualTimeRangePost;
begin
  if not Storage.ActualTimeRangeAvailable then Exit;
  if (EventType in [etCustom, etException]) and (Pattern <> nil) then
  begin
    Pattern.CheckRecurrenceLinkEx(Self);
    Pattern.CalculateActualTimeRange;
  end;
end;

function TcxSchedulerEvent.AllowLink(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := Storage.LinksController.AllowLink(Self, AEvent);
end;

function TcxSchedulerEvent.CanLink(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := (AEvent <> nil) and (AEvent <> Self) and AllowLink(AEvent) and
    (AEvent.TaskLinkOwners.IndexOf(Self) = -1);
end;

function TcxSchedulerEvent.IsAllDayOrLonger: Boolean;
begin
  Result := AllDayEvent or (Duration >= 1);
end;

function TcxSchedulerEvent.IsEventLinked(AHeaderEvent: TcxSchedulerEvent): Boolean;
begin
  Result := Storage.LinksController.IsLinked(Self, AHeaderEvent);
end;

procedure TcxSchedulerEvent.AddItem(AEvent: TcxSchedulerEvent);
begin
  if CanAddItem(AEvent) then
  begin
    IsGroup := True;
    GroupItems.Add(AEvent);
    AEvent.FParentGroup := Self;
    PrepareTaskLinks;
    AEvent.PrepareTaskLinks;
    AEvent.SetGroupID(ID);
    IsDataValid := False;
  end;
end;

function TcxSchedulerEvent.CanAddItem(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := (AEvent.EventType = etNone) and (EventType = etNone) and
    not HasAsParentGroup(AEvent) and (GroupItems.Find(AEvent) = nil) and
    (AEvent <> Self);
end;

procedure TcxSchedulerEvent.RemoveItem(AEvent: TcxSchedulerEvent);
var
  ALink: TcxDoublyLinkedObject;
begin
  if GroupItems.IsChild(AEvent) then
  begin
    ALink := GroupItems.Find(AEvent);
    AEvent.FParentGroup := nil;
    GroupItems.Remove(ALink);
    if not Storage.IsEventDeleting(AEvent) and not Storage.IsDestroying then
      AEvent.SetGroupID(NULL);
    IsDataValid := False;
  end;
end;

function TcxSchedulerEvent.CanMoveTo(ANewTime: TDateTime): Boolean;
var
  AStartTime, AFinishTime: TDateTime;
begin
  Result := TaskStatus = tsComplete;
  if not Result then
  begin
    GetValidTaskTimeRange(AStartTime, AFinishTime);
    Result := (AStartTime <= ANewTime) and (AFinishTime >= (ANewTime + Duration));
  end;
end;

procedure TcxSchedulerEvent.CheckCalculated;
begin
  if not IsDataValid then
    UpdateTemporaryData;
end;

procedure TcxSchedulerEvent.CheckLinksOnChangeEventType(ANewEventType: TcxEventType);
begin
  if ANewEventType <> etNone then
  begin
    TaskLinks.Clear;
    RemoveTaskLinkOwnersReferences;
  end;
end;

procedure TcxSchedulerEvent.CheckRecurrenceLink(AEvent: TcxSchedulerEvent);
var
  ALink: TcxSchedulerEvent;
begin
  if (AEvent.EventType in [etCustom, etException]) and VarEquals(ID, AEvent.ParentID) then
  begin
    AEvent.FPattern := Self;
    if (FLink = nil) or (FLink.RecurrenceIndex >= AEvent.RecurrenceIndex) then
    begin
      AEvent.FLink := FLink;
      FLink := AEvent;
    end
    else
      begin
       ALink := FLink;
       while ALink.FLink <> nil do
         ALink := ALink.FLink;
       AEvent.FLink := nil;
       ALink.FLink := AEvent;
      end;
  end;
end;

procedure TcxSchedulerEvent.CheckRecurrenceLinkEx(AEvent: TcxSchedulerEvent);
var
  ALink: TcxSchedulerEvent;
begin
  if AEvent.Link <> nil then Exit;
  ALink := FLink;
  while (ALink <> nil) and (ALink <> AEvent) do
    ALink := ALink.Link;
  if ALink = nil then
  begin
    AEvent.FLink := Link;
    FLink := AEvent;
  end;
end;

function TcxSchedulerEvent.CreateGroupItems: TcxSchedulerEventGroupItems;
begin
  Result := TcxSchedulerEventGroupItems.Create(Self);
end;

function TcxSchedulerEvent.CheckTimeRange(
  const AStartDate, AFinishDate: Integer): Boolean;
begin
  if Storage.ActualTimeRangeAvailable then
    Result := (ActualStart <= AFinishDate) and (ActualFinish >= AStartDate)
  else
    Result := (EventType = etPattern) or (Start <= AFinishDate) and (Finish >= AStartDate);
end;

function TcxSchedulerEvent.CreateTaskLinks: TcxSchedulerEventLinks;
begin
  Result := TcxSchedulerEventLinks.Create(TcxSchedulerEventItemLink);
  Result.FEvent := Self;
end;

function TcxSchedulerEvent.CreateTaskLinkOwners: TcxSchedulerEventList;
begin
  Result := TcxSchedulerEventList.Create();
end;

procedure TcxSchedulerEvent.DestroySubClasses;
begin
  FreeAndNil(FTaskLinks);
  FreeAndNil(FTaskLinkOwners);
  FreeAndNil(FRecurrenceInfo);
  FreeAndNil(FGroupItems);
end;

function TcxSchedulerEvent.CreateRecurrenceInfo: TcxSchedulerEventRecurrenceInfo;
begin
  Result := TcxSchedulerEventRecurrenceInfo.Create(Self);
end;

function TcxSchedulerEvent.CreateReminderResourcesData: TcxSchedulerReminderResourcesData;
var
  ACount, I: Integer;
  AReminderDate, ADismissDate: TDateTime;
begin
  Result.Version := 3;
  AReminderDate := ReminderDate;
  ADismissDate := RecurrenceInfo.DismissDate;
  ACount := ResourceIDCount;
  SetLength(Result.Resources, ACount);
  for I := 0 to ACount - 1 do
    with Result.Resources[I] do
    begin
      DismissDate := ADismissDate;
      ResourceID := ResourceIDs[I];
      ReminderDate := AReminderDate;
    end;
end;

procedure TcxSchedulerEvent.CreateSubClasses;
begin
  FRecurrenceInfo := CreateRecurrenceInfo;
  FTaskLinkOwners := CreateTaskLinkOwners;
  FTaskLinks := CreateTaskLinks;
  FGroupItems := CreateGroupItems;
end;

function TcxSchedulerEvent.GetDataController: TcxSchedulerStorageDataController;
begin
  Result := Storage.DataController;
end;

function TcxSchedulerEvent.GetID: Variant;
begin
  Result := FID;
end;

function TcxSchedulerEvent.GetIndex: Integer;
begin
  Result := FIndex;
end;

function TcxSchedulerEvent.GetIsFreeState: Boolean;
begin
  Result := State = tlsFree;
end;

function TcxSchedulerEvent.GetGroupItems: TcxSchedulerEventGroupItems;
begin
  Result := FGroupItems;
end;

function TcxSchedulerEvent.GetOccurrenceByDate(const ADate: TDateTime): TcxSchedulerEvent;
begin
  Result := FLink;
end;

function TcxSchedulerEvent.GetOccurrenceByIndex(
  AIndex: Integer; var AOccurrence: TcxSchedulerEvent): Boolean;
begin
  Result := False;
  if (FLink = nil) or SkipExceptions then Exit;
  AOccurrence := FLink;
  while (AOccurrence <> nil) and (AOccurrence.RecurrenceIndex <> AIndex) do
    AOccurrence := AOccurrence.FLink;
  Result := (AOccurrence <> nil) and (AOccurrence.RecurrenceIndex = AIndex);
end;

function TcxSchedulerEvent.GetOwner: TPersistent;
begin
  Result := FStorage;
end;

function TcxSchedulerEvent.GetRecurrenceInfoValue(
  var AValue: AnsiString): Boolean;
begin
  CheckCalculated;
  AValue := FInfo.RecurrenceInfo;
  Result := AValue <> '';
end;

procedure TcxSchedulerEvent.GetStartFinishTime(var AStart, AFinish: TDateTime);
begin
  AStart := Start;
  AFinish := Finish;
end;

function TcxSchedulerEvent.GetTaskLinks: TcxSchedulerEventLinks;
begin
  Result := FTaskLinks;
end;

function TcxSchedulerEvent.GetTaskLinkOwners: TcxSchedulerEventList;
begin
  Result := FTaskLinkOwners;
end;

function TcxSchedulerEvent.GetTaskCompleteDuration: TDateTime;
begin
  Result := Duration * TaskComplete / 100;
end;

function TcxSchedulerEvent.GetValueByIndex(AIndex: Integer): Variant;
begin
  Result := FStorage.GetEventValue(FRecordHandle, AIndex);
  if (Pattern <> nil) and (Pattern <> Self) and VarIsNull(Result) and Storage.CanGetValueFromPattern(AIndex) then
     Result := Pattern.GetValueByIndex(AIndex);
end;

function TcxSchedulerEvent.GetValueDefault(
  AField: TcxCustomSchedulerStorageField; const ADefValue: Variant): Variant;
begin
  if IsEditing then
    Result := FStorage.GetEventValue(FEditingRecordHandle, AField.Index)
  else
    Result := FStorage.GetEventValue(FRecordHandle, AField.Index);
  if VarType(Result) in [varNull, varEmpty] then
    Result := ADefValue;
end;

procedure TcxSchedulerEvent.InternalMoveTo(const AStartTime: TDateTime);
var
  ADuration: Double;
begin
  BeginEditing;
  try
    ADuration := Duration;
    Start := AStartTime;
    Duration := ADuration;
  finally
    EndEditing;
  end;
end;

procedure TcxSchedulerEvent.InvalidateReferences;
begin
  if ParentGroup <> nil then
    ParentGroup.IsDataValid := False;
  TaskLinks.UpdateLinksTime;
  GroupItems.UpdateItemsTime;
end;

function TcxSchedulerEvent.IsParentGroupForEvent(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := IsGroup and (AEvent <> nil) and (AEvent.ParentGroup = Self);
end;

procedure TcxSchedulerEvent.Modified;
begin
  FIsModified := True;
  IsDataValid := False;
end;

procedure TcxSchedulerEvent.PostEditingData;
begin
  if IsModified then
  begin
    Storage.BeginUpdate;
    try
      CalculateActualTimeRange;
      if FEditingRecordHandle <> nil then
        FStorage.PostEditingData(Self);
      Storage.Changed;
    finally
      CalculateActualTimeRangePost;
      Storage.EndUpdate;
    end;
  end;
end;

procedure TcxSchedulerEvent.PrepareTaskLinks;
var
  ALink: TcxDoublyLinkedEventListData;
begin
  TaskLinks.PrepareLinks;
  ALink := GroupItems.First;
  while ALink <> nil do
  begin
    ALink.Event.PrepareTaskLinks;
    ALink := ALink.Next;
  end;
end;

procedure TcxSchedulerEvent.RefreshTaskLinks;
begin
  cxFieldValueToTaskLinks(GetValueDefault(FStorage.FTaskLinksField, Null), TaskLinks);
  FIsDataValid := False;
end;

procedure TcxSchedulerEvent.ResetReferences;
begin
  FTaskLinks.Clear;
  UpdateParentGroup;
end;

procedure TcxSchedulerEvent.RemoveTaskLink(ALink: TcxSchedulerEvent);
begin
  if TaskLinks <> nil then
    TaskLinks.RemoveLink(ALink);
end;

procedure TcxSchedulerEvent.RemoveTaskLinkOwnersReferences;
var
  I: Integer;
begin
  for I := TaskLinkOwners.Count - 1 downto 0 do
    if TaskLinkOwners[I] <> nil then
      TaskLinkOwners[I].RemoveTaskLink(Self);
end;

procedure TcxSchedulerEvent.ResetReminderResourcesData;
begin
  if Storage.IsReminderByResourceAvailable then
  begin
    if Reminder and Shared then
      SetValue(Storage.FReminderResourcesData, cxReminderResourcesDataToFieldValue(CreateReminderResourcesData))
    else
      SetValue(Storage.FReminderResourcesData, Null);
  end;
end;

procedure TcxSchedulerEvent.SetActualTimeRange(
  const ActualStart, ActualFinish: TDateTime);
begin
  SetValue(FStorage.FActualStartField, Integer(Trunc(ActualStart + TimeBias)));
  SetValue(FStorage.FActualFinishField, Integer(Trunc(ActualFinish + TimeBias)));
end;

procedure TcxSchedulerEvent.SetRecurrenceInfoValue(
  const AValue: AnsiString);
begin
  SetValue(FStorage.FRecurrenceInfoField, AValue);
end;

procedure TcxSchedulerEvent.SetValue(
  AField: TcxCustomSchedulerStorageField; const AValue: Variant);
begin
  SetValueByIndex(AField.Index, AValue);
  IsDataValid := False;
end;

procedure TcxSchedulerEvent.SetValueByIndex(
  AIndex: Integer; const AValue: Variant);
var
  V: Variant;
begin
  Modified;
  V := Storage.GetEventActualValue(Self, AIndex);
  if not IsNewEvent and (not Storage.GetIsBoundMode or Storage.IsUpdatingMode) then
  begin
    if (EditingRecordHandle = nil) or (Storage.IsUpdatingMode and (RecordHandle <> nil)) then
    begin
      Storage.SetValue(RecordHandle, AIndex, AValue);
      if not Storage.FIsLoading then
        Storage.DoEventModified(Self);
      Storage.Changed;
    end
    else
      if EditingRecordHandle <> nil then
        Storage.SetValue(EditingRecordHandle, AIndex, AValue);
    if not Storage.GetIsBoundMode then Exit;
  end;
  if (VarType(AValue) = VarType(V)) and VarEquals(AValue, V) or Storage.FIsLoading then Exit;
  BeginEditing;
  try
    Storage.SetEventValue(Self, AIndex, AValue);
  finally
    EndEditing;
  end;
end;

procedure TcxSchedulerEvent.TaskLinksChanged(Sender: TcxSchedulerEventLinks);
begin
  if (Sender = FTaskLinks) and not Storage.IsUpdatingMode then
    SetValue(FStorage.FTaskLinksField, cxTaskLinksToFieldValue(TaskLinks));
end;

procedure TcxSchedulerEvent.UpdateReminderDate;
begin
  ReminderDate := Start - MinuteToTime * ReminderMinutesBeforeStart;
  RecurrenceInfo.DismissDate := dxDateOf(ReminderDate) - 1;
end;

function TcxSchedulerEvent.TimeBias: Double;
begin
  Result := Storage.TimeBias;
end;

function TcxSchedulerEvent.ValidateOccurrenceTimeBounds(AOccurrence: TcxSchedulerEvent;
  const ANewStart, ANewFinish: TDateTime): TcxOccurrenceDateStatus;
begin
  Result := odsValid;
  if EventType <> etPattern then
    Exit;
  with TcxSchedulerOccurrenceCalculator.Create(Self, 0, cxMaxDate) do
  try
    Result := ValidateTimeBoundsForOccurrence(AOccurrence, AOccurrence.Start, AOccurrence.Finish, ANewStart, ANewFinish);
  finally
    Free;
  end;
end;

procedure TcxSchedulerEvent.UpdateParentGroup;
begin
  ParentGroup := Storage.GetEventByID(GroupID);
  if ParentGroup <> nil then
    ParentGroup.IsGroup := True;
end;

procedure TcxSchedulerEvent.UpdateTemporaryData;

  function CanUseTimeCalculator: Boolean;
  begin
    Result := IsGroup or (ParentGroup <> nil) or
      ((TaskLinks <> nil) and (TaskLinks.Count <> 0)) or
      ((TaskLinkOwners <> nil) and (TaskLinkOwners.Count <> 0));
  end;

var
  I: Integer;
  ATimeCalculator: TcxSchedulerEventTimeCalculator;
begin
  if IsDataValid then
    Exit;
  FIsDataValid := True;
  FInfo.EventType := GetValueDefault(FStorage.FEventTypeField, etNone);
  FInfo.State := GetValueDefault(FStorage.FStateField, 0);
  FInfo.RecurrenceInfo := dxVariantToAnsiString(GetValueDefault(FStorage.FRecurrenceInfoField, ''));
  FInfo.ActualStart := Integer(GetValueDefault(FStorage.FActualStartField,
    IfThen(FInfo.EventType = etNone, FInfo.DayStart, 0)));
  FInfo.ActualFinish := Integer(GetValueDefault(FStorage.FActualFinishField,
    IfThen(FInfo.EventType = etNone, FInfo.DayFinish, cxMaxDate)));
  FInfo.ResourceID := GetValueDefault(FStorage.FResourceIDField, Null);

  if dxVarIsBlob(FInfo.ResourceID) then
    FInfo.ResourceID := cxFieldValueToVariant(FInfo.ResourceID);

  if FInfo.ResourceIDHashTable = nil then
    FInfo.ResourceIDHashTable := TdxVariantDictionary<Integer>.Create()
  else
    FInfo.ResourceIDHashTable.Clear;

  FInfo.Visible := False;
  if VarIsArray(FInfo.ResourceID) then
    for I := VarArrayLowBound(FInfo.ResourceID, 1) to VarArrayHighBound(FInfo.ResourceID, 1) do
    begin
      FInfo.ResourceIDHashTable.AddOrSetValue(VarToStr(FInfo.ResourceID[I]), I);
      FInfo.Visible := FInfo.Visible or Storage.Resources.IsResourceVisible(FInfo.ResourceID[I]);
    end
  else
  begin
    FInfo.ResourceIDHashTable.AddOrSetValue(VarToStr(FInfo.ResourceID), 0);
    FInfo.Visible := VarIsNull(FInfo.ResourceID) or Storage.Resources.IsResourceVisible(FInfo.ResourceID);
  end;

  if (FPattern <> nil) and (FInfo.EventType <> etPattern) then
    FInfo.Options := GetValueDefault(FStorage.FOptionsField, FPattern.GetOptionsFlag)
  else
    FInfo.Options := GetValueDefault(FStorage.FOptionsField, 0);
  if CanUseTimeCalculator then
  begin
    ATimeCalculator := TcxSchedulerEventTimeCalculator.Create(Self);
    try
      ATimeCalculator.Calculate;
      FInfo.Start := ATimeCalculator.Start;
      FInfo.Finish := ATimeCalculator.Finish;
      FInfo.Duration := DateTimeHelper.RoundTime(ATimeCalculator.Duration);
    finally
      ATimeCalculator.Free;
    end;
  end
  else
  begin
    FInfo.Finish := TDateTime(GetValueDefault(FStorage.FFinishField, 0)) - TimeBias;
    FInfo.Start := TDateTime(GetValueDefault(FStorage.FStartField, 0)) - TimeBias;
    FInfo.Duration := FInfo.Finish - FInfo.Start;
  end;
  FInfo.DayStart := Trunc(FInfo.Start);
  FInfo.DayFinish := Trunc(FInfo.Finish);
end;

function TcxSchedulerEvent.GetActualFinish: Integer;
begin
  CheckCalculated;
  Result := FInfo.ActualFinish;
end;

function TcxSchedulerEvent.GetActualStart: Integer;
begin
  CheckCalculated;
  Result := FInfo.ActualStart;
end;

function TcxSchedulerEvent.GetActualLabelColor: Integer;
begin
  Result := LabelColor;
  if (Result = clDefault) and (GetResourceItem <> nil) then
    Result := GetResourceItem.ActualEventColor;
end;

function TcxSchedulerEvent.GetAllDayEvent: Boolean;
begin
  Result := (GetOptionsFlag and omAllDayEvent) <> 0;
end;

function TcxSchedulerEvent.GetCaption: string;
begin
  Result := GetValueDefault(FStorage.FCaptionField, '');
end;

function TcxSchedulerEvent.GetDuration: TDateTime;
begin
  Result := DateTimeHelper.RoundTime(FInfo.Duration);
end;

function TcxSchedulerEvent.GetEditValue(AIndex: Integer): Variant;
begin
  if IsEditing then
    Result := Storage.GetEventValue(FEditingRecordHandle, AIndex)
  else
    Result := Null;
end;

function TcxSchedulerEvent.GetEnabled: Boolean;
begin
  Result := (GetOptionsFlag and omEnabled) <> 0;
end;

function TcxSchedulerEvent.GetEventType: TcxEventType;
begin
  Result := GetValueDefault(FStorage.FEventTypeField, etNone);
end;

function TcxSchedulerEvent.GetFinish: TDateTime;
begin
  CheckCalculated;
  Result := FInfo.Finish;
end;

function TcxSchedulerEvent.GetGroupID: Variant;
begin
  Result := GetValueDefault(FStorage.FGroupIDField, Null);
end;

function TcxSchedulerEvent.GetIsNewEvent: Boolean;
begin
  Result := Storage.FNewEvents.IndexOf(Self) >= 0;
end;

function TcxSchedulerEvent.GetIsBoundMode: Boolean;
begin
  Result := Storage.GetIsBoundMode;
end;

function TcxSchedulerEvent.GetIsEditing: Boolean;
begin
  Result := FEditCount > 0;
end;

function TcxSchedulerEvent.GetIsGroup: Boolean;
begin
  CheckCalculated;
  Result := ((FInfo.Options and omGroup) <> 0);
end;

function TcxSchedulerEvent.GetLabelColor: Integer;
begin
  Result := GetValueDefault(FStorage.FLabelColorField, clDefault);
end;

function TcxSchedulerEvent.GetLocation: string;
begin
  Result := GetValueDefault(FStorage.FLocationField, '');
end;

function TcxSchedulerEvent.Getmessage: string;
begin
  Result := GetValueDefault(FStorage.FMessageField, '');
end;

function TcxSchedulerEvent.GetOptionsFlag: Integer;
begin
  CheckCalculated;
  Result := FInfo.Options;
end;

function TcxSchedulerEvent.GetParentID: Variant;
begin
  Result := GetValueDefault(FStorage.FParentIDField, Integer(cxSchedulerEventDefaultParentID));
end;

function TcxSchedulerEvent.GetReadOnly: Boolean;
begin
  Result := (GetResourceItem <> nil) and not GetResourceItem.CanModify;
end;

function TcxSchedulerEvent.GetRecurrenceIndex: Integer;
begin
{  if EventType = etNone then
    Result := -1
  else}
    Result := GetValueDefault(FStorage.FRecurrenceIndexField, -1)
end;

function TcxSchedulerEvent.GetReminder: Boolean;
begin
  Result := (GetOptionsFlag and omReminder) <> 0;
end;

function TcxSchedulerEvent.GetReminderDate: TDateTime;
var
  AValue: Variant;
begin
  AValue := Storage.GetEventActualValue(Self, FStorage.FReminderDateField.Index);
  if VarType(AValue) in [varNull, varEmpty] then
    Result := Start
  else
    Result := TDateTime(AValue) - TimeBias;
end;

function TcxSchedulerEvent.GetReminderMinutesBeforeStart: Integer;
begin
  Result := GetValueDefault(FStorage.FReminderMinutesBeforeStartField, Integer(-2));
end;

function TcxSchedulerEvent.GetReminderResourcesData: TcxSchedulerReminderResourcesData;
begin
  Result := cxFieldValueToReminderResourcesData(GetValueDefault(FStorage.FReminderResourcesData, Null));
end;

function TcxSchedulerEvent.GetResourceID: Variant;
begin
  CheckCalculated;
  Result := FInfo.ResourceID;
end;

function TcxSchedulerEvent.GetResourceIDCount: Integer;
var
  AResources: Variant;
begin
  AResources := ResourceID;
  if VarIsNull(AResources) then
  begin
    Result := 0;
    Exit;
  end;
  if VarIsArray(AResources) then
    Result := VarArrayHighBound(AResources, 1) - VarArrayLowBound(AResources, 1) + 1
  else
    Result := 1;
end;

function TcxSchedulerEvent.GetResourceIDs(Index: Integer): Variant;
var
  AResources: Variant;
begin
  AResources := ResourceID;
  if VarIsArray(AResources) then
    Result := AResources[Index]
  else
    Result := AResources;
end;

function TcxSchedulerEvent.GetShared: Boolean;
begin
  Result := VarIsArray(ResourceID) and ((VarArrayHighBound(ResourceID, 1) -
    VarArrayLowBound(ResourceID, 1)) > 0);
end;

function TcxSchedulerEvent.GetStart: TDateTime;
begin
  CheckCalculated;
  Result := FInfo.Start;
end;

function TcxSchedulerEvent.GetState: Integer;
begin
  CheckCalculated;
  Result := FInfo.State;
end;

function TcxSchedulerEvent.GetTaskIndex: Integer;
begin
  Result := GetValueDefault(FStorage.FTaskIndexField, -1);
end;

function TcxSchedulerEvent.GetTaskComplete: Integer;
begin
  Result := GetValueDefault(FStorage.FTaskCompleteField, 0);
end;

function TcxSchedulerEvent.GetTaskStatus: TcxSchedulerEventTaskStatus;
var
  AStatus: Integer;
begin
  AStatus := GetValueDefault(FStorage.FTaskStatusField, 0);
  if (AStatus < 0) or (Integer(AStatus) > Integer(High(TcxSchedulerEventTaskStatus))) then
    Result := tsNotStarted
  else
    Result := TcxSchedulerEventTaskStatus(AStatus);
end;

function TcxSchedulerEvent.GetUTCFinish: TDateTime;
begin
  Result := DateTimeHelper.RoundTime(Finish + TimeBias);
end;

function TcxSchedulerEvent.GetUTCStart: TDateTime;
begin
  Result := DateTimeHelper.RoundTime(Start + TimeBias);
end;

function TcxSchedulerEvent.GetValueCount: Integer;
begin
  Result := FStorage.FieldCount;
end;

function TcxSchedulerEvent.GetValueDef(AFieldIndex: Integer): TcxValueDef;
begin
  Result := TcxCustomDataHelper.GetValueDef(DataController, AFieldIndex);
end;

procedure TcxSchedulerEvent.InternalSetFinish(const AValue: TDateTime);
begin
  SetValue(FStorage.FFinishField, DateTimeHelper.RoundTime(AValue) + TimeBias);
end;

procedure TcxSchedulerEvent.InternalSetStart(const AValue: TDateTime);
begin
  SetValue(FStorage.FStartField, DateTimeHelper.RoundTime(AValue) + TimeBias);
end;

procedure TcxSchedulerEvent.InternalSetTaskComplete(const AValue: Integer; AUpdateTaskStatus: Boolean = True);
begin
  if TaskComplete = AValue then Exit;
  if (AValue = 100) and (TaskComplete <> 100) then
    FPrevTaskComplete := TaskComplete
  else
    if AValue = 0 then
      FPrevTaskComplete := 0;
  BeginEditing;
  try
    SetValue(FStorage.FTaskCompleteField, AValue);
    if AUpdateTaskStatus then
    begin
      if AValue = 100 then
        InternalSetTaskStatus(tsComplete, False)
      else
         if (AValue = 0) and not (TaskStatus in [tsWaiting, tsDeferred]) then
          InternalSetTaskStatus(tsNotStarted, False)
         else
          InternalSetTaskStatus(tsInProgress, False);
    end;
  finally
    EndEditing;
  end;
  Post;
  IsDataValid := False;
end;

procedure TcxSchedulerEvent.InternalSetTaskStatus(
  AValue: TcxSchedulerEventTaskStatus; AUpdateTaskComplete: Boolean = True);
begin
  if TaskStatus = AValue then Exit;
  BeginEditing;
  try
    SetValue(FStorage.FTaskStatusField, AValue);
    if AUpdateTaskComplete then
      case AValue of
        tsNotStarted:
          InternalSetTaskComplete(0, False);
        tsComplete:
          InternalSetTaskComplete(100, False);
        else
          InternalSetTaskComplete(FPrevTaskComplete, False);
      end;
  finally
    EndEditing;
  end;
end;

procedure TcxSchedulerEvent.SetAllDayEvent(const AValue: Boolean);
begin
  if SetOptionsFlag(omAllDayEvent, AValue) then
  begin
    if AValue then
    begin
      Start := dxDateOf(Start);
      if dxDateOf(Finish) <= Start then
        Finish := Start + 1
      else
        Finish := dxDateOf(Finish)
    end
    else
    begin
      Start := dxDateOf(Start) + cxTime8AM;
      Finish := dxDateOf(Finish) + cxTime8AM + cxHalfHour;
    end;
  end;
end;

procedure TcxSchedulerEvent.SetCaption(const AValue: string);
begin
  SetValue(FStorage.FCaptionField, AValue);
end;

procedure TcxSchedulerEvent.SetDuration(const AValue: TDateTime);
begin
  Finish := Start + DateTimeHelper.RoundTime(AValue);
  CheckCalculated;
end;

procedure TcxSchedulerEvent.SetEditValue(AIndex: Integer;
  const AValue: Variant);
begin
  if not IsEditing then Exit;
  Storage.SetEventValue(Self, AIndex, AValue);
end;

procedure TcxSchedulerEvent.SetEnabled(const AValue: Boolean);
begin
  SetOptionsFlag(omEnabled, AValue);
end;

procedure TcxSchedulerEvent.SetEventType(AValue: TcxEventType);
var
  AResetRecurrence: Boolean;
begin
  try
    CheckLinksOnChangeEventType(AValue);
    AResetRecurrence := FStorage.IsRecurrenceAvailable;
    if not AResetRecurrence then
      AValue := etNone;
    AResetRecurrence := AResetRecurrence and (AValue = etNone) and
      (AValue <> EventType);
    SetValue(FStorage.FEventTypeField, AValue);
    if AResetRecurrence then
      RecurrenceInfo.AssignDefaultValues;
    if AValue = etException then
      Storage.EditingEventInfoList.CloseExceptionEditor(Self);
  finally
    IsDataValid := False;
  end;
end;

procedure TcxSchedulerEvent.SetFinish(const AValue: TDateTime);
begin
  InternalSetFinish(AValue);
  IsDataValid := False;
end;

procedure TcxSchedulerEvent.SetGroupID(const AValue: Variant);
begin
  SetValue(FStorage.FGroupIDField, AValue);
end;

procedure TcxSchedulerEvent.SetID(const AValue: Variant);
begin
  FID := AValue;
  if Storage.IsUpdatingMode then
  begin
    FPattern := nil;
    IsDataValid := False;
  end;
end;

procedure TcxSchedulerEvent.SetIsDataValid(Value: Boolean);
begin
  if IsDataValid <> Value then
  begin
    FIsDataValid := Value;
    if not Value then
      InvalidateReferences;
  end;
end;

procedure TcxSchedulerEvent.SetIsGroup(const AValue: Boolean);
begin
  SetOptionsFlag(omGroup, AValue);
end;

procedure TcxSchedulerEvent.SetLabelColor(const AValue: Integer);
begin
  SetValue(FStorage.FLabelColorField, AValue);
end;

procedure TcxSchedulerEvent.SetLocation(const AValue: string);
begin
  SetValue(FStorage.FLocationField, AValue);
end;

procedure TcxSchedulerEvent.SetMessage(const AValue: string);
begin
  SetValue(FStorage.FMessageField, AValue);
end;

function TcxSchedulerEvent.SetOptionsFlag(const AMask: Integer;
  AValue: Boolean): Boolean;
begin
  Result := (GetOptionsFlag and AMask = AMask) <> AValue;
  if AValue then
    FInfo.Options := FInfo.Options or AMask
  else
    FInfo.Options := FInfo.Options and not AMask;
  SetValue(FStorage.FOptionsField, FInfo.Options);
  IsDataValid := False;
end;

procedure TcxSchedulerEvent.SetParentGroup(AValue: TcxSchedulerEvent);
begin
  if ParentGroup <> AValue then
  begin
    Storage.BeginStructureUpdating;
    try
      IsDataValid := False;
      if ParentGroup <> nil then
        ParentGroup.RemoveItem(Self);
      if AValue <> nil then
        AValue.AddItem(Self);
    finally
      Storage.EndStructureUpdating;
      if not Storage.IsUpdatingMode then
      begin
        Storage.UpdateStructure;
        Storage.Changed;
      end;
      IsDataValid := False;
    end;
  end
end;

procedure TcxSchedulerEvent.SetParentID(const AValue: Variant);
begin
  SetValue(FStorage.FParentIDField, AValue);
end;

procedure TcxSchedulerEvent.SetRecurrenceIndex(const AValue: Integer);
begin
  SetValue(FStorage.FRecurrenceIndexField, AValue);
end;

procedure TcxSchedulerEvent.SetRecurrenceInfo(
  AValue: TcxSchedulerEventRecurrenceInfo);
begin
  FRecurrenceInfo.Assign(AValue);
end;

procedure TcxSchedulerEvent.SetReminder(const AValue: Boolean);
begin
  SetOptionsFlag(omReminder, AValue);
  ResetReminderResourcesData;
end;

procedure TcxSchedulerEvent.SetReminderDate(AValue: TDateTime);
begin
  SetValue(FStorage.FReminderDateField, AValue + TimeBias);
  ResetReminderResourcesData;
end;

procedure TcxSchedulerEvent.SetReminderMinutesBeforeStart(const AValue: Integer);
var
  AChanged: Boolean;
begin
  AChanged := ReminderMinutesBeforeStart <> AValue;
  if AChanged then
    ResetReminderResourcesData;
  SetValue(FStorage.FReminderMinutesBeforeStartField, AValue);
  if AChanged then
    UpdateReminderDate;
end;

procedure TcxSchedulerEvent.SetReminderResourcesData(const AValue: TcxSchedulerReminderResourcesData);
begin
  SetValue(FStorage.FReminderResourcesData, cxReminderResourcesDataToFieldValue(AValue));
end;

procedure TcxSchedulerEvent.SetResourceID(const AValue: Variant);
begin
  SetValue(FStorage.FResourceIDField,
    cxVariantToFieldValue(AValue, FStorage.FResourceIDField.IsBlob));
  ResetReminderResourcesData;
end;

procedure TcxSchedulerEvent.SetStart(const AValue: TDateTime);
begin
  InternalSetStart(AValue);
  IsDataValid := False;
  UpdateReminderDate;
end;

procedure TcxSchedulerEvent.SetState(const AValue: Integer);
begin
  SetValue(FStorage.FStateField, AValue);
end;

procedure TcxSchedulerEvent.SetTaskComplete(const AValue: Integer);
begin
  InternalSetTaskComplete(AValue);
end;

procedure TcxSchedulerEvent.SetTaskIndex(const AValue: Integer);
begin
  SetValue(FStorage.FTaskIndexField, AValue);
end;

procedure TcxSchedulerEvent.SetTaskStatus(AValue: TcxSchedulerEventTaskStatus);
begin
  InternalSetTaskStatus(AValue);
end;

procedure TcxSchedulerEvent.SetUTCFinish(const AValue: TDateTime);
begin
  Finish := AValue - TimeBias;
end;

procedure TcxSchedulerEvent.SetUTCStart(const AValue: TDateTime);
begin
  Start := AValue - TimeBias;
end;

{ TcxSchedulerControlEvent }

constructor TcxSchedulerControlEvent.Create(
  AStorage: TcxCustomSchedulerStorage; ADisableEditing: Boolean = False);
begin
  FStorage := AStorage;
  FID := Null;
  CreateSubClasses;
  AssignDefaultValues;
  FLockedResource := Null;
end;

constructor TcxSchedulerControlEvent.Create(ASource: TcxSchedulerEvent);
begin
  inherited Create(ASource.FStorage);
  InitFromSource(ASource);
end;

procedure TcxSchedulerControlEvent.InitFromSource(ASource: TcxSchedulerEvent);
begin
  if ASource = nil then
    Exit;
  FLockedResource := Null;
  FTimeBias := ASource.TimeBias;
  FIsClone := ASource is TcxSchedulerControlEvent;
  SetSource(ASource);
  FLink := ASource.FLink;
  FID := ASource.ID;
  Assign(ASource);
end;

constructor TcxSchedulerControlEvent.Create(
  ASource: TcxSchedulerEvent; const AStart, AFinish: TDateTime);
begin
  Create(ASource);
  Start := AStart;
  Finish := AFinish;
  FLockedResource := Null;
end;

constructor TcxSchedulerControlEvent.Create(
  ASource, AOriginal: TcxSchedulerEvent);
begin
  Create(ASource);
  FTimeBias := AOriginal.TimeBias;
  UTCStart := AOriginal.UTCStart;
  UTCFinish := AOriginal.UTCFinish;
end;

destructor TcxSchedulerControlEvent.Destroy;
begin
  if FIsClone and (Source is TcxSchedulerControlEvent) then
    TcxSchedulerControlEvent(Source).FIsSource := False;
  Storage.FreeRecord(FRecordHandle);
  inherited Destroy;
end;

procedure TcxSchedulerControlEvent.Assign(Source: TPersistent);

  function GetPattern: TcxSchedulerEvent;
  begin
    if IsClone then
      Result := TcxSchedulerEvent(Source).Pattern
    else
      if TcxSchedulerEvent(Source).EventType = etPattern then
        Result := TcxSchedulerEvent(Source)
      else
        Result := nil;
  end;

var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  if Source is TcxSchedulerEvent then
  begin
    AEvent := TcxSchedulerEvent(Source);
    Data := AEvent.Data;
    for I := 0 to ValueCount - 1 do
      SetValueByIndex(I, TcxSchedulerEvent(Source).GetValueByIndex(I));
    InternalSetStart(AEvent.Start);
    InternalSetFinish(AEvent.Finish);
    IsDataValid := False;
    FPattern := GetPattern;
    FTimeBias := AEvent.TimeBias;
  end;
end;

procedure TcxSchedulerControlEvent.AssignDefaultValues;
begin
  FRecordHandle := Storage.AllocateRecord;
  inherited AssignDefaultValues;
end;

procedure TcxSchedulerControlEvent.BeginEditing;
begin
  // do nothing for
end;

procedure TcxSchedulerControlEvent.Delete;
begin
  if EventType = etOccurrence then
    inherited Delete
  else
    if Source <> nil then
      Source.Delete;
end;

procedure TcxSchedulerControlEvent.EndEditing;
begin
end;

function TcxSchedulerControlEvent.GetTaskLinkOwnerRelation(ATaskLinkOwner: TcxSchedulerEvent; var ARelation: TcxSchedulerEventRelation): Boolean;
begin
  if Source <> nil then
    Result := Source.GetTaskLinkOwnerRelation(ATaskLinkOwner, ARelation)
  else
    Result := inherited GetTaskLinkOwnerRelation(ATaskLinkOwner, ARelation);
end;

procedure TcxSchedulerControlEvent.DoGetValidTaskTimeRange(var AStart, AFinish: TDateTime);
begin
  if Source <> nil then
    Source.DoGetValidTaskTimeRange(AStart, AFinish)
  else
    inherited DoGetValidTaskTimeRange(AStart, AFinish);
end;

function TcxSchedulerControlEvent.IsOrigin(AEvent: TcxSchedulerEvent): Boolean;
begin
  if IsClone then
    Result := TcxSchedulerControlEvent(Source).IsOrigin(AEvent)
  else
    Result := Source = AEvent;
end;

procedure TcxSchedulerControlEvent.lockResource(const ALockedResource: Variant);
begin
  FLockedResource := ALockedResource;
end;

function TcxSchedulerControlEvent.OpenedInEditor: Boolean;
begin
  Result := Storage.IsEventEditing(Self);
end;

procedure TcxSchedulerControlEvent.unlockResource;
begin
  FLockedResource := Null;
end;

function TcxSchedulerControlEvent.CanMoveTo(ANewTime: TDateTime): Boolean;
begin
  Result := (Source = nil) or Source.CanMoveTo(ANewTime);
end;

function TcxSchedulerControlEvent.CanMove: Boolean;
var
  AResource: TcxSchedulerStorageResourceItem;
begin
  AResource := GetResourceItem;
  Result := (AResource = nil) or AResource.CanModify;
end;

function TcxSchedulerControlEvent.CanResize: Boolean;
var
  AResource: TcxSchedulerStorageResourceItem;
begin
  AResource := GetResourceItem;
  Result := (AResource = nil) or AResource.CanModify and (not IsGroup or not HasChildren);
end;

procedure TcxSchedulerControlEvent.CheckLinksOnChangeEventType(ANewEventType: TcxEventType);
begin
  // don't delete
end;

function TcxSchedulerControlEvent.CreateTaskLinks: TcxSchedulerEventLinks;
begin
  Result := nil;
end;

function TcxSchedulerControlEvent.GetGroupItems: TcxSchedulerEventGroupItems;
begin
  if Source <> nil then
    Result := Source.GroupItems
  else
    Result := inherited GetGroupItems;
end;

function TcxSchedulerControlEvent.GetIndex: Integer;
begin
  Result := FIndex;
  if Source <> nil then
    Result := Source.Index;
end;

function TcxSchedulerControlEvent.GetParentID: Variant;
begin
  if Source = nil then
    Result := Null
  else
    Result := Source.ParentID;
end;

function TcxSchedulerControlEvent.GetReadOnly: Boolean;
begin
  if Source = nil then
    Result := inherited GetReadOnly
  else
    Result := Source.ReadOnly;
end;

function TcxSchedulerControlEvent.GetTaskIndex: Integer;
begin
  if IsClone then
    Result := Source.TaskIndex
  else
    if Pattern <> nil then
      Result := Pattern.TaskIndex
    else
      Result := inherited GetTaskIndex;
end;

function TcxSchedulerControlEvent.GetTaskLinks: TcxSchedulerEventLinks;
begin
  if Source <> nil then
    Result := Source.TaskLinks
  else
    Result := inherited GetTaskLinks;
end;

function TcxSchedulerControlEvent.GetTaskLinkOwners: TcxSchedulerEventList;
begin
  if Source <> nil then
    Result := Source.TaskLinkOwners
  else
    Result := inherited GetTaskLinkOwners;
end;

function TcxSchedulerControlEvent.GetValueByIndex(
  AIndex: Integer): Variant;
begin
  Result := TcxDataStorageHelper.GetValue(ValueDefs[AIndex], FRecordHandle);
  if (Pattern <> nil) and VarIsNull(Result) and Storage.CanGetValueFromPattern(AIndex) then
    Result := Pattern.GetValueByIndex(AIndex);
  if (AIndex = FStorage.FResourceIDField.Index) and not VarIsNull(FLockedResource) then
    Result := FLockedResource;
end;

procedure TcxSchedulerControlEvent.InvalidateReferences;
begin
// do nothing
end;

function TcxSchedulerControlEvent.IsParentGroupForEvent(AEvent: TcxSchedulerEvent): Boolean;

  function CheckBySources(AEvent: TcxSchedulerControlEvent): Boolean;
  begin
    Result := (Source <> nil) and Source.IsParentGroupForEvent(AEvent.Source);
  end;

var
  AControlEvent: TcxSchedulerControlEvent;
begin
  Result := False;
  if not (AEvent is TcxSchedulerControlEvent) then
    Exit;
  AControlEvent := TcxSchedulerControlEvent(AEvent);
  Result := inherited IsParentGroupForEvent(AControlEvent) or CheckBySources(AControlEvent);
end;

procedure TcxSchedulerControlEvent.RemoveTaskLinkOwnersReferences;
begin
// do nothing
end;

procedure TcxSchedulerControlEvent.PrepareTaskLinks;
begin
// do nothing
end;

procedure TcxSchedulerControlEvent.SetSource(ASource: TcxSchedulerEvent);
begin
  FSource := ASource;
end;

procedure TcxSchedulerControlEvent.SetValueByIndex(
  AIndex: Integer; const AValue: Variant);
begin
  Storage.SetValue(RecordHandle, AIndex, AValue);
  Modified;
end;

function TcxSchedulerControlEvent.StorageTimeBias: Double;
begin
  Result := Storage.TimeBias;
end;

function TcxSchedulerControlEvent.TimeBias: Double;
begin
  Result := FTimeBias;
end;

function TcxSchedulerControlEvent.GetNonExceptionLinkCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  if TaskLinks = nil then Exit;
  for I := 0 to TaskLinks.Count - 1 do
    if (TaskLinks[I].Link <> nil) and (TaskLinks[I].Link.EventType <> etException) then
      Inc(Result);
end;

function TcxSchedulerControlEvent.GetSelected: Boolean;
begin
  if SelectionAdapter <> nil then
    Result := SelectionAdapter.IsSelected(Self)
  else
  begin
    if Source = nil then
      Result := True
    else
      Result := FIsClone;
  end;
end;

procedure TcxSchedulerControlEvent.SetPattern(AValue: TcxSchedulerEvent);
begin
  FPattern := AValue;
end;

procedure TcxSchedulerControlEvent.SetSelected(AValue: Boolean);
begin
  if (AValue <> GetSelected) and (SelectionAdapter <> nil) then
    SelectionAdapter.Add(Self, [ssCtrl]);
end;

{ TcxSchedulerStorageResourceItems }

constructor TcxSchedulerStorageResourceItems.Create(
  AOwner: TcxSchedulerStorageResources;
  AItemClass: TcxSchedulerStorageResourceItemClass);
begin
  FOwner := AOwner;
  FVisibleList := TdxFastList.Create;
  inherited Create(AItemClass);
end;

destructor TcxSchedulerStorageResourceItems.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FVisibleList);
end;

function TcxSchedulerStorageResourceItems.Add: TcxSchedulerStorageResourceItem;
begin
  Result := TcxSchedulerStorageResourceItem(inherited Add);
end;

function TcxSchedulerStorageResourceItems.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxSchedulerStorageResourceItems.Update(Item: TCollectionItem);
var
  I: Integer;
  AItem: TcxSchedulerStorageResourceItem;
begin
  if Resources.IsLocked then
    Exit;
  inherited Update(Item);
  FVisibleList.Clear;
  FVisibleList.Capacity := Count;
  for I := 0 to Count - 1 do
  begin
    AItem := Items[I];
    if AItem.Visible then
      FVisibleList.Add(AItem);
  end;
  Resources.Changed;
end;

function TcxSchedulerStorageResourceItems.GetItem(
  AIndex: Integer): TcxSchedulerStorageResourceItem;
begin
  Result := TcxSchedulerStorageResourceItem(inherited Items[AIndex]);
end;

function TcxSchedulerStorageResourceItems.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := FOwner.Storage;
end;

procedure TcxSchedulerStorageResourceItems.SetItem(
  AIndex: Integer; AValue: TcxSchedulerStorageResourceItem);
begin
  Items[AIndex].Assign(AValue);
end;

function TcxSchedulerStorageResourceItems.GetVisibleResource(
  AIndex: Integer): TcxSchedulerStorageResourceItem;
begin
  Result := nil;
  if AIndex < FVisibleList.Count then
    Result := FVisibleList[AIndex];
end;

function TcxSchedulerStorageResourceItems.GetVisibleResourceCount: Integer;
begin
  Result := FVisibleList.Count;
end;

{ TcxSchedulerStorageResourceItem }

constructor TcxSchedulerStorageResourceItem.Create(Collection: TCollection);
begin
  FColor := clDefault;
  FImageIndex := -1;
  FVisible := True;
  FReadOnly := False;
  FWorkDays := DateTimeHelper.WorkDays;
  FWorkFinish := DateTimeHelper.WorkFinish;
  FWorkStart := DateTimeHelper.WorkStart;
  inherited Create(Collection);
end;

procedure TcxSchedulerStorageResourceItem.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerStorageResourceItem then
  begin
    FColor := TcxSchedulerStorageResourceItem(Source).FColor;
    FImageIndex := TcxSchedulerStorageResourceItem(Source).FImageIndex;
    FName := TcxSchedulerStorageResourceItem(Source).FName;
    FReadOnly := TcxSchedulerStorageResourceItem(Source).FReadOnly;
    FResourceID := TcxSchedulerStorageResourceItem(Source).FResourceID;
    FVisible := TcxSchedulerStorageResourceItem(Source).FVisible;
  end
  else
    inherited Assign(Source);
end;

function TcxSchedulerStorageResourceItem.CanModify: Boolean;
begin
  Result := not ReadOnly;
end;

function TcxSchedulerStorageResourceItem.HasAsParent(AItem: TcxSchedulerStorageResourceItem): Boolean;
var
  AItem1: TcxSchedulerStorageResourceItem;
begin
  AItem1 := Self;
  repeat
    Result := AItem1 = AItem;
    AItem1 := AItem1.Parent;
  until Result or (AItem1 = nil);
end;

procedure TcxSchedulerStorageResourceItem.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('WorkStart', ReadWorkStart, WriteWorkStart, FWorkStartAssigned);
  Filer.DefineProperty('WorkFinish', ReadWorkFinish, WriteWorkFinish, FWorkFinishAssigned);
end;

function TcxSchedulerStorageResourceItem.GetDisplayName: string;
begin
  Result := TcxSchedulerStorageResourceItems(Collection).
    Resources.DoGetResourceName(Self);
end;

function TcxSchedulerStorageResourceItem.GetActualEventColor: TColor;
begin
  Result := clDefault;
end;

function TcxSchedulerStorageResourceItem.GetBackgroundColor: TColor;
begin
  Result := Color;
end;

function TcxSchedulerStorageResourceItem.GetReadOnly: Boolean;
begin
  Result := FReadOnly;
end;

function TcxSchedulerStorageResourceItem.GetResourceID: Variant;
begin
  Result := FResourceID;
end;

procedure TcxSchedulerStorageResourceItem.SetName(
  const AValue: string);
begin
  if AValue = FName then Exit;
  FName := AValue;
  Changed(True);
end;

procedure TcxSchedulerStorageResourceItem.SetColor(
  const AValue: TColor);
begin
  FColor := AValue;
  Changed(True);
end;

procedure TcxSchedulerStorageResourceItem.SetImageIndex(
  const AValue: TcxImageIndex);
begin
  FImageIndex := AValue;
  Changed(True);
end;

procedure TcxSchedulerStorageResourceItem.SetReadOnly(const AValue: Boolean);
begin
  FReadOnly := AValue;
end;

procedure TcxSchedulerStorageResourceItem.SetResourceID(
  const AValue: Variant);
begin
  FResourceID := AValue;
  Changed(True);
end;

procedure TcxSchedulerStorageResourceItem.SetVisible(const AValue: Boolean);
begin
  FVisible := AValue;
  Changed(True);
end;

procedure TcxSchedulerStorageResourceItem.SetWorkStart(const AValue: TTime);
begin
  if AValue <> FWorkStart then
  begin
    FWorkStart := DateTimeHelper.RoundTime(AValue);
    FWorkStartAssigned := FWorkStart <> DateTimeHelper.WorkStart;
    Changed(True);
  end;
end;

procedure TcxSchedulerStorageResourceItem.SetWorkDays(AValue: TDays);
begin
  if AValue <> FWorkDays then
  begin
    FWorkDays := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerStorageResourceItem.SetWorkFinish(const AValue: TTime);
begin
  if AValue <> FWorkFinish then
  begin
    FWorkFinish := DateTimeHelper.RoundTime(AValue);
    FWorkFinishAssigned := FWorkFinish <> DateTimeHelper.WorkFinish;
    Changed(True);
  end;
end;

function TcxSchedulerStorageResourceItem.GetActualImageIndex: TcxImageIndex;
begin
  Result := Resources.DoGetResourceImageIndex(Self);
  if (Resources.Images = nil) or (Resources.Images.Count <= Result) then
    Result := -1;
end;

function TcxSchedulerStorageResourceItem.GetActuallyVisible: Boolean;
begin
  Result := Visible and ((FParent = nil) or FParent.ActuallyVisible);
end;

function TcxSchedulerStorageResourceItem.GetResources: TcxSchedulerStorageResources;
begin
  Result := TcxSchedulerStorageResourceItems(Collection).Resources;
end;

function TcxSchedulerStorageResourceItem.IsWorkDaysStored: Boolean;
begin
  Result := FWorkDays <> DateTimeHelper.WorkDays;
end;

procedure TcxSchedulerStorageResourceItem.ReadWorkFinish(AReader: TReader);
begin
  FWorkFinish := AReader.ReadFloat;
end;

procedure TcxSchedulerStorageResourceItem.ReadWorkStart(AReader: TReader);
begin
  FWorkStart := AReader.ReadFloat;
end;

procedure TcxSchedulerStorageResourceItem.SetParent(
  AValue: TcxSchedulerStorageResourceItem);
begin
  if (FParent <> AValue) and not HasAsParent(FParent) then
  begin
    FParent := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerStorageResourceItem.WriteWorkFinish(AWriter: TWriter);
begin
  AWriter.WriteFloat(FWorkFinish);
end;

procedure TcxSchedulerStorageResourceItem.WriteWorkStart(AWriter: TWriter);
begin
  AWriter.WriteFloat(FWorkStart);
end;

{ TcxSchedulerStorageResources }

constructor TcxSchedulerStorageResources.Create(
  AOwner: TcxCustomSchedulerStorage);
begin
  FOwner := AOwner;
  inherited Create(AOwner);
  FItemsVisibility := TdxVariantToObjectDictionary<TcxSchedulerStorageResourceItem>.Create(False);
  FItems := CreateItems;
end;

destructor TcxSchedulerStorageResources.Destroy;
begin
  FItems.Free;
  FItemsVisibility.Free;
  inherited Destroy;
end;

procedure TcxSchedulerStorageResources.Assign(
  Source: TPersistent);
begin
  if Source is TcxSchedulerStorageResources then
  begin
    FItems.Assign(TcxSchedulerStorageResources(Source).Items);
    FImages := TcxSchedulerStorageResources(Source).Images;
  end
  else
    inherited;
end;

procedure TcxSchedulerStorageResources.PopulateItemsAsTreeStructure(AList: TList);

   procedure PopulateForParent(AParent: TcxSchedulerStorageResourceItem);
   var
     I: Integer;
   begin
     for I := 0 to ResourceItems.Count - 1 do
       if ResourceItems[I].Visible and (ResourceItems[I].Parent = AParent) then
       begin
         AList.Add(ResourceItems[I]);
         PopulateForParent(ResourceItems[I]);
       end;
   end;

begin
  PopulateForParent(nil);
end;

function TcxSchedulerStorageResources.GetResourceName(
  AResource: TcxSchedulerStorageResourceItem): string;
begin
  Result := DoGetResourceName(AResource);
end;

function TcxSchedulerStorageResources.GetResourceNameByID(const AResource: Variant): string;
var
  I: Integer;
  AResourceItem: TcxSchedulerStorageResourceItem;
begin
  Result := '';
  if VarIsEmptyEx(AResource) then Exit;
  for I := 0 to ResourceItems.Count - 1 do
  begin
    AResourceItem := ResourceItems[I];
    if VarEqualsSoft(AResource, AResourceItem.ResourceID) then
    begin
      Result := DoGetResourceName(AResourceItem);
      Break;
    end;
  end;
end;

function TcxSchedulerStorageResources.GetObjectName: string;
begin
  if Storage.StoringName <> '' then
    Result := Storage.StoringName
  else
    Result := Storage.Name;
end;

function TcxSchedulerStorageResources.DecodePropertyName(const AName: string;
  var ASubValue: string): Integer;
begin
  Result := StrToIntDef('$' + Copy(AName, 9, 8), -1);
  ASubValue := Copy(AName, 17, Length(AName) - 16);
end;

function TcxSchedulerStorageResources.GetProperties(
  AProperties: TStrings): Boolean;
var
  I: Integer;
  APropertyName: string;
begin
  Result := ResourceItems.Count > 0;
  for I := 0 to ResourceItems.Count - 1 do
    with ResourceItems[I] do
    begin
      APropertyName := 'Resource' + IntToHex(I, 8);
      AProperties.Add(APropertyName);
      AProperties.Add(APropertyName + 'WorkStart');
      AProperties.Add(APropertyName + 'WorkFinish');
      AProperties.Add(APropertyName + 'WorkDays');
    end;
end;

procedure TcxSchedulerStorageResources.GetPropertyValue(const AName: string;
  var AValue: Variant);
var
  AItem: TcxSchedulerStorageResourceItem;
  ASubValue: string;
begin
  if ResourceItems.Count = 0 then Exit;
  if Pos('Resource', AName) = 1 then
  begin
    AItem := TcxSchedulerStorageResourceItem(
      ResourceItems.FindItemID(DecodePropertyName(AName, ASubValue)));
    if AItem <> nil then
    begin
      if SameText(ASubValue, 'WorkStart') then
        AValue := AItem.WorkStart
      else
        if SameText(ASubValue, 'WorkFinish') then
          AValue := AItem.WorkFinish
        else
          if SameText(ASubValue, 'WorkDays') then
            AValue := WorkDaysToInteger(AItem.WorkDays)
          else
            AValue := ((AItem.Index and $FFFF) shl 1) or Ord(AItem.Visible);
    end;
  end;
end;

procedure TcxSchedulerStorageResources.SetPropertyValue(const AName: string;
  const AValue: Variant);
var
  AItem: TcxSchedulerStorageResourceItem;
  ASubValue: string;
  P: PRestoringItem;

  function GetRestoreItem(AItem: TcxSchedulerStorageResourceItem): PRestoringItem;
  var
    I: Integer;
  begin
    Result := nil;
    if FRestoringItems = nil then
      Exit;
    for I := 0 to FRestoringItems.Count - 1 do
      if AItem = PRestoringItem(FRestoringItems.Items[I])^.Item then
      begin
        Result := PRestoringItem(FRestoringItems.Items[I]);
        Exit;
      end;
    New(Result);
    Result^.WorkDays := DateTimeHelper.WorkDays;
    Result^.WorkFinish := DateTimeHelper.WorkFinish;
    Result^.WorkStart := DateTimeHelper.WorkStart;
    Result^.Item := AItem;
    FRestoringItems.Add(Result);
  end;

begin
  if ResourceItems.Count = 0 then Exit;
  if Pos('Resource', AName) = 1 then
  begin
    AItem := TcxSchedulerStorageResourceItem(ResourceItems.FindItemID(
      DecodePropertyName(AName, ASubValue)));
    if AItem <> nil then
    begin
      P := GetRestoreItem(AItem);
      if SameText(ASubValue, 'WorkStart') then
      begin
        if P <> nil then
          P^.WorkStart := AValue
        else
          AItem.WorkStart := AValue;
      end
      else
        if SameText(ASubValue, 'WorkFinish') then
        begin
          if P <> nil then
            P^.WorkFinish := AValue
          else
            AItem.WorkFinish := AValue;
        end
        else
          if SameText(ASubValue, 'WorkDays') then
          begin
            if P <> nil then
              P^.WorkDays := IntegerToWorkDays(AValue)
            else
              AItem.WorkDays := IntegerToWorkDays(AValue);
          end
          else
          begin
            if P <> nil then
            begin
              P^.Index := (AValue shr 1) and $FFFFFFF;
              P^.Visible := Boolean(AValue and 1);
            end
            else
              AItem.Visible := Boolean(AValue and 1);
          end;
    end;
  end;
end;

procedure TcxSchedulerStorageResources.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxSchedulerStorageResources.Changed;
var
  I: Integer;
  AItem: TcxSchedulerStorageResourceItem;
begin
  FItemsVisibility.Clear;
  FIsNullResourceVisible := False;
  for I := 0 to ResourceItems.VisibleResourceCount - 1 do
  begin
    AItem := ResourceItems.VisibleResources[I];
    FIsNullResourceVisible := FIsNullResourceVisible or VarIsNull(AItem.ResourceID);
    FItemsVisibility.AddOrSetValue(VarToStr(AItem.ResourceID), AItem);
  end;
  Storage.UpdateStructure;
  Storage.Changed;
end;

function TcxSchedulerStorageResources.CreateItems: TcxSchedulerStorageResourceItems;
begin
  Result := TcxSchedulerStorageResourceItems.Create(
    Self, TcxSchedulerStorageResourceItem);
end;

function CompareRestoringItems(Item1, Item2: Pointer): Integer;
begin
  Result := PRestoringItem(Item1).Index - PRestoringItem(Item2).Index;
end;

procedure TcxSchedulerStorageResources.DoneRestore;
var
  I: Integer;
  P: PRestoringItem;
begin
  FRestoringItems.Sort(CompareRestoringItems);
  Storage.BeginUpdate;
  try
    for I := 0 to FRestoringItems.Count - 1 do
    begin
      P := FRestoringItems[I];
      P.Item.Index := P.Index;
      P.Item.Visible := P.Visible;
      P.Item.WorkFinish := P.WorkFinish;
      P.Item.WorkStart := P.WorkStart;
      P.Item.WorkDays := P.WorkDays;
      Dispose(P);
    end;
  finally
    Storage.EndUpdate;
    FreeAndNil(FRestoringItems);
  end;
end;

function TcxSchedulerStorageResources.DoGetResourceImageIndex(
  AItem: TcxSchedulerStorageResourceItem): TcxImageIndex;
begin
  Result := AItem.ImageIndex;
  if Assigned(FOnGetResourceImageIndex) then
    FOnGetResourceImageIndex(Storage, AItem, Result);
end;

function TcxSchedulerStorageResources.DoGetResourceName(
  AItem: TcxSchedulerStorageResourceItem): string;
begin
  Result := '';
  if AItem <> nil then
    Result := AItem.Name;
  if Assigned(FOnGetResourceName) then
    FOnGetResourceName(Storage, AItem, Result);
end;

function TcxSchedulerStorageResources.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function TcxSchedulerStorageResources.GetResourceItems: TcxSchedulerStorageResourceItems;
begin
  Result := Items;
end;

procedure TcxSchedulerStorageResources.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    ResourceItems.Update(nil);
//    Changed;
end;

procedure TcxSchedulerStorageResources.InitRestore;
begin
  FRestoringItems := TList.Create;
end;

function TcxSchedulerStorageResources.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TcxSchedulerStorageResources.IsResourceVisible(const AResourceID: Variant): Boolean;
var
  AItem: TcxSchedulerStorageResourceItem;
begin
  Result := (FItemsVisibility.Count = 0) or IsNullResourceVisible;
  if Result then
    Exit;
  Result := FItemsVisibility.TryGetValue(VarToStr(AResourceID), AItem);
  if Result then
    Result := VarEqualsSoftEx(AItem.ResourceID, AResourceID);
end;

function TcxSchedulerStorageResources.GetAreImagesUsed: Boolean;
var
  I: Integer;
begin
  Result := (Images <> nil);
  if Result then
  begin
    Result := False;
    for I := 0 to ResourceItems.Count - 1 do
    begin
      Result := Result or (ResourceItems[I].ActualImageIndex >= 0);
      if Result then Break;
    end;
  end;
end;

procedure TcxSchedulerStorageResources.SetImages(AValue: TCustomImageList);
begin
  if AValue <> FImages then
  begin
    if FImages <> nil then
      FImages.RemoveFreeNotification(Storage);
    FImages := AValue;
    if FImages <> nil then
      FImages.FreeNotification(Storage);
    Changed;
  end;
end;

procedure TcxSchedulerStorageResources.SetItems(
  AValue: TcxSchedulerStorageResourceItems);
begin
  FItems.Assign(AValue);
end;

function cxCompareConflictEvents(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  if AEvent1.Start = AEvent2.Start then
    Result := 0
  else
    if AEvent1.Start < AEvent2.Start then
      Result := -1
    else
      Result := 1;
end;

function cxCompareConflictEventsEx(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  if AEvent1 = AEvent2 then
    Result := 0
  else
    if AEvent1.Finish <= AEvent2.Start then
      Result := -1
    else
      if AEvent1.Start = AEvent2.Start then
        Result := 0
      else
        Result := 1;
end;

{ TcxSchedulerEventConflictsInfo }

constructor TcxSchedulerEventConflictsInfo.Create(
  AEvent: TcxSchedulerEvent; AExceptEventsWithoutResources: Boolean);
begin
  FEvent := AEvent;
  Init(AEvent.Storage, AExceptEventsWithoutResources, AEvent.ResourceID, NullDate, NullDate);
  Calculate;
end;

constructor TcxSchedulerEventConflictsInfo.Create(AEvent: TcxSchedulerEvent;
  AExceptEventsWithoutResources: Boolean; const AStart, AFinish: TDateTime);
begin
  FEvent := AEvent;
  Init(AEvent.Storage, AExceptEventsWithoutResources, AEvent.ResourceID, AStart, AFinish);
  Calculate(False);
end;

constructor TcxSchedulerEventConflictsInfo.Create(
  AStorage: TcxCustomSchedulerStorage; AExceptEventsWithoutResources: Boolean;
  const AStart, AFinish: TDateTime; AResourceID: Variant; AExcludedEvent: TcxSchedulerEvent = nil);
begin
  Init(AStorage, AExceptEventsWithoutResources, AResourceID, AStart, AFinish);
  FExcludedEvent := AExcludedEvent;
  if AExcludedEvent <> nil then
    ExcludedEventID := TcxSchedulerControlEventID.Create(AExcludedEvent);
  Calculate;
end;

destructor TcxSchedulerEventConflictsInfo.Destroy;
begin
  ExcludedEventID.Free;
  FConflictEvents.Free;
  FTimeRanges.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEventConflictsInfo.Init(
  AStorage: TcxCustomSchedulerStorage; AExceptEventsWithoutResources: Boolean;
  AResourceID: Variant; const AStart, AFinish: TDateTime);
begin
  Start := AStart;
  Finish := AFinish;
  FStorage := AStorage;
  FExceptEventsWithoutResources := AExceptEventsWithoutResources;
  FConflictEvents := TcxSchedulerFilteredEventList.Create;
  FTimeRanges := TcxSchedulerTimeRanges.CreateEx(Self);
  ResourceID := AResourceID;
end;

procedure TcxSchedulerEventConflictsInfo.AddRange(AStart, AFinish: TDateTime);
begin
  AStart := Min(Finish, Max(AStart, Start));
  AFinish := Max(Start, Min(AFinish, Finish));
  with DateTimeHelper do
  begin
    if (RoundTime(AStart) <> RoundTime(AFinish)) and (AFinish > AStart) then
      FTimeRanges.Add(AStart, AFinish);
  end;
end;

procedure TcxSchedulerEventConflictsInfo.Calculate(
  ACalculateFreeTime: Boolean = True);
begin
  FConflictEvents.DestroyItems;
  FConflictEvents.ExceptEventsWithoutResources := ExceptEventsWithoutResources;
  FTimeRanges.Clear;
  if (FEvent <> nil) and ACalculateFreeTime then
  begin
    Start := FEvent.Start;
    Finish := FEvent.Finish;
    EventID := TcxSchedulerControlEventID.Create(FEvent);
  end;
  try
    if (FEvent = nil) or not FEvent.IsFreeState then
    begin
      Storage.GetEvents(FConflictEvents, Start - 1, Finish + 1, ResourceID);
      if ACalculateFreeTime then
      begin
        FConflictEvents.Sort(cxCompareConflictEvents);
        CheckIntersectionWithEvents
      end
      else
      begin
        FConflictEvents.Sort(cxCompareConflictEventsEx);
        CheckSomeIntersection;
      end;
    end;
    if ACalculateFreeTime then
      CheckFreeTimeRanges;
  finally
    EventID.Free;
  end;
end;

procedure TcxSchedulerEventConflictsInfo.CheckFreeTimeRanges;
var
  I: Integer;
  AStart, AFinish: TDateTime;
begin
  if not HasConflicts then
    AddRange(Start, Finish)
  else
  begin
    I := 0;
    AStart := Start;
    AFinish := Finish;
    while I < ConflictEvents.Count do
    begin
      AFinish := ConflictEvents[I].Start;
      AddRange(AStart, AFinish);
      ExpandRange(ConflictEvents[I], I, AFinish);
      AStart := AFinish;
    end;
    AddRange(AStart, Finish);
  end;
end;

procedure TcxSchedulerEventConflictsInfo.CheckIntersectionWithEvents;
var
  I: Integer;
begin
  for I := FConflictEvents.Count - 1 downto 0 do
  begin
    if ExcludeFromCalculate(FConflictEvents[I]) then
    begin
      FConflictEvents[I].Free;
      FConflictEvents.Delete(I);
    end;
  end;
end;

procedure TcxSchedulerEventConflictsInfo.CheckSomeIntersection;
var
  I, J: Integer;
  AEvent, ACheckedEvent: TcxSchedulerEvent;
  AList: TcxSchedulerFilteredEventList;
  AHasIntersection: Boolean;
begin
  AList :=  TcxSchedulerFilteredEventList.Create;
  try
    if Event.SkipExceptions or (Event.EventType = etPattern) then
      Event.RecurrenceInfo.GetOccurrences(AList, Start, Finish)
    else
      AList.Add(TcxSchedulerControlEvent.Create(Event));
    // delete equals
    for I := FConflictEvents.Count - 1 downto 0 do
    begin
      AEvent := FConflictEvents[I];
      if AEvent.IsFreeState or IsSameEvent(AEvent) or IsSameSeries(FEvent, AEvent) then
      begin
        AEvent.Free;
        FConflictEvents.Delete(I);
      end;
    end;
    //
    AHasIntersection := False;
    for I := 0 to AList.Count - 1 do
    begin
      if AHasIntersection then Break;
      AEvent := AList[I];
      for J := 0 to FConflictEvents.Count - 1 do
      begin
        ACheckedEvent := FConflictEvents[J];
        AHasIntersection := IntersectEvents(AEvent, ACheckedEvent);
        if AHasIntersection and not ExceptEventsWithoutResources then
          AHasIntersection := not VarIsNull(ACheckedEvent.ResourceID);
        if ExceptEventsWithoutResources and AHasIntersection then
          AHasIntersection := VarIsEmptyEx(ACheckedEvent.ResourceID) or VarIsEmptyEx(AEvent.ResourceID) or
            (ACheckedEvent.IsSharedWithResource(AEvent.ResourceID) or
              AEvent.IsSharedWithResource(ACheckedEvent.ResourceID))
        else
          AHasIntersection := AHasIntersection and (ACheckedEvent.IsSharedWithResource(AEvent.ResourceID) or
            AEvent.IsSharedWithResource(ACheckedEvent.ResourceID));
        AHasIntersection := AHasIntersection and Storage.HasEventIntersect(FEvent, ACheckedEvent);
        if AHasIntersection then Break;
      end;
    end;
    if not AHasIntersection then
      FConflictEvents.DoClear(True);
  finally
    AList.Free;
  end;
end;

function TcxSchedulerEventConflictsInfo.ExcludeFromCalculate(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent.IsFreeState or not IntersectEvents(AEvent);
  if not Result and (ExcludedEventID <> nil) then
    Result := ExcludedEventID.SameEvent(AEvent);
end;

procedure TcxSchedulerEventConflictsInfo.ExpandRange(AEvent: TcxSchedulerEvent;
  var AIndex: Integer; var ALastPosition: TDateTime);
var
  ACheckedEvent: TcxSchedulerEvent;
  ARangeStart, ARangeFinish: TDateTime;
begin
  ARangeStart := AEvent.Start;
  ARangeFinish := AEvent.Finish;
  while AIndex < ConflictEvents.Count do
  begin
     ACheckedEvent := ConflictEvents[AIndex];
     Inc(AIndex);
     if IntersectTime(ARangeStart, ARangeFinish,
       ACheckedEvent.Start, ACheckedEvent.Finish) then
     begin
       ARangeStart := Min(ARangeStart, ACheckedEvent.Start);
       ARangeFinish := Max(ARangeFinish, ACheckedEvent.Finish);
     end
     else
     begin
       Dec(AIndex);
       Break;
     end;
  end;
  ALastPosition := ARangeFinish;
end;

function TcxSchedulerEventConflictsInfo.IntersectEvents(
  ACheckedEvent: TcxSchedulerEvent): Boolean;
begin
  Result := IntersectTime(ACheckedEvent.Start, ACheckedEvent.Finish);
  if Result and not ExceptEventsWithoutResources then
    Result := not VarIsNull(ACheckedEvent.ResourceID);
  if Result and (FEvent <> nil) and not VarIsNull(ACheckedEvent.ResourceID) and not VarIsNull(ResourceID) then
    Result := ACheckedEvent.IsSharedWithResource(ResourceID) and not IsSameEvent(ACheckedEvent);
  if Result and (FEvent <> nil) then
    Result := not IsSameSeries(ACheckedEvent, FEvent);
  if Result and (FExcludedEvent <> nil) then
    Result := not IsSameSeries(ACheckedEvent, FExcludedEvent);
end;

function TcxSchedulerEventConflictsInfo.IntersectEvents(
  AEvent1, AEvent2: TcxSchedulerEvent): Boolean;
begin
  Result := IntersectTime(AEvent1.UTCStart, AEvent1.UTCFinish,
    AEvent2.UTCStart, AEvent2.UTCFinish);
end;

function TcxSchedulerEventConflictsInfo.IntersectTime(
  const AStart, AFinish: TDateTime): Boolean;
begin
  Result := IntersectTime(AStart, AFinish, Start, Finish);
end;

function TcxSchedulerEventConflictsInfo.IntersectTime(
  const AStart, AFinish, AStart1, AFinish1: TDateTime): Boolean;
begin
  Result := (AStart < AFinish1) and (AStart1 < AFinish);
end;

function TcxSchedulerEventConflictsInfo.IsSameEvent(
  ACheckedEvent: TcxSchedulerEvent): Boolean;
begin
  Result := (EventID <> nil) and EventID.SameEvent(ACheckedEvent);
end;

function TcxSchedulerEventConflictsInfo.IsSameSeries(
  AEvent1, AEvent2: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent1.RecordHandle = AEvent2.RecordHandle;
  if (AEvent1.RecordHandle = nil) or (AEvent2.RecordHandle = nil) then Exit;
  if not Result and (((AEvent1.Pattern = AEvent2) and (AEvent1.EventType = etOccurrence)) or
    ((AEvent2.Pattern = AEvent1) and (AEvent2.EventType = etOccurrence))) then
  begin
    Result := True;
    Exit;
  end;
  if not Result then
    Result := VarEquals(AEvent1.ID, AEvent2.ID);
  if not Result and (AEvent1.EventType = etPattern) then
    Result := VarEquals(AEvent2.ParentID, AEvent1.ID);
  if not Result and (AEvent2.EventType = etPattern) then
    Result := VarEquals(AEvent1.ParentID, AEvent2.ID);
  if Result and (AEvent1.EventType <> etPattern) then
    Result := AEvent1.RecurrenceIndex = AEvent2.RecurrenceIndex;
end;

function TcxSchedulerEventConflictsInfo.GetHasConflicts: Boolean;
begin
  Result := FConflictEvents.Count > 0;
end;

function TcxSchedulerEventConflictsInfo.GetHasFreeTime: Boolean;
begin
  Result := FTimeRanges.Count > 0;
end;

{ TcxSchedulerEventLinksController }

constructor TcxSchedulerEventLinksController.Create(AStorage: TcxCustomSchedulerStorage);
begin
  inherited Create;
  FStorage := AStorage;
end;

function TcxSchedulerEventLinksController.AllowLink(AEvent, ALink: TcxSchedulerEvent): Boolean;
begin
  Result := (AEvent <> nil) and (ALink <> nil) and
    (AEvent.Storage = ALink.Storage) and ((AEvent.Pattern = nil) or (AEvent.Pattern <> ALink.Pattern)) and
    AEvent.IsSharedWithResource(ALink.ResourceID) and
    not AEvent.HasAsParentGroup(ALink) and not ALink.HasAsParentGroup(AEvent) and
    DoAllowLink(AEvent, ALink);
end;

function TcxSchedulerEventLinksController.IsLinked(AEvent, ALink: TcxSchedulerEvent; ACheckGroupItems: Boolean = True): Boolean;
var
  I: Integer;
  AChild: TcxDoublyLinkedEventListData;
begin
  Result := False;
  if AEvent = ALink then
    Exit;
  for I := 0 to ALink.TaskLinks.Count - 1 do
    begin
      if (ALink.TaskLinks[I].Link = nil) then
        Continue;
      Result := (AEvent = ALink.TaskLinks[I].Link) or IsLinked(AEvent, ALink.TaskLinks[I].Link);
      if Result then
        Break;
    end;

  if not Result and ACheckGroupItems then
  begin
    AChild := ALink.GroupItems.First;
    while not Result and (AChild <> nil) do
    begin
      Result := IsLinked(AEvent, AChild.Event);
      AChild := AChild.Next;
    end;
  end;
  if not Result and (ALink.ParentGroup <> nil) then
    Result := IsLinked(AEvent, ALink.ParentGroup, False);
end;

function TcxSchedulerEventLinksController.DoAllowLink(AEvent, ALink: TcxSchedulerEvent): Boolean;

  function InternalIsEventLinked(ACurrentEvent, ACurrentLink: TcxSchedulerEvent): Boolean;
  begin
    Result := IsLinked(ACurrentEvent, ACurrentLink);
    if not Result and (ACurrentEvent.ParentGroup <> nil) then
      Result := InternalIsEventLinked(ACurrentEvent.ParentGroup, ACurrentLink);
  end;

var
  AChild: TcxDoublyLinkedEventListData;
begin
  Result := not InternalIsEventLinked(AEvent, ALink);
  if Result and AEvent.IsGroup then
  begin
    AChild := AEvent.GroupItems.First;
    while Result and (AChild <> nil) do
    begin
      Result := not InternalIsEventLinked(AChild.Event, ALink);
      AChild := AChild.Next;
    end;
  end;
end;

{ TcxSchedulerEditingEventInfo }

constructor TcxSchedulerEditingEventInfo.Create(AOwner: TcxSchedulerEditingEventInfoList;
  const AEditInfo: TcxSchedulerEventEditInfo);
begin
  inherited Create;
  Owner := AOwner;
  FFreeNotificator := TcxFreeNotificator.Create(nil);
  FFreeNotificator.OnFreeNotification := DoCloseEditor;
  FEditInfo := AEditInfo;
  SetEvent(Owner.CreateEvent(EditInfo));
  FOnAfterEditing := EditInfo.OnAfterEditingProc;
  if EditInfo.Event.Index >= 0 then
    EditInfo.Event.BeginEditing;
  FEventModified := False;
end;

destructor TcxSchedulerEditingEventInfo.Destroy;
begin
  FreeAndNil(FEvent);
  FreeAndNil(FFreeNotificator);
  CloseEditor;
  Owner.Extract(Self);
  inherited Destroy;
end;

procedure TcxSchedulerEditingEventInfo.CloseEditor;
var
  AEditor: TForm;
begin
  if Assigned(Editor) then
  begin
    AEditor := Editor;
    FEditor := nil;
    if IsDestroying then
      AEditor.Free
    else
      cxReleaseForm(AEditor);
  end;
end;

function TcxSchedulerEditingEventInfo.IsEventEditing(
  ASourceID, APatternID: Variant; ARecurrenceIndex: Integer; AEventType: TcxEventType): Boolean;
begin
  Result := False;
  if ASourceID = Null then
    Exit;
  if (ASourceID = SourceID) and (APatternID = PatternID) and
    (AEventType = EventType) and
    ((EventType = etPattern) or (ARecurrenceIndex = RecurrenceIndex)) then
  begin
    Editor.BringToFront;
    Result := True;
  end;
end;

function TcxSchedulerEditingEventInfo.IsPatternValid: Boolean;
begin
  Result := VarIsNull(PatternID) or
    (AnsiCompareStr(FRecurrenceInfoData, GetPatternRecurrenceInfoData) = 0);
end;

function TcxSchedulerEditingEventInfo.IsValid: Boolean;
begin
  Result := VarIsNull(SourceID) or not (IsEventDeleted or not IsPatternValid);
end;

procedure TcxSchedulerEditingEventInfo.ReplaceEvent(AEvent: TcxSchedulerControlEvent);
begin
  SetEvent(AEvent);
end;

function TcxSchedulerEditingEventInfo.ShowEditor(out AModified: Boolean): Boolean;
var
  AEditor: IcxSchedulerEventEditorForm;
begin
  AEditor := cxSchedulerEditorManager.CreateEditor(EditInfo.DialogsStyle, Event);
  try
    AEditor.Initialize(EditInfo, Self);
    FEditor := AEditor.Form;
    FEditor.BiDiMode := Application.BiDiMode;
    cxDialogsMetricsStore.InitDialog(FEditor);
    if not EditInfo.ShowModal then
    begin
      Editor.FreeNotification(FFreeNotificator);
      Result := False;
      Editor.Show;
    end
    else
    begin
      Result := Editor.ShowModal = mrOk;
      AModified := AEditor.Modified;
    end;
  finally
    AEditor := nil;
  end;
end;

procedure TcxSchedulerEditingEventInfo.DoAfterEditing;
var
  AEvent: TcxSchedulerControlEvent;
begin
  if Assigned(FOnAfterEditing) then
  begin
    if FEventModified then
      AEvent := Event
    else
      if Event.Source <> nil then
        AEvent := EditInfo.Event
      else
        AEvent := nil;
    OnAfterEditing(AEvent);
  end;
end;

function TcxSchedulerEditingEventInfo.GetEventID(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := cxGetEventID(AEvent);
end;

function TcxSchedulerEditingEventInfo.IsEventDeleted: Boolean;
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  Result := True;
  for I := 0 to Storage.EventCount - 1 do
  begin
    AEvent := Storage.Events[I];
    if VarEquals(SourceID, AEvent.ID) then
      Result := False;
    if EventType in [etOccurrence, etException] then
    begin
      if VarEquals(PatternID, AEvent.ID) and (AEvent.EventType <> etPattern) then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
end;

procedure TcxSchedulerEditingEventInfo.Reset;
begin
  PatternID := NULL;
  RecurrenceIndex := -1;
  FRecurrenceInfoData := '';
  SourceID := NULL;
end;

procedure TcxSchedulerEditingEventInfo.SetStorage(AValue: TcxCustomSchedulerStorage);
begin
  if AValue <> FStorage then
  begin
    FStorage := AValue;
    if FStorage <> nil then
      Owner := Storage.EditingEventInfoList;
  end;
end;

procedure TcxSchedulerEditingEventInfo.Update;
begin
  if Event <> nil then
  begin
    SourceID := GetEventID(Event.Source);
    EventType := Event.EventType;
    PatternID := GetEventID(Event.Pattern);
    if EventType in [etPattern, etNone] then
      RecurrenceIndex := -1
    else
      RecurrenceIndex := Event.RecurrenceIndex;
    FRecurrenceInfoData := GetPatternRecurrenceInfoData;
    UpdateStorage;
  end
  else
    Reset;
end;

procedure TcxSchedulerEditingEventInfo.UpdateStorage;
begin
  SetStorage(Event.Storage);
end;

procedure TcxSchedulerEditingEventInfo.DoCloseEditor(Sender: TComponent);
begin
  FEditor := nil;
  Free;
end;

function TcxSchedulerEditingEventInfo.GetIsDestroying: Boolean;
begin
  Result := FIsDestroying or Owner.IsDestroying;
end;

function TcxSchedulerEditingEventInfo.GetPatternRecurrenceInfoData: AnsiString;
begin
  if FEvent.Pattern <> nil then
    Result := cxRecurrenceInfoDataToString(FEvent.Pattern.RecurrenceInfo.GetData)
  else
    Result := '';
end;

procedure TcxSchedulerEditingEventInfo.SetEvent(AValue: TcxSchedulerControlEvent);
begin
  if FEvent <> AValue then
  begin
    if Event <> nil then
      FEvent.Free;
    FEvent := AValue;
    FEventModified := True;
  end;
  Update;
end;

procedure TcxSchedulerEditingEventInfo.SetOwner(AValue: TcxSchedulerEditingEventInfoList);
begin
  if (FOwner <> AValue) then
  begin
    if Assigned(FOwner) then
      FOwner.Extract(Self);
    FOwner := AValue;
    FOwner.Add(Self);
  end;
end;

procedure TcxSchedulerEditingEventInfo.Destroying;
begin
  FIsDestroying := True;
end;

function TcxSchedulerEditingEventInfo.CreateEvent(ASource: TcxSchedulerEvent;
  AOriginal: TcxSchedulerControlEvent;
  AForcePatternEditing: Boolean): TcxSchedulerControlEvent;
begin
  Result := Owner.CreateEvent(ASource, AOriginal, AForcePatternEditing);
end;

procedure TcxSchedulerEditingEventInfo.DeleteEvent(ADeleteSeries: Boolean);
begin
  Reset;
  if ADeleteSeries then
    Event.Pattern.Delete
  else
    Event.Delete;
  SetEvent(nil);
end;

procedure TcxSchedulerEditingEventInfo.BeforeDestruction;
begin
  inherited BeforeDestruction;
  if not Owner.IsDestroying then
    DoAfterEditing;
end;

{ TcxSchedulerEditingEventInfoList }

constructor TcxSchedulerEditingEventInfoList.Create(AStorage: TcxCustomSchedulerStorage);
begin
  inherited Create;
  FStorage := AStorage;
end;

destructor TcxSchedulerEditingEventInfoList.Destroy;
begin
  DoDestroy;
  inherited;
end;

procedure TcxSchedulerEditingEventInfoList.BeforeDestruction;
begin
  FIsDestroying := True;
end;

procedure TcxSchedulerEditingEventInfoList.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxSchedulerEditingEventInfoList.CancelUpdate;
begin
  Dec(FLockCount);
end;

procedure TcxSchedulerEditingEventInfoList.EndUpdate;
begin
  Dec(FLockCount);
  Update;
end;

procedure TcxSchedulerEditingEventInfoList.Update;
begin
  if IsLocked then
    Exit;
  BeginUpdate;
  try
    DoUpdate;
  finally
    CancelUpdate;
  end;
end;

procedure TcxSchedulerEditingEventInfoList.DeleteInfoByEvent(const AEvent: TcxSchedulerEvent);
var
  AInfo: TcxSchedulerEditingEventInfo;
begin
  if IsLocked then
    Exit;
  AInfo := FindInfo(AEvent.ID);
  while AInfo <> nil do
  begin
    AInfo.Free;
    AInfo := FindInfo(AEvent.ID);
  end;
end;

function TcxSchedulerEditingEventInfoList.FindInfo(const ASourceID: Variant): TcxSchedulerEditingEventInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].SourceID = ASourceID then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxSchedulerEditingEventInfoList.IsLocked: Boolean;
begin
  Result := FLockCount > 0;
end;

function TcxSchedulerEditingEventInfoList.IsEventEditing(ASourceID, APatternID: Variant;
  ARecurrenceIndex: Integer; AEventType: TcxEventType): Boolean;
var
  I: Integer;
begin
  Result := False;
  if ASourceID <> Null then
    for I := 0 to Count - 1 do
    begin
      Result := Items[I].IsEventEditing(ASourceID, APatternID, ARecurrenceIndex, AEventType);
      if Result then
        Break;
    end;
end;

function TcxSchedulerEditingEventInfoList.IsEventEditing(AEvent: TcxSchedulerEvent;
  ARecurrenceIndex: Integer; AEventType: TcxEventType): Boolean;
var
  ASourceID, APatternID: Variant;
begin
  ASourceID := cxGetEventID(AEvent);
  if (AEvent.EventType = etPattern) and (AEvent.Pattern = nil) then
    APatternID := ASourceID
  else
    APatternID := cxGetEventID(AEvent.Pattern);
  Result := IsEventEditing(ASourceID, APatternID, ARecurrenceIndex, AEventType);
end;

function TcxSchedulerEditingEventInfoList.IsEventEditing(AEvent: TcxSchedulerControlEvent; AForcePatternEditing: Boolean): Boolean;
var
  AEventType: TcxEventType;
  ASource: TcxSchedulerEvent;
begin
  if AForcePatternEditing and AEvent.IsRecurring and
    (AEvent.EventType <> etPattern) then
  begin
    AEventType := etPattern;
    ASource := AEvent.Pattern;
  end
  else
  begin
    AEventType := AEvent.EventType;
    ASource := AEvent.Source;
  end;
  Result := (AEvent.Source <> nil) and IsEventEditing(ASource, AEvent.RecurrenceIndex, AEventType);
end;

function TcxSchedulerEditingEventInfoList.ShowEditor(const AEventInfo: TcxSchedulerEventEditInfo;
  out AModified: Boolean): Boolean;
begin
  AEventInfo.ForcePatternEditing := AEventInfo.ForcePatternEditing and AEventInfo.Event.IsRecurring;
  Result := not IsEventEditing(AEventInfo.Event, AEventInfo.ForcePatternEditing);
  Result := Result and DoShowEditor(AEventInfo, AModified);
end;

procedure TcxSchedulerEditingEventInfoList.ShowEditor(const AEventInfo: TcxSchedulerEventEditInfo);
var
  AModified: Boolean;
begin
  ShowEditor(AEventInfo, AModified);
end;

procedure TcxSchedulerEditingEventInfoList.CloseExceptionEditor(AEvent: TcxSchedulerEvent);
var
  AInfo: TcxSchedulerEditingEventInfo;
begin
  AInfo := FindExceptionInfo(AEvent);
  if Assigned(AInfo) then
    AInfo.Free;
end;

procedure TcxSchedulerEditingEventInfoList.DoDestroy;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    Items[I].Free;
end;

function TcxSchedulerEditingEventInfoList.DoShowEditor(const AEditInfo: TcxSchedulerEventEditInfo; out AModified: Boolean): Boolean;
var
  AInfo: TcxSchedulerEditingEventInfo;
begin
  AInfo := GetInfoClass.Create(Self, AEditInfo);
  try
    Result := AInfo.ShowEditor(AModified);
  finally
    if AInfo.EditInfo.ShowModal then
      AInfo.Free;
  end;
end;

procedure TcxSchedulerEditingEventInfoList.DoUpdate;
var
  I: Integer;
  AInfo: TcxSchedulerEditingEventInfo;
begin
  for I := Count - 1 downto 0 do
  begin
    AInfo := Items[I];
    if not (AInfo.EditInfo.ShowModal or AInfo.IsValid) then
      AInfo.Free;
  end;
end;

function TcxSchedulerEditingEventInfoList.FindExceptionInfo(const AExceptionEvent: TcxSchedulerEvent): TcxSchedulerEditingEventInfo;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if (AExceptionEvent.ParentID = Items[I].PatternID) and
      (Items[I].EventType <> etPattern) and
      (AExceptionEvent.RecurrenceIndex = Items[I].RecurrenceIndex) then
    begin
      Result := Items[I];
      Break;
    end;
end;

function TcxSchedulerEditingEventInfoList.GetCount: Integer;
begin
  Result := inherited Count;
end;

function TcxSchedulerEditingEventInfoList.GetEventID(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := cxGetEventID(AEvent);
end;

function TcxSchedulerEditingEventInfoList.GetInfoClass: TcxSchedulerEditingEventInfoClass;
begin
  Result := TcxSchedulerEditingEventInfo;
end;

function TcxSchedulerEditingEventInfoList.GetItemCore(AIndex: Integer): TcxSchedulerEditingEventInfo;
begin
  Result := TcxSchedulerEditingEventInfo(inherited Items[AIndex]);
end;

function TcxSchedulerEditingEventInfoList.GetRootEvent(AEvent: TcxSchedulerEvent): TcxSchedulerEvent;
begin
  Result := AEvent;
end;

function TcxSchedulerEditingEventInfoList.GetRootStorage(AEvent: TcxSchedulerEvent): TcxCustomSchedulerStorage;
begin
  if AEvent = nil then
    Result := Storage
  else
    Result := GetRootEvent(AEvent).Storage;
end;

function TcxSchedulerEditingEventInfoList.CreateEvent(ASource, AOriginal: TcxSchedulerEvent;
  AForcePatternEditing: Boolean): TcxSchedulerControlEvent;
var
  AEvent: TcxSchedulerEvent;
begin
  if AForcePatternEditing and (ASource.Pattern <> nil) then
  begin
    AEvent := ASource.Pattern;
    ASource := AEvent;
  end
  else
    if ASource is TcxSchedulerControlEvent then
      AEvent := TcxSchedulerControlEvent(ASource).Source
    else
      AEvent := ASource;

  AEvent := GetRootEvent(AEvent);

  Result := TcxSchedulerControlEvent.Create(GetRootStorage(AEvent));
  Result.FTimeBias := AOriginal.TimeBias;
  Result.Assign(ASource);
  Result.FLockedResource := Null;
  Result.SetSource(AEvent);
  Result.FLink := AOriginal.FLink;
  Result.FID := GetEventID(AEvent);
  if AForcePatternEditing then
    Result.EventType := etPattern;
  if Result.EventType = etPattern then
    Result.FPattern := AEvent
  else
    Result.FPattern := GetRootEvent(ASource.Pattern);
end;

function TcxSchedulerEditingEventInfoList.CreateEvent(const AEventInfo: TcxSchedulerEventEditInfo): TcxSchedulerControlEvent;
begin
  Result := CreateEvent(AEventInfo.Event, AEventInfo.Event, AEventInfo.ForcePatternEditing);
end;

function TcxSchedulerEditingEventInfoList.GetActiveEditingEvent: TcxSchedulerControlEvent;
var
  I: Integer;
  AInfo: TcxSchedulerEditingEventInfo;
begin
  Result := nil;
  for I := 0 to Storage.EditingEventInfoList.Count - 1 do
  begin
    AInfo := Storage.EditingEventInfoList[I];
    if AInfo.Editor.Active then
      Result := AInfo.Event;
    if Result <> nil then
      Break;
  end;
end;

function TcxSchedulerEditingEventInfoList.GetItem(AIndex: Integer): TcxSchedulerEditingEventInfo;
begin
  Result := GetItemCore(AIndex);
end;

{ TcxCustomSchedulerStorage }

constructor TcxCustomSchedulerStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIsLoading := True;
  CreateSubClasses;
  FIsLoading := False;
end;

destructor TcxCustomSchedulerStorage.Destroy;
begin
  StopUpdateRemindersTimer;
  SendNotification(nil, True);
  DestroySubClasses;
  Holidays := nil;
  inherited Destroy;
end;

procedure TcxCustomSchedulerStorage.Assign(
  Source: TPersistent);
begin
  if Source is TcxCustomSchedulerStorage then
  begin
    Resources := TcxCustomSchedulerStorage(Source).Resources;
    CustomFields.Assign(TcxCustomSchedulerStorage(Source).CustomFields);
    InternalFields.Assign(TcxCustomSchedulerStorage(Source).InternalFields);
  end;
end;

procedure TcxCustomSchedulerStorage.AddListener(
  AListener: IcxSchedulerStorageListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

procedure TcxCustomSchedulerStorage.BeginUpdate;
begin
  BeginUpdateDataController;
end;

procedure TcxCustomSchedulerStorage.Clear;
begin
  BeginUpdate;
  try
    while EventCount > 0 do
      Events[0].Delete;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomSchedulerStorage.DoneRestore;
begin
  if ResourceCount > 0 then
    Resources.DoneRestore
end;

procedure TcxCustomSchedulerStorage.CalculateEventActualTimeRanges;
var
  I: Integer;
begin
  if not ActualTimeRangeAvailable then Exit;
  BeginUpdate;
  try
    for I := 0 to EventCount - 1 do
      Events[I].CalculateActualTimeRange;
  finally
    EndUpdate;
  end;
end;

function TcxCustomSchedulerStorage.CheckRequiredFields: Boolean;
begin
  Result := True;
end;

function TcxCustomSchedulerStorage.CreateEvent: TcxSchedulerEvent;
begin
  Result := GetEventClass.Create(Self);
  Result.FID := GetEventID(Result);
  FNewEvents.Add(Result);
end;

function TcxCustomSchedulerStorage.CreateOccurrence(APattern: TcxSchedulerEvent;
  const ADate: TDateTime; AType: TcxEventType): TcxSchedulerEvent;
var
  ACalculator: TcxSchedulerOccurrenceCalculator;
  AOccurrence: TcxSchedulerEvent;
begin
  Result := nil;
  if not (AType in [etCustom, etException]) or (APattern.EventType <> etPattern) then Exit;
  ACalculator := TcxSchedulerOccurrenceCalculator.Create(APattern, APattern.Start, ADate + 7);
  try
    while ACalculator.GetNextOccurrence do
    begin
      if APattern.GetOccurrenceByIndex(ACalculator.Index, AOccurrence) then
        Continue
      else
        if dxDateOf(ACalculator.OccurrenceStart) > dxDateOf(ADate) then
          Break
        else
          if dxDateOf(ACalculator.OccurrenceStart) = dxDateOf(ADate) then
          begin
            Result := CreateEvent;
            Result.Assign(APattern);
            Result.Start := ACalculator.OccurrenceStart;
            Result.Finish := ACalculator.OccurrenceFinish;
            Result.RecurrenceIndex := ACalculator.Index;
            Result.EventType := AType;
            Result.ParentID := APattern.ID;
            Result.FPattern := APattern;
            Result.RecurrenceInfo.OriginalStart := ACalculator.OccurrenceStart;
            Break;
          end;
    end;
  finally
    ACalculator.Free;
  end;
end;

procedure TcxCustomSchedulerStorage.EndUpdate;
begin
  FInternalUpdate := True;
  try
    try
      if LockCount = 1 then
      begin
        PostEvents;
        DeleteEvents(FDeletedEvents);
      end;
    finally
      EndUpdateDataController;
    end;
    DoUpdateStructure;
    if FIsChanged then
      Changed;
  finally
    FInternalUpdate := False;
  end;
end;

function TcxCustomSchedulerStorage.FindAvailableAllDay(
  var AStart, AFinish: TDateTime; AResourceID: Variant;
  AExceptEventsWithoutResources: Boolean; ADuration: TDateTime = 0): Boolean;
var
  I, J: Integer;
  AList: TcxSchedulerFilteredEventList;
  AStartDay: TDateTime;
begin
  Result := False;
  AStartDay := EncodeTime(0, 0, 1, 0);
  if ADuration = 0 then
    ADuration := Max(1, Trunc(AFinish) - Trunc(AStart));
  AList := TcxSchedulerFilteredEventList.Create;
  try
    for I := Trunc(AStart) to Trunc(AStart) + cxMaxCheckedDuration do
    begin
      Result := not GetEvents(AList, I + AStartDay, I + ADuration - AStartDay, AResourceID);
      if not Result then
      begin
        Result := True;
        for J := 0 to AList.Count - 1 do
          Result := Result and AList[J].IsFreeState;
      end;
      if Result then
      begin
        AStart := I;
        AFinish := AStart +  ADuration;
        Break;
      end;
    end;
  finally
    AList.Free;
  end;
end;

function TcxCustomSchedulerStorage.FindAvailableTime(
  var AStart, AFinish: TDateTime; AllDay: Boolean;
  AResourceID: Variant; AExceptEventsWithoutResources: Boolean; ADuration: TDateTime = 0;
  AExcludedEvent: TcxSchedulerEvent = nil): Boolean;
var
  I: Integer;
  AInfo: TcxSchedulerEventConflictsInfo;
begin
  if ADuration = 0 then
    ADuration := AFinish - AStart;
  if AllDay then
    Result := FindAvailableAllDay(AStart, AFinish, AResourceID, AExceptEventsWithoutResources, ADuration)
  else
  begin
    AInfo := TcxSchedulerEventConflictsInfo.Create(Self, AExceptEventsWithoutResources, Trunc(AStart),
          Trunc(AFinish) + cxMaxCheckedDuration, AResourceID, AExcludedEvent);
    try
      Result := False;
      for I := 0 to AInfo.TimeRanges.Count - 1 do
        if (AInfo.TimeRanges[I].Duration >= ADuration) and
          (DateTimeHelper.RoundTime(AInfo.TimeRanges[I].Finish - ADuration) >= AStart) then
        begin
          if AInfo.TimeRanges[I].Start > AStart then
            AStart := AInfo.TimeRanges[I].Start;
          AFinish := AStart + ADuration;
          Result := True;
          Break;
        end;
    finally
      AInfo.Free;
    end;
  end;
end;

function TcxCustomSchedulerStorage.FindAvailableTime(AEvent: TcxSchedulerEvent;
  AExceptEventsWithoutResources: Boolean; var AStart, AFinish: TDateTime): Boolean;
var
  I: Integer;
  S: TDateTime;
  AInfo: TcxSchedulerEventConflictsInfo;
begin
  Result := AEvent.Conflicts(AExceptEventsWithoutResources);
  S := AEvent.Start;
  AEvent.GetStartFinishTime(AStart, AFinish);
  if Result then
  begin
    if AEvent.EventType <> etPattern then
    begin
      AStart := AStart + AEvent.TimeBias - TimeBias;
      Result := FindAvailableTime(AStart, AFinish, AEvent.AllDayEvent,
        AEvent.ResourceID, AExceptEventsWithoutResources, AEvent.Duration, AEvent);
      if Result then
      begin
        AStart := AStart + TimeBias - AEvent.TimeBias;
        AFinish := AFinish + TimeBias - AEvent.TimeBias;
      end;
    end
    else
    begin
      AEvent.BeginEditing;
      try
        AInfo := cxSchedulerEventConflictsInfoClass.Create(Self, AExceptEventsWithoutResources,
          Trunc(AEvent.Start), Trunc(AEvent.Finish) + 1, AEvent.ResourceID);
        try
          Result := False;
          for I := 1 to 24 * 6 do
          begin
            AEvent.MoveTo(AStart + I * 10 * MinuteToTime);
            if not AEvent.Conflicts(AExceptEventsWithoutResources) then
            begin
              AEvent.GetStartFinishTime(AStart, AFinish);
              Result := True;
              Break;
            end;
          end;
        finally
          AInfo.Free;
        end;
      finally
        AEvent.MoveTo(S);
        AEvent.Cancel;
      end;
    end;
  end
end;

procedure TcxCustomSchedulerStorage.FullRefresh;
begin
  if IsUpdatingMode then
  begin
    RefreshNeeded := True;
    Exit;
  end;
  FIsLoading := True;
  try
    DoRefresh;
  finally
    FIsLoading := False;
    SendNotification(nil);
  end;
end;

procedure TcxCustomSchedulerStorage.GenerateHolidayEvents(const AResourceID: Variant);
begin
  GenerateHolidayEventsBySchedulerHolidays(AResourceID, Holidays);
end;

function TcxCustomSchedulerStorage.GetEventByID(const AID: Variant): TcxSchedulerEvent;
var
  I: Integer;
begin
  Result := nil;
  if VarIsNull(AID) then Exit;
  for I := 0 to EventCount - 1 do
    if VarEquals(AID, Events[I].ID) then
    begin
      Result := Events[I];
      Break;
    end;
end;

function TcxCustomSchedulerStorage.GetEvents(
  AList: TcxSchedulerFilteredEventList;
  const AStart, AFinish: TDateTime): Boolean;
begin
  Result := GetEvents(AList, AStart, AFinish, Null);
end;

procedure TcxCustomSchedulerStorage.InternalGetEvents(AList: TcxSchedulerFilteredEventList;
  const AStart, AFinish: TDateTime; const AResourceID: Variant);
var
  I, J, C: Integer;
  AIsNull: Boolean;
  AEvent: TcxSchedulerEvent;
  AStartDate, AFinishDate: Integer;
begin
  AStartDate := Trunc(AStart);
  AFinishDate := Trunc(AFinish) + 1;
  AIsNull := VarIsNull(AResourceID);
  AList.PostponedInitialization := True;
  try
    for I := 0 to SortedList.Count - 1 do
    begin
      AEvent := TcxSchedulerEvent(SortedList.List[I]);
      if not AList.UseTimeRange or AEvent.CheckTimeRange(AStartDate, AFinishDate) then
      begin
        if not VarIsArray(AResourceID) then
          AList.CheckEvent(AEvent, AResourceID, AIsNull)
        else
        begin
          C := AList.Count;
          for J := VarArrayLowBound(AResourceID, 1) to VarArrayHighBound(AResourceID, 1) do
          begin
            AList.CheckEvent(AEvent, AResourceID[J], False);
            if AList.Count <> C then Break;
          end;
        end;
      end
      else
        if AList.UseTimeRange and (AEvent.FInfo.DayStart > AFinishDate) then
          Break;
    end;

    if AList.Count = 0 then
      Exit;
     TdxMultithreadedListHelper.IterateItems(AList.List,
      procedure(AEvent: Pointer; AIndex: Integer)
      begin
        AList.PostInitializeEvent(TcxSchedulerControlEvent(AEvent), True);
      end
    );
  finally
    AList.PostponedInitialization := False;
  end;
end;

function TcxCustomSchedulerStorage.GetEvents(
  AList: TcxSchedulerFilteredEventList; const AStart, AFinish: TDateTime;
  const AResourceID: Variant): Boolean;
var
  ASmartUpdate: Boolean;
  APrevStart, APrevFinish: TDateTime;
begin
  APrevStart := AList.Start;
  APrevFinish := AList.Finish;
  ASmartUpdate := AList.CanSmartUpdate(Self, AStart, AFinish, AResourceID);
  Result := False;
  if not Active then
  begin
    AList.Clear;
    Exit;
  end;
  if not ASmartUpdate then
    InternalGetEvents(AList, AStart, AFinish, AResourceID)
  else
  begin
    if AStart < APrevStart then
    begin
      AList.SetCheckRange(Trunc(APrevStart) - 1, Trunc(APrevStart) + 1);
      InternalGetEvents(AList, AStart, APrevStart, AResourceID);
    end;
    if AFinish > APrevFinish then
    begin
      AList.SetCheckRange(Trunc(APrevFinish) - 1, Trunc(APrevFinish) + 1);
      InternalGetEvents(AList, APrevFinish, AFinish, AResourceID);
    end;
  end;
  AList.Changed;
  Result := AList.Count > 0;
end;

function TcxCustomSchedulerStorage.GetFieldByName(
  const AName: string): TcxCustomSchedulerStorageField;
begin
  Result := CustomFields.FindFieldByName(AName);
end;

function TcxCustomSchedulerStorage.GetHolidayNamesByDate(ADate: TDate; var ANames: string;
  AOnlyVisible: Boolean = True): Boolean;
begin
  Result := (Holidays <> nil) and
    Holidays.GetHolidayNamesByDate(ADate, ANames, AOnlyVisible);
end;

function TcxCustomSchedulerStorage.GetReminderEvents(
  ADateTime: TDateTime; AList: TcxSchedulerFilteredEventList): Boolean;
var
  I: Integer;
begin
  if FIsLoading or not Active then
  begin
    Result := False;
    Exit;
  end;
  AList.Init(NullDate, ADateTime + 14, Self, True); //calculate for two weeks (= max AdvanceTime)
  AList.FReminderEventsOnly := True;
  AList.FNow := Reminders.GetNow;
  for I := 0 to EventCount - 1 do
    AList.CheckEvent(Events[I], Null, True);
  AList.Changed;
  Result := AList.Count > 0;
end;

procedure TcxCustomSchedulerStorage.PopulateHolidayDates(AList: TcxSchedulerDateList;
  AStart, AFinish: TDate; AOnlyVisible: Boolean = True; AClearList: Boolean = True);
begin
  if AList <> nil then
  begin
    if AClearList then
      AList.Clear;
    if Holidays <> nil then
      Holidays.PopulateHolidayDates(AList, AStart, AFinish, AOnlyVisible, AClearList);
  end;
end;

procedure TcxCustomSchedulerStorage.PostEvents;
begin
  if not IsActive or (FNewEvents.Count = 0) then Exit;
  BeginUpdate;
  try
    while FNewEvents.Count <> 0 do FNewEvents.Last.Post;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomSchedulerStorage.InitRestore;
begin
  if ResourceCount > 0 then
    Resources.InitRestore;
end;

function TcxCustomSchedulerStorage.IsActive: Boolean;
begin
  Result := True;
end;

function TcxCustomSchedulerStorage.IsCaptionAvailable: Boolean;
begin
  Result := FCaptionField.IsActive;
end;

function TcxCustomSchedulerStorage.IsEventEditing(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := EditingEventInfoList.IsEventEditing(AEvent, False);
end;

function TcxCustomSchedulerStorage.IsLabelColorAvailable: Boolean;
begin
  Result := FLabelColorField.IsActive;
end;

function TcxCustomSchedulerStorage.IsLocationAvailable: Boolean;
begin
  Result := FLocationField.IsActive;
end;

function TcxCustomSchedulerStorage.IsMessageAvailable: Boolean;
begin
  Result := FMessageField.IsActive;
end;

function TcxCustomSchedulerStorage.IsRecurrenceAvailable: Boolean;
begin
  Result := FRecurrenceIndexField.IsActive and FRecurrenceInfoField.IsActive;
end;

function TcxCustomSchedulerStorage.IsReminderAvailable: Boolean;
begin
  Result := FReminderMinutesBeforeStartField.IsActive;
end;

function TcxCustomSchedulerStorage.IsReminderByResourceAvailable: Boolean;
begin
  Result := IsReminderAvailable and FReminderResourcesData.IsActive;
end;

function TcxCustomSchedulerStorage.IsStateAvailable: Boolean;
begin
  Result := FStateField.IsActive;
end;

procedure TcxCustomSchedulerStorage.BeginUpdateDataController;
begin
  Inc(LockCount);
  DataController.BeginUpdate;
end;

procedure TcxCustomSchedulerStorage.EndUpdateDataController;
begin
  Dec(LockCount);
  DataController.EndUpdate;
end;

procedure TcxCustomSchedulerStorage.BeginStructureUpdating;
begin
  Inc(FStructureUpdatingCount);
end;

procedure TcxCustomSchedulerStorage.EndStructureUpdating;
begin
  Dec(FStructureUpdatingCount);
end;

function TcxCustomSchedulerStorage.IsStructureUpdating: Boolean;
begin
  Result := FStructureUpdatingCount > 0;
end;

function TcxCustomSchedulerStorage.GetEventID(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := Null;
end;

function TcxCustomSchedulerStorage.GetFieldValueTypeClass(
  AField: TcxCustomSchedulerStorageField): TcxValueTypeClass;
begin
  if AField = nil then
    Result := TcxIntegerValueType
  else
    Result := DataController.GetItemValueTypeClass(AField.Index);
end;

procedure TcxCustomSchedulerStorage.RemoveListener(
  AListener: IcxSchedulerStorageListener);
begin
  FListeners.Remove(AListener);
end;

function TcxCustomSchedulerStorage.IsEventSharingAllowed: Boolean;
begin
  Result := True;
end;

// IcxSchedulerHolidaysListener
procedure TcxCustomSchedulerStorage.HolidaysChanged(Sender: TObject);
begin
  LayoutChanged;
end;

procedure TcxCustomSchedulerStorage.HolidaysRemoved(Sender: TObject);
begin
  FHolidays.RemoveListener(Self);
  FHolidays := nil;
  LayoutChanged;
end;

function TcxCustomSchedulerStorage.ActualTimeRangeAvailable: Boolean;
begin
  Result := UseActualTimeRange and
    FActualStartField.IsActive and FActualFinishField.IsActive;
end;

function TcxCustomSchedulerStorage.AddEventRecord(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := False;
end;

procedure TcxCustomSchedulerStorage.AddInternalField(
  var AField: TcxCustomSchedulerStorageField; AValueType: TcxValueTypeClass;
  AIsUnique: Boolean = True);
begin
  AField := TcxCustomSchedulerStorageField(InternalFields.Add);
  AField.ValueTypeClass := AValueType;
  AField.FIsUnique := AIsUnique;
end;

function TcxCustomSchedulerStorage.AllocateRecord: Pointer;
begin
  Result := TcxDataStorageHelper.AllocateRecord(DataController.DataStorage);
end;

procedure TcxCustomSchedulerStorage.AssignIndexes;
var
  I: Integer;
begin
  for I := 0 to FEventsList.Count - 1 do
    TcxSchedulerEvent(FEventsList.List[I]).FIndex := I;
end;

procedure TcxCustomSchedulerStorage.AfterPostEditingData(AEvent: TcxSchedulerEvent);
begin
end;

procedure TcxCustomSchedulerStorage.BeforePostEditingData(AEvent: TcxSchedulerEvent);
begin
end;

procedure TcxCustomSchedulerStorage.BeforeDeleteEvent(AEvent: TcxSchedulerEvent);
begin
  if IsUpdatingMode and not IsStructureUpdating then
    AEvent.ParentGroup := nil;
end;

procedure TcxCustomSchedulerStorage.CancelEvent(AEvent: TcxSchedulerEvent);
begin
  Dec(AEvent.FEditCount);
  if (AEvent.FEditCount = 0) and AEvent.IsNewEvent then
    AEvent.Free;
  Changed;
end;

function TcxCustomSchedulerStorage.CanGetValueFromPattern(
  AIndex: Integer): Boolean;
begin
  Result := AIndex in [
    FCaptionField.Index,
    FLabelColorField.Index,
    FLocationField.Index,
    FMessageField.Index,
    FOptionsField.Index,
    FReminderMinutesBeforeStartField.Index,
    FStateField.Index,
    FReminderDateField.Index,
    FReminderResourcesData.Index];
  Result := Result or (AIndex >= InternalFieldCount);
end;

procedure TcxCustomSchedulerStorage.CalculateTimeBias;
var
  AOldTimeBias: Double;
begin
  AOldTimeBias := FTimeBias;
  if StoreUsingGlobalTime then
    FTimeBias := DateTimeHelper.CurrentTimeZoneBias
  else
    FTimeBias := 0;
  if AOldTimeBias <> FTimeBias then
  begin
    Changed;
    FullRefresh;
  end;
end;

procedure TcxCustomSchedulerStorage.Changed;
begin
  FIsChanged := IsLocked;
  if not IsLocked then
  begin
    SendNotification(nil);
    if CanUpdateReminders then
      Reminders.Refresh;
  end;
end;

procedure TcxCustomSchedulerStorage.CheckEditingEventInfo;
begin
  EditingEventInfoList.Update;
end;

procedure TcxCustomSchedulerStorage.CreateDefaultFields(const AVersion: Double);
begin
  InternalFields.BeginUpdate;
  try
    if AVersion = -1 then
    begin
      AddInternalField(FCaptionField, TcxStringValueType);
      AddInternalField(FEventTypeField, TcxIntegerValueType);
      AddInternalField(FFinishField, TcxDateTimeValueType);
      AddInternalField(FLabelColorField, TcxIntegerValueType);
      AddInternalField(FLocationField, TcxStringValueType);
      AddInternalField(FMessageField, TcxStringValueType);
      AddInternalField(FOptionsField, TcxIntegerValueType);
      AddInternalField(FParentIDField, TcxIntegerValueType);
      AddInternalField(FRecurrenceIndexField, TcxIntegerValueType, False);
      AddInternalField(FRecurrenceInfoField, TcxVariantValueType, False);
      AddInternalField(FReminderDateField, TcxDateTimeValueType);
      AddInternalField(FReminderMinutesBeforeStartField, TcxIntegerValueType);
      AddInternalField(FResourceIDField, TcxVariantValueType, False);
      AddInternalField(FStartField, TcxDateTimeValueType);
      AddInternalField(FStateField, TcxIntegerValueType);
      AddInternalField(FActualFinishField, TcxIntegerValueType, False);
      AddInternalField(FActualStartField, TcxIntegerValueType, False);
    end;
    if (AVersion < 3) or (AVersion = -1) then
    begin
      AddInternalField(FTaskCompleteField, TcxIntegerValueType, False);
      AddInternalField(FTaskIndexField, TcxIntegerValueType, False);
      AddInternalField(FTaskLinksField, TcxVariantValueType, False);
      AddInternalField(FTaskStatusField, TcxIntegerValueType, False);
      AddInternalField(FReminderResourcesData, TcxVariantValueType, False);
    end;
    if (AVersion < 2011.2) or (AVersion = -1) then
      AddInternalField(FGroupIDField, TcxVariantValueType, False);
  finally
    InternalFields.EndUpdate;
  end;
end;

function TcxCustomSchedulerStorage.CreateEventInternal: TcxSchedulerEvent;
begin
  Result := GetEventClass.Create(Self, True);
  FEventsList.List.Add(Result);
end;

function TcxCustomSchedulerStorage.CreateFields: TcxCustomSchedulerStorageFields;
begin
  Result := TcxSchedulerStorageFields.Create(TcxSchedulerStorageField);
end;

procedure TcxCustomSchedulerStorage.CreateHolidayEvent(AHoliday: TcxSchedulerHolidaysLocationHoliday;
  AResourceID: Variant);
begin
  with createEvent do
  begin
    Caption := AHoliday.DisplayText;
    Start := AHoliday.Date;
    Finish := Start + 1;
    AllDayEvent := True;
    State := tlsFree;
    ResourceID := AResourceID;
    if AHoliday.IsRecurring then
    begin
      EventType := etPattern;
      RecurrenceInfo.Assign(AHoliday.RecurrenceInfo);
    end;
    Post;
  end;
end;

function TcxCustomSchedulerStorage.CreateReminders: TcxSchedulerReminders;
begin
  Result := TcxSchedulerReminders.Create(Self);
end;

function TcxCustomSchedulerStorage.CreateResources: TcxSchedulerStorageResources;
begin
  Result := TcxSchedulerStorageResources.Create(Self);
end;

procedure TcxCustomSchedulerStorage.CreateSubClasses;
begin
  FLinksController := GetLinksControllerClass.Create(Self);
  FDeletedEvents := TcxObjectList.Create;
  FResources := CreateResources;
  FInternalFields := CreateFields;
  FInternalFields.FOwner := Self;
  FCustomFields := CreateFields;
  FCustomFields.FOwner := Self;
  FFields := TdxFastList.Create;
  FNewEvents := TcxSchedulerEventList.Create;
  FEventsList := TcxSchedulerEventList.Create;
  FSortedList := TdxFastList.Create;
  FListeners := TInterfaceList.Create();
  FDataController := GetDataControllerClass.Create(Self) as TcxSchedulerStorageDataController;
  FEditingEventInfoList := GetEditingEventInfoListClass.Create(Self);
  FReminders := CreateReminders;
  CreateDefaultFields(-1);
end;

procedure TcxCustomSchedulerStorage.DeleteEvents(AEvents: TcxObjectList);
var
  I: Integer;
begin
  for I := 0 to AEvents.Count - 1 do
    EditingEventInfoList.DeleteInfoByEvent(TcxSchedulerEvent(AEvents[I]));
  AEvents.Clear;
end;

procedure TcxCustomSchedulerStorage.DestroyDefaultFields(AVersion: Double);

  procedure DeleteField(var AField: TcxCustomSchedulerStorageField);
  begin
    try
      FInternalFields.Delete(AField.Index);
    finally
      AField := nil;
    end;
  end;

begin
  if AVersion < 3 then
  begin
    DeleteField(FTaskCompleteField);
    DeleteField(FTaskIndexField);
    DeleteField(FTaskLinksField);
    DeleteField(FTaskStatusField);
    DeleteField(FReminderResourcesData);
  end;
  if AVersion < 2011.2 then
    DeleteField(FGroupIDField);
end;

procedure TcxCustomSchedulerStorage.DestroySubClasses;
begin
  FreeAndNil(FEditingEventInfoList);
  FReminders.Free;
  try
    FEventsList.DestroyItems;
    FNewEvents.DestroyItems;
    FDeletedEvents.Free;
    FCustomFields.Clear;
    FInternalFields.Clear;
  finally
    FDataController.Free;
    FreeAndNil(FSortedList);
    FNewEvents.Free;
    FEventsList.Free;
    FCustomFields.Free;
    FInternalFields.Free;
    FFields.Free;
    FResources.Free;
    FreeAndNil(FEditor);
    FListeners.Free;
    FreeAndNil(FLinksController);
  end;
end;

procedure TcxCustomSchedulerStorage.DoDeleteEvent(
  AEvent: TcxSchedulerEvent);
var
  I: Integer;
  AChildEvent: TcxSchedulerEvent;
  AEventsChain: TcxSchedulerEventList;
begin
  if IsUpdatingMode or DoEventDeleted(AEvent) then
    Exit;
  BeginUpdate;
  try
    AEvent.GroupItems.DeleteEvents;
    try
      AEventsChain := AEvent.GetRecurrenceChain;
      try
        for I := 0 to AEventsChain.Count - 1 do
        begin
          AChildEvent := AEventsChain[I];
          InternalDelete(AChildEvent);
          FDeletedEvents.Add(AChildEvent);
        end;
        FDeletedEvents.Add(AEvent);
        for I := 0 to AEventsChain.Count - 1 do
          DoDestroyEvent(AEventsChain[I]);
        DoDestroyEvent(AEvent);
      finally
        AEventsChain.Free;
      end;
    finally
      Changed;
      AssignIndexes;
    end;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomSchedulerStorage.DoDestroyEvent(
  AEvent: TcxSchedulerEvent);
begin
  SendNotification(AEvent, True, False);
  InternalRemove(AEvent);
end;

function TcxCustomSchedulerStorage.DoEventDeleted(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := False;
  if Assigned(FOnEventDeleted) then
    FOnEventDeleted(Self, AEvent, Result);
end;

function TcxCustomSchedulerStorage.DoEventInserted(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := False;
  if Assigned(FOnEventInserted) then
    FOnEventInserted(Self, AEvent, Result);
end;

function TcxCustomSchedulerStorage.DoEventIntersect(AEvent1,
  AEvent2: TcxSchedulerEvent): Boolean;
begin
  Result := False;
  if Assigned(FOnEventIntersect) then
    FOnEventIntersect(Self, AEvent1, AEvent2, Result);
end;

function TcxCustomSchedulerStorage.DoEventModified(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := False;
  with AEvent do
  begin
    if IsModified and Assigned(FOnEventModified) then
      FOnEventModified(Self, AEvent, Result);
  end;
end;

function TcxCustomSchedulerStorage.DoFilterEvent(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := True;
  if Assigned(FOnFilterEvent) then
    FOnFilterEvent(Self, AEvent, Result);
end;

procedure TcxCustomSchedulerStorage.DoRefresh;
begin
  if LockCount > 0 then Exit;
  Reminders.StopTimers;
  SynchronizeEventsWithRecords;
  DoUpdateStructure;
end;

procedure TcxCustomSchedulerStorage.DoUpdateStructure(ANeedUpdateInfo: Boolean = True);
begin
  UpdateStructure;
  if not IsDestroying and ANeedUpdateInfo then
    CheckEditingEventInfo;
end;

procedure TcxCustomSchedulerStorage.FreeRecord(var ARecordHandle: Pointer);
begin
  if ARecordHandle <> nil then
    TcxDataStorageHelper.FreeRecord(DataController.DataStorage, ARecordHandle);
end;

procedure TcxCustomSchedulerStorage.GenerateHolidayEventsBySchedulerHolidays(
  const AResourceID: Variant; AHolidays: TcxSchedulerHolidays);
var
  I: Integer;
begin
  if (AHolidays = nil) or
      (AHolidays.Count = 0) then
    Exit;
  BeginUpdate;
  try
    for I := 0 to AHolidays.Count - 1 do
      if AHolidays[I].IsVisible then
        CreateHolidayEvent(AHolidays[I], AResourceID);
  finally
    EndUpdate;
  end;
end;

function TcxCustomSchedulerStorage.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxSchedulerStorageDataController;
end;

function TcxCustomSchedulerStorage.GetEditingEventInfoListClass: TcxSchedulerEditingEventInfoListClass;
begin
  Result := TcxSchedulerEditingEventInfoList;
end;

function TcxCustomSchedulerStorage.GetEventClass: TcxSchedulerEventClass;
begin
  Result := TcxSchedulerEvent;
end;

function TcxCustomSchedulerStorage.GetLinksControllerClass: TcxSchedulerEventLinksControllerClass;
begin
  Result := TcxSchedulerEventLinksController;
end;

function TcxCustomSchedulerStorage.GetParentForm: TForm;
var
  AOwner: TComponent;
begin
  AOwner := Owner;
  while not (AOwner is TForm) and (AOwner <> nil) do
    AOwner := AOwner.Owner;
  Result := TForm(AOwner);
end;

function TcxCustomSchedulerStorage.GetStoreUsingGlobalTime: Boolean;
begin
  Result := FStoreUsingGlobalTime;
end;

function TcxCustomSchedulerStorage.GetEventActualValue(
  AEvent: TcxSchedulerEvent; AIndex: Integer): Variant;
begin
  if AEvent.IsEditing then
    Result := GetEventValue(AEvent.FEditingRecordHandle, AIndex)
  else
    Result := GetEventValue(AEvent.FRecordHandle, AIndex);
end;

function TcxCustomSchedulerStorage.GetEventValue(
  const AHandle: Pointer; AIndex: Integer): Variant;
begin
  if AHandle = nil then
    Result := Null
  else
    Result := TcxDataStorageHelper.GetValue(Fields[AIndex].ValueDef, AHandle);
end;

function TcxCustomSchedulerStorage.GetIsBoundMode: Boolean;
begin
  Result := False;
end;

function TcxCustomSchedulerStorage.HasEventIntersect(AEvent1,
  AEvent2: TcxSchedulerEvent): Boolean;
begin
  Result := not DoEventIntersect(AEvent1, AEvent2);
end;

procedure TcxCustomSchedulerStorage.InternalDelete(AEvent: TcxSchedulerEvent);
begin
  BeforeDeleteEvent(AEvent);
end;

function TcxCustomSchedulerStorage.IsEventDeleting(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := AEvent.FIsDeletion or (FDeletedEvents.IndexOf(AEvent) <> -1);
end;

procedure TcxCustomSchedulerStorage.ItemAdded(
  AItem: TcxCustomSchedulerStorageField);
begin
  if FFields.IndexOf(AItem) = -1 then
  try
    Active := False;
    AItem.FIndex := FFields.Add(AItem);
    DataController.AddItem(AItem);
    if AItem.ValueDef = nil then
    begin
      if AItem.ValueTypeClass = nil then
        AItem.SetValueTypeClass(TcxStringValueType)
      else
        AItem.SetValueTypeClass(AItem.ValueTypeClass)
    end
    else
      DataController.InsertValueDef(AItem);
  finally
    if not IsLoading and not FIsLoading then
      DataController.UpdateItemIndexes;
    Active := True;
  end;
end;

procedure TcxCustomSchedulerStorage.ItemRemoved(
  AItem: TcxCustomSchedulerStorageField);
begin
  if FFields.Remove(AItem) <> -1 then
  begin
    if not IsDestroying then
    try
      DataController.RemoveValueDef(AItem);
      DataController.RemoveItem(AItem);
    finally
      DataController.UpdateItemIndexes;
      LayoutChanged;
    end;
  end;
end;

function TcxCustomSchedulerStorage.IsDataSettingsValid: Boolean;
begin
  Result := IsActive and FEventTypeField.IsActive and FOptionsField.IsActive and
    FStartField.IsActive and FFinishField.IsActive;
end;

procedure TcxCustomSchedulerStorage.LayoutChanged;
begin
  SendNotification(nil);
end;

procedure TcxCustomSchedulerStorage.Loaded;
begin
  inherited Loaded;
  DataController.UpdateItemIndexes;
  DataController.Loaded;
  FullRefresh;
end;

procedure TcxCustomSchedulerStorage.Notification(
  AComponent: TComponent; Operation: TOperation);
begin
  if (Resources <> Nil) and (Operation = opRemove)and (AComponent = Resources.Images) then
    Resources.Images := nil;
  inherited Notification(AComponent, Operation);
end;

procedure TcxCustomSchedulerStorage.PostEvent(
  AEvent: TcxSchedulerEvent);
begin
  BeginUpdate;
  try
    if AEvent.IsNewEvent then
    begin
      FNewEvents.Remove(AEvent);
      FEventsList.Add(AEvent);
      if DoEventInserted(AEvent) then Exit;
      if CheckRequiredFields then
      begin
        if not AddEventRecord(AEvent) then
          AEvent.EndEditing;
      end
      else
        AEvent.Free;
    end
    else
      PostEditingData(AEvent);
    SendNotification(AEvent, False, False);
    Changed;
  finally
    EndUpdate;
  end;
end;

procedure TcxCustomSchedulerStorage.PostEditingData(
  AEvent: TcxSchedulerEvent);
var
  AFieldIndex: Integer;
begin
  if DoEventModified(AEvent) then Exit;
  if not GetIsBoundMode then
  begin
    ExchangePointers(AEvent.FRecordHandle, AEvent.FEditingRecordHandle);
    Exit;
  end;
  BeginUpdateDataController;
  try
    BeforePostEditingData(AEvent);
    for AFieldIndex := 0 to AEvent.ValueCount - 1 do
      SetPostFieldValue(AEvent, AFieldIndex);
    ExchangePointers(AEvent.FRecordHandle, AEvent.FEditingRecordHandle);
    AfterPostEditingData(AEvent);
  finally
    EndUpdateDataController;
  end;
end;

procedure TcxCustomSchedulerStorage.InternalRemove(AEvent: TcxSchedulerEvent);
begin
  if not AEvent.FIsDeletion then
  begin
    FEventsList.Remove(AEvent);
    FNewEvents.Remove(AEvent);
    Reminders.RemoveEvent(AEvent);
  end;
end;

procedure TcxCustomSchedulerStorage.SendNotification(AEvent: TcxSchedulerEvent;
  AIsRemoved: Boolean = False; ACheckLockCount: Boolean = True);
var
  I: Integer;
  AIntf: IcxSchedulerStorageListener;
  AIntf2: IcxSchedulerStorageListener2;
begin
  if (LockCount <> 0) and ACheckLockCount then Exit;
  for I := Listeners.Count - 1 downto 0 do
    if AEvent <> nil then
    begin
      if Supports(Listeners[I], IcxSchedulerStorageListener2, AIntf2) then
      begin
        if AIsRemoved then
          AIntf2.RemoveEvent(AEvent)
        else
          AIntf2.AddEvent(AEvent);
      end;
    end
    else
      if Supports(Listeners[I], IcxSchedulerStorageListener, AIntf) then
      begin
        if AIsRemoved then
          AIntf.StorageRemoved(Self)
        else
          if not IsUpdatingMode then
            AIntf.StorageChanged(Self)
          else
            FIsChanged := True;
      end;
end;

procedure TcxCustomSchedulerStorage.SetEventValue(
  AEvent: TcxSchedulerEvent; AFieldIndex: Integer; const AValue: Variant);
begin
  if AFieldIndex < 0 then
    AEvent.ID := AValue;
  if AEvent.IsEditing then
    SetValue(AEvent.EditingRecordHandle, AFieldIndex, AValue)
  else
  begin
    SetValue(AEvent.RecordHandle, AFieldIndex, AValue);
    Changed;
  end;
end;

procedure TcxCustomSchedulerStorage.SetPostFieldValue(AEvent: TcxSchedulerEvent;
  AFieldIndex: Integer);
var
  AEventValue, APatternValue, AStoredValue: Variant;
begin
  AEventValue := AEvent.EditValues[AFieldIndex];
  if (AEvent.Pattern <> nil) and (AEvent.EventType = etCustom) and CanGetValueFromPattern(AFieldIndex) then
  begin
    APatternValue := AEvent.Pattern.Values[AFieldIndex];
    AStoredValue := AEvent.Values[AFieldIndex];
    if not VarEquals(AEventValue, APatternValue) or
      (not VarEquals(AEventValue, AStoredValue) and not VarIsNull(AStoredValue)) then
        SetValueBeforePost(AEvent, AFieldIndex);
  end
  else
    SetValueBeforePost(AEvent, AFieldIndex);
end;

procedure TcxCustomSchedulerStorage.SetValue(const ARecordHandle: Pointer;
  const AFieldIndex: Integer; const AValue: Variant);
begin
  if AFieldIndex < 0 then Exit;
  TcxDataStorageHelper.SetValue(ARecordHandle, Fields[AFieldIndex].ValueDef, AValue);
end;

procedure TcxCustomSchedulerStorage.SetValueBeforePost(
  AEvent: TcxSchedulerEvent; AItemIndex: Integer);
begin
 // todo: for DB
end;

procedure TcxCustomSchedulerStorage.SynchronizeEventsWithRecords;
begin
end;

procedure TcxCustomSchedulerStorage.UpdateControl(
  AInfo: TcxUpdateControlInfo);
begin
  if (AInfo is TcxDataChangedInfo) or (AInfo is TcxUpdateRecordInfo) then
    FullRefresh;
end;

procedure TcxCustomSchedulerStorage.UpdateData;
begin
end;

procedure TcxCustomSchedulerStorage.UpdateItemIndexes;
var
  I: Integer;
begin
  for I := 0 to InternalFields.Count - 1 do
    TcxCustomSchedulerStorageField(InternalFields.Items[I]).FIndex := I;
  for I := 0 to CustomFields.Count - 1 do
    TcxCustomSchedulerStorageField(CustomFields.Items[I]).FIndex := I + InternalFields.Count;
  FFields.Sort(@cxSchedulerStorageFieldsCompare);
  SendNotification(nil);
end;

procedure TcxCustomSchedulerStorage.UpdateRemindersTimer;
begin
  if (FUpdateRemindersTimer <> nil) or
    (not Reminders.Active or (csDesigning in ComponentState)) then Exit;
  FUpdateRemindersTimer := TTimer.Create(nil);
  FUpdateRemindersTimer.Interval := 50;
  FUpdateRemindersTimer.OnTimer := UpdateRemindersTimerEvent;
end;

procedure TcxCustomSchedulerStorage.UpdateStructure;
var
  I, J: Integer;
  AEvent: TcxSchedulerEvent;
begin
  if IsLocked or Reminders.UpdatingReminderList then
    Exit;
  Reminders.StopTimers;
  for I := 0 to EventCount - 1 do
  begin
    AEvent := TcxSchedulerEvent(FEventsList.List.List[I]);
    AEvent.FIsDataValid := False;
    AEvent.FIndex := I;
    if AEvent.EventType = etPattern then
    begin
      AEvent.FLink := nil;
      for J := 0 to EventCount - 1 do
        if J <> I then
          AEvent.CheckRecurrenceLink(FEventsList.List.List[J]);
    end;
  end;
  BeginStructureUpdating;
  try
    for I := 0 to EventCount - 1 do
      Events[I].ResetReferences;
    for I := 0 to EventCount - 1 do
      Events[I].RefreshTaskLinks;
    FSortedList.Count := 0;
  finally
    EndStructureUpdating;
  end;
  UpdateRemindersTimer;
  RefreshNeeded := False;
end;

function TcxCustomSchedulerStorage.GetDataField(
  AIndex: Integer): TcxCustomSchedulerStorageField;
begin
  Result := TcxCustomSchedulerStorageField(FFields[AIndex]);
end;

function TcxCustomSchedulerStorage.GetEvent(
  AIndex: Integer): TcxSchedulerEvent;
begin
  Result := FEventsList[AIndex];
end;

function TcxCustomSchedulerStorage.GetEventCount: Integer;
begin
  Result := FEventsList.Count;
end;

function TcxCustomSchedulerStorage.GetField(AIndex: Integer): TcxCustomSchedulerStorageField;
begin
  Result := TcxCustomSchedulerStorageField(FFields[AIndex]);
end;

function TcxCustomSchedulerStorage.GetFieldCount: Integer;
begin
  Result := FFields.Count
end;

function TcxCustomSchedulerStorage.GetInternalFieldCount: Integer;
begin
  Result := FInternalFields.Count;
end;

function TcxCustomSchedulerStorage.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxCustomSchedulerStorage.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxCustomSchedulerStorage.GetIsLocked: Boolean;
begin
  Result := DataController.LockCount + LockCount > 0;
end;

function TcxCustomSchedulerStorage.GetCanUpdateReminders: Boolean;
begin
  Result := not (FIsLoading or (ComponentState * [csDesigning, csDestroying, csLoading] <> []) or
    IsLocked);
end;

function TcxCustomSchedulerStorage.GetIsUpdatingMode: Boolean;
begin
  Result := FIsLoading or ([csLoading, csDestroying] * ComponentState <> []) or (FStructureUpdatingCount > 0);
end;

function TcxCustomSchedulerStorage.GetResourceCount: Integer;
begin
  Result := Resources.ResourceItems.Count;
end;

function TcxCustomSchedulerStorage.GetResourceID(AIndex: Integer): Variant;
begin
  Result := Resources.ResourceItems[AIndex].ResourceID;
end;

function TcxCustomSchedulerStorage.GetResourceName(AIndex: Integer): string;
begin
  Result := Resources.ResourceItems[AIndex].DisplayName;
end;

function CompareTime(const T1, T2: TDateTime): Integer; inline;
begin
  Result := 0;
  if T1 > T2 then
    Result := 1
  else
     if T1 < T2 then
       Result := -1;
end;

function CompareEvents(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  Result := dxCompareValues(AEvent1, AEvent2);
  if Result = 0 then
    Exit;
  Result := CompareTime(AEvent1.Start, AEvent2.Start);
  if Result = 0 then
    Result := CompareTime(AEvent1.Finish, AEvent2.Finish);
  if Result = 0 then
    Result := dxCompareValues(AEvent1.Index, AEvent2.Index);
end;

function TcxCustomSchedulerStorage.GetSortedList: TdxFastList;
begin
  Result := FSortedList;
  if FSortedList.Count <> FEventsList.Count then
  begin
    TdxMultithreadedListHelper.IterateItems(FEventsList.List,
      procedure(AEvent: Pointer; AIndex: Integer)
      begin
        TcxSchedulerEvent(AEvent).CheckCalculated;
      end
    );
    FSortedList.Assign(FEventsList.List);
    FSortedList.Sort(TListSortCompare(@CompareEvents), True);
  end;
end;

procedure TcxCustomSchedulerStorage.SetActive(AValue: Boolean);
begin
  if FActive = AValue then Exit;
  FActive := AValue;
  if not Active then
    Reminders.StopReminder
  else
    if Reminders.Active then
      Reminders.StartReminder;
  LayoutChanged;
end;

procedure TcxCustomSchedulerStorage.SetEvent(
  AIndex: Integer; AValue: TcxSchedulerEvent);
begin
  Events[AIndex].Assign(AValue);
end;

procedure TcxCustomSchedulerStorage.SetHolidays(AValue: TcxSchedulerHolidays);
begin
  if AValue <> FHolidays then
  begin
    if FHolidays <> nil then
      FHolidays.RemoveListener(Self);
    FHolidays := AValue;
    if FHolidays <> nil then
      FHolidays.AddListener(Self);
    FullRefresh;
  end;
end;

procedure TcxCustomSchedulerStorage.SetOnFilterEvent(
  AValue: TcxSchedulerFilterEventEvent);
begin
  if @FOnFilterEvent <> @AValue then
  begin
    FOnFilterEvent := AValue;
    SendNotification(nil);
  end;
end;

procedure TcxCustomSchedulerStorage.SetReminders(
  AValue: TcxSchedulerReminders);
begin
  FReminders.Assign(AValue);
end;

procedure TcxCustomSchedulerStorage.SetResources(
  AValue: TcxSchedulerStorageResources);
begin
  FResources.Assign(AValue);
end;

procedure TcxCustomSchedulerStorage.SetStoreUsingGlobalTime(AValue: Boolean);
begin
  if StoreUsingGlobalTime <> AValue then
  begin
    FStoreUsingGlobalTime := AValue;
    CalculateTimeBias;
  end;
end;

procedure TcxCustomSchedulerStorage.StopUpdateRemindersTimer;
begin
  FreeAndNil(FUpdateRemindersTimer);
end;

procedure TcxCustomSchedulerStorage.UpdateReminders;
begin
  StopUpdateRemindersTimer;
  with Reminders do if Active then Refresh;
end;

procedure TcxCustomSchedulerStorage.UpdateRemindersTimerEvent(
  Sender: TObject);
begin
  UpdateReminders;
end;

{ TcxSchedulerStorage }

procedure TcxSchedulerStorage.Assign(Source: TPersistent);
var
  AMemStream: TMemoryStream;
begin
  if Source is TcxSchedulerStorage then
  begin
    AMemStream := TMemoryStream.Create;
    try
      TcxSchedulerStorage(Source).SaveToStream(AMemStream);
      AMemStream.Position := 0;
      LoadFromStream(AMemStream);
    finally
      AMemStream.Free;
    end;
  end
  else
    inherited Assign(Source);
end;

procedure TcxSchedulerStorage.SaveToFile(
  const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TcxSchedulerStorage.SaveToStream(AStream: TStream);
begin
  WriteAnsiStringProc(AStream, scxSchedulerSignature);
  AStream.WriteBuffer(cxSchedulerStreamVersion, SizeOf(cxSchedulerStreamVersion));
  DataController.InitializeRecordIndexes;
  DataController.SaveToStream(AStream);
  DataController.InitializeEventHandles;
end;

procedure TcxSchedulerStorage.LoadFromFile(const AFileName: string);
var
  AStream: TFileStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead);
  try
    LoadFromStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TcxSchedulerStorage.LoadFromStream(
  AStream: TStream);
var
  I, C: Integer;
  AVersion: Double;
  AMemStream: TMemoryStream;
begin
  AMemStream := TMemoryStream.Create;
  BeginUpdate;
  try
    SaveToStream(AMemStream);
    try
      Clear;
      AVersion := GetStreamVersion(AStream, C, CustomFields.Count);
      if AVersion < 0 then
        cxSchedulerError(cxGetResourceString(@cxSDataInvalidStreamFormat));
      try
        if AVersion < cxSchedulerStreamVersion then
          DestroyDefaultFields(AVersion);
        DataController.LoadFromStream(AStream);
        for I := 0 to DataController.RecordCount - 1 do
          CreateEventInternal.RecordHandle := Pointer(I);
        DataController.InitializeEventHandles;
        DoUpdateStructure;
        for I := 0 to EventCount - 1 do
          SendNotification(Events[I], False, False);
      finally
        if AVersion < cxSchedulerStreamVersion then
          CreateDefaultFields(AVersion);
      end;
    except
      Clear;
      DataController.LoadFromStream(AMemStream);
      raise;
    end;
  finally
    AMemStream.Free;
    if IsUpdatingMode then
      DoRefresh;
    EndUpdate;
  end;
end;

procedure TcxSchedulerStorage.DefineProperties(
  Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('BinaryData', LoadFromStream,
    SaveToStream, EventCount > 0);
end;

function TcxSchedulerStorage.GetEventID(AEvent: TcxSchedulerEvent): Variant;
var
  P: PByte;
begin
  Result := FEventIDCounter;
  P := PByte(AEvent.EditingRecordHandle);
  Inc(P);
  PInteger(P)^ := Result;
  Inc(FEventIDCounter);
end;

function TcxSchedulerStorage.GetCustomFields: TcxSchedulerStorageFields;
begin
  Result := TcxSchedulerStorageFields(inherited CustomFields)
end;

procedure TcxSchedulerStorage.SetCustomFields(
  const AValue: TcxSchedulerStorageFields);
begin
  CustomFields.Assign(AValue);
end;

{ TcxSchedulerControlEventID }

constructor TcxSchedulerControlEventID.Create(AEvent: TcxSchedulerEvent);
begin
  EventType := AEvent.EventType;
  ID := AEvent.ID;
  ParentID := AEvent.ParentID;
  RecurrenceIndex := AEvent.RecurrenceIndex;
  PatternID := cxGetEventID(AEvent.Pattern);
end;

constructor TcxSchedulerControlEventID.Create(AEventID: TcxSchedulerControlEventID);
begin
  EventType := AEventID.EventType;
  ID := AEventID.ID;
  ParentID := AEventID.ParentID;
  RecurrenceIndex := AEventID.RecurrenceIndex;
  PatternID := AEventID.PatternID;
end;

function TcxSchedulerControlEventID.SameEvent(AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := VarEquals(AEvent.ID, ID) and VarEquals(AEvent.ParentID, ParentID) and
    (AEvent.RecurrenceIndex = RecurrenceIndex) and VarEquals(cxGetEventID(AEvent.Pattern), PatternID);
end;

function TcxSchedulerControlEventID.Equals(AValue: TcxSchedulerControlEventID): Boolean;
begin
  Result := (AValue = Self) or VarEquals(AValue.ID, ID) and
    VarEquals(AValue.ParentID, ParentID) and (AValue.RecurrenceIndex = RecurrenceIndex) and
    VarEquals(AValue.PatternID, PatternID);
end;

{ TcxSchedulerEventSelection }

constructor TcxSchedulerEventSelection.Create(
  ASourceEvents: TcxSchedulerEventList);
begin
  FKeys := TList.Create;
  FEvents := TList.Create;
  FSourceEvents := ASourceEvents;
end;

destructor TcxSchedulerEventSelection.Destroy;
begin
  InternalClear;
  FEvents.Free;
  FKeys.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEventSelection.Add(
  AEvent: TcxSchedulerControlEvent; AShift: TShiftState);
var
  AIndex: Integer;
  ASelected: Boolean;
begin
  ASelected := (AEvent <> nil) and AEvent.Selected;
  if ASelected and (AShift = [ssRight]) then Exit;
  if (AEvent = nil) or ([ssShift, ssCtrl] * AShift = []) then
    InternalClear;
  if AEvent <> nil then
  try
    AIndex := KeyIndexOf(AEvent);
    if (ssCtrl in AShift) and (AIndex >= 0) then
    begin
      Keys[AIndex].Free;
      FKeys.Delete(AIndex);
    end
    else
      if AIndex < 0 then
        FKeys.Add(CreateItem(AEvent));
  finally
    Update;
    DoEventSelectionChanged(AEvent);
  end;
end;
procedure TcxSchedulerEventSelection.Clear;
begin
  if Count = 0 then Exit;
  InternalClear;
  DoEventSelectionChanged(nil);
end;

function TcxSchedulerEventSelection.IsSelected(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := KeyIndexOf(AEvent) >= 0;
end;

procedure TcxSchedulerEventSelection.Select(AEvent: TcxSchedulerEvent);
begin
  InternalClear;
  FKeys.Add(TcxSchedulerControlEventID.Create(AEvent));
  DoEventSelectionChanged(nil);
end;

procedure TcxSchedulerEventSelection.Update;
begin
  FKeys.Sort(TListSortCompare(@cxCompareSelectionKeys));
  ReplaceSelection;
end;

procedure TcxSchedulerEventSelection.ClearKeys;
var
  I: Integer;
begin
  for I := FKeys.Count - 1 downto 0 do
    TObject(FKeys.List[I]).Free;
  FKeys.Clear;
end;

function TcxSchedulerEventSelection.CreateItem(
  AEvent: TcxSchedulerEvent): TcxSchedulerControlEventID;
begin
  Result := TcxSchedulerControlEventID.Create(AEvent);
end;

procedure TcxSchedulerEventSelection.DoEventSelectionChanged(
  AEvent: TcxSchedulerControlEvent);
begin
  if Assigned(FOnEventSelectionChanged) then
    FOnEventSelectionChanged(AEvent);
end;

function TcxSchedulerEventSelection.KeyIndexOf(
  AEvent: TcxSchedulerControlEvent): Integer;
var
  I, L, R, C: Integer;
  AItemForSearch: TcxSchedulerControlEventID;
begin
  Result := -1;
  L := 0;
  R := KeyCount - 1;
  if R < L then Exit;
  AItemForSearch := CreateItem(AEvent);
  try
    while L <= R do
    begin
      I := (L + R) shr 1;
      C := cxCompareSelectionKeys(Keys[I], AItemForSearch);
      if C < 0 then
        L := I + 1
      else
      begin
        R := I - 1;
        if C = 0 then
        begin
          Result := I;
          Break;
        end;
      end;
    end;
  finally
    AItemForSearch.Free;
  end;
end;

function TcxSchedulerEventSelection.IndexOf(
  AEvent: TcxSchedulerControlEvent): Integer;
begin
  Result := FEvents.IndexOf(AEvent);
end;

procedure TcxSchedulerEventSelection.InternalClear;
begin
  ClearKeys;
  FEvents.Clear;
end;

procedure TcxSchedulerEventSelection.ReplaceSelection;
var
  AIndex, I: Integer;
begin
  FEvents.Clear;
  for I := 0 to FSourceEvents.Count - 1 do
  begin
    AIndex := KeyIndexOf(TcxSchedulerControlEvent(FSourceEvents[I]));
    if AIndex <> -1 then FEvents.Add(FSourceEvents[I]);
  end;
  ClearKeys;
  for I := 0 to FEvents.Count - 1 do
    FKeys.Add(CreateItem(TcxSchedulerControlEvent(FEvents[I])));
  FKeys.Sort(TListSortCompare(@cxCompareSelectionKeys));
end;

function TcxSchedulerEventSelection.GetCount: Integer;
begin
  Result := FEvents.Count;
end;

function TcxSchedulerEventSelection.GetItem(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent(FEvents.List[AIndex])
end;

function TcxSchedulerEventSelection.GetKey(
  AIndex: Integer): TcxSchedulerControlEventID;
begin
  Result := FKeys.List[AIndex];
end;

function TcxSchedulerEventSelection.GetKeyCount: Integer;
begin
  Result := FKeys.Count;
end;

{ TcxSchedulerEventList }

constructor TcxSchedulerEventList.Create;
begin
  FItems := TdxFastList.Create;
end;

destructor TcxSchedulerEventList.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEventList.Assign(ASource: TcxSchedulerEventList; AOperator: TListAssignOp = laCopy);
begin
  if ASource <> nil then
    FItems.Assign(ASource.FItems, AOperator);
end;

function TcxSchedulerEventList.Add(AEvent: TcxSchedulerEvent): Integer;
begin
  Result := FItems.Add(AEvent);
  EventAdded(AEvent);
end;

procedure TcxSchedulerEventList.Clear;
begin
  DoClear(False);
end;

procedure TcxSchedulerEventList.Delete(AIndex: Integer);
begin
  FItems.Delete(AIndex);
end;

procedure TcxSchedulerEventList.DestroyItem(AIndex: Integer);
var
  AEvent: TcxSchedulerEvent;
begin
  AEvent := Items[AIndex];
  Delete(AIndex);
  DestroyEvent(AEvent);
end;

procedure TcxSchedulerEventList.DestroyItems;
begin
  DoClear(True);
end;

function TcxSchedulerEventList.Remove(AEvent: TcxSchedulerEvent): Integer;
begin
  if (FItems.Count > 0) and (FItems.Last = AEvent) then
  begin
    FItems.Count := FItems.Count - 1;
    Result := FItems.Count;
  end
  else
    Result := FItems.Remove(AEvent);
end;

function TcxSchedulerEventList.IndexOf(AEvent: TcxSchedulerEvent): Integer;
begin
  Result := FItems.IndexOf(AEvent);
end;

procedure TcxSchedulerEventList.Sort(ACompare: TcxCompareEventsProc);
begin
  FItems.Sort(TListSortCompare(@ACompare));
end;

procedure TcxSchedulerEventList.BeforeDestroyEvent(AEvent: TcxSchedulerEvent);
begin
  AEvent.FIsDeletion := True;
  AEvent.ParentGroup := nil;
end;

procedure TcxSchedulerEventList.EventAdded(AEvent: TcxSchedulerEvent);
begin
end;

procedure TcxSchedulerEventList.DestroyEvent(AEvent: TcxSchedulerEvent);
begin
  BeforeDestroyEvent(AEvent);
  AEvent.Free;
end;

procedure TcxSchedulerEventList.DoClear(AFreeItems: Boolean);
var
  I: Integer;
begin
  if AFreeItems then
  begin
    for I := FItems.Count - 1 downto 0 do
      BeforeDestroyEvent(Items[I]);
    for I := FItems.Count - 1 downto 0 do
      DestroyEvent(Items[I]);
  end;
  FItems.Clear;
end;

function TcxSchedulerEventList.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxSchedulerEventList.GetFirst: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(FItems.First);
end;

function TcxSchedulerEventList.GetEvent(AIndex: Integer): TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(FItems.List[AIndex]);
end;

function TcxSchedulerEventList.GetLast: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(FItems.Last);
end;

{ TcxSchedulerFilteredEventList }

constructor TcxSchedulerFilteredEventList.Create;
begin
  inherited Create;
  UseTimeRange := True;
end;

destructor TcxSchedulerFilteredEventList.Destroy;
begin
  DestroyItems;
  inherited Destroy;
end;

procedure TcxSchedulerFilteredEventList.ValidateTimeBias(
  AEvent: TcxSchedulerControlEvent);
begin
  if AEvent.Source <> nil then
    AEvent.FTimeBias := GetTimeBias + GetTimeBiasDaylightSavingTime(AEvent.Source.Start)
  else
    AEvent.FTimeBias := GetTimeBias + GetTimeBiasDaylightSavingTime(AEvent.Start);
  AEvent.IsDataValid := False;
end;

function TcxSchedulerFilteredEventList.AddEvent(
  AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent;
begin
  Result := CreateControlEvent(AEvent);
  Add(Result);
end;

function TcxSchedulerFilteredEventList.AddOccurrence(
  ACalculator: TcxSchedulerOccurrenceCalculator): TcxSchedulerControlEvent;
begin
  Result := nil;
  if not CanAddEvent(ACalculator.Event, ACalculator.Index) then
    Exit;
  if ReminderEventsOnly and not ACalculator.Event.Reminder then Exit;
  begin
    if ReminderEventsOnly then
    begin
      if NeedAddOccurrenceForReminder(ACalculator) then
      begin
        if (ACalculator.FOccurrence <> nil) and (dxDateOf(ACalculator.ReminderStart) < dxDateOf(FNow)) then
        begin
          FreeAndNil(ACalculator.FOccurrence);
          Delete(ACalculator.FOccurrencePos);
        end;
        ACalculator.FOccurrencePos := Count;
        Result := AddEvent(ACalculator.Event);
        ACalculator.FOccurrence := Result;
      end;
    end
    else
      if not UseTimeRange or (ACalculator.OccurrenceStart <= Finish) and (ACalculator.OccurrenceFinish >= Start) then
        Result := AddEvent(ACalculator.Event);
    if Result = nil then Exit;
    if ACalculator.Event.RecordHandle <> nil then
      Result.ParentID := ACalculator.Event.ID;
    Result.UTCStart := ACalculator.OccurrenceStart + ACalculator.Event.RecurrenceInfo.DisplayTimeBias;
    Result.UTCFinish := ACalculator.OccurrenceFinish + ACalculator.Event.RecurrenceInfo.DisplayTimeBias;
    Result.RecurrenceIndex := ACalculator.Index;
    Result.EventType := etOccurrence;
    Result.FPattern := ACalculator.Event;
    Result.FInfo.Options := ACalculator.Event.FInfo.Options;
    Result.ReminderDate := ACalculator.Event.ReminderDate;
    ValidateTimeBias(Result);
  end;
  if (Result <> nil) and not Storage.DoFilterEvent(Result) then
  begin
    if ACalculator.FOccurrence = Result then
      ACalculator.FOccurrence := nil;
    FreeAndNil(Result);
    Delete(Count - 1);
  end;
end;

function TcxSchedulerFilteredEventList.CanAddEvent(
  ASource: TcxSchedulerEvent; ARecurrenceIndex: Integer): Boolean;
begin
  Result := True;
end;

function TcxSchedulerFilteredEventList.CanSmartUpdate(AStorage: TcxCustomSchedulerStorage;
  const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean;
begin
  Init(AStart, AFinish, AStorage, True);
  FReminderEventsOnly := False;
  Result := False;
end;

procedure TcxSchedulerFilteredEventList.Changed;
begin
end;

procedure TcxSchedulerFilteredEventList.CheckEvent(
  AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
begin
  if AEvent.EventType in [etNone, etCustom] then
    CheckSimpleEvent(AEvent, AResourceID, AddAll)
  else
    if (AEvent.EventType = etPattern) or AEvent.SkipExceptions then
      CheckRecurrenceEvent(AEvent, AResourceID, AddAll);
end;

function TcxSchedulerFilteredEventList.CheckResourceID(
  const AEventResourceID, AResourceID: Variant): Boolean;
begin
  Result := VarIsNull(AResourceID) or
    (VarIsNull(AEventResourceID) and ExceptEventsWithoutResources);
end;

procedure TcxSchedulerFilteredEventList.CheckRecurrenceEvent(
  AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
var
  AFinish: TDateTime;
  AOccurrence: TcxSchedulerEvent;
  ACalculator: TcxSchedulerOccurrenceCalculator;
begin
  if not AEvent.FInfo.Visible then
    Exit;
  AEvent.RecurrenceInfo.DisplayTimeBias := GetTimeBias;
  AFinish := FFinish;
  if not UseTimeRange then
  begin
    AFinish :=  AEvent.RecurrenceInfo.Finish + 1;
    if AFinish < AEvent.Start then
      AFinish := AEvent.Start + cxMaxCheckedDuration * 100;
  end;
  ACalculator := TcxSchedulerOccurrenceCalculator.Create(AEvent, AEvent.Start, AFinish,
    ReminderEventsOnly);
  try
    while ACalculator.GetNextOccurrence do
    begin
      if not AEvent.GetOccurrenceByIndex(ACalculator.Index, AOccurrence) then
        if AddAll or (CheckResourceID(AEvent.ResourceID, AResourceID) or AEvent.IsSharedWithResource(AResourceID)) then
          AddOccurrence(ACalculator);
    end;
  finally
    ACalculator.Free;
  end;
end;

procedure TcxSchedulerFilteredEventList.CheckSimpleEvent(
  AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
begin
  if not Storage.DoFilterEvent(AEvent) or not CanAddEvent(AEvent, AEvent.RecurrenceIndex) then
    Exit;
  if FReminderEventsOnly then
  begin
    if AEvent.Reminder and (AEvent.ReminderDate <= Finish) then
      if AddAll or (CheckResourceID(AEvent.ResourceID, AResourceID) or AEvent.IsSharedWithResource(AResourceID)) then
        AddEvent(AEvent).FPattern := AEvent.Pattern;
  end
  else
    if not UseTimeRange or ((AEvent.Start <= Finish) and (AEvent.Finish >= Start)) then
      if AddAll or (CheckResourceID(AEvent.ResourceID, AResourceID) or AEvent.IsSharedWithResource(AResourceID)) then
        AddEvent(AEvent).FPattern := AEvent.FPattern;
end;

function TcxSchedulerFilteredEventList.CreateControlEvent(
  AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent.Create(AEvent.Storage, False);
  Result.FSource := AEvent;
  if not PostponedInitialization or (AEvent.EventType <> etNone) then
    PostInitializeEvent(Result, False);
end;

function TcxSchedulerFilteredEventList.GetTimeBias: Double;
begin
  Result := {DateTimeHelper.CurrentTimeZoneBias - }Storage.TimeBias;
end;

function TcxSchedulerFilteredEventList.GetTimeBiasDaylightSavingTime(
  ATime: TDateTime): TDateTime;
begin
  Result := 0;
end;

procedure TcxSchedulerFilteredEventList.Init(const AStart, AFinish: TDateTime;
  AStorage: TcxCustomSchedulerStorage; AClearItems: Boolean);
begin
  if AClearItems then
    DestroyItems;
  FStart := AStart;
  FFinish := AFinish;
  FStorage := AStorage;
end;

function TcxSchedulerFilteredEventList.NeedAddOccurrenceForReminder(
  ACalculator: TcxSchedulerOccurrenceCalculator): Boolean;
begin
  Result :=
    (ACalculator.DismissDate < dxDateOf(Now)) and
    (dxDateOf(ACalculator.OccurrenceStart) > ACalculator.DismissDate) and
    (ACalculator.ReminderStart < Finish);
end;

procedure TcxSchedulerFilteredEventList.PostInitializeEvent(AEvent: TcxSchedulerControlEvent; ACheckType: Boolean);
begin
  if ACheckType and (AEvent.FInfo.EventType <> etNone) then
    Exit;
  AEvent.InitFromSource(AEvent.FSource);
  AEvent.FTimeBias := AEvent.TimeBias;
  AEvent.FLink := AEvent.FLink;
  ValidateTimeBias(AEvent);
  AEvent.CheckCalculated;
end;

procedure TcxSchedulerFilteredEventList.SetCheckRange(AStart, AFinish: Integer);
begin
end;

procedure TcxSchedulerFilteredEventList.SetUseTimeRange(AValue: Boolean);
begin
  FUseTimeRange := AValue;
end;

function TcxSchedulerFilteredEventList.GetEvent(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent(FItems.List[AIndex])
end;

{ TcxSchedulerCachedEventList }

constructor TcxSchedulerCachedEventList.Create;
begin
  inherited Create;
  FAbsoluteItems := TdxFastList.Create;
  FSelection := CreateSelection;
  FClones := CreateCloneList;
  FShowEventsWithoutResource := True;
  FTimeZone := -1;
  FDictionary := TDictionary<TcxSchedulerEvent, Boolean>.Create;
end;

destructor TcxSchedulerCachedEventList.Destroy;
begin
  FreeAndNil(FDictionary);
  FreeAndNil(FClones);
  FreeAndNil(FSelection);
  FreeAndNil(FAbsoluteItems);
  inherited Destroy;
end;

function TcxSchedulerCachedEventList.Add(AEvent: TcxSchedulerEvent): Integer;
begin
  Result := inherited Add(AEvent);
  if AEvent.EventType = etNone  then
    FDictionary.AddOrSetValue(TcxSchedulerControlEvent(AEvent).Source, True);
end;

procedure TcxSchedulerCachedEventList.AfterEditing(AEvent: TcxSchedulerControlEvent);
begin
  AEvent.FIsEditing := False;
end;

procedure TcxSchedulerCachedEventList.BeforeEditing(
  AEvent: TcxSchedulerControlEvent; AIsInplace: Boolean);
begin
  AEvent.FIsEditing := AIsInplace;
end;

procedure TcxSchedulerCachedEventList.BeforeUpdate;
begin
  FExpandedTimeRange := CalculateNecessaryDate(
    FMinNecessaryDate, FMaxNecessaryDate);
end;

procedure TcxSchedulerCachedEventList.DeleteClone(AClone: TcxSchedulerControlEvent);
begin
  FClones.Remove(AClone);
  FAbsoluteItems.Remove(AClone);
end;

procedure TcxSchedulerCachedEventList.DeleteEvent(
  AEvent: TcxSchedulerControlEvent);
begin
  if (AEvent.Source <> nil) or (AEvent.EventType = etOccurrence) then
    AEvent.Delete
  else
  begin
    FItems.Remove(AEvent);
    FAbsoluteItems.Remove(AEvent);
    AEvent.Free;
  end;
end;

procedure TcxSchedulerCachedEventList.CalculateClonesRange(
  var AMinDate, AMaxDate: TDateTime);
var
  I: Integer;
begin
  if Clones.Count = 0 then Exit;
  AMinDate := Clones[0].Start;
  AMaxDate := Clones[0].Finish;
  for I := 1 to Clones.Count - 1 do
  begin
    AMinDate := Min(AMinDate, Clones[I].Start);
    AMaxDate := Max(AMaxDate, Clones[I].Finish);
  end;
end;

function TcxSchedulerCachedEventList.CalculateNecessaryDate(
  var AMinDate, AMaxDate: TDateTime): Boolean;
var
  AMin, AMax: TDateTime;
begin
  Result := (Clones.Count > 0) or (Selection.Count > 0);
  if not Result then Exit;
  if Clones.Count > 0 then
    CalculateClonesRange(AMinDate, AMaxDate);
  if Selection.Count > 0 then
    CalculateSelectionRange(AMin, AMax)
  else
    Exit;
  if Clones.Count > 0 then
  begin
    AMinDate := Min(AMinDate, AMin);
    AMaxDate := Max(AMaxDate, AMax);
  end
  else
  begin
    AMinDate := AMin;
    AMaxDate := AMax;
  end;
end;

procedure TcxSchedulerCachedEventList.CalculateSelectionRange(
  var AMinDate, AMaxDate: TDateTime);
var
  I: Integer;
begin
  if Selection.Count = 0 then Exit;
  AMinDate := Selection.Items[0].Start;
  AMaxDate := Selection.Items[0].Finish;
  for I := 1 to Selection.Count - 1 do
  begin
    AMinDate := Min(AMinDate, Selection.Items[I].Start);
    AMaxDate := Max(AMaxDate, Selection.Items[I].Finish);
  end;
end;

procedure TcxSchedulerCachedEventList.CancelClones;
begin
  FHasClones := False;
  FClones.DestroyItems;
  Changed;
end;

function TcxSchedulerCachedEventList.CreateEvent(AInsertToList: Boolean): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent.Create(Storage);
  Result.FIsSource := True;
  Result.FTimeBias := FTimeBias;
  if AInsertToList then
  begin
    FItems.Add(Result);
    FAbsoluteItems.Add(Result);
  end;
end;

function TcxSchedulerCachedEventList.CreateClones: TcxSchedulerEventList;
var
  I: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  FClones.DestroyItems;
  for I := 0 to Selection.Count - 1 do
  begin
    AEvent := CreateControlEvent(Selection.Items[I]);
    Selection.Items[I].FIsSource := True;
    AEvent.FIsClone := True;
    AEvent.FPattern := AEvent.Source.Pattern;
    FClones.Add(AEvent);
  end;
  Result := FClones;
  FHasClones := FClones.Count > 0;
  FExpandedTimeRange := CalculateNecessaryDate(FMinNecessaryDate, FMaxNecessaryDate);
  Changed;
end;

procedure TcxSchedulerCachedEventList.ExtractEvents(const ADate: TDateTime; AList: TcxSchedulerEventList);
var
  ADateNoTime: Integer;
begin
  AList.Clear;
  AList.List.Count := FAbsoluteItems.Count;
  ADateNoTime := Trunc(ADate);
  TdxMultithreadedListHelper.IterateItems(FAbsoluteItems,
    procedure(AEvent: Pointer; AItemIndex: Integer)
    begin
      if TcxSchedulerEvent(AEvent).IsDayEvent(ADateNoTime) and (AList.IndexOf(AEvent) = -1) then
        AList.List.List^[AItemIndex] := AEvent;
    end
  );
  AList.List.Pack();
end;

procedure TcxSchedulerCachedEventList.ExtractEventsByResource(AList: TcxSchedulerEventList;
  const AResource: TcxSchedulerStorageResourceItem; AllowUnassigned: Boolean);
begin
  AList.List.Count := FAbsoluteItems.Count;
  TdxMultithreadedListHelper.IterateItems(FAbsoluteItems,
    procedure(AEvent: Pointer; AItemIndex: Integer)
    begin
      if TcxSchedulerEvent(AEvent).IsResourceEvent(AResource, AllowUnassigned) then
        AList.List[AItemIndex] := AEvent
      else
        AList.List[AItemIndex] := nil;
    end
  );
  AList.List.Pack();
end;

procedure TcxSchedulerCachedEventList.ExtractUsedDays(AList: TcxSchedulerDateList);
var
  I, J: Integer;
begin
  AList.Count := 0;
  if Count = 0 then Exit;
  if UseTimeRange then
  begin
    for I := Trunc(FStart) to Trunc(FFinish) do
      if IsDayNoneEmpty(I) then
        AList.AddEx(I)
  end
  else
  begin
    for I := 0 to Count - 1 do
      for J := Trunc(AbsoluteItems[I].Start) to Trunc(AbsoluteItems[I].Finish) do
        if AList.IndexOf(J) < 0 then
          AList.Add(J);
  end
end;

function TcxSchedulerCachedEventList.HasConflict(IsDragCopy: Boolean; AStartDrag: Boolean): Boolean;
begin
  if AStartDrag then
    Result := (Selection.Count > 1) and HasIntersection(Selection.FEvents)
  else
    if (Selection.Count = 1) and (Clones.Count  = 0) then
      Result := HasIntersection(Selection[0])
    else
      Result := HasIntersection(Self, Clones, not IsDragCopy);
end;

function TcxSchedulerCachedEventList.HasIntersection(AEvent: TcxSchedulerControlEvent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to FItems.Count - 1 do
  begin
    if FItems[I] = AEvent then Continue;
    Result := IsIntersect(TcxSchedulerControlEvent(FItems[I]), AEvent);
    if Result then Exit;
  end;
end;

function TcxSchedulerCachedEventList.HasIntersection(AList: TList): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  for I := 0 to AList.Count - 1 do
    for J := 0 to AList.Count - 1 do
    begin
      Result := (I <> J) and IsIntersect(
        TcxSchedulerEvent(AList[I]), TcxSchedulerEvent(AList[J]));
      if Result then Exit;
    end;
end;

function TcxSchedulerCachedEventList.HasIntersection(
  AList1, AList2: TcxSchedulerEventList; AExcludeEquals: Boolean): Boolean;
var
  I, J: Integer;
begin
  Result := False;
  for I := 0 to AList1.Count - 1 do
    for J := 0 to AList2.Count - 1 do
    begin
      if AExcludeEquals and (AList1[I] = TcxSchedulerControlEvent(AList2[J]).Source) then Continue;
      Result := IsIntersect(AList1[I], AList2[J]);
      if Result then Exit;
    end;
end;

procedure TcxSchedulerCachedEventList.PostClones(ACopy: Boolean);
var
  I: Integer;
begin
  Storage.BeginUpdate;
  try
    for I := 0 to FClones.Count - 1 do
      InternalPost(TcxSchedulerControlEvent(FClones[I]), ACopy);
  finally
    Selection.Clear;
    CancelClones;
    Storage.EndUpdate;
  end;
end;

function TcxSchedulerCachedEventList.PostEvent(
  AEvent: TcxSchedulerControlEvent): TcxSchedulerEvent;
begin
  Result := nil;
  FBeforePostCount := Count;
  try
    AEvent.FIsEditing := False;
    if AEvent.Source = nil then
      Result := PostNewEvent(AEvent)
    else
      if AEvent.EventType in [etNone, etCustom] then
        Result := PostCloneForSimpleEvent(AEvent, False, AEvent.EventType)
      else
        if AEvent.EventType = etOccurrence then
          Result := PostCloneForSimpleEvent(AEvent, True, etCustom)
  except
    FNewEvent := nil;
    Storage.SendNotification(nil, False);
    raise;
  end;
end;

procedure TcxSchedulerCachedEventList.Sort(ACompare: TcxCompareEventsProc);
begin
  inherited Sort(ACompare);
  FAbsoluteItems.Sort(TListSortCompare(@ACompare));
end;

function TcxSchedulerCachedEventList.CanAddEvent(
  ASource: TcxSchedulerEvent; ARecurrenceIndex: Integer): Boolean;

  function FindEvent: Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to FItems.Count - 1 do
    begin
      Result := (TcxSchedulerControlEvent(FItems.List[I]).Source = ASource) and
        (ARecurrenceIndex = TcxSchedulerControlEvent(FItems.List[I]).RecurrenceIndex);
      if Result then
        Break;
    end;
  end;

begin
  Result := True;
  if FCheckStart < FCheckFinish then
  begin
    if ASource.EventType <> etNone then
      Result := not FindEvent
    else
      if ASource.CheckTimeRange(FCheckStart, FCheckFinish) then
        Result := not FDictionary.ContainsKey(ASource);
  end;
end;

function TcxSchedulerCachedEventList.CanSmartUpdate(AStorage: TcxCustomSchedulerStorage;
  const AStart, AFinish: TDateTime; const AResourceID: Variant): Boolean;
var
  I: Integer;
begin
  Result := not FNeedForceUpdate and UseTimeRange and ((AFinish > Start) and (AStart < Finish)) and
    VarEqualsExact(FResourceID, AResourceID) and ((Storage <> nil) and (Storage.Listeners.IndexOf(Self) >= 0)) ;
  Init(AStart, AFinish, AStorage, not Result);
  if Result then
  begin
    for I := Count - 1 downto 0 do
      if (Items[I].Start > AFinish) or (Items[I].Finish < AStart) then
      begin
        FDictionary.Remove(Items[I].Source);
        Items[I].Free;
        FItems.Delete(I);
      end
  end
  else
    FDictionary.Clear;
  FReminderEventsOnly := False;
  FNeedForceUpdate := False;
  FResourceID := AResourceID;
  FCheckStart := 0;
  FCheckFinish := 0;
end;

procedure TcxSchedulerCachedEventList.Changed;
begin
  if FChangeRef <> 0 then Exit;
  Inc(FChangeRef);
  PopulateAbsoluteItems;
  if (Count > FBeforePostCount) and (FNewEvent <> nil) then
  begin
    Selection.ClearKeys;
    Selection.Select(FNewEvent);
  end;
  FNewEvent := nil;
  Selection.Update;
  if FHasClones then
  begin
    CreateClones;
    PopulateAbsoluteItems;
  end;
  inherited Changed;
  Dec(FChangeRef);
end;

procedure TcxSchedulerCachedEventList.CheckEvent(
  AEvent: TcxSchedulerEvent; const AResourceID: Variant; AddAll: Boolean);
begin
  if AEvent.FInfo.Visible and (AddAll or CheckEventVisibility(AEvent, ShowEventsWithoutResource)) then
    inherited CheckEvent(AEvent, AResourceID, AddAll);
end;

function TcxSchedulerCachedEventList.CheckEventVisibility(
  AEvent: TcxSchedulerEvent; AIncludeUnassigned: Boolean): Boolean;
var
  I: Integer;
  AItems: TcxSchedulerStorageResourceItems;
begin
  Result := VarIsNull(AEvent.ResourceID);
  if Result and AIncludeUnassigned then
    Exit;

  AItems := Storage.Resources.ResourceItems;
  Result := AItems.Count = 0;
  for I := 0 to AItems.Count - 1 do
  begin
    if AItems[I].Visible and AEvent.IsResourceEvent(AItems[I], False) then
      Exit(True);
  end;
end;

function TcxSchedulerCachedEventList.CreateCloneList: TcxSchedulerFilteredEventList;
begin
  Result := TcxSchedulerFilteredEventList.Create;
end;

function TcxSchedulerCachedEventList.CreateSelection: TcxSchedulerEventSelection;
begin
  Result := TcxSchedulerEventSelection.Create(Self);
end;

procedure TcxSchedulerCachedEventList.DestroyEvent(AEvent: TcxSchedulerEvent);
begin
  inherited DestroyEvent(AEvent);
end;

procedure TcxSchedulerCachedEventList.DoClear(AFreeItems: Boolean);
begin
  if FClones <> nil then
    FClones.DestroyItems;
  inherited DoClear(True);
end;

procedure TcxSchedulerCachedEventList.EventAdded(
  AEvent: TcxSchedulerEvent);
begin
  TcxSchedulerControlEvent(AEvent).SelectionAdapter := Self;
  inherited EventAdded(AEvent);
end;

function TcxSchedulerCachedEventList.GetTimeBias: Double;
begin
  Result := FTimeBias;
end;

function TcxSchedulerCachedEventList.GetTimeBiasDaylightSavingTime(
  ATime: TDateTime): TDateTime;
begin
  Result := 0;
  if DaylightSaving then
    Result := TcxSchedulerDateTimeHelper.TimeZoneDaylightBias(ATime, FTimeZone) * MinuteToTime;
end;

procedure TcxSchedulerCachedEventList.Init(const AStart, AFinish: TDateTime;
  AStorage: TcxCustomSchedulerStorage; AClearItems: Boolean);
begin
  FHasClones := (FClones <> nil) and (FClones.Count > 0);
  if FExpandedTimeRange then
    inherited Init(Min(AStart, FMinNecessaryDate), Max(AFinish, FMaxNecessaryDate), AStorage, AClearItems)
  else
    inherited Init(AStart, AFinish, AStorage, AClearItems);
  FTimeBias := DateTimeHelper.TimeZoneBiasDelta(FTimeZone) + Storage.TimeBias;
  FAbsoluteItems.Clear;
end;

procedure TcxSchedulerCachedEventList.InternalPost(
  AEvent: TcxSchedulerControlEvent; ACopy: Boolean);
begin
  if AEvent.EventType = etNone then
    PostCloneForSimpleEvent(AEvent, ACopy, etNone)
  else
    PostCloneForRecurrenceEvent(AEvent, ACopy);
end;

function TcxSchedulerCachedEventList.IsDayNoneEmpty(ADay: Integer): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
  begin
    Result := Items[I].IsDayEvent(ADay);
    if Result then Break;
  end;
end;

function TcxSchedulerCachedEventList.IsIntersect(
  AEvent1, AEvent2: TcxSchedulerEvent): Boolean;
begin
  Result := not (AEvent1.IsFreeState or AEvent2.IsFreeState);
  if Result then
    Result := (((AEvent1.Start < AEvent2.Finish) and (AEvent2.Start < AEvent1.Finish)) or
      ((AEvent2.Start < AEvent1.Finish) and (AEvent1.Start < AEvent2.Finish)));
  if Result then
    Result := (VarIsNull(AEvent2.ResourceID) or AEvent1.IsSharedWithResource(AEvent2.ResourceID)) or
      (VarIsNull(AEvent1.ResourceID) or AEvent2.IsSharedWithResource(AEvent1.ResourceID));
  Result := Result and Storage.HasEventIntersect(AEvent2, AEvent1);
end;

function TcxSchedulerCachedEventList.IsSelected(
  AEvent: TcxSchedulerControlEvent): Boolean;
begin
  Result := AEvent.IsClone or (Selection.IsSelected(AEvent) and
    (AlwaysShowSelectedEvent or (FClones.Count = 0)));
end;

procedure TcxSchedulerCachedEventList.PopulateAbsoluteItems;
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  FAbsoluteItems.Count := 0;
  FAbsoluteItems.Capacity := GetAbsoluteCountInternal;
  for I := 0 to GetAbsoluteCountInternal - 1 do
  begin
    AEvent := GetAbsoluteItemInternal(I);
    if not UseTimeRange or ((AEvent.Start <= SelFinish) and (SelStart <= AEvent.Finish)) then
    begin
      FAbsoluteItems.Add(GetAbsoluteItemInternal(I));
    end;
  end;
end;

procedure TcxSchedulerCachedEventList.PostCloneForRecurrenceEvent(
  AEvent: TcxSchedulerControlEvent; ACopy: Boolean);
const
  ANewType: array[Boolean] of TcxEventType = (etCustom, etNone);
begin
  if not ACopy and (AEvent.EventType = etOccurrence) then
    PostCloneForSimpleEvent(AEvent, True, etCustom)
  else
    PostCloneForSimpleEvent(AEvent, ACopy, ANewType[ACopy]);
end;

function TcxSchedulerCachedEventList.PostCloneForSimpleEvent(
  AEvent: TcxSchedulerControlEvent; ACopy: Boolean; AType: TcxEventType): TcxSchedulerEvent;
var
  AStorageEvent, ACopyEvent: TcxSchedulerEvent;
begin
  AEvent.EventType := AType;
  if ACopy then
  begin
    ACopyEvent := Storage.CreateEvent;
    if AEvent.IsClone then
      TcxSchedulerControlEvent(AEvent.Source).SetSource(ACopyEvent)
    else
      AEvent.SetSource(ACopyEvent);
  end;
  if AEvent.IsClone then
    AStorageEvent := TcxSchedulerControlEvent(AEvent.Source).Source
  else
    AStorageEvent := AEvent.Source;
  AStorageEvent.BeginEditing;
  try
    AStorageEvent.FPattern := AEvent.Pattern;
    AStorageEvent.Assign(AEvent);
    AStorageEvent.MoveTo(AEvent.Start + AEvent.TimeBias - Storage.TimeBias);
    if AType <> etNone then
      AStorageEvent.ParentID := AEvent.Pattern.ID
    else
    begin
      AStorageEvent.ParentID := Null;
      AStorageEvent.RecurrenceIndex := -1;
      AStorageEvent.RecurrenceInfo.AssignDefaultValues;
    end;
  finally
    AStorageEvent.EndEditing;
    AStorageEvent.Post;
    Result := AStorageEvent;
  end;
end;

function TcxSchedulerCachedEventList.PostNewEvent(
  AEvent: TcxSchedulerControlEvent): TcxSchedulerEvent;
begin
  Result := FNewEvent;
  FBeforePostCount := Count - 1;
  FNewEvent := Storage.CreateEvent;
  try
    AEvent.SetSource(FNewEvent);
    FNewEvent.Assign(AEvent);
    FNewEvent.Post;
  except
    FNewEvent := nil;
    raise;
  end;
end;

procedure TcxSchedulerCachedEventList.SelectionAdd(
  AEvent: TcxSchedulerControlEvent; Shift: TShiftState);
begin
  Selection.Add(AEvent, Shift);
end;

procedure TcxSchedulerCachedEventList.SelectionClear;
begin
  Selection.Clear;
end;

procedure TcxSchedulerCachedEventList.SelectionUpdate;
begin
  Selection.Update;
end;

procedure TcxSchedulerCachedEventList.SetCheckRange(AStart, AFinish: Integer);
begin
  FCheckStart := AStart;
  FCheckFinish := AFinish;
end;

procedure TcxSchedulerCachedEventList.SetUseTimeRange(AValue: Boolean);
begin
  NeedForceUpdate := NeedForceUpdate or (AValue <> UseTimeRange);
  inherited SetUseTimeRange(AValue);
end;

function TcxSchedulerCachedEventList._AddRef: Integer;
begin
  Result := -1;
end;

function TcxSchedulerCachedEventList._Release: Integer;
begin
  Result := -1;
end;

function TcxSchedulerCachedEventList.QueryInterface(
  const IID: TGUID; out Obj): HResult;
const
  E_NOINTERFACE = HResult($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TcxSchedulerCachedEventList.AddEvent(AEvent: TcxSchedulerEvent);
begin
end;

procedure TcxSchedulerCachedEventList.RemoveEvent(AEvent: TcxSchedulerEvent);
var
  I: Integer;
  AAbsoluteItem: TcxSchedulerControlEvent;
begin
  for I := 0 to AbsoluteCount - 1 do
  begin
    AAbsoluteItem := AbsoluteItems[I];
    if AAbsoluteItem.Source = AEvent then
    begin
      AAbsoluteItem.SetSource(nil);
      AAbsoluteItem.IsDataValid := True;
      Break;
    end;
  end;
end;

procedure TcxSchedulerCachedEventList.StorageChanged(Sender: TObject);
begin
  FNeedForceUpdate := True;
end;

procedure TcxSchedulerCachedEventList.StorageRemoved(Sender: TObject);
begin
end;

function TcxSchedulerCachedEventList.GetAbsoluteCount: Integer;
begin
  Result := FAbsoluteItems.Count;
end;

function TcxSchedulerCachedEventList.GetAbsoluteItem(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent(FAbsoluteItems.List[AIndex])
end;

function TcxSchedulerCachedEventList.GetAbsoluteCountInternal: Integer;
begin
  Result := Count + FClones.Count;
end;

function TcxSchedulerCachedEventList.GetAbsoluteItemInternal(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  if AIndex >= Count then
    Result := TcxSchedulerControlEvent(FClones.Items[AIndex - Count])
  else
    Result := Items[AIndex];
end;

function TcxSchedulerCachedEventList.GetItem(
  AIndex: Integer): TcxSchedulerControlEvent;
begin
  Result := TcxSchedulerControlEvent(FItems.List[AIndex])
end;

function TcxSchedulerCachedEventList.GetShowEventsWithoutResource: Boolean;
begin
  Result := FShowEventsWithoutResource or Storage.Resources.IsNullResourceVisible;
end;

procedure TcxSchedulerCachedEventList.SetSelFinish(const AValue: TDateTime);
begin
  FSelFinish := AValue;
  if FExpandedTimeRange then
    FSelFinish := Max(FSelFinish, FMaxNecessaryDate);
end;

procedure TcxSchedulerCachedEventList.SetSelStart(const AValue: TDateTime);
begin
  FSelStart := AValue;
  if FExpandedTimeRange then
    FSelStart := Min(FSelStart, FMinNecessaryDate);
end;

procedure TcxSchedulerCachedEventList.SetShowEventsWithoutResource(const AValue: Boolean);
begin
  FNeedForceUpdate := FNeedForceUpdate or (AValue <> FShowEventsWithoutResource);
  FShowEventsWithoutResource := AValue;
end;

{ TcxSchedulerOccurrenceCalculator }

constructor TcxSchedulerOccurrenceCalculator.Create(AEvent: TcxSchedulerEvent;
  const AStart, AFinish: TDateTime; ACalcForReminders: Boolean = False);
begin
  inherited Create(AEvent, AStart, AFinish);
  FCalcForReminders := ACalcForReminders;
end;

procedure TcxSchedulerOccurrenceCalculator.CalcNearestOccurrenceIntervals(
  AStart, AFinish: TDateTime; var AnIntervalBefore, AnIntervalAfter: TDateTime);

  function OccurrenceExists: Boolean;
  var
    AOccurrence: TcxSchedulerEvent;
  begin
    Result := not (Event.GetOccurrenceByIndex(FIndex, AOccurrence) and
      (AOccurrence.EventType in [etException, etCustom]));
  end;

begin
  InitTimes;
  AnIntervalBefore := MaxDateTime;
  while GetNextOccurrence and (FOccurrenceFinish < AStart) do
    if IsValidOccurrence and OccurrenceExists then
      AnIntervalBefore := AStart - FOccurrenceStart;
  AnIntervalAfter := MaxDateTime;
  if IsValidOccurrence then
  begin
    InitTimes;
    while GetNextOccurrence and (FOccurrenceStart <= AFinish) do;
    if IsValidOccurrence and (FOccurrenceStart > AFinish) then
    begin
      if OccurrenceExists then
        AnIntervalAfter := FOccurrenceStart - AFinish
      else
      begin
        while GetNextOccurrence and not OccurrenceExists do;
        if IsValidOccurrence then
          AnIntervalAfter := FOccurrenceStart - AFinish;
      end;
    end;
  end;
end;

function TcxSchedulerOccurrenceCalculator.ValidateTimeBoundsForOccurrence(AEvent: TcxSchedulerEvent;
  AOldStart, AOldFinish, ANewStart, ANewFinish: TDateTime): TcxOccurrenceDateStatus;
var
  AMinDate, AMaxDate: TDateTime;
  AOccurrence: TcxSchedulerEvent;
  AOccurrenceIndex: Integer;
  AAllDayEvent: Boolean;

  function CheckOccurrence(var AStatus: TcxOccurrenceDateStatus;
    AOccurrenceStart, AOccurrenceFinish: TDateTime; AAllDayEvent: Boolean): Boolean;
  begin
    Result := False;
    AOccurrenceFinish := AOccurrenceFinish - MSecsToTime;
    if dxDateOf(AOccurrenceStart) = dxDateOf(ANewStart) then
    begin
      AStatus := odsOccupied;
      Result := True;
    end
    else
      if AOccurrenceFinish < AOldStart then
        AMinDate := Max(AMinDate, AOccurrenceFinish)
      else
        if AOldFinish < AOccurrenceStart  then
        begin
          AMaxDate := Min(AMaxDate, AOccurrenceStart);
          Result := True;
        end;
  end;

begin
  Result := odsValid;
  AOldFinish := AOldFinish - MSecsToTime;
  ANewFinish := ANewFinish - MSecsToTime;
  AMinDate   := 0;
  AMaxDate   := MaxDateTime;
  AOccurrenceIndex := AEvent.RecurrenceIndex;
  AOccurrence := AEvent.Pattern.Link;
  while AOccurrence <> nil do
  begin
    if (AOccurrence.EventType = etCustom) and (AOccurrence.RecurrenceIndex <> AOccurrenceIndex) then
    begin
      CheckOccurrence(Result, AOccurrence.Start, AOccurrence.Finish, AOccurrence.AllDayEvent);
      if Result = odsOccupied then
        Exit;
    end;
    AOccurrence := AOccurrence.Link;
  end;
  InitTimes;
  AAllDayEvent := AEvent.Pattern.AllDayEvent;
  while GetNextOccurrence do
  begin
    if (Index <> AOccurrenceIndex) and not AEvent.Pattern.GetOccurrenceByIndex(Index, AOccurrence) then
    begin
      if CheckOccurrence(Result, OccurrenceStart, OccurrenceFinish, AAllDayEvent) then
      begin
        if Result = odsOccupied then
          Exit
        else
          Break;
      end;
    end;
  end;
  if not ((ANewStart > AMinDate) and (ANewFinish < AMaxDate)) then
    Result := odsIntersectRange;
end;

function TcxSchedulerOccurrenceCalculator.GetOccurrenceCount(
  AEndDate: TDateTime): Integer;
begin
  Result := 0;
  InitTimes;
  while GetNextOccurrence and (dxDateOf(FOccurrenceStart) <= dxDateOf(AEndDate)) do
    Inc(Result);
end;

function TcxSchedulerOccurrenceCalculator.GetEvent: TcxSchedulerEvent;
begin
  Result := TcxSchedulerEvent(Owner);
end;

function TcxSchedulerOccurrenceCalculator.GetReminderStart: TDateTime;
begin
  Result := OccurrenceStart - FReminderAdvanceTime;
end;

function TcxSchedulerOccurrenceCalculator.GetCalcForReminders: Boolean;
begin
  Result := FCalcForReminders;
end;

function TcxSchedulerOccurrenceCalculator.GetDuration: TDateTime;
begin
  Result := Event.Duration;
end;

function TcxSchedulerOccurrenceCalculator.GetRecurrenceInfo: TcxSchedulerCustomRecurrenceInfo;
begin
  Result := Event.RecurrenceInfo;
end;

function TcxSchedulerOccurrenceCalculator.GetStart: TDateTime;
begin
  Result := Event.Start;
end;

function TcxSchedulerOccurrenceCalculator.GetTimeBias: Double;
begin
  Result := Event.TimeBias;
end;

procedure TcxSchedulerOccurrenceCalculator.InitTimes;
begin
  inherited;
  if (Event.EventType <> etPattern) and (Event.Pattern <> nil) then
  begin
    FOccurrenceStart := Event.Pattern.Start - Event.RecurrenceInfo.DisplayTimeBias + Event.TimeBias;
    FOccurrenceFinish := FOccurrenceStart + Event.Pattern.Duration;
  end;
  FActualStart := FOccurrenceStart;
  FIndex := -1;
  FReminderAdvanceTime := Event.ReminderMinutesBeforeStart * MinuteToTime;
end;

{ TcxSchedulerContentNavigationInfo }

constructor TcxSchedulerContentNavigationInfo.Create(AResourceID: Variant);
begin
  FResourceID := AResourceID;
  FIntervalBefore := MaxDateTime;
  FIntervalAfter := MaxDateTime;
end;

{ TcxSchedulerContentNavigationCalculator }

class procedure TcxSchedulerContentNavigationCalculator.FindNavigationIntervals(
  AStorage: TcxCustomSchedulerStorage;
  AContentNavigationInfo: TObjectList; AStart, AFinish: TDateTime;
  AWithoutResources: Boolean; AShowEventsWithoutResource: Boolean; AScaleUnit: TDateTime);

  procedure ProcessEventsWithoutResource(AnEventIndex: Integer);
  begin
    if AShowEventsWithoutResource and
      not IsEventSharedWithAnyResource(AStorage, AnEventIndex, False) or
      (AStorage.ResourceCount = 0) then
      CalcIntervals(AStorage.FEventsList[AnEventIndex],
        TcxSchedulerContentNavigationInfo(AContentNavigationInfo[AContentNavigationInfo.Count - 1]),
        AStart, AFinish, AScaleUnit);
  end;

  procedure WithResources;
  var
    AnEventIndex, AResourceIndex, AResourceInfoCount: Integer;
  begin
    for AnEventIndex := 0 to AStorage.FEventsList.Count - 1 do
    begin
      if AStorage.DoFilterEvent(AStorage.FEventsList[AnEventIndex]) then
      begin
        AResourceIndex := 0;
        AResourceInfoCount := AContentNavigationInfo.Count - IfThen(AShowEventsWithoutResource, 1);
        while AResourceIndex < AResourceInfoCount do
        begin
          if AStorage.FEventsList[AnEventIndex].IsSharedWithResource(
            TcxSchedulerContentNavigationInfo(AContentNavigationInfo[AResourceIndex]).FResourceID) then
            CalcIntervals(AStorage.FEventsList[AnEventIndex],
              TcxSchedulerContentNavigationInfo(AContentNavigationInfo[AResourceIndex]),
              AStart, AFinish, AScaleUnit);
          Inc(AResourceIndex);
        end;
        ProcessEventsWithoutResource(AnEventIndex);
      end;
    end;
  end;

  procedure WithoutResources;
  var
    AnEventIndex: Integer;
  begin
    for AnEventIndex := 0 to AStorage.FEventsList.Count - 1 do
    begin
      if AStorage.DoFilterEvent(AStorage.FEventsList[AnEventIndex]) then
      begin
        if IsEventSharedWithAnyResource(AStorage, AnEventIndex, True) then
          CalcIntervals(AStorage.FEventsList[AnEventIndex],
            TcxSchedulerContentNavigationInfo(AContentNavigationInfo[0]),
            AStart, AFinish, AScaleUnit);
        ProcessEventsWithoutResource(AnEventIndex);
      end;
    end;
  end;

begin
  if (AStorage = nil) or (AContentNavigationInfo = nil) then
    Exit;
  AShowEventsWithoutResource := AShowEventsWithoutResource or AStorage.Resources.IsNullResourceVisible;
  if AShowEventsWithoutResource or
    (AContentNavigationInfo.Count = 0) and (AStorage.ResourceCount = 0) then
    AContentNavigationInfo.Add(TcxSchedulerContentNavigationInfo.Create(0));
  if not AWithoutResources then
    WithResources
  else
    WithoutResources;
  if AShowEventsWithoutResource then
  begin
    ChangeResourceNavigationIntervals(AContentNavigationInfo);
    AContentNavigationInfo.Delete(AContentNavigationInfo.Count - 1);
  end;
end;

class procedure TcxSchedulerContentNavigationCalculator.CalcAppointmentIntervals(
  AnEvent: TcxSchedulerEvent; AInfo: TcxSchedulerContentNavigationInfo;
  AStart, AFinish, AScaleUnit: TDateTime);
var
  ANewInterval: TDateTime;
begin
  if AnEvent.Start < AStart then
  begin
    ANewInterval := AStart - AnEvent.Start;
    if AnEvent.Start = AnEvent.Finish then
      ANewInterval := ANewInterval + AScaleUnit;
  end
  else
    ANewInterval := MaxDateTime;
  AInfo.FIntervalBefore := Min(AInfo.FIntervalBefore, ANewInterval);

  if AFinish < AnEvent.Start then
  begin
    ANewInterval := AnEvent.Start - AFinish;
    if AnEvent.Start = AnEvent.Finish then
      ANewInterval := ANewInterval - AScaleUnit;
  end
  else
    ANewInterval := MaxDateTime;
  AInfo.FIntervalAfter := Min(AInfo.FIntervalAfter, ANewInterval);
end;

class procedure TcxSchedulerContentNavigationCalculator.CalcIntervals(
  AnEvent: TcxSchedulerEvent; AInfo: TcxSchedulerContentNavigationInfo;
  AStart, AFinish, AScaleUnit: TDateTime);
begin
  case AnEvent.EventType of
    etNone: CalcAppointmentIntervals(AnEvent, AInfo, AStart, AFinish, AScaleUnit);
    etOccurrence, etCustom:
      if AnEvent.Pattern <> nil then
        CalcAppointmentIntervals(AnEvent, AInfo, AStart, AFinish, AScaleUnit);
    etPattern:  CalcPatternIntervals(AnEvent, AInfo, AStart, AFinish);
  end;
end;

class procedure TcxSchedulerContentNavigationCalculator.CalcPatternIntervals(
  AnEvent: TcxSchedulerEvent; AInfo: TcxSchedulerContentNavigationInfo;
  AStart, AFinish: TDateTime);
var
  AOccurrenceCalculator: TcxSchedulerOccurrenceCalculator;
  ANewIntervalAfter, ANewIntervalBefore: TDateTime;
begin
  AOccurrenceCalculator :=
    TcxSchedulerOccurrenceCalculator.Create(AnEvent, dxDateOf(MinDateTime), cxMaxDate);
  try
    AOccurrenceCalculator.CalcNearestOccurrenceIntervals(AStart, AFinish,
      ANewIntervalBefore, ANewIntervalAfter);
  finally
    AOccurrenceCalculator.Free;
  end;
  AInfo.FIntervalBefore := Min(AInfo.FIntervalBefore, ANewIntervalBefore);
  AInfo.FIntervalAfter := Min(AInfo.FIntervalAfter, ANewIntervalAfter);
end;

class procedure TcxSchedulerContentNavigationCalculator.ChangeResourceNavigationIntervals(
  AContentNavigationInfo: TObjectList);
var
  I: Integer;
  ABefore, AnAfter: TDateTime;
begin
  ABefore := TcxSchedulerContentNavigationInfo(
    AContentNavigationInfo[AContentNavigationInfo.Count - 1]).FIntervalBefore;
  AnAfter := TcxSchedulerContentNavigationInfo(
    AContentNavigationInfo[AContentNavigationInfo.Count - 1]).FIntervalAfter;
  for I := 0 to AContentNavigationInfo.Count - 2 do
  begin
    TcxSchedulerContentNavigationInfo(AContentNavigationInfo[I]).FIntervalBefore :=
      Min(TcxSchedulerContentNavigationInfo(AContentNavigationInfo[I]).FIntervalBefore,
      ABefore);
    TcxSchedulerContentNavigationInfo(AContentNavigationInfo[I]).FIntervalAfter :=
      Min(TcxSchedulerContentNavigationInfo(AContentNavigationInfo[I]).FIntervalAfter,
      AnAfter);
  end;
end;

class function TcxSchedulerContentNavigationCalculator.IsEventSharedWithAnyResource(
  AStorage: TcxCustomSchedulerStorage; AnEventIndex: Integer;
  ConsiderVisibility: Boolean): Boolean;
var
  AResourceIndex: Integer;
  AnEvent: TcxSchedulerEvent;
begin
  AnEvent := AStorage.FEventsList[AnEventIndex];
  AResourceIndex := 0;
  Result := False;
  while not Result and (AResourceIndex < AStorage.Resources.Items.Count) do
  begin
    Result := AnEvent.IsSharedWithResource(AStorage.ResourceIDs[AResourceIndex]) and
      (not ConsiderVisibility or AStorage.Resources.Items[AResourceIndex].Visible);
    Inc(AResourceIndex);
  end;
end;

{ TcxSchedulerCustomReminderForm }

constructor TcxSchedulerCustomReminderForm.Create(
  AReminders: TcxSchedulerReminders);
begin
  inherited Create(nil);
  FReminders := AReminders;
end;

destructor TcxSchedulerCustomReminderForm.Destroy;
begin
  Reminders.FReminderWindow := nil;
  inherited Destroy;
end;

procedure TcxSchedulerCustomReminderForm.CheckFormPosition;
var
  X, Y: Integer;
  ACenterForm: TCustomForm;
begin
  if Position = poScreenCenter then
  begin
    X := (Screen.Width - Width) div 2;
    Y := (Screen.Height - Height) div 2;
    if X < Screen.DesktopLeft then
      X := Screen.DesktopLeft;
    if Y < Screen.DesktopTop then
      Y := Screen.DesktopTop;
    SetBounds(X, Y, Width, Height);
  end
  else if Position in [poMainFormCenter, poOwnerFormCenter] then
  begin
    ACenterForm := Application.MainForm;
    if (Position = poOwnerFormCenter) and (Owner is TCustomForm) then
      ACenterForm := TCustomForm(Owner);
    if Assigned(ACenterForm) then
    begin
      X := ((ACenterForm.Width - Width) div 2) + ACenterForm.Left;
      Y := ((ACenterForm.Height - Height) div 2) + ACenterForm.Top;
    end
    else
    begin
      X := (Screen.Width - Width) div 2;
      Y := (Screen.Height - Height) div 2;
    end;
    if X < 0 then X := 0;
    if Y < 0 then Y := 0;
    SetBounds(X, Y, Width, Height);
  end
  else if Position = poDesktopCenter then
  begin
    X := ((Screen.DesktopWidth div 2) + Screen.DesktopLeft - (Width div 2));
    Y := ((Screen.DesktopHeight div 2) + Screen.DesktopTop - (Height div 2));
    if X < Screen.DesktopLeft then X := Screen.DesktopLeft;
    if Y < Screen.DesktopTop then Y := Screen.DesktopTop;
    SetBounds(X, Y, Width, Height);
  end;
end;

procedure TcxSchedulerCustomReminderForm.CreateParams(
  var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  Params.ExStyle := Params.ExStyle or WS_EX_APPWINDOW;
end;

procedure TcxSchedulerCustomReminderForm.DoClose(var Action: TCloseAction);
begin
  inherited DoClose(Action);
  if Action = caHide then Action := caFree;
  Reminders.DoHideReminderWindow;
  cxDialogsMetricsStore.StoreMetrics(Self);
end;

procedure TcxSchedulerCustomReminderForm.DoShow;
begin
  SetControlLookAndFeel(Self, Reminders.ReminderWindowLookAndFeel);
  if UseSchedulerColorInDialogs then
    Color := GetFormColor;
  UpdateReminderList;
  inherited DoShow;
  Reminders.DoShowReminderWindow;
end;

procedure TcxSchedulerCustomReminderForm.FlashCaption;
var
  AFlashInfo: TFlashWInfo;
begin
  if Assigned(FlashWindowExProc) then
  begin
    AFlashInfo.cbSize := SizeOf(TFlashWInfo);
    AFlashInfo.hwnd := Handle;
    AFlashInfo.uCount := 1;
    AFlashInfo.dwFlags := FLASHW_ALL;
    AFlashInfo.dwTimeout := 1000;
    FlashWindowExProc(AFlashInfo);
  end
  else
    FlashWindow(Handle, True);
end;

function TcxSchedulerCustomReminderForm.GetFormColor: TColor;
begin
  Result := Reminders.ReminderWindowLookAndFeel.Painter.DefaultSchedulerControlColor;
end;

function TcxSchedulerCustomReminderForm.IsLocked: Boolean;
begin
  Result := (csDestroying in ComponentState) or
    (Reminders.Storage.ComponentState * [csLoading, csDestroying] <> [])
    or Reminders.NeedRestoreSelection or Reminders.IsLocked;
end;

procedure TcxSchedulerCustomReminderForm.LayoutChanged;
begin
end;

procedure TcxSchedulerCustomReminderForm.OpenEvent(
  AEvent: TcxSchedulerControlEvent);
begin
  Reminders.DoOpenEvent(AEvent);
end;

function TcxSchedulerCustomReminderForm.OpenEventSupported: Boolean;
begin
  Result := Assigned(Reminders.FOnOpenEvent);
end;

procedure TcxSchedulerCustomReminderForm.RestoreSelection;
begin
end;

procedure TcxSchedulerCustomReminderForm.SaveSelection;
begin
end;

procedure TcxSchedulerCustomReminderForm.ShowInactive;
begin
  if not Visible then
  begin
    cxDialogsMetricsStore.InitDialog(Self);
    CheckFormPosition;
    if Application.Active then
    begin
      ShowWindow(Handle, SW_SHOWNA);
      Visible := True;
    end
    else
      Show;
  end
  else
    ShowWindow(Handle, SW_SHOWNA);
  FlashCaption;
end;

procedure TcxSchedulerCustomReminderForm.UpdateReminderList;
begin
end;

procedure TcxSchedulerCustomReminderForm.UpdateSelectionCaptions;
begin
  Caption := Reminders.GetReminderWindowCaption;
end;

function TcxSchedulerCustomReminderForm.GetStorage: TcxCustomSchedulerStorage;
begin
  Result := Reminders.Storage;
end;

{ TcxSchedulerReminderEventID }

constructor TcxSchedulerReminderEventID.Create(AEvent: TcxSchedulerControlEvent);
var
  I: Integer;
begin
  SetLength(Values, AEvent.ValueCount);
  for I := Low(Values) to High(Values) do
  begin
    if AEvent.Storage.Fields[I].IsUnique then
      Values[I] := AEvent.Values[I]
    else
      Values[I] := Null;
  end;
  ID := AEvent.ID;
  ParentID := AEvent.ParentID;
  RecurrenceIndex := AEvent.RecurrenceIndex;
  PatternID := cxGetEventID(AEvent.Pattern);
end;

destructor TcxSchedulerReminderEventID.Destroy;
begin
  Values := nil;
  inherited Destroy;
end;

function TcxSchedulerReminderEventID.SameEvent(
  AEvent: TcxSchedulerControlEvent): Boolean;
var
  I: Integer;
begin
  Result := inherited SameEvent(AEvent);
  if Result then
    for I := Low(Values) to High(Values) do
      if AEvent.Storage.Fields[I].IsUnique and not VarEquals(AEvent.Values[I], Values[I]) then
      begin
        Result := False;
        Break;
      end;
end;

{ TcxSchedulerEventReminders }

constructor TcxSchedulerEventReminders.Create(AEvent: TcxSchedulerControlEvent);
begin
  inherited Create;
  EventID := TcxSchedulerReminderEventID.Create(AEvent);
end;

destructor TcxSchedulerEventReminders.Destroy;
begin
  FreeReminders;
  EventID.Free;
  inherited Destroy;
end;

procedure TcxSchedulerEventReminders.AddReminder(AReminder: TcxSchedulerReminder);
begin
  SetLength(Reminders, Length(Reminders) + 1);
  Reminders[High(Reminders)] := AReminder;
  AReminder.FEventReminders := Self;
end;

function TcxSchedulerEventReminders.Find(AReminderDate: TDateTime;
  const AResourceID: Variant): TcxSchedulerReminder;
var
  I: Integer;
  AReminder: TcxSchedulerReminder;
begin
  Result := nil;
  for I := Low(Reminders) to High(Reminders) do
  begin
    AReminder := Reminders[I];
    if (AReminder.ReminderDate = AReminderDate) and VarEquals(AReminder.ResourceID, AResourceID) then
    begin
      Result := AReminder;
      Break;
    end;
  end;
end;

procedure TcxSchedulerEventReminders.FreeReminderByIndex(AIndex: Integer);
var
  I: Integer;
begin
  Reminders[AIndex].Free;
  for I := AIndex + 1 to High(Reminders) do
    Reminders[I - 1] := Reminders[I];
  SetLength(Reminders, Length(Reminders) - 1);
end;

procedure TcxSchedulerEventReminders.FreeReminders;
var
  I: Integer;
begin
  for I := Low(Reminders) to High(Reminders) do
    Reminders[I].Free;
  Reminders := nil;
end;

procedure TcxSchedulerEventReminders.Invalidate;
var
  I: Integer;
begin
  FInvalid := True;
  for I := Low(Reminders) to High(Reminders) do
    Reminders[I].Invalid := True;
end;

function TcxSchedulerEventReminders.IsValid: Boolean;
begin
  Result := not FInvalid and (Length(Reminders) > 0);
end;

procedure TcxSchedulerEventReminders.RemoveInvalidReminders;
var
  I: Integer;
begin
  for I := High(Reminders) downto Low(Reminders) do
    if Reminders[I].Invalid then
      FreeReminderByIndex(I);
end;

procedure TcxSchedulerEventReminders.Validate;
begin
  RemoveInvalidReminders;
  FInvalid := False;
end;

{ TcxSchedulerEventRemindersList }

function TcxSchedulerEventRemindersList.CreateEventReminders(
  AEvent: TcxSchedulerControlEvent): TcxSchedulerEventReminders;
begin
  Result := TcxSchedulerEventReminders.Create(AEvent);
  Add(Result);
end;

function TcxSchedulerEventRemindersList.FindForEvent(
  AEvent: TcxSchedulerControlEvent): TcxSchedulerEventReminders;
var
  I: Integer;
begin
  Result := nil;
  for I := 0 to Count - 1 do
    if Items[I].EventID.SameEvent(AEvent) then
    begin
      Result := Items[I];
      Break;
    end;
end;

procedure TcxSchedulerEventRemindersList.Invalidate;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    Items[I].Invalidate;
end;

procedure TcxSchedulerEventRemindersList.Remove(AEventReminders: TcxSchedulerEventReminders);
begin
  inherited Remove(AEventReminders);
  AEventReminders.Free;
end;

procedure TcxSchedulerEventRemindersList.RemoveInvalidItems;
var
  I: Integer;
begin
  for I := Count - 1 downto 0 do
    if not Items[I].IsValid then
      Remove(Items[I]);
end;

function TcxSchedulerEventRemindersList.GetItem(AIndex: Integer): TcxSchedulerEventReminders;
begin
  Result := TcxSchedulerEventReminders(inherited Items[AIndex]);
end;

{ TcxSchedulerReminder }

constructor TcxSchedulerReminder.Create(
  AOwner: TcxSchedulerReminders;
  AEvent: TcxSchedulerControlEvent);
begin
  inherited Create;
  FOwner := AOwner;
  FEvent := AEvent;
end;

destructor TcxSchedulerReminder.Destroy;
begin
  with Owner.FItems do if not Clearing then Remove(Self);
  inherited Destroy;
end;

procedure TcxSchedulerReminder.Dismiss;
begin
  DoDismiss;
end;

procedure TcxSchedulerReminder.Snooze(ASnoozeTime: TDateTime);
begin
  DoSnooze(ASnoozeTime);
end;

procedure TcxSchedulerReminder.ClearReminder;
begin
  if Event.EventType in [etCustom, etNone] then
    Event.Source.Reminder := False
  else
    Event.Pattern.RecurrenceInfo.DismissDate := GetRecurrenceDismissDate;
  Event.Reminder := False;
end;

procedure TcxSchedulerReminder.DismissEvent;
begin
  if Owner.IsReminderByResourceAvailable and Event.Shared then
    DismissEventForResource
  else
    ClearReminder;
end;

procedure TcxSchedulerReminder.DismissEventForResource;
var
  AData: TcxSchedulerReminderResourcesData;
  AReminderResources, ANewReminderResources: TcxSchedulerReminderResources;
  I, J, AIndex: Integer;
begin
  AData := GetResourcesData;
  AReminderResources := AData.Resources;
  for I := Low(AReminderResources) to High(AReminderResources) do
    if VarEqualsSoftEx(AReminderResources[I].ResourceID, ResourceID) then
    begin
      if Event.EventType in [etCustom, etNone, etPattern] then
      begin
        if Length(AReminderResources) > 1 then
        begin
          SetLength(ANewReminderResources, Length(AReminderResources) - 1);
          AIndex := 0;
          for J := Low(AReminderResources) to High(AReminderResources) do
            if I <> J then
            begin
              ANewReminderResources[AIndex] := AReminderResources[J];
              Inc(AIndex);
            end;
          AData.Resources := ANewReminderResources;
          Event.Source.ReminderResourcesData := AData;
          Event.Reminder := False;
        end
        else
          ClearReminder;
      end
      else
      begin
        AData.Resources[I].DismissDate := GetRecurrenceDismissDate;
        Event.Pattern.ReminderResourcesData := AData;
        Event.Reminder := False;
      end;
      Break;
    end;
end;

procedure TcxSchedulerReminder.DoDismiss;
begin
  if not Owner.DoDismissReminder(Self) then
    DismissEvent;
end;

procedure TcxSchedulerReminder.DoSnooze(var ASnoozeTime: TDateTime);
begin
  if not Owner.DoSnoozeReminder(Self, ASnoozeTime) then
    SnoozeEvent(ASnoozeTime);
end;

function TcxSchedulerReminder.GetRecurrenceDismissDate: TDateTime;
begin
  Result := dxDateOf(Event.Start);
end;

procedure TcxSchedulerReminder.SetData(const AResourceID: Variant; AReminderDate: TDateTime);
begin
  FReminderDate := AReminderDate;
  FResourceID := AResourceID;
end;

procedure TcxSchedulerReminder.SnoozeEvent(const ASnoozeTime: TDateTime);
var
  ATime: TDateTime;
begin
  if ASnoozeTime > 0 then
    ATime := DateTimeHelper.RoundTime(Owner.GetNow + Max(ASnoozeTime, MinuteToTime))
  else
  begin
    ATime := DateTimeHelper.RoundTime(Event.Start + ASnoozeTime);
    if ATime <= Owner.GetNow then Exit;
  end;
  if Owner.IsReminderByResourceAvailable and Event.Shared then
    SnoozeEventForResource(ATime)
  else
  begin
    if Event.EventType in [etCustom, etNone] then
      Event.Source.ReminderDate := ATime
    else
    begin
      Event.Pattern.BeginEditing;
      Event.Pattern.ReminderDate := ATime;
      Event.Pattern.RecurrenceInfo.DismissDate := dxDateOf(ATime) - MinuteToTime;
      Event.Pattern.EndEditing;
      Event.Pattern.Post;
    end;
    Event.ReminderDate := ATime;
  end;
end;

procedure TcxSchedulerReminder.SnoozeEventForResource(const ATime: TDateTime);
var
  AData: TcxSchedulerReminderResourcesData;
  I: Integer;
begin
  AData := GetResourcesData;
  for I := Low(AData.Resources) to High(AData.Resources) do
    if VarEqualsSoftEx(AData.Resources[I].ResourceID, ResourceID) then
    begin
      AData.Resources[I].ReminderDate := ATime;
      AData.Resources[I].DismissDate := dxDateOf(ATime) - MinuteToTime;
      if Event.EventType in [etCustom, etNone] then
        Event.Source.ReminderResourcesData := AData
      else
        Event.Pattern.ReminderResourcesData := AData;
      Event.ReminderDate := ATime;
      Break;
    end;
end;

procedure TcxSchedulerReminder.Validate(AEvent: TcxSchedulerControlEvent);
begin
  FInvalid := False;
  FEvent := AEvent;
end;

function TcxSchedulerReminder.GetResourcesData: TcxSchedulerReminderResourcesData;
begin
  if Event.EventType = etOccurrence then
    Result := Event.Pattern.ReminderResourcesData
  else
    Result := Event.Source.ReminderResourcesData;
end;

{ TcxSchedulerReminderList }

procedure TcxSchedulerReminderList.Clear;
var
  I: Integer;
begin
  FClearing := True;
  try
    for I := 0 to Count - 1 do Items[I].Free;
    inherited Clear;
  finally
    FClearing := False;
  end;
end;

function TcxSchedulerReminderList.GetItem(
  Index: Integer): TcxSchedulerReminder;
begin
  Result := inherited Items[Index];
end;

{ TcxSchedulerReminderEventsList }

constructor TcxSchedulerReminderEventsList.Create(AOwner: TcxSchedulerReminders);
begin
  inherited Create;
  FReminders := AOwner;
end;

function TcxSchedulerReminderEventsList.CreateControlEvent(
  AEvent: TcxSchedulerEvent): TcxSchedulerControlEvent;
begin
  Result := inherited CreateControlEvent(AEvent);
  Result.FTimeBias := GetTimeBias + GetTimeBiasDaylightSavingTime(AEvent.Start);
end;

function TcxSchedulerReminderEventsList.GetTimeBias: Double;
begin
  Result := TcxSchedulerDateTimeHelper.TimeZoneBiasDelta(Reminders.TimeZone) + Storage.TimeBias
end;

function TcxSchedulerReminderEventsList.GetTimeBiasDaylightSavingTime(ATime: TDateTime): TDateTime;
begin
  Result := 0;
  if Reminders.TimeZoneDaylightSaving then
    Result := TcxSchedulerDateTimeHelper.TimeZoneDaylightBias(
      ATime, Reminders.TimeZone) * MinuteToTime;
end;

{ TcxSchedulerReminders }

constructor TcxSchedulerReminders.Create(AStorage: TcxCustomSchedulerStorage);
begin
  inherited Create(AStorage);
  FTimeZone := -1;
  FStorage := AStorage;
  FDefaultMinutesBeforeStart := 15;
  FUseReminderWindow := True;
  FReminderWindowLookAndFeel := TcxLookAndFeel.Create(nil);
  FEvents := TcxSchedulerReminderEventsList.Create(Self);
  FItems := TcxSchedulerReminderList.Create;
  FEventRemindersList := TcxSchedulerEventRemindersList.Create;
  FShowResourcesInReminderWindow := True;
  CreateTimers;
  Active := True;
  cxFormatController.AddListener(Self);
end;

destructor TcxSchedulerReminders.Destroy;
begin
  cxFormatController.RemoveListener(Self);
  FUpdateEventsTimer.Free;
  FUpdateRemindersTimer.Free;
  FreeAndNil(FReminderWindow);
  FReminderWindowLookAndFeel.Free;
  FEventRemindersList.Free;
  FItems.Free;
  FEvents.Free;
  inherited Destroy;
end;

procedure TcxSchedulerReminders.Assign(Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerReminders then
    with TcxSchedulerReminders(Source) do
    begin
      Self.Active := Active;
      Self.DefaultReminder := DefaultReminder;
      Self.DefaultMinutesBeforeStart := DefaultMinutesBeforeStart;
      Self.ReminderByResource := ReminderByResource;
      Self.ReminderWindowLookAndFeel := ReminderWindowLookAndFeel;
      Self.ShowResourcesInReminderWindow := ShowResourcesInReminderWindow;
      Self.UseReminderWindow := UseReminderWindow;
    end;
end;

procedure TcxSchedulerReminders.BeginUpdate;
begin
  Inc(FLockCount);
  if FLockCount = 1 then
    Storage.BeginUpdate;
end;

procedure TcxSchedulerReminders.EndUpdate;
begin
  Dec(FLockCount);
  if FLockCount = 0 then
    Storage.EndUpdate;
end;

procedure TcxSchedulerReminders.DismissAll;
var
  I: Integer;
begin
  if not HasReminders then Exit;
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      Items[I].Dismiss;
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerReminders.SnoozeAll(ASnoozeTime: TDateTime);
var
  I: Integer;
begin
  if not HasReminders then Exit;
  BeginUpdate;
  try
    for I := Count - 1 downto 0 do
      Items[I].Snooze(ASnoozeTime);
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerReminders.GetEventDueTimeText(
  AReminder: TcxSchedulerReminder; ATime: TDateTime): string;
var
  ADueTimeInfo: TcxSchedulerReminderDueTimeInfo;
  AMinutes: Integer;
begin
  AMinutes := Trunc((AReminder.Event.Start - ATime) * MinsPerDay);
  ADueTimeInfo := cxMinutesToDueTimeInfo(AMinutes);
  if Assigned(cxDueTimeInfoToTextProc) then
    Result := cxDueTimeInfoToTextProc(ADueTimeInfo)
  else
    Result := cxDueTimeInfoToText(ADueTimeInfo);
  DoGetEventDueTimeText(AReminder, ADueTimeInfo, Result);
end;

function TcxSchedulerReminders.HasReminders: Boolean;
begin
  Result := FItems.Count > 0;
end;

procedure TcxSchedulerReminders.HideReminderWindow;
begin
  if IsReminderWindowShown then
    ReminderWindow.Close;
end;

function TcxSchedulerReminders.IsLocked: Boolean;
begin
  Result := (FLockCount > 0) or not Storage.CanUpdateReminders;
end;

function TcxSchedulerReminders.IsReminderByResourceAvailable: Boolean;
begin
  Result := ReminderByResource and Storage.IsReminderByResourceAvailable;
end;

function TcxSchedulerReminders.IsReminderValid(
  AReminder: TcxSchedulerReminder): Boolean;
begin
  Result := FItems.IndexOf(AReminder) >= 0;
end;

function TcxSchedulerReminders.IsReminderWindowShown: Boolean;
begin
  Result := (ReminderWindow <> nil) and
    not (csDestroying in ReminderWindow.ComponentState) and ReminderWindow.Showing;
end;

procedure TcxSchedulerReminders.Refresh;
begin
  if Active and PopulateEventsCache then
    RefreshReminders;
end;

procedure TcxSchedulerReminders.FormatChanged;
begin
end;

procedure TcxSchedulerReminders.TimeChanged;
begin
  Refresh;
end;

procedure TcxSchedulerReminders.AddReminder(
  AEvent: TcxSchedulerControlEvent; const AResourceID: Variant;
  AReminderDate: TDateTime; var AEventReminders: TcxSchedulerEventReminders);
var
  AReminder: TcxSchedulerReminder;
begin
  AReminder := GetReminderClass.Create(Self, AEvent);
  AReminder.SetData(AResourceID, AReminderDate);
  if not DoAlertReminder(AReminder) then
  begin
    FItems.Add(AReminder);
    AddReminderToEventReminders(AReminder, AEventReminders);
    FNeedShowWindow := UseReminderWindow;
  end
  else
    AReminder.Free;
end;

procedure TcxSchedulerReminders.AddReminderToEventReminders(
  AReminder: TcxSchedulerReminder;
  var AEventReminders: TcxSchedulerEventReminders);
begin
  if AEventReminders = nil then
    AEventReminders := FEventRemindersList.CreateEventReminders(AReminder.Event);
  AEventReminders.AddReminder(AReminder);
end;

procedure TcxSchedulerReminders.CheckAddReminder(AEvent: TcxSchedulerControlEvent;
  const AResourceID: Variant; AReminderDate: TDateTime;
  var AEventReminders: TcxSchedulerEventReminders);
begin
  if IsNeedAddToReminders(AEvent, AReminderDate) then
    AddReminder(AEvent, AResourceID, AReminderDate, AEventReminders);
end;

procedure TcxSchedulerReminders.CheckAddReminders(
  AEvent: TcxSchedulerControlEvent; AEventReminders: TcxSchedulerEventReminders);
var
  I: Integer;
  AIsOccurrence: Boolean;
  AReminderResources: TcxSchedulerReminderResources;
begin
  AReminderResources := nil;
  if IsReminderByResourceAvailable and AEvent.Shared then
  begin
    AIsOccurrence := AEvent.EventType = etOccurrence;
    AReminderResources := GetReminderResourcesFromEvent(AEvent, AIsOccurrence);
    for I := Low(AReminderResources) to High(AReminderResources) do
      with AReminderResources[I] do
      begin
        if not AIsOccurrence or (dxDateOf(GetNow) > DismissDate) then
          CheckAddReminder(AEvent, ResourceID, ReminderDate, AEventReminders);
      end;
  end
  else
    CheckAddReminder(AEvent, AEvent.ResourceID, AEvent.ReminderDate, AEventReminders);
end;

procedure TcxSchedulerReminders.CheckRemindersForEvent(AEvent: TcxSchedulerControlEvent);
var
  AEventReminders: TcxSchedulerEventReminders;
begin
  if AEvent = nil then
    Exit;
  AEventReminders := FEventRemindersList.FindForEvent(AEvent);
  if AEventReminders = nil then
    CheckAddReminders(AEvent, AEventReminders)
  else
    ValidateRemindersForEvent(AEventReminders, AEvent);
end;

function TcxSchedulerReminders.CreateReminderWindow: TcxSchedulerCustomReminderForm;
begin
  if Assigned(cxReminderFormClass) then
    Result := cxReminderFormClass.Create(Self)
  else
    Result := TcxSchedulerReminderForm.Create(Self);
end;

function TcxSchedulerReminders.DoAlertReminder(
  AReminder: TcxSchedulerReminder): Boolean;
begin
  Result := False;
  if Assigned(FOnAlertReminder) then
    FOnAlertReminder(Self, AReminder, Result);
end;

function TcxSchedulerReminders.DoDismissReminder(
  AReminder: TcxSchedulerReminder): Boolean;
begin
  Result := False;
  if Assigned(FOnDismissReminder) then
    FOnDismissReminder(Self, AReminder, Result);
end;

procedure TcxSchedulerReminders.DoGetEventDueTimeText(AReminder: TcxSchedulerReminder;
  const ADueTimeInfo: TcxSchedulerReminderDueTimeInfo; var AText: string);
begin
  if Assigned(FOnGetEventDueTimeText) then
    FOnGetEventDueTimeText(Self, AReminder, ADueTimeInfo, AText);
end;

procedure TcxSchedulerReminders.DoGetReminderWindowCaption(
  var ACaption: string);
begin
  if Assigned(FOnGetReminderWindowCaption) then
    FOnGetReminderWindowCaption(Self, ACaption);
end;

procedure TcxSchedulerReminders.DoOpenEvent(
  AEvent: TcxSchedulerControlEvent);
begin
  if Assigned(FOnOpenEvent) then
    FOnOpenEvent(Self, AEvent);
end;

function TcxSchedulerReminders.DoSnoozeReminder(
  AReminder: TcxSchedulerReminder; var ASnoozeTime: TDateTime): Boolean;
begin
  Result := False;
  if Assigned(FOnSnoozeReminder) then
    FOnSnoozeReminder(Self, AReminder, ASnoozeTime, Result);
end;

procedure TcxSchedulerReminders.DoHideReminderWindow;
begin
  if Assigned(FOnHideReminderWindow) then
    FOnHideReminderWindow(Self);
end;

procedure TcxSchedulerReminders.DoShowReminderWindow;
begin
  if Assigned(FOnShowReminderWindow) then
    FOnShowReminderWindow(Self);
end;

function TcxSchedulerReminders.GetFirstReminderTimeForEvent(
  AEvent: TcxSchedulerEvent; ANow: TDateTime): TDateTime;
var
  I: Integer;
  AReminderResources: TcxSchedulerReminderResources;
begin
  AReminderResources := nil;
  if not (IsReminderByResourceAvailable and AEvent.Shared) then
    Result := AEvent.ReminderDate
  else
  begin
    AReminderResources := AEvent.ReminderResourcesData.Resources;
    if Length(AReminderResources) = 0 then
      Result := AEvent.ReminderDate
    else
    begin
      Result := AReminderResources[Low(AReminderResources)].ReminderDate;
      for I := Low(AReminderResources) + 1 to High(AReminderResources) do
        if AReminderResources[I].ReminderDate > ANow then
          Result := Min(Result, AReminderResources[I].ReminderDate);
    end;
  end;
end;

function TcxSchedulerReminders.GetNextPopulateEventsCacheTime(
  ANow: TDateTime; AIsLocked: Boolean): TDateTime;
begin
  if not AIsLocked then
    Result := dxDateOf(ANow) + 1 {one day look around}
  else
    Result := ANow + SecondToTime; {repeat in one second}
end;

function TcxSchedulerReminders.GetNextRefreshRemindersTime(
  ANow: TDateTime; AIsLocked: Boolean): TDateTime;
var
  I: Integer;
  AReminderDate: TDateTime;
begin
  Result := ANow;
  if not AIsLocked then
  begin
    Result := Result + ReminderRefreshInterval;
    for I := 0 to Events.Count - 1 do
    begin
      AReminderDate := GetFirstReminderTimeForEvent(Events[I].Source, ANow);
      if AReminderDate > ANow then
        Result := Min(AReminderDate, Result);
    end;
  end;
  Result := Max(Result, ANow + SecondToTime);
end;

function TcxSchedulerReminders.GetNow: TDateTime;
begin
  Result := Now + DateTimeHelper.TimeZoneBiasDelta(TimeZone);
  if TimeZoneDaylightSaving then
    Result := Result + DateTimeHelper.TimeZoneDaylightBias(Result, TimeZone) * MinuteToTime;
end;

function TcxSchedulerReminders.GetReminderClass: TcxSchedulerReminderClass;
begin
  Result := TcxSchedulerReminder;
end;

function TcxSchedulerReminders.GetReminderResourcesFromEvent(
  AEvent: TcxSchedulerControlEvent; AGetFromPattern: Boolean): TcxSchedulerReminderResources;
begin
  if AGetFromPattern then
    Result := AEvent.Pattern.ReminderResourcesData.Resources
  else
    Result := AEvent.ReminderResourcesData.Resources;
end;

function TcxSchedulerReminders.GetReminderWindowCaption: string;
begin
  if Count = 1 then
    Result := cxGetResourceString(@scxrCaptionReminder)
  else
    Result := Format(cxGetResourceString(@scxrCaptionReminders), [Count]);
  DoGetReminderWindowCaption(Result);
end;

function TcxSchedulerReminders.IsNeedAddToReminders(
  AEvent: TcxSchedulerControlEvent; const AReminderDate: TDateTime): Boolean;
var
  AEventStart, ANow: TDateTime;
begin
  ANow := GetNow;
  Result := AReminderDate <= ANow;
  if Result and (AEvent.EventType = etOccurrence) then
  begin
    AEventStart := AEvent.Start;
    Result := not ((dxDateOf(ANow) = dxDateOf(AEventStart)) and
      (AEventStart - AEvent.ReminderMinutesBeforeStart * MinuteToTime > ANow));
  end;
end;

function TcxSchedulerReminders.PopulateEventsCache: Boolean;
var
  ANow, ANextTime: TDateTime;
begin
  Result := not IsLocked;
  ANow := GetNow;
  ANextTime := GetNextPopulateEventsCacheTime(ANow, not Result);
  if Result then
  begin
    if IsReminderWindowShown then
    begin
      ReminderWindow.SaveSelection;
      NeedRestoreSelection := True;
    end;
    Storage.GetReminderEvents(dxDateOf(ANextTime), Events);
  end;
  UpdateTimer(UpdateEventsTimer, ANextTime - ANow);
end;

procedure TcxSchedulerReminders.RefreshReminders;
var
  ANow, ANextTime: TDateTime;
  AIsLocked: Boolean;
begin
  ANow := GetNow;
  AIsLocked := IsLocked;
  ANextTime := GetNextRefreshRemindersTime(ANow, AIsLocked);
  if not AIsLocked then
    UpdateReminderList;
  UpdateTimer(UpdateRemindersTimer, ANextTime - ANow);
end;

procedure TcxSchedulerReminders.RemoveEvent(AEvent: TcxSchedulerEvent);
var
  I: Integer;
  AChanged: Boolean;
  AControlEvent: TcxSchedulerControlEvent;
begin
  if Storage.IsDestroying then
    Exit;
  AChanged := False;
  for I := FEvents.Count - 1 downto 0 do
  begin
    AControlEvent := Events[I];
    if AControlEvent.Source = AEvent then
    begin
      AChanged := True;
      Events.Delete(I);
      AControlEvent.Free;
    end;
  end;
  if AChanged then
    ValidateReminderList;
end;

procedure TcxSchedulerReminders.RemoveEventReminders(
  AReminder: TcxSchedulerReminder);
begin
  if AReminder.EventReminders = nil then
    AReminder.Free
  else
    FEventRemindersList.Remove(AReminder.EventReminders);
end;

procedure TcxSchedulerReminders.StopTimers;
begin
  FUpdateEventsTimer.Enabled := False;
  FUpdateRemindersTimer.Enabled := False;
end;

procedure TcxSchedulerReminders.CheckRemindersForRecurringEvent(
  AEvent: TcxSchedulerControlEvent; var AIndex: Integer);
var
  AOccurrenceEvent: TcxSchedulerControlEvent;
  ACurrentPattern: TcxSchedulerEvent;
  ANow: TDateTime;
  AFound: Boolean;
  AEventStart: TDateTime;
begin
  ANow := GetNow;
  //find a right occurrence for a current pattern
  AOccurrenceEvent := nil;
  ACurrentPattern := AEvent.Pattern;
  AFound := False;
  while AIndex >= 0 do
  begin
    AEvent := Events[AIndex];
    if not AEvent.IsRecurring or (ACurrentPattern <> AEvent.Pattern) then
    begin
      Inc(AIndex); //correct index for an outer loop
      Break;
    end;
    //check a prev occurrence for a current pattern
    if (AEvent.EventType <> etException) and (AEvent.ReminderDate < ANow) then
    begin
      if AEvent.EventType <> etOccurrence then
        CheckRemindersForEvent(AEvent)
      else
        if not AFound then
        begin
          AEventStart := AEvent.Start;
          if dxDateOf(AEventStart) > dxDateOf(ANow) then
          begin
            if AEventStart - AEvent.ReminderMinutesBeforeStart * MinuteToTime < ANow then
              AOccurrenceEvent := AEvent;
          end
          else
          begin
            if dxDateOf(AEventStart) = dxDateOf(ANow) then
              AOccurrenceEvent := AEvent
            else
              if AOccurrenceEvent = nil then
                AOccurrenceEvent := AEvent;
            AFound := True;
          end;
        end;
    end;
    Dec(AIndex);
  end;
  CheckRemindersForEvent(AOccurrenceEvent);
end;

procedure TcxSchedulerReminders.ValidateReminderList;
var
  AIndex: Integer;
  AEvent: TcxSchedulerControlEvent;
begin
  FNeedShowWindow := False;
  FNeedCloseWindow := FItems.Count > 0;
  FEventRemindersList.Invalidate;
  //need the countdown loop to ensure that the next occurrences (if exist)
  //from the current chain are already in the list (FItems)
  Inc(Storage.LockCount);
  try
    AIndex := Events.Count - 1;
    while AIndex >= 0 do
    begin
      AEvent := Events[AIndex];
      if AEvent.EventType = etOccurrence then
        CheckRemindersForRecurringEvent(AEvent, AIndex)
      else
        CheckRemindersForEvent(AEvent);
      Dec(AIndex);
    end;
    FEventRemindersList.RemoveInvalidItems;
    FNeedCloseWindow := FNeedCloseWindow and (FItems.Count = 0);
  finally
    Dec(Storage.LockCount);
    if Storage.FIsChanged then
      Storage.Changed;
  end;
end;

procedure TcxSchedulerReminders.ValidateRemindersForEvent(
  AEventReminders: TcxSchedulerEventReminders;
  AEvent: TcxSchedulerControlEvent);

  procedure ValidateResourceReminder(AEventReminders: TcxSchedulerEventReminders;
    AEvent: TcxSchedulerControlEvent; const AResourceID: Variant;
    AReminderDate: TDateTime);
  var
    AReminder: TcxSchedulerReminder;
  begin
    AReminder := AEventReminders.Find(AReminderDate, AResourceID);
    if AReminder = nil then
      CheckAddReminder(AEvent, AResourceID, AReminderDate, AEventReminders)
    else
      if IsNeedAddToReminders(AEvent, AReminderDate) then
        AReminder.Validate(AEvent);
  end;

var
  I: Integer;
  AReminderResources: TcxSchedulerReminderResources;
  AIsOccurrence: Boolean;
begin
  AReminderResources := nil; //to avoid Delphi6 warning
  if IsReminderByResourceAvailable and AEvent.Shared then
  begin
    AIsOccurrence := AEvent.EventType = etOccurrence;
    AReminderResources := GetReminderResourcesFromEvent(AEvent, AIsOccurrence);
    for I := Low(AReminderResources) to High(AReminderResources) do
      with AReminderResources[I] do
      begin
        if not AIsOccurrence or (dxDateOf(GetNow) > DismissDate) then
          ValidateResourceReminder(AEventReminders, AEvent, ResourceID, ReminderDate);
      end;
  end
  else
    ValidateResourceReminder(AEventReminders, AEvent, AEvent.ResourceID, AEvent.ReminderDate);
  AEventReminders.Validate;
end;

procedure TcxSchedulerReminders.UpdateReminderList;
var
  ARestoreSelection: Boolean;
begin
  if IsLocked or UpdatingReminderList then Exit;
  FUpdatingReminderList := True;
  try
    ValidateReminderList;
    ARestoreSelection := NeedRestoreSelection;
    NeedRestoreSelection := False;
    if IsReminderWindowShown then
    begin
      if NeedCloseWindow then
        HideReminderWindow
      else
      begin
        if NeedShowWindow and not ARestoreSelection then
        begin
          ReminderWindow.SaveSelection;
          ARestoreSelection := True;
        end;
        if ARestoreSelection then
          ReminderWindow.RestoreSelection;
      end;
      if not HasReminders then
        HideReminderWindow
      else
        if NeedShowWindow then
          ReminderWindow.FlashCaption;
    end
    else
      if NeedShowWindow then ShowReminderWindow;
  finally
    FUpdatingReminderList := False;
  end;
end;

procedure TcxSchedulerReminders.UpdateTimer(ATimer: TTimer;
  AInterval: TDateTime);
begin
  if Active then
  begin
    ATimer.Enabled := False;
    ATimer.Interval := Trunc(AInterval * MSecsPerDay);
    ATimer.Enabled := True;
  end;
end;

procedure TcxSchedulerReminders.CreateTimers;
begin
  FUpdateEventsTimer := TTimer.Create(nil);
  FUpdateEventsTimer.Enabled := False;
  FUpdateEventsTimer.OnTimer := OnUpdateEventsTimer;
  FUpdateRemindersTimer := TTimer.Create(nil);
  FUpdateRemindersTimer.Enabled := False;
  FUpdateRemindersTimer.OnTimer := OnUpdateRemindersTimer;
end;

function TcxSchedulerReminders.GetCount: Integer;
begin
  Result := FItems.Count;
end;

function TcxSchedulerReminders.GetItem(
  Index: Integer): TcxSchedulerReminder;
begin
  if (Index >= 0) and (Index < Count) then
    Result := FItems[Index]
  else
    Result := nil;
end;

function TcxSchedulerReminders.GetTimeZone: Integer;
begin
  Result := FTimeZone;
  if (Result < 0) or (Result >= DateTimeHelper.TimeZoneCount) then
    Result := -1;
end;

procedure TcxSchedulerReminders.SetActive(AValue: Boolean);
begin
  if FActive <> AValue then
  begin
    FActive := AValue;
    if AValue then
      StartReminder
    else
      StopReminder;
    Storage.SendNotification(nil);
  end;
end;

procedure TcxSchedulerReminders.SetDefaultMinutesBeforeStart(AValue: Integer);
begin
  FDefaultMinutesBeforeStart := Max(0, AValue);
end;

procedure TcxSchedulerReminders.SetReminderByResource(AValue: Boolean);
begin
  if FReminderByResource <> AValue then
  begin
    FReminderByResource := AValue;
    Refresh;
    UpdateReminderWindow;
  end;
end;

procedure TcxSchedulerReminders.SetReminderWindowLookAndFeel(
  AValue: TcxLookAndFeel);
begin
  FReminderWindowLookAndFeel.Assign(AValue);
end;

procedure TcxSchedulerReminders.SetShowResourcesInReminderWindow(AValue: Boolean);
begin
  if FShowResourcesInReminderWindow <> AValue then
  begin
    FShowResourcesInReminderWindow := AValue;
    UpdateReminderWindow;
  end;
end;

procedure TcxSchedulerReminders.SetTimeZone(AValue: Integer);
begin
  FTimeZone := Max(Min(AValue, DateTimeHelper.TimeZoneCount - 1), -1);
  Storage.FullRefresh;
end;

procedure TcxSchedulerReminders.ShowReminderWindow;
begin
  if ReminderWindow = nil then
    FReminderWindow := CreateReminderWindow;
  FReminderWindow.BiDiMode := Application.BiDiMode;
  UpdateReminderList;
  ReminderWindow.ShowInactive;
end;

procedure TcxSchedulerReminders.StartReminder;
begin
  if (csDesigning in Storage.ComponentState) then Exit;
  FUpdateEventsTimer.Enabled := True;
  FUpdateRemindersTimer.Enabled := True;
  Refresh;
end;

procedure TcxSchedulerReminders.StopReminder;
begin
  StopTimers;
  HideReminderWindow;
  FEventRemindersList.Clear;
  FItems.Clear;
  FEvents.DestroyItems;
end;

procedure TcxSchedulerReminders.UpdateReminderWindow;
begin
  if IsReminderWindowShown then
    ReminderWindow.LayoutChanged;
end;

procedure TcxSchedulerReminders.OnUpdateEventsTimer(Sender: TObject);
begin
  Refresh;
end;

procedure TcxSchedulerReminders.OnUpdateRemindersTimer(Sender: TObject);
begin
  RefreshReminders;
end;

var
  User32Lib: HMODULE;

function cxVariantToFieldValue(const AValue: Variant; ABLOBFormat: Boolean = False): Variant;
var
  S: AnsiString;
  AStream: TMemoryStream;
  AWriter: TcxWriter;
begin
  Result := AValue;
  AStream := TMemoryStream.Create;
  AWriter := TcxWriter.Create(AStream);
  try
    if ABLOBFormat or (VarIsArray(AValue) and (VarArrayHighBound(AValue, 1) > 0)) then
    begin
      AWriter.WriteInteger(varArray or varVariant);
      AWriter.WriteInteger(0);
      AWriter.WriteVariant(AValue);
      AStream.Position := SizeOf(Integer);
      AWriter.WriteInteger(AStream.Size);
      SetLength(S, AStream.Size);
      if AStream.Size > 0 then
        Move(AStream.Memory^, S[1], AStream.Size);
      Result := S;
    end
    else
      if VarIsArray(AValue) then
      begin
        Result := AValue[0];
        if VarIsArray(Result) then
          Result := cxVariantToFieldValue(Result, ABLOBFormat);
      end;
  finally
    AStream.Free;
    AWriter.Free;
  end;
end;

function cxFieldValueToVariant(const AValue: Variant): Variant;
var
  S: AnsiString;
  AStream: TMemoryStream;
  AReader: TcxReader;
begin
  Result := AValue;
  AStream := TMemoryStream.Create;
  AReader := TcxReader.Create(AStream);
  try
    if dxVarIsBlob(AValue) then
    begin
      S := dxVariantToAnsiString(AValue);
      if Length(S) > SizeOf(Integer) * 2 then
      begin
        AStream.Size := Length(S);
        Move(S[1], AStream.Memory^, AStream.Size);
        if (AReader.ReadInteger = (varArray or varVariant)) and (AReader.ReadInteger = AStream.Size) then
        begin
          Result := AReader.ReadVariant;
          Exit;
        end;
      end;
      Result := S;
    end;
  finally
    AReader.Free;
    AStream.Free;
  end;
end;

function cxReminderResourcesDataToFieldValue(const AValue: TcxSchedulerReminderResourcesData): Variant;
var
  I: Integer;
  S: AnsiString;
  AStream: TMemoryStream;
  AWriter: TcxWriter;
begin
  Result := Null;
  AStream := TMemoryStream.Create;
  AWriter := TcxWriter.Create(AStream);
  try
    if Length(AValue.Resources) = 0 then
      Exit;
    AWriter.WriteByte(AValue.Version);
    AWriter.WriteInteger(Length(AValue.Resources));
    AWriter.WriteInteger(0);
    for I := Low(AValue.Resources) to High(AValue.Resources) do
      with AValue.Resources[I] do
      begin
        AWriter.WriteDateTime(DismissDate);
        AWriter.WriteVariant(ResourceID);
        AWriter.WriteDateTime(ReminderDate);
      end;
    AStream.Position := SizeOf(Integer) + SizeOf(Byte);
    AWriter.WriteInteger(AStream.Size);
    SetLength(S, AStream.Size);
    if AStream.Size > 0 then
      Move(AStream.Memory^, S[1], AStream.Size);
    Result := S;
  finally
    AStream.Free;
    AWriter.Free;
  end;
end;

function cxFieldValueToReminderResourcesData(const AValue: Variant): TcxSchedulerReminderResourcesData;
var
  S: AnsiString;
  ACount, I: Integer;
  AStream: TMemoryStream;
  AReader: TcxReader;
begin
  Result.Version := 0;
  Result.Resources := nil;
  AStream := TMemoryStream.Create;
  AReader := TcxReader.Create(AStream);
  try
    if dxVarIsBlob(AValue) then
    begin
      S := dxVariantToAnsiString(AValue);
      if Length(S) > SizeOf(Integer) + SizeOf(Byte) then
      begin
        AStream.Clear;
        AStream.Size := Length(S);
        Move(S[1], AStream.Memory^, AStream.Size);
        Result.Version := AReader.ReadByte;
        ACount := AReader.ReadInteger;
        if (Result.Version = 3) and (ACount > 0) and (AReader.ReadInteger = AStream.Size) then
        begin
          SetLength(Result.Resources, ACount);
          for I := 0 to ACount - 1 do
            with Result.Resources[I] do
            begin
              DismissDate := AReader.ReadDateTime;
              ResourceID := AReader.ReadVariant;
              ReminderDate := AReader.ReadDateTime;
            end;
        end;
      end
    end;
  finally
    AStream.Free;
    AReader.Free;
  end;
end;

function cxGetOccurrenceDateStatusMessage(AEvent: TcxSchedulerEvent; AStatus: TcxOccurrenceDateStatus): string;
begin
  case AStatus of
    odsOccupied:
      Result := Format(cxGetResourceString(@scxTwoOccurrencesPerDay), [AEvent.Caption]);
    odsIntersectRange:
      Result := Format(cxGetResourceString(@scxCannotRescheduleOccurrence), [AEvent.Caption]);
  else
    Result := '';
  end;
end;

function cxTaskLinksToFieldValue(ALinks: TcxSchedulerEventLinks): Variant;
var
  I: Integer;
  ADataArray: Variant;
begin
  if ALinks.Count = 0 then
  begin
    Result := Null;
    Exit;
  end;
  ADataArray := VarArrayCreate([0, ALinks.Count - 1], varVariant);
  for I := 0 to ALinks.Count - 1 do
    ADataArray[I] := ALinks[I].GetData;
  Result := cxVariantToFieldValue(ADataArray, True);
end;

procedure cxFieldValueToTaskLinks(const AValue: Variant; ALinks: TcxSchedulerEventLinks);
var
  I, L, H: Integer;
  ADataValue: Variant;
begin
  if VarIsNull(AValue) and (ALinks.Count = 0) then Exit;
  ALinks.BeginUpdate;
  try
    ALinks.Clear;
    ADataValue := cxFieldValueToVariant(AValue);
    if VarIsArray(ADataValue) then
    begin
      L := VarArrayLowBound(ADataValue, 1);
      H := VarArrayHighBound(ADataValue, 1);
      if (H - L + 1 = 3) and not VarIsArray(ADataValue[0]) then
      begin
        if ALinks.Event.Storage.GetEventByID(ADataValue[1]) <> nil then
          TcxSchedulerEventItemLink(ALinks.Add).SetData(ADataValue);
      end
      else
        for I := L to H do
          if ALinks.Event.Storage.GetEventByID(ADataValue[I][1]) <> nil then
            TcxSchedulerEventItemLink(ALinks.Add).SetData(ADataValue[I]);
    end;
  finally
    ALinks.EndUpdate;
  end;
end;

function GetStreamVersion(const AStream: TStream;
  var AFieldCount: Integer; ACustomFieldCount: Integer = 0): Double;

  function TryRead(const AOriginal: AnsiString;
    var ABuffer; const ABufferSize: Integer): Boolean;
  var
    L: Integer;
    ACandidate: AnsiString;
  begin
    Result := (AStream.Size - AStream.Position) >
      (Length(AOriginal) + SizeOf(L));
    if Result then
      AStream.Read(L, SizeOf(L));
    Result := L = Length(AOriginal);
    if Result then
    begin
      SetLength(ACandidate, L);
      AStream.ReadBuffer(ACandidate[1], L);
      Result := ACandidate = AOriginal;
    end;
    if Result then
      AStream.ReadBuffer(ABuffer, ABufferSize);
  end;

var
  APos: Integer;
begin
  Result := 0;
  APos := AStream.Position;
  if not TryRead(scxSchedulerSignature, Result, SizeOf(Result)) then
  begin
    AStream.Position := APos;
    if TryRead('DataController1', AFieldCount, SizeOf(AFieldCount)) then
    begin
      case AFieldCount - ACustomFieldCount of
        cxSchedulerFieldCountVersion1:
          Result := 1;
        cxSchedulerFieldCountVersion2:
          Result := 2;
        cxSchedulerFieldCountVersion3:
          Result := 3;
      else
        Result := cxSchedulerStreamVersion;
      end;
    end;
  end
  else
  begin
    APos := AStream.Position;
    if not TryRead('DataController1', AFieldCount, SizeOf(AFieldCount)) then
      Result := -1;
  end;
  AStream.Position := APos;
end;

initialization
  //
  User32Lib := GetModuleHandle(user32);
  FlashWindowExProc := GetProcAddress(User32Lib, 'FlashWindowEx');

finalization

end.



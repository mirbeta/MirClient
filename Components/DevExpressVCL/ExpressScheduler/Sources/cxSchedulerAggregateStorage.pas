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

unit cxSchedulerAggregateStorage;

{$I cxVer.inc}

interface

uses
  Classes, cxClasses, Controls, cxSchedulerStorage, cxSchedulerHolidays,
  cxSchedulerUtils, cxCustomData, dxCoreClasses, cxDataStorage;

type

  TcxSchedulerStorageLinks = class;
  TcxSchedulerAggregateStorage = class;

  TcxSchedulerStorageLinkClass = class of TcxSchedulerStorageLink;

  { TcxSchedulerAggregateStorageEditingEventInfo }

  TcxSchedulerAggregateStorageEditingEventInfo = class(TcxSchedulerEditingEventInfo)
  protected
    procedure UpdateStorage; override;
  end;

  { TcxSchedulerAggregateStorageEditingEventInfoList }

  TcxSchedulerAggregateStorageEditingEventInfoList = class(TcxSchedulerEditingEventInfoList)
  private
    function GetStorage: TcxSchedulerAggregateStorage;
  protected
    function CreateEvent(ASource, AOriginal: TcxSchedulerEvent; AForcePatternEditing: Boolean): TcxSchedulerControlEvent; override;
    function GetEventID(AEvent: TcxSchedulerEvent): Variant; override;
    function GetInfoClass: TcxSchedulerEditingEventInfoClass; override;
    function GetItemCore(AIndex: Integer): TcxSchedulerEditingEventInfo; override;
    function GetRootEvent(AEvent: TcxSchedulerEvent): TcxSchedulerEvent; override;

    property Storage: TcxSchedulerAggregateStorage read GetStorage;
  public
    function IsEventEditing(AEvent: TcxSchedulerEvent; ARecurrenceIndex: Integer;
      AEventType: TcxEventType): Boolean; override;
  end;

  { TcxSchedulerStorageLink }

  TcxSchedulerStorageLink = class(TCollectionItem)
  private
    FIsDestroying: Boolean;
    FStorage: TcxCustomSchedulerStorage;
    function GetAggregateStorage: TcxSchedulerAggregateStorage;
    function GetDefault: Boolean;
    function GetLinks: TcxSchedulerStorageLinks;
    procedure SetDefault(AValue: Boolean);
    procedure SetStorage(AValue: TcxCustomSchedulerStorage);
  protected
    property IsDestroying: Boolean read FIsDestroying;
  public
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;

    property AggregateStorage: TcxSchedulerAggregateStorage read GetAggregateStorage;
    property Links: TcxSchedulerStorageLinks read GetLinks;
  published
    property Default: Boolean read GetDefault write SetDefault default False;
    property Storage: TcxCustomSchedulerStorage read FStorage write SetStorage;
  end;

  { TcxSchedulerStorageLinks }

  TcxSchedulerStorageLinks = class(TCollection)
  private
    FAggregateStorage: TcxSchedulerAggregateStorage;
    FDefaultLink: TcxSchedulerStorageLink;
    function GetDefaultStorage: TcxCustomSchedulerStorage;
    function GetItem(AIndex: Integer): TcxSchedulerStorageLink;
    procedure SetDefaultLink(AValue: TcxSchedulerStorageLink);
    procedure SetItem(AIndex: Integer; AValue: TcxSchedulerStorageLink);
  protected
    function GetItemLinkClass: TcxSchedulerStorageLinkClass; virtual;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor CreateEx(AOwner: TcxSchedulerAggregateStorage); virtual;

    function AddStorageLink(AStorage: TcxCustomSchedulerStorage): TcxSchedulerStorageLink;
    function GetStorageLinkIndex(AStorage: TcxCustomSchedulerStorage): Integer;
    procedure PopulateHolidays(AList: TList);

    property AggregateStorage: TcxSchedulerAggregateStorage read FAggregateStorage;
    property DefaultLink: TcxSchedulerStorageLink read FDefaultLink write SetDefaultLink;
    property DefaultStorage: TcxCustomSchedulerStorage read GetDefaultStorage;
    property Items[AIndex: Integer]: TcxSchedulerStorageLink read GetItem write SetItem; default;
  end;

  { TcxSchedulerAggregateStorageEvent }

  TcxSchedulerAggregateStorageEvent = class(TcxSchedulerEvent)
  private
    FSourceEvent: TcxSchedulerEvent;
    function GetSourceStorage: TcxCustomSchedulerStorage;
    function GetStorage: TcxSchedulerAggregateStorage;
    procedure SetSourceEvent(AValue: TcxSchedulerEvent);
  protected
    function GetParentID: Variant; override;
    function GetValueByIndex(AIndex: Integer): Variant; override;
    function GetValueDefault(AField: TcxCustomSchedulerStorageField; const ADefValue: Variant): Variant; override;
    procedure SetValue(AField: TcxCustomSchedulerStorageField; const AValue: Variant); override;
  public
    procedure DeleteExceptions; override;
    procedure EndEditing; override;
    procedure RemoveRecurrence; override;

    property SourceEvent: TcxSchedulerEvent read FSourceEvent write SetSourceEvent;
    property SourceStorage: TcxCustomSchedulerStorage read GetSourceStorage;
    property Storage: TcxSchedulerAggregateStorage read GetStorage;
  end;

  { TcxSchedulerAggregateStorage }

  TcxSchedulerEventInsertingEvent = procedure(Sender: TcxSchedulerAggregateStorage;
    AEvent: TcxSchedulerEvent; var AStorage: TcxCustomSchedulerStorage) of object;

  TcxSchedulerAggregateStorage = class(TcxCustomSchedulerStorage, IcxSchedulerStorageListener, IcxSchedulerStorageListener2)
  private
    FIsPostEvent: Boolean;
    FLinks: TcxSchedulerStorageLinks;
    FOnEventInserting: TcxSchedulerEventInsertingEvent;
    function CanRefresh: Boolean;
    function GetCustomFields: TcxSchedulerStorageFields;
    procedure SetCustomFields(const AValue: TcxSchedulerStorageFields);
  protected
    procedure AddInternalField(var AField: TcxCustomSchedulerStorageField;
      AValueType: TcxValueTypeClass; AIsUnique: Boolean = True); override;
    procedure DoDeleteEvent(AEvent: TcxSchedulerEvent); override;
    procedure DoEventInserting(AEvent: TcxSchedulerEvent; out AStorage: TcxCustomSchedulerStorage); virtual;
    function DoFilterEvent(AEvent: TcxSchedulerEvent): Boolean; override;
    procedure DoRefresh; override;
    function GetEditingEventInfoListClass: TcxSchedulerEditingEventInfoListClass; override;
    function GetEventClass: TcxSchedulerEventClass; override;
    function GetEventBySource(AEvent: TcxSchedulerEvent): TcxSchedulerAggregateStorageEvent;
    function InternalAddEvent(AEvent: TcxSchedulerEvent): TcxSchedulerAggregateStorageEvent; virtual;
    function IsDesigning: Boolean;
    function IsDestroying: Boolean;
    procedure PopulateEventsFromStorage(AStorage: TcxCustomSchedulerStorage); virtual;
    procedure PostEditingData(AEvent: TcxSchedulerEvent); override;
    procedure PostEvent(AEvent: TcxSchedulerEvent); override;
    procedure RemoveStorageEvents(AStorage: TcxCustomSchedulerStorage);
    procedure SendNotification(AEvent: TcxSchedulerEvent;
      AIsRemoved: Boolean = False; ACheckLockCount: Boolean = True); override;
    procedure BeginUpdateDataController; override;
    procedure EndUpdateDataController; override;
    // IcxSchedulerStorageListener & IcxSchedulerStorageListener2
    procedure AddEvent(AEvent: TcxSchedulerEvent);
    procedure RemoveEvent(AEvent: TcxSchedulerEvent);
    procedure StorageChanged(Sender: TObject);
    procedure StorageRemoved(Sender: TObject);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    procedure Clear; override;
    procedure GenerateHolidayEvents(const AResourceID: Variant); override;
    function GetHolidayNamesByDate(ADate: TDate; var ANames: string;
      AOnlyVisible: Boolean = True): Boolean; override;
    function IsActive: Boolean; override;
    procedure PopulateHolidayDates(AList: TcxSchedulerDateList; AStart, AFinish: TDate;
      AOnlyVisible: Boolean = True; AClearList: Boolean = True); override;
  published
    property CustomFields: TcxSchedulerStorageFields read GetCustomFields write SetCustomFields;
    property Reminders;
    property Resources;
    property Links: TcxSchedulerStorageLinks read FLinks write FLinks;
    property OnEventInserting: TcxSchedulerEventInsertingEvent read
      FOnEventInserting write FOnEventInserting;
    property OnEventIntersect;
    property OnFilterEvent;
  end;

implementation

uses
  SysUtils, Variants, Types, cxVariants;

type
  TcxCustomSchedulerStorageAccess = class(TcxCustomSchedulerStorage);

function cxGetRootEvent(AEvent: TcxSchedulerEvent): TcxSchedulerEvent;

  function IsAggregateEvent(AEvent: TcxSchedulerEvent): Boolean;
  begin
    Result := AEvent is TcxSchedulerAggregateStorageEvent;
  end;

var
  ARootEvent: TcxSchedulerEvent;
begin
  Result := AEvent;
  ARootEvent := Result;
  while (Assigned(ARootEvent) and IsAggregateEvent(ARootEvent))  do
  begin
    ARootEvent := TcxSchedulerAggregateStorageEvent(ARootEvent).SourceEvent;
    if Assigned(ARootEvent) then
      Result := ARootEvent;
  end;
end;

function cxGetRootEventID(AEvent: TcxSchedulerEvent): Variant;
var
  ARootEvent: TcxSchedulerEvent;
begin
  ARootEvent := cxGetRootEvent(AEvent);
  Result := cxGetEventID(ARootEvent);
end;

function cxCompareEvents(AItem1, AItem2: Integer): Integer;
begin
  Result := AItem1 - AItem2;
end;

procedure cxMakeUniqueList(AList: TList);
var
  I: Integer;
begin
  AList.Sort(@cxCompareEvents);
  for I := AList.Count - 1 downto 1 do
    if AList[I] = AList[I - 1] then
      AList.Delete(I);
end;

{ TcxSchedulerAggregateStorageEditingEventInfo }

procedure TcxSchedulerAggregateStorageEditingEventInfo.UpdateStorage;
begin
  if (Event <> nil) and (Event.Source <> nil) then
    SetStorage(cxGetRootEvent(Event.Source).Storage)
  else
    inherited UpdateStorage;
end;

{ TcxSchedulerAggregateStorageEditingEventInfoList }

function TcxSchedulerAggregateStorageEditingEventInfoList.IsEventEditing(AEvent: TcxSchedulerEvent; ARecurrenceIndex: Integer;
  AEventType: TcxEventType): Boolean;
var
  I: Integer;
begin
  Result := inherited IsEventEditing(AEvent, ARecurrenceIndex, AEventType);
  if not Result then
  begin
    if AEvent is TcxSchedulerAggregateStorageEvent then
      AEvent := TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent;
    for I := 0 to Storage.Links.Count - 1 do
      if Storage.Links[I].Storage <> nil then
      begin
        Result := Storage.Links[I].Storage.EditingEventInfoList.IsEventEditing(
          AEvent, ARecurrenceIndex, AEventType);
        if Result then
          Break;
      end;
  end;
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.CreateEvent(ASource, AOriginal: TcxSchedulerEvent;
  AForcePatternEditing: Boolean): TcxSchedulerControlEvent;
begin
  Result := inherited CreateEvent(cxGetRootEvent(ASource), cxGetRootEvent(AOriginal), AForcePatternEditing);
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.GetEventID(AEvent: TcxSchedulerEvent): Variant;
begin
  Result := cxGetRootEventID(AEvent);
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.GetInfoClass: TcxSchedulerEditingEventInfoClass;
begin
  Result := TcxSchedulerAggregateStorageEditingEventInfo;
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.GetItemCore(AIndex: Integer): TcxSchedulerEditingEventInfo;
var
  I: Integer;
  ACount: Integer;
  AList: TcxSchedulerEditingEventInfoList;
begin
  I := 0;
  AList := Self;
  ACount := Count;
  while (AIndex >= ACount) do
  begin
    if I > Storage.Links.Count - 1 then
      Break;
    if Assigned(Storage.Links[I].Storage) then
    begin
      AIndex := AIndex - ACount;
      AList := Storage.Links[I].Storage.EditingEventInfoList;
      ACount := AList.Count;
    end;
    Inc(I);
  end;
  if AList = Self then
    Result := inherited GetItemCore(AIndex)
  else
    Result := AList.Items[AIndex];
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.GetRootEvent(AEvent: TcxSchedulerEvent): TcxSchedulerEvent;
begin
  Result := cxGetRootEvent(AEvent);
end;

function TcxSchedulerAggregateStorageEditingEventInfoList.GetStorage: TcxSchedulerAggregateStorage;
begin
  Result := TcxSchedulerAggregateStorage(inherited Storage);
end;

{ TcxSchedulerStorageLink }

destructor TcxSchedulerStorageLink.Destroy;
begin
  FIsDestroying := True;
  if Default then
    Links.FDefaultLink := nil;
  Storage := nil;
  inherited Destroy;
end;

procedure TcxSchedulerStorageLink.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerStorageLink then
  with Source as TcxSchedulerStorageLink do
    begin
      Self.Storage := Storage;
      Self.Default := Default;
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerStorageLink.GetAggregateStorage: TcxSchedulerAggregateStorage;
begin
  Result := Links.AggregateStorage;
end;

function TcxSchedulerStorageLink.GetDefault: Boolean;
begin
  Result := Links.DefaultLink = Self;
end;

function TcxSchedulerStorageLink.GetLinks: TcxSchedulerStorageLinks;
begin
  Result := inherited Collection as TcxSchedulerStorageLinks;
end;

procedure TcxSchedulerStorageLink.SetDefault(AValue: Boolean);
begin
  if AValue then
    Links.DefaultLink := Self;
end;

procedure TcxSchedulerStorageLink.SetStorage(AValue: TcxCustomSchedulerStorage);
begin
   if (Links.GetStorageLinkIndex(AValue) = -1) and
    (AValue <> AggregateStorage) then
  begin
    if FStorage <> nil then
      FStorage.RemoveListener(AggregateStorage);
    AggregateStorage.RemoveStorageEvents(FStorage);
    FStorage := AValue;
    if FStorage <> nil then
      FStorage.AddListener(AggregateStorage)
    else
      if Links.DefaultLink = Self then
        Links.DefaultLink := nil;
    if not IsDestroying then
    begin
      AggregateStorage.PopulateEventsFromStorage(FStorage);
      Changed(True);
    end;
  end;
end;

{ TcxSchedulerStorageLinks }

constructor TcxSchedulerStorageLinks.CreateEx(AOwner: TcxSchedulerAggregateStorage);
begin
  Create(GetItemLinkClass);
  FAggregateStorage := AOwner;
end;

function TcxSchedulerStorageLinks.AddStorageLink(
  AStorage: TcxCustomSchedulerStorage): TcxSchedulerStorageLink;
begin
  Result := nil;
  if GetStorageLinkIndex(AStorage) < 0 then
  begin
    BeginUpdate;
    try
      Result := Add as TcxSchedulerStorageLink;
      Result.Storage := AStorage;
    finally
      EndUpdate;
    end;
  end;
end;

function TcxSchedulerStorageLinks.GetStorageLinkIndex(
  AStorage: TcxCustomSchedulerStorage): Integer;
begin
  if AStorage <> nil then
    for Result := 0 to Count - 1 do
      if Items[Result].Storage = AStorage then Exit;
  Result := -1;
end;

procedure TcxSchedulerStorageLinks.PopulateHolidays(AList: TList);
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    with Items[I] do
    begin
      if (Storage <> nil) then
      if Storage is TcxSchedulerAggregateStorage then
        TcxSchedulerAggregateStorage(Storage).Links.PopulateHolidays(AList)
      else
        if Storage.Holidays <> nil then
          AList.Add(Storage.Holidays);
    end;
  cxMakeUniqueList(AList);
end;

function TcxSchedulerStorageLinks.GetItemLinkClass: TcxSchedulerStorageLinkClass;
begin
  Result := TcxSchedulerStorageLink;
end;

procedure TcxSchedulerStorageLinks.Update(Item: TCollectionItem);
var
  I: Integer;
begin
  inherited Update(Item);
  if Count > 0 then
  begin
    if (FDefaultLink = nil) or (FDefaultLink.Storage = nil) then
    begin
      for I := 0 to Count - 1 do
        if not Items[I].IsDestroying and (Items[I].Storage <> nil) then
        begin
          FDefaultLink := Items[I];
          Break;
        end;
    end;
  end;
  AggregateStorage.StorageChanged(nil);
end;

function TcxSchedulerStorageLinks.GetDefaultStorage: TcxCustomSchedulerStorage;
begin
  if FDefaultLink <> nil then
    Result := FDefaultLink.Storage
  else
    Result := nil;
end;

function TcxSchedulerStorageLinks.GetItem(AIndex:
  Integer): TcxSchedulerStorageLink;
begin
  Result := TcxSchedulerStorageLink(inherited Items[AIndex]);
end;

procedure TcxSchedulerStorageLinks.SetDefaultLink(
  AValue: TcxSchedulerStorageLink);
begin
  if ((AValue = nil) or ((AValue.Storage <> nil) and
    not AValue.IsDestroying)) and (FDefaultLink <> AValue) then
  begin
    FDefaultLink := AValue;
    Changed;
  end;
end;

procedure TcxSchedulerStorageLinks.SetItem(AIndex: Integer;
  AValue: TcxSchedulerStorageLink);
begin
  Items[AIndex].Assign(AValue);
end;

{ TcxSchedulerAggregateStorageEvent }

procedure TcxSchedulerAggregateStorageEvent.DeleteExceptions;
begin
  SourceEvent.DeleteExceptions;
end;

procedure TcxSchedulerAggregateStorageEvent.EndEditing;
begin
  inherited EndEditing;
  if FEditCount = 0 then
    Storage.Changed;
end;

procedure TcxSchedulerAggregateStorageEvent.RemoveRecurrence;
begin
  SourceEvent.RemoveRecurrence;
end;

function TcxSchedulerAggregateStorageEvent.GetParentID: Variant;
begin
  if (SourceEvent <> nil) and SourceEvent.IsRecurring then
    Result := Integer(SourceEvent.Pattern)
  else
    Result := inherited GetParentID;
end;

function TcxSchedulerAggregateStorageEvent.GetValueByIndex(AIndex: Integer): Variant;
begin
  if IsEditing then
    Result := EditValues[AIndex]
  else
    if SourceEvent <> nil then
      Result := TcxSchedulerAggregateStorageEvent(SourceEvent).GetValueByIndex(AIndex)
    else
      Result := inherited GetValueByIndex(AIndex)
end;

function TcxSchedulerAggregateStorageEvent.GetValueDefault(
  AField: TcxCustomSchedulerStorageField; const ADefValue: Variant): Variant;
begin
  Result := GetValueByIndex(AField.Index);
  if VarIsNull(Result) then
    Result := ADefValue;
end;

procedure TcxSchedulerAggregateStorageEvent.SetValue(AField: TcxCustomSchedulerStorageField;
  const AValue: Variant);
begin
  Modified;
  if IsEditing then
    EditValues[AField.Index] := AValue
  else
    if SourceEvent <> nil then
      SourceEvent.Values[AField.Index] := AValue;
end;

function TcxSchedulerAggregateStorageEvent.GetSourceStorage: TcxCustomSchedulerStorage;
begin
  Result := nil;
  if SourceEvent <> nil then
    Result := SourceEvent.Storage;
end;

function TcxSchedulerAggregateStorageEvent.GetStorage: TcxSchedulerAggregateStorage;
begin
  Result := TcxSchedulerAggregateStorage(inherited Storage);
end;

procedure TcxSchedulerAggregateStorageEvent.SetSourceEvent(AValue: TcxSchedulerEvent);
begin
  FSourceEvent := AValue;
  ID := Integer(FSourceEvent);
end;

{ TcxSchedulerAggregateStorage }

constructor TcxSchedulerAggregateStorage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLinks := TcxSchedulerStorageLinks.CreateEx(Self);
end;

destructor TcxSchedulerAggregateStorage.Destroy;
begin
  SendNotification(nil, True);
  FLinks.Free;
  inherited Destroy;
end;

procedure TcxSchedulerAggregateStorage.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerAggregateStorage then
    Links.Assign(TcxSchedulerAggregateStorage(Source).Links);
  inherited Assign(Source);
end;

procedure TcxSchedulerAggregateStorage.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    for I := 0 to Links.Count - 1 do
      if Links[I].Storage <> nil then
        Links[I].Storage.Clear;
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerAggregateStorage.GenerateHolidayEvents(const AResourceID: Variant);
var
  AHolidaysList: TList;
  I: Integer;
begin
  AHolidaysList := TList.Create;
  try
    Links.PopulateHolidays(AHolidaysList);
    for I := 0 to AHolidaysList.Count - 1 do
      GenerateHolidayEventsBySchedulerHolidays(AResourceID, TcxSchedulerHolidays(AHolidaysList[I]));
  finally
    AHolidaysList.Free;
  end;
end;

function TcxSchedulerAggregateStorage.GetHolidayNamesByDate(ADate: TDate; var ANames: string;
  AOnlyVisible: Boolean = True): Boolean;
var
  I: Integer;
  AHolidaysList: TList;
begin
  AHolidaysList := TList.Create;
  try
    Links.PopulateHolidays(AHolidaysList);
    for I := 0 to AHolidaysList.Count - 1 do
      TcxSchedulerHolidays(AHolidaysList.Items[I]).GetHolidayNamesByDate(ADate, ANames, AOnlyVisible);
  finally
    FreeAndNil(AHolidaysList);
    Result := ANames <> '';
  end;
end;

function TcxSchedulerAggregateStorage.IsActive: Boolean;
begin
  Result := (Links <> nil) and (Links.Count > 0) and (Links.DefaultStorage <> nil) and
    (Links.DefaultStorage.IsActive or Assigned(FOnEventInserting));
  if Result then
    Result := CanRefresh;
end;

procedure TcxSchedulerAggregateStorage.PopulateHolidayDates(AList: TcxSchedulerDateList;
  AStart, AFinish: TDate; AOnlyVisible: Boolean = True; AClearList: Boolean = True);

  procedure AddHolidays(AHolidays: TcxSchedulerHolidays);
  begin
    AHolidays.PopulateHolidayDates(AList, AStart, AFinish, AOnlyVisible, False);
  end;

var
  AHolidaysList: TList;
  I: Integer;
begin
  AHolidaysList := TList.Create;
  try
    Links.PopulateHolidays(AHolidaysList);
    if AClearList then
      AList.Clear;
    for I := 0 to AHolidaysList.Count - 1 do
      AddHolidays(AHolidaysList[I]);
  finally
    FreeAndNil(AHolidaysList);
  end;
end;

procedure TcxSchedulerAggregateStorage.AddInternalField(
  var AField: TcxCustomSchedulerStorageField; AValueType: TcxValueTypeClass;
  AIsUnique: Boolean = True);
begin
  inherited;
  DataController.ChangeValueTypeClass(AField.Index, TcxVariantValueType);
end;

procedure TcxSchedulerAggregateStorage.DoDeleteEvent(AEvent: TcxSchedulerEvent);
var
  ASource: TcxSchedulerEvent;
begin
  BeginUpdate;
  try
    BeforeDeleteEvent(AEvent);
    DoDestroyEvent(AEvent);
    SendNotification(AEvent, True, False);
    FDeletedEvents.Add(AEvent);
    ASource := TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent;
    TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent := nil;
    if ASource <> nil then
      ASource.Delete;
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerAggregateStorage.DoEventInserting(AEvent: TcxSchedulerEvent;
  out AStorage: TcxCustomSchedulerStorage);
begin
  if AEvent.Pattern <> nil then
    AStorage := TcxSchedulerAggregateStorageEvent(AEvent.Pattern).SourceEvent.Storage
  else
    if TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent <> nil then
      AStorage := TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent.Storage
    else
    begin
      AStorage := Links.DefaultStorage;
      if Assigned(FOnEventInserting) then
        FOnEventInserting(Self, AEvent, AStorage);
    end;
end;

function TcxSchedulerAggregateStorage.DoFilterEvent(AEvent: TcxSchedulerEvent): Boolean;
var
  AStorage: TcxCustomSchedulerStorageAccess;
begin
  if AEvent is TcxSchedulerAggregateStorageEvent then
  begin
    AStorage := TcxCustomSchedulerStorageAccess(TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent.Storage);
    Result := AStorage.DoFilterEvent(TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent);
  end
  else
    Result := True;
  if Assigned(OnFilterEvent) then
    OnFilterEvent(Self, AEvent, Result);
end;

procedure TcxSchedulerAggregateStorage.DoRefresh;
begin
  if CanRefresh then
    inherited DoRefresh;
end;

function TcxSchedulerAggregateStorage.GetEditingEventInfoListClass: TcxSchedulerEditingEventInfoListClass;
begin
  Result := TcxSchedulerAggregateStorageEditingEventInfoList;
end;

function TcxSchedulerAggregateStorage.GetEventClass:
  TcxSchedulerEventClass;
begin
  Result := TcxSchedulerAggregateStorageEvent;
end;

function TcxSchedulerAggregateStorage.GetEventBySource(
  AEvent: TcxSchedulerEvent): TcxSchedulerAggregateStorageEvent;
var
  I: Integer;
begin
  for I := EventCount - 1 downto 0 do
  begin
    Result := TcxSchedulerAggregateStorageEvent(Events[I]);
    if Result.SourceEvent = AEvent then Exit;
  end;
  Result := nil;
end;

function TcxSchedulerAggregateStorage.InternalAddEvent(
  AEvent: TcxSchedulerEvent): TcxSchedulerAggregateStorageEvent;
begin
  Result := nil;
  if FIsPostEvent then Exit;
  Result := GetEventClass.Create(Self, True) as TcxSchedulerAggregateStorageEvent;
  Result.SourceEvent := AEvent;
  Result.ID := Integer(AEvent);
  FEventsList.Add(Result);
  SendNotification(Result, False, False);
end;

function TcxSchedulerAggregateStorage.IsDesigning: Boolean;
begin
  Result := csDesigning in ComponentState;
end;

function TcxSchedulerAggregateStorage.IsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

procedure TcxSchedulerAggregateStorage.PopulateEventsFromStorage(
  AStorage: TcxCustomSchedulerStorage);
var
  I: Integer;
begin
  if not Assigned(AStorage) or (AStorage.EventCount = 0) then Exit;
  for I := 0 to AStorage.EventCount - 1 do
    InternalAddEvent(AStorage.Events[I])
end;

procedure TcxSchedulerAggregateStorage.PostEditingData(
  AEvent: TcxSchedulerEvent);
var
  ASource: TcxSchedulerEvent;
begin
  ASource := TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent;
  if (ASource <> nil) and not (ASource is TcxSchedulerAggregateStorageEvent) then
  begin
    if DoEventModified(AEvent) then Exit;
    ASource.BeginEditing;
    try
      ASource.Assign(AEvent);
      if AEvent.Pattern <> nil then
      begin
        ASource.ParentID :=
          TcxSchedulerAggregateStorageEvent(AEvent.Pattern).SourceEvent.ID;
        TcxSchedulerAggregateStorageEvent(ASource).FPattern :=
          TcxSchedulerAggregateStorageEvent(AEvent.Pattern).SourceEvent;
      end;
    finally
      ASource.EndEditing;
    end;
  end
  else
    inherited;
end;

procedure TcxSchedulerAggregateStorage.PostEvent(AEvent: TcxSchedulerEvent);
var
  ASource: TcxSchedulerEvent;
  AStorage: TcxCustomSchedulerStorage;
  APattern: TcxSchedulerEvent;
begin
  if TcxSchedulerAggregateStorageEvent(AEvent).IsNewEvent then
  begin
    FIsPostEvent := True;
    BeginUpdate;
    try
      FNewEvents.Remove(AEvent);
      if not IsDestroying then
      begin
        APattern := nil;
        if AEvent.Pattern <> nil then
          APattern := AEvent.Pattern
        else
          if AEvent.ParentID >= 0 then
            APattern := GetEventByID(AEvent.ParentID);
        TcxSchedulerAggregateStorageEvent(AEvent).FPattern := APattern;
        DoEventInserting(AEvent, AStorage);
        if AStorage <> nil then
        begin
          ASource := AStorage.createEvent;
          ASource.Assign(AEvent);
          if APattern <> nil then
            ASource.ParentID := TcxSchedulerAggregateStorageEvent(APattern).SourceEvent.ID;
          AEvent.EndEditing;
          ASource.Post;
          TcxSchedulerAggregateStorageEvent(AEvent).SourceEvent := ASource;
          FEventsList.Add(AEvent);
        end
        else
          FreeAndNil(AEvent);
      end;
      Changed;
    finally
      FIsPostEvent := False;
      EndUpdate;
    end;
    if AEvent <> nil then
      SendNotification(AEvent, False, False);
  end;
end;

procedure TcxSchedulerAggregateStorage.RemoveStorageEvents(
  AStorage: TcxCustomSchedulerStorage);

  procedure CheckEvent(AEvent: TcxSchedulerAggregateStorageEvent);
  begin
    if AEvent.SourceStorage = AStorage then
    begin
      DoDestroyEvent(AEvent);
      SendNotification(AEvent, True, False);
      AEvent.Free;
    end;
  end;

var
  I: Integer;
begin
  for I := EventCount - 1 downto 0 do
    CheckEvent(TcxSchedulerAggregateStorageEvent(Events[I]));
end;

procedure TcxSchedulerAggregateStorage.SendNotification(
  AEvent: TcxSchedulerEvent; AIsRemoved: Boolean = False; ACheckLockCount: Boolean = True);
begin
  if (AIsRemoved or IsDestroying or CanRefresh or (AEvent <> nil)) then
    inherited SendNotification(AEvent, AIsRemoved, ACheckLockCount);
end;

procedure TcxSchedulerAggregateStorage.BeginUpdateDataController;
var
  ALinkIndex: Integer;
begin
  if Links = nil then Exit;
  for ALinkIndex := 0 to Links.Count - 1 do
    if (Links[ALinkIndex].Storage <> nil) then
      Links[ALinkIndex].Storage.BeginUpdate;
  inherited BeginUpdateDataController;
end;

procedure TcxSchedulerAggregateStorage.EndUpdateDataController;
var
  ALinkIndex: Integer;
begin
  if Links = nil then Exit;
  for ALinkIndex := 0 to Links.Count - 1 do
    if (Links[ALinkIndex].Storage <> nil) then
      Links[ALinkIndex].Storage.EndUpdate;
  inherited EndUpdateDataController;
end;

// IcxSchedulerStorageListener

procedure TcxSchedulerAggregateStorage.AddEvent(AEvent: TcxSchedulerEvent);
begin
  InternalAddEvent(AEvent);
end;

procedure TcxSchedulerAggregateStorage.RemoveEvent(AEvent: TcxSchedulerEvent);
var
  AAggregatedEvent: TcxSchedulerAggregateStorageEvent;
begin
  BeginUpdate;
  try
    AAggregatedEvent := GetEventBySource(AEvent);
    if AAggregatedEvent = nil then Exit;
    AAggregatedEvent.SourceEvent := nil;
    DoDeleteEvent(AAggregatedEvent);
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerAggregateStorage.StorageChanged(Sender: TObject);
begin
  UpdateStructure;
  IsChanged := IsLocked;
  if not IsLocked then
  begin
    inherited SendNotification(nil);
    if CanUpdateReminders then
      Reminders.Refresh;
  end;
end;

procedure TcxSchedulerAggregateStorage.StorageRemoved(Sender: TObject);
var
  AIndex: Integer;
begin
  AIndex := Links.GetStorageLinkIndex(TcxCustomSchedulerStorage(Sender));
  if AIndex >= 0 then
    Links[AIndex].Storage := nil;
end;

function TcxSchedulerAggregateStorage.CanRefresh: Boolean;
var
  I: Integer;
begin
  Result := True;
  if Links <> nil then
    for I := 0 to Links.Count - 1 do
      if Links[I].Storage <> nil then
      begin
        Result := Result and (FieldCount = Links[I].Storage.FieldCount);
        if not Result then
          Break;
      end;
end;

function TcxSchedulerAggregateStorage.GetCustomFields: TcxSchedulerStorageFields;
begin
  Result := TcxSchedulerStorageFields(inherited CustomFields);
end;

procedure TcxSchedulerAggregateStorage.SetCustomFields(const AValue: TcxSchedulerStorageFields);
begin
  CustomFields.Assign(AValue);
end;

end.



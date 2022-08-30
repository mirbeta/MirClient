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

unit cxSchedulerDBStorage;

{$I cxVer.inc}

interface

uses
  Types, Windows, Classes, SysUtils, Graphics, DB, RTLConsts, dxCoreClasses, cxDB, cxDBData, cxSchedulerStorage,
  cxClasses, cxCustomData, cxDataStorage, cxDataUtils;

type
  TcxSchedulerDBStorage = class;
  TcxSchedulerDBStorageDataController = class;
  TcxFieldName = type string;

  { TcxSchedulerDataLink }

  TcxSchedulerDataLink = class(TcxDBCustomDataLink)
  private
    FDataController: TcxSchedulerDBStorageDataController;
    FDataLocate: Boolean;
    FIsRefresh: Boolean;
    FModified: Boolean;
    function GetDataBusy: Boolean;
    function GetStorage: TcxSchedulerDBStorage;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    function GetIsDataSetBusyState: Boolean; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
  public
    constructor Create(ADataController: TcxSchedulerDBStorageDataController); virtual;

    property DataController: TcxSchedulerDBStorageDataController read FDataController;
    property DataBusy: Boolean read GetDataBusy;
    property DataLocate: Boolean read FDataLocate write FDataLocate;
    property Modified: Boolean read FModified write FModified;
    property IsRefresh: Boolean read FIsRefresh write FIsRefresh;
    property Storage: TcxSchedulerDBStorage read GetStorage;
  end;

  { TcxSchedulerDBStorageDataController }

  TcxSchedulerDBStorageDataController = class(TcxSchedulerStorageDataController)
  private
    FDataLink: TcxSchedulerDataLink;
    FKeyField: TField;
    function GetDataSet: TDataSet;
    function GetStorage: TcxSchedulerDBStorage;
  protected
    FDataSetBookmark: TObject;
    procedure ComponentRemoved(AComponent: TComponent); virtual;
    procedure Initialize; virtual;
    procedure InitializeField(var AField: TField; const AFieldName: string);
    procedure RestorePos;
    procedure SavePos(ASetTopPosition: Boolean);
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
    function UpdateEvent(const AEvent: TcxSchedulerEvent): Boolean; virtual;
    procedure UpdateEventValues(AEvent: TcxSchedulerEvent);

    property DataLink: TcxSchedulerDataLink read FDataLink;
    property DataSet: TDataSet read GetDataSet;
    property Storage: TcxSchedulerDBStorage read GetStorage;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetItem(Index: Integer): TObject; override;
    function GetItemValueSource(AItemIndex: Integer): TcxDataEditValueSource; override;
    function GetItemID(AItem: TObject): Integer; override;
    procedure UpdateData; override;
    procedure UpdateItemIndexes; override;

    property KeyField: TField read FKeyField;
  end;

  TcxSchedulerDBFieldLink = class
  public
    Field: TField;
    RefCount: Integer;
  end;

  { TcxSchedulerDBStorageField }

  TcxSchedulerDBStorageField = class(TcxCustomSchedulerStorageField)
  private
    FFieldLink: TcxSchedulerDBFieldLink;
    FFieldName: string;
    function GetDataController: TcxSchedulerDBStorageDataController;
    function GetField: TField;
    function GetStorage: TcxSchedulerDBStorage;
    procedure SetFieldName(const AValue: string);
  protected
    procedure Initialize; virtual;
    function GetDisplayName: string; override;
    function GetIsActive: Boolean; override;
    function GetIsBlob: Boolean; override;
    function GetIsTextStored: Boolean; override;

    property DataController: TcxSchedulerDBStorageDataController read GetDataController;
    property FieldLink: TcxSchedulerDBFieldLink read FFieldLink;
  public
    procedure Assign(Source: TPersistent); override;

    property Field: TField read GetField;
    property Storage: TcxSchedulerDBStorage read GetStorage;
  published
    property FieldName: string read FFieldName write SetFieldName;
  end;

  { TcxSchedulerDBStorageFields }

  TcxSchedulerDBStorageFields = class(TcxCustomSchedulerStorageFields)
  private
    function GetItem(AIndex: Integer): TcxSchedulerDBStorageField;
    procedure SetItem(AIndex: Integer; const AValue: TcxSchedulerDBStorageField);
  protected
    function FindFieldByName(const AName: string): TcxCustomSchedulerStorageField; override;
  public
    function Add: TcxSchedulerDBStorageField;
    function ItemByFieldName(const AFieldName: string): TcxSchedulerDBStorageField;

    property Items[Index: Integer]: TcxSchedulerDBStorageField read GetItem write SetItem;
  end;

  { TcxSchedulerDBStorageFieldNames }

  TcxSchedulerDBStorageFieldNames = class(TPersistent)
  private
    FID: TcxFieldName;
    FOwner: TcxSchedulerDBStorage;
    function GetDataController: TcxSchedulerDBStorageDataController;
    procedure SetIDFieldName(const AValue: TcxFieldName);
  protected
    function GetFieldName(AIndex: Integer): TcxFieldName;
    function GetOwner: TPersistent; override;
    procedure SetFieldName(AIndex: Integer; const AValue: TcxFieldName);
    property ReminderTime: TcxFieldName index 10 read GetFieldName write SetFieldName;
  public
    constructor Create(AOwner: TcxSchedulerDBStorage); virtual;
    procedure Assign(Source: TPersistent); override;

    property Storage: TcxSchedulerDBStorage read FOwner;
    property DataController: TcxSchedulerDBStorageDataController read GetDataController;
  published
    property ActualFinish: TcxFieldName index 15 read GetFieldName write SetFieldName;
    property ActualStart: TcxFieldName index 16 read GetFieldName write SetFieldName;
    property Caption: TcxFieldName index 0 read GetFieldName write SetFieldName;
    property GroupID: TcxFieldName index 22 read GetFieldName write SetFieldName;
    property EventType: TcxFieldName index 1 read GetFieldName write SetFieldName;
    property Finish: TcxFieldName index 2 read GetFieldName write SetFieldName;
    property ID: TcxFieldName read FID write SetIDFieldName;
    property LabelColor: TcxFieldName index 3 read GetFieldName write SetFieldName;
    property Location: TcxFieldName index 4 read GetFieldName write SetFieldName;
    property Message: TcxFieldName index 5 read GetFieldName write SetFieldName;
    property Options: TcxFieldName index 6 read GetFieldName write SetFieldName;
    property ParentID: TcxFieldName index 7 read GetFieldName write SetFieldName;
    property RecurrenceIndex: TcxFieldName index 8 read GetFieldName write SetFieldName;
    property RecurrenceInfo: TcxFieldName index 9 read GetFieldName write SetFieldName;
    property ReminderDate: TcxFieldName index 10 read GetFieldName write SetFieldName;
    property ReminderMinutesBeforeStart: TcxFieldName index 11 read GetFieldName write SetFieldName;
    property ReminderResourcesData: TcxFieldName index 21 read GetFieldName write SetFieldName;
    property ResourceID: TcxFieldName index 12 read GetFieldName write SetFieldName;
    property Start: TcxFieldName index 13 read GetFieldName write SetFieldName;
    property State: TcxFieldName index 14 read GetFieldName write SetFieldName;
    property TaskCompleteField: TcxFieldName index 17 read GetFieldName write SetFieldName;
    property TaskIndexField: TcxFieldName index 18 read GetFieldName write SetFieldName;
    property TaskLinksField: TcxFieldName index 19 read GetFieldName write SetFieldName;
    property TaskStatusField: TcxFieldName index 20 read GetFieldName write SetFieldName;
  end;

  { TcxSchedulerDBStorageResourceDataController }

  TcxSchedulerDBStorageResourceDataController = class(TcxDBDataController)
  protected
    procedure UpdateControl(AInfo: TcxUpdateControlInfo); override;
  public
    function GetItem(AIndex: Integer): TObject; override;
  end;

  { TcxSchedulerDBStorageResources }

  TcxSchedulerDBStorageResources = class(TcxSchedulerStorageResources)
  private
    FDataController: TcxSchedulerDBStorageResourceDataController;
    FDataItems: TcxSchedulerStorageResourceItems;
    FResourceColorField: TcxDBDataField;
    FResourceColorFieldName: TcxFieldName;
    FResourceIDField: TcxDBDataField;
    FResourceIDFieldName: TcxFieldName;
    FResourceImageIndexField: TcxDBDataField;
    FResourceImageIndexFieldName: TcxFieldName;
    FResourceNameField: TcxDBDataField;
    FResourceNameFieldName: TcxFieldName;
    function GetDataSource: TDataSource;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetResourceColorFieldName(const AValue: TcxFieldName);
    procedure SetResourceIDFieldName(const AValue: TcxFieldName);
    procedure SetResourceImageIndexFieldName(const AValue: TcxFieldName);
    procedure SetResourceNameFieldName(const AValue: TcxFieldName);
  protected
    function CreateDataController: TcxSchedulerDBStorageResourceDataController; virtual;
    function GetResourceItems: TcxSchedulerStorageResourceItems; override;
    function GetValueDef(ARecordIndex: Integer; AField: TcxDBDataField;
      const DefValue: Variant): Variant;
    procedure InitDataItem(AItem: TcxSchedulerStorageResourceItem; AIndex: Integer); virtual;
    procedure SetInternalFieldName(var AFieldName: TcxFieldName; const AValue: TcxFieldName; var AField: TcxDBDataField);
    procedure UpdateResourceItems; virtual;
    property DataController: TcxSchedulerDBStorageResourceDataController read FDataController;
    property ResourceColorField: TcxDBDataField read FResourceColorField;
    property ResourceIDField: TcxDBDataField read FResourceIDField;
    property ResourceImageIndexField: TcxDBDataField read FResourceImageIndexField;
    property ResourceNameField: TcxDBDataField read FResourceNameField;
  public
    constructor Create(AOwner: TcxCustomSchedulerStorage); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    property DataItems: TcxSchedulerStorageResourceItems read FDataItems;
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property ResourceColor: TcxFieldName read FResourceColorFieldName write SetResourceColorFieldName;
    property ResourceID: TcxFieldName read FResourceIDFieldName write SetResourceIDFieldName;
    property ResourceImageIndex: TcxFieldName read FResourceImageIndexFieldName write SetResourceImageIndexFieldName;
    property ResourceName: TcxFieldName read FResourceNameFieldName write SetResourceNameFieldName;
    property Images;
    property Items;
    property OnGetResourceImageIndex;
    property OnGetResourceName;
  end;

  TcxSchedulerGetEventGeneratedIDEvent = procedure(Sender: TcxSchedulerDBStorage;
    AEvent: TcxSchedulerEvent; var EventID: Variant) of object;

  { TcxSchedulerDBStorage }

  TcxSchedulerDBStorage = class(TcxCustomSchedulerStorage)
  private
    FDataBusy: Boolean;
    FFieldNames: TcxSchedulerDBStorageFieldNames;
    FFieldLinks: TcxObjectList;
    FGenerateGUIDForID: Boolean;
    FSmartRefresh: Boolean;
    FUpdatingEvent: TcxSchedulerEvent;
    FUseIndexedID: Boolean;
    FOnGetEventGeneratedID: TcxSchedulerGetEventGeneratedIDEvent;
    function GetDataController: TcxSchedulerDBStorageDataController;
    function GetDataSource: TDataSource;
    function GetDataSet: TDataSet;
    function GetCustomFields: TcxSchedulerDBStorageFields;
    function GetResources: TcxSchedulerDBStorageResources;
    procedure SetDataSource(AValue: TDataSource);
    procedure SetCustomFields(const AValue: TcxSchedulerDBStorageFields);
    procedure SetFieldNames(AValue: TcxSchedulerDBStorageFieldNames);
    procedure SetResources(AValue: TcxSchedulerDBStorageResources);
    procedure SetSmartRefresh(AValue: Boolean);
    procedure SetUseIndexedID(AValue: Boolean);
  protected
    function AddEventRecord(AEvent: TcxSchedulerEvent): Boolean; override;
    procedure AfterPostEditingData(AEvent: TcxSchedulerEvent); override;
    procedure BeforeDeleteEvent(AEvent: TcxSchedulerEvent); override;
    procedure BeforePostEditingData(AEvent: TcxSchedulerEvent); override;
    function CreateEventInternal: TcxSchedulerEvent; override;
    function CreateFieldNames: TcxSchedulerDBStorageFieldNames; virtual;
    function CreateFields: TcxCustomSchedulerStorageFields; override;
    function CreateResources: TcxSchedulerStorageResources; override;
    procedure CreateSubClasses; override;
    procedure DeleteEvents(AEvents: TcxObjectList); override;
    procedure DestroySubClasses; override;
    procedure DoDestroyEvent(AEvent: TcxSchedulerEvent); override;
    function DoGetEventGeneratedID(AEvent: TcxSchedulerEvent; const AEventID: Variant): Variant; virtual;
    function GetFieldLink(AField: TField): TcxSchedulerDBFieldLink;
    function GetFieldLinkByIndex(AFieldIndex: Integer): TcxSchedulerDBFieldLink;
    function GetDataControllerClass: TcxCustomDataControllerClass; override;
    function GetIsBoundMode: Boolean; override;
    //todo: after storage optimization!!!
    function GetFieldValueTypeClass(AField: TcxCustomSchedulerStorageField): TcxValueTypeClass; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function MakeFocused(AEvent: TcxSchedulerEvent): Boolean;
    procedure RelateFields(AStorageField: TcxSchedulerDBStorageField; AField: TField);
    procedure RemoveDeletedEvents(AList: TdxFastList);
    procedure SetValue(const ARecordHandle: Pointer; const AFieldIndex: Integer; const AValue: Variant); override;
    procedure SetValueBeforePost(AEvent: TcxSchedulerEvent; AItemIndex: Integer); override;
    procedure SynchronizeEventsWithRecords; override;
    procedure SynchronizeIndexedEvents; virtual;
    procedure SynchronizeNoIndexedEvents; virtual;
    procedure UpdateEventFieldValue(AEvent: TcxSchedulerEvent; AField: TField);
    //
    procedure BeginUpdateDataController; override;
    procedure EndUpdateDataController; override;
    //
    property DataController: TcxSchedulerDBStorageDataController read GetDataController;
    property DataBusy: Boolean read FDataBusy write FDataBusy;
    property DataSet: TDataSet read GetDataSet;
    property FieldLinks: TcxObjectList read FFieldLinks;
    property UpdatingEvent: TcxSchedulerEvent read FUpdatingEvent write FUpdatingEvent;
  public
    procedure Assign(Source: TPersistent); override;
    function CheckRequiredFields: Boolean; override;
    procedure Clear; override;
    function IsActive: Boolean; override;
    procedure PostEvents; override;
  published
    property GenerateGUIDForID: Boolean read FGenerateGUIDForID write FGenerateGUIDForID default False;
    property Reminders;
    property Resources: TcxSchedulerDBStorageResources read GetResources write SetResources;
    property SmartRefresh: Boolean read FSmartRefresh write SetSmartRefresh default False;
    property CustomFields: TcxSchedulerDBStorageFields read GetCustomFields write SetCustomFields;
    property Holidays;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FieldNames: TcxSchedulerDBStorageFieldNames read FFieldNames write SetFieldNames;
    property UseIndexedID: Boolean read FUseIndexedID write SetUseIndexedID default False;
    property OnEventDeleted;
    property OnEventInserted;
    property OnEventIntersect;
    property OnEventModified;
    property OnFilterEvent;
    property OnGetEventGeneratedID: TcxSchedulerGetEventGeneratedIDEvent read FOnGetEventGeneratedID write FOnGetEventGeneratedID;
  end;

implementation

uses
  Variants, dxCore, cxVariants, cxSchedulerStrs, cxSchedulerUtils;

type
  TcxSchedulerEventAccess = class(TcxSchedulerEvent);
  TcxCustomDataFieldAccess = class(TcxCustomDataField);

function CheckAndSetFieldValue(AField: TField; const AValue: Variant): Boolean;
begin
  Result := (AField <> nil) and AField.CanModify and ((VarType(AField.Value) <> VarType(AValue)) or
    VarIsSoftNull(AField.Value) or not VarEqualsSoft(AField.Value, AValue));
  if Result then
    SetFieldValue(AField, AValue);
end;

function cxCompareEventsByID(AEvent1, AEvent2: TcxSchedulerEvent): Integer;
begin
  Result := VarCompare(AEvent2.ID, AEvent1.ID);
end;

function cxFindEvent(AList: TdxFastList; const ID: Variant; var AEvent: TcxSchedulerEvent): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := AList.Count - 1;
  if H < 0 then Exit;
  AEvent := TcxSchedulerEvent(AList.Last);
  Result := VarCompare(AEvent.ID, ID) = 0;
  if Result then
  begin
    AList.Delete(H);
    Exit;
  end;
  while L <= H do
  begin
    I := (L + H) shr 1;
    AEvent := TcxSchedulerEvent(AList.List[I]);
    C := VarCompare(ID, AEvent.ID);
    if C < 0 then
      L := I + 1
    else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        AList.Delete(I);
        Break;
      end;
    end;
  end;
end;

{ TcxSchedulerDataLink }

constructor TcxSchedulerDataLink.Create(ADataController: TcxSchedulerDBStorageDataController);
begin
  inherited Create;
  FDataController := ADataController;
end;

procedure TcxSchedulerDataLink.ActiveChanged;
begin
  inherited ActiveChanged;
  DataController.Initialize;
  if not Active then
    Storage.Clear;
end;

procedure TcxSchedulerDataLink.DataSetChanged;
begin
  if IsDataSetBusy or DataController.UpdateEvent(Storage.UpdatingEvent) then
    Exit;
  if DataLocate then
  begin
    try
      RecordChanged(nil);
    finally
      DataLocate := False;
    end;
    Exit;
  end;
  if IsDataSetBusy or IsRefresh or (DataSet.State in dsEditModes) then
    Exit;
  Storage.FullRefresh;
  FModified := False;
end;

procedure TcxSchedulerDataLink.DataSetScrolled(Distance: Integer);
begin
end;

function TcxSchedulerDataLink.GetIsDataSetBusyState: Boolean;
begin
  Result := DataBusy;
end;

procedure TcxSchedulerDataLink.LayoutChanged;
begin
  if DataBusy then
    Storage.FullRefresh
  else
    DataSetChanged;
end;

procedure TcxSchedulerDataLink.RecordChanged(Field: TField);
begin
  //
end;

procedure TcxSchedulerDataLink.UpdateData;
begin
  RecordChanged(nil);
end;

function TcxSchedulerDataLink.GetDataBusy: Boolean;
begin
  Result := Storage.DataBusy;
end;

function TcxSchedulerDataLink.GetStorage: TcxSchedulerDBStorage;
begin
  Result := DataController.Storage;
end;

{ TcxSchedulerDBStorageDataController }

constructor TcxSchedulerDBStorageDataController.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TcxSchedulerDataLink.Create(Self);
end;

destructor TcxSchedulerDBStorageDataController.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TcxSchedulerDBStorageDataController.GetItem(
  Index: Integer): TObject;
begin
  Result := Storage.FFields[Index];
end;

function TcxSchedulerDBStorageDataController.GetItemValueSource(
  AItemIndex: Integer): TcxDataEditValueSource;
begin
  Result := evsValue;
end;

function TcxSchedulerDBStorageDataController.GetItemID(
  AItem: TObject): Integer;
begin
  if AItem is TcxCustomSchedulerStorageField then
    Result := TcxCustomSchedulerStorageField(AItem).Index
  else
    Result := -1;
end;

procedure TcxSchedulerDBStorageDataController.UpdateData;
begin
  Storage.FullRefresh;
end;

procedure TcxSchedulerDBStorageDataController.UpdateItemIndexes;
begin
  Storage.UpdateItemIndexes;
  inherited UpdateItemIndexes;
end;

procedure TcxSchedulerDBStorageDataController.ComponentRemoved(
  AComponent: TComponent);
begin
  if AComponent = FKeyField then
    FKeyField := nil;
end;

procedure TcxSchedulerDBStorageDataController.Initialize;
var
  I: Integer;
  AField: TField;
begin
  InitializeField(AField, Storage.FieldNames.ID);
  if AField <> FKeyField then
    Storage.RelateFields(nil, AField);
  for I := 0 to Storage.FieldCount - 1 do
    TcxSchedulerDBStorageField(Storage.Fields[I]).Initialize;
  Storage.FullRefresh;
end;

procedure TcxSchedulerDBStorageDataController.InitializeField(
  var AField: TField; const AFieldName: string);
begin
  if DataLink.Active then
  begin
    AField := DataSet.FindField(AFieldName);
    if AField <> nil then
      AField.FreeNotification(Storage);
  end
  else
    AField := nil;
end;

procedure TcxSchedulerDBStorageDataController.RestorePos;
begin
  cxDataSetRestoreBookmark(DataSet, TcxDataSetBookmark(FDataSetBookmark));
  DataSet.EnableControls;
end;

procedure TcxSchedulerDBStorageDataController.SavePos(ASetTopPosition: Boolean);
begin
  FDataSetBookmark := cxDataSetCreateBookMark(DataSet);
  DataSet.DisableControls;
  if ASetTopPosition then
    DataSet.First;
end;

procedure TcxSchedulerDBStorageDataController.UpdateControl(
  AInfo: TcxUpdateControlInfo);
begin
  Storage.UpdateControl(AInfo);
end;

function TcxSchedulerDBStorageDataController.UpdateEvent(
  const AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := (AEvent <> nil) and not VarIsNull(AEvent.ID);
  if not Result then Exit;
  if not VarEquals(AEvent.ID, KeyField.Value) then
  begin
    DataLink.DataLocate := True;
    try
      Result := DataSet.Locate(KeyField.FieldName, AEvent.ID, []);
    finally
      DataLink.DataLocate := False;
    end;
  end;
  UpdateEventValues(AEvent);
end;

procedure TcxSchedulerDBStorageDataController.UpdateEventValues(
  AEvent: TcxSchedulerEvent);
var
  I: Integer;
  AHandle: Pointer;
  AValueDef: TcxValueDef;
  AStorageField: TcxSchedulerDBStorageField;
begin
  AHandle := AEvent.RecordHandle;
  if AEvent.IsEditing then
    AHandle := AEvent.EditingRecordHandle;
  if AHandle = nil then Exit;
  AEvent.ID := GetFieldValue(KeyField);
  for I := 0 to DataStorage.ValueDefs.Count - 1 do
  begin
    AStorageField := TcxSchedulerDBStorageField(Storage.Fields[I]);
    AValueDef := AStorageField.ValueDef;
    if AStorageField.Field <> nil then
    begin
      TcxDataStorageHelper.SetValue(AHandle, AValueDef, GetFieldValue(AStorageField.Field));
      if AValueDef.TextStored then
         TcxDataStorageHelper.SetDisplayText(AHandle, AValueDef, AStorageField.Field.DisplayText);
    end
    else
    begin
      TcxDataStorageHelper.SetValue(AHandle, AValueDef, Null);
      if AValueDef.TextStored then
         TcxDataStorageHelper.SetDisplayText(AHandle, AValueDef, '');
    end;
  end;
end;

function TcxSchedulerDBStorageDataController.GetDataSet: TDataSet;
begin
  Result := DataLink.DataSet;
end;

function TcxSchedulerDBStorageDataController.GetStorage: TcxSchedulerDBStorage;
begin
  Result := TcxSchedulerDBStorage(GetOwner)
end;

{ TcxSchedulerDBStorageField }

procedure TcxSchedulerDBStorageField.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerDBStorageField then
    FieldName := TcxSchedulerDBStorageField(Source).FieldName;
end;

procedure TcxSchedulerDBStorageField.Initialize;
var
  AField: TField;
  ATypeClass: TcxValueTypeClass;
begin
  DataController.InitializeField(AField, FieldName);
  if Field <> AField then
    Storage.RelateFields(Self, AField);
  if Field <> nil then
  begin
    ATypeClass := GetValueTypeClassByField(Field);
    DataController.ChangeValueTypeClass(Index, ATypeClass);
  end;
end;

function TcxSchedulerDBStorageField.GetDisplayName: string;
begin
  Result := FieldName;
  if Result = '' then
    Result := inherited GetDisplayName;
end;

function TcxSchedulerDBStorageField.GetIsActive: Boolean;
begin
  Result := DataController.DataLink.Active and (Field <> nil);
end;

function TcxSchedulerDBStorageField.GetIsBlob: Boolean;
begin
  Result := (Field <> nil) and Field.IsBlob;
end;

function TcxSchedulerDBStorageField.GetIsTextStored: Boolean;
begin
  Result := (Field <> nil) and IsFieldFormatted(Field,
    DataController.GetItemValueSource(Index) = evsText);
end;

function TcxSchedulerDBStorageField.GetDataController: TcxSchedulerDBStorageDataController;
begin
  Result := TcxSchedulerDBStorageDataController(inherited DataController);
end;

function TcxSchedulerDBStorageField.GetField: TField;
begin
  if FieldLink <> nil then
    Result := FieldLink.Field
  else
    Result := nil;
end;

function TcxSchedulerDBStorageField.GetStorage: TcxSchedulerDBStorage;
begin
  Result := TcxSchedulerDBStorage(inherited Storage);
end;

procedure TcxSchedulerDBStorageField.SetFieldName(
  const AValue: string);
begin
  if not SameText(AValue, FieldName) then
  begin
    FFieldName := AValue;
    Initialize;
    Storage.FullRefresh;
  end;
end;

{ TcxSchedulerDBStorageFields }

function TcxSchedulerDBStorageFields.Add: TcxSchedulerDBStorageField;
begin
  Result := TcxSchedulerDBStorageField(inherited Add)
end;

function TcxSchedulerDBStorageFields.ItemByFieldName(
  const AFieldName: string): TcxSchedulerDBStorageField;
begin
  Result := TcxSchedulerDBStorageField(FindFieldByName(AFieldName));
end;

function TcxSchedulerDBStorageFields.FindFieldByName(
  const AName: string): TcxCustomSchedulerStorageField;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := TcxSchedulerStorageField(inherited Items[I]);
    if AnsiSameText(AName, TcxSchedulerDBStorageField(Result).FieldName) then Exit;
  end;
  Result := nil;
end;

function TcxSchedulerDBStorageFields.GetItem(
  AIndex: Integer): TcxSchedulerDBStorageField;
begin
  Result := TcxSchedulerDBStorageField(inherited Items[AIndex])
end;

procedure TcxSchedulerDBStorageFields.SetItem(
  AIndex: Integer; const AValue: TcxSchedulerDBStorageField);
begin
  Items[AIndex].Assign(AValue);
end;

{ TcxSchedulerDBStorageFieldNames }

constructor TcxSchedulerDBStorageFieldNames.Create(
  AOwner: TcxSchedulerDBStorage);
begin
  FOwner := AOwner;
end;

procedure TcxSchedulerDBStorageFieldNames.Assign(
  Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerDBStorageFieldNames then
  begin
    for I := 0 to Storage.InternalFields.Count - 1 do
      SetFieldName(I, TcxSchedulerDBStorageFieldNames(Source).GetFieldName(I));
  end;
end;

function TcxSchedulerDBStorageFieldNames.GetDataController: TcxSchedulerDBStorageDataController;
begin
  Result := FOwner.DataController;
end;

function TcxSchedulerDBStorageFieldNames.GetFieldName(
  AIndex: Integer): TcxFieldName;
begin
  Result := TcxSchedulerDBStorageField(
    Storage.InternalFields.Items[AIndex]).FieldName;
end;

function TcxSchedulerDBStorageFieldNames.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxSchedulerDBStorageFieldNames.SetFieldName(
  AIndex: Integer; const AValue: TcxFieldName);
begin
  TcxSchedulerDBStorageField(
    Storage.InternalFields.Items[AIndex]).FieldName := AValue;
end;

procedure TcxSchedulerDBStorageFieldNames.SetIDFieldName(
  const AValue: TcxFieldName);
begin
  FID := AValue;
  DataController.Initialize;
end;

{ TcxSchedulerDBStorageResourceDataController }

function TcxSchedulerDBStorageResourceDataController.GetItem(
  AIndex: Integer): TObject;
begin
  Result := nil;
end;

procedure TcxSchedulerDBStorageResourceDataController.UpdateControl(
  AInfo: TcxUpdateControlInfo);
begin
  if (AInfo is TcxDataChangedInfo) or (AInfo is TcxUpdateRecordInfo) then
    TcxSchedulerDBStorage(GetOwner).Resources.UpdateResourceItems;
end;

{ TcxSchedulerDBStorageResources }

constructor TcxSchedulerDBStorageResources.Create(
  AOwner: TcxCustomSchedulerStorage);
begin
  inherited Create(AOwner);
  FDataController := CreateDataController;
  FDataItems := CreateItems;
end;

destructor TcxSchedulerDBStorageResources.Destroy;
begin
  FDataItems.Free;
  FreeAndNil(FResourceColorField);
  FreeAndNil(FResourceIDField);
  FreeAndNil(FResourceImageIndexField);
  FreeAndNil(FResourceNameField);
  FreeAndNil(FDataController);
  inherited Destroy;
end;

procedure TcxSchedulerDBStorageResources.Assign(
  Source: TPersistent);
begin
  inherited Assign(Source);
  if Source is TcxSchedulerDBStorageResources then
  begin
    DataSource := TcxSchedulerDBStorageResources(Source).DataSource;
    ResourceColor := TcxSchedulerDBStorageResources(Source).ResourceColor;
    ResourceID := TcxSchedulerDBStorageResources(Source).ResourceID;
    ResourceImageIndex := TcxSchedulerDBStorageResources(Source).ResourceImageIndex;
    ResourceName := TcxSchedulerDBStorageResources(Source).ResourceName;
  end;
end;

function TcxSchedulerDBStorageResources.CreateDataController: TcxSchedulerDBStorageResourceDataController;
begin
  Result := TcxSchedulerDBStorageResourceDataController.Create(Storage);
end;

function TcxSchedulerDBStorageResources.GetResourceItems: TcxSchedulerStorageResourceItems;
begin
  if DataItems.Count > 0 then
    Result := DataItems
  else
    Result := Items;
end;

procedure TcxSchedulerDBStorageResources.InitDataItem(
  AItem: TcxSchedulerStorageResourceItem; AIndex: Integer);
begin
  AItem.ResourceID := GetValueDef(AIndex, ResourceIDField, Null);
  AItem.ImageIndex := GetValueDef(AIndex, ResourceImageIndexField, -1);
  AItem.Name := GetValueDef(AIndex, ResourceNameField, '');
  AItem.Color := GetValueDef(AIndex, ResourceColorField, clDefault);
end;

function TcxSchedulerDBStorageResources.GetValueDef(ARecordIndex: Integer;
  AField: TcxDBDataField; const DefValue: Variant): Variant;
begin
  if AField <> nil then
  begin
    Result := DataController.GetInternalValue(ARecordIndex, AField);
    if VarIsNull(Result) then
      Result := DefValue
  end
  else
    Result := DefValue
end;

procedure TcxSchedulerDBStorageResources.SetInternalFieldName(
  var AFieldName: TcxFieldName; const AValue: TcxFieldName;
  var AField: TcxDBDataField);
begin
  if AFieldName <> AValue then
  begin
    AFieldName := AValue;
    DataController.UpdateInternalKeyFields(AValue, AField);
  end;
end;

procedure TcxSchedulerDBStorageResources.UpdateResourceItems;
var
  I, J: Integer;
  APrevItems: TcxSchedulerStorageResourceItems;
begin
  BeginUpdate;
  try
    APrevItems := FDataItems;
    FDataItems := CreateItems;
    try
      for I := 0 to DataController.RecordCount - 1 do
        InitDataItem(FDataItems.Add, I);
      for I := 0 to FDataItems.Count - 1 do
        for J := 0 to APrevItems.Count - 1 do
          if VarEquals(FDataItems[I].ResourceID, APrevItems[J].ResourceID) then
            with FDataItems[I] do
            begin
              Assign(APrevItems[J]);
              ResourceID := GetValueDef(I, ResourceIDField, ResourceID);
              ImageIndex := GetValueDef(I, ResourceImageIndexField, -1);
              Name := GetValueDef(I, ResourceNameField, Name);
              Color := GetValueDef(I, ResourceColorField, Color);
            end;
    finally
      APrevItems.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerDBStorageResources.GetDataSource: TDataSource;
begin
  Result := DataController.DataSource;
end;

procedure TcxSchedulerDBStorageResources.SetDataSource(
  AValue: TDataSource);
begin
  DataController.DataSource := AValue;
end;

procedure TcxSchedulerDBStorageResources.SetResourceColorFieldName(
  const AValue: TcxFieldName);
begin
  SetInternalFieldName(FResourceColorFieldName, AValue, FResourceColorField);
end;

procedure TcxSchedulerDBStorageResources.SetResourceIDFieldName(
  const AValue: TcxFieldName);
begin
  SetInternalFieldName(FResourceIDFieldName, AValue, FResourceIDField);
end;

procedure TcxSchedulerDBStorageResources.SetResourceImageIndexFieldName(
  const AValue: TcxFieldName);
begin
  SetInternalFieldName(FResourceImageIndexFieldName, AValue, FResourceImageIndexField);
end;

procedure TcxSchedulerDBStorageResources.SetResourceNameFieldName(
  const AValue: TcxFieldName);
begin
  SetInternalFieldName(FResourceNameFieldName, AValue, FResourceNameField);
end;

{ TcxSchedulerDBStorage }

procedure TcxSchedulerDBStorage.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerDBStorage then
  begin
    FieldNames := TcxSchedulerDBStorage(Source).FieldNames;
    DataSource := TcxSchedulerDBStorage(Source).DataSource;
  end;
  inherited Assign(Source);
end;

function TcxSchedulerDBStorage.CheckRequiredFields: Boolean;
var
  S: string;
begin
  Result := IsDataSettingsValid;
  if not Result then
  begin
    S := '';
    if (FieldNames.ID = '') then
      S := S + ' '#$B7' ID'#13#10;
    if not FEventTypeField.IsActive then
      S := S + ' '#$B7' EventType'#13#10;
    if not FStartField.IsActive then
      S := S + ' '#$B7' Start'#13#10;
    if not FFinishField.IsActive then
      S := S + ' '#$B7' Finish'#13#10;
    if not FOptionsField.IsActive then
      S := S + ' '#$B7' Options'#13#10;
    cxSchedulerError(scxRequiredFieldsNeeded, [S]);
  end;
end;

procedure TcxSchedulerDBStorage.Clear;
var
  I: Integer;
begin
  BeginUpdate;
  try
    if IsActive and IsDataSettingsValid then
    begin
      DataSet.DisableControls;
      try
        while not DataSet.Eof do
          DataSet.Delete;
        inherited Clear;
      finally
        DataSet.EnableControls;
      end;
    end
    else
    begin
      for I := FEventsList.Count - 1 downto 0 do
        SendNotification(FEventsList.Items[I], True, False);
      FEventsList.DestroyItems;
    end;
    Changed;
  finally
    EndUpdate;
  end;
end;

function TcxSchedulerDBStorage.IsActive: Boolean;
begin
  Result := inherited IsActive and DataController.DataLink.Active and
    (DataController.KeyField <> nil);
end;

procedure TcxSchedulerDBStorage.PostEvents;
var
  APrevState: Boolean;
begin
  if not IsActive or (FNewEvents.Count = 0) then Exit;
  APrevState := SmartRefresh;
  SmartRefresh := False;
  try
    inherited PostEvents;
  finally
    SmartRefresh := APrevState;
  end;
end;

procedure TcxSchedulerDBStorage.BeginUpdateDataController;
begin
  Inc(LockCount);
  if Assigned(DataController.DataSet) then
    DataSet.DisableControls;
end;

procedure TcxSchedulerDBStorage.EndUpdateDataController;
begin
  Dec(LockCount);
  BeginStructureUpdating;
  try
    if Assigned(DataSet) then
      DataSet.EnableControls;
    if (DataSet = nil) or not DataSet.ControlsDisabled then
      UpdatingEvent := nil;
  finally
    EndStructureUpdating;
  end;
  if (LockCount = 0) and (not SmartRefresh or RefreshNeeded) then
    FullRefresh;
end;

function TcxSchedulerDBStorage.AddEventRecord(AEvent: TcxSchedulerEvent): Boolean;
var
  ID: Variant;
begin
  Result := True;
  BeginUpdate;
  try
    DataSet.Append;
    if GenerateGUIDForID then
    begin
      ID := dxGenerateGUID;
      DataController.KeyField.Value := ID;
    end;
    SetEventValue(AEvent, -1, DataController.KeyField.Value);
    // todo: for skip locate operation in MakeFocused method
    AEvent.EndEditing;
    // todo: for auto increment fields if key field was modified after post
    if not GenerateGUIDForID then
      ID := DoGetEventGeneratedID(AEvent, DataController.KeyField.Value)
    else
      ID := DataController.KeyField.Value;
    if not VarEquals(AEvent.ID, ID) then
      SetEventValue(AEvent, -1, ID);
  finally
    EndUpdate;
  end;
end;

procedure TcxSchedulerDBStorage.AfterPostEditingData(AEvent: TcxSchedulerEvent);
begin
  UpdatingEvent := AEvent;
  DataSet.Post;
end;

procedure TcxSchedulerDBStorage.BeforeDeleteEvent(AEvent: TcxSchedulerEvent);
begin
  inherited BeforeDeleteEvent(AEvent);
  if AEvent = UpdatingEvent then
    UpdatingEvent := nil;
end;

procedure TcxSchedulerDBStorage.BeforePostEditingData(AEvent: TcxSchedulerEvent);
begin
  MakeFocused(AEvent);
  if not (DataSet.State in dsEditModes) then
    DataSet.Edit;
end;

function TcxSchedulerDBStorage.CreateEventInternal: TcxSchedulerEvent;
begin
  Result := inherited CreateEventInternal;
  Result.RecordHandle := AllocateRecord;
end;

function TcxSchedulerDBStorage.CreateFieldNames: TcxSchedulerDBStorageFieldNames;
begin
  Result := TcxSchedulerDBStorageFieldNames.Create(Self);
end;

function TcxSchedulerDBStorage.CreateFields: TcxCustomSchedulerStorageFields;
begin
  Result := TcxSchedulerDBStorageFields.Create(TcxSchedulerDBStorageField);
end;

function TcxSchedulerDBStorage.CreateResources: TcxSchedulerStorageResources;
begin
  Result := TcxSchedulerDBStorageResources.Create(Self);
end;

procedure TcxSchedulerDBStorage.CreateSubClasses;
begin
  FFieldLinks := TcxObjectList.Create;
  inherited CreateSubClasses;
  FFieldNames := CreateFieldNames;
end;

procedure TcxSchedulerDBStorage.DeleteEvents(AEvents: TcxObjectList);
begin
  if (DataSet <> nil) and (AEvents.Count > 0) then
  begin
    DataSet.DisableControls;
    try
      inherited DeleteEvents(AEvents);
    finally
      DataSet.EnableControls;
    end
  end
  else
    inherited DeleteEvents(AEvents);
end;


procedure TcxSchedulerDBStorage.DestroySubClasses;
begin
  inherited DestroySubClasses;
  FFieldNames.Free;
  FFieldLinks.Free;
end;

procedure TcxSchedulerDBStorage.DoDestroyEvent(AEvent: TcxSchedulerEvent);
begin
  inherited DoDestroyEvent(AEvent);
  if MakeFocused(AEvent) then
    DataSet.Delete;
end;

function TcxSchedulerDBStorage.DoGetEventGeneratedID(AEvent: TcxSchedulerEvent;
  const AEventID: Variant): Variant;
begin
  Result := AEventID;
  if Assigned(FOnGetEventGeneratedID) then
    FOnGetEventGeneratedID(Self, AEvent, Result);
end;

function TcxSchedulerDBStorage.GetFieldLink(AField: TField): TcxSchedulerDBFieldLink;
var
  I: Integer;
begin
  for I := 0 to FieldLinks.Count - 1 do
  begin
    Result := TcxSchedulerDBFieldLink(FieldLinks[I]);
    if Result.Field = AField then Exit;
  end;
  Result := nil;
end;

function TcxSchedulerDBStorage.GetFieldLinkByIndex(
  AFieldIndex: Integer): TcxSchedulerDBFieldLink;
begin
  if AFieldIndex < 0 then
    Result := GetFieldLink(DataController.KeyField)
  else
    Result := TcxSchedulerDBStorageField(Fields[AFieldIndex]).FieldLink;
end;

function TcxSchedulerDBStorage.GetDataControllerClass: TcxCustomDataControllerClass;
begin
  Result := TcxSchedulerDBStorageDataController;
end;

function TcxSchedulerDBStorage.GetIsBoundMode: Boolean;
begin
  Result := True;
end;

function TcxSchedulerDBStorage.GetFieldValueTypeClass(
  AField: TcxCustomSchedulerStorageField): TcxValueTypeClass;
begin
  if (AField = nil) and (DataController.KeyField <> nil) then
    Result := GetValueTypeClassByField(DataController.KeyField)
  else
    Result := inherited GetFieldValueTypeClass(AField)
end;

procedure TcxSchedulerDBStorage.Notification(
  AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if (Operation <> opRemove) or IsDestroying then Exit;
  if AComponent is TField then
  begin
    for I := 0 to FieldCount - 1 do
      if TcxSchedulerDBStorageField(Fields[I]).Field = AComponent then
        RelateFields(TcxSchedulerDBStorageField(Fields[I]), nil);
    if AComponent = DataController.KeyField then
      RelateFields(nil, nil);
  end;
  DataController.ComponentRemoved(AComponent);
end;

function TcxSchedulerDBStorage.MakeFocused(
  AEvent: TcxSchedulerEvent): Boolean;
begin
  Result := VarEquals(AEvent.ID, DataController.KeyField.Value);
  if not Result then
  begin
    DataSet.Locate(FieldNames.ID, AEvent.ID, []);
    Result := VarEquals(AEvent.ID, DataController.KeyField.Value);
  end;
end;

procedure TcxSchedulerDBStorage.RelateFields(
  AStorageField: TcxSchedulerDBStorageField; AField: TField);
var
  APrevLink, ALink: TcxSchedulerDBFieldLink;
begin
  if AStorageField <> nil then
    APrevLink := AStorageField.FieldLink
  else
    APrevLink := GetFieldLink(DataController.KeyField);
  ALink := GetFieldLink(AField);
  if (ALink = nil) and (AField <> nil) then
  begin
    ALink := TcxSchedulerDBFieldLink.Create;
    ALink.Field := AField;
    Inc(ALink.RefCount);
    FFieldLinks.Add(ALink);
  end
  else
    if ALink <> nil then
      Inc(ALink.RefCount);
  if APrevLink <> nil then
  begin
    Dec(APrevLink.RefCount);
    if APrevLink.RefCount = 0 then
    begin
      FFieldLinks.Remove(APrevLink);
      APrevLink.Free;
    end;
  end;
  if AStorageField <> nil then
    AStorageField.FFieldLink := ALink
  else
    DataController.FKeyField := AField;
end;

procedure TcxSchedulerDBStorage.RemoveDeletedEvents(AList: TdxFastList);
var
  I: Integer;
  AEvent: TcxSchedulerEvent;
begin
  for I := 0 to AList.Count - 1 do
  begin
    AEvent := AList.List[I];
    if AEvent <> nil then
    begin
      SendNotification(AEvent, True);
      InternalDelete(AEvent);
      AEvent.Free;
    end;
  end;
end;

procedure TcxSchedulerDBStorage.SetValue(const ARecordHandle: Pointer;
  const AFieldIndex: Integer; const AValue: Variant);
var
  I: Integer;
  ALink: TcxSchedulerDBFieldLink;
begin
  ALink := GetFieldLinkByIndex(AFieldIndex);
  inherited SetValue(ARecordHandle, AFieldIndex, AValue);
  if (ALink = nil) or (ALink.RefCount = 1) then Exit;
  for I := 0 to FieldCount - 1 do
    if (I <> AFieldIndex) and (TcxSchedulerDBStorageField(Fields[I]).FieldLink = ALink) then
      inherited SetValue(ARecordHandle, I, AValue);
end;

procedure TcxSchedulerDBStorage.SetValueBeforePost(
  AEvent: TcxSchedulerEvent; AItemIndex: Integer);
var
  AField: TcxSchedulerDBStorageField;
begin
  AField := TcxSchedulerDBStorageField(FFields.List[AItemIndex]);
  CheckAndSetFieldValue(AField.Field, AEvent.EditValues[AItemIndex]);
end;

procedure TcxSchedulerDBStorage.SynchronizeEventsWithRecords;
begin
  if not IsDataSettingsValid then
  begin
    if not IsLoading and not IsDestroying and DataController.DataLink.Active then
      DataController.Initialize;
    if not IsDataSettingsValid then
    begin
      Clear;
      Exit;
    end;
  end;
  DataController.SavePos(True);
  try
    if UseIndexedID then
      SynchronizeIndexedEvents
    else
      SynchronizeNoIndexedEvents;
  finally
    DataController.RestorePos;
  end;
end;

procedure TcxSchedulerDBStorage.SynchronizeIndexedEvents;
var
  ID: Variant;
  I, C: Integer;
  IndexList: TdxFastList;
  AIsNewEvent: Boolean;
  AEvent: TcxSchedulerEvent;
begin
  IndexList := FEventsList.List;
  IndexList.Sort(TListSortCompare(@cxCompareEventsByID));
  FEventsList.List := TdxFastList.Create;
  try
    I := IndexList.Count - 1;
    FEventsList.List.Capacity := IndexList.Count * 2;
    while not DataSet.Eof do
    begin
      AEvent := nil;
      ID := DataController.KeyField.Value;
      while I >= 0 do
      begin
        C := VarCompare(ID, TcxSchedulerEvent(IndexList.List[I]).ID);
        if C <= 0 then
        begin
          if C = 0 then
          begin
            AEvent := TcxSchedulerEvent(IndexList.List[I]);
            IndexList.List[I] := nil;
          end;
          Dec(I);
        end;
        if C >= 0 then Break;
      end;
      AIsNewEvent := AEvent = nil;
      if AIsNewEvent then
        AEvent := CreateEventInternal
      else
        FEventsList.List.Add(AEvent);
      DataController.UpdateEventValues(AEvent);
      if AIsNewEvent then
        SendNotification(AEvent, False, False);
      DataSet.Next;
    end;
    RemoveDeletedEvents(IndexList);
  finally
    IndexList.Free;
  end;
end;

procedure TcxSchedulerDBStorage.SynchronizeNoIndexedEvents;
var
  ID: Variant;
  IndexList: TdxFastList;
  AEvent: TcxSchedulerEvent;
  AIsNewEvent: Boolean;
begin
  IndexList := FEventsList.List;
  IndexList.Sort(TListSortCompare(@cxCompareEventsByID));
  FEventsList.List := TdxFastList.Create;
  try
    FEventsList.List.Capacity := IndexList.Count * 2;
    while not DataSet.Eof do
    begin
      ID := DataController.KeyField.Value;
      AIsNewEvent := not cxFindEvent(IndexList, ID, AEvent);
      if AIsNewEvent then
        AEvent := CreateEventInternal
      else
        FEventsList.List.Add(AEvent);
      DataController.UpdateEventValues(AEvent);
      if AIsNewEvent then
        SendNotification(AEvent, False, False);
      DataSet.Next;
    end;
    RemoveDeletedEvents(IndexList);
  finally
    IndexList.Free;
  end;
end;

procedure TcxSchedulerDBStorage.UpdateEventFieldValue(AEvent: TcxSchedulerEvent; AField: TField);
var
  I: Integer;
  AValue: Variant;
  AUpdateField: TcxSchedulerDBStorageField;
begin
  AValue := GetFieldValue(AField);
  for I := 0 to FieldCount - 1 do
  begin
    AUpdateField := TcxSchedulerDBStorageField(Fields[I]);
    if AUpdateField.Field = AField then
    begin
      AEvent.Values[AUpdateField.Index] := AValue;
      if AUpdateField.GetIsTextStored then
        TcxDataStorageHelper.SetDisplayText(AEvent.RecordHandle, AUpdateField.ValueDef, AField.DisplayText);
    end;
  end;
end;

function TcxSchedulerDBStorage.GetDataController: TcxSchedulerDBStorageDataController;
begin
  Result := TcxSchedulerDBStorageDataController(inherited DataController);
end;

function TcxSchedulerDBStorage.GetDataSource: TDataSource;
begin
  Result := DataController.DataLink.DataSource;
end;

function TcxSchedulerDBStorage.GetDataSet: TDataSet;
begin
  Result := DataController.DataSet;
end;

function TcxSchedulerDBStorage.GetCustomFields: TcxSchedulerDBStorageFields;
begin
  Result := TcxSchedulerDBStorageFields(inherited CustomFields);
end;

function TcxSchedulerDBStorage.GetResources: TcxSchedulerDBStorageResources;
begin
  Result := TcxSchedulerDBStorageResources(inherited Resources);
end;

procedure TcxSchedulerDBStorage.SetDataSource(
  AValue: TDataSource);
begin
  DataController.DataLink.DataSource := AValue;
end;

procedure TcxSchedulerDBStorage.SetCustomFields(
  const AValue: TcxSchedulerDBStorageFields);
begin
  CustomFields.Assign(AValue);
end;

procedure TcxSchedulerDBStorage.SetFieldNames(
  AValue: TcxSchedulerDBStorageFieldNames);
begin
  FFieldNames.Assign(AValue);
end;

procedure TcxSchedulerDBStorage.SetResources(
  AValue: TcxSchedulerDBStorageResources);
begin
  Resources.Assign(AValue);
end;

procedure TcxSchedulerDBStorage.SetSmartRefresh(AValue: Boolean);
begin
  if SmartRefresh <> AValue then
    FSmartRefresh := AValue;
end;

procedure TcxSchedulerDBStorage.SetUseIndexedID(AValue: Boolean);
var
  ADataSource: TDataSource;
begin
  if AValue <> FUseIndexedID then
  begin
    FUseIndexedID := AValue;
    ADataSource := DataSource;
    DataSource := nil;
    DataSource := ADataSource;
  end;
end;

end.


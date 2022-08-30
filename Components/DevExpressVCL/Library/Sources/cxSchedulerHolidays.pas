{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressDataController                                    }
{                                                                    }
{           Copyright (c) 1998-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSDATACONTROLLER AND ALL         }
{   ACCOMPANYING VCL CONTROLS AS PART OF AN EXECUTABLE PROGRAM ONLY. }
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

unit cxSchedulerHolidays;

{$I cxVer.inc}

interface

uses
  Messages, Windows, Variants, Classes, cxClasses, Controls, cxControls,
  cxDateUtils, cxSchedulerUtils, cxCustomData, SysUtils, cxStorage, cxSchedulerRecurrence;

type

  { TcxSchedulerLocationHoliday }

  TcxSchedulerHolidaysLocations = class;
  TcxSchedulerHolidaysLocation = class;
  TcxSchedulerHolidaysLocationHoliday = class;
  TcxSchedulerHolidayRecurrenceInfo = class;
  TcxSchedulerHolidays = class;
  TcxSchedulerHolidayCalculator = class;

  TcxGetHolidayRecurrenceDescriptionStringProc = function(
    ARecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo): string;

  TcxSchedulerHolidaysLocationHoliday = class(TcxInterfacedCollectionItem, IcxStoredObject)
  private
    FDate: TDateTime;
    FName: string;
    FRecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo;
    FRecurrenceData: AnsiString;
    FVisible: Boolean;

    procedure ReadRecurrenceData(AReader: TStream);
    procedure WriteRecurrenceData(AWriter: TStream);

    function GetIsVisible: Boolean;
    function GetLocation: TcxSchedulerHolidaysLocation;
    procedure SetDate(const AValue: TDateTime);
    procedure SetName(const AValue: string);
    procedure SetRecurrenceData(const AValue: AnsiString);
    procedure SetVisible(AValue: Boolean);
  protected
    function CreateRecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayText: string; virtual;

    function IsDateHoliday(const ADate: TDate; AOnlyVisible: Boolean): Boolean;
    function IsEqual(AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean;
    function IsHolidayInRange(const AStart, AFinish: TDate): Boolean;
    function IsRecurrenceDateHoliday(ADate: TDate): Boolean;
    function ToString: string; reintroduce; virtual;
    // IcxStoredObject
    function IcxStoredObject.GetObjectName = GetStoredObjectName;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    procedure IcxStoredObject.GetPropertyValue = GetStoredPropertyValue;
    procedure IcxStoredObject.SetPropertyValue = SetStoredPropertyValue;
    function GetStoredObjectName: string; virtual;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;

    property RecurrenceData: AnsiString read FRecurrenceData write SetRecurrenceData;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function IsRecurring: Boolean;
    procedure PopulateHolidayDates(AList: TcxSchedulerDateList;
      const AStart, AFinish: TDate; AOnlyVisible: Boolean = True);

    procedure RemoveRecurrence;

    property DisplayText: string read GetDisplayText;
    property IsVisible: Boolean read GetIsVisible;
    property Location: TcxSchedulerHolidaysLocation read GetLocation;
    property RecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo read FRecurrenceInfo;
  published
    property Date: TDateTime read FDate write SetDate;
    property Name: string read FName write SetName;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;
  TcxSchedulerHolidaysLocationHolidayClass = class of TcxSchedulerHolidaysLocationHoliday;

  { TcxSchedulerHolidaysLocation }

  TcxSchedulerHolidaysLocation = class(TcxInterfacedCollectionItem,
    IcxStoredParent,
    IcxStoredObject)
  private
    FHolidays: TCollection;
    FName: string;
    FVisible: Boolean;
    function GetCount: Integer;
    function GetHoliday(AIndex: Integer): TcxSchedulerHolidaysLocationHoliday;
    function GetLocations: TcxSchedulerHolidaysLocations;
    procedure SetHoliday(AIndex: Integer; AValue: TcxSchedulerHolidaysLocationHoliday);
    procedure SetName(const AValue: string);
    procedure SetVisible(AValue: Boolean);
    procedure ReadData(AReader: TReader); virtual;
    procedure WriteData(AWriter: TWriter); virtual;
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function IsEqual(ALocation: TcxSchedulerHolidaysLocation): Boolean;
    function ToString: string; reintroduce; virtual;
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    // IcxStoredObject
    function IcxStoredObject.GetObjectName = GetStoredObjectName;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    procedure IcxStoredObject.GetPropertyValue = GetStoredPropertyValue;
    procedure IcxStoredObject.SetPropertyValue = SetStoredPropertyValue;
    function GetStoredObjectName: string; virtual;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;

    property HolidaysList: TCollection read FHolidays;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function AddHoliday(const AName: string; const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday; overload; virtual;
    procedure Clear;
    procedure Delete(AIndex: Integer);
    procedure PopulateHolidayDates(AList: TcxSchedulerDateList; const AStart, AFinish: TDate; AOnlyVisible: Boolean = True);
    function Find(const AName: string; const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday; overload; virtual;
    procedure Sort(ASortOrder: TcxDataSortOrder; ASortByDate: Boolean);

    property Count: Integer read GetCount;
    property Holidays[AIndex: Integer]: TcxSchedulerHolidaysLocationHoliday read GetHoliday write SetHoliday; default;
    property Locations: TcxSchedulerHolidaysLocations read GetLocations;
  published
    property Name: string read FName write SetName;
    property Visible: Boolean read FVisible write SetVisible default False;
  end;
  TcxSchedulerHolidaysLocationClass = class of TcxSchedulerHolidaysLocation;

  { TcxSchedulerHolidaysLocations }

  TcxSchedulerHolidaysLocations = class(TCollection)
  private
    FOwner: TcxSchedulerHolidays;
    function GetItem(AIndex: Integer): TcxSchedulerHolidaysLocation;
    function GetOwnerHolidays: TcxSchedulerHolidays;
    procedure SetItem(AIndex: Integer; AValue: TcxSchedulerHolidaysLocation);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AOwner: TcxSchedulerHolidays); reintroduce; overload;
    destructor Destroy; override;

    procedure Assign(Source: TPersistent); override;
    function Add(const AName: string): TcxSchedulerHolidaysLocation;
    function GetLocationByName(const AName: string): TcxSchedulerHolidaysLocation;
    procedure Sort(ASortOrder: TcxDataSortOrder);

    property Items[AIndex: Integer]: TcxSchedulerHolidaysLocation read GetItem write SetItem; default;
    property OwnerHolidays: TcxSchedulerHolidays read GetOwnerHolidays;
  end;

  { IcxSchedulerHolidaysListener }

  IcxSchedulerHolidaysListener = interface
  ['{0FE58B1C-71C0-4ED0-9A10-12074CE13EA3}']
    procedure HolidaysChanged(Sender: TObject);
    procedure HolidaysRemoved(Sender: TObject);
  end;

  { TcxSchedulerHolidays }

  TcxSchedulerHolidaysImportExportHolidayEvent = procedure (ASender: TcxSchedulerHolidays;
    AHoliday: TcxSchedulerHolidaysLocationHoliday; var Accept: Boolean) of object;
  TcxSchedulerHolidaysImportUnknownDateEvent = procedure (ASender: TcxSchedulerHolidays;
    var AYear, AMonth, ADay: Word; const ATypeCalendar: Word; var Accept: Boolean) of object;

  TcxSchedulerHolidays = class(TcxCustomComponent,
    IcxStoredObject,
    IcxStoredParent)
  private
    FLocations: TcxSchedulerHolidaysLocations;
    FLockCount: Integer;
    FListeners: TInterfaceList;
    FStoringName: string;
    FOnExportHoliday: TcxSchedulerHolidaysImportExportHolidayEvent;
    FOnImportHoliday: TcxSchedulerHolidaysImportExportHolidayEvent;
    FOnImportUnknownDate: TcxSchedulerHolidaysImportUnknownDateEvent;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TcxSchedulerHolidaysLocationHoliday;
    function GetIsDestroying: Boolean;
    function GetIsLoading: Boolean;
    function GetIsUpdatingMode: Boolean;
    procedure GetHolidaysIndex(AIndex: Integer; out ALocationIndex, AHolidayIndex: Integer);
    function GetStringPart(const S: string; var APos: Integer; ACheckDateSeparator: Boolean = True): string;
    procedure SetLocations(const AValue: TcxSchedulerHolidaysLocations);
  protected
    procedure Changed; virtual;
    function DoExportHoliday(AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean; virtual;
    function DoImportHoliday(AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean; virtual;
    function DoImportUnknownDate(var AYear, AMonth, ADay: Word;
      const ATypeCalendar: Word): Boolean; virtual;
    procedure SendNotification(AIsRemoved: Boolean = False); virtual;
    function TryCreateFromStream(AStream: TStream;
      out ALocations: TcxSchedulerHolidaysLocations): Boolean; virtual;
    function TryStringToHoliday(const S: string; ALocation: TcxSchedulerHolidaysLocation): Boolean;
    function TryStringToLocation(const S: string; ALocations: TcxSchedulerHolidaysLocations): Boolean;
    function TryStrToIntW(const S: string; out AValue: Word): Boolean;

    // storing
    procedure RestoreFrom(const AStorageName: string; AStream: TStream;
      AReaderClass: TcxCustomReaderClass; const ARestoreName: string); virtual;
    procedure StoreTo(const AStorageName: string; AStream: TStream;
      AWriterClass: TcxCustomWriterClass; const ASaveName: string); virtual;
    function IsStoringNameMode: Boolean;

    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure IcxStoredParent.GetChildren = GetStoredChildren;
    procedure GetStoredChildren(AChildren: TStringList); virtual;
    // IcxStoredObject
    function IcxStoredObject.GetObjectName = GetStoredObjectName;
    function IcxStoredObject.GetProperties = GetStoredProperties;
    procedure IcxStoredObject.GetPropertyValue = GetStoredPropertyValue;
    procedure IcxStoredObject.SetPropertyValue = SetStoredPropertyValue;
    function GetStoredObjectName: string; virtual;
    function GetStoredProperties(AProperties: TStrings): Boolean; virtual;
    procedure GetStoredPropertyValue(const AName: string; var AValue: Variant); virtual;
    procedure SetStoredPropertyValue(const AName: string; const AValue: Variant); virtual;

    property IsDestroying: Boolean read GetIsDestroying;
    property IsLoading: Boolean read GetIsLoading;
    property IsUpdatingMode: Boolean read GetIsUpdatingMode;
    property Listeners: TInterfaceList read FListeners;
    property LockCount: Integer read FLockCount write FLockCount;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function AddHoliday(const ALocationName, AHolidayName: string; const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday;
    procedure AddListener(AListener: IcxSchedulerHolidaysListener);
    function AddLocation(const AName: string): TcxSchedulerHolidaysLocation; overload; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure BeginUpdate;
    procedure Clear; virtual;
    procedure EndUpdate;
    function GetHolidayNamesByDate(const ADate: TDate;
      var ANames: string; AOnlyVisible: Boolean = True): Boolean; virtual;
    procedure LoadFromFile(const AFileName: string); virtual;
    procedure LoadFromStream(AStream: TStream); virtual;
    procedure PopulateHolidayDates(AList: TcxSchedulerDateList; const AStart, AFinish: TDate;
      AOnlyVisible: Boolean = True; AClearList: Boolean = True); virtual;
    procedure RemoveListener(AListener: IcxSchedulerHolidaysListener);
    procedure SaveToFile(const AFileName: string); virtual;
    procedure SaveToStream(var AStream: TStream); virtual;

    // storing
    procedure RestoreFromIniFile(const AStorageName: string; const ARestoreName: string = '');
    procedure RestoreFromRegistry(const AStorageName: string; const ARestoreName: string = '');
    procedure RestoreFromStream(AStream: TStream; const ARestoreName: string = '');
    procedure RestoreFromStorage(const AStorageName: string; AReaderClass: TcxCustomReaderClass; const ARestoreName: string = '');
    procedure StoreToIniFile(const AStorageName: string; const ASaveName: string = '');
    procedure StoreToRegistry(const AStorageName: string; const ASaveName: string = '');
    procedure StoreToStream(AStream: TStream; const ASaveName: string = '');
    procedure StoreToStorage(const AStorageName: string; AWriterClass: TcxCustomWriterClass; const ASaveName: string = '');

    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TcxSchedulerHolidaysLocationHoliday read GetItem; default;
  published
    property Locations: TcxSchedulerHolidaysLocations read
      FLocations write SetLocations;
    property OnExportHoliday: TcxSchedulerHolidaysImportExportHolidayEvent
      read FOnExportHoliday write FOnExportHoliday;
    property OnImportHoliday: TcxSchedulerHolidaysImportExportHolidayEvent
      read FOnImportHoliday write FOnImportHoliday;
    property OnImportUnknownDate: TcxSchedulerHolidaysImportUnknownDateEvent
      read FOnImportUnknownDate write FOnImportUnknownDate;
  end;

  { TcxSchedulerHolidayCalculator }

  TcxSchedulerHolidayCalculator = class(TcxSchedulerRecurrenceCalculator)
  private
    function GetHoliday: TcxSchedulerHolidaysLocationHoliday;
  protected
    function GetDuration: TDateTime; override;
    function GetRecurrenceInfo: TcxSchedulerCustomRecurrenceInfo; override;
    function GetStart: TDateTime; override;
    function GetTimeBias: Double; override;
    procedure InitTimes; override;
  public
    function GetOccurrenceCount(AEndDate: TDateTime): Integer;

    property Holiday: TcxSchedulerHolidaysLocationHoliday read GetHoliday;
  end;

  { TcxSchedulerEvenHolidayRecurrenceInfo }

  TcxSchedulerHolidayRecurrenceInfo = class(TcxSchedulerCustomRecurrenceInfo)
  private
    function GetHoliday: TcxSchedulerHolidaysLocationHoliday;
  protected
    function GetFinish: TDateTime; override;
    function GetStart: TDateTime; override;
    procedure SetFinish(AValue: TDateTime); override;
    procedure SetStart(const AValue: TDateTime); override;

    function GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass; override;
    function GetData: TcxSchedulerCustomRecurrenceInfoData; override;
    function GetValue(var AValue: AnsiString): Boolean; override;
    procedure SetDataItem(AOffset: Pointer; ASize: Integer; const AValue); override;
    procedure SetValue(const AValue: AnsiString); override;
  public
    function GetValidStatus: TcxRecurrenceValidStatus;
    procedure Validate;

    property Holiday: TcxSchedulerHolidaysLocationHoliday read GetHoliday;
  end;

function cxGetHolidayRecurrenceDescriptionString(ARecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo): string;
function cxHolidayRecurrenceInfoDataToString(AHoliday: TcxSchedulerHolidaysLocationHoliday): AnsiString;
function cxHolidayStringToRecurrenceInfoData(const S: AnsiString): AnsiString;

const
  cxGetHolidayRecurrenceDescriptionStringProc: TcxGetHolidayRecurrenceDescriptionStringProc =
    cxGetHolidayRecurrenceDescriptionString;

implementation

uses
  DateUtils, cxSchedulerStrs, dxCore, cxSchedulerStorage;

type
  TcxSchedulerHolidaysCollection = class(TCollection)
  private
    FOwner: TPersistent;
  protected
    function GetOwner: TPersistent; override;
  public
    procedure Assign(Source: TPersistent); override;
  end;

const
  cxDateSeparator = '/';

  cxStartLocationName = '[';
  cxStopLocationName = ']';
  cxDateFormat = 'yyyy/MM/dd';

  cxSeparatorHolidayPart = ',';
  cxHolidaysFormat: TFormatSettings = (DateSeparator: cxDateSeparator; ShortDateFormat: 'yyyy/MM/dd');

function cxGetHolidayRecurrenceDescriptionString(ARecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo): string;

  function GetDateBounds: string;
    begin
      Result := ' effective ' + FormatDateTime('ddddd', ARecurrenceInfo.Start);
      if ARecurrenceInfo.Count >= 0 then
        Result := Result + ' until ' + FormatDateTime('ddddd', ARecurrenceInfo.GetEndDate);
    end;

begin
  Result := '';
  if not Assigned(ARecurrenceInfo) or not ARecurrenceInfo.Holiday.IsRecurring then
    Exit;
  Result := 'Occurs ' + cxGetCustomRecurrenceDescriptionString(ARecurrenceInfo) +
    GetDateBounds + '.';
end;

function cxHolidayRecurrenceInfoDataToString(AHoliday: TcxSchedulerHolidaysLocationHoliday): AnsiString;
begin
  Result := '';
  if AHoliday.IsRecurring then
  begin
    SetLength(Result, SizeOf(TcxSchedulerCustomRecurrenceInfoData));
    HexToBin(PAnsiChar(AHoliday.RecurrenceData), PAnsiChar(Result), Length(Result));
  end;
end;

function cxHolidayStringToRecurrenceInfoData(const S: AnsiString): AnsiString;
var
  I: Integer;
begin
  Result := '';
  for I := 1 to Length(S) do
    Result := Result + AnsiString(IntToHex(Ord(S[I]), 2));
end;

{ TcxSchedulerHolidaysCollection }

procedure TcxSchedulerHolidaysCollection.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerHolidaysCollection then
    with TcxSchedulerHolidaysCollection(Source) do
    begin
      Self.BeginUpdate;
      try
        Self.Clear;
        for I := 0 to Count - 1 do
          Self.Add.Assign(Items[I]);
      finally
        Self.EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerHolidaysCollection.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

function cxCompareHolidays(
  AItem1, AItem2: TcxSchedulerHolidaysLocationHoliday): Integer;
begin
  Result := AnsiCompareText(AItem1.Name, AItem2.Name)
end;

function cxCompareHolidaysByDate(
  AItem1, AItem2: TcxSchedulerHolidaysLocationHoliday): Integer;
begin
  Result := 0;
  if AItem1.Date <> AItem2.Date then
  begin
    if AItem1.Date - AItem2.Date > 0 then
      Result := 1
    else
      Result := -1;
  end;
end;

function cxCompareLocations(
  AItem1, AItem2: TcxSchedulerHolidaysLocation): Integer;
begin
  Result := AnsiCompareText(AItem1.Name, AItem2.Name);
end;

procedure SortCollection(ACollection: TCollection;
  ACompare: TListSortCompare; AIsAscending: Boolean);
var
  I, J, C: Integer;
begin
  for I := 0 to ACollection.Count - 1 do
    for J := I + 1 to ACollection.Count - 1 do
    begin
      C := ACompare(ACollection.Items[I], ACollection.Items[J]);
      if not AIsAscending then
        C := -C;
      if C > 0 then
        ACollection.Items[J].Index := I;
    end;
end;

{ TcxSchedulerHolidaysLocationHoliday }

constructor TcxSchedulerHolidaysLocationHoliday.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FVisible := True;
  FRecurrenceInfo := CreateRecurrenceInfo;
end;

destructor TcxSchedulerHolidaysLocationHoliday.Destroy;
begin
  FreeAndNil(FRecurrenceInfo);
  inherited Destroy;
end;

procedure TcxSchedulerHolidaysLocationHoliday.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerHolidaysLocationHoliday then
    with Source as TcxSchedulerHolidaysLocationHoliday do
    begin
      Self.Date := Date;
      Self.Name := Name;
      Self.Visible := Visible;
      Self.RecurrenceData := RecurrenceData;
      Self.RecurrenceInfo.Assign(RecurrenceInfo);
      Self.Changed(True);
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerHolidaysLocationHoliday.IsRecurring: Boolean;
begin
  Result := RecurrenceData <> '';
end;

procedure TcxSchedulerHolidaysLocationHoliday.PopulateHolidayDates(AList: TcxSchedulerDateList;
  const AStart, AFinish: TDate; AOnlyVisible: Boolean = True);
var
  AHolidayCalculator: TcxSchedulerHolidayCalculator;
begin
  if not Visible and AOnlyVisible then
    Exit;
  if IsRecurring then
  begin
    AHolidayCalculator := TcxSchedulerHolidayCalculator.Create(Self, AStart, AFinish);
    try
      while AHolidayCalculator.GetNextOccurrence do
        AList.Add(AHolidayCalculator.OccurrenceStart);
    finally
      AHolidayCalculator.Free;
    end;
  end
  else
    if IsHolidayInRange(AStart, AFinish) then AList.Add(Date);
end;

procedure TcxSchedulerHolidaysLocationHoliday.RemoveRecurrence;
begin
  RecurrenceData := '';
end;

function TcxSchedulerHolidaysLocationHoliday.CreateRecurrenceInfo: TcxSchedulerHolidayRecurrenceInfo;
begin
  Result := TcxSchedulerHolidayRecurrenceInfo.Create(Self);
end;

procedure TcxSchedulerHolidaysLocationHoliday.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineBinaryProperty('RecurrenceData', ReadRecurrenceData, WriteRecurrenceData, IsRecurring);
end;

function TcxSchedulerHolidaysLocationHoliday.GetDisplayText: string;
begin
  Result := Name;
  if Location <> nil then
    Result := Result + ' (' + Location.Name + ')';
end;

function TcxSchedulerHolidaysLocationHoliday.IsDateHoliday(
  const ADate: TDate; AOnlyVisible: Boolean): Boolean;
begin
  Result := (not AOnlyVisible or IsVisible) and ((ADate = Date) or IsRecurrenceDateHoliday(ADate)) and (Name <> '');
end;

function TcxSchedulerHolidaysLocationHoliday.IsEqual(
  AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean;
begin
  Result := (AHoliday.FDate = FDate) and (AHoliday.FVisible = FVisible) and
    (AHoliday.FName = FName);
end;

function TcxSchedulerHolidaysLocationHoliday.IsHolidayInRange(
  const AStart, AFinish: TDate): Boolean;
begin
  Result := (Date >= AStart) and (Date <= AFinish);
end;

function TcxSchedulerHolidaysLocationHoliday.IsRecurrenceDateHoliday(ADate: TDate): Boolean;
var
  AHolidayCalculator: TcxSchedulerHolidayCalculator;
begin
  Result := False;
  if IsRecurring then
  begin
    AHolidayCalculator := TcxSchedulerHolidayCalculator.Create(Self, ADate, ADate);
    try
      Result := AHolidayCalculator.GetNextOccurrence;
    finally
      AHolidayCalculator.Free;
    end;
  end;
end;

function TcxSchedulerHolidaysLocationHoliday.ToString: string;
begin
  Result := Name + ',' + cxDateToStr(Date, cxHolidaysFormat);
end;

function TcxSchedulerHolidaysLocationHoliday.GetStoredObjectName: string;
begin
  Result := GetNamePath;
end;

function TcxSchedulerHolidaysLocationHoliday.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
  AProperties.Add('Date');
  AProperties.Add('Name');
  AProperties.Add('Visible');
  AProperties.Add('RecurrenceData');
end;

procedure TcxSchedulerHolidaysLocationHoliday.GetStoredPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Date' then
    AValue := Date
  else
    if AName = 'Name' then
      AValue := Name
    else
      if AName = 'Visible' then
        AValue := Visible
      else
        if AName = 'RecurrenceData' then
          AValue := RecurrenceData;
end;

procedure TcxSchedulerHolidaysLocationHoliday.SetStoredPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Date' then
    Date := AValue
  else
    if AName = 'Name' then
      Name := AValue
    else
      if AName = 'Visible' then
        Visible := AValue
      else
        if AName = 'RecurrenceData' then
          RecurrenceData := AnsiString(AValue);
end;

procedure TcxSchedulerHolidaysLocationHoliday.ReadRecurrenceData(AReader: TStream);
var
  AData: TcxSchedulerCustomRecurrenceInfoData;
  S: AnsiString;
begin
  AReader.Read(AData, SizeOf(TcxSchedulerCustomRecurrenceInfoData));
  S := cxRecurrenceInfoDataToString(AData);
  RecurrenceInfo.SetValue(S);
end;

procedure TcxSchedulerHolidaysLocationHoliday.WriteRecurrenceData(AWriter: TStream);
var
  AData: TcxSchedulerCustomRecurrenceInfoData;
begin
  AData := RecurrenceInfo.GetData;
  AWriter.Write(AData, SizeOf(TcxSchedulerCustomRecurrenceInfoData));
end;

function TcxSchedulerHolidaysLocationHoliday.GetIsVisible: Boolean;
begin
  Result := Visible;
  if Location <> nil then
    Result := Result and Location.Visible;
end;

function TcxSchedulerHolidaysLocationHoliday.GetLocation: TcxSchedulerHolidaysLocation;
begin
  Result := TcxSchedulerHolidaysLocation(Collection.Owner);
end;

procedure TcxSchedulerHolidaysLocationHoliday.SetDate(const AValue: TDateTime);
begin
  if AValue <> FDate then
  begin
    FDate := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerHolidaysLocationHoliday.SetName(const AValue: string);
begin
  if AValue <> FName then
  begin
    FName := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerHolidaysLocationHoliday.SetRecurrenceData(const AValue: AnsiString);
begin
  if AValue <> FRecurrenceData then
  begin
    FRecurrenceData := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerHolidaysLocationHoliday.SetVisible(AValue: Boolean);
begin
  if AValue <> FVisible then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

{ TcxSchedulerHolidaysLocation }

constructor TcxSchedulerHolidaysLocation.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FHolidays := TcxSchedulerHolidaysCollection.Create(TcxSchedulerHolidaysLocationHoliday);
  TcxSchedulerHolidaysCollection(FHolidays).FOwner := Self;
  FVisible := False;
end;

destructor TcxSchedulerHolidaysLocation.Destroy;
begin
  FHolidays.Free;
  inherited Destroy;
end;

procedure TcxSchedulerHolidaysLocation.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerHolidaysLocation then
    with Source as TcxSchedulerHolidaysLocation do
    begin
      Self.Name := Name;
      Self.Visible := Visible;
      Self.HolidaysList.Assign(HolidaysList);
      Self.Changed(True);
    end
  else
    inherited Assign(Source);
end;

function TcxSchedulerHolidaysLocation.AddHoliday(const AName: string;
  const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday;
begin
  Result := HolidaysList.Add as TcxSchedulerHolidaysLocationHoliday;
  Result.Name := AName;
  Result.Date := ADate;
end;

procedure TcxSchedulerHolidaysLocation.Clear;
begin
  FHolidays.Clear;
  Changed(True);
end;

procedure TcxSchedulerHolidaysLocation.Delete(AIndex: Integer);
begin
  FHolidays.Items[AIndex].Free;
end;

procedure TcxSchedulerHolidaysLocation.PopulateHolidayDates(AList: TcxSchedulerDateList;
  const AStart, AFinish: TDate; AOnlyVisible: Boolean = True);
var
  I: Integer;
begin
  if not Visible and AOnlyVisible then Exit;
  for I := 0 to Count - 1 do
    Holidays[I].PopulateHolidayDates(AList, AStart, AFinish, AOnlyVisible);
end;

function TcxSchedulerHolidaysLocation.Find(
  const AName: string; const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Holidays[I];
    if (AnsiCompareText(Result.Name, AName) = 0) and (Result.Date = ADate) then
      Exit;
  end;
  Result := nil;
end;

procedure TcxSchedulerHolidaysLocation.Sort(
  ASortOrder: TcxDataSortOrder; ASortByDate: Boolean);
begin
  if ASortOrder <> soNone then
  begin
    if not ASortByDate then
      SortCollection(HolidaysList, @cxCompareHolidays, ASortOrder = soAscending)
    else
      SortCollection(HolidaysList, @cxCompareHolidaysByDate, ASortOrder = soAscending);
  end;
end;

procedure TcxSchedulerHolidaysLocation.DefineProperties(Filer: TFiler);

  function HasData: Boolean;
  begin
    if Filer.Ancestor <> nil then
      Result := not TcxSchedulerHolidaysLocation(Filer.Ancestor).IsEqual(Self)
    else
      Result := Count > 0;
  end;

begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('Holidays', ReadData, WriteData, HasData);
  if (FHolidays.Count = 0) and (Filer.Ancestor <> nil) then
    Assign(TcxSchedulerHolidaysLocation(Filer.Ancestor))
end;

function TcxSchedulerHolidaysLocation.IsEqual(
  ALocation: TcxSchedulerHolidaysLocation): Boolean;
var
  I: Integer;
begin
  Result := Count = ALocation.Count;
  if not Result then Exit;
  for I := 0 to Count - 1 do
  begin
    Result := Holidays[I].IsEqual(ALocation.Holidays[I]);
    if not Result then Break;
  end;
end;

function TcxSchedulerHolidaysLocation.ToString: string;
begin
  Result := cxStartLocationName + Name + cxStopLocationName;
end;

function TcxSchedulerHolidaysLocation.CreateChild(const AObjectName, AClassName: string): TObject;
begin
  Result := HolidaysList.Add;
end;

procedure TcxSchedulerHolidaysLocation.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

procedure TcxSchedulerHolidaysLocation.GetStoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to HolidaysList.Count - 1 do
    AChildren.AddObject(Holidays[I].GetStoredObjectName, Holidays[I])
end;

function TcxSchedulerHolidaysLocation.GetStoredObjectName: string;
begin
  Result := GetNamePath;
end;

function TcxSchedulerHolidaysLocation.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
  AProperties.Add('Name');
  AProperties.Add('Visible');
end;

procedure TcxSchedulerHolidaysLocation.GetStoredPropertyValue(const AName: string; var AValue: Variant);
begin
  if AName = 'Name' then
    AValue := Name
  else
    if AName = 'Visible' then
      AValue := Visible;
end;

procedure TcxSchedulerHolidaysLocation.SetStoredPropertyValue(const AName: string; const AValue: Variant);
begin
  if AName = 'Name' then
    Name := AValue
  else
    if AName = 'Visible' then
      Visible := AValue;
end;

function TcxSchedulerHolidaysLocation.GetCount: Integer;
begin
  Result := FHolidays.Count;
end;

function TcxSchedulerHolidaysLocation.GetHoliday(
  AIndex: Integer): TcxSchedulerHolidaysLocationHoliday;
begin
  Result := TcxSchedulerHolidaysLocationHoliday(FHolidays.Items[AIndex]);
end;

function TcxSchedulerHolidaysLocation.GetLocations: TcxSchedulerHolidaysLocations;
begin
  Result := TcxSchedulerHolidaysLocations(Collection);
end;

procedure TcxSchedulerHolidaysLocation.SetHoliday(AIndex: Integer;
  AValue: TcxSchedulerHolidaysLocationHoliday);
begin
  TcxSchedulerHolidaysLocationHoliday(FHolidays.Items[AIndex]).Assign(AValue);
end;

procedure TcxSchedulerHolidaysLocation.SetName(const AValue: string);
begin
  if FName <> AValue then
  begin
    FName := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerHolidaysLocation.SetVisible(AValue: Boolean);
begin
  if FVisible <> AValue then
  begin
    FVisible := AValue;
    Changed(True);
  end;
end;

procedure TcxSchedulerHolidaysLocation.ReadData(AReader: TReader);
begin
  if AReader.NextValue = vaCollection then
  begin
    FHolidays.Clear;
    AReader.ReadValue;
    AReader.ReadCollection(FHolidays);
  end;
end;

procedure TcxSchedulerHolidaysLocation.WriteData(AWriter: TWriter);
begin
  AWriter.WriteCollection(FHolidays);
end;

{ TcxSchedulerHolidaysLocations }

constructor TcxSchedulerHolidaysLocations.Create(AOwner: TcxSchedulerHolidays);
begin
  inherited Create(TcxSchedulerHolidaysLocation);
  FOwner := AOwner;
end;

destructor TcxSchedulerHolidaysLocations.Destroy;
begin
  Clear;
  inherited Destroy;
end;

procedure TcxSchedulerHolidaysLocations.Assign(Source: TPersistent);
var
  I: Integer;
begin
  if Source is TcxSchedulerHolidaysLocations then
  begin
    BeginUpdate;
    try
      Clear;
      for I := 0 to TcxSchedulerHolidaysLocations(Source).Count - 1 do
        Add(TcxSchedulerHolidaysLocations(Source).Items[I].Name).Assign(
          TcxSchedulerHolidaysLocations(Source).Items[I]);
    finally
      EndUpdate;
    end;
  end
  else
    inherited Assign(Source);
end;

function TcxSchedulerHolidaysLocations.Add(
  const AName: string): TcxSchedulerHolidaysLocation;
begin
  Result := TcxSchedulerHolidaysLocation.Create(Self);
  Result.Name := AName;
end;

function TcxSchedulerHolidaysLocations.GetLocationByName(
  const AName: string): TcxSchedulerHolidaysLocation;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
  begin
    Result := Items[I];
    if AnsiCompareText(Result.Name, AName) = 0 then
      Exit;
  end;
  Result := nil;
end;

procedure TcxSchedulerHolidaysLocations.Sort(ASortOrder: TcxDataSortOrder);
begin
  SortCollection(Self, @cxCompareLocations, ASortOrder = soAscending);
end;

function TcxSchedulerHolidaysLocations.GetOwner: TPersistent;
begin
  Result := FOwner;
end;

procedure TcxSchedulerHolidaysLocations.Update(Item: TCollectionItem);
begin
  if (OwnerHolidays <> nil) then
    OwnerHolidays.Changed;
end;

function TcxSchedulerHolidaysLocations.GetItem(
  AIndex: Integer): TcxSchedulerHolidaysLocation;
begin
  Result := TcxSchedulerHolidaysLocation(inherited Items[AIndex]);
end;

function TcxSchedulerHolidaysLocations.GetOwnerHolidays: TcxSchedulerHolidays;
begin
  Result := TcxSchedulerHolidays(GetOwner);
end;

procedure TcxSchedulerHolidaysLocations.SetItem(AIndex: Integer;
  AValue: TcxSchedulerHolidaysLocation);
begin
  Items[AIndex].Assign(AValue);
end;

{ TcxSchedulerHolidays }

constructor TcxSchedulerHolidays.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FLocations := TcxSchedulerHolidaysLocations.Create(Self);
  FListeners := TInterfaceList.Create();
end;

destructor TcxSchedulerHolidays.Destroy;
begin
  SendNotification(True);
  FreeAndNil(FLocations);
  FListeners.Free;
  inherited Destroy;
end;

procedure TcxSchedulerHolidays.Changed;
begin
  if LockCount = 0 then
    SendNotification();
end;

function TcxSchedulerHolidays.DoExportHoliday(
  AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean;
begin
  Result := True;
  if Assigned(FOnExportHoliday) then
    FOnExportHoliday(Self, AHoliday, Result);
end;

function TcxSchedulerHolidays.DoImportHoliday(
  AHoliday: TcxSchedulerHolidaysLocationHoliday): Boolean;
begin
  Result := True;
  if Assigned(FOnImportHoliday) then
    FOnImportHoliday(Self, AHoliday, Result);
end;

function TcxSchedulerHolidays.DoImportUnknownDate(var AYear, AMonth, ADay: Word;
  const ATypeCalendar: Word): Boolean;
begin
  Result := False;
  if Assigned(FOnImportUnknownDate) then
    FOnImportUnknownDate(Self, AYear, AMonth, ADay, ATypeCalendar, Result);
end;

function TcxSchedulerHolidays.AddHoliday(const ALocationName,
  AHolidayName: string; const ADate: TDateTime): TcxSchedulerHolidaysLocationHoliday;
var
  ALocation: TcxSchedulerHolidaysLocation;
begin
  Result := nil;
  ALocation := Locations.GetLocationByName(ALocationName);
  if  ALocation <> nil then
    Result := ALocation.AddHoliday(AHolidayName, ADate);
end;

procedure TcxSchedulerHolidays.AddListener(AListener: IcxSchedulerHolidaysListener);
begin
  if FListeners.IndexOf(AListener) = -1 then
    FListeners.Add(AListener);
end;

function TcxSchedulerHolidays.AddLocation(
  const AName: string): TcxSchedulerHolidaysLocation;
begin
  Result := FLocations.Add(AName);
end;

procedure TcxSchedulerHolidays.Assign(Source: TPersistent);
begin
  if Source is TcxSchedulerHolidays then
    with Source as TcxSchedulerHolidays do
    begin
      Self.BeginUpdate;
      try
        Self.Clear;
        Self.Locations := Locations;
      finally
        Self.EndUpdate;
      end;
    end
  else
    inherited Assign(Source);
end;

procedure TcxSchedulerHolidays.BeginUpdate;
begin
  Inc(FLockCount);
end;

procedure TcxSchedulerHolidays.Clear;
begin
  FLocations.Clear;
end;

procedure TcxSchedulerHolidays.EndUpdate;
begin
  Dec(FLockCount);
  Changed;
end;

function TcxSchedulerHolidays.GetHolidayNamesByDate(const ADate: TDate;
  var ANames: string; AOnlyVisible: Boolean = True): Boolean;
var
  I, J: Integer;
  ALocation: TcxSchedulerHolidaysLocation;
begin
  for I := 0 to Locations.Count - 1 do
  begin
    ALocation := Locations[I];
    if AOnlyVisible and not Locations[I].Visible then Continue;
    for J := 0 to ALocation.Count - 1 do
    begin
      if ALocation[J].IsDateHoliday(ADate, AOnlyVisible) then
      begin
        if Length(ANames) > 0 then
          ANames := ANames + dxCRLF;
        ANames := ANames + ALocation[J].DisplayText;
      end;
    end;
  end;
  Result := ANames <> '';
end;

procedure TcxSchedulerHolidays.LoadFromFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(AStream);
  finally
    FreeAndNil(AStream);
  end;
end;

procedure TcxSchedulerHolidays.LoadFromStream(AStream: TStream);
var
  ALocations: TcxSchedulerHolidaysLocations;
begin
  if TryCreateFromStream(AStream, ALocations) then
  try
    ShowHourglassCursor;
    try
      BeginUpdate;
      try
        Clear;
        Locations := ALocations;
      finally
        EndUpdate;
      end
    finally
      HideHourglassCursor;
    end;
  finally
    ALocations.Free;
  end
  else
    cxSchedulerError(cxGetResourceString(@scxOutlookFormatMismatch));
end;

procedure TcxSchedulerHolidays.PopulateHolidayDates(AList: TcxSchedulerDateList;
  const AStart, AFinish: TDate; AOnlyVisible: Boolean = True; AClearList: Boolean = True);
var
  I: Integer;
begin
  if AClearList then
    AList.Clear;
  for I := 0 to Locations.Count - 1 do
    Locations[I].PopulateHolidayDates(AList, AStart, AFinish, AOnlyVisible);
end;

procedure TcxSchedulerHolidays.RemoveListener(AListener: IcxSchedulerHolidaysListener);
begin
  FListeners.Remove(AListener);
end;

procedure TcxSchedulerHolidays.SaveToFile(const AFileName: string);
var
  AStream: TStream;
begin
  AStream := TFileStream.Create(AFileName, fmCreate or fmOpenWrite);
  try
    SaveToStream(AStream);
  finally
    AStream.Free;
  end;
end;

procedure TcxSchedulerHolidays.SaveToStream(var AStream: TStream);
var
  I, J: Integer;
  ALocation: TcxSchedulerHolidaysLocation;
  AExportList: TStringList;
  ADataAsString: WideString;
begin
  for I := 0 to Locations.Count - 1 do
  begin
    ALocation := Locations[I];
    AExportList := TStringList.Create;
    try
      for J := 0 to ALocation.Count - 1 do
        if DoExportHoliday(ALocation.Holidays[J]) then
          AExportList.Add(ALocation.Holidays[J].ToString);
      if AExportList.Count > 0 then
        ADataAsString := ADataAsString + ALocation.ToString + ' ' +
          IntToStr(AExportList.Count) + dxCRLF + AExportList.Text;
    finally
      AExportList.Free
    end;
  end;
  if Length(ADataAsString) > 0 then
  begin
    AStream.WriteBuffer(dxUnicodePrefix, SizeOf(dxUnicodePrefix));
    AStream.Write(ADataAsString[1], Length(ADataAsString) * 2);
  end;
end;

procedure TcxSchedulerHolidays.RestoreFromIniFile(const AStorageName: string; const ARestoreName: string = '');
begin
  RestoreFrom(AStorageName, nil, TcxIniFileReader, ARestoreName);
end;

procedure TcxSchedulerHolidays.RestoreFromRegistry(const AStorageName: string; const ARestoreName: string = '');
begin
  RestoreFrom(AStorageName, nil, TcxRegistryReader, ARestoreName);
end;

procedure TcxSchedulerHolidays.RestoreFromStream(AStream: TStream; const ARestoreName: string = '');
begin
  RestoreFrom('', AStream, TcxStreamReader, ARestoreName);
end;

procedure TcxSchedulerHolidays.RestoreFromStorage(const AStorageName: string; AReaderClass: TcxCustomReaderClass;
  const ARestoreName: string = '');
begin
  RestoreFrom(AStorageName, nil, AReaderClass, ARestoreName);
end;

procedure TcxSchedulerHolidays.StoreToIniFile(const AStorageName: string; const ASaveName: string = '');
begin
  StoreTo(AStorageName, nil, TcxIniFileWriter, ASaveName);
end;

procedure TcxSchedulerHolidays.StoreToRegistry(const AStorageName: string; const ASaveName: string = '');
begin
  StoreTo(AStorageName, nil, TcxRegistryWriter, ASaveName);
end;

procedure TcxSchedulerHolidays.StoreToStream(AStream: TStream; const ASaveName: string = '');
begin
  StoreTo('', AStream, TcxStreamWriter, ASaveName);
end;

procedure TcxSchedulerHolidays.StoreToStorage(const AStorageName: string; AWriterClass: TcxCustomWriterClass;
  const ASaveName: string = '');
begin
  StoreTo(AStorageName, nil, AWriterClass, ASaveName);
end;

procedure TcxSchedulerHolidays.SendNotification(AIsRemoved: Boolean = False);
var
  I: Integer;
  AIntf: IcxSchedulerHolidaysListener;
begin
  for I := Listeners.Count - 1 downto 0 do
    if Supports(Listeners[I], IcxSchedulerHolidaysListener, AIntf) then
    begin
      if AIsRemoved then
        AIntf.HolidaysRemoved(Self)
      else
        if not IsUpdatingMode then
          AIntf.HolidaysChanged(Self);
    end;
end;

function TcxSchedulerHolidays.TryCreateFromStream(AStream: TStream;
  out ALocations: TcxSchedulerHolidaysLocations): Boolean;
var
  I: Integer;
  AHolidaysList: TStringList;
begin
  Result := True;
  ALocations := TcxSchedulerHolidaysLocations.Create(nil);
  try
    AHolidaysList := TStringList.Create;
    try
      AHolidaysList.LoadFromStream(AStream);
      for I := 0 to AHolidaysList.Count - 1 do
        if TryStringToLocation(AHolidaysList[I], ALocations) or ((ALocations.Count > 0) and
          TryStringToHoliday(AHolidaysList[I], ALocations[ALocations.Count - 1])) then
        else
          Result := False;
    finally
      AHolidaysList.Free;
    end;
  except
    ALocations.Free;
    raise;
  end;
end;

function TcxSchedulerHolidays.TryStringToHoliday(
  const S: string; ALocation: TcxSchedulerHolidaysLocation): Boolean;
var
  ADate: TDate;
  APart, AName: string;
  AYear, AMonth, ADay: Word;
  ACalendar, ACurPos: Integer;
  AHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  Result := (ALocation <> nil) and (S <> '');
  if not Result then
  begin
    Result := True;
    Exit;
  end;
  ADate := NullDate;
  ACalendar := CAL_GREGORIAN;
  ACurPos := Length(S);
  APart := GetStringPart(S, ACurPos, False);
  if Pos(cxDateSeparator, APart) <= 0 then
  begin
    Result := Result and TryStrToInt(APart, ACalendar);
    APart := GetStringPart(S, ACurPos, False);
  end;
  AName := GetStringPart(S, ACurPos, False);
  if ACalendar in [CAL_GREGORIAN, CAL_HIJRI, CAL_HEBREW] then
    ADate := cxStrToDate(APart, cxHolidaysFormat, ACalendar)
  else
  begin
    ACurPos := Length(APart);
    if not TryStrToIntW(GetStringPart(APart, ACurPos), ADay) or
      not TryStrToIntW(GetStringPart(APart, ACurPos), AMonth) or
      not TryStrToIntW(GetStringPart(APart, ACurPos), AYear) then
        cxSchedulerError(cxGetResourceString(@scxOutlookFormatMismatch))
    else
    begin
      if DoImportUnknownDate(AYear, AMonth, ADay, ACalendar) then
        ADate := EncodeDate(AYear, AMonth, ADay)
    end;
  end;
  if Result then
  begin
    AHoliday := ALocation.AddHoliday(AName, ADate);
    if not DoImportHoliday(AHoliday) then
       AHoliday.Free;
  end;
end;

function TcxSchedulerHolidays.TryStringToLocation(
  const S: string; ALocations: TcxSchedulerHolidaysLocations): Boolean;
var
  AStartPos, AEndPos: Integer;
begin
  AStartPos := Pos(cxStartLocationName, S);
  AEndPos := Pos(cxStopLocationName, S);
  Result := (ALocations <> nil) and (AStartPos > 0) and (AEndPos > AStartPos);
  if Result then
    Result := ALocations.Add(Copy(S, AStartPos + 1, AEndPos - AStartPos - 1)) <> nil;
end;

function TcxSchedulerHolidays.TryStrToIntW(const S: string; out AValue: Word): Boolean;
var
  I: Integer;
begin
  Result := TryStrToInt(S, I);
  AValue := I;
end;

procedure TcxSchedulerHolidays.RestoreFrom(const AStorageName: string; AStream: TStream;
  AReaderClass: TcxCustomReaderClass; const ARestoreName: string);
var
  AStorage: TcxStorage;
begin
  FStoringName := ARestoreName;
  AStorage := TcxStorage.Create(AStorageName, AStream);
  try
    if not IsStoringNameMode then
      AStorage.NamePrefix := Name;
    AStorage.Modes := [smChildrenCreating, smChildrenDeleting];
    BeginUpdate;
    try
      AStorage.RestoreFrom(Self, AReaderClass);
    finally
      EndUpdate;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxSchedulerHolidays.StoreTo(const AStorageName: string; AStream: TStream;
  AWriterClass: TcxCustomWriterClass; const ASaveName: string);
var
  AStorage: TcxStorage;
begin
  FStoringName := ASaveName;
  AStorage := TcxStorage.Create(AStorageName, AStream);
  try
    if not IsStoringNameMode then
      AStorage.NamePrefix := Name;
    AStorage.ReCreate := True;
    AStorage.StoreTo(Self, AWriterClass);
  finally
    AStorage.Free;
  end;
end;

function TcxSchedulerHolidays.IsStoringNameMode: Boolean;
begin
  Result := FStoringName <> '';
end;

function TcxSchedulerHolidays.CreateChild(const AObjectName, AClassName: string): TObject;
begin
  Result := TCollection(Locations).Add;
end;

procedure TcxSchedulerHolidays.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  AObject.Free;
end;

procedure TcxSchedulerHolidays.GetStoredChildren(AChildren: TStringList);
var
  I: Integer;
begin
  for I := 0 to Locations.Count - 1 do
    AChildren.AddObject(Locations[I].GetStoredObjectName, Locations[I]);
end;

function TcxSchedulerHolidays.GetStoredObjectName: string;
begin
  if FStoringName <> '' then
    Result := FStoringName
  else
    Result := Name;
end;

function TcxSchedulerHolidays.GetStoredProperties(AProperties: TStrings): Boolean;
begin
  Result := True;
end;

procedure TcxSchedulerHolidays.GetStoredPropertyValue(const AName: string; var AValue: Variant);
begin
// do nothing
end;

procedure TcxSchedulerHolidays.SetStoredPropertyValue(const AName: string; const AValue: Variant);
begin
// do nothing
end;

function TcxSchedulerHolidays.GetCount: Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to Locations.Count - 1 do
    Inc(Result, Locations[I].Count);
end;

function TcxSchedulerHolidays.GetItem(AIndex: Integer): TcxSchedulerHolidaysLocationHoliday;
var
  I, J: Integer;
begin
  GetHolidaysIndex(AIndex, I, J);
  Result := Locations[I].Holidays[J];
end;

function TcxSchedulerHolidays.GetIsDestroying: Boolean;
begin
  Result := csDestroying in ComponentState;
end;

function TcxSchedulerHolidays.GetIsLoading: Boolean;
begin
  Result := csLoading in ComponentState;
end;

function TcxSchedulerHolidays.GetIsUpdatingMode: Boolean;
begin
  Result := IsDestroying or IsLoading or (LockCount > 0);
end;

procedure TcxSchedulerHolidays.GetHolidaysIndex(AIndex: Integer;
  out ALocationIndex, AHolidayIndex: Integer);
var
  I: Integer;
  ACount: Integer;
begin
  ACount := 0;
  for I := 0 to Locations.Count - 1 do
  begin
    if AIndex < (ACount + Locations[I].Count) then
    begin
      ALocationIndex := I;
      AHolidayIndex := AIndex - ACount;
      Break;
    end
    else
      Inc(ACount, Locations[I].Count);
  end;
end;

function TcxSchedulerHolidays.GetStringPart(const S: string;
  var APos: Integer; ACheckDateSeparator: Boolean = True): string;
var
  L: Integer;
  ASeparators: TdxAnsiCharSet;
begin
  Result := '';
  if ACheckDateSeparator then
    ASeparators := [cxDateSeparator, cxSeparatorHolidayPart]
  else
    ASeparators := [cxSeparatorHolidayPart];
  if (APos > Length(S)) or (APos <= 0) then Exit;
  while (APos > 0) and dxCharInSet(S[APos], ASeparators) or (S[APos] = ' ') do Dec(APos);
  L := APos;
  while (APos > 0) and not dxCharInSet(S[APos], ASeparators) do Dec(APos);
  if APos < L then
  begin
    Result := Copy(S, APos + 1, L - APos);
    Dec(APos);
  end;
end;

procedure TcxSchedulerHolidays.SetLocations(
  const AValue: TcxSchedulerHolidaysLocations);
begin
  BeginUpdate;
  try
    FLocations.Assign(AValue);
  finally
    EndUpdate;
  end;
end;

{ TcxSchedulerHolidayCalculator }

function TcxSchedulerHolidayCalculator.GetOccurrenceCount(
  AEndDate: TDateTime): Integer;
begin
  Result := 0;
  InitTimes;
  while GetNextOccurrence and (dxDateOf(FOccurrenceStart) <= dxDateOf(AEndDate)) do
    Inc(Result);
end;

function TcxSchedulerHolidayCalculator.GetDuration: TDateTime;
begin
  Result := 0;
end;

function TcxSchedulerHolidayCalculator.GetRecurrenceInfo: TcxSchedulerCustomRecurrenceInfo;
begin
  Result := Holiday.RecurrenceInfo;
end;

function TcxSchedulerHolidayCalculator.GetStart: TDateTime;
begin
  Result := Holiday.Date;
end;

function TcxSchedulerHolidayCalculator.GetTimeBias: Double;
begin
  Result := 0;
end;

procedure TcxSchedulerHolidayCalculator.InitTimes;
begin
  inherited;
  FActualStart := FOccurrenceStart;
  FIndex := -1;
end;

function TcxSchedulerHolidayCalculator.GetHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  Result := TcxSchedulerHolidaysLocationHoliday(Owner);
end;

{ TcxSchedulerHolidayRecurrenceInfo }

function TcxSchedulerHolidayRecurrenceInfo.GetValidStatus: TcxRecurrenceValidStatus;

  function IntersectOccurrences(APrevFinish, AStart: TDateTime): Boolean;
  begin
      Result := dxDateOf(AStart) < dxDateOf(APrevFinish)
  end;

var
  AEdge, AFinish: TDateTime;
begin
  if not Holiday.IsRecurring then
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
  with TcxSchedulerHolidayCalculator.Create(Holiday,
    Holiday.RecurrenceInfo.Start, cxMaxDate) do
  try
    // find a first occurrence
    if GetNextOccurrence then
    begin
      if Holiday.RecurrenceInfo.Count = 1 then Exit;
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

procedure TcxSchedulerHolidayRecurrenceInfo.Validate;
begin
  with TcxSchedulerHolidayCalculator.Create(Holiday,
    Holiday.RecurrenceInfo.Start, cxMaxDate) do
  try
    if GetNextOccurrence then
      Holiday.Date := OccurrenceStart
    else
      cxSchedulerError(cxGetResourceString(@scxWrongPattern));
  finally
    Free;
  end;
end;

function TcxSchedulerHolidayRecurrenceInfo.GetFinish: TDateTime;
begin
  Result := GetData.Finish;
end;

function TcxSchedulerHolidayRecurrenceInfo.GetStart: TDateTime;
begin
  Result := dxDateOf(Holiday.Date);
end;

procedure TcxSchedulerHolidayRecurrenceInfo.SetFinish(
  AValue: TDateTime);
begin
  AValue := DateTimeHelper.RoundTime(AValue);
  SetDataItem(@DefInfoData.Finish, SizeOf(AValue), AValue);
end;

procedure TcxSchedulerHolidayRecurrenceInfo.SetStart(
  const AValue: TDateTime);
begin
  Holiday.Date := AValue;
end;

function TcxSchedulerHolidayRecurrenceInfo.GetCalculatorClass: TcxSchedulerRecurrenceCalculatorClass;
begin
  Result := TcxSchedulerHolidayCalculator;
end;

function TcxSchedulerHolidayRecurrenceInfo.GetData: TcxSchedulerCustomRecurrenceInfoData;
var
  S: AnsiString;
begin
  if GetValue(S) then
    Result := cxStringToRecurrenceInfoData(S)
  else
    Result := DefInfoData;
end;

function TcxSchedulerHolidayRecurrenceInfo.GetValue(
  var AValue: AnsiString): Boolean;
begin
  Result := Holiday.IsRecurring;
  if Result then
    AValue := cxHolidayRecurrenceInfoDataToString(Holiday);
end;

procedure TcxSchedulerHolidayRecurrenceInfo.SetDataItem(
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

procedure TcxSchedulerHolidayRecurrenceInfo.SetValue(
  const AValue: AnsiString);
begin
  Holiday.RecurrenceData := cxHolidayStringToRecurrenceInfoData(AValue);
end;

function TcxSchedulerHolidayRecurrenceInfo.GetHoliday: TcxSchedulerHolidaysLocationHoliday;
begin
  Result := TcxSchedulerHolidaysLocationHoliday(Owner);
end;

end.

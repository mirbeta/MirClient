{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEditors                                           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSEDITORS AND ALL                }
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

unit cxDBLookupEdit;

{$I cxVer.inc}

interface

uses
  Variants, Messages, Controls, SysUtils, Classes, DB, cxClasses, cxContainer,
  cxEdit, cxDBEdit, cxEditConsts, cxDB, cxDataUtils, cxDataStorage, cxCustomData,
  cxDBData, cxDropDownEdit, cxLookupEdit, dxCoreClasses;

type
  TcxCustomDBLookupEditProperties = class;

  { TcxCustomDBLookupEditLookupData }

  TcxCustomDBLookupEditLookupData = class(TcxCustomLookupEditLookupData)
  private
    function GetDataController: TcxDBDataController;
    function GetProperties: TcxCustomDBLookupEditProperties;
  protected
    procedure DoSetCurrentKey(ARecordIndex: Integer); override;
    procedure DoSyncGrid; override;
    property DataController: TcxDBDataController read GetDataController;
    property Properties: TcxCustomDBLookupEditProperties read GetProperties;
  end;

  { TcxCustomDBLookupEditProperties }

  TcxCustomDBLookupEditProperties = class(TcxCustomLookupEditProperties)
  private
    FCachedLookupSource: TDataSource;
    FCaseSensitiveSearch: Boolean;
    FLockGridModeCount: Integer;
    FLookupField: TField;
    FLookupList: TcxLookupList;
    FLookupSource: TDataSource;
    FLookupSourceFreeNotificator: TcxFreeNotificator;
    FSyncLookup: Boolean;
    function GetIsUseLookupList: Boolean;
    function GetKeyFieldNames: string;
    function GetListField: TField;
    function GetListFieldIndex: Integer;
    function GetListFieldNames: string;
    procedure SetIsUseLookupList(Value: Boolean);
    procedure SetKeyFieldNames(const Value: string);
    procedure SetListFieldIndex(Value: Integer);
    procedure SetListFieldNames(const Value: string);
  protected
    // DBLookupGrid methods
    procedure DBLookupGridBeginUpdate; virtual;
    procedure DBLookupGridCheckColumnByFieldName(const AFieldName: string); virtual; // if a column does not exist, then create it with zero index
    procedure DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string); virtual;
    procedure DBLookupGridEndUpdate; virtual;
    function GetDBLookupGridColumnField(AIndex: Integer): TField; virtual;
    function GetDBLookupGridColumnFieldName(AIndex: Integer): string; virtual;
    function GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer; virtual;
    function GetDBLookupGridDataController: TcxDBDataController; virtual;

    function CanDisplayArbitraryEditValue: Boolean;
    procedure CheckLookup; virtual;
    procedure CheckLookupColumn; virtual;
    procedure CheckLookupList;
    procedure DefaultValuesProviderDestroyed; override;
    procedure DefineByLookupError;
    procedure DoChanged; override;
    function FindByText(AItemIndex: Integer; const AText: string; APartialCompare: Boolean): Integer; override;
    function GetDisplayColumnIndex: Integer; override;
    function GetDisplayLookupText(const AKey: TcxEditValue): string; override;
    function GetDefaultHorzAlignment: TAlignment; override;
    function GetDefaultMaxLength: Integer; override;
    function GetIncrementalFiltering: Boolean; override;
    function GetKeyByRecordIndex(ARecordIndex: Integer): Variant;
    class function GetLookupDataClass: TcxInterfacedPersistentClass; override;
    function GetLookupResultFieldName: string;
    function GetNullKey: Variant; override;
    function GetRecordIndexByKey(const AKey: Variant): Integer;
    function IsPickMode: Boolean; override;
    procedure LockDataChanged; override;
    procedure LookupSourceFreeNotification(Sender: TComponent); virtual;
    procedure SetDisplayColumnIndex(Value: Integer); override;
    procedure SetLookupField(ALookupField: TField);
    procedure UnlockDataChanged; override;
    property InSyncLookup: Boolean read FSyncLookup;
    property IsUseLookupList: Boolean read GetIsUseLookupList write SetIsUseLookupList;
  public
    destructor Destroy; override;
    function AllowRepositorySharing: Boolean; override;
    procedure Assign(Source: TPersistent); override;
    class function GetContainerClass: TcxContainerClass; override;
    function GetDataField: TField;
    function GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource; override;
    function GetLookupField: TField;
    function IsLookupField: Boolean; override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
    procedure RefreshNonShareable; override;

    property CaseSensitiveSearch: Boolean read FCaseSensitiveSearch
      write FCaseSensitiveSearch default False;
    property DataController: TcxDBDataController read GetDBLookupGridDataController;
    property KeyFieldNames: string read GetKeyFieldNames write SetKeyFieldNames;
    property ListField: TField read GetListField;
    property ListFieldNames: string read GetListFieldNames write SetListFieldNames stored False;
    property ListFieldIndex: Integer read GetListFieldIndex write SetListFieldIndex default 0;
  end;

  { TcxCustomDBLookupEdit }

  TcxCustomDBLookupEdit = class(TcxCustomLookupEdit)
  private
    function GetProperties: TcxCustomDBLookupEditProperties;
    function GetActiveProperties: TcxCustomDBLookupEditProperties;
    procedure SetProperties(Value: TcxCustomDBLookupEditProperties);
  protected
    function GetClearValue: TcxEditValue; override;
    function IsValidChar(AChar: Char): Boolean; override;
    function ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue; override;
    function LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue; override;
    function LookupKeyToItemIndex(const AKey: TcxEditValue): Integer; override;
    procedure PopupWindowBeforeClosing(Sender: TObject); override;
    procedure PopupWindowClosed(Sender: TObject); override;
    procedure PrepareDisplayValue(const AEditValue: TcxEditValue;
      var DisplayValue: TcxEditValue; AEditFocused: Boolean); override;
  public
    class function GetPropertiesClass: TcxCustomEditPropertiesClass; override;
    property ActiveProperties: TcxCustomDBLookupEditProperties
      read GetActiveProperties;
    property Properties: TcxCustomDBLookupEditProperties read GetProperties
      write SetProperties;
  end;

  { TcxDBLookupEditDataBinding }

  TcxDBLookupEditDataBinding = class(TcxDBTextEditDataBinding)
  protected
    function IsLookupControl: Boolean; override;
  end;

function IsLinkedToDataSet(ADataSource: TDataSource{list}; ADataSet: TDataSet{data binding}): Boolean;

implementation

uses
  VDBConsts, Contnrs, DBConsts, cxVariants;

function IsLinkedToDataSet(ADataSource: TDataSource{list}; ADataSet: TDataSet{data binding}): Boolean;
var
  AListDataSet: TDataSet;
begin
  AListDataSet := ADataSource.DataSet;
  Result := True;
  while ADataSet <> nil do
  begin
    if ADataSet = AListDataSet then Exit;
    if (ADataSet.DataSetField <> nil) and
       (ADataSet.DataSetField.DataSet = AListDataSet) then Exit;
    if ADataSet.DataSource = nil then
      Break
    else
      ADataSet := ADataSet.DataSource.DataSet;
  end;
  Result := False;
end;

{ TcxCustomDBLookupEditLookupData }

procedure TcxCustomDBLookupEditLookupData.DoSetCurrentKey(ARecordIndex: Integer);
begin
  FCurrentKey := Properties.GetKeyByRecordIndex(ARecordIndex);
end;

procedure TcxCustomDBLookupEditLookupData.DoSyncGrid;
begin
  if (DataController <> nil) and DataController.Active then
    try
      Properties.LockDataChanged;
      try
        DataController.LocateByKey(GetCurrentKey);
      finally
        Properties.UnlockDataChanged;
      end;
    except
      on EVariantError do;
      on EDatabaseError do;
    end;
end;

function TcxCustomDBLookupEditLookupData.GetDataController: TcxDBDataController;
begin
  Result := Properties.DataController;
end;

function TcxCustomDBLookupEditLookupData.GetProperties: TcxCustomDBLookupEditProperties;
begin
  Result := TcxCustomDBLookupEditProperties(inherited Properties);
end;

{ TcxCustomDBLookupEditProperties }

destructor TcxCustomDBLookupEditProperties.Destroy;
begin
  SetLookupField(nil);
  FreeAndNil(FLookupSourceFreeNotificator);
  FLookupList.Free;
  FLookupList := nil;
  FreeAndNil(FCachedLookupSource);
  inherited Destroy;
end;

function TcxCustomDBLookupEditProperties.AllowRepositorySharing: Boolean;
begin
  Result := False;
end;

procedure TcxCustomDBLookupEditProperties.Assign(Source: TPersistent);
begin
  if Source is TcxCustomDBLookupEditProperties then
  begin
    BeginUpdate;
    try
      inherited Assign(Source);
      CaseSensitiveSearch := TcxCustomDBLookupEditProperties(Source).CaseSensitiveSearch;
      if not IsDefinedByLookup then
        KeyFieldNames := TcxCustomDBLookupEditProperties(Source).KeyFieldNames;
    finally
      EndUpdate;
    end
  end
  else
    inherited Assign(Source);
end;

class function TcxCustomDBLookupEditProperties.GetContainerClass: TcxContainerClass;
begin
  Result := TcxCustomDBLookupEdit;
end;

function TcxCustomDBLookupEditProperties.GetDataField: TField;
var
  ADefaultValuesProvider: TcxCustomEditDefaultValuesProvider;
begin
  Result := nil;
  if IDefaultValuesProvider <> nil then
  begin
    ADefaultValuesProvider := TcxCustomEditDefaultValuesProvider(IDefaultValuesProvider.GetInstance);
    if ADefaultValuesProvider is TcxCustomDBEditDefaultValuesProvider then
      Result := TcxCustomDBEditDefaultValuesProvider(ADefaultValuesProvider).Field;
  end;
end;

function TcxCustomDBLookupEditProperties.GetEditValueSource(AEditFocused: Boolean): TcxDataEditValueSource;
begin
  if GetLookupField <> nil then
  begin
    if AEditFocused then
      Result := evsKey
    else
      Result := evsText;
  end
  else
    Result := inherited GetEditValueSource(AEditFocused);
end;

function TcxCustomDBLookupEditProperties.GetLookupField: TField;
begin
  Result := GetDataField;
  if (Result <> nil) and (not Result.Lookup or (csDestroying in Result.ComponentState)) then
    Result := nil;
end;

function TcxCustomDBLookupEditProperties.IsLookupField: Boolean;
begin
  Result := GetLookupField <> nil;
end;

procedure TcxCustomDBLookupEditProperties.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  if CanDisplayArbitraryEditValue and (DropDownListStyle <> lsEditList) and
    not AEditFocused then
      DisplayValue := VarToStr(AEditValue)
  else
    inherited PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
end;

procedure TcxCustomDBLookupEditProperties.RefreshNonShareable;
begin
  CheckLookupList;
  CheckLookup;
  CheckLookupColumn;
end;

procedure TcxCustomDBLookupEditProperties.DBLookupGridBeginUpdate;
begin
end;

procedure TcxCustomDBLookupEditProperties.DBLookupGridCheckColumnByFieldName(const AFieldName: string);
begin
end;

procedure TcxCustomDBLookupEditProperties.DBLookupGridCreateColumnsByFieldNames(const AFieldNames: string);
begin
end;

procedure TcxCustomDBLookupEditProperties.DBLookupGridEndUpdate;
begin
end;

function TcxCustomDBLookupEditProperties.GetDBLookupGridColumnField(AIndex: Integer): TField;
begin
  Result := nil;
end;

function TcxCustomDBLookupEditProperties.GetDBLookupGridColumnFieldName(AIndex: Integer): string;
begin
  Result := '';
end;

function TcxCustomDBLookupEditProperties.GetDBLookupGridColumnIndexByFieldName(const AFieldName: string): Integer;
begin
  Result := -1;
end;

function TcxCustomDBLookupEditProperties.GetDBLookupGridDataController: TcxDBDataController;
begin
  Result := nil;
end;

function TcxCustomDBLookupEditProperties.CanDisplayArbitraryEditValue: Boolean;
var
  AKeyField: TField;
begin
  Result := False; // TODO: method in DataController?
  if (KeyFieldNames <> '') and not IsMultipleFieldNames(KeyFieldNames) and
    (DataController <> nil) and (DataController.DataSet <> nil) then
  begin
    AKeyField := DataController.DataSet.FindField(KeyFieldNames);
    if AKeyField <> nil then
      Result := (AKeyField = GetListField) and
        ((DropDownListStyle = lsEditList) or (AKeyField is TStringField));
  end;
end;

procedure TcxCustomDBLookupEditProperties.CheckLookup;

  procedure CheckListSource;
  var
    AField: TField;
  begin
    AField := GetDataField;
    if Assigned(AField) and Assigned(DataController) and Assigned(DataController.DataSource) and
//      AField.DataSet.IsLinkedTo(DataController.DataSource) then
      IsLinkedToDataSet(DataController.DataSource, AField.DataSet) then
      DatabaseError(SCircularDataLink);
  end;

begin
  SetLookupField(GetLookupField);
  CheckListSource;
end;

procedure TcxCustomDBLookupEditProperties.CheckLookupColumn;
var
  AFieldName: string;
begin
  AFieldName := GetLookupResultFieldName;
  if AFieldName <> '' then
    DBLookupGridCheckColumnByFieldName(AFieldName);
end;

procedure TcxCustomDBLookupEditProperties.CheckLookupList;
begin
  if FLookupList <> nil then
    FLookupList.Clear;
  if (DataController <> nil) then
    DataController.DataModeController.GridMode := IsUseLookupList;
end;

procedure TcxCustomDBLookupEditProperties.DefaultValuesProviderDestroyed;
begin
  inherited DefaultValuesProviderDestroyed;
  BeginUpdate;
  try
    Changed;
  finally
    EndUpdate(False);
  end;
end;

procedure TcxCustomDBLookupEditProperties.DefineByLookupError;
begin
  DatabaseError(SPropDefByLookup);
end;

procedure TcxCustomDBLookupEditProperties.DoChanged;
begin
  RefreshNonShareable;
  inherited DoChanged;
end;

function TcxCustomDBLookupEditProperties.IsPickMode: Boolean;
begin
  Result := (DropDownListStyle = lsEditList) and CanDisplayArbitraryEditValue;
end;

procedure TcxCustomDBLookupEditProperties.LockDataChanged;
begin
  inherited LockDataChanged;
  // TODO: if GridMode
  if (DataController <> nil) and DataController.IsGridMode then
    Inc(FLockGridModeCount);
  if FLockGridModeCount <> 0 then
    DataController.LockGridModeNotify;
end;

procedure TcxCustomDBLookupEditProperties.LookupSourceFreeNotification(Sender: TComponent);
begin
  CheckLookup;
end;

procedure TcxCustomDBLookupEditProperties.SetDisplayColumnIndex(Value: Integer);
begin
  if IsDefinedByLookup and not FSyncLookup then
    DefineByLookupError;
  inherited SetDisplayColumnIndex(Value);
end;

procedure TcxCustomDBLookupEditProperties.SetLookupField(ALookupField: TField);
begin
  if FLookupField <> ALookupField then
  begin
    FLookupField := ALookupField;
    if FLookupField <> nil then
    begin
      // Lookup Source
      if FCachedLookupSource = nil then
        FCachedLookupSource := TDataSource.Create(nil);
      FLookupSource := FCachedLookupSource;

      FreeAndNil(FLookupSourceFreeNotificator);
      FLookupSourceFreeNotificator := TcxFreeNotificator.Create(nil);
      FLookupSourceFreeNotificator.OnFreeNotification := LookupSourceFreeNotification;
      FLookupSourceFreeNotificator.AddSender(FLookupField);

      FLookupSource.DataSet := FLookupField.LookupDataSet;
      // Sync Lookup Data
      if DataController <> nil then
      begin
        FSyncLookup := True;
        try
//          if DataController.DataSet <> FLookupSource.DataSet then // SC DB11573
            DataController.DataSource := FLookupSource;
          if GetDBLookupGridColumnIndexByFieldName(FLookupField.LookupResultField) = -1 then
            ListFieldNames := FLookupField.LookupResultField;
          KeyFieldNames := FLookupField.LookupKeyFields;
          ListFieldIndex := 0;
        finally
          FSyncLookup := False;
        end;
      end;
    end
    else
    begin
      FreeAndNil(FLookupSourceFreeNotificator);
      FLookupSource := nil;
      if (DataController <> nil) and (DataController.DataSource = FCachedLookupSource) then
        DataController.DataSource := nil;
    end;
  end;
end;

procedure TcxCustomDBLookupEditProperties.UnlockDataChanged;
begin
  if FLockGridModeCount <> 0 then
  begin
    if DataController <> nil then
      DataController.UnlockGridModeNotify;
    Dec(FLockGridModeCount);
  end;
  inherited UnlockDataChanged;
end;

function TcxCustomDBLookupEditProperties.FindByText(AItemIndex: Integer;
  const AText: string; APartialCompare: Boolean): Integer;

  function GetLocateOptions: TLocateOptions;
  begin
    Result := [];
    if not CaseSensitiveSearch then
      Include(Result, loCaseInsensitive);
    if APartialCompare then
      Result := Result + [loPartialKey];
  end;

  function GetLocateValue: Variant;
  begin
    Result := AText;
    // TDataSet.Locate does not work with empty strings passed as key values for numeric fields
    if (AText = '') and not (DataController.GetItemField(AItemIndex) is TStringField) then
      Result := Null;
  end;

var
  ADataSet: TDataSet;
  AListFieldName: string;
begin
  if not IsUseLookupList then
  begin
    Result := inherited FindByText(AItemIndex, AText, APartialCompare);
    Exit;
  end;

  Result := -1;
  LockDataChanged;
  try
    ADataSet := DataController.DataSet;
    AListFieldName := DataController.GetItemFieldName(AItemIndex);
    try
      if (ADataSet <> nil) and ADataSet.Active and (AItemIndex <> -1) and
        ADataSet.Locate(AListFieldName, GetLocateValue, GetLocateOptions) then
          Result := DataController.GetFocusedRecordIndex;
    except
      on EDatabaseError do;
      on EVariantError do;
    end;
  finally
    UnlockDataChanged;
  end;
end;

function TcxCustomDBLookupEditProperties.GetDisplayColumnIndex: Integer;
var
  AFieldName: string;
begin
  AFieldName := GetLookupResultFieldName;
  if AFieldName <> '' then
    Result := GetDBLookupGridColumnIndexByFieldName(AFieldName)
  else
    Result := inherited GetDisplayColumnIndex;
end;

function TcxCustomDBLookupEditProperties.GetDisplayLookupText(const AKey: TcxEditValue): string;
var
  ARecordIndex: Integer;
  AItemIndex: Integer;
  ADataSet: TDataSet;
  AProperties: TcxCustomEditProperties;
begin
  Result := '';
  AItemIndex := GetListIndex;
  if (AItemIndex <> -1) and (DataController <> nil) then
  begin
    if IsUseLookupList then
    begin

      // TODO: proc?
      if FLookupList.Find(AKey, ARecordIndex) then
        Result := FLookupList[ARecordIndex]^.DisplayText
      else
      begin
        ADataSet := DataController.DataSet;
        LockDataChanged;
        try
          if (ADataSet <> nil) and ADataSet.Active and (ListField <> nil) and
            CanCallDataSetLocate(DataController.DataSet, KeyFieldNames, AKey) and
            ADataSet.Locate(KeyFieldNames, AKey, []) then
            Result := ListField.AsString {ListField.DisplayText}
          else
            Result := '';
        finally
          UnlockDataChanged;
        end;
        FLookupList.Insert(ARecordIndex, AKey, Result);
      end;

    end
    else
    begin
      ARecordIndex := GetRecordIndexByKey(AKey);
      if ARecordIndex <> -1 then
      begin
        AProperties := GetLookupGridColumnProperties(AItemIndex);
        if (AProperties <> nil) and not (AProperties is TcxCustomLookupEditProperties) and
          not IsFieldFormatted(DataController.GetItemField(AItemIndex), False) then
          Result := AProperties.GetDisplayText(DataController.Values[ARecordIndex, AItemIndex])
        else
          Result := DataController.DisplayTexts[ARecordIndex, AItemIndex];
      end;
    end;
  end;
end;

function TcxCustomDBLookupEditProperties.GetDefaultHorzAlignment: TAlignment;
begin
  if ListField <> nil then
    Result := ListField.Alignment
  else
    Result := inherited GetDefaultHorzAlignment;
end;

function TcxCustomDBLookupEditProperties.GetDefaultMaxLength: Integer;
begin
  if ListField <> nil then
    Result := ListField.Size
  else
    Result := inherited GetDefaultMaxLength;
end;

function TcxCustomDBLookupEditProperties.GetIncrementalFiltering: Boolean;
begin
  if IsUseLookupList then
    Result := False
  else
    Result := inherited GetIncrementalFiltering;
end;

function TcxCustomDBLookupEditProperties.GetKeyByRecordIndex(ARecordIndex: Integer): Variant;
begin
  if (ARecordIndex <> -1) and (DataController <> nil) then
    Result := DataController.GetRecordId(ARecordIndex)
  else
    Result := GetNullKey;
end;

class function TcxCustomDBLookupEditProperties.GetLookupDataClass: TcxInterfacedPersistentClass;
begin
  Result := TcxCustomDBLookupEditLookupData;
end;

function TcxCustomDBLookupEditProperties.GetLookupResultFieldName: string;
begin
  if GetLookupField <> nil then
    Result := GetLookupField.LookupResultField
  else
    Result := '';
end;

function TcxCustomDBLookupEditProperties.GetNullKey: Variant;
var
  I, C: Integer;
begin
  if IsMultipleFieldNames(KeyFieldNames) then
  begin
    C := GetFieldNamesCount(KeyFieldNames);
    Result := VarArrayCreate([0, C - 1], varVariant);
    for I := 0 to C - 1 do
      Result[I] := Null;
  end
  else
    Result := Null;
end;

function TcxCustomDBLookupEditProperties.GetRecordIndexByKey(const AKey: Variant): Integer;
begin
  try
    Result := DataController.FindRecordIndexByKey(AKey);
  except
    on EVariantError do
      Result := -1;
  end;
end;

function TcxCustomDBLookupEditProperties.GetIsUseLookupList: Boolean;
begin
  Result := FLookupList <> nil;
end;

function TcxCustomDBLookupEditProperties.GetKeyFieldNames: string;
begin
  if DataController <> nil then
    Result := DataController.KeyFieldNames
  else
    Result := '';
end;

function TcxCustomDBLookupEditProperties.GetListField: TField;
var
  AListIndex: Integer;
begin
  AListIndex := GetListIndex;
  if AListIndex <> -1 then
    Result := GetDBLookupGridColumnField(AListIndex)
  else
    Result := nil;
end;

function TcxCustomDBLookupEditProperties.GetListFieldIndex: Integer;
begin
  Result := inherited DisplayColumnIndex;
end;

function TcxCustomDBLookupEditProperties.GetListFieldNames: string;
var
  I: Integer;
begin
  Result := '';
  if GetLookupGridColumnCount > 0 then
  begin
   Result := GetDBLookupGridColumnFieldName(0);
   for I := 1 to GetLookupGridColumnCount - 1 do
     Result := Result + ';' + GetDBLookupGridColumnFieldName(I);
  end;
end;

procedure TcxCustomDBLookupEditProperties.SetIsUseLookupList(Value: Boolean);
begin
  if (FLookupList <> nil) <> Value then
  begin
    if Value then
    begin
      FLookupList := TcxLookupList.Create;
    end
    else
    begin
      FLookupList.Free;
      FLookupList := nil;
    end;
    Changed;
  end;
end;

procedure TcxCustomDBLookupEditProperties.SetKeyFieldNames(const Value: string);
begin
  if IsDefinedByLookup and not FSyncLookup then
    DefineByLookupError;
  if DataController <> nil then
    DataController.KeyFieldNames := Value;
end;

procedure TcxCustomDBLookupEditProperties.SetListFieldIndex(Value: Integer);
begin
  inherited DisplayColumnIndex := Value;
end;

procedure TcxCustomDBLookupEditProperties.SetListFieldNames(const Value: string);
var
  AChanged: Boolean;
begin
  AChanged := ListFieldNames <> Value;
  DBLookupGridBeginUpdate;
  try
    DBLookupGridCreateColumnsByFieldNames(Value);
    CheckLookupColumn;
    CheckDisplayColumnIndex;
  finally
    DBLookupGridEndUpdate;
  end;
  if AChanged then
    Changed;
end;

{ TcxCustomDBLookupEdit }

class function TcxCustomDBLookupEdit.GetPropertiesClass: TcxCustomEditPropertiesClass;
begin
  Result := TcxCustomDBLookupEditProperties;
end;

function TcxCustomDBLookupEdit.GetClearValue: TcxEditValue;

  function GetNullArray: Variant;
  var
    AFieldList: TcxDBFieldList;
    I: Integer;
  begin
    AFieldList := TcxDBFieldList.Create;
    try
      ActiveProperties.FLookupSource.DataSet.GetFieldList(AFieldList, ActiveProperties.KeyFieldNames);
      Result := VarArrayCreate([0, AFieldList.Count - 1], varVariant);
      for I := 0 to AFieldList.Count - 1 do
        Result[I] := Null;
    finally
      AFieldList.Free;
    end;
  end;

begin
  if (ActiveProperties.GetEditValueSource(InternalFocused) = evsKey) and
    (Pos(';', ActiveProperties.KeyFieldNames) <> 0) then
    Result := GetNullArray
  else
    Result := inherited GetClearValue;
end;

function TcxCustomDBLookupEdit.IsValidChar(AChar: Char): Boolean;
begin
  if ActiveProperties.ListField <> nil then
    Result := ActiveProperties.ListField.IsValidChar(AChar)
  else
    Result := True;
end;

function TcxCustomDBLookupEdit.ItemIndexToLookupKey(AItemIndex: Integer): TcxEditValue;
begin
  Result := ActiveProperties.GetKeyByRecordIndex(AItemIndex);
end;

function TcxCustomDBLookupEdit.LookupKeyToEditValue(const AKey: TcxEditValue): TcxEditValue;
begin
  Result := AKey;
end;

function TcxCustomDBLookupEdit.LookupKeyToItemIndex(const AKey: TcxEditValue): Integer;
begin
  Result := ActiveProperties.GetRecordIndexByKey(AKey);
end;

procedure TcxCustomDBLookupEdit.PopupWindowBeforeClosing(Sender: TObject);
begin
  try
    ActiveProperties.DataController.CheckBrowseMode;
  except
    ActiveProperties.DataController.Cancel;
    raise
  end;
end;

procedure TcxCustomDBLookupEdit.PopupWindowClosed(Sender: TObject);
begin
  if ActiveProperties.DataController.DataModeController.SyncMode then
  begin
    if not VarEquals(ILookupData.CurrentKey, EditValue) then
      ILookupData.CurrentKey := EditValue
    else
      TcxCustomDBLookupEditLookupData(LookupData).SyncGrid;
  end;
  inherited PopupWindowClosed(Sender);
end;

procedure TcxCustomDBLookupEdit.PrepareDisplayValue(
  const AEditValue: TcxEditValue; var DisplayValue: TcxEditValue;
  AEditFocused: Boolean);
begin
  if (ActiveProperties.DropDownListStyle <> lsEditList) and not Focused and
    ActiveProperties.CanDisplayArbitraryEditValue then
      DisplayValue := VarToStr(AEditValue)
  else
    ActiveProperties.PrepareDisplayValue(AEditValue, DisplayValue, AEditFocused);
end;

function TcxCustomDBLookupEdit.GetProperties: TcxCustomDBLookupEditProperties;
begin
  Result := TcxCustomDBLookupEditProperties(inherited Properties);
end;

function TcxCustomDBLookupEdit.GetActiveProperties: TcxCustomDBLookupEditProperties;
begin
  Result := TcxCustomDBLookupEditProperties(InternalGetActiveProperties);
end;

procedure TcxCustomDBLookupEdit.SetProperties(Value: TcxCustomDBLookupEditProperties);
begin
  Properties.Assign(Value);
end;

{ TcxDBLookupEditDataBinding }

function TcxDBLookupEditDataBinding.IsLookupControl: Boolean;
begin
  Result := True;
end;

end.

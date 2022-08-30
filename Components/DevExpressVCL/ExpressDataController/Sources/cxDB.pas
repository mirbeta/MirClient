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

unit cxDB;

{$I cxVer.inc}

interface

uses
{$IFDEF DELPHI17}
  System.Generics.Defaults, Generics.Collections,
{$ENDIF}
  Classes, SysUtils, Variants, DB,
  dxCore, cxDataUtils;

type
{$IFDEF DELPHI17}
  TcxDBFieldList = TList<TField>;
{$ELSE}
  TcxDBFieldList = TList;
{$ENDIF}

  { TcxDBAdapterList }

  TDataSetClass = class of TDataSet;

  TcxDBAdapterItem = class
  private
    FDataSetClass: TDataSetClass;
  public
    constructor Create(ADataSetClass: TDataSetClass); virtual;
    property DataSetClass: TDataSetClass read FDataSetClass;
  end;

  TcxDBAdapterItemClass = class of TcxDBAdapterItem;

  TcxDBAdapterList = class
  private
    FItems: TList;
    function GetItem(Index: Integer): TcxDBAdapterItem;
    function GetItemCount: Integer;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    function FindAdapter(ADataSetClass: TDataSetClass; var AIndex: Integer): Boolean; virtual;
    procedure RegisterAdapter(ADataSetClass: TDataSetClass; AItemClass: TcxDBAdapterItemClass); virtual;
    procedure UnregisterAdapter(ADataSetClass: TDataSetClass; AItemClass: TcxDBAdapterItemClass); virtual;
    property ItemCount: Integer read GetItemCount;
    property Items[Index: Integer]: TcxDBAdapterItem read GetItem; default;
  end;

  { TcxCustomFieldDataLink }

  TcxCustomDBDataBinding = class;

  TcxCustomFieldDataLink = class(TDataLink)
  private
    FField: TField;
    FFieldName: string;
    FEditing: Boolean;
    FModified: Boolean;
    function GetCanModify: Boolean;
    function GetDataComponent: TComponent;
    procedure SetEditing(Value: Boolean);
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: string);
    procedure UpdateField;
  protected
    FDataBinding: TcxCustomDBDataBinding;
    procedure ActiveChanged; override;
    procedure DataEvent(Event: TDataEvent; Info: TdxNativeInt); override;
    procedure EditingChanged; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure UpdateData; override;
    procedure DataComponentChanged; virtual;
    procedure UpdateRightToLeft; virtual;
    procedure VisualControlChanged; virtual;
    property DataBinding: TcxCustomDBDataBinding read FDataBinding;
    property DataComponent: TComponent read GetDataComponent;
  public
    constructor Create(ADataBinding: TcxCustomDBDataBinding); virtual;
    function Edit: Boolean;
    procedure Modified;
    procedure Reset;
    property CanModify: Boolean read GetCanModify;
    property Editing: Boolean read FEditing;
    property Field: TField read FField;
    property FieldName: string read FFieldName write SetFieldName;
  end;

  TcxCustomFieldDataLinkClass = class of TcxCustomFieldDataLink;

  { TcxCustomDBDataBinding }

  TcxCustomDBDataBinding = class(TcxCustomDataBinding)
  private
    FRefreshCount: Integer;
    function GetDataField: string;
    function GetDataSource: TDataSource;
    function GetField: TField;
    procedure InternalDataChange;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(Value: TDataSource);
  protected
    FDataLink: TcxCustomFieldDataLink;
    function GetModified: Boolean; override;
    function GetReadOnly: Boolean; override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure VisualControlChanged; override;
    procedure DisableRefresh;
    procedure EnableRefresh;
    function GetDataLinkClass: TcxCustomFieldDataLinkClass; virtual;
    function IsRefreshDisabled: Boolean;
  public
    constructor Create(AOwner, ADataComponent: TComponent); override;
    destructor Destroy; override;
    function CanModify: Boolean; override;
    function ExecuteAction(Action: TBasicAction): Boolean; override;
    function GetStoredValue(AValueSource: TcxDataEditValueSource; AFocused: Boolean): Variant; override;
    function IsControlReadOnly: Boolean; override;
    function IsDataSourceLive: Boolean; override;
    function IsDataStorage: Boolean; override;
    procedure Reset; override;
    function SetEditMode: Boolean; override;
    procedure SetStoredValue(AValueSource: TcxDataEditValueSource; const Value: Variant); override;
    function UpdateAction(Action: TBasicAction): Boolean; override;
    procedure UpdateDataSource; override;
    property Field: TField read GetField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataField: string read GetDataField write SetDataField;
    property DataLink: TcxCustomFieldDataLink read FDataLink;
  end;

  TcxDBDataBinding = class(TcxCustomDBDataBinding)
  published
    property DataSource;
    property DataField;
  end;

function CanModifyLookupField(AField: TField): Boolean;
procedure CheckFilterFieldName(var AFieldName: string);
function GetDataSetValues(ADataSet: TDataSet; AFields: TList): Variant;
function GetFilterFieldName(AField: TField; AIgnoreOrigin: Boolean): string;
function IsDataAvailable(AField: TField): Boolean;
function IsDefaultFields(ADataSet: TDataSet): Boolean;
function IsEqualFieldNames(const AFieldName1, AFieldName2: string): Boolean;
function IsFieldCanModify(AField: TField; AIsValueSource: Boolean): Boolean;
function IsFieldFormatted(AField: TField; AIsTextEdit: Boolean): Boolean;
function IsMultipleFieldNames(const AFieldNames: string): Boolean;
function IsSimpleCurrencyField(AField: TField): Boolean;
function GetFieldNamesCount(const AFieldNames: string): Integer;
procedure GetFieldNames(const AFieldNames: string; AList: TStrings);
function GetFieldValue(AField: TField): Variant;
procedure SetDataSetValues(ADataSet: TDataSet; AFields: TList; const AValues: Variant);
procedure SetFieldValue(AField: TField; const AValue: Variant);
function FindField(AFields: TFields; const AName: string): TField; inline;

implementation

uses
  cxVariants;

type
  TDataSetAccess = class(TDataSet);

function CanModifyLookupField(AField: TField): Boolean;
var
  AMasterFields: TcxDBFieldList;
  I: Integer;
begin
  Result := False;
  if Assigned(AField.DataSet) then
  begin
    AMasterFields := TcxDBFieldList.Create;
    try
      AField.DataSet.GetFieldList(AMasterFields, AField.KeyFields);
      Result := AMasterFields.Count > 0;
      for I := 0 to AMasterFields.Count - 1 do
        Result := Result and TField(AMasterFields[I]).CanModify;
    finally
      AMasterFields.Free;
    end;
  end;
end;

procedure CheckFilterFieldName(var AFieldName: string);
begin
  if not IsValidIdent(AFieldName) then
    AFieldName := '[' + AFieldName + ']';
end;

function GetDataSetValues(ADataSet: TDataSet; AFields: TList): Variant;
var
  I: Integer;
begin
  if AFields.Count > 0 then
  begin
    if AFields.Count > 1 then
    begin
      Result := VarArrayCreate([0, AFields.Count - 1], varVariant);
      for I := 0 to AFields.Count - 1 do
        Result[I] := TField(AFields[I]).Value;
    end
    else
      Result := TField(AFields[0]).Value;
  end
  else
    Result := Null;
end;

function GetFilterFieldName(AField: TField; AIgnoreOrigin: Boolean): string;
begin
  Result := '';
  if Assigned(AField) then
  begin
    if not AIgnoreOrigin then
      Result := AField.Origin;
    if Result = '' then
    begin
      Result := AField.FieldName;
      CheckFilterFieldName(Result);
    end;
  end;
end;

function IsDataAvailable(AField: TField): Boolean;
begin
  Result := (AField <> nil) and (AField.DataSet <> nil) and
    (AField.DataSet.State <> dsInactive);
end;

function IsDefaultFields(ADataSet: TDataSet): Boolean;
begin
{$IFDEF DELPHI20}
  Result := (TDataSetAccess(ADataSet).FieldOptions.AutoCreateMode <> acExclusive) or not (lcPersistent in ADataSet.Fields.LifeCycles);
{$ELSE}
  Result := ADataSet.DefaultFields;
{$ENDIF}
end;

function IsEqualFieldNames(const AFieldName1, AFieldName2: string): Boolean;
begin
  Result := AnsiUpperCase(AFieldName1) = AnsiUpperCase(AFieldName2);
end;

function IsFieldCanModify(AField: TField; AIsValueSource: Boolean): Boolean;
begin
  Result := AField.CanModify and
    (AIsValueSource or not (AField.DataType in ftNonTextTypes) or Assigned(AField.OnSetText));
end;

function IsFieldFormatted(AField: TField; AIsTextEdit: Boolean): Boolean;
begin
  if AField = nil then
    Exit(False);
  Result := Assigned(AField.OnGetText) or (AField.EditMask <> '');
  if not Result then
  begin
    if AField.DataType in ftNonTextTypes  then
      Result := AIsTextEdit // Field.DisplayText!
    else
      if AField is TBooleanField then
        Result := AIsTextEdit // TODO: compare DisplayValues
      else
        if AField is TNumericField then
        begin
          Result := (TNumericField(AField).DisplayFormat <> '') or (TNumericField(AField).EditFormat <> '');
          if not Result then
          begin
            if AField is TFloatField then
              Result := TFloatField(AField).Currency
            else
              if AField is TBCDField then
                Result := TBCDField(AField).Currency
              else
                if AField is TFMTBCDField then
                  Result := TFMTBCDField(AField).Currency
          end;
        end
        else
          if AField is TDateTimeField then
            Result := TDateTimeField(AField).DisplayFormat <> ''
          else
            if AField is TAggregateField then
              Result := (TAggregateField(AField).DisplayFormat <> '') or TAggregateField(AField).Currency
            else
              if AField is TSQLTimeStampField then
                Result := TSQLTimeStampField(AField).DisplayFormat <> '';
  end;
end;

function IsMultipleFieldNames(const AFieldNames: string): Boolean;
var
  APos: Integer;
begin
  APos := 1;
{$WARNINGS OFF}
  ExtractFieldName(AFieldNames, APos);
{$WARNINGS ON}
  Result := APos <= Length(AFieldNames);
end;

function IsSimpleCurrencyField(AField: TField): Boolean;
begin
  Result := False;
  if AField is TNumericField then
  begin
    if AField is TFloatField then
      Result := TFloatField(AField).Currency
    else
    begin
      if AField is TBCDField then
        Result := TBCDField(AField).Currency
      else
        if AField is TFMTBCDField then
          Result := TFMTBCDField(AField).Currency;
    end;
    if Result then
    begin
      if (TNumericField(AField).DisplayFormat <> '') or Assigned(AField.OnGetText) then
        Result := False;
    end;
  end;
end;

function GetFieldNamesCount(const AFieldNames: string): Integer;
var
  APos: Integer;
begin
  Result := 0; // TODO: use GetFieldNames?
  APos := 1;
  while APos <= Length(AFieldNames) do
  begin
    Inc(Result);
  {$WARNINGS OFF}
    ExtractFieldName(AFieldNames, APos);
  {$WARNINGS ON}
  end;
end;

procedure GetFieldNames(const AFieldNames: string; AList: TStrings);
var
  APos: Integer;
begin
  AList.Clear;
  APos := 1;
  while APos <= Length(AFieldNames) do
  {$WARNINGS OFF}
    AList.Add(ExtractFieldName(AFieldNames, APos))
  {$WARNINGS ON}
end;

function GetFieldValue(AField: TField): Variant;
begin
  if AField is TAggregateField then // bug in Delphi (IsNull = True!)
    Result := AField.Value
  else
    if AField.IsNull then
      Result := Null
    else
      Result := AField.Value;
end;

procedure SetDataSetValues(ADataSet: TDataSet; AFields: TList;
  const AValues: Variant);
var
  I: Integer;
begin
  if AFields.Count > 0 then
  begin
    if AFields.Count > 1 then
    begin
      for I := 0 to AFields.Count - 1 do
        TField(AFields[I]).Value := AValues[I];
    end
    else
      TField(AFields[0]).Value := AValues;
  end;
end;

procedure SetFieldValue(AField: TField; const AValue: Variant);
begin
  if (AField is TDateTimeField) and (VarType(AValue) = varDouble) then // bug in Delphi
    TDateTimeField(AField).Value := AValue
  else
    AField.Value := AValue;
end;

function FindField(AFields: TFields; const AName: string): TField;
{$IFDEF DELPHI10SEATTLE}
var
  I: Integer;
{$ENDIF}
begin
{$IFDEF DELPHI10SEATTLE}
  for I := 0 to AFields.Count - 1 do
  begin
    Result := AFields[I];
    if AnsiCompareText(Result.FieldName, AName) = 0 then
      Exit;
  end;
  Result := nil;
{$ELSE}
  Result := AFields.FindField(AName);
{$ENDIF}
end;

{ TcxDBAdapterItem }

constructor TcxDBAdapterItem.Create(ADataSetClass: TDataSetClass);
begin
  inherited Create;
  FDataSetClass := ADataSetClass;
end;

{ TcxDBAdapterList }

constructor TcxDBAdapterList.Create;
begin
  inherited Create;
  FItems := TList.Create;
end;

destructor TcxDBAdapterList.Destroy;
begin
  Clear;
  FItems.Free;
  FItems := nil;
  inherited Destroy;
end;

procedure TcxDBAdapterList.Clear;
var
  I: Integer;
begin
  for I := 0 to FItems.Count - 1 do
    TObject(FItems[I]).Free;
  FItems.Clear;
end;

function TcxDBAdapterList.FindAdapter(ADataSetClass: TDataSetClass;
  var AIndex: Integer): Boolean;
var
  I: Integer;
  AItem: TcxDBAdapterItem;
begin
  Result := False;
  for I := FItems.Count - 1 downto 0 do
  begin
    AItem := TcxDBAdapterItem(FItems[I]);
    if ADataSetClass.InheritsFrom(AItem.DataSetClass) then
    begin
      AIndex := I;
      Result := True;
      Break;
    end
    else
      if AItem.DataSetClass.InheritsFrom(ADataSetClass) then
        AIndex := I;
  end;
end;

procedure TcxDBAdapterList.RegisterAdapter(ADataSetClass: TDataSetClass;
  AItemClass: TcxDBAdapterItemClass);
var
  AIndex: Integer;
begin
  AIndex := -1;
  if FindAdapter(ADataSetClass, AIndex) then
    FItems.Insert(AIndex + 1, AItemClass.Create(ADataSetClass))
  else
    if AIndex <> -1 then
      FItems.Insert(AIndex, AItemClass.Create(ADataSetClass))
    else
      FItems.Add(AItemClass.Create(ADataSetClass));
end;

procedure TcxDBAdapterList.UnregisterAdapter(ADataSetClass: TDataSetClass;
  AItemClass: TcxDBAdapterItemClass);
var
  I: Integer;
  AItem: TcxDBAdapterItem;
begin
  for I := FItems.Count - 1 downto 0 do
  begin
    AItem := TcxDBAdapterItem(FItems[I]);
    if (AItem.DataSetClass = ADataSetClass) and (AItem.ClassType = AItemClass) then
    begin
      AItem.Free;
      FItems.Delete(I);
    end;
  end;
end;

function TcxDBAdapterList.GetItem(Index: Integer): TcxDBAdapterItem;
begin
  Result := TcxDBAdapterItem(FItems[Index]);
end;

function TcxDBAdapterList.GetItemCount: Integer;
begin
  Result := FItems.Count;
end;

{ TcxCustomFieldDataLink }

constructor TcxCustomFieldDataLink.Create(ADataBinding: TcxCustomDBDataBinding);
begin
  inherited Create;
  VisualControl := False;
  FDataBinding := ADataBinding;
end;

function TcxCustomFieldDataLink.Edit: Boolean;
begin
  if CanModify then
    inherited Edit;
  Result := FEditing;
end;

procedure TcxCustomFieldDataLink.Modified;
begin
  FModified := True;
end;

procedure TcxCustomFieldDataLink.Reset;
begin
  RecordChanged(nil);
end;

procedure TcxCustomFieldDataLink.ActiveChanged;
begin
  UpdateField;
  FDataBinding.DataSetChange;
end;

procedure TcxCustomFieldDataLink.DataEvent(Event: TDataEvent;
  Info: TdxNativeInt);
begin
  if Event = deDataSetChange then
    UpdateField;
  inherited DataEvent(Event, Info);
  if Event = deDataSetChange then
    FDataBinding.DataSetChange;
end;

procedure TcxCustomFieldDataLink.EditingChanged;
begin
  SetEditing(inherited Editing and CanModify);
end;

(*procedure TcxCustomFieldDataLink.FocusControl(Field: TFieldRef);
begin
  if (Field^ <> nil) and (Field^ = FField) and (FDataComponent is TWinControl) then
    if TWinControl(FDataComponent).CanFocus then
    begin
      Field^ := nil;
      TWinControl(FDataComponent).SetFocus;
    end;
end;*)

procedure TcxCustomFieldDataLink.LayoutChanged;
begin
  UpdateField;
end;

procedure TcxCustomFieldDataLink.RecordChanged(Field: TField);
begin
  if (Field = nil) or (Field = FField) then
  begin
    FDataBinding.InternalDataChange;
    if not FDataBinding.IsRefreshDisabled then
      FModified := False;
  end;
end;

procedure TcxCustomFieldDataLink.UpdateData;
begin
  if FModified then
  begin
    if Field <> nil then
      FDataBinding.UpdateData;
    if not FDataBinding.IsRefreshDisabled then
      FModified := False;
  end;
end;

procedure TcxCustomFieldDataLink.DataComponentChanged;
begin
end;

procedure TcxCustomFieldDataLink.UpdateRightToLeft;
begin
end;

procedure TcxCustomFieldDataLink.VisualControlChanged;
begin
end;

function TcxCustomFieldDataLink.GetCanModify: Boolean;
begin
  Result := not ReadOnly and (Field <> nil) and (Field.CanModify or
    (Field.Lookup and CanModifyLookupField(Field)));
end;

function TcxCustomFieldDataLink.GetDataComponent: TComponent;
begin
  Result := FDataBinding.DataComponent;
end;

procedure TcxCustomFieldDataLink.SetEditing(Value: Boolean);
begin
  if FEditing <> Value then
  begin
    FEditing := Value;
    if not FDataBinding.IsRefreshDisabled then
      FModified := False;
  end;
end;

procedure TcxCustomFieldDataLink.SetField(Value: TField);
begin
  if FField <> Value then
  begin
    FField := Value;
    FDataBinding.DataSetChange;
    EditingChanged;
    RecordChanged(nil);
    UpdateRightToLeft;
  end;
end;

procedure TcxCustomFieldDataLink.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    FFieldName :=  Value;
    UpdateField;
  end;
end;

procedure TcxCustomFieldDataLink.UpdateField;
begin
  if Active and (FFieldName <> '') then
  begin
    FField := nil;
    if Assigned(DataComponent) then
      SetField(GetFieldProperty(DataSource.DataSet, DataComponent, FFieldName))
    else
      SetField(DataSource.DataSet.FieldByName(FFieldName));
  end
  else
    SetField(nil);
end;

{ TcxCustomDBDataBinding }

constructor TcxCustomDBDataBinding.Create(AOwner, ADataComponent: TComponent);
begin
  inherited Create(AOwner, ADataComponent);
  FDataLink := GetDataLinkClass.Create(Self);
// TODO SetReplicable
end;

destructor TcxCustomDBDataBinding.Destroy;
begin
  FDataLink.Free;
  inherited Destroy;
end;

function TcxCustomDBDataBinding.CanModify: Boolean;
begin
  Result := IsDataSourceLive and not Field.ReadOnly;
  Result := Result and (Field.CanModify or (Field.Lookup and CanModifyLookupField(Field)));
end;

function TcxCustomDBDataBinding.ExecuteAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.ExecuteAction(Action);
end;

function TcxCustomDBDataBinding.IsControlReadOnly: Boolean;
begin
  Result := ReadOnly;
  if not Result and IsDataSourceLive then
    Result := Field.ReadOnly;
end;

function TcxCustomDBDataBinding.IsDataSourceLive: Boolean;
begin
  Result := IsDataAvailable(FDataLink.FField);
end;

function TcxCustomDBDataBinding.IsDataStorage: Boolean;
begin
  Result := True;
end;

procedure TcxCustomDBDataBinding.Reset;
begin
  FDataLink.Reset;
end;

function TcxCustomDBDataBinding.SetEditMode: Boolean;
begin
  Result := inherited SetEditMode;
  if not Result then
    Exit;

  DisableRefresh;
  try
    FDataLink.Edit;
    Result := FDataLink.Editing;
    if Result then
      FDataLink.Modified;
  finally
    EnableRefresh;
  end;
end;

procedure TcxCustomDBDataBinding.UpdateDataSource;
begin
  FDataLink.UpdateRecord;
end;

function TcxCustomDBDataBinding.GetModified: Boolean;
begin
  Result := IsDataSourceLive and FDataLink.Editing and FDataLink.FModified;
end;

function TcxCustomDBDataBinding.GetReadOnly: Boolean;
begin
  Result := FDataLink.ReadOnly;
end;

function TcxCustomDBDataBinding.GetStoredValue(AValueSource: TcxDataEditValueSource;
  AFocused: Boolean): Variant;
begin
  if not IsDataSourceLive or (Field.IsNull and (AValueSource <> evsText)) then
    Result := Null
  else
    case AValueSource of
      evsKey:
        if Field.KeyFields <> '' then
          Result := Field.DataSet.FieldValues[Field.KeyFields]
        else
          Result := Field.Value;
      evsText:
        if AFocused and FDataLink.CanModify then
          Result := Field.Text
        else
          Result := Field.DisplayText;
    else {evsValue:}
      Result := Field.Value;
    end;
end;

procedure TcxCustomDBDataBinding.SetReadOnly(Value: Boolean);
begin
  if Value <> ReadOnly then
  begin
    FDataLink.ReadOnly := Value;
    DataSetChange;
  end;
end;

procedure TcxCustomDBDataBinding.VisualControlChanged;
begin
  FDataLink.VisualControlChanged;
end;

procedure TcxCustomDBDataBinding.SetStoredValue(AValueSource: TcxDataEditValueSource;
  const Value: Variant);

  procedure SetFieldValueEx(AField: TField; const AValue: Variant);
  begin
    if VarIsStr(Value) and (Value = '') and not(Field.DataType in [ftString, ftWideString]) then
      AField.Value := Null
    else
      AField.Value := Value;
  end;

var
  AFieldList: TcxDBFieldList;
  I: Integer;
begin
  if IsDataSourceLive then
  begin
    DisableRefresh;
    try
      if FDataLink.Edit then
      begin
        if (*(*)AValueSource = evsText(*) or Assigned(Field.OnSetText)*) then
          Field.Text := VarToStr(Value)
        else
          if (AValueSource = evsKey) and (Field.KeyFields <> '') then
            if Pos(';', Field.KeyFields) = 0 then
              SetFieldValueEx(Field.DataSet.FieldByName(Field.KeyFields), Value)
            else
            begin
              AFieldList := TcxDBFieldList.Create;
              try
                Field.DataSet.GetFieldList(AFieldList, Field.KeyFields);
                for I := 0 to AFieldList.Count - 1 do
                  SetFieldValueEx(TField(AFieldList[I]), Value[I]);
              finally
                AFieldList.Free;
              end;
              Field.DataSet.FieldValues[Field.KeyFields] := Value;
            end
          else
            SetFieldValueEx(Field, Value);
      end;
    finally
      EnableRefresh;
    end;
  end;
end;

function TcxCustomDBDataBinding.UpdateAction(Action: TBasicAction): Boolean;
begin
  Result := FDataLink.UpdateAction(Action);
end;

procedure TcxCustomDBDataBinding.DisableRefresh;
begin
  Inc(FRefreshCount);
end;

procedure TcxCustomDBDataBinding.EnableRefresh;
begin
  if FRefreshCount > 0 then
    Dec(FRefreshCount);
end;

function TcxCustomDBDataBinding.GetDataLinkClass: TcxCustomFieldDataLinkClass;
begin
  Result := TcxCustomFieldDataLink;
end;

function TcxCustomDBDataBinding.IsRefreshDisabled: Boolean;
begin
  Result := FRefreshCount > 0;
end;

function TcxCustomDBDataBinding.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

function TcxCustomDBDataBinding.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

function TcxCustomDBDataBinding.GetField: TField;
begin
  Result := FDataLink.Field;
end;

procedure TcxCustomDBDataBinding.InternalDataChange;
begin
  if not IsRefreshDisabled then
    DataChange;
end;

procedure TcxCustomDBDataBinding.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TcxCustomDBDataBinding.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

end.

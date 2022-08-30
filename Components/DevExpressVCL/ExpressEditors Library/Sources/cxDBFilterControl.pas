{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressFilterControl                                     }
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
unit cxDBFilterControl;

{$I cxVer.inc}

interface

uses
  Classes, Graphics, cxClasses, cxEdit, cxFilter, cxDataStorage, cxLookAndFeels,
  cxFilterControl, cxFilterControlUtils, DB;

type
  TcxDBFilterControl = class;

  { TcxFilterItem }

  TcxFilterItem = class(TcxInterfacedCollectionItem, IcxEditRepositoryItemListener)
  private
    FOwnerInterface: IUnknown;
    FFilterControl: TcxDBFilterControl;
    FCaption: string;
    FFieldName: string;
    FProperties: TcxCustomEditProperties;
    FRepositoryItem: TcxEditRepositoryItem;
    FPropertiesClass: TcxCustomEditPropertiesClass;
    procedure DoPropertiesChanged(Sender: TObject);
    function GetPropertiesClassName: string;
    function IsCaptionStored: Boolean;
    procedure RecreateProperties;
    procedure SetCaption(const Value: string);
    procedure SetFieldName(const Value: string);
    procedure SetProperties(const Value: TcxCustomEditProperties);
    procedure SetPropertiesClass(const Value: TcxCustomEditPropertiesClass);
    procedure SetPropertiesClassName(const Value: string);
    procedure SetRepositoryItem(const Value: TcxEditRepositoryItem);
  protected
    // IcxEditRepositoryItemListener
    procedure ItemRemoved(Sender: TcxEditRepositoryItem);
    procedure PropertiesChanged(Sender: TcxEditRepositoryItem);

    function GetDisplayName: string; override;
    procedure CreateProperties; virtual;
    procedure DestroyProperties; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure AfterConstruction; override;
    function GetProperties: TcxCustomEditProperties;

    property FilterControl: TcxDBFilterControl read FFilterControl;
    property PropertiesClass: TcxCustomEditPropertiesClass read FPropertiesClass write SetPropertiesClass;
  published
    property Caption: string read FCaption write SetCaption stored IsCaptionStored;
    property FieldName: string read FFieldName write SetFieldName;
    property PropertiesClassName: string read GetPropertiesClassName write SetPropertiesClassName;
    property Properties: TcxCustomEditProperties read FProperties write SetProperties;
    property RepositoryItem: TcxEditRepositoryItem read FRepositoryItem write SetRepositoryItem;
  end;

  { TcxFilterPropertiesList }

  TcxFilterPropertiesList = class(TList)
  public
    procedure Clear; override;
    function GetProperties(AClass: TcxCustomEditPropertiesClass): TcxCustomEditProperties;
  end;

  { TcxFilterItemCollection }

  TcxFilterItemCollection = class(TOwnedCollection)
  private
    function GetControl: TcxDBFilterControl;
    function GetItems(Index: Integer): TcxFilterItem;
    procedure SetItems(Index: Integer; const Value: TcxFilterItem);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    property Control: TcxDBFilterControl read GetControl;
    property Items[Index: Integer]: TcxFilterItem read GetItems write SetItems; default;
  end;

  { TcxDBFilterOptions }

  TcxDBFilterOptions = class(TPersistent)
  private
    FFilterControl: TcxDBFilterControl;
    FSupportedBetween: Boolean;
    FSupportedIn: Boolean;
    FSupportedLike: Boolean;
    function GetDateTimeFormat: string;
    function GetPercentWildcard: Char;
    function GetSoftNull: Boolean;
    function GetTranslateBetween: Boolean;
    function GetTranslateIn: Boolean;
    function GetTranslateLike: Boolean;
    function GetUnderscoreWildcard: Char;
    procedure SetDateTimeFormat(const Value: string);
    procedure SetPercentWildcard(Value: Char);
    procedure SetSoftNull(Value: Boolean);
    procedure SetTranslateBetween(Value: Boolean);
    procedure SetTranslateIn(Value: Boolean);
    procedure SetTranslateLike(Value: Boolean);
    procedure SetUnderscoreWildcard(Value: Char);
  protected
    function GetOwner: TPersistent; override;
  public
    constructor Create(AFilterControl: TcxDBFilterControl); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure ProcessFilterOperators(var SupportedOperations: TcxFilterControlOperators);
  published
    property DateTimeFormat: string read GetDateTimeFormat write SetDateTimeFormat;
    property PercentWildcard: Char read GetPercentWildcard write SetPercentWildcard default '%';
    property SoftNull: Boolean read GetSoftNull write SetSoftNull default False;
    property SupportedBetween: Boolean read FSupportedBetween write FSupportedBetween default True;
    property SupportedIn: Boolean read FSupportedIn write FSupportedIn default True;
    property SupportedLike: Boolean read FSupportedLike write FSupportedLike default True;
    property TranslateBetween: Boolean read GetTranslateBetween write SetTranslateBetween default False;
    property TranslateIn: Boolean read GetTranslateIn write SetTranslateIn default False;
    property TranslateLike: Boolean read GetTranslateLike write SetTranslateLike default False;
    property UnderscoreWildcard: Char read GetUnderscoreWildcard write SetUnderscoreWildcard default '_';
  end;

  { TcxDBFilterControl }

  TcxDBFilterControl = class(TcxCustomFilterControl, IcxFilterControl,
    IcxFilterControlDialog)
  private
    FApplyingFilter: Boolean;
    FDataSource: TDataSource;
    FDataSet: TDataSet;
    FFieldNamePostfix: string;
    FFieldNamePrefix: string;
    FFieldsProperties: TcxFilterPropertiesList;
    FFilterOptions: TcxDBFilterOptions;
    FItems: TcxFilterItemCollection;
    function GetDataSet: TDataSet;
    function GetItems: TcxFilterItemCollection;
    function GetField(AIndex: Integer): TField;
    function GetFilterOptions: TcxDBFilterOptions;
    procedure DataSetStateChange(Sender: TObject);
    function IsItemIndexValid(AIndex: Integer): Boolean;
    procedure SetDataSet(const Value: TDataSet);
    procedure SetFilterOptions(Value: TcxDBFilterOptions);
    procedure SetItems(Value: TcxFilterItemCollection);
  protected
    // IcxFilterControl
    function GetCaption(Index: Integer): string;
    function GetCount: Integer;
    function GetCriteria: TcxFilterCriteria;
    function GetItemLink(Index: Integer): TObject;
    function GetItemLinkID(Index: Integer): Integer;
    function GetItemLinkName(Index: Integer): string;
    function GetFieldName(Index: Integer): string;
    function GetProperties(Index: Integer): TcxCustomEditProperties;
    function GetValueType(Index: Integer): TcxValueTypeClass;
    //IcxFilterControlDialog
    procedure SetDialogLinkComponent(ALink: TComponent);
    // override VCL
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    // override TcxCustomFilterControl
    procedure CorrectOperatorClass(var AOperatorClass: TcxFilterOperatorClass); override;
    function GetExpressionFieldName(AField: TField): string; virtual;
    procedure DoApplyFilter; override;
    procedure ValidateConditions(var SupportedOperations: TcxFilterControlOperators); override;
    function GetFilterLink: IcxFilterControl; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure BuildFromCriteria; override;
    procedure Clear; override;
    function GetFilterTextEx(const AFieldNamePrefix, AFieldNamePostfix: string): string;
    function GetPropertiesClassFromFieldType(AFieldType: TFieldType): TcxCustomEditPropertiesClass; virtual;

    property Criteria;
    property DataSource: TDataSource read FDataSource;
  published
    property Align;
    property Anchors;
    property AssignedFonts;
    property BiDiMode;
    property Color;
    property DataSet: TDataSet read GetDataSet write SetDataSet;
    property Items: TcxFilterItemCollection read GetItems write SetItems;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property FilterOptions: TcxDBFilterOptions read GetFilterOptions write SetFilterOptions;
    property Font;
    property FontBoolOperator;
    property FontCondition;
    property FontItem;
    property FontValue;
    property HelpContext;
    property HelpKeyword;
    property HelpType;
    property Hint;
    property HotTrackOnUnfocused;
    property LookAndFeel;
    property Nullstring; //lowercase because define Nullstring in CBuilder
    property ParentBiDiMode;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property ShowLevelLines;
    property SortItems;
    property TabOrder;
    property TabStop;
    property Visible;
    property WantTabs;
    property OnApplyFilter;
    property OnClick;
    property OnContextPopup;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

function ExecuteDBFilterControlDialog(ADataSet: TDataSet;
  ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil;
  AOnShowDialog: TNotifyEvent = nil; AColor: TColor = clDefault;
  const AInitialDir: string = ''): Boolean;

implementation

uses
  SysUtils, cxDBFilter, cxDB, cxDBData, cxCalc, cxCalendar, cxTextEdit,
  cxSpinEdit, cxCheckBox, cxCurrencyEdit, cxTimeEdit, cxFilterControlDialog;

function ExecuteDBFilterControlDialog(ADataSet: TDataSet;
  ALookAndFeel: TcxLookAndFeel; AOnApplyProc: TNotifyEvent = nil;
  AOnShowDialog: TNotifyEvent = nil; AColor: TColor = clDefault;
  const AInitialDir: string = ''): Boolean;
begin
  Result := cxInternalExecuteFilterControlDialog(TcxDBFilterControl, ADataSet,
    ALookAndFeel, AOnApplyProc, AOnShowDialog, AColor, AInitialDir);
end;

{ TcxFilterItem }

constructor TcxFilterItem.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FFilterControl := TcxFilterItemCollection(Collection).Control;
end;

destructor TcxFilterItem.Destroy;
begin
  RepositoryItem := nil;
  DestroyProperties;
  inherited Destroy;
end;

procedure TcxFilterItem.AfterConstruction;
begin
  inherited;
  if GetOwner <> nil then
    GetOwner.GetInterface(IUnknown, FOwnerInterface);
end;

function TcxFilterItem.GetProperties: TcxCustomEditProperties;
begin
  if FRepositoryItem = nil then
    Result := FProperties
  else
    Result := FRepositoryItem.Properties;
end;

procedure TcxFilterItem.ItemRemoved(Sender: TcxEditRepositoryItem);
begin
  RepositoryItem := nil;
  PropertiesChanged(Sender);
end;

procedure TcxFilterItem.PropertiesChanged(Sender: TcxEditRepositoryItem);
begin
  with FilterControl do
    if (ComponentState * [csLoading, csDestroying, csDesigning]) = [] then
      RefreshProperties;
end;

function TcxFilterItem.GetDisplayName: string;
begin
  if FCaption <> '' then
    Result := FCaption
  else
    if FieldName <> '' then
      Result := FieldName
    else
      if GetProperties <> nil then
        Result := GetProperties.ClassName
      else
        Result := ClassName
end;

procedure TcxFilterItem.CreateProperties;
begin
  if FPropertiesClass <> nil then
  begin
    FProperties := FPropertiesClass.Create(Self);
    FProperties.OnPropertiesChanged := DoPropertiesChanged;
  end;
end;

procedure TcxFilterItem.DestroyProperties;
begin
  FreeAndNil(FProperties);
end;

procedure TcxFilterItem.DoPropertiesChanged(Sender: TObject);
begin
  PropertiesChanged(nil);
end;

function TcxFilterItem.GetPropertiesClassName: string;
begin
  if FProperties = nil then
    Result := ''
  else
    Result := FProperties.ClassName;
end;

function TcxFilterItem.IsCaptionStored: Boolean;
begin
  Result := (FCaption <> '') and (FCaption <> FFieldName);
end;

procedure TcxFilterItem.RecreateProperties;
begin
  DestroyProperties;
  CreateProperties;
end;

procedure TcxFilterItem.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TcxFilterItem.SetFieldName(const Value: string);
begin
  if FFieldName <> Value then
  begin
    if FFieldName = FCaption then
      FCaption := Value;
    FFieldName := Value;
    Changed(False);
  end;
end;

procedure TcxFilterItem.SetProperties(const Value: TcxCustomEditProperties);
begin
  if (Value <> nil) and (esoFiltering in Value.GetSupportedOperations) then
    FProperties.Assign(Value);
end;

procedure TcxFilterItem.SetPropertiesClass(
  const Value: TcxCustomEditPropertiesClass);
begin
  if (FPropertiesClass <> Value) and ((Value = nil) or IsSupportFiltering(Value)) then
  begin
    FPropertiesClass := Value;
    RecreateProperties;
    PropertiesChanged(nil);
  end;
end;

procedure TcxFilterItem.SetPropertiesClassName(const Value: string);
var
  APropertiesClass: TcxCustomEditPropertiesClass;
begin
  APropertiesClass :=
    TcxCustomEditPropertiesClass(GetRegisteredEditProperties.FindByClassName(Value));
  if (APropertiesClass = nil) or IsSupportFiltering(APropertiesClass) then
    PropertiesClass := APropertiesClass;
end;

procedure TcxFilterItem.SetRepositoryItem(
  const Value: TcxEditRepositoryItem);
begin
  if FRepositoryItem <> Value then
  begin
    if (Value <> nil) and
      not (esoFiltering in Value.Properties.GetSupportedOperations) then Exit;
    if FRepositoryItem <> nil then
      FRepositoryItem.RemoveListener(Self);
    FRepositoryItem := Value;
    if FRepositoryItem <> nil then
      FRepositoryItem.AddListener(Self);
    PropertiesChanged(nil);
  end;
end;

{ TcxFilterItemCollection }

procedure TcxFilterItemCollection.Update(Item: TCollectionItem);
begin
  if (UpdateCount = 0) and (Control.ComponentState * [csDestroying, csLoading] = []) then
    Control.LayoutChanged;
end;

function TcxFilterItemCollection.GetControl: TcxDBFilterControl;
begin
  if GetOwner is TcxDBFilterControl then
    Result := TcxDBFilterControl(GetOwner)
  else
    Result := nil;
end;

function TcxFilterItemCollection.GetItems(Index: Integer): TcxFilterItem;
begin
  Result := TcxFilterItem(inherited Items[Index])
end;

procedure TcxFilterItemCollection.SetItems(Index: Integer;
  const Value: TcxFilterItem);
begin
  inherited Items[Index] := Value;
end;

{ TcxFilterPropertiesList }

procedure TcxFilterPropertiesList.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    TcxCustomEditProperties(Items[I]).Free;
  inherited Clear;
end;

function TcxFilterPropertiesList.GetProperties(
  AClass: TcxCustomEditPropertiesClass): TcxCustomEditProperties;
var
  I: Integer;
begin
  Result := nil;
  if AClass = nil then Exit;
  for I := 0 to Count - 1 do
    if TcxCustomEditProperties(Items[I]).ClassType = AClass then
    begin
      Result := TcxCustomEditProperties(Items[I]);
      break
    end;
  if Result = nil then
  begin
    Result := AClass.Create(nil);
    Add(Result);
  end;
end;

{ TcxDBFilterOptions }

constructor TcxDBFilterOptions.Create(AFilterControl: TcxDBFilterControl);
begin
  FFilterControl := AFilterControl;
  FSupportedBetween := True;
  FSupportedIn := True;
  FSupportedLike := True;
end;

procedure TcxDBFilterOptions.Assign(Source: TPersistent);
begin
  if Source is TcxDBFilterOptions then
    with TcxDBFilterOptions(Source) do
    begin
      Self.DateTimeFormat := DateTimeFormat;
      Self.PercentWildcard := PercentWildcard;
      Self.SoftNull := SoftNull;
      Self.SupportedBetween := SupportedBetween;
      Self.SupportedIn := SupportedIn;
      Self.SupportedLike := SupportedLike;
      Self.TranslateBetween := TranslateBetween;
      Self.TranslateIn := TranslateIn;
      Self.TranslateLike := TranslateLike;
      Self.UnderscoreWildcard := UnderscoreWildcard;
    end;
end;

procedure TcxDBFilterOptions.ProcessFilterOperators(
  var SupportedOperations: TcxFilterControlOperators);
begin
  if not SupportedBetween then
    SupportedOperations := SupportedOperations - [fcoBetween, fcoNotBetween];
  if not SupportedIn then
    SupportedOperations := SupportedOperations - [fcoInList, fcoNotInList];
  if not SupportedLike then
    SupportedOperations := SupportedOperations - [fcoLike, fcoNotLike,
      fcoContains, fcoNotContains, fcoBeginsWith, fcoEndsWith];
end;

function TcxDBFilterOptions.GetOwner: TPersistent;
begin
  Result := FFilterControl;
end;

function TcxDBFilterOptions.GetDateTimeFormat: string;
begin
  Result := FFilterControl.Criteria.DateTimeFormat;
end;

function TcxDBFilterOptions.GetPercentWildcard: Char;
begin
  Result := FFilterControl.Criteria.PercentWildcard;
end;

function TcxDBFilterOptions.GetSoftNull: Boolean;
begin
  Result := fcoSoftNull in FFilterControl.Criteria.Options;
end;

function TcxDBFilterOptions.GetTranslateBetween: Boolean;
begin
  Result := FFilterControl.Criteria.TranslateBetween;
end;

function TcxDBFilterOptions.GetTranslateIn: Boolean;
begin
  Result := FFilterControl.Criteria.TranslateIn;
end;

function TcxDBFilterOptions.GetTranslateLike: Boolean;
begin
  Result := FFilterControl.Criteria.TranslateLike;
end;

function TcxDBFilterOptions.GetUnderscoreWildcard: Char;
begin
  Result := FFilterControl.Criteria.UnderscoreWildcard;
end;

procedure TcxDBFilterOptions.SetDateTimeFormat(const Value: string);
begin
  FFilterControl.Criteria.DateTimeFormat := Value;
end;

procedure TcxDBFilterOptions.SetPercentWildcard(Value: Char);
begin
  FFilterControl.Criteria.PercentWildcard := Value;
end;

procedure TcxDBFilterOptions.SetSoftNull(Value: Boolean);
begin
  with FFilterControl.Criteria do
    if Value then
      Options := Options + [fcoSoftNull]
    else
      Options := Options - [fcoSoftNull];
end;

procedure TcxDBFilterOptions.SetTranslateBetween(Value: Boolean);
begin
  FFilterControl.Criteria.TranslateBetween := Value;
end;

procedure TcxDBFilterOptions.SetTranslateIn(Value: Boolean);
begin
  FFilterControl.Criteria.TranslateIn := Value;
end;

procedure TcxDBFilterOptions.SetTranslateLike(Value: Boolean);
begin
  FFilterControl.Criteria.TranslateLike := Value;
end;

procedure TcxDBFilterOptions.SetUnderscoreWildcard(Value: Char);
begin
  FFilterControl.Criteria.UnderscoreWildcard := Value;
end;

{ TcxDBFilterControl }

constructor TcxDBFilterControl.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFilterOptions := TcxDBFilterOptions.Create(Self);
  FItems := TcxFilterItemCollection.Create(Self, TcxFilterItem);
  FDataSource := TDataSource.Create(Self);
  FDataSource.OnStateChange := DataSetStateChange;
  FFieldsProperties := TcxFilterPropertiesList.Create;
end;

destructor TcxDBFilterControl.Destroy;
begin
  FreeAndNil(FDataSource);
  FreeAndNil(FItems);
  FreeAndNil(FFieldsProperties);
  FreeAndNil(FFilterOptions);
  inherited Destroy;
end;

procedure TcxDBFilterControl.BuildFromCriteria;
begin
  inherited;
end;

procedure TcxDBFilterControl.Clear;
begin
  inherited Clear;
  FFieldsProperties.Clear;
end;

function TcxDBFilterControl.GetFilterTextEx(
  const AFieldNamePrefix, AFieldNamePostfix: string): string;
begin
  FFieldNamePrefix := AFieldNamePrefix;
  FFieldNamePostfix := AFieldNamePostfix;
  try
    Result := GetFilterText;
  finally
    FFieldNamePrefix := '';
    FFieldNamePostfix := '';
  end;
end;

function TcxDBFilterControl.GetPropertiesClassFromFieldType(
  AFieldType: TFieldType): TcxCustomEditPropertiesClass;
begin
  case AFieldType of
    ftString, ftWideString:
      Result := TcxTextEditProperties;
    ftSmallint, ftInteger, ftWord, ftAutoInc, ftLargeint:
      Result := TcxSpinEditProperties;
    ftBoolean:
      Result := TcxCheckBoxProperties;
    ftFloat:
      Result := TcxCalcEditProperties;
    ftCurrency, ftBCD:
      Result := TcxCurrencyEditProperties;
    ftDate, ftDateTime, ftTimeStamp:
      Result := TcxDateEditProperties;
    ftTime:
      Result := TcxTimeEditProperties;
  else
    Result := nil;
  end;
end;

function TcxDBFilterControl.GetCaption(Index: Integer): string;
begin
  if (Index > -1) and (Index < GetCount) then
    if FItems.Count > 0 then
      Result := FItems[Index].Caption
    else
      Result := GetField(Index).DisplayName
  else
    Result := '';
end;

function TcxDBFilterControl.GetCount: Integer;
var
  I: Integer;
begin
  if (FItems <> nil) and (FItems.Count > 0) then
    Result := FItems.Count
  else
    if (FDataSet <> nil) and not (IsDefaultFields(FDataSet) and (FDataSource.State = dsInactive)) then
    begin
      Result := 0;
      with FDataSet do
        for I := 0 to Fields.Count - 1 do
          if Fields[I].Visible then Inc(Result);
    end
    else
      Result := 0;
end;

function TcxDBFilterControl.GetCriteria: TcxFilterCriteria;
begin
  Result := inherited Criteria;
end;

function TcxDBFilterControl.GetItemLink(Index: Integer): TObject;
begin
  if (Index > -1) and (Index < GetCount) then
    if FItems.Count > 0 then
      Result := FItems[Index]
    else
      Result := GetField(Index)
  else
    Result := nil;
end;

function TcxDBFilterControl.GetItemLinkID(Index: Integer): Integer;
begin
  Result := Index;
end;

function TcxDBFilterControl.GetItemLinkName(Index: Integer): string;
begin
  Result := GetFieldName(Index);
end;

function TcxDBFilterControl.GetFieldName(Index: Integer): string;
var
  AField: TField;
begin
  if (Index > -1) and (Index < GetCount) then
    if FItems.Count > 0 then
    begin
      Result := FItems[Index].FieldName;
      if DataSet <> nil then
      begin
        AField := DataSet.FindField(Result);
        if AField <> nil then
          Result := GetExpressionFieldName(AField);
      end;
    end
    else
    begin
      AField := GetField(Index);
      Result := GetExpressionFieldName(AField);
    end
  else
    Result := '';
end;

function TcxDBFilterControl.GetProperties(Index: Integer): TcxCustomEditProperties;
begin
  if (Index > -1) and (Index < GetCount) then
    if FItems.Count > 0 then
      Result := FItems[Index].GetProperties
    else
      Result := FFieldsProperties.GetProperties(
        GetPropertiesClassFromFieldType(GetField(Index).DataType))
  else
    Result := nil;
  if Result = nil then Result := GetDefaultProperties;
end;

function TcxDBFilterControl.GetValueType(Index: Integer): TcxValueTypeClass;

  function TrimFieldName(const AFieldName: string): string;
  begin
    Result := AFieldName;
    while (Length(Result) > 0) and
      (AnsiChar(Result[1]) in [#0..' ', '"', '''', '[']) do
        Delete(Result, 1, 1);
    while (Length(Result) > 0) and
      (AnsiChar(Result[Length(Result)]) in [#0..' ', '"', '''', ']']) do
        Delete(Result, Length(Result), 1);
  end;

var
  AField: TField;
begin
  AField := nil;
  if (DataSet <> nil) and IsItemIndexValid(Index) then
    if FItems.Count > 0 then
      AField := DataSet.FindField(TrimFieldName(FItems[Index].FieldName))
    else
      AField := GetField(Index);
  if AField <> nil then
    Result := GetValueTypeClassByField(AField)
  else
    Result := nil;
end;

procedure TcxDBFilterControl.SetFilterOptions(Value: TcxDBFilterOptions);
begin
  FFilterOptions.Assign(Value);
end;

procedure TcxDBFilterControl.SetItems(Value: TcxFilterItemCollection);
begin
  FItems.Assign(Value);
end;

procedure TcxDBFilterControl.SetDialogLinkComponent(ALink: TComponent);
begin
  if ALink = nil then
    DataSet := nil
  else
    if ALink is TDataSet then DataSet := TDataSet(ALink);
end;

procedure TcxDBFilterControl.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = FDataSet) then
    DataSet := nil;
end;

procedure TcxDBFilterControl.CorrectOperatorClass(
  var AOperatorClass: TcxFilterOperatorClass);
var
  AOperatorAdapter: TcxDBFilterOperatorAdapter;
begin
  AOperatorAdapter := cxGetFilterOperatorAdapter(FDataSet);
  if Assigned(AOperatorAdapter) then
    AOperatorAdapter.PrepareOperatorClass(nil, FDataSet, AOperatorClass);
end;

function TcxDBFilterControl.GetExpressionFieldName(AField: TField): string;
begin
  if AField.Origin <> '' then
    Result := FFieldNamePrefix + AField.Origin + FFieldNamePostfix
  else
    Result := FFieldNamePrefix + AField.FieldName + FFieldNamePostfix;
end;

procedure TcxDBFilterControl.DoApplyFilter;
begin
  FApplyingFilter := True;
  try
    inherited DoApplyFilter;
  finally
    FApplyingFilter := False;
  end;
end;

procedure TcxDBFilterControl.ValidateConditions(
  var SupportedOperations: TcxFilterControlOperators);
begin
  FilterOptions.ProcessFilterOperators(SupportedOperations);
end;

function TcxDBFilterControl.GetFilterLink: IcxFilterControl;
begin
  QueryInterface(IcxFilterControl, Result);
end;

function TcxDBFilterControl.GetDataSet: TDataSet;
begin
  Result := FDataSet;
end;

function TcxDBFilterControl.GetItems: TcxFilterItemCollection;
begin
  Result := FItems;
end;

function TcxDBFilterControl.GetField(AIndex: Integer): TField;
var
  I, ATest: Integer;
begin
  ATest := 0;
  Result := nil;
  with FDataSet do
    for I := 0 to Fields.Count - 1 do
      if Fields[I].Visible then
      begin
        if ATest = AIndex then
        begin
          Result := Fields[I];
          Exit;
        end;
        Inc(ATest);
      end;
end;

function TcxDBFilterControl.GetFilterOptions: TcxDBFilterOptions;
begin
  Result := FFilterOptions;
end;

procedure TcxDBFilterControl.DataSetStateChange(Sender: TObject);
begin
  if not FApplyingFilter and (FDataSource.State in [dsInactive, dsBrowse]) then
    LayoutChanged;
end;

function TcxDBFilterControl.IsItemIndexValid(AIndex: Integer): Boolean;
begin
  Result := (AIndex > -1) and (AIndex < GetCount);
end;

procedure TcxDBFilterControl.SetDataSet(const Value: TDataSet);
begin
  if FDataSet <> Value then
  begin
    FDataSet := Value;
    FDataSource.DataSet := Value;
    Clear;
    if Value <> nil then Value.FreeNotification(Self);
  end;
end;

end.

{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           Express Cross Platform Library classes                   }
{                                                                    }
{           Copyright (c) 2001-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSCROSSPLATFORMLIBRARY AND ALL   }
{   ACCOMPANYING VCL AND CLX CONTROLS AS PART OF AN EXECUTABLE       }
{   PROGRAM ONLY.                                                    }
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
unit cxPropertiesStore;

{$I cxVer.inc}

interface

uses
  Classes, SysUtils, TypInfo, Controls, Forms, Variants, cxClasses, cxStorage;

type
  TcxCustomPropertiesStore = class;

  { TcxPropertiesStoreComponent }

  TcxPropertiesStoreComponent = class(TcxInterfacedCollectionItem,
    IcxScalableComponent,
    IcxStoredObject,
    IcxStoredParent)
  private
    FComponent: TComponent;
    FProperties: TStrings;
    FPropertiesEx: TStrings;

    procedure ExtractProperties;
    function ExtractPersistentAndPropertyName(AStartPersistent: TPersistent;
      const AStartName: string; var AResultName: string): TPersistent;
    function GetCollectionItemByName(ACollection: TCollection; const AName: string): TCollectionItem;
    function GetPersistentAndPropertyName(const AStartName: string; var AResultName: string): TPersistent;
    function GetStorageModes: TcxStorageModes;
    function GetComponentByName(const AName: string): TComponent;
    function GetUseInterfaceOnly: Boolean;
    procedure InternalGetPropertyValue(const AName: string; var AValue: Variant);
    procedure InternalSetPropertyValue(const AName: string; const AValue: Variant);
    procedure SetComponent(const Value: TComponent);
    procedure SetProperties(const Value: TStrings);
    function TestClassProperty(const AName: string; AObject: TObject): Boolean;
    procedure AssignStorageProperties(AStorage: TcxStorage);
  protected
    // IcxScalableComponent
    procedure ChangeScale(M, D: Integer);
    procedure ScaleForPPI(TargetPPI: Integer);
    // IcxStoredParent
    function CreateChild(const AObjectName, AClassName: string): TObject;
    procedure DeleteChild(const AObjectName: string; AObject: TObject);
    procedure GetChildren(AChildren: TStringList);
    // IcxStoredObject
    function GetObjectName: string;
    function GetProperties(AProperties: TStrings): Boolean;
    procedure GetPropertyValue(const AName: string; var AValue: Variant);
    procedure SetPropertyValue(const AName: string; const AValue: Variant);
    // IUnknown
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; override; stdcall;

    function GetPropertiesStore: TcxCustomPropertiesStore;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;

    procedure RestoreFrom(AStorage: TcxStorage; AReader: TcxCustomReader);
    procedure RestoreFromIniFile(const AStorageName: string);
    procedure RestoreFromRegistry(const AStorageName: string);
    procedure RestoreFromStream(const AStream: TStream);
    procedure StoreTo(AStorage: TcxStorage; AWriter: TcxCustomWriter);
    procedure StoreToIniFile(const AStorageName: string; const AReCreate: Boolean);
    procedure StoreToRegistry(const AStorageName: string; const AReCreate: Boolean);
    procedure StoreToStream(const AStream: TStream; const AReCreate: Boolean);
  published
    property Component: TComponent read FComponent write SetComponent;
    property Properties: TStrings read FProperties write SetProperties;
  end;

  { TcxPropertiesStoreComponents }

  TcxPropertiesStoreComponents = class(TOwnedCollection)
  private
    function GetComponentItem(Index: Integer): TcxPropertiesStoreComponent;
    procedure SetComponentItem(Index: Integer; const Value: TcxPropertiesStoreComponent);
  protected
    function GetPropertiesStore: TcxCustomPropertiesStore;
    procedure RemoveComponent(const AComponent: TComponent);
  public
    function Add: TcxPropertiesStoreComponent;
    function FindComponentItemByComponent(AComponent: TComponent; out AComponentItem: TcxPropertiesStoreComponent): Boolean;
    property ComponentItems[Index: Integer]: TcxPropertiesStoreComponent read GetComponentItem write SetComponentItem; default;
  end;

  { TcxCustomPropertiesStore }

  TcxCustomPropertiesStore = class(TcxCustomComponent)
  private
    FActive: Boolean;
    FComponents: TcxPropertiesStoreComponents;
    FStorageName: string;
    FStorageStream: TStream;
    FStorageType: TcxStorageType;
    FOnCreateHandler: TNotifyEvent;
    FOnDestroyHandler: TNotifyEvent;

    function GetStorageName: string;
    procedure SetComponents(const Value: TcxPropertiesStoreComponents);
  protected
    procedure Loaded; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure OwnerCreate(Sender: TObject);
    procedure OwnerDestroy(Sender: TObject);
    function CreateReader: TcxCustomReader;
    function CreateWriter(AReCreate: Boolean = True): TcxCustomWriter;
    function CreateStorage: TcxStorage;

    property Active: Boolean read FActive write FActive default True;
    property Components: TcxPropertiesStoreComponents read FComponents write SetComponents;
    property StorageName: string read GetStorageName write FStorageName;
    property StorageType: TcxStorageType read FStorageType write FStorageType default stIniFile;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure RestoreFrom; virtual;
    procedure StoreTo(const AReCreate: Boolean = True); virtual;

    property StorageStream: TStream read FStorageStream write FStorageStream;
  end;

  { TcxPropertiesStore }
  TcxPropertiesStore = class(TcxCustomPropertiesStore)
  published
    property Active;
    property Components;
    property StorageName;
    property StorageType;
  end;

implementation

uses
  cxGeometry, cxControls, Windows;

{ TcxPropertiesStoreComponent }

constructor TcxPropertiesStoreComponent.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FProperties := TStringList.Create;
  FPropertiesEx := TStringList.Create;
end;

destructor TcxPropertiesStoreComponent.Destroy;
begin
  FPropertiesEx.Free;
  FProperties.Free;
  inherited Destroy;
end;

procedure TcxPropertiesStoreComponent.Assign(Source: TPersistent);
begin
  if Source is TcxPropertiesStoreComponent then
    with TcxPropertiesStoreComponent(Source) do
    begin
      Self.Component := Component;
      Self.Properties := Properties;
    end
  else
    inherited;
end;

procedure TcxPropertiesStoreComponent.RestoreFrom(AStorage: TcxStorage; AReader: TcxCustomReader);
var
  AOriginalParentSize: TPoint;
begin
  ExtractProperties;
  AssignStorageProperties(AStorage);
  AOriginalParentSize := TcxControlHelper.GetOriginalParentSize(Component);
  AStorage.RestoreWithExistingReader(Self, AReader);
  if TcxControlHelper.GetCurrentDPIFromControl(Component) <> AStorage.StoredDPI then
    TcxControlHelper.SetOriginalParentSize(Component, AOriginalParentSize);
end;

procedure TcxPropertiesStoreComponent.RestoreFromIniFile(const AStorageName: string);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStorageName);
  try
    AssignStorageProperties(AStorage);
    AStorage.RestoreFromIni(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.RestoreFromRegistry(const AStorageName: string);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStorageName);
  try
    AssignStorageProperties(AStorage);
    AStorage.RestoreFromRegistry(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.RestoreFromStream(const AStream: TStream);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStream);
  try
    AssignStorageProperties(AStorage);
    AStorage.RestoreFromStream(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.StoreTo(AStorage: TcxStorage; AWriter: TcxCustomWriter);
begin
  ExtractProperties;
  with AStorage do
  begin
    AssignStorageProperties(AStorage);
    ReCreate := AWriter.ReCreate;
    StoreWithExistingWriter(Self, AWriter);
  end;
end;

procedure TcxPropertiesStoreComponent.StoreToIniFile(const AStorageName: string; const AReCreate: Boolean);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStorageName);
  try
    AssignStorageProperties(AStorage);
    AStorage.ReCreate := AReCreate;
    AStorage.StoreToIni(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.StoreToRegistry(const AStorageName: string; const AReCreate: Boolean);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStorageName);
  try
    AssignStorageProperties(AStorage);
    AStorage.ReCreate := AReCreate;
    AStorage.StoreToRegistry(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.StoreToStream(const AStream: TStream; const AReCreate: Boolean);
var
  AStorage: TcxStorage;
begin
  ExtractProperties;
  AStorage := TcxStorage.Create(AStream);
  try
    AssignStorageProperties(AStorage);
    AStorage.ReCreate := AReCreate;
    AStorage.StoreToStream(Self);
  finally
    AStorage.Free;
  end;
end;

procedure TcxPropertiesStoreComponent.ChangeScale(M, D: Integer);
begin
  // do nothing
end;

procedure TcxPropertiesStoreComponent.ScaleForPPI(TargetPPI: Integer);
begin
  TcxControlHelper.ScaleForPPI(FComponent, TargetPPI);
end;

function TcxPropertiesStoreComponent.CreateChild(const AObjectName, AClassName: string): TObject;
begin
  Result := nil;
end;

procedure TcxPropertiesStoreComponent.DeleteChild(const AObjectName: string; AObject: TObject);
begin
  if AObject is TCollectionItem then
    AObject.Free;
end;

procedure TcxPropertiesStoreComponent.GetChildren(AChildren: TStringList);
var
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
  I: Integer;
  APersistent: TPersistent;
  APropName: string;
  AChild: TObject;
begin
  AChildren.Clear;
  for I := 0 to FProperties.Count - 1 do
  begin
    APersistent := GetPersistentAndPropertyName(FProperties[I], APropName);
    if (APersistent <> nil) and (APropName <> '') then
    begin
      ATypeInfo := APersistent.ClassInfo;
      APropInfo := GetPropInfo(ATypeInfo, APropName);
      if APropInfo <> nil then
      begin
        if APropInfo^.PropType^.Kind = tkClass then
        begin
          AChild := GetObjectProp(APersistent, APropInfo);
          if (AChild is TPersistent) and not (AChild is TComponent) then
            AChildren.AddObject(FProperties[I]{APropName}, AChild);
        end;
      end
      else
      begin
        if APersistent is TCollection then
        begin
          AChild := GetCollectionItemByName(APersistent as TCollection, APropName);
          if AChild <> nil then
            AChildren.AddObject(FProperties[I]{APropName}, AChild);
        end;
      end;
    end;
  end;
end;

function TcxPropertiesStoreComponent.GetObjectName: string;
begin
  if FComponent <> nil then
    Result := FComponent.Name
  else
    Result := '';
  if Result = '' then
    Result := 'Component' + IntToStr(Index);
end;

function TcxPropertiesStoreComponent.GetProperties(AProperties: TStrings): Boolean;
var
  I: Integer;
begin
  for I := 0 to FProperties.Count - 1 do
    AProperties.Add(FProperties[I]);
  Result := True;
end;

procedure TcxPropertiesStoreComponent.GetPropertyValue(const AName: string; var AValue: Variant);
begin
  if FComponent <> nil then
    InternalGetPropertyValue(AName, AValue)
  else
    AValue := Null;
end;

procedure TcxPropertiesStoreComponent.SetPropertyValue(const AName: string; const AValue: Variant);
begin
  if FComponent <> nil then
    InternalSetPropertyValue(AName, AValue);
end;

function TcxPropertiesStoreComponent.QueryInterface(const IID: TGUID; out Obj): HRESULT;
begin
  Result := inherited QueryInterface(IID, Obj);
  if (Result = E_NOINTERFACE) and IsEqualGUID(IID, IdxScaleFactor) then
  begin
    if Supports(FComponent, IID, Obj) then
      Result := S_OK;
  end;
end;

function TcxPropertiesStoreComponent.GetPropertiesStore: TcxCustomPropertiesStore;
begin
  Result := TcxPropertiesStoreComponents(Collection).GetPropertiesStore;
end;

procedure TcxPropertiesStoreComponent.ExtractProperties;
var
  I: Integer;
  APersistent: TPersistent;
  AName: string;
begin
  FPropertiesEx.Clear;
  for I := 0 to FProperties.Count - 1 do
  begin
    APersistent := ExtractPersistentAndPropertyName(FComponent, FProperties[I], AName);
    FPropertiesEx.AddObject(AName, APersistent);
  end;
end;

function TcxPropertiesStoreComponent.ExtractPersistentAndPropertyName(
  AStartPersistent: TPersistent; const AStartName: string;
  var AResultName: string): TPersistent;

  function ExtractName(var AFName: string): string;
  var
    AIndex: Integer;
  begin
    Result := '';
    AIndex := Pos('.', AFName);
    if AIndex > 0 then
    begin
      if AIndex > 1 then
        Result := Copy(AFName, 1, AIndex - 1);
      Delete(AFName, 1, AIndex);
    end
    else
    begin
      Result := AFName;
      AFName := '';
    end;
  end;

  function GetPersistentByName(const AName: string): TPersistent;
  var
    ATypeInfo: PTypeInfo;
    APropInfo: PPropInfo;
    AObject: TObject;
  begin
    Result := nil;
    ATypeInfo := AStartPersistent.ClassInfo;
    APropInfo := GetPropInfo(ATypeInfo, AName);
    if APropInfo <> nil then
    begin
      if APropInfo^.PropType^.Kind = tkClass then
      begin
        AObject := GetObjectProp(AStartPersistent, APropInfo);
        if (AObject is TPersistent) and not (AObject is TComponent)then
          Result := AObject as TPersistent;
      end;
    end
    else
      if AStartPersistent is TCollection then
        Result := GetCollectionItemByName(AStartPersistent as TCollection, AName);
  end;
var
  AFullName: string;
  APersistent: TPersistent;
begin
  Result := nil;
  AFullName := AStartName;
  AResultName := ExtractName(AFullName);
  if AResultName <> '' then
  begin
    if AFullName = '' then
      Result := AStartPersistent
    else
    begin
      APersistent := GetPersistentByName(AResultName);
      if APersistent <> nil then
        Result := ExtractPersistentAndPropertyName(APersistent, AFullName, AResultName);
    end;
  end;
end;

function TcxPropertiesStoreComponent.GetCollectionItemByName(ACollection: TCollection;
  const AName: string): TCollectionItem;
var
  AInteger: Integer;
  ACode: Integer;
begin
  Result := nil;
  Val(AName, AInteger, ACode);
  if ACode = 0 then
    if (AInteger >= 0) and (AInteger < ACollection.Count) then
      Result := ACollection.Items[AInteger];
end;

function TcxPropertiesStoreComponent.GetPersistentAndPropertyName(
  const AStartName: string; var AResultName: string): TPersistent;

  function GetPropIndex: Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to FProperties.Count - 1 do
      if FProperties[I] = AStartName then
      begin
        Result := I;
        Break;
      end;
  end;
var
  AIndex: Integer;
begin
  Result := nil;
  AResultName := '';
  AIndex := GetPropIndex;
  if (AIndex >= 0) and (AIndex < FProperties.Count) then
    if FPropertiesEx.Objects[AIndex] <> nil then
    begin
      Result := FPropertiesEx.Objects[AIndex] as TPersistent;
      AResultName := FPropertiesEx[AIndex];
    end;
end;

function TcxPropertiesStoreComponent.GetStorageModes: TcxStorageModes;
begin
  Result := [smChildrenCreating, smChildrenDeleting];
end;

function TcxPropertiesStoreComponent.GetComponentByName(const AName: string): TComponent;
var
  AComponent: TComponent;
begin
  Result := nil;
  AComponent := GetPropertiesStore.GetParentComponent;
  if AComponent <> nil then
    Result := AComponent.FindComponent(AName);
  if Result = nil then
  begin
    AComponent := GetPropertiesStore.Owner;
    if AComponent <> nil then
      Result := AComponent.FindComponent(AName);
  end;
end;

function TcxPropertiesStoreComponent.GetUseInterfaceOnly: Boolean;
begin
  Result := True;
end;

procedure TcxPropertiesStoreComponent.InternalGetPropertyValue(const AName: string;
  var AValue: Variant);
var
  APersistent: TPersistent;
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
  APropName: string;
  AObject: TObject;
begin
  AValue := Null;
  APersistent := GetPersistentAndPropertyName(AName, APropName);
  if (APersistent <> nil) and (APropName <> '') then
  begin
    ATypeInfo := APersistent.ClassInfo;
    if ATypeInfo <> nil then
    begin
      APropInfo := GetPropInfo(ATypeInfo, APropName);
      if APropInfo <> nil then
      begin
        case APropInfo^.PropType^.Kind of
          tkInteger, tkChar, tkWChar:
            AValue := GetOrdProp(APersistent, APropInfo);
          tkEnumeration:
            AValue := GetEnumProp(APersistent, APropInfo);
          tkFloat:
            AValue := GetFloatProp(APersistent, APropInfo);
          tkString, tkLString, tkUString:
            AValue := GetStrProp(APersistent, APropInfo);
          tkWString:
            AValue := GetWideStrProp(APersistent, APropInfo);
          tkInt64:
            AValue := GetInt64Prop(APersistent, APropInfo);
          tkSet:
            AValue := GetSetProp(APersistent, APropInfo, True);
          tkVariant:
            AValue := GetVariantProp(APersistent, APropInfo);
          tkClass:
          begin
            AObject := GetObjectProp(APersistent, APropInfo);
            if AObject = nil then
              AValue := ''
            else if AObject is TComponent then
              AValue := TComponent(AObject).Name;
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxPropertiesStoreComponent.InternalSetPropertyValue(const AName: string;
  const AValue: Variant);
var
  ATypeInfo: PTypeInfo;
  APropInfo: PPropInfo;
  AInt64: Int64;
  APersistent: TPersistent;
  APropName: string;
  AComponent: TComponent;
  AParentComponent: TComponent;
  AOwner: TComponent;
  AComponentName: string;
begin
  if not VarIsNull(AValue) then
  begin
    APersistent := GetPersistentAndPropertyName(AName, APropName);
    if (APersistent <> nil) and (APropName <> '') then
    begin
      ATypeInfo := APersistent.ClassInfo;
      if ATypeInfo <> nil then
      begin
        APropInfo := GetPropInfo(ATypeInfo, APropName);
        if APropInfo <> nil then
        begin
          case APropInfo^.PropType^.Kind of
            tkInteger, tkChar, tkWChar:
              SetOrdProp(APersistent, APropInfo, AValue);
            tkEnumeration:
              SetEnumProp(APersistent, APropInfo, AValue);
            tkFloat:
              SetFloatProp(APersistent, APropInfo, AValue);
            tkString, tkLString, tkUString:
              SetStrProp(APersistent, APropName, VarToStr(AValue));
            tkWString:
              SetWideStrProp(APersistent, APropInfo, AValue);
            tkInt64:
              begin
                AInt64 := AValue;
                SetInt64Prop(APersistent, APropInfo, AInt64);
              end;
            tkSet:
              SetSetProp(APersistent, APropInfo, AValue);
            tkVariant:
              SetVariantProp(APersistent, APropInfo, AValue);
            tkClass:
              begin
                AComponentName := AValue;
                if AComponentName = '' then
                  SetObjectProp(APersistent, APropInfo, nil)
                else
                begin
                  AComponent := nil;
                  if FComponent is TControl then
                    AComponent := GetParentForm(FComponent as TControl).FindComponent(AComponentName);
                  if AComponent = nil then
                  begin
                    AParentComponent := FComponent.GetParentComponent;
                    if AParentComponent <> nil then
                      AComponent := AParentComponent.FindComponent(AComponentName);
                    if AComponent = nil then
                    begin
                      AOwner := FComponent.Owner;
                      if AOwner <> nil then
                        AComponent := AOwner.FindComponent(AComponentName);
                    end;
                  end;
                  if AComponent <> nil then
                    SetObjectProp(APersistent, APropInfo, AComponent);
                end;
              end;
          end;
        end;
      end;
    end;
  end;
end;

procedure TcxPropertiesStoreComponent.SetComponent(const Value: TComponent);
begin
  if Component <> Value then
  begin
    if (Component <> nil) and not (csDestroying in Component.ComponentState) then
      Component.RemoveFreeNotification(GetPropertiesStore);
    FComponent := Value;
    if (Component <> nil) then
      Component.FreeNotification(GetPropertiesStore);
  end;
end;

procedure TcxPropertiesStoreComponent.SetProperties(const Value: TStrings);
begin
  FProperties.Assign(Value);
end;

function TcxPropertiesStoreComponent.TestClassProperty(const AName: string;
  AObject: TObject): Boolean;
begin
  Result := (AObject is TPersistent) and not (AObject is TComponent);
end;

procedure TcxPropertiesStoreComponent.AssignStorageProperties(AStorage: TcxStorage);
begin
  with AStorage do
  begin
    Modes := [smSavePublishedClassProperties, smChildrenCreating, smChildrenDeleting];
    OnGetStorageModes := GetStorageModes;
    OnTestClassProperty := TestClassProperty;
    OnGetComponentByName := GetComponentByName;
    OnGetUseInterfaceOnly := GetUseInterfaceOnly;
    SaveComponentPropertiesByName := True;
  end;
end;

{ TcxPropertiesStoreComponents }

function TcxPropertiesStoreComponents.Add: TcxPropertiesStoreComponent;
begin
  Result := inherited Add as TcxPropertiesStoreComponent;
end;

function TcxPropertiesStoreComponents.FindComponentItemByComponent(
  AComponent: TComponent; out AComponentItem: TcxPropertiesStoreComponent): Boolean;
var
  I: Integer;
begin
  Result := False;
  for I := 0 to Count - 1 do
    if ComponentItems[I].Component = AComponent then
    begin
      AComponentItem := ComponentItems[I];
      Result := True;
      Break;
    end;
end;

function TcxPropertiesStoreComponents.GetPropertiesStore: TcxCustomPropertiesStore;
begin
  Result := TcxPropertiesStore(Owner);
end;

procedure TcxPropertiesStoreComponents.RemoveComponent(
  const AComponent: TComponent);
var
  AList: TList;
  I: Integer;
begin
  AList := TList.Create;
  try
    for I := 0 to Count - 1 do
      if ComponentItems[I].Component = AComponent then
        AList.Add(ComponentItems[I]);
    for I := 0 to AList.Count - 1 do
      TcxPropertiesStoreComponent(AList[I]).Free;
  finally
    AList.Free;
  end;
end;

function TcxPropertiesStoreComponents.GetComponentItem(
  Index: Integer): TcxPropertiesStoreComponent;
begin
  Result := Items[Index] as TcxPropertiesStoreComponent;
end;

procedure TcxPropertiesStoreComponents.SetComponentItem(Index: Integer;
  const Value: TcxPropertiesStoreComponent);
begin
  ComponentItems[Index].Assign(Value);
end;

{ TcxCustomPropertiesStore }

constructor TcxCustomPropertiesStore.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FComponents := TcxPropertiesStoreComponents.Create(self, TcxPropertiesStoreComponent);
  FStorageName := '';
  FStorageType := stIniFile;
  FStorageStream := nil;
  FActive := True;
end;

destructor TcxCustomPropertiesStore.Destroy;
begin
  FComponents.Free;
  inherited Destroy;
end;

procedure TcxCustomPropertiesStore.RestoreFrom;
var
  I: Integer;
  AReader: TcxCustomReader;
  AStorage: TcxStorage;
begin
  AReader := CreateReader;
  AStorage := CreateStorage;
  try
    for I := 0 to Components.Count - 1 do
      Components[I].RestoreFrom(AStorage, AReader);
  finally
    AStorage.Free;
    AReader.Free;
  end;
end;

procedure TcxCustomPropertiesStore.StoreTo(const AReCreate: Boolean);
var
  I: Integer;
  AWriter: TcxCustomWriter;
  AStorage: TcxStorage;
begin
  AStorage := CreateStorage;
  try
    if Components.Count > 0 then
    begin
      AWriter := CreateWriter(AReCreate);
      try
        Components[0].StoreTo(AStorage, AWriter);
        AWriter.ReCreate := False;
        for I := 1 to Components.Count - 1 do
          Components[I].StoreTo(AStorage, AWriter);
      finally
        AWriter.Free;
      end;
    end;
  finally
    AStorage.Free;
  end;
end;

procedure TcxCustomPropertiesStore.Loaded;
var
  AMyOwnerCreate: TNotifyEvent;
begin
  inherited Loaded;
  if not (csDesigning in ComponentState) then
  begin
    if Owner <> nil then
    begin
      AMyOwnerCreate := OwnerCreate;
      if Owner is TForm then
      begin
        FOnCreateHandler := TForm(Owner).OnCreate;
        FOnDestroyHandler := TForm(Owner).OnDestroy;
        TForm(Owner).OnCreate := OwnerCreate;
        TForm(Owner).OnDestroy := OwnerDestroy;
      end
      else if Owner is TDataModule then
      begin
        FOnCreateHandler := TDataModule(Owner).OnCreate;
        FOnDestroyHandler := TDataModule(Owner).OnDestroy;
        TDataModule(Owner).OnCreate := OwnerCreate;
        TDataModule(Owner).OnDestroy := OwnerDestroy;
      end;
    end;
  end;
end;

procedure TcxCustomPropertiesStore.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if not (csDestroying in ComponentState) and (Operation = opRemove) then
      Components.RemoveComponent(AComponent);
end;

procedure TcxCustomPropertiesStore.OwnerCreate(Sender: TObject);
begin
  if FActive then
    RestoreFrom;
  if Assigned(FOnCreateHandler) then
    FOnCreateHandler(Sender);
end;

procedure TcxCustomPropertiesStore.OwnerDestroy(Sender: TObject);
begin
  if Assigned(FOnDestroyHandler) then
    FOnDestroyHandler(Sender);
  if FActive then
  begin
    if StorageType <> stStream then
      StoreTo;
  end;
end;

function TcxCustomPropertiesStore.CreateReader: TcxCustomReader;
begin
  Result := nil;
  case FStorageType of
    stIniFile:
      Result := TcxIniFileReader.Create(StorageName);
    stRegistry:
      Result := TcxRegistryReader.Create(StorageName);
    stStream:
    begin
      Result := TcxStreamReader.Create(StorageName);
      (Result as TcxStreamReader).SetStream(FStorageStream);
    end;
  end;
end;

function TcxCustomPropertiesStore.CreateWriter(AReCreate: Boolean): TcxCustomWriter;
begin
  Result := nil;
  case FStorageType of
    stIniFile:
      Result := TcxIniFileWriter.Create(StorageName, AReCreate);
    stRegistry:
      Result := TcxRegistryWriter.Create(StorageName, AReCreate);
    stStream:
    begin
      Result := TcxStreamWriter.Create(StorageName, AReCreate);
      (Result as TcxStreamWriter).SetStream(FStorageStream);
    end;
  end;
end;

function TcxCustomPropertiesStore.CreateStorage: TcxStorage;
begin
  Result := nil;
  case FStorageType of
    stIniFile, stRegistry:
      Result := TcxStorage.Create(StorageName);
    stStream:
      Result := TcxStorage.Create(FStorageStream);
  end;
end;

function TcxCustomPropertiesStore.GetStorageName: string;
begin
  if FStorageName <> '' then
    Result := FStorageName
  else
    Result := Name;
end;

procedure TcxCustomPropertiesStore.SetComponents(
  const Value: TcxPropertiesStoreComponents);
begin
  Components.Assign(Value);
end;

end.

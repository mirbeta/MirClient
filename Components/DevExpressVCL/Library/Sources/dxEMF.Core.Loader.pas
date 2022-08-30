{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressEntityMapping Framework                           }
{                                                                    }
{           Copyright (c) 2016-2019 Developer Express Inc.           }
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
{   (DCU, OBJ, DLL, DPU, SO, ETC.) ARE CONFIDENTIAL AND PROPRIETARY  }
{   TRADE SECRETS OF DEVELOPER EXPRESS INC. THE REGISTERED DEVELOPER }
{   IS LICENSED TO DISTRIBUTE THE EXPRESSENTITYMAPPING FRAMEWORK     }
{   AS PART OF AN EXECUTABLE PROGRAM ONLY.                           }
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

unit dxEMF.Core.Loader;

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  Types, SysUtils, Generics.Defaults, Generics.Collections,
  TypInfo, Rtti, DB,
  dxEMF.Metadata,
  dxEMF.Types,
  dxEMF.Core.Pool,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query;

type

  TdxQueryData = class;

  { TdxDataLoader }

  TdxDataLoaderFieldDef = record
    DataType: TFieldType;
    Precision: Integer;
    Size: Integer;
    constructor Create(const AValue: TValue);
  end;

  TdxDataLoader = class
  strict private
    FSession: IdxSession;
    FEntityInfo: TdxEntityInfo;
    FLoadingStrategy: TdxLoadingStrategy;
    FReadRecords: Integer;
    FLoadedRecords: Integer;
    FLoadClasses: TClass;
    FDataInfo: PTypeInfo;
    FPredicate: TdxPredicate;
    FTopReturnedObjects: Integer;
    FSkipReturnedObjects: Integer;
    FForceReload: Boolean;
    FPool: TdxCustomEntityPool;
    FQuery: TdxQuery;
    procedure AppendObject(const AKeys: TArray<TValue>; AObject: TObject); inline;
    function GetLoadedObjectByKey(const AKeys: TArray<TValue>): TObject; inline;
    procedure SetLoadingClass(Value: TClass);
    procedure SetDataInfo(const Value: PTypeInfo);
    procedure SetEntityInfo(const Value: TdxEntityInfo);
  protected
    procedure BeginLoad; virtual;
    function GetEof: boolean; virtual;
    function GetRecordCount: Integer; virtual; abstract;
    function GetFieldCount: Integer; virtual; abstract;
    function GetFieldNames(AIndex: Integer): string; virtual; abstract;
    function GetFieldDef(AIndex: Integer): TdxDataLoaderFieldDef; virtual; abstract;
    function MoveNext: Boolean; virtual; abstract;
    function ReadCurrentRecordKeys: TArray<TValue>; virtual; abstract;
    procedure ReadCurrentRecord(AInstance: Pointer); overload; virtual; abstract;
    procedure ReadCurrentRecord(out AValues: TArray<Variant>); overload; virtual; abstract;
    procedure ReadCurrentRecord(out AValues: TArray<TValue>); overload; virtual; abstract;

    property ReadRecords: Integer read FReadRecords;
  public
    constructor Create(const ASession: IdxSession); overload; virtual;
    destructor Destroy; override;
    procedure Next; inline;
    function CheckObject(AObject: TObject): Boolean;
    function GetNextObject: TObject;

    property DataTypeInfo: PTypeInfo read FDataInfo write SetDataInfo;
    property EntityInfo: TdxEntityInfo read FEntityInfo write SetEntityInfo;
    property Query: TdxQuery read FQuery write FQuery;
    property Eof: boolean read GetEof;
    property ForceReload: Boolean read FForceReload write FForceReload;
    property FieldCount: Integer read GetFieldCount;
    property FieldNames[AIndex: Integer]: string read GetFieldNames;
    property FieldDef[AIndex: Integer]: TdxDataLoaderFieldDef read GetFieldDef;
    property LoadedRecords: Integer read FLoadedRecords;
    property LoadingClass: TClass read FLoadClasses write SetLoadingClass;
    property LoadingStrategy: TdxLoadingStrategy read FLoadingStrategy write FLoadingStrategy;
    property Predicate: TdxPredicate read FPredicate write FPredicate;
    property RecordCount: Integer read GetRecordCount;
    property Session: IdxSession read FSession write FSession;
    property SkipReturnedObjects: Integer read FSkipReturnedObjects write FSkipReturnedObjects;
    property TopReturnedObjects: Integer read FTopReturnedObjects write FTopReturnedObjects;
  end;

  { TdxSelectStatementResultRow }

  TdxSelectStatementResultRow = class
  strict private
    FValues: TArray<Variant>;
  public
    constructor Create(const AValues: TArray<Variant>);
    property Values: TArray<Variant> read FValues;
  end;

  { TdxSelectStatementResult }

  TdxSelectStatementResult = class
  strict private
    FRows: TList<TdxSelectStatementResultRow>;
    FQueryData: TdxQueryData;
    FFieldTypes: TArray<TFieldType>;
    function GetEntityInfo: TdxEntityInfo; // inline;
    function GetCapacity: Integer;
    procedure SetCapacity(const Value: Integer);
    function GetRows(AIndex: Integer): TdxSelectStatementResultRow; inline;
    function GetCount: Integer; inline;
  protected
    procedure Add(const ARow: TArray<Variant>);
//    procedure SetFieldTypes(ADataSet: TDataSet);
    procedure Load;
    property EntityInfo: TdxEntityInfo read GetEntityInfo;
    property FieldTypes: TArray<TFieldType> read FFieldTypes write FFieldTypes;
    property Capacity: Integer read GetCapacity write SetCapacity;
    property Rows: TList<TdxSelectStatementResultRow> read FRows;
  public
    constructor Create; overload;
    constructor Create(AQueryData: TdxQueryData); overload;
    constructor Create(const ARows: TArray<TdxSelectStatementResultRow>); overload;
    destructor Destroy; override;
    property Count: Integer read GetCount;
    property Items[AIndex: Integer]: TdxSelectStatementResultRow read GetRows; default;
  end;

  { TdxQueryData }

  TdxQueryData = class
  protected type
{$REGION 'ValueAccessor'}

    { TdxValueAccessor }

    TdxValueAccessor = class abstract
    strict private
      FMember: TdxMappingMemberInfo;
    protected
      function GetValue: TValue; virtual; abstract;
      procedure SetRowValue(const ARow: TArray<TValue>; var AIndex: Integer); virtual;
      property Member: TdxMappingMemberInfo read FMember;
    public
      constructor Create(AMember: TdxMappingMemberInfo);
      class function CreateAccessor(AData: TdxQueryData; AMember: TdxMappingMemberInfo; var AIndex: Integer): TdxValueAccessor; static;

      property Value: TValue read GetValue;
    end;

    { TdxTypedRefAccessor }

    TdxTypedRefAccessor = class(TdxValueAccessor)
    strict private
      FValue: TdxValueAccessor;
      FType: TdxValueAccessor;
      function GetTypeId: TValue;
    protected
      function GetValue: TValue; override;
    public
      constructor Create(AValue: TdxValueAccessor; AType: TdxValueAccessor);

      property TypeId: TValue read GetTypeId;
    end;

    { TdxSimpleValueAccessor }

    TdxSimpleValueAccessor = class(TdxValueAccessor)
    strict private
      FData: TdxQueryData;
      FIndex: Integer;
    protected
      function GetValue: TValue; override;
    public
      constructor Create(AData: TdxQueryData; AIndex: Integer; AMember: TdxMappingMemberInfo);
    end;


    { TdxNullValueAccessor }

    TdxNullValueAccessor = class(TdxValueAccessor)
    protected
      function GetValue: TValue; override;
    end;

    { TdxSubValueAccessor }

    TdxSubValueAccessor = class(TdxValueAccessor)
    strict private
      FNested: TList<TdxValueAccessor>;
    protected
      function GetValue: TValue; override;
      procedure SetRowValue(const ARow: TArray<TValue>; var AIndex: Integer); override;
    public
      constructor Create(ANested: TList<TdxValueAccessor>; AMember: TdxMappingMemberInfo);
    end;
{$ENDREGION}
  strict private
    FProperties: TdxMemberPathCollection;
    FDataLoader: TdxDataLoader;
    FPos: Integer;
    FItemCount: Integer;
    FNestedDataDictionary: TDictionary<TdxMappingMemberInfo, TdxQueryData>;
    FEntityInfo: TdxEntityInfo;
    FInternalEntityInfo: TdxEntityInfo;
    FLinearMembers: TList<TdxValueAccessor>;
    function GetEof: Boolean;
    function GetCount: Integer;
    function GetFieldCount: Integer;
    function GetEntityInfo: TdxEntityInfo;
  protected
    function GetNestedData(AMemberInfo: TdxMappingMemberInfo): TdxQueryData;
    procedure AddMember(AMemberInfo: TdxMappingMemberInfo; ARoot: TdxQueryData; AParentPath: TdxMemberInfoCollection;
      var AIndex: Integer);
    procedure Init(ARoot: TdxQueryData; AEntityInfo: TdxEntityInfo; AParentPath: TdxMemberInfoCollection; var AIndex: Integer);
    procedure CreateInternalEntityInfo(const AProperties: TArray<IdxCriteriaOperator>);
    procedure SetDataLoader(ADataLoader: TdxDataLoader);
    property FieldCount: Integer read GetFieldCount;
  public
    constructor Create; overload;
    constructor Create(AEntityInfo: TdxEntityInfo); overload;
    constructor Create(AEntityInfo: TdxEntityInfo; const AProperties: TArray<IdxCriteriaOperator>;
      ADataLoader: TdxDataLoader); overload;
    destructor Destroy; override;

    function GetRowValues: TArray<Variant>;
    function MoveNext: Boolean;
    property Eof: Boolean read GetEof;

    property EntityInfo: TdxEntityInfo read GetEntityInfo;
    property DataLoader: TdxDataLoader read FDataLoader;
    property Properties: TdxMemberPathCollection read FProperties;
    property Count: Integer read GetCount;
  end;

  { TdxQueryDataPool }

  TdxQueryDataPool = class(TObjectDictionary<TdxEntityInfo, TdxQueryData>)
  public
    constructor Create;
    function GetQueryData(AEntityInfo: TdxEntityInfo): TdxQueryData;
  end;

  { TdxSelectStatementCollections }

  TdxSelectStatementCollections = class sealed
  public
    class function Create(ASelectStatementResult: TdxSelectStatementResult): IdxEMFCollection; overload; static;
  end;

implementation

uses
  dxCore,
  FmtBcd,
  dxEMF.Core,
  dxEMF.Core.Collections,
  dxEMF.DB.Generator,
  dxEMF.DB.Utils,
  dxEMF.Utils;

type

  TdxEMFCustomSessionAccess = class(TdxEMFCustomSession);
  TdxMappingClassInfoAccess = class(TdxMappingClassInfo);
  TdxMappingMemberInfoAccess = class(TdxMappingMemberInfo);

  { TdxSelectStatementResultMemberInfo }

  TdxSelectStatementResultMemberInfo = class(TdxMappingMemberInfo)
  private
    FIndex: Integer;
    FOriginal: TdxMappingMemberInfo;
  protected
    property Index: Integer read FIndex write FIndex;
  public
    constructor Create(AOwner: TdxEntityInfo; AIndex: Integer; AOriginal: TdxMappingMemberInfo);
    function GetValue(AObject: TObject): TValue; override;
    property Original: TdxMappingMemberInfo read FOriginal;
  end;

  { TdxSelectStatementConstantMemberInfo }

  TdxSelectStatementConstantMemberInfo = class(TdxMappingMemberInfo)
  private
    FOperandValue: IdxOperandValue;
  protected
    function GetIsLoadable: Boolean; override;
  public
    constructor Create(AOwner: TdxEntityInfo; const AOperandValue: IdxOperandValue);
    function GetValue(AObject: TObject): TValue; override;
    procedure SetValue(AObject: TObject; const AValue: TValue); override;
  end;

  { TdxSelectStatementResultEntityInfo }

  TdxSelectStatementResultEntityInfo = class(TdxEntityInfo)
  strict private
    FQueryData: TdxQueryData;
    function UniqueName(const AName: string): string;
    function IsUniqueName(const AName: string): Boolean;
    procedure Make(const AProperties: TArray<IdxCriteriaOperator>);
  public
    constructor Create(AQueryData: TdxQueryData; const AProperties: TArray<IdxCriteriaOperator>);
  end;

  { TdxEMFSelectStatementResultCollection }

  TdxEMFSelectStatementResultCollection = class(TdxEMFBaseCollection, IdxCollection, IdxList, IdxEMFCollection)
  strict private type

    TEnumerator = class(TInterfacedObject, IEnumerator)
    private
      FSelectStatementResult: TdxSelectStatementResult;
      FIndex: Integer;
    protected
      { IEnumerator }
      function GetCurrent: TObject;
    public
      constructor Create(ASelectStatementResult: TdxSelectStatementResult);
      function MoveNext: Boolean;
      procedure Reset;
      property Current: TObject read GetCurrent;
    end;

  strict private
    FSelectStatementResult: TdxSelectStatementResult;
  protected
    procedure CheckDataType; override;
    procedure Clear; override;
    procedure FetchAll; override;
    function LoadNextObject: Boolean; override;
    function GetCollectionElementClass: TClass; override;
    function InternalGetEnumerator: IEnumerator; override;
    { IdxCollection }
    function GetCount: Integer;
    { IdxList }
    function Add(AObject: TObject): Integer;
    function Contains(AValue: TObject): Boolean;
    function GetItems(AIndex: Integer): TObject;
    procedure Remove(AObject: TObject);
    function IndexOf(AValue: TObject): Integer;
    { IdxEMFCollection }
    function First: TObject;
    function Last: TObject;
    function GetDeleteObjectOnRemove: Boolean;
    procedure SetDeleteObjectOnRemove(Value: Boolean);
  public
    constructor Create(ASelectStatementResult: TdxSelectStatementResult);
    destructor Destroy; override;
    procedure Load; override;
    procedure Insert(AIndex: Integer; AObject: TObject); override;
  end;

{ TdxDataLoaderFieldDef }

constructor TdxDataLoaderFieldDef.Create(const AValue: TValue);
begin
  if AValue.IsString then
    Size := Length(AValue.AsString);
  DataType := AValue.GetFieldType;
  Precision := 0;
end;

{ TdxSelectStatementResultMemberInfo }

constructor TdxSelectStatementResultMemberInfo.Create(AOwner: TdxEntityInfo; AIndex: Integer;
  AOriginal: TdxMappingMemberInfo);
begin
  inherited Create(AOwner);
  FIndex := AIndex;
  FOriginal := AOriginal;
  if AOriginal <> nil then
  begin
    RttiType := AOriginal.RttiType;
    TypeKind := AOriginal.TypeKind;
  end;
end;

function TdxSelectStatementResultMemberInfo.GetValue(AObject: TObject): TValue;
var
  AValue: Variant;
begin
  Assert(AObject is TdxSelectStatementResultRow);
  AValue := TdxSelectStatementResultRow(AObject).Values[FIndex];
  case FieldType of
    ftFMTBcd:
      Result := TValue.From<TBCD>(VarToBcd(AValue));
  else
    Result := TValue.FromVariant(AValue);
  end
end;

{ TdxSelectStatementConstantMemberInfo }

constructor TdxSelectStatementConstantMemberInfo.Create(AOwner: TdxEntityInfo; const AOperandValue: IdxOperandValue);
begin
  inherited Create(AOwner);
  FOperandValue := AOperandValue;
end;

function TdxSelectStatementConstantMemberInfo.GetIsLoadable: Boolean;
begin
  Result := False;
end;

function TdxSelectStatementConstantMemberInfo.GetValue(AObject: TObject): TValue;
begin
  Result := FOperandValue.Value;
end;

procedure TdxSelectStatementConstantMemberInfo.SetValue(AObject: TObject; const AValue: TValue);
begin
end;

{ TdxSelectStatementResultEntityInfo }

constructor TdxSelectStatementResultEntityInfo.Create(AQueryData: TdxQueryData; const AProperties: TArray<IdxCriteriaOperator>);
begin
  inherited Create;
  FQueryData := AQueryData;
  Make(AProperties);
end;

function TdxSelectStatementResultEntityInfo.IsUniqueName(const AName: string): Boolean;
var
  I: Integer;
begin
  Result := True;
  for I := 0 to MemberAttributes.Count - 1 do
    if SameText(MemberAttributes[I].AttributeName, AName) then
      Exit(False);
end;

procedure TdxSelectStatementResultEntityInfo.Make(const AProperties: TArray<IdxCriteriaOperator>);
var
  I, AFieldCount: Integer;
  AMemberInfo, AParentMemberInfo: TdxMappingMemberInfo;
  AName: string;
  AFieldDef: TdxDataLoaderFieldDef;
  AMappingClassInfo: TdxMappingClassInfo;
  AHasConst: Boolean;
  AQuery: TdxQuery;
  AParentCriteria: IdxParentCriteria;
  AMemberOperand: IdxMemberOperand;
begin
  if
  ((FQueryData.DataLoader = nil) or (FQueryData.DataLoader.FieldCount = 0)) then
    Exit;
  AFieldCount := FQueryData.FieldCount;
  AQuery := FQueryData.DataLoader.Query;
  AHasConst := (AQuery.ConstantValues <> nil) and (AQuery.ConstantValues.Count > 0);
  AMappingClassInfo := TdxMappingClassInfo.Create(Self, FQueryData.DataLoader.EntityInfo.ClassAttributes.PersistentClass);
  TdxMappingClassInfoAccess(AMappingClassInfo).Assign(FQueryData.DataLoader.EntityInfo.ClassAttributes);
  Add(AMappingClassInfo);
  for I := 0 to AFieldCount - 1 do
  begin
    AParentMemberInfo := nil;
    Supports(AProperties[I], IdxParentCriteria, AParentCriteria);
    if AParentCriteria <> nil then
    begin
      Supports(AParentCriteria.ParentCriteria, IdxMemberOperand, AMemberOperand);
      if AMemberOperand <> nil then
        AParentMemberInfo := AMemberOperand.Member;
    end;
    if AHasConst then
    begin
      if AQuery.ConstantValues.ContainsKey(I) then
        AMemberInfo := TdxSelectStatementConstantMemberInfo.Create(Self, AQuery.ConstantValues[I])
      else
        AMemberInfo := TdxSelectStatementResultMemberInfo.Create(Self, AQuery.OperandIndexes[I], AParentMemberInfo);
    end
    else
      AMemberInfo := TdxSelectStatementResultMemberInfo.Create(Self, I, AParentMemberInfo);
    AMemberInfo.Attributes := [TdxAttribute.Column, TdxAttribute.ReadOnly];
    AMemberInfo.AttributeType := TdxAttributeType.Field;

    AFieldDef := FQueryData.DataLoader.FieldDef[I];
    TdxMappingMemberInfoAccess(AMemberInfo).SetFieldType(AFieldDef.DataType);
    TdxMappingMemberInfoAccess(AMemberInfo).Size(AFieldDef.Size);
    AName := FQueryData.DataLoader.FieldNames[I];
    AName := UniqueName(AName);
    TdxMappingMemberInfoAccess(AMemberInfo).SetName(AName);
    TdxMappingMemberInfoAccess(AMemberInfo).SetMemberName(AName);
    Add(AMemberInfo);
  end;
end;

function TdxSelectStatementResultEntityInfo.UniqueName(const AName: string): string;
var
  I: Integer;
begin
  I := 0;
  Result := AName;
  while not IsUniqueName(Result) do
  begin
    Inc(I);
    Result := Format('%s%d', [AName, I]);
  end;
end;

{ TdxDataLoader }

constructor TdxDataLoader.Create(const ASession: IdxSession);
begin
  inherited Create;
  Assert(ASession <> nil, 'Session is nil');
  FSession := ASession;
  FLoadingStrategy := FSession.LoadingStrategy;
end;

destructor TdxDataLoader.Destroy;
begin
  FreeAndNil(FQuery);
  inherited Destroy;
end;

procedure TdxDataLoader.AppendObject(const AKeys: TArray<TValue>; AObject: TObject);
begin
  FPool.Add(AKeys, AObject);
end;

procedure TdxDataLoader.BeginLoad;
begin

end;

function TdxDataLoader.CheckObject(AObject: TObject): Boolean;
begin
  if Assigned(FPredicate) then
    Result := FPredicate(AObject)
  else
    Result := True;
end;

function TdxDataLoader.GetEof: boolean;
begin
  Result := FLoadedRecords >= RecordCount;
end;

function TdxDataLoader.GetLoadedObjectByKey(const AKeys: TArray<TValue>): TObject;
begin
  Assert(FPool <> nil);
  Result := FPool.GetByKey(AKeys);
end;

function TdxDataLoader.GetNextObject: TObject;
var
  ANewObject: TObject;
  ACurrentKeyValue: TArray<TValue>;
  ASession: TdxEMFCustomSession;
begin
  if (Session = nil) or Eof{ not MoveNext } then
    Exit(nil);
  ASession := Session as TdxEMFCustomSession;
  repeat
    Inc(FReadRecords);
    ACurrentKeyValue := ReadCurrentRecordKeys;
    Result := GetLoadedObjectByKey(ACurrentKeyValue);
    if Result = nil then
    begin
      ANewObject := ASession.CreateObject(LoadingClass);
      AppendObject(ACurrentKeyValue, ANewObject);
      TdxEMFCustomSessionAccess(ASession).LoadingObjects.Add(ANewObject);
      try
        ReadCurrentRecord(ANewObject);
      finally
        TdxEMFCustomSessionAccess(ASession).LoadingObjects.Remove(ANewObject);
      end;
      Result := ANewObject;
    end
    else
    begin
      if ForceReload then
        ReadCurrentRecord(Result);
      ANewObject := nil;
    end;
    if CheckObject(Result) then
    begin
      Inc(FLoadedRecords);
      if ANewObject <> nil then
        EntityInfo.InitSession(ASession, ANewObject);
      MoveNext;
      Break;
    end
    else
      Result := nil;
    if ANewObject <> nil then
    begin
      ASession.Detach(ANewObject);
      ANewObject.Free;
    end;
  until not MoveNext;
end;

procedure TdxDataLoader.Next;
begin
  MoveNext;
end;


procedure TdxDataLoader.SetDataInfo(const Value: PTypeInfo);
begin
  if FDataInfo = Value then
    Exit;
  if Value.Kind = tkClass then
    SetLoadingClass(Value.TypeData.ClassType);
  FDataInfo := Value;
end;

procedure TdxDataLoader.SetEntityInfo(const Value: TdxEntityInfo);
begin
  FEntityInfo := Value;
  FLoadClasses := Value.ClassAttributes.PersistentClass;
  FDataInfo := PTypeInfo(FLoadClasses.ClassInfo);
  FPool := TdxEMFCustomSessionAccess(Session as TdxEMFCustomSession).PoolClasses.GetPool(FLoadClasses);
end;

procedure TdxDataLoader.SetLoadingClass(Value: TClass);
begin
  FLoadClasses := Value;
  FEntityInfo := EntityManager.GetEntityInfo(Value);
  FDataInfo := PTypeInfo(FLoadClasses.ClassInfo);
  FPool := TdxEMFCustomSessionAccess(Session as TdxEMFCustomSession).PoolClasses.GetPool(Value);
end;


{ TdxSelectStatementResultRow }

constructor TdxSelectStatementResultRow.Create(const AValues: TArray<Variant>);
begin
  inherited Create;
  FValues := AValues;
end;


{ TdxQueryData.TdxValueAccessor }

constructor TdxQueryData.TdxValueAccessor.Create(AMember: TdxMappingMemberInfo);
begin
  FMember := AMember;
end;

class function TdxQueryData.TdxValueAccessor.CreateAccessor(AData: TdxQueryData; AMember: TdxMappingMemberInfo; var AIndex: Integer): TdxValueAccessor;
var
  ARealMember, ASubMember: TdxMappingMemberInfo;
  ANested: TList<TdxValueAccessor>;
begin
  if AMember.ReferenceType = nil then
    ARealMember := AMember
  else
    ARealMember := AMember.ReferenceType.KeyProperty.Member;
  if Length(ARealMember.SubMembers) = 0 then
  begin
      Result := TdxSimpleValueAccessor.Create(AData, AIndex, AMember);
    Inc(AIndex);
  end
  else
  begin
    ANested := TList<TdxValueAccessor>.Create;
    for ASubMember in ARealMember.SubMembers do
      if ASubMember.IsPersistent then
        ANested.Add(CreateAccessor(AData, ASubMember, AIndex));
    Result := TdxSubValueAccessor.Create(ANested, AMember);
  end;
end;

procedure TdxQueryData.TdxValueAccessor.SetRowValue(const ARow: TArray<TValue>; var AIndex: Integer);
begin
  ARow[AIndex] := Value;
  Inc(AIndex);
end;

{ TdxQueryData.TdxTypedRefAccessor }

constructor TdxQueryData.TdxTypedRefAccessor.Create(AValue: TdxValueAccessor; AType: TdxValueAccessor);
begin
  inherited Create(nil);
  FValue := AValue;
  FType := AType;
end;

function TdxQueryData.TdxTypedRefAccessor.GetValue: TValue;
begin
  Result := FValue.Value;
end;

function TdxQueryData.TdxTypedRefAccessor.GetTypeId: TValue;
begin
  Result := FType.Value;
end;

{ TdxQueryData.TdxSimpleValueAccessor }

constructor TdxQueryData.TdxSimpleValueAccessor.Create(AData: TdxQueryData; AIndex: Integer; AMember: TdxMappingMemberInfo);
begin
  inherited Create(AMember);
  FData := AData;
  FIndex := AIndex;
end;

function TdxQueryData.TdxSimpleValueAccessor.GetValue: TValue;
begin
  NotImplemented
end;


{ TdxQueryData.TdxNullValueAccessor }

function TdxQueryData.TdxNullValueAccessor.GetValue: TValue;
begin
  Result := TValue.Empty;
end;

{ TdxQueryData.TdxSubValueAccessor }

constructor TdxQueryData.TdxSubValueAccessor.Create(ANested: TList<TdxValueAccessor>; AMember: TdxMappingMemberInfo);
begin
  inherited Create(AMember);
  FNested := ANested;
end;

function TdxQueryData.TdxSubValueAccessor.GetValue: TValue;
begin
  raise ENotImplemented.Create('');
end;

procedure TdxQueryData.TdxSubValueAccessor.SetRowValue(const ARow: TArray<TValue>; var AIndex: Integer);
begin
  NotImplemented;
end;

{ TdxQueryData }

constructor TdxQueryData.Create;
begin
  inherited Create;
  FNestedDataDictionary := TObjectDictionary<TdxMappingMemberInfo, TdxQueryData>.Create([doOwnsValues]);
end;

constructor TdxQueryData.Create(AEntityInfo: TdxEntityInfo; const AProperties: TArray<IdxCriteriaOperator>;
  ADataLoader: TdxDataLoader);
var
  AProp: IdxCriteriaOperator;
  AMember: TdxOperandProperty;
  AAccessor: TdxSimpleValueAccessor;
  APath: TdxMemberInfoCollection;
begin
  Create;
  SetDataLoader(ADataLoader);
  CreateInternalEntityInfo(AProperties);
  FDataLoader.EntityInfo := FInternalEntityInfo;

  FLinearMembers := TObjectList<TdxValueAccessor>.Create;
  for AProp in AProperties do
  begin
    AMember := Safe<TdxOperandProperty>.Cast(AProp as TdxCriteriaOperator);
    if AMember = nil then
    begin
      AAccessor := TdxSimpleValueAccessor.Create(Self, FItemCount, nil);
      Inc(FItemCount);
      FLinearMembers.Add(AAccessor);
    end
    else
    begin
      APath := AEntityInfo.ParsePersistentPath(AMember.PropertyName);
      FLinearMembers.Add(TdxValueAccessor.CreateAccessor(Self, APath[APath.Count - 1], FItemCount));
    end;
  end;
end;

constructor TdxQueryData.Create(AEntityInfo: TdxEntityInfo);
var
  I: Integer;
  AParentPath: TdxMemberInfoCollection;
begin
  Create;
  FEntityInfo := AEntityInfo;
  I := 0;
  AParentPath := TdxMemberInfoCollection.Create(AEntityInfo);
  try
    Init(Self, AEntityInfo, AParentPath, I);
  finally
    AParentPath.Free;
  end;
end;

destructor TdxQueryData.Destroy;
begin
  FreeAndNil(FProperties);
  FreeAndNil(FNestedDataDictionary);
  FreeAndNil(FInternalEntityInfo);
  FreeAndNil(FDataLoader);

    FreeAndNil(FLinearMembers); // ?
  inherited Destroy;
end;

procedure TdxQueryData.CreateInternalEntityInfo(const AProperties: TArray<IdxCriteriaOperator>);
begin
  FInternalEntityInfo := TdxSelectStatementResultEntityInfo.Create(Self, AProperties);
end;

procedure TdxQueryData.SetDataLoader(ADataLoader: TdxDataLoader);
begin
  FDataLoader := ADataLoader;
  FPos := 0;
end;

function TdxQueryData.GetCount: Integer;
begin
  if FDataLoader = nil then
    Result := 0
  else
    Result := FDataLoader.RecordCount;
end;

function TdxQueryData.GetEntityInfo: TdxEntityInfo;
begin
  if FInternalEntityInfo <> nil then
    Result := FInternalEntityInfo
  else
    Result := FEntityInfo;
end;

function TdxQueryData.GetEof: Boolean;
begin
  Result := FDataLoader.Eof;
end;

function TdxQueryData.GetFieldCount: Integer;
begin
//  if FQueryData.Count <> 0 then
//    ACount := Length(FSelectStatementResult.Rows[0].Values)
//  else
//    ACount := Length(FSelectStatementResult.FieldTypes);
  Result := DataLoader.FieldCount;
end;

function TdxQueryData.GetNestedData(AMemberInfo: TdxMappingMemberInfo): TdxQueryData;
begin
  FNestedDataDictionary.TryGetValue(AMemberInfo, Result);
end;

function TdxQueryData.GetRowValues: TArray<Variant>;
//var
//  I, AIndex: Integer;
//  AValues: TArray<Variant>;
begin
//  SetLength(Result, FItemCount);
//  AIndex := 0;

  DataLoader.ReadCurrentRecord(Result);
//  for I := 0 to Length(AValues) - 1 do
//    Result[I] := TValue.FromVariant(AValues[I]);

//  for I := 0 to FLinearMembers.Count - 1 do
//    FLinearMembers{$IFDEF DLPHIXE3}.List{$ENDIF}[I].SetRowValue(Result, AIndex);
end;


procedure TdxQueryData.AddMember(AMemberInfo: TdxMappingMemberInfo; ARoot: TdxQueryData;
  AParentPath: TdxMemberInfoCollection; var AIndex: Integer);
var
  ACollection: TdxMemberInfoCollection;
begin
  ACollection := TdxMemberInfoCollection.Create(ARoot.EntityInfo{, AParentPath.Count + 1});
  ACollection.AddRange(AParentPath);
  ACollection.Add(AMemberInfo);
  FProperties.Add(ACollection);
end;

procedure TdxQueryData.Init(ARoot: TdxQueryData; AEntityInfo: TdxEntityInfo; AParentPath: TdxMemberInfoCollection;
  var AIndex: Integer);
var
  M: TdxMappingMemberInfo;
  AExplicitLoading: Boolean;
  ADepth: Integer;
  ARefType: TdxEntityInfo;
  ANestedData: TdxQueryData;
  ACollection: TdxMemberInfoCollection;
begin
  FProperties := TdxMemberPathCollection.Create;

    for M in AEntityInfo.PersistentProperties do
    begin
      if ((not M.IsDelayed) or (M.ReferenceType <> nil)) then // and not M.IsReadOnly then
      begin
        AExplicitLoading := False;
        ADepth := 1;
        ARefType := M.ReferenceType;
        if ((((AParentPath.Count <= ADepth) and (ARefType <> nil)) and not M.IsKey) and not M.IsDelayed) and
          (M.IsAggregated or (AExplicitLoading and (ADepth > 0))) then
        begin
          ACollection := TdxMemberInfoCollection.Create(ARoot.EntityInfo);
          try
            ACollection.AddRange(AParentPath);
            ACollection.Add(M);
            ANestedData := TdxQueryData.Create;
            ANestedData.Init(ARoot, ARefType, ACollection, AIndex);
            ANestedData.Properties.OwnsObjects := False;
            FNestedDataDictionary.Add(M, ANestedData);
            FProperties.AddRange(ANestedData.Properties);
          finally
            ACollection.Free;
          end;
        end
        else
        begin
          AddMember(M, ARoot, AParentPath, AIndex);
        end;
      end;
    end;
end;

function TdxQueryData.MoveNext: Boolean;
begin
  if (FDataLoader <> nil) and FDataLoader.Eof then
    Result := False
  else
  begin
    FDataLoader.Next;
    Result := True;
    Inc(FPos);
  end;
end;


{ TdxQueryDataPool }

constructor TdxQueryDataPool.Create;
begin
  inherited Create([doOwnsValues]);
end;

function TdxQueryDataPool.GetQueryData(AEntityInfo: TdxEntityInfo): TdxQueryData;
begin
  if not TryGetValue(AEntityInfo, Result) then
  begin
    Result := TdxQueryData.Create(AEntityInfo);
    Add(AEntityInfo, Result);
  end;
end;

{ TdxSelectStatementResult }

constructor TdxSelectStatementResult.Create;
begin
  FRows := TObjectList<TdxSelectStatementResultRow>.Create;
end;

constructor TdxSelectStatementResult.Create(const ARows: TArray<TdxSelectStatementResultRow>);
begin
  Create;
  FRows.AddRange(ARows);
end;

constructor TdxSelectStatementResult.Create(AQueryData: TdxQueryData);
begin
  Create;
  FQueryData := AQueryData;
//  SetFieldTypes(AQueryData.DataLoader);
  Capacity := AQueryData.Count;
  Load;
end;

destructor TdxSelectStatementResult.Destroy;
begin
  FreeAndNil(FRows);
  FreeAndNil(FQueryData);
  inherited Destroy;
end;

function TdxSelectStatementResult.GetCapacity: Integer;
begin
  Result := FRows.Capacity;
end;

function TdxSelectStatementResult.GetCount: Integer;
begin
  Result := FRows.Count;
end;

function TdxSelectStatementResult.GetEntityInfo: TdxEntityInfo;
begin
  Result := FQueryData.EntityInfo;
end;

function TdxSelectStatementResult.GetRows(AIndex: Integer): TdxSelectStatementResultRow;
begin
  Result := FRows[AIndex];
end;

procedure TdxSelectStatementResult.Load;
begin
  while not FQueryData.Eof do
  begin
    Add(FQueryData.GetRowValues);
    FQueryData.MoveNext;
  end;
end;

procedure TdxSelectStatementResult.SetCapacity(const Value: Integer);
begin
  FRows.Capacity := Value;
end;

//procedure TdxSelectStatementResult.SetFieldTypes(ADataSet: TDataSet);
//var
//  AFieldTypes: TArray<TFieldType>;
//  I: Integer;
//begin
//  SetLength(AFieldTypes, ADataSet.Fields.Count);
//  for I := 0 to ADataSet.Fields.Count - 1 do
//    AFieldTypes[I] :=  ADataSet.Fields[I].DataType;
//  FieldTypes := AFieldTypes;
//end;

procedure TdxSelectStatementResult.Add(const ARow: TArray<Variant>);
begin
  FRows.Add(TdxSelectStatementResultRow.Create(ARow));
end;


{ TdxSelectStatementCollections }

class function TdxSelectStatementCollections.Create(ASelectStatementResult: TdxSelectStatementResult): IdxEMFCollection;
begin
  Result := TdxEMFSelectStatementResultCollection.Create(ASelectStatementResult);
end;

{ TdxEMFSelectStatementResultCollection }

constructor TdxEMFSelectStatementResultCollection.Create(ASelectStatementResult: TdxSelectStatementResult);
begin
  inherited Create;
  FSelectStatementResult := ASelectStatementResult;
  LoadingEnabled := False;
  SetCollectionProperties(TCollectionProperty.IsEntity, False);
end;

destructor TdxEMFSelectStatementResultCollection.Destroy;
begin
  FreeAndNil(FSelectStatementResult);
  inherited Destroy;
end;

function TdxEMFSelectStatementResultCollection.Add(AObject: TObject): Integer;
begin
  raise ENotImplemented.Create('');
end;

procedure TdxEMFSelectStatementResultCollection.CheckDataType;
begin
end;

procedure TdxEMFSelectStatementResultCollection.Clear;
begin
  FSelectStatementResult.Rows.Clear;
end;

function TdxEMFSelectStatementResultCollection.Contains(AValue: TObject): Boolean;
begin
  Result := FSelectStatementResult.Rows.Contains(AValue as TdxSelectStatementResultRow);
end;

procedure TdxEMFSelectStatementResultCollection.FetchAll;
begin
end;

function TdxEMFSelectStatementResultCollection.First: TObject;
begin
  if FSelectStatementResult.Count > 0 then
    Result := FSelectStatementResult[0]
  else
    Result := nil;
end;

function TdxEMFSelectStatementResultCollection.GetCollectionElementClass: TClass;
begin
  Result := TdxSelectStatementResultRow;
end;

function TdxEMFSelectStatementResultCollection.GetCount: Integer;
begin
  Result := FSelectStatementResult.Rows.Count;
end;

function TdxEMFSelectStatementResultCollection.GetDeleteObjectOnRemove: Boolean;
begin
  Result := False;
end;

function TdxEMFSelectStatementResultCollection.GetItems(AIndex: Integer): TObject;
begin
  Result := FSelectStatementResult.Rows[AIndex];
end;

function TdxEMFSelectStatementResultCollection.IndexOf(AValue: TObject): Integer;
begin
  Result := FSelectStatementResult.Rows.IndexOf(AValue as TdxSelectStatementResultRow);
end;

procedure TdxEMFSelectStatementResultCollection.Insert(AIndex: Integer; AObject: TObject);
begin
  raise ENotSupportedException.Create('sNotSupportedException');
end;

function TdxEMFSelectStatementResultCollection.InternalGetEnumerator: IEnumerator;
begin
  Result := TEnumerator.Create(FSelectStatementResult);
end;

function TdxEMFSelectStatementResultCollection.Last: TObject;
begin
  if FSelectStatementResult.Rows.Count > 0 then
    Result := FSelectStatementResult.Rows.Last
  else
    Result := nil;
end;

procedure TdxEMFSelectStatementResultCollection.Load;
begin
end;

function TdxEMFSelectStatementResultCollection.LoadNextObject: Boolean;
begin
  Result := False;
end;

procedure TdxEMFSelectStatementResultCollection.Remove(AObject: TObject);
begin
  FSelectStatementResult.Rows.Remove(AObject as TdxSelectStatementResultRow);
end;

procedure TdxEMFSelectStatementResultCollection.SetDeleteObjectOnRemove(Value: Boolean);
begin
end;

{ TdxEMFSelectStatementResultCollection.TEnumerator }

constructor TdxEMFSelectStatementResultCollection.TEnumerator.Create(ASelectStatementResult: TdxSelectStatementResult);
begin
  FSelectStatementResult := ASelectStatementResult;
  FIndex := -1;
end;

function TdxEMFSelectStatementResultCollection.TEnumerator.GetCurrent: TObject;
begin
  if FIndex >= 0 then
    Result := FSelectStatementResult.Rows{$IFDEF DELPHIXE3}.List{$ENDIF}[FIndex]
  else
    Result := nil;
end;

function TdxEMFSelectStatementResultCollection.TEnumerator.MoveNext: Boolean;
begin
  Result := FIndex < FSelectStatementResult.Rows.Count - 1;
  if Result then
    Inc(FIndex);
end;

procedure TdxEMFSelectStatementResultCollection.TEnumerator.Reset;
begin
  FIndex := -1;
end;

end.


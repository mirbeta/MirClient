{********************************************************************}
{                                                                    }
{           Developer Express Visual Component Library               }
{           ExpressPivotGrid                                         }
{                                                                    }
{           Copyright (c) 2005-2019 Developer Express Inc.           }
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
{   LICENSED TO DISTRIBUTE THE EXPRESSPIVOTGRID AND ALL ACCOMPANYING }
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

unit cxPivotGridOLAPDataSource;

{$I cxVer.inc}

interface

uses
  Types, Classes, SysUtils, Windows, Forms, Contnrs, Math, DB, ActiveX, dxCore, cxClasses,
  cxCustomPivotGrid, cxPivotGridOLAPQueryBuilder, cxCustomData, cxPivotGridStrs, Generics.Defaults, Generics.Collections;

type

  TcxPivotGridOLAPDataSource = class;

{$REGION 'internal types'}
  TcxPivotGridOLAPSchemaInfo = (osiLevels, osiMeasures, osiKPIs, osiSets, osiHierarchies);

  { TcxCustomPivotGridOLAPDataSet }

  TcxCustomPivotGridOLAPDataSet = class
  protected
    function IsValid: Boolean; virtual; abstract;
  end;

  { TcxPivotGridOLAPDBDataSetField }

  TcxPivotGridOLAPDBDataSetField = class
  private
    FInnerField: TField;
  strict protected
    function GetAsString: string;
    function GetAsInteger: Integer;
    function GetIndex: Integer;
    function GetAsVariant: Variant;
    function GetDisplayName: string;
    function GetFullName: string;
    function GetFieldName: string;
  protected
    property InnerField: TField read FInnerField;
  public
    constructor Create(AField: TField);

    property DisplayName: string read GetDisplayName;
    property FieldName: string read GetFieldName;
    property FullName: string read GetFullName;
    property AsString: string read GetAsString;
    property AsInteger: Integer read GetAsInteger;
    property Value: Variant read GetAsVariant;
    property Index: Integer read GetIndex;
  end;

  { TcxPivotGridOLAPDBDataSet }

  TcxPivotGridOLAPDBDataSet = class(TcxCustomPivotGridOLAPDataSet)
  strict private
    FInnerDataSet: TDataSet;
    FOLAPFields: TList;
    function GetItem(Index: Integer): TcxPivotGridOLAPDBDataSetField;
  strict protected
    function GetRecNo: Integer;
    procedure SetRecNo(Value: Integer);

    property OLAPFields: TList read FOLAPFields;
  protected
    function CreateDataSet: TDataSet; virtual; abstract;
    function FieldCount: Integer;
    function FieldByName(const AFieldName: string): TcxPivotGridOLAPDBDataSetField;
    function FindField(const AFieldName: string): TcxPivotGridOLAPDBDataSetField;
    procedure InitializeFields;
    function IsValid: Boolean; override;
    function RecordCount: Integer;
    function Eof: Boolean;
    procedure First;
    procedure Next;

    property Fields[Index: Integer]: TcxPivotGridOLAPDBDataSetField read GetItem;
    property InnerDataSet: TDataSet read FInnerDataSet;
    property RecNo: Integer read GetRecNo write SetRecNo;
  public
    constructor Create;
    destructor Destroy; override;
  end;

  { TcxPivotGridOLAPGroupInfos }

  TcxPivotGridOLAPGroupInfos = class
  private type
  {$REGION 'private types'}
    TInfo = class
    private
      FUniqueName: string;
      FDisplayText: string;
      FValueIsNull: Boolean;
    public
      property DisplayText: string read FDisplayText;
      property UniqueName: string  read FUniqueName;
      property ValueIsNull: Boolean read FValueIsNull;
    end;
  {$ENDREGION}
  private
    FList: TObjectList;
    function GetCount: Integer;
    function GetItem(Index: Integer): TInfo;
  public
    constructor Create;
    destructor Destroy; override;
    function AddItem(const AUniqueName, ADisplayText: string; AValueIsNull: Boolean): Integer;

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TInfo read GetItem; default;
  end;

  { TcxCustomPivotGridOLAPDataSetReader }

  TcxCustomPivotGridOLAPDataSetReader = class
  private
    FDataSet: TcxCustomPivotGridOLAPDataSet;
  protected
    class procedure SetCrossCellFormattedValue(ACell: TcxPivotGridCrossCell; ASummaryCellIndex: Integer;
      const AFormattedValue: Variant);
    class procedure SetCrossCellValues(ACell: TcxPivotGridCrossCell;
      const AFormattedValues, ANativeValues: Variant);
    class procedure SetCrossCellValue(Cell: TcxPivotGridCrossCell; ASummaryCellIndex: Integer;
      const AFormattedValue, ANativeValue: Variant);

    class procedure FieldNotFoundException;
    property DataSet: TcxCustomPivotGridOLAPDataSet read FDataSet;
  public
    constructor Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
    procedure Execute; virtual; abstract;
  end;
{$ENDREGION}

  { TcxCustomPivotGridOLAPDrillDownDataSource }

  TcxCustomPivotGridOLAPDrillDownDataSource = class(TcxPivotGridCrossCellDataSource)
  strict private
    FDataSet: TObject;
    function IsUniqueNamesEqual(APivotFieldName, ADrillDownName: string;
      var ACoincidenceCount: Integer): Boolean;
    procedure SetDataSet(const Value: TObject);
  protected
    function TryGetPivotGridFieldByFieldName(const ADataSetFieldName: string;
      out APivotGridField: TcxPivotGridField): Boolean;

    property DataSet: TObject read FDataSet write SetDataSet;
  public
    destructor Destroy; override;
  end;

  TcxCustomPivotGridOLAPDrillDownDataSourceClass = class of TcxCustomPivotGridOLAPDrillDownDataSource;

  { TcxPivotGridOLAPDrillDownDataSource }

  TcxPivotGridOLAPDrillDownDataSource = class(TcxCustomPivotGridOLAPDrillDownDataSource)
  strict private
    function GetDataSet: TcxPivotGridOLAPDBDataSet;
    procedure SetDataSet(const Value: TcxPivotGridOLAPDBDataSet);
  protected
    FDataSetFields: TList<TField>;
    FPivotGridFields: TList<TcxPivotGridField>;
    function GetFieldCount: Integer; override;
    function GetPivotGridField(AIndex: Integer): TcxPivotGridField; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle;
      AItemHandle: TcxDataItemHandle): Variant; override;

    property DataSet: TcxPivotGridOLAPDBDataSet read GetDataSet write SetDataSet;
  public
    destructor Destroy; override;
  end;

  { TcxPivotGridOLAPArrayDrillDownDataSource }

  TcxPivotGridOLAPArrayDrillDownDataSource = class(TcxCustomPivotGridOLAPDrillDownDataSource)
  protected type
    TDataSet = class
    private
      FFields: TArray<string>;
      FRows: Variant;
      function GetFieldCount: Integer;
      function GetRow(Index: Integer): Variant;
      function GetRowCount: Integer;
      function GetFieldName(AIndex: Integer): string;
      procedure SetRow(Index: Integer; const Value: Variant);
    public
      constructor Create(ARowCount: Integer);
      procedure AddField(const AFieldName: string);
      function CreateRow: Variant;

      property FieldName[Index: Integer]: string read GetFieldName;
      property Row[Index: Integer]: Variant read GetRow write SetRow;
      property RowCount: Integer read GetRowCount;
      property FieldCount: Integer read GetFieldCount;
    end;
  strict private
    function GetDataSet: TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet;
    procedure SetDataSet(const Value: TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet);
  protected
    FDataSetFieldIndexes: TList<Integer>;
    FPivotGridFields: TList<TcxPivotGridField>;
    function GetFieldCount: Integer; override;
    function GetPivotGridField(AIndex: Integer): TcxPivotGridField; override;
    function GetRecordCount: Integer; override;
    function GetValue(ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant; override;

    property DataSet: TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet read GetDataSet write SetDataSet;
  public
    destructor Destroy; override;
  end;

  { TcxCustomPivotGridOLAPProvider }

  TcxCustomPivotGridOLAPProvider = class(TPersistent)
  strict private
    FCacheLocalCubes: Boolean;
    FCachedFileName: string;
    FConnectionString: string;
    FDataSource: TcxPivotGridOLAPDataSource;

    procedure SetConnected(const Value: Boolean);
    procedure SetCacheLocalCubes(const Value: Boolean);
    procedure SetConnectionString(const Value: string);
  protected
    procedure DoConnect; virtual; abstract;
    procedure DoDisconnect; virtual; abstract;
    function IsOLAPVersion8: Boolean; virtual;

    function Execute(const ACommandText: string): TcxCustomPivotGridOLAPDataSet; overload; virtual; abstract;
    function Execute(const AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet; overload; virtual; abstract;
    function GetConnected: Boolean; virtual; abstract;
    function GetDrillDownDataSourceClass: TcxCustomPivotGridOLAPDrillDownDataSourceClass; virtual; abstract;
    procedure PopulateCatalogs(ANames: TStrings); virtual; abstract;
    procedure PopulateHierarchyAllMemberNames(const ACubeName: string; ANames: TDictionary<string, string>); virtual; abstract;
    function OpenSchema(const ACubeName: string; ASchema: TcxPivotGridOLAPSchemaInfo): TcxPivotGridOLAPDBDataSet; virtual; abstract;

    function CreateTemporaryCube(const AOriginalCubeFilePath: string): Boolean;
    procedure DeleteCachedFile;
    function ExtractCubeName(const AConnectionString: string): string;

    procedure Open;
    procedure Close;

    function CreateDrillDownDataSet(const ACommandText: string; ACrossCell: TcxPivotGridCrossCell;
      ASummaryIndex: Integer): TObject; virtual; abstract;
    function CreateQueryBuilder: TcxCustomOLAPDataQueryBuilder; virtual; abstract;
    function CreateCrossCellsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; virtual; abstract;
    function CreateGrandTotalsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroups, ARowGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields; AnExpandedMembers: TList): TcxCustomPivotGridOLAPDataSetReader; virtual; abstract;
    function CreateMainGrandTotalReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
      ADataFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; virtual; abstract;
    function CreateMembersReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AGroupValues: TcxPivotGridOLAPGroupInfos;
      AField: TcxPivotGridField): TcxCustomPivotGridOLAPDataSetReader; virtual; abstract;

    property Connected: Boolean read GetConnected write SetConnected;
    property CacheLocalCubes: Boolean read FCacheLocalCubes write SetCacheLocalCubes default True;
    property CachedFileName: string read FCachedFileName;
    property ConnectionString: string read FConnectionString write SetConnectionString;
    property DataSource: TcxPivotGridOLAPDataSource read FDataSource;
  public
    constructor Create(ADataSource: TcxPivotGridOLAPDataSource); virtual;
    class procedure Register;
    class procedure Unregister;

    class function GetDisplayName: string; virtual;

    procedure Assign(Source: TPersistent); override;
    procedure PopulateCubeNames(ANames: TStrings); virtual; abstract;
  end;

  TcxCustomPivotGridOLAPProviderClass = class of TcxCustomPivotGridOLAPProvider;

  TcxPivotGridAddOLAPStructureItemProc = function(AParent: Pointer; const ACaption: string;
    const ANameIndex, AImageIndex: Integer): Pointer of object;

  { TcxPivotGridOLAPDataSource }

  TcxPivotGridOLAPDataSource = class(TcxPivotGridCustomOLAPDataSource)
  strict private
    FCube: string;
    FIsCubeNameAssigned: Boolean;
    FProvider: TcxCustomPivotGridOLAPProvider;
    FProviderClass: TcxCustomPivotGridOLAPProviderClass;
    FQueryBuilder: TcxCustomOLAPDataQueryBuilder;
    FTerminated: Boolean;

    procedure RecreateQueryBuilder;
    procedure ExpandedItemWithParentsToList(AnGroup: TcxPivotGridGroupItem; AList: TList);
    function GetActive: Boolean;
    function GetDrillDownCommandText(ACell: TcxPivotGridCrossCell; ASummaryIndex: Integer): string;
    function IsDataValid(ADataSet: TcxCustomPivotGridOLAPDataSet): Boolean; overload;
    procedure SetActive(AValue: Boolean);
    procedure SetCube(const AValue: string);
    procedure SetProviderClass(const Value: TcxCustomPivotGridOLAPProviderClass);
    procedure SetProvider(const Value: TcxCustomPivotGridOLAPProvider);
    function GetProviderClassName: string;
    procedure SetProviderClassName(const Value: string);

    function GetConnectionString: string;
    procedure SetConnectionString(const AValue: string);
    procedure ReadCacheLocalCubes(AReader: TReader);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    procedure CreateFieldsFromStructure; override;

    procedure AddDimensions;
    procedure AddLevels(ALevels: TStringList);
    procedure AddMeasures;
    procedure AddKPIs;
    procedure AddSETs;
    procedure PopulateAllMemberUniqueNames;
    function CreateNode(AParent: TcxPivotGridOLAPStructureNode;
      const ACaption: string; ANodeType: TcxOLAPStructureNodeType): TcxPivotGridOLAPStructureNode;
    procedure CreateRootLayout(APivotGrid: TcxCustomPivotGrid; ARowFields, AColumnFields,
      ASummaryFields, AFilterFields: TcxPivotGridFields); override;
    procedure CreateStructure; override;
    procedure DoPopulateGroupValues(AGroupValues: TcxPivotGridOLAPGroupInfos; AField: TcxPivotGridOLAPField;
      AnExpandedMembers: TList; ADataFields, AFilterFields: TcxPivotGridFields);
    function GetLastNameFromPath(const APath: string): string;
    function GetPathItem(var APath: string): string;
    procedure PopulateChildrenGroups(AParentGroup: TcxPivotGridGroupItem; AField: TcxPivotGridField;
      AnExpandedMembers: TList; AHasChildren: Boolean; ADataFields, AFilterFields: TcxPivotGridFields);
    procedure PopulateGrandTotals(AColumnGroups, ARowGroups: TcxPivotGridGroupItem; AColumnFields, ASummaryFields,
      AFilterFields: TcxPivotGridFields; AnExpandedMembers: TList);
    procedure PopulateGroupValues(AField: TcxPivotGridOLAPField; AList: TcxPivotGridVariantList); override;
    procedure PopulateCrossCells(AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem; ARowFields, AColumnFields,
      ASummaryFields, AFilterFields: TcxPivotGridFields);

    function CreateDrillDownDataSet(ACells: TList; ASummaryIndex: Integer): TObject;
    function CreateDrillDownDataSource(ACells: TList; AFieldList: TcxObjectList;
      ASummaryIndex: Integer): TcxPivotGridCrossCellDataSource; override;
    function ExecuteQuery(const AQuery: string): TcxCustomPivotGridOLAPDataSet; overload;
    function ExecuteQuery(AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet; overload;

    function GetIsActive: Boolean; override;
    function GetIsTerminated: Boolean; override;
    function GetNonExpandedFields(ADataBuilder: TcxPivotGridDataBuilder;
      AnExpandingGroup, AnGroup: TcxPivotGridGroupItem; ACrossGroupItems: TList; AddHierarchyField: Boolean): TcxPivotGridFields;
    procedure Initialize; override;
    procedure InitializeExpanding(ADataBuilder: TcxPivotGridDataBuilder; AField: TcxPivotGridField;
      AnExpandingGroup, ACrossGroup: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields); override;
    procedure PopulateMainGrandTotal(ADataFields, AFilterFields: TcxPivotGridFields; AColumns, ARows: TcxPivotGridGroupItem);
    function IsValueVisible(AFilter: TcxPivotGridFieldFilter; const AValue: WideString): Boolean;
    procedure Loaded; override;
    procedure PopulateFilteredUniqueNames(AField: TcxPivotGridOLAPField;
      AFilter: TcxPivotGridFieldFilter; var AUniqueValues: TStringList); override;
    procedure PopulateFilteredValues(AField: TcxPivotGridOLAPField; AFilter: TcxPivotGridFieldFilter;
      AValues: TStrings; AUniqueValues: TStringList); override;
    procedure TerminateConnection;

    property IsCubeNameAssigned: Boolean read FIsCubeNameAssigned write FIsCubeNameAssigned;
    property QueryBuilder: TcxCustomOLAPDataQueryBuilder read FQueryBuilder write FQueryBuilder;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    property ProviderClass: TcxCustomPivotGridOLAPProviderClass read FProviderClass write SetProviderClass;
  published
    property ProviderClassName: string read GetProviderClassName write SetProviderClassName;
    property Provider: TcxCustomPivotGridOLAPProvider read FProvider write SetProvider;
    property ConnectionString: string read GetConnectionString write SetConnectionString stored False; // obsolete
    property Cube: string read FCube write SetCube stored FIsCubeNameAssigned;
    property Active: Boolean read GetActive write SetActive default False;
    property OnInitializeField;
  end;

function cxGetPivotGridOLAPDataSourceProviders: TcxRegisteredClasses;

implementation

uses
  Variants, StrUtils, ComObj, DBConsts, dxCustomTree,
  cxPivotGridOLAPOLEDBProvider, cxPivotGridOLAPADOMDProvider;

const
  OLAPMemberNameColumnName = '[Measures].[MemberName]';
  OLAPMemberUniqueNameColumnName = '[Measures].[MemberUniqueName]';
  OLAPParentUniqueNameColumnName = '[Measures].[ParentUniqueName]';

type
  TcxPivotGridVariantListAccess = class(TcxPivotGridVariantList);
  TcxCustomPivotGridAccess = class(TcxCustomPivotGrid);
  TcxPivotGridOLAPCrossCellAccess = class(TcxPivotGridOLAPCrossCell);
  TcxPivotGridCrossCellSummaryAccess = class(TcxPivotGridCrossCellSummary);
  TcxCustomOLAPDataQueryBuilderAccess = class(TcxCustomOLAPDataQueryBuilder);

var
  FProviders: TcxRegisteredClasses;

function cxGetPivotGridOLAPDataSourceProviders: TcxRegisteredClasses;
begin
  if FProviders = nil then
    FProviders := TcxRegisteredClasses.Create;
  Result := FProviders;
end;

procedure cxPivotGridOLAPDataSourceRegisterProvider(AClass: TcxCustomPivotGridOLAPProviderClass; const AName: string);
begin
  cxGetPivotGridOLAPDataSourceProviders.Register(AClass, AName);
end;

procedure cxPivotGridOLAPDataSourceUnregisterProvider(AClass: TcxCustomPivotGridOLAPProviderClass);
begin
  cxGetPivotGridOLAPDataSourceProviders.Unregister(AClass);
  if cxGetPivotGridOLAPDataSourceProviders.Count = 0 then
    FreeAndNil(FProviders);
end;

{ TcxPivotGridOLAPDBDataSetField }

constructor TcxPivotGridOLAPDBDataSetField.Create(AField: TField);
begin
  inherited Create;
  FInnerField := AField;
end;

function TcxPivotGridOLAPDBDataSetField.GetAsInteger: Integer;
begin
  Result := FInnerField.AsInteger;
end;

function TcxPivotGridOLAPDBDataSetField.GetAsString: string;
begin
  Result := FInnerField.AsString;
end;

function TcxPivotGridOLAPDBDataSetField.GetAsVariant: Variant;
begin
  Result := FInnerField.Value;
end;

function TcxPivotGridOLAPDBDataSetField.GetDisplayName: string;
begin
  Result := FInnerField.DisplayName;
end;

function TcxPivotGridOLAPDBDataSetField.GetFieldName: string;
begin
  Result := FInnerField.FieldName;
end;

function TcxPivotGridOLAPDBDataSetField.GetFullName: string;
begin
  Result := FInnerField.FullName;
end;

function TcxPivotGridOLAPDBDataSetField.GetIndex: Integer;
begin
  Result := FInnerField.Index;
end;

{ TcxPivotGridOLAPDBDataSet }

constructor TcxPivotGridOLAPDBDataSet.Create;
begin
  inherited Create;
  FInnerDataSet := CreateDataSet;
  FInnerDataSet.DisableControls;
  FOLAPFields := TObjectList.Create;
end;

destructor TcxPivotGridOLAPDBDataSet.Destroy;
begin
  FOLAPFields.Free;
  FInnerDataSet.Free;
  inherited Destroy;
end;

function TcxPivotGridOLAPDBDataSet.FieldCount: Integer;
begin
  Result := FOLAPFields.Count;
end;

function TcxPivotGridOLAPDBDataSet.FieldByName(const AFieldName: string): TcxPivotGridOLAPDBDataSetField;
begin
  Result := FindField(AFieldName);
  if Result = nil then
    DatabaseErrorFmt(SFieldNotFound, [AFieldName]);
end;

function TcxPivotGridOLAPDBDataSet.FindField(const AFieldName: string): TcxPivotGridOLAPDBDataSetField;
var
  I: Integer;
begin
  for I := 0 to FOLAPFields.Count - 1 do
  begin
    Result := FOLAPFields[I];
    if (AnsiCompareText(AFieldName, Result.FieldName) = 0) then
      Exit;
  end;
  Result := nil;
end;

procedure TcxPivotGridOLAPDBDataSet.InitializeFields;
var
  I: Integer;
begin
  Assert(OLAPFields.Count = 0);
  for I := 0 to InnerDataSet.FieldCount - 1 do
    OLAPFields.Add(TcxPivotGridOLAPDBDataSetField.Create(InnerDataSet.Fields[I]));
end;

function TcxPivotGridOLAPDBDataSet.IsValid: Boolean;
begin
  Result := InnerDataSet.Active and (InnerDataSet.RecordCount > 0) and (InnerDataSet.FieldCount > 0);
end;

function TcxPivotGridOLAPDBDataSet.RecordCount: Integer;
begin
  Result := FInnerDataSet.RecordCount;
end;

function TcxPivotGridOLAPDBDataSet.Eof: Boolean;
begin
  Result := FInnerDataSet.Eof;
end;

procedure TcxPivotGridOLAPDBDataSet.First;
begin
  FInnerDataSet.First;
end;

procedure TcxPivotGridOLAPDBDataSet.Next;
begin
  FInnerDataSet.Next;
end;

function TcxPivotGridOLAPDBDataSet.GetRecNo: Integer;
begin
  Result := FInnerDataSet.RecNo;
end;

procedure TcxPivotGridOLAPDBDataSet.SetRecNo(Value: Integer);
begin
  FInnerDataSet.RecNo := Value;
end;

function TcxPivotGridOLAPDBDataSet.GetItem(Index: Integer): TcxPivotGridOLAPDBDataSetField;
begin
  Result := OLAPFields[Index];
end;

{ TcxPivotGridOLAPGroupInfos }

constructor TcxPivotGridOLAPGroupInfos.Create;
begin
  inherited Create;
  FList := TObjectList.Create;
end;

destructor TcxPivotGridOLAPGroupInfos.Destroy;
begin
  FList.Free;
  inherited Destroy;
end;

function TcxPivotGridOLAPGroupInfos.AddItem(const AUniqueName, ADisplayText: string; AValueIsNull: Boolean): Integer;
var
  AItem: TInfo;
begin
  AItem := TInfo.Create;
  AItem.FUniqueName := AUniqueName;
  AItem.FDisplayText := ADisplayText;
  AItem.FValueIsNull := AValueIsNull;
  Result := FList.Add(AItem);
end;

function TcxPivotGridOLAPGroupInfos.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxPivotGridOLAPGroupInfos.GetItem(Index: Integer): TInfo;
begin
  Result := TInfo(FList[Index]);
end;

{ TcxCustomPivotGridOLAPDataSetReader }

constructor TcxCustomPivotGridOLAPDataSetReader.Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
end;

class procedure TcxCustomPivotGridOLAPDataSetReader.SetCrossCellFormattedValue(ACell: TcxPivotGridCrossCell; ASummaryCellIndex: Integer;
  const AFormattedValue: Variant);
var
  I: Integer;
begin
  for I := 0 to Integer(High(TcxPivotGridSummaryType)) do
    TcxPivotGridCrossCellSummaryAccess(ACell.SummaryCells[ASummaryCellIndex]).SetSummaryByIndex(I, AFormattedValue);
end;

class procedure TcxCustomPivotGridOLAPDataSetReader.SetCrossCellValues(ACell: TcxPivotGridCrossCell;
  const AFormattedValues, ANativeValues: Variant);
var
  I: Integer;
begin
  if VarIsNull(ANativeValues) then
    TcxPivotGridOLAPCrossCellAccess(ACell).IsEmpty := True
  else
  begin
    Assert(VarIsArray(ANativeValues));
    for I := 0 to ACell.SummaryCellCount - 1 do
      SetCrossCellFormattedValue(ACell, I, AFormattedValues[I]);
    TcxPivotGridOLAPCrossCell(ACell).NativeValues := ANativeValues;
  end;
end;

class procedure TcxCustomPivotGridOLAPDataSetReader.SetCrossCellValue(Cell: TcxPivotGridCrossCell; ASummaryCellIndex: Integer;
  const AFormattedValue, ANativeValue: Variant);
var
  ACell: TcxPivotGridOLAPCrossCellAccess absolute Cell;
begin
  Assert(Cell is TcxPivotGridOLAPCrossCell);
  if not VarIsArray(ACell.NativeValues) then
    ACell.NativeValues := VarArrayCreate([0, ACell.SummaryCellCount - 1], varVariant);
  ACell.SetNativeValue(ASummaryCellIndex, ANativeValue);
  SetCrossCellFormattedValue(ACell, ASummaryCellIndex, AFormattedValue);
end;

class procedure TcxCustomPivotGridOLAPDataSetReader.FieldNotFoundException;
begin
  raise Exception.Create('Field not found');
end;

{ TcxCustomPivotGridOLAPProvider }

constructor TcxCustomPivotGridOLAPProvider.Create(ADataSource: TcxPivotGridOLAPDataSource);
begin
  inherited Create;
  FDataSource := ADataSource;
end;

class procedure TcxCustomPivotGridOLAPProvider.Register;
begin
  cxPivotGridOLAPDataSourceRegisterProvider(Self, GetDisplayName);
end;

class procedure TcxCustomPivotGridOLAPProvider.Unregister;
begin
  cxPivotGridOLAPDataSourceUnregisterProvider(Self);
end;

class function TcxCustomPivotGridOLAPProvider.GetDisplayName: string;
begin
  Result := '';
end;

procedure TcxCustomPivotGridOLAPProvider.Assign(Source: TPersistent);
var
  ASource: TcxCustomPivotGridOLAPProvider absolute Source;
begin
  if Source is TcxCustomPivotGridOLAPProvider then
  begin
    CacheLocalCubes := ASource.CacheLocalCubes;
    ConnectionString := ASource.ConnectionString;
  end
  else
    inherited Assign(Source);
end;

procedure TcxCustomPivotGridOLAPProvider.Close;
begin
  Connected := False;
end;

procedure TcxCustomPivotGridOLAPProvider.Open;
begin
  Connected := True;
end;

function TcxCustomPivotGridOLAPProvider.IsOLAPVersion8: Boolean;
begin
  Result := False;
end;

function TcxCustomPivotGridOLAPProvider.CreateTemporaryCube(const AOriginalCubeFilePath: string): Boolean;
var
  AStream: TMemoryStream;
begin
  if FileExists(AOriginalCubeFilePath) then
  try
    FCachedFileName := dxCreateTempFile('.cub');
    AStream := TMemoryStream.Create;
    try
      AStream.LoadFromFile(AOriginalCubeFilePath);
      AStream.SaveToFile(FCachedFileName);
    finally
      AStream.Free;
    end;
  except
    on EStreamError do
      DeleteCachedFile
    else
      raise;
  end;
  Result := FCachedFileName <> '';
end;

procedure TcxCustomPivotGridOLAPProvider.DeleteCachedFile;
begin
  if FileExists(FCachedFileName) then
    DeleteFile(PChar(FCachedFileName));
  FCachedFileName := '';
end;

function TcxCustomPivotGridOLAPProvider.ExtractCubeName(const AConnectionString: string): string;
const
  ADataSource = 'Data Source=';
var
  APos, AEndPos: Integer;
begin
  APos := Pos(ADataSource, AConnectionString);
  if APos = 0 then
    Exit;
  Inc(APos, Length(ADataSource));
  AEndPos := APos;
  while (AEndPos < Length(AConnectionString)) and (AConnectionString[AEndPos] <> ';') do
    Inc(AEndPos);
  Result := Copy(AConnectionString, APos, AEndPos - APos + 1);
end;

procedure TcxCustomPivotGridOLAPProvider.SetConnected(const Value: Boolean);
begin
  if Connected <> Value then
    try
      if Value then
        DoConnect
      else
        DoDisconnect;
    finally
      if Connected = False then
        DeleteCachedFile;
    end;
end;

procedure TcxCustomPivotGridOLAPProvider.SetCacheLocalCubes(const Value: Boolean);
begin
  if CacheLocalCubes <> Value then
  begin
    if DataSource <> nil then
      DataSource.Active := False
    else
      Close;
    FCacheLocalCubes := Value;
  end;
end;

procedure TcxCustomPivotGridOLAPProvider.SetConnectionString(const Value: string);
begin
  if ConnectionString <> Value then
  begin
    if DataSource <> nil then
      DataSource.Active := False
    else
      Close;
    FConnectionString := Value;
  end;
end;

{ TcxCustomPivotGridOLAPDrillDownDataSource }

destructor TcxCustomPivotGridOLAPDrillDownDataSource.Destroy;
begin
  DataSet := nil;
  inherited Destroy;
end;

procedure TcxCustomPivotGridOLAPDrillDownDataSource.SetDataSet(const Value: TObject);
begin
  FreeAndNil(FDataSet);
  FDataSet := Value;
end;

function TcxCustomPivotGridOLAPDrillDownDataSource.TryGetPivotGridFieldByFieldName(const ADataSetFieldName: string;
  out APivotGridField: TcxPivotGridField): Boolean;
var
  I, ACoincidenceCount, ACurrentCoincidenceCount: Integer;
begin
  ACoincidenceCount := 0;
  APivotGridField := nil;
  for I := 0 to PivotGrid.FieldCount - 1 do
    if IsUniqueNamesEqual(PivotGrid.Fields[I].UniqueName, ADataSetFieldName, ACurrentCoincidenceCount) and
      (ACoincidenceCount < ACurrentCoincidenceCount) then
    begin
      APivotGridField := PivotGrid.Fields[I];
    end;
  Result := APivotGridField <> nil;
end;

function TcxCustomPivotGridOLAPDrillDownDataSource.IsUniqueNamesEqual(
  APivotFieldName, ADrillDownName: string;
  var ACoincidenceCount: Integer): Boolean;

  procedure TrimBrackets(var AName: string);
  var
    ALength: Integer;
  begin
    if dxCharInSet(AName[1], ['{', '[', '(']) then
      Delete(AName, 1, 1);
    ALength := Length(AName);
    if dxCharInSet(AName[ALength], ['}', ']', ')']) then
      Delete(AName, ALength, 1);
  end;

var
  APivotPart, ADrillDownPart: string;
  APos: Integer;
begin
  ACoincidenceCount := 0;
  while (APivotFieldName <> '') and (ADrillDownName <> '') do
  begin
    APos := Pos('.', APivotFieldName);
    if APos <> 0 then
    begin
      APivotPart := Copy(APivotFieldName, 0, APos - 1);
      Delete(APivotFieldName, 1, APos);
      TrimBrackets(APivotPart);
      APos := Pos('.', ADrillDownName);
      if APos <> 0 then
      begin
        ADrillDownPart := Copy(ADrillDownName, 0, APos - 1);
        Delete(ADrillDownName, 1, APos);
        if (Pos(APivotPart, ADrillDownPart) <> 0) or
          (Pos('Measures', APivotPart) <> 0) then
          Inc(ACoincidenceCount)
        else
          Dec(ACoincidenceCount);
      end
      else
        Dec(ACoincidenceCount);
    end
    else
    begin
      APivotPart := APivotFieldName;
      APivotFieldName := '';
      TrimBrackets(APivotPart);
      APos := Pos('.', ADrillDownName);
      if (APos = 0) and (Pos(APivotPart, ADrillDownName) <> 0) then
        Inc(ACoincidenceCount)
      else
        Dec(ACoincidenceCount);
    end;
  end;
  Result := ACoincidenceCount > 0;
end;

{ TcxPivotGridOLAPDrillDownDataSource }

destructor TcxPivotGridOLAPDrillDownDataSource.Destroy;
begin
  FreeAndNil(FDataSetFields);
  FreeAndNil(FPivotGridFields);
  inherited Destroy;
end;

function TcxPivotGridOLAPDrillDownDataSource.GetDataSet: TcxPivotGridOLAPDBDataSet;
begin
  Result := TcxPivotGridOLAPDBDataSet(inherited DataSet);
end;

procedure TcxPivotGridOLAPDrillDownDataSource.SetDataSet(const Value: TcxPivotGridOLAPDBDataSet);
begin
  inherited DataSet := Value as TcxPivotGridOLAPDBDataSet;
end;

function TcxPivotGridOLAPDrillDownDataSource.GetFieldCount: Integer;
var
  I: Integer;
  APivotGridField: TcxPivotGridField;
begin
  if FDataSetFields = nil then
  begin
    FDataSetFields := TList<TField>.Create;
    FPivotGridFields := TList<TcxPivotGridField>.Create;
  end
  else
  begin
    FDataSetFields.Clear;
    FPivotGridFields.Clear;
  end;

  if HasData and (DataSet <> nil) then
    for I := 0 to DataSet.FieldCount - 1 do
      if TryGetPivotGridFieldByFieldName(DataSet.Fields[I].FullName, APivotGridField) then
      begin
        FDataSetFields.Add(DataSet.Fields[I].InnerField);
        FPivotGridFields.Add(APivotGridField);
      end;
  Result := FPivotGridFields.Count;
end;

function TcxPivotGridOLAPDrillDownDataSource.GetPivotGridField(
  AIndex: Integer): TcxPivotGridField;
begin
  Result := TcxPivotGridField(FPivotGridFields.Items[AIndex]);
end;

function TcxPivotGridOLAPDrillDownDataSource.GetRecordCount: Integer;
begin
  if HasData and (DataSet <> nil) then
    Result := DataSet.RecordCount
  else
    Result := 0;
end;

function TcxPivotGridOLAPDrillDownDataSource.GetValue(
  ARecordHandle: TcxDataRecordHandle; AItemHandle: TcxDataItemHandle): Variant;
begin
  Result := Unassigned;
  if (DataSet <> nil) and (Integer(AItemHandle) < DataSet.FieldCount) then
  begin
    DataSet.RecNo := Integer(ARecordHandle) + 1;
    Result := FDataSetFields[Integer(AItemHandle)].AsVariant;
  end;
end;

{ TcxPivotGridOLAPArrayDrillDownDataSource }

destructor TcxPivotGridOLAPArrayDrillDownDataSource.Destroy;
begin
  FDataSetFieldIndexes.Free;
  FPivotGridFields.Free;
  inherited Destroy;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.GetFieldCount: Integer;
var
  I: Integer;
  APivotGridField: TcxPivotGridField;
begin
  if FDataSetFieldIndexes = nil then
  begin
    FDataSetFieldIndexes := TList<Integer>.Create;
    FPivotGridFields := TList<TcxPivotGridField>.Create;
  end
  else
  begin
    FDataSetFieldIndexes.Clear;
    FPivotGridFields.Clear;
  end;

  if HasData and (DataSet <> nil) then
    for I := 0 to DataSet.FieldCount - 1 do
      if TryGetPivotGridFieldByFieldName(DataSet.FieldName[I], APivotGridField) then
      begin
        FDataSetFieldIndexes.Add(I);
        FPivotGridFields.Add(APivotGridField);
      end;
  Result := FPivotGridFields.Count;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.GetPivotGridField(AIndex: Integer): TcxPivotGridField;
begin
  Result := TcxPivotGridField(FPivotGridFields.Items[AIndex]);
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.GetRecordCount: Integer;
begin
  if HasData and (DataSet <> nil) then
    Result := DataSet.GetRowCount
  else
    Result := 0;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.GetValue(ARecordHandle: TcxDataRecordHandle;
  AItemHandle: TcxDataItemHandle): Variant;
var
  ARecordData: Variant;
  ARecNo, AFieldIndex: Integer;
begin
  Result := Unassigned;
  ARecNo := Integer(ARecordHandle);
  if (DataSet <> nil) and (ARecNo < DataSet.RowCount) then
  begin
    ARecordData := DataSet.Row[ARecNo];
    if Integer(AItemHandle) < FDataSetFieldIndexes.Count then
    begin
      AFieldIndex := FDataSetFieldIndexes[Integer(AItemHandle)];
      Result := ARecordData[AFieldIndex];
    end;
  end;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.GetDataSet: TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet;
begin
  Result := TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet(inherited DataSet);
end;

procedure TcxPivotGridOLAPArrayDrillDownDataSource.SetDataSet(
  const Value: TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet);
begin
  inherited DataSet := Value as TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet;
end;

{ TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet }

constructor TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.Create(ARowCount: Integer);
begin
  inherited Create;
  FRows := VarArrayCreate([0, ARowCount - 1], varVariant)
end;

procedure TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.AddField(const AFieldName: string);
var
  I: Integer;
begin
  I := Length(FFields);
  SetLength(FFields, I + 1);
  FFields[I] := AFieldName;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.CreateRow: Variant;
begin
  Result := VarArrayCreate([0, FieldCount - 1], varVariant);
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.GetFieldCount: Integer;
begin
  Result := Length(FFields);
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.GetRow(Index: Integer): Variant;
begin
  Result := FRows[Index];
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.GetRowCount: Integer;
begin
  if VarIsArray(FRows) then
    Result := VarArrayHighBound(FRows, 1) - VarArrayLowBound(FRows, 1) + 1
  else
    Result := 0;
end;

procedure TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.SetRow(Index: Integer; const Value: Variant);
begin
  FRows[Index] := Value;
end;

function TcxPivotGridOLAPArrayDrillDownDataSource.TDataSet.GetFieldName(AIndex: Integer): string;
begin
  Result := FFields[AIndex];
end;

{ TcxPivotGridOLAPDataSource }

constructor TcxPivotGridOLAPDataSource.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ProviderClass := TcxPivotGridOLAPOLEDBProvider;
end;

destructor TcxPivotGridOLAPDataSource.Destroy;
begin
  Active := False;
  FreeAndNil(FQueryBuilder);
  FProvider.Free;
  inherited Destroy;
end;

procedure TcxPivotGridOLAPDataSource.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('CacheLocalCubes', ReadCacheLocalCubes, nil, False);
end;

procedure TcxPivotGridOLAPDataSource.CreateFieldsFromStructure;
begin
  inherited CreateFieldsFromStructure;
  PopulateAllMemberUniqueNames;
end;

procedure TcxPivotGridOLAPDataSource.AddDimensions;
var
  ADName: string;
  I: Integer;
  ADataSet: TcxPivotGridOLAPDBDataSet;
  ALevels: TStringList;
  AParent, AFolderNode, ANode: TcxPivotGridOLAPStructureNode;
  ACardinality, AStructure, AFolder, ACaption, ADimensionUniqueName, AHierarchyUniqueName: TcxPivotGridOLAPDBDataSetField;
begin
  ADataSet := Provider.OpenSchema(Cube, osiHierarchies);
  try
    if IsDataValid(ADataSet) then
    begin
      AFolder := ADataSet.FindField('HIERARCHY_DISPLAY_FOLDER');
      ACaption := ADataSet.FieldByName('HIERARCHY_CAPTION');
      ADimensionUniqueName := ADataSet.FieldByName('DIMENSION_UNIQUE_NAME');
      AHierarchyUniqueName := ADataSet.FieldByName('HIERARCHY_UNIQUE_NAME');
      AStructure := ADataSet.FieldByName('STRUCTURE');
      ACardinality := ADataSet.FieldByName('HIERARCHY_CARDINALITY');
      ALevels := TStringList.Create;
      try
        for I := 0 to ADataSet.RecordCount - 1 do
        begin
          if ADimensionUniqueName.AsString = '[Measures]' then
          begin
            ADataSet.Next;
            Continue;
          end;
          ADName := ADimensionUniqueName.AsString;
          ADName := Copy(ADName, 2, Length(ADName) - 2);
          // find dimension
          AParent := Structure.ItemByDisplayText(ADName);
          if AParent = nil then
          begin
            AParent := CreateNode(Structure, ADName, ntDimension);
            AParent.DimensionUniqueName := ADName;
          end;
          // find folder
          if (AFolder <> nil) and (AFolder.AsString <> '') then
          begin
            AFolderNode := AParent.ItemByDisplayText(AFolder.AsString);
            if AFolderNode = nil then
              AFolderNode := CreateNode(AParent, AFolder.AsString, ntFolder);
          end
          else
            AFolderNode := AParent;
          // add member
          ANode := CreateNode(AFolderNode, ACaption.AsString, ntField);
          AFolderNode.Expanded := False;
          ANode.HierarchyUniqueName := AHierarchyUniqueName.AsString;
          ANode.DimensionUniqueName := ADimensionUniqueName.AsString;
          ANode.UniqueName := ANode.DimensionUniqueName;
          if not VarIsNull(AStructure.Value) then
            ANode.StructureType := AStructure.AsInteger;
          if not VarIsNull(ACardinality.Value) then
            ANode.Cardinality := ACardinality.AsInteger;
          ANode.NodeType := ntGroup;
          ALevels.AddObject(ANode.HierarchyUniqueName, ANode);
          ANode.LevelUniqueName := ANode.HierarchyUniqueName + '.[' + GetLastNameFromPath(ANode.HierarchyUniqueName) + ']';
          ADataSet.Next;
        end;
        AddLevels(ALevels);
      finally
        ALevels.Free;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.AddLevels(ALevels: TStringList);
var
  I, AIndex: Integer;
  ADataSet: TcxPivotGridOLAPDBDataSet;
  AParent, ANode: TcxPivotGridOLAPStructureNode;
  ADimensionUniqueName, AUniqueName, AHierarchyUniqueName, AName, ACardinality, ADBType, ANumber, AType: TcxPivotGridOLAPDBDataSetField;
begin
  ADataSet := Provider.OpenSchema(Cube, osiLevels);
  try
    if IsDataValid(ADataSet) then
    begin
      AHierarchyUniqueName := ADataSet.FieldByName('HIERARCHY_UNIQUE_NAME');
      ADimensionUniqueName := ADataSet.FieldByName('DIMENSION_UNIQUE_NAME');
      AUniqueName := ADataSet.FieldByName('LEVEL_UNIQUE_NAME');
      AName := ADataSet.FieldByName('LEVEL_NAME');
      ANumber := ADataSet.FieldByName('LEVEL_NUMBER');
      ACardinality := ADataSet.FieldByName('LEVEL_CARDINALITY');
      ADBType := ADataSet.FieldByName('LEVEL_DBTYPE');
      AType := ADataSet.FieldByName('LEVEL_TYPE');
      for I := 0 to ADataSet.RecordCount - 1 do
      begin
        AIndex := ALevels.IndexOf(AHierarchyUniqueName.AsString);
        if (AType.Value <> 1) and (AIndex >= 0) then
        begin
          AParent := TcxPivotGridOLAPStructureNode(ALevels.Objects[AIndex]);
          ANode := CreateNode(AParent, AName.AsString, ntField);
          ANode.HierarchyUniqueName := AHierarchyUniqueName.AsString;
          ANode.DimensionUniqueName := ADimensionUniqueName.AsString;
          if not VarIsNull(ACardinality.Value) then
            ANode.Cardinality := ACardinality.AsInteger;
          if not VarIsNull(ADBType.Value) then
            ANode.DBType := ADBType.AsInteger;
          if not VarIsNull(ANumber.Value) then
            ANode.LevelNumber := ANumber.AsInteger;
          ANode.LevelUniqueName := AUniqueName.AsString;
          ANode.UniqueName := ANode.LevelUniqueName;
        end;
        ADataSet.Next;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.AddMeasures;

  function GetFormatName(const AName: string): TcxPivotGridFormatNameType;
  begin
    Result := fnGeneralNumber;
    if AName = 'Currency' then
      Result := fnCurrency
    else
      if AName = 'Fixed' then
        Result := fnFixed
      else
        if AName = 'Percent' then
          Result := fnPercent
        else
          if AName = 'General Number' then
            Result := fnGeneralNumber
          else
            if AName = 'Standard' then
              Result := fnStandard
            else
              if AName = 'Scientific' then
                Result := fnScientific
              else
                if AName = 'Yes/No' then
                  Result := fnYesNo
                else
                  if AName = 'True/False' then
                    Result := fnTrueFalse
                  else
                    if AName = 'On/Off' then
                      Result := fnOnOff
                    else
                      if AName <> '' then
                        Result := fnCustom;
  end;

  function GetParentNode(AFolder: TcxPivotGridOLAPDBDataSetField;
    AParentNode: TcxPivotGridOLAPStructureNode): TcxPivotGridOLAPStructureNode;
  begin
    Result := AParentNode;
    if (AFolder <> nil) and (AFolder.AsString <> '') then
    begin
      Result := AParentNode.ItemByDisplayText(AFolder.AsString);
      if Result = nil then
        Result := CreateNode(AParentNode, AFolder.AsString, ntFolder);
    end;
  end;

var
  I: Integer;
  ADataSet: TcxPivotGridOLAPDBDataSet;
  AFolder, ACaption, AUniqueName, AMeasureDisplayFolder, ADefaultFormatString: TcxPivotGridOLAPDBDataSetField;
  AMeasures, AParent, ANode: TcxPivotGridOLAPStructureNode;
begin
  ADataSet := Provider.OpenSchema(Cube, osiMeasures);
  try
    if IsDataValid(ADataSet) then
    begin
      AMeasures := CreateNode(Structure, cxGetResourceString(@scxMeasures), ntMeasure);
      ACaption := ADataSet.FieldByName('MEASURE_CAPTION');
      AUniqueName := ADataSet.FieldByName('MEASURE_UNIQUE_NAME');
      AFolder := ADataSet.FindField('MEASUREGROUP_NAME');
      AMeasureDisplayFolder := ADataSet.FindField('MEASURE_DISPLAY_FOLDER');
      ADefaultFormatString := ADataSet.FindField('DEFAULT_FORMAT_STRING');
      for I := 0 to ADataSet.RecordCount - 1 do
      begin
        AParent := GetParentNode(AFolder, AMeasures);
        if AMeasureDisplayFolder <> nil then
          AParent := GetParentNode(AMeasureDisplayFolder, AParent);
        ANode := CreateNode(AParent, ACaption.AsString, ntField);
        ANode.UniqueName := AUniqueName.AsString;
        if ADefaultFormatString <> nil then
        begin
          ANode.FormatName := GetFormatName(ADefaultFormatString.AsString);
          if ANode.FormatName = fnCustom then
            ANode.FormatString := ADefaultFormatString.AsString;
        end;
        ADataSet.Next;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.AddKPIs;
var
  APath: string;
  AKPIType: TcxOLAPKPIType;
  AName, ACaption, AValue: TcxPivotGridOLAPDBDataSetField;
  AKPI, AParent: TcxPivotGridOLAPStructureNode;
  AGraphicFields: array[TcxOLAPKPIType] of TcxPivotGridOLAPDBDataSetField;
  AGraphicTypes: TStringList;
const
  KPI_Names: array[TcxOLAPKPIType] of string =
    ('KPI_VALUE', 'KPI_GOAL', 'KPI_STATUS', 'KPI_TREND', 'KPI_WEIGHT');

  function GetGraphicType(AField: TcxPivotGridOLAPDBDataSetField): TcxPivotGridOLAPKPIGraphicType;
  var
    APos: Integer;
  begin
    Result := gtNone;
    if AField = nil then
      Exit;
    APos := AGraphicTypes.IndexOf(AField.AsString);
    if APos >= 0 then
      Result := TcxPivotGridOLAPKPIGraphicType(AGraphicTypes.Objects[APos])
  end;

var
  I: Integer;
  ADataSet: TcxPivotGridOLAPDBDataSet;
begin
  if Provider.IsOLAPVersion8 then
    Exit;
  ADataSet := Provider.OpenSchema(Cube, osiKPIs);
  try
    if IsDataValid(ADataSet) then
    begin
      AKPI := CreateNode(Structure, cxGetResourceString(@scxKPIs), ntKPI);
      ACaption := ADataSet.FieldByName('KPI_CAPTION');
      AName := ADataSet.FieldByName('KPI_NAME');
      AValue := ADataSet.FieldByName('KPI_VALUE');
      FillChar(AGraphicFields, SizeOf(AGraphicFields), 0);
      AGraphicFields[oktStatus] := ADataSet.FieldByName('KPI_STATUS_GRAPHIC');
      AGraphicFields[oktTrend] := ADataSet.FieldByName('KPI_TREND_GRAPHIC');
      AGraphicTypes := TStringList.Create;
      try
        AGraphicTypes.AddObject('Shapes', TObject(gtShapes));
        AGraphicTypes.AddObject('Traffic Light', TObject(gtTrafficLights));
        AGraphicTypes.AddObject('Traffic Light - Single', TObject(gtTrafficLights));
        AGraphicTypes.AddObject('Traffic Light - Multiple', TObject(gtTrafficLights));
        AGraphicTypes.AddObject('Road Signs', TObject(gtRoadSigns));
        AGraphicTypes.AddObject('Gauge - Ascending', TObject(gtGauge));
        AGraphicTypes.AddObject('Gauge - Descending', TObject(gtReversedGauge));
        AGraphicTypes.AddObject('Thermometer', TObject(gtThermometer));
        AGraphicTypes.AddObject('Cylinder', TObject(gtCylinder));
        AGraphicTypes.AddObject('Smiley Face', TObject(gtFaces));
        AGraphicTypes.AddObject('Variance Arrow', TObject(gtVarianceArrow));
        AGraphicTypes.AddObject('Standard Arrow', TObject(gtStandardArrow));
        AGraphicTypes.AddObject('Status Arrow - Ascending', TObject(gtStatusArrow));
        AGraphicTypes.AddObject('Status Arrow - Descending', TObject(gtReversedStatusArrow));
        for I := 0 to ADataSet.RecordCount - 1 do
        begin
          AParent := CreateNode(AKPI, AName.AsString, ntFolder);
          with CreateNode(AParent, ACaption.AsString, ntField) do
          begin
            UniqueName := AValue.AsString;
            KPIName := AName.AsString;
          end;
          for AKPIType := oktGoal to oktWeight do
          begin
            APath := ADataSet.FieldByName(KPI_NAMES[AKPIType]).AsString;
            if APath <> '' then
            begin
              with CreateNode(AParent, GetLastNameFromPath(APath), ntField) do
              begin
                UniqueName := APath;
                KPIName := AName.AsString;
                KPIType := AKPIType;
                KPIGraphicType := GetGraphicType(AGraphicFields[AKPIType]);
              end;
            end;
          end;
          ADataSet.Next;
        end;
      finally
        AGraphicTypes.Free;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.AddSETs;
var
  I: Integer;
  ADataSet: TcxPivotGridOLAPDBDataSet;
  AItem, APath: string;
  ADimensions, ACaption, AFolder, AName: TcxPivotGridOLAPDBDataSetField;
  AParent, AFolderNode, ANode: TcxPivotGridOLAPStructureNode;
begin
  ADataSet := Provider.OpenSchema(Cube, osiSets);
  try
    if IsDataValid(ADataSet) then
    begin
      ADimensions := ADataSet.FieldByName('DIMENSIONS');
      ACaption := ADataSet.FieldByName('SET_CAPTION');
      AFolder := ADataSet.FieldByName('SET_DISPLAY_FOLDER');
      AName := ADataSet.FieldByName('SET_NAME');
      for I := 0 to ADataSet.RecordCount - 1 do
      begin
        APath := ADimensions.AsString;
        AParent := Structure;
        while APath <> '' do
        begin
          AItem := GetPathItem(APath);
          if APath <> '' then
            AParent := AParent.ItemByDisplayText(AItem);
          if AParent = nil then
            Break;
        end;
        if AParent <> nil then
        begin
          AFolderNode := AParent.ItemByDisplayText(AFolder.AsString);
          if AFolderNode = nil then
            AFolderNode := CreateNode(AParent, AFolder.AsString, ntSet);
          ANode := CreateNode(AFolderNode, ACaption.AsString, ntSet);
          ANode.DimensionUniqueName := ADimensions.AsString;
          ANode.UniqueName := AName.AsString;
          ANode.LevelUniqueName := ADimensions.AsString;
          ANode.HierarchyUniqueName := ADimensions.AsString;
        end;
        ADataSet.Next;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

type
  TcxPivotGridOLAPFieldAccess = class(TcxPivotGridOLAPField);

procedure TcxPivotGridOLAPDataSource.PopulateAllMemberUniqueNames;
var
  I: Integer;
  AKey: string;
  ANames: TDictionary<string, string>;
  AField: TcxPivotGridOLAPFieldAccess;
  AHierarchyUniqueName, AAllMemberUniqueName: string;
begin
  ANames := TDictionary<string, string>.Create;
  try
    for I := 0 to PivotGrid.FieldCount - 1 do
    begin
      AField := TcxPivotGridOLAPFieldAccess(PivotGrid.Fields[I]);
      AField.AllMemberUniqueName := AField.HierarchyUniqueName;
    end;
    Provider.PopulateHierarchyAllMemberNames(Cube, ANames);
    for AKey in ANames.Keys do
    begin
      AHierarchyUniqueName := AKey;
      AAllMemberUniqueName := ANames[AHierarchyUniqueName];
      for I := 0 to PivotGrid.FieldCount - 1 do
      begin
        AField := TcxPivotGridOLAPFieldAccess(PivotGrid.Fields[I]);
        if AField.HierarchyUniqueName = AHierarchyUniqueName then
          AField.AllMemberUniqueName := AAllMemberUniqueName;
      end;
    end;
  finally
    ANames.Free;
  end;
end;

function TcxPivotGridOLAPDataSource.CreateNode(AParent: TcxPivotGridOLAPStructureNode;
  const ACaption: string; ANodeType: TcxOLAPStructureNodeType): TcxPivotGridOLAPStructureNode;
begin
  Result := TcxPivotGridOLAPStructureNode(AParent.AddChild);
  Result.DisplayText := ACaption;
  Result.NodeType := ANodeType;
end;

procedure TcxPivotGridOLAPDataSource.CreateRootLayout(APivotGrid: TcxCustomPivotGrid; ARowFields, AColumnFields,
  ASummaryFields, AFilterFields: TcxPivotGridFields);
var
  ADataBuilder: TcxPivotGridDataBuilder;
  ARowGroup, AColumnGroup: TcxPivotGridGroupItem;
begin
  if csDesigning in ComponentState then
    Exit;
  ADataBuilder := TcxCustomPivotGridAccess(APivotGrid).DataBuilder;
  ARowGroup := ADataBuilder.Rows;
  AColumnGroup := ADataBuilder.Columns;
  if ARowFields.Count > 0 then
    PopulateChildrenGroups(ARowGroup, ARowFields[0], nil, ARowFields.Count > 1, ASummaryFields, AFilterFields);
  if AColumnFields.Count > 0 then
    PopulateChildrenGroups(AColumnGroup, AColumnFields[0], nil, AColumnFields.Count > 1, ASummaryFields, AFilterFields);
  if ASummaryFields.Count > 0 then
  begin
    if (ARowGroup.ItemCount > 0) and (AColumnGroup.ItemCount > 0) then
      PopulateCrossCells(AColumnGroup, ARowGroup, AColumnGroup, ARowFields, AColumnFields, ASummaryFields, AFilterFields);
    if (AColumnGroup.ItemCount > 0) then
      PopulateGrandTotals(AColumnGroup, ARowGroup, AColumnFields, ASummaryFields, AFilterFields, nil);
    if (ARowGroup.ItemCount > 0) then
      PopulateGrandTotals(ARowGroup, AColumnGroup, ARowFields, ASummaryFields, AFilterFields, nil);
  end;
  PopulateMainGrandTotal(ASummaryFields, AFilterFields, AColumnGroup, ARowGroup);

  if AColumnFields.Count > 0 then
    ProcessMembersForExpanding(AColumnFields[0], AColumnGroup, True);
  if ARowFields.Count > 0 then
    ProcessMembersForExpanding(ARowFields[0], ARowGroup, True);
end;

function CompareStructureNodes(ANode1, ANode2: TcxPivotGridOLAPStructureNode): Integer;
const
  NodeType2Compare: array[TcxOLAPStructureNodeType] of Integer =
    (0, 1, 2, 3, 4, 5, 7, 6);
begin
  Result := 0;
  if ANode1 = ANode2 then Exit;
  Result := NodeType2Compare[ANode1.NodeType] - NodeType2Compare[ANode2.NodeType];
  if Result = 0 then
  begin
    Result := ANode1.LevelNumber - ANode2.LevelNumber;
    if Result = 0 then
      Result := AnsiCompareText(ANode1.DisplayText, ANode2.DisplayText);
  end;
end;

procedure TcxPivotGridOLAPDataSource.CreateStructure;

  procedure CheckSingleNodes(ANode: TcxPivotGridOLAPStructureNode);
  var
    I: Integer;
  begin
    if not ANode.HasChildren then Exit;
    if ANode.NodeType = ntGroup then
    begin
      if ANode.Count = 1 then
      begin
        ANode.First.MoveTo(ANode.Parent, namAddChild);
        ANode.Free;
      end;
    end
    else
      for I := ANode.Count - 1 downto 0 do
        CheckSingleNodes(TcxPivotGridOLAPStructureNode(ANode.Items[I]));
  end;

begin
  Structure.Clear;
  Structure.DisplayText := Cube;
  AddMeasures;
  AddDimensions;
  AddKPIs;
  AddSETs;
  CheckSingleNodes(Structure);
  Structure.CustomSort(@CompareStructureNodes, True);
end;

procedure TcxPivotGridOLAPDataSource.DoPopulateGroupValues(AGroupValues: TcxPivotGridOLAPGroupInfos;
  AField: TcxPivotGridOLAPField; AnExpandedMembers: TList; ADataFields, AFilterFields: TcxPivotGridFields);
var
  AQuery: string;
  AReader: TcxCustomPivotGridOLAPDataSetReader;
  ADataSet: TcxCustomPivotGridOLAPDataSet;
begin
  AQuery := QueryBuilder.GetMembersQueryString(Cube, TcxPivotGridOLAPField(AField), AnExpandedMembers,
    ADataFields, AFilterFields);
  ADataSet := ExecuteQuery(AQuery);
  try
    if IsDataValid(ADataSet) then
    begin
      AReader := Provider.CreateMembersReader(ADataSet, AGroupValues, AField);
      try
        AReader.Execute;
      finally
        AReader.Free;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

function TcxPivotGridOLAPDataSource.GetLastNameFromPath(const APath: string): string;
var
  I: Integer;
begin
  I := Length(APath) - 1;
  while (I > 0) and (APath[I] <> '[') do
    Dec(I);
  Result := Copy(APath, I + 1, Length(APath) - I - 1);
end;

function TcxPivotGridOLAPDataSource.GetPathItem(var APath: string): string;
var
  APos: Integer;
begin
  Result := '';
  APos := Pos(']', APath);
  if APos = 0 then
    Exit;
  Result := Copy(APath, 2, APos - 2);
  Delete(APath, 1, APos + 1);
end;

procedure TcxPivotGridOLAPDataSource.PopulateChildrenGroups(AParentGroup: TcxPivotGridGroupItem; AField: TcxPivotGridField;
  AnExpandedMembers: TList; AHasChildren: Boolean; ADataFields, AFilterFields: TcxPivotGridFields);
var
  I: Integer;
  AGroupItem: TcxPivotGridGroupItem;
  AGroupValues: TcxPivotGridOLAPGroupInfos;
begin
  AGroupValues := TcxPivotGridOLAPGroupInfos.Create;
  try
    DoPopulateGroupValues(AGroupValues, TcxPivotGridOLAPField(AField), AnExpandedMembers, ADataFields, AFilterFields);
    for I := 0 to AGroupValues.Count - 1 do
    begin
      if not AGroupValues[I].ValueIsNull or (ADataFields.Count = 0) then
      begin
        AGroupItem := AParentGroup.AddChild(AParentGroup.ItemCount, I, AField);
        AGroupItem.InitializeValue(AGroupValues[I].DisplayText, AGroupValues[I].UniqueName);
        AGroupItem.HasChildren := AHasChildren;
      end;
    end;
  finally
    AGroupValues.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.PopulateGrandTotals(AColumnGroups, ARowGroups: TcxPivotGridGroupItem; AColumnFields,
  ASummaryFields, AFilterFields: TcxPivotGridFields; AnExpandedMembers: TList);
var
  AQuery: string;
  ADataSet: TcxCustomPivotGridOLAPDataSet;
  AReader: TcxCustomPivotGridOLAPDataSetReader;
begin
  if ASummaryFields.Count = 0 then
    Exit;
  AQuery := QueryBuilder.GetTotalsQueryString(Cube, AColumnFields, AnExpandedMembers, ASummaryFields, AFilterFields);
  ADataSet := ExecuteQuery(AQuery);
  try
    if not IsDataValid(ADataSet) then
      Exit;
    AReader := Provider.CreateGrandTotalsReader(ADataSet, AColumnGroups, ARowGroups, ASummaryFields, AnExpandedMembers);
    try
      AReader.Execute;
    finally
      AReader.Free;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.PopulateGroupValues(AField: TcxPivotGridOLAPField; AList: TcxPivotGridVariantList);
var
  I, AAxisIndex: Integer;
  AValue: TcxPivotGridVariantValue;
  AGroupValues: TcxPivotGridOLAPGroupInfos;
begin
  TcxPivotGridVariantListAccess(AList).FItems.Clear;
  AGroupValues := TcxPivotGridOLAPGroupInfos.Create;
  try
    AAxisIndex := TcxCustomOLAPDataQueryBuilderAccess(QueryBuilder).FAxisIndex;
    try
      DoPopulateGroupValues(AGroupValues, TcxPivotGridOLAPField(AField), nil, nil, nil);
    finally
      TcxCustomOLAPDataQueryBuilderAccess(QueryBuilder).FAxisIndex := AAxisIndex;
    end;
    for I := 0 to AGroupValues.Count - 1 do
    begin
      AValue := TcxPivotGridVariantValue.Create(AGroupValues[I].DisplayText);
      AValue.UniqueName := AGroupValues[I].UniqueName;
      TcxPivotGridVariantListAccess(AList).FItems.Add(AValue);
    end;
  finally
    AGroupValues.Free;
  end;
  TcxPivotGridVariantListAccess(AList).Changed;
  TcxPivotGridVariantListAccess(AList).FSorted := True;
end;

procedure TcxPivotGridOLAPDataSource.PopulateCrossCells(AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
  ARowFields, AColumnFields, ASummaryFields, AFilterFields: TcxPivotGridFields);
var
  AQuery: TcxMDXQuery;
  ADataSet: TcxCustomPivotGridOLAPDataSet;
  AReader: TcxCustomPivotGridOLAPDataSetReader;
begin
  if ASummaryFields.Count = 0 then
    Exit;
  AQuery := QueryBuilder.GetCrossCellsQuery(Cube, AOwnedGroup, AColumnFields, ARowFields, ASummaryFields,
    AFilterFields);
  try
    ADataSet := ExecuteQuery(AQuery);
    try
      if not IsDataValid(ADataSet) then
        Exit;
      AReader := Provider.CreateCrossCellsReader(ADataSet, AOwnedGroup, ARowGroups, AColumnGroups, ASummaryFields);
      try
        AReader.Execute;
      finally
        AReader.Free;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

function TcxPivotGridOLAPDataSource.CreateDrillDownDataSet(
  ACells: TList; ASummaryIndex: Integer): TObject;
var
  AQuery: string;
begin
  Result := nil;
  if not TcxPivotGridCrossCell(ACells[0]).IsEmpty then
  begin
    AQuery := GetDrillDownCommandText(TcxPivotGridCrossCell(ACells[0]), ASummaryIndex);
    if AQuery <> '' then
      Result := Provider.CreateDrillDownDataSet(AQuery, TcxPivotGridCrossCell(ACells[0]), ASummaryIndex);
  end;
end;

function TcxPivotGridOLAPDataSource.CreateDrillDownDataSource(
  ACells: TList; AFieldList: TcxObjectList; ASummaryIndex: Integer): TcxPivotGridCrossCellDataSource;
var
  AResult: TcxCustomPivotGridOLAPDrillDownDataSource absolute Result;
begin
  AResult := Provider.GetDrillDownDataSourceClass.CreateEx(ACells);
  AResult.DataSet := CreateDrillDownDataSet(ACells, ASummaryIndex);
end;

function TcxPivotGridOLAPDataSource.ExecuteQuery(const AQuery: string): TcxCustomPivotGridOLAPDataSet;
begin
  if AQuery = '' then
    Exit(nil);
  Result := Provider.Execute(AQuery);
end;

function TcxPivotGridOLAPDataSource.ExecuteQuery(AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet;
begin
  Result := Provider.Execute(AQuery);
end;

function TcxPivotGridOLAPDataSource.GetIsActive: Boolean;
begin
  Result := (Provider.Connected or FTerminated) and inherited GetIsActive;
  Result := Result and ([csLoading, csDestroying] * ComponentState = []);
end;

function TcxPivotGridOLAPDataSource.GetIsTerminated: Boolean;
begin
  Result := FTerminated;
end;

function TcxPivotGridOLAPDataSource.GetNonExpandedFields(ADataBuilder: TcxPivotGridDataBuilder;
  AnExpandingGroup, AnGroup: TcxPivotGridGroupItem; ACrossGroupItems: TList; AddHierarchyField: Boolean): TcxPivotGridFields;

  function HierarchyFieldDuplicationAvoidance(AFields: TcxPivotGridFields; ACrossGroupItems: TList; AnExpandingItem: TcxPivotGridGroupItem): Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := 0 to AFields.Count - 1 do
    begin
      if (AFields[I].Group <> nil) and (AFields[I].Group = AnExpandingItem.Field.Group) then
        Result := False;
    end;

    if Result and (ACrossGroupItems <> nil) then
      for I := ACrossGroupItems.Count - 1 downto 0 do
      begin
        if TcxPivotGridGroupItem(ACrossGroupItems[I]).Field.Group = AnExpandingItem.Field.Group then
          ACrossGroupItems.Delete(I);
      end;
  end;

  function IsFieldExist(AField: TcxPivotGridField): Boolean;
  begin
    Result := AField.GroupIndex + 1 < AField.Group.VisibleCount;
  end;

var
  AFieldList: TcxPivotGridFields;
begin
  Result := TcxPivotGridFields.Create;
  if AnExpandingGroup <> nil then
  begin
    if not IsNotLastVisibleOLAPHierarchyItem(AnExpandingGroup) then
    begin
      AFieldList := ADataBuilder.GetFieldsListByArea(AnExpandingGroup.Field.Area);
      Result.Add(AFieldList.Fields[AFieldList.IndexOf(AnExpandingGroup.Field) + 1]);
    end
    else
      if AddHierarchyField and IsOLAPHierarchy(AnExpandingGroup) and
        HierarchyFieldDuplicationAvoidance(Result, ACrossGroupItems, AnExpandingGroup) and
        IsFieldExist(AnExpandingGroup.Field) then
          Result.Add(AnExpandingGroup.Field.Group.Fields[AnExpandingGroup.Field.GroupIndex + 1]);
  end;
  if (AnGroup <> nil) and not IsNotLastVisibleOLAPHierarchyItem(AnGroup) and AnGroup.HasChildren then
    Result.Add(AnGroup.Items[0].Field)
  else
    if AddHierarchyField and (AnGroup <> nil) and IsOLAPHierarchy(AnGroup) and IsFieldExist(AnGroup.Field) then
      Result.Add(AnGroup.Field.Group.Fields[AnGroup.Field.GroupIndex + 1]);
end;

procedure TcxPivotGridOLAPDataSource.Initialize;
var
  ACubes: TStrings;
begin
  inherited Initialize;
  RecreateQueryBuilder;
  if not IsCubeNameAssigned then
    FCube := '';
  ACubes := TStringList.Create;
  try
    Provider.PopulateCubeNames(ACubes);
    if IsCubeNameAssigned then
    begin
      if ACubes.IndexOf(Cube) < 0 then
      begin
        Active := False;
        PivotGridError(False, Format(scxInvalidCubeName, [Cube]));
      end;
    end
    else
      if ACubes.Count > 0 then
        FCube := ACubes[0];
  finally
    ACubes.Free;
  end;
  if csDesigning in ComponentState then
    TerminateConnection;
end;

procedure TcxPivotGridOLAPDataSource.InitializeExpanding(ADataBuilder: TcxPivotGridDataBuilder; AField: TcxPivotGridField;
  AnExpandingGroup, ACrossGroup: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields);
var
  AMembers: TList;
  AHasChildren: Boolean;
  AFields, ANonExpandedFields, AFilter: TcxPivotGridFields;
begin
  if AnExpandingGroup.UniqueName = '' then
    Exit;
  AMembers := TList.Create;
  try
    ExpandedItemWithParentsToList(AnExpandingGroup, AMembers);
    AHasChildren := (AField.Area = faColumn) and (AField.VisibleIndex < (ADataBuilder.ColumnFields.Count - 1))
      or (AField.Area = faRow) and (AField.VisibleIndex < (ADataBuilder.RowFields.Count - 1));
    PopulateChildrenGroups(AnExpandingGroup, AField, AMembers, AHasChildren, ASummaryFields, ADataBuilder.FilteredFields);
    if AnExpandingGroup.ItemCount = 0 then
      Exit;

    ANonExpandedFields := GetNonExpandedFields(ADataBuilder, AnExpandingGroup, nil, nil, False);
    try
      AFields := GetNonExpandedFields(ADataBuilder, AnExpandingGroup, ACrossGroup, nil, False);
      try
        if ACrossGroup.ItemCount > 0 then
        begin
          AFilter := TcxPivotGridFields.Create;
          try
            AFilter.Assign(ADataBuilder.FilteredFields);
            PopulateCrossCells(AnExpandingGroup, ADataBuilder.Rows, ADataBuilder.Columns,
              ADataBuilder.RowFields, ADataBuilder.ColumnFields, ASummaryFields, AFilter);
          finally
            AFilter.Free;
          end;
        end;
        if AnExpandingGroup.Field.Area = faColumn then
          PopulateGrandTotals(AnExpandingGroup, ADataBuilder.Rows, ANonExpandedFields, ASummaryFields,
            ADataBuilder.FilteredFields, AMembers)
        else
          PopulateGrandTotals(AnExpandingGroup, ADataBuilder.Columns, ANonExpandedFields, ASummaryFields,
            ADataBuilder.FilteredFields, AMembers);
        finally
          AFields.Free;
        end;
    finally
      ANonExpandedFields.Free;
    end;
  finally
    AMembers.Free;
  end;
end;

procedure TcxPivotGridOLAPDataSource.PopulateMainGrandTotal(ADataFields, AFilterFields: TcxPivotGridFields;
  AColumns, ARows: TcxPivotGridGroupItem);
var
  AQuery: string;
  ADataSet: TcxCustomPivotGridOLAPDataSet;
  AReader: TcxCustomPivotGridOLAPDataSetReader;
begin
  if ADataFields.Count = 0 then
    Exit;
  AQuery := QueryBuilder.GetTotalQueryString(Cube, AFilterFields, ADataFields);
  ADataSet := ExecuteQuery(AQuery);
  try
    if IsDataValid(ADataSet) then
    begin
      AReader := Provider.CreateMainGrandTotalReader(ADataSet, AColumns, ARows, ADataFields);
      try
        AReader.Execute;
      finally
        AReader.Free;
      end;
    end;
  finally
    ADataSet.Free;
  end;
end;

function TcxPivotGridOLAPDataSource.IsValueVisible(
  AFilter: TcxPivotGridFieldFilter; const AValue: WideString): Boolean;
begin
  Result := not AFilter.HasFilter or AFilter.Contains(AValue);
end;

procedure TcxPivotGridOLAPDataSource.Loaded;
begin
  inherited Loaded;
  NotifyListeners;
end;

procedure TcxPivotGridOLAPDataSource.PopulateFilteredUniqueNames(AField: TcxPivotGridOLAPField;
  AFilter: TcxPivotGridFieldFilter; var AUniqueValues: TStringList);

  function GetParentUniqueName(const AUniqueName: string): string;
  var
    ADataSet: TcxCustomPivotGridOLAPDataSet;
  begin
    if Provider is TcxPivotGridOLAPOLEDBProvider then
    begin
      ADataSet := ExecuteQuery(QueryBuilder.GetParentQueryString(Cube, TcxPivotGridOLAPField(AField), AUniqueName));
      try
        Result := (ADataSet as TcxPivotGridOLAPDBDataSet).FieldByName(OLAPParentUniqueNameColumnName).AsString;
      finally
        ADataSet.Free;
      end;
    end
    else
      Result := '';
  end;

var
  I: Integer;
  AValues: TStringList;
  AUniqueName: string;
begin
  if not AFilter.HasFilter and (AUniqueValues = nil) then
    Exit;
  if AUniqueValues <> nil then
    AUniqueValues.Sort;
  AValues := TStringList.Create;
  try
    for I := 0 to AField.GroupValueList.Count - 1 do
    begin
      AUniqueName := AField.GroupValueList.Items[I].UniqueName;
      if IsValueVisible(AFilter, AField.GroupValueList.Items[I].Value) and
        ((AUniqueValues = nil) or (AUniqueValues.IndexOf(AUniqueName) <> -1)) then
        AValues.Add(GetParentUniqueName(AUniqueName));
    end;
    FreeAndNil(AUniqueValues);
  finally
    AUniqueValues := AValues;
  end;
end;

procedure TcxPivotGridOLAPDataSource.PopulateFilteredValues(
  AField: TcxPivotGridOLAPField; AFilter: TcxPivotGridFieldFilter;
  AValues: TStrings; AUniqueValues: TStringList);
var
  I: Integer;
  AUniqueName: string;
begin
  if AUniqueValues <> nil then
    AUniqueValues.Sort;
  for I := 0 to AField.GroupValueList.Count - 1 do
  begin
    AUniqueName := AField.GroupValueList.Items[I].UniqueName;
    if IsValueVisible(AFilter, AField.GroupValueList.Items[I].Value) and
      ((AUniqueValues = nil) or (AUniqueValues.IndexOf(AUniqueName) <> -1)) then
      AValues.Add(AUniqueName);
  end;
end;

procedure TcxPivotGridOLAPDataSource.TerminateConnection;
begin
  if Provider = nil then
    Exit;
  FTerminated := Provider.Connected;
  if FTerminated then
    Provider.Close;
end;

procedure TcxPivotGridOLAPDataSource.RecreateQueryBuilder;
begin
  FreeAndNil(FQueryBuilder);
  QueryBuilder := Provider.CreateQueryBuilder;
end;

procedure TcxPivotGridOLAPDataSource.ExpandedItemWithParentsToList(AnGroup: TcxPivotGridGroupItem; AList: TList);
var
  APreviousGroup: TcxPivotGridGroupItem;
begin
  APreviousGroup := nil;
  while AnGroup.UniqueName <> '' do
  begin
    if (APreviousGroup = nil) or (AnGroup.Field.Group = nil) or (APreviousGroup.Field.Group <> AnGroup.Field.Group) then
      AList.Add(AnGroup);
    APreviousGroup := AnGroup;
    AnGroup := AnGroup.Parent;
  end;
end;

function TcxPivotGridOLAPDataSource.GetActive: Boolean;
begin
  Result := IsActive;
end;

function TcxPivotGridOLAPDataSource.GetProviderClassName: string;
begin
  if Provider = nil then
    Result := ''
  else
    Result := Provider.ClassName;
end;

function TcxPivotGridOLAPDataSource.GetConnectionString: string;
begin
  Result := Provider.ConnectionString;
end;

function TcxPivotGridOLAPDataSource.GetDrillDownCommandText(ACell: TcxPivotGridCrossCell; ASummaryIndex: Integer): string;
begin
  Result := QueryBuilder.GetDrillDownQueryString(Cube, ACell, ASummaryIndex);
end;

function TcxPivotGridOLAPDataSource.IsDataValid(ADataSet: TcxCustomPivotGridOLAPDataSet): Boolean;
begin
  Result := (ADataSet <> nil) and ADataSet.IsValid;
end;

procedure TcxPivotGridOLAPDataSource.SetActive(AValue: Boolean);
begin
  if (Provider = nil) or (Provider.ConnectionString = '') then
    Exit;
  if AValue then
  begin
    try
      try
        Provider.Connected := True;
      except
        AValue := False;
        raise;
      end;
    finally
      inherited Active := AValue;
    end;
  end
  else
  begin
    inherited Active := False;
    TerminateConnection;
    FTerminated := False;
  end;
end;

procedure TcxPivotGridOLAPDataSource.SetProvider(const Value: TcxCustomPivotGridOLAPProvider);
begin
  if (Provider <> nil) and (Value <> nil) then
    Provider.Assign(Value);
end;

procedure TcxPivotGridOLAPDataSource.SetProviderClass(const Value: TcxCustomPivotGridOLAPProviderClass);
var
  AOldProvider: TcxCustomPivotGridOLAPProvider;
begin
  if FProviderClass <> Value then
  begin
    Active := False;
    PivotGridError(Value <> nil, 'Invalid provider');
    FProviderClass := Value;
    AOldProvider := FProvider;
    FProvider := FProviderClass.Create(Self);
    Provider := AOldProvider;
    AOldProvider.Free
  end;
end;

procedure TcxPivotGridOLAPDataSource.SetProviderClassName(const Value: string);
begin
  if ProviderClassName <> Value then
    ProviderClass := TcxCustomPivotGridOLAPProviderClass(cxGetPivotGridOLAPDataSourceProviders.FindByClassName(Value));
end;

procedure TcxPivotGridOLAPDataSource.SetConnectionString(const AValue: string);
begin
  Provider.ConnectionString := AValue;
end;

procedure TcxPivotGridOLAPDataSource.ReadCacheLocalCubes(AReader: TReader);
begin
  Provider.CacheLocalCubes := AReader.ReadBoolean;
end;

procedure TcxPivotGridOLAPDataSource.SetCube(const AValue: string);
begin
  FCube := AValue;
  if Active and not (csLoading in ComponentState) then
    Changed;
  FIsCubeNameAssigned := AValue <> '';
end;

end.


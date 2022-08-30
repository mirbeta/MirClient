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

unit cxPivotGridOLAPADOMDProvider;

{$I cxVer.inc}

interface

uses
  Windows, Dialogs, Contnrs, Types, Classes, SysUtils, Forms, Math, Variants, StrUtils, DB, ADODB, ComObj,
  ActiveX, Generics.Defaults, Generics.Collections, dxCore, cxClasses, cxPivotGridOLAPDataSource,
  cxCustomPivotGrid, cxADOMDInt, cxPivotGridOLAPQueryBuilder;

type

{$REGION 'internal types'}

  { TdxADORecordset }

  TdxADORecordset = class
  strict private
    FRecordset: _Recordset;
    function GetStates: TObjectStates;
  public
    constructor Create;
    destructor Destroy; override;
    function Eof: Boolean;
    function FieldCount: Integer;
    function GetFieldName(AFieldIndex: Integer): string;
    function GetFieldValue(AFieldIndex: Integer): OleVariant; overload;
    function GetFieldValue(const AFieldName: string): OleVariant; overload;
    function RecordCount: Integer;
    procedure MoveFirst;
    procedure MoveNext;

    procedure Open(const ACommandText: string; const AActiveConnection: IDispatch);
  end;

  { TcxPivotGridOLAPMemDataDataSet }

  TcxPivotGridOLAPMemDataDataSet = class(TcxPivotGridOLAPDBDataSet)
  protected
    function CreateDataSet: TDataSet; override;
  end;

  { TcxPivotGridOLAPADOMDDataSet }

  TcxPivotGridOLAPADOMDDataSet = class(TcxCustomPivotGridOLAPDataSet)
  private
    FCellSet: ICellSet;
  protected
    FQuery: TcxMDXQuery;
    procedure Initialize(const ACellSet: ICellSet);
    function IsValid: Boolean; override;

    property Query: TcxMDXQuery read FQuery;
  public
    destructor Destroy; override;
  end;

  { TcxPivotGridOLAPCellCoordinate }

  TcxPivotGridOLAPCellCoordinate = record
  strict private
    FColumn: TStringDynArray;
    FRow: TStringDynArray;
    FMeasure: string;
  private
    procedure ClearRow;
  public
    constructor Create(const AColumn, ARow: TStringDynArray; const AMeasure: string);

    property Column: TStringDynArray read FColumn;
    property Row: TStringDynArray read FRow;
    property Measure: string read FMeasure;
  end;

  { TcxPivotGridOLAPADOMDCellSetEnumerator }

  TcxPivotGridOLAPADOMDCellSetEnumerator = class
  protected type

    TPositionsEnumerator = class(TEnumerator<Position>)
    strict private
      FIndex: Integer;
      FPositions: Positions;
      FMemberNamesCache: TDictionary<Integer, TStringDynArray>;
    protected
      function DoGetCurrent: Position; override;
      function DoMoveNext: Boolean; override;
      function DoMoveBack: Boolean; virtual;
    public
      constructor Create(const APositions: Positions);
      destructor Destroy; override;
      class function ExtractMeasureUniqueName(var AUniqueNames: TStringDynArray): string;
      function Count: Integer; inline;
      procedure First;
      function GetCurrentMemberNames: TStringDynArray;
      procedure Reset; inline;
      function MoveBack: Boolean;

      property Index: Integer read FIndex;
    end;

  strict private
    FDataSet: TcxPivotGridOLAPADOMDDataSet;
    FRowEnumerator: TPositionsEnumerator;
    FColumnEnumerator: TPositionsEnumerator;
    function DoFirst(AEnumerator: TPositionsEnumerator): Boolean;
    function DoMoveNext(AEnumerator: TPositionsEnumerator): Boolean;
    procedure Initialize;
    function GetCurrentCell: Cell;
    function GetCurrentCellValue: Variant;
  strict protected
    property DataSet: TcxPivotGridOLAPADOMDDataSet read FDataSet;
  protected
    function FirstRow: Boolean;
    function FirstColumn: Boolean;
    function MoveNextRow: Boolean;
    function MoveNextColumn: Boolean;

    function GetCoordinate: TcxPivotGridOLAPCellCoordinate;

    property CurrentCellValue: Variant read GetCurrentCellValue;

    property ColumnEnumerator: TPositionsEnumerator read FColumnEnumerator;
    property RowEnumerator: TPositionsEnumerator read FRowEnumerator;
  public
    constructor Create(const ADataSet: TcxPivotGridOLAPADOMDDataSet);
    destructor Destroy; override;
  end;

{$ENDREGION}

  { TcxPivotGridOLAPADOMDProvider }

  TcxPivotGridOLAPADOMDProvider = class(TcxCustomPivotGridOLAPProvider)
  strict private
    FCatalog: ICatalog;
    function AddFieldToDataSet(ADataSet: TDataSet; const AFieldName: string; AFieldType: TFieldType): TField;
    function DoGetKPIs(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
    function DoGetLevels(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
    function DoGetMeasures(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
    function DoGetHierarchies(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
    function DoGetSets(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
    function GetCubeDefByName(const ACubeName: string): CubeDef;
    function GetPropertyValue(const AProperties: Properties; const APropertyName: string;
      const ADefaultValue: Variant): Variant;
    function GetPropertyIntValue(const AProperties: Properties; const APropertyName: string;
      const ADefaultValue: Integer = 0): Integer;
    function GetPropertyStrValue(const AProperties: Properties; const APropertyName: string;
      const ADefaultValue: string = ''): string;
    function OpenRecordset(const ACommandText: string): TdxADORecordset;
  protected
    function CreateDrillDownDataSet(const ACommandText: string; ACrossCell: TcxPivotGridCrossCell;
      ASummaryIndex: Integer): TObject; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function Execute(const ACommandText: string): TcxCustomPivotGridOLAPDataSet; override;
    function Execute(const AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet; override;
    function GetConnected: Boolean; override;
    function GetDrillDownDataSourceClass: TcxCustomPivotGridOLAPDrillDownDataSourceClass; override;

    procedure PopulateCatalogs(ANames: TStrings); override;
    procedure PopulateHierarchyAllMemberNames(const ACubeName: string; ANames: TDictionary<string, string>); override;
    function OpenSchema(const ACubeName: string; ASchema: TcxPivotGridOLAPSchemaInfo): TcxPivotGridOLAPDBDataSet; override;

    function CreateQueryBuilder: TcxCustomOLAPDataQueryBuilder; override;
    function CreateCrossCellsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AOwnedGroup, ARowGroups,
      AColumnGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateGrandTotalsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroups,
      ARowGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields;
      AnExpandedMembers: TList): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateMainGrandTotalReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup,
      ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateMembersReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AGroupValues: TcxPivotGridOLAPGroupInfos;
      AField: TcxPivotGridField): TcxCustomPivotGridOLAPDataSetReader; override;

    property Catalog: ICatalog read FCatalog;
  public
    constructor Create(ADataSource: TcxPivotGridOLAPDataSource); override;
    destructor Destroy; override;

    class function GetDisplayName: string; override;
    procedure PopulateCubeNames(ANames: TStrings); override;
  published
    property CacheLocalCubes;
    property ConnectionString;
  end;

  { TcxCustomPivotGridOLAPADOMDDataSetReader }

  TcxCustomPivotGridOLAPADOMDDataSetReader = class(TcxCustomPivotGridOLAPDataSetReader)
  protected const
    InvalidPosition = -1;
  strict private
    function GetDataSet: TcxPivotGridOLAPADOMDDataSet;
  protected
    class function NativeValueToValue(const ANativeValue: Variant): Variant;

    property DataSet: TcxPivotGridOLAPADOMDDataSet read GetDataSet;
  public
    constructor Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
  end;

  { TcxPivotGridOLAPADOMDDataSetCrossCellsReader }

  TcxPivotGridOLAPADOMDDataSetCrossCellsReader = class(TcxCustomPivotGridOLAPADOMDDataSetReader)
  private
    FOwnedGroup: TcxPivotGridGroupItem;
    FRowGroups: TcxPivotGridGroupItem;
    FColumnGroups: TcxPivotGridGroupItem;
    FSummaryFields: TcxPivotGridFields;
  protected
    procedure Initialize(AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields);

    property OwnedGroup: TcxPivotGridGroupItem read FOwnedGroup;
    property RowGroups: TcxPivotGridGroupItem read FRowGroups;
    property ColumnGroups: TcxPivotGridGroupItem read FColumnGroups;
    property SummaryFields: TcxPivotGridFields read FSummaryFields;
  public
    constructor Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
    procedure Execute; override;
  end;

  { TcxPivotGridOLAPADOMDDataSetMembersReader }

  TcxPivotGridOLAPADOMDDataSetMembersReader = class(TcxCustomPivotGridOLAPADOMDDataSetReader)
  strict private
    FGroupValues: TcxPivotGridOLAPGroupInfos;
    FField: TcxPivotGridField;
  protected
    procedure Initialize(AGroupValues: TcxPivotGridOLAPGroupInfos; AField: TcxPivotGridField);

    property GroupValues: TcxPivotGridOLAPGroupInfos read FGroupValues;
    property Field: TcxPivotGridField read FField;
  public
    procedure Execute; override;
  end;

  { TcxPivotGridOLAPADOMDDataSetGrandTotalReader }

  TcxPivotGridOLAPADOMDDataSetGrandTotalReader = class(TcxCustomPivotGridOLAPADOMDDataSetReader)
  strict private
    FRowGroups: TcxPivotGridGroupItem;
    FColumnGroups: TcxPivotGridGroupItem;
    FAnExpandedMembers: TList;
    FSummaryFields: TcxPivotGridFields;
  protected
    procedure Initialize(AColumnGroups, ARowGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields;
      AnExpandedMembers: TList);

    property ColumnGroups: TcxPivotGridGroupItem read FColumnGroups;
    property RowGroups: TcxPivotGridGroupItem read FRowGroups;
    property SummaryFields: TcxPivotGridFields read FSummaryFields;
    property AnExpandedMembers: TList read FAnExpandedMembers;
  public
    procedure Execute; override;
  end;

  { TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader }

  TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader = class(TcxCustomPivotGridOLAPADOMDDataSetReader)
  strict private
    FDataFields: TcxPivotGridFields;
    FRowGroup: TcxPivotGridGroupItem;
    FColumnGroup: TcxPivotGridGroupItem;
  protected
    procedure Initialize(AColumnGroup, ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields);

    property ColumnGroup: TcxPivotGridGroupItem read FColumnGroup;
    property RowGroup: TcxPivotGridGroupItem read FRowGroup;
    property DataFields: TcxPivotGridFields read FDataFields;
  public
    procedure Execute; override;
  end;


  { TcxPivotGridOLAPCellMatrix }

  TcxPivotGridOLAPCellMatrix = class //TODO: remove
  private type
  {$REGION 'private types'}
    TValue = record
    strict private
      FCell: TcxPivotGridCrossCell;
      FFieldIndex: Integer;
    public
      constructor Create(ACell: TcxPivotGridCrossCell; AFieldIndex: Integer);

      property Cell: TcxPivotGridCrossCell read FCell;
      property FieldIndex: Integer read FFieldIndex;
    end;
  {$ENDREGION}
  strict private
    FColumnGroups: TcxPivotGridGroupItem;
    FRowGroups: TcxPivotGridGroupItem;
    FSummaryFields: TcxPivotGridFields;
    function FindGroupByUniqueNames(AParentGroup: TcxPivotGridGroupItem;
      const ANames: TStringDynArray): TcxPivotGridGroupItem;
    function GetGroupByUniqueName(AGroups: TcxPivotGridGroupItem; const AUniqueName: string): TcxPivotGridGroupItem;
    function GetSummaryFieldIndex(const AUniqueName: string): Integer;
  protected
    property ColumnGroups: TcxPivotGridGroupItem read FColumnGroups;
    property RowGroups: TcxPivotGridGroupItem read FRowGroups;
    property SummaryFields: TcxPivotGridFields read FSummaryFields;
  public
    constructor Create(ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields);
    function TryGetCellValueInfo(const AKey: TcxPivotGridOLAPCellCoordinate;
      out AValue: TcxPivotGridOLAPCellMatrix.TValue): Boolean;
  end;

implementation

uses
  dxmdaset, cxPivotGridStrs, RTLConsts, ADOInt;

{ TdxADORecordset }

constructor TdxADORecordset.Create;
begin
  inherited Create;
  FRecordset := CoRecordset.Create;
end;

destructor TdxADORecordset.Destroy;
var
  AStates: TObjectStates;
begin
  AStates := GetStates;
  if (stOpen in AStates) and not (stClosed in AStates) then
    FRecordset.Close;
  FRecordset := nil;
  inherited Destroy;
end;

function TdxADORecordset.Eof: Boolean;
begin
  Result := FRecordset.EOF;
end;

function TdxADORecordset.FieldCount: Integer;
begin
  Result := FRecordset.Fields.Count;
end;

function TdxADORecordset.GetFieldName(AFieldIndex: Integer): string;
begin
  Result := FRecordset.Fields[AFieldIndex].Name;
end;

function TdxADORecordset.GetFieldValue(const AFieldName: string): OleVariant;
begin
  Result := FRecordset.Fields[AFieldName].Value;
end;

function TdxADORecordset.GetFieldValue(AFieldIndex: Integer): OleVariant;
begin
  Result := FRecordset.Fields[AFieldIndex].Value;
end;

procedure TdxADORecordset.MoveFirst;
begin
  if FRecordset.RecordCount > 0 then
    FRecordset.MoveFirst;
end;

procedure TdxADORecordset.MoveNext;
begin
  FRecordset.MoveNext;
end;

procedure TdxADORecordset.Open(const ACommandText: string; const AActiveConnection: IDispatch);
begin
  FRecordset.Open(ACommandText, AActiveConnection, adOpenStatic, adLockReadOnly, 0);
end;

function TdxADORecordset.RecordCount: Integer;
begin
  Result := FRecordset.RecordCount;
end;

function TdxADORecordset.GetStates: TObjectStates;
var
  AState: Integer;
begin
  Result := [];
  AState := FRecordset.State;
  if (adStateOpen and AState) <> 0 then
    Include(Result, stOpen);
  if (adStateClosed and AState) <> 0 then
    Include(Result, stClosed);
end;

{ TcxPivotGridOLAPMemDataDataSet }

function TcxPivotGridOLAPMemDataDataSet.CreateDataSet: TDataSet;
begin
  Result := TdxCustomMemData.Create(nil);
end;

{ TcxPivotGridOLAPADOMDDataSet }

destructor TcxPivotGridOLAPADOMDDataSet.Destroy;
begin
  FCellSet.Close;
  FCellSet := nil;
  inherited Destroy;
end;

procedure TcxPivotGridOLAPADOMDDataSet.Initialize(const ACellSet: ICellSet);
begin
  Assert(FCellSet = nil);
  FCellSet := ACellSet;
end;

function TcxPivotGridOLAPADOMDDataSet.IsValid: Boolean;
begin
  Result := FCellSet <> nil;
end;

{ TcxPivotGridOLAPCellCoordinate }

constructor TcxPivotGridOLAPCellCoordinate.Create(const AColumn, ARow: TStringDynArray; const AMeasure: string);
begin
  FColumn := AColumn;
  FRow := ARow;
  FMeasure := AMeasure;
end;

procedure TcxPivotGridOLAPCellCoordinate.ClearRow;
begin
  SetLength(FRow, 0);
end;

{ TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator }

constructor TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.Create(const APositions: Positions);
begin
  inherited Create;
  FPositions := APositions;
  FIndex := -1;
  FMemberNamesCache := TDictionary<Integer, TStringDynArray>.Create(Count);
end;

destructor TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.Destroy;
begin
  FreeAndNil(FMemberNamesCache);
  FPositions := nil;
  inherited Destroy;
end;

class function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.ExtractMeasureUniqueName(
  var AUniqueNames: TStringDynArray): string;
var
  ALength: Integer;
begin
  ALength := Length(AUniqueNames);
  if ALength < 1 then
    raise EArgumentOutOfRangeException.CreateRes(@SArgumentOutOfRange);
  Result := AUniqueNames[ALength - 1];
  SetLength(AUniqueNames, ALength - 1);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.Count: Integer;
begin
  Result := FPositions.Count;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.GetCurrentMemberNames: TStringDynArray;
var
  AMembers: Members;
  I, AMembersCount: Integer;
begin
  if not FMemberNamesCache.TryGetValue(Index, Result) then
  begin
    AMembers := Current.Members;
    AMembersCount := AMembers.Count;
    SetLength(Result, AMembersCount);
    for I := 0 to AMembersCount - 1 do
      Result[I] := AMembers[I].UniqueName;
    FMemberNamesCache.Add(Index, Result);
  end;
end;

procedure TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.First;
begin
  Reset;
  MoveNext;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.MoveBack: Boolean;
begin
  Result := DoMoveBack;
end;

procedure TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.Reset;
begin
  FIndex := -1;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.DoGetCurrent: Position;
begin
  Result := FPositions[FIndex];
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.DoMoveNext: Boolean;
begin
  Inc(FIndex);
  Result := FIndex < Count;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.TPositionsEnumerator.DoMoveBack: Boolean;
begin
  Dec(FIndex);
  Result := FIndex > -1;
end;

{ TcxPivotGridOLAPADOMDCellSetEnumerator }

constructor TcxPivotGridOLAPADOMDCellSetEnumerator.Create(const ADataSet: TcxPivotGridOLAPADOMDDataSet);
begin
  inherited Create;
  FDataSet := ADataSet;
  Initialize;
end;

destructor TcxPivotGridOLAPADOMDCellSetEnumerator.Destroy;
begin
  FreeAndNil(FRowEnumerator);
  FreeAndNil(FColumnEnumerator);
  inherited Destroy;
end;

procedure TcxPivotGridOLAPADOMDCellSetEnumerator.Initialize;
var
  AAxes: Axes;
  AAxisCount: Integer;
begin
  AAxes := DataSet.FCellSet.Axes;
  AAxisCount := AAxes.Count;
  if AAxisCount = 1 then
    FColumnEnumerator := TPositionsEnumerator.Create(AAxes[0].Positions)
  else
    if AAxisCount = 2 then
    begin
      FColumnEnumerator := TPositionsEnumerator.Create(AAxes[0].Positions);
      FRowEnumerator := TPositionsEnumerator.Create(AAxes[1].Positions);
    end
    else
      raise Exception.Create('Not implemented');
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.FirstColumn: Boolean;
begin
  Result := DoFirst(ColumnEnumerator);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.FirstRow: Boolean;
begin
  Result := DoFirst(RowEnumerator);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.GetCoordinate: TcxPivotGridOLAPCellCoordinate;
var
  AMeasure: string;
  AColumn, ARow: TStringDynArray;
begin
  SetLength(ARow, 0);
  AColumn := ColumnEnumerator.GetCurrentMemberNames;
  AMeasure := ColumnEnumerator.ExtractMeasureUniqueName(AColumn);
  if RowEnumerator <> nil then
    ARow := RowEnumerator.GetCurrentMemberNames;
  Result := TcxPivotGridOLAPCellCoordinate.Create(AColumn, ARow, AMeasure);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.MoveNextColumn: Boolean;
begin
  Result := DoMoveNext(ColumnEnumerator);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.MoveNextRow: Boolean;
begin
  Result := DoMoveNext(RowEnumerator);
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.GetCurrentCell: Cell;
var
  ACoordinate: OleVariant;
begin
  if RowEnumerator = nil then
  begin
    ACoordinate := VarArrayCreate([0, 0], varVariant);
    ACoordinate[0] := ColumnEnumerator.Index;
  end
  else
  begin
    ACoordinate := VarArrayCreate([0, 1], varVariant);
    ACoordinate[0] := ColumnEnumerator.Index;
    ACoordinate[1] := RowEnumerator.Index;
  end;
  Result := DataSet.FCellSet.Item[PSafeArray(TVarData(ACoordinate).VArray)];
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.GetCurrentCellValue: Variant;
var
  ACell: Cell;
begin
  Result := Null;
  ACell := GetCurrentCell;
  if ACell.FormattedValue <> '' then
    Result := ACell.Value;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.DoFirst(AEnumerator: TPositionsEnumerator): Boolean;
begin
  Result := AEnumerator <> nil;
  if Result then
    AEnumerator.First;
end;

function TcxPivotGridOLAPADOMDCellSetEnumerator.DoMoveNext(AEnumerator: TPositionsEnumerator): Boolean;
begin
  Result := AEnumerator <> nil;
  if Result then
    Result := AEnumerator.MoveNext;
end;

{ TcxPivotGridOLAPADOMDProvider }

constructor TcxPivotGridOLAPADOMDProvider.Create(ADataSource: TcxPivotGridOLAPDataSource);
begin
  inherited Create(ADataSource);
  CacheLocalCubes := True;
end;

destructor TcxPivotGridOLAPADOMDProvider.Destroy;
begin
  DoDisconnect;
  inherited Destroy;
end;

class function TcxPivotGridOLAPADOMDProvider.GetDisplayName: string;
begin
  Result := 'ADO MD';
end;

procedure TcxPivotGridOLAPADOMDProvider.PopulateCubeNames(ANames: TStrings);
var
  I: Integer;
  ACubeDef: CubeDef;
  ACubeDefs: CubeDefs;
begin
  ANames.Clear;
  if not Connected then
    Open;
  ACubeDefs := Catalog.CubeDefs;
  try
    for I := 0 to ACubeDefs.Count - 1 do
    begin
      ACubeDef := ACubeDefs[I];
      ANames.Add(ACubeDef.Name);
    end;
  finally
    ACubeDef := nil;
    ACubeDefs := nil;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.CreateQueryBuilder: TcxCustomOLAPDataQueryBuilder;
begin
  Result := TcxADOMDOLAPDataQueryBuilder.Create;
end;

function TcxPivotGridOLAPADOMDProvider.CreateCrossCellsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AOwnedGroup,
  ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
  ASummaryFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPADOMDDataSetCrossCellsReader;
begin
  AReader := TcxPivotGridOLAPADOMDDataSetCrossCellsReader.Create(ADataSet);
  AReader.Initialize(AOwnedGroup, ARowGroups, AColumnGroups, ASummaryFields);
  Result := AReader;
end;

function TcxPivotGridOLAPADOMDProvider.CreateGrandTotalsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroups,
  ARowGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields;
  AnExpandedMembers: TList): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPADOMDDataSetGrandTotalReader;
begin
  AReader := TcxPivotGridOLAPADOMDDataSetGrandTotalReader.Create(ADataSet);
  AReader.Initialize(AColumnGroups, ARowGroups, ASummaryFields, AnExpandedMembers);
  Result := AReader;
end;

function TcxPivotGridOLAPADOMDProvider.CreateMainGrandTotalReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup,
  ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader;
begin
  AReader := TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader.Create(ADataSet);
  AReader.Initialize(AColumnGroup, ARowGroup, ADataFields);
  Result := AReader;
end;

function TcxPivotGridOLAPADOMDProvider.CreateMembersReader(ADataSet: TcxCustomPivotGridOLAPDataSet;
  AGroupValues: TcxPivotGridOLAPGroupInfos; AField: TcxPivotGridField): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPADOMDDataSetMembersReader;
begin
  AReader := TcxPivotGridOLAPADOMDDataSetMembersReader.Create(ADataSet);
  AReader.Initialize(AGroupValues, AField);
  Result := AReader;
end;

type
  TcxPivotGridOLAPArrayDrillDownDataSourceAccess = class(TcxPivotGridOLAPArrayDrillDownDataSource);

function TcxPivotGridOLAPADOMDProvider.CreateDrillDownDataSet(const ACommandText: string;
  ACrossCell: TcxPivotGridCrossCell; ASummaryIndex: Integer): TObject;
var
  I, ARowIndex: Integer;
  ARow: Variant;
  ARecordset: TdxADORecordset;
  AResult: TcxPivotGridOLAPArrayDrillDownDataSourceAccess.TDataSet absolute Result;
begin
  Result := nil;
  ARecordset := OpenRecordset(ACommandText);
  if ARecordset = nil then
    Exit;
  try
    AResult := TcxPivotGridOLAPArrayDrillDownDataSourceAccess.TDataSet.Create(ARecordset.RecordCount);
    for I := 0 to ARecordset.FieldCount - 1 do
      AResult.AddField(ARecordset.GetFieldName(I));
    ARowIndex := 0;
    ARecordset.MoveFirst;
    while not ARecordset.Eof do
    begin
      ARow := AResult.CreateRow;
      for I := 0 to AResult.FieldCount - 1 do
        ARow[I] := ARecordset.GetFieldValue(I);
      AResult.Row[ARowIndex] := ARow;
      ARecordset.MoveNext;
      Inc(ARowIndex);
    end;
  finally
    ARecordset.Free;
  end;
end;

procedure TcxPivotGridOLAPADOMDProvider.DoConnect;

  procedure _DoConnect(const AConnectionString: string);
  begin
    FCatalog := CreateComObject(StringToGUID('ADOMD.Catalog')) as ICatalog;
    try
      FCatalog.Set_ActiveConnection(AConnectionString);
    except
      FCatalog := nil;
      raise;
    end;
  end;

var
  AOriginalCubeFilePath, AConnectionString: string;
begin
  AConnectionString := ConnectionString;
  if CacheLocalCubes then
  begin
    AOriginalCubeFilePath := ExtractCubeName(ConnectionString);
    if CreateTemporaryCube(AOriginalCubeFilePath) then
      AConnectionString := StringReplace(ConnectionString, AOriginalCubeFilePath, CachedFileName, [rfReplaceAll]);
  end;
  _DoConnect(AConnectionString);
end;

procedure TcxPivotGridOLAPADOMDProvider.DoDisconnect;
begin
  if FCatalog <> nil then
    FCatalog._Set_ActiveConnection(nil);
  FCatalog := nil;
end;

function TcxPivotGridOLAPADOMDProvider.Execute(const ACommandText: string): TcxCustomPivotGridOLAPDataSet;
var
  ACellSet: ICellSet;
begin
  Result := TcxPivotGridOLAPADOMDDataSet.Create;
  ACellSet := CoCellset.Create;
  try
    ACellSet.Open(ACommandText, Catalog.Get_ActiveConnection);
    TcxPivotGridOLAPADOMDDataSet(Result).Initialize(ACellSet);
  except
    ACellSet := nil;
    raise;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.Execute(const AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet;
begin
  Result := Execute(AQuery.GetQuery);
  if Result <> nil then
    TcxPivotGridOLAPADOMDDataSet(Result).FQuery := AQuery;
end;

function TcxPivotGridOLAPADOMDProvider.GetConnected: Boolean;
begin
  Result := FCatalog <> nil;
end;

function TcxPivotGridOLAPADOMDProvider.GetDrillDownDataSourceClass: TcxCustomPivotGridOLAPDrillDownDataSourceClass;
begin
  Result := TcxPivotGridOLAPArrayDrillDownDataSource;
end;

function TcxPivotGridOLAPADOMDProvider.OpenSchema(const ACubeName: string;
  ASchema: TcxPivotGridOLAPSchemaInfo): TcxPivotGridOLAPDBDataSet;
var
  ACubeDef: CubeDef;
begin
  if not Connected then
    Open;
  ACubeDef := GetCubeDefByName(ACubeName);
  Result := nil;
  try
    case ASchema of
      osiLevels:
        Result := DoGetLevels(ACubeDef);
      osiMeasures:
        Result := DoGetMeasures(ACubeDef);
      osiKPIs:
        Result := DoGetKPIs(ACubeDef);
      osiSets:
        Result := DoGetSets(ACubeDef);
      osiHierarchies:
        Result := DoGetHierarchies(ACubeDef);
    else
      PivotGridError(False, 'Not implemented!');
    end;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

procedure TcxPivotGridOLAPADOMDProvider.PopulateCatalogs(ANames: TStrings);
begin
  // do nothing for now
end;

procedure TcxPivotGridOLAPADOMDProvider.PopulateHierarchyAllMemberNames(const ACubeName: string;
  ANames: TDictionary<string, string>);

  procedure AddAllMember(ADimension: Dimension);
  var
    I: Integer;
    S, AUniqueName: string;
  begin
    for I := 0 to ADimension.Hierarchies.Count - 1 do
    begin
      S := GetPropertyStrValue(ADimension.Hierarchies[I].Properties, 'ALL_MEMBER');
      if S <> '' then
      begin
        AUniqueName := ADimension.Hierarchies[I].UniqueName;
        ANames.Add(AUniqueName, S);
      end;
    end;
  end;

var
  I: Integer;
  ACube: CubeDef;
  ADimension: Dimension;
  ADimensions: Dimensions;
begin
  ACube := GetCubeDefByName(ACubeName);
  ADimensions := ACube.Dimensions;
  for I := 0 to ADimensions.Count - 1 do
  begin
    ADimension := ADimensions[I];
    AddAllMember(ADimension);
  end;
end;

function TcxPivotGridOLAPADOMDProvider.AddFieldToDataSet(ADataSet: TDataSet; const AFieldName: string;
  AFieldType: TFieldType): TField;
begin
  ADataSet.FieldDefs.Add(AFieldName, AFieldType);
  Result := ADataSet.FieldDefs.Find(AFieldName).CreateField(ADataSet);
end;

function TcxPivotGridOLAPADOMDProvider.GetCubeDefByName(const ACubeName: string): CubeDef;
var
  I: Integer;
  ACubeDef: CubeDef;
  ACubeDefs: CubeDefs;
begin
  ACubeDefs := Catalog.CubeDefs;
  try
    for I := 0 to ACubeDefs.Count - 1 do
    begin
      ACubeDef := ACubeDefs[I];
      if ACubeDef.Name = ACubeName then
      begin
        Result := ACubeDef;
        Exit;
      end;
    end;
  finally
    ACubeDef := nil;
    ACubeDefs := nil;
  end;
  raise Exception.Create('Invalid cube name');
end;

function TcxPivotGridOLAPADOMDProvider.GetPropertyValue(const AProperties: Properties; const APropertyName: string;
  const ADefaultValue: Variant): Variant;
var
  I: Integer;
  AProperty: Property_;
begin
  Result := ADefaultValue;
  for I := 0 to AProperties.Count - 1 do
  begin
    AProperty := AProperties[I];
    if AProperty.Name = APropertyName then
    begin
      Result := AProperty.Value;
      Break;
    end;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.GetPropertyIntValue(const AProperties: Properties; const APropertyName: string;
  const ADefaultValue: Integer = 0): Integer;
var
  V: Variant;
begin
  Result := ADefaultValue;
  V := GetPropertyValue(AProperties, APropertyName, ADefaultValue);
  if not VarIsNull(V) then
    Result := V;
end;

function TcxPivotGridOLAPADOMDProvider.GetPropertyStrValue(const AProperties: Properties; const APropertyName: string;
  const ADefaultValue: string = ''): string;
var
  V: Variant;
begin
  V := GetPropertyValue(AProperties, APropertyName, ADefaultValue);
  Result := VarToStrDef(V, ADefaultValue);
end;

function TcxPivotGridOLAPADOMDProvider.OpenRecordset(const ACommandText: string): TdxADORecordset;
begin
  Result := TdxADORecordset.Create;
  try
    try
      Result.Open(ACommandText, Catalog.Get_ActiveConnection);
    except
      FreeAndNil(Result);
      raise;
    end;
  except
    on E: EOleSysError do
      if E.ErrorCode = -2147467259 then
      else
        raise;
  else
    raise;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.DoGetKPIs(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;

  procedure InitializeInnerDataSet(ADataSet: TDataSet; const AFieldNames: array of string);
  var
    I: Integer;
  begin
    for I := Low(AFieldNames) to High(AFieldNames) do
      AddFieldToDataSet(ADataSet, AFieldNames[I], ftWideMemo);
    ADataSet.Open;
  end;

const
  FieldNames: array[0..8] of string = ('KPI_CAPTION', 'KPI_NAME', 'KPI_VALUE', 'KPI_STATUS_GRAPHIC',
    'KPI_TREND_GRAPHIC', 'KPI_GOAL', 'KPI_STATUS', 'KPI_TREND','KPI_WEIGHT');
var
  I: Integer;
  ADataSet: TDataSet;
  ARecordset: TdxADORecordset;
  ACommandText, AFieldName: string;
begin
  ACommandText := Format('SELECT * FROM $system.MDSCHEMA_KPIS WHERE CUBE_NAME=%s', [QuotedStr(ACube.Name)]);
  ARecordset := OpenRecordset(ACommandText);
  if ARecordset = nil then
    Exit(nil);
  try
    Result := TcxPivotGridOLAPMemDataDataSet.Create;
    ADataSet := Result.InnerDataSet;
    InitializeInnerDataSet(ADataSet, FieldNames);
    Result.InitializeFields;
    ARecordset.MoveFirst;
    while not ARecordset.Eof do
    begin
      ADataSet.Insert;
      for I := 0 to ARecordset.FieldCount - 1 do
      begin
        AFieldName := ARecordset.GetFieldName(I);
        if IndexStr(AFieldName, FieldNames) >= 0 then
          ADataSet.FieldByName(AFieldName).AsString := VarToStr(ARecordset.GetFieldValue(I));
      end;
      ADataSet.Post;
      ARecordset.MoveNext;
    end;
  finally
    ARecordset.Free;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.DoGetLevels(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
var
  ADimensionUniqueName, AHierarchyUniqueName, AUniqueName, AName, ACardinality, ADBType, ANumber, AType: TField;

  procedure AddLevel(ADataSet: TDataSet; ADimension: Dimension; AHierarchy: Hierarchy; ALevel: Level);
  var
    ALevelProperties: Properties;
  begin
    ADataSet.Append;
    ADimensionUniqueName.AsString := ADimension.UniqueName;
    AHierarchyUniqueName.AsString := AHierarchy.UniqueName;
    AUniqueName.AsString := ALevel.UniqueName;
    AName.AsString := ALevel.Caption;
    ALevelProperties := ALevel.Properties;
    ANumber.AsInteger := GetPropertyIntValue(ALevelProperties, 'LEVEL_NUMBER');
    ACardinality.AsInteger := GetPropertyIntValue(ALevelProperties, 'LEVEL_CARDINALITY');
    ADBType.AsInteger := GetPropertyIntValue(ALevelProperties, 'LEVEL_DBTYPE');
    AType.AsInteger := GetPropertyIntValue(ALevelProperties, 'LEVEL_TYPE');
    ADataSet.Post;
  end;

  procedure InitializeInnerDataSet(ADataSet: TDataSet);
  begin
    ADimensionUniqueName := AddFieldToDataSet(ADataSet, 'DIMENSION_UNIQUE_NAME', ftWideMemo);
    AHierarchyUniqueName := AddFieldToDataSet(ADataSet, 'HIERARCHY_UNIQUE_NAME', ftWideMemo);
    AUniqueName := AddFieldToDataSet(ADataSet, 'LEVEL_UNIQUE_NAME', ftWideMemo);
    AName := AddFieldToDataSet(ADataSet, 'LEVEL_NAME', ftWideMemo);
    ANumber := AddFieldToDataSet(ADataSet, 'LEVEL_NUMBER', ftInteger);
    ACardinality := AddFieldToDataSet(ADataSet, 'LEVEL_CARDINALITY', ftInteger);
    ADBType := AddFieldToDataSet(ADataSet, 'LEVEL_DBTYPE', ftInteger);
    AType := AddFieldToDataSet(ADataSet, 'LEVEL_TYPE', ftInteger);
    ADataSet.Open;
  end;

var
  I, J, N: Integer;
  ALevels: Levels;
  AHierarchy: Hierarchy;
  AHierarchies: Hierarchies;
  ADimension: Dimension;
  ADimensions: Dimensions;
begin
  Result := TcxPivotGridOLAPMemDataDataSet.Create;
  try
    InitializeInnerDataSet(Result.InnerDataSet);
    Result.InitializeFields;
    ADimensions := ACube.Dimensions;
    for I := 0 to ADimensions.Count - 1  do
    begin
      ADimension := ADimensions[I];
      AHierarchies := ADimension.Hierarchies;
      for J := 0 to AHierarchies.Count -1 do
      begin
        AHierarchy := AHierarchies[J];
        ALevels := AHierarchy.Levels;
        for N := 0 to ALevels.Count - 1 do
          AddLevel(Result.InnerDataSet, ADimension, AHierarchy, ALevels[N]);
      end;
    end;
    Result.First;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.DoGetMeasures(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
var
  AGroupName, ACaption, AUniqueName, ADisplayFolder, ADefaultFormatString: TField;

  procedure AddMeasures(ADataSet: TDataSet; ADimension: Dimension; AMember: Member);
  var
    AMemberProperties: Properties;
  begin
    ADataSet.Append;
    ACaption.AsString := AMember.Caption;
    AUniqueName.AsString := AMember.UniqueName;
    AMemberProperties := AMember.Properties;
    AGroupName.AsString := GetPropertyStrValue(AMemberProperties, 'MEASUREGROUP_NAME');
    ADisplayFolder.AsString := GetPropertyStrValue(AMemberProperties, 'MEASURE_DISPLAY_FOLDER');
    ADefaultFormatString.AsString := GetPropertyStrValue(AMemberProperties, 'DEFAULT_FORMAT_STRING');
    ADataSet.Post;
  end;

  procedure InitializeInnerDataSet(ADataSet: TDataSet);
  begin
    ACaption := AddFieldToDataSet(ADataSet, 'MEASURE_CAPTION', ftWideMemo);
    AUniqueName := AddFieldToDataSet(ADataSet, 'MEASURE_UNIQUE_NAME', ftWideMemo);
    AGroupName := AddFieldToDataSet(ADataSet, 'MEASUREGROUP_NAME', ftWideMemo);
    ADisplayFolder := AddFieldToDataSet(ADataSet, 'MEASURE_DISPLAY_FOLDER', ftWideMemo);
    ADefaultFormatString := AddFieldToDataSet(ADataSet, 'DEFAULT_FORMAT_STRING', ftWideMemo);
    ADataSet.Open;
  end;

var
  I, J: Integer;
  AMembers: Members;
  ADimension: Dimension;
  ADimensions: Dimensions;
begin
  Result := TcxPivotGridOLAPMemDataDataSet.Create;
  try
    InitializeInnerDataSet(Result.InnerDataSet);
    Result.InitializeFields;
    ADimensions := ACube.Dimensions;
    for I := 0 to ADimensions.Count - 1 do
    begin
      ADimension := ADimensions[I];
      if SameText(ADimension.UniqueName, '[' + scxMeasures + ']') then
      begin
        AMembers := ADimension.Hierarchies[0].Levels[0].Members;
        for J := 0 to AMembers.Count - 1 do
          AddMeasures(Result.InnerDataSet, ADimension, AMembers[J]);
      end;
    end;
    Result.First;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.DoGetHierarchies(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;
var
  ADisplayFolder, ACaption, ADimensionUniqueName, AUniqueName, AStructure, ACardinality: TField;

  procedure AddHierarchy(ADataSet: TDataSet; ADimension: Dimension; AHierarchy: Hierarchy);
  var
    AHierarchyProperties: Properties;
  begin
    ADataSet.Append;
    AHierarchyProperties := AHierarchy.Properties;
    ADisplayFolder.AsString := GetPropertyStrValue(AHierarchyProperties, 'HIERARCHY_DISPLAY_FOLDER');
    ACaption.AsString := AHierarchy.Name;
    ADimensionUniqueName.AsString := ADimension.UniqueName;
    AUniqueName.AsString := AHierarchy.UniqueName;
    AStructure.AsInteger := GetPropertyIntValue(AHierarchyProperties, 'STRUCTURE');
    ACardinality.AsInteger := GetPropertyIntValue(AHierarchyProperties, 'HIERARCHY_CARDINALITY');
    ADataSet.Post;
  end;

  procedure InitializeInnerDataSet(ADataSet: TDataSet);
  begin
    ADisplayFolder := AddFieldToDataSet(ADataSet, 'HIERARCHY_DISPLAY_FOLDER', ftWideMemo);
    ACaption := AddFieldToDataSet(ADataSet, 'HIERARCHY_CAPTION', ftWideMemo);
    ADimensionUniqueName := AddFieldToDataSet(ADataSet, 'DIMENSION_UNIQUE_NAME', ftWideMemo);
    AUniqueName := AddFieldToDataSet(ADataSet, 'HIERARCHY_UNIQUE_NAME', ftWideMemo);
    AStructure := AddFieldToDataSet(ADataSet, 'STRUCTURE', ftInteger);
    ACardinality := AddFieldToDataSet(ADataSet, 'HIERARCHY_CARDINALITY', ftInteger);
    ADataSet.Open;
  end;

var
  I, J: Integer;
  AHierarchies: Hierarchies;
  ADimension: Dimension;
  ADimensions: Dimensions;
begin
  Result := TcxPivotGridOLAPMemDataDataSet.Create;
  try
    InitializeInnerDataSet(Result.InnerDataSet);
    Result.InitializeFields;

    ADimensions := ACube.Dimensions;
    for I := 0 to ADimensions.Count - 1  do
    begin
      ADimension := ADimensions[I];
      AHierarchies := ADimension.Hierarchies;
      for J := 0 to AHierarchies.Count - 1 do
        AddHierarchy(Result.InnerDataSet, ADimension, AHierarchies[J]);
    end;
    Result.First;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

function TcxPivotGridOLAPADOMDProvider.DoGetSets(const ACube: CubeDef): TcxPivotGridOLAPMemDataDataSet;

  procedure InitializeInnerDataSet(ADataSet: TDataSet);
  begin
    AddFieldToDataSet(ADataSet, 'DIMENSIONS', ftWideMemo);
    AddFieldToDataSet(ADataSet, 'SET_CAPTION', ftWideMemo);
    AddFieldToDataSet(ADataSet, 'SET_DISPLAY_FOLDER', ftWideMemo);
    AddFieldToDataSet(ADataSet, 'SET_NAME', ftWideMemo);
    ADataSet.Open;
  end;

begin
  Result := TcxPivotGridOLAPMemDataDataSet.Create;
  try
    InitializeInnerDataSet(Result.InnerDataSet);
    Result.InitializeFields;
  except
    FreeAndNil(Result);
    raise;
  end;
end;

{ TcxCustomPivotGridOLAPADOMDDataSetReader }

constructor TcxCustomPivotGridOLAPADOMDDataSetReader.Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
begin
  inherited Create(ADataSet);
  Assert(ADataSet is TcxPivotGridOLAPADOMDDataSet);
end;

class function TcxCustomPivotGridOLAPADOMDDataSetReader.NativeValueToValue(const ANativeValue: Variant): Variant;
begin
  if VarIsEmpty(ANativeValue) then
    Result := Null
  else
    Result := ANativeValue;
end;

function TcxCustomPivotGridOLAPADOMDDataSetReader.GetDataSet: TcxPivotGridOLAPADOMDDataSet;
begin
  Result := TcxPivotGridOLAPADOMDDataSet(inherited DataSet);
end;

{ TcxPivotGridOLAPADOMDDataSetCrossCellsReader }

constructor TcxPivotGridOLAPADOMDDataSetCrossCellsReader.Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
begin
  inherited Create(ADataSet);
  Assert(DataSet.Query <> nil);
end;

procedure TcxPivotGridOLAPADOMDDataSetCrossCellsReader.Execute;

  function SameArray(const AArray1, AArray2: TStringDynArray): Boolean;
  var
    I, ALength: Integer;
  begin
    ALength := Length(AArray1);
    Result := ALength = Length(AArray2);
    if Result then
      for I := 0 to ALength - 1 do
        if AArray1[I] <> AArray2[I] then
          Exit(False);
  end;

  procedure SkipEmptyRowMembers(var AIndex: Integer; const ACurrentRowMembers: TStringDynArray;
    AMembers: TcxMDXQueryAxisMembers);
  var
    ACoordinate: TStringDynArray;
  begin
    repeat
      ACoordinate := AMembers.GetMemberCoordinate(AIndex);
      if SameArray(ACoordinate, ACurrentRowMembers) then
        Exit;
      Inc(AIndex);
    until AMembers.MemberCount < AIndex;
  end;

  procedure SkipEmptyColumnMembers(var AIndex: Integer; const ACurrentColumnMembers: TStringDynArray;
    AColumnMembers: TcxMDXQueryAxisMembers);
  var
    I: Integer;
    ACoordinate: TStringDynArray;
  begin
    for I := 0 to AColumnMembers.MemberCount - 1 do
    begin
      ACoordinate := AColumnMembers.GetMemberCoordinate(AIndex mod AColumnMembers.MemberCount);
      if SameArray(ACoordinate, ACurrentColumnMembers) then
        Break;
      Inc(AIndex);
    end;
  end;

  procedure SkipEmptyMeasures(var AIndex: Integer; const ACurrentMeasure: string;
    AMembers: TcxMDXQueryAxisMembers);
  var
    I: Integer;
  begin
    for I := 0 to AMembers.MeasureCount - 1 do
    begin
      if AMembers.Measures[AIndex mod AMembers.MeasureCount].UniqueName = ACurrentMeasure then
        Break;
      Inc(AIndex);
    end;
  end;

var
  ANativeValue: Variant;
  ACell: TcxPivotGridCrossCell;
  I, ARowIndex, AColumnIndex, AFieldIndex: Integer;
  AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
  AOLAPCoordinate: TcxPivotGridOLAPCellCoordinate;
  AEnumerator: TcxPivotGridOLAPADOMDCellSetEnumerator;
  AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;
begin
  if not DataSet.Query.TryGetMembers(TcxMDXQuery.COLUMN_AXIS, AColumnMembers) or
    not DataSet.Query.TryGetMembers(TcxMDXQuery.ROW_AXIS, ARowMembers) then
    raise EcxPivotGrid.Create('Not implemented.');

  AEnumerator := TcxPivotGridOLAPADOMDCellSetEnumerator.Create(DataSet);
  try
    ARowIndex := 0;
    while AEnumerator.MoveNextRow do
    begin
      I := 0;
      AEnumerator.FirstColumn;
      AOLAPCoordinate := AEnumerator.GetCoordinate;
      SkipEmptyRowMembers(ARowIndex, AOLAPCoordinate.Row, ARowMembers);
      ARowGroup := ARowMembers.Members[ARowIndex].Last;
      repeat
        AOLAPCoordinate := AEnumerator.GetCoordinate;
        SkipEmptyColumnMembers(I, AOLAPCoordinate.Column, AColumnMembers);
        AColumnIndex := I mod AColumnMembers.MemberCount;
        AColumnGroup := AColumnMembers.Members[AColumnIndex].Last;

        SkipEmptyMeasures(I, AOLAPCoordinate.Measure, AColumnMembers);
        AFieldIndex := I mod AColumnMembers.MeasureCount;

        ACell := AColumnGroup.GetCellByCrossItem(ARowGroup);
        ANativeValue := AEnumerator.CurrentCellValue;
        SetCrossCellValue(ACell, AFieldIndex,
          ANativeValue, NativeValueToValue(ANativeValue));
      until not AEnumerator.MoveNextColumn;
      Inc(ARowIndex);
    end;
  finally
    AEnumerator.Free;
  end;
end;

procedure TcxPivotGridOLAPADOMDDataSetCrossCellsReader.Initialize(AOwnedGroup, ARowGroups,
  AColumnGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields);
begin
  FOwnedGroup := AOwnedGroup;
  FRowGroups := ARowGroups;
  FColumnGroups := AColumnGroups;
  FSummaryFields := ASummaryFields;
end;

{ TcxPivotGridOLAPADOMDDataSetMembersReader }

procedure TcxPivotGridOLAPADOMDDataSetMembersReader.Execute;
var
  AEnumerator: TcxPivotGridOLAPADOMDCellSetEnumerator;

  function DataIsNull: Boolean;
  var
    AValue: OleVariant;
    AMembersUniqueName: string;
  begin
    Result := True;
    AEnumerator.FirstColumn;
    while AEnumerator.MoveNextColumn do
    begin
      AMembersUniqueName := AEnumerator.ColumnEnumerator.Current.Members[0].UniqueName;
      if (AMembersUniqueName = '[Measures].[MemberName]') or
        (AMembersUniqueName = '[Measures].[MemberUniqueName]') then
        Continue;
      AValue := AEnumerator.CurrentCellValue;
      if not VarIsNull(AValue) then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  AMember: Member;
  ADataIsNull: Boolean;
begin
  AEnumerator := TcxPivotGridOLAPADOMDCellSetEnumerator.Create(DataSet);
  try
    if AEnumerator.RowEnumerator = nil then
      raise Exception.Create('Not implemented!');
    while AEnumerator.MoveNextRow do
    begin
      AMember := AEnumerator.RowEnumerator.Current.Members[0];
      ADataIsNull := DataIsNull;
      GroupValues.AddItem(AMember.UniqueName, AMember.Caption, ADataIsNull);
    end;
  finally
    AMember := nil;
    AEnumerator.Free;
  end;
end;

procedure TcxPivotGridOLAPADOMDDataSetMembersReader.Initialize(AGroupValues: TcxPivotGridOLAPGroupInfos;
  AField: TcxPivotGridField);
begin
  FGroupValues := AGroupValues;
  FField := AField;
end;

{ TcxPivotGridOLAPADOMDDataSetGrandTotalReader }

procedure TcxPivotGridOLAPADOMDDataSetGrandTotalReader.Execute;
var
  ANativeValue: Variant;
  ACellMatrix: TcxPivotGridOLAPCellMatrix;
  AOLAPCoordinate: TcxPivotGridOLAPCellCoordinate;
  ACellValueInfo: TcxPivotGridOLAPCellMatrix.TValue;
  AEnumerator: TcxPivotGridOLAPADOMDCellSetEnumerator;
begin
  ACellMatrix := TcxPivotGridOLAPCellMatrix.Create(RowGroups, ColumnGroups, SummaryFields);
  try
    AEnumerator := TcxPivotGridOLAPADOMDCellSetEnumerator.Create(DataSet);
    try
      AEnumerator.FirstRow;
      AEnumerator.FirstColumn;
      repeat
        AOLAPCoordinate := AEnumerator.GetCoordinate;
        AOLAPCoordinate.ClearRow;
        if ACellMatrix.TryGetCellValueInfo(AOLAPCoordinate, ACellValueInfo) then
        begin
          ANativeValue := AEnumerator.CurrentCellValue;
          SetCrossCellValue(ACellValueInfo.Cell, ACellValueInfo.FieldIndex,
            ANativeValue, NativeValueToValue(ANativeValue));
        end;
      until not AEnumerator.MoveNextColumn;
    finally
      AEnumerator.Free;
    end;
  finally
    ACellMatrix.Free;
  end;
end;

procedure TcxPivotGridOLAPADOMDDataSetGrandTotalReader.Initialize(AColumnGroups, ARowGroups: TcxPivotGridGroupItem;
  ASummaryFields: TcxPivotGridFields; AnExpandedMembers: TList);
begin
  FColumnGroups := AColumnGroups;
  FRowGroups := ARowGroups;
  FSummaryFields := ASummaryFields;
  FAnExpandedMembers := AnExpandedMembers;
end;

{ TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader }

procedure TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader.Execute;
var
  I, AColumnCount: Integer;
  ANativeValue: OleVariant;
  AIsValuesInitialized: Boolean;
  AValues, ANativeValues: Variant;
  AEnumerator: TcxPivotGridOLAPADOMDCellSetEnumerator;
begin
  AIsValuesInitialized := False;
  AEnumerator := TcxPivotGridOLAPADOMDCellSetEnumerator.Create(DataSet);
  try
    AColumnCount := AEnumerator.ColumnEnumerator.Count;
    AValues := VarArrayCreate([0, AColumnCount - 1], varVariant);
    ANativeValues := VarArrayCreate([0, AColumnCount - 1], varVariant);
    if AEnumerator.RowEnumerator <> nil then
      raise Exception.Create('Not implemented!');

    while AEnumerator.MoveNextColumn do
    begin
      I := AEnumerator.ColumnEnumerator.Index;
      ANativeValue := AEnumerator.CurrentCellValue;
      ANativeValues[I] := ANativeValue;

      AIsValuesInitialized := False;
      AValues[I] := NativeValueToValue(ANativeValue);
      if not VarIsEmpty(ANativeValue) then
        AIsValuesInitialized := True;
    end;
    if AIsValuesInitialized then
      SetCrossCellValues(RowGroup.GetCellByCrossItem(ColumnGroup), AValues, ANativeValues)
    else
      SetCrossCellValues(RowGroup.GetCellByCrossItem(ColumnGroup), Null, Null);
  finally
    AEnumerator.Free;
  end;
end;

procedure TcxPivotGridOLAPADOMDDataSetMainGrandTotalReader.Initialize(AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
  ADataFields: TcxPivotGridFields);
begin
  FColumnGroup := AColumnGroup;
  FRowGroup := ARowGroup;
  FDataFields := ADataFields;
end;

{ TcxPivotGridOLAPCellMatrix }

constructor TcxPivotGridOLAPCellMatrix.Create(ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
  ASummaryFields: TcxPivotGridFields);
begin
  inherited Create;
  FRowGroups := ARowGroups;
  FColumnGroups := AColumnGroups;
  FSummaryFields := ASummaryFields;
end;

function TcxPivotGridOLAPCellMatrix.TryGetCellValueInfo(const AKey: TcxPivotGridOLAPCellCoordinate;
  out AValue: TcxPivotGridOLAPCellMatrix.TValue): Boolean;

  function NeedSwitchGroups: Boolean;
  begin
    if Length(AKey.Column) > 0 then
      Result := GetGroupByUniqueName(ColumnGroups, AKey.Column[0]) = nil
    else
      if Length(AKey.Row) > 0 then
        Result := GetGroupByUniqueName(RowGroups, AKey.Row[0]) = nil
      else
        Result := False;
  end;

var
  AFieldIndex: Integer;
  ACell: TcxPivotGridCrossCell;
  AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
begin
  Result := False;
  AColumnGroup := ColumnGroups;
  ARowGroup := RowGroups;
  if NeedSwitchGroups then
  begin
    AColumnGroup := RowGroups;
    ARowGroup := ColumnGroups;
  end;

  AColumnGroup := FindGroupByUniqueNames(AColumnGroup, AKey.Column);
  if AColumnGroup = nil then
    Exit;
  ARowGroup := FindGroupByUniqueNames(ARowGroup, AKey.Row);
  if ARowGroup = nil then
    Exit;
  AFieldIndex := GetSummaryFieldIndex(AKey.Measure);
  Result := AFieldIndex >= 0;
  if Result then
  begin
    ACell := AColumnGroup.GetCellByCrossItem(ARowGroup);
    AValue := TcxPivotGridOLAPCellMatrix.TValue.Create(ACell, AFieldIndex);
  end;
end;

function TcxPivotGridOLAPCellMatrix.FindGroupByUniqueNames(AParentGroup: TcxPivotGridGroupItem;
  const ANames: TStringDynArray): TcxPivotGridGroupItem;
var
  I, ALength: Integer;
begin
  Result := nil;
  ALength := Length(ANames);
  if ALength = 0 then
    Exit(AParentGroup);
  for I := 0 to ALength - 1 do
  begin
    Result := GetGroupByUniqueName(AParentGroup, ANames[I]);
    if AParentGroup = Result then
      Exit;
    if Result = nil then
      Exit;
    AParentGroup := Result;
  end;
end;

type
  TcxPivotGridFieldAccess = class(TcxPivotGridField);

function TcxPivotGridOLAPCellMatrix.GetGroupByUniqueName(AGroups: TcxPivotGridGroupItem;
  const AUniqueName: string): TcxPivotGridGroupItem;

  function GetTotalUniqueNameForQuery(Field: TcxPivotGridField): string;
  var
    AField: TcxPivotGridOLAPField absolute Field;
  begin
    Assert(Field is TcxPivotGridOLAPField);
    Result := AField.HierarchyUniqueName + '.[All]';
  end;

  function GetNextField(AField: TcxPivotGridField): TcxPivotGridField;
  var
    AFields: TcxPivotGridFields;
  begin
    Result := nil;
    if AField.Area = faRow then
      AFields := TcxPivotGridFieldAccess(AField).DataBuilder.RowFields
    else
      AFields := TcxPivotGridFieldAccess(AField).DataBuilder.ColumnFields;
    if AFields.Count - 1 > AField.AreaIndex then
      Result := AFields[AField.AreaIndex + 1];
  end;

var
  I: Integer;
  AField, AField1, AField2: TcxPivotGridField;
begin
  Result := nil;
  for I := 0 to AGroups.ItemCount - 1 do
    if SameStr(AGroups.Items[I].UniqueName, AUniqueName) then
    begin
      Result := AGroups.Items[I];
      Break;
    end;
  if (Result = nil) and AGroups.HasChildren and (AGroups.Field <> nil) then
  begin
    AField := GetNextField(AGroups.Field);
    if (AField <> nil) and SameStr(GetTotalUniqueNameForQuery(AField), AUniqueName) then
      Result := AGroups;
  end;
  if (Result = nil) and (AGroups.ItemCount > 0) and (AGroups.Items[0].ItemCount > 0) then
  begin
    AField1 := AGroups.Items[0].Field;
    AField2 := AGroups.Items[0].Items[0].Field;
    if (AField1 <> nil) and (AField2 <> nil) and TcxCustomOLAPDataQueryBuilder.SameHierarchy(AField1, AField2) then
      for I := 0 to AGroups.ItemCount - 1 do
      begin
        Result := GetGroupByUniqueName(AGroups.Items[I], AUniqueName);
        if Result <> nil then
          Break;
      end;
  end;
end;

function TcxPivotGridOLAPCellMatrix.GetSummaryFieldIndex(const AUniqueName: string): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to SummaryFields.Count - 1 do
    if SameStr(SummaryFields[I].UniqueName, AUniqueName) then
    begin
      Result := I;
      Break;
    end;
end;

{ TcxPivotGridOLAPCellMatrix.TValue }

constructor TcxPivotGridOLAPCellMatrix.TValue.Create(ACell: TcxPivotGridCrossCell; AFieldIndex: Integer);
begin
  FCell := ACell;
  FFieldIndex := AFieldIndex;
end;

initialization
  TcxPivotGridOLAPADOMDProvider.Register;

finalization
  TcxPivotGridOLAPADOMDProvider.Unregister;

end.


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

unit cxPivotGridOLAPOLEDBProvider;

{$I cxVer.inc}

interface

uses
  Windows, Contnrs, Types, Classes, SysUtils, Forms, Math, Variants, StrUtils, DB, ADODB,
  Generics.Defaults, Generics.Collections, dxCore, cxClasses, cxPivotGridOLAPDataSource,
  cxCustomPivotGrid, cxPivotGridOLAPQueryBuilder, cxVariants;

type

  { TcxPivotGridOLAPOLEDBDataSet }

  TcxPivotGridOLAPOLEDBDataSet = class(TcxPivotGridOLAPDBDataSet)
  private
    function GetInnerDataSet: TADODataSet;
  protected
    FQuery: TcxMDXQuery;
    function CreateDataSet: TDataSet; override;

    property InnerDataSet: TADODataSet read GetInnerDataSet;
    property Query: TcxMDXQuery read FQuery;
  end;

  { TcxPivotGridOLAPOLEDBProvider }

  TcxPivotGridOLAPOLEDBProvider = class(TcxCustomPivotGridOLAPProvider)
  private const
    SCHEMA_CATALOGS      = '{C8B52211-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_CUBES         = '{C8B522D8-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_DIMENSIONS    = '{C8B522D9-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_LEVELS        = '{C8B522DB-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_MEASURES      = '{C8B522DC-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_KPI           = '{2AE44109-ED3D-4842-B16F-B694D1CB0E3F}';
    SCHEMA_SETS          = '{A07CCD0B-8148-11D0-87BB-00C04FC33942}';
    SCHEMA_HIERARCHIES   = '{C8B522DA-5CF3-11CE-ADE5-00AA0044773D}';
    SCHEMA_MEASUREGROUPS = '{E1625EBF-FA96-42fd-BEA6-DB90ADAFD96B}';
    SCHEMA_MEMBERS       = '{C8B522DE-5CF3-11CE-ADE5-00AA0044773D}';
  strict private
    FConnection: TADOConnection;
    FVersion: Integer;
  protected
    function CreateDataSet: TcxPivotGridOLAPOLEDBDataSet; virtual;
    function CreateDrillDownDataSet(const ACommandText: string; ACrossCell: TcxPivotGridCrossCell;
      ASummaryIndex: Integer): TObject; override;
    procedure DoConnect; override;
    procedure DoDisconnect; override;
    function DoOpenSchema(const ASchemaType: string; const ARestriction: OleVariant): TcxPivotGridOLAPOLEDBDataSet;
    function Execute(const ACommandText: string): TcxCustomPivotGridOLAPDataSet; override;
    function Execute(const AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet; override;
    procedure ExtractProviderVersion(const AVersion: string);
    function IsLocalCube: Boolean;
    function IsOLAPVersion8: Boolean; override;
    function GetConnected: Boolean; override;
    function GetDrillDownDataSourceClass: TcxCustomPivotGridOLAPDrillDownDataSourceClass; override;
    function GetProviderVersion: string;

    procedure PopulateCatalogs(ANames: TStrings); override;
    procedure PopulateHierarchyAllMemberNames(const ACubeName: string; ANames: TDictionary<string, string>); override;
    function OpenSchema(const ACubeName: string; ASchema: TcxPivotGridOLAPSchemaInfo): TcxPivotGridOLAPDBDataSet; override;

    function CreateQueryBuilder: TcxCustomOLAPDataQueryBuilder; override;
    function CreateCrossCellsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateGrandTotalsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroups, ARowGroups: TcxPivotGridGroupItem;
      ASummaryFields: TcxPivotGridFields; AnExpandedMembers: TList): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateMainGrandTotalReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
      ADataFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader; override;
    function CreateMembersReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AGroupValues: TcxPivotGridOLAPGroupInfos;
      AField: TcxPivotGridField): TcxCustomPivotGridOLAPDataSetReader; override;

    property Version: Integer read FVersion;
  public
    constructor Create(ADataSource: TcxPivotGridOLAPDataSource); override;
    destructor Destroy; override;
    class function GetDisplayName: string; override;
    procedure PopulateCubeNames(ANames: TStrings); override;

    property Connection: TADOConnection read FConnection;
  published
    property CacheLocalCubes;
    property ConnectionString;
  end;

  { TcxCustomPivotGridOLAPOLEDBDataSetReader }

  TcxCustomPivotGridOLAPOLEDBDataSetReader = class(TcxCustomPivotGridOLAPDataSetReader)
  protected const
    OLAPMemberNameColumnName = '[Measures].[MemberName]';
    OLAPMemberUniqueNameColumnName = '[Measures].[MemberUniqueName]';
  private
    function GetDataSet: TcxPivotGridOLAPOLEDBDataSet;
  protected
    property DataSet: TcxPivotGridOLAPOLEDBDataSet read GetDataSet;
  public
    constructor Create(ADataSet: TcxCustomPivotGridOLAPDataSet);

    class procedure ProcessDataFields(DataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
      ADataFields: TcxPivotGridFields; AFieldIndex: Integer; const AFieldDimension: string);
    class procedure ProcessDataFieldsEx(DataSet: TcxCustomPivotGridOLAPDataSet;
      AColumnGroup, ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields);
  end;

  { TcxPivotGridOLAPOLEDBDataSetCrossCellsReader }

  TcxPivotGridOLAPOLEDBDataSetCrossCellsReader = class(TcxCustomPivotGridOLAPOLEDBDataSetReader)
  strict private
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
    procedure Execute; override;
  end;

  { TcxPivotGridOLAPOLEDBDataSetMembersReader }

  TcxPivotGridOLAPOLEDBDataSetMembersReader = class(TcxCustomPivotGridOLAPOLEDBDataSetReader)
  strict private
    FGroupValues: TcxPivotGridOLAPGroupInfos;
    FField: TcxPivotGridField;
    procedure AddGroupItem(AIndex: Integer);
  protected
    procedure Initialize(AGroupValues: TcxPivotGridOLAPGroupInfos; AField: TcxPivotGridField);

    property GroupValues: TcxPivotGridOLAPGroupInfos read FGroupValues;
    property Field: TcxPivotGridField read FField;
  public
    procedure Execute; override;
  end;

  { TcxPivotGridOLAP8OLEDBDataSetMembersReader }

  TcxPivotGridOLAP8OLEDBDataSetMembersReader = class(TcxPivotGridOLAPOLEDBDataSetMembersReader)
  strict private
    function ExtractMemberName(const AUniqueName: string): string;
    procedure AddGroupItem(AIndex: Integer);
  public
    procedure Execute; override;
  end;

  { TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader }

  TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader = class(TcxCustomPivotGridOLAPOLEDBDataSetReader)
  strict private
    FColumnGroups: TcxPivotGridGroupItem;
    FRowGroups: TcxPivotGridGroupItem;
    FSummaryFields: TcxPivotGridFields;
    FAnExpandedMembers: TList;
    function GetDimensionField: TcxPivotGridOLAPDBDataSetField;
  protected
    procedure Initialize(AColumnGroups, ARowGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields;
      AnExpandedMembers: TList);
    function GetMeasureName: string; virtual;

    property ColumnGroups: TcxPivotGridGroupItem read FColumnGroups;
    property RowGroups: TcxPivotGridGroupItem read FRowGroups;
    property SummaryFields: TcxPivotGridFields read FSummaryFields;
    property AnExpandedMembers: TList read FAnExpandedMembers;
  public
    procedure Execute; override;
  end;

  { TcxPivotGridOLAP8OLEDBDataSetGrandTotalsReader }

  TcxPivotGridOLAP8OLEDBDataSetGrandTotalsReader = class(TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader)
  protected
    function GetMeasureName: string; override;
  end;

  { TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader }

  TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader = class(TcxCustomPivotGridOLAPOLEDBDataSetReader)
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

implementation

uses
  cxPivotGridStrs, RTLConsts;

{ TcxPivotGridOLAPOLEDBDataSet }

function TcxPivotGridOLAPOLEDBDataSet.CreateDataSet: TDataSet;
begin
  Result := TADODataSet.Create(nil);
end;

function TcxPivotGridOLAPOLEDBDataSet.GetInnerDataSet: TADODataSet;
begin
  Result := TADODataSet(inherited InnerDataSet);
end;

{ TcxPivotGridOLAPOLEDBProvider }

constructor TcxPivotGridOLAPOLEDBProvider.Create(ADataSource: TcxPivotGridOLAPDataSource);
begin
  inherited Create(ADataSource);
  FConnection := TADOConnection.Create(nil);
  Connection.LoginPrompt := False;
  Connection.KeepConnection := False;
  Connection.CommandTimeout := 1000000;
  CacheLocalCubes := True;
end;

destructor TcxPivotGridOLAPOLEDBProvider.Destroy;
begin
  FConnection.Free;
  inherited Destroy;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateCrossCellsReader(ADataSet: TcxCustomPivotGridOLAPDataSet;
  AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPOLEDBDataSetCrossCellsReader;
begin
  AReader := TcxPivotGridOLAPOLEDBDataSetCrossCellsReader.Create(ADataSet);
  AReader.Initialize(AOwnedGroup, ARowGroups, AColumnGroups, ASummaryFields);
  Result := AReader;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateGrandTotalsReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroups,
  ARowGroups: TcxPivotGridGroupItem; ASummaryFields: TcxPivotGridFields; AnExpandedMembers: TList): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader;
begin
  if IsOLAPVersion8 then
    AReader := TcxPivotGridOLAP8OLEDBDataSetGrandTotalsReader.Create(ADataSet)
  else
    AReader := TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader.Create(ADataSet);
  AReader.Initialize(AColumnGroups, ARowGroups, ASummaryFields, AnExpandedMembers);
  Result := AReader;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateMainGrandTotalReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AColumnGroup,
  ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader;
begin
  AReader := TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader.Create(ADataSet);
  AReader.Initialize(AColumnGroup, ARowGroup, ADataFields);
  Result := AReader;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateMembersReader(ADataSet: TcxCustomPivotGridOLAPDataSet; AGroupValues: TcxPivotGridOLAPGroupInfos;
  AField: TcxPivotGridField): TcxCustomPivotGridOLAPDataSetReader;
var
  AReader: TcxPivotGridOLAPOLEDBDataSetMembersReader;
begin
  if IsOLAPVersion8 then
    AReader := TcxPivotGridOLAP8OLEDBDataSetMembersReader.Create(ADataSet)
  else
    AReader := TcxPivotGridOLAPOLEDBDataSetMembersReader.Create(ADataSet);
  AReader.Initialize(AGroupValues, AField);
  Result := AReader;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateQueryBuilder: TcxCustomOLAPDataQueryBuilder;
begin
  Result := TcxOLEDBOLAPDataQueryBuilder.Create(Version);
end;

function TcxPivotGridOLAPOLEDBProvider.GetConnected: Boolean;
begin
  Result := Connection.Connected;
end;

function TcxPivotGridOLAPOLEDBProvider.GetDrillDownDataSourceClass: TcxCustomPivotGridOLAPDrillDownDataSourceClass;
begin
  Result :=  TcxPivotGridOLAPDrillDownDataSource;
end;

class function TcxPivotGridOLAPOLEDBProvider.GetDisplayName: string;
begin
  Result := 'OLE DB';
end;

procedure TcxPivotGridOLAPOLEDBProvider.PopulateCubeNames(ANames: TStrings);
var
  I: Integer;
  ACubeNameField: TcxPivotGridOLAPDBDataSetField;
  ADataSet: TcxPivotGridOLAPOLEDBDataSet;
begin
  ANames.Clear;
  if not Connected then
    Open;
  ADataSet := DoOpenSchema(SCHEMA_CUBES, EmptyParam);
  try
    if ADataSet = nil then
      Exit;
    ACubeNameField := ADataSet.FieldByName('CUBE_NAME');
    for I := 0 to ADataSet.RecordCount - 1 do
    begin
      ANames.Add(ACubeNameField.AsString);
      ADataSet.Next;
    end;
  finally
    ADataSet.Free;
  end;
end;

procedure TcxPivotGridOLAPOLEDBProvider.ExtractProviderVersion(const AVersion: string);
var
  ACode: Integer;
begin
  FVersion := 0;
  if Pos('.', AVersion) > 0 then
  begin
    Val(Copy(AVersion, 1, Pos('.', AVersion) - 1), FVersion, ACode);
    if ACode <> 0 then
      FVersion := 0;
  end;
  if FVersion > 10 then
    raise EcxPivotGrid.CreateFmt(scxUnsupportedProviderVersion, [FVersion]);
end;

function TcxPivotGridOLAPOLEDBProvider.GetProviderVersion: string;
begin
  Result := VarToStr(Connection.Properties.Item['Provider Version'].Value);
end;

function TcxPivotGridOLAPOLEDBProvider.IsLocalCube: Boolean;
begin
  Result := Pos('.CUB', UpperCase(ConnectionString)) <> 0
end;

function TcxPivotGridOLAPOLEDBProvider.IsOLAPVersion8: Boolean;
begin
  Result := Version <= 8;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateDataSet: TcxPivotGridOLAPOLEDBDataSet;
begin
  Result := TcxPivotGridOLAPOLEDBDataSet.Create;
end;

function TcxPivotGridOLAPOLEDBProvider.CreateDrillDownDataSet(const ACommandText: string; ACrossCell: TcxPivotGridCrossCell;
  ASummaryIndex: Integer): TObject;
begin
  Result := TcxPivotGridOLAPDBDataSet(Execute(ACommandText));
end;

procedure TcxPivotGridOLAPOLEDBProvider.DoConnect;
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
  Connection.ConnectionString := AConnectionString;
  Connection.Open;
  ExtractProviderVersion(GetProviderVersion);
end;

procedure TcxPivotGridOLAPOLEDBProvider.DoDisconnect;
begin
  Connection.Close;
end;

function TcxPivotGridOLAPOLEDBProvider.DoOpenSchema(const ASchemaType: string; const ARestriction: OleVariant): TcxPivotGridOLAPOLEDBDataSet;
begin
  Result := CreateDataSet;
  try
    Connection.OpenSchema(siProviderSpecific, ARestriction, ASchemaType, TcxPivotGridOLAPOLEDBDataSet(Result).InnerDataSet);
    TcxPivotGridOLAPOLEDBDataSet(Result).InitializeFields;
    Result.First;
  except
    FreeAndNil(Result);
  end;
end;

function TcxPivotGridOLAPOLEDBProvider.Execute(const ACommandText: string): TcxCustomPivotGridOLAPDataSet;
begin
  Result := CreateDataSet;
  try
    TcxPivotGridOLAPOLEDBDataSet(Result).InnerDataSet.Recordset := Connection.Execute(ACommandText);
    TcxPivotGridOLAPOLEDBDataSet(Result).InitializeFields;
  except
    FreeAndNil(Result);
    //raise; //#TODO: check ErrorCode as .Net
  end;
end;

function TcxPivotGridOLAPOLEDBProvider.Execute(const AQuery: TcxMDXQuery): TcxCustomPivotGridOLAPDataSet;
begin
  Result := Execute(AQuery.GetQuery);
  if Result <> nil then
    TcxPivotGridOLAPOLEDBDataSet(Result).FQuery := AQuery;
end;

procedure TcxPivotGridOLAPOLEDBProvider.PopulateCatalogs(ANames: TStrings);
var
  ACatalogNameField: TcxPivotGridOLAPDBDataSetField;
  ADataSet: TcxPivotGridOLAPOLEDBDataSet;
  I: Integer;
begin
  ADataSet := DoOpenSchema(SCHEMA_CATALOGS, EmptyParam);
  try
    ACatalogNameField := ADataSet.FieldByName('CATALOG_NAME');
    for I := 0 to ADataSet.RecordCount - 1 do
    begin
      ANames.Add(ACatalogNameField.AsString);
      ADataSet.Next;
    end;
  finally
    FreeAndNil(ADataSet);
  end;
end;

procedure TcxPivotGridOLAPOLEDBProvider.PopulateHierarchyAllMemberNames(const ACubeName: string;
  ANames: TDictionary<string, string>);
var
  I: Integer;
  ACommandText: string;
  ADataSet: TcxPivotGridOLAPOLEDBDataSet;
  AHierarchyNameField, AAllMemberNameField: TcxPivotGridOLAPDBDataSetField;
begin
  if Version <= 9 then
    Exit;
  ACommandText := Format(
    'SELECT [HIERARCHY_UNIQUE_NAME], [ALL_MEMBER] FROM $system.MDSCHEMA_HIERARCHIES WHERE [CUBE_NAME]=%s AND [ALL_MEMBER]>''''',
    [QuotedStr(ACubeName)]);
  ADataSet := Execute(ACommandText) as TcxPivotGridOLAPOLEDBDataSet;
  try
    if not ADataSet.IsValid then
      Exit;
    AHierarchyNameField := ADataSet.FieldByName('HIERARCHY_UNIQUE_NAME');
    AAllMemberNameField := ADataSet.FieldByName('ALL_MEMBER');
    for I := 0 to ADataSet.RecordCount - 1 do
    begin
      ANames.Add(AHierarchyNameField.AsString, AAllMemberNameField.AsString);
      ADataSet.Next;
    end;
  finally
    ADataSet.Free
  end;
end;

function TcxPivotGridOLAPOLEDBProvider.OpenSchema(const ACubeName: string; ASchema: TcxPivotGridOLAPSchemaInfo): TcxPivotGridOLAPDBDataSet;
var
  ARestriction: OleVariant;
begin
  Result := nil;
  if (ACubeName = '') or IsLocalCube then
    ARestriction := EmptyParam
  else
    ARestriction := VarArrayOf([Null, Null, ACubeName, Null]);
  case ASchema of
    osiLevels:
      Result := DoOpenSchema(SCHEMA_LEVELS, ARestriction);
    osiMeasures:
      Result := DoOpenSchema(SCHEMA_MEASURES, ARestriction);
    osiKPIs:
      Result := DoOpenSchema(SCHEMA_KPI, ARestriction);
    osiSets:
      Result := DoOpenSchema(SCHEMA_SETS, ARestriction);
    osiHierarchies:
      Result := DoOpenSchema(SCHEMA_HIERARCHIES, ARestriction);
  else
    PivotGridError(False, 'Not implemented!');
  end;
end;

{ TcxCustomPivotGridOLAPOLEDBDataSetReader }

constructor TcxCustomPivotGridOLAPOLEDBDataSetReader.Create(ADataSet: TcxCustomPivotGridOLAPDataSet);
begin
  inherited Create(ADataSet);
  Assert(ADataSet is TcxPivotGridOLAPOLEDBDataSet);
end;

function TcxCustomPivotGridOLAPOLEDBDataSetReader.GetDataSet: TcxPivotGridOLAPOLEDBDataSet;
begin
  Result := TcxPivotGridOLAPOLEDBDataSet(inherited DataSet);
end;

class procedure TcxCustomPivotGridOLAPOLEDBDataSetReader.ProcessDataFields(DataSet: TcxCustomPivotGridOLAPDataSet;
  AColumnGroup, ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields; AFieldIndex: Integer;
  const AFieldDimension: string);

  function SeparateDimensionAndMeasure(AValue: string; out ADimension, AMeasure: string): Boolean;
  var
    APosition: Integer;
  begin
    Result := False;
    APosition := Pos('[Measures]', AValue);
    if APosition > 0 then
    begin
      ADimension := Copy(AValue, 1, APosition - 2);
      AMeasure := Copy(AValue, APosition, Length(AValue) - APosition + 1);
      Result := True;
    end;
  end;

var
  AIsValuesInitialized: Boolean;
  AValues, ANativeValues: Variant;
  ADimension, AMeasure: string;
  ADataSet: TcxPivotGridOLAPOLEDBDataSet;
  ASummaryIndex, AProcessedFieldCount: Integer;
begin
  ADataSet := DataSet as TcxPivotGridOLAPOLEDBDataSet;
  AValues := VarArrayCreate([0, ADataFields.Count - 1], varVariant);
  ANativeValues := VarArrayCreate([0, ADataFields.Count - 1], varVariant);
  AProcessedFieldCount := 0;
  AIsValuesInitialized := False;
  for ASummaryIndex := 0 to ADataFields.Count - 1 do
  begin
    AValues[ASummaryIndex] := Null;
    ANativeValues[ASummaryIndex] := Null;
    if AFieldIndex + AProcessedFieldCount < ADataset.FieldCount then //OLAP v.8 problem
      if SeparateDimensionAndMeasure(ADataset.Fields[AFieldIndex + AProcessedFieldCount].DisplayName, ADimension, AMeasure) and
        ((AFieldDimension = '') or (AFieldDimension = ADimension)) and (ADataFields[ASummaryIndex].UniqueName = AMeasure) then
      begin
        AValues[ASummaryIndex] := ADataset.Fields[AFieldIndex + AProcessedFieldCount].Value;
        Inc(AProcessedFieldCount);
      end
      else
        AValues[ASummaryIndex] := Null;
    ANativeValues[ASummaryIndex] := AValues[ASummaryIndex];
    AIsValuesInitialized := False;
    if VarIsEmpty(AValues[ASummaryIndex]) then
      AValues[ASummaryIndex] := Null
    else
      AIsValuesInitialized := True;
  end;

  if AIsValuesInitialized then
    SetCrossCellValues(ARowGroup.GetCellByCrossItem(AColumnGroup), AValues, ANativeValues)
  else
    SetCrossCellValues(ARowGroup.GetCellByCrossItem(AColumnGroup), Null, Null);
end;

class procedure TcxCustomPivotGridOLAPOLEDBDataSetReader.ProcessDataFieldsEx(DataSet: TcxCustomPivotGridOLAPDataSet;
  AColumnGroup, ARowGroup: TcxPivotGridGroupItem; ADataFields: TcxPivotGridFields);
var
  I: Integer;
  AField: TcxPivotGridOLAPDBDataSetField;
  AIsValuesInitialized: Boolean;
  AValues, ANativeValues: Variant;
  ADataSet: TcxPivotGridOLAPOLEDBDataSet;
begin
  ADataSet := DataSet as TcxPivotGridOLAPOLEDBDataSet;
  AIsValuesInitialized := False;
  AValues := VarArrayCreate([0, ADataFields.Count - 1], varVariant);
  ANativeValues := VarArrayCreate([0, ADataFields.Count - 1], varVariant);
  for I := 0 to ADataFields.Count - 1 do
  begin
    AField := ADataSet.FindField(ADataFields[I].UniqueName);
    if AField = nil then
      ANativeValues[I] := Null
    else
      ANativeValues[I] := AField.Value;
    if VarIsEmpty(ANativeValues[I]) then
      AValues[I] := Null
    else
    begin
      AValues[I] := ANativeValues[I];
      AIsValuesInitialized := True;
    end;
  end;
  if AIsValuesInitialized then
    SetCrossCellValues(ARowGroup.GetCellByCrossItem(AColumnGroup), AValues, ANativeValues)
  else
    SetCrossCellValues(ARowGroup.GetCellByCrossItem(AColumnGroup), Null, Null);
end;

{ TcxPivotGridOLAPOLEDBDataSetCrossCellsReader }

type
  TcxOLEDBQueryData = class
  protected const
    MEASURE_AXIS = 0;
    COLUMN_AXIS = 1;
    ROW_AXIS = 2;
  protected type
    TItem = class
    private type
      TMember = array of string;
      TMembers = array of TMember;
    private
      FColumn: TMembers;
      FRow: TMembers;
      FValues: TVariantArray;
      function ExtractMembers(const AMembers: string): TMember;
      function IsSame(const AMembers: TMembers; const ACoordinate: array of string; ALevel: Integer): Boolean;
    public
      constructor Create(const AColumn, ARow: TStringDynArray);
      function GetValue(MeasureIndex: Integer): Variant;
      function IsEquals(AItem: TItem): Boolean;
      function IsSameColumn(const ACoordinate: array of string; ALevel: Integer): Boolean;
      function IsSameRow(const ACoordinate: array of string; ALevel: Integer): Boolean;

      property Column: TMembers read FColumn;
      property Row: TMembers read FRow;
    end;
  strict private
    FList: TObjectList;
    FQuery: TcxMDXQuery;
    FMeasures: TStringDynArray;
    function GetCount: Integer;
    function GetItem(Index: Integer): TcxOLEDBQueryData.TItem;
  protected
    property Query: TcxMDXQuery read FQuery;
  public
    constructor Create(AQuery: TcxMDXQuery);
    destructor Destroy; override;
    procedure Initialize(ADataSet: TcxPivotGridOLAPOLEDBDataSet);
    procedure Normalize;
    function AddItem(const AColumn, ARow: TStringDynArray): TItem;
    procedure AddMeasure(const AMeasure: string);
    procedure GetMeasureValue(AItemIndex: Integer; const MeasureUniqueName: string);

    property Count: Integer read GetCount;
    property Items[Index: Integer]: TcxOLEDBQueryData.TItem read GetItem; default;
  end;

{ TcxOLEDBQueryData.TItem }

constructor TcxOLEDBQueryData.TItem.Create(const AColumn, ARow: TStringDynArray);
var
  I: Integer;
begin
  inherited Create;
  SetLength(FColumn, Length(AColumn));
  for I := Low(AColumn) to High(AColumn) do
    FColumn[I] := ExtractMembers(AColumn[I]);

  SetLength(FRow, Length(ARow));
  for I := Low(ARow) to High(ARow) do
    FRow[I] := ExtractMembers(ARow[I]);
end;

function TcxOLEDBQueryData.TItem.GetValue(MeasureIndex: Integer): Variant;
begin
  if (MeasureIndex < 0) or (MeasureIndex > High(FValues)) then
    TList.Error(@SListIndexError, MeasureIndex);
  Result := FValues[MeasureIndex];
end;

function TcxOLEDBQueryData.TItem.IsEquals(AItem: TItem): Boolean;
var
  I: Integer;
begin
  Result := (Length(FColumn) = Length(AItem.FColumn)) and
    (Length(FRow) = Length(AItem.FRow)) and
    (Length(FValues) = Length(AItem.FValues));

  for I := Low(FColumn) to High(FColumn) do
    if FColumn[I] <> AItem.FColumn[I] then
      Exit(False);
  for I := Low(FRow) to High(FRow) do
    if FRow[I] <> AItem.FRow[I] then
      Exit(False);
  for I := Low(FValues) to High(FValues) do
    if not VarEquals(FValues[I], AItem.FValues[I]) then
      Exit(False);
end;

function TcxOLEDBQueryData.TItem.IsSameColumn(const ACoordinate: array of string; ALevel: Integer): Boolean;
begin
  Result := IsSame(FColumn, ACoordinate, ALevel);
end;

function TcxOLEDBQueryData.TItem.IsSameRow(const ACoordinate: array of string; ALevel: Integer): Boolean;
begin
  Result := IsSame(FRow, ACoordinate, ALevel);
end;

function TcxOLEDBQueryData.TItem.ExtractMembers(const AMembers: string): TMember;
const
  Quote = '''';
  Separator = ',';
var
  S: string;
  AChar: Char;
  AInQuote: Boolean;
  I, ALength: Integer;
begin
  SetLength(Result, 1);
  ALength := Length(AMembers);
  Assert((ALength >= 2) and (AMembers[1] = '{') and (AMembers[ALength] = '}'));

  S := '';
  AInQuote := False;
  for I := 2 to ALength - 1 do
  begin
    AChar := AMembers[I];
    case AChar of
      Quote:
        AInQuote := not AInQuote;
      Separator:
        if not AInQuote then
        begin
          Result[Length(Result) - 1] := S;
          SetLength(Result, Length(Result) + 1);
          S := '';
          Continue;
        end;
    end;
    S := S + AChar;
  end;
  Result[Length(Result) - 1] := S;
end;

function TcxOLEDBQueryData.TItem.IsSame(const AMembers: TMembers;
  const ACoordinate: array of string; ALevel: Integer): Boolean;

  function NameInArray(const AUniqueName: string; const AArray: array of string): Boolean;
  var
    I: Integer;
  begin
    Result := False;
    for I := 0 to Length(AArray) - 1 do
      if AUniqueName = AArray[I] then
        Exit(True);
  end;

var
  I, ALength: Integer;
begin
  ALength := Length(ACoordinate);
  Result := ALength = Length(AMembers);
  if Result then
    for I := 0 to Min(ALevel, ALength) - 1 do
      if not NameInArray(ACoordinate[I], AMembers[I]) then
        Exit(False);
end;

{ TcxOLEDBQueryData }

constructor TcxOLEDBQueryData.Create(AQuery: TcxMDXQuery);
begin
  inherited Create;
  FQuery := AQuery;
  FList := TObjectList.Create;
end;

destructor TcxOLEDBQueryData.Destroy;
begin
  FreeAndNil(FList);
  inherited Destroy;
end;

procedure TcxOLEDBQueryData.Initialize(ADataSet: TcxPivotGridOLAPOLEDBDataSet);

  function DoGetCoordinate(AAxisMembers: TcxMDXQueryAxisMembers; const AUniqueNameFormat: string): TStringDynArray;
  var
    V: Variant;
    I, AIndex: Integer;
  begin
    AIndex := 0;
    SetLength(Result, AAxisMembers.FieldCount);
    for I := 0 to AAxisMembers.FieldCount - 1 do
    begin
      if (I > 0) and TcxCustomOLAPDataQueryBuilder.SameHierarchy(AAxisMembers.Fields[I], AAxisMembers.Fields[I - 1]) then
        Continue;
      V := ADataSet.FieldByName(Format(AUniqueNameFormat, [AIndex])).Value;
      if VarIsNull(V) then
        Break;
      Result[AIndex] := V;
      Inc(AIndex);
    end;
    SetLength(Result, AIndex);
  end;

var
  AItem: TItem;
  AUniqueName: string;
  I, AMeasureCount: Integer;
  AColumn, ARow: TStringDynArray;
  AMeasureFields: array of TcxPivotGridOLAPDBDataSetField;
  AMeasureMembers, AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;
begin
  if not Query.TryGetMembers(MEASURE_AXIS, AMeasureMembers) or
    not Query.TryGetMembers(COLUMN_AXIS, AColumnMembers) or
    not Query.TryGetMembers(ROW_AXIS, ARowMembers) then
    raise EcxPivotGrid.Create('Internal error');
  AMeasureCount := AMeasureMembers.MeasureCount;

  SetLength(AMeasureFields, AMeasureCount);
  for I := 0 to AMeasureCount - 1 do
  begin
    AUniqueName := (AMeasureMembers.Measures[I] as TcxPivotGridOLAPField).UniqueName;
    AddMeasure(AUniqueName);
    AMeasureFields[I] := ADataSet.FindField(AUniqueName);
  end;

  while not ADataSet.Eof do
  begin
    AColumn := DoGetCoordinate(AColumnMembers, '[Measures].[DX_COLUMN_%d_UNIQUE_NAME]');
    ARow := DoGetCoordinate(ARowMembers, '[Measures].[DX_ROW_%d_UNIQUE_NAME]');
    if (Length(AColumn) <> 0) and (Length(ARow) <> 0) then
    begin
      AItem := AddItem(AColumn, ARow);
      SetLength(AItem.FValues, AMeasureCount);
      for I := 0 to AMeasureCount - 1 do
      begin
        if AMeasureFields[I] = nil then
          AItem.FValues[I] := Null
        else
          AItem.FValues[I] := AMeasureFields[I].Value;
      end;
    end;
    ADataSet.Next;
  end;
end;

procedure TcxOLEDBQueryData.Normalize;

  function IndexOf(AItems: TList; const ARowCoordinate: TStringDynArray; ALevel: Integer): Integer;
  var
    I: Integer;
  begin
    Result := -1;
    for I := 0 to AItems.Count - 1 do
      if TcxOLEDBQueryData.TItem(AItems[I]).IsSameRow(ARowCoordinate, ALevel) then
        Exit(I);
  end;

var
  I, AIndex: Integer;
  AList: TObjectList;
  ARowCoordinate: TStringDynArray;
  ARowMembers: TcxMDXQueryAxisMembers;
begin
  if not Query.TryGetMembers(ROW_AXIS, ARowMembers) then
    raise EcxPivotGrid.Create('Internal error');
  AList := FList;
  FList := TObjectList.Create;
  try
    for I := 0 to ARowMembers.MemberCount - 1 do
    begin
      ARowCoordinate := ARowMembers.GetMemberCoordinate(I);
      repeat
        AIndex := IndexOf(AList, ARowCoordinate, ARowMembers.Members[I].Count);
        if AIndex >= 0 then
          FList.Add(AList.Extract(AList[AIndex]))
        else
          Break;
      until False;
    end;
    Assert(AList.Count = 0);
  finally
    FreeAndNil(AList);
  end;
end;

function TcxOLEDBQueryData.AddItem(const AColumn, ARow: TStringDynArray): TItem;
begin
  Result := TItem.Create(AColumn, ARow);
  FList.Add(Result);
end;

procedure TcxOLEDBQueryData.AddMeasure(const AMeasure: string);
var
  I: Integer;
begin
  I := Length(FMeasures);
  SetLength(FMeasures, I + 1);
  FMeasures[I] := AMeasure;
end;

procedure TcxOLEDBQueryData.GetMeasureValue(AItemIndex: Integer; const MeasureUniqueName: string);
var
  I, AIndex: Integer;
begin
  AIndex := -1;
  for I := Low(FMeasures) to High(FMeasures) do
    if MeasureUniqueName = FMeasures[I] then
    begin
      AIndex := I;
      Break;
    end;
  if (AIndex < 0) or (AIndex > High(FMeasures)) then
    TList.Error(@SListIndexError, AIndex);
  Items[AItemIndex].GetValue(AIndex);
end;


function TcxOLEDBQueryData.GetCount: Integer;
begin
  Result := FList.Count;
end;

function TcxOLEDBQueryData.GetItem(Index: Integer): TcxOLEDBQueryData.TItem;
begin
  Result := TcxOLEDBQueryData.TItem(FList[Index]);
end;

procedure TcxPivotGridOLAPOLEDBDataSetCrossCellsReader.Execute;

  function NativeValueToValue(const ANativeValue: Variant): Variant;
  begin
    if VarIsEmpty(ANativeValue) then
      Result := Null
    else
      Result := ANativeValue;
  end;

var
  AMeasureMembers, AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;

  procedure IterateMeasures(AData: TcxOLEDBQueryData; var AItemIndex: Integer;
    AColumnGroup, ARowGroup: TcxPivotGridGroupItem);
  var
    I: Integer;
    ANativeValue: Variant;
    ACell: TcxPivotGridCrossCell;
  begin
    ACell := AColumnGroup.GetCellByCrossItem(ARowGroup);
    for I := 0 to AMeasureMembers.MeasureCount - 1 do
    begin
      ANativeValue := AData.Items[AItemIndex].GetValue(I);
      SetCrossCellValue(ACell, I, ANativeValue, NativeValueToValue(ANativeValue));
    end;
    Inc(AItemIndex);
  end;

  procedure IterateColumns(AData: TcxOLEDBQueryData; var AItemIndex: Integer; ARowGroup: TcxPivotGridGroupItem);
  var
    I: Integer;
    ACoordinate: TStringDynArray;
    AItem: TcxOLEDBQueryData.TItem;
  begin
    for I := 0 to AColumnMembers.MemberCount - 1 do
    begin
      AItem := AData.Items[AItemIndex];
      ACoordinate := AColumnMembers.GetMemberCoordinate(I);
      if AItem.IsSameColumn(ACoordinate, AColumnMembers.Members[I].Count) then
        IterateMeasures(AData, AItemIndex, AColumnMembers.Members[I].Last, ARowGroup);
    end;
  end;

  procedure IterateRows(AData: TcxOLEDBQueryData; var AItemIndex: Integer);
  var
    I, AMemberCount: Integer;
    ACoordinate: TStringDynArray;
    AItem: TcxOLEDBQueryData.TItem;
  begin
    AMemberCount := ARowMembers.MemberCount;
    for I := 0 to AMemberCount - 1 do
    begin
      AItem := AData.Items[AItemIndex];
      ACoordinate := ARowMembers.GetMemberCoordinate(I);
      if AItem.IsSameRow(ACoordinate, ARowMembers.Members[I].Count) then
        IterateColumns(AData, AItemIndex, ARowMembers.Members[I].Last);
      while (I < AMemberCount - 1) and (AItemIndex + 1 < AData.Count) and
        AData.Items[AItemIndex].IsSameRow(ACoordinate, ARowMembers.Members[I].Count) and
        not AData.Items[AItemIndex].IsSameRow(ARowMembers.GetMemberCoordinate(I + 1), ARowMembers.Members[I + 1].Count) do
        Inc(AItemIndex);
    end;
  end;

var
  I: Integer;
  AQuery: TcxMDXQuery;
  AData: TcxOLEDBQueryData;
begin
  AQuery := DataSet.Query;
  if not AQuery.TryGetMembers(TcxOLEDBQueryData.MEASURE_AXIS, AMeasureMembers) or
    not AQuery.TryGetMembers(TcxOLEDBQueryData.COLUMN_AXIS, AColumnMembers) or
    not AQuery.TryGetMembers(TcxOLEDBQueryData.ROW_AXIS, ARowMembers) then
    raise EcxPivotGrid.Create('Not implemented.');

  AData := TcxOLEDBQueryData.Create(AQuery);
  try
    AData.Initialize(DataSet);
    AData.Normalize;
    I := 0;
    IterateRows(AData, I);
  finally
    AData.Free;
  end;
end;

procedure TcxPivotGridOLAPOLEDBDataSetCrossCellsReader.Initialize(AOwnedGroup, ARowGroups, AColumnGroups: TcxPivotGridGroupItem;
  ASummaryFields: TcxPivotGridFields);
begin
  FOwnedGroup := AOwnedGroup;
  FRowGroups := ARowGroups;
  FColumnGroups := AColumnGroups;
  FSummaryFields := ASummaryFields;
end;

{ TcxPivotGridOLAPOLEDBDataSetMembersReader }

procedure TcxPivotGridOLAPOLEDBDataSetMembersReader.Execute;
var
  I: Integer;
begin
  DataSet.First;
  for I := 0 to DataSet.RecordCount - 1 do
  begin
    AddGroupItem(I);
    DataSet.Next;
  end;
end;

procedure TcxPivotGridOLAPOLEDBDataSetMembersReader.Initialize(AGroupValues: TcxPivotGridOLAPGroupInfos; AField: TcxPivotGridField);
begin
  FGroupValues := AGroupValues;
  FField := AField;
end;

procedure TcxPivotGridOLAPOLEDBDataSetMembersReader.AddGroupItem(AIndex: Integer);

  function DataIsNull: Boolean;
  var
    I: Integer;
  begin
    Result := True;
    for I := DataSet.FieldByName(OLAPMemberUniqueNameColumnName).Index + 1 to DataSet.FieldCount - 1 do
    begin
      if DataSet.Fields[I].Value <> Null then
      begin
        Result := False;
        Break;
      end;
    end;
  end;

var
  AUniqueName, AFullName: string;
  AMemberFieldCaption, AMemberName: TcxPivotGridOLAPDBDataSetField;
begin
  AUniqueName := DataSet.FieldByName(OLAPMemberUniqueNameColumnName).AsString;
  if Pos(TcxPivotGridOLAPField(Field).HierarchyUniqueName + '.[All]', AUniqueName) <= 1 then
  begin
    AMemberFieldCaption := DataSet.Fields[0];
    AMemberName := DataSet.FindField(OLAPMemberNameColumnName);
    if AMemberName = nil then
      AMemberName := DataSet.Fields[1];
    if VarIsNull(AMemberName.Value) then
      AFullName := AMemberFieldCaption.AsString
    else
      AFullName := AMemberName.AsString;
    GroupValues.AddItem(AUniqueName, AFullName, DataIsNull);
  end;
end;

{ TcxPivotGridOLAP8OLEDBDataSetMembersReader }

procedure TcxPivotGridOLAP8OLEDBDataSetMembersReader.Execute;
var
  I: Integer;
begin
  for I := 0 to DataSet.FieldCount - 1 do
    AddGroupItem(I);
end;

function TcxPivotGridOLAP8OLEDBDataSetMembersReader.ExtractMemberName(const AUniqueName: string): string;
const
  StartSign = '.[';
  EndSign = ']';
var
  AStart, AEnd: Integer;
begin
  Result := '';
  AStart := Pos(StartSign, AUniqueName);
  while PosEx(StartSign, AUniqueName, AStart + 1) > 0 do
    AStart := PosEx(StartSign, AUniqueName, AStart + 1);

  if AStart > 0 then
  begin
    AEnd := PosEx(EndSign, AUniqueName, AStart);
    Result := Copy(AUniqueName, AStart + 2, AEnd - AStart - 2);
  end;
end;

procedure TcxPivotGridOLAP8OLEDBDataSetMembersReader.AddGroupItem(AIndex: Integer);
var
  AUniqueName: string;
begin
  AUniqueName := DataSet.Fields[AIndex].DisplayName;
  GroupValues.AddItem(AUniqueName, ExtractMemberName(AUniqueName), False);
end;

{ TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader }

procedure TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader.Execute;

  function GetChildColumnGroupByUniqueName(const AUniqueName: string): TcxPivotGridGroupItem;
  var
    I: Integer;
  begin
    Result := nil;
    for I := 0 to ColumnGroups.ItemCount - 1 do
      if ColumnGroups.Items[I].UniqueName = AUniqueName then
      begin
        Result := ColumnGroups.Items[I];
        Break;
      end;
  end;

var
  I: Integer;
  AUniqueName: string;
  AColumnGroup: TcxPivotGridGroupItem;
  ADimensionField: TcxPivotGridOLAPDBDataSetField;
begin
  DataSet.First;
  ADimensionField := GetDimensionField;
  for I := 0 to DataSet.RecordCount - 1 do
  begin
    AUniqueName := ADimensionField.Value;
    AColumnGroup := GetChildColumnGroupByUniqueName(AUniqueName);
    if AColumnGroup <> nil then
      ProcessDataFieldsEx(DataSet, AColumnGroup, RowGroups, SummaryFields);
    DataSet.Next;
  end;
end;

function TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader.GetDimensionField: TcxPivotGridOLAPDBDataSetField;
var
  I: Integer;
  AName: string;
begin
  if (AnExpandedMembers <> nil) and IsNotLastVisibleOLAPHierarchyItem(TcxPivotGridGroupItem(AnExpandedMembers[0])) then
    AName := TcxPivotGridOLAPField(TcxPivotGridGroupItem(AnExpandedMembers[0]).Field).HierarchyUniqueName
  else
    AName := GetMeasureName;
  Result := nil;
  for I := 0 to DataSet.FieldCount - 1 do
    if Pos(AName, DataSet.Fields[I].DisplayName) > 0 then
      Result := DataSet.Fields[I];
  if Result = nil then
    FieldNotFoundException;
end;

function TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader.GetMeasureName: string;
begin
  Result := OLAPMemberUniqueNameColumnName;
end;

procedure TcxPivotGridOLAPOLEDBDataSetGrandTotalsReader.Initialize(AColumnGroups, ARowGroups: TcxPivotGridGroupItem;
  ASummaryFields: TcxPivotGridFields; AnExpandedMembers: TList);
begin
  FColumnGroups := AColumnGroups;
  FRowGroups := ARowGroups;
  FSummaryFields := ASummaryFields;
  FAnExpandedMembers := AnExpandedMembers;
end;

{ TcxPivotGridOLAP8OLEDBDataSetGrandTotalsReader }

function TcxPivotGridOLAP8OLEDBDataSetGrandTotalsReader.GetMeasureName: string;
begin
  Result := SummaryFields[0].UniqueName;
end;

{ TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader }

procedure TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader.Execute;
begin
  ProcessDataFields(DataSet, ColumnGroup, RowGroup, DataFields, 0, '');
end;

procedure TcxPivotGridOLAPOLEDBDataSetMainGrandTotalReader.Initialize(AColumnGroup, ARowGroup: TcxPivotGridGroupItem;
  ADataFields: TcxPivotGridFields);
begin
  FColumnGroup := AColumnGroup;
  FRowGroup := ARowGroup;
  FDataFields := ADataFields;
end;

initialization
  TcxPivotGridOLAPOLEDBProvider.Register;

finalization
  TcxPivotGridOLAPOLEDBProvider.Unregister;

end.


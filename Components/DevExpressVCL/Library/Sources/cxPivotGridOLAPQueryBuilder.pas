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

unit cxPivotGridOLAPQueryBuilder;

{$I cxVer.inc}

interface

uses
  Types, Classes, SysUtils, dxCore, cxClasses, cxCustomPivotGrid, Generics.Defaults, Generics.Collections, Contnrs;

type

  TcxMDXQuery = class; //internal type
  TcxMDXQueryAxisMembers = class; //internal type

  TcxOLAPStructureMembersType = (smtAll, smtItem, smtChildren);

  { TcxCustomOLAPDataQueryBuilder }

  TcxCustomOLAPDataQueryBuilder = class
  protected
    FAxisIndex: Integer;
  public
    function GetDrillDownQueryString(const ACubeName: string; ACrossCell: TcxPivotGridCrossCell;
      ASummaryIndex: Integer): string; virtual; abstract;
    function GetKPIQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
      ARows: TcxPivotGridFields; AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; virtual; abstract;
    function GetKPITotalQueryString(const ACubeName: string; AColumns, ARows: TcxPivotGridFields;
      AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; virtual; abstract;
    function GetKPITotalsQueryString(const ACubeName: string; AColumns, AFilters: TcxPivotGridFields;
      AnExpandedMembers: TList; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; virtual; abstract;
    class function GetMeasureCount(AMeasures: TcxPivotGridFields; AFieldType: TcxPivotGridOLAPFieldType): Integer;
    function GetMembersQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField; AnExpandedMembers: TList;
      AMeasures, AFilters: TcxPivotGridFields): string; virtual; abstract;
    function GetParentQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField;
      const AMemberUniqueName: string): string; virtual; abstract;
    function GetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
      AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields): TcxMDXQuery; virtual; abstract;
    function GetTotalQueryString(const ACubeName: string; AFilters: TcxPivotGridFields;
      AMeasures: TcxPivotGridFields): string; virtual; abstract;
    function GetTotalsQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
      AnExpandedMembers: TList; AMeasures: TcxPivotGridFields; AFilters: TcxPivotGridFields): string; virtual; abstract;
    class function SameHierarchy(AField1, AField2: TcxPivotGridField): Boolean; //internal use only
  end;

  { TcxOLAPDataQueryBuilder }

  TcxOLAPDataQueryBuilder = class(TcxCustomOLAPDataQueryBuilder)
  strict protected
    FVersion: Integer;
    procedure ClearAxisIndex;
    function CreateFilterWhereClause(AFilters: TcxPivotGridFields): string;
    procedure DoGetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem; AColumns, ARows,
      AMeasures, AFilters: TcxPivotGridFields; AColumnMembers, AMeasureMembers, ARowMembers: TcxMDXQueryAxisMembers);
    procedure ExcludeExpandingMembersFormFilters(AnExpandedMembers: TList; AFilters: TcxPivotGridFields);
    procedure ExcludeField(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField);
    function GetAxis: string;
    function GetExpandedMembers(AnExpandedMembers: TList; AClearMember: Boolean = False): string;
    function GetFilterString(AFilters: TcxPivotGridFields; AFilter: TcxPivotGridOLAPField): string;
    function GetMeasures(AMeasures: TcxPivotGridFields; const AAdditionalString: string = ''): string;
    function GetMembers(AFilter: TStrings): string;
    function GetNamedSetName(AField: TcxPivotGridOLAPField): string;
    function GetNamedSetsQuery(ANamedSets: TList): string;
    function GetSortOrder(AField: TcxPivotGridOLAPField): string;
    function GetTuple(ATuple: TcxPivotGridGroupItem): string;
    function IsHierarchy(AField: TcxPivotGridOLAPField;
      AParentField: TcxPivotGridOLAPField): Boolean;
    function IsFiltered(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField): Boolean;
    function IsOLAPVersion8: Boolean;
    function MakeFilterFields(AFilters: TcxPivotGridFields): TcxPivotGridFields;
    function WriteAllMembersWithSorting(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField;
      IncludeCalculatedMembers: Boolean): string;
    function WriteAllMembersCore(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField;
      IncludeCalculatedMembers: Boolean; AFilter: Boolean): string;
    function WriteSortedMembers(AField: TcxPivotGridOLAPField;
      const AContent: string): string;

    //query parts
    function ColumnPartVersion8(const ACubeName: string; AColumns: TcxPivotGridFields; AMeasures,
      AFilters: TcxPivotGridFields; IncludeCalculatedMembers, IncludeTotals: Boolean;
      const AAdditionalString: string = ''): string;
    function ColumnPart(const ACubeName: string; AColumns, AMeasures, AFilters: TcxPivotGridFields;
      IncludeCalculatedMembers, IncludeTotals, IsFirstMember: Boolean; const AAdditionalString: string): string;
    function SourcePart(const ACubeName: string; AFilters: TcxPivotGridFields; ANamedSets: TList): string;
    function SourcePartWithSubSelect(const ACubeName: string; AFilters: TcxPivotGridFields;
      ANamedSets: TList): string;

    procedure TrimTrailingComma(var AStr: string);
    procedure TrimTrailingSequence(var AStr: string; const ASequence: string);

    function GetSortBy(AField: TcxPivotGridOLAPField): string;
  public
    constructor Create(AVersion: Integer);
    function GetDrillDownQueryString(const ACubeName: string; ACrossCell: TcxPivotGridCrossCell;
      ASummaryIndex: Integer): string; override;
    function GetKPIQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
      ARows: TcxPivotGridFields; AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; override;
    function GetKPITotalQueryString(const ACubeName: string; AColumns, ARows: TcxPivotGridFields;
      AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; override;
    function GetKPITotalsQueryString(const ACubeName: string; AColumns, AFilters: TcxPivotGridFields;
      AnExpandedMembers: TList; AKPIType: TcxOLAPKPIType; const AKPIName: string): string; override;
    function GetMembersQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField; AnExpandedMembers: TList;
      AMeasures, AFilters: TcxPivotGridFields): string; override;
    function GetParentQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField;
      const AMemberUniqueName: string): string; override;
    function GetTotalQueryString(const ACubeName: string; AFilters: TcxPivotGridFields;
      AMeasures: TcxPivotGridFields): string; override;
    function GetTotalsQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
      AnExpandedMembers: TList; AMeasures: TcxPivotGridFields; AFilters: TcxPivotGridFields): string; override;

    property Version: Integer read FVersion write FVersion;
  end;

  { TcxOLEDBOLAPDataQueryBuilder }

  TcxOLEDBOLAPDataQueryBuilder = class(TcxOLAPDataQueryBuilder)
  public
    function GetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
      AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields): TcxMDXQuery; override;
  end;

  { TcxADOMDOLAPDataQueryBuilder }

  TcxADOMDOLAPDataQueryBuilder = class(TcxOLAPDataQueryBuilder)
  public
    constructor Create;
    function GetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
      AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields): TcxMDXQuery; override;
    function GetTotalsQueryString(const ACubeName: string; AColumns: TcxPivotGridFields; AnExpandedMembers: TList;
      AMeasures: TcxPivotGridFields; AFilters: TcxPivotGridFields): string; override;
  end;

{$REGION 'internal types'}
  { TcxMDXQueryAxisMembers }

  TcxMDXQueryAxisMembers = class
  public type

    TFields = TList<TcxPivotGridOLAPField>;

    TMember = class
    strict private
      FItems: TList<TcxPivotGridGroupItem>;
      function GetMember(Index: Integer): TcxPivotGridGroupItem;
    protected
      function GetCoordinate(AFields: TFields): TStringDynArray;
      function ToQuery(AFields: TFields): string;
    public
      constructor Create;
      destructor Destroy; override;
      function Add(AItem: TcxPivotGridGroupItem): Integer;
      function Count: Integer;
      function Last: TcxPivotGridGroupItem;

      property Items[Index: Integer]: TcxPivotGridGroupItem read GetMember; default;
    end;

    TMembers = TObjectList<TMember>;

  strict private
    FFields: TFields;
    FMeasures: TFields;
    FMembers: TMembers;
    function GetField(Index: Integer): TcxPivotGridField;
    function GetMeasure(Index: Integer): TcxPivotGridField;
    function GetMember(Index: Integer): TMember;
    function MeasuresToQuery: string;
  public
    constructor Create;
    destructor Destroy; override;
    procedure AddField(AField: TcxPivotGridOLAPField);
    procedure AddMeasure(AField: TcxPivotGridOLAPField);
    function AddMember: TMember;
    function FieldCount: Integer;
    function GetMemberCoordinate(AMemberIndex: Integer): TStringDynArray;
    function LastMember: TMember;
    function MeasureCount: Integer;
    function MemberCount: Integer;
    function ToQuery: string;

    property Fields[Index: Integer]: TcxPivotGridField read GetField;
    property Measures[Index: Integer]: TcxPivotGridField  read GetMeasure;
    property Members[Index: Integer]: TMember read GetMember;
  end;

  { TcxMDXQuery }

  TcxMDXQuery = class
  public const
    COLUMN_AXIS = 0;
    ROW_AXIS = 1;
  private
    FAxisClauses: TStrings;
    FSubcubeClause: string;
    FWhereExpressions: TStrings;
    FWithClauses: TStrings;
    FGarbageCollector: TObjectList;
  protected
    property AxisClauses: TStrings read FAxisClauses;
    property SubcubeClause: string read FSubcubeClause;
    property WhereExpressions: TStrings read FWhereExpressions;
    property WithClauses: TStrings read FWithClauses;
  public
    constructor Create;
    destructor Destroy; override;
    function TryGetMembers(AAxis: Integer; out AMembers: TcxMDXQueryAxisMembers): Boolean;

    procedure AddAxisClause(const AClause: string; AMembers: TcxMDXQueryAxisMembers = nil);
    procedure AddWithClause(const AClause: string);
    procedure AddWhereExpression(const AExpression: string);
    procedure SetSubcubeClause(const AClause: string);

    function GetQuery: string;
  end;
{$ENDREGION}

implementation

uses
  cxCustomData, cxPivotGridStrs;

const
  Comma = ', ';
  CrossJoinOperator = ' * ';

  KPITypeMap: array[TcxOLAPKPIType] of string = ('KPIValue', 'KPIGoal', 'KPIStatus', 'KPITrend', 'KPIWeight');

type
  TcxCustomPivotGridAccess = class(TcxCustomPivotGrid);
  TcxPivotGridGroupItemAccess = class(TcxPivotGridGroupItem);
  TcxPivotGridOLAPFieldOptionsAccess = class(TcxPivotGridOLAPFieldOptions);

function Implode(const AGlue: string; const AStrings: TStringDynArray): string; overload;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to Length(AStrings) - 1 do
    if Result = '' then
      Result := AStrings[I]
    else
      Result := Result  + AGlue + AStrings[I];
end;

function Implode(const AGlue: string; AStrings: TStrings): string; overload;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AStrings.Count - 1 do
    if I = 0 then
      Result := AStrings[I]
    else
      Result := Result  + AGlue + AStrings[I];
end;

{ TcxCustomOLAPDataQueryBuilder }

class function TcxCustomOLAPDataQueryBuilder.GetMeasureCount(AMeasures: TcxPivotGridFields;
  AFieldType: TcxPivotGridOLAPFieldType): Integer;
var
  I: Integer;
begin
  Result := 0;
  for I := 0 to AMeasures.Count - 1 do
    if TcxPivotGridOLAPField(AMeasures[I]).FieldType = AFieldType then
      Inc(Result);
end;

class function TcxCustomOLAPDataQueryBuilder.SameHierarchy(AField1, AField2: TcxPivotGridField): Boolean;
begin
  Result := AField2.IsHierarchy and AField1.IsHierarchy and (AField1.Group = AField2.Group);
end;

{ TcxOLAPDataQueryBuilder }

constructor TcxOLAPDataQueryBuilder.Create(AVersion: Integer);
begin
  inherited Create;
  FVersion := AVersion;
end;

function TcxOLAPDataQueryBuilder.GetDrillDownQueryString(const ACubeName: string;
  ACrossCell: TcxPivotGridCrossCell; ASummaryIndex: Integer): string;
var
  AMeasure: TcxPivotGridOLAPField;
  ATempColumnTuple, ATempRowTuple, AColumnTuple, ARowTuple: string;
begin
  ClearAxisIndex;

  Result := '';
  if ACrossCell = nil then
    Exit;
  AMeasure := ACrossCell.SummaryCells[ASummaryIndex].DataField as TcxPivotGridOLAPField;
  if (AMeasure = nil) or (ACubeName = '') then
    Exit;
  Result := 'drillthrough select non empty ';

  AColumnTuple := GetTuple(ACrossCell.Column);
  ARowTuple := GetTuple(ACrossCell.Row);

  case Version of
  7, 8:
    begin
      ATempColumnTuple := AColumnTuple;
      ATempRowTuple := ARowTuple;
      if (ATempColumnTuple = '') and (ATempRowTuple <> '') then
      begin
        ATempColumnTuple := ATempRowTuple;
        ATempRowTuple := '';
      end;
      Result := Result + '{ ' + ATempColumnTuple + ' }' + GetAxis;
      if ATempRowTuple <> '' then
        Result := Result + ', { ' + ATempRowTuple + ' }' + GetAxis;
      Result := Result + 'from [' + ACubeName + '] ';
    end;
  else
    begin
      if AColumnTuple <> '' then
        Result := Result + '{ { ' + AColumnTuple + ' } } * ';
      Result := Result + '{ ' + AMeasure.UniqueName + ' }' + GetAxis;
      if ARowTuple <> '' then
        Result := Result + ', { { ' + ARowTuple + ' } }' + GetAxis;
      Result := Result + ' from [' + ACubeName + '] ';
    end;
  end;
end;

function TcxOLAPDataQueryBuilder.GetKPIQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
  ARows: TcxPivotGridFields; AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string;
var
  AKPICondition: string;
begin
  ClearAxisIndex;

  Result := '';
  if (AKPIName = '') or (AColumns.Count = 0) then
    Exit;
  AKPICondition := KPITypeMap[AKPIType] + '("' + AKPIName + '")';
  Result := ColumnPart(ACubeName, AColumns, nil, AFilters, True, False, True, '');
  if ARows.Count > 0 then
    Result := Result + ', non empty ' + WriteAllMembersWithSorting(AFilters, TcxPivotGridOLAPField(ARows[0]), True) +
      'dimension properties member_unique_name' + GetAxis;
  Result := Result + ' from ' + SourcePart(ACubeName, nil, nil) + ' where(' + AKPICondition + ')';
end;

function TcxOLAPDataQueryBuilder.GetKPITotalQueryString(const ACubeName: string; AColumns, ARows: TcxPivotGridFields;
  AFilters: TcxPivotGridFields; AKPIType: TcxOLAPKPIType; const AKPIName: string): string;

  function GetDimension(AField: TcxPivotGridOLAPField; AFilters: TcxPivotGridFields): string;
  begin
    if AField.IsFiltered then
    begin
      Result := GetFilterString(AFilters, AField);
      ExcludeField(AFilters, AField);
    end
    else
      Result := AField.HierarchyUniqueName;
  end;

var
  AKPICondition, ACondition: string;
  AColumnExists: Boolean;
  AFilterFields: TcxPivotGridFields;
begin
  ClearAxisIndex;

  Result := '';
  if AKPIName = '' then
    Exit;
  AKPICondition := KPITypeMap[AKPIType] + '("' + AKPIName + '")';
  AFilterFields := MakeFilterFields(AFilters);
  try
    if AFilterFields <> nil then
      AFilterFields.Assign(AFilters);

    if (AColumns.Count = 0) or (ARows.Count = 0) then
    begin
      Result := 'select {' + AKPICondition + '}' + GetAxis;
      AKPICondition := '';
    end
    else
    begin
      Result := 'select ';
      AColumnExists := False;
      if AColumns.Count > 0 then
      begin
        Result := Result + 'non empty {' +
          GetDimension(TcxPivotGridOLAPField(AColumns[0]), AFilterFields) + '}' + GetAxis;
        AColumnExists := True;
      end;
      if ARows.Count > 0 then
      begin
        if AColumnExists then
          Result := Result + ', ';
        Result := Result + 'non empty {' +
          GetDimension(TcxPivotGridOLAPField(ARows[0]), AFilterFields) + '} dimension properties member_unique_name' + GetAxis;
      end;
    end;
    Result := Result + ' from [' + ACubeName + ']';
    ACondition := CreateFilterWhereClause(AFilterFields);
    if (AKPICondition > '') and (ACondition > '') then
      Result := Result + Format(' where (%s, %s)', [AKPICondition, ACondition])
    else
      if ACondition > '' then
        Result := Result + Format(' where (%s)', [ACondition]);
  finally
    FreeAndNil(AFilterFields);
  end;
end;

function TcxOLAPDataQueryBuilder.GetKPITotalsQueryString(const ACubeName: string; AColumns, AFilters: TcxPivotGridFields;
  AnExpandedMembers: TList; AKPIType: TcxOLAPKPIType; const AKPIName: string): string;
var
  AKPICondition: string;
begin
  ClearAxisIndex;

  Result := '';
  if AKPIName = '' then
    Exit;
  AKPICondition := KPITypeMap[AKPIType] + '("' + AKPIName + '")';
  if (AnExpandedMembers <> nil) and IsNotLastVisibleOLAPHierarchyItem(TcxPivotGridGroupItem(AnExpandedMembers[0])) then
    Result := ColumnPart(ACubeName, AColumns, nil, AFilters, True, False, True, GetExpandedMembers(AnExpandedMembers))
  else
  begin
    Result := ColumnPart(ACubeName, AColumns, nil, AFilters, True, False, True, '');
    if AnExpandedMembers <> nil then
      Result := Result + ', ' + GetExpandedMembers(AnExpandedMembers) + GetAxis;
  end;
  if Result <> '' then
    Result := Result + ' from ' + SourcePart(ACubeName, nil, nil) + ' where(' + AKPICondition + ')';
end;

function TcxOLAPDataQueryBuilder.GetMembersQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField;
  AnExpandedMembers: TList; AMeasures, AFilters: TcxPivotGridFields): string;

  function IsFieldInExpandingHierarchy(AField: TcxPivotGridOLAPField; AnExpandedMembers: TList): Boolean;
  var
    I: Integer;
    AExpandedField: TcxPivotGridOLAPField;
  begin
    Result := False;
    if (AField <> nil) and (AnExpandedMembers <> nil) and AField.IsHierarchy then
      for I := 0 to AnExpandedMembers.Count - 1 do
      begin
        AExpandedField := TcxPivotGridOLAPField(TcxPivotGridGroupItem(AnExpandedMembers[I]).Field);
        if AExpandedField.IsHierarchy and (AExpandedField.Group = AField.Group) then
        begin
          Result := True;
          Break;
        end;
      end;
  end;

var
  AMembers, AColumnQuery, ASortedMembers: string;
  ANamedSets: TList;
  AFilterFields: TcxPivotGridFields;
begin
  ClearAxisIndex;

  Result := '';
  if AField <> nil then
    case Version of
      7, 8:
        begin
          Result := Result + 'select non empty { ';
          Result := Result + WriteSortedMembers(AField,
            WriteAllMembersCore(AFilters, AField, True, True)) + '}' + GetAxis;
          Result := Result + ' from ' + SourcePartWithSubSelect(ACubeName, AFilters, nil);
        end
    else
    begin
      AFilterFields := MakeFilterFields(AFilters);
      try
        if AFilterFields <> nil then
        begin
          AFilterFields.Assign(AFilters);
          ExcludeField(AFilterFields, AField);
          ExcludeExpandingMembersFormFilters(AnExpandedMembers, AFilterFields);
        end;

        Result := 'with member Measures.MemberName as ' + AField.HierarchyUniqueName + '.MEMBER_NAME ' +
          'member Measures.MemberUniqueName as ' + AField.HierarchyUniqueName + '.MEMBER_UNIQUE_NAME ';
        AColumnQuery := GetMeasures(AMeasures, 'Measures.MemberName, Measures.MemberUniqueName');

        AMembers := '';
        if not IsFieldInExpandingHierarchy(AField, AnExpandedMembers) then
        begin
          ASortedMembers := WriteSortedMembers(AField,
            WriteAllMembersCore(AFilters, AField, True, AFilterFields <> nil));
          if AField.FieldType = oftSet then
            AMembers := Format('{%s - {%s.[All]}}', [ASortedMembers, AField.LevelUniqueName])
          else
            AMembers := ASortedMembers;
        end;
        if AnExpandedMembers <> nil then
          if AMembers = '' then
            AMembers := GetExpandedMembers(AnExpandedMembers)
          else
            AMembers := Format('%s * %s', [AMembers, GetExpandedMembers(AnExpandedMembers)]);
        ClearAxisIndex;
        Result := Result + 'select non empty ' + AColumnQuery + GetAxis + ', ';
        Result := Result + 'non empty {' + AMembers + '}' + GetAxis;

        ANamedSets := TList.Create;
        try
          if AField.FieldType = oftSet then
            ANamedSets.Add(AField);
          Result := Result + ' from ' + SourcePart(ACubeName, AFilterFields, ANamedSets);
        finally
          ANamedSets.Free;
        end;
      finally
        FreeAndNil(AFilterFields);
      end;
    end;
  end;
end;

function TcxOLAPDataQueryBuilder.GetParentQueryString(const ACubeName: string; AField: TcxPivotGridOLAPField;
  const AMemberUniqueName: string): string;
begin
  ClearAxisIndex;

  Result := '';
  if AField <> nil then
  begin
    Result := 'with member Measures.ParentUniqueName as ' + AField.HierarchyUniqueName + '.PARENT_UNIQUE_NAME ';
    Result := Result + 'select non empty {Measures.ParentUniqueName}' + GetAxis + ', ';
    Result := Result + AMemberUniqueName + GetAxis;
    Result := Result + ' from ' + SourcePartWithSubSelect(ACubeName, nil, nil);
  end;
end;

function TcxOLAPDataQueryBuilder.GetTotalQueryString(const ACubeName: string;
  AFilters: TcxPivotGridFields; AMeasures: TcxPivotGridFields): string;
begin
  Result := '';
  ClearAxisIndex;
  if AMeasures.Count > 0 then
    Result := 'select {'+ GetMeasures(AMeasures) + '}' + GetAxis +
      ' from ' + SourcePartWithSubSelect(ACubeName, AFilters, nil);
end;

function TcxOLAPDataQueryBuilder.GetTotalsQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
  AnExpandedMembers: TList; AMeasures: TcxPivotGridFields; AFilters: TcxPivotGridFields): string;
begin
  ClearAxisIndex;

  Result := '';
  if AMeasures.Count = 0 then
    Exit;

  if (AnExpandedMembers <> nil) and IsNotLastVisibleOLAPHierarchyItem(TcxPivotGridGroupItem(AnExpandedMembers[0])) then
    Result := ColumnPart(ACubeName, AColumns, AMeasures, AFilters, True, True, True, GetExpandedMembers(AnExpandedMembers))
  else
  begin
    if IsOLAPVersion8 then
      Result := ColumnPartVersion8(ACubeName, AColumns, AMeasures, AFilters, True, True)
    else
      Result := ColumnPart(ACubeName, AColumns, AMeasures, AFilters, True, True, True, '');
    if AnExpandedMembers <> nil then
      Result := Result + ', ' + GetExpandedMembers(AnExpandedMembers) + GetAxis;
  end;
  Result := Result + ' from ' + SourcePart(ACubeName, AFilters, nil);
end;

procedure TcxOLAPDataQueryBuilder.ClearAxisIndex;
begin
  FAxisIndex := 0;
end;

function TcxOLAPDataQueryBuilder.CreateFilterWhereClause(AFilters: TcxPivotGridFields): string;
var
  I: Integer;
begin
  Result := '';
  if AFilters = nil then
    Exit;
  for I := AFilters.Count - 1 downto 0 do
    if ((I = AFilters.Count - 1) or
      not IsHierarchy(TcxPivotGridOLAPField(AFilters[I]), TcxPivotGridOLAPField(AFilters[I + 1]))) then
      Result := Result + GetFilterString(AFilters, TcxPivotGridOLAPField(AFilters[I])) + Comma;
  if Result <> '' then
    TrimTrailingComma(Result);
end;

procedure TcxOLAPDataQueryBuilder.DoGetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
  AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields;
  AColumnMembers, AMeasureMembers, ARowMembers: TcxMDXQueryAxisMembers);

  function PopulateMeasures(AMembers: TcxMDXQueryAxisMembers; AMeasures: TcxPivotGridFields): string;
  var
    I: Integer;
  begin
    for I := 0 to AMeasures.Count - 1 do
      AMembers.AddMeasure(TcxPivotGridOLAPField(AMeasures[I]));
  end;

  function GetParentInfo(AGroup: TcxPivotGridGroupItem): TArray<TcxPivotGridGroupItem>;
  var
    ALength: Integer;
  begin
    ALength := 0;
    SetLength(Result, 0);
    while (AGroup <> nil) and (AGroup.UniqueName <> '') do
    begin
      Inc(ALength);
      SetLength(Result, ALength);
      Result[ALength - 1] := AGroup;
      AGroup := AGroup.Parent;
    end;
  end;

  procedure AddParentInfo(const AParentInfo: TArray<TcxPivotGridGroupItem>; AMember: TcxMDXQueryAxisMembers.TMember);
  var
    I: Integer;
  begin
    for I := 0 to Length(AParentInfo) - 1 do
      AMember.Add(AParentInfo[I]);
  end;

  procedure PopulateColumnMembers(AMembers: TcxMDXQueryAxisMembers;
    AColumnAxisGroups: TcxPivotGridGroupItem; AColumnAxisFields: TcxPivotGridFields);
  var
    I: Integer;
    AMember: TcxMDXQueryAxisMembers.TMember;
    AParentInfo: TArray<TcxPivotGridGroupItem>;
  begin
    for I := 0 to AColumnAxisFields.Count - 1 do
      AMembers.AddField(AColumnAxisFields[I] as TcxPivotGridOLAPField);
    AParentInfo := GetParentInfo(AOwnedGroup);
    for I := 0 to AOwnedGroup.ItemCount - 1 do
    begin
      AMember := AMembers.AddMember;
      AddParentInfo(AParentInfo, AMember);
      AMember.Add(AOwnedGroup.Items[I]);
    end;
  end;

  procedure DoPopulateRowMembers(AMembers: TcxMDXQueryAxisMembers; AParentMember: TcxMDXQueryAxisMembers.TMember;
    AGroup: TcxPivotGridGroupItem);
  var
    I: Integer;
    ANewMember: TcxMDXQueryAxisMembers.TMember;
  begin
    if AParentMember = nil then
      ANewMember := AMembers.AddMember
    else
    begin
      ANewMember := AMembers.AddMember;
      for I := 0 to AParentMember.Count - 1 do
        ANewMember.Add(AParentMember[I]);
    end;
    ANewMember.Add(AGroup);
    for I := 0 to AGroup.ItemCount - 1 do
      DoPopulateRowMembers(AMembers, ANewMember, AGroup.Items[I]);
  end;

  procedure PopulateRowMembers(AMembers: TcxMDXQueryAxisMembers;
    ARowAxisGroups: TcxPivotGridGroupItem; ARowAxisFields: TcxPivotGridFields);
  var
    I: Integer;
  begin
    for I := 0 to ARowAxisFields.Count - 1 do
      AMembers.AddField(ARowAxisFields[I] as TcxPivotGridOLAPField);
    for I := 0 to ARowAxisGroups.ItemCount - 1 do
      DoPopulateRowMembers(AMembers, nil, ARowAxisGroups.Items[I]);
  end;

var
  AColumnAxisFields, ARowAxisFields: TcxPivotGridFields;
  AColumnAxisGroups, ARowAxisGroups: TcxPivotGridGroupItem;
begin
  ARowAxisGroups := TcxCustomPivotGridAccess(AOwnedGroup.PivotGrid).DataBuilder.Rows;
  AColumnAxisGroups := TcxCustomPivotGridAccess(AOwnedGroup.PivotGrid).DataBuilder.Columns;
  ARowAxisFields := ARows;
  AColumnAxisFields := AColumns;
  if TcxPivotGridGroupItemAccess(AOwnedGroup).IsRow then
  begin
    ARowAxisGroups := TcxCustomPivotGridAccess(AOwnedGroup.PivotGrid).DataBuilder.Columns;
    AColumnAxisGroups := TcxCustomPivotGridAccess(AOwnedGroup.PivotGrid).DataBuilder.Rows;
    ARowAxisFields := AColumns;
    AColumnAxisFields := ARows;
  end;
  PopulateColumnMembers(AColumnMembers, AColumnAxisGroups, AColumnAxisFields);
  PopulateMeasures(AMeasureMembers, AMeasures);
  PopulateRowMembers(ARowMembers, ARowAxisGroups, ARowAxisFields);
end;

procedure TcxOLAPDataQueryBuilder.ExcludeExpandingMembersFormFilters(AnExpandedMembers: TList;
  AFilters: TcxPivotGridFields);
var
  I: Integer;
begin
  if AnExpandedMembers <> nil then
    for I := 0 to AnExpandedMembers.Count - 1 do
      ExcludeField(AFilters, TcxPivotGridOLAPField(TcxPivotGridGroupItem(AnExpandedMembers[I]).Field));
end;

procedure TcxOLAPDataQueryBuilder.ExcludeField(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField);
var
  I: Integer;
begin
  if (AFilters <> nil) and (AField <> nil) then
    for I := AFilters.Count - 1 downto 0 do
      if TcxPivotGridOLAPField(AFilters[I]).HierarchyUniqueName = AField.HierarchyUniqueName then
        AFilters.Delete(I);
end;

function TcxOLAPDataQueryBuilder.GetAxis: string;
begin
  Result := ' on ' + IntToStr(FAxisIndex);
  Inc(FAxisIndex);
end;

function TcxOLAPDataQueryBuilder.GetExpandedMembers(AnExpandedMembers: TList; AClearMember: Boolean = False): string;

  function GetMemberString(Index: Integer): string;
  var
    AnItemName: string;
  begin
    AnItemName := TcxPivotGridGroupItem(AnExpandedMembers[Index]).UniqueName;
    if IsNotLastVisibleOLAPHierarchyItem(TcxPivotGridGroupItem(AnExpandedMembers[Index])) and not AClearMember then
      Result := WriteSortedMembers(TcxPivotGridOLAPField(TcxPivotGridGroupItem(
         AnExpandedMembers[Index]).Field), 'addcalculatedmembers(' + AnItemName + '.children)')
    else
      Result := AnItemName;
  end;

var
  I: Integer;
begin
  Result := '';
  if (AnExpandedMembers <> nil) and (AnExpandedMembers.Count > 0) then
  begin
    for I := 0 to AnExpandedMembers.Count - 1 do
    begin
      if I = 0 then
        Result := '{' + GetMemberString(I) + '}'
      else
        Result := Result + ' * {' + GetMemberString(I) + '}';
    end;
  end;
end;

function TcxOLAPDataQueryBuilder.GetFilterString(AFilters: TcxPivotGridFields; AFilter: TcxPivotGridOLAPField): string;
begin
  Result := Format('{%s}', [WriteAllMembersCore(AFilters, AFilter, False, True)]);
end;

function TcxOLAPDataQueryBuilder.GetMembers(AFilter: TStrings): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to AFilter.Count - 1 do
    if I = 0 then
      Result := AFilter[I]
    else
      Result := Format('%s, %s', [Result, AFilter[I]]);
end;

function TcxOLAPDataQueryBuilder.GetMeasures(AMeasures: TcxPivotGridFields; const AAdditionalString: string = ''): string;
var
  I: Integer;
begin
  Result := '';
  if AAdditionalString <> '' then
    Result := AAdditionalString;
  if AMeasures <> nil then
    for I := 0 to AMeasures.Count - 1 do
      if Result = '' then
        Result := TcxPivotGridOLAPField(AMeasures[I]).UniqueName
      else
        Result := Format('%s, %s', [Result, TcxPivotGridOLAPField(AMeasures[I]).UniqueName]);
  if Result <> '' then
    Result := Format('{ %s }', [Result]);
end;

function TcxOLAPDataQueryBuilder.GetNamedSetName(AField: TcxPivotGridOLAPField): string;
begin
  Result := '{[' + AField.UniqueName + ']}';
end;

function TcxOLAPDataQueryBuilder.GetNamedSetsQuery(ANamedSets: TList): string;
var
  I: Integer;
begin
  Result := 'select ';
  for I := 0 to ANamedSets.Count - 1 do
    Result := Result +  GetNamedSetName(TcxPivotGridOLAPField(ANamedSets[I])) +
      ' ON ' + IntToStr(I) + ', ';
  TrimTrailingComma(Result);
end;

function TcxOLAPDataQueryBuilder.IsHierarchy(AField: TcxPivotGridOLAPField;
  AParentField: TcxPivotGridOLAPField): Boolean;
begin
  Result := (AField.Group <> nil) and (AParentField.Group = AField.Group);
end;

function TcxOLAPDataQueryBuilder.GetSortOrder(AField: TcxPivotGridOLAPField): string;
begin
  if AField.SortOrder = soDescending then
    Result := 'desc'
  else
    Result := 'asc';
end;

function TcxOLAPDataQueryBuilder.GetTuple(ATuple: TcxPivotGridGroupItem): string;
begin
  if ATuple.Parent <> nil then
  begin
    Result := ' )';
    while ATuple.Parent <> nil do
    begin
      Result := Comma + ATuple.UniqueName + Result;
      ATuple := ATuple.Parent;
    end;
    Delete(Result, 1, 2);
    Result := '( ' + Result;
  end
  else
    Result := '';
end;

function TcxOLAPDataQueryBuilder.IsFiltered(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField): Boolean;
var
  I: Integer;
begin
  Result := AField.Filter.HasFilter;
  I := 0;
  while (AFilters <> nil) and (I < AFilters.Count) and not Result do
  begin
    Result := (AField.Group <> nil) and (AFilters[I].Group = AField.Group) and
      AFilters[I].Filter.HasFilter;
    Inc(I);
  end;
end;

function TcxOLAPDataQueryBuilder.IsOLAPVersion8: Boolean;
begin
  Result := Version <= 8;
end;

function TcxOLAPDataQueryBuilder.MakeFilterFields(
  AFilters: TcxPivotGridFields): TcxPivotGridFields;
begin
  if AFilters = nil then
    Result := nil
  else
    Result := TcxPivotGridFields.Create;
end;


function TcxOLAPDataQueryBuilder.WriteAllMembersWithSorting(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField;
  IncludeCalculatedMembers: Boolean): string;
begin
  Result := '{ ' + WriteSortedMembers(AField,
    WriteAllMembersCore(AFilters, AField, IncludeCalculatedMembers, True)) + '} ';
end;

function TcxOLAPDataQueryBuilder.WriteAllMembersCore(AFilters: TcxPivotGridFields; AField: TcxPivotGridOLAPField;
  IncludeCalculatedMembers: Boolean; AFilter: Boolean): string;
var
  AList: TStrings;
begin
  Result := '';
  if IsFiltered(AFilters, AField) and AFilter then
  begin
    AList := TcxPivotGridOLAPField.PopulateFilteredValues(AField);
    try
      Result := Result + GetMembers(AList);
    finally
      AList.Free;
    end;
  end
  else
  begin
    if IncludeCalculatedMembers then
      Result := Result + 'addcalculatedmembers({';
    if AField.FieldType <> oftSet then
      Result := Result + AField.LevelUniqueName + '.members'
    else
      Result := Result + GetNamedSetName(AField);
    if IncludeCalculatedMembers then
      Result := Result + '})';
  end;
end;

function TcxOLAPDataQueryBuilder.WriteSortedMembers(AField: TcxPivotGridOLAPField;
  const AContent: string): string;
begin
  if not TcxPivotGridOLAPFieldOptionsAccess(AField.Options).CanSorting then
    Result := AContent
  else
    Result := 'order({' + AContent + '}, ' + GetSortBy(AField) + Comma + GetSortOrder(AField) + ')';
end;

function TcxOLAPDataQueryBuilder.ColumnPartVersion8(const ACubeName: string; AColumns: TcxPivotGridFields;
  AMeasures, AFilters: TcxPivotGridFields; IncludeCalculatedMembers, IncludeTotals: Boolean;
  const AAdditionalString: string = ''): string;
begin
  Result := '';
  if ((AMeasures <> nil) and (AMeasures.Count = 0) or
    (AMeasures = nil) and (AColumns.Count = 0)) and (AAdditionalString = '') then
    Exit;

  Result := Result + 'select non empty { ' + AAdditionalString;
  if AColumns.Count > 0 then
    Result := Result + WriteAllMembersWithSorting(AFilters,
      TcxPivotGridOLAPField(AColumns[0]), IncludeCalculatedMembers) + CrossJoinOperator;
  if AMeasures = nil then
    TrimTrailingComma(Result)
  else
    Result := Result + GetMeasures(AMeasures);
  Result := Result + '}' + GetAxis;
end;

function TcxOLAPDataQueryBuilder.ColumnPart(const ACubeName: string; AColumns, AMeasures, AFilters: TcxPivotGridFields;
  IncludeCalculatedMembers, IncludeTotals, IsFirstMember: Boolean; const AAdditionalString: string): string;

  function GetMember(AField: TcxPivotGridOLAPField): string;
  begin
    Result := ' member Measures.MemberUniqueName as ' + AField.HierarchyUniqueName + '.MEMBER_UNIQUE_NAME ';
  end;

var
  AMember: string;
begin
  Result := '';
  if ((AMeasures <> nil) and (AMeasures.Count = 0) or
    (AMeasures = nil) and (AColumns.Count = 0)) and (AAdditionalString = '') then
    Exit;

  if AColumns.Count > 0 then
  begin
    if IsFirstMember then
      Result := 'with';
    Result := Result + GetMember(TcxPivotGridOLAPField(AColumns[0]));
  end;

  Result := Result + ' select';

  if AMeasures <> nil then
  begin
    if AColumns.Count > 0 then
      AMember := 'Measures.MemberUniqueName'
    else
      AMember := '';
    Result := Result + ' non empty' + GetMeasures(AMeasures, AMember) + GetAxis + ', ';
  end;

  if (AAdditionalString = '') and (AColumns.Count = 0) then
    TrimTrailingComma(Result)
  else
  begin
    Result := Result + ' non empty{ ' + AAdditionalString;
    if AColumns.Count > 0 then
    begin
      if AAdditionalString <> '' then
        Result := Result + CrossJoinOperator;
      Result := Result + WriteAllMembersWithSorting(AFilters, TcxPivotGridOLAPField(AColumns[0]),
        IncludeCalculatedMembers)
    end;
    Result := Result + '}';
    if AAdditionalString <> '' then
      Result := Result + 'dimension properties member_unique_name';
    Result := Result + GetAxis;
  end;
end;

function TcxOLAPDataQueryBuilder.SourcePart(const ACubeName: string;
  AFilters: TcxPivotGridFields; ANamedSets: TList): string;
begin
  Result := SourcePartWithSubSelect(ACubeName, AFilters, ANamedSets);
end;

function TcxOLAPDataQueryBuilder.SourcePartWithSubSelect(const ACubeName: string;
  AFilters: TcxPivotGridFields; ANamedSets: TList): string;
var
  I, AxisIndex, AParenthesisCount: Integer;
  AList: TStrings;
  ASubSelect: string;
  ASubSelectIsCorrect: Boolean;
begin
  Result := ' ';
  ASubSelectIsCorrect := False;
  AParenthesisCount := 0;
  if (AFilters <> nil) and (AFilters.Count > 0) then
  begin
    ASubSelect := '(select ';
    AxisIndex := 0;
    for I := 0 to AFilters.Count - 1 do
    begin
      if (AFilters[I].Group <> nil) and (I + 1 < AFilters.Count) and
        (AFilters[I].Group = AFilters[I + 1].Group) then
        Continue;
      AList := TcxPivotGridOLAPField.PopulateFilteredValues(AFilters[I]);
      if AList.Count > 0 then
        ASubSelectIsCorrect := True;
      try
        ASubSelect := ASubSelect + '{' + GetMembers(AList) + '} on ' + IntToStr(AxisIndex) + Comma;
        Inc(AxisIndex);
      finally
        AList.Free;
      end;
    end;
    TrimTrailingComma(ASubSelect);
    ASubSelect := ASubSelect + ' ';
    if ASubSelectIsCorrect then
    begin
      Result := Result + ASubSelect + ' from';
      Inc(AParenthesisCount);
    end;
  end;
  if (ANamedSets <> nil) and (ANamedSets.Count > 0) then
  begin
    Result := Result + '(' + GetNamedSetsQuery(ANamedSets);
    Inc(AParenthesisCount);
  end;
  Result := Result + ' [' + ACubeName + ']';
  for I := 0 to AParenthesisCount - 1 do
    Result := Result + ')';
end;

procedure TcxOLAPDataQueryBuilder.TrimTrailingComma(var AStr: string);
begin
  TrimTrailingSequence(AStr, Comma);
end;

procedure TcxOLAPDataQueryBuilder.TrimTrailingSequence(var AStr: string; const ASequence: string);
var
  ASequenceLength: Integer;
begin
  ASequenceLength := Length(ASequence);
  Delete(AStr, Length(AStr) - ASequenceLength + 1, ASequenceLength);
end;

function TcxOLAPDataQueryBuilder.GetSortBy(AField: TcxPivotGridOLAPField): string;
begin
  Result := AField.HierarchyUniqueName;
  case Version of
    7, 8:
      begin
        Result := Result + '.currentmember.properties("';
        case AField.SortMode of
          osmValue, osmDisplayText: Result := Result + 'caption")';
          osmKey: Result := Result + 'key")';
        else
          Result := Result + 'id")';
        end;
      end;
  else
    Result := Result + '.currentmember.';
    case AField.SortMode of
      osmValue: Result := Result + 'member_value';
      osmDisplayText: Result := Result + 'member_caption';
      osmKey: Result := Result + 'member_key';
    else
      Result := Result + 'properties(''id'', typed)';
    end;
  end;
end;

{ TcxOLEDBOLAPDataQueryBuilder }

function TcxOLEDBOLAPDataQueryBuilder.GetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
  AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields): TcxMDXQuery;

  procedure PopulateWithClauses(AWithClauses: TStrings; AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;
    out ANames: TStringDynArray);
  var
    S, AUniqueName: string;
    I, AStartIndex, AIndex: Integer;
    AField: TcxPivotGridOLAPField;
  begin
    SetLength(ANames, AColumnMembers.FieldCount + ARowMembers.FieldCount);
    AIndex := 0;

    for I := 0 to AColumnMembers.FieldCount - 1 do
    begin
      AField := AColumnMembers.Fields[I] as TcxPivotGridOLAPField;
      if (I > 0) and SameHierarchy(AField, AColumnMembers.Fields[I - 1]) then
        Continue;
      AUniqueName := '[Measures].[DX_COLUMN_' + IntToStr(AIndex) + '_UNIQUE_NAME]';
      S := 'SetToStr(Ascendants(' + AField.HierarchyUniqueName + '.currentmember))';
      AWithClauses.Add('MEMBER ' + AUniqueName + ' AS ' + QuotedStr(S));
      ANames[AIndex] := AUniqueName;
      Inc(AIndex);
    end;

    AStartIndex := AIndex;
    for I := 0 to ARowMembers.FieldCount - 1 do
    begin
      AField := ARowMembers.Fields[I] as TcxPivotGridOLAPField;
      if (I > 0) and SameHierarchy(AField, ARowMembers.Fields[I - 1]) then
        Continue;
      AUniqueName := '[Measures].[DX_ROW_' + IntToStr(AIndex - AStartIndex) + '_UNIQUE_NAME]';
      S := 'SetToStr(Ascendants(' + AField.HierarchyUniqueName + '.currentmember))';
      AWithClauses.Add('MEMBER ' + AUniqueName + ' AS ' + QuotedStr(S));
      ANames[AIndex] := AUniqueName;
      Inc(AIndex);
    end;
    SetLength(ANames, AIndex);
  end;

var
  ANames: TStringDynArray;
  AMeasureAxis, AColumnAxis, ARowAxis: string;
  AMeasureMembers, AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;
begin
  AMeasureMembers := TcxMDXQueryAxisMembers.Create;
  AColumnMembers := TcxMDXQueryAxisMembers.Create;
  ARowMembers := TcxMDXQueryAxisMembers.Create;
  DoGetCrossCellsQuery(ACubeName, AOwnedGroup, AColumns, ARows, AMeasures, AFilters,
    AColumnMembers, AMeasureMembers, ARowMembers);

  Result := TcxMDXQuery.Create;
  PopulateWithClauses(Result.WithClauses, AColumnMembers, ARowMembers, ANames);
  AMeasureAxis := Format('non empty{ %s, %s }',
    [Implode(',', ANames), AMeasureMembers.ToQuery]);
  Result.AddAxisClause(AMeasureAxis, AMeasureMembers);
  AColumnAxis := Format('non empty{ %s } dimension properties MEMBER_UNIQUE_NAME', [AColumnMembers.ToQuery]);
  Result.AddAxisClause(AColumnAxis, AColumnMembers);

  if ARowMembers.MemberCount > 0 then
  begin
    ARowAxis := Format('non empty{ %s } dimension properties MEMBER_UNIQUE_NAME', [ARowMembers.ToQuery]);
    Result.AddAxisClause(ARowAxis, ARowMembers);
  end
  else
    FreeAndNil(ARowMembers);

  Result.SetSubcubeClause(SourcePart(ACubeName, AFilters, nil));
end;

{ TcxADOMDOLAPDataQueryBuilder }

constructor TcxADOMDOLAPDataQueryBuilder.Create;
const
  Version = 10;
begin
  inherited Create(Version);
end;

function TcxADOMDOLAPDataQueryBuilder.GetCrossCellsQuery(const ACubeName: string; AOwnedGroup: TcxPivotGridGroupItem;
  AColumns, ARows, AMeasures, AFilters: TcxPivotGridFields): TcxMDXQuery;
var
  AColumnAxis, ARowAxis: string;
  AColumnMembers, ARowMembers: TcxMDXQueryAxisMembers;
begin
  AColumnMembers := TcxMDXQueryAxisMembers.Create;
  ARowMembers := TcxMDXQueryAxisMembers.Create;
  DoGetCrossCellsQuery(ACubeName, AOwnedGroup, AColumns, ARows, AMeasures, AFilters,
    AColumnMembers, AColumnMembers, ARowMembers);

  Result := TcxMDXQuery.Create;
  AColumnAxis := Format('non empty{ %s }', [AColumnMembers.ToQuery]);
  Result.AddAxisClause(AColumnAxis, AColumnMembers);

  if ARowMembers.MemberCount > 0 then
  begin
    ARowAxis := Format('non empty{ %s } dimension properties MEMBER_UNIQUE_NAME', [ARowMembers.ToQuery]);
    Result.AddAxisClause(ARowAxis, ARowMembers);
  end
  else
    FreeAndNil(ARowMembers);

  Result.SetSubcubeClause(SourcePart(ACubeName, AFilters, nil));
end;

function TcxADOMDOLAPDataQueryBuilder.GetTotalsQueryString(const ACubeName: string; AColumns: TcxPivotGridFields;
  AnExpandedMembers: TList; AMeasures, AFilters: TcxPivotGridFields): string;
var
  AQuery: TcxMDXQuery;

  function ColumnPart(const ACubeName: string; AColumns, AMeasures, AFilters: TcxPivotGridFields;
    IncludeCalculatedMembers, IncludeTotals: Boolean; const AAdditionalString: string): string;
  var
    AMember: string;
  begin
    Result := '';
    if ((AMeasures <> nil) and (AMeasures.Count = 0) or
      (AMeasures = nil) and (AColumns.Count = 0)) and (AAdditionalString = '') then
      Exit;

    if AColumns.Count > 0 then
      AQuery.AddWithClause(' member Measures.MemberUniqueName as ' +
        TcxPivotGridOLAPField(AColumns[0]).HierarchyUniqueName + '.MEMBER_UNIQUE_NAME ');

    if (AAdditionalString <> '') or (AColumns.Count > 0) then
    begin
      Result := AAdditionalString;
      if AColumns.Count > 0 then
        Result := Result + WriteAllMembersWithSorting(AFilters, TcxPivotGridOLAPField(AColumns[0]),
          IncludeCalculatedMembers);
      if AMeasures <> nil then
        Result := Format('%s * %s', [Result, GetMeasures(AMeasures, AMember)]);

      if AAdditionalString = '' then
        Result := Format('non empty{ %s }', [Result])
      else
        Result := Format('non empty{ %s } dimension properties member_unique_name', [Result])
    end;
  end;

begin
  Result := '';
  if AMeasures.Count = 0 then
    Exit;
  AQuery := TcxMDXQuery.Create;
  try
    if (AnExpandedMembers <> nil) and IsNotLastVisibleOLAPHierarchyItem(TcxPivotGridGroupItem(AnExpandedMembers[0])) then
      AQuery.AddAxisClause(ColumnPart(ACubeName, AColumns, AMeasures, AFilters, True, True,
        GetExpandedMembers(AnExpandedMembers)))
    else
    begin
      AQuery.AddAxisClause(ColumnPart(ACubeName, AColumns, AMeasures, AFilters, True, True, ''));
      if AnExpandedMembers <> nil then
        AQuery.AddAxisClause(GetExpandedMembers(AnExpandedMembers));
    end;
    AQuery.SetSubcubeClause(SourcePart(ACubeName, AFilters, nil));
    Result := AQuery.GetQuery;
  finally
    AQuery.Free;
  end;
end;

{ TcxMDXQueryAxisMembers.TMember }

constructor TcxMDXQueryAxisMembers.TMember.Create;
begin
  inherited Create;
  FItems := TList<TcxPivotGridGroupItem>.Create;
end;

destructor TcxMDXQueryAxisMembers.TMember.Destroy;
begin
  FItems.Free;
  inherited Destroy;
end;

function TcxMDXQueryAxisMembers.TMember.Add(AItem: TcxPivotGridGroupItem): Integer;
begin
  Result := FItems.Add(AItem);
end;

function TcxMDXQueryAxisMembers.TMember.Count: Integer;
begin
  Result := FItems.Count;
end;

function TcxMDXQueryAxisMembers.TMember.Last: TcxPivotGridGroupItem;
begin
  Result := Items[Count - 1];
end;

type
  TcxPivotGridOLAPFieldAccess = class(TcxPivotGridOLAPField);

function TcxMDXQueryAxisMembers.TMember.GetCoordinate(AFields: TFields): TStringDynArray;

  function GetAllMemberUniqueName(AField: TcxPivotGridOLAPField): string;
  begin
    Result := TcxPivotGridOLAPFieldAccess(AField).AllMemberUniqueName;
  end;

var
  I, AIndex: Integer;
begin
  Assert(AFields.Count >= FItems.Count);
  AIndex := 0;
  SetLength(Result, AFields.Count);
  for I := 0 to AFields.Count - 1 do
  begin
    if (I > 0) and not TcxCustomOLAPDataQueryBuilder.SameHierarchy(AFields[I - 1], AFields[I]) then
      Inc(AIndex);
    if I < Count then
      Result[AIndex] := Items[I].UniqueName
    else
      if not TcxCustomOLAPDataQueryBuilder.SameHierarchy(AFields[I - 1], AFields[I]) then
        Result[AIndex] := GetAllMemberUniqueName(AFields[I]);
  end;
  SetLength(Result, AIndex + 1);
end;

function TcxMDXQueryAxisMembers.TMember.ToQuery(AFields: TFields): string;
begin
  Result := Implode(', ', GetCoordinate(AFields));
end;

function TcxMDXQueryAxisMembers.TMember.GetMember(Index: Integer): TcxPivotGridGroupItem;
begin
  Result := FItems[Index];
end;

{ TcxMDXQueryAxisMembers }

constructor TcxMDXQueryAxisMembers.Create;
begin
  inherited Create;
  FFields := TFields.Create;
  FMeasures := TFields.Create;
  FMembers := TMembers.Create;
end;

destructor TcxMDXQueryAxisMembers.Destroy;
begin
  FMembers.Free;
  FMeasures.Free;
  FFields.Free;
  inherited Destroy;
end;

procedure TcxMDXQueryAxisMembers.AddField(AField: TcxPivotGridOLAPField);
begin
  FFields.Add(AField);
end;

procedure TcxMDXQueryAxisMembers.AddMeasure(AField: TcxPivotGridOLAPField);
begin
  FMeasures.Add(AField);
end;

function TcxMDXQueryAxisMembers.AddMember: TMember;
begin
  Result := TMember.Create;
  FMembers.Add(Result);
end;

function TcxMDXQueryAxisMembers.FieldCount: Integer;
begin
  Result := FFields.Count;
end;

function TcxMDXQueryAxisMembers.GetMemberCoordinate(AMemberIndex: Integer): TStringDynArray;
begin
  Result := Members[AMemberIndex].GetCoordinate(FFields);
end;

function TcxMDXQueryAxisMembers.LastMember: TMember;
begin
  Result := FMembers.Last;
end;

function TcxMDXQueryAxisMembers.GetField(Index: Integer): TcxPivotGridField;
begin
  Result := FFields[Index];
end;

function TcxMDXQueryAxisMembers.GetMeasure(Index: Integer): TcxPivotGridField;
begin
  Result := FMeasures[Index];
end;

function TcxMDXQueryAxisMembers.GetMember(Index: Integer): TMember;
begin
  Result := FMembers[Index];
end;

function TcxMDXQueryAxisMembers.MeasuresToQuery: string;
var
  I: Integer;
  AStrings: TStrings;
begin
  AStrings := TStringList.Create;
  try
    for I := 0 to MeasureCount - 1 do
      AStrings.Add(Measures[I].UniqueName);
    Result := Implode(', ', AStrings);
  finally
    AStrings.Free;
  end;
end;

function TcxMDXQueryAxisMembers.MeasureCount: Integer;
begin
  Result := FMeasures.Count;
end;

function TcxMDXQueryAxisMembers.MemberCount: Integer;
begin
  Result := FMembers.Count;
end;

function TcxMDXQueryAxisMembers.ToQuery: string;
var
  S: string;
  I: Integer;
  AStrings: TStrings;
begin
  Result := '';
  AStrings := TStringList.Create;
  try
    for I := 0 to MemberCount - 1 do
    begin
      S := Members[I].ToQuery(FFields);
      if S > '' then
        AStrings.Add(Format('(%s)', [S]));
    end;
    Result := Implode(', ', AStrings);
  finally
    AStrings.Free;
  end;
  if MeasureCount > 0 then
    if Result = '' then
      Result := Format('{%s}', [MeasuresToQuery])
    else
      Result := Format('{%s} * {%s}', [Result, MeasuresToQuery]);
end;

{ TcxMDXQuery }

constructor TcxMDXQuery.Create;
begin
  inherited Create;
  FGarbageCollector := TObjectList.Create;
  FAxisClauses := TStringList.Create;
  FWhereExpressions := TStringList.Create;
  FWithClauses := TStringList.Create;
end;

destructor TcxMDXQuery.Destroy;
begin
  FWithClauses.Free;
  FWhereExpressions.Free;
  FAxisClauses.Free;
  FGarbageCollector.Free;
  inherited Destroy;
end;

function TcxMDXQuery.TryGetMembers(AAxis: Integer; out AMembers: TcxMDXQueryAxisMembers): Boolean;
begin
  Result := AxisClauses.Count > AAxis;
  if not Result then
    Exit;
  AMembers := AxisClauses.Objects[AAxis] as TcxMDXQueryAxisMembers;
  Result := AMembers <> nil;
end;

procedure TcxMDXQuery.AddAxisClause(const AClause: string; AMembers: TcxMDXQueryAxisMembers = nil);
begin
  if AMembers <> nil then
    FGarbageCollector.Add(AMembers);
  FAxisClauses.AddObject(AClause, AMembers);
end;

procedure TcxMDXQuery.AddWithClause(const AClause: string);
begin
  FWithClauses.Add(AClause);
end;

procedure TcxMDXQuery.AddWhereExpression(const AExpression: string);
begin
  FWhereExpressions.Add(AExpression);
end;

procedure TcxMDXQuery.SetSubcubeClause(const AClause: string);
begin
  Assert(FSubcubeClause = '');
  FSubcubeClause := AClause;
end;

function TcxMDXQuery.GetQuery: string;

  function GetWithClauses: string;
  begin
    Result := Implode(' ', WithClauses);
  end;

  function GetAxisClauses: string;
  var
    I: Integer;
  begin
    Result := '';
    for I := 0 to AxisClauses.Count - 1 do
    begin
      if I = 0 then
        Result := Format('%s on %d', [AxisClauses[I], I])
      else
        Result := Format('%s, %s on %d', [Result, AxisClauses[I], I]);
    end;
  end;

  function GetWhereExpressions: string;
  begin
    Result := Implode(', ', WhereExpressions);
  end;

begin
  Result := '';
  if WithClauses.Count > 0 then
    Result := Format('WITH %s', [GetWithClauses]);
  if AxisClauses.Count > 0 then
    Result := Format('%s SELECT %s', [Result, GetAxisClauses]);
  Result := Format('%s FROM %s', [Result, SubcubeClause]);
  if WhereExpressions.Count > 0 then
    Result := Format('%s WHERE (%s)', [Result, GetWhereExpressions]);
end;

end.

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

unit dxEMF.DB.Firebird;

{$HPPEMIT '#pragma link "dxEMF.DB.Firebird"'}

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Collections, DB, Rtti, Variants, dxCore,
  dxEMF.Types,
  dxEMF.DB.Model,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type
  { TdxFirebirdConnectionProvider }

  TdxFirebirdConnectionProvider = class(TdxSQLConnectionProvider)
  strict private const
    StopChars: array [0 .. 1] of Char = ('_', '%');
  strict private type

    TVersion = record
    private
      procedure Empty;
    public
      HasValue: Boolean;
      MajorHi, MajorLo, Minor: Byte;
      constructor Create(const AVersion: string);
      function Support(AMajorHi, AMajorLo: Byte; AMinor: Byte = 0): Boolean;
    end;

  strict private
    FCaseSensitiveFieldNames: Boolean;
    FVersion: TVersion;
    class var
      FConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
      FDBTypes: TDictionary<string, TdxDBColumnType>;
    function GetVersion: TVersion;

    class destructor Destroy;
    class function GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>; static;
    class procedure PopulateDBTypes; static; inline;
  public const
    ProviderTypeString = 'Firebird';
    MaximumStringSize = 4000;
  protected
    function ConvertToDBParameter(const AParameterValue: TValue): TValue; override;
    class function GetColumnTypeByInfo(AType, ASubType, AScale, ASize: SmallInt): TdxDBColumnType;
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; override;
    procedure ReadDbVersion;

    function GetMaximumStringSize: Integer; override;
    function GetFunctionStrLenName: string;
    function GetSafeNameTableMaxLength: Integer; override;

    procedure GetColumns(ATable: TdxDBTable);
    procedure GetIndexes(ATable: TdxDBTable);
    procedure GetPrimaryKey(ATable: TdxDBTable);
    function GetSeqName(ATable: TdxDBTable): string;
    function GetFieldStringValue(AField: TField): string;
    function GetFieldValue(AField: TField): Variant;
    function GetFieldValueDef(AField: TField; const ADefValue: Variant): Variant;
    procedure GetForeignKeys(ATable: TdxDBTable);

    function GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64; override;
    function IsSequenceExists(const ASequenceName: string): Boolean;

    function GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;

    property CaseSensitiveFieldNames: Boolean read FCaseSensitiveFieldNames;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); override;
    class function GetDBEngine: TdxDBEngine; override;

    procedure CreateTable(ATable: TdxDBTable); override;
    procedure CreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex); override;
    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatColumn(const AColumnName: string): string; override;
    function FormatColumn(const AColumnName, ATableAlias: string): string; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      const AOperands: TArray<TValue>): string; override;
    function FormatTable(const ATableName: string): string; overload;
    function FormatTable(const ASchemaName, ATableName: string): string; override;
    function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; override;
    function FormatSelect(const ASelectedPropertiesSQL, AFromSQL, AWhereSQL, AOrderBySQL, AGroupBySQL, AHavingSQL: string;
      ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string; override;
    function FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string; override;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; override;
    function FormatDelete(const ATableName, AWhereClause: string): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean); override;

    function SupportsNativeSkipTake: Boolean; override;
    function SupportsNativeOuterApply: Boolean; override;

    class property ConnectionParameters: TDictionary<string, TdxConnectionParameterValues> read GetConnectionParameters;
    property Version: TVersion read GetVersion;
  end;

implementation

uses
  StrUtils, TypInfo,
  dxStringHelper,
  dxEMF.DB.SQLGenerator,
  dxEMF.Utils,
  dxEMF.DB.Utils;

type
  TdxConnectionVendorAccess = class(TdxCustomConnectionVendor);

{ TdxFirebirdConnectionProvider.TVersion }

constructor TdxFirebirdConnectionProvider.TVersion.Create(const AVersion: string);
var
  AVersionParts: TArray<string>;
begin
  AVersionParts := {$IFDEF DELPHIXE3}AVersion.Split(['.']){$ELSE}TdxStringHelper.Split(AVersion, ['.']){$ENDIF};
  if Length(AVersionParts) = 3 then
  try
    MajorHi := StrToInt(AVersionParts[0]);
    MajorLo := StrToInt(AVersionParts[1]);
    Minor := StrToInt(AVersionParts[2]);
    HasValue := True;
  except
    Empty;
  end
  else
    Empty;
end;

procedure TdxFirebirdConnectionProvider.TVersion.Empty;
begin
  HasValue := False;
  MajorHi := 0;
  MajorLo := 0;
  Minor := 0;
end;

function TdxFirebirdConnectionProvider.TVersion.Support(AMajorHi, AMajorLo, AMinor: Byte): Boolean;
begin
  if not HasValue then
    Exit(False);
  Result := (MajorHi >= AMajorHi) and (MajorLo >= AMajorLo) and (Minor >= AMinor);
end;

{ TdxFirebirdConnectionProvider }

constructor TdxFirebirdConnectionProvider.Create(
  AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited Create(AConnectionVendor, AAutoCreateOption);
  FCaseSensitiveFieldNames := TdxConnectionVendorAccess(AConnectionVendor).GetCaseSensitiveFieldNames;
  FVersion.Empty;
end;

class function TdxFirebirdConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.Firebird;
end;

function TdxFirebirdConnectionProvider.FormatBinary(
  AOperatorType: TdxBinaryOperatorType; const ALeftOperand: string; const ARightOperand: string): string;
const
  AOperatorFormat: array[TdxBinaryOperatorType] of string  = ('', '', '', '',  '',  '',
    'bin_and(%s, %s)', 'bin_or(%s, %s)',  'bin_xor(%s, %s)',  '', 'mod(%s, %s)',  '',  '',  '');
begin
  if AOperatorFormat[AOperatorType] <> '' then
    Result := Format(AOperatorFormat[AOperatorType], [ALeftOperand, ARightOperand])
  else
    Result := inherited FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

procedure TdxFirebirdConnectionProvider.CreateTable(ATable: TdxDBTable);
var
  AKey: TdxDBColumn;
  ASequenceName: string;
begin
  inherited CreateTable(ATable);
  if (ATable.PrimaryKey <> nil) and (ATable.PrimaryKey.Columns.Count > 0) then
  begin
    AKey := ATable.GetColumn(ATable.PrimaryKey.Columns[0]);
    ASequenceName := GetSeqName(ATable);
    if AKey.IsIdentity and not IsSequenceExists(ASequenceName) then
      ExecuteSQLSchemaUpdate('AutoInc', 'Key', '', 'create generator ' + FormatTable(ASequenceName));
  end;
end;

procedure TdxFirebirdConnectionProvider.CreateIndex(ATable: TdxDBTable; AIndex: TdxDBIndex);
begin
  if ATable.Name <> 'XPObjectType' then
    inherited CreateIndex(ATable, AIndex);
end;

function TdxFirebirdConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  if not CaseSensitiveFieldNames then
    Result := '"' + AColumnName + '"'
  else
    Result := '"' + UpperCase(AColumnName) + '"';
end;

function TdxFirebirdConnectionProvider.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := ATableAlias + '.' + FormatColumn(AColumnName);
end;

function TdxFirebirdConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  if not CaseSensitiveFieldNames then
    Result := '"' + AConstraintName + '"'
  else
    Result := Uppercase(AConstraintName)
end;

function TdxFirebirdConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType;
  const AOperands: TArray<string>): string;
var
  AIndex: Integer;
  AStringBuilder: TStringBuilder;
begin
  case AOperatorType of
    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1: Result := Format('((%s) is null)', [AOperands[0]]);
        2: Result := Format('COALESCE(%s, %s)', [AOperands[0], AOperands[1]]);
      else
         Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%0:s) is null or (%0:s) = '#$27#$27')', [AOperands[0]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('Sqrt(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1: Result := Format('Ln(%s)', [AOperands[0]]);
        2: Result := Format('log(%1:s, %0:s)', [AOperands[0], AOperands[1]]);
        else
          Result := inherited FormatFunction(AOperatorType, AOperands);
      end;
    TdxFunctionOperatorType.Log10:
      Result := Format('log(10, %s})', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('acos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('asin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('atan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('atan2(%s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('cosh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('sinh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('tanh(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Max:
      Result := Format('iif(%0:s > %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Min:
      Result := Format('iif(%0:s < %1:s, %0:s, %1:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Random:
      Result := 'Rand()';
    TdxFunctionOperatorType.BigMul:
      Result := Format('(cast(%s as bigint) * cast(%s as bigint))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondOf:
      Result := Format('(cast(extract(second from %0:s) - floor(extract(second from %0:s)) as numeric(4, 4)) * 1000)', [AOperands[0]]);
    TdxFunctionOperatorType.SecondOf:
      Result := Format('floor(extract(second from %s))', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('extract(minute from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('extract(hour from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('extract(day from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('extract(month from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('extract(year from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('floor(extract(hour from %0:s) * 36000000000 + extract(minute from %0:s) * 600000000 + extract(second from {0}) * 10000000)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('extract(weekday from %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('(extract(yearday from %s) + 1)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('cast(cast(%s as date) as timestamp)', [AOperands[0]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('ascii_val(substring(cast(%s as varchar(8191)) from 1 for 1))', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('ascii_char(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInteger:
      Result := Format('cast(%s as integer)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      Result := Format('cast(%s as bigint)', [AOperands[0]]);
    TdxFunctionOperatorType.ToSingle:
      Result := Format('cast(%s as float)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDouble:
      Result := Format('cast(%s as double precision)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('cast(%s as decimal(18,4))', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format('cast(%s as varchar(8191))', [AOperands[0]]);
    TdxFunctionOperatorType.Len:
      Result := Format(GetFunctionStrLenName + '(trim(%s))', [AOperands[0]]);
    TdxFunctionOperatorType.Trim:
      Result := Format('trim(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.PadLeft:
      case Length(AOperands) of
        2: Result := Format('iif(%2:s(%0:s) > %1:s, %0:s, lpad(%0:s, %0:s, '#$27' '#$27'))', [AOperands[0], AOperands[1], GetFunctionStrLenName]);
        3: Result := Format('iif(%3:s(%0:s) > %0:s, %0:s, lpad(%0:s, %1:s, %2:s))', [AOperands[0], AOperands[1], AOperands[2], GetFunctionStrLenName])
        else
          raise ENotSupportedException.Create('');
      end;
    TdxFunctionOperatorType.PadRight:
      case Length(AOperands) of
        2: Result := Format('iif(%2:s(%0:s) > %1:s, %0:s, rpad(%0:s, %1:s, '#$27' '#$27'))', [AOperands[0], AOperands[1], GetFunctionStrLenName]);
        3: Result := Format('iif(%3:s(%0:s) > %0:s, %0:s, rpad(%0:s, %1:s, %2:s))', [AOperands[0], AOperands[1], AOperands[2], GetFunctionStrLenName])
        else
          raise ENotSupportedException.Create('');
      end;
    TdxFunctionOperatorType.IncTick:
      Result := Format('(%s + cast(%s as double precision) / cast(864000000000 as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('(%s + cast(%s as double precision) / cast(86400000 as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncSecond:
      Result := Format('(%s + cast(%s as double precision) / cast(86400 as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMinute:
      Result := Format('(%s + cast(%s as double precision) / cast(1440 as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncHour:
      Result := Format('(%s + cast(%s as double precision) / cast(24 as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format('(%s + cast(%s as double precision))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Now:
      Result := 'cast('#$27'now'#$27' as timestamp)';
    TdxFunctionOperatorType.Today:
      Result := 'cast('#$27'today'#$27' as timestamp)';
    TdxFunctionOperatorType.Concat:
    begin
      AStringBuilder := TStringBuilder.Create;
      try
        for AIndex := 0 to Length(AOperands) - 1 do
          if Length(AOperands[AIndex]) > 0 then
          begin
            if AStringBuilder.Length > 0 then
              AStringBuilder.Append(' || ');
            AStringBuilder.Append(AOperands[AIndex]);
          end;
        Result := AStringBuilder.ToString;
      finally
        AStringBuilder.Free;
      end;
    end;
    TdxFunctionOperatorType.DaysBetween:
      Result := Format('datediff(day, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.HoursBetween:
      Result := Format('datediff(hour, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('datediff(millisecond, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('datediff(minute, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('datediff(month, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('datediff(second, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('datediff(year, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Substring:
      if Length(AOperands) = 3 then
        Result := Format('substring((%s) from cast(%s as integer)+1 for cast(%s as integer))', [AOperands[0], AOperands[1], AOperands[2]])
      else
        Result := Format('substring((%s) from cast(%s as integer)+1)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMonth:
      if Version.Support(2, 1) then
        Result := Format('dateadd(month, cast(%s as double precision), cast(%s as timestamp))', [AOperands[1], AOperands[0]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.IncYear:
      if Version.Support(2, 1) then
        Result := Format('dateadd(year, cast(%s as double precision), cast(%s as timestamp))', [AOperands[1], AOperands[0]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.CharIndex:
      if Version.Support(2, 1) then
      begin
        if Length(AOperands) >= 3 then
          Result := Format('(position(%s, %s, cast(%s as integer) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2]])
        else
          Result := Format('(position(%s, %s) - 1)', [AOperands[0], AOperands[1]]);
      end
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Round:
      if Version.Support(2, 1) then
      begin
        if Length(AOperands) >= 2 then
          Result := Format('round(cast(%s as double precision), cast(%s as integer))', [AOperands[0], AOperands[1]])
        else
          Result := Format('round(cast(%s as double precision))', [AOperands[0]]);
      end
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Remove:
      if Version.Support(2, 1) then
      begin
        if Length(AOperands) >= 3 then
          Result := Format('(left(%0:s, cast(%1:s as integer)) || right(%0:s, char_length(%0:s)-cast({1} as integer)-cast(%2:s as integer)))', [AOperands[0], AOperands[1], AOperands[2]])
        else
          Result := Format('(left(%s, cast(%s as integer)))', [AOperands[0], AOperands[1]]);
      end
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Insert:
      if Version.Support(2, 1) then
        Result := Format('(left(%0:s, cast(%1:s as integer)) || (%2:s) || right(%0:s, char_length(%0:s)-cast(%1:s as integer)))', [AOperands[0], AOperands[1], AOperands[2]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Reverse:
      if Version.Support(2, 1) then
        Result := Format('reverse(%s)', [AOperands[0]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Replace:
      if Version.Support(2, 1) then
        Result := Format('replace(%s, %s, %s)', [AOperands[0], AOperands[1], AOperands[2]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Exp:
      if Version.Support(2, 1) then
        Result := Format('exp(cast(%s as double precision))', [AOperands[0]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.Power:
      if Version.Support(2, 1) then
        Result := Format('power(cast(%s as double precision), cast(%s as double precision))', [AOperands[0], AOperands[1]])
      else
        raise ENotSupportedException.Create('');
    TdxFunctionOperatorType.UtcNow:
      raise ENotSupportedException.Create('');
    else
      Result := inherited FormatFunction(AOperatorType, AOperands);
  end;
end;

function TdxFirebirdConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
var
  ANeedEscape: Boolean;
  AConstantValue: IdxOperandValue;
  AOperandString, AOperandStringFormatted: string;
  AFirstOperand, ASecondOperand: TdxCriteriaOperator;
begin
  if AOperatorType in [TdxFunctionOperatorType.StartsWith, TdxFunctionOperatorType.EndsWith, TdxFunctionOperatorType.Contains] then
  begin
    AFirstOperand := Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject);
    ASecondOperand := Safe<TdxCriteriaOperator>.Cast(AOperands[1].AsObject);
    if (ASecondOperand is TdxOperandValue) and ((TdxOperandValue(ASecondOperand)).Value.IsString) then
    begin
      AOperandString := TdxOperandValue(ASecondOperand).Value.AsString;
      ANeedEscape := TdxStringHelper.IndexOfAny(AOperandString, StopChars) >= 0;
      if ANeedEscape then
        AOperandString := ReplaceText(ReplaceText(ReplaceText(AOperandString, '\', '\\'), '%', '\%'), '_', '\_');
      case AOperatorType of
        TdxFunctionOperatorType.StartsWith:
          AConstantValue := TdxConstantValue.Create(AOperandString + '%');
        TdxFunctionOperatorType.EndsWith:
          AConstantValue := TdxConstantValue.Create('%' + AOperandString);
        TdxFunctionOperatorType.Contains:
          AConstantValue := TdxConstantValue.Create('%' + AOperandString + '%');
      else
        raise ENotSupportedException.Create('');
      end;
      AOperandStringFormatted := AProcessParameter(AConstantValue);
      if ANeedEscape then
        Result := Format('(%s like %s ESCAPE '#$27'\'#$27')', [AProcessParameter(AFirstOperand), AOperandStringFormatted])
      else
        Result := Format('(%s like %s)', [AProcessParameter(AFirstOperand), AOperandStringFormatted]);
    end
    else
      raise ENotSupportedException.Create('');
  end
  else
    Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands);
end;

function TdxFirebirdConnectionProvider.FormatTable(const ATableName: string): string;
begin
    Result := '"' + ATableName + '"'
end;

function TdxFirebirdConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string): string;
begin
  Result := FormatTable(ATableName);
end;

function TdxFirebirdConnectionProvider.FormatTable(const ASchemaName: string; const ATableName: string; const ATableAlias: string): string;
begin
  Result := FormatTable(ATableName) + ' ' + ATableAlias;
end;

function TdxFirebirdConnectionProvider.FormatSelect(const ASelectedPropertiesSQL, AFromSQL, AWhereSQL, AOrderBySQL, AGroupBySQL, AHavingSQL: string;
  ASkipSelectedRecords: Integer; ATopSelectedRecords: Integer): string;
var
  ATopSql, ASkipSql, AModificatorsSql, AExpandedWhereSql, AExpandedOrderBySql, AExpandedHavingSql, AExpandedGroupBySql: string;
begin
  Result := inherited FormatSelect(ASelectedPropertiesSql, AFromSql, AWhereSql,
    AOrderBySql, AGroupBySql, AHavingSql, ASkipSelectedRecords, ATopSelectedRecords);
  ATopSql := '';
  ASkipSql := '';
  AExpandedWhereSql := '';
  AExpandedOrderBySql := '';
  AExpandedHavingSql := '';
  AExpandedGroupBySql := '';
  if ATopSelectedRecords <> 0 then
    ATopSql := ' first '+ IntToStr(ATopSelectedRecords) + ' ';
  if ASkipSelectedRecords <> 0 then
    ASkipSql := ' skip ' + IntToStr(ASkipSelectedRecords) + ' ';
  AModificatorsSql := ATopSql + ASkipSql;
  if AWhereSql <> '' then
    AExpandedWhereSql := dxCRLF + 'where ' + AWhereSql;
  if AOrderBySql <> '' then
    AExpandedOrderBySql := dxCRLF + 'order by '+ AOrderBySql;
  if AHavingSql <> '' then
    AExpandedHavingSql := dxCRLF + 'having ' + AHavingSql;
  if AGroupBySql <> '' then
    AExpandedGroupBySql := dxCRLF + 'group by ' + AGroupBySql;
  Result := 'select ' + AModificatorsSql + ASelectedPropertiesSql +' from ' +
    AFromSql + AExpandedWhereSql + AExpandedGroupBySql + AExpandedHavingSql + AExpandedOrderBySql;
end;

function TdxFirebirdConnectionProvider.FormatOrder(const ASortProperty: string; ASortDirection: TdxSortDirection): string;
begin
  if ASortDirection = TdxSortDirection.Ascending then
    Result := ASortProperty + 'asc nulls first'
  else
    Result := ASortProperty + 'desc nulls last';
end;

function TdxFirebirdConnectionProvider.FormatInsert(
  const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := 'insert into ' + ATableName + '(' + AFieldNames + ')values(' + AValuesClause + ')';
end;

function TdxFirebirdConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
begin
  Result := 'insert into ' + FormatTableSafe(ATable) +' values()';
end;

function TdxFirebirdConnectionProvider.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := Format('update %s set %s where %s', [ATableName, ASetClause, AWhereClause]);
end;

function TdxFirebirdConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := Format('delete from %s where %s', [ATableName, AWhereClause]);
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnFullAttributes(
  ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := GetSQLCreateColumnType(ATable, AColumn);
  if AColumn.IsKey or not AColumn.IsNullable then
    Result := Result + ' NOT NULL';
end;

function TdxFirebirdConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AList: TList<string>;
begin
  Result := nil;
  AQuery := TdxQuery.Create('select RDB$RELATION_NAME, RDB$VIEW_BLR from RDB$RELATIONS where RDB$SYSTEM_FLAG = 0');
  try
    AList := TList<string>.Create;
    ADataSet := SelectData(AQuery);
    try
      while not ADataSet.Eof do
      begin
        if not AIncludeViews and not VarIsNull(GetFieldValue(ADataSet.Fields[1])) then
        else
          AList.Add(GetFieldStringValue(ADataSet.Fields[0]));
        ADataSet.Next;
      end;
      Result := AList.ToArray;
    finally
      AList.Free;
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

function TdxFirebirdConnectionProvider.GetParameterName(const AParameter: IdxOperandValue;
  AIndex: Integer; var ACreateParameter: Boolean): string;
var
  AValue: TValue;
begin
  AValue := AParameter.Value;
  ACreateParameter := False;
  if (AParameter is TdxConstantValue) and not AValue.IsEmpty then
  begin
    if AValue.IsType<Boolean> then
      Exit(IfThen(AValue.AsBoolean, '1', '0'));
    case AValue.Kind of
      tkInteger:
        Exit(IntToStr(AValue.AsInteger));
      tkEnumeration:
        Exit(IntToStr(AValue.AsOrdinal));
      tkString, tkLString, tkWString, tkUString:
        Exit(#$27 + ReplaceText(AValue.AsString, #$27, #$27#$27) + #$27);
    end;
  end;
  ACreateParameter := True;
  if AParameter.Value.IsString  then
    Result := Format('s%d_%d', [AIndex, Length(AParameter.Value.ToString)])
  else
    Result := GetParamAlias(AIndex);
end;

procedure TdxFirebirdConnectionProvider.GetTableSchema(
  ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean);
begin
  GetColumns(ATable);
  GetPrimaryKey(ATable);
  if ACheckIndexes then
    GetIndexes(ATable);
  if ACheckForeignKeys then
    GetForeignKeys(ATable);
end;

function TdxFirebirdConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := True;
end;

function TdxFirebirdConnectionProvider.SupportsNativeOuterApply: Boolean;
begin
  Result := True;
end;

function TdxFirebirdConnectionProvider.GetVersion: TVersion;
begin
  if not FVersion.HasValue then
    ReadDBVersion;
  Result := FVersion;
end;

function TdxFirebirdConnectionProvider.IsSequenceExists(const ASequenceName: string): Boolean;
var
  AQuery: TdxQuery;
begin
  if ASequenceName = '' then
    Exit(False);
  AQuery := TdxQuery.Create('select count(*) from RDB$GENERATORS where RDB$GENERATOR_NAME = ''' + ASequenceName + '''');
  try
    Result := GetScalar(AQuery) <> 0;
  finally
    AQuery.Free;
  end;
end;

class destructor TdxFirebirdConnectionProvider.Destroy;
begin
  FreeAndNil(FConnectionParameters);
  FreeAndNil(FDBTypes);
end;

class function TdxFirebirdConnectionProvider.GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
begin
  if FConnectionParameters = nil then
  begin
    FConnectionParameters := TDictionary<string, TdxConnectionParameterValues>.Create;
    FConnectionParameters.AddOrSetValue(TdxConnectionTypes.ADO, TConnectionParameters.Create(['initial catalog']));
    FConnectionParameters.AddOrSetValue(TdxConnectionTypes.FireDAC, TConnectionParameters.Create(['Database']));
  end;
  Result := FConnectionParameters;
end;

class procedure TdxFirebirdConnectionProvider.PopulateDBTypes;
begin
  FDBTypes.Add('bigint', TdxDBColumnType.Int64);
  FDBTypes.Add('binary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('bit', TdxDBColumnType.Boolean);
  FDBTypes.Add('char', TdxDBColumnType.Char);
  FDBTypes.Add('datetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('decimal', TdxDBColumnType.Decimal);
  FDBTypes.Add('float', TdxDBColumnType.Double);
  FDBTypes.Add('image', TdxDBColumnType.ByteArray);
  FDBTypes.Add('int', TdxDBColumnType.Int32);
  FDBTypes.Add('money', TdxDBColumnType.Decimal);
  FDBTypes.Add('nchar', TdxDBColumnType.String);
  FDBTypes.Add('ntext', TdxDBColumnType.String);
  FDBTypes.Add('numeric', TdxDBColumnType.Decimal);
  FDBTypes.Add('nvarchar', TdxDBColumnType.String);
  FDBTypes.Add('real', TdxDBColumnType.Double);
  FDBTypes.Add('smalldatetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('smallint', TdxDBColumnType.Int16);
  FDBTypes.Add('smallmoney', TdxDBColumnType.Decimal);
  FDBTypes.Add('sql_variant', TdxDBColumnType.ByteArray);
  FDBTypes.Add('text', TdxDBColumnType.String);
  FDBTypes.Add('timestamp', TdxDBColumnType.Int32);
  FDBTypes.Add('tinyint', TdxDBColumnType.Byte);
  FDBTypes.Add('varbinary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('varchar', TdxDBColumnType.String);
  FDBTypes.Add('uniqueidentifier', TdxDBColumnType.Guid);
end;

function TdxFirebirdConnectionProvider.ConvertToDBParameter(const AParameterValue: TValue): TValue;
const
  ABool2Char: array[Boolean] of Char = ('0', '1');
begin
  if AParameterValue.IsType<TGUID> then
    Result := Copy(GUIDToString(AParameterValue.AsType<TGUID>), 2, 36)
  else
    if AParameterValue.IsType<Boolean> then
      Result := ABool2Char[AParameterValue.AsBoolean]
    else
      Result := inherited ConvertToDBParameter(AParameterValue);

end;

class function TdxFirebirdConnectionProvider.GetColumnTypeByInfo(
  AType, ASubType, AScale, ASize: SmallInt): TdxDBColumnType;
begin
  Result := TdxDBColumnType.Unknown;
  case AType of
    7:
    begin
      if (ASubType = 2) or (ASubType = 1) or (AScale < 0) then
        Result := TdxDBColumnType.Decimal
      else
        Result := TdxDBColumnType.Int16;
    end;
    8:
    begin
      if (ASubType = 2) or (ASubType = 1) or (AScale < 0) then
        Result := TdxDBColumnType.Decimal
      else
        Result := TdxDBColumnType.Int32;
    end;
    9, $10, $2d:
    begin
      if (ASubType = 2) or (ASubType = 1) or (AScale < 0) then
        Result := TdxDBColumnType.Decimal
      else
        Result := TdxDBColumnType.Int64;
    end;
    10:
      Result := TdxDBColumnType.Single;
    11, $1B:
      Result := TdxDBColumnType.Double;
    14, 15:
      if ASize = 1 then
        Result := TdxDBColumnType.Char
      else
        Result := TdxDBColumnType.String;
    $0C, $23:
      Result := TdxDBColumnType.DateTime;
    $25, $26, 40, $29:
      Result := TdxDBColumnType.String;
    $105:
      if ASubType = 1 then
        Result := TdxDBColumnType.String
      else
        Result := TdxDBColumnType.ByteArray;
  end;
end;

class function TdxFirebirdConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  if FDBTypes = nil then
  begin
    FDBTypes := TDictionary<string, TdxDBColumnType>.Create;
    PopulateDBTypes;
  end;
  Result := FDBTypes;
end;


procedure TdxFirebirdConnectionProvider.ReadDbVersion;
var
  AQuery: TdxQuery;
  C: TdxCustomDBCommand;
  AVersion: Variant;
begin
  AQuery := TdxQuery.Create('SELECT rdb$get_context('#$27'SYSTEM'#$27', '#$27'ENGINE_VERSION'#$27') as version from rdb$database');
  try
    C := CreateCommand(AQuery);
    try
      AVersion := C.GetScalar;
      if VarIsStr(AVersion) then
        FVersion := TVersion.Create(AVersion);
    finally
      C.Free;
    end;
  except
  end;
  AQuery.Free;
end;

function TdxFirebirdConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := MaximumStringSize;
end;

function TdxFirebirdConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := 31;
end;

function TdxFirebirdConnectionProvider.GetFunctionStrLenName: string;
begin
  if Version.Support(2, 1) then
    Result := 'CHAR_LENGTH'
  else
    Result := 'strlen';
end;

procedure TdxFirebirdConnectionProvider.GetColumns(ATable: TdxDBTable);
var
  AColumn: TdxDBColumn;
  ADataSet: TDataSet;
  AQuery: TdxQuery;
  F0, F1, F2, F3, F4, F5, F6: TField;
  ASize: Integer;
  AType: TdxDBColumnType;
  S: string;
begin
  AQuery := TdxQuery.Create('select r.RDB$FIELD_NAME, f.RDB$FIELD_TYPE, f.RDB$FIELD_SUB_TYPE, f.RDB$FIELD_SCALE, ' +
    'RDB$CHARACTER_LENGTH, r.RDB$NULL_FLAG, r.RDB$DEFAULT_SOURCE from RDB$RELATION_FIELDS r join ' +
    'RDB$FIELDS f on r.RDB$FIELD_SOURCE  = f.RDB$FIELD_NAME where r.RDB$RELATION_NAME = '#$27 + ComposeSafeTableName(ATable.Name) + #$27);
  try
    ADataSet := SelectData(AQuery);
    try
      F0 := ADataSet.Fields[0];
      F1 := ADataSet.Fields[1];
      F2 := ADataSet.Fields[2];
      F3 := ADataSet.Fields[3];
      F4 := ADataSet.Fields[4];
      F5 := ADataSet.Fields[5];
      F6 := ADataSet.Fields[6];
      while not ADataSet.Eof do
      begin
        ASize := GetFieldValueDef(F4, 0);
        AType := GetColumnTypeByInfo(GetFieldValueDef(F1, 0), GetFieldValueDef(F2, 0), GetFieldValueDef(F3, 0), ASize);
        if AType <> TdxDBColumnType.String then
          ASize := 0;
        AColumn := TdxDBColumn.Create(GetFieldStringValue(F0), False, '', ASize, AType);
        AColumn.IsNullable := F5.IsNull and (F5.AsInteger <> 1);
        if not F6.IsNull then
        begin
          S := Trim(F6.AsString);
          if StartsText('DEFAULT', S) then
            S := Trim(Copy(S, 8, Length(S)));
          AColumn.DefaultValue := S;
        end;
        ATable.AddColumn(AColumn);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

function TdxFirebirdConnectionProvider.GetSeqName(ATable: TdxDBTable): string;
var
  ASequenceName: string;
begin
  ASequenceName := ATable.SequenceName;
  if ASequenceName = '' then
    Result := ComposeSafeConstraintName('sq_' + ATable.Name)
  else
    Result := ASequenceName;
end;

procedure TdxFirebirdConnectionProvider.GetIndexes(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AIndex: TdxDBIndex;
  AIsUnique: Boolean;
  F0, F1, F2, F3: TField;
begin
  AQuery := TdxQuery.Create('select RDB$INDICES.RDB$INDEX_NAME, RDB$INDEX_SEGMENTS.RDB$FIELD_NAME, RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION, RDB$INDICES.RDB$UNIQUE_FLAG fro' +
    'm RDB$INDICES join RDB$INDEX_SEGMENTS on RDB$INDEX_SEGMENTS.RDB$INDEX_NAME = RDB$INDICES.RDB$INDEX_NAME where RDB$INDICES.RDB$RELATION_NAME ' +
    '= '#$27 + ComposeSafeTableName(ATable.Name) + #$27' and RDB$INDICES.RDB$FOREIGN_KEY is NULL order by RDB$INDICES.RDB$INDEX_NAME, RDB$INDEX_SEGMENTS.RDB$FIELD_POSITION');
  try
    ADataSet := SelectData(AQuery);
    try
      AIndex := nil;
      F0 := ADataSet.Fields[0];
      F1 := ADataSet.Fields[1];
      F2 := ADataSet.Fields[2];
      F3 := ADataSet.Fields[3];
      while not ADataSet.Eof do
      begin
        AIsUnique := GetFieldValueDef(F3, 0) = 1;
       if GetFieldValueDef(F2, 0) = 0 then
        begin
          AIndex := TdxDBIndex.Create(GetFieldStringValue(F0), [GetFieldStringValue(F1)], AIsUnique);
          ATable.Indexes.Add(AIndex);
        end
        else
          AIndex.Columns.Add(GetFieldStringValue(F1));
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

procedure TdxFirebirdConnectionProvider.GetPrimaryKey(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AColumn: TdxDBColumn;
  AColumns: TList<string>;
begin
  AQuery := TdxQuery.Create('select RDB$INDEX_SEGMENTS.RDB$FIELD_NAME from RDB$RELATION_CONSTRAINTS join RDB$INDEX_SEGMENTS on RDB$INDEX_SEGMENTS.RDB$INDEX_NAME = RDB$RE' +
    'LATION_CONSTRAINTS.RDB$INDEX_NAME where RDB$RELATION_NAME = '#$27 + ComposeSafeTableName(ATable.Name) + #$27' and RDB$CONSTRAINT_TYPE = '#$27'PRIMARY KEY'#$27' order by RDB$I' +
    'NDEX_SEGMENTS.RDB$FIELD_POSITION');
  try
    ADataSet := SelectData(AQuery);
    try
      AColumns := TList<string>.Create;
      try
        while not ADataSet.Eof do
        begin
          AColumn := ATable.GetColumn(VarToStr(GetFieldStringValue(ADataSet.Fields[0])));
          if AColumn <> nil then
          begin
            AColumn.IsKey := True;
            AColumns.Add(AColumn.Name);
          end;
          ADataSet.Next;
        end;
       if AColumns.Count > 0 then
         ATable.PrimaryKey := TdxDBPrimaryKey.Create(AColumns);
      finally
        AColumns.Free;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

function TdxFirebirdConnectionProvider.GetFieldStringValue(AField: TField): string;
begin
  Result := Trim(VarToStr(GetFieldValue(AField)));
end;

function TdxFirebirdConnectionProvider.GetFieldValue(AField: TField): Variant;
begin
  if AField is TAggregateField then
    Result := AField.Value
  else
    if AField.IsNull then
      Result := Null
    else
      Result := AField.Value;
end;

function TdxFirebirdConnectionProvider.GetFieldValueDef(AField: TField; const ADefValue: Variant): Variant;
begin
  Result := GetFieldValue(AField);
  if VarIsNull(Result) then
    Result := ADefValue;
end;

procedure TdxFirebirdConnectionProvider.GetForeignKeys(ATable: TdxDBTable);
var
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AForeignKey: TdxDBForeignKey;
begin
  AForeignKey := nil;
  AQuery := TdxQuery.Create('select p.RDB$FIELD_POSITION, p.RDB$FIELD_NAME, f.RDB$FIELD_NAME, pt.RDB$RELATION_NAME'#13#10 +
    'from RDB$INDICES i'#13#10 +  'join RDB$INDEX_SEGMENTS f on f.RDB$INDEX_NAME = i.RDB$INDEX_NAME'#13#10 +
    'join RDB$INDEX_SEGMENTS p on p.RDB$INDEX_NAME = i.RDB$FOREIGN_KEY and p.RDB$FIELD_POSITION = f.RDB$FIELD_POSITION'#13#10 +
    'join RDB$INDICES pt on i.RDB$FOREIGN_KEY = pt.RDB$INDEX_NAME'#13#10 +
    'where i.RDB$RELATION_NAME = '#$27 + ComposeSafeTableName(ATable.Name) + #$27' and i.RDB$FOREIGN_KEY is NOT NULL'#13#10 +
    'order by i.RDB$INDEX_NAME, f.RDB$FIELD_POSITION');
  try
    ADataSet := SelectData(AQuery);
    try
      while not ADataSet.Eof do
      begin
        if GetFieldValueDef(ADataSet.Fields[0], 0) = 0 then
        begin
          AForeignKey := TdxDBForeignKey.Create([GetFieldValue(ADataSet.Fields[2])],
            GetFieldValue(ADataSet.Fields[3]), [GetFieldValue(ADataSet.Fields[1])]);
          ATable.ForeignKeys.Add(AForeignKey);
        end
        else
          if AForeignKey <> nil then
          begin
            AForeignKey.Columns.Add(FormatColumn(GetFieldValue(ADataSet.Fields[2])));
            AForeignKey.PrimaryKeyTableKeyColumns.Add(GetFieldValue(ADataSet.Fields[1]));
          end;
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQuery.Free;
  end;
end;

type
  TdxFirebirdIdentityInsertSqlGenerator = class(TdxInsertSQLGenerator)
  protected
    function InternalGenerateSQL: string; override;
  end;

function TdxFirebirdIdentityInsertSqlGenerator.InternalGenerateSQL: string;
var
  I: Integer;
  ANames, AValues: TStringBuilder;
begin
  ANames := TStringBuilder.Create;
  AValues := TStringBuilder.Create;
  try
    for I := 0 to Root.Operands.Count - 1 do
    begin
      ANames.Append(Process(Root.Operands[I])).Append(',');
      AValues.Append(GetNextParameterName((TdxInsertStatement(Root)).Parameters[I])).Append(',');
    end;
    ANames.Append(FFormatter.FormatColumn(FFormatter.ComposeSafeColumnName((TdxInsertStatement(Root)).IdentityColumn)));
    AValues.Append(':seq');
    Result := FFormatter.FormatInsert(
      FFormatter.FormatTable(FFormatter.ComposeSafeSchemaName(Root.Table.Name),
      FFormatter.ComposeSafeTableName(Root.Table.Name)),
      ANames.ToString, AValues.ToString);
  finally
    ANames.Free;
    AValues.Free;
  end;
end;

function TdxFirebirdConnectionProvider.GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64;
var
  Id64: Int64;
  Id32: Integer;
  AValue: Variant;
  AQuery: TdxQuery;
  ASQL: TdxQuery;
  AGenerator: TdxFirebirdIdentityInsertSqlGenerator;
begin
  AQuery := TdxQuery.Create('select GEN_ID(' + FormatTable(GetSeqName(ARoot.Table)) + ', 1) from RDB$DATABASE');
  try
    AValue := GetScalar(AQuery);
    Result := AValue;
  finally
    AQuery.Free;
  end;
  AGenerator := TdxFirebirdIdentityInsertSqlGenerator.Create(Self, AIdentities);
  try
    ASQL := AGenerator.GenerateSQL(ARoot);
    try
      case ARoot.IdentityColumnType of
        TdxDBColumnType.Int32:
          begin
            Id32 := Integer(AValue);
            ASQL.Parameters.Add(TdxOperandValue.Create(Id32));
          end;
        TdxDBColumnType.Int64:
          begin
            Id64 := AValue;
            ASQL.Parameters.Add(TdxOperandValue.Create(Id64));
          end
        else
          raise ENotSupportedException.Create('');
      end;
      ASQL.AppendParameterName('seq');
      Execute(ASQL);
    finally
      ASQL.Free;
    end;
  finally
    AGenerator.Free;
  end;
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(1)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(3,0)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(3,0)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char CHARACTER SET UNICODE_FSS';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'decimal(18,4)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double precision';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'float';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'integer';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(10,0)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'smallint';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(5,0)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bigint';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(18,0)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumStringSize) then
    Result := 'char varying (' + IntToStr(AColumn.Size) + ') CHARACTER SET UNICODE_FSS'
  else
    if AColumn.Size = 0 then
      Result := 'char varying (' + IntToStr(DefaultStringSize) + ') CHARACTER SET UNICODE_FSS'
    else
      Result := 'BLOB SUB_TYPE TEXT';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'timestamp';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'char(36)';
end;

function TdxFirebirdConnectionProvider.GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'blob';
end;

function TdxFirebirdConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;
const
  AQueryStr = 'select RDB$RELATION_NAME, RDB$VIEW_BLR from RDB$RELATIONS where RDB$RELATION_NAME in (%s) and RDB$SYSTEM_FLAG = 0';
var
  I: Integer;
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  ADBTables: TDictionary<string, Boolean>;
  ATable: TdxDBTable;
  AList: TList<TdxDBTable>;
  AStrList, AParamNames: TStringList;
  AValue: Boolean;
  AParameters: TdxQueryParameterCollection;
begin
  Result := nil;
  AList := TList<TdxDBTable>.Create;
  AStrList := TStringList.Create;
  AParamNames := TStringList.Create;
  ADBTables := TDictionary<string, Boolean>.Create;
  AParameters := TdxQueryParameterCollection.Create;
  try
    AStrList.Delimiter := ',';
    for I := 0 to Length(ATables) - 1  do
    begin
      AStrList.Add(':' + GetParamAlias(I));
      AParamNames.Add(GetParamAlias(I));
      AParameters.Add(TdxOperandValue.Create(ComposeSafeTableName(ATables[I].Name)));
    end;
    AQuery := TdxQuery.Create(Format(AQueryStr, [AStrList.CommaText]), AParameters, AParamNames.ToStringArray);
    try
      ADataSet := SelectData(AQuery);
      try
        while not ADataSet.Eof do
        begin
          ADBTables.AddOrSetValue(VarToStr(GetFieldStringValue(ADataSet.Fields[0])), not ADataSet.Fields[1].IsNull);
          ADataSet.Next;
        end;
      finally
        ADataSet.Free;
      end;
      for ATable in ATables do
      begin
        if not ADBTables.TryGetValue(ComposeSafeTableName(ATable.Name), AValue) then
          AList.Add(ATable)
        else
          ATable.IsView := AValue;
      end;
      if AList.Count > 0 then
        Result := AList.ToArray;
    finally
      AQuery.Free;
    end;
  finally
    AParameters.Free;
    AStrList.Free;
    AParamNames.Free;
    AList.Free;
    ADBTables.Free;
  end;
end;


initialization
  TdxSQLConnectionProviderFactory.Register(TdxFirebirdConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxFirebirdConnectionProvider);

end.


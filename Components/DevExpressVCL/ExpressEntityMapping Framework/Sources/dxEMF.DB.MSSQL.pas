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

unit dxEMF.DB.MSSQL;

{$HPPEMIT '#pragma link "dxEMF.DB.MSSQL"'}

interface

{$I cxVer.inc}
{$I dxEMF.inc}

uses
  SysUtils, Classes, Generics.Collections, DB, Rtti,
  dxEMF.DB.Model,
  dxEMF.Types,
  dxEMF.DB.Criteria,
  dxEMF.DB.Query,
  dxEMF.DB.SQLConnectionProvider;

type
  { TdxMSSQLConnectionProvider }

  TdxMSSQLConnectionProvider = class(TdxSQLConnectionProvider)
  public type
    TVersion = (is2000, is2005, is2008, is2012, isAzure);
    TVersions = set of TVersion;
  strict private
    class var
      FConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
      FDBTypes: TDictionary<string, TdxDBColumnType>;
    class destructor Destroy;
    class procedure PopulateConnectionParameters; static; inline;
    class procedure PopulateDBTypes; static; inline;
    class function GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>; static;
  strict private const
    DatabaseObjectDefaultOwner = 'dbo';
    NameTableMaxLength = 128;
    MaximumBinarySize = 8000;
  strict private
    FDatabaseObjectOwner: string;
    FVersion: TVersions;
    FIsAzureHasValue: Boolean;
    function GetIsAzure: Boolean;
    function GetVersion(const Index: TVersion): Boolean;
    function HasSequence(ATable: TdxDBTable): Boolean;
    function FormatOwnedDBObject(const ASchemaName, AObjectName: string): string;
    procedure ReadDBVersion;
    function GetDataForTables(const ATables: TArray<TdxDBTable>; AFilter: TdxMSSQLConnectionProvider.TTableFilter;
      const AQueryText: string): TDataSet;
    procedure GetColumns(ATable: TdxDBTable);
    procedure GetPrimaryKey(ATable: TdxDBTable);
    procedure GetIndexes(ATable: TdxDBTable);
    procedure GetForeignKeys(ATable: TdxDBTable);
    function GetSeqName(ATable: TdxDBTable): string;
    function GetTypeFromString(const ATypeName: string; ALength: Integer): TdxDBColumnType;
  protected
    function CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>; override;
    function GetMaximumStringSize: Integer; override;
    function GetSafeNameTableMaxLength: Integer; override;
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
    function GetIdentity(ASQL: TdxQuery): Int64; override;
    function GetIdentity(ARoot: TdxInsertStatement; AIdentities: TdxTaggedParameterHolder): Int64; override;
    class function GetDBTypes: TDictionary<string, TdxDBColumnType>; override;

    property DBTypes: TDictionary<string, TdxDBColumnType> read GetDBTypes;
    property Is2000: Boolean index TVersion.is2000 read GetVersion;
    property Is2005: Boolean index TVersion.is2005 read GetVersion;
    property Is2008: Boolean index TVersion.is2008 read GetVersion;
    property Is2012: Boolean index TVersion.is2012 read GetVersion;
    property IsAzure: Boolean read GetIsAzure;
  public
    constructor Create(AConnectionVendor: TdxCustomConnectionVendor; AAutoCreateOption: TdxAutoCreateOption); override;

    class function GetDBEngine: TdxDBEngine; override;

    procedure CreateDatabase(const ADatabaseName: string; const AValues: array of string); override;
    procedure CreateTable(ATable: TdxDBTable); override;
    function GetSQLCreateColumnFullAttributes(ATable: TdxDBTable; AColumn: TdxDBColumn): string; override;
    function GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer; var ACreateParameter: Boolean): string; override;
    procedure GetTableSchema(ATable: TdxDBTable; ACheckIndexes: Boolean; ACheckForeignKeys: Boolean); override;
    function GetStorageTableNames(AIncludeViews: Boolean): TArray<string>; override;
    function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; override;
    function FormatColumn(const AColumnName: string): string; overload; override;
    function FormatColumn(const AColumnName: string; const ATableAlias: string): string; overload; override;
    function FormatConstraint(const AConstraintName: string): string; override;
    function FormatFunction(AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<string>): string; override;
    function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      const AOperands: TArray<TValue>): string; override;
    function FormatTable(const ASchemaName, ATableName, ATableAlias: string): string; override;
    function FormatTable(const ASchemaName, ATableName: string): string; override;
    function FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause: string;
      ASkipSelectedRecords, ATopSelectedRecords: Integer): string; override;
    function FormatInsertDefaultValues(ATable: TdxDBTable): string; override;
    function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; override;
    function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; override;
    function FormatDelete(const ATableName, AWhereClause: string): string; override;
    function GetConnectionParameter(const AConnectionType: TdxConnectionType; AParameter: TdxConnectionParameter): string; override;
    property DatabaseObjectOwner: string read FDatabaseObjectOwner write FDatabaseObjectOwner;
    function SupportsNativeSkipTake: Boolean; override;
    function SupportsNativeOuterApply: Boolean; override;
    class property ConnectionParameters: TDictionary<string, TdxConnectionParameterValues> read GetConnectionParameters;
  end;

implementation

uses
  StrUtils, TypInfo,
  dxCore, dxStringHelper,
  dxEMF.Utils,
  dxEMF.DB.SQLGenerator,
  dxEMF.DB.Utils;

type

  { TdxSequenceInsertSqlGenerator }

  TdxSequenceInsertSqlGenerator = class(TdxInsertSQLGenerator)
  protected
    function InternalGenerateSQL: string; override;
  end;

  { TdxMsSQLFormatterHelper }

  TdxMsSQLFormatterHelper = class
  strict private const
    StopChars: array[0 .. 3] of char = ('_', '%', '[', ']');
  public
    class function FormatColumn(const AColumnName: string): string; overload; static;
    class function FormatColumn(const AColumnName, ATableAlias: string): string; overload; static;
    class function FormatInsertDefaultValues(const ATableName: string): string; static;
    class function FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string; static;
    class function FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string; static;
    class function FormatDelete(const ATableName, AWhereClause: string): string; static;
    class function FormatFunction(AOperatorType: TdxFunctionOperatorType; const ADBEngineVersion: TdxMSSQLConnectionProvider.TVersions;
      const AOperands: TArray<string>): string; overload; static;
    class function FormatFunction(AProcessParameter: TdxProcessParameter; AOperatorType: TdxFunctionOperatorType;
      ADBEngineVersion: TdxMSSQLConnectionProvider.TVersions; const AOperands: TArray<TValue>): string; overload; static;
    class function FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand, ARightOperand: string): string; static;
    class function FormatConstraint(const AConstraintName: string): string; static;
  end;

{ TdxSequenceInsertSqlGenerator }

function TdxSequenceInsertSqlGenerator.InternalGenerateSQL: string;
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

{ TdxMsSQLFormatterHelper }

class function TdxMsSQLFormatterHelper.FormatColumn(const AColumnName: string): string;
begin
  Result := '"' + AColumnName + '"';
end;

class function TdxMsSQLFormatterHelper.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := ATableAlias + '."' + AColumnName + '"';
end;

class function TdxMsSQLFormatterHelper.FormatInsertDefaultValues(const ATableName: string): string;
begin
  Result := 'insert into ' + ATableName + ' default values';
end;

class function TdxMsSQLFormatterHelper.FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := 'insert into ' + ATableName + '(' + AFieldNames + ')values(' + AValuesClause + ')';
end;

class function TdxMsSQLFormatterHelper.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := 'update ' + ATableName + ' set ' + ASetClause + ' where ' + AWhereClause;
end;

class function TdxMsSQLFormatterHelper.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := 'delete from ' + ATableName + ' where ' + AWhereClause;
end;

class function TdxMsSQLFormatterHelper.FormatFunction(AOperatorType: TdxFunctionOperatorType;
  const ADBEngineVersion: TdxMSSQLConnectionProvider.TVersions; const AOperands: TArray<string>): string;
begin
  case AOperatorType of
    TdxFunctionOperatorType.Abs:
      Result := Format('ABS(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sqr:
      Result := Format('sqrt(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Random:
      Result := 'rand()';
    TdxFunctionOperatorType.Log:
      case Length(AOperands) of
        1:
          Result := Format('Log(%s)', [AOperands[0]]);
        2:
          Result := Format('(Log(%s) / Log(%s))', [AOperands[0], AOperands[1]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.BigMul:
      Result := Format('(Convert(bigint, %s) * CONVERT(bigint,  %s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Log10:
      Result := Format('Log10(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Sin:
      Result := Format('Sin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Tan:
      Result := Format('Tan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan:
      Result := Format('Atan(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcTan2:
      Result := Format('(case when (%1:s) = 0 then Sign(%0:s) * Atan(1) * 2 else Atn2(%0:s,  %1:s) end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Cos:
      Result := Format('Cos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcCos:
      Result := Format('Acos(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ArcSin:
      Result := Format('Asin(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Cosh:
      Result := Format('((Exp(%s) + Exp(-(%0:s))) / 2)', [AOperands[0]]);
    TdxFunctionOperatorType.Sinh:
      Result := Format('((Exp(%s) - Exp(-(%0:s))) / 2)', [AOperands[0]]);
    TdxFunctionOperatorType.Tanh:
      Result := Format('((Exp(%s) - Exp(-(%0:s))) / (Exp(%0:s) + Exp(-(%0:s))))', [AOperands[0]]);
    TdxFunctionOperatorType.Exp:
      Result := Format('Exp(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Power:
      Result := Format('Power(%s,%s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Round:
      case Length(AOperands) of
        1:
          Result := Format('Round(%s,0)', [AOperands[0]]);
        2:
          Result := Format('Round(%s,%s)', [AOperands[0], AOperands[1]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.Sign:
      Result := Format('Sign(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Floor:
      Result := Format('Floor(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Ceil:
      Result := Format('Ceiling(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Max:
      Result := Format('(case when %0:s > %1:s then %0:s else %1:s end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Min:
      Result := Format('(case when %0:s < %1:s then %0:s else %1:s end)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Ascii:
      Result := Format('Ascii(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Char:
      Result := Format('Char(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInteger:
      Result := Format('Cast((%s) as int)', [AOperands[0]]);
    TdxFunctionOperatorType.ToInt64:
      Result := Format('Cast((%s) as bigint)', [AOperands[0]]);
    TdxFunctionOperatorType.ToSingle:
      Result := Format('Cast((%s) as real)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDouble:
      Result := Format('Cast((%s) as double precision)', [AOperands[0]]);
    TdxFunctionOperatorType.ToDecimal:
      Result := Format('Cast((%s) as money)', [AOperands[0]]);
    TdxFunctionOperatorType.ToString:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2005 in ADBEngineVersion, 'Cast((%s) as nvarchar(max))',
        'Cast((%s) as nvarchar(4000))'), [AOperands[0]]);
    TdxFunctionOperatorType.MillisecondOf:
      Result := Format('DATEPART(Millisecond, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.SecondOf:
      Result := Format('DATEPART(Second, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MinuteOf:
      Result := Format('DATEPART(Minute, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.HourOf:
      Result := Format('DATEPART(Hour, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOf:
      Result := Format('DATEPART(Day, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.MonthOf:
      Result := Format('DATEPART(Month, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.YearOf:
      Result := Format('DATEPART(Year, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.GetTimeOfDay:
      Result := Format('(CONVERT(BigInt,((CONVERT(BigInt,DATEPART(HOUR, %0:s))) * 36000000000) + ' +
        '((CONVERT(BigInt,DATEPART(MINUTE, %0:s))) * 600000000) + ((CONVERT(BigInt,DATEPART(SECOND, %0:s))) * 10000000) + ' +
        '((CONVERT(BigInt,DATEPART(MILLISECOND, %0:s))) * 10000)))', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheWeek:
      Result := Format('CONVERT(Int,(DATEPART(dw, %s) + (@@DATEFIRST) + 6) %% 7)', [AOperands[0]]);
    TdxFunctionOperatorType.DayOfTheYear:
      Result := Format('DATEPART(DayOfYear, %s)', [AOperands[0]]);
    TdxFunctionOperatorType.DateOf:
      Result := Format('DATEADD(HOUR, -DATEPART(HOUR, %0:s), DATEADD(MINUTE, -DATEPART(MINUTE, %0:s), ' +
        'DATEADD(SECOND, -DATEPART(SECOND, %0:s), DATEADD(MILLISECOND, -DATEPART(MILLISECOND, %0:s), %0:s))))', [AOperands[0]]);
    TdxFunctionOperatorType.IncTick:
      Result := Format('DATEADD(ms, CONVERT(BigInt, (%1:s) / 10000) %% 86400000, DATEADD(day, (%1:s) / 864000000000, %0:s))',
        [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMillisecond:
      Result := Format('DATEADD(ms, %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.AddTimeSpan,
    TdxFunctionOperatorType.IncSecond:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2005 in ADBEngineVersion,
        'DATEADD(ms, (CONVERT(decimal(38, 19),(%1:s)) * 1000)',
        'DATEADD(ms, CONVERT(bigint, (CONVERT(decimal(38, 19),(%1:s)) * 1000))') +
        '%% 86400000, DATEADD(day, (CONVERT(decimal(38, 19),(%1:s)) * 1000) / 86400000, %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMinute:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2005 in ADBEngineVersion,
        'DATEADD(ms, (CONVERT(decimal(38, 19),(%1:s)) * 60000)',
        'DATEADD(ms, CONVERT(bigint, (CONVERT(decimal(38, 19),(%1:s)) * 60000))') +
        ' %% 86400000, DATEADD(day, (CONVERT(decimal(38, 19),(%1:s)) * 60000) / 86400000, %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncHour:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2005 in ADBEngineVersion,
        'DATEADD(ms, (CONVERT(decimal(38, 19),(%1:s)) * 3600000)',
        'DATEADD(ms, CONVERT(bigint, (CONVERT(decimal(38, 19),(%1:s)) * 3600000))') +
        ' %% 86400000, DATEADD(day, (CONVERT(decimal(38, 19),(%1:s)) * 3600000) / 86400000, %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncDay:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2005 in ADBEngineVersion,
        'DATEADD(ms, (CONVERT(decimal(38, 19),(%1:s)) * 86400000)',
        'DATEADD(ms, CONVERT(bigint, (CONVERT(decimal(38, 19),(%1:s)) * 86400000))') +
        ' %% 86400000, DATEADD(day, (CONVERT(decimal(38, 19),(%1:s)) * 86400000) / 86400000, %0:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncMonth:
      Result := Format('DATEADD(MONTH, %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.IncYear:
      Result := Format('DATEADD(YEAR, %1:s, %0:s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DaysBetween:
      Result := Format('DATEDIFF(day, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.HoursBetween:
      Result := Format('DATEDIFF(hour, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MillisecondsBetween:
      Result := Format('DATEDIFF(millisecond, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MinutesBetween:
      Result := Format('DATEDIFF(minute, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.MonthsBetween:
      Result := Format('DATEDIFF(month, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.SecondsBetween:
      Result := Format('DATEDIFF(second, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.DateDiffTicks:
      Result := Format(IfThen(TdxMSSQLConnectionProvider.TVersion.is2008 in ADBEngineVersion,
        '((DATEDIFF(microsecond, %s, %s)) * 10)',
        '((DATEDIFF(millisecond, %s, %s)) * 10000)'), [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.YearsBetween:
      Result := Format('DATEDIFF(year, %s, %s)', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Now:
      Result := 'getdate()';
    TdxFunctionOperatorType.UtcNow:
      Result := 'getutcdate()';
    TdxFunctionOperatorType.Today:
      Result := 'DATEADD(day, DATEDIFF(day, '#$27'00:00:00'#$27', getdate()), '#$27'00:00:00'#$27')';
    TdxFunctionOperatorType.Concat:
      Result := TdxFormatterHelper.Concat(AOperands, ' + ');
    TdxFunctionOperatorType.Replace:
      Result := Format('Replace(%s, %s, %s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Reverse:
      Result := Format('Reverse(%s)', [AOperands[0]]);
    TdxFunctionOperatorType.Insert:
      Result := Format('Stuff(%s, (%1:s)+1, 0, %2:s)', [AOperands[0], AOperands[1], AOperands[2]]);
    TdxFunctionOperatorType.Remove:
      case Length(AOperands) of
        2:
          Result := Format('LEFT(%s, %s)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('Stuff(%s, (%s)+1, %s, '#$27#$27')', [AOperands[0], AOperands[1], AOperands[2]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.CharIndex:
      case Length(AOperands) of
        2:
          Result := Format('(Charindex(%s, %s) - 1)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('(Charindex(%s, %s, (%s) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2]]);
        4:
          Result := Format('(Charindex(%s, SUBSTRING(%1:s, 1, (%2:s) + (%3:s)), (%2:s) + 1) - 1)', [AOperands[0], AOperands[1], AOperands[2], AOperands[3]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.PadLeft:
      case Length(AOperands) of
        2:
          Result := Format('isnull(REPLICATE('#$27' '#$27', ((%1:s) - LEN(%0:s))) + (%0:s), %0:s)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('isnull(REPLICATE(%2:s, ((%1:s) - LEN(%0:s))) + (%0:s), %0:s)', [AOperands[0], AOperands[1], AOperands[2]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.PadRight:
      case Length(AOperands) of
        2:
          Result := Format('isnull(%0:s + REPLICATE('#$27' '#$27', ((%1:s) - LEN(%0:s))), %0:s)', [AOperands[0], AOperands[1]]);
        3:
          Result := Format('isnull(%0:s + REPLICATE(%2:s, ((%1:s) - LEN(%0:s))), %0:s)', [AOperands[0], AOperands[1], AOperands[2]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.IsNull:
      case Length(AOperands) of
        1:
          Result := Format('((%s) is null)', [AOperands[0]]);
        2:
          Result := Format('isnull(%s, %s)', [AOperands[0], AOperands[1]]);
        else
          Result := '';
      end;
    TdxFunctionOperatorType.IsNullOrEmpty:
      Result := Format('((%0:s) is null or len(%0:s) = 0)', [AOperands[0]]);
    TdxFunctionOperatorType.EndsWith:
      Result := Format('(Right(%s, Len(%1:s)) = (%1:s))', [AOperands[0], AOperands[1]]);
    TdxFunctionOperatorType.Contains:
      Result := Format('(isnull(CharIndex(%1:s, %0:s), 0) > 0)', [AOperands[0], AOperands[1]]);
    else
      Result := '';
  end;
end;

class function TdxMsSQLFormatterHelper.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; ADBEngineVersion: TdxMSSQLConnectionProvider.TVersions; const AOperands: TArray<TValue>): string;
var
  ASecondOperand: TdxCriteriaOperator;
  AConstantValue: IdxCriteriaOperator;
  AOperandString: string;
  ALikeIndex: Integer;
begin
  case AOperatorType of
    TdxFunctionOperatorType.StartsWith:
      begin
        ASecondOperand := Safe<TdxCriteriaOperator>.Cast(AOperands[1].AsObject);
        if (ASecondOperand is TdxOperandValue) and ((TdxOperandValue(ASecondOperand)).Value.IsString) then
        begin
          AOperandString := TdxOperandValue(ASecondOperand).Value.AsString;
          ALikeIndex := {$IFDEF DELPHIXE3}AOperandString.IndexOfAny(StopChars){$ELSE}TdxStringHelper.IndexOfAny(AOperandString, StopChars){$ENDIF};
          if ALikeIndex < 0 then
          begin
            AConstantValue := TdxConstantValue.Create(AOperandString + '%');
            Exit(Format('(%0:s like %1:s)',
              [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
               AProcessParameter(AConstantValue)]))
          end
          else
            if ALikeIndex > 0 then
            begin
              AConstantValue := TdxConstantValue.Create({$IFDEF DELPHIXE3}AOperandString.Substring(0, ALikeIndex){$ELSE}TdxStringHelper.Substring(AOperandString, 0, ALikeIndex){$ENDIF} + '%');
              Exit(Format('((%0:s like %2:s) And (Left(%0:s, Len(%1:s)) = (%1:s)))',
                [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
                AProcessParameter(ASecondOperand),
                AProcessParameter(AConstantValue)]));
            end;
        end;
        Result := Format('(Left(%0:s, Len(%1:s)) = (%1:s))',
          [AProcessParameter(Safe<TdxCriteriaOperator>.Cast(AOperands[0].AsObject)),
          AProcessParameter(ASecondOperand)]);
      end;
    else
      Result := '';
  end;
end;

class function TdxMsSQLFormatterHelper.FormatBinary(AOperatorType: TdxBinaryOperatorType;
  const ALeftOperand, ARightOperand: string): string;
begin
  case AOperatorType of
    TdxBinaryOperatorType.Modulo:
      Result := ALeftOperand + ' % ' + ARightOperand;
    else
      Result := TdxBaseFormatterHelper.DefaultFormatBinary(AOperatorType, ALeftOperand, ARightOperand);
  end;
end;

class function TdxMsSQLFormatterHelper.FormatConstraint(const AConstraintName: string): string;
begin
  Result := '"' + AConstraintName + '"';
end;

{ TdxMSSQLConnectionProvider }

constructor TdxMSSQLConnectionProvider.Create(AConnectionVendor: TdxCustomConnectionVendor;
  AAutoCreateOption: TdxAutoCreateOption);
begin
  inherited Create(AConnectionVendor, AAutoCreateOption);
  FDatabaseObjectOwner := DatabaseObjectDefaultOwner;
  ReadDBVersion;
end;

class destructor TdxMSSQLConnectionProvider.Destroy;
begin
  FreeAndNil(FConnectionParameters);
  FreeAndNil(FDBTypes);
end;

class function TdxMSSQLConnectionProvider.GetDBEngine: TdxDBEngine;
begin
  Result := TdxDBEngines.MSSQL;
end;

class procedure TdxMSSQLConnectionProvider.PopulateConnectionParameters;
begin
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.ADO, TConnectionParameters.Create(['initial catalog']));
  FConnectionParameters.AddOrSetValue(TdxConnectionTypes.FireDAC, TConnectionParameters.Create(['Database']));
end;

class procedure TdxMSSQLConnectionProvider.PopulateDBTypes;
begin
  FDBTypes.Add('int', TdxDBColumnType.Int32);
  FDBTypes.Add('image', TdxDBColumnType.ByteArray);
  FDBTypes.Add('varbinary', TdxDBColumnType.ByteArray);
  FDBTypes.Add('nchar', TdxDBColumnType.Char);
  FDBTypes.Add('char', TdxDBColumnType.Char);
  FDBTypes.Add('varchar', TdxDBColumnType.String);
  FDBTypes.Add('nvarchar', TdxDBColumnType.String);
  FDBTypes.Add('xml', TdxDBColumnType.String);
  FDBTypes.Add('ntext', TdxDBColumnType.String);
  FDBTypes.Add('text', TdxDBColumnType.String);
  FDBTypes.Add('bit', TdxDBColumnType.Boolean);
  FDBTypes.Add('tinyint', TdxDBColumnType.Byte);
  FDBTypes.Add('smallint', TdxDBColumnType.Int16);
  FDBTypes.Add('bigint', TdxDBColumnType.Int64);
  FDBTypes.Add('numeric', TdxDBColumnType.Decimal);
  FDBTypes.Add('decimal', TdxDBColumnType.Decimal);
  FDBTypes.Add('money', TdxDBColumnType.Decimal);
  FDBTypes.Add('smallmoney', TdxDBColumnType.Decimal);
  FDBTypes.Add('float', TdxDBColumnType.Double);
  FDBTypes.Add('real', TdxDBColumnType.Single);
  FDBTypes.Add('uniqueidentifier', TdxDBColumnType.Guid);
  FDBTypes.Add('datetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('datetime2', TdxDBColumnType.DateTime);
  FDBTypes.Add('smalldatetime', TdxDBColumnType.DateTime);
  FDBTypes.Add('date', TdxDBColumnType.DateTime);
end;

class function TdxMSSQLConnectionProvider.GetDBTypes: TDictionary<string, TdxDBColumnType>;
begin
  if FDBTypes = nil then
  begin
    FDBTypes := TDictionary<string, TdxDBColumnType>.Create;
    PopulateDBTypes;
  end;
  Result := FDBTypes;
end;

class function TdxMSSQLConnectionProvider.GetConnectionParameters: TDictionary<string, TdxConnectionParameterValues>;
begin
  if FConnectionParameters = nil then
  begin
    FConnectionParameters := TDictionary<string, TdxConnectionParameterValues>.Create;
    PopulateConnectionParameters;
  end;
  Result := FConnectionParameters;
end;

function TdxMSSQLConnectionProvider.CollectTablesToCreate(const ATables: TArray<TdxDBTable>): TArray<TdxDBTable>;
var
  ADBTables, ADBSchemaTables: TDictionary<string, Boolean>;
  ATableName, ATableSchemaName: string;
  AIsView: Boolean;
  AList: TList<TdxDBTable>;
  ATable: TdxDBTable;
  ADataSet: TDataSet;
begin
  ADBTables := TDictionary<string, Boolean>.Create;
  ADBSchemaTables := TDictionary<string, Boolean>.Create;
  try
    ADataSet := GetDataForTables(ATables, nil,
      'select TABLE_NAME, TABLE_TYPE, TABLE_SCHEMA from INFORMATION_SCHEMA.TABLES ' +
      'where (%s) and TABLE_TYPE in (''BASE TABLE'', ''VIEW'')');
    if ADataSet <> nil then
      try
        while not ADataSet.Eof do
        begin
          if not ADataSet.Fields[0].IsNull then
          begin
            ATableName := ADataSet.Fields[0].AsString;
            AIsView := SameText(ADataSet.Fields[1].AsString, 'VIEW');
            ATableSchemaName := ADataSet.Fields[2].AsString;
            if SameText(ATableSchemaName, DatabaseObjectOwner) then
              ADBTables.AddOrSetValue(ATableName, AIsView);
            ADBSchemaTables.Add(Concat(ATableSchemaName, '.', ATableName), AIsView);
          end;
          ADataSet.Next;
        end;
      finally
        ADataSet.Free;
      end;
    AList := TList<TdxDBTable>.Create;
    try
      for ATable in ATables do
      begin
        ATableName := ComposeSafeTableName(ATable.Name);
        ATableSchemaName := ComposeSafeSchemaName(ATable.Name);
        AIsView := False;
        if not ADBSchemaTables.TryGetValue(Concat(ATableSchemaName, '.', ATableName), AIsView) and
          not ADBTables.TryGetValue(ATableName, AIsView) then
          AList.Add(ATable)
        else
          ATable.IsView := AIsView;
      end;
      Result := AList.ToArray;
    finally
      AList.Free;
    end;
  finally
    ADBTables.Free;
    ADBSchemaTables.Free;
  end;
end;

function TdxMSSQLConnectionProvider.GetMaximumStringSize: Integer;
begin
  Result := 4000;
end;

function TdxMSSQLConnectionProvider.FormatColumn(const AColumnName: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatColumn(AColumnName);
end;

procedure TdxMSSQLConnectionProvider.CreateDatabase(const ADatabaseName: string; const AValues: array of string);
var
  AConnection: TCustomConnection;
  AConnectionVendor: TdxCustomConnectionVendor;
begin
  AConnection := ConnectionVendor.CreateConnection(Self, 'master');
  try
    AConnectionVendor := TdxCustomConnectionVendorClass(ConnectionVendor.ClassType).Create(AConnection);
    try
      AConnection.Open;
      AConnectionVendor.Execute(Format('create database [%s]', [ADatabaseName]));
    finally
      AConnectionVendor.Free;
    end;
  finally
    AConnection.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.CreateTable(ATable: TdxDBTable);
var
  ASchemaName, ASequenceName: string;
  AQuery: TdxQuery;
begin
  inherited CreateTable(ATable);
  if not HasSequence(ATable) then
    Exit;
  ASequenceName := ATable.SequenceName;
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  if ASchemaName = '' then
    AQuery := TdxQuery.Create('select count(*) from sys.sequences s where s.name = ''' + ASequenceName + '''')
  else
    AQuery := TdxQuery.Create('select count(*) from sys.sequences q ' +
      'join sys.schemas s on s.schema_id = q.schema_id'#13#10 +
      'where q.name=''' + ASequenceName + ''' and s.name = ''' + ASchemaName + '''');
  try
    if GetScalar(AQuery) = 0 then
      ExecuteSQLSchemaUpdate('SEQUENCE', 'Key', '', 'CREATE SEQUENCE ' + ASequenceName + ' START WITH 1');
  finally
    AQuery.Free;
  end;
end;

function TdxMSSQLConnectionProvider.FormatBinary(AOperatorType: TdxBinaryOperatorType; const ALeftOperand,
  ARightOperand: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatBinary(AOperatorType, ALeftOperand, ARightOperand);
end;

function TdxMSSQLConnectionProvider.FormatColumn(const AColumnName, ATableAlias: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatColumn(AColumnName, ATableAlias);
end;

function TdxMSSQLConnectionProvider.FormatConstraint(const AConstraintName: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatConstraint(AConstraintName);
end;

function TdxMSSQLConnectionProvider.FormatOwnedDBObject(const ASchemaName, AObjectName: string): string;
begin
  if ASchemaName <> '' then
    Exit('"' + ASchemaName + '"."' + AObjectName + '"');
  if DatabaseObjectOwner <> '' then
    Result := '"' + AObjectName + '"'
  else
    Result := '"' + DatabaseObjectOwner + '"."' + AObjectName + '"';
end;

function TdxMSSQLConnectionProvider.FormatSelect(const ASelectClause, AFromClause, AWhereClause, AOrderByClause,
  AGroupByClause, AHavingClause: string; ASkipSelectedRecords, ATopSelectedRecords: Integer): string;
var
  AModificatorsSQL, AExpandedWhereClause, AExpandedOrderByClause, AExpandedHavingClause, AExpandedGroupByClause, AFetchRowsSQL,
    ASelectedSQL, ABaseFormat, AExpandedSelectedProperties: string;
  AFields: TArray<string>;
begin
  if ASkipSelectedRecords <> 0 then
    inherited FormatSelect(ASelectClause, AFromClause, AWhereClause, AOrderByClause, AGroupByClause, AHavingClause,
      ASkipSelectedRecords, ATopSelectedRecords);
  AModificatorsSQL := '';
  if (ATopSelectedRecords <> 0) and (ASkipSelectedRecords = 0) then
    AModificatorsSQL := Format('top %d ', [ATopSelectedRecords]);
  if AWhereClause = '' then
    AExpandedWhereClause := ''
  else
    AExpandedWhereClause := #10'where ' + AWhereClause;
  if AOrderByClause <> '' then
    AExpandedOrderByClause := #10'order by ' + AOrderByClause
  else
    AExpandedOrderByClause := '';
  if AHavingClause <> '' then
    AExpandedHavingClause := #10'having ' + AHavingClause
  else
    AExpandedHavingClause := '';
  if AGroupByClause <> '' then
    AExpandedGroupByClause := #10'group by ' + AGroupByClause
  else
    AExpandedGroupByClause := '';
  if ASkipSelectedRecords = 0 then
    Result := Format('select %s%s from %s%s%s%s%s', [AModificatorsSQL, ASelectClause, AFromClause,
      AExpandedWhereClause, AExpandedGroupByClause, AExpandedHavingClause, AExpandedOrderByClause])
  else
    if Is2012 and (AExpandedOrderByClause <> '') then
    begin
      if ATopSelectedRecords <> 0 then
        AFetchRowsSQL := Format(#10'fetch next %d rows only', [ATopSelectedRecords])
      else
        AFetchRowsSQL := '';
      Result := Format('select %s from %s%s%s%s%s'#10'offset %d rows%s', [ASelectClause, AFromClause, AExpandedWhereClause,
        AExpandedGroupByClause, AExpandedHavingClause, AExpandedOrderByClause, ASkipSelectedRecords, AFetchRowsSQL]);
    end
    else
    begin
      AFields := TdxSimpleSQLParser.GetColumns(ASelectClause);
      AExpandedSelectedProperties := TdxSimpleSQLParser.GetExpandedProperties(AFields, 'resultSet');
      ASelectedSQL := TdxFormatterHelper.Concat(AFields, ', ');
      ABaseFormat := 'select %8:s from(select %0:s, row_number() over(%1:s) as '#$27'rowNumber'#$27 +
        ' from %4:s%5:s%6:s%7:s)resultSet where resultSet.rowNumber > %2:d';
      if ATopSelectedRecords <> 0 then
        ABaseFormat := ABaseFormat + ' and resultSet.rowNumber <= %2:d + %3:d';
      Result := Format(ABaseFormat, [ASelectedSQL, AExpandedOrderByClause, ASkipSelectedRecords, ATopSelectedRecords,
        AFromClause, AExpandedWhereClause, AExpandedGroupByClause, AExpandedHavingClause, AExpandedSelectedProperties]);
    end;
end;

function TdxMSSQLConnectionProvider.FormatTable(const ASchemaName, ATableName: string): string;
begin
  Result := FormatOwnedDBObject(ASchemaName, ATableName);
end;

function TdxMSSQLConnectionProvider.FormatTable(const ASchemaName, ATableName, ATableAlias: string): string;
begin
  Result := FormatOwnedDBObject(ASchemaName, ATableName) + ' ' + ATableAlias;
end;

function TdxMSSQLConnectionProvider.FormatInsertDefaultValues(ATable: TdxDBTable): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatInsertDefaultValues(FormatTableSafe(ATable));
end;

function TdxMSSQLConnectionProvider.FormatInsert(const ATableName, AFieldNames, AValuesClause: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatInsert(ATableName, AFieldNames, AValuesClause);
end;

function TdxMSSQLConnectionProvider.FormatUpdate(const ATableName, ASetClause, AWhereClause: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatUpdate(ATableName, ASetClause, AWhereClause);
end;

function TdxMSSQLConnectionProvider.FormatDelete(const ATableName, AWhereClause: string): string;
begin
  Result := TdxMsSQLFormatterHelper.FormatDelete(ATableName, AWhereClause);
end;

function TdxMSSQLConnectionProvider.FormatFunction(AOperatorType: TdxFunctionOperatorType;
  const AOperands: TArray<string>): string;
var
  AFormat: string;
begin
  AFormat := TdxMsSQLFormatterHelper.FormatFunction(AOperatorType,
    FVersion, AOperands);
  if AFormat = '' then
    Result := inherited FormatFunction(AOperatorType, AOperands)
  else
    Result := AFormat;
end;

function TdxMSSQLConnectionProvider.FormatFunction(AProcessParameter: TdxProcessParameter;
  AOperatorType: TdxFunctionOperatorType; const AOperands: TArray<TValue>): string;
var
  AFormat: string;
begin
  AFormat := TdxMsSQLFormatterHelper.FormatFunction(AProcessParameter, AOperatorType,
    FVersion, AOperands);
  if AFormat = '' then
    Result := inherited FormatFunction(AProcessParameter, AOperatorType, AOperands)
  else
    Result := AFormat;
end;

function TdxMSSQLConnectionProvider.GetConnectionParameter(const AConnectionType: TdxConnectionType;
  AParameter: TdxConnectionParameter): string;
begin
  Result := ConnectionParameters[AConnectionType][AParameter];
end;

function TdxMSSQLConnectionProvider.GetDataForTables(const ATables: TArray<TdxDBTable>;
  AFilter: TdxMSSQLConnectionProvider.TTableFilter; const AQueryText: string): TDataSet;
var
  AParameters: TdxQueryParameterCollection;
  AInList: TList<string>;
  ANames: TStringBuilder;
  ASchemas: TDictionary<string, string>;
  ATable: TdxDBTable;
  AName, ASchemaName: string;
  AQuery: TdxQuery;
begin
  AParameters := TdxQueryParameterCollection.Create;
  AInList := TList<string>.Create;
  ANames := TStringBuilder.Create;
  ASchemas := TDictionary<string, string>.Create;
  try
    for ATable in ATables do
    begin
      if Assigned(AFilter) and not AFilter(ATable) then
        Continue;
      AParameters.Add(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
      AName := GetParamAlias(AInList.Count);
      AInList.Add(AName);
      if ANames.Length > 0 then
        ANames.Append(' OR ');
      ANames.Append('TABLE_NAME = :').Append(AName);
      ASchemaName := ComposeSafeSchemaName(ATable.Name);
      if ASchemaName <> '' then
      begin
        if not ASchemas.TryGetValue(ASchemaName, AName) then
        begin
          AParameters.Add(TdxOperandValue.Create(ASchemaName));
          AName := GetParamAlias(AInList.Count);
          AInList.Add(AName);
          ASchemas.Add(ASchemaName, AName);
        end;
        ANames.Append(' AND TABLE_SCHEMA = :').Append(AName);
      end;
    end;
    if AInList.Count = 0 then
      Exit(nil);
    AQuery := TdxQuery.Create(Format(AQueryText, [ANames.ToString]), AParameters, AInList.ToArray);
    try
      Result := SelectData(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    AParameters.Free;
    AInList.Free;
    ANames.Free;
    ASchemas.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.GetColumns(ATable: TdxDBTable);
var
  ASchemaName: string;
  AQuery: TdxQuery;
  ADataSet: TDataSet;
  AField: TField;
  ASize: Integer;
  AType: TdxDBColumnType;
  AColumn: TdxDBColumn;
  AQueryParameters: TdxQueryParameterCollection;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  if ASchemaName = '' then
  begin
    AQueryParameters := TdxQueryParameterCollection.Create(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
    AQuery := TdxQuery.Create('select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, COLUMN_DEFAULT, IS_NULLABLE from INFORMATION_SCHEMA.COLUMNS ' +
      'where TABLE_NAME = :p1', AQueryParameters, TArray<string>.Create('p1'))
  end
  else
  begin
    AQueryParameters := TdxQueryParameterCollection.Create([TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)),
      TdxOperandValue.Create(ASchemaName)]);
    AQuery := TdxQuery.Create('select COLUMN_NAME, DATA_TYPE, CHARACTER_MAXIMUM_LENGTH, COLUMN_DEFAULT, IS_NULLABLE from INFORMATION_SCHEMA.COLUMNS ' +
      'where TABLE_NAME = :p1 and TABLE_SCHEMA = :p2', AQueryParameters, TArray<string>.Create('p1', 'p2'));
  end;
  try
    ADataSet := SelectData(AQuery);
    try
      while not ADataSet.Eof do
      begin
        AField := ADataSet.Fields[2];
        if not AField.IsNull then
          ASize := AField.AsInteger
        else
          ASize := 0;
        AType := GetTypeFromString(ADataSet.Fields[1].AsString, ASize);
        if AType <> TdxDBColumnType.String then
          ASize := 0;
        AColumn := TdxDBColumn.Create(ADataSet.Fields[0].AsString, False, '', ASize, AType);
        AColumn.IsNullable := ADataSet.Fields[4].Value = 'YES';
        if not ADataSet.Fields[3].IsNull then
          AColumn.DefaultValue := ADataSet.Fields[3].Value;
        ATable.AddColumn(AColumn);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQueryParameters.Free;
    AQuery.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.GetPrimaryKey(ATable: TdxDBTable);
var
  ASchemaName: string;
  AQuery: TdxQuery;
  AQueryParameters: TdxQueryParameterCollection;
  ADataSet: TDataSet;
  AColumns: TList<string>;
  I, AColumnProperty: Integer;
  AColumn: TdxDBColumn;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  if ASchemaName = '' then
  begin
    AQueryParameters := TdxQueryParameterCollection.Create(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
    AQuery := TdxQuery.Create(IfThen(is2005,
      'select c.COLUMN_NAME, COLUMNPROPERTY(OBJECT_ID(c.TABLE_SCHEMA + ''.'' + c.TABLE_NAME), c.COLUMN_NAME, ''IsIdentity'')'^M +
      'from INFORMATION_SCHEMA.KEY_COLUMN_USAGE c'^M +
      'join INFORMATION_SCHEMA.TABLE_CONSTRAINTS p on p.CONSTRAINT_NAME = c.CONSTRAINT_NAME'^M +
      'where c.TABLE_NAME = :p1 and p.CONSTRAINT_TYPE = ''PRIMARY KEY'''
      ,
      'select c.name, COLUMNPROPERTY(t.object_id, c.name, ''IsIdentity'') from sys.key_constraints p'^M +
      'join sys.index_columns i on p.parent_object_id = i.object_id and p.unique_index_id = i.index_id'^M +
      'join sys.columns c on i.column_id = c.column_id and p.parent_object_id = c.object_id'^M +
      'join sys.tables t on p.parent_object_id = t.object_id'^M +
      'where t.name = :p1 and p.type = ''PK'''^M +
      'order by i.key_ordinal'),
      AQueryParameters, TArray<string>.Create('p1'))
  end
  else
  begin
    AQueryParameters := TdxQueryParameterCollection.Create([TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)),
      TdxOperandValue.Create(ASchemaName)]);
    AQuery := TdxQuery.Create(IfThen(is2005,
      'SELECT clmns.name, COLUMNPROPERTY(tbl.id, clmns.name, ''IsIdentity'')'^M +
      'FROM dbo.sysobjects AS tbl'^M +
      'INNER JOIN sysusers AS stbl ON stbl.uid = tbl.uid'^M +
      'INNER JOIN dbo.syscolumns AS clmns ON clmns.id=tbl.id'^M +
      'LEFT OUTER JOIN dbo.sysindexes AS ik ON ik.id = clmns.id and 0 != ik.status & 0x0800'^M +
      'LEFT OUTER JOIN dbo.sysindexkeys AS cik ON cik.indid = ik.indid and cik.colid = clmns.colid and cik.id = clmns.id'^M +
      'WHERE (tbl.type=''U'')and(tbl.name=:p1 and stbl.name=:p2) and cik.colid is not null'
      ,
      'select c.name, COLUMNPROPERTY(t.object_id, c.name, ''IsIdentity'') from sys.key_constraints p'^M +
      'join sys.index_columns i on p.parent_object_id = i.object_id and p.unique_index_id = i.index_id'^M +
      'join sys.columns c on i.column_id = c.column_id and p.parent_object_id = c.object_id'^M +
      'join sys.tables t on p.parent_object_id = t.object_id'^M +
      'join sys.schemas s on s.schema_id = p.schema_id'^M +
      'where t.name = :p1 and p.type = ''PK'' and s.name = :p2'^M +
      'order by i.key_ordinal'),
      AQueryParameters, TArray<string>.Create('p1', 'p2'));
  end;
  try
    ADataSet := SelectData(AQuery);
    try
      if ADataSet.IsEmpty then
        Exit;
      AColumns := TList<string>.Create;
      try
        I := 0;
        AColumnProperty := 0;
        while not ADataSet.Eof do
        begin
          AColumns.Add(ADataSet.Fields[0].AsString);
          if I = 0 then
            AColumnProperty := ADataSet.Fields[1].AsInteger;
          Inc(I);
          ADataSet.Next;
        end;
        ATable.PrimaryKey := TdxDBPrimaryKey.Create(AColumns);
        for I := 0 to AColumns.Count - 1 do
        begin
          AColumn := ATable.GetColumn(AColumns[I]);
          if AColumn <> nil then
            AColumn.IsKey := True;
        end;
        if (AColumns.Count = 1) and (AColumnProperty = 1) then
          ATable.GetColumn(AColumns[0]).IsIdentity := True;
      finally
        AColumns.Free;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQueryParameters.Free;
    AQuery.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.GetIndexes(ATable: TdxDBTable);
var
  ASchemaName, AColumnName: string;
  AQuery: TdxQuery;
  AQueryParameters: TdxQueryParameterCollection;
  ADataSet: TDataSet;
  AField: TField;
  AIndex: TdxDBIndex;
  AIsUnique: Boolean;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  if ASchemaName = '' then
  begin
    if not Is2005 then
    begin
      AQueryParameters := TdxQueryParameterCollection.Create(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
      AQuery := TdxQuery.Create('select i.name, c.name, INDEXPROPERTY(i.id, i.name, ''IsUnique'') from sysobjects o'#13#10 +
        'join sysindexes i on i.id=o.id'#13#10 +
        'join sysindexkeys k on k.id=i.id and k.indid=i.indid'#13#10 +
        'join syscolumns c on c.id=k.id and c.colid=k.colid'#13#10 +
        'where o.name = :p1 and o.type=''U'' and i.name is not null and i.status&96=0'#13#10 +
        'order by i.name, k.keyno',
        AQueryParameters, TArray<string>.Create('p1'));
    end
    else
    begin
      AQueryParameters := TdxQueryParameterCollection.Create(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
      AQuery := TdxQuery.Create('select i.name, c.name, i.is_unique from sys.objects t'#13#10 +
        'join sys.indexes i on t.object_id = i.object_id'#13#10 +
        'join sys.index_columns ic on ic.index_id = i.index_id and ic.object_id = t.object_id'#13#10 +
        'join sys.columns c on c.column_id = ic.column_id and c.object_id = t.object_id'#13#10 +
        'where t.name=:p1 and i.name is not null and ic.key_ordinal > 0'#13#10 +
        'order by i.name, ic.key_ordinal',
        AQueryParameters, TArray<string>.Create('p1'));
    end;
  end
  else
  begin
    if not Is2005 then
    begin
      AQueryParameters := TdxQueryParameterCollection.Create([TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)),
        TdxOperandValue.Create(ASchemaName)]);
      AQuery := TdxQuery.Create('select i.name, c.name, INDEXPROPERTY(i.id, i.name, ''IsUnique'') from sysobjects o'#13#10 +
        'join sysindexes i on i.id=o.id'#13#10 +
        'join sysindexkeys k on k.id=i.id and k.indid=i.indid'#13#10 +
        'join syscolumns c on c.id=k.id and c.colid=k.colid'#13#10 +
        'join sysusers u on u.uid = o.uid'#13#10 +
        'where o.name = :p1 and u.name = :p2 and o.type=''U'' and i.name is not null and i.status&96=0'#13#10 +
        'order by i.name, k.keyno',
        AQueryParameters, TArray<string>.Create('p1', 'p2'));
    end
    else
    begin
      AQueryParameters := TdxQueryParameterCollection.Create([TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)),
        TdxOperandValue.Create(ASchemaName)]);
      AQuery := TdxQuery.Create('select i.name, c.name, i.is_unique from sys.objects t'#13#10 +
        'join sys.indexes i on t.object_id = i.object_id'#13#10 +
        'join sys.index_columns ic on ic.index_id = i.index_id and ic.object_id = t.object_id'#13#10 +
        'join sys.columns c on c.column_id = ic.column_id and c.object_id = t.object_id'#13#10 +
        'join sys.schemas s on s.schema_id = t.schema_id'#13#10 +
        'where t.name=:p1 and s.name = :p2 and i.name is not null and ic.key_ordinal > 0'#13#10 +
        'order by i.name, ic.key_ordinal',
        AQueryParameters, TArray<string>.Create('p1', 'p2'));
    end;
  end;
  try
    ADataSet := SelectData(AQuery);
    try
      AIndex := nil;
      while not ADataSet.Eof do
      begin
        if (AIndex = nil) or (AIndex.Name <> ADataSet.Fields[0].AsString) then
        begin
          AColumnName := ADataSet.Fields[1].AsString;
          AField := ADataSet.Fields[2];
          if AField.DataType = ftBoolean then
            AIsUnique := AField.AsBoolean
          else
            AIsUnique := AField.AsInteger = 1;
          AIndex := TdxDBIndex.Create(ADataSet.Fields[0].AsString, [AColumnName], AIsUnique);
          ATable.Indexes.Add(AIndex);
        end
        else
          AIndex.Columns.Add(ADataSet.Fields[1].AsString);
        ADataSet.Next;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQueryParameters.Free;
    AQuery.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.GetForeignKeys(ATable: TdxDBTable);
var
  ASchemaName, ARTable, ARSchema: string;
  AQuery: TdxQuery;
  AQueryParameters: TdxQueryParameterCollection;
  ADataSet: TDataSet;
  AForeignKeys: TDictionary<string, TdxDBForeignKey>;
  AForeignKey: TdxDBForeignKey;
  APrimaryKeyColumn, AForeignKeyColumn: string;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  if ASchemaName = '' then
  begin
    AQueryParameters := TdxQueryParameterCollection.Create(TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)));
    AQuery := TdxQuery.Create(IfThen(Is2005,
      'select fk.name, c.name, rc.name, '''', rtbl.name from sysforeignkeys fkdata'^M +
      'join sysobjects fk on fkdata.constid=fk.id'^M +
      'join sysobjects rtbl on rtbl.id=fkdata.rkeyid'^M +
      'join sysobjects tbl on tbl.id=fkdata.fkeyid'^M +
      'join syscolumns c on c.id=fkdata.fkeyid and c.colid=fkdata.fkey'^M +
      'join syscolumns rc on rc.id=fkdata.rkeyid and rc.colid=fkdata.rkey'^M +
      'where tbl.name = :p1'^M +
      'order by fk.name, fkdata.keyno'
      ,
      'select c.name, fk.name, pk.name, '''', pkt.name from sys.foreign_key_columns r'^M +
      'inner join sys.foreign_keys c on r.constraint_object_id = c.object_id'^M +
      'inner join sys.columns fk on r.parent_object_id = fk.object_id and r.parent_column_id = fk.column_id'^M +
      'inner join sys.tables fkt on r.parent_object_id = fkt.object_id'^M +
      'inner join sys.columns pk on r.referenced_object_id = pk.object_id and r.referenced_column_id = pk.column_id'^M +
      'inner join sys.tables pkt on r.referenced_object_id = pkt.object_id'^M +
      'where fkt.name = :p1'^M +
      'order by c.name, r.constraint_column_id'),
      AQueryParameters, TArray<string>.Create('p1'));
  end
  else
  begin
    AQueryParameters := TdxQueryParameterCollection.Create([TdxOperandValue.Create(ComposeSafeTableName(ATable.Name)),
      TdxOperandValue.Create(ASchemaName)]);
    AQuery := TdxQuery.Create(IfThen(Is2005,
      'select fk.name, c.name, rc.name, ru.name, rtbl.name from sysforeignkeys fkdata'^M +
      'join sysobjects fk on fkdata.constid=fk.id'^M +
      'join sysobjects rtbl on rtbl.id=fkdata.rkeyid'^M +
      'join sysobjects tbl on tbl.id=fkdata.fkeyid'^M +
      'join syscolumns c on c.id=fkdata.fkeyid and c.colid=fkdata.fkey'^M +
      'join syscolumns rc on rc.id=fkdata.rkeyid and rc.colid=fkdata.rkey'^M +
      'join sysusers u on u.uid = tbl.uid'^M +
      'join sysusers ru on ru.uid = rtbl.uid'^M +
      'where tbl.name = :p1 and u.name = :p2'^M +
      'order by fk.name, fkdata.keyno'
      ,
      'select c.name, fk.name, pk.name, pks.name, + pkt.name from sys.foreign_key_columns r'^M +
      'inner join sys.foreign_keys c on r.constraint_object_id = c.object_id'^M +
      'inner join sys.columns fk on r.parent_object_id = fk.object_id and r.parent_column_id = fk.column_id'^M +
      'inner join sys.objects fkt on r.parent_object_id = fkt.object_id'^M +
      'inner join sys.schemas fks on fks.schema_id = fkt.schema_id'^M +
      'inner join sys.columns pk on r.referenced_object_id = pk.object_id and r.referenced_column_id = pk.column_id'^M +
      'inner join sys.objects pkt on r.referenced_object_id = pkt.object_id'^M +
      'inner join sys.schemas pks on pks.schema_id = pkt.schema_id'^M +
      'where fkt.name = :p1 and fks.name = :p2'^M +
      'order by c.name, r.constraint_column_id'),
      AQueryParameters, TArray<string>.Create('p1', 'p2'));
  end;
  try
    ADataSet := SelectData(AQuery);
    try
      AForeignKeys := TDictionary<string, TdxDBForeignKey>.Create;
      try
        while not ADataSet.Eof do
        begin
          if not AForeignKeys.TryGetValue(ADataSet.Fields[0].AsString, AForeignKey) then
          begin
            APrimaryKeyColumn := ADataSet.Fields[1].AsString;
            AForeignKeyColumn := ADataSet.Fields[2].AsString;
            ARTable := ADataSet.Fields[4].AsString;
            ARSchema := ADataSet.Fields[3].AsString;
            if (DatabaseObjectOwner <> ARSchema) and (ARSchema <> '') then
              ARTable := ARSchema + '.' + ARTable;
            AForeignKey := TdxDBForeignKey.Create([APrimaryKeyColumn], ARTable, [AForeignKeyColumn]);
            ATable.ForeignKeys.Add(AForeignKey);
            AForeignKeys.AddOrSetValue(ADataSet.Fields[0].AsString, AForeignKey);
          end
          else
          begin
            AForeignKey.Columns.Add(ADataSet.Fields[1].AsString);
            AForeignKey.PrimaryKeyTableKeyColumns.Add(ADataSet.Fields[2].AsString);
          end;
          ADataSet.Next;
        end;
      finally
        AForeignKeys.Free;
      end;
    finally
      ADataSet.Free;
    end;
  finally
    AQueryParameters.Free;
    AQuery.Free;
  end;
end;

function TdxMSSQLConnectionProvider.GetIdentity(ASQL: TdxQuery): Int64;
var
  AQuery: TdxQuery;
begin
  AQuery := TdxQuery.Create(ASQL.SQLText + #10' select ' + IfThen(is2000, 'SCOPE_IDENTITY()', '@@Identity'),
    ASQL.Parameters, ASQL.ParameterNames);
  try
    Result := GetScalar(AQuery);
  finally
    AQuery.Free;
  end;
end;

function TdxMSSQLConnectionProvider.GetIdentity(ARoot: TdxInsertStatement;
  AIdentities: TdxTaggedParameterHolder): Int64;
var
  AGenerator: TdxSequenceInsertSqlGenerator;
  AQuery: TdxQuery;
  AValue: Variant;
  Id64: Int64;
  Id32: Integer;
begin
  if not HasSequence(ARoot.Table) then
    Exit(inherited GetIdentity(ARoot, AIdentities));
  AQuery := TdxQuery.Create('SELECT NEXT VALUE FOR ' + GetSeqName(ARoot.Table));
  try
    AValue := GetScalar(AQuery);
    Result := AValue;
  finally
    AQuery.Free;
  end;
  AGenerator := TdxSequenceInsertSqlGenerator.Create(Self, AIdentities);
  try
    AQuery := AGenerator.GenerateSQL(ARoot);
    try
      case ARoot.IdentityColumnType of
        TdxDBColumnType.Int32:
          begin
            Id32 := Integer(AValue);
            AQuery.Parameters.Add(TdxOperandValue.Create(Id32));
          end;
        TdxDBColumnType.Int64:
          begin
            Id64 := AValue;
            AQuery.Parameters.Add(TdxOperandValue.Create(Id64));
          end
        else
          raise ENotSupportedException.Create('');
      end;
      AQuery.AppendParameterName('seq');
      Execute(AQuery);
    finally
      AQuery.Free;
    end;
  finally
    AGenerator.Free;
  end;
end;

function TdxMSSQLConnectionProvider.GetIsAzure: Boolean;
begin
  if not Is2000 then
    Exit(False);
  if not FIsAzureHasValue then
  begin
    if ConnectionVendor.ExecuteScalar('select SERVERPROPERTY('#$27'edition'#$27')') = 'SQL Azure' then
      Include(FVersion, TVersion.isAzure);
    FIsAzureHasValue := True;
  end;
  Result := TVersion.isAzure in FVersion;
end;

function TdxMSSQLConnectionProvider.SupportsNativeOuterApply: Boolean;
begin
  Result := Is2005;
end;

function TdxMSSQLConnectionProvider.SupportsNativeSkipTake: Boolean;
begin
  Result := Is2005;
end;

function TdxMSSQLConnectionProvider.GetParameterName(const AParameter: IdxOperandValue; AIndex: Integer;
  var ACreateParameter: Boolean): string;
var
  AValue: TValue;
begin
  AValue := AParameter.Value;
  ACreateParameter := False;
  if (AParameter is TdxConstantValue) and (not AValue.IsEmpty) then
  begin
    if AValue.IsType<Boolean> then
    begin
      if AValue.AsBoolean then
        Exit('1')
      else
        Exit('0');
    end
    else
      case AValue.Kind of
        tkInteger:
          Exit(IntToStr(AValue.AsInteger));
        tkEnumeration:
          Exit(IntToStr(AValue.AsOrdinal));
        tkString, tkLString, tkWString, tkUString:
          Exit(Concat('N'#$27, ReplaceText(AValue.AsString, #$27, #$27#$27), #$27));
      end;
  end;
  ACreateParameter := True;
  Result := GetParamAlias(AIndex);
end;

function TdxMSSQLConnectionProvider.GetSafeNameTableMaxLength: Integer;
begin
  Result := NameTableMaxLength;
end;

function TdxMSSQLConnectionProvider.GetSeqName(ATable: TdxDBTable): string;
var
  ASchemaName, ASequenceName: string;
begin
  ASchemaName := ComposeSafeSchemaName(ATable.Name);
  ASequenceName := ATable.SequenceName;
  if ASchemaName = '' then
    Result := ASequenceName
  else
    Result := ASchemaName + '.' + ASequenceName;
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForBoolean(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bit';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'tinyint';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForSByte(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(3,0)';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForChar(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'nchar(1)';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForDecimal(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'money';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForDouble(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'double precision';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForSingle(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'float';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'int';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForUInt32(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(10,0)';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'smallint';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForUInt16(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(5,0)';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'bigint';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForUInt64(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'numeric(20,0)';
end;

function TdxMSSQLConnectionProvider.GetStorageTableNames(AIncludeViews: Boolean): TArray<string>;
var
  AQuery: TdxQuery;
  ATables: TDataSet;
  AOwner: string;
  AResult: TList<string>;
begin
  AResult := TList<string>.Create;
  try
    if Is2005 then
      AQuery := TdxQuery.Create(Format('select t.name, p.name from sys.objects t join sys.schemas p on p.schema_id = ' +
        't.schema_id where (t.type =''u'' %s) and objectProperty(t.object_id, ''IsMSShipped'') = 0',
        [IfThen(AIncludeViews, ' or t.type =''v''', '')]))
    else
      AQuery := TdxQuery.Create(Format('select name from sysobjects where (type=''U'' %s) and objectProperty(id, ''IsMSShipped'') = 0',
        [IfThen(AIncludeViews, ' or t.type =''v''', '')]));
    try
      ATables := SelectData(AQuery);
      if ATables = nil then
        Exit(nil);
      AOwner := '';
      while not ATables.Eof do
      begin
        if Is2005 then
          AOwner := ATables.Fields[1].AsString;
        if (AOwner <> '') and (DatabaseObjectOwner <> AOwner) then
          AResult.Add(Concat(AOwner, '.', ATables.Fields[0].AsString))
        else
          AResult.Add(ATables.Fields[0].AsString);
        ATables.Next;
      end;
    finally
      AQuery.Free;
    end;
    Result := AResult.ToArray;
  finally
    AResult.Free;
  end;
end;

procedure TdxMSSQLConnectionProvider.GetTableSchema(ATable: TdxDBTable; ACheckIndexes, ACheckForeignKeys: Boolean);
begin
  GetColumns(ATable);
  GetPrimaryKey(ATable);
  if ACheckIndexes then
    GetIndexes(ATable);
  if ACheckForeignKeys then
    GetForeignKeys(ATable);
end;

function TdxMSSQLConnectionProvider.GetTypeFromString(const ATypeName: string; ALength: Integer): TdxDBColumnType;
begin
  if DBTypes.TryGetValue(ATypeName, Result) then
  begin
    if (Result = TdxDBColumnType.Char) and (ALength > 1) then
      Result := TdxDBColumnType.String;
  end
  else
    Result := TdxDBColumnType.Unknown;
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForString(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumStringSize) then
      Result := Format('nvarchar(%d)', [AColumn.Size])
  else
    if AColumn.Size = 0 then
      Result := Format('nvarchar(%d)', [DefaultStringSize])
    else
      if Is2005 then
        Result := 'nvarchar(max)'
      else
        Result := 'ntext';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForDateTime(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'datetime';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForGuid(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  Result := 'uniqueidentifier';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnTypeForByteArray(ATable: TdxDBTable; AColumn: TdxDBColumn): string;
begin
  if (AColumn.Size > 0) and (AColumn.Size <= MaximumBinarySize) then
    Result := 'varbinary(' + IntToStr(AColumn.Size) + ')'
  else
    if Is2005 then
      Result := 'varbinary(max)'
    else
      Result := 'image';
end;

function TdxMSSQLConnectionProvider.GetSQLCreateColumnFullAttributes(ATable: TdxDBTable;
  AColumn: TdxDBColumn): string;
begin
  Result := GetSQLCreateColumnType(ATable, AColumn);
  if AColumn.IsKey or not AColumn.IsNullable then
    Result := Result + ' NOT NULL'
  else
    Result := Result + ' NULL';
  if AColumn.IsKey and IsSingleColumnPKColumn(ATable, AColumn)then
  begin
    if AColumn.IsIdentity and not HasSequence(ATable) and
    (AColumn.ColumnType in [TdxDBColumnType.Int32, TdxDBColumnType.Int64]) then
      Result := Result + IfThen(GetIsAzure, ' IDENTITY', ' IDENTITY NOT FOR REPLICATION')
    else
      if (AColumn.ColumnType = TdxDBColumnType.Guid) and not GetIsAzure then
        Result := Result + ' ROWGUIDCOL';
  end;
end;

function TdxMSSQLConnectionProvider.GetVersion(const Index: TVersion): Boolean;
begin
  Result := Index in FVersion;
end;

function TdxMSSQLConnectionProvider.HasSequence(ATable: TdxDBTable): Boolean;
begin
  if not Is2012 then
    Exit(False);
  Result := ATable.SequenceName <> '';
end;

procedure TdxMSSQLConnectionProvider.ReadDBVersion;
var
  AVersion: Integer;
  AConnection, ANewConnection: TCustomConnection;
begin
  AConnection := ConnectionVendor.Connection;
  if not AConnection.Connected then
  begin
    ANewConnection := ConnectionVendor.CreateConnection(Self, 'master');
    try
      ANewConnection.Open;
      AVersion := ConnectionVendor.ExecuteScalar(ANewConnection, 'select @@MICROSOFTVERSION / 0x1000000');
    finally
      ANewConnection.Free;
    end
  end
  else
    AVersion := ConnectionVendor.ExecuteScalar(AConnection, 'select @@MICROSOFTVERSION / 0x1000000');
  FVersion := [];
  if AVersion > 7 then
    Include(FVersion, TVersion.is2000);
  if AVersion > 8 then
    Include(FVersion, TVersion.is2005);
  if AVersion > 9 then
    Include(FVersion, TVersion.is2008);
  if AVersion > 10 then
    Include(FVersion, TVersion.is2012);
end;

initialization
  TdxSQLConnectionProviderFactory.Register(TdxMSSQLConnectionProvider);

finalization
  TdxSQLConnectionProviderFactory.Unregister(TdxMSSQLConnectionProvider);

end.
